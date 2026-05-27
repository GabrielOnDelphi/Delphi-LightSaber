UNIT LightCore.ExceptionLogger;

{=============================================================================================================
   Gabriel Moraru
   2026.05.27
--------------------------------------------------------------------------------------------------------------
   Cross-platform "log every raised Pascal exception to a flat file" hook.

   Purpose. Captures the class name + message + thread ID + UTC timestamp of every Pascal raise to a flat text
   file under the per-app private storage. The intent is post-mortem diagnostics on platforms where the usual
   IDE / logcat / tombstone signal is unreliable — notably Android, where `logcat` often shows no app-side
   output from an FMX build and the IDE shows only a numeric signal code with no class name. If the file is
   empty after a crash, that itself is a useful signal — it means the trigger was NOT a Pascal exception
   (e.g. a raw Unix signal like SIGUSR1 from the Android Runtime).

   Mechanism. Hooks `System.RaiseExceptObjProc` — fires on EVERY raise, BEFORE try/except matches. This is the
   only RTL layer that catches a swallowed-and-rethrown cycle (where the exception is caught upstream so
   `ExceptProc`'s "unhandled" tail hook never fires). The previous hook is chained so we don't break other
   tooling.

   Re-entrancy. A threadvar (`InHook`) short-circuits recursive raises that happen INSIDE our hook (e.g.
   `EFCreateError` if the log file path itself is unwritable). The body is also wrapped in a try/except that
   swallows silently — the hook MUST NOT propagate; doing so would corrupt the unwind of the original
   exception and crash the host.

   File path. `TPath.GetDocumentsPath \ <LogFileName>`.
     - Android: per-app private storage (`/data/data/<pkg>/files/`) — no permissions required.
     - Windows: the logged-in user's `Documents\` folder.
     - macOS / iOS / Linux: the platform's `GetDocumentsPath` equivalent.

   The full path is computed ONCE at `InstallExceptionLogger` time so the hook never depends on `TPath` /
   `AppData` being functional at raise-time.

   Line format (one line per raise):
       2026-05-27T19:42:18.123Z<TAB>T#7531<TAB>EJNIException<TAB>java.lang.SecurityException — ...

   Lifecycle. Call `InstallExceptionLogger` as EARLY as possible from the .dpr (before `AppData.Create`), so
   even a startup-time raise is captured. The unit's `finalization` is intentionally minimal — the hook
   stays active until the OS reaps the process.

   Typical wiring (FMX project, in the .dpr's `uses` clause and `begin..end`):
       uses
         ..., LightCore.ExceptionLogger;
       begin
         InstallExceptionLogger('MyApp-Exceptions.log');   // first line — before AppData.Create
         AppData:= TAppData.Create(...);
         ...
       end.

   Pulling the log on Android:
       adb shell run-as <package-name> cat files/MyApp-Exceptions.log

   History. Extracted from `c:\Projects\Project OrinocoReader\Frame FMX\uExceptionLogger.pas` on 2026-05-27
   after the Samsung Tab S11 Ultra "exception class 10" investigation showed the hook was generally useful
   across FMX projects — see
   `c:\Projects\FMX\Bug reporter FMX\Crash Reporting Tools for Delphi FMX Android.md` Section 3 for the
   broader DIY baseline this fits into.
=============================================================================================================}

INTERFACE

USES
  System.SysUtils;

procedure InstallExceptionLogger(CONST LogFileName: string = 'Exceptions.log');
function  ExceptionLogPath: string;     { exposed for diagnostics — read the value from outside if you want to surface it in the UI / About box }


IMPLEMENTATION

USES
  System.Classes,
  System.SyncObjs,
  System.IOUtils,
  System.DateUtils;

VAR
  FLogPath        : string  = '';
  FLogLock        : TCriticalSection = NIL;
  FPrevRaiseProc  : Pointer = NIL;
  FInstalled      : Boolean = FALSE;

THREADVAR
  InHook: Boolean;     { per-thread re-entry guard. Default FALSE in every thread when first read. }


function  ExceptionLogPath: string;
begin
  Result:= FLogPath;
end;


{ Format the line. KEEP THIS BRANCH-FREE FOR EXCEPTION SAFETY — every operation runs inside the hook. }
function  FormatLine(CONST AClassName, AMessage: string; AThreadID: TThreadID): string;
VAR
  TS: string;
begin
  TS:= FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss"."zzz"Z"', TTimeZone.Local.ToUniversalTime(Now));
  Result:= TS + #9 + 'T#' + IntToStr(AThreadID) + #9 + AClassName + #9 + AMessage + sLineBreak;
end;


{ Append the line to the log file. Atomic per-line on POSIX (single write under PAGE_SIZE — our lines are well
  under 4 KB) — we still take the critical section to serialize the Windows path and to keep behavior identical
  across platforms. }
procedure AppendLine(CONST ALine: string);
VAR
  FS   : TFileStream;
  Bytes: TBytes;
begin
  if FLogPath = '' then EXIT;     { not installed — nothing to do }
  if FLogLock = NIL then EXIT;
  FLogLock.Enter;
  try
    try
      if TFile.Exists(FLogPath)
      then FS:= TFileStream.Create(FLogPath, fmOpenWrite or fmShareDenyWrite)
      else FS:= TFileStream.Create(FLogPath, fmCreate or fmShareDenyWrite);
      try
        FS.Seek(0, soEnd);
        Bytes:= TEncoding.UTF8.GetBytes(ALine);
        if Length(Bytes) > 0
        then FS.WriteBuffer(Bytes[0], Length(Bytes));
      finally
        FS.Free;
      end;
    except
      { Disk full, permission denied, file locked — never propagate from inside an exception hook. }
    end;
  finally
    FLogLock.Leave;
  end;
end;


{ Chain to the previous RaiseExceptObjProc (if anything else hooked it) so we don't break other tooling. }
procedure CallPrevRaiseProc(P: PExceptionRecord);
type
  TRaiseObjProc = procedure(P: PExceptionRecord);
begin
  if FPrevRaiseProc <> NIL
  then TRaiseObjProc(FPrevRaiseProc)(P);
end;


{ The hook itself. Fires on EVERY raise, including caught ones. }
procedure LightSaberRaiseExceptObjProc(P: PExceptionRecord);
VAR
  Obj     : TObject;
  EClass  : string;
  EMessage: string;
begin
  if InHook then
    begin
      CallPrevRaiseProc(P);
      EXIT;
    end;
  InHook:= TRUE;
  try
    try
      if P <> NIL then
        begin
          Obj:= TObject(P^.ExceptObject);
          if Obj <> NIL then
            begin
              EClass:= Obj.ClassName;
              if Obj is Exception
              then EMessage:= Exception(Obj).Message
              else EMessage:= '';
            end
          else
            begin
              EClass:= '<nil-exception-object>';
              EMessage:= '';
            end;
        end
      else
        begin
          EClass:= '<nil-exception-record>';
          EMessage:= '';
        end;
      AppendLine(FormatLine(EClass, EMessage, TThread.Current.ThreadID));
    except
      { Hook must never raise. Swallow. }
    end;
  finally
    InHook:= FALSE;
  end;
  CallPrevRaiseProc(P);
end;


procedure InstallExceptionLogger(CONST LogFileName: string = 'Exceptions.log');
VAR
  Dir    : string;
  FileName: string;
begin
  if FInstalled then EXIT;

  Dir:= TPath.GetDocumentsPath;
  if Dir = '' then EXIT;     { no writable per-app path — give up silently; alternative would be to log to in-memory list, but that defeats the purpose }
  if NOT TDirectory.Exists(Dir) then
    try
      TDirectory.CreateDirectory(Dir);
    except
      EXIT;
    end;

  if LogFileName = ''
  then FileName:= 'Exceptions.log'     { defensive — empty arg falls back to the default }
  else FileName:= LogFileName;

  FLogPath:= TPath.Combine(Dir, FileName);
  FLogLock:= TCriticalSection.Create;

  { Write a session-start banner so we can see app launches in the log even with no exceptions. }
  AppendLine('---- Session start: '+ FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss"."zzz"Z"', TTimeZone.Local.ToUniversalTime(Now))+ ' ----' + sLineBreak);

  FPrevRaiseProc:= System.RaiseExceptObjProc;
  System.RaiseExceptObjProc:= @LightSaberRaiseExceptObjProc;

  FInstalled:= TRUE;
end;


INITIALIZATION
FINALIZATION
  { Hook stays installed for the life of the process. Don't unhook in finalization — by then the OS
    is tearing the process down and unhooking races with whatever is still raising on shutdown.
    We still free the critical section so heap-leak detectors stay quiet. }
  if FLogLock <> NIL
  then FreeAndNil(FLogLock);
END.
