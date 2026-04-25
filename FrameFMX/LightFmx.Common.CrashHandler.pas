UNIT LightFmx.Common.CrashHandler;

{=============================================================================================================
   2026.04.25
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Minimal cross-platform crash logger.

   Why this exists:
   - madExcept and EurekaLog are Windows-only.
   - On Android/iOS an unhandled exception silently kills the process: no
     dialog, no stack trace, no clue what happened.
   - This unit installs Application.OnException and writes one line per
     uncaught exception to <AppDataFolder>\crash.log. On next launch the
     application can read the file, show it to the user (or upload it),
     then clear it.

   Known limits (Phase B baseline):
   1. NO STACK TRACE. Captures only Exception class name + message + timestamp.
      One-line summary, nothing more. Phase C: pull in Grijjy.ErrorReporting.pas
      and append the symbolicated stack to crash.log.
      Source: github.com/grijjy/JustAddCode/tree/master/ErrorReporting
      Research: c:\Delphi\FMX\Bug reporter FMX\Crash Reporting Tools for Delphi FMX Android.md

   2. MAIN-THREAD ONLY. Application.OnException catches exceptions only on the
      UI thread. Anything that escapes TTask.Run / TThread.Execute past the
      thread's own try/except dies silently — same Android symptom as Issue 14.
      Phase C: also hook ExceptionAcquired (per-thread) and ExceptProc (global
      runtime fallback). Both are documented in Grijjy.ErrorReporting.pas.

   3. NO USER-FACING REPORT FLOW. crash.log is shown via ShowMessage on next
      launch. No upload, no email. Phase C: replace ShowMessage with
      Intent.ACTION_SEND (Android) / mailto: (desktop) prompt that attaches
      crash.log.

   4. SHOWMESSAGE FROM FORMCREATE may cause focus / dialog-ordering glitches
      on Android. If observed, defer the prompt with TThread.ForceQueue
      (same pattern used by FormLessonChat.btnDoneSessionClick).

   Usage:
   - In the DPR, after AppData.Create, before AppData.Run:
        InstallCrashHandler;
   - In the main form's OnCreate, after AppData is fully ready:
        if HasPendingCrashLog then
          begin
            ShowMessage('Previous session crashed:'#13#10 + ReadPendingCrashLog);
            ClearPendingCrashLog;
          end;
==============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Classes,
  FMX.Forms;

procedure InstallCrashHandler;                  // Idempotent. Call once early in DPR.
function  CrashLogPath: string;                 // <AppDataFolder>\crash.log. Returns '' if AppData not ready.
function  HasPendingCrashLog: Boolean;          // True if a crash.log from a previous session exists.
function  ReadPendingCrashLog: string;          // File contents, or '' if no file.
procedure ClearPendingCrashLog;                 // Deletes the file. Safe to call when no file exists.


IMPLEMENTATION

USES
  LightCore.AppData, LightCore.LogRam, LightCore.TextFile, LightCore.IO;

CONST
  CRASH_LOG_FILENAME = 'crash.log';

TYPE
  // Application.OnException is TExceptionEvent = procedure(Sender: TObject; E: Exception) OF OBJECT.
  // It needs a method, not a standalone procedure — hence this tiny wrapper class.
  // The instance is owned by the global Application object so it is freed cleanly on shutdown.
  TCrashHandlerHook = class(TComponent)
    procedure HandleException(Sender: TObject; E: Exception);
  end;

VAR
  HookInstance: TCrashHandlerHook = nil;        // Set once in InstallCrashHandler. Owned by Application.


{ The path to the crash log file. Returns '' if AppData has not been constructed yet
  (defensive — InstallCrashHandler is called AFTER AppData.Create, so this should not happen
  in normal flow, but a very-early exception during DPR initialization could hit this path). }
function CrashLogPath: string;
begin
  if TAppDataCore.AppName = ''
  then Result:= ''
  else Result:= TAppDataCore.AppDataFolder(True) + CRASH_LOG_FILENAME;
end;


{ Append one line to crash.log. Wrapped in TRY/EXCEPT because the crash handler must
  never raise — that would mask the original exception or, on Android, recurse into
  another silent kill. }
procedure WriteCrashLog(CONST Text: string);
VAR Path: string;
begin
  TRY
    Path:= CrashLogPath;
    if Path = '' then EXIT;
    StringToFile(Path, Text + sLineBreak, woAppend);
  EXCEPT
    // Swallow. We are already in a degraded state; nothing useful to do here.
  END;
end;


procedure TCrashHandlerHook.HandleException(Sender: TObject; E: Exception);
VAR Line: string;
begin
  // Application.OnException always passes a non-nil E, but be defensive — a third
  // party could call HandleException directly.
  if NOT Assigned(E) then EXIT;

  // Outer try/except: an exception escaping THIS handler would bypass OnException
  // (the dispatcher does not re-enter for handler failures) and on Android would
  // silently kill the process — exactly what we are trying to prevent.
  TRY
    // ISO-style timestamp so logs sort and parse the same regardless of user locale
    // (DateTimeToStr would render 'dd/mm/yyyy' or 'mm/dd/yyyy' depending on the OS).
    Line:= Format('%s | %s: %s',
                  [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), E.ClassName, E.Message]);

    // Mirror to RamLog so the in-app log viewer sees it during the current session
    if Assigned(AppDataCore) AND Assigned(AppDataCore.RamLog)
    then AppDataCore.RamLog.AddError('Unhandled: ' + Line);

    // Persist to disk so the next session can show the user what happened
    WriteCrashLog(Line);
  EXCEPT
    // Swallow. We are already in a degraded state; nothing useful to do here.
  END;
end;


procedure InstallCrashHandler;
begin
  if Assigned(HookInstance) then EXIT;          // Idempotent
  HookInstance:= TCrashHandlerHook.Create(Application);  // Application owns and frees it
  Application.OnException:= HookInstance.HandleException;
end;


function HasPendingCrashLog: Boolean;
VAR Path: string;
begin
  Path:= CrashLogPath;
  Result:= (Path <> '') AND FileExists(Path);
end;


function ReadPendingCrashLog: string;
VAR Path: string;
begin
  Path:= CrashLogPath;
  if Path = ''
  then Result:= ''
  else Result:= StringFromFileExists(Path);     // Returns '' if file does not exist; never raises
end;


procedure ClearPendingCrashLog;
VAR Path: string;
begin
  Path:= CrashLogPath;
  if Path <> ''
  then TryDeleteFile(Path);                     // Returns FALSE if file missing or locked; never raises
end;


END.
