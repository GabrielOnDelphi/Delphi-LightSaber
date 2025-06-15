UNIT LightVcl.Common.Debugger;

{=============================================================================================================
   2025.05
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   Functions to:
      Measure code execution time
      Output debug strings
      Test if the application is being debugged
      Generate reports about the system (hardware)
      Generate errors (for testing)
      Check compiler options

   This uses System.Diagnostics.TStopwatch which is platform independent.

   Also see: QuickImageFX\Quick.Chrono.pas

=============================================================================================================}

INTERFACE

USES
   Winapi.Windows, Winapi.MultiMon, Winapi.ShlObj, Winapi.SHFolder,
   System.Diagnostics, System.Classes, System.SysUtils,
   Vcl.Forms, Vcl.Dialogs,
   ccAppData, LightVcl.Common.AppData;

 { ANTI-DEBUGGER PROTECTION }
 procedure AntiDebug; assembler;
 procedure AntiProcDump; assembler;
 procedure ExitIfUnderDebugger (ProjectFileName: string);
 procedure HaltApplication(UserMessage : string);

 function  IsRunningUnderDelphiDebugger: Boolean;
 function  IsDebuggerPresent: Boolean;

 { COMPILER INFO }
 function CompilerOptimization : Boolean;
 function CompilerOptimizationS: String;
 function AppBitness: string;                             { Shows if the program is compiled as 32 or 64bit app }  // old name: AppPlatform

 { CRASH ME }
 procedure GenerateCrashNIL;
 procedure GenerateLeak;
 procedure EmptyDummy(i: Integer);

 { CODE TIMING }
 procedure TimerStart;                                     { use it with: SetPriorityMax  }
 function  TimerElapsed: Double;                           { In miliseconds }
 function  TimerElapsedS: string;                          { In miliseconds or seconds }

 function  ShowTransferSpeed(FileSize: Cardinal): string;  { Shows the disk/internet speed }

 { SYSTEM REPORTS }
 procedure WriteSystemReport(FileName: string);
 function  GenerateSystemRep:    string;

 //function GenerateScreenRep:    string;    // Moved to 3rdParty MonitorHelper.pas
 function  GenerateAppRep:       string;
 function  ScreenResApi:  string;
 function  GenerateWinSysRep:    string;
 function  GenerateWinPathRep:   string;
 function  GenerateWinPathRepEx: string;
 function  GenerateHardwareRep:  string;
 function  GenerateHardwareRepTSL: TStringList;

 { LOGGING }
 procedure OutputDebugStr (s: string);             overload;
 procedure OutputDebugStr (s: string; i: Integer); overload;
 procedure OutputDebugStr (s: string; r: Real);    overload;

 procedure LogFile_Init  (FullFileName: string);    { Init the log  }
 procedure LogFile_Add   (s: string);               { Write in Log }

 {}
 function LastErrorMsgStr: string;

 { DELPHI SPECIFIC }
 function FixEmbarcaderoAtomTableMemLeak: Boolean; Deprecated 'Use in-program leak fixing: 3rdPartyPkg.AtomGarbageCollector.pas.GarbageCollectAtoms(Log)'


IMPLEMENTATION
USES
   LightVcl.Common.SystemTime, LightVcl.Common.System, ccCore, LightVcl.Common.Dialogs, ccIO, ccTextFile, LightVcl.Common.IO, LightVcl.Common.WinVersion, LightVcl.Common.ExecuteProc, LightVcl.Common.SystemPermissions, System.DateUtils;




{--------------------------------------------------------------------------------------------------
   PROTECTION
   Also see c:\MyProjects\Packages\Third party packages\uDebugger.pas
--------------------------------------------------------------------------------------------------}

{ Is the process running as part of Delphi? }
{$IFDEF msWindows}
 {$WARN SYMBOL_PLATFORM OFF} // prevent W1002 Symbol 'DebugHook' is specific to a platform
function IsRunningUnderDelphiDebugger: Boolean;
begin
  Result := (DebugHook <> 0);
end;
 {$WARN SYMBOL_PLATFORM ON}
{$ENDIF}


{ Relies on the IsDebuggerPresent API function in kernel32. But first it tests if the function is available. }
function IsDebuggerPresent: Boolean;
var
  DbgPresentFunc: function: Boolean; stdcall;
  KernelHandle: THandle;
  Ptr: Pointer;
begin
  KernelHandle := GetModuleHandle(kernel32);       // also see  IsDebuggerPresent: BOOL; stdcall; external 'kernel32.dll';
  @DbgPresentFunc := GetProcAddress(KernelHandle, 'IsDebuggerPresent');
  if Assigned(DbgPresentFunc)
  then Result := DbgPresentFunc
  else
   begin
     // We are bellow Windows 98
     Ptr := GetProcAddress(KernelHandle, 'GetProcAddress');
     Result:= LongWord(Ptr) < KernelHandle;
   end;
end;







TYPE
   EApplicationFail = class(Exception);

procedure HaltApplication(UserMessage : string);
begin
   Application.Terminate;
   Raise EApplicationFail.Create(UserMessage);
end;


procedure ExitIfUnderDebugger(ProjectFileName: string);
begin
 if IsDebuggerPresent                 // IsDebuggerPresent is external 'kernel32.dll';
 AND NOT FileExists(ProjectFileName)
 then Application.Terminate;
end;


procedure AntiProcDump; assembler;
asm
    MOV EAX, fs:[30h]
    TEST EAX, EAX
    JS @is9x

 @isNT:
    MOV EAX, [EAX+0Ch]
    MOV EAX, [EAX+0Ch]
    ADD DWORD PTR [EAX+20h], 2000h                                                             {increase size variable}
    JMP @finished

 @is9x:
    PUSH 0
    CALL GetModuleHandleA
    TEST EDX, EDX
    JNS @finished                                                                              {Most probably incompatible!!!}
    CMP DWORD PTR [EDX+8], -1
    JNE @finished                                                                              {Most probably incompatible!!!}
    MOV EDX, [EDX+4]                                                                           {get address of internaly used}
                                                                                               {PE header}
    ADD DWORD PTR [EDX+50h], 2000h                                                             {increase size variable}

 @finished:
end;


procedure Antidebug; assembler;
asm
   jmp @jump;
   db $b8; // fake mov-instruction
    @fake1: jmp @ende;
  @endlos:
   int 3
   xor ax,ax
   jmp @endlos;

  @jump:
    jmp @fake1
  @ende:
end;



{-------------------------------------------------------------------------------------------------------------
   COMPILER
-------------------------------------------------------------------------------------------------------------}

{ Importan note:
   $O+ has a local scope, therefore, the result of the function reflects only the optimization state at that specific source code location.
   So, if you are using the $O switch to optimize pieces of code then the function MUST be used as a subfunction;
   Otherwise, if you use the global switch ONLY (in Project Options) it can be used as a normal (declared) function. }

{ Returns true in the compiler optimization is on (probably we are in release mode, in this case) }
function CompilerOptimization: Boolean;
begin
 {$IfOpt O+}
 Result:= TRUE;
 {$Else}
 Result:= FALSE;
 {$EndIf}
end;


{ Same as above }
function CompilerOptimizationS: String;
begin
 Result:= 'Compiler optimization is ' +
 {$IfOpt O+}
 'enabled'
 {$Else}
 'disabled'
 {$EndIf}
end;


{ Shows if the program is compiled as 32 or 64bit app }
function AppBitness: String;
begin
 {$IF Defined(CPUX86)}
   Result:= '32bit';
 {$ELSEIF Defined(CPUX64)}
   Result:= '64bit';
 {$ELSE}
   {$Message Fatal 'Unknown CPU'}  {TODO 2: do this in all functions that are platform conditionated }        { Contitional compilation: http://docwiki.embarcadero.com/RADStudio/XE8/en/Conditional_compilation_%28Delphi%29 }
 {$ENDIF}
end;







{--------------------------------------------------------------------------------------------------
   CODE TIMING
--------------------------------------------------------------------------------------------------
   TStopwatch is a wrap arround QueryPerformanceCounter which according to Microsoft has resolution < 1us.

   WARNING!
     The value of Elapsed is only updated if the stopwatch is priorly stopped. Reading the Elapsed property while the stopwatch is running does not yield any difference.

   How to use it:
      OnFormCreate -> SetPriorityMax;

      TimerStart;
      MySlowFunction;
      Caption:= TimerElapsedS;

   Use it only for small intervals (way under 1 day)!

   Source: http://stackoverflow.com/questions/6420051/why-queryperformancecounter-timed-different-from-wall-clock
   https://blogs.msdn.microsoft.com/oldnewthing/20050902-00/?p=34333
--------------------------------------------------------------------------------------------------}
VAR
   sw: TStopWatch;              { This is a record not an class! }

procedure TimerStart;
begin
  if NOT TStopWatch.IsHighResolution
  then MessageWarning('High resolution timer not availalbe!');
  sw := TStopWatch.Create;      { Hint: We can use directly: TStopWatch.CreateNew but SilverWarior says there is a bug in it. Maybe this one? https://codeverge.com/embarcadero.delphi.win32/tstopwatch-a-bug-delphi-2010/1046096 }
  sw.Start;
end;


{ Returns the time elapsed, in miliseconds }
function TimerElapsed: Double;
begin
  sw.Stop;
  Result:= sw.ElapsedMilliseconds;    {WARNING!  The value of Elapsed is only updated if the stopwatch is priorly stopped. Reading the Elapsed property while the stopwatch is running does not yield any difference. }
end;



function TimerElapsedS: string;
VAR NanoSec: Int64;
begin
  sw.Stop;
  NanoSec:= sw.Elapsed.Ticks*100;     { is in 100ns increments Elapsed.Ticks }

  if NanoSec < 1000
  then Result := IntToStr(NanoSec)+ 'ns'
  else
    if NanoSec < 1000000
    then Result := Real2Str(NanoSec / 1000, 3)+ 'us'
    else
      if NanoSec < 1000000000
      then Result := Real2Str(NanoSec / 1000000, 3)+ 'ms'
      else
        if NanoSec < 1000000000000
        then Result := Real2Str(NanoSec / 1000000000, 3)+ 's'
        else Result := Real2Str(NanoSec / 60*1000000000, 3)+ 'm'
end;


{ In seconds/miliseconds }   (*
function TimerElapsedS: string;
VAR
   elapsedMilliseconds : Int64;
begin
 sw.Stop;
 elapsedMilliseconds:= sw.ElapsedMilliseconds;  {WARNING!  The value of Elapsed is only updated if the stopwatch is priorly stopped. Reading the Elapsed property while the stopwatch is running does not yield any difference. }

 if elapsedMilliseconds = 0         // If the time is too small we cannot show it as 0ms so we show it as "high precision"
 then Result:= sw.Elapsed.ToString + 'ms'
 else
   if elapsedMilliseconds < 1000
   then Result:= Real2Str(elapsedMilliseconds, 12)+ 'ms'
   else Result:= Real2Str(elapsedMilliseconds / 1000, 2)+ 's';
end;
*)



{ Shows the disk/internet trnasfer speed. Use it in conjuction with TimerStart.
  Usage:
        TimerStart;
        CopyFile(FileName);
        ShowTransferSpeed(GetFileSize(FileName))   }
function ShowTransferSpeed(FileSize: Cardinal): string;
begin
 Result:= 'Speed '+ FormatBytes( Round(FileSize / (TimerElapsed / 1000)), 1)+ '/sec';
end;





{--------------------------------------------------------------------------------------------------
   LOGGING
--------------------------------------------------------------------------------------------------}
procedure OutputDebugStr(s: string);
begin
 s:= s+ Tab;
 WinApi.Windows.OutputDebugString( PChar(s) );   { Poate fi folosita cu SysInternals DebugView }
end;


procedure OutputDebugStr(s: string; i: Integer);
begin
 WinApi.Windows.OutputDebugString( PChar(s+ IntToStr(i)+ Tab) );
end;


procedure OutputDebugStr(s: string; r: Real);
begin
 WinApi.Windows.OutputDebugString( PChar(s+ Real2Str(r, 3)+ Tab) );
end;



{--------------------------------------------------------------------------------------------------
   LOGGING
   Writes strings to a Log file that is placed in app's data folder.
   You must use AppLog_Init one time before calling AppLog_Add.
--------------------------------------------------------------------------------------------------}

VAR LogFile: string;

procedure LogFile_Init(FullFileName: string);                                 { Init the log  }
begin
 LogFile:= FullFileName;
 ForceDirectoriesMsg(ExtractFilePath(FullFileName));

 if FileExists(LogFile)
 then DeleteFile(LogFile);                                               { Clear existing log }

 StringToFile(LogFile, LogFile+ CRLF+ TAppData.AppName+ ' v'+ TAppData.GetVersionInfo+ CRLF+ DateTimeToStr(Now)+ CRLF +LBRK, woAppend);
end;


procedure LogFile_Add(s: string);                                       { Writes a tring to the 'crash' log file. The writing happens only if a file called CrashLog.txt' is present in AppData.AppDataFolder. The file is cleared every time I call CrashLog_Init }
begin
 if FileExists(LogFile)
 then StringToFile(LogFile, s+ CRLF, woAppend);
end;




{--------------------------------------------------------------------------------------------------
   DEBUGER
--------------------------------------------------------------------------------------------------}
procedure GenerateCrashNIL;
VAR T: TObject;
begin
 T:= NIL;
 T.ClassName;  // We could also simply use "Raise"
end;


procedure GenerateLeak;
VAR T: TObject;
begin
  T:= TObject.Create;
  T.ToString;
end;


{ Call it after a routine API function/procedure/call, to see what that procedure returned }
function LastErrorMsgStr: String;
VAR szError: array [0..255] of Char;
begin
  Result:= '';
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, GetLastError(), 0, szError, sizeof(szError), nil);
  ShowMessage(String(szError));
end;




{--------------------------------------------------------------------------------------------------
   REPORTS
   Spacing optimized for Lucinda Console monospaced font!
--------------------------------------------------------------------------------------------------}
procedure WriteSystemReport(FileName: string);
VAR TSR: TStringList;
begin
 TSR:= TStringList.Create;
 TSR.Text:= GenerateSystemRep;
 TSR.SaveToFile(FileName);
 FreeAndNil(TSR);
end;



function GenerateSystemRep: string;
begin
 Result:= '=< SYSTEM REPORT >='+ CRLF;
 Result:= Result+ CRLF;
 Result:= Result+ GenerateAppRep      + CRLF+ CRLF;                                 { The AppDataPath parameter lets the user provide the data path in case the app is not using the Default data path }
 Result:= Result+ GenerateWinSysRep   + CRLF+ CRLF;
 Result:= Result+ GenerateHardwareRep + CRLF+ CRLF;                                 { Before calling this I need to enter a valid key into chHardID:   chHardID.HDIDValid:= TRUE;  }
 Result:= Result+ GenerateWinPathRep  + CRLF+ CRLF;
 Result:= Result+ GenerateWinPathRepEx+ CRLF+ CRLF;
 //Result:= Result+ GenerateScreenRep   + CRLF+ CRLF;    Moved to 3rdParty MonitorHelper.pas
end;




{--------------------------------------------------------------------------------------------------
   INDIVIDUAL REPORTS
--------------------------------------------------------------------------------------------------}

{Note: A super detailed report can be obtained via Hardware Extractor ID which can be obtained from https://www.soft.tahionic.com/download-hdd_id/index.html }
function GenerateHardwareRep: string;
begin
 Result:= ' [HARDWARE]'+ CRLF;
 Result:= Result+'  User name: '      + Tab+ Tab+ GetUserName+ CRLF;
 Result:= Result+'  UserName Ex: '    + Tab+ Tab+ GetUserNameEx(2)+ CRLF;
 Result:= Result+'  Computer name: '  + Tab+ GetComputerName+ CRLF;                 { Also see GetComputerNameEx:   http://stackoverflow.com/questions/30778736/how-to-get-the-full-computer-name-in-inno-setup/30779280#30779280 }
 Result:= Result+'  Domain name: '    + Tab+ Tab+ GetDomainName+ CRLF;
 Result:= Result+'  Host name: '      + Tab+ Tab+ GetHostName+ CRLF;
 Result:= Result+'  Total monitors: ' + Tab+ IntToStr(Screen.MonitorCount)+ CRLF;
 Result:= Result+'  Screen res:     ' + Tab+ IntToStr(Screen.Width)+ 'x'+ IntToStr(Screen.Height)+ CRLF;
 Result:= Result+'  '+ ScreenResApi+ CRLF;
 Result:= Result+'  Free space: '     + Tab+ Tab+ DriveFreeSpaceS ('C') + CRLF;
 //Result:= Result+'  Local IP: '     + Tab+ Tab+ LightVcl.Internet.GetLocalIP+ CRLF;
end;



{ THIS WILL RETURN THE VIRTUALISED RESOLUTION (when high DPI is set).
  It is useles if we want to get the real resolution.
   http://stackoverflow.com/questions/7077572/get-current-native-screen-resolution-of-all-monitors-in-delphi-directx }
function ScreenResApi: string;
VAR MonInfo: TMonitorInfo;
begin
 MonInfo.cbSize := SizeOf(MonInfo);
 Assert(Application.MainForm <> NIL, 'MainForm is nil. This happens usually when code is initialized in OnFormCreate.');
 GetMonitorInfo(MonitorFromWindow(Application.MainForm.Handle, MONITOR_DEFAULTTONEAREST), @MonInfo);
 Result:= Format('Monitor resolution (API): %dx%d', [MonInfo.rcMonitor.Right - MonInfo.rcMonitor.Left, MonInfo.rcMonitor.Bottom - MonInfo.rcMonitor.Top]);
end;


function GenerateHardwareRepTSL: TStringList;
begin
 Result:= TStringList.Create;
 Result.Text:= GenerateHardwareRep;
end;


{ The AppDataPath parameter lets the user provide the data path in case the app is not using the Default data path }
function GenerateAppRep: string;
begin
 TAppData.AppDataFolder(True);
 Result:= ' [APPLICATION]'+ CRLF;
 Result:= Result+'  AppDataFolder: '       + Tab + TAppData.AppDataFolder+ CRLF;
 Result:= Result+'  AppData.IniFile: '     + Tab + TAppData.IniFile+ CRLF;
 ///Result:= Result+'  AppData.ExeFolder: '   + Tab + AppDataEx.CurFolder+ CRLF;
 Result:= Result+'  Exe name: '            + Tab + Tab + Application.ExeName+ CRLF;
 Result:= Result+'  Version: '             + Tab + Tab + TAppData.GetVersionInfo+ CRLF;
 Result:= Result+'  RunningUnderDelphi: '  + Tab + BoolToStrYesNo(IsRunningUnderDelphiDebugger)+ CRLF;
 Result:= Result+'  IsDebuggerPresent: '   + Tab + BoolToStrYesNo(IsDebuggerPresent)+ CRLF;
 Result:= Result+'  AppBitness: '          + Tab + AppBitness;
 Result:= Result+'  CompilerOptimiz: '     + Tab + CompilerOptimizationS+ CRLF;
end;


function GenerateWinSysRep: string;
begin
 Result:= ' [SYSTEM/OS]'+ CRLF;
 Result:= Result+'  OS platform: '          + Tab+ Tab+ LightVcl.Common.WinVersion.GetOSName+ CRLF;
 Result:= Result+'  OS architecture: '      + Tab     + LightVcl.Common.WinVersion.Architecture+ CRLF;
 Result:= Result+'  App has admin rights: ' + BoolToStr(AppHasAdminRights, TRUE)+ CRLF;
 Result:= Result+'  Invalid system time: '  + Tab+ BoolToStr(SystemTimeIsInvalid , TRUE)+ CRLF;
 Result:= Result+'  Windows up time: '      + Tab+ Date2FormatAuto(WindowsUpTime);
end;


{ Useful paths }
function GenerateWinPathRep: string;
begin
 Result:= ' [PATHS]'+ CRLF;
 Result:= Result+'  Windows: '              + Tab+Tab + GetSpecialFolder(CSIDL_WINDOWS)+ CRLF;
 Result:= Result+'  System: '               + Tab+Tab + GetSpecialFolder(CSIDL_SYSTEM)+ CRLF;
 Result:= Result+'  COMMON APPDATA: '       + Tab     + GetSpecialFolder(CSIDL_COMMON_APPDATA)+ CRLF;
 Result:= Result+'  Program Files: '        + Tab     + GetSpecialFolder(CSIDL_PROGRAM_FILES)+ CRLF;          { C:\Program Files }
 Result:= Result+'  Program Files cmn: '    + Tab     + GetSpecialFolder(CSIDL_PROGRAM_FILES_COMMON)+ CRLF;   { C:\Program Files\Common }
 Result:= Result+'  APPDATA: '              + Tab+Tab + GetSpecialFolder(CSIDL_APPDATA)+ CRLF;
 Result:= Result+'  LOCAL APPDATA: '        + Tab+      GetSpecialFolder(CSIDL_LOCAL_APPDATA)+ CRLF;
 Result:= Result+'  COMMON DOCUMENTS: '     + Tab     + GetSpecialFolder(CSIDL_COMMON_DOCUMENTS)+ CRLF;       { All Users\Documents }
 Result:= Result+'  PERSONAL: '             + Tab+Tab + GetSpecialFolder(CSIDL_PERSONAL);
end;


{ Some other less useful paths }
function GenerateWinPathRepEx: string;
begin
 Result:= ' [PATHS 2]'+ CRLF;
 Result:= Result+'  COMMON DESKTOP DIR: ' + Tab     + GetSpecialFolder(CSIDL_COMMON_DESKTOPDIRECTORY)+ CRLF;
 Result:= Result+'  DESKTOP DIR: '        + Tab+Tab + GetSpecialFolder(CSIDL_DESKTOPDIRECTORY)+ CRLF; // // <user name>\Desktop
 Result:= Result+'  PROGRAMS: '           + Tab+Tab + GetSpecialFolder(CSIDL_PROGRAMS)+ CRLF;
 Result:= Result+'  STARTUP: '            + Tab+Tab + GetSpecialFolder(CSIDL_STARTUP)+ CRLF;
 Result:= Result+'  STARTMENU: '          + Tab+Tab + GetSpecialFolder(CSIDL_STARTMENU)+ CRLF;
 Result:= Result+'  COMMON STARTMENU: '   + Tab     + GetSpecialFolder(CSIDL_COMMON_STARTMENU)+ CRLF;
 Result:= Result+'  COMMON STARTUP: '     + Tab     + GetSpecialFolder(CSIDL_COMMON_STARTUP)+ CRLF;
 Result:= Result+'  COMMON PROGRAMS: '    + Tab     + GetSpecialFolder(CSIDL_COMMON_PROGRAMS)+ CRLF;
 Result:= Result+'  FONTS:      '         + Tab+Tab + GetSpecialFolder(CSIDL_FONTS);
end;




{--------------------------------------------------------------------------------------------------
   Fixes this Delphi bug:
    EOsError-System Error. Code:_8. Not enough storage is available to process this command
    Details: https://stackoverflow.com/questions/507853/system-error-code-8-not-enough-storage-is-available-to-process-this-command

    Set LastLeakFix:= Now; at program start up.
--------------------------------------------------------------------------------------------------}

VAR
   LastLeakFix: TDateTime= 0;   { The time when the fix ran for the last time }

function FixEmbarcaderoAtomTableMemLeak: Boolean;  { Deprecated "Use in-program leak fixing: 3rdPartyPkg.AtomGarbageCollector.pas.GarbageCollectAtoms(Log)" }
begin
 if (System.DateUtils.MinutesBetween(now, LastLeakFix) > 15)
 AND FileExists(AppData.SysDir+ 'AtomGarbageCollector.exe')
 then
  begin
   Result:= LightVcl.Common.ExecuteProc.ExecuteProc(AppData.SysDir+ 'AtomGarbageCollector.exe', SW_HIDE);
   LastLeakFix:= now;
  end
 else Result:= TRUE;
end;







procedure EmptyDummy(i: Integer);
begin
 MessageInfo(IntToStr(i));
end;



end.

