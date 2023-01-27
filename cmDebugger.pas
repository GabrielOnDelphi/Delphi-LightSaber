UNIT cmDebugger;

{=============================================================================================================
   2023.01
   See Copyright.txt
==============================================================================================================

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
   Winapi.Windows, Winapi.MultiMon, System.Diagnostics, System.Classes, System.SysUtils, Vcl.Forms, Winapi.ShlObj, Winapi.SHFolder, Vcl.Dialogs;


 { ANTI-DEBUGGER PROTECTION }
 procedure AntiDebug; assembler;
 procedure AntiProcDump; assembler;
 procedure ExitIfUnderDebugger (ProjectFileName: string);
 procedure HaltApplication(UserMessage : string);

 function  IsRunningUnderDelphiDebugger: Boolean;          // new (to test)
 function  IsDebuggerPresent: Boolean;

 { COMPILER INFO }
 function CompilerOptimization : Boolean;
 function CompilerOptimizationS: String;
 function PlatformBitness: string;                         { Shows if the program is compiled as 32 or 64bit app }  // old name: AppPlatform

 { CRASH ME }
 procedure GenerateCrashNIL;
 procedure GenerateLeak;

 { CODE TIMING }
 procedure TimerStart;                                     { use it with: SetPriorityMax  }
 function  TimerElapsed: Double;                           { In miliseconds }
 function  TimerElapsedS: string;                          { In miliseconds or seconds }

 function  ShowTransferSpeed(FileSize: Cardinal): string;  { Shows the disk/internet speed }

 { SYSTEM REPORTS }
 procedure WriteSystemReport(FileName: string);
 function  GenerateSystemRep:    string;

 function  GenerateAppRep:       string;
 function  GenerateScreenRep:    string;
 function  GenerateScreenRepEx:  string;
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


{.$DEFINE IncludeMonitorHelper}  { Undefine this if you don't have MonitorHelper.pas. It can be obtained from  https://github.com/r1me/delphi-monitorhelper }

IMPLEMENTATION
USES
   cmSystem, ccCore, ccAppData, ccIO, ccWinVersion, cmExecuteProc, cmPermissions, System.DateUtils
   {$IFDEF IncludeMonitorHelper}, MonitorHelper{$ENDIF};


   

{--------------------------------------------------------------------------------------------------
   PROTECTION
   Also see c:\MyProjects\Packages\Third party packages\uDebugger.pas
--------------------------------------------------------------------------------------------------}

{ Is the process running as part of Delphi? }
function IsRunningUnderDelphiDebugger: Boolean;
begin
 {$WARN SYMBOL_PLATFORM OFF} // prevent W1002 Symbol 'DebugHook' is specific to a platform
  Result := (DebugHook <> 0);
 {$WARN SYMBOL_PLATFORM OFF}
end;


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
function PlatformBitness: String;
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
  then MesajWarning('High resolution timer not availalbe!');
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

 StringToFile(LogFile, LogFile+ CRLF+ AppData.AppName+ ' v'+ AppData.GetVersionInfo+ CRLF+ DateTimeToStr(Now)+ CRLF +LBRK, woAppend);
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
 T.ClassName;  // I could say "raise"
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
 Result:= Result+ GenerateHardwareRep + CRLF+ CRLF;                                 { Before calling this I need to enter a valid key into cmHardID:   cmHardID.HDIDValid:= TRUE;  }
 Result:= Result+ GenerateScreenRep   + CRLF+ CRLF;
 Result:= Result+ GenerateWinPathRep;
end;




{--------------------------------------------------------------------------------------------------
   INDIVIDUAL REPORTS
--------------------------------------------------------------------------------------------------}

{Note: A super detailed report can be obtained via Hardware Extractor ID which can be obtained from https://www.soft.tahionic.com/download-hdd_id/index.html }
function GenerateHardwareRep: string;
begin
 Result:= ' [HARDWARE]'+ CRLF;
 Result:= Result+'  Computer name: '  + Tab+ GetComputerName+ CRLF;                 { Also see GetComputerNameEx:   http://stackoverflow.com/questions/30778736/how-to-get-the-full-computer-name-in-inno-setup/30779280#30779280 }
 Result:= Result+'  User name: '      + Tab+ Tab+ GetLogonName+ CRLF;
 Result:= Result+'  Total monitors: ' + Tab+ IntToStr(Screen.MonitorCount)+ '. Screen res: ' + IntToStr(Screen.Width)+ 'x'+ IntToStr(Screen.Height)+ CRLF;
 Result:= Result+'  Free space: '     + Tab+ Tab+ ccIO.DriveFreeSpaceS ('C') + CRLF;
 //Result:= Result+'  Local IP: '     + Tab+ Tab+ ciInternet.GetLocalIP+ CRLF;
end;



function GenerateHardwareRepTSL: TStringList;
begin
 Result:= TStringList.Create;
 Result.Text:= GenerateHardwareRep;
end;


{ The AppDataPath parameter lets the user provide the data path in case the app is not using the Default data path }
function GenerateAppRep: string;
begin
 AppData.AppDataFolder(True);
 Result:= ' [APPLICATION]'+ CRLF;
 Result:= Result+'  Exe name: '            + Tab + Application.ExeName+ CRLF;
 Result:= Result+'  AppDataFolder: '       +       AppData.AppDataFolder+ CRLF;
 Result:= Result+'  AppData.CurFolder: '   + Tab + AppData.CurFolder+ CRLF;
 Result:= Result+'  AppData.IniFile: '     + Tab + AppData.IniFile+ CRLF;
 Result:= Result+'  Version: '             + Tab + AppData.GetVersionInfo;
end;



function GenerateScreenRep: string;
begin
 Result:= ' [SCREEN]'+ CRLF;
 Result:= Result+ CRLF;
 Result:= Result+ '  Total monitors: '     + Tab+ IntToStr(Screen.MonitorCount)+ CRLF;
 Result:= Result+ '  Desktop rect:'        + Tab+ Rectangle2Str(Screen.DesktopRect)+ CRLF;                         { Specifies the boundaries of the virtual desktop relative to the upper-left corner of the primary monitor. Use DesktopRect to determine the coordinates of the entire virtual desktop, which includes all monitors in the system. DesktopRect is expressed in coordinates where (0,0) is the upper-left corner of the primary monitor. The primary monitor is the monitor with its Primary property set to true. }
 Result:= Result+ '  Desktop size: '       + Tab+ IntToStr(Screen.DesktopWidth)+ 'x'+ IntToStr(Screen.DesktopHeight)+ CRLF;
 Result:= Result+ '  Screen size: '        + Tab+ IntToStr(Screen.Width)+ 'x'+ IntToStr(Screen.Height)+ CRLF;
 Result:= Result+ '  Screen.WorkArea: '    + Tab+ IntToStr(Screen.WorkAreaWidth)+ 'x'+ IntToStr(Screen.WorkAreaHeight)+ CRLF;  { Use Screen to obtain information about the current state of the screen in an application. }
 Result:= Result+ '  Screen.PixPerInch: '  + Tab+ IntToStr(Screen.PixelsPerInch)+ CRLF;
 Result:= Result+ '  WorkareaRect: '       + Tab+ Rectangle2Str(Screen.WorkAreaRect)+ CRLF;  { Use Screen to obtain information about the current state of the screen in an application. }
 Result:= Result+ CRLF;
 Result:= Result+ CRLF;

 Result:= Result+ ' [MONTORS]'+ CRLF;
 Result:= Result+ CRLF;
 for var i:= 0 to Screen.MonitorCount-1 DO     { Enumerate through all monitors }
  begin
   Result:= Result+ '  Monitor '+ IntToStr(i)+''+ CRLF;
   Result:= Result+ '    Monitor no: '     + Tab+ IntToStr(Screen.Monitors[i].MonitorNum)+ CRLF;
   {$IFDEF IncludeMonitorHelper}
   Result:= Result+ '    DeviceID: '       + Tab+Tab+ Screen.Monitors[i].DeviceID+ CRLF;
   Result:= Result+ '    FriendlyName: '   + Tab+ Screen.Monitors[i].FriendlyName+ CRLF;
   {$ENDIF}
   Result:= Result+ '    BoundsRect:'      + Tab+Tab+ Rectangle2Str(Screen.Monitors[i].BoundsRect)+ CRLF;       { Real monitor area. Indicates the dimensions of the monitor in pixels. Read BoundsRect to learn the dimensions of the monitor. BoundsRect gives the dimensions of the monitor in pixels, where (0,0) represents the top-left corner of the primary monitor. The top of BoundsRect is given by the Top property, the left edge by the Left property, and the height and width by the Height and Width properties respectively. Note:  The BoundsRect property does not take into account any task bars or tool bars docked on the monitor. To determine the area on the monitor that is free of such docked Winapi.Windows, use the WorkareaRect property. }
   Result:= Result+ '    Workarea rect: '  + Tab+ Rectangle2Str(Screen.Monitors[i].WorkareaRect)+ CRLF;     { Monitor's usable area (without task bar). Gives the application useable area of the monitor. WorkareaRect returns a TRect value furnished with the coordinates and dimensions of the work area of the Monitor. On Winapi.Windows, for example, the application tabs at the screen mean that the Workarea is smaller than the monitor size. Note:  The TRect Right and Bottom values are one pixel beyond Workarea boundary. They are given these values to allow for easy calculation of Workarea width and height as (Right-Left) and (Bottom-Top) respectively.}
   Result:= Result+ '    Is Primary: '     + Tab+ BoolToStrYesNo(Screen.Monitors[i].Primary)+ CRLF;
   Result:= Result+ '    Top: '            + Tab+Tab+ IntToStr( Screen.Monitors[i].Top   )+ CRLF;
   Result:= Result+ '    Left: '           + Tab+Tab+ IntToStr( Screen.Monitors[i].Left  )+ CRLF;
   Result:= Result+ '    Width: '          + Tab+Tab+ IntToStr( Screen.Monitors[i].Width )+ CRLF;
   Result:= Result+ '    Height: '         + Tab+Tab+ IntToStr( Screen.Monitors[i].Height)+ CRLF;
   Result:= Result+ '    DPI: '            + Tab+Tab+ IntToStr( Screen.Monitors[i].PixelsPerInch)+ CRLF;
   //Result:= Result+ '    MonitorFromRect(mdNearest): ' + IntToStr( (Screen.MonitorFromRect( TRect.Create(P,100,100), mdNearest)).MonitorNum);
   Result:= Result+ CRLF;
  end;

 Result:= RemoveLastEnter(Result);
end;



{ THIS WILL RETURN THE VIRTUALISED RESOLUTION (when high DPI is set) SO IT IS USELESS
   http://stackoverflow.com/questions/7077572/get-current-native-screen-resolution-of-all-monitors-in-delphi-directx }
function GenerateScreenRepEx: string;
VAR MonInfo: TMonitorInfo;
begin
 MonInfo.cbSize := SizeOf(MonInfo);
 GetMonitorInfo(MonitorFromWindow(Application.MainForm.Handle, MONITOR_DEFAULTTONEAREST), @MonInfo);
 Result:= Format('    Monitor resolution (API): %dx%d', [MonInfo.rcMonitor.Right - MonInfo.rcMonitor.Left, MonInfo.rcMonitor.Bottom - MonInfo.rcMonitor.Top]);
end;



function GenerateWinSysRep: string;
begin
 Result:= ' [SYSTEM]'+ CRLF;
 Result:= Result+'  OS platform: '          + Tab+ Tab+ ccWinVersion.GetOSName+ CRLF;
 Result:= Result+'  OS architecture: '      + Tab     + ccWinVersion.Architecture+ CRLF;
 Result:= Result+'  App has admin rights: ' + BoolToStr(AppHasAdminRights, TRUE)+ CRLF;
 Result:= Result+'  Invalid system time: '  + Tab+ BoolToStr(SystemTimeIsInvalid , TRUE)+ CRLF;
 Result:= Result+'  Windows up time: '      + Tab+ Date2FormatAuto(WindowsUpTime);
end;



function GenerateWinPathRep: string;
begin
 Result:= ' [PATHS]'+ CRLF;
 Result:= Result+'  Windows: '              + Tab+Tab + GetSpecialFolder(CSIDL_WINDOWS)+ CRLF;
 Result:= Result+'  System: '               + Tab+Tab + GetSpecialFolder(CSIDL_SYSTEM)+ CRLF;
 Result:= Result+'  Program Files: '        + Tab     + GetSpecialFolder(CSIDL_PROGRAM_FILES)+ CRLF;          { C:\Program Files }
 Result:= Result+'  Program Files cmn: '    + Tab     + GetSpecialFolder(CSIDL_PROGRAM_FILES_COMMON)+ CRLF;   { C:\Program Files\Common }
 Result:= Result+'  APPDATA: '              + Tab+Tab + GetSpecialFolder(CSIDL_APPDATA)+ CRLF;
 Result:= Result+'  LOCAL APPDATA: '        + Tab+      GetSpecialFolder(CSIDL_LOCAL_APPDATA)+ CRLF;
 Result:= Result+'  COMMON APPDATA: '       + Tab     + GetSpecialFolder(CSIDL_COMMON_APPDATA)+ CRLF;
 Result:= Result+'  COMMON DOCUMENTS: '     + Tab     + GetSpecialFolder(CSIDL_COMMON_DOCUMENTS)+ CRLF;       { All Users\Documents }
 Result:= Result+'  PERSONAL: '             + Tab+Tab + GetSpecialFolder(CSIDL_PERSONAL);
end;



function GenerateWinPathRepEx: string;
begin
 Result:= ' [PATHS2]'+ CRLF;
 Result:= Result+'  DESKTOP: '              + Tab+Tab + GetSpecialFolder(CSIDL_DESKTOP)+ CRLF;
 Result:= Result+'  DESKTOP DIRECTORY: '    + Tab     + GetSpecialFolder(CSIDL_DESKTOPDIRECTORY)+ CRLF;
 Result:= Result+'  COMMON DESKTOP DIR: '   + Tab     + GetSpecialFolder(CSIDL_COMMON_DESKTOPDIRECTORY)+ CRLF;
 Result:= Result+'  PROGRAMS: '             + Tab+Tab + GetSpecialFolder(CSIDL_PROGRAMS)+ CRLF;
 Result:= Result+'  COMMON PROGRAMS: '      + Tab     + GetSpecialFolder(CSIDL_COMMON_PROGRAMS)+ CRLF;
 Result:= Result+'  FONTS:      '           + Tab+Tab + GetSpecialFolder(CSIDL_FONTS)+ CRLF;
 Result:= Result+'  STARTUP: '              + Tab+Tab + GetSpecialFolder(CSIDL_STARTUP)+ CRLF;
 Result:= Result+'  ALTSTARTUP: '           + Tab+Tab + GetSpecialFolder(CSIDL_ALTSTARTUP)+ CRLF;
 Result:= Result+'  COMMON STARTMENU: '     + Tab     + GetSpecialFolder(CSIDL_COMMON_STARTMENU)+ CRLF;
 Result:= Result+'  COMMON STARTUP: '       + Tab     + GetSpecialFolder(CSIDL_COMMON_STARTUP)+ CRLF;
 Result:= Result+'  STARTMENU: '            + Tab+Tab + GetSpecialFolder(CSIDL_STARTMENU)+ CRLF;
 Result:= Result+'  NETWORK: '              + Tab+Tab + GetSpecialFolder(CSIDL_NETWORK);
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
   Result:= cmExecuteProc.ExecuteProc(AppData.SysDir+ 'AtomGarbageCollector.exe', SW_HIDE);
   LastLeakFix:= now;
  end
 else Result:= TRUE;
end;


end.

