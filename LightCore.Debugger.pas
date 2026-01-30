UNIT LightCore.Debugger;

{=============================================================================================================
   2026.01.29
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   Debugging and profiling utilities:
     - Code execution timing (using high-resolution TStopwatch)
     - Debugger detection (check if running under Delphi IDE)
     - Compiler optimization status
     - Crash/leak generators for testing error handlers
     - Simple file-based logging

   Platform: Cross-platform via System.Diagnostics.TStopwatch
             (DebugHook is Windows-specific)

   WARNING: Timer functions use global state and are NOT thread-safe.
            For multi-threaded timing, create local TStopwatch instances.

   Tester:
      c:\Projects\LightSaber\Demo\VCL\Demo SystemReport\VCL_Demo_SystemReport.dpr
      c:\Projects\LightSaber\Demo\FMX\Demo SystemReport\FMX_Demo_SystemReport.dpr

   Also see:
      QuickImageFX\Quick.Chrono.pas

=============================================================================================================}

INTERFACE

USES
   System.Diagnostics, System.SysUtils;

 { ANTI-DEBUGGER PROTECTION }
 function  IsRunningUnderDelphiDebugger: Boolean;

 { COMPILER INFO }
 function CompilerOptimization : Boolean;
 function CompilerOptimizationS: String;

 { CRASH ME }
 procedure GenerateCrashNIL;
 procedure GenerateLeak;

 { CODE TIMING }
 procedure TimerStart;                                     { use it with: SetPriorityMax  }
 function  TimerElapsed: Double;                           { In miliseconds }
 function  TimerElapsedS: string;                          { In miliseconds or seconds }

 function  ShowTransferSpeed(FileSize: Cardinal): string;  { Shows the disk/internet speed }

 procedure LogFile_Init  (FullFileName: string);           { Init the log  }
 procedure LogFile_Add   (s: string);                      { Write in Log }

 { Reports }
 function GenerateCompilerReport: string;


IMPLEMENTATION
USES
   LightCore, LightCore.Platform, LightCore.IO, LightCore.TextFile, LightCore.AppData, LightCore.Types;





{--------------------------------------------------------------------------------------------------
   PROTECTION
   Also see c:\MyProjects\Packages\Third party packages\uDebugger.pas
--------------------------------------------------------------------------------------------------}

{ Is the process running as part of Delphi? }
{.$IFDEF msWindows}
 {$WARN SYMBOL_PLATFORM OFF} // prevent W1002 Symbol 'DebugHook' is specific to a platform
function IsRunningUnderDelphiDebugger: Boolean;
begin
  Result := (DebugHook <> 0);
end;
 {$WARN SYMBOL_PLATFORM ON}
{.$ENDIF}





{-------------------------------------------------------------------------------------------------------------
   COMPILER
-------------------------------------------------------------------------------------------------------------}

{ Important note:
   $O+ has LOCAL scope - the result reflects the optimization state at THIS specific location.
   If you use $O+ / $O- switches inline to optimize specific code sections, this function
   must be INLINED at each call site to reflect the local setting.
   If you only use the global switch (Project Options), it works as a normal function. }

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
  sw := TStopWatch.Create;      { Hint: We can use directly: TStopWatch.CreateNew but SilverWarior says there is a bug in it. Maybe this one? https://codeverge.com/embarcadero.delphi.win32/tstopwatch-a-bug-delphi-2010/1046096 }
  sw.Start;
end;


{ Returns the time elapsed, in miliseconds }
function TimerElapsed: Double;
begin
  sw.Stop;
  Result:= sw.ElapsedMilliseconds;    {WARNING!  The value of Elapsed is only updated if the stopwatch is priorly stopped. Reading the Elapsed property while the stopwatch is running does not yield any difference. }
end;



{  Converts elapsed time to human-readable format with appropriate units.
   Returns the most suitable unit based on magnitude:
   - ns (nanoseconds)  for < 1 microsecond
   - us (microseconds) for < 1 millisecond
   - ms (milliseconds) for < 1 second
   - s  (seconds)      for < 1 minute
   - m  (minutes)      for >= 1 minute  }
function TimerElapsedS: string;
VAR NanoSec: Int64;
begin
  sw.Stop;
  NanoSec:= sw.Elapsed.Ticks * 100;     { Elapsed.Ticks is in 100ns increments }

  if NanoSec < NanosPerMicroSec
  then Result:= IntToStr(NanoSec) + 'ns'
  else
    if NanoSec < NanosPerMileSec
    then Result:= Real2Str(NanoSec / NanosPerMicroSec, 3) + 'us'
    else
      if NanoSec < NanosPerSecond
      then Result:= Real2Str(NanoSec / NanosPerMileSec, 3) + 'ms'
      else
        if NanoSec < NanosPerMinute
        then Result:= Real2Str(NanoSec / NanosPerSecond, 3) + 's'
        else Result:= Real2Str(NanoSec / NanosPerMinute, 3) + 'm';  { BUG FIX: was NanoSec / 60*1000000000 (wrong precedence) }
end;


{ Calculates and formats transfer speed (disk/network).
  Call TimerStart before the operation, then pass the transferred size.
  Example:
      TimerStart;
      CopyFile(FileName);
      Caption:= ShowTransferSpeed(GetFileSize(FileName));  }
function ShowTransferSpeed(FileSize: Cardinal): string;
VAR
  ElapsedSec: Double;
begin
  ElapsedSec:= TimerElapsed / 1000;
  if ElapsedSec > 0
  then Result:= 'Speed: ' + FormatBytes(Round(FileSize / ElapsedSec), 1) + '/sec'
  else Result:= 'Speed: UFO (too fast to measure)';
end;






{--------------------------------------------------------------------------------------------------
   LOGGING

   Simple file-based logging. NOT thread-safe (uses global LogFile variable).
   Call LogFile_Init once before using LogFile_Add.

   WARNING: Uses global state. For thread-safe logging, use a proper logging framework
            or create a TLogFile class with instance-based state.
--------------------------------------------------------------------------------------------------}

VAR
  LogFile: string;  { Global: path to the log file. Set by LogFile_Init. }


{ Initializes the log file. Creates parent directories if needed.
  Clears any existing log file and writes a header with app name and timestamp. }
procedure LogFile_Init(FullFileName: string);
begin
  Assert(FullFileName <> '', 'LogFile_Init: FullFileName cannot be empty');

  LogFile:= FullFileName;
  ForceDirectories(ExtractFilePath(FullFileName));

  if FileExists(LogFile)
  then DeleteFile(LogFile);

  StringToFile(LogFile, LogFile + CRLF + TAppDataCore.AppName + CRLF + DateTimeToStr(Now) + CRLF + LBRK, woAppend);
end;


{ Appends a line to the log file. Only writes if the log file exists (was initialized).
  Silently ignores writes if LogFile_Init was not called. }
procedure LogFile_Add(s: string);
begin
  if (LogFile <> '')
  AND FileExists(LogFile)
  then StringToFile(LogFile, s + CRLF, woAppend);
end;




{--------------------------------------------------------------------------------------------------
   CRASH/LEAK GENERATORS (for testing error handlers and memory leak detection)
--------------------------------------------------------------------------------------------------}

{ Intentionally causes an Access Violation by calling a method on a nil reference.
  Use this to test exception handlers, crash reporters, or madExcept/EurekaLog. }
procedure GenerateCrashNIL;
VAR T: TObject;
begin
  T:= NIL;
  T.ClassName;  { Access Violation: calling virtual method on nil }
end;


{ Intentionally creates a memory leak by allocating an object without freeing it.
  Use this to test ReportMemoryLeaksOnShutdown or memory leak detection tools. }
procedure GenerateLeak;
VAR T: TObject;
begin
  T:= TObject.Create;
  T.ToString;  { Reference T to prevent compiler hint, but never free it }
end;




{--------------------------------------------------------------------------------------------------
   REPORTS
--------------------------------------------------------------------------------------------------}
function GenerateCompilerReport: string;
begin
 Result:= ' [COMPILER]'+ CRLF;
 Result:= Result+'  RunningUnderDelphi: '  + BoolToStrYesNo(IsRunningUnderDelphiDebugger)+ CRLF;
 Result:= Result+'  AppBitnessEx: '        + Tab + AppBitnessEx+ CRLF;
 Result:= Result+'  CompilerOptim: '       + Tab + BoolToStrYesNo(CompilerOptimization)+ CRLF;
end;


end.

