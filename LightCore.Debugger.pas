UNIT LightCore.Debugger;

{=============================================================================================================
   2025.12
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   Functions to:
      Measure code execution time

      Test if the application is being debugged
      Generate reports about the hardware
      Generate errors (for testing)
      Check compiler options

   This uses System.Diagnostics.TStopwatch which is platform independent.

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
   LightCore, LightCore.Platform, LightCore.IO, LightCore.TextFile, LightCore.AppData;





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
   Writes strings to a Log file that is placed in app's data folder.
   You must use AppLog_Init one time before calling AppLog_Add.
--------------------------------------------------------------------------------------------------}

VAR LogFile: string;

procedure LogFile_Init(FullFileName: string);                                 { Init the log  }
begin
 LogFile:= FullFileName;
 ForceDirectories(ExtractFilePath(FullFileName));

 if FileExists(LogFile)
 then DeleteFile(LogFile);                                               { Clear existing log }

 StringToFile(LogFile, LogFile+ CRLF+ TAppDataCore.AppName+ {' v'+ TAppDataCore.GetVersionInfo+} CRLF+ DateTimeToStr(Now)+ CRLF +LBRK, woAppend);
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

