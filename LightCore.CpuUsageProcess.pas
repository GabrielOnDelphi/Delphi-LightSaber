UNIT LightCore.CpuUsageProcess;

{=============================================================================================================
   2026.11.02
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Returns CPU/CORE usage for the specified process.

   Usage:
      var Monitor: TCpuUsageMonitor;
      Monitor:= TCpuUsageMonitor.Create;                // monitors current process  [OR]  Create(SomePID)
      try
        Usage:= Monitor.CoreUsage;   // per-core percentage (one core at 100% = 100%)
        Usage:= Monitor.CpuUsage;    // total CPU percentage (one core at 100% on 4-core = 25%)
      finally
        FreeAndNil(Monitor);
      end;

   How it measures:
      CoreUsage = the CPU cycles the process used in the interval (QueryProcessCycleTime), divided by the ticks of the processor's time-stamp counter (RDTSC) in the same interval.
      Windows reads these cycles from the CPU "using mechanisms like RDTSC" (https://devblogs.microsoft.com/oldnewthing/20161021-00/?p=94565), so both numbers count the same clock and no CPU frequency is needed.
      Microsoft warns not to convert the cycles to elapsed time, because the rate of the counter differs between CPUs (https://learn.microsoft.com/en-us/windows/win32/api/realtimeapiset/nf-realtimeapiset-querythreadcycletime). This unit never converts them: it divides one count by another count of the same counter.
      The busy-loop tests in Test.LightCore.CpuUsageProcess check that one busy thread reads close to 100.
      [UNVERIFIED] on Windows on ARM, where an x86/x64 program runs emulated.

   Why not GetProcessTimes:
      It counts CPU time in whole clock ticks (Gary Nebbett's ETW analysis: https://learn.microsoft.com/en-us/answers/questions/1183193/totalprocessortime-differs-between-windows-10-and), and on Windows 11 the ticks land in lumps.
      Measured on Windows 11 with a 1 ms clock tick: a VCL program that draws frames on a TTimer used 11-12% of one core in every second (QueryProcessCycleTime),
      while GetProcessTimes read 0% for 13 seconds in a row, then 30-43%. Its 30-second average was 6.8%, so a longer average does not repair it. Minimized, the same.
      Test_CoreUsage_ShortBursts_MatchesSpinTime reproduces it: GetProcessTimes read 0 for a thread that was busy 12.9% of the time.

   Also see:
      * Get the Percentage of Total CPU Usage: https://stackoverflow.com/questions/33571061/get-the-percentage-of-total-cpu-usage
      * E:\Backups\My projects\2020\2020.04.15 BioniX 11.45 Freeze!!!!\Third party packages\CpuUsage.pas

   Every member of this class either calls Windows or reads a field that a Windows member wrote, and CoreUsage and CpuUsage return a measured value, so the whole class is declared inside a MSWINDOWS conditional - interface and implementation both. Off Windows a CoreUsage of 0 would read as "this process is idle", which is a silent wrong answer, so a call written in an Android, macOS or iOS build fails to compile instead, and this unit compiles empty there.
-------------------------------------------------------------------------------------------------------------}

INTERFACE
{$IFDEF MSWINDOWS}

USES
  Winapi.Windows;

CONST
  MIN_MEASUREMENT_INTERVAL= 250; { Minimum elapsed time (ms) before recalculating. Returns cached value otherwise. }

TYPE
  TCpuUsageMonitor = class
  private
    FPID: Cardinal;
    FHandle: THandle;
    FOldCycles: UInt64;     { QueryProcessCycleTime at the last measurement }
    FOldStamp: UInt64;      { ReadTimeStampCounter at the last measurement }
    FLastUpdateTime: UInt64;
    FLastUsage: Single;
  public
    constructor Create; overload;
    constructor Create(APID: Cardinal); overload;
    destructor Destroy; override;
    function CoreUsage: Single;  { Per-core usage (0..100). One core at max on 4-core CPU returns 100% }
    function CpuUsage: Single;   { Total CPU usage (0..100). One core at max on 4-core CPU returns 25% }
    property PID: Cardinal read FPID;
    property LastUsage: Single read FLastUsage;
  end;

function ReadTimeStampCounter: UInt64;  { The processor's time-stamp counter (RDTSC): the clock QueryProcessCycleTime counts in }

{$ENDIF}


IMPLEMENTATION
{$IFDEF MSWINDOWS}

USES
  System.SysUtils;


{ Reads the processor's time-stamp counter. Whole-routine asm functions: Win64 does not allow asm inside a Pascal body. }
{$IFDEF CPUX64}
function ReadTimeStampCounter: UInt64; assembler;
asm
  RDTSC                  { The counter comes back split across EDX:EAX }
  SHL   RDX, 32
  OR    RAX, RDX
end;
{$ELSE}
function ReadTimeStampCounter: UInt64; assembler;
asm
  RDTSC                  { EDX:EAX, which is already where Win32 returns a 64-bit result }
end;
{$ENDIF}


constructor TCpuUsageMonitor.Create;
begin
  Create(GetCurrentProcessID);
end;


constructor TCpuUsageMonitor.Create(APID: Cardinal);
begin
  inherited Create;
  FPID:= APID;
  FLastUsage:= 0;

  FHandle:= OpenProcess(PROCESS_QUERY_INFORMATION, False, FPID);
  if FHandle = 0
  then raise Exception.CreateFmt('Cannot open process %d: %s', [FPID, SysErrorMessage(GetLastError)]);

  if NOT QueryProcessCycleTime(FHandle, FOldCycles)
  then
    begin
      CloseHandle(FHandle);
      FHandle:= 0;
      raise Exception.CreateFmt('Cannot query the cycle time of PID %d: %s', [FPID, SysErrorMessage(GetLastError)]);
    end;

  FOldStamp:= ReadTimeStampCounter;
  FLastUpdateTime:= GetTickCount64;
end;


destructor TCpuUsageMonitor.Destroy;
begin
  if FHandle <> 0
  then CloseHandle(FHandle);
  inherited;
end;


function TCpuUsageMonitor.CoreUsage: Single;
VAR
  ThisTime, Cycles, Stamp: UInt64;
begin
  Result:= FLastUsage;
  ThisTime:= GetTickCount64;
  if ThisTime - FLastUpdateTime < MIN_MEASUREMENT_INTERVAL then EXIT;

  if NOT QueryProcessCycleTime(FHandle, Cycles)
  then EXIT;
  Stamp:= ReadTimeStampCounter;

  { If either count went backwards (a time-stamp counter that differs between cores), the unsigned subtractions below would wrap. Start a new interval instead. }
  if (Cycles >= FOldCycles) AND (Stamp > FOldStamp) then
    begin
      Result:= (Cycles - FOldCycles) / (Stamp - FOldStamp) * 100;
      FLastUsage:= Result;
    end;

  FOldCycles:= Cycles;
  FOldStamp:= Stamp;
  FLastUpdateTime:= ThisTime;
end;


function TCpuUsageMonitor.CpuUsage: Single;
begin
  Result:= CoreUsage / CPUCount;
end;

{$ENDIF}

end.
