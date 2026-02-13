UNIT LightVcl.Common.CpuUsageProcess;

{=============================================================================================================
   2026.01.30
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

   Also see:
      * Get the Percentage of Total CPU Usage: https://stackoverflow.com/questions/33571061/get-the-percentage-of-total-cpu-usage
      * c:\Myprojects\Packages\Third party packages\CpuUsage.pas
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  Winapi.Windows;

CONST
  MIN_MEASUREMENT_INTERVAL= 250; { Minimum elapsed time (ms) before recalculating. Returns cached value otherwise. }

TYPE
  TCpuUsageMonitor = class
  private
    FPID: Cardinal;
    FHandle: THandle;
    FOldUser: Int64;
    FOldKernel: Int64;
    FLastUpdateTime: UInt64;
    FLastUsage: Single;
    function FileTimeToInt64(CONST FT: TFileTime): Int64;
  public
    constructor Create; overload;
    constructor Create(APID: Cardinal); overload;
    destructor Destroy; override;
    function CoreUsage: Single;  { Per-core usage (0..100). One core at max on 4-core CPU returns 100% }
    function CpuUsage: Single;   { Total CPU usage (0..100). One core at max on 4-core CPU returns 25% }
    property PID: Cardinal read FPID;
    property LastUsage: Single read FLastUsage;
  end;


IMPLEMENTATION

USES
  System.SysUtils;


function TCpuUsageMonitor.FileTimeToInt64(CONST FT: TFileTime): Int64;
begin
  Result:= Int64(FT.dwLowDateTime) or (Int64(FT.dwHighDateTime) shl 32);
end;


constructor TCpuUsageMonitor.Create;
begin
  Create(GetCurrentProcessID);
end;


constructor TCpuUsageMonitor.Create(APID: Cardinal);
VAR
  CreationTime, ExitTime, KernelTime, UserTime: TFileTime;
begin
  inherited Create;
  FPID:= APID;
  FLastUsage:= 0;

  FHandle:= OpenProcess(PROCESS_QUERY_INFORMATION, False, FPID);
  if FHandle = 0
  then raise Exception.CreateFmt('Cannot open process %d: %s', [FPID, SysErrorMessage(GetLastError)]);

  if NOT GetProcessTimes(FHandle, CreationTime, ExitTime, KernelTime, UserTime)
  then
    begin
      CloseHandle(FHandle);
      FHandle:= 0;
      raise Exception.CreateFmt('Cannot query process times for PID %d: %s', [FPID, SysErrorMessage(GetLastError)]);
    end;

  FOldKernel:= FileTimeToInt64(KernelTime);
  FOldUser:= FileTimeToInt64(UserTime);
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
  CreationTime, ExitTime, KernelTime, UserTime: TFileTime;
  DeltaMs, ThisTime: UInt64;
  Kernel, User, Delta: Int64;
begin
  Result:= FLastUsage;
  ThisTime:= GetTickCount64;

  DeltaMs:= ThisTime - FLastUpdateTime;
  if DeltaMs < MIN_MEASUREMENT_INTERVAL then EXIT;

  if NOT GetProcessTimes(FHandle, CreationTime, ExitTime, KernelTime, UserTime)
  then EXIT;

  FLastUpdateTime:= ThisTime;
  Kernel:= FileTimeToInt64(KernelTime);
  User:= FileTimeToInt64(UserTime);

  Delta:= (User + Kernel) - (FOldUser + FOldKernel);
  FOldUser:= User;
  FOldKernel:= Kernel;

  { FILETIME units: 100ns intervals. DeltaMs: milliseconds.
    1ms = 10,000 x 100ns. Percentage = Delta / (DeltaMs x 10,000) x 100
    Simplified: Delta / (DeltaMs x 100) }
  Result:= Delta / (DeltaMs * 100);
  FLastUsage:= Result;
end;


function TCpuUsageMonitor.CpuUsage: Single;
begin
  Result:= CoreUsage / CPUCount;
end;


end.
