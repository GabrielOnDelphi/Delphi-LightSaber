UNIT LightVcl.Common.CpuMonitor;

{-------------------------------------------------------------------------------------------------------------
  CPU usage monitor and power-aware wallpaper change gating.

  TCpuMonitor:
    Lightweight record using GetSystemTimes to measure total system CPU usage (0..100).
    No dependencies beyond Winapi.Windows.

  TPowerSettings:
    Class that owns a TCpuMonitor + TTimer. 
    Automatically samples CPU every 2 seconds.
    CpuIsBusy returns TRUE when the system is too busy (or on battery).

    Requirements: Windows 10+ (GetSystemTimes guaranteed).
--------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.Classes, Vcl.ExtCtrls;

CONST
  CPU_HISTORY_SIZE = 10;     { Rolling window size for Average. At 2-second intervals this gives a 20-second window. }

TYPE
  TCpuMonitor = record
  private
    FPrevIdle: Int64;
    FPrevTotal: Int64;
    FHistory: array[0..CPU_HISTORY_SIZE-1] of Integer;
    FCount: Integer;         { Number of samples collected so far (0..CPU_HISTORY_SIZE) }
    FIndex: Integer;         { Next write position in ring buffer }
  public
    function Usage: Integer;     { Collect a sample and return current CPU usage (0..100). Returns -1 on API error. }
    function Average: Integer;   { Rolling average of last CPU_HISTORY_SIZE samples (0..100). Returns 0 if no data. }
  end;


  TPowerSettings = class
  private
    FTimer: TTimer;
    FCpuMon: TCpuMonitor;
    procedure OnTimer(Sender: TObject);
  public
    CheckBatteries   : Boolean;  { Prevent wallpaper changes when on battery }
    NotifyPowerChange: Boolean;  { Show power change notifications }
    HighCpuThreshold : Integer;  { Maximum CPU usage threshold (percentage). Wallpaper changes blocked above this. }
    LastCpuUsage     : Integer;  { Updated automatically every 2 seconds by the internal timer }

    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    function CpuIsBusy: Boolean; { Returns TRUE when CPU >= HighCpuThreshold or on battery. Meaning: wallpaper change should be skipped. }
  end;


IMPLEMENTATION

USES
  System.SysUtils, Winapi.Windows, LightVcl.Common.PowerUtils;

{ Ensure GetSystemTimes is available. Declared in Winapi.Windows since at least XE2,
  but we provide a fallback declaration for older or incomplete headers.
  The API itself is available since Windows XP SP1 - guaranteed on Windows 10+. }
{$IF NOT DECLARED(GetSystemTimes)}
function GetSystemTimes(var lpIdleTime, lpKernelTime, lpUserTime: TFileTime): BOOL; stdcall; external kernel32 name 'GetSystemTimes';
{$IFEND}


function FileTimeToI64(const FT: TFileTime): Int64;
begin
  Result:= Int64(FT.dwHighDateTime) shl 32 + Int64(FT.dwLowDateTime);
end;



{-------------------------------------------------------------------------------------------------------------
   TCpuMonitor
--------------------------------------------------------------------------------------------------------------}

function TCpuMonitor.Usage: Integer;
var
  IdleFT, KernelFT, UserFT: TFileTime;
  Idle, Total: Int64;
  IdleDelta, TotalDelta: Int64;
begin
  if NOT GetSystemTimes(IdleFT, KernelFT, UserFT)
  then EXIT(-1);

  { KernelTime includes idle time (per MSDN).
    Total = Kernel + User covers all CPU time across all cores. }
  Idle:= FileTimeToI64(IdleFT);
  Total:= FileTimeToI64(KernelFT) + FileTimeToI64(UserFT);

  { First call: store baseline, return 0 }
  if FPrevTotal = 0 then
    begin
    FPrevIdle:= Idle;
    FPrevTotal:= Total;
    EXIT(0);
    end;

  IdleDelta:= Idle - FPrevIdle;
  TotalDelta:= Total - FPrevTotal;

  FPrevIdle:= Idle;
  FPrevTotal:= Total;

  if TotalDelta <= 0
  then EXIT(0);

  { CPU% = (busy time / total time) * 100
    Busy time = total - idle }
  Result:= Round((TotalDelta - IdleDelta) * 100.0 / TotalDelta);

  if Result < 0   then Result:= 0;
  if Result > 100  then Result:= 100;

  { Store in ring buffer for Average }
  FHistory[FIndex]:= Result;
  FIndex:= (FIndex + 1) mod CPU_HISTORY_SIZE;
  if FCount < CPU_HISTORY_SIZE
  then Inc(FCount);
end;


function TCpuMonitor.Average: Integer;
var
  i, Sum: Integer;
begin
  if FCount = 0
  then EXIT(0);

  Sum:= 0;
  for i:= 0 to FCount - 1 do
    Sum:= Sum + FHistory[i];

  Result:= Sum div FCount;
end;



{-------------------------------------------------------------------------------------------------------------
   TPowerSettings
--------------------------------------------------------------------------------------------------------------}

constructor TPowerSettings.Create;
begin
  inherited Create;
  Reset;

  FTimer:= TTimer.Create(NIL);
  FTimer.Interval:= 2000;  { Sample CPU every 2 seconds }
  FTimer.OnTimer:= OnTimer;
  FTimer.Enabled:= TRUE;
end;


destructor TPowerSettings.Destroy;
begin
  FreeAndNil(FTimer);
  inherited Destroy;
end;


procedure TPowerSettings.Reset;
begin
  CheckBatteries   := TRUE;
  NotifyPowerChange:= TRUE;
  HighCpuThreshold := 80;
  LastCpuUsage     := 0;
end;


procedure TPowerSettings.OnTimer(Sender: TObject);
var CpuUsage: Integer;
begin
  CpuUsage:= FCpuMon.Usage;
  if CpuUsage >= 0
  then LastCpuUsage:= CpuUsage;
end;


{ Returns TRUE when it is NOT appropriate to change wallpaper:
    - CPU usage is at or above the configured HighCpuThreshold threshold
    - Computer is running on batteries (when battery check is enabled) }
function TPowerSettings.CpuIsBusy: Boolean;
begin
  if LastCpuUsage >= HighCpuThreshold
  then EXIT(TRUE);

  if CheckBatteries AND (PowerStatus <= pwTypeBat)
  then EXIT(TRUE);

  Result:= FALSE;
end;


END.
