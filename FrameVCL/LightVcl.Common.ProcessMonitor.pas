UNIT LightVcl.Common.ProcessMonitor;

{=============================================================================================================
   2026.02.28
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Enumerates all running processes and returns the top-N by CPU usage and RAM consumption.
   Designed for overlay display (mini Task Manager on the desktop).

   CPU% is smoothed via exponential moving average (EMA) to prevent the process list
   from jumping around between updates. The smoothing factor (ALPHA) controls how fast
   the display reacts: lower = slower/smoother, higher = faster/jumpier.

   Usage:
      var Mon: TProcessListMonitor;
      Mon:= TProcessListMonitor.Create;
      try
        Mon.Sample;   // First call establishes baseline (CPU deltas need two samples)
        Sleep(1500);
        Mon.Sample;   // Now TopCpu is populated

        for i:= 0 to Mon.TopCpuCount-1 do
          WriteLn(Mon.TopCpu[i].Name, ' ', Mon.TopCpu[i].CpuPercent:5:1, '%');

        for i:= 0 to Mon.TopRamCount-1 do
          WriteLn(Mon.TopRam[i].Name, ' ', Mon.TopRam[i].WorkingSetBytes);
      finally
        FreeAndNil(Mon);
      end;

   Notes:
     - Processes with the same exe name are aggregated (all chrome.exe instances summed)
     - PID reuse is detected by name change (delta reset to 0)
     - Protected/system processes that cannot be opened are silently skipped
     - First Sample() call populates RAM but not CPU (needs two samples for delta)
     - CPU% values are smoothed over time; processes not seen for several samples fade out

   Also see:
     * LightVcl.Common.CpuUsageProcess.pas - per-process CPU monitor (single process)
     * LightVcl.Common.CpuMonitor.pas      - system-wide CPU monitor
     * LightVcl.Common.Process.pas          - process enumeration utilities
=============================================================================================================}

INTERFACE

USES
  Winapi.Windows;

CONST
  TOP_N = 5;

  { EMA smoothing factor: 0.25 means 25% weight on new sample, 75% on history.
    At 1.5s sample interval this gives ~6 second effective averaging window.
    Lower value = smoother/slower, higher = faster/jumpier. }
  ALPHA = 0.25;

  { Processes with smoothed CPU% below this threshold are pruned from the smoothing table }
  PRUNE_THRESHOLD = 0.01;

TYPE
  RProcessInfo = record
    Name: string;           { Process name without .exe }
    CpuPercent: Single;     { 0..100, normalized to total CPU capacity (all cores) }
    WorkingSetBytes: Int64;
  end;

  RPidSample = record
    PID: DWORD;
    Name: string;
    KernelPlusUser: Int64;
  end;

  TPidSamples = array of RPidSample;

  TProcessListMonitor = class
  private
    type
      RAggregated = record
        Name: string;
        CpuPercent: Single;
        WorkingSetBytes: Int64;
      end;

      { Smoothed CPU% per process name, persists across Sample() calls }
      RSmoothed = record
        Name: string;
        SmoothedCpu: Single;
      end;

    var
      FPrevSamples: TPidSamples;
      FPrevSampleCount: Integer;
      FPrevTickCount: UInt64;
      FTopCpu: array[0..TOP_N-1] of RProcessInfo;
      FTopRam: array[0..TOP_N-1] of RProcessInfo;
      FTopCpuCount: Integer;  { 0..TOP_N, actual entries filled }
      FTopRamCount: Integer;

      { EMA smoothing table for CPU% }
      FSmoothed: array of RSmoothed;
      FSmoothedCount: Integer;

    function FindPrevSample(PID: DWORD; CONST Name: string): Integer;
    function FindSmoothed(CONST Name: string): Integer;
    procedure UpdateSmoothed(CONST Name: string; RawCpu: Single);
    procedure DecayUnseen(CONST Agg: array of RAggregated; AggCount: Integer);
    procedure PruneSmoothed;
  public
    ExcludeName: string;  { Process name to hide from results (e.g. 'MyApp.exe'). Case-insensitive. Compared with and without .exe extension. }
    procedure Sample;
    function GetTopCpu(Index: Integer): RProcessInfo;
    function GetTopRam(Index: Integer): RProcessInfo;
    property TopCpuCount: Integer read FTopCpuCount;
    property TopRamCount: Integer read FTopRamCount;
  end;


IMPLEMENTATION

USES
  System.SysUtils, System.Math, TlHelp32, Winapi.PsAPI;

CONST
  { Vista+ constant. Declared here in case Winapi.Windows header is missing it. }
  PROCESS_QUERY_LIMITED_INFORMATION_FLAG = $1000;


function FileTimeToI64(CONST FT: TFileTime): Int64;
begin
  Result:= Int64(FT.dwHighDateTime) shl 32 + Int64(FT.dwLowDateTime);
end;


{ Look up a PID in the previous sample array.
  Returns index or -1 if not found or if name changed (PID reuse). }
function TProcessListMonitor.FindPrevSample(PID: DWORD; CONST Name: string): Integer;
VAR i: Integer;
begin
  for i:= 0 to FPrevSampleCount - 1 do
    if (FPrevSamples[i].PID = PID) AND SameText(FPrevSamples[i].Name, Name)
    then EXIT(i);
  Result:= -1;
end;


{ Find a process name in the smoothing table. Returns index or -1. }
function TProcessListMonitor.FindSmoothed(CONST Name: string): Integer;
VAR i: Integer;
begin
  for i:= 0 to FSmoothedCount - 1 do
    if SameText(FSmoothed[i].Name, Name)
    then EXIT(i);
  Result:= -1;
end;


{ Update or insert an EMA-smoothed CPU% entry for the given process name. }
procedure TProcessListMonitor.UpdateSmoothed(CONST Name: string; RawCpu: Single);
VAR Idx: Integer;
begin
  Idx:= FindSmoothed(Name);
  if Idx >= 0 then
    { Blend new value into existing average }
    FSmoothed[Idx].SmoothedCpu:= FSmoothed[Idx].SmoothedCpu * (1 - ALPHA) + RawCpu * ALPHA
  else
    begin
      { New entry: start with raw value }
      if FSmoothedCount >= Length(FSmoothed)
      then SetLength(FSmoothed, FSmoothedCount + 32);
      FSmoothed[FSmoothedCount].Name:= Name;
      FSmoothed[FSmoothedCount].SmoothedCpu:= RawCpu;
      Inc(FSmoothedCount);
    end;
end;


{ Decay smoothed values for processes NOT seen in the current aggregation.
  This makes processes that stopped using CPU fade out gracefully. }
procedure TProcessListMonitor.DecayUnseen(CONST Agg: array of RAggregated; AggCount: Integer);
VAR i, j: Integer;
    Found: Boolean;
begin
  for i:= 0 to FSmoothedCount - 1 do
    begin
      Found:= FALSE;
      for j:= 0 to AggCount - 1 do
        if SameText(FSmoothed[i].Name, Agg[j].Name) then
          begin
            Found:= TRUE;
            Break;
          end;

      if NOT Found then
        { Process not running or has 0 CPU: decay toward zero }
        FSmoothed[i].SmoothedCpu:= FSmoothed[i].SmoothedCpu * (1 - ALPHA);
    end;
end;


{ Remove entries with negligible smoothed CPU% to prevent unbounded table growth. }
procedure TProcessListMonitor.PruneSmoothed;
VAR i: Integer;
begin
  i:= 0;
  while i < FSmoothedCount do
    begin
      if FSmoothed[i].SmoothedCpu < PRUNE_THRESHOLD then
        begin
          { Move last entry into this slot }
          FSmoothed[i]:= FSmoothed[FSmoothedCount - 1];
          Dec(FSmoothedCount);
        end
      else
        Inc(i);
    end;
end;


{ Takes a snapshot of all running processes and computes top-N by CPU and RAM.
  Algorithm:
    1. Capture wall clock via GetTickCount64, compute DeltaMs from previous call
    2. Enumerate all processes via CreateToolhelp32Snapshot
    3. For each process: OpenProcess -> GetProcessTimes (CPU) + GetProcessMemoryInfo (RAM)
    4. CPU% is computed as delta of kernel+user time since previous sample,
       normalized to total CPU capacity: Delta / (DeltaMs * 100 * NumCPUs)
    5. Aggregate all instances of the same exe name (sum CPU% and RAM)
    6. Smooth CPU% via exponential moving average (EMA) for stable rankings
    7. Partial selection sort to extract top-5 by smoothed CPU and top-5 by RAM
    8. Store current PID times for the next delta computation }
procedure TProcessListMonitor.Sample;
VAR
  SnapHandle: THandle;
  ProcEntry: TProcessEntry32;
  ProcHandle: THandle;
  CreationTime, ExitTime, KernelTime, UserTime: TFileTime;
  MemCounters: TProcessMemoryCounters;
  ThisTick, DeltaMs: UInt64;
  KernelPlusUser, Delta: Int64;
  CpuPct: Single;
  NumCPUs: Integer;
  HasPrev: Boolean;
  PrevIdx, SmIdx: Integer;

  { Current sample storage }
  CurSamples: TPidSamples;
  CurCount: Integer;

  { Aggregation storage }
  Agg: array of RAggregated;
  AggCount: Integer;

  function StripExe(CONST AName: string): string;
  VAR Len: Integer;
  begin
    Result:= AName;
    Len:= Length(Result);
    if (Len > 4) AND SameText(Copy(Result, Len - 3, 4), '.exe')
    then Result:= Copy(Result, 1, Len - 4);
  end;

  procedure AddToAggregation(CONST AName: string; ACpuPct: Single; AWorkingSet: Int64);
  VAR j: Integer;
  begin
    { Skip excluded process }
    if (ExcludeName <> '') AND (SameText(AName, ExcludeName) OR SameText(StripExe(AName), ExcludeName))
    then EXIT;

    { Search for existing entry with same name }
    for j:= 0 to AggCount - 1 do
      if SameText(Agg[j].Name, AName) then
        begin
          Agg[j].CpuPercent:= Agg[j].CpuPercent + ACpuPct;
          Agg[j].WorkingSetBytes:= Agg[j].WorkingSetBytes + AWorkingSet;
          EXIT;
        end;

    { New entry }
    if AggCount >= Length(Agg)
    then SetLength(Agg, AggCount + 64);
    Agg[AggCount].Name:= AName;
    Agg[AggCount].CpuPercent:= ACpuPct;
    Agg[AggCount].WorkingSetBytes:= AWorkingSet;
    Inc(AggCount);
  end;

  procedure SortBySmoothedCpu;
  VAR i, j, Best: Integer;
      Tmp: RAggregated;
  begin
    { Replace raw CPU% with smoothed values before sorting }
    for i:= 0 to AggCount - 1 do
      begin
        SmIdx:= FindSmoothed(Agg[i].Name);
        if SmIdx >= 0
        then Agg[i].CpuPercent:= FSmoothed[SmIdx].SmoothedCpu;
      end;

    { Partial selection sort - only need top TOP_N }
    for i:= 0 to Min(TOP_N, AggCount) - 1 do
      begin
        Best:= i;
        for j:= i + 1 to AggCount - 1 do
          if Agg[j].CpuPercent > Agg[Best].CpuPercent
          then Best:= j;
        if Best <> i then
          begin
            Tmp:= Agg[i];
            Agg[i]:= Agg[Best];
            Agg[Best]:= Tmp;
          end;
      end;
  end;

  procedure SortByRam;
  VAR i, j, Best: Integer;
      Tmp: RAggregated;
  begin
    for i:= 0 to Min(TOP_N, AggCount) - 1 do
      begin
        Best:= i;
        for j:= i + 1 to AggCount - 1 do
          if Agg[j].WorkingSetBytes > Agg[Best].WorkingSetBytes
          then Best:= j;
        if Best <> i then
          begin
            Tmp:= Agg[i];
            Agg[i]:= Agg[Best];
            Agg[Best]:= Tmp;
          end;
      end;
  end;

VAR
  i, Count: Integer;
begin
  ThisTick:= GetTickCount64;
  DeltaMs:= ThisTick - FPrevTickCount;
  HasPrev:= (FPrevTickCount > 0) AND (DeltaMs > 0);
  NumCPUs:= CPUCount;

  SnapHandle:= CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if SnapHandle = INVALID_HANDLE_VALUE then EXIT;

  CurCount:= 0;
  AggCount:= 0;
  SetLength(CurSamples, 256);
  SetLength(Agg, 128);

  TRY
    ProcEntry.dwSize:= SizeOf(ProcEntry);
    if NOT Process32First(SnapHandle, ProcEntry) then EXIT;

    REPEAT
      { Skip System Idle Process }
      if ProcEntry.th32ProcessID = 0 then Continue;

      ProcHandle:= OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION_FLAG, False, ProcEntry.th32ProcessID);
      if ProcHandle = 0 then Continue;

      TRY
        KernelPlusUser:= 0;
        CpuPct:= 0;

        if GetProcessTimes(ProcHandle, CreationTime, ExitTime, KernelTime, UserTime)
        then KernelPlusUser:= FileTimeToI64(KernelTime) + FileTimeToI64(UserTime);

        { Compute CPU delta if we have a previous sample }
        if HasPrev AND (KernelPlusUser > 0) then
          begin
            PrevIdx:= FindPrevSample(ProcEntry.th32ProcessID, ProcEntry.szExeFile);
            if PrevIdx >= 0 then
              begin
                Delta:= KernelPlusUser - FPrevSamples[PrevIdx].KernelPlusUser;
                if Delta > 0
                then CpuPct:= Delta / (Int64(DeltaMs) * 100 * NumCPUs);
                { Clamp to reasonable range }
                if CpuPct > 100 then CpuPct:= 100;
                if CpuPct < 0   then CpuPct:= 0;
              end;
          end;

        { Query memory }
        FillChar(MemCounters, SizeOf(MemCounters), 0);
        MemCounters.cb:= SizeOf(MemCounters);
        GetProcessMemoryInfo(ProcHandle, @MemCounters, SizeOf(MemCounters));

        { Store current sample for next delta computation }
        if CurCount >= Length(CurSamples)
        then SetLength(CurSamples, CurCount + 128);
        CurSamples[CurCount].PID:= ProcEntry.th32ProcessID;
        CurSamples[CurCount].Name:= ProcEntry.szExeFile;
        CurSamples[CurCount].KernelPlusUser:= KernelPlusUser;
        Inc(CurCount);

        { Aggregate by process name }
        AddToAggregation(ProcEntry.szExeFile, CpuPct, Int64(MemCounters.WorkingSetSize));
      FINALLY
        CloseHandle(ProcHandle);
      END;
    UNTIL NOT Process32Next(SnapHandle, ProcEntry);
  FINALLY
    CloseHandle(SnapHandle);
  END;

  { Store samples for next call }
  FPrevTickCount:= ThisTick;
  SetLength(CurSamples, CurCount);
  FPrevSamples:= CurSamples;
  FPrevSampleCount:= CurCount;

  { Update EMA smoothing for all aggregated processes }
  if HasPrev then
    begin
      for i:= 0 to AggCount - 1 do
        UpdateSmoothed(Agg[i].Name, Agg[i].CpuPercent);
      DecayUnseen(Agg, AggCount);
      PruneSmoothed;
    end;

  { Fill TopCpu using smoothed CPU% values }
  if HasPrev then
    begin
      SortBySmoothedCpu;
      Count:= Min(TOP_N, AggCount);
      FTopCpuCount:= 0;
      for i:= 0 to Count - 1 do
        if Agg[i].CpuPercent > 0 then
          begin
            FTopCpu[FTopCpuCount].Name:= StripExe(Agg[i].Name);
            FTopCpu[FTopCpuCount].CpuPercent:= Agg[i].CpuPercent;
            FTopCpu[FTopCpuCount].WorkingSetBytes:= Agg[i].WorkingSetBytes;
            Inc(FTopCpuCount);
          end;
    end
  else
    FTopCpuCount:= 0;  { First sample - no delta available }

  { Fill TopRam (RAM is instant, no smoothing needed) }
  SortByRam;
  Count:= Min(TOP_N, AggCount);
  FTopRamCount:= 0;
  for i:= 0 to Count - 1 do
    if Agg[i].WorkingSetBytes > 0 then
      begin
        FTopRam[FTopRamCount].Name:= StripExe(Agg[i].Name);
        FTopRam[FTopRamCount].CpuPercent:= Agg[i].CpuPercent;
        FTopRam[FTopRamCount].WorkingSetBytes:= Agg[i].WorkingSetBytes;
        Inc(FTopRamCount);
      end;
end;


function TProcessListMonitor.GetTopCpu(Index: Integer): RProcessInfo;
begin
  Assert((Index >= 0) AND (Index < FTopCpuCount), 'TopCpu index out of range');
  Result:= FTopCpu[Index];
end;


function TProcessListMonitor.GetTopRam(Index: Integer): RProcessInfo;
begin
  Assert((Index >= 0) AND (Index < FTopRamCount), 'TopRam index out of range');
  Result:= FTopRam[Index];
end;


end.
