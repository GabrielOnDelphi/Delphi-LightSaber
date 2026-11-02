UNIT LightCore.Win.FrameTimer;

{$IFNDEF MSWINDOWS}
  {$MESSAGE FATAL 'LightCore.Win.FrameTimer is Windows-only. Its package LightCore.Win builds for Win32 and Win64 only.'}
{$ENDIF}

{=============================================================================================================
   2026.11.02
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   TFrameTimer - a drop-in replacement for the VCL TTimer, for animations.

   A TTimer fires only on the 15.625 ms Windows clock tick, so "30 fps" gives 21 fps and "60 fps" gives 36 fps.
   TFrameTimer runs one worker thread that waits on a high-resolution waitable timer and posts one message per frame
   to a hidden window, so OnTimer still runs on the main thread, at the exact rate.
   Measured (BioniX frame-timing tester, row c): 30.0 fps, standard deviation 0.23 ms, also with the window minimized.
   Design and the reason for each rule: C:\Projects\BioniX\Projects\Tester - Best timing method\Frame timing - findings.md,
   sections "The design" and "Counter-analysis".

   Same as TTimer:
     No frames while Enabled = FALSE, Interval = 0 or OnTimer = NIL.
     The first frame comes one interval after the timer becomes active.
     An exception raised by OnTimer goes to System.Classes.ApplicationHandleException (the VCL points it at
     Application.HandleException); when that is NIL, it is re-raised.
     Free from inside its own OnTimer is safe.
     The interval is never shorter than 10 ms (USER_TIMER_MINIMUM of SetTimer).
   Different from TTimer:
     A new Interval while the timer runs counts from the due time of the running frame. Enabled:= TRUE restarts from now.
     OnTimer is never entered twice, also not from a modal loop that OnTimer runs.
     When the main thread is busy, frames are dropped (counted in Skipped), never queued.

   Create it in code. It is not registered on the component palette.
=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, System.SyncObjs, System.Math, System.Diagnostics,
  LightCore.FramePacer;

TYPE
  EFrameTimerError = class(Exception);

  { Shared between the main thread, the worker thread and the window procedure.
    Outlives the TFrameTimer when the timer is freed inside its own OnTimer }
  TFrameTimerState = class
  private
    Request  : Int64;     { (Generation SHL GenShift) OR interval in 100 ns steps. Interval 0 = inactive. Written by the main thread }
    Pending  : Integer;   { 1 while a WM_FRAME is queued or OnTimer runs }
    Skipped  : Integer;
    InOnTimer: Boolean;   { Main thread only }
    OwnerFreed: Boolean;  { Main thread only }
    ErrorText: string;    { Written by the worker before it posts WM_FRAME_FAILED and ends }
  end;

  TFrameTimer = class(TComponent)
  private
    FState      : TFrameTimerState;
    FWnd        : HWND;
    FThread     : TThread;
    FStopEvent  : THandle;
    FWakeEvent  : THandle;
    FWaitTimer  : THandle;
    FEnabled    : Boolean;
    FTicks      : Int64;          { Requested interval in 100 ns steps, as set by the user (before the 10 ms floor). 0 = no frames }
    FActive     : Boolean;
    FGeneration : Int64;
    FOnTimer    : TNotifyEvent;
    procedure WndProc(var Msg: TMessage);
    procedure UpdateTimer;
    procedure StartWorker;
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    function  GetInterval: Cardinal;
    procedure SetFramesPerSecond(Value: Double);
    function  GetFramesPerSecond: Double;
    procedure SetOnTimer(Value: TNotifyEvent);
    function  GetSkipped: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property FramesPerSecond: Double read GetFramesPerSecond write SetFramesPerSecond;  { Exact rate, interval in 100 ns steps. 0 = no frames; < 0, NaN or infinite raises }
    property Skipped: Integer read GetSkipped;                                         { Frames dropped because the previous one was still pending }
  published
    property Enabled: Boolean read FEnabled write SetEnabled default TRUE;
    property Interval: Cardinal read GetInterval write SetInterval default 1000;      { Milliseconds. 0 = no frames }
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;


IMPLEMENTATION

USES
  LightCore.AppData;

CONST
  WM_FRAME        = WM_APP + 1;
  WM_FRAME_FAILED = WM_APP + 2;

  TicksPerMs   = 10000;                  { 100 ns steps per millisecond }
  MinTicks     = 10 * TicksPerMs;        { 10 ms = USER_TIMER_MINIMUM: the floor SetTimer applies to a TTimer }
  GenShift     = 47;                     { Interval bits: Cardinal ms * 10000 < 2^46 }
  TicksMask    = (Int64(1) SHL GenShift) - 1;
  GenMask      = $FFFF;                  { Only "changed or not" matters, so the generation may wrap }

  CREATE_WAITABLE_TIMER_HIGH_RESOLUTION = $00000002;  { CreateWaitableTimerExW, Windows 10 version 1803 and later. Not in Winapi.Windows }
  TIMER_ALL_ACCESS = $001F0003;                       { Not in Winapi.Windows; same value as c:\Delphi\Delphi 13\source\fmx\FMX.DisplayLink.Default.pas }


function NowMs: Double;
begin
  Result:= TStopwatch.GetTimeStamp * 1000.0 / TStopwatch.Frequency;
end;




{-------------------------------------------------------------------------------------------------------------
   WORKER THREAD
   Never waits for the main thread: no Synchronize, no Queue.
-------------------------------------------------------------------------------------------------------------}
TYPE
  TFrameWorker = class(TThread)
  private
    FState     : TFrameTimerState;
    FWnd       : HWND;
    FStopEvent : THandle;
    FWakeEvent : THandle;
    FWaitTimer : THandle;
    procedure Run;
    procedure Fail(CONST Msg: string);
  protected
    procedure Execute; override;
  public
    constructor Create(aState: TFrameTimerState; aWnd: HWND; aStopEvent, aWakeEvent, aWaitTimer: THandle);
  end;


constructor TFrameWorker.Create(aState: TFrameTimerState; aWnd: HWND; aStopEvent, aWakeEvent, aWaitTimer: THandle);
begin
  FState    := aState;
  FWnd      := aWnd;
  FStopEvent:= aStopEvent;
  FWakeEvent:= aWakeEvent;
  FWaitTimer:= aWaitTimer;
  FreeOnTerminate:= FALSE;
  inherited Create(FALSE);
end;


procedure TFrameWorker.Execute;
begin
  { An exception that leaves Execute is kept silently in FatalException, so catch it here and report it on the main thread }
  TRY
    Run;
  EXCEPT
    on E: Exception do
      Fail(E.ClassName + ': ' + E.Message);
  END;
end;


procedure TFrameWorker.Fail(CONST Msg: string);
begin
  FState.ErrorText:= 'TFrameTimer worker thread: ' + Msg;
  if NOT PostMessage(FWnd, WM_FRAME_FAILED, 0, 0)
  then AppDataCore.LogError(FState.ErrorText + ' (and PostMessage of the failure failed too: ' + SysErrorMessage(GetLastError) + ')');
end;


procedure TFrameWorker.Run;
VAR
  Pacer: TFramePacer;
  Request, Ticks, CurTicks: Int64;
  Generation, SeenGeneration: Integer;
  Active: Boolean;
  Handles: array[0..2] of THandle;
  WaitMs: Double;
  Due: Int64;
  WaitResult: DWORD;
begin
  Handles[0]:= FStopEvent;
  Handles[1]:= FWakeEvent;
  Handles[2]:= FWaitTimer;
  SeenGeneration:= -1;   { A masked generation is never negative, so the first request always starts the pacer }
  CurTicks:= 0;
  Active:= FALSE;

  repeat
    { Apply what the main thread asked for }
    Request   := TInterlocked.Read(FState.Request);
    Ticks     := Request AND TicksMask;
    Generation:= Integer(Request SHR GenShift);
    if Ticks = 0
    then Active:= FALSE
    else
      if Generation <> SeenGeneration then
      begin
        SeenGeneration:= Generation;
        CurTicks:= Ticks;
        Pacer.SetIntervalMs(Ticks / TicksPerMs);
        Pacer.Start(NowMs);
        Active:= TRUE;
      end
      else
        if Ticks <> CurTicks then
        begin
          CurTicks:= Ticks;
          Pacer.SetIntervalMs(Ticks / TicksPerMs);
        end;

    { Wait }
    if Active then
    begin
      WaitMs:= Pacer.MsUntilDue(NowMs);
      if WaitMs > 0 then
      begin
        Due:= -Max(Int64(1), Round(WaitMs * TicksPerMs));    { Negative = relative time, in 100 ns steps }
        if NOT SetWaitableTimer(FWaitTimer, Due, 0, NIL, NIL, FALSE) then
        begin
          Fail('SetWaitableTimer failed: ' + SysErrorMessage(GetLastError));
          EXIT;
        end;
        WaitResult:= WaitForMultipleObjects(3, @Handles, FALSE, INFINITE);
      end
      else
        WaitResult:= WaitForMultipleObjects(2, @Handles, FALSE, 0);
    end
    else
      WaitResult:= WaitForMultipleObjects(2, @Handles, FALSE, INFINITE);

    case WaitResult of
      WAIT_OBJECT_0    : EXIT;        { Stop }
      WAIT_OBJECT_0 + 1: Continue;    { Wake: a new request }
      WAIT_OBJECT_0 + 2,
      WAIT_TIMEOUT     : ;            { Timer, or a frame was already due }
    else
      Fail('WaitForMultipleObjects failed: ' + SysErrorMessage(GetLastError));
      EXIT;
    end;

    if Active AND Pacer.IsDue(NowMs) then
      { At most one WM_FRAME waits or runs: a busy main thread drops frames instead of receiving a burst }
      if TInterlocked.CompareExchange(FState.Pending, 1, 0) = 0 then
      begin
        if NOT PostMessage(FWnd, WM_FRAME, 0, 0) then
        begin
          TInterlocked.Exchange(FState.Pending, 0);
          Fail('PostMessage failed: ' + SysErrorMessage(GetLastError));
          EXIT;
        end;
      end
      else
        TInterlocked.Increment(FState.Skipped);
  until FALSE;
end;




{-------------------------------------------------------------------------------------------------------------
   TFrameTimer
-------------------------------------------------------------------------------------------------------------}
constructor TFrameTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FState  := TFrameTimerState.Create;
  FEnabled:= TRUE;
  FTicks  := 1000 * TicksPerMs;
  FWnd    := AllocateHWnd(WndProc);
end;


destructor TFrameTimer.Destroy;
begin
  { The worker never waits for the main thread, so a plain wait is safe. Never TThread.WaitFor here: on the main thread it runs CheckSynchronize }
  if FThread <> NIL then
  begin
    SetEvent(FStopEvent);
    WaitForSingleObject(FThread.Handle, INFINITE);
    FreeAndNil(FThread);
  end;

  if FWaitTimer <> 0 then CloseHandle(FWaitTimer);
  if FWakeEvent <> 0 then CloseHandle(FWakeEvent);
  if FStopEvent <> 0 then CloseHandle(FStopEvent);
  if FWnd <> 0 then DeallocateHWnd(FWnd);   { Drops a WM_FRAME that is still queued }

  { Freed inside our own OnTimer: the window procedure still needs the state, and frees it when OnTimer returns }
  if FState.InOnTimer
  then FState.OwnerFreed:= TRUE
  else FreeAndNil(FState);

  inherited;
end;


procedure TFrameTimer.StartWorker;
begin
  FStopEvent:= CreateEvent(NIL, TRUE,  FALSE, NIL);   { Manual reset }
  if FStopEvent = 0 then RaiseLastOSError;
  FWakeEvent:= CreateEvent(NIL, FALSE, FALSE, NIL);   { Auto reset }
  if FWakeEvent = 0 then RaiseLastOSError;

  FWaitTimer:= CreateWaitableTimerEx(NIL, NIL, CREATE_WAITABLE_TIMER_HIGH_RESOLUTION, TIMER_ALL_ACCESS);
  if FWaitTimer = 0 then
  begin
    AppDataCore.LogWarn('TFrameTimer: no high-resolution waitable timer (Windows older than 10 version 1803?): ' + SysErrorMessage(GetLastError) + '. Frames fall back to the 15.6 ms clock tick.');
    FWaitTimer:= CreateWaitableTimerEx(NIL, NIL, 0, TIMER_ALL_ACCESS);
    if FWaitTimer = 0 then RaiseLastOSError;
  end;

  FThread:= TFrameWorker.Create(FState, FWnd, FStopEvent, FWakeEvent, FWaitTimer);
end;


{ Hands the current settings to the worker. Main thread only }
procedure TFrameTimer.UpdateTimer;
VAR
  NewActive: Boolean;
  Ticks: Int64;
begin
  NewActive:= FEnabled AND (FTicks > 0) AND Assigned(FOnTimer);

  if NewActive then
  begin
    if FThread = NIL then StartWorker;
    if NOT FActive
    then Inc(FGeneration);   { Becoming active restarts the countdown, even when the worker never saw the inactive state }
    Ticks:= Max(FTicks, Int64(MinTicks));
    TInterlocked.Exchange(FState.Request, ((FGeneration AND GenMask) SHL GenShift) OR Ticks);
  end
  else
    TInterlocked.Exchange(FState.Request, 0);

  FActive:= NewActive;
  if FWakeEvent <> 0
  then SetEvent(FWakeEvent);
end;


procedure TFrameTimer.WndProc(var Msg: TMessage);
VAR State: TFrameTimerState;
begin
  case Msg.Msg of
    WM_FRAME, WM_FRAME_FAILED:
      begin
        { Use only locals after OnTimer: OnTimer may free Self }
        State:= FState;
        State.InOnTimer:= TRUE;
        TRY
          TRY
            if Msg.Msg = WM_FRAME_FAILED
            then raise EFrameTimerError.Create(State.ErrorText);
            if FActive
            then FOnTimer(Self);
          EXCEPT
            if Assigned(System.Classes.ApplicationHandleException)
            then System.Classes.ApplicationHandleException(Self)
            else RAISE;
          END;
        FINALLY
          State.InOnTimer:= FALSE;
          { Cleared only after OnTimer returns: a frame longer than one period then makes the worker skip a due time, so input and paint are not starved }
          TInterlocked.Exchange(State.Pending, 0);
          if State.OwnerFreed
          then FreeAndNil(State);
        END;
      end;
  else
    Msg.Result:= DefWindowProc(FWnd, Msg.Msg, Msg.WParam, Msg.LParam);
  end;
end;


procedure TFrameTimer.SetEnabled(Value: Boolean);
begin
  if Value = FEnabled then EXIT;
  FEnabled:= Value;
  UpdateTimer;
end;


procedure TFrameTimer.SetInterval(Value: Cardinal);
VAR NewTicks: Int64;
begin
  NewTicks:= Int64(Value) * TicksPerMs;
  if NewTicks = FTicks then EXIT;
  FTicks:= NewTicks;
  UpdateTimer;
end;


function TFrameTimer.GetInterval: Cardinal;
begin
  Result:= Cardinal(Round(FTicks / TicksPerMs));
end;


procedure TFrameTimer.SetFramesPerSecond(Value: Double);
VAR NewTicks: Int64;
begin
  if IsNan(Value) OR IsInfinite(Value) OR (Value < 0)
  then raise EArgumentOutOfRangeException.Create('TFrameTimer.FramesPerSecond must be 0 or a positive number.');

  if Value = 0
  then NewTicks:= 0
  else NewTicks:= Max(Int64(1), Round(10000000 / Value));   { 10^7 steps of 100 ns per second }

  if NewTicks = FTicks then EXIT;
  FTicks:= NewTicks;
  UpdateTimer;
end;


function TFrameTimer.GetFramesPerSecond: Double;
begin
  if FTicks = 0
  then Result:= 0
  else Result:= 10000000 / FTicks;
end;


procedure TFrameTimer.SetOnTimer(Value: TNotifyEvent);
begin
  if (TMethod(Value).Code = TMethod(FOnTimer).Code) AND (TMethod(Value).Data = TMethod(FOnTimer).Data) then EXIT;
  FOnTimer:= Value;
  UpdateTimer;
end;


function TFrameTimer.GetSkipped: Integer;
begin
  Result:= TInterlocked.CompareExchange(FState.Skipped, 0, 0);
end;


end.
