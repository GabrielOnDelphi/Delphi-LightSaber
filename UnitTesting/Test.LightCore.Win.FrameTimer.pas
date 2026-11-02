unit Test.LightCore.Win.FrameTimer;

{=============================================================================================================
   Unit tests for LightCore.Win.FrameTimer.pas
   Real timer, real worker thread. The tests pump messages with their own PeekMessage / DispatchMessage loop
   (never Application.ProcessMessages).
   Test plan: 'Frame timing - findings.md', section "Test plan", tests 7-16
   (C:\Projects\BioniX\Projects\Tester - Best timing method\).
=============================================================================================================}

interface
{$IFDEF MSWINDOWS}

uses
  DUnitX.TestFramework,
  Winapi.Windows, Winapi.Messages, Winapi.TlHelp32,
  System.SysUtils, System.Classes, System.Diagnostics,
  LightCore.Win.FrameTimer;

type
  ETestTimerError = class(Exception);

  [TestFixture]
  TTestFrameTimer = class
  private
    FTimer: TFrameTimer;
    FClock: TStopwatch;
    FCalls: Integer;
    FTimes: TArray<Double>;
    FDepth, FMaxDepth: Integer;
    FHandleCalls: Integer;
    FHandleSender: TObject;
    FRaiseOnce: Boolean;
    FChangeAtCall: Integer;
    function  Ms: Double;
    procedure Pump(DurationMs: Integer);
    procedure PumpUntilCalls(Calls, MaxMs: Integer);
    procedure CountTick(Sender: TObject);
    procedure FreeTick(Sender: TObject);
    procedure RaiseTick(Sender: TObject);
    procedure ChangeIntervalTick(Sender: TObject);
    procedure NestedPumpTick(Sender: TObject);
    procedure HandleException(Sender: TObject);
    function  ProcessThreadCount: Integer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test07_50fps_For1s;
    [Test]
    procedure Test08_DisableWhileQueued_NoCall;
    [Test]
    procedure Test09_FreeWhileEnabled;
    [Test]
    procedure Test10_FreeInsideOnTimer;
    [Test]
    procedure Test11a_OnTimerRaises_NoHandler_Reraises;
    [Test]
    procedure Test11b_OnTimerRaises_HandlerCalled;
    [Test]
    procedure Test12_IntervalZero_NoFrames;
    [Test]
    procedure Test13_Interval5_FloorIs10ms;
    [Test]
    procedure Test14_IntervalChangedInsideOnTimer;
    [Test]
    procedure Test15_NestedPump_NoReentry;
    [Test]
    procedure Test16_DisableEnable_Restarts;
  end;
{$ENDIF}


implementation
{$IFDEF MSWINDOWS}


{ Helpers }

procedure TTestFrameTimer.Setup;
begin
  FTimer       := NIL;
  FClock       := TStopwatch.StartNew;
  FCalls       := 0;
  FTimes       := NIL;
  FDepth       := 0;
  FMaxDepth    := 0;
  FHandleCalls := 0;
  FHandleSender:= NIL;
  FRaiseOnce   := FALSE;
  FChangeAtCall:= 0;
end;


procedure TTestFrameTimer.TearDown;
begin
  FreeAndNil(FTimer);
end;


function TTestFrameTimer.Ms: Double;
begin
  Result:= FClock.Elapsed.TotalMilliseconds;
end;


procedure TTestFrameTimer.Pump(DurationMs: Integer);
VAR
  Msg: TMsg;
  Dummy: THandle;
  Stop: Double;
begin
  Dummy:= 0;
  Stop:= Ms + DurationMs;
  while Ms < Stop do
  begin
    while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
    begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
    MsgWaitForMultipleObjects(0, Dummy, FALSE, 1, QS_ALLINPUT);
  end;
end;


procedure TTestFrameTimer.PumpUntilCalls(Calls, MaxMs: Integer);
VAR Stop: Double;
begin
  Stop:= Ms + MaxMs;
  while (FCalls < Calls) AND (Ms < Stop) do
    Pump(1);
end;


procedure TTestFrameTimer.CountTick(Sender: TObject);
begin
  Inc(FCalls);
  FTimes:= FTimes + [Ms];
end;


procedure TTestFrameTimer.FreeTick(Sender: TObject);
begin
  Inc(FCalls);
  FreeAndNil(FTimer);
end;


procedure TTestFrameTimer.RaiseTick(Sender: TObject);
begin
  Inc(FCalls);
  if FRaiseOnce then
  begin
    FRaiseOnce:= FALSE;
    raise ETestTimerError.Create('Raised by OnTimer on purpose');
  end;
end;


procedure TTestFrameTimer.ChangeIntervalTick(Sender: TObject);
begin
  CountTick(Sender);
  if FCalls = FChangeAtCall
  then FTimer.Interval:= 50;
end;


procedure TTestFrameTimer.NestedPumpTick(Sender: TObject);
begin
  Inc(FDepth);
  TRY
    if FDepth > FMaxDepth
    then FMaxDepth:= FDepth;
    Inc(FCalls);
    Pump(100);
  FINALLY
    Dec(FDepth);
  END;
end;


procedure TTestFrameTimer.HandleException(Sender: TObject);
begin
  Inc(FHandleCalls);
  FHandleSender:= Sender;
end;


function TTestFrameTimer.ProcessThreadCount: Integer;
VAR
  Snap: THandle;
  Entry: TThreadEntry32;
  Pid: DWORD;
begin
  Result:= 0;
  Pid:= GetCurrentProcessId;
  Snap:= CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
  if Snap = INVALID_HANDLE_VALUE then RaiseLastOSError;
  TRY
    Entry.dwSize:= SizeOf(Entry);
    if Thread32First(Snap, Entry) then
      repeat
        if Entry.th32OwnerProcessID = Pid
        then Inc(Result);
      until NOT Thread32Next(Snap, Entry);
  FINALLY
    CloseHandle(Snap);
  END;
end;




{ Tests }

procedure TTestFrameTimer.Test07_50fps_For1s;
begin
  FTimer:= TFrameTimer.Create(NIL);
  FTimer.FramesPerSecond:= 50;
  FTimer.OnTimer:= CountTick;
  Pump(1000);
  Assert.IsTrue((FCalls >= 48) AND (FCalls <= 51), 'OnTimer calls in 1 s at 50 fps: ' + IntToStr(FCalls));
end;


procedure TTestFrameTimer.Test08_DisableWhileQueued_NoCall;
begin
  FTimer:= TFrameTimer.Create(NIL);
  FTimer.Interval:= 10;
  FTimer.OnTimer:= CountTick;
  Sleep(40);                  { No pumping: the worker posts a WM_FRAME that stays queued }
  FTimer.Enabled:= FALSE;
  Pump(100);
  Assert.AreEqual(0, FCalls, 'OnTimer ran after Enabled:= FALSE');

  { The pending flag was cleared by the skipped frame, so frames come again }
  FTimer.Enabled:= TRUE;
  PumpUntilCalls(1, 500);
  Assert.AreEqual(1, FCalls, 'No frame after Enabled:= TRUE again');
end;


procedure TTestFrameTimer.Test09_FreeWhileEnabled;
VAR ThreadsBefore, CallsAtFree: Integer;
begin
  ThreadsBefore:= ProcessThreadCount;
  FTimer:= TFrameTimer.Create(NIL);
  FTimer.FramesPerSecond:= 100;
  FTimer.OnTimer:= CountTick;
  Pump(50);
  Assert.AreEqual(ThreadsBefore + 1, ProcessThreadCount, 'The worker thread runs');

  Sleep(15);                  { Let a frame get queued }
  FreeAndNil(FTimer);
  CallsAtFree:= FCalls;
  Assert.IsTrue(CallsAtFree > 0, 'No frame before Free');
  Pump(100);
  Assert.AreEqual(CallsAtFree, FCalls, 'OnTimer ran after Free');
  Assert.AreEqual(ThreadsBefore, ProcessThreadCount, 'The worker thread still runs after Free');
end;


procedure TTestFrameTimer.Test10_FreeInsideOnTimer;
begin
  FTimer:= TFrameTimer.Create(NIL);
  FTimer.Interval:= 10;
  FTimer.OnTimer:= FreeTick;
  Pump(100);
  Assert.IsNull(FTimer, 'OnTimer did not free the timer');
  Assert.AreEqual(1, FCalls, 'OnTimer ran after the timer was freed');

  { A second timer works }
  FCalls:= 0;
  FTimer:= TFrameTimer.Create(NIL);
  FTimer.Interval:= 10;
  FTimer.OnTimer:= CountTick;
  PumpUntilCalls(3, 500);
  Assert.AreEqual(3, FCalls, 'The second timer delivers no frames');
end;


procedure TTestFrameTimer.Test11a_OnTimerRaises_NoHandler_Reraises;
VAR OldHandler: procedure(Sender: TObject) of object;
begin
  OldHandler:= System.Classes.ApplicationHandleException;
  System.Classes.ApplicationHandleException:= NIL;
  TRY
    FTimer:= TFrameTimer.Create(NIL);
    FTimer.Interval:= 10;
    FRaiseOnce:= TRUE;
    FTimer.OnTimer:= RaiseTick;
    Assert.WillRaise(procedure begin Pump(300) end, ETestTimerError);

    { The pending flag was cleared in the finally, so the next frame comes }
    FCalls:= 0;
    PumpUntilCalls(1, 500);
    Assert.AreEqual(1, FCalls, 'No frame after the exception');
  FINALLY
    System.Classes.ApplicationHandleException:= OldHandler;
  END;
end;


procedure TTestFrameTimer.Test11b_OnTimerRaises_HandlerCalled;
VAR OldHandler: procedure(Sender: TObject) of object;
begin
  OldHandler:= System.Classes.ApplicationHandleException;
  System.Classes.ApplicationHandleException:= HandleException;
  TRY
    FTimer:= TFrameTimer.Create(NIL);
    FTimer.Interval:= 10;
    FRaiseOnce:= TRUE;
    FTimer.OnTimer:= RaiseTick;
    PumpUntilCalls(2, 500);
    Assert.AreEqual(1, FHandleCalls, 'ApplicationHandleException calls');
    Assert.AreSame(TObject(FTimer), FHandleSender, 'Sender of ApplicationHandleException');
    Assert.AreEqual(2, FCalls, 'OnTimer must run again on the next frame');
  FINALLY
    System.Classes.ApplicationHandleException:= OldHandler;
  END;
end;


procedure TTestFrameTimer.Test12_IntervalZero_NoFrames;
VAR SetAt: Double;
begin
  FTimer:= TFrameTimer.Create(NIL);
  FTimer.Interval:= 0;
  FTimer.OnTimer:= CountTick;
  Pump(200);
  Assert.AreEqual(0, FCalls, 'OnTimer ran with Interval = 0');

  SetAt:= Ms;
  FTimer.Interval:= 20;
  PumpUntilCalls(2, 500);
  Assert.AreEqual(2, FCalls, 'Frames did not resume');
  Assert.IsTrue(FTimes[0] - SetAt >= 19.5, 'The first frame came too early: ' + FloatToStr(FTimes[0] - SetAt) + ' ms');
end;


procedure TTestFrameTimer.Test13_Interval5_FloorIs10ms;
VAR i: Integer;
begin
  FTimer:= TFrameTimer.Create(NIL);
  FTimer.Interval:= 5;
  FTimer.OnTimer:= CountTick;
  Pump(300);
  Assert.IsTrue(FCalls >= 20, 'Too few frames: ' + IntToStr(FCalls));

  { The frames are DUE exactly 10 ms apart, but each OnTimer starts up to ~0.6 ms late (measured), so one gap can read 9.45 ms.
    Without the floor the gaps would be 5 ms: check each gap with a margin and the mean gap tightly }
  for i:= 1 to High(FTimes) do
    Assert.IsTrue(FTimes[i] - FTimes[i-1] >= 8, 'Gap far below the 10 ms floor: ' + FloatToStr(FTimes[i] - FTimes[i-1]) + ' ms');
  Assert.IsTrue((FTimes[High(FTimes)] - FTimes[0]) / High(FTimes) >= 9.9, 'Mean gap below the 10 ms floor: ' + FloatToStr((FTimes[High(FTimes)] - FTimes[0]) / High(FTimes)) + ' ms');
end;


procedure TTestFrameTimer.Test14_IntervalChangedInsideOnTimer;
VAR Gap: Double;
begin
  FChangeAtCall:= 3;
  FTimer:= TFrameTimer.Create(NIL);
  FTimer.Interval:= 20;
  FTimer.OnTimer:= ChangeIntervalTick;
  PumpUntilCalls(4, 1000);
  Assert.AreEqual(4, FCalls);
  Gap:= FTimes[3] - FTimes[2];
  Assert.IsTrue((Gap >= 45) AND (Gap <= 60), 'Gap after the change to 50 ms: ' + FloatToStr(Gap) + ' ms');
end;


procedure TTestFrameTimer.Test15_NestedPump_NoReentry;
begin
  FTimer:= TFrameTimer.Create(NIL);
  FTimer.Interval:= 20;
  FTimer.OnTimer:= NestedPumpTick;
  Pump(450);
  Assert.IsTrue(FCalls >= 2, 'Too few frames: ' + IntToStr(FCalls));
  Assert.AreEqual(1, FMaxDepth, 'OnTimer was entered twice');
end;


procedure TTestFrameTimer.Test16_DisableEnable_Restarts;
VAR RestartAt, Gap: Double;
begin
  FTimer:= TFrameTimer.Create(NIL);
  FTimer.Interval:= 50;
  FTimer.OnTimer:= CountTick;
  PumpUntilCalls(1, 500);
  Assert.AreEqual(1, FCalls);

  Pump(30);                    { The old schedule has its next frame about 20 ms from now }
  RestartAt:= Ms;
  FTimer.Enabled:= FALSE;
  FTimer.Enabled:= TRUE;
  PumpUntilCalls(2, 500);
  Assert.AreEqual(2, FCalls);
  Gap:= FTimes[1] - RestartAt;
  Assert.IsTrue(Gap >= 45, 'The frame after the restart came at the old due time: ' + FloatToStr(Gap) + ' ms after the restart');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFrameTimer);
{$ENDIF}

end.
