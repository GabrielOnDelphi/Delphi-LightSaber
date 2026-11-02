unit Test.LightCore.FramePacer;

{=============================================================================================================
   Unit tests for LightCore.FramePacer.pas
   No real timer: the clock is a number the test passes in.
   Test plan: 'Frame timing - findings.md', section "Test plan", tests 1-6
   (C:\Projects\BioniX\Projects\Tester - Best timing method\).
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils, System.Math,
  LightCore.FramePacer;

type
  [TestFixture]
  TTestFramePacer = class
  public
    [Test]
    procedure Test01_Grid15ms_Gives300FramesIn10s;
    [Test]
    procedure Test02_Calls1ms_GapsAre33or34;
    [Test]
    procedure Test03_Stall_NoBurst;
    [Test]
    procedure Test04_SetInterval_CountsFromLastDue;
    [Test]
    procedure Test05_Start_FirstFrameOnePeriodLater;
    [Test]
    procedure Test06_SetInterval_RaisesOnBadValues;
  end;


implementation

CONST
  Period30 = 1000 / 30;


procedure TTestFramePacer.Test01_Grid15ms_Gives300FramesIn10s;
VAR
  Pacer: TFramePacer;
  Tick, Frames: Integer;
begin
  Pacer.SetIntervalMs(Period30);
  Pacer.Start(0);
  Frames:= 0;
  for Tick:= 1 to 640 do      { 640 * 15.625 = 10000 ms }
    if Pacer.IsDue(Tick * 15.625)
    then Inc(Frames);

  Assert.IsTrue(Abs(Frames - 300) <= 1, 'Frames in 10 s: ' + IntToStr(Frames));
end;


procedure TTestFramePacer.Test02_Calls1ms_GapsAre33or34;
VAR
  Pacer: TFramePacer;
  Ms, LastTrue, Frames: Integer;
begin
  Pacer.SetIntervalMs(Period30);
  Pacer.Start(0);
  LastTrue:= 0;
  Frames:= 0;
  for Ms:= 1 to 5000 do
    if Pacer.IsDue(Ms) then
    begin
      Assert.IsTrue((Ms - LastTrue = 33) OR (Ms - LastTrue = 34), 'Gap ' + IntToStr(Ms - LastTrue) + ' ms at ' + IntToStr(Ms));
      LastTrue:= Ms;
      Inc(Frames);
    end;

  Assert.AreEqual(150, Frames);
end;


procedure TTestFramePacer.Test03_Stall_NoBurst;
VAR
  Pacer: TFramePacer;
  Ms: Integer;
  TrueTimes: TArray<Integer>;
begin
  Pacer.SetIntervalMs(Period30);
  Pacer.Start(0);

  { No call for 500 ms, then one call every 1 ms for 90 ms }
  for Ms:= 500 to 590 do
    if Pacer.IsDue(Ms)
    then TrueTimes:= TrueTimes + [Ms];

  Assert.IsTrue(Length(TrueTimes) >= 2, 'Frames after the stall: ' + IntToStr(Length(TrueTimes)));
  Assert.AreEqual(500, TrueTimes[0], 'The first frame comes at once');
  Assert.AreEqual(534, TrueTimes[1], 'The second frame comes one period later (500 + 33.3 rounded up to the next 1 ms call)');
  Assert.AreEqual(3, Length(TrueTimes), 'No burst: 500, 534, 567');
end;


procedure TTestFramePacer.Test04_SetInterval_CountsFromLastDue;
VAR
  Pacer: TFramePacer;
  Ms, FirstDue: Integer;
begin
  Pacer.SetIntervalMs(Period30);
  Pacer.Start(0);

  { First TRUE at 34 ms; its due time was 33.33 ms }
  FirstDue:= -1;
  for Ms:= 1 to 100 do
    if Pacer.IsDue(Ms) then
    begin
      FirstDue:= Ms;
      Break;
    end;
  Assert.AreEqual(34, FirstDue);

  Pacer.SetIntervalMs(50);
  Assert.AreEqual(50.0, Pacer.IntervalMs, 0.0001);

  { Next due = 33.33 + 50 = 83.33, counted from the due time, not from the call at 34 ms }
  Assert.AreEqual(83.3333 - 40, Pacer.MsUntilDue(40), 0.001, 'MsUntilDue counts from the last due time');
  Assert.IsFalse(Pacer.IsDue(83.3));
  Assert.IsTrue (Pacer.IsDue(83.34));
end;


procedure TTestFramePacer.Test05_Start_FirstFrameOnePeriodLater;
VAR Pacer: TFramePacer;
begin
  Pacer.SetIntervalMs(33.3);
  Pacer.Start(1000);
  Assert.IsFalse(Pacer.IsDue(1000));
  Assert.AreEqual(33.3, Pacer.MsUntilDue(1000), 0.0001);
  Assert.IsTrue (Pacer.IsDue(1033.4));
  Assert.AreEqual(0.0, Pacer.MsUntilDue(2000), 0.0001, 'MsUntilDue is 0 when a frame is due');
end;


procedure TTestFramePacer.Test06_SetInterval_RaisesOnBadValues;
VAR Pacer: TFramePacer;
begin
  Pacer.SetIntervalMs(10);
  Assert.WillRaise(procedure begin Pacer.SetIntervalMs(0)   end, EArgumentOutOfRangeException);
  Assert.WillRaise(procedure begin Pacer.SetIntervalMs(-1)  end, EArgumentOutOfRangeException);
  Assert.WillRaise(procedure begin Pacer.SetIntervalMs(NaN) end, EArgumentOutOfRangeException);
  Assert.AreEqual(10.0, Pacer.IntervalMs, 0.0001, 'A rejected value leaves the interval unchanged');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFramePacer);

end.
