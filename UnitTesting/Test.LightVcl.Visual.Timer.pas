unit Test.LightVcl.Visual.Timer;

{=============================================================================================================
   Unit tests for LightVcl.Visual.Timer.pas
   Tests TCubicTimer enhanced timer component and ResetTimer utility function.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Vcl.ExtCtrls,
  LightVcl.Visual.Timer;

type
  [TestFixture]
  TTestCubicTimer = class
  private
    FTimer: TCubicTimer;
    FStandardTimer: TTimer;
    FFireCount: Integer;
    procedure TimerFired(Sender: TObject);
    function MsFromResetToFirstFire(Timer: TTimer; ResetProc: TProc): Int64;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Restart Tests }
    [Test]
    procedure TestRestart_EnablesTimer;

    [Test]
    procedure TestRestart_WhenDisabled_EnablesTimer;

    [Test]
    procedure TestRestart_WhenEnabled_StaysEnabled;

    { Reset Tests }
    [Test]
    procedure TestReset_WhenEnabled_RestartsTimer;

    [Test]
    procedure TestReset_WhenDisabled_StaysDisabled;

    { Stop Tests }
    [Test]
    procedure TestStop_DisablesTimer;

    [Test]
    procedure TestStop_WhenAlreadyDisabled_StaysDisabled;

    { ResetTimer Standalone Function Tests }
    [Test]
    procedure TestResetTimer_WhenEnabled_ResetsTimer;

    [Test]
    procedure TestResetTimer_WhenDisabled_StaysDisabled;

    [Test]
    procedure TestResetTimer_NilTimer_RaisesAssertion;

    { Integration Tests }
    [Test]
    procedure TestRestartThenStop_DisablesTimer;

    [Test]
    procedure TestStopThenReset_StaysDisabled;

    [Test]
    procedure TestRestartThenReset_StaysEnabled;
  end;

implementation

uses
  System.Diagnostics,
  Vcl.Forms;

CONST
  TestInterval = 500;   { ms }
  SleepBeforeReset = 350;   { ms. Less than TestInterval, so the first countdown has not elapsed yet }


procedure TTestCubicTimer.TimerFired(Sender: TObject);
begin
  Inc(FFireCount);
end;


{ Starts Timer, waits SleepBeforeReset ms WITHOUT a message pump (so no WM_TIMER can be dispatched), calls ResetProc,
  then pumps messages until the first OnTimer. Returns the ms from ResetProc to that OnTimer.
  A reset that restarts the countdown gives about TestInterval; a reset that does nothing gives about TestInterval - SleepBeforeReset.
  A slow PC can only make the result larger, never smaller. }
function TTestCubicTimer.MsFromResetToFirstFire(Timer: TTimer; ResetProc: TProc): Int64;
var
  Watch: TStopwatch;
begin
  FFireCount:= 0;
  Timer.OnTimer:= TimerFired;
  Timer.Interval:= TestInterval;
  Timer.Enabled:= TRUE;
  Sleep(SleepBeforeReset);

  ResetProc();
  Watch:= TStopwatch.StartNew;
  while (FFireCount = 0) AND (Watch.ElapsedMilliseconds < 5000) DO
    begin
      Application.ProcessMessages;
      Sleep(5);
    end;
  Result:= Watch.ElapsedMilliseconds;
  Timer.Enabled:= FALSE;

  Assert.IsTrue(FFireCount > 0, 'Precondition: the timer fired within 5 seconds');
end;


procedure TTestCubicTimer.Setup;
begin
  FTimer:= TCubicTimer.Create(nil);
  FTimer.Interval:= 1000;
  FTimer.Enabled:= FALSE;

  FStandardTimer:= TTimer.Create(nil);
  FStandardTimer.Interval:= 1000;
  FStandardTimer.Enabled:= FALSE;
end;


procedure TTestCubicTimer.TearDown;
begin
  FreeAndNil(FTimer);
  FreeAndNil(FStandardTimer);
end;


{ Restart Tests }

procedure TTestCubicTimer.TestRestart_EnablesTimer;
begin
  FTimer.Enabled:= FALSE;

  FTimer.Restart;

  Assert.IsTrue(FTimer.Enabled, 'Restart should enable the timer');
end;


{ The countdown started SleepBeforeReset ms earlier is stopped, then Restart must start the timer again with a full interval }
procedure TTestCubicTimer.TestRestart_WhenDisabled_EnablesTimer;
var
  Ms: Int64;
  EnabledAfterRestart: Boolean;
begin
  EnabledAfterRestart:= FALSE;
  Ms:= MsFromResetToFirstFire(FTimer,
         procedure
         begin
           FTimer.Enabled:= FALSE;
           FTimer.Restart;
           EnabledAfterRestart:= FTimer.Enabled;
         end);

  Assert.IsTrue(EnabledAfterRestart, 'Restart should enable a disabled timer');
  Assert.IsTrue(Ms >= TestInterval - 100, 'Restart must start a full countdown: OnTimer came ' + IntToStr(Ms) + ' ms after Restart');
end;


procedure TTestCubicTimer.TestRestart_WhenEnabled_StaysEnabled;
begin
  FTimer.Enabled:= TRUE;

  FTimer.Restart;

  Assert.IsTrue(FTimer.Enabled, 'Restart should keep timer enabled when already enabled');
end;


{ Reset Tests }

procedure TTestCubicTimer.TestReset_WhenEnabled_RestartsTimer;
var
  Ms: Int64;
begin
  Ms:= MsFromResetToFirstFire(FTimer, procedure begin FTimer.Reset; end);

  Assert.IsTrue(Ms >= TestInterval - 100, 'Reset must restart the countdown: OnTimer came ' + IntToStr(Ms) + ' ms after Reset');
end;


procedure TTestCubicTimer.TestReset_WhenDisabled_StaysDisabled;
begin
  FTimer.Enabled:= FALSE;

  FTimer.Reset;

  Assert.IsFalse(FTimer.Enabled, 'Reset should NOT enable timer when it was disabled');
end;


{ Stop Tests }

procedure TTestCubicTimer.TestStop_DisablesTimer;
begin
  FTimer.Enabled:= TRUE;

  FTimer.Stop;

  Assert.IsFalse(FTimer.Enabled, 'Stop should disable the timer');
end;


procedure TTestCubicTimer.TestStop_WhenAlreadyDisabled_StaysDisabled;
begin
  FTimer.Enabled:= FALSE;

  FTimer.Stop;

  Assert.IsFalse(FTimer.Enabled, 'Stop should keep timer disabled');
end;


{ ResetTimer Standalone Function Tests }

procedure TTestCubicTimer.TestResetTimer_WhenEnabled_ResetsTimer;
var
  Ms: Int64;
begin
  Ms:= MsFromResetToFirstFire(FStandardTimer, procedure begin ResetTimer(FStandardTimer); end);

  Assert.IsTrue(Ms >= TestInterval - 100, 'ResetTimer must restart the countdown: OnTimer came ' + IntToStr(Ms) + ' ms after ResetTimer');
end;


procedure TTestCubicTimer.TestResetTimer_WhenDisabled_StaysDisabled;
begin
  FStandardTimer.Enabled:= FALSE;

  ResetTimer(FStandardTimer);

  Assert.IsFalse(FStandardTimer.Enabled, 'ResetTimer should NOT enable a disabled timer');
end;


procedure TTestCubicTimer.TestResetTimer_NilTimer_RaisesAssertion;
begin
  Assert.WillRaise(
    procedure
    begin
      ResetTimer(nil);
    end,
    EAssertionFailed,
    'ResetTimer should raise assertion when Timer is nil');
end;


{ Integration Tests }

procedure TTestCubicTimer.TestRestartThenStop_DisablesTimer;
begin
  FTimer.Restart;
  Assert.IsTrue(FTimer.Enabled, 'Timer should be enabled after Restart');

  FTimer.Stop;
  Assert.IsFalse(FTimer.Enabled, 'Timer should be disabled after Stop');
end;


procedure TTestCubicTimer.TestStopThenReset_StaysDisabled;
begin
  FTimer.Stop;
  Assert.IsFalse(FTimer.Enabled, 'Timer should be disabled after Stop');

  FTimer.Reset;
  Assert.IsFalse(FTimer.Enabled, 'Timer should stay disabled after Reset (was not enabled)');
end;


procedure TTestCubicTimer.TestRestartThenReset_StaysEnabled;
begin
  FTimer.Restart;
  Assert.IsTrue(FTimer.Enabled, 'Timer should be enabled after Restart');

  FTimer.Reset;
  Assert.IsTrue(FTimer.Enabled, 'Timer should stay enabled after Reset (was enabled)');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestCubicTimer);

end.
