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

    { Initial State Tests }
    [Test]
    procedure TestCreate_InheritsFromTTimer;

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
  Vcl.Forms;


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


procedure TTestCubicTimer.TestRestart_WhenDisabled_EnablesTimer;
begin
  FTimer.Enabled:= FALSE;

  FTimer.Restart;

  Assert.IsTrue(FTimer.Enabled, 'Restart should enable a disabled timer');
end;


procedure TTestCubicTimer.TestRestart_WhenEnabled_StaysEnabled;
begin
  FTimer.Enabled:= TRUE;

  FTimer.Restart;

  Assert.IsTrue(FTimer.Enabled, 'Restart should keep timer enabled when already enabled');
end;


{ Reset Tests }

procedure TTestCubicTimer.TestReset_WhenEnabled_RestartsTimer;
begin
  FTimer.Enabled:= TRUE;

  FTimer.Reset;

  Assert.IsTrue(FTimer.Enabled, 'Reset should keep timer enabled when it was enabled');
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
begin
  FStandardTimer.Enabled:= TRUE;

  ResetTimer(FStandardTimer);

  Assert.IsTrue(FStandardTimer.Enabled, 'ResetTimer should keep timer enabled');
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


{ Initial State Tests }

procedure TTestCubicTimer.TestCreate_InheritsFromTTimer;
begin
  Assert.IsTrue(FTimer is TTimer, 'TCubicTimer should inherit from TTimer');
  Assert.IsTrue(FTimer.Interval > 0, 'Timer should have a valid interval');
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
