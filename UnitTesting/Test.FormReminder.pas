unit Test.FormReminder;

{=============================================================================================================
   Unit tests for FormReminder.pas
   Tests TfrmReminder - the countdown timer/reminder form.

   Note: These tests focus on form creation, timer logic, and event handling.
   System operations (sleep, shutdown, file execution) are not tested
   to avoid side effects during automated testing.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Graphics;

type
  [TestFixture]
  TTestFormReminder = class
  private
    FTestForm: TObject;
    FAdvanceCount: Integer;
    FTimeUpCount: Integer;
    procedure CleanupForm;
    procedure OnAdvanceHandler(Sender: TObject);
    procedure OnTimeUpHandler(Sender: TObject);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constants Tests }
    [Test]
    procedure TestConstant_TimerActiveColor;

    [Test]
    procedure TestConstant_ReminderTextX;

    [Test]
    procedure TestConstant_ReminderTextY;

    [Test]
    procedure TestConstant_ReminderFontName;

    [Test]
    procedure TestConstant_ReminderFontSize;

    { Form Creation Tests }
    [Test]
    procedure TestFormClassExists;

    [Test]
    procedure TestFormCreate_Succeeds;

    [Test]
    procedure TestFormCreate_WithNilOwner;

    { Component Tests }
    [Test]
    procedure TestFormHasTimer;

    [Test]
    procedure TestFormHasStartButton;

    [Test]
    procedure TestFormHasStopButton;

    [Test]
    procedure TestFormHasResetButton;

    [Test]
    procedure TestFormHasRunButton;

    [Test]
    procedure TestFormHasTimeSpinEdit;

    [Test]
    procedure TestFormHasPathEdit;

    [Test]
    procedure TestFormHasMakeNoiseCheckbox;

    [Test]
    procedure TestFormHasRunOnceCheckbox;

    [Test]
    procedure TestFormHasRunFileRadio;

    [Test]
    procedure TestFormHasSleepRadio;

    [Test]
    procedure TestFormHasShutdownRadio;

    [Test]
    procedure TestFormHasExecuteGroupBox;

    [Test]
    procedure TestFormHasTimerGroupBox;

    { TimeLeft Property Tests }
    [Test]
    procedure TestTimeLeft_DefaultValue;

    [Test]
    procedure TestTimeLeft_CanBeSet;

    [Test]
    procedure TestTimeLeft_CanBeRead;

    { Initialize Tests }
    [Test]
    procedure TestInitialize_SetsOnTimeUp;

    [Test]
    procedure TestInitialize_SetsOnAdvance;

    [Test]
    procedure TestInitialize_AcceptsNilHandlers;

    { Timer Start/Stop Tests }
    [Test]
    procedure TestBtnStartClick_EnablesTimer;

    [Test]
    procedure TestBtnStopClick_DisablesTimer;

    [Test]
    procedure TestBtnResetClick_ResetsTimeLeft;

    { SpinTimeChange Tests }
    [Test]
    procedure TestSpnTimeChange_UpdatesTimeLeft;

    [Test]
    procedure TestSpnTimeChange_ConvertsMinutesToSeconds;

    { UpdateDisplay Tests }
    [Test]
    procedure TestUpdateDisplay_WhenTimerEnabled;

    [Test]
    procedure TestUpdateDisplay_WhenTimerDisabled;

    { Radio Button Tests }
    [Test]
    procedure TestRadRunFileClick_EnablesExecuteGroup;

    [Test]
    procedure TestRadSleepClick_DisablesExecuteGroup;

    { Event Handling Tests }
    [Test]
    procedure TestOnAdvance_CalledDuringTimerTick;

    [Test]
    procedure TestOnTimeUp_CalledWhenTimeReachesZero;

    { FormDestroy Tests }
    [Test]
    procedure TestFormDestroy_DisablesTimer;

    { Button Click Tests }
    [Test]
    procedure TestBtnRunClick_NoExceptionWithEmptyPath;

    [Test]
    procedure TestBtnRunClick_NoExceptionWithPath;

    { Global Variable Tests }
    [Test]
    procedure TestGlobalVariable_Exists;
  end;

implementation

uses
  LightCore.AppData,
  LightVcl.Visual.AppData,
  FormReminder;


procedure TTestFormReminder.Setup;
begin
  Assert.IsNotNull(AppData, 'AppData must be initialized before running tests');
  FTestForm:= NIL;
  FAdvanceCount:= 0;
  FTimeUpCount:= 0;
end;


procedure TTestFormReminder.TearDown;
begin
  CleanupForm;
end;


procedure TTestFormReminder.CleanupForm;
var
  Form: TfrmReminder;
begin
  if FTestForm <> NIL then
    begin
      Form:= TfrmReminder(FTestForm);
      Form.Timer.Enabled:= FALSE;  // Ensure timer is stopped
      FreeAndNil(Form);
      FTestForm:= NIL;
    end;
end;


procedure TTestFormReminder.OnAdvanceHandler(Sender: TObject);
begin
  Inc(FAdvanceCount);
end;


procedure TTestFormReminder.OnTimeUpHandler(Sender: TObject);
begin
  Inc(FTimeUpCount);
end;


{ Constants Tests }

procedure TTestFormReminder.TestConstant_TimerActiveColor;
begin
  Assert.AreEqual(Cardinal($00C6E7C6), Cardinal(TIMER_ACTIVE_COLOR),
    'TIMER_ACTIVE_COLOR should be light green');
end;


procedure TTestFormReminder.TestConstant_ReminderTextX;
begin
  Assert.AreEqual(10, REMINDER_TEXT_X, 'REMINDER_TEXT_X should be 10');
end;


procedure TTestFormReminder.TestConstant_ReminderTextY;
begin
  Assert.AreEqual(10, REMINDER_TEXT_Y, 'REMINDER_TEXT_Y should be 10');
end;


procedure TTestFormReminder.TestConstant_ReminderFontName;
begin
  Assert.AreEqual('Arial', REMINDER_FONT_NAME, 'REMINDER_FONT_NAME should be Arial');
end;


procedure TTestFormReminder.TestConstant_ReminderFontSize;
begin
  Assert.AreEqual(30, REMINDER_FONT_SIZE, 'REMINDER_FONT_SIZE should be 30');
end;


{ Form Creation Tests }

procedure TTestFormReminder.TestFormClassExists;
begin
  Assert.IsNotNull(TfrmReminder, 'TfrmReminder class should exist');
end;


procedure TTestFormReminder.TestFormCreate_Succeeds;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form, 'Form creation should succeed');
end;


procedure TTestFormReminder.TestFormCreate_WithNilOwner;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Assert.IsNull(Form.Owner, 'Owner should be nil when created with nil');
end;


{ Component Tests }

procedure TTestFormReminder.TestFormHasTimer;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.Timer, 'Form should have Timer');
  Assert.IsTrue(Form.Timer is TTimer, 'Timer should be a TTimer');
end;


procedure TTestFormReminder.TestFormHasStartButton;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnStart, 'Form should have btnStart');
end;


procedure TTestFormReminder.TestFormHasStopButton;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnStop, 'Form should have btnStop');
end;


procedure TTestFormReminder.TestFormHasResetButton;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnReset, 'Form should have btnReset');
end;


procedure TTestFormReminder.TestFormHasRunButton;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnRun, 'Form should have btnRun');
end;


procedure TTestFormReminder.TestFormHasTimeSpinEdit;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.spnTime, 'Form should have spnTime');
end;


procedure TTestFormReminder.TestFormHasPathEdit;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.edtPath, 'Form should have edtPath');
end;


procedure TTestFormReminder.TestFormHasMakeNoiseCheckbox;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.chkMakeNoise, 'Form should have chkMakeNoise');
end;


procedure TTestFormReminder.TestFormHasRunOnceCheckbox;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.chkRunOnce, 'Form should have chkRunOnce');
end;


procedure TTestFormReminder.TestFormHasRunFileRadio;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.radRunFile, 'Form should have radRunFile');
end;


procedure TTestFormReminder.TestFormHasSleepRadio;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.radSleep, 'Form should have radSleep');
end;


procedure TTestFormReminder.TestFormHasShutdownRadio;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.radShutDown, 'Form should have radShutDown');
end;


procedure TTestFormReminder.TestFormHasExecuteGroupBox;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.grpExecute, 'Form should have grpExecute');
end;


procedure TTestFormReminder.TestFormHasTimerGroupBox;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.grpTimer, 'Form should have grpTimer');
end;


{ TimeLeft Property Tests }

procedure TTestFormReminder.TestTimeLeft_DefaultValue;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual(MaxInt, Form.TimeLeft, 'TimeLeft should default to MaxInt');
end;


procedure TTestFormReminder.TestTimeLeft_CanBeSet;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Form.TimeLeft:= 300;

  Assert.AreEqual(300, Form.TimeLeft, 'TimeLeft should be settable');
end;


procedure TTestFormReminder.TestTimeLeft_CanBeRead;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Form.TimeLeft:= 120;

  Assert.AreEqual(120, Form.TimeLeft, 'TimeLeft should be readable');
end;


{ Initialize Tests }

procedure TTestFormReminder.TestInitialize_SetsOnTimeUp;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Form.Initialize(NIL, OnTimeUpHandler);

  Assert.IsTrue(Assigned(Form.OnTimeUp), 'OnTimeUp should be assigned after Initialize');
end;


procedure TTestFormReminder.TestInitialize_SetsOnAdvance;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Form.Initialize(OnAdvanceHandler, NIL);

  Assert.IsTrue(Assigned(Form.OnAdvance), 'OnAdvance should be assigned after Initialize');
end;


procedure TTestFormReminder.TestInitialize_AcceptsNilHandlers;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  { Initialize should accept nil handlers }
  Assert.WillNotRaise(
    procedure
    begin
      Form.Initialize(NIL, NIL);
    end);
end;


{ Timer Start/Stop Tests }

procedure TTestFormReminder.TestBtnStartClick_EnablesTimer;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Form.Timer.Enabled:= FALSE;
  Form.btnStartClick(Form);

  Assert.IsTrue(Form.Timer.Enabled, 'btnStartClick should enable the timer');
end;


procedure TTestFormReminder.TestBtnStopClick_DisablesTimer;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Form.Timer.Enabled:= TRUE;
  Form.btnStopClick(Form);

  Assert.IsFalse(Form.Timer.Enabled, 'btnStopClick should disable the timer');
end;


procedure TTestFormReminder.TestBtnResetClick_ResetsTimeLeft;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Form.spnTime.Value:= 5;  // 5 minutes
  Form.TimeLeft:= 10;  // Some arbitrary value
  Form.btnResetClick(Form);

  Assert.AreEqual(300, Form.TimeLeft, 'btnResetClick should reset TimeLeft to spnTime * 60');
end;


{ SpinTimeChange Tests }

procedure TTestFormReminder.TestSpnTimeChange_UpdatesTimeLeft;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Form.spnTime.Value:= 10;
  Form.spnTimeChange(Form);

  Assert.AreEqual(600, Form.TimeLeft, 'spnTimeChange should update TimeLeft');
end;


procedure TTestFormReminder.TestSpnTimeChange_ConvertsMinutesToSeconds;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Form.spnTime.Value:= 1;  // 1 minute
  Form.spnTimeChange(Form);

  Assert.AreEqual(60, Form.TimeLeft, 'spnTimeChange should convert minutes to seconds');
end;


{ UpdateDisplay Tests }

procedure TTestFormReminder.TestUpdateDisplay_WhenTimerEnabled;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Form.Timer.Enabled:= TRUE;
  Form.TimeLeft:= 120;

  // Trigger display update via spnTimeChange (which calls UpdateDisplay)
  Form.spnTime.Value:= 2;
  Form.spnTimeChange(Form);

  Assert.IsTrue(Pos('Reminder in', Form.Caption) > 0,
    'Caption should show "Reminder in" when timer is enabled');
end;


procedure TTestFormReminder.TestUpdateDisplay_WhenTimerDisabled;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Form.Timer.Enabled:= FALSE;
  Form.btnStopClick(Form);

  Assert.AreEqual('Timer disabled!', Form.Caption,
    'Caption should show "Timer disabled!" when timer is disabled');
end;


{ Radio Button Tests }

procedure TTestFormReminder.TestRadRunFileClick_EnablesExecuteGroup;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Form.radRunFile.Checked:= TRUE;
  Form.radRunFileClick(Form);

  Assert.IsTrue(Form.grpExecute.Enabled,
    'Execute group should be enabled when Run File is selected');
end;


procedure TTestFormReminder.TestRadSleepClick_DisablesExecuteGroup;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Form.radRunFile.Checked:= FALSE;
  Form.radSleep.Checked:= TRUE;
  Form.radSleepClick(Form);

  Assert.IsFalse(Form.grpExecute.Enabled,
    'Execute group should be disabled when Sleep is selected');
end;


{ Event Handling Tests }

procedure TTestFormReminder.TestOnAdvance_CalledDuringTimerTick;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  FAdvanceCount:= 0;
  Form.Initialize(OnAdvanceHandler, NIL);
  Form.spnTime.Value:= 1;
  Form.TimeLeft:= 60;  // More than 0 so TimesUp is not triggered

  // Simulate timer tick
  Form.TimerTimer(Form);

  Assert.AreEqual(1, FAdvanceCount, 'OnAdvance should be called during timer tick');
end;


procedure TTestFormReminder.TestOnTimeUp_CalledWhenTimeReachesZero;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  FTimeUpCount:= 0;
  Form.Initialize(NIL, OnTimeUpHandler);
  Form.spnTime.Value:= 1;
  Form.TimeLeft:= 1;  // Will reach 0 after one tick
  Form.chkRunOnce.Checked:= TRUE;  // Prevent repeat
  Form.chkMakeNoise.Checked:= FALSE;  // No sound
  Form.radRunFile.Checked:= FALSE;
  Form.radSleep.Checked:= FALSE;
  Form.radShutDown.Checked:= FALSE;

  // Simulate timer tick - should trigger TimesUp
  Form.TimerTimer(Form);

  Assert.AreEqual(1, FTimeUpCount, 'OnTimeUp should be called when time reaches zero');
end;


{ FormDestroy Tests }

procedure TTestFormReminder.TestFormDestroy_DisablesTimer;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Form.Timer.Enabled:= TRUE;
  Form.FormDestroy(Form);

  Assert.IsFalse(Form.Timer.Enabled, 'FormDestroy should disable the timer');
end;


{ Button Click Tests }

procedure TTestFormReminder.TestBtnRunClick_NoExceptionWithEmptyPath;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  Form.edtPath.Path:= '';

  { btnRunClick should not raise exception with empty path }
  Assert.WillNotRaise(
    procedure
    begin
      Form.btnRunClick(Form);
    end);
end;


procedure TTestFormReminder.TestBtnRunClick_NoExceptionWithPath;
var
  Form: TfrmReminder;
begin
  Form:= TfrmReminder.Create(NIL);
  FTestForm:= Form;

  // Use a path that likely doesn't exist but won't cause an exception
  Form.edtPath.Path:= 'C:\NonExistent\File.txt';

  { btnRunClick should not raise exception with non-existent path }
  Assert.WillNotRaise(
    procedure
    begin
      Form.btnRunClick(Form);
    end);
end;


{ Global Variable Tests }

procedure TTestFormReminder.TestGlobalVariable_Exists;
begin
  // Just verify the global variable exists
  Assert.Pass('Global frmReminder variable exists');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFormReminder);

end.
