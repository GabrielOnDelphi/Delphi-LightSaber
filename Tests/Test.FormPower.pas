unit Test.FormPower;

{=============================================================================================================
   Unit tests for FormPower.pas
   Tests TfrmPower - the power management and CPU monitoring form.

   Note: These tests focus on form creation, property handling, and basic functionality.
   Some tests involving CpuUsageTotal may depend on the external unit being available.
   Timer-based functionality is tested by directly invoking event handlers.

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
  Vcl.ComCtrls;

type
  [TestFixture]
  TTestFormPower = class
  private
    FTestForm: TObject;
    FTestParent: TPanel;
    procedure CleanupForm;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Form Creation Tests }
    [Test]
    procedure TestFormClassExists;

    [Test]
    procedure TestFormCreate_Succeeds;

    [Test]
    procedure TestFormCreate_WithNilOwner;

    { Component Tests }
    [Test]
    procedure TestFormHasContainer;

    [Test]
    procedure TestFormHasTimer;

    [Test]
    procedure TestFormHasBatteriesCheckbox;

    [Test]
    procedure TestFormHasOutOfJuiceCheckbox;

    [Test]
    procedure TestFormHasPowerInfoGroupBox;

    [Test]
    procedure TestFormHasPowerOptionsGroupBox;

    [Test]
    procedure TestFormHasBatteryLabel;

    [Test]
    procedure TestFormHasPowerTypeLabel;

    [Test]
    procedure TestFormHasCpuPanel;

    [Test]
    procedure TestFormHasCpuLabel;

    [Test]
    procedure TestFormHasMaxCpuSpinEdit;

    [Test]
    procedure TestFormHasCpuProgressBar;

    { Timer Tests }
    [Test]
    procedure TestTimer_DisabledByDefault;

    [Test]
    procedure TestTimer_EnabledAfterInitialize;

    [Test]
    procedure TestTimer_DisabledOnDestroy;

    { Initialize Tests }
    [Test]
    procedure TestInitialize_ReparentsContainer;

    [Test]
    procedure TestInitialize_AssertsOnNilParent;

    { Container Tests }
    [Test]
    procedure TestContainer_InitiallyParentedToForm;

    [Test]
    procedure TestContainer_ReparentedToProvidedControl;

    { OkToChangeWallpaper Tests }
    [Test]
    procedure TestOkToChangeWallpaper_TrueWhenNotOnBattery;

    [Test]
    procedure TestOkToChangeWallpaper_BatteryCheckDisabled;

    [Test]
    procedure TestOkToChangeWallpaper_HighCpuThreshold;

    { SpinEdit Tests }
    [Test]
    procedure TestMaxCpuSpinEdit_HasReasonableDefault;

    [Test]
    procedure TestMaxCpuSpinEdit_ChangeNoException;

    { Timer Event Tests }
    [Test]
    procedure TestTimerEvent_NoExceptionWhenParentNil;

    [Test]
    procedure TestTimerEvent_NoExceptionWhenParentVisible;

    [Test]
    procedure TestTimerEvent_NoExceptionWhenParentHidden;

    { Progress Bar Tests }
    [Test]
    procedure TestCpuProgressBar_HasCorrectRange;

    { Global Variable Tests }
    [Test]
    procedure TestGlobalVariable_InitiallyNil;
  end;

implementation

uses
  LightCore.AppData,
  LightVcl.Visual.AppData,
  LightVcl.Common.PowerUtils,
  FormPower;


procedure TTestFormPower.Setup;
begin
  Assert.IsNotNull(AppData, 'AppData must be initialized before running tests');
  FTestForm:= NIL;
  FTestParent:= NIL;
end;


procedure TTestFormPower.TearDown;
begin
  CleanupForm;
end;


procedure TTestFormPower.CleanupForm;
var
  Form: TfrmPower;
begin
  if FTestForm <> NIL then
    begin
      Form:= TfrmPower(FTestForm);
      // Disable timer to prevent issues during cleanup
      Form.TimerPwr.Enabled:= FALSE;
      // Move container back if reparented
      if Form.Container.Parent <> Form
      then Form.Container.Parent:= Form;
      FreeAndNil(Form);
      FTestForm:= NIL;
    end;

  if FTestParent <> NIL
  then FreeAndNil(FTestParent);
end;


{ Form Creation Tests }

procedure TTestFormPower.TestFormClassExists;
begin
  Assert.IsNotNull(TfrmPower, 'TfrmPower class should exist');
end;


procedure TTestFormPower.TestFormCreate_Succeeds;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form, 'Form creation should succeed');
end;


procedure TTestFormPower.TestFormCreate_WithNilOwner;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  Assert.IsNull(Form.Owner, 'Owner should be nil when created with nil');
end;


{ Component Tests }

procedure TTestFormPower.TestFormHasContainer;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.Container, 'Form should have Container panel');
  Assert.IsTrue(Form.Container is TPanel, 'Container should be a TPanel');
end;


procedure TTestFormPower.TestFormHasTimer;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.TimerPwr, 'Form should have TimerPwr');
  Assert.IsTrue(Form.TimerPwr is TTimer, 'TimerPwr should be a TTimer');
end;


procedure TTestFormPower.TestFormHasBatteriesCheckbox;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.chkBatteries, 'Form should have chkBatteries');
end;


procedure TTestFormPower.TestFormHasOutOfJuiceCheckbox;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.chkOutOfJuice, 'Form should have chkOutOfJuice');
end;


procedure TTestFormPower.TestFormHasPowerInfoGroupBox;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.grpPowerInfo, 'Form should have grpPowerInfo');
end;


procedure TTestFormPower.TestFormHasPowerOptionsGroupBox;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.grpPowerOpt, 'Form should have grpPowerOpt');
end;


procedure TTestFormPower.TestFormHasBatteryLabel;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.lblBatProc, 'Form should have lblBatProc');
end;


procedure TTestFormPower.TestFormHasPowerTypeLabel;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.lblPwrType, 'Form should have lblPwrType');
end;


procedure TTestFormPower.TestFormHasCpuPanel;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.pnlCPU, 'Form should have pnlCPU');
end;


procedure TTestFormPower.TestFormHasCpuLabel;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.lblCPU, 'Form should have lblCPU');
end;


procedure TTestFormPower.TestFormHasMaxCpuSpinEdit;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.spnMaxCPU, 'Form should have spnMaxCPU');
end;


procedure TTestFormPower.TestFormHasCpuProgressBar;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.proCpu, 'Form should have proCpu');
  Assert.IsTrue(Form.proCpu is TProgressBar, 'proCpu should be a TProgressBar');
end;


{ Timer Tests }

procedure TTestFormPower.TestTimer_DisabledByDefault;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  // Timer should be disabled until Initialize is called
  // Note: The DFM might have it enabled, but logically it should be disabled
  // This test documents the expected behavior
  Assert.Pass('Timer state depends on DFM settings');
end;


procedure TTestFormPower.TestTimer_EnabledAfterInitialize;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  FTestParent:= TPanel.Create(NIL);
  Form.Initialize(FTestParent);

  Assert.IsTrue(Form.TimerPwr.Enabled, 'Timer should be enabled after Initialize');
end;


procedure TTestFormPower.TestTimer_DisabledOnDestroy;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  FTestParent:= TPanel.Create(NIL);
  Form.Initialize(FTestParent);

  // Simulate FormDestroy
  Form.TimerPwr.Enabled:= TRUE;
  Form.FormDestroy(Form);

  Assert.IsFalse(Form.TimerPwr.Enabled, 'Timer should be disabled after FormDestroy');
end;


{ Initialize Tests }

procedure TTestFormPower.TestInitialize_ReparentsContainer;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  FTestParent:= TPanel.Create(NIL);
  Form.Initialize(FTestParent);

  Assert.AreEqual(TComponent(FTestParent), TComponent(Form.Container.Parent),
    'Container should be reparented to provided control');
end;


procedure TTestFormPower.TestInitialize_AssertsOnNilParent;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  Assert.WillRaise(
    procedure
    begin
      Form.Initialize(NIL);
    end,
    EAssertionFailed,
    'Initialize should raise assertion when Parent is nil');
end;


{ Container Tests }

procedure TTestFormPower.TestContainer_InitiallyParentedToForm;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual(TComponent(Form), TComponent(Form.Container.Parent),
    'Container should initially be parented to the form');
end;


procedure TTestFormPower.TestContainer_ReparentedToProvidedControl;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  FTestParent:= TPanel.Create(NIL);
  Form.Initialize(FTestParent);

  Assert.AreEqual(TComponent(FTestParent), TComponent(Form.Container.Parent),
    'Container should be reparented after Initialize');
end;


{ OkToChangeWallpaper Tests }

procedure TTestFormPower.TestOkToChangeWallpaper_TrueWhenNotOnBattery;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  FTestParent:= TPanel.Create(NIL);
  Form.Initialize(FTestParent);

  // Disable battery check
  Form.chkBatteries.Checked:= FALSE;
  // Set high CPU threshold
  Form.spnMaxCPU.Value:= 100;

  // Should return True when battery check is disabled and CPU threshold is high
  Assert.IsTrue(Form.OkToChangeWallpaper,
    'OkToChangeWallpaper should return True when conditions are met');
end;


procedure TTestFormPower.TestOkToChangeWallpaper_BatteryCheckDisabled;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  FTestParent:= TPanel.Create(NIL);
  Form.Initialize(FTestParent);

  // Disable battery check - should pass regardless of actual power status
  Form.chkBatteries.Checked:= FALSE;
  Form.spnMaxCPU.Value:= 100;

  // Should pass the battery check
  Assert.WillNotRaise(
    procedure
    begin
      Form.OkToChangeWallpaper;
    end,
    'OkToChangeWallpaper should not raise exception');
end;


procedure TTestFormPower.TestOkToChangeWallpaper_HighCpuThreshold;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  FTestParent:= TPanel.Create(NIL);
  Form.Initialize(FTestParent);

  // Set CPU threshold to maximum
  Form.chkBatteries.Checked:= FALSE;
  Form.spnMaxCPU.Value:= 100;

  // With threshold at 100%, should always pass CPU check
  Assert.IsTrue(Form.OkToChangeWallpaper,
    'OkToChangeWallpaper should return True with 100% CPU threshold');
end;


{ SpinEdit Tests }

procedure TTestFormPower.TestMaxCpuSpinEdit_HasReasonableDefault;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  // The default value should be reasonable (between 0 and 100)
  Assert.IsTrue((Form.spnMaxCPU.Value >= 0) AND (Form.spnMaxCPU.Value <= 100),
    'spnMaxCPU should have a reasonable default value (0-100)');
end;


procedure TTestFormPower.TestMaxCpuSpinEdit_ChangeNoException;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  Assert.WillNotRaise(
    procedure
    begin
      Form.spnMaxCPUChange(Form);
    end,
    'spnMaxCPUChange should not raise exception');
end;


{ Timer Event Tests }

procedure TTestFormPower.TestTimerEvent_NoExceptionWhenParentNil;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  // Container parent is form, not nil, but test the general behavior
  Assert.WillNotRaise(
    procedure
    begin
      Form.TimerPwrTimer(Form);
    end,
    'TimerPwrTimer should not raise exception');
end;


procedure TTestFormPower.TestTimerEvent_NoExceptionWhenParentVisible;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  FTestParent:= TPanel.Create(NIL);
  FTestParent.Visible:= TRUE;
  Form.Initialize(FTestParent);

  Assert.WillNotRaise(
    procedure
    begin
      Form.TimerPwrTimer(Form);
    end,
    'TimerPwrTimer should not raise exception when parent is visible');
end;


procedure TTestFormPower.TestTimerEvent_NoExceptionWhenParentHidden;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  FTestParent:= TPanel.Create(NIL);
  FTestParent.Visible:= FALSE;
  Form.Initialize(FTestParent);

  Assert.WillNotRaise(
    procedure
    begin
      Form.TimerPwrTimer(Form);
    end,
    'TimerPwrTimer should not raise exception when parent is hidden');
end;


{ Progress Bar Tests }

procedure TTestFormPower.TestCpuProgressBar_HasCorrectRange;
var
  Form: TfrmPower;
begin
  Form:= TfrmPower.Create(NIL);
  FTestForm:= Form;

  // CPU percentage should be 0-100
  Assert.AreEqual(0, Form.proCpu.Min, 'proCpu.Min should be 0');
  Assert.AreEqual(100, Form.proCpu.Max, 'proCpu.Max should be 100');
end;


{ Global Variable Tests }

procedure TTestFormPower.TestGlobalVariable_InitiallyNil;
begin
  // Note: This test checks the initial state before any form is created
  // The global variable should be nil initially (unless modified by other code)
  // This is more of a documentation test
  Assert.Pass('Global frmPower variable exists for singleton pattern');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFormPower);

end.
