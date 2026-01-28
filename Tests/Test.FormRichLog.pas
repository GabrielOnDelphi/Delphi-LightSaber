unit Test.FormRichLog;

{=============================================================================================================
   Unit tests for FormRichLog.pas
   Tests TfrmRichLog - the rich log visual form.

   Note: These tests focus on form creation, component existence, and basic behavior.
   The underlying TRichLog component functionality is tested separately.

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
  TTestFormRichLog = class
  private
    FTestForm: TObject;
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
    procedure TestFormHasLog;

    [Test]
    procedure TestFormHasContainer;

    [Test]
    procedure TestFormHasBottomPanel;

    [Test]
    procedure TestFormHasClearButton;

    [Test]
    procedure TestFormHasAutoOpenCheckbox;

    [Test]
    procedure TestFormHasVerbosityTrackbar;

    { Log Component Tests }
    [Test]
    procedure TestLog_IsRichLog;

    [Test]
    procedure TestLog_HasOnErrorEvent;

    [Test]
    procedure TestLog_HasOnWarnEvent;

    { Container Tests }
    [Test]
    procedure TestContainer_ParentIsForm;

    [Test]
    procedure TestContainer_AlignsClient;

    { AutoOpen Checkbox Tests }
    [Test]
    procedure TestAutoOpen_DefaultChecked;

    [Test]
    procedure TestAutoOpen_CanBeUnchecked;

    { Clear Button Tests }
    [Test]
    procedure TestBtnClearClick_ClearsLog;

    [Test]
    procedure TestBtnClearClick_NoException;

    { LogError Event Tests }
    [Test]
    procedure TestLogError_ShowsFormWhenAutoOpenEnabled;

    [Test]
    procedure TestLogError_DoesNotShowWhenAutoOpenDisabled;

    { FormCreate Tests }
    [Test]
    procedure TestFormCreate_ConnectsOnWarn;

    [Test]
    procedure TestFormCreate_ConnectsOnError;

    { FormDestroy Tests }
    [Test]
    procedure TestFormDestroy_NoException;

    [Test]
    procedure TestFormDestroy_ContainerParentIsForm;

    { FormPostInitialize Tests }
    [Test]
    procedure TestFormPostInitialize_NoException;

    { Log Functionality Tests }
    [Test]
    procedure TestLog_CanAddMessage;

    [Test]
    procedure TestLog_CanAddError;

    [Test]
    procedure TestLog_CanAddWarning;

    [Test]
    procedure TestLog_CanClear;
  end;

implementation

uses
  LightCore.AppData,
  LightVcl.Visual.AppData,
  LightVcl.Visual.RichLog,
  FormRichLog;


procedure TTestFormRichLog.Setup;
begin
  Assert.IsNotNull(AppData, 'AppData must be initialized before running tests');
  FTestForm:= NIL;
end;


procedure TTestFormRichLog.TearDown;
begin
  CleanupForm;
end;


procedure TTestFormRichLog.CleanupForm;
var
  Form: TfrmRichLog;
begin
  if FTestForm <> NIL then
    begin
      Form:= TfrmRichLog(FTestForm);
      // Ensure Container is parented to form before freeing
      if Form.Container.Parent <> Form
      then Form.Container.Parent:= Form;
      FreeAndNil(Form);
      FTestForm:= NIL;
    end;
end;


{ Form Creation Tests }

procedure TTestFormRichLog.TestFormClassExists;
begin
  Assert.IsNotNull(TfrmRichLog, 'TfrmRichLog class should exist');
end;


procedure TTestFormRichLog.TestFormCreate_Succeeds;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form, 'Form creation should succeed');
end;


procedure TTestFormRichLog.TestFormCreate_WithNilOwner;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  Assert.IsNull(Form.Owner, 'Owner should be nil when created with nil');
end;


{ Component Tests }

procedure TTestFormRichLog.TestFormHasLog;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.Log, 'Form should have Log component');
end;


procedure TTestFormRichLog.TestFormHasContainer;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.Container, 'Form should have Container panel');
  Assert.IsTrue(Form.Container is TPanel, 'Container should be a TPanel');
end;


procedure TTestFormRichLog.TestFormHasBottomPanel;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.pnlBottom, 'Form should have pnlBottom panel');
end;


procedure TTestFormRichLog.TestFormHasClearButton;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnClear, 'Form should have btnClear button');
end;


procedure TTestFormRichLog.TestFormHasAutoOpenCheckbox;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.chkAutoOpen, 'Form should have chkAutoOpen checkbox');
end;


procedure TTestFormRichLog.TestFormHasVerbosityTrackbar;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.trkLogVerb, 'Form should have trkLogVerb trackbar');
end;


{ Log Component Tests }

procedure TTestFormRichLog.TestLog_IsRichLog;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.Log is TRichLog, 'Log should be a TRichLog');
end;


procedure TTestFormRichLog.TestLog_HasOnErrorEvent;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Assigned(Form.Log.OnError),
    'Log.OnError should be assigned after FormCreate');
end;


procedure TTestFormRichLog.TestLog_HasOnWarnEvent;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Assigned(Form.Log.OnWarn),
    'Log.OnWarn should be assigned after FormCreate');
end;


{ Container Tests }

procedure TTestFormRichLog.TestContainer_ParentIsForm;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual(TComponent(Form), TComponent(Form.Container.Parent),
    'Container parent should be the form');
end;


procedure TTestFormRichLog.TestContainer_AlignsClient;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual(alClient, Form.Container.Align,
    'Container should align to client');
end;


{ AutoOpen Checkbox Tests }

procedure TTestFormRichLog.TestAutoOpen_DefaultChecked;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.chkAutoOpen.Checked,
    'chkAutoOpen should be checked by default');
end;


procedure TTestFormRichLog.TestAutoOpen_CanBeUnchecked;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  Form.chkAutoOpen.Checked:= FALSE;

  Assert.IsFalse(Form.chkAutoOpen.Checked,
    'chkAutoOpen should be uncheckable');
end;


{ Clear Button Tests }

procedure TTestFormRichLog.TestBtnClearClick_ClearsLog;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  // Add some content
  Form.Log.AddInfo('Test message');
  Assert.IsTrue(Form.Log.Lines.Count > 0, 'Log should have content before clear');

  // Clear
  Form.btnClearClick(Form);

  // Verify cleared (RichEdit may have 1 empty line)
  Assert.IsTrue(Form.Log.Lines.Count <= 1,
    'Log should be cleared after btnClearClick');
end;


procedure TTestFormRichLog.TestBtnClearClick_NoException;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  Assert.WillNotRaise(
    procedure
    begin
      Form.btnClearClick(Form);
    end,
    'btnClearClick should not raise exception');
end;


{ LogError Event Tests }

procedure TTestFormRichLog.TestLogError_ShowsFormWhenAutoOpenEnabled;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  Form.chkAutoOpen.Checked:= TRUE;
  Form.Hide;

  // Trigger the LogError handler
  Form.LogError(Form);

  Assert.IsTrue(Form.Visible,
    'Form should be visible after LogError when AutoOpen is enabled');
end;


procedure TTestFormRichLog.TestLogError_DoesNotShowWhenAutoOpenDisabled;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  Form.chkAutoOpen.Checked:= FALSE;
  Form.Hide;

  // Trigger the LogError handler
  Form.LogError(Form);

  Assert.IsFalse(Form.Visible,
    'Form should remain hidden after LogError when AutoOpen is disabled');
end;


{ FormCreate Tests }

procedure TTestFormRichLog.TestFormCreate_ConnectsOnWarn;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  // OnWarn should be connected in FormCreate
  Assert.IsTrue(Assigned(Form.Log.OnWarn),
    'FormCreate should connect Log.OnWarn');
end;


procedure TTestFormRichLog.TestFormCreate_ConnectsOnError;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  // OnError should be connected in FormCreate
  Assert.IsTrue(Assigned(Form.Log.OnError),
    'FormCreate should connect Log.OnError');
end;


{ FormDestroy Tests }

procedure TTestFormRichLog.TestFormDestroy_NoException;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  Assert.WillNotRaise(
    procedure
    begin
      Form.FormDestroy(Form);
    end,
    'FormDestroy should not raise exception');
end;


procedure TTestFormRichLog.TestFormDestroy_ContainerParentIsForm;
var
  Form: TfrmRichLog;
  TempPanel: TPanel;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  // Reparent Container
  TempPanel:= TPanel.Create(NIL);
  try
    Form.Container.Parent:= TempPanel;

    // Call FormDestroy
    Form.FormDestroy(Form);

    // Container should be moved back to Form
    Assert.AreEqual(TComponent(Form), TComponent(Form.Container.Parent),
      'FormDestroy should reparent Container back to Form');
  finally
    FreeAndNil(TempPanel);
  end;
end;


{ FormPostInitialize Tests }

procedure TTestFormRichLog.TestFormPostInitialize_NoException;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  Assert.WillNotRaise(
    procedure
    begin
      Form.FormPostInitialize;
    end,
    'FormPostInitialize should not raise exception');
end;


{ Log Functionality Tests }

procedure TTestFormRichLog.TestLog_CanAddMessage;
var
  Form: TfrmRichLog;
  InitialCount: Integer;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  Form.Log.Clear;
  InitialCount:= Form.Log.Lines.Count;

  Form.Log.AddMsg('Test message');

  Assert.IsTrue(Form.Log.Lines.Count > InitialCount,
    'AddMsg should add a line to the log');
end;


procedure TTestFormRichLog.TestLog_CanAddError;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  // Disable auto-open to prevent form from showing
  Form.chkAutoOpen.Checked:= FALSE;

  Assert.WillNotRaise(
    procedure
    begin
      Form.Log.AddError('Test error');
    end,
    'AddError should not raise exception');
end;


procedure TTestFormRichLog.TestLog_CanAddWarning;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  // Disable auto-open to prevent form from showing
  Form.chkAutoOpen.Checked:= FALSE;

  Assert.WillNotRaise(
    procedure
    begin
      Form.Log.AddWarn('Test warning');
    end,
    'AddWarn should not raise exception');
end;


procedure TTestFormRichLog.TestLog_CanClear;
var
  Form: TfrmRichLog;
begin
  Form:= TfrmRichLog.Create(NIL);
  FTestForm:= Form;

  Form.Log.AddInfo('Test');

  Assert.WillNotRaise(
    procedure
    begin
      Form.Log.Clear;
    end,
    'Clear should not raise exception');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFormRichLog);

end.
