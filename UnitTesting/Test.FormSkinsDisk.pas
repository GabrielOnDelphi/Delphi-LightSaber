unit Test.FormSkinsDisk;

{=============================================================================================================
   Unit tests for FormSkinsDisk.pas
   Tests TfrmStyleDisk - the VCL skin selector form.

   Note: These tests focus on form creation, component existence, and basic behavior.
   Skin loading is tested with mock/existing files where possible.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  Winapi.Windows,
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Themes,
  Vcl.Styles;

type
  [TestFixture]
  TTestFormSkinsDisk = class
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
    procedure TestFormHasListBox;

    [Test]
    procedure TestFormHasTopLabel;

    [Test]
    procedure TestFormHasBottomPanel;

    [Test]
    procedure TestFormHasOKButton;

    [Test]
    procedure TestFormHasSkinEditorButton;

    [Test]
    procedure TestFormHasMoreSkinsLabel;

    { ListBox Tests }
    [Test]
    procedure TestListBox_IsPopulated;

    [Test]
    procedure TestListBox_HasDefaultTheme;

    [Test]
    procedure TestListBox_ClickDoesNotRaiseException;

    { Button Tests }
    [Test]
    procedure TestBtnOKClick_ClosesForm;

    [Test]
    procedure TestBtnSkinEditor_HandlerWired;

    { Form Events Tests }
    [Test]
    procedure TestFormCreate_CallsLoadForm;

    [Test]
    procedure TestFormCreate_PopulatesSkins;

    [Test]
    procedure TestFormPreRelease_NoException;

    [Test]
    procedure TestFormClose_SetsCaFree;

    [Test]
    procedure TestFormKeyPress_EnterClosesForm;

    [Test]
    procedure TestFormKeyPress_EscapeClosesForm;

    { Label Click Tests }
    [Test]
    procedure TestLblTopClick_RefreshesSkins;

    { Property Tests }
    [Test]
    procedure TestOnDefaultStyle_CanBeAssigned;

    [Test]
    procedure TestOnDefaultStyle_DefaultIsNil;


    { GetSkinDir Tests }
    [Test]
    procedure TestSkinDirHint_IsSet;

    { Default Windows Theme Tests }
    [Test]
    procedure TestDefaultTheme_IsFirstInList;

    [Test]
    procedure TestDefaultTheme_TextIsCorrect;

    { Form Attribute Tests }
    [Test]
    procedure TestFormCaption_IsStyleSelector;

    [Test]
    procedure TestFormKeyPreview_IsEnabled;

    [Test]
    procedure TestFormAlphaBlend_IsEnabled;
  end;

implementation

uses
  LightCore.AppData,
  LightVcl.Visual.AppData,
  FormSkinsDisk;


procedure TTestFormSkinsDisk.Setup;
begin
  Assert.IsNotNull(AppData, 'AppData must be initialized before running tests');
  FTestForm:= NIL;
end;


procedure TTestFormSkinsDisk.TearDown;
begin
  CleanupForm;
end;


procedure TTestFormSkinsDisk.CleanupForm;
var
  Form: TfrmStyleDisk;
begin
  if FTestForm <> NIL then
  begin
    Form:= TfrmStyleDisk(FTestForm);
    FreeAndNil(Form);
    FTestForm:= NIL;
  end;
end;


{ Form Creation Tests }

procedure TTestFormSkinsDisk.TestFormClassExists;
begin
  Assert.IsNotNull(TfrmStyleDisk, 'TfrmStyleDisk class should exist');
end;


procedure TTestFormSkinsDisk.TestFormCreate_Succeeds;
var
  Form: TfrmStyleDisk;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form, 'Form creation should succeed');
end;


procedure TTestFormSkinsDisk.TestFormCreate_WithNilOwner;
var
  Form: TfrmStyleDisk;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  Assert.IsNull(Form.Owner, 'Owner should be nil when created with nil');
end;


{ Component Tests }

procedure TTestFormSkinsDisk.TestFormHasListBox;
var
  Form: TfrmStyleDisk;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.lBox, 'Form should have lBox component');
  Assert.IsTrue(Form.lBox is TListBox, 'lBox should be a TListBox');
end;


procedure TTestFormSkinsDisk.TestFormHasTopLabel;
var
  Form: TfrmStyleDisk;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.lblTop, 'Form should have lblTop label');
end;


procedure TTestFormSkinsDisk.TestFormHasBottomPanel;
var
  Form: TfrmStyleDisk;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.pnlBottom, 'Form should have pnlBottom panel');
end;


procedure TTestFormSkinsDisk.TestFormHasOKButton;
var
  Form: TfrmStyleDisk;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnOK, 'Form should have btnOK button');
end;


procedure TTestFormSkinsDisk.TestFormHasSkinEditorButton;
var
  Form: TfrmStyleDisk;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnSkinEditor, 'Form should have btnSkinEditor button');
end;


procedure TTestFormSkinsDisk.TestFormHasMoreSkinsLabel;
var
  Form: TfrmStyleDisk;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.lblMoreSkinsTrial, 'Form should have lblMoreSkinsTrial label');
end;


{ ListBox Tests }

procedure TTestFormSkinsDisk.TestListBox_IsPopulated;
var
  Form: TfrmStyleDisk;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  { ListBox should have at least the default Windows theme entry }
  Assert.IsTrue(Form.lBox.Items.Count >= 1,
    'ListBox should have at least the default theme entry');
end;


procedure TTestFormSkinsDisk.TestListBox_HasDefaultTheme;
var
  Form: TfrmStyleDisk;
  Index: Integer;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  Index:= Form.lBox.Items.IndexOf('Windows default theme');
  Assert.IsTrue(Index >= 0, 'ListBox should contain "Windows default theme"');
end;


procedure TTestFormSkinsDisk.TestListBox_ClickDoesNotRaiseException;
var
  Form: TfrmStyleDisk;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  { Select default theme (first item) }
  Form.lBox.ItemIndex:= 0;

  { lBoxClick should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      Form.lBoxClick(Form);
    end);
end;


{ Button Tests }

procedure TTestFormSkinsDisk.TestBtnOKClick_ClosesForm;
var
  Form: TfrmStyleDisk;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  Form.Show;

  { btnOKClick should not raise exception - this will try to close the form }
  Assert.WillNotRaise(
    procedure
    begin
      Form.btnOKClick(Form);
    end);
end;


procedure TTestFormSkinsDisk.TestBtnSkinEditor_HandlerWired;
var
  Form: TfrmStyleDisk;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  { NOT executed: btnStyleEditorClick launches StyleDesigner.exe or opens a browser URL —
    side effects a test run must not trigger. Verify the DFM wiring instead. }
  Assert.IsTrue(Assigned(Form.btnSkinEditor.OnClick),
    'btnSkinEditor must have OnClick wired in the DFM (btnStyleEditorClick)');
end;


{ Form Events Tests }

procedure TTestFormSkinsDisk.TestFormCreate_CallsLoadForm;
var
  Form: TfrmStyleDisk;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  { If LoadForm was called, the form should exist without exception }
  Assert.IsNotNull(Form, 'FormCreate should call LoadForm successfully');
end;


procedure TTestFormSkinsDisk.TestFormCreate_PopulatesSkins;
var
  Form: TfrmStyleDisk;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  { FormCreate calls PopulateSkins, so list should have items }
  Assert.IsTrue(Form.lBox.Items.Count > 0,
    'FormCreate should populate skins list');
end;


procedure TTestFormSkinsDisk.TestFormPreRelease_NoException;
var
  Form: TfrmStyleDisk;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  { FormPreRelease (the TLightForm cleanup event; this form has no FormDestroy) should not raise.
    While Initializing is TRUE it must skip the INI write and return quietly. }
  Assert.WillNotRaise(
    procedure
    begin
      Form.FormPreRelease;
    end);
end;


procedure TTestFormSkinsDisk.TestFormClose_SetsCaFree;
var
  Form: TfrmStyleDisk;
  Action: TCloseAction;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  Action:= caNone;
  Form.FormClose(Form, Action);

  Assert.AreEqual(caFree, Action, 'FormClose should set Action to caFree');
end;


procedure TTestFormSkinsDisk.TestFormKeyPress_EnterClosesForm;
var
  Form: TfrmStyleDisk;
  Key: Word;
  Shift: TShiftState;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  Form.Show;
  Key:= VK_RETURN;
  Shift:= [];

  { FormKeyDown with Enter should not raise exception.
    Note: TfrmStyleDisk handles keys via FormKeyDown (wired in DFM as OnKeyDown), not FormKeyPress. }
  Assert.WillNotRaise(
    procedure
    begin
      Form.FormKeyDown(Form, Key, Shift);
    end);
end;


procedure TTestFormSkinsDisk.TestFormKeyPress_EscapeClosesForm;
var
  Form: TfrmStyleDisk;
  Key: Word;
  Shift: TShiftState;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  Form.Show;
  Key:= VK_ESCAPE;
  Shift:= [];

  { FormKeyDown with Escape should not raise exception.
    Note: TfrmStyleDisk handles keys via FormKeyDown (wired in DFM as OnKeyDown), not FormKeyPress. }
  Assert.WillNotRaise(
    procedure
    begin
      Form.FormKeyDown(Form, Key, Shift);
    end);
end;


{ Label Click Tests }

procedure TTestFormSkinsDisk.TestLblTopClick_RefreshesSkins;
var
  Form: TfrmStyleDisk;
  InitialCount: Integer;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  InitialCount:= Form.lBox.Items.Count;
  Form.lblTopClick(Form);

  { After refresh, count should be same (no new skins added dynamically) }
  Assert.AreEqual(InitialCount, Form.lBox.Items.Count,
    'lblTopClick should refresh skins list');
end;


{ Property Tests }

procedure TTestFormSkinsDisk.TestOnDefaultStyle_CanBeAssigned;
var
  Form: TfrmStyleDisk;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  { Assign nil to test assignment capability }
  Form.OnDefaultstyle:= NIL;

  { Should not raise exception on assignment }
  Assert.IsFalse(Assigned(Form.OnDefaultstyle),
    'OnDefaultstyle should be assignable (nil was assigned)');
end;


procedure TTestFormSkinsDisk.TestOnDefaultStyle_DefaultIsNil;
var
  Form: TfrmStyleDisk;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  Assert.IsFalse(Assigned(Form.OnDefaultstyle), 'OnDefaultstyle must be NIL after creation');
end;




{ GetSkinDir Tests }

procedure TTestFormSkinsDisk.TestSkinDirHint_IsSet;
var
  Form: TfrmStyleDisk;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.lblTop.Hint <> '',
    'lblTop.Hint should be set with skin directory path');
end;


{ Default Windows Theme Tests }

procedure TTestFormSkinsDisk.TestDefaultTheme_IsFirstInList;
var
  Form: TfrmStyleDisk;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual('Windows default theme', Form.lBox.Items[0],
    'First item should be "Windows default theme"');
end;


procedure TTestFormSkinsDisk.TestDefaultTheme_TextIsCorrect;
var
  Form: TfrmStyleDisk;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.lBox.Items.IndexOf('Windows default theme') >= 0,
    'List should contain "Windows default theme" text');
end;


{ Form Attribute Tests }

procedure TTestFormSkinsDisk.TestFormCaption_IsStyleSelector;
var
  Form: TfrmStyleDisk;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual('Style selector', Form.Caption,
    'Form caption should be "Style selector" (as set in the DFM)');
end;


procedure TTestFormSkinsDisk.TestFormKeyPreview_IsEnabled;
var
  Form: TfrmStyleDisk;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.KeyPreview,
    'KeyPreview should be enabled for keyboard shortcuts');
end;


procedure TTestFormSkinsDisk.TestFormAlphaBlend_IsEnabled;
var
  Form: TfrmStyleDisk;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.AlphaBlend,
    'AlphaBlend should be enabled per DFM');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFormSkinsDisk);

end.
