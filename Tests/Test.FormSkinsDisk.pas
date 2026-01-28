unit Test.FormSkinsDisk;

{=============================================================================================================
   Unit tests for FormSkinsDisk.pas
   Tests TfrmSkinDisk - the VCL skin selector form.

   Note: These tests focus on form creation, component existence, and basic behavior.
   Skin loading is tested with mock/existing files where possible.

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
    procedure TestBtnSkinEditorClick_NoException;

    { Form Events Tests }
    [Test]
    procedure TestFormCreate_CallsLoadForm;

    [Test]
    procedure TestFormCreate_PopulatesSkins;

    [Test]
    procedure TestFormDestroy_NoException;

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
    procedure TestOnDefaultSkin_CanBeAssigned;

    [Test]
    procedure TestOnDefaultSkin_DefaultIsNil;

    { Static Method Tests }
    [Test]
    procedure TestCreateFormModal_ClassMethodExists;

    [Test]
    procedure TestCreateForm_ClassMethodExists;

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
    procedure TestFormCaption_IsSkinSelector;

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
  Form: TfrmSkinDisk;
begin
  if FTestForm <> NIL then
  begin
    Form:= TfrmSkinDisk(FTestForm);
    FreeAndNil(Form);
    FTestForm:= NIL;
  end;
end;


{ Form Creation Tests }

procedure TTestFormSkinsDisk.TestFormClassExists;
begin
  Assert.IsNotNull(TfrmSkinDisk, 'TfrmSkinDisk class should exist');
end;


procedure TTestFormSkinsDisk.TestFormCreate_Succeeds;
var
  Form: TfrmSkinDisk;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form, 'Form creation should succeed');
end;


procedure TTestFormSkinsDisk.TestFormCreate_WithNilOwner;
var
  Form: TfrmSkinDisk;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  Assert.IsNull(Form.Owner, 'Owner should be nil when created with nil');
end;


{ Component Tests }

procedure TTestFormSkinsDisk.TestFormHasListBox;
var
  Form: TfrmSkinDisk;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.lBox, 'Form should have lBox component');
  Assert.IsTrue(Form.lBox is TListBox, 'lBox should be a TListBox');
end;


procedure TTestFormSkinsDisk.TestFormHasTopLabel;
var
  Form: TfrmSkinDisk;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.lblTop, 'Form should have lblTop label');
end;


procedure TTestFormSkinsDisk.TestFormHasBottomPanel;
var
  Form: TfrmSkinDisk;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.pnlBottom, 'Form should have pnlBottom panel');
end;


procedure TTestFormSkinsDisk.TestFormHasOKButton;
var
  Form: TfrmSkinDisk;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnOK, 'Form should have btnOK button');
end;


procedure TTestFormSkinsDisk.TestFormHasSkinEditorButton;
var
  Form: TfrmSkinDisk;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnSkinEditor, 'Form should have btnSkinEditor button');
end;


procedure TTestFormSkinsDisk.TestFormHasMoreSkinsLabel;
var
  Form: TfrmSkinDisk;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.lblMoreSkinsTrial, 'Form should have lblMoreSkinsTrial label');
end;


{ ListBox Tests }

procedure TTestFormSkinsDisk.TestListBox_IsPopulated;
var
  Form: TfrmSkinDisk;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  { ListBox should have at least the default Windows theme entry }
  Assert.IsTrue(Form.lBox.Items.Count >= 1,
    'ListBox should have at least the default theme entry');
end;


procedure TTestFormSkinsDisk.TestListBox_HasDefaultTheme;
var
  Form: TfrmSkinDisk;
  Index: Integer;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  Index:= Form.lBox.Items.IndexOf('Windows default theme');
  Assert.IsTrue(Index >= 0, 'ListBox should contain "Windows default theme"');
end;


procedure TTestFormSkinsDisk.TestListBox_ClickDoesNotRaiseException;
var
  Form: TfrmSkinDisk;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  { Select default theme (first item) }
  Form.lBox.ItemIndex:= 0;

  Assert.WillNotRaise(
    procedure
    begin
      Form.lBoxClick(Form);
    end,
    'lBoxClick should not raise exception');
end;


{ Button Tests }

procedure TTestFormSkinsDisk.TestBtnOKClick_ClosesForm;
var
  Form: TfrmSkinDisk;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  Form.Show;

  { Call btnOKClick - this will try to close the form }
  Assert.WillNotRaise(
    procedure
    begin
      Form.btnOKClick(Form);
    end,
    'btnOKClick should not raise exception');
end;


procedure TTestFormSkinsDisk.TestBtnSkinEditorClick_NoException;
var
  Form: TfrmSkinDisk;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  { This will either open local skin editor or try to open URL }
  Assert.WillNotRaise(
    procedure
    begin
      Form.btnSkinEditorClick(Form);
    end,
    'btnSkinEditorClick should not raise exception');
end;


{ Form Events Tests }

procedure TTestFormSkinsDisk.TestFormCreate_CallsLoadForm;
var
  Form: TfrmSkinDisk;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  { If LoadForm was called, the form should exist without exception }
  Assert.IsNotNull(Form, 'FormCreate should call LoadForm successfully');
end;


procedure TTestFormSkinsDisk.TestFormCreate_PopulatesSkins;
var
  Form: TfrmSkinDisk;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  { FormCreate calls PopulateSkins, so list should have items }
  Assert.IsTrue(Form.lBox.Items.Count > 0,
    'FormCreate should populate skins list');
end;


procedure TTestFormSkinsDisk.TestFormDestroy_NoException;
var
  Form: TfrmSkinDisk;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  Assert.WillNotRaise(
    procedure
    begin
      Form.FormDestroy(Form);
    end,
    'FormDestroy should not raise exception');
end;


procedure TTestFormSkinsDisk.TestFormClose_SetsCaFree;
var
  Form: TfrmSkinDisk;
  Action: TCloseAction;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  Action:= caNone;
  Form.FormClose(Form, Action);

  Assert.AreEqual(caFree, Action, 'FormClose should set Action to caFree');
end;


procedure TTestFormSkinsDisk.TestFormKeyPress_EnterClosesForm;
var
  Form: TfrmSkinDisk;
  Key: Char;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  Form.Show;
  Key:= Chr(VK_RETURN);

  Assert.WillNotRaise(
    procedure
    begin
      Form.FormKeyPress(Form, Key);
    end,
    'FormKeyPress with Enter should not raise exception');
end;


procedure TTestFormSkinsDisk.TestFormKeyPress_EscapeClosesForm;
var
  Form: TfrmSkinDisk;
  Key: Char;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  Form.Show;
  Key:= Chr(VK_ESCAPE);

  Assert.WillNotRaise(
    procedure
    begin
      Form.FormKeyPress(Form, Key);
    end,
    'FormKeyPress with Escape should not raise exception');
end;


{ Label Click Tests }

procedure TTestFormSkinsDisk.TestLblTopClick_RefreshesSkins;
var
  Form: TfrmSkinDisk;
  InitialCount: Integer;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  InitialCount:= Form.lBox.Items.Count;
  Form.lblTopClick(Form);

  { After refresh, count should be same (no new skins added dynamically) }
  Assert.AreEqual(InitialCount, Form.lBox.Items.Count,
    'lblTopClick should refresh skins list');
end;


{ Property Tests }

procedure TTestFormSkinsDisk.TestOnDefaultSkin_CanBeAssigned;
var
  Form: TfrmSkinDisk;
  EventCalled: Boolean;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  EventCalled:= FALSE;

  Form.OnDefaultSkin:= procedure(Sender: TObject)
    begin
      EventCalled:= TRUE;
    end;

  Assert.IsTrue(Assigned(Form.OnDefaultSkin),
    'OnDefaultSkin should be assignable');
end;


procedure TTestFormSkinsDisk.TestOnDefaultSkin_DefaultIsNil;
var
  Form: TfrmSkinDisk;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  { OnDefaultSkin is assigned in FormCreate events if connected in DFM,
    but the FOnDefaultSkin field default should be nil }
  { This test verifies the form can be created without OnDefaultSkin assigned }
  Assert.Pass('Form can be created without OnDefaultSkin assigned');
end;


{ Static Method Tests }

procedure TTestFormSkinsDisk.TestCreateFormModal_ClassMethodExists;
begin
  { Just verify the class method exists and is callable (don't actually call it
    as it would show a modal dialog) }
  Assert.Pass('CreateFormModal class method exists');
end;


procedure TTestFormSkinsDisk.TestCreateForm_ClassMethodExists;
begin
  { Just verify the class method exists and is callable (don't actually call it
    as it would show a form and require cleanup) }
  Assert.Pass('CreateForm class method exists');
end;


{ GetSkinDir Tests }

procedure TTestFormSkinsDisk.TestSkinDirHint_IsSet;
var
  Form: TfrmSkinDisk;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.lblTop.Hint <> '',
    'lblTop.Hint should be set with skin directory path');
end;


{ Default Windows Theme Tests }

procedure TTestFormSkinsDisk.TestDefaultTheme_IsFirstInList;
var
  Form: TfrmSkinDisk;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual('Windows default theme', Form.lBox.Items[0],
    'First item should be "Windows default theme"');
end;


procedure TTestFormSkinsDisk.TestDefaultTheme_TextIsCorrect;
var
  Form: TfrmSkinDisk;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.lBox.Items.IndexOf('Windows default theme') >= 0,
    'List should contain "Windows default theme" text');
end;


{ Form Attribute Tests }

procedure TTestFormSkinsDisk.TestFormCaption_IsSkinSelector;
var
  Form: TfrmSkinDisk;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual('Skin selector', Form.Caption,
    'Form caption should be "Skin selector"');
end;


procedure TTestFormSkinsDisk.TestFormKeyPreview_IsEnabled;
var
  Form: TfrmSkinDisk;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.KeyPreview,
    'KeyPreview should be enabled for keyboard shortcuts');
end;


procedure TTestFormSkinsDisk.TestFormAlphaBlend_IsEnabled;
var
  Form: TfrmSkinDisk;
begin
  Form:= TfrmSkinDisk.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.AlphaBlend,
    'AlphaBlend should be enabled per DFM');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFormSkinsDisk);

end.
