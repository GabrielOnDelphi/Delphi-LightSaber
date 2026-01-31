unit Test.FormTranslEditor;

{=============================================================================================================
   Unit tests for FormTranslEditor.pas
   Tests TfrmTranslEditor - the translation editor form.

   Note: These tests focus on form creation, component existence, and basic behavior.
   Full translation functionality is tested separately in Test.LightVcl.Common.Translate.pas.

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
  Vcl.Mask;

type
  [TestFixture]
  TTestFormTranslEditor = class
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

    { Component Tests - Main Editor }
    [Test]
    procedure TestFormHasLangEditor;

    [Test]
    procedure TestFormHasValuesEditor;

    [Test]
    procedure TestFormHasFormsListBox;

    [Test]
    procedure TestFormHasInfoLabel;

    { Component Tests - Buttons }
    [Test]
    procedure TestFormHasCreateTranslButton;

    [Test]
    procedure TestFormHasLoadButton;

    [Test]
    procedure TestFormHasSaveButton;

    [Test]
    procedure TestFormHasApplyButton;

    [Test]
    procedure TestFormHasCopyButton;

    [Test]
    procedure TestFormHasValuesButton;

    [Test]
    procedure TestFormHasHelpButton;

    [Test]
    procedure TestFormHasOKButton;

    [Test]
    procedure TestFormHasCancelButton;

    { Component Tests - Options }
    [Test]
    procedure TestFormHasDontSaveEmptyCheckbox;

    [Test]
    procedure TestFormHasOverwriteCheckbox;

    [Test]
    procedure TestFormHasParseCtrlsCheckbox;

    { Component Tests - Input Fields }
    [Test]
    procedure TestFormHasFileNameEdit;

    [Test]
    procedure TestFormHasAuthorEdit;

    { Component Tests - Panels and Groups }
    [Test]
    procedure TestFormHasNewTranslGroup;

    [Test]
    procedure TestFormHasRightPanel;

    { Component Tests - Internet Links }
    [Test]
    procedure TestFormHasDeepLLabel;

    [Test]
    procedure TestFormHasGoogleLabel;

    { Form Attribute Tests }
    [Test]
    procedure TestFormAlphaBlend_IsEnabled;

    [Test]
    procedure TestFormShowHint_IsEnabled;

    { Button State Tests }
    [Test]
    procedure TestSaveButton_InitiallyDisabled;

    [Test]
    procedure TestApplyButton_InitiallyDisabled;

    { Group Visibility Tests }
    [Test]
    procedure TestNewTranslGroup_InitiallyHidden;

    [Test]
    procedure TestRightPanel_InitiallyHidden;

    { Event Handler Tests }
    [Test]
    procedure TestFormClose_SetsCaFree;

    [Test]
    procedure TestCreateTranslClick_ShowsGroup;

    [Test]
    procedure TestCancelClick_HidesGroup;

    [Test]
    procedure TestValuesClick_TogglesPanel;

    { Button Click Tests - No Exceptions }
    [Test]
    procedure TestCopyClick_NoException;

    [Test]
    procedure TestDeepLClick_NoException;

    [Test]
    procedure TestGoogleClick_NoException;

    { Live Forms Tests }
    [Test]
    procedure TestLiveFormsClick_PopulatesList;

    { Auto Translate (DeepL) Component Tests }
    [Test]
    procedure TestFormHasAutoTranslateGroup;

    [Test]
    procedure TestFormHasTargetLangCombo;

    [Test]
    procedure TestFormHasAutoTranslateButton;

    [Test]
    procedure TestFormHasDeepLSettingsButton;

    [Test]
    procedure TestTargetLangCombo_HasItems;

    [Test]
    procedure TestDeepLSettingsClick_NoException;
  end;

implementation

uses
  LightCore.AppData,
  LightVcl.Visual.AppData,
  LightVcl.Common.Translate,
  FormTranslEditor;


procedure TTestFormTranslEditor.Setup;
begin
  Assert.IsNotNull(AppData, 'AppData must be initialized before running tests');
  Assert.IsNotNull(Translator, 'Translator must be initialized before running tests');
  FTestForm:= NIL;
end;


procedure TTestFormTranslEditor.TearDown;
begin
  CleanupForm;
end;


procedure TTestFormTranslEditor.CleanupForm;
var
  Form: TfrmTranslEditor;
begin
  if FTestForm <> NIL then
  begin
    Form:= TfrmTranslEditor(FTestForm);
    FreeAndNil(Form);
    FTestForm:= NIL;
  end;
end;


{ Form Creation Tests }

procedure TTestFormTranslEditor.TestFormClassExists;
begin
  Assert.IsNotNull(TfrmTranslEditor, 'TfrmTranslEditor class should exist');
end;


procedure TTestFormTranslEditor.TestFormCreate_Succeeds;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form, 'Form creation should succeed');
end;


procedure TTestFormTranslEditor.TestFormCreate_WithNilOwner;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNull(Form.Owner, 'Owner should be nil when created with nil');
end;


{ Component Tests - Main Editor }

procedure TTestFormTranslEditor.TestFormHasLangEditor;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.mmoLangEditor, 'Form should have mmoLangEditor component');
  Assert.IsTrue(Form.mmoLangEditor is TMemo, 'mmoLangEditor should be a TMemo');
end;


procedure TTestFormTranslEditor.TestFormHasValuesEditor;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.mmoValues, 'Form should have mmoValues component');
  Assert.IsTrue(Form.mmoValues is TMemo, 'mmoValues should be a TMemo');
end;


procedure TTestFormTranslEditor.TestFormHasFormsListBox;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.lbxForms, 'Form should have lbxForms component');
  Assert.IsTrue(Form.lbxForms is TListBox, 'lbxForms should be a TListBox');
end;


procedure TTestFormTranslEditor.TestFormHasInfoLabel;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.lblInfo, 'Form should have lblInfo component');
end;


{ Component Tests - Buttons }

procedure TTestFormTranslEditor.TestFormHasCreateTranslButton;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnCreateTransl, 'Form should have btnCreateTransl button');
end;


procedure TTestFormTranslEditor.TestFormHasLoadButton;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnLoad, 'Form should have btnLoad button');
end;


procedure TTestFormTranslEditor.TestFormHasSaveButton;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnSaveEditor, 'Form should have btnSaveEditor button');
end;


procedure TTestFormTranslEditor.TestFormHasApplyButton;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnApplyEdits, 'Form should have btnApplyEdits button');
end;


procedure TTestFormTranslEditor.TestFormHasCopyButton;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnCopy, 'Form should have btnCopy button');
end;


procedure TTestFormTranslEditor.TestFormHasValuesButton;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnValues, 'Form should have btnValues button');
end;


procedure TTestFormTranslEditor.TestFormHasHelpButton;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnHelp, 'Form should have btnHelp button');
end;


procedure TTestFormTranslEditor.TestFormHasOKButton;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnOK, 'Form should have btnOK button');
end;


procedure TTestFormTranslEditor.TestFormHasCancelButton;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnCancel, 'Form should have btnCancel button');
end;


{ Component Tests - Options }

procedure TTestFormTranslEditor.TestFormHasDontSaveEmptyCheckbox;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.chkDontSaveEmpty, 'Form should have chkDontSaveEmpty checkbox');
end;


procedure TTestFormTranslEditor.TestFormHasOverwriteCheckbox;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.chkOverwrite, 'Form should have chkOverwrite checkbox');
end;


procedure TTestFormTranslEditor.TestFormHasParseCtrlsCheckbox;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.chkParseCtrlsAction, 'Form should have chkParseCtrlsAction checkbox');
end;


{ Component Tests - Input Fields }

procedure TTestFormTranslEditor.TestFormHasFileNameEdit;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.edtFileName, 'Form should have edtFileName edit');
  Assert.IsTrue(Form.edtFileName is TLabeledEdit, 'edtFileName should be a TLabeledEdit');
end;


procedure TTestFormTranslEditor.TestFormHasAuthorEdit;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.edtAuthor, 'Form should have edtAuthor edit');
  Assert.IsTrue(Form.edtAuthor is TLabeledEdit, 'edtAuthor should be a TLabeledEdit');
end;


{ Component Tests - Panels and Groups }

procedure TTestFormTranslEditor.TestFormHasNewTranslGroup;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.grpNewTransl, 'Form should have grpNewTransl group');
end;


procedure TTestFormTranslEditor.TestFormHasRightPanel;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.pnlRight, 'Form should have pnlRight panel');
end;


{ Component Tests - Internet Links }

procedure TTestFormTranslEditor.TestFormHasDeepLLabel;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.inetDeepL, 'Form should have inetDeepL label');
end;


procedure TTestFormTranslEditor.TestFormHasGoogleLabel;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.InternetLabel1, 'Form should have InternetLabel1 label');
end;


{ Form Attribute Tests }

procedure TTestFormTranslEditor.TestFormAlphaBlend_IsEnabled;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.AlphaBlend, 'AlphaBlend should be enabled');
end;


procedure TTestFormTranslEditor.TestFormShowHint_IsEnabled;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.ShowHint, 'ShowHint should be enabled');
end;


{ Button State Tests }

procedure TTestFormTranslEditor.TestSaveButton_InitiallyDisabled;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsFalse(Form.btnSaveEditor.Enabled,
    'Save button should be initially disabled');
end;


procedure TTestFormTranslEditor.TestApplyButton_InitiallyDisabled;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsFalse(Form.btnApplyEdits.Enabled,
    'Apply button should be initially disabled');
end;


{ Group Visibility Tests }

procedure TTestFormTranslEditor.TestNewTranslGroup_InitiallyHidden;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsFalse(Form.grpNewTransl.Visible,
    'New translation group should be initially hidden');
end;


procedure TTestFormTranslEditor.TestRightPanel_InitiallyHidden;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsFalse(Form.pnlRight.Visible,
    'Right panel (values) should be initially hidden');
end;


{ Event Handler Tests }

procedure TTestFormTranslEditor.TestFormClose_SetsCaFree;
var
  Form: TfrmTranslEditor;
  Action: TCloseAction;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Action:= caNone;
  Form.FormClose(Form, Action);

  Assert.AreEqual(caFree, Action, 'FormClose should set Action to caFree');
end;


procedure TTestFormTranslEditor.TestCreateTranslClick_ShowsGroup;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsFalse(Form.grpNewTransl.Visible, 'Group should start hidden');

  Form.btnCreateTranslClick(Form);

  Assert.IsTrue(Form.grpNewTransl.Visible,
    'CreateTransl click should show the new translation group');
end;


procedure TTestFormTranslEditor.TestCancelClick_HidesGroup;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Form.grpNewTransl.Visible:= TRUE;

  Form.btnCancelClick(Form);

  Assert.IsFalse(Form.grpNewTransl.Visible,
    'Cancel click should hide the new translation group');
end;


procedure TTestFormTranslEditor.TestValuesClick_TogglesPanel;
var
  Form: TfrmTranslEditor;
  InitialState: Boolean;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  InitialState:= Form.pnlRight.Visible;

  Form.btnValuesClick(Form);

  Assert.AreNotEqual(InitialState, Form.pnlRight.Visible,
    'Values click should toggle right panel visibility');
end;


{ Button Click Tests - No Exceptions }

procedure TTestFormTranslEditor.TestCopyClick_NoException;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Form.mmoLangEditor.Text:= 'Test content';

  { Copy click should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      Form.btnCopyClick(Form);
    end);
end;


procedure TTestFormTranslEditor.TestDeepLClick_NoException;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  { DeepL click should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      Form.inetDeepLClick(Form);
    end);
end;


procedure TTestFormTranslEditor.TestGoogleClick_NoException;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  { Google translate click should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      Form.InternetLabel1Click(Form);
    end);
end;


{ Live Forms Tests }

procedure TTestFormTranslEditor.TestLiveFormsClick_PopulatesList;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Form.lbxForms.Clear;

  Form.lblLiveFormsClick(Form);

  { Should have at least one form (the test form itself) }
  Assert.IsTrue(Form.lbxForms.Items.Count >= 1,
    'Live forms click should populate the list with at least one form');
end;


{ Auto Translate (DeepL) Component Tests }

procedure TTestFormTranslEditor.TestFormHasAutoTranslateGroup;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.grpAutoTranslate,
    'Form should have grpAutoTranslate group box');
end;


procedure TTestFormTranslEditor.TestFormHasTargetLangCombo;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.cmbTargetLang,
    'Form should have cmbTargetLang combo box');
  Assert.IsTrue(Form.cmbTargetLang is TComboBox,
    'cmbTargetLang should be a TComboBox');
end;


procedure TTestFormTranslEditor.TestFormHasAutoTranslateButton;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnAutoTranslate,
    'Form should have btnAutoTranslate button');
end;


procedure TTestFormTranslEditor.TestFormHasDeepLSettingsButton;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnDeepLSettings,
    'Form should have btnDeepLSettings button');
end;


procedure TTestFormTranslEditor.TestTargetLangCombo_HasItems;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  { The combo should be populated with supported languages after FormPostInitialize }
  Assert.IsTrue(Form.cmbTargetLang.Items.Count > 0,
    'Target language combo should have items after initialization');
  Assert.IsTrue(Form.cmbTargetLang.Items.Count >= 10,
    'Should have at least 10 supported languages');
end;


procedure TTestFormTranslEditor.TestDeepLSettingsClick_NoException;
var
  Form: TfrmTranslEditor;
begin
  Form:= TfrmTranslEditor.Create(NIL);
  FTestForm:= Form;

  { Note: This test cannot fully test the click because it opens a modal dialog.
    We just verify the button exists and the event handler is assigned. }
  Assert.IsTrue(Assigned(Form.btnDeepLSettings.OnClick),
    'btnDeepLSettings should have OnClick event handler assigned');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFormTranslEditor);

end.
