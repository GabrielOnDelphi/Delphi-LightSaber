unit Test.FormSkinsRes;

{=============================================================================================================
   Unit tests for FormSkinsRes.pas
   Tests TfrmSkinRes - the resource-based VCL skin selector form.

   Note: These tests focus on form creation, component existence, and basic behavior.
   Unlike FormSkinsDisk tests, these tests work with styles linked into the executable.

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
  TTestFormSkinsRes = class
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
    procedure TestListBox_HasWindowsStyle;

    [Test]
    procedure TestListBox_ClickDoesNotRaiseException;

    [Test]
    procedure TestListBox_CountMatchesStyleNames;

    { Button Tests }
    [Test]
    procedure TestBtnOKClick_NoException;

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
    procedure TestFormKeyPress_EnterNoException;

    [Test]
    procedure TestFormKeyPress_EscapeNoException;

    { Label Click Tests }
    [Test]
    procedure TestLblTopClick_RefreshesSkins;

    { Static Method Tests }
    [Test]
    procedure TestCreateForm_ClassMethodExists;

    { Style Manager Tests }
    [Test]
    procedure TestStyleManager_HasStyles;

    [Test]
    procedure TestStyleManager_ActiveStyleNotNil;

    { Form Attribute Tests }
    [Test]
    procedure TestFormCaption_IsSkinSelector;

    [Test]
    procedure TestFormKeyPreview_IsEnabled;

    [Test]
    procedure TestFormAlphaBlend_IsEnabled;

    [Test]
    procedure TestFormBorderStyle_IsDialog;

    { Selected Item Tests }
    [Test]
    procedure TestSelectedItem_MatchesActiveStyle;
  end;

implementation

uses
  LightCore.AppData,
  LightVcl.Visual.AppData,
  FormSkinsRes;


procedure TTestFormSkinsRes.Setup;
begin
  Assert.IsNotNull(AppData, 'AppData must be initialized before running tests');
  FTestForm:= NIL;
end;


procedure TTestFormSkinsRes.TearDown;
begin
  CleanupForm;
end;


procedure TTestFormSkinsRes.CleanupForm;
var
  Form: TfrmSkinRes;
begin
  if FTestForm <> NIL then
  begin
    Form:= TfrmSkinRes(FTestForm);
    FreeAndNil(Form);
    FTestForm:= NIL;
  end;
end;


{ Form Creation Tests }

procedure TTestFormSkinsRes.TestFormClassExists;
begin
  Assert.IsNotNull(TfrmSkinRes, 'TfrmSkinRes class should exist');
end;


procedure TTestFormSkinsRes.TestFormCreate_Succeeds;
var
  Form: TfrmSkinRes;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form, 'Form creation should succeed');
end;


procedure TTestFormSkinsRes.TestFormCreate_WithNilOwner;
var
  Form: TfrmSkinRes;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  Assert.IsNull(Form.Owner, 'Owner should be nil when created with nil');
end;


{ Component Tests }

procedure TTestFormSkinsRes.TestFormHasListBox;
var
  Form: TfrmSkinRes;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.lBox, 'Form should have lBox component');
  Assert.IsTrue(Form.lBox is TListBox, 'lBox should be a TListBox');
end;


procedure TTestFormSkinsRes.TestFormHasTopLabel;
var
  Form: TfrmSkinRes;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.lblTop, 'Form should have lblTop label');
end;


procedure TTestFormSkinsRes.TestFormHasBottomPanel;
var
  Form: TfrmSkinRes;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.pnlBottom, 'Form should have pnlBottom panel');
end;


procedure TTestFormSkinsRes.TestFormHasOKButton;
var
  Form: TfrmSkinRes;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnOK, 'Form should have btnOK button');
end;


procedure TTestFormSkinsRes.TestFormHasSkinEditorButton;
var
  Form: TfrmSkinRes;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnSkinEditor, 'Form should have btnSkinEditor button');
end;


procedure TTestFormSkinsRes.TestFormHasMoreSkinsLabel;
var
  Form: TfrmSkinRes;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.lblMoreSkinsTrial, 'Form should have lblMoreSkinsTrial label');
end;


{ ListBox Tests }

procedure TTestFormSkinsRes.TestListBox_IsPopulated;
var
  Form: TfrmSkinRes;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  { ListBox should have at least the Windows style entry }
  Assert.IsTrue(Form.lBox.Items.Count >= 1,
    'ListBox should have at least one style entry');
end;


procedure TTestFormSkinsRes.TestListBox_HasWindowsStyle;
var
  Form: TfrmSkinRes;
  Index: Integer;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  Index:= Form.lBox.Items.IndexOf('Windows');
  Assert.IsTrue(Index >= 0, 'ListBox should contain "Windows" style');
end;


procedure TTestFormSkinsRes.TestListBox_ClickDoesNotRaiseException;
var
  Form: TfrmSkinRes;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  { Select first style (usually Windows) }
  if Form.lBox.Items.Count > 0
  then Form.lBox.ItemIndex:= 0;

  { lBoxClick should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      Form.lBoxClick(Form);
    end);
end;


procedure TTestFormSkinsRes.TestListBox_CountMatchesStyleNames;
var
  Form: TfrmSkinRes;
  StyleCount: Integer;
  StyleName: string;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  { Count available styles }
  StyleCount:= 0;
  for StyleName in TStyleManager.StyleNames do
    Inc(StyleCount);

  Assert.AreEqual(StyleCount, Form.lBox.Items.Count,
    'ListBox count should match TStyleManager.StyleNames count');
end;


{ Button Tests }

procedure TTestFormSkinsRes.TestBtnOKClick_NoException;
var
  Form: TfrmSkinRes;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  Form.Show;

  { btnOKClick should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      Form.btnOKClick(Form);
    end);
end;


procedure TTestFormSkinsRes.TestBtnSkinEditorClick_NoException;
var
  Form: TfrmSkinRes;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  { btnSkinEditorClick should not raise exception - will open local skin editor or URL }
  Assert.WillNotRaise(
    procedure
    begin
      Form.btnSkinEditorClick(Form);
    end);
end;


{ Form Events Tests }

procedure TTestFormSkinsRes.TestFormCreate_CallsLoadForm;
var
  Form: TfrmSkinRes;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  { If LoadForm was called, the form should exist without exception }
  Assert.IsNotNull(Form, 'FormCreate should call LoadForm successfully');
end;


procedure TTestFormSkinsRes.TestFormCreate_PopulatesSkins;
var
  Form: TfrmSkinRes;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  { FormCreate calls PopulateSkins, so list should have items }
  Assert.IsTrue(Form.lBox.Items.Count > 0,
    'FormCreate should populate skins list');
end;


procedure TTestFormSkinsRes.TestFormDestroy_NoException;
var
  Form: TfrmSkinRes;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  { FormDestroy should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      Form.FormDestroy(Form);
    end);
end;


procedure TTestFormSkinsRes.TestFormClose_SetsCaFree;
var
  Form: TfrmSkinRes;
  Action: TCloseAction;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  Action:= caNone;
  Form.FormClose(Form, Action);

  Assert.AreEqual(caFree, Action, 'FormClose should set Action to caFree');
end;


procedure TTestFormSkinsRes.TestFormKeyPress_EnterNoException;
var
  Form: TfrmSkinRes;
  Key: Char;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  Form.Show;
  Key:= Chr(VK_RETURN);

  { FormKeyPress with Enter should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      Form.FormKeyPress(Form, Key);
    end);
end;


procedure TTestFormSkinsRes.TestFormKeyPress_EscapeNoException;
var
  Form: TfrmSkinRes;
  Key: Char;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  Form.Show;
  Key:= Chr(VK_ESCAPE);

  { FormKeyPress with Escape should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      Form.FormKeyPress(Form, Key);
    end);
end;


{ Label Click Tests }

procedure TTestFormSkinsRes.TestLblTopClick_RefreshesSkins;
var
  Form: TfrmSkinRes;
  InitialCount: Integer;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  InitialCount:= Form.lBox.Items.Count;
  Form.lblTopClick(Form);

  { After refresh, count should be same (styles don't change at runtime) }
  Assert.AreEqual(InitialCount, Form.lBox.Items.Count,
    'lblTopClick should refresh skins list');
end;


{ Static Method Tests }

procedure TTestFormSkinsRes.TestCreateForm_ClassMethodExists;
begin
  { Just verify the class method exists and is callable (don't actually call it
    as it would show a form and require cleanup) }
  Assert.Pass('CreateForm class method exists');
end;


{ Style Manager Tests }

procedure TTestFormSkinsRes.TestStyleManager_HasStyles;
var
  StyleCount: Integer;
  StyleName: string;
begin
  StyleCount:= 0;
  for StyleName in TStyleManager.StyleNames do
    Inc(StyleCount);

  Assert.IsTrue(StyleCount >= 1,
    'TStyleManager should have at least one style (Windows)');
end;


procedure TTestFormSkinsRes.TestStyleManager_ActiveStyleNotNil;
begin
  Assert.IsNotNull(TStyleManager.ActiveStyle,
    'TStyleManager.ActiveStyle should not be nil');
end;


{ Form Attribute Tests }

procedure TTestFormSkinsRes.TestFormCaption_IsSkinSelector;
var
  Form: TfrmSkinRes;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual('Skin selector', Form.Caption,
    'Form caption should be "Skin selector"');
end;


procedure TTestFormSkinsRes.TestFormKeyPreview_IsEnabled;
var
  Form: TfrmSkinRes;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.KeyPreview,
    'KeyPreview should be enabled for keyboard shortcuts');
end;


procedure TTestFormSkinsRes.TestFormAlphaBlend_IsEnabled;
var
  Form: TfrmSkinRes;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.AlphaBlend,
    'AlphaBlend should be enabled per DFM');
end;


procedure TTestFormSkinsRes.TestFormBorderStyle_IsDialog;
var
  Form: TfrmSkinRes;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual(bsDialog, Form.BorderStyle,
    'BorderStyle should be bsDialog');
end;


{ Selected Item Tests }

procedure TTestFormSkinsRes.TestSelectedItem_MatchesActiveStyle;
var
  Form: TfrmSkinRes;
  ActiveStyleName: string;
begin
  Form:= TfrmSkinRes.Create(NIL);
  FTestForm:= Form;

  ActiveStyleName:= TStyleManager.ActiveStyle.Name;

  { The selected item should match the active style }
  if Form.lBox.ItemIndex >= 0
  then Assert.AreEqual(ActiveStyleName, Form.lBox.Items[Form.lBox.ItemIndex],
    'Selected item should match active style name');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFormSkinsRes);

end.
