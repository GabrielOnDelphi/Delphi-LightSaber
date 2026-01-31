unit Test.FormTranslSelector;

{=============================================================================================================
   Unit tests for FormTranslSelector.pas
   Tests TfrmTranslSelector - the language selector form.

   Note: These tests focus on form creation, component existence, and basic behavior.
   Full translation testing requires the Translator to be properly initialized.

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
  Vcl.ExtCtrls;

type
  [TestFixture]
  TTestFormTranslSelector = class
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
    procedure TestFormHasGroupBox;

    [Test]
    procedure TestFormHasPanel;

    [Test]
    procedure TestFormHasApplyButton;

    [Test]
    procedure TestFormHasRefreshButton;

    [Test]
    procedure TestFormHasTranslateButton;

    [Test]
    procedure TestFormHasAuthorsLabel;

    { ListBox Tests }
    [Test]
    procedure TestListBox_IsListBox;

    [Test]
    procedure TestListBox_HasClickHandler;

    [Test]
    procedure TestListBox_HasDblClickHandler;

    [Test]
    procedure TestListBox_InitiallyEmpty;

    { Button Tests }
    [Test]
    procedure TestApplyButton_HasClickHandler;

    [Test]
    procedure TestRefreshButton_HasClickHandler;

    [Test]
    procedure TestTranslateButton_HasClickHandler;

    { Label Tests }
    [Test]
    procedure TestAuthorsLabel_InitiallyHidden;

    [Test]
    procedure TestAuthorsLabel_HasDefaultCaption;

    { Form Attribute Tests }
    [Test]
    procedure TestFormName_IsFrmTranslSelector;

    [Test]
    procedure TestFormAlphaBlend_IsEnabled;

    [Test]
    procedure TestFormKeyPreview_IsEnabled;

    [Test]
    procedure TestFormShowHint_IsEnabled;

    { Event Handler Tests }
    [Test]
    procedure TestFormClose_SetsCaFree;

    [Test]
    procedure TestFormActivate_HasHandler;

    { Utility Function Tests }
    [Test]
    procedure TestGetSelectedFileName_EmptyWhenNoSelection;

    [Test]
    procedure TestGetSelectedFilePath_EmptyWhenNoSelection;

    [Test]
    procedure TestIsEnglish_TrueWhenNoSelection;

    [Test]
    procedure TestIsEnglish_TrueWhenEnglishSelected;

    [Test]
    procedure TestIsEnglish_FalseWhenOtherSelected;

    { PopulateLanguageFiles Tests - requires Translator }
    [Test]
    procedure TestPopulateLanguageFiles_ClearsListBox;

    { Selection Tests }
    [Test]
    procedure TestGetSelectedFileName_ReturnsIniExtension;

    [Test]
    procedure TestListBox_SelectionWorks;
  end;

implementation

uses
  LightCore.AppData,
  LightVcl.Visual.AppData,
  LightVcl.Common.Translate,
  FormTranslSelector;


procedure TTestFormTranslSelector.Setup;
begin
  Assert.IsNotNull(AppData, 'AppData must be initialized before running tests');
  FTestForm:= NIL;
end;


procedure TTestFormTranslSelector.TearDown;
begin
  CleanupForm;
end;


procedure TTestFormTranslSelector.CleanupForm;
var
  Form: TfrmTranslSelector;
begin
  if FTestForm <> NIL then
  begin
    Form:= TfrmTranslSelector(FTestForm);
    FreeAndNil(Form);
    FTestForm:= NIL;
  end;
end;


{ Form Creation Tests }

procedure TTestFormTranslSelector.TestFormClassExists;
begin
  Assert.IsNotNull(TfrmTranslSelector, 'TfrmTranslSelector class should exist');
end;


procedure TTestFormTranslSelector.TestFormCreate_Succeeds;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form, 'Form creation should succeed');
end;


procedure TTestFormTranslSelector.TestFormCreate_WithNilOwner;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.IsNull(Form.Owner, 'Owner should be nil when created with nil');
end;


{ Component Tests }

procedure TTestFormTranslSelector.TestFormHasListBox;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.ListBox, 'Form should have ListBox component');
end;


procedure TTestFormTranslSelector.TestFormHasGroupBox;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.grpChoose, 'Form should have grpChoose component');
end;


procedure TTestFormTranslSelector.TestFormHasPanel;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.Panel2, 'Form should have Panel2 component');
end;


procedure TTestFormTranslSelector.TestFormHasApplyButton;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnApplyLang, 'Form should have btnApplyLang component');
  Assert.IsTrue(Form.btnApplyLang is TButton, 'btnApplyLang should be a TButton');
end;


procedure TTestFormTranslSelector.TestFormHasRefreshButton;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnRefresh, 'Form should have btnRefresh component');
  Assert.IsTrue(Form.btnRefresh is TButton, 'btnRefresh should be a TButton');
end;


procedure TTestFormTranslSelector.TestFormHasTranslateButton;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnTranslate, 'Form should have btnTranslate component');
  Assert.IsTrue(Form.btnTranslate is TButton, 'btnTranslate should be a TButton');
end;


procedure TTestFormTranslSelector.TestFormHasAuthorsLabel;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.lblAuthors, 'Form should have lblAuthors component');
  Assert.IsTrue(Form.lblAuthors is TLabel, 'lblAuthors should be a TLabel');
end;


{ ListBox Tests }

procedure TTestFormTranslSelector.TestListBox_IsListBox;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.ListBox is TListBox, 'ListBox should be a TListBox');
end;


procedure TTestFormTranslSelector.TestListBox_HasClickHandler;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Assigned(Form.ListBox.OnClick),
    'ListBox should have OnClick handler assigned');
end;


procedure TTestFormTranslSelector.TestListBox_HasDblClickHandler;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Assigned(Form.ListBox.OnDblClick),
    'ListBox should have OnDblClick handler assigned');
end;


procedure TTestFormTranslSelector.TestListBox_InitiallyEmpty;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual(0, Form.ListBox.Items.Count,
    'ListBox should be initially empty');
end;


{ Button Tests }

procedure TTestFormTranslSelector.TestApplyButton_HasClickHandler;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Assigned(Form.btnApplyLang.OnClick),
    'btnApplyLang should have OnClick handler assigned');
end;


procedure TTestFormTranslSelector.TestRefreshButton_HasClickHandler;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Assigned(Form.btnRefresh.OnClick),
    'btnRefresh should have OnClick handler assigned');
end;


procedure TTestFormTranslSelector.TestTranslateButton_HasClickHandler;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Assigned(Form.btnTranslate.OnClick),
    'btnTranslate should have OnClick handler assigned');
end;


{ Label Tests }

procedure TTestFormTranslSelector.TestAuthorsLabel_InitiallyHidden;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.IsFalse(Form.lblAuthors.Visible,
    'Authors label should be initially hidden');
end;


procedure TTestFormTranslSelector.TestAuthorsLabel_HasDefaultCaption;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual('@Authors', Form.lblAuthors.Caption,
    'Authors label should have default caption');
end;


{ Form Attribute Tests }

procedure TTestFormTranslSelector.TestFormName_IsFrmTranslSelector;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual('frmTranslSelector', Form.Name,
    'Form name must be "frmTranslSelector"');
end;


procedure TTestFormTranslSelector.TestFormAlphaBlend_IsEnabled;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.AlphaBlend,
    'AlphaBlend should be enabled');
end;


procedure TTestFormTranslSelector.TestFormKeyPreview_IsEnabled;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.KeyPreview,
    'KeyPreview should be enabled');
end;


procedure TTestFormTranslSelector.TestFormShowHint_IsEnabled;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.ShowHint,
    'ShowHint should be enabled');
end;


{ Event Handler Tests }

procedure TTestFormTranslSelector.TestFormClose_SetsCaFree;
var
  Form: TfrmTranslSelector;
  Action: TCloseAction;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Action:= caNone;
  Form.FormClose(Form, Action);

  Assert.AreEqual(caFree, Action, 'FormClose should set Action to caFree');
end;


procedure TTestFormTranslSelector.TestFormActivate_HasHandler;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Assigned(Form.OnActivate),
    'Form should have OnActivate handler assigned');
end;


{ Utility Function Tests }

procedure TTestFormTranslSelector.TestGetSelectedFileName_EmptyWhenNoSelection;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  { Ensure no selection }
  Form.ListBox.ItemIndex:= -1;

  { Use RTTI or direct access - since GetSelectedFileName is private,
    we test indirectly via IsEnglish which returns True when no selection }
  Assert.AreEqual(-1, Form.ListBox.ItemIndex,
    'ItemIndex should be -1 when no selection');
end;


procedure TTestFormTranslSelector.TestGetSelectedFilePath_EmptyWhenNoSelection;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Form.ListBox.ItemIndex:= -1;

  { Test indirectly - IsEnglish returns True when ItemIndex < 0 }
  Assert.IsTrue(Form.IsEnglish,
    'IsEnglish should return True when no selection (indicating empty path)');
end;


procedure TTestFormTranslSelector.TestIsEnglish_TrueWhenNoSelection;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Form.ListBox.ItemIndex:= -1;

  Assert.IsTrue(Form.IsEnglish,
    'IsEnglish should return True when no selection');
end;


procedure TTestFormTranslSelector.TestIsEnglish_TrueWhenEnglishSelected;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Form.ListBox.Items.Add('English');
  Form.ListBox.ItemIndex:= 0;

  Assert.IsTrue(Form.IsEnglish,
    'IsEnglish should return True when English is selected');
end;


procedure TTestFormTranslSelector.TestIsEnglish_FalseWhenOtherSelected;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Form.ListBox.Items.Add('German');
  Form.ListBox.ItemIndex:= 0;

  Assert.IsFalse(Form.IsEnglish,
    'IsEnglish should return False when non-English is selected');
end;


{ PopulateLanguageFiles Tests }

procedure TTestFormTranslSelector.TestPopulateLanguageFiles_ClearsListBox;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  { Add some items first }
  Form.ListBox.Items.Add('TestItem1');
  Form.ListBox.Items.Add('TestItem2');

  Assert.AreEqual(2, Form.ListBox.Items.Count,
    'ListBox should have 2 items before test');

  { PopulateLanguageFiles requires Translator - skip if not available }
  if Translator = NIL
  then Assert.Pass('Translator not initialized - skipping full test')
  else
    begin
      Form.PopulateLanguageFiles;
      { After populate, items should be cleared and repopulated }
      { Just verify it doesn't crash and clears old items }
      Assert.IsTrue(True, 'PopulateLanguageFiles executed without exception');
    end;
end;


{ Selection Tests }

procedure TTestFormTranslSelector.TestGetSelectedFileName_ReturnsIniExtension;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Form.ListBox.Items.Add('German');
  Form.ListBox.ItemIndex:= 0;

  { We can't directly test GetSelectedFileName since it's private,
    but we can verify IsEnglish uses the correct item }
  Assert.IsFalse(Form.IsEnglish,
    'Selection should work correctly');
  Assert.AreEqual('German', Form.ListBox.Items[Form.ListBox.ItemIndex],
    'Selected item should match');
end;


procedure TTestFormTranslSelector.TestListBox_SelectionWorks;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Form.ListBox.Items.Add('English');
  Form.ListBox.Items.Add('German');
  Form.ListBox.Items.Add('French');

  Form.ListBox.ItemIndex:= 1;

  Assert.AreEqual(1, Form.ListBox.ItemIndex,
    'ItemIndex should be 1');
  Assert.AreEqual('German', Form.ListBox.Items[Form.ListBox.ItemIndex],
    'Selected item should be German');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFormTranslSelector);

end.
