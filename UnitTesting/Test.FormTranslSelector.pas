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
    FCreatedLangFolder: Boolean;     { TRUE if Setup had to create the Lang folder }
    FCreatedFiles: TStringList;      { The language files Setup wrote; TearDown deletes exactly these }
    procedure CleanupForm;
    procedure WriteLangFile(CONST LangName, Author: string);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Form Creation Tests }
    [Test]
    procedure TestFormCreate_SetsTagAndButtons;

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
    procedure TestListBox_PopulatedOnCreate;

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
    procedure TestApplyLanguage_ReadsSelectedFilePath;

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
    procedure TestApplyLanguage_AddsIniExtension;
  end;

implementation

uses
  Winapi.Windows,
  LightCore.AppData,
  LightCore.IO,
  LightCore.TextFile,
  LightVcl.Visual.AppData,
  LightVcl.Common.Translate,
  FormTranslSelector;


CONST
  TestLangA = 'ZzTestLangA';
  TestLangB = 'ZzTestLangB';


{ Writes <Lang folder>\<LangName>.ini holding only the translator credits. An existing file is left alone. }
procedure TTestFormTranslSelector.WriteLangFile(CONST LangName, Author: string);
VAR
  FileName: string;
begin
  FileName:= AppData.Translator.GetLangFolder + LangName + '.ini';
  if FileExists(FileName) then EXIT;

  StringToFile(FileName, '[Authors]' + sLineBreak + 'Name=' + Author + sLineBreak);
  FCreatedFiles.Add(FileName);
end;


{ The form lists the .ini files of the Lang folder. That folder is usually missing on a test PC, so every
  test gets two language files of its own, and TearDown removes them again. }
procedure TTestFormTranslSelector.Setup;
VAR
  LangFolder: string;
begin
  Assert.IsNotNull(AppData, 'AppData must be initialized before running tests');
  Assert.IsNotNull(AppData.Translator, 'AppData.Translator must be initialized before running tests');
  FTestForm:= NIL;
  FCreatedFiles:= TStringList.Create;

  LangFolder:= AppData.Translator.GetLangFolder;
  FCreatedLangFolder:= NOT DirectoryExists(LangFolder);
  if FCreatedLangFolder
  then ForceDirectories(LangFolder);

  WriteLangFile(TestLangA, 'Test Author A');
  WriteLangFile(TestLangB, 'Test Author B');
end;


procedure TTestFormTranslSelector.TearDown;
VAR
  FileName: string;
begin
  CleanupForm;

  for FileName in FCreatedFiles DO
    System.SysUtils.DeleteFile(FileName);
  FreeAndNil(FCreatedFiles);

  if FCreatedLangFolder
  then RemoveDir(AppData.Translator.GetLangFolder);
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

{ The DFM leaves Tag at 0 and both buttons visible; FormCreate must change them }
procedure TTestFormTranslSelector.TestFormCreate_SetsTagAndButtons;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual(Integer(DontTranslate), Integer(Form.Tag), 'FormCreate must mark the form as DontTranslate');
  Assert.AreEqual(NOT AppData.RunningFirstTime, Form.btnTranslate.Visible, 'btnTranslate is hidden only on the first run');
  Assert.AreEqual(Form.btnTranslate.Visible, Form.btnRefresh.Visible, 'btnRefresh follows btnTranslate');
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


{ The handlers below are fired as a click would fire them. The ones that apply a language change the global
  translator, so those tests restore its language and credits afterwards. }

procedure TTestFormTranslSelector.TestListBox_HasClickHandler;
var
  Form: TfrmTranslSelector;
  OldLanguage, OldAuthors: string;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Assigned(Form.ListBox.OnClick),
    'ListBox should have OnClick handler assigned');

  OldLanguage:= AppData.Translator.CurLanguage;
  OldAuthors := AppData.Translator.Authors;
  TRY
    Form.ListBox.ItemIndex:= Form.ListBox.Items.IndexOf(TestLangB);
    Assert.IsTrue(Form.ListBox.ItemIndex >= 0, TestLangB + ' must be listed');

    { A single click applies the selected language at once }
    Form.ListBox.OnClick(Form.ListBox);

    Assert.AreEqual(TestLangB + '.ini', AppData.Translator.CurLanguageName, 'The click must apply the selected language');
    Assert.AreEqual('Translated by: Test Author B', Form.lblAuthors.Caption, 'The click must show the credits of the selected language');
    Assert.IsTrue(Form.lblAuthors.Visible, 'The credits label must become visible');
  FINALLY
    AppData.Translator.CurLanguage:= OldLanguage;
    AppData.Translator.Authors:= OldAuthors;
  END;
end;


procedure TTestFormTranslSelector.TestListBox_HasDblClickHandler;
var
  Form: TfrmTranslSelector;
  OldLanguage, OldAuthors: string;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Assigned(Form.ListBox.OnDblClick),
    'ListBox should have OnDblClick handler assigned');

  OldLanguage:= AppData.Translator.CurLanguage;
  OldAuthors := AppData.Translator.Authors;
  TRY
    Form.ListBox.ItemIndex:= Form.ListBox.Items.IndexOf(TestLangA);
    Assert.IsTrue(Form.ListBox.ItemIndex >= 0, TestLangA + ' must be listed');

    Form.ListBox.OnDblClick(Form.ListBox);

    Assert.AreEqual(TestLangA + '.ini', AppData.Translator.CurLanguageName, 'The double click must apply the selected language');
    Assert.AreEqual('Translated by: Test Author A', Form.lblAuthors.Caption, 'The double click must show the credits of the selected language');
  FINALLY
    AppData.Translator.CurLanguage:= OldLanguage;
    AppData.Translator.Authors:= OldAuthors;
  END;
end;


procedure TTestFormTranslSelector.TestListBox_PopulatedOnCreate;
var
  Form: TfrmTranslSelector;
  Files: TStringList;
  Expected, LastLang: Integer;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  { FormCreate calls PopulateLanguageFiles: one entry per .ini file in the Lang folder, without the extension }
  Files:= ListFilesOf(AppData.Translator.GetLangFolder, '*.ini', TRUE, FALSE);
  TRY
    { When Setup had to create the Lang folder, it holds exactly the 2 files Setup wrote }
    if FCreatedLangFolder
    then Expected:= 2
    else Expected:= Files.Count;
    Assert.AreEqual(Expected, Form.ListBox.Items.Count,
      'ListBox should hold one entry per language file');
    Assert.IsTrue(Form.ListBox.Items.IndexOf(TestLangA) >= 0, 'ListBox must list ' + TestLangA);
    Assert.IsTrue(Form.ListBox.Items.IndexOf(TestLangB) >= 0, 'ListBox must list ' + TestLangB);

    { Pre-selection: the language in use if it is listed, else the first entry }
    LastLang:= Form.ListBox.Items.IndexOf(ExtractOnlyName(AppData.Translator.CurLanguageName));
    if LastLang < 0
    then LastLang:= 0;
    Assert.AreEqual(LastLang, Form.ListBox.ItemIndex, 'The language in use, or the first entry, must be pre-selected');
  FINALLY
    FreeAndNil(Files);
  END;
end;


{ Button Tests }

procedure TTestFormTranslSelector.TestApplyButton_HasClickHandler;
var
  Form: TfrmTranslSelector;
  OldLanguage, OldAuthors: string;
  Msg: TMsg;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Assigned(Form.btnApplyLang.OnClick),
    'btnApplyLang should have OnClick handler assigned');

  OldLanguage:= AppData.Translator.CurLanguage;
  OldAuthors := AppData.Translator.Authors;
  TRY
    Form.ListBox.ItemIndex:= Form.ListBox.Items.IndexOf(TestLangB);
    Assert.IsTrue(Form.ListBox.ItemIndex >= 0, TestLangB + ' must be listed');

    { Apply = apply the language, then Close. Close on this form ends in Release (FormClose sets caFree),
      and Release posts CM_RELEASE (c:\Delphi\Delphi 13\source\vcl\Vcl.Forms.pas:9950). The test takes that
      message out of the queue, which proves Close ran and keeps the form alive for TearDown to free. }
    Form.btnApplyLang.OnClick(Form.btnApplyLang);

    Assert.AreEqual(TestLangB + '.ini', AppData.Translator.CurLanguageName, 'Apply must apply the selected language');
    Assert.IsTrue(PeekMessage(Msg, Form.Handle, CM_RELEASE, CM_RELEASE, PM_REMOVE), 'Apply must close the form (CM_RELEASE posted)');
  FINALLY
    AppData.Translator.CurLanguage:= OldLanguage;
    AppData.Translator.Authors:= OldAuthors;
  END;
end;


procedure TTestFormTranslSelector.TestRefreshButton_HasClickHandler;
var
  Form: TfrmTranslSelector;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Assigned(Form.btnRefresh.OnClick),
    'btnRefresh should have OnClick handler assigned');

  { Refresh must re-read the Lang folder: a stale entry goes, the language files come back }
  Form.ListBox.Items.Clear;
  Form.ListBox.Items.Add('Stale entry');
  Form.btnRefresh.OnClick(Form.btnRefresh);

  Assert.AreEqual(-1, Form.ListBox.Items.IndexOf('Stale entry'), 'Refresh must clear the old entries');
  Assert.IsTrue(Form.ListBox.Items.IndexOf(TestLangA) >= 0, 'Refresh must list ' + TestLangA);
  Assert.IsTrue(Form.ListBox.Items.IndexOf(TestLangB) >= 0, 'Refresh must list ' + TestLangB);
end;


{ The handler opens the translation editor form, so it is not fired here (no test may put a window on screen).
  The test checks instead that the button is wired to btnTranslateClick of this very form. }
procedure TTestFormTranslSelector.TestTranslateButton_HasClickHandler;
var
  Form: TfrmTranslSelector;
  Handler: TMethod;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Assigned(Form.btnTranslate.OnClick),
    'btnTranslate should have OnClick handler assigned');

  Handler:= TMethod(Form.btnTranslate.OnClick);
  Assert.IsTrue(Handler.Code = Form.MethodAddress('btnTranslateClick'), 'btnTranslate must call btnTranslateClick');
  Assert.IsTrue(Handler.Data = Pointer(Form), 'btnTranslate must call the handler of its own form');
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

  { Activating the form re-reads the Lang folder: a stale entry goes, the language files come back }
  Form.ListBox.Items.Clear;
  Form.ListBox.Items.Add('Stale entry');
  Form.OnActivate(Form);

  Assert.AreEqual(-1, Form.ListBox.Items.IndexOf('Stale entry'), 'OnActivate must clear the old entries');
  Assert.IsTrue(Form.ListBox.Items.IndexOf(TestLangA) >= 0, 'OnActivate must list ' + TestLangA);
  Assert.IsTrue(Form.ListBox.Items.IndexOf(TestLangB) >= 0, 'OnActivate must list ' + TestLangB);
  Assert.IsTrue(Form.ListBox.ItemIndex >= 0, 'OnActivate must pre-select a language');
end;


{ Utility Function Tests }

{ GetSelectedFileName and GetSelectedFilePath are private; their one caller is ApplyLanguage, which the
  published ListBoxDblClick runs. ApplyLanguage loads the file at GetSelectedFilePath into the global
  translator, so the two tests below restore the translator's language and credits afterwards. }

procedure TTestFormTranslSelector.TestApplyLanguage_ReadsSelectedFilePath;
var
  Form: TfrmTranslSelector;
  OldLanguage, OldAuthors: string;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  OldLanguage:= AppData.Translator.CurLanguage;
  OldAuthors := AppData.Translator.Authors;
  TRY
    Form.ListBox.ItemIndex:= Form.ListBox.Items.IndexOf(TestLangB);
    Assert.IsTrue(Form.ListBox.ItemIndex >= 0, TestLangB + ' must be listed');

    Form.ListBoxDblClick(Form);

    { The credits come from the [Authors] section of the file at GetSelectedFilePath }
    Assert.IsTrue(Form.lblAuthors.Visible, 'The selected language file must be found and applied');
    Assert.AreEqual('Translated by: Test Author B', Form.lblAuthors.Caption, 'Credits read from ' + TestLangB + '.ini');
  FINALLY
    AppData.Translator.CurLanguage:= OldLanguage;
    AppData.Translator.Authors:= OldAuthors;
  END;
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

  Form.ListBox.Items.Clear;   // FormCreate pre-populated it from the Lang folder
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

  Form.ListBox.Items.Clear;   // FormCreate pre-populated it from the Lang folder
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

  { Start from a known state, then add marker items }
  Form.ListBox.Items.Clear;   // FormCreate pre-populated it from the Lang folder
  Form.ListBox.Items.Add('TestItem1');
  Form.ListBox.Items.Add('TestItem2');

  Assert.AreEqual(2, Form.ListBox.Items.Count,
    'ListBox should have 2 items before test');

  { PopulateLanguageFiles requires the Translator (a TAppData field) - skip if not available }
  if AppData.Translator = NIL
  then Assert.Pass('Translator not initialized - skipping full test')
  else
    begin
      Form.PopulateLanguageFiles;
      { After populate, the old items must be gone (list cleared and repopulated) }
      Assert.IsTrue(Form.ListBox.Items.IndexOf('TestItem1') < 0,
        'PopulateLanguageFiles should clear the old items');
    end;
end;


{ Selection Tests }

procedure TTestFormTranslSelector.TestApplyLanguage_AddsIniExtension;
var
  Form: TfrmTranslSelector;
  OldLanguage, OldAuthors: string;
begin
  Form:= TfrmTranslSelector.Create(NIL);
  FTestForm:= Form;

  OldLanguage:= AppData.Translator.CurLanguage;
  OldAuthors := AppData.Translator.Authors;
  TRY
    { The list holds names without extension; GetSelectedFileName must add '.ini' }
    Form.ListBox.ItemIndex:= Form.ListBox.Items.IndexOf(TestLangA);
    Assert.IsTrue(Form.ListBox.ItemIndex >= 0, TestLangA + ' must be listed');

    Form.ListBoxDblClick(Form);

    Assert.AreEqual(TestLangA + '.ini', AppData.Translator.CurLanguageName, 'The translator must get the file name with .ini');
    Assert.AreEqual(AppData.Translator.GetLangFolder + TestLangA + '.ini', AppData.Translator.CurLanguage, 'Full path in the Lang folder');
  FINALLY
    AppData.Translator.CurLanguage:= OldLanguage;
    AppData.Translator.Authors:= OldAuthors;
  END;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFormTranslSelector);

end.
