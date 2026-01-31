UNIT FormTranslEditor;

{=============================================================================================================
   2026.01.31
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   TRANSLATION MANAGER FORM

   Main form for creating and managing translation files.
   Works with LightVcl.Translate.pas translation engine.

   Features:
     - Create new translation files from live forms
     - Auto-translate using DeepL API
     - Opens INI editor for manual review/editing

   USAGE:
     Call TfrmTranslEditor.ShowEditor to display the translation manager.

   NOTES:
     - THIS FORM IS NOT SELF TRANSLATED (Tag=128)

   RELATED:
     - LightVcl.Translate.pas - Translation engine
     - FormTranslatorIniEditor.pas - INI file editor
     - FormTranslSelector.pas - Language selector form
=============================================================================================================}

INTERFACE
{$DENYPACKAGEUNIT ON}

USES
  System.SysUtils, System.Classes, System.IOUtils,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Mask,
  LightCore, LightCore.Time, LightCore.Types, LightCore.AppData,
  LightVcl.Translate, LightVcl.TranslatorAPI, LightVcl.Visual.AppDataForm, LightVcl.Visual.AppData, LightVcl.Common.Dialogs, LightVcl.Visual.Panel,
  LightVcl.Visual.GroupBox, LightVcl.Visual.DropDownSearch, Vcl.WinXCtrls;

TYPE
  TfrmTranslEditor = class(TLightForm)
    lblInfo: TLabel;
    GroupBox1: TGroupBox;
    btnLoadTranslation: TButton;
    CubicGroupBox2: TCubicGroupBox;
    lbxForms: TListBox;
    GroupBox3: TGroupBox;
    InternetLabel1: TLabel;
    inetDeepL: TLabel;
    btnHelp: TButton;
    chkTranslateTranslator: TCheckBox;
    CubicGroupBox3: TCubicGroupBox;
    lblTargetLang: TLabel;
    sbxTargetLang: TDropDownSearchBox;
    edtAuthor: TLabeledEdit;
    btnAutoTranslate: TButton;
    btnManualTranslate: TButton;
    btnDeepLSettings: TButton;
    GroupBox2: TGroupBox;
    chkParseCtrlsAction: TCheckBox;
    chkDontSaveEmpty: TCheckBox;
    chkOverwrite: TCheckBox;
    procedure btnManualTranslateClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnLoadTranslationClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lblLiveFormsClick(Sender: TObject);
    procedure inetDeepLClick(Sender: TObject);
    procedure InternetLabel1Click(Sender: TObject);
    procedure btnAutoTranslateClick(Sender: TObject);
    procedure btnDeepLSettingsClick(Sender: TObject);
    procedure chkTranslateTranslatorClick(Sender: TObject);
  private
    FSourceFile: string;  { Source file for auto-translation }
    procedure PopulateLanguageList;
    function GetTargetFileName: string;
    procedure ApplyOptionsToTranslator;
    procedure ShowIniEditor(const FileName: string);
  public
    class procedure ShowEditor; static;
    procedure FormPostInitialize; override;
  end;



IMPLEMENTATION {$R *.dfm}

USES
   LightVcl.Common.ExecuteShell, LightCore.IO, LightCore.TextFile, LightVcl.Common.IO,
   FormTranslDeepL, FormTranslatorIniEditor;


{ Shows the translation manager form }
class procedure TfrmTranslEditor.ShowEditor;
var
  frmEditor: TfrmTranslEditor;
begin
  AppData.CreateForm(TfrmTranslEditor, frmEditor);
end;


procedure TfrmTranslEditor.FormPostInitialize;
begin
  inherited FormPostInitialize;
  Assert(Translator <> NIL, 'Translator must be initialized before using translation editor');
  lblLiveFormsClick(Self);  { Populate live forms list }
  PopulateLanguageList;

  { Default source file is the current language }
  FSourceFile:= Translator.CurLanguage;
  if FSourceFile <> ''
  then lblInfo.Caption:= 'Source: ' + ExtractFileName(FSourceFile)
  else lblInfo.Caption:= 'No source file loaded';
  lblInfo.Visible:= TRUE;
end;


procedure TfrmTranslEditor.PopulateLanguageList;
var
  LangList: TStringList;
begin
  LangList:= TStringList.Create;
  TRY
    { Add all DeepL supported languages }
    for var Lang in GetSupportedLanguages do
      LangList.Add(Lang);

    { Add existing translation files that might have custom language names }
    if DirectoryExists(Translator.GetLangFolder)
    then
      for var FileName in TDirectory.GetFiles(Translator.GetLangFolder, '*.ini') do
        begin
          var LangName:= TPath.GetFileNameWithoutExtension(FileName);
          if LangList.IndexOf(LangName) < 0
          then LangList.Add(LangName);
        end;

    LangList.Sort;
    sbxTargetLang.PopulateDictionary(LangList);
  FINALLY
    FreeAndNil(LangList);
  END;
end;


procedure TfrmTranslEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= TCloseAction.caFree;
end;


procedure TfrmTranslEditor.ApplyOptionsToTranslator;
begin
  Translator.ParseCtrlsWithAction:= chkParseCtrlsAction.Checked;
  Translator.DontSaveEmpty:= chkDontSaveEmpty.Checked;
  Translator.Authors:= edtAuthor.Text;
end;


function TfrmTranslEditor.GetTargetFileName: string;
begin
  Result:= Translator.GetLangFolder + ForceExtension(Trim(sbxTargetLang.Text), '.ini');
end;


procedure TfrmTranslEditor.ShowIniEditor(const FileName: string);
begin
  TfrmTranslatorIniEditor.ShowEditor(FileName);
end;




{-------------------------------------------------------------------------------------------------------------
   CREATE NEW TRANSLATION (Extract from live forms)
-------------------------------------------------------------------------------------------------------------}

procedure TfrmTranslEditor.btnManualTranslateClick(Sender: TObject);
VAR
  TargetFile: string;
  s: string;
begin
  { Validate target language }
  if Trim(sbxTargetLang.Text) = '' then
    begin
      MessageWarning('Please enter or select a target language.');
      EXIT;
    end;

  TargetFile:= GetTargetFileName;

  if NOT System.IOUtils.TPath.HasValidPathChars(TargetFile, FALSE) then
    begin
      MessageError('The file name has invalid characters: ' + TargetFile);
      EXIT;
    end;

  { Warn if file exists }
  if FileExists(TargetFile) then
    if NOT MesajYesNo('File already exists: ' + ExtractFileName(TargetFile) + CRLF + 'Overwrite?')
    then EXIT;

  { Backup existing file }
  if FileExists(TargetFile)
  then BackupFileIncrement(TargetFile, Translator.GetLangFolder + 'Backups\');

  { Apply options }
  ApplyOptionsToTranslator;

  { Create translation file from live forms }
  Translator.SaveTranslation(TargetFile, chkOverwrite.Checked);

  { Format the file for better readability }
  s:= StringFromFile(TargetFile);
  s:= ReplaceString(s, CRLF+'[', CRLF+CRLF+'[');
  StringToFile(TargetFile, s, woOverwrite, wpOn);

  { GUI feedback }
  lblInfo.Caption:= 'Created: ' + ExtractFileName(TargetFile);

  { Open in editor }
  ShowIniEditor(TargetFile);
end;




{-------------------------------------------------------------------------------------------------------------
   LOAD SOURCE FILE
-------------------------------------------------------------------------------------------------------------}

procedure TfrmTranslEditor.btnLoadTranslationClick(Sender: TObject);
begin
  VAR sTempFile:= FSourceFile;

  if PromptToLoadFile(sTempFile, FilterTransl, 'Open a source translation file...')
  then
    begin
      FSourceFile:= sTempFile;
      lblInfo.Caption:= 'Source: ' + ExtractFileName(FSourceFile);
    end;
end;




{-------------------------------------------------------------------------------------------------------------
   AUTO TRANSLATE (DEEPL)
-------------------------------------------------------------------------------------------------------------}

procedure TfrmTranslEditor.btnDeepLSettingsClick(Sender: TObject);
begin
  TfrmDeepLSettings.ShowSettings;
end;


procedure TfrmTranslEditor.btnAutoTranslateClick(Sender: TObject);
var
  DeepL: TDeepLTranslator;
  TargetLang: string;
  TargetLangCode: string;
  TargetFile: string;
  CharCount: Integer;
begin
  { Validate source file }
  if (FSourceFile = '') OR NOT FileExists(FSourceFile) then
    begin
      MessageWarning('Please load a source translation file first.');
      EXIT;
    end;

  { Validate target language }
  TargetLang:= Trim(sbxTargetLang.Text);
  if TargetLang = '' then
    begin
      MessageWarning('Please enter or select a target language.');
      EXIT;
    end;

  TargetLangCode:= LanguageNameToDeepL(TargetLang);
  if TargetLangCode = '' then
    begin
      MessageWarning('Language "' + TargetLang + '" is not supported by DeepL.' + CRLF +
                     'Use "Extract from live forms" for manual translation.');
      EXIT;
    end;

  { Check API key }
  if DeepL_GetApiKey.Trim.IsEmpty then
    begin
      MessageWarning('Please configure your DeepL API key first.');
      TfrmDeepLSettings.ShowSettings;
      EXIT;
    end;

  { Estimate characters }
  CharCount:= Length(StringFromFile(FSourceFile));
  if NOT MesajYesNo(Format(
    'Translate to %s?'+ CRLF+ CRLF+
    'Source: %s'+ CRLF+
    'Estimated characters: %d'+ CRLF+
    'This will use your DeepL API quota.', [TargetLang, ExtractFileName(FSourceFile), CharCount]))
  then EXIT;

  { Create target file path }
  TargetFile:= GetTargetFileName;

  { Warn if file exists }
  if FileExists(TargetFile) then
    if NOT MesajYesNo('File already exists: ' + ExtractFileName(TargetFile) + CRLF + 'Overwrite?')
    then EXIT;

  { Apply options }
  ApplyOptionsToTranslator;

  { Perform translation }
  Screen.Cursor:= crHourGlass;
  DeepL:= TDeepLTranslator.Create;
  try
    DeepL.ApiKey:= DeepL_GetApiKey;
    DeepL.UseFreeAPI:= DeepL_GetUseFreeAPI;

    DeepL.TranslateINIFile(FSourceFile, TargetFile, TargetLangCode, FALSE);

    if DeepL.LastError <> '' then
      begin
        MessageError('Translation failed: ' + DeepL.LastError);
        EXIT;
      end;

    { Success }
    lblInfo.Caption:= 'Translated: ' + ExtractFileName(TargetFile);
    MessageInfo('Translation completed!' + CRLF + 'File: ' + ExtractFileName(TargetFile));

    { Open in editor for review }
    ShowIniEditor(TargetFile);

  finally
    FreeAndNil(DeepL);
    Screen.Cursor:= crDefault;
  end;
end;




{-------------------------------------------------------------------------------------------------------------
   UTILS
-------------------------------------------------------------------------------------------------------------}

procedure TfrmTranslEditor.lblLiveFormsClick(Sender: TObject);
begin
  lbxForms.Clear;
  for VAR i:= 0 to Screen.FormCount - 1 DO
    begin
      VAR CurForm:= Screen.Forms[I];
      lbxForms.Items.Add(CurForm.Name + ' - ' + CurForm.Caption);
    end;
end;


procedure TfrmTranslEditor.inetDeepLClick(Sender: TObject);
begin
  ExecuteURL('https://www.deepl.com/translator');
end;


procedure TfrmTranslEditor.InternetLabel1Click(Sender: TObject);
begin
  ExecuteURL('https://translate.google.com/');
end;


procedure TfrmTranslEditor.btnHelpClick(Sender: TObject);
begin
  ExecuteURL('https://github.com/GabrielOnDelphi/DelphiLightSaber-AutoTranslator');
end;


procedure TfrmTranslEditor.chkTranslateTranslatorClick(Sender: TObject);
begin
  if chkTranslateTranslator.Checked
  then Tag:= 0
  else Tag:= 128;   // Do not translate this form
end;


end.
