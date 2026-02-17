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
  LightVcl.Common. Translate, LightVcl.TranslatorAPI, LightVcl.Visual.AppDataForm, LightVcl.Visual.AppData, LightVcl.Common.Dialogs, LightVcl.Visual.Panel,
  LightVcl.Visual.GroupBox, LightVcl.Visual.DropDownSearch, Vcl.WinXCtrls;

TYPE
  TfrmTranslEditor = class(TLightForm)
    lblInfo: TLabel;
    grpOptions: TGroupBox;
    grpLive: TCubicGroupBox;
    lbxForms: TListBox;
    grpHelp: TGroupBox;
    InternetLabel1: TLabel;
    inetDeepL: TLabel;
    btnHelp: TButton;
    chkTranslateTranslator: TCheckBox;
    grpNew: TCubicGroupBox;
    lblTargetLang: TLabel;
    sbxTargetLang: TDropDownSearchBox;
    edtAuthor: TLabeledEdit;
    btnAutoTranslate: TButton;
    btnManualTranslate: TButton;
    chkParseCtrlsAction: TCheckBox;
    chkDontSaveEmpty: TCheckBox;
    chkOverwrite: TCheckBox;
    btnLoadTranslation: TButton;
    btnDeepLSettings: TButton;
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
    procedure PopulateLanguageList;
    function GetTargetFileName: string;
    procedure ApplyOptionsToTranslator;
    procedure ShowIniEditor(const FileName: string);
  public
    class procedure ShowEditor; overload; static;
    class procedure ShowEditor(const TargetLang: string; AutoTranslate: Boolean); overload; static;
    procedure FormPostInitialize; override;
  end;



IMPLEMENTATION {$R *.dfm}

USES
   LightVcl.Common.ExecuteShell, LightCore.IO, LightCore.TextFile, LightVcl.Common.IO,
   FormTranslDeepL, FormTranslatorIniEditor;


{ Shows the translation manager form }
class procedure TfrmTranslEditor.ShowEditor;
begin
  ShowEditor('', FALSE);
end;


{ Shows the translation manager with preselected language and optional auto-translate }
class procedure TfrmTranslEditor.ShowEditor(const TargetLang: string; AutoTranslate: Boolean);
var
  frmEditor: TfrmTranslEditor;
begin
  AppData.CreateForm(TfrmTranslEditor, frmEditor);

  { Apply preselected language }
  if TargetLang <> ''
  then frmEditor.sbxTargetLang.Text:= TargetLang;

  { Trigger auto-translate after form is shown }
  if AutoTranslate
  then frmEditor.btnAutoTranslateClick(frmEditor);
end;


procedure TfrmTranslEditor.FormPostInitialize;
begin
  inherited FormPostInitialize;
  Tag:= DontTranslate;

  Assert(Translator <> NIL, 'Translator must be initialized before using translation editor');
  lblLiveFormsClick(Self);  { Populate live forms list }
  PopulateLanguageList;

  lblInfo.Caption:= 'Select target language and click Auto or Manual translate';
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
VAR FileName: string;
begin
  FileName:= Translator.GetLangFolder;
  if PromptToLoadFile(FileName, FilterTransl, 'Open translation file for editing...')
  then TfrmTranslatorIniEditor.ShowEditor(FileName);
end;




{-------------------------------------------------------------------------------------------------------------
   AUTO TRANSLATE (DEEPL)
-------------------------------------------------------------------------------------------------------------}

procedure TfrmTranslEditor.btnDeepLSettingsClick(Sender: TObject);
begin
  TfrmDeepLSettings.ShowAsModal;
end;


procedure TfrmTranslEditor.btnAutoTranslateClick(Sender: TObject);
var
  DeepL: TDeepLTranslator;
  TargetLang: string;
  TargetLangCode: string;
  TargetFile: string;
  TempSourceFile: string;
  TargetExists: Boolean;
  s: string;
begin
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
                     'Use "Manual translate" for unsupported languages.');
      EXIT;
    end;

  { Check API key }
  if DeepL_GetApiKey.Trim.IsEmpty then
    begin
      MessageWarning('Please configure your DeepL API key first.');
      TfrmDeepLSettings.ShowAsModal;
      EXIT;
    end;

  TargetFile:= GetTargetFileName;
  TargetExists:= FileExists(TargetFile);

  { Confirm with user }
  if TargetExists then
    begin
      if NOT MesajYesNo(Format(
        'Update translation to %s?'+ CRLF+ CRLF+
        'Existing translations will be preserved.'+ CRLF+
        'Only NEW entries will be translated using DeepL.', [TargetLang]))
      then EXIT;
    end
  else
    begin
      if NOT MesajYesNo(Format(
        'Create new translation to %s?'+ CRLF+ CRLF+
        'All entries will be translated using DeepL API.', [TargetLang]))
      then EXIT;
    end;

  { Backup existing file }
  if TargetExists
  then BackupFileIncrement(TargetFile, Translator.GetLangFolder + 'Backups\');

  { Apply options }
  ApplyOptionsToTranslator;

  Screen.Cursor:= crHourGlass;
  TempSourceFile:= Translator.GetLangFolder + '_temp_source_.ini';
  DeepL:= TDeepLTranslator.Create;
  try
    { Step 1: Extract fresh from live forms to temp file }
    Translator.SaveTranslation(TempSourceFile, TRUE);

    { Step 2: Translate - only new entries if target exists }
    DeepL.ApiKey:= DeepL_GetApiKey;
    DeepL.UseFreeAPI:= DeepL_GetUseFreeAPI;
    DeepL.TranslateINIFile(TempSourceFile, TargetFile, TargetLangCode, TargetExists);

    if DeepL.LastError <> '' then
      begin
        MessageError('Translation failed: ' + DeepL.LastError);
        EXIT;
      end;

    { Format the file for better readability }
    s:= StringFromFile(TargetFile);
    s:= ReplaceString(s, CRLF+'[', CRLF+CRLF+'[');
    StringToFile(TargetFile, s, woOverwrite, wpOn);

    { Success }
    lblInfo.Caption:= 'Translated: ' + ExtractFileName(TargetFile);
    if TargetExists
    then MessageInfo('Translation updated!' + CRLF + 'New entries have been translated.')
    else MessageInfo('Translation completed!' + CRLF + 'File: ' + ExtractFileName(TargetFile));

    { Open in editor for review }
    ShowIniEditor(TargetFile);

  finally
    { Clean up temp file }
    if FileExists(TempSourceFile)
    then DeleteFile(TempSourceFile);
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
