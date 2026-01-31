UNIT FormTranslEditor;

{=============================================================================================================
   2026.01.29
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   TRANSLATION EDITOR FORM

   Visual editor for creating and editing translation INI files.
   Works with LightVcl.Common.Translate.pas translation engine.

   Features:
     - Create new translation files from live forms
     - Edit existing translation files
     - Extract values for easy translation
     - Apply translations live without restart
     - Links to DeepL and Google Translate

   USAGE:
     Call TfrmTranslEditor.ShowEditor to display the translation editor.

   NOTES:
     - Don't add dependencies to CubicVisualControls here!
     - Use this as a helper for human translators to see GUI structure
     - Only live (running) forms can be translated

   RELATED:
     - LightVcl.Common.Translate.pas - Translation engine
     - FormTranslSelector.pas - Language selector form

   See: https://scotthollows.com/2016/10/12/list-of-delphi-controls-on-a-form-hierarchical-and-flat-list-vcl/
=============================================================================================================}

INTERFACE
{$DENYPACKAGEUNIT ON}

USES
  System.SysUtils, System.Classes, System.IOUtils,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Mask,
  LightVcl.Common.Translate, LightVcl.Visual.AppDataForm,
  LightCore, LightCore.Time, LightCore.Types, LightVcl.Common.Dialogs,
  LightCore.AppData, LightVcl.Visual.AppData,
  LightVcl.Common.TranslatorAPI;

TYPE
  TfrmTranslEditor = class(TLightForm)
    btnApplyEdits        : TButton;
    btnCopy              : TButton;
    btnCopyRight         : TButton;
    btnCreateTransl      : TButton;
    btnHelp              : TButton;
    btnLoad              : TButton;
    btnOK                : TButton;
    btnSaveEditor        : TButton;
    btnValues            : TButton;
    chkDontSaveEmpty     : TCheckBox;
    chkOverwrite         : TCheckBox;
    chkParseCtrlsAction  : TCheckBox;
    CubicGroupBox1       : TGroupBox;
    CubicGroupBox3       : TGroupBox;
    edtAuthor            : TLabeledEdit;
    edtFileName          : TLabeledEdit;
    GroupBox2            : TGroupBox;
    grpNewTransl         : TGroupBox;
    lblInfo              : TLabel;
    lblLiveForms         : TLabel;
    lbxForms             : TListBox;
    mmoLangEditor        : TMemo;
    mmoValues            : TMemo;
    Panel1               : TPanel;
    Panel3               : TPanel;
    Panel4               : TPanel;
    pnlRight             : TPanel;
    Splitter1            : TSplitter;
    inetDeepL            : TLabel;
    InternetLabel1       : TLabel;
    btnCancel            : TButton;
    grpAutoTranslate     : TGroupBox;
    lblTargetLang        : TLabel;
    cmbTargetLang        : TComboBox;
    btnAutoTranslate     : TButton;
    btnDeepLSettings     : TButton;
    procedure btnApplyEditsClick  (Sender: TObject);
    procedure btnCopyClick        (Sender: TObject);
    procedure btnCopyRightClick   (Sender: TObject);
    procedure btnCreateTranslClick(Sender: TObject);
    procedure btnHelpClick        (Sender: TObject);
    procedure btnLoadClick        (Sender: TObject);
    procedure btnOKClick          (Sender: TObject);
    procedure btnSaveEditorClick  (Sender: TObject);
    procedure btnValuesClick      (Sender: TObject);
    procedure FormClose           (Sender: TObject; var Action: TCloseAction);
    procedure lblLiveFormsClick   (Sender: TObject);
    procedure inetDeepLClick      (Sender: TObject);
    procedure InternetLabel1Click (Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnAutoTranslateClick(Sender: TObject);
    procedure btnDeepLSettingsClick(Sender: TObject);
  private
    FCurLangFile: string;  { Full path to currently loaded translation file }
    function GetNewFileName: string;
    procedure SaveEditor;
    procedure LoadTranslation(const FileName: string);
    procedure LoadCurTranslation;
  public
    class procedure ShowEditor; static;
    procedure FormPostInitialize; override;
  end;



IMPLEMENTATION {$R *.dfm}

USES
   LightVcl.Common.ExecuteShell, LightVcl.Common.SystemTime, LightVcl.Common.Clipboard, LightCore.IO, LightCore.TextFile, LightVcl.Common.IO,
   FormDeepLSettings;


{ Shows the translation editor form }
class procedure TfrmTranslEditor.ShowEditor;
var
  frmEditor: TfrmTranslEditor;
begin
  AppData.CreateForm(TfrmTranslEditor, frmEditor);
  frmEditor.LoadCurTranslation;
end;


procedure TfrmTranslEditor.FormPostInitialize;
begin
  inherited FormPostInitialize;
  Assert(Translator <> NIL, 'Translator must be initialized before using translation editor');
  lblLiveFormsClick(Self);  { Populate live forms list }

  { Populate target language combo box }
  cmbTargetLang.Items.Clear;
  for var Lang in GetSupportedLanguages do
    cmbTargetLang.Items.Add(Lang);
  if cmbTargetLang.Items.Count > 0
  then cmbTargetLang.ItemIndex:= 0;
end;


procedure TfrmTranslEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  { Note: caFree bug (RSP-33140) was fixed in Delphi 11 }
  Action:= TCloseAction.caFree;
end;











{-------------------------------------------------------------------------------------------------------------
   CREATE NEW TRANSLATION
-------------------------------------------------------------------------------------------------------------}

procedure TfrmTranslEditor.btnCreateTranslClick(Sender: TObject);
begin
  grpNewTransl.Visible:= TRUE;
end;


procedure TfrmTranslEditor.btnOKClick(Sender: TObject);
VAR s: string;
begin
  if NOT System.IOUtils.TPath.HasValidPathChars(GetNewFileName, FALSE) then
   begin
     MessageError('The file name has invalid characters!'+ GetNewFileName);
     EXIT;
   end;

  LoadTranslation(GetNewFileName);

  if FileExists(FCurLangFile)
  then BackupFileIncrement(FCurLangFile, Translator.GetLangFolder+'Backups\');

  { Set properties }
  Translator.ParseCtrlsWithAction:= chkParseCtrlsAction.Checked;
  Translator.DontSaveEmpty:= chkDontSaveEmpty.Checked;
  Translator.Authors:= edtAuthor.Text;

  { Create translation file }
  Translator.SaveTranslation(FCurLangFile, chkOverwrite.Checked);

  { GUI }
  lblInfo.Caption:= 'Saved as: '+ FCurLangFile;
  lblInfo.Visible:= TRUE;
  btnApplyEdits.Enabled:= TRUE;
  edtFileName.Enabled:= FALSE;
  //btnCreateTransl.Caption:= 'Update >>';
  {
  if frmLanguage<> NIL
  then frmLanguage.PopulateLanguageFiles;  // Announce the other form that a new translation file is available
  }
  { Show text in editor }
  s:= StringFromFile(FCurLangFile);
  s:= ReplaceString(s, CRLF+'[', CRLF+CRLF+'['); { Help user to see the sections better }
  StringToFile(FCurLangFile, s, woOverwrite, wpOn);
  mmoLangEditor.Text:= s;

  grpNewTransl.Visible:= FALSE;
end;


procedure TfrmTranslEditor.btnCancelClick(Sender: TObject);
begin
  grpNewTransl.Visible:= FALSE;
end;





{-------------------------------------------------------------------------------------------------------------
   FILE NAME
-------------------------------------------------------------------------------------------------------------}
function TfrmTranslEditor.GetNewFileName: string;
begin
  Result:= Translator.GetLangFolder+ ForceExtension(edtFileName.Text, '.ini');
end;



{-------------------------------------------------------------------------------------------------------------
   LOAD TRANSLATION
-------------------------------------------------------------------------------------------------------------}
procedure TfrmTranslEditor.LoadCurTranslation;
begin
  LoadTranslation(Translator.CurLanguage);
end;


procedure TfrmTranslEditor.btnLoadClick(Sender: TObject);
begin
  VAR sTempFile:= Translator.CurLanguage;

  if PromptToLoadFile(sTempFile, FilterTransl, 'Open a translation file...')
  then LoadTranslation(sTempFile)
  else LoadTranslation('');
end;


{ Loads a translation file into the editor }
procedure TfrmTranslEditor.LoadTranslation(const FileName: string);
begin
  FCurLangFile:= FileName;

  if FileName <> ''
  then Caption:= 'Current file: ' + ExtractFileName(FileName)
  else Caption:= 'No file loaded!';

  btnSaveEditor.Enabled:= FileName <> '';

  if FileExists(FileName)
  then mmoLangEditor.Text:= StringFromFile(FileName);
end;



{-------------------------------------------------------------------------------------------------------------
   SAVE TRANSLATION
-------------------------------------------------------------------------------------------------------------}
procedure TfrmTranslEditor.btnSaveEditorClick(Sender: TObject);
begin
  SaveEditor;
end;


procedure TfrmTranslEditor.SaveEditor;
begin
  // HasValidFileNameChars only work with file names, not also with full paths
  Assert(System.IOUtils.TPath.HasValidFileNameChars(ExtractFileName(FCurLangFile), FALSE));

  StringToFile(FCurLangFile, mmoLangEditor.Text, woOverwrite, wpOn); { Write with BOM in order to support Chinese lang }
  lblInfo.Caption := 'Saved as: '+ FCurLangFile;
  lblInfo.Visible:= TRUE;
  btnApplyEdits.Enabled:= TRUE;
end;




{-------------------------------------------------------------------------------------------------------------
   EDITOR
-------------------------------------------------------------------------------------------------------------}
procedure TfrmTranslEditor.btnValuesClick(Sender: TObject);
VAR
   s, Output: string;
begin
 Output:= '';
 for s in mmoLangEditor.Lines DO
   if Pos('=', s) > 0
   then Output:= Output + CopyFrom(s, '=', Length(s), FALSE)+ CRLF
   else Output:= Output + s+ CRLF;
   //Output:= RemoveLastEnter(Output);

  mmoValues.Text:= Output;
  pnlRight.Visible:= NOT pnlRight.Visible;
end;


procedure TfrmTranslEditor.btnApplyEditsClick(Sender: TObject);
begin
 if NOT FileExistsMsg(FCurLangFile) then EXIT;

 btnSaveEditorClick(Sender);
 Translator.CurLanguage:= FCurLangFile;
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


procedure TfrmTranslEditor.btnCopyClick(Sender: TObject);
begin
  StringToClipboard(mmoLangEditor.Text);
end;


procedure TfrmTranslEditor.btnCopyRightClick(Sender: TObject);
begin
  StringToClipboard(mmoValues.Text);
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
  LightVcl.Common.ExecuteShell.ExecuteShell(Translator.GetLangFolder+'How to translate.rtf');
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
  { Validate }
  if FCurLangFile = '' then
    begin
      MessageWarning('Please load or create a translation file first.');
      EXIT;
    end;

  if cmbTargetLang.ItemIndex < 0 then
    begin
      MessageWarning('Please select a target language.');
      EXIT;
    end;

  TargetLang:= cmbTargetLang.Items[cmbTargetLang.ItemIndex];
  TargetLangCode:= LanguageNameToDeepL(TargetLang);

  if TargetLangCode = '' then
    begin
      MessageWarning('Unknown language: ' + TargetLang);
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
  CharCount:= Length(mmoLangEditor.Text);
  if NOT MessageConfirm(Format(
    'Translate to %s?'+ CRLF+ CRLF+
    'Estimated characters: %d'+ CRLF+
    'This will use your DeepL API quota.', [TargetLang, CharCount]))
  then EXIT;

  { Create target file path }
  TargetFile:= Translator.GetLangFolder + TargetLang + '.ini';

  { Warn if file exists }
  if FileExists(TargetFile) then
    if NOT MessageConfirm('File already exists: ' + ExtractFileName(TargetFile) + CRLF + 'Overwrite?')
    then EXIT;

  { Perform translation }
  Screen.Cursor:= crHourGlass;
  DeepL:= TDeepLTranslator.Create;
  try
    DeepL.ApiKey:= DeepL_GetApiKey;
    DeepL.UseFreeAPI:= DeepL_GetUseFreeAPI;

    DeepL.TranslateINIFile(FCurLangFile, TargetFile, TargetLangCode, FALSE);

    if DeepL.LastError <> '' then
      begin
        MessageError('Translation failed: ' + DeepL.LastError);
        EXIT;
      end;

    { Success - load the new file }
    LoadTranslation(TargetFile);
    lblInfo.Caption:= 'Translation saved to: ' + TargetFile;
    lblInfo.Visible:= TRUE;
    MessageInfo('Translation completed successfully!' + CRLF + 'File: ' + ExtractFileName(TargetFile));

  finally
    FreeAndNil(DeepL);
    Screen.Cursor:= crDefault;
  end;
end;




end.
