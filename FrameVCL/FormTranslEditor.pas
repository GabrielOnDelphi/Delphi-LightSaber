UNIT FormTranslEditor;

{-------------------------------------------------------------------------------------------------------------
   Don't add dependencies to CubicVisualControls here!

   Use this as a helper for the human translator, to be able to see the structure of the GUI (where the controls are located)
   List of Delphi controls on a form - Tree hierarchy and flat list (VCL)
   https://scotthollows.com/2016/10/12/list-of-delphi-controls-on-a-form-hierarchical-and-flat-list-vcl/

   www.GabrielMoraru.com
   See Copyright file
-------------------------------------------------------------------------------------------------------------}

//todo: put digger in the programs to help users translate the program

INTERFACE
{.$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, System.IOUtils, Vcl.Mask,
  LightVcl.Common.Translate, LightCore, LightCore.Time, LightCore.Types, LightVcl.Common.Dialogs, LightCore.AppData, LightVcl.Common.AppData, LightVcl.Common.AppDataForm;

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
    btnCancel: TButton;
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
  private
    CurLangFile: string;
    function GetNewFileName: string;
    procedure SaveEditor;
    procedure LoadTranslation(const FileName: string);
    procedure LoadCurTranslation;
  public
    class procedure ShowEditor; static;
    procedure FormPostInitialize; override; // Called after the main form was fully created
  end;



IMPLEMENTATION {$R *.dfm}
USES
   LightVcl.Common.ExecuteShell, LightVcl.Common.SystemTime, LightVcl.Common.Clipboard, LightCore.IO, LightCore.TextFile, LightVcl.Common.IO;



class procedure TfrmTranslEditor.ShowEditor;
VAR
   frmEditor: TfrmTranslEditor;
begin
  AppData.CreateForm(TfrmTranslEditor, frmEditor);
  frmEditor.LoadCurTranslation;
end;



procedure TfrmTranslEditor.FormPostInitialize;
begin
  inherited FormPostInitialize;
  Assert(Translator <> NIL);
  lblLiveFormsClick(Self);
end;


procedure TfrmTranslEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= TCloseAction.caFree;  {Action:= caFree; Delphi XE7 bug: https://quality.embarcadero.com/browse/RSP-33140 }
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

  if FileExists(CurLangFile)
  then BackupFileIncrement(CurLangFile, Translator.GetLangFolder+'Backups\');

  { Set properties }
  Translator.ParseCtrlsWithAction:= chkParseCtrlsAction.Checked;
  Translator.DontSaveEmpty:= chkDontSaveEmpty.Checked;
  Translator.Authors:= edtAuthor.Text;

  { Create translation file }
  Translator.SaveTranslation(CurLangFile, chkOverwrite.Checked);

  { GUI }
  lblInfo.Caption:= 'Saved as: '+ CurLangFile;
  lblInfo.Visible:= TRUE;
  btnApplyEdits.Enabled:= TRUE;
  edtFileName.Enabled:= FALSE;
  //btnCreateTransl.Caption:= 'Update >>';
  {
  if frmLanguage<> NIL
  then frmLanguage.PopulateLanguageFiles;  // Announce the other form that a new translation file is available
  }
  { Show text in editor }
  s:= StringFromFile(CurLangFile);
  s:= ReplaceString(s, CRLF+'[', CRLF+CRLF+'['); { Help user to see the sections better }
  StringToFile(CurLangFile, s, woOverwrite, wpOn);
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


procedure TfrmTranslEditor.LoadTranslation(CONST FileName: string);
begin
  CurLangFile:= FileName;

  if FileName > ''
  then Caption:= 'Current file: '+ ExtractFileName(FileName)
  else Caption:= 'No file loaded!';

  btnSaveEditor.Enabled:= FileName > '';

  if FileExists(FileName)
  then mmoLangEditor.Text:= StringFromFile(FileName);

  //lblInfo.Visible:= TRUE;
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
  Assert(System.IOUtils.TPath.HasValidFileNameChars(ExtractFileName(CurLangFile), FALSE));

  StringToFile(CurLangFile, mmoLangEditor.Text, woOverwrite, wpOn); { Write with BOM in order to support Chinese lang }
  lblInfo.Caption := 'Saved as: '+ CurLangFile;
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
 if NOT FileExistsMsg(CurLangFile) then EXIT;

 btnSaveEditorClick(Sender);
 Translator.CurLanguage:= CurLangFile;
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





end.
