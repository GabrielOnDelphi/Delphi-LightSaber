UNIT FormTranslator;

{-------------------------------------------------------------------------------------------------------------
   Don't add dependencies to CubicVisualControls here!

   Use this as a helper for the human translator, to be able to see the structure of the GUI (where the controls are located)
   List of Delphi controls on a form - Tree hierarchy and flat list (VCL)
   https://scotthollows.com/2016/10/12/list-of-delphi-controls-on-a-form-hierarchical-and-flat-list-vcl/

   See Copyright.txt
-------------------------------------------------------------------------------------------------------------}

//todo: put digger in the programs to help users translate the program

INTERFACE
{.$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  Winapi.Messages, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, System.IOUtils, Vcl.Mask,
  cTranslate, ccCore, cbDialogs, cbAppData, cbAppDataForm;

TYPE
  TfrmTranslator = class(TLightForm)
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
    procedure FormDestroy         (Sender: TObject);
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
    procedure inetDeepLClick(Sender: TObject);
    procedure InternetLabel1Click(Sender: TObject);
  private
    function GetFileName: string;
  public
    procedure LateInitialize; override; // Called after the main form was fully created
  end;



IMPLEMENTATION {$R *.dfm}
USES
   csExecuteShell, csSystem, ccIO, ccTextFile, cmIO, cmIO.Win, cbIniFile, FormSelectLang;




procedure TfrmTranslator.LateInitialize;
begin
 inherited LateInitialize;
 Assert(Translator <> NIL);
 // LoadFormBase(Self);                              //Note: Don't add dependencies to CubicVisualControls here!
 lblLiveFormsClick(Self);
end;


procedure TfrmTranslator.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action:= TCloseAction.caFree;  {Action:= caFree; Delphi XE7 bug: https://quality.embarcadero.com/browse/RSP-33140 }
end;


procedure TfrmTranslator.FormDestroy(Sender: TObject);
begin
end;





{---------------------------------------------------------------------------
   CREATE TRANSLATION
---------------------------------------------------------------------------}
procedure TfrmTranslator.btnCreateTranslClick(Sender: TObject);
begin
  grpNewTransl.Visible:= TRUE;
end;


procedure TfrmTranslator.btnOKClick(Sender: TObject);
VAR s: string;
begin
 if FileExists(GetFileName)
 then BackupFileIncrement(GetFileName, Translator.GetLangFolder+'Backups\');

 { Set properties }
 Translator.ParseCtrlsWithAction:= chkParseCtrlsAction.Checked;
 Translator.DontSaveEmpty:= chkDontSaveEmpty.Checked;
 Translator.Authors:= edtAuthor.Text;

 { Create translation file }
 Translator.SaveTranslation(GetFileName, chkOverwrite.Checked);

 { GUI }
 lblInfo.Caption:= 'Saved as: '+ GetFileName;
 lblInfo.Visible:= TRUE;
 btnApplyEdits.Enabled:= TRUE;
 edtFileName.Enabled:= FALSE;
 btnCreateTransl.Caption:= 'Update >>';
 {
 if frmLanguage<> NIL
 then frmLanguage.PopulateLanguageFiles;  // Announce the other form that a new translation file is available
 }
 { Show text in editor }
 s:= StringFromFile(GetFileName);
 s:= ReplaceString(s, CRLF+'[', CRLF+CRLF+'['); { Help user to see the sections better }
 StringToFile(GetFileName, s, woOverwrite, wpOn);
 mmoLangEditor.Text:= s;

 grpNewTransl.Visible:= FALSE;
end;



procedure TfrmTranslator.btnHelpClick(Sender: TObject);
begin
 csExecuteShell.ExecuteShell(Translator.GetLangFolder+'How to translate.rtf');
end;



{---------------------------------------------------------------------------
   EDIT TRANSLATION
---------------------------------------------------------------------------}
procedure TfrmTranslator.btnLoadClick(Sender: TObject);
VAR CurrentFile: string;
begin
 CurrentFile:= Translator.GetLangFolder+ 'English.ini';
 if PromptToLoadFile(CurrentFile, FilterTransl, 'Open a translation file...')
 then mmoLangEditor.Lines.LoadFromFile(CurrentFile); //todo 1: replace with StringFromFile
end;


procedure TfrmTranslator.btnSaveEditorClick(Sender: TObject);
begin
 // HasValidFileNameChars only work with file names, not also with full paths
 if System.IOUtils.TPath.HasValidFileNameChars(edtFileName.Text, FALSE)
 then
  begin
    StringToFile(GetFileName, mmoLangEditor.Text, woOverwrite, wpOn); { Write with BOM in order to support Chinese lang }
    lblInfo.Caption := 'Saved as: '+ GetFileName;
    lblInfo.Visible:= TRUE;
    btnApplyEdits.Enabled:= TRUE;
  end
 else
    MesajError('Invalid file name!');
end;


procedure TfrmTranslator.btnValuesClick(Sender: TObject);
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


procedure TfrmTranslator.btnApplyEditsClick(Sender: TObject);
begin
 if NOT FileExistsMsg(GetFileName) then EXIT;
 btnSaveEditorClick(Sender);
 Translator.LoadTranslationAllForms(GetFileName, TRUE);
 lblInfo.Caption:= TimeToStr(now)+ '. Loaded: '+ GetFileName;
 lblInfo.Visible:= TRUE;
end;





procedure TfrmTranslator.lblLiveFormsClick(Sender: TObject);
begin
 lbxForms.Clear;
 for VAR i:= 0 to Screen.FormCount - 1 DO
   begin
     VAR CurForm:= Screen.Forms[I];
     lbxForms.Items.Add(CurForm.Name + ' - ' + CurForm.Caption);
   end;
end;







{---------------------------------------------------------------------------
   UTILS
---------------------------------------------------------------------------}
procedure TfrmTranslator.btnCopyClick(Sender: TObject);
begin
  csSystem.StringToClipboard(mmoLangEditor.Text);
end;


procedure TfrmTranslator.btnCopyRightClick(Sender: TObject);
begin
  csSystem.StringToClipboard(mmoValues.Text);
end;


function TfrmTranslator.GetFileName: string;
begin
  Result:= Translator.GetLangFolder+ ForceExtension(edtFileName.Text, '.ini');
end;



procedure TfrmTranslator.inetDeepLClick(Sender: TObject);
begin
  ExecuteURL('https://www.deepl.com/translator');
end;

procedure TfrmTranslator.InternetLabel1Click(Sender: TObject);
begin
  ExecuteURL('https://translate.google.com/');
end;


end.
