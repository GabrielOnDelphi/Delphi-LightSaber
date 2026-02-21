UNIT FormUpdaterRecEditor;
{-------------------------------------------------------------------------------------------------------------
   Editor for RNews
   2025.10
-------------------------------------------------------------------------------------------------------------}

INTERFACE
{$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  System.SysUtils, System.Classes, Vcl.Forms, LightVcl.Visual.AppDataForm,Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls, Vcl.Samples.Spin;

TYPE
  TfrmRecEditor = class(TLightForm)
    btnClose       : TButton;
    btnCopy        : TButton;
    btnSave        : TButton;
    Button1        : TButton;
    chkBetaVer     : TCheckBox;
    chkCriticalUpd : TCheckBox;
    cmbTarget      : TComboBox;
    edtComment     : TEdit;
    edtHeadline    : TEdit;
    edtOnlineVer   : TEdit;
    GroupBox1      : TGroupBox;
    Label1         : TLabel;
    Label2         : TLabel;
    Label3         : TLabel;
    lblCounter     : TLabel;
    lblVers        : TLabel;
    Memo           : TMemo;
    pnlTop: TPanel;
    Panel3         : TPanel;
    Panel4         : TPanel;
    Panel5         : TPanel;
    Panel6         : TPanel;
    Panel7         : TPanel;
    Panel8         : TPanel;
    pnlBtm         : TPanel;
    spnCounter     : TSpinEdit;
    spnShowCntr    : TSpinEdit;
    procedure FormDestroy (Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  public
    class procedure CreateFormModal(ParentForm: TForm= NIL); static;
 end;

IMPLEMENTATION  {$R *.DFM}

USES
   LightCore.AppData, LightVcl.Visual.AppData, LightVcl.Common.SystemTime, LightVcl.Common.Clipboard, LightVcl.Common. Translate, ciUpdaterRec, LightVcl.Common.Dialogs, LightCore.INIFile, LightVcl.Visual.INIFile;



function GetBinFileName: string;
begin
  Result:= Appdata.AppFolder+ 'OnlineNews.bin';  // switched to v3 since 2025.10
end;


class procedure TfrmRecEditor.CreateFormModal(ParentForm: TForm= NIL);
VAR Form: TfrmRecEditor;
begin
 TAppData.RaiseIfStillInitializing;

 AppData.CreateFormHidden(TfrmRecEditor, Form, asPosOnly, ParentForm);      { Freed by ShowModal }
 WITH Form DO
 begin
   PopulateUsers(cmbTarget);
   if FileExists(GetBinFileName)
   then Button1Click(NIL);
 end;

 Form.ShowModal;  { Closed by mrOk/mrCancel }
end;


procedure TfrmRecEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caFree;
end;


procedure TfrmRecEditor.FormDestroy(Sender: TObject);
begin
  SaveForm;
end;


procedure TfrmRecEditor.btnCopyClick(Sender: TObject);
begin
 StringToClipboard(GetBinFileName);
end;


procedure TfrmRecEditor.btnSaveClick(Sender: TObject);
VAR News: RNews;
begin
  News.Comment     := edtComment.Text;
  News.AppVersion  := edtOnlineVer.Text;
  News.NewsHeadline:= edtHeadline.Text;
  News.NewsBody    := Memo.Text;               // ToDo: Add support for CRLF and links
  News.NewsID      := spnCounter.Value;
  News.TargetUser  := TTargetUser(cmbTarget.ItemIndex);
  News.CriticalUpd := chkCriticalUpd.Checked;
  News.ShowCounter := spnShowCntr.Value;
  News.IsBetaVers  := chkBetaVer.Checked;

  News.SaveTo(GetBinFileName);
  MessageInfo('File saved as '+ GetBinFileName);
end;


procedure TfrmRecEditor.Button1Click(Sender: TObject);
VAR
  FName: string;
  News: RNews;
begin
  FName:= GetBinFileName;
  if NOT AppData.PromptToLoadFile(FName, '.bin') then EXIT;

  News.LoadFrom(FName);

  edtComment.Text     := News.Comment;
  edtOnlineVer.Text   := News.AppVersion;
  edtHeadline.Text    := News.NewsHeadline;
  Memo.Text           := News.NewsBody;               // ToDo: Add support for CRLF and links
  spnCounter.Value    := News.NewsID;
  spnShowCntr.Value   := News.ShowCounter;
  cmbTarget.ItemIndex := Ord(News.TargetUser);
  chkBetaVer.Checked  := News.IsBetaVers;
  chkCriticalUpd.Checked:= News.CriticalUpd;
end;


end.
