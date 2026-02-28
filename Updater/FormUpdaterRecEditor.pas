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
    btnLoad        : TButton;
    chkBetaVer     : TCheckBox;
    chkCriticalUpd : TCheckBox;
    cmbTarget      : TComboBox;
    edtHeadline    : TEdit;
    edtOnlineVer   : TEdit;
    GroupBox1      : TGroupBox;
    Label1         : TLabel;
    Label2         : TLabel;
    Label3         : TLabel;
    lblCounter     : TLabel;
    lblVers        : TLabel;
    Memo           : TMemo;
    pnlTop         : TPanel;
    Panel3         : TPanel;
    Panel4         : TPanel;
    Panel5         : TPanel;
    Panel7         : TPanel;
    Panel8         : TPanel;
    pnlBtm         : TPanel;
    spnCounter     : TSpinEdit;
    spnShowCntr    : TSpinEdit;
    edtComment: TEdit;
    procedure FormDestroy (Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FName: string;
  public
    class function GetNewsFileName: string; static;
    class procedure CreateFormModal(ParentForm: TForm= NIL); static;
 end;

IMPLEMENTATION  {$R *.DFM}

USES
   LightCore.AppData, LightVcl.Visual.AppData, LightVcl.Common.Clipboard, ciUpdaterRec, LightVcl.Common.Dialogs;


procedure PopulateUsers(Combo: TComboBox);
begin
 Combo.Items.Clear;
 Combo.Items.Add('All users');
 Combo.Items.Add('Registered users');
 Combo.Items.Add('Trial users');
 Combo.Items.Add('Demo users');
 Combo.ItemIndex:= 0;
end;


class procedure TfrmRecEditor.CreateFormModal(ParentForm: TForm= NIL);
VAR Form: TfrmRecEditor;
begin
  TAppData.RaiseIfStillInitializing;

  AppData.CreateFormHidden(TfrmRecEditor, Form, asPosOnly, ParentForm);      { Freed by ShowModal }
  PopulateUsers(Form.cmbTarget);
  if FileExists(GetNewsFileName)
  then Form.btnLoadClick(NIL);

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
  StringToClipboard(GetNewsFileName);
end;


class function TfrmRecEditor.GetNewsFileName: string;
begin
  Result:= Appdata.AppFolder+ 'OnlineNews.ini';
end;




procedure TfrmRecEditor.btnSaveClick(Sender: TObject);
VAR News: RNews;
begin
  News.Comment     := edtComment.Text;
  News.AppVersion  := edtOnlineVer.Text;
  News.NewsHeadline:= edtHeadline.Text;
  News.NewsBody    := Memo.Text;
  News.NewsID      := spnCounter.Value;
  News.TargetUser  := TTargetUser(cmbTarget.ItemIndex);
  News.CriticalUpd := chkCriticalUpd.Checked;
  News.ShowCounter := spnShowCntr.Value;
  News.IsBetaVers  := chkBetaVer.Checked;

  if FName = ''
  then FName:= GetNewsFileName;
  News.SaveTo(FName);
  MessageInfo('File saved as '+ FName);
end;


procedure TfrmRecEditor.btnLoadClick(Sender: TObject);
VAR
  News: RNews;
begin
  FName:= GetNewsFileName;
  if NOT AppData.PromptToLoadFile(FName, '.ini') then EXIT;

  News.LoadFrom(FName);

  edtComment.Text     := News.Comment;
  edtOnlineVer.Text   := News.AppVersion;
  edtHeadline.Text    := News.NewsHeadline;
  Memo.Text           := News.NewsBody;               // ToDo: Add support for links
  spnCounter.Value    := News.NewsID;
  spnShowCntr.Value   := News.ShowCounter;
  cmbTarget.ItemIndex := Ord(News.TargetUser);
  chkBetaVer.Checked  := News.IsBetaVers;
  chkCriticalUpd.Checked:= News.CriticalUpd;
end;


end.
