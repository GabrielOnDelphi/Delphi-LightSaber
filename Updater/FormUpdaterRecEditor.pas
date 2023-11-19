UNIT FormUpdaterRecEditor;
{-------------------------------------------------------------------------------------------------------------
   Editor for RNews
   2022-01-29
-------------------------------------------------------------------------------------------------------------}

INTERFACE
{$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  System.SysUtils, System.Classes, Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls, Vcl.Samples.Spin;

TYPE
  TfrmRecEditor = class(TForm)
    btnSave      : TButton;
    Button1      : TButton;
    cmbTarget    : TComboBox;
    Label1       : TLabel;
    lblCounter   : TLabel;
    Panel1       : TPanel;
    pnlBtm       : TPanel;
    spnCounter   : TSpinEdit;
    btnCopy: TButton;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    edtOnlineVer: TEdit;
    spnShowCntr: TSpinEdit;
    Label2: TLabel;
    chkCriticalUpd: TCheckBox;
    chkBetaVer: TCheckBox;
    edtComment: TEdit;
    lblVers: TLabel;
    btnClose: TButton;
    GroupBox1: TGroupBox;
    edtHeadline: TEdit;
    Memo: TMemo;
    Label3: TLabel;
    procedure FormDestroy (Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
  private
  public
    class procedure ShowEditor; static;
 end;

IMPLEMENTATION  {$R *.DFM}

USES
   ccAppdata, csSystem, ctTranslate, ciUpdaterRec, ccINIFileVCL;



function GetBinFileName: string;
begin
  Result:= AppData.CurFolder+ 'OnlineNews.bin';
end;


class procedure TfrmRecEditor.ShowEditor;
begin
 TAppData.RaiseIfStillInitializing;

 VAR frmEditor:= TfrmRecEditor.Create(NIL);  { Freed by ShowModal }
 WITH frmEditor DO
 begin
   LoadForm(frmEditor, TRUE);            { Position form }
   if Translator <> NIL then Translator.LoadFormTranlation(frmEditor);
   Font:= Application.MainForm.Font;     { Themes }
   PopulateUsers(cmbTarget);
   if FileExists(GetBinFileName)
   then Button1Click(NIL);
 end;

 { Closed by mrOk/mrCancel }
 frmEditor.ShowModal;
 FreeAndNil(frmEditor);    { We need to free the form because the Close will only hide the form! }
end;


procedure TfrmRecEditor.FormDestroy(Sender: TObject);
begin
  SaveForm(Self, TRUE);
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
end;


procedure TfrmRecEditor.Button1Click(Sender: TObject);
VAR News: RNews;
begin
  News.LoadFrom(GetBinFileName);

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
