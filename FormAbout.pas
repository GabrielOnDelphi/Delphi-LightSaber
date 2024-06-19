UNIT FormAbout;

//DON'T ADD IT TO ANY DPK!
//Tester: c:\Myprojects\Project Testers\cAboutForm\TesterAboutForm.dpr

INTERFACE
{$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  Winapi.Windows, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls,
  InternetLabel, cpProteus;

TYPE
  // This form cannot be named TfrmAbout because of DFM resource conflict: https://stackoverflow.com/questions/71518287/h2161-warning-duplicate-resource-type-10-rcdata-id-tfrmabout
  TfrmAboutApp = class(TForm)
    Container: TPanel;
    lblChildren: TLabel;
    lblVersion: TLabel;
    imgLogo: TImage;
    inetHomePage: TInternetLabel;
    Label1: TLabel;
    pnlEnterKey: TPanel;
    btnEnterKey: TButton;
    btnOrderNow: TButton;
    Panel1: TPanel;
    lblExpire: TLabel;
    Label2: TLabel;
    lblCopyRight1: TLabel;
    inetEULA: TInternetLabel;
    lblAppName: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure btnEnterKeyClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
  public
    Proteus: TProteus;
  end;

VAR
  frmAbout: TfrmAboutApp;

procedure ShowAbout        (CONST ProductHomePage: string);
procedure ShowAboutParented(CONST ProductHomePage: string; Parent: TWinControl);

IMPLEMENTATION {$R *.dfm}

USES
   uLinks, cbAppData, ccCore, csSystem, cbDialogs;



procedure ShowAbout(CONST ProductHomePage: string);
begin
 frmAbout:= TfrmAboutApp.Create(Application);
 frmAbout.inetHomePage.Link:= ProductHomePage;
 frmAbout.Show;
end;


{ This won't to parent the form directly. See: https://stackoverflow.com/questions/42065369/how-to-parent-a-form-controls-wont-accept-focus }
procedure ShowAboutParented(CONST ProductHomePage: string; Parent: TWinControl);
begin
 frmAbout:= TfrmAboutApp.Create(Application);
 frmAbout.Container.Align:= alNone;
 frmAbout.Container.BevelInner:= bvRaised;
 frmAbout.Container.BevelOuter:= bvLowered;

 frmAbout.Container.Parent:= Parent;
 frmAbout.inetHomePage.Link:= ProductHomePage;
 CenterChild(frmAbout.Container, Parent);
end;





procedure TfrmAboutApp.FormCreate(Sender: TObject);
begin
 if Proteus<> NIL then
  begin
   btnOrderNow.Visible:= NOT Proteus.CurCertif.Platit;
   if Proteus.CurCertif.Trial
   then lblExpire.Caption:= 'Lite edition'
   else lblExpire.Caption:= 'Registered';
  end;

 lblCopyRight1.Caption:= 'Copyright '+ ctCompanyNameCubic;
 lblVersion.Caption := 'Version '+ AppData.GetVersionInfo;
 lblAppName.Caption:= AppData.AppName;
end;


procedure TfrmAboutApp.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 CanClose:= TRUE;
end;


procedure TfrmAboutApp.FormShow(Sender: TObject);
begin
 //pnlAbout.ShowGradient:= NOT Vcl.Themes.TStyleManager.Enabled;
end;


procedure TfrmAboutApp.FormKeyPress(Sender: TObject; var Key: Char);
begin
 if Ord(Key) = VK_RETURN then Close;
 if Ord(Key) = VK_ESCAPE then Close;
end;


procedure TfrmAboutApp.btnEnterKeyClick(Sender: TObject);
begin
 if Proteus.ShowEnterKeyBox
 then MesajInfo ('Key accepted. Please restart the program.')
 else MesajError('Key not accepted!');
end;


end.
