UNIT FormAbout;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

   Template "About" form

--------------------------------------------------------------------------------------------------------------
   Reads data (program name, website, etc) from AppData.
   Shows Trial details (from Proteus). If you don't have Proteus, just ignore this part of the program.
   The form can be closed with Escape.

   DON'T ADD IT TO ANY DPK!

   Tester: c:\Myprojects\LightSaber\Demo\Template App\
=============================================================================================================}

INTERFACE
{$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  Winapi.Windows, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Imaging.pngimage,
  InternetLabel, cpProteus, cbIniFile;

TYPE
  TfrmAboutApp = class(TForm)
    Container    : TPanel;
    imgLogo      : TImage;
    lblCompany   : TInternetLabel;
    lblAppName   : TLabel;
    lblChildren  : TLabel;
    lblVersion   : TLabel;
    lblExpire    : TLabel;
    inetEULA     : TInternetLabel;
    btnEnterKey  : TButton;
    btnOrderNow  : TButton;
    procedure FormShow         (Sender: TObject);
    procedure FormCreate       (Sender: TObject);
    procedure FormKeyPress     (Sender: TObject; var Key: Char);
    procedure btnEnterKeyClick (Sender: TObject);
    procedure FormCloseQuery   (Sender: TObject; var CanClose: Boolean);
    procedure btnOrderNowClick (Sender: TObject);
  private
  public
    Proteus: TProteus;
    class procedure CreateFormModal; static;
    class function CreateFormParented(Parent: TWinControl): TfrmAboutApp; static;
  end;




IMPLEMENTATION {$R *.dfm}

USES
   cbCenterControl, cbAppData, cbDialogs;





class procedure TfrmAboutApp.CreateFormModal;
VAR Form: TfrmAboutApp;
begin
  AppData.CreateFormModal(TfrmAboutApp, Form);
end;



{ This won't to parent the form directly. See: https://stackoverflow.com/questions/42065369/how-to-parent-a-form-controls-wont-accept-focus }
class function TfrmAboutApp.CreateFormParented(Parent: TWinControl): TfrmAboutApp;
begin
 AppData.CreateFormHidden(TfrmAboutApp, Result);
 Result.Container.Align:= alNone;
 Result.Container.BevelInner:= bvRaised;
 Result.Container.BevelOuter:= bvLowered;
 Result.Container.Parent := Parent;
 CenterChild(Result.Container, Parent);
end;


procedure TfrmAboutApp.FormCreate(Sender: TObject);
begin
 Assert(ClassName <> 'TfrmAbout', 'This form cannot be named TfrmAbout because of DFM resource conflict'); // https://stackoverflow.com/questions/71518287/h2161-warning-duplicate-resource-type-10-rcdata-id-tfrmabout

 if Proteus<> NIL then
  begin
    btnOrderNow.Visible:= NOT Proteus.CurCertif.Platit;
    if Proteus.CurCertif.Trial
    then lblExpire.Caption:= 'Lite edition'
    else lblExpire.Caption:= 'Registered';
  end;

 lblCompany.Caption := AppData.CompanyName;
 lblCompany.Link    := AppData.ProductHome;
 lblAppName.Caption := AppData.AppName;
 lblVersion.Caption := AppData.GetVersionInfoV;
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


procedure TfrmAboutApp.btnOrderNowClick(Sender: TObject);
begin
  ///executeurl();
end;


end.
