UNIT FormAbout;

{=============================================================================================================
   2026.04.14
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   Template "About" form — FMX version

   CROSS-PLATFORM: Yes (Windows, macOS, Android, iOS)
   PROTEUS: Windows-only (cpProteus uses Vcl.Controls — not yet ported to FMX)

--------------------------------------------------------------------------------------------------------------
   Reads data (program name, website, etc) from AppData.
   Shows Trial details (from Proteus). If you don't have Proteus, just ignore this part.
   Form closed with Escape or Enter.

   USAGE:
     1. Modal:
          TfrmAboutApp.CreateFormModal(ShowOrderNow, ShowEnterKey);

     2. Logo image (optional — imgLogo is empty by default):
          Before calling CreateFormModal, in FormCreate override or after creation:
          Form.imgLogo.Bitmap.LoadFromFile(AppData.AppSysDir + 'logo.png');

     3. Proteus (Windows only):
          Form.Proteus := MainForm.Proteus;   // must be set BEFORE showing the form

     4. Credits / EULA links (optional):
          Set Form.CreditsURL and/or Form.EULAURL before showing.
          Labels are hidden if URLs are empty.

   DON'T ADD IT TO ANY DPK!
=============================================================================================================}

INTERFACE
{$DENYPACKAGEUNIT ON}
{.DEFINE USEPROTEUS}

USES
  System.SysUtils, System.Classes, System.UITypes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.Objects, FMX.Layouts,
  FMX.Controls.Presentation,
  LightFmx.Common.AppData.Form
  {$IFDEF USEPROTEUS}
  , cpProteus
  {$ENDIF};

TYPE
  TfrmAboutApp = class(TLightForm)
    Container   : TLayout;
    imgLogo     : TImage;
    lblAppName  : TLabel;
    lblVersion  : TLabel;
    lblCompany  : TLabel;
    lblCredits  : TLabel;
    lblChildren : TLabel;
    lblExpire   : TLabel;
    inetEULA    : TLabel;
    btnEnterKey : TButton;
    btnOrderNow : TButton;
    procedure FormCreate       (Sender: TObject);
    procedure FormKeyDown      (Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure btnEnterKeyClick (Sender: TObject);
    procedure btnOrderNowClick (Sender: TObject);
    procedure lblCompanyClick  (Sender: TObject);
    procedure inetEULAClick    (Sender: TObject);
    procedure lblCreditsClick  (Sender: TObject);
  private
    FProductHome : string;
    FProductOrder: string;
    FEULAURL     : string;
    FCreditsURL  : string;
  public
    {$IFDEF USEPROTEUS}
    Proteus: TProteus;
    {$ENDIF}
    EULAURL    : string;   // Optional. Set before showing. If empty, EULA label stays hidden.
    CreditsURL : string;   // Optional. Set before showing. If empty, Credits label stays hidden.
    CreditsText: string;   // Optional. Caption for credits label.
    class procedure CreateFormModal; static;
  end;




IMPLEMENTATION {$R *.fmx}

USES
  LightCore.AppData, LightCore.Internet,
  LightFmx.Common.AppData, LightFmx.Common.Dialogs;




{ Creates and displays the About form modally. }
class procedure TfrmAboutApp.CreateFormModal;
var
  Form: TfrmAboutApp;
begin
  AppData.CreateForm(TfrmAboutApp, Form, asNone);
  {$IFDEF USEPROTEUS}
  Form.btnOrderNow.Visible:= TRUE;
  Form.btnEnterKey.Visible:= TRUE;
  {$ENDIF}

  AppData.ShowModal(Form);
end;


{ Initializes the About form with application and license information. }
procedure TfrmAboutApp.FormCreate(Sender: TObject);
begin
  CloseOnEscape:= TRUE;

  FProductHome := AppData.ProductHome;
  FProductOrder:= AppData.ProductOrder;
  FEULAURL     := EULAURL;
  FCreditsURL  := CreditsURL;

  {$IFDEF USEPROTEUS}
  if Proteus <> NIL then
    begin
      btnOrderNow.Visible:= NOT Proteus.CurCertif.Platit;
      lblExpire.Visible  := TRUE;
      if Proteus.CurCertif.Trial
      then lblExpire.Text:= 'Lite edition'
      else lblExpire.Text:= 'Registered';
    end
  else
    begin
      btnOrderNow.Visible:= FALSE;
      btnEnterKey.Visible:= FALSE;
      lblExpire.Visible  := FALSE;
    end;
  {$ELSE}
  btnOrderNow.Visible:= FALSE;
  btnEnterKey.Visible:= FALSE;
  lblExpire.Visible  := FALSE;
  {$ENDIF}

  lblCompany.Text  := AppData.CompanyName;
  lblAppName.Text  := AppData.AppName;
  lblVersion.Text  := AppData.GetAppVersion;

  // Clickable company label — show only if URL available
  lblCompany.Visible := FProductHome <> '';

  // EULA link
  inetEULA.Visible:= FEULAURL <> '';

  // Credits link
  if FCreditsURL <> '' then
    begin
      lblCredits.Text   := CreditsText;
      lblCredits.Visible:= TRUE;
    end
  else
    lblCredits.Visible:= FALSE;

  if FileExists(AppData.AppSysDir+ 'Logo.jpg')
  then imgLogo.Bitmap.LoadFromFile(AppData.AppSysDir+ 'Logo.jpg');
end;


{ Closes the form on Enter or Escape. }
procedure TfrmAboutApp.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key in [vkReturn, vkEscape] then Close;
end;


{ Displays the license key entry dialog. Requires Proteus. }
procedure TfrmAboutApp.btnEnterKeyClick(Sender: TObject);
begin
  {$IFDEF USEPROTEUS}
  Assert(Proteus <> NIL, 'Proteus not assigned! Set TfrmAboutApp.Proteus before showing.');
  if Proteus.ShowEnterKeyBox
  then MessageInfo('Key accepted. Please restart the program.')
  else MessageError('Key not accepted!');
  {$ENDIF}
end;


procedure TfrmAboutApp.btnOrderNowClick(Sender: TObject);
begin
  OpenURL(FProductOrder);
end;


procedure TfrmAboutApp.lblCompanyClick(Sender: TObject);
begin
  OpenURL(FProductHome);
end;


procedure TfrmAboutApp.inetEULAClick(Sender: TObject);
begin
  OpenURL(FEULAURL);
end;


procedure TfrmAboutApp.lblCreditsClick(Sender: TObject);
begin
  OpenURL(FCreditsURL);
end;


end.
