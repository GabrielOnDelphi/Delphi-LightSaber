UNIT FormAbout;

{=============================================================================================================
   2026.05.12
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   Template "About" form

--------------------------------------------------------------------------------------------------------------
   Reads data (program name, website, etc) from AppData.
   Shows Trial details (from Proteus). If you don't have Proteus, just ignore this part of the program.
   The form can be closed with Escape or Enter.

   USAGE:
     If you have license protection, pass Proteus to CreateFormModal:
       TfrmAboutApp.CreateFormModal(TRUE, TRUE, MainForm.Proteus);
     Or, when you hold a form reference (CreateFormParented), assign it before showing:
       Form.Proteus:= MainForm.Proteus;   // The setter updates the license UI
     If Proteus is nil, the license-related UI elements are hidden/disabled.

   DON'T ADD IT TO ANY DPK!

   Tester: c:\Projects\LightSaber\Demo\Template App\
=============================================================================================================}

INTERFACE
{$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  Winapi.Windows, System.Classes, Vcl.Controls, Vcl.Forms, LightVcl.Visual.AppDataForm,Vcl.StdCtrls, Vcl.ExtCtrls,
  InternetLabel, cpProteus, Vcl.Imaging.pngimage;

TYPE
  TfrmAboutApp = class(TLightForm)
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
    procedure FormCreate       (Sender: TObject);
    procedure FormKeyPress     (Sender: TObject; var Key: Char);
    procedure btnEnterKeyClick (Sender: TObject);
    procedure btnOrderNowClick (Sender: TObject);
  private
    FProteus: TProteus;
    procedure setProteus(aProteus: TProteus);
  public
    { License system. FormCreate always runs with Proteus=NIL (the form is instantiated inside
      CreateFormModal/CreateFormParented), so the license UI is applied by the SETTER, not by FormCreate. }
    property Proteus: TProteus read FProteus write setProteus;
    class procedure CreateFormModal(ShowOrderNow, ShowEnterKey: Boolean; aProteus: TProteus= NIL); static;
    class function CreateFormParented(Parent: TWinControl): TfrmAboutApp; static;
  end;




IMPLEMENTATION {$R *.dfm}

USES
   System.SysUtils, LightVcl.Common.CenterControl, LightCore.AppData, LightCore.Debugger, LightVcl.Visual.AppData, LightVcl.Common.Dialogs, LightVcl.Common.ExecuteShell;





{ Creates and displays the About form modally.
  Parameters:
    ShowOrderNow - Show the "Order Now" button for unregistered users
    ShowEnterKey - Show the "Enter Key" button for license entry
    aProteus     - Pass MainForm.Proteus if license features are needed. The form variable is local
                   to this method, so this parameter is the ONLY way to deliver Proteus on this path.
  Note: The explicit ShowOrderNow/ShowEnterKey parameters win over the Proteus-derived defaults. }
class procedure TfrmAboutApp.CreateFormModal(ShowOrderNow, ShowEnterKey: Boolean; aProteus: TProteus= NIL);
var
  Form: TfrmAboutApp;
begin
  AppData.CreateForm(TfrmAboutApp, Form, FALSE, asFull);
  Form.Proteus:= aProteus;                  // Setter applies the license UI (expire label, button defaults)
  Form.btnOrderNow.Visible:= ShowOrderNow;
  Form.btnEnterKey.Visible:= ShowEnterKey;
  Form.ShowModal;
end;


{ Creates the About form as an embedded panel within a parent control.
  The form's Container panel is re-parented and centered within the specified Parent.
  IMPORTANT: Caller is responsible for freeing the returned form instance.

  Note: Cannot parent the form directly due to focus issues.
  See: https://stackoverflow.com/questions/42065369/how-to-parent-a-form-controls-wont-accept-focus

  Parameters:
    Parent - The TWinControl that will host the About panel
  Returns:
    The created form instance (caller must free it) }
class function TfrmAboutApp.CreateFormParented(Parent: TWinControl): TfrmAboutApp;
begin
  Assert(Parent <> NIL, 'Parent control cannot be nil');

  AppData.CreateFormHidden(TfrmAboutApp, Result);
  Result.Container.Align:= alNone;
  Result.Container.BevelInner:= bvRaised;
  Result.Container.BevelOuter:= bvLowered;
  Result.Container.Parent:= Parent;
  CenterChild(Result.Container, Parent);
end;


{ Initializes the About form with application information.
  Proteus is ALWAYS nil here: the form is instantiated inside CreateFormModal/CreateFormParented,
  so no caller can assign Proteus before this event fires. We set the no-license defaults here;
  setProteus applies the license UI when (and if) Proteus is assigned later. }
procedure TfrmAboutApp.FormCreate(Sender: TObject);
begin
  // Prevent DFM resource conflict with other forms named 'TfrmAbout'
  // See: https://stackoverflow.com/questions/71518287/h2161-warning-duplicate-resource-type-10-rcdata-id-tfrmabout
  Assert(ClassName <> 'TfrmAbout', 'This form cannot be named TfrmAbout because of DFM resource conflict');

  // Hide license UI. setProteus shows it when a license system is provided.
  btnOrderNow.Visible:= FALSE;
  btnEnterKey.Visible:= FALSE;
  lblExpire.Caption:= '';

  // Populate application info from AppData
  lblCompany.Caption:= AppData.CompanyName;
  lblCompany.Link:= AppData.ProductHome;
  lblAppName.Caption:= AppData.AppName;
  lblVersion.Caption:= TAppData.GetVersionInfoV + '   |   High-DPI: ' + HighDpiAwarenessS;

  // Load Logo.png from AppSysDir only if no design-time image present
  if (imgLogo.Picture.Graphic = NIL)
  AND FileExists(AppData.AppSysDir+ 'Logo.png')
  then imgLogo.Picture.LoadFromFile(AppData.AppSysDir+ 'Logo.png');
end;


{ Applies the license-related UI. Mirrors the FMX twin (FrameFMX\FormAbout.pas):
  lblExpire is Visible=FALSE in the DFM, so it must be shown here, otherwise the
  'Lite edition'/'Registered' caption is painted on an invisible label. }
procedure TfrmAboutApp.setProteus(aProteus: TProteus);
begin
  FProteus:= aProteus;
  if FProteus = NIL then EXIT;   // Keep the no-license defaults set in FormCreate

  btnOrderNow.Visible:= NOT FProteus.CurCertif.Platit;
  btnEnterKey.Visible:= TRUE;
  lblExpire.Visible  := TRUE;
  if FProteus.CurCertif.Trial
  then lblExpire.Caption:= 'Lite edition'
  else lblExpire.Caption:= 'Registered';
end;




{ Allows closing the form with Enter or Escape keys for quick dismissal. }
procedure TfrmAboutApp.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then Close;  // Enter
  if Key = #27 then Close;  // Escape
end;


{ Displays the license key entry dialog.
  Requires Proteus to be assigned before calling. }
procedure TfrmAboutApp.btnEnterKeyClick(Sender: TObject);
begin
  { Hard raise (not Assert) so the failure is also clear in Release builds. Reachable when a caller
    passes ShowEnterKey=TRUE to CreateFormModal without supplying Proteus - previously this was a
    nil-dereference (access violation) in Release. }
  if Proteus = NIL
  then raise Exception.Create('Proteus not assigned! Pass it to TfrmAboutApp.CreateFormModal or set Form.Proteus before showing the form.');

  if Proteus.ShowEnterKeyBox
  then MessageInfo('Key accepted. Please restart the program.')
  else MessageError('Key not accepted!');
end;


{ Opens the product order page in the default browser. }
procedure TfrmAboutApp.btnOrderNowClick(Sender: TObject);
begin
  ExecuteURL(AppData.ProductOrder);
end;


end.
