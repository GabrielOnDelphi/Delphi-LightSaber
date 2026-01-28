UNIT FormAbout;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------

   Template "About" form

--------------------------------------------------------------------------------------------------------------
   Reads data (program name, website, etc) from AppData.
   Shows Trial details (from Proteus). If you don't have Proteus, just ignore this part of the program.
   The form can be closed with Escape or Enter.

   USAGE:
     Set the Proteus property BEFORE showing the form if you have license protection:
       Form.Proteus:= MainForm.Proteus;
     If Proteus is nil, the license-related UI elements will be hidden/disabled.

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
    lblCredits: TInternetLabel;
    procedure FormCreate       (Sender: TObject);
    procedure FormKeyPress     (Sender: TObject; var Key: Char);
    procedure btnEnterKeyClick (Sender: TObject);
    procedure btnOrderNowClick (Sender: TObject);
  private
  public
    Proteus: TProteus;
    class procedure CreateFormModal(ShowOrderNow, ShowEnterKey: Boolean); static;
    class function CreateFormParented(Parent: TWinControl): TfrmAboutApp; static;
  end;




IMPLEMENTATION {$R *.dfm}

USES
   LightVcl.Common.CenterControl, LightCore.AppData, LightVcl.Visual.AppData, LightVcl.Common.Dialogs, LightVcl.Common.ExecuteShell;





{ Creates and displays the About form modally.
  Parameters:
    ShowOrderNow - Show the "Order Now" button for unregistered users
    ShowEnterKey - Show the "Enter Key" button for license entry
  Note: Set Form.Proteus before calling if license features are needed. }
class procedure TfrmAboutApp.CreateFormModal(ShowOrderNow, ShowEnterKey: Boolean);
var
  Form: TfrmAboutApp;
begin
  AppData.CreateForm(TfrmAboutApp, Form, FALSE, asFull);
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


{ Initializes the About form with application and license information.
  Note: Proteus must be assigned by the caller BEFORE showing the form (via CreateFormModal)
  if license features are needed. If Proteus is nil, license UI is hidden. }
procedure TfrmAboutApp.FormCreate(Sender: TObject);
begin
  // Prevent DFM resource conflict with other forms named 'TfrmAbout'
  // See: https://stackoverflow.com/questions/71518287/h2161-warning-duplicate-resource-type-10-rcdata-id-tfrmabout
  Assert(ClassName <> 'TfrmAbout', 'This form cannot be named TfrmAbout because of DFM resource conflict');

  // Configure license-related UI if Proteus is available
  if Proteus <> NIL then
    begin
      btnOrderNow.Visible:= NOT Proteus.CurCertif.Platit;
      if Proteus.CurCertif.Trial
      then lblExpire.Caption:= 'Lite edition'
      else lblExpire.Caption:= 'Registered';
    end
  else
    begin
      // Hide license UI when Proteus not available
      btnOrderNow.Visible:= FALSE;
      btnEnterKey.Visible:= FALSE;
      lblExpire.Caption:= '';
    end;

  // Populate application info from AppData
  lblCompany.Caption:= AppData.CompanyName;
  lblCompany.Link:= AppData.ProductHome;
  lblAppName.Caption:= AppData.AppName;
  lblVersion.Caption:= TAppData.GetVersionInfoV;
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
  Assert(Proteus <> NIL, 'Proteus not assigned! Set TfrmAboutApp.Proteus before showing the form.');

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
