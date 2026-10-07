UNIT FormAbout;

{=============================================================================================================
   2026.10.06
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   Template "About" form

--------------------------------------------------------------------------------------------------------------
   Reads data (program name, website, etc) from AppData.
   The form can be closed with Escape or Enter.

   License UI (Order now, Enter key, the 'Lite edition'/'Registered' label):
     The form knows no license library. It gets plain values: an 'order now' flag, an expiry text
     and an OnEnterKey function. Without them the license UI stays hidden.
     Programs that use the private Proteus library fill these values with
     LightProteus\ProteusSource\cpProteusAbout.pas:
       cpProteusAbout.ShowAboutBox(MainForm.Proteus, TRUE, TRUE);
     Without a license library:
       TfrmAboutApp.CreateFormModal(FALSE, FALSE);

   DON'T ADD IT TO ANY DPK!

   Tester: c:\Projects\LightSaber\Demo\Template App\
=============================================================================================================}

INTERFACE
{$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  Winapi.Windows, System.Classes, Vcl.Controls, Vcl.Forms, LightVcl.Visual.AppDataForm,Vcl.StdCtrls, Vcl.ExtCtrls,
  InternetLabel, Vcl.Imaging.pngimage;

TYPE
  TEnterKeyFunc = reference to function: Boolean;   { Shows the license-key box. Returns TRUE when the key was accepted. }

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
    FOnEnterKey: TEnterKeyFunc;
  public
    { License UI. FormCreate always runs before a caller can pass license data (the form is
      instantiated inside CreateFormModal/CreateFormParented), so it is applied here, not in FormCreate. }
    procedure SetLicense(OrderNow: Boolean; CONST ExpireText: string; aOnEnterKey: TEnterKeyFunc);
    class procedure CreateFormModal(ShowOrderNow, ShowEnterKey: Boolean); overload; static;
    class procedure CreateFormModal(ShowOrderNow, ShowEnterKey: Boolean; CONST ExpireText: string; aOnEnterKey: TEnterKeyFunc); overload; static;
    class function CreateFormParented(Parent: TWinControl): TfrmAboutApp; static;
  end;




IMPLEMENTATION {$R *.dfm}

USES
   System.SysUtils, LightVcl.Common.CenterControl, LightCore.AppData, LightCore.Debugger, LightVcl.Visual.AppData, LightVcl.Common.Dialogs, LightVcl.Common.ExecuteShell;




{ Creates and displays the About form modally, without license data.
  ShowEnterKey has no effect here: with no OnEnterKey there is nothing for the "Enter Key" button to do, so it stays hidden. }
class procedure TfrmAboutApp.CreateFormModal(ShowOrderNow, ShowEnterKey: Boolean);
begin
  CreateFormModal(ShowOrderNow, ShowEnterKey, '', NIL);
end;


{ Creates and displays the About form modally.
  Parameters:
    ShowOrderNow - Show the "Order Now" button
    ShowEnterKey - Show the "Enter Key" button. It is shown only if aOnEnterKey is assigned.
    ExpireText   - License state shown in lblExpire ('Lite edition', 'Registered'). Empty = label hidden.
    aOnEnterKey  - Called by the "Enter Key" button. The form variable is local to this method,
                   so these parameters are the ONLY way to deliver license data on this path. }
class procedure TfrmAboutApp.CreateFormModal(ShowOrderNow, ShowEnterKey: Boolean; CONST ExpireText: string; aOnEnterKey: TEnterKeyFunc);
var
  Form: TfrmAboutApp;
begin
  AppData.CreateForm(TfrmAboutApp, Form, FALSE, asFull);
  Form.SetLicense(ShowOrderNow, ExpireText, aOnEnterKey);
  Form.btnEnterKey.Visible:= ShowEnterKey AND Assigned(aOnEnterKey);
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
  No license data exists yet here: the form is instantiated inside CreateFormModal/CreateFormParented,
  so no caller can pass it before this event fires. We set the no-license defaults here;
  SetLicense applies the license UI when (and if) it is called later. }
procedure TfrmAboutApp.FormCreate(Sender: TObject);
begin
  // Prevent DFM resource conflict with other forms named 'TfrmAbout'
  // See: https://stackoverflow.com/questions/71518287/h2161-warning-duplicate-resource-type-10-rcdata-id-tfrmabout
  Assert(ClassName <> 'TfrmAbout', 'This form cannot be named TfrmAbout because of DFM resource conflict');

  // Hide license UI. SetLicense shows it when a license system is provided.
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
  ExpireText caption is painted on an invisible label.
  The "Enter Key" button is shown only when aOnEnterKey is assigned. }
procedure TfrmAboutApp.SetLicense(OrderNow: Boolean; CONST ExpireText: string; aOnEnterKey: TEnterKeyFunc);
begin
  FOnEnterKey:= aOnEnterKey;
  btnOrderNow.Visible:= OrderNow;
  btnEnterKey.Visible:= Assigned(FOnEnterKey);
  lblExpire.Caption  := ExpireText;
  lblExpire.Visible  := ExpireText <> '';
end;




{ Allows closing the form with Enter or Escape keys for quick dismissal. }
procedure TfrmAboutApp.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then Close;  // Enter
  if Key = #27 then Close;  // Escape
end;


{ Displays the license key entry dialog.
  The button is visible only when SetLicense received an OnEnterKey function. }
procedure TfrmAboutApp.btnEnterKeyClick(Sender: TObject);
begin
  Assert(Assigned(FOnEnterKey), 'btnEnterKey is visible but no OnEnterKey was passed to SetLicense');

  if FOnEnterKey()
  then MessageInfo('Key accepted. Please restart the program.')
  else MessageError('Key not accepted!');
end;


{ Opens the product order page in the default browser. }
procedure TfrmAboutApp.btnOrderNowClick(Sender: TObject);
begin
  ExecuteURL(AppData.ProductOrder);
end;


end.
