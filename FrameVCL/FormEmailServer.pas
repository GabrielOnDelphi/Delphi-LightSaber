UNIT FormEmailServer;

{=============================================================================================================
   2026.01
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   SMTP SERVER SETTINGS FORM

   A universal form for configuring SMTP email server settings.
   Used by FormEmailComposer and other email-sending applications.

   USAGE:
     frmSmtpSettings:= TfrmSmtpSettings.Create(ParentForm);
     frmSmtpSettings.Initialize;  // REQUIRED - decrypts saved password
     frmSmtpSettings.Show;

   FEATURES:
     - Configurable SMTP host, port, username, password
     - TLS/SSL options (None, Implicit, Explicit, Required)
     - Gmail defaults preset
     - Password visibility toggle
     - Password encryption in INI file (XOR-based)

   SECURITY NOTE:
     Password is stored with simple XOR encoding - not cryptographically secure.
     Consider using stronger encryption for production applications.

   RATE LIMITS:
     Hostinger: max 60 emails/minute, 3000 emails/day

   Used in: Power Email Extractor, PingMail, BioniX Wallpaper, etc.
=============================================================================================================}

INTERFACE
{$DENYPACKAGEUNIT ON}  // Prevents unit from being placed in a package

{Hostinger: the max PHP emails per minute is 60 and your limit for emails per day is 3000 ? = 1 email at 24 sec  }

USES
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, LightVcl.Visual.AppDataForm,Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ExtCtrls,
  IdSMTP, IdExplicitTLSClientServerBase, Vcl.ComCtrls, LightVcl.Visual.RadioButton, InternetLabel, Vcl.Mask;

CONST
  DEFAULT_SMTP_HOST = 'smtp.Gmail.com';
  DEFAULT_SMTP_PORT = 587;

TYPE
  TfrmSmtpSettings = class(TLightForm)
    btnGMailDef   : TButton;
    btnShowPassword: TButton;  // Toggle password visibility
    edtPsw        : TLabeledEdit;
    edtUserName   : TLabeledEdit;
    grpSettings   : TGroupBox;
    grpTLS        : TGroupBox;  // TLS settings group
    lblPort       : TLabel;
    ledHost       : TLabeledEdit;
    radTslExpl    : TCubicRadioButton;
    radTslImplic  : TCubicRadioButton;
    radTslNone    : TCubicRadioButton;
    radTslRequire : TCubicRadioButton;
    spnPort       : TSpinEdit;
    inetAllowLessSecure: TInternetLabel;
    procedure btnShowPasswordClick(Sender: TObject);
    procedure btnGMailDefClick    (Sender: TObject);
    procedure FormDestroy         (Sender: TObject);
    procedure FormCreate          (Sender: TObject);
  private
    function GetTLSMode: TIdUseTLS;
  public
    procedure UseExternalMailer(SMTP: TIdSMTP);
    procedure UseInternalMailer(SMTP: TIdSMTP);
    function  IsValid(SMTP: TIdSMTP): Boolean;
    procedure Initialize;
  end;


IMPLEMENTATION {$R *.dfm}

USES
  LightCore.EncodeXOR, LightVcl.Visual.INIFile;


{ Form creation - initialization is deferred to Initialize method }
procedure TfrmSmtpSettings.FormCreate(Sender: TObject);
begin
  // Initialization is done in Initialize method after INI loading
end;


{ Initializes the form after creation and INI loading.
  IMPORTANT: Must be called after the form is created and INI settings are loaded.
  Decrypts the password that was stored encrypted in the INI file. }
procedure TfrmSmtpSettings.Initialize;
begin
  edtPsw.Text:= SimpleDecode(edtPsw.Text);
end;


{ Encrypts password before saving to INI file }
procedure TfrmSmtpSettings.FormDestroy(Sender: TObject);
begin
  edtPsw.Text:= SimpleEncode(edtPsw.Text);
  // Note: SaveForm is called automatically by AppData
end;


{ Returns the TLS mode based on which radio button is selected }
function TfrmSmtpSettings.GetTLSMode: TIdUseTLS;
begin
  if radTslImplic.Checked
  then Result:= utUseImplicitTLS
  else
    if radTslExpl.Checked
    then Result:= utUseExplicitTLS
    else
      if radTslRequire.Checked
      then Result:= utUseRequireTLS
      else Result:= utNoTLSSupport;  // Default if none selected
end;


{ Validates that SMTP settings are properly configured.
  Returns True if all required fields are filled and valid. }
function TfrmSmtpSettings.IsValid(SMTP: TIdSMTP): Boolean;
begin
  Assert(SMTP <> NIL, 'SMTP component cannot be nil');

  Result:= (Trim(SMTP.UserName) <> '') and
           (Trim(SMTP.UserName) <> 'YourAddress@Gmail.com') and  // Not the placeholder
           (Trim(SMTP.Password) <> '') and
           (Trim(SMTP.Host) <> '') and
           (SMTP.Port > 0) and
           (radTslNone.Checked or radTslImplic.Checked or radTslExpl.Checked or radTslRequire.Checked);
end;



{--------------------------------------------------------------------------------------------------
   SMTP CONFIGURATION
--------------------------------------------------------------------------------------------------}

{ Configures SMTP component with user-specified external server settings.
  Disconnects any existing connection before applying new settings. }
procedure TfrmSmtpSettings.UseExternalMailer(SMTP: TIdSMTP);
begin
  Assert(SMTP <> NIL, 'SMTP component cannot be nil');

  // Disconnect if already connected
  if SMTP.Connected
  then SMTP.Disconnect;

  // Apply settings from form
  SMTP.UseTLS:= GetTLSMode;
  SMTP.Password:= edtPsw.Text;
  SMTP.Username:= edtUserName.Text;
  SMTP.Port:= spnPort.Value;
  SMTP.Host:= ledHost.Text;
end;


{ Configures SMTP component with internal/default mailer settings.
  Note: Internal credentials should be configured externally for security. }
procedure TfrmSmtpSettings.UseInternalMailer(SMTP: TIdSMTP);
begin
  Assert(SMTP <> NIL, 'SMTP component cannot be nil');

  // Disconnect if already connected
  if SMTP.Connected
  then SMTP.Disconnect;

  SMTP.UseTLS:= utUseExplicitTLS;
  SMTP.Password:= '';  // Should be configured via external secure method
  SMTP.Username:= '';  // Should be configured via external secure method
  SMTP.Port:= DEFAULT_SMTP_PORT;
  SMTP.Host:= DEFAULT_SMTP_HOST;
end;


{ Fills in default Gmail SMTP settings as a starting point.
  User still needs to enter their own credentials. }
procedure TfrmSmtpSettings.btnGMailDefClick(Sender: TObject);
begin
  ledHost.Text:= DEFAULT_SMTP_HOST;
  spnPort.Value:= DEFAULT_SMTP_PORT;
  edtUserName.Text:= 'YourAddress@Gmail.com';  // Placeholder - user must change
  radTslExpl.Checked:= TRUE;
  inetAllowLessSecure.Visible:= TRUE;
end;





{--------------------------------------------------------------------------------------------------
   UI HELPERS
--------------------------------------------------------------------------------------------------}

{ Toggles password visibility between hidden (asterisks) and visible (plain text) }
procedure TfrmSmtpSettings.btnShowPasswordClick(Sender: TObject);
begin
  if edtPsw.PasswordChar = #0
  then edtPsw.PasswordChar:= '*'
  else edtPsw.PasswordChar:= #0;
end;


end.
