UNIT FormEmailServer;
{-------------------------------------------------------------------------------------------------------------
  2021.03.24
  Universal 'email server settings' form
  Used in: Power Email Extractor, PingMail, etc
-------------------------------------------------------------------------------------------------------------}
INTERFACE
{$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

{Hostinger: the max PHP emails per minute is 60 and your limit for emails per day is 3000 ? = 1 email at 24 sec  }

USES
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ExtCtrls,
  IdSMTP, IdExplicitTLSClientServerBase, Vcl.ComCtrls, cvRadioButton, InternetLabel;

type
  TfrmSmtpSettings = class(TForm)
    btnGMailDef   : TButton;
    Button1       : TButton;
    edtPsw        : TLabeledEdit;
    edtUserName   : TLabeledEdit;
    grpSettings   : TGroupBox;
    grpTSL        : TGroupBox;
    lblPort       : TLabel;
    ledHost       : TLabeledEdit;
    radTslExpl    : TCubicRadioButton;
    radTslImplic  : TCubicRadioButton;
    radTslNone    : TCubicRadioButton;
    radTslRequire : TCubicRadioButton;
    spnPort       : TSpinEdit;
    inetAllowLessSecure: TInternetLabel;
    procedure Button1Click     (Sender: TObject);
    procedure btnGMailDefClick (Sender: TObject);
    procedure FormDestroy      (Sender: TObject);
    procedure FormCreate       (Sender: TObject);
  private
  public
    procedure UseExternalMailer(SMTP: TIdSMTP);
    procedure UseInternalMailer(SMTP: TIdSMTP);
    function  IsValid(SMTP: TIdSMTP): Boolean;
    procedure Initialize;
  end;

VAR
  frmSmtpSettings: TfrmSmtpSettings;


IMPLEMENTATION {$R *.dfm}

USES cmEncodeXOR, cvIniFile;



procedure TfrmSmtpSettings.FormCreate(Sender: TObject);
begin
 // Let the main from call frmSettings.LoadIni
end;


procedure TfrmSmtpSettings.Initialize;
begin
 LoadForm(Self);
 edtPsw.Text:= SimpleDecode(edtPsw.Text);  // automatically decrypt AFTER loading from INI
end;


procedure TfrmSmtpSettings.FormDestroy(Sender: TObject);
begin
 edtPsw.Text:= Simpleencode(edtPsw.Text);   // encrypt the password BEFORE saving to INI!
 SaveForm(Self);
end;



CONST DemoEmail=  'smtp.Gmail.com';

function TfrmSmtpSettings.IsValid(SMTP: TIdSMTP): Boolean;
begin
 Result:= (SMTP.UserName > '') AND
          (SMTP.UserName <> DemoEmail) AND
          (SMTP.Password > '') AND
          (SMTP.Host > '') AND
          (SMTP.Port > 0) AND
          (radTslNone.Checked OR radTslImplic.Checked OR radTslExpl.Checked OR radTslRequire.Checked);
end;



{--------------------------------------------------------------------------------------------------
   Apply SMTP server settings
--------------------------------------------------------------------------------------------------}
procedure TfrmSmtpSettings.UseExternalMailer(SMTP: TIdSMTP);
begin
 Assert(SMTP <> NIL);

 if SMTP.Connected
 then SMTP.Disconnect;

 if radTslNone.Checked
 then SMTP.UseTLS:= utNoTLSSupport else
  if radTslImplic.Checked
  then SMTP.UseTLS:= utUseImplicitTLS else
   if radTslExpl.Checked
   then SMTP.UseTLS:= utUseExplicitTLS else
    if radTslRequire.Checked
    then SMTP.UseTLS:= utUseRequireTLS;

 SMTP.Password:= edtPsw.Text;
 SMTP.Username:= edtUserName.Text;
 SMTP.Port    := spnPort.Value;
 SMTP.Host    := ledHost.Text;
end;


procedure TfrmSmtpSettings.UseInternalMailer(SMTP: TIdSMTP);
begin
 SMTP.UseTLS  := utUseExplicitTLS;
 SMTP.Password:= '';
 SMTP.Username:= 'bionixwallpapermanager@gmail.com';
 SMTP.Port    := 587;
 SMTP.Host    := 'smtp.Gmail.com';
end;


procedure TfrmSmtpSettings.btnGMailDefClick(Sender: TObject);
begin
 ledHost.Text      := DemoEmail;
 spnPort.Value     := 587;
 edtUserName.Text  := 'YourAddress@Gmail.com';
 radTslExpl.Checked:= TRUE;
 inetAllowLessSecure.Visible:= TRUE;
end;







{--------------------------------------------------------------------------------------------------
   OTHERS
--------------------------------------------------------------------------------------------------}

procedure TfrmSmtpSettings.Button1Click(Sender: TObject);
begin
 if edtPsw.PasswordChar= #0
 then edtPsw.PasswordChar:= '*'
 else edtPsw.PasswordChar:= #0;
end;




end.
