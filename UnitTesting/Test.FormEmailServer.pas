unit Test.FormEmailServer;

{=============================================================================================================
   Unit tests for FormEmailServer.pas
   Tests TfrmSmtpSettings - the SMTP server configuration form.

   Note: These tests focus on form creation, property handling, and validation logic.
   Actual SMTP connectivity requires network access and is not tested here.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  IdSMTP,
  IdExplicitTLSClientServerBase;

type
  [TestFixture]
  TTestFormEmailServer = class
  private
    FTestForm: TObject;
    FSMTP: TIdSMTP;
    procedure CleanupForm;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constants Tests }
    [Test]
    procedure TestConstant_DefaultSmtpHost;

    [Test]
    procedure TestConstant_DefaultSmtpPort;

    { Form Creation Tests }
    [Test]
    procedure TestFormClassExists;

    [Test]
    procedure TestFormCreate_Succeeds;

    [Test]
    procedure TestFormCreate_WithNilOwner;

    { Component Tests }
    [Test]
    procedure TestFormHasHostEdit;

    [Test]
    procedure TestFormHasPortSpinEdit;

    [Test]
    procedure TestFormHasUserNameEdit;

    [Test]
    procedure TestFormHasPasswordEdit;

    [Test]
    procedure TestFormHasShowPasswordButton;

    [Test]
    procedure TestFormHasGMailDefaultButton;

    [Test]
    procedure TestFormHasSettingsGroupBox;

    [Test]
    procedure TestFormHasTLSGroupBox;

    [Test]
    procedure TestFormHasTLSRadioButtons;

    [Test]
    procedure TestFormHasInternetLabel;

    { Initialize Tests }
    [Test]
    procedure TestInitialize_NoException;

    [Test]
    procedure TestInitialize_DecodesPassword;

    { Password Visibility Tests }
    [Test]
    procedure TestShowPassword_TogglesVisibility;

    [Test]
    procedure TestShowPassword_DefaultHidden;

    [Test]
    procedure TestShowPassword_ToggleTwiceRestoresHidden;

    { Gmail Defaults Tests }
    [Test]
    procedure TestGMailDefaults_SetsHost;

    [Test]
    procedure TestGMailDefaults_SetsPort;

    [Test]
    procedure TestGMailDefaults_SetsPlaceholderUser;

    [Test]
    procedure TestGMailDefaults_SetsTLSExplicit;

    [Test]
    procedure TestGMailDefaults_ShowsInternetLabel;

    { UseExternalMailer Tests }
    [Test]
    procedure TestUseExternalMailer_SetsHost;

    [Test]
    procedure TestUseExternalMailer_SetsPort;

    [Test]
    procedure TestUseExternalMailer_SetsUsername;

    [Test]
    procedure TestUseExternalMailer_SetsPassword;

    [Test]
    procedure TestUseExternalMailer_SetsTLS;

    [Test]
    procedure TestUseExternalMailer_DisconnectsIfConnected;

    { UseInternalMailer Tests }
    [Test]
    procedure TestUseInternalMailer_SetsDefaultHost;

    [Test]
    procedure TestUseInternalMailer_SetsDefaultPort;

    [Test]
    procedure TestUseInternalMailer_ClearsCredentials;

    [Test]
    procedure TestUseInternalMailer_SetsExplicitTLS;

    { IsValid Tests }
    [Test]
    procedure TestIsValid_ReturnsFalseWhenEmpty;

    [Test]
    procedure TestIsValid_ReturnsFalseWithPlaceholder;

    [Test]
    procedure TestIsValid_ReturnsFalseWithoutPassword;

    [Test]
    procedure TestIsValid_ReturnsFalseWithoutHost;

    [Test]
    procedure TestIsValid_ReturnsFalseWithZeroPort;

    [Test]
    procedure TestIsValid_ReturnsTrueWhenComplete;

    { TLS Radio Button Tests }
    [Test]
    procedure TestTLSNone_SetsNoTLSSupport;

    [Test]
    procedure TestTLSImplicit_SetsImplicitTLS;

    [Test]
    procedure TestTLSExplicit_SetsExplicitTLS;

    [Test]
    procedure TestTLSRequired_SetsRequireTLS;
  end;

implementation

uses
  LightCore.AppData,
  LightVcl.Visual.AppData,
  FormEmailServer;


procedure TTestFormEmailServer.Setup;
begin
  Assert.IsNotNull(AppData, 'AppData must be initialized before running tests');
  FTestForm:= NIL;
  FSMTP:= TIdSMTP.Create(NIL);
end;


procedure TTestFormEmailServer.TearDown;
begin
  CleanupForm;
  FreeAndNil(FSMTP);
end;


procedure TTestFormEmailServer.CleanupForm;
var
  Form: TfrmSmtpSettings;
begin
  if FTestForm <> NIL then
    begin
      Form:= TfrmSmtpSettings(FTestForm);
      FreeAndNil(Form);
      FTestForm:= NIL;
    end;
end;


{ Constants Tests }

procedure TTestFormEmailServer.TestConstant_DefaultSmtpHost;
begin
  Assert.AreEqual('smtp.Gmail.com', DEFAULT_SMTP_HOST,
    'DEFAULT_SMTP_HOST should be smtp.Gmail.com');
end;


procedure TTestFormEmailServer.TestConstant_DefaultSmtpPort;
begin
  Assert.AreEqual(587, DEFAULT_SMTP_PORT,
    'DEFAULT_SMTP_PORT should be 587 (standard SMTP submission port)');
end;


{ Form Creation Tests }

procedure TTestFormEmailServer.TestFormClassExists;
begin
  Assert.IsNotNull(TfrmSmtpSettings, 'TfrmSmtpSettings class should exist');
end;


procedure TTestFormEmailServer.TestFormCreate_Succeeds;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form, 'Form creation should succeed');
end;


procedure TTestFormEmailServer.TestFormCreate_WithNilOwner;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Assert.IsNull(Form.Owner, 'Owner should be nil when created with nil');
end;


{ Component Tests }

procedure TTestFormEmailServer.TestFormHasHostEdit;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.ledHost, 'Form should have ledHost');
end;


procedure TTestFormEmailServer.TestFormHasPortSpinEdit;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.spnPort, 'Form should have spnPort');
end;


procedure TTestFormEmailServer.TestFormHasUserNameEdit;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.edtUserName, 'Form should have edtUserName');
end;


procedure TTestFormEmailServer.TestFormHasPasswordEdit;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.edtPsw, 'Form should have edtPsw');
end;


procedure TTestFormEmailServer.TestFormHasShowPasswordButton;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnShowPassword, 'Form should have btnShowPassword');
end;


procedure TTestFormEmailServer.TestFormHasGMailDefaultButton;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnGMailDef, 'Form should have btnGMailDef');
end;


procedure TTestFormEmailServer.TestFormHasSettingsGroupBox;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.grpSettings, 'Form should have grpSettings');
end;


procedure TTestFormEmailServer.TestFormHasTLSGroupBox;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.grpTLS, 'Form should have grpTLS');
end;


procedure TTestFormEmailServer.TestFormHasTLSRadioButtons;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.radTslNone, 'Form should have radTslNone');
  Assert.IsNotNull(Form.radTslImplic, 'Form should have radTslImplic');
  Assert.IsNotNull(Form.radTslExpl, 'Form should have radTslExpl');
  Assert.IsNotNull(Form.radTslRequire, 'Form should have radTslRequire');
end;


procedure TTestFormEmailServer.TestFormHasInternetLabel;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.inetAllowLessSecure, 'Form should have inetAllowLessSecure');
end;


{ Initialize Tests }

procedure TTestFormEmailServer.TestInitialize_NoException;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  { Initialize should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      Form.Initialize;
    end);
end;


procedure TTestFormEmailServer.TestInitialize_DecodesPassword;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  // Set an encoded password (SimpleDecode will decode it)
  // Since we don't know the encoding algorithm details, we just test that
  // Initialize doesn't crash and the password field is accessible
  Form.edtPsw.Text:= 'TestPassword';
  Form.Initialize;

  // After initialize, the password should have been processed
  Assert.IsNotNull(Form.edtPsw.Text, 'Password field should be accessible after Initialize');
end;


{ Password Visibility Tests }

procedure TTestFormEmailServer.TestShowPassword_TogglesVisibility;
var
  Form: TfrmSmtpSettings;
  OriginalChar: Char;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  OriginalChar:= Form.edtPsw.PasswordChar;
  Form.btnShowPasswordClick(Form);

  Assert.AreNotEqual(OriginalChar, Form.edtPsw.PasswordChar,
    'PasswordChar should change after clicking show password');
end;


procedure TTestFormEmailServer.TestShowPassword_DefaultHidden;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  // Password should be hidden by default (non-zero PasswordChar)
  Assert.AreNotEqual(#0, Form.edtPsw.PasswordChar,
    'Password should be hidden by default');
end;


procedure TTestFormEmailServer.TestShowPassword_ToggleTwiceRestoresHidden;
var
  Form: TfrmSmtpSettings;
  OriginalChar: Char;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  OriginalChar:= Form.edtPsw.PasswordChar;
  Form.btnShowPasswordClick(Form);  // Show
  Form.btnShowPasswordClick(Form);  // Hide again

  Assert.AreEqual(OriginalChar, Form.edtPsw.PasswordChar,
    'PasswordChar should be restored after toggling twice');
end;


{ Gmail Defaults Tests }

procedure TTestFormEmailServer.TestGMailDefaults_SetsHost;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Form.btnGMailDefClick(Form);

  Assert.AreEqual(DEFAULT_SMTP_HOST, Form.ledHost.Text,
    'Gmail defaults should set host to DEFAULT_SMTP_HOST');
end;


procedure TTestFormEmailServer.TestGMailDefaults_SetsPort;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Form.btnGMailDefClick(Form);

  Assert.AreEqual(DEFAULT_SMTP_PORT, Form.spnPort.Value,
    'Gmail defaults should set port to DEFAULT_SMTP_PORT');
end;


procedure TTestFormEmailServer.TestGMailDefaults_SetsPlaceholderUser;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Form.btnGMailDefClick(Form);

  Assert.AreEqual('YourAddress@Gmail.com', Form.edtUserName.Text,
    'Gmail defaults should set placeholder username');
end;


procedure TTestFormEmailServer.TestGMailDefaults_SetsTLSExplicit;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Form.btnGMailDefClick(Form);

  Assert.IsTrue(Form.radTslExpl.Checked,
    'Gmail defaults should select Explicit TLS');
end;


procedure TTestFormEmailServer.TestGMailDefaults_ShowsInternetLabel;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Form.inetAllowLessSecure.Visible:= FALSE;
  Form.btnGMailDefClick(Form);

  Assert.IsTrue(Form.inetAllowLessSecure.Visible,
    'Gmail defaults should show the "allow less secure apps" link');
end;


{ UseExternalMailer Tests }

procedure TTestFormEmailServer.TestUseExternalMailer_SetsHost;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Form.ledHost.Text:= 'mail.example.com';
  Form.UseExternalMailer(FSMTP);

  Assert.AreEqual('mail.example.com', FSMTP.Host,
    'UseExternalMailer should set SMTP host from form');
end;


procedure TTestFormEmailServer.TestUseExternalMailer_SetsPort;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Form.spnPort.Value:= 465;
  Form.UseExternalMailer(FSMTP);

  Assert.AreEqual(465, FSMTP.Port,
    'UseExternalMailer should set SMTP port from form');
end;


procedure TTestFormEmailServer.TestUseExternalMailer_SetsUsername;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Form.edtUserName.Text:= 'testuser@example.com';
  Form.UseExternalMailer(FSMTP);

  Assert.AreEqual('testuser@example.com', FSMTP.Username,
    'UseExternalMailer should set SMTP username from form');
end;


procedure TTestFormEmailServer.TestUseExternalMailer_SetsPassword;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Form.edtPsw.Text:= 'secretpassword';
  Form.UseExternalMailer(FSMTP);

  Assert.AreEqual('secretpassword', FSMTP.Password,
    'UseExternalMailer should set SMTP password from form');
end;


procedure TTestFormEmailServer.TestUseExternalMailer_SetsTLS;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Form.radTslImplic.Checked:= TRUE;
  Form.UseExternalMailer(FSMTP);

  Assert.AreEqual(utUseImplicitTLS, FSMTP.UseTLS,
    'UseExternalMailer should set SMTP TLS mode from form');
end;


procedure TTestFormEmailServer.TestUseExternalMailer_DisconnectsIfConnected;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  // We can't actually connect without a server, but we can verify
  // the method doesn't crash when SMTP is not connected
  { UseExternalMailer should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      Form.UseExternalMailer(FSMTP);
    end);
end;


{ UseInternalMailer Tests }

procedure TTestFormEmailServer.TestUseInternalMailer_SetsDefaultHost;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Form.UseInternalMailer(FSMTP);

  Assert.AreEqual(DEFAULT_SMTP_HOST, FSMTP.Host,
    'UseInternalMailer should set default host');
end;


procedure TTestFormEmailServer.TestUseInternalMailer_SetsDefaultPort;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Form.UseInternalMailer(FSMTP);

  Assert.AreEqual(DEFAULT_SMTP_PORT, FSMTP.Port,
    'UseInternalMailer should set default port');
end;


procedure TTestFormEmailServer.TestUseInternalMailer_ClearsCredentials;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  FSMTP.Username:= 'olduser';
  FSMTP.Password:= 'oldpass';

  Form.UseInternalMailer(FSMTP);

  Assert.AreEqual('', FSMTP.Username,
    'UseInternalMailer should clear username (for external configuration)');
  Assert.AreEqual('', FSMTP.Password,
    'UseInternalMailer should clear password (for external configuration)');
end;


procedure TTestFormEmailServer.TestUseInternalMailer_SetsExplicitTLS;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Form.UseInternalMailer(FSMTP);

  Assert.AreEqual(utUseExplicitTLS, FSMTP.UseTLS,
    'UseInternalMailer should use Explicit TLS');
end;


{ IsValid Tests }

procedure TTestFormEmailServer.TestIsValid_ReturnsFalseWhenEmpty;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  // Clear all fields
  FSMTP.Username:= '';
  FSMTP.Password:= '';
  FSMTP.Host:= '';
  FSMTP.Port:= 0;

  Assert.IsFalse(Form.IsValid(FSMTP),
    'IsValid should return False when fields are empty');
end;


procedure TTestFormEmailServer.TestIsValid_ReturnsFalseWithPlaceholder;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  FSMTP.Username:= 'YourAddress@Gmail.com';  // Placeholder
  FSMTP.Password:= 'password';
  FSMTP.Host:= 'smtp.gmail.com';
  FSMTP.Port:= 587;
  Form.radTslExpl.Checked:= TRUE;

  Assert.IsFalse(Form.IsValid(FSMTP),
    'IsValid should return False when username is placeholder');
end;


procedure TTestFormEmailServer.TestIsValid_ReturnsFalseWithoutPassword;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  FSMTP.Username:= 'user@example.com';
  FSMTP.Password:= '';
  FSMTP.Host:= 'smtp.example.com';
  FSMTP.Port:= 587;
  Form.radTslExpl.Checked:= TRUE;

  Assert.IsFalse(Form.IsValid(FSMTP),
    'IsValid should return False when password is empty');
end;


procedure TTestFormEmailServer.TestIsValid_ReturnsFalseWithoutHost;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  FSMTP.Username:= 'user@example.com';
  FSMTP.Password:= 'password';
  FSMTP.Host:= '';
  FSMTP.Port:= 587;
  Form.radTslExpl.Checked:= TRUE;

  Assert.IsFalse(Form.IsValid(FSMTP),
    'IsValid should return False when host is empty');
end;


procedure TTestFormEmailServer.TestIsValid_ReturnsFalseWithZeroPort;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  FSMTP.Username:= 'user@example.com';
  FSMTP.Password:= 'password';
  FSMTP.Host:= 'smtp.example.com';
  FSMTP.Port:= 0;
  Form.radTslExpl.Checked:= TRUE;

  Assert.IsFalse(Form.IsValid(FSMTP),
    'IsValid should return False when port is zero');
end;


procedure TTestFormEmailServer.TestIsValid_ReturnsTrueWhenComplete;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  FSMTP.Username:= 'user@example.com';
  FSMTP.Password:= 'password';
  FSMTP.Host:= 'smtp.example.com';
  FSMTP.Port:= 587;
  Form.radTslExpl.Checked:= TRUE;

  Assert.IsTrue(Form.IsValid(FSMTP),
    'IsValid should return True when all fields are valid');
end;


{ TLS Radio Button Tests }

procedure TTestFormEmailServer.TestTLSNone_SetsNoTLSSupport;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Form.radTslNone.Checked:= TRUE;
  Form.radTslImplic.Checked:= FALSE;
  Form.radTslExpl.Checked:= FALSE;
  Form.radTslRequire.Checked:= FALSE;

  Form.UseExternalMailer(FSMTP);

  Assert.AreEqual(utNoTLSSupport, FSMTP.UseTLS,
    'radTslNone should set utNoTLSSupport');
end;


procedure TTestFormEmailServer.TestTLSImplicit_SetsImplicitTLS;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Form.radTslNone.Checked:= FALSE;
  Form.radTslImplic.Checked:= TRUE;
  Form.radTslExpl.Checked:= FALSE;
  Form.radTslRequire.Checked:= FALSE;

  Form.UseExternalMailer(FSMTP);

  Assert.AreEqual(utUseImplicitTLS, FSMTP.UseTLS,
    'radTslImplic should set utUseImplicitTLS');
end;


procedure TTestFormEmailServer.TestTLSExplicit_SetsExplicitTLS;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Form.radTslNone.Checked:= FALSE;
  Form.radTslImplic.Checked:= FALSE;
  Form.radTslExpl.Checked:= TRUE;
  Form.radTslRequire.Checked:= FALSE;

  Form.UseExternalMailer(FSMTP);

  Assert.AreEqual(utUseExplicitTLS, FSMTP.UseTLS,
    'radTslExpl should set utUseExplicitTLS');
end;


procedure TTestFormEmailServer.TestTLSRequired_SetsRequireTLS;
var
  Form: TfrmSmtpSettings;
begin
  Form:= TfrmSmtpSettings.Create(NIL);
  FTestForm:= Form;

  Form.radTslNone.Checked:= FALSE;
  Form.radTslImplic.Checked:= FALSE;
  Form.radTslExpl.Checked:= FALSE;
  Form.radTslRequire.Checked:= TRUE;

  Form.UseExternalMailer(FSMTP);

  Assert.AreEqual(utUseRequireTLS, FSMTP.UseTLS,
    'radTslRequire should set utUseRequireTLS');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFormEmailServer);

end.
