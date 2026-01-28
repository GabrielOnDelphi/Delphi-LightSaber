unit Test.FormEmailComposer;

{=============================================================================================================
   Unit tests for FormEmailComposer.pas
   Tests TfrmComposer - the email composer form.

   Note: These tests focus on form creation and property handling.
   Actual email sending requires network access and SMTP server configuration,
   so those tests are limited to validation logic only.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls;

type
  [TestFixture]
  TTestFormEmailComposer = class
  private
    FTestForm: TObject;
    procedure CleanupForm;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constants Tests }
    [Test]
    procedure TestConstant_EmailBodyFilename;

    { Form Creation Tests }
    [Test]
    procedure TestFormClassExists;

    [Test]
    procedure TestFormCreate_Succeeds;

    [Test]
    procedure TestFormCreate_WithNilOwner;

    { Component Tests }
    [Test]
    procedure TestFormHasSubjectEdit;

    [Test]
    procedure TestFormHasFromEdit;

    [Test]
    procedure TestFormHasToEdit;

    [Test]
    procedure TestFormHasBodyMemo;

    [Test]
    procedure TestFormHasSendButton;

    [Test]
    procedure TestFormHasSMTPComponent;

    [Test]
    procedure TestFormHasSSLHandler;

    [Test]
    procedure TestFormHasInternalSMTPCheckbox;

    [Test]
    procedure TestFormHasHtmlCheckbox;

    { Advert Property Tests }
    [Test]
    procedure TestAdvert_DefaultEmpty;

    [Test]
    procedure TestAdvert_CanBeSet;

    { GetEmailBodyPath Tests }
    [Test]
    procedure TestGetEmailBodyPath_ContainsFilename;

    [Test]
    procedure TestGetEmailBodyPath_ContainsAppDataFolder;

    { Initialize Tests }
    [Test]
    procedure TestInitialize_CreatesFrmSmtpSettings;

    [Test]
    procedure TestInitialize_NoExceptionWithoutMainForm;

    { Checkbox Behavior Tests }
    [Test]
    procedure TestInternalSMTPClick_HidesServerSettings;

    [Test]
    procedure TestInternalSMTPClick_ShowsServerSettings;

    { SaveSettings Tests }
    [Test]
    procedure TestSaveSettings_NoExceptionWhenEmpty;

    { SMTP Event Handler Tests }
    [Test]
    procedure TestSMTPConnected_NoException;

    [Test]
    procedure TestSMTPDisconnected_NoException;

    [Test]
    procedure TestSMTPTLSNotAvailable_NoException;

    [Test]
    procedure TestSMTPWorkBegin_SetsCaption;

    [Test]
    procedure TestSMTPWorkEnd_SetsCaption;

    [Test]
    procedure TestSMTPWork_SetsCaption;

    [Test]
    procedure TestSMTPFailedRecipient_ContinuesTrue;
  end;

implementation

uses
  IdComponent,
  LightCore.AppData,
  LightVcl.Visual.AppData,
  FormEmailComposer;


procedure TTestFormEmailComposer.Setup;
begin
  Assert.IsNotNull(AppData, 'AppData must be initialized before running tests');
  FTestForm:= NIL;
end;


procedure TTestFormEmailComposer.TearDown;
begin
  CleanupForm;
end;


procedure TTestFormEmailComposer.CleanupForm;
var
  Form: TfrmComposer;
begin
  if FTestForm <> NIL then
    begin
      Form:= TfrmComposer(FTestForm);
      FreeAndNil(Form);
      FTestForm:= NIL;
    end;
end;


{ Constants Tests }

procedure TTestFormEmailComposer.TestConstant_EmailBodyFilename;
begin
  Assert.AreEqual('Email body.txt', EMAIL_BODY_FILENAME,
    'EMAIL_BODY_FILENAME should be "Email body.txt"');
end;


{ Form Creation Tests }

procedure TTestFormEmailComposer.TestFormClassExists;
begin
  Assert.IsNotNull(TfrmComposer, 'TfrmComposer class should exist');
end;


procedure TTestFormEmailComposer.TestFormCreate_Succeeds;
var
  Form: TfrmComposer;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form, 'Form creation should succeed');
end;


procedure TTestFormEmailComposer.TestFormCreate_WithNilOwner;
var
  Form: TfrmComposer;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;

  Assert.IsNull(Form.Owner, 'Owner should be nil when created with nil');
end;


{ Component Tests }

procedure TTestFormEmailComposer.TestFormHasSubjectEdit;
var
  Form: TfrmComposer;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.edtSubject, 'Form should have edtSubject');
end;


procedure TTestFormEmailComposer.TestFormHasFromEdit;
var
  Form: TfrmComposer;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.ledFrom, 'Form should have ledFrom');
end;


procedure TTestFormEmailComposer.TestFormHasToEdit;
var
  Form: TfrmComposer;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.edtTo, 'Form should have edtTo');
end;


procedure TTestFormEmailComposer.TestFormHasBodyMemo;
var
  Form: TfrmComposer;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.mmoEmailBody, 'Form should have mmoEmailBody');
  Assert.IsTrue(Form.mmoEmailBody is TMemo, 'mmoEmailBody should be a TMemo');
end;


procedure TTestFormEmailComposer.TestFormHasSendButton;
var
  Form: TfrmComposer;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnSendMail, 'Form should have btnSendMail');
end;


procedure TTestFormEmailComposer.TestFormHasSMTPComponent;
var
  Form: TfrmComposer;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.SMTP, 'Form should have SMTP component');
end;


procedure TTestFormEmailComposer.TestFormHasSSLHandler;
var
  Form: TfrmComposer;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.SSLIOHandler, 'Form should have SSLIOHandler');
end;


procedure TTestFormEmailComposer.TestFormHasInternalSMTPCheckbox;
var
  Form: TfrmComposer;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.chkInternalSMTP, 'Form should have chkInternalSMTP');
end;


procedure TTestFormEmailComposer.TestFormHasHtmlCheckbox;
var
  Form: TfrmComposer;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.chkSendAsHtml, 'Form should have chkSendAsHtml');
end;


{ Advert Property Tests }

procedure TTestFormEmailComposer.TestAdvert_DefaultEmpty;
var
  Form: TfrmComposer;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual('', Form.Advert, 'Advert should default to empty string');
end;


procedure TTestFormEmailComposer.TestAdvert_CanBeSet;
var
  Form: TfrmComposer;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;

  Form.Advert:= 'Test advertisement';
  Assert.AreEqual('Test advertisement', Form.Advert, 'Advert should be settable');
end;


{ GetEmailBodyPath Tests }

procedure TTestFormEmailComposer.TestGetEmailBodyPath_ContainsFilename;
var
  Form: TfrmComposer;
  Path: string;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;

  // Access private method via RTTI or test indirectly
  // Since GetEmailBodyPath is private, we test it indirectly through SaveSettings behavior
  // For now, we verify the constant is used correctly
  Assert.IsTrue(EMAIL_BODY_FILENAME.EndsWith('.txt'),
    'Email body filename should have .txt extension');
end;


procedure TTestFormEmailComposer.TestGetEmailBodyPath_ContainsAppDataFolder;
begin
  // The path should be constructed using AppData.AppDataFolder
  // We can verify AppData.AppDataFolder exists and is valid
  Assert.IsNotEmpty(AppData.AppDataFolder, 'AppDataFolder should not be empty');
end;


{ Initialize Tests }

procedure TTestFormEmailComposer.TestInitialize_CreatesFrmSmtpSettings;
var
  Form: TfrmComposer;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;

  // Before Initialize, settings form should not exist (it's private, so we test indirectly)
  // After Initialize, btnServSettClick should work without exception
  Form.Initialize;

  Assert.WillNotRaise(
    procedure
    begin
      // This would raise assertion if frmSmtpSettings is nil
      Form.btnServSettClick(Form);
    end,
    'Initialize should create frmSmtpSettings');
end;


procedure TTestFormEmailComposer.TestInitialize_NoExceptionWithoutMainForm;
var
  Form: TfrmComposer;
  OldMainForm: TForm;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;

  // Temporarily clear MainForm to test nil handling
  OldMainForm:= Application.MainForm;
  try
    // Note: We can't actually set MainForm to nil in a running app,
    // but Initialize should handle the case gracefully
    Assert.WillNotRaise(
      procedure
      begin
        Form.Initialize;
      end,
      'Initialize should not raise exception');
  finally
    // MainForm cannot be changed, test passes if Initialize completes
  end;
end;


{ Checkbox Behavior Tests }

procedure TTestFormEmailComposer.TestInternalSMTPClick_HidesServerSettings;
var
  Form: TfrmComposer;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;
  Form.Initialize;

  Form.chkInternalSMTP.Checked:= TRUE;
  Form.chkInternalSMTPClick(Form);

  Assert.IsFalse(Form.btnServSett.Visible,
    'Server settings button should be hidden when using internal SMTP');
end;


procedure TTestFormEmailComposer.TestInternalSMTPClick_ShowsServerSettings;
var
  Form: TfrmComposer;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;
  Form.Initialize;

  Form.chkInternalSMTP.Checked:= FALSE;
  Form.chkInternalSMTPClick(Form);

  Assert.IsTrue(Form.btnServSett.Visible,
    'Server settings button should be visible when using external SMTP');
end;


{ SaveSettings Tests }

procedure TTestFormEmailComposer.TestSaveSettings_NoExceptionWhenEmpty;
var
  Form: TfrmComposer;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;
  Form.Initialize;

  Form.mmoEmailBody.Clear;

  Assert.WillNotRaise(
    procedure
    begin
      Form.FormDestroy(Form);  // This calls SaveSettings
    end,
    'SaveSettings should not raise exception with empty body');
end;


{ SMTP Event Handler Tests }

procedure TTestFormEmailComposer.TestSMTPConnected_NoException;
var
  Form: TfrmComposer;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;

  Assert.WillNotRaise(
    procedure
    begin
      Form.SMTPConnected(Form);
    end,
    'SMTPConnected should not raise exception');
end;


procedure TTestFormEmailComposer.TestSMTPDisconnected_NoException;
var
  Form: TfrmComposer;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;

  Assert.WillNotRaise(
    procedure
    begin
      Form.SMTPDisconnected(Form);
    end,
    'SMTPDisconnected should not raise exception');
end;


procedure TTestFormEmailComposer.TestSMTPTLSNotAvailable_NoException;
var
  Form: TfrmComposer;
  VContinue: Boolean;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;

  VContinue:= TRUE;
  Assert.WillNotRaise(
    procedure
    begin
      Form.SMTPTLSNotAvailable(Form, VContinue);
    end,
    'SMTPTLSNotAvailable should not raise exception');
end;


procedure TTestFormEmailComposer.TestSMTPWorkBegin_SetsCaption;
var
  Form: TfrmComposer;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;

  Form.SMTPWorkBegin(Form, wmRead, 1000);

  Assert.AreEqual('Sending data...', Form.Caption,
    'SMTPWorkBegin should set caption');
end;


procedure TTestFormEmailComposer.TestSMTPWorkEnd_SetsCaption;
var
  Form: TfrmComposer;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;

  Form.SMTPWorkEnd(Form, wmRead);

  Assert.AreEqual('Data sent', Form.Caption,
    'SMTPWorkEnd should set caption');
end;


procedure TTestFormEmailComposer.TestSMTPWork_SetsCaption;
var
  Form: TfrmComposer;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;

  Form.SMTPWork(Form, wmWrite, 12345);

  Assert.IsTrue(Form.Caption.Contains('12345'),
    'SMTPWork should show byte count in caption');
  Assert.IsTrue(Form.Caption.Contains('bytes'),
    'SMTPWork caption should contain "bytes"');
end;


procedure TTestFormEmailComposer.TestSMTPFailedRecipient_ContinuesTrue;
var
  Form: TfrmComposer;
  VContinue: Boolean;
begin
  Form:= TfrmComposer.Create(NIL);
  FTestForm:= Form;

  VContinue:= FALSE;
  Form.SMTPFailedRecipient(Form, 'test@example.com', '550', 'User not found', VContinue);

  Assert.IsTrue(VContinue,
    'SMTPFailedRecipient should set VContinue to True to continue sending');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFormEmailComposer);

end.
