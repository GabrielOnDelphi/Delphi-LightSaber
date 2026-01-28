UNIT FormEmailComposer;

{=============================================================================================================
   Gabriel Moraru
   2026.01
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   EMAIL COMPOSER FORM

   A form for composing and sending emails via SMTP. Supports:
   - Internal SMTP server (configured via settings form)
   - HTML or plain text emails
   - File attachments
   - Auto-save of email body text

   USAGE:
     var Composer: TfrmComposer;
     Composer:= TfrmComposer.Create(Application);
     Composer.Initialize;  // REQUIRED - creates settings form and loads saved data
     Composer.Show;

   DEPENDENCIES:
     - Indy components (TIdSMTP, TIdSSLIOHandlerSocketOpenSSL)
     - FormEmailServer.pas (TfrmSmtpSettings)
     - OpenSSL DLLs for SSL/TLS support

   SSL ISSUES:
     If you get "SSL version mismatch" errors, see:
     http://stackoverflow.com/questions/37484762/cannot-send-email-with-indy

   TODO:
     - Add proxy support
     - Add BCC support
     - Encrypt SMTP password using Base64 or better encryption
=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.ActnList, Vcl.Menus, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Samples.Spin, Vcl.ExtDlgs, Vcl.Mask,
  IdSSL, IdComponent, IdSMTP, IdSSLOpenSSL, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack,
  IdBaseComponent, IdTCPConnection, IdTCPClient, IdExplicitTLSClientServerBase, IdMessageClient, IdSMTPBase,
  LightVcl.Visual.AppDataForm, LightVcl.Visual.CheckBox, LightVcl.Visual.Splitter, LightVcl.Visual.PathEdit,
  FormEmailServer;

CONST
  EMAIL_BODY_FILENAME = 'Email body.txt';  // File used to persist email body text

TYPE
  TfrmComposer = class(TLightForm)
    btnAttachment    : TBitBtn;
    btnSaveIni       : TButton;
    btnServSett      : TButton;
    lblWarn          : TLabel;
    edtBcc           : TLabeledEdit;
    ledFrom          : TLabeledEdit;
    edtTo            : TLabeledEdit;
    mmoEmailBody     : TMemo;
    pnlBottomToolBar : TPanel;
    pnlBottom        : TPanel;
    pnlTop           : TPanel;
    SMTP             : TIdSMTP;
    splitComposer    : TCubicSplitter;
    SSLIOHandler     : TIdSSLIOHandlerSocketOpenSSL;
    Panel1           : TPanel;
    edtSubject       : TLabeledEdit;
    btnSendMail      : TBitBtn;
    chkSendAsHtml    : TCubicCheckBox;
    edtAttachment    : TCubicPathEdit;
    chkInternalSMTP  : TCubicCheckBox;
    lblInfoAttachment: TLabel;
    procedure FormDestroy           (Sender: TObject);
    procedure SMTPConnected         (Sender: TObject);
    procedure SMTPDisconnected      (Sender: TObject);
    procedure SMTPFailedRecipient   (Sender: TObject; const AAddress, ACode, AText: string; var VContinue: Boolean);
    procedure SMTPStatus            (ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
    procedure SMTPTLSNotAvailable   (ASender: TObject; var VContinue: Boolean);
    procedure SMTPWork              (ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure SMTPWorkBegin         (ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    procedure SMTPWorkEnd           (ASender: TObject; AWorkMode: TWorkMode);
    procedure SslIOHandlerStatus    (ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
    procedure SslIOHandlerStatusInfo(const AMsg: string);
    procedure btnSaveIniClick       (Sender: TObject);
    procedure btnServSettClick      (Sender: TObject);
    procedure btnSendMailClick      (Sender: TObject);
    procedure chkInternalSMTPClick  (Sender: TObject);
  private
    frmSmtpSettings: TfrmSmtpSettings;
    procedure SaveSettings;
    function GetEmailBodyPath: string;
  public
    Advert: string;  // Optional advertisement/signature appended to emails
    function  SendEmail: Boolean;
    procedure Initialize;
  end;



IMPLEMENTATION {$R *.dfm}

USES
  System.IOUtils,
  LightVcl.Visual.INIFile, LightCore, LightCore.Time, LightCore.Types, LightVcl.Common.SystemTime, LightVcl.Common.Clipboard,
  LightVcl.Common.Dialogs, LightCore.INIFile, LightVcl.Common.Sound, LightCore.AppData, LightVcl.Visual.AppData,
  LightCore.IO, LightCore.TextFile, LightVcl.Common.IO, LightVcl.Internet.EmailSender, LightVcl.Common.System;


{ Returns the full path to the email body persistence file }
function TfrmComposer.GetEmailBodyPath: string;
begin
  Result:= TPath.Combine(AppData.AppDataFolder, EMAIL_BODY_FILENAME);
end;


{ Initializes the form and creates child forms.
  IMPORTANT: Must be called after form creation before showing.
  Creates the SMTP settings form and loads any saved email body text. }
procedure TfrmComposer.Initialize;
var
  EmailBodyPath: string;
begin
  // Create SMTP settings form (freed automatically via Owner)
  frmSmtpSettings:= TfrmSmtpSettings.Create(Self);
  frmSmtpSettings.Initialize;

  // Inherit font from main form if available
  if Application.MainForm <> NIL
  then Font:= Application.MainForm.Font;

  // Reload previously saved email body
  EmailBodyPath:= GetEmailBodyPath;
  if FileExists(EmailBodyPath) then
    try
      mmoEmailBody.Lines.LoadFromFile(EmailBodyPath);
    except
      on E: Exception do
        AppData.LogWarn('Could not load email body: ' + E.Message);
    end;

  chkInternalSMTPClick(Self);
end;


{ Saves settings when the form is destroyed }
procedure TfrmComposer.FormDestroy(Sender: TObject);
begin
  SaveSettings;
end;


{ Manual save button handler }
procedure TfrmComposer.btnSaveIniClick(Sender: TObject);
begin
  SaveSettings;
end;


{ Saves the email body text to disk for persistence across sessions }
procedure TfrmComposer.SaveSettings;
begin
  try
    mmoEmailBody.Lines.SaveToFile(GetEmailBodyPath);
  except
    on E: Exception do
      AppData.LogWarn('Could not save email body: ' + E.Message);
  end;
end;



{--------------------------------------------------------------------------------------------------
   SEND EMAIL
--------------------------------------------------------------------------------------------------}

{ Validates form fields and sends the email.
  Returns True if email was sent successfully, False otherwise.
  Shows warning dialogs for validation errors. }
function TfrmComposer.SendEmail: Boolean;
var
  Body: string;
begin
  Result:= FALSE;

  // Validate required fields
  if Trim(edtSubject.Text) = '' then
    begin
      MessageWarning('Please enter a subject...');
      EXIT;
    end;

  if Trim(ledFrom.Text) = '' then
    begin
      MessageWarning('Please enter your email address...');
      EXIT;
    end;

  // TODO: Add email address validation
  if Trim(edtTo.Text) = '' then
    begin
      MessageWarning('Where do you want to send the email? Please fill in the field...');
      EXIT;
    end;

  // Ensure settings form is initialized
  Assert(frmSmtpSettings <> NIL, 'Initialize must be called before SendEmail');

  // Configure SMTP based on internal/external setting
  if chkInternalSMTP.Checked
  then frmSmtpSettings.UseInternalMailer(SMTP)
  else
    begin
      frmSmtpSettings.UseExternalMailer(SMTP);

      // Validate external SMTP server settings
      if NOT frmSmtpSettings.IsValid(SMTP) then
        begin
          MessageWarning('Please check the email server settings.');
          frmSmtpSettings.Show;
          EXIT;
        end;
    end;

  LightVcl.Common.System.CursorBusy;
  try
    // Build email body with optional advertisement/signature
    if Advert <> ''
    then Body:= mmoEmailBody.Text + LBRK + Advert
    else Body:= mmoEmailBody.Text;

    Result:= LightVcl.Internet.EmailSender.SendEmail(
      SMTP,
      edtTo.Text,
      ledFrom.Text,
      edtSubject.Text,
      Body,
      '',  // CC (not implemented)
      edtAttachment.Path,
      chkSendAsHtml.Checked);
  finally
    CursorNotBusy;
  end;
end;



{ Sends the email when the Send button is clicked.
  If SMTP is not configured, opens the settings dialog instead.

  HOW TO SET GMAIL TO ALLOW MY APP TO SEND EMAILS:
  https://stackoverflow.com/questions/20337040/getting-error-while-sending-email-through-gmail-smtp-please-log-in-via-your-w }
procedure TfrmComposer.btnSendMailClick(Sender: TObject);
var
  Success: Boolean;
begin
  // If using external SMTP but no host configured, show settings
  if (NOT chkInternalSMTP.Checked) and (frmSmtpSettings.ledHost.Text = '') then
    begin
      btnServSettClick(Sender);
      EXIT;
    end;

  Caption:= 'Sending email...';
  Success:= SendEmail;

  lblWarn.Visible:= NOT Success;
  if Success
  then Caption:= 'Email sent'
  else Caption:= 'Failed to send email! Please see the log. Make sure this app is not blocked by your firewall.';
end;



{--------------------------------------------------------------------------------------------------
   GUI EVENTS
--------------------------------------------------------------------------------------------------}

{ Opens the SMTP server settings dialog }
procedure TfrmComposer.btnServSettClick(Sender: TObject);
begin
  Assert(frmSmtpSettings <> NIL, 'Initialize must be called first');
  frmSmtpSettings.Show;
end;


{ Toggles visibility of the server settings button based on internal SMTP checkbox }
procedure TfrmComposer.chkInternalSMTPClick(Sender: TObject);
begin
  btnServSett.Visible:= NOT chkInternalSMTP.Checked;
end;


{--------------------------------------------------------------------------------------------------
   SMTP EVENT HANDLERS
   These events provide feedback during the email sending process.
--------------------------------------------------------------------------------------------------}

procedure TfrmComposer.SMTPConnected(Sender: TObject);
begin
  AppData.LogVerb('Connected to SMTP server');
end;


procedure TfrmComposer.SMTPDisconnected(Sender: TObject);
begin
  AppData.LogVerb('Disconnected from SMTP server');
end;


procedure TfrmComposer.SMTPTLSNotAvailable(ASender: TObject; var VContinue: Boolean);
begin
  AppData.LogError('TLS not available on SMTP server');
  // VContinue defaults to False - connection will be aborted
end;


procedure TfrmComposer.SMTPWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  Caption:= 'Progress: ' + IntToStr(AWorkCount) + ' bytes';
end;


procedure TfrmComposer.SMTPWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  Caption:= 'Sending data...';
end;


procedure TfrmComposer.SMTPWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
  Caption:= 'Data sent';
end;


{ Called when a recipient address fails. Logs the error and continues to next recipient. }
procedure TfrmComposer.SMTPFailedRecipient(Sender: TObject; const AAddress, ACode, AText: string; var VContinue: Boolean);
begin
  AppData.LogError('Failed recipient: ' + AAddress + ' - Code: ' + ACode + ' - ' + AText);
  VContinue:= TRUE;  // Continue sending to remaining recipients
  Bip50;
end;


procedure TfrmComposer.SMTPStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
begin
  Caption:= AStatusText;
end;


procedure TfrmComposer.SslIOHandlerStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
begin
  Caption:= AStatusText;
end;


procedure TfrmComposer.SslIOHandlerStatusInfo(const AMsg: string);
begin
  Caption:= AMsg;
end;


end.