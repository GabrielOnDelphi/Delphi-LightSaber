UNIT FormEmailComposer;

{TODO: Add proxy support }
{TODO: Add BCC uspport }
{TODO: Automatically save the user text in FormEmailComposer.mmoEmailBody }

{ SSL version mismatch.
  Solved: http://stackoverflow.com/questions/37484762/cannot-send-email-with-indy/37485204#37485204 }

{todo 5: use c:\MyProjects\Packages\Third party packages\uBase64.PAS to encrypt the password for the emailer}

INTERFACE

USES
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Vcl.Forms, cbAppDataForm, Dialogs, StdCtrls, Buttons, ActnList, Menus, ComCtrls, ExtCtrls, Spin,  ExtDlgs, System.Actions,
  IdSSL, IdComponent, IdSMTP, IdAntiFreeze,
  IdSSLOpenSSL, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack,
   cvCheckBox, cvSplitter, IdBaseComponent, IdTCPConnection, IdTCPClient, IdExplicitTLSClientServerBase,
  IdMessageClient, IdSMTPBase, cvPathEdit, FormEmailServer, Vcl.Mask;

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
    procedure SMTPStatus            (ASender: TObject; CONST AStatus: TIdStatus; const AStatusText: string);
    procedure SMTPTLSNotAvailable   (Asender: TObject; var VContinue: Boolean);
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
  public
    Advert: string;
    function  SendEmail: Boolean;
    procedure Initialize;
  end;



IMPLEMENTATION {$R *.dfm}

USES
  cvIniFile, ccCore, csSystem, cbDialogs, ccINIFile, cbAppDataForm, cmSound, cbAppData, ccIO, ccTextFile, cmIO, cmIO.Win, ciEmailSender;


procedure TfrmComposer.Initialize; { This will create also the frmSmtpSettings }
begin
 frmSmtpSettings:= TfrmSmtpSettings.Create(Self);  { Freed by self }
 frmSmtpSettings.Initialize;

 LoadForm(Self);
 Font:= Application.MainForm.Font;

 { RELOAD EMAIL BODY }
 if FileExists(AppData.AppDataFolder+ 'Email body.txt')
 then mmoEmailBody.Lines.LoadFromFile(AppData.AppDataFolder+ 'Email body.txt');

 chkInternalSMTPClick(Self);
end;


procedure TfrmComposer.FormDestroy(Sender: TObject);
begin
 SaveSettings;
end;


procedure TfrmComposer.btnSaveIniClick(Sender: TObject);
begin
 SaveSettings;
end;


procedure TfrmComposer.SaveSettings;
begin
 SaveForm(Self);
 mmoEmailBody.Lines.SaveToFile(AppData.AppDataFolder+ 'Email body.txt');  { AUTO SAVE EMAIL BODY }
end;



{--------------------------------------------------------------------------------------------------
   SEND EMAIL
--------------------------------------------------------------------------------------------------}
function TfrmComposer.SendEmail: Boolean;
VAR Body: string;
begin
 Result:= FALSE;

 { Check composer fields }
 if edtSubject.Text = '' then
  begin
   MesajWarning('Please enter a subject...');
   EXIT;
  end;

 if ledFrom.Text = '' then
  begin
   MesajWarning('Please enter your email address...');
   EXIT;
  end;

 //ToDo 5: validate each email
 if edtTo.Text = '' then
  begin
   MesajWarning('Where do you want to send the email? Please fill in the field...');
   EXIT;
  end;

 if chkInternalSMTP.Checked
 then frmSmtpSettings.UseInternalMailer(SMTP)
 else
   begin
    frmSmtpSettings.UseExternalMailer(SMTP);

    { Check SMTP server settings }
    if NOT frmSmtpSettings.IsValid(SMTP) then
     begin
       MesajWarning('Please check the email server settings.');
       frmSmtpSettings.Show;
       EXIT;
     end;
   end;

 CursorBusy;
 TRY
  { Add my own ads }
  Advert:= 'PS: I use BioniX Wallpaper to manage my images/wallpaper. I totally recommend it to you. www.BioniXWallpaper.com';

  if chkSendAsHtml.Checked
  then Body:= mmoEmailBody.Text+ LBRK+ Advert       { Send HTML email }
  else Body:= mmoEmailBody.Text+ LBRK+ Advert;

  Result:= ciEmailSender.SendEmail(SMTP, edtTo.Text, ledFrom.Text, edtSubject.Text, Body, '', edtAttachment.path, chkSendAsHtml.Checked);
 FINALLY
  CursorNotBusy;
 END;
end;



{ HOW TO SET GMAIL TO ALLOW MY APP TO SENT EMAILS:
  https://stackoverflow.com/questions/20337040/getting-error-while-sending-email-through-gmail-smtp-please-log-in-via-your-w }
procedure TfrmComposer.btnSendMailClick(Sender: TObject);
begin
 if chkInternalSMTP.Checked AND (frmSmtpSettings.ledHost.Text <> '')
 then
  begin
    Caption:= 'Sending email...';
    lblWarn.Visible:= NOT SendEmail;
    if NOT lblWarn.Visible
    then Caption:= 'Email sent'
    else Caption:= 'Failed to send email! Please see the log. Make sure this app is not blocked by your firewall.';
  end
 else
   btnServSettClick(Sender);
end;



{--------------------------------------------------------------------------------------------------
   GUI
--------------------------------------------------------------------------------------------------}
procedure TfrmComposer.btnServSettClick(Sender: TObject);
begin
 Assert(frmSmtpSettings <> NIL);
 frmSmtpSettings.Show;
end;


procedure TfrmComposer.chkInternalSMTPClick(Sender: TObject);
begin
 btnServSett.Visible:= NOT chkInternalSMTP.Checked;
end;

{--------------------------------------------------------------------------------------------------
   SEND EVENTS
--------------------------------------------------------------------------------------------------}
procedure TfrmComposer.SMTPConnected(Sender: TObject);
begin
 AppData.LogVerb('Connected to SMTP server...');
end;


procedure TfrmComposer.SMTPDisconnected(Sender: TObject);
begin
 AppData.LogVerb('Disconnected from SMTP');
end;


procedure TfrmComposer.SMTPTLSNotAvailable(Asender: TObject; var VContinue: Boolean);
begin
 AppData.LogError('TSL not available.');
end;


procedure TfrmComposer.SMTPWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
 Caption:= 'Progress: '+ IntToStr(AWorkCount)+ 'byte';
end;


procedure TfrmComposer.SMTPWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
 Caption:= 'Sending data...';
end;


procedure TfrmComposer.SMTPWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
 Caption:= 'Data sent...';
end;


procedure TfrmComposer.SMTPFailedRecipient(Sender: TObject; const AAddress, ACode, AText: string; var VContinue: Boolean);
begin
 AppData.LogError(AAddress+ '   '+ ACode+ '   '+ AText);
 VContinue:= TRUE;
 Bip50;
end;


procedure TfrmComposer.SMTPStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: String);  { SMTP Status }
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


