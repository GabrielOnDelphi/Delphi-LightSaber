UNIT LightVcl.Internet.EmailSender;

{-------------------------------------------------------------------------------------------------------------
   Gabriel Moraru
   2026.01.31
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt

   Universal 'SendMail' function using Indy components.
   Supports plain text and HTML emails with optional embedded images and file attachments.

   Used in: Power Email Extractor, PingMail, BX

   Dependencies:
     - Indy components (TIdSMTP, TIdMessage, TIdMessageBuilder)
     - LightCore.AppData for error logging

   References:
     - www.indyproject.org/2005/08/17/html-messages
     - www.indyproject.org/2008/01/16/new-html-message-builder-class
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, IdTCPConnection, IdSMTP, IdMessage;

function SendEmail(SMTP: TIdSMTP; CONST AdrTo, AdrFrom, Subject, Body, HtmlImage, DownloadableAttachment: string; SendAsHtml: Boolean= FALSE): Boolean;

IMPLEMENTATION

USES
  IdMessageBuilder, LightCore.AppData;



{ Sends an email via the provided SMTP connection.
  Parameters:
    SMTP                   - Pre-configured TIdSMTP component (caller is responsible for setting server/credentials)
    AdrTo                  - Recipient email address(es), comma-separated for multiple
    AdrFrom                - Sender email address
    Subject                - Email subject line
    Body                   - Email body (plain text or HTML depending on SendAsHtml)
    HtmlImage              - Path to image file to embed in HTML email (ignored if SendAsHtml=False)
    DownloadableAttachment - Path to file to attach (can be empty)
    SendAsHtml             - True for HTML email, False for plain text
  Returns:
    True if email was sent successfully, False otherwise.
  Note:
    Errors are logged via AppDataCore.LogError, not raised as exceptions. }
function SendEmail;
VAR
  MailMessage: TIdMessage;
  MsgBuilder : TIdMessageBuilderHtml;
begin
 Result:= FALSE;
 Assert(SMTP <> NIL, 'SendEmail: SMTP parameter cannot be nil');

 MailMessage:= TIdMessage.Create(NIL);
 TRY
  MailMessage.ConvertPreamble:= TRUE;
  MailMessage.Encoding       := meDefault;
  MailMessage.Subject        := Subject;
  MailMessage.From.Address   := AdrFrom;
  MailMessage.Priority       := mpNormal;
  MailMessage.Recipients.EMailAddresses:= AdrTo;

  { Build email with optional HTML and attachments }
  MsgBuilder:= TIdMessageBuilderHtml.Create;
  TRY
    if SendAsHtml
    then MsgBuilder.Html.Text:= Body
    else MsgBuilder.PlainText.Text:= Body;

    { Embedded images are visible ONLY in HTML emails }
    if SendAsHtml AND FileExists(HtmlImage)
    then MsgBuilder.HtmlFiles.Add(HtmlImage);

    if FileExists(DownloadableAttachment)
    then MsgBuilder.Attachments.Add(DownloadableAttachment);

    MsgBuilder.FillMessage(MailMessage);
  FINALLY
    FreeAndNil(MsgBuilder);
  END;

  { Connect to SMTP server }
  TRY
    if NOT SMTP.Connected
    then SMTP.Connect;
  EXCEPT
    on E: Exception DO
     begin
      AppDataCore.LogError('Cannot connect to the email server.');
      AppDataCore.LogError(E.Message);
     end;
  END;

  { Send the email }
  if SMTP.Connected then
   TRY
     SMTP.Send(MailMessage);
     Result:= TRUE;
   EXCEPT
     on E: Exception DO
      begin
       AppDataCore.LogError('Connected to server but could not send email!');
       AppDataCore.LogError(E.Message);
      end;
   END;

  { Disconnect }
  if SMTP.Connected
  then SMTP.Disconnect;

 FINALLY
  FreeAndNil(MailMessage);
 END;
end;



end.
