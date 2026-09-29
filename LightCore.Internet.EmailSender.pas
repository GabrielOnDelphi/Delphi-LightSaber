UNIT LightCore.Internet.EmailSender;

{-------------------------------------------------------------------------------------------------------------
   Gabriel Moraru
   2026.09.10
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt

   Universal 'SendEmail' function using Indy components.
   Supports plain text and HTML emails with optional embedded images and file attachments.

   Used in: Power Email Extractor, PingMail, BX

   Dependencies:
     - Indy components (TIdSMTP, TIdMessage, TIdMessageBuilderHtml)
     - LightCore.AppData for error logging

   References:
     - www.indyproject.org/2005/08/17/html-messages
     - www.indyproject.org/2008/01/16/new-html-message-builder-class
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, IdTCPConnection, IdSMTP, IdMessage;

TYPE
  { Raised by SendEmail when SMTP connect or send fails.
    The original Indy exception has already been logged. }
  EEmailSendError = class(Exception);

function SendEmail(
  SMTP: TIdSMTP;
  CONST AdrTo, AdrFrom, Subject, Body: string;
  CONST HtmlImage: string = '';
  CONST DownloadableAttachment: string = '';
  SendAsHtml: Boolean = FALSE): Boolean;

IMPLEMENTATION

USES
  IdMessageBuilder, LightCore.AppData;



{ Parameters:
    SMTP                   - Pre-configured TIdSMTP component (caller is responsible for setting server/credentials)
    AdrTo                  - Recipient email address(es), comma-separated for multiple
    Body                   - Email body (plain text or HTML depending on SendAsHtml)
    HtmlImage              - Path to image file to embed in HTML email (ignored if SendAsHtml=False or empty)
    DownloadableAttachment - Path to file to attach (can be empty)
  Note:
    Result is never False on a normal return path - failures raise EEmailSendError. }
function SendEmail(
  SMTP: TIdSMTP;
  CONST AdrTo, AdrFrom, Subject, Body: string;
  CONST HtmlImage: string;
  CONST DownloadableAttachment: string;
  SendAsHtml: Boolean): Boolean;
VAR
  MailMessage: TIdMessage;
  MsgBuilder : TIdMessageBuilderHtml;
begin
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

    { Embedded images are visible ONLY in HTML emails. }
    if SendAsHtml AND (HtmlImage <> '') AND FileExists(HtmlImage)
    then MsgBuilder.HtmlFiles.Add(HtmlImage);

    if (DownloadableAttachment <> '') AND FileExists(DownloadableAttachment)
    then MsgBuilder.Attachments.Add(DownloadableAttachment);

    MsgBuilder.FillMessage(MailMessage);
  FINALLY
    FreeAndNil(MsgBuilder);
  END;

  TRY
    { Connect to SMTP server }
    TRY
      if NOT SMTP.Connected
      then SMTP.Connect;
    EXCEPT
      on E: Exception DO
       begin
        AppDataCore.LogError('Cannot connect to the email server: ' + E.Message);
        raise EEmailSendError.Create('Cannot connect to the email server: ' + E.Message);
       end;
    END;

    { Send the email }
    TRY
      SMTP.Send(MailMessage);
      Result:= TRUE;
    EXCEPT
      on E: Exception DO
       begin
        AppDataCore.LogError('Connected to server but could not send email: ' + E.Message);
        raise EEmailSendError.Create('Send failed: ' + E.Message);
       end;
    END;

  FINALLY
    { Disconnect. Log disconnect errors but don't propagate - they would mask a real send/connect failure. }
    if SMTP.Connected then
     TRY
      SMTP.Disconnect;
     EXCEPT
      on E: Exception DO
        AppDataCore.LogError('SMTP disconnect error: ' + E.Message);
     END;
  END;

 FINALLY
  FreeAndNil(MailMessage);
 END;
end;



end.
