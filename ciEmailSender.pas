UNIT ciEmailSender;

{-------------------------------------------------------------------------------------------------------------
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   See Copyright file

   Universal 'SendMail' function
   Used in: Power Email Extractor, PingMail, BX
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  SysUtils, IdTCPConnection, IdSMTP, IdMessage;

function SendEmail(SMTP: TIdSMTP; CONST AdrTo, AdrFrom, Subject, Body, HtmlImage, DownloadableAttachment: string; SendAsHtml: Boolean= FALSE): Boolean;

IMPLEMENTATION

USES
  IdMessageBuilder, ccAppData, cbAppDataVCL
;



function SendEmail;
VAR MailMessage: TIdMessage;
begin
 Result:= FALSE;
 Assert(SMTP <> NIL, 'SMTP in NIL!');

 MailMessage:= TIdMessage.Create(NIL);
 TRY
  MailMessage.ConvertPreamble:= TRUE;
  MailMessage.Encoding       := meDefault;
  MailMessage.Subject        := Subject;
  MailMessage.From.Address   := AdrFrom;
  MailMessage.Priority       := mpNormal;
  MailMessage.Recipients.EMailAddresses := AdrTo;

  {How to send multi-part/attachment emails with Indy:
  www.indyproject.org/2005/08/17/html-messages
  www.indyproject.org/2008/01/16/new-html-message-builder-class }
  WITH IdMessageBuilder.TIdMessageBuilderHtml.Create DO
  TRY
    if SendAsHtml
    then Html.Text := Body
    else PlainText.Text := Body;

    { This will be visible ONLY if the email contains HTML! }
    if SendAsHtml AND FileExists(HtmlImage)
    then HtmlFiles.Add(HtmlImage);

    if FileExists(DownloadableAttachment)
    then Attachments.Add(DownloadableAttachment);
    FillMessage(MailMessage);
  FINALLY
    Free;
  END;

  { Connect }
  TRY
    if NOT SMTP.Connected
    then SMTP.Connect;
  EXCEPT
    on E: Exception DO
     begin
      AppData.LogError('Cannot connect to the email server.');
      AppData.LogError(E.Message);
     end;
  END;

  { Send mail }
  if SMTP.Connected then
   TRY
     SMTP.Send(MailMessage);
     Result:= TRUE;
   EXCEPT
     on E:Exception DO
      begin
       AppData.LogError('Connected to server but could not send email!');
       AppData.LogError(E.Message);
      end;
   END;

  if SMTP.Connected
  then SMTP.Disconnect;

 FINALLY
  FreeAndNil(MailMessage);
 END;
end;



end.
