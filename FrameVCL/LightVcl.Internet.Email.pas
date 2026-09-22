UNIT LightVcl.Internet.Email;

{-------------------------------------------------------------------------------------------------------------
   2026.09.10
   www.GabrielMoraru.com

   Opens the user's external mail program.
   Windows only: MAPI and the VCL Application handle.

   The framework-neutral part of this unit (email validation, correction, extraction and sorting)
   lives in LightCore.Internet.Email.pas.

   Useful resources:
      Send email:                  http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_22095910.html?sfQueryTermInfo=1+address+email#a18190296
      Find Current Email Program:  http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_10106205.html?sfQueryTermInfo=1+address+email
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
   Winapi.Windows, Winapi.MAPI, Winapi.ShellAPI{ Required by OpenDefaultEmail },
   System.SysUtils,
   Vcl.Forms;


 { SYSTEM }
 function  OpenDefaultEmail(CONST Recipient, Subject, Mesaj: String): Cardinal;                             { This will open the default email program in 'Compose' mode }
 function  OpenDefaultEmailEx(CONST Subject, Body, FileName, SenderName, SenderEMail, RecipientName, RecipientEMail: AnsiString): Integer;
 procedure SendEmail            (CONST sTo, sSubject, sBody: string);




IMPLEMENTATION

Uses
   System.NetEncoding, LightCore.AppData, LightVcl.Common.ExecuteShell;



{--------------------------------------------------------------------------------------------------
                                     EMAIL
--------------------------------------------------------------------------------------------------}
procedure SendEmail(CONST sTo, sSubject, sBody: string);
VAR s: string;
begin
  // URL-encode each segment to prevent CRLF/query-string injection into the user's default mail client
  s := 'mailto:' + TNetEncoding.URL.Encode(sTo)
     + '?subject=' + TNetEncoding.URL.Encode(sSubject)
     + '&body='    + TNetEncoding.URL.Encode(sBody);
  ExecuteURL(s);
end;


function OpenDefaultEmail(CONST recipient, subject, mesaj: String): Cardinal;
VAR MailBody : String;
begin
 // URL-encode each segment to prevent CRLF/query-string injection into the mail client
 MailBody:= 'mailto:' + TNetEncoding.URL.Encode(recipient)
          + '?subject=' + TNetEncoding.URL.Encode(subject)
          + '&body='    + TNetEncoding.URL.Encode(mesaj);
 Result:= Winapi.ShellAPI.ShellExecute(vcl.Forms.Application.Handle, 'open', PChar(MailBody), NIL, NIL, SW_Normal);
(*
 Sleep(500);                                                                                       {give mail prog time to open}
 keybd_Event(VK_MENU, 0, 0, 0);                                                                    {get email prog menu}
 keybd_Event(ord('S'), 0, 0, 0);                                                                   {s for send}
 keybd_Event(ord('S'), 0, KEYEVENTF_KEYUP, 0);                                                     {click}
 keybd_Event(VK_MENU, 0, KEYEVENTF_KEYUP, 0);                                                      {exit menu}
*)
end;


function OpenDefaultEmailEx(CONST Subject, Body, FileName, SenderName, SenderEMail, RecipientName, RecipientEMail: AnsiString): Integer;
{
  This allows you to also add attachments
  You must add the MAPI unit in USES-clause
  Use it like this: OpenDefaultEmailEx('Re: mailing from Delphi', 'Welcome to www.test.com'#13#10'Dany', 'c:\autoexec.bat', 'your name', 'your@address.com', 'Dany', 'test@test.com')
}
VAR
  Message: TMapiMessage;
  lpSender, lpRecipient: TMapiRecipDesc;
  FileAttach: TMapiFileDesc;

  SM: TFNMapiSendMail;
  MAPIModule: HModule;
begin
  FillChar(Message, SizeOf(Message), 0);

  if (Subject <> '')
  then Message.lpszSubject := PAnsiChar(Subject);

  if (Body <> '')
  then Message.lpszNoteText := PAnsiChar(Body);

  if (SenderEmail <> '') then
   begin
    lpSender.ulRecipClass := MAPI_ORIG;
    if (SenderName = '')
    then lpSender.lpszName := PAnsiChar(SenderEMail)
    else lpSender.lpszName := PAnsiChar(SenderName);
    lpSender.lpszAddress := PAnsiChar(SenderEmail);
    lpSender.ulReserved := 0;
    lpSender.ulEIDSize := 0;
    lpSender.lpEntryID := nil;
    Message.lpOriginator := @lpSender;
   end;

  if (RecipientEmail <> '') then
   begin
    lpRecipient.ulRecipClass := MAPI_TO;
    if (RecipientName = '')
    then lpRecipient.lpszName := PAnsiChar(RecipientEMail)
    else lpRecipient.lpszName := PAnsiChar(RecipientName);

    lpRecipient.lpszAddress := PAnsiChar(RecipientEmail);
    lpRecipient.ulReserved := 0;
    lpRecipient.ulEIDSize := 0;
    lpRecipient.lpEntryID := nil;
    Message.nRecipCount := 1;
    Message.lpRecips := @lpRecipient;
   end
  else
    Message.lpRecips := nil;

  if (FileName = '') then
   begin
    Message.nFileCount := 0;
    Message.lpFiles := nil;
   end
  else
   begin
    FillChar(FileAttach, SizeOf(FileAttach), 0);
    FileAttach.nPosition := Cardinal($FFFFFFFF);
    FileAttach.lpszPathName := PAnsiChar(FileName);

    Message.nFileCount := 1;
    Message.lpFiles := @FileAttach;
   end;

  MAPIModule := LoadLibrary(PChar(MAPIDLL));
  if MAPIModule = 0
  then Result := -1
  else
    TRY
      @SM := GetProcAddress(MAPIModule, 'MAPISendMail');
      if @SM <> NIL
      then Result := SM(0, Application.Handle, Message, MAPI_DIALOG or MAPI_LOGON_UI, 0)
      else Result := 1;
    FINALLY
      FreeLibrary(MAPIModule);
    END;

  if Result <> 0
  then
    AppDataCore.LogError('OpenDefaultEmailEx: MAPISendMail failed with code '+ IntToStr(Result));
end;


end.
