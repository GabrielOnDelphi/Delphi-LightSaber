UNIT cbDialogsFMX;

{=============================================================================================================
   www.GabrielMoraru.com
   2024.05
   See Copyright file
--------------------------------------------------------------------------------------------------------------
   Advanced message boxes
=============================================================================================================}

INTERFACE
{$I Frameworks.inc}

USES
   System.SysUtils,
   System.UITypes, Generics.Collections;


 function  MesajGeneric   (CONST MessageText: string; Title: string= ''; Icon: Integer= -1): integer;         { 'Title' will appear in window's caption }
 procedure Mesaj          (CONST MessageText: string);
 procedure MesajInfo      (CONST MessageText: string; CONST Title: string= '');
 procedure MesajWarning   (CONST MessageText: string);
 procedure MesajError     (CONST MessageText: string);
 procedure MesajErrDetail (CONST MessageText, Where: string);
 function  MesajYesNo     (CONST MessageText: string; CONST Title: string= ''): Boolean;                      { Returns True if the user presses the YES btn }


IMPLEMENTATION

USES
   ccCore;


{============================================================================================================
   MESSAGES
============================================================================================================}

{ Displays a message box with an approriate icon (info, warning, error, ask).
  Cross-platform.

  IMPORTANT:
    Application.MessageBox uses the handle of the current active window.
    This can of course create problems if the current window (is not the main form and) is closed.
    The message box will also be closed. So, don't use it in a from created by a thread.

  Blocking
    The MessageBox function is not blocking per se, it merely creates a dialog box with its own message loop.
    To test, show the current time in a TTimer. It will still paint the time, even if a MessageBox is shown.
    However, a MessageBox is still synchronous so it will block a 'for' loop.
    In this case use FromAsyncMessage if you want a non-blocking message box.

  See:
    https://stackoverflow.com/questions/60241794/why-tapplication-messagebox-closes-automatically?noredirect=1#comment106557585_60241794
    https://stackoverflow.com/questions/15696885/why-does-a-messagebox-not-block-the-application-on-a-synchronized-thread  // PRINTED!
    https://stackoverflow.com/questions/1256963/if-messagebox-related-are-synchronous-why-doesnt-my-message-loop-freeze
    http://www.delphigroups.info/2/11/544013.html
    https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-messagebox?redirectedfrom=MSDN
 }
CONST
   MB_OK              = $00000000;  // Defined originally in Windows.pas but we don't want to include that platform unit here.
   MB_YESNO           = $00000004;
   MB_ICONERROR       = $00000010;
   MB_ICONQUESTION    = $00000020;
   MB_ICONWARNING     = $00000030;
   MB_ICONINFORMATION = $00000040;

function MesajGeneric(const MessageText: string; Title: string = ''; Icon: Integer = -1): Integer;
begin
  {$IFDEF FRAMEWORK_FMX}
  // FMX (Cross-platform)
  if MessageText = '' then
    Exit(0);
  if Title = ''
  then Title := Application.Title
  else Title := Application.Title+ ' - ' + Title;
  TDialogService.MessageDialog(MessageText,
    TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0,
    procedure(const AResult: TModalResult)
    begin
      // Handle the result of the dialog if necessary
    end);
  Result := 0; // FMX dialogs are asynchronous, so we return 0
  {$ENDIF}

  {$IFDEF LINUX}
  Result := 0; // No specific result handling for console

  // Linux console application
  if MessageText = '' then EXIT;
  if Title = ''
  then WriteLn(                MessageText)
  else WriteLn(Title + ' - ' + MessageText);
  {$ENDIF}
end;



procedure Mesaj(CONST MessageText: string);   { 'Title' will appear in window's caption } //todo: get rid of if. use MesajInfo instead
 begin
  MesajInfo(MessageText, '');
 end;



procedure MesajInfo(CONST MessageText: string; CONST Title: string= '');
begin
 MesajGeneric(MessageText, Title, -1);
end;



procedure MesajWarning(CONST MessageText: string);
begin
 MesajGeneric(MessageText, 'Warning', MB_ICONWARNING or MB_OK);
end;



procedure MesajError(CONST MessageText: string);                                                   { afiseaza un mesaj cu icon de eroare pe ecran. If the MessageText is empty then dispaly nothing }
begin
 MesajGeneric(MessageText, 'Error', MB_ICONERROR or MB_OK);
end;



procedure MesajErrDetail(CONST MessageText, Where: string);                                                    { afiseaza un mesaj cu icon de eroare pe ecran }
VAR sMsg: string;
begin
 sMsg:= MessageText+
         LBRK+ 'Please report this error to us and the exact steps to reproduce it and we will fix it.'+
         CRLF+ 'Hint: press Control+C to copy this message to clipboard.';

 MesajGeneric(sMsg, 'Error in '+  Where, MB_ICONERROR or MB_OK);
end;



//todo: rename this to  MesajAsk
{ Returns True if the user presses the YES btn }
function MesajYesNo(CONST MessageText: string; CONST Title: string= ''): Boolean;
begin
 if MessageText= ''
 then RAISE Exception.Create('No message provided for MesajYesNo() !');
 Result:= MesajGeneric(MessageText, Title, MB_ICONQUESTION or MB_YESNO) = mrYes;      { FUCKING IMPORTANT! Always check for mrYes, never for mrNo. This is why: http://capnbry.net/blog/?p=46 }
end;





end.
