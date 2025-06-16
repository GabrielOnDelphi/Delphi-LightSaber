UNIT LightVcl.Common.Dialogs;

{=============================================================================================================
   www.GabrielMoraru.com
   2025.04
--------------------------------------------------------------------------------------------------------------
   Easy message boxes (for VCL only)


  Displays a message box with an approriate icon (info, warning, error, ask).
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
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.UITypes, Generics.Collections,
   Vcl.Themes, Vcl.Forms, Vcl.Dialogs;

 function  MesajGeneric   (CONST MessageText: string; Title: string= ''; Icon: Integer= -1): integer;         { 'Title' will appear in window's caption }
 procedure MessageInfo    (CONST MessageText: string; CONST Caption: string= '');
 procedure MessageWarning (CONST MessageText: string; CONST Caption: string= '');
 procedure MessageError   (CONST MessageText: string; CONST Caption: string= '');

 procedure MesajErrDetail (CONST MessageText, Where: string);
 function  MesajYesNo     (CONST MessageText: string; CONST Title: string= ''): Boolean;                      { Returns True if the user presses the YES btn }

 procedure MesajTaskDlg   (CONST MessageText, Title: string);

IMPLEMENTATION

USES
   LightCore;

CONST
   MB_OK              = $00000000;  // Defined originally in Windows.pas but we don't want to include that platform unit here.
   MB_YESNO           = $00000004;
   MB_ICONERROR       = $00000010;
   MB_ICONQUESTION    = $00000020;
   MB_ICONWARNING     = $00000030;
   MB_ICONINFORMATION = $00000040;


function MesajGeneric(const MessageText: string; Title: string = ''; Icon: Integer = -1): Integer;
begin
  if Icon < 0 then Icon:= MB_ICONINFORMATION or MB_OK;
  if MessageText = '' then Exit(0);
  if Title = ''
  then Title := Application.Title
  else Title := Application.Title+ ' - ' + Title;
  Result:= Application.MessageBox(PCHAR(CRLFToEnter(MessageText)), PChar(Title), Icon); //icon =  MB_ICONINFORMATION or MB_OK
end;





procedure MessageInfo(CONST MessageText: string; CONST Caption: string= '');
begin
  MesajGeneric(MessageText, Caption, -1);
end;



procedure MessageWarning(const MessageText, Caption: string);
begin
  MesajGeneric(MessageText, 'Warning', MB_ICONWARNING or MB_OK);
end;



procedure MessageError(const MessageText, Caption: string);
begin
 MesajGeneric(MessageText, 'Error', MB_ICONERROR or MB_OK);
end;



procedure MesajErrDetail(CONST MessageText, Where: string);
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



{============================================================================================================
  There is a problem with TTaskDialog. It doesn't break long strings! So it is not good for showing long strings/debugging.

  When you have to display longer strings, MessageBox is much better than TTaskDialog because TTaskDialog will wrap your strings too soon (its width tends to be much smaller than MessageBox's width).     So, don't use TTaskDialog for 'normal' messages!
  But you can use TTaskDialog.cxWidth to customize the size of the dialog -> https://stackoverflow.com/questions/33302622/how-can-i-make-the-showmessage-dialog-wider-so-it-fits-the-text

  Documentation about TTaskDialog: http://stackoverflow.com/questions/4979556/how-to-use-the-ttaskdialog
============================================================================================================}
procedure MesajTaskDlg(CONST MessageText, Title: string);
VAR
   s: string;
   Dlg: TTaskDialog;
begin
 if MessageText= '' then EXIT;

 if (Win32MajorVersion >= 6)
 AND UseLatestCommonDialogs
 AND StyleServices.Enabled
 then
  begin
   Dlg:= TTaskDialog.Create(Application);
   TRY
     if Title= ''
     then s := Application.Title
     else s := Title;
     Dlg.Caption:= s;
     Dlg.Title  := s;
     Dlg.Text   := CRLFToEnter(MessageText);
     Dlg.CommonButtons := [tcbOk];
     Dlg.Execute;
   FINALLY
     FreeAndNil(Dlg);
   END
  end
 else
   MessageInfo(MessageText, Title);
end;



end.
