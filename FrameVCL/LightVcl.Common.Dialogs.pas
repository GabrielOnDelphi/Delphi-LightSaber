UNIT LightVcl.Common.Dialogs;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Easy message boxes (for VCL only)

   Provides simple wrapper functions for displaying message dialogs with appropriate icons.
   All functions use Application.MessageBox internally except MesajTaskDlg which uses TTaskDialog.

   Functions:
     - MesajGeneric    : Base function with customizable icon
     - MessageInfo     : Information message (blue 'i' icon)
     - MessageWarning  : Warning message (yellow triangle icon)
     - MessageError    : Error message (red 'X' icon)
     - MesajYesNo      : Yes/No question dialog, returns Boolean
     - MesajErrDetail  : Error with "report this bug" text
     - MesajTaskDlg    : Uses modern TTaskDialog (Vista+)

   IMPORTANT:
     Application.MessageBox uses the handle of the current active window.
     This can create problems if the current window (not the main form) is closed.
     The message box will also be closed. So, don't use it in a form created by a thread.

   Blocking Behavior:
     MessageBox is synchronous - it blocks the calling code until dismissed.
     However, it has its own message loop so TTimer events and painting still work.
     Use FormAsyncMessage if you need a non-blocking message box.

   See:
     https://stackoverflow.com/questions/60241794/why-tapplication-messagebox-closes-automatically
     https://stackoverflow.com/questions/15696885/why-does-a-messagebox-not-block-the-application-on-a-synchronized-thread
     https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-messagebox
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.UITypes,
   Vcl.Themes, Vcl.Forms, Vcl.Dialogs;

 { Base message function - Title appears in window caption, Icon uses MB_* constants }
 function  MesajGeneric   (CONST MessageText: string; Title: string= ''; Icon: Integer= -1): Integer;

 { Convenience functions with predefined icons }
 procedure MessageInfo    (CONST MessageText: string; CONST Title: string= '');
 procedure MessageWarning (CONST MessageText: string; CONST Title: string= '');
 procedure MessageError   (CONST MessageText: string; CONST Title: string= '');

 { Extended error dialog with "report this bug" message }
 procedure MesajErrDetail (CONST MessageText, Where: string);

 { Yes/No question dialog - returns True if user clicks Yes }
 function  MesajYesNo     (CONST MessageText: string; CONST Title: string= ''): Boolean;

 { Modern task dialog (Vista+), falls back to MessageInfo on older systems }
 procedure MesajTaskDlg   (CONST MessageText, Title: string);

IMPLEMENTATION

USES
   LightCore;

CONST
   { MessageBox constants - defined here to avoid including Windows.pas }
   MB_OK              = $00000000;
   MB_YESNO           = $00000004;
   MB_ICONERROR       = $00000010;
   MB_ICONQUESTION    = $00000020;
   MB_ICONWARNING     = $00000030;
   MB_ICONINFORMATION = $00000040;


{-------------------------------------------------------------------------------------------------------------
   GENERIC MESSAGE
-------------------------------------------------------------------------------------------------------------}

{ Displays a message box with customizable icon.
  MessageText: The message to display (CRLF sequences are normalized)
  Title: Optional caption suffix (prepended with Application.Title)
  Icon: MB_ICON* constant combined with MB_OK or MB_YESNO. Use -1 for default info icon.
  Returns: Dialog result (mrYes, mrNo, mrOk, etc.) or 0 if message was empty }
function MesajGeneric(CONST MessageText: string; Title: string = ''; Icon: Integer = -1): Integer;
begin
  if MessageText = ''
  then EXIT(0);

  if Icon < 0
  then Icon:= MB_ICONINFORMATION or MB_OK;

  if Title = ''
  then Title:= Application.Title
  else Title:= Application.Title + ' - ' + Title;

  Result:= Application.MessageBox(PChar(CRLFToEnter(MessageText)), PChar(Title), Icon);
end;



{-------------------------------------------------------------------------------------------------------------
   CONVENIENCE FUNCTIONS
   These provide predefined icons for common message types.
-------------------------------------------------------------------------------------------------------------}

{ Information message with blue 'i' icon }
procedure MessageInfo(CONST MessageText: string; CONST Title: string = '');
begin
  MesajGeneric(MessageText, Title, MB_ICONINFORMATION or MB_OK);
end;


{ Warning message with yellow triangle icon.
  If Title is empty, uses 'Warning' as the caption suffix. }
procedure MessageWarning(CONST MessageText: string; CONST Title: string = '');
begin
  if Title = ''
  then MesajGeneric(MessageText, 'Warning', MB_ICONWARNING or MB_OK)
  else MesajGeneric(MessageText, Title, MB_ICONWARNING or MB_OK);
end;


{ Error message with red 'X' icon.
  If Title is empty, uses 'Error' as the caption suffix. }
procedure MessageError(CONST MessageText: string; CONST Title: string = '');
begin
  if Title = ''
  then MesajGeneric(MessageText, 'Error', MB_ICONERROR or MB_OK)
  else MesajGeneric(MessageText, Title, MB_ICONERROR or MB_OK);
end;



{-------------------------------------------------------------------------------------------------------------
   ERROR DETAIL
-------------------------------------------------------------------------------------------------------------}

{ Displays an error message with additional "report this bug" text.
  Where: Location identifier (e.g., function name, module name) shown in caption. }
procedure MesajErrDetail(CONST MessageText, Where: string);
var
  FullMsg: string;
begin
  FullMsg:= MessageText +
            LBRK + 'Please report this error to us and the exact steps to reproduce it and we will fix it.' +
            CRLF + 'Hint: press Control+C to copy this message to clipboard.';

  MesajGeneric(FullMsg, 'Error in ' + Where, MB_ICONERROR or MB_OK);
end;


{-------------------------------------------------------------------------------------------------------------
   YES/NO QUESTION
-------------------------------------------------------------------------------------------------------------}

{ Displays a Yes/No question dialog with question mark icon.
  Returns True if user clicks Yes, False if user clicks No.
  Raises exception if MessageText is empty (prevents accidental empty dialogs).

  Note: Always check for mrYes, not mrNo. If the dialog is closed via X button
  or Alt+F4, the result is neither Yes nor No. See: http://capnbry.net/blog/?p=46 }
function MesajYesNo(CONST MessageText: string; CONST Title: string = ''): Boolean;
begin
  if MessageText = ''
  then RAISE Exception.Create('No message provided for MesajYesNo() !');

  Result:= MesajGeneric(MessageText, Title, MB_ICONQUESTION or MB_YESNO) = mrYes;
end;



{-------------------------------------------------------------------------------------------------------------
   TASK DIALOG (Modern Vista+ Style)

   TTaskDialog provides a modern Windows Vista+ look but has limitations:
   - Doesn't handle long strings well (wraps too early)
   - Width is smaller than MessageBox
   - Use cxWidth property to customize size if needed

   For long messages, prefer MessageInfo/MesajGeneric instead.

   Falls back to MessageInfo on:
   - Windows XP and earlier (Win32MajorVersion < 6)
   - When UseLatestCommonDialogs is False
   - When visual styles are disabled

   See: https://stackoverflow.com/questions/4979556/how-to-use-the-ttaskdialog
-------------------------------------------------------------------------------------------------------------}
procedure MesajTaskDlg(CONST MessageText, Title: string);
var
  DlgTitle: string;
  Dlg: TTaskDialog;
begin
  if MessageText = ''
  then EXIT;

  { Check if TTaskDialog is available and visual styles are enabled }
  if (Win32MajorVersion >= 6)
  AND UseLatestCommonDialogs
  AND StyleServices.Enabled
  then begin
    Dlg:= TTaskDialog.Create(Application);
    try
      if Title = ''
      then DlgTitle:= Application.Title
      else DlgTitle:= Title;

      Dlg.Caption:= DlgTitle;
      Dlg.Title  := DlgTitle;
      Dlg.Text   := CRLFToEnter(MessageText);
      Dlg.CommonButtons:= [tcbOk];
      Dlg.Execute;
    finally
      FreeAndNil(Dlg);
    end;
  end
  else
    MessageInfo(MessageText, Title);
end;


end.
