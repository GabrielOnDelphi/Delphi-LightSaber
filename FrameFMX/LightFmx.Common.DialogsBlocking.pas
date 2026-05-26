unit LightFmx.Common.DialogsBlocking;

{=============================================================================================================
   2026.05.16
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Easy message boxes (FMX)
   Type: Synchronous on Windows/macOS/iOS. Pseudo-async fallback on Android.
   Platform: Cross-platform.

   FMX.DialogService.Sync is NOT shipped as a DCU for the Android target by Embarcadero
   (the source unit has a banner "Android does not support Synchronous Dialogs"; the
   compiled Android library set omits it). Calling TDialogServiceSync.MessageDialog from
   Android code therefore fails at link time with F2613.

   Android branch uses TDialogServiceAsync for the fire-and-forget Info/Warning/Error path.
   MessageYesNo on Android returns FALSE without showing UI: the only callers in Orinoco
   are admin-mode forms (Library Manager activation / reset) that are gated by
   AppData.RunningHome=TRUE, which is FALSE on customer Android devices. Truly blocking
   user prompts under Android need a redesign (callback-based UX) and are outside this
   unit's scope.
=============================================================================================================}

INTERFACE
{$I Frameworks.inc}

USES
  System.SysUtils,
  System.UITypes,
  {$IFDEF ANDROID}
  FMX.DialogService.Async;
  {$ELSE}
  FMX.DialogService.Sync;
  {$ENDIF}


{ Displays a generic message dialog synchronously (asynchronously on Android — caller does not block). }
procedure GenericMessage  (CONST MessageText: string; CONST Caption: string= ''; DlgType: TMsgDlgType= TMsgDlgType.mtCustom);

{ Info, warning, and error dialogs (sync on desktop/iOS, async on Android) }
procedure MessageInfo       (CONST MessageText: string; CONST Caption: string= '');
procedure MessageWarning    (CONST MessageText: string; CONST Caption: string= '');
procedure MessageError      (CONST MessageText: string; CONST Caption: string= '');    overload;
procedure MessageError      (CONST MessageText, Where: string; CONST Caption: string); overload;

{ Returns TRUE if user pressed YES (sync on desktop/iOS). On Android: returns FALSE without
  prompting — Android lacks a synchronous dialog primitive and refactoring every caller to
  a callback is a separate workstream. Admin-only call sites are RunningHome-gated. }
function  MessageYesNo      (CONST MessageText: string; CONST Caption: string= ''): Boolean;


IMPLEMENTATION
USES LightCore, LightCore.Time;


// Note: FMX.DialogService.ShowMessage exists but doesn't support icons.
procedure GenericMessage(CONST MessageText: string; CONST Caption: string= ''; DlgType: TMsgDlgType= TMsgDlgType.mtCustom);
begin
  // FMX.DialogService.MessageDlg primarily uses the first parameter for message text.
  // Native dialogs might use it for the title too.
  VAR CombinedMsg := Caption + CRLF+ CRLF+ MessageText;

  // Call the standard FMX Dialog Service method. Synchronous on desktop/iOS; non-blocking on Android.
  {$IFDEF ANDROID}
  TDialogServiceAsync.MessageDialog(CombinedMsg, DlgType, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, -1);
  {$ELSE}
  TDialogServiceSync.MessageDialog(CombinedMsg, DlgType, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, -1);
  {$ENDIF}
end;





procedure MessageInfo(CONST MessageText: string; CONST Caption: string= '');
begin
  GenericMessage(MessageText, Caption, TMsgDlgType.mtInformation);
end;


procedure MessageWarning(CONST MessageText: string; CONST Caption: string= '');
begin
  GenericMessage(MessageText, Caption, TMsgDlgType.mtWarning);
end;


procedure MessageError(CONST MessageText: string; CONST Caption: string= '');
begin
  GenericMessage(MessageText, Caption, TMsgDlgType.mtError);
end;


procedure MessageError(const MessageText, Where: string; CONST Caption: string);
VAR sMsg: string;
begin
  sMsg := MessageText + CRLF + CRLF +
          'Please report this error to us along with the exact steps to reproduce it, and we will fix it.' +
          sLineBreak + 'Hint: press Control+C to copy this message to clipboard.';

  MessageError(sMsg, 'Error in '+  Where);
end;


{ Returns True if the user presses the YES btn (desktop/iOS only).
  Android: returns FALSE without prompting — see header. }
function MessageYesNo(CONST MessageText: string; CONST Caption: string= ''): boolean;
begin
  {$IFDEF ANDROID}
  Result:= FALSE;
  {$ELSE}
  Result:= TDialogServiceSync.MessageDialog(MessageText, TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbYes, -1) = ord(TMsgDlgBtn.mbYes);
  {$ENDIF}
end;


end.
