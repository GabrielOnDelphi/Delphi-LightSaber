unit LightFmx.DialogsDesktop;
{=============================================================================================================
   www.GabrielMoraru.com
   2025.04
--------------------------------------------------------------------------------------------------------------
   Easy message boxes (FMX)
   Type: Synchronous (blocking)
   Platform: Windows, OS X, iOS (no Android!)
=============================================================================================================}

INTERFACE
{$I Frameworks.inc}

USES
  System.SysUtils,
  System.UITypes,
  FMX.DialogService.Sync;


procedure GenericMessage  (CONST MessageText: string; CONST Caption: string= ''; DlgType: TMsgDlgType= TMsgDlgType.mtCustom);   { 'Title' will appear in window's caption }
procedure MesajInfo       (CONST MessageText: string; CONST Caption: string= '');
procedure MesajWarning    (CONST MessageText: string; CONST Caption: string= '');
procedure MesajError      (CONST MessageText: string; CONST Caption: string= '');
procedure MesajErrorSubmit(CONST MessageText, Where: string; CONST Caption: string= '');
function  MesajYesNo      (CONST MessageText: string; CONST Caption: string= ''): Boolean;


IMPLEMENTATION


// Note: FMX.DialogService.ShowMessage exists but doesn't support icons.
procedure GenericMessage(CONST MessageText: string; CONST Caption: string= ''; DlgType: TMsgDlgType= TMsgDlgType.mtCustom);
var
  CombinedMsg: string;
begin
  // FMX.DialogService.MessageDlg primarily uses the first parameter for message text.
  // Native dialogs might use it for the title too, or, ignore ACaption.
  // Prepending the caption is a common strategy if you want it visible. Test appearance.
  // LCombinedMsg := ACaption + System.SysUtils.sLineBreak + AMsg;
  // Or just use the message directly if the caption isn't critical for the dialog body:
  CombinedMsg := MessageText;

  // Call the standard FMX Dialog Service method. This is synchronous on Desktop platforms.
  TDialogServiceSync.MessageDialog(CombinedMsg, DlgType, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, -1);
end;





procedure MesajInfo(CONST MessageText: string; CONST Caption: string= '');
begin
  GenericMessage(MessageText, Caption, TMsgDlgType.mtInformation);
end;


procedure MesajWarning(CONST MessageText: string; CONST Caption: string= '');
begin
  GenericMessage(MessageText, Caption, TMsgDlgType.mtWarning);
end;


procedure MesajError(CONST MessageText: string; CONST Caption: string= '');
begin
  GenericMessage(MessageText, Caption, TMsgDlgType.mtError);
end;


procedure MesajErrorSubmit(const MessageText, Where: string; CONST Caption: string= '');
VAR sMsg: string;
begin
  sMsg:= MessageText+
         sLineBreak+sLineBreak+ 'Please report this error to us and the exact steps to reproduce it and we will fix it.'+
         sLineBreak+ 'Hint: press Control+C to copy this message to clipboard.';

  MesajError(sMsg, 'Error in '+  Where);
end;


{ Returns True if the user presses the YES btn }
function MesajYesNo(CONST MessageText: string; CONST Caption: string= ''): boolean;
begin
  Result:= TDialogServiceSync.MessageDialog(MessageText, TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbYes, -1) = ord(TMsgDlgBtn.mbYes);
end;


end.