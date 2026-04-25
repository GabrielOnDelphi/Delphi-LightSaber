unit LightFmx.Common.Dialogs;

{=============================================================================================================
   2026.04.25
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Easy message boxes (FMX)
   Type: Asynchronous (non-blocking)
   Platform: Cross-platform (including Android)

   Limitation: TDialogServiceAsync ignores the icon (TMsgDlgType) on most platforms.
   Using this unit anyway for future icon support and API consistency with VCL version.
=============================================================================================================}

INTERFACE
{$I Frameworks.inc}

USES
  System.SysUtils,
  System.UITypes,
  FMX.DialogService.Async;

{ Displays a generic message dialog asynchronously }
procedure GenericMessage(CONST MessageText: string; CONST Caption: string= ''; DlgType: TMsgDlgType= TMsgDlgType.mtCustom);

{ Asynchronous info, warning, and error dialogs }
procedure MessageInfo   (CONST MessageText: string; CONST Caption: string= '');
procedure MessageWarning(CONST MessageText: string; CONST Caption: string= '');
procedure MessageError  (CONST MessageText: string; CONST Caption: string= '');    overload;
procedure MessageError  (CONST MessageText, Where: string; CONST Caption: string); overload;

{ Displays a Yes/No confirmation dialog asynchronously.
  Callback receives True if user selects Yes, False otherwise.
  If Callback is nil, the dialog is shown but no action is taken on result.
  If MessageText is empty, the dialog is NOT shown and Callback (if assigned) is invoked with False. }
procedure MessageYesNo(CONST MessageText: string; CONST Caption: string= ''; CONST Callback: TProc<Boolean>= NIL);

{ Resolves the caption used by the 3-arg MessageError overload.
  Exposed for unit testing. }
function  ResolveErrorCaption(CONST Where, Caption: string): string;

IMPLEMENTATION
USES LightCore;


{ Note: FMX.DialogService.ShowMessage exists but doesn't support icons.
  Native dialogs might use the message parameter for the title too. }
procedure GenericMessage(CONST MessageText: string; CONST Caption: string= ''; DlgType: TMsgDlgType= TMsgDlgType.mtCustom);
VAR CombinedMsg: string;
begin
  if MessageText = '' then EXIT;

  // Combine caption and message, avoiding leading blank lines when caption is empty
  if Caption = ''
  then CombinedMsg:= MessageText
  else CombinedMsg:= Caption+ LBRK+ MessageText;

  TDialogServiceAsync.MessageDialog(CombinedMsg, DlgType, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, -1, NIL);
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


function ResolveErrorCaption(CONST Where, Caption: string): string;
begin
  if Caption = ''
  then Result:= 'Error in ' + Where
  else Result:= Caption;
end;


procedure MessageError(CONST MessageText, Where: string; CONST Caption: string);
VAR FullMsg: string;
begin
  FullMsg:= MessageText +
            LBRK + 'Please report this error to us along with the exact steps to reproduce it, and we will fix it.' +
            CRLF + 'Hint: press Control+C to copy this message to clipboard.';

  MessageError(FullMsg, ResolveErrorCaption(Where, Caption));
end;


procedure MessageYesNo(CONST MessageText: string; CONST Caption: string= ''; CONST Callback: TProc<Boolean>= NIL);
VAR CombinedMsg: string;
begin
  if MessageText = '' then
    begin
      // Always notify caller, otherwise a flow waiting on the callback would stall.
      if Assigned(Callback)
      then Callback(False);
      EXIT;
    end;

  // Combine caption and message for consistency with GenericMessage
  if Caption = ''
  then CombinedMsg:= MessageText
  else CombinedMsg:= Caption + LBRK + MessageText;

  TDialogServiceAsync.MessageDialog(CombinedMsg, TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbYes, -1,
    procedure(CONST AResult: TModalResult)
    begin
      if Assigned(Callback)
      then Callback(AResult = mrYes);
    end);
end;


end.
