unit LightFmx.Common.Dialogs;

{=============================================================================================================
   2026.07.06
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Easy message boxes (FMX)
   Type: Asynchronous (non-blocking)
   Platform: Cross-platform (including Android)

   Limitation: TDialogServiceAsync ignores the icon (TMsgDlgType) on most platforms.
   Using this unit anyway for future icon support and API consistency with VCL version.

   TEST_MODE: when TAppDataCore.TEST_MODE is TRUE, no dialog is created — a headless test run never
   closes an async dialog, so it would leak. MessageYesNo still invokes its callback (with FALSE).
=============================================================================================================}

INTERFACE
{$I Frameworks.inc}

USES
  System.SysUtils,
  System.UITypes,
  FMX.DialogService.Async,
  LightCore.AppData;

{ Displays a generic message dialog asynchronously }
procedure GenericMessage(CONST MessageText: string; CONST Caption: string= ''; DlgType: TMsgDlgType= TMsgDlgType.mtCustom);

{ Asynchronous info, warning, and error dialogs }
procedure MessageInfo   (CONST MessageText: string; CONST Caption: string= '');
procedure MessageWarning(CONST MessageText: string; CONST Caption: string= '');
procedure MessageError  (CONST MessageText: string; CONST Caption: string= '');    overload;
procedure MessageError  (CONST MessageText, Where: string; CONST Caption: string); overload;

{ Same as MessageError, but ALSO writes the error into the log. See the note above the body. }
procedure MessageErrorLog(CONST MessageText: string; CONST LogText: string= ''; CONST Caption: string= '');

{ Displays a Yes/No confirmation dialog asynchronously.
  Callback receives True if user selects Yes, False otherwise.
  If Callback is nil, the dialog is shown but no action is taken on result.
  If MessageText is empty (or TEST_MODE is on), the dialog is NOT shown and Callback (if assigned) is invoked with False. }
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
  if TAppDataCore.TEST_MODE then EXIT;   // See the TEST_MODE note in the unit header

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


{ Shows the error to the user AND writes it into the log.

  Why both, and why it matters MORE here than in the VCL version:
    - There is no madExcept on Android or iOS, so nothing else records that this failed.
    - These dialogs are asynchronous. The code does not wait, and on a phone the box can be missed
      entirely when the app goes to background. The log line is what survives.
    - TRamLog saves itself to disk on its own (PeriodicLogSave.logbin, in the AppData folder), so
      the line is still there when the user complains weeks later.

  LogText: a short one-line version, for when the dialog text is long and multi-line.
           Left empty, the dialog text itself is logged.

  AppDataCore is checked for NIL: an application creates it by hand in its DPR, and the finalization
  of LightFmx.Common.AppData sets it back to NIL during shutdown.

  In TEST_MODE no dialog is created (GenericMessage returns immediately) but the log line is still written. }
procedure MessageErrorLog(CONST MessageText: string; CONST LogText: string = ''; CONST Caption: string = '');
begin
  if AppDataCore <> NIL then
   begin
     if LogText = ''
     then AppDataCore.LogError(MessageText)
     else AppDataCore.LogError(LogText);
   end;

  MessageError(MessageText, Caption);
end;


procedure MessageYesNo(CONST MessageText: string; CONST Caption: string= ''; CONST Callback: TProc<Boolean>= NIL);
VAR CombinedMsg: string;
begin
  if (MessageText = '') OR TAppDataCore.TEST_MODE then
    begin
      // Always notify the caller, otherwise a flow waiting on the callback would stall.
      // TEST_MODE (see unit header): no dialog; FALSE = the safe "No" answer.
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
