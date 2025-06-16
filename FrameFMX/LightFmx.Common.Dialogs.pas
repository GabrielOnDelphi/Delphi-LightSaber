unit LightFmx.Common.Dialogs;
{=============================================================================================================
   www.GabrielMoraru.com
   2025.04
--------------------------------------------------------------------------------------------------------------
   Easy message boxes (FMX)
   Type: Asynchronous (non-blocking)
   Platform: Cross-platform (including Android)

   UNFORTUNATELLY this unit is pointless since TDialogServiceAsync ignores the icon (TMsgDlgType).
   Should be used anyway because one day I will add support for cool icons.
=============================================================================================================}

INTERFACE
{$I Frameworks.inc}

USES
  System.SysUtils,
  System.UITypes,
  FMX.DialogService.Async,  // Note the async unit for mobile support
  System.Classes;

{ Displays a generic message dialog asynchronously }
procedure GenericMessage  (CONST MessageText: string; CONST Caption: string= ''; DlgType: TMsgDlgType= TMsgDlgType.mtCustom);  

{ Asynchronous info, warning, and error dialogs }
procedure MessageInfo       (CONST MessageText: string; CONST Caption: string= '');
procedure MessageWarning    (CONST MessageText: string; CONST Caption: string= '');
procedure MessageError      (CONST MessageText: string; CONST Caption: string= '');    overload;
procedure MessageError      (CONST MessageText, Where: string; CONST Caption: string); overload;

{ Displays a Yes/No confirmation dialog asynchronously.
  The Callback is called with True if user selects Yes, False otherwise. }
procedure MessageYesNo(const MessageText: string; const Caption: string; const Callback: TProc<Boolean>);

IMPLEMENTATION
USES LightCore;


// Note: FMX.DialogService.ShowMessage exists but doesn't support icons.
procedure GenericMessage(CONST MessageText: string; CONST Caption: string= ''; DlgType: TMsgDlgType= TMsgDlgType.mtCustom);
begin
  // FMX.DialogService.MessageDlg primarily uses the first parameter for message text.
  // Native dialogs might use it for the title too.
  VAR CombinedMsg := Caption + CRLF+ CRLF+ MessageText;

  TDialogServiceAsync.MessageDialog(CombinedMsg, DlgType, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, -1, NIL { No code since there's no need for a callback here.});
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


procedure MessageYesNo(const MessageText: string; const Caption: string; const Callback: TProc<Boolean>);
begin
  // Although a caption parameter is provided here for consistency,
  // you might want to combine it with MessageText if needed.
  TDialogServiceAsync.MessageDialog(MessageText, TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbYes, -1,
    procedure(const AResult: TModalResult)
    begin
      Callback(AResult = mrYes);
    end);
end;


end.
