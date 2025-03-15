UNIT cbDialogsFMX;

{=============================================================================================================
   www.GabrielMoraru.com
   2025.02
--------------------------------------------------------------------------------------------------------------
   BLOCKING message boxes

   Not a good idea:
   https://stackoverflow.com/questions/79477595/what-will-happen-if-i-use-blocking-showmessage-in-fmx
=============================================================================================================}

INTERFACE
{$I Frameworks.inc}

USES
   System.SysUtils, System.UITypes, System.SyncObjs, Generics.Collections,
   FMX.Dialogs, FMX.Forms, FMX.DialogService, FMX.Types;

CONST
  cMB_YESNO           = $00000004;
  cMB_ICONERROR       = $00000010;
  cMB_ICONQUESTION    = $00000020;
  cMB_ICONWARNING     = $00000030;
  cMB_ICONINFORMATION = $00000040;

 function  MesajGeneric   (CONST MessageText: string; Title: string= ''; Icon: Integer= -1): Integer;
 procedure Mesaj          (CONST MessageText: string);
 procedure MesajInfo      (CONST MessageText: string; const Title: string = 'Info');
 procedure MesajWarning   (CONST MessageText: string; const Title: string = 'Warning');
 procedure MesajError     (CONST MessageText: string; const Title: string = 'Error');

 procedure MesajErrDetail (CONST MessageText, Where: string);
 function  MesajYesNo     (CONST MessageText: string; CONST Title: string= ''): Boolean;                      { Returns True if the user presses the YES btn }


IMPLEMENTATION

USES
   ccCore;


// Map VCL-style icons to FMX dialog types
function GetDialogTypeFromIcon(Icon: Integer): TMsgDlgType;
begin
  case Icon AND $F0 of   // Mask to get icon bits
    cMB_ICONERROR:       Result := TMsgDlgType.mtError;        // MB_ICONERROR
    cMB_ICONQUESTION:    Result := TMsgDlgType.mtConfirmation; // MB_ICONQUESTION
    cMB_ICONWARNING:     Result := TMsgDlgType.mtWarning;      // MB_ICONWARNING
    cMB_ICONINFORMATION: Result := TMsgDlgType.mtInformation;  // MB_ICONINFORMATION
  else
    Result := TMsgDlgType.mtInformation;            // Default
  end;
end;


{ 'Title' will appear in window's caption
   Platform behavior:
      * Windows: Blocks calling thread, UI remains responsive
      * Android: Blocks calling thread, dialog overlays app
      * macOS  : Blocks calling thread, native modal dialog }
function MesajGeneric(const MessageText: string; Title: string = ''; Icon: Integer = -1): Integer;
var
  DialogType: TMsgDlgType;
  Buttons: TMsgDlgButtons;
  DefaultButton: TMsgDlgBtn;
  Event: TEvent;
  ResultValue: Integer;
begin
  if MessageText = '' then Exit(0);

  if Title = ''
  then Title := Application.Title
  else Title := Application.Title + ' - ' + Title;

  DialogType := GetDialogTypeFromIcon(Icon);
  if (Icon and cMB_YESNO) = cMB_YESNO then    // MB_YESNO
    begin
      Buttons := [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo];
      DefaultButton := TMsgDlgBtn.mbNo;
    end
  else
    begin
      Buttons := [TMsgDlgBtn.mbOK];
      DefaultButton := TMsgDlgBtn.mbOK;
    end;

  Event:= TEvent.Create;
  ResultValue:= 0;
    
  try
    TDialogService.MessageDialog(MessageText,
        DialogType,
        Buttons,
        DefaultButton,
        0,
        procedure(const AResult: TModalResult)
        begin
            ResultValue := AResult;
            Event.SetEvent;
        end);

    // Wait for dialog to close
    Event.WaitFor(INFINITE);
    Result := ResultValue;
  finally
    Event.Free;
  end;

  {$IFDEF LINUX}
    //todo
    if MessageText = '' then Exit(0);
    if Title = ''
    then WriteLn(MessageText)
    else WriteLn(Title + ' - ' + MessageText);
    Result := 0;
  {$ENDIF}
end;



procedure Mesaj(const MessageText: string);
begin
  MesajInfo(MessageText, '');
end;

procedure MesajInfo(const MessageText: string; const Title: string = 'Info');
begin
  MesajGeneric(MessageText, Title, cMB_ICONINFORMATION);
end;

procedure MesajWarning(const MessageText: string; const Title: string = 'Warning');
begin
  MesajGeneric(MessageText, Title, cMB_ICONWARNING);
end;

procedure MesajError(const MessageText: string; const Title: string = 'Error');
begin
  MesajGeneric(MessageText, Title, cMB_ICONERROR);
end;

procedure MesajErrDetail(const MessageText, Where: string);
begin
    VAR sMsg := MessageText +
            sLineBreak + 'Please report this error to us and the exact steps to reproduce it and we will fix it.' +
            sLineBreak + 'Hint: press Control+C to copy this message to clipboard.';
  MesajGeneric(sMsg, 'Error in '+Where, cMB_ICONERROR);
end;



{ Returns True if the user presses the YES btn }
function MesajYesNo(const MessageText: string; const Title: string = ''): Boolean;
begin
  if MessageText = ''
  then raise Exception.Create('No message provided for MesajYesNo()!');

  Result := MesajGeneric(MessageText, Title, cMB_ICONQUESTION OR cMB_YESNO) = mrYes;  { FUCKING IMPORTANT! Always check for mrYes, never for mrNo. This is why: http://capnbry.net/blog/?p=46 }
end;

end.
