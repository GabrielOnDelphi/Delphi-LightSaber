unit MainForm;

interface

uses
  Winapi.Windows,
  System.UITypes, System.Classes, System.SysUtils,
  FMX.Dialogs, // Warning: MessageDlg is deprecated. Please use IFMXDialogServiceAsync.MessageDialogAsync or IFMXDialogServiceSync.MessageDialogSync from the FMX.Platform unit.
  FMX.DialogService, FMX.Types, FMX.Forms, FMX.StdCtrls, FMX.Controls, FMX.Controls.Presentation,
  LightFMX.AppData.Form, LightFmx.Dialogs, ccIniFile;

TYPE
  TForm1 = class(TLightForm)
    btnFmxDialogs: TButton;
    Button2: TButton;
    Button3: TButton;
    btnQuestion: TButton;
    btnFmxDialogs2: TButton;
    procedure btnFmxDialogsClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure btnQuestionClick(Sender: TObject);
    procedure btnFmxDialogs2Click(Sender: TObject);
  private
    procedure DoSomething;
  public
   procedure FormPostInitialize; override;
  end;

VAR
  Form1: TForm1;

IMPLEMENTATION
{$R *.fmx}


procedure TForm1.FormPostInitialize;
begin
  AutoState:= asPosOnly;
  inherited;
end;



procedure TForm1.DoSomething;
begin
  Caption:= 'Something';
end;







// Synchronicity: Uses TDialogServiceAsync on Android, and TDialogServiceSync on the other platforms
// Custom caption: No
// Deprecated: No
// Icons: No
// Ding: No
procedure TForm1.btnFmxDialogsClick(Sender: TObject);
begin
  Caption:= '';
  FMX.Dialogs.ShowMessage('FMX.Dialogs.ShowMessage');
  Caption:= 'Sync';
end;


{
 MessageDlg
 Warning: FMX.Dialogs.MessageDlg is deprecated. (https://docwiki.embarcadero.com/Libraries/Berlin/en/FMX.Dialogs.MessageDlg)
------------------------------------------------------
            Without              With
Platform    ACloseDialogProc 	 ACloseDialogProc
------------------------------------------------------
  Windows   Blocking 	         Blocking
  OS X      Blocking 	         Blocking
  iOS 	    Blocking 	         Non-blocking
  Android   [Not possible]       Non-blocking
------------------------------------------------------  }

procedure TForm1.btnFmxDialogs2Click(Sender: TObject);
begin
  Caption:= '';
  FMX.Dialogs.MessageDlg('FMX.Dialogs.MessageDlg', TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbCancel], 0);
  Caption:= 'sssss';
end;




// Synchronicity: Sync
// Custom caption: No
// Icons: Yes
// Ding: Yes
// You can force TDialogService.PreferredMode := TPreferredMode.Sync on mobile to simulate synchronous behavior, but not recommended
procedure TForm1.Button3Click(Sender: TObject);
begin
  Caption:= '';
  TDialogService.MessageDialog('TDialogService.MessageDialog',
              TMsgDlgType.mtInformation,
              [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbCancel],
              TMsgDlgBtn.mbOK, 0, NIL);


  Caption:= 'Sync';
end;


procedure TForm1.btnQuestionClick(Sender: TObject);
begin
  Caption:= '';
  TDialogService.MessageDialog('TDialogService.MessageDialog question',
              TMsgDlgType.mtConfirmation,
              [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
              TMsgDlgBtn.mbYes, 0,
         procedure(const AResult: TModalResult)
         begin
           if AResult = mrYes
           then DoSomething
           else Caption:= 'Canceled';
         end
  );
  Caption:= 'Sync';
end;


// Synchronicity: Async
// Based on TDialogServiceAsync.MessageDialog
// Custom caption: No
// Icons: No
// Ding: Yes
procedure TForm1.Button2Click(Sender: TObject);
begin
  Caption:= '';
  MessageInfo('MessageInfo', 'No caption?');
  Caption:= 'Async';

  //MessageWarning('Hello info', 'Caption');
  //MessageError('Hello info', 'Caption');
  //MessageErrorSubmit('Hello info', 'Caption');
end;


end.
