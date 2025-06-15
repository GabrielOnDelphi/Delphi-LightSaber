unit MainForm;

interface

uses
  Winapi.Windows,
  System.UITypes, System.Classes, System.SysUtils,
  FMX.Dialogs, // Warning: MessageDlg is deprecated. Please use IFMXDialogServiceAsync.MessageDialogAsync or IFMXDialogServiceSync.MessageDialogSync from the FMX.Platform unit.
  FMX.DialogService, FMX.Types, FMX.Forms, FMX.StdCtrls, FMX.Controls, FMX.Controls.Presentation,
  LightFMX.lbAppData.Form, LightFMX.lbDialogs, LightCore.INIFile;

TYPE
  TForm1 = class(TLightForm)
    btnFmxDialogs: TButton;
    btnMessageInfo: TButton;
    Button3: TButton;
    btnQuestion: TButton;
    btnFmxDialogs2: TButton;
    btnInputQuery1: TButton;
    btnInputQuery3: TButton;
    btnInputQuery2: TButton;
    procedure btnFmxDialogsClick(Sender: TObject);
    procedure btnMessageInfoClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure btnQuestionClick(Sender: TObject);
    procedure btnFmxDialogs2Click(Sender: TObject);
    procedure btnInputQuery1Click(Sender: TObject);
    procedure btnInputQuery2Click(Sender: TObject);
    procedure btnInputQuery3Click(Sender: TObject);
  private
    procedure DoSomething;
  public
   procedure FormPostInitialize; override;
  end;

VAR
  Form1: TForm1;

IMPLEMENTATION
{$R *.fmx}

USES LightCore.AppData;


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
// Has X button: Yes
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
  // Compiler warning here: W1000 Symbol 'MessageDlg' is deprecated: 'Use FMX.DialogService methods'
  FMX.Dialogs.MessageDlg('FMX.Dialogs.MessageDlg', TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbCancel], 0);
  Caption:= 'sssss';
end;




// Synchronicity: Sync
// Custom caption: No
// Icons: Yes
// Ding: Yes
// Has X button: Yes

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



// Has X button: Yes BUT it is disabled (so it does nothing)
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
// Has X button: No
procedure TForm1.btnMessageInfoClick(Sender: TObject);
begin
  Caption:= '';
  MessageInfo('MessageInfo', 'No caption?');
  Caption:= 'Async';

  //MessageWarning('Hello info', 'Caption');
  //MessageError('Hello info', 'Caption');
  //MessageErrorSubmit('Hello info', 'Caption');
end;



// TDialogService.InputQuery() is a method in the FireMonkey (FMX) framework that provides a cross-platform way to prompt the user for input.
// InputQuery can work synchronously or asynchronously depending on the preferred mode.
// InputQuery internally calls TDialogServiceAsync.InputQuery or TDialogServiceSync.InputQuery.
// Has X button: Yes BUT it does nothing!
procedure TForm1.btnInputQuery1Click(Sender: TObject);
begin
  FMX.DialogService.TDialogService.PreferredMode:= TDialogService.TPreferredMode.Async;

  TDialogService.InputQuery(
      'Enter Your Name',          // Dialog title
      ['Name:'],                  // Array of prompts for input fields
      [''],                       // Array of default values
      procedure(const AResult: TModalResult; const AValues: array of string)
      begin
        if AResult = mrOk
        then Caption:= 'Hello, ' + AValues[0]
        else Caption:= 'User cancelled';
      end
    );
end;


// InputQuery supports multiple input fields. For instance, to collect a first and last name:
procedure TForm1.btnInputQuery2Click(Sender: TObject);
begin
  FMX.DialogService.TDialogService.PreferredMode:= TDialogService.TPreferredMode.Async;

  TDialogService.InputQuery(
    'Enter Your Details',
    ['First Name:', 'Last Name:'],
    ['', ''],
    procedure(const AResult: TModalResult; const AValues: array of string)
    begin
      if AResult = mrOk
      then Caption := 'Hello, ' + AValues[0] + ' ' + AValues[1];
    end
  );
end;


// Wrong usage example.
// Because of the asynchronicity, the Caption will be initially set to 'John Doe'. Only after the user enters its name, the Caption will show correct data!
procedure TForm1.btnInputQuery3Click(Sender: TObject);
VAR UserName: string; // Scope Capture: The anonymous callback can capture variables from the surrounding scope, which is useful for maintaining context:
begin
  FMX.DialogService.TDialogService.PreferredMode:= TDialogService.TPreferredMode.Async;

  UserName:= 'John Doe';
  TDialogService.InputQuery(
    'Enter Your Details',
    ['First Name:', 'Last Name:'],
    ['', ''],
    procedure(const AResult: TModalResult; const AValues: array of string)
    begin
      if AResult = mrOk
      then Caption := 'Hello, ' + AValues[0] + ' ' + AValues[1];
      UserName:= AValues[0];
    end
  );

  Caption:= UserName; // This is executed BEFORE the user has a chance to enter its name!
end;


end.
