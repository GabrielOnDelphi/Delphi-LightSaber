UNIT MainForm;

{-------------------------------------------------------------------------------------------------------------
  This application will demonstrate:

    1. How to use AppData

    2. How to save the state of all GUI controls on application shutdown and then restore them loaded on application startup:
         * checkboxes
         * radiobuttons
         * cubic custom controls
         * form's position
         * controls on the second form

    3. How to send messages to the AppLog


  This demo app requires the Cubic LightSaber library.
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.StdCtrls, Vcl.Forms, Vcl.Controls, Vcl.Samples.Spin,
  LightVcl.Visual.RichEdit, Vcl.ComCtrls, LightCore.Core, LightVcl.Common.SystemTime, LightVcl.Common.Clipboard, LightVcl.Common.Dialogs, LightCore.INIFile, LightVcl.Common.AppDataForm,  Vcl.Dialogs, LightVcl.Visual.CheckBox, Vcl.ExtCtrls, LightVcl.Visual.RichLogTrack,
  System.Actions, Vcl.ActnList, Vcl.Menus, LightVcl.Visual.RadioButton, LightCore.AppData, LightVcl.Common.AppData
;

TYPE
 TfrmTester = class(TLightForm)
    actAutoSave: TAction;
    ActionList : TActionList;
    actLoadGUI : TAction;
    actSaveGUI : TAction;
    Autosave1  : TMenuItem;
    btnLoad    : TButton;
    btnSave    : TButton;
    btnShowLog : TButton;
    Button2    : TButton;
    Button3    : TButton;
    Button4    : TButton;
    Button5    : TButton;
    CheckBox1  : TCheckBox;
    File1      : TMenuItem;
    FontDialog : TFontDialog;
    GroupBox2  : TGroupBox;
    GroupBox3  : TGroupBox;
    MainMenu   : TMainMenu;
    Panel1     : TPanel;
    procedure actAutoSaveExecute (Sender: TObject);
    procedure actLoadGUIExecute  (Sender: TObject);
    procedure actSaveGUIExecute  (Sender: TObject);
    procedure btnShowLogClick    (Sender: TObject);
    procedure Button2Click       (Sender: TObject);
    procedure Button3Click       (Sender: TObject);
    procedure Button4Click       (Sender: TObject);
    procedure Button5Click       (Sender: TObject);
    procedure FormClick          (Sender: TObject);
    procedure FormCreate         (Sender: TObject);
    procedure FormDestroy        (Sender: TObject);
  private
  public
    procedure FormPostInitialize; override; // Called after the main form was fully created
 end;

VAR
   frmTester: TfrmTester;

IMPLEMENTATION  {$R *.dfm}

USES
   LightVcl.Visual.INIFile, LightCore.IO, LightCore.TextFile, LightVcl.Common.IO, LightVcl.Common.Debugger, SecondForm;



{--------------------------------------------------------------------------------------------------
   APP START/CLOSE
--------------------------------------------------------------------------------------------------}
procedure TfrmTester.FormCreate(Sender: TObject);
begin
  //
end;


procedure TfrmTester.FormPostInitialize;
begin
 inherited FormPostInitialize;

 if AppData.RunningFirstTime
 then MessageInfo(
           'This application will demonstrate:'+ CRLF+
           CRLF+
           '1. How to save the state of all GUI controls on application shutdown and then restore them loaded on application startup:'+ CRLF+
                '* checkboxes'+ CRLF+
                '* radiobuttons'+ CRLF+
                '* cubic custom controls'+ CRLF+
                '* form''s position'+ CRLF+
                '* controls on the second form'+ CRLF+
                CRLF+
           '2. How to send messages to the AppLog');

 AppData.LogImpo('Application started ok.');
end;


//todo 1: the position log's trackbar is not saved!
procedure TfrmTester.FormDestroy(Sender: TObject);
begin
 AppData.LogWarn('Application shutting down...');
end;




procedure TfrmTester.actLoadGUIExecute(Sender: TObject);
begin
  LoadForm;
end;

procedure TfrmTester.actSaveGUIExecute(Sender: TObject);
begin
  SaveForm;
end;

procedure TfrmTester.btnShowLogClick(Sender: TObject);
begin
  AppData.PopUpLogWindow;
end;








procedure TfrmTester.actAutoSaveExecute(Sender: TObject);
begin
 //The status of this action (autocheck) will be stored to the INI file also BUT not when it is unchecked (obviously) because then we don't save the GUI to INI file
 if actAutoSave.Checked
 then AutoState:= asFull
 else AutoState:= asNone;
end;


procedure TfrmTester.Button2Click(Sender: TObject);
begin
  if FontDialog.Execute
  then AppData.Font:= FontDialog.Font;
end;


procedure TfrmTester.Button3Click(Sender: TObject);
begin
  AppData.LogVerb('Some less important information');
  MessageInfo('You need to set log''s verbosity to "verbose" in order to see this message.');
end;


procedure TfrmTester.Button4Click(Sender: TObject);
begin
  AppData.LogError('Error encountered!');
end;



procedure TfrmTester.Button5Click(Sender: TObject);
VAR
  frmContainer: TfrmContainer;
begin
  AppData.CreateForm(TfrmContainer, frmContainer, FALSE, asFull);
  frmContainer.grpContainer.Parent:= Self;
  frmContainer.grpContainer.Align := alTop;
end;


procedure TfrmTester.FormClick(Sender: TObject);
begin
 //frmContainer.Show;
end;


end.
