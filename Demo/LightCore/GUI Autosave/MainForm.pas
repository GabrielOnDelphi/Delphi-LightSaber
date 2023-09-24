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
  WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.StdCtrls, VCL.Forms, Vcl.Controls, Vcl.Samples.Spin,
  cvRichEdit, Vcl.ComCtrls, ccCore, ccINIFile, FormLog, Vcl.Dialogs, cvCheckBox, Vcl.ExtCtrls, clRichLogTrack,
  System.Actions, Vcl.ActnList, Vcl.Menus, cvRadioButton;

TYPE
 TfrmTester = class(TForm)
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
    procedure LateInitialize(VAR message: TMessage);  message MSG_LateInitialize;
  public
 end;

VAR
   frmTester: TfrmTester;

IMPLEMENTATION  {$R *.dfm}

USES
   ccAppData, cvIniFile, ccIO, cmDebugger, SecondForm;



{--------------------------------------------------------------------------------------------------
   APP START/CLOSE
--------------------------------------------------------------------------------------------------}
procedure TfrmTester.FormCreate(Sender: TObject);
begin
 PostMessage(Self.Handle, MSG_LateInitialize, 0, 0);   { This will call LateInitialize }
end;


procedure TfrmTester.LateInitialize;
begin
 LoadForm(frmTester);

 if AppData.RunningFirstTime
 then MesajInfo(
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

 {if radShow.Checked
 then Button5Click(Self);}

 AppData.Initializing:= FALSE;
 LogAddImpo('Application started ok.');
end;

//todo 1: the log's trackbar is not saved!

procedure TfrmTester.FormDestroy(Sender: TObject);
begin
 LogAddWarn('Application shutting down...');

 if actAutoSave.Checked
 then SaveForm(Self);

 FreeAndNil(AppData);
end;




procedure TfrmTester.actLoadGUIExecute(Sender: TObject);
begin
 LoadForm(Self);
end;

procedure TfrmTester.actSaveGUIExecute(Sender: TObject);
begin
 SaveForm(Self);
end;

procedure TfrmTester.btnShowLogClick(Sender: TObject);
begin
 ShowLog;
end;








procedure TfrmTester.actAutoSaveExecute(Sender: TObject);
begin
 //The status of this action (autocheck) will be stored to the ini file also
end;


procedure TfrmTester.Button2Click(Sender: TObject);
begin
  if FontDialog.Execute
  then AppData.Font:= FontDialog.Font;
end;


procedure TfrmTester.Button3Click(Sender: TObject);
begin
  LogAddVerb('Some less important information');
  MesajInfo('You need to set log''s verbosity to "verbose" in order to see this message.');
end;


procedure TfrmTester.Button4Click(Sender: TObject);
begin
  LogAddError('Error encountered!');
end;



procedure TfrmTester.Button5Click(Sender: TObject);
VAR
  frmContainer: TfrmContainer;
begin
  AppData.CreateForm(TfrmContainer, frmContainer, FALSE);
  frmContainer.grpContainer.Parent:= Self;
  frmContainer.grpContainer.Align := alTop;
end;


procedure TfrmTester.FormClick(Sender: TObject);
begin
 //frmContainer.Show;
end;


end.

















