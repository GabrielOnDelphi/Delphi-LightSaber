UNIT FormMain;

INTERFACE

USES
  WinApi.Windows, WinApi.Messages, Winapi.ShellApi,
  System.SysUtils, System.Classes, Vcl.StdCtrls, Vcl.ComCtrls, VCL.Forms, Vcl.Controls, Vcl.Samples.Spin, Vcl.ExtCtrls, Vcl.ActnList, Vcl.Dialogs, Vcl.Graphics,
  ccCore, csSystem, cbDialogs, CoolTrayIcon, cvPathEdit, VCL.Menus,
  cvStatusBar, cpProteus, FormLog, cvRadioButton, cvCheckBox, System.Actions, cpProteusIO,
  Vcl.AppEvnts, cbAppData;

  // c:\MyProjects\Packages\Third party packages\MenuPopupHints.pas

TYPE 
  TMainForm = class(TForm)
    actEnterKey: TAction;
    mnuUpdates: TMenuItem;
    mnuEnterKey: TMenuItem;
    Actions: TActionList;
    actSettings : TAction;
    AppEvents   : TApplicationEvents;
    btnStart    : TButton;
    File1       : TMenuItem;
    MainMenu    : TMainMenu;
    mmo         : TMemo;
    Path        : TCubicPathEdit;
    pgCtrl      : TPageControl;
    pnlRight    : TPanel;
    RichEdit1   : TRichEdit;
    mnuSettings: TMenuItem;
    StatBar     : TcubicStatusBar;
    tabLog      : TTabSheet;
    tabMain     : TTabSheet;
    tabMemo     : TTabSheet;
    TrayIcon    : TCoolTrayIcon;
    Proteus: TProteus;
    Info1: TMenuItem;
    actUpdater: TAction;
    mnuAbout: TMenuItem;
    actLanguage: TAction;
    mnuLanguage: TMenuItem;
    procedure actSettingsExecute (Sender: TObject);
    procedure AppEventsMinimize(Sender: TObject);    
    procedure btnSTARTClick      (Sender: TObject);
    procedure CanShowHint(Sender: TObject);    
    procedure FormClose          (Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery     (Sender: TObject; var CanClose: Boolean);
    procedure FormCreate         (Sender: TObject);
    procedure FormDestroy        (Sender: TObject);
    procedure TrayIconClick      (Sender: TObject);
    procedure actEnterKeyExecute(Sender: TObject);
    procedure actUpdaterExecute(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure actLanguageExecute(Sender: TObject);
  protected
    procedure WMDROPFILES (VAR Msg: TWMDropFiles); message WM_DROPFILES;   { Accept the dropped files from Windows Explorer }
  private
    Saved: Boolean;
    procedure WMEndSession(VAR Msg: TWMEndSession); message WM_ENDSESSION;
    procedure LateInitialize(VAR Msg: TMessage); message MSG_LateAppInit;
  public
    procedure SaveBeforeExit;
    procedure FontSizeChanged;
 end;

VAR
   MainForm: TMainForm;

IMPLEMENTATION {$R *.dfm}

USES cTranslate, cmGuiSettings, FormAbout, cbWinVersion, cvIniFile, ccIO, cmIO, cmIO.Win,
     ciUpdater, FormSelectLang, FormUpdaterNotifier, FormSettings, cmDebugger, uInitialization;





{--------------------------------------------------------------------------------------------------
   APP START/CLOSE
--------------------------------------------------------------------------------------------------}
procedure TMainForm.FormCreate(Sender: TObject);
begin
 Saved:= FALSE;
end;


procedure TMainForm.LateInitialize;
begin
 uInitialization.LateInitialization;
 btnStartClick(self);
end;




{--------------------------------------------------------------------------------------------------
 CLOSE
--------------------------------------------------------------------------------------------------}
procedure TMainForm.WMEndSession(var Msg: TWMEndSession);
begin
 SaveBeforeExit;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
 SaveBeforeExit;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caFree;
 SaveBeforeExit;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 CanClose := TRUE;
 SaveBeforeExit;
end;


procedure TMainForm.SaveBeforeExit;                   { I need to put SaveBeforeExit in only two places: OnCloseQueryand OnDestroy. Details: https://groups.google.com/forum/#!msg/borland.public.delphi.objectpascal/82AG0_kHonU/ft53lAjxWRMJ }
begin
 if NOT Saved then
  begin
    Saved:= TRUE;
    GuiSettings.Save;
    FreeAndNil(GuiSettings);
    SaveForm(Self, TRUE);
    FreeAndNil(Updater);
    FreeAndNil(Translator);
  end;
end;


procedure TMainForm.CanShowHint(Sender: TObject);
begin
  Application.ShowHint:= GuiSettings.HintType <> htOff;
  if GuiSettings.HintType = htStatBar
  then StatBar.SimpleText := GetLongHint(Application.Hint);
end;


{ Calculates the size of GUI after the user applies new font. Needs to be called manually. }
{ToDo: intercept this msg }
procedure TMainForm.FontSizeChanged;
begin
  StatBar.Height:= Canvas.TextHeight('pT|')+ 8;
end;
 

procedure TMainForm.AppEventsMinimize(Sender: TObject);
begin
 if GuiSettings.Minimize2Tray
 then TrayIcon.PutIconOnlyInTray;
end;



{--------------------------------------------------------------------------------------------------
   MAIN
--------------------------------------------------------------------------------------------------}

procedure TMainForm.btnSTARTClick(Sender: TObject);
begin
 CursorBusy;
 TRY
   Caption:= 'Starting...';
 FINALLY
  CursorNotBusy;
 END;
end;






{-------------------------------------------------------------------------------------------------------------
   STUFF
-------------------------------------------------------------------------------------------------------------}

procedure TMainForm.WMDROPFILES(var Msg: TWMDropFiles);  { Accept the dropped files from Windows Explorer }
var
  i, amount: Integer;
  FileName: array[0..MAX_PATH] of Char;
begin
  inherited;
  TRY
    Amount := DragQueryFile(Msg.Drop, $FFFFFFFF, FileName, MAX_PATH);
    for i := 0 to (Amount - 1) do
     begin
      DragQueryFile(Msg.Drop, i, FileName, MAX_PATH);
      Caption:= FileName;
     end;
  FINALLY
    DragFinish(Msg.Drop);
  END;
end;





procedure TMainForm.mnuAboutClick(Sender: TObject);
begin
  TfrmAboutApp.ShowFormModal;
end;

procedure TMainForm.actEnterKeyExecute(Sender: TObject);
begin
  Proteus.ShowEnterKeyBox;
end;


procedure TMainForm.actLanguageExecute(Sender: TObject);
begin
  FormSelectLang.ShowSelectLanguage;
end;

procedure TMainForm.actSettingsExecute(Sender: TObject);
begin
  TfrmSettings.CreateModal;
  TrayIcon.MinimizeToTray:= GuiSettings.Minimize2Tray;
end;


procedure TMainForm.actUpdaterExecute(Sender: TObject);
begin
  TFrmUpdater.ShowUpdater(TRUE);
end;


procedure TMainForm.TrayIconClick(Sender: TObject);
begin
  TrayIcon.PutIconOnlyInTask;
end;




 



end.{


procedure TMainForm.btnProgressClick(Sender: TObject);
VAR i: Integer;
begin
 Taskbar.ProgressState:= TTaskBarProgressState(2);
 for i:= 1 to 100 DO
  begin
   Taskbar.ProgressValue:=  Taskbar.ProgressValue+ 1;
   DelayEx(40);
  end;
 Taskbar.ProgressState:= TTaskBarProgressState(0);
end;



 fastmm4.RegisterExpectedMemoryLeak(Taskbar);



  { Check Internet connection
  StatBar.SimpleText:= 'Checking Internet connection...';
  InternetChecked:= FALSE;
  if NOT InternetChecked then                                                                      { Check only once
   begin
     InternetChecked:= true;
     InetStatus:= ProgramConnect2Internet;
     if ProgramConnect2Internet < 1 then
       StatBar.SimpleText:= ProgramConnect2InternetMsg(InetStatus);
   end;



procedure TMainForm.spnOpacityChange(Sender: TObject);
begin
 AlphaBlend:= spnOpacity.Value< 255;
 AlphaBlendValue:= spnOpacity.Value;
end;}
