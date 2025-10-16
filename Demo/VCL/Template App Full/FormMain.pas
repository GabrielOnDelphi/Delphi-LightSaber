UNIT FormMain;

{=============================================================================================================
   2025.05
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Use this as a template when you start a new commercial application
--------------------------------------------------------------------------------------------------------------
   Application that implements the following features:
     About box
     EULA box
     News and Updates box
     Settings box
      * User-selectable skins
      * User-selectable font
      * User-defined data folder (where the app saves its files)
      * User-defined Window opacity (alpha transparency)
     GUI self-translation services
     GUI saves/loads its state from disk
     GUI minimizable to taskbar or to system tray
     Splash screen
     Trial protection (via Proteus)
     Event log
     Only one instance - prevents the user to start more than one instance of this app
     Accepts Drag and Drop.
     Create startmenu/desktop shortcut (on first run)
     Associates itself with its own file extension (app starts when the user double clicks a '.LightSaber' file in Explorer
=============================================================================================================}

INTERFACE

USES
  WinApi.Windows, WinApi.Messages, Winapi.ShellApi,
  System.SysUtils, System.Classes, System.Actions,
  VCL.Menus, Vcl.AppEvnts, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Forms, Vcl.Controls, Vcl.ExtCtrls, Vcl.ActnList, Vcl.Graphics,
  CoolTrayIcon, LightCore.AppData, LightVcl.Common.AppData, LightVcl.Common.SystemTime, LightVcl.Common.Clipboard, LightVcl.Visual.PathEdit, LightVcl.Visual.StatusBar,
  cpProteus {Delete this line if you don't have Proteus library}, cpProteusIO,
  LightVcl.Common.GuiSettings, LightVcl.Common.AppDataForm, LightCore, LightCore.Time;

TYPE
  TMainForm = class(TLightForm)
    actAbout    : TAction;
    actEnterKey : TAction;
    Actions     : TActionList;
    actLanguage : TAction;
    actSettings : TAction;
    actShowLog  : TAction;
    actUpdater  : TAction;
    AppEvents   : TApplicationEvents;
    btnProgress : TButton;
    btnShowLog  : TButton;
    Button1     : TButton;
    Button2     : TButton;
    Button3     : TButton;
    Button4     : TButton;
    Button5     : TButton;
    Button6     : TButton;
    Button7     : TButton;
    Button8     : TButton;
    Button9     : TButton;
    Button10    : TButton;
    Button11    : TButton;
    Button12    : TButton;
    MainMenu    : TMainMenu;
    mmo         : TMemo;
    mnuAbout    : TMenuItem;
    mnuEnterKey : TMenuItem;
    mnuFile     : TMenuItem;
    mnuInfo     : TMenuItem;
    mnuLanguage : TMenuItem;
    mnuSettings : TMenuItem;
    mnuUpdates  : TMenuItem;
    N1          : TMenuItem;
    pgCtrl      : TPageControl;
    pnlRight    : TPanel;
    tabLog      : TTabSheet;
    tabMain     : TTabSheet;
    tabProgress : TTabSheet;
    TrayIcon    : TCoolTrayIcon;
    btnStart    : TButton;
    StatBar     : TStatusBar;
    Proteus: TProteus;
    procedure actAboutExecute    (Sender: TObject);
    procedure actEnterKeyExecute (Sender: TObject);
    procedure actLanguageExecute (Sender: TObject);
    procedure actSettingsExecute (Sender: TObject);
    procedure actUpdaterExecute  (Sender: TObject);
    procedure AppEventsMinimize  (Sender: TObject);
    procedure btnProgressClick   (Sender: TObject);
    procedure btnSTARTClick      (Sender: TObject);
    procedure Button5Click       (Sender: TObject);
    procedure Button6Click       (Sender: TObject);
    procedure Button7Click       (Sender: TObject);
    procedure Button8Click       (Sender: TObject);
    procedure Button9Click       (Sender: TObject);
    procedure Button10Click      (Sender: TObject);
    procedure Button11Click      (Sender: TObject);
    procedure Button12Click      (Sender: TObject);
    procedure CanShowHint        (Sender: TObject);
    procedure FormClose          (Sender: TObject; var Action: TCloseAction);
    procedure TrayIconClick      (Sender: TObject);
    procedure actShowLogExecute  (Sender: TObject);
    procedure FormCreate         (Sender: TObject);
  protected
    procedure WMDROPFILES (VAR Msg: TWMDropFiles); message WM_DROPFILES;   { Accept the dropped files from Windows Explorer }
  private
  public
    procedure FormPostInitialize; override;  { Called after the main form was fully created }
    procedure FormPreRelease; override;
    procedure FontSizeChanged;
 end;

VAR
   MainForm: TMainForm;
   GuiSettings: TGuiSettings;

IMPLEMENTATION {$R *.dfm}

USES
   ciUpdater,
   LightVcl.Common.Translate,
   LightVcl.Visual.INIFile,
   LightVcl.Common.System,
   FormAbout,
   FormSettings,
   FormTranslSelector,
   FormUpdaterNotifier,
   uInitialization;



{--------------------------------------------------------------------------------------------------
   APP START/CLOSE
--------------------------------------------------------------------------------------------------}
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Too early to do initialization here!
  // Initialization code is better done in LateInitialize which takes place AFTER the form was properly constructed!
end;


procedure TMainForm.FormPostInitialize;
begin
  inherited FormPostInitialize;

  uInitialization.LateInitialization;
  //btnStartClick(self);
  //actShowLogExecute(Self);    //temp
  Show;
end;



{--------------------------------------------------------------------------------------------------
 CLOSE
--------------------------------------------------------------------------------------------------}

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caFree;
end;


{ It is enough to put SaveBeforeExit in thse two places only: OnCloseQueryand & OnDestroy.
  Details: https://groups.google.com/forum/#!msg/borland.public.delphi.objectpascal/82AG0_kHonU/ft53lAjxWRMJ }
procedure TMainForm.FormPreRelease;
begin
  inherited FormPreRelease;
  if NOT Saved then
   begin
     GuiSettings.Save;
     FreeAndNil(GuiSettings);
     FreeAndNil(Updater);
   end;
end;




{--------------------------------------------------------------------------------------------------
   MAIN
--------------------------------------------------------------------------------------------------}
procedure TMainForm.btnSTARTClick(Sender: TObject);
begin
  LightVcl.Common.System.CursorBusy;
  TRY
    Caption:= 'Started...';
    //actShowLogExecute(Sender);
    Application.DefaultFont.Size:= 20;
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


{ Calculates the size of GUI after the user applies new font. Needs to be called manually. }
{ToDo: intercept the FontSizeChange msg }
procedure TMainForm.FontSizeChanged;
begin
  StatBar.Height:= Canvas.TextHeight('pT|')+ 8;
end;





{-------------------------------------------------------------------------------------------------------------
   MENUS
-------------------------------------------------------------------------------------------------------------}
procedure TMainForm.actAboutExecute(Sender: TObject);
begin
  TfrmAboutApp.CreateFormModal(TRUE, TRUE);
end;


procedure TMainForm.actEnterKeyExecute(Sender: TObject);
begin
  Proteus.ShowEnterKeyBox;   // Delete this line if you don't have Proteus library
end;


procedure TMainForm.actLanguageExecute(Sender: TObject);
begin
  TfrmTranslSelector.ShowSelector;
end;


procedure TMainForm.actSettingsExecute(Sender: TObject);
begin
  TfrmSettings.CreateFormModal(GuiSettings);
  TrayIcon.MinimizeToTray:= AppData.Minimize2Tray;
end;


procedure TMainForm.actUpdaterExecute(Sender: TObject);
begin
  TFrmUpdater.CreateForm(TRUE);
end;


procedure TMainForm.TrayIconClick(Sender: TObject);
begin
  TrayIcon.PutIconOnlyInTask;
end;


procedure TMainForm.AppEventsMinimize(Sender: TObject);
begin
 if AppData.Minimize2Tray
 then TrayIcon.PutIconOnlyInTray;
end;

 
procedure TMainForm.CanShowHint(Sender: TObject);
begin
  Application.ShowHint:= AppData.HintType <> htOff;
  if AppData.HintType = htStatBar
  then StatBar.SimpleText := GetLongHint(Application.Hint);
end; 



{-------------------------------------------------------------------------------------------------------------
   LOG
-------------------------------------------------------------------------------------------------------------}
procedure TMainForm.actShowLogExecute(Sender: TObject);
begin
  AppData.PopUpLogWindow;
end;



procedure TMainForm.Button5Click(Sender: TObject);
begin
  AppData.LogHint ('LogHint');
end;

procedure TMainForm.Button6Click(Sender: TObject);
begin
  AppData.LogInfo ('LogInfo');
end;

procedure TMainForm.Button7Click(Sender: TObject);
begin
  AppData.LogVerb ('LogVerb');
end;

procedure TMainForm.Button8Click(Sender: TObject);
begin
  AppData.LogImpo ('LogImpo');
end;

procedure TMainForm.Button9Click(Sender: TObject);
begin
  AppData.LogWarn ('LogWarn');
end;

procedure TMainForm.Button11Click(Sender: TObject);
begin
  AppData.LogError('LogError');
end;

procedure TMainForm.Button12Click(Sender: TObject);
begin
  AppData.LogBold ('LogBold');   ///nu merge
end;

procedure TMainForm.Button10Click(Sender: TObject);
begin
  AppData.LogEmptyRow;
end;






procedure TMainForm.btnProgressClick(Sender: TObject);
begin
 {
 USES Vcl.TaskBar
 Taskbar.ProgressState:= TTaskBarProgressState(2);
 for i:= 1 to 100 DO
  begin
   Taskbar.ProgressValue:=  Taskbar.ProgressValue+ 1;
   DelayEx(40);
  end;
 Taskbar.ProgressState:= TTaskBarProgressState(0);

 fastmm4.RegisterExpectedMemoryLeak(Taskbar); }
end;





end.
