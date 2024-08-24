UNIT FormMain;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Application that features:
     About box,
     News and Updates box
     Settings box
     User-selectable skins
     GUI self-translation services
     GUI saves/loads its state from disk
     Trial protection (via Proteus)
     Event log
     Only one instance
     Drag and drop.

   Can be used as template for future applications.
--------------------------------------------------------------------------------------------------------------

=============================================================================================================}

INTERFACE

USES 
  WinApi.Windows, WinApi.Messages, Winapi.ShellApi,
  System.SysUtils, System.Classes, System.Actions,
  VCL.Menus, Vcl.AppEvnts, Vcl.StdCtrls, Vcl.ComCtrls, VCL.Forms, Vcl.Controls, Vcl.ExtCtrls, Vcl.ActnList, Vcl.Graphics,
  csSystem, CoolTrayIcon, cvPathEdit, cvStatusBar, cpProteus, cbAppData, cpProteusIO;

  // c:\MyProjects\Packages\Third party packages\MenuPopupHints.pas

TYPE 
  TMainForm = class(TForm)
    actEnterKey : TAction;
    Actions     : TActionList;
    actLanguage : TAction;
    actSettings : TAction;
    actUpdater  : TAction;
    AppEvents   : TApplicationEvents;
    btnProgress : TButton;
    btnStart    : TButton;
    mnuFile     : TMenuItem;
    mnuInfo     : TMenuItem;
    MainMenu    : TMainMenu;
    mmo         : TMemo;
    mnuAbout    : TMenuItem;
    mnuEnterKey : TMenuItem;
    mnuLanguage : TMenuItem;
    mnuSettings : TMenuItem;
    mnuUpdates  : TMenuItem;
    Path        : TCubicPathEdit;
    pgCtrl      : TPageControl;
    pnlRight    : TPanel;
    Proteus     : TProteus;
    RichEdit1   : TRichEdit;
    StatBar     : TcubicStatusBar;
    tabLog      : TTabSheet;
    tabMain     : TTabSheet;
    tabMemo     : TTabSheet;
    tabProgress : TTabSheet;
    TrayIcon    : TCoolTrayIcon;
    procedure actSettingsExecute (Sender: TObject);
    procedure AppEventsMinimize  (Sender: TObject);
    procedure btnSTARTClick      (Sender: TObject);
    procedure CanShowHint        (Sender: TObject);
    procedure FormClose          (Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery     (Sender: TObject; var CanClose: Boolean);
    procedure FormCreate         (Sender: TObject);
    procedure FormDestroy        (Sender: TObject);
    procedure TrayIconClick      (Sender: TObject);
    procedure actEnterKeyExecute (Sender: TObject);
    procedure actUpdaterExecute  (Sender: TObject);
    procedure mnuAboutClick      (Sender: TObject);
    procedure actLanguageExecute (Sender: TObject);
    procedure btnProgressClick   (Sender: TObject);
  protected
    procedure WMDROPFILES (VAR Msg: TWMDropFiles); message WM_DROPFILES;   { Accept the dropped files from Windows Explorer }
  private
    Saved: Boolean;
    procedure WMEndSession(VAR Msg: TWMEndSession); message WM_ENDSESSION;
    procedure LateInitialize(VAR Msg: TMessage); message MSG_LateFormInit;  { Called after the main form was fully created }
  public
    procedure SaveBeforeExit;
    procedure FontSizeChanged;
 end;

VAR
   MainForm: TMainForm;

IMPLEMENTATION {$R *.dfm}

USES
   ciUpdater,
   cmGuiSettings,
   cTranslate,
   cvIniFile,
   FormAbout,
   FormSelectLang,   
   FormSettings,
   FormUpdaterNotifier,
   uInitialization;



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
    SaveForm(Self);
    FreeAndNil(Updater);
    FreeAndNil(Translator);
  end;
end;




{--------------------------------------------------------------------------------------------------
   MAIN
--------------------------------------------------------------------------------------------------}

procedure TMainForm.btnSTARTClick(Sender: TObject);
begin
 CursorBusy;
 TRY
   Caption:= 'Started...';
 FINALLY
  CursorNotBusy;
 END;
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
{ToDo: intercept this msg }
procedure TMainForm.FontSizeChanged;
begin
  StatBar.Height:= Canvas.TextHeight('pT|')+ 8;
end;





{-------------------------------------------------------------------------------------------------------------
   MENUS
-------------------------------------------------------------------------------------------------------------}
procedure TMainForm.mnuAboutClick(Sender: TObject);
begin
  TfrmAboutApp.CreateFormModal;
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
  TrayIcon.MinimizeToTray:= AppData.Minimize2Tray;
end;


procedure TMainForm.actUpdaterExecute(Sender: TObject);
begin
  TFrmUpdater.ShowUpdater(TRUE);
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



end.
