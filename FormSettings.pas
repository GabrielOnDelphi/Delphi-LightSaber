UNIT FormSettings;

INTERFACE
{$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

{$mesage This is the main template file. Do not link to it. Copy it in your project! }

USES
  WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.StdCtrls, Vcl.ComCtrls, VCL.Forms, Vcl.AppEvnts, Vcl.Controls, Vcl.Samples.Spin, Vcl.Dialogs, Vcl.ExtCtrls,
  cvIniFile, ccCore, CoolTrayIcon, cvPathEdit, cpProteus, cmDebugger, cvRadioButton, cvCheckBox, cpProteusIO, cvTrayIcon, FoldPanel;

TYPE
  TfrmSettings = class(TForm)
    AppEvents          : TApplicationEvents;
    FontDialog         : TFontDialog;
    Proteus            : TProteus;
    pgCtrl             : TPageControl;
    tabSystem          : TTabSheet;
    TabSheet2          : TTabSheet;
    grpSystem          : TGroupBox;
    btnDesktopShortcut : TButton;
    chkAutoStartUp     : TCubicCheckBox;
    GroupHelp          : TGroupBox;
    lblHintHide        : TLabel;
    radHintsOff        : TCubicRadioButton;
    radHintsTooltips   : TCubicRadioButton;
    radHintsStatBar    : TCubicRadioButton;
    spnHideHint        : TSpinEdit;
    TabSheet3          : TTabSheet;
    GroupBox1          : TGroupBox;
    btnSkins           : TButton;
    btnFont            : TButton;
    Path               : TCubicPathEdit;
    btnCrash           : TButton;
    chkTrayIcon        : TCubicCheckBox;
    chkStartMinim      : TCubicCheckBox;
    TrayIcon: TCoolTrayIcon;
    procedure AppEventsMinimize       (Sender: TObject);
    procedure btnCrashClick           (Sender: TObject);
    procedure btnDesktopShortcutClick (Sender: TObject);
    procedure btnEnterKeyClick        (Sender: TObject);
    procedure btnFontClick            (Sender: TObject);
    procedure btnSkinsClick           (Sender: TObject);
    procedure CanShowHint             (Sender: TObject);
    procedure chkAutoStartUpClick     (Sender: TObject);
    procedure chkTrayIconClick        (Sender: TObject);
    procedure FontDialogApply         (Sender: TObject; Wnd: HWND);
    procedure FormClose               (Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery          (Sender: TObject; var CanClose: Boolean);
    procedure FormCreate              (Sender: TObject);
    procedure FormDestroy             (Sender: TObject);
    procedure spnHideHintChange       (Sender: TObject);
    procedure TrayIconClick           (Sender: TObject);
  protected
  private
    Saved: Boolean;
    procedure WMEndSession  (VAR Msg: TWMEndSession); message WM_ENDSESSION;
    procedure LateInitialize(VAR Msg: TMessage); message MSG_LateAppInit; // Called after the main form was fully created
  public
    procedure SaveBeforeExit;
 end;

procedure SetCaption(const CaptionText: string);


VAR
   frmSettings: TfrmSettings;

IMPLEMENTATION {$R *.dfm}

USES
   FormSkinsDisk, ccAppData, csShell, FormMain;


{--------------------------------------------------------------------------------------------------
   APP START/CLOSE
--------------------------------------------------------------------------------------------------}
procedure TfrmSettings.FormCreate(Sender: TObject);
begin
 PostMessage(Self.Handle, MSG_LateAppInit_, 0, 0); Not needed anymore. Moved to ccAppData { This will call LateInitialize }
 Saved:= FALSE;
end;


procedure TfrmSettings.LateInitialize;
begin
 { Hints }
 Application.HintColor     := $c0c090;
 Application.HintPause     := 350;                                                                 { Specifies the time interval that passes before the control's Help Hint appears when the user places the mouse pointer on a control or menu item. Windows' default is 500 ms }
 Application.HintShortPause:= 40;                                                                  { Specifies the time period to wait before bringing up a hint if another hint has already been shown. Windows' default is 50 ms }
 Application.UpdateFormatSettings:= FALSE;                                                         { more http://www.delphi3000.com/articles/article_4462.asp?SK= }
 Application.OnHint:= frmSettings.CanShowHint;
 //lblHintHide.Hint:= 'The hints will automatically disappear after this interval.' +CRLF+'If you don''t have time to read the hints, please increase this value with few seconds.';

 btnCrash.Visible:= AppData.BetaTesterMode;
end;


procedure TfrmSettings.SaveBeforeExit;                   { I need to put SaveBeforeExit in only two places: OnCloseQueryand OnDestroy. Details: https://groups.google.com/forum/#!msg/borland.public.delphi.objectpascal/82AG0_kHonU/ft53lAjxWRMJ }
begin
 if NOT Saved then
  begin
   Saved:= TRUE;
   SaveForm(Self);
  end;
end;





{--------------------------------------------------------------------------------------------------
 CLOSE
--------------------------------------------------------------------------------------------------}
procedure TfrmSettings.WMEndSession(var Msg: TWMEndSession);
begin
 SaveBeforeExit;
end;

procedure TfrmSettings.FormDestroy(Sender: TObject);
begin
 SaveBeforeExit;
end;

procedure TfrmSettings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caFree;
 SaveBeforeExit;
end;

procedure TfrmSettings.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 CanClose := TRUE;
 SaveBeforeExit;
end;




{--------------------------------------------------------------------------------------------------
   SETTINGS
--------------------------------------------------------------------------------------------------}
procedure TfrmSettings.spnHideHintChange(Sender: TObject);
begin
  Application.HintHidePause:= spnHideHint.Value;                                 // Specifies the time interval to wait before hiding the Help Hint if the mouse has not moved from the control or menu item. Windows' default is 2500 ms
end;


procedure TfrmSettings.CanShowHint(Sender: TObject);
begin
  Application.ShowHint:= NOT radHintsOff.Checked;
  if radHintsStatBar.Checked
  then MainForm.StatBar.SimpleText := GetLongHint(Application.Hint);
end;


procedure TfrmSettings.chkAutoStartUpClick(Sender: TObject);
begin
  AppData.RunSelfAtWinStartUp(chkAutoStartUp.Checked);
end;


procedure TfrmSettings.btnDesktopShortcutClick(Sender: TObject);
begin
  csShell.CreateShortcut(AppData.AppName, TRUE);
end;


procedure TfrmSettings.btnSkinsClick(Sender: TObject);
begin
  TfrmSkinDisk.ShowEditor;
end;




{--------------------------------------------------------------------------------------------------
   TRAY & FONTS
--------------------------------------------------------------------------------------------------}
procedure TfrmSettings.AppEventsMinimize(Sender: TObject);
begin
 if chkTrayIcon.Checked
 then TrayIcon.PutIconOnlyInTray;
end;


procedure TfrmSettings.chkTrayIconClick(Sender: TObject);
begin
 TrayIcon.MinimizeToTray:= chkTrayIcon.Checked;
end;


procedure TfrmSettings.TrayIconClick(Sender: TObject);
begin
 TrayIcon.PutIconOnlyInTask;
end;


procedure TfrmSettings.FontDialogApply(Sender: TObject; Wnd: HWND);
begin
 AppData.Font:= FontDialog.Font;
 MainForm.FontSizeChanged;
end;


procedure TfrmSettings.btnFontClick(Sender: TObject);
begin
 FontDialog.Font:= AppData.Font;
 FontDialog.Execute(Self.Handle);
end;






{-------------------------------------------------------------------------------------------------------------
   STUFF
-------------------------------------------------------------------------------------------------------------}

procedure TfrmSettings.btnEnterKeyClick(Sender: TObject);
begin
  Proteus.ShowEnterKeyBox;
end;


procedure TfrmSettings.btnCrashClick(Sender: TObject);
begin
  cmDebugger.GenerateCrashNIL;
end;


procedure SetCaption(CONST CaptionText: string);
begin
 if CaptionText= ''
 then Application.MainForm.Caption:= AppData.AppName+ ' '+ AppData.GetVersionInfoV
 else Application.MainForm.Caption:= AppData.AppName+ ' '+ AppData.GetVersionInfoV+ ' - ' + CaptionText;
end;





end.
