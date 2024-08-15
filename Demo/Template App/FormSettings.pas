UNIT FormSettings;

INTERFACE
{$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.StdCtrls, Vcl.ComCtrls, VCL.Forms, Vcl.AppEvnts, Vcl.Controls, Vcl.Samples.Spin, Vcl.Dialogs, Vcl.ExtCtrls,
  cvIniFile, csSystem, cbDialogs, coolTrayIcon, cvPathEdit, cpProteus, cmDebugger,
  cvRadioButton, cvCheckBox, cbAppData, cpProteusIO, cvTrayIcon, FoldPanel, cmGuiSettings;

TYPE
  TfrmSettings = class(TForm)
    FontDialog         : TFontDialog;
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
    procedure btnCrashClick           (Sender: TObject);
    procedure btnDesktopShortcutClick (Sender: TObject);
    procedure btnFontClick            (Sender: TObject);
    procedure btnSkinsClick           (Sender: TObject);
    procedure chkAutoStartUpClick     (Sender: TObject);
    procedure FontDialogApply         (Sender: TObject; Wnd: HWND);
    procedure FormClose               (Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery          (Sender: TObject; var CanClose: Boolean);
    procedure FormCreate              (Sender: TObject);
    procedure FormDestroy             (Sender: TObject);
    procedure spnHideHintChange       (Sender: TObject);
  protected
    procedure GuiFromObject;
    procedure ObjectFromGUI; // Called after the main form was fully created
  private
    Saved: Boolean;
    procedure WMEndSession  (VAR Msg: TWMEndSession); message WM_ENDSESSION;
    procedure LateInitialize(VAR Msg: TMessage); message MSG_LateAppInit; // Called after the main form was fully created
    procedure SaveBeforeExit;
  public
    class procedure CreateModal; static;
 end;


VAR GuiSettings: TGuiSettings;


IMPLEMENTATION {$R *.dfm}

USES
   ccCore, cTranslate, FormSkinsDisk, csShell, FormMain;



{--------------------------------------------------------------------------------------------------
   CONSTRUCTOR
--------------------------------------------------------------------------------------------------}
class procedure TfrmSettings.CreateModal;
begin
 TAppData.RaiseIfStillInitializing;
 Assert(GuiSettings <> NIL);

 VAR frmSettings:= TfrmSettings.Create(NIL);
 frmSettings.GuiFromObject;
 frmSettings.Font:= Application.MainForm.Font;     { Themes }
 frmSettings.FontDialog.Font.Assign(frmSettings.Font);
 Translator.LoadFormTranlation(frmSettings);
 frmSettings.ShowModal;      { Closed by mrOk/mrCancel }
 frmSettings.ObjectFromGUI;
 FreeAndNil(frmSettings);    { We need to free the form because the Close will only hide the form! }
end;





{--------------------------------------------------------------------------------------------------
   CONSTRUCTOR
--------------------------------------------------------------------------------------------------}
procedure TfrmSettings.FormCreate(Sender: TObject);
begin
 Saved:= FALSE;
end;


procedure TfrmSettings.LateInitialize;
begin
 btnCrash.Visible:= AppData.BetaTesterMode;
 LoadForm(Self, TRUE);
end;


procedure TfrmSettings.SaveBeforeExit;  { I need to put SaveBeforeExit in only two places: OnCloseQueryand OnDestroy. Details: https://groups.google.com/forum/#!msg/borland.public.delphi.objectpascal/82AG0_kHonU/ft53lAjxWRMJ }
begin
 if NOT AppData.Initializing            { We don't save anything if the start up was improper! }
 AND NOT Saved then
  begin
   Saved:= TRUE;
   SaveForm(Self, TRUE);
   ObjectFromGUI;
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


procedure TfrmSettings.GuiFromObject;
begin
 chkAutoStartUp  .Checked  := GuiSettings.autoStartUp;
 chkStartMinim   .Checked  := GuiSettings.StartMinim;
 chkTrayIcon     .Checked  := GuiSettings.Minimize2Tray;
 Path            .Path     := GuiSettings.UserPath;
 radHintsOff     .Checked  := GuiSettings.HintType = htOff;
 radHintsTooltips.Checked  := GuiSettings.HintType = htTooltips;
 radHintsStatBar .Checked  := GuiSettings.HintType = htStatBar;
 spnHideHint     .Value    := GuiSettings.HideHint;
end;


procedure TfrmSettings.ObjectFromGUI;
begin
 GuiSettings.autoStartUp  := chkAutoStartUp.Checked;
 GuiSettings.StartMinim   := chkStartMinim .Checked;
 GuiSettings.Minimize2Tray:= chkTrayIcon   .Checked;
 GuiSettings.UserPath     := Path          .Path   ;
 GuiSettings.HideHint     := spnHideHint   .Value  ;
 if radHintsOff     .Checked then GuiSettings.HintType := htOff else
 if radHintsTooltips.Checked then GuiSettings.HintType := htTooltips else
 if radHintsStatBar .Checked then GuiSettings.HintType := htStatBar else RAISE Exception.Create('Undefined HintType!');
end;




{--------------------------------------------------------------------------------------------------
   SETTINGS
--------------------------------------------------------------------------------------------------}
procedure TfrmSettings.spnHideHintChange(Sender: TObject);
begin
  Application.HintHidePause:= spnHideHint.Value;                                 // Specifies the time interval to wait before hiding the Help Hint if the mouse has not moved from the control or menu item. Windows' default is 2500 ms
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
procedure TfrmSettings.FontDialogApply(Sender: TObject; Wnd: HWND);
begin
 AppData.Font:= FontDialog.Font; // Apply this font to all existing forms.
 //Application.MainForm.FontSizeChanged;
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

procedure TfrmSettings.btnCrashClick(Sender: TObject);
begin
  cmDebugger.GenerateCrashNIL;
end;







end.
