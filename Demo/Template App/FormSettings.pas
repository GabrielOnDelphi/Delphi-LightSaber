UNIT FormSettings;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------
   A template form that allows the user to change the behavior/settings of an application:
     Skins
     Tooltips & Hints
     Language
     Autostartup
     Font size
     User-defined application storage folder
=============================================================================================================}


INTERFACE
{$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.StdCtrls, Vcl.ComCtrls, VCL.Forms, Vcl.Controls, Vcl.Samples.Spin, Vcl.Dialogs,
  cvIniFile, cvPathEdit, cmDebugger, cvRadioButton, cvCheckBox, cbAppData, cmGuiSettings;

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
    tabUserDefined: TTabSheet;
    grpUser: TGroupBox;
    Label1: TLabel;
    spnUser: TSpinEdit;
    chkUser: TCheckBox;
    spnOpacity: TSpinEdit;
    lblOpacity: TLabel;
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
    procedure spnOpacityChange(Sender: TObject);
  protected
    procedure GuiFromObject;
    procedure ObjectFromGUI; // Called after the main form was fully created
  private
    Saved: Boolean;
    procedure WMEndSession  (VAR Msg: TWMEndSession); message WM_ENDSESSION;
    procedure LateInitialize(VAR Msg: TMessage); message MSG_LateFormInit; // Called after the main form was fully created
    procedure SaveBeforeExit;
  public
    class procedure CreateModal; static;
 end;

VAR
   GuiSettings: TGuiSettings;


IMPLEMENTATION {$R *.dfm}

USES
   cTranslate, FormSkinsDisk, csShell, FormMain;


{--------------------------------------------------------------------------------------------------
   CONSTRUCTOR
--------------------------------------------------------------------------------------------------}
class procedure TfrmSettings.CreateModal;
begin
 TAppData.RaiseIfStillInitializing;
 //Assert(GuiSettings <> NIL);

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
 { Get settings from AppData }
 Path            .Path    := AppData.UserPath;
 chkAutoStartUp  .Checked := AppData.autoStartUp;
 chkStartMinim   .Checked := AppData.StartMinim;
 chkTrayIcon     .Checked := AppData.Minimize2Tray;
 radHintsOff     .Checked := AppData.HintType = htOff;
 radHintsTooltips.Checked := AppData.HintType = htTooltips;
 radHintsStatBar .Checked := AppData.HintType = htStatBar;
 spnHideHint     .Value   := AppData.HideHint;
 spnOpacity      .Value   := AppData.Opacity;

 { Demo/template settings }
 chkUser         .Checked := GuiSettings.bUser;   // Replace this with your own code
 spnUser         .Value   := GuiSettings.iUser;   // Replace this with your own code
end;


procedure TfrmSettings.ObjectFromGUI;
begin
 { Save settings to AppData }
 AppData.UserPath     := Path.Path;
 AppData.autoStartUp  := chkAutoStartUp.Checked;
 AppData.StartMinim   := chkStartMinim .Checked;
 AppData.Minimize2Tray:= chkTrayIcon   .Checked;
 AppData.HideHint     := spnHideHint   .Value;
 AppData.Opacity      := spnOpacity    .Value;

 if radHintsOff     .Checked then AppData.HintType := htOff else
 if radHintsTooltips.Checked then AppData.HintType := htTooltips else
 if radHintsStatBar .Checked then AppData.HintType := htStatBar
   else RAISE Exception.Create('Undefined HintType!');

 { Demo/template settings }
 GuiSettings.bUser:= chkUser.Checked;   // Replace this with your own code
 GuiSettings.iUser:= spnUser.Value;     // Replace this with your own code
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
  //AppData.RunSelfAtWinStartUp(chkAutoStartUp.Checked);
end;


procedure TfrmSettings.btnDesktopShortcutClick(Sender: TObject);
begin
  csShell.CreateShortcut(AppData.AppName, TRUE);
end;


procedure TfrmSettings.btnSkinsClick(Sender: TObject);
begin
  TfrmSkinDisk.CreateFormModal;
end;


procedure TfrmSettings.spnOpacityChange(Sender: TObject);
begin
 AlphaBlend:= spnOpacity.Value< 255;
 AlphaBlendValue:= spnOpacity.Value;
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
