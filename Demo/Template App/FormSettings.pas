UNIT FormSettings;

{=============================================================================================================
   Gabriel Moraru
   2024.10
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

{ToDo: fix issue with Skins. SkinForm loses modality after I apply skins. Maybe I close the form after I apply skins? }

INTERFACE
{$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Forms, Vcl.Controls, Vcl.Samples.Spin, Vcl.Dialogs,
  cvIniFile, cvPathEdit, cmDebugger, cvRadioButton, cvCheckBox, cbAppData, cbAppDataForm,
  cmGuiSettings;

TYPE
  TfrmSettings = class(TLightForm)
    FontDialog         : TFontDialog;
    pgCtrl             : TPageControl;
    tabSystem          : TTabSheet;
    tabHelp            : TTabSheet;
    btnCrash           : TButton;
    btnDesktopShortcut : TButton;
    btnFont            : TButton;
    btnSkins           : TButton;
    chkAutoStartUp     : TCubicCheckBox;
    chkStartMinim      : TCubicCheckBox;
    chkTrayIcon        : TCubicCheckBox;
    chkUser            : TCheckBox;
    GroupBox1          : TGroupBox;
    GroupHelp          : TGroupBox;
    grpSystem          : TGroupBox;
    grpUser            : TGroupBox;
    Label1             : TLabel;
    lblHintHide        : TLabel;
    lblOpacity         : TLabel;
    Path               : TCubicPathEdit;
    radHintsOff        : TCubicRadioButton;
    radHintsStatBar    : TCubicRadioButton;
    radHintsTooltips   : TCubicRadioButton;
    spnHideHint        : TSpinEdit;
    spnOpacity         : TSpinEdit;
    spnUser            : TSpinEdit;
    tabInterface       : TTabSheet;	
    tabUserDefined     : TTabSheet;
    chkLogOnError: TCheckBox;
    procedure btnCrashClick           (Sender: TObject);
    procedure btnDesktopShortcutClick (Sender: TObject);
    procedure btnFontClick            (Sender: TObject);
    procedure btnSkinsClick           (Sender: TObject);
    procedure FontDialogApply         (Sender: TObject; Wnd: HWND);
    procedure FormClose               (Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery          (Sender: TObject; var CanClose: Boolean);
    procedure FormCreate              (Sender: TObject);
    procedure spnHideHintChange       (Sender: TObject);
    procedure FormKeyPress            (Sender: TObject; var Key: Char);
    procedure spnOpacityChange        (Sender: TObject);
  protected
  private
    GuiSettings: TGuiSettings;

    procedure GuiFromObject;  // Called after the main form was fully created
    procedure ObjectFromGUI;

  public
    procedure FormInitialize; override; // Called after the main form was fully created
    procedure FormRelease; override;
    class procedure CreateFormModal(aGuiSettings: TGuiSettings); static;
 end;


IMPLEMENTATION {$R *.dfm}

USES
   ccINIFile, cTranslate, FormSkinsDisk, csShell, FormMain;



{--------------------------------------------------------------------------------------------------
   CONSTRUCTOR
--------------------------------------------------------------------------------------------------}
class procedure TfrmSettings.CreateFormModal(aGuiSettings: TGuiSettings);
VAR frmSettings: TfrmSettings;
begin
 TAppData.RaiseIfStillInitializing;

 Assert(aGuiSettings <> NIL);

 AppData.CreateFormHidden(TfrmSettings, frmSettings);

 if Translator <> NIL
 then Translator.LoadFormTranlation(frmSettings);

 frmSettings.GuiSettings:= aGuiSettings;
 frmSettings.ShowModal;    { Closed by mrOk/mrCancel. Set to caFree. }
end;



{--------------------------------------------------------------------------------------------------
   CONSTRUCTOR
--------------------------------------------------------------------------------------------------}
procedure TfrmSettings.FormCreate(Sender: TObject);
begin
  GuiFromObject;
  FontDialog.Font.Assign(Font);
end;


procedure TfrmSettings.FormInitialize;
begin
  inherited FormInitialize;
  btnCrash.Visible:= AppData.BetaTesterMode;
end;


procedure TfrmSettings.FormRelease;
begin
  inherited;
  ObjectFromGUI;
end;





{--------------------------------------------------------------------------------------------------
 CLOSE
--------------------------------------------------------------------------------------------------}
procedure TfrmSettings.FormKeyPress(Sender: TObject; var Key: Char);
begin
  Assert(KeyPreview, 'In order to close with Esc we need to activate KeyPreview!');
  
  {KeyDown event is less safer. If the form has a drop down controls with event assigned then form will close before the actual control finishes his drop-down behavior (like lookup combobox for example). Also if the form has caFree set on Close and combobox has OnCloseUp event you could get an AV because the form is closed before the combobox closeup event is called!
  https://stackoverflow.com/questions/41940049/onkeypress-for-escape-closes-form-by-default }
  if Ord(key) = vk_Escape then Close;
end;


procedure TfrmSettings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caFree;
end;


procedure TfrmSettings.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 CanClose := TRUE;
end;


procedure TfrmSettings.GuiFromObject;
begin
 { Get settings from AppData }
 Path            .Path    := AppData.UserPath;
 spnOpacity      .Value   := AppData.Opacity;
 chkAutoStartUp  .Checked := AppData.autoStartUp;
 chkStartMinim   .Checked := AppData.StartMinim;
 chkTrayIcon     .Checked := AppData.Minimize2Tray;
 radHintsOff     .Checked := AppData.HintType = htOff;
 radHintsTooltips.Checked := AppData.HintType = htTooltips;
 radHintsStatBar .Checked := AppData.HintType = htStatBar;
 spnHideHint     .Value   := AppData.HideHint;
 chkLogOnError   .Checked := AppData.RamLog.ShowonError;

 { Demo/template settings }
 chkUser         .Checked := GuiSettings.bUser;   // Replace this with your own code
 spnUser         .Value   := GuiSettings.iUser;   // Replace this with your own code
end;


procedure TfrmSettings.ObjectFromGUI;
begin
 { Save settings to AppData }
 AppData.UserPath      := Path.Path;
 AppData.Opacity       := spnOpacity    .Value;
 AppData.autoStartUp   := chkAutoStartUp.Checked;
 AppData.StartMinim    := chkStartMinim .Checked;
 AppData.Minimize2Tray := chkTrayIcon   .Checked;
 AppData.HideHint      := spnHideHint   .Value;
 AppData.RamLog.ShowonError   := chkLogOnError .Checked;

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
 //ToDo: Application.MainForm.FontSizeChanged; Let the form recalculate its GUI stuff when font size changes
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
