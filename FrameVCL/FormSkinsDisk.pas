UNIT FormSkinsDisk;

{=============================================================================================================
   2026.05.15
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   UNIVERSAL VCL STYLE LOADER

   Loads VCL style files (.vsf) from disk and provides a visual selector dialog.

   WARNING:
     * Vcl.Styles & Vcl.Forms MUST be in the DPR USES list BEFORE Forms.pas
     * DON'T ADD THIS UNIT TO ANY DPK (enforced by $DENYPACKAGEUNIT)

   USAGE (AppData DPR):
        AppData:= TAppData.Create(...);
        LoadLastStyle('Graphite Green.vsf'); // BEFORE CreateMainForm. Empty string = default Windows theme.
        AppData.CreateMainForm(TMainForm, MainForm, True, True);
        AppData.Run;

     Styles live in 'System\Skins' relative to AppData.AppSysDir.
     Show selector via TfrmStyleDisk.ShowAsModal.

   STYLE-AWARE CODE:
     Use StyleServices.GetStyleColor / GetStyleFontColor / GetSystemColor (Vcl.Themes).
     See also: TControl.IsLightStyleColor.

--------------------------------------------------------------------------------------------------------------

  KNOWN BUGS / DESIGN CONSTRAINTS

   BUG 1: TStyleManager.SetStyle breaks modal z-order via RecreateWnd
       Mitigated by PopupMode=pmAuto in the DFM. This dialog never calls SetStyle
       at runtime (see BUG 5), so the in-dialog z-order problem can no longer
       arise; LoadLastStyle runs before any form exists.
       Sources: https://blogs.embarcadero.com/popupmode-and-popupparent/

   BUG 2: TStyleManager.IsValidStyle requires Vcl.Styles in USES (XE7+).
       http://stackoverflow.com/questions/30328644

   BUG 3: caFree in FormClose. Fixed in Delphi 11 (RSP-33140). Safe to use.

   BUG 4: EAV in TMainMenuBarStyleHook on dialog close (historical)
       A previous workaround posted a second ForceQueue CM_RECREATEWND in
       FormPreRelease, which could fire inside ProcessMenuLoop's DispatchMessage
       loop and destroy the menu hook mid-loop. Fix: do not post a second
       RecreateWnd. The single CM_CUSTOMSTYLECHANGED that SetStyle already
       posts is enough.
       Related: RSP-38114, RSP-39197 (partially fixed in 11.3, menu-bar variant
       persists in 13.1). HeidiSQL #465 hit the same family of bugs.

   BUG 5: TMainMenuBarStyleHook leak on any live SetStyle (Delphi 13.1)
       Confirmed 2026-05-14/15: once the main form (with a TMainMenu) exists,
       every SetStyle leaks a TMainMenuBarStyleHook. The leaked hook sits in
       freed memory; the form's private FMainMenuBarHook still points at it.
       Crash fires on the next main-menu click — not inside SetStyle itself.
       Even one live SetStyle is enough.

       LoadLastStyle runs BEFORE CreateMainForm, so no TMainMenuBarStyleHook
       exists yet → startup SetStyle is safe. Any later SetStyle leaks.

       Mitigation: this dialog NEVER calls SetStyle. lBoxClick only persists
       the choice to INI and tells the user to close & reopen the app. Same
       UX as HeidiSQL #465.

       Auto-restart via AppData.Restart was rejected: it races the host app's
       window-based single-instance check (FindWindow on the class name still
       finds the dying window during finalization; the new instance resurrects
       the old one and exits).

       Diagnostic notes:
         c:\Delphi\Styles & resources\Known bugs in VCL styles\
         02 - TMainMenuBarStyleHook use-after-free.md

--------------------------------------------------------------------------------------------------------------
   STYLE FOLDERS:
     c:\Projects\Packages\VCL Styles utils\Styles\
     c:\Users\Public\Documents\Embarcadero\Studio\XX.0\Styles\

   See also:
     c:\Projects\LightSaber\FrameFMX\FormSkinsDisk.pas
     c:\Projects-3rd_Packages\VCL Styles Tools\FrmSkins tester\SkinSettingsTemplate.dpr

=============================================================================================================}

INTERFACE
{$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  Winapi.Windows, System.SysUtils, System.Classes, System.UITypes,
  Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls, Vcl.Forms, Vcl.Themes,  {Vcl.Themes, Vcl.Styles MUST be present in the DPR file (before the Forms.pas) or at least here }
  LightVcl.Visual.AppDataForm;

TYPE
  TfrmStyleDisk = class(TLightForm)
    lBox: TListBox;
    lblTop: TLabel;
    pnlBottom: TPanel;
    pnlBtm: TPanel;
    btnOK: TButton;
    btnSkinEditor: TButton;
    lblMoreSkinsTrial: TLabel;
    procedure FormCreate  (Sender: TObject);
    procedure lBoxClick   (Sender: TObject);
    procedure lblTopClick (Sender: TObject);
    procedure btnStyleEditorClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOKClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FOnDefaultstyle: TNotifyEvent;
    procedure PopulateStyles;
  public
    procedure FormPreRelease; override;
    class procedure ShowAsModal; static;
  published
    property OnDefaultstyle: TNotifyEvent read FOnDefaultstyle write FOnDefaultstyle;    { Event handler called when default Windows theme is selected. }
  end;


CONST
   wwwSkinDesinger = 'https://www.bionixwallpaper.com/downloads/Skin_Designer/index.html';

{ Loads the last used style from INI file. Call during app initialization.
  Defaultstyle: Style filename to use on first run (e.g. 'Graphite Green.vsf').
               Pass empty string for default Windows theme.

  WARNING: Must be called BEFORE AppData.CreateMainForm.
    SetStyle posts CM_CUSTOMSTYLECHANGED to all existing forms, triggering RecreateWnd.
    If the main form already exists, RecreateWnd destroys its window handle, and any
    messages queued to that handle are discarded — including the WM_POSTINIT that
    CreateMainForm uses to schedule FormPostInitialize. Result: FormPostInitialize
    never fires, and any state initialized there stays NIL. }
procedure LoadLastStyle(const DefaultStyle: string= '');



IMPLEMENTATION {$R *.dfm}

USES
   LightVcl.Common.Colors, LightCore.INIFileQuick, LightCore.AppData, LightVcl.Visual.AppData, LightVcl.Common.ExecuteShell,
   System.IOUtils, LightCore.IO, LightCore, LightVcl.Common.Dialogs;

CONST
  DefWinTheme  = 'Windows default theme';
  IniKeyStyle  = 'LastStyle';

VAR
  { Unit-level variable for current style name.
    Kept as unit variable (not class var) because LoadLastStyle is called
    before any form instance exists. The style name is a short filename (not full path) stored in INI for portability when app folder moves. }
  CurrentStyleName: string;



{-----------------------------------------------------------------------------------------------------------------------
   UTILS
-----------------------------------------------------------------------------------------------------------------------}
function GetStyleDir: string;
begin
  Result:= AppData.AppSysDir+ 'Skins\';
end;


{ Loads and applies a style from the styles directory.
  DiskShortName: Style filename without path (e.g. 'MyStyle.vsf')
  Returns: TRUE if style was loaded and applied successfully }
function LoadstyleFromFile(const DiskShortName: string): Boolean;
var
  FullPath: string;
  Style: TStyleInfo;
begin
  FullPath:= GetStyleDir + DiskShortName;

  if NOT FileExists(FullPath)
  then EXIT(FALSE);

  if NOT TStyleManager.IsValidStyle(FullPath, Style) then
  begin
    MessageError('Style is not valid: ' + FullPath);
    EXIT(FALSE);
  end;

  { TrySetStyle returns TRUE if the style is already loaded and was set.
    If FALSE, we need to load it first from file. }
  if TStyleManager.TrySetStyle(Style.Name, FALSE)
  then Result:= TRUE
  else
  begin
    TStyleManager.LoadFromFile(FullPath);
    TStyleManager.SetStyle(Style.Name);
    Result:= TRUE;
  end;
end;


procedure LoadLastStyle(const DefaultStyle: string= '');
begin
  { Guard against the most common misuse of this routine.
    If a main form already exists, SetStyle below will RecreateWnd on it and discard the WM_POSTINIT message that AppData.CreateMainForm queued — leaving
    FormPostInitialize unfired and any state initialized there as NIL.
    Use Assert (compiled out in Release) AND a hard raise so the check survives Release builds too. }
  Assert(Application.MainForm = NIL, 'LoadLastStyle must be called BEFORE AppData.CreateMainForm. Calling it after destroys the WM_POSTINIT message and FormPostInitialize never fires.');
  if Application.MainForm <> NIL
  then RAISE Exception.Create('LoadLastStyle must be called BEFORE AppData.CreateMainForm. Calling it after destroys the WM_POSTINIT message and FormPostInitialize never fires.');

  { Read from INI using 'Laststyle' key for backward compatibility }
  CurrentStyleName:= LightCore.INIFileQuick.ReadString(IniKeyStyle, DefaultStyle);

  if CurrentStyleName = ''
  then CurrentStyleName:= Defaultstyle;

  { DefWinTheme = use default Windows theme (don't load any style file) }
  if (CurrentStyleName <> '') AND (CurrentStyleName <> DefWinTheme)
  then LoadStyleFromFile(CurrentStyleName);
end;



{-----------------------------------------------------------------------------------------------------------------------
   SHOW FORM
-----------------------------------------------------------------------------------------------------------------------}

{ Shows style selector as a modal dialog. PopupMode=pmAuto in DFM keeps
  the modal z-order intact (legacy precaution; the dialog no longer calls
  SetStyle so the original z-order problem cannot fire from here). }
class procedure TfrmStyleDisk.ShowAsModal;
begin
  AppData.CreateFormModal(TfrmStyleDisk);
end;




{-----------------------------------------------------------------------------------------------------------------------
   FORM LIFECYCLE
-----------------------------------------------------------------------------------------------------------------------}

procedure TfrmStyleDisk.FormCreate(Sender: TObject);
begin
  PopulateStyles;
  lblTop.Hint:= 'Style files are located in ' + GetStyleDir;
end;


procedure TfrmStyleDisk.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= TCloseAction.caFree;
end;


procedure TfrmStyleDisk.FormPreRelease;
begin
  inherited;

  { Skip save if startup was improper (Initializing still TRUE). }
  if NOT AppData.Initializing
  then LightCore.INIFileQuick.WriteString(IniKeyStyle, CurrentStyleName);
end;


procedure TfrmStyleDisk.btnOKClick(Sender: TObject);
begin
  Close;
end;


{ Closes form on Enter or Escape key }
procedure TfrmStyleDisk.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = vkEscape) or (Key = vkReturn) then Close;
end;



{-----------------------------------------------------------------------------------------------------------------------
   STYLE LIST POPULATION
-----------------------------------------------------------------------------------------------------------------------}

{ Clicking the label refreshes the style list }
procedure TfrmStyleDisk.lblTopClick(Sender: TObject);
begin
  PopulateStyles;
end;


{ Fills the listbox with available styles from the styles directory }
procedure TfrmStyleDisk.PopulateStyles;
var
  s, FullFileName: string;
begin
  lBox.Clear;
  lBox.Items.Add(DefWinTheme);  { First item is the default Windows theme }
  lblTop.Hint:= GetStyleDir;

  if NOT DirectoryExists(GetStyleDir) then
  begin
    lblTop.Caption:= 'The styles directory could not be located! ' + GetStyleDir + CRLF +
                     'Add styles then click here to refresh the list.';
    lblTop.Color:= clRedBright;
    lblTop.Transparent:= FALSE;
    EXIT;
  end;

  { Display all *.vsf files }
  for FullFileName in TDirectory.GetFiles(GetstyleDir, '*.vsf') do
  begin
    s:= ExtractFileName(FullFileName);
    lBox.Items.Add(s);
  end;

  { Select the currently active style in the list }
  lBox.ItemIndex:= lBox.Items.IndexOf(TStyleManager.ActiveStyle.Name);
end;


{ Persists the chosen skin and asks the user to restart. We never call SetStyle
  at runtime — BUG 5 (see file header) leaks a TMainMenuBarStyleHook on every
  live SetStyle, crashing on the next main-menu click. }
procedure TfrmStyleDisk.lBoxClick(Sender: TObject);
VAR NewStyleName: string;
begin
  if lBox.ItemIndex < 0 then EXIT;

  NewStyleName:= lBox.Items[lBox.ItemIndex];
  if NewStyleName = CurrentStyleName
  then EXIT;

  CurrentStyleName:= NewStyleName;
  LightCore.INIFileQuick.WriteString(IniKeyStyle, CurrentStyleName);
  MesajGeneric('The new skin will be applied the next time you start ' + Application.Title + '.' + CRLF + CRLF +
               'Close and reopen the application to see the change.');
end;


{ Opens local style editor if available, otherwise opens web-based designer }
procedure TfrmStyleDisk.btnStyleEditorClick(Sender: TObject);
begin
  if FileExists(Appdata.AppSysDir + 'StyleDesigner.exe')
  then ExecuteFile(Appdata.AppSysDir + 'StyleDesigner.exe')
  else ExecuteURL(wwwSkinDesinger);
end;


end.
