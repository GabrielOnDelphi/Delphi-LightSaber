UNIT FormSkinsDisk;

{=============================================================================================================
   2026.01.29
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   UNIVERSAL VCL STYLE LOADER

   Loads VCL style styles from disk (.vsf files) at runtime.
   Provides a visual form for users to select and apply styles.

   WARNING:
     * Vcl.Styles & VCL.Forms MUST be present in the DPR file BEFORE Forms.pas
     * DON'T ADD THIS UNIT TO ANY DPK! (enforced by $DENYPACKAGEUNIT)

   USAGE:
     1. In DPR file, before creating main form:
        Application.ShowMainForm:= FALSE;   // Prevents flicker during style loading
        MainForm.Visible:= FALSE;           // Don't show 'vanilla' form until skins are ready

     2. Call LoadLaststyle during application initialization:
        LoadLaststyle('Graphite Green.vsf');  // Default style on first run. Pass empty string for default Windows theme

     3. MainForm.Show;

     4. Styles must be in 'System\Skins' folder relative to AppData.AppSysDir

     5. To show style selector use: TfrmstyleDisk.CreateForm or CreateFormModal

   STYLE-AWARE CODE:
     Use StyleServices.GetStyleColor, StyleServices.GetStyleFontColor, and StyleServices.GetSystemColor from Vcl.Themes unit.

    Hint:
       Use TControl.IsLightStyleColor
--------------------------------------------------------------------------------------------------------------

  KNOWN BUGS

   BUG 1. TfrmStyleDisk form loses its modal property

       TStyleManager.SetStyle triggers RecreateWnd on all forms. The modal form's window handle is destroyed and recreated,
       but the new window lacks the Windows-level owner relationship that enforces z-order. The recreated modal form ends up
       behind the disabled owner windows — the app appears frozen.

       The old workaround (Application.ProcessMessages + BringToFront) didn't work because BringToFront only calls
       SetWindowPos(HWND_TOP), which can't overcome broken window ownership.

       Fix 1: PopupMode = pmAuto in DFM (line 22)
         Tells VCL to set the correct owner window (WndParent) in CreateParams during RecreateWnd. This maintains the
         Windows-level ownership that enforces z-order. Setting it at design-time in the DFM (rather than at runtime) avoids an extra RecreateWnd that VCL triggers when PopupMode changes at runtime.

       Fix 2: ReassertZOrder method (lines 309-314)
         Replaces the broken Application.ProcessMessages + BringToFront. Uses the TOPMOST + NOTOPMOST trick: temporarily makes
         the window topmost, then immediately removes the flag. This forces Windows to recalculate z-order, placing the form at the top of the non-topmost band. SetForegroundWindow gives it input focus.

       Fix 3: DefWinTheme branch (line 333)
         The DefWinTheme branch (SetStyle('Windows')) also triggers RecreateWnd but had no z-order fix at all — this was a secondary bug. Now fixed with the same ReassertZOrder call.

       Sources:
       - https://blogs.embarcadero.com/popupmode-and-popupparent/
       - https://www.experts-exchange.com/questions/26286057/Delphi-7-modal-form-hides-behind-window-on-a-Windows-7-box.html

   BUG 2: XE7
       TStyleManager.IsValidStyle always fails if Vcl.Styles is not in USES list!
       http://stackoverflow.com/questions/30328644/how-to-check-if-a-style-file-is-already-loaded

   BUG 3: caFree
       procedure Tfrm.FormClose(Sender: TObject; var Action: TCloseAction);
       begin
        Action:= caFree; //Delphi bug: Don't use caFree:
       end;

       Fixed in Delphi 11! (quality.embarcadero.com/browse/RSP-33140)

--------------------------------------------------------------------------------------------------------------
   STYLE FOLDERS:
     c:\Projects\Packages\VCL Styles utils\Styles\
     c:\Users\Public\Documents\Embarcadero\Studio\XX.0\Styles\

   Also see:
     c:\Projects\LightSaber\FrameFMX\FormSkinsDisk.pas
     c:\Projects-3rd_Packages\VCL Styles Tools\FrmSkins tester\SkinSettingsTemplate.dpr

   MORE INFO:
     https://subscription.packtpub.com/book/application_development/9781783559589/1/ch01lvl1sec10/changing-the-style-of-your-vcl-application-at-runtime

=============================================================================================================}

INTERFACE
{$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  Winapi.Windows, System.SysUtils, System.Classes, System.UITypes,
  Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls, Vcl.Forms, Vcl.Themes, Vcl.Styles,  {Vcl.Themes, Vcl.Styles MUST be present in the DPR file (before the Forms.pas) or at least here }
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
    procedure ReassertZOrder;
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
               Pass empty string for default Windows theme. }
procedure LoadLastStyle(const DefaultStyle: string= '');



IMPLEMENTATION {$R *.dfm}

USES
   LightVcl.Common.Colors, LightVcl.Common. Translate, LightCore.INIFileQuick, LightCore.AppData, LightVcl.Visual.AppData, LightVcl.Common.ExecuteShell,
   LightVcl.Visual.INIFile, System.IOUtils, LightCore.IO, LightCore, LightCore.Time, LightCore.Types, LightVcl.Common.Dialogs;

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

{ Shows style selector as a modal dialog.
  SetStyle triggers RecreateWnd which breaks modal z-order.
  Fix: PopupMode=pmAuto (set in DFM) + ReassertZOrder after each style change.
  See: http://stackoverflow.com/questions/30328924 }
class procedure TfrmStyleDisk.ShowAsModal;
begin
  AppData.CreateFormModal(TfrmStyleDisk);
end;



{-----------------------------------------------------------------------------------------------------------------------
   FORM LIFECYCLE
-----------------------------------------------------------------------------------------------------------------------}

procedure TfrmStyleDisk.FormCreate(Sender: TObject);
begin
  //OnDefaultStyle:= Notify;
  PopulateStyles; 
  lblTop.Hint:= 'Style files are located in ' + GetStyleDir;
end;


procedure TfrmStyleDisk.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= TCloseAction.caFree;
  { Note: caFree bug (RSP-33140) was fixed in Delphi 11 }
end;


procedure TfrmStyleDisk.FormPreRelease;
begin
  inherited;
  
  { Save using 'Laststyle' key for backward compatibility.
    Don't save if startup was improper (Initializing still TRUE). }
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


{ SetStyle triggers RecreateWnd on all forms. The recreated modal window
  loses its z-order position above the disabled owner.
  TOPMOST + NOTOPMOST forces Windows to recalculate z-order, placing
  this form at the top of the non-topmost band. }
procedure TfrmStyleDisk.ReassertZOrder;
begin
  SetWindowPos(Handle, HWND_TOPMOST,    0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
  SetWindowPos(Handle, HWND_NOTOPMOST,  0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
  SetForegroundWindow(Handle);
end;


{ Handles style selection - loads and applies the selected style }
procedure TfrmStyleDisk.lBoxClick(Sender: TObject);
begin
  if lBox.ItemIndex < 0 then EXIT;

  { Disable list to prevent double-clicks during style switching }
  lBox.Enabled:= FALSE;
  try
    CurrentStyleName:= lBox.Items[lBox.ItemIndex];

    if CurrentStyleName = DefWinTheme
    then
      begin
        TStyleManager.SetStyle('Windows');
        CurrentstyleName:= DefWinTheme;
        ReassertZOrder;
        if Assigned(FOnDefaultstyle)
        then FOnDefaultstyle(Self);
      end
    else
      if LoadStyleFromFile(CurrentStyleName)
      then ReassertZOrder;
  FINALLY
    lBox.Enabled:= TRUE;
  END;
end;


{ Opens local style editor if available, otherwise opens web-based designer }
procedure TfrmStyleDisk.btnStyleEditorClick(Sender: TObject);
begin
  if FileExists(Appdata.AppSysDir + 'styleDesigner.exe')
  then ExecuteFile(Appdata.AppSysDir + 'styleDesigner.exe')
  else ExecuteURL(wwwSkinDesinger);
end;


end.
