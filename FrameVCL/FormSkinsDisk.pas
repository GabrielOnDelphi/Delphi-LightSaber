UNIT FormSkinsDisk;

{=============================================================================================================
   2026.01.29
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   UNIVERSAL SKIN LOADER

   Loads VCL style skins from disk (.vsf files) at runtime.
   Provides a visual form for users to select and apply skins.

   WARNING:
     * Vcl.Styles & VCL.Forms MUST be present in the DPR file BEFORE Forms.pas
     * DON'T ADD THIS UNIT TO ANY DPK! (enforced by $DENYPACKAGEUNIT)

   USAGE:
     1. In DPR file, before creating main form:
        Application.ShowMainForm:= FALSE;   // Prevents flicker during skin loading
        MainForm.Visible:= FALSE;

     2. Call LoadLastSkin during application initialization:
        LoadLastSkin('Graphite Green.vsf');  // Default skin on first run
        // Pass empty string for default Windows theme

     3. Show main form:
        MainForm.Show;

     4. Skins must be in 'System\skins' folder relative to AppData.AppSysDir

     5. To show skin selector: TfrmSkinDisk.CreateForm or CreateFormModal

   STYLE-AWARE CODE:
     Use StyleServices.GetStyleColor, StyleServices.GetStyleFontColor,
     and StyleServices.GetSystemColor from Vcl.Themes unit.

  KNOWN BUGS - This form loses its modal property

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

   KNOWN BUGS:

     XE7: TStyleManager.IsValidStyle always fails if Vcl.Styles is not in USES list!
       http://stackoverflow.com/questions/30328644/how-to-check-if-a-style-file-is-already-loaded

   KNOWN BUGS:
     caFree
       procedure Tfrm.FormClose(Sender: TObject; var Action: TCloseAction);
       begin
        Action:= caFree;
        Delphi bug: Don't use caFree: 
       end;

     (fixed in Delphi 11):
     https://quality.embarcadero.com/browse/RSP-33140

     Solution:
       https://stackoverflow.com/questions/70840792/how-to-patch-vcl-forms-pas
--------------------------------------------------------------------------------------------------------------
   SKIN FOLDERS:
     c:\Projects\Packages\VCL Styles utils\Styles\
     c:\Users\Public\Documents\Embarcadero\Studio\XX.0\Styles\

   TESTER:
     c:\Projects\Packages\VCL Styles Tools\FrmSkins tester\

   MORE INFO:
     https://subscription.packtpub.com/book/application_development/9781783559589/1/ch01lvl1sec10/changing-the-style-of-your-vcl-application-at-runtime

=============================================================================================================}

INTERFACE
{$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  Winapi.Windows, System.SysUtils, System.Classes, 
  Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls, Vcl.Forms,
  Vcl.Themes, Vcl.Styles,  {Vcl.Themes, Vcl.Styles MUST be present in the DPR file (before the Forms.pas) or at least here }
  LightVcl.Visual.AppDataForm;

TYPE
  TfrmSkinDisk = class(TLightForm)
    lBox: TListBox;
    lblTop: TLabel;
    pnlBottom: TPanel;
    pnlBtm: TPanel;
    btnOK: TButton;
    btnSkinEditor: TButton;
    lblMoreSkinsTrial: TLabel;
    procedure FormCreate  (Sender: TObject);
    procedure FormDestroy (Sender: TObject);
    procedure lBoxClick   (Sender: TObject);
    procedure lblTopClick (Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure btnSkinEditorClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOKClick(Sender: TObject);
  private
    FOnDefaultSkin: TNotifyEvent;
    procedure PopulateSkins;
    procedure ReassertZOrder;
  public
    class procedure ShowAsModal; static;
    class procedure CreateForm(Notify: TNotifyEvent= NIL); static;
  published
    property OnDefaultSkin: TNotifyEvent read FOnDefaultSkin write FOnDefaultSkin;
  end;


CONST
   wwwSkinDesinger = 'https://www.bionixwallpaper.com/downloads/Skin_Designer/index.html';

{ Loads the last used skin from INI file. Call during app initialization.
  DefaultSkin: Skin filename to use on first run (e.g. 'Graphite Green.vsf').
               Pass empty string for default Windows theme. }
procedure LoadLastSkin(const DefaultSkin: string= '');



IMPLEMENTATION {$R *.dfm}

USES
   LightVcl.Common.Colors, LightVcl.Common. Translate, LightCore.INIFileQuick, LightCore.AppData, LightVcl.Visual.AppData, LightVcl.Common.ExecuteShell,
   LightVcl.Visual.INIFile, System.IOUtils, LightCore.IO, LightCore, LightCore.Time, LightCore.Types, LightVcl.Common.Dialogs;

CONST
  DefWinTheme = 'Windows default theme';

VAR
  { Unit-level variable for current skin name.
    Kept as unit variable (not class var) because LoadLastSkin is called
    before any form instance exists. The skin name is a short filename (not full path) stored in INI for portability when app folder moves. }
  CurrentSkinName: string;



{-----------------------------------------------------------------------------------------------------------------------
   UTILS
-----------------------------------------------------------------------------------------------------------------------}
function GetSkinDir: string;
begin
  Result:= Appdata.AppSysDir+ 'Skins\';
end;


{ Loads and applies a skin from the skins directory.
  DiskShortName: Skin filename without path (e.g. 'MyStyle.vsf')
  Returns: TRUE if skin was loaded and applied successfully }
function LoadSkinFromFile(const DiskShortName: string): Boolean;
var
  FullPath: string;
  Style: TStyleInfo;
begin
  FullPath:= GetSkinDir + DiskShortName;

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


procedure LoadLastSkin(const DefaultSkin: string= '');
begin
  { Read from INI using 'LastSkin' key for backward compatibility }
  CurrentSkinName:= LightCore.INIFileQuick.ReadString('LastSkin', DefaultSkin);

  if CurrentSkinName = ''
  then CurrentSkinName:= DefaultSkin;

  { DefWinTheme = use default Windows theme (don't load any skin file) }
  if (CurrentSkinName <> '') AND (CurrentSkinName <> DefWinTheme)
  then LoadSkinFromFile(CurrentSkinName);
end;










{-----------------------------------------------------------------------------------------------------------------------
   SHOW EDITOR
-----------------------------------------------------------------------------------------------------------------------}

{ Shows skin selector as a modal dialog.
  SetStyle triggers RecreateWnd which breaks modal z-order.
  Fix: PopupMode=pmAuto (set in DFM) + ReassertZOrder after each style change.
  See: http://stackoverflow.com/questions/30328924 }
class procedure TfrmSkinDisk.ShowAsModal;
begin
  AppData.CreateFormModal(TfrmSkinDisk);
end;

{ Shows skin selector as a non-modal form.
  Notify: Optional event handler called when default Windows theme is selected. }
class procedure TfrmSkinDisk.CreateForm(Notify: TNotifyEvent= NIL);
var
  frmEditor: TfrmSkinDisk;
begin
  AppData.CreateFormHidden(TfrmSkinDisk, frmEditor);
  frmEditor.OnDefaultSkin:= Notify;  
  frmEditor.Show;
end;






{-----------------------------------------------------------------------------------------------------------------------
   FORM LIFECYCLE
-----------------------------------------------------------------------------------------------------------------------}

procedure TfrmSkinDisk.FormCreate(Sender: TObject);
begin
  LoadForm;
  PopulateSkins;
  lblTop.Hint:= 'Skin files are located in ' + GetSkinDir;
end;


procedure TfrmSkinDisk.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caFree;
  { Note: caFree bug (RSP-33140) was fixed in Delphi 11 }
end;


procedure TfrmSkinDisk.FormDestroy(Sender: TObject);
begin
  SaveForm;
  { Save using 'LastSkin' key for backward compatibility.
    Don't save if startup was improper (Initializing still TRUE). }
  if NOT AppData.Initializing
  then LightCore.INIFileQuick.WriteString('LastSkin', CurrentSkinName);
end;


procedure TfrmSkinDisk.btnOKClick(Sender: TObject);
begin
  Close;
end;





{-----------------------------------------------------------------------------------------------------------------------
   SKIN LIST POPULATION
-----------------------------------------------------------------------------------------------------------------------}

{ Clicking the label refreshes the skin list }
procedure TfrmSkinDisk.lblTopClick(Sender: TObject);
begin
  PopulateSkins;
end;


{ Fills the listbox with available skins from the skins directory }
procedure TfrmSkinDisk.PopulateSkins;
var
  s, FullFileName: string;
begin
  lBox.Clear;
  lBox.Items.Add(DefWinTheme);  { First item is the default Windows theme }
  lblTop.Hint:= GetSkinDir;

  if NOT DirectoryExists(GetSkinDir) then
  begin
    lblTop.Caption:= 'The skin directory could not be located! ' + GetSkinDir + CRLF +
                     'Add skins then click here to refresh the list.';
    lblTop.Color:= clRedBright;
    lblTop.Transparent:= FALSE;
    EXIT;
  end;

  { Display all *.vsf files }
  for FullFileName in TDirectory.GetFiles(GetSkinDir, '*.vsf') do
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
procedure TfrmSkinDisk.ReassertZOrder;
begin
  SetWindowPos(Handle, HWND_TOPMOST,    0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
  SetWindowPos(Handle, HWND_NOTOPMOST,  0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
  SetForegroundWindow(Handle);
end;


{ Handles skin selection - loads and applies the selected skin }
procedure TfrmSkinDisk.lBoxClick(Sender: TObject);
begin
  if lBox.ItemIndex < 0 then EXIT;

  { Disable list to prevent double-clicks during style switching }
  lBox.Enabled:= FALSE;
  try
    CurrentSkinName:= lBox.Items[lBox.ItemIndex];

    if CurrentSkinName = DefWinTheme
    then
      begin
        TStyleManager.SetStyle('Windows');
        CurrentSkinName:= DefWinTheme;
        ReassertZOrder;
        if Assigned(FOnDefaultSkin)
        then FOnDefaultSkin(Self);
      end
    else
      if LoadSkinFromFile(CurrentSkinName)
      then ReassertZOrder;
  FINALLY
    lBox.Enabled:= TRUE;
  END;
end;


{ Opens local skin editor if available, otherwise opens web-based designer }
procedure TfrmSkinDisk.btnSkinEditorClick(Sender: TObject);
begin
  if FileExists(Appdata.AppSysDir + 'SkinDesigner.exe')
  then ExecuteFile(Appdata.AppSysDir + 'SkinDesigner.exe')
  else ExecuteURL(wwwSkinDesinger);
end;


{ Closes form on Enter or Escape key }
procedure TfrmSkinDisk.FormKeyPress(Sender: TObject; var Key: Char);
begin
 if Ord(Key) = VK_RETURN then Close;
 if Ord(Key) = VK_ESCAPE then Close;
end;


end.
