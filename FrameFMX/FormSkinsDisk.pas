UNIT FormSkinsDisk;

{=============================================================================================================
   2026.04.24
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   UNIVERSAL FMX STYLE LOADER

   Loads FMX style files (.style, .fsf) from disk at runtime.
   Provides a visual form for users to select and apply styles.

   Uses TStyleManager.SetStyleFromFile to apply styles.
--------------------------------------------------------------------------------------------------------------
   USAGE:
     Call LoadLastStyle during application initialization (in DPR, before Run):
        LoadLastStyle('Jet.style');      // Default style on first run. Pass empty string for default platform style

     To show style selector:
       Call TfrmStyleDisk.CreateEmbedded or AppData.CreateForm(TfrmStyleDisk)
       Info:
         Style files are organized in platform-specific subfolders under AppData.AppSysDir:
              System\Skins\Win\       - Windows styles (PlatformTarget = '[MSWINDOWS]')
              System\Skins\Android\   - Android styles (PlatformTarget = '[ANDROID]...')
              System\Skins\macOS\     - macOS styles   (PlatformTarget = '[MACOS]')
              System\Skins\iOS\       - iOS styles     (PlatformTarget = '[IOS7]' or '[IOSALTERNATE]')
              System\Skins\Linux\     - Linux styles   (PlatformTarget = '[LINUX]')
          GetStyleDir auto-selects the correct subfolder at compile time via $IFDEF.
          Falls back to root Skins\ if the platform subfolder doesn't exist.
          Supports both .style (text) and .fsf (binary) format.

   PLATFORM-SPECIFIC STYLE FILES:
     Each platform needs its own style files (PlatformTarget embedded in the file metadata).
     IsStyleCompatible() checks platform compatibility before loading.
     On Android/iOS, style files must be added to the Deployment Manager
     (remote path assets\internal\System\Skins\Android\) so StartUpCopy
     copies them to TPath.GetDocumentsPath at first launch.

   KNOWN LIMITATION:
     TStyleManager.SetStyleFromFile may permanently lose ListView selection highlights
     after loading certain incompatible styles (e.g. Air.style).
     See: https://en.delphipraxis.net/topic/14848-inconsistent-behavior-when-loading-styles/
     Workaround: Avoid using known-broken styles, or switch to TStyleBook approach.

     Selecting 'Default platform style' saves the preference to INI.
     The platform default is fully restored on next application start.

   STYLE FOLDERS:
     c:\Users\Public\Documents\Embarcadero\Studio\XX.0\Styles\
     c:\Delphi\Styles & resources\FMX Styles\

   Also see:
     c:\Projects\LightSaber\Demo\FMX\Demo Styles\
     c:\Projects\LightSaber\Demo\FMX\Demo Styles2\
     c:\Delphi\Styles & resources\FMX Styles\ Embarcadero Bug Report\TStyleManager.SetStyleFromFile inconsisten behavior.txt
=============================================================================================================}

INTERFACE
{$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  System.SysUtils, System.Classes, System.UITypes, System.Types,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.Layouts, FMX.ListBox, FMX.Controls.Presentation, FMX.Styles,
  LightFmx.Common.AppData.Form;

TYPE
  TfrmStyleDisk = class(TLightForm)
    Container : TLayout;
    lblTop    : TLabel;
    lBox      : TListBox;
    layBottom : TLayout;
    btnOK     : TButton;
    procedure FormCreate (Sender: TObject);
    procedure FormClose  (Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure lBoxChange (Sender: TObject);
    procedure lblTopClick(Sender: TObject);
    procedure btnOKClick (Sender: TObject);
  private
    FOnEmbeddedClose: TNotifyEvent;
    class var FInstance: TfrmStyleDisk;    // used by CreateEmbedded
    procedure PopulateStyles;
  public
    procedure FormPreRelease; override;
    class procedure ShowAsModal; static;
    class function  CreateEmbedded(AOnClose: TNotifyEvent): TfrmStyleDisk;
    class procedure CloseEmbedded;
    destructor Destroy; override;
  end;


{ Loads the last used style from INI file. Call during app initialization.
  DefaultStyle: Style filename to use on first run (e.g. 'Jet.style'). Pass empty string for default platform style.
  Poison-marker recovery: before applying a style, writes an INI flag. If form streaming later crashes on an
  incompatible style, the flag survives; next launch detects it and falls back to platform default. The flag is
  cleared automatically via TThread.ForceQueue once the main message loop starts pumping. }
procedure LoadLastStyle(const DefaultStyle: string= '');

{ Returns the path to the styles directory (AppSysDir\Styles\) }
function GetStyleDir: string;


IMPLEMENTATION {$R *.fmx}

USES
   System.IOUtils,
   LightCore.INIFileQuick, LightCore.AppData, LightCore.IO, LightCore,
   LightFmx.Common.AppData, LightFmx.Common.Styles, LightFmx.Common.Dialogs;

CONST
  DefPlatformStyle    = 'Default platform style';
  IniKeyStyle         = 'LastStyle';
  IniKeyStyleLoading  = 'StyleLoadInProgress';  // Poison marker: set before applying a style, cleared after MainForm successfully constructs. If still present at next startup, last run crashed on this style.

VAR
  { Unit-level variable for current style name.
    Kept as unit variable (not class var) because LoadLastStyle is called before any form instance exists. The style name is a short filename (not full path) stored in INI for portability when app folder moves. }
  CurrentStyleName: string;



{-----------------------------------------------------------------------------------------------------------------------
   UTILS
-----------------------------------------------------------------------------------------------------------------------}
function GetStyleDir: string;
var
  PlatformDir: string;
begin
  case TOSVersion.Platform of
    pfWindows: PlatformDir:= 'Win';
    pfAndroid: PlatformDir:= 'Android';
    pfMacOS:   PlatformDir:= 'macOS';
    pfiOS:     PlatformDir:= 'iOS';
    pfLinux:   PlatformDir:= 'Linux';
  else
    RAISE Exception.Create('GetStyleDir - Unsupported platform');
  end;

  Result:= AppData.AppSysDir + Trail('Skins') + Trail(PlatformDir);

  { Fallback: if platform subfolder doesn't exist, use root Skins\ folder }
  if NOT DirectoryExists(Result)
  then Result:= AppData.AppSysDir + Trail('Skins');
end;


{ Loads and applies a style file from the styles directory.
  DiskShortName: Style filename without path (e.g. 'Jet.style')
  Returns: TRUE if style was loaded and applied successfully.
  Uses TStyleManager.SetStyleFromFile (see FormStylesMain.pas LoadViaStyleManager). }
function LoadStyleFromFile(const DiskShortName: string): Boolean;
var
  FullPath: string;
begin
  FullPath:= GetStyleDir + DiskShortName;

  if NOT FileExists(FullPath)
  then EXIT(FALSE);

  if NOT IsStyleCompatible(FullPath) then
  begin
    MessageError('Style is not compatible with this platform: ' + FullPath);
    EXIT(FALSE);
  end;

  try
    Result:= TStyleManager.SetStyleFromFile(FullPath);
    if NOT Result
    then MessageError('Failed to apply style: ' + ExtractFileName(FullPath));
  except
    on E: Exception do
    begin
      MessageError('Error loading style: ' + E.Message);
      EXIT(FALSE);
    end;
  end;
end;


procedure LoadLastStyle(const DefaultStyle: string= '');
VAR PoisonedStyle: string;
begin
  { Crash recovery: if the previous run set the poison marker but never cleared it (ConfirmStyleLoaded not reached),
    the style itself is incompatible (e.g. TBitmapLink format change across Delphi versions causes stream misalignment
    inside FMX.Styles during form creation). Drop it and fall back to platform default. }
  PoisonedStyle:= LightCore.INIFileQuick.ReadString(IniKeyStyleLoading, '');
  if PoisonedStyle <> '' then
  begin
    LightCore.INIFileQuick.WriteString(IniKeyStyleLoading, '');
    LightCore.INIFileQuick.WriteString(IniKeyStyle, DefPlatformStyle);
    CurrentStyleName:= DefPlatformStyle;
    MessageError('Previous startup crashed while loading style: ' + PoisonedStyle + CRLF + 'Reverted to platform default.');
    EXIT;
  end;

  { Read from INI }
  CurrentStyleName:= LightCore.INIFileQuick.ReadString(IniKeyStyle, DefaultStyle);

  if CurrentStyleName = ''
  then CurrentStyleName:= DefaultStyle;

  { DefPlatformStyle = use default FMX platform style (don't load any style file) }
  if (CurrentStyleName <> '') AND (CurrentStyleName <> DefPlatformStyle) then
  begin
    { Arm poison marker BEFORE loading. If app crashes during style deserialization or later form streaming,
      marker survives in the INI and triggers fallback on next launch. }
    LightCore.INIFileQuick.WriteString(IniKeyStyleLoading, CurrentStyleName);
    LoadStyleFromFile(CurrentStyleName);

    { Auto-clear the marker once the main message loop starts pumping — guaranteed only if all forms finished
      streaming without crashing. If RealCreateForms AV's, this queued proc never runs and the marker survives. }
    TThread.ForceQueue(NIL, procedure
      begin
        LightCore.INIFileQuick.WriteString(IniKeyStyleLoading, '');
      end);
  end;
end;






{-----------------------------------------------------------------------------------------------------------------------
   SHOW FORM
-----------------------------------------------------------------------------------------------------------------------}

{ Shows style selector as a modal dialog.
  On Android, falls back to non-modal Show. }
class procedure TfrmStyleDisk.ShowAsModal;
begin
  AppData.CreateFormModal(TfrmStyleDisk);
end;


class function TfrmStyleDisk.CreateEmbedded(AOnClose: TNotifyEvent): TfrmStyleDisk;
begin
  // Prevent multiple instances (content is already visible in the host)
  if Assigned(FInstance)
  then EXIT(NIL);

  AppData.CreateForm(TfrmStyleDisk, FInstance, asNone);
  Assert(Assigned(FInstance), 'TfrmStyleDisk.CreateEmbedded: CreateForm returned NIL — must be called after AppData.Run');
  FInstance.FOnEmbeddedClose:= AOnClose;
  Result:= FInstance;
end;


class procedure TfrmStyleDisk.CloseEmbedded;
begin
  if Assigned(FInstance)
  AND FInstance.FEmbedded
  then FreeAndNil(FInstance);
end;


destructor TfrmStyleDisk.Destroy;
begin
  if FEmbedded then
  begin
    if Assigned(Container)
    then Container.Parent:= Self;
    if Assigned(FOnEmbeddedClose)
    then FOnEmbeddedClose(Self);
  end;
  FInstance:= NIL;
  inherited;
end;







{-----------------------------------------------------------------------------------------------------------------------
   FORM LIFECYCLE
-----------------------------------------------------------------------------------------------------------------------}

procedure TfrmStyleDisk.FormCreate(Sender: TObject);
begin
  PopulateStyles;
end;


procedure TfrmStyleDisk.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= TCloseAction.caFree;
end;


procedure TfrmStyleDisk.FormPreRelease;
begin
  inherited;
  { Save style name. Don't save if startup was improper (Initializing still TRUE). }
  if NOT AppData.Initializing
  then LightCore.INIFileQuick.WriteString(IniKeyStyle, CurrentStyleName);
end;


{ Embedded: deferred Free via ForceQueue — FreeAndNil inside an OnClick handler would destroy the button while FMX still accesses it (GetAction), causing an AV on freed memory. }
procedure TfrmStyleDisk.btnOKClick(Sender: TObject);
begin
  if FEmbedded
  then TThread.ForceQueue(NIL, procedure begin CloseEmbedded end)
  else Close;
end;


{ Closes form on Enter or Escape key }
procedure TfrmStyleDisk.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if (Key = vkEscape) or (Key = vkReturn) then
    if FEmbedded
    then TThread.ForceQueue(NIL, procedure begin CloseEmbedded end)
    else Close;
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
  FullFileName: string;
  Files: TStringDynArray;
  SavedOnChange: TNotifyEvent;
begin
  { Suppress OnChange while we programmatically set ItemIndex.
    Android AV fires here: setting ItemIndex during FormCreate triggers lBoxChange → TStyleManager.SetStyle(nil),
    which re-styles every control while the form is still under construction. Windows tolerates it; Android does not. }
  SavedOnChange:= lBox.OnChange;
  lBox.OnChange:= NIL;
  TRY
    lBox.Items.Clear;
    lBox.Items.Add(DefPlatformStyle);  { First item is the default platform style }
    lblTop.Hint:= 'Style files are located in ' + GetStyleDir;

    if NOT DirectoryExists(GetStyleDir) then
    begin
      lblTop.Text:= 'The styles directory could not be located! ' + GetStyleDir + CRLF +
                     'Add styles then click here to refresh the list.';
      EXIT;
    end;

    { List *.style files (text format) }                    //todo: use LightCore.IO.ListFilesOf()
    Files:= TDirectory.GetFiles(GetStyleDir, '*.style');
    for FullFileName in Files do
      lBox.Items.Add(ExtractFileName(FullFileName));

    { List *.fsf files (binary format) }
    Files:= TDirectory.GetFiles(GetStyleDir, '*.fsf');
    for FullFileName in Files do
      lBox.Items.Add(ExtractFileName(FullFileName));

    { Select the currently active style in the list }
    lBox.ItemIndex:= lBox.Items.IndexOf(CurrentStyleName);
    if lBox.ItemIndex < 0
    then lBox.ItemIndex:= 0;  { Default platform style }
  FINALLY
    lBox.OnChange:= SavedOnChange;
  END;
end;


{ Handles style selection - loads and applies the selected style }
procedure TfrmStyleDisk.lBoxChange(Sender: TObject);
begin
  if lBox.ItemIndex < 0 then EXIT;

  { Disable list to prevent double-clicks during style switching }
  lBox.Enabled:= FALSE;
  try
    if lBox.Items[lBox.ItemIndex] = DefPlatformStyle
    then
      begin
        TStyleManager.SetStyle(nil);   { Revert to native platform style }
        CurrentStyleName:= DefPlatformStyle;
      end
    else
      if LoadStyleFromFile(lBox.Items[lBox.ItemIndex])
      then CurrentStyleName:= lBox.Items[lBox.ItemIndex];
  FINALLY
    lBox.Enabled:= TRUE;
  END;
end;


end.
