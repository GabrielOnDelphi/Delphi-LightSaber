UNIT FormSkinsDisk;

{=============================================================================================================
   2026.04.25
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
     TStyleManager.SetStyleFromFile may permanently lose ListView selection highlights after loading certain incompatible styles (e.g. Air.style).
     See: https://en.delphipraxis.net/topic/14848-inconsistent-behavior-when-loading-styles/
     Workaround: Avoid using known-broken styles, or switch to TStyleBook approach.

     Selecting 'Default platform style' saves the preference to INI.
     The platform default is fully restored on next application start.

   POISON-MARKER RECOVERY (why this unit looks bigger than it should):
     LoadLastStyle runs in the DPR before Application.Run. If a style file crashes form streaming
     (e.g. TBitmapLink format change between Delphi versions), the app dies before the message
     loop ever pumps — so we cannot show a dialog or clear state from inside the crash.
     Solution: write an INI flag BEFORE loading; clear it AFTER forms streamed OK. If the flag
     survives to the next launch, last run crashed → revert to platform default.
     The clear was originally a TThread.ForceQueue, but ForceQueue silently no-ops on Android in
     some call depths (delphipraxis 14705). Replaced with a 1ms TTimer + helper class — TTimer
     needs a real method (OnTimer is TNotifyEvent, no anonymous overload), and no form exists at
     LoadLastStyle time, so the helper is a TComponent owned by Application.
     The same timer also surfaces PendingStartupMsg, deferring the user-visible warning until
     the message pump is running (FMX dialogs deadlock pre-Run on Android/iOS).

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
  DefaultStyle: Style filename to use on first run (e.g. 'Jet.style'). 
  Pass empty string for default platform style.
  Poison-marker recovery: before applying a style, writes an INI flag. 
  If form streaming later crashes on an incompatible style, the flag survives; next launch detects it and falls back to platform default. 
  The flag is cleared automatically by a one-shot TTimer once Application.Run starts pumping the message loop. }
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

TYPE
  { Tiny owner for the marker-clear timer callback. TTimer.OnTimer is TNotifyEvent
    (procedure of object), and anonymous procs cannot be assigned to it. We need a real method, but no form instance exists at LoadLastStyle time, so use a
    standalone TComponent owned by Application — same lifetime as MarkerClearTimer. }
  TMarkerClearHelper = class(TComponent)
  public
    procedure OnTick(Sender: TObject);
  end;

VAR
  { Unit-level variable for current style name.
    Kept as unit variable (not class var) because LoadLastStyle is called before any form instance exists. The style name is a short filename (not full path) stored in INI for portability when app folder moves. }
  CurrentStyleName  : string;
  MarkerClearTimer  : TTimer;             // One-shot timer fired by Application.Run pump; clears IniKeyStyleLoading on successful startup. Owned by Application, disabled inside its OnTimer handler.
  MarkerClearHelper : TMarkerClearHelper; // Holds the TNotifyEvent method for MarkerClearTimer. Owned by Application.
  PendingStartupMsg : string;             // Recovery message captured pre-Application.Run (poison-marker fallback).
                                          // Surfaced by ShowPendingStartupMessage once the message loop is pumping —
                                          // dialogs from pre-Run code silently fail or deadlock on mobile.



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

  var LoadFailed: Boolean;
  if NOT IsStyleCompatible(FullPath, LoadFailed) then
  begin
    if LoadFailed
    then MessageError('Cannot read style file (corrupted or wrong format): ' + ExtractFileName(FullPath))
    else MessageError('Style is not compatible with this platform: ' + ExtractFileName(FullPath));
    EXIT(FALSE);
  end;

  try
    Result:= TStyleManager.SetStyleFromFile(FullPath);
    if NOT Result then
    begin
      AppDataCore.RamLog.AddError('LoadStyleFromFile: SetStyleFromFile returned FALSE for ' + FullPath);
      MessageError('Failed to apply style: ' + ExtractFileName(FullPath));
    end;
  except
    on E: Exception do
    begin
      { Log+show, then swallow. Cannot reraise: caller (LoadLastStyle) runs in DPR before
        Application.Run, an uncaught exception there aborts startup. We surface the error to
        the user via dialog and to the log for post-mortem; app continues with platform default. }
      AppDataCore.RamLog.AddError('LoadStyleFromFile [' + DiskShortName + ']: ' + E.ClassName + ' - ' + E.Message);
      MessageError('Error loading style: ' + E.Message);
      EXIT(FALSE);
    end;
  end;
end;


{ One-shot deferred clear of the poison marker. Replaces TThread.ForceQueue, which is documented
  unreliable on Android in some call depths (LightFmx.Common.AppData.Form.pas:69, delphipraxis 14705).
  TTimer.OnTimer cannot fire until the FMX message loop is pumping — by which point Application.Run
  has called RealCreateForms, so a streaming-time crash will leave the marker armed for next launch. }
procedure TMarkerClearHelper.OnTick(Sender: TObject);
begin
  LightCore.INIFileQuick.WriteString(IniKeyStyleLoading, '');
  MarkerClearTimer.Enabled:= FALSE;
  { Do NOT FreeAndNil here — destroying the timer inside its own OnTimer can leave the
    platform timer service touching freed memory after we return. Owner = Application
    ensures the timer is freed during normal app shutdown. }

  // Now that the message loop is running, surface any deferred startup warning
  // captured during LoadLastStyle (poison-marker recovery path). MessageError is
  // safe here — dispatched from the timer thread on the message pump.
  if PendingStartupMsg <> '' then
    begin
      var Msg: string;
      Msg:= PendingStartupMsg;
      PendingStartupMsg:= '';   // clear before showing so a second tick is a no-op
      MessageError(Msg);
    end;
end;


procedure StartMarkerClearTimer;
begin
  Assert(MarkerClearTimer = NIL, 'StartMarkerClearTimer: timer already armed — LoadLastStyle called twice?');
  if MarkerClearHelper = NIL
  then MarkerClearHelper:= TMarkerClearHelper.Create(Application);   // Owner=Application => freed at shutdown
  MarkerClearTimer:= TTimer.Create(Application);                     // Owner=Application => freed at shutdown, safe to leak through OnTimer
  MarkerClearTimer.Interval:= 1;                                     // Fire ASAP after pump starts. 0 is invalid on some FMX platforms; 1ms is the conventional one-shot value.
  MarkerClearTimer.OnTimer := MarkerClearHelper.OnTick;
  MarkerClearTimer.Enabled := TRUE;
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
    // CANNOT show a dialog here — LoadLastStyle runs before Application.Run, and on
    // Android/iOS FMX dialogs require a running message loop (silently fail/deadlock).
    // Always log; queue the user-visible warning for after Run starts.
    if Assigned(AppDataCore) AND Assigned(AppDataCore.RamLog)
    then AppDataCore.RamLog.AddError('Previous startup crashed on style: ' + PoisonedStyle + ' — reverted to platform default.');
    PendingStartupMsg:= 'Previous startup crashed while loading style: ' + PoisonedStyle + CRLF + 'Reverted to platform default.';
    StartMarkerClearTimer;   // Arm the timer so OnTick surfaces PendingStartupMsg once the message pump is running. Without this, the user never sees the warning. OnTick safely no-ops on the (already-cleared) INI marker.
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

    if NOT LoadStyleFromFile(CurrentStyleName) then
    begin
      { Non-crash failure (file missing, incompatible, SetStyleFromFile returned FALSE).
        Without this branch CurrentStyleName would still hold the bad name and FormPreRelease
        would re-save it to INI, leaving the user permanently stuck on a missing/broken style. }
      LightCore.INIFileQuick.WriteString(IniKeyStyleLoading, '');     // disarm — no crash, no need to recover next launch
      LightCore.INIFileQuick.WriteString(IniKeyStyle, DefPlatformStyle);
      CurrentStyleName:= DefPlatformStyle;
      EXIT;
    end;

    { Auto-clear the marker once forms have been realized.
      Uses TTimer instead of TThread.ForceQueue because ForceQueue may silently fail on Android in
      some call depths (see LightFmx.Common.AppData.Form.pas:69 + delphipraxis topic 14705).
      The timer fires only after Application.Run starts pumping, by which point RealCreateForms
      has already constructed all forms — so a streaming-time crash leaves the marker intact. }
    StartMarkerClearTimer;
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
  // Only nil the singleton guard if we ARE the guarded instance. A modal-shown
  // TfrmStyleDisk created while an embedded one is alive must NOT clear FInstance,
  // or the embedded form becomes unreachable from CloseEmbedded and leaks.
  if Self = FInstance
  then FInstance:= NIL;
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
  then 
    begin
      TThread.ForceQueue(NIL, procedure begin CloseEmbedded end);   // 2-arg overload with TThreadProcedure exists; 2-arg Queue does NOT (only Queue(AThread, AMethod) or single-arg Queue(AProc)).
    end 
  else Close;
end;


{ Closes form on Enter or Escape key }
procedure TfrmStyleDisk.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if (Key = vkEscape) or (Key = vkReturn) then
    if FEmbedded
    then TThread.ForceQueue(NIL, procedure begin CloseEmbedded end)   // See btnOKClick: 2-arg Queue with anon proc doesn't exist; ForceQueue does.
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
  FullFileName : string;
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

    { List *.style files (text format) }
    var StyleFiles: TStringList;
    StyleFiles:= LightCore.IO.ListFilesOf(GetStyleDir, '*.style', TRUE, FALSE);
    TRY
      for FullFileName in StyleFiles do
        lBox.Items.Add(ExtractFileName(FullFileName));
    FINALLY
      FreeAndNil(StyleFiles);
    END;

    { List *.fsf files (binary format) }
    var FsfFiles: TStringList;
    FsfFiles:= LightCore.IO.ListFilesOf(GetStyleDir, '*.fsf', TRUE, FALSE);
    TRY
      for FullFileName in FsfFiles do
        lBox.Items.Add(ExtractFileName(FullFileName));
    FINALLY
      FreeAndNil(FsfFiles);
    END;

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
