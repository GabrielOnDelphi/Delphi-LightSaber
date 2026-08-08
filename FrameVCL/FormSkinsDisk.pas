UNIT FormSkinsDisk;

{=============================================================================================================
   2026.08.06
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   UNIVERSAL VCL STYLE LOADER (from disk)

   Loads VCL style files (.vsf) from AppSysDir\Skins\ and shows a visual selector dialog.
   LoadLastStyle MUST run before AppData.CreateMainForm - it RAISES otherwise.
   This dialog never applies a style live; it saves the choice and asks the user to restart.

   Ordering contract, the RTL trace behind it, the 5 known VCL bugs, INI key history and the consumer list:
     Docs\Skins-VCL.md
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
   DefWinTheme     = 'Windows default theme';
   IniKeyStyle     = 'LastStyle';
   IniKeyStyleOld  = 'LastSkin';   { Pre-2026-02-23 key. Read-only fallback so INIs written before the rename still migrate forward. Docs\Skins-VCL.md }

{ Loads the last used style from INI. Call during app initialization.
  DefaultStyle: style filename used on first run (e.g. 'Graphite Green.vsf'). Empty string = default Windows theme.
  WARNING: must be called BEFORE AppData.CreateMainForm - it RAISES otherwise, in every build config. Docs\Skins-VCL.md }
procedure LoadLastStyle(const DefaultStyle: string= '');

{ The style the app is currently configured to use (filename, or DefWinTheme). Read-only view of the unit state. }
function CurrentStyle: string;



IMPLEMENTATION {$R *.dfm}

USES
   LightVcl.Common.Colors, LightCore.INIFileQuick, LightCore.AppData, LightVcl.Visual.AppData, LightVcl.Common.ExecuteShell,
   System.IOUtils, LightCore.IO, LightCore, LightVcl.Common.Dialogs;

VAR
  { Unit variable, not a class var: LoadLastStyle runs before any form instance exists.
    Holds a short filename (not a full path) so the INI survives the app folder being moved. }
  CurrentStyleName: string;



{-----------------------------------------------------------------------------------------------------------------------
   UTILS
-----------------------------------------------------------------------------------------------------------------------}
function CurrentStyle: string;
begin
  Result:= CurrentStyleName;
end;


function GetStyleDir: string;
begin
  Result:= AppData.AppSysDir+ 'Skins\';
end;


{ Loads and applies a style from the styles directory.
  DiskShortName: style filename without path (e.g. 'MyStyle.vsf').
  Returns TRUE if the style was loaded and applied. }
function LoadstyleFromFile(const DiskShortName: string): Boolean;
var
  FullPath: string;
  Style: TStyleInfo;
begin
  FullPath:= GetStyleDir + DiskShortName;

  if NOT FileExists(FullPath)
  then EXIT(FALSE);

  try
    if NOT TStyleManager.IsValidStyle(FullPath, Style) then
    begin
      AppDataCore.RamLog.AddError('LoadStyleFromFile: not a valid VCL style: ' + FullPath);
      MessageError('Style is not valid: ' + FullPath);
      EXIT(FALSE);
    end;

    { TrySetStyle succeeds if the style is already loaded. If not, load it from file first. }
    if NOT TStyleManager.TrySetStyle(Style.Name, FALSE) then
    begin
      TStyleManager.LoadFromFile(FullPath);
      TStyleManager.SetStyle(Style.Name);
    end;
    Result:= TRUE;
  except
    { Log+show, then swallow. Cannot reraise: the caller (LoadLastStyle) runs in the DPR before
      Application.Run, where an unhandled exception aborts startup - and with ShowMainForm=FALSE
      its error box is invisible (Docs\Skins-VCL.md). IsValidStyle opens a TFileStream, so a locked
      or truncated file raises here. The app continues with the default Windows theme. }
    on E: Exception do
    begin
      AppDataCore.RamLog.AddError('LoadStyleFromFile [' + DiskShortName + ']: ' + E.ClassName + ' - ' + E.Message);
      MessageError('Error loading style: ' + E.Message);
      EXIT(FALSE);
    end;
  end;
end;


procedure LoadLastStyle(const DefaultStyle: string= '');
begin
  { Ordering guard. Applying a style with a live main form leaks its TMainMenuBarStyleHook (AV on the
    next menu click), replaces the window handle, and can swallow the queued WM_POSTINIT so
    FormPostInitialize never fires. Unconditional on purpose - the contract is "before CreateMainForm",
    not "before CreateMainForm if a style happens to be configured". RAISE, not Assert: assertions are
    compiled out in Release. Full reasoning: Docs\Skins-VCL.md }
  if Application.MainForm <> NIL
  then RAISE Exception.Create('LoadLastStyle must be called BEFORE AppData.CreateMainForm. Calling it later leaks the main menu style hook, replaces the form handle, and can suppress FormPostInitialize.');

  { Current key first, then the pre-2026-02-23 one, then the caller's default. We only ever WRITE
    IniKeyStyle, so an old INI migrates forward on the first save. }
  CurrentStyleName:= LightCore.INIFileQuick.ReadString(IniKeyStyle, '');

  if CurrentStyleName = ''
  then CurrentStyleName:= LightCore.INIFileQuick.ReadString(IniKeyStyleOld, '');

  if CurrentStyleName = ''
  then CurrentStyleName:= DefaultStyle;

  if (CurrentStyleName <> '') AND (CurrentStyleName <> DefWinTheme)
  then LoadStyleFromFile(CurrentStyleName);
end;



{-----------------------------------------------------------------------------------------------------------------------
   SHOW FORM
-----------------------------------------------------------------------------------------------------------------------}

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

  { Select the user's current choice. Match on CurrentStyleName, NOT on TStyleManager.ActiveStyle.Name:
    the list holds .vsf FILENAMES ('CyanDusk.vsf') while ActiveStyle.Name is the style's internal name
    ('Cyan Dusk'), so the old lookup never matched and nothing was ever preselected.
    Assigning ItemIndex does not fire OnClick - it only sends LB_SETCURSEL (Vcl.StdCtrls.pas:7564-7573). }
  lBox.ItemIndex:= lBox.Items.IndexOf(CurrentStyleName);
  if lBox.ItemIndex < 0
  then lBox.ItemIndex:= 0;   { Windows default theme }
end;


{ Persists the chosen skin and asks for a restart. Deliberately NEVER calls SetStyle: every live
  SetStyle leaks the main form's TMainMenuBarStyleHook, which AVs on the next menu click (BUG 5 in Docs\Skins-VCL.md). }
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
