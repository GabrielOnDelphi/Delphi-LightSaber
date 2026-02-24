UNIT FormSkinsDisk;

{=============================================================================================================
   2026.02
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   UNIVERSAL FMX STYLE LOADER

   Loads FMX style files (.style, .fsf) from disk at runtime.
   Provides a visual form for users to select and apply styles.

   Uses TStyleManager.SetStyleFromFile to apply styles.

   USAGE:
     1. Call LoadLastStyle during application initialization (in DPR, before Run):
        LoadLastStyle('Jet.style');      // Default style on first run
        // Pass empty string for default platform style

     2. Style files must be in 'System\Styles' folder relative to AppData.AppSysDir.
        Supports both .style (text) and .fsf (binary) format.

     3. To show style selector: TfrmStyleDisk.ShowAsModal or TfrmStyleDisk.CreateForm

   PLATFORM-SPECIFIC STYLE FILES:
     Each platform (Windows, macOS, Android, iOS) needs its own style files.
     IsStyleCompatible() checks platform compatibility before loading.

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
     c:\Projects\LightSaber\FrameFMX\FormSkinsDisk.pas
     c:\Projects\LightSaber\Demo\FMX\Template - Styles\FMX_Demo_Styles.dpr
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
    lblTop: TLabel;
    lBox: TListBox;
    layBottom: TLayout;
    btnOK: TButton;
    procedure FormCreate (Sender: TObject);
    procedure FormClose  (Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure lBoxChange (Sender: TObject);
    procedure lblTopClick(Sender: TObject);
    procedure btnOKClick (Sender: TObject);
  private
    procedure PopulateStyles;
  public
    procedure FormPreRelease; override;
    class procedure ShowAsModal; static;
  end;


{ Loads the last used style from INI file. Call during app initialization.
  DefaultStyle: Style filename to use on first run (e.g. 'Jet.style'). Pass empty string for default platform style. }
procedure LoadLastStyle(const DefaultStyle: string= '');

{ Returns the path to the styles directory (AppSysDir\Styles\) }
function GetStyleDir: string;


IMPLEMENTATION {$R *.fmx}

USES
   System.IOUtils,
   LightCore.INIFileQuick, LightCore.AppData, LightCore.IO, LightCore,
   LightFmx.Common.AppData, LightFmx.Common.Styles, LightFmx.Common.Dialogs;

CONST
  DefPlatformStyle = 'Default platform style';
  IniKeyStyle      = 'LastStyle';

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
  Result:= AppData.AppSysDir+ Trail('Skins');
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
begin
  { Read from INI }
  CurrentStyleName:= LightCore.INIFileQuick.ReadString(IniKeyStyle, DefaultStyle);

  if CurrentStyleName = ''
  then CurrentStyleName:= DefaultStyle;

  { DefPlatformStyle = use default FMX platform style (don't load any style file) }
  if (CurrentStyleName <> '') AND (CurrentStyleName <> DefPlatformStyle)
  then LoadStyleFromFile(CurrentStyleName);
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
  { Save style name. Don't save if startup was improper (Initializing still TRUE). }
  if NOT AppData.Initializing
  then LightCore.INIFileQuick.WriteString(IniKeyStyle, CurrentStyleName);
end;


procedure TfrmStyleDisk.btnOKClick(Sender: TObject);
begin
  Close;
end;


{ Closes form on Enter or Escape key }
procedure TfrmStyleDisk.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
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
  FullFileName: string;
  Files: TStringDynArray;
begin
  lBox.Items.Clear;
  lBox.Items.Add(DefPlatformStyle);  { First item is the default platform style }
  lblTop.Hint:= GetStyleDir;

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
end;


{ Handles style selection - loads and applies the selected style }
procedure TfrmStyleDisk.lBoxChange(Sender: TObject);
begin
  if lBox.ItemIndex < 0 then EXIT;

  { Disable list to prevent double-clicks during style switching }
  lBox.Enabled:= FALSE;
  try
    CurrentStyleName:= lBox.Items[lBox.ItemIndex];

    if CurrentStyleName <> DefPlatformStyle
    then LoadStyleFromFile(CurrentStyleName);
    { Note: Selecting 'Default platform style' saves the preference.
      The platform default is restored on next application start. }
  FINALLY
    lBox.Enabled:= TRUE;
  END;
end;


end.
