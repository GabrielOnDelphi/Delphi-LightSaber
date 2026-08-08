UNIT FormSkinsRes;

{=============================================================================================================
   2026.08.06
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   UNIVERSAL VCL SKIN LOADER (from resources)

   Loads VCL styles linked into the EXE via an $R directive on the .vsf and shows a visual selector dialog.
   Unlike FormSkinsDisk (which reads .vsf files from disk) this unit stores the style NAME, not a filename.
   It has NO "before CreateMainForm" guard and DOES apply styles live - a different contract on purpose.

   Why the two units must not be unified, the z-order fix, and the known VCL bugs:
     Docs\Skins-VCL.md
=============================================================================================================}

INTERFACE
{$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  Winapi.Windows, System.SysUtils, System.Classes,
  Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls, Vcl.Forms,
  Vcl.Themes, Vcl.Styles,  {Vcl.Themes, Vcl.Styles MUST be present in the DPR file (before the Forms.pas) or at least here }
  LightVcl.Visual.AppDataForm;

TYPE
  TfrmSkinRes = class(TLightForm)
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
    procedure btnSkinEditorClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOKClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    FOnDefaultStyle: TNotifyEvent;
    procedure PopulateStyles;
    procedure ReassertZOrder;
  public
    procedure FormPreRelease; override;
    class procedure ShowAsModal; static;
    { Shows the skin selector form. Modal=TRUE for modal dialog, FALSE for non-modal. }
    class procedure CreateForm(Modal: Boolean; Notify: TNotifyEvent= NIL); static;
  published
    property OnDefaultStyle: TNotifyEvent read FOnDefaultStyle write FOnDefaultStyle;    { Event handler called when default Windows theme is selected. }
  end;

CONST
  wwwSkinDesinger = 'https://www.bionixwallpaper.com/downloads/Skin_Designer/index.html';
  DefWinTheme     = 'Windows default theme';
  IniKeySkin      = 'LastSkin';   { This unit's LIVE key, holding a style NAME. Not the same as FormSkinsDisk's legacy 'LastSkin', which held a .vsf filename. Docs\Skins-VCL.md }

{ Loads the last used skin from INI. Call during app initialization.
  DefaultStyle: style NAME used on first run (e.g. 'Carbon', not 'Carbon.vsf'). Empty string = default Windows theme. }
procedure LoadLastStyle(const DefaultStyle: string= '');

{ The style the app is currently configured to use (style name, or DefWinTheme). Read-only view of the unit state. }
function CurrentStyle: string;



IMPLEMENTATION {$R *.dfm}

USES
  LightVcl.Common.ExecuteShell,
  LightCore.AppData,
  LightVcl.Visual.AppData,
  LightVcl.Visual.INIFile,
  LightVcl.Common. Translate,
  LightCore.INIFileQuick;

VAR
  { Unit variable, not a class var: LoadLastStyle runs before any form instance exists. Holds the style NAME. }
  CurrentStyleName: string;



{-----------------------------------------------------------------------------------------------------------------------
   SKIN LOADING
-----------------------------------------------------------------------------------------------------------------------}
function CurrentStyle: string;
begin
  Result:= CurrentStyleName;
end;


procedure LoadLastStyle(const DefaultStyle: string= '');
begin
  CurrentStyleName:= LightCore.INIFileQuick.ReadString(IniKeySkin, DefaultStyle);

  if CurrentStyleName = ''
  then CurrentStyleName:= DefaultStyle;

  { TrySetStyle, not SetStyle: SetStyle raises ECustomStyleException when the INI-saved style is no longer
    linked into this build of the EXE, killing startup before any form exists. TrySetStyle keeps the default theme. }
  if (CurrentStyleName <> '') AND (CurrentStyleName <> DefWinTheme)
  then TStyleManager.TrySetStyle(CurrentStyleName, FALSE);
end;



{-----------------------------------------------------------------------------------------------------------------------
   SHOW FORM
-----------------------------------------------------------------------------------------------------------------------}

class procedure TfrmSkinRes.ShowAsModal;
begin
  AppData.CreateFormModal(TfrmSkinRes);
end;


{ Modal: TRUE for modal dialog, FALSE for non-modal.
  Non-modal is safer - after a live style change the modal form can lose its modal attribute (BUG 1 in Docs\Skins-VCL.md). }
class procedure TfrmSkinRes.CreateForm(Modal: Boolean; Notify: TNotifyEvent= NIL);
var
  frmEditor: TfrmSkinRes;
begin
  AppData.CreateFormHidden(TfrmSkinRes, frmEditor);
  frmEditor.OnDefaultStyle:= Notify;

  if Modal
  then frmEditor.ShowModal
  else frmEditor.Show;
end;


{-----------------------------------------------------------------------------------------------------------------------
   FORM LIFECYCLE
-----------------------------------------------------------------------------------------------------------------------}

procedure TfrmSkinRes.FormCreate(Sender: TObject);
begin
  PopulateStyles;
end;


procedure TfrmSkinRes.FormDestroy(Sender: TObject);
begin
  { The chosen skin is persisted in FormPreRelease (guaranteed single call, see TLightForm.saveBeforeExit).
    The form layout is saved automatically by TLightForm. Nothing else to clean up here. }
end;


procedure TfrmSkinRes.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= TCloseAction.caFree;
end;


procedure TfrmSkinRes.FormPreRelease;
begin
  inherited;

  { Skip save if startup was improper (Initializing still TRUE). }
  if NOT AppData.Initializing
  then LightCore.INIFileQuick.WriteString(IniKeySkin, CurrentStyleName);
end;


procedure TfrmSkinRes.btnOKClick(Sender: TObject);
begin
  Close;
end;


{ Closes form on Enter or Escape key }
procedure TfrmSkinRes.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then Close;  // Enter
  if Key = #27 then Close;  // Escape
end;



{-----------------------------------------------------------------------------------------------------------------------
   STYLE LIST POPULATION
-----------------------------------------------------------------------------------------------------------------------}

{ Clicking the label refreshes the style list }
procedure TfrmSkinRes.lblTopClick(Sender: TObject);
begin
  PopulateStyles;
end;


{ Fills the listbox with the styles linked into the executable. Styles loaded from disk at runtime are not listed here.
  TStyleManager.StyleNames already includes 'Windows', so the default theme needs no separate entry. }
procedure TfrmSkinRes.PopulateStyles;
var
  StyleName: string;
begin
  lBox.Clear;

  for StyleName in TStyleManager.StyleNames do
    lBox.Items.Add(StyleName);

  { Select the currently active style in the list }
  lBox.ItemIndex:= lBox.Items.IndexOf(TStyleManager.ActiveStyle.Name);
end;


{ SetStyle recreates every form's window handle, which drops the modal form behind its disabled owner.
  TOPMOST + NOTOPMOST forces Windows to recompute z-order (BUG 1 in Docs\Skins-VCL.md). }
procedure TfrmSkinRes.ReassertZOrder;
begin
  SetWindowPos(Handle, HWND_TOPMOST,    0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
  SetWindowPos(Handle, HWND_NOTOPMOST,  0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
  SetForegroundWindow(Handle);
end;


{ Applies the selected style LIVE. This unit was deliberately NOT converted to FormSkinsDisk's
  save-and-restart model - see Docs\Skins-VCL.md before changing it either way. }
procedure TfrmSkinRes.lBoxClick(Sender: TObject);
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
        CurrentStyleName:= DefWinTheme;
        ReassertZOrder;
        if Assigned(FOnDefaultStyle)
        then FOnDefaultStyle(Self);
      end
    else
    begin
      TStyleManager.SetStyle(CurrentStyleName);
      ReassertZOrder;

    end;
  FINALLY
    lBox.Enabled:= TRUE;
  END;
end;


{ Opens local style editor if available, otherwise opens web-based designer }
procedure TfrmSkinRes.btnSkinEditorClick(Sender: TObject);
begin
  if FileExists(Appdata.AppSysDir + 'StyleDesigner.exe')
  then ExecuteFile(Appdata.AppSysDir + 'StyleDesigner.exe')
  else ExecuteURL(wwwSkinDesinger);
end;


end.
