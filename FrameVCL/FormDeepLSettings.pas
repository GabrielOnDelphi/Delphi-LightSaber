UNIT FormDeepLSettings;

{=============================================================================================================
   2026.01.31
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   DEEPL API SETTINGS FORM

   Configuration dialog for DeepL translation API settings.
   Allows setting API key, choosing Free/Pro endpoint, and testing connection.

   USAGE:
     Call TfrmDeepLSettings.ShowSettings to display the settings dialog.
     Returns True if user clicked OK and settings were saved.

   NOTES:
     - API key is stored in the application's INI file
     - Free API: 500,000 chars/month, no cost
     - Pro API: $5.49/month + $25/million chars
=============================================================================================================}

INTERFACE
{$DENYPACKAGEUNIT ON}

USES
  System.SysUtils, System.Classes,
  Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Controls,
  LightVcl.Visual.AppDataForm;

TYPE
  TfrmDeepLSettings = class(TLightForm)
    grpSettings: TGroupBox;
    lblApiKey: TLabel;
    edtApiKey: TEdit;
    chkUseFreeAPI: TCheckBox;
    btnTest: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    lblStatus: TLabel;
    lblInfo: TLabel;
    pnlButtons: TPanel;
    procedure btnTestClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FResultOK: Boolean;
    procedure LoadSettings;
    procedure SaveSettings;
  public
    class function ShowSettings: Boolean; static;
    procedure FormPostInitialize; override;
  end;


{ Global functions to access settings }
function DeepL_GetApiKey: string;
function DeepL_GetUseFreeAPI: Boolean;
procedure DeepL_SetApiKey(const Value: string);
procedure DeepL_SetUseFreeAPI(Value: Boolean);


IMPLEMENTATION {$R *.dfm}

USES
  LightCore.AppData, LightVcl.Visual.AppData,
  LightVcl.Common.IniFile, LightVcl.Common.Dialogs,
  LightVcl.Common.TranslatorAPI;

CONST
  INI_SECTION = 'DeepL';
  INI_KEY_APIKEY = 'ApiKey';
  INI_KEY_USEFREE = 'UseFreeAPI';



{-------------------------------------------------------------------------------------------------------------
   GLOBAL SETTINGS ACCESS
-------------------------------------------------------------------------------------------------------------}

function DeepL_GetApiKey: string;
begin
  VAR IniFile:= TIniFileApp.Create(AppData.AppName, AppData.IniFile);
  try
    Result:= IniFile.ReadString(INI_SECTION, INI_KEY_APIKEY, '');
  finally
    FreeAndNil(IniFile);
  end;
end;


function DeepL_GetUseFreeAPI: Boolean;
begin
  VAR IniFile:= TIniFileApp.Create(AppData.AppName, AppData.IniFile);
  try
    Result:= IniFile.ReadBool(INI_SECTION, INI_KEY_USEFREE, TRUE);
  finally
    FreeAndNil(IniFile);
  end;
end;


procedure DeepL_SetApiKey(const Value: string);
begin
  VAR IniFile:= TIniFileApp.Create(AppData.AppName, AppData.IniFile);
  try
    IniFile.WriteString(INI_SECTION, INI_KEY_APIKEY, Value);
  finally
    FreeAndNil(IniFile);
  end;
end;


procedure DeepL_SetUseFreeAPI(Value: Boolean);
begin
  VAR IniFile:= TIniFileApp.Create(AppData.AppName, AppData.IniFile);
  try
    IniFile.WriteBool(INI_SECTION, INI_KEY_USEFREE, Value);
  finally
    FreeAndNil(IniFile);
  end;
end;



{-------------------------------------------------------------------------------------------------------------
   FORM
-------------------------------------------------------------------------------------------------------------}

class function TfrmDeepLSettings.ShowSettings: Boolean;
var
  frmSettings: TfrmDeepLSettings;
begin
  AppData.CreateFormHidden(TfrmDeepLSettings, frmSettings);
  try
    frmSettings.LoadSettings;
    frmSettings.ShowModal;
    Result:= frmSettings.FResultOK;
  finally
    FreeAndNil(frmSettings);
  end;
end;


procedure TfrmDeepLSettings.FormPostInitialize;
begin
  inherited FormPostInitialize;
  FResultOK:= FALSE;
  lblStatus.Caption:= '';
end;


procedure TfrmDeepLSettings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= TCloseAction.caFree;
end;


procedure TfrmDeepLSettings.LoadSettings;
begin
  edtApiKey.Text:= DeepL_GetApiKey;
  chkUseFreeAPI.Checked:= DeepL_GetUseFreeAPI;
end;


procedure TfrmDeepLSettings.SaveSettings;
begin
  DeepL_SetApiKey(edtApiKey.Text);
  DeepL_SetUseFreeAPI(chkUseFreeAPI.Checked);
end;



{-------------------------------------------------------------------------------------------------------------
   BUTTONS
-------------------------------------------------------------------------------------------------------------}

procedure TfrmDeepLSettings.btnTestClick(Sender: TObject);
var
  Translator: TDeepLTranslator;
begin
  lblStatus.Caption:= 'Testing connection...';
  lblStatus.Font.Color:= clWindowText;
  Application.ProcessMessages;

  Translator:= TDeepLTranslator.Create;
  try
    Translator.ApiKey:= edtApiKey.Text;
    Translator.UseFreeAPI:= chkUseFreeAPI.Checked;

    if Translator.TestConnection then
      begin
        lblStatus.Caption:= 'Connection successful!';
        lblStatus.Font.Color:= clGreen;
      end
    else
      begin
        lblStatus.Caption:= 'Failed: ' + Translator.LastError;
        lblStatus.Font.Color:= clRed;
      end;
  finally
    FreeAndNil(Translator);
  end;
end;


procedure TfrmDeepLSettings.btnOKClick(Sender: TObject);
begin
  if edtApiKey.Text.Trim.IsEmpty then
    begin
      MessageWarning('Please enter an API key.');
      EXIT;
    end;

  SaveSettings;
  FResultOK:= TRUE;
  Close;
end;


procedure TfrmDeepLSettings.btnCancelClick(Sender: TObject);
begin
  FResultOK:= FALSE;
  Close;
end;


end.
