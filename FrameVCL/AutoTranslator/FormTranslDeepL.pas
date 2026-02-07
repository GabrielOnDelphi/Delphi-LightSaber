UNIT FormTranslDeepL;

{=============================================================================================================
   2026.01.31
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   DEEPL API SETTINGS FORM

   Configuration dialog for DeepL translation API settings.
   Allows setting API key, choosing Free/Pro endpoint, and testing connection.

   USAGE:
     Call TfrmDeepLSettings.ShowAsModal to display the settings dialog.

   NOTES:
     - API key is stored in the application's INI file
     - Free API: 500,000 chars/month, no cost
     - Pro API: $5.49/month + $25/million chars
=============================================================================================================}

INTERFACE
{$DENYPACKAGEUNIT ON}

USES
  System.SysUtils, System.Classes,
  Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Controls, Vcl.Graphics,
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
    class procedure ShowAsModal; static;
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
  LightVcl.TranslatorAPI;

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

class procedure TfrmDeepLSettings.ShowAsModal;
begin
  AppData.CreateFormModal(TfrmDeepLSettings);
end;


procedure TfrmDeepLSettings.FormPostInitialize;
begin
  inherited FormPostInitialize;
  FResultOK:= FALSE;
  lblStatus.Caption:= '';
  LoadSettings;
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
  DeepL: TDeepLTranslator;
begin
  lblStatus.Caption:= 'Testing connection...';
  lblStatus.Font.Color:= clWindowText;
  Application.ProcessMessages;

  DeepL:= TDeepLTranslator.Create;
  try
    DeepL.ApiKey:= edtApiKey.Text;
    DeepL.UseFreeAPI:= chkUseFreeAPI.Checked;

    if DeepL.TestConnection then
      begin
        lblStatus.Caption:= 'Connection successful!';
        lblStatus.Font.Color:= clGreen;
      end
    else
      begin
        lblStatus.Caption:= 'Failed: ' + DeepL.LastError;
        lblStatus.Font.Color:= clRed;
      end;
  finally
    FreeAndNil(DeepL);
  end;
end;


procedure TfrmDeepLSettings.btnOKClick(Sender: TObject);
var
  sApiKey: string;
begin
  sApiKey:= Trim(edtApiKey.Text);
  if sApiKey = '' then
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
