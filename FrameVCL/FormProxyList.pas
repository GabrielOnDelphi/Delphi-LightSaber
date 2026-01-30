UNIT FormProxyList;

{=============================================================================================================
   2026.01.29
   www.GabrielMoraru.com

--------------------------------------------------------------------------------------------------------------
   PROXY LIST CONFIGURATION FORM

   Allows the user to specify how the program connects to the Internet:
     - Direct connection (no proxy)
     - Via a gateway/proxy server
     - Via a rotating proxy list

   For the proxy list option, a list of proxies can be loaded/saved to/from disk.

   DON'T ADD IT TO ANY DPK!

   USAGE:
     frmProxyList:= TfrmProxyList.Create(Application);
     frmProxyList.LoadProxyFile(AppData.AppDataFolder + 'ProxyList.txt');

   See also: LightVcl.Visual.ProxyList.pas (visual component version)
=============================================================================================================}

INTERFACE
{$DENYPACKAGEUNIT ON}  // Prevents unit from being placed in a package

USES
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms,
  Vcl.StdCtrls, Vcl.ExtCtrls,
  LightCore, LightCore.Time, LightCore.Types, LightCore.INIFile,
  LightVcl.Visual.AppDataForm, LightVcl.Visual.INIFile,
  LightVcl.Common.SystemTime, LightVcl.Common.Clipboard, LightVcl.Common.Dialogs,
  LightVcl.Visual.RadioButton;

TYPE
  { Connection type enumeration }
  TConType = (
    ctDirect,     // Direct internet connection (no proxy)
    ctGateway,    // Single gateway/proxy server
    ctProxyList   // Rotating list of proxy servers
  );

  TfrmProxyList = class(TLightForm)
    grpInternetType : TGroupBox;          // Group for connection type selection
    radGateway      : TCubicRadioButton;  // Use gateway option
    radDirect       : TCubicRadioButton;  // Direct connection option
    radProxyList    : TCubicRadioButton;  // Use proxy list option
    grpProxyList    : TGroupBox;          // Group containing proxy list controls
    pnlBottom       : TPanel;             // Bottom panel for buttons
    btnSaveProxy    : TButton;            // Save proxy list button
    btnLocate       : TButton;            // Open proxy file location button
    btnAutoDetect   : TButton;            // Auto-detect system proxy button
    btnClean        : TButton;            // Clean proxy list button
    mmoProxyList    : TMemo;              // Memo for proxy list entries
    edtGateway      : TEdit;              // Edit for gateway address (host:port)
    procedure btnSaveProxyClick(Sender: TObject);
    procedure ConnectionTypeChanged(Sender: TObject);
    procedure btnLocateClick(Sender: TObject);
    procedure btnAutoDetectClick(Sender: TObject);
    procedure btnCleanClick(Sender: TObject);
  private
    FFileName: string;
  public
    function  GetConnectionType: TConType;
    procedure LoadProxyFile(const AFileName: string);
    procedure Save;
    procedure CleanList;

    property FileName: string read FFileName write FFileName;
  end;

{VAR
    Global form reference - used for singleton access pattern.
    Consider using AppData.GetForm<TfrmProxyList> instead for better encapsulation. 
  frmProxyList: TfrmProxyList;}


IMPLEMENTATION {$R *.dfm}

USES
  LightVcl.Internet.Common,
  LightVcl.Common.IO,
  LightVcl.Common.ExecuteShell;



{--------------------------------------------------------------------------------------------------
   CONNECTION TYPE
--------------------------------------------------------------------------------------------------}

{ Returns the currently selected connection type based on radio button state.
  If no radio button is checked (shouldn't happen), defaults to ctDirect. }
function TfrmProxyList.GetConnectionType: TConType;
begin
  if radDirect.Checked
  then Result:= ctDirect
  else
    if radGateway.Checked
    then Result:= ctGateway
    else
      if radProxyList.Checked
      then Result:= ctProxyList
      else
        begin
          // Fallback: no option selected, default to direct
          radDirect.Checked:= TRUE;
          Result:= ctDirect;
        end;
end;



{--------------------------------------------------------------------------------------------------
   EVENT HANDLERS
--------------------------------------------------------------------------------------------------}

{ Called when the connection type radio buttons change.
  Updates visibility and enabled state of related controls. }
procedure TfrmProxyList.ConnectionTypeChanged(Sender: TObject);
begin
  grpProxyList.Visible:= radProxyList.Checked;
  edtGateway.Enabled:= radGateway.Checked;
  btnAutoDetect.Visible:= radGateway.Checked;
end;


{ Auto-detects system proxy settings from Internet Explorer/Windows configuration }
procedure TfrmProxyList.btnAutoDetectClick(Sender: TObject);
var
  ProxyAddress: string;
  ProxyPort: string;
  UseProxy: Boolean;
begin
  if LightVcl.Internet.Common.IE_GetProxySettings(ProxyAddress, ProxyPort, UseProxy) then
    begin
      radGateway.Checked:= UseProxy;
      radDirect.Checked:= NOT UseProxy;
    end;

  edtGateway.Text:= ProxyAddress + ':' + ProxyPort;
end;



{--------------------------------------------------------------------------------------------------
   LOAD/SAVE PROXY LIST
--------------------------------------------------------------------------------------------------}

{ Loads a proxy list from the specified file.

  Parameters:
    AFileName - Full path to the proxy list text file.

  The file should contain one proxy per line in format: host:port
  Example: 192.168.1.1:8080 }
procedure TfrmProxyList.LoadProxyFile(const AFileName: string);
begin
  FFileName:= AFileName;  // Store filename first

  if FileExists(FFileName)
  then mmoProxyList.Lines.LoadFromFile(FFileName);
end;


{ Saves the current proxy list to the file specified by FileName }
procedure TfrmProxyList.Save;
begin
  Assert(FFileName <> '', 'TfrmProxyList.Save: FileName not set');
  mmoProxyList.Lines.SaveToFile(FFileName);
end;


{ Opens Windows Explorer at the proxy file's location }
procedure TfrmProxyList.btnLocateClick(Sender: TObject);
begin
  if FFileName <> ''
  then ExecuteExplorer(ExtractFilePath(FFileName));
end;


{ Cleans the proxy list and saves it to disk }
procedure TfrmProxyList.btnSaveProxyClick(Sender: TObject);
begin
  CleanList;

  if FFileName <> ''
  then mmoProxyList.Lines.SaveToFile(FFileName);
end;



{--------------------------------------------------------------------------------------------------
   PROXY LIST CLEANUP
--------------------------------------------------------------------------------------------------}

{ Button handler for cleaning the proxy list }
procedure TfrmProxyList.btnCleanClick(Sender: TObject);
begin
  CleanList;
end;


{ Removes empty and whitespace-only lines from the proxy list.

  TODO: Add validation for proper proxy format (e.g., 192.168.0.1:80)
  TODO: Add duplicate removal
  TODO: Add validation that port is numeric and in valid range }
procedure TfrmProxyList.CleanList;
var
  i: Integer;
begin
  // Remove empty lines (iterate backwards to safely delete)
  for i:= mmoProxyList.Lines.Count - 1 downto 0 do
    if Trim(mmoProxyList.Lines[i]) = ''
    then mmoProxyList.Lines.Delete(i);
end;


end.
