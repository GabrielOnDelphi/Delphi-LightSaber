UNIT FormProxy;
{--------------------------------------------------------------------------------------------------
  CubicDesign
  2019.07

  Let user specify if he wants to connect the program directly to Internet, via a gateway or via a proxy list.
  For the third option, a proxy list can be loaded/saved to/from disk

  DON'T ADD IT TO ANY DPK!

  How to use it:
    Initialize like this: frameProxy.LoadProxyFile(AppData.AppDataFolder+ 'ProxyList.txt');
--------------------------------------------------------------------------------------------------}

INTERFACE
{$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, ccINIFile, cvIniFile, ccCore, ccINIFile,
  cvRadioButton;

TYPE
  TConType= (ctDirect, ctGateway, ctProxyList);

  TfrmProxyList = class(TForm)
    grpInternetType   : TGroupBox;
    radGateway        : TCubicRadioButton;
    radDirect         : TCubicRadioButton;
    radProxyList      : TCubicRadioButton;
    grpProxyList      : TGroupBox;
    pnlBottom         : TPanel;
    btnSaveProxy      : TButton;
    btnLocate         : TButton;
    btnAutoDetect     : TButton;
    btnClean          : TButton;
    procedure btnSaveProxyClick    (Sender: TObject);
    procedure ConectionTypeChanged (Sender: TObject);
    procedure btnLocateClick       (Sender: TObject);
    procedure btnAutoDetectClick   (Sender: TObject);
    procedure btnCleanClick        (Sender: TObject);
  private
  protected
  public
    FileName: string;
    mmoProxyList : TMemo;
    edtGateway   : TEdit;
    function  GetConnectionType: TConType;
    procedure LoadProxyFile(CONST aFileName: string);
    procedure Save;                                   { Save content to disk }
    procedure CleanList;
  end;

VAR frmProxyList: TfrmProxyList;


IMPLEMENTATION
{$R *.dfm}
{$WARN GARBAGE OFF}                                                                                {Silence the: 'W1011 Text after final END' warning }

USES
   ciInternet, IniFiles;

{todo:
oncreate
loadForm;

ondestroy
saveForm;}

function TfrmProxyList.GetConnectionType: TConType;
begin
 if radDirect.Checked                                                                              { DIRECT CONNECTION }
 then Result:= ctDirect
 else

 if radGateway.Checked
 then Result:= ctGateway
 else

 if radProxyList.Checked                                                                           { PROXY LIST }
 then Result:= ctProxyList
 else
   begin
    radDirect.Checked:= TRUE;
    Result:= ctDirect;
   end;
end;





{--------------------------------------------------------------------------------------------------
   EVENTS
--------------------------------------------------------------------------------------------------}
procedure TfrmProxyList.ConectionTypeChanged(Sender: TObject);
begin
 grpProxyList.Visible := radProxyList.Checked;
 edtGateway.Enabled   := radGateway.Checked;
 btnAutoDetect.Visible:= radGateway.Checked;
end;


procedure TfrmProxyList.btnAutoDetectClick(Sender: TObject);
VAR sProxyAdr, sProxyPort: string;
    UseProxy: Boolean;
begin
 if ciInternet.IE_GetProxySettings(sProxyAdr, sProxyPort, UseProxy) then
  begin
   radGateway.Checked := UseProxy;
   radDirect .Checked := NOT UseProxy;
  end;
 edtGateway.Text:= sProxyAdr+ ':'+ sProxyPort;
end;






{--------------------------------------------------------------------------------------------------
   LOAD/SAVE PROXY LIST
--------------------------------------------------------------------------------------------------}
procedure TfrmProxyList.LoadProxyFile(CONST aFileName: string);
begin
 if FileExists(FileName) then
  begin
   mmoProxyList.lines.LoadFromFile(FileName);
   FileName:= aFileName;
  end;
end;


procedure TfrmProxyList.Save;              { Save content to disk }
begin
 mmoProxyList.Lines.SaveToFile(FileName);
end;



procedure TfrmProxyList.btnLocateClick(Sender: TObject);
begin
 ExecuteExplorer(ExtractFilePath(FileName))
end;


procedure TfrmProxyList.btnSaveProxyClick(Sender: TObject);
begin
 CleanList;
 mmoProxyList.Lines.SaveToFile(FileName);
end;





{--------------------------------------------------------------------------------------------------
   CLEAN
--------------------------------------------------------------------------------------------------}
procedure TfrmProxyList.btnCleanClick(Sender: TObject);
begin
 CleanList;
end;


procedure TfrmProxyList.CleanList;                                                                    {TODO: Willy: Imi trebuie o functie care verifica daca un string e un valid proxy. de exemplu 192.168.0.1:80 }
VAR i: Integer;
begin
 {TODO: check for invalid proxy lines }
 for i:= mmoProxyList.Lines.Count-1 downto 0 DO
   if (mmoProxyList.Lines[i]= '')
   OR (mmoProxyList.Lines[i]= ' ')
   then mmoProxyList.Lines.Delete(i);
end;








{--------------------------------------------------------------------------------------------------
   LOAD/SAVE SETTINGS
--------------------------------------------------------------------------------------------------}
{
procedure TfrmProxyList.LoadSettings(CONST IniFile: TCubicIniFile);
begin
 radDirect.Checked    := IniFile.ReadBool   ('PROXY', 'radDirect'   , TRUE);
 radGateway.Checked   := IniFile.ReadBool   ('PROXY', 'radGateway'  , FALSE);
 radProxyList.Checked := IniFile.ReadBool   ('PROXY', 'radProxyList', FALSE);
 edtGateway.Text      := IniFile.ReadString ('PROXY', 'edtGateway'  , '');
end;


procedure TfrmProxyList.SaveSettings(CONST IniFile: TCubicIniFile);
begin
 IniFile.WriteBool   ('PROXY', 'radDirect'   , radDirect.Checked);
 IniFile.WriteBool   ('PROXY', 'radGateway'  , radGateway.Checked);
 IniFile.WriteBool   ('PROXY', 'radProxyList', radProxyList.Checked);
 IniFile.WriteString ('PROXY', 'edtGateway'  , edtGateway.Text);
end;
}



end.{===================================================================================================================
