UNIT cvProxyList;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------

  Features:

  How to use it:
    Initialize like this: frameProxy.LoadProxyFile(AppData.AppDataFolder+ 'ProxyList.txt');

  This is also available as form: FormProxy.pas
=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, ccINIFile, cbAppDataForm, cvRadioButton, ccCore;

TYPE
  TConType= (ctDirect, ctGateway, ctProxyList);

  TProxyList = class(TGroupBox)
   private
    Initialized       : Boolean;
    grpInternetType   : TGroupBox;
    radGateway        : TCubicRadioButton;
    radDirect         : TCubicRadioButton;
    radProxyList      : TCubicRadioButton;
    grpProxyList      : TGroupBox;
    pnlBottom         : TPanel;
    btnSaveProxy      : TButton;
    btnLocate         : TButton;
    btnAutoDetect     : TButton;
    FTypeChanged      : TNotifyEvent;
    btnClean          : TButton;
    function  GetConnectionType: TConType;
    procedure btnSaveProxyClick(Sender: TObject);
    procedure ConectionTypeChanged(Sender: TObject);
    procedure btnLocateClick(Sender: TObject);
    procedure btnAutoDetectClick(Sender: TObject);
    procedure btnCleanClick(Sender: TObject);
  protected
    procedure CreateWnd; override;
  public
    FileName: string;
    mmoProxyList : TMemo;
    edtGateway   : TEdit;

    procedure LoadProxyFile(CONST aFileName: string);
    procedure Save;                                    { Save content to disk }

    procedure LoadSettings (CONST IniFile: TIniFileEx);
    procedure SaveSettings (CONST IniFile: TIniFileEx);

    procedure CleanList;
    constructor Create(aOwner: TComponent); override;
  published
    property ConnectionType: TConType read GetConnectionType;
    property OnTypeChanged: TNotifyEvent read FTypeChanged write FTypeChanged;
  end;

procedure Register;


IMPLEMENTATION
{$WARN GARBAGE OFF}                                                                                {Silence the: 'W1011 Text after final END' warning }

USES
   ciInternet, IniFiles, csExecuteShell;



constructor TProxyList.Create(AOwner: TComponent);
begin
 inherited Create(aOwner);
 // Note: Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create

 grpInternetType:= TGroupBox.Create(Self);
 grpInternetType.Parent:= Self;   // Here I can set the parent

    radGateway:= TCubicRadioButton.Create(Self);
    radGateway.Parent:= grpInternetType;

    radDirect:= TCubicRadioButton.Create(Self);
    radDirect.Parent:= grpInternetType;

    radProxyList:= TCubicRadioButton.Create(Self);
    radProxyList.Parent:= grpInternetType;

    edtGateway:= TEdit.Create(Self);
    edtGateway.Parent:= grpInternetType;

    btnAutoDetect:= TButton.Create(Self);
    btnAutoDetect.Parent:= grpInternetType;


 grpProxyList:= TGroupBox.Create(Self);
 grpProxyList.Parent:= Self;

    mmoProxyList:= TMemo.Create(Self);
    mmoProxyList.Parent:= grpProxyList;

    pnlBottom:= TPanel.Create(Self);
    pnlBottom.Parent:= grpProxyList;

      btnSaveProxy:= TButton.Create(Self);
      btnSaveProxy.Parent:= pnlBottom;

      btnLocate:= TButton.Create(Self);
      btnLocate.Parent:= pnlBottom;

      btnClean:= TButton.Create(Self);
      btnClean.Parent:= pnlBottom;
end;




//CreateWnd can be called more than once:  http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Controls_TWinControl_CreateWnd.html
procedure TProxyList.CreateWnd;
begin
 inherited CreateWnd;

 if NOT Initialized then { Make sure we don't call this code twice }
  begin
   Initialized:= TRUE;
   Width  := 521;
   Height := 503;
   DoubleBuffered := TRUE;
   Caption:= 'Proxy settings';                                      //  https://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create

   //grpInternetType.SetSubComponent(True);
   grpInternetType.Caption       := 'Internet access';
   grpInternetType.Left          := 13;
   grpInternetType.Top           := 128;
   grpInternetType.Width         := 204;
   grpInternetType.Height        := 190;
   grpInternetType.DoubleBuffered:= TRUE;

     radGateway.SetBounds(18, 86, 150, 17);
     radGateway.Caption         := 'Gateway';
     radGateway.Hint            := 'Gateway';
     radGateway.ShowHint        := TRUE;
     radGateway.OnClick         := ConectionTypeChanged;

     radDirect.Caption          := 'Direct connection';
     radDirect.SetBounds(18, 56, 150, 17);
     radDirect.Hint             := 'Direct Internet connection';
     radDirect.Font.Charset     := DEFAULT_CHARSET;
     radDirect.ShowHint         := TRUE;
     radDirect.TabOrder         := 1;
     radDirect.OnClick          := ConectionTypeChanged;

     radProxyList.Caption       := 'Use list of proxies';
     radProxyList.SetBounds(18, 115, 150, 17);
     radProxyList.Hint          := 'Use the provided list of proxies.';
     radProxyList.ShowHint      := TRUE;
     radProxyList.TabOrder      := 2;
     radProxyList.OnClick       := ConectionTypeChanged;

     edtGateway.SetBounds(87, 85, 150, 21);
     edtGateway.TabOrder        := 3;
     edtGateway.Text            := '192.168.0.1:80';

     btnAutoDetect.Caption      := 'Auto detect';
     btnAutoDetect.Left         := 60;
     btnAutoDetect.Top          := 154;
     btnAutoDetect.Width        := 79;
     btnAutoDetect.Height       := 28;
     btnAutoDetect.ShowHint     := TRUE;
     btnAutoDetect.Visible      := False;
     btnAutoDetect.OnClick      := btnAutoDetectClick;


   grpProxyList.AlignwithMargins:= True;
   grpProxyList.Left            := 229;
   grpProxyList.Width           := 289;
   grpProxyList.Align           := alRight;
   grpProxyList.Anchors         := [akLeft, akTop, akRight, akBottom];
   grpProxyList.Caption         := 'Proxy list';
   grpProxyList.DoubleBuffered  := TRUE;
   grpProxyList.Visible         := FALSE;

     mmoProxyList.Align         := alClient;
     mmoProxyList.Anchors       := [akLeft, akTop, akRight, akBottom];
     mmoProxyList.Top           := 24;
     mmoProxyList.Hint          := 'The format accepted by this proxy list is: xxx.xxx.xxx.xxx:port'+CRLFw+ 'Example: 66.122.234.1:80';
     mmoProxyList.ShowHint      := TRUE;
     mmoProxyList.Color         := 14730932;
     mmoProxyList.DoubleBuffered:= True;
     mmoProxyList.ScrollBars    := ssVertical;
     mmoProxyList.WordWrap      := False;

     pnlBottom.Left      := 2;
     pnlBottom.Top       := 459;
     pnlBottom.Width     := 285;
     pnlBottom.Height    := 36;
     pnlBottom.Align     := alBottom;
     pnlBottom.BevelInner:= bvLowered;

       btnSaveProxy.Left    := 11;
       btnSaveProxy.Top     := 6;
       btnSaveProxy.Width   := 75;
       btnSaveProxy.Height  := 25;
       btnSaveProxy.Hint    := 'Clean current proxy list and save it to disk.'#13#10'The list will be automatically loaded next time the program starts.';
       btnSaveProxy.Anchors := [akLeft];
       btnSaveProxy.Caption := 'Save';
       btnSaveProxy.ShowHint:= True;
       btnSaveProxy.OnClick := btnSaveProxyClick;

       btnLocate.Anchors    := [akRight];
       btnLocate.SetBounds(198, 6, 80, 25);
       btnLocate.Hint       := 'Open the folder that contains the proxy list';
       btnLocate.ShowHint   := TRUE;
       btnLocate.Caption    := 'Open folder...';
       btnLocate.OnClick    := btnLocateClick;

       btnClean.Left        := 118;
       btnClean.Top         := 6;
       btnClean.Width       := 75;
       btnClean.Height      := 25;
       btnClean.Anchors     := [akRight];
       btnClean.Caption     := 'Clean';
       btnClean.Hint        := 'Delete empty lines';
       btnClean.ShowHint    := True;
       btnClean.OnClick     := btnCleanClick;
  end;
end;















function TProxyList.GetConnectionType: TConType;
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
procedure TProxyList.ConectionTypeChanged(Sender: TObject);
begin
 grpProxyList.Visible := radProxyList.Checked;
 edtGateway.Enabled   := radGateway.Checked;
 btnAutoDetect.Visible:= radGateway.Checked;

 if Assigned(FTypeChanged) then FTypeChanged(Self);
end;


procedure TProxyList.btnAutoDetectClick(Sender: TObject);
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
procedure TProxyList.LoadProxyFile(CONST aFileName: string);
begin
 if FileExists(FileName) then
  begin
   mmoProxyList.lines.LoadFromFile(FileName);
   FileName:= aFileName;
  end;
end;


procedure TProxyList.Save;              { Save content to disk }
begin
 mmoProxyList.Lines.SaveToFile(FileName);
end;



procedure TProxyList.btnLocateClick(Sender: TObject);
begin
 ExecuteExplorer(ExtractFilePath(FileName))
end;


procedure TProxyList.btnSaveProxyClick(Sender: TObject);
begin
 CleanList;
 mmoProxyList.Lines.SaveToFile(FileName);
end;





{--------------------------------------------------------------------------------------------------
   CLEAN
--------------------------------------------------------------------------------------------------}
procedure TProxyList.btnCleanClick(Sender: TObject);
begin
 CleanList;
end;


procedure TProxyList.CleanList;                                                                    {TODO: Willy: Imi trebuie o functie care verifica daca un string e un valid proxy. de exemplu 192.168.0.1:80 }
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
procedure TProxyList.LoadSettings(CONST IniFile: TIniFileEx);
begin
 radDirect.Checked    := IniFile.ReadBool   ('PROXY', 'radDirect'   , TRUE);
 radGateway.Checked   := IniFile.ReadBool   ('PROXY', 'radGateway'  , FALSE);
 radProxyList.Checked := IniFile.ReadBool   ('PROXY', 'radProxyList', FALSE);
 edtGateway.Text      := IniFile.ReadString ('PROXY', 'edtGateway'  , '');
end;


procedure TProxyList.SaveSettings(CONST IniFile: TIniFileEx);
begin
 IniFile.WriteBool   ('PROXY', 'radDirect'   , radDirect.Checked);
 IniFile.WriteBool   ('PROXY', 'radGateway'  , radGateway.Checked);
 IniFile.WriteBool   ('PROXY', 'radProxyList', radProxyList.Checked);
 IniFile.WriteString ('PROXY', 'edtGateway'  , edtGateway.Text);
end;


procedure Register;
begin
  RegisterComponents('LightSaber', [TProxyList]);
end;


end.
