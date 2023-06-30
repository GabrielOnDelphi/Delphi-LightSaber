UNIT csWMIResolution;

{=============================================================================================================
   SYSTEM - WMI
   2023.01
   See Copyright.txt
==============================================================================================================

 If the program is not DPI aware, Windows will lie about the real screen resolution.
 This unit bypases this problem by reading the resolution (and other info) from the WMI system.
 However, WMI is a service and it could be disabled on some systems.
 Even more, the Screen WMI service is only supported starting with Win Vista AND in newer version of Windows has broken compatibility

 PROBLEM WITH THIS UNIT:
   It returns the real resolution, but it returns resolution per monitor not per desktop

 PEOBLEMS:
   The code does not work on Win8
   WMI service might be disabled!

 Source:
   https://stackoverflow.com/questions/26100182/how-to-obtain-the-real-screen-resolution-in-a-high-dpi-system

 WMI article:
  https://theroadtodelphi.wordpress.com/2011/11/16/wmi-tasks-using-delphi-services/

 Other DPI related questions:
  https://msdn.microsoft.com/en-us/library/dn469266%28v=vs.85%29.aspx
  http://stackoverflow.com/questions/33397511/how-to-work-arround-the-high-dpi-issue
  http://stackoverflow.com/questions/6983837/getting-custom-dpi-percentage-in-delphi
  http://stackoverflow.com/questions/26852150/how-can-i-make-windows-8-1-aware-that-my-delphi-application-wants-to-support-per
  http://stackoverflow.com/questions/5080380/troubleshooting-dpi-virtualization-and-dpi-aware-applications-in-windows-vista
  http://stackoverflow.com/questions/3904619/delphi7-get-attached-monitor-properties <----
  https://stackoverflow.com/questions/26100182/how-to-obtain-the-real-screen-resolution-in-a-high-dpi-system


  How to use it:
    uses ActiveX;
    TRY
      CoInitialize(NIL);
      TRY
        MonitorInfo:= GetMonitorInfoWMI;
        Writeln('Width  ' + IntToStr(MonitorInfo.ScreenWidth));
        Writeln('Height ' + IntToStr(MonitorInfo.ScreenHeight));
        Writeln;
        Readln;
      FINALLY
        CoUninitialize;
      END;
    except_
    END;

   In this group:
     * csShell.pas
     * csSystem.pas
     * csWindow.pas
     * csWindowMetrics.pas
     * csExecuteProc.pas
     * csExecuteShell.pas

 TESTER:
    c:\MyProjects\BIONIX\Projects\          High DPI Tests\cmWMIResolution.pas Tester\

=============================================================================================================}

INTERFACE

USES
  ActiveX,
  System.Win.ComObj,
  Variants;

TYPE
  TMonitorInfo= record
   Dpi      : Integer;
   Caption  : string;
   DeviceID : string;
   Width : Integer;
   Height: Integer;
  end;

function GetMonitorInfoWMI: TMonitorInfo;


IMPLEMENTATION



function VarStrNull(VarStr: OleVariant): string;   // dummy function to handle null variants
begin
  Result := '';
  if NOT VarIsNull(VarStr)
  then Result := VarToStr(VarStr);
end;


function VarIntNull(VarInt: OleVariant): Integer;   // dummy function to handle null variants
begin
  if VarIsNull(VarInt)
  then Result := -1
  else Result := VarInt;
end;


function GetWMIObject(const objectName: String): IDispatch;
VAR
  chEaten: Integer;
  BindCtx: IBindCtx;
  Moniker: IMoniker;
begin
  OleCheck(CreateBindCtx(0, BindCtx));
  OleCheck(MkParseDisplayName(BindCtx, StringToOleStr(objectName), chEaten, Moniker));
  OleCheck(Moniker.BindToObject(BindCtx, nil, IDispatch, Result));
end;


function GetMonitorInfoWMI: TMonitorInfo;
var
  objWMIService: OleVariant;
  colItems: OleVariant;
  colItem: OleVariant;
  oEnum: IEnumvariant;
  iValue: Longword;
begin
  objWMIService := GetWMIObject('winmgmts:\\localhost\root\CIMV2');
  colItems := objWMIService.ExecQuery('SELECT * FROM Win32_DesktopMonitor', 'WQL', 0);  { Hardware that is not compatible with Windows Display Driver Model (WDDM) returns inaccurate property values for instances of this class:       https://msdn.microsoft.com/en-us/library/aa394122%28v=vs.85%29.aspx?f=255&MSPPError=-2147217396  }
  oEnum    := IUnknown(colItems._NewEnum) as IEnumvariant;

  WHILE oEnum.Next(1, colItem, iValue) = 0 DO    { which monitor no is this? }
   begin
    Result.Caption     := VarStrNull(colItem.Caption);
    Result.DeviceID    := VarStrNull(colItem.DeviceID);
    Result.Width       := VarIntNull(colItem.ScreenWidth);
    Result.Height      := VarIntNull(colItem.ScreenHeight);
    {
    uint16   Availability;
    uint16   DisplayType;
    string   Description;
    uint16   StatusInfo;
    boolean  PowerManagementSupported;
    uint32   PixelsPerXLogicalInch;
    uint32   PixelsPerYLogicalInch;
    string   MonitorManufacturer;
    string   Status;

    string   SystemName;
    uint32   Bandwidth;
    uint32   ConfigManagerErrorCode;
    boolean  ConfigManagerUserConfig;
    string   CreationClassName;
    boolean  ErrorCleared;
    string   ErrorDescription;
    datetime InstallDate;
    boolean  IsLocked;
    uint32   LastErrorCode;
    string   MonitorType;
    string   Name;
    string   PNPDeviceID;
    uint16   PowerManagementCapabilities[];
    string   SystemCreationClassName; }

    {todo 2: i also need:
    Result.left
    result.top }
   end;
end;


end.
