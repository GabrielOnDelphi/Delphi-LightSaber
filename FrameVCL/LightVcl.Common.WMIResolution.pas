UNIT LightVcl.Common.WMIResolution;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com
==============================================================================================================

 If the program is not DPI aware, Windows will lie about the real screen resolution.
 This unit bypasses this problem by reading the resolution (and other info) from the WMI system.
 However, WMI is a service and it could be disabled on some systems.
 Even more, the Screen WMI service is only supported starting with Win Vista AND in newer version of Windows has broken compatibility

 PROBLEM WITH THIS UNIT:
   It returns the real resolution, but it returns resolution per monitor not per desktop

 PROBLEMS:
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
    CoInitialize(NIL);
    TRY
      MonitorInfo:= GetMonitorInfoWMI;
      Writeln('Width  ' + IntToStr(MonitorInfo.Width));
      Writeln('Height ' + IntToStr(MonitorInfo.Height));
      Writeln;
      Readln;
    FINALLY
      CoUninitialize;
    END;

   In this group:
     * LightVcl.Common.Shell.pas
     * csSystem.pas
     * csWindow.pas
     * LightVcl.Common.WindowMetrics.pas
     * LightVcl.Common.ExecuteProc.pas
     * LightVcl.Common.ExecuteShell.pas

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



{ Safely converts OleVariant to string. Returns empty string if variant is null. }
function VarStrNull(VarStr: OleVariant): string;
begin
  Result:= '';
  if NOT VarIsNull(VarStr)
  then Result:= VarToStr(VarStr);
end;


{ Safely converts OleVariant to Integer. Returns 0 if variant is null (suitable for dimensions). }
function VarIntNull(VarInt: OleVariant): Integer;
begin
  if VarIsNull(VarInt)
  then Result:= 0
  else Result:= VarInt;
end;


{ Creates a WMI object via moniker binding.
  Raises EOleSysError if WMI service is not available or objectName is invalid. }
function GetWMIObject(CONST objectName: String): IDispatch;
VAR
  chEaten: Integer;
  BindCtx: IBindCtx;
  Moniker: IMoniker;
begin
  OleCheck(CreateBindCtx(0, BindCtx));
  { PWideChar cast avoids memory leak from StringToOleStr which allocates BSTR }
  OleCheck(MkParseDisplayName(BindCtx, PWideChar(objectName), chEaten, Moniker));
  OleCheck(Moniker.BindToObject(BindCtx, nil, IDispatch, Result));
end;


{ Queries WMI for monitor information.
  Returns the LAST monitor found if multiple monitors exist (see PROBLEM WITH THIS UNIT in header).
  Returns zeroed record if WMI query returns no monitors or WMI service is unavailable.
  Caller MUST call CoInitialize before and CoUninitialize after calling this function.

  Note: Hardware not compatible with WDDM returns inaccurate values.
  See: https://msdn.microsoft.com/en-us/library/aa394122%28v=vs.85%29.aspx }
function GetMonitorInfoWMI: TMonitorInfo;
VAR
  objWMIService: OleVariant;
  colItems: OleVariant;
  colItem: OleVariant;
  oEnum: IEnumvariant;
  iValue: Longword;
begin
  { Initialize result to avoid undefined return value }
  Result.Dpi     := 0;
  Result.Caption := '';
  Result.DeviceID:= '';
  Result.Width   := 0;
  Result.Height  := 0;

  objWMIService:= GetWMIObject('winmgmts:\\localhost\root\CIMV2');
  colItems:= objWMIService.ExecQuery('SELECT * FROM Win32_DesktopMonitor', 'WQL', 0);
  oEnum:= IUnknown(colItems._NewEnum) as IEnumvariant;

  { Iterate monitors - returns info for the last monitor found }
  WHILE oEnum.Next(1, colItem, iValue) = 0 DO
   begin
    Result.Caption := VarStrNull(colItem.Caption);
    Result.DeviceID:= VarStrNull(colItem.DeviceID);
    Result.Width   := VarIntNull(colItem.ScreenWidth);
    Result.Height  := VarIntNull(colItem.ScreenHeight);
    Result.Dpi     := VarIntNull(colItem.PixelsPerXLogicalInch);
    {
    Available WMI properties (Win32_DesktopMonitor):
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
