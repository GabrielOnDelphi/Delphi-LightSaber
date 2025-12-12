UNIT MonitorHelper;
(*-------------------------------------------------------------------------------------------------------------
  MonitorHelper
  [2018]
  https://github.com/r1me/delphi-monitorhelper

  DisplayDevice can return:
     displayDevice.DeviceID     =>    'MONITOR\Default_Monitor\{4d36e96e-e325-11ce-bfc1-08002be10318}\0004'
     displayDevice.DeviceString =>    'Generic Non-PnP Monitor'
     displayDevice.DeviceKey    =>    '\Registry\Machine\System\CurrentControlSet\Control\Class\{4d36e96e-e325-11ce-bfc1-08002be10318}\0004'
     displayDevice.DeviceName   =>    '\\.\DISPLAY1\Monitor0'

  Does not support USB monitors:
     https://github.com/r1me/delphi-monitorhelper/issues/3

     Possible fix:
        https://unix.stackexchange.com/questions/74367/watch-usb-connections-vendor-id-product-id-and-revision
        https://stackoverflow.com/questions/24944865/monitor-usb-drives-and-retrieve-device-info-using-a-devicewatcher


  Tester:
     Monitor ID\Tester for MonitorHelper.pas\MonitorOrientationDemo.dpr
------------------------------------------------------------------------------------------------------------*)
INTERFACE

USES
  Winapi.Windows, System.SysUtils, system.StrUtils, Vcl.Forms;

CONST
  DM_DISPLAYQUERYORIENTATION = $01000000;
  DMDO_DEFAULT = 0;
  DMDO_90 = 1;
  DMDO_180 = 2;
  DMDO_270 = 3;
  ENUM_CURRENT_SETTINGS = DWORD(-1);

TYPE
  _devicemode = record
    dmDeviceName: array [0..CCHDEVICENAME-1] of {$IFDEF UNICODE}WideChar{$ELSE}AnsiChar{$ENDIF};
    dmSpecVersion: WORD;
    dmDriverVersion: WORD;
    dmSize: WORD;
    dmDriverExtra: WORD;
    dmFields: DWORD;
    union1: record
     case Integer of
       0: (
         dmOrientation: SmallInt;
         dmPaperSize: SmallInt;
         dmPaperLength: SmallInt;
         dmPaperWidth: SmallInt;
         dmScale: SmallInt;
         dmCopies: SmallInt;
         dmDefaultSource: SmallInt;
         dmPrintQuality: SmallInt);
       1: (
         dmPosition: TPointL;
         dmDisplayOrientation: DWORD;
         dmDisplayFixedOutput: DWORD);
     end;
    dmColor           : ShortInt;
    dmDuplex          : ShortInt;
    dmYResolution     : ShortInt;
    dmTTOption        : ShortInt;
    dmCollate         : ShortInt;
    dmFormName        : array [0..CCHFORMNAME-1] of {$IFDEF UNICODE}WideChar{$ELSE}AnsiChar{$ENDIF};
    dmLogPixels       : WORD;
    dmBitsPerPel      : DWORD;
    dmPelsWidth       : DWORD;
    dmPelsHeight      : DWORD;
    dmDiusplayFlags   : DWORD;
    dmDisplayFrequency: DWORD;
    dmICMMethod       : DWORD;
    dmICMIntent       : DWORD;
    dmMediaType       : DWORD;
    dmDitherType      : DWORD;
    dmReserved1       : DWORD;
    dmReserved2       : DWORD;
    dmPanningWidth    : DWORD;
    dmPanningHeight   : DWORD;
  end;

  devicemode  = _devicemode;
  Pdevicemode = ^devicemode;

  TMonitorOrientation = (
    moLandscape,
    moPortrait,
    moLandscapeFlipped,
    moPortraitFlipped);

  TMonitorHelper = class helper for TMonitor
  private
    function  GetFriendlyName: String;
    function  GetSupportsRotation: Boolean;
    function  GetOrientation: TMonitorOrientation;
    procedure SetOrientation(ANewOrientation: TMonitorOrientation);
    function  GetDeviceID: string;
    function  GetDeviceString: string;
  public
    property FriendlyName: String read GetFriendlyName;
    property DeviceID:     String read GetDeviceID;
    property DeviceString: String read GetDeviceString;

    property Orientation: TMonitorOrientation read GetOrientation write SetOrientation;
    property SupportsRotation: Boolean read GetSupportsRotation;
  end;


function MonitorOrientationToString(AMonitorOrientation: TMonitorOrientation): string;
function ExtractDeviceID(ID: String): string;

function GenerateScreenReport: string;



IMPLEMENTATION



function TMonitorHelper.GetFriendlyName: String;
var
  displayDevice: TDisplayDevice;
  devName: String;
begin
  // todo: get friendly name from EDID
  Result := '';
  ZeroMemory(@displayDevice, SizeOf(displayDevice));
  displayDevice.cb := SizeOf(displayDevice);

  if EnumDisplayDevices(nil, Self.MonitorNum, displayDevice, 0) then
   begin
    devName := displayDevice.DeviceName;
    EnumDisplayDevices(PChar(devName), 0, displayDevice, 0);
    Result := displayDevice.DeviceName;
   end;
end;


function TMonitorHelper.GetDeviceID: String;
var
  displayDevice: TDisplayDevice;
  devName: String;
begin
  Result := '';
  ZeroMemory(@displayDevice, SizeOf(displayDevice));
  displayDevice.cb := SizeOf(displayDevice);

  if EnumDisplayDevices(nil, Self.MonitorNum, displayDevice, 0) then
   begin
    devName := displayDevice.DeviceName;
    EnumDisplayDevices(PChar(devName), 0, displayDevice, 0);
    Result := displayDevice.DeviceID;
   end;
end;


function TMonitorHelper.GetDeviceString: String;
VAR
  displayDevice: TDisplayDevice;
  devName: String;
begin
  Result := '';
  ZeroMemory(@displayDevice, SizeOf(displayDevice));
  displayDevice.cb := SizeOf(displayDevice);

  if EnumDisplayDevices(nil, Self.MonitorNum, displayDevice, 0) then
   begin
    devName := displayDevice.DeviceName;
    EnumDisplayDevices(PChar(devName), 0, displayDevice, 0);
    Result := displayDevice.DeviceString;
   end;
end;









function TMonitorHelper.GetSupportsRotation: Boolean;
var
  devMode: TDevMode;
  displayDevice: TDisplayDevice;
begin
  Result := False;

  ZeroMemory(@displayDevice, SizeOf(displayDevice));
  displayDevice.cb := SizeOf(displayDevice);

  if EnumDisplayDevices(nil, Self.MonitorNum, displayDevice, 0) then
  begin
    ZeroMemory(@devMode, SizeOf(devMode));
    devMode.dmSize := SizeOf(devMode);
    devMode.dmFields := DM_DISPLAYQUERYORIENTATION;

    Result := (ChangeDisplaySettingsEx(@displayDevice.DeviceName, devMode, 0, CDS_TEST, nil) = DISP_CHANGE_SUCCESSFUL);
  end;
end;


function TMonitorHelper.GetOrientation: TMonitorOrientation;
var
  devMode: TDeviceMode;
  displayDevice: TDisplayDevice;
begin
  Result := moLandscape;

  ZeroMemory(@displayDevice, SizeOf(displayDevice));
  displayDevice.cb := SizeOf(displayDevice);

  if EnumDisplayDevices(nil, Self.MonitorNum, displayDevice, 0) then
  begin
    ZeroMemory(@devMode, SizeOf(devMode));
    devMode.dmSize := SizeOf(devMode);

    if EnumDisplaySettings(@displayDevice.DeviceName, ENUM_CURRENT_SETTINGS, devMode) then
    begin
      case Pdevicemode(@devMode)^.union1.dmDisplayOrientation of
        DMDO_DEFAULT: Result := moLandscape;
        DMDO_90:      Result := moPortrait;
        DMDO_180:     Result := moLandscapeFlipped;
        DMDO_270:     Result := moPortraitFlipped;
      else
        Result := moLandscape;
      end;
    end;
  end;
end;


procedure TMonitorHelper.SetOrientation(ANewOrientation: TMonitorOrientation);
var
  devMode: TDevMode;
  displayDevice: TDisplayDevice;
  dwTemp: DWORD;
begin
  ZeroMemory(@displayDevice, SizeOf(displayDevice));
  displayDevice.cb := SizeOf(displayDevice);

  if EnumDisplayDevices(nil, Self.MonitorNum, displayDevice, 0) then
  begin
    ZeroMemory(@devMode, SizeOf(devMode));
    devMode.dmSize := SizeOf(devMode);

    if EnumDisplaySettings(@displayDevice.DeviceName, ENUM_CURRENT_SETTINGS, devMode) then
    begin
      if Odd(Pdevicemode(@devMode)^.union1.dmDisplayOrientation) <> Odd(Ord(ANewOrientation)) then
       begin
        dwTemp := devMode.dmPelsHeight;
        devMode.dmPelsHeight:= devMode.dmPelsWidth;
        devMode.dmPelsWidth := dwTemp;
       end;

      if Pdevicemode(@devMode)^.union1.dmDisplayOrientation <> DWORD(Ord(ANewOrientation)) then
       begin
        Pdevicemode(@devMode)^.union1.dmDisplayOrientation := DWORD(Ord(ANewOrientation));
        ChangeDisplaySettingsEx(@displayDevice.DeviceName, devMode, 0, 0, nil);
       end;
    end;
  end;
end;









{-------------------------------------------------------------------------------------------------------------
   UTILS
-------------------------------------------------------------------------------------------------------------}

function CopyTo(CONST s: string; iFrom, iTo: integer): string;                                                { Copy the text between iFrom and ending at iTo. The char at iTo is also copied. }
begin
 Result:= system.COPY(s, iFrom, iTo-iFrom+1);                                                                 { +1 ca sa includa si valoarea de la potitia 'iFrom' }
end;


function ExtractTextBetween(CONST s, TagStart, TagEnd: string): string;                                       { Extract the text between sFrom and iTo. For example '<H>Title</H>' will return 'Title' is iFrom= '<H>' and iTo= '</H>'. The search of iTo starts at the position of iFrom+ length(iFrom) }
VAR iFrom, iTo: Integer;
begin
 iFrom:= Pos(TagStart, s);
 iTo:= PosEx(TagEnd, s, iFrom+ Length(TagStart));

 if (iFrom> 0) AND (iTo> 0)
 then Result:= CopyTo(s, ifrom+ Length(TagStart), iTo-1)
 else Result:= '';
end;


// Extracts the {} part from a valid ID like: 'MONITOR\Default_Monitor\{4d36e96e-e325-11ce-bfc1-08002be10318}\0004'
function ExtractDeviceID(ID: String): string;
begin
 if Pos('MONITOR\', ID) <> 1 then EXIT('');              // Invalid ID
 Result:= ExtractTextBetween(ID, '{', '}');
end;


function MonitorOrientationToString(AMonitorOrientation: TMonitorOrientation): String;
begin
  case AMonitorOrientation of
    moLandscape: Result := 'Landscape';
    moPortrait : Result := 'Portrait';
    moLandscapeFlipped: Result := 'Landscape (flipped)';
    moPortraitFlipped : Result := 'Portrait (flipped)';
  end;
end;





{-------------------------------------------------------------------------------------------------------------
   THIS IS MOVED HERE FROM cmDebuger so I can get rid of this package (3rd party) dependencies in my LightCommon.bpl
-------------------------------------------------------------------------------------------------------------}
CONST
   CRLF             = #13#10;
   //LBRK             = CRLF+CRLF;
   TAB              = #9;

function Rectangle2Str(CONST Rect: TRect): string;
begin
 Result:= 'Top: '+ IntToStr(Rect.Top)+ ',  Left: '+ IntToStr(Rect.Left)+ ',  Bottom: '+ IntToStr(Rect.Bottom)+ ',  Right: '+ IntToStr(Rect.Right);
end;


function BoolToStrYesNo(CONST B: Boolean): string;
begin
 if B
 then Result := 'Yes'
 else Result := 'No';
end;


function GenerateScreenReport: string;
begin
 Result:= ' [SCREEN]'+ CRLF;
 Result:= Result+ '  Total monitors: '     + Tab+ IntToStr(Screen.MonitorCount)+ CRLF;
 Result:= Result+ '  Desktop rectangle:'   + Tab+ Rectangle2Str(Screen.DesktopRect)+ CRLF;                    { Specifies the boundaries of the virtual desktop relative to the upper-left corner of the primary monitor. Use DesktopRect to determine the coordinates of the entire virtual desktop, which includes all monitors in the system. DesktopRect is expressed in coordinates where (0,0) is the upper-left corner of the primary monitor. The primary monitor is the monitor with its Primary property set to true. }
 Result:= Result+ '  Desktop size: '       + Tab+ IntToStr(Screen.DesktopWidth) + 'x'+ IntToStr(Screen.DesktopHeight)+ CRLF;
 Result:= Result+ '  Screen size: '        + Tab+Tab+ IntToStr(Screen.Width)        + 'x'+ IntToStr(Screen.Height)+ CRLF;
 Result:= Result+ '  Screen.WorkArea: '    + Tab+ IntToStr(Screen.WorkAreaWidth)+ 'x'+ IntToStr(Screen.WorkAreaHeight)+ CRLF;  { Use Screen to obtain information about the current state of the screen in an application. }
 Result:= Result+ '  Screen.PixPerInch: '  + Tab+ IntToStr(Screen.PixelsPerInch)+ CRLF;
 Result:= Result+ '  WorkareaRect: '       + Tab+ Rectangle2Str(Screen.WorkAreaRect)+ CRLF;                   { Use Screen to obtain information about the current state of the screen in an application. }
 Result:= Result+ CRLF;

 Result:= Result+ ' [MONTORS]'+ CRLF;
 for var i:= 0 to Screen.MonitorCount-1 DO     { Enumerate through all monitors }
  begin
   Result:= Result+ '  Monitor '+ IntToStr(i)+''+ CRLF;
   Result:= Result+ '    Monitor no: '     + Tab+     IntToStr(Screen.Monitors[i].MonitorNum)+ CRLF;
   Result:= Result+ '    DeviceID: '       + Tab+Tab+ Screen.Monitors[i].DeviceID+ CRLF;
   Result:= Result+ '    FriendlyName: '   + Tab+     Screen.Monitors[i].FriendlyName+ CRLF;
   Result:= Result+ '    BoundsRect:'      + Tab+Tab+ Rectangle2Str(Screen.Monitors[i].BoundsRect)+ CRLF;     { Real monitor area. Indicates the dimensions of the monitor in pixels. Read BoundsRect to learn the dimensions of the monitor. BoundsRect gives the dimensions of the monitor in pixels, where (0,0) represents the top-left corner of the primary monitor. The top of BoundsRect is given by the Top property, the left edge by the Left property, and the height and width by the Height and Width properties respectively. Note:  The BoundsRect property does not take into account any task bars or tool bars docked on the monitor. To determine the area on the monitor that is free of such docked Winapi.Windows, use the WorkareaRect property. }
   Result:= Result+ '    Workarea rect: '  + Tab+     Rectangle2Str(Screen.Monitors[i].WorkareaRect)+ CRLF;       { Monitor's usable area (without task bar). Gives the application useable area of the monitor. WorkareaRect returns a TRect value furnished with the coordinates and dimensions of the work area of the Monitor. On Winapi.Windows, for example, the application tabs at the screen mean that the Workarea is smaller than the monitor size. Note:  The TRect Right and Bottom values are one pixel beyond Workarea boundary. They are given these values to allow for easy calculation of Workarea width and height as (Right-Left) and (Bottom-Top) respectively.}
   Result:= Result+ '    Is Primary: '     + Tab+     BoolToStrYesNo(Screen.Monitors[i].Primary)+ CRLF;
   Result:= Result+ '    Top: '            + Tab+Tab+ IntToStr( Screen.Monitors[i].Top   )+ CRLF;
   Result:= Result+ '    Left: '           + Tab+Tab+ IntToStr( Screen.Monitors[i].Left  )+ CRLF;
   Result:= Result+ '    Width: '          + Tab+Tab+ IntToStr( Screen.Monitors[i].Width )+ CRLF;
   Result:= Result+ '    Height: '         + Tab+Tab+ IntToStr( Screen.Monitors[i].Height)+ CRLF;
   Result:= Result+ '    DPI: '            + Tab+Tab+ IntToStr( Screen.Monitors[i].PixelsPerInch)+ CRLF;
   //Result:= Result+ '    MonitorFromRect(mdNearest): ' + IntToStr( (Screen.MonitorFromRect( TRect.Create(P,100,100), mdNearest)).MonitorNum);
   Result:= Result+ CRLF;
  end;

 //Result:= RemoveLastEnter(Result);
end;

end.
