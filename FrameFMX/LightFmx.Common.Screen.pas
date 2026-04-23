UNIT LightFmx.Common.Screen;

{=============================================================================================================
   2026.04.21
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   FMX-specific screen and platform utilities.
   Cross-platform (non-FMX) utilities live in LightCore.Platform.

   FMX always works in logical pixels. When you set Button.Width := 100, that's 100 logical pixels on every device.
   The OS scales it to physical pixels automatically. So when you decide "hide this button below width X", X is in logical pixels.

   Common logical widths:
   - Small phones: ~360 dp (older Androids, iPhone SE)
   - Typical phones: 393-412 dp (iPhone 15, Pixel 7/8)
   - Large phones: 430 dp (iPhone 15 Pro Max)
   - Small tablets: 600-768 dp
   - Full tablets: 800-1024+ dp

   +-----------------------------------------------------+
   ¦           Device           ¦ Logical Width ¦ Scale  ¦
   +----------------------------+---------------+--------¦
   ¦ Samsung Galaxy S24/S23/A54 ¦ 360 dp        ¦ 3.0x   ¦
   +----------------------------+---------------+--------¦
   ¦ Google Pixel 9/8/7         ¦ 412 dp        ¦ 2.625x ¦
   +----------------------------+---------------+--------¦
   ¦ iPhone SE                  ¦ 375 pt        ¦ 2x     ¦
   +----------------------------+---------------+--------¦
   ¦ iPhone 15/16               ¦ 393 pt        ¦ 3x     ¦
   +----------------------------+---------------+--------¦
   ¦ iPhone 15 Pro Max          ¦ 430 pt        ¦ 3x     ¦
   +----------------------------+---------------+--------¦
   ¦ iPad mini                  ¦ ~744 pt       ¦ —      ¦
   +----------------------------+---------------+--------¦
   ¦ iPad Pro 11"               ¦ ~834 pt       ¦ —      ¦
   +-----------------------------------------------------+

   COMPACT_WIDTH = 600 cleanly separates all phones (<430) from all tablets (600+).

   Important: Screen.Size.Width gives the full screen logical width (including areas behind system bars).
   Your form's ClientWidth gives the usable width. On phones in portrait mode they're nearly identical.
   On desktop or split-screen, ClientWidth is what matters.


   My Nord Plus Phone: 350x697

   Tester: LightSaber\Demo\FMX\Demos\FMX_Demos.dpr
=============================================================================================================}

INTERFACE

USES
   System.SysUtils,
   FMX.Forms, FMX.Platform;

function ScreenScale: Single;

// SCREEN SIZE
function IsPhoneScreen  : Boolean;   { Width < COMPACT_WIDTH (600) — phone }
function IsTabletScreen : Boolean;   { Width in COMPACT_WIDTH..HIGH_WIDTH (600–1024) — tablet }
function IsDesktopScreen: Boolean;   { Width >= HIGH_WIDTH (1024+) — desktop }

// WINDOW WIDTH — pass a container/window width (e.g. Form.ClientWidth) to classify it against the same breakpoints.
function WidthFitsPhone  (W: Single): Boolean;   { W < COMPACT_WIDTH (600) }
function WidthFitsTablet (W: Single): Boolean;   { COMPACT_WIDTH ≤ W < HIGH_WIDTH }
function WidthFitsDesktop(W: Single): Boolean;   { W ≥ HIGH_WIDTH (1024) }

function GenerateScreenResolutionRep: string;


IMPLEMENTATION


function ScreenScale: Single;
var
  ScreenSvc: IFMXScreenService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, IInterface(ScreenSvc))
  then Result:= ScreenSvc.GetScreenScale
  else Result:= 1;
end;


CONST
  COMPACT_WIDTH = 600;  // Below this width, hide most buttons and use popup menu.
  HiGH_WIDTH    = 1024; // Below this with, show some buttons but some in "Compact" mode (only icon, no text). Over this resolution we show all.



{ IFMXScreenService returns physical screen size on mobile (Android/iOS).
  Screen.Size can return 0 on mobile if called before the main form is fully created,
  because FMX populates it lazily from the platform's screen metrics. }
function GetScreenWidth: Single;
VAR ScreenSvc: IFMXScreenService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenSvc)
  then Result:= ScreenSvc.GetScreenSize.X
  else Result:= Screen.Size.Width;
end;


function IsPhoneScreen: Boolean;
begin
  Result:= GetScreenWidth < COMPACT_WIDTH;
end;


function IsTabletScreen: Boolean;
VAR W: Single;
begin
  W:= GetScreenWidth;
  Result:= (W >= COMPACT_WIDTH) AND (W < HiGH_WIDTH);
end;


function IsDesktopScreen: Boolean;
begin
  Result:= GetScreenWidth >= HiGH_WIDTH;
end;


function WidthFitsPhone(W: Single): Boolean;
begin
  Result:= W < COMPACT_WIDTH;
end;


function WidthFitsTablet(W: Single): Boolean;
begin
  Result:= (W >= COMPACT_WIDTH) AND (W < HiGH_WIDTH);
end;


function WidthFitsDesktop(W: Single): Boolean;
begin
  Result:= W >= HiGH_WIDTH;
end;







// Reports
function GenerateScreenResolutionRep: string;
var
  Scale: Single;
  LogicalW, LogicalH: Single;
  PhysW, PhysH: Integer;
  DeviceType: string;
begin
  LogicalW:= Screen.Size.Width;
  LogicalH:= Screen.Size.Height;
  Scale:= ScreenScale;
  PhysW:= Round(LogicalW * Scale);
  PhysH:= Round(LogicalH * Scale);

  // Classify device type
  if IsPhoneScreen then
    DeviceType:= 'Phone'
  else if IsTabletScreen then
    DeviceType:= 'Tablet'
  else
    DeviceType:= 'Desktop';

  Result:=
    'Logical (DIPs): ' + Round(LogicalW).ToString + ' x ' + Round(LogicalH).ToString + sLineBreak +
    'Physical (px): '  + PhysW.ToString           + ' x ' + PhysH.ToString + sLineBreak +
    'Scale factor: '   + FormatFloat('0.##', Scale) + 'x' + sLineBreak +
    'Device type: '    + DeviceType + sLineBreak +
    'Breakpoints: '    + sLineBreak +
    '    COMPACT_WIDTH = '+ IntToStr(COMPACT_WIDTH)+ sLineBreak +
    '    HIGH_WIDTH = '   + IntToStr(HiGH_WIDTH);
end;


end.
