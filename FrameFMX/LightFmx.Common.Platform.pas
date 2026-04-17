UNIT LightFmx.Common.Platform;

{=============================================================================================================
   2026.04
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
=============================================================================================================}

INTERFACE

USES
   System.SysUtils,
   FMX.Forms, FMX.Platform;

function ScreenScale: Single;
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


function GenerateScreenResolutionRep: string;
var
  Scale: Single;
  LogicalW, LogicalH: Single;
  PhysW, PhysH: Integer;
begin
  LogicalW:= Screen.Size.Width;
  LogicalH:= Screen.Size.Height;
  Scale:= ScreenScale;
  PhysW:= Round(LogicalW * Scale);
  PhysH:= Round(LogicalH * Scale);

  Result:=
    'Logical (DIPs): ' + Round(LogicalW).ToString + ' x ' + Round(LogicalH).ToString + sLineBreak +
    'Physical (px): '  + PhysW.ToString           + ' x ' + PhysH.ToString + sLineBreak +
    'Scale factor: '   + FormatFloat('0.##', Scale) + 'x' + sLineBreak +
    'COMPACT_WIDTH: 600 (current: ' + Round(LogicalW).ToString + ')';
end;


end.
