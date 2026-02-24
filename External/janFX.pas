UNIT janFX;

{--------------------------------------------------------------------------------------------------
  JanFX - Image filters and effects library
  Original:  2-July-2000
  Link    :  jansfreeware.com (offline)
  
  Updated 2026-02-22, 
  Gabriel Moraru

  Requires pf24bit
    Many functions expect the input image to be in 24 bit (16.7 mil) color depth.
    Functions that use ScanLine with pbytearray assume BGR byte order (pf24bit).
    Some functions auto-set pf24bit; others require the caller to ensure it.

  Requires $R- (range checking off)
    Why: ScanLine pixel access uses raw byte arrays indexed beyond declared bounds.
    See: http://stackoverflow.com/questions/628965

  Origins
    The original code from JanFX was migrated to Jedi.JVPaintFX.pas
    Some modifications by Gabriel Moraru
    Bug reported to Jedi: http://issuetracker.delphi-jedi.org/view.php?id=6436

  Tester:
     c:\Projects\Project Testers\gr JanFx demo\Tester.dpr
     c:\Projects\LightSaber ImageResampler Test\ResamplerTester.dpr

  Unit tests:
     c:\Projects\LightSaber\External\UnitTesting\Test.janFX.pas
-------------------------------------------------------------------------------------------------------------}

INTERFACE                                                                                                     {$WARN GARBAGE OFF}   {Silence the: 'W1011 Text after final END' warning }

USES
 Windows, System.Types, System.SysUtils, System.Classes, System.Math,
 Vcl.Graphics, janFXStretch;

TYPE
 { Type of a filter for use with Stretch }
 //TODO: TLightBrush mixes two concerns: lb* values used elsewhere and mb* values used by SqueezeHor/SplitRound. Consider splitting into two enums.
 TLightBrush = (lbBrightness, lbContrast, lbSaturation, lbfisheye, lbrotate, lbtwist, lbrimple, mbHor, mbTop, mbBottom, mbDiamond, mbWaste, mbRound, mbround2, mbsplitround, mbsplitwaste);


{ Utils }
function  ConvertColor     (Value: Integer): TColor;
function  Set255           (Clr: Integer): Integer;
procedure CopyBMP          (toBmp: TBitmap; fromBMP: TGraphic);
function  TrimInt          (i, Min, Max: Integer): Integer;

{ Blend/Transparency }
procedure Blend            (src1, src2, dst: TBitmap; amount: extended);
procedure QuartoOpaque     (src, dst: TBitmap);
procedure semiOpaque       (src, dst: TBitmap);

{ Flip/ Mirror/ Rotate }
procedure FlipRight        (src: TBitmap);
procedure FlipDown         (src: TBitmap);                     { my cGraphics.FlipDown is better }

procedure MirrorRight      (src: TBitmap);
procedure MirrorDown       (src: TBitmap);                     { mirrors the top of the image over the bottom (the bottom is lost) }

{ Rotate }
procedure TurnLeft         (src, dst: TBitmap);                { Turn the image 90degree to the left }
procedure TurnRight        (src, dst: TBitmap);
procedure SmoothRotate     (src, dst: TBitmap; centerX, centerY: Integer; Angle: extended);     { CenterX, centerY: shows where to put the rotated image relative to the center of the canvas. Use (Width div 2), and (height div 2). IMPORTANT: doesn't work with BMP.HandleType:= bmDDB. needs pf24 }

{ Tile }
procedure TexturizeTile    (src: TBitmap; amount: Integer);                    { makes the image smaller then tiles that image until it fils the screen }
procedure TexturizeOverlap (src: TBitmap; amount: Integer);                    { similar with TexturizeTile but the image is a bit croped }
procedure Tile             (src, dst: TBitmap; amount: Integer);               { makes the image smaller then tiles that image. Src could be same as Dst  }

{ Color filters }
procedure filterxBlue      (src: TBitmap; Min, Max: Integer);
procedure filterxGreen     (src: TBitmap; Min, Max: Integer);
procedure filterxRed       (src: TBitmap; Min, Max: Integer);
procedure filterBlue       (src: TBitmap; Min, Max: Integer);
procedure filterGreen      (src: TBitmap; Min, Max: Integer);
procedure filterRed        (src: TBitmap; Min, Max: Integer);

procedure KeepBlue         (src: TBitmap; factor: extended);
procedure KeepGreen        (src: TBitmap; factor: extended);
procedure KeepRed          (src: TBitmap; factor: extended);

procedure ExtractColor     (src: TBitmap; acolor: TColor);
procedure ExcludeColor     (src: TBitmap; acolor: TColor);

{ LIGHT/COLOR }
procedure InvertColors     (src: TBitmap);
procedure GrayScale        (src: TBitmap);
procedure Contrast         (src: TBitmap; amount: Integer);          {   : this expects an image with 24 bit color depth }
procedure Lightness        (src: TBitmap; amount: Integer);       { Brightness } {   this expects an image with 24 bit color depth }
procedure Darkness         (src: TBitmap; amount: Integer);
procedure Saturation       (src: TBitmap; amount: Integer);      {  : this expects an image with 24 bit color depth - cubicdesign }

{ Noise }
procedure AddColorNoise    (clip: TBitmap; amount: Integer);
procedure AddMonoNoise     (clip: TBitmap; amount: Integer);
procedure Spray            (clip: TBitmap; amount: Integer);
procedure SplitBlur        (clip: TBitmap; EffectPower: Integer);
procedure GaussianBlur     (clip: TBitmap; Count: Integer; EffectPower: Integer=1);

{ AntiAlising }
procedure AntiAlias        (clip: TBitmap);                    { blurs/smooths the image }
procedure AntiAliasRect    (clip: TBitmap; XOrigin, YOrigin, XFinal, YFinal: Integer);
procedure SmoothPoint      (clip: TBitmap; xk, yk: Integer);    { Averages 4 neighboring pixels to smooth a single point }

{ Size }
procedure SqueezeHor       (src, dst: TBitmap; amount: Integer; style: TLightBrush);

{ Efecte }
procedure SplitRound       (src, dst: TBitmap; amount: Integer; style: TLightBrush);
procedure Wave             (Clip: TBitmap; amount, inference, style: Integer);
procedure Mosaic           (Bm: TBitmap; Size: Integer= 9);                             { Pixelated image  }
procedure Twist            (Bmp, dst: TBitmap; amount: Integer);
procedure Solorize         (src, dst: TBitmap; amount: Integer);
procedure Posterize        (src, dst: TBitmap; amount: Integer);
procedure Emboss           (Bmp: TBitmap);
procedure FishEye          (Bmp, dst: TBitmap; amount: extended);

{ Efecte }
procedure MaskOval         (src: TBitmap; acolor: TColor);                     { oval frame arround image. nothing special }
procedure Buttonize        (src: TBitmap; depth: byte; weight: Integer);       { classic buttonize. the 3D frame looks poor! }
procedure ButtonizeOval    (src: TBitmap; depth: byte; weight: Integer; rim: string);
procedure HeightMap        (src: TBitmap; amount: Integer);                    { weird effect. I think it tried to detect borders (based on their height) }
procedure RippleRandom     (src: TBitmap; amount: Integer);                    { Random noise over image }
procedure RippleTooth      (src: TBitmap; amount: Integer);                    { Image is split in lines and the lines are moved up/down a bit }
procedure RippleTriangle   (src: TBitmap; amount: Integer);
procedure Triangles        (src: TBitmap; amount: Integer);                    { Pure shit! }
procedure Shake            (src, dst: TBitmap; factor: extended);
procedure ShakeDown        (src, dst: TBitmap; factor: extended);
procedure FoldRight        (src1, src2, dst: TBitmap; amount: extended);
procedure Trace            (src: TBitmap; intensity: Integer);
procedure SpotLight        (src: TBitmap; amount: Integer; Spot: TRect);
procedure splitlight       (clip: TBitmap; amount: Integer);
procedure MakeSeamlessClip (clip: TBitmap; seam: Integer);
procedure Seamless         (src: TBitmap; depth: byte);

{ Shadows }
procedure ShadowDownLeft   (src: TBitmap);
procedure ShadowDownRight  (src: TBitmap);
procedure shadowUpleft     (src: TBitmap);
procedure shadowUpright    (src: TBitmap);

{ Marble }                                                                                 { shitty }
procedure marble           (src, dst: TBitmap; scale: extended; turbulence: Integer);      { src = dest }
procedure marble2          (src, dst: TBitmap; scale: extended; turbulence: Integer);
procedure marble3          (src, dst: TBitmap; scale: extended; turbulence: Integer);
procedure marble4          (src, dst: TBitmap; scale: extended; turbulence: Integer);
procedure marble5          (src, dst: TBitmap; scale: extended; turbulence: Integer);
procedure marble6          (src, dst: TBitmap; scale: extended; turbulence: Integer);
procedure marble7          (src, dst: TBitmap; scale: extended; turbulence: Integer);
procedure marble8          (src, dst: TBitmap; scale: extended; turbulence: Integer);

{ Fractal }
procedure DrawMandelJulia  (src: TBitmap; x0, y0, x1, y1: extended; Niter: Integer; Mandel: Boolean);
procedure Plasma           (src1, src2, dst: TBitmap; scale, turbulence: extended);          { does't work }
procedure MandelBrot       (src: TBitmap; factor: Integer);    { Black/White maldenbrot - poor!    factor= 16-64 }
procedure MaskMandelBrot   (src: TBitmap; factor: Integer);    { Same as MandelBrot but on top of an image }

{ Convolution }
procedure ConvolveM        (ray: array of Integer; z: word; aBmp: TBitmap);
procedure ConvolveE        (ray: array of Integer; z: word; aBmp: TBitmap);
procedure ConvolveI        (ray: array of Integer; z: word; aBmp: TBitmap);
procedure ConvolveFilter   (FilterType, EdgeType: Integer; src: TBitmap);    //Uses ConvolveX.   FilterType=0..8; EdgeType=0..2 (0 for seamless)


IMPLEMENTATION


TYPE
 TFColor = record
  b, g, r: byte;
 end;

 { For scanline simplification }
 TRGBArray = ARRAY [0 .. 32767] OF TRGBTriple;
 pRGBArray = ^TRGBArray;


{-------------------------------------------------------------------------------------------------------------
   UTILS
-------------------------------------------------------------------------------------------------------------}
function IntToByte(CONST i: Integer): byte; { Ar fi bine sa INLINE aceasta functie direct in cod }
begin
 if i > 255
 then Result := 255
 else
   if i < 0
   then Result := 0
   else Result := i;
end;

procedure CopyBMP(toBmp: TBitmap; fromBMP: TGraphic);
begin
 toBmp.Width  := fromBMP.Width;
 toBmp.Height := fromBMP.Height;
 toBmp.PixelFormat := pf24bit;
 toBmp.canvas.Draw(0, 0, fromBMP);
end;


procedure GrayScale(src: TBitmap);
var
 p0: pbytearray;
 Gray, x, y: Integer;
begin
 for y := 0 to src.Height - 1 do
  begin
   p0 := src.ScanLine[y];
   for x := 0 to src.Width - 1 do
    begin
     Gray := Round(p0[x * 3] * 0.3 + p0[x * 3 + 1] * 0.59 + p0[x * 3 + 2] * 0.11);
     p0[x * 3]     := Gray;
     p0[x * 3 + 1] := Gray;
     p0[x * 3 + 2] := Gray;
    end;
  end;
end;


{ This just forces a value to be 0 - 255 for rgb purposes.  I used asm in an attempt at speed, but I don't think it helps much. }
function Set255(Clr: Integer): Integer;
asm
 MOV  EAX,Clr  // store value in EAX register (32-bit register)
 CMP  EAX,254  // compare it to 254
 JG   @SETHI   // if greater than 254 then go set to 255 (max value)
 CMP  EAX,1    // if less than 255, compare to 1
 JL   @SETLO   // if less than 1 go set to 0 (min value)
 RET           // otherwise it doesn't change, just exit
@SETHI:        // Set value to 255
 MOV  EAX,255  // Move 255 into the EAX register
 RET           // Exit (result value is the EAX register value)
@SETLO:        // Set value to 0
 MOV  EAX,0    // Move 0 into EAX register
end; // Result is in EAX








{-------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------}
procedure Contrast(src: TBitmap; amount: Integer); { this expects an image with 24 bit color depth }
var
 p0: pbytearray;
 rg, gg, bg, r, g, b, x, y: Integer;
begin
 if amount= 0 then EXIT;
 src.PixelFormat := pf24bit; // Force 24bit - ScanLine assumes BGR byte layout

 for y := 0 to src.Height - 1 do
  begin
   p0 := src.ScanLine[y];
   for x := 0 to src.Width - 1 do
    begin
     r := p0[x * 3];
     g := p0[x * 3 + 1];
     b := p0[x * 3 + 2];
     rg := (Abs(127 - r) * amount) div 255;
     gg := (Abs(127 - g) * amount) div 255;
     bg := (Abs(127 - b) * amount) div 255;

     if r > 127
     then r := r + rg
     else r := r - rg;

     if g > 127
     then g := g + gg
     else g := g - gg;

     if b > 127
     then b := b + bg
     else b := b - bg;

     p0[x * 3] := IntToByte(r);
     p0[x * 3 + 1] := IntToByte(g);
     p0[x * 3 + 2] := IntToByte(b);
    end;
  end;
end;


procedure Lightness(src: TBitmap; amount: Integer); { Brightness }
var
 p0: pbytearray;
 r, g, b, x, y: Integer;
begin
 if amount= 0 then EXIT;
 src.PixelFormat := pf24bit;  // Force 24bit - ScanLine assumes BGR byte layout

 for y := 0 to src.Height - 1 do begin
   p0 := src.ScanLine[y];
   for x := 0 to src.Width - 1 do
    begin
     r := p0[x * 3];
     g := p0[x * 3 + 1];
     b := p0[x * 3 + 2];
     p0[x * 3] := IntToByte(r + ((255 - r) * amount) div 255);
     p0[x * 3 + 1] := IntToByte(g + ((255 - g) * amount) div 255);
     p0[x * 3 + 2] := IntToByte(b + ((255 - b) * amount) div 255);
    end;
  end;
end;


procedure Darkness(src: TBitmap; amount: Integer);
var
 p0: pbytearray;
 r, g, b, x, y: Integer;
begin
 if amount= 0 then EXIT;
 src.PixelFormat := pf24bit; // Force 24bit - ScanLine assumes BGR byte layout

 for y := 0 to src.Height - 1 DO
 begin
   p0 := src.ScanLine[y];
   for x := 0 to src.Width - 1 do
    begin
     r := p0[x * 3];
     g := p0[x * 3 + 1];
     b := p0[x * 3 + 2];
     p0[x * 3] := IntToByte(r - ((r) * amount) div 255);
     p0[x * 3 + 1] := IntToByte(g - ((g) * amount) div 255);
     p0[x * 3 + 2] := IntToByte(b - ((b) * amount) div 255);
    end;
  end;
end;


procedure Saturation(src: TBitmap; amount: Integer);
{
 512= Oversaturated
 255= Normal saturation (no processing)
   0= Colorless (gray)
-255= Negative colors

 but it can go way beyond these values }
VAR
  p0: pbytearray;
  Gray, r, g, b, x, y: Integer;
begin
 if amount= 255 then EXIT;

 src.PixelFormat := pf24bit;   // Force 24bit - ScanLine assumes BGR byte layout

 for y := 0 to src.Height - 1 do
  begin
   p0 := src.ScanLine[y];
   for x := 0 to src.Width - 1 do
    begin
     r := p0[x * 3];
     g := p0[x * 3 + 1];
     b := p0[x * 3 + 2];
     Gray := (r + g + b) div 3;
     p0[x * 3] := IntToByte(Gray + (((r - Gray) * amount) div 255));
     p0[x * 3 + 1] := IntToByte(Gray + (((g - Gray) * amount) div 255));
     p0[x * 3 + 2] := IntToByte(Gray + (((b - Gray) * amount) div 255));
    end;
  end;
end;




{-------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------}
procedure SmoothRotate(src, dst: TBitmap; centerX, centerY: Integer; Angle: extended);
{ CenterX, centerY: shows where to put the rotated image relative to the center of the canvas. Use (Width div 2), and (height div 2)
  IMPORTANT: doesn't work with BMP.HandleType:= bmDDB. needs pf24 }
TYPE
TFColor = record
 b, g, r: byte end;

 VAR
   Top, Bottom, eww, nsw, fx, fy: extended;
   cAngle, sAngle: Double;
   xDiff, yDiff, ifx, ify, px, py, ix, iy, x, y: Integer;
   nw, ne, sw, se: TFColor;
   p1, p2, P3: pbytearray;
 begin
  Assert(dst.PixelFormat = pf24bit, 'SmoothRotate needs pf24!');
  Assert(src.PixelFormat = pf24bit, 'SmoothRotate needs pf24!');

  //Angle := Angle;
  Angle := -Angle * Pi / 180;
  sAngle:= sin(Angle);
  cAngle:= cos(Angle);
  xDiff := (dst.Width - src.Width) div 2;
  yDiff := (dst.Height - src.Height) div 2;

  for y := 0 to dst.Height - 1 DO
   begin
    P3 := dst.ScanLine[y];
    py := 2 * (y - centerY) + 1;
    for x := 0 to dst.Width - 1 do
     begin
      px := 2 * (x - centerX) + 1;
      fx := (((px * cAngle - py * sAngle) - 1) / 2 + centerX) - xDiff;
      fy := (((px * sAngle + py * cAngle) - 1) / 2 + centerY) - yDiff;
      ifx := Round(fx);
      ify := Round(fy);

      if (ifx > -1) AND (ifx < src.Width) AND (ify > -1) AND (ify < src.Height) then
       begin
        eww := fx - ifx;
        nsw := fy - ify;
        iy := TrimInt(ify + 1, 0, src.Height - 1);
        ix := TrimInt(ifx + 1, 0, src.Width  - 1);
        p1 := src.ScanLine[ify];
        p2 := src.ScanLine[iy];
        nw.r := p1[ifx * 3];
        nw.g := p1[ifx * 3 + 1];
        nw.b := p1[ifx * 3 + 2];
        ne.r := p1[ix * 3];
        ne.g := p1[ix * 3 + 1];
        ne.b := p1[ix * 3 + 2];
        sw.r := p2[ifx * 3];
        sw.g := p2[ifx * 3 + 1];
        sw.b := p2[ifx * 3 + 2];
        se.r := p2[ix * 3];
        se.g := p2[ix * 3 + 1];
        se.b := p2[ix * 3 + 2];

        Top := nw.b + eww * (ne.b - nw.b);
        Bottom := sw.b + eww * (se.b - sw.b);
        P3[x * 3 + 2] := IntToByte(Round(Top + nsw * (Bottom - Top)));

        Top := nw.g + eww * (ne.g - nw.g);
        Bottom := sw.g + eww * (se.g - sw.g);
        P3[x * 3 + 1] := IntToByte(Round(Top + nsw * (Bottom - Top)));

        Top := nw.r + eww * (ne.r - nw.r);
        Bottom := sw.r + eww * (se.r - sw.r);
        P3[x * 3] := IntToByte(Round(Top + nsw * (Bottom - Top)));
       end;
     end;
   end;
 end;


{-------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------}
procedure GaussianBlur(Clip: TBitmap; Count: Integer; EffectPower: Integer=1); { Count tells how many time to apply the blur effect. }
VAR i: Integer;
begin
 for i := 1 to Count
  DO SplitBlur(clip, EffectPower);
end;


procedure SplitBlur(clip: TBitmap; EffectPower: Integer);
VAR
   p0, p1, p2: pbytearray;
   cx, x, y: Integer;
   Buf: array [0 .. 3, 0 .. 2] of Byte;
begin
 if EffectPower = 0 then EXIT;

 for y := 0 to clip.Height - 1 DO
  begin
   p0 := clip.ScanLine[y];
   if y - EffectPower < 0
   then p1 := clip.ScanLine[y]
   else p1 := clip.ScanLine[y - EffectPower];{ y-Amount>0 }

   if y + EffectPower < clip.Height
   then p2 := clip.ScanLine[y + EffectPower]
   else p2 := clip.ScanLine[clip.Height - 1 - y];  { BUG FIX: was clip.Height - y which is OOB when y=0 }

   for x := 0 to clip.Width - 1 DO
    begin
     if x - EffectPower < 0
     then cx := x
     else  cx := x - EffectPower;{ x-Amount>0 }

     Buf[0, 0] := p1[cx * 3];
     Buf[0, 1] := p1[cx * 3 + 1];
     Buf[0, 2] := p1[cx * 3 + 2];
     Buf[1, 0] := p2[cx * 3];
     Buf[1, 1] := p2[cx * 3 + 1];
     Buf[1, 2] := p2[cx * 3 + 2];

     if x + EffectPower < clip.Width
     then cx := x + EffectPower
     else cx := clip.Width - 1 - x;  { BUG FIX: was clip.Width - x which is OOB when x=0 }

     Buf[2, 0] := p1[cx * 3];
     Buf[2, 1] := p1[cx * 3 + 1];
     Buf[2, 2] := p1[cx * 3 + 2];
     Buf[3, 0] := p2[cx * 3];
     Buf[3, 1] := p2[cx * 3 + 1];
     Buf[3, 2] := p2[cx * 3 + 2];
     p0[x * 3] := (Buf[0, 0] + Buf[1, 0] + Buf[2, 0] + Buf[3, 0]) shr 2;
     p0[x * 3 + 1] := (Buf[0, 1] + Buf[1, 1] + Buf[2, 1] + Buf[3, 1]) shr 2;
     p0[x * 3 + 2] := (Buf[0, 2] + Buf[1, 2] + Buf[2, 2] + Buf[3, 2]) shr 2;
    end;
  end;
end;


{-------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------}
//TODO: Spray uses Canvas.Pixels which is extremely slow. Rewrite with ScanLine for orders-of-magnitude speedup.
procedure Spray(clip: TBitmap; amount: Integer);  { Note: uses Canvas.Pixels which is very slow }
VAR
   i, j, x, y, w, h, Val: Integer;
begin
 if amount <= 0 then EXIT;
 h := clip.Height;
 w := clip.Width;
 for i := 0 to w - 1 do
  for j := 0 to h - 1 do
   begin
    Val := Random(amount);
    x := i + Val - Random(Val * 2);
    y := j + Val - Random(Val * 2);
    if (x > -1) and (x < w) and (y > -1) and (y < h) then
     clip.canvas.Pixels[i, j] := clip.canvas.Pixels[x, y];
   end;
end;


procedure Mosaic(Bm: TBitmap; size: Integer= 9);
var
 x, y, i, j: Integer;
 p1, p2: pbytearray;
 r, g, b: byte;
begin
 if size <= 0 then EXIT;  { Avoid infinite loop }
 y := 0;
 repeat
  p1 := Bm.ScanLine[y];
  // x:= 0;
  repeat
   j := 1;
   repeat
    p2 := Bm.ScanLine[y];
    x := 0;
    repeat
     r := p1[x * 3];
     g := p1[x * 3 + 1];
     b := p1[x * 3 + 2];
     i := 1;
     repeat
      p2[x * 3] := r;
      p2[x * 3 + 1] := g;
      p2[x * 3 + 2] := b;
      inc(x);
      inc(i);
     until (x >= Bm.Width) or (i > size);
    until x >= Bm.Width;
    inc(j);
    inc(y);
   until (y >= Bm.Height) or (j > size);
  until (y >= Bm.Height) or (x >= Bm.Width);
 until y >= Bm.Height;
end;


function TrimInt(i, Min, Max: Integer): Integer;
begin
 if i > Max then
  Result := Max
 else
  if i < Min then
   Result := Min
  else
   Result := i;
end;


procedure Twist(Bmp, dst: TBitmap; amount: Integer);

var
 fxmid, fymid: Single;
 txmid, tymid: Single;
 fx, fy: Single;
 tx2, ty2: Single;
 r: Single;
 theta: Single;
 ifx, ify: Integer;
 dx, dy: Single;
 OFFSET: Single;
 ty, tx: Integer;
 weight_x, weight_y: array [0 .. 1] of Single;
 weight: Single;
 new_red, new_green: Integer;
 new_blue: Integer;
 total_red, total_green: Single;
 total_blue: Single;
 ix, iy: Integer;
 sli, slo: pbytearray;

 function ArcTan2(xt, yt: Single): Single;
 begin
  if xt = 0 then
   if yt > 0 then
    Result := Pi / 2
   else
    Result := -(Pi / 2)
  else begin
    Result := ArcTan(yt / xt);
    if xt < 0 then
     Result := Pi + ArcTan(yt / xt);
   end;
 end;

 begin
  OFFSET := -(Pi / 2);
  dx := Bmp.Width - 1;
  dy := Bmp.Height - 1;
  r := Sqrt(dx * dx + dy * dy);
  tx2 := r;
  ty2 := r;
  txmid := (Bmp.Width - 1) / 2; // Adjust these to move center of rotation
  tymid := (Bmp.Height - 1) / 2; // Adjust these to move ......
  fxmid := (Bmp.Width - 1) / 2;
  fymid := (Bmp.Height - 1) / 2;
  if tx2 >= Bmp.Width then
   tx2 := Bmp.Width - 1;
  if ty2 >= Bmp.Height then
   ty2 := Bmp.Height - 1;

  for ty := 0 to Round(ty2) do begin
    for tx := 0 to Round(tx2) do begin
      dx := tx - txmid;
      dy := ty - tymid;
      r := Sqrt(dx * dx + dy * dy);
      if r = 0 then begin
        fx := 0;
        fy := 0;
       end
      else begin
        theta := ArcTan2(dx, dy) - r / amount - OFFSET;
        fx := r * cos(theta);
        fy := r * sin(theta);
       end;
      fx := fx + fxmid;
      fy := fy + fymid;

      ify := Trunc(fy);
      ifx := Trunc(fx);
      // Calculate the weights.
      if fy >= 0 then begin
        weight_y[1] := fy - ify;
        weight_y[0] := 1 - weight_y[1];
       end
      else begin
        weight_y[0] := -(fy - ify);
        weight_y[1] := 1 - weight_y[0];
       end;
      if fx >= 0 then begin
        weight_x[1] := fx - ifx;
        weight_x[0] := 1 - weight_x[1];
       end
      else begin
        weight_x[0] := -(fx - ifx);
        weight_x[1] := 1 - weight_x[0];
       end;

      if ifx < 0 then
       ifx := Bmp.Width - 1 - (-ifx mod Bmp.Width)
      else
       if ifx > Bmp.Width - 1 then
        ifx := ifx mod Bmp.Width;
      if ify < 0 then
       ify := Bmp.Height - 1 - (-ify mod Bmp.Height)
      else
       if ify > Bmp.Height - 1 then
        ify := ify mod Bmp.Height;

      total_red := 0.0;
      total_green := 0.0;
      total_blue := 0.0;
      for ix := 0 to 1 do begin
        for iy := 0 to 1 do begin
          if ify + iy < Bmp.Height then
           sli := Bmp.ScanLine[ify + iy]
          else
           sli := Bmp.ScanLine[Bmp.Height - ify - iy];
          if ifx + ix < Bmp.Width then begin
            new_red := sli[(ifx + ix) * 3];
            new_green := sli[(ifx + ix) * 3 + 1];
            new_blue := sli[(ifx + ix) * 3 + 2];
           end
          else begin
            new_red := sli[(Bmp.Width - ifx - ix) * 3];
            new_green := sli[(Bmp.Width - ifx - ix) * 3 + 1];
            new_blue := sli[(Bmp.Width - ifx - ix) * 3 + 2];
           end;
          weight := weight_x[ix] * weight_y[iy];
          total_red := total_red + new_red * weight;
          total_green := total_green + new_green * weight;
          total_blue := total_blue + new_blue * weight;
         end;
       end;
      slo := dst.ScanLine[ty];
      slo[tx * 3] := Round(total_red);
      slo[tx * 3 + 1] := Round(total_green);
      slo[tx * 3 + 2] := Round(total_blue);
     end;
   end;
 end;


procedure Wave(clip: TBitmap; amount, inference, style: Integer);
var
 x, y: Integer;
 BitMap: TBitmap;
 p1, p2: pbytearray;
 b: Integer;
 fangle: real;
 wavex: Integer;
begin
 if amount <= 0 then EXIT;  { Avoid division by zero in fangle and negative loop bounds }
 BitMap := TBitmap.create;
 BitMap.assign(clip);
 wavex := style;
 fangle := Pi / 2 / amount;
 for y := BitMap.Height - 1 - (2 * amount) downto amount do begin
   p1 := BitMap.ScanLine[y];
   b := 0;
   for x := 0 to BitMap.Width - 1 do begin
     p2 := clip.ScanLine[y + amount + b];
     p2[x * 3] := p1[x * 3];
     p2[x * 3 + 1] := p1[x * 3 + 1];
     p2[x * 3 + 2] := p1[x * 3 + 2];
     case wavex of               { BUG FIX: was variant() which is slow and fragile; use Round() }
      0: b := Round(amount * sin(fangle * x));
      1: b := Round(amount * sin(fangle * x) * cos(fangle * x));
      2: b := Round(amount * sin(fangle * x) * sin(inference * fangle * x));
     end;
    end;
  end;
 FreeAndNil(BitMap);
end;


procedure MakeSeamlessClip(clip: TBitmap; seam: Integer);
var
 p0, p1, p2: pbytearray;
 h, w, i, j, sv, sh: Integer;
 f0, f1, f2: real;
begin
 if seam = 0 then EXIT;  { Avoid division by zero }
 h := clip.Height;
 w := clip.Width;
 sv := h div seam;
 sh := w div seam;
 p1 := clip.ScanLine[0];
 p2 := clip.ScanLine[h - 1];
 for i := 0 to w - 1 do begin
   p1[i * 3] := p2[i * 3];
   p1[i * 3 + 1] := p2[i * 3 + 1];
   p1[i * 3 + 2] := p2[i * 3 + 2];
  end;
 p0 := clip.ScanLine[0];
 p2 := clip.ScanLine[sv];
 for j := 1 to sv - 1 do begin
   p1 := clip.ScanLine[j];
   for i := 0 to w - 1 do begin
     f0 := (p2[i * 3] - p0[i * 3]) / sv * j + p0[i * 3];
     p1[i * 3] := Round(f0);
     f1 := (p2[i * 3 + 1] - p0[i * 3 + 1]) / sv * j + p0[i * 3 + 1];
     p1[i * 3 + 1] := Round(f1);
     f2 := (p2[i * 3 + 2] - p0[i * 3 + 2]) / sv * j + p0[i * 3 + 2];
     p1[i * 3 + 2] := Round(f2);
    end;
  end;
 for j := 0 to h - 1 do begin
   p1 := clip.ScanLine[j];
   p1[(w - 1) * 3] := p1[0];
   p1[(w - 1) * 3 + 1] := p1[1];
   p1[(w - 1) * 3 + 2] := p1[2];
   for i := 1 to sh - 1 do begin
     f0 := (p1[(w - sh) * 3] - p1[(w - 1) * 3]) / sh * i + p1[(w - 1) * 3];
     p1[(w - 1 - i) * 3] := Round(f0);
     f1 := (p1[(w - sh) * 3 + 1] - p1[(w - 1) * 3 + 1]) / sh * i + p1[(w - 1) * 3 + 1];
     p1[(w - 1 - i) * 3 + 1] := Round(f1);
     f2 := (p1[(w - sh) * 3 + 2] - p1[(w - 1) * 3 + 2]) / sh * i + p1[(w - 1) * 3 + 2];
     p1[(w - 1 - i) * 3 + 2] := Round(f2);
    end;
  end;
end;


procedure splitLight(clip: TBitmap; amount: Integer);
var
 x, y, i: Integer;
 p1: pbytearray;

 function sinpixs(a: Integer): Integer;
 begin
  Result := Round(sin(a / 255 * Pi / 2) * 255);  { BUG FIX: was variant() }
 end;
 begin
  for i := 1 to amount do
   for y := 0 to clip.Height - 1 do begin
     p1 := clip.ScanLine[y];
     for x := 0 to clip.Width - 1 do begin
       p1[x * 3] := sinpixs(p1[x * 3]);
       p1[x * 3 + 1] := sinpixs(p1[x * 3 + 1]);
       p1[x * 3 + 2] := sinpixs(p1[x * 3 + 2]);
      end;
    end;
 end;


procedure SqueezeHor(src, dst: TBitmap; amount: Integer; style: TLightBrush);
var
 dx, x, y, c, cx: Integer;
 r: TRect;
 Bm: TBitmap;
 p0, p1: pbytearray;
begin
 r:= TRect.Empty;

 if amount > (src.Width div 2)
 then amount := src.Width div 2;
 Bm := TBitmap.create;
 Bm.PixelFormat := pf24bit;
 Bm.Height := 1;
 Bm.Width := src.Width;
 cx := src.Width div 2;
 p0 := Bm.ScanLine[0];

 for y := 0 to src.Height - 1 do begin
   p1 := src.ScanLine[y];
   for x := 0 to src.Width - 1 do begin
     c := x * 3;
     p0[c] := p1[c];
     p0[c + 1] := p1[c + 1];
     p0[c + 2] := p1[c + 2];
    end;

   case style of  { Only handles horizontal squeeze styles. Other TLightBrush values are handled by SplitRound. }
    mbHor:
     begin
      dx := amount;
      r := rect(dx, y, src.Width - dx, y + 1);
     end;
    mbTop:
     begin
      dx := Round((src.Height - 1 - y) / src.Height * amount);
      r := rect(dx, y, src.Width - dx, y + 1);
     end;
    mbBottom:
     begin
      dx := Round(y / src.Height * amount);
      r := rect(dx, y, src.Width - dx, y + 1);
     end;
    mbDiamond:
     begin
      dx := Round(amount * Abs(cos(y / (src.Height - 1) * Pi)));
      r := rect(dx, y, src.Width - dx, y + 1);
     end;
    mbWaste:
     begin
      dx := Round(amount * Abs(sin(y / (src.Height - 1) * Pi)));
      r := rect(dx, y, src.Width - dx, y + 1);
     end;
    mbRound:
     begin
      dx := Round(amount * Abs(sin(y / (src.Height - 1) * Pi)));
      r := rect(cx - dx, y, cx + dx, y + 1);
     end;
    mbround2:
     begin
      dx := Round(amount * Abs(sin(y / (src.Height - 1) * Pi * 2)));
      r := rect(cx - dx, y, cx + dx, y + 1);
     end;
   else
     r := rect(0, y, src.Width, y + 1);  { Fallback: full-width row for unsupported styles }
   end;
   dst.canvas.StretchDraw(r, Bm);
  end;
 FreeAndNil(Bm);
end;








{--------------------------------------------------------------------------------------------------
   FLIP / MIRROR / TILE
-------------------------------------------------------------------------------------------------- }
procedure FlipDown(src: TBitmap);    { FLIPS the image upside down without rotating it - This might be faster: http://delphi-kb.blogspot.com.es/2011/02/how-to-flip-bitmap-fast.html }
var
 dest: TBitmap;
 w, h, x, y: Integer;
 pd, ps: pbytearray;
begin
 w := src.Width;
 h := src.Height;
 dest := TBitmap.create;
 dest.Width := w;
 dest.Height := h;
 dest.PixelFormat := pf24bit;
 src.PixelFormat  := pf24bit;
 for y := 0 to h - 1 do
 begin
   pd := dest.ScanLine[y];
   ps := src.ScanLine[h - 1 - y];
   for x := 0 to w - 1 do
   begin
     pd[x * 3]     := ps[x * 3];
     pd[x * 3 + 1] := ps[x * 3 + 1];
     pd[x * 3 + 2] := ps[x * 3 + 2];
    end;
  end;
 src.assign(dest);
 FreeAndNil(dest);
end;


procedure FlipRight(src: TBitmap);
VAR
  dest: TBitmap;
  w, h, x, y: Integer;
  pd, ps: pbytearray;
begin
 w := src.Width;
 h := src.Height;
 dest := TBitmap.create;
 dest.Width := w;
 dest.Height := h;
 dest.PixelFormat := pf24bit;
 src.PixelFormat  := pf24bit;
 for y := 0 to h - 1 do begin
   pd := dest.ScanLine[y];
   ps := src.ScanLine[y];
   for x := 0 to w - 1 do
   begin
     pd[x * 3]     := ps[(w - 1 - x) * 3];
     pd[x * 3 + 1] := ps[(w - 1 - x) * 3 + 1];
     pd[x * 3 + 2] := ps[(w - 1 - x) * 3 + 2];
    end;
  end;
 src.assign(dest);
 FreeAndNil(dest);
end;


procedure MirrorRight(src: TBitmap);    { Copy the left side of the image over the right side, creating a mirrored image }
var
 w, h, x, y: Integer;
 P: pbytearray;
begin
 w := src.Width;
 h := src.Height;
 src.PixelFormat := pf24bit;
 for y := 0 to h - 1 do begin
   P := src.ScanLine[y];
   for x := 0 to w div 2 do begin
     P[(w - 1 - x) * 3] := P[x * 3];
     P[(w - 1 - x) * 3 + 1] := P[x * 3 + 1];
     P[(w - 1 - x) * 3 + 2] := P[x * 3 + 2];
    end;
  end;
end;


procedure MirrorDown(src: TBitmap);
var
 w, h, x, y: Integer;
 p1, p2: pbytearray;
begin
 w := src.Width;
 h := src.Height;
 src.PixelFormat := pf24bit;
 for y := 0 to h div 2 do
 begin
   p1 := src.ScanLine[y];
   p2 := src.ScanLine[h - 1 - y];
   for x := 0 to w - 1 do
   begin
     p2[x * 3] := p1[x * 3];
     p2[x * 3 + 1] := p1[x * 3 + 1];
     p2[x * 3 + 2] := p1[x * 3 + 2];
    end;
  end;
end;


procedure Tile(src, dst: TBitmap; amount: Integer);
var
   w, h, w2, h2, i, j: Integer;
   Bm: TBitmap;
begin
 w := src.Width;
 h := src.Height;
 dst.Width := w;
 dst.Height := h;
 dst.canvas.Draw(0, 0, src);
 if (amount <= 0) or ((w div amount) < 5) or ((h div amount) < 5) then Exit;
 h2 := h div amount;
 w2 := w div amount;

 Bm := TBitmap.create;
 TRY
  Bm.Width := w2;
  Bm.Height := h2;
  Bm.PixelFormat := pf24bit;
  smoothresize(src, Bm);
  for j := 0 to amount - 1 do
   for i := 0 to amount - 1 do
    dst.canvas.Draw(i * w2, j * h2, Bm);
 FINALLY
  FreeAndNil(Bm);
 END;
end;








{ --------------------------------------------------------------------------------------------------

  -------------------------------------------------------------------------------------------------- }
procedure SpotLight(src: TBitmap; amount: Integer; Spot: TRect);
VAR
   Bm: TBitmap;
   w, h: Integer;
begin
 Darkness(src, amount);
 w := src.Width;
 h := src.Height;
 Bm := TBitmap.create;
 Bm.Width := w;
 Bm.Height := h;
 Bm.canvas.Brush.color := clblack;
 Bm.canvas.FillRect(rect(0, 0, w, h));
 Bm.canvas.Brush.color := clwhite;
 Bm.canvas.Ellipse(Spot.Left, Spot.Top, Spot.Right, Spot.Bottom);
 Bm.transparent := true;
 Bm.TransparentColor := clwhite;
 src.canvas.Draw(0, 0, Bm);
 FreeAndNil(Bm);
end;


procedure Trace(src: TBitmap; intensity: Integer);
var
 x, y, i: Integer;
 p1, p2, P3, P4: pbytearray;
 tb, TraceB: byte;
 hasb: Boolean;
 BitMap: TBitmap;
begin
 tb := 0; //TODO: Forced initialization suppresses warning but may mask a logic bug - tb could be read before truly set. Review Trace algorithm.

 BitMap := TBitmap.create;
 BitMap.Width := src.Width;
 BitMap.Height := src.Height;
 BitMap.canvas.Draw(0, 0, src);
 BitMap.PixelFormat := pf8bit;
 src.PixelFormat := pf24bit;
 hasb := false;
 TraceB := $00;
 for i := 1 to intensity do
  begin
   for y := 0 to BitMap.Height - 2 do begin
     p1 := BitMap.ScanLine[y];
     p2 := BitMap.ScanLine[y + 1];
     P3 := src.ScanLine[y];
     P4 := src.ScanLine[y + 1];
     x := 0;
     repeat
      if p1[x] <> p1[x + 1] then begin
        if not hasb then
         begin
          tb := p1[x + 1];
          hasb := true;
          P3[x * 3] := TraceB;
          P3[x * 3 + 1] := TraceB;
          P3[x * 3 + 2] := TraceB;
         end
        else begin
          if p1[x] <> tb then // Variable might not be initialized
           begin
            P3[x * 3] := TraceB;
            P3[x * 3 + 1] := TraceB;
            P3[x * 3 + 2] := TraceB;
           end
          else
           begin
            P3[(x + 1) * 3] := TraceB;
            P3[(x + 1) * 3 + 1] := TraceB;
            P3[(x + 1) * 3 + 2] := TraceB;  { BUG FIX: was +1 (duplicate), should be +2 for blue channel }
           end;
         end;
       end;
      if p1[x] <> p2[x] then begin
        if not hasb then begin
          tb := p2[x];
          hasb := true;
          P3[x * 3] := TraceB;
          P3[x * 3 + 1] := TraceB;
          P3[x * 3 + 2] := TraceB;
         end
        else begin
          if p1[x] <> tb then
           begin
            P3[x * 3] := TraceB;
            P3[x * 3 + 1] := TraceB;
            P3[x * 3 + 2] := TraceB;
           end
          else
           begin
            P4[x * 3] := TraceB;
            P4[x * 3 + 1] := TraceB;
            P4[x * 3 + 2] := TraceB;
           end;
         end;
       end;
      inc(x);
     until x >= (BitMap.Width - 2);
    end;
   // do the same in the opposite direction
   // only when intensity>1
   if i > 1 then
    for y := BitMap.Height - 1 downto 1 do begin
      p1 := BitMap.ScanLine[y];
      p2 := BitMap.ScanLine[y - 1];
      P3 := src.ScanLine[y];
      P4 := src.ScanLine[y - 1];
      x := BitMap.Width - 1;
      repeat
       if p1[x] <> p1[x - 1] then begin
         if not hasb then begin
           tb := p1[x - 1];
           hasb := true;
           P3[x * 3] := TraceB;
           P3[x * 3 + 1] := TraceB;
           P3[x * 3 + 2] := TraceB;
          end
         else begin
           if p1[x] <> tb then
            begin
             P3[x * 3] := TraceB;
             P3[x * 3 + 1] := TraceB;
             P3[x * 3 + 2] := TraceB;
            end
           else
            begin
             P3[(x - 1) * 3] := TraceB;
             P3[(x - 1) * 3 + 1] := TraceB;
             P3[(x - 1) * 3 + 2] := TraceB;
            end;
          end;
        end;
       if p1[x] <> p2[x] then begin
         if not hasb then begin
           tb := p2[x];
           hasb := true;
           P3[x * 3] := TraceB;
           P3[x * 3 + 1] := TraceB;
           P3[x * 3 + 2] := TraceB;
          end
         else begin
           if p1[x] <> tb then
            begin
             P3[x * 3] := TraceB;
             P3[x * 3 + 1] := TraceB;
             P3[x * 3 + 2] := TraceB;
            end
           else
            begin
             P4[x * 3] := TraceB;
             P4[x * 3 + 1] := TraceB;
             P4[x * 3 + 2] := TraceB;
            end;
          end;
        end;
       dec(x);
      until x <= 1;
     end;
  end;
 FreeAndNil(BitMap);
end;


procedure shadowUpleft(src: TBitmap);
var
 x, y: Integer;
 BitMap: TBitmap;
 p1, p2: pbytearray;
begin
 BitMap := TBitmap.create;
 BitMap.Width := src.Width;
 BitMap.Height := src.Height;
 BitMap.PixelFormat := pf24bit;
 BitMap.canvas.Draw(0, 0, src);
 for y := 0 to BitMap.Height - 5 do begin
   p1 := BitMap.ScanLine[y];
   p2 := BitMap.ScanLine[y + 4];
   for x := 0 to BitMap.Width - 5 do
    if p1[x * 3] > p2[(x + 4) * 3] then begin
      p1[x * 3] := p2[(x + 4) * 3] + 1;
      p1[x * 3 + 1] := p2[(x + 4) * 3 + 1] + 1;
      p1[x * 3 + 2] := p2[(x + 4) * 3 + 2] + 1;
     end;
  end;
 src.assign(BitMap);
 FreeAndNil(BitMap);
end;


procedure shadowUpright(src: TBitmap);
var
 x, y: Integer;
 BitMap: TBitmap;
 p1, p2: pbytearray;
begin
 BitMap := TBitmap.create;
 BitMap.Width := src.Width;
 BitMap.Height := src.Height;
 BitMap.PixelFormat := pf24bit;
 BitMap.canvas.Draw(0, 0, src);
 for y := 0 to BitMap.Height - 5 do begin
   p1 := BitMap.ScanLine[y];
   p2 := BitMap.ScanLine[y + 4];
   for x := BitMap.Width - 1 downto 4 do
    if p1[x * 3] > p2[(x - 4) * 3] then begin
      p1[x * 3] := p2[(x - 4) * 3] + 1;
      p1[x * 3 + 1] := p2[(x - 4) * 3 + 1] + 1;
      p1[x * 3 + 2] := p2[(x - 4) * 3 + 2] + 1;
     end;
  end;
 src.assign(BitMap);
 FreeAndNil(BitMap);
end;


procedure ShadowDownLeft(src: TBitmap);
var
 x, y: Integer;
 BitMap: TBitmap;
 p1, p2: pbytearray;
begin
 BitMap := TBitmap.create;
 BitMap.Width := src.Width;
 BitMap.Height := src.Height;
 BitMap.PixelFormat := pf24bit;
 BitMap.canvas.Draw(0, 0, src);
 for y := BitMap.Height - 1 downto 4 do begin
   p1 := BitMap.ScanLine[y];
   p2 := BitMap.ScanLine[y - 4];
   for x := 0 to BitMap.Width - 5 do
    if p1[x * 3] > p2[(x + 4) * 3] then begin
      p1[x * 3] := p2[(x + 4) * 3] + 1;
      p1[x * 3 + 1] := p2[(x + 4) * 3 + 1] + 1;
      p1[x * 3 + 2] := p2[(x + 4) * 3 + 2] + 1;
     end;
  end;
 src.assign(BitMap);
 FreeAndNil(BitMap);
end;


procedure ShadowDownRight(src: TBitmap);
var
 x, y: Integer;
 BitMap: TBitmap;
 p1, p2: pbytearray;
begin
 BitMap := TBitmap.create;
 BitMap.Width := src.Width;
 BitMap.Height := src.Height;
 BitMap.PixelFormat := pf24bit;
 BitMap.canvas.Draw(0, 0, src);
 for y := BitMap.Height - 1 downto 4 do begin
   p1 := BitMap.ScanLine[y];
   p2 := BitMap.ScanLine[y - 4];
   for x := BitMap.Width - 1 downto 4 do
    if p1[x * 3] > p2[(x - 4) * 3] then begin
      p1[x * 3] := p2[(x - 4) * 3] + 1;
      p1[x * 3 + 1] := p2[(x - 4) * 3 + 1] + 1;
      p1[x * 3 + 2] := p2[(x - 4) * 3 + 2] + 1;
     end;
  end;
 src.assign(BitMap);
 FreeAndNil(BitMap);
end;


{-------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------}
procedure semiOpaque(src, dst: TBitmap);
var
 b: TBitmap;
 P: pbytearray;
 x, y: Integer;
begin
 b := TBitmap.create;
 b.Width := src.Width;
 b.Height := src.Height;
 b.PixelFormat := pf24bit;
 b.canvas.Draw(0, 0, src);
 for y := 0 to b.Height - 1 do begin
   P := b.ScanLine[y];
   if (y mod 2) = 0 then begin
     for x := 0 to b.Width - 1 do
      if (x mod 2) = 0 then begin
        P[x * 3] := $FF;
        P[x * 3 + 1] := $FF;
        P[x * 3 + 2] := $FF;
       end;
    end
   else begin
     for x := 0 to b.Width - 1 do
      if ((x + 1) mod 2) = 0 then begin
        P[x * 3] := $FF;
        P[x * 3 + 1] := $FF;
        P[x * 3 + 2] := $FF;
       end;
    end;
  end;
 b.transparent := true;
 b.TransparentColor := clwhite;
 dst.canvas.Draw(0, 0, b);
 FreeAndNil(b);
end;


procedure QuartoOpaque(src, dst: TBitmap);
var
 b: TBitmap;
 P: pbytearray;
 x, y: Integer;
begin
 b := TBitmap.create;
 b.Width := src.Width;
 b.Height := src.Height;
 b.PixelFormat := pf24bit;
 b.canvas.Draw(0, 0, src);
 for y := 0 to b.Height - 1 do begin
   P := b.ScanLine[y];
   if (y mod 2) = 0 then begin
     for x := 0 to b.Width - 1 do
      if (x mod 2) = 0 then begin
        P[x * 3] := $FF;
        P[x * 3 + 1] := $FF;
        P[x * 3 + 2] := $FF;
       end;
    end
   else begin
     for x := 0 to b.Width - 1 do begin
       P[x * 3] := $FF;
       P[x * 3 + 1] := $FF;
       P[x * 3 + 2] := $FF;
      end;

    end;
  end;
 b.transparent := true;
 b.TransparentColor := clwhite;
 dst.canvas.Draw(0, 0, b);
 FreeAndNil(b);
end;


procedure FoldRight(src1, src2, dst: TBitmap; amount: extended);
var
 w, h, x, y, xf, xf0: Integer;
 ps1, ps2, pd: pbytearray;
begin
 src1.PixelFormat := pf24bit;
 src2.PixelFormat := pf24bit;
 w := Min(src1.Width, src2.Width);    { Use smallest to avoid OOB }
 h := Min(src1.Height, src2.Height);  { Use smallest to avoid OOB }
 dst.Width := w;
 dst.Height := h;
 dst.PixelFormat := pf24bit;
 xf := Round(amount * w);
 for y := 0 to h - 1 do begin
   ps1 := src1.ScanLine[y];
   ps2 := src2.ScanLine[y];
   pd := dst.ScanLine[y];
   for x := 0 to xf do begin
     xf0 := xf + (xf - x);
     if xf0 < w then begin
       pd[xf0 * 3] := ps1[x * 3];
       pd[xf0 * 3 + 1] := ps1[x * 3 + 1];
       pd[xf0 * 3 + 2] := ps1[x * 3 + 2];
       pd[x * 3] := ps2[x * 3];
       pd[x * 3 + 1] := ps2[x * 3 + 1];
       pd[x * 3 + 2] := ps2[x * 3 + 2];
      end;
    end;
   if (2 * xf) < w - 1 then
    for x := 2 * xf + 1 to w - 1 do begin
      pd[x * 3] := ps1[x * 3];
      pd[x * 3 + 1] := ps1[x * 3 + 1];
      pd[x * 3 + 2] := ps1[x * 3 + 2];
     end;
  end;
end;


procedure Shake(src, dst: TBitmap; factor: extended);
var
 x, y, h, w, dx: Integer;
 P: pbytearray;
begin
 dst.canvas.Draw(0, 0, src);
 dst.PixelFormat := pf24bit;
 w := dst.Width;
 h := dst.Height;
 dx := Round(factor * w);
 if dx = 0 then
  Exit;
 if dx > (w div 2) then
  Exit;

 for y := 0 to h - 1 do begin
   P := dst.ScanLine[y];
   if (y mod 2) = 0 then
    for x := dx to w - 1 do begin
      P[(x - dx) * 3] := P[x * 3];
      P[(x - dx) * 3 + 1] := P[x * 3 + 1];
      P[(x - dx) * 3 + 2] := P[x * 3 + 2];
     end
   else
    for x := w - 1 downto dx do begin
      P[x * 3] := P[(x - dx) * 3];
      P[x * 3 + 1] := P[(x - dx) * 3 + 1];
      P[x * 3 + 2] := P[(x - dx) * 3 + 2];
     end;
  end;

end;


procedure ShakeDown(src, dst: TBitmap; factor: extended);
var
 x, y, h, w, dy: Integer;
 P, p2, P3: pbytearray;
begin
 dst.canvas.Draw(0, 0, src);
 dst.PixelFormat := pf24bit;
 w := dst.Width;
 h := dst.Height;
 dy := Round(factor * h);
 if dy = 0 then
  Exit;
 if dy > (h div 2) then
  Exit;

 for y := dy to h - 1 do begin
   P := dst.ScanLine[y];
   p2 := dst.ScanLine[y - dy];
   for x := 0 to w - 1 do
    if (x mod 2) = 0 then
     begin
      p2[x * 3] := P[x * 3];
      p2[x * 3 + 1] := P[x * 3 + 1];
      p2[x * 3 + 2] := P[x * 3 + 2];
     end;
  end;
 for y := h - 1 - dy downto 0 do begin
   P := dst.ScanLine[y];
   P3 := dst.ScanLine[y + dy];
   for x := 0 to w - 1 do
    if (x mod 2) <> 0 then
     begin
      P3[x * 3] := P[x * 3];
      P3[x * 3 + 1] := P[x * 3 + 1];
      P3[x * 3 + 2] := P[x * 3 + 2];
     end;
  end;
end;


procedure Plasma(src1, src2, dst: TBitmap; scale, turbulence: extended);  { doesn't work - original comment }
var
 cval, sval: array [0 .. 255] of Integer;
 i, x, y, w, h, xx, yy: Integer;
 Asin, Acos: extended;
 ps1, ps2, pd: pbytearray;
begin
 w := src1.Width;
 h := src1.Height;
 if turbulence < 10 then
  turbulence := 10;
 if scale < 5 then
  scale := 5;
 for i := 0 to 255 do begin
   sincos(i / turbulence, Asin, Acos);
   sval[i] := Round(-scale * Asin);
   cval[i] := Round(scale * Acos);
  end;
 for y := 0 to h - 1 do begin
   pd := dst.ScanLine[y];
   ps2 := src2.ScanLine[y];
   for x := 0 to w - 1 do begin
     xx := x + sval[ps2[x * 3]];
     yy := y + cval[ps2[x * 3]];
     if (xx >= 0) and (xx < w) and (yy >= 0) and (yy < h) then begin
       ps1 := src1.ScanLine[yy];
       pd[x * 3] := ps1[xx * 3];
       pd[x * 3 + 1] := ps1[xx * 3 + 1];
       pd[x * 3 + 2] := ps1[xx * 3 + 2];
      end;
    end;
  end;
end;


procedure SplitRound(src, dst: TBitmap; amount: Integer; style: TLightBrush);
VAR
 x, y, w, c, c00, dx, cx: Integer;
 r, R00: TRect;
 Bm, bm2: TBitmap;
 p0, p00, p1: pbytearray;
begin
 if amount = 0 then
  begin
   dst.canvas.Draw(0, 0, src);
   Exit;
  end;
 cx := src.Width DIV 2;
 if amount > cx
 then amount := cx;
 w := src.Width;
 Bm := TBitmap.create;
 TRY
  Bm.PixelFormat := pf24bit;
  Bm.Height := 1;
  Bm.Width := cx;
  bm2 := TBitmap.create;  
  TRY
   bm2.PixelFormat := pf24bit;
   bm2.Height := 1;
   bm2.Width := cx;
   p0 := Bm.ScanLine[0];
   p00 := bm2.ScanLine[0];
   for y := 0 to src.Height - 1 DO
    begin
     p1 := src.ScanLine[y];
     for x := 0 to cx - 1 DO
      begin
       c := x * 3;
       c00 := (cx + x) * 3;
       p0[c] := p1[c];
       p0[c + 1] := p1[c + 1];
       p0[c + 2] := p1[c + 2];
       p00[c] := p1[c00];
       p00[c + 1] := p1[c00 + 1];
       p00[c + 2] := p1[c00 + 2];
      end;

     case style of
       mbsplitround: dx := Round(amount * Abs(sin(y / (src.Height - 1) * Pi)));
       mbsplitwaste: dx := Round(amount * Abs(cos(y / (src.Height - 1) * Pi)));
      else
       raise Exception.Create('Error in janFX.SplitRound: unsupported style');
       // dx := 0;  -- dead code after raise (removed)
     end;

     r := rect(0, y, dx, y + 1);
     dst.canvas.StretchDraw(r, Bm);
     R00 := rect(w - 1 - dx, y, w - 1, y + 1);
     dst.canvas.StretchDraw(R00, bm2);
    end;
  FINALLY
    FreeAndNil(Bm2);
  END;
 FINALLY
   FreeAndNil(bm);
 END;
end;


procedure Emboss(Bmp: TBitmap);
var
 x, y: Integer;
 p1, p2: pbytearray;
begin
 for y := 0 to Bmp.Height - 2 do
  begin
   p1 := Bmp.ScanLine[y];
   p2 := Bmp.ScanLine[y + 1];
   for x := 0 to Bmp.Width - 4 do
    begin
     p1[x * 3] := (p1[x * 3] + (p2[(x + 3) * 3] xor $FF)) shr 1;
     p1[x * 3 + 1] := (p1[x * 3 + 1] + (p2[(x + 3) * 3 + 1] xor $FF)) shr 1;
     p1[x * 3 + 2] := (p1[x * 3 + 2] + (p2[(x + 3) * 3 + 2] xor $FF)) shr 1;
    end;
  end;
end;


{-------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------}
procedure FishEye(Bmp, dst: TBitmap; amount: extended);
var
 xmid, ymid: Single;
 fx, fy: Single;
 r1, r2: Single;
 ifx, ify: Integer;
 dx, dy: Single;
 rmax: Single;
 ty, tx: Integer;
 weight_x, weight_y: array [0 .. 1] of Single;
 weight: Single;
 new_red, new_green: Integer;
 new_blue: Integer;
 total_red, total_green: Single;
 total_blue: Single;
 ix, iy: Integer;
 sli, slo: pbytearray;
begin
 if amount = 0 then EXIT;  { Avoid division by zero in rmax calculation }
 xmid := Bmp.Width / 2;
 ymid := Bmp.Height / 2;
 rmax := dst.Width * amount;

 for ty := 0 to dst.Height - 1 do begin
   for tx := 0 to dst.Width - 1 do begin
     dx := tx - xmid;
     dy := ty - ymid;
     r1 := Sqrt(dx * dx + dy * dy);
     if r1 = 0 then begin
       fx := xmid;
       fy := ymid;
      end
     else begin
       r2 := rmax / 2 * (1 / (1 - r1 / rmax) - 1);
       fx := dx * r2 / r1 + xmid;
       fy := dy * r2 / r1 + ymid;
      end;
     ify := Trunc(fy);
     ifx := Trunc(fx);
     // Calculate the weights.
     if fy >= 0 then begin
       weight_y[1] := fy - ify;
       weight_y[0] := 1 - weight_y[1];
      end
     else begin
       weight_y[0] := -(fy - ify);
       weight_y[1] := 1 - weight_y[0];
      end;
     if fx >= 0 then begin
       weight_x[1] := fx - ifx;
       weight_x[0] := 1 - weight_x[1];
      end
     else begin
       weight_x[0] := -(fx - ifx);
       weight_x[1] := 1 - weight_x[0];
      end;

     if ifx < 0 then
      ifx := Bmp.Width - 1 - (-ifx mod Bmp.Width)
     else
      if ifx > Bmp.Width - 1 then
       ifx := ifx mod Bmp.Width;
     if ify < 0 then
      ify := Bmp.Height - 1 - (-ify mod Bmp.Height)
     else
      if ify > Bmp.Height - 1 then
       ify := ify mod Bmp.Height;

     total_red := 0.0;
     total_green := 0.0;
     total_blue := 0.0;
     for ix := 0 to 1 do begin
       for iy := 0 to 1 do begin
         if ify + iy < Bmp.Height then
          sli := Bmp.ScanLine[ify + iy]
         else
          sli := Bmp.ScanLine[Bmp.Height - ify - iy];
         if ifx + ix < Bmp.Width then begin
           new_red := sli[(ifx + ix) * 3];
           new_green := sli[(ifx + ix) * 3 + 1];
           new_blue := sli[(ifx + ix) * 3 + 2];
          end
         else begin
           new_red := sli[(Bmp.Width - ifx - ix) * 3];
           new_green := sli[(Bmp.Width - ifx - ix) * 3 + 1];
           new_blue := sli[(Bmp.Width - ifx - ix) * 3 + 2];
          end;
         weight := weight_x[ix] * weight_y[iy];
         total_red := total_red + new_red * weight;
         total_green := total_green + new_green * weight;
         total_blue := total_blue + new_blue * weight;
        end;
      end;
     slo := dst.ScanLine[ty];
     slo[tx * 3] := Round(total_red);
     slo[tx * 3 + 1] := Round(total_green);
     slo[tx * 3 + 2] := Round(total_blue);

    end;
  end;
end;




{-------------------------------------------------------------------------------------------------------------
    COLOR CHANNEL
-------------------------------------------------------------------------------------------------------------}
procedure KeepBlue(src: TBitmap; factor: extended);
var
 x, y, w, h: Integer;
 p0: pbytearray;
begin
 src.PixelFormat := pf24bit;
 w := src.Width;
 h := src.Height;
 for y := 0 to h - 1 do begin
   p0 := src.ScanLine[y];
   for x := 0 to w - 1 do begin
     p0[x * 3] := Round(factor * p0[x * 3]);
     p0[x * 3 + 1] := 0;
     p0[x * 3 + 2] := 0;
    end;
  end;
end;


procedure KeepGreen(src: TBitmap; factor: extended);
var
 x, y, w, h: Integer;
 p0: pbytearray;
begin
 src.PixelFormat := pf24bit;
 w := src.Width;
 h := src.Height;
 for y := 0 to h - 1 do begin
   p0 := src.ScanLine[y];
   for x := 0 to w - 1 do begin
     p0[x * 3 + 1] := Round(factor * p0[x * 3 + 1]);
     p0[x * 3] := 0;
     p0[x * 3 + 2] := 0;
    end;
  end;
end;


procedure KeepRed(src: TBitmap; factor: extended);
var
 x, y, w, h: Integer;
 p0: pbytearray;
begin
 src.PixelFormat := pf24bit;
 w := src.Width;
 h := src.Height;
 for y := 0 to h - 1 do begin
   p0 := src.ScanLine[y];
   for x := 0 to w - 1 do begin
     p0[x * 3 + 2] := Round(factor * p0[x * 3 + 2]);
     p0[x * 3 + 1] := 0;
     p0[x * 3] := 0;
    end;
  end;
end;



procedure filterRed(src: TBitmap; Min, Max: Integer);
var
 c, x, y: Integer;
 p1: pbytearray;
begin
 for y := 0 to src.Height - 1 do begin
   p1 := src.ScanLine[y];
   for x := 0 to src.Width - 1 do begin
     c := x * 3;
     if (p1[c + 2] > Min) and (p1[c + 2] < Max) then
      p1[c + 2] := $FF
     else
      p1[c + 2] := 0;
     p1[c] := 0;
     p1[c + 1] := 0;
    end;
  end;
end;


procedure filterGreen(src: TBitmap; Min, Max: Integer);
var
 c, x, y: Integer;
 p1: pbytearray;

begin
 for y := 0 to src.Height - 1 do begin
   p1 := src.ScanLine[y];
   for x := 0 to src.Width - 1 do begin
     c := x * 3;
     if (p1[c + 1] > Min) and (p1[c + 1] < Max) then
      p1[c + 1] := $FF
     else
      p1[c + 1] := 0;
     p1[c] := 0;
     p1[c + 2] := 0;
    end;
  end;
end;


procedure filterBlue(src: TBitmap; Min, Max: Integer);
var
 c, x, y: Integer;
 p1: pbytearray;
begin
 for y := 0 to src.Height - 1 do begin
   p1 := src.ScanLine[y];
   for x := 0 to src.Width - 1 do begin
     c := x * 3;
     if (p1[c] > Min) and (p1[c] < Max) then
      p1[c] := $FF
     else
      p1[c] := 0;
     p1[c + 1] := 0;
     p1[c + 2] := 0;
    end;
  end;
end;


procedure filterxRed(src: TBitmap; Min, Max: Integer);
var
 c, x, y: Integer;
 p1: pbytearray;
begin
 for y := 0 to src.Height - 1 do begin
   p1 := src.ScanLine[y];
   for x := 0 to src.Width - 1 do begin
     c := x * 3;
     if (p1[c + 2] > Min) and (p1[c + 2] < Max) then
      p1[c + 2] := $FF
     else
      p1[c + 2] := 0;
    end;
  end;
end;


procedure filterxGreen(src: TBitmap; Min, Max: Integer);
var
 c, x, y: Integer;
 p1: pbytearray;
begin
 for y := 0 to src.Height - 1 do begin
   p1 := src.ScanLine[y];
   for x := 0 to src.Width - 1 do begin
     c := x * 3;
     if (p1[c + 1] > Min) and (p1[c + 1] < Max) then
      p1[c + 1] := $FF
     else
      p1[c + 1] := 0;
    end;
  end;
end;


procedure filterxBlue(src: TBitmap; Min, Max: Integer);
var
 c, x, y: Integer;
 p1: pbytearray;
begin
 for y := 0 to src.Height - 1 do begin
   p1 := src.ScanLine[y];
   for x := 0 to src.Width - 1 do begin
     c := x * 3;
     if (p1[c] > Min) and (p1[c] < Max) then
      p1[c] := $FF
     else
      p1[c] := 0;
    end;
  end;
end;


// Just a small function to map the numbers to colors
function ConvertColor(Value: Integer): TColor;
begin
 case Value of
  0 : Result := clblack;
  1 : Result := clNavy;
  2 : Result := clGreen;
  3 : Result := clAqua;
  4 : Result := clRed;
  5 : Result := clPurple;
  6 : Result := clMaroon;
  7 : Result := clSilver;
  8 : Result := clGray;
  9 : Result := clBlue;
  10: Result := clLime;
  11: Result := clOlive;
  12: Result := clFuchsia;
  13: Result := clTeal;
  14: Result := clYellow;
  15: Result := clwhite;
 else Result := clwhite;
 end;
end;


procedure InvertColors(src: TBitmap);
var
  w, h, x, y: Integer;
  P: pbytearray;
begin
 w := src.Width;
 h := src.Height;
 src.PixelFormat := pf24bit;
 for y := 0 to h - 1 do begin
   P := src.ScanLine[y];
   for x := 0 to w - 1 do begin
     P[x * 3] := not P[x * 3];
     P[x * 3 + 1] := not P[x * 3 + 1];
     P[x * 3 + 2] := not P[x * 3 + 2];
    end;
  end;
end;


procedure ExtractColor(src: TBitmap; acolor: TColor);
var
 w, h, x, y: Integer;
 P: pbytearray;
 Ecolor: TColor;
 r, g, b: byte;
begin
 w := src.Width;
 h := src.Height;
 Ecolor := colortoRGB(acolor);
 r := GetRValue(Ecolor);
 g := GetGValue(Ecolor);
 b := GetBValue(Ecolor);
 src.PixelFormat := pf24bit;
 for y := 0 to h - 1 do begin
   P := src.ScanLine[y];
   for x := 0 to w - 1 do begin
     if ((P[x * 3] <> b) or (P[x * 3 + 1] <> g) or (P[x * 3 + 2] <> r)) then
     begin
       P[x * 3] := $00;
       P[x * 3 + 1] := $00;
       P[x * 3 + 2] := $00;
      end;
    end
  end;
 src.transparent := true;
 src.TransparentColor := clblack;
end;


procedure ExcludeColor(src: TBitmap; acolor: TColor);
var
   w, h, x, y: Integer;
   P: pbytearray;
   Ecolor: TColor;
   r, g, b: byte;
begin
 w := src.Width;
 h := src.Height;
 Ecolor := colortoRGB(acolor);
 r := GetRValue(Ecolor);
 g := GetGValue(Ecolor);
 b := GetBValue(Ecolor);
 src.PixelFormat := pf24bit;
 for y := 0 to h - 1 do begin
   P := src.ScanLine[y];
   for x := 0 to w - 1 do begin
     if ((P[x * 3] = b) and (P[x * 3 + 1] = g) and (P[x * 3 + 2] = r)) then begin
       P[x * 3] := $00;
       P[x * 3 + 1] := $00;
       P[x * 3 + 2] := $00;
      end;
    end
  end;
 src.transparent := true;
 src.TransparentColor := clblack;
end;









{-------------------------------------------------------------------------------------------------------------
    NOISE
-------------------------------------------------------------------------------------------------------------}
procedure AddColorNoise(clip: TBitmap; amount: Integer);
var
 p0: pbytearray;
 x, y, r, g, b: Integer;

begin
 clip.PixelFormat := pf24bit;
 // Force 24bit - ScanLine assumes BGR byte layout
 for y := 0 to clip.Height - 1 do
  begin
   p0 := clip.ScanLine[y];
   for x := 0 to clip.Width - 1 do
    begin
     r := p0[x * 3] + (Random(amount) - (amount shr 1));
     g := p0[x * 3 + 1] + (Random(amount) - (amount shr 1));
     b := p0[x * 3 + 2] + (Random(amount) - (amount shr 1));
     p0[x * 3] := IntToByte(r);
     p0[x * 3 + 1] := IntToByte(g);
     p0[x * 3 + 2] := IntToByte(b);
    end;
  end;
end;

procedure AddMonoNoise(clip: TBitmap; amount: Integer);
var
 p0: pbytearray;
 x, y, a, r, g, b: Integer;
begin
 clip.PixelFormat := pf24bit;
 // Force 24bit - ScanLine assumes BGR byte layout
 for y := 0 to clip.Height - 1 do
  begin
   p0 := clip.ScanLine[y];
   for x := 0 to clip.Width - 1 do
    begin
     a := Random(amount) - (amount shr 1);
     r := p0[x * 3] + a;
     g := p0[x * 3 + 1] + a;
     b := p0[x * 3 + 2] + a;
     p0[x * 3] := IntToByte(r);
     p0[x * 3 + 1] := IntToByte(g);
     p0[x * 3 + 2] := IntToByte(b);
    end;
  end;
end;



{-------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------}
procedure Triangles(src: TBitmap; amount: Integer);
type
 Ttriplet = record
  r, g, b: byte;
 end;
VAR
  w, h, x, y, tb, tm, te: Integer;
  ps: pbytearray;
  T: Ttriplet;
begin
 w := src.Width;
 h := src.Height;
 src.PixelFormat := pf24bit;
 if amount < 5 then
  amount := 5;
 amount := (amount div 2) * 2 + 1;
 tm := amount div 2;
 for y := 0 to h - 1 do begin
   ps := src.ScanLine[y];
   T.r := ps[0];
   T.g := ps[1];
   T.b := ps[2];
   tb := y mod (amount - 1);
   if tb > tm then
    tb := 2 * tm - tb;
   if tb = 0 then
    tb := amount;
   te := tm + Abs(tm - (y mod amount));
   for x := 0 to w - 1 do begin
     if (x mod tb) = 0 then begin
       T.r := ps[x * 3];
       T.g := ps[x * 3 + 1];
       T.b := ps[x * 3 + 2];
      end;
     if ((x mod te) = 1) and (tb <> 0) then
     begin
       T.r := ps[x * 3];
       T.g := ps[x * 3 + 1];
       T.b := ps[x * 3 + 2];
      end;
     ps[x * 3] := T.r;
     ps[x * 3 + 1] := T.g;
     ps[x * 3 + 2] := T.b;
    end;
  end;
end;


procedure RippleTooth(src: TBitmap; amount: Integer);
var
 x, y: Integer;
 p1, p2: pbytearray;
 b: Byte;
begin
 src.PixelFormat := pf24bit;
 amount := Min(src.Height div 2, amount);
 for y := src.Height - 1 - amount downto 0 do begin
   p1 := src.ScanLine[y];
   b := 0;
   for x := 0 to src.Width - 1 do begin
     p2 := src.ScanLine[y + b];
     p2[x * 3] := p1[x * 3];
     p2[x * 3 + 1] := p1[x * 3 + 1];
     p2[x * 3 + 2] := p1[x * 3 + 2];
     inc(b);
     if b > amount then
      b := 0;
    end;
  end;
end;


procedure RippleTriangle(src: TBitmap; amount: Integer);
var
 x, y: Integer;
 p1, p2: pbytearray;
 b: byte;
 doinc: Boolean;
begin
 amount := Min(src.Height div 2, amount);
 for y := src.Height - 1 - amount downto 0 do begin
   p1 := src.ScanLine[y];
   b := 0;
   doinc := true;
   for x := 0 to src.Width - 1 do begin
     p2 := src.ScanLine[y + b];
     p2[x * 3] := p1[x * 3];
     p2[x * 3 + 1] := p1[x * 3 + 1];
     p2[x * 3 + 2] := p1[x * 3 + 2];
     if doinc then begin
       inc(b);
       if b > amount then begin
         doinc := false;
         b := amount - 1;
        end;
      end
     else begin
       if b = 0 then begin
         doinc := true;
         b := 2;
        end;
       dec(b);
      end;
    end;
  end;
end;


procedure RippleRandom(src: TBitmap; amount: Integer);
var
 x, y: Integer;
 p1, p2: pbytearray;
 b: byte;
begin
 amount := Min(src.Height div 2, amount);
 src.PixelFormat := pf24bit;
 randomize;
 for y := src.Height - 1 - amount downto 0 do begin
   p1 := src.ScanLine[y];
   b := 0;
   for x := 0 to src.Width - 1 do begin
     p2 := src.ScanLine[y + b];
     p2[x * 3] := p1[x * 3];
     p2[x * 3 + 1] := p1[x * 3 + 1];
     p2[x * 3 + 2] := p1[x * 3 + 2];
     b := Random(amount);
    end;
  end;
end;


{-------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------}
procedure TexturizeOverlap(src: TBitmap; amount: Integer);
var
 w, h, x, y, xo: Integer;
 Bm: TBitmap;
 arect: TRect;
begin
 Bm := TBitmap.create;
 amount := Min(src.Width div 2, amount);
 amount := Min(src.Height div 2, amount);
 xo := Round(amount * 2 / 3);
 Bm.Width := amount;
 Bm.Height := amount;
 w := src.Width;
 h := src.Height;
 arect := rect(0, 0, amount, amount);
 Bm.canvas.StretchDraw(arect, src);
 y := 0;
 repeat
  x := 0;
  repeat
   src.canvas.Draw(x, y, Bm);
   x := x + xo;
  until x >= w;
  y := y + xo;
 until y >= h;
 FreeAndNil(Bm);
end;


procedure TexturizeTile(src: TBitmap; amount: Integer);
var
 w, h, x, y: Integer;
 Bm: TBitmap;
 arect: TRect;
begin
 Bm := TBitmap.create;
 amount := Min(src.Width div 2, amount);
 amount := Min(src.Height div 2, amount);
 Bm.Width := amount;
 Bm.Height := amount;
 w := src.Width;
 h := src.Height;
 arect := rect(0, 0, amount, amount);
 Bm.canvas.StretchDraw(arect, src);
 y := 0;
 repeat
  x := 0;
  repeat
   src.canvas.Draw(x, y, Bm);
   x := x + Bm.Width;
  until x >= w;
  y := y + Bm.Height;
 until y >= h;
 FreeAndNil(Bm);
end;


procedure HeightMap(src: TBitmap; amount: Integer);
VAR
  Bm: TBitmap;
  w, h, x, y: Integer;
  pb, ps: pbytearray;
  c: Integer;
begin
 h := src.Height;
 w := src.Width;
 Bm := TBitmap.create;
 Bm.Width := w;
 Bm.Height := h;
 Bm.PixelFormat  := pf24bit;
 src.PixelFormat := pf24bit;
 Bm.canvas.Draw(0, 0, src);
 for y := 0 to h - 1 do begin
   pb := Bm.ScanLine[y];
   for x := 0 to w - 1 do begin
     c := Round((pb[x * 3] + pb[x * 3 + 1] + pb[x * 3 + 2]) / 3 / 255 * amount);
     if (y - c) >= 0 then begin
       ps := src.ScanLine[y - c];
       ps[x * 3] := pb[x * 3];
       ps[x * 3 + 1] := pb[x * 3 + 1];
       ps[x * 3 + 2] := pb[x * 3 + 2];
      end;
    end;
  end;
 FreeAndNil(Bm);
end;








{-------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------}
procedure TurnLeft(src, dst: TBitmap);
VAR
   w, h, x, y: Integer;
   ps, pd: pbytearray;
begin
 h := src.Height;
 w := src.Width;
 src.PixelFormat := pf24bit;
 dst.PixelFormat := pf24bit;
 dst.Height := w;
 dst.Width := h;
 for y := 0 to h - 1 do
 begin
   ps := src.ScanLine[y];
   for x := 0 to w - 1 do
   begin
     pd := dst.ScanLine[w - 1 - x];
     pd[y * 3] := ps[x * 3];
     pd[y * 3 + 1] := ps[x * 3 + 1];
     pd[y * 3 + 2] := ps[x * 3 + 2];
    end;
  end;
end;


procedure turnRight(src, dst: TBitmap);
var
 w, h, x, y: Integer;
 ps, pd: pbytearray;
begin
 h := src.Height;
 w := src.Width;
 src.PixelFormat := pf24bit;
 dst.PixelFormat := pf24bit;
 dst.Height := w;
 dst.Width := h;
 for y := 0 to h - 1 do begin
   ps := src.ScanLine[y];
   for x := 0 to w - 1 do begin
     pd := dst.ScanLine[x];
     pd[(h - 1 - y) * 3] := ps[x * 3];
     pd[(h - 1 - y) * 3 + 1] := ps[x * 3 + 1];
     pd[(h - 1 - y) * 3 + 2] := ps[x * 3 + 2];
    end;
  end;
end;




{-------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------}
procedure Blend(src1, src2, dst: TBitmap; amount: extended);
var
 w, h, x, y: Integer;
 ps1, ps2, pd: pbytearray;
begin
 w := src1.Width;
 h := Min(src1.Height, src2.Height);  { Use smallest dimension to avoid OOB on src2 }
 dst.Width  := w;
 dst.Height := h;
 src1.PixelFormat := pf24bit;
 src2.PixelFormat := pf24bit;
 dst.PixelFormat  := pf24bit;
 for y := 0 to h - 1 DO
 begin
   ps1 := src1.ScanLine[y];
   ps2 := src2.ScanLine[y];
   pd  := dst.ScanLine[y];
   for x := 0 to w - 1 DO
    begin
     pd[x * 3] := Round((1 - amount) * ps1[x * 3] + amount * ps2[x * 3]);
     pd[x * 3 + 1] := Round((1 - amount) * ps1[x * 3 + 1] + amount * ps2[x * 3 + 1]);
     pd[x * 3 + 2] := Round((1 - amount) * ps1[x * 3 + 2] + amount * ps2[x * 3 + 2]);
    end;
  end;
end;


procedure Solorize(src, dst: TBitmap; amount: Integer);  { Note: misspelled. Should be 'Solarize'. Kept for backward compatibility. }
var
 w, h, x, y: Integer;
 ps, pd: pbytearray;
 c: Integer;
begin
 w := src.Width;
 h := src.Height;
 src.PixelFormat := pf24bit;
 dst.Width := w;
 dst.Height := h;
 dst.PixelFormat := pf24bit;
 for y := 0 to h - 1 do begin
   ps := src.ScanLine[y];
   pd := dst.ScanLine[y];
   for x := 0 to w - 1 do begin
     c := (ps[x * 3] + ps[x * 3 + 1] + ps[x * 3 + 2]) div 3;
     if c > amount then begin
       pd[x * 3] := 255 - ps[x * 3];
       pd[x * 3 + 1] := 255 - ps[x * 3 + 1];
       pd[x * 3 + 2] := 255 - ps[x * 3 + 2];
      end
     else begin
       pd[x * 3] := ps[x * 3];
       pd[x * 3 + 1] := ps[x * 3 + 1];
       pd[x * 3 + 2] := ps[x * 3 + 2];
      end;
    end;
  end;
end;


procedure Posterize(src, dst: TBitmap; amount: Integer);
VAR
 w, h, x, y: Integer;
 ps, pd: pbytearray;
begin
 if amount = 0 then EXIT;  { Avoid division by zero }
 w := src.Width;
 h := src.Height;
 src.PixelFormat := pf24bit;
 dst.Width := w;
 dst.Height := h;
 dst.PixelFormat := pf24bit;
 for y := 0 to h - 1 do begin
   ps := src.ScanLine[y];
   pd := dst.ScanLine[y];
   for x := 0 to w - 1 do begin
     pd[x * 3]     := Round(ps[x * 3] / amount) * amount;
     pd[x * 3 + 1] := Round(ps[x * 3 + 1] / amount) * amount;
     pd[x * 3 + 2] := Round(ps[x * 3 + 2] / amount) * amount;
    end;
  end;
end;




procedure Buttonize(src: TBitmap; depth: byte; weight: Integer);
var
 p1, p2: pbytearray;
 w, w3, h, x, x3, y: Integer;
 a, r, g, b: Integer;
begin
 a := weight;
 w := src.Width;
 h := src.Height;
 src.PixelFormat := pf24bit;
 if depth = 0 then
  Exit;
 for y := 0 to depth do
  begin
   p1 := src.ScanLine[y];
   p2 := src.ScanLine[h - y - 1];
   // amount:= 1-y*am;
   for x := y to w - 1 - y do
    begin
     x3 := x * 3;
     // lighter
     r := p1[x3];
     g := p1[x3 + 1];
     b := p1[x3 + 2];
     p1[x3] := IntToByte(r + ((255 - r) * a) div 255);
     p1[x3 + 1] := IntToByte(g + ((255 - g) * a) div 255);
     p1[x3 + 2] := IntToByte(b + ((255 - b) * a) div 255);
     // darker
     r := p2[x3];
     g := p2[x3 + 1];
     b := p2[x3 + 2];
     p2[x3] := IntToByte(r - ((r) * a) div 255);
     p2[x3 + 1] := IntToByte(g - ((g) * a) div 255);
     p2[x3 + 2] := IntToByte(b - ((b) * a) div 255);
    end;
   for x := 0 to y do
    begin
     x3 := x * 3;
     w3 := (w - 1 - x) * 3;
     // lighter left
     r := p1[x3];
     g := p1[x3 + 1];
     b := p1[x3 + 2];
     p1[x3] := IntToByte(r + ((255 - r) * a) div 255);
     p1[x3 + 1] := IntToByte(g + ((255 - g) * a) div 255);
     p1[x3 + 2] := IntToByte(b + ((255 - b) * a) div 255);
     // darker right
     r := p1[w3];
     g := p1[w3 + 1];
     b := p1[w3 + 2];
     p1[w3] := IntToByte(r - ((r) * a) div 255);
     p1[w3 + 1] := IntToByte(g - ((g) * a) div 255);
     p1[w3 + 2] := IntToByte(b - ((b) * a) div 255);
     // lighter bottom left
     r := p2[x3];
     g := p2[x3 + 1];
     b := p2[x3 + 2];
     p2[x3] := IntToByte(r + ((255 - r) * a) div 255);
     p2[x3 + 1] := IntToByte(g + ((255 - g) * a) div 255);
     p2[x3 + 2] := IntToByte(b + ((255 - b) * a) div 255);
     // darker bottom right
     r := p2[w3];
     g := p2[w3 + 1];
     b := p2[w3 + 2];
     p2[w3] := IntToByte(r - ((r) * a) div 255);
     p2[w3 + 1] := IntToByte(g - ((g) * a) div 255);
     p2[w3 + 2] := IntToByte(b - ((b) * a) div 255);
    end;
  end;
 for y := depth + 1 to h - 2 - depth do
  begin
   p1 := src.ScanLine[y];
   for x := 0 to depth do
    begin
     x3 := x * 3;
     w3 := (w - 1 - x) * 3;
     // lighter left
     r := p1[x3];
     g := p1[x3 + 1];
     b := p1[x3 + 2];
     p1[x3] := IntToByte(r + ((255 - r) * a) div 255);
     p1[x3 + 1] := IntToByte(g + ((255 - g) * a) div 255);
     p1[x3 + 2] := IntToByte(b + ((255 - b) * a) div 255);
     // darker right
     r := p1[w3];
     g := p1[w3 + 1];
     b := p1[w3 + 2];
     p1[w3] := IntToByte(r - ((r) * a) div 255);
     p1[w3 + 1] := IntToByte(g - ((g) * a) div 255);
     p1[w3 + 2] := IntToByte(b - ((b) * a) div 255);
    end;
  end;
end;


//TODO: ButtonizeOval uses `with contour.canvas do` which violates coding conventions. Refactor to explicit `contour.canvas.xxx` calls.
procedure ButtonizeOval(src: TBitmap; depth: byte; weight: Integer; rim: string);
var
 p0, p1, p2, P3: pbytearray;
 w, h, i, x, x3, y: Integer;
 // am,amount:extended;
 fac, a, r, g, b, r2, g2: Integer;
 contour: TBitmap;
 // biclight,
 // bicdark,bicnone:byte;
 // act:boolean;
begin
 x3 := 0; // Suppresses compiler warning. x3 is always set before use in the loops below.
 a := weight;
 w := src.Width;
 h := src.Height;
 contour := TBitmap.create;
 contour.Width := w;
 contour.Height := h;
 contour.PixelFormat := pf24bit;
 contour.canvas.Brush.color := clwhite;
 contour.canvas.FillRect(rect(0, 0, w, h));
 with contour.canvas do begin
   pen.Width := 1;
   pen.style := pssolid;
   for i := 0 to depth - 1 do begin
     if rim = 'rimmed' then begin
       // (bottom-right)
       pen.color := rgb($00, $02, i);
       Arc(i, i, w - i, h - i, // ellipse
             0, h, // start
             w, 0); // end
       // (top-left)
       pen.color := rgb($00, $01, i);
       Arc(i, i, w - i, h - i, // ellipse
             w, 0, // start
             0, h); // end
      end
     else
      if (rim = 'round') or (rim = 'doubleround') then begin
        // (bottom-right)
        pen.color := rgb($00, $01, depth - 1 - i);
        Arc(i, i, w - i, h - i, // ellipse
              0, h, // start
              w, 0); // end
        // (top-left)
        pen.color := rgb($00, $02, depth - 1 - i);
        Arc(i, i, w - i, h - i, // ellipse
              w, 0, // start
              0, h); // end
       end;
    end;
   if rim = 'doubleround' then
    for i := depth to depth - 1 + depth do begin
      // (bottom-right)
      pen.color := rgb($00, $02, i);
      Arc(i, i, w - i, h - i, // ellipse
            0, h, // start
            w, 0); // end
      // (top-left)
      pen.color := rgb($00, $01, i);
      Arc(i, i, w - i, h - i, // ellipse
            w, 0, // start
            0, h); // end
     end;
  end;
 src.PixelFormat := pf24bit;

 for y := 0 to h - 1 do
  begin
   p1 := src.ScanLine[y];
   p2 := contour.ScanLine[y];
   for x := 0 to w - 1 do
    begin
     x3 := x * 3;
     r := p1[x3];
     g := p1[x3 + 1];
     b := p1[x3 + 2];
     r2 := p2[x3];
     g2 := p2[x3 + 1];
     // b2:= p2[x3+2];   //  value assigned, never used
     fac := Trunc(r2 / depth * a);
     if g2 = $02 then
      begin // lighter
       p1[x3] := IntToByte(r + ((255 - r) * fac) div 255);
       p1[x3 + 1] := IntToByte(g + ((255 - g) * fac) div 255);
       p1[x3 + 2] := IntToByte(b + ((255 - b) * fac) div 255);
      end
     else
      if g2 = $01 then
       begin // darker
        p1[x3] := IntToByte(r - ((r) * fac) div 255);
        p1[x3 + 1] := IntToByte(g - ((g) * fac) div 255);
        p1[x3 + 2] := IntToByte(b - ((b) * fac) div 255);
       end;
    end;
  end;

 // anti alias
 for y := 1 to h - 2 do begin
   p0 := src.ScanLine[y - 1];
   p1 := src.ScanLine[y];
   p2 := src.ScanLine[y + 1];
   P3 := contour.ScanLine[y];
   for x := 1 to w - 2 do begin
     g2 := P3[x * 3 + 1];
     if g2 <> $00 then begin
       p1[x * 3] := (p0[x * 3] + p2[x * 3] + p1[(x - 1) * 3] + p1[(x + 1) * 3]) div 4;
       p1[x * 3 + 1] := (p0[x * 3 + 1] + p2[x * 3 + 1] + p1[(x - 1) * 3 + 1] + p1[(x + 1) * 3 + 1]) div 4;  { BUG FIX: was x3 (stale from previous loop) instead of x * 3 }
       p1[x * 3 + 2] := (p0[x * 3 + 2] + p2[x * 3 + 2] + p1[(x - 1) * 3 + 2] + p1[(x + 1) * 3 + 2]) div 4;
      end;
    end;
  end;
 FreeAndNil(contour);
end;


procedure MaskOval(src: TBitmap; acolor: TColor);
var
 p1, p2: pbytearray;
 w, h, x, x3, y: Integer;
 b2: Integer;
 mr, mg, mb: byte;
 contour: TBitmap;

begin
 acolor := colortoRGB(acolor);
 mr := GetRValue(acolor);
 mg := GetGValue(acolor);
 mb := GetBValue(acolor);
 w := src.Width;
 h := src.Height;
 contour := TBitmap.create;
 contour.Width := w;
 contour.Height := h;
 contour.PixelFormat := pf24bit;
 contour.canvas.Brush.color := clblack;
 contour.canvas.FillRect(rect(0, 0, w, h));
 contour.canvas.pen.color := clRed;
 contour.canvas.Brush.color := clRed;
 contour.canvas.Ellipse(0, 0, w, h);
 src.PixelFormat := pf24bit;
 for y := 0 to h - 1 do
  begin
   p1 := src.ScanLine[y];
   p2 := contour.ScanLine[y];
   for x := 0 to w - 1 do begin
     x3 := x * 3;
     // r:= p1[x3];
     // g:= p1[x3+1];   //  value assigned, never used
     // b:= p1[x3+2];   //  value assigned, never used
     // r2:= p2[x3];    //  value assigned, never used
     // g2:= p2[x3+1];  //  value assigned, never used
     b2 := p2[x3 + 2];
     if b2 = $00 then begin // mask
       p1[x3] := mb;
       p1[x3 + 1] := mg;
       p1[x3 + 2] := mr;
      end;
    end;
  end;
 FreeAndNil(contour);
end;




{-------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------}
{ The Ignore (basic) version of a 3 x 3 convolution.

  The 3 x 3 convolve uses the eight surrounding pixels as part of the calculation.  But, for the pixels on the edges, there is nothing to use for the top row values.
  In other words, the leftmost pixel in the 3rd row, or scanline, has no pixels on its left to use in the calculations.
  This version just ignores the outermost edge of the image, and doesn't alter those pixels at all.
  Repeated applications of filters will eventually cause a pronounced 'border' effect, as those pixels never change but all others do.
  However, this version is simpler, and the logic is easier to follow.
  It's the fastest of the three in this application, and works great if the 'borders' are not an issue. }
procedure ConvolveI(ray: array of Integer; z: word; aBmp: TBitmap);
var
 O, T, c, b: pRGBArray; // Scanlines
 x, y: Integer;
 tBufr: TBitmap; // temp bitmap
begin
 tBufr := TBitmap.create;
 CopyBMP(tBufr, aBmp);
 for x := 1 to aBmp.Height - 2 do begin // Walk scanlines
   O := aBmp.ScanLine[x]; // New Target (Original)
   T := tBufr.ScanLine[x - 1]; // old x-1  (Top)
   c := tBufr.ScanLine[x]; // old x    (Center)
   b := tBufr.ScanLine[x + 1]; // old x+1  (Bottom)
   // Now do the main piece
   for y := 1 to (tBufr.Width - 2) do begin // Walk pixels
     O[y].rgbtRed := Set255(((T[y - 1].rgbtRed * ray[0]) + (T[y].rgbtRed * ray[1]) + (T[y + 1].rgbtRed * ray[2]) +
           (c[y - 1].rgbtRed * ray[3]) + (c[y].rgbtRed * ray[4]) + (c[y + 1].rgbtRed * ray[5]) +
           (b[y - 1].rgbtRed * ray[6]) + (b[y].rgbtRed * ray[7]) + (b[y + 1].rgbtRed * ray[8])) div z);
     O[y].rgbtBlue := Set255(((T[y - 1].rgbtBlue * ray[0]) + (T[y].rgbtBlue * ray[1]) + (T[y + 1].rgbtBlue * ray[2]
           ) + (c[y - 1].rgbtBlue * ray[3]) + (c[y].rgbtBlue * ray[4]) + (c[y + 1].rgbtBlue * ray[5]) +
           (b[y - 1].rgbtBlue * ray[6]) + (b[y].rgbtBlue * ray[7]) + (b[y + 1].rgbtBlue * ray[8])) div z);
     O[y].rgbtGreen := Set255(((T[y - 1].rgbtGreen * ray[0]) + (T[y].rgbtGreen * ray[1]) +
           (T[y + 1].rgbtGreen * ray[2]) + (c[y - 1].rgbtGreen * ray[3]) + (c[y].rgbtGreen * ray[4]) +
           (c[y + 1].rgbtGreen * ray[5]) + (b[y - 1].rgbtGreen * ray[6]) + (b[y].rgbtGreen * ray[7]) +
           (b[y + 1].rgbtGreen * ray[8])) div z);
    end;
  end;
 FreeAndNil(tBufr);
end;


{ The Expand version of a 3 x 3 convolution.

  This approach is similar to the mirror version, except that it copies or duplicates the pixels from the edges to the same edge.
  This is probably the best version if you're interested in quality, but don't need a tiled (seamless) image. }
procedure ConvolveE(ray: array of Integer; z: word; aBmp: TBitmap);
var
 O, T, c, b: pRGBArray; // Scanlines
 x, y: Integer;
 tBufr: TBitmap; // temp bitmap for 'enlarged' image
begin
 tBufr := TBitmap.create;
 tBufr.Width := aBmp.Width + 2; // Add a box around the outside...
 tBufr.Height := aBmp.Height + 2;
 tBufr.PixelFormat := pf24bit;
 O := tBufr.ScanLine[0]; // Copy top corner pixels
 T := aBmp.ScanLine[0];
 O[0] := T[0]; // Left
 O[tBufr.Width - 1] := T[aBmp.Width - 1]; // Right
 // Copy top lines
 tBufr.canvas.CopyRect(rect(1, 0, tBufr.Width - 1, 1), aBmp.canvas, rect(0, 0, aBmp.Width, 1));

 O := tBufr.ScanLine[tBufr.Height - 1]; // Copy bottom corner pixels
 T := aBmp.ScanLine[aBmp.Height - 1];
 O[0] := T[0];
 O[tBufr.Width - 1] := T[aBmp.Width - 1];
 // Copy bottoms
 tBufr.canvas.CopyRect(rect(1, tBufr.Height - 1, tBufr.Width - 1, tBufr.Height), aBmp.Canvas, rect(0, aBmp.Height - 1, aBmp.Width, aBmp.Height));
 // Copy rights
 tBufr.canvas.CopyRect(rect(tBufr.Width - 1, 1, tBufr.Width, tBufr.Height - 1), aBmp.Canvas, rect(aBmp.Width - 1, 0, aBmp.Width, aBmp.Height));
 // Copy lefts
 tBufr.canvas.CopyRect(rect(0, 1, 1, tBufr.Height - 1), aBmp.canvas, rect(0, 0, 1, aBmp.Height));
 // Now copy main rectangle
 tBufr.canvas.CopyRect(rect(1, 1, tBufr.Width - 1, tBufr.Height - 1), aBmp.Canvas, rect(0, 0, aBmp.Width, aBmp.Height));
 // bmp now enlarged and copied, apply convolve
 for x := 0 to aBmp.Height - 1 do begin // Walk scanlines
   O := aBmp.ScanLine[x];       // New Target (Original)
   T := tBufr.ScanLine[x];      // old x-1  (Top)
   c := tBufr.ScanLine[x + 1];  // old x    (Center)
   b := tBufr.ScanLine[x + 2];  // old x+1  (Bottom)
   // Now do the main piece
   for y := 1 to (tBufr.Width - 2) do begin // Walk pixels
     O[y - 1].rgbtRed := Set255((
           (T[y - 1].rgbtRed * ray[0]) + (T[y].rgbtRed * ray[1]) + (T[y + 1].rgbtRed * ray[2]) +
           (c[y - 1].rgbtRed * ray[3]) + (c[y].rgbtRed * ray[4]) + (c[y + 1].rgbtRed * ray[5]) +
           (b[y - 1].rgbtRed * ray[6]) + (b[y].rgbtRed * ray[7]) + (b[y + 1].rgbtRed * ray[8])) div z);
     O[y - 1].rgbtBlue :=
           Set255((
           (T[y - 1].rgbtBlue * ray[0]) + (T[y].rgbtBlue * ray[1]) + (T[y + 1].rgbtBlue * ray[2]) +
           (c[y - 1].rgbtBlue * ray[3]) + (c[y].rgbtBlue * ray[4]) + (c[y + 1].rgbtBlue * ray[5]) +
           (b[y - 1].rgbtBlue * ray[6]) + (b[y].rgbtBlue * ray[7]) + (b[y + 1].rgbtBlue * ray[8])) div z);
     O[y - 1].rgbtGreen :=
           Set255((
           (T[y - 1].rgbtGreen * ray[0]) + (T[y].rgbtGreen * ray[1]) + (T[y + 1].rgbtGreen * ray[2]) +
           (c[y - 1].rgbtGreen * ray[3]) + (c[y].rgbtGreen * ray[4]) + (c[y + 1].rgbtGreen * ray[5]) +
           (b[y - 1].rgbtGreen * ray[6]) + (b[y].rgbtGreen * ray[7]) + (b[y + 1].rgbtGreen * ray[8])) div z);
    end;
  end;
 FreeAndNil(tBufr);
end;


{ The mirror version of a 3 x 3 convolution.

  The 3 x 3 convolve uses the eight surrounding pixels as part of the calculation.  But, for the pixels on the edges, there is nothing to use for the top row values.
  In other words, the leftmost pixel in the 3rd row, or scanline, has no pixels on its left to use in the calculations.
  I compensate for this by increasing the size of the bitmap by one pixel on top, left, bottom, and right.
  The mirror version is used in an application that creates seamless tiles, so I copy the opposite sides to maintain the seamless integrity. }
procedure ConvolveM(ray: array of Integer; z: word; aBmp: TBitmap);
var
 O, T, c, b: pRGBArray; // Scanlines
 x, y: Integer;
 tBufr: TBitmap; // temp bitmap for 'enlarged' image
begin
 tBufr := TBitmap.create;
 tBufr.Width := aBmp.Width + 2; // Add a box around the outside...
 tBufr.Height := aBmp.Height + 2;
 tBufr.PixelFormat := pf24bit;
 O := tBufr.ScanLine[0]; // Copy top corner pixels
 T := aBmp.ScanLine[0];
 O[0] := T[0]; // Left
 O[tBufr.Width - 1] := T[aBmp.Width - 1]; // Right
 // Copy bottom line to our top - trying to remain seamless...
 tBufr.canvas.CopyRect(rect(1, 0, tBufr.Width - 1, 1), aBmp.Canvas, rect(0, aBmp.Height - 1, aBmp.Width, aBmp.Height - 2));

 O := tBufr.ScanLine[tBufr.Height - 1]; // Copy bottom corner pixels
 T := aBmp.ScanLine[aBmp.Height - 1];
 O[0] := T[0];
 O[tBufr.Width - 1] := T[aBmp.Width - 1];
 // Copy top line to our bottom
 tBufr.canvas.CopyRect(rect(1, tBufr.Height - 1, tBufr.Width - 1, tBufr.Height), aBmp.Canvas, rect(0, 0, aBmp.Width, 1));
 // Copy left to our right
 tBufr.canvas.CopyRect(rect(tBufr.Width - 1, 1, tBufr.Width, tBufr.Height - 1), aBmp.Canvas, rect(0, 0, 1, aBmp.Height));
 // Copy right to our left
 tBufr.canvas.CopyRect(rect(0, 1, 1, tBufr.Height - 1), aBmp.Canvas, rect(aBmp.Width - 1, 0, aBmp.Width, aBmp.Height));
 // Now copy main rectangle
 tBufr.canvas.CopyRect(rect(1, 1, tBufr.Width - 1, tBufr.Height - 1), aBmp.Canvas, rect(0, 0, aBmp.Width, aBmp.Height));
 // bmp now enlarged and copied, apply convolve
 for x := 0 to aBmp.Height - 1 do
 begin // Walk scanlines
   O := aBmp.ScanLine[x]; // New Target (Original)
   T := tBufr.ScanLine[x]; // old x-1  (Top)
   c := tBufr.ScanLine[x + 1]; // old x    (Center)
   b := tBufr.ScanLine[x + 2]; // old x+1  (Bottom)
   // Now do the main piece
   for y := 1 to (tBufr.Width - 2) do begin // Walk pixels
     O[y - 1].rgbtRed := Set255(((T[y - 1].rgbtRed * ray[0]) + (T[y].rgbtRed * ray[1]) + (T[y + 1].rgbtRed * ray[2]
           ) + (c[y - 1].rgbtRed * ray[3]) + (c[y].rgbtRed * ray[4]) + (c[y + 1].rgbtRed * ray[5]) +
           (b[y - 1].rgbtRed * ray[6]) + (b[y].rgbtRed * ray[7]) + (b[y + 1].rgbtRed * ray[8])) div z);
     O[y - 1].rgbtBlue :=
           Set255(((T[y - 1].rgbtBlue * ray[0]) + (T[y].rgbtBlue * ray[1]) + (T[y + 1].rgbtBlue * ray[2]) +
           (c[y - 1].rgbtBlue * ray[3]) + (c[y].rgbtBlue * ray[4]) + (c[y + 1].rgbtBlue * ray[5]) +
           (b[y - 1].rgbtBlue * ray[6]) + (b[y].rgbtBlue * ray[7]) + (b[y + 1].rgbtBlue * ray[8])) div z);
     O[y - 1].rgbtGreen :=
           Set255(((T[y - 1].rgbtGreen * ray[0]) + (T[y].rgbtGreen * ray[1]) + (T[y + 1].rgbtGreen * ray[2]) +
           (c[y - 1].rgbtGreen * ray[3]) + (c[y].rgbtGreen * ray[4]) + (c[y + 1].rgbtGreen * ray[5]) +
           (b[y - 1].rgbtGreen * ray[6]) + (b[y].rgbtGreen * ray[7]) + (b[y + 1].rgbtGreen * ray[8])) div z);
    end;
  end;
 FreeAndNil(tBufr);
end;


procedure ConvolveFilter(FilterType, EdgeType: Integer; src: TBitmap);
var
  z: Integer;
  ray: array [0 .. 8] of Integer;
  OrigBMP: TBitmap; // Bitmap for temporary use
begin
 // z :=  1;  // just to avoid compiler warnings!

 // Fill filter with data
 case FilterType of
  0: begin // Laplace
      ray[0] := -1;
      ray[1] := -1;
      ray[2] := -1;
      ray[3] := -1;
      ray[4] :=  8;
      ray[5] := -1;
      ray[6] := -1;
      ray[7] := -1;
      ray[8] := -1;
      z := 1;
    end;
  1: begin // Hipass
      ray[0] := -1;
      ray[1] := -1;
      ray[2] := -1;
      ray[3] := -1;
      ray[4] := 9;
      ray[5] := -1;
      ray[6] := -1;
      ray[7] := -1;
      ray[8] := -1;
      z := 1;
     end;
  2: begin // Find Edges (top down)
      ray[0] := 1;
      ray[1] := 1;
      ray[2] := 1;
      ray[3] := 1;
      ray[4] := -2;
      ray[5] := 1;
      ray[6] := -1;
      ray[7] := -1;
      ray[8] := -1;
      z := 1;
     end;
  3: begin // Sharpen
      ray[0] := -1;
      ray[1] := -1;
      ray[2] := -1;
      ray[3] := -1;
      ray[4] := 16;
      ray[5] := -1;
      ray[6] := -1;
      ray[7] := -1;
      ray[8] := -1;
      z := 8;
     end;
  4: begin // Edge Enhance
      ray[0] := 0;
      ray[1] := -1;
      ray[2] := 0;
      ray[3] := -1;
      ray[4] := 5;
      ray[5] := -1;
      ray[6] := 0;
      ray[7] := -1;
      ray[8] := 0;
      z := 1;
     end;
  5: begin // Color Emboss (Sorta)
      ray[0] := 1;
      ray[1] := 0;
      ray[2] := 1;
      ray[3] := 0;
      ray[4] := 0;
      ray[5] := 0;
      ray[6] := 1;
      ray[7] := 0;
      ray[8] := -2;
      z := 1;
     end;
  6: begin // Soften
      ray[0] := 2;
      ray[1] := 2;
      ray[2] := 2;
      ray[3] := 2;
      ray[4] := 0;
      ray[5] := 2;
      ray[6] := 2;
      ray[7] := 2;
      ray[8] := 2;
      z := 16;
     end;
  7: begin // Blur
      ray[0] := 3;
      ray[1] := 3;
      ray[2] := 3;
      ray[3] := 3;
      ray[4] := 8;
      ray[5] := 3;
      ray[6] := 3;
      ray[7] := 3;
      ray[8] := 3;
      z := 32;
     end;
  8: begin // Soften less
      ray[0] := 0;
      ray[1] := 1;
      ray[2] := 0;
      ray[3] := 1;
      ray[4] := 2;
      ray[5] := 1;
      ray[6] := 0;
      ray[7] := 1;
      ray[8] := 0;
      z := 6;
     end;
  else Exit;  //raise
 end;

 OrigBMP := TBitmap.create; // Copy image to 24-bit bitmap
 CopyBMP(OrigBMP, src);

 case EdgeType of
  0: ConvolveM(ray, z, OrigBMP);
  1: ConvolveE(ray, z, OrigBMP);
  2: ConvolveI(ray, z, OrigBMP);
 end;

 src.assign(OrigBMP); // Assign filtered image to Image1
 FreeAndNil(OrigBMP);
end;









{-------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------}
procedure Seamless(src: TBitmap; depth: byte);
var
 p1, p2: pbytearray;
 w, w3, h, x, x3, y: Integer;
 am, amount: extended;
begin
 w := src.Width;
 h := src.Height;
 src.PixelFormat := pf24bit;
 if depth = 0 then
  Exit;
 am := 1 / depth;
 for y := 0 to depth do
  begin
   p1 := src.ScanLine[y];
   p2 := src.ScanLine[h - y - 1];
   amount := 1 - y * am;
   for x := y to w - 1 - y do
    begin
     x3 := x * 3;
     p2[x3] := Round((1 - amount) * p2[x3] + amount * p1[x3]);
     p2[x3 + 1] := Round((1 - amount) * p2[x3 + 1] + amount * p1[x3 + 1]);
     p2[x3 + 2] := Round((1 - amount) * p2[x3 + 2] + amount * p1[x3 + 2]);
    end;
   for x := 0 to y do
    begin
     amount := 1 - x * am;
     x3 := x * 3;
     w3 := (w - 1 - x) * 3;
     p1[w3] := Round((1 - amount) * p1[w3] + amount * p1[x3]);
     p1[w3 + 1] := Round((1 - amount) * p1[w3 + 1] + amount * p1[x3 + 1]);
     p1[w3 + 2] := Round((1 - amount) * p1[w3 + 2] + amount * p1[x3 + 2]);
     p2[w3] := Round((1 - amount) * p2[w3] + amount * p2[x3]);
     p2[w3 + 1] := Round((1 - amount) * p2[w3 + 1] + amount * p2[x3 + 1]);
     p2[w3 + 2] := Round((1 - amount) * p2[w3 + 2] + amount * p2[x3 + 2]);
    end;
  end;
 for y := depth to h - 1 - depth do
  begin
   p1 := src.ScanLine[y];
   for x := 0 to depth do
    begin
     x3 := x * 3;
     w3 := (w - 1 - x) * 3;
     amount := 1 - x * am;
     p1[w3] := Round((1 - amount) * p1[w3] + amount * p1[x3]);
     p1[w3 + 1] := Round((1 - amount) * p1[w3 + 1] + amount * p1[x3 + 1]);
     p1[w3 + 2] := Round((1 - amount) * p1[w3 + 2] + amount * p1[x3 + 2]);
    end;
  end;
end;



//TODO: SmoothPoint uses Canvas.Pixels which is extremely slow. Rewrite with ScanLine.
procedure SmoothPoint(clip: TBitmap; xk, yk: Integer);   { Averages 4 neighboring pixels (up/down/left/right) to smooth a single point. Uses slow Canvas.Pixels. }
VAR
 Bleu, Vert, Rouge, w, h: Integer;
 acolor: TColor;
 BB, gg, RR: array [1 .. 5] of Integer;
begin
 w := clip.Width;
 h := clip.Height;
 if (xk > 0) AND (yk > 0) AND (xk < w - 1) and (yk < h - 1) then
  with clip.canvas DO
   begin
    acolor := colortoRGB(Pixels[xk, yk - 1]);
    RR[1] := GetRValue(acolor);
    gg[1] := GetGValue(acolor);
    BB[1] := GetBValue(acolor);

    acolor := colortoRGB(Pixels[xk + 1, yk]);
    RR[2] := GetRValue(acolor);
    gg[2] := GetGValue(acolor);
    BB[2] := GetBValue(acolor);

    acolor := colortoRGB(Pixels[xk, yk + 1]);
    RR[3] := GetRValue(acolor);
    gg[3] := GetGValue(acolor);
    BB[3] := GetBValue(acolor);

    acolor := colortoRGB(Pixels[xk - 1, yk]);
    RR[4] := GetRValue(acolor);
    gg[4] := GetGValue(acolor);
    BB[4] := GetBValue(acolor);

    Bleu := (BB[1] + BB[2] + BB[3] + BB[4]) div 4; (* Valeur moyenne *)
    Vert := (gg[1] + gg[2] + gg[3] + gg[4]) div 4; (* en cours d'evaluation *)
    Rouge := (RR[1] + RR[2] + RR[3] + RR[4]) div 4;
    Pixels[xk, yk] := rgb(Rouge, Vert, Bleu);
   end;
end;


procedure AntiAlias(clip: TBitmap);
begin
 AntiAliasRect(clip, 0, 0, clip.Width, clip.Height);
end;


procedure AntiAliasRect(clip: TBitmap; XOrigin, YOrigin, XFinal, YFinal: Integer);
VAR
  Memo, x, y: Integer; (* Composantes primaires des points environnants *)
  p0, p1, p2: pbytearray;
begin
 if XFinal < XOrigin then
 begin
   Memo := XOrigin;
   XOrigin := XFinal;
   XFinal := Memo;
  end; (* Inversion des valeurs *)

 if YFinal < YOrigin then
 begin
   Memo := YOrigin;
   YOrigin := YFinal;
   YFinal := Memo;
  end; (* si difference negative *)

 XOrigin := Max(1, XOrigin);
 YOrigin := Max(1, YOrigin);
 XFinal := Min(clip.Width - 2, XFinal);
 YFinal := Min(clip.Height - 2, YFinal);
 clip.PixelFormat := pf24bit;

 for y := YOrigin to YFinal DO
 begin
   p0 := clip.ScanLine[y - 1];
   p1 := clip.ScanLine[y];
   p2 := clip.ScanLine[y + 1];
   for x := XOrigin to XFinal do
   begin
     p1[x * 3] := (p0[x * 3] + p2[x * 3] + p1[(x - 1) * 3] + p1[(x + 1) * 3]) div 4;
     p1[x * 3 + 1] := (p0[x * 3 + 1] + p2[x * 3 + 1] + p1[(x - 1) * 3 + 1] + p1[(x + 1) * 3 + 1]) div 4;
     p1[x * 3 + 2] := (p0[x * 3 + 2] + p2[x * 3 + 2] + p1[(x - 1) * 3 + 2] + p1[(x + 1) * 3 + 2]) div 4;
    end;
  end;
end;








{ MARBLE }
//TODO: marble1-8 are nearly identical (differ only in trig formula). Refactor into a single parameterized procedure.

procedure marble(src, dst: TBitmap; scale: extended; turbulence: Integer);
var
 x, xm, y, ym: Integer;
 xx, yy: extended;
 p1, p2: pbytearray;
 w, h: Integer;
begin
 if (turbulence = 0) or (scale = 0) then EXIT;  { Avoid division by zero in mod/div }
 h := src.Height;
 w := src.Width;
 dst.Width := w;
 dst.Height := h;
 dst.canvas.Draw(0, 0, src);
 for y := 0 to h - 1 do begin
   yy := scale * cos((y mod turbulence) / scale);

   p1 := src.ScanLine[y];
   for x := 0 to w - 1 do begin
     xx := -scale * sin((x mod turbulence) / scale);
     xm := Round(Abs(x + xx + yy));
     ym := Round(Abs(y + yy + xx));
     if ym < h then begin
       p2 := dst.ScanLine[ym];
       if xm < w then begin
         p2[xm * 3] := p1[x * 3];
         p2[xm * 3 + 1] := p1[x * 3 + 1];
         p2[xm * 3 + 2] := p1[x * 3 + 2];
        end;
      end;
    end;
  end;
end;


procedure marble2(src, dst: TBitmap; scale: extended; turbulence: Integer);
var
 x, xm, y, ym: Integer;
 xx, yy: extended;
 p1, p2: pbytearray;
 w, h: Integer;
begin
 if (turbulence = 0) or (scale = 0) then EXIT;  { Avoid division by zero }
 h := src.Height;
 w := src.Width;
 dst.assign(src);
 for y := 0 to h - 1 do begin
   yy := scale * cos((y mod turbulence) / scale);

   p1 := src.ScanLine[y];
   for x := 0 to w - 1 do begin
     xx := -scale * sin((x mod turbulence) / scale);
     xm := Round(Abs(x + xx - yy));
     ym := Round(Abs(y + yy - xx));
     if ym < h then begin
       p2 := dst.ScanLine[ym];
       if xm < w then begin
         p2[xm * 3] := p1[x * 3];
         p2[xm * 3 + 1] := p1[x * 3 + 1];
         p2[xm * 3 + 2] := p1[x * 3 + 2];
        end;
      end;
    end;
  end;
end;


procedure marble3(src, dst: TBitmap; scale: extended; turbulence: Integer);
var
 x, xm, y, ym: Integer;
 xx, yy: extended;
 p1, p2: pbytearray;
 w, h: Integer;
begin
 if (turbulence = 0) or (scale = 0) then EXIT;  { Avoid division by zero }
 h := src.Height;
 w := src.Width;
 dst.assign(src);
 for y := 0 to h - 1 do
  begin
   yy := scale * cos((y mod turbulence) / scale);

   p1 := src.ScanLine[y];
   for x := 0 to w - 1 do
    begin
     xx := -scale * sin((x mod turbulence) / scale);
     xm := Round(Abs(x - xx + yy));
     ym := Round(Abs(y - yy + xx));
     if ym < h then
      begin
       p2 := dst.ScanLine[ym];
       if xm < w then
        begin
         p2[xm * 3] := p1[x * 3];
         p2[xm * 3 + 1] := p1[x * 3 + 1];
         p2[xm * 3 + 2] := p1[x * 3 + 2];
        end;
      end;
    end;
  end;
end;


procedure marble4(src, dst: TBitmap; scale: extended; turbulence: Integer);
var
 x, xm, y, ym: Integer;
 xx, yy: extended;
 p1, p2: pbytearray;
 w, h: Integer;
begin
 if (turbulence = 0) or (scale = 0) then EXIT;  { Avoid division by zero }
 h := src.Height;
 w := src.Width;
 dst.assign(src);
 for y := 0 to h - 1 do begin
   yy := scale * sin((y mod turbulence) / scale);

   p1 := src.ScanLine[y];
   for x := 0 to w - 1 do begin
     xx := -scale * cos((x mod turbulence) / scale);
     xm := Round(Abs(x + xx + yy));
     ym := Round(Abs(y + yy + xx));
     if ym < h then begin
       p2 := dst.ScanLine[ym];
       if xm < w then begin
         p2[xm * 3] := p1[x * 3];
         p2[xm * 3 + 1] := p1[x * 3 + 1];
         p2[xm * 3 + 2] := p1[x * 3 + 2];
        end;
      end;
    end;
  end;
end;


procedure marble5(src, dst: TBitmap; scale: extended; turbulence: Integer);
var
 x, xm, y, ym: Integer;
 xx, yy: extended;
 p1, p2: pbytearray;
 w, h: Integer;
begin
 if (turbulence = 0) or (scale = 0) then EXIT;  { Avoid division by zero }
 h := src.Height;
 w := src.Width;
 dst.assign(src);
 for y := h - 1 downto 0 do begin
   yy := scale * cos((y mod turbulence) / scale);

   p1 := src.ScanLine[y];
   for x := w - 1 downto 0 do begin
     xx := -scale * sin((x mod turbulence) / scale);
     xm := Round(Abs(x + xx + yy));
     ym := Round(Abs(y + yy + xx));
     if ym < h then begin
       p2 := dst.ScanLine[ym];
       if xm < w then begin
         p2[xm * 3] := p1[x * 3];
         p2[xm * 3 + 1] := p1[x * 3 + 1];
         p2[xm * 3 + 2] := p1[x * 3 + 2];
        end;
      end;
    end;
  end;
end;


procedure marble6(src, dst: TBitmap; scale: extended; turbulence: Integer);
var
 x, xm, y, ym: Integer;
 xx, yy: extended;
 p1, p2: pbytearray;
 w, h: Integer;
begin
 if (turbulence = 0) or (scale = 0) then EXIT;  { Avoid division by zero }
 h := src.Height;
 w := src.Width;
 dst.assign(src);
 for y := 0 to h - 1 do begin
   yy := scale * cos((y mod turbulence) / scale);

   p1 := src.ScanLine[y];
   for x := 0 to w - 1 do begin
     xx := -tan((x mod turbulence) / scale) / scale;
     xm := Round(Abs(x + xx + yy));
     ym := Round(Abs(y + yy + xx));
     if ym < h then begin
       p2 := dst.ScanLine[ym];
       if xm < w then begin
         p2[xm * 3] := p1[x * 3];
         p2[xm * 3 + 1] := p1[x * 3 + 1];
         p2[xm * 3 + 2] := p1[x * 3 + 2];
        end;
      end;
    end;
  end;
end;


procedure marble7(src, dst: TBitmap; scale: extended; turbulence: Integer);
var
 x, xm, y, ym: Integer;
 xx, yy: extended;
 p1, p2: pbytearray;
 w, h: Integer;
begin
 if (turbulence = 0) or (scale = 0) then EXIT;  { Avoid division by zero }
 h := src.Height;
 w := src.Width;
 dst.assign(src);
 for y := 0 to h - 1 do begin
   yy := scale * sin((y mod turbulence) / scale);

   p1 := src.ScanLine[y];
   for x := 0 to w - 1 do begin
     xx := -tan((x mod turbulence) / scale) / (scale * scale);
     xm := Round(Abs(x + xx + yy));
     ym := Round(Abs(y + yy + xx));
     if ym < h then begin
       p2 := dst.ScanLine[ym];
       if xm < w then begin
         p2[xm * 3] := p1[x * 3];
         p2[xm * 3 + 1] := p1[x * 3 + 1];
         p2[xm * 3 + 2] := p1[x * 3 + 2];
        end;
      end;
    end;
  end;
end;


procedure marble8(src, dst: TBitmap; scale: extended; turbulence: Integer);
var
 x, xm, y, ym: Integer;
 xx, yy: extended;
 p1, p2: pbytearray;
 w, h: Integer;
 ax: extended;
begin
 if (turbulence = 0) or (scale = 0) then EXIT;  { Avoid division by zero }
 h := src.Height;
 w := src.Width;
 dst.assign(src);
 for y := 0 to h - 1 do
 begin
   ax := (y mod turbulence) / scale;
   yy := scale * sin(ax) * cos(1.5 * ax);
   p1 := src.ScanLine[y];
   for x := 0 to w - 1 do
   begin
     ax := (x mod turbulence) / scale;
     xx := -scale * sin(2 * ax) * cos(ax);
     xm := Round(Abs(x + xx + yy));
     ym := Round(Abs(y + yy + xx));
     if ym < h then
     begin
       p2 := dst.ScanLine[ym];
       if xm < w then
       begin
         p2[xm * 3] := p1[x * 3];
         p2[xm * 3 + 1] := p1[x * 3 + 1];
         p2[xm * 3 + 2] := p1[x * 3 + 2];
        end;
      end;
    end;
  end;
end;







{ FRACTALS }
procedure DrawMandelJulia(src: TBitmap; x0, y0, x1, y1: extended; Niter: Integer; Mandel: Boolean);
const
 // Number if colors. If this is changed, the number of mapped colors must also be changed
 nc = 16;
type
 TjvRGBTriplet = record
 r, g, b: byte end;

 var
  x, xx, y, yy, cx, Cy, dx, dy, XSquared, YSquared: Double;
  Nx, Ny, py, px, i: Integer;
  p0: pbytearray;
  cc: array [0 .. 15] of TjvRGBTriplet;
  acolor: TColor;
begin
 src.PixelFormat := pf24bit;
 for i := 0 to 15 do begin
   acolor := ConvertColor(i);
   cc[i].b := GetBValue(colortoRGB(acolor));
   cc[i].g := GetGValue(colortoRGB(acolor));
   cc[i].r := GetRValue(colortoRGB(acolor));
  end;

 if Niter < nc
 then Niter := nc;

 Nx := src.Width;
 Ny := src.Height;
 cx := 0;
 Cy := 1;
 dx := (x1 - x0) / Nx;
 dy := (y1 - y0) / Ny;
 py := 0;
 while (py < Ny) do begin
   p0 := src.ScanLine[py];
   px := 0;
   while (px < Nx) do begin
     x := x0 + px * dx;
     y := y0 + py * dy;
     if (Mandel) then begin
       cx := x;
       Cy := y;
       x := 0;
       y := 0;
      end;
     XSquared := 0;
     YSquared := 0;
     i := 0;
     while (i <= Niter) and (XSquared + YSquared < (4)) do begin
       XSquared := x * x;
       YSquared := y * y;
       xx := XSquared - YSquared + cx;
       yy := (2 * x * y) + Cy;
       x := xx;
       y := yy;
       i := i + 1;
      end;
     i := i - 1;
     if (i = Niter)
     then i := 0
     else i := Min(nc - 1, Round(i / (Niter / nc)));  { Clamp to valid cc[] range 0..15 }
     // Canvas.Pixels[PX,PY] :=  ConvertColor(I);
     p0[px * 3] := cc[i].b;
     p0[px * 3 + 1] := cc[i].g;
     p0[px * 3 + 2] := cc[i].r;
     px := px + 1;
    end;
   py := py + 1;
  end;

end;


procedure MandelBrot(src: TBitmap; factor: Integer);
const
 maxX = 1.25;
 minX = -2;
 maxY = 1.25;
 minY = -1.25;
var
 w, h, x, y: Integer;
 dx, dy: extended;
 p0: pbytearray;
 color: Integer;
 // ylo,yhi:extended;

 FUNCTION IsMandel(CA, CBi: extended): Integer;
 const
  MAX_ITERATION = 64;

 VAR
  OLD_A: extended; { just a variable to keep 'a' from being destroyed }
  a, b: extended; { function Z divided in real and imaginary parts }
  LENGTH_Z: extended; { length of Z, sqrt(length_z)>2 => Z->infinity }
  iteration: Integer;
 BEGIN
  a := 0; { initialize Z(0) = 0 }
  b := 0;
  iteration := 0; { initialize iteration }
  REPEAT
   OLD_A := a; { saves the 'a'  (Will be destroyed in next line }

   a := a * a - b * b + CA;
   b := 2 * OLD_A * b + CBi;
   iteration := iteration + 1;
   LENGTH_Z := a * a + b * b;
  UNTIL (LENGTH_Z >= 4) OR (iteration > MAX_ITERATION);
  Result := iteration;
 END;

begin
 w := src.Width;
 h := src.Height;
 src.PixelFormat := pf24bit;
 dx := (maxX - minX) / w;
 dy := (maxY - minY) / h;
 for y := 0 to h - 1 do begin
   p0 := src.ScanLine[y];
   for x := 0 to w - 1 do begin
     color := IsMandel(minX + x * dx, minY + y * dy);
     if color > factor then
      color := $FF
     else
      color := $00;
     p0[x * 3] := color;
     p0[x * 3 + 1] := color;
     p0[x * 3 + 2] := color;
    end;
  end;
end;


procedure MaskMandelBrot(src: TBitmap; factor: Integer);
var
 Bm: TBitmap;
begin
 Bm := TBitmap.create;
 Bm.Width := src.Width;
 Bm.Height := src.Height;
 MandelBrot(Bm, factor);
 Bm.transparent := true;
 Bm.TransparentColor := clwhite;
 src.canvas.Draw(0, 0, Bm);
 FreeAndNil(Bm);
end;


end.


{ function RGB2Color(Color: TColorRGB): TColor;
  begin
  Result:= Color.r OR (Color.g SHL 8) OR (Color.b SHL 16);
  end;

  function Color2RGB(Color: TColor): TColorRGB;
  begin
  Result.R:=   Color AND $000000FF;
  Result.G:=  (Color AND $0000FF00) SHR 8;
  Result.B:=  (Color AND $00FF0000) SHR 16;
  end; }


