UNIT cGraphUtil;

{=============================================================================================================
   Gabriel Moraru
   2024.06
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------

 Features:
   * Replace color
   * Lighten/darken color
   * Split color to RGB
   * Antialised line
   * Capture control canvas to BMP
   * Themes and styles

 External dependencies:
   * None

 Why is the canvas hidden?
    https://stackoverflow.com/questions/65050463/why-canvas-is-hidden-in-all-vcl-controls

 Also exists (Windows.pas):
    function GetRValue(rgb: DWORD): Byte;
    function GetGValue(rgb: DWORD): Byte;
    function GetBValue(rgb: DWORD): Byte;

 Also check Embarcadero's GraphUtil.pas

 Speed test for ScanLine
    SpeedTest: ScanLine vs Pixels[]
    http://ksymeon.blogspot.com.es/2010/02/getdibits-vs-scanline-vs-pixels-in.html
    Scanline is 140x faster than Pixels!!!

 TESTER:
    c:\MyProjects\LightSaber\cGraphUtil ReplaceColor\
--------------------------------------------------------------------------------------------------}

INTERFACE

USES
   Winapi.Windows,
   System.SysUtils, System.Classes, System.Types, System.Math,
   Vcl.Graphics, Vcl.Controls, Vcl.Imaging.PngImage, Vcl.Themes, Vcl.GraphUtil;

CONST
  JPG             = '*.jpg;*.jpeg;*.jpe;*.jp;*.jfif';
  JPGFtl          = 'JPEG|'+ JPG;

  JPG2k           = '*.J2K;*JPC;*.JP2';
  JPG2kFtl        = 'JPEG 2000|'+ JPG2k;

  JPGAll          = JPG+ ';'+ JPG2k;
  JPGAllFlt       = JPGFtl+ '|'+ JPG2kFtl;

  BmpType         = '*.BMP';   // Also you can use: GraphicExtension(TBitmap)
  BMPFtl          = 'Bitmaps|'+ BmpType;

  PNG             = '*.PNG';
  PNGFtl          = 'PNG|'+ PNG;

  GIF             = '*.GIF';
  GIFFtl          = 'GIF|'+ GIF;

  XMF             = '*.EMF;*.WMF';
  XMFFtl          = 'Microsoft EMF/WMF|'+ XMF;

  AllImg          = JPG+ ';'+ JPG2k+ ';'+ BmpType+ ';'+ PNG+ ';'+ GIF;
  AllImgFlt       = 'Images|'+ AllImg;

  AllImgFltLong   =  AllImgFlt+ '|'+ JPGFtl+ '|'+ JPG2kFtl+ '|'+ BMPFtl+ '|'+ PNGFtl+ '|'+ GIFFtl+ '|'+ XMFFtl;


TYPE

  { Scan line for pf24 images }
  TRGB24 = packed record
    B, G, R: Byte;
  end;
  TRGB24Array = packed array[0..MaxInt div SizeOf(TRGB24)-1] of TRGB24;
  PRGB24Array = ^TRGB24Array;


  { Scan line for pf32 images
    Also see System.SysUtils.PByteArray }
  TRGB32 = packed record
    B, G, R, A: Byte;
  end;
  TRGB32Array = packed array[0..MaxInt div SizeOf(TRGB32)-1] of TRGB32;  // some use MaxInt instead of MaxWord
  PRGB32Array = ^TRGB32Array;


  { We give this to a function that want to paint a text on screen. Instead of providing 4 parameters, we provide just this record (that we pre-fill) }
  TTextWithAttrib= record
    Text     : string;
    TextColor: TColor;
    BkgColor : TColor;
    FontSize : Integer;
  end;


{-------------------------------------------------------------------------------------------------------------
   Color
-------------------------------------------------------------------------------------------------------------}
 function  DarkenColor         (aColor: TColor; Percent: Byte): TColor;                           {  make a color more dark or light. Percent arata la cat la suta din luminozitate sa pun noua valoare. O= totally dark, 100= unchanged }
 function  LightenColor        (aColor: TColor; Percent: Byte): TColor;

 function  ChangeBrightness    (aSource: TColor; Change: Integer): TColor;
 function  ChangeColor         (aSourceColor, aDestClr: TColor; Change: Integer): TColor;         { Change Source color towards Destination. Change is in 'percents'. }

 function  SimilarColor        (Color1, Color2: TColor; Tolerance: Integer): Boolean;             { Checks is two colors are similar }
 function  ComplementaryColor  (aColor: TColor): TColor;

 { Color conversion }
 function  ColorToHtml         (aColor: TColor): string;
 function  HtmlToColor         (aColor: string): TColor;

 { Color split }
 function  RGB2Color           (R,G,B: Integer): Cardinal; deprecated 'Use WinApi.Windows.RGB instead.'  { !!!!! uses WinApi.Windows.RGB(R,G,B) }
 //function  RGB2Gray          (aColor: TColor) : TColor;  see RGB2Gray.pas            { If you want to convert a colored image into same gray scaled, then you must convert the color of the each pixel by the next schema }
 procedure SplitColor2RGB      (aColor: TColor; OUT R,G,B: Byte);
 function  Integer2Color       (i: Integer): TColor;                                              { Tries to make a color from an integer. The color are 'lighted' in this order: BGR.   }

 { Color blend }
 function  BlendColors         (Color1, Color2: TColor; A: Byte): TColor;                         { Mixing two colors.     Usage  NewColor:= Blend(Color1, Color2, blending level 0 to 100);    Source: http://rmklever.com/?cat=6 }
 {$IF Defined(CPUX86)}
 function  CombinePixels       (Pixels: PByte; Weights: PInteger; Size: Cardinal): Integer;       { NOT TESTED UNDER WIN64. IT MIGHT WORK! }
 function  MixColors           (FG, BG: TColor; BlendPower: Byte): TColor;
 {$ELSE}
 function  MixColors           (FG, BG: TColor; BlendPower: Byte): TColor; {$ENDIF}

 procedure ReplaceColor        (BMP: TBitmap; OldColor, NewColor: TColor);  overload;
 procedure ReplaceColor        (BMP: TBitmap; OldColor, NewColor: TColor; ToleranceR, ToleranceG, ToleranceB: Byte);  overload;

 function  GetAverageColor     (BMP: TBitmap; Fast: Boolean): TColor;
 function  GetAverageColorPf8  (BMP: TBitmap): Byte;

{-------------------------------------------------------------------------------------------------------------
   Lines
-------------------------------------------------------------------------------------------------------------}
 procedure AntialisedLine(Canvas: TCanvas; CONST AX1, AY1, AX2, AY2: Real; Color: TColor);
 procedure DrawPolygon   (Canvas: TCanvas; Color: TColor; const Points: array of integer);

{-------------------------------------------------------------------------------------------------------------
   Draw glyphs
-------------------------------------------------------------------------------------------------------------}
 procedure DrawTriangle (Canvas: TCanvas; Width, Height, BorderDistance: Integer);  { Draw a triangle (like a Play button) the that fits into the specified control }
 procedure DrawBorder   (Control: TWinControl);                                     { Draw red frame arround the specified control }
 procedure DrawCheck    (Canvas: TCanvas; Location : TPoint; Size: Integer; Shadow: Boolean = True);     deprecated 'Call directly VCLGhraphUtil instead.' { Draws checkmarks of any Size at Location with/out a shadow. }
 procedure DrawChevron  (Canvas: TCanvas; Direction: TScrollDirection; Location: TPoint; Size: Integer); deprecated 'Call directly VCLGhraphUtil instead.' { Draws arrows that look like ">" which can point in any TScrollDirection }
 procedure DrawArrow    (Canvas: TCanvas; Direction: TScrollDirection; Location: TPoint; Size: Integer); deprecated 'Call directly VCLGhraphUtil instead.' { Draws a solid triangular arrow that can point in any TScrollDirection }

 //Moved here to eliminate dependencies
 // c:\Projekte\CommonLib\Dependencies\code_that_depends_on_ext_lib.pas
 //procedure DrawCheck  (Canvas: TCanvas; Location : TPoint; Size: Integer; Shadow: Boolean = True);     deprecated 'Call directly VCLGhraphUtil instead.' { Draws checkmarks of any Size at Location with/out a shadow. }
 //procedure DrawChevron(Canvas: TCanvas; Direction: TScrollDirection; Location: TPoint; Size: Integer); deprecated 'Call directly VCLGhraphUtil instead.' { Draws arrows that look like ">" which can point in any TScrollDirection }
 //procedure DrawArrow  (Canvas: TCanvas; Direction: TScrollDirection; Location: TPoint; Size: Integer); deprecated 'Call directly VCLGhraphUtil instead.' { Draws a solid triangular arrow that can point in any TScrollDirection }


{-------------------------------------------------------------------------------------------------------------
   Graph controls
-------------------------------------------------------------------------------------------------------------}
 function  Control2Bitmap      (Control: TWinControl): TBitmap;                                  { Copy the image of a VCL control (tbutton, tmemo, tcalendar) to a bitmap }
 function  Control2Png         (Control: TWinControl): TPngImage;

{-------------------------------------------------------------------------------------------------------------
   Themes
-------------------------------------------------------------------------------------------------------------}
 function  WindowsThemesEnabled: Boolean;
 function  VclStylesEnabled: Boolean;

 function  ThemeColorBkg: TColor;
 function  ThemeColorHilight: TColor;
 function  ThemeColorButtonFace: TColor;
 function  ThemeGetPanelElementColor(Elem: TElementColor): TColor;  { Use it like this: cl:= ThemeGetPanelElementColor; if cl= clNone then UseDefaultColor }

{-------------------------------------------------------------------------------------------------------------
   Utils
-------------------------------------------------------------------------------------------------------------}
 function  GetDeviceColorDepth: Integer;


IMPLEMENTATION

USES
   cGraphBitmap, ccColors, cmMath;






{--------------------------------------------------------------------------------------------------
   Shape Drawing Routines
--------------------------------------------------------------------------------------------------}
{ Draw a triangle (like a Play button) the that fits into the specified control }
procedure DrawTriangle(Canvas: TCanvas; Width, Height, BorderDistance: Integer);
VAR
   Vertices: array[0..2] of TPoint;     { You can do the same with less code using open array parameters. See LU RD's answer:  https://stackoverflow.com/questions/9999166/delphi-image-canvas-paint-an-area-triangle-rectangle-polygons }
begin
  vertices[0] := Point(BorderDistance, BorderDistance);
  vertices[2] := Point(Width - BorderDistance, Height DIV 2);
  vertices[1] := Point(BorderDistance, Height - BorderDistance);//ClientHeight = 32

  Canvas.Polygon(Vertices);
end;


procedure DrawPolygon(Canvas: TCanvas; Color: TColor; const Points: array of integer);
var
  arr: array of TPoint;
  i: Integer;
begin
  SetLength(arr, Length(Points) div 2);
  for i := 0 to High(arr) DO arr[i] := Point(Points[2*i], Points[2*i+1]);

  Canvas.Pen.Color := Color;
  Canvas.Polygon(arr);
end;


procedure DrawBorder(Control: TWinControl);
VAR
   r: TRect;
   i: Integer;
   canvas: TCanvas;
   ParentControl: TWinControl;
begin
 ParentControl:= Control.Parent;
 Canvas:= TCanvas.Create; {!! This could be done also without creating a canvas. Just pass the canvas of a control (if you are "inside" the control }
 TRY
  Canvas.Handle:= GetWindowDC(ParentControl.Handle);
  r:= Rect(Control.Left, Control.Top, Control.Left+ Control.Width, Control.Top+ Control.Height);
  WITH canvas DO
  for i:= 1 to 3 do
    begin
     InflateRect(r, -1, -1);
     Brush.Color:= clRed;
     Brush.Style:= bsSolid;
     FrameRect(r);
    end;
 FINALLY
   FreeAndNil(canvas);
 END;
end;



procedure DrawArrow(Canvas: TCanvas; Direction: TScrollDirection; Location: TPoint; Size: Integer);
begin
 VCL.GraphUtil.DrawArrow(Canvas, Direction, Location, Size);
end;


procedure DrawChevron(Canvas: TCanvas; Direction: TScrollDirection; Location: TPoint; Size: Integer);
begin
 VCL.GraphUtil.DrawChevron(Canvas, Direction, Location, Size);
end;


procedure DrawCheck(Canvas: TCanvas; Location: TPoint; Size: Integer; Shadow: Boolean = True);
begin
 VCL.GraphUtil.DrawCheck(Canvas, Location, Size, Shadow);
end;













{--------------------------------------------------------------------------------------------------
   ANTI-ALISED LINE
   FASTER METHOD HERE: http://stackoverflow.com/questions/3613130/simple-anti-aliasing-function-for-delphi-7
--------------------------------------------------------------------------------------------------}
procedure AntialisedLine(Canvas: TCanvas; CONST AX1, AY1, AX2, AY2: Real; Color: TColor);
VAR Swapped: boolean;

  {$R-}
  procedure plot(const x, y, c: real);
  VAR resclr: TColor;
  begin
    if swapped
    then resclr := Canvas.Pixels[round(y), round(x)]
    else resclr := Canvas.Pixels[round(x), round(y)];
    resclr := RGB(round(GetRValue(resclr) * (1-c) + GetRValue(Color) * c),
                  round(GetGValue(resclr) * (1-c) + GetGValue(Color) * c),
                  round(GetBValue(resclr) * (1-c) + GetBValue(Color) * c));
    if swapped
    then Canvas.Pixels[round(y), round(x)] := resclr
    else Canvas.Pixels[round(x), round(y)] := resclr;
  end;
  {$R+}

  function rfrac(const x: real): real; inline;
  begin
   rfrac := 1 - frac(x);
  end;

  procedure swap(var a, b: real);
  VAR tmp: real;
  begin
    tmp := a;
    a := b;
    b := tmp;
  end;

VAR
  x1, x2, y1, y2, dx, dy, gradient, xend, yend, xgap, xpxl1, ypxl1,
  xpxl2, ypxl2, intery: real;
  x: integer;

begin
  x1 := AX1;
  x2 := AX2;
  y1 := AY1;
  y2 := AY2;

  dx := x2 - x1;
  dy := y2 - y1;
  swapped := abs(dx) < abs(dy);

  if swapped then
   begin
    swap(x1, y1);
    swap(x2, y2);
    swap(dx, dy);
   end;

  if x2 < x1 then
   begin
    swap(x1, x2);
    swap(y1, y2);
   end;

  if Dx=0
  then gradient:= dy
  else gradient:= dy / dx;

  xend  := round(x1);
  yend  := y1 + gradient * (xend - x1);
  xgap  := rfrac(x1 + 0.5);
  xpxl1 := xend;
  ypxl1 := floor(yend);
  plot(xpxl1, ypxl1, rfrac(yend) * xgap);
  plot(xpxl1, ypxl1 + 1, frac(yend) * xgap);
  intery := yend + gradient;

  xend  := round(x2);
  yend  := y2 + gradient * (xend - x2);
  xgap  := frac(x2 + 0.5);
  xpxl2 := xend;
  ypxl2 := floor(yend);
  plot(xpxl2, ypxl2, rfrac(yend) * xgap);
  plot(xpxl2, ypxl2 + 1, frac(yend) * xgap);

  for x := round(xpxl1) + 1 to round(xpxl2) - 1 do
   begin
    plot(x, floor(intery), rfrac(intery));
    plot(x, floor(intery) + 1, frac(intery));
    intery:= intery + gradient;
   end;
end;




 










{--------------------------------------------------------------------------------------------------
                                    COLORS
--------------------------------------------------------------------------------------------------}
{see:
  VclGraphUtil.GetHighLightColor
  VclGraphUtil.GetShadowColor  }

function SimilarColor(Color1, Color2: TColor; Tolerance: Integer): Boolean;            { Checks is two colors are similar }
VAR R1, R2, G1, G2, B1, B2: Byte;        {todo 4: this is a better algorithm https://en.wikipedia.org/wiki/Color_difference }
begin
 R1:= Byte(Color1);
 G1:= Byte(Color1 shr  8);
 B1:= Byte(Color1 shr 16);

 R2:= Byte(Color2);
 G2:= Byte(Color2 shr  8);
 B2:= Byte(Color2 shr 16);

 Result:= (abs(R1-R2)< Tolerance) AND (abs(G1-G2)< Tolerance) AND (abs(B1-B2)< Tolerance);
end;



function DarkenColor(aColor: TColor; Percent: Byte): TColor;                                       { Make a color more dark or light. Percent arata la cat la suta din luminozitate sa pun noua valoare. O= totally dark, 100= unchanged. See also:   http://www.delphi3000.com/articles/article_2310.asp?SK=   }
var R, G, B: Byte;
begin
  aColor:= ColorToRGB(aColor);                                                                     {  GetRValue which accepts cardinals. However, TColor is integer. So I need a conversion else I get e RangeCheckError one line below.       See this: http://stackoverflow.com/questions/9809687/windows-getrvalue-accepts-cardinals-but-tcolor-is-integer}
  R := GetRValue(aColor);
  G := GetGValue(aColor);
  B := GetBValue(aColor);
  R := Round(R* Percent/100);
  G := Round(G* Percent/100);
  B := Round(B* Percent/100);
  Result := RGB(R, G, B);
end;



function LightenColor(aColor: TColor; Percent: Byte): TColor;                                      { Make a color more dark or light. Percent arata la cat la suta din luminozitate sa pun noua valoare. O= totally light, 100= unchanged  }
var R, G, B: Byte;
begin
  aColor:= ColorToRGB(aColor);                                                                     {  GetRValue which accepts cardinals. However, TColor is integer. So I need a conversion else I get e RangeCheckError one line below.       See this: http://stackoverflow.com/questions/9809687/windows-getrvalue-accepts-cardinals-but-tcolor-is-integer}
  R := GetRValue(aColor);
  G := GetGValue(aColor);
  B := GetBValue(aColor);
  R := Round(R*Percent/100) + Round(255 - Percent/100*255);
  G := Round(G*Percent/100) + Round(255 - Percent/100*255);
  B := Round(B*Percent/100) + Round(255 - Percent/100*255);
  Result := RGB(R, G, B);
end;




TYPE
  TRGBColor = packed record
      case Boolean of
        TRUE:  (Color: LongInt);
        FALSE: (r: Byte;
                g: Byte;
                b: Byte;
                a: Byte;);
  end;

{ Change brightness. Change is in 'value' not in 'percents'. }
function ChangeBrightness(aSource: TColor; Change: Integer): TColor;
VAR
   Source: TRGBColor;
begin
  Source.Color := ColorToRGB(aSource);

  Source.r := Min(Max(Source.r + Change, 0), 255);
  Source.g := Min(Max(Source.g + Change, 0), 255);
  Source.b := Min(Max(Source.b + Change, 0), 255);  // We use Min/Max to ensure RGB remains in the 0-255 range.

  Result   := Source.Color;
end;



{ Change Source color towards Destination. Change is in 'percents'. }
function ChangeColor(aSourceColor, aDestClr: TColor; Change: Integer): TColor;
VAR
    Source, Dest: TRGBColor;
begin
  Source.Color := ColorToRGB(aSourceColor);
  Dest.Color   := ColorToRGB(aDestClr);

  Source.b := Min(Max (Source.b - Round(Change * (Source.b - Dest.b) / 100), 0), 255);
  Source.g := Min(Max (Source.g - Round(Change * (Source.g - Dest.g) / 100), 0), 255);
  Source.r := Min(Max (Source.r - Round(Change * (Source.r - Dest.r) / 100), 0), 255);

  Result := Source.Color;
end;







function ColorToHtml(aColor: TColor): string;  { This is total equivalent with Vcl.GraphUtil.ColorToWebColorStr. But they use a different approach }
begin
  Result := IntToHex(aColor, 6);
  Result := system.COPY(Result, 5, 2) + system.COPY(Result, 3, 2) + system.COPY(Result, 1, 2);     { The caler will have to add '#' in from of it to get a valid HTML color }
end;



{ Example: HtmlToColor('D1D2D9')  }
function HtmlToColor(aColor: string): TColor;  { This is total equivalent with Vcl.GraphUtil.WebColorStrToColor}
begin
  Result:= StringToColor('$' + system.COPY(aColor, 6, 2) + system.COPY(aColor, 4, 2) + system.COPY(aColor, 2, 2));
end;



function ComplementaryColor(aColor: TColor) : TColor;
begin
 Result:= $FFFFFF XOR aColor;
end;



function BlendColors(Color1, Color2: TColor; A: Byte): TColor;     { Mixing two colors.  Usage  NewColor:= BlendColors(Color1, Color2, blending level 0 to 100);    Source: http://rmklever.com/?cat=6 }
var
  c1, c2: LongInt;
  r, g, b, v1, v2: byte;
begin
  A:= Round(2.55 * A);
  c1 := ColorToRGB(Color1);
  c2 := ColorToRGB(Color2);
  v1:= Byte(c1);
  v2:= Byte(c2);
  r:= A * (v1 - v2) shr 8 + v2;
  v1:= Byte(c1 shr 8);
  v2:= Byte(c2 shr 8);
  g:= A * (v1 - v2) shr 8 + v2;
  v1:= Byte(c1 shr 16);
  v2:= Byte(c2 shr 16);
  b:= A * (v1 - v2) shr 8 + v2;
  Result := (b shl 16) + (g shl 8) + r;
end;



procedure SplitColor2RGB(aColor: TColor; OUT R,G,B: Byte);   { Or like this: http://www.delphi3000.com/articles/article_624.asp?SK=          }
begin                                                        //BackgndColor:= $0164FF; {BGR - Rosu maxim, fara Albastru - CAZ API}            FrmBX.WallpaperObj.BackgndColor:= $0002ff;     {RGB - Rosu maxim, fara Albastru;  i= 16737281 - CAZ DELPHI}
 aColor:= ColorToRGB(aColor);                                {  GetRValue which accepts cardinals. However, TColor is integer. So I need a conversion else I get e RangeCheckError one line below.       See this: http://stackoverflow.com/questions/9809687/windows-getrvalue-accepts-cardinals-but-tcolor-is-integer}
 R := GetRValue (aColor);                                    {red}
 G := GetGValue (aColor);                                    {green}
 B := GetBValue (aColor);                                    {blue}
end;

{ old code. give range check errors. need to disable range check?
procedure SplitColor2RGB_(aColor: TColor; VAR R,G,B: Byte);
VAR i: longint;
begin
  i:= ColorToRGB(culoare);
  R:=  i AND $0000FF;
  G:= (i shr 8) AND $0000FF;
  B:= (i shr 8) shr 8;
end; }



function RGB2Color(R,G,B: Integer): Cardinal;                                                { uses WinApi.Windows.RGB(R,G,B) }
begin
 Result:= WinApi.Windows.RGB(R,G,B);
end;


function Integer2Color(i: Integer): TColor;     { Tries to make a color from an integer. The color are 'lighted' in this order: BGR.   }
VAR R, G, B: Byte;
begin
 if i < 0 then i:= 0;

 if i > 65535
 then
  begin
   r:= (i div 65536) mod 256;
   g:= 255;
   b:= 255;
  end
 else

 if i > 255
 then
  begin
   r:= 0;
   g:= (i mod 65536) div 256;
   b:= 255;
  end
 else

  begin
   r:= 0;
   g:= 0;
   b:= i;
  end;

 Result:= WinApi.Windows.RGB(R,G,B);
end;




{ Tester: c:\Myprojects\Project Testers\gr cGraphicsTester\Tester.dpr }
procedure ReplaceColor(BMP: TBitmap; OldColor, NewColor: TColor);
VAR
   x, y: Integer;
   R,G,B: Byte;
   R_,G_,B_: Byte;
   aPixel: PRGBTriple;
begin
 R:= GetRValue(OldColor);
 G:= GetGValue(OldColor);
 B:= GetBValue(OldColor);

 R_:= GetRValue(NewColor);
 G_:= GetGValue(NewColor);
 B_:= GetBValue(NewColor);

 BMP.PixelFormat := pf24bit;
 for y := 0 to BMP.Height - 1 do
  for x := 0 to BMP.Width - 1 do
   begin
     aPixel := BMP.ScanLine[y];
     Inc(aPixel, x);
     if  (aPixel^.rgbtRed   = R)
     AND (aPixel^.rgbtGreen = G)
     AND (aPixel^.rgbtBlue  = B) then
      begin
       aPixel^.rgbtRed   := R_;
       aPixel^.rgbtGreen := G_;
       aPixel^.rgbtBlue  := B_;
      end;
   end;
end;


procedure ReplaceColor(BMP: TBitmap; OldColor, NewColor: TColor; ToleranceR, ToleranceG, ToleranceB: Byte);
VAR
   x, y: Integer;
   R,G,B: Byte;
   R_,G_,B_: Byte;
   aPixel: PRGBTriple;
begin
 R:= GetRValue(OldColor);
 G:= GetGValue(OldColor);
 B:= GetBValue(OldColor);

 R_:= GetRValue(NewColor);
 G_:= GetGValue(NewColor);
 B_:= GetBValue(NewColor);

 BMP.PixelFormat := pf24bit;
 for y := 0 to BMP.Height - 1 do
  for x := 0 to BMP.Width - 1 do
   begin
     aPixel := BMP.ScanLine[y];
     Inc(aPixel, x);
     if  (abs(aPixel^.rgbtRed  - R)< ToleranceR)
     AND (abs(aPixel^.rgbtGreen- G)< ToleranceG)
     AND (abs(aPixel^.rgbtBlue - B)< ToleranceB) then
      begin
       aPixel^.rgbtRed   := R_;
       aPixel^.rgbtGreen := G_;
       aPixel^.rgbtBlue  := B_;
      end;
   end;
end;



{--------------------------------------------------------------------------------------------------
   AVERAGE COLOR
   If Fast is true, I only read the odd lines, to make it faster.

   Note: This makes an average between pixels. It does not search for the DOMINANT color!
   Source: http://www.delphigroups.info/2/fd/205241.html
--------------------------------------------------------------------------------------------------}
//ToDo: make a function that determines the average color using only the margins of the image, not also the center.
function GetAverageColor(Bmp: TBitmap; Fast: Boolean): TColor;
VAR
   X, Y: Integer;
   pixels: Cardinal;
   r,g,b: uint64; //We used to use a Cardial (0..4294967295) to store the colors (r,g,b). If the image is bigger than 4M pixles and is totally white than the overflow is expected. White is the worst because its value is 255. As the values accumulates over the entire image, it will reach the max value of 4294967295 much faster.   The darker the color, the higher the resolution supported.   Fixed by upgrading from Cardinal to UInt64
   Row: PRGB24Array;
begin
 r:= 0;
 g:= 0;
 b:= 0;
 Assert(Bmp.PixelFormat= pf24bit);

 for y := 0 to bmp.Height-1 do
  begin
   if Fast AND (y mod 2 = 0) then Continue;
   row := bmp.ScanLine[y];
   for x := 0 to bmp.Width -1 do
    begin
     r := r + row[x].R;
     g := g + row[x].G;
     b := b + row[x].B;
    end;
  end;

 pixels := bmp.Height*bmp.Width;
 r := r div pixels;
 g := g div pixels;
 b := b div pixels;
 Result:= WinApi.Windows.RGB(r,g,b);
end;


function GetAverageColorPf8(Bmp: TBitmap): Byte;
VAR
   X, Y: Integer;
   Row: PByte;
   Summ: Int64;
begin
 Summ:= 0;
 Assert(Bmp.PixelFormat= pf8bit);

 for y := 0 to bmp.Height-1 do
  begin
   row := bmp.ScanLine[y];
   for x := 0 to bmp.Width -1 do
     Summ := Summ + row[x];
  end;

 Result := Summ div (bmp.Height*bmp.Width);
end;



{$IFDEF CPUx86}
{does not work. need to retest now }
{TYPE
  TRGBTriple - defined in winapi.windows  }

function GetAverageColor_asm(Bmp: TBitmap): TColor;    //http://www.delphigroups.info/2/fd/205241.html
var
  Y, W: Integer;
  P: PRGBTriple;
  r, g, b: Integer;
begin
 bmp.pixelformat := pf24bit;
 Result:= clPink;

 r := 0;
 g := 0;
 b := 0;
 W := Bmp.Width;
 for y := 0 to Bmp.Height-1 do
  begin
   P := Bmp.ScanLine[y];
   asm
     mov     ecx, W
     and     ecx, ecx
     jz      @end
     push    ebx
     push    esi
     push    edi

     mov     edi, ecx

     mov     ebx, b
     mov     ecx, g
     mov     edx, r

     mov     esi, P

   @Next:
     movzx   eax, BYTE PTR [esi]
     add     ebx, eax
     movzx   eax, BYTE PTR [esi]+1
     add     ecx, eax
     movzx   eax, BYTE PTR [esi]+2
     add     edx, eax

     dec     edi
     jz      @finis

     movzx   eax, BYTE PTR [esi]+3
     add     ebx, eax
     movzx   eax, BYTE PTR [esi]+4
     add     ecx, eax
     movzx   eax, BYTE PTR [esi]+5
     add     edx, eax

     dec     edi
     jz      @finis

     movzx   eax, BYTE PTR [esi]+6
     add     ebx, eax
     movzx   eax, BYTE PTR [esi]+7
     add     ecx, eax
     movzx   eax, BYTE PTR [esi]+8
     add     edx, eax

     dec     edi
     jz      @finis

     movzx   eax, BYTE PTR [esi]+9
     add     ebx, eax
     movzx   eax, BYTE PTR [esi]+10
     add     ecx, eax
     movzx   eax, BYTE PTR [esi]+11
     add     edx, eax

     add     esi, TYPE TRGBTriple * 4
     dec     edi
     jnz     @Next

   @finis:
     mov     b, ebx
     mov     g, ecx
     mov     r, edx

     pop     edi
     pop     esi
     pop     ebx
   @end:
  end;
 end;
end;
{$ENDIF}





{$IF Defined(CPUX86)}
{ Mix R,G and B channels of our colors separately.
  The value of BlendPower is between 0 and 255 as described above.  }
function MixColors(FG, BG: TColor; BlendPower: byte): TColor;
var r,g,b:byte;                                                                       { As you know, TColor value is 4 bytes length integer value where low byte is red channel, 2nd byte is green and 3rd byte is blue }
begin
  R := cmMath.MixBytes( FG and 255,BG and 255, BlendPower);                                  // extracting and mixing Red
  G := cmMath.MixBytes((FG shr 8 ) and 255, (BG shr 8 ) and 255, BlendPower);                // the same with green
  B := cmMath.MixBytes((FG shr 16) and 255, (BG shr 16) and 255, BlendPower);                // and blue, of course
  Result:= r+ g* 256+ b* 65536;                                                       // finishing with combining all channels together
end;

{$ELSE}
{The value of BlendPower is between 0 and 255 as described above. }
function MixColors(FG, BG: TColor; BlendPower: Byte): TColor;                 {Source http://rmklever.com }
VAR
  c1, c2: LongInt;
  r, g, b, v1, v2: byte;
begin
  BlendPower:= Round(2.55 * BlendPower);
  c1 := ColorToRGB (FG);
  c2 := ColorToRGB (BG);
  v1 := Byte(c1);
  v2 := Byte(c2);
  r  := BlendPower * (v1 - v2) shr 8 + v2;
  v1 := Byte(c1 shr 8);
  v2 := Byte(c2 shr 8);
  g  := BlendPower * (v1 - v2) shr 8 + v2;
  v1 := Byte(c1 shr 16);
  v2 := Byte(c2 shr 16);
  b  := BlendPower * (v1 - v2) shr 8 + v2;
  Result := (b shl 16) + (g shl 8) + r;
end;
{$ENDIF}




{$IF Defined(CPUX86)}
function CombinePixels(Pixels: PByte; Weights: PInteger; Size: Cardinal): Integer;           { http://stackoverflow.com/questions/10126198/how-to-optimize-this-delphi-function-with-sse2 }
//x86, register calling convention - three parameters in EAX, EDX, ECX
const
  Precision: Single = 1.0;
asm
  pxor XMM6, XMM6           // zero const
  pxor XMM4, XMM4           // zero accum

@@cycle:
  movd XMM1, [eax]          // load color data
  movss XMM3, [edx]         // load weight

  punpcklbw XMM1, XMM6      // bytes to words
  shufps XMM3, XMM3, 0      // 4 x weight
  punpcklwd XMM1, XMM6      // words to ints
  cvtdq2ps XMM2, XMM3       // ints to singles
  cvtdq2ps XMM0, XMM1       // ints to singles

  mulps XMM0, XMM2          // data * weight
  addps XMM4, XMM0          // accum  = accum + data * weight

  add eax, 4                // inc pointers
  add edx, 4
  loop @@cycle

  movss XMM5, Precision
  shufps XMM5, XMM5, 0      // 4 x precision constant

  divps XMM4, XMM5          // accum/precision

  cvtps2dq XMM2, XMM4       // rounding singles to ints
  packssdw XMM2, XMM2       // ints to ShortInts
  packuswb XMM2, XMM2       // ShortInts to bytes

  movd eax, XMM2            // result
end;
{$ENDIF}



 



{--------------------------------------------------------------------------------------------------
   VCL
--------------------------------------------------------------------------------------------------}

function Control2Bitmap(Control: TWinControl): TBitmap;                                           { This will create a BMP object. Don't forget to free it! } { Copy the image of a VCL control (tbutton, tmemo, tcalendar) to a bitmap }
VAR DC: HDC;
begin
 if NOT Assigned(Control)
 then RAISE exception.Create('Cannot copy control image. Control is NIL!');

 Result:= cGraphBitmap.CreateBitmap(Control.Width, Control.Height);

 DC := GetWindowDC(Control.Handle);
 TRY
   Control.PaintTo(Result.Canvas, 0, 0);
 FINALLY
   ReleaseDC(Control.Handle, DC);
 END;
end;



function Control2Png(Control: TWinControl): TPngImage;
VAR BMP: TBitmap;
begin
 BMP:= Control2Bitmap(Control);
 TRY
  Result:= TPngImage.Create;
  Result.Assign(BMP);
 FINALLY
  FreeAndNil(BMP);
 END;
end;










 





{--------------------------------------------------------------------------------------------------
   THEMES / SKINS
--------------------------------------------------------------------------------------------------}
function WindowsThemesEnabled: Boolean;
begin
 Result:= Vcl.Themes.TStyleManager.Enabled;    // Returns true  when no skin is loaded
 //Result:= TStyleManager.IsCustomStyleActive; // Returns false when no skin ('Windows') is loaded and true when a skin ('Aqua graphite') is loaded
 //Result:= TStyleManager.ActiveStyle.Enabled; // Returns true  when no skin is loaded
end;


function VclStylesEnabled: Boolean;
begin
 Result:= TStyleManager.IsCustomStyleActive;         // Returns false when no skin ('Windows') is loaded and true when a skin ('Aqua graphite') is loaded
end;


function ThemeColorBkg: TColor;
begin
 Result:= TStyleManager.ActiveStyle.GetSystemColor(clBackground);
end;


function ThemeColorHilight: TColor;
begin
 Result:= TStyleManager.ActiveStyle.GetSystemColor(clHighlight);
end;


function ThemeColorButtonFace: TColor;    { Works also with TPanel }
begin
 Result:= TStyleManager.ActiveStyle.GetSystemColor(clBtnFace);
end;


{ Get misc colors used to paint a TPanel (but can be used as template for other controls also!)
  Use it like this: cl:= ThemeGetPanelElementColor(ecFillColor); if cl= clNone then UseDefaultColor  }
function ThemeGetPanelElementColor(Elem: TElementColor): TColor;
VAR
  LStyle: TCustomStyleServices;
  LDetails: TThemedElementDetails;
begin
  Result:= clPink; // What should I return if not LStyle.Enabled
  LStyle := StyleServices;
  if LStyle.Enabled
  then
    begin
      LDetails := LStyle.GetElementDetails(tpPanelBackground);        //Example tpPanelBevel  tpPanelBackground
      LStyle.GetElementColor(LDetails, Elem, Result)                  //Example: ecFillColor ecBorderColor ecEdgeDkShadowColor
    end
end;



{--------------------------------------------------------------------------------------------------
   UTILS
--------------------------------------------------------------------------------------------------}
function GetDeviceColorDepth: Integer;
VAR dc: THandle;
begin
 dc:= GetDC( 0 );
 TRY
   Result := GetDeviceCaps( dc, BITSPIXEL ) * GetDeviceCaps( dc, PLANES );
 FINALLY
   ReleaseDC( 0, dc );
 END;
end;





end.
