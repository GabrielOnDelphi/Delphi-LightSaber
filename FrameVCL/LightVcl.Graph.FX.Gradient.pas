UNIT LightVcl.Graph.FX.Gradient;

{=============================================================================================================
   2026.01.30
   Author: Gabriel Moraru
--------------------------------------------------------------------------------------------------------------
   Draws a gradient over a canvas

   Also:
      How to paint gradient on Form?
      https://stackoverflow.com/questions/9532549/how-to-paint-2-gradients-together-on-a-canvas

      TForm with gradient colors:
      http://www.festra.com/wwwboard/messages/13144.html

      See also: Embarcadero's GraphUtil.pas
                c:\MyProjects\Packages\Third party packages\GradientFill.pas

      Tester: c:\Myprojects\Project Testers\gr cGraphicsTester\Tester.dpr
==================================================================================================}

INTERFACE

USES
  WinApi.Windows, System.SysUtils, Vcl.Graphics, Vcl.Controls, Vcl.GraphUtil, Vcl.Forms;

procedure VistaGradient (BMP: TBitmap; const c1: Byte; const c3, c4: TColor; const Center, Reverse: Boolean);    { http://rmklever.com/?tag=thumbnails }
procedure GradientFill  (Control: TWinControl; Color1, Color2: TColor; Orientation: Integer= GRADIENT_FILL_RECT_V; Transparency: Word= 255); { Draws a gradient over a canvas }
procedure GradientFillCanvas(ACanvas: TCanvas; const AStartColor, AEndColor: TColor; const ARect: TRect; const Direction: TGradientDirection); deprecated 'Call Vcl.GraphUtil.GradientFillCanvas directly!';

procedure DrawRedPattern(BMP: TBitmap);     { Draws a nice rectangle patern }




IMPLEMENTATION
USES
   LightVcl.Common.VclUtils, LightVcl.Graph.Util, LightVcl.Graph.Bitmap;




{ Deprecated wrapper - use Vcl.GraphUtil.GradientFillCanvas directly }
procedure GradientFillCanvas(ACanvas: TCanvas; const AStartColor, AEndColor: TColor; const ARect: TRect; const Direction: TGradientDirection);
begin
  Assert(ACanvas <> NIL, 'GradientFillCanvas: ACanvas parameter cannot be nil');
  Vcl.GraphUtil.GradientFillCanvas(ACanvas, AStartColor, AEndColor, ARect, Direction);
end;


{---------------------------------------------------------------------------------------------------------------
   GradientFill
   Draws a gradient directly on a TWinControl's canvas.

   Parameters:
     Control      - The control to paint on (must be TCustomControl or TCustomForm)
     Color1       - Gradient start color
     Color2       - Gradient end color
     Orientation  - GRADIENT_FILL_RECT_H (horizontal) or GRADIENT_FILL_RECT_V (vertical)
     Transparency - Alpha value (0-255), though not all renderers support this

   Note: Silently exits if Control is not a supported type (TCustomControl/TCustomForm).
---------------------------------------------------------------------------------------------------------------}
procedure GradientFill(Control: TWinControl; Color1, Color2: TColor; Orientation: Integer= GRADIENT_FILL_RECT_V; Transparency: Word= 255);

   procedure VertexColor(VAR aVertex: triVertex; aColor: TColor);
   begin
     { Convert TColor to TRIVERTEX format (16-bit color channels) }
     aVertex.Red   := GetRValue(aColor) SHL 8;
     aVertex.Green := GetGValue(aColor) SHL 8;
     aVertex.Blue  := GetBValue(aColor) SHL 8;
     aVertex.Alpha := Transparency;
   end;


   Function UpperLeft(aColor: TColor): triVertex;
   begin
     result.x := Control.ClientRect.Left;
     result.y := Control.ClientRect.Top;
     VertexColor(result, aColor);
   end;


   Function LowerRight(aColor: TColor): triVertex;
   begin
     result.x := Control.ClientWidth;
     result.y := Control.ClientHeight;
     VertexColor(result, aColor);
   end;


CONST
  gRect: GRADIENT_RECT = (UpperLeft: 0; LowerRight: 1); { References the TRIVERTEX vertices }
VAR
  DC: HDC;
  vert: array [0 .. 1] of triVertex;
  aCanvas: TCanvas;
begin
  if Control = NIL then EXIT;

  { Get canvas from supported control types }
  if Control is TCustomControl
  then aCanvas:= TCustomControlEx(Control).Canvas
  else
    if Control is TCustomForm
    then aCanvas:= TCustomForm(Control).Canvas
    else EXIT;  { Unsupported control type }

  if Assigned(aCanvas) then
   begin
    DC := HDC(aCanvas.Handle);
    vert[0] := UpperLeft (Color1);
    vert[1] := LowerRight(Color2);
    WinApi.Windows.GradientFill(DC, @vert[0], 2, @gRect, 1, Orientation);
   end;
end;



{---------------------------------------------------------------------------------------------------------------
   VistaGradient
   Creates a Vista-style gradient effect on a bitmap.

   Parameters:
     BMP     - The bitmap to receive the gradient (existing content will be overwritten)
     c1      - Brightness factor (0-255). 0=full brightness gradient, 255=darker gradient
     c3, c4  - Start and end colors for the vertical gradient
     Center  - If True, creates a mirrored/centered horizontal gradient
     Reverse - If True, reverses the vertical gradient direction

   Note: Range checking is disabled for this procedure due to direct scanline manipulation.
   Source: http://rmklever.com/?tag=thumbnails
---------------------------------------------------------------------------------------------------------------}
{$R-}
procedure VistaGradient(BMP: TBitmap; const c1: Byte; const c3, c4: TColor; const Center, Reverse: Boolean);
TYPE
  TRGB = record b, g, r : Byte; end;
  PRGBArray = ^TRGBArray;
  TRGBARRAY = array[0..0] of TRGB;
VAR
  rc1, gc1, bc1, rc2, gc2, bc2: Integer;
  x, y, w, h, j: Integer;
  i, w1: Integer;
  Row: PRGBArray;
  slMain, slSize, slPtr: NativeInt;  { Must be NativeInt for 64-bit compatibility (pointer arithmetic) }
  Color: Integer;
  QCol: Array of TRGB;
  Temp: TBitmap;
begin
 Assert(BMP <> NIL, 'VistaGradient: BMP parameter cannot be nil');
 Assert((BMP.Width > 0) AND (BMP.Height > 1), 'VistaGradient: BMP Width must be > 0 and Height must be > 1');
 { Note: Height > 1 is required because we compute scanline stride from ScanLine[0] and ScanLine[1] }

 Temp:= TBitmap.Create;
 TRY
  Temp.PixelFormat:= pf24Bit;
  SetLargeSize(Temp, BMP.Width, BMP.Height);

  h:= Temp.Height;
  w:= Temp.Width;

  { Extract RGB components from start color (c3) }
  Color:= ColorToRGB(c3);
  rc1 := Byte(Color);
  gc1 := Byte(Color shr 8);
  bc1 := Byte(Color shr 16);

  { Extract RGB components from end color (c4) }
  Color:= ColorToRGB(c4);
  rc2 := Byte(Color);
  gc2 := Byte(Color shr 8);
  bc2 := Byte(Color shr 16);

  { Pre-calculate vertical gradient colors }
  SetLength(QCol, h);
  for i := 0 to h - 1 do
   begin
    if Reverse then
    begin
      QCol[i].r:= Byte(rc1 + (((rc2 - rc1) * i) div h));
      QCol[i].g:= Byte(gc1 + (((gc2 - gc1) * i) div h));
      QCol[i].b:= Byte(bc1 + (((bc2 - bc1) * i) div h));
    end
    else
    begin
      QCol[i].r:= Byte(rc2 + (((rc1 - rc2) * i) div h));
      QCol[i].g:= Byte(gc2 + (((gc1 - gc2) * i) div h));
      QCol[i].b:= Byte(bc2 + (((bc1 - bc2) * i) div h));
    end;
  end;

  w1:= w - 1;
  if Center
  then w:= (w shr 1) + (w and 1);

  { Initialize scanline access using pointer arithmetic for performance }
  slMain:= NativeInt(Temp.ScanLine[0]);
  slSize:= NativeInt(Temp.ScanLine[1]) - slMain;

  { Paint gradient }
  for x:= 0 to w - 1 DO
  begin
    j:= (255 - c1) + MulDiv(c1, x, w);
    slPtr:= slMain;
    for y := 0 to h - 1 do begin
      Row := PRGBArray(slPtr);
      Row[x].r:= j * (QCol[y].r - rc1) shr 8 + rc1;
      Row[x].g:= j * (QCol[y].g - gc1) shr 8 + gc1;
      Row[x].b:= j * (QCol[y].b - bc1) shr 8 + bc1;
      { Mirror the pixel to the right side if centered mode }
      if (Center) and (x < (w1 - x)) then
       begin
        Row[w1 - x].r:= Row[x].r;
        Row[w1 - x].g:= Row[x].g;
        Row[w1 - x].b:= Row[x].b;
       end;
      slPtr:= slPtr + slSize;
    end;
   end;

  BMP.Assign(Temp);
 FINALLY
   FreeAndNil(Temp);
 END;
end;
{$R+}


{ Draws a XOR pattern where the red channel = X xor Y.
  Creates a distinctive diagonal pattern useful for testing.
  TRGB32Array defined in LightVcl.Graph.Util }
procedure DrawRedPattern(BMP: TBitmap);
var
   X, Y: Integer;
   P: PRGB32Array;
begin
 Assert(BMP <> NIL, 'DrawRedPattern: BMP parameter cannot be nil');

 BMP.PixelFormat:= pf32bit;
 for Y:= 0 to BMP.Height - 1 DO
  begin
   P:= BMP.ScanLine[Y];
   for X:= 0 to BMP.Width - 1 DO
    begin
     P[X].R:= X xor Y;
     P[X].G:= 0;
     P[X].B:= 0;
     P[X].A:= 0;
    end;
  end;
end;


end.
