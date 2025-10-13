UNIT LightVcl.Graph.FX.Gradient;

{=============================================================================================================
   2023.08.05
   Source: ?
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




procedure GradientFillCanvas(ACanvas: TCanvas; const AStartColor, AEndColor: TColor; const ARect: TRect; const Direction: TGradientDirection);
begin
  Vcl.GraphUtil.GradientFillCanvas(ACanvas, AStartColor, AEndColor, ARect, Direction);
end;


{--------------------------------------------------------
   Draws a gradient over a canvas
   Gradient orientation is defined in WinApi.Windows as:
       GRADIENT_FILL_RECT_H, GRADIENT_FILL_RECT_V,
       GRADIENT_FILL_TRIANGLE, GRADIENT_FILL_OP_FLAG   <- this has no effect
--------------------------------------------------------}

procedure GradientFill(Control: TWinControl; Color1, Color2: TColor; Orientation: Integer= GRADIENT_FILL_RECT_V; Transparency: Word= 255);

   procedure VertexColor(VAR aVertex: triVertex; aColor: TColor);
   VAR
     ColorID: string;
   begin
     if NOT ColorToIdent(aColor, ColorID)
     then ColorID := IntToStr(aColor);
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
  gRect: GRADIENT_RECT = (UpperLeft: 0; LowerRight: 1); // Create a GRADIENT_RECT structure that references the TRIVERTEX vertices.
VAR
  DC: HDC;
  vert: array [0 .. 1] of triVertex;
  aCanvas: TCanvas;
begin
  if Control is TCustomControl
  then aCanvas:= TCustomControlEx(Control).Canvas
  else
    if Control is TCustomForm
    then aCanvas:= TCustomForm(Control).Canvas
    else EXIT;

  if Assigned(aCanvas) then
   begin
    DC := HDC(aCanvas.Handle);
    vert[0] := UpperLeft (Color1);
    vert[1] := LowerRight(Color2);
    winapi.Windows.GradientFill(DC, @vert[0], 2, @gRect, 1, Orientation);
   end;
end;



{$R-}
procedure VistaGradient(BMP: TBitmap; const c1: Byte; const c3, c4: TColor; const Center, Reverse: Boolean);    { http://rmklever.com/?tag=thumbnails }
TYPE
  PRGB = ^TRGB;
  TRGB = record b, g, r : Byte; end;
  PRGBArray = ^TRGBArray;
  TRGBARRAY = array[0..0] of TRGB;
VAR
  rc1, gc1, bc1, rc2, gc2, bc2: Integer;
  x, y, w, h, j: Integer;
  i, w1: Integer;
  Row: PRGBArray;
  slMain, slSize, slPtr: Integer;
  Color: Integer;
  QCol: Array of TRGB;
  Temp: TBitmap;
begin
 Temp:= TBitmap.Create;
 TRY
  Temp.PixelFormat:= pf24Bit;
  SetLargeSize(Temp, BMP.Width, BMP.Height);

  h:= Temp.Height;
  w:= Temp.Width;

  Color:= ColorToRGB(c3);
  rc1 := Byte(Color);
  gc1 := Byte(Color shr 8);
  bc1 := Byte(Color shr 16);
  Color:= ColorToRGB(c4);
  rc2 := Byte(Color);
  gc2 := Byte(Color shr 8);
  bc2 := Byte(Color shr 16);
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
  slMain:= Integer(Temp.ScanLine[0]);             // Init scanline accsess
  slSize:= Integer(Temp.ScanLine[1]) - slMain;

  for x:= 0 to w - 1 DO
  begin                    // Paint gradient
    j:= (255 - c1) + MulDiv(c1, x, w);
    slPtr:= slMain;
    for y := 0 to h - 1 do begin
      Row := PRGBArray(slPtr);
      Row[x].r:= j * (QCol[y].r - rc1) shr 8 + rc1;
      Row[x].g:= j * (QCol[y].g - gc1) shr 8 + gc1;
      Row[x].b:= j * (QCol[y].b - bc1) shr 8 + bc1;
      if (Center) and (x < (w1 - x)) then
       begin
        Row[w1 - x].r:= Row[x].r;
        Row[w1 - x].g:= Row[x].g;
        Row[w1 - x].b:= Row[x].b;
       end;
      slPtr:= slPtr + slSize;
    end;
   end;

  //QCol:= nil;
  BMP.Assign(Temp);
 FINALLY
   FreeandNil(Temp);
 END;
end;


// TRGB32Array defined in cGraphicUtil
procedure DrawRedPattern(BMP: TBitmap);     { Draws a nice rectangle patern }
var
   X, Y: Integer;
   P: PRGB32Array;
begin
 //DoubleBuffered := TRUE; del
 with BMP DO
   begin
     PixelFormat := pf32bit;
     for Y := 0 to height - 1 DO
      begin
       P := ScanLine[Y];
       X := 0;
       while X < width DO
        begin
         P[X].R := X xor Y;
         P[X].G := 0;
         P[X].B := 0;
         P[X].A := 0;
         inc(X);
        end;
      end;
   end;
end;
{$R+}


end.
