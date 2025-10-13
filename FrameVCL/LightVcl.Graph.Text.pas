UNIT LightVcl.Graph.Text;

{=============================================================================================================
   www.GabrielMoraru.com
   2025.10
--------------------------------------------------------------------------------------------------------------
  Features:
    Text-related routines
    Draw text with shadow

  Dependencies:
    JanFx - Install the JanFx free library, OR remove LightVcl.Graph.Text.pas from your package.

  More shadows:
    Semi-transparent text here:  stackoverflow.com/questions/360476/writing-transparent-text-on-image
    Nice shadow under controls (not under text): stackoverflow.com/questions/27359570/delphi-vcl-shadoweffect-like-fmx-tshadoweffect

  Demo: c:\Projects\Testers\gr cGraphText.pas\Tester.dpr
--------------------------------------------------------------------------------------------------}

INTERFACE

USES
   Winapi.Windows, System.SysUtils, System.Types, Vcl.Graphics,
   janFX;

{-------------------------------------------------------------------------------------------------------------
   Text routines
-------------------------------------------------------------------------------------------------------------}
 procedure DrawTextCentered    (Canvas: TCanvas; CONST Text: string; Rect: TRect);
 function  CenterTextX         (BMP: TBitmap;    CONST Text: string): Integer;              overload;      { Returns the X coordinate where we must draw the text if we want to have the text centered in bitmap }
 function  CenterTextY         (BMP: TBitmap;    CONST Text: string): Integer;              overload;
 function  CenterTextX         (Canvas: TCanvas; CONST Text: string; R: TRect): Integer;    overload;
 function  CenterTextY         (Canvas: TCanvas; CONST Text: string; R: TRect): Integer;    overload;

 function  GetFontHeight       (aFont : TFont) : integer;                                                  { Get fornt height when we don't have a canvas }


{-------------------------------------------------------------------------------------------------------------
   Text shadows
-------------------------------------------------------------------------------------------------------------}
CONST
   clTextShadow= TColor($303035);

 procedure DrawTextShadowBox     (BMP: TBitmap;     CONST Text: string; AlignTop: Boolean; ShadowColor: TColor= clTextShadow; ShadowOpacity: Byte= 20; Blur: Byte= 2); overload;
 procedure DrawTextShadowBox     (aCanvas: TCanvas; CONST Text: string; X, Y: Integer;     ShadowColor: TColor= clTextShadow; ShadowOpacity: Byte= 20; Blur: Byte= 2); overload;
 procedure DrawTextShadow3DSoft  (aCanvas: TCanvas; CONST Text: string; X, Y: Integer;     ShadowColor: TColor= clTextShadow);                                   { Faster. No transparency }
 procedure DrawTextShadow3DHard  (aCanvas: TCanvas; CONST Text: string; X, Y: Integer;     ShadowColor: TColor= clTextShadow; TopShadow: Boolean= FALSE);

 procedure DrawTextOutline       (aCanvas: TCanvas; CONST Text: string; X, Y: Integer; SolidMiddle: Boolean= TRUE);    { Very poor }
 procedure DrawTextXOR           (DC: HDC; Font: TFont; CONST Text: string; X, Y: integer);                            { Very poor }

 procedure ShadowDownLeft        (BMP: TBitmap);  deprecated 'Use JanFX directly';
 procedure ShadowDownRight       (BMP: TBitmap);  deprecated 'Use JanFX directly';


IMPLEMENTATION


function GetFontHeight(aFont : TFont) : integer;     //not tested. ce face?
var
   DC        : HDC;
   SysMetrics: TTextMetric;
   Metrics   : TTextMetric;
begin
 DC := GetDC(0);
 GetTextMetrics(DC, SysMetrics);
 SelectObject  (DC, aFont.Handle);
 GetTextMetrics(DC, Metrics);
 ReleaseDC(0, DC);
 Result := Metrics.tmHeight;
end;



{-------------------------------------------------------------------------------------------------------------
   DRAW CENTERED TEXT

 Before calling these functions, you might want to set:
    aCanvas.Font.Color := clBlack
    aCanvas.Brush.Style:= bsClear
-------------------------------------------------------------------------------------------------------------}

procedure DrawTextCentered(Canvas: TCanvas; CONST Text: string; Rect: TRect);
CONST
   uFormat: Cardinal = DT_CENTER OR DT_VCENTER OR {DT_TOP OR } DT_SINGLELINE OR DT_NOCLIP;
begin
 DrawText(Canvas.Handle, Text, Length(Text), Rect, uFormat);
end;


function CenterTextX(BMP: TBitmap; CONST Text: string): Integer;  { Returns the X coordinate where we must drew the text }
begin
 Result:= BMP.Canvas.TextWidth(Text);
 Result:= (BMP.Width - Result) DIV 2;
end;


function CenterTextX(Canvas: TCanvas; CONST Text: string; R: TRect): Integer;
begin
 Result:= Canvas.TextWidth(Text);
 Result:= (R.Width - Result) DIV 2;
end;


function CenterTextY(Canvas: TCanvas; CONST Text: string; R: TRect): Integer;
begin
 Result:= Canvas.TextHeight(Text);
 Result:= (R.Height - Result) DIV 2;
end;


function CenterTextY(BMP: TBitmap; CONST Text: string): Integer;
begin
 Result:= BMP.Canvas.TextHeight(Text);
 Result:= (BMP.Height - Result) DIV 2;
end;




{-------------------------------------------------------------------------------------------------------------
  DrawTextShadowBox
    Draws text in a semi-transparent rectangle with shadow text.
    The shadow text is blended to the background and then blurred.

  Variant:
     1: Draws text in a box that is as wide as the BMP and can be aligned to top or bottom. ToDo: let user center text instead of aligning it
     2: Draws text in a box that is as wide as text and is placed into the image at coordinates x,y

  Parameters:
     Opacity a value from 0-255. 0 => Shadow is completelly transparent
     To set the Font color/size, the caller should do: aCanvas.Font.Size:= x

  Issues:
     The blurring function cuts too suddenly. The rectangle that was blurred is too visible. Do a blur that slowly fades at the edges.
     Might be slow becuase of the alpha blending and because of the blur.

  Important!
     The input img must be pf24bit.
     When the AlphaFormat member is AC_SRC_ALPHA, the source bitmap must be 32 bpp. If it is not, the AlphaBlend function will fail.

  For more shadow text see also:
     http://www.delphi-central.com/tutorials/AlphaBlend.aspx
     http://www.codeproject.com/Articles/21071/Glow-and-Shadow-Effects-using-Windows-GDI  Similar code. Uses GDI+
     http://docwiki.embarcadero.com/Libraries/Tokyo/en/FMX.Effects.TShadowEffect    FMX:
     C:\MyProjects\Packages\Third party packages\ShadowText.pas
-------------------------------------------------------------------------------------------------------------}
procedure DrawTextShadowBox(BMP: TBitmap; CONST Text: string; AlignTop: Boolean; ShadowColor: TColor= clTextShadow; ShadowOpacity: Byte= 20; Blur: Byte= 2);
VAR
   Shadow: Vcl.Graphics.TBitmap;
   BlendFunc: BLENDFUNCTION;
   x, y: Integer;
   BmpRect: TRect; { Rectangle in the original bitmap where we want to draw the shadowed text }
   ShadowRect: TRect;
   TextWidth, TextHeight: Integer;
   OriginalColor: TColor;
begin
  Assert(BMP.PixelFormat= pf24bit, 'Wrong pixel format!!');
  OriginalColor:= bmp.Canvas.Font.Color;
  TextWidth := BMP.Canvas.TextWidth (Text);
  TextHeight:= BMP.Canvas.TextHeight(Text);

  { Write the shadow on a separate bitmap (overlay) }
  Shadow := TBitmap.Create;
  TRY
    { Bitmap setup }
    Shadow.Canvas.Font.Assign(BMP.Canvas.Font);
    Shadow.PixelFormat:= pf24bit;
    Shadow.SetSize(BMP.Width, TextHeight);

    { Bitmap rectangle as big as ShadowBMP }
    ShadowRect.Left:= 0;
    ShadowRect.Top := 0;
    ShadowRect.Right := Shadow.Width;
    ShadowRect.Bottom:= Shadow.Height;

    { Fill shadow rectangle }
    Shadow.Canvas.Brush.Color := clBlack;                { In AlphaBlend, Black is always 100% transparent. So, paint Shadow completely Black. }
    Shadow.Canvas.FillRect(ShadowRect);

    BmpRect.Left  := 0;
    BmpRect.Right := Shadow.Width;
    if AlignTop
    then BmpRect.Top := 0
    else BmpRect.Top := BMP.Height- TextHeight;
    BmpRect.Bottom:= BmpRect.Top+ TextHeight;

    { Blend rectangle with orig image }                  { Use the AlphaBlend function to overlay the bitmap holding the text on top of the bitmap holding the original image. }
    BlendFunc.BlendOp    := AC_SRC_OVER;
    BlendFunc.BlendFlags := 0;
    BlendFunc.SourceConstantAlpha := ShadowOpacity;
    BlendFunc.AlphaFormat         := 0;                  //AC_SRC_ALPHA; //  if I put this back, the shadow will be completly invisible when merged with a white source image
    WinApi.Windows.AlphaBlend(BMP.Canvas.Handle, BmpRect.Left, BmpRect.Top, BmpRect.Right, TextHeight, Shadow.Canvas.Handle, 0, 0, Shadow.Width, Shadow.Height, BlendFunc);

    { Copy the blended area back to the Shadow bmp }
    Shadow.Canvas.CopyRect(ShadowRect, BMP.Canvas, BmpRect);

    { Diagonal shadow }
    x:= (BMP.Width  - TextWidth) DIV 2;   // Find center
    Shadow.Canvas.Brush.Style:= bsClear;
    Shadow.Canvas.Font.Color := ShadowColor;
    Shadow.Canvas.TextOut(x, 0, Text);

    { Blur the shadow }
    janFX.GaussianBlur(Shadow, Blur, 1);

    { Paste it back }
    BMP.Canvas.CopyRect(BmpRect, Shadow.Canvas, ShadowRect);
  FINALLY
    FreeAndNil(Shadow);
  END;

  { Draw actual text at 100% opacity }
  if AlignTop
  then y := 0
  else y := BMP.Height- TextHeight;
  BMP.Canvas.Brush.Style:= bsClear;
  BMP.Canvas.Font.Color := OriginalColor;
  BMP.Canvas.TextOut(x, y, Text);
end;



procedure DrawTextShadowBox(aCanvas: TCanvas; CONST Text: string;  X, Y: Integer; ShadowColor: TColor= clTextShadow; ShadowOpacity: Byte= 20; Blur: Byte= 2);
VAR
   Shadow: Vcl.Graphics.TBitmap;
   BlendFunc: BLENDFUNCTION;
   H, W: Integer;
   OriginalColor: TColor;
   R, R2: TRect;
CONST Edge= 5;
begin
 OriginalColor:= aCanvas.Font.Color;

 { Write the shadow on a separate bitmap (overlay) }
 Shadow := TBitmap.Create;
 TRY
   { Assign font }
   Shadow.Canvas.Font.Assign(aCanvas.Font);
   Shadow.PixelFormat:= pf24bit;

   { Compute overlay size }
   W:= Shadow.Canvas.TextWidth (Text);
   H:= Shadow.Canvas.TextHeight(Text);
   Shadow.SetSize(W, H);

   { Fill shadow rectangle }
   R:= Rect(0, 0, Shadow.Width, Shadow.Height);
   Shadow.Canvas.Brush.Color := clBlack;                               { In AlphaBlend, Black is always 100% transparent. So, paint Shadow completely Black. }
   Shadow.Canvas.FillRect(R);

   { Blend rectangle with orig image }                                 { Use the AlphaBlend function to overlay the bitmap holding the text on top of the bitmap holding the original image. }
   BlendFunc.BlendOp := AC_SRC_OVER;
   BlendFunc.BlendFlags := 0;
   BlendFunc.SourceConstantAlpha := ShadowOpacity;
   BlendFunc.AlphaFormat := 0;                                         //AC_SRC_ALPHA; //  if I put this back, the shadow will be completly invisible when merged with a white source image
   WinApi.Windows.AlphaBlend(aCanvas.Handle, x, y, Shadow.Width, Shadow.Height, Shadow.Canvas.Handle, 0, 0, Shadow.Width, Shadow.Height, BlendFunc);

   { Copy the blended area back to the Shadow bmp }
   R2:= rect(x, y, x+Shadow.Width, y+Shadow.Height);
   Shadow.Canvas.CopyRect(R, aCanvas, R2);

   { Diagonal shadow }
   Shadow.Canvas.Brush.Style:= bsClear;
   Shadow.Canvas.Font.Color := ShadowColor;
   Shadow.Canvas.TextOut(0, 0, Text);

   { Blur the shadow }
   janFX.GaussianBlur(Shadow, blur, 1);

   { Paste it back }
   aCanvas.CopyRect(R2, Shadow.Canvas, R);
 FINALLY
   FreeAndNil(Shadow);
 END;

 { Draw actual text at 100% opacity }
 aCanvas.Brush.Style:= bsClear;
 aCanvas.Font.Color := OriginalColor;
 aCanvas.TextOut(x, y, Text);
end;



{TODO: Create a functions with bkg color autodetext: when the background is light use dark text else use light text } {
procedure DrawTextShadowBox(BMP: TBitmap; CONST Text: string; AlignTop: Boolean; ShadowOpacity: Byte= 20; Blur: Byte= 2);
begin
  {var Clr: TColor;
   if JanFX.ColorIsLight(BMP)
   then Clr:= clBlack
   else Clr:= White;
   DrawTextShadowBox(BMP, Text, AlignTop, Clr, ShadowOpacity, Blur);
end;  }



{ Draws a (cheap) shadow projected arround the text, by simply drawing the same text shifted by a few pixels
  No transparency. }
procedure DrawTextShadow3DSoft(aCanvas: TCanvas; CONST Text: string; X, Y: Integer; ShadowColor: TColor= clTextShadow);
VAR OriginalTextColor: TColor;
begin
 OriginalTextColor:= aCanvas.Font.Color;

 { Write the shadow first }
 aCanvas.Brush.Style := bsClear;
 aCanvas.Font.Color  := ShadowColor;
 aCanvas.TextOut(x+1, y+1, Text);         { Diagonal left shadow }
 aCanvas.TextOut(x+1, y,   Text);         { Left shadow }

 { Then put the text on top }
 aCanvas.Brush.Style := bsClear;
 aCanvas.Font.Color  := OriginalTextColor;
 aCanvas.TextOut(x, y, Text);
end;



{ Same as above, but the shadow is more visible }
procedure DrawTextShadow3DHard(aCanvas: TCanvas; CONST Text: string; X, Y: Integer; ShadowColor: TColor= clTextShadow; TopShadow: Boolean= FALSE);
VAR
   OriginalTextColor, ShadowLight: TColor;
   x2, y2: Integer;
CONST
  ShadowDist= 1;
begin
 OriginalTextColor:= aCanvas.Font.Color;
 aCanvas.Brush.Style:= bsClear;   //todo: if FSolidBkg

 { First we draw the lighter shadows }
 ShadowLight:= TColor($EEEEEE); /// LightenColor(ShadowColor, 50);
 aCanvas.Font.Color:= ShadowLight;

 // Up
 if TopShadow then
  begin
   y2:= y -ShadowDist;   // up left
   x2:= x -ShadowDist;
   aCanvas.TextOut(x2, y2, Text);

   y2:= y -ShadowDist;   // up right
   x2:= x +ShadowDist;
   aCanvas.TextOut(x2, y2, Text);
  end;

 // Down left
 y2:= y +ShadowDist;
 x2:= x -ShadowDist;
 aCanvas.TextOut(x2, y2, Text);

 { Darker shadows }
 aCanvas.Font.Color:= ShadowColor;

 y2:= y;                          // right more
 x2:= x +ShadowDist+1;
 aCanvas.TextOut(x2, y2, Text);

 y2:= y +ShadowDist;              // down right
 x2:= x +ShadowDist;
 aCanvas.TextOut(x2, y2, Text);

 y2:= y +ShadowDist+2;            // down right more
 x2:= x +ShadowDist+1;
 aCanvas.TextOut(x2, y2, Text);

 y2:= y+ShadowDist+1;             // down under
 x2:= x;
 aCanvas.TextOut(x2, y2, Text);

 // Actual text
 aCanvas.Font.Color:= OriginalTextColor;
 aCanvas.TextOut(x, y, Text);
end;





procedure ShadowDownLeft(BMP: TBitmap);
begin
 JanFX.ShadowDownLeft(BMP);
end;

procedure ShadowDownRight(BMP: TBitmap);
begin
 JanFX.ShadowDownRight(BMP);
end;




{ Surrounds the text with a border.
  Parameters:
    SolidMiddle        -> makes the middle text solid/transparent
    Canvas.Brush.Color -> gives the FILL color
    Canvas.Pen.Color   -> gives the BORDER color

  Result quality: Very poor
  Source: http://delphidabbler.com/tips/55
}
procedure DrawTextOutline(aCanvas: TCanvas; CONST Text: string; X, Y: Integer; SolidMiddle: Boolean= TRUE);
VAR OldBkMode: Integer;                                     { Stores previous background mode }
begin
 if SolidMiddle
 then aCanvas.Brush.Style:= bsSolid                         { Hint: Set Canvas.Brush.Style to bsClear to draw only the outline not also the middle, else set it to bsSolid }
 else aCanvas.Brush.Style:= bsClear;
 OldBkMode := SetBkMode(aCanvas.Handle, TRANSPARENT);
 BeginPath(aCanvas.Handle);
 aCanvas.TextOut(X, Y, Text);
 EndPath(aCanvas.Handle);
 StrokeAndFillPath(aCanvas.Handle);
 SetBkMode(aCanvas.Handle, OldBkMode);
end;


{ Draw 'inverted' text on a canvas.
  Call it like this:  DrawTextNOT(Canvas.Handle, Canvas.Font, 'This is a test.', 20, 100);

  Result quality: Very poor
  Source: http://stackoverflow.com/questions/5053282/how-to-draw-a-not-colored-text }
procedure DrawTextXOR(DC: HDC; Font: TFont; CONST Text: string; X, Y: integer);
VAR BMP: TBitmap;
begin
 BMP:= TBitmap.Create;
 TRY
  BMP.Canvas.Font.Assign(Font);
  WITH BMP.Canvas.TextExtent(Text)
    DO BMP.SetSize(cx, cy);
  BMP.Canvas.Brush.Color := clBlack;
  BMP.Canvas.FillRect(Rect(0, 0, BMP.Width, BMP.Height));
  BMP.Canvas.Font.Color := clWhite;
  BMP.Canvas.TextOut(0, 0, Text);
  BitBlt(DC, X, Y, BMP.Width, BMP.Height, BMP.Canvas.Handle, 0, 0, SRCINVERT);
 FINALLY
  FreeAndNil(BMP);
 END;
end;


end.
