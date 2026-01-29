UNIT LightVcl.Graph.Alpha;

{=============================================================================================================
   Gabriel Moraru
   2023.08.05
--------------------------------------------------------------------------------------------------------------
   Alpha Blend
   For more Alpha tools see c:\MyProjects\Packages\Third party packages\GraphAlpha.pas

   Transparency in TImageList: https://stackoverflow.com/questions/7050683
---------------------------------------------------------------------------------------------------}

INTERFACE

USES
   Winapi.CommCtrl, Winapi.Windows, System.SysUtils, Vcl.Graphics, Vcl.GraphUtil, Vcl.Controls, Vcl.ImgList;

 { Transparency }
 procedure AlphaBlendBitmaps (Source: TBitmap; Destination: TCanvas; DestRect: TRect; Opacity: Byte);   overload;
 procedure AlphaBlendBitmaps (MainBitmap, SmallBitmap: TBitmap; CONST Transparency, x, y: Integer);     overload;              { Transparency for SmallBitmap is between 0% and 100%. The SmallBitmap image MUST be smaller than MainBitmap. XY are the coordinates where the small image will be blend in the main image }

 function  TransparencyBlend (MainBitmap, SmallBitmap: TBitmap; CONST TransparentColor: TColor; CONST x, y: Integer): TBitmap; { Mix two images. Pixels of 'TransparentColor' color in the SmallBitmap are 100% transparent. The SmallBitmap image MUST be smaller than MainBitmap. XY are the coordinates where the small image will be blend in the main image }

 { From Vcl.GraphUtil }
 procedure DrawTransparentBitmap(Source: TBitmap; Destination: TCanvas; DestRect: TRect; Opacity: Byte);                    overload; deprecated 'Call Vcl.GraphUtil.DrawTransparentBitmap directly!';
 procedure DrawTransparentBitmap(Source: TBitmap; SourceRect: TRect; Destination: TCanvas; DestRect: TRect; Opacity: Byte); overload; deprecated 'Call Vcl.GraphUtil.DrawTransparentBitmap directly!';

 procedure GetTransparentBitmapFromImagelist(ImageList: TImageList; Index:integer; Bitmap: TBitmap);

IMPLEMENTATION

USES
   LightVcl.Graph.Bitmap;




 





{-------------------------------------------------------------------------------------------------------------
   ALPHA BLEND
-------------------------------------------------------------------------------------------------------------}

procedure DrawTransparentBitmap(Source: TBitmap; Destination: TCanvas; DestRect: TRect; Opacity: Byte);
begin
 // Placeholder. Call Vcl.GraphUtil.DrawTransparentBitmap directly!
 Vcl.GraphUtil.DrawTransparentBitmap(Source,Destination,DestRect,Opacity);
end;


procedure DrawTransparentBitmap(Source: TBitmap; SourceRect: TRect; Destination: TCanvas; DestRect: TRect; Opacity: Byte);
begin
 // Placeholder. Call Vcl.GraphUtil.DrawTransparentBitmap directly!
 Vcl.GraphUtil.DrawTransparentBitmap(Source,SourceRect,Destination,DestRect,Opacity);
end;



{ This is WinApi based }
procedure AlphaBlendBitmaps(Source: TBitmap; Destination: TCanvas; DestRect: TRect; Opacity: Byte);
begin
  if Source = NIL
  then raise Exception.Create('AlphaBlendBitmaps: Source parameter cannot be nil');

  if Destination = NIL
  then raise Exception.Create('AlphaBlendBitmaps: Destination parameter cannot be nil');

  Vcl.GraphUtil.DrawTransparentBitmap(Source, Destination, DestRect, Opacity);
end;


TYPE
  TRGB = record    { this is same as TRGBTriple }    { used by AlphaBlendBitmaps }
    R: Byte;
    G: Byte;
    B: Byte;
  end;
  TRGBArray = array[0..32767] of TRGB;
  pRGBArray = ^TRGBArray;


{ Vcl.GraphUtil.DrawTransparentBitmap is WinApi based.
  This is not.
  Note: you can shift the x coordinate for R and G pixels to obtain stereoscopic images }
procedure AlphaBlendBitmaps(MainBitmap, SmallBitmap: TBitmap; CONST Transparency, x, y: Integer);  { Transparency for SmallBitmap is between 0% and 100%. The SmallBitmap image MUST be smaller than MainBitmap. XY are the coordinates where the small image will be blend in the main image }
VAR
   Col: integer;
   BlendRatio, BlendRatioMin, Row : integer;
   ScanLine1, ScanLine2: pRGBArray;
   ScanlinesOut : array of pRGBArray;
begin
 if MainBitmap = NIL
 then raise Exception.Create('AlphaBlendBitmaps: MainBitmap parameter cannot be nil');

 if SmallBitmap = NIL
 then raise Exception.Create('AlphaBlendBitmaps: SmallBitmap parameter cannot be nil');

 if SmallBitmap.PixelFormat <> pf24bit
 then raise Exception.Create('AlphaBlendBitmaps: SmallBitmap must be pf24bit');

 MainBitmap.PixelFormat:= pf24bit;

 { Output scanline }
 SetLength(ScanlinesOut, MainBitmap.Height);
 for Row:= 0 to MainBitmap.Height-1
  DO ScanlinesOut[Row]:= MainBitmap.ScanLine[Row];

 { Blend ratio }
 BlendRatio    := Round((Transparency / 100) * 256);
 BlendRatioMin := 256 - BlendRatio;

 { Mix }
 for Row:= y to MainBitmap.Height-1 DO
  begin
   ScanLine1:= MainBitmap.Scanline[Row];
   if Row>= SmallBitmap.Height+y then Continue;
   ScanLine2:= SmallBitmap.Scanline[Row-y];

   for Col := 0 to MainBitmap.Width-1 DO
    begin
     if Col>= SmallBitmap.Width
     then Continue;

     { Cool trick: shift the x coordinate for R and G pixels to obtain stereoscopic images }
     ScanlinesOut[Row][Col+ x].R := (BlendRatio * Scanline1[Col+ x].R + BlendRatioMin * Scanline2[Col].R) SHR 8;
     ScanlinesOut[Row][Col+ x].G := (BlendRatio * Scanline1[Col+ x].G + BlendRatioMin * Scanline2[Col].G) SHR 8;
     ScanlinesOut[Row][Col+ x].B := (BlendRatio * Scanline1[Col+ x].B + BlendRatioMin * Scanline2[Col].B) SHR 8;
    end;
  end;

 SetLength(ScanlinesOut, 0);
end;



function TransparencyBlend(MainBitmap, SmallBitmap: TBitmap; CONST TransparentColor: TColor; CONST x, y: Integer): TBitmap; { Mix two images. Pixels of 'TransparentColor' color in the SmallBitmap are 100% transparent. The SmallBitmap image MUST be smaller than MainBitmap. XY are the coordinates where the small image will be blend in the main image }
VAR
   Row, Col, MaxWidth, MaxHeight: integer;
   ScanLine2: pRGBArray;
   ScanlinesOut: array of pRGBArray;
   Color: TColor;
begin
 if MainBitmap = NIL
 then raise Exception.Create('TransparencyBlend: MainBitmap parameter cannot be nil');

 if SmallBitmap = NIL
 then raise Exception.Create('TransparencyBlend: SmallBitmap parameter cannot be nil');

 if MainBitmap.PixelFormat <> pf24bit
 then raise Exception.Create('TransparencyBlend: MainBitmap must be pf24bit');

 if SmallBitmap.PixelFormat <> pf24bit
 then raise Exception.Create('TransparencyBlend: SmallBitmap must be pf24bit');

 MaxWidth := MainBitmap.Width;
 MaxHeight:= MainBitmap.Height;

 { Output image }
 Result:= CreateBitmap(MaxWidth, MaxHeight, pf24bit);
 Result.Assign(MainBitmap);

 { Output scanline }
 SetLength(ScanlinesOut, MainBitmap.Height);
 for Row:= 0 to MainBitmap.Height-1
   DO ScanlinesOut[Row]:= Result.ScanLine[Row];

 { Mix }
 for Row:= y to MaxHeight-1 DO
  begin
   if Row>= SmallBitmap.Height+y
   then Continue;
   ScanLine2:= SmallBitmap.Scanline[Row-y];

   for Col := 0 to MaxWidth-1 DO
    begin
     if Col>= SmallBitmap.Width
     then Continue;

     Color:= RGB(Scanline2[Col].r, Scanline2[Col].g, Scanline2[Col].b);

     if Color <> TransparentColor
     then ScanlinesOut[Row][Col+ x]:= Scanline2[Col];
    end;
  end;

 SetLength(ScanlinesOut, 0);  // Why?
end;
{ Also see: How to paint on a Canvas with Transparency? - http://stackoverflow.com/questions/10342603/how-to-paint-on-a-canvas-with-transparency-and-opacity}





{ TImageList.GetBitmap fails to preserve the transparency, so we need our own utility }
(* OLD
procedure GetTransparentBitmapFromImagelist(ImageList: TImageList; Index:integer; Bitmap: TBitmap);
var
  i: Integer;
  P: Pointer;
begin
  Bitmap.SetSize(ImageList.Width, ImageList.Height);
  Bitmap.PixelFormat := pf32bit;
  if ImageList.ColorDepth = cd32Bit then
      begin
        Bitmap.Transparent := False;
        Bitmap.AlphaFormat := afDefined;
      end
  else
      Bitmap.Transparent := TRUE;

  P := Bitmap.ScanLine[Bitmap.Height - 1];   // Don't use 0 (first line) because the actual raw image data it is bottom up
  for i := 0 to Bitmap.Height-1 do
      FillChar(P^, Vcl.Graphics.BytesPerScanLine(Bitmap.Width, 32, 32) * Bitmap.Height, 0);

  ImageList_Draw(ImageList.Handle, Index, Bitmap.Canvas.Handle, 0, 0, ILD_TRANSPARENT);
end;   *)


{Clearing with Transparency: I fixed the clearing logic by using the correct scanline for each row and filling the memory with zeros (for full transparency in 32-bit RGBA mode). The fill size is calculated as Bitmap.Width * 4, as each pixel is represented by 4 bytes in 32-bit mode (RGBA).
Memory Safety: The memory of each scanline is cleared separately to avoid overwriting unintended parts of the bitmap.
AlphaChannel Handling: The AlphaFormat := afDefined ensures that the bitmap is treated as having a defined alpha channel when the ImageList has 32-bit color depth.
Use of ImageList_Draw: The function ImageList_Draw is used to draw the image into the canvas, respecting the transparency if the ILD_TRANSPARENT flag is passed.}
procedure GetTransparentBitmapFromImagelist(ImageList: TImageList; Index: Integer; Bitmap: TBitmap);
var
  i: Integer;
  ScanLinePtr: PByteArray;
begin
  if ImageList = NIL
  then raise Exception.Create('GetTransparentBitmapFromImagelist: ImageList parameter cannot be nil');

  if Bitmap = NIL
  then raise Exception.Create('GetTransparentBitmapFromImagelist: Bitmap parameter cannot be nil');

  if (Index < 0) OR (Index >= ImageList.Count)
  then raise Exception.Create('GetTransparentBitmapFromImagelist: Index out of range');

  { Set bitmap size and format }
  Bitmap.SetSize(ImageList.Width, ImageList.Height);
  Bitmap.PixelFormat := pf32bit;  // Ensure 32-bit format

  if ImageList.ColorDepth = cd32Bit
  then
    begin
      Bitmap.Transparent := False;
      Bitmap.AlphaFormat := afDefined;  // Define alpha format
    end
  else
    Bitmap.Transparent := True;

  // Clear the bitmap to fully transparent (0 alpha)
  for i := 0 to Bitmap.Height - 1 do
  begin
    ScanLinePtr := Bitmap.ScanLine[i];
    FillChar(ScanLinePtr^, Bitmap.Width * 4, 0);  // 4 bytes per pixel (RGBA)
  end;

  // Draw the image from ImageList onto the bitmap's canvas
  ImageList_Draw(ImageList.Handle, Index, Bitmap.Canvas.Handle, 0, 0, ILD_TRANSPARENT);
end;


end.


