UNIT LightVcl.Graph.Alpha;

{=============================================================================================================
   Gabriel Moraru
   2026.01.30
--------------------------------------------------------------------------------------------------------------
   Alpha Blend

   Alpha blending functions for combining bitmaps with transparency effects.

   Key functions:
     - AlphaBlendBitmaps: Blend SmallBitmap onto MainBitmap with adjustable opacity (0-100%)
     - TransparencyBlend: Overlay SmallBitmap onto MainBitmap using a specific color as transparent
     - GetTransparentBitmapFromImagelist: Extract bitmap from TImageList preserving alpha channel

   Notes:
     - Most functions require pf24bit pixel format for scanline-based operations
     - Windows bitmaps store pixels in BGR order (Blue, Green, Red), not RGB

   See also:
     - Vcl.GraphUtil.DrawTransparentBitmap for WinAPI-based alpha blending
     - TImageList transparency: https://stackoverflow.com/questions/7050683
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
  { Windows DIBs store pixels in BGR order, not RGB.
    Field order must match TRGBTriple: Blue at offset 0, then Green, then Red.
    This is critical for correct color channel blending and transparency comparison. }
  TBGR = record
    B: Byte;
    G: Byte;
    R: Byte;
  end;
  TBGRArray = array[0..32767] of TBGR;
  pBGRArray = ^TBGRArray;


{ Pure Pascal alpha blending (no WinAPI).
  Blends SmallBitmap onto MainBitmap at position (x, y) with specified transparency.

  Parameters:
    MainBitmap   - The destination bitmap (will be modified in place)
    SmallBitmap  - The source bitmap to blend onto MainBitmap
    Transparency - Opacity of MainBitmap: 0% = SmallBitmap fully visible, 100% = MainBitmap fully visible
    x, y         - Position in MainBitmap where SmallBitmap will be placed

  Constraints:
    - Both bitmaps must be pf24bit (or MainBitmap will be converted)
    - SmallBitmap should fit within MainBitmap bounds at position (x, y)
    - x, y must be non-negative

  Note: You can shift the x coordinate for R and G pixels to obtain stereoscopic images }
procedure AlphaBlendBitmaps(MainBitmap, SmallBitmap: TBitmap; CONST Transparency, x, y: Integer);
VAR
   Col, DestCol: Integer;
   BlendRatio, BlendRatioMin, Row: Integer;
   ScanLine1, ScanLine2: pBGRArray;
   ScanlinesOut: array of pBGRArray;
   MaxCol: Integer;
begin
 if MainBitmap = NIL
 then raise Exception.Create('AlphaBlendBitmaps: MainBitmap parameter cannot be nil');

 if SmallBitmap = NIL
 then raise Exception.Create('AlphaBlendBitmaps: SmallBitmap parameter cannot be nil');

 if SmallBitmap.PixelFormat <> pf24bit
 then raise Exception.Create('AlphaBlendBitmaps: SmallBitmap must be pf24bit');

 if (x < 0) OR (y < 0)
 then raise Exception.Create('AlphaBlendBitmaps: x and y must be non-negative');

 MainBitmap.PixelFormat:= pf24bit;

 { Cache scanlines for faster access }
 SetLength(ScanlinesOut, MainBitmap.Height);
 for Row:= 0 to MainBitmap.Height-1
  DO ScanlinesOut[Row]:= MainBitmap.ScanLine[Row];

 { Blend ratio: 256-based fixed point for fast integer math }
 BlendRatio    := Round((Transparency / 100) * 256);
 BlendRatioMin := 256 - BlendRatio;

 { Calculate max column to prevent writing beyond MainBitmap width }
 MaxCol:= MainBitmap.Width - x;
 if MaxCol > SmallBitmap.Width
 then MaxCol:= SmallBitmap.Width;

 { Mix scanlines }
 for Row:= y to MainBitmap.Height-1 DO
  begin
   if Row >= SmallBitmap.Height + y
   then Break;  { No more rows to blend }

   ScanLine1:= MainBitmap.Scanline[Row];
   ScanLine2:= SmallBitmap.Scanline[Row-y];

   for Col:= 0 to MaxCol-1 DO
    begin
     DestCol:= Col + x;
     { Blend each color channel using fixed-point arithmetic }
     ScanlinesOut[Row][DestCol].R:= (BlendRatio * Scanline1[DestCol].R + BlendRatioMin * Scanline2[Col].R) SHR 8;
     ScanlinesOut[Row][DestCol].G:= (BlendRatio * Scanline1[DestCol].G + BlendRatioMin * Scanline2[Col].G) SHR 8;
     ScanlinesOut[Row][DestCol].B:= (BlendRatio * Scanline1[DestCol].B + BlendRatioMin * Scanline2[Col].B) SHR 8;
    end;
  end;

 { Dynamic array is auto-freed when it goes out of scope, but explicit cleanup for clarity }
 SetLength(ScanlinesOut, 0);
end;



{ Color-key transparency blend (similar to "green screen" effect).
  Overlays SmallBitmap onto MainBitmap, treating pixels of TransparentColor as fully transparent.

  Parameters:
    MainBitmap       - The background bitmap
    SmallBitmap      - The foreground bitmap to overlay
    TransparentColor - Pixels in SmallBitmap matching this color will not be copied
    x, y             - Position in result where SmallBitmap will be placed

  Returns:
    A new bitmap containing the blended result (caller must free)

  Constraints:
    - Both bitmaps must be pf24bit
    - x, y must be non-negative

  Also see: How to paint on a Canvas with Transparency?
  http://stackoverflow.com/questions/10342603/how-to-paint-on-a-canvas-with-transparency-and-opacity }
function TransparencyBlend(MainBitmap, SmallBitmap: TBitmap; CONST TransparentColor: TColor; CONST x, y: Integer): TBitmap;
VAR
   Row, Col, DestCol, MaxWidth, MaxHeight, MaxCol: Integer;
   ScanLine2: pBGRArray;
   ScanlinesOut: array of pBGRArray;
   PixelColor: TColor;
begin
 if MainBitmap = NIL
 then raise Exception.Create('TransparencyBlend: MainBitmap parameter cannot be nil');

 if SmallBitmap = NIL
 then raise Exception.Create('TransparencyBlend: SmallBitmap parameter cannot be nil');

 if MainBitmap.PixelFormat <> pf24bit
 then raise Exception.Create('TransparencyBlend: MainBitmap must be pf24bit');

 if SmallBitmap.PixelFormat <> pf24bit
 then raise Exception.Create('TransparencyBlend: SmallBitmap must be pf24bit');

 if (x < 0) OR (y < 0)
 then raise Exception.Create('TransparencyBlend: x and y must be non-negative');

 MaxWidth := MainBitmap.Width;
 MaxHeight:= MainBitmap.Height;

 { Create output image as copy of MainBitmap }
 Result:= CreateBitmap(MaxWidth, MaxHeight, pf24bit);
 Result.Assign(MainBitmap);

 { Cache output scanlines for faster access }
 SetLength(ScanlinesOut, MainBitmap.Height);
 for Row:= 0 to MainBitmap.Height-1
   DO ScanlinesOut[Row]:= Result.ScanLine[Row];

 { Calculate max column to prevent writing beyond output width }
 MaxCol:= MaxWidth - x;
 if MaxCol > SmallBitmap.Width
 then MaxCol:= SmallBitmap.Width;

 { Overlay non-transparent pixels }
 for Row:= y to MaxHeight-1 DO
  begin
   if Row >= SmallBitmap.Height + y
   then Break;  { No more rows to process }

   ScanLine2:= SmallBitmap.Scanline[Row-y];

   for Col:= 0 to MaxCol-1 DO
    begin
     { Build TColor from BGR scanline data - note: RGB() expects R,G,B order }
     PixelColor:= RGB(Scanline2[Col].R, Scanline2[Col].G, Scanline2[Col].B);

     if PixelColor <> TransparentColor then
      begin
       DestCol:= Col + x;
       ScanlinesOut[Row][DestCol]:= Scanline2[Col];
      end;
    end;
  end;

 { Explicit cleanup - dynamic array auto-frees but this documents intent }
 SetLength(ScanlinesOut, 0);
end;





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


