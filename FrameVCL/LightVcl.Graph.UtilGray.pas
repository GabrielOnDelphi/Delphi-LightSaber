UNIT LightVcl.Graph.UtilGray;

{=============================================================================================================
   2026.01.30
   Gabriel Moraru
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Functions for converting color images to gray-scale.
   Includes palette manipulation and average color calculation for grayscale bitmaps.
-------------------------------------------------------------------------------------------------------------}

//see: https://stackoverflow.com/questions/78343032/convert-color-bitmap-to-pf8bit-grayscale/78344683?noredirect=1#comment138123752_78344683

INTERFACE

USES
   Winapi.Windows, System.SysUtils, System.Types,
   Vcl.Graphics;


function  RGB2Gray  (Color: TColor): TColor;

procedure ConvertToGrayscale(BMP: TBitmap);

function  GetAverageColorPf8 (GrayBmp: TBitmap): Byte;          overload;
function  GetAverageColorPf32(GrayBmp: TBitmap): Byte;          overload;

procedure SetBitmapGrayPalette(BMP: TBitmap);

function  HasGrayscalePalette(const FileName: string): Boolean; overload;
function  HasGrayscalePalette(BMP: TBitmap): Boolean;           overload;


IMPLEMENTATION




{--------------------------------------------------------------------------------------------------
   GET AVERAGE COLOR from a gray-scale image.
   Note: This calculates the arithmetic mean of all pixel values.
         It does NOT search for the DOMINANT (most frequent) color!
--------------------------------------------------------------------------------------------------}
function GetAverageColorPf8(GrayBmp: TBitmap): Byte;
VAR
   Cl, Rw: Integer;
   BmpLine: PByte;
   Summ: Int64;
   PixelCount: Int64;
begin
 Assert(GrayBmp <> NIL, 'GetAverageColorPf8: GrayBmp parameter cannot be nil');
 Assert(GrayBmp.PixelFormat = pf8bit, 'GetAverageColorPf8: The input image must be pf8bit!');

 PixelCount:= Int64(GrayBmp.Height) * GrayBmp.Width;
 if PixelCount = 0
 then EXIT(0);  // Empty bitmap - return black

 Summ:= 0;
 for Rw:= 0 to GrayBmp.Height-1 do
   begin
     BmpLine:= GrayBmp.ScanLine[Rw];
     for Cl:= 0 to GrayBmp.Width -1 do
       Summ:= Summ + BmpLine[Cl];
   end;

 Result:= Round(Summ / PixelCount);
end;


function GetAverageColorPf32(GrayBmp: TBitmap): Byte;
var
  Summ: Int64;
  Row, Col: Integer;
  Pixel: PDWord;
  Blue: Byte;
  PixelCount: Int64;
begin
  Assert(GrayBmp <> NIL, 'GetAverageColorPf32: GrayBmp parameter cannot be nil');
  Assert(GrayBmp.PixelFormat = pf32bit, 'GetAverageColorPf32: The input image must be pf32bit!');

  PixelCount:= Int64(GrayBmp.Height) * GrayBmp.Width;
  if PixelCount = 0
  then EXIT(0);  // Empty bitmap - return black

  Summ:= 0;
  for Row:= 0 to GrayBmp.Height - 1 do
  begin
    Pixel:= GrayBmp.ScanLine[Row];
    for Col:= 0 to GrayBmp.Width - 1 do
    begin
      // Extracting blue channel from the pixel value (BGRA format in memory).
      // For a grayscale image, R=G=B so we only need one channel.
      Blue:= Pixel^ and $FF;
      { Green := (Pixel^ shr 8) and $FF;
        Red := (Pixel^ shr 16) and $FF;
        Alpha := (Pixel^ shr 24) and $FF; }

      Summ:= Summ + Blue;
      Inc(Pixel);  // Pointer arithmetic
    end;
  end;

  Result:= Round(Summ / PixelCount);
end;









{--------------------------------------------------------------------------------------------------
   Gray-scale palette
--------------------------------------------------------------------------------------------------}

{ Returns true if the image has a grayscale palette.
  A grayscale palette has R=G=B for all 256 palette entries. }
function HasGrayscalePalette(BMP: TBitmap): Boolean;
var
  ColorTable: array[0..255] of TPaletteEntry;
  I: Integer;
begin
  Assert(BMP <> NIL, 'HasGrayscalePalette: BMP parameter cannot be nil');

  Result:= TRUE;
  GetPaletteEntries(BMP.Palette, 0 {Start}, 256 {Entry count}, ColorTable);
  for I:= 0 to 255 do
    if (ColorTable[I].peRed <> ColorTable[I].peGreen)
    OR (ColorTable[I].peRed <> ColorTable[I].peBlue)
    then EXIT(FALSE);
end;

function HasGrayscalePalette(const FileName: string): Boolean;
begin
  Assert(FileName <> '', 'HasGrayscalePalette: FileName cannot be empty');

  VAR BMP:= TBitmap.Create;
  TRY
    BMP.LoadFromFile(FileName);
    Result:= HasGrayscalePalette(BMP);
  FINALLY
    FreeAndNil(BMP);
  END;
end;





{ Creates a grayscale palette with the specified number of colors (default 256).
  The palette entries range from black (0) to white (255).
  Source: en.delphipraxis.net/topic/6656-palette-for-8-bit-greyscale-bitmap/ }
function CreateGrayPalette(NumColors: Integer= 256): HPALETTE;
VAR
  i: Integer;
  lp: TMaxLogPalette;
  Grey: Byte;
begin
  Assert(NumColors > 1, 'CreateGrayPalette: NumColors must be greater than 1');
  Assert(NumColors <= 256, 'CreateGrayPalette: NumColors cannot exceed 256');

  lp.palVersion:= $300;
  lp.palNumEntries:= NumColors;
  for i:= 0 to NumColors - 1 do
   begin
     // Use (NumColors - 1) as divisor to ensure full range 0..255
     Grey:= i * 255 DIV (NumColors - 1);
     lp.palPalEntry[i].peRed  := Grey;
     lp.palPalEntry[i].peGreen:= Grey;
     lp.palPalEntry[i].peBlue := Grey;
     lp.palPalEntry[i].peFlags:= PC_RESERVED;
   end;
  Result:= CreatePalette(pLogPalette(@lp)^);  // https://learn.microsoft.com/en-us/windows/win32/api/wingdi/nf-wingdi-createpalette
end;


{ Assigns a 256-entry grayscale palette to the bitmap.
  The bitmap takes ownership of the palette handle. }
procedure SetBitmapGrayPalette(BMP: TBitmap);
begin
  Assert(BMP <> NIL, 'SetBitmapGrayPalette: BMP parameter cannot be nil');

  VAR GrayPalette:= CreateGrayPalette;
  if GrayPalette <> 0
  then BMP.Palette:= GrayPalette;
end;


{function CreateGrayPalette_: HPALETTE; // does not work!
VAR
  PaletteEntries: array[0..255] of TPaletteEntry;
begin
  for VAR i:= 0 to 255 do
  begin
    PaletteEntries[i].peRed   := i;
    PaletteEntries[i].peGreen := i;
    PaletteEntries[i].peBlue  := i;
    PaletteEntries[i].peFlags := 0;
  end;
  Result:= CreatePalette(@PaletteEntries);
end; }





{--------------------------------------------------------------------------------------------------
   CONVERT COLOR IMAGE TO GRAYSCALE
---------------------------------------------------------------------------------------------------
   How to:
     1. Change the pixelformat to pf32bit
     2. Desaturate the RGB

   Formula:
      Y = 0.21 * R + 0.72 * G + 0.07 * B.    Rec. 709 (also used by Gimp)

   Source:
     en.delphipraxis.net/topic/6501-any-example-bitmap-to-grayscale

   Also see:
     Stackoverflow.com/questions/51067460/in-delphi-how-does-tbitmap-monochrome-and-pixelformat-influcence-the-format-of
     Stackoverflow.com/questions/8559341/how-to-convert-bitmap-to-grayscale-by-pixel-intensity-using-gdi
     Entwickler-ecke.de/topic_Tbitmap+als+8+bit+grey+scale+abspeichern_86555,0.html
--------------------------------------------------------------------------------------------------}

{ Convert one color pixel to gray pixel using luminance weights.
  Range checking is disabled for performance in pixel-level operations. }
{$R-}
function RGB2Gray(Color: TColor): TColor;
CONST
  LuminanceR = 54;   // Luminance multipliers
  LuminanceG = 184;
  LuminanceB = 18;
VAR
  Luminance: Byte;
begin
  Luminance :=
    (((Color AND $00FF0000) SHR 16 * LuminanceR) +
     ((Color AND $0000FF00) SHR 8  * LuminanceG) +
     ((Color AND $000000FF)        * LuminanceB)) SHR 8;

  Result := (Color and $FF000000) OR (Luminance SHL 16) OR (Luminance SHL 8) OR Luminance;
end;


{ UNUSED!
  Slower (probably) than the other one. }
function RGB2Gray_(Color: TColor): TColor;
var Target: Byte;
begin
  Color:= ColorToRGB(Color);    {  GetRValue which accepts cardinals. However, TColor is integer. So I need a conversion else I get e RangeCheckError one line below.       See this: http://stackoverflow.com/questions/9809687/windows-getrvalue-accepts-cardinals-but-tcolor-is-integer}
  Target := Round(
          (0.30 * GetRValue(Color)) +
          (0.59 * GetGValue(Color)) +
          (0.11 * GetBValue(Color)));
  Result:= RGB(Target, Target, Target);
end;


{ Converts a color bitmap to 8-bit grayscale.
  The process: 1) Convert to 32-bit, 2) Desaturate pixels, 3) Set gray palette, 4) Convert to 8-bit.
  Warning: Converting to pf8bit may cause banding artifacts (similar to GIF color reduction). }
procedure ConvertToGrayscale(BMP: TBitmap);
begin
  Assert(BMP <> NIL, 'ConvertToGrayscale: BMP parameter cannot be nil');

  // Step 1: Convert to 32-bit for pixel manipulation
  BMP.PixelFormat:= pf32bit;

  // Step 2: Desaturate each pixel
  for var Row:= 0 to BMP.Height-1 do
  begin
    var Line:= PDword(BMP.ScanLine[Row]);
    var Col:= BMP.Width;
    WHILE Col > 0 DO
    begin
      Line^:= RGB2Gray(Line^);
      Inc(Line);
      Dec(Col);
    end;
  end;

  // Step 3: Set the grayscale palette
  SetBitmapGrayPalette(BMP);

  // Step 4: Reduce to 8-bit
  // Note: This may cause banding/dithering artifacts (like converting high-color to 256-color GIF)
  BMP.PixelFormat:= pf8bit;
end;
{$R+}



(*  del
function RGB2Gray2(Color: TColor): Byte;
const
  LuminanceR = 54;   // Luminance multipliers
  LuminanceG = 184;
  LuminanceB = 18;
var
  Luminance: Byte;
begin
  Luminance :=
    (((Color AND $00FF0000) SHR 16 * LuminanceR) +
     ((Color AND $0000FF00) SHR 8  * LuminanceG) +
     ((Color AND $000000FF)        * LuminanceB)) SHR 8;
  Result := Luminance;
end;


procedure ConvertToGrayscale2(BMP: TBitmap);
var
  Row, Col: Integer;
  GrayValue: Byte;
begin
  // Desaturate
  BMP.PixelFormat := pf32bit;
  for Row := 0 to BMP.Height - 1 do
  begin
    for Col := 0 to BMP.Width - 1 do
    begin
      GrayValue := RGB2Gray2(BMP.Canvas.Pixels[Col, Row]);
      BMP.Canvas.Pixels[Col, Row] := RGB(GrayValue, GrayValue, GrayValue);
    end;
  end;
  // Convert to 8-bit grayscale (if not already)
  if BMP.PixelFormat <> pf8bit then
  begin
    SetBitmapGrayPalette(BMP);
    BMP.PixelFormat := pf8bit;
  end;
end;   *)

{.$R-}



end.
