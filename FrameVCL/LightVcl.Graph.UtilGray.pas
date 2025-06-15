UNIT LightVcl.Graph.UtilGray;

{=============================================================================================================
   Gabriel Moraru
   2024.03
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
   Functions for converting color images to gray-scale
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
   Note: This makes an average between pixels. It does not search for the DOMINANT color!
--------------------------------------------------------------------------------------------------}
function GetAverageColorPf8(GrayBmp: TBitmap): Byte;
VAR
   Cl, Rw: Integer;
   BmpLine: PByte;
   Summ: Int64;
begin
 Assert(GrayBmp.PixelFormat= pf8bit, 'The input image must be pf8bit!');

 Summ:= 0;
 for Rw:= 0 to GrayBmp.Height-1 do
   begin
     BmpLine:= GrayBmp.ScanLine[Rw];
     for Cl:= 0 to GrayBmp.Width -1 do
       Summ:= Summ + BmpLine[Cl];
   end;

 Result := Round(Summ / (GrayBmp.Height*GrayBmp.Width));
end;


function GetAverageColorPf32(GrayBmp: TBitmap): Byte;
var
  Summ: Int64;
  Row, Col: Integer;
  Pixel: PDWord;
  Blue: Byte;
begin
  Assert(GrayBmp.PixelFormat = pf32bit, 'The input image must be pf32bit!');

  Summ := 0;
  for Row := 0 to GrayBmp.Height - 1 do
  begin
    Pixel := GrayBmp.ScanLine[Row];
    for Col := 0 to GrayBmp.Width - 1 do
    begin
      // Extracting color channels from the pixel value
      Blue := Pixel^ and $FF; {
      Green := (Pixel^ shr 8) and $FF;
      Red := (Pixel^ shr 16) and $FF;
      Alpha := (Pixel^ shr 24) and $FF; }

      // All 3 color channels must have the same value, so it is enough to use only one of them.
      Summ := Summ + Blue;

      Inc(Pixel);  //Pointer math
    end;
  end;

 Result := Round(Summ / (GrayBmp.Height*GrayBmp.Width));
end;









{--------------------------------------------------------------------------------------------------
   Gray-scale palette
--------------------------------------------------------------------------------------------------}

{ Returns true if the image has a grayscale palette }
function HasGrayscalePalette(BMP: TBitmap): Boolean;
var
  ColorTable: array[0..255] of TPaletteEntry;
  I: Integer;
begin
  Result:= TRUE;
  GetPaletteEntries(BMP.Palette, 0 {Start}, 256 {Entry count}, ColorTable);
  for I := 0 to 255 do
    if (ColorTable[I].peRed <> ColorTable[I].peGreen)
    OR (ColorTable[I].peRed <> ColorTable[I].peBlue)
    then EXIT(FALSE);
end;

function HasGrayscalePalette(const FileName: string): Boolean;
begin
  VAR BMP:= TBitmap.Create;
  try
    BMP.LoadFromFile(FileName);
    Result:= HasGrayscalePalette(BMP);
  finally
    BMP.Free;
  end;
end;





// en.delphipraxis.net/topic/6656-palette-for-8-bit-greyscale-bitmap/
function CreateGrayPalette(NumColors: Integer= 256): HPALETTE;
VAR
  i  : Integer;
  lp : TMaxLogPalette;
  Grey: Byte;
begin
  lp.palVersion := $300;
  lp.palNumEntries := NumColors;
  for i := 0 to NumColors - 1 do
   begin
     Grey := i * 255 DIV NumColors;
     lp.palPalEntry[i].peRed   := Grey;
     lp.palPalEntry[i].peGreen := Grey;
     lp.palPalEntry[i].peBlue  := Grey;
     lp.palPalEntry[i].peFlags := PC_RESERVED;
   end;
  Result := CreatePalette(pLogPalette(@lp)^);  // https://learn.microsoft.com/en-us/windows/win32/api/wingdi/nf-wingdi-createpalette
end;


procedure SetBitmapGrayPalette(BMP: TBitmap);
begin
  VAR GrayPalette := CreateGrayPalette;
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
      Y = 0.21 × R + 0.72 × G + 0.07 × B.    Rec. 709 (also used by Gimp)

   Source:
     en.delphipraxis.net/topic/6501-any-example-bitmap-to-grayscale

   Also see:
     Stackoverflow.com/questions/51067460/in-delphi-how-does-tbitmap-monochrome-and-pixelformat-influcence-the-format-of
     Stackoverflow.com/questions/8559341/how-to-convert-bitmap-to-grayscale-by-pixel-intensity-using-gdi
     Entwickler-ecke.de/topic_Tbitmap+als+8+bit+grey+scale+abspeichern_86555,0.html
--------------------------------------------------------------------------------------------------}
{ Covert one color pixel to gray pixel }
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


procedure ConvertToGrayscale(BMP: TBitmap);
begin
  // Desaturate
  BMP.PixelFormat:= pf32bit;
  for var Row := 0 to BMP.Height-1 do
  begin
    var Line:= PDword(BMP.ScanLine[Row]);
    var Col := BMP.Width;
    WHILE Col > 0 DO
    begin
      Line^ := RGB2Gray(Line^);
      Inc(Line);
      Dec(Col);
    end;
  end;

  // And set the palette to gray
  SetBitmapGrayPalette(BMP);

  // This reduces the no of colors but does not make it gray. DO NOT USE IT. It will make "waves" to appear into the image (like when you convert a high color image to 256 color GIF)
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
