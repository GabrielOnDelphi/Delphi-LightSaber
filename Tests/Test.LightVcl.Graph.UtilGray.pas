unit Test.LightVcl.Graph.UtilGray;

{=============================================================================================================
   Unit tests for LightVcl.Graph.UtilGray.pas
   Tests grayscale conversion, palette manipulation, and average color calculation.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Winapi.Windows,
  Vcl.Graphics;

type
  [TestFixture]
  TTestGraphUtilGray = class
  private
    FBitmap: TBitmap;
    procedure CreateColorBitmap(Width, Height: Integer; Color: TColor);
    procedure CreateGrayscaleBitmap8(Width, Height: Integer; GrayValue: Byte);
    procedure CreateGrayscaleBitmap32(Width, Height: Integer; GrayValue: Byte);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { RGB2Gray Tests }
    [Test]
    procedure TestRGB2Gray_Black;

    [Test]
    procedure TestRGB2Gray_White;

    [Test]
    procedure TestRGB2Gray_Red;

    [Test]
    procedure TestRGB2Gray_Green;

    [Test]
    procedure TestRGB2Gray_Blue;

    [Test]
    procedure TestRGB2Gray_PreservesAlpha;

    { GetAverageColorPf8 Tests }
    [Test]
    procedure TestGetAverageColorPf8_AllBlack;

    [Test]
    procedure TestGetAverageColorPf8_AllWhite;

    [Test]
    procedure TestGetAverageColorPf8_MidGray;

    [Test]
    procedure TestGetAverageColorPf8_EmptyBitmap;

    { GetAverageColorPf32 Tests }
    [Test]
    procedure TestGetAverageColorPf32_AllBlack;

    [Test]
    procedure TestGetAverageColorPf32_AllWhite;

    [Test]
    procedure TestGetAverageColorPf32_MidGray;

    [Test]
    procedure TestGetAverageColorPf32_EmptyBitmap;

    { ConvertToGrayscale Tests }
    [Test]
    procedure TestConvertToGrayscale_BasicCall;

    [Test]
    procedure TestConvertToGrayscale_ResultIs8Bit;

    [Test]
    procedure TestConvertToGrayscale_PreservesDimensions;

    [Test]
    procedure TestConvertToGrayscale_BlackStaysBlack;

    [Test]
    procedure TestConvertToGrayscale_WhiteStaysWhite;

    { SetBitmapGrayPalette Tests }
    [Test]
    procedure TestSetBitmapGrayPalette_BasicCall;

    [Test]
    procedure TestSetBitmapGrayPalette_CreatesPalette;

    { HasGrayscalePalette Tests }
    [Test]
    procedure TestHasGrayscalePalette_TrueForGray;

    [Test]
    procedure TestHasGrayscalePalette_FalseForColor;
  end;

implementation

uses
  LightVcl.Graph.UtilGray;


procedure TTestGraphUtilGray.Setup;
begin
  FBitmap:= TBitmap.Create;
end;


procedure TTestGraphUtilGray.TearDown;
begin
  FreeAndNil(FBitmap);
end;


procedure TTestGraphUtilGray.CreateColorBitmap(Width, Height: Integer; Color: TColor);
var
  Row, Col: Integer;
begin
  FBitmap.SetSize(Width, Height);
  FBitmap.PixelFormat:= pf32bit;
  for Row:= 0 to Height - 1 do
    for Col:= 0 to Width - 1 do
      FBitmap.Canvas.Pixels[Col, Row]:= Color;
end;


procedure TTestGraphUtilGray.CreateGrayscaleBitmap8(Width, Height: Integer; GrayValue: Byte);
var
  Row, Col: Integer;
  Line: PByte;
begin
  FBitmap.SetSize(Width, Height);
  FBitmap.PixelFormat:= pf8bit;
  SetBitmapGrayPalette(FBitmap);

  for Row:= 0 to Height - 1 do
  begin
    Line:= FBitmap.ScanLine[Row];
    for Col:= 0 to Width - 1 do
      Line[Col]:= GrayValue;
  end;
end;


procedure TTestGraphUtilGray.CreateGrayscaleBitmap32(Width, Height: Integer; GrayValue: Byte);
var
  Row, Col: Integer;
  Line: PDWord;
  GrayColor: DWord;
begin
  FBitmap.SetSize(Width, Height);
  FBitmap.PixelFormat:= pf32bit;

  // Create BGRA value with same gray for R, G, B
  GrayColor:= $FF000000 OR (GrayValue SHL 16) OR (GrayValue SHL 8) OR GrayValue;

  for Row:= 0 to Height - 1 do
  begin
    Line:= FBitmap.ScanLine[Row];
    for Col:= 0 to Width - 1 do
    begin
      Line^:= GrayColor;
      Inc(Line);
    end;
  end;
end;


{ RGB2Gray Tests }

procedure TTestGraphUtilGray.TestRGB2Gray_Black;
var
  Result: TColor;
begin
  Result:= RGB2Gray($00000000);  // Black
  Assert.AreEqual(TColor($00000000), Result, 'Black should remain black');
end;


procedure TTestGraphUtilGray.TestRGB2Gray_White;
var
  Result: TColor;
  GrayR, GrayG, GrayB: Byte;
begin
  Result:= RGB2Gray($00FFFFFF);  // White
  GrayR:= (Result SHR 16) and $FF;
  GrayG:= (Result SHR 8) and $FF;
  GrayB:= Result and $FF;

  // White should produce a very light gray (close to 255)
  Assert.IsTrue(GrayR > 250, 'Red channel should be near white');
  Assert.AreEqual(GrayR, GrayG, 'R and G should be equal');
  Assert.AreEqual(GrayR, GrayB, 'R and B should be equal');
end;


procedure TTestGraphUtilGray.TestRGB2Gray_Red;
var
  Result: TColor;
  GrayR, GrayG, GrayB: Byte;
begin
  Result:= RGB2Gray($00FF0000);  // Red
  GrayR:= (Result SHR 16) and $FF;
  GrayG:= (Result SHR 8) and $FF;
  GrayB:= Result and $FF;

  // Pure red with luminance ~0.21 should give ~54
  Assert.IsTrue(GrayR > 40, 'Red should produce some gray');
  Assert.IsTrue(GrayR < 80, 'Red should not produce too much gray');
  Assert.AreEqual(GrayR, GrayG, 'All channels should be equal');
  Assert.AreEqual(GrayR, GrayB, 'All channels should be equal');
end;


procedure TTestGraphUtilGray.TestRGB2Gray_Green;
var
  Result: TColor;
  GrayR, GrayG, GrayB: Byte;
begin
  Result:= RGB2Gray($0000FF00);  // Green
  GrayR:= (Result SHR 16) and $FF;
  GrayG:= (Result SHR 8) and $FF;
  GrayB:= Result and $FF;

  // Pure green with luminance ~0.72 should give ~184
  Assert.IsTrue(GrayR > 150, 'Green should produce significant gray');
  Assert.IsTrue(GrayR < 200, 'Green should not produce too much gray');
  Assert.AreEqual(GrayR, GrayG, 'All channels should be equal');
  Assert.AreEqual(GrayR, GrayB, 'All channels should be equal');
end;


procedure TTestGraphUtilGray.TestRGB2Gray_Blue;
var
  Result: TColor;
  GrayR, GrayG, GrayB: Byte;
begin
  Result:= RGB2Gray($000000FF);  // Blue
  GrayR:= (Result SHR 16) and $FF;
  GrayG:= (Result SHR 8) and $FF;
  GrayB:= Result and $FF;

  // Pure blue with luminance ~0.07 should give ~18
  Assert.IsTrue(GrayR > 10, 'Blue should produce some gray');
  Assert.IsTrue(GrayR < 30, 'Blue should produce little gray');
  Assert.AreEqual(GrayR, GrayG, 'All channels should be equal');
  Assert.AreEqual(GrayR, GrayB, 'All channels should be equal');
end;


procedure TTestGraphUtilGray.TestRGB2Gray_PreservesAlpha;
var
  Result: TColor;
  Alpha: Byte;
begin
  Result:= RGB2Gray($80808080);  // 50% alpha, mid gray
  Alpha:= (Result SHR 24) and $FF;
  Assert.AreEqual(Byte($80), Alpha, 'Alpha channel should be preserved');
end;


{ GetAverageColorPf8 Tests }

procedure TTestGraphUtilGray.TestGetAverageColorPf8_AllBlack;
var
  Avg: Byte;
begin
  CreateGrayscaleBitmap8(10, 10, 0);  // All black
  Avg:= GetAverageColorPf8(FBitmap);
  Assert.AreEqual(Byte(0), Avg, 'Average of all-black bitmap should be 0');
end;


procedure TTestGraphUtilGray.TestGetAverageColorPf8_AllWhite;
var
  Avg: Byte;
begin
  CreateGrayscaleBitmap8(10, 10, 255);  // All white
  Avg:= GetAverageColorPf8(FBitmap);
  Assert.AreEqual(Byte(255), Avg, 'Average of all-white bitmap should be 255');
end;


procedure TTestGraphUtilGray.TestGetAverageColorPf8_MidGray;
var
  Avg: Byte;
begin
  CreateGrayscaleBitmap8(10, 10, 128);  // Mid gray
  Avg:= GetAverageColorPf8(FBitmap);
  Assert.AreEqual(Byte(128), Avg, 'Average of mid-gray bitmap should be 128');
end;


procedure TTestGraphUtilGray.TestGetAverageColorPf8_EmptyBitmap;
var
  Avg: Byte;
begin
  FBitmap.SetSize(0, 0);
  FBitmap.PixelFormat:= pf8bit;
  Avg:= GetAverageColorPf8(FBitmap);
  Assert.AreEqual(Byte(0), Avg, 'Average of empty bitmap should be 0');
end;


{ GetAverageColorPf32 Tests }

procedure TTestGraphUtilGray.TestGetAverageColorPf32_AllBlack;
var
  Avg: Byte;
begin
  CreateGrayscaleBitmap32(10, 10, 0);  // All black
  Avg:= GetAverageColorPf32(FBitmap);
  Assert.AreEqual(Byte(0), Avg, 'Average of all-black bitmap should be 0');
end;


procedure TTestGraphUtilGray.TestGetAverageColorPf32_AllWhite;
var
  Avg: Byte;
begin
  CreateGrayscaleBitmap32(10, 10, 255);  // All white
  Avg:= GetAverageColorPf32(FBitmap);
  Assert.AreEqual(Byte(255), Avg, 'Average of all-white bitmap should be 255');
end;


procedure TTestGraphUtilGray.TestGetAverageColorPf32_MidGray;
var
  Avg: Byte;
begin
  CreateGrayscaleBitmap32(10, 10, 128);  // Mid gray
  Avg:= GetAverageColorPf32(FBitmap);
  Assert.AreEqual(Byte(128), Avg, 'Average of mid-gray bitmap should be 128');
end;


procedure TTestGraphUtilGray.TestGetAverageColorPf32_EmptyBitmap;
var
  Avg: Byte;
begin
  FBitmap.SetSize(0, 0);
  FBitmap.PixelFormat:= pf32bit;
  Avg:= GetAverageColorPf32(FBitmap);
  Assert.AreEqual(Byte(0), Avg, 'Average of empty bitmap should be 0');
end;


{ ConvertToGrayscale Tests }

procedure TTestGraphUtilGray.TestConvertToGrayscale_BasicCall;
begin
  CreateColorBitmap(10, 10, clRed);
  ConvertToGrayscale(FBitmap);
  Assert.Pass('ConvertToGrayscale should not raise exception');
end;


procedure TTestGraphUtilGray.TestConvertToGrayscale_ResultIs8Bit;
begin
  CreateColorBitmap(10, 10, clGreen);
  ConvertToGrayscale(FBitmap);
  Assert.AreEqual(pf8bit, FBitmap.PixelFormat, 'Result should be 8-bit');
end;


procedure TTestGraphUtilGray.TestConvertToGrayscale_PreservesDimensions;
begin
  CreateColorBitmap(50, 30, clBlue);
  ConvertToGrayscale(FBitmap);
  Assert.AreEqual(50, FBitmap.Width, 'Width should be preserved');
  Assert.AreEqual(30, FBitmap.Height, 'Height should be preserved');
end;


procedure TTestGraphUtilGray.TestConvertToGrayscale_BlackStaysBlack;
var
  Line: PByte;
begin
  CreateColorBitmap(10, 10, clBlack);
  ConvertToGrayscale(FBitmap);
  Line:= FBitmap.ScanLine[0];
  Assert.AreEqual(Byte(0), Line[0], 'Black should convert to palette index 0');
end;


procedure TTestGraphUtilGray.TestConvertToGrayscale_WhiteStaysWhite;
var
  Line: PByte;
begin
  CreateColorBitmap(10, 10, clWhite);
  ConvertToGrayscale(FBitmap);
  Line:= FBitmap.ScanLine[0];
  Assert.AreEqual(Byte(255), Line[0], 'White should convert to palette index 255');
end;


{ SetBitmapGrayPalette Tests }

procedure TTestGraphUtilGray.TestSetBitmapGrayPalette_BasicCall;
begin
  FBitmap.SetSize(10, 10);
  FBitmap.PixelFormat:= pf8bit;
  SetBitmapGrayPalette(FBitmap);
  Assert.Pass('SetBitmapGrayPalette should not raise exception');
end;


procedure TTestGraphUtilGray.TestSetBitmapGrayPalette_CreatesPalette;
begin
  FBitmap.SetSize(10, 10);
  FBitmap.PixelFormat:= pf8bit;
  SetBitmapGrayPalette(FBitmap);
  Assert.IsTrue(FBitmap.Palette <> 0, 'Palette handle should be non-zero');
end;


{ HasGrayscalePalette Tests }

procedure TTestGraphUtilGray.TestHasGrayscalePalette_TrueForGray;
begin
  CreateGrayscaleBitmap8(10, 10, 128);
  Assert.IsTrue(HasGrayscalePalette(FBitmap), 'Grayscale bitmap should have grayscale palette');
end;


procedure TTestGraphUtilGray.TestHasGrayscalePalette_FalseForColor;
var
  ColorTable: array[0..255] of TPaletteEntry;
  i: Integer;
  LogPal: TMaxLogPalette;
begin
  // Create a bitmap with a non-grayscale palette
  FBitmap.SetSize(10, 10);
  FBitmap.PixelFormat:= pf8bit;

  // Create a rainbow palette (not grayscale)
  LogPal.palVersion:= $300;
  LogPal.palNumEntries:= 256;
  for i:= 0 to 255 do
  begin
    LogPal.palPalEntry[i].peRed  := i;
    LogPal.palPalEntry[i].peGreen:= (i * 2) mod 256;  // Different from R
    LogPal.palPalEntry[i].peBlue := (i * 3) mod 256;  // Different from R and G
    LogPal.palPalEntry[i].peFlags:= 0;
  end;
  FBitmap.Palette:= CreatePalette(pLogPalette(@LogPal)^);

  Assert.IsFalse(HasGrayscalePalette(FBitmap), 'Color palette should not be detected as grayscale');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestGraphUtilGray);

end.
