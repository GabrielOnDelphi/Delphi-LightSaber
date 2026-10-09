unit Test.LightVcl.Graph.Bitmap;

{=============================================================================================================
   Unit tests for LightVcl.Graph.Bitmap.pas
   Tests bitmap creation, manipulation, text centering, RAM estimation, and orientation functions.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Types,
  Winapi.Windows,   { Before Vcl.Graphics: Winapi.Windows also declares a TBitmap }
  Vcl.Graphics,
  Vcl.ExtCtrls;

type
  [TestFixture]
  TTestGraphBitmap = class
  private
    FBitmap: TBitmap;
    procedure FillBitmapWithColor(BMP: TBitmap; Color: TColor);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { CreateBitmap Tests }
    [Test]
    procedure TestCreateBitmap_BasicCall;

    [Test]
    procedure TestCreateBitmap_CorrectDimensions;

    [Test]
    procedure TestCreateBitmap_DefaultPixelFormat;

    [Test]
    procedure TestCreateBitmap_CustomPixelFormat;

    { CreateBlankBitmap Tests }
    [Test]
    procedure TestCreateBlankBitmap_BasicCall;

    [Test]
    procedure TestCreateBlankBitmap_FillsWithColor;

    { SetLargeSize Tests }
    [Test]
    procedure TestSetLargeSize_BasicCall;

    [Test]
    procedure TestSetLargeSize_NilBitmap;

    [Test]
    procedure TestSetLargeSize_InvalidWidth;

    [Test]
    procedure TestSetLargeSize_InvalidHeight;

    { ClearImage Tests }
    [Test]
    procedure TestClearImage_NilImage;

    { ClearBitmap Tests }
    [Test]
    procedure TestClearBitmap_BasicCall;

    [Test]
    procedure TestClearBitmap_NilBitmap;

    { FillBitmap Tests }
    [Test]
    procedure TestFillBitmap_BasicCall;

    [Test]
    procedure TestFillBitmap_NilBitmap;

    [Test]
    procedure TestFillBitmap_FillsCorrectly;

    { CenterText Tests }
    [Test]
    procedure TestCenterText_BasicCall;

    [Test]
    procedure TestCenterText_NilBitmap;

    [Test]
    procedure TestCenterText_EmptyString;

    [Test]
    procedure TestCenterText_WithRFont;

    [Test]
    procedure TestCenterText_WithFontParams;

    { GetBitmapRamSize Tests }
    [Test]
    procedure TestGetBitmapRamSize_BasicCall;

    [Test]
    procedure TestGetBitmapRamSize_NilBitmap;

    [Test]
    procedure TestGetBitmapRamSize_ReturnsPositiveValue;

    { PredictBitmapRamSize Tests }
    [Test]
    procedure TestPredictBitmapRamSize_BasicCall;

    [Test]
    procedure TestPredictBitmapRamSize_WithBitmap;

    [Test]
    procedure TestPredictBitmapRamSize_NilBitmap;

    [Test]
    procedure TestPredictBitmapRamSize_CorrectCalculation;

    { IsPanoramic Tests }
    [Test]
    procedure TestIsPanoramic_WithBitmap_BasicCall;

    [Test]
    procedure TestIsPanoramic_WithBitmap_NilBitmap;

    { The 3 tests for the IsPanoramic(Width, Height: Integer) overload moved to Test.LightCore.Math.pas }

    { AspectIsSmaller Tests }
    [Test]
    procedure TestAspectIsSmaller_WithBitmap_BasicCall;

    [Test]
    procedure TestAspectIsSmaller_WithBitmap_NilBitmap;

    [Test]
    procedure TestAspectIsSmaller_WithDimensions_Smaller;

    [Test]
    procedure TestAspectIsSmaller_WithDimensions_Larger;

    [Test]
    procedure TestAspectIsSmaller_ZeroHeight;

    { AspectOrientation Tests }
    [Test]
    procedure TestAspectOrientation_WithBitmap_Landscape;

    [Test]
    procedure TestAspectOrientation_WithBitmap_Portrait;

    [Test]
    procedure TestAspectOrientation_WithBitmap_Square;

    [Test]
    procedure TestAspectOrientation_WithBitmap_NilBitmap;

    [Test]
    procedure TestAspectOrientation_WithDimensions_Landscape;

    [Test]
    procedure TestAspectOrientation_WithDimensions_Portrait;

    [Test]
    procedure TestAspectOrientation_WithDimensions_Square;

    { IsLandscape Tests }
    [Test]
    procedure TestIsLandscape_Landscape;

    [Test]
    procedure TestIsLandscape_Portrait;

    [Test]
    procedure TestIsLandscape_Square;

    { GetImageScale Tests }
    [Test]
    procedure TestGetImageScale_WithBitmaps_NilInput;

    [Test]
    procedure TestGetImageScale_WithBitmaps_NilDesktop;

    [Test]
    procedure TestGetImageScale_Large;

    [Test]
    procedure TestGetImageScale_Small;

    [Test]
    procedure TestGetImageScale_Tiny;

    [Test]
    procedure TestGetImageScale_TinyButTall;

    [Test]
    procedure TestGetImageScale_InvalidThreshold;

    { EnlargeCanvas Tests }
    [Test]
    procedure TestEnlargeCanvas_BasicCall;

    [Test]
    procedure TestEnlargeCanvas_NilBitmap;

    [Test]
    procedure TestEnlargeCanvas_CorrectDimensions;

    { CenterBitmap Tests }
    [Test]
    procedure TestCenterBitmap_BasicCall;

    [Test]
    procedure TestCenterBitmap_NilSource;

    [Test]
    procedure TestCenterBitmap_NilDest;

    [Test]
    procedure TestCenterBitmap_SameSize;

    [Test]
    procedure TestCenterBitmap_SourceSmaller;

    [Test]
    procedure TestCenterBitmap_SourceLarger;

    { Thread safety: no TBitmapCanvas DC may be left for the main thread's FreeMemoryContexts }
    [Test]
    procedure TestFillBitmap_LeavesNoCanvasDC;

    [Test]
    procedure TestCenterBitmap_LeavesNoCanvasDC;

    [Test]
    procedure TestCenterText_LeavesNoCanvasDC;

    { RFont Tests }
    [Test]
    procedure TestRFont_Clear_DefaultValues;

    [Test]
    procedure TestRFont_Clear_WithParams;

    [Test]
    procedure TestRFont_AssignTo_BasicCall;

    [Test]
    procedure TestRFont_AssignTo_NilFont;

    [Test]
    procedure TestRFont_AssignTo_CopiesValues;
  end;

implementation

uses
  LightVcl.Graph.Bitmap, LightVcl.Graph.FX;


{ Pixel helpers for pf24bit bitmaps. They work through ScanLine, so they create no TBitmapCanvas. }

{ Fills a pf24bit bitmap through ScanLine }
procedure FillScanLines24(BMP: TBitmap; R, G, B: Byte);
VAR
  x, y: Integer;
  Pixel: PRGBTriple;
begin
  for y:= 0 to BMP.Height-1 do
    begin
      Pixel:= BMP.ScanLine[y];
      for x:= 0 to BMP.Width-1 do
        begin
          Pixel.rgbtRed  := R;
          Pixel.rgbtGreen:= G;
          Pixel.rgbtBlue := B;
          Inc(Pixel);
        end;
    end;
end;


function PixelIsRed24(BMP: TBitmap; X, Y: Integer): Boolean;
VAR Pixel: PRGBTriple;
begin
  Pixel:= BMP.ScanLine[Y];
  Inc(Pixel, X);
  Result:= (Pixel.rgbtRed = 255) AND (Pixel.rgbtGreen = 0) AND (Pixel.rgbtBlue = 0);
end;


{ Counts the pixels that have exactly the color Color }
function CountColor24(BMP: TBitmap; Color: TColor): Integer;
VAR
  X, Y: Integer;
  Pixel: PRGBTriple;
begin
  Result:= 0;
  for Y:= 0 to BMP.Height- 1 do
    begin
      Pixel:= BMP.ScanLine[Y];
      for X:= 0 to BMP.Width- 1 do
        begin
          if TColor(RGB(Pixel.rgbtRed, Pixel.rgbtGreen, Pixel.rgbtBlue)) = Color
          then Inc(Result);
          Inc(Pixel);
        end;
    end;
end;


function CountNonWhite24(BMP: TBitmap): Integer;
VAR
  X, Y: Integer;
  Pixel: PRGBTriple;
begin
  Result:= 0;
  for Y:= 0 to BMP.Height- 1 do
    begin
      Pixel:= BMP.ScanLine[Y];
      for X:= 0 to BMP.Width- 1 do
        begin
          if (Pixel.rgbtRed <> 255) OR (Pixel.rgbtGreen <> 255) OR (Pixel.rgbtBlue <> 255)
          then Inc(Result);
          Inc(Pixel);
        end;
    end;
end;


{ Counts the pixels in which A and B differ. A and B have the same size. }
function CountDifferentPixels24(A, B: TBitmap): Integer;
VAR
  X, Y: Integer;
  PixelA, PixelB: PRGBTriple;
begin
  Result:= 0;
  for Y:= 0 to A.Height- 1 do
    begin
      PixelA:= A.ScanLine[Y];
      PixelB:= B.ScanLine[Y];
      for X:= 0 to A.Width- 1 do
        begin
          if (PixelA.rgbtRed <> PixelB.rgbtRed) OR (PixelA.rgbtGreen <> PixelB.rgbtGreen) OR (PixelA.rgbtBlue <> PixelB.rgbtBlue)
          then Inc(Result);
          Inc(PixelA);
          Inc(PixelB);
        end;
    end;
end;


{ The smallest rectangle that holds every pixel that is not white. Right and Bottom are inclusive. }
function InkBox24(BMP: TBitmap): TRect;
VAR
  X, Y: Integer;
  Pixel: PRGBTriple;
begin
  Result:= Rect(MaxInt, MaxInt, -1, -1);
  for Y:= 0 to BMP.Height- 1 do
    begin
      Pixel:= BMP.ScanLine[Y];
      for X:= 0 to BMP.Width- 1 do
        begin
          if (Pixel.rgbtRed <> 255) OR (Pixel.rgbtGreen <> 255) OR (Pixel.rgbtBlue <> 255) then
            begin
              if X < Result.Left   then Result.Left  := X;
              if X > Result.Right  then Result.Right := X;
              if Y < Result.Top    then Result.Top   := Y;
              if Y > Result.Bottom then Result.Bottom:= Y;
            end;
          Inc(Pixel);
        end;
    end;
end;


{ Draws Text centered on BMP with plain VCL calls: the picture that the CenterText overloads must produce }
procedure DrawCenteredReference(BMP: TBitmap; CONST Text, FontName: string; FontSize: Integer; FontColor: TColor);
begin
  BMP.Canvas.Font.Quality:= fqNonAntialiased;
  BMP.Canvas.Font.Name   := FontName;
  BMP.Canvas.Font.Size   := FontSize;
  BMP.Canvas.Font.Color  := FontColor;
  BMP.Canvas.Brush.Style := bsClear;
  BMP.Canvas.TextOut((BMP.Width- BMP.Canvas.TextWidth(Text)) DIV 2, (BMP.Height- BMP.Canvas.TextHeight(Text)) DIV 2, Text);
end;


{ Gives the canvas of BMP a font other than the one the test asks CenterText to use, non-antialiased so that every glyph pixel has exactly the font color }
procedure SetOtherFont(BMP: TBitmap);
begin
  BMP.Canvas.Font.Quality:= fqNonAntialiased;
  BMP.Canvas.Font.Name   := 'Courier New';
  BMP.Canvas.Font.Size   := 8;
  BMP.Canvas.Font.Color  := clBlack;
end;


procedure TTestGraphBitmap.Setup;
begin
  FBitmap:= TBitmap.Create;
  FBitmap.Width:= 100;
  FBitmap.Height:= 80;
  FBitmap.PixelFormat:= pf24bit;
  FillBitmapWithColor(FBitmap, clWhite);
end;


procedure TTestGraphBitmap.TearDown;
begin
  FreeAndNil(FBitmap);
end;


procedure TTestGraphBitmap.FillBitmapWithColor(BMP: TBitmap; Color: TColor);
begin
  BMP.Canvas.Brush.Color:= Color;
  BMP.Canvas.FillRect(Rect(0, 0, BMP.Width, BMP.Height));
end;


{ CreateBitmap Tests }

procedure TTestGraphBitmap.TestCreateBitmap_BasicCall;
var
  BMP: TBitmap;
begin
  BMP:= CreateBitmap(200, 150);
  TRY
    Assert.AreEqual(200, BMP.Width,  'CreateBitmap must give the asked width');
    Assert.AreEqual(150, BMP.Height, 'CreateBitmap must give the asked height');
    Assert.AreEqual(pf24bit, BMP.PixelFormat, 'The default pixel format is pf24bit');
    Assert.AreEqual(bmDIB, BMP.HandleType, 'A pf24bit bitmap is a DIB');
    { A pf24bit row holds 200 * 3 = 600 bytes, already a multiple of 4, so it has no padding }
    Assert.AreEqual(600, Integer(Abs(NativeInt(BMP.ScanLine[0]) - NativeInt(BMP.ScanLine[1]))), 'Row stride of a 200-pixel wide pf24bit DIB');
  FINALLY
    FreeAndNil(BMP);
  END;
end;


procedure TTestGraphBitmap.TestCreateBitmap_CorrectDimensions;
var
  BMP: TBitmap;
begin
  BMP:= CreateBitmap(200, 150);
  TRY
    Assert.AreEqual(200, BMP.Width, 'Width should be 200');
    Assert.AreEqual(150, BMP.Height, 'Height should be 150');
  FINALLY
    FreeAndNil(BMP);
  END;
end;


procedure TTestGraphBitmap.TestCreateBitmap_DefaultPixelFormat;
var
  BMP: TBitmap;
begin
  BMP:= CreateBitmap(100, 100);
  TRY
    Assert.AreEqual(pf24bit, BMP.PixelFormat, 'Default pixel format should be pf24bit');
  FINALLY
    FreeAndNil(BMP);
  END;
end;


procedure TTestGraphBitmap.TestCreateBitmap_CustomPixelFormat;
var
  BMP: TBitmap;
begin
  BMP:= CreateBitmap(100, 100, pf32bit);
  TRY
    Assert.AreEqual(pf32bit, BMP.PixelFormat, 'Pixel format should be pf32bit');
  FINALLY
    FreeAndNil(BMP);
  END;
end;


{ CreateBlankBitmap Tests }

procedure TTestGraphBitmap.TestCreateBlankBitmap_BasicCall;
var
  BMP: TBitmap;
begin
  BMP:= CreateBlankBitmap(200, 150);
  TRY
    Assert.AreEqual(200, BMP.Width,  'CreateBlankBitmap must give the asked width');
    Assert.AreEqual(150, BMP.Height, 'CreateBlankBitmap must give the asked height');
    Assert.AreEqual(pf24bit, BMP.PixelFormat, 'The default pixel format is pf24bit');
    { The default background is clBlack: all 200 * 150 = 30000 pixels }
    Assert.AreEqual(30000, CountColor24(BMP, clBlack), 'Every pixel must have the default background color, black');
  FINALLY
    FreeAndNil(BMP);
  END;
end;


procedure TTestGraphBitmap.TestCreateBlankBitmap_FillsWithColor;
var
  BMP: TBitmap;
  CenterPixel: TColor;
begin
  BMP:= CreateBlankBitmap(100, 100, clRed);
  TRY
    CenterPixel:= BMP.Canvas.Pixels[50, 50];
    Assert.AreEqual(TColor(clRed), CenterPixel, 'Bitmap should be filled with red');
  FINALLY
    FreeAndNil(BMP);
  END;
end;


{ SetLargeSize Tests }

procedure TTestGraphBitmap.TestSetLargeSize_BasicCall;
begin
  Assert.WillNotRaiseAny(
    procedure
    begin
      SetLargeSize(FBitmap, 200, 200);
    end,
    'SetLargeSize(FBitmap, 200, 200) must not raise');

  Assert.AreEqual(200, FBitmap.Width, 'Width should be 200');
  Assert.AreEqual(200, FBitmap.Height, 'Height should be 200');
end;


procedure TTestGraphBitmap.TestSetLargeSize_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      SetLargeSize(NIL, 100, 100);
    end,
    Exception,
    'SetLargeSize(NIL, 100, 100) must raise Exception');
end;


procedure TTestGraphBitmap.TestSetLargeSize_InvalidWidth;
begin
  Assert.WillRaise(
    procedure
    begin
      SetLargeSize(FBitmap, 0, 100);
    end,
    Exception,
    'SetLargeSize(FBitmap, 0, 100) must raise Exception');
end;


procedure TTestGraphBitmap.TestSetLargeSize_InvalidHeight;
begin
  Assert.WillRaise(
    procedure
    begin
      SetLargeSize(FBitmap, 100, -1);
    end,
    Exception,
    'SetLargeSize(FBitmap, 100, -1) must raise Exception');
end;


{ ClearImage Tests }

procedure TTestGraphBitmap.TestClearImage_NilImage;
begin
  Assert.WillRaise(
    procedure
    begin
      ClearImage(NIL);
    end,
    Exception,
    'ClearImage(NIL) must raise Exception');
end;


{ ClearBitmap Tests }

procedure TTestGraphBitmap.TestClearBitmap_BasicCall;
begin
  Assert.IsFalse(FBitmap.Empty, 'Precondition: the 100x80 bitmap is not empty');
  ClearBitmap(FBitmap);
  Assert.IsTrue(FBitmap.Empty, 'ClearBitmap must leave an empty bitmap');
  Assert.AreEqual(0, FBitmap.Width,  'ClearBitmap must set Width to 0');
  Assert.AreEqual(0, FBitmap.Height, 'ClearBitmap must set Height to 0');
end;


procedure TTestGraphBitmap.TestClearBitmap_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      ClearBitmap(NIL);
    end,
    Exception,
    'ClearBitmap(NIL) must raise Exception');
end;


{ FillBitmap Tests }

procedure TTestGraphBitmap.TestFillBitmap_BasicCall;
begin
  Assert.AreEqual(0, CountColor24(FBitmap, clBlue), 'Precondition: Setup filled the bitmap white');

  FillBitmap(FBitmap, clBlue);

  { 100 * 80 = 8000 pixels }
  Assert.AreEqual(8000, CountColor24(FBitmap, clBlue), 'FillBitmap must paint every pixel');
  Assert.AreEqual(100, FBitmap.Width,  'FillBitmap must not resize');
  Assert.AreEqual(80,  FBitmap.Height, 'FillBitmap must not resize');
end;


procedure TTestGraphBitmap.TestFillBitmap_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      FillBitmap(NIL, clBlue);
    end,
    Exception,
    'FillBitmap(NIL, clBlue) must raise Exception');
end;


procedure TTestGraphBitmap.TestFillBitmap_FillsCorrectly;
var
  CenterPixel, CornerPixel: TColor;
begin
  FillBitmap(FBitmap, clGreen);

  CenterPixel:= FBitmap.Canvas.Pixels[50, 40];
  CornerPixel:= FBitmap.Canvas.Pixels[0, 0];

  Assert.AreEqual(TColor(clGreen), CenterPixel, 'Center should be green');
  Assert.AreEqual(TColor(clGreen), CornerPixel, 'Corner should be green');
end;


{ CenterText Tests }

procedure TTestGraphBitmap.TestCenterText_BasicCall;
VAR
  Reference: TBitmap;
  Ink: TRect;
begin
  { This overload draws with the font the canvas already has. Non-antialiased: every glyph pixel has exactly the font color }
  FBitmap.Canvas.Font.Quality:= fqNonAntialiased;
  FBitmap.Canvas.Font.Name   := 'Arial';
  FBitmap.Canvas.Font.Size   := 12;
  FBitmap.Canvas.Font.Color  := clNavy;

  CenterText(FBitmap, 'Test');

  Assert.IsTrue(CountColor24(FBitmap, clNavy) > 0, 'CenterText must draw the text');
  Assert.AreEqual(CountNonWhite24(FBitmap), CountColor24(FBitmap, clNavy), 'Every drawn pixel has the color of the canvas font');

  { The glyphs sit inside the text cell that CenterText centers. Side bearings, internal leading and descent move the ink
    a pixel or two off the center of the cell, hence the 3-pixel tolerance. Text drawn at (0, 0) misses it by about 35 pixels. }
  Ink:= InkBox24(FBitmap);
  Assert.IsTrue(Abs(Ink.Left - (99 - Ink.Right)) <= 3,
    'Horizontally centered: left margin ' + IntToStr(Ink.Left) + ', right margin ' + IntToStr(99 - Ink.Right));
  Assert.IsTrue(Abs(Ink.Top - (79 - Ink.Bottom)) <= 3,
    'Vertically centered: top margin ' + IntToStr(Ink.Top) + ', bottom margin ' + IntToStr(79 - Ink.Bottom));

  Reference:= CreateBlankBitmap(100, 80, clWhite);
  TRY
    DrawCenteredReference(Reference, 'Test', 'Arial', 12, clNavy);
    Assert.AreEqual(0, CountDifferentPixels24(FBitmap, Reference), 'CenterText must draw what TextOut draws at the centered position');
  FINALLY
    FreeAndNil(Reference);
  END;
end;


procedure TTestGraphBitmap.TestCenterText_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      CenterText(NIL, 'Test');
    end,
    Exception,
    'CenterText(NIL, Test) must raise Exception');
end;


procedure TTestGraphBitmap.TestCenterText_EmptyString;
begin
  SetOtherFont(FBitmap);

  CenterText(FBitmap, '');

  Assert.AreEqual(0, CountNonWhite24(FBitmap), 'An empty string must leave the white bitmap untouched');
  Assert.AreEqual(100, FBitmap.Width,  'An empty string must not resize the bitmap');
  Assert.AreEqual(80,  FBitmap.Height, 'An empty string must not resize the bitmap');
end;


procedure TTestGraphBitmap.TestCenterText_WithRFont;
var
  Font: RFont;
  Reference: TBitmap;
begin
  Font.Clear;   { Verdana, 10, clLime }

  { The canvas starts with Courier New 8 black, so a font field that CenterText does not apply shows in the pixels }
  SetOtherFont(FBitmap);
  CenterText(FBitmap, 'Test', Font);

  Assert.IsTrue(CountColor24(FBitmap, clLime) > 0, 'The text must be drawn in the RFont color, lime');
  Assert.AreEqual(CountNonWhite24(FBitmap), CountColor24(FBitmap, clLime), 'Every drawn pixel has the RFont color');

  Reference:= CreateBlankBitmap(100, 80, clWhite);
  TRY
    DrawCenteredReference(Reference, 'Test', 'Verdana', 10, clLime);
    Assert.AreEqual(0, CountDifferentPixels24(FBitmap, Reference), 'The text must be drawn in the RFont name and size (Verdana 10), centered');
  FINALLY
    FreeAndNil(Reference);
  END;
end;


procedure TTestGraphBitmap.TestCenterText_WithFontParams;
VAR Reference: TBitmap;
begin
  { The canvas starts with Courier New 8 black, so a parameter that CenterText does not apply shows in the pixels }
  SetOtherFont(FBitmap);
  CenterText(FBitmap, 'Test', 'Arial', 12, clRed);

  Assert.IsTrue(CountColor24(FBitmap, clRed) > 0, 'The text must be drawn in FontColor, red');
  Assert.AreEqual(CountNonWhite24(FBitmap), CountColor24(FBitmap, clRed), 'Every drawn pixel has FontColor');

  Reference:= CreateBlankBitmap(100, 80, clWhite);
  TRY
    DrawCenteredReference(Reference, 'Test', 'Arial', 12, clRed);
    Assert.AreEqual(0, CountDifferentPixels24(FBitmap, Reference), 'The text must be drawn in FontName and FontSize (Arial 12), centered');
  FINALLY
    FreeAndNil(Reference);
  END;
end;


{ GetBitmapRamSize Tests }

{ GetBitmapRamSize returns the size of the BMP stream that TBitmap.SaveToStream writes: a 14-byte BITMAPFILEHEADER, a 40-byte
  BITMAPINFOHEADER (no color table at 24 bits) and the rows, each padded to a multiple of 4 bytes
  (c:\Delphi\Delphi 13\source\vcl\Vcl.Graphics.pas, TBitmap.WriteStream and InternalGetDIBSizes) }
procedure TTestGraphBitmap.TestGetBitmapRamSize_BasicCall;
var
  Size: Int64;
begin
  Size:= GetBitmapRamSize(FBitmap);
  { 100x80 pf24bit: a row is 100 * 3 = 300 bytes, already a multiple of 4. 14 + 40 + 80 * 300 = 24054 }
  Assert.AreEqual(Int64(24054), Size, 'BMP stream size of a 100x80 pf24bit bitmap');
end;


procedure TTestGraphBitmap.TestGetBitmapRamSize_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      GetBitmapRamSize(NIL);
    end,
    Exception,
    'GetBitmapRamSize(NIL) must raise Exception');
end;


procedure TTestGraphBitmap.TestGetBitmapRamSize_ReturnsPositiveValue;
var
  Size: Int64;
  BMP: TBitmap;
begin
  BMP:= CreateBitmap(50, 50);
  TRY
    Size:= GetBitmapRamSize(BMP);
    { 50x50 pf24bit: a row is 50 * 3 = 150 bytes, padded to 152. 14 + 40 + 50 * 152 = 7654 }
    Assert.AreEqual(Int64(7654), Size, 'BMP stream size of a 50x50 pf24bit bitmap, with row padding');
  FINALLY
    FreeAndNil(BMP);
  END;
end;


{ PredictBitmapRamSize Tests }

procedure TTestGraphBitmap.TestPredictBitmapRamSize_BasicCall;
begin
  { This overload assumes pf24bit: 3 bytes per pixel }
  Assert.AreEqual(Cardinal(921600), PredictBitmapRamSize(640, 480), '640 * 480 * 3');
  Assert.AreEqual(Cardinal(3),      PredictBitmapRamSize(1, 1),     '1 * 1 * 3');
  { 30000 * 30000 * 3 = 2700000000 is over MaxInt (2147483647) but fits in Cardinal }
  Assert.AreEqual(Cardinal(2700000000), PredictBitmapRamSize(30000, 30000), '30000 * 30000 * 3 must not overflow');
end;


procedure TTestGraphBitmap.TestPredictBitmapRamSize_WithBitmap;
begin
  { Bytes per pixel = bits per pixel of BMP DIV 8 }
  Assert.AreEqual(Cardinal(120000), PredictBitmapRamSize(FBitmap, 200, 200), 'pf24bit: 200 * 200 * 3');

  FBitmap.PixelFormat:= pf32bit;
  Assert.AreEqual(Cardinal(160000), PredictBitmapRamSize(FBitmap, 200, 200), 'pf32bit: 200 * 200 * 4');

  FBitmap.PixelFormat:= pf8bit;
  Assert.AreEqual(Cardinal(40000),  PredictBitmapRamSize(FBitmap, 200, 200), 'pf8bit: 200 * 200 * 1');

  Assert.AreEqual(100, FBitmap.Width,  'Predicting must not resize the bitmap');
  Assert.AreEqual(80,  FBitmap.Height, 'Predicting must not resize the bitmap');
end;


procedure TTestGraphBitmap.TestPredictBitmapRamSize_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      PredictBitmapRamSize(NIL, 100, 100);
    end,
    Exception,
    'PredictBitmapRamSize(NIL, 100, 100) must raise Exception');
end;


procedure TTestGraphBitmap.TestPredictBitmapRamSize_CorrectCalculation;
var
  Size: Cardinal;
begin
  { For pf24bit: 100 * 100 * 3 bytes = 30000 }
  Size:= PredictBitmapRamSize(100, 100);
  Assert.AreEqual(Cardinal(30000), Size, 'Size should be 30000 bytes for 100x100 pf24');
end;


{ IsPanoramic Tests }

procedure TTestGraphBitmap.TestIsPanoramic_WithBitmap_BasicCall;
begin
  { The product rule: Width > 6000 AND Width/Height > 4 }
  Assert.IsFalse(IsPanoramic(FBitmap), '100x80 is not panoramic');
  FBitmap.SetSize(7000, 1000);
  Assert.IsTrue(IsPanoramic(FBitmap), '7000x1000 (ratio 7) is panoramic');
  FBitmap.SetSize(7000, 2000);
  Assert.IsFalse(IsPanoramic(FBitmap), '7000x2000 (ratio 3.5) is not panoramic');
end;


procedure TTestGraphBitmap.TestIsPanoramic_WithBitmap_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      IsPanoramic(TBitmap(NIL));
    end,
    Exception,
    'IsPanoramic(TBitmap(NIL)) must raise Exception');
end;


{ AspectIsSmaller Tests }

procedure TTestGraphBitmap.TestAspectIsSmaller_WithBitmap_BasicCall;
begin
  { FBitmap is 100x80: ratio 1.25 }
  Assert.IsTrue (AspectIsSmaller(FBitmap, 1920, 1080), '1.25 is smaller than 1920/1080 = 1.78');
  Assert.IsFalse(AspectIsSmaller(FBitmap, 100, 200),   '1.25 is not smaller than 100/200 = 0.5');
end;


procedure TTestGraphBitmap.TestAspectIsSmaller_WithBitmap_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      AspectIsSmaller(NIL, 1920, 1080);
    end,
    Exception,
    'AspectIsSmaller(NIL, 1920, 1080) must raise Exception');
end;


procedure TTestGraphBitmap.TestAspectIsSmaller_WithDimensions_Smaller;
begin
  { 100/200 = 0.5 < 1920/1080 = 1.78 - aspect is smaller (taller) }
  Assert.IsTrue(AspectIsSmaller(1920, 1080, 100, 200),
    'Tall aspect should be smaller than wide');
end;


procedure TTestGraphBitmap.TestAspectIsSmaller_WithDimensions_Larger;
begin
  { 1920/1080 = 1.78 > 100/200 = 0.5 - aspect is larger (wider) }
  Assert.IsFalse(AspectIsSmaller(100, 200, 1920, 1080),
    'Wide aspect should not be smaller than tall');
end;


procedure TTestGraphBitmap.TestAspectIsSmaller_ZeroHeight;
begin
  Assert.WillRaise(
    procedure
    begin
      AspectIsSmaller(1920, 0, 100, 100);
    end,
    Exception,
    'AspectIsSmaller(1920, 0, 100, 100) must raise Exception');
end;


{ AspectOrientation Tests }

procedure TTestGraphBitmap.TestAspectOrientation_WithBitmap_Landscape;
var
  BMP: TBitmap;
begin
  BMP:= TBitmap.Create;
  TRY
    BMP.Width:= 200;
    BMP.Height:= 100;

    Assert.AreEqual(orLandscape, AspectOrientation(BMP), 'Should be landscape');
  FINALLY
    FreeAndNil(BMP);
  END;
end;


procedure TTestGraphBitmap.TestAspectOrientation_WithBitmap_Portrait;
var
  BMP: TBitmap;
begin
  BMP:= TBitmap.Create;
  TRY
    BMP.Width:= 100;
    BMP.Height:= 200;

    Assert.AreEqual(orPortrait, AspectOrientation(BMP), 'Should be portrait');
  FINALLY
    FreeAndNil(BMP);
  END;
end;


procedure TTestGraphBitmap.TestAspectOrientation_WithBitmap_Square;
var
  BMP: TBitmap;
begin
  BMP:= TBitmap.Create;
  TRY
    BMP.Width:= 100;
    BMP.Height:= 100;

    Assert.AreEqual(orSquare, AspectOrientation(BMP), 'Should be square');
  FINALLY
    FreeAndNil(BMP);
  END;
end;


procedure TTestGraphBitmap.TestAspectOrientation_WithBitmap_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      AspectOrientation(TBitmap(NIL));
    end,
    Exception,
    'AspectOrientation(TBitmap(NIL)) must raise Exception');
end;


procedure TTestGraphBitmap.TestAspectOrientation_WithDimensions_Landscape;
begin
  Assert.AreEqual(orLandscape, AspectOrientation(200, 100), 'Should be landscape');
end;


procedure TTestGraphBitmap.TestAspectOrientation_WithDimensions_Portrait;
begin
  Assert.AreEqual(orPortrait, AspectOrientation(100, 200), 'Should be portrait');
end;


procedure TTestGraphBitmap.TestAspectOrientation_WithDimensions_Square;
begin
  Assert.AreEqual(orSquare, AspectOrientation(100, 100), 'Should be square');
end;


{ IsLandscape Tests }

procedure TTestGraphBitmap.TestIsLandscape_Landscape;
begin
  Assert.IsTrue(IsLandscape(200, 100), 'Wide image should be landscape');
end;


procedure TTestGraphBitmap.TestIsLandscape_Portrait;
begin
  Assert.IsFalse(IsLandscape(100, 200), 'Tall image should not be landscape');
end;


procedure TTestGraphBitmap.TestIsLandscape_Square;
begin
  Assert.IsTrue(IsLandscape(100, 100), 'Square image should count as landscape');
end;


{ GetImageScale Tests }

procedure TTestGraphBitmap.TestGetImageScale_WithBitmaps_NilInput;
var
  Desktop: TBitmap;
  Tile: RTileParams;
begin
  Desktop:= TBitmap.Create;
  TRY
    Desktop.Width:= 1920;
    Desktop.Height:= 1080;
    Tile.Reset;

    Assert.WillRaise(
      procedure
      begin
        GetImageScale(NIL, Desktop, Tile);
      end,
      Exception,
      'GetImageScale(NIL, Desktop, Tile) must raise Exception');
  FINALLY
    FreeAndNil(Desktop);
  END;
end;


procedure TTestGraphBitmap.TestGetImageScale_WithBitmaps_NilDesktop;
var
  Tile: RTileParams;
begin
  Tile.Reset;

  Assert.WillRaise(
    procedure
    begin
      GetImageScale(FBitmap, TBitmap(NIL), Tile);
    end,
    Exception,
    'GetImageScale(FBitmap, TBitmap(NIL), Tile) must raise Exception');
end;


procedure TTestGraphBitmap.TestGetImageScale_Large;
var
  InputBMP: TBitmap;
  Tile: RTileParams;
  Scale: TImageScale;
begin
  InputBMP:= TBitmap.Create;
  TRY
    InputBMP.Width:= 2000;
    InputBMP.Height:= 1200;
    Tile.Reset;

    Scale:= GetImageScale(InputBMP, 1920, 1080, Tile);

    Assert.AreEqual(isLarge, Scale, 'Image larger than desktop should be isLarge');
  FINALLY
    FreeAndNil(InputBMP);
  END;
end;


procedure TTestGraphBitmap.TestGetImageScale_Small;
var
  InputBMP: TBitmap;
  Tile: RTileParams;
  Scale: TImageScale;
begin
  InputBMP:= TBitmap.Create;
  TRY
    InputBMP.Width:= 1500;
    InputBMP.Height:= 900;
    Tile.Reset;

    Scale:= GetImageScale(InputBMP, 1920, 1080, Tile);

    Assert.AreEqual(isSmall, Scale, 'Image between threshold and 100% should be isSmall');
  FINALLY
    FreeAndNil(InputBMP);
  END;
end;


procedure TTestGraphBitmap.TestGetImageScale_Tiny;
var
  InputBMP: TBitmap;
  Tile: RTileParams;
  Scale: TImageScale;
begin
  InputBMP:= TBitmap.Create;
  TRY
    InputBMP.Width:= 100;
    InputBMP.Height:= 100;
    Tile.Reset;
    Tile.TileThreshold:= 40; { 40% threshold }

    Scale:= GetImageScale(InputBMP, 1920, 1080, Tile);

    Assert.AreEqual(isTiny, Scale, 'Very small image should be isTiny');
  FINALLY
    FreeAndNil(InputBMP);
  END;
end;


procedure TTestGraphBitmap.TestGetImageScale_TinyButTall;
var
  InputBMP: TBitmap;
  Tile: RTileParams;
  Scale: TImageScale;
begin
  { Test the special case: image with small area but taller than desktop }
  InputBMP:= TBitmap.Create;
  TRY
    InputBMP.Width:= 300;
    InputBMP.Height:= 1200; { Taller than 1080 }
    Tile.Reset;
    Tile.TileThreshold:= 40;

    Scale:= GetImageScale(InputBMP, 1920, 1080, Tile);

    { Should be isSmall, not isTiny, because height >= desktop height }
    Assert.AreEqual(isSmall, Scale, 'Tall but slim image should be isSmall, not isTiny');
  FINALLY
    FreeAndNil(InputBMP);
  END;
end;


procedure TTestGraphBitmap.TestGetImageScale_InvalidThreshold;
var
  Tile: RTileParams;
begin
  Tile.Reset;
  Tile.TileThreshold:= 100; { Invalid - must be < 100 }

  Assert.WillRaise(
    procedure
    begin
      GetImageScale(FBitmap, 1920, 1080, Tile);
    end,
    Exception,
    'GetImageScale(FBitmap, 1920, 1080, Tile) must raise Exception');
end;


{ EnlargeCanvas Tests }

procedure TTestGraphBitmap.TestEnlargeCanvas_BasicCall;
begin
  { Mark two corners of the white 100x80 source }
  FBitmap.Canvas.Pixels[0, 0]  := clRed;
  FBitmap.Canvas.Pixels[99, 79]:= clLime;

  EnlargeCanvas(FBitmap, 200, 200, clYellow);

  Assert.AreEqual(200, FBitmap.Width,  'Width should be enlarged to 200');
  Assert.AreEqual(200, FBitmap.Height, 'Height should be enlarged to 200');

  { The source lands at ((200 - 100) div 2, (200 - 80) div 2) = (50, 60), so it covers X 50..149 and Y 60..139 }
  Assert.AreEqual(Integer(clRed),    Integer(FBitmap.Canvas.Pixels[50, 60]),   'Source (0, 0) moves to (50, 60)');
  Assert.AreEqual(Integer(clLime),   Integer(FBitmap.Canvas.Pixels[149, 139]), 'Source (99, 79) moves to (149, 139)');
  Assert.AreEqual(Integer(clWhite),  Integer(FBitmap.Canvas.Pixels[100, 100]), 'Source (50, 40) moves to (100, 100)');
  Assert.AreEqual(Integer(clYellow), Integer(FBitmap.Canvas.Pixels[49, 60]),   'Left of the source: CanvasFillColor');
  Assert.AreEqual(Integer(clYellow), Integer(FBitmap.Canvas.Pixels[50, 59]),   'Above the source: CanvasFillColor');
  Assert.AreEqual(Integer(clYellow), Integer(FBitmap.Canvas.Pixels[150, 139]), 'Right of the source: CanvasFillColor');
  Assert.AreEqual(Integer(clYellow), Integer(FBitmap.Canvas.Pixels[149, 140]), 'Below the source: CanvasFillColor');
  Assert.AreEqual(Integer(clYellow), Integer(FBitmap.Canvas.Pixels[0, 0]),     'Corner of the new canvas: CanvasFillColor');
  Assert.AreEqual(Integer(clYellow), Integer(FBitmap.Canvas.Pixels[199, 199]), 'Far corner of the new canvas: CanvasFillColor');
end;


procedure TTestGraphBitmap.TestEnlargeCanvas_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      EnlargeCanvas(NIL, 200, 200, clBlack);
    end,
    Exception,
    'EnlargeCanvas(NIL, 200, 200, clBlack) must raise Exception');
end;


procedure TTestGraphBitmap.TestEnlargeCanvas_CorrectDimensions;
begin
  { Mark two corners of the white 100x80 source }
  FBitmap.Canvas.Pixels[0, 0]  := clRed;
  FBitmap.Canvas.Pixels[99, 79]:= clLime;

  EnlargeCanvas(FBitmap, 300, 250, clBlue);

  Assert.AreEqual(300, FBitmap.Width, 'Width should be enlarged to 300');
  Assert.AreEqual(250, FBitmap.Height, 'Height should be enlarged to 250');

  { The source lands at ((300 - 100) div 2, (250 - 80) div 2) = (100, 85), so it covers X 100..199 and Y 85..164 }
  Assert.AreEqual(Integer(clRed),   Integer(FBitmap.Canvas.Pixels[100, 85]),  'Source (0, 0) moves to (100, 85)');
  Assert.AreEqual(Integer(clLime),  Integer(FBitmap.Canvas.Pixels[199, 164]), 'Source (99, 79) moves to (199, 164)');
  Assert.AreEqual(Integer(clWhite), Integer(FBitmap.Canvas.Pixels[150, 125]), 'Source (50, 40) moves to (150, 125)');
  Assert.AreEqual(Integer(clBlue),  Integer(FBitmap.Canvas.Pixels[99, 85]),   'Left of the source: CanvasFillColor');
  Assert.AreEqual(Integer(clBlue),  Integer(FBitmap.Canvas.Pixels[100, 84]),  'Above the source: CanvasFillColor');
  Assert.AreEqual(Integer(clBlue),  Integer(FBitmap.Canvas.Pixels[200, 164]), 'Right of the source: CanvasFillColor');
  Assert.AreEqual(Integer(clBlue),  Integer(FBitmap.Canvas.Pixels[199, 165]), 'Below the source: CanvasFillColor');
  Assert.AreEqual(Integer(clBlue),  Integer(FBitmap.Canvas.Pixels[0, 0]),     'Corner of the new canvas: CanvasFillColor');
  Assert.AreEqual(Integer(clBlue),  Integer(FBitmap.Canvas.Pixels[299, 249]), 'Far corner of the new canvas: CanvasFillColor');
end;


{ CenterBitmap Tests }

procedure TTestGraphBitmap.TestCenterBitmap_BasicCall;
var
  Dest: TBitmap;
begin
  { Mark two corners of the white 100x80 source }
  FBitmap.Canvas.Pixels[0, 0]  := clRed;
  FBitmap.Canvas.Pixels[99, 79]:= clLime;

  Dest:= TBitmap.Create;
  TRY
    Dest.PixelFormat:= pf24bit;
    Dest.Width:= 200;
    Dest.Height:= 160;
    Dest.Canvas.Brush.Color:= clBlue;
    Dest.Canvas.FillRect(Rect(0, 0, 200, 160));

    CenterBitmap(FBitmap, Dest);

    Assert.AreEqual(200, Dest.Width,  'CenterBitmap must not resize Dest');
    Assert.AreEqual(160, Dest.Height, 'CenterBitmap must not resize Dest');
    { The source lands at ((200 - 100) div 2, (160 - 80) div 2) = (50, 40), so it covers X 50..149 and Y 40..119 }
    Assert.AreEqual(Integer(clRed),   Integer(Dest.Canvas.Pixels[50, 40]),   'Source (0, 0) moves to (50, 40)');
    Assert.AreEqual(Integer(clLime),  Integer(Dest.Canvas.Pixels[149, 119]), 'Source (99, 79) moves to (149, 119)');
    Assert.AreEqual(Integer(clWhite), Integer(Dest.Canvas.Pixels[100, 80]),  'Source (50, 40) moves to (100, 80)');
    Assert.AreEqual(Integer(clBlue),  Integer(Dest.Canvas.Pixels[49, 40]),   'Left of the source Dest keeps its color');
    Assert.AreEqual(Integer(clBlue),  Integer(Dest.Canvas.Pixels[50, 39]),   'Above the source Dest keeps its color');
    Assert.AreEqual(Integer(clBlue),  Integer(Dest.Canvas.Pixels[150, 119]), 'Right of the source Dest keeps its color');
    Assert.AreEqual(Integer(clBlue),  Integer(Dest.Canvas.Pixels[149, 120]), 'Below the source Dest keeps its color');
    Assert.AreEqual(Integer(clRed),   Integer(FBitmap.Canvas.Pixels[0, 0]),  'CenterBitmap must not change Source');
  FINALLY
    FreeAndNil(Dest);
  END;
end;


procedure TTestGraphBitmap.TestCenterBitmap_NilSource;
var
  Dest: TBitmap;
begin
  Dest:= TBitmap.Create;
  TRY
    Dest.Width:= 200;
    Dest.Height:= 160;

    Assert.WillRaise(
      procedure
      begin
        CenterBitmap(NIL, Dest);
      end,
      Exception,
      'CenterBitmap(NIL, Dest) must raise Exception');
  FINALLY
    FreeAndNil(Dest);
  END;
end;


procedure TTestGraphBitmap.TestCenterBitmap_NilDest;
begin
  Assert.WillRaise(
    procedure
    begin
      CenterBitmap(FBitmap, NIL);
    end,
    Exception,
    'CenterBitmap(FBitmap, NIL) must raise Exception');
end;


procedure TTestGraphBitmap.TestCenterBitmap_SameSize;
var
  Source, Dest: TBitmap;
begin
  Source:= TBitmap.Create;
  Dest:= TBitmap.Create;
  TRY
    Source.Width:= 100;
    Source.Height:= 100;
    Source.Canvas.Brush.Color:= clRed;
    Source.Canvas.FillRect(Rect(0, 0, 100, 100));

    Dest.Width:= 100;
    Dest.Height:= 100;

    CenterBitmap(Source, Dest);

    { Dest should have Source content }
    Assert.AreEqual(TColor(clRed), Dest.Canvas.Pixels[50, 50], 'Dest should have source content');
  FINALLY
    FreeAndNil(Source);
    FreeAndNil(Dest);
  END;
end;


procedure TTestGraphBitmap.TestCenterBitmap_SourceSmaller;
var
  Source, Dest: TBitmap;
begin
  Source:= TBitmap.Create;
  Dest:= TBitmap.Create;
  TRY
    Source.Width:= 50;
    Source.Height:= 50;
    Source.Canvas.Brush.Color:= clGreen;
    Source.Canvas.FillRect(Rect(0, 0, 50, 50));
    { Mark two corners of the source }
    Source.Canvas.Pixels[0, 0]  := clRed;
    Source.Canvas.Pixels[49, 49]:= clYellow;

    Dest.Width:= 100;
    Dest.Height:= 100;
    Dest.Canvas.Brush.Color:= clBlue;
    Dest.Canvas.FillRect(Rect(0, 0, 100, 100));

    CenterBitmap(Source, Dest);

    { Center of Dest should be green }
    Assert.AreEqual(TColor(clGreen), Dest.Canvas.Pixels[50, 50], 'Center should be green');
    { The source lands at ((100 - 50) div 2, (100 - 50) div 2) = (25, 25), so it covers X and Y 25..74.
      Aligned bottom-right it would cover 50..99 and still hold (50, 50). }
    Assert.AreEqual(Integer(clRed),    Integer(Dest.Canvas.Pixels[25, 25]), 'Source (0, 0) moves to (25, 25)');
    Assert.AreEqual(Integer(clYellow), Integer(Dest.Canvas.Pixels[74, 74]), 'Source (49, 49) moves to (74, 74)');
    Assert.AreEqual(Integer(clBlue),   Integer(Dest.Canvas.Pixels[24, 25]), 'Left of the source Dest keeps its color');
    Assert.AreEqual(Integer(clBlue),   Integer(Dest.Canvas.Pixels[25, 24]), 'Above the source Dest keeps its color');
    Assert.AreEqual(Integer(clBlue),   Integer(Dest.Canvas.Pixels[75, 74]), 'Right of the source Dest keeps its color');
    Assert.AreEqual(Integer(clBlue),   Integer(Dest.Canvas.Pixels[74, 75]), 'Below the source Dest keeps its color');
  FINALLY
    FreeAndNil(Source);
    FreeAndNil(Dest);
  END;
end;


procedure TTestGraphBitmap.TestCenterBitmap_SourceLarger;
var
  Source, Dest: TBitmap;
begin
  Source:= TBitmap.Create;
  Dest:= TBitmap.Create;
  TRY
    Source.Width:= 200;
    Source.Height:= 200;
    Source.Canvas.Brush.Color:= clYellow;
    Source.Canvas.FillRect(Rect(0, 0, 200, 200));
    { The center crop of a 200x200 source into 100x100 starts at ((200 - 100) div 2, (200 - 100) div 2) = (50, 50)
      and covers source X and Y 50..149. Mark its two corners, and the source corner a crop from the top-left would take. }
    Source.Canvas.Pixels[50, 50]  := clRed;
    Source.Canvas.Pixels[149, 149]:= clLime;
    Source.Canvas.Pixels[0, 0]    := clFuchsia;

    Dest.Width:= 100;
    Dest.Height:= 100;

    CenterBitmap(Source, Dest);

    { Dest should have center portion of source }
    Assert.AreEqual(TColor(clYellow), Dest.Canvas.Pixels[50, 50], 'Should have source center');
    Assert.AreEqual(100, Dest.Width,  'CenterBitmap must not resize Dest');
    Assert.AreEqual(100, Dest.Height, 'CenterBitmap must not resize Dest');
    Assert.AreEqual(Integer(clRed),    Integer(Dest.Canvas.Pixels[0, 0]),   'Source (50, 50) moves to (0, 0)');
    Assert.AreEqual(Integer(clLime),   Integer(Dest.Canvas.Pixels[99, 99]), 'Source (149, 149) moves to (99, 99)');
    Assert.AreEqual(Integer(clYellow), Integer(Dest.Canvas.Pixels[1, 0]),   'Source (51, 50) moves to (1, 0)');
  FINALLY
    FreeAndNil(Source);
    FreeAndNil(Dest);
  END;
end;


{ RFont Tests }

procedure TTestGraphBitmap.TestRFont_Clear_DefaultValues;
var
  Font: RFont;
begin
  Font.Clear;

  Assert.AreEqual('Verdana', Font.Name, 'Default name should be Verdana');
  Assert.AreEqual(10, Font.Size, 'Default size should be 10');
  Assert.AreEqual(TColor(clLime), Font.Color, 'Default color should be clLime');
end;


procedure TTestGraphBitmap.TestRFont_Clear_WithParams;
var
  Font: RFont;
begin
  Font.Clear('Arial', 14, clRed);

  Assert.AreEqual('Arial', Font.Name, 'Name should be Arial');
  Assert.AreEqual(14, Font.Size, 'Size should be 14');
  Assert.AreEqual(TColor(clRed), Font.Color, 'Color should be clRed');
end;


procedure TTestGraphBitmap.TestRFont_AssignTo_BasicCall;
var
  RFont_: RFont;
  Font: TFont;
begin
  Font:= TFont.Create;
  TRY
    { Start from other values, so a field that AssignTo does not copy shows }
    Font.Name := 'Arial';
    Font.Size := 20;
    Font.Color:= clRed;
    RFont_.Clear;   { Verdana, 10, clLime }

    RFont_.AssignTo(Font);

    Assert.AreEqual('Verdana', Font.Name, 'AssignTo must copy the name');
    Assert.AreEqual(10, Font.Size, 'AssignTo must copy the size');
    Assert.AreEqual(Integer(clLime), Integer(Font.Color), 'AssignTo must copy the color');
  FINALLY
    FreeAndNil(Font);
  END;
end;


procedure TTestGraphBitmap.TestRFont_AssignTo_NilFont;
var
  Font: RFont;
begin
  Font.Clear;

  Assert.WillRaise(
    procedure
    begin
      Font.AssignTo(NIL);
    end,
    Exception,
    'Font.AssignTo(NIL) must raise Exception');
end;


procedure TTestGraphBitmap.TestRFont_AssignTo_CopiesValues;
var
  RFont_: RFont;
  Font: TFont;
begin
  Font:= TFont.Create;
  TRY
    RFont_.Clear('Courier New', 16, clBlue);
    RFont_.AssignTo(Font);

    Assert.AreEqual('Courier New', Font.Name, 'Font name should match');
    Assert.AreEqual(16, Font.Size, 'Font size should match');
    Assert.AreEqual(TColor(clBlue), Font.Color, 'Font color should match');
  FINALLY
    FreeAndNil(Font);
  END;
end;


{ Thread safety
  The main thread's Vcl.Graphics.FreeMemoryContexts frees the DC of every TBitmapCanvas in its list. A worker that
  frees a bitmap whose canvas still owns a DC races with it (FastMM, 2026-10-05: TBitmapCanvas modified after free).
  FillBitmap and CenterBitmap run on the BioniX thumbnail worker (GetVideoPlayerLogo), so they must leave no canvas DC. }

procedure TTestGraphBitmap.TestFillBitmap_LeavesNoCanvasDC;
VAR BMP: TBitmap;
begin
  BMP:= CreateBitmap(20, 20);
  TRY
    FillBitmap(BMP, clRed);
    Assert.IsTrue(PixelIsRed24(BMP, 10, 10), 'FillBitmap must fill');
    Assert.IsFalse(BMP.Canvas.HandleAllocated, 'FillBitmap must not leave a DC on the canvas');
  FINALLY
    FreeAndNil(BMP);
  END;
end;


procedure TTestGraphBitmap.TestCenterBitmap_LeavesNoCanvasDC;
VAR Source, Dest: TBitmap;
begin
  Source:= CreateBitmap(10, 10);
  Dest  := CreateBitmap(30, 30);
  TRY
    FillScanLines24(Source, 255, 0, 0);
    FillScanLines24(Dest,   0, 0, 255);

    CenterBitmap(Source, Dest);

    Assert.IsTrue (PixelIsRed24(Dest, 15, 15), 'The center of Dest must hold Source');
    Assert.IsTrue (PixelIsRed24(Dest, 10, 10), 'Source starts at (30-10)/2 = 10');
    Assert.IsFalse(PixelIsRed24(Dest,  9,  9), 'Outside the centered area Dest keeps its own color');
    Assert.IsFalse(Source.Canvas.HandleAllocated, 'CenterBitmap must not leave a DC on the Source canvas');
    Assert.IsFalse(Dest.Canvas.HandleAllocated,   'CenterBitmap must not leave a DC on the Dest canvas');
  FINALLY
    FreeAndNil(Source);
    FreeAndNil(Dest);
  END;
end;


{ CenterText runs on the BioniX thumbnail worker (placeholder thumbnails), so each overload must draw
  and leave no DC on the canvas. }
procedure TTestGraphBitmap.TestCenterText_LeavesNoCanvasDC;
VAR
  BMP: TBitmap;
  Font: RFont;
begin
  BMP:= CreateBlankBitmap(80, 40, clWhite);
  TRY
    BMP.Canvas.Font.Color:= clBlack;
    CenterText(BMP, 'Test');
    Assert.IsFalse(BMP.Canvas.HandleAllocated, 'CenterText(BMP, Text) must not leave a DC on the canvas');
    Assert.IsTrue(CountNonWhite24(BMP) > 0, 'CenterText(BMP, Text) must draw the text');
  FINALLY
    FreeAndNil(BMP);
  END;

  BMP:= CreateBlankBitmap(80, 40, clWhite);
  TRY
    Font.Clear('Arial', 12, clBlack);
    CenterText(BMP, 'Test', Font);
    Assert.IsFalse(BMP.Canvas.HandleAllocated, 'CenterText(BMP, Text, RFont) must not leave a DC on the canvas');
    Assert.IsTrue(CountNonWhite24(BMP) > 0, 'CenterText(BMP, Text, RFont) must draw the text');
  FINALLY
    FreeAndNil(BMP);
  END;

  BMP:= CreateBlankBitmap(80, 40, clWhite);
  TRY
    CenterText(BMP, 'Test', 'Arial', 12, clBlack);
    Assert.IsFalse(BMP.Canvas.HandleAllocated, 'CenterText(BMP, Text, FontName, ...) must not leave a DC on the canvas');
    Assert.IsTrue(CountNonWhite24(BMP) > 0, 'CenterText(BMP, Text, FontName, ...) must draw the text');
  FINALLY
    FreeAndNil(BMP);
  END;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestGraphBitmap);

end.
