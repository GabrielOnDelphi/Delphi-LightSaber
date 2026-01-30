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

    [Test]
    procedure TestIsPanoramic_WithDimensions_NotPanoramic;

    [Test]
    procedure TestIsPanoramic_WithDimensions_IsPanoramic;

    [Test]
    procedure TestIsPanoramic_ZeroHeight;

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
    Assert.IsNotNull(BMP, 'CreateBitmap should return a bitmap');
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
    Assert.IsNotNull(BMP, 'CreateBlankBitmap should return a bitmap');
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
  Assert.WillNotRaise(
    procedure
    begin
      SetLargeSize(FBitmap, 200, 200);
    end);

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
    Exception);
end;


procedure TTestGraphBitmap.TestSetLargeSize_InvalidWidth;
begin
  Assert.WillRaise(
    procedure
    begin
      SetLargeSize(FBitmap, 0, 100);
    end,
    Exception);
end;


procedure TTestGraphBitmap.TestSetLargeSize_InvalidHeight;
begin
  Assert.WillRaise(
    procedure
    begin
      SetLargeSize(FBitmap, 100, -1);
    end,
    Exception);
end;


{ ClearImage Tests }

procedure TTestGraphBitmap.TestClearImage_NilImage;
begin
  Assert.WillRaise(
    procedure
    begin
      ClearImage(NIL);
    end,
    Exception);
end;


{ ClearBitmap Tests }

procedure TTestGraphBitmap.TestClearBitmap_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      ClearBitmap(FBitmap);
    end);
end;


procedure TTestGraphBitmap.TestClearBitmap_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      ClearBitmap(NIL);
    end,
    Exception);
end;


{ FillBitmap Tests }

procedure TTestGraphBitmap.TestFillBitmap_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      FillBitmap(FBitmap, clBlue);
    end);
end;


procedure TTestGraphBitmap.TestFillBitmap_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      FillBitmap(NIL, clBlue);
    end,
    Exception);
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
begin
  Assert.WillNotRaise(
    procedure
    begin
      CenterText(FBitmap, 'Test');
    end);
end;


procedure TTestGraphBitmap.TestCenterText_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      CenterText(NIL, 'Test');
    end,
    Exception);
end;


procedure TTestGraphBitmap.TestCenterText_EmptyString;
begin
  { Should not crash with empty string }
  Assert.WillNotRaise(
    procedure
    begin
      CenterText(FBitmap, '');
    end);
end;


procedure TTestGraphBitmap.TestCenterText_WithRFont;
var
  Font: RFont;
begin
  Font.Clear;

  Assert.WillNotRaise(
    procedure
    begin
      CenterText(FBitmap, 'Test', Font);
    end);
end;


procedure TTestGraphBitmap.TestCenterText_WithFontParams;
begin
  Assert.WillNotRaise(
    procedure
    begin
      CenterText(FBitmap, 'Test', 'Arial', 12, clBlack);
    end);
end;


{ GetBitmapRamSize Tests }

procedure TTestGraphBitmap.TestGetBitmapRamSize_BasicCall;
var
  Size: Int64;
begin
  Size:= GetBitmapRamSize(FBitmap);
  Assert.IsTrue(Size > 0, 'Size should be greater than 0');
end;


procedure TTestGraphBitmap.TestGetBitmapRamSize_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      GetBitmapRamSize(NIL);
    end,
    Exception);
end;


procedure TTestGraphBitmap.TestGetBitmapRamSize_ReturnsPositiveValue;
var
  Size: Int64;
  BMP: TBitmap;
begin
  BMP:= CreateBitmap(50, 50);
  TRY
    Size:= GetBitmapRamSize(BMP);
    Assert.IsTrue(Size > 0, 'RAM size should be positive');
  FINALLY
    FreeAndNil(BMP);
  END;
end;


{ PredictBitmapRamSize Tests }

procedure TTestGraphBitmap.TestPredictBitmapRamSize_BasicCall;
var
  Size: Cardinal;
begin
  Size:= PredictBitmapRamSize(100, 100);
  Assert.IsTrue(Size > 0, 'Predicted size should be positive');
end;


procedure TTestGraphBitmap.TestPredictBitmapRamSize_WithBitmap;
var
  Size: Cardinal;
begin
  Size:= PredictBitmapRamSize(FBitmap, 200, 200);
  Assert.IsTrue(Size > 0, 'Predicted size should be positive');
end;


procedure TTestGraphBitmap.TestPredictBitmapRamSize_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      PredictBitmapRamSize(NIL, 100, 100);
    end,
    Exception);
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
  Assert.WillNotRaise(
    procedure
    begin
      IsPanoramic(FBitmap);
    end);
end;


procedure TTestGraphBitmap.TestIsPanoramic_WithBitmap_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      IsPanoramic(TBitmap(NIL));
    end,
    Exception);
end;


procedure TTestGraphBitmap.TestIsPanoramic_WithDimensions_NotPanoramic;
begin
  { 1920x1080 is wide but not panoramic (ratio ~1.78, needs >4) }
  Assert.IsFalse(IsPanoramic(1920, 1080), 'HD resolution should not be panoramic');
end;


procedure TTestGraphBitmap.TestIsPanoramic_WithDimensions_IsPanoramic;
begin
  { 8000x1000 has ratio 8:1 and width > 6000 }
  Assert.IsTrue(IsPanoramic(8000, 1000), '8000x1000 should be panoramic');
end;


procedure TTestGraphBitmap.TestIsPanoramic_ZeroHeight;
begin
  { Should return false, not crash with division by zero }
  Assert.IsFalse(IsPanoramic(1000, 0), 'Zero height should return false, not crash');
end;


{ AspectIsSmaller Tests }

procedure TTestGraphBitmap.TestAspectIsSmaller_WithBitmap_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      AspectIsSmaller(FBitmap, 1920, 1080);
    end);
end;


procedure TTestGraphBitmap.TestAspectIsSmaller_WithBitmap_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      AspectIsSmaller(NIL, 1920, 1080);
    end,
    Exception);
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
    Exception);
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
    Exception);
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
      Exception);
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
    Exception);
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
    Exception);
end;


{ EnlargeCanvas Tests }

procedure TTestGraphBitmap.TestEnlargeCanvas_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      EnlargeCanvas(FBitmap, 200, 200, clBlack);
    end);
end;


procedure TTestGraphBitmap.TestEnlargeCanvas_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      EnlargeCanvas(NIL, 200, 200, clBlack);
    end,
    Exception);
end;


procedure TTestGraphBitmap.TestEnlargeCanvas_CorrectDimensions;
begin
  EnlargeCanvas(FBitmap, 300, 250, clBlue);

  Assert.AreEqual(300, FBitmap.Width, 'Width should be enlarged to 300');
  Assert.AreEqual(250, FBitmap.Height, 'Height should be enlarged to 250');
end;


{ CenterBitmap Tests }

procedure TTestGraphBitmap.TestCenterBitmap_BasicCall;
var
  Dest: TBitmap;
begin
  Dest:= TBitmap.Create;
  TRY
    Dest.Width:= 200;
    Dest.Height:= 160;

    Assert.WillNotRaise(
      procedure
      begin
        CenterBitmap(FBitmap, Dest);
      end);
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
      Exception);
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
    Exception);
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

    Dest.Width:= 100;
    Dest.Height:= 100;
    Dest.Canvas.Brush.Color:= clBlue;
    Dest.Canvas.FillRect(Rect(0, 0, 100, 100));

    CenterBitmap(Source, Dest);

    { Center of Dest should be green }
    Assert.AreEqual(TColor(clGreen), Dest.Canvas.Pixels[50, 50], 'Center should be green');
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

    Dest.Width:= 100;
    Dest.Height:= 100;

    CenterBitmap(Source, Dest);

    { Dest should have center portion of source }
    Assert.AreEqual(TColor(clYellow), Dest.Canvas.Pixels[50, 50], 'Should have source center');
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
    RFont_.Clear;

    Assert.WillNotRaise(
      procedure
      begin
        RFont_.AssignTo(Font);
      end);
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
    Exception);
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


initialization
  TDUnitX.RegisterTestFixture(TTestGraphBitmap);

end.
