unit Test.LightVcl.Graph.Alpha;

{=============================================================================================================
   Unit tests for LightVcl.Graph.Alpha.pas
   Tests alpha blending, transparency blend, and ImageList bitmap extraction.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ImgList;

type
  [TestFixture]
  TTestGraphAlpha = class
  private
    FMainBitmap: TBitmap;
    FSmallBitmap: TBitmap;
    procedure FillBitmapWithColor(BMP: TBitmap; Color: TColor);
    procedure CreateTestBitmaps(MainW, MainH, SmallW, SmallH: Integer);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { AlphaBlendBitmaps (Canvas overload) Tests }
    [Test]
    procedure TestAlphaBlendBitmaps_Canvas_BasicCall;

    [Test]
    procedure TestAlphaBlendBitmaps_Canvas_NilSourceRaises;

    [Test]
    procedure TestAlphaBlendBitmaps_Canvas_NilDestRaises;

    { AlphaBlendBitmaps (Bitmap overload) Tests }
    [Test]
    procedure TestAlphaBlendBitmaps_Bitmap_BasicCall;

    [Test]
    procedure TestAlphaBlendBitmaps_Bitmap_NilMainRaises;

    [Test]
    procedure TestAlphaBlendBitmaps_Bitmap_NilSmallRaises;

    [Test]
    procedure TestAlphaBlendBitmaps_Bitmap_WrongPixelFormatRaises;

    [Test]
    procedure TestAlphaBlendBitmaps_Bitmap_NegativeXRaises;

    [Test]
    procedure TestAlphaBlendBitmaps_Bitmap_NegativeYRaises;

    [Test]
    procedure TestAlphaBlendBitmaps_Bitmap_ZeroTransparency;

    [Test]
    procedure TestAlphaBlendBitmaps_Bitmap_FullTransparency;

    [Test]
    procedure TestAlphaBlendBitmaps_Bitmap_HalfTransparency;

    [Test]
    procedure TestAlphaBlendBitmaps_Bitmap_WithOffset;

    [Test]
    procedure TestAlphaBlendBitmaps_Bitmap_SmallBitmapLargerClips;

    { TransparencyBlend Tests }
    [Test]
    procedure TestTransparencyBlend_BasicCall;

    [Test]
    procedure TestTransparencyBlend_NilMainRaises;

    [Test]
    procedure TestTransparencyBlend_NilSmallRaises;

    [Test]
    procedure TestTransparencyBlend_MainWrongPixelFormatRaises;

    [Test]
    procedure TestTransparencyBlend_SmallWrongPixelFormatRaises;

    [Test]
    procedure TestTransparencyBlend_NegativeXRaises;

    [Test]
    procedure TestTransparencyBlend_NegativeYRaises;

    [Test]
    procedure TestTransparencyBlend_ReturnsNewBitmap;

    [Test]
    procedure TestTransparencyBlend_CorrectDimensions;

    [Test]
    procedure TestTransparencyBlend_TransparentPixelsNotCopied;

    [Test]
    procedure TestTransparencyBlend_NonTransparentPixelsCopied;

    [Test]
    procedure TestTransparencyBlend_WithOffset;

    { GetTransparentBitmapFromImagelist Tests }
    [Test]
    procedure TestGetTransparentBitmapFromImagelist_BasicCall;

    [Test]
    procedure TestGetTransparentBitmapFromImagelist_NilImageListRaises;

    [Test]
    procedure TestGetTransparentBitmapFromImagelist_NilBitmapRaises;

    [Test]
    procedure TestGetTransparentBitmapFromImagelist_NegativeIndexRaises;

    [Test]
    procedure TestGetTransparentBitmapFromImagelist_IndexOutOfRangeRaises;

    [Test]
    procedure TestGetTransparentBitmapFromImagelist_CorrectSize;
  end;

implementation

uses
  LightVcl.Graph.Alpha;


procedure TTestGraphAlpha.Setup;
begin
  FMainBitmap:= TBitmap.Create;
  FSmallBitmap:= TBitmap.Create;
  CreateTestBitmaps(100, 80, 50, 40);
end;


procedure TTestGraphAlpha.TearDown;
begin
  FreeAndNil(FMainBitmap);
  FreeAndNil(FSmallBitmap);
end;


procedure TTestGraphAlpha.CreateTestBitmaps(MainW, MainH, SmallW, SmallH: Integer);
begin
  FMainBitmap.Width:= MainW;
  FMainBitmap.Height:= MainH;
  FMainBitmap.PixelFormat:= pf24bit;
  FillBitmapWithColor(FMainBitmap, clWhite);

  FSmallBitmap.Width:= SmallW;
  FSmallBitmap.Height:= SmallH;
  FSmallBitmap.PixelFormat:= pf24bit;
  FillBitmapWithColor(FSmallBitmap, clRed);
end;


procedure TTestGraphAlpha.FillBitmapWithColor(BMP: TBitmap; Color: TColor);
begin
  BMP.Canvas.Brush.Color:= Color;
  BMP.Canvas.FillRect(Rect(0, 0, BMP.Width, BMP.Height));
end;


{ AlphaBlendBitmaps (Canvas overload) Tests }

procedure TTestGraphAlpha.TestAlphaBlendBitmaps_Canvas_BasicCall;
var
  DestBitmap: TBitmap;
begin
  DestBitmap:= TBitmap.Create;
  TRY
    DestBitmap.Width:= 100;
    DestBitmap.Height:= 100;

    Assert.WillNotRaise(
      procedure
      begin
        AlphaBlendBitmaps(FMainBitmap, DestBitmap.Canvas, Rect(0, 0, 100, 80), 128);
      end);
  FINALLY
    FreeAndNil(DestBitmap);
  END;
end;


procedure TTestGraphAlpha.TestAlphaBlendBitmaps_Canvas_NilSourceRaises;
var
  DestBitmap: TBitmap;
begin
  DestBitmap:= TBitmap.Create;
  TRY
    DestBitmap.Width:= 100;
    DestBitmap.Height:= 100;

    Assert.WillRaise(
      procedure
      begin
        AlphaBlendBitmaps(NIL, DestBitmap.Canvas, Rect(0, 0, 100, 80), 128);
      end, Exception);
  FINALLY
    FreeAndNil(DestBitmap);
  END;
end;


procedure TTestGraphAlpha.TestAlphaBlendBitmaps_Canvas_NilDestRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      AlphaBlendBitmaps(FMainBitmap, NIL, Rect(0, 0, 100, 80), 128);
    end, Exception);
end;


{ AlphaBlendBitmaps (Bitmap overload) Tests }

procedure TTestGraphAlpha.TestAlphaBlendBitmaps_Bitmap_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      AlphaBlendBitmaps(FMainBitmap, FSmallBitmap, 50, 0, 0);
    end);
end;


procedure TTestGraphAlpha.TestAlphaBlendBitmaps_Bitmap_NilMainRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      AlphaBlendBitmaps(NIL, FSmallBitmap, 50, 0, 0);
    end, Exception);
end;


procedure TTestGraphAlpha.TestAlphaBlendBitmaps_Bitmap_NilSmallRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      AlphaBlendBitmaps(FMainBitmap, NIL, 50, 0, 0);
    end, Exception);
end;


procedure TTestGraphAlpha.TestAlphaBlendBitmaps_Bitmap_WrongPixelFormatRaises;
begin
  FSmallBitmap.PixelFormat:= pf32bit;

  Assert.WillRaise(
    procedure
    begin
      AlphaBlendBitmaps(FMainBitmap, FSmallBitmap, 50, 0, 0);
    end, Exception);
end;


procedure TTestGraphAlpha.TestAlphaBlendBitmaps_Bitmap_NegativeXRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      AlphaBlendBitmaps(FMainBitmap, FSmallBitmap, 50, -1, 0);
    end, Exception);
end;


procedure TTestGraphAlpha.TestAlphaBlendBitmaps_Bitmap_NegativeYRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      AlphaBlendBitmaps(FMainBitmap, FSmallBitmap, 50, 0, -1);
    end, Exception);
end;


procedure TTestGraphAlpha.TestAlphaBlendBitmaps_Bitmap_ZeroTransparency;
var
  PixelColor: TColor;
begin
  { Zero transparency means SmallBitmap fully visible }
  FillBitmapWithColor(FMainBitmap, clWhite);
  FillBitmapWithColor(FSmallBitmap, clRed);

  AlphaBlendBitmaps(FMainBitmap, FSmallBitmap, 0, 0, 0);

  { At position (25, 20) which is inside SmallBitmap area, should be red }
  PixelColor:= FMainBitmap.Canvas.Pixels[25, 20];
  Assert.AreEqual(clRed, PixelColor, 'At 0% transparency, SmallBitmap should be fully visible');
end;


procedure TTestGraphAlpha.TestAlphaBlendBitmaps_Bitmap_FullTransparency;
var
  PixelColor: TColor;
begin
  { Full transparency means MainBitmap preserved }
  FillBitmapWithColor(FMainBitmap, clWhite);
  FillBitmapWithColor(FSmallBitmap, clRed);

  AlphaBlendBitmaps(FMainBitmap, FSmallBitmap, 100, 0, 0);

  { At position (25, 20) which is inside SmallBitmap area, should be white }
  PixelColor:= FMainBitmap.Canvas.Pixels[25, 20];
  Assert.AreEqual(clWhite, PixelColor, 'At 100% transparency, MainBitmap should be preserved');
end;


procedure TTestGraphAlpha.TestAlphaBlendBitmaps_Bitmap_HalfTransparency;
var
  PixelColor: TColor;
  R, G, B: Byte;
begin
  { 50% transparency should blend colors }
  FillBitmapWithColor(FMainBitmap, clWhite);  { RGB(255, 255, 255) }
  FillBitmapWithColor(FSmallBitmap, clRed);   { RGB(255, 0, 0) }

  AlphaBlendBitmaps(FMainBitmap, FSmallBitmap, 50, 0, 0);

  PixelColor:= FMainBitmap.Canvas.Pixels[25, 20];
  R:= GetRValue(PixelColor);
  G:= GetGValue(PixelColor);
  B:= GetBValue(PixelColor);

  { Red channel: (255 + 255) / 2 = 255 }
  Assert.AreEqual(Byte(255), R, 'Red channel should be 255');
  { Green/Blue: (255 + 0) / 2 ~ 127-128 }
  Assert.IsTrue((G >= 126) AND (G <= 129), 'Green channel should be ~127-128');
  Assert.IsTrue((B >= 126) AND (B <= 129), 'Blue channel should be ~127-128');
end;


procedure TTestGraphAlpha.TestAlphaBlendBitmaps_Bitmap_WithOffset;
var
  PixelColor: TColor;
begin
  FillBitmapWithColor(FMainBitmap, clWhite);
  FillBitmapWithColor(FSmallBitmap, clBlue);

  AlphaBlendBitmaps(FMainBitmap, FSmallBitmap, 0, 10, 10);

  { Position (5, 5) is outside SmallBitmap area, should be white }
  PixelColor:= FMainBitmap.Canvas.Pixels[5, 5];
  Assert.AreEqual(clWhite, PixelColor, 'Outside blend area should remain white');

  { Position (15, 15) is inside SmallBitmap area, should be blue }
  PixelColor:= FMainBitmap.Canvas.Pixels[15, 15];
  Assert.AreEqual(clBlue, PixelColor, 'Inside blend area should be blue');
end;


procedure TTestGraphAlpha.TestAlphaBlendBitmaps_Bitmap_SmallBitmapLargerClips;
begin
  { SmallBitmap extends beyond MainBitmap - should not crash }
  CreateTestBitmaps(50, 50, 100, 100);

  Assert.WillNotRaise(
    procedure
    begin
      AlphaBlendBitmaps(FMainBitmap, FSmallBitmap, 50, 0, 0);
    end);

  { Also test with offset that pushes beyond bounds }
  CreateTestBitmaps(100, 100, 50, 50);

  Assert.WillNotRaise(
    procedure
    begin
      AlphaBlendBitmaps(FMainBitmap, FSmallBitmap, 50, 80, 80);
    end);
end;


{ TransparencyBlend Tests }

procedure TTestGraphAlpha.TestTransparencyBlend_BasicCall;
var
  ResultBitmap: TBitmap;
begin
  ResultBitmap:= TransparencyBlend(FMainBitmap, FSmallBitmap, clGreen, 0, 0);
  TRY
    Assert.IsNotNull(ResultBitmap, 'TransparencyBlend should return a bitmap');
  FINALLY
    FreeAndNil(ResultBitmap);
  END;
end;


procedure TTestGraphAlpha.TestTransparencyBlend_NilMainRaises;
begin
  Assert.WillRaise(
    procedure
    var
      ResultBitmap: TBitmap;
    begin
      ResultBitmap:= TransparencyBlend(NIL, FSmallBitmap, clGreen, 0, 0);
      FreeAndNil(ResultBitmap);
    end, Exception);
end;


procedure TTestGraphAlpha.TestTransparencyBlend_NilSmallRaises;
begin
  Assert.WillRaise(
    procedure
    var
      ResultBitmap: TBitmap;
    begin
      ResultBitmap:= TransparencyBlend(FMainBitmap, NIL, clGreen, 0, 0);
      FreeAndNil(ResultBitmap);
    end, Exception);
end;


procedure TTestGraphAlpha.TestTransparencyBlend_MainWrongPixelFormatRaises;
begin
  FMainBitmap.PixelFormat:= pf32bit;

  Assert.WillRaise(
    procedure
    var
      ResultBitmap: TBitmap;
    begin
      ResultBitmap:= TransparencyBlend(FMainBitmap, FSmallBitmap, clGreen, 0, 0);
      FreeAndNil(ResultBitmap);
    end, Exception);
end;


procedure TTestGraphAlpha.TestTransparencyBlend_SmallWrongPixelFormatRaises;
begin
  FSmallBitmap.PixelFormat:= pf32bit;

  Assert.WillRaise(
    procedure
    var
      ResultBitmap: TBitmap;
    begin
      ResultBitmap:= TransparencyBlend(FMainBitmap, FSmallBitmap, clGreen, 0, 0);
      FreeAndNil(ResultBitmap);
    end, Exception);
end;


procedure TTestGraphAlpha.TestTransparencyBlend_NegativeXRaises;
begin
  Assert.WillRaise(
    procedure
    var
      ResultBitmap: TBitmap;
    begin
      ResultBitmap:= TransparencyBlend(FMainBitmap, FSmallBitmap, clGreen, -1, 0);
      FreeAndNil(ResultBitmap);
    end, Exception);
end;


procedure TTestGraphAlpha.TestTransparencyBlend_NegativeYRaises;
begin
  Assert.WillRaise(
    procedure
    var
      ResultBitmap: TBitmap;
    begin
      ResultBitmap:= TransparencyBlend(FMainBitmap, FSmallBitmap, clGreen, 0, -1);
      FreeAndNil(ResultBitmap);
    end, Exception);
end;


procedure TTestGraphAlpha.TestTransparencyBlend_ReturnsNewBitmap;
var
  ResultBitmap: TBitmap;
begin
  ResultBitmap:= TransparencyBlend(FMainBitmap, FSmallBitmap, clGreen, 0, 0);
  TRY
    Assert.AreNotEqual(Pointer(FMainBitmap), Pointer(ResultBitmap),
      'TransparencyBlend should return a new bitmap, not the original');
  FINALLY
    FreeAndNil(ResultBitmap);
  END;
end;


procedure TTestGraphAlpha.TestTransparencyBlend_CorrectDimensions;
var
  ResultBitmap: TBitmap;
begin
  ResultBitmap:= TransparencyBlend(FMainBitmap, FSmallBitmap, clGreen, 0, 0);
  TRY
    Assert.AreEqual(FMainBitmap.Width, ResultBitmap.Width, 'Width should match MainBitmap');
    Assert.AreEqual(FMainBitmap.Height, ResultBitmap.Height, 'Height should match MainBitmap');
  FINALLY
    FreeAndNil(ResultBitmap);
  END;
end;


procedure TTestGraphAlpha.TestTransparencyBlend_TransparentPixelsNotCopied;
var
  ResultBitmap: TBitmap;
  PixelColor: TColor;
begin
  { MainBitmap is white, SmallBitmap is red (transparent color) }
  FillBitmapWithColor(FMainBitmap, clWhite);
  FillBitmapWithColor(FSmallBitmap, clRed);

  ResultBitmap:= TransparencyBlend(FMainBitmap, FSmallBitmap, clRed, 0, 0);
  TRY
    { Red pixels should not be copied, so result should be white }
    PixelColor:= ResultBitmap.Canvas.Pixels[25, 20];
    Assert.AreEqual(clWhite, PixelColor, 'Transparent pixels should not be copied');
  FINALLY
    FreeAndNil(ResultBitmap);
  END;
end;


procedure TTestGraphAlpha.TestTransparencyBlend_NonTransparentPixelsCopied;
var
  ResultBitmap: TBitmap;
  PixelColor: TColor;
begin
  { MainBitmap is white, SmallBitmap is blue, transparent color is red }
  FillBitmapWithColor(FMainBitmap, clWhite);
  FillBitmapWithColor(FSmallBitmap, clBlue);

  ResultBitmap:= TransparencyBlend(FMainBitmap, FSmallBitmap, clRed, 0, 0);
  TRY
    { Blue pixels should be copied since they're not the transparent color }
    PixelColor:= ResultBitmap.Canvas.Pixels[25, 20];
    Assert.AreEqual(clBlue, PixelColor, 'Non-transparent pixels should be copied');
  FINALLY
    FreeAndNil(ResultBitmap);
  END;
end;


procedure TTestGraphAlpha.TestTransparencyBlend_WithOffset;
var
  ResultBitmap: TBitmap;
  PixelColor: TColor;
begin
  FillBitmapWithColor(FMainBitmap, clWhite);
  FillBitmapWithColor(FSmallBitmap, clBlue);

  ResultBitmap:= TransparencyBlend(FMainBitmap, FSmallBitmap, clRed, 20, 20);
  TRY
    { Position (10, 10) is outside SmallBitmap area, should be white }
    PixelColor:= ResultBitmap.Canvas.Pixels[10, 10];
    Assert.AreEqual(clWhite, PixelColor, 'Outside blend area should remain white');

    { Position (30, 30) is inside SmallBitmap area, should be blue }
    PixelColor:= ResultBitmap.Canvas.Pixels[30, 30];
    Assert.AreEqual(clBlue, PixelColor, 'Inside blend area should be blue');
  FINALLY
    FreeAndNil(ResultBitmap);
  END;
end;


{ GetTransparentBitmapFromImagelist Tests }

procedure TTestGraphAlpha.TestGetTransparentBitmapFromImagelist_BasicCall;
var
  ImageList: TImageList;
  Bitmap: TBitmap;
  TempBmp: TBitmap;
begin
  ImageList:= TImageList.Create(NIL);
  Bitmap:= TBitmap.Create;
  TempBmp:= TBitmap.Create;
  TRY
    { Add a bitmap to the ImageList }
    TempBmp.Width:= 16;
    TempBmp.Height:= 16;
    TempBmp.PixelFormat:= pf24bit;
    TempBmp.Canvas.Brush.Color:= clRed;
    TempBmp.Canvas.FillRect(Rect(0, 0, 16, 16));
    ImageList.Add(TempBmp, NIL);

    Assert.WillNotRaise(
      procedure
      begin
        GetTransparentBitmapFromImagelist(ImageList, 0, Bitmap);
      end);
  FINALLY
    FreeAndNil(TempBmp);
    FreeAndNil(Bitmap);
    FreeAndNil(ImageList);
  END;
end;


procedure TTestGraphAlpha.TestGetTransparentBitmapFromImagelist_NilImageListRaises;
var
  Bitmap: TBitmap;
begin
  Bitmap:= TBitmap.Create;
  TRY
    Assert.WillRaise(
      procedure
      begin
        GetTransparentBitmapFromImagelist(NIL, 0, Bitmap);
      end, Exception);
  FINALLY
    FreeAndNil(Bitmap);
  END;
end;


procedure TTestGraphAlpha.TestGetTransparentBitmapFromImagelist_NilBitmapRaises;
var
  ImageList: TImageList;
  TempBmp: TBitmap;
begin
  ImageList:= TImageList.Create(NIL);
  TempBmp:= TBitmap.Create;
  TRY
    TempBmp.Width:= 16;
    TempBmp.Height:= 16;
    ImageList.Add(TempBmp, NIL);

    Assert.WillRaise(
      procedure
      begin
        GetTransparentBitmapFromImagelist(ImageList, 0, NIL);
      end, Exception);
  FINALLY
    FreeAndNil(TempBmp);
    FreeAndNil(ImageList);
  END;
end;


procedure TTestGraphAlpha.TestGetTransparentBitmapFromImagelist_NegativeIndexRaises;
var
  ImageList: TImageList;
  Bitmap: TBitmap;
  TempBmp: TBitmap;
begin
  ImageList:= TImageList.Create(NIL);
  Bitmap:= TBitmap.Create;
  TempBmp:= TBitmap.Create;
  TRY
    TempBmp.Width:= 16;
    TempBmp.Height:= 16;
    ImageList.Add(TempBmp, NIL);

    Assert.WillRaise(
      procedure
      begin
        GetTransparentBitmapFromImagelist(ImageList, -1, Bitmap);
      end, Exception);
  FINALLY
    FreeAndNil(TempBmp);
    FreeAndNil(Bitmap);
    FreeAndNil(ImageList);
  END;
end;


procedure TTestGraphAlpha.TestGetTransparentBitmapFromImagelist_IndexOutOfRangeRaises;
var
  ImageList: TImageList;
  Bitmap: TBitmap;
  TempBmp: TBitmap;
begin
  ImageList:= TImageList.Create(NIL);
  Bitmap:= TBitmap.Create;
  TempBmp:= TBitmap.Create;
  TRY
    TempBmp.Width:= 16;
    TempBmp.Height:= 16;
    ImageList.Add(TempBmp, NIL);

    Assert.WillRaise(
      procedure
      begin
        GetTransparentBitmapFromImagelist(ImageList, 5, Bitmap);
      end, Exception);
  FINALLY
    FreeAndNil(TempBmp);
    FreeAndNil(Bitmap);
    FreeAndNil(ImageList);
  END;
end;


procedure TTestGraphAlpha.TestGetTransparentBitmapFromImagelist_CorrectSize;
var
  ImageList: TImageList;
  Bitmap: TBitmap;
  TempBmp: TBitmap;
begin
  ImageList:= TImageList.Create(NIL);
  Bitmap:= TBitmap.Create;
  TempBmp:= TBitmap.Create;
  TRY
    ImageList.Width:= 24;
    ImageList.Height:= 24;

    TempBmp.Width:= 24;
    TempBmp.Height:= 24;
    TempBmp.PixelFormat:= pf24bit;
    TempBmp.Canvas.Brush.Color:= clBlue;
    TempBmp.Canvas.FillRect(Rect(0, 0, 24, 24));
    ImageList.Add(TempBmp, NIL);

    GetTransparentBitmapFromImagelist(ImageList, 0, Bitmap);

    Assert.AreEqual(24, Bitmap.Width, 'Width should match ImageList');
    Assert.AreEqual(24, Bitmap.Height, 'Height should match ImageList');
    Assert.AreEqual(pf32bit, Bitmap.PixelFormat, 'PixelFormat should be pf32bit');
  FINALLY
    FreeAndNil(TempBmp);
    FreeAndNil(Bitmap);
    FreeAndNil(ImageList);
  END;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestGraphAlpha);

end.
