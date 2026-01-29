unit Test.LightVcl.Graph.FX;

{=============================================================================================================
   Unit tests for LightVcl.Graph.FX.pas
   Tests bitmap crop, tile, and flip operations.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Types,
  Vcl.Graphics;

type
  [TestFixture]
  TTestGraphFX = class
  private
    FBitmap: TBitmap;
    procedure FillBitmapWithColor(BMP: TBitmap; Color: TColor);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { CropBitmap Tests - XYWH overload }
    [Test]
    procedure TestCropBitmap_BasicCall;

    [Test]
    procedure TestCropBitmap_CropsCorrectly;

    [Test]
    procedure TestCropBitmap_LargerThanBitmap;

    [Test]
    procedure TestCropBitmap_AtOffset;

    { CropBitmap Tests - WH overload (centered) }
    [Test]
    procedure TestCropBitmap_Centered_BasicCall;

    [Test]
    procedure TestCropBitmap_Centered_CropsSymmetrically;

    { CropBitmap Tests - MasterBMP overload }
    [Test]
    procedure TestCropBitmap_MasterBMP_BasicCall;

    [Test]
    procedure TestCropBitmap_MasterBMP_MatchesDimensions;

    { TileBitmap Tests }
    [Test]
    procedure TestTileBitmap_BasicCall;

    [Test]
    procedure TestTileBitmap_ReturnsNewBitmap;

    [Test]
    procedure TestTileBitmap_CorrectDimensions;

    [Test]
    procedure TestTileBitmap_TilesMultipleTimes;

    { TileBitmapMirror Tests }
    [Test]
    procedure TestTileBitmapMirror_BasicCall;

    [Test]
    procedure TestTileBitmapMirror_HorizontalOnly;

    [Test]
    procedure TestTileBitmapMirror_VerticalOnly;

    [Test]
    procedure TestTileBitmapMirror_BothDirections;

    [Test]
    procedure TestTileBitmapMirror_OneRow;

    { FlipDown Tests }
    [Test]
    procedure TestFlipDown_BasicCall;

    [Test]
    procedure TestFlipDown_ModifiesBitmap;

    [Test]
    procedure TestFlipDown_DoubleFlipRestores;

    { FlipRight Tests }
    [Test]
    procedure TestFlipRight_BasicCall;

    [Test]
    procedure TestFlipRight_ModifiesBitmap;

    [Test]
    procedure TestFlipRight_DoubleFlipRestores;

    { RTileParams Tests }
    [Test]
    procedure TestRTileParams_Reset;

    { REnhanceParams Tests }
    [Test]
    procedure TestREnhanceParams_Reset;
  end;

implementation

uses
  LightVcl.Graph.FX;


procedure TTestGraphFX.Setup;
begin
  FBitmap:= TBitmap.Create;
  FBitmap.Width:= 100;
  FBitmap.Height:= 80;
  FBitmap.PixelFormat:= pf24bit;
  FillBitmapWithColor(FBitmap, clWhite);
end;


procedure TTestGraphFX.TearDown;
begin
  FreeAndNil(FBitmap);
end;


procedure TTestGraphFX.FillBitmapWithColor(BMP: TBitmap; Color: TColor);
begin
  BMP.Canvas.Brush.Color:= Color;
  BMP.Canvas.FillRect(Rect(0, 0, BMP.Width, BMP.Height));
end;


{ CropBitmap Tests - XYWH overload }

procedure TTestGraphFX.TestCropBitmap_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      CropBitmap(FBitmap, 0, 0, 50, 40);
    end);
end;


procedure TTestGraphFX.TestCropBitmap_CropsCorrectly;
begin
  CropBitmap(FBitmap, 0, 0, 50, 40);

  Assert.AreEqual(50, FBitmap.Width, 'Width should be cropped to 50');
  Assert.AreEqual(40, FBitmap.Height, 'Height should be cropped to 40');
end;


procedure TTestGraphFX.TestCropBitmap_LargerThanBitmap;
begin
  { When crop dimensions exceed bitmap size, should clamp to bitmap size }
  CropBitmap(FBitmap, 0, 0, 200, 200);

  Assert.AreEqual(100, FBitmap.Width, 'Width should be clamped to original 100');
  Assert.AreEqual(80, FBitmap.Height, 'Height should be clamped to original 80');
end;


procedure TTestGraphFX.TestCropBitmap_AtOffset;
begin
  CropBitmap(FBitmap, 10, 10, 30, 30);

  Assert.AreEqual(30, FBitmap.Width, 'Width should be 30');
  Assert.AreEqual(30, FBitmap.Height, 'Height should be 30');
end;


{ CropBitmap Tests - WH overload (centered) }

procedure TTestGraphFX.TestCropBitmap_Centered_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      CropBitmap(FBitmap, 50, 40);
    end);
end;


procedure TTestGraphFX.TestCropBitmap_Centered_CropsSymmetrically;
begin
  CropBitmap(FBitmap, 50, 40);

  Assert.AreEqual(50, FBitmap.Width, 'Width should be 50');
  Assert.AreEqual(40, FBitmap.Height, 'Height should be 40');
end;


{ CropBitmap Tests - MasterBMP overload }

procedure TTestGraphFX.TestCropBitmap_MasterBMP_BasicCall;
var
  MasterBMP: TBitmap;
begin
  MasterBMP:= TBitmap.Create;
  TRY
    MasterBMP.Width:= 60;
    MasterBMP.Height:= 50;

    Assert.WillNotRaise(
      procedure
      begin
        CropBitmap(FBitmap, MasterBMP);
      end);
  FINALLY
    FreeAndNil(MasterBMP);
  END;
end;


procedure TTestGraphFX.TestCropBitmap_MasterBMP_MatchesDimensions;
var
  MasterBMP: TBitmap;
begin
  MasterBMP:= TBitmap.Create;
  TRY
    MasterBMP.Width:= 60;
    MasterBMP.Height:= 50;

    CropBitmap(FBitmap, MasterBMP);

    Assert.AreEqual(60, FBitmap.Width, 'Width should match MasterBMP');
    Assert.AreEqual(50, FBitmap.Height, 'Height should match MasterBMP');
  FINALLY
    FreeAndNil(MasterBMP);
  END;
end;


{ TileBitmap Tests }

procedure TTestGraphFX.TestTileBitmap_BasicCall;
var
  Result: TBitmap;
begin
  Result:= TileBitmap(FBitmap, 200, 200);
  TRY
    Assert.IsNotNull(Result, 'TileBitmap should return a bitmap');
  FINALLY
    FreeAndNil(Result);
  END;
end;


procedure TTestGraphFX.TestTileBitmap_ReturnsNewBitmap;
var
  Result: TBitmap;
begin
  Result:= TileBitmap(FBitmap, 200, 200);
  TRY
    Assert.AreNotEqual(Pointer(FBitmap), Pointer(Result),
      'TileBitmap should return a new bitmap, not the original');
  FINALLY
    FreeAndNil(Result);
  END;
end;


procedure TTestGraphFX.TestTileBitmap_CorrectDimensions;
var
  Result: TBitmap;
begin
  Result:= TileBitmap(FBitmap, 250, 180);
  TRY
    Assert.AreEqual(250, Result.Width, 'Result width should be 250');
    Assert.AreEqual(180, Result.Height, 'Result height should be 180');
  FINALLY
    FreeAndNil(Result);
  END;
end;


procedure TTestGraphFX.TestTileBitmap_TilesMultipleTimes;
var
  Result: TBitmap;
  SmallBMP: TBitmap;
begin
  SmallBMP:= TBitmap.Create;
  TRY
    SmallBMP.Width:= 20;
    SmallBMP.Height:= 20;
    FillBitmapWithColor(SmallBMP, clRed);

    Result:= TileBitmap(SmallBMP, 100, 100);
    TRY
      { Should tile 5x5 = 25 times }
      Assert.AreEqual(100, Result.Width, 'Result should be 100 wide');
      Assert.AreEqual(100, Result.Height, 'Result should be 100 tall');
    FINALLY
      FreeAndNil(Result);
    END;
  FINALLY
    FreeAndNil(SmallBMP);
  END;
end;


{ TileBitmapMirror Tests }

procedure TTestGraphFX.TestTileBitmapMirror_BasicCall;
var
  TileType: RTileType;
begin
  TileType.Horizon:= FALSE;
  TileType.Vertical:= FALSE;
  TileType.OneRow:= FALSE;

  Assert.WillNotRaise(
    procedure
    begin
      TileBitmapMirror(FBitmap, 200, 200, TileType);
    end);
end;


procedure TTestGraphFX.TestTileBitmapMirror_HorizontalOnly;
var
  TileType: RTileType;
begin
  TileType.Horizon:= TRUE;
  TileType.Vertical:= FALSE;
  TileType.OneRow:= FALSE;

  Assert.WillNotRaise(
    procedure
    begin
      TileBitmapMirror(FBitmap, 200, 200, TileType);
    end);

  Assert.AreEqual(200, FBitmap.Width, 'Width should be 200');
  Assert.AreEqual(200, FBitmap.Height, 'Height should be 200');
end;


procedure TTestGraphFX.TestTileBitmapMirror_VerticalOnly;
var
  TileType: RTileType;
begin
  TileType.Horizon:= FALSE;
  TileType.Vertical:= TRUE;
  TileType.OneRow:= FALSE;

  Assert.WillNotRaise(
    procedure
    begin
      TileBitmapMirror(FBitmap, 200, 200, TileType);
    end);
end;


procedure TTestGraphFX.TestTileBitmapMirror_BothDirections;
var
  TileType: RTileType;
begin
  TileType.Horizon:= TRUE;
  TileType.Vertical:= TRUE;
  TileType.OneRow:= FALSE;

  Assert.WillNotRaise(
    procedure
    begin
      TileBitmapMirror(FBitmap, 200, 200, TileType);
    end);
end;


procedure TTestGraphFX.TestTileBitmapMirror_OneRow;
var
  TileType: RTileType;
begin
  TileType.Horizon:= TRUE;
  TileType.Vertical:= FALSE;
  TileType.OneRow:= TRUE;

  Assert.WillNotRaise(
    procedure
    begin
      TileBitmapMirror(FBitmap, 300, 100, TileType);
    end);
end;


{ FlipDown Tests }

procedure TTestGraphFX.TestFlipDown_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      FlipDown(FBitmap);
    end);
end;


procedure TTestGraphFX.TestFlipDown_ModifiesBitmap;
var
  TopPixel, BottomPixel: TColor;
begin
  { Create a gradient - red at top, blue at bottom }
  FBitmap.Canvas.Pixels[50, 0]:= clRed;
  FBitmap.Canvas.Pixels[50, FBitmap.Height - 1]:= clBlue;

  TopPixel:= FBitmap.Canvas.Pixels[50, 0];

  FlipDown(FBitmap);

  BottomPixel:= FBitmap.Canvas.Pixels[50, FBitmap.Height - 1];

  { After flip, top should have what was bottom }
  Assert.AreEqual(TopPixel, BottomPixel, 'Flip should swap top and bottom');
end;


procedure TTestGraphFX.TestFlipDown_DoubleFlipRestores;
var
  OriginalPixel, FinalPixel: TColor;
begin
  FBitmap.Canvas.Pixels[50, 10]:= clRed;
  OriginalPixel:= FBitmap.Canvas.Pixels[50, 10];

  FlipDown(FBitmap);
  FlipDown(FBitmap);

  FinalPixel:= FBitmap.Canvas.Pixels[50, 10];

  Assert.AreEqual(OriginalPixel, FinalPixel, 'Double flip should restore original');
end;


{ FlipRight Tests }

procedure TTestGraphFX.TestFlipRight_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      FlipRight(FBitmap);
    end);
end;


procedure TTestGraphFX.TestFlipRight_ModifiesBitmap;
var
  LeftPixel, RightPixel: TColor;
begin
  { Create distinct colors - red at left, blue at right }
  FBitmap.Canvas.Pixels[0, 40]:= clRed;
  FBitmap.Canvas.Pixels[FBitmap.Width - 1, 40]:= clBlue;

  LeftPixel:= FBitmap.Canvas.Pixels[0, 40];

  FlipRight(FBitmap);

  RightPixel:= FBitmap.Canvas.Pixels[FBitmap.Width - 1, 40];

  { After flip, right should have what was left }
  Assert.AreEqual(LeftPixel, RightPixel, 'Flip should swap left and right');
end;


procedure TTestGraphFX.TestFlipRight_DoubleFlipRestores;
var
  OriginalPixel, FinalPixel: TColor;
begin
  FBitmap.Canvas.Pixels[10, 40]:= clGreen;
  OriginalPixel:= FBitmap.Canvas.Pixels[10, 40];

  FlipRight(FBitmap);
  FlipRight(FBitmap);

  FinalPixel:= FBitmap.Canvas.Pixels[10, 40];

  Assert.AreEqual(OriginalPixel, FinalPixel, 'Double flip should restore original');
end;


{ RTileParams Tests }

procedure TTestGraphFX.TestRTileParams_Reset;
var
  Params: RTileParams;
begin
  { Set to non-default values first }
  Params.TileAuto:= FALSE;
  Params.TileType.Horizon:= FALSE;
  Params.TileType.Vertical:= TRUE;
  Params.TileType.OneRow:= FALSE;
  Params.TileThreshold:= 99;

  Params.Reset;

  Assert.IsTrue(Params.TileAuto, 'TileAuto should be TRUE after reset');
  Assert.IsTrue(Params.TileType.Horizon, 'Horizon should be TRUE after reset');
  Assert.IsFalse(Params.TileType.Vertical, 'Vertical should be FALSE after reset');
  Assert.IsTrue(Params.TileType.OneRow, 'OneRow should be TRUE after reset');
  Assert.AreEqual(40, Params.TileThreshold, 'TileThreshold should be 40 after reset');
end;


{ REnhanceParams Tests }

procedure TTestGraphFX.TestREnhanceParams_Reset;
var
  Params: REnhanceParams;
begin
  { Set to non-default values first }
  Params.Smooth:= TRUE;
  Params.Darkness:= 50;
  Params.Brightness:= 100;
  Params.Contrast:= 100;
  Params.Saturation:= 100;

  Params.Reset;

  Assert.IsFalse(Params.Smooth, 'Smooth should be FALSE after reset');
  Assert.AreEqual(ShortInt(0), Params.Darkness, 'Darkness should be 0 after reset');
  Assert.AreEqual(SmallInt(0), Params.Brightness, 'Brightness should be 0 after reset');
  Assert.AreEqual(SmallInt(0), Params.Contrast, 'Contrast should be 0 after reset');
  Assert.AreEqual(SmallInt(255), Params.Saturation, 'Saturation should be 255 after reset');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestGraphFX);

end.
