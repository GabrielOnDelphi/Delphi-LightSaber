unit Test.LightVcl.Graph.Resize;

{=============================================================================================================
   Unit tests for LightVcl.Graph.Resize.pas
   Tests image resizing functions including SmartStretch, StretchPercent, StretchProport, and LoadAndStretch.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Vcl.Graphics;

type
  [TestFixture]
  TTestGraphResize = class
  private
    FBitmap: TBitmap;
    procedure CreateTestBitmap(Width, Height: Integer);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { SmartStretch with RResizeParams Tests }
    [Test]
    procedure TestSmartStretch_WithParams_NilBitmap;

    [Test]
    procedure TestSmartStretch_WithParams_BasicCall;

    { SmartStretch with MaxWidth/MaxHeight Tests }
    [Test]
    procedure TestSmartStretch_WithDimensions_NilBitmap;

    [Test]
    procedure TestSmartStretch_WithDimensions_InvalidWidth;

    [Test]
    procedure TestSmartStretch_WithDimensions_InvalidHeight;

    [Test]
    procedure TestSmartStretch_WithDimensions_BasicCall;

    [Test]
    procedure TestSmartStretch_WithDimensions_ResizesDown;

    [Test]
    procedure TestSmartStretch_WithDimensions_ResizesUp;

    { SmartStretch with TResizeOp Tests }
    [Test]
    procedure TestSmartStretch_WithResizeOp_NilBitmap;

    [Test]
    procedure TestSmartStretch_WithResizeOp_Fit;

    [Test]
    procedure TestSmartStretch_WithResizeOp_Fill;

    { SmartStretch with FitTolerance Tests }
    [Test]
    procedure TestSmartStretch_WithTolerance_NilBitmap;

    [Test]
    procedure TestSmartStretch_WithTolerance_InvalidLow;

    [Test]
    procedure TestSmartStretch_WithTolerance_InvalidHigh;

    [Test]
    procedure TestSmartStretch_WithTolerance_BasicCall;

    { SmartStretchCrop Tests }
    [Test]
    procedure TestSmartStretchCrop_NilBitmap;

    [Test]
    procedure TestSmartStretchCrop_InvalidWidth;

    [Test]
    procedure TestSmartStretchCrop_InvalidHeight;

    [Test]
    procedure TestSmartStretchCrop_BasicCall;

    [Test]
    procedure TestSmartStretchCrop_CropsToExactSize;

    { StretchProport with MaxWidth/MaxHeight Tests }
    [Test]
    procedure TestStretchProport_WithDimensions_NilBitmap;

    [Test]
    procedure TestStretchProport_WithDimensions_InvalidWidth;

    [Test]
    procedure TestStretchProport_WithDimensions_InvalidHeight;

    [Test]
    procedure TestStretchProport_WithDimensions_ZeroBMPWidth;

    [Test]
    procedure TestStretchProport_WithDimensions_ZeroBMPHeight;

    [Test]
    procedure TestStretchProport_WithDimensions_LandscapeImage;

    [Test]
    procedure TestStretchProport_WithDimensions_PortraitImage;

    [Test]
    procedure TestStretchProport_WithDimensions_SameAspectRatio;

    [Test]
    procedure TestStretchProport_WithDimensions_PreservesAspectRatio;

    { StretchProport with OutWidth Only Tests }
    [Test]
    procedure TestStretchProport_WithWidth_NilBitmap;

    [Test]
    procedure TestStretchProport_WithWidth_InvalidWidth;

    [Test]
    procedure TestStretchProport_WithWidth_BasicCall;

    [Test]
    procedure TestStretchProport_WithWidth_PreservesAspectRatio;

    { StretchProportF Tests }
    [Test]
    procedure TestStretchProportF_NilBitmap;

    [Test]
    procedure TestStretchProportF_ReturnsNewBitmap;

    [Test]
    procedure TestStretchProportF_OriginalUnchanged;

    [Test]
    procedure TestStretchProportF_ResultHasCorrectDimensions;

    { StretchPercent Tests }
    [Test]
    procedure TestStretchPercent_NilBitmap;

    [Test]
    procedure TestStretchPercent_InvalidPercent_TooLow;

    [Test]
    procedure TestStretchPercent_InvalidPercent_MinusHundred;

    [Test]
    procedure TestStretchPercent_ZeroPercent_NoChange;

    [Test]
    procedure TestStretchPercent_PositivePercent_Enlarges;

    [Test]
    procedure TestStretchPercent_NegativePercent_Shrinks;

    [Test]
    procedure TestStretchPercent_FiftyPercent_Enlarges;

    [Test]
    procedure TestStretchPercent_MinusFiftyPercent_Shrinks;

    { StretchPercentX Tests }
    [Test]
    procedure TestStretchPercentX_NilBitmap;

    [Test]
    procedure TestStretchPercentX_InvalidTimes_Zero;

    [Test]
    procedure TestStretchPercentX_InvalidTimes_Negative;

    [Test]
    procedure TestStretchPercentX_OneTimes_NoChange;

    [Test]
    procedure TestStretchPercentX_TwoTimes_Doubles;

    [Test]
    procedure TestStretchPercentX_HalfTimes_Halves;

    [Test]
    procedure TestStretchPercentX_OnePointFiveTimes;

    [Test]
    procedure TestStretchPercentX_CorrectCalculation_TwoX;

    [Test]
    procedure TestStretchPercentX_CorrectCalculation_ThreeX;
  end;

implementation

uses
  LightVcl.Graph.Resize, LightVcl.Graph.ResizeParams, LightVcl.Graph.Bitmap;


procedure TTestGraphResize.Setup;
begin
  FBitmap:= TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Width:= 200;
  FBitmap.Height:= 100;
  FBitmap.Canvas.Brush.Color:= clWhite;
  FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
end;


procedure TTestGraphResize.TearDown;
begin
  FreeAndNil(FBitmap);
end;


procedure TTestGraphResize.CreateTestBitmap(Width, Height: Integer);
begin
  FreeAndNil(FBitmap);
  FBitmap:= TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Width:= Width;
  FBitmap.Height:= Height;
  FBitmap.Canvas.Brush.Color:= clWhite;
  FBitmap.Canvas.FillRect(Rect(0, 0, Width, Height));
end;


{ SmartStretch with RResizeParams Tests }

procedure TTestGraphResize.TestSmartStretch_WithParams_NilBitmap;
var
  Params: RResizeParams;
begin
  Params.Reset;

  Assert.WillRaise(
    procedure
    begin
      SmartStretch(NIL, Params);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResize.TestSmartStretch_WithParams_BasicCall;
var
  Params: RResizeParams;
begin
  Params.Reset;
  Params.MaxWidth:= 100;
  Params.MaxHeight:= 50;

  Assert.WillNotRaise(
    procedure
    begin
      SmartStretch(FBitmap, Params);
    end);
end;


{ SmartStretch with MaxWidth/MaxHeight Tests }

procedure TTestGraphResize.TestSmartStretch_WithDimensions_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      SmartStretch(NIL, 100, 100);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResize.TestSmartStretch_WithDimensions_InvalidWidth;
begin
  Assert.WillRaise(
    procedure
    begin
      SmartStretch(FBitmap, 0, 100);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResize.TestSmartStretch_WithDimensions_InvalidHeight;
begin
  Assert.WillRaise(
    procedure
    begin
      SmartStretch(FBitmap, 100, -1);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResize.TestSmartStretch_WithDimensions_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      SmartStretch(FBitmap, 100, 100);
    end);
end;


procedure TTestGraphResize.TestSmartStretch_WithDimensions_ResizesDown;
begin
  CreateTestBitmap(400, 300);

  SmartStretch(FBitmap, 200, 200);

  { Image should be smaller but aspect ratio maintained }
  Assert.IsTrue(FBitmap.Width <= 200, 'Width should be <= 200');
  Assert.IsTrue(FBitmap.Height <= 200, 'Height should be <= 200');
end;


procedure TTestGraphResize.TestSmartStretch_WithDimensions_ResizesUp;
begin
  CreateTestBitmap(50, 50);

  SmartStretch(FBitmap, 200, 200);

  { Image should be larger }
  Assert.IsTrue(FBitmap.Width >= 50, 'Width should have increased');
end;


{ SmartStretch with TResizeOp Tests }

procedure TTestGraphResize.TestSmartStretch_WithResizeOp_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      SmartStretch(NIL, 100, 100, roFit);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResize.TestSmartStretch_WithResizeOp_Fit;
begin
  CreateTestBitmap(300, 200);

  SmartStretch(FBitmap, 100, 100, roFit);

  { Should fit within 100x100 }
  Assert.IsTrue(FBitmap.Width <= 100, 'Width should fit');
  Assert.IsTrue(FBitmap.Height <= 100, 'Height should fit');
end;


procedure TTestGraphResize.TestSmartStretch_WithResizeOp_Fill;
begin
  CreateTestBitmap(300, 200);

  SmartStretch(FBitmap, 100, 100, roFill);

  { At least one dimension should fill the target }
  Assert.IsTrue((FBitmap.Width >= 100) OR (FBitmap.Height >= 100),
    'At least one dimension should fill');
end;


{ SmartStretch with FitTolerance Tests }

procedure TTestGraphResize.TestSmartStretch_WithTolerance_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      SmartStretch(NIL, 100, 100, 10);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResize.TestSmartStretch_WithTolerance_InvalidLow;
begin
  Assert.WillRaise(
    procedure
    begin
      SmartStretch(FBitmap, 100, 100, -1);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResize.TestSmartStretch_WithTolerance_InvalidHigh;
begin
  Assert.WillRaise(
    procedure
    begin
      SmartStretch(FBitmap, 100, 100, 101);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResize.TestSmartStretch_WithTolerance_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      SmartStretch(FBitmap, 100, 100, 10);
    end);
end;


{ SmartStretchCrop Tests }

procedure TTestGraphResize.TestSmartStretchCrop_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      SmartStretchCrop(NIL, 100, 100);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResize.TestSmartStretchCrop_InvalidWidth;
begin
  Assert.WillRaise(
    procedure
    begin
      SmartStretchCrop(FBitmap, 0, 100);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResize.TestSmartStretchCrop_InvalidHeight;
begin
  Assert.WillRaise(
    procedure
    begin
      SmartStretchCrop(FBitmap, 100, 0);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResize.TestSmartStretchCrop_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      SmartStretchCrop(FBitmap, 100, 80);
    end);
end;


procedure TTestGraphResize.TestSmartStretchCrop_CropsToExactSize;
begin
  CreateTestBitmap(300, 200);

  SmartStretchCrop(FBitmap, 100, 100);

  { Result should be exactly 100x100 after cropping }
  Assert.AreEqual(100, FBitmap.Width, 'Width should be exactly 100');
  Assert.AreEqual(100, FBitmap.Height, 'Height should be exactly 100');
end;


{ StretchProport with MaxWidth/MaxHeight Tests }

procedure TTestGraphResize.TestStretchProport_WithDimensions_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      StretchProport(TBitmap(NIL), 100, 100);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResize.TestStretchProport_WithDimensions_InvalidWidth;
begin
  Assert.WillRaise(
    procedure
    begin
      StretchProport(FBitmap, 0, 100);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResize.TestStretchProport_WithDimensions_InvalidHeight;
begin
  Assert.WillRaise(
    procedure
    begin
      StretchProport(FBitmap, 100, 0);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResize.TestStretchProport_WithDimensions_ZeroBMPWidth;
var
  BMP: TBitmap;
begin
  BMP:= TBitmap.Create;
  TRY
    BMP.Width:= 0;
    BMP.Height:= 100;

    Assert.WillRaise(
      procedure
      begin
        StretchProport(BMP, 100, 100);
      end,
      EAssertionFailed);
  FINALLY
    FreeAndNil(BMP);
  END;
end;


procedure TTestGraphResize.TestStretchProport_WithDimensions_ZeroBMPHeight;
var
  BMP: TBitmap;
begin
  BMP:= TBitmap.Create;
  TRY
    BMP.Width:= 100;
    BMP.Height:= 0;

    Assert.WillRaise(
      procedure
      begin
        StretchProport(BMP, 100, 100);
      end,
      EAssertionFailed);
  FINALLY
    FreeAndNil(BMP);
  END;
end;


procedure TTestGraphResize.TestStretchProport_WithDimensions_LandscapeImage;
begin
  CreateTestBitmap(400, 200);  { 2:1 landscape }

  StretchProport(FBitmap, 200, 200);

  { Width should be constrained to 200, height proportionally smaller }
  Assert.AreEqual(200, FBitmap.Width, 'Width should be 200');
  Assert.AreEqual(100, FBitmap.Height, 'Height should be 100 (maintaining 2:1)');
end;


procedure TTestGraphResize.TestStretchProport_WithDimensions_PortraitImage;
begin
  CreateTestBitmap(100, 200);  { 1:2 portrait }

  StretchProport(FBitmap, 200, 200);

  { Height should be constrained to 200, width proportionally smaller }
  Assert.AreEqual(100, FBitmap.Width, 'Width should be 100 (maintaining 1:2)');
  Assert.AreEqual(200, FBitmap.Height, 'Height should be 200');
end;


procedure TTestGraphResize.TestStretchProport_WithDimensions_SameAspectRatio;
begin
  CreateTestBitmap(200, 100);  { 2:1 ratio }

  StretchProport(FBitmap, 400, 200);  { Also 2:1 ratio }

  { Should scale to exact target }
  Assert.AreEqual(400, FBitmap.Width, 'Width should be 400');
  Assert.AreEqual(200, FBitmap.Height, 'Height should be 200');
end;


procedure TTestGraphResize.TestStretchProport_WithDimensions_PreservesAspectRatio;
var
  OrigRatio, NewRatio: Double;
begin
  CreateTestBitmap(300, 200);
  OrigRatio:= 300 / 200;

  StretchProport(FBitmap, 150, 150);

  NewRatio:= FBitmap.Width / FBitmap.Height;

  Assert.AreEqual(OrigRatio, NewRatio, 0.01, 'Aspect ratio should be preserved');
end;


{ StretchProport with OutWidth Only Tests }

procedure TTestGraphResize.TestStretchProport_WithWidth_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      StretchProport(TBitmap(NIL), 100);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResize.TestStretchProport_WithWidth_InvalidWidth;
begin
  Assert.WillRaise(
    procedure
    begin
      StretchProport(FBitmap, 0);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResize.TestStretchProport_WithWidth_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      StretchProport(FBitmap, 100);
    end);
end;


procedure TTestGraphResize.TestStretchProport_WithWidth_PreservesAspectRatio;
var
  OrigRatio, NewRatio: Double;
begin
  CreateTestBitmap(400, 200);
  OrigRatio:= 400 / 200;

  StretchProport(FBitmap, 200);

  NewRatio:= FBitmap.Width / FBitmap.Height;

  Assert.AreEqual(200, FBitmap.Width, 'Width should be 200');
  Assert.AreEqual(OrigRatio, NewRatio, 0.01, 'Aspect ratio should be preserved');
end;


{ StretchProportF Tests }

procedure TTestGraphResize.TestStretchProportF_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      StretchProportF(NIL, 100, 100);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResize.TestStretchProportF_ReturnsNewBitmap;
var
  Result: TBitmap;
begin
  Result:= StretchProportF(FBitmap, 100, 100);
  TRY
    Assert.IsNotNull(Result, 'Should return a bitmap');
    Assert.AreNotEqual(Pointer(FBitmap), Pointer(Result), 'Should be a different object');
  FINALLY
    FreeAndNil(Result);
  END;
end;


procedure TTestGraphResize.TestStretchProportF_OriginalUnchanged;
var
  OrigWidth, OrigHeight: Integer;
  Result: TBitmap;
begin
  OrigWidth:= FBitmap.Width;
  OrigHeight:= FBitmap.Height;

  Result:= StretchProportF(FBitmap, 50, 50);
  TRY
    Assert.AreEqual(OrigWidth, FBitmap.Width, 'Original width should be unchanged');
    Assert.AreEqual(OrigHeight, FBitmap.Height, 'Original height should be unchanged');
  FINALLY
    FreeAndNil(Result);
  END;
end;


procedure TTestGraphResize.TestStretchProportF_ResultHasCorrectDimensions;
var
  Result: TBitmap;
begin
  CreateTestBitmap(400, 200);

  Result:= StretchProportF(FBitmap, 200, 200);
  TRY
    { 400x200 -> fit into 200x200 -> 200x100 }
    Assert.AreEqual(200, Result.Width, 'Result width should be 200');
    Assert.AreEqual(100, Result.Height, 'Result height should be 100');
  FINALLY
    FreeAndNil(Result);
  END;
end;


{ StretchPercent Tests }

procedure TTestGraphResize.TestStretchPercent_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      StretchPercent(NIL, 50);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResize.TestStretchPercent_InvalidPercent_TooLow;
begin
  Assert.WillRaise(
    procedure
    begin
      StretchPercent(FBitmap, -101);
    end,
    Exception);
end;


procedure TTestGraphResize.TestStretchPercent_InvalidPercent_MinusHundred;
begin
  Assert.WillRaise(
    procedure
    begin
      StretchPercent(FBitmap, -100);
    end,
    Exception);
end;


procedure TTestGraphResize.TestStretchPercent_ZeroPercent_NoChange;
var
  OrigWidth, OrigHeight: Integer;
begin
  OrigWidth:= FBitmap.Width;
  OrigHeight:= FBitmap.Height;

  StretchPercent(FBitmap, 0);

  Assert.AreEqual(OrigWidth, FBitmap.Width, 'Width should not change');
  Assert.AreEqual(OrigHeight, FBitmap.Height, 'Height should not change');
end;


procedure TTestGraphResize.TestStretchPercent_PositivePercent_Enlarges;
var
  OrigWidth: Integer;
begin
  OrigWidth:= FBitmap.Width;

  StretchPercent(FBitmap, 100); { +100% = double size }

  Assert.IsTrue(FBitmap.Width > OrigWidth, 'Width should increase');
end;


procedure TTestGraphResize.TestStretchPercent_NegativePercent_Shrinks;
var
  OrigWidth: Integer;
begin
  OrigWidth:= FBitmap.Width;

  StretchPercent(FBitmap, -50); { -50% = half size }

  Assert.IsTrue(FBitmap.Width < OrigWidth, 'Width should decrease');
end;


procedure TTestGraphResize.TestStretchPercent_FiftyPercent_Enlarges;
begin
  CreateTestBitmap(100, 100);

  StretchPercent(FBitmap, 50); { +50% -> 150% of original }

  Assert.AreEqual(150, FBitmap.Width, 'Width should be 150');
  Assert.AreEqual(150, FBitmap.Height, 'Height should be 150');
end;


procedure TTestGraphResize.TestStretchPercent_MinusFiftyPercent_Shrinks;
begin
  CreateTestBitmap(100, 100);

  StretchPercent(FBitmap, -50); { -50% -> 50% of original }

  Assert.AreEqual(50, FBitmap.Width, 'Width should be 50');
  Assert.AreEqual(50, FBitmap.Height, 'Height should be 50');
end;


{ StretchPercentX Tests }

procedure TTestGraphResize.TestStretchPercentX_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      StretchPercentX(NIL, 2.0);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResize.TestStretchPercentX_InvalidTimes_Zero;
begin
  Assert.WillRaise(
    procedure
    begin
      StretchPercentX(FBitmap, 0);
    end,
    Exception);
end;


procedure TTestGraphResize.TestStretchPercentX_InvalidTimes_Negative;
begin
  Assert.WillRaise(
    procedure
    begin
      StretchPercentX(FBitmap, -1.0);
    end,
    Exception);
end;


procedure TTestGraphResize.TestStretchPercentX_OneTimes_NoChange;
var
  OrigWidth, OrigHeight: Integer;
begin
  OrigWidth:= FBitmap.Width;
  OrigHeight:= FBitmap.Height;

  StretchPercentX(FBitmap, 1.0); { 1x = no change }

  Assert.AreEqual(OrigWidth, FBitmap.Width, 'Width should not change');
  Assert.AreEqual(OrigHeight, FBitmap.Height, 'Height should not change');
end;


procedure TTestGraphResize.TestStretchPercentX_TwoTimes_Doubles;
begin
  CreateTestBitmap(100, 100);

  StretchPercentX(FBitmap, 2.0); { 2x = double size }

  Assert.AreEqual(200, FBitmap.Width, 'Width should double to 200');
  Assert.AreEqual(200, FBitmap.Height, 'Height should double to 200');
end;


procedure TTestGraphResize.TestStretchPercentX_HalfTimes_Halves;
begin
  CreateTestBitmap(100, 100);

  StretchPercentX(FBitmap, 0.5); { 0.5x = half size }

  Assert.AreEqual(50, FBitmap.Width, 'Width should halve to 50');
  Assert.AreEqual(50, FBitmap.Height, 'Height should halve to 50');
end;


procedure TTestGraphResize.TestStretchPercentX_OnePointFiveTimes;
begin
  CreateTestBitmap(100, 100);

  StretchPercentX(FBitmap, 1.5); { 1.5x = 150% }

  Assert.AreEqual(150, FBitmap.Width, 'Width should be 150');
  Assert.AreEqual(150, FBitmap.Height, 'Height should be 150');
end;


procedure TTestGraphResize.TestStretchPercentX_CorrectCalculation_TwoX;
begin
  { This test specifically verifies the bug fix:
    2x should result in 200% of original, NOT 300% (which was the bug) }
  CreateTestBitmap(100, 50);

  StretchPercentX(FBitmap, 2.0);

  Assert.AreEqual(200, FBitmap.Width, '2x should double width to 200');
  Assert.AreEqual(100, FBitmap.Height, '2x should double height to 100');
end;


procedure TTestGraphResize.TestStretchPercentX_CorrectCalculation_ThreeX;
begin
  { 3x should result in 300% of original }
  CreateTestBitmap(100, 50);

  StretchPercentX(FBitmap, 3.0);

  Assert.AreEqual(300, FBitmap.Width, '3x should triple width to 300');
  Assert.AreEqual(150, FBitmap.Height, '3x should triple height to 150');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestGraphResize);

end.
