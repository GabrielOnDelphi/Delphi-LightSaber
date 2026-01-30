unit Test.LightVcl.Graph.FX.RotateGr32;

{=============================================================================================================
   Unit tests for LightVcl.Graph.FX.RotateGr32.pas
   Tests bitmap rotation with GR32 library support.

   Tests cover:
     - TBitmap rotation (basic overload)
     - TBitmap32 rotation
     - Composite rotation (Source/Destination overload)
     - AdjustSize parameter behavior
     - Nil parameter assertions
     - Various rotation angles

   Note: These tests require the Graphics32 library.
   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Types,
  Vcl.Graphics,
  GR32;

type
  [TestFixture]
  TTestRotateGr32 = class
  private
    FBitmap: TBitmap;
    FBitmap32: TBitmap32;
    procedure FillBitmapWithColor(BMP: TBitmap; Color: TColor);
    procedure FillBitmap32WithColor(BMP: TBitmap32; Color: TColor32);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { TBitmap overload - Basic functionality }
    [Test]
    procedure TestRotateBitmap_BasicCall;

    [Test]
    procedure TestRotateBitmap_ZeroAngle;

    [Test]
    procedure TestRotateBitmap_90Degrees;

    [Test]
    procedure TestRotateBitmap_180Degrees;

    [Test]
    procedure TestRotateBitmap_270Degrees;

    [Test]
    procedure TestRotateBitmap_ArbitraryAngle;

    { TBitmap overload - AdjustSize parameter }
    [Test]
    procedure TestRotateBitmap_AdjustSizeTrue_ExpandsDimensions;

    [Test]
    procedure TestRotateBitmap_AdjustSizeFalse_KeepsDimensions;

    { TBitmap overload - Transparent parameter }
    [Test]
    procedure TestRotateBitmap_TransparentTrue;

    [Test]
    procedure TestRotateBitmap_TransparentFalse;

    { TBitmap32 overload - Basic functionality }
    [Test]
    procedure TestRotateBitmap32_BasicCall;

    [Test]
    procedure TestRotateBitmap32_ZeroAngle;

    [Test]
    procedure TestRotateBitmap32_90Degrees;

    [Test]
    procedure TestRotateBitmap32_AdjustSizeTrue;

    [Test]
    procedure TestRotateBitmap32_AdjustSizeFalse;

    { Composite overload - Source/Destination }
    [Test]
    procedure TestRotateComposite_BasicCall;

    [Test]
    procedure TestRotateComposite_AtPosition;

    [Test]
    procedure TestRotateComposite_ZeroAngle;

    { Nil parameter handling }
    [Test]
    procedure TestRotateBitmap_NilBitmap_RaisesAssertion;

    [Test]
    procedure TestRotateBitmap32_NilBitmap_RaisesAssertion;

    [Test]
    procedure TestRotateComposite_NilSource_RaisesAssertion;

    [Test]
    procedure TestRotateComposite_NilDestination_RaisesAssertion;

    { Edge cases }
    [Test]
    procedure TestRotateBitmap_SmallImage;

    [Test]
    procedure TestRotateBitmap_SquareImage;

    [Test]
    procedure TestRotateBitmap_NegativeAngle;

    [Test]
    procedure TestRotateBitmap_LargeAngle;

    { Different resampler kernels }
    [Test]
    procedure TestRotateBitmap_DifferentKernels;
  end;

implementation

uses
  LightVcl.Graph.FX.RotateGr32,
  LightVcl.Graph.ResizeGr32;


procedure TTestRotateGr32.Setup;
begin
  FBitmap:= TBitmap.Create;
  FBitmap.Width:= 100;
  FBitmap.Height:= 80;
  FBitmap.PixelFormat:= pf24bit;
  FillBitmapWithColor(FBitmap, clWhite);

  FBitmap32:= TBitmap32.Create;
  FBitmap32.SetSize(100, 80);
  FillBitmap32WithColor(FBitmap32, clWhite32);
end;


procedure TTestRotateGr32.TearDown;
begin
  FreeAndNil(FBitmap);
  FreeAndNil(FBitmap32);
end;


procedure TTestRotateGr32.FillBitmapWithColor(BMP: TBitmap; Color: TColor);
begin
  BMP.Canvas.Brush.Color:= Color;
  BMP.Canvas.FillRect(Rect(0, 0, BMP.Width, BMP.Height));
end;


procedure TTestRotateGr32.FillBitmap32WithColor(BMP: TBitmap32; Color: TColor32);
begin
  BMP.Clear(Color);
end;


{ TBitmap overload - Basic functionality }

procedure TTestRotateGr32.TestRotateBitmap_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      RotateBitmapGR32(FBitmap, 45);
    end);
end;


procedure TTestRotateGr32.TestRotateBitmap_ZeroAngle;
var
  OrigWidth, OrigHeight: Integer;
begin
  OrigWidth:= FBitmap.Width;
  OrigHeight:= FBitmap.Height;

  RotateBitmapGR32(FBitmap, 0);

  { Zero rotation should preserve dimensions (with AdjustSize=True default) }
  Assert.AreEqual(OrigWidth, FBitmap.Width, 'Width should be preserved with 0 angle');
  Assert.AreEqual(OrigHeight, FBitmap.Height, 'Height should be preserved with 0 angle');
end;


procedure TTestRotateGr32.TestRotateBitmap_90Degrees;
begin
  Assert.WillNotRaise(
    procedure
    begin
      RotateBitmapGR32(FBitmap, 90, True);
    end);

  { After 90 degree rotation with AdjustSize, dimensions should swap (approximately) }
  Assert.IsTrue(FBitmap.Width > 0, 'Bitmap should have valid width');
  Assert.IsTrue(FBitmap.Height > 0, 'Bitmap should have valid height');
end;


procedure TTestRotateGr32.TestRotateBitmap_180Degrees;
var
  OrigWidth, OrigHeight: Integer;
begin
  OrigWidth:= FBitmap.Width;
  OrigHeight:= FBitmap.Height;

  RotateBitmapGR32(FBitmap, 180, True);

  { 180 degree rotation should preserve dimensions }
  Assert.AreEqual(OrigWidth, FBitmap.Width, 'Width should be preserved with 180 angle');
  Assert.AreEqual(OrigHeight, FBitmap.Height, 'Height should be preserved with 180 angle');
end;


procedure TTestRotateGr32.TestRotateBitmap_270Degrees;
begin
  Assert.WillNotRaise(
    procedure
    begin
      RotateBitmapGR32(FBitmap, 270, True);
    end);

  Assert.IsTrue(FBitmap.Width > 0, 'Bitmap should have valid width');
  Assert.IsTrue(FBitmap.Height > 0, 'Bitmap should have valid height');
end;


procedure TTestRotateGr32.TestRotateBitmap_ArbitraryAngle;
begin
  Assert.WillNotRaise(
    procedure
    begin
      RotateBitmapGR32(FBitmap, 37.5, True);
    end);

  Assert.IsTrue(FBitmap.Width > 0, 'Bitmap should have valid width');
  Assert.IsTrue(FBitmap.Height > 0, 'Bitmap should have valid height');
end;


{ TBitmap overload - AdjustSize parameter }

procedure TTestRotateGr32.TestRotateBitmap_AdjustSizeTrue_ExpandsDimensions;
var
  OrigWidth, OrigHeight: Integer;
begin
  OrigWidth:= FBitmap.Width;
  OrigHeight:= FBitmap.Height;

  { 45 degree rotation should expand dimensions when AdjustSize=True }
  RotateBitmapGR32(FBitmap, 45, True);

  { Diagonal of a rectangle is longer than its sides }
  Assert.IsTrue((FBitmap.Width > OrigWidth) OR (FBitmap.Height > OrigHeight),
    'Dimensions should expand with 45 degree rotation and AdjustSize=True');
end;


procedure TTestRotateGr32.TestRotateBitmap_AdjustSizeFalse_KeepsDimensions;
var
  OrigWidth, OrigHeight: Integer;
begin
  OrigWidth:= FBitmap.Width;
  OrigHeight:= FBitmap.Height;

  RotateBitmapGR32(FBitmap, 45, False);

  Assert.AreEqual(OrigWidth, FBitmap.Width, 'Width should be preserved with AdjustSize=False');
  Assert.AreEqual(OrigHeight, FBitmap.Height, 'Height should be preserved with AdjustSize=False');
end;


{ TBitmap overload - Transparent parameter }

procedure TTestRotateGr32.TestRotateBitmap_TransparentTrue;
begin
  RotateBitmapGR32(FBitmap, 45, True, clPurple, True);

  Assert.IsTrue(FBitmap.Transparent, 'Bitmap should be transparent when Transparent=True');
end;


procedure TTestRotateGr32.TestRotateBitmap_TransparentFalse;
begin
  RotateBitmapGR32(FBitmap, 45, True, clPurple, False);

  Assert.IsFalse(FBitmap.Transparent, 'Bitmap should not be transparent when Transparent=False');
end;


{ TBitmap32 overload - Basic functionality }

procedure TTestRotateGr32.TestRotateBitmap32_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      RotateBitmapGR32(FBitmap32, 45);
    end);
end;


procedure TTestRotateGr32.TestRotateBitmap32_ZeroAngle;
var
  OrigWidth, OrigHeight: Integer;
begin
  OrigWidth:= FBitmap32.Width;
  OrigHeight:= FBitmap32.Height;

  RotateBitmapGR32(FBitmap32, 0);

  Assert.AreEqual(OrigWidth, FBitmap32.Width, 'Width should be preserved with 0 angle');
  Assert.AreEqual(OrigHeight, FBitmap32.Height, 'Height should be preserved with 0 angle');
end;


procedure TTestRotateGr32.TestRotateBitmap32_90Degrees;
begin
  Assert.WillNotRaise(
    procedure
    begin
      RotateBitmapGR32(FBitmap32, 90, True);
    end);

  Assert.IsTrue(FBitmap32.Width > 0, 'Bitmap32 should have valid width');
  Assert.IsTrue(FBitmap32.Height > 0, 'Bitmap32 should have valid height');
end;


procedure TTestRotateGr32.TestRotateBitmap32_AdjustSizeTrue;
var
  OrigWidth, OrigHeight: Integer;
begin
  OrigWidth:= FBitmap32.Width;
  OrigHeight:= FBitmap32.Height;

  RotateBitmapGR32(FBitmap32, 45, True);

  Assert.IsTrue((FBitmap32.Width > OrigWidth) OR (FBitmap32.Height > OrigHeight),
    'Dimensions should expand with 45 degree rotation and AdjustSize=True');
end;


procedure TTestRotateGr32.TestRotateBitmap32_AdjustSizeFalse;
var
  OrigWidth, OrigHeight: Integer;
begin
  OrigWidth:= FBitmap32.Width;
  OrigHeight:= FBitmap32.Height;

  RotateBitmapGR32(FBitmap32, 45, False);

  Assert.AreEqual(OrigWidth, FBitmap32.Width, 'Width should be preserved with AdjustSize=False');
  Assert.AreEqual(OrigHeight, FBitmap32.Height, 'Height should be preserved with AdjustSize=False');
end;


{ Composite overload - Source/Destination }

procedure TTestRotateGr32.TestRotateComposite_BasicCall;
var
  Source, Destination: TBitmap;
begin
  Source:= TBitmap.Create;
  Destination:= TBitmap.Create;
  TRY
    Source.Width:= 50;
    Source.Height:= 50;
    Source.PixelFormat:= pf24bit;
    FillBitmapWithColor(Source, clRed);

    Destination.Width:= 200;
    Destination.Height:= 200;
    Destination.PixelFormat:= pf24bit;
    FillBitmapWithColor(Destination, clBlue);

    Assert.WillNotRaise(
      procedure
      begin
        RotateBitmapGR32(Source, Destination, 30, 50, 50);
      end);
  FINALLY
    FreeAndNil(Source);
    FreeAndNil(Destination);
  END;
end;


procedure TTestRotateGr32.TestRotateComposite_AtPosition;
var
  Source, Destination: TBitmap;
begin
  Source:= TBitmap.Create;
  Destination:= TBitmap.Create;
  TRY
    Source.Width:= 40;
    Source.Height:= 40;
    Source.PixelFormat:= pf24bit;
    FillBitmapWithColor(Source, clGreen);

    Destination.Width:= 150;
    Destination.Height:= 150;
    Destination.PixelFormat:= pf24bit;
    FillBitmapWithColor(Destination, clWhite);

    { Composite at corner position }
    RotateBitmapGR32(Source, Destination, 45, 10, 10);

    { Destination should maintain its size }
    Assert.AreEqual(150, Destination.Width, 'Destination width should be preserved');
    Assert.AreEqual(150, Destination.Height, 'Destination height should be preserved');
  FINALLY
    FreeAndNil(Source);
    FreeAndNil(Destination);
  END;
end;


procedure TTestRotateGr32.TestRotateComposite_ZeroAngle;
var
  Source, Destination: TBitmap;
begin
  Source:= TBitmap.Create;
  Destination:= TBitmap.Create;
  TRY
    Source.Width:= 50;
    Source.Height:= 50;
    Source.PixelFormat:= pf24bit;

    Destination.Width:= 200;
    Destination.Height:= 200;
    Destination.PixelFormat:= pf24bit;

    Assert.WillNotRaise(
      procedure
      begin
        RotateBitmapGR32(Source, Destination, 0, 0, 0);
      end);
  FINALLY
    FreeAndNil(Source);
    FreeAndNil(Destination);
  END;
end;


{ Nil parameter handling }

procedure TTestRotateGr32.TestRotateBitmap_NilBitmap_RaisesAssertion;
var
  NilBitmap: TBitmap;
begin
  NilBitmap:= NIL;

  Assert.WillRaise(
    procedure
    begin
      RotateBitmapGR32(NilBitmap, 45);
    end,
    EAssertionFailed,
    'Should raise assertion for nil bitmap');
end;


procedure TTestRotateGr32.TestRotateBitmap32_NilBitmap_RaisesAssertion;
var
  NilBitmap32: TBitmap32;
begin
  NilBitmap32:= NIL;

  Assert.WillRaise(
    procedure
    begin
      RotateBitmapGR32(NilBitmap32, 45);
    end,
    EAssertionFailed,
    'Should raise assertion for nil bitmap32');
end;


procedure TTestRotateGr32.TestRotateComposite_NilSource_RaisesAssertion;
var
  NilSource: TBitmap;
  Destination: TBitmap;
begin
  NilSource:= NIL;
  Destination:= TBitmap.Create;
  TRY
    Destination.Width:= 100;
    Destination.Height:= 100;

    Assert.WillRaise(
      procedure
      begin
        RotateBitmapGR32(NilSource, Destination, 45, 0, 0);
      end,
      EAssertionFailed,
      'Should raise assertion for nil source');
  FINALLY
    FreeAndNil(Destination);
  END;
end;


procedure TTestRotateGr32.TestRotateComposite_NilDestination_RaisesAssertion;
var
  Source: TBitmap;
  NilDestination: TBitmap;
begin
  Source:= TBitmap.Create;
  NilDestination:= NIL;
  TRY
    Source.Width:= 50;
    Source.Height:= 50;
    Source.PixelFormat:= pf24bit;

    Assert.WillRaise(
      procedure
      begin
        RotateBitmapGR32(Source, NilDestination, 45, 0, 0);
      end,
      EAssertionFailed,
      'Should raise assertion for nil destination');
  FINALLY
    FreeAndNil(Source);
  END;
end;


{ Edge cases }

procedure TTestRotateGr32.TestRotateBitmap_SmallImage;
var
  SmallBmp: TBitmap;
begin
  SmallBmp:= TBitmap.Create;
  TRY
    SmallBmp.Width:= 5;
    SmallBmp.Height:= 5;
    SmallBmp.PixelFormat:= pf24bit;

    Assert.WillNotRaise(
      procedure
      begin
        RotateBitmapGR32(SmallBmp, 45);
      end);

    Assert.IsTrue(SmallBmp.Width > 0, 'Small bitmap should remain valid');
    Assert.IsTrue(SmallBmp.Height > 0, 'Small bitmap should remain valid');
  FINALLY
    FreeAndNil(SmallBmp);
  END;
end;


procedure TTestRotateGr32.TestRotateBitmap_SquareImage;
var
  SquareBmp: TBitmap;
  OrigSize: Integer;
begin
  SquareBmp:= TBitmap.Create;
  TRY
    OrigSize:= 100;
    SquareBmp.Width:= OrigSize;
    SquareBmp.Height:= OrigSize;
    SquareBmp.PixelFormat:= pf24bit;

    { 90 degree rotation of square should preserve dimensions with AdjustSize=True }
    RotateBitmapGR32(SquareBmp, 90, True);

    Assert.AreEqual(OrigSize, SquareBmp.Width, 'Square width should be preserved at 90 degrees');
    Assert.AreEqual(OrigSize, SquareBmp.Height, 'Square height should be preserved at 90 degrees');
  FINALLY
    FreeAndNil(SquareBmp);
  END;
end;


procedure TTestRotateGr32.TestRotateBitmap_NegativeAngle;
begin
  Assert.WillNotRaise(
    procedure
    begin
      RotateBitmapGR32(FBitmap, -45);
    end);

  Assert.IsTrue(FBitmap.Width > 0, 'Bitmap should be valid after negative angle rotation');
end;


procedure TTestRotateGr32.TestRotateBitmap_LargeAngle;
begin
  { Test angle > 360 degrees }
  Assert.WillNotRaise(
    procedure
    begin
      RotateBitmapGR32(FBitmap, 405);  { 405 = 360 + 45 }
    end);

  Assert.IsTrue(FBitmap.Width > 0, 'Bitmap should be valid after large angle rotation');
end;


{ Different resampler kernels }

procedure TTestRotateGr32.TestRotateBitmap_DifferentKernels;
begin
  { Test with different kernel types }
  Assert.WillNotRaise(
    procedure
    begin
      RotateBitmapGR32(FBitmap, 30, True, clPurple, False, BoxKernel);
    end);

  { Reset for next test }
  FBitmap.Width:= 100;
  FBitmap.Height:= 80;
  FillBitmapWithColor(FBitmap, clWhite);

  Assert.WillNotRaise(
    procedure
    begin
      RotateBitmapGR32(FBitmap, 30, True, clPurple, False, LanczosKernel);
    end);

  { Reset for next test }
  FBitmap.Width:= 100;
  FBitmap.Height:= 80;
  FillBitmapWithColor(FBitmap, clWhite);

  Assert.WillNotRaise(
    procedure
    begin
      RotateBitmapGR32(FBitmap, 30, True, clPurple, False, HermiteKernel);
    end);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestRotateGr32);

end.
