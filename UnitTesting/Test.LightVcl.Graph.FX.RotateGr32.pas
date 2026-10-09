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
    procedure PaintRedBlock(BMP: TBitmap);
    procedure AssertColorNear(Expected, Actual: TColor; const Msg: string);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { TBitmap overload - Basic functionality }
    [Test]
    procedure TestRotateBitmap_BasicCall;

    [Test]
    procedure TestRotateBitmap_LeavesNoCanvasDC;

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


{ A 20x20 red block at X 10..29, Y 10..29: away from the border, which RotateBitmapGR32 makes transparent.
  Rotating the 100x80 FBitmap by 90 degrees clockwise moves its center (20, 20) to (79-20, 20) = (59, 20);
  counter-clockwise moves it to (20, 99-20) = (20, 79). }
procedure TTestRotateGr32.PaintRedBlock(BMP: TBitmap);
begin
  BMP.Canvas.Brush.Color:= clRed;
  BMP.Canvas.FillRect(Rect(10, 10, 30, 30));
end;


{ Resampling blurs edges a little, so each channel may differ by up to 40 }
procedure TTestRotateGr32.AssertColorNear(Expected, Actual: TColor; const Msg: string);
VAR E, A: Integer;
begin
  E:= ColorToRGB(Expected);
  A:= ColorToRGB(Actual);
  Assert.IsTrue((Abs((E and $FF) - (A and $FF)) <= 40)
            AND (Abs(((E shr 8) and $FF) - ((A shr 8) and $FF)) <= 40)
            AND (Abs(((E shr 16) and $FF) - ((A shr 16) and $FF)) <= 40),
    Msg + ' (expected ' + IntToHex(E, 6) + ', got ' + IntToHex(A, 6) + ')');
end;


{ TBitmap overload - Basic functionality }

{ Thread safety: GR32 copies to and from a TBitmap through its canvas DC. A DC left on the canvas is what the main
  thread's Vcl.Graphics.FreeMemoryContexts races with when a worker frees the bitmap (FastMM, 2026-10-05). }
procedure TTestRotateGr32.TestRotateBitmap_LeavesNoCanvasDC;
VAR BMP: TBitmap;
begin
  BMP:= TBitmap.Create;
  TRY
    BMP.PixelFormat:= pf24bit;
    BMP.SetSize(60, 40);
    RotateBitmapGR32(BMP, 90);
    Assert.AreEqual(40, BMP.Width, 'Rotated by 90 degrees: width and height swap');
    Assert.IsFalse(BMP.Canvas.HandleAllocated, 'RotateBitmapGR32 must not leave a DC on the canvas');
  FINALLY
    FreeAndNil(BMP);
  END;
end;


{ Rotating the 100x80 FBitmap by 45 degrees gives a 127x127 bitmap: (100 + 80) * 0.7071 = 127.28 -> 127.
  Clockwise by 45 degrees, the red block center (20, 20), at offset (-30, -20) from the source center (50, 40), moves to
  offset (-30 * 0.7071 + 20 * 0.7071, -30 * 0.7071 - 20 * 0.7071) = (-7.1, -35.4) from the new center (63.5, 63.5): (56, 28).
  Counter-clockwise it would land at offset (-35.4, 7.1): (28, 70). }
procedure TTestRotateGr32.TestRotateBitmap_BasicCall;
begin
  PaintRedBlock(FBitmap);

  RotateBitmapGR32(FBitmap, 45);

  Assert.AreEqual(127, FBitmap.Width,  'Width after 45 degrees');
  Assert.AreEqual(127, FBitmap.Height, 'Height after 45 degrees');
  AssertColorNear(clRed,    FBitmap.Canvas.Pixels[56, 28], 'Clockwise: the red block lands at (56, 28)');
  AssertColorNear(clWhite,  FBitmap.Canvas.Pixels[28, 70], 'The counter-clockwise position (28, 70) stays white');
  AssertColorNear(clPurple, FBitmap.Canvas.Pixels[0, 0],   'The corner outside the rotated image gets BkColor (default clPurple)');
end;


procedure TTestRotateGr32.TestRotateBitmap_ZeroAngle;
var
  OrigWidth, OrigHeight: Integer;
begin
  PaintRedBlock(FBitmap);
  OrigWidth:= FBitmap.Width;
  OrigHeight:= FBitmap.Height;

  RotateBitmapGR32(FBitmap, 0);

  { Zero rotation should preserve dimensions (with AdjustSize=True default) }
  Assert.AreEqual(OrigWidth, FBitmap.Width, 'Width should be preserved with 0 angle');
  Assert.AreEqual(OrigHeight, FBitmap.Height, 'Height should be preserved with 0 angle');
  { The block X 10..29, Y 10..29 stays where it is. A left-right mirror would move it to X 70..89, a top-bottom
    mirror to Y 50..69, a 90-degree turn to (59, 20). }
  AssertColorNear(clRed,   FBitmap.Canvas.Pixels[20, 20], 'The red block stays at (20, 20)');
  AssertColorNear(clWhite, FBitmap.Canvas.Pixels[79, 20], 'Not mirrored left-right');
  AssertColorNear(clWhite, FBitmap.Canvas.Pixels[20, 59], 'Not mirrored top-bottom');
  AssertColorNear(clWhite, FBitmap.Canvas.Pixels[59, 20], 'Not turned by 90 degrees');
  AssertColorNear(clWhite, FBitmap.Canvas.Pixels[50, 40], 'The center stays white');
end;


procedure TTestRotateGr32.TestRotateBitmap_90Degrees;
begin
  PaintRedBlock(FBitmap);
  RotateBitmapGR32(FBitmap, 90, True);

  Assert.AreEqual(80,  FBitmap.Width,  '90 degrees: width and height swap');
  Assert.AreEqual(100, FBitmap.Height, '90 degrees: width and height swap');
  { Positive angle = clockwise (the header of RotateBitmapGR32) }
  AssertColorNear(clRed,   FBitmap.Canvas.Pixels[59, 20], 'Clockwise: the red block lands at (59, 20)');
  AssertColorNear(clWhite, FBitmap.Canvas.Pixels[20, 79], 'Clockwise: (20, 79) stays white');
  AssertColorNear(clWhite, FBitmap.Canvas.Pixels[20, 20], 'The red block must leave (20, 20)');
end;


procedure TTestRotateGr32.TestRotateBitmap_180Degrees;
var
  OrigWidth, OrigHeight: Integer;
begin
  PaintRedBlock(FBitmap);
  OrigWidth:= FBitmap.Width;
  OrigHeight:= FBitmap.Height;

  RotateBitmapGR32(FBitmap, 180, True);

  { 180 degree rotation should preserve dimensions }
  Assert.AreEqual(OrigWidth, FBitmap.Width, 'Width should be preserved with 180 angle');
  Assert.AreEqual(OrigHeight, FBitmap.Height, 'Height should be preserved with 180 angle');
  { 180 degrees maps (X, Y) to (100 - X, 80 - Y): the block X 10..29, Y 10..29 moves to X 70..89, Y 50..69 }
  AssertColorNear(clRed,   FBitmap.Canvas.Pixels[79, 59], '180 degrees: the red block lands at (79, 59)');
  AssertColorNear(clWhite, FBitmap.Canvas.Pixels[20, 20], 'The red block must leave (20, 20)');
  AssertColorNear(clWhite, FBitmap.Canvas.Pixels[79, 20], 'A left-right mirror would put the block at (79, 20)');
  AssertColorNear(clWhite, FBitmap.Canvas.Pixels[20, 59], 'A top-bottom mirror would put the block at (20, 59)');
end;


procedure TTestRotateGr32.TestRotateBitmap_270Degrees;
begin
  PaintRedBlock(FBitmap);
  RotateBitmapGR32(FBitmap, 270, True);

  Assert.AreEqual(80,  FBitmap.Width,  '270 degrees: width and height swap');
  Assert.AreEqual(100, FBitmap.Height, '270 degrees: width and height swap');
  { 270 clockwise = 90 counter-clockwise }
  AssertColorNear(clRed,   FBitmap.Canvas.Pixels[20, 79], '270 degrees: the red block lands at (20, 79)');
  AssertColorNear(clWhite, FBitmap.Canvas.Pixels[59, 20], '270 degrees: (59, 20) stays white');
end;


procedure TTestRotateGr32.TestRotateBitmap_ArbitraryAngle;
begin
  RotateBitmapGR32(FBitmap, 37.5, True);

  { Bounding box of a 100x80 rectangle rotated by 37.5 degrees:
    W = 100*cos + 80*sin = 79.34 + 48.70 = 128.04 -> 128
    H = 100*sin + 80*cos = 60.88 + 63.47 = 124.35 -> 124 }
  Assert.AreEqual(128, FBitmap.Width,  'Width of the rotated bounding box');
  Assert.AreEqual(124, FBitmap.Height, 'Height of the rotated bounding box');
  AssertColorNear(clPurple, FBitmap.Canvas.Pixels[0, 0],   'The corner outside the rotated image gets BkColor (default clPurple)');
  AssertColorNear(clWhite,  FBitmap.Canvas.Pixels[64, 62], 'The center keeps the white of the source');
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
  { Start from a transparent bitmap, so the routine must really clear the flag. When GR32 copies a transparent TBitmap,
    it makes the pixels of TransparentColor fully transparent (GR32.ImageFormats.TBitmap.pas,
    TImageFormatAdapterTBitmap.AssignFrom), so TransparentColor is a color the white bitmap does not hold. }
  FBitmap.TransparentColor:= clFuchsia;
  FBitmap.Transparent:= TRUE;

  RotateBitmapGR32(FBitmap, 45, True, clYellow, False);

  Assert.IsFalse(FBitmap.Transparent, 'Bitmap should not be transparent when Transparent=False');
  AssertColorNear(clYellow, FBitmap.Canvas.Pixels[0, 0],   'The corner outside the rotated image gets BkColor');
  AssertColorNear(clWhite,  FBitmap.Canvas.Pixels[63, 63], 'The center keeps the white of the source');
end;


{ TBitmap32 overload - Basic functionality }

{ Same geometry as TestRotateBitmap_BasicCall: 127x127, the red block lands at (56, 28) }
procedure TTestRotateGr32.TestRotateBitmap32_BasicCall;
begin
  FBitmap32.FillRectS(10, 10, 30, 30, clRed32);   { Same block as PaintRedBlock }

  RotateBitmapGR32(FBitmap32, 45);

  Assert.AreEqual(127, FBitmap32.Width,  'Width after 45 degrees');
  Assert.AreEqual(127, FBitmap32.Height, 'Height after 45 degrees');
  AssertColorNear(clRed,    WinColor(FBitmap32.Pixel[56, 28]), 'Clockwise: the red block lands at (56, 28)');
  AssertColorNear(clWhite,  WinColor(FBitmap32.Pixel[28, 70]), 'The counter-clockwise position (28, 70) stays white');
  AssertColorNear(clPurple, WinColor(FBitmap32.Pixel[0, 0]),   'The corner outside the rotated image gets BkColor (default clPurple)');
end;


procedure TTestRotateGr32.TestRotateBitmap32_ZeroAngle;
var
  OrigWidth, OrigHeight: Integer;
begin
  FBitmap32.FillRectS(10, 10, 30, 30, clRed32);   { Same block as PaintRedBlock }
  OrigWidth:= FBitmap32.Width;
  OrigHeight:= FBitmap32.Height;

  RotateBitmapGR32(FBitmap32, 0);

  Assert.AreEqual(OrigWidth, FBitmap32.Width, 'Width should be preserved with 0 angle');
  Assert.AreEqual(OrigHeight, FBitmap32.Height, 'Height should be preserved with 0 angle');
  AssertColorNear(clRed,   WinColor(FBitmap32.Pixel[20, 20]), 'The red block stays at (20, 20)');
  AssertColorNear(clWhite, WinColor(FBitmap32.Pixel[79, 20]), 'Not mirrored left-right');
  AssertColorNear(clWhite, WinColor(FBitmap32.Pixel[20, 59]), 'Not mirrored top-bottom');
  AssertColorNear(clWhite, WinColor(FBitmap32.Pixel[59, 20]), 'Not turned by 90 degrees');
end;


procedure TTestRotateGr32.TestRotateBitmap32_90Degrees;
begin
  FBitmap32.FillRectS(10, 10, 30, 30, clRed32);   { Same block as PaintRedBlock }
  RotateBitmapGR32(FBitmap32, 90, True);

  Assert.AreEqual(80,  FBitmap32.Width,  '90 degrees: width and height swap');
  Assert.AreEqual(100, FBitmap32.Height, '90 degrees: width and height swap');
  AssertColorNear(clRed,   WinColor(FBitmap32.Pixel[59, 20]), 'Clockwise: the red block lands at (59, 20)');
  AssertColorNear(clWhite, WinColor(FBitmap32.Pixel[20, 79]), 'Clockwise: (20, 79) stays white');
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

    RotateBitmapGR32(Source, Destination, 30, 50, 50);

    { The rotated 50x50 square has a 68x68 bounding box (50 * (cos 30 + sin 30) = 68.3), drawn at (50, 50) }
    Assert.AreEqual(200, Destination.Width,  'Destination width should be preserved');
    Assert.AreEqual(200, Destination.Height, 'Destination height should be preserved');
    AssertColorNear(clRed,  Destination.Canvas.Pixels[84, 84],   'The center of the rotated source is red');
    AssertColorNear(clBlue, Destination.Canvas.Pixels[10, 10],   'Above-left of (50, 50) the destination stays blue');
    AssertColorNear(clBlue, Destination.Canvas.Pixels[190, 190], 'Far from the drawn square the destination stays blue');
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

    { The 40x40 square rotated by 45 degrees is a diamond with a 56x56 bounding box, drawn at (10, 10): center (38, 38) }
    AssertColorNear(clGreen, Destination.Canvas.Pixels[38, 38],   'The center of the diamond is green');
    AssertColorNear(clWhite, Destination.Canvas.Pixels[13, 13],   'The corner of the bounding box lies outside the diamond');
    AssertColorNear(clWhite, Destination.Canvas.Pixels[140, 140], 'Far from the diamond the destination stays white');
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
    FillBitmapWithColor(Source, clRed);

    Destination.Width:= 200;
    Destination.Height:= 200;
    Destination.PixelFormat:= pf24bit;
    FillBitmapWithColor(Destination, clBlue);

    RotateBitmapGR32(Source, Destination, 0, 0, 0);

    { No rotation: the source is copied unrotated into the top-left 50x50 of the destination }
    AssertColorNear(clRed,  Destination.Canvas.Pixels[25, 25],   'The unrotated source covers (25, 25)');
    AssertColorNear(clBlue, Destination.Canvas.Pixels[25, 100],  'Below the source the destination stays blue');
    AssertColorNear(clBlue, Destination.Canvas.Pixels[100, 25],  'Right of the source the destination stays blue');
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
    FillBitmapWithColor(SmallBmp, clWhite);

    RotateBitmapGR32(SmallBmp, 45);

    { Bounding box at 45 degrees: (5 + 5) * 0.7071 = 7.07 -> 7 }
    Assert.AreEqual(7, SmallBmp.Width,  'Width of a 5x5 bitmap after 45 degrees');
    Assert.AreEqual(7, SmallBmp.Height, 'Height of a 5x5 bitmap after 45 degrees');
    AssertColorNear(clPurple, SmallBmp.Canvas.Pixels[0, 0], 'The corner of the bounding box lies outside the rotated square: BkColor');
    AssertColorNear(clWhite,  SmallBmp.Canvas.Pixels[3, 3], 'The center keeps the white of the source');
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
    FillBitmapWithColor(SquareBmp, clWhite);
    PaintRedBlock(SquareBmp);

    { 90 degree rotation of square should preserve dimensions with AdjustSize=True }
    RotateBitmapGR32(SquareBmp, 90, True);

    Assert.AreEqual(OrigSize, SquareBmp.Width, 'Square width should be preserved at 90 degrees');
    Assert.AreEqual(OrigSize, SquareBmp.Height, 'Square height should be preserved at 90 degrees');
    { Clockwise by 90 degrees maps (X, Y) to (100 - Y, X): the block X 10..29, Y 10..29 moves to X 70..89, Y 10..29.
      Counter-clockwise would move it to X 10..29, Y 70..89. }
    AssertColorNear(clRed,   SquareBmp.Canvas.Pixels[79, 20], 'Clockwise: the red block lands at (79, 20)');
    AssertColorNear(clWhite, SquareBmp.Canvas.Pixels[20, 20], 'The red block must leave (20, 20)');
    AssertColorNear(clWhite, SquareBmp.Canvas.Pixels[20, 79], 'The counter-clockwise position (20, 79) stays white');
  FINALLY
    FreeAndNil(SquareBmp);
  END;
end;


procedure TTestRotateGr32.TestRotateBitmap_NegativeAngle;
begin
  RotateBitmapGR32(FBitmap, -45);

  { Bounding box at 45 degrees: (100 + 80) * 0.7071 = 127.28 -> 127, in both directions }
  Assert.AreEqual(127, FBitmap.Width,  'Width after -45 degrees');
  Assert.AreEqual(127, FBitmap.Height, 'Height after -45 degrees');
  AssertColorNear(clPurple, FBitmap.Canvas.Pixels[0, 0],   'The corner outside the rotated image gets BkColor');
  AssertColorNear(clWhite,  FBitmap.Canvas.Pixels[63, 63], 'The center keeps the white of the source');
end;


procedure TTestRotateGr32.TestRotateBitmap_LargeAngle;
begin
  { Test angle > 360 degrees: 405 = 360 + 45, so the result is that of 45 degrees }
  RotateBitmapGR32(FBitmap, 405);

  Assert.AreEqual(127, FBitmap.Width,  'Width after 405 degrees equals the width after 45');
  Assert.AreEqual(127, FBitmap.Height, 'Height after 405 degrees equals the height after 45');
  AssertColorNear(clPurple, FBitmap.Canvas.Pixels[0, 0],   'The corner outside the rotated image gets BkColor');
  AssertColorNear(clWhite,  FBitmap.Canvas.Pixels[63, 63], 'The center keeps the white of the source');
end;


{ Different resampler kernels }

procedure TTestRotateGr32.TestRotateBitmap_DifferentKernels;
CONST
  Kernels: array[0..2] of Integer = (BoxKernel, LanczosKernel, HermiteKernel);
  KernelNames: array[0..2] of string = ('BoxKernel', 'LanczosKernel', 'HermiteKernel');
begin
  { Bounding box at 30 degrees: W = 100*0.866 + 80*0.5 = 126.6 -> 127; H = 100*0.5 + 80*0.866 = 119.3 -> 119 }
  for VAR i:= Low(Kernels) to High(Kernels) do
    begin
      { pf24bit again: the rotation returns a pf32bit bitmap, and a GDI FillRect on pf32bit leaves alpha 0,
        which GR32 then treats as fully transparent (the rotated image came out all BkColor) }
      FBitmap.PixelFormat:= pf24bit;
      FBitmap.SetSize(100, 80);
      FillBitmapWithColor(FBitmap, clWhite);

      RotateBitmapGR32(FBitmap, 30, True, clPurple, False, Kernels[i]);

      Assert.AreEqual(127, FBitmap.Width,  KernelNames[i] + ': width after 30 degrees');
      Assert.AreEqual(119, FBitmap.Height, KernelNames[i] + ': height after 30 degrees');
      AssertColorNear(clPurple, FBitmap.Canvas.Pixels[0, 0],   KernelNames[i] + ': the corner gets BkColor');
      AssertColorNear(clWhite,  FBitmap.Canvas.Pixels[63, 59], KernelNames[i] + ': the center keeps the white of the source');
    end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestRotateGr32);

end.
