unit Test.LightVcl.Graph.ResizeWinWIC;

{=============================================================================================================
   Unit tests for LightVcl.Graph.ResizeWinWIC.pas
   Tests WIC-based image resizing function ResizeBitmapWic.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Vcl.Graphics;

type
  [TestFixture]
  TTestGraphResizeWinWIC = class
  private
    FBitmap: TBitmap;
    procedure CreateTestBitmap(Width, Height: Integer; PixelFormat: TPixelFormat = pf24bit);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Parameter Validation Tests }
    [Test]
    procedure TestResizeBitmapWic_NilBitmap;

    [Test]
    procedure TestResizeBitmapWic_ZeroWidth;

    [Test]
    procedure TestResizeBitmapWic_NegativeWidth;

    [Test]
    procedure TestResizeBitmapWic_ZeroHeight;

    [Test]
    procedure TestResizeBitmapWic_NegativeHeight;

    { Basic Functionality Tests }
    [Test]
    procedure TestResizeBitmapWic_BasicCall;

    [Test]
    procedure TestResizeBitmapWic_ResizesDown;

    [Test]
    procedure TestResizeBitmapWic_ResizesUp;

    [Test]
    procedure TestResizeBitmapWic_ExactDimensions;

    [Test]
    procedure TestResizeBitmapWic_SameSize;

    { Pixel Format Tests }
    [Test]
    procedure TestResizeBitmapWic_ConvertsTo32bit;

    [Test]
    procedure TestResizeBitmapWic_Preserves32bit;

    { Aspect Ratio Tests }
    [Test]
    procedure TestResizeBitmapWic_NonProportional;

    [Test]
    procedure TestResizeBitmapWic_LandscapeToSquare;

    [Test]
    procedure TestResizeBitmapWic_PortraitToSquare;

    { Edge Case Tests }
    [Test]
    procedure TestResizeBitmapWic_VerySmallImage;

    [Test]
    procedure TestResizeBitmapWic_ResizeToOne;
  end;

implementation

uses
  LightVcl.Graph.ResizeWinWIC;


procedure TTestGraphResizeWinWIC.Setup;
begin
  FBitmap:= TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Width:= 200;
  FBitmap.Height:= 100;
  FBitmap.Canvas.Brush.Color:= clWhite;
  FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
end;


procedure TTestGraphResizeWinWIC.TearDown;
begin
  FreeAndNil(FBitmap);
end;


procedure TTestGraphResizeWinWIC.CreateTestBitmap(Width, Height: Integer; PixelFormat: TPixelFormat = pf24bit);
begin
  FreeAndNil(FBitmap);
  FBitmap:= TBitmap.Create;
  FBitmap.PixelFormat:= PixelFormat;
  FBitmap.Width:= Width;
  FBitmap.Height:= Height;
  FBitmap.Canvas.Brush.Color:= clWhite;
  FBitmap.Canvas.FillRect(Rect(0, 0, Width, Height));
end;


{ Parameter Validation Tests }

procedure TTestGraphResizeWinWIC.TestResizeBitmapWic_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      ResizeBitmapWic(NIL, 100, 100);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResizeWinWIC.TestResizeBitmapWic_ZeroWidth;
begin
  Assert.WillRaise(
    procedure
    begin
      ResizeBitmapWic(FBitmap, 0, 100);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResizeWinWIC.TestResizeBitmapWic_NegativeWidth;
begin
  Assert.WillRaise(
    procedure
    begin
      ResizeBitmapWic(FBitmap, -10, 100);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResizeWinWIC.TestResizeBitmapWic_ZeroHeight;
begin
  Assert.WillRaise(
    procedure
    begin
      ResizeBitmapWic(FBitmap, 100, 0);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResizeWinWIC.TestResizeBitmapWic_NegativeHeight;
begin
  Assert.WillRaise(
    procedure
    begin
      ResizeBitmapWic(FBitmap, 100, -10);
    end,
    EAssertionFailed);
end;


{ Basic Functionality Tests }

procedure TTestGraphResizeWinWIC.TestResizeBitmapWic_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      ResizeBitmapWic(FBitmap, 100, 50);
    end);
end;


procedure TTestGraphResizeWinWIC.TestResizeBitmapWic_ResizesDown;
begin
  CreateTestBitmap(400, 300);

  ResizeBitmapWic(FBitmap, 200, 150);

  Assert.AreEqual(200, FBitmap.Width, 'Width should be 200');
  Assert.AreEqual(150, FBitmap.Height, 'Height should be 150');
end;


procedure TTestGraphResizeWinWIC.TestResizeBitmapWic_ResizesUp;
begin
  CreateTestBitmap(50, 50);

  ResizeBitmapWic(FBitmap, 200, 200);

  Assert.AreEqual(200, FBitmap.Width, 'Width should be 200');
  Assert.AreEqual(200, FBitmap.Height, 'Height should be 200');
end;


procedure TTestGraphResizeWinWIC.TestResizeBitmapWic_ExactDimensions;
begin
  CreateTestBitmap(100, 100);

  ResizeBitmapWic(FBitmap, 75, 50);

  Assert.AreEqual(75, FBitmap.Width, 'Width should be exactly 75');
  Assert.AreEqual(50, FBitmap.Height, 'Height should be exactly 50');
end;


procedure TTestGraphResizeWinWIC.TestResizeBitmapWic_SameSize;
begin
  CreateTestBitmap(100, 100);

  ResizeBitmapWic(FBitmap, 100, 100);

  Assert.AreEqual(100, FBitmap.Width, 'Width should remain 100');
  Assert.AreEqual(100, FBitmap.Height, 'Height should remain 100');
end;


{ Pixel Format Tests }

procedure TTestGraphResizeWinWIC.TestResizeBitmapWic_ConvertsTo32bit;
begin
  CreateTestBitmap(100, 100, pf24bit);

  ResizeBitmapWic(FBitmap, 50, 50);

  Assert.AreEqual(pf32bit, FBitmap.PixelFormat, 'Should convert to pf32bit');
end;


procedure TTestGraphResizeWinWIC.TestResizeBitmapWic_Preserves32bit;
begin
  CreateTestBitmap(100, 100, pf32bit);

  ResizeBitmapWic(FBitmap, 50, 50);

  Assert.AreEqual(pf32bit, FBitmap.PixelFormat, 'Should remain pf32bit');
end;


{ Aspect Ratio Tests }

procedure TTestGraphResizeWinWIC.TestResizeBitmapWic_NonProportional;
begin
  { WIC resize does not preserve aspect ratio - it resizes to exact dimensions }
  CreateTestBitmap(200, 100);  { 2:1 ratio }

  ResizeBitmapWic(FBitmap, 100, 100);  { 1:1 ratio }

  Assert.AreEqual(100, FBitmap.Width, 'Width should be 100');
  Assert.AreEqual(100, FBitmap.Height, 'Height should be 100');
end;


procedure TTestGraphResizeWinWIC.TestResizeBitmapWic_LandscapeToSquare;
begin
  CreateTestBitmap(400, 200);

  ResizeBitmapWic(FBitmap, 100, 100);

  Assert.AreEqual(100, FBitmap.Width, 'Width should be 100');
  Assert.AreEqual(100, FBitmap.Height, 'Height should be 100');
end;


procedure TTestGraphResizeWinWIC.TestResizeBitmapWic_PortraitToSquare;
begin
  CreateTestBitmap(200, 400);

  ResizeBitmapWic(FBitmap, 100, 100);

  Assert.AreEqual(100, FBitmap.Width, 'Width should be 100');
  Assert.AreEqual(100, FBitmap.Height, 'Height should be 100');
end;


{ Edge Case Tests }

procedure TTestGraphResizeWinWIC.TestResizeBitmapWic_VerySmallImage;
begin
  CreateTestBitmap(2, 2);

  ResizeBitmapWic(FBitmap, 100, 100);

  Assert.AreEqual(100, FBitmap.Width, 'Width should be 100');
  Assert.AreEqual(100, FBitmap.Height, 'Height should be 100');
end;


procedure TTestGraphResizeWinWIC.TestResizeBitmapWic_ResizeToOne;
begin
  CreateTestBitmap(100, 100);

  ResizeBitmapWic(FBitmap, 1, 1);

  Assert.AreEqual(1, FBitmap.Width, 'Width should be 1');
  Assert.AreEqual(1, FBitmap.Height, 'Height should be 1');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestGraphResizeWinWIC);

end.
