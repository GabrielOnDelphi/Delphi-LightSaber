unit Test.LightVcl.Graph.ResizeFMX;

{=============================================================================================================
   Unit tests for LightVcl.Graph.ResizeFMX.pas
   Tests FMX-based image resizing functions.

   Note: These tests require FMX framework. They may fail in pure VCL environments.
   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Types,        { Rect }
  Vcl.Graphics;

type
  [TestFixture]
  TTestGraphResizeFMX = class
  private
    FBitmap: TBitmap;
    procedure CreateTestBitmap(Width, Height: Integer);
    procedure PaintHalves;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { ResizeFMX Tests }
    [Test]
    procedure TestResizeFMX_NilBitmap;

    [Test]
    procedure TestResizeFMX_InvalidWidth;

    [Test]
    procedure TestResizeFMX_InvalidHeight;

    [Test]
    procedure TestResizeFMX_BasicCall;

    [Test]
    procedure TestResizeFMX_ResizesDown;

    [Test]
    procedure TestResizeFMX_ResizesUp;

    [Test]
    procedure TestResizeFMX_ConvertsTo32Bit;

    { ResizeFmxF Tests }
    [Test]
    procedure TestResizeFmxF_NilBitmap;

    [Test]
    procedure TestResizeFmxF_InvalidWidth;

    [Test]
    procedure TestResizeFmxF_InvalidHeight;

    [Test]
    procedure TestResizeFmxF_ReturnsNewBitmap;

    [Test]
    procedure TestResizeFmxF_OriginalUnchanged;

    [Test]
    procedure TestResizeFmxF_ResultHasCorrectDimensions;

    [Test]
    procedure TestResizeFmxF_ResultIs32Bit;
  end;

implementation

uses
  LightVcl.Graph.ResizeFMX;


procedure TTestGraphResizeFMX.Setup;
begin
  FBitmap:= TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Width:= 200;
  FBitmap.Height:= 100;
  FBitmap.Canvas.Brush.Color:= clWhite;
  FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
end;


procedure TTestGraphResizeFMX.TearDown;
begin
  FreeAndNil(FBitmap);
end;


procedure TTestGraphResizeFMX.CreateTestBitmap(Width, Height: Integer);
begin
  FreeAndNil(FBitmap);
  FBitmap:= TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Width:= Width;
  FBitmap.Height:= Height;
  FBitmap.Canvas.Brush.Color:= clWhite;
  FBitmap.Canvas.FillRect(Rect(0, 0, Width, Height));
end;


{ Left half red, right half blue: a scaled image keeps both halves, a crop or a flip does not }
procedure TTestGraphResizeFMX.PaintHalves;
begin
  FBitmap.Canvas.Brush.Color:= clRed;
  FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width DIV 2, FBitmap.Height));
  FBitmap.Canvas.Brush.Color:= clBlue;
  FBitmap.Canvas.FillRect(Rect(FBitmap.Width DIV 2, 0, FBitmap.Width, FBitmap.Height));
end;


function ColorHex(Color: TColor): string;
begin
  Result:= IntToHex(ColorToRGB(Color) and $FFFFFF, 6);
end;


function PixelHex(BMP: TBitmap; X, Y: Integer): string;
begin
  Result:= ColorHex(BMP.Canvas.Pixels[X, Y]);
end;


{ ResizeFMX Tests }

procedure TTestGraphResizeFMX.TestResizeFMX_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      ResizeFMX(NIL, 100, 100);
    end,
    EAssertionFailed,
    'ResizeFMX(NIL, 100, 100) must raise EAssertionFailed');
end;


procedure TTestGraphResizeFMX.TestResizeFMX_InvalidWidth;
begin
  Assert.WillRaise(
    procedure
    begin
      ResizeFMX(FBitmap, 0, 100);
    end,
    EAssertionFailed,
    'ResizeFMX(FBitmap, 0, 100) must raise EAssertionFailed');
end;


procedure TTestGraphResizeFMX.TestResizeFMX_InvalidHeight;
begin
  Assert.WillRaise(
    procedure
    begin
      ResizeFMX(FBitmap, 100, -1);
    end,
    EAssertionFailed,
    'ResizeFMX(FBitmap, 100, -1) must raise EAssertionFailed');
end;


procedure TTestGraphResizeFMX.TestResizeFMX_BasicCall;
begin
  PaintHalves;   { The Setup bitmap is 200x100 }

  ResizeFMX(FBitmap, 100, 50);

  { FMX CreateThumbnail returns exactly the asked size: c:\Delphi\Delphi 13\source\fmx\FMX.Graphics.pas:4620 }
  Assert.AreEqual(100, FBitmap.Width,  'Width must be the requested 100');
  Assert.AreEqual(50,  FBitmap.Height, 'Height must be the requested 50');
  Assert.AreEqual(ColorHex(clRed),  PixelHex(FBitmap, 25, 25), 'The left half must stay red');
  Assert.AreEqual(ColorHex(clBlue), PixelHex(FBitmap, 75, 25), 'The right half must stay blue');
end;


procedure TTestGraphResizeFMX.TestResizeFMX_ResizesDown;
begin
  CreateTestBitmap(400, 300);
  PaintHalves;

  ResizeFMX(FBitmap, 100, 75);

  { FMX CreateThumbnail returns exactly the asked size: c:\Delphi\Delphi 13\source\fmx\FMX.Graphics.pas:4620 }
  Assert.AreEqual(100, FBitmap.Width,  'Width must be the requested 100');
  Assert.AreEqual(75,  FBitmap.Height, 'Height must be the requested 75');
  Assert.AreEqual(ColorHex(clRed),  PixelHex(FBitmap, 25, 37), 'The left half must stay red');
  Assert.AreEqual(ColorHex(clBlue), PixelHex(FBitmap, 75, 37), 'The right half must stay blue');
end;


procedure TTestGraphResizeFMX.TestResizeFMX_ResizesUp;
begin
  CreateTestBitmap(50, 50);

  ResizeFMX(FBitmap, 200, 200);

  { FMX CreateThumbnail creates a bitmap of exactly the requested size and draws the source scaled to fit into it
    (c:\Delphi\Delphi 13\source\fmx\FMX.Graphics.pas, TBitmap.CreateThumbnail) }
  Assert.AreEqual(200, FBitmap.Width,  'Width must be the requested 200');
  Assert.AreEqual(200, FBitmap.Height, 'Height must be the requested 200');
  Assert.AreEqual(IntToHex(Integer(clWhite), 6), IntToHex(Integer(ColorToRGB(FBitmap.Canvas.Pixels[100, 100]) and $FFFFFF), 6),
    'The white source must be scaled up over the whole thumbnail');
end;


procedure TTestGraphResizeFMX.TestResizeFMX_ConvertsTo32Bit;
begin
  FBitmap.PixelFormat:= pf24bit;

  ResizeFMX(FBitmap, 100, 50);

  Assert.AreEqual(pf32bit, FBitmap.PixelFormat, 'Should be converted to pf32bit');
end;


{ ResizeFmxF Tests }

procedure TTestGraphResizeFMX.TestResizeFmxF_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      ResizeFmxF(NIL, 100, 100);
    end,
    EAssertionFailed,
    'ResizeFmxF(NIL, 100, 100) must raise EAssertionFailed');
end;


procedure TTestGraphResizeFMX.TestResizeFmxF_InvalidWidth;
begin
  Assert.WillRaise(
    procedure
    begin
      ResizeFmxF(FBitmap, 0, 100);
    end,
    EAssertionFailed,
    'ResizeFmxF(FBitmap, 0, 100) must raise EAssertionFailed');
end;


procedure TTestGraphResizeFMX.TestResizeFmxF_InvalidHeight;
begin
  Assert.WillRaise(
    procedure
    begin
      ResizeFmxF(FBitmap, 100, 0);
    end,
    EAssertionFailed,
    'ResizeFmxF(FBitmap, 100, 0) must raise EAssertionFailed');
end;


procedure TTestGraphResizeFMX.TestResizeFmxF_ReturnsNewBitmap;
var
  Result: TBitmap;
begin
  Result:= ResizeFmxF(FBitmap, 100, 50);
  TRY
    Assert.IsNotNull(Result, 'Should return a bitmap');
    Assert.AreNotEqual(Pointer(FBitmap), Pointer(Result), 'Should be a different object');
  FINALLY
    FreeAndNil(Result);
  END;
end;


procedure TTestGraphResizeFMX.TestResizeFmxF_OriginalUnchanged;
var
  OrigWidth, OrigHeight: Integer;
  OrigFormat: TPixelFormat;
  ResizedBMP: TBitmap;
begin
  OrigWidth:= FBitmap.Width;
  OrigHeight:= FBitmap.Height;
  OrigFormat:= FBitmap.PixelFormat;

  ResizedBMP:= ResizeFmxF(FBitmap, 50, 25);
  TRY
    Assert.AreEqual(OrigWidth, FBitmap.Width, 'Original width should be unchanged');
    Assert.AreEqual(OrigHeight, FBitmap.Height, 'Original height should be unchanged');
    Assert.AreEqual(OrigFormat, FBitmap.PixelFormat, 'Original pixel format should be unchanged');
  FINALLY
    FreeAndNil(ResizedBMP);
  END;
end;


procedure TTestGraphResizeFMX.TestResizeFmxF_ResultHasCorrectDimensions;
var
  Result: TBitmap;
begin
  CreateTestBitmap(400, 300);
  PaintHalves;

  Result:= ResizeFmxF(FBitmap, 100, 75);
  TRY
    { FMX CreateThumbnail returns exactly the asked size: c:\Delphi\Delphi 13\source\fmx\FMX.Graphics.pas:4620 }
    Assert.AreEqual(100, Result.Width,  'Width must be the requested 100');
    Assert.AreEqual(75,  Result.Height, 'Height must be the requested 75');
    Assert.AreEqual(ColorHex(clRed),  PixelHex(Result, 25, 37), 'The left half must stay red');
    Assert.AreEqual(ColorHex(clBlue), PixelHex(Result, 75, 37), 'The right half must stay blue');
  FINALLY
    FreeAndNil(Result);
  END;
end;


procedure TTestGraphResizeFMX.TestResizeFmxF_ResultIs32Bit;
var
  Result: TBitmap;
begin
  FBitmap.PixelFormat:= pf24bit;

  Result:= ResizeFmxF(FBitmap, 100, 50);
  TRY
    Assert.AreEqual(pf32bit, Result.PixelFormat, 'Result should be pf32bit');
  FINALLY
    FreeAndNil(Result);
  END;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestGraphResizeFMX);

end.
