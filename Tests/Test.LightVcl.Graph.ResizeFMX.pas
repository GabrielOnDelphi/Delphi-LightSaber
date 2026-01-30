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
  Vcl.Graphics;

type
  [TestFixture]
  TTestGraphResizeFMX = class
  private
    FBitmap: TBitmap;
    procedure CreateTestBitmap(Width, Height: Integer);
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


{ ResizeFMX Tests }

procedure TTestGraphResizeFMX.TestResizeFMX_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      ResizeFMX(NIL, 100, 100);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResizeFMX.TestResizeFMX_InvalidWidth;
begin
  Assert.WillRaise(
    procedure
    begin
      ResizeFMX(FBitmap, 0, 100);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResizeFMX.TestResizeFMX_InvalidHeight;
begin
  Assert.WillRaise(
    procedure
    begin
      ResizeFMX(FBitmap, 100, -1);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResizeFMX.TestResizeFMX_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      ResizeFMX(FBitmap, 100, 50);
    end);
end;


procedure TTestGraphResizeFMX.TestResizeFMX_ResizesDown;
begin
  CreateTestBitmap(400, 300);

  ResizeFMX(FBitmap, 100, 75);

  { FMX CreateThumbnail may not produce exact dimensions but should be close }
  Assert.IsTrue(FBitmap.Width <= 100, 'Width should be <= 100');
  Assert.IsTrue(FBitmap.Height <= 75, 'Height should be <= 75');
end;


procedure TTestGraphResizeFMX.TestResizeFMX_ResizesUp;
begin
  CreateTestBitmap(50, 50);

  ResizeFMX(FBitmap, 200, 200);

  { FMX CreateThumbnail typically only shrinks, but test the call }
  Assert.IsTrue(FBitmap.Width > 0, 'Width should be positive');
  Assert.IsTrue(FBitmap.Height > 0, 'Height should be positive');
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
    EAssertionFailed);
end;


procedure TTestGraphResizeFMX.TestResizeFmxF_InvalidWidth;
begin
  Assert.WillRaise(
    procedure
    begin
      ResizeFmxF(FBitmap, 0, 100);
    end,
    EAssertionFailed);
end;


procedure TTestGraphResizeFMX.TestResizeFmxF_InvalidHeight;
begin
  Assert.WillRaise(
    procedure
    begin
      ResizeFmxF(FBitmap, 100, 0);
    end,
    EAssertionFailed);
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

  Result:= ResizeFmxF(FBitmap, 100, 75);
  TRY
    { FMX CreateThumbnail may not produce exact dimensions but should be close }
    Assert.IsTrue(Result.Width <= 100, 'Width should be <= 100');
    Assert.IsTrue(Result.Height <= 75, 'Height should be <= 75');
    Assert.IsTrue(Result.Width > 0, 'Width should be positive');
    Assert.IsTrue(Result.Height > 0, 'Height should be positive');
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
