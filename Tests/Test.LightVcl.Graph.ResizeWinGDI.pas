unit Test.LightVcl.Graph.ResizeWinGDI;

{=============================================================================================================
   Unit tests for LightVcl.Graph.ResizeWinGDI.pas
   Tests GDI+-based image resizing function.

   Note: These tests require GDI+ which is available on Windows XP and later.
   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Vcl.Graphics;

type
  [TestFixture]
  TTestResizeWinGDI = class
  private
    FSource: TBitmap;
    FDest: TBitmap;
    procedure CreateTestBitmap(Width, Height: Integer);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Parameter Validation Tests }
    [Test]
    procedure TestResizeBitmapGDI_NilSource;

    [Test]
    procedure TestResizeBitmapGDI_NilDest;

    [Test]
    procedure TestResizeBitmapGDI_InvalidWidth;

    [Test]
    procedure TestResizeBitmapGDI_InvalidHeight;

    { Basic Functionality Tests }
    [Test]
    procedure TestResizeBitmapGDI_BasicCall;

    [Test]
    procedure TestResizeBitmapGDI_ResizeDown;

    [Test]
    procedure TestResizeBitmapGDI_ResizeUp;

    [Test]
    procedure TestResizeBitmapGDI_DestHasCorrectDimensions;

    [Test]
    procedure TestResizeBitmapGDI_SourceUnchanged;

    [Test]
    procedure TestResizeBitmapGDI_NonUniformScale;
  end;

implementation

uses
  LightVcl.Graph.ResizeWinGDI;


procedure TTestResizeWinGDI.Setup;
begin
  FSource:= TBitmap.Create;
  FSource.PixelFormat:= pf24bit;
  FSource.Width:= 200;
  FSource.Height:= 100;
  FSource.Canvas.Brush.Color:= clWhite;
  FSource.Canvas.FillRect(Rect(0, 0, FSource.Width, FSource.Height));

  FDest:= TBitmap.Create;
end;


procedure TTestResizeWinGDI.TearDown;
begin
  FreeAndNil(FSource);
  FreeAndNil(FDest);
end;


procedure TTestResizeWinGDI.CreateTestBitmap(Width, Height: Integer);
begin
  FreeAndNil(FSource);
  FSource:= TBitmap.Create;
  FSource.PixelFormat:= pf24bit;
  FSource.Width:= Width;
  FSource.Height:= Height;
  FSource.Canvas.Brush.Color:= clWhite;
  FSource.Canvas.FillRect(Rect(0, 0, Width, Height));
end;


{ Parameter Validation Tests }

procedure TTestResizeWinGDI.TestResizeBitmapGDI_NilSource;
begin
  Assert.WillRaise(
    procedure
    begin
      ResizeBitmapGDI(NIL, FDest, 100, 100);
    end,
    EAssertionFailed);
end;


procedure TTestResizeWinGDI.TestResizeBitmapGDI_NilDest;
begin
  Assert.WillRaise(
    procedure
    begin
      ResizeBitmapGDI(FSource, NIL, 100, 100);
    end,
    EAssertionFailed);
end;


procedure TTestResizeWinGDI.TestResizeBitmapGDI_InvalidWidth;
begin
  Assert.WillRaise(
    procedure
    begin
      ResizeBitmapGDI(FSource, FDest, 0, 100);
    end,
    EAssertionFailed);
end;


procedure TTestResizeWinGDI.TestResizeBitmapGDI_InvalidHeight;
begin
  Assert.WillRaise(
    procedure
    begin
      ResizeBitmapGDI(FSource, FDest, 100, -1);
    end,
    EAssertionFailed);
end;


{ Basic Functionality Tests }

procedure TTestResizeWinGDI.TestResizeBitmapGDI_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      ResizeBitmapGDI(FSource, FDest, 100, 50);
    end);
end;


procedure TTestResizeWinGDI.TestResizeBitmapGDI_ResizeDown;
begin
  CreateTestBitmap(400, 300);

  ResizeBitmapGDI(FSource, FDest, 200, 150);

  Assert.AreEqual(200, FDest.Width, 'Dest width should be 200');
  Assert.AreEqual(150, FDest.Height, 'Dest height should be 150');
end;


procedure TTestResizeWinGDI.TestResizeBitmapGDI_ResizeUp;
begin
  CreateTestBitmap(100, 100);

  ResizeBitmapGDI(FSource, FDest, 300, 300);

  Assert.AreEqual(300, FDest.Width, 'Dest width should be 300');
  Assert.AreEqual(300, FDest.Height, 'Dest height should be 300');
end;


procedure TTestResizeWinGDI.TestResizeBitmapGDI_DestHasCorrectDimensions;
begin
  CreateTestBitmap(500, 400);

  ResizeBitmapGDI(FSource, FDest, 250, 200);

  Assert.AreEqual(250, FDest.Width, 'Dest should have exact target width');
  Assert.AreEqual(200, FDest.Height, 'Dest should have exact target height');
end;


procedure TTestResizeWinGDI.TestResizeBitmapGDI_SourceUnchanged;
var
  OrigWidth, OrigHeight: Integer;
begin
  CreateTestBitmap(300, 200);
  OrigWidth:= FSource.Width;
  OrigHeight:= FSource.Height;

  ResizeBitmapGDI(FSource, FDest, 150, 100);

  Assert.AreEqual(OrigWidth, FSource.Width, 'Source width should be unchanged');
  Assert.AreEqual(OrigHeight, FSource.Height, 'Source height should be unchanged');
end;


procedure TTestResizeWinGDI.TestResizeBitmapGDI_NonUniformScale;
begin
  CreateTestBitmap(200, 200);

  ResizeBitmapGDI(FSource, FDest, 400, 100);

  Assert.AreEqual(400, FDest.Width, 'Width should be 400');
  Assert.AreEqual(100, FDest.Height, 'Height should be 100');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestResizeWinGDI);

end.
