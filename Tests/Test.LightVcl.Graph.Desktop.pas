unit Test.LightVcl.Graph.Desktop;

{=============================================================================================================
   Unit tests for LightVcl.Graph.Desktop.pas
   Tests Windows desktop manipulation functions.

   Note: Many functions in this unit interact with the actual Windows desktop.
   These tests focus on verifying function signatures, parameter validation,
   and return values without causing visible changes to the user's desktop.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Types,
  Winapi.Windows,
  Vcl.Forms,
  Vcl.Graphics;

type
  [TestFixture]
  TTestGraphDesktop = class
  private
    FBitmap: TBitmap;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { GetDPI Tests }
    [Test]
    procedure TestGetDPI_ReturnsPositiveValue;

    [Test]
    procedure TestGetDPI_ReturnsReasonableValue;

    { GetDesktopResolutionAPI Tests }
    [Test]
    procedure TestGetDesktopResolutionAPI_ReturnsNonEmpty;

    [Test]
    procedure TestGetDesktopResolutionAPI_HasPositiveDimensions;

    { Handle Functions Tests }
    [Test]
    procedure TestGetShellWindow_ReturnsHandle;

    [Test]
    procedure TestProgManHandle_ReturnsHandle;

    [Test]
    procedure TestGetDesktopHandle_ReturnsHandle;

    { DrawOnWindow Parameter Validation Tests }
    [Test]
    procedure TestDrawOnWindow_NilBitmap_RaisesAssertion;

    [Test]
    procedure TestDrawOnWindow_InvalidHandle_RaisesAssertion;

    [Test]
    procedure TestDrawOnWindowBitBlt_NilBitmap_RaisesAssertion;

    [Test]
    procedure TestDrawOnWindowBitBlt_InvalidHandle_RaisesAssertion;

    { PaintOverIcons Parameter Validation Tests }
    [Test]
    procedure TestPaintOverIcons_NilBitmap_RaisesException;

    { MSWallStyle Enum Tests }
    [Test]
    procedure TestMSWallStyle_EnumValues;
  end;

implementation

uses
  LightVcl.Graph.Desktop;


procedure TTestGraphDesktop.Setup;
begin
  FBitmap:= TBitmap.Create;
  FBitmap.Width:= 100;
  FBitmap.Height:= 100;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Canvas.Brush.Color:= clRed;
  FBitmap.Canvas.FillRect(Rect(0, 0, 100, 100));
end;


procedure TTestGraphDesktop.TearDown;
begin
  FreeAndNil(FBitmap);
end;


{ GetDPI Tests }

procedure TTestGraphDesktop.TestGetDPI_ReturnsPositiveValue;
var
  DPI: Integer;
begin
  DPI:= GetDPI;
  Assert.IsTrue(DPI > 0, 'DPI should be positive');
end;


procedure TTestGraphDesktop.TestGetDPI_ReturnsReasonableValue;
var
  DPI: Integer;
begin
  { Standard DPI is 96, high DPI monitors typically use 120, 144, 168, or 192 }
  DPI:= GetDPI;
  Assert.IsTrue((DPI >= 72) and (DPI <= 480), 'DPI should be within reasonable range (72-480)');
end;


{ GetDesktopResolutionAPI Tests }

procedure TTestGraphDesktop.TestGetDesktopResolutionAPI_ReturnsNonEmpty;
var
  R: TRect;
begin
  R:= GetDesktopResolutionAPI;
  Assert.IsFalse(R.IsEmpty, 'Desktop resolution rect should not be empty');
end;


procedure TTestGraphDesktop.TestGetDesktopResolutionAPI_HasPositiveDimensions;
var
  R: TRect;
begin
  R:= GetDesktopResolutionAPI;
  Assert.IsTrue(R.Width > 0, 'Desktop width should be positive');
  Assert.IsTrue(R.Height > 0, 'Desktop height should be positive');
end;


{ Handle Functions Tests }

procedure TTestGraphDesktop.TestGetShellWindow_ReturnsHandle;
var
  Handle: HWND;
begin
  Handle:= GetShellWindow;
  { Shell window should exist on any Windows system with Explorer running }
  Assert.IsTrue(Handle <> 0, 'GetShellWindow should return a valid handle when Explorer is running');
end;


procedure TTestGraphDesktop.TestProgManHandle_ReturnsHandle;
var
  Handle: HWND;
begin
  Handle:= ProgManHandle;
  { Program Manager window should exist on Windows }
  Assert.IsTrue(Handle <> 0, 'ProgManHandle should return a valid handle');
end;


procedure TTestGraphDesktop.TestGetDesktopHandle_ReturnsHandle;
var
  Handle: HWND;
begin
  Handle:= GetDesktopHandle;
  { Desktop handle should be obtainable. May be 0 if desktop is in an unusual state }
  { This is more of a smoke test to ensure the function doesn't crash }
  Assert.WillNotRaise(
    procedure
    begin
      Handle:= GetDesktopHandle;
    end,
    'GetDesktopHandle should not raise an exception');
end;


{ DrawOnWindow Parameter Validation Tests }

procedure TTestGraphDesktop.TestDrawOnWindow_NilBitmap_RaisesAssertion;
begin
  Assert.WillRaise(
    procedure
    begin
      DrawOnWindow(GetDesktopWindow, 0, 0, NIL);
    end,
    EAssertionFailed,
    'DrawOnWindow should raise assertion for nil bitmap');
end;


procedure TTestGraphDesktop.TestDrawOnWindow_InvalidHandle_RaisesAssertion;
begin
  Assert.WillRaise(
    procedure
    begin
      DrawOnWindow(0, 0, 0, FBitmap);
    end,
    EAssertionFailed,
    'DrawOnWindow should raise assertion for invalid handle');
end;


procedure TTestGraphDesktop.TestDrawOnWindowBitBlt_NilBitmap_RaisesAssertion;
begin
  Assert.WillRaise(
    procedure
    begin
      DrawOnWindowBitBlt(GetDesktopWindow, 0, 0, NIL);
    end,
    EAssertionFailed,
    'DrawOnWindowBitBlt should raise assertion for nil bitmap');
end;


procedure TTestGraphDesktop.TestDrawOnWindowBitBlt_InvalidHandle_RaisesAssertion;
begin
  Assert.WillRaise(
    procedure
    begin
      DrawOnWindowBitBlt(0, 0, 0, FBitmap);
    end,
    EAssertionFailed,
    'DrawOnWindowBitBlt should raise assertion for invalid handle');
end;


{ PaintOverIcons Parameter Validation Tests }

procedure TTestGraphDesktop.TestPaintOverIcons_NilBitmap_RaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      PaintOverIcons(0, 0, NIL);
    end,
    Exception,
    'PaintOverIcons should raise exception for nil bitmap');
end;


{ MSWallStyle Enum Tests }

procedure TTestGraphDesktop.TestMSWallStyle_EnumValues;
begin
  { Verify enum values are distinct }
  Assert.AreNotEqual(Ord(dsStretch), Ord(dsCenter), 'dsStretch and dsCenter should be different');
  Assert.AreNotEqual(Ord(dsStretch), Ord(dsTile), 'dsStretch and dsTile should be different');
  Assert.AreNotEqual(Ord(dsCenter), Ord(dsTile), 'dsCenter and dsTile should be different');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestGraphDesktop);

end.
