unit Test.LightVcl.Graph.BkgColor;

{=============================================================================================================
   Unit tests for LightVcl.Graph.BkgColor
   Tests bitmap border effects: fading, color detection, border removal.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  Vcl.Graphics;

type
  [TestFixture]
  TTestBkgColor = class
  private
    FTestBitmap: TBitmap;
    function CreateSolidColorBitmap(Width, Height: Integer; Color: TColor): TBitmap;
    function CreateBitmapWithBlackBorder(Width, Height: Integer; BorderWidth: Integer; InnerColor: TColor): TBitmap;
    function CreateGradientBitmap(Width, Height: Integer): TBitmap;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { LineIsBlack Tests }
    [Test]
    procedure TestLineIsBlack_BlackLine;

    [Test]
    procedure TestLineIsBlack_WhiteLine;

    [Test]
    procedure TestLineIsBlack_NearBlackLine;

    [Test]
    procedure TestLineIsBlack_InvalidRow;

    [Test]
    procedure TestLineIsBlack_NilBitmap;

    { HasBlackBorder Tests }
    [Test]
    procedure TestHasBlackBorder_WithBlackBorder;

    [Test]
    procedure TestHasBlackBorder_WithoutBlackBorder;

    [Test]
    procedure TestHasBlackBorder_NilBitmap;

    { RemoveBorder Tests }
    [Test]
    procedure TestRemoveBorder_ReducesSize;

    [Test]
    procedure TestRemoveBorder_NilBitmap;

    { ApplyBorderRectOut Tests }
    [Test]
    procedure TestApplyBorderRectOut_IncreasesSize;

    [Test]
    procedure TestApplyBorderRectOut_ZeroBorder;

    [Test]
    procedure TestApplyBorderRectOut_NilBitmap;

    { ApplyBorderRectIn Tests }
    [Test]
    procedure TestApplyBorderRectIn_SameSize;

    [Test]
    procedure TestApplyBorderRectIn_ZeroBorder;

    [Test]
    procedure TestApplyBorderRectIn_NilBitmap;

    { GetBorderDominantColor Tests }
    [Test]
    procedure TestGetBorderDominantColor_SolidColor;

    [Test]
    procedure TestGetBorderDominantColor_TooSmallImage;

    [Test]
    procedure TestGetBorderDominantColor_NilBitmap;

    [Test]
    procedure TestGetBorderDominantColor_AllBorders;
  end;


implementation

uses
  LightVcl.Graph.BkgColor,
  LightVcl.Graph.BkgColorParams;


{ Helper Methods }

function TTestBkgColor.CreateSolidColorBitmap(Width, Height: Integer; Color: TColor): TBitmap;
begin
  Result:= TBitmap.Create;
  Result.SetSize(Width, Height);
  Result.PixelFormat:= pf24bit;
  Result.Canvas.Brush.Color:= Color;
  Result.Canvas.FillRect(Rect(0, 0, Width, Height));
end;


function TTestBkgColor.CreateBitmapWithBlackBorder(Width, Height: Integer; BorderWidth: Integer; InnerColor: TColor): TBitmap;
begin
  Result:= TBitmap.Create;
  Result.SetSize(Width, Height);
  Result.PixelFormat:= pf24bit;

  { Fill with black first }
  Result.Canvas.Brush.Color:= clBlack;
  Result.Canvas.FillRect(Rect(0, 0, Width, Height));

  { Fill inner area with different color }
  Result.Canvas.Brush.Color:= InnerColor;
  Result.Canvas.FillRect(Rect(BorderWidth, BorderWidth, Width - BorderWidth, Height - BorderWidth));
end;


function TTestBkgColor.CreateGradientBitmap(Width, Height: Integer): TBitmap;
VAR
  x, y: Integer;
begin
  Result:= TBitmap.Create;
  Result.SetSize(Width, Height);
  Result.PixelFormat:= pf24bit;

  { Create a simple gradient from black at top to white at bottom }
  for y:= 0 to Height - 1 do
    for x:= 0 to Width - 1 do
      Result.Canvas.Pixels[x, y]:= RGB(
        (y * 255) div Height,
        (y * 255) div Height,
        (y * 255) div Height
      );
end;


procedure TTestBkgColor.Setup;
begin
  FTestBitmap:= NIL;
end;


procedure TTestBkgColor.TearDown;
begin
  FreeAndNil(FTestBitmap);
end;


{ LineIsBlack Tests }

procedure TTestBkgColor.TestLineIsBlack_BlackLine;
begin
  FTestBitmap:= CreateSolidColorBitmap(100, 50, clBlack);
  Assert.IsTrue(LineIsBlack(FTestBitmap, 0, 3), 'First line of black bitmap should be black');
  Assert.IsTrue(LineIsBlack(FTestBitmap, 25, 3), 'Middle line of black bitmap should be black');
  Assert.IsTrue(LineIsBlack(FTestBitmap, 49, 3), 'Last line of black bitmap should be black');
end;


procedure TTestBkgColor.TestLineIsBlack_WhiteLine;
begin
  FTestBitmap:= CreateSolidColorBitmap(100, 50, clWhite);
  Assert.IsFalse(LineIsBlack(FTestBitmap, 0, 3), 'White line should not be detected as black');
  Assert.IsFalse(LineIsBlack(FTestBitmap, 25, 3), 'White line should not be detected as black');
end;


procedure TTestBkgColor.TestLineIsBlack_NearBlackLine;
begin
  { Create a bitmap with very dark gray (near black) }
  FTestBitmap:= CreateSolidColorBitmap(100, 50, RGB(2, 2, 2));
  Assert.IsTrue(LineIsBlack(FTestBitmap, 0, 3), 'Near-black line should be detected as black with tolerance 3');

  { Create with slightly brighter gray }
  FreeAndNil(FTestBitmap);
  FTestBitmap:= CreateSolidColorBitmap(100, 50, RGB(10, 10, 10));
  Assert.IsFalse(LineIsBlack(FTestBitmap, 0, 3), 'Brighter gray should not be detected as black with tolerance 3');
  Assert.IsTrue(LineIsBlack(FTestBitmap, 0, 15), 'Brighter gray should be detected as black with higher tolerance');
end;


procedure TTestBkgColor.TestLineIsBlack_InvalidRow;
begin
  FTestBitmap:= CreateSolidColorBitmap(100, 50, clBlack);

  Assert.WillRaise(
    procedure
    begin
      LineIsBlack(FTestBitmap, -1, 3);
    end,
    Exception,
    'Should raise exception for negative row'
  );

  Assert.WillRaise(
    procedure
    begin
      LineIsBlack(FTestBitmap, 50, 3);  { Height is 50, so valid rows are 0-49 }
    end,
    Exception,
    'Should raise exception for row >= Height'
  );
end;


procedure TTestBkgColor.TestLineIsBlack_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      LineIsBlack(NIL, 0, 3);
    end,
    Exception,
    'Should raise exception for nil bitmap'
  );
end;


{ HasBlackBorder Tests }

procedure TTestBkgColor.TestHasBlackBorder_WithBlackBorder;
begin
  { Create bitmap with 1-pixel black border and white interior }
  FTestBitmap:= CreateBitmapWithBlackBorder(100, 100, 1, clWhite);
  Assert.IsTrue(HasBlackBorder(FTestBitmap, 5), 'Should detect black border');
end;


procedure TTestBkgColor.TestHasBlackBorder_WithoutBlackBorder;
begin
  { Solid white bitmap - no black border }
  FTestBitmap:= CreateSolidColorBitmap(100, 100, clWhite);
  Assert.IsFalse(HasBlackBorder(FTestBitmap, 5), 'Should not detect black border on white image');

  { Solid red bitmap }
  FreeAndNil(FTestBitmap);
  FTestBitmap:= CreateSolidColorBitmap(100, 100, clRed);
  Assert.IsFalse(HasBlackBorder(FTestBitmap, 5), 'Should not detect black border on red image');
end;


procedure TTestBkgColor.TestHasBlackBorder_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      HasBlackBorder(NIL, 5);
    end,
    Exception,
    'Should raise exception for nil bitmap'
  );
end;


{ RemoveBorder Tests }

procedure TTestBkgColor.TestRemoveBorder_ReducesSize;
VAR
  OrigWidth, OrigHeight: Integer;
begin
  FTestBitmap:= CreateBitmapWithBlackBorder(100, 80, 1, clWhite);
  OrigWidth:= FTestBitmap.Width;
  OrigHeight:= FTestBitmap.Height;

  RemoveBorder(FTestBitmap);

  { RemoveBorder crops 1 pixel from each side }
  Assert.AreEqual(OrigWidth - 2, FTestBitmap.Width, 'Width should decrease by 2');
  Assert.AreEqual(OrigHeight - 2, FTestBitmap.Height, 'Height should decrease by 2');
end;


procedure TTestBkgColor.TestRemoveBorder_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      RemoveBorder(NIL);
    end,
    Exception,
    'Should raise exception for nil bitmap'
  );
end;


{ ApplyBorderRectOut Tests }

procedure TTestBkgColor.TestApplyBorderRectOut_IncreasesSize;
VAR
  OrigWidth, OrigHeight: Integer;
  BorderSize: Integer;
begin
  FTestBitmap:= CreateSolidColorBitmap(100, 80, clBlue);
  OrigWidth:= FTestBitmap.Width;
  OrigHeight:= FTestBitmap.Height;
  BorderSize:= 10;

  ApplyBorderRectOut(FTestBitmap, BorderSize, clWhite, FALSE);

  { Size should increase by 2*BorderSize in each dimension }
  Assert.AreEqual(OrigWidth + (BorderSize * 2), FTestBitmap.Width, 'Width should increase by 2*BorderSize');
  Assert.AreEqual(OrigHeight + (BorderSize * 2), FTestBitmap.Height, 'Height should increase by 2*BorderSize');
end;


procedure TTestBkgColor.TestApplyBorderRectOut_ZeroBorder;
VAR
  OrigWidth, OrigHeight: Integer;
begin
  FTestBitmap:= CreateSolidColorBitmap(100, 80, clBlue);
  OrigWidth:= FTestBitmap.Width;
  OrigHeight:= FTestBitmap.Height;

  ApplyBorderRectOut(FTestBitmap, 0, clWhite, FALSE);

  { Size should remain unchanged with zero border }
  Assert.AreEqual(OrigWidth, FTestBitmap.Width, 'Width should remain unchanged');
  Assert.AreEqual(OrigHeight, FTestBitmap.Height, 'Height should remain unchanged');
end;


procedure TTestBkgColor.TestApplyBorderRectOut_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      ApplyBorderRectOut(NIL, 10, clWhite, FALSE);
    end,
    Exception,
    'Should raise exception for nil bitmap'
  );
end;


{ ApplyBorderRectIn Tests }

procedure TTestBkgColor.TestApplyBorderRectIn_SameSize;
VAR
  OrigWidth, OrigHeight: Integer;
begin
  FTestBitmap:= CreateSolidColorBitmap(100, 80, clBlue);
  OrigWidth:= FTestBitmap.Width;
  OrigHeight:= FTestBitmap.Height;

  ApplyBorderRectIn(FTestBitmap, 5, clRed);

  { Size should remain unchanged - border is drawn inside }
  Assert.AreEqual(OrigWidth, FTestBitmap.Width, 'Width should remain unchanged');
  Assert.AreEqual(OrigHeight, FTestBitmap.Height, 'Height should remain unchanged');
end;


procedure TTestBkgColor.TestApplyBorderRectIn_ZeroBorder;
VAR
  OrigWidth, OrigHeight: Integer;
begin
  FTestBitmap:= CreateSolidColorBitmap(100, 80, clBlue);
  OrigWidth:= FTestBitmap.Width;
  OrigHeight:= FTestBitmap.Height;

  ApplyBorderRectIn(FTestBitmap, 0, clRed);

  { No change with zero border }
  Assert.AreEqual(OrigWidth, FTestBitmap.Width, 'Width should remain unchanged');
  Assert.AreEqual(OrigHeight, FTestBitmap.Height, 'Height should remain unchanged');
end;


procedure TTestBkgColor.TestApplyBorderRectIn_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      ApplyBorderRectIn(NIL, 10, clWhite);
    end,
    Exception,
    'Should raise exception for nil bitmap'
  );
end;


{ GetBorderDominantColor Tests }

procedure TTestBkgColor.TestGetBorderDominantColor_SolidColor;
VAR
  DominantColor: TColor;
begin
  FTestBitmap:= CreateSolidColorBitmap(100, 100, clRed);

  DominantColor:= GetBorderDominantColor(FTestBitmap, btTop, 8);
  Assert.AreEqual(Integer(clRed), Integer(DominantColor), 'Top border should be red');

  DominantColor:= GetBorderDominantColor(FTestBitmap, btBottom, 8);
  Assert.AreEqual(Integer(clRed), Integer(DominantColor), 'Bottom border should be red');

  DominantColor:= GetBorderDominantColor(FTestBitmap, btLeft, 8);
  Assert.AreEqual(Integer(clRed), Integer(DominantColor), 'Left border should be red');

  DominantColor:= GetBorderDominantColor(FTestBitmap, btRight, 8);
  Assert.AreEqual(Integer(clRed), Integer(DominantColor), 'Right border should be red');
end;


procedure TTestBkgColor.TestGetBorderDominantColor_TooSmallImage;
VAR
  DominantColor: TColor;
begin
  { Image smaller than 4x4 should return -1 }
  FTestBitmap:= CreateSolidColorBitmap(3, 3, clRed);
  DominantColor:= GetBorderDominantColor(FTestBitmap, btTop, 8);
  Assert.AreEqual(-1, Integer(DominantColor), 'Should return -1 for too small image');
end;


procedure TTestBkgColor.TestGetBorderDominantColor_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      GetBorderDominantColor(NIL, btTop, 8);
    end,
    Exception,
    'Should raise exception for nil bitmap'
  );
end;


procedure TTestBkgColor.TestGetBorderDominantColor_AllBorders;
VAR
  DominantColor: TColor;
begin
  { Create bitmap with different colored borders }
  FTestBitmap:= TBitmap.Create;
  FTestBitmap.SetSize(100, 100);
  FTestBitmap.PixelFormat:= pf24bit;

  { Fill with white }
  FTestBitmap.Canvas.Brush.Color:= clWhite;
  FTestBitmap.Canvas.FillRect(Rect(0, 0, 100, 100));

  { Draw colored borders }
  FTestBitmap.Canvas.Pen.Color:= clRed;
  FTestBitmap.Canvas.MoveTo(0, 0);
  FTestBitmap.Canvas.LineTo(100, 0);  { Top = Red }

  FTestBitmap.Canvas.Pen.Color:= clBlue;
  FTestBitmap.Canvas.MoveTo(0, 99);
  FTestBitmap.Canvas.LineTo(100, 99);  { Bottom = Blue }

  { Test detection }
  DominantColor:= GetBorderDominantColor(FTestBitmap, btTop, 8);
  Assert.AreEqual(Integer(clRed), Integer(DominantColor), 'Top border should be red');

  DominantColor:= GetBorderDominantColor(FTestBitmap, btBottom, 8);
  Assert.AreEqual(Integer(clBlue), Integer(DominantColor), 'Bottom border should be blue');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestBkgColor);

end.
