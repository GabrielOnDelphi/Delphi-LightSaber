unit Test.LightVcl.Graph.ShadowText;

{=============================================================================================================
   Unit tests for LightVcl.Graph.ShadowText.pas
   Tests shadow text drawing functions with various color types and configurations.

   Note: These tests create temporary bitmaps to test drawing functions.
   Visual verification would require manual inspection, but we test that:
   - Functions don't raise exceptions
   - Functions return valid results
   - System colors are handled correctly

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics;

type
  [TestFixture]
  TTestShadowText = class
  private
    FBitmap: TBitmap;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Basic DrawShadowText Tests - X,Y overload }
    [Test]
    procedure TestDrawShadowText_XY_BasicCall;

    [Test]
    procedure TestDrawShadowText_XY_EmptyText;

    [Test]
    procedure TestDrawShadowText_XY_SystemColors;

    [Test]
    procedure TestDrawShadowText_XY_NegativeShadowDist;

    [Test]
    procedure TestDrawShadowText_XY_ZeroShadowDist;

    [Test]
    procedure TestDrawShadowText_XY_LargeShadowDist;

    [Test]
    procedure TestDrawShadowText_XY_NilCanvas;

    { DrawShadowText Tests - Rect overload }
    [Test]
    procedure TestDrawShadowText_Rect_BasicCall;

    [Test]
    procedure TestDrawShadowText_Rect_EmptyText;

    [Test]
    procedure TestDrawShadowText_Rect_SystemColors;

    [Test]
    procedure TestDrawShadowText_Rect_CenterAligned;

    [Test]
    procedure TestDrawShadowText_Rect_RightAligned;

    [Test]
    procedure TestDrawShadowText_Rect_WordBreak;

    [Test]
    procedure TestDrawShadowText_Rect_NilCanvas;

    { Color handling tests }
    [Test]
    procedure TestDrawShadowText_RGBColors;

    [Test]
    procedure TestDrawShadowText_SystemColorClBtnFace;

    [Test]
    procedure TestDrawShadowText_SystemColorClWindow;

    [Test]
    procedure TestDrawShadowText_SystemColorClHighlight;

    { Return value tests }
    [Test]
    procedure TestDrawShadowText_ReturnsNonZeroForValidText;

    [Test]
    procedure TestDrawShadowText_ReturnsZeroForEmptyText;

    { Long text tests }
    [Test]
    procedure TestDrawShadowText_LongText;

    [Test]
    procedure TestDrawShadowText_MultilineText;
  end;

implementation

uses
  LightVcl.Graph.ShadowText;


procedure TTestShadowText.Setup;
begin
  FBitmap:= TBitmap.Create;
  FBitmap.Width:= 400;
  FBitmap.Height:= 200;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Canvas.Brush.Color:= clWhite;
  FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
  FBitmap.Canvas.Font.Name:= 'Arial';
  FBitmap.Canvas.Font.Size:= 12;
end;


procedure TTestShadowText.TearDown;
begin
  FreeAndNil(FBitmap);
end;


{ Basic DrawShadowText Tests - X,Y overload }

procedure TTestShadowText.TestDrawShadowText_XY_BasicCall;
var
  Result: Integer;
begin
  Result:= DrawShadowText(FBitmap.Canvas, 'Hello World', 10, 10, clBlack, clGray);
  Assert.IsTrue(Result > 0, 'DrawShadowText should return height of text drawn');
end;


procedure TTestShadowText.TestDrawShadowText_XY_EmptyText;
var
  Result: Integer;
begin
  Result:= DrawShadowText(FBitmap.Canvas, '', 10, 10, clBlack, clGray);
  Assert.AreEqual(0, Result, 'DrawShadowText with empty text should return 0');
end;


procedure TTestShadowText.TestDrawShadowText_XY_SystemColors;
begin
  Assert.WillNotRaise(
    procedure
    begin
      DrawShadowText(FBitmap.Canvas, 'Test', 10, 10, clBtnFace, clBtnShadow);
    end,
    'DrawShadowText should handle system colors without exception');
end;


procedure TTestShadowText.TestDrawShadowText_XY_NegativeShadowDist;
begin
  Assert.WillNotRaise(
    procedure
    begin
      DrawShadowText(FBitmap.Canvas, 'Test', 10, 10, clBlack, clGray, -2);
    end,
    'DrawShadowText should handle negative shadow distance');
end;


procedure TTestShadowText.TestDrawShadowText_XY_ZeroShadowDist;
begin
  Assert.WillNotRaise(
    procedure
    begin
      DrawShadowText(FBitmap.Canvas, 'Test', 10, 10, clBlack, clGray, 0);
    end,
    'DrawShadowText should handle zero shadow distance');
end;


procedure TTestShadowText.TestDrawShadowText_XY_LargeShadowDist;
begin
  Assert.WillNotRaise(
    procedure
    begin
      DrawShadowText(FBitmap.Canvas, 'Test', 10, 10, clBlack, clGray, 50);
    end,
    'DrawShadowText should handle large shadow distance');
end;


procedure TTestShadowText.TestDrawShadowText_XY_NilCanvas;
begin
  Assert.WillRaise(
    procedure
    begin
      DrawShadowText(NIL, 'Test', 10, 10, clBlack, clGray);
    end,
    EAssertionFailed,
    'DrawShadowText should raise assertion for nil canvas');
end;


{ DrawShadowText Tests - Rect overload }

procedure TTestShadowText.TestDrawShadowText_Rect_BasicCall;
var
  Result: Integer;
  TextRect: TRect;
begin
  TextRect:= Rect(10, 10, 300, 100);
  Result:= DrawShadowText(FBitmap.Canvas, 'Hello World', TextRect, clBlack, clGray, 2);
  Assert.IsTrue(Result > 0, 'DrawShadowText should return height of text drawn');
end;


procedure TTestShadowText.TestDrawShadowText_Rect_EmptyText;
var
  Result: Integer;
  TextRect: TRect;
begin
  TextRect:= Rect(10, 10, 300, 100);
  Result:= DrawShadowText(FBitmap.Canvas, '', TextRect, clBlack, clGray, 2);
  Assert.AreEqual(0, Result, 'DrawShadowText with empty text should return 0');
end;


procedure TTestShadowText.TestDrawShadowText_Rect_SystemColors;
var
  TextRect: TRect;
begin
  TextRect:= Rect(10, 10, 300, 100);
  Assert.WillNotRaise(
    procedure
    begin
      DrawShadowText(FBitmap.Canvas, 'Test', TextRect, clBtnFace, clBtnShadow, 2);
    end,
    'DrawShadowText should handle system colors without exception');
end;


procedure TTestShadowText.TestDrawShadowText_Rect_CenterAligned;
var
  TextRect: TRect;
begin
  TextRect:= Rect(10, 10, 300, 100);
  Assert.WillNotRaise(
    procedure
    begin
      DrawShadowText(FBitmap.Canvas, 'Centered Text', TextRect, clBlack, clGray, 2, DT_CENTER);
    end,
    'DrawShadowText should handle DT_CENTER flag');
end;


procedure TTestShadowText.TestDrawShadowText_Rect_RightAligned;
var
  TextRect: TRect;
begin
  TextRect:= Rect(10, 10, 300, 100);
  Assert.WillNotRaise(
    procedure
    begin
      DrawShadowText(FBitmap.Canvas, 'Right Aligned', TextRect, clBlack, clGray, 2, DT_RIGHT);
    end,
    'DrawShadowText should handle DT_RIGHT flag');
end;


procedure TTestShadowText.TestDrawShadowText_Rect_WordBreak;
var
  TextRect: TRect;
begin
  TextRect:= Rect(10, 10, 100, 100);  { Narrow rect to force word break }
  Assert.WillNotRaise(
    procedure
    begin
      DrawShadowText(FBitmap.Canvas, 'This is a long text that should wrap', TextRect, clBlack, clGray, 2, DT_WORDBREAK);
    end,
    'DrawShadowText should handle DT_WORDBREAK flag');
end;


procedure TTestShadowText.TestDrawShadowText_Rect_NilCanvas;
var
  TextRect: TRect;
begin
  TextRect:= Rect(10, 10, 300, 100);
  Assert.WillRaise(
    procedure
    begin
      DrawShadowText(NIL, 'Test', TextRect, clBlack, clGray, 2);
    end,
    EAssertionFailed,
    'DrawShadowText should raise assertion for nil canvas');
end;


{ Color handling tests }

procedure TTestShadowText.TestDrawShadowText_RGBColors;
begin
  Assert.WillNotRaise(
    procedure
    begin
      DrawShadowText(FBitmap.Canvas, 'RGB Test', 10, 10, RGB(255, 0, 0), RGB(128, 128, 128));
    end,
    'DrawShadowText should handle RGB colors');
end;


procedure TTestShadowText.TestDrawShadowText_SystemColorClBtnFace;
begin
  Assert.WillNotRaise(
    procedure
    begin
      DrawShadowText(FBitmap.Canvas, 'BtnFace Test', 10, 10, clBtnFace, clBtnShadow);
    end,
    'DrawShadowText should handle clBtnFace system color');
end;


procedure TTestShadowText.TestDrawShadowText_SystemColorClWindow;
begin
  Assert.WillNotRaise(
    procedure
    begin
      DrawShadowText(FBitmap.Canvas, 'Window Test', 10, 10, clWindowText, clWindow);
    end,
    'DrawShadowText should handle clWindowText/clWindow system colors');
end;


procedure TTestShadowText.TestDrawShadowText_SystemColorClHighlight;
begin
  Assert.WillNotRaise(
    procedure
    begin
      DrawShadowText(FBitmap.Canvas, 'Highlight Test', 10, 10, clHighlightText, clHighlight);
    end,
    'DrawShadowText should handle clHighlightText/clHighlight system colors');
end;


{ Return value tests }

procedure TTestShadowText.TestDrawShadowText_ReturnsNonZeroForValidText;
var
  Result: Integer;
begin
  Result:= DrawShadowText(FBitmap.Canvas, 'Valid Text', 10, 10, clBlack, clGray);
  Assert.IsTrue(Result > 0, 'DrawShadowText should return non-zero for valid text');
end;


procedure TTestShadowText.TestDrawShadowText_ReturnsZeroForEmptyText;
var
  Result: Integer;
begin
  Result:= DrawShadowText(FBitmap.Canvas, '', 10, 10, clBlack, clGray);
  Assert.AreEqual(0, Result, 'DrawShadowText should return 0 for empty text');
end;


{ Long text tests }

procedure TTestShadowText.TestDrawShadowText_LongText;
var
  LongText: string;
begin
  LongText:= StringOfChar('A', 1000);
  Assert.WillNotRaise(
    procedure
    begin
      DrawShadowText(FBitmap.Canvas, LongText, 10, 10, clBlack, clGray);
    end,
    'DrawShadowText should handle long text');
end;


procedure TTestShadowText.TestDrawShadowText_MultilineText;
var
  TextRect: TRect;
begin
  TextRect:= Rect(10, 10, 300, 200);
  Assert.WillNotRaise(
    procedure
    begin
      DrawShadowText(FBitmap.Canvas, 'Line 1'#13#10'Line 2'#13#10'Line 3', TextRect, clBlack, clGray, 2, DT_LEFT);
    end,
    'DrawShadowText should handle multiline text');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestShadowText);

end.
