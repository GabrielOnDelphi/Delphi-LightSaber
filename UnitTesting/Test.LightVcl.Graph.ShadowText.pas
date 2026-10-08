unit Test.LightVcl.Graph.ShadowText;

{=============================================================================================================
   Unit tests for LightVcl.Graph.ShadowText.pas
   Tests shadow text drawing functions with various color types and configurations.

   Note: These tests draw on a temporary bitmap and read its pixels back:
   - the text and shadow colours (system colours converted with ColorToRGB) appear on the canvas
   - DT_CENTER / DT_RIGHT move the text, DT_WORDBREAK raises the returned height
   - an empty string paints nothing

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
    procedure PrepareCanvas(Background: TColor);
    function  CountPixels(Color: TColor): Integer;
    function  InkColumns(Background: TColor; out MinX, MaxX: Integer): Boolean;
    procedure CheckColorsPainted(TextColor, ShadowColor: TColor; UseRect: Boolean);
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


{ Fills the bitmap and selects a big, non-antialiased font, so the text and shadow colours land on the pixels unblended }
procedure TTestShadowText.PrepareCanvas(Background: TColor);
begin
  FBitmap.Canvas.Brush.Color:= Background;
  FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
  FBitmap.Canvas.Font.Name:= 'Arial';
  FBitmap.Canvas.Font.Size:= 24;
  FBitmap.Canvas.Font.Style:= [fsBold];
  FBitmap.Canvas.Font.Quality:= fqNonAntialiased;
end;


function TTestShadowText.CountPixels(Color: TColor): Integer;
var
  x, y: Integer;
  RGBColor: TColorRef;
  Line: PRGBTriple;
begin
  Result:= 0;
  RGBColor:= ColorToRGB(Color);
  for y:= 0 to FBitmap.Height - 1 do
  begin
    Line:= FBitmap.ScanLine[y];
    for x:= 0 to FBitmap.Width - 1 do
    begin
      if  (Line.rgbtRed   = GetRValue(RGBColor))
      AND (Line.rgbtGreen = GetGValue(RGBColor))
      AND (Line.rgbtBlue  = GetBValue(RGBColor))
      then Inc(Result);
      Inc(Line);
    end;
  end;
end;


{ Returns the first and the last column that holds a pixel different from Background }
function TTestShadowText.InkColumns(Background: TColor; out MinX, MaxX: Integer): Boolean;
var
  x, y: Integer;
  Bkg: TColorRef;
begin
  MinX:= MaxInt;
  MaxX:= -1;
  Bkg:= ColorToRGB(Background);
  for y:= 0 to FBitmap.Height - 1 do
    for x:= 0 to FBitmap.Width - 1 do
      if TColorRef(FBitmap.Canvas.Pixels[x, y]) <> Bkg then
      begin
        if x < MinX then MinX:= x;
        if x > MaxX then MaxX:= x;
      end;
  Result:= MaxX >= 0;
end;


{ Draws on a green background and demands that both colours, converted with ColorToRGB, appear on the canvas }
procedure TTestShadowText.CheckColorsPainted(TextColor, ShadowColor: TColor; UseRect: Boolean);
begin
  PrepareCanvas(RGB(0, 128, 0));
  if UseRect
  then DrawShadowText(FBitmap.Canvas, 'WWWW', Rect(10, 10, 390, 190), TextColor, ShadowColor, 3)
  else DrawShadowText(FBitmap.Canvas, 'WWWW', 10, 10, TextColor, ShadowColor, 3);

  Assert.IsTrue(CountPixels(TextColor)   > 20, 'The text colour must appear on the canvas');
  Assert.IsTrue(CountPixels(ShadowColor) > 20, 'The shadow colour must appear on the canvas');
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
  Before: TColor;
begin
  { See TestDrawShadowText_ReturnsZeroForEmptyText: the return of 0 was a guess about Windows.
    Windows 11 gives 1 for an empty string. What must hold is that nothing is painted. }
  FBitmap.Canvas.Brush.Color:= clWhite;
  FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
  Before:= FBitmap.Canvas.Pixels[12, 12];

  DrawShadowText(FBitmap.Canvas, '', 10, 10, clBlack, clGray);

  Assert.AreEqual(Integer(Before), Integer(FBitmap.Canvas.Pixels[12, 12]), 'Empty text must paint nothing');
end;


procedure TTestShadowText.TestDrawShadowText_XY_SystemColors;
begin
  CheckColorsPainted(clActiveCaption, clBtnShadow, FALSE);
end;


procedure TTestShadowText.TestDrawShadowText_XY_NegativeShadowDist;
begin
  Assert.WillNotRaiseAny(
    procedure
    begin
      DrawShadowText(FBitmap.Canvas, 'Test', 10, 10, clBlack, clGray, -2);
    end,
    'DrawShadowText should handle negative shadow distance');
end;


procedure TTestShadowText.TestDrawShadowText_XY_ZeroShadowDist;
begin
  Assert.WillNotRaiseAny(
    procedure
    begin
      DrawShadowText(FBitmap.Canvas, 'Test', 10, 10, clBlack, clGray, 0);
    end,
    'DrawShadowText should handle zero shadow distance');
end;


procedure TTestShadowText.TestDrawShadowText_XY_LargeShadowDist;
begin
  Assert.WillNotRaiseAny(
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
  TextRect: TRect;
  Before: TColor;
begin
  { See TestDrawShadowText_ReturnsZeroForEmptyText: the return of 0 was a guess about Windows.
    Windows 11 gives 1 for an empty string. What must hold is that nothing is painted. }
  FBitmap.Canvas.Brush.Color:= clWhite;
  FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
  Before:= FBitmap.Canvas.Pixels[12, 12];

  TextRect:= Rect(10, 10, 300, 100);
  DrawShadowText(FBitmap.Canvas, '', TextRect, clBlack, clGray, 2);

  Assert.AreEqual(Integer(Before), Integer(FBitmap.Canvas.Pixels[12, 12]), 'Empty text must paint nothing');
end;


procedure TTestShadowText.TestDrawShadowText_Rect_SystemColors;
begin
  CheckColorsPainted(clBtnFace, clBtnShadow, TRUE);
end;


procedure TTestShadowText.TestDrawShadowText_Rect_CenterAligned;
var
  MinX, MaxX, TextW, ExpectedLeft: Integer;
begin
  PrepareCanvas(clWhite);
  TextW:= FBitmap.Canvas.TextWidth('Centered');
  DrawShadowText(FBitmap.Canvas, 'Centered', Rect(10, 10, 390, 100), clBlack, clGray, 2, DT_CENTER);

  Assert.IsTrue(InkColumns(clWhite, MinX, MaxX), 'Something must be painted');
  ExpectedLeft:= 10 + (380 - TextW) div 2;
  Assert.IsTrue(Abs(MinX - ExpectedLeft) <= 4, 'Centred text must start at about ' + IntToStr(ExpectedLeft) + ', starts at ' + IntToStr(MinX));
end;


procedure TTestShadowText.TestDrawShadowText_Rect_RightAligned;
var
  MinX, MaxX, TextW: Integer;
begin
  PrepareCanvas(clWhite);
  TextW:= FBitmap.Canvas.TextWidth('Right');
  DrawShadowText(FBitmap.Canvas, 'Right', Rect(10, 10, 390, 100), clBlack, clGray, 2, DT_RIGHT);

  Assert.IsTrue(InkColumns(clWhite, MinX, MaxX), 'Something must be painted');
  Assert.IsTrue(Abs(MinX - (390 - TextW)) <= 4, 'Right-aligned text must start at about ' + IntToStr(390 - TextW) + ', starts at ' + IntToStr(MinX));
end;


procedure TTestShadowText.TestDrawShadowText_Rect_WordBreak;
var
  Height1, HeightWrapped: Integer;
begin
  { One line, then the same kind of text in a rect too narrow for it: with DT_WORDBREAK the returned height must cover several lines }
  Height1:= DrawShadowText(FBitmap.Canvas, 'Wrap', Rect(10, 10, 390, 190), clBlack, clGray, 2, DT_LEFT);
  HeightWrapped:= DrawShadowText(FBitmap.Canvas, 'This is a long text that should wrap', Rect(10, 10, 100, 190), clBlack, clGray, 2, DT_WORDBREAK);

  Assert.IsTrue(Height1 > 0, 'A single line must have a height');
  Assert.IsTrue(HeightWrapped >= 2 * Height1, 'Wrapped text must be at least two lines high: single ' + IntToStr(Height1) + ', wrapped ' + IntToStr(HeightWrapped));
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
  CheckColorsPainted(RGB(255, 0, 0), RGB(128, 128, 128), FALSE);
end;


procedure TTestShadowText.TestDrawShadowText_SystemColorClBtnFace;
begin
  CheckColorsPainted(clBtnFace, clBtnShadow, FALSE);
end;


procedure TTestShadowText.TestDrawShadowText_SystemColorClWindow;
begin
  CheckColorsPainted(clWindowText, clWindow, FALSE);
end;


procedure TTestShadowText.TestDrawShadowText_SystemColorClHighlight;
begin
  CheckColorsPainted(clHighlightText, clHighlight, FALSE);
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
  Before: TColor;
begin
  { The old test demanded a return of 0. That was a guess about Windows, not a LightSaber promise:
    DrawShadowText passes the string to the DrawShadowText in ComCtl32 and returns what that
    returns, and on Windows 11 an empty string gives 1. What an empty string must really do is
    change nothing on the canvas, so that is what is checked. }
  FBitmap.Canvas.Brush.Color:= clWhite;
  FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
  Before:= FBitmap.Canvas.Pixels[10, 10];

  DrawShadowText(FBitmap.Canvas, '', 10, 10, clBlack, clGray);

  Assert.AreEqual(Integer(Before), Integer(FBitmap.Canvas.Pixels[10, 10]), 'Empty text must paint nothing');
end;


{ Long text tests }

procedure TTestShadowText.TestDrawShadowText_LongText;
var
  LongText: string;
begin
  LongText:= StringOfChar('A', 1000);
  Assert.WillNotRaiseAny(
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
  Assert.WillNotRaiseAny(
    procedure
    begin
      DrawShadowText(FBitmap.Canvas, 'Line 1'#13#10'Line 2'#13#10'Line 3', TextRect, clBlack, clGray, 2, DT_LEFT);
    end,
    'DrawShadowText should handle multiline text');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestShadowText);

end.
