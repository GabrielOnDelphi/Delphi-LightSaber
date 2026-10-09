unit Test.LightVcl.Graph.Util;

{=============================================================================================================
   Unit tests for LightVcl.Graph.Util.pas
   Tests color manipulation, conversion, blending, and replacement functions.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Winapi.Windows,
  System.Types,           { Rect() - used by Canvas.FillRect }
  Vcl.Graphics;

type
  [TestFixture]
  TTestGraphUtil = class
  private
    FBitmap: TBitmap;
    procedure CreateColorBitmap(Width, Height: Integer; Color: TColor);
    procedure CreateSolidBitmap24(Width, Height: Integer; R, G, B: Byte);
    procedure SetPixel24(X, Y: Integer; R, G, B: Byte);
    procedure AssertRGB(Color: TColor; R, G, B: Byte; const Msg: string);
    procedure AssertPixel24(X, Y: Integer; R, G, B: Byte; const Msg: string);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { DarkenColor Tests }
    [Test]
    procedure TestDarkenColor_Black;

    [Test]
    procedure TestDarkenColor_White_50Percent;

    [Test]
    procedure TestDarkenColor_100Percent_Unchanged;

    [Test]
    procedure TestDarkenColor_0Percent_TotallyDark;

    { LightenColor Tests }
    [Test]
    procedure TestLightenColor_Black_50Percent;

    [Test]
    procedure TestLightenColor_White_Unchanged;

    [Test]
    procedure TestLightenColor_100Percent_Unchanged;

    [Test]
    procedure TestLightenColor_0Percent_TotallyLight;

    { ChangeBrightness Tests }
    [Test]
    procedure TestChangeBrightness_Increase;

    [Test]
    procedure TestChangeBrightness_Decrease;

    [Test]
    procedure TestChangeBrightness_ClampMax;

    [Test]
    procedure TestChangeBrightness_ClampMin;

    { ChangeColor Tests }
    [Test]
    procedure TestChangeColor_TowardsWhite;

    [Test]
    procedure TestChangeColor_TowardsBlack;

    [Test]
    procedure TestChangeColor_0Percent_NoChange;

    { SimilarColor Tests }
    [Test]
    procedure TestSimilarColor_ExactMatch;

    [Test]
    procedure TestSimilarColor_WithinTolerance;

    [Test]
    procedure TestSimilarColor_OutsideTolerance;

    [Test]
    procedure TestSimilarColor_SystemColors;

    { ComplementaryColor Tests }
    [Test]
    procedure TestComplementaryColor_Black;

    [Test]
    procedure TestComplementaryColor_White;

    [Test]
    procedure TestComplementaryColor_Red;

    { ColorToHtml Tests }
    [Test]
    procedure TestColorToHtml_Black;

    [Test]
    procedure TestColorToHtml_White;

    [Test]
    procedure TestColorToHtml_Red;

    { HtmlToColor Tests }
    [Test]
    procedure TestHtmlToColor_WithHash;

    [Test]
    procedure TestHtmlToColor_WithoutHash;

    [Test]
    procedure TestHtmlToColor_Black;

    [Test]
    procedure TestHtmlToColor_White;

    [Test]
    procedure TestHtmlToColor_RoundTrip;

    { SplitColor2RGB Tests }
    [Test]
    procedure TestSplitColor2RGB_Red;

    [Test]
    procedure TestSplitColor2RGB_Green;

    [Test]
    procedure TestSplitColor2RGB_Blue;

    [Test]
    procedure TestSplitColor2RGB_White;

    { Integer2Color Tests }
    [Test]
    procedure TestInteger2Color_Zero;

    [Test]
    procedure TestInteger2Color_255;

    [Test]
    procedure TestInteger2Color_65535;

    [Test]
    procedure TestInteger2Color_Negative;

    { BlendColors Tests }
    [Test]
    procedure TestBlendColors_50Percent;

    [Test]
    procedure TestBlendColors_0Percent;

    [Test]
    procedure TestBlendColors_100Percent;

    { MixColors Tests }
    [Test]
    procedure TestMixColors_0_FullyFG;

    [Test]
    procedure TestMixColors_255_FullyBG;

    [Test]
    procedure TestMixColors_128_MidBlend;

    { ReplaceColor Tests }
    [Test]
    procedure TestReplaceColor_BasicCall;

    [Test]
    procedure TestReplaceColor_NilBitmap;

    [Test]
    procedure TestReplaceColor_ReplacesCorrectly;

    [Test]
    procedure TestReplaceColor_NoMatchNoChange;

    { ReplaceColor with Tolerance Tests }
    [Test]
    procedure TestReplaceColorTolerance_BasicCall;

    [Test]
    procedure TestReplaceColorTolerance_NilBitmap;

    [Test]
    procedure TestReplaceColorTolerance_WithinTolerance;

    { GetAverageColor Tests }
    [Test]
    procedure TestGetAverageColor_AllBlack;

    [Test]
    procedure TestGetAverageColor_AllWhite;

    [Test]
    procedure TestGetAverageColor_AllRed;

    [Test]
    procedure TestGetAverageColor_NilBitmap;

    [Test]
    procedure TestGetAverageColor_EmptyBitmap;

    [Test]
    procedure TestGetAverageColor_FastMode;

    { GetAverageColorPf8 Tests }
    [Test]
    procedure TestGetAverageColorPf8_AllBlack;

    [Test]
    procedure TestGetAverageColorPf8_AllWhite;

    [Test]
    procedure TestGetAverageColorPf8_NilBitmap;

    [Test]
    procedure TestGetAverageColorPf8_EmptyBitmap;

    { GetDeviceColorDepth Tests }
    [Test]
    procedure TestGetDeviceColorDepth_ReturnsPositive;

    { Theme Functions Tests }
    [Test]
    procedure TestVclStylesEnabled_NoCrash;

    [Test]
    procedure TestThemeColorBkg_NoCrash;

    [Test]
    procedure TestThemeColorHilight_NoCrash;

    [Test]
    procedure TestThemeColorButtonFace_NoCrash;
  end;

implementation

uses
  Vcl.Themes,
  LightVcl.Graph.Util;


procedure TTestGraphUtil.Setup;
begin
  FBitmap:= TBitmap.Create;
end;


procedure TTestGraphUtil.TearDown;
begin
  FreeAndNil(FBitmap);
end;


procedure TTestGraphUtil.CreateColorBitmap(Width, Height: Integer; Color: TColor);
begin
  FBitmap.SetSize(Width, Height);
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Canvas.Brush.Color:= Color;
  FBitmap.Canvas.FillRect(Rect(0, 0, Width, Height));
end;


procedure TTestGraphUtil.CreateSolidBitmap24(Width, Height: Integer; R, G, B: Byte);
var
  Row, Col: Integer;
  Line: PRGB24Array;
begin
  FBitmap.SetSize(Width, Height);
  FBitmap.PixelFormat:= pf24bit;

  for Row:= 0 to Height - 1 do
  begin
    Line:= FBitmap.ScanLine[Row];
    for Col:= 0 to Width - 1 do
    begin
      Line[Col].R:= R;
      Line[Col].G:= G;
      Line[Col].B:= B;
    end;
  end;
end;


procedure TTestGraphUtil.SetPixel24(X, Y: Integer; R, G, B: Byte);
var
  Line: PRGB24Array;
begin
  Line:= FBitmap.ScanLine[Y];
  Line[X].R:= R;
  Line[X].G:= G;
  Line[X].B:= B;
end;


{ Checks all three channels of Color, so a routine that gets only one channel right cannot pass }
procedure TTestGraphUtil.AssertRGB(Color: TColor; R, G, B: Byte; const Msg: string);
begin
  Assert.AreEqual(Integer(R), Integer(GetRValue(Color)), Msg + ' (red channel)');
  Assert.AreEqual(Integer(G), Integer(GetGValue(Color)), Msg + ' (green channel)');
  Assert.AreEqual(Integer(B), Integer(GetBValue(Color)), Msg + ' (blue channel)');
end;


procedure TTestGraphUtil.AssertPixel24(X, Y: Integer; R, G, B: Byte; const Msg: string);
var
  Line: PRGB24Array;
begin
  Line:= FBitmap.ScanLine[Y];
  Assert.AreEqual(Integer(R), Integer(Line[X].R), Msg + ' (red channel)');
  Assert.AreEqual(Integer(G), Integer(Line[X].G), Msg + ' (green channel)');
  Assert.AreEqual(Integer(B), Integer(Line[X].B), Msg + ' (blue channel)');
end;


{ DarkenColor Tests }

procedure TTestGraphUtil.TestDarkenColor_Black;
var
  Result: TColor;
begin
  Result:= DarkenColor(clBlack, 50);
  Assert.AreEqual(TColor(clBlack), Result, 'Black should remain black');
end;


procedure TTestGraphUtil.TestDarkenColor_White_50Percent;
var
  Result: TColor;
  R, G, B: Byte;
begin
  Result:= DarkenColor(clWhite, 50);
  R:= GetRValue(Result);
  G:= GetGValue(Result);
  B:= GetBValue(Result);
  { 255 * 50 / 100 = 127 or 128 }
  Assert.IsTrue((R >= 127) and (R <= 128), 'Red should be around 127');
  Assert.IsTrue((G >= 127) and (G <= 128), 'Green should be around 127');
  Assert.IsTrue((B >= 127) and (B <= 128), 'Blue should be around 127');
end;


procedure TTestGraphUtil.TestDarkenColor_100Percent_Unchanged;
var
  Result: TColor;
begin
  Result:= DarkenColor(clRed, 100);
  Assert.AreEqual(TColor(clRed), Result, '100% should leave color unchanged');
end;


procedure TTestGraphUtil.TestDarkenColor_0Percent_TotallyDark;
var
  Result: TColor;
begin
  Result:= DarkenColor(clWhite, 0);
  Assert.AreEqual(TColor(clBlack), Result, '0% should make color totally dark');
end;


{ LightenColor Tests }

procedure TTestGraphUtil.TestLightenColor_Black_50Percent;
var
  Result: TColor;
  R, G, B: Byte;
begin
  Result:= LightenColor(clBlack, 50);
  R:= GetRValue(Result);
  G:= GetGValue(Result);
  B:= GetBValue(Result);
  { Should be around 127-128 }
  Assert.IsTrue((R >= 127) and (R <= 128), 'Red should be around 127');
  Assert.IsTrue((G >= 127) and (G <= 128), 'Green should be around 127');
  Assert.IsTrue((B >= 127) and (B <= 128), 'Blue should be around 127');
end;


procedure TTestGraphUtil.TestLightenColor_White_Unchanged;
var
  Result: TColor;
begin
  Result:= LightenColor(clWhite, 50);
  Assert.AreEqual(TColor(clWhite), Result, 'White should remain white when lightened');
end;


procedure TTestGraphUtil.TestLightenColor_100Percent_Unchanged;
var
  Result: TColor;
begin
  Result:= LightenColor(clRed, 100);
  Assert.AreEqual(TColor(clRed), Result, '100% should leave color unchanged');
end;


procedure TTestGraphUtil.TestLightenColor_0Percent_TotallyLight;
var
  Result: TColor;
begin
  Result:= LightenColor(clBlack, 0);
  Assert.AreEqual(TColor(clWhite), Result, '0% should make color totally light (white)');
end;


{ ChangeBrightness Tests }

procedure TTestGraphUtil.TestChangeBrightness_Increase;
begin
  { Each channel different, so a routine that mixes up or skips a channel fails }
  AssertRGB(ChangeBrightness(RGB(100, 60, 30), 50), 150, 110, 80, 'Each channel must increase by 50');
end;


procedure TTestGraphUtil.TestChangeBrightness_Decrease;
begin
  AssertRGB(ChangeBrightness(RGB(100, 60, 200), -50), 50, 10, 150, 'Each channel must decrease by 50');
end;


procedure TTestGraphUtil.TestChangeBrightness_ClampMax;
begin
  { R and B overflow and clamp; G does not, so it proves the clamp is per channel }
  AssertRGB(ChangeBrightness(RGB(200, 100, 160), 100), 255, 200, 255, 'Brightness should clamp at 255');
end;


procedure TTestGraphUtil.TestChangeBrightness_ClampMin;
begin
  { R and B underflow and clamp; G does not }
  AssertRGB(ChangeBrightness(RGB(50, 150, 30), -100), 0, 50, 0, 'Brightness should clamp at 0');
end;


{ ChangeColor Tests }

{ Odd source values make every half-way difference an integer (254/2, 154/2, 54/2), so Round has no .5 case to decide }
procedure TTestGraphUtil.TestChangeColor_TowardsWhite;
begin
  AssertRGB(ChangeColor(RGB(1, 101, 201), clWhite, 50), 128, 178, 228, 'Each channel must move half way towards white');
end;


procedure TTestGraphUtil.TestChangeColor_TowardsBlack;
begin
  AssertRGB(ChangeColor(RGB(254, 154, 54), clBlack, 50), 127, 77, 27, 'Each channel must move half way towards black');
end;


procedure TTestGraphUtil.TestChangeColor_0Percent_NoChange;
var
  Result: TColor;
begin
  Result:= ChangeColor(clRed, clBlue, 0);
  Assert.AreEqual(TColor(clRed), Result, '0% change should leave color unchanged');
end;


{ SimilarColor Tests }

procedure TTestGraphUtil.TestSimilarColor_ExactMatch;
begin
  Assert.IsTrue(SimilarColor(clRed, clRed, 0), 'Exact same colors should be similar with 0 tolerance');
end;


procedure TTestGraphUtil.TestSimilarColor_WithinTolerance;
begin
  Assert.IsTrue(SimilarColor(RGB(100, 100, 100), RGB(105, 105, 105), 10), 'Colors within tolerance should be similar');
end;


procedure TTestGraphUtil.TestSimilarColor_OutsideTolerance;
begin
  { One unit past the tolerance, one channel at a time: each channel must be checked on its own }
  Assert.IsFalse(SimilarColor(RGB(100, 100, 100), RGB(111, 100, 100), 10), 'Red 11 apart must not be similar');
  Assert.IsFalse(SimilarColor(RGB(100, 100, 100), RGB(100, 111, 100), 10), 'Green 11 apart must not be similar');
  Assert.IsFalse(SimilarColor(RGB(100, 100, 100), RGB(100, 100, 111), 10), 'Blue 11 apart must not be similar');
  Assert.IsFalse(SimilarColor(RGB(111, 100, 100), RGB(100, 100, 100), 10), 'The difference must count in both directions');
  { The boundary: a difference of exactly Tolerance is still similar (the routine compares with <=) }
  Assert.IsTrue(SimilarColor(RGB(100, 100, 100), RGB(110, 90, 110), 10), 'A difference of exactly 10 is still similar');
end;


procedure TTestGraphUtil.TestSimilarColor_SystemColors;
begin
  { clWindow is a system color ($80000005): it matches its own RGB value only if SimilarColor converts it with ColorToRGB }
  Assert.IsTrue(SimilarColor(clWindow, ColorToRGB(clWindow), 0), 'A system color must equal its RGB value');
  Assert.IsTrue(SimilarColor(ColorToRGB(clBtnFace), clBtnFace, 0), 'A system color must equal its RGB value (second argument)');
end;


{ ComplementaryColor Tests }

procedure TTestGraphUtil.TestComplementaryColor_Black;
var
  Result: TColor;
begin
  Result:= ComplementaryColor(clBlack);
  Assert.AreEqual(TColor(clWhite), Result, 'Complement of black should be white');
end;


procedure TTestGraphUtil.TestComplementaryColor_White;
var
  Result: TColor;
begin
  Result:= ComplementaryColor(clWhite);
  Assert.AreEqual(TColor(clBlack), Result, 'Complement of white should be black');
end;


procedure TTestGraphUtil.TestComplementaryColor_Red;
var
  Result: TColor;
begin
  Result:= ComplementaryColor(clRed);
  Assert.AreEqual(TColor(clAqua), Result, 'Complement of red should be cyan (aqua)');
end;


{ ColorToHtml Tests }

procedure TTestGraphUtil.TestColorToHtml_Black;
var
  Result: string;
begin
  Result:= ColorToHtml(clBlack);
  Assert.AreEqual('000000', Result, 'Black should convert to 000000');
end;


procedure TTestGraphUtil.TestColorToHtml_White;
var
  Result: string;
begin
  Result:= ColorToHtml(clWhite);
  Assert.AreEqual('FFFFFF', Result, 'White should convert to FFFFFF');
end;


procedure TTestGraphUtil.TestColorToHtml_Red;
var
  Result: string;
begin
  Result:= ColorToHtml(clRed);
  Assert.AreEqual('FF0000', Result, 'Red should convert to FF0000');
end;


{ HtmlToColor Tests }

procedure TTestGraphUtil.TestHtmlToColor_WithHash;
var
  Result: TColor;
begin
  Result:= HtmlToColor('#FF0000');
  Assert.AreEqual(TColor(clRed), Result, '#FF0000 should convert to clRed');
end;


procedure TTestGraphUtil.TestHtmlToColor_WithoutHash;
var
  Result: TColor;
begin
  Result:= HtmlToColor('FF0000');
  Assert.AreEqual(TColor(clRed), Result, 'FF0000 should convert to clRed');
end;


procedure TTestGraphUtil.TestHtmlToColor_Black;
var
  Result: TColor;
begin
  Result:= HtmlToColor('000000');
  Assert.AreEqual(TColor(clBlack), Result, '000000 should convert to clBlack');
end;


procedure TTestGraphUtil.TestHtmlToColor_White;
var
  Result: TColor;
begin
  Result:= HtmlToColor('FFFFFF');
  Assert.AreEqual(TColor(clWhite), Result, 'FFFFFF should convert to clWhite');
end;


procedure TTestGraphUtil.TestHtmlToColor_RoundTrip;
var
  Original: TColor;
  Html: string;
  Result: TColor;
begin
  Original:= RGB(171, 205, 239);
  Html:= ColorToHtml(Original);
  Result:= HtmlToColor(Html);
  Assert.AreEqual(Original, Result, 'Round trip should preserve color');
end;


{ SplitColor2RGB Tests }

procedure TTestGraphUtil.TestSplitColor2RGB_Red;
var
  R, G, B: Byte;
begin
  SplitColor2RGB(clRed, R, G, B);
  Assert.AreEqual(Byte(255), R, 'Red channel should be 255');
  Assert.AreEqual(Byte(0), G, 'Green channel should be 0');
  Assert.AreEqual(Byte(0), B, 'Blue channel should be 0');
end;


procedure TTestGraphUtil.TestSplitColor2RGB_Green;
var
  R, G, B: Byte;
begin
  SplitColor2RGB(clLime, R, G, B);
  Assert.AreEqual(Byte(0), R, 'Red channel should be 0');
  Assert.AreEqual(Byte(255), G, 'Green channel should be 255');
  Assert.AreEqual(Byte(0), B, 'Blue channel should be 0');
end;


procedure TTestGraphUtil.TestSplitColor2RGB_Blue;
var
  R, G, B: Byte;
begin
  SplitColor2RGB(clBlue, R, G, B);
  Assert.AreEqual(Byte(0), R, 'Red channel should be 0');
  Assert.AreEqual(Byte(0), G, 'Green channel should be 0');
  Assert.AreEqual(Byte(255), B, 'Blue channel should be 255');
end;


procedure TTestGraphUtil.TestSplitColor2RGB_White;
var
  R, G, B: Byte;
begin
  SplitColor2RGB(clWhite, R, G, B);
  Assert.AreEqual(Byte(255), R, 'Red channel should be 255');
  Assert.AreEqual(Byte(255), G, 'Green channel should be 255');
  Assert.AreEqual(Byte(255), B, 'Blue channel should be 255');
end;


{ Integer2Color Tests }

procedure TTestGraphUtil.TestInteger2Color_Zero;
var
  Result: TColor;
begin
  Result:= Integer2Color(0);
  Assert.AreEqual(TColor(clBlack), Result, '0 should give black');
end;


procedure TTestGraphUtil.TestInteger2Color_255;
var
  Result: TColor;
begin
  Result:= Integer2Color(255);
  Assert.AreEqual(TColor(clBlue), Result, '255 should give blue');
end;


procedure TTestGraphUtil.TestInteger2Color_65535;
var
  Result: TColor;
  R, G, B: Byte;
begin
  Result:= Integer2Color(65535);
  R:= GetRValue(Result);
  G:= GetGValue(Result);
  B:= GetBValue(Result);
  Assert.AreEqual(Byte(0), R, 'Red should be 0');
  Assert.AreEqual(Byte(255), G, 'Green should be 255');
  Assert.AreEqual(Byte(255), B, 'Blue should be 255');
end;


procedure TTestGraphUtil.TestInteger2Color_Negative;
var
  Result: TColor;
begin
  Result:= Integer2Color(-10);
  Assert.AreEqual(TColor(clBlack), Result, 'Negative should give black');
end;


{ BlendColors Tests }

{ BlendColors turns 50% into A = Round(2.55 * 50), which is 127 or 128 depending on the floating-point precision.
  Each channel = Color2 + A * (Color1 - Color2) div 255. With an ODD difference d below 255, 127*d div 255 and 128*d div 255
  are the same number, so these colours give one exact answer for both values of A:
    R: 1   + 199 * A div 255 = 1   + 99  = 100
    G: 31  + 69  * A div 255 = 31  + 34  = 65
    B: 241 - 231 * A div 255 = 241 - 115 = 126 }
procedure TTestGraphUtil.TestBlendColors_50Percent;
begin
  AssertRGB(BlendColors(RGB(200, 100, 10), RGB(1, 31, 241), 50), 100, 65, 126, '50% blend of each channel');
end;


procedure TTestGraphUtil.TestBlendColors_0Percent;
var
  Result: TColor;
begin
  Result:= BlendColors(clRed, clBlue, 0);
  { 0% should give Color2 (second color) }
  Assert.AreEqual(TColor(clBlue), Result, '0% should give second color');
end;


procedure TTestGraphUtil.TestBlendColors_100Percent;
var
  Result: TColor;
begin
  Result:= BlendColors(clRed, clBlue, 100);
  { 100% should give Color1 (first color) }
  Assert.AreEqual(TColor(clRed), Result, '100% should give first color');
end;


{ MixColors Tests }

procedure TTestGraphUtil.TestMixColors_0_FullyFG;
var
  Result: TColor;
begin
  Result:= MixColors(clRed, clBlue, 0);
  { BlendPower 0 should give fully FG (first color) }
  Assert.AreEqual(TColor(clBlue), Result, 'BlendPower 0 should give BG (second) color');
end;


procedure TTestGraphUtil.TestMixColors_255_FullyBG;
var
  Result: TColor;
  R, B: Byte;
begin
  Result:= MixColors(clRed, clBlue, 255);
  R:= GetRValue(Result);
  B:= GetBValue(Result);
  { BlendPower 255 should give mostly FG with a bit of BG }
  { The algorithm is: BlendPower * (v1 - v2) shr 8 + v2 }
  { For R: 255 * (255 - 0) shr 8 + 0 = 255 }
  { For B: 255 * (0 - 255) shr 8 + 255 = 0 }
  Assert.AreEqual(Byte(255), R, 'Red should be 255');
  Assert.AreEqual(Byte(0), B, 'Blue should be 0');
end;


{ Each channel = BG + 128 * (FG - BG) div 255 (div truncates towards zero):
    R: 0   + 128 * 200  div 255 = 0   + 100  = 100
    G: 50  + 128 * 50   div 255 = 50  + 25   = 75
    B: 250 + 128 * -240 div 255 = 250 - 120  = 130 }
procedure TTestGraphUtil.TestMixColors_128_MidBlend;
begin
  AssertRGB(MixColors(RGB(200, 100, 10), RGB(0, 50, 250), 128), 100, 75, 130, 'Mid blend of each channel');
end;


{ ReplaceColor Tests }

{ The match must be exact on all three channels: the neighbours differ from OldColor by 1 in one channel each and must stay }
procedure TTestGraphUtil.TestReplaceColor_BasicCall;
begin
  CreateSolidBitmap24(10, 10, 200, 100, 50);
  SetPixel24(1, 0, 201, 100, 50);
  SetPixel24(2, 0, 200, 101, 50);
  SetPixel24(3, 0, 200, 100, 51);

  ReplaceColor(FBitmap, RGB(200, 100, 50), RGB(10, 20, 30));

  AssertPixel24(0, 0, 10, 20, 30,   'The exact match must be replaced');
  AssertPixel24(9, 9, 10, 20, 30,   'The exact match must be replaced in the last row too');
  AssertPixel24(1, 0, 201, 100, 50, 'Red off by 1 must stay');
  AssertPixel24(2, 0, 200, 101, 50, 'Green off by 1 must stay');
  AssertPixel24(3, 0, 200, 100, 51, 'Blue off by 1 must stay');
end;


procedure TTestGraphUtil.TestReplaceColor_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      ReplaceColor(NIL, clRed, clBlue);
    end,
    EAssertionFailed,
    'Should raise EAssertionFailed');
end;


procedure TTestGraphUtil.TestReplaceColor_ReplacesCorrectly;
var
  Pixel: TColor;
begin
  CreateColorBitmap(10, 10, clRed);
  ReplaceColor(FBitmap, clRed, clBlue);
  Pixel:= FBitmap.Canvas.Pixels[5, 5];
  Assert.AreEqual(TColor(clBlue), Pixel, 'Red should be replaced with blue');
end;


procedure TTestGraphUtil.TestReplaceColor_NoMatchNoChange;
var
  Pixel: TColor;
begin
  CreateColorBitmap(10, 10, clRed);
  ReplaceColor(FBitmap, clGreen, clBlue);  { Looking for green, but image is red }
  Pixel:= FBitmap.Canvas.Pixels[5, 5];
  Assert.AreEqual(TColor(clRed), Pixel, 'Red should remain unchanged');
end;


{ ReplaceColor with Tolerance Tests }

{ A different tolerance per channel (10, 20, 30). Each pixel moves one channel just inside or just outside its own tolerance,
  so a routine that swaps the tolerances, or ignores one, fails }
procedure TTestGraphUtil.TestReplaceColorTolerance_BasicCall;
begin
  CreateSolidBitmap24(10, 10, 0, 0, 0);
  SetPixel24(0, 0, 209, 100, 50);    { R  9 away: inside 10 }
  SetPixel24(1, 0, 211, 100, 50);    { R 11 away: outside 10 }
  SetPixel24(2, 0, 200, 119, 50);    { G 19 away: inside 20 }
  SetPixel24(3, 0, 200, 121, 50);    { G 21 away: outside 20 }
  SetPixel24(4, 0, 200, 100, 79);    { B 29 away: inside 30 }
  SetPixel24(5, 0, 200, 100, 81);    { B 31 away: outside 30 }
  SetPixel24(6, 0, 191, 81, 21);     { all three inside, below OldColor }

  ReplaceColor(FBitmap, RGB(200, 100, 50), RGB(10, 20, 30), 10, 20, 30);

  AssertPixel24(0, 0, 10, 20, 30,   'R inside its tolerance must be replaced');
  AssertPixel24(1, 0, 211, 100, 50, 'R outside its tolerance must stay');
  AssertPixel24(2, 0, 10, 20, 30,   'G inside its tolerance must be replaced');
  AssertPixel24(3, 0, 200, 121, 50, 'G outside its tolerance must stay');
  AssertPixel24(4, 0, 10, 20, 30,   'B inside its tolerance must be replaced');
  AssertPixel24(5, 0, 200, 100, 81, 'B outside its tolerance must stay');
  AssertPixel24(6, 0, 10, 20, 30,   'A pixel below OldColor, inside all three tolerances, must be replaced');
  AssertPixel24(9, 9, 0, 0, 0,      'Black is far from OldColor and must stay');
end;


procedure TTestGraphUtil.TestReplaceColorTolerance_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      ReplaceColor(NIL, clRed, clBlue, 10, 10, 10);
    end,
    EAssertionFailed,
    'Should raise EAssertionFailed');
end;


procedure TTestGraphUtil.TestReplaceColorTolerance_WithinTolerance;
var
  Pixel: TColor;
begin
  { Create bitmap with almost-red color }
  CreateSolidBitmap24(10, 10, 250, 5, 5);
  { Replace with tolerance - should match }
  ReplaceColor(FBitmap, clRed, clBlue, 10, 10, 10);
  Pixel:= FBitmap.Canvas.Pixels[5, 5];
  Assert.AreEqual(TColor(clBlue), Pixel, 'Near-red should be replaced with blue');
end;


{ GetAverageColor Tests }

procedure TTestGraphUtil.TestGetAverageColor_AllBlack;
var
  Result: TColor;
begin
  CreateSolidBitmap24(10, 10, 0, 0, 0);
  Result:= GetAverageColor(FBitmap, False);
  Assert.AreEqual(TColor(clBlack), Result, 'Average of all black should be black');
end;


procedure TTestGraphUtil.TestGetAverageColor_AllWhite;
var
  Result: TColor;
begin
  CreateSolidBitmap24(10, 10, 255, 255, 255);
  Result:= GetAverageColor(FBitmap, False);
  Assert.AreEqual(TColor(clWhite), Result, 'Average of all white should be white');
end;


procedure TTestGraphUtil.TestGetAverageColor_AllRed;
var
  Result: TColor;
begin
  CreateSolidBitmap24(10, 10, 255, 0, 0);
  Result:= GetAverageColor(FBitmap, False);
  Assert.AreEqual(TColor(clRed), Result, 'Average of all red should be red');
end;


procedure TTestGraphUtil.TestGetAverageColor_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      GetAverageColor(NIL, False);
    end,
    EAssertionFailed,
    'Should raise EAssertionFailed');
end;


procedure TTestGraphUtil.TestGetAverageColor_EmptyBitmap;
var
  Result: TColor;
begin
  FBitmap.SetSize(0, 0);
  FBitmap.PixelFormat:= pf24bit;
  Result:= GetAverageColor(FBitmap, False);
  Assert.AreEqual(TColor(0), Result, 'Empty bitmap should return 0');
end;


{ Rows 1 and 3 hold a colour, rows 0 and 2 are black. Fast mode reads only the odd rows (the routine's header), so it sees only the colour;
  the normal mode averages all four rows, so it gets half of each channel }
procedure TTestGraphUtil.TestGetAverageColor_FastMode;
var
  Col: Integer;
begin
  CreateSolidBitmap24(10, 4, 0, 0, 0);
  for Col:= 0 to 9 do
  begin
    SetPixel24(Col, 1, 200, 100, 50);
    SetPixel24(Col, 3, 200, 100, 50);
  end;

  AssertRGB(GetAverageColor(FBitmap, True),  200, 100, 50, 'Fast mode must average only the odd rows');
  AssertRGB(GetAverageColor(FBitmap, False), 100, 50,  25, 'Normal mode must average all rows');
end;


{ GetAverageColorPf8 Tests }

procedure TTestGraphUtil.TestGetAverageColorPf8_AllBlack;
var
  Row, Col: Integer;
  Line: PByte;
  Avg: Byte;
begin
  FBitmap.SetSize(10, 10);
  FBitmap.PixelFormat:= pf8bit;
  for Row:= 0 to 9 do
  begin
    Line:= FBitmap.ScanLine[Row];
    for Col:= 0 to 9 do
      Line[Col]:= 0;
  end;

  Avg:= GetAverageColorPf8(FBitmap);
  Assert.AreEqual(Byte(0), Avg, 'Average of all black should be 0');
end;


procedure TTestGraphUtil.TestGetAverageColorPf8_AllWhite;
var
  Row, Col: Integer;
  Line: PByte;
  Avg: Byte;
begin
  FBitmap.SetSize(10, 10);
  FBitmap.PixelFormat:= pf8bit;
  for Row:= 0 to 9 do
  begin
    Line:= FBitmap.ScanLine[Row];
    for Col:= 0 to 9 do
      Line[Col]:= 255;
  end;

  Avg:= GetAverageColorPf8(FBitmap);
  Assert.AreEqual(Byte(255), Avg, 'Average of all white should be 255');
end;


procedure TTestGraphUtil.TestGetAverageColorPf8_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      GetAverageColorPf8(NIL);
    end,
    EAssertionFailed,
    'Should raise EAssertionFailed');
end;


procedure TTestGraphUtil.TestGetAverageColorPf8_EmptyBitmap;
var
  Avg: Byte;
begin
  FBitmap.SetSize(0, 0);
  FBitmap.PixelFormat:= pf8bit;
  Avg:= GetAverageColorPf8(FBitmap);
  Assert.AreEqual(Byte(0), Avg, 'Empty bitmap should return 0');
end;


{ GetDeviceColorDepth Tests }

{ The screen's bits per pixel, read through a second API (EnumDisplaySettings) that the routine does not use }
procedure TTestGraphUtil.TestGetDeviceColorDepth_ReturnsPositive;
const
  ENUM_CURRENT_SETTINGS = DWORD(-1);   { Not declared in Winapi.Windows; same value as C:\Projects\LightSaber\External\MonitorHelper.pas }
var
  Depth: Integer;
  DevMode: TDeviceMode;
begin
  Depth:= GetDeviceColorDepth;

  FillChar(DevMode, SizeOf(DevMode), 0);
  DevMode.dmSize:= SizeOf(DevMode);
  Assert.IsTrue(EnumDisplaySettings(NIL, ENUM_CURRENT_SETTINGS, DevMode), 'EnumDisplaySettings must read the current display mode');
  Assert.AreEqual(Integer(DevMode.dmBitsPerPel), Depth, 'The colour depth must match the current display mode');
end;


{ Theme Functions Tests }

{ The test EXE loads no VCL style, so the active style is the plain Windows one (TUxThemeStyle): not custom,
  and its GetSystemColor returns ColorToRGB of the colour asked for (c:\Delphi\Delphi 13\source\vcl\Vcl.Themes.pas, TUxThemeStyle.DoGetSystemColor).
  WindowsThemesEnabled has no test: its body is Result:= TStyleManager.Enabled, which is FALSE in this test EXE
  (TUxThemeStyle.GetEnabled needs comctl32 v6 and active themes), so a broken body returning FALSE would pass,
  and making it TRUE means changing the global VCL style for every other test. }
procedure TTestGraphUtil.TestVclStylesEnabled_NoCrash;
begin
  Assert.IsFalse(VclStylesEnabled, 'The test EXE loads no custom VCL style');
end;


procedure TTestGraphUtil.TestThemeColorBkg_NoCrash;
begin
  Assert.AreEqual(Integer(ColorToRGB(clBackground)), Integer(ThemeColorBkg), 'Windows style: ThemeColorBkg must be the RGB of clBackground');
end;


procedure TTestGraphUtil.TestThemeColorHilight_NoCrash;
begin
  Assert.AreEqual(Integer(ColorToRGB(clHighlight)), Integer(ThemeColorHilight), 'Windows style: ThemeColorHilight must be the RGB of clHighlight');
end;


procedure TTestGraphUtil.TestThemeColorButtonFace_NoCrash;
begin
  Assert.AreEqual(Integer(ColorToRGB(clBtnFace)), Integer(ThemeColorButtonFace), 'Windows style: ThemeColorButtonFace must be the RGB of clBtnFace');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestGraphUtil);

end.
