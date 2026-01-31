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
  Vcl.Graphics;

type
  [TestFixture]
  TTestGraphUtil = class
  private
    FBitmap: TBitmap;
    procedure CreateColorBitmap(Width, Height: Integer; Color: TColor);
    procedure CreateSolidBitmap24(Width, Height: Integer; R, G, B: Byte);
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
    procedure TestWindowsThemesEnabled_NoCrash;

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
var
  Row, Col: Integer;
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
var
  Result: TColor;
  R: Byte;
begin
  Result:= ChangeBrightness(RGB(100, 100, 100), 50);
  R:= GetRValue(Result);
  Assert.AreEqual(Byte(150), R, 'Brightness should increase by 50');
end;


procedure TTestGraphUtil.TestChangeBrightness_Decrease;
var
  Result: TColor;
  R: Byte;
begin
  Result:= ChangeBrightness(RGB(100, 100, 100), -50);
  R:= GetRValue(Result);
  Assert.AreEqual(Byte(50), R, 'Brightness should decrease by 50');
end;


procedure TTestGraphUtil.TestChangeBrightness_ClampMax;
var
  Result: TColor;
  R: Byte;
begin
  Result:= ChangeBrightness(RGB(200, 200, 200), 100);
  R:= GetRValue(Result);
  Assert.AreEqual(Byte(255), R, 'Brightness should clamp at 255');
end;


procedure TTestGraphUtil.TestChangeBrightness_ClampMin;
var
  Result: TColor;
  R: Byte;
begin
  Result:= ChangeBrightness(RGB(50, 50, 50), -100);
  R:= GetRValue(Result);
  Assert.AreEqual(Byte(0), R, 'Brightness should clamp at 0');
end;


{ ChangeColor Tests }

procedure TTestGraphUtil.TestChangeColor_TowardsWhite;
var
  Result: TColor;
  R: Byte;
begin
  Result:= ChangeColor(clBlack, clWhite, 50);
  R:= GetRValue(Result);
  { Black towards white at 50% should give mid-gray }
  Assert.IsTrue((R >= 127) and (R <= 128), 'Should move towards white');
end;


procedure TTestGraphUtil.TestChangeColor_TowardsBlack;
var
  Result: TColor;
  R: Byte;
begin
  Result:= ChangeColor(clWhite, clBlack, 50);
  R:= GetRValue(Result);
  { White towards black at 50% should give mid-gray }
  Assert.IsTrue((R >= 127) and (R <= 128), 'Should move towards black');
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
  Assert.IsFalse(SimilarColor(RGB(100, 100, 100), RGB(120, 100, 100), 10), 'Colors outside tolerance should not be similar');
end;


procedure TTestGraphUtil.TestSimilarColor_SystemColors;
begin
  { clWindow is a system color - test that it gets converted properly }
  Assert.IsTrue(SimilarColor(clWindow, clWindow, 0), 'System colors should compare correctly');
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

procedure TTestGraphUtil.TestBlendColors_50Percent;
var
  Result: TColor;
  R: Byte;
begin
  Result:= BlendColors(clBlack, clWhite, 50);
  R:= GetRValue(Result);
  { 50% blend should give mid-gray }
  Assert.IsTrue((R >= 127) and (R <= 128), '50% blend should give mid-gray');
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


procedure TTestGraphUtil.TestMixColors_128_MidBlend;
var
  Result: TColor;
  R, B: Byte;
begin
  Result:= MixColors(clRed, clBlue, 128);
  R:= GetRValue(Result);
  B:= GetBValue(Result);
  { Mid blend should give balanced result }
  Assert.IsTrue(R > 100, 'Red should be significant');
  Assert.IsTrue(B > 100, 'Blue should be significant');
end;


{ ReplaceColor Tests }

procedure TTestGraphUtil.TestReplaceColor_BasicCall;
begin
  CreateColorBitmap(10, 10, clRed);
  Assert.WillNotRaise(
    procedure
    begin
      ReplaceColor(FBitmap, clRed, clBlue);
    end);
end;


procedure TTestGraphUtil.TestReplaceColor_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      ReplaceColor(NIL, clRed, clBlue);
    end,
    EAssertionFailed);
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

procedure TTestGraphUtil.TestReplaceColorTolerance_BasicCall;
begin
  CreateColorBitmap(10, 10, clRed);
  Assert.WillNotRaise(
    procedure
    begin
      ReplaceColor(FBitmap, clRed, clBlue, 10, 10, 10);
    end);
end;


procedure TTestGraphUtil.TestReplaceColorTolerance_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      ReplaceColor(NIL, clRed, clBlue, 10, 10, 10);
    end,
    EAssertionFailed);
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
    EAssertionFailed);
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


procedure TTestGraphUtil.TestGetAverageColor_FastMode;
var
  Result1, Result2: TColor;
begin
  { For uniform color, Fast mode should give same result as normal }
  CreateSolidBitmap24(10, 10, 128, 128, 128);
  Result1:= GetAverageColor(FBitmap, False);
  Result2:= GetAverageColor(FBitmap, True);
  Assert.AreEqual(Result1, Result2, 'Fast mode should give same result for uniform color');
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
    EAssertionFailed);
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

procedure TTestGraphUtil.TestGetDeviceColorDepth_ReturnsPositive;
var
  Depth: Integer;
begin
  Depth:= GetDeviceColorDepth;
  Assert.IsTrue(Depth > 0, 'Color depth should be positive');
  Assert.IsTrue(Depth >= 8, 'Color depth should be at least 8');
end;


{ Theme Functions Tests }

procedure TTestGraphUtil.TestWindowsThemesEnabled_NoCrash;
begin
  Assert.WillNotRaise(
    procedure
    begin
      WindowsThemesEnabled;
    end);
end;


procedure TTestGraphUtil.TestVclStylesEnabled_NoCrash;
begin
  Assert.WillNotRaise(
    procedure
    begin
      VclStylesEnabled;
    end);
end;


procedure TTestGraphUtil.TestThemeColorBkg_NoCrash;
begin
  Assert.WillNotRaise(
    procedure
    var
      C: TColor;
    begin
      C:= ThemeColorBkg;
    end);
end;


procedure TTestGraphUtil.TestThemeColorHilight_NoCrash;
begin
  Assert.WillNotRaise(
    procedure
    var
      C: TColor;
    begin
      C:= ThemeColorHilight;
    end);
end;


procedure TTestGraphUtil.TestThemeColorButtonFace_NoCrash;
begin
  Assert.WillNotRaise(
    procedure
    var
      C: TColor;
    begin
      C:= ThemeColorButtonFace;
    end);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestGraphUtil);

end.
