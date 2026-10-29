unit Test.LightVcl.Visual.SvgSpeedButton;

{=============================================================================================================
   2026.10.29
   Unit tests for LightVcl.Visual.SvgSpeedButton.pas
   RenderSvgGlyph draws an SVG into a 32-bit premultiplied bitmap with GDI+; the tests read its pixels.
   The SVG texts are the icons of the BioniX mini-monitor taskbar (viewBox 0 0 24 24), so at Size = 24 one
   bitmap pixel is one viewBox unit and the expected pixels can be read straight from the SVG numbers.
   No form tests: the button is never shown.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  LightCore.SvgShapes,
  LightVcl.Visual.SvgSpeedButton;

type
  [TestFixture]
  TTestSvgSpeedButton = class
  public
    [Test]
    procedure Test_RenderGlyph_HasRequestedSizeAndAlpha;
    [Test]
    procedure Test_RenderGlyph_PauseBarsOpaqueGapTransparent;
    [Test]
    procedure Test_RenderGlyph_At1x15x2x_EdgesStayOnePixelWide;
    [Test]
    procedure Test_RenderGlyph_StrokeOnlyCircleLeavesInsideEmpty;
    [Test]
    procedure Test_RenderGlyph_FillRuleNonZeroAndEvenOdd;
    [Test]
    procedure Test_ContrastRatio_KnownValues;
    [Test]
    procedure Test_BestContrastColor_WhiteOnDarkNearBlackOnLight;
    [Test]
    procedure Test_GlyphBitmap_RebuiltWhenSizeOrColorChanges;
    [Test]
    procedure Test_GlyphBitmap_HotAndPressedUseTheirColors;
    [Test]
    procedure Test_SvgText_InvalidTextRaises;
  end;

implementation

CONST
  SvgPause =
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">' +
    '<rect x="5" y="4.5" width="5" height="15" rx="1"/><rect x="14" y="4.5" width="5" height="15" rx="1"/></svg>';

  SvgStopwatch =
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">' +
    '<rect x="9" y="0.8" width="6" height="2.6" rx="1"/><rect x="10.9" y="3" width="2.2" height="3"/>' +
    '<circle cx="12" cy="14" r="7.4" fill="none" stroke="#000" stroke-width="2.6"/>' +
    '<rect x="10.9" y="9" width="2.2" height="6" rx="1.1"/>' +
    '<rect x="17.3" y="4.2" width="3" height="3" rx="0.7" transform="rotate(45 18.8 5.7)"/></svg>';

  { Two squares drawn in the same direction, overlapping in 8..14 }
  SvgOverlapNonZero =
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">' +
    '<path d="M2 2 H14 V14 H2 Z M8 8 H22 V22 H8 Z"/></svg>';
  SvgOverlapEvenOdd =
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">' +
    '<path fill-rule="evenodd" d="M2 2 H14 V14 H2 Z M8 8 H22 V22 H8 Z"/></svg>';

  TestColor = TColor($00336699);   { R = $99, G = $66, B = $33 }


{ Byte Index of the pixel: 0 = blue, 1 = green, 2 = red, 3 = alpha (premultiplied BGRA, as a 32-bit DIB stores it) }
function PixelByte(Bmp: TBitmap; X, Y, Index: Integer): Byte;
begin
  Result:= PByteArray(Bmp.ScanLine[Y])[X * 4 + Index];
end;


function Alpha(Bmp: TBitmap; X, Y: Integer): Byte;
begin
  Result:= PixelByte(Bmp, X, Y, 3);
end;


procedure TTestSvgSpeedButton.Test_RenderGlyph_HasRequestedSizeAndAlpha;
VAR Bmp: TBitmap;
begin
  Bmp:= RenderSvgGlyph(ParseSvg(SvgPause), 30, TestColor);
  try
    Assert.AreEqual(30, Bmp.Width,  'Width');
    Assert.AreEqual(30, Bmp.Height, 'Height');
    Assert.IsTrue(Bmp.PixelFormat = pf32bit, 'PixelFormat must be pf32bit');
    Assert.IsTrue(Bmp.AlphaFormat = afPremultiplied, 'AlphaFormat must be afPremultiplied, so Canvas.Draw blends it');
  finally
    FreeAndNil(Bmp);
  end;
end;


procedure TTestSvgSpeedButton.Test_RenderGlyph_PauseBarsOpaqueGapTransparent;
VAR Bmp: TBitmap;
begin
  Bmp:= RenderSvgGlyph(ParseSvg(SvgPause), 24, TestColor);
  try
    { Left bar covers x 5..10, y 4.5..19.5 }
    Assert.AreEqual(Byte(255), Alpha(Bmp, 7, 12), 'Inside the left bar: opaque');
    Assert.AreEqual(Byte($33), PixelByte(Bmp, 7, 12, 0), 'Inside the left bar: blue channel of TestColor');
    Assert.AreEqual(Byte($66), PixelByte(Bmp, 7, 12, 1), 'Inside the left bar: green channel of TestColor');
    Assert.AreEqual(Byte($99), PixelByte(Bmp, 7, 12, 2), 'Inside the left bar: red channel of TestColor');
    Assert.AreEqual(Byte(255), Alpha(Bmp, 16, 12), 'Inside the right bar: opaque');
    Assert.AreEqual(Byte(0),   Alpha(Bmp, 12, 12), 'Gap between the bars: transparent');
    Assert.AreEqual(Byte(0),   Alpha(Bmp, 2, 12),  'Left of the bars: transparent');
    Assert.AreEqual(Byte(0),   Alpha(Bmp, 7, 1),   'Above the bars: transparent');
  finally
    FreeAndNil(Bmp);
  end;
end;


{ A glyph drawn again at each size has an anti-aliased fringe of at most one pixel per edge.
  A small bitmap stretched to the bigger size would smear each edge over two or more pixels. }
procedure TTestSvgSpeedButton.Test_RenderGlyph_At1x15x2x_EdgesStayOnePixelWide;
CONST
  Sizes: array[0..2] of Integer = (24, 36, 48);
VAR
  Bmp: TBitmap;
  i, x, Partial, Opaque, Clear, Size: Integer;
  A: Byte;
begin
  for i:= Low(Sizes) to High(Sizes) do
    begin
      Size:= Sizes[i];
      Bmp:= RenderSvgGlyph(ParseSvg(SvgPause), Size, TestColor);
      try
        Partial:= 0;
        Opaque:= 0;
        Clear:= 0;
        for x:= 0 to Size - 1 do
          begin
            A:= Alpha(Bmp, x, Size DIV 2);
            if A = 255 then Inc(Opaque)
            else
            if A > 0
            then Inc(Partial)
            else Inc(Clear);
          end;
        { 14 of the 24 units are outside the bars }
        Assert.IsTrue(Clear >= Round(14 * Size / 24) - 2, 'Size ' + IntToStr(Size) + ': only ' + IntToStr(Clear) + ' transparent pixels on the middle row');
        { 2 bars x 2 vertical edges: at most one partly covered pixel each }
        Assert.IsTrue(Partial <= 4, 'Size ' + IntToStr(Size) + ': ' + IntToStr(Partial) + ' partly covered pixels on the middle row, expected at most 4');
        { The two bars are 5 units wide each }
        Assert.IsTrue(Opaque >= Round(10 * Size / 24) - 2, 'Size ' + IntToStr(Size) + ': only ' + IntToStr(Opaque) + ' opaque pixels on the middle row');
      finally
        FreeAndNil(Bmp);
      end;
    end;
end;


procedure TTestSvgSpeedButton.Test_RenderGlyph_StrokeOnlyCircleLeavesInsideEmpty;
VAR Bmp: TBitmap;
begin
  Bmp:= RenderSvgGlyph(ParseSvg(SvgStopwatch), 24, TestColor);
  try
    { Ring: centre (12, 14), radius 7.4, stroke 2.6, so it covers the distances 6.1 .. 8.7 }
    Assert.AreEqual(Byte(255), Alpha(Bmp, 19, 14), 'On the ring: opaque');
    Assert.AreEqual(Byte(0),   Alpha(Bmp, 8, 14),  'Inside the ring (fill="none"), away from the hand: transparent');
    Assert.AreEqual(Byte(255), Alpha(Bmp, 11, 11), 'On the hand (rect 10.9..13.1 x 9..15): opaque');
  finally
    FreeAndNil(Bmp);
  end;
end;


procedure TTestSvgSpeedButton.Test_RenderGlyph_FillRuleNonZeroAndEvenOdd;
VAR Bmp: TBitmap;
begin
  Bmp:= RenderSvgGlyph(ParseSvg(SvgOverlapNonZero), 24, TestColor);
  try
    Assert.AreEqual(Byte(255), Alpha(Bmp, 11, 11), 'nonzero (the SVG default): the overlap of two same-direction squares is filled');
    Assert.AreEqual(Byte(255), Alpha(Bmp, 4, 4),   'nonzero: first square filled');
  finally
    FreeAndNil(Bmp);
  end;

  Bmp:= RenderSvgGlyph(ParseSvg(SvgOverlapEvenOdd), 24, TestColor);
  try
    Assert.AreEqual(Byte(0),   Alpha(Bmp, 11, 11), 'evenodd: the overlap is a hole');
    Assert.AreEqual(Byte(255), Alpha(Bmp, 4, 4),   'evenodd: first square filled');
  finally
    FreeAndNil(Bmp);
  end;
end;


procedure TTestSvgSpeedButton.Test_ContrastRatio_KnownValues;
begin
  { WCAG 2: (1.0 + 0.05) / (0.0 + 0.05) = 21 }
  Assert.AreEqual(Double(21), ContrastRatio(clWhite, clBlack), 0.001, 'White on black');
  Assert.AreEqual(Double(21), ContrastRatio(clBlack, clWhite), 0.001, 'Order does not matter');
  Assert.AreEqual(Double(1),  ContrastRatio(TColor($585858), TColor($585858)), 0.001, 'Same colour');
end;


procedure TTestSvgSpeedButton.Test_BestContrastColor_WhiteOnDarkNearBlackOnLight;
begin
  Assert.AreEqual(Integer(clWhite),         Integer(BestContrastColor(TColor($585858))), 'Auric bar #585858: white');
  Assert.AreEqual(Integer(TColor($202020)), Integer(BestContrastColor(TColor($0075D9FF))), 'Obsidian hot background #FFD975: near-black');
  Assert.AreEqual(Integer(TColor($202020)), Integer(BestContrastColor(TColor($F0F0F0))), 'Windows bar #F0F0F0: near-black');
end;


procedure TTestSvgSpeedButton.Test_GlyphBitmap_RebuiltWhenSizeOrColorChanges;
VAR Btn: TLightSvgSpeedButton;
begin
  Btn:= TLightSvgSpeedButton.Create(NIL);
  try
    Btn.SetBounds(0, 0, 50, 40);
    Btn.SvgText:= SvgPause;
    Assert.AreEqual(26, Btn.ActualGlyphSize, 'GlyphSize 0: 64% of the smaller side (40 * 0.64 = 25.6)');

    Btn.GlyphSize:= 24;
    Btn.GlyphColor:= TestColor;
    Assert.AreEqual(24, Btn.GlyphBitmap(gsNormal).Width, 'Glyph at GlyphSize 24');
    Assert.AreEqual(Byte($99), PixelByte(Btn.GlyphBitmap(gsNormal), 7, 12, 2), 'Normal glyph in GlyphColor');

    Btn.GlyphSize:= 36;
    Assert.AreEqual(36, Btn.GlyphBitmap(gsNormal).Width, 'The cache is rebuilt for the new size');

    Btn.GlyphSize:= 24;
    Btn.GlyphColor:= clRed;
    Assert.AreEqual(Byte(255), PixelByte(Btn.GlyphBitmap(gsNormal), 7, 12, 2), 'The cache is rebuilt for the new colour: red');
    Assert.AreEqual(Byte(0),   PixelByte(Btn.GlyphBitmap(gsNormal), 7, 12, 1), 'The cache is rebuilt for the new colour: no green');
  finally
    FreeAndNil(Btn);
  end;
end;


procedure TTestSvgSpeedButton.Test_GlyphBitmap_HotAndPressedUseTheirColors;
VAR Btn: TLightSvgSpeedButton;
begin
  Btn:= TLightSvgSpeedButton.Create(NIL);
  try
    Btn.SetBounds(0, 0, 40, 40);
    Btn.SvgText:= SvgPause;
    Btn.GlyphSize:= 24;
    Btn.GlyphColor  := clBlack;
    Btn.HotColor    := clLime;
    Btn.PressedColor:= clBlue;
    Assert.AreEqual(Byte(255), PixelByte(Btn.GlyphBitmap(gsHot), 7, 12, 1),     'Hot glyph in HotColor (green)');
    Assert.AreEqual(Byte(0),   PixelByte(Btn.GlyphBitmap(gsHot), 7, 12, 2),     'Hot glyph has no red');
    Assert.AreEqual(Byte(255), PixelByte(Btn.GlyphBitmap(gsPressed), 7, 12, 0), 'Pressed glyph in PressedColor (blue)');
    Assert.AreEqual(Byte(0),   PixelByte(Btn.GlyphBitmap(gsNormal), 7, 12, 1),  'Normal glyph stays black');
    Assert.AreEqual(Byte(255), Alpha(Btn.GlyphBitmap(gsNormal), 7, 12),         'Normal glyph is opaque');
  finally
    FreeAndNil(Btn);
  end;
end;


procedure TTestSvgSpeedButton.Test_SvgText_InvalidTextRaises;
VAR Btn: TLightSvgSpeedButton;
begin
  Btn:= TLightSvgSpeedButton.Create(NIL);
  try
    Assert.WillRaiseDescendant(
      procedure
      begin
        Btn.SvgText:= '<svg viewBox="0 0 24 24"><path d="M1 1 A 2 2 0 0 1 5 5"/></svg>';
      end,
      ESvgError, 'An SVG the reader cannot draw must raise when it is assigned, not when the button paints');
  finally
    FreeAndNil(Btn);
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestSvgSpeedButton);
end.
