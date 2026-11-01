unit Test.LightVcl.Visual.SvgSpeedButton;

{=============================================================================================================
   2026.10.29
   Unit tests for LightVcl.Visual.SvgSpeedButton.pas
   RenderSvgGlyph draws an SVG into a 32-bit premultiplied bitmap with GDI+; the tests read its pixels.
   The SVG texts are the icons of the BioniX mini-monitor taskbar (viewBox 0 0 24 24), so at Size = 24 one
   bitmap pixel is one viewBox unit and the expected pixels can be read straight from the SVG numbers.
   No form tests: the button is never shown.
   The image-list tests draw each picture with TCustomImageList.Draw onto a white and a black bitmap and read
   the blended pixels, so they test what a menu or a TButton really gets.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.Math,
  Winapi.Windows,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ImgList,
  Vcl.StdCtrls,
  Vcl.Themes,
  LightCore.SvgShapes,
  LightVcl.Visual.SvgSpeedButton;

type
  [TestFixture]
  TTestSvgSpeedButton = class
  public
    [Test]
    procedure Test_RenderGlyph_HasRequestedSizeAndAlpha;
    [Test]
    procedure Test_RenderGlyph_NoOutlineIsByteIdentical;
    [Test]
    procedure Test_RenderGlyph_OutlineDarkensPixelJustOutside;
    [Test]
    procedure Test_SvgToImageList_OutlineReachesTheList;
    [Test]
    procedure Test_StyleMenuHighlightColor_NoStyleIsClHighlight;
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
    [Test]
    procedure Test_SvgToImageList_ReplaceKeepsCountAndDrawsOnWhiteAndBlack;
    [Test]
    procedure Test_SvgToImageList_EdgePixelIsStraightAlpha;
    [Test]
    procedure Test_SvgToImageList_AppendAtCount;
    [Test]
    procedure Test_SvgToImageList_NonSquareListIsCentred;
    [Test]
    procedure Test_SvgToImageList_BadIndexOrColorDepthRaises;
    [Test]
    procedure Test_SetButtonSvg_SecondCallReplacesPicture;
    [Test]
    procedure Test_SetButtonSvg_DisabledPictureHas40PercentAlpha;
    [Test]
    procedure Test_SetButtonSvg_ForeignImageListRaises;
    [Test]
    procedure Test_SetButtonSvg_OutlineInBothPictures;
    [Test]
    procedure Test_GlyphOutlineColor_OnlyWhiteGetsRim;
    [Test]
    procedure Test_BestContrastColorOn_WorstSurfaceDecides;
    [Test]
    procedure Test_StyleSurfaceColors_NoStyleAreSystemColors;
    [Test]
    procedure Test_StyleGlyphColor_NoStyleUsesBtnFace;
    [Test]
    procedure Test_ImageList_DisabledDraw_Measure;
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

  { viewBox 0 0 16 16, drawn into 16 x 16 image lists: one pixel per viewBox unit }
  SvgFull16   = '<svg viewBox="0 0 16 16"><rect x="0" y="0" width="16" height="16"/></svg>';
  SvgSquare16 = '<svg viewBox="0 0 16 16"><rect x="4" y="4" width="8" height="8"/></svg>';
  { Edges on half units: column 3 and column 12 are half covered, so their alpha is about 128 }
  SvgHalfEdge16 = '<svg viewBox="0 0 16 16"><rect x="3.5" y="3.5" width="9" height="9"/></svg>';

  { TColor values ($00BBGGRR) }
  RgbRed   = Integer($000000FF);
  RgbLime  = Integer($0000FF00);
  RgbBlue  = Integer($00FF0000);
  RgbWhite = Integer($00FFFFFF);
  RgbBlack = Integer($00000000);


{ Byte Index of the pixel: 0 = blue, 1 = green, 2 = red, 3 = alpha (premultiplied BGRA, as a 32-bit DIB stores it) }
function PixelByte(Bmp: TBitmap; X, Y, Index: Integer): Byte;
begin
  Result:= PByteArray(Bmp.ScanLine[Y])[X * 4 + Index];
end;


function Alpha(Bmp: TBitmap; X, Y: Integer): Byte;
begin
  Result:= PixelByte(Bmp, X, Y, 3);
end;


{ A cd32Bit image list with no pictures; the caller frees it }
function NewList32(Width, Height: Integer): TImageList;
begin
  Result:= TImageList.Create(NIL);
  Result.ColorDepth:= cd32Bit;
  Result.SetSize(Width, Height);
end;


{ Picture Index of List drawn with List.Draw onto a 24-bit bitmap filled with Background; the caller frees it }
function DrawOnto(List: TCustomImageList; Index: Integer; Background: TColor; Enabled: Boolean = TRUE): TBitmap;
begin
  Result:= TBitmap.Create;
  try
    Result.PixelFormat:= pf24bit;
    Result.SetSize(List.Width, List.Height);
    Result.Canvas.Brush.Color:= Background;
    Result.Canvas.FillRect(Rect(0, 0, List.Width, List.Height));
    List.Draw(Result.Canvas, 0, 0, Index, Enabled);
  except
    FreeAndNil(Result);
    raise;
  end;
end;


{ The pixel as $00BBGGRR }
function PixelRGB(Bmp: TBitmap; X, Y: Integer): Integer;
begin
  Result:= Integer(ColorToRGB(Bmp.Canvas.Pixels[X, Y]));
end;


{ The centre pixel of picture Index drawn onto Background }
function DrawnCentre(List: TCustomImageList; Index: Integer; Background: TColor; Enabled: Boolean = TRUE): Integer;
VAR Bmp: TBitmap;
begin
  Bmp:= DrawOnto(List, Index, Background, Enabled);
  try
    Result:= PixelRGB(Bmp, List.Width DIV 2, List.Height DIV 2);
  finally
    FreeAndNil(Bmp);
  end;
end;


{ The largest difference of one colour channel between C1 and C2 ($00BBGGRR) }
function ChannelDistance(C1, C2: Integer): Integer;
begin
  Result:= Abs(GetRValue(Cardinal(C1)) - GetRValue(Cardinal(C2)));
  Result:= Max(Result, Abs(GetGValue(Cardinal(C1)) - GetGValue(Cardinal(C2))));
  Result:= Max(Result, Abs(GetBValue(Cardinal(C1)) - GetBValue(Cardinal(C2))));
end;


{ FNV-1a hash of every pixel byte of a 32-bit bitmap }
function PixelHash(Bmp: TBitmap): Cardinal;
VAR
  x, y: Integer;
  Row: PByteArray;
begin
  Result:= 2166136261;
  for y:= 0 to Bmp.Height - 1 do
    begin
      Row:= Bmp.ScanLine[y];
      for x:= 0 to Bmp.Width * 4 - 1 do
        Result:= Cardinal(UInt64(Result XOR Row[x]) * 16777619);
    end;
end;


function RenderHash(CONST SvgText: string; Size: Integer; OutlineColor: TColor): Cardinal;
VAR Bmp: TBitmap;
begin
  Bmp:= RenderSvgGlyph(ParseSvg(SvgText), Size, TestColor, OutlineColor);
  try
    Result:= PixelHash(Bmp);
  finally
    FreeAndNil(Bmp);
  end;
end;


{ The hashes were taken from RenderSvgGlyph before it had the OutlineColor parameter }
procedure TTestSvgSpeedButton.Test_RenderGlyph_NoOutlineIsByteIdentical;
begin
  Assert.AreEqual(Cardinal($A09B4572), RenderHash(SvgStopwatch, 24, clNone),      'Stopwatch at 24');
  Assert.AreEqual(Cardinal($6C165C94), RenderHash(SvgPause, 36, clNone),          'Pause at 36');
  Assert.AreEqual(Cardinal($726AACE5), RenderHash(SvgOverlapEvenOdd, 48, clNone), 'Even-odd overlap at 48');
end;


{ The pause bars at 24 px: the left bar's left edge is at x = 5, so pixel 4 lies just outside the fill.
  With a 1.5 px outline the pen reaches 0.75 px outside the edge, so pixel 4 is partly covered in black. }
procedure TTestSvgSpeedButton.Test_RenderGlyph_OutlineDarkensPixelJustOutside;
VAR
  Plain, Outlined, OnWhite: TBitmap;
  OutsideRgb, InsideRgb: Cardinal;
begin
  Plain:= RenderSvgGlyph(ParseSvg(SvgPause), 24, clWhite);
  Outlined:= RenderSvgGlyph(ParseSvg(SvgPause), 24, clWhite, clBlack);
  OnWhite:= TBitmap.Create;
  try
    Assert.AreEqual(Byte(0), Alpha(Plain, 4, 12), 'Without outline: pixel 4 is outside the bar, transparent');
    Assert.IsTrue(Alpha(Outlined, 4, 12) > 100, 'With outline: pixel 4 is covered by the rim, alpha ' + IntToStr(Alpha(Outlined, 4, 12)));
    Assert.AreEqual(Byte(255), Alpha(Outlined, 7, 12), 'Inside the bar: opaque');
    Assert.AreEqual(Byte(255), PixelByte(Outlined, 7, 12, 2), 'Inside the bar the fill colour (white) covers the outline');

    { On a white background the rim pixel must be dark }
    OnWhite.PixelFormat:= pf24bit;
    OnWhite.SetSize(24, 24);
    OnWhite.Canvas.Brush.Color:= clWhite;
    OnWhite.Canvas.FillRect(Rect(0, 0, 24, 24));
    OnWhite.Canvas.Draw(0, 0, Outlined);
    OutsideRgb:= Cardinal(PixelRGB(OnWhite, 4, 12));
    InsideRgb := Cardinal(PixelRGB(OnWhite, 7, 12));
    Assert.IsTrue(GetRValue(OutsideRgb) < 160, 'Rim pixel on white is dark: $' + IntToHex(OutsideRgb, 6));
    Assert.AreEqual(Integer(RgbWhite), Integer(InsideRgb), 'Inside the bar on white: white');
  finally
    FreeAndNil(Plain);
    FreeAndNil(Outlined);
    FreeAndNil(OnWhite);
  end;
end;


procedure TTestSvgSpeedButton.Test_SvgToImageList_OutlineReachesTheList;
VAR
  L: TImageList;
  Bmp: TBitmap;
begin
  L:= NewList32(16, 16);
  try
    SvgToImageList(L, 0, SvgSquare16, clWhite, clBlack);   { Square 4..12; pixel 3 is just outside }
    Bmp:= DrawOnto(L, 0, clWhite);
    try
      Assert.IsTrue(GetRValue(Cardinal(PixelRGB(Bmp, 3, 8))) < 160, 'Rim pixel on white is dark: $' + IntToHex(PixelRGB(Bmp, 3, 8), 6));
      Assert.AreEqual(RgbWhite, PixelRGB(Bmp, 8, 8), 'Centre: white fill');
      Assert.AreEqual(RgbWhite, PixelRGB(Bmp, 0, 8), 'Far outside: background');
    finally
      FreeAndNil(Bmp);
    end;
  finally
    FreeAndNil(L);
  end;
end;


procedure TTestSvgSpeedButton.Test_StyleMenuHighlightColor_NoStyleIsClHighlight;
begin
  Assert.IsTrue(TStyleManager.ActiveStyle.IsSystemStyle, 'Precondition: no VCL style is active in the test EXE');
  Assert.AreEqual(Integer(ColorToRGB(clHighlight)), Integer(StyleMenuHighlightColor), 'No style: clHighlight');
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



{-------------------------------------------------------------------------------------------------------------
   IMAGE LISTS
-------------------------------------------------------------------------------------------------------------}

procedure TTestSvgSpeedButton.Test_SvgToImageList_ReplaceKeepsCountAndDrawsOnWhiteAndBlack;
VAR
  L: TImageList;
  OnWhite, OnBlack: TBitmap;
begin
  L:= NewList32(16, 16);
  try
    SvgToImageList(L, 0, SvgFull16, clBlue);
    SvgToImageList(L, 1, SvgFull16, clLime);
    Assert.AreEqual(2, L.Count, 'Two pictures before the replace');

    SvgToImageList(L, 1, SvgSquare16, clRed);
    Assert.AreEqual(2, L.Count, 'Replacing picture 1 keeps the count');

    OnWhite:= DrawOnto(L, 1, clWhite);
    OnBlack:= DrawOnto(L, 1, clBlack);
    try
      Assert.AreEqual(RgbRed,   PixelRGB(OnWhite, 8, 8), 'Centre of the square on white: pure red');
      Assert.AreEqual(RgbRed,   PixelRGB(OnBlack, 8, 8), 'Centre of the square on black: pure red');
      Assert.AreEqual(RgbWhite, PixelRGB(OnWhite, 0, 0), 'Corner outside the square on white: white');
      Assert.AreEqual(RgbBlack, PixelRGB(OnBlack, 0, 0), 'Corner outside the square on black: black');
    finally
      FreeAndNil(OnWhite);
      FreeAndNil(OnBlack);
    end;
    Assert.AreEqual(RgbBlue, DrawnCentre(L, 0, clWhite), 'Picture 0 is untouched');
  finally
    FreeAndNil(L);
  end;
end;


{ For a straight-alpha source pixel of colour C and alpha A, a correct blend gives C*A/255 + Bkg*(255-A)/255.
  The red glyph on black must give R = A, on white G = 255 - A. A premultiplied pixel handed to a list that
  wants straight alpha would give R = A*A/255 on black; a straight one handed to a list that wants it
  premultiplied would give R = 255 on black. }
procedure TTestSvgSpeedButton.Test_SvgToImageList_EdgePixelIsStraightAlpha;
CONST Row = 8;
VAR
  L: TImageList;
  Ref, OnWhite, OnBlack: TBitmap;
  x, EdgeX: Integer;
  A, ListA: Integer;
  PxBlack, PxWhite: Cardinal;
begin
  { The alpha of the edge pixel, read from RenderSvgGlyph's own bitmap }
  Ref:= RenderSvgGlyph(ParseSvg(SvgHalfEdge16), 16, clRed);
  try
    EdgeX:= -1;
    for x:= 0 to 15 do
      if (Alpha(Ref, x, Row) >= 40) AND (Alpha(Ref, x, Row) <= 215) then
        begin
          EdgeX:= x;
          Break;
        end;
    Assert.IsTrue(EdgeX >= 0, 'The test SVG must have an anti-aliased edge pixel on row 8');
    A:= Alpha(Ref, EdgeX, Row);
  finally
    FreeAndNil(Ref);
  end;

  L:= NewList32(16, 16);
  try
    SvgToImageList(L, 0, SvgHalfEdge16, clRed);
    OnWhite:= DrawOnto(L, 0, clWhite);
    OnBlack:= DrawOnto(L, 0, clBlack);
    try
      PxWhite:= Cardinal(PixelRGB(OnWhite, EdgeX, Row));
      PxBlack:= Cardinal(PixelRGB(OnBlack, EdgeX, Row));
      ListA:= 255 - GetGValue(PxWhite);   { The alpha the image list used, read back from the blend on white }

      Assert.AreEqual(Double(A), Double(ListA), 2.0, 'Alpha of the drawn edge pixel (from 255 - green on white) = alpha of the glyph');
      Assert.AreEqual(Double(255), Double(GetRValue(PxWhite)), 2.0, 'Edge on white: red stays 255');
      Assert.AreEqual(Double(ListA), Double(GetRValue(PxBlack)), 2.0, 'Edge on black: red = alpha (straight alpha blended once)');
      Assert.AreEqual(Double(0), Double(GetGValue(PxBlack)), 1.0, 'Edge on black: no green');
    finally
      FreeAndNil(OnWhite);
      FreeAndNil(OnBlack);
    end;
  finally
    FreeAndNil(L);
  end;
end;


procedure TTestSvgSpeedButton.Test_SvgToImageList_AppendAtCount;
VAR L: TImageList;
begin
  L:= NewList32(16, 16);
  try
    SvgToImageList(L, 0, SvgFull16, clBlue);
    SvgToImageList(L, L.Count, SvgSquare16, clRed);
    Assert.AreEqual(2, L.Count, 'Index = Count appends');
    Assert.AreEqual(RgbRed,   DrawnCentre(L, 1, clWhite), 'The appended picture: red square');
    Assert.AreEqual(RgbBlue,  DrawnCentre(L, 0, clWhite), 'Picture 0 is untouched');
  finally
    FreeAndNil(L);
  end;
end;


procedure TTestSvgSpeedButton.Test_SvgToImageList_NonSquareListIsCentred;
VAR
  L: TImageList;
  Bmp: TBitmap;
begin
  L:= NewList32(24, 16);   { Glyph side 16, so 4 empty columns left and right }
  try
    SvgToImageList(L, 0, SvgFull16, clRed);
    Bmp:= DrawOnto(L, 0, clWhite);
    try
      Assert.AreEqual(RgbWhite, PixelRGB(Bmp, 3,  8), 'Column 3: left of the centred glyph');
      Assert.AreEqual(RgbRed,   PixelRGB(Bmp, 4,  8), 'Column 4: first column of the glyph');
      Assert.AreEqual(RgbRed,   PixelRGB(Bmp, 19, 8), 'Column 19: last column of the glyph');
      Assert.AreEqual(RgbWhite, PixelRGB(Bmp, 20, 8), 'Column 20: right of the glyph');
    finally
      FreeAndNil(Bmp);
    end;
  finally
    FreeAndNil(L);
  end;
end;


procedure TTestSvgSpeedButton.Test_SvgToImageList_BadIndexOrColorDepthRaises;
VAR L, L24: TImageList;
begin
  L:= NewList32(16, 16);
  try
    SvgToImageList(L, 0, SvgFull16, clBlue);
    Assert.WillRaise(procedure begin SvgToImageList(L, L.Count + 1, SvgFull16, clRed); end, ESvgRenderError, 'Index = Count + 1');
    Assert.WillRaise(procedure begin SvgToImageList(L, -1, SvgFull16, clRed); end, ESvgRenderError, 'Index = -1');
    Assert.AreEqual(1, L.Count, 'A rejected call adds nothing');
  finally
    FreeAndNil(L);
  end;

  L24:= TImageList.Create(NIL);
  try
    L24.ColorDepth:= cd24Bit;
    Assert.WillRaise(procedure begin SvgToImageList(L24, 0, SvgFull16, clRed); end, ESvgRenderError, 'A cd24Bit list has no alpha');
    Assert.AreEqual(0, L24.Count, 'Nothing was added to the cd24Bit list');
  finally
    FreeAndNil(L24);
  end;
end;


procedure TTestSvgSpeedButton.Test_SetButtonSvg_SecondCallReplacesPicture;
VAR
  Btn: TButton;
  List1: TCustomImageList;
begin
  Btn:= TButton.Create(NIL);
  try
    Btn.SetBounds(0, 0, 50, 40);
    SetButtonSvg(Btn, SvgFull16, clRed);
    List1:= Btn.Images;
    Assert.IsNotNull(List1, 'The button got an image list');
    Assert.IsTrue(List1.Owner = Btn, 'The button owns the image list');
    Assert.IsTrue(List1.ColorDepth = cd32Bit, 'cd32Bit');
    Assert.AreEqual(2, List1.Count, 'Two pictures: normal and disabled');
    Assert.AreEqual(26, List1.Width, 'GlyphSize 0: 64% of the smaller side (40 * 0.64 = 25.6)');
    Assert.AreEqual(0, Btn.ImageIndex, 'ImageIndex');
    Assert.AreEqual(1, Btn.DisabledImageIndex, 'DisabledImageIndex');
    Assert.IsTrue(Btn.ImageAlignment = iaCenter, 'No caption: the picture is centred');
    Assert.AreEqual(RgbRed, DrawnCentre(List1, 0, clBlack), 'First picture: red');

    SetButtonSvg(Btn, SvgFull16, clLime);
    Assert.IsTrue(Btn.Images = List1, 'The second call reuses the same image list');
    Assert.AreEqual(1, Btn.ComponentCount, 'No second image list was created');
    Assert.AreEqual(2, List1.Count, 'Still two pictures after the second call');
    Assert.AreEqual(RgbLime, DrawnCentre(List1, 0, clBlack), 'The picture was replaced: green');
    Assert.AreEqual(Double(102), Double(GetGValue(Cardinal(DrawnCentre(List1, 1, clBlack)))), 3.0, 'The disabled picture was replaced too: green at 40%');

    SetButtonSvg(Btn, SvgFull16, clBlue, 20);
    Assert.IsTrue(Btn.Images = List1, 'A new size reuses the same image list');
    Assert.AreEqual(20, List1.Width, 'GlyphSize 20');
    Assert.AreEqual(2, List1.Count, 'Still two pictures after the resize');
    Assert.AreEqual(RgbBlue, DrawnCentre(List1, 0, clBlack), 'The resized picture: blue');
    Assert.AreEqual(1, Btn.DisabledImageIndex, 'DisabledImageIndex after the resize');
  finally
    FreeAndNil(Btn);     { Frees the image list it owns; a leak would show in the FastMM log }
  end;
end;


{ A red glyph drawn onto black gives red = alpha; onto white it gives green = blue = 255 - alpha.
  The disabled picture must carry 40% of the normal alpha (102 of 255) and still be pure red where it covers. }
procedure TTestSvgSpeedButton.Test_SetButtonSvg_DisabledPictureHas40PercentAlpha;
VAR
  Btn: TButton;
  NormalA, DisabledA: Integer;
  OnWhite: Cardinal;
begin
  Btn:= TButton.Create(NIL);
  try
    Btn.SetBounds(0, 0, 50, 40);
    SetButtonSvg(Btn, SvgFull16, clRed);
    NormalA  := GetRValue(Cardinal(DrawnCentre(Btn.Images, 0, clBlack)));
    DisabledA:= GetRValue(Cardinal(DrawnCentre(Btn.Images, Btn.DisabledImageIndex, clBlack)));
    OnWhite  := Cardinal(DrawnCentre(Btn.Images, Btn.DisabledImageIndex, clWhite));

    Assert.AreEqual(255, NormalA, 'Normal picture: opaque');
    Assert.AreEqual(Double(102), Double(DisabledA), 3.0, 'Disabled picture on black: red = alpha = 40% of 255');
    Assert.AreEqual(Double(0.40), DisabledA / NormalA, 0.015, 'Disabled alpha / normal alpha');
    Assert.AreEqual(Double(255), Double(GetRValue(OnWhite)), 2.0, 'Disabled picture on white: red stays 255 (the colour is not darkened)');
    Assert.AreEqual(Double(255 - DisabledA), Double(GetGValue(OnWhite)), 3.0, 'Disabled picture on white: green = 255 - alpha');
  finally
    FreeAndNil(Btn);
  end;
end;


procedure TTestSvgSpeedButton.Test_SetButtonSvg_ForeignImageListRaises;
VAR
  Btn: TButton;
  Foreign: TImageList;
begin
  Foreign:= NewList32(16, 16);
  try
    Btn:= TButton.Create(NIL);
    try
      Btn.SetBounds(0, 0, 50, 40);
      Btn.Images:= Foreign;
      Assert.WillRaise(procedure begin SetButtonSvg(Btn, SvgFull16, clRed); end, ESvgRenderError, 'The button already uses an image list from someone else');
      Assert.IsTrue(Btn.Images = Foreign, 'The foreign image list is still the button''s');
      Assert.AreEqual(0, Foreign.Count, 'Nothing was drawn into the foreign image list');
      Assert.AreEqual(0, Btn.ComponentCount, 'No image list was created');
    finally
      FreeAndNil(Btn);
    end;
  finally
    FreeAndNil(Foreign);
  end;
end;


{ Square 4..12 in a 16 px picture: pixel 3 lies just outside the fill, inside the 0.75 px rim }
procedure TTestSvgSpeedButton.Test_SetButtonSvg_OutlineInBothPictures;
VAR
  Btn: TButton;
  Bmp: TBitmap;
begin
  Btn:= TButton.Create(NIL);
  try
    Btn.SetBounds(0, 0, 50, 40);
    SetButtonSvg(Btn, SvgSquare16, clWhite, 16, clBlack);
    Assert.AreEqual(16, Btn.Images.Width, 'GlyphSize 16');

    Bmp:= DrawOnto(Btn.Images, 0, clWhite);
    try
      Assert.IsTrue(GetRValue(Cardinal(PixelRGB(Bmp, 3, 8))) < 160, 'Normal picture: rim pixel on white is dark: $' + IntToHex(PixelRGB(Bmp, 3, 8), 6));
      Assert.AreEqual(RgbWhite, PixelRGB(Bmp, 8, 8), 'Normal picture: white fill');
      Assert.AreEqual(RgbWhite, PixelRGB(Bmp, 0, 8), 'Normal picture: background far outside');
    finally
      FreeAndNil(Bmp);
    end;

    { At 40% opacity the rim is fainter, but still darker than the white background }
    Bmp:= DrawOnto(Btn.Images, Btn.DisabledImageIndex, clWhite);
    try
      Assert.IsTrue(GetRValue(Cardinal(PixelRGB(Bmp, 3, 8))) < 240, 'Disabled picture: rim pixel on white is darker than white: $' + IntToHex(PixelRGB(Bmp, 3, 8), 6));
      Assert.AreEqual(RgbWhite, PixelRGB(Bmp, 0, 8), 'Disabled picture: background far outside');
    finally
      FreeAndNil(Bmp);
    end;
  finally
    FreeAndNil(Btn);
  end;
end;


procedure TTestSvgSpeedButton.Test_GlyphOutlineColor_OnlyWhiteGetsRim;
begin
  Assert.AreEqual(Integer($202020), Integer(GlyphOutlineColor(clWhite)),        'White glyph: #202020 rim');
  Assert.AreEqual(Integer($202020), Integer(GlyphOutlineColor(TColor($FFFFFF))), 'White as an RGB value: #202020 rim');
  Assert.AreEqual(Integer(clNone),  Integer(GlyphOutlineColor(TColor($202020))), 'Dark glyph: no rim');
  Assert.AreEqual(Integer(clNone),  Integer(GlyphOutlineColor(clRed)),           'Any other colour: no rim');
end;


{ Contrast values (WCAG 2, computed by hand): white on #3A3A3A = 11.4, #202020 on #3A3A3A = 1.4; white on #FFFFFF = 1.0,
  #202020 on #FFFFFF = 16.3; white on #C0C0C0 = 1.8, #202020 on #C0C0C0 = 9.0 }
procedure TTestSvgSpeedButton.Test_BestContrastColorOn_WorstSurfaceDecides;
begin
  Assert.AreEqual(Integer(clWhite),         Integer(BestContrastColorOn([TColor($3A3A3A)])),                   'One dark surface: white');
  Assert.AreEqual(Integer($202020),         Integer(BestContrastColorOn([TColor($FFFFFF)])),                   'One white surface: dark');
  { A dark panel with white menus and silver buttons: white fails on the menus (1.0), dark only on the panel (1.4) }
  Assert.AreEqual(Integer($202020),         Integer(BestContrastColorOn([TColor($3A3A3A), clWhite, TColor($C0C0C0)])), 'Dark panel, white menu, silver button: dark');
  Assert.AreEqual(Integer(clWhite),         Integer(BestContrastColorOn([TColor($3A3A3A), TColor($202020)])),  'Two dark surfaces: white');
  Assert.AreEqual(Integer(BestContrastColor(TColor($808080))), Integer(BestContrastColorOn([TColor($808080)])), 'One surface: the same answer as BestContrastColor');
  Assert.WillRaise(procedure begin BestContrastColorOn([]); end, ESvgRenderError, 'No surface: raises');
end;


procedure TTestSvgSpeedButton.Test_StyleSurfaceColors_NoStyleAreSystemColors;
begin
  Assert.IsTrue(TStyleManager.ActiveStyle.IsSystemStyle, 'Precondition: no VCL style is active in the test EXE');
  Assert.AreEqual(Integer(ColorToRGB(clMenu)),    Integer(StyleMenuColor),        'Menu: clMenu');
  Assert.AreEqual(Integer(ColorToRGB(clMenuBar)), Integer(StyleMenuBarColor),     'Menu bar: clMenuBar');
  Assert.AreEqual(Integer(ColorToRGB(clBtnFace)), Integer(StyleButtonFaceColor),  'Button: clBtnFace');
  Assert.AreEqual(Integer(ColorToRGB(clBtnFace)), Integer(StyleToolButtonColor),  'Tool button: clBtnFace');
  Assert.AreEqual(Integer(BestContrastColor(clBtnFace)), Integer(StyleButtonGlyphColor), 'Button glyph: BestContrastColor of clBtnFace');
end;


procedure TTestSvgSpeedButton.Test_StyleGlyphColor_NoStyleUsesBtnFace;
VAR Btn: TLightSvgSpeedButton;
begin
  Assert.IsTrue(TStyleManager.ActiveStyle.IsSystemStyle, 'Precondition: no VCL style is active in the test EXE');
  Assert.AreEqual(Integer(BestContrastColor(clBtnFace)), Integer(StyleGlyphColor), 'No style: the contrast colour of clBtnFace');

  Btn:= TLightSvgSpeedButton.Create(NIL);
  try
    Btn.PickColorsFromStyle;
    Assert.AreEqual(Integer(Btn.GlyphColor), Integer(StyleGlyphColor(Btn)), 'StyleGlyphColor and PickColorsFromStyle agree');
  finally
    FreeAndNil(Btn);
  end;
end;


CONST
  WhiteGlyph = clWhite;
  DarkGlyph  = TColor($202020);
  DarkBar    = TColor($585858);    { Auric panel }
  LightBar   = TColor($F0F0F0);    { Windows clBtnFace }

{ A full 16 x 16 square of Glyph, drawn enabled and disabled onto Bar.
  DistOn / DistOff = channel distance of the centre pixel to the bar: the smaller, the fainter. }
procedure MeasureDisabled(Glyph, Bar: TColor; Factor: Byte; Style: TDisabledStyle; out DistOn, DistOff: Integer);
VAR L: TImageList;
begin
  L:= NewList32(16, 16);
  try
    L.GrayscaleFactor:= Factor;
    L.DisabledStyle:= Style;
    SvgToImageList(L, 0, SvgFull16, Glyph);
    DistOn := ChannelDistance(DrawnCentre(L, 0, Bar, TRUE),  Integer(ColorToRGB(Bar)));
    DistOff:= ChannelDistance(DrawnCentre(L, 0, Bar, FALSE), Integer(ColorToRGB(Bar)));
  finally
    FreeAndNil(L);
  end;
end;


{ What List.Draw(Enabled = FALSE) does to a white glyph on a dark bar and a #202020 glyph on a light bar
  (TCustomImageList.DoDraw, Vcl.ImgList.pas). Distances measured on Windows 11, out of 167 / 209 enabled:
    diDesaturate           166 / 210   not fainter
    diDesaturateBlend25      6 /  58   the default: the white glyph all but vanishes
    diDesaturateBlend50     20 / 112   the only setting that makes both fainter and keeps both visible
    diGrayscale, diMono      0 /  80   the white glyph vanishes
    GrayscaleFactor 128     40 / 224   the dark glyph gets darker, not fainter
  BlendColor changes none of these numbers. }
procedure TTestSvgSpeedButton.Test_ImageList_DisabledDraw_Measure;
VAR WOn, WOff, DOn, DOff: Integer;
begin
  MeasureDisabled(WhiteGlyph, DarkBar, 0, diDesaturate, WOn, WOff);
  MeasureDisabled(DarkGlyph, LightBar, 0, diDesaturate, DOn, DOff);
  Assert.IsTrue(WOn > 150, 'Enabled white glyph stands out from the dark bar: ' + IntToStr(WOn));
  Assert.IsTrue(DOn > 150, 'Enabled dark glyph stands out from the light bar: ' + IntToStr(DOn));
  Assert.IsTrue(WOff >= WOn - 5, 'diDesaturate: white not fainter (' + IntToStr(WOff) + ' of ' + IntToStr(WOn) + ')');
  Assert.IsTrue(DOff >= DOn - 5, 'diDesaturate: dark not fainter (' + IntToStr(DOff) + ' of ' + IntToStr(DOn) + ')');

  MeasureDisabled(WhiteGlyph, DarkBar, 0, diDesaturateBlend25, WOn, WOff);
  Assert.IsTrue(WOff < 15, 'diDesaturateBlend25 (the default): the white glyph nearly vanishes (' + IntToStr(WOff) + ' of ' + IntToStr(WOn) + ')');

  MeasureDisabled(WhiteGlyph, DarkBar, 0, diDesaturateBlend50, WOn, WOff);
  MeasureDisabled(DarkGlyph, LightBar, 0, diDesaturateBlend50, DOn, DOff);
  Assert.IsTrue((WOff >= 10) AND (WOff < WOn DIV 2), 'diDesaturateBlend50: white fainter but visible (' + IntToStr(WOff) + ' of ' + IntToStr(WOn) + ')');
  Assert.IsTrue((DOff >= 10) AND (DOff < DOn * 3 DIV 4), 'diDesaturateBlend50: dark fainter but visible (' + IntToStr(DOff) + ' of ' + IntToStr(DOn) + ')');

  MeasureDisabled(DarkGlyph, LightBar, 128, diDesaturateBlend25, DOn, DOff);
  Assert.IsTrue(DOff >= DOn, 'GrayscaleFactor 128: the dark glyph is not fainter (' + IntToStr(DOff) + ' of ' + IntToStr(DOn) + ')');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestSvgSpeedButton);
end.
