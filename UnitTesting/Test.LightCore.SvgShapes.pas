unit Test.LightCore.SvgShapes;

{=============================================================================================================
   Unit tests for LightCore.SvgShapes - the small SVG reader.
   The five icon constants are the BioniX taskbar shapes, copied from
   c:\Projects\BioniX\Projects\Tester - Taskbar icons\MainForm.pas (constant SvgGlyphs).
   Expected coordinates are read from the SVG text by hand, never computed with the reader's own code.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils, System.UITypes,
  LightCore.SvgShapes;

type
  [TestFixture]
  TTestSvgShapes = class
  private
    procedure CheckNear(Expected, Actual: Double; CONST Msg: string);
    procedure CheckPoint(CONST P: TSvgPoint; X, Y: Double; CONST Msg: string);
    procedure CheckSegment(CONST Seg: TSvgSegment; Kind: TSvgSegmentKind; X, Y: Double; CONST Msg: string);
    procedure CheckRect(CONST Shape: TSvgShape; X, Y, W, H, R: Double; CONST Msg: string);
    procedure CheckCircle(CONST Shape: TSvgShape; CX, CY, R: Double; CONST Msg: string);
    procedure CheckDefaultPaint(CONST Shape: TSvgShape; CONST Msg: string);
  public
    { The five BioniX taskbar icons }
    [Test] procedure Effects_ParsesTwoCirclesRectAndLensPath;
    [Test] procedure NextItem_ParsesTrianglePathThenBar;
    [Test] procedure Stopwatch_ParsesRingStrokeAndRotatedSideButton;
    [Test] procedure Pause_ParsesTwoRoundedBars;
    [Test] procedure NextFrame_ParsesBarThenTrianglePath;

    { Path data }
    [Test] procedure PathData_PackedNumbers;
    [Test] procedure PathData_Exponents;
    [Test] procedure PathData_RelativeCommands;
    [Test] procedure PathData_RelativeAndAbsoluteCubic;
    [Test] procedure PathData_ImplicitLineAfterMove;
    [Test] procedure PathData_LineAfterCloseStartsNewFigure;
    [Test] procedure PathData_IndependentOfLocaleDecimalSeparator;

    { Other elements and attributes }
    [Test] procedure Ellipse_And_Polygon_WithEvenOddRule;
    [Test] procedure Transforms_TranslateScaleRotateMatrixAndList;
    [Test] procedure Paint_HexColoursNoneAndCurrentColor;
    [Test] procedure RootPresentation_IsInheritedByShapes;
    [Test] procedure Rect_RxOnlyCopiesToRyAndIsClamped;
    [Test] procedure ViewBox_FallsBackToWidthHeight;
    [Test] procedure SkipsXmlDeclarationCommentsAndTitle;
    [Test] procedure ZeroSizeShapesAreNotEmitted;

    { Errors }
    [Test] procedure Arc_RaisesUnsupported_NamingTheArc;
    [Test] procedure QuadraticAndSmoothCurves_RaiseUnsupported;
    [Test] procedure Gradient_Group_Use_Style_RaiseUnsupported;
    [Test] procedure EmptyText_RaisesSvgError;
    [Test] procedure BrokenText_RaisesSvgError;
    [Test] procedure OddCoordinateCount_RaisesSvgError;
  end;

implementation

CONST
  Tol = 0.001;

  SvgEffects =
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">' +
      '<circle cx="6.5" cy="6" r="3.5"/><circle cx="14" cy="6.5" r="3"/>' +
      '<rect x="2" y="10" width="14" height="9" rx="1.5"/>' +
      '<path d="M16.5 13 L22 10 V19 L16.5 16 Z"/></svg>';

  SvgNextItem =
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">' +
      '<path d="M4 4.5 L15.5 12 L4 19.5 Z"/><rect x="16.5" y="4.5" width="3.5" height="15" rx="0.8"/></svg>';

  SvgStopwatch =
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">' +
      '<rect x="9" y="0.8" width="6" height="2.6" rx="1"/><rect x="10.9" y="3" width="2.2" height="3"/>' +
      '<circle cx="12" cy="14" r="7.4" fill="none" stroke="#000" stroke-width="2.6"/>' +
      '<rect x="10.9" y="9" width="2.2" height="6" rx="1.1"/>' +
      '<rect x="17.3" y="4.2" width="3" height="3" rx="0.7" transform="rotate(45 18.8 5.7)"/></svg>';

  SvgPause =
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">' +
      '<rect x="5" y="4.5" width="5" height="15" rx="1"/><rect x="14" y="4.5" width="5" height="15" rx="1"/></svg>';

  SvgNextFrame =
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">' +
      '<rect x="4" y="4.5" width="3.5" height="15" rx="0.8"/><path d="M9.5 4.5 L21 12 L9.5 19.5 Z"/></svg>';

  Black = TAlphaColor($FF000000);


function Pt(X, Y: Single): TSvgPoint;
begin
  Result.X:= X;
  Result.Y:= Y;
end;


function Wrap(CONST Body: string): string;
begin
  Result:= '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">' + Body + '</svg>';
end;


{ Helpers }

procedure TTestSvgShapes.CheckNear(Expected, Actual: Double; CONST Msg: string);
begin
  Assert.AreEqual(Expected, Actual, Double(Tol), Msg);
end;


procedure TTestSvgShapes.CheckPoint(CONST P: TSvgPoint; X, Y: Double; CONST Msg: string);
begin
  CheckNear(X, P.X, Msg + ' X');
  CheckNear(Y, P.Y, Msg + ' Y');
end;


procedure TTestSvgShapes.CheckSegment(CONST Seg: TSvgSegment; Kind: TSvgSegmentKind; X, Y: Double; CONST Msg: string);
begin
  Assert.AreEqual(Ord(Kind), Ord(Seg.Kind), Msg + ' kind');
  if Kind <> sgClose
  then CheckPoint(Seg.Pt, X, Y, Msg);
end;


procedure TTestSvgShapes.CheckRect(CONST Shape: TSvgShape; X, Y, W, H, R: Double; CONST Msg: string);
begin
  Assert.AreEqual(Ord(sskRect), Ord(Shape.Kind), Msg + ' kind');
  CheckNear(X, Shape.X, Msg + ' x');
  CheckNear(Y, Shape.Y, Msg + ' y');
  CheckNear(W, Shape.Width, Msg + ' width');
  CheckNear(H, Shape.Height, Msg + ' height');
  CheckNear(R, Shape.RX, Msg + ' rx');
  CheckNear(R, Shape.RY, Msg + ' ry');
end;


procedure TTestSvgShapes.CheckCircle(CONST Shape: TSvgShape; CX, CY, R: Double; CONST Msg: string);
begin
  Assert.AreEqual(Ord(sskEllipse), Ord(Shape.Kind), Msg + ' kind');
  CheckNear(CX, Shape.CX, Msg + ' cx');
  CheckNear(CY, Shape.CY, Msg + ' cy');
  CheckNear(R, Shape.RX, Msg + ' rx');
  CheckNear(R, Shape.RY, Msg + ' ry');
end;


{ A shape with no paint attributes: SVG default fill black, no stroke, nonzero rule, no transform }
procedure TTestSvgShapes.CheckDefaultPaint(CONST Shape: TSvgShape; CONST Msg: string);
begin
  Assert.AreEqual(Ord(spkColor), Ord(Shape.Fill.Kind), Msg + ' fill kind');
  Assert.AreEqual(Black, Shape.Fill.Color, Msg + ' fill colour');
  Assert.AreEqual(Ord(spkNone), Ord(Shape.Stroke.Kind), Msg + ' stroke kind');
  Assert.AreEqual(Ord(sfrNonZero), Ord(Shape.FillRule), Msg + ' fill rule');
  Assert.IsTrue(Shape.Transform.IsIdentity, Msg + ' transform is identity');
end;


{ The five BioniX icons }

procedure TTestSvgShapes.Effects_ParsesTwoCirclesRectAndLensPath;
VAR Img: TSvgImage;
begin
  Img:= ParseSvg(SvgEffects);
  CheckNear(0,  Img.ViewBox.MinX,   'viewBox minX');
  CheckNear(0,  Img.ViewBox.MinY,   'viewBox minY');
  CheckNear(24, Img.ViewBox.Width,  'viewBox width');
  CheckNear(24, Img.ViewBox.Height, 'viewBox height');
  Assert.AreEqual(4, Integer(Length(Img.Shapes)), 'shape count');

  CheckCircle(Img.Shapes[0], 6.5, 6,   3.5, 'reel 1');
  CheckCircle(Img.Shapes[1], 14,  6.5, 3,   'reel 2');
  CheckRect  (Img.Shapes[2], 2, 10, 14, 9, 1.5, 'body');

  Assert.AreEqual(Ord(sskPath), Ord(Img.Shapes[3].Kind), 'lens kind');
  Assert.AreEqual(5, Integer(Length(Img.Shapes[3].Segments)), 'lens segment count');
  CheckSegment(Img.Shapes[3].Segments[0], sgMoveTo, 16.5, 13, 'lens M');
  CheckSegment(Img.Shapes[3].Segments[1], sgLineTo, 22,   10, 'lens L');
  CheckSegment(Img.Shapes[3].Segments[2], sgLineTo, 22,   19, 'lens V keeps X');
  CheckSegment(Img.Shapes[3].Segments[3], sgLineTo, 16.5, 16, 'lens L2');
  CheckSegment(Img.Shapes[3].Segments[4], sgClose,  0,    0,  'lens Z');

  CheckDefaultPaint(Img.Shapes[0], 'reel 1');
  CheckDefaultPaint(Img.Shapes[3], 'lens');
end;


procedure TTestSvgShapes.NextItem_ParsesTrianglePathThenBar;
VAR Img: TSvgImage;
begin
  Img:= ParseSvg(SvgNextItem);
  Assert.AreEqual(2, Integer(Length(Img.Shapes)), 'shape count');
  Assert.AreEqual(Ord(sskPath), Ord(Img.Shapes[0].Kind), 'triangle kind');
  Assert.AreEqual(4, Integer(Length(Img.Shapes[0].Segments)), 'triangle segment count');
  CheckSegment(Img.Shapes[0].Segments[0], sgMoveTo, 4,    4.5,  'M');
  CheckSegment(Img.Shapes[0].Segments[1], sgLineTo, 15.5, 12,   'L1');
  CheckSegment(Img.Shapes[0].Segments[2], sgLineTo, 4,    19.5, 'L2');
  CheckSegment(Img.Shapes[0].Segments[3], sgClose,  0,    0,    'Z');
  CheckRect(Img.Shapes[1], 16.5, 4.5, 3.5, 15, 0.8, 'bar');
end;


procedure TTestSvgShapes.Stopwatch_ParsesRingStrokeAndRotatedSideButton;
VAR
  Img: TSvgImage;
  Side: TSvgShape;
begin
  Img:= ParseSvg(SvgStopwatch);
  Assert.AreEqual(5, Integer(Length(Img.Shapes)), 'shape count');
  CheckRect(Img.Shapes[0], 9,    0.8, 6,   2.6, 1,   'crown');
  CheckRect(Img.Shapes[1], 10.9, 3,   2.2, 3,   0,   'stem, no rx');
  CheckRect(Img.Shapes[3], 10.9, 9,   2.2, 6,   1.1, 'hand');

  { The ring: not filled, stroked black, 2.6 wide }
  CheckCircle(Img.Shapes[2], 12, 14, 7.4, 'ring');
  Assert.AreEqual(Ord(spkNone),  Ord(Img.Shapes[2].Fill.Kind),   'ring fill none');
  Assert.AreEqual(Ord(spkColor), Ord(Img.Shapes[2].Stroke.Kind), 'ring stroke is a colour');
  Assert.AreEqual(Black, Img.Shapes[2].Stroke.Color, 'ring stroke #000');
  CheckNear(2.6, Img.Shapes[2].StrokeWidth, 'ring stroke width');

  { The side button: rotate(45 18.8 5.7) turns the square around its own centre.
    The centre stays put; the corner (17.3, 4.2) is 1.5*sqrt(2) = 2.1213 from it, rotated to straight above. }
  Side:= Img.Shapes[4];
  CheckRect(Side, 17.3, 4.2, 3, 3, 0.7, 'side button');
  Assert.IsFalse(Side.Transform.IsIdentity, 'side button has a transform');
  CheckPoint(Side.Transform.Transform(Pt(18.8, 5.7)), 18.8, 5.7,    'centre is fixed');
  CheckPoint(Side.Transform.Transform(Pt(17.3, 4.2)), 18.8, 3.5787, 'top-left corner goes to the top');
  CheckPoint(Side.Transform.Transform(Pt(20.3, 4.2)), 20.9213, 5.7, 'top-right corner goes to the right');
  Assert.IsTrue(Img.Shapes[0].Transform.IsIdentity, 'crown has no transform');
end;


procedure TTestSvgShapes.Pause_ParsesTwoRoundedBars;
VAR Img: TSvgImage;
begin
  Img:= ParseSvg(SvgPause);
  Assert.AreEqual(2, Integer(Length(Img.Shapes)), 'shape count');
  CheckRect(Img.Shapes[0], 5,  4.5, 5, 15, 1, 'left bar');
  CheckRect(Img.Shapes[1], 14, 4.5, 5, 15, 1, 'right bar');
  CheckDefaultPaint(Img.Shapes[1], 'right bar');
end;


procedure TTestSvgShapes.NextFrame_ParsesBarThenTrianglePath;
VAR Img: TSvgImage;
begin
  Img:= ParseSvg(SvgNextFrame);
  Assert.AreEqual(2, Integer(Length(Img.Shapes)), 'shape count');
  CheckRect(Img.Shapes[0], 4, 4.5, 3.5, 15, 0.8, 'bar');
  Assert.AreEqual(Ord(sskPath), Ord(Img.Shapes[1].Kind), 'triangle kind');
  Assert.AreEqual(4, Integer(Length(Img.Shapes[1].Segments)), 'triangle segment count');
  CheckSegment(Img.Shapes[1].Segments[0], sgMoveTo, 9.5, 4.5,  'M');
  CheckSegment(Img.Shapes[1].Segments[1], sgLineTo, 21,  12,   'L1');
  CheckSegment(Img.Shapes[1].Segments[2], sgLineTo, 9.5, 19.5, 'L2');
  CheckSegment(Img.Shapes[1].Segments[3], sgClose,  0,   0,    'Z');
end;


{ Path data }

procedure TTestSvgShapes.PathData_PackedNumbers;
VAR Segs: TSvgSegments;
begin
  { 10, -5.5, .5, -1: the sign and the second dot each start a new number }
  Segs:= ParseSvgPathData('M10-5.5.5-1');
  Assert.AreEqual(2, Integer(Length(Segs)), 'segment count');
  CheckSegment(Segs[0], sgMoveTo, 10,  -5.5, 'M');
  CheckSegment(Segs[1], sgLineTo, 0.5, -1,   'implicit L');
end;


procedure TTestSvgShapes.PathData_Exponents;
VAR Segs: TSvgSegments;
begin
  Segs:= ParseSvgPathData('M1e1 2.5E-1L1.5e+2,-3e0');
  Assert.AreEqual(2, Integer(Length(Segs)), 'segment count');
  CheckSegment(Segs[0], sgMoveTo, 10,  0.25, 'M');
  CheckSegment(Segs[1], sgLineTo, 150, -3,   'L');
end;


procedure TTestSvgShapes.PathData_RelativeCommands;
VAR Segs: TSvgSegments;
begin
  { After z the current point is the start of the figure (10,10), so the next m is relative to it }
  Segs:= ParseSvgPathData('m10 10 l5 0 v5 h-5 z m2 2 l1 1');
  Assert.AreEqual(7, Integer(Length(Segs)), 'segment count');
  CheckSegment(Segs[0], sgMoveTo, 10, 10, 'm');
  CheckSegment(Segs[1], sgLineTo, 15, 10, 'l');
  CheckSegment(Segs[2], sgLineTo, 15, 15, 'v');
  CheckSegment(Segs[3], sgLineTo, 10, 15, 'h');
  CheckSegment(Segs[4], sgClose,  0,  0,  'z');
  CheckSegment(Segs[5], sgMoveTo, 12, 12, 'm after z');
  CheckSegment(Segs[6], sgLineTo, 13, 13, 'l after m');
end;


procedure TTestSvgShapes.PathData_RelativeAndAbsoluteCubic;
VAR Segs: TSvgSegments;
begin
  Segs:= ParseSvgPathData('M10 10 c1 2 3 4 5 6 C20 20 21 21 22 22 H30');
  Assert.AreEqual(4, Integer(Length(Segs)), 'segment count');
  Assert.AreEqual(Ord(sgCubicTo), Ord(Segs[1].Kind), 'c kind');
  CheckPoint(Segs[1].Ctrl1, 11, 12, 'c ctrl1');
  CheckPoint(Segs[1].Ctrl2, 13, 14, 'c ctrl2');
  CheckPoint(Segs[1].Pt,    15, 16, 'c end');
  Assert.AreEqual(Ord(sgCubicTo), Ord(Segs[2].Kind), 'C kind');
  CheckPoint(Segs[2].Ctrl1, 20, 20, 'C ctrl1');
  CheckPoint(Segs[2].Ctrl2, 21, 21, 'C ctrl2');
  CheckPoint(Segs[2].Pt,    22, 22, 'C end');
  CheckSegment(Segs[3], sgLineTo, 30, 22, 'H keeps Y');
end;


procedure TTestSvgShapes.PathData_ImplicitLineAfterMove;
VAR Segs: TSvgSegments;
begin
  Segs:= ParseSvgPathData('M0 0 1 1, 2 2 m1 1 2 2');
  Assert.AreEqual(5, Integer(Length(Segs)), 'segment count');
  CheckSegment(Segs[0], sgMoveTo, 0, 0, 'M');
  CheckSegment(Segs[1], sgLineTo, 1, 1, 'implicit L 1');
  CheckSegment(Segs[2], sgLineTo, 2, 2, 'implicit L 2');
  CheckSegment(Segs[3], sgMoveTo, 3, 3, 'm');
  CheckSegment(Segs[4], sgLineTo, 5, 5, 'implicit l');
end;


procedure TTestSvgShapes.PathData_LineAfterCloseStartsNewFigure;
VAR Segs: TSvgSegments;
begin
  Segs:= ParseSvgPathData('M1 1 L5 1 Z L3 3');
  Assert.AreEqual(5, Integer(Length(Segs)), 'segment count');
  CheckSegment(Segs[2], sgClose,  0, 0, 'Z');
  CheckSegment(Segs[3], sgMoveTo, 1, 1, 'implicit move to the figure start');
  CheckSegment(Segs[4], sgLineTo, 3, 3, 'L');
end;


procedure TTestSvgShapes.PathData_IndependentOfLocaleDecimalSeparator;
VAR
  OldSeparator: Char;
  Segs: TSvgSegments;
begin
  OldSeparator:= FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator:= ',';
  TRY
    Segs:= ParseSvgPathData('M0.5 1.25');
  FINALLY
    FormatSettings.DecimalSeparator:= OldSeparator;
  END;
  Assert.AreEqual(1, Integer(Length(Segs)), 'segment count');
  CheckSegment(Segs[0], sgMoveTo, 0.5, 1.25, 'M with comma locale');
end;


{ Other elements and attributes }

procedure TTestSvgShapes.Ellipse_And_Polygon_WithEvenOddRule;
VAR Img: TSvgImage;
begin
  Img:= ParseSvg(Wrap('<ellipse cx="5" cy="6" rx="3" ry="2"/><polygon points="0,0 10,0 5,8" fill-rule="evenodd"/>'));
  Assert.AreEqual(2, Integer(Length(Img.Shapes)), 'shape count');

  Assert.AreEqual(Ord(sskEllipse), Ord(Img.Shapes[0].Kind), 'ellipse kind');
  CheckNear(5, Img.Shapes[0].CX, 'ellipse cx');
  CheckNear(6, Img.Shapes[0].CY, 'ellipse cy');
  CheckNear(3, Img.Shapes[0].RX, 'ellipse rx');
  CheckNear(2, Img.Shapes[0].RY, 'ellipse ry');

  Assert.AreEqual(Ord(sskPath), Ord(Img.Shapes[1].Kind), 'polygon kind');
  Assert.AreEqual(Ord(sfrEvenOdd), Ord(Img.Shapes[1].FillRule), 'polygon fill rule');
  Assert.AreEqual(4, Integer(Length(Img.Shapes[1].Segments)), 'polygon segment count');
  CheckSegment(Img.Shapes[1].Segments[0], sgMoveTo, 0,  0, 'P1');
  CheckSegment(Img.Shapes[1].Segments[1], sgLineTo, 10, 0, 'P2');
  CheckSegment(Img.Shapes[1].Segments[2], sgLineTo, 5,  8, 'P3');
  CheckSegment(Img.Shapes[1].Segments[3], sgClose,  0,  0, 'closed');
end;


procedure TTestSvgShapes.Transforms_TranslateScaleRotateMatrixAndList;
VAR Img: TSvgImage;
begin
  Img:= ParseSvg(Wrap(
    '<rect width="1" height="1" transform="translate(5)"/>' +
    '<rect width="1" height="1" transform="scale(2 3)"/>' +
    '<rect width="1" height="1" transform="rotate(90)"/>' +
    '<rect width="1" height="1" transform="translate(10 0) scale(2)"/>' +
    '<rect width="1" height="1" transform="matrix(1,0,0,1,7,8)"/>'));
  Assert.AreEqual(5, Integer(Length(Img.Shapes)), 'shape count');
  CheckPoint(Img.Shapes[0].Transform.Transform(Pt(1, 1)), 6,  1, 'translate(5): ty defaults to 0');
  CheckPoint(Img.Shapes[1].Transform.Transform(Pt(1, 1)), 2,  3, 'scale(2 3)');
  CheckPoint(Img.Shapes[2].Transform.Transform(Pt(1, 0)), 0,  1, 'rotate(90) turns +X into +Y');
  CheckPoint(Img.Shapes[3].Transform.Transform(Pt(1, 1)), 12, 2, 'list: scale applied first, then translate');
  CheckPoint(Img.Shapes[4].Transform.Transform(Pt(1, 1)), 8,  9, 'matrix');
end;


procedure TTestSvgShapes.Paint_HexColoursNoneAndCurrentColor;
VAR Img: TSvgImage;
begin
  Img:= ParseSvg(Wrap(
    '<rect width="1" height="1" fill="#f00"/>' +
    '<rect width="1" height="1" fill="#20A0c0" stroke="white" stroke-width="0.5"/>' +
    '<rect width="1" height="1" fill="currentColor"/>' +
    '<rect width="1" height="1" fill="none" stroke="currentColor"/>'));
  Assert.AreEqual(4, Integer(Length(Img.Shapes)), 'shape count');
  Assert.AreEqual(Ord(spkColor), Ord(Img.Shapes[0].Fill.Kind), '#f00 kind');
  Assert.AreEqual(TAlphaColor($FFFF0000), Img.Shapes[0].Fill.Color, '#f00 expands to #ff0000');
  Assert.AreEqual(TAlphaColor($FF20A0C0), Img.Shapes[1].Fill.Color, '#20A0c0');
  Assert.AreEqual(Ord(spkColor), Ord(Img.Shapes[1].Stroke.Kind), 'white stroke kind');
  Assert.AreEqual(TAlphaColor($FFFFFFFF), Img.Shapes[1].Stroke.Color, 'white stroke');
  CheckNear(0.5, Img.Shapes[1].StrokeWidth, 'stroke width');
  Assert.AreEqual(Ord(spkCurrentColor), Ord(Img.Shapes[2].Fill.Kind), 'currentColor fill');
  Assert.AreEqual(Ord(spkNone), Ord(Img.Shapes[3].Fill.Kind), 'fill none');
  Assert.AreEqual(Ord(spkCurrentColor), Ord(Img.Shapes[3].Stroke.Kind), 'currentColor stroke');
  CheckNear(1, Img.Shapes[3].StrokeWidth, 'default stroke width is 1');
end;


procedure TTestSvgShapes.RootPresentation_IsInheritedByShapes;
VAR Img: TSvgImage;
begin
  Img:= ParseSvg('<svg viewBox="0 0 10 10" fill="none" stroke="currentColor" stroke-width="2" fill-rule="evenodd">' +
                 '<rect width="4" height="4"/><circle r="3" fill="#00f"/></svg>');
  Assert.AreEqual(2, Integer(Length(Img.Shapes)), 'shape count');
  Assert.AreEqual(Ord(spkNone), Ord(Img.Shapes[0].Fill.Kind), 'inherited fill none');
  Assert.AreEqual(Ord(spkCurrentColor), Ord(Img.Shapes[0].Stroke.Kind), 'inherited stroke');
  CheckNear(2, Img.Shapes[0].StrokeWidth, 'inherited stroke width');
  Assert.AreEqual(Ord(sfrEvenOdd), Ord(Img.Shapes[0].FillRule), 'inherited fill rule');
  Assert.AreEqual(TAlphaColor($FF0000FF), Img.Shapes[1].Fill.Color, 'own fill wins over the inherited one');
  Assert.AreEqual(Ord(spkCurrentColor), Ord(Img.Shapes[1].Stroke.Kind), 'stroke still inherited');
end;


procedure TTestSvgShapes.Rect_RxOnlyCopiesToRyAndIsClamped;
VAR Img: TSvgImage;
begin
  Img:= ParseSvg(Wrap('<rect width="10" height="4" rx="1"/><rect width="10" height="4" ry="1.5"/>' +
                      '<rect width="10" height="4" rx="8"/><rect width="10" height="4" rx="1" ry="2"/>'));
  Assert.AreEqual(4, Integer(Length(Img.Shapes)), 'shape count');
  CheckNear(1,   Img.Shapes[0].RY, 'ry copies rx');
  CheckNear(1.5, Img.Shapes[1].RX, 'rx copies ry');
  CheckNear(5,   Img.Shapes[2].RX, 'rx clamped to width/2');
  CheckNear(2,   Img.Shapes[2].RY, 'ry (copied 8) clamped to height/2');
  CheckNear(1,   Img.Shapes[3].RX, 'own rx');
  CheckNear(2,   Img.Shapes[3].RY, 'own ry');
end;


procedure TTestSvgShapes.ViewBox_FallsBackToWidthHeight;
VAR Img: TSvgImage;
begin
  Img:= ParseSvg('<svg width="32px" height="16"><rect width="1" height="1"/></svg>');
  CheckNear(0,  Img.ViewBox.MinX,   'minX');
  CheckNear(32, Img.ViewBox.Width,  'width from width="32px"');
  CheckNear(16, Img.ViewBox.Height, 'height');

  Img:= ParseSvg('<svg viewBox="-2,-3 20 10"/>');
  CheckNear(-2, Img.ViewBox.MinX,   'minX with comma separator');
  CheckNear(-3, Img.ViewBox.MinY,   'minY');
  CheckNear(20, Img.ViewBox.Width,  'width');
  CheckNear(10, Img.ViewBox.Height, 'height');
  Assert.AreEqual(0, Integer(Length(Img.Shapes)), 'self-closed svg has no shapes');
end;


procedure TTestSvgShapes.SkipsXmlDeclarationCommentsAndTitle;
VAR Img: TSvgImage;
begin
  Img:= ParseSvg('<?xml version="1.0" encoding="UTF-8"?>'#13#10 +
                 '<!-- made by hand -->'#13#10 +
                 '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">'#13#10 +
                 '  <title>Pause</title><desc>Two <b>bars</b></desc>'#13#10 +
                 '  <!-- a comment with <rect/> inside -->'#13#10 +
                 '  <rect x="1" y="2" width="3" height="4"><title>tip</title></rect>'#13#10 +
                 '</svg>'#13#10);
  Assert.AreEqual(1, Integer(Length(Img.Shapes)), 'only the rect is a shape');
  CheckRect(Img.Shapes[0], 1, 2, 3, 4, 0, 'rect');
end;


procedure TTestSvgShapes.ZeroSizeShapesAreNotEmitted;
VAR Img: TSvgImage;
begin
  Img:= ParseSvg(Wrap('<rect width="0" height="5"/><circle r="0"/><path d=""/><rect width="2" height="2"/>'));
  Assert.AreEqual(1, Integer(Length(Img.Shapes)), 'only the 2x2 rect is drawn');
  CheckNear(2, Img.Shapes[0].Width, 'the 2x2 rect');
end;


{ Errors }

procedure TTestSvgShapes.Arc_RaisesUnsupported_NamingTheArc;
VAR Raised: Boolean;
begin
  Raised:= FALSE;
  TRY
    ParseSvg(Wrap('<path d="M0 0 A5 5 0 0 1 10 10"/>'));
  EXCEPT
    on E: ESvgUnsupported do
      begin
        Raised:= TRUE;
        Assert.Contains(E.Message, 'arc', TRUE, 'message names the arc');
        Assert.Contains(E.Message, '"A"', 'message names the command letter');
      end;
  END;
  Assert.IsTrue(Raised, 'ESvgUnsupported raised for an arc');

  Assert.WillRaise(procedure begin ParseSvgPathData('M0 0 a5 5 0 0 1 10 10') end, ESvgUnsupported, 'relative arc');
end;


procedure TTestSvgShapes.QuadraticAndSmoothCurves_RaiseUnsupported;
begin
  Assert.WillRaise(procedure begin ParseSvgPathData('M0 0 Q1 1 2 2') end, ESvgUnsupported, 'Q');
  Assert.WillRaise(procedure begin ParseSvgPathData('M0 0 t2 2') end,     ESvgUnsupported, 't');
  Assert.WillRaise(procedure begin ParseSvgPathData('M0 0 S1 1 2 2') end, ESvgUnsupported, 'S');
end;


procedure TTestSvgShapes.Gradient_Group_Use_Style_RaiseUnsupported;
begin
  Assert.WillRaise(procedure begin ParseSvg(Wrap('<rect width="1" height="1" fill="url(#grad)"/>')) end, ESvgUnsupported, 'gradient fill');
  Assert.WillRaise(procedure begin ParseSvg(Wrap('<g><rect width="1" height="1"/></g>')) end,          ESvgUnsupported, '<g>');
  Assert.WillRaise(procedure begin ParseSvg(Wrap('<use href="#a"/>')) end,                             ESvgUnsupported, '<use>');
  Assert.WillRaise(procedure begin ParseSvg(Wrap('<rect width="1" height="1" style="fill:red"/>')) end, ESvgUnsupported, 'style attribute');
  Assert.WillRaise(procedure begin ParseSvg(Wrap('<linearGradient id="g"/>')) end,                     ESvgUnsupported, '<linearGradient>');
  Assert.WillRaise(procedure begin ParseSvg(Wrap('<rect width="1" height="1" transform="skewX(10)"/>')) end, ESvgUnsupported, 'skewX');
  Assert.WillRaise(procedure begin ParseSvg(Wrap('<rect width="50%" height="1"/>')) end,               ESvgUnsupported, 'percent length');
end;


procedure TTestSvgShapes.EmptyText_RaisesSvgError;
begin
  Assert.WillRaise(procedure begin ParseSvg('') end,       ESvgError, 'empty');
  Assert.WillRaise(procedure begin ParseSvg('  '#13#10) end, ESvgError, 'whitespace only');
  Assert.WillRaise(procedure begin ParseSvgPathData('') end, ESvgError, 'empty path data');
end;


procedure TTestSvgShapes.BrokenText_RaisesSvgError;
begin
  Assert.WillRaise(procedure begin ParseSvg('<svg viewBox="0 0 24 24"><path d="M4 4 L') end, ESvgError, 'truncated');
  Assert.WillRaise(procedure begin ParseSvg('<svg viewBox="0 0 24 24"><rect width="1" height="1"/>') end, ESvgError, 'no </svg>');
  Assert.WillRaise(procedure begin ParseSvg('<path d="M0 0 L1 1"/>') end,                    ESvgError, 'no <svg> root');
  Assert.WillRaise(procedure begin ParseSvg('just text') end,                                ESvgError, 'not XML');
  Assert.WillRaise(procedure begin ParseSvg(Wrap('<rect width="1x" height="1"/>')) end,      ESvgError, 'bad number');
  Assert.WillRaise(procedure begin ParseSvg(Wrap('<rect width="1" height="1"></circle>')) end, ESvgError, 'mismatched close tag');
  Assert.WillRaise(procedure begin ParseSvg('<svg viewBox="0 0 24"/>') end,                  ESvgError, 'viewBox with 3 numbers');
  Assert.WillRaise(procedure begin ParseSvg('<svg><rect width="1" height="1"/></svg>') end,  ESvgError, 'no viewBox and no size');
  Assert.WillRaise(procedure begin ParseSvgPathData('L1 1') end,                             ESvgError, 'path not starting with M');
  Assert.WillRaise(procedure begin ParseSvgPathData('M1 1 X2 2') end,                        ESvgError, 'unknown command');
  Assert.WillRaise(procedure begin ParseSvgPathData('M1 1 Z 5 5') end,                       ESvgError, 'number after Z');
end;


procedure TTestSvgShapes.OddCoordinateCount_RaisesSvgError;
begin
  Assert.WillRaise(procedure begin ParseSvgPathData('M1') end,                       ESvgError, 'M with one number');
  Assert.WillRaise(procedure begin ParseSvgPathData('M1 1 C1 2 3 4 5') end,          ESvgError, 'C with 5 numbers');
  Assert.WillRaise(procedure begin ParseSvg(Wrap('<polygon points="0,0 10"/>')) end, ESvgError, 'polygon with 3 numbers');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestSvgShapes);

end.
