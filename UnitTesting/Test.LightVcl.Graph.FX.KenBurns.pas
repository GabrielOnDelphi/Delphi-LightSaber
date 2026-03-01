unit Test.LightVcl.Graph.FX.KenBurns;

{=============================================================================================================
   Unit tests for LightVcl.Graph.FX.KenBurns.pas
   Tests easing functions, rectangle interpolation, random path generation,
   clamping logic, easing name/index conversion, and KenBurnsFrame rendering.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Types,
  System.Math,
  Vcl.Graphics;

type
  [TestFixture]
  TTestKenBurns = class
  private
    FSource: TBitmap;
    FOutput: TBitmap;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { ApplyEasing - boundary values }
    [Test]
    procedure TestApplyEasing_Linear_Zero;
    [Test]
    procedure TestApplyEasing_Linear_One;
    [Test]
    procedure TestApplyEasing_Linear_Half;

    { ApplyEasing - all easing types at midpoint }
    [Test]
    procedure TestApplyEasing_SmoothStep_Half;
    [Test]
    procedure TestApplyEasing_EaseInOut_Half;
    [Test]
    procedure TestApplyEasing_EaseIn_Half;
    [Test]
    procedure TestApplyEasing_EaseOut_Half;

    { ApplyEasing - boundary values for all types }
    [Test]
    procedure TestApplyEasing_AllTypes_ZeroReturnsZero;
    [Test]
    procedure TestApplyEasing_AllTypes_OneReturnsOne;

    { ApplyEasing - out-of-range clamping }
    [Test]
    procedure TestApplyEasing_NegativeInput_ClampedToZero;
    [Test]
    procedure TestApplyEasing_OverOneInput_ClampedToOne;
    [Test]
    procedure TestApplyEasing_LargeNegative_ClampedToZero;
    [Test]
    procedure TestApplyEasing_LargePositive_ClampedToOne;

    { LerpRectF }
    [Test]
    procedure TestLerpRectF_AtZero_ReturnsA;
    [Test]
    procedure TestLerpRectF_AtOne_ReturnsB;
    [Test]
    procedure TestLerpRectF_AtHalf_ReturnsMidpoint;
    [Test]
    procedure TestLerpRectF_IdenticalRects;

    { GenerateRandomPath - rect validity }
    [Test]
    procedure TestGenerateRandomPath_RectsWithinBounds;
    [Test]
    procedure TestGenerateRandomPath_RectsHavePositiveSize;
    [Test]
    procedure TestGenerateRandomPath_MultipleCalls_ProduceDifferentResults;
    [Test]
    procedure TestGenerateRandomPath_ZeroZoomRange;
    [Test]
    procedure TestGenerateRandomPath_ZeroPanRange;
    [Test]
    procedure TestGenerateRandomPath_LargeZoomRange;
    [Test]
    procedure TestGenerateRandomPath_LargePanRange;
    [Test]
    procedure TestGenerateRandomPath_NegativeInputs;

    { ClampRect - tested indirectly through GenerateRandomPath,
      but we can verify specific scenarios via path rects }
    [Test]
    procedure TestGenerateRandomPath_100Iterations_AllValid;

    { EasingName }
    [Test]
    procedure TestEasingName_AllValues_NonEmpty;
    [Test]
    procedure TestEasingName_Linear;
    [Test]
    procedure TestEasingName_SmoothStep;
    [Test]
    procedure TestEasingName_EaseInOut;
    [Test]
    procedure TestEasingName_EaseIn;
    [Test]
    procedure TestEasingName_EaseOut;

    { IndexToEasing }
    [Test]
    procedure TestIndexToEasing_ValidIndices;
    [Test]
    procedure TestIndexToEasing_NegativeIndex_ReturnsSmoothStep;
    [Test]
    procedure TestIndexToEasing_TooLargeIndex_ReturnsSmoothStep;
    [Test]
    procedure TestIndexToEasing_BoundaryLow;
    [Test]
    procedure TestIndexToEasing_BoundaryHigh;

    { KenBurnsFrame }
    [Test]
    procedure TestKenBurnsFrame_ValidInputs_NoException;
    [Test]
    procedure TestKenBurnsFrame_AllEasingTypes_NoException;
    [Test]
    procedure TestKenBurnsFrame_ProgressBoundaries;
    [Test]
    procedure TestKenBurnsFrame_ProgressOutOfRange_Clamped;
  end;

implementation

uses
  LightVcl.Graph.FX.KenBurns;

const
  FLOAT_TOLERANCE = 0.0001;


procedure TTestKenBurns.Setup;
begin
  FSource:= TBitmap.Create;
  FSource.Width:= 200;
  FSource.Height:= 150;
  FSource.PixelFormat:= pf24bit;
  FSource.Canvas.Brush.Color:= clWhite;
  FSource.Canvas.FillRect(Rect(0, 0, FSource.Width, FSource.Height));

  FOutput:= TBitmap.Create;
  FOutput.Width:= 100;
  FOutput.Height:= 75;
  FOutput.PixelFormat:= pf24bit;
end;


procedure TTestKenBurns.TearDown;
begin
  FreeAndNil(FSource);
  FreeAndNil(FOutput);
end;


{ === ApplyEasing - boundary values === }

procedure TTestKenBurns.TestApplyEasing_Linear_Zero;
begin
  Assert.AreEqual(Double(0.0), Double(ApplyEasing(0.0, etLinear)), FLOAT_TOLERANCE,
    'Linear easing at T=0 should return 0');
end;


procedure TTestKenBurns.TestApplyEasing_Linear_One;
begin
  Assert.AreEqual(Double(1.0), Double(ApplyEasing(1.0, etLinear)), FLOAT_TOLERANCE,
    'Linear easing at T=1 should return 1');
end;


procedure TTestKenBurns.TestApplyEasing_Linear_Half;
begin
  Assert.AreEqual(Double(0.5), Double(ApplyEasing(0.5, etLinear)), FLOAT_TOLERANCE,
    'Linear easing at T=0.5 should return 0.5');
end;


{ === ApplyEasing - all easing types at midpoint === }

procedure TTestKenBurns.TestApplyEasing_SmoothStep_Half;
var
  R: Single;
begin
  R:= ApplyEasing(0.5, etSmoothStep);
  { SmoothStep(0.5) = 0.5*0.5*(3 - 2*0.5) = 0.25*2 = 0.5 }
  Assert.AreEqual(Double(0.5), Double(R), FLOAT_TOLERANCE,
    'SmoothStep at T=0.5 should return 0.5');
end;


procedure TTestKenBurns.TestApplyEasing_EaseInOut_Half;
var
  R: Single;
begin
  R:= ApplyEasing(0.5, etEaseInOut);
  { EaseInOut at T=0.5 (exactly at boundary): 2*0.5*0.5 = 0.5 }
  Assert.AreEqual(Double(0.5), Double(R), FLOAT_TOLERANCE,
    'EaseInOut at T=0.5 should return 0.5');
end;


procedure TTestKenBurns.TestApplyEasing_EaseIn_Half;
var
  R: Single;
begin
  R:= ApplyEasing(0.5, etEaseIn);
  { EaseIn(0.5) = 0.5*0.5 = 0.25 }
  Assert.AreEqual(Double(0.25), Double(R), FLOAT_TOLERANCE,
    'EaseIn at T=0.5 should return 0.25');
end;


procedure TTestKenBurns.TestApplyEasing_EaseOut_Half;
var
  R: Single;
begin
  R:= ApplyEasing(0.5, etEaseOut);
  { EaseOut(0.5) = 1 - (1-0.5)*(1-0.5) = 1 - 0.25 = 0.75 }
  Assert.AreEqual(Double(0.75), Double(R), FLOAT_TOLERANCE,
    'EaseOut at T=0.5 should return 0.75');
end;


{ === ApplyEasing - boundary values for all types === }

procedure TTestKenBurns.TestApplyEasing_AllTypes_ZeroReturnsZero;
var
  E: TEasingType;
begin
  for E:= Low(TEasingType) to High(TEasingType) do
    Assert.AreEqual(Double(0.0), Double(ApplyEasing(0.0, E)), FLOAT_TOLERANCE,
      'All easing types at T=0 should return 0 (failed for ' + EasingName(E) + ')');
end;


procedure TTestKenBurns.TestApplyEasing_AllTypes_OneReturnsOne;
var
  E: TEasingType;
begin
  for E:= Low(TEasingType) to High(TEasingType) do
    Assert.AreEqual(Double(1.0), Double(ApplyEasing(1.0, E)), FLOAT_TOLERANCE,
      'All easing types at T=1 should return 1 (failed for ' + EasingName(E) + ')');
end;


{ === ApplyEasing - out-of-range clamping === }

procedure TTestKenBurns.TestApplyEasing_NegativeInput_ClampedToZero;
begin
  Assert.AreEqual(Double(0.0), Double(ApplyEasing(-0.5, etLinear)), FLOAT_TOLERANCE,
    'Negative input should be clamped to 0');
end;


procedure TTestKenBurns.TestApplyEasing_OverOneInput_ClampedToOne;
begin
  Assert.AreEqual(Double(1.0), Double(ApplyEasing(1.5, etLinear)), FLOAT_TOLERANCE,
    'Input > 1 should be clamped to 1');
end;


procedure TTestKenBurns.TestApplyEasing_LargeNegative_ClampedToZero;
begin
  Assert.AreEqual(Double(0.0), Double(ApplyEasing(-100.0, etSmoothStep)), FLOAT_TOLERANCE,
    'Large negative input should be clamped to 0');
end;


procedure TTestKenBurns.TestApplyEasing_LargePositive_ClampedToOne;
begin
  Assert.AreEqual(Double(1.0), Double(ApplyEasing(100.0, etEaseInOut)), FLOAT_TOLERANCE,
    'Large positive input should be clamped to 1');
end;


{ === LerpRectF === }

procedure TTestKenBurns.TestLerpRectF_AtZero_ReturnsA;
var
  A, B, R: TRectF;
begin
  A:= TRectF.Create(0.1, 0.2, 0.8, 0.9);
  B:= TRectF.Create(0.3, 0.4, 0.6, 0.7);
  R:= LerpRectF(A, B, 0.0);

  Assert.AreEqual(Double(A.Left),   Double(R.Left),   FLOAT_TOLERANCE, 'Left at T=0');
  Assert.AreEqual(Double(A.Top),    Double(R.Top),    FLOAT_TOLERANCE, 'Top at T=0');
  Assert.AreEqual(Double(A.Right),  Double(R.Right),  FLOAT_TOLERANCE, 'Right at T=0');
  Assert.AreEqual(Double(A.Bottom), Double(R.Bottom), FLOAT_TOLERANCE, 'Bottom at T=0');
end;


procedure TTestKenBurns.TestLerpRectF_AtOne_ReturnsB;
var
  A, B, R: TRectF;
begin
  A:= TRectF.Create(0.1, 0.2, 0.8, 0.9);
  B:= TRectF.Create(0.3, 0.4, 0.6, 0.7);
  R:= LerpRectF(A, B, 1.0);

  Assert.AreEqual(Double(B.Left),   Double(R.Left),   FLOAT_TOLERANCE, 'Left at T=1');
  Assert.AreEqual(Double(B.Top),    Double(R.Top),    FLOAT_TOLERANCE, 'Top at T=1');
  Assert.AreEqual(Double(B.Right),  Double(R.Right),  FLOAT_TOLERANCE, 'Right at T=1');
  Assert.AreEqual(Double(B.Bottom), Double(R.Bottom), FLOAT_TOLERANCE, 'Bottom at T=1');
end;


procedure TTestKenBurns.TestLerpRectF_AtHalf_ReturnsMidpoint;
var
  A, B, R: TRectF;
begin
  A:= TRectF.Create(0.0, 0.0, 1.0, 1.0);
  B:= TRectF.Create(0.2, 0.2, 0.8, 0.8);
  R:= LerpRectF(A, B, 0.5);

  Assert.AreEqual(Double(0.1), Double(R.Left),   FLOAT_TOLERANCE, 'Left at T=0.5');
  Assert.AreEqual(Double(0.1), Double(R.Top),    FLOAT_TOLERANCE, 'Top at T=0.5');
  Assert.AreEqual(Double(0.9), Double(R.Right),  FLOAT_TOLERANCE, 'Right at T=0.5');
  Assert.AreEqual(Double(0.9), Double(R.Bottom), FLOAT_TOLERANCE, 'Bottom at T=0.5');
end;


procedure TTestKenBurns.TestLerpRectF_IdenticalRects;
var
  A, R: TRectF;
begin
  A:= TRectF.Create(0.25, 0.25, 0.75, 0.75);
  R:= LerpRectF(A, A, 0.5);

  Assert.AreEqual(Double(A.Left),   Double(R.Left),   FLOAT_TOLERANCE, 'Identical rects Left');
  Assert.AreEqual(Double(A.Top),    Double(R.Top),    FLOAT_TOLERANCE, 'Identical rects Top');
  Assert.AreEqual(Double(A.Right),  Double(R.Right),  FLOAT_TOLERANCE, 'Identical rects Right');
  Assert.AreEqual(Double(A.Bottom), Double(R.Bottom), FLOAT_TOLERANCE, 'Identical rects Bottom');
end;


{ === GenerateRandomPath === }

procedure TTestKenBurns.TestGenerateRandomPath_RectsWithinBounds;
var
  Path: RKenBurnsPath;
begin
  RandSeed:= 42;
  Path:= GenerateRandomPath(0.3, 0.15);

  Assert.IsTrue(Path.StartRect.Left   >= 0.0, 'StartRect.Left >= 0');
  Assert.IsTrue(Path.StartRect.Top    >= 0.0, 'StartRect.Top >= 0');
  Assert.IsTrue(Path.StartRect.Right  <= 1.0, 'StartRect.Right <= 1');
  Assert.IsTrue(Path.StartRect.Bottom <= 1.0, 'StartRect.Bottom <= 1');

  Assert.IsTrue(Path.EndRect.Left   >= 0.0, 'EndRect.Left >= 0');
  Assert.IsTrue(Path.EndRect.Top    >= 0.0, 'EndRect.Top >= 0');
  Assert.IsTrue(Path.EndRect.Right  <= 1.0, 'EndRect.Right <= 1');
  Assert.IsTrue(Path.EndRect.Bottom <= 1.0, 'EndRect.Bottom <= 1');
end;


procedure TTestKenBurns.TestGenerateRandomPath_RectsHavePositiveSize;
var
  Path: RKenBurnsPath;
begin
  RandSeed:= 42;
  Path:= GenerateRandomPath(0.3, 0.15);

  Assert.IsTrue(Path.StartRect.Width  > 0, 'StartRect width must be positive');
  Assert.IsTrue(Path.StartRect.Height > 0, 'StartRect height must be positive');
  Assert.IsTrue(Path.EndRect.Width    > 0, 'EndRect width must be positive');
  Assert.IsTrue(Path.EndRect.Height   > 0, 'EndRect height must be positive');
end;


procedure TTestKenBurns.TestGenerateRandomPath_MultipleCalls_ProduceDifferentResults;
var
  Path1, Path2: RKenBurnsPath;
  Same: Boolean;
begin
  Randomize;
  Path1:= GenerateRandomPath(0.3, 0.15);
  Path2:= GenerateRandomPath(0.3, 0.15);

  { It is statistically improbable that two random paths are identical }
  Same:= SameValue(Path1.StartRect.Left, Path2.StartRect.Left, FLOAT_TOLERANCE) AND
         SameValue(Path1.StartRect.Top,  Path2.StartRect.Top,  FLOAT_TOLERANCE) AND
         SameValue(Path1.EndRect.Left,   Path2.EndRect.Left,   FLOAT_TOLERANCE) AND
         SameValue(Path1.EndRect.Top,    Path2.EndRect.Top,    FLOAT_TOLERANCE);

  Assert.IsFalse(Same, 'Two random paths should differ');
end;


procedure TTestKenBurns.TestGenerateRandomPath_ZeroZoomRange;
var
  Path: RKenBurnsPath;
begin
  RandSeed:= 42;
  Path:= GenerateRandomPath(0.0, 0.15);

  { With zero zoom range, both rects should be full size (1.0) }
  Assert.IsTrue(Path.StartRect.Width  > 0, 'Width must be positive');
  Assert.IsTrue(Path.StartRect.Height > 0, 'Height must be positive');
  Assert.IsTrue(Path.EndRect.Width    > 0, 'End width must be positive');
  Assert.IsTrue(Path.EndRect.Height   > 0, 'End height must be positive');
end;


procedure TTestKenBurns.TestGenerateRandomPath_ZeroPanRange;
var
  Path: RKenBurnsPath;
begin
  RandSeed:= 42;
  Path:= GenerateRandomPath(0.3, 0.0);

  { With zero pan range, centers should be at 0.5 and drift should be 0 }
  Assert.IsTrue(Path.StartRect.Left   >= 0.0, 'StartRect.Left >= 0');
  Assert.IsTrue(Path.StartRect.Right  <= 1.0, 'StartRect.Right <= 1');
  Assert.IsTrue(Path.EndRect.Left     >= 0.0, 'EndRect.Left >= 0');
  Assert.IsTrue(Path.EndRect.Right    <= 1.0, 'EndRect.Right <= 1');
end;


procedure TTestKenBurns.TestGenerateRandomPath_LargeZoomRange;
var
  Path: RKenBurnsPath;
begin
  { Large zoom range - sizes get clamped to 0.4 minimum }
  RandSeed:= 42;
  Path:= GenerateRandomPath(5.0, 0.15);

  Assert.IsTrue(Path.StartRect.Width  >= 0.39, 'StartRect width >= 0.4 (with tolerance)');
  Assert.IsTrue(Path.StartRect.Height >= 0.39, 'StartRect height >= 0.4 (with tolerance)');
  Assert.IsTrue(Path.EndRect.Width    >= 0.39, 'EndRect width >= 0.4 (with tolerance)');
  Assert.IsTrue(Path.EndRect.Height   >= 0.39, 'EndRect height >= 0.4 (with tolerance)');
end;


procedure TTestKenBurns.TestGenerateRandomPath_LargePanRange;
var
  Path: RKenBurnsPath;
begin
  { Large pan range - rects should still be clamped within 0..1 }
  RandSeed:= 42;
  Path:= GenerateRandomPath(0.3, 5.0);

  Assert.IsTrue(Path.StartRect.Left   >= 0.0, 'StartRect.Left >= 0');
  Assert.IsTrue(Path.StartRect.Top    >= 0.0, 'StartRect.Top >= 0');
  Assert.IsTrue(Path.StartRect.Right  <= 1.0, 'StartRect.Right <= 1');
  Assert.IsTrue(Path.StartRect.Bottom <= 1.0, 'StartRect.Bottom <= 1');
  Assert.IsTrue(Path.EndRect.Left     >= 0.0, 'EndRect.Left >= 0');
  Assert.IsTrue(Path.EndRect.Top      >= 0.0, 'EndRect.Top >= 0');
  Assert.IsTrue(Path.EndRect.Right    <= 1.0, 'EndRect.Right <= 1');
  Assert.IsTrue(Path.EndRect.Bottom   <= 1.0, 'EndRect.Bottom <= 1');
end;


procedure TTestKenBurns.TestGenerateRandomPath_NegativeInputs;
var
  Path: RKenBurnsPath;
begin
  { Negative inputs should be treated as positive (Abs) }
  RandSeed:= 42;
  Path:= GenerateRandomPath(-0.3, -0.15);

  Assert.IsTrue(Path.StartRect.Left   >= 0.0, 'StartRect.Left >= 0');
  Assert.IsTrue(Path.StartRect.Right  <= 1.0, 'StartRect.Right <= 1');
  Assert.IsTrue(Path.StartRect.Width  > 0, 'StartRect width must be positive');
  Assert.IsTrue(Path.EndRect.Width    > 0, 'EndRect width must be positive');
end;


procedure TTestKenBurns.TestGenerateRandomPath_100Iterations_AllValid;
var
  i: Integer;
  Path: RKenBurnsPath;
begin
  { Stress test: run 100 iterations with various parameters and verify all results }
  Randomize;
  for i:= 1 to 100 do
  begin
    Path:= GenerateRandomPath(Random * 2.0, Random * 2.0);

    Assert.IsTrue(Path.StartRect.Left   >= 0.0, Format('Iter %d: StartRect.Left < 0', [i]));
    Assert.IsTrue(Path.StartRect.Top    >= 0.0, Format('Iter %d: StartRect.Top < 0', [i]));
    Assert.IsTrue(Path.StartRect.Right  <= 1.0, Format('Iter %d: StartRect.Right > 1', [i]));
    Assert.IsTrue(Path.StartRect.Bottom <= 1.0, Format('Iter %d: StartRect.Bottom > 1', [i]));
    Assert.IsTrue(Path.StartRect.Width  > 0,    Format('Iter %d: StartRect zero/negative width', [i]));
    Assert.IsTrue(Path.StartRect.Height > 0,    Format('Iter %d: StartRect zero/negative height', [i]));

    Assert.IsTrue(Path.EndRect.Left   >= 0.0, Format('Iter %d: EndRect.Left < 0', [i]));
    Assert.IsTrue(Path.EndRect.Top    >= 0.0, Format('Iter %d: EndRect.Top < 0', [i]));
    Assert.IsTrue(Path.EndRect.Right  <= 1.0, Format('Iter %d: EndRect.Right > 1', [i]));
    Assert.IsTrue(Path.EndRect.Bottom <= 1.0, Format('Iter %d: EndRect.Bottom > 1', [i]));
    Assert.IsTrue(Path.EndRect.Width  > 0,    Format('Iter %d: EndRect zero/negative width', [i]));
    Assert.IsTrue(Path.EndRect.Height > 0,    Format('Iter %d: EndRect zero/negative height', [i]));
  end;
end;


{ === EasingName === }

procedure TTestKenBurns.TestEasingName_AllValues_NonEmpty;
var
  E: TEasingType;
begin
  for E:= Low(TEasingType) to High(TEasingType) do
    Assert.IsNotEmpty(EasingName(E), 'EasingName should return non-empty string for ' + Ord(E).ToString);
end;


procedure TTestKenBurns.TestEasingName_Linear;
begin
  Assert.AreEqual('Linear', EasingName(etLinear));
end;


procedure TTestKenBurns.TestEasingName_SmoothStep;
begin
  Assert.AreEqual('Smooth', EasingName(etSmoothStep));
end;


procedure TTestKenBurns.TestEasingName_EaseInOut;
begin
  Assert.AreEqual('Ease In/Out', EasingName(etEaseInOut));
end;


procedure TTestKenBurns.TestEasingName_EaseIn;
begin
  Assert.AreEqual('Ease In', EasingName(etEaseIn));
end;


procedure TTestKenBurns.TestEasingName_EaseOut;
begin
  Assert.AreEqual('Ease Out', EasingName(etEaseOut));
end;


{ === IndexToEasing === }

procedure TTestKenBurns.TestIndexToEasing_ValidIndices;
var
  E: TEasingType;
begin
  for E:= Low(TEasingType) to High(TEasingType) do
    Assert.AreEqual(Ord(E), Ord(IndexToEasing(Ord(E))),
      'IndexToEasing(' + Ord(E).ToString + ') should round-trip');
end;


procedure TTestKenBurns.TestIndexToEasing_NegativeIndex_ReturnsSmoothStep;
begin
  Assert.AreEqual(Ord(etSmoothStep), Ord(IndexToEasing(-1)),
    'Negative index should return etSmoothStep');
end;


procedure TTestKenBurns.TestIndexToEasing_TooLargeIndex_ReturnsSmoothStep;
begin
  Assert.AreEqual(Ord(etSmoothStep), Ord(IndexToEasing(100)),
    'Too-large index should return etSmoothStep');
end;


procedure TTestKenBurns.TestIndexToEasing_BoundaryLow;
begin
  Assert.AreEqual(Ord(Low(TEasingType)), Ord(IndexToEasing(Ord(Low(TEasingType)))),
    'Lowest valid index should return first easing type');
end;


procedure TTestKenBurns.TestIndexToEasing_BoundaryHigh;
begin
  Assert.AreEqual(Ord(High(TEasingType)), Ord(IndexToEasing(Ord(High(TEasingType)))),
    'Highest valid index should return last easing type');
end;


{ === KenBurnsFrame === }

procedure TTestKenBurns.TestKenBurnsFrame_ValidInputs_NoException;
var
  Path: RKenBurnsPath;
begin
  RandSeed:= 42;
  Path:= GenerateRandomPath(0.3, 0.15);

  Assert.WillNotRaise(
    procedure
    begin
      KenBurnsFrame(FSource, FOutput, 0.5, Path, etSmoothStep);
    end);
end;


procedure TTestKenBurns.TestKenBurnsFrame_AllEasingTypes_NoException;
var
  Path: RKenBurnsPath;
  E: TEasingType;
begin
  RandSeed:= 42;
  Path:= GenerateRandomPath(0.3, 0.15);

  for E:= Low(TEasingType) to High(TEasingType) do
    Assert.WillNotRaise(
      procedure
      begin
        KenBurnsFrame(FSource, FOutput, 0.5, Path, E);
      end,
      'KenBurnsFrame should not raise for easing type ' + EasingName(E));
end;


procedure TTestKenBurns.TestKenBurnsFrame_ProgressBoundaries;
var
  Path: RKenBurnsPath;
begin
  RandSeed:= 42;
  Path:= GenerateRandomPath(0.3, 0.15);

  Assert.WillNotRaise(
    procedure
    begin
      KenBurnsFrame(FSource, FOutput, 0.0, Path, etLinear);
      KenBurnsFrame(FSource, FOutput, 1.0, Path, etLinear);
    end,
    'KenBurnsFrame should handle progress 0 and 1');
end;


procedure TTestKenBurns.TestKenBurnsFrame_ProgressOutOfRange_Clamped;
var
  Path: RKenBurnsPath;
begin
  RandSeed:= 42;
  Path:= GenerateRandomPath(0.3, 0.15);

  { Progress values outside 0..1 should be clamped, not crash }
  Assert.WillNotRaise(
    procedure
    begin
      KenBurnsFrame(FSource, FOutput, -0.5, Path, etLinear);
      KenBurnsFrame(FSource, FOutput, 1.5, Path, etLinear);
    end,
    'KenBurnsFrame should clamp out-of-range progress');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestKenBurns);

end.
