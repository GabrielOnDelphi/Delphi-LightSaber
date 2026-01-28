unit Test.LightCore.Math;

{=============================================================================================================
   Unit tests for LightCore.Math
   Tests math functions: min/max, range, percent, round, median, factorial

   Requires: TESTINSIGHT compiler directive for TestInsight integration
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Types;

type
  [TestFixture]
  TTestLightCoreMath = class
  public
    { Min/Max Tests }
    [Test]
    procedure TestMin3_AllDifferent;

    [Test]
    procedure TestMin3_AllSame;

    [Test]
    procedure TestMin3_TwoSame;

    [Test]
    procedure TestMin3S_Floats;

    [Test]
    procedure TestFind_Max_Integer;

    [Test]
    procedure TestFind_Max_Cardinal;

    [Test]
    procedure TestFind_Max_Double;

    { Range Tests }
    [Test]
    procedure TestNotHigherThan;

    [Test]
    procedure TestNotSmallerThan_Integer;

    [Test]
    procedure TestNotSmallerThan_Real;

    [Test]
    procedure TestEnsureZero;

    [Test]
    procedure TestEnsureRange;

    { Percent Tests }
    [Test]
    procedure TestProcentNormal;

    [Test]
    procedure TestProcentRepresent_Int64;

    [Test]
    procedure TestProcentRepresent_Extended;

    { Round Tests }
    [Test]
    procedure TestRoundEx_Up;

    [Test]
    procedure TestRoundEx_Down;

    [Test]
    procedure TestRoundEx_Exact;

    [Test]
    procedure TestRoundUp;

    [Test]
    procedure TestRoundDown;

    [Test]
    procedure TestRoundTo_Category5;

    [Test]
    procedure TestRoundTo_Category10;

    [Test]
    procedure TestBeautifyPrice;

    [Test]
    procedure TestSameValue_True;

    [Test]
    procedure TestSameValue_False;

    { Median/Interquartile Tests }
    [Test]
    procedure TestMedian_Double_Odd;

    [Test]
    procedure TestMedian_Double_Even;

    [Test]
    procedure TestMedian_Integer_Odd;

    [Test]
    procedure TestMedian_Integer_Even;

    [Test]
    procedure TestMean;

    [Test]
    procedure TestInterq25;

    [Test]
    procedure TestInterq75;

    { Basic Math Tests }
    [Test]
    procedure TestFactorial_0;

    [Test]
    procedure TestFactorial_1;

    [Test]
    procedure TestFactorial_5;

    [Test]
    procedure TestFactorial_10;

    [Test]
    procedure TestCombinations;

    { Other Tests }
    [Test]
    procedure TestGenerateRandomBoolean;
  end;

implementation

uses
  LightCore.Math;

{ Min/Max Tests }

procedure TTestLightCoreMath.TestMin3_AllDifferent;
begin
  Assert.AreEqual(1, Min3(3, 1, 2));
  Assert.AreEqual(1, Min3(1, 2, 3));
  Assert.AreEqual(1, Min3(2, 3, 1));
end;

procedure TTestLightCoreMath.TestMin3_AllSame;
begin
  Assert.AreEqual(5, Min3(5, 5, 5));
end;

procedure TTestLightCoreMath.TestMin3_TwoSame;
begin
  Assert.AreEqual(2, Min3(2, 2, 3));
  Assert.AreEqual(2, Min3(3, 2, 2));
end;

procedure TTestLightCoreMath.TestMin3S_Floats;
begin
  Assert.AreEqual(Single(1.5), Min3S(3.5, 1.5, 2.5));
  Assert.AreEqual(Single(-1.0), Min3S(-1.0, 0.0, 1.0));
end;

procedure TTestLightCoreMath.TestFind_Max_Integer;
begin
  Assert.AreEqual(3, Find_Max(1, 2, 3));
  Assert.AreEqual(3, Find_Max(3, 2, 1));
  Assert.AreEqual(3, Find_Max(2, 3, 1));
  Assert.AreEqual(5, Find_Max(5, 5, 5));
end;

procedure TTestLightCoreMath.TestFind_Max_Cardinal;
begin
  Assert.AreEqual(Cardinal(300), Find_Max(Cardinal(100), Cardinal(200), Cardinal(300)));
end;

procedure TTestLightCoreMath.TestFind_Max_Double;
begin
  Assert.AreEqual(Double(3.5), Find_Max(1.5, 2.5, 3.5));
end;

{ Range Tests }

procedure TTestLightCoreMath.TestNotHigherThan;
var
  Value: Integer;
begin
  Value:= 150;
  NotHigherThan(Value, 100);
  Assert.AreEqual(100, Value);

  Value:= 50;
  NotHigherThan(Value, 100);
  Assert.AreEqual(50, Value);
end;

procedure TTestLightCoreMath.TestNotSmallerThan_Integer;
var
  Value: Integer;
begin
  Value:= 5;
  NotSmallerThan(Value, 10);
  Assert.AreEqual(10, Value);

  Value:= 50;
  NotSmallerThan(Value, 10);
  Assert.AreEqual(50, Value);
end;

procedure TTestLightCoreMath.TestNotSmallerThan_Real;
var
  Value: Real;
begin
  Value:= 5.0;
  NotSmallerThan(Value, 10);
  Assert.AreEqual(Double(10.0), Double(Value));

  Value:= 50.0;
  NotSmallerThan(Value, 10);
  Assert.AreEqual(Double(50.0), Double(Value));
end;

procedure TTestLightCoreMath.TestEnsureZero;
var
  Value: Integer;
begin
  Value:= -5;
  EnsureZero(Value);
  Assert.AreEqual(0, Value);

  Value:= 5;
  EnsureZero(Value);
  Assert.AreEqual(5, Value);
end;

procedure TTestLightCoreMath.TestEnsureRange;
var
  Value: Integer;
begin
  Value:= 5;
  EnsureRange(Value, 10, 20);
  Assert.AreEqual(10, Value);

  Value:= 25;
  EnsureRange(Value, 10, 20);
  Assert.AreEqual(20, Value);

  Value:= 15;
  EnsureRange(Value, 10, 20);
  Assert.AreEqual(15, Value);
end;

{ Percent Tests }

procedure TTestLightCoreMath.TestProcentNormal;
begin
  { What is 10% of 20? Answer: 2 }
  Assert.AreEqual(Double(2), Double(ProcentNormal(10, 20)));
  { What is 25% of 100? Answer: 25 }
  Assert.AreEqual(Double(25), Double(ProcentNormal(25, 100)));
  { What is 100% of 50? Answer: 50 }
  Assert.AreEqual(Double(50), Double(ProcentNormal(100, 50)));
end;

procedure TTestLightCoreMath.TestProcentRepresent_Int64;
begin
  { 5 is what percent of 20? Answer: 25% }
  Assert.AreEqual(Double(25), Double(ProcentRepresent(Int64(5), Int64(20))));
  { 25 is what percent of 100? Answer: 25% }
  Assert.AreEqual(Double(25), Double(ProcentRepresent(Int64(25), Int64(100))));
  { 100 is what percent of 50? Answer: 200% }
  Assert.AreEqual(Double(200), Double(ProcentRepresent(Int64(100), Int64(50))));
end;

procedure TTestLightCoreMath.TestProcentRepresent_Extended;
begin
  Assert.AreEqual(Double(50), Double(ProcentRepresent(Extended(10), Extended(20))));
end;

{ Round Tests }

procedure TTestLightCoreMath.TestRoundEx_Up;
begin
  Assert.AreEqual(LongInt(3), RoundEx(2.5));
  Assert.AreEqual(LongInt(4), RoundEx(3.7));
end;

procedure TTestLightCoreMath.TestRoundEx_Down;
begin
  Assert.AreEqual(LongInt(2), RoundEx(2.4));
  Assert.AreEqual(LongInt(3), RoundEx(3.2));
end;

procedure TTestLightCoreMath.TestRoundEx_Exact;
begin
  Assert.AreEqual(LongInt(5), RoundEx(5.0));
end;

procedure TTestLightCoreMath.TestRoundUp;
begin
  Assert.AreEqual(LongInt(3), RoundUp(2.1));
  Assert.AreEqual(LongInt(3), RoundUp(2.9));
  Assert.AreEqual(LongInt(2), RoundUp(2.0));  { No fractional part }
end;

procedure TTestLightCoreMath.TestRoundDown;
begin
  Assert.AreEqual(LongInt(2), RoundDown(2.9));
  Assert.AreEqual(LongInt(2), RoundDown(2.1));
  Assert.AreEqual(LongInt(2), RoundDown(2.0));
end;

procedure TTestLightCoreMath.TestRoundTo_Category5;
begin
  { 72 rounded to nearest 5 = 75 }
  Assert.AreEqual(LongInt(75), RoundTo(72, 5));
  { 71 rounded to nearest 5 = 75 }
  Assert.AreEqual(LongInt(75), RoundTo(71, 5));
end;

procedure TTestLightCoreMath.TestRoundTo_Category10;
begin
  { 72 rounded to nearest 10 = 80 }
  Assert.AreEqual(LongInt(80), RoundTo(72, 10));
  { 75 rounded to nearest 10 = 80 }
  Assert.AreEqual(LongInt(80), RoundTo(75, 10));
end;

procedure TTestLightCoreMath.TestBeautifyPrice;
begin
  { 341 -> 345 - 0.1 = 344.9 }
  Assert.AreEqual(Double(344.9), Double(BeautifyPrice(341)));
  { 346 -> 350 - 0.1 = 349.9 }
  Assert.AreEqual(Double(349.9), Double(BeautifyPrice(346)));
end;

procedure TTestLightCoreMath.TestSameValue_True;
begin
  Assert.IsTrue(SameValue(1.005, 1.005));
  Assert.IsTrue(SameValue(1.0, 1.005));  { Within 0.01 tolerance }
end;

procedure TTestLightCoreMath.TestSameValue_False;
begin
  Assert.IsFalse(SameValue(1.0, 1.02));  { Outside 0.01 tolerance }
  Assert.IsFalse(SameValue(1.0, 2.0));
end;

{ Median/Interquartile Tests }

procedure TTestLightCoreMath.TestMedian_Double_Odd;
var
  Arr: TDoubleDynArray;
begin
  Arr:= TDoubleDynArray.Create(4.1, 5.6, 7.2, 1.7, 9.3);
  Assert.AreEqual(Double(5.6), Median(Arr));
end;

procedure TTestLightCoreMath.TestMedian_Double_Even;
var
  Arr: TDoubleDynArray;
begin
  Arr:= TDoubleDynArray.Create(1.0, 2.0, 3.0, 4.0);
  Assert.AreEqual(Double(2.5), Median(Arr));
end;

procedure TTestLightCoreMath.TestMedian_Integer_Odd;
var
  Arr: TIntegerDynArray;
begin
  Arr:= TIntegerDynArray.Create(1, 2, 3, 4, 5);
  Assert.AreEqual(3, Median(Arr));
end;

procedure TTestLightCoreMath.TestMedian_Integer_Even;
var
  Arr: TIntegerDynArray;
begin
  Arr:= TIntegerDynArray.Create(1, 2, 3, 4);
  Assert.AreEqual(3, Median(Arr));  { (2+3)/2 = 2.5, rounded = 3 }
end;

procedure TTestLightCoreMath.TestMean;
var
  Arr: TIntegerDynArray;
begin
  Arr:= TIntegerDynArray.Create(10, 20, 30, 40);
  Assert.AreEqual(25, Mean(Arr));
end;

procedure TTestLightCoreMath.TestInterq25;
var
  Arr: TIntegerDynArray;
begin
  { For array [1,2,3,4,5,6,7,8,9,10,11,12], Q25 is around position 3 }
  Arr:= TIntegerDynArray.Create(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12);
  Assert.IsTrue(Interq25(Arr) > 0);  { Just verify it returns something reasonable }
end;

procedure TTestLightCoreMath.TestInterq75;
var
  Arr: TIntegerDynArray;
begin
  Arr:= TIntegerDynArray.Create(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12);
  Assert.IsTrue(Interq75(Arr) > Interq25(Arr));  { Q75 should be greater than Q25 }
end;

{ Basic Math Tests }

procedure TTestLightCoreMath.TestFactorial_0;
begin
  { 0! = 1 by mathematical definition }
  Assert.AreEqual(Int64(1), Factorial(0));
end;

procedure TTestLightCoreMath.TestFactorial_1;
begin
  Assert.AreEqual(Int64(1), Factorial(1));
end;

procedure TTestLightCoreMath.TestFactorial_5;
begin
  { 5! = 5*4*3*2*1 = 120 }
  Assert.AreEqual(Int64(120), Factorial(5));
end;

procedure TTestLightCoreMath.TestFactorial_10;
begin
  { 10! = 3628800 }
  Assert.AreEqual(Int64(3628800), Factorial(10));
end;

procedure TTestLightCoreMath.TestCombinations;
begin
  { C(5,2) = 5!/(3!*2!) = 120/(6*2) = 10 }
  Assert.AreEqual(Int64(10), Combinations(5, 2));
  { C(6,3) = 6!/(3!*3!) = 720/(6*6) = 20 }
  Assert.AreEqual(Int64(20), Combinations(6, 3));
end;

{ Other Tests }

procedure TTestLightCoreMath.TestGenerateRandomBoolean;
var
  TrueCount, FalseCount, i: Integer;
begin
  TrueCount:= 0;
  FalseCount:= 0;

  { Run many times to ensure both values are possible }
  for i:= 1 to 1000 do
    if GenerateRandomBoolean
    then Inc(TrueCount)
    else Inc(FalseCount);

  { Both should have occurred at least once }
  Assert.IsTrue(TrueCount > 0, 'True should occur at least once');
  Assert.IsTrue(FalseCount > 0, 'False should occur at least once');
end;

initialization
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  {$ENDIF}
  TDUnitX.RegisterTestFixture(TTestLightCoreMath);

end.
