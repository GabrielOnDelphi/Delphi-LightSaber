unit Test.LightCore.RttiSetToString;

{=============================================================================================================
   Unit tests for LightCore.RttiSetToString
   Tests set-to-string and string-to-set conversions using RTTI

   Requires: TESTINSIGHT compiler directive for TestInsight integration
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.TypInfo,
  LightCore.RttiSetToString;

type
  { Test enum and set types }
  TTestEnum = (teFirst, teSecond, teThird, teFourth, teFifth);
  TTestSet = set of TTestEnum;

  TByteEnum = (beZero, beOne, beTwo, beThree, beFour, beFive, beSix, beSeven);
  TByteSet = set of TByteEnum;

  [TestFixture]
  TTestRttiSetToString = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { SetToString Tests }
    [Test]
    procedure TestSetToString_EmptySet;

    [Test]
    procedure TestSetToString_SingleElement;

    [Test]
    procedure TestSetToString_MultipleElements;

    [Test]
    procedure TestSetToString_AllElements;

    [Test]
    procedure TestSetToString_FirstAndLast;

    { StringToSet Tests }
    [Test]
    procedure TestStringToSet_EmptyString;

    [Test]
    procedure TestStringToSet_SingleElement;

    [Test]
    procedure TestStringToSet_MultipleElements;

    [Test]
    procedure TestStringToSet_WithBrackets;

    [Test]
    procedure TestStringToSet_WithSpaces;

    [Test]
    procedure TestStringToSet_WithBracketsAndSpaces;

    [Test]
    procedure TestStringToSet_InvalidElement;

    [Test]
    procedure TestStringToSet_PartiallyInvalid;

    { Round-trip Tests }
    [Test]
    procedure TestRoundTrip_EmptySet;

    [Test]
    procedure TestRoundTrip_SingleElement;

    [Test]
    procedure TestRoundTrip_MultipleElements;

    [Test]
    procedure TestRoundTrip_AllElements;

    { ByteSet Tests (smaller storage) }
    [Test]
    procedure TestByteSet_SetToString;

    [Test]
    procedure TestByteSet_StringToSet;

    [Test]
    procedure TestByteSet_RoundTrip;

    { Edge Cases }
    [Test]
    procedure TestCaseSensitivity;

    [Test]
    procedure TestLeadingTrailingSpaces;
  end;

implementation


procedure TTestRttiSetToString.Setup;
begin
  { No setup needed }
end;


procedure TTestRttiSetToString.TearDown;
begin
  { No teardown needed }
end;


{ SetToString Tests }

procedure TTestRttiSetToString.TestSetToString_EmptySet;
var
  TestSet: TTestSet;
  Result: string;
begin
  TestSet:= [];
  Result:= SetToString(TypeInfo(TTestSet), TestSet);
  Assert.AreEqual('', Result);
end;


procedure TTestRttiSetToString.TestSetToString_SingleElement;
var
  TestSet: TTestSet;
  Result: string;
begin
  TestSet:= [teSecond];
  Result:= SetToString(TypeInfo(TTestSet), TestSet);
  Assert.AreEqual('teSecond', Result);
end;


procedure TTestRttiSetToString.TestSetToString_MultipleElements;
var
  TestSet: TTestSet;
  Result: string;
begin
  TestSet:= [teFirst, teThird];
  Result:= SetToString(TypeInfo(TTestSet), TestSet);
  Assert.AreEqual('teFirst,teThird', Result);
end;


procedure TTestRttiSetToString.TestSetToString_AllElements;
var
  TestSet: TTestSet;
  Result: string;
begin
  TestSet:= [teFirst, teSecond, teThird, teFourth, teFifth];
  Result:= SetToString(TypeInfo(TTestSet), TestSet);
  Assert.AreEqual('teFirst,teSecond,teThird,teFourth,teFifth', Result);
end;


procedure TTestRttiSetToString.TestSetToString_FirstAndLast;
var
  TestSet: TTestSet;
  Result: string;
begin
  TestSet:= [teFirst, teFifth];
  Result:= SetToString(TypeInfo(TTestSet), TestSet);
  Assert.AreEqual('teFirst,teFifth', Result);
end;


{ StringToSet Tests }

procedure TTestRttiSetToString.TestStringToSet_EmptyString;
var
  TestSet: TTestSet;
begin
  TestSet:= [teFirst]; { Pre-fill to verify it gets cleared }
  StringToSet(TypeInfo(TTestSet), TestSet, '');
  Assert.AreEqual(0, Byte(TestSet));
end;


procedure TTestRttiSetToString.TestStringToSet_SingleElement;
var
  TestSet: TTestSet;
begin
  TestSet:= [];
  StringToSet(TypeInfo(TTestSet), TestSet, 'teSecond');
  Assert.IsTrue(teSecond in TestSet);
  Assert.IsFalse(teFirst in TestSet);
end;


procedure TTestRttiSetToString.TestStringToSet_MultipleElements;
var
  TestSet: TTestSet;
begin
  TestSet:= [];
  StringToSet(TypeInfo(TTestSet), TestSet, 'teFirst,teThird,teFifth');
  Assert.IsTrue(teFirst in TestSet);
  Assert.IsFalse(teSecond in TestSet);
  Assert.IsTrue(teThird in TestSet);
  Assert.IsFalse(teFourth in TestSet);
  Assert.IsTrue(teFifth in TestSet);
end;


procedure TTestRttiSetToString.TestStringToSet_WithBrackets;
var
  TestSet: TTestSet;
begin
  TestSet:= [];
  StringToSet(TypeInfo(TTestSet), TestSet, '[teFirst,teSecond]');
  Assert.IsTrue(teFirst in TestSet);
  Assert.IsTrue(teSecond in TestSet);
  Assert.IsFalse(teThird in TestSet);
end;


procedure TTestRttiSetToString.TestStringToSet_WithSpaces;
var
  TestSet: TTestSet;
begin
  TestSet:= [];
  StringToSet(TypeInfo(TTestSet), TestSet, 'teFirst, teSecond, teThird');
  Assert.IsTrue(teFirst in TestSet);
  Assert.IsTrue(teSecond in TestSet);
  Assert.IsTrue(teThird in TestSet);
end;


procedure TTestRttiSetToString.TestStringToSet_WithBracketsAndSpaces;
var
  TestSet: TTestSet;
begin
  TestSet:= [];
  StringToSet(TypeInfo(TTestSet), TestSet, '[teFirst, teSecond, teThird]');
  Assert.IsTrue(teFirst in TestSet);
  Assert.IsTrue(teSecond in TestSet);
  Assert.IsTrue(teThird in TestSet);
end;


procedure TTestRttiSetToString.TestStringToSet_InvalidElement;
var
  TestSet: TTestSet;
begin
  TestSet:= [teFirst]; { Pre-fill }
  StringToSet(TypeInfo(TTestSet), TestSet, 'InvalidElement');
  Assert.AreEqual(0, Byte(TestSet), 'Invalid element should result in empty set');
end;


procedure TTestRttiSetToString.TestStringToSet_PartiallyInvalid;
var
  TestSet: TTestSet;
begin
  TestSet:= [teFirst]; { Pre-fill }
  StringToSet(TypeInfo(TTestSet), TestSet, 'teFirst,InvalidElement,teSecond');
  Assert.AreEqual(0, Byte(TestSet), 'Partially invalid should result in empty set');
end;


{ Round-trip Tests }

procedure TTestRttiSetToString.TestRoundTrip_EmptySet;
var
  Original, Restored: TTestSet;
  Str: string;
begin
  Original:= [];
  Str:= SetToString(TypeInfo(TTestSet), Original);
  Restored:= [teFirst]; { Pre-fill }
  StringToSet(TypeInfo(TTestSet), Restored, Str);
  Assert.AreEqual(Byte(Original), Byte(Restored));
end;


procedure TTestRttiSetToString.TestRoundTrip_SingleElement;
var
  Original, Restored: TTestSet;
  Str: string;
begin
  Original:= [teThird];
  Str:= SetToString(TypeInfo(TTestSet), Original);
  Restored:= [];
  StringToSet(TypeInfo(TTestSet), Restored, Str);
  Assert.AreEqual(Byte(Original), Byte(Restored));
end;


procedure TTestRttiSetToString.TestRoundTrip_MultipleElements;
var
  Original, Restored: TTestSet;
  Str: string;
begin
  Original:= [teFirst, teThird, teFifth];
  Str:= SetToString(TypeInfo(TTestSet), Original);
  Restored:= [];
  StringToSet(TypeInfo(TTestSet), Restored, Str);
  Assert.AreEqual(Byte(Original), Byte(Restored));
end;


procedure TTestRttiSetToString.TestRoundTrip_AllElements;
var
  Original, Restored: TTestSet;
  Str: string;
begin
  Original:= [teFirst, teSecond, teThird, teFourth, teFifth];
  Str:= SetToString(TypeInfo(TTestSet), Original);
  Restored:= [];
  StringToSet(TypeInfo(TTestSet), Restored, Str);
  Assert.AreEqual(Byte(Original), Byte(Restored));
end;


{ ByteSet Tests }

procedure TTestRttiSetToString.TestByteSet_SetToString;
var
  TestSet: TByteSet;
  Result: string;
begin
  TestSet:= [beOne, beThree, beFive];
  Result:= SetToString(TypeInfo(TByteSet), TestSet);
  Assert.AreEqual('beOne,beThree,beFive', Result);
end;


procedure TTestRttiSetToString.TestByteSet_StringToSet;
var
  TestSet: TByteSet;
begin
  TestSet:= [];
  StringToSet(TypeInfo(TByteSet), TestSet, 'beTwo,beFour,beSix');
  Assert.IsTrue(beTwo in TestSet);
  Assert.IsTrue(beFour in TestSet);
  Assert.IsTrue(beSix in TestSet);
  Assert.IsFalse(beZero in TestSet);
end;


procedure TTestRttiSetToString.TestByteSet_RoundTrip;
var
  Original, Restored: TByteSet;
  Str: string;
begin
  Original:= [beZero, beThree, beSeven];
  Str:= SetToString(TypeInfo(TByteSet), Original);
  Restored:= [];
  StringToSet(TypeInfo(TByteSet), Restored, Str);
  Assert.AreEqual(Byte(Original), Byte(Restored));
end;


{ Edge Cases }

procedure TTestRttiSetToString.TestCaseSensitivity;
var
  TestSet: TTestSet;
begin
  TestSet:= [teFirst];
  { Enum names are case-sensitive in RTTI }
  StringToSet(TypeInfo(TTestSet), TestSet, 'TEFIRST');
  Assert.AreEqual(0, Byte(TestSet), 'Wrong case should result in empty set');
end;


procedure TTestRttiSetToString.TestLeadingTrailingSpaces;
var
  TestSet: TTestSet;
begin
  TestSet:= [];
  StringToSet(TypeInfo(TTestSet), TestSet, '  [teFirst, teSecond]  ');
  Assert.IsTrue(teFirst in TestSet);
  Assert.IsTrue(teSecond in TestSet);
end;


initialization
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  {$ENDIF}
  TDUnitX.RegisterTestFixture(TTestRttiSetToString);

end.
