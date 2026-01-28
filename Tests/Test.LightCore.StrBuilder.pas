unit Test.LightCore.StrBuilder;

{=============================================================================================================
   Unit tests for LightCore.StrBuilder
   Tests TCStringBuilder - fast string builder implementation

   Requires: TESTINSIGHT compiler directive for TestInsight integration
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils;

type
  [TestFixture]
  TTestLightCoreStrBuilder = class
  public
    { Basic Operations Tests }
    [Test]
    procedure TestCreate;

    [Test]
    procedure TestCreate_CustomBufferSize;

    [Test]
    procedure TestAddChar_Single;

    [Test]
    procedure TestAddChar_Multiple;

    [Test]
    procedure TestAddEnter;

    [Test]
    procedure TestAddEnter_Multiple;

    [Test]
    procedure TestAsText;

    [Test]
    procedure TestAsText_CalledMultipleTimes;

    [Test]
    procedure TestAsText_AddContentAfterAsText;

    [Test]
    procedure TestClear;

    { Buffer Tests }
    [Test]
    procedure TestBufferGrowth;

    [Test]
    procedure TestBufferGrowth_MultipleGrowths;

    [Test]
    procedure TestSmallBuffer;

    [Test]
    procedure TestExactBufferBoundary;

    [Test]
    procedure TestBufferBoundaryPlusOne;

    { Combined Operations Tests }
    [Test]
    procedure TestAddCharsAndEnters;

    [Test]
    procedure TestBuildSentence;

    [Test]
    procedure TestReuseAfterClear;

    [Test]
    procedure TestClearAndRebuildLarger;

    { Edge Cases Tests }
    [Test]
    procedure TestEmptyResult;

    [Test]
    procedure TestSpecialCharacters;

    [Test]
    procedure TestUnicodeCharacters;

    [Test]
    procedure TestHighUnicodeCharacters;

    { Stress Tests }
    [Test]
    procedure TestLargeString;

    [Test]
    procedure TestEnterAtBufferBoundary;
  end;

implementation

uses
  LightCore.StrBuilder;

{ Basic Operations Tests }

procedure TTestLightCoreStrBuilder.TestCreate;
var
  SB: TCStringBuilder;
begin
  SB:= TCStringBuilder.Create;
  try
    Assert.AreEqual(10000, SB.BufferGrowth);
    Assert.AreEqual('', SB.AsText);
  finally
    FreeAndNil(SB);
  end;
end;

procedure TTestLightCoreStrBuilder.TestAddChar_Single;
var
  SB: TCStringBuilder;
begin
  SB:= TCStringBuilder.Create;
  try
    SB.AddChar('A');
    Assert.AreEqual('A', SB.AsText);
  finally
    FreeAndNil(SB);
  end;
end;

procedure TTestLightCoreStrBuilder.TestAddChar_Multiple;
var
  SB: TCStringBuilder;
begin
  SB:= TCStringBuilder.Create;
  try
    SB.AddChar('H');
    SB.AddChar('e');
    SB.AddChar('l');
    SB.AddChar('l');
    SB.AddChar('o');
    Assert.AreEqual('Hello', SB.AsText);
  finally
    FreeAndNil(SB);
  end;
end;

procedure TTestLightCoreStrBuilder.TestAddEnter;
var
  SB: TCStringBuilder;
  Result: string;
begin
  SB:= TCStringBuilder.Create;
  try
    SB.AddChar('A');
    SB.AddEnter;
    SB.AddChar('B');
    Result:= SB.AsText;
    Assert.AreEqual('A'#13#10'B', Result);
  finally
    FreeAndNil(SB);
  end;
end;

procedure TTestLightCoreStrBuilder.TestAsText;
var
  SB: TCStringBuilder;
begin
  SB:= TCStringBuilder.Create;
  try
    SB.AddChar('X');
    SB.AddChar('Y');
    SB.AddChar('Z');
    { AsText should return exactly the characters added }
    Assert.AreEqual(3, Length(SB.AsText));
    Assert.AreEqual('XYZ', SB.AsText);
  finally
    FreeAndNil(SB);
  end;
end;

procedure TTestLightCoreStrBuilder.TestClear;
var
  SB: TCStringBuilder;
begin
  SB:= TCStringBuilder.Create;
  try
    SB.AddChar('A');
    SB.AddChar('B');
    SB.Clear;
    Assert.AreEqual('', SB.AsText);
  finally
    FreeAndNil(SB);
  end;
end;

{ Buffer Tests }

procedure TTestLightCoreStrBuilder.TestBufferGrowth;
var
  SB: TCStringBuilder;
  i: Integer;
  Result: string;
begin
  { Use small buffer to force multiple growths }
  SB:= TCStringBuilder.Create(10);
  try
    { Add more than buffer size to trigger growth }
    for i:= 1 to 100 do
      SB.AddChar('X');

    Result:= SB.AsText;
    Assert.AreEqual(100, Length(Result));
    Assert.AreEqual(StringOfChar('X', 100), Result);
  finally
    FreeAndNil(SB);
  end;
end;

procedure TTestLightCoreStrBuilder.TestSmallBuffer;
var
  SB: TCStringBuilder;
begin
  SB:= TCStringBuilder.Create(5);
  try
    Assert.AreEqual(5, SB.BufferGrowth);
    { Should still work with very small buffer }
    SB.AddChar('1');
    SB.AddChar('2');
    SB.AddChar('3');
    SB.AddChar('4');
    SB.AddChar('5');
    SB.AddChar('6');  { Triggers buffer growth }
    Assert.AreEqual('123456', SB.AsText);
  finally
    FreeAndNil(SB);
  end;
end;

{ Combined Operations Tests }

procedure TTestLightCoreStrBuilder.TestAddCharsAndEnters;
var
  SB: TCStringBuilder;
  Result: string;
begin
  SB:= TCStringBuilder.Create;
  try
    SB.AddChar('L');
    SB.AddChar('1');
    SB.AddEnter;
    SB.AddChar('L');
    SB.AddChar('2');
    SB.AddEnter;
    SB.AddChar('L');
    SB.AddChar('3');
    Result:= SB.AsText;
    Assert.AreEqual('L1'#13#10'L2'#13#10'L3', Result);
  finally
    FreeAndNil(SB);
  end;
end;

procedure TTestLightCoreStrBuilder.TestBuildSentence;
var
  SB: TCStringBuilder;
  Sentence: string;
  i: Integer;
begin
  SB:= TCStringBuilder.Create;
  try
    Sentence:= 'The quick brown fox jumps over the lazy dog.';
    for i:= 1 to Length(Sentence) do
      SB.AddChar(Sentence[i]);

    Assert.AreEqual(Sentence, SB.AsText);
  finally
    FreeAndNil(SB);
  end;
end;

procedure TTestLightCoreStrBuilder.TestReuseAfterClear;
var
  SB: TCStringBuilder;
begin
  SB:= TCStringBuilder.Create;
  try
    SB.AddChar('A');
    SB.AddChar('B');
    Assert.AreEqual('AB', SB.AsText);

    SB.Clear;

    SB.AddChar('C');
    SB.AddChar('D');
    Assert.AreEqual('CD', SB.AsText);
  finally
    FreeAndNil(SB);
  end;
end;

{ Edge Cases Tests }

procedure TTestLightCoreStrBuilder.TestEmptyResult;
var
  SB: TCStringBuilder;
begin
  SB:= TCStringBuilder.Create;
  try
    Assert.AreEqual('', SB.AsText);
    Assert.AreEqual(0, Length(SB.AsText));
  finally
    FreeAndNil(SB);
  end;
end;

procedure TTestLightCoreStrBuilder.TestSpecialCharacters;
var
  SB: TCStringBuilder;
begin
  SB:= TCStringBuilder.Create;
  try
    SB.AddChar(#0);    { Null }
    SB.AddChar(#9);    { Tab }
    SB.AddChar(#32);   { Space }
    SB.AddChar(#127);  { DEL }

    Assert.AreEqual(4, Length(SB.AsText));
  finally
    FreeAndNil(SB);
  end;
end;

procedure TTestLightCoreStrBuilder.TestUnicodeCharacters;
var
  SB: TCStringBuilder;
  Expected: string;
begin
  SB:= TCStringBuilder.Create;
  try
    { Use character codes to avoid file encoding issues }
    SB.AddChar(Char($00E9));  { é - Latin Small Letter E with Acute }
    SB.AddChar(Char($00F1));  { ñ - Latin Small Letter N with Tilde }
    SB.AddChar(Char($00FC));  { ü - Latin Small Letter U with Diaeresis }
    Expected:= Char($00E9) + Char($00F1) + Char($00FC);
    Assert.AreEqual(Expected, SB.AsText);
    Assert.AreEqual(3, Length(SB.AsText));
  finally
    FreeAndNil(SB);
  end;
end;


{ Additional Tests }

procedure TTestLightCoreStrBuilder.TestCreate_CustomBufferSize;
var
  SB: TCStringBuilder;
begin
  SB:= TCStringBuilder.Create(500);
  try
    Assert.AreEqual(500, SB.BufferGrowth);
    Assert.AreEqual('', SB.AsText);
  finally
    FreeAndNil(SB);
  end;
end;


procedure TTestLightCoreStrBuilder.TestAddEnter_Multiple;
var
  SB: TCStringBuilder;
  ResultText: string;
begin
  SB:= TCStringBuilder.Create;
  try
    SB.AddEnter;
    SB.AddEnter;
    SB.AddEnter;
    ResultText:= SB.AsText;
    Assert.AreEqual(6, Length(ResultText), 'Three enters = 6 chars (CR+LF each)');
    Assert.AreEqual(#13#10#13#10#13#10, ResultText);
  finally
    FreeAndNil(SB);
  end;
end;


procedure TTestLightCoreStrBuilder.TestAsText_CalledMultipleTimes;
var
  SB: TCStringBuilder;
  First, Second, Third: string;
begin
  SB:= TCStringBuilder.Create;
  try
    SB.AddChar('A');
    SB.AddChar('B');
    SB.AddChar('C');
    { Calling AsText multiple times should return the same result }
    First:= SB.AsText;
    Second:= SB.AsText;
    Third:= SB.AsText;
    Assert.AreEqual('ABC', First);
    Assert.AreEqual(First, Second);
    Assert.AreEqual(Second, Third);
  finally
    FreeAndNil(SB);
  end;
end;


procedure TTestLightCoreStrBuilder.TestAsText_AddContentAfterAsText;
var
  SB: TCStringBuilder;
  First, Second: string;
begin
  { Critical test: Verifies AsText doesn't modify internal state }
  SB:= TCStringBuilder.Create;
  try
    SB.AddChar('A');
    SB.AddChar('B');
    First:= SB.AsText;
    Assert.AreEqual('AB', First);

    { Add more content after AsText was called }
    SB.AddChar('C');
    SB.AddChar('D');
    Second:= SB.AsText;
    Assert.AreEqual('ABCD', Second, 'Adding content after AsText should work correctly');
  finally
    FreeAndNil(SB);
  end;
end;


procedure TTestLightCoreStrBuilder.TestBufferGrowth_MultipleGrowths;
var
  SB: TCStringBuilder;
  i: Integer;
  ResultText: string;
begin
  { Use tiny buffer to force many growths }
  SB:= TCStringBuilder.Create(3);
  try
    { Add 50 characters to trigger multiple buffer expansions }
    for i:= 1 to 50 do
      SB.AddChar(Char(Ord('A') + (i mod 26)));

    ResultText:= SB.AsText;
    Assert.AreEqual(50, Length(ResultText));
  finally
    FreeAndNil(SB);
  end;
end;


procedure TTestLightCoreStrBuilder.TestExactBufferBoundary;
var
  SB: TCStringBuilder;
  i: Integer;
begin
  SB:= TCStringBuilder.Create(10);
  try
    { Add exactly buffer size characters }
    for i:= 1 to 10 do
      SB.AddChar('X');

    Assert.AreEqual(10, Length(SB.AsText));
    Assert.AreEqual(StringOfChar('X', 10), SB.AsText);
  finally
    FreeAndNil(SB);
  end;
end;


procedure TTestLightCoreStrBuilder.TestBufferBoundaryPlusOne;
var
  SB: TCStringBuilder;
  i: Integer;
begin
  SB:= TCStringBuilder.Create(10);
  try
    { Add buffer size + 1 characters to trigger exactly one growth }
    for i:= 1 to 11 do
      SB.AddChar('Y');

    Assert.AreEqual(11, Length(SB.AsText));
    Assert.AreEqual(StringOfChar('Y', 11), SB.AsText);
  finally
    FreeAndNil(SB);
  end;
end;


procedure TTestLightCoreStrBuilder.TestClearAndRebuildLarger;
var
  SB: TCStringBuilder;
  i: Integer;
begin
  SB:= TCStringBuilder.Create(10);
  try
    { First build }
    for i:= 1 to 5 do
      SB.AddChar('A');
    Assert.AreEqual('AAAAA', SB.AsText);

    SB.Clear;

    { Second build is larger than first }
    for i:= 1 to 20 do
      SB.AddChar('B');
    Assert.AreEqual(StringOfChar('B', 20), SB.AsText);
  finally
    FreeAndNil(SB);
  end;
end;


procedure TTestLightCoreStrBuilder.TestHighUnicodeCharacters;
var
  SB: TCStringBuilder;
  Expected: string;
begin
  SB:= TCStringBuilder.Create;
  try
    { Test characters from various Unicode ranges }
    SB.AddChar(Char($03B1));  { Greek Alpha }
    SB.AddChar(Char($0414));  { Cyrillic De }
    SB.AddChar(Char($4E2D));  { CJK character for "middle" }
    Expected:= Char($03B1) + Char($0414) + Char($4E2D);
    Assert.AreEqual(Expected, SB.AsText);
    Assert.AreEqual(3, Length(SB.AsText));
  finally
    FreeAndNil(SB);
  end;
end;


procedure TTestLightCoreStrBuilder.TestLargeString;
var
  SB: TCStringBuilder;
  i: Integer;
  ResultText: string;
const
  LargeSize = 100000;
begin
  SB:= TCStringBuilder.Create;
  try
    for i:= 1 to LargeSize do
      SB.AddChar(Char(Ord('A') + (i mod 26)));

    ResultText:= SB.AsText;
    Assert.AreEqual(LargeSize, Length(ResultText));
    { Verify first and last characters }
    Assert.AreEqual('B', ResultText[1]);  { (1 mod 26) + 'A' = 'B' }
  finally
    FreeAndNil(SB);
  end;
end;


procedure TTestLightCoreStrBuilder.TestEnterAtBufferBoundary;
var
  SB: TCStringBuilder;
  i: Integer;
  ResultText: string;
begin
  { Buffer of 10, fill with 9 chars, then add Enter (2 chars) to cross boundary }
  SB:= TCStringBuilder.Create(10);
  try
    for i:= 1 to 9 do
      SB.AddChar('X');
    SB.AddEnter;  { This crosses the buffer boundary }
    SB.AddChar('Y');

    ResultText:= SB.AsText;
    Assert.AreEqual(12, Length(ResultText));  { 9 + 2 + 1 }
    Assert.AreEqual('XXXXXXXXX'#13#10'Y', ResultText);
  finally
    FreeAndNil(SB);
  end;
end;


initialization
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  {$ENDIF}
  TDUnitX.RegisterTestFixture(TTestLightCoreStrBuilder);

end.
