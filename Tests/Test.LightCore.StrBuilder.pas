unit Test.LightCore.StrBuilder;

{=============================================================================================================
   Unit tests for LightCore.StrBuilder
   Tests TCStringBuilder - fast string builder implementation
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
    procedure TestAddChar_Single;

    [Test]
    procedure TestAddChar_Multiple;

    [Test]
    procedure TestAddEnter;

    [Test]
    procedure TestAsText;

    [Test]
    procedure TestClear;

    { Buffer Tests }
    [Test]
    procedure TestBufferGrowth;

    [Test]
    procedure TestSmallBuffer;

    { Combined Operations Tests }
    [Test]
    procedure TestAddCharsAndEnters;

    [Test]
    procedure TestBuildSentence;

    [Test]
    procedure TestReuseAfterClear;

    { Edge Cases Tests }
    [Test]
    procedure TestEmptyResult;

    [Test]
    procedure TestSpecialCharacters;

    [Test]
    procedure TestUnicodeCharacters;
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
    Assert.AreEqual(10000, SB.BuffSize);
    Assert.AreEqual('', SB.AsText);
  finally
    SB.Free;
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
    SB.Free;
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
    SB.Free;
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
    SB.Free;
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
    SB.Free;
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
    SB.Free;
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
    SB.Free;
  end;
end;

procedure TTestLightCoreStrBuilder.TestSmallBuffer;
var
  SB: TCStringBuilder;
begin
  SB:= TCStringBuilder.Create(5);
  try
    Assert.AreEqual(5, SB.BuffSize);
    { Should still work with very small buffer }
    SB.AddChar('1');
    SB.AddChar('2');
    SB.AddChar('3');
    SB.AddChar('4');
    SB.AddChar('5');
    SB.AddChar('6');  { Triggers buffer growth }
    Assert.AreEqual('123456', SB.AsText);
  finally
    SB.Free;
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
    SB.Free;
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
    SB.Free;
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
    SB.Free;
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
    SB.Free;
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
    SB.Free;
  end;
end;

procedure TTestLightCoreStrBuilder.TestUnicodeCharacters;
var
  SB: TCStringBuilder;
begin
  SB:= TCStringBuilder.Create;
  try
    SB.AddChar('é');
    SB.AddChar('ñ');
    SB.AddChar('ü');
    Assert.AreEqual('éñü', SB.AsText);
  finally
    SB.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestLightCoreStrBuilder);

end.
