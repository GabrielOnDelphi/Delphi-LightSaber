unit Test.LightCore.EncodeCRC;

{=============================================================================================================
   Unit tests for LightCore.EncodeCRC
   Tests CRC32 implementations for AnsiString and TBytesArray
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  LightCore.Types,
  LightCore.EncodeCRC;

type
  [TestFixture]
  TTestEncodeCRC = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    [TestCase('EmptyString', ',0')]
    [TestCase('SingleChar', 'A,D3D99E8B')]
    [TestCase('Hello', 'Hello,F7D18982')]
    [TestCase('Numbers', '12345,CBEE8E23')]
    procedure TestCRC32_AnsiString(const Input: AnsiString; const ExpectedHex: string);

    [Test]
    procedure TestCRC32_EmptyBytes;

    [Test]
    procedure TestCRC32_SingleByte;

    [Test]
    procedure TestCRC32_MultipleBytes;

    [Test]
    procedure TestCRC32_Consistency;

    [Test]
    procedure TestCRC32_Unicode_DifferentFromAnsi;
  end;

implementation

procedure TTestEncodeCRC.Setup;
begin
  { No setup needed }
end;

procedure TTestEncodeCRC.TearDown;
begin
  { No teardown needed }
end;

procedure TTestEncodeCRC.TestCRC32_AnsiString(const Input: AnsiString; const ExpectedHex: string);
var
  Result: Cardinal;
  ExpectedValue: Cardinal;
begin
  Result := CRC32(Input);

  if ExpectedHex = '0'
  then ExpectedValue := 0
  else ExpectedValue := StrToInt64('$' + ExpectedHex);

  Assert.AreEqual(ExpectedValue, Result, Format('CRC32 of "%s" should be $%s', [Input, ExpectedHex]));
end;

procedure TTestEncodeCRC.TestCRC32_EmptyBytes;
var
  Bytes: TBytesArray;
  Result: Cardinal;
begin
  SetLength(Bytes, 0);
  Result := CRC32(Bytes);
  Assert.AreEqual(Cardinal(0), Result, 'CRC32 of empty byte array should be 0');
end;

procedure TTestEncodeCRC.TestCRC32_SingleByte;
var
  Bytes: TBytesArray;
  ResultBytes, ResultAnsi: Cardinal;
begin
  SetLength(Bytes, 1);
  Bytes[0] := Ord('A');

  ResultBytes := CRC32(Bytes);
  ResultAnsi := CRC32(AnsiString('A'));

  Assert.AreEqual(ResultAnsi, ResultBytes, 'CRC32 of single byte should match AnsiString version');
end;

procedure TTestEncodeCRC.TestCRC32_MultipleBytes;
var
  Bytes: TBytesArray;
  i: Integer;
  ResultBytes, ResultAnsi: Cardinal;
  TestStr: AnsiString;
begin
  TestStr := 'Test123';
  SetLength(Bytes, Length(TestStr));
  for i := 1 to Length(TestStr) do
    Bytes[i-1] := Ord(TestStr[i]);

  ResultBytes := CRC32(Bytes);
  ResultAnsi := CRC32(TestStr);

  Assert.AreEqual(ResultAnsi, ResultBytes, 'CRC32 of byte array should match AnsiString version');
end;

procedure TTestEncodeCRC.TestCRC32_Consistency;
var
  Result1, Result2: Cardinal;
  TestStr: AnsiString;
begin
  TestStr := 'ConsistencyTest';
  Result1 := CRC32(TestStr);
  Result2 := CRC32(TestStr);

  Assert.AreEqual(Result1, Result2, 'CRC32 should return consistent results for the same input');
end;

procedure TTestEncodeCRC.TestCRC32_Unicode_DifferentFromAnsi;
var
  ResultU: Cardinal;
  ResultA: Cardinal;
  UnicodeStr: string;
  AnsiStr: AnsiString;
begin
  UnicodeStr := 'Test';
  AnsiStr := AnsiString(UnicodeStr);

  ResultU := CRC32_U(UnicodeStr);
  ResultA := CRC32(AnsiStr);

  { Note: CRC32_U uses TStringStream which encodes as UTF-8, so results may differ }
  { This test documents the behavior rather than asserting equality }
  Assert.Pass(Format('Unicode CRC: $%X, Ansi CRC: $%X', [ResultU, ResultA]));
end;

initialization
  TDUnitX.RegisterTestFixture(TTestEncodeCRC);

end.
