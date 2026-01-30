unit Test.LightCore.EncodeCRC;

{=============================================================================================================
   Unit tests for LightCore.EncodeCRC
   Tests CRC32 implementations for AnsiString, TBytesArray, TStream, and Unicode strings.
   Last updated: 2026.01.30
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
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

    { CRC32 AnsiString tests }
    [Test]
    [TestCase('EmptyString', ',0')]
    [TestCase('SingleChar_A', 'A,D3D99E8B')]
    [TestCase('Hello', 'Hello,F7D18982')]
    [TestCase('123456789', '123456789,CBF43926')]  { Standard CRC32 test vector }
    procedure TestCRC32_AnsiString(const Input: AnsiString; const ExpectedHex: string);

    [Test]
    procedure TestCRC32_Numbers_Consistency;

    { CRC32 TBytesArray tests }
    [Test]
    procedure TestCRC32_EmptyBytes;

    [Test]
    procedure TestCRC32_SingleByte;

    [Test]
    procedure TestCRC32_MultipleBytes;

    [Test]
    procedure TestCRC32_BytesMatchesAnsiString;

    { CRC32Stream tests }
    [Test]
    procedure TestCRC32Stream_Empty;

    [Test]
    procedure TestCRC32Stream_MatchesAnsiString;

    [Test]
    procedure TestCRC32Stream_LargeData;

    { CRC32_U Unicode tests }
    [Test]
    procedure TestCRC32_Unicode_ASCIIMatchesAnsi;

    [Test]
    procedure TestCRC32_Unicode_NonASCIIDiffers;

    { General consistency tests }
    [Test]
    procedure TestCRC32_Consistency;

    [Test]
    procedure TestCRC32_DifferentInputsDifferentResults;
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


{---------------------------------------------------------------------------------------------------------------
   CRC32 AnsiString tests
---------------------------------------------------------------------------------------------------------------}

procedure TTestEncodeCRC.TestCRC32_AnsiString(const Input: AnsiString; const ExpectedHex: string);
var
  CRCResult: Cardinal;
  ExpectedValue: Cardinal;
begin
  CRCResult:= CRC32(Input);

  if ExpectedHex = '0'
  then ExpectedValue:= 0
  else ExpectedValue:= StrToInt64('$' + ExpectedHex);

  Assert.AreEqual(ExpectedValue, CRCResult, Format('CRC32 of "%s" should be $%s but got $%X', [string(Input), ExpectedHex, CRCResult]));
end;


procedure TTestEncodeCRC.TestCRC32_Numbers_Consistency;
var
  Result1, Result2: Cardinal;
  TestStr: AnsiString;
begin
  TestStr:= '12345';
  Result1:= CRC32(TestStr);
  Result2:= CRC32(TestStr);

  Assert.AreEqual(Result1, Result2, 'CRC32 should return consistent results for "12345"');
  Assert.AreNotEqual(Cardinal(0), Result1, 'CRC32 of "12345" should not be zero');
end;


{---------------------------------------------------------------------------------------------------------------
   CRC32 TBytesArray tests
---------------------------------------------------------------------------------------------------------------}

procedure TTestEncodeCRC.TestCRC32_EmptyBytes;
var
  Bytes: TBytesArray;
  CRCResult: Cardinal;
begin
  SetLength(Bytes, 0);
  CRCResult:= CRC32(Bytes);
  Assert.AreEqual(Cardinal(0), CRCResult, 'CRC32 of empty byte array should be 0 (NOT $FFFFFFFF = 0)');
end;


procedure TTestEncodeCRC.TestCRC32_SingleByte;
var
  Bytes: TBytesArray;
  ResultBytes, ResultAnsi: Cardinal;
begin
  SetLength(Bytes, 1);
  Bytes[0]:= Ord('A');

  ResultBytes:= CRC32(Bytes);
  ResultAnsi:= CRC32(AnsiString('A'));

  Assert.AreEqual(ResultAnsi, ResultBytes, 'CRC32 of single byte should match AnsiString version');
end;


procedure TTestEncodeCRC.TestCRC32_MultipleBytes;
var
  Bytes: TBytesArray;
  i: Integer;
  ResultBytes, ResultAnsi: Cardinal;
  TestStr: AnsiString;
begin
  TestStr:= 'Test123';
  SetLength(Bytes, Length(TestStr));
  for i:= 1 to Length(TestStr) do
    Bytes[i-1]:= Ord(TestStr[i]);

  ResultBytes:= CRC32(Bytes);
  ResultAnsi:= CRC32(TestStr);

  Assert.AreEqual(ResultAnsi, ResultBytes, 'CRC32 of byte array should match AnsiString version');
end;


procedure TTestEncodeCRC.TestCRC32_BytesMatchesAnsiString;
var
  Bytes: TBytesArray;
  i: Integer;
  TestStr: AnsiString;
begin
  { Test with the standard CRC32 test vector '123456789' }
  TestStr:= '123456789';
  SetLength(Bytes, Length(TestStr));
  for i:= 1 to Length(TestStr) do
    Bytes[i-1]:= Ord(TestStr[i]);

  Assert.AreEqual(CRC32(TestStr), CRC32(Bytes), 'CRC32 of bytes should match AnsiString for "123456789"');
  Assert.AreEqual(Cardinal($CBF43926), CRC32(Bytes), 'CRC32 of "123456789" should be $CBF43926 (standard test vector)');
end;


{---------------------------------------------------------------------------------------------------------------
   CRC32Stream tests
---------------------------------------------------------------------------------------------------------------}

procedure TTestEncodeCRC.TestCRC32Stream_Empty;
var
  Stream: TMemoryStream;
  CRCResult: Cardinal;
begin
  Stream:= TMemoryStream.Create;
  try
    CRCResult:= CRC32Stream(Stream);
    Assert.AreEqual(Cardinal(0), CRCResult, 'CRC32Stream of empty stream should be 0');
  finally
    FreeAndNil(Stream);
  end;
end;


procedure TTestEncodeCRC.TestCRC32Stream_MatchesAnsiString;
var
  Stream: TMemoryStream;
  TestStr: AnsiString;
  ResultStream, ResultAnsi: Cardinal;
begin
  TestStr:= '123456789';

  Stream:= TMemoryStream.Create;
  try
    Stream.Write(TestStr[1], Length(TestStr));
    Stream.Position:= 0;

    ResultStream:= CRC32Stream(Stream);
    ResultAnsi:= CRC32(TestStr);

    Assert.AreEqual(ResultAnsi, ResultStream, 'CRC32Stream should match CRC32(AnsiString) for same data');
    Assert.AreEqual(Cardinal($CBF43926), ResultStream, 'CRC32Stream of "123456789" should be $CBF43926');
  finally
    FreeAndNil(Stream);
  end;
end;


procedure TTestEncodeCRC.TestCRC32Stream_LargeData;
var
  Stream: TMemoryStream;
  i: Integer;
  ResultStream, ResultBytes: Cardinal;
  LargeData: TBytesArray;
const
  DATA_SIZE = 100000;  { 100KB - larger than the 64KB buffer }
begin
  { Create test data larger than the internal buffer to test chunked processing }
  SetLength(LargeData, DATA_SIZE);
  for i:= 0 to DATA_SIZE - 1 do
    LargeData[i]:= Byte(i mod 256);

  Stream:= TMemoryStream.Create;
  try
    Stream.Write(LargeData[0], Length(LargeData));
    Stream.Position:= 0;

    ResultStream:= CRC32Stream(Stream);
    ResultBytes:= CRC32(LargeData);

    Assert.AreEqual(ResultBytes, ResultStream, 'CRC32Stream should match CRC32(Bytes) for large data');
    Assert.AreNotEqual(Cardinal(0), ResultStream, 'CRC32 of non-empty data should not be zero');
  finally
    FreeAndNil(Stream);
  end;
end;


{---------------------------------------------------------------------------------------------------------------
   CRC32_U Unicode tests
---------------------------------------------------------------------------------------------------------------}

procedure TTestEncodeCRC.TestCRC32_Unicode_ASCIIMatchesAnsi;
var
  ResultU: Cardinal;
  ResultA: Cardinal;
  UnicodeStr: string;
  AnsiStr: AnsiString;
begin
  { For ASCII-only strings, CRC32_U and CRC32 should produce the same result
    because UTF-8 encoding of ASCII is identical to the byte values }
  UnicodeStr:= 'Hello123';
  AnsiStr:= AnsiString(UnicodeStr);

  ResultU:= CRC32_U(UnicodeStr);
  ResultA:= CRC32(AnsiStr);

  Assert.AreEqual(ResultA, ResultU, Format('CRC32_U and CRC32 should match for ASCII strings. Unicode=$%X, Ansi=$%X', [ResultU, ResultA]));
end;


procedure TTestEncodeCRC.TestCRC32_Unicode_NonASCIIDiffers;
var
  ResultU: Cardinal;
  UnicodeStr: string;
begin
  { Test that CRC32_U handles Unicode characters (UTF-8 multi-byte encoding) }
  UnicodeStr:= 'Привет';  { Russian "Hello" - uses Cyrillic characters }

  ResultU:= CRC32_U(UnicodeStr);

  { Just verify it produces a non-zero result and doesn't crash }
  Assert.AreNotEqual(Cardinal(0), ResultU, 'CRC32_U of Cyrillic text should produce a non-zero checksum');
end;


{---------------------------------------------------------------------------------------------------------------
   General consistency tests
---------------------------------------------------------------------------------------------------------------}

procedure TTestEncodeCRC.TestCRC32_Consistency;
var
  Result1, Result2: Cardinal;
  TestStr: AnsiString;
begin
  TestStr:= 'ConsistencyTest';
  Result1:= CRC32(TestStr);
  Result2:= CRC32(TestStr);

  Assert.AreEqual(Result1, Result2, 'CRC32 should return consistent results for the same input');
end;


procedure TTestEncodeCRC.TestCRC32_DifferentInputsDifferentResults;
var
  CRC1, CRC2, CRC3: Cardinal;
begin
  CRC1:= CRC32(AnsiString('ABC'));
  CRC2:= CRC32(AnsiString('ABD'));
  CRC3:= CRC32(AnsiString('abc'));

  Assert.AreNotEqual(CRC1, CRC2, 'Different inputs should produce different CRC32 values');
  Assert.AreNotEqual(CRC1, CRC3, 'CRC32 should be case-sensitive');
  Assert.AreNotEqual(CRC2, CRC3, 'All three different inputs should have different CRCs');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestEncodeCRC);

end.
