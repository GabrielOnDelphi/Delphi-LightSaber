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
    [TestCase('123456789', '123456789,CBF43926')]  { Standard CRC32 check value: "check=0xcbf43926" of CRC-32/ISO-HDLC in https://reveng.sourceforge.io/crc-catalogue/17plus.htm }
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

    { The 256-entry lookup table, checked against an outside source }
    [Test]
    procedure TestCRC32Table_ExpectedTableMatchesPolynomial;
    [Test]
    procedure TestCRC32Table_EverySingleByte_UsesTheStandardEntry;
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


{---------------------------------------------------------------------------------------------------------------
   The CRC32Table of LightCore.EncodeCRC (private, in its IMPLEMENTATION section), entry by entry

   Outside source: the standard reflected CRC-32 (zip, gzip, PNG), polynomial $EDB88320.
     RFC 1952 section 8, make_crc_table(): "c = 0xedb88320L ^ (c >> 1);"  (https://www.rfc-editor.org/rfc/rfc1952)
   EXPECTED_CRC32_TABLE was rebuilt from that polynomial by a script that never read LightSaber code.
   The script, its output and the sources: UnitTesting\Expected - CRC32Table.txt.

   How every entry is reached through the public API: for ONE byte B, CRC32 computes
     NOT( ($FFFFFFFF shr 8) XOR Table[B XOR $FF] )
   so the 256 single-byte inputs read the 256 entries, each exactly once.
---------------------------------------------------------------------------------------------------------------}
CONST
  EXPECTED_CRC32_TABLE: array[0..255] of Cardinal = (
    { 0 }   $00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F, $E963A535, $9E6495A3,
    { 8 }   $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
    { 16 }  $1DB71064, $6AB020F2, $F3B97148, $84BE41DE, $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
    { 24 }  $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC, $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
    { 32 }  $3B6E20C8, $4C69105E, $D56041E4, $A2677172, $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
    { 40 }  $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
    { 48 }  $26D930AC, $51DE003A, $C8D75180, $BFD06116, $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
    { 56 }  $2802B89E, $5F058808, $C60CD9B2, $B10BE924, $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,
    { 64 }  $76DC4190, $01DB7106, $98D220BC, $EFD5102A, $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
    { 72 }  $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
    { 80 }  $6B6B51F4, $1C6C6162, $856530D8, $F262004E, $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
    { 88 }  $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
    { 96 }  $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2, $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
    { 104 } $4369E96A, $346ED9FC, $AD678846, $DA60B8D0, $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
    { 112 } $5005713C, $270241AA, $BE0B1010, $C90C2086, $5768B525, $206F85B3, $B966D409, $CE61E49F,
    { 120 } $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,
    { 128 } $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A, $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
    { 136 } $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8, $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
    { 144 } $F00F9344, $8708A3D2, $1E01F268, $6906C2FE, $F762575D, $806567CB, $196C3671, $6E6B06E7,
    { 152 } $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC, $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
    { 160 } $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
    { 168 } $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
    { 176 } $CB61B38C, $BC66831A, $256FD2A0, $5268E236, $CC0C7795, $BB0B4703, $220216B9, $5505262F,
    { 184 } $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,
    { 192 } $9B64C2B0, $EC63F226, $756AA39C, $026D930A, $9C0906A9, $EB0E363F, $72076785, $05005713,
    { 200 } $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38, $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
    { 208 } $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
    { 216 } $88085AE6, $FF0F6A70, $66063BCA, $11010B5C, $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
    { 224 } $A00AE278, $D70DD2EE, $4E048354, $3903B3C2, $A7672661, $D06016F7, $4969474D, $3E6E77DB,
    { 232 } $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
    { 240 } $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605, $CDD70693, $54DE5729, $23D967BF,
    { 248 } $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94, $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);


{ Guards the pinned copy above against a typing error: each entry is rebuilt bit by bit from the polynomial (RFC 1952 section 8) }
procedure TTestEncodeCRC.TestCRC32Table_ExpectedTableMatchesPolynomial;
var
  n, k: Integer;
  c: Cardinal;
  Mismatches: string;
begin
  Mismatches:= '';
  for n:= 0 to 255 do
    begin
      c:= Cardinal(n);
      for k:= 1 to 8 do
        if (c and 1) <> 0
        then c:= (c shr 1) xor $EDB88320
        else c:= c shr 1;
      if c <> EXPECTED_CRC32_TABLE[n]
      then Mismatches:= Mismatches + Format(' [%d] pinned $%.8X, polynomial gives $%.8X;', [n, EXPECTED_CRC32_TABLE[n], c]);
    end;
  Assert.AreEqual('', Mismatches, 'The pinned expected table differs from its polynomial:' + Mismatches);
end;


{ Every one of the 256 entries of the product table, read through CRC32(TBytesArray) }
procedure TTestEncodeCRC.TestCRC32Table_EverySingleByte_UsesTheStandardEntry;
var
  b: Integer;
  Bytes: TBytesArray;
  Expected, Actual: Cardinal;
  Mismatches: string;
begin
  Mismatches:= '';
  SetLength(Bytes, 1);
  for b:= 0 to 255 do
    begin
      Bytes[0]:= Byte(b);
      Expected:= NOT (($FFFFFFFF shr 8) xor EXPECTED_CRC32_TABLE[b xor $FF]);
      Actual:= CRC32(Bytes);
      if Actual <> Expected
      then Mismatches:= Mismatches + Format(' byte %d (table entry %d): expected CRC $%.8X, got $%.8X;', [b, b xor $FF, Expected, Actual]);
    end;
  Assert.AreEqual('', Mismatches, 'CRC32Table holds a wrong entry:' + Mismatches);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestEncodeCRC);

end.
