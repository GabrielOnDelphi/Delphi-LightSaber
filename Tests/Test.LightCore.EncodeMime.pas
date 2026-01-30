UNIT Test.LightCore.EncodeMime;

{=============================================================================================================
   2026.01.30
   Unit tests for LightCore.EncodeMime.pas
   Tests MIME (Base64) encoding and decoding functions for both Unicode and AnsiString variants.
=============================================================================================================}

INTERFACE

USES
  DUnitX.TestFramework,
  System.SysUtils,
  LightCore.EncodeMime;

TYPE
  [TestFixture]
  TTestEncodeMime = class
  public
    { Unicode String Tests - MimeString/DeMimeString }
    [Test]
    procedure TestMimeString_Simple;

    [Test]
    procedure TestMimeString_Empty;

    [Test]
    procedure TestMimeString_Unicode;

    [Test]
    procedure TestMimeString_RoundTrip;

    [Test]
    procedure TestMimeString_SpecialChars;

    [Test]
    procedure TestMimeString_LongString;

    [Test]
    procedure TestDeMimeString_Simple;

    [Test]
    procedure TestDeMimeString_Empty;

    { AnsiString Tests - MimeStringA/DeMimeStringA }
    [Test]
    procedure TestMimeStringA_Simple;

    [Test]
    procedure TestMimeStringA_Empty;

    [Test]
    procedure TestMimeStringA_RoundTrip;

    [Test]
    procedure TestMimeStringA_LongString;

    [Test]
    procedure TestDeMimeStringA_Simple;

    [Test]
    procedure TestDeMimeStringA_Empty;

    { Known Value Tests - verify against standard Base64 values }
    [Test]
    procedure TestMimeString_KnownValue_Hello;

    [Test]
    procedure TestMimeString_KnownValue_HelloWorld;

    [Test]
    procedure TestMimeStringA_KnownValue_Hello;

    [Test]
    procedure TestMimeStringA_KnownValue_ABC;

    { Edge Cases }
    [Test]
    procedure TestMimeString_SingleChar;

    [Test]
    procedure TestMimeStringA_SingleChar;

    [Test]
    procedure TestMimeString_BinaryLikeData;

    [Test]
    procedure TestMimeStringA_HighBytes;

    [Test]
    procedure TestDeMimeString_InvalidBase64;

    [Test]
    procedure TestDeMimeStringA_InvalidBase64;

    [Test]
    procedure TestMimeString_PaddingVariants;

    [Test]
    procedure TestMimeString_Whitespace;
  end;


IMPLEMENTATION


{--------------------------------------------------------------------------------------------------
   UNICODE STRING TESTS
--------------------------------------------------------------------------------------------------}

procedure TTestEncodeMime.TestMimeString_Simple;
var
  Encoded: string;
begin
  Encoded:= MimeString('Hello');
  Assert.IsNotEmpty(Encoded);
  Assert.AreNotEqual('Hello', Encoded, 'Output should be encoded, not plain text');
end;


procedure TTestEncodeMime.TestMimeString_Empty;
var
  Encoded: string;
begin
  Encoded:= MimeString('');
  Assert.AreEqual('', Encoded, 'Empty input should produce empty output');
end;


procedure TTestEncodeMime.TestMimeString_Unicode;
var
  Original, Encoded, Decoded: string;
begin
  Original:= 'Hello éàü 世界 🎉';
  Encoded:= MimeString(Original);
  Decoded:= DeMimeString(Encoded);
  Assert.AreEqual(Original, Decoded, 'Unicode round-trip should preserve all characters');
end;


procedure TTestEncodeMime.TestMimeString_RoundTrip;
var
  Original, Decoded: string;
begin
  Original:= 'Test String 12345 !@#$%';
  Decoded:= DeMimeString(MimeString(Original));
  Assert.AreEqual(Original, Decoded, 'Round-trip should preserve the original string');
end;


procedure TTestEncodeMime.TestMimeString_SpecialChars;
var
  Original, Decoded: string;
begin
  Original:= 'Line1'#13#10'Line2'#9'Tab'#0'Null';
  Decoded:= DeMimeString(MimeString(Original));
  Assert.AreEqual(Original, Decoded, 'Special characters including CR/LF/Tab/Null should be preserved');
end;


procedure TTestEncodeMime.TestMimeString_LongString;
var
  Original, Decoded: string;
  i: Integer;
begin
  { Build a long string (10KB+) }
  Original:= '';
  for i:= 1 to 1000 do
    Original:= Original + 'ABCDEFGHIJ';

  Decoded:= DeMimeString(MimeString(Original));
  Assert.AreEqual(Length(Original), Length(Decoded), 'Long string length should be preserved');
  Assert.AreEqual(Original, Decoded, 'Long string content should be preserved');
end;


procedure TTestEncodeMime.TestDeMimeString_Simple;
var
  Encoded, Decoded: string;
begin
  Encoded:= MimeString('Test');
  Decoded:= DeMimeString(Encoded);
  Assert.AreEqual('Test', Decoded);
end;


procedure TTestEncodeMime.TestDeMimeString_Empty;
var
  Decoded: string;
begin
  Decoded:= DeMimeString('');
  Assert.AreEqual('', Decoded, 'Empty input should produce empty output');
end;



{--------------------------------------------------------------------------------------------------
   ANSISTRING TESTS
--------------------------------------------------------------------------------------------------}

procedure TTestEncodeMime.TestMimeStringA_Simple;
var
  Encoded: AnsiString;
begin
  Encoded:= MimeStringA('Hello');
  Assert.IsNotEmpty(string(Encoded));
  Assert.AreNotEqual('Hello', string(Encoded), 'Output should be encoded, not plain text');
end;


procedure TTestEncodeMime.TestMimeStringA_Empty;
var
  Encoded: AnsiString;
begin
  Encoded:= MimeStringA('');
  Assert.AreEqual('', string(Encoded), 'Empty input should produce empty output');
end;


procedure TTestEncodeMime.TestMimeStringA_RoundTrip;
var
  Original, Decoded: AnsiString;
begin
  Original:= 'Test AnsiString 12345!';
  Decoded:= DeMimeStringA(MimeStringA(Original));
  Assert.AreEqual(string(Original), string(Decoded), 'AnsiString round-trip should preserve content');
end;


procedure TTestEncodeMime.TestMimeStringA_LongString;
var
  Original, Decoded: AnsiString;
  i: Integer;
begin
  { Build a long AnsiString (10KB+) }
  Original:= '';
  for i:= 1 to 1000 do
    Original:= Original + 'ABCDEFGHIJ';

  Decoded:= DeMimeStringA(MimeStringA(Original));
  Assert.AreEqual(Length(Original), Length(Decoded), 'Long AnsiString length should be preserved');
  Assert.AreEqual(string(Original), string(Decoded), 'Long AnsiString content should be preserved');
end;


procedure TTestEncodeMime.TestDeMimeStringA_Simple;
var
  Encoded, Decoded: AnsiString;
begin
  Encoded:= MimeStringA('Test');
  Decoded:= DeMimeStringA(Encoded);
  Assert.AreEqual('Test', string(Decoded));
end;


procedure TTestEncodeMime.TestDeMimeStringA_Empty;
var
  Decoded: AnsiString;
begin
  Decoded:= DeMimeStringA('');
  Assert.AreEqual('', string(Decoded), 'Empty input should produce empty output');
end;



{--------------------------------------------------------------------------------------------------
   KNOWN VALUE TESTS
   These verify our implementation against standard Base64 encoding.
--------------------------------------------------------------------------------------------------}

procedure TTestEncodeMime.TestMimeString_KnownValue_Hello;
var
  Encoded: string;
begin
  { Standard Base64 of 'Hello' (UTF-8) is 'SGVsbG8=' }
  Encoded:= MimeString('Hello');
  Assert.AreEqual('SGVsbG8=', Encoded, 'Base64 of "Hello" should be "SGVsbG8="');
end;


procedure TTestEncodeMime.TestMimeString_KnownValue_HelloWorld;
var
  Encoded: string;
begin
  { Standard Base64 of 'Hello World' (UTF-8) is 'SGVsbG8gV29ybGQ=' }
  Encoded:= MimeString('Hello World');
  Assert.AreEqual('SGVsbG8gV29ybGQ=', Encoded, 'Base64 of "Hello World" should be "SGVsbG8gV29ybGQ="');
end;


procedure TTestEncodeMime.TestMimeStringA_KnownValue_Hello;
var
  Encoded: AnsiString;
begin
  { Standard Base64 of 'Hello' is 'SGVsbG8=' }
  Encoded:= MimeStringA('Hello');
  Assert.AreEqual('SGVsbG8=', string(Encoded), 'Base64 of "Hello" should be "SGVsbG8="');
end;


procedure TTestEncodeMime.TestMimeStringA_KnownValue_ABC;
var
  Encoded: AnsiString;
begin
  { Standard Base64 of 'ABC' is 'QUJD' }
  Encoded:= MimeStringA('ABC');
  Assert.AreEqual('QUJD', string(Encoded), 'Base64 of "ABC" should be "QUJD"');
end;



{--------------------------------------------------------------------------------------------------
   EDGE CASES
--------------------------------------------------------------------------------------------------}

procedure TTestEncodeMime.TestMimeString_SingleChar;
var
  Original, Decoded: string;
begin
  Original:= 'A';
  Decoded:= DeMimeString(MimeString(Original));
  Assert.AreEqual(Original, Decoded, 'Single character should round-trip correctly');
end;


procedure TTestEncodeMime.TestMimeStringA_SingleChar;
var
  Original, Decoded: AnsiString;
begin
  Original:= 'A';
  Decoded:= DeMimeStringA(MimeStringA(Original));
  Assert.AreEqual(string(Original), string(Decoded), 'Single character should round-trip correctly');
end;


procedure TTestEncodeMime.TestMimeString_BinaryLikeData;
var
  Original, Decoded: string;
begin
  { Test with characters that might cause issues in binary contexts }
  Original:= #0#1#2#255'Test'#254#253;
  Decoded:= DeMimeString(MimeString(Original));
  Assert.AreEqual(Original, Decoded, 'Binary-like data should round-trip correctly');
end;


procedure TTestEncodeMime.TestMimeStringA_HighBytes;
var
  Original, Decoded: AnsiString;
  i: Integer;
begin
  { Test AnsiString with all high-byte values (128-255) }
  SetLength(Original, 128);
  for i:= 0 to 127 do
    Original[i + 1]:= AnsiChar(128 + i);

  Decoded:= DeMimeStringA(MimeStringA(Original));
  Assert.AreEqual(Length(Original), Length(Decoded), 'High-byte data length should be preserved');
  Assert.AreEqual(string(Original), string(Decoded), 'High-byte data should round-trip correctly');
end;


procedure TTestEncodeMime.TestDeMimeString_InvalidBase64;
begin
  { Decoding invalid Base64 raises EEncodingError due to invalid characters.
    This is expected behavior - the decoder does not silently accept garbage. }
  Assert.WillRaiseAny(
    procedure
    begin
      DeMimeString('!!!invalid!!!');
    end,
    'Invalid Base64 should raise an exception');
end;


procedure TTestEncodeMime.TestDeMimeStringA_InvalidBase64;
begin
  { Decoding invalid Base64 with AnsiString variant should also raise an exception }
  Assert.WillRaiseAny(
    procedure
    begin
      DeMimeStringA('!!!invalid!!!');
    end,
    'Invalid Base64 should raise an exception for AnsiString variant');
end;


procedure TTestEncodeMime.TestMimeString_PaddingVariants;
var
  Encoded: string;
begin
  { Test different input lengths that produce different padding:
    - 1 byte input: 2 padding chars (==)
    - 2 byte input: 1 padding char (=)
    - 3 byte input: no padding }
  Encoded:= MimeString('A');
  Assert.IsTrue(Encoded.EndsWith('=='), '1 char should produce "==" padding');

  Encoded:= MimeString('AB');
  Assert.IsTrue(Encoded.EndsWith('=') AND NOT Encoded.EndsWith('=='), '2 chars should produce "=" padding');

  Encoded:= MimeString('ABC');
  Assert.IsFalse(Encoded.EndsWith('='), '3 chars should produce no padding');
end;


procedure TTestEncodeMime.TestMimeString_Whitespace;
var
  Original, Decoded: string;
begin
  { Test strings containing only whitespace characters }
  Original:= '   ';
  Decoded:= DeMimeString(MimeString(Original));
  Assert.AreEqual(Original, Decoded, 'Whitespace-only string should round-trip correctly');

  Original:= #9#9#9;  { Tabs }
  Decoded:= DeMimeString(MimeString(Original));
  Assert.AreEqual(Original, Decoded, 'Tab-only string should round-trip correctly');
end;


INITIALIZATION
  TDUnitX.RegisterTestFixture(TTestEncodeMime);

end.
