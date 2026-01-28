unit Test.LightCore.EncodeXOR;

{=============================================================================================================
   Unit tests for LightCore.EncodeXOR
   Tests XOR encoding/decoding and simple encryption functions

   Requires: TESTINSIGHT compiler directive for TestInsight integration
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  LightCore.Types,
  LightCore.EncodeXOR;

type
  [TestFixture]
  TTestEncodeXOR = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestSimpleEncode_Decode_Roundtrip;

    [Test]
    procedure TestSimpleEncode_ShiftsChars;

    [Test]
    procedure TestEncodeDecode_NOT_Roundtrip;

    [Test]
    procedure TestEncodeDecode_XOR_String_Roundtrip;

    [Test]
    procedure TestEncodeDecode_XOR_AnsiString_Roundtrip;

    [Test]
    procedure TestEncodeDecode_XOR_Bytes_Roundtrip;

    [Test]
    procedure TestBetterEncode_Decode_Roundtrip;

    [Test]
    procedure TestBetterEncode_DifferentKeys;

    [Test]
    procedure TestBetterDecode_InvalidLength;

    [Test]
    procedure TestEncodeXorText_DecodeXorText_Roundtrip;
  end;

implementation

procedure TTestEncodeXOR.Setup;
begin
  { No setup needed }
end;

procedure TTestEncodeXOR.TearDown;
begin
  { No teardown needed }
end;

procedure TTestEncodeXOR.TestSimpleEncode_Decode_Roundtrip;
var
  Original, Encoded, Decoded: string;
begin
  Original := 'TestString123';
  Encoded := SimpleEncode(Original);
  Decoded := SimpleDecode(Encoded);

  Assert.AreEqual(Original, Decoded, 'SimpleEncode followed by SimpleDecode should return original');
end;

procedure TTestEncodeXOR.TestSimpleEncode_ShiftsChars;
var
  Original, Encoded: string;
begin
  Original := 'ABC';
  Encoded := SimpleEncode(Original);

  { SimpleEncode shifts each character by +1 }
  Assert.AreEqual('BCD', Encoded, 'SimpleEncode should shift each character by +1');
end;

procedure TTestEncodeXOR.TestEncodeDecode_NOT_Roundtrip;
var
  Original, FirstPass, SecondPass: string;
begin
  Original := 'TestNOT';
  FirstPass := EncodeDecode_NOT(Original);
  SecondPass := EncodeDecode_NOT(FirstPass);

  Assert.AreEqual(Original, SecondPass, 'Double NOT should return original (NOT is its own inverse)');
end;

procedure TTestEncodeXOR.TestEncodeDecode_XOR_String_Roundtrip;
var
  Original, Encoded, Decoded: string;
  Key: Byte;
begin
  Original := 'XOR Test String!';
  Key := 42;

  Encoded := EncodeDecode_XOR(Original, Key);
  Decoded := EncodeDecode_XOR(Encoded, Key);

  Assert.AreEqual(Original, Decoded, 'XOR is its own inverse - double XOR should return original');
end;

procedure TTestEncodeXOR.TestEncodeDecode_XOR_AnsiString_Roundtrip;
var
  Original, Encoded, Decoded: AnsiString;
  Key: Byte;
begin
  Original := 'AnsiXOR';
  Key := 99;

  Encoded := EncodeDecode_XOR(Original, Key);
  Decoded := EncodeDecode_XOR(Encoded, Key);

  Assert.AreEqual(Original, Decoded, 'XOR should work correctly with AnsiString');
end;

procedure TTestEncodeXOR.TestEncodeDecode_XOR_Bytes_Roundtrip;
var
  Original, Encoded, Decoded: TBytesArray;
  Key: Byte;
  i: Integer;
begin
  SetLength(Original, 5);
  Original[0] := 10;
  Original[1] := 20;
  Original[2] := 30;
  Original[3] := 40;
  Original[4] := 50;
  Key := 128;

  Encoded := EncodeDecode_XOR(Original, Key);
  Decoded := EncodeDecode_XOR(Encoded, Key);

  Assert.AreEqual(Length(Original), Length(Decoded), 'Length should be preserved');
  for i := 0 to High(Original) do
    Assert.AreEqual(Original[i], Decoded[i], Format('Byte at index %d should match', [i]));
end;

procedure TTestEncodeXOR.TestBetterEncode_Decode_Roundtrip;
var
  Original, Encoded, Decoded: string;
  StartKey: Word;
begin
  Original := 'BetterEncryption123';
  StartKey := 12345;

  Encoded := BetterEncode(Original, StartKey);
  Decoded := BetterDecode(Encoded, StartKey);

  Assert.AreEqual(Original, Decoded, 'BetterEncode/Decode roundtrip should return original');
end;

procedure TTestEncodeXOR.TestBetterEncode_DifferentKeys;
var
  Original: string;
  Encoded1, Encoded2: string;
begin
  Original := 'KeyTest';

  Encoded1 := BetterEncode(Original, 100);
  Encoded2 := BetterEncode(Original, 200);

  Assert.AreNotEqual(Encoded1, Encoded2, 'Different keys should produce different encodings');
end;

procedure TTestEncodeXOR.TestBetterDecode_InvalidLength;
begin
  Assert.WillRaise(
    procedure
    begin
      BetterDecode('ABC', 100);  { Odd length - invalid }
    end,
    Exception,
    'BetterDecode should raise exception for odd-length input'
  );
end;

procedure TTestEncodeXOR.TestEncodeXorText_DecodeXorText_Roundtrip;
var
  PlainText: string;
  Key: Byte;
  EncodedDef: string;
  EncodedArr: array of Byte;
  Decoded: string;
  i: Integer;
begin
  PlainText := 'ABC';
  Key := 99;

  { EncodeXorText returns a string like: array[0..2] of Byte = ($22, $21, $20); }
  EncodedDef := EncodeXorText(PlainText, Key);

  { Manually create the byte array to test DecodeXorText }
  SetLength(EncodedArr, 3);
  for i := 1 to Length(PlainText) do
    EncodedArr[i-1] := Byte(PlainText[i]) XOR Key;

  Decoded := DecodeXorText(EncodedArr, Key);

  Assert.AreEqual(PlainText, Decoded, 'DecodeXorText should recover original plaintext');
end;

initialization
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  {$ENDIF}
  TDUnitX.RegisterTestFixture(TTestEncodeXOR);

end.
