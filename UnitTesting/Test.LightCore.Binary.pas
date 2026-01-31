unit Test.LightCore.Binary;

{=============================================================================================================
   Unit tests for LightCore.Binary
   Tests binary operations, hex conversions, bit manipulation, and byte swapping
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils, System.Types, System.Classes;

type
  [TestFixture]
  TTestLightCoreBinary = class
  public
    { String to Number Conversions }
    [Test]
    procedure TestHexToInt_ValidHex;

    [Test]
    procedure TestHexToInt_LowerCase;

    [Test]
    procedure TestHexToInt_EmptyString;

    [Test]
    procedure TestHexToInt_InvalidChar;

    [Test]
    procedure TestBinToInt_AllZeros;

    [Test]
    procedure TestBinToInt_AllOnes;

    [Test]
    procedure TestBinToInt_Mixed;

    [Test]
    procedure TestIntToByte_Normal;

    [Test]
    procedure TestIntToByte_Overflow;

    [Test]
    procedure TestIntToByte_Negative;

    [Test]
    procedure TestStringIsHexNumber_Valid;

    [Test]
    procedure TestStringIsHexNumber_WithDollar;

    [Test]
    procedure TestStringIsHexNumber_Invalid;

    [Test]
    procedure TestStringIsHexNumber_Empty;

    [Test]
    procedure TestStringIsHexNumber_OnlyDollar;

    { Number to String Conversions }
    [Test]
    procedure TestIntToBin_Zero;

    [Test]
    procedure TestIntToBin_Eight;

    [Test]
    procedure TestIntToBin_255;

    [Test]
    procedure TestByteToBin_Zero;

    [Test]
    procedure TestByteToBin_255;

    [Test]
    procedure TestWordToBin_Zero;

    [Test]
    procedure TestWordToBin_MaxWord;

    { Bit Operations }
    [Test]
    procedure TestGetBit_BitSet;

    [Test]
    procedure TestGetBit_BitClear;

    [Test]
    procedure TestSetBit;

    [Test]
    procedure TestClearBit;

    [Test]
    procedure TestToggleBit_On;

    [Test]
    procedure TestToggleBit_Off;

    [Test]
    procedure TestMakeByte;

    { Byte Extraction }
    [Test]
    procedure TestGetByte_Cardinal_Pos1;

    [Test]
    procedure TestGetByte_Cardinal_Pos4;

    [Test]
    procedure TestGetByte_Word_Pos1;

    [Test]
    procedure TestGetByte_Word_Pos2;

    [Test]
    procedure TestGetByte_InvalidPos;

    { Byte Swapping }
    [Test]
    procedure TestSwapWord;

    [Test]
    procedure TestSwapCardinal;

    [Test]
    procedure TestSwapInt;

    [Test]
    procedure TestSwapUInt64;

    [Test]
    procedure TestSwapUInt64_Zero;

    [Test]
    procedure TestSwapUInt64_MaxValue;

    { Byte Reversal }
    [Test]
    procedure TestReverseByte_Zero;

    [Test]
    procedure TestReverseByte_AllOnes;

    [Test]
    procedure TestReverseByte_Pattern;

    [Test]
    procedure TestReverseByte2_CompareWithReverseByte;

    [Test]
    procedure TestReverseByte3_CompareWithReverseByte;

    { Rotation }
    [Test]
    procedure TestRotateRight32;

    [Test]
    procedure TestRotateLeft32;

    [Test]
    procedure TestRotateRight64;

    [Test]
    procedure TestRotateLeft64;

    { Make Functions }
    [Test]
    procedure TestMakeWord;

    [Test]
    procedure TestMakeCardinal_Bytes;

    [Test]
    procedure TestMakeCardinal_HexString;

    [Test]
    procedure TestMakeCardinal_FourHexStrings;

    [Test]
    procedure TestMakeCardinal_InvalidLength_Short;

    [Test]
    procedure TestMakeCardinal_InvalidLength_Long;

    [Test]
    procedure TestMakeCardinal_Empty;

    { Ensure Range }
    [Test]
    procedure TestEnsureByte_Integer_InRange;

    [Test]
    procedure TestEnsureByte_Integer_TooHigh;

    [Test]
    procedure TestEnsureByte_Integer_Negative;

    [Test]
    procedure TestEnsure100_Integer_InRange;

    [Test]
    procedure TestEnsure100_Integer_TooHigh;

    [Test]
    procedure TestEnsure100_Integer_Negative;

    { Base Conversion }
    [Test]
    procedure TestBase255to256_And_Back;

    { Serialization }
    [Test]
    procedure TestSerializeCardinal;

    { Stream Reading - Motorola format (big-endian) }
    [Test]
    procedure TestReadMotorolaWord_Valid;

    [Test]
    procedure TestReadMotorolaWord_EmptyStream;

    [Test]
    procedure TestReadMotorolaWord_InsufficientBytes;
  end;

implementation

uses
  LightCore.Binary;

{ String to Number Conversions }

procedure TTestLightCoreBinary.TestHexToInt_ValidHex;
begin
  Assert.AreEqual(Integer(255), HexToInt('FF'));
  Assert.AreEqual(Integer(16), HexToInt('10'));
  Assert.AreEqual(Integer(0), HexToInt('0'));
  Assert.AreEqual(Integer(4095), HexToInt('FFF'));
end;

procedure TTestLightCoreBinary.TestHexToInt_LowerCase;
begin
  Assert.AreEqual(Integer(255), HexToInt('ff'));
  Assert.AreEqual(Integer(171), HexToInt('ab'));
end;

procedure TTestLightCoreBinary.TestHexToInt_EmptyString;
begin
  Assert.WillRaise(
    procedure
    begin
      HexToInt('');
    end,
    EConvertError
  );
end;

procedure TTestLightCoreBinary.TestHexToInt_InvalidChar;
begin
  Assert.WillRaise(
    procedure
    begin
      HexToInt('GG');
    end,
    EConvertError
  );
end;

procedure TTestLightCoreBinary.TestBinToInt_AllZeros;
begin
  Assert.AreEqual(0, BinToInt('00000000'));
end;

procedure TTestLightCoreBinary.TestBinToInt_AllOnes;
begin
  Assert.AreEqual(255, BinToInt('11111111'));
end;

procedure TTestLightCoreBinary.TestBinToInt_Mixed;
begin
  Assert.AreEqual(170, BinToInt('10101010'));
  Assert.AreEqual(85, BinToInt('01010101'));
end;

procedure TTestLightCoreBinary.TestIntToByte_Normal;
begin
  Assert.AreEqual(Byte(100), IntToByte(100));
  Assert.AreEqual(Byte(0), IntToByte(0));
  Assert.AreEqual(Byte(255), IntToByte(255));
end;

procedure TTestLightCoreBinary.TestIntToByte_Overflow;
begin
  Assert.AreEqual(Byte(255), IntToByte(300));
  Assert.AreEqual(Byte(255), IntToByte(1000));
end;

procedure TTestLightCoreBinary.TestIntToByte_Negative;
begin
  Assert.AreEqual(Byte(0), IntToByte(-1));
  Assert.AreEqual(Byte(0), IntToByte(-100));
end;

procedure TTestLightCoreBinary.TestStringIsHexNumber_Valid;
begin
  Assert.IsTrue(StringIsHexNumber('FF'));
  Assert.IsTrue(StringIsHexNumber('1234'));
  Assert.IsTrue(StringIsHexNumber('ABCDEF'));
  Assert.IsTrue(StringIsHexNumber('abcdef'));
end;

procedure TTestLightCoreBinary.TestStringIsHexNumber_WithDollar;
begin
  Assert.IsTrue(StringIsHexNumber('$FF'));
  Assert.IsTrue(StringIsHexNumber('$1234'));
end;

procedure TTestLightCoreBinary.TestStringIsHexNumber_Invalid;
begin
  Assert.IsFalse(StringIsHexNumber('GG'));
  Assert.IsFalse(StringIsHexNumber('XYZ'));
end;

procedure TTestLightCoreBinary.TestStringIsHexNumber_Empty;
begin
  { Empty string raises an exception per implementation }
  Assert.WillRaise(
    procedure
    begin
      StringIsHexNumber('');
    end,
    Exception);
end;

procedure TTestLightCoreBinary.TestStringIsHexNumber_OnlyDollar;
begin
  { Just a dollar sign with no hex digits should return FALSE }
  Assert.IsFalse(StringIsHexNumber('$'));
end;

{ Number to String Conversions }

procedure TTestLightCoreBinary.TestIntToBin_Zero;
begin
  Assert.AreEqual('00000000', IntToBin(0, 8));
end;

procedure TTestLightCoreBinary.TestIntToBin_Eight;
begin
  Assert.AreEqual('1000', IntToBin(8, 4));
end;

procedure TTestLightCoreBinary.TestIntToBin_255;
begin
  Assert.AreEqual('11111111', IntToBin(255, 8));
end;

procedure TTestLightCoreBinary.TestByteToBin_Zero;
begin
  Assert.AreEqual('00000000', ByteToBin(0));
end;

procedure TTestLightCoreBinary.TestByteToBin_255;
begin
  Assert.AreEqual('11111111', ByteToBin(255));
end;

procedure TTestLightCoreBinary.TestWordToBin_Zero;
begin
  Assert.AreEqual('0000000000000000', WordToBin(0));
end;

procedure TTestLightCoreBinary.TestWordToBin_MaxWord;
begin
  Assert.AreEqual('1111111111111111', WordToBin(65535));
end;

{ Bit Operations }

procedure TTestLightCoreBinary.TestGetBit_BitSet;
begin
  Assert.IsTrue(GetBit($80, 7));  { 10000000 - bit 7 is set }
  Assert.IsTrue(GetBit($01, 0));  { 00000001 - bit 0 is set }
end;

procedure TTestLightCoreBinary.TestGetBit_BitClear;
begin
  Assert.IsFalse(GetBit($7F, 7)); { 01111111 - bit 7 is clear }
  Assert.IsFalse(GetBit($FE, 0)); { 11111110 - bit 0 is clear }
end;

procedure TTestLightCoreBinary.TestSetBit;
begin
  Assert.AreEqual(Cardinal($01), SetBit(0, 0));
  Assert.AreEqual(Cardinal($80), SetBit(0, 7));
  Assert.AreEqual(Cardinal($FF), SetBit($7F, 7));
end;

procedure TTestLightCoreBinary.TestClearBit;
begin
  Assert.AreEqual(Cardinal($FE), ClearBit($FF, 0));
  Assert.AreEqual(Cardinal($7F), ClearBit($FF, 7));
  Assert.AreEqual(Cardinal(0), ClearBit($01, 0));
end;

procedure TTestLightCoreBinary.TestToggleBit_On;
begin
  Assert.AreEqual(Cardinal($01), ToggleBit(0, 0, True));
  Assert.AreEqual(Cardinal($FF), ToggleBit($FF, 0, True));  { Already on, stays on }
end;

procedure TTestLightCoreBinary.TestToggleBit_Off;
begin
  Assert.AreEqual(Cardinal($FE), ToggleBit($FF, 0, False));
  Assert.AreEqual(Cardinal(0), ToggleBit(0, 0, False));     { Already off, stays off }
end;

procedure TTestLightCoreBinary.TestMakeByte;
begin
  Assert.AreEqual(Byte($FF), MakeByte(True, True, True, True, True, True, True, True));
  Assert.AreEqual(Byte($00), MakeByte(False, False, False, False, False, False, False, False));
  Assert.AreEqual(Byte($80), MakeByte(True, False, False, False, False, False, False, False));
  Assert.AreEqual(Byte($01), MakeByte(False, False, False, False, False, False, False, True));
end;

{ Byte Extraction }

procedure TTestLightCoreBinary.TestGetByte_Cardinal_Pos1;
begin
  Assert.AreEqual(Byte($AA), GetByte(1, Cardinal($AABBCCDD)));
end;

procedure TTestLightCoreBinary.TestGetByte_Cardinal_Pos4;
begin
  Assert.AreEqual(Byte($DD), GetByte(4, Cardinal($AABBCCDD)));
end;

procedure TTestLightCoreBinary.TestGetByte_Word_Pos1;
begin
  Assert.AreEqual(Byte($AA), GetByte(1, Word($AABB)));
end;

procedure TTestLightCoreBinary.TestGetByte_Word_Pos2;
begin
  Assert.AreEqual(Byte($BB), GetByte(2, Word($AABB)));
end;

procedure TTestLightCoreBinary.TestGetByte_InvalidPos;
begin
  Assert.WillRaise(
    procedure
    begin
      GetByte(5, Cardinal($AABBCCDD));
    end,
    Exception
  );
end;

{ Byte Swapping }

procedure TTestLightCoreBinary.TestSwapWord;
var
  W: Word;
begin
  W:= $AABB;
  SwapWord(W);
  Assert.AreEqual(Word($BBAA), W);
end;

procedure TTestLightCoreBinary.TestSwapCardinal;
var
  C: Cardinal;
begin
  C:= $AABBCCDD;
  SwapCardinal(C);
  Assert.AreEqual(Cardinal($DDCCBBAA), C);
end;

procedure TTestLightCoreBinary.TestSwapInt;
var
  I: Integer;
begin
  I:= $12345678;
  SwapInt(I);
  Assert.AreEqual(Integer($78563412), I);
end;

procedure TTestLightCoreBinary.TestSwapUInt64;
var
  V: UInt64;
begin
  { Test byte reversal: $0102030405060708 -> $0807060504030201 }
  V:= $0102030405060708;
  Assert.AreEqual(UInt64($0807060504030201), SwapUInt64(V));
end;

procedure TTestLightCoreBinary.TestSwapUInt64_Zero;
begin
  { Zero should remain zero }
  Assert.AreEqual(UInt64(0), SwapUInt64(0));
end;

procedure TTestLightCoreBinary.TestSwapUInt64_MaxValue;
begin
  { MaxValue should remain MaxValue (all bytes are $FF) }
  Assert.AreEqual(UInt64($FFFFFFFFFFFFFFFF), SwapUInt64($FFFFFFFFFFFFFFFF));
end;

{ Byte Reversal }

procedure TTestLightCoreBinary.TestReverseByte_Zero;
begin
  Assert.AreEqual(Byte(0), ReverseByte(0));
end;

procedure TTestLightCoreBinary.TestReverseByte_AllOnes;
begin
  Assert.AreEqual(Byte(255), ReverseByte(255));
end;

procedure TTestLightCoreBinary.TestReverseByte_Pattern;
begin
  { 10000000 reversed = 00000001 }
  Assert.AreEqual(Byte($01), ReverseByte($80));
  { 11110000 reversed = 00001111 }
  Assert.AreEqual(Byte($0F), ReverseByte($F0));
end;

procedure TTestLightCoreBinary.TestReverseByte2_CompareWithReverseByte;
var
  i: Integer;
begin
  { ReverseByte2 uses LUT and should produce same results as ReverseByte }
  for i:= 0 to 255 do
    Assert.AreEqual(ReverseByte(i), ReverseByte2(i), 'Mismatch at byte ' + IntToStr(i));
end;

procedure TTestLightCoreBinary.TestReverseByte3_CompareWithReverseByte;
var
  i: Integer;
begin
  for i:= 0 to 255 do
    Assert.AreEqual(ReverseByte(i), ReverseByte3(i), 'Mismatch at byte ' + IntToStr(i));
end;

{ Rotation }

procedure TTestLightCoreBinary.TestRotateRight32;
var
  V: DWord;
begin
  V:= $80000000;
  Assert.AreEqual(DWord($40000000), RotateRight32(V, 1));
  Assert.AreEqual(DWord($00000001), RotateRight32(V, 31));
end;

procedure TTestLightCoreBinary.TestRotateLeft32;
var
  V: DWord;
begin
  V:= $00000001;
  Assert.AreEqual(DWord($00000002), RotateLeft32(V, 1));
  Assert.AreEqual(DWord($80000000), RotateLeft32(V, 31));
end;

procedure TTestLightCoreBinary.TestRotateRight64;
var
  V: Int64;
begin
  V:= Int64($8000000000000000);
  Assert.AreEqual(Int64($4000000000000000), RotateRight64(V, 1));
end;

procedure TTestLightCoreBinary.TestRotateLeft64;
var
  V: Int64;
begin
  V:= Int64($0000000000000001);
  Assert.AreEqual(Int64($0000000000000002), RotateLeft64(V, 1));
end;

{ Make Functions }

procedure TTestLightCoreBinary.TestMakeWord;
begin
  Assert.AreEqual(Word($AABB), MakeWord($AA, $BB));
  Assert.AreEqual(Word($0100), MakeWord($01, $00));
  Assert.AreEqual(Word(256), MakeWord(1, 0));
end;

procedure TTestLightCoreBinary.TestMakeCardinal_Bytes;
begin
  Assert.AreEqual(Cardinal($AABBCCDD), MakeCardinal_($AA, $BB, $CC, $DD));
end;

procedure TTestLightCoreBinary.TestMakeCardinal_HexString;
begin
  Assert.AreEqual(Cardinal($AABBCCDD), MakeCardinal('AABBCCDD'));
  Assert.AreEqual(Cardinal($12345678), MakeCardinal('12345678'));
end;

procedure TTestLightCoreBinary.TestMakeCardinal_FourHexStrings;
begin
  Assert.AreEqual(Cardinal($AABBCCDD), MakeCardinal('AA', 'BB', 'CC', 'DD'));
end;

procedure TTestLightCoreBinary.TestMakeCardinal_InvalidLength_Short;
begin
  { Hex string must be exactly 8 characters - raises EConvertError }
  Assert.WillRaise(
    procedure
    begin
      MakeCardinal('AABB');  { Only 4 chars }
    end,
    EConvertError
  );
end;

procedure TTestLightCoreBinary.TestMakeCardinal_InvalidLength_Long;
begin
  { Hex string must be exactly 8 characters - raises EConvertError }
  Assert.WillRaise(
    procedure
    begin
      MakeCardinal('AABBCCDDEE');  { 10 chars }
    end,
    EConvertError
  );
end;

procedure TTestLightCoreBinary.TestMakeCardinal_Empty;
begin
  { Empty string raises EConvertError }
  Assert.WillRaise(
    procedure
    begin
      MakeCardinal('');
    end,
    EConvertError
  );
end;

{ Ensure Range }

procedure TTestLightCoreBinary.TestEnsureByte_Integer_InRange;
begin
  Assert.AreEqual(Byte(0), EnsureByte(Integer(0)));
  Assert.AreEqual(Byte(128), EnsureByte(Integer(128)));
  Assert.AreEqual(Byte(255), EnsureByte(Integer(255)));
end;

procedure TTestLightCoreBinary.TestEnsureByte_Integer_TooHigh;
begin
  Assert.AreEqual(Byte(255), EnsureByte(Integer(300)));
  Assert.AreEqual(Byte(255), EnsureByte(Integer(1000)));
end;

procedure TTestLightCoreBinary.TestEnsureByte_Integer_Negative;
begin
  Assert.AreEqual(Byte(0), EnsureByte(Integer(-1)));
  Assert.AreEqual(Byte(0), EnsureByte(Integer(-100)));
end;

procedure TTestLightCoreBinary.TestEnsure100_Integer_InRange;
begin
  Assert.AreEqual(Byte(0), Ensure100(Integer(0)));
  Assert.AreEqual(Byte(50), Ensure100(Integer(50)));
  Assert.AreEqual(Byte(100), Ensure100(Integer(100)));
end;

procedure TTestLightCoreBinary.TestEnsure100_Integer_TooHigh;
begin
  Assert.AreEqual(Byte(100), Ensure100(Integer(150)));
  Assert.AreEqual(Byte(100), Ensure100(Integer(1000)));
end;

procedure TTestLightCoreBinary.TestEnsure100_Integer_Negative;
begin
  Assert.AreEqual(Byte(0), Ensure100(Integer(-1)));
  Assert.AreEqual(Byte(0), Ensure100(Integer(-100)));
end;

{ Base Conversion }

procedure TTestLightCoreBinary.TestBase255to256_And_Back;
var
  Original, Converted, BackConverted: Cardinal;
begin
  Original:= 1000000;
  Converted:= Base255to256(Original);
  BackConverted:= Base256to255(Converted);
  Assert.AreEqual(Original, BackConverted);
end;

{ Serialization }

procedure TTestLightCoreBinary.TestSerializeCardinal;
begin
  Assert.AreEqual('AABBCCDD', SerializeCardinal($AABBCCDD));
  Assert.AreEqual('12345678', SerializeCardinal($12345678));
end;

{ Stream Reading - Motorola format (big-endian) }

procedure TTestLightCoreBinary.TestReadMotorolaWord_Valid;
var
  Stream: TMemoryStream;
  B: Byte;
begin
  Stream:= TMemoryStream.Create;
  try
    { Write bytes in big-endian order: $AB, $CD -> Word $ABCD }
    B:= $AB;
    Stream.Write(B, SizeOf(B));
    B:= $CD;
    Stream.Write(B, SizeOf(B));
    Stream.Position:= 0;

    Assert.AreEqual(Word($ABCD), ReadMotorolaWord(Stream));
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TTestLightCoreBinary.TestReadMotorolaWord_EmptyStream;
var
  Stream: TMemoryStream;
begin
  Stream:= TMemoryStream.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        ReadMotorolaWord(Stream);
      end,
      Exception
    );
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TTestLightCoreBinary.TestReadMotorolaWord_InsufficientBytes;
var
  Stream: TMemoryStream;
  B: Byte;
begin
  Stream:= TMemoryStream.Create;
  try
    { Only write 1 byte when 2 are needed }
    B:= $AB;
    Stream.Write(B, SizeOf(B));
    Stream.Position:= 0;

    Assert.WillRaise(
      procedure
      begin
        ReadMotorolaWord(Stream);
      end,
      Exception);
  finally
    FreeAndNil(Stream);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestLightCoreBinary);

end.
