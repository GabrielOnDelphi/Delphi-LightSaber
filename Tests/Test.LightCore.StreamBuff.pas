unit Test.LightCore.StreamBuff;

{=============================================================================================================
   Unit tests for LightCore.StreamBuff
   Tests TLightStream - extended buffered file stream with versioning and type-safe read/write.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.IOUtils,
  LightCore.Types;

type
  [TestFixture]
  TTestLightStream = class
  private
    FTestDir: string;
    FTestFile: string;
    procedure CleanupTestFile;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor Tests }
    [Test]
    procedure TestCreateRead;

    [Test]
    procedure TestCreateWrite;

    { Header Tests }
    [Test]
    procedure TestWriteReadHeader;

    [Test]
    procedure TestReadHeader_WrongSignature;

    [Test]
    procedure TestReadHeader_WrongVersion;

    [Test]
    procedure TestReadHeader_VersionOnly;

    [Test]
    procedure TestReadHeader_LongSignature;

    { Numeric Types Tests }
    [Test]
    procedure TestBoolean;

    [Test]
    procedure TestBoolean_NonZeroValues;

    [Test]
    procedure TestByte;

    [Test]
    procedure TestShortInt;

    [Test]
    procedure TestSmallInt;

    [Test]
    procedure TestWord;

    [Test]
    procedure TestInteger;

    [Test]
    procedure TestCardinal;

    [Test]
    procedure TestInt64;

    [Test]
    procedure TestUInt64;

    [Test]
    procedure TestSingle;

    [Test]
    procedure TestDouble;

    [Test]
    procedure TestDate;

    { String Tests }
    [Test]
    procedure TestString_Empty;

    [Test]
    procedure TestString_Short;

    [Test]
    procedure TestString_Unicode;

    [Test]
    procedure TestStringA_Empty;

    [Test]
    procedure TestStringA_Short;

    [Test]
    procedure TestStringA_SafetyLimit;

    { Complex Structure Tests }
    [Test]
    procedure TestRect;

    [Test]
    procedure TestRectF;

    [Test]
    procedure TestIntegers;

    [Test]
    procedure TestIntegers_Empty;

    [Test]
    procedure TestDoubles;

    [Test]
    procedure TestDoubles_Empty;

    { Checkpoint Tests }
    [Test]
    procedure TestCheckPoint;

    [Test]
    procedure TestCheckPoint_WithMessage;

    [Test]
    procedure TestCheckPoint_Failure;

    { Padding Tests }
    [Test]
    procedure TestPadding0;

    [Test]
    procedure TestPadding0_Zero;

    [Test]
    procedure TestPadding_WithValidation;

    { Enter Tests }
    [Test]
    procedure TestEnter;

    { Chars Tests }
    [Test]
    procedure TestChars;

    [Test]
    procedure TestChars_Empty;

    [Test]
    procedure TestCharsA;

    [Test]
    procedure TestCharsA_Empty;

    { TStringList Tests }
    [Test]
    procedure TestStrings;

    [Test]
    procedure TestStrings_Empty;

    [Test]
    procedure TestStrings_CreateReturn;

    { Bytes Tests }
    [Test]
    procedure TestPushBytes;

    [Test]
    procedure TestByteChunk;

    [Test]
    procedure TestByteChunk_Empty;

    [Test]
    procedure TestAsBytes;

    [Test]
    procedure TestAsBytes_Empty;

    [Test]
    procedure TestAsString;

    { Raw String Tests }
    [Test]
    procedure TestPushString;

    [Test]
    procedure TestPushAnsi;

    [Test]
    procedure TestPushAnsi_Empty;

    [Test]
    procedure TestTryReadStringA;

    [Test]
    procedure TestTryReadStringA_Partial;

    [Test]
    procedure TestReadStringCnt;

    { Reverse Read Tests (Motorola format) }
    [Test]
    procedure TestRevReadWord;

    [Test]
    procedure TestRevReadCardinal;

    [Test]
    procedure TestRevReadInteger;

    { Combined Tests }
    [Test]
    procedure TestMultipleTypes;

    [Test]
    procedure TestCompleteFile;
  end;


implementation

uses
  LightCore.StreamBuff;


{ Helper Methods }

procedure TTestLightStream.CleanupTestFile;
begin
  if TFile.Exists(FTestFile)
  then TFile.Delete(FTestFile);
end;


procedure TTestLightStream.Setup;
begin
  FTestDir:= TPath.GetTempPath;
  FTestFile:= TPath.Combine(FTestDir, 'LightStreamTest_' + IntToStr(Random(MaxInt)) + '.dat');
end;


procedure TTestLightStream.TearDown;
begin
  CleanupTestFile;
end;


{ Constructor Tests }

procedure TTestLightStream.TestCreateRead;
var
  Stream: TLightStream;
begin
  { Create a test file first }
  Stream:= TLightStream.CreateWrite(FTestFile);
  try
    Stream.WriteInteger(42);
  finally
    Stream.Free;
  end;

  { Now test reading }
  Stream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(42, Stream.ReadInteger);
  finally
    Stream.Free;
  end;
end;


procedure TTestLightStream.TestCreateWrite;
var
  Stream: TLightStream;
begin
  Stream:= TLightStream.CreateWrite(FTestFile);
  try
    Stream.WriteInteger(123);
    Assert.IsTrue(Stream.Size > 0);
  finally
    Stream.Free;
  end;

  Assert.IsTrue(TFile.Exists(FTestFile));
end;


{ Header Tests }

procedure TTestLightStream.TestWriteReadHeader;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteHeader('TestSig', 1);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.IsTrue(ReadStream.ReadHeader('TestSig', 1), 'Header should match');
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestReadHeader_WrongSignature;
var
  WriteStream, ReadStream: TLightStream;
  Version: Word;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteHeader('CorrectSig', 1);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    { ReadHeader returns 0 when signature doesn't match (and logs error internally) }
    Version:= ReadStream.ReadHeader('WrongSig');
    Assert.AreEqual(Word(0), Version, 'Wrong signature should return version 0');
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestReadHeader_WrongVersion;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteHeader('TestSig', 1);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.IsFalse(ReadStream.ReadHeader('TestSig', 2), 'Wrong version should fail');
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestReadHeader_VersionOnly;
var
  WriteStream, ReadStream: TLightStream;
  Version: Word;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteHeader('TestSig', 5);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Version:= ReadStream.ReadHeader('TestSig');
    Assert.AreEqual(Word(5), Version, 'Should return correct version');
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestReadHeader_LongSignature;
var
  WriteStream, ReadStream: TLightStream;
  LongSig: AnsiString;
begin
  LongSig:= 'ThisIsALongerSignatureForTesting';

  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteHeader(LongSig, 10);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.IsTrue(ReadStream.ReadHeader(LongSig, 10), 'Long signature should work');
  finally
    ReadStream.Free;
  end;
end;


{ Numeric Types Tests }

procedure TTestLightStream.TestBoolean;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteBoolean(True);
    WriteStream.WriteBoolean(False);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.IsTrue(ReadStream.ReadBoolean);
    Assert.IsFalse(ReadStream.ReadBoolean);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestBoolean_NonZeroValues;
var
  WriteStream, ReadStream: TLightStream;
begin
  { Test that any non-zero byte is read as True }
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteByte(0);    // False
    WriteStream.WriteByte(1);    // True
    WriteStream.WriteByte(2);    // Should also be True
    WriteStream.WriteByte(255);  // Should also be True
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.IsFalse(ReadStream.ReadBoolean, '0 should be False');
    Assert.IsTrue(ReadStream.ReadBoolean, '1 should be True');
    Assert.IsTrue(ReadStream.ReadBoolean, '2 should be True');
    Assert.IsTrue(ReadStream.ReadBoolean, '255 should be True');
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestByte;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteByte(0);
    WriteStream.WriteByte(127);
    WriteStream.WriteByte(255);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(Byte(0), ReadStream.ReadByte);
    Assert.AreEqual(Byte(127), ReadStream.ReadByte);
    Assert.AreEqual(Byte(255), ReadStream.ReadByte);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestShortInt;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteShortInt(-128);
    WriteStream.WriteShortInt(0);
    WriteStream.WriteShortInt(127);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(ShortInt(-128), ReadStream.ReadShortInt);
    Assert.AreEqual(ShortInt(0), ReadStream.ReadShortInt);
    Assert.AreEqual(ShortInt(127), ReadStream.ReadShortInt);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestSmallInt;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteSmallInt(-32768);
    WriteStream.WriteSmallInt(0);
    WriteStream.WriteSmallInt(32767);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(SmallInt(-32768), ReadStream.ReadSmallInt);
    Assert.AreEqual(SmallInt(0), ReadStream.ReadSmallInt);
    Assert.AreEqual(SmallInt(32767), ReadStream.ReadSmallInt);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestWord;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteWord(0);
    WriteStream.WriteWord(32768);
    WriteStream.WriteWord(65535);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(Word(0), ReadStream.ReadWord);
    Assert.AreEqual(Word(32768), ReadStream.ReadWord);
    Assert.AreEqual(Word(65535), ReadStream.ReadWord);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestInteger;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteInteger(-2147483648);
    WriteStream.WriteInteger(0);
    WriteStream.WriteInteger(2147483647);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(-2147483648, ReadStream.ReadInteger);
    Assert.AreEqual(0, ReadStream.ReadInteger);
    Assert.AreEqual(2147483647, ReadStream.ReadInteger);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestCardinal;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteCardinal(0);
    WriteStream.WriteCardinal(2147483648);
    WriteStream.WriteCardinal(4294967295);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(Cardinal(0), ReadStream.ReadCardinal);
    Assert.AreEqual(Cardinal(2147483648), ReadStream.ReadCardinal);
    Assert.AreEqual(Cardinal(4294967295), ReadStream.ReadCardinal);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestInt64;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteInt64(-9223372036854775808);
    WriteStream.WriteInt64(0);
    WriteStream.WriteInt64(9223372036854775807);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(Int64(-9223372036854775808), ReadStream.ReadInt64);
    Assert.AreEqual(Int64(0), ReadStream.ReadInt64);
    Assert.AreEqual(Int64(9223372036854775807), ReadStream.ReadInt64);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestUInt64;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteUInt64(0);
    WriteStream.WriteUInt64(9223372036854775808);
    WriteStream.WriteUInt64(18446744073709551615);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(UInt64(0), ReadStream.ReadUInt64);
    Assert.AreEqual(UInt64(9223372036854775808), ReadStream.ReadUInt64);
    Assert.AreEqual(UInt64(18446744073709551615), ReadStream.ReadUInt64);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestSingle;
var
  WriteStream, ReadStream: TLightStream;
  Value: Single;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteSingle(0.0);
    WriteStream.WriteSingle(3.14159);
    WriteStream.WriteSingle(-123.456);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Value:= ReadStream.ReadSingle;
    Assert.AreEqual(Single(0.0), Value, 0.0001);

    Value:= ReadStream.ReadSingle;
    Assert.AreEqual(Single(3.14159), Value, 0.0001);

    Value:= ReadStream.ReadSingle;
    Assert.AreEqual(Single(-123.456), Value, 0.001);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestDouble;
var
  WriteStream, ReadStream: TLightStream;
  Value: Double;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteDouble(0.0);
    WriteStream.WriteDouble(3.141592653589793);
    WriteStream.WriteDouble(-123456.789012345);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Value:= ReadStream.ReadDouble;
    Assert.AreEqual(0.0, Value, 0.0000001);

    Value:= ReadStream.ReadDouble;
    Assert.AreEqual(3.141592653589793, Value, 0.0000000001);

    Value:= ReadStream.ReadDouble;
    Assert.AreEqual(-123456.789012345, Value, 0.0000001);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestDate;
var
  WriteStream, ReadStream: TLightStream;
  TestDate, ReadDate: TDateTime;
begin
  TestDate:= EncodeDate(2024, 6, 15) + EncodeTime(14, 30, 45, 500);

  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteDate(TestDate);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    ReadDate:= ReadStream.ReadDate;
    { Compare with small tolerance for floating-point precision }
    Assert.AreEqual(Double(TestDate), Double(ReadDate), 0.00001);
  finally
    ReadStream.Free;
  end;
end;


{ String Tests }

procedure TTestLightStream.TestString_Empty;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteString('');
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual('', ReadStream.ReadString);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestString_Short;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteString('Hello, World!');
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual('Hello, World!', ReadStream.ReadString);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestString_Unicode;
var
  WriteStream, ReadStream: TLightStream;
  TestStr: string;
begin
  TestStr:= 'Hello ' + Char($00E9) + Char($00F1) + Char($00FC) + ' ' + Char($4E2D) + Char($6587);

  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteString(TestStr);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(TestStr, ReadStream.ReadString);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestStringA_Empty;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteStringA('');
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(AnsiString(''), ReadStream.ReadStringA);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestStringA_Short;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteStringA('Test ANSI String');
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(AnsiString('Test ANSI String'), ReadStream.ReadStringA);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestStringA_SafetyLimit;
var
  WriteStream, ReadStream: TLightStream;
  LargeString: AnsiString;
begin
  SetLength(LargeString, 100);
  FillChar(LargeString[1], 100, 'X');

  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteStringA(LargeString);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    { Should raise exception when SafetyLimit is exceeded }
    Assert.WillRaise(
      procedure
      begin
        ReadStream.ReadStringA(50);  // SafetyLimit = 50, but string is 100
      end,
      Exception
    );
  finally
    ReadStream.Free;
  end;
end;


{ Complex Structure Tests }

procedure TTestLightStream.TestRect;
var
  WriteStream, ReadStream: TLightStream;
  WriteRect, ReadRect: TRect;
begin
  WriteRect:= Rect(10, 20, 100, 200);

  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteRect(WriteRect);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    ReadRect:= ReadStream.ReadRect;
    Assert.AreEqual(10, ReadRect.Left);
    Assert.AreEqual(20, ReadRect.Top);
    Assert.AreEqual(100, ReadRect.Right);
    Assert.AreEqual(200, ReadRect.Bottom);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestRectF;
var
  WriteStream, ReadStream: TLightStream;
  WriteRect, ReadRect: TRectF;
begin
  WriteRect:= TRectF.Create(10.5, 20.5, 100.5, 200.5);

  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteRectF(WriteRect);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    ReadRect:= ReadStream.ReadRectF;
    Assert.AreEqual(Single(10.5), ReadRect.Left, 0.001);
    Assert.AreEqual(Single(20.5), ReadRect.Top, 0.001);
    Assert.AreEqual(Single(100.5), ReadRect.Right, 0.001);
    Assert.AreEqual(Single(200.5), ReadRect.Bottom, 0.001);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestIntegers;
var
  WriteStream, ReadStream: TLightStream;
  WriteArr, ReadArr: TIntegerArray;
begin
  SetLength(WriteArr, 5);
  WriteArr[0]:= 1;
  WriteArr[1]:= 2;
  WriteArr[2]:= 3;
  WriteArr[3]:= -100;
  WriteArr[4]:= 1000;

  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteIntegers(WriteArr);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    ReadStream.ReadIntegers(ReadArr);
    Assert.AreEqual(5, Length(ReadArr));
    Assert.AreEqual(1, ReadArr[0]);
    Assert.AreEqual(2, ReadArr[1]);
    Assert.AreEqual(3, ReadArr[2]);
    Assert.AreEqual(-100, ReadArr[3]);
    Assert.AreEqual(1000, ReadArr[4]);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestIntegers_Empty;
var
  WriteStream, ReadStream: TLightStream;
  WriteArr, ReadArr: TIntegerArray;
begin
  SetLength(WriteArr, 0);

  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteIntegers(WriteArr);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    ReadStream.ReadIntegers(ReadArr);
    Assert.AreEqual(0, Length(ReadArr));
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestDoubles;
var
  WriteStream, ReadStream: TLightStream;
  WriteArr, ReadArr: TDoubleArray;
begin
  SetLength(WriteArr, 3);
  WriteArr[0]:= 1.1;
  WriteArr[1]:= 2.2;
  WriteArr[2]:= 3.3;

  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteDoubles(WriteArr);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    ReadStream.ReadDoubles(ReadArr);
    Assert.AreEqual(3, Length(ReadArr));
    Assert.AreEqual(1.1, ReadArr[0], 0.0001);
    Assert.AreEqual(2.2, ReadArr[1], 0.0001);
    Assert.AreEqual(3.3, ReadArr[2], 0.0001);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestDoubles_Empty;
var
  WriteStream, ReadStream: TLightStream;
  WriteArr, ReadArr: TDoubleArray;
begin
  SetLength(WriteArr, 0);

  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteDoubles(WriteArr);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    ReadStream.ReadDoubles(ReadArr);
    Assert.AreEqual(0, Length(ReadArr));
  finally
    ReadStream.Free;
  end;
end;


{ Checkpoint Tests }

procedure TTestLightStream.TestCheckPoint;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteCheckPoint;
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.IsTrue(ReadStream.ReadCheckPoint, 'Checkpoint should pass');
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestCheckPoint_WithMessage;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteCheckPoint('MyCheckpoint');
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.IsTrue(ReadStream.ReadCheckPoint('MyCheckpoint'), 'Checkpoint with message should pass');
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestCheckPoint_Failure;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteCheckPoint('CorrectMessage');
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.IsFalse(ReadStream.ReadCheckPoint('WrongMessage'), 'Wrong checkpoint message should fail');
  finally
    ReadStream.Free;
  end;
end;


{ Padding Tests }

procedure TTestLightStream.TestPadding0;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteInteger(100);
    WriteStream.WritePadding0(64);
    WriteStream.WriteInteger(200);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(100, ReadStream.ReadInteger);
    ReadStream.ReadPadding0(64);
    Assert.AreEqual(200, ReadStream.ReadInteger);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestPadding0_Zero;
var
  WriteStream, ReadStream: TLightStream;
begin
  { Test that zero-byte padding works }
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteInteger(42);
    WriteStream.WritePadding0(0);  // Zero bytes
    WriteStream.WriteInteger(99);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(42, ReadStream.ReadInteger);
    ReadStream.ReadPadding0(0);
    Assert.AreEqual(99, ReadStream.ReadInteger);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestPadding_WithValidation;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteInteger(100);
    WriteStream.WritePadding(64);
    WriteStream.WriteInteger(200);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(100, ReadStream.ReadInteger);
    { ReadPadding validates the content }
    ReadStream.ReadPadding(64);
    Assert.AreEqual(200, ReadStream.ReadInteger);
  finally
    ReadStream.Free;
  end;
end;


{ Enter Tests }

procedure TTestLightStream.TestEnter;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteEnter;
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.IsTrue(ReadStream.ReadEnter, 'Should read CRLF');
  finally
    ReadStream.Free;
  end;
end;


{ Chars Tests }

procedure TTestLightStream.TestChars;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteChars('Hello');
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual('Hello', ReadStream.ReadChars(5));
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestChars_Empty;
var
  WriteStream, ReadStream: TLightStream;
begin
  { Test that empty WriteChars doesn't crash }
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteInteger(42);
    WriteStream.WriteChars(AnsiString(''));  // Empty - should do nothing
    WriteStream.WriteInteger(99);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(42, ReadStream.ReadInteger);
    Assert.AreEqual(99, ReadStream.ReadInteger);  // Should be immediately after
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestCharsA;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteChars(AnsiString('World'));
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(AnsiString('World'), ReadStream.ReadCharsA(5));
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestCharsA_Empty;
var
  ReadStream: TLightStream;
  WriteStream: TLightStream;
begin
  { Test that ReadCharsA(0) returns empty string }
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteInteger(42);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(AnsiString(''), ReadStream.ReadCharsA(0));
    Assert.AreEqual(42, ReadStream.ReadInteger);  // Position should not have changed
  finally
    ReadStream.Free;
  end;
end;


{ TStringList Tests }

procedure TTestLightStream.TestStrings;
var
  WriteStream, ReadStream: TLightStream;
  WriteTSL, ReadTSL: TStringList;
begin
  WriteTSL:= TStringList.Create;
  ReadTSL:= TStringList.Create;
  try
    WriteTSL.Add('Line 1');
    WriteTSL.Add('Line 2');
    WriteTSL.Add('Line 3');

    WriteStream:= TLightStream.CreateWrite(FTestFile);
    try
      WriteStream.WriteStrings(WriteTSL);
    finally
      WriteStream.Free;
    end;

    ReadStream:= TLightStream.CreateRead(FTestFile);
    try
      ReadStream.ReadStrings(ReadTSL);
      Assert.AreEqual(3, ReadTSL.Count);
      Assert.AreEqual('Line 1', ReadTSL[0]);
      Assert.AreEqual('Line 2', ReadTSL[1]);
      Assert.AreEqual('Line 3', ReadTSL[2]);
    finally
      ReadStream.Free;
    end;
  finally
    WriteTSL.Free;
    ReadTSL.Free;
  end;
end;


procedure TTestLightStream.TestStrings_Empty;
var
  WriteStream, ReadStream: TLightStream;
  WriteTSL, ReadTSL: TStringList;
begin
  WriteTSL:= TStringList.Create;
  ReadTSL:= TStringList.Create;
  try
    WriteStream:= TLightStream.CreateWrite(FTestFile);
    try
      WriteStream.WriteStrings(WriteTSL);
    finally
      WriteStream.Free;
    end;

    ReadStream:= TLightStream.CreateRead(FTestFile);
    try
      ReadStream.ReadStrings(ReadTSL);
      Assert.AreEqual(0, ReadTSL.Count);
    finally
      ReadStream.Free;
    end;
  finally
    WriteTSL.Free;
    ReadTSL.Free;
  end;
end;


procedure TTestLightStream.TestStrings_CreateReturn;
var
  WriteStream, ReadStream: TLightStream;
  WriteTSL, ReadTSL: TStringList;
begin
  { Test the function that creates and returns a TStringList }
  WriteTSL:= TStringList.Create;
  try
    WriteTSL.Add('Alpha');
    WriteTSL.Add('Beta');

    WriteStream:= TLightStream.CreateWrite(FTestFile);
    try
      WriteStream.WriteStrings(WriteTSL);
    finally
      WriteStream.Free;
    end;

    ReadStream:= TLightStream.CreateRead(FTestFile);
    try
      ReadTSL:= ReadStream.ReadStrings;
      try
        Assert.AreEqual(2, ReadTSL.Count);
        Assert.AreEqual('Alpha', ReadTSL[0]);
        Assert.AreEqual('Beta', ReadTSL[1]);
      finally
        ReadTSL.Free;
      end;
    finally
      ReadStream.Free;
    end;
  finally
    WriteTSL.Free;
  end;
end;


{ Bytes Tests }

procedure TTestLightStream.TestPushBytes;
var
  WriteStream, ReadStream: TLightStream;
  WriteBytes: TBytes;
begin
  SetLength(WriteBytes, 5);
  WriteBytes[0]:= 1;
  WriteBytes[1]:= 2;
  WriteBytes[2]:= 3;
  WriteBytes[3]:= 4;
  WriteBytes[4]:= 5;

  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.PushBytes(WriteBytes);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(Byte(1), ReadStream.ReadByte);
    Assert.AreEqual(Byte(2), ReadStream.ReadByte);
    Assert.AreEqual(Byte(3), ReadStream.ReadByte);
    Assert.AreEqual(Byte(4), ReadStream.ReadByte);
    Assert.AreEqual(Byte(5), ReadStream.ReadByte);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestByteChunk;
var
  WriteStream, ReadStream: TLightStream;
  WriteBytes, ReadBytes: TBytes;
begin
  SetLength(WriteBytes, 3);
  WriteBytes[0]:= 10;
  WriteBytes[1]:= 20;
  WriteBytes[2]:= 30;

  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.PushBytesCnt(WriteBytes);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    ReadBytes:= ReadStream.ReadByteChunk;
    Assert.AreEqual(3, Length(ReadBytes));
    Assert.AreEqual(Byte(10), ReadBytes[0]);
    Assert.AreEqual(Byte(20), ReadBytes[1]);
    Assert.AreEqual(Byte(30), ReadBytes[2]);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestByteChunk_Empty;
var
  WriteStream, ReadStream: TLightStream;
  ReadBytes: TBytes;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteCardinal(0);  // Write zero length
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    ReadBytes:= ReadStream.ReadByteChunk;
    Assert.AreEqual(0, Length(ReadBytes));
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestAsBytes;
var
  WriteStream, ReadStream: TLightStream;
  Bytes: TBytes;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteByte(1);
    WriteStream.WriteByte(2);
    WriteStream.WriteByte(3);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Bytes:= ReadStream.AsBytes;
    Assert.AreEqual(3, Length(Bytes));
    Assert.AreEqual(Byte(1), Bytes[0]);
    Assert.AreEqual(Byte(2), Bytes[1]);
    Assert.AreEqual(Byte(3), Bytes[2]);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestAsBytes_Empty;
var
  WriteStream, ReadStream: TLightStream;
  Bytes: TBytes;
begin
  { Create empty file }
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  WriteStream.Free;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Bytes:= ReadStream.AsBytes;
    Assert.AreEqual(0, Length(Bytes));
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestAsString;
var
  WriteStream, ReadStream: TLightStream;
  Content: AnsiString;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.PushAnsi('TestContent');  { Use PushAnsi for raw content - AsString reads raw bytes }
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Content:= ReadStream.AsString;
    Assert.AreEqual(AnsiString('TestContent'), Content);
  finally
    FreeAndNil(ReadStream);
  end;
end;


procedure TTestLightStream.TestPushString;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.PushString('RawData');  // No length prefix
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual('RawData', ReadStream.ReadChars(7));
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestPushAnsi;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.PushAnsi('AnsiData');
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(AnsiString('AnsiData'), ReadStream.ReadCharsA(8));
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestPushAnsi_Empty;
var
  WriteStream, ReadStream: TLightStream;
begin
  { Test that empty PushAnsi doesn't crash }
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteInteger(123);
    WriteStream.PushAnsi('');  // Empty - should do nothing
    WriteStream.WriteInteger(456);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(123, ReadStream.ReadInteger);
    Assert.AreEqual(456, ReadStream.ReadInteger);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestTryReadStringA;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.PushAnsi('HelloWorld');
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(AnsiString('HelloWorld'), ReadStream.TryReadStringA(10));
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestTryReadStringA_Partial;
var
  WriteStream, ReadStream: TLightStream;
  Result: AnsiString;
begin
  { TryReadStringA should read whatever is available without error }
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.PushAnsi('Short');  // 5 bytes
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Result:= ReadStream.TryReadStringA(100);  // Request 100, but only 5 available
    Assert.AreEqual(AnsiString('Short'), Result);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestReadStringCnt;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.PushString('TestString');
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual('TestString', ReadStream.ReadStringCnt(10));
  finally
    ReadStream.Free;
  end;
end;


{ Reverse Read Tests (Motorola format) }

procedure TTestLightStream.TestRevReadWord;
var
  WriteStream, ReadStream: TLightStream;
begin
  { Write bytes in Motorola (big-endian) order }
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteByte($12);  { High byte }
    WriteStream.WriteByte($34);  { Low byte }
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(Word($1234), ReadStream.RevReadWord);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestRevReadCardinal;
var
  WriteStream, ReadStream: TLightStream;
begin
  { Write bytes in Motorola (big-endian) order }
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteByte($12);
    WriteStream.WriteByte($34);
    WriteStream.WriteByte($56);
    WriteStream.WriteByte($78);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(Cardinal($12345678), ReadStream.RevReadCardinal);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestRevReadInteger;
var
  WriteStream, ReadStream: TLightStream;
begin
  { Write bytes in Motorola (big-endian) order }
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteByte($00);
    WriteStream.WriteByte($00);
    WriteStream.WriteByte($00);
    WriteStream.WriteByte($FF);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(Integer($000000FF), ReadStream.RevReadInteger);
  finally
    ReadStream.Free;
  end;
end;


{ Combined Tests }

procedure TTestLightStream.TestMultipleTypes;
var
  WriteStream, ReadStream: TLightStream;
begin
  WriteStream:= TLightStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteBoolean(True);
    WriteStream.WriteInteger(42);
    WriteStream.WriteString('Test');
    WriteStream.WriteDouble(3.14);
  finally
    WriteStream.Free;
  end;

  ReadStream:= TLightStream.CreateRead(FTestFile);
  try
    Assert.IsTrue(ReadStream.ReadBoolean);
    Assert.AreEqual(42, ReadStream.ReadInteger);
    Assert.AreEqual('Test', ReadStream.ReadString);
    Assert.AreEqual(3.14, ReadStream.ReadDouble, 0.001);
  finally
    ReadStream.Free;
  end;
end;


procedure TTestLightStream.TestCompleteFile;
var
  WriteStream, ReadStream: TLightStream;
  TestRect: TRect;
  TSL: TStringList;
begin
  TSL:= TStringList.Create;
  try
    TSL.Add('Item 1');
    TSL.Add('Item 2');

    WriteStream:= TLightStream.CreateWrite(FTestFile);
    try
      WriteStream.WriteHeader('CompleteTest', 1);
      WriteStream.WriteBoolean(True);
      WriteStream.WriteInteger(12345);
      WriteStream.WriteString('Unicode test: ' + Char($00E9));
      WriteStream.WriteDouble(2.71828);
      WriteStream.WriteRect(Rect(1, 2, 3, 4));
      WriteStream.WriteStrings(TSL);
      WriteStream.WriteCheckPoint('End');
      WriteStream.WritePadding(64);
    finally
      WriteStream.Free;
    end;

    TSL.Clear;

    ReadStream:= TLightStream.CreateRead(FTestFile);
    try
      Assert.IsTrue(ReadStream.ReadHeader('CompleteTest', 1));
      Assert.IsTrue(ReadStream.ReadBoolean);
      Assert.AreEqual(12345, ReadStream.ReadInteger);
      Assert.AreEqual('Unicode test: ' + Char($00E9), ReadStream.ReadString);
      Assert.AreEqual(2.71828, ReadStream.ReadDouble, 0.00001);

      TestRect:= ReadStream.ReadRect;
      Assert.AreEqual(1, TestRect.Left);
      Assert.AreEqual(2, TestRect.Top);
      Assert.AreEqual(3, TestRect.Right);
      Assert.AreEqual(4, TestRect.Bottom);

      ReadStream.ReadStrings(TSL);
      Assert.AreEqual(2, TSL.Count);
      Assert.AreEqual('Item 1', TSL[0]);
      Assert.AreEqual('Item 2', TSL[1]);

      Assert.IsTrue(ReadStream.ReadCheckPoint('End'));
      ReadStream.ReadPadding(64);
    finally
      ReadStream.Free;
    end;
  finally
    TSL.Free;
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestLightStream);

end.
