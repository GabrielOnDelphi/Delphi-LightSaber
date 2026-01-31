unit Test.LightCore.StreamMem;

{=============================================================================================================
   Unit tests for LightCore.StreamMem
   Tests TCubicMemStream for reading/writing various data types to memory streams
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.Types,
  LightCore.StreamMem;

type
  [TestFixture]
  TTestStreamMem = class
  private
    FStream: TCubicMemStream;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor Tests }
    [Test]
    procedure TestCreateFromStream;

    [Test]
    procedure TestCreateFromStreamPos;

    { Header Tests }
    [Test]
    procedure TestWriteReadHeader_Match;

    [Test]
    procedure TestWriteReadHeader_Mismatch;

    [Test]
    procedure TestWriteReadHeader_Version;

    { Checkpoint Tests }
    [Test]
    procedure TestWriteReadCheckPoint;

    [Test]
    procedure TestWriteReadCheckPoint_WithString;

    { Enter Tests }
    [Test]
    procedure TestWriteReadEnter;

    { Padding Tests }
    [Test]
    procedure TestWriteReadPadding0;

    [Test]
    procedure TestWriteReadPadding;

    { Boolean Tests }
    [Test]
    procedure TestWriteReadBoolean_True;

    [Test]
    procedure TestWriteReadBoolean_False;

    { Byte Tests }
    [Test]
    procedure TestWriteReadByte;

    { ShortInt Tests }
    [Test]
    procedure TestWriteReadShortInt;

    { SmallInt Tests }
    [Test]
    procedure TestWriteReadSmallInt;

    { Word Tests }
    [Test]
    procedure TestWriteReadWord;

    { Cardinal Tests }
    [Test]
    procedure TestWriteReadCardinal;

    { Integer Tests }
    [Test]
    procedure TestWriteReadInteger;

    [Test]
    procedure TestWriteReadInteger_Negative;

    { Int64 Tests }
    [Test]
    procedure TestWriteReadInt64;

    { UInt64 Tests }
    [Test]
    procedure TestWriteReadUInt64;

    { Single Tests }
    [Test]
    procedure TestWriteReadSingle;

    { Double Tests }
    [Test]
    procedure TestWriteReadDouble;

    { Date Tests }
    [Test]
    procedure TestWriteReadDate;

    { Complex Structure Tests }
    [Test]
    procedure TestWriteReadRect;

    [Test]
    procedure TestWriteReadRectF;

    [Test]
    procedure TestWriteReadIntegers;

    [Test]
    procedure TestWriteReadDoubles;

    { Unicode String Tests }
    [Test]
    procedure TestWriteReadString_ASCII;

    [Test]
    procedure TestWriteReadString_Unicode;

    [Test]
    procedure TestWriteReadString_Empty;

    { AnsiString Tests }
    [Test]
    procedure TestWriteReadStringA;

    [Test]
    procedure TestWriteReadStringA_Empty;

    [Test]
    procedure TestTryReadStringA;

    { Chars Tests }
    [Test]
    procedure TestWriteReadChars_Ansi;

    [Test]
    procedure TestWriteReadChars_Unicode;

    [Test]
    procedure TestWriteReadChar;

    { StringList Tests }
    [Test]
    procedure TestWriteReadStrings;

    [Test]
    procedure TestReadStrings_ReturnsNewList;

    { Reverse Read Tests }
    [Test]
    procedure TestRevReadCardinal;

    [Test]
    procedure TestRevReadInteger;

    [Test]
    procedure TestRevReadSmallInt;

    [Test]
    procedure TestRevReadWord;

    { Raw Data Tests }
    [Test]
    procedure TestAsBytes;

    [Test]
    procedure TestAsBytes_Empty;

    [Test]
    procedure TestAsString;

    [Test]
    procedure TestPushData_AnsiString;

    [Test]
    procedure TestPushData_Bytes;

    [Test]
    procedure TestPushData_Empty;

    [Test]
    procedure TestPushAnsi;

    [Test]
    procedure TestPushBytes;

    [Test]
    procedure TestPushString;

    { ByteChunk Tests }
    [Test]
    procedure TestPushBytesCnt_ReadByteChunk;

    [Test]
    procedure TestReadByteChunk_Empty;

    { ReadStringCnt Tests }
    [Test]
    procedure TestReadStringCnt;

    { Utility Function Tests }
    [Test]
    procedure TestStringFromStream;

    [Test]
    procedure TestStringFromStream_Empty;

    [Test]
    procedure TestStringFromStream_WithPos;
  end;

implementation

uses
  LightCore.Types;

procedure TTestStreamMem.Setup;
begin
  FStream:= TCubicMemStream.Create;
  FStream.StringListSafetyLimit:= 1024 * 1024;  { 1 MB - required for ReadStrings to work }
end;

procedure TTestStreamMem.TearDown;
begin
  FreeAndNil(FStream);
end;

{ Constructor Tests }

procedure TTestStreamMem.TestCreateFromStream;
var
  SourceStream: TMemoryStream;
  NewStream: TCubicMemStream;
  TestData: AnsiString;
begin
  TestData:= 'Test Data';
  SourceStream:= TMemoryStream.Create;
  try
    SourceStream.WriteBuffer(TestData[1], Length(TestData));
    SourceStream.Position:= 0;

    NewStream:= TCubicMemStream.CreateFromStream(SourceStream);
    try
      Assert.AreEqual(Int64(Length(TestData)), NewStream.Size, 'Size should match');
      Assert.AreEqual(Int64(0), NewStream.Position, 'Position should be 0');
    finally
      FreeAndNil(NewStream);
    end;
  finally
    FreeAndNil(SourceStream);
  end;
end;

procedure TTestStreamMem.TestCreateFromStreamPos;
var
  SourceStream: TMemoryStream;
  NewStream: TCubicMemStream;
  TestData: AnsiString;
begin
  TestData:= 'ABCDEFGHIJ';
  SourceStream:= TMemoryStream.Create;
  try
    SourceStream.WriteBuffer(TestData[1], Length(TestData));
    SourceStream.Position:= 5;  { Start from middle }

    NewStream:= TCubicMemStream.CreateFromStream(SourceStream, SourceStream.Position);
    try
      Assert.AreEqual(Int64(5), NewStream.Size, 'Should copy remaining 5 bytes');
      Assert.AreEqual(Int64(0), NewStream.Position, 'Position should be 0');
    finally
      FreeAndNil(NewStream);
    end;
  finally
    FreeAndNil(SourceStream);
  end;
end;

{ Header Tests }

procedure TTestStreamMem.TestWriteReadHeader_Match;
begin
  FStream.WriteHeader('TestSignature', 1);
  FStream.Position:= 0;
  Assert.IsTrue(FStream.ReadHeader('TestSignature', 1), 'Header should match');
end;

procedure TTestStreamMem.TestWriteReadHeader_Mismatch;
begin
  FStream.WriteHeader('SignatureA', 1);
  FStream.Position:= 0;
  Assert.IsFalse(FStream.ReadHeader('SignatureB', 1), 'Header should not match different signature');
end;

procedure TTestStreamMem.TestWriteReadHeader_Version;
var
  Version: Word;
begin
  FStream.WriteHeader('MyApp', 5);
  FStream.Position:= 0;
  Version:= FStream.ReadHeader('MyApp');
  Assert.AreEqual(Word(5), Version, 'Should read correct version');
end;

{ Checkpoint Tests }

procedure TTestStreamMem.TestWriteReadCheckPoint;
begin
  FStream.WriteCheckPoint;
  FStream.Position:= 0;
  Assert.IsTrue(FStream.ReadCheckPoint, 'Checkpoint should match');
end;

procedure TTestStreamMem.TestWriteReadCheckPoint_WithString;
begin
  FStream.WriteCheckPoint('MyMarker');
  FStream.Position:= 0;
  Assert.IsTrue(FStream.ReadCheckPoint('MyMarker'), 'Checkpoint with string should match');
end;

{ Enter Tests }

procedure TTestStreamMem.TestWriteReadEnter;
begin
  FStream.WriteEnter;
  FStream.Position:= 0;
  Assert.IsTrue(FStream.ReadEnter, 'Should read CRLF correctly');
end;

{ Padding Tests }

procedure TTestStreamMem.TestWriteReadPadding0;
var
  PaddingSize: Integer;
begin
  PaddingSize:= 64;
  FStream.WritePadding0(PaddingSize);
  Assert.AreEqual(Int64(PaddingSize), FStream.Size, 'Size should equal padding size');

  FStream.Position:= 0;
  FStream.ReadPadding0(PaddingSize);
  Assert.AreEqual(Int64(PaddingSize), FStream.Position, 'Position should be at end');
end;

procedure TTestStreamMem.TestWriteReadPadding;
var
  PaddingSize: Integer;
begin
  PaddingSize:= 64;
  FStream.WritePadding(PaddingSize);
  Assert.AreEqual(Int64(PaddingSize), FStream.Size, 'Size should equal padding size');

  FStream.Position:= 0;
  FStream.ReadPadding(PaddingSize);  // Should not raise exception
  Assert.AreEqual(Int64(PaddingSize), FStream.Position, 'Position should be at end');
end;

{ Boolean Tests }

procedure TTestStreamMem.TestWriteReadBoolean_True;
begin
  FStream.WriteBoolean(True);
  FStream.Position:= 0;
  Assert.IsTrue(FStream.ReadBoolean, 'Should read True');
end;

procedure TTestStreamMem.TestWriteReadBoolean_False;
begin
  FStream.WriteBoolean(False);
  FStream.Position:= 0;
  Assert.IsFalse(FStream.ReadBoolean, 'Should read False');
end;

{ Byte Tests }

procedure TTestStreamMem.TestWriteReadByte;
var
  Original, ReadBack: Byte;
begin
  Original:= 255;
  FStream.WriteByte(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.ReadByte;
  Assert.AreEqual(Original, ReadBack, 'Byte should round-trip correctly');
end;

{ ShortInt Tests }

procedure TTestStreamMem.TestWriteReadShortInt;
var
  Original, ReadBack: ShortInt;
begin
  Original:= -128;
  FStream.WriteShortInt(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.ReadShortInt;
  Assert.AreEqual(Original, ReadBack, 'ShortInt should round-trip correctly');
end;

{ SmallInt Tests }

procedure TTestStreamMem.TestWriteReadSmallInt;
var
  Original, ReadBack: SmallInt;
begin
  Original:= -32768;
  FStream.WriteSmallInt(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.ReadSmallInt;
  Assert.AreEqual(Original, ReadBack, 'SmallInt should round-trip correctly');
end;

{ Word Tests }

procedure TTestStreamMem.TestWriteReadWord;
var
  Original, ReadBack: Word;
begin
  Original:= 65535;
  FStream.WriteWord(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.ReadWord;
  Assert.AreEqual(Original, ReadBack, 'Word should round-trip correctly');
end;

{ Cardinal Tests }

procedure TTestStreamMem.TestWriteReadCardinal;
var
  Original, ReadBack: Cardinal;
begin
  Original:= $FFFFFFFF;
  FStream.WriteCardinal(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.ReadCardinal;
  Assert.AreEqual(Original, ReadBack, 'Cardinal should round-trip correctly');
end;

{ Integer Tests }

procedure TTestStreamMem.TestWriteReadInteger;
var
  Original, ReadBack: Integer;
begin
  Original:= MaxInt;
  FStream.WriteInteger(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.ReadInteger;
  Assert.AreEqual(Original, ReadBack, 'Integer should round-trip correctly');
end;

procedure TTestStreamMem.TestWriteReadInteger_Negative;
var
  Original, ReadBack: Integer;
begin
  Original:= -123456789;
  FStream.WriteInteger(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.ReadInteger;
  Assert.AreEqual(Original, ReadBack, 'Negative Integer should round-trip correctly');
end;

{ Int64 Tests }

procedure TTestStreamMem.TestWriteReadInt64;
var
  Original, ReadBack: Int64;
begin
  Original:= Int64($7FFFFFFFFFFFFFFF);
  FStream.WriteInt64(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.ReadInt64;
  Assert.AreEqual(Original, ReadBack, 'Int64 should round-trip correctly');
end;

{ UInt64 Tests }

procedure TTestStreamMem.TestWriteReadUInt64;
var
  Original, ReadBack: UInt64;
begin
  Original:= UInt64($FFFFFFFFFFFFFFFF);
  FStream.WriteUInt64(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.ReadUInt64;
  Assert.AreEqual(Original, ReadBack, 'UInt64 should round-trip correctly');
end;

{ Single Tests }

procedure TTestStreamMem.TestWriteReadSingle;
var
  Original, ReadBack: Single;
begin
  Original:= 3.14159;
  FStream.WriteSingle(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.ReadSingle;
  Assert.AreEqual(Original, ReadBack, 0.00001, 'Single should round-trip correctly');
end;

{ Double Tests }

procedure TTestStreamMem.TestWriteReadDouble;
var
  Original, ReadBack: Double;
begin
  Original:= 3.141592653589793;
  FStream.WriteDouble(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.ReadDouble;
  Assert.AreEqual(Original, ReadBack, 0.0000000000001, 'Double should round-trip correctly');
end;

{ Date Tests }

procedure TTestStreamMem.TestWriteReadDate;
var
  Original, ReadBack: TDateTime;
begin
  Original:= Now;
  FStream.WriteDate(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.ReadDate;
  Assert.AreEqual(Original, ReadBack, 0.0001, 'Date should round-trip correctly');
end;

{ Complex Structure Tests }

procedure TTestStreamMem.TestWriteReadRect;
var
  Original, ReadBack: TRect;
begin
  Original:= Rect(10, 20, 100, 200);
  FStream.WriteRect(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.ReadRect;

  Assert.AreEqual(Original.Left, ReadBack.Left, 'Left should match');
  Assert.AreEqual(Original.Top, ReadBack.Top, 'Top should match');
  Assert.AreEqual(Original.Right, ReadBack.Right, 'Right should match');
  Assert.AreEqual(Original.Bottom, ReadBack.Bottom, 'Bottom should match');
end;

procedure TTestStreamMem.TestWriteReadRectF;
var
  Original, ReadBack: TRectF;
begin
  Original:= TRectF.Create(10.5, 20.5, 100.5, 200.5);
  FStream.WriteRectF(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.ReadRectF;

  Assert.AreEqual(Original.Left, ReadBack.Left, 0.001, 'Left should match');
  Assert.AreEqual(Original.Top, ReadBack.Top, 0.001, 'Top should match');
  Assert.AreEqual(Original.Right, ReadBack.Right, 0.001, 'Right should match');
  Assert.AreEqual(Original.Bottom, ReadBack.Bottom, 0.001, 'Bottom should match');
end;

procedure TTestStreamMem.TestWriteReadIntegers;
var
  Original, ReadBack: TIntegerArray;
  i: Integer;
begin
  SetLength(Original, 5);
  for i:= 0 to High(Original) do
    Original[i]:= i * 100;

  FStream.WriteIntegers(Original);
  FStream.Position:= 0;
  FStream.ReadIntegers(ReadBack);

  Assert.AreEqual(Length(Original), Length(ReadBack), 'Array length should match');
  for i:= 0 to High(Original) do
    Assert.AreEqual(Original[i], ReadBack[i], Format('Element %d should match', [i]));
end;

procedure TTestStreamMem.TestWriteReadDoubles;
var
  Original, ReadBack: TDoubleArray;
  i: Integer;
begin
  SetLength(Original, 5);
  for i:= 0 to High(Original) do
    Original[i]:= i * 1.5;

  FStream.WriteDoubles(Original);
  FStream.Position:= 0;
  FStream.ReadDoubles(ReadBack);

  Assert.AreEqual(Length(Original), Length(ReadBack), 'Array length should match');
  for i:= 0 to High(Original) do
    Assert.AreEqual(Original[i], ReadBack[i], 0.0001, Format('Element %d should match', [i]));
end;

{ Unicode String Tests }

procedure TTestStreamMem.TestWriteReadString_ASCII;
var
  Original, ReadBack: string;
begin
  Original:= 'Hello World!';
  FStream.WriteString(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.ReadString;
  Assert.AreEqual(Original, ReadBack, 'ASCII string should round-trip correctly');
end;

procedure TTestStreamMem.TestWriteReadString_Unicode;
var
  Original, ReadBack: string;
begin
  Original:= 'Unicode: äöü ß 中文 日本語';
  FStream.WriteString(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.ReadString;
  Assert.AreEqual(Original, ReadBack, 'Unicode string should round-trip correctly');
end;

procedure TTestStreamMem.TestWriteReadString_Empty;
var
  Original, ReadBack: string;
begin
  Original:= '';
  FStream.WriteString(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.ReadString;
  Assert.AreEqual(Original, ReadBack, 'Empty string should round-trip correctly');
end;

{ AnsiString Tests }

procedure TTestStreamMem.TestWriteReadStringA;
var
  Original, ReadBack: AnsiString;
begin
  Original:= 'Test ANSI String';
  FStream.WriteStringA(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.ReadStringA;
  Assert.AreEqual(Original, ReadBack, 'AnsiString should round-trip correctly');
end;

procedure TTestStreamMem.TestWriteReadStringA_Empty;
var
  Original, ReadBack: AnsiString;
begin
  Original:= '';
  FStream.WriteStringA(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.ReadStringA;
  Assert.AreEqual(Original, ReadBack, 'Empty AnsiString should round-trip correctly');
end;

procedure TTestStreamMem.TestTryReadStringA;
var
  Original, ReadBack: AnsiString;
begin
  Original:= 'ABCDEFGHIJ';
  FStream.WriteChars(Original);  { Write without length prefix }
  FStream.Position:= 0;
  ReadBack:= FStream.TryReadStringA(5);  { Read only 5 chars }
  Assert.AreEqual(AnsiString('ABCDE'), ReadBack, 'Should read exactly 5 characters');
end;

{ Chars Tests }

procedure TTestStreamMem.TestWriteReadChars_Ansi;
var
  Original, ReadBack: AnsiString;
begin
  Original:= 'TestChars';
  FStream.WriteChars(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.ReadCharsA(Length(Original));
  Assert.AreEqual(Original, ReadBack, 'Ansi chars should round-trip correctly');
end;

procedure TTestStreamMem.TestWriteReadChars_Unicode;
var
  Original, ReadBack: string;
begin
  Original:= 'TestChars';
  FStream.WriteChars(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.ReadChars(Length(Original));
  Assert.AreEqual(Original, ReadBack, 'Unicode chars should round-trip correctly');
end;

procedure TTestStreamMem.TestWriteReadChar;
var
  Original, ReadBack: AnsiChar;
begin
  Original:= 'X';
  FStream.WriteChar(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.ReadChar;
  Assert.AreEqual(Original, ReadBack, 'Char should round-trip correctly');
end;

{ StringList Tests }

procedure TTestStreamMem.TestWriteReadStrings;
var
  Original, ReadBack: TStringList;
begin
  Original:= TStringList.Create;
  ReadBack:= TStringList.Create;
  try
    Original.Add('Line 1');
    Original.Add('Line 2');
    Original.Add('Line 3');

    FStream.WriteStrings(Original);
    FStream.Position:= 0;
    FStream.ReadStrings(ReadBack);

    Assert.AreEqual(Original.Text, ReadBack.Text, 'StringList should round-trip correctly');
  finally
    FreeAndNil(Original);
    FreeAndNil(ReadBack);
  end;
end;

procedure TTestStreamMem.TestReadStrings_ReturnsNewList;
var
  Original, ReadBack: TStringList;
begin
  Original:= TStringList.Create;
  try
    Original.Add('Test');
    FStream.WriteStrings(Original);
    FStream.Position:= 0;

    ReadBack:= FStream.ReadStrings;
    try
      Assert.IsNotNull(ReadBack, 'Should return a new TStringList');
      Assert.AreEqual(Original.Text, ReadBack.Text, 'Content should match');
    finally
      FreeAndNil(ReadBack);
    end;
  finally
    FreeAndNil(Original);
  end;
end;

{ Reverse Read Tests }

procedure TTestStreamMem.TestRevReadCardinal;
var
  Original: Cardinal;
  ReadBack: Cardinal;
begin
  { Write bytes in reverse order to test swap }
  Original:= $12345678;
  FStream.WriteCardinal(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.RevReadCardinal;
  Assert.AreEqual(Cardinal($78563412), ReadBack, 'Should swap bytes correctly');
end;

procedure TTestStreamMem.TestRevReadInteger;
var
  Original: Cardinal;
  ReadBack: Integer;
begin
  Original:= $12345678;
  FStream.WriteCardinal(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.RevReadInteger;
  Assert.AreEqual(Integer($78563412), ReadBack, 'Should swap bytes correctly');
end;

procedure TTestStreamMem.TestRevReadSmallInt;
var
  Original: Word;
  ReadBack: SmallInt;
begin
  Original:= $1234;
  FStream.WriteWord(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.RevReadSmallInt;
  Assert.AreEqual(SmallInt($3412), ReadBack, 'Should swap bytes correctly');
end;

procedure TTestStreamMem.TestRevReadWord;
var
  Original: Word;
  ReadBack: Word;
begin
  Original:= $1234;
  FStream.WriteWord(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.RevReadWord;
  Assert.AreEqual(Word($3412), ReadBack, 'Should swap bytes correctly');
end;

{ Raw Data Tests }

procedure TTestStreamMem.TestAsBytes;
var
  TestData: AnsiString;
  Bytes: TBytes;
begin
  TestData:= 'TestData';
  FStream.WriteChars(TestData);
  Bytes:= FStream.AsBytes;
  Assert.AreEqual(Length(TestData), Length(Bytes), 'Length should match');
end;

procedure TTestStreamMem.TestAsBytes_Empty;
var
  Bytes: TBytes;
begin
  Bytes:= FStream.AsBytes;
  Assert.AreEqual(0, Length(Bytes), 'Empty stream should return empty bytes');
end;

procedure TTestStreamMem.TestAsString;
var
  TestData, ReadBack: AnsiString;
begin
  TestData:= 'TestString';
  FStream.WriteChars(TestData);
  ReadBack:= FStream.AsString;
  Assert.AreEqual(TestData, ReadBack, 'AsString should return stream content');
end;

procedure TTestStreamMem.TestPushData_AnsiString;
var
  TestData, ReadBack: AnsiString;
begin
  TestData:= 'PushTest';
  FStream.PushData(TestData);
  ReadBack:= FStream.AsString;
  Assert.AreEqual(TestData, ReadBack, 'PushData should write string to stream');
end;

procedure TTestStreamMem.TestPushData_Bytes;
var
  TestBytes, ReadBytes: TBytes;
begin
  SetLength(TestBytes, 5);
  TestBytes[0]:= 1;
  TestBytes[1]:= 2;
  TestBytes[2]:= 3;
  TestBytes[3]:= 4;
  TestBytes[4]:= 5;

  FStream.PushData(TestBytes);
  ReadBytes:= FStream.AsBytes;

  Assert.AreEqual(Length(TestBytes), Length(ReadBytes), 'Length should match');
  Assert.AreEqual(TestBytes[0], ReadBytes[0], 'First byte should match');
  Assert.AreEqual(TestBytes[4], ReadBytes[4], 'Last byte should match');
end;

procedure TTestStreamMem.TestPushData_Empty;
begin
  FStream.PushData(AnsiString(''));
  Assert.AreEqual(Int64(0), FStream.Size, 'Empty push should result in empty stream');
end;

procedure TTestStreamMem.TestPushAnsi;
var
  TestData, ReadBack: AnsiString;
begin
  TestData:= 'PushAnsiTest';
  FStream.PushAnsi(TestData);
  ReadBack:= FStream.AsString;
  Assert.AreEqual(TestData, ReadBack, 'PushAnsi should write string without length prefix');
end;

procedure TTestStreamMem.TestPushBytes;
var
  TestBytes, ReadBytes: TBytes;
  i: Integer;
begin
  SetLength(TestBytes, 5);
  for i:= 0 to High(TestBytes) do
    TestBytes[i]:= i + 1;

  FStream.PushBytes(TestBytes);
  ReadBytes:= FStream.AsBytes;

  Assert.AreEqual(Length(TestBytes), Length(ReadBytes), 'Length should match');
  for i:= 0 to High(TestBytes) do
    Assert.AreEqual(TestBytes[i], ReadBytes[i], Format('Byte %d should match', [i]));
end;

procedure TTestStreamMem.TestPushString;
var
  Original, ReadBack: string;
begin
  Original:= 'PushStringTest';
  FStream.PushString(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.ReadChars(Length(Original));
  Assert.AreEqual(Original, ReadBack, 'PushString should write without length prefix');
end;

{ ByteChunk Tests }

procedure TTestStreamMem.TestPushBytesCnt_ReadByteChunk;
var
  Original, ReadBack: TBytes;
  i: Integer;
begin
  SetLength(Original, 10);
  for i:= 0 to High(Original) do
    Original[i]:= i;

  FStream.PushBytesCnt(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.ReadByteChunk;

  Assert.AreEqual(Length(Original), Length(ReadBack), 'Length should match');
  for i:= 0 to High(Original) do
    Assert.AreEqual(Original[i], ReadBack[i], Format('Byte %d should match', [i]));
end;

procedure TTestStreamMem.TestReadByteChunk_Empty;
var
  ReadBack: TBytes;
begin
  FStream.WriteCardinal(0);  // Write count of 0
  FStream.Position:= 0;
  ReadBack:= FStream.ReadByteChunk;
  Assert.AreEqual(0, Length(ReadBack), 'Empty chunk should return empty array');
end;

{ ReadStringCnt Tests }

procedure TTestStreamMem.TestReadStringCnt;
var
  Original, ReadBack: string;
begin
  Original:= 'ABCDEFGHIJ';
  FStream.PushString(Original);
  FStream.Position:= 0;
  ReadBack:= FStream.ReadStringCnt(5);
  Assert.AreEqual('ABCDE', ReadBack, 'Should read exactly 5 characters');
end;

{ Utility Function Tests }

procedure TTestStreamMem.TestStringFromStream;
var
  TestData: AnsiString;
  Result: string;
  MemStream: TMemoryStream;
begin
  TestData:= 'Hello World';
  MemStream:= TMemoryStream.Create;
  try
    MemStream.WriteBuffer(TestData[1], Length(TestData));
    Result:= StringFromStream(MemStream);
    Assert.AreEqual(string(TestData), Result, 'Should read entire stream');
  finally
    FreeAndNil(MemStream);
  end;
end;

procedure TTestStreamMem.TestStringFromStream_Empty;
var
  Result: string;
  MemStream: TMemoryStream;
begin
  MemStream:= TMemoryStream.Create;
  try
    Result:= StringFromStream(MemStream);
    Assert.AreEqual('', Result, 'Empty stream should return empty string');
  finally
    FreeAndNil(MemStream);
  end;
end;

procedure TTestStreamMem.TestStringFromStream_WithPos;
var
  TestData: AnsiString;
  Result: string;
  MemStream: TMemoryStream;
begin
  TestData:= 'Hello World';
  MemStream:= TMemoryStream.Create;
  try
    MemStream.WriteBuffer(TestData[1], Length(TestData));
    Result:= StringFromStream(MemStream, 5, 6);  { Read 'World' }
    Assert.AreEqual('World', Result, 'Should read from specified position');
  finally
    FreeAndNil(MemStream);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestStreamMem);

end.
