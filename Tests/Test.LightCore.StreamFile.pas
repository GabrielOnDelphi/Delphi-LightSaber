unit Test.LightCore.StreamFile;

{=============================================================================================================
   Unit tests for LightCore.StreamFile
   Tests TCubicFileStream for reading/writing various data types
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.IOUtils,
  LightCore.StreamFile;

type
  [TestFixture]
  TTestStreamFile = class
  private
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
    procedure TestWriteReadHeader_Match;

    [Test]
    procedure TestWriteReadHeader_Mismatch;

    [Test]
    procedure TestWriteReadHeader_Version;

    { CheckPoint Tests }
    [Test]
    procedure TestCheckPoint;

    [Test]
    procedure TestCheckPoint_WithString;

    { Enter Tests }
    [Test]
    procedure TestWriteReadEnter;

    { Padding Tests }
    [Test]
    procedure TestWriteReadPadding0;

    [Test]
    procedure TestWriteReadPadding;

    { String Tests }
    [Test]
    procedure TestWriteReadString;

    [Test]
    procedure TestWriteReadStringA;

    [Test]
    procedure TestWriteReadStrings;

    [Test]
    procedure TestWriteReadChars;

    [Test]
    procedure TestPushString;

    [Test]
    procedure TestReadStringCnt;

    { Numeric Tests }
    [Test]
    procedure TestWriteReadBoolean;

    [Test]
    procedure TestWriteReadByte;

    [Test]
    procedure TestWriteReadShortInt;

    [Test]
    procedure TestWriteReadSmallInt;

    [Test]
    procedure TestWriteReadWord;

    [Test]
    procedure TestWriteReadCardinal;

    [Test]
    procedure TestWriteReadInteger;

    [Test]
    procedure TestWriteReadInt64;

    [Test]
    procedure TestWriteReadUInt64;

    [Test]
    procedure TestWriteReadSingle;

    [Test]
    procedure TestWriteReadDouble;

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

    { Reverse Read Tests }
    [Test]
    procedure TestRevReadCardinal;

    [Test]
    procedure TestRevReadInteger;

    [Test]
    procedure TestRevReadWord;

    { Raw Data Tests }
    [Test]
    procedure TestAsString;

    [Test]
    procedure TestAsBytes;

    [Test]
    procedure TestPushBytes;

    [Test]
    procedure TestPushBytesCnt_ReadByteChunk;
  end;

implementation

uses
  LightCore.Types;

procedure TTestStreamFile.Setup;
begin
  FTestFile:= TPath.GetTempFileName;
end;

procedure TTestStreamFile.TearDown;
begin
  CleanupTestFile;
end;

procedure TTestStreamFile.CleanupTestFile;
begin
  if FileExists(FTestFile)
  then DeleteFile(FTestFile);
end;


{ Constructor Tests }

procedure TTestStreamFile.TestCreateRead;
var
  WriteStream, ReadStream: TCubicFileStream;
begin
  // First create a file
  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteInteger(12345);
  finally
    FreeAndNil(WriteStream);
  end;

  // Now test CreateRead
  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(12345, ReadStream.ReadInteger, 'CreateRead should open file for reading');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestCreateWrite;
var
  WriteStream, ReadStream: TCubicFileStream;
begin
  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteInteger(67890);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(67890, ReadStream.ReadInteger, 'CreateWrite should create file for writing');
  finally
    FreeAndNil(ReadStream);
  end;
end;


{ Header Tests }

procedure TTestStreamFile.TestWriteReadHeader_Match;
var
  WriteStream, ReadStream: TCubicFileStream;
begin
  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteHeader('TestSignature', 1);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    Assert.IsTrue(ReadStream.ReadHeader('TestSignature', 1), 'Header should match');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestWriteReadHeader_Mismatch;
var
  WriteStream, ReadStream: TCubicFileStream;
begin
  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteHeader('SignatureA', 1);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    Assert.IsFalse(ReadStream.ReadHeader('SignatureB', 1), 'Header should not match different signature');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestWriteReadHeader_Version;
var
  WriteStream, ReadStream: TCubicFileStream;
  Version: Word;
begin
  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteHeader('MyApp', 5);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    Version:= ReadStream.ReadHeader('MyApp');
    Assert.AreEqual(Word(5), Version, 'Should read correct version');
  finally
    FreeAndNil(ReadStream);
  end;
end;


{ CheckPoint Tests }

procedure TTestStreamFile.TestCheckPoint;
var
  WriteStream, ReadStream: TCubicFileStream;
begin
  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteCheckPoint;
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    Assert.IsTrue(ReadStream.ReadCheckPoint, 'Checkpoint should match');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestCheckPoint_WithString;
var
  WriteStream, ReadStream: TCubicFileStream;
begin
  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteCheckPoint('MyMarker');
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    Assert.IsTrue(ReadStream.ReadCheckPoint('MyMarker'), 'Checkpoint with string should match');
  finally
    FreeAndNil(ReadStream);
  end;
end;


{ Enter Tests }

procedure TTestStreamFile.TestWriteReadEnter;
var
  WriteStream, ReadStream: TCubicFileStream;
begin
  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteEnter;
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    Assert.IsTrue(ReadStream.ReadEnter, 'Should read CRLF correctly');
  finally
    FreeAndNil(ReadStream);
  end;
end;


{ Padding Tests }

procedure TTestStreamFile.TestWriteReadPadding0;
var
  WriteStream, ReadStream: TCubicFileStream;
  PaddingSize: Integer;
begin
  PaddingSize:= 64;

  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WritePadding0(PaddingSize);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(Int64(PaddingSize), ReadStream.Size, 'File size should equal padding size');
    ReadStream.ReadPadding0(PaddingSize);
    Assert.AreEqual(Int64(PaddingSize), ReadStream.Position, 'Position should be at end');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestWriteReadPadding;
var
  WriteStream, ReadStream: TCubicFileStream;
  PaddingSize: Integer;
begin
  PaddingSize:= 64;

  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WritePadding(PaddingSize);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    ReadStream.ReadPadding(PaddingSize);  // Should not raise exception
    Assert.AreEqual(Int64(PaddingSize), ReadStream.Position, 'Position should be at end');
  finally
    FreeAndNil(ReadStream);
  end;
end;


{ String Tests }

procedure TTestStreamFile.TestWriteReadString;
var
  WriteStream, ReadStream: TCubicFileStream;
  Original, ReadBack: string;
begin
  Original:= 'Test Unicode String: äöü ß 中文';

  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteString(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadString;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Original, ReadBack, 'Unicode string should round-trip correctly');
end;

procedure TTestStreamFile.TestWriteReadStringA;
var
  WriteStream, ReadStream: TCubicFileStream;
  Original, ReadBack: AnsiString;
begin
  Original:= 'Test ANSI String 123';

  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteStringA(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadStringA;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Original, ReadBack, 'ANSI string should round-trip correctly');
end;

procedure TTestStreamFile.TestWriteReadStrings;
var
  WriteStream, ReadStream: TCubicFileStream;
  Original, ReadBack: TStringList;
begin
  Original:= TStringList.Create;
  ReadBack:= TStringList.Create;
  try
    Original.Add('Line 1');
    Original.Add('Line 2');
    Original.Add('Line 3');

    WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
    try
      WriteStream.WriteStrings(Original);
    finally
      FreeAndNil(WriteStream);
    end;

    ReadStream:= TCubicFileStream.CreateRead(FTestFile);
    try
      ReadStream.ReadStrings(ReadBack);
    finally
      FreeAndNil(ReadStream);
    end;

    Assert.AreEqual(Original.Text, ReadBack.Text, 'StringList should round-trip correctly');
  finally
    FreeAndNil(Original);
    FreeAndNil(ReadBack);
  end;
end;

procedure TTestStreamFile.TestWriteReadChars;
var
  WriteStream, ReadStream: TCubicFileStream;
  Original, ReadBack: string;
begin
  Original:= 'TestChars';

  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteChars(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadChars(Length(Original));
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Original, ReadBack, 'Chars should round-trip correctly');
end;

procedure TTestStreamFile.TestPushString;
var
  WriteStream, ReadStream: TCubicFileStream;
  Original, ReadBack: string;
begin
  Original:= 'PushStringTest';

  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.PushString(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadChars(Length(Original));
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Original, ReadBack, 'PushString should write without length prefix');
end;

procedure TTestStreamFile.TestReadStringCnt;
var
  WriteStream, ReadStream: TCubicFileStream;
  Original, ReadBack: string;
begin
  Original:= 'ABCDEFGHIJ';

  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.PushString(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadStringCnt(5);
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual('ABCDE', ReadBack, 'Should read exactly 5 characters');
end;


{ Numeric Tests }

procedure TTestStreamFile.TestWriteReadBoolean;
var
  WriteStream, ReadStream: TCubicFileStream;
begin
  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteBoolean(True);
    WriteStream.WriteBoolean(False);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    Assert.IsTrue(ReadStream.ReadBoolean, 'True should round-trip correctly');
    Assert.IsFalse(ReadStream.ReadBoolean, 'False should round-trip correctly');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestWriteReadByte;
var
  WriteStream, ReadStream: TCubicFileStream;
begin
  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteByte(255);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(Byte(255), ReadStream.ReadByte, 'Byte should round-trip correctly');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestWriteReadShortInt;
var
  WriteStream, ReadStream: TCubicFileStream;
begin
  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteShortInt(-128);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(ShortInt(-128), ReadStream.ReadShortInt, 'ShortInt should round-trip correctly');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestWriteReadSmallInt;
var
  WriteStream, ReadStream: TCubicFileStream;
begin
  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteSmallInt(-32768);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(SmallInt(-32768), ReadStream.ReadSmallInt, 'SmallInt should round-trip correctly');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestWriteReadWord;
var
  WriteStream, ReadStream: TCubicFileStream;
begin
  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteWord(65535);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(Word(65535), ReadStream.ReadWord, 'Word should round-trip correctly');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestWriteReadCardinal;
var
  WriteStream, ReadStream: TCubicFileStream;
begin
  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteCardinal($FFFFFFFF);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(Cardinal($FFFFFFFF), ReadStream.ReadCardinal, 'Cardinal should round-trip correctly');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestWriteReadInteger;
var
  WriteStream, ReadStream: TCubicFileStream;
begin
  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteInteger(-123456789);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(-123456789, ReadStream.ReadInteger, 'Integer should round-trip correctly');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestWriteReadInt64;
var
  WriteStream, ReadStream: TCubicFileStream;
  Original, ReadBack: Int64;
begin
  Original:= Int64($7FFFFFFFFFFFFFFF);

  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteInt64(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadInt64;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Original, ReadBack, 'Int64 should round-trip correctly');
end;

procedure TTestStreamFile.TestWriteReadUInt64;
var
  WriteStream, ReadStream: TCubicFileStream;
  Original, ReadBack: UInt64;
begin
  Original:= UInt64($FFFFFFFFFFFFFFFF);

  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteUInt64(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadUInt64;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Original, ReadBack, 'UInt64 should round-trip correctly');
end;

procedure TTestStreamFile.TestWriteReadSingle;
var
  WriteStream, ReadStream: TCubicFileStream;
  Original, ReadBack: Single;
begin
  Original:= 3.14159;

  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteSingle(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadSingle;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Original, ReadBack, 0.00001, 'Single should round-trip correctly');
end;

procedure TTestStreamFile.TestWriteReadDouble;
var
  WriteStream, ReadStream: TCubicFileStream;
  Original, ReadBack: Double;
begin
  Original:= 3.141592653589793;

  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteDouble(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadDouble;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Original, ReadBack, 0.0000000000001, 'Double should round-trip correctly');
end;

procedure TTestStreamFile.TestWriteReadDate;
var
  WriteStream, ReadStream: TCubicFileStream;
  Original, ReadBack: TDateTime;
begin
  Original:= Now;

  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteDate(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadDate;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Original, ReadBack, 0.0001, 'Date should round-trip correctly');
end;


{ Complex Structure Tests }

procedure TTestStreamFile.TestWriteReadRect;
var
  WriteStream, ReadStream: TCubicFileStream;
  Original, ReadBack: TRect;
begin
  Original:= Rect(10, 20, 100, 200);

  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteRect(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadRect;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Original.Left, ReadBack.Left, 'Left should match');
  Assert.AreEqual(Original.Top, ReadBack.Top, 'Top should match');
  Assert.AreEqual(Original.Right, ReadBack.Right, 'Right should match');
  Assert.AreEqual(Original.Bottom, ReadBack.Bottom, 'Bottom should match');
end;

procedure TTestStreamFile.TestWriteReadRectF;
var
  WriteStream, ReadStream: TCubicFileStream;
  Original, ReadBack: TRectF;
begin
  Original:= TRectF.Create(10.5, 20.5, 100.5, 200.5);

  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteRectF(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadRectF;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Original.Left, ReadBack.Left, 0.001, 'Left should match');
  Assert.AreEqual(Original.Top, ReadBack.Top, 0.001, 'Top should match');
  Assert.AreEqual(Original.Right, ReadBack.Right, 0.001, 'Right should match');
  Assert.AreEqual(Original.Bottom, ReadBack.Bottom, 0.001, 'Bottom should match');
end;

procedure TTestStreamFile.TestWriteReadIntegers;
var
  WriteStream, ReadStream: TCubicFileStream;
  Original, ReadBack: TIntegerArray;
  i: Integer;
begin
  SetLength(Original, 5);
  for i:= 0 to High(Original) do
    Original[i]:= i * 100;

  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteIntegers(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    ReadStream.ReadIntegers(ReadBack);
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Length(Original), Length(ReadBack), 'Array length should match');
  for i:= 0 to High(Original) do
    Assert.AreEqual(Original[i], ReadBack[i], Format('Element %d should match', [i]));
end;

procedure TTestStreamFile.TestWriteReadDoubles;
var
  WriteStream, ReadStream: TCubicFileStream;
  Original, ReadBack: TDoubleArray;
  i: Integer;
begin
  SetLength(Original, 5);
  for i:= 0 to High(Original) do
    Original[i]:= i * 1.5;

  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteDoubles(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    ReadStream.ReadDoubles(ReadBack);
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Length(Original), Length(ReadBack), 'Array length should match');
  for i:= 0 to High(Original) do
    Assert.AreEqual(Original[i], ReadBack[i], 0.0001, Format('Element %d should match', [i]));
end;


{ Reverse Read Tests }

procedure TTestStreamFile.TestRevReadCardinal;
var
  WriteStream, ReadStream: TCubicFileStream;
  Original, ReadBack: Cardinal;
begin
  Original:= $12345678;

  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteCardinal(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.RevReadCardinal;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Cardinal($78563412), ReadBack, 'Should swap bytes correctly');
end;

procedure TTestStreamFile.TestRevReadInteger;
var
  WriteStream, ReadStream: TCubicFileStream;
  Original: Cardinal;
  ReadBack: Integer;
begin
  Original:= $12345678;

  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteCardinal(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.RevReadInteger;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Integer($78563412), ReadBack, 'Should swap bytes correctly');
end;

procedure TTestStreamFile.TestRevReadWord;
var
  WriteStream, ReadStream: TCubicFileStream;
  Original, ReadBack: Word;
begin
  Original:= $1234;

  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteWord(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.RevReadWord;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Word($3412), ReadBack, 'Should swap bytes correctly');
end;


{ Raw Data Tests }

procedure TTestStreamFile.TestAsString;
var
  WriteStream, ReadStream: TCubicFileStream;
  TestData, Result: AnsiString;
begin
  TestData:= 'Test raw data content';

  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.PushAnsi(TestData);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    Result:= ReadStream.AsString;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(TestData, Result, 'AsString should return the raw file content');
end;

procedure TTestStreamFile.TestAsBytes;
var
  WriteStream, ReadStream: TCubicFileStream;
  TestData: AnsiString;
  Result: TBytes;
begin
  TestData:= 'TestBytes';

  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.PushAnsi(TestData);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    Result:= ReadStream.AsBytes;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Length(TestData), Length(Result), 'Length should match');
end;

procedure TTestStreamFile.TestPushBytes;
var
  WriteStream, ReadStream: TCubicFileStream;
  Original, ReadBack: TBytes;
  i: Integer;
begin
  SetLength(Original, 5);
  for i:= 0 to High(Original) do
    Original[i]:= i + 1;

  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.PushBytes(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.AsBytes;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Length(Original), Length(ReadBack), 'Length should match');
  for i:= 0 to High(Original) do
    Assert.AreEqual(Original[i], ReadBack[i], Format('Byte %d should match', [i]));
end;

procedure TTestStreamFile.TestPushBytesCnt_ReadByteChunk;
var
  WriteStream, ReadStream: TCubicFileStream;
  Original, ReadBack: TBytes;
  i: Integer;
begin
  SetLength(Original, 10);
  for i:= 0 to High(Original) do
    Original[i]:= i;

  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.PushBytesCnt(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TCubicFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadByteChunk;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Length(Original), Length(ReadBack), 'Length should match');
  for i:= 0 to High(Original) do
    Assert.AreEqual(Original[i], ReadBack[i], Format('Byte %d should match', [i]));
end;


initialization
  TDUnitX.RegisterTestFixture(TTestStreamFile);

end.
