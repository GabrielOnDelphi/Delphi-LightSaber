unit Test.LightCore.StreamFile;

{=============================================================================================================
   Unit tests for LightCore.StreamFile
   Tests TLightFileStream for reading/writing various data types
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
    procedure TestWriteReadString_Empty;

    [Test]
    procedure TestWriteReadStringA;

    [Test]
    procedure TestWriteReadStringA_Empty;

    [Test]
    procedure TestWriteReadStrings;

    [Test]
    procedure TestReadStrings_Function;

    [Test]
    procedure TestWriteReadChars;

    [Test]
    procedure TestWriteChars_Empty;

    [Test]
    procedure TestPushString;

    [Test]
    procedure TestReadStringCnt;

    [Test]
    procedure TestTryReadStringA;

    [Test]
    procedure TestTryReadStringA_InsufficientData;

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
    procedure TestWriteReadIntegers_Empty;

    [Test]
    procedure TestWriteReadDoubles;

    [Test]
    procedure TestWriteReadDoubles_Empty;

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
    procedure TestAsString_Empty;

    [Test]
    procedure TestAsBytes;

    [Test]
    procedure TestAsBytes_Empty;

    [Test]
    procedure TestPushBytes;

    [Test]
    procedure TestPushBytesCnt_ReadByteChunk;

    [Test]
    procedure TestReadByteChunk_Empty;

    [Test]
    procedure TestPushAnsi;

    [Test]
    procedure TestPushAnsi_Empty;

    { Safety Limit Tests }
    [Test]
    procedure TestReadString_SafetyLimit;

    [Test]
    procedure TestReadStringA_ExceedsFileSize;

    { Backwards Compatibility Tests }
    [Test]
    procedure TestTCubicFileStream_Alias;
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
  WriteStream, ReadStream: TLightFileStream;
begin
  // First create a file
  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteInteger(12345);
  finally
    FreeAndNil(WriteStream);
  end;

  // Now test CreateRead
  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(12345, ReadStream.ReadInteger, 'CreateRead should open file for reading');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestCreateWrite;
var
  WriteStream, ReadStream: TLightFileStream;
begin
  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteInteger(67890);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(67890, ReadStream.ReadInteger, 'CreateWrite should create file for writing');
  finally
    FreeAndNil(ReadStream);
  end;
end;


{ Header Tests }

procedure TTestStreamFile.TestWriteReadHeader_Match;
var
  WriteStream, ReadStream: TLightFileStream;
begin
  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteHeader('TestSignature', 1);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    Assert.IsTrue(ReadStream.ReadHeader('TestSignature', 1), 'Header should match');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestWriteReadHeader_Mismatch;
var
  WriteStream, ReadStream: TLightFileStream;
begin
  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteHeader('SignatureA', 1);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    Assert.IsFalse(ReadStream.ReadHeader('SignatureB', 1), 'Header should not match different signature');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestWriteReadHeader_Version;
var
  WriteStream, ReadStream: TLightFileStream;
  Version: Word;
begin
  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteHeader('MyApp', 5);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
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
  WriteStream, ReadStream: TLightFileStream;
begin
  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteCheckPoint;
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    Assert.IsTrue(ReadStream.ReadCheckPoint, 'Checkpoint should match');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestCheckPoint_WithString;
var
  WriteStream, ReadStream: TLightFileStream;
begin
  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteCheckPoint('MyMarker');
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    Assert.IsTrue(ReadStream.ReadCheckPoint('MyMarker'), 'Checkpoint with string should match');
  finally
    FreeAndNil(ReadStream);
  end;
end;


{ Enter Tests }

procedure TTestStreamFile.TestWriteReadEnter;
var
  WriteStream, ReadStream: TLightFileStream;
begin
  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteEnter;
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    Assert.IsTrue(ReadStream.ReadEnter, 'Should read CRLF correctly');
  finally
    FreeAndNil(ReadStream);
  end;
end;


{ Padding Tests }

procedure TTestStreamFile.TestWriteReadPadding0;
var
  WriteStream, ReadStream: TLightFileStream;
  PaddingSize: Integer;
begin
  PaddingSize:= 64;

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WritePadding0(PaddingSize);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
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
  WriteStream, ReadStream: TLightFileStream;
  PaddingSize: Integer;
begin
  PaddingSize:= 64;

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WritePadding(PaddingSize);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
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
  WriteStream, ReadStream: TLightFileStream;
  Original, ReadBack: string;
begin
  Original:= 'Test Unicode String: äöü ß 中文';

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteString(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadString;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Original, ReadBack, 'Unicode string should round-trip correctly');
end;

procedure TTestStreamFile.TestWriteReadString_Empty;
var
  WriteStream, ReadStream: TLightFileStream;
  ReadBack: string;
begin
  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteString('');
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadString;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual('', ReadBack, 'Empty string should round-trip correctly');
end;

procedure TTestStreamFile.TestWriteReadStringA;
var
  WriteStream, ReadStream: TLightFileStream;
  Original, ReadBack: AnsiString;
begin
  Original:= 'Test ANSI String 123';

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteStringA(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadStringA;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Original, ReadBack, 'ANSI string should round-trip correctly');
end;

procedure TTestStreamFile.TestWriteReadStringA_Empty;
var
  WriteStream, ReadStream: TLightFileStream;
  ReadBack: AnsiString;
begin
  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteStringA('');
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadStringA;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(AnsiString(''), ReadBack, 'Empty ANSI string should round-trip correctly');
end;

procedure TTestStreamFile.TestWriteReadStrings;
var
  WriteStream, ReadStream: TLightFileStream;
  Original, ReadBack: TStringList;
begin
  Original:= TStringList.Create;
  ReadBack:= TStringList.Create;
  try
    Original.Add('Line 1');
    Original.Add('Line 2');
    Original.Add('Line 3');

    WriteStream:= TLightFileStream.CreateWrite(FTestFile);
    try
      WriteStream.WriteStrings(Original);
    finally
      FreeAndNil(WriteStream);
    end;

    ReadStream:= TLightFileStream.CreateRead(FTestFile);
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

procedure TTestStreamFile.TestReadStrings_Function;
var
  WriteStream, ReadStream: TLightFileStream;
  Original, ReadBack: TStringList;
begin
  Original:= TStringList.Create;
  try
    Original.Add('Line A');
    Original.Add('Line B');

    WriteStream:= TLightFileStream.CreateWrite(FTestFile);
    try
      WriteStream.WriteStrings(Original);
    finally
      FreeAndNil(WriteStream);
    end;

    ReadStream:= TLightFileStream.CreateRead(FTestFile);
    try
      ReadBack:= ReadStream.ReadStrings;
      try
        Assert.AreEqual(Original.Text, ReadBack.Text, 'ReadStrings function should return correct data');
      finally
        FreeAndNil(ReadBack);
      end;
    finally
      FreeAndNil(ReadStream);
    end;
  finally
    FreeAndNil(Original);
  end;
end;

procedure TTestStreamFile.TestWriteReadChars;
var
  WriteStream, ReadStream: TLightFileStream;
  Original, ReadBack: string;
begin
  Original:= 'TestChars';

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteChars(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadChars(Length(Original));
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Original, ReadBack, 'Chars should round-trip correctly');
end;

procedure TTestStreamFile.TestWriteChars_Empty;
var
  WriteStream, ReadStream: TLightFileStream;
begin
  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteChars(AnsiString(''));  // Should not throw
    WriteStream.WriteInteger(42);            // Write something to verify stream is working
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(42, ReadStream.ReadInteger, 'Stream should work after writing empty chars');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestPushString;
var
  WriteStream, ReadStream: TLightFileStream;
  Original, ReadBack: string;
begin
  Original:= 'PushStringTest';

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.PushString(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadChars(Length(Original));
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Original, ReadBack, 'PushString should write without length prefix');
end;

procedure TTestStreamFile.TestReadStringCnt;
var
  WriteStream, ReadStream: TLightFileStream;
  Original, ReadBack: string;
begin
  Original:= 'ABCDEFGHIJ';

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.PushString(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadStringCnt(5);
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual('ABCDE', ReadBack, 'Should read exactly 5 characters');
end;

procedure TTestStreamFile.TestTryReadStringA;
var
  WriteStream, ReadStream: TLightFileStream;
  Original, ReadBack: AnsiString;
begin
  Original:= 'TestTryRead';

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.PushAnsi(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.TryReadStringA(Length(Original));
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Original, ReadBack, 'TryReadStringA should read entire string');
end;

procedure TTestStreamFile.TestTryReadStringA_InsufficientData;
var
  WriteStream, ReadStream: TLightFileStream;
  Original, ReadBack: AnsiString;
begin
  Original:= 'Short';

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.PushAnsi(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    // Try to read more than available - should not raise exception
    ReadBack:= ReadStream.TryReadStringA(100);
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Original, ReadBack, 'TryReadStringA should read available data without exception');
end;


{ Numeric Tests }

procedure TTestStreamFile.TestWriteReadBoolean;
var
  WriteStream, ReadStream: TLightFileStream;
begin
  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteBoolean(True);
    WriteStream.WriteBoolean(False);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    Assert.IsTrue(ReadStream.ReadBoolean, 'True should round-trip correctly');
    Assert.IsFalse(ReadStream.ReadBoolean, 'False should round-trip correctly');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestWriteReadByte;
var
  WriteStream, ReadStream: TLightFileStream;
begin
  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteByte(255);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(Byte(255), ReadStream.ReadByte, 'Byte should round-trip correctly');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestWriteReadShortInt;
var
  WriteStream, ReadStream: TLightFileStream;
begin
  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteShortInt(-128);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(ShortInt(-128), ReadStream.ReadShortInt, 'ShortInt should round-trip correctly');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestWriteReadSmallInt;
var
  WriteStream, ReadStream: TLightFileStream;
begin
  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteSmallInt(-32768);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(SmallInt(-32768), ReadStream.ReadSmallInt, 'SmallInt should round-trip correctly');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestWriteReadWord;
var
  WriteStream, ReadStream: TLightFileStream;
begin
  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteWord(65535);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(Word(65535), ReadStream.ReadWord, 'Word should round-trip correctly');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestWriteReadCardinal;
var
  WriteStream, ReadStream: TLightFileStream;
begin
  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteCardinal($FFFFFFFF);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(Cardinal($FFFFFFFF), ReadStream.ReadCardinal, 'Cardinal should round-trip correctly');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestWriteReadInteger;
var
  WriteStream, ReadStream: TLightFileStream;
begin
  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteInteger(-123456789);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(-123456789, ReadStream.ReadInteger, 'Integer should round-trip correctly');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestWriteReadInt64;
var
  WriteStream, ReadStream: TLightFileStream;
  Original, ReadBack: Int64;
begin
  Original:= Int64($7FFFFFFFFFFFFFFF);

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteInt64(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadInt64;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Original, ReadBack, 'Int64 should round-trip correctly');
end;

procedure TTestStreamFile.TestWriteReadUInt64;
var
  WriteStream, ReadStream: TLightFileStream;
  Original, ReadBack: UInt64;
begin
  Original:= UInt64($FFFFFFFFFFFFFFFF);

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteUInt64(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadUInt64;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Original, ReadBack, 'UInt64 should round-trip correctly');
end;

procedure TTestStreamFile.TestWriteReadSingle;
var
  WriteStream, ReadStream: TLightFileStream;
  Original, ReadBack: Single;
begin
  Original:= 3.14159;

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteSingle(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadSingle;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Original, ReadBack, 0.00001, 'Single should round-trip correctly');
end;

procedure TTestStreamFile.TestWriteReadDouble;
var
  WriteStream, ReadStream: TLightFileStream;
  Original, ReadBack: Double;
begin
  Original:= 3.141592653589793;

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteDouble(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadDouble;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Original, ReadBack, 0.0000000000001, 'Double should round-trip correctly');
end;

procedure TTestStreamFile.TestWriteReadDate;
var
  WriteStream, ReadStream: TLightFileStream;
  Original, ReadBack: TDateTime;
begin
  Original:= Now;

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteDate(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
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
  WriteStream, ReadStream: TLightFileStream;
  Original, ReadBack: TRect;
begin
  Original:= Rect(10, 20, 100, 200);

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteRect(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
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
  WriteStream, ReadStream: TLightFileStream;
  Original, ReadBack: TRectF;
begin
  Original:= TRectF.Create(10.5, 20.5, 100.5, 200.5);

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteRectF(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
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
  WriteStream, ReadStream: TLightFileStream;
  Original, ReadBack: TIntegerArray;
  i: Integer;
begin
  SetLength(Original, 5);
  for i:= 0 to High(Original) do
    Original[i]:= i * 100;

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteIntegers(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    ReadStream.ReadIntegers(ReadBack);
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Length(Original), Length(ReadBack), 'Array length should match');
  for i:= 0 to High(Original) do
    Assert.AreEqual(Original[i], ReadBack[i], Format('Element %d should match', [i]));
end;

procedure TTestStreamFile.TestWriteReadIntegers_Empty;
var
  WriteStream, ReadStream: TLightFileStream;
  Original, ReadBack: TIntegerArray;
begin
  SetLength(Original, 0);

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteIntegers(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    ReadStream.ReadIntegers(ReadBack);
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(0, Length(ReadBack), 'Empty array should round-trip correctly');
end;

procedure TTestStreamFile.TestWriteReadDoubles;
var
  WriteStream, ReadStream: TLightFileStream;
  Original, ReadBack: TDoubleArray;
  i: Integer;
begin
  SetLength(Original, 5);
  for i:= 0 to High(Original) do
    Original[i]:= i * 1.5;

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteDoubles(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    ReadStream.ReadDoubles(ReadBack);
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Length(Original), Length(ReadBack), 'Array length should match');
  for i:= 0 to High(Original) do
    Assert.AreEqual(Original[i], ReadBack[i], 0.0001, Format('Element %d should match', [i]));
end;

procedure TTestStreamFile.TestWriteReadDoubles_Empty;
var
  WriteStream, ReadStream: TLightFileStream;
  Original, ReadBack: TDoubleArray;
begin
  SetLength(Original, 0);

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteDoubles(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    ReadStream.ReadDoubles(ReadBack);
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(0, Length(ReadBack), 'Empty double array should round-trip correctly');
end;


{ Reverse Read Tests }

procedure TTestStreamFile.TestRevReadCardinal;
var
  WriteStream, ReadStream: TLightFileStream;
  Original, ReadBack: Cardinal;
begin
  Original:= $12345678;

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteCardinal(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.RevReadCardinal;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Cardinal($78563412), ReadBack, 'Should swap bytes correctly');
end;

procedure TTestStreamFile.TestRevReadInteger;
var
  WriteStream, ReadStream: TLightFileStream;
  Original: Cardinal;
  ReadBack: Integer;
begin
  Original:= $12345678;

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteCardinal(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.RevReadInteger;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Integer($78563412), ReadBack, 'Should swap bytes correctly');
end;

procedure TTestStreamFile.TestRevReadWord;
var
  WriteStream, ReadStream: TLightFileStream;
  Original, ReadBack: Word;
begin
  Original:= $1234;

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteWord(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
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
  WriteStream, ReadStream: TLightFileStream;
  TestData, Result: AnsiString;
begin
  TestData:= 'Test raw data content';

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.PushAnsi(TestData);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    Result:= ReadStream.AsString;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(TestData, Result, 'AsString should return the raw file content');
end;

procedure TTestStreamFile.TestAsString_Empty;
var
  WriteStream, ReadStream: TLightFileStream;
begin
  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    // Write nothing - create empty file
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    Assert.WillRaise(
      procedure
      begin
        ReadStream.AsString;
      end,
      Exception,
      'AsString should raise exception on empty stream');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestAsBytes;
var
  WriteStream, ReadStream: TLightFileStream;
  TestData: AnsiString;
  Result: TBytes;
begin
  TestData:= 'TestBytes';

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.PushAnsi(TestData);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    Result:= ReadStream.AsBytes;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Length(TestData), Length(Result), 'Length should match');
end;

procedure TTestStreamFile.TestAsBytes_Empty;
var
  WriteStream, ReadStream: TLightFileStream;
  Result: TBytes;
begin
  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    // Write nothing - create empty file
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    Result:= ReadStream.AsBytes;  // Should not raise exception
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(0, Length(Result), 'Empty stream should return empty bytes array');
end;

procedure TTestStreamFile.TestPushBytes;
var
  WriteStream, ReadStream: TLightFileStream;
  Original, ReadBack: TBytes;
  i: Integer;
begin
  SetLength(Original, 5);
  for i:= 0 to High(Original) do
    Original[i]:= i + 1;

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.PushBytes(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
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
  WriteStream, ReadStream: TLightFileStream;
  Original, ReadBack: TBytes;
  i: Integer;
begin
  SetLength(Original, 10);
  for i:= 0 to High(Original) do
    Original[i]:= i;

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.PushBytesCnt(Original);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadByteChunk;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(Length(Original), Length(ReadBack), 'Length should match');
  for i:= 0 to High(Original) do
    Assert.AreEqual(Original[i], ReadBack[i], Format('Byte %d should match', [i]));
end;

procedure TTestStreamFile.TestReadByteChunk_Empty;
var
  WriteStream, ReadStream: TLightFileStream;
  ReadBack: TBytes;
begin
  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteCardinal(0);  // Write count of 0
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadByteChunk;
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(0, Length(ReadBack), 'Empty chunk should return empty array');
end;

procedure TTestStreamFile.TestPushAnsi;
var
  WriteStream, ReadStream: TLightFileStream;
  TestData, ReadBack: AnsiString;
begin
  TestData:= 'PushAnsiTest';

  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.PushAnsi(TestData);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    ReadBack:= ReadStream.ReadCharsA(Length(TestData));
  finally
    FreeAndNil(ReadStream);
  end;

  Assert.AreEqual(TestData, ReadBack, 'PushAnsi should write raw data correctly');
end;

procedure TTestStreamFile.TestPushAnsi_Empty;
var
  WriteStream, ReadStream: TLightFileStream;
begin
  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.PushAnsi('');  // Should not throw
    WriteStream.WriteInteger(42);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(42, ReadStream.ReadInteger, 'Stream should work after pushing empty string');
  finally
    FreeAndNil(ReadStream);
  end;
end;


{ Safety Limit Tests }

procedure TTestStreamFile.TestReadString_SafetyLimit;
var
  WriteStream, ReadStream: TLightFileStream;
begin
  // Write a string that is larger than the safety limit we'll use for reading
  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteString('This is a test string longer than 10 bytes');
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    Assert.WillRaise(
      procedure
      begin
        ReadStream.ReadString(10);  // Safety limit of 10 bytes
      end,
      Exception,
      'Should raise exception when string exceeds safety limit');
  finally
    FreeAndNil(ReadStream);
  end;
end;

procedure TTestStreamFile.TestReadStringA_ExceedsFileSize;
var
  WriteStream, ReadStream: TLightFileStream;
begin
  // Write a corrupt length prefix that exceeds file size
  WriteStream:= TLightFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteCardinal(1000);  // Claim string is 1000 bytes
    WriteStream.PushAnsi('Short');    // But only write a few bytes
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    Assert.WillRaise(
      procedure
      begin
        ReadStream.ReadStringA(2000);  // Large enough safety limit
      end,
      Exception,
      'Should raise exception when string length exceeds file size');
  finally
    FreeAndNil(ReadStream);
  end;
end;


{ Backwards Compatibility Tests }

procedure TTestStreamFile.TestTCubicFileStream_Alias;
var
  WriteStream: TCubicFileStream;  // Using the alias
  ReadStream: TLightFileStream;   // Using the new name
begin
  WriteStream:= TCubicFileStream.CreateWrite(FTestFile);
  try
    WriteStream.WriteInteger(99999);
  finally
    FreeAndNil(WriteStream);
  end;

  ReadStream:= TLightFileStream.CreateRead(FTestFile);
  try
    Assert.AreEqual(99999, ReadStream.ReadInteger, 'TCubicFileStream alias should work with TLightFileStream');
  finally
    FreeAndNil(ReadStream);
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestStreamFile);

end.
