unit Test.LightCore.Graph.BkgColorParams;

{=============================================================================================================
   Unit tests for LightCore.Graph.BkgColorParams.pas
   Tests RBkgColorParams record: Reset, stream serialization, enum validation.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  System.UITypes,
  LightCore.StreamBuff,
  LightCore.Graph.BkgColorParams;

type
  [TestFixture]
  TTestBkgColorParams = class
  private
    FParams: RBkgColorParams;
    FTempFile: string;
    procedure CreateTempFile;
    procedure DeleteTempFile;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Reset Tests }
    [Test]
    procedure TestReset_SetsDefaultFillType;

    [Test]
    procedure TestReset_SetsDefaultEffectShape;

    [Test]
    procedure TestReset_SetsDefaultEffectColor;

    [Test]
    procedure TestReset_SetsDefaultFadeSpeed;

    [Test]
    procedure TestReset_SetsDefaultEdgeSmear;

    [Test]
    procedure TestReset_SetsDefaultNeighborWeight;

    [Test]
    procedure TestReset_SetsDefaultNeighborDist;

    [Test]
    procedure TestReset_SetsDefaultTolerance;

    [Test]
    procedure TestReset_SetsDefaultColor;

    { Stream Write Tests }
    [Test]
    procedure TestWriteToStream_NilStream;

    [Test]
    procedure TestWriteToStream_BasicWrite;

    { Stream Read Tests }
    [Test]
    procedure TestReadFromStream_NilStream;

    [Test]
    procedure TestReadFromStream_RoundTrip;

    [Test]
    procedure TestReadFromStream_AllEnumValues;

    [Test]
    procedure TestReadFromStream_InvalidFillType;

    [Test]
    procedure TestReadFromStream_InvalidEffectShape;

    [Test]
    procedure TestReadFromStream_InvalidEffectColor;

    [Test]
    procedure TestReadFromStream_FutureVersion;

    { Integration Tests }
    [Test]
    procedure TestRoundTrip_PreservesAllFields;

    [Test]
    procedure TestRoundTrip_NonDefaultValues;
  end;

implementation


procedure TTestBkgColorParams.Setup;
begin
  FParams.Reset;
  FTempFile:= '';
end;


procedure TTestBkgColorParams.TearDown;
begin
  DeleteTempFile;
end;


procedure TTestBkgColorParams.CreateTempFile;
begin
  FTempFile:= TPath.GetTempFileName;
end;


procedure TTestBkgColorParams.DeleteTempFile;
begin
  if (FTempFile <> '') AND TFile.Exists(FTempFile)
  then TFile.Delete(FTempFile);
  FTempFile:= '';
end;


{ Reset Tests }

procedure TTestBkgColorParams.TestReset_SetsDefaultFillType;
begin
  FParams.FillType:= ftFade;  { Set non-default }
  FParams.Reset;
  Assert.AreEqual(Ord(ftSolid), Ord(FParams.FillType), 'FillType should be ftSolid after Reset');
end;


procedure TTestBkgColorParams.TestReset_SetsDefaultEffectShape;
begin
  FParams.EffectShape:= esRectangles;  { Set non-default }
  FParams.Reset;
  Assert.AreEqual(Ord(esOneColor), Ord(FParams.EffectShape), 'EffectShape should be esOneColor after Reset');
end;


procedure TTestBkgColorParams.TestReset_SetsDefaultEffectColor;
begin
  FParams.EffectColor:= ecUserColor;  { Set non-default }
  FParams.Reset;
  Assert.AreEqual(Ord(ecImageAverage), Ord(FParams.EffectColor), 'EffectColor should be ecImageAverage after Reset');
end;


procedure TTestBkgColorParams.TestReset_SetsDefaultFadeSpeed;
begin
  FParams.FadeSpeed:= 999;
  FParams.Reset;
  Assert.AreEqual(200, FParams.FadeSpeed, 'FadeSpeed should be 200 after Reset');
end;


procedure TTestBkgColorParams.TestReset_SetsDefaultEdgeSmear;
begin
  FParams.EdgeSmear:= 100;
  FParams.Reset;
  Assert.AreEqual(Byte(0), FParams.EdgeSmear, 'EdgeSmear should be 0 after Reset');
end;


procedure TTestBkgColorParams.TestReset_SetsDefaultNeighborWeight;
begin
  FParams.NeighborWeight:= 999;
  FParams.Reset;
  Assert.AreEqual(100, FParams.NeighborWeight, 'NeighborWeight should be 100 after Reset');
end;


procedure TTestBkgColorParams.TestReset_SetsDefaultNeighborDist;
begin
  FParams.NeighborDist:= 999;
  FParams.Reset;
  Assert.AreEqual(2, FParams.NeighborDist, 'NeighborDist should be 2 after Reset');
end;


procedure TTestBkgColorParams.TestReset_SetsDefaultTolerance;
begin
  FParams.Tolerance:= 999;
  FParams.Reset;
  Assert.AreEqual(8, FParams.Tolerance, 'Tolerance should be 8 after Reset');
end;


procedure TTestBkgColorParams.TestReset_SetsDefaultColor;
begin
  FParams.Color:= TColors.Red;
  FParams.Reset;
  Assert.AreEqual(TColor($218F42), FParams.Color, 'Color should be $218F42 after Reset');
end;


{ Stream Write Tests }

procedure TTestBkgColorParams.TestWriteToStream_NilStream;
begin
  Assert.WillRaise(
    procedure
    begin
      FParams.WriteToStream(NIL);
    end,
    Exception,
    'FParams.WriteToStream(NIL) must raise Exception');
end;


procedure TTestBkgColorParams.TestWriteToStream_BasicWrite;
CONST
  { Version, Color: 2 Integers; FillType, EffectShape, EffectColor, EdgeSmear: 4 Bytes; NeighborDist, Tolerance, FadeSpeed, NeighborWeight: 4 Integers.
    4+4 + 4*1 + 4*4 = 28 bytes, plus the 64-byte validation padding (TLightStream.FrozenPaddingSize) }
  RecordSize = 92;
VAR Stream: TLightStream;
begin
  CreateTempFile;
  FParams.Reset;

  Stream:= TLightStream.CreateWrite(FTempFile);
  TRY
    Assert.WillNotRaiseAny(
      procedure
      begin
        FParams.WriteToStream(Stream);
      end,
      'FParams.WriteToStream(Stream) must not raise');
  FINALLY
    FreeAndNil(Stream);
  END;

  { Read the file field by field with the plain stream readers, not with ReadFromStream }
  Stream:= TLightStream.CreateRead(FTempFile);
  TRY
    Assert.AreEqual(Int64(RecordSize), Stream.Size, 'Size of the saved record');
    Assert.AreEqual(1, Stream.ReadInteger, 'Version');
    Assert.AreEqual($218F42, Stream.ReadInteger, 'Color');
    Assert.AreEqual(Byte(0), Stream.ReadByte, 'FillType = ftSolid');
    Assert.AreEqual(Byte(2), Stream.ReadByte, 'EffectShape = esOneColor');
    Assert.AreEqual(Byte(1), Stream.ReadByte, 'EffectColor = ecImageAverage');
    Assert.AreEqual(Byte(0), Stream.ReadByte, 'EdgeSmear');
    Assert.AreEqual(2, Stream.ReadInteger, 'NeighborDist');
    Assert.AreEqual(8, Stream.ReadInteger, 'Tolerance');
    Assert.AreEqual(200, Stream.ReadInteger, 'FadeSpeed');
    Assert.AreEqual(100, Stream.ReadInteger, 'NeighborWeight');
    Assert.WillNotRaiseAny(
      procedure
      begin
        Stream.ReadPaddingValidation;
      end,
      'The record must end with the validation padding');
    Assert.AreEqual(Stream.Size, Stream.Position, 'Nothing may follow the padding');
  FINALLY
    FreeAndNil(Stream);
  END;
end;


{ Stream Read Tests }

procedure TTestBkgColorParams.TestReadFromStream_NilStream;
begin
  Assert.WillRaise(
    procedure
    begin
      FParams.ReadFromStream(NIL);
    end,
    Exception,
    'FParams.ReadFromStream(NIL) must raise Exception');
end;


procedure TTestBkgColorParams.TestReadFromStream_RoundTrip;
VAR
  Stream: TLightStream;
  ReadParams: RBkgColorParams;
begin
  CreateTempFile;
  FParams.Reset;

  { Write }
  Stream:= TLightStream.CreateWrite(FTempFile);
  TRY
    FParams.WriteToStream(Stream);
  FINALLY
    FreeAndNil(Stream);
  END;

  { Read }
  Stream:= TLightStream.CreateRead(FTempFile);
  TRY
    ReadParams.ReadFromStream(Stream);
  FINALLY
    FreeAndNil(Stream);
  END;

  Assert.AreEqual(Ord(FParams.FillType), Ord(ReadParams.FillType), 'FillType mismatch');
  Assert.AreEqual(Ord(FParams.EffectShape), Ord(ReadParams.EffectShape), 'EffectShape mismatch');
  Assert.AreEqual(Ord(FParams.EffectColor), Ord(ReadParams.EffectColor), 'EffectColor mismatch');
end;


procedure TTestBkgColorParams.TestReadFromStream_AllEnumValues;
VAR
  Stream: TLightStream;
  ReadParams: RBkgColorParams;
begin
  CreateTempFile;

  { Test all enum combinations }
  FParams.Reset;
  FParams.FillType   := ftFade;
  FParams.EffectShape:= esTriangles;
  FParams.EffectColor:= ecUserColor;

  { Write }
  Stream:= TLightStream.CreateWrite(FTempFile);
  TRY
    FParams.WriteToStream(Stream);
  FINALLY
    FreeAndNil(Stream);
  END;

  { Read }
  Stream:= TLightStream.CreateRead(FTempFile);
  TRY
    ReadParams.ReadFromStream(Stream);
  FINALLY
    FreeAndNil(Stream);
  END;

  Assert.AreEqual(Ord(ftFade), Ord(ReadParams.FillType), 'FillType should be ftFade');
  Assert.AreEqual(Ord(esTriangles), Ord(ReadParams.EffectShape), 'EffectShape should be esTriangles');
  Assert.AreEqual(Ord(ecUserColor), Ord(ReadParams.EffectColor), 'EffectColor should be ecUserColor');
end;


procedure TTestBkgColorParams.TestReadFromStream_InvalidFillType;
VAR Stream: TLightStream;
begin
  CreateTempFile;

  { Write invalid data manually: Version=1, Color, then invalid FillType byte }
  Stream:= TLightStream.CreateWrite(FTempFile);
  TRY
    Stream.WriteInteger(1);           { CurrentVersion }
    Stream.WriteInteger(TColors.Black); { Color }
    Stream.WriteByte(255);            { Invalid FillType (only 0-1 valid) }
    Stream.WriteByte(0);              { EffectShape }
    Stream.WriteByte(0);              { EffectColor }
    Stream.WriteByte(0);              { EdgeSmear }
    Stream.WriteInteger(0);           { NeighborDist }
    Stream.WriteInteger(0);           { Tolerance }
    Stream.WriteInteger(0);           { FadeSpeed }
    Stream.WriteInteger(0);           { NeighborWeight }
  FINALLY
    FreeAndNil(Stream);
  END;

  { Read should raise exception }
  Stream:= TLightStream.CreateRead(FTempFile);
  TRY
    Assert.WillRaise(
      procedure
      begin
        FParams.ReadFromStream(Stream);
      end,
      Exception,
      'FParams.ReadFromStream(Stream) must raise Exception');
  FINALLY
    FreeAndNil(Stream);
  END;
end;


procedure TTestBkgColorParams.TestReadFromStream_InvalidEffectShape;
VAR Stream: TLightStream;
begin
  CreateTempFile;

  { Write invalid data: valid FillType but invalid EffectShape }
  Stream:= TLightStream.CreateWrite(FTempFile);
  TRY
    Stream.WriteInteger(1);           { CurrentVersion }
    Stream.WriteInteger(TColors.Black); { Color }
    Stream.WriteByte(0);              { FillType (valid) }
    Stream.WriteByte(200);            { Invalid EffectShape (only 0-2 valid) }
    Stream.WriteByte(0);              { EffectColor }
    Stream.WriteByte(0);              { EdgeSmear }
    Stream.WriteInteger(0);           { NeighborDist }
    Stream.WriteInteger(0);           { Tolerance }
    Stream.WriteInteger(0);           { FadeSpeed }
    Stream.WriteInteger(0);           { NeighborWeight }
  FINALLY
    FreeAndNil(Stream);
  END;

  Stream:= TLightStream.CreateRead(FTempFile);
  TRY
    Assert.WillRaise(
      procedure
      begin
        FParams.ReadFromStream(Stream);
      end,
      Exception,
      'FParams.ReadFromStream(Stream) must raise Exception');
  FINALLY
    FreeAndNil(Stream);
  END;
end;


procedure TTestBkgColorParams.TestReadFromStream_InvalidEffectColor;
VAR Stream: TLightStream;
begin
  CreateTempFile;

  { Write invalid data: valid FillType/EffectShape but invalid EffectColor }
  Stream:= TLightStream.CreateWrite(FTempFile);
  TRY
    Stream.WriteInteger(1);           { CurrentVersion }
    Stream.WriteInteger(TColors.Black); { Color }
    Stream.WriteByte(0);              { FillType (valid) }
    Stream.WriteByte(0);              { EffectShape (valid) }
    Stream.WriteByte(100);            { Invalid EffectColor (only 0-2 valid) }
    Stream.WriteByte(0);              { EdgeSmear }
    Stream.WriteInteger(0);           { NeighborDist }
    Stream.WriteInteger(0);           { Tolerance }
    Stream.WriteInteger(0);           { FadeSpeed }
    Stream.WriteInteger(0);           { NeighborWeight }
  FINALLY
    FreeAndNil(Stream);
  END;

  Stream:= TLightStream.CreateRead(FTempFile);
  TRY
    Assert.WillRaise(
      procedure
      begin
        FParams.ReadFromStream(Stream);
      end,
      Exception,
      'FParams.ReadFromStream(Stream) must raise Exception');
  FINALLY
    FreeAndNil(Stream);
  END;
end;


procedure TTestBkgColorParams.TestReadFromStream_FutureVersion;
VAR Stream: TLightStream;
begin
  CreateTempFile;

  { Write with future version number }
  Stream:= TLightStream.CreateWrite(FTempFile);
  TRY
    Stream.WriteInteger(999);         { Future version }
  FINALLY
    FreeAndNil(Stream);
  END;

  Stream:= TLightStream.CreateRead(FTempFile);
  TRY
    Assert.WillRaise(
      procedure
      begin
        FParams.ReadFromStream(Stream);
      end,
      Exception,
      'FParams.ReadFromStream(Stream) must raise Exception');
  FINALLY
    FreeAndNil(Stream);
  END;
end;


{ Integration Tests }

procedure TTestBkgColorParams.TestRoundTrip_PreservesAllFields;
VAR
  Stream: TLightStream;
  ReadParams: RBkgColorParams;
begin
  CreateTempFile;
  FParams.Reset;

  { Write }
  Stream:= TLightStream.CreateWrite(FTempFile);
  TRY
    FParams.WriteToStream(Stream);
  FINALLY
    FreeAndNil(Stream);
  END;

  { Read }
  Stream:= TLightStream.CreateRead(FTempFile);
  TRY
    ReadParams.ReadFromStream(Stream);
  FINALLY
    FreeAndNil(Stream);
  END;

  Assert.AreEqual(Ord(FParams.FillType), Ord(ReadParams.FillType), 'FillType mismatch');
  Assert.AreEqual(Ord(FParams.EffectShape), Ord(ReadParams.EffectShape), 'EffectShape mismatch');
  Assert.AreEqual(Ord(FParams.EffectColor), Ord(ReadParams.EffectColor), 'EffectColor mismatch');
  Assert.AreEqual(FParams.FadeSpeed, ReadParams.FadeSpeed, 'FadeSpeed mismatch');
  Assert.AreEqual(FParams.EdgeSmear, ReadParams.EdgeSmear, 'EdgeSmear mismatch');
  Assert.AreEqual(FParams.NeighborWeight, ReadParams.NeighborWeight, 'NeighborWeight mismatch');
  Assert.AreEqual(FParams.NeighborDist, ReadParams.NeighborDist, 'NeighborDist mismatch');
  Assert.AreEqual(FParams.Tolerance, ReadParams.Tolerance, 'Tolerance mismatch');
  Assert.AreEqual(FParams.Color, ReadParams.Color, 'Color mismatch');
end;


procedure TTestBkgColorParams.TestRoundTrip_NonDefaultValues;
VAR
  Stream: TLightStream;
  ReadParams: RBkgColorParams;
begin
  CreateTempFile;

  { Set non-default values }
  FParams.FillType      := ftFade;
  FParams.EffectShape   := esRectangles;
  FParams.EffectColor   := ecAutoDetBorder;
  FParams.FadeSpeed     := 500;
  FParams.EdgeSmear     := 25;
  FParams.NeighborWeight:= 300;
  FParams.NeighborDist  := 5;
  FParams.Tolerance     := 15;
  FParams.Color         := TColors.Red;

  { Write }
  Stream:= TLightStream.CreateWrite(FTempFile);
  TRY
    FParams.WriteToStream(Stream);
  FINALLY
    FreeAndNil(Stream);
  END;

  { Read }
  Stream:= TLightStream.CreateRead(FTempFile);
  TRY
    ReadParams.ReadFromStream(Stream);
  FINALLY
    FreeAndNil(Stream);
  END;

  Assert.AreEqual(Ord(ftFade), Ord(ReadParams.FillType), 'FillType should be ftFade');
  Assert.AreEqual(Ord(esRectangles), Ord(ReadParams.EffectShape), 'EffectShape should be esRectangles');
  Assert.AreEqual(Ord(ecAutoDetBorder), Ord(ReadParams.EffectColor), 'EffectColor should be ecAutoDetBorder');
  Assert.AreEqual(500, ReadParams.FadeSpeed, 'FadeSpeed should be 500');
  Assert.AreEqual(Byte(25), ReadParams.EdgeSmear, 'EdgeSmear should be 25');
  Assert.AreEqual(300, ReadParams.NeighborWeight, 'NeighborWeight should be 300');
  Assert.AreEqual(5, ReadParams.NeighborDist, 'NeighborDist should be 5');
  Assert.AreEqual(15, ReadParams.Tolerance, 'Tolerance should be 15');
  Assert.AreEqual(TColor(TColors.Red), ReadParams.Color, 'Color should be TColors.Red');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestBkgColorParams);

end.
