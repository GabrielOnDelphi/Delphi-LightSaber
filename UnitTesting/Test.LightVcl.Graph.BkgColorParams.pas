unit Test.LightVcl.Graph.BkgColorParams;

{=============================================================================================================
   Unit tests for LightVcl.Graph.BkgColorParams.pas
   Tests RBkgColorParams record: Reset, stream serialization, enum validation.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  Vcl.Graphics,
  LightCore.StreamBuff,
  LightVcl.Graph.BkgColorParams;

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
  FParams.Color:= clRed;
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
    Exception);
end;


procedure TTestBkgColorParams.TestWriteToStream_BasicWrite;
VAR Stream: TLightStream;
begin
  CreateTempFile;
  FParams.Reset;

  Stream:= TLightStream.CreateWrite(FTempFile);
  TRY
    Assert.WillNotRaise(
      procedure
      begin
        FParams.WriteToStream(Stream);
      end);
  FINALLY
    FreeAndNil(Stream);
  END;

  { Verify file was created and has content }
  Assert.IsTrue(TFile.Exists(FTempFile), 'Temp file should exist');
end;


{ Stream Read Tests }

procedure TTestBkgColorParams.TestReadFromStream_NilStream;
begin
  Assert.WillRaise(
    procedure
    begin
      FParams.ReadFromStream(NIL);
    end,
    Exception);
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
    Stream.WriteInteger(clBlack);     { Color }
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
      Exception);
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
    Stream.WriteInteger(clBlack);     { Color }
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
      Exception);
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
    Stream.WriteInteger(clBlack);     { Color }
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
      Exception);
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
      Exception);
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
  FParams.Color         := clRed;

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
  Assert.AreEqual(TColor(clRed), ReadParams.Color, 'Color should be clRed');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestBkgColorParams);

end.
