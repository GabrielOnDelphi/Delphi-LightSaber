unit Test.LightCore.Graph.RainDropParams;

{=============================================================================================================
   Unit tests for LightCore.Graph.RainDropParams.pas
   Tests the RRaindropParams record: Reset, stream Save/Load, the Damping range and the Damping clamp in Load.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils;

type
  [TestFixture]
  TTestRainDropParams = class
  private
    FTestDir: string;
    procedure WriteRawParams(CONST FileName: string; CONST RawDamping: Integer);
  public
    [Setup]
    procedure Setup;

    { RRaindropParams Tests }
    [Test]
    procedure TestParams_Reset;

    [Test]
    procedure TestParams_SaveLoad;

    [Test]
    procedure TestParams_DampingClamping_Low;

    [Test]
    procedure TestParams_DampingClamping_High;

    [Test]
    procedure TestParams_DampingValidRange;
  end;


implementation

uses
  LightCore.Graph.RainDropParams,
  LightCore.StreamBuff;


procedure TTestRainDropParams.Setup;
begin
  FTestDir:= TPath.GetTempPath;
end;


{ RRaindropParams Tests }

procedure TTestRainDropParams.TestParams_Reset;
VAR
  Params: RRaindropParams;
begin
  { Initialize with non-default values }
  Params.TargetFPS:= 999;
  Params.WaveAplitude:= 999;
  Params.WaveTravelDist:= 999;
  Params.DropInterval:= 999;

  Params.Reset;

  Assert.AreEqual(24, Params.TargetFPS, 'TargetFPS should reset to 24');
  Assert.AreEqual(15, Integer(Params.Damping), 'Damping should reset to 15');
  Assert.AreEqual(1, Params.WaveAplitude, 'WaveAplitude should reset to 1');
  Assert.AreEqual(50, Params.WaveTravelDist, 'WaveTravelDist should reset to 50');
  Assert.AreEqual(150, Params.DropInterval, 'DropInterval should reset to 150');
end;


procedure TTestRainDropParams.TestParams_SaveLoad;
VAR
  WriteParams, ReadParams: RRaindropParams;
  Stream: TLightStream;
  TempFile: string;
begin
  TempFile:= TPath.Combine(FTestDir, 'ParamsTest_' + IntToStr(Random(MaxInt)) + '.dat');

  WriteParams.Reset;   { without this, AdvancedMode / MouseDrops / MouseDropInterv are whatever was on the stack }
  WriteParams.TargetFPS:= 30;
  WriteParams.Damping:= 25;
  WriteParams.WaveAplitude:= 5;
  WriteParams.WaveTravelDist:= 500;
  WriteParams.DropInterval:= 100;

  { Write params }
  Stream:= TLightStream.CreateWrite(TempFile);
  TRY
    WriteParams.Save(Stream);
  FINALLY
    FreeAndNil(Stream);
  END;

  { Read params }
  ReadParams.Reset;
  Stream:= TLightStream.CreateRead(TempFile);
  TRY
    ReadParams.Load(Stream);
  FINALLY
    FreeAndNil(Stream);
  END;

  Assert.AreEqual(WriteParams.TargetFPS, ReadParams.TargetFPS, 'TargetFPS mismatch');
  Assert.AreEqual(WriteParams.Damping, ReadParams.Damping, 'Damping mismatch');
  Assert.AreEqual(WriteParams.WaveAplitude, ReadParams.WaveAplitude, 'WaveAplitude mismatch');
  Assert.AreEqual(WriteParams.WaveTravelDist, ReadParams.WaveTravelDist, 'WaveTravelDist mismatch');
  Assert.AreEqual(WriteParams.DropInterval, ReadParams.DropInterval, 'DropInterval mismatch');

  TFile.Delete(TempFile);
end;


{ Writes one RRaindropParams record to a stream field by field, so a test can put a value in it
  that the record's own Save could never produce. Damping is the FIRST integer Save writes
  (LightCore.Graph.RainDropParams.pas, RRaindropParams.Save), so the field order below must stay
  exactly as it is or Load reads the wrong field. }
procedure TTestRainDropParams.WriteRawParams(CONST FileName: string; CONST RawDamping: Integer);
VAR Stream: TLightStream;
begin
  Stream:= TLightStream.CreateWrite(FileName);
  TRY
    Stream.WriteInteger(RawDamping);      { Damping }
    Stream.WriteInteger(30);              { TargetFPS }
    Stream.WriteInteger(5);               { WaveAplitude }
    Stream.WriteInteger(500);             { WaveTravelDist }
    Stream.WriteInteger(100);             { DropInterval }
    Stream.WriteBoolean(FALSE);           { AdvancedMode }
    Stream.WriteBoolean(FALSE);           { MouseDrops }
    Stream.WriteInteger(500);             { MouseDropInterv }
    Stream.WritePaddingValidation;
  FINALLY
    FreeAndNil(Stream);
  END;
end;


{ Damping is a plain FIELD of type TWaterDamping = 1..99, not a property, so writing 0 into it
  clamps nothing - the compiler refuses the out-of-range constant outright. The only clamp in the
  record is in Load: "Damping := EnsureRange(Stream.ReadInteger, 1, 99)"
  (LightCore.Graph.RainDropParams.pas). So the value has to arrive from a stream. }
procedure TTestRainDropParams.TestParams_DampingClamping_Low;
VAR
  Params: RRaindropParams;
  Stream: TLightStream;
  TempFile: string;
begin
  TempFile:= TPath.Combine(FTestDir, 'DampingLow_' + IntToStr(Random(MaxInt)) + '.dat');
  TRY
    WriteRawParams(TempFile, 0);          { below the 1..99 range }

    Params.Reset;
    Stream:= TLightStream.CreateRead(TempFile);
    TRY
      Params.Load(Stream);
    FINALLY
      FreeAndNil(Stream);
    END;

    Assert.AreEqual(1, Integer(Params.Damping), 'Damping should clamp to minimum (1)');
  FINALLY
    if TFile.Exists(TempFile)
    then TFile.Delete(TempFile);
  END;
end;


procedure TTestRainDropParams.TestParams_DampingClamping_High;
VAR
  Params: RRaindropParams;
  Stream: TLightStream;
  TempFile: string;
begin
  TempFile:= TPath.Combine(FTestDir, 'DampingHigh_' + IntToStr(Random(MaxInt)) + '.dat');
  TRY
    WriteRawParams(TempFile, 100);        { above the 1..99 range }

    Params.Reset;
    Stream:= TLightStream.CreateRead(TempFile);
    TRY
      Params.Load(Stream);
    FINALLY
      FreeAndNil(Stream);
    END;

    Assert.AreEqual(99, Integer(Params.Damping), 'Damping should clamp to maximum (99)');
  FINALLY
    if TFile.Exists(TempFile)
    then TFile.Delete(TempFile);
  END;
end;


procedure TTestRainDropParams.TestParams_DampingValidRange;
VAR
  Params: RRaindropParams;
begin
  Params.Reset;

  Params.Damping:= 1;
  Assert.AreEqual(1, Integer(Params.Damping), 'Minimum damping (1) should be accepted');

  Params.Damping:= 50;
  Assert.AreEqual(50, Integer(Params.Damping), 'Mid-range damping (50) should be accepted');

  Params.Damping:= 99;
  Assert.AreEqual(99, Integer(Params.Damping), 'Maximum damping (99) should be accepted');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestRainDropParams);

end.
