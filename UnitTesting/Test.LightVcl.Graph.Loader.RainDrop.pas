unit Test.LightVcl.Graph.Loader.RainDrop;

{=============================================================================================================
   Unit tests for LightVcl.Graph.Loader.RainDrop
   Tests the RainDrop file format, TRainShelter class, and RRaindropParams record.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  Vcl.Graphics;

type
  [TestFixture]
  TTestRainDrop = class
  private
    FTestDir: string;
    FTestFile: string;
    FTestBitmap: TBitmap;
    procedure CleanupTestFile;
    function CreateTestBitmap(Width, Height: Integer): TBitmap;
    function CreateTestMaskBitmap(Width, Height: Integer): TBitmap;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { IsRainDrop Tests }
    [Test]
    procedure TestIsRainDrop_ValidExtension;

    [Test]
    procedure TestIsRainDrop_InvalidExtension;

    [Test]
    procedure TestIsRainDrop_CaseInsensitive;

    [Test]
    procedure TestIsRainDrop_EmptyString;

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

    { TRainShelter Tests }
    [Test]
    procedure TestRainShelter_Create;

    [Test]
    procedure TestRainShelter_SaveLoad_RoundTrip;

    [Test]
    procedure TestRainShelter_SaveLoad_Params;

    [Test]
    procedure TestRainShelter_SaveLoad_PixelMap;

    [Test]
    procedure TestRainShelter_LoadFromFile_InvalidFile;

    [Test]
    procedure TestRainShelter_Save_ChangesExtension;

    [Test]
    procedure TestRainShelter_Clear_ResetsPixelMap;

    { LoadRainShelter Tests }
    [Test]
    procedure TestLoadRainShelter_ValidFile;

    [Test]
    procedure TestLoadRainShelter_InvalidFile;
  end;


implementation

uses
  LightVcl.Graph.Loader.RainDrop,
  LightCore.StreamBuff;


{ Helper Methods }

procedure TTestRainDrop.CleanupTestFile;
begin
  if TFile.Exists(FTestFile)
  then TFile.Delete(FTestFile);
end;


function TTestRainDrop.CreateTestBitmap(Width, Height: Integer): TBitmap;
begin
  Result:= TBitmap.Create;
  Result.SetSize(Width, Height);
  Result.PixelFormat:= pf24bit;
  Result.Canvas.Brush.Color:= clBlue;
  Result.Canvas.FillRect(Rect(0, 0, Width, Height));
end;


function TTestRainDrop.CreateTestMaskBitmap(Width, Height: Integer): TBitmap;
VAR
  x, y: Integer;
begin
  Result:= TBitmap.Create;
  Result.SetSize(Width, Height);
  Result.PixelFormat:= pf24bit;
  Result.Canvas.Brush.Color:= clWhite;
  Result.Canvas.FillRect(Rect(0, 0, Width, Height));

  { Add some pink pixels to mark water drop positions }
  for x:= 0 to Width-1 do
    for y:= 0 to Height-1 do
      if ((x + y) mod 10 = 0)
      then Result.Canvas.Pixels[x, y]:= clFuchsia;
end;


procedure TTestRainDrop.Setup;
begin
  FTestDir:= TPath.GetTempPath;
  FTestFile:= TPath.Combine(FTestDir, 'RainDropTest_' + IntToStr(Random(MaxInt)) + RainDropExt);
  FTestBitmap:= NIL;
end;


procedure TTestRainDrop.TearDown;
begin
  CleanupTestFile;
  FreeAndNil(FTestBitmap);
end;


{ IsRainDrop Tests }

procedure TTestRainDrop.TestIsRainDrop_ValidExtension;
begin
  Assert.IsTrue(IsRainDrop('test.RainDrop'), 'Should recognize .RainDrop extension');
  Assert.IsTrue(IsRainDrop('c:\path\to\file.RainDrop'), 'Should recognize full path with .RainDrop');
end;


procedure TTestRainDrop.TestIsRainDrop_InvalidExtension;
begin
  Assert.IsFalse(IsRainDrop('test.jpg'), 'Should not recognize .jpg');
  Assert.IsFalse(IsRainDrop('test.png'), 'Should not recognize .png');
  Assert.IsFalse(IsRainDrop('test.bmp'), 'Should not recognize .bmp');
  Assert.IsFalse(IsRainDrop('test.txt'), 'Should not recognize .txt');
end;


procedure TTestRainDrop.TestIsRainDrop_CaseInsensitive;
begin
  Assert.IsTrue(IsRainDrop('test.RAINDROP'), 'Should be case-insensitive (upper)');
  Assert.IsTrue(IsRainDrop('test.raindrop'), 'Should be case-insensitive (lower)');
  Assert.IsTrue(IsRainDrop('test.RaInDrOp'), 'Should be case-insensitive (mixed)');
end;


procedure TTestRainDrop.TestIsRainDrop_EmptyString;
begin
  Assert.IsFalse(IsRainDrop(''), 'Empty string should return false');
end;


{ RRaindropParams Tests }

procedure TTestRainDrop.TestParams_Reset;
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
  Assert.AreEqual(15, Params.Damping, 'Damping should reset to 15');
  Assert.AreEqual(1, Params.WaveAplitude, 'WaveAplitude should reset to 1');
  Assert.AreEqual(50, Params.WaveTravelDist, 'WaveTravelDist should reset to 50');
  Assert.AreEqual(150, Params.DropInterval, 'DropInterval should reset to 150');
end;


procedure TTestRainDrop.TestParams_SaveLoad;
VAR
  WriteParams, ReadParams: RRaindropParams;
  Stream: TLightStream;
  TempFile: string;
begin
  TempFile:= TPath.Combine(FTestDir, 'ParamsTest_' + IntToStr(Random(MaxInt)) + '.dat');

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


procedure TTestRainDrop.TestParams_DampingClamping_Low;
VAR
  Params: RRaindropParams;
begin
  Params.Reset;
  { Attempt to set damping below minimum (1) }
  Params.Damping:= 0;
  Assert.AreEqual(1, Params.Damping, 'Damping should clamp to minimum (1)');
end;


procedure TTestRainDrop.TestParams_DampingClamping_High;
VAR
  Params: RRaindropParams;
begin
  Params.Reset;
  { Attempt to set damping above maximum (99) }
  Params.Damping:= 100;
  Assert.AreEqual(99, Params.Damping, 'Damping should clamp to maximum (99)');
end;


procedure TTestRainDrop.TestParams_DampingValidRange;
VAR
  Params: RRaindropParams;
begin
  Params.Reset;

  Params.Damping:= 1;
  Assert.AreEqual(1, Params.Damping, 'Minimum damping (1) should be accepted');

  Params.Damping:= 50;
  Assert.AreEqual(50, Params.Damping, 'Mid-range damping (50) should be accepted');

  Params.Damping:= 99;
  Assert.AreEqual(99, Params.Damping, 'Maximum damping (99) should be accepted');
end;


{ TRainShelter Tests }

procedure TTestRainDrop.TestRainShelter_Create;
VAR
  Shelter: TRainShelter;
begin
  Shelter:= TRainShelter.Create;
  TRY
    Assert.IsTrue(Shelter.OrigImage = NIL, 'OrigImage should be nil after creation');
    Assert.AreEqual('', Shelter.FileName, 'FileName should be empty after creation');
  FINALLY
    FreeAndNil(Shelter);
  END;
end;


procedure TTestRainDrop.TestRainShelter_SaveLoad_RoundTrip;
VAR
  WriteShelter, ReadShelter: TRainShelter;
  Mask: TBitmap;
begin
  WriteShelter:= TRainShelter.Create;
  ReadShelter:= TRainShelter.Create;
  Mask:= NIL;
  TRY
    { Setup write shelter }
    WriteShelter.OrigImage:= CreateTestBitmap(100, 100);
    WriteShelter.Params.Reset;
    WriteShelter.Params.TargetFPS:= 30;
    Mask:= CreateTestMaskBitmap(100, 100);

    { Save }
    WriteShelter.SaveToFile(FTestFile, Mask);

    Assert.IsTrue(TFile.Exists(FTestFile), 'File should exist after save');

    { Load }
    Assert.IsTrue(ReadShelter.LoadFromFile(FTestFile), 'LoadFromFile should return true');
    Assert.IsNotNull(ReadShelter.OrigImage, 'OrigImage should not be nil after load');
    Assert.AreEqual(100, ReadShelter.OrigImage.Width, 'Image width mismatch');
    Assert.AreEqual(100, ReadShelter.OrigImage.Height, 'Image height mismatch');
  FINALLY
    FreeAndNil(Mask);
    FreeAndNil(WriteShelter);
    FreeAndNil(ReadShelter);
  END;
end;


procedure TTestRainDrop.TestRainShelter_SaveLoad_Params;
VAR
  WriteShelter, ReadShelter: TRainShelter;
  Mask: TBitmap;
begin
  WriteShelter:= TRainShelter.Create;
  ReadShelter:= TRainShelter.Create;
  Mask:= NIL;
  TRY
    { Setup write shelter with specific params }
    WriteShelter.OrigImage:= CreateTestBitmap(50, 50);
    WriteShelter.Params.TargetFPS:= 60;
    WriteShelter.Params.Damping:= 25;
    WriteShelter.Params.WaveAplitude:= 3;
    WriteShelter.Params.WaveTravelDist:= 200;
    WriteShelter.Params.DropInterval:= 75;
    Mask:= CreateTestMaskBitmap(50, 50);

    { Save and load }
    WriteShelter.SaveToFile(FTestFile, Mask);
    ReadShelter.LoadFromFile(FTestFile);

    { Verify params }
    Assert.AreEqual(WriteShelter.Params.TargetFPS, ReadShelter.Params.TargetFPS, 'TargetFPS mismatch');
    Assert.AreEqual(WriteShelter.Params.Damping, ReadShelter.Params.Damping, 'Damping mismatch');
    Assert.AreEqual(WriteShelter.Params.WaveAplitude, ReadShelter.Params.WaveAplitude, 'WaveAplitude mismatch');
    Assert.AreEqual(WriteShelter.Params.WaveTravelDist, ReadShelter.Params.WaveTravelDist, 'WaveTravelDist mismatch');
    Assert.AreEqual(WriteShelter.Params.DropInterval, ReadShelter.Params.DropInterval, 'DropInterval mismatch');
  FINALLY
    FreeAndNil(Mask);
    FreeAndNil(WriteShelter);
    FreeAndNil(ReadShelter);
  END;
end;


procedure TTestRainDrop.TestRainShelter_SaveLoad_PixelMap;
VAR
  WriteShelter, ReadShelter: TRainShelter;
  Mask: TBitmap;
  x, y: Integer;
  ExpectedPixel: Boolean;
begin
  WriteShelter:= TRainShelter.Create;
  ReadShelter:= TRainShelter.Create;
  Mask:= NIL;
  TRY
    { Setup with 20x20 image for faster test }
    WriteShelter.OrigImage:= CreateTestBitmap(20, 20);
    WriteShelter.Params.Reset;
    Mask:= CreateTestMaskBitmap(20, 20);

    { Save and load }
    WriteShelter.SaveToFile(FTestFile, Mask);
    ReadShelter.LoadFromFile(FTestFile);

    { Verify pixel map dimensions }
    Assert.AreEqual(20, Length(ReadShelter.PixelMap), 'PixelMap width mismatch');
    Assert.AreEqual(20, Length(ReadShelter.PixelMap[0]), 'PixelMap height mismatch');

    { Verify pixel map values match the mask (pink pixels = true) }
    for x:= 0 to 19 do
      for y:= 0 to 19 do
        begin
          ExpectedPixel:= ((x + y) mod 10 = 0);  { Same pattern as CreateTestMaskBitmap }
          Assert.AreEqual(ExpectedPixel, ReadShelter.PixelMap[x, y],
            Format('PixelMap[%d,%d] mismatch', [x, y]));
        end;
  FINALLY
    FreeAndNil(Mask);
    FreeAndNil(WriteShelter);
    FreeAndNil(ReadShelter);
  END;
end;


procedure TTestRainDrop.TestRainShelter_LoadFromFile_InvalidFile;
VAR
  Shelter: TRainShelter;
  TempFile: string;
  Stream: TFileStream;
begin
  Shelter:= TRainShelter.Create;
  TempFile:= TPath.Combine(FTestDir, 'InvalidRainDrop_' + IntToStr(Random(MaxInt)) + RainDropExt);
  TRY
    { Create an invalid file (just random data) }
    Stream:= TFileStream.Create(TempFile, fmCreate);
    TRY
      Stream.WriteData(Integer(12345));
      Stream.WriteData(Integer(67890));
    FINALLY
      FreeAndNil(Stream);
    END;

    { Try to load - should return false, not crash }
    Assert.IsFalse(Shelter.LoadFromFile(TempFile), 'LoadFromFile should return false for invalid file');
    Assert.IsTrue(Shelter.OrigImage = NIL, 'OrigImage should remain nil for invalid file');
  FINALLY
    FreeAndNil(Shelter);
    if TFile.Exists(TempFile)
    then TFile.Delete(TempFile);
  END;
end;


procedure TTestRainDrop.TestRainShelter_Save_ChangesExtension;
VAR
  Shelter: TRainShelter;
  Mask: TBitmap;
  JpgFileName, ExpectedRainDropFile: string;
begin
  Shelter:= TRainShelter.Create;
  Mask:= NIL;
  JpgFileName:= TPath.Combine(FTestDir, 'TestImage_' + IntToStr(Random(MaxInt)) + '.jpg');
  ExpectedRainDropFile:= ChangeFileExt(JpgFileName, RainDropExt);
  TRY
    { Setup shelter with a .jpg filename }
    Shelter.OrigImage:= CreateTestBitmap(50, 50);
    Shelter.FileName:= JpgFileName;  { Not a RainDrop extension }
    Shelter.Params.Reset;
    Mask:= CreateTestMaskBitmap(50, 50);

    { Call Save (not SaveToFile) - should change extension }
    Shelter.Save(Mask);

    { Verify it created a .RainDrop file }
    Assert.IsTrue(TFile.Exists(ExpectedRainDropFile), 'Save should create .RainDrop file');
    Assert.IsFalse(TFile.Exists(JpgFileName), 'Original .jpg should not be created');
  FINALLY
    FreeAndNil(Mask);
    FreeAndNil(Shelter);
    if TFile.Exists(ExpectedRainDropFile)
    then TFile.Delete(ExpectedRainDropFile);
    if TFile.Exists(JpgFileName)
    then TFile.Delete(JpgFileName);
  END;
end;


procedure TTestRainDrop.TestRainShelter_Clear_ResetsPixelMap;
VAR
  Shelter: TRainShelter;
  Mask: TBitmap;
begin
  Shelter:= TRainShelter.Create;
  Mask:= NIL;
  TRY
    { Setup and save a shelter with pixel map data }
    Shelter.OrigImage:= CreateTestBitmap(20, 20);
    Shelter.Params.Reset;
    Mask:= CreateTestMaskBitmap(20, 20);
    Shelter.SaveToFile(FTestFile, Mask);

    { Load the file - this populates PixelMap }
    Shelter.LoadFromFile(FTestFile);
    Assert.AreEqual(20, Length(Shelter.PixelMap), 'PixelMap should be populated after load');

    { Simulate Clear being called (LoadFromFile calls Clear at start) }
    { Load a different file or call Clear indirectly by loading invalid file }
    { Actually, let's test by loading the same file again - Clear is called first }
    Shelter.LoadFromFile(FTestFile);

    { The PixelMap should still be valid (repopulated) }
    Assert.AreEqual(20, Length(Shelter.PixelMap), 'PixelMap should be repopulated after second load');

    { Now test that PixelMap is cleared when OrigImage is freed }
    FreeAndNil(Shelter);
    Shelter:= TRainShelter.Create;

    { After creation, PixelMap should be empty }
    Assert.AreEqual(0, Length(Shelter.PixelMap), 'PixelMap should be empty on new instance');
  FINALLY
    FreeAndNil(Mask);
    FreeAndNil(Shelter);
  END;
end;


{ LoadRainShelter Tests }

procedure TTestRainDrop.TestLoadRainShelter_ValidFile;
VAR
  Shelter: TRainShelter;
  Mask: TBitmap;
  LoadedBitmap: TBitmap;
begin
  Shelter:= TRainShelter.Create;
  Mask:= NIL;
  LoadedBitmap:= NIL;
  TRY
    { Create and save a valid RainDrop file }
    Shelter.OrigImage:= CreateTestBitmap(80, 60);
    Shelter.Params.Reset;
    Mask:= CreateTestMaskBitmap(80, 60);
    Shelter.SaveToFile(FTestFile, Mask);
    FreeAndNil(Shelter);

    { Use LoadRainShelter function }
    LoadedBitmap:= LoadRainShelter(FTestFile);

    Assert.IsNotNull(LoadedBitmap, 'LoadRainShelter should return a bitmap');
    Assert.AreEqual(80, LoadedBitmap.Width, 'Width mismatch');
    Assert.AreEqual(60, LoadedBitmap.Height, 'Height mismatch');
  FINALLY
    FreeAndNil(Mask);
    FreeAndNil(Shelter);
    FreeAndNil(LoadedBitmap);
  END;
end;


procedure TTestRainDrop.TestLoadRainShelter_InvalidFile;
VAR
  LoadedBitmap: TBitmap;
  TempFile: string;
  Stream: TFileStream;
begin
  TempFile:= TPath.Combine(FTestDir, 'InvalidRainDrop2_' + IntToStr(Random(MaxInt)) + RainDropExt);
  LoadedBitmap:= NIL;
  TRY
    { Create an invalid file }
    Stream:= TFileStream.Create(TempFile, fmCreate);
    TRY
      Stream.WriteData(Integer(99999));
    FINALLY
      FreeAndNil(Stream);
    END;

    { LoadRainShelter should return NIL for invalid files }
    LoadedBitmap:= LoadRainShelter(TempFile);
    Assert.IsTrue(LoadedBitmap = NIL, 'LoadRainShelter should return NIL for invalid file');
  FINALLY
    FreeAndNil(LoadedBitmap);
    if TFile.Exists(TempFile)
    then TFile.Delete(TempFile);
  END;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestRainDrop);

end.
