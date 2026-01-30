unit Test.LightVcl.Graph.FX.Rotate;

{=============================================================================================================
   Unit tests for LightVcl.Graph.FX.Rotate.pas
   Tests bitmap rotation functions and RSpatialParams record.

   Includes TestInsight support: define TESTINSIGHT in project options.

   Note: Tests for RotateExif and RotateBitmapJanFX require CCRExif to be defined.
   Tests for RotateBitmapGR32 require GR32 to be defined.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Math,
  Vcl.Graphics;

type
  [TestFixture]
  TTestGraphRotate = class
  private
    FBitmap: TBitmap;
    procedure FillBitmapWithColor(BMP: TBitmap; Color: TColor);
    function GetPixelAt(BMP: TBitmap; X, Y: Integer): TColor;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { RotateBitmap Tests - Main function }
    [Test]
    procedure TestRotateBitmap_NilBitmap;

    [Test]
    procedure TestRotateBitmap_ZeroDegrees;

    [Test]
    procedure TestRotateBitmap_90Degrees;

    [Test]
    procedure TestRotateBitmap_180Degrees;

    [Test]
    procedure TestRotateBitmap_270Degrees;

    [Test]
    procedure TestRotateBitmap_45Degrees_AdjustSize;

    [Test]
    procedure TestRotateBitmap_45Degrees_NoAdjustSize;

    { RotateBitmapGDI Tests }
    [Test]
    procedure TestRotateBitmapGDI_NilBitmap;

    [Test]
    procedure TestRotateBitmapGDI_BasicCall;

    [Test]
    procedure TestRotateBitmapGDI_90Degrees_AdjustSize;

    [Test]
    procedure TestRotateBitmapGDI_90Degrees_NoAdjustSize;

    [Test]
    procedure TestRotateBitmapGDI_WithBackgroundColor;

    { RotateBitmapSWT Tests }
    [Test]
    procedure TestRotateBitmapSWT_NilBitmap;

    [Test]
    procedure TestRotateBitmapSWT_BasicCall;

    [Test]
    procedure TestRotateBitmapSWT_90Degrees_AdjustSize;

    { RotateBitmapPLG Tests }
    [Test]
    procedure TestRotateBitmapPLG_NilBitmap;

    [Test]
    procedure TestRotateBitmapPLG_BasicCall;

    [Test]
    procedure TestRotateBitmapPLG_90Degrees_AdjustSize;

    { RotateBitmapBLT Tests }
    [Test]
    procedure TestRotateBitmapBLT_NilBitmap;

    [Test]
    procedure TestRotateBitmapBLT_BasicCall;

    [Test]
    procedure TestRotateBitmapBLT_90Degrees;

    { TRotateSense Tests }
    [Test]
    procedure TestTRotateSense_Values;

    { RSpatialParams Tests }
    [Test]
    procedure TestRSpatialParams_Reset;

    [Test]
    procedure TestRSpatialParams_Reset_DefaultFlip;

    [Test]
    procedure TestRSpatialParams_Reset_DefaultMirror;

    [Test]
    procedure TestRSpatialParams_Reset_DefaultRotation;

    { RSpatialParams Stream Tests }
    [Test]
    procedure TestRSpatialParams_WriteReadStream;

    [Test]
    procedure TestRSpatialParams_WriteReadStream_AllRotations;

    [Test]
    procedure TestRSpatialParams_ReadStream_InvalidRotation;

    { Rotation Dimension Tests }
    [Test]
    procedure TestRotation_90Degrees_SwapsDimensions;

    [Test]
    procedure TestRotation_180Degrees_PreservesDimensions;

    [Test]
    procedure TestRotation_45Degrees_IncreasesDimensions;
  end;

implementation

uses
  LightVcl.Graph.FX.Rotate, LightCore.StreamBuff;


procedure TTestGraphRotate.Setup;
begin
  FBitmap:= TBitmap.Create;
  FBitmap.Width:= 100;
  FBitmap.Height:= 80;
  FBitmap.PixelFormat:= pf24bit;
  FillBitmapWithColor(FBitmap, clWhite);
end;


procedure TTestGraphRotate.TearDown;
begin
  FreeAndNil(FBitmap);
end;


procedure TTestGraphRotate.FillBitmapWithColor(BMP: TBitmap; Color: TColor);
begin
  BMP.Canvas.Brush.Color:= Color;
  BMP.Canvas.FillRect(Rect(0, 0, BMP.Width, BMP.Height));
end;


function TTestGraphRotate.GetPixelAt(BMP: TBitmap; X, Y: Integer): TColor;
begin
  Result:= BMP.Canvas.Pixels[X, Y];
end;


{ RotateBitmap Tests }

procedure TTestGraphRotate.TestRotateBitmap_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      RotateBitmap(NIL, 90);
    end,
    EAssertionFailed);
end;


procedure TTestGraphRotate.TestRotateBitmap_ZeroDegrees;
VAR
  OrigWidth, OrigHeight: Integer;
begin
  OrigWidth:= FBitmap.Width;
  OrigHeight:= FBitmap.Height;

  RotateBitmap(FBitmap, 0);

  { Zero degrees should not change the bitmap }
  Assert.AreEqual(OrigWidth, FBitmap.Width, 'Width should not change');
  Assert.AreEqual(OrigHeight, FBitmap.Height, 'Height should not change');
end;


procedure TTestGraphRotate.TestRotateBitmap_90Degrees;
begin
  Assert.WillNotRaise(
    procedure
    begin
      RotateBitmap(FBitmap, 90);
    end);
end;


procedure TTestGraphRotate.TestRotateBitmap_180Degrees;
begin
  Assert.WillNotRaise(
    procedure
    begin
      RotateBitmap(FBitmap, 180);
    end);
end;


procedure TTestGraphRotate.TestRotateBitmap_270Degrees;
begin
  Assert.WillNotRaise(
    procedure
    begin
      RotateBitmap(FBitmap, 270);
    end);
end;


procedure TTestGraphRotate.TestRotateBitmap_45Degrees_AdjustSize;
begin
  Assert.WillNotRaise(
    procedure
    begin
      RotateBitmap(FBitmap, 45, TRUE);
    end);
end;


procedure TTestGraphRotate.TestRotateBitmap_45Degrees_NoAdjustSize;
VAR
  OrigWidth, OrigHeight: Integer;
begin
  OrigWidth:= FBitmap.Width;
  OrigHeight:= FBitmap.Height;

  RotateBitmap(FBitmap, 45, FALSE);

  { With AdjustSize=FALSE, dimensions should remain the same }
  Assert.AreEqual(OrigWidth, FBitmap.Width, 'Width should not change');
  Assert.AreEqual(OrigHeight, FBitmap.Height, 'Height should not change');
end;


{ RotateBitmapGDI Tests }

procedure TTestGraphRotate.TestRotateBitmapGDI_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      RotateBitmapGDI(NIL, 90);
    end,
    EAssertionFailed);
end;


procedure TTestGraphRotate.TestRotateBitmapGDI_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      RotateBitmapGDI(FBitmap, 45);
    end);
end;


procedure TTestGraphRotate.TestRotateBitmapGDI_90Degrees_AdjustSize;
VAR
  OrigWidth, OrigHeight: Integer;
begin
  OrigWidth:= FBitmap.Width;
  OrigHeight:= FBitmap.Height;

  RotateBitmapGDI(FBitmap, 90, TRUE);

  { 90 degree rotation with AdjustSize should swap dimensions }
  Assert.AreEqual(OrigHeight, FBitmap.Width, 'Width should equal original height');
  Assert.AreEqual(OrigWidth, FBitmap.Height, 'Height should equal original width');
end;


procedure TTestGraphRotate.TestRotateBitmapGDI_90Degrees_NoAdjustSize;
VAR
  OrigWidth, OrigHeight: Integer;
begin
  OrigWidth:= FBitmap.Width;
  OrigHeight:= FBitmap.Height;

  RotateBitmapGDI(FBitmap, 90, FALSE);

  { With AdjustSize=FALSE, dimensions should remain the same }
  Assert.AreEqual(OrigWidth, FBitmap.Width, 'Width should not change');
  Assert.AreEqual(OrigHeight, FBitmap.Height, 'Height should not change');
end;


procedure TTestGraphRotate.TestRotateBitmapGDI_WithBackgroundColor;
begin
  Assert.WillNotRaise(
    procedure
    begin
      RotateBitmapGDI(FBitmap, 45, TRUE, clBlue);
    end);
end;


{ RotateBitmapSWT Tests }

procedure TTestGraphRotate.TestRotateBitmapSWT_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      {$WARNINGS OFF}
      RotateBitmapSWT(NIL, DegToRad(90));
      {$WARNINGS ON}
    end,
    EAssertionFailed);
end;


procedure TTestGraphRotate.TestRotateBitmapSWT_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      {$WARNINGS OFF}
      RotateBitmapSWT(FBitmap, DegToRad(45));
      {$WARNINGS ON}
    end);
end;


procedure TTestGraphRotate.TestRotateBitmapSWT_90Degrees_AdjustSize;
begin
  Assert.WillNotRaise(
    procedure
    begin
      {$WARNINGS OFF}
      RotateBitmapSWT(FBitmap, DegToRad(90), TRUE);
      {$WARNINGS ON}
    end);
end;


{ RotateBitmapPLG Tests }

procedure TTestGraphRotate.TestRotateBitmapPLG_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      {$WARNINGS OFF}
      RotateBitmapPLG(NIL, DegToRad(90));
      {$WARNINGS ON}
    end,
    EAssertionFailed);
end;


procedure TTestGraphRotate.TestRotateBitmapPLG_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      {$WARNINGS OFF}
      RotateBitmapPLG(FBitmap, DegToRad(45));
      {$WARNINGS ON}
    end);
end;


procedure TTestGraphRotate.TestRotateBitmapPLG_90Degrees_AdjustSize;
begin
  Assert.WillNotRaise(
    procedure
    begin
      {$WARNINGS OFF}
      RotateBitmapPLG(FBitmap, DegToRad(90), TRUE);
      {$WARNINGS ON}
    end);
end;


{ RotateBitmapBLT Tests }

procedure TTestGraphRotate.TestRotateBitmapBLT_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      RotateBitmapBLT(NIL, DegToRad(90), 100, 80);
    end,
    EAssertionFailed);
end;


procedure TTestGraphRotate.TestRotateBitmapBLT_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      RotateBitmapBLT(FBitmap, DegToRad(45), 100, 80);
    end);
end;


procedure TTestGraphRotate.TestRotateBitmapBLT_90Degrees;
begin
  Assert.WillNotRaise(
    procedure
    begin
      RotateBitmapBLT(FBitmap, DegToRad(90), 100, 80);
    end);
end;


{ TRotateSense Tests }

procedure TTestGraphRotate.TestTRotateSense_Values;
begin
  { Verify enum values are correctly defined }
  Assert.AreEqual(0, Ord(rtNone), 'rtNone should be 0');
  Assert.AreEqual(1, Ord(rtLeft), 'rtLeft should be 1');
  Assert.AreEqual(2, Ord(rtRight), 'rtRight should be 2');
  Assert.AreEqual(3, Ord(rtExif), 'rtExif should be 3');
end;


{ RSpatialParams Tests }

procedure TTestGraphRotate.TestRSpatialParams_Reset;
VAR
  Params: RSpatialParams;
begin
  { Set some values first }
  Params.Flip:= TRUE;
  Params.Mirror:= TRUE;
  Params.Rotation:= rtLeft;

  { Reset should restore defaults }
  Params.Reset;

  Assert.IsFalse(Params.Flip, 'Flip should be FALSE after Reset');
  Assert.IsFalse(Params.Mirror, 'Mirror should be FALSE after Reset');
  Assert.AreEqual(rtExif, Params.Rotation, 'Rotation should be rtExif after Reset');
end;


procedure TTestGraphRotate.TestRSpatialParams_Reset_DefaultFlip;
VAR
  Params: RSpatialParams;
begin
  Params.Reset;
  Assert.IsFalse(Params.Flip, 'Default Flip should be FALSE');
end;


procedure TTestGraphRotate.TestRSpatialParams_Reset_DefaultMirror;
VAR
  Params: RSpatialParams;
begin
  Params.Reset;
  Assert.IsFalse(Params.Mirror, 'Default Mirror should be FALSE');
end;


procedure TTestGraphRotate.TestRSpatialParams_Reset_DefaultRotation;
VAR
  Params: RSpatialParams;
begin
  Params.Reset;
  Assert.AreEqual(rtExif, Params.Rotation, 'Default Rotation should be rtExif');
end;


{ RSpatialParams Stream Tests }

procedure TTestGraphRotate.TestRSpatialParams_WriteReadStream;
VAR
  WriteParams, ReadParams: RSpatialParams;
  TempFile: string;
  Stream: TLightStream;
begin
  TempFile:= GetEnvironmentVariable('TEMP') + '\TestSpatialParams.tmp';

  WriteParams.Flip:= TRUE;
  WriteParams.Mirror:= TRUE;
  WriteParams.Rotation:= rtRight;

  { Write to stream }
  Stream:= TLightStream.CreateWrite(TempFile);
  TRY
    WriteParams.WriteToStream(Stream);
  FINALLY
    FreeAndNil(Stream);
  END;

  { Read from stream }
  Stream:= TLightStream.CreateRead(TempFile);
  TRY
    ReadParams.ReadFromStream(Stream);
  FINALLY
    FreeAndNil(Stream);
  END;

  { Verify values }
  Assert.AreEqual(WriteParams.Flip, ReadParams.Flip, 'Flip should match');
  Assert.AreEqual(WriteParams.Mirror, ReadParams.Mirror, 'Mirror should match');
  Assert.AreEqual(WriteParams.Rotation, ReadParams.Rotation, 'Rotation should match');

  { Cleanup }
  if FileExists(TempFile)
  then DeleteFile(TempFile);
end;


procedure TTestGraphRotate.TestRSpatialParams_WriteReadStream_AllRotations;
VAR
  WriteParams, ReadParams: RSpatialParams;
  TempFile: string;
  Stream: TLightStream;
  Rot: TRotateSense;
begin
  TempFile:= GetEnvironmentVariable('TEMP') + '\TestSpatialParamsRot.tmp';

  { Test all rotation values }
  for Rot:= Low(TRotateSense) to High(TRotateSense) do
   begin
    WriteParams.Reset;
    WriteParams.Rotation:= Rot;

    Stream:= TLightStream.CreateWrite(TempFile);
    TRY
      WriteParams.WriteToStream(Stream);
    FINALLY
      FreeAndNil(Stream);
    END;

    Stream:= TLightStream.CreateRead(TempFile);
    TRY
      ReadParams.ReadFromStream(Stream);
    FINALLY
      FreeAndNil(Stream);
    END;

    Assert.AreEqual(Rot, ReadParams.Rotation, 'Rotation ' + IntToStr(Ord(Rot)) + ' should match');
   end;

  { Cleanup }
  if FileExists(TempFile)
  then DeleteFile(TempFile);
end;


procedure TTestGraphRotate.TestRSpatialParams_ReadStream_InvalidRotation;
VAR
  ReadParams: RSpatialParams;
  TempFile: string;
  Stream: TLightStream;
begin
  TempFile:= GetEnvironmentVariable('TEMP') + '\TestSpatialParamsInv.tmp';

  { Write invalid rotation value directly }
  Stream:= TLightStream.CreateWrite(TempFile);
  TRY
    Stream.WriteBoolean(FALSE);  { Flip }
    Stream.WriteBoolean(FALSE);  { Mirror }
    Stream.WriteByte(255);       { Invalid rotation value }
    Stream.WritePadding;
  FINALLY
    FreeAndNil(Stream);
  END;

  { Read should handle invalid value gracefully }
  Stream:= TLightStream.CreateRead(TempFile);
  TRY
    ReadParams.ReadFromStream(Stream);
  FINALLY
    FreeAndNil(Stream);
  END;

  { Invalid value should default to rtNone }
  Assert.AreEqual(rtNone, ReadParams.Rotation, 'Invalid rotation should default to rtNone');

  { Cleanup }
  if FileExists(TempFile)
  then DeleteFile(TempFile);
end;


{ Rotation Dimension Tests }

procedure TTestGraphRotate.TestRotation_90Degrees_SwapsDimensions;
VAR
  OrigWidth, OrigHeight: Integer;
begin
  OrigWidth:= FBitmap.Width;
  OrigHeight:= FBitmap.Height;

  RotateBitmapGDI(FBitmap, 90, TRUE);

  { 90 degree rotation should swap width and height }
  Assert.AreEqual(OrigHeight, FBitmap.Width, 'Width should equal original height after 90 degree rotation');
  Assert.AreEqual(OrigWidth, FBitmap.Height, 'Height should equal original width after 90 degree rotation');
end;


procedure TTestGraphRotate.TestRotation_180Degrees_PreservesDimensions;
VAR
  OrigWidth, OrigHeight: Integer;
begin
  OrigWidth:= FBitmap.Width;
  OrigHeight:= FBitmap.Height;

  RotateBitmapGDI(FBitmap, 180, TRUE);

  { 180 degree rotation should preserve dimensions }
  Assert.AreEqual(OrigWidth, FBitmap.Width, 'Width should be preserved after 180 degree rotation');
  Assert.AreEqual(OrigHeight, FBitmap.Height, 'Height should be preserved after 180 degree rotation');
end;


procedure TTestGraphRotate.TestRotation_45Degrees_IncreasesDimensions;
VAR
  OrigWidth, OrigHeight: Integer;
begin
  OrigWidth:= FBitmap.Width;
  OrigHeight:= FBitmap.Height;

  RotateBitmapGDI(FBitmap, 45, TRUE);

  { 45 degree rotation with AdjustSize should increase dimensions to fit rotated image }
  Assert.IsTrue(FBitmap.Width > OrigWidth, 'Width should increase after 45 degree rotation');
  Assert.IsTrue(FBitmap.Height > OrigHeight, 'Height should increase after 45 degree rotation');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestGraphRotate);

end.
