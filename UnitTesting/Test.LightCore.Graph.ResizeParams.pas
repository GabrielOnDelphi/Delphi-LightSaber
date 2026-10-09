unit Test.LightCore.Graph.ResizeParams;

{=============================================================================================================
   Unit tests for LightCore.Graph.ResizeParams.pas
   Tests RResizeParams record functionality including Reset, ComputeOutputSize, and stream I/O.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  LightCore.Graph.ResizeParams;

type
  [TestFixture]
  TTestResizeParams = class
  private
    procedure Poison(VAR Params: RResizeParams);
    procedure CheckPersistedOp(Op: TResizeOp; ExpectedByte: Byte);
  public
    { Reset Tests }
    [Test]
    procedure TestReset_DefaultValues;

    [Test]
    procedure TestReset_OutWOutH_Uninitialized;

    { ComputeOutputSize Validation Tests }
    [Test]
    procedure TestComputeOutputSize_InvalidMaxWidth;

    [Test]
    procedure TestComputeOutputSize_InvalidMaxHeight;

    [Test]
    procedure TestComputeOutputSize_InvalidInputWidth;

    [Test]
    procedure TestComputeOutputSize_InvalidInputHeight;

    { roNone Mode Tests }
    [Test]
    procedure TestComputeOutputSize_roNone_NoChange;

    { roStretch Mode Tests }
    [Test]
    procedure TestComputeOutputSize_roStretch_ExactDimensions;

    { roFit Mode Tests }
    [Test]
    procedure TestComputeOutputSize_roFit_LandscapeImage;

    [Test]
    procedure TestComputeOutputSize_roFit_PortraitImage;

    [Test]
    procedure TestComputeOutputSize_roFit_SameAspect;

    [Test]
    procedure TestComputeOutputSize_roFit_NoExceedViewport;

    { roFill Mode Tests }
    [Test]
    procedure TestComputeOutputSize_roFill_LandscapeImage;

    [Test]
    procedure TestComputeOutputSize_roFill_PortraitImage;

    [Test]
    procedure TestComputeOutputSize_roFill_FillsViewport;

    { roCustom Mode Tests }
    [Test]
    procedure TestComputeOutputSize_roCustom_ZoomIn;

    [Test]
    procedure TestComputeOutputSize_roCustom_ZoomOut;

    [Test]
    procedure TestComputeOutputSize_roCustom_InvalidZoom;

    { roForceWidth Mode Tests }
    [Test]
    procedure TestComputeOutputSize_roForceWidth_BasicCall;

    [Test]
    procedure TestComputeOutputSize_roForceWidth_MaintainsAspect;

    [Test]
    procedure TestComputeOutputSize_roForceWidth_InvalidWidth;

    { roForceHeight Mode Tests }
    [Test]
    procedure TestComputeOutputSize_roForceHeight_BasicCall;

    [Test]
    procedure TestComputeOutputSize_roForceHeight_MaintainsAspect;

    [Test]
    procedure TestComputeOutputSize_roForceHeight_InvalidHeight;

    { roAutoDetect Mode Tests }
    [Test]
    procedure TestComputeOutputSize_roAutoDetect_SameDimensions;

    [Test]
    procedure TestComputeOutputSize_roAutoDetect_ProducesValidOutput;

    { TResizeOp Enum Tests }
    [Test]
    procedure TestTResizeOp_EnumValues;
  end;

implementation

uses
  System.IOUtils,
  LightCore.StreamBuff;


{ Fills every field with a value that differs from the one Reset writes, so a field that Reset forgets keeps the wrong value }
procedure TTestResizeParams.Poison(VAR Params: RResizeParams);
begin
  Params.ResizeOpp    := roStretch;
  Params.MaxZoomVal   := -3;
  Params.MaxZoomUse   := FALSE;
  Params.CustomZoom   := 7.25;
  Params.MaxWidth     := 11;
  Params.MaxHeight    := 13;
  Params.FitTolerance := 77;
  Params.ResizePanoram:= TRUE;
  Params.ForcedWidth  := 17;
  Params.ForcedHeight := 19;
  Params.OutW         := 23;
  Params.OutH         := 29;
end;


{ Writes a record whose ResizeOpp is Op, checks the size and the first byte of the file, then reads it back.
  The byte is what files saved by earlier builds hold, so it must never change. }
procedure TTestResizeParams.CheckPersistedOp(Op: TResizeOp; ExpectedByte: Byte);
CONST
  { WriteToStream: Byte + Boolean + Integer + Single + Integer + Integer + Boolean + Byte = 1+1+4+4+4+4+1+1 = 20 bytes, plus the 64-byte validation padding (TLightStream.FrozenPaddingSize) }
  RecordSize = 84;
VAR
  FileName: string;
  Stream: TLightStream;
  Written, ReadBack: RResizeParams;
  Bytes: TBytes;
begin
  FileName:= TPath.GetTempFileName;
  TRY
    Written.Reset;
    Written.ResizeOpp:= Op;
    Stream:= TLightStream.CreateWrite(FileName);
    TRY
      Written.WriteToStream(Stream);
    FINALLY
      FreeAndNil(Stream);
    END;

    Bytes:= TFile.ReadAllBytes(FileName);
    Assert.AreEqual(RecordSize, Length(Bytes), 'Size of the saved record');
    Assert.AreEqual(ExpectedByte, Bytes[0], 'Persisted byte of the resize mode ' + IntToStr(Ord(Op)));

    Poison(ReadBack);
    if Op = roStretch
    then ReadBack.ResizeOpp:= roNone;   { Poison sets roStretch; the read must overwrite it }
    Stream:= TLightStream.CreateRead(FileName);
    TRY
      ReadBack.ReadFromStream(Stream);
    FINALLY
      FreeAndNil(Stream);
    END;
    Assert.AreEqual(Op, ReadBack.ResizeOpp, 'ReadFromStream must restore the resize mode ' + IntToStr(ExpectedByte));
  FINALLY
    if TFile.Exists(FileName)
    then TFile.Delete(FileName);
  END;
end;


{ Reset Tests }

procedure TTestResizeParams.TestReset_DefaultValues;
var
  Params: RResizeParams;
begin
  Poison(Params);
  Params.Reset;

  Assert.AreEqual(roAutoDetect, Params.ResizeOpp, 'Default ResizeOpp should be roAutoDetect');
  Assert.AreEqual(50, Params.MaxZoomVal, 'Default MaxZoomVal should be 50');
  Assert.IsTrue(Params.MaxZoomUse, 'Default MaxZoomUse should be TRUE');
  Assert.AreEqual(Single(1.5), Params.CustomZoom, 'Default CustomZoom should be 1.5');
  Assert.AreEqual(1920, Params.MaxWidth, 'Default MaxWidth should be 1920');
  Assert.AreEqual(1200, Params.MaxHeight, 'Default MaxHeight should be 1200');
  Assert.AreEqual(Byte(10), Params.FitTolerance, 'Default FitTolerance should be 10');
  Assert.IsFalse(Params.ResizePanoram, 'Default ResizePanoram should be FALSE');
  Assert.AreEqual(800, Params.ForcedWidth, 'Default ForcedWidth should be 800');
  Assert.AreEqual(600, Params.ForcedHeight, 'Default ForcedHeight should be 600');
end;


procedure TTestResizeParams.TestReset_OutWOutH_Uninitialized;
var
  Params: RResizeParams;
begin
  Poison(Params);
  Params.Reset;

  Assert.AreEqual(UNINITIALIZED_SIZE, Params.OutW, 'OutW should be UNINITIALIZED_SIZE after Reset');
  Assert.AreEqual(UNINITIALIZED_SIZE, Params.OutH, 'OutH should be UNINITIALIZED_SIZE after Reset');
end;


{ ComputeOutputSize Validation Tests }

procedure TTestResizeParams.TestComputeOutputSize_InvalidMaxWidth;
var
  Params: RResizeParams;
begin
  Params.Reset;
  Params.MaxWidth:= 0;

  Assert.WillRaise(
    procedure
    begin
      Params.ComputeOutputSize(100, 100);
    end,
    Exception,
    'Params.ComputeOutputSize(100, 100) must raise Exception');
end;


procedure TTestResizeParams.TestComputeOutputSize_InvalidMaxHeight;
var
  Params: RResizeParams;
begin
  Params.Reset;
  Params.MaxHeight:= -1;

  Assert.WillRaise(
    procedure
    begin
      Params.ComputeOutputSize(100, 100);
    end,
    Exception,
    'Params.ComputeOutputSize(100, 100) must raise Exception');
end;


procedure TTestResizeParams.TestComputeOutputSize_InvalidInputWidth;
var
  Params: RResizeParams;
begin
  Params.Reset;

  Assert.WillRaise(
    procedure
    begin
      Params.ComputeOutputSize(0, 100);
    end,
    Exception,
    'Params.ComputeOutputSize(0, 100) must raise Exception');
end;


procedure TTestResizeParams.TestComputeOutputSize_InvalidInputHeight;
var
  Params: RResizeParams;
begin
  Params.Reset;

  Assert.WillRaise(
    procedure
    begin
      Params.ComputeOutputSize(100, -1);
    end,
    Exception,
    'Params.ComputeOutputSize(100, -1) must raise Exception');
end;


{ roNone Mode Tests }

procedure TTestResizeParams.TestComputeOutputSize_roNone_NoChange;
var
  Params: RResizeParams;
begin
  Params.Reset;
  Params.ResizeOpp:= roNone;

  Params.ComputeOutputSize(400, 300);

  Assert.AreEqual(400, Params.OutW, 'roNone should preserve width');
  Assert.AreEqual(300, Params.OutH, 'roNone should preserve height');
end;


{ roStretch Mode Tests }

procedure TTestResizeParams.TestComputeOutputSize_roStretch_ExactDimensions;
var
  Params: RResizeParams;
begin
  Params.Reset;
  Params.ResizeOpp:= roStretch;
  Params.MaxWidth:= 800;
  Params.MaxHeight:= 600;

  Params.ComputeOutputSize(400, 300);

  Assert.AreEqual(800, Params.OutW, 'roStretch should output exact MaxWidth');
  Assert.AreEqual(600, Params.OutH, 'roStretch should output exact MaxHeight');
end;


{ roFit Mode Tests }

procedure TTestResizeParams.TestComputeOutputSize_roFit_LandscapeImage;
var
  Params: RResizeParams;
begin
  Params.Reset;
  Params.ResizeOpp:= roFit;
  Params.MaxWidth:= 800;
  Params.MaxHeight:= 600;

  { 1600x800 = 2:1 landscape, target is 800x600 = 4:3 }
  Params.ComputeOutputSize(1600, 800);

  { 2:1 is wider than 4:3, so Fit is bound by the width: zoom = 1600 / 800 = 2, OutW = 800, OutH = 800 / 2 = 400 }
  Assert.AreEqual(800, Params.OutW, 'Width should be constrained to 800');
  Assert.AreEqual(400, Params.OutH, 'Height = 800 / 2');
end;


procedure TTestResizeParams.TestComputeOutputSize_roFit_PortraitImage;
var
  Params: RResizeParams;
begin
  Params.Reset;
  Params.ResizeOpp:= roFit;
  Params.MaxWidth:= 800;
  Params.MaxHeight:= 600;

  { 400x800 = 1:2 portrait, target is 800x600 }
  Params.ComputeOutputSize(400, 800);

  { 1:2 is narrower than 4:3, so Fit is bound by the height: zoom = 800 / 600, OutH = 600, OutW = 400 / (800 / 600) = 300 }
  Assert.AreEqual(300, Params.OutW, 'Width = 400 * 600 / 800');
  Assert.AreEqual(600, Params.OutH, 'Height should be constrained to 600');
end;


procedure TTestResizeParams.TestComputeOutputSize_roFit_SameAspect;
var
  Params: RResizeParams;
begin
  Params.Reset;
  Params.ResizeOpp:= roFit;
  Params.MaxWidth:= 800;
  Params.MaxHeight:= 600;

  { 1600x1200 = 4:3, same as target 800x600 = 4:3 }
  Params.ComputeOutputSize(1600, 1200);

  Assert.AreEqual(800, Params.OutW, 'Width should be 800');
  Assert.AreEqual(600, Params.OutH, 'Height should be 600');
end;


procedure TTestResizeParams.TestComputeOutputSize_roFit_NoExceedViewport;
var
  Params: RResizeParams;
begin
  Params.Reset;
  Params.ResizeOpp:= roFit;
  Params.MaxWidth:= 800;
  Params.MaxHeight:= 600;

  Params.ComputeOutputSize(2000, 1500);

  { 2000x1500 is 4:3 like the viewport: zoom = 1500 / 600 = 2000 / 800 = 2.5, so the image shrinks to exactly 800x600 }
  Assert.AreEqual(800, Params.OutW, 'Width = 2000 / 2.5');
  Assert.AreEqual(600, Params.OutH, 'Height = 1500 / 2.5');
end;


{ roFill Mode Tests }

procedure TTestResizeParams.TestComputeOutputSize_roFill_LandscapeImage;
var
  Params: RResizeParams;
begin
  Params.Reset;
  Params.ResizeOpp:= roFill;
  Params.MaxWidth:= 800;
  Params.MaxHeight:= 600;

  { 1600x800 = 2:1 landscape }
  Params.ComputeOutputSize(1600, 800);

  { 2:1 is wider than 4:3, so Fill is bound by the height: zoom = 800 / 600, OutH = 600, OutW = 1600 / (800 / 600) = 1200 (the 400 extra pixels get cropped) }
  Assert.AreEqual(1200, Params.OutW, 'Width = 1600 * 600 / 800');
  Assert.AreEqual(600, Params.OutH, 'Height = viewport height');
end;


procedure TTestResizeParams.TestComputeOutputSize_roFill_PortraitImage;
var
  Params: RResizeParams;
begin
  Params.Reset;
  Params.ResizeOpp:= roFill;
  Params.MaxWidth:= 800;
  Params.MaxHeight:= 600;

  { 400x800 = 1:2 portrait }
  Params.ComputeOutputSize(400, 800);

  { 1:2 is narrower than 4:3, so Fill is bound by the width: zoom = 400 / 800 = 0.5, OutW = 800, OutH = 800 / 0.5 = 1600 }
  Assert.AreEqual(800, Params.OutW, 'Width = viewport width');
  Assert.AreEqual(1600, Params.OutH, 'Height = 800 * 800 / 400');
end;


procedure TTestResizeParams.TestComputeOutputSize_roFill_FillsViewport;
var
  Params: RResizeParams;
begin
  Params.Reset;
  Params.ResizeOpp:= roFill;
  Params.MaxWidth:= 800;
  Params.MaxHeight:= 600;

  Params.ComputeOutputSize(1000, 1000);

  { 1:1 is narrower than 4:3, so Fill is bound by the width: zoom = 1000 / 800 = 1.25, OutW = 800, OutH = 1000 / 1.25 = 800. It covers the whole 800x600 viewport }
  Assert.AreEqual(800, Params.OutW, 'Width = viewport width');
  Assert.AreEqual(800, Params.OutH, 'Height = 1000 / 1.25');
end;


{ roCustom Mode Tests }

procedure TTestResizeParams.TestComputeOutputSize_roCustom_ZoomIn;
var
  Params: RResizeParams;
begin
  Params.Reset;
  Params.ResizeOpp:= roCustom;
  Params.CustomZoom:= 2.0;  { 200% }

  Params.ComputeOutputSize(100, 100);

  Assert.AreEqual(200, Params.OutW, 'Width should double');
  Assert.AreEqual(200, Params.OutH, 'Height should double');
end;


procedure TTestResizeParams.TestComputeOutputSize_roCustom_ZoomOut;
var
  Params: RResizeParams;
begin
  Params.Reset;
  Params.ResizeOpp:= roCustom;
  Params.CustomZoom:= 0.5;  { 50% }

  Params.ComputeOutputSize(200, 200);

  Assert.AreEqual(100, Params.OutW, 'Width should halve');
  Assert.AreEqual(100, Params.OutH, 'Height should halve');
end;


procedure TTestResizeParams.TestComputeOutputSize_roCustom_InvalidZoom;
var
  Params: RResizeParams;
begin
  Params.Reset;
  Params.ResizeOpp:= roCustom;
  Params.CustomZoom:= 0;  { Invalid }

  Assert.WillRaise(
    procedure
    begin
      Params.ComputeOutputSize(100, 100);
    end,
    Exception,
    'Params.ComputeOutputSize(100, 100) must raise Exception');
end;


{ roForceWidth Mode Tests }

procedure TTestResizeParams.TestComputeOutputSize_roForceWidth_BasicCall;
var
  Params: RResizeParams;
begin
  Params.Reset;
  Params.ResizeOpp:= roForceWidth;
  Params.ForcedWidth:= 500;

  Params.ComputeOutputSize(1000, 500);

  Assert.AreEqual(500, Params.OutW, 'Width should be ForcedWidth');
end;


procedure TTestResizeParams.TestComputeOutputSize_roForceWidth_MaintainsAspect;
var
  Params: RResizeParams;
  OrigRatio, NewRatio: Double;
begin
  Params.Reset;
  Params.ResizeOpp:= roForceWidth;
  Params.ForcedWidth:= 400;

  { 800x400 = 2:1 aspect ratio }
  Params.ComputeOutputSize(800, 400);

  OrigRatio:= 800 / 400;
  NewRatio:= Params.OutW / Params.OutH;

  Assert.AreEqual(400, Params.OutW, 'Width should be ForcedWidth');
  Assert.AreEqual(OrigRatio, NewRatio, 0.01, 'Aspect ratio should be preserved');
end;


procedure TTestResizeParams.TestComputeOutputSize_roForceWidth_InvalidWidth;
var
  Params: RResizeParams;
begin
  Params.Reset;
  Params.ResizeOpp:= roForceWidth;
  Params.ForcedWidth:= 0;  { Invalid }

  Assert.WillRaise(
    procedure
    begin
      Params.ComputeOutputSize(100, 100);
    end,
    Exception,
    'Params.ComputeOutputSize(100, 100) must raise Exception');
end;


{ roForceHeight Mode Tests }

procedure TTestResizeParams.TestComputeOutputSize_roForceHeight_BasicCall;
var
  Params: RResizeParams;
begin
  Params.Reset;
  Params.ResizeOpp:= roForceHeight;
  Params.ForcedHeight:= 300;

  Params.ComputeOutputSize(800, 600);

  Assert.AreEqual(300, Params.OutH, 'Height should be ForcedHeight');
end;


procedure TTestResizeParams.TestComputeOutputSize_roForceHeight_MaintainsAspect;
var
  Params: RResizeParams;
  OrigRatio, NewRatio: Double;
begin
  Params.Reset;
  Params.ResizeOpp:= roForceHeight;
  Params.ForcedHeight:= 200;

  { 400x800 = 1:2 aspect ratio }
  Params.ComputeOutputSize(400, 800);

  OrigRatio:= 400 / 800;
  NewRatio:= Params.OutW / Params.OutH;

  Assert.AreEqual(200, Params.OutH, 'Height should be ForcedHeight');
  Assert.AreEqual(OrigRatio, NewRatio, 0.01, 'Aspect ratio should be preserved');
end;


procedure TTestResizeParams.TestComputeOutputSize_roForceHeight_InvalidHeight;
var
  Params: RResizeParams;
begin
  Params.Reset;
  Params.ResizeOpp:= roForceHeight;
  Params.ForcedHeight:= -1;  { Invalid }

  Assert.WillRaise(
    procedure
    begin
      Params.ComputeOutputSize(100, 100);
    end,
    Exception,
    'Params.ComputeOutputSize(100, 100) must raise Exception');
end;


{ roAutoDetect Mode Tests }

procedure TTestResizeParams.TestComputeOutputSize_roAutoDetect_SameDimensions;
var
  Params: RResizeParams;
begin
  Params.Reset;
  Params.ResizeOpp:= roAutoDetect;
  Params.MaxWidth:= 800;
  Params.MaxHeight:= 600;

  { Same dimensions as viewport - should return as-is }
  Params.ComputeOutputSize(800, 600);

  Assert.AreEqual(800, Params.OutW, 'Same dimensions should return width unchanged');
  Assert.AreEqual(600, Params.OutH, 'Same dimensions should return height unchanged');
end;


procedure TTestResizeParams.TestComputeOutputSize_roAutoDetect_ProducesValidOutput;
var
  Params: RResizeParams;
begin
  Params.Reset;
  Params.ResizeOpp:= roAutoDetect;
  Params.MaxWidth:= 1920;
  Params.MaxHeight:= 1080;

  Params.ComputeOutputSize(2560, 1440);

  { 2560x1440 is 16:9 like the 1920x1080 viewport. AutoDetect tries Fill first: zoom = 2560 / 1920 = 1.333, so 1920x1080.
    It shrinks the image, so the MaxZoom limit does not apply, and it covers exactly 100% of the viewport area
    (not more than 105%), so there is no fall-back to Fit }
  Assert.AreEqual(1920, Params.OutW, 'Width = 2560 / 1.333');
  Assert.AreEqual(1080, Params.OutH, 'Height = 1440 / 1.333');
end;


{ TResizeOp Enum Tests }

procedure TTestResizeParams.TestTResizeOp_EnumValues;
begin
  { WriteToStream saves the mode as one byte }
  CheckPersistedOp(roAutoDetect,  0);
  CheckPersistedOp(roCustom,      1);
  CheckPersistedOp(roNone,        2);
  CheckPersistedOp(roFill,        3);
  CheckPersistedOp(roFit,         4);
  CheckPersistedOp(roForceWidth,  5);
  CheckPersistedOp(roForceHeight, 6);
  CheckPersistedOp(roStretch,     7);

  Assert.AreEqual(0, Ord(roAutoDetect), 'roAutoDetect should be 0');
  Assert.AreEqual(1, Ord(roCustom), 'roCustom should be 1');
  Assert.AreEqual(2, Ord(roNone), 'roNone should be 2');
  Assert.AreEqual(3, Ord(roFill), 'roFill should be 3');
  Assert.AreEqual(4, Ord(roFit), 'roFit should be 4');
  Assert.AreEqual(5, Ord(roForceWidth), 'roForceWidth should be 5');
  Assert.AreEqual(6, Ord(roForceHeight), 'roForceHeight should be 6');
  Assert.AreEqual(7, Ord(roStretch), 'roStretch should be 7');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestResizeParams);

end.
