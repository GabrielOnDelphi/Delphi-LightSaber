unit Test.LightVcl.Graph.ResizeParams;

{=============================================================================================================
   Unit tests for LightVcl.Graph.ResizeParams.pas
   Tests RResizeParams record functionality including Reset, ComputeOutputSize, and stream I/O.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes;

type
  [TestFixture]
  TTestResizeParams = class
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

    { Constants Tests }
    [Test]
    procedure TestConstants_UninitializedSize;
  end;

implementation

uses
  LightVcl.Graph.ResizeParams;


{ Reset Tests }

procedure TTestResizeParams.TestReset_DefaultValues;
var
  Params: RResizeParams;
begin
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
    Exception);
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
    Exception);
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
    Exception);
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
    Exception);
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

  { Should fit by width: 800 wide, height = 800 / 2 = 400 }
  Assert.AreEqual(800, Params.OutW, 'Width should be constrained to 800');
  Assert.IsTrue(Params.OutH <= 600, 'Height should fit within 600');
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

  { Should fit by height: 600 tall, width = 600 / 2 = 300 }
  Assert.IsTrue(Params.OutW <= 800, 'Width should fit within 800');
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

  Assert.IsTrue(Params.OutW <= 800, 'Width should not exceed MaxWidth');
  Assert.IsTrue(Params.OutH <= 600, 'Height should not exceed MaxHeight');
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

  { Fill: at least one dimension should equal viewport }
  Assert.IsTrue((Params.OutW >= 800) OR (Params.OutH >= 600),
    'Fill should meet or exceed at least one viewport dimension');
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

  { Fill: at least one dimension should equal viewport }
  Assert.IsTrue((Params.OutW >= 800) OR (Params.OutH >= 600),
    'Fill should meet or exceed at least one viewport dimension');
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

  { Fill mode: viewport should be completely covered }
  Assert.IsTrue((Params.OutW >= 800) AND (Params.OutH >= 600),
    'Fill should cover entire viewport');
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
    Exception);
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
    Exception);
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
    Exception);
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

  Assert.IsTrue(Params.OutW > 0, 'Output width should be positive');
  Assert.IsTrue(Params.OutH > 0, 'Output height should be positive');
end;


{ TResizeOp Enum Tests }

procedure TTestResizeParams.TestTResizeOp_EnumValues;
begin
  Assert.AreEqual(0, Ord(roAutoDetect), 'roAutoDetect should be 0');
  Assert.AreEqual(1, Ord(roCustom), 'roCustom should be 1');
  Assert.AreEqual(2, Ord(roNone), 'roNone should be 2');
  Assert.AreEqual(3, Ord(roFill), 'roFill should be 3');
  Assert.AreEqual(4, Ord(roFit), 'roFit should be 4');
  Assert.AreEqual(5, Ord(roForceWidth), 'roForceWidth should be 5');
  Assert.AreEqual(6, Ord(roForceHeight), 'roForceHeight should be 6');
  Assert.AreEqual(7, Ord(roStretch), 'roStretch should be 7');
end;


{ Constants Tests }

procedure TTestResizeParams.TestConstants_UninitializedSize;
begin
  Assert.AreEqual(-7777, UNINITIALIZED_SIZE, 'UNINITIALIZED_SIZE should be -7777');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestResizeParams);

end.
