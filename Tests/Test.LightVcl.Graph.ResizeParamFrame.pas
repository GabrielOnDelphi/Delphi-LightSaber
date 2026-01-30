unit Test.LightVcl.Graph.ResizeParamFrame;

{=============================================================================================================
   Unit tests for LightVcl.Graph.ResizeParamFrame.pas
   Tests TResizeParameters frame GUI-to-record conversion methods.

   Note: These tests create the frame programmatically without a parent form.
   The frame must support being created without a parent for these tests to work.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Vcl.Forms;

type
  [TestFixture]
  TTestResizeParamFrame = class
  private
    FFrame: TObject;  { Using TObject to avoid interface section dependency }
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { ObjectFromGUI Tests }
    [Test]
    procedure TestObjectFromGUI_NilParams;

    [Test]
    procedure TestObjectFromGUI_roNone;

    [Test]
    procedure TestObjectFromGUI_roAutoDetect;

    [Test]
    procedure TestObjectFromGUI_roFit;

    [Test]
    procedure TestObjectFromGUI_roFill;

    [Test]
    procedure TestObjectFromGUI_roCustom;

    [Test]
    procedure TestObjectFromGUI_roForceWidth;

    [Test]
    procedure TestObjectFromGUI_roForceHeight;

    [Test]
    procedure TestObjectFromGUI_roStretch;

    [Test]
    procedure TestObjectFromGUI_MaxZoomUse;

    [Test]
    procedure TestObjectFromGUI_NumericValues;

    { GUIFromObject Tests }
    [Test]
    procedure TestGUIFromObject_NilParams;

    [Test]
    procedure TestGUIFromObject_roNone;

    [Test]
    procedure TestGUIFromObject_roAutoDetect;

    [Test]
    procedure TestGUIFromObject_roFit;

    [Test]
    procedure TestGUIFromObject_roFill;

    [Test]
    procedure TestGUIFromObject_roCustom;

    [Test]
    procedure TestGUIFromObject_roForceWidth;

    [Test]
    procedure TestGUIFromObject_roForceHeight;

    [Test]
    procedure TestGUIFromObject_roStretch;

    [Test]
    procedure TestGUIFromObject_MaxZoomValues;

    [Test]
    procedure TestGUIFromObject_NumericValues;

    { Round-trip Tests }
    [Test]
    procedure TestRoundTrip_AllModes;

    [Test]
    procedure TestRoundTrip_NumericValues;

    { GUIChanged Tests }
    [Test]
    procedure TestGUIChanged_AutoZoom_EnablesMaxZoom;

    [Test]
    procedure TestGUIChanged_OtherMode_DisablesMaxZoom;
  end;

implementation

uses
  LightVcl.Graph.ResizeParams,
  LightVcl.Graph.ResizeParamFrame;


procedure TTestResizeParamFrame.Setup;
begin
  FFrame:= TResizeParameters.Create(nil);
end;


procedure TTestResizeParamFrame.TearDown;
begin
  FreeAndNil(FFrame);
end;


{ ObjectFromGUI Tests }

procedure TTestResizeParamFrame.TestObjectFromGUI_NilParams;
var
  Frame: TResizeParameters;
begin
  Frame:= TResizeParameters(FFrame);

  Assert.WillRaise(
    procedure
    begin
      Frame.ObjectFromGUI(nil);
    end,
    EAssertionFailed);
end;


procedure TTestResizeParamFrame.TestObjectFromGUI_roNone;
var
  Frame: TResizeParameters;
  Params: RResizeParams;
begin
  Frame:= TResizeParameters(FFrame);
  Frame.radZoomNone.Checked:= TRUE;

  Frame.ObjectFromGUI(@Params);

  Assert.AreEqual(roNone, Params.ResizeOpp, 'Should be roNone');
end;


procedure TTestResizeParamFrame.TestObjectFromGUI_roAutoDetect;
var
  Frame: TResizeParameters;
  Params: RResizeParams;
begin
  Frame:= TResizeParameters(FFrame);
  Frame.radZoomAuto.Checked:= TRUE;

  Frame.ObjectFromGUI(@Params);

  Assert.AreEqual(roAutoDetect, Params.ResizeOpp, 'Should be roAutoDetect');
end;


procedure TTestResizeParamFrame.TestObjectFromGUI_roFit;
var
  Frame: TResizeParameters;
  Params: RResizeParams;
begin
  Frame:= TResizeParameters(FFrame);
  Frame.radZoomFit.Checked:= TRUE;

  Frame.ObjectFromGUI(@Params);

  Assert.AreEqual(roFit, Params.ResizeOpp, 'Should be roFit');
end;


procedure TTestResizeParamFrame.TestObjectFromGUI_roFill;
var
  Frame: TResizeParameters;
  Params: RResizeParams;
begin
  Frame:= TResizeParameters(FFrame);
  Frame.radZoomFill.Checked:= TRUE;

  Frame.ObjectFromGUI(@Params);

  Assert.AreEqual(roFill, Params.ResizeOpp, 'Should be roFill');
end;


procedure TTestResizeParamFrame.TestObjectFromGUI_roCustom;
var
  Frame: TResizeParameters;
  Params: RResizeParams;
begin
  Frame:= TResizeParameters(FFrame);
  Frame.radZoomCustom.Checked:= TRUE;

  Frame.ObjectFromGUI(@Params);

  Assert.AreEqual(roCustom, Params.ResizeOpp, 'Should be roCustom');
end;


procedure TTestResizeParamFrame.TestObjectFromGUI_roForceWidth;
var
  Frame: TResizeParameters;
  Params: RResizeParams;
begin
  Frame:= TResizeParameters(FFrame);
  Frame.radForceWidth.Checked:= TRUE;

  Frame.ObjectFromGUI(@Params);

  Assert.AreEqual(roForceWidth, Params.ResizeOpp, 'Should be roForceWidth');
end;


procedure TTestResizeParamFrame.TestObjectFromGUI_roForceHeight;
var
  Frame: TResizeParameters;
  Params: RResizeParams;
begin
  Frame:= TResizeParameters(FFrame);
  Frame.radForceHeight.Checked:= TRUE;

  Frame.ObjectFromGUI(@Params);

  Assert.AreEqual(roForceHeight, Params.ResizeOpp, 'Should be roForceHeight');
end;


procedure TTestResizeParamFrame.TestObjectFromGUI_roStretch;
var
  Frame: TResizeParameters;
  Params: RResizeParams;
begin
  Frame:= TResizeParameters(FFrame);
  Frame.radStretch.Checked:= TRUE;

  Frame.ObjectFromGUI(@Params);

  Assert.AreEqual(roStretch, Params.ResizeOpp, 'Should be roStretch');
end;


procedure TTestResizeParamFrame.TestObjectFromGUI_MaxZoomUse;
var
  Frame: TResizeParameters;
  Params: RResizeParams;
begin
  Frame:= TResizeParameters(FFrame);

  Frame.chkZoomMax.Checked:= TRUE;
  Frame.ObjectFromGUI(@Params);
  Assert.IsTrue(Params.MaxZoomUse, 'MaxZoomUse should be TRUE');

  Frame.chkZoomMax.Checked:= FALSE;
  Frame.ObjectFromGUI(@Params);
  Assert.IsFalse(Params.MaxZoomUse, 'MaxZoomUse should be FALSE');
end;


procedure TTestResizeParamFrame.TestObjectFromGUI_NumericValues;
var
  Frame: TResizeParameters;
  Params: RResizeParams;
begin
  Frame:= TResizeParameters(FFrame);
  Frame.spnZoomMax.Value:= 75;
  Frame.numForceWidth.ValueInt:= 1024;
  Frame.numForceHeight.ValueInt:= 768;
  Frame.spnZoomCustom.Value:= 2.5;

  Frame.ObjectFromGUI(@Params);

  Assert.AreEqual(75, Params.MaxZoomVal, 'MaxZoomVal should be 75');
  Assert.AreEqual(1024, Params.ForcedWidth, 'ForcedWidth should be 1024');
  Assert.AreEqual(768, Params.ForcedHeight, 'ForcedHeight should be 768');
  Assert.AreEqual(Single(2.5), Params.CustomZoom, 'CustomZoom should be 2.5');
end;


{ GUIFromObject Tests }

procedure TTestResizeParamFrame.TestGUIFromObject_NilParams;
var
  Frame: TResizeParameters;
begin
  Frame:= TResizeParameters(FFrame);

  Assert.WillRaise(
    procedure
    begin
      Frame.GUIFromObject(nil);
    end,
    EAssertionFailed);
end;


procedure TTestResizeParamFrame.TestGUIFromObject_roNone;
var
  Frame: TResizeParameters;
  Params: RResizeParams;
begin
  Frame:= TResizeParameters(FFrame);
  Params.Reset;
  Params.ResizeOpp:= roNone;

  Frame.GUIFromObject(@Params);

  Assert.IsTrue(Frame.radZoomNone.Checked, 'radZoomNone should be checked');
end;


procedure TTestResizeParamFrame.TestGUIFromObject_roAutoDetect;
var
  Frame: TResizeParameters;
  Params: RResizeParams;
begin
  Frame:= TResizeParameters(FFrame);
  Params.Reset;
  Params.ResizeOpp:= roAutoDetect;

  Frame.GUIFromObject(@Params);

  Assert.IsTrue(Frame.radZoomAuto.Checked, 'radZoomAuto should be checked');
end;


procedure TTestResizeParamFrame.TestGUIFromObject_roFit;
var
  Frame: TResizeParameters;
  Params: RResizeParams;
begin
  Frame:= TResizeParameters(FFrame);
  Params.Reset;
  Params.ResizeOpp:= roFit;

  Frame.GUIFromObject(@Params);

  Assert.IsTrue(Frame.radZoomFit.Checked, 'radZoomFit should be checked');
end;


procedure TTestResizeParamFrame.TestGUIFromObject_roFill;
var
  Frame: TResizeParameters;
  Params: RResizeParams;
begin
  Frame:= TResizeParameters(FFrame);
  Params.Reset;
  Params.ResizeOpp:= roFill;

  Frame.GUIFromObject(@Params);

  Assert.IsTrue(Frame.radZoomFill.Checked, 'radZoomFill should be checked');
end;


procedure TTestResizeParamFrame.TestGUIFromObject_roCustom;
var
  Frame: TResizeParameters;
  Params: RResizeParams;
begin
  Frame:= TResizeParameters(FFrame);
  Params.Reset;
  Params.ResizeOpp:= roCustom;

  Frame.GUIFromObject(@Params);

  Assert.IsTrue(Frame.radZoomCustom.Checked, 'radZoomCustom should be checked');
end;


procedure TTestResizeParamFrame.TestGUIFromObject_roForceWidth;
var
  Frame: TResizeParameters;
  Params: RResizeParams;
begin
  Frame:= TResizeParameters(FFrame);
  Params.Reset;
  Params.ResizeOpp:= roForceWidth;

  Frame.GUIFromObject(@Params);

  Assert.IsTrue(Frame.radForceWidth.Checked, 'radForceWidth should be checked');
end;


procedure TTestResizeParamFrame.TestGUIFromObject_roForceHeight;
var
  Frame: TResizeParameters;
  Params: RResizeParams;
begin
  Frame:= TResizeParameters(FFrame);
  Params.Reset;
  Params.ResizeOpp:= roForceHeight;

  Frame.GUIFromObject(@Params);

  Assert.IsTrue(Frame.radForceHeight.Checked, 'radForceHeight should be checked');
end;


procedure TTestResizeParamFrame.TestGUIFromObject_roStretch;
var
  Frame: TResizeParameters;
  Params: RResizeParams;
begin
  Frame:= TResizeParameters(FFrame);
  Params.Reset;
  Params.ResizeOpp:= roStretch;

  Frame.GUIFromObject(@Params);

  Assert.IsTrue(Frame.radStretch.Checked, 'radStretch should be checked');
end;


procedure TTestResizeParamFrame.TestGUIFromObject_MaxZoomValues;
var
  Frame: TResizeParameters;
  Params: RResizeParams;
begin
  Frame:= TResizeParameters(FFrame);
  Params.Reset;
  Params.MaxZoomUse:= TRUE;
  Params.MaxZoomVal:= 60;

  Frame.GUIFromObject(@Params);

  Assert.IsTrue(Frame.chkZoomMax.Checked, 'chkZoomMax should be checked');
  Assert.AreEqual(60, Frame.spnZoomMax.Value, 'spnZoomMax should be 60');
end;


procedure TTestResizeParamFrame.TestGUIFromObject_NumericValues;
var
  Frame: TResizeParameters;
  Params: RResizeParams;
begin
  Frame:= TResizeParameters(FFrame);
  Params.Reset;
  Params.ForcedWidth:= 1280;
  Params.ForcedHeight:= 720;
  Params.CustomZoom:= 1.75;

  Frame.GUIFromObject(@Params);

  Assert.AreEqual(1280, Frame.numForceWidth.ValueInt, 'numForceWidth should be 1280');
  Assert.AreEqual(720, Frame.numForceHeight.ValueInt, 'numForceHeight should be 720');
  Assert.AreEqual(Double(1.75), Frame.spnZoomCustom.Value, 0.001, 'spnZoomCustom should be 1.75');
end;


{ Round-trip Tests }

procedure TTestResizeParamFrame.TestRoundTrip_AllModes;
var
  Frame: TResizeParameters;
  Original, Recovered: RResizeParams;
  Mode: TResizeOp;
begin
  Frame:= TResizeParameters(FFrame);

  for Mode:= Low(TResizeOp) to High(TResizeOp) do
  begin
    Original.Reset;
    Original.ResizeOpp:= Mode;

    Frame.GUIFromObject(@Original);
    Frame.ObjectFromGUI(@Recovered);

    Assert.AreEqual(Original.ResizeOpp, Recovered.ResizeOpp,
      'Round-trip failed for mode: ' + IntToStr(Ord(Mode)));
  end;
end;


procedure TTestResizeParamFrame.TestRoundTrip_NumericValues;
var
  Frame: TResizeParameters;
  Original, Recovered: RResizeParams;
begin
  Frame:= TResizeParameters(FFrame);

  Original.Reset;
  Original.MaxZoomUse:= TRUE;
  Original.MaxZoomVal:= 45;
  Original.ForcedWidth:= 1600;
  Original.ForcedHeight:= 900;
  Original.CustomZoom:= 2.25;

  Frame.GUIFromObject(@Original);
  Frame.ObjectFromGUI(@Recovered);

  Assert.AreEqual(Original.MaxZoomUse, Recovered.MaxZoomUse, 'MaxZoomUse mismatch');
  Assert.AreEqual(Original.MaxZoomVal, Recovered.MaxZoomVal, 'MaxZoomVal mismatch');
  Assert.AreEqual(Original.ForcedWidth, Recovered.ForcedWidth, 'ForcedWidth mismatch');
  Assert.AreEqual(Original.ForcedHeight, Recovered.ForcedHeight, 'ForcedHeight mismatch');
  Assert.AreEqual(Original.CustomZoom, Recovered.CustomZoom, 0.001, 'CustomZoom mismatch');
end;


{ GUIChanged Tests }

procedure TTestResizeParamFrame.TestGUIChanged_AutoZoom_EnablesMaxZoom;
var
  Frame: TResizeParameters;
begin
  Frame:= TResizeParameters(FFrame);
  Frame.radZoomAuto.Checked:= TRUE;

  Frame.GUIChanged(nil);

  Assert.IsTrue(Frame.chkZoomMax.Enabled, 'chkZoomMax should be enabled when AutoZoom selected');
  Assert.IsTrue(Frame.spnZoomMax.Enabled, 'spnZoomMax should be enabled when AutoZoom selected');
end;


procedure TTestResizeParamFrame.TestGUIChanged_OtherMode_DisablesMaxZoom;
var
  Frame: TResizeParameters;
begin
  Frame:= TResizeParameters(FFrame);
  Frame.radZoomFit.Checked:= TRUE;

  Frame.GUIChanged(nil);

  Assert.IsFalse(Frame.chkZoomMax.Enabled, 'chkZoomMax should be disabled when not AutoZoom');
  Assert.IsFalse(Frame.spnZoomMax.Enabled, 'spnZoomMax should be disabled when not AutoZoom');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestResizeParamFrame);

end.
