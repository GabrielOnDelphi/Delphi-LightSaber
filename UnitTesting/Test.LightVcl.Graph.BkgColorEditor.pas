unit Test.LightVcl.Graph.BkgColorEditor;

{=============================================================================================================
   Unit tests for LightVcl.Graph.BkgColorEditor.pas
   Tests the background color editor form logic: Get/Set functions and GUI<->Object conversion.

   Note: These tests create actual form instances. They require VCL initialization.
   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Vcl.Graphics,
  Vcl.Forms,
  LightVcl.Graph.BkgColorParams;

type
  [TestFixture]
  TTestBkgColorEditor = class
  private
    FParams: RBkgColorParams;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { GetEffectShape Tests }
    [Test]
    procedure TestGetEffectShape_Solid;

    [Test]
    procedure TestGetEffectShape_Rectangles;

    [Test]
    procedure TestGetEffectShape_Triangles;

    { GetEffectColor Tests }
    [Test]
    procedure TestGetEffectColor_AutoDetect;

    [Test]
    procedure TestGetEffectColor_ImageAverage;

    [Test]
    procedure TestGetEffectColor_UserColor;

    { GetFillType Tests }
    [Test]
    procedure TestGetFillType_Solid;

    [Test]
    procedure TestGetFillType_Fade;

    { Round-trip Tests }
    [Test]
    procedure TestGuiFromObject_ObjectFromGUI_RoundTrip;

    [Test]
    procedure TestReset_ResetsAllFields;
  end;

implementation

uses
  LightVcl.Graph.BkgColorEditor;

{ Setup and TearDown }

procedure TTestBkgColorEditor.Setup;
begin
  FParams.Reset;
end;

procedure TTestBkgColorEditor.TearDown;
begin
  { Nothing to clean up }
end;

{ GetEffectShape Tests }

procedure TTestBkgColorEditor.TestGetEffectShape_Solid;
var
  Form: TfrmBorderEditor;
begin
  Application.CreateForm(TfrmBorderEditor, Form);
  try
    Form.radShapeSolid.Checked:= True;
    Form.radShapeRect.Checked:= False;
    Form.radShapeTraing.Checked:= False;
    Assert.AreEqual(esOneColor, Form.GetEffectShape, 'Solid should return esOneColor');
  finally
    Form.Free;
  end;
end;

procedure TTestBkgColorEditor.TestGetEffectShape_Rectangles;
var
  Form: TfrmBorderEditor;
begin
  Application.CreateForm(TfrmBorderEditor, Form);
  try
    Form.radShapeSolid.Checked:= False;
    Form.radShapeRect.Checked:= True;
    Form.radShapeTraing.Checked:= False;
    Assert.AreEqual(esRectangles, Form.GetEffectShape, 'Rect should return esRectangles');
  finally
    Form.Free;
  end;
end;

procedure TTestBkgColorEditor.TestGetEffectShape_Triangles;
var
  Form: TfrmBorderEditor;
begin
  Application.CreateForm(TfrmBorderEditor, Form);
  try
    Form.radShapeSolid.Checked:= False;
    Form.radShapeRect.Checked:= False;
    Form.radShapeTraing.Checked:= True;
    Assert.AreEqual(esTriangles, Form.GetEffectShape, 'Triangle should return esTriangles');
  finally
    Form.Free;
  end;
end;

{ GetEffectColor Tests }

procedure TTestBkgColorEditor.TestGetEffectColor_AutoDetect;
var
  Form: TfrmBorderEditor;
begin
  Application.CreateForm(TfrmBorderEditor, Form);
  try
    Form.radAutoDetBorder.Checked:= True;
    Form.radImageAverage.Checked:= False;
    Form.radUserColor.Checked:= False;
    Assert.AreEqual(ecAutoDetBorder, Form.GetEffectColor, 'AutoDetect should return ecAutoDetBorder');
  finally
    Form.Free;
  end;
end;

procedure TTestBkgColorEditor.TestGetEffectColor_ImageAverage;
var
  Form: TfrmBorderEditor;
begin
  Application.CreateForm(TfrmBorderEditor, Form);
  try
    Form.radAutoDetBorder.Checked:= False;
    Form.radImageAverage.Checked:= True;
    Form.radUserColor.Checked:= False;
    Assert.AreEqual(ecImageAverage, Form.GetEffectColor, 'ImageAverage should return ecImageAverage');
  finally
    Form.Free;
  end;
end;

procedure TTestBkgColorEditor.TestGetEffectColor_UserColor;
var
  Form: TfrmBorderEditor;
begin
  Application.CreateForm(TfrmBorderEditor, Form);
  try
    Form.radAutoDetBorder.Checked:= False;
    Form.radImageAverage.Checked:= False;
    Form.radUserColor.Checked:= True;
    Assert.AreEqual(ecUserColor, Form.GetEffectColor, 'UserColor should return ecUserColor');
  finally
    Form.Free;
  end;
end;

{ GetFillType Tests }

procedure TTestBkgColorEditor.TestGetFillType_Solid;
var
  Form: TfrmBorderEditor;
begin
  Application.CreateForm(TfrmBorderEditor, Form);
  try
    Form.radFill.Checked:= True;
    Form.radFade.Checked:= False;
    Assert.AreEqual(ftSolid, Form.GetFillType, 'Fill should return ftSolid');
  finally
    Form.Free;
  end;
end;

procedure TTestBkgColorEditor.TestGetFillType_Fade;
var
  Form: TfrmBorderEditor;
begin
  Application.CreateForm(TfrmBorderEditor, Form);
  try
    Form.radFill.Checked:= False;
    Form.radFade.Checked:= True;
    Assert.AreEqual(ftFade, Form.GetFillType, 'Fade should return ftFade');
  finally
    Form.Free;
  end;
end;

{ Round-trip Tests }

procedure TTestBkgColorEditor.TestGuiFromObject_ObjectFromGUI_RoundTrip;
var
  Form: TfrmBorderEditor;
  OrigParams, ResultParams: RBkgColorParams;
begin
  { Set up original params with non-default values }
  OrigParams.FillType       := ftFade;
  OrigParams.EffectShape    := esTriangles;
  OrigParams.EffectColor    := ecUserColor;
  OrigParams.Color          := clRed;
  OrigParams.FadeSpeed      := 300;
  OrigParams.EdgeSmear      := 5;
  OrigParams.NeighborWeight := 150;
  OrigParams.NeighborDist   := 4;
  OrigParams.Tolerance      := 12;

  Application.CreateForm(TfrmBorderEditor, Form);
  try
    { Load original params into GUI }
    Form.BkgClrParams:= @OrigParams;
    Form.GuiFromObject;

    { Read back from GUI into result params }
    ResultParams.Reset;
    Form.BkgClrParams:= @ResultParams;
    Form.ObjectFromGUI;

    { Verify round-trip preserves values }
    Assert.AreEqual(OrigParams.FillType, ResultParams.FillType, 'FillType mismatch');
    Assert.AreEqual(OrigParams.EffectShape, ResultParams.EffectShape, 'EffectShape mismatch');
    Assert.AreEqual(OrigParams.EffectColor, ResultParams.EffectColor, 'EffectColor mismatch');
    Assert.AreEqual(OrigParams.Color, ResultParams.Color, 'Color mismatch');
    Assert.AreEqual(OrigParams.FadeSpeed, ResultParams.FadeSpeed, 'FadeSpeed mismatch');
    Assert.AreEqual(OrigParams.EdgeSmear, ResultParams.EdgeSmear, 'EdgeSmear mismatch');
    Assert.AreEqual(OrigParams.NeighborWeight, ResultParams.NeighborWeight, 'NeighborWeight mismatch');
    Assert.AreEqual(OrigParams.NeighborDist, ResultParams.NeighborDist, 'NeighborDist mismatch');
    Assert.AreEqual(OrigParams.Tolerance, ResultParams.Tolerance, 'Tolerance mismatch');
  finally
    Form.Free;
  end;
end;

procedure TTestBkgColorEditor.TestReset_ResetsAllFields;
var
  Form: TfrmBorderEditor;
  Params: RBkgColorParams;
begin
  { Set up non-default values }
  Params.FillType    := ftFade;
  Params.EffectShape := esTriangles;
  Params.FadeSpeed   := 999;

  Application.CreateForm(TfrmBorderEditor, Form);
  try
    Form.BkgClrParams:= @Params;

    { Reset should restore defaults }
    Params.Reset;
    Form.GuiFromObject;

    Assert.AreEqual(ftSolid, Form.GetFillType, 'Reset should set FillType to ftSolid');
    Assert.AreEqual(esOneColor, Form.GetEffectShape, 'Reset should set EffectShape to esOneColor');
  finally
    Form.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBkgColorEditor);

end.
