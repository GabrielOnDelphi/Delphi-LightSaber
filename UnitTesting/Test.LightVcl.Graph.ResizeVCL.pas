unit Test.LightVcl.Graph.ResizeVCL;

{=============================================================================================================
   Unit tests for LightVcl.Graph.ResizeVCL.pas
   Tests ScaleImage and CanvasStretch functions.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics;

type
  [TestFixture]
  TTestResizeVCL = class
  private
    FSourceBMP: TBitmap;
    FDestBMP: TBitmap;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { ScaleImage Tests }
    [Test]
    procedure TestScaleImage_NilInpBMP;

    [Test]
    procedure TestScaleImage_NilOutBMP;

    [Test]
    procedure TestScaleImage_ZeroWidthInpBMP;

    [Test]
    procedure TestScaleImage_ZeroHeightInpBMP;

    [Test]
    procedure TestScaleImage_ZeroScaleAmount;

    [Test]
    procedure TestScaleImage_NegativeScaleAmount;

    [Test]
    procedure TestScaleImage_ScaleUp;

    [Test]
    procedure TestScaleImage_ScaleDown;

    [Test]
    procedure TestScaleImage_NoChange;

    { CanvasStretch (two bitmaps) Tests }
    [Test]
    procedure TestCanvasStretch_TwoBitmaps_NilInpBMP;

    [Test]
    procedure TestCanvasStretch_TwoBitmaps_NilOutBMP;

    [Test]
    procedure TestCanvasStretch_TwoBitmaps_ZeroWidthInp;

    [Test]
    procedure TestCanvasStretch_TwoBitmaps_ZeroWidthOut;

    [Test]
    procedure TestCanvasStretch_TwoBitmaps_ValidStretch;

    { CanvasStretch (in-place with dimensions) Tests }
    [Test]
    procedure TestCanvasStretch_InPlace_NilBMP;

    [Test]
    procedure TestCanvasStretch_InPlace_ZeroWidth;

    [Test]
    procedure TestCanvasStretch_InPlace_ZeroHeight;

    [Test]
    procedure TestCanvasStretch_InPlace_ZeroOutWidth;

    [Test]
    procedure TestCanvasStretch_InPlace_ZeroOutHeight;

    [Test]
    procedure TestCanvasStretch_InPlace_StretchUp;

    [Test]
    procedure TestCanvasStretch_InPlace_StretchDown;

    { CanvasStretch (proportional) Tests }
    [Test]
    procedure TestCanvasStretch_Proportional_NilBMP;

    [Test]
    procedure TestCanvasStretch_Proportional_ZeroWidth;

    [Test]
    procedure TestCanvasStretch_Proportional_ZeroOutWidth;

    [Test]
    procedure TestCanvasStretch_Proportional_MaintainsAspectRatio;

    [Test]
    procedure TestCanvasStretch_Proportional_StretchUp;

    [Test]
    procedure TestCanvasStretch_Proportional_StretchDown;
  end;

implementation

uses
  LightVcl.Graph.ResizeVCL;


procedure TTestResizeVCL.Setup;
begin
  FSourceBMP:= TBitmap.Create;
  FSourceBMP.SetSize(100, 50);  { 2:1 aspect ratio }
  FSourceBMP.Canvas.Brush.Color:= clRed;
  FSourceBMP.Canvas.FillRect(Rect(0, 0, 100, 50));

  FDestBMP:= TBitmap.Create;
  FDestBMP.SetSize(200, 100);
end;


procedure TTestResizeVCL.TearDown;
begin
  FreeAndNil(FSourceBMP);
  FreeAndNil(FDestBMP);
end;


{ ScaleImage Tests }

procedure TTestResizeVCL.TestScaleImage_NilInpBMP;
begin
  Assert.WillRaise(
    procedure
    begin
      ScaleImage(nil, FDestBMP, 1.0);
    end,
    EAssertionFailed);
end;


procedure TTestResizeVCL.TestScaleImage_NilOutBMP;
begin
  Assert.WillRaise(
    procedure
    begin
      ScaleImage(FSourceBMP, nil, 1.0);
    end,
    EAssertionFailed);
end;


procedure TTestResizeVCL.TestScaleImage_ZeroWidthInpBMP;
var
  EmptyBMP: TBitmap;
begin
  EmptyBMP:= TBitmap.Create;
  try
    EmptyBMP.Width:= 0;
    EmptyBMP.Height:= 50;

    Assert.WillRaise(
      procedure
      begin
        ScaleImage(EmptyBMP, FDestBMP, 1.0);
      end,
      EAssertionFailed);
  finally
    FreeAndNil(EmptyBMP);
  end;
end;


procedure TTestResizeVCL.TestScaleImage_ZeroHeightInpBMP;
var
  EmptyBMP: TBitmap;
begin
  EmptyBMP:= TBitmap.Create;
  try
    EmptyBMP.Width:= 100;
    EmptyBMP.Height:= 0;

    Assert.WillRaise(
      procedure
      begin
        ScaleImage(EmptyBMP, FDestBMP, 1.0);
      end,
      EAssertionFailed);
  finally
    FreeAndNil(EmptyBMP);
  end;
end;


procedure TTestResizeVCL.TestScaleImage_ZeroScaleAmount;
begin
  Assert.WillRaise(
    procedure
    begin
      ScaleImage(FSourceBMP, FDestBMP, 0.0);
    end,
    EAssertionFailed);
end;


procedure TTestResizeVCL.TestScaleImage_NegativeScaleAmount;
begin
  Assert.WillRaise(
    procedure
    begin
      ScaleImage(FSourceBMP, FDestBMP, -1.0);
    end,
    EAssertionFailed);
end;


procedure TTestResizeVCL.TestScaleImage_ScaleUp;
begin
  ScaleImage(FSourceBMP, FDestBMP, 2.0);

  Assert.AreEqual(200, FDestBMP.Width, 'Width should be doubled');
  Assert.AreEqual(100, FDestBMP.Height, 'Height should be doubled');
end;


procedure TTestResizeVCL.TestScaleImage_ScaleDown;
begin
  ScaleImage(FSourceBMP, FDestBMP, 0.5);

  Assert.AreEqual(50, FDestBMP.Width, 'Width should be halved');
  Assert.AreEqual(25, FDestBMP.Height, 'Height should be halved');
end;


procedure TTestResizeVCL.TestScaleImage_NoChange;
begin
  ScaleImage(FSourceBMP, FDestBMP, 1.0);

  Assert.AreEqual(100, FDestBMP.Width, 'Width should be unchanged');
  Assert.AreEqual(50, FDestBMP.Height, 'Height should be unchanged');
end;


{ CanvasStretch (two bitmaps) Tests }

procedure TTestResizeVCL.TestCanvasStretch_TwoBitmaps_NilInpBMP;
begin
  Assert.WillRaise(
    procedure
    begin
      CanvasStretch(nil, FDestBMP);
    end,
    EAssertionFailed);
end;


procedure TTestResizeVCL.TestCanvasStretch_TwoBitmaps_NilOutBMP;
begin
  Assert.WillRaise(
    procedure
    begin
      CanvasStretch(FSourceBMP, nil);
    end,
    EAssertionFailed);
end;


procedure TTestResizeVCL.TestCanvasStretch_TwoBitmaps_ZeroWidthInp;
var
  EmptyBMP: TBitmap;
begin
  EmptyBMP:= TBitmap.Create;
  try
    EmptyBMP.Width:= 0;
    EmptyBMP.Height:= 50;

    Assert.WillRaise(
      procedure
      begin
        CanvasStretch(EmptyBMP, FDestBMP);
      end,
      EAssertionFailed);
  finally
    FreeAndNil(EmptyBMP);
  end;
end;


procedure TTestResizeVCL.TestCanvasStretch_TwoBitmaps_ZeroWidthOut;
var
  EmptyBMP: TBitmap;
begin
  EmptyBMP:= TBitmap.Create;
  try
    EmptyBMP.Width:= 0;
    EmptyBMP.Height:= 50;

    Assert.WillRaise(
      procedure
      begin
        CanvasStretch(FSourceBMP, EmptyBMP);
      end,
      EAssertionFailed);
  finally
    FreeAndNil(EmptyBMP);
  end;
end;


procedure TTestResizeVCL.TestCanvasStretch_TwoBitmaps_ValidStretch;
begin
  FDestBMP.SetSize(400, 200);

  CanvasStretch(FSourceBMP, FDestBMP);

  { FDestBMP should remain 400x200 (its dimensions are used for the stretch) }
  Assert.AreEqual(400, FDestBMP.Width, 'Width should be 400');
  Assert.AreEqual(200, FDestBMP.Height, 'Height should be 200');
end;


{ CanvasStretch (in-place with dimensions) Tests }

procedure TTestResizeVCL.TestCanvasStretch_InPlace_NilBMP;
begin
  Assert.WillRaise(
    procedure
    begin
      CanvasStretch(nil, 200, 100);
    end,
    EAssertionFailed);
end;


procedure TTestResizeVCL.TestCanvasStretch_InPlace_ZeroWidth;
var
  EmptyBMP: TBitmap;
begin
  EmptyBMP:= TBitmap.Create;
  try
    EmptyBMP.Width:= 0;
    EmptyBMP.Height:= 50;

    Assert.WillRaise(
      procedure
      begin
        CanvasStretch(EmptyBMP, 200, 100);
      end,
      EAssertionFailed);
  finally
    FreeAndNil(EmptyBMP);
  end;
end;


procedure TTestResizeVCL.TestCanvasStretch_InPlace_ZeroHeight;
var
  EmptyBMP: TBitmap;
begin
  EmptyBMP:= TBitmap.Create;
  try
    EmptyBMP.Width:= 100;
    EmptyBMP.Height:= 0;

    Assert.WillRaise(
      procedure
      begin
        CanvasStretch(EmptyBMP, 200, 100);
      end,
      EAssertionFailed);
  finally
    FreeAndNil(EmptyBMP);
  end;
end;


procedure TTestResizeVCL.TestCanvasStretch_InPlace_ZeroOutWidth;
begin
  Assert.WillRaise(
    procedure
    begin
      CanvasStretch(FSourceBMP, 0, 100);
    end,
    EAssertionFailed);
end;


procedure TTestResizeVCL.TestCanvasStretch_InPlace_ZeroOutHeight;
begin
  Assert.WillRaise(
    procedure
    begin
      CanvasStretch(FSourceBMP, 200, 0);
    end,
    EAssertionFailed);
end;


procedure TTestResizeVCL.TestCanvasStretch_InPlace_StretchUp;
begin
  CanvasStretch(FSourceBMP, 200, 100);

  Assert.AreEqual(200, FSourceBMP.Width, 'Width should be 200');
  Assert.AreEqual(100, FSourceBMP.Height, 'Height should be 100');
end;


procedure TTestResizeVCL.TestCanvasStretch_InPlace_StretchDown;
begin
  CanvasStretch(FSourceBMP, 50, 25);

  Assert.AreEqual(50, FSourceBMP.Width, 'Width should be 50');
  Assert.AreEqual(25, FSourceBMP.Height, 'Height should be 25');
end;


{ CanvasStretch (proportional) Tests }

procedure TTestResizeVCL.TestCanvasStretch_Proportional_NilBMP;
begin
  Assert.WillRaise(
    procedure
    begin
      CanvasStretch(nil, 200);
    end,
    EAssertionFailed);
end;


procedure TTestResizeVCL.TestCanvasStretch_Proportional_ZeroWidth;
var
  EmptyBMP: TBitmap;
begin
  EmptyBMP:= TBitmap.Create;
  try
    EmptyBMP.Width:= 0;
    EmptyBMP.Height:= 50;

    Assert.WillRaise(
      procedure
      begin
        CanvasStretch(EmptyBMP, 200);
      end,
      EAssertionFailed);
  finally
    FreeAndNil(EmptyBMP);
  end;
end;


procedure TTestResizeVCL.TestCanvasStretch_Proportional_ZeroOutWidth;
begin
  Assert.WillRaise(
    procedure
    begin
      CanvasStretch(FSourceBMP, 0);
    end,
    EAssertionFailed);
end;


procedure TTestResizeVCL.TestCanvasStretch_Proportional_MaintainsAspectRatio;
var
  OriginalRatio, NewRatio: Double;
begin
  { FSourceBMP is 100x50, aspect ratio = 2.0 }
  OriginalRatio:= FSourceBMP.Width / FSourceBMP.Height;

  CanvasStretch(FSourceBMP, 200);  { Stretch to width 200, height should be 100 }

  NewRatio:= FSourceBMP.Width / FSourceBMP.Height;

  Assert.AreEqual(200, FSourceBMP.Width, 'Width should be 200');
  Assert.AreEqual(OriginalRatio, NewRatio, 0.01, 'Aspect ratio should be preserved');
end;


procedure TTestResizeVCL.TestCanvasStretch_Proportional_StretchUp;
begin
  CanvasStretch(FSourceBMP, 400);

  Assert.AreEqual(400, FSourceBMP.Width, 'Width should be 400');
  Assert.AreEqual(200, FSourceBMP.Height, 'Height should be 200 (proportional)');
end;


procedure TTestResizeVCL.TestCanvasStretch_Proportional_StretchDown;
begin
  CanvasStretch(FSourceBMP, 50);

  Assert.AreEqual(50, FSourceBMP.Width, 'Width should be 50');
  Assert.AreEqual(25, FSourceBMP.Height, 'Height should be 25 (proportional)');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestResizeVCL);

end.
