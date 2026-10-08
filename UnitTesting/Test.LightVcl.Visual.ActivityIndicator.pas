unit Test.LightVcl.Visual.ActivityIndicator;

{=============================================================================================================
   2026.10.07
   Unit tests for LightVcl.Visual.ActivityIndicator.pas
   Tests TActivityIndicatorC.DrawFrame - the only routine the LightSaber class adds to the VCL TActivityIndicator.
   When stopped it outlines a red triangle (DrawTriangle with BorderDistance 8); when animating it calls the VCL drawing.

   The control is painted into a bitmap with PaintTo, then pixels are read.
   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Types,
  System.Classes,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.Graphics,
  LightVcl.Visual.ActivityIndicator;

type
  [TestFixture]
  TTestActivityIndicatorC = class
  private
    FForm: TForm;
    FIndicator: TActivityIndicatorC;
    function PaintIndicator: TBitmap;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestDrawFrameWhenNotAnimating;

    [Test]
    procedure TestDrawFrameWhenAnimating;
  end;

implementation

CONST
  IndicatorSize = 50;
  BorderDistance = 8;   { The value TActivityIndicatorC.DrawFrame passes to DrawTriangle }


procedure TTestActivityIndicatorC.Setup;
begin
  FForm:= TForm.CreateNew(NIL);
  FForm.Width:= 400;
  FForm.Height:= 300;
  FForm.Visible:= FALSE;

  FIndicator:= TActivityIndicatorC.Create(FForm);
  FIndicator.Parent:= FForm;
  FIndicator.Width:= IndicatorSize;
  FIndicator.Height:= IndicatorSize;
end;


procedure TTestActivityIndicatorC.TearDown;
begin
  FreeAndNil(FIndicator);
  FreeAndNil(FForm);
end;


{ Paints the indicator into a white bitmap of the same size. The caller frees the bitmap. }
function TTestActivityIndicatorC.PaintIndicator: TBitmap;
begin
  FForm.HandleNeeded;
  FIndicator.HandleNeeded;   { DrawFrame draws the triangle only when HandleAllocated }

  Result:= TBitmap.Create;
  Result.PixelFormat:= pf24bit;
  Result.SetSize(FIndicator.Width, FIndicator.Height);   { The VCL sizes the control from IndicatorSize, not from the Width/Height set in Setup }
  Result.Canvas.Brush.Color:= clWhite;
  Result.Canvas.FillRect(Rect(0, 0, Result.Width, Result.Height));

  FIndicator.PaintTo(Result.Canvas.Handle, 0, 0);   { WM_PAINT -> Paint -> DrawFrame }
end;


{ The left edge of the triangle runs from (8,8) to (8,Height-8). Its middle must be red.
  The top-right corner lies outside the triangle and must not be red. }
procedure TTestActivityIndicatorC.TestDrawFrameWhenNotAnimating;
var
  BMP: TBitmap;
begin
  FIndicator.Animate:= FALSE;
  BMP:= PaintIndicator;
  try
    Assert.IsTrue(BMP.Height > 2 * BorderDistance, 'Precondition: the control is high enough for a triangle. Height: ' + IntToStr(BMP.Height));
    Assert.AreEqual(Integer(clRed), Integer(BMP.Canvas.Pixels[BorderDistance, BMP.Height DIV 2]), 'The left edge of the triangle must be red');
    Assert.AreNotEqual(Integer(clRed), Integer(BMP.Canvas.Pixels[BMP.Width - 2, 1]), 'A pixel outside the triangle must not be red');
  finally
    FreeAndNil(BMP);
  end;
end;


{ When animating, DrawFrame calls the VCL drawing, which draws no red triangle. }
procedure TTestActivityIndicatorC.TestDrawFrameWhenAnimating;
var
  BMP: TBitmap;
begin
  FIndicator.Animate:= TRUE;
  try
    BMP:= PaintIndicator;
    try
      Assert.AreNotEqual(Integer(clRed), Integer(BMP.Canvas.Pixels[BorderDistance, BMP.Height DIV 2]),'No red triangle may be drawn while animating');
    finally
      FreeAndNil(BMP);
    end;
  finally
    FIndicator.Animate:= FALSE;
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestActivityIndicatorC);

end.
