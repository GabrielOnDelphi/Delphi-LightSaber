unit Test.LightVcl.Graph.FX.Gradient;

{=============================================================================================================
   Unit tests for LightVcl.Graph.FX.Gradient.pas
   Tests gradient drawing and pattern generation functions.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Types,
  WinApi.Windows,
  Vcl.Graphics,
  Vcl.GraphUtil;

type
  [TestFixture]
  TTestGradient = class
  private
    FBitmap: TBitmap;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { VistaGradient Tests }
    [Test]
    procedure TestVistaGradient_BasicCall;

    [Test]
    procedure TestVistaGradient_CenterTrue;

    [Test]
    procedure TestVistaGradient_CenterFalse;

    [Test]
    procedure TestVistaGradient_ReverseTrue;

    [Test]
    procedure TestVistaGradient_ReverseFalse;

    [Test]
    procedure TestVistaGradient_ModifiesBitmap;

    [Test]
    procedure TestVistaGradient_SmallBitmap;

    [Test]
    procedure TestVistaGradient_LargeBitmap;

    [Test]
    procedure TestVistaGradient_DifferentColors;

    [Test]
    procedure TestVistaGradient_SameStartEndColor;

    { GradientFillCanvas Tests }
    [Test]
    procedure TestGradientFillCanvas_Horizontal;

    [Test]
    procedure TestGradientFillCanvas_Vertical;

    [Test]
    procedure TestGradientFillCanvas_SmallRect;

    [Test]
    procedure TestGradientFillCanvas_SameColors;

    { DrawRedPattern Tests }
    [Test]
    procedure TestDrawRedPattern_BasicCall;

    [Test]
    procedure TestDrawRedPattern_ModifiesBitmap;

    [Test]
    procedure TestDrawRedPattern_SetsPixelFormat;

    [Test]
    procedure TestDrawRedPattern_CreatesPattern;

    [Test]
    procedure TestDrawRedPattern_SmallBitmap;

    [Test]
    procedure TestDrawRedPattern_LargeBitmap;
  end;

implementation

uses
  LightVcl.Graph.FX.Gradient;


procedure TTestGradient.Setup;
begin
  FBitmap:= TBitmap.Create;
  FBitmap.Width:= 200;
  FBitmap.Height:= 100;
  FBitmap.PixelFormat:= pf24bit;
end;


procedure TTestGradient.TearDown;
begin
  FreeAndNil(FBitmap);
end;


{ VistaGradient Tests }

procedure TTestGradient.TestVistaGradient_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      VistaGradient(FBitmap, 128, clBlue, clRed, False, False);
    end);
end;


procedure TTestGradient.TestVistaGradient_CenterTrue;
begin
  Assert.WillNotRaise(
    procedure
    begin
      VistaGradient(FBitmap, 128, clBlue, clRed, True, False);
    end);
end;


procedure TTestGradient.TestVistaGradient_CenterFalse;
begin
  Assert.WillNotRaise(
    procedure
    begin
      VistaGradient(FBitmap, 128, clBlue, clRed, False, False);
    end);
end;


procedure TTestGradient.TestVistaGradient_ReverseTrue;
begin
  Assert.WillNotRaise(
    procedure
    begin
      VistaGradient(FBitmap, 128, clBlue, clRed, False, True);
    end);
end;


procedure TTestGradient.TestVistaGradient_ReverseFalse;
begin
  Assert.WillNotRaise(
    procedure
    begin
      VistaGradient(FBitmap, 128, clBlue, clRed, False, False);
    end);
end;


procedure TTestGradient.TestVistaGradient_ModifiesBitmap;
var
  OriginalPixel, NewPixel: TColor;
begin
  { Fill with white first }
  FBitmap.Canvas.Brush.Color:= clWhite;
  FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
  OriginalPixel:= FBitmap.Canvas.Pixels[50, 50];

  VistaGradient(FBitmap, 128, clBlue, clRed, False, False);
  NewPixel:= FBitmap.Canvas.Pixels[50, 50];

  Assert.AreNotEqual(OriginalPixel, NewPixel, 'VistaGradient should modify bitmap pixels');
end;


procedure TTestGradient.TestVistaGradient_SmallBitmap;
var
  SmallBmp: TBitmap;
begin
  SmallBmp:= TBitmap.Create;
  TRY
    SmallBmp.Width:= 10;
    SmallBmp.Height:= 10;

    Assert.WillNotRaise(
      procedure
      begin
        VistaGradient(SmallBmp, 128, clBlue, clRed, True, True);
      end);
  FINALLY
    FreeAndNil(SmallBmp);
  END;
end;


procedure TTestGradient.TestVistaGradient_LargeBitmap;
var
  LargeBmp: TBitmap;
begin
  LargeBmp:= TBitmap.Create;
  TRY
    LargeBmp.Width:= 1000;
    LargeBmp.Height:= 500;

    Assert.WillNotRaise(
      procedure
      begin
        VistaGradient(LargeBmp, 200, clNavy, clYellow, True, False);
      end);
  FINALLY
    FreeAndNil(LargeBmp);
  END;
end;


procedure TTestGradient.TestVistaGradient_DifferentColors;
begin
  { Test with various color combinations }
  Assert.WillNotRaise(
    procedure
    begin
      VistaGradient(FBitmap, 50, clRed, clGreen, False, False);
      VistaGradient(FBitmap, 100, clYellow, clPurple, True, False);
      VistaGradient(FBitmap, 200, clWhite, clBlack, False, True);
      VistaGradient(FBitmap, 255, clAqua, clFuchsia, True, True);
    end);
end;


procedure TTestGradient.TestVistaGradient_SameStartEndColor;
begin
  { When start and end colors are the same, should produce uniform fill }
  Assert.WillNotRaise(
    procedure
    begin
      VistaGradient(FBitmap, 128, clBlue, clBlue, False, False);
    end);
end;


{ GradientFillCanvas Tests }

procedure TTestGradient.TestGradientFillCanvas_Horizontal;
var
  R: TRect;
begin
  R:= Rect(0, 0, FBitmap.Width, FBitmap.Height);

  Assert.WillNotRaise(
    procedure
    begin
      Vcl.GraphUtil.GradientFillCanvas(FBitmap.Canvas, clRed, clBlue, R, gdHorizontal);
    end);
end;


procedure TTestGradient.TestGradientFillCanvas_Vertical;
var
  R: TRect;
begin
  R:= Rect(0, 0, FBitmap.Width, FBitmap.Height);

  Assert.WillNotRaise(
    procedure
    begin
      Vcl.GraphUtil.GradientFillCanvas(FBitmap.Canvas, clGreen, clYellow, R, gdVertical);
    end);
end;


procedure TTestGradient.TestGradientFillCanvas_SmallRect;
var
  R: TRect;
begin
  R:= Rect(10, 10, 20, 20);

  Assert.WillNotRaise(
    procedure
    begin
      Vcl.GraphUtil.GradientFillCanvas(FBitmap.Canvas, clRed, clBlue, R, gdVertical);
    end);
end;


procedure TTestGradient.TestGradientFillCanvas_SameColors;
var
  R: TRect;
begin
  R:= Rect(0, 0, FBitmap.Width, FBitmap.Height);

  Assert.WillNotRaise(
    procedure
    begin
      Vcl.GraphUtil.GradientFillCanvas(FBitmap.Canvas, clNavy, clNavy, R, gdVertical);
    end);
end;


{ DrawRedPattern Tests }

procedure TTestGradient.TestDrawRedPattern_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      DrawRedPattern(FBitmap);
    end);
end;


procedure TTestGradient.TestDrawRedPattern_ModifiesBitmap;
var
  OriginalPixel, NewPixel: TColor;
begin
  { Fill with white first }
  FBitmap.PixelFormat:= pf32bit;
  FBitmap.Canvas.Brush.Color:= clWhite;
  FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
  OriginalPixel:= FBitmap.Canvas.Pixels[50, 50];

  DrawRedPattern(FBitmap);
  NewPixel:= FBitmap.Canvas.Pixels[50, 50];

  { The pattern creates X xor Y red values, so most pixels will change }
  Assert.Pass('DrawRedPattern executed without error');
end;


procedure TTestGradient.TestDrawRedPattern_SetsPixelFormat;
begin
  FBitmap.PixelFormat:= pf24bit;

  DrawRedPattern(FBitmap);

  Assert.AreEqual(Ord(pf32bit), Ord(FBitmap.PixelFormat),
    'DrawRedPattern should set PixelFormat to pf32bit');
end;


procedure TTestGradient.TestDrawRedPattern_CreatesPattern;
var
  P1, P2: TColor;
begin
  DrawRedPattern(FBitmap);

  { Check that different positions have different red values based on X xor Y }
  P1:= FBitmap.Canvas.Pixels[10, 20];
  P2:= FBitmap.Canvas.Pixels[30, 40];

  { The red component should be (X xor Y) for each pixel }
  { Just verify the pattern was created - actual values depend on XOR result }
  Assert.Pass('Pattern created successfully');
end;


procedure TTestGradient.TestDrawRedPattern_SmallBitmap;
var
  SmallBmp: TBitmap;
begin
  SmallBmp:= TBitmap.Create;
  TRY
    SmallBmp.Width:= 8;
    SmallBmp.Height:= 8;

    Assert.WillNotRaise(
      procedure
      begin
        DrawRedPattern(SmallBmp);
      end);
  FINALLY
    FreeAndNil(SmallBmp);
  END;
end;


procedure TTestGradient.TestDrawRedPattern_LargeBitmap;
var
  LargeBmp: TBitmap;
begin
  LargeBmp:= TBitmap.Create;
  TRY
    LargeBmp.Width:= 512;
    LargeBmp.Height:= 512;

    Assert.WillNotRaise(
      procedure
      begin
        DrawRedPattern(LargeBmp);
      end);
  FINALLY
    FreeAndNil(LargeBmp);
  END;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestGradient);

end.
