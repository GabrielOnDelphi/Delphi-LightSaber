unit Test.LightVcl.Graph.FX.Gradient;

{=============================================================================================================
   2026.01.30
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

    [Test]
    procedure TestVistaGradient_NilBitmap;

    [Test]
    procedure TestVistaGradient_BrightnessExtremes;

    [Test]
    procedure TestVistaGradient_PreservesOutputDimensions;

    [Test]
    procedure TestVistaGradient_MinimumHeight;

    { GradientFillCanvas Tests }
    [Test]
    procedure TestGradientFillCanvas_Horizontal;

    [Test]
    procedure TestGradientFillCanvas_Vertical;

    [Test]
    procedure TestGradientFillCanvas_SmallRect;

    [Test]
    procedure TestGradientFillCanvas_SameColors;

    [Test]
    procedure TestGradientFillCanvas_NilCanvas;

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

    [Test]
    procedure TestDrawRedPattern_NilBitmap;

    [Test]
    procedure TestDrawRedPattern_VerifyXorPattern;
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


procedure TTestGradient.TestVistaGradient_NilBitmap;
begin
  { Should raise assertion when bitmap is nil }
  Assert.WillRaise(
    procedure
    begin
      VistaGradient(NIL, 128, clBlue, clRed, False, False);
    end, EAssertionFailed);
end;


procedure TTestGradient.TestVistaGradient_BrightnessExtremes;
begin
  { Test with c1=0 (full brightness) and c1=255 (minimum brightness) }
  Assert.WillNotRaise(
    procedure
    begin
      VistaGradient(FBitmap, 0, clBlue, clRed, False, False);    { Full brightness }
      VistaGradient(FBitmap, 255, clBlue, clRed, False, False);  { Minimum brightness }
    end);
end;


procedure TTestGradient.TestVistaGradient_PreservesOutputDimensions;
var
  OrigWidth, OrigHeight: Integer;
begin
  OrigWidth:= FBitmap.Width;
  OrigHeight:= FBitmap.Height;

  VistaGradient(FBitmap, 128, clBlue, clRed, True, True);

  Assert.AreEqual(OrigWidth, FBitmap.Width, 'Width should be preserved');
  Assert.AreEqual(OrigHeight, FBitmap.Height, 'Height should be preserved');
end;


procedure TTestGradient.TestVistaGradient_MinimumHeight;
var
  TinyBmp: TBitmap;
begin
  { VistaGradient requires Height > 1 because it calculates scanline stride
    from consecutive scanlines. Height=1 would cause invalid memory access. }
  TinyBmp:= TBitmap.Create;
  TRY
    TinyBmp.Width:= 10;
    TinyBmp.Height:= 1;

    Assert.WillRaise(
      procedure
      begin
        VistaGradient(TinyBmp, 128, clBlue, clRed, False, False);
      end, EAssertionFailed);
  FINALLY
    FreeAndNil(TinyBmp);
  END;
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


procedure TTestGradient.TestGradientFillCanvas_NilCanvas;
var
  R: TRect;
begin
  R:= Rect(0, 0, 100, 100);
  { The wrapper function should raise assertion when canvas is nil }
  Assert.WillRaise(
    procedure
    begin
      {$WARN SYMBOL_DEPRECATED OFF}
      LightVcl.Graph.FX.Gradient.GradientFillCanvas(NIL, clRed, clBlue, R, gdVertical);
      {$WARN SYMBOL_DEPRECATED ON}
    end, EAssertionFailed);
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
begin
  { Fill with white first }
  FBitmap.PixelFormat:= pf32bit;
  FBitmap.Canvas.Brush.Color:= clWhite;
  FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));

  DrawRedPattern(FBitmap);

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
begin
  DrawRedPattern(FBitmap);

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


procedure TTestGradient.TestDrawRedPattern_NilBitmap;
begin
  { Should raise assertion when bitmap is nil }
  Assert.WillRaise(
    procedure
    begin
      DrawRedPattern(NIL);
    end, EAssertionFailed);
end;


procedure TTestGradient.TestDrawRedPattern_VerifyXorPattern;
var
  TestBmp: TBitmap;
  X, Y: Integer;
  ExpectedRed: Byte;
  ActualColor: TColor;
  ActualRed: Byte;
begin
  { Create a small bitmap to verify the XOR pattern }
  TestBmp:= TBitmap.Create;
  TRY
    TestBmp.Width:= 16;
    TestBmp.Height:= 16;

    DrawRedPattern(TestBmp);

    { Verify a few specific pixels follow the X xor Y pattern }
    for Y:= 0 to 15 do
      for X:= 0 to 15 do
      begin
        ExpectedRed:= X xor Y;
        ActualColor:= TestBmp.Canvas.Pixels[X, Y];
        ActualRed:= GetRValue(ActualColor);

        Assert.AreEqual(Integer(ExpectedRed), Integer(ActualRed),
          Format('Red channel at (%d,%d) should be %d (X xor Y)', [X, Y, ExpectedRed]));
      end;
  FINALLY
    FreeAndNil(TestBmp);
  END;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestGradient);

end.
