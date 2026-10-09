unit Test.LightVcl.Graph.ResizeGr32;

{=============================================================================================================
   Unit tests for LightVcl.Graph.ResizeGr32.pas
   Tests GR32-based image resizing functions.

   Note: These tests require Graphics32 library to be installed.
   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Types,        { Rect }
  Vcl.Graphics;

type
  [TestFixture]
  TTestGraphResizeGr32 = class
  private
    FBitmap: TBitmap;
    procedure CreateTestBitmap(Width, Height: Integer);
    procedure PaintHalves;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { TGr32Stretch Constructor Tests }
    [Test]
    procedure TestCreate_DefaultValues;

    [Test]
    procedure TestCreate_WithKernelResampler;

    [Test]
    procedure TestCreate_WithLinearResampler;

    { TGr32Stretch.StretchImage(BMP) Tests }
    [Test]
    procedure TestStretchImage_NilBitmap;

    [Test]
    procedure TestStretchImage_InvalidPixelFormat;

    [Test]
    procedure TestStretchImage_InvalidScaleX;

    [Test]
    procedure TestStretchImage_InvalidScaleY;

    [Test]
    procedure TestStretchImage_NoScale;

    [Test]
    procedure TestStretchImage_ScaleUp;

    [Test]
    procedure TestStretchImage_ScaleDown;

    [Test]
    procedure TestStretchImage_NonUniformScale;

    { TGr32Stretch.StretchImage(FileName) Tests }
    [Test]
    procedure TestStretchImage_EmptyFilename;

    { StretchGr32 Procedure Tests }
    [Test]
    procedure TestStretchGr32_NilBitmap;

    [Test]
    procedure TestStretchGr32_BasicCall;

    [Test]
    procedure TestStretchGr32_DoubleSize;

    [Test]
    procedure TestStretchGr32_HalfSize;

    [Test]
    procedure TestStretchGr32_WithNearestResampler;

    [Test]
    procedure TestStretchGr32_WithLanczosKernel;

    [Test]
    procedure TestStretchGr32_WithMitchellKernel;

    { Constants Tests }
    [Test]
    procedure TestConstants_ResamplerValues;

    [Test]
    procedure TestConstants_KernelValues;
  end;

implementation

uses
  GR32, GR32_Resamplers,
  LightVcl.Graph.ResizeGr32;


type
  { Opens the protected Src bitmap, so a test can see which resampler the constructor installed }
  TGr32StretchAccess = class(TGr32Stretch);


function ColorHex(Color: TColor): string;
begin
  Result:= IntToHex(ColorToRGB(Color) and $FFFFFF, 6);
end;


function PixelHex(BMP: TBitmap; X, Y: Integer): string;
begin
  Result:= ColorHex(BMP.Canvas.Pixels[X, Y]);
end;


procedure TTestGraphResizeGr32.Setup;
begin
  FBitmap:= TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;  { GR32 requires pf24bit }
  FBitmap.Width:= 200;
  FBitmap.Height:= 100;
  FBitmap.Canvas.Brush.Color:= clWhite;
  FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
end;


procedure TTestGraphResizeGr32.TearDown;
begin
  FreeAndNil(FBitmap);
end;


procedure TTestGraphResizeGr32.CreateTestBitmap(Width, Height: Integer);
begin
  FreeAndNil(FBitmap);
  FBitmap:= TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Width:= Width;
  FBitmap.Height:= Height;
  FBitmap.Canvas.Brush.Color:= clWhite;
  FBitmap.Canvas.FillRect(Rect(0, 0, Width, Height));
end;


{ Left half lime, right half blue: a scaled image keeps both halves, a crop or a flip does not.
  No red: StretchImage clears its target with clRed32 before the transform, so red marks a pixel that was never drawn. }
procedure TTestGraphResizeGr32.PaintHalves;
begin
  FBitmap.Canvas.Brush.Color:= clLime;
  FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width DIV 2, FBitmap.Height));
  FBitmap.Canvas.Brush.Color:= clBlue;
  FBitmap.Canvas.FillRect(Rect(FBitmap.Width DIV 2, 0, FBitmap.Width, FBitmap.Height));
end;


{ TGr32Stretch Constructor Tests }

procedure TTestGraphResizeGr32.TestCreate_DefaultValues;
var
  Gr32: TGr32Stretch;
begin
  Gr32:= TGr32Stretch.Create(KernelResampler, LanczosKernel);
  TRY
    Assert.AreEqual(Extended(1.0), Gr32.ScaleX, 'Default ScaleX should be 1.0');
    Assert.AreEqual(Extended(1.0), Gr32.ScaleY, 'Default ScaleY should be 1.0');
  FINALLY
    FreeAndNil(Gr32);
  END;
end;


{ The expected classes are the ones GR32 registers at these indexes: initialization section of GR32_Resamplers.pas }
procedure TTestGraphResizeGr32.TestCreate_WithKernelResampler;
var
  Gr32: TGr32Stretch;
  Resampler: TCustomResampler;
begin
  Gr32:= TGr32Stretch.Create(KernelResampler, LanczosKernel);
  TRY
    Resampler:= TGr32StretchAccess(Gr32).Src.Resampler;
    Assert.IsNotNull(Resampler, 'The constructor must install a resampler on the source bitmap');
    Assert.AreEqual(TKernelResampler.ClassName, Resampler.ClassName, 'KernelResampler must install a TKernelResampler');
    Assert.AreEqual(TLanczosKernel.ClassName, TKernelResampler(Resampler).Kernel.ClassName, 'LanczosKernel must install a TLanczosKernel');
  FINALLY
    FreeAndNil(Gr32);
  END;
end;


procedure TTestGraphResizeGr32.TestCreate_WithLinearResampler;
var
  Gr32: TGr32Stretch;
  Resampler: TCustomResampler;
begin
  Gr32:= TGr32Stretch.Create(LinearResampler, BoxKernel);
  TRY
    Resampler:= TGr32StretchAccess(Gr32).Src.Resampler;
    Assert.IsNotNull(Resampler, 'The constructor must install a resampler on the source bitmap');
    { ClassName, not "is": TDraftResampler descends from TLinearResampler }
    Assert.AreEqual(TLinearResampler.ClassName, Resampler.ClassName, 'LinearResampler must install a TLinearResampler');
  FINALLY
    FreeAndNil(Gr32);
  END;
end;


{ TGr32Stretch.StretchImage(BMP) Tests }

procedure TTestGraphResizeGr32.TestStretchImage_NilBitmap;
var
  Gr32: TGr32Stretch;
begin
  Gr32:= TGr32Stretch.Create(KernelResampler, LanczosKernel);
  TRY
    Assert.WillRaise(
      procedure
      begin
        Gr32.StretchImage(TBitmap(NIL));
      end,
      EAssertionFailed,
      'Gr32.StretchImage(TBitmap(NIL)) must raise EAssertionFailed');
  FINALLY
    FreeAndNil(Gr32);
  END;
end;


procedure TTestGraphResizeGr32.TestStretchImage_InvalidPixelFormat;
var
  Gr32: TGr32Stretch;
begin
  FBitmap.PixelFormat:= pf32bit;  { GR32 requires pf24bit }

  Gr32:= TGr32Stretch.Create(KernelResampler, LanczosKernel);
  TRY
    Assert.WillRaise(
      procedure
      begin
        Gr32.StretchImage(FBitmap);
      end,
      Exception,
      'Gr32.StretchImage(FBitmap) must raise Exception');
  FINALLY
    FreeAndNil(Gr32);
  END;
end;


procedure TTestGraphResizeGr32.TestStretchImage_InvalidScaleX;
var
  Gr32: TGr32Stretch;
begin
  Gr32:= TGr32Stretch.Create(KernelResampler, LanczosKernel);
  TRY
    Gr32.ScaleX:= 0;  { Invalid }
    Gr32.ScaleY:= 1;

    Assert.WillRaise(
      procedure
      begin
        Gr32.StretchImage(FBitmap);
      end,
      Exception,
      'Gr32.StretchImage(FBitmap) must raise Exception');
  FINALLY
    FreeAndNil(Gr32);
  END;
end;


procedure TTestGraphResizeGr32.TestStretchImage_InvalidScaleY;
var
  Gr32: TGr32Stretch;
begin
  Gr32:= TGr32Stretch.Create(KernelResampler, LanczosKernel);
  TRY
    Gr32.ScaleX:= 1;
    Gr32.ScaleY:= -1;  { Invalid }

    Assert.WillRaise(
      procedure
      begin
        Gr32.StretchImage(FBitmap);
      end,
      Exception,
      'Gr32.StretchImage(FBitmap) must raise Exception');
  FINALLY
    FreeAndNil(Gr32);
  END;
end;


procedure TTestGraphResizeGr32.TestStretchImage_NoScale;
var
  Gr32: TGr32Stretch;
  OrigWidth, OrigHeight: Integer;
begin
  OrigWidth:= FBitmap.Width;
  OrigHeight:= FBitmap.Height;

  Gr32:= TGr32Stretch.Create(KernelResampler, LanczosKernel);
  TRY
    Gr32.ScaleX:= 1.0;
    Gr32.ScaleY:= 1.0;
    Gr32.StretchImage(FBitmap);

    Assert.AreEqual(OrigWidth, FBitmap.Width, 'Width should not change');
    Assert.AreEqual(OrigHeight, FBitmap.Height, 'Height should not change');
  FINALLY
    FreeAndNil(Gr32);
  END;
end;


procedure TTestGraphResizeGr32.TestStretchImage_ScaleUp;
var
  Gr32: TGr32Stretch;
begin
  CreateTestBitmap(100, 100);

  Gr32:= TGr32Stretch.Create(KernelResampler, LanczosKernel);
  TRY
    Gr32.ScaleX:= 2.0;
    Gr32.ScaleY:= 2.0;
    Gr32.StretchImage(FBitmap);

    Assert.AreEqual(200, FBitmap.Width, 'Width should double');
    Assert.AreEqual(200, FBitmap.Height, 'Height should double');
  FINALLY
    FreeAndNil(Gr32);
  END;
end;


procedure TTestGraphResizeGr32.TestStretchImage_ScaleDown;
var
  Gr32: TGr32Stretch;
begin
  CreateTestBitmap(200, 200);

  Gr32:= TGr32Stretch.Create(KernelResampler, LanczosKernel);
  TRY
    Gr32.ScaleX:= 0.5;
    Gr32.ScaleY:= 0.5;
    Gr32.StretchImage(FBitmap);

    Assert.AreEqual(100, FBitmap.Width, 'Width should halve');
    Assert.AreEqual(100, FBitmap.Height, 'Height should halve');
  FINALLY
    FreeAndNil(Gr32);
  END;
end;


procedure TTestGraphResizeGr32.TestStretchImage_NonUniformScale;
var
  Gr32: TGr32Stretch;
begin
  CreateTestBitmap(100, 100);

  Gr32:= TGr32Stretch.Create(KernelResampler, LanczosKernel);
  TRY
    Gr32.ScaleX:= 2.0;
    Gr32.ScaleY:= 0.5;
    Gr32.StretchImage(FBitmap);

    Assert.AreEqual(200, FBitmap.Width, 'Width should double');
    Assert.AreEqual(50, FBitmap.Height, 'Height should halve');
  FINALLY
    FreeAndNil(Gr32);
  END;
end;


{ TGr32Stretch.StretchImage(FileName) Tests }

procedure TTestGraphResizeGr32.TestStretchImage_EmptyFilename;
var
  Gr32: TGr32Stretch;
begin
  Gr32:= TGr32Stretch.Create(KernelResampler, LanczosKernel);
  TRY
    Assert.WillRaise(
      procedure
      begin
        Gr32.StretchImage('', True);
      end,
      EAssertionFailed,
      'Gr32.StretchImage(, True) must raise EAssertionFailed');
  FINALLY
    FreeAndNil(Gr32);
  END;
end;


{ StretchGr32 Procedure Tests }

procedure TTestGraphResizeGr32.TestStretchGr32_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      StretchGr32(NIL, 1.0, 1.0);
    end,
    EAssertionFailed,
    'StretchGr32(NIL, 1.0, 1.0) must raise EAssertionFailed');
end;


procedure TTestGraphResizeGr32.TestStretchGr32_BasicCall;
begin
  PaintHalves;   { The Setup bitmap is 200x100 }

  StretchGr32(FBitmap, 1.0, 1.0);

  Assert.AreEqual(200, FBitmap.Width,  'Width must not change at scale 1');
  Assert.AreEqual(100, FBitmap.Height, 'Height must not change at scale 1');
  Assert.AreEqual(ColorHex(clLime), PixelHex(FBitmap, 50, 50),  'The left half must stay lime');
  Assert.AreEqual(ColorHex(clBlue), PixelHex(FBitmap, 150, 50), 'The right half must stay blue');
end;


procedure TTestGraphResizeGr32.TestStretchGr32_DoubleSize;
begin
  CreateTestBitmap(100, 100);

  StretchGr32(FBitmap, 2.0, 2.0);

  Assert.AreEqual(200, FBitmap.Width, 'Width should double');
  Assert.AreEqual(200, FBitmap.Height, 'Height should double');
end;


procedure TTestGraphResizeGr32.TestStretchGr32_HalfSize;
begin
  CreateTestBitmap(200, 200);

  StretchGr32(FBitmap, 0.5, 0.5);

  Assert.AreEqual(100, FBitmap.Width, 'Width should halve');
  Assert.AreEqual(100, FBitmap.Height, 'Height should halve');
end;


procedure TTestGraphResizeGr32.TestStretchGr32_WithNearestResampler;
begin
  CreateTestBitmap(100, 100);
  PaintHalves;   { Lime in source columns 0..49, blue in 50..99 }

  StretchGr32(FBitmap, 2.0, 2.0, NearestResampler, BoxKernel);

  Assert.AreEqual(200, FBitmap.Width,  'Width should double');
  Assert.AreEqual(200, FBitmap.Height, 'Height should double');
  Assert.AreEqual(ColorHex(clLime), PixelHex(FBitmap, 50, 100),  'The left half must stay lime');
  Assert.AreEqual(ColorHex(clBlue), PixelHex(FBitmap, 150, 100), 'The right half must stay blue');
  { Nearest neighbour copies source pixels, so the border between the halves stays sharp; a filtering resampler blends these two columns }
  Assert.AreEqual(ColorHex(clLime), PixelHex(FBitmap, 99, 100),  'Column 99 must be pure lime (no blending)');
  Assert.AreEqual(ColorHex(clBlue), PixelHex(FBitmap, 100, 100), 'Column 100 must be pure blue (no blending)');
end;


procedure TTestGraphResizeGr32.TestStretchGr32_WithLanczosKernel;
begin
  CreateTestBitmap(100, 100);
  PaintHalves;

  StretchGr32(FBitmap, 1.5, 1.5, KernelResampler, LanczosKernel);

  Assert.AreEqual(150, FBitmap.Width,  'Width should be 150');
  Assert.AreEqual(150, FBitmap.Height, 'Height should be 150');
  Assert.AreEqual(ColorHex(clLime), PixelHex(FBitmap, 37, 75),  'The left half must stay lime');
  Assert.AreEqual(ColorHex(clBlue), PixelHex(FBitmap, 112, 75), 'The right half must stay blue');
end;


procedure TTestGraphResizeGr32.TestStretchGr32_WithMitchellKernel;
begin
  CreateTestBitmap(100, 100);
  PaintHalves;

  StretchGr32(FBitmap, 1.5, 1.5, KernelResampler, MitchellKernel);

  Assert.AreEqual(150, FBitmap.Width,  'Width should be 150');
  Assert.AreEqual(150, FBitmap.Height, 'Height should be 150');
  Assert.AreEqual(ColorHex(clLime), PixelHex(FBitmap, 37, 75),  'The left half must stay lime');
  Assert.AreEqual(ColorHex(clBlue), PixelHex(FBitmap, 112, 75), 'The right half must stay blue');
end;


{ Constants Tests }

procedure TTestGraphResizeGr32.TestConstants_ResamplerValues;
begin
  Assert.AreEqual(0, NearestResampler, 'NearestResampler should be 0');
  Assert.AreEqual(1, LinearResampler, 'LinearResampler should be 1');
  Assert.AreEqual(2, DraftResampler, 'DraftResampler should be 2');
  Assert.AreEqual(3, KernelResampler, 'KernelResampler should be 3');
end;


procedure TTestGraphResizeGr32.TestConstants_KernelValues;
begin
  Assert.AreEqual(0, BoxKernel, 'BoxKernel should be 0');
  Assert.AreEqual(3, SplineKernel, 'SplineKernel should be 3');
  Assert.AreEqual(5, MitchellKernel, 'MitchellKernel should be 5');
  Assert.AreEqual(7, LanczosKernel, 'LanczosKernel should be 7');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestGraphResizeGr32);

end.
