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
  Vcl.Graphics;

type
  [TestFixture]
  TTestGraphResizeGr32 = class
  private
    FBitmap: TBitmap;
    procedure CreateTestBitmap(Width, Height: Integer);
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
    procedure TestConstants_DefaultKernelIsLanczos;

    [Test]
    procedure TestConstants_ResamplerValues;

    [Test]
    procedure TestConstants_KernelValues;
  end;

implementation

uses
  LightVcl.Graph.ResizeGr32;


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


procedure TTestGraphResizeGr32.TestCreate_WithKernelResampler;
var
  Gr32: TGr32Stretch;
begin
  Assert.WillNotRaise(
    procedure
    begin
      Gr32:= TGr32Stretch.Create(KernelResampler, LanczosKernel);
      FreeAndNil(Gr32);
    end);
end;


procedure TTestGraphResizeGr32.TestCreate_WithLinearResampler;
var
  Gr32: TGr32Stretch;
begin
  Assert.WillNotRaise(
    procedure
    begin
      Gr32:= TGr32Stretch.Create(LinearResampler, BoxKernel);
      FreeAndNil(Gr32);
    end);
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
      EAssertionFailed);
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
      Exception);
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
      Exception);
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
      Exception);
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
      EAssertionFailed);
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
    EAssertionFailed);
end;


procedure TTestGraphResizeGr32.TestStretchGr32_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      StretchGr32(FBitmap, 1.0, 1.0);
    end);
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

  Assert.WillNotRaise(
    procedure
    begin
      StretchGr32(FBitmap, 2.0, 2.0, NearestResampler, BoxKernel);
    end);

  Assert.AreEqual(200, FBitmap.Width, 'Width should double');
end;


procedure TTestGraphResizeGr32.TestStretchGr32_WithLanczosKernel;
begin
  CreateTestBitmap(100, 100);

  Assert.WillNotRaise(
    procedure
    begin
      StretchGr32(FBitmap, 1.5, 1.5, KernelResampler, LanczosKernel);
    end);

  Assert.AreEqual(150, FBitmap.Width, 'Width should be 150');
end;


procedure TTestGraphResizeGr32.TestStretchGr32_WithMitchellKernel;
begin
  CreateTestBitmap(100, 100);

  Assert.WillNotRaise(
    procedure
    begin
      StretchGr32(FBitmap, 1.5, 1.5, KernelResampler, MitchellKernel);
    end);

  Assert.AreEqual(150, FBitmap.Width, 'Width should be 150');
end;


{ Constants Tests }

procedure TTestGraphResizeGr32.TestConstants_DefaultKernelIsLanczos;
begin
  Assert.AreEqual(LanczosKernel, DefaultKernel, 'DefaultKernel should be LanczosKernel');
end;


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
