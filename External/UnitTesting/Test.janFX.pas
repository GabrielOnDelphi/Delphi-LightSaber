UNIT Test.janFX;

{=====================================================
   2026.02.22
   GabrielMoraru.com

   DUnitX tests for janFX.pas
   Tests utility functions, color filters, flip/mirror/rotate,
   blend, noise, effects, and parameter validation (crash prevention).

   All tests create small pf24bit bitmaps to keep execution fast.
=====================================================}

INTERFACE

USES
  System.SysUtils, System.Classes, System.Types, System.Math,
  Vcl.Graphics,
  DUnitX.TestFramework,
  janFX;

TYPE
  [TestFixture]
  TTestJanFXUtils = class
  public
    [Test] procedure Test_IntToByte_Clamping;
    [Test] procedure Test_TrimInt_Clamping;
    [Test] procedure Test_Set255_Clamping;
    [Test] procedure Test_ConvertColor_ValidRange;
    [Test] procedure Test_ConvertColor_OutOfRange;
  end;

  [TestFixture]
  TTestJanFXBitmap = class
  private
    FSrc: TBitmap;
    FDst: TBitmap;
    { Creates a small test bitmap with known pixel values }
    procedure SetupBitmap(Bmp: TBitmap; W, H: Integer; R, G, B: Byte);
    function GetPixelRGB(Bmp: TBitmap; X, Y: Integer; out R, G, B: Byte): Boolean;
  public
    [Setup]    procedure Setup;
    [TearDown] procedure TearDown;

    { Flip / Mirror / Rotate }
    [Test] procedure Test_FlipDown_Flips;
    [Test] procedure Test_FlipRight_Flips;
    [Test] procedure Test_MirrorRight_Mirrors;
    [Test] procedure Test_MirrorDown_Mirrors;
    [Test] procedure Test_TurnLeft_Rotates;
    [Test] procedure Test_TurnRight_Rotates;

    { Color/Light }
    [Test] procedure Test_GrayScale_MakesGray;
    [Test] procedure Test_InvertColors_Inverts;
    [Test] procedure Test_Contrast_ZeroNoOp;
    [Test] procedure Test_Lightness_ZeroNoOp;
    [Test] procedure Test_Darkness_ZeroNoOp;
    [Test] procedure Test_Saturation_255NoOp;
    [Test] procedure Test_Contrast_Modifies;
    [Test] procedure Test_Lightness_Modifies;
    [Test] procedure Test_Darkness_Modifies;

    { Blend }
    [Test] procedure Test_Blend_ZeroAmount;
    [Test] procedure Test_Blend_FullAmount;

    { Noise }
    [Test] procedure Test_AddColorNoise_NoAmount;
    [Test] procedure Test_AddMonoNoise_NoAmount;
    [Test] procedure Test_GaussianBlur_SinglePass;

    { Effects }
    [Test] procedure Test_Emboss_NoCrash;
    [Test] procedure Test_Mosaic_NoCrash;
    [Test] procedure Test_Wave_NoCrash;
    [Test] procedure Test_Tile_NoCrash;
    [Test] procedure Test_AntiAlias_NoCrash;
    [Test] procedure Test_Twist_NoCrash;
    [Test] procedure Test_FishEye_NoCrash;
    [Test] procedure Test_SmoothRotate_NoCrash;
    [Test] procedure Test_Solorize_NoCrash;
    [Test] procedure Test_Posterize_NoCrash;
    [Test] procedure Test_Trace_NoCrash;
    [Test] procedure Test_SplitBlur_NoCrash;

    { Color channel filters }
    [Test] procedure Test_KeepRed;
    [Test] procedure Test_KeepGreen;
    [Test] procedure Test_KeepBlue;

    { Marble }
    [Test] procedure Test_Marble_NoCrash;

    { Fractals }
    [Test] procedure Test_MandelBrot_NoCrash;
    [Test] procedure Test_DrawMandelJulia_NoCrash;

    { Convolution }
    [Test] procedure Test_ConvolveFilter_AllTypes;

    { Shadows }
    [Test] procedure Test_Shadows_NoCrash;

    { Parameter validation / crash prevention }
    [Test] procedure Test_Mosaic_ZeroSize;
    [Test] procedure Test_Posterize_ZeroAmount;
    [Test] procedure Test_FishEye_ZeroAmount;
    [Test] procedure Test_Wave_ZeroAmount;
    [Test] procedure Test_MakeSeamlessClip_ZeroSeam;
    [Test] procedure Test_Marble_ZeroTurbulence;

    { CopyBMP }
    [Test] procedure Test_CopyBMP;

    { Seamless }
    [Test] procedure Test_Seamless_NoCrash;

    { TexturizeTile/Overlap }
    [Test] procedure Test_TexturizeTile_NoCrash;
    [Test] procedure Test_TexturizeOverlap_NoCrash;
  end;


IMPLEMENTATION

{=====================================================
   TTestJanFXUtils - Pure function tests
=====================================================}

procedure TTestJanFXUtils.Test_IntToByte_Clamping;
begin
  { IntToByte is private, but we test via Set255 which has similar clamping }
  Assert.AreEqual(0,   Set255(-100), 'Negative should clamp to 0');
  Assert.AreEqual(0,   Set255(0),    '0 should stay 0');
  Assert.AreEqual(128, Set255(128),  'Mid-range should pass through');
  Assert.AreEqual(255, Set255(255),  '255 should stay 255');
  Assert.AreEqual(255, Set255(500),  'Above 255 should clamp to 255');
end;


procedure TTestJanFXUtils.Test_TrimInt_Clamping;
begin
  Assert.AreEqual(10,  TrimInt(5, 10, 20),  'Below min should clamp to min');
  Assert.AreEqual(10,  TrimInt(10, 10, 20), 'At min should stay');
  Assert.AreEqual(15,  TrimInt(15, 10, 20), 'Mid-range should pass through');
  Assert.AreEqual(20,  TrimInt(20, 10, 20), 'At max should stay');
  Assert.AreEqual(20,  TrimInt(99, 10, 20), 'Above max should clamp to max');
end;


procedure TTestJanFXUtils.Test_Set255_Clamping;
begin
  Assert.AreEqual(0,   Set255(-1),  'Negative should clamp to 0');
  Assert.AreEqual(1,   Set255(1),   '1 should stay 1');
  Assert.AreEqual(254, Set255(254), '254 should stay 254');
  Assert.AreEqual(255, Set255(255), '255 should clamp to 255');
  Assert.AreEqual(255, Set255(1000),'Large values should clamp to 255');
end;


procedure TTestJanFXUtils.Test_ConvertColor_ValidRange;
begin
  Assert.AreEqual(Integer(clBlack), Integer(ConvertColor(0)),  'Color 0 = black');
  Assert.AreEqual(Integer(clWhite), Integer(ConvertColor(15)), 'Color 15 = white');
  Assert.AreEqual(Integer(clRed),   Integer(ConvertColor(4)),  'Color 4 = red');
end;


procedure TTestJanFXUtils.Test_ConvertColor_OutOfRange;
begin
  Assert.AreEqual(Integer(clWhite), Integer(ConvertColor(99)),  'Out of range should return white');
  Assert.AreEqual(Integer(clWhite), Integer(ConvertColor(-1)),  'Negative should return white');
end;


{=====================================================
   TTestJanFXBitmap - Bitmap operation tests
=====================================================}

procedure TTestJanFXBitmap.SetupBitmap(Bmp: TBitmap; W, H: Integer; R, G, B: Byte);
var
  x, y: Integer;
  p: PByteArray;
begin
  Bmp.PixelFormat:= pf24bit;
  Bmp.SetSize(W, H);
  for y:= 0 to H - 1 do
  begin
    p:= Bmp.ScanLine[y];
    for x:= 0 to W - 1 do
    begin
      p[x * 3]    := B;  { BGR order }
      p[x * 3 + 1]:= G;
      p[x * 3 + 2]:= R;
    end;
  end;
end;


function TTestJanFXBitmap.GetPixelRGB(Bmp: TBitmap; X, Y: Integer; out R, G, B: Byte): Boolean;
var
  p: PByteArray;
begin
  if (X < 0) or (X >= Bmp.Width) or (Y < 0) or (Y >= Bmp.Height) then EXIT(False);
  p:= Bmp.ScanLine[Y];
  B:= p[X * 3];
  G:= p[X * 3 + 1];
  R:= p[X * 3 + 2];
  Result:= True;
end;


procedure TTestJanFXBitmap.Setup;
begin
  FSrc:= TBitmap.Create;
  FDst:= TBitmap.Create;
  SetupBitmap(FSrc, 32, 32, 100, 150, 200);
  SetupBitmap(FDst, 32, 32, 0, 0, 0);
end;


procedure TTestJanFXBitmap.TearDown;
begin
  FreeAndNil(FSrc);
  FreeAndNil(FDst);
end;


{ --- Flip / Mirror / Rotate --- }

procedure TTestJanFXBitmap.Test_FlipDown_Flips;
var
  Bmp: TBitmap;
  p: PByteArray;
begin
  { Create a 4x4 bitmap where top row is red, bottom row is blue }
  Bmp:= TBitmap.Create;
  TRY
    Bmp.PixelFormat:= pf24bit;
    Bmp.SetSize(4, 4);
    { Set top row (y=0) to red }
    p:= Bmp.ScanLine[0];
    p[0]:= 0; p[1]:= 0; p[2]:= 255;  { pixel 0: R=255,G=0,B=0 in BGR }
    { Set bottom row (y=3) to blue }
    p:= Bmp.ScanLine[3];
    p[0]:= 255; p[1]:= 0; p[2]:= 0;  { pixel 0: R=0,G=0,B=255 in BGR }

    FlipDown(Bmp);

    { After flip, top row should have what was on bottom }
    p:= Bmp.ScanLine[0];
    Assert.AreEqual(Byte(255), p[0], 'After FlipDown, top row blue channel should be 255');
    { And bottom row should have what was on top }
    p:= Bmp.ScanLine[3];
    Assert.AreEqual(Byte(255), p[2], 'After FlipDown, bottom row red channel should be 255');
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


procedure TTestJanFXBitmap.Test_FlipRight_Flips;
var
  Bmp: TBitmap;
  p: PByteArray;
begin
  Bmp:= TBitmap.Create;
  TRY
    Bmp.PixelFormat:= pf24bit;
    Bmp.SetSize(4, 2);
    { Set leftmost pixel to red, rightmost to blue }
    p:= Bmp.ScanLine[0];
    p[0]:= 0; p[1]:= 0; p[2]:= 255;       { x=0: Red }
    p[9]:= 255; p[10]:= 0; p[11]:= 0;      { x=3: Blue }

    FlipRight(Bmp);

    p:= Bmp.ScanLine[0];
    Assert.AreEqual(Byte(255), p[0], 'After FlipRight, leftmost pixel blue channel should be 255');
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


procedure TTestJanFXBitmap.Test_MirrorRight_Mirrors;
var
  r, g, b, r2, g2, b2: Byte;
begin
  { After MirrorRight, left half should be mirrored to right }
  MirrorRight(FSrc);
  GetPixelRGB(FSrc, 0, 0, r, g, b);
  GetPixelRGB(FSrc, FSrc.Width - 1, 0, r2, g2, b2);
  Assert.AreEqual(r, r2, 'MirrorRight: leftmost and rightmost pixels should match');
  Assert.AreEqual(g, g2);
  Assert.AreEqual(b, b2);
end;


procedure TTestJanFXBitmap.Test_MirrorDown_Mirrors;
var
  r, g, b, r2, g2, b2: Byte;
begin
  MirrorDown(FSrc);
  GetPixelRGB(FSrc, 0, 0, r, g, b);
  GetPixelRGB(FSrc, 0, FSrc.Height - 1, r2, g2, b2);
  Assert.AreEqual(r, r2, 'MirrorDown: top and bottom pixels should match');
  Assert.AreEqual(g, g2);
  Assert.AreEqual(b, b2);
end;


procedure TTestJanFXBitmap.Test_TurnLeft_Rotates;
begin
  TurnLeft(FSrc, FDst);
  Assert.AreEqual(FSrc.Height, FDst.Width, 'TurnLeft: dst width should equal src height');
  Assert.AreEqual(FSrc.Width, FDst.Height, 'TurnLeft: dst height should equal src width');
end;


procedure TTestJanFXBitmap.Test_TurnRight_Rotates;
begin
  TurnRight(FSrc, FDst);
  Assert.AreEqual(FSrc.Height, FDst.Width, 'TurnRight: dst width should equal src height');
  Assert.AreEqual(FSrc.Width, FDst.Height, 'TurnRight: dst height should equal src width');
end;


{ --- Color/Light --- }

procedure TTestJanFXBitmap.Test_GrayScale_MakesGray;
var
  r, g, b: Byte;
begin
  GrayScale(FSrc);
  GetPixelRGB(FSrc, 0, 0, r, g, b);
  Assert.AreEqual(r, g, 'GrayScale: R and G channels should be equal');
  Assert.AreEqual(g, b, 'GrayScale: G and B channels should be equal');
end;


procedure TTestJanFXBitmap.Test_InvertColors_Inverts;
var
  r, g, b: Byte;
begin
  { FSrc pixels are R=100, G=150, B=200 }
  InvertColors(FSrc);
  GetPixelRGB(FSrc, 0, 0, r, g, b);
  Assert.AreEqual(Byte(155), r, 'InvertColors: R should be 255-100=155');
  Assert.AreEqual(Byte(105), g, 'InvertColors: G should be 255-150=105');
  Assert.AreEqual(Byte(55),  b, 'InvertColors: B should be 255-200=55');
end;


procedure TTestJanFXBitmap.Test_Contrast_ZeroNoOp;
var
  r, g, b: Byte;
begin
  Contrast(FSrc, 0);  { amount=0 should be no-op }
  GetPixelRGB(FSrc, 0, 0, r, g, b);
  Assert.AreEqual(Byte(100), r, 'Contrast(0): R should be unchanged');
  Assert.AreEqual(Byte(150), g, 'Contrast(0): G should be unchanged');
  Assert.AreEqual(Byte(200), b, 'Contrast(0): B should be unchanged');
end;


procedure TTestJanFXBitmap.Test_Lightness_ZeroNoOp;
var
  r, g, b: Byte;
begin
  Lightness(FSrc, 0);
  GetPixelRGB(FSrc, 0, 0, r, g, b);
  Assert.AreEqual(Byte(100), r, 'Lightness(0): R should be unchanged');
end;


procedure TTestJanFXBitmap.Test_Darkness_ZeroNoOp;
var
  r, g, b: Byte;
begin
  Darkness(FSrc, 0);
  GetPixelRGB(FSrc, 0, 0, r, g, b);
  Assert.AreEqual(Byte(100), r, 'Darkness(0): R should be unchanged');
end;


procedure TTestJanFXBitmap.Test_Saturation_255NoOp;
var
  r, g, b: Byte;
begin
  Saturation(FSrc, 255);  { 255 = normal saturation, should be no-op }
  GetPixelRGB(FSrc, 0, 0, r, g, b);
  Assert.AreEqual(Byte(100), r, 'Saturation(255): R should be unchanged');
  Assert.AreEqual(Byte(150), g, 'Saturation(255): G should be unchanged');
  Assert.AreEqual(Byte(200), b, 'Saturation(255): B should be unchanged');
end;


procedure TTestJanFXBitmap.Test_Contrast_Modifies;
var
  r, g, b: Byte;
begin
  Contrast(FSrc, 128);
  GetPixelRGB(FSrc, 0, 0, r, g, b);
  { With amount=128, pixels should change from original values }
  Assert.AreNotEqual(Byte(100), r, 'Contrast(128): R should be modified');
end;


procedure TTestJanFXBitmap.Test_Lightness_Modifies;
var
  r, g, b: Byte;
begin
  Lightness(FSrc, 128);
  GetPixelRGB(FSrc, 0, 0, r, g, b);
  Assert.IsTrue(r > 100, 'Lightness(128): R should increase');
end;


procedure TTestJanFXBitmap.Test_Darkness_Modifies;
var
  r, g, b: Byte;
begin
  Darkness(FSrc, 128);
  GetPixelRGB(FSrc, 0, 0, r, g, b);
  Assert.IsTrue(r < 100, 'Darkness(128): R should decrease');
end;


{ --- Blend --- }

procedure TTestJanFXBitmap.Test_Blend_ZeroAmount;
var
  Src2: TBitmap;
  r, g, b: Byte;
begin
  Src2:= TBitmap.Create;
  TRY
    SetupBitmap(Src2, 32, 32, 0, 0, 0);
    Blend(FSrc, Src2, FDst, 0.0);  { 0 = fully src1 }
    GetPixelRGB(FDst, 0, 0, r, g, b);
    Assert.AreEqual(Byte(100), r, 'Blend(0): should be fully src1');
    Assert.AreEqual(Byte(150), g);
    Assert.AreEqual(Byte(200), b);
  FINALLY
    FreeAndNil(Src2);
  END;
end;


procedure TTestJanFXBitmap.Test_Blend_FullAmount;
var
  Src2: TBitmap;
  r, g, b: Byte;
begin
  Src2:= TBitmap.Create;
  TRY
    SetupBitmap(Src2, 32, 32, 50, 60, 70);
    Blend(FSrc, Src2, FDst, 1.0);  { 1.0 = fully src2 }
    GetPixelRGB(FDst, 0, 0, r, g, b);
    Assert.AreEqual(Byte(50), r, 'Blend(1.0): should be fully src2');
    Assert.AreEqual(Byte(60), g);
    Assert.AreEqual(Byte(70), b);
  FINALLY
    FreeAndNil(Src2);
  END;
end;


{ --- Noise --- }

procedure TTestJanFXBitmap.Test_AddColorNoise_NoAmount;
var
  r, g, b: Byte;
begin
  { amount=0 means Random(0) which returns 0, so no change }
  AddColorNoise(FSrc, 0);
  GetPixelRGB(FSrc, 0, 0, r, g, b);
  Assert.AreEqual(Byte(100), r, 'AddColorNoise(0): should not change pixels');
end;


procedure TTestJanFXBitmap.Test_AddMonoNoise_NoAmount;
var
  r, g, b: Byte;
begin
  AddMonoNoise(FSrc, 0);
  GetPixelRGB(FSrc, 0, 0, r, g, b);
  Assert.AreEqual(Byte(100), r, 'AddMonoNoise(0): should not change pixels');
end;


procedure TTestJanFXBitmap.Test_GaussianBlur_SinglePass;
begin
  { Just ensure no crash on a small image }
  GaussianBlur(FSrc, 1, 1);
  Assert.IsTrue(FSrc.Width > 0, 'GaussianBlur should not destroy the bitmap');
end;


{ --- Effects (crash tests) --- }

procedure TTestJanFXBitmap.Test_Emboss_NoCrash;
begin
  Emboss(FSrc);
  Assert.IsTrue(FSrc.Width > 0);
end;


procedure TTestJanFXBitmap.Test_Mosaic_NoCrash;
begin
  Mosaic(FSrc, 5);
  Assert.IsTrue(FSrc.Width > 0);
end;


procedure TTestJanFXBitmap.Test_Wave_NoCrash;
begin
  Wave(FSrc, 3, 1, 0);
  Assert.IsTrue(FSrc.Width > 0);
end;


procedure TTestJanFXBitmap.Test_Tile_NoCrash;
begin
  Tile(FSrc, FDst, 2);
  Assert.AreEqual(FSrc.Width, FDst.Width, 'Tile: dst width should match src');
end;


procedure TTestJanFXBitmap.Test_AntiAlias_NoCrash;
begin
  AntiAlias(FSrc);
  Assert.IsTrue(FSrc.Width > 0);
end;


procedure TTestJanFXBitmap.Test_Twist_NoCrash;
begin
  FDst.Assign(FSrc);
  Twist(FSrc, FDst, 50);
  Assert.IsTrue(FDst.Width > 0);
end;


procedure TTestJanFXBitmap.Test_FishEye_NoCrash;
begin
  FDst.Assign(FSrc);
  FishEye(FSrc, FDst, 0.5);
  Assert.IsTrue(FDst.Width > 0);
end;


procedure TTestJanFXBitmap.Test_SmoothRotate_NoCrash;
begin
  FDst.PixelFormat:= pf24bit;
  FDst.SetSize(FSrc.Width, FSrc.Height);
  SmoothRotate(FSrc, FDst, FSrc.Width div 2, FSrc.Height div 2, 45.0);
  Assert.IsTrue(FDst.Width > 0);
end;


procedure TTestJanFXBitmap.Test_Solorize_NoCrash;
begin
  FDst.PixelFormat:= pf24bit;
  FDst.SetSize(FSrc.Width, FSrc.Height);
  Solorize(FSrc, FDst, 128);
  Assert.IsTrue(FDst.Width > 0);
end;


procedure TTestJanFXBitmap.Test_Posterize_NoCrash;
begin
  FDst.PixelFormat:= pf24bit;
  FDst.SetSize(FSrc.Width, FSrc.Height);
  Posterize(FSrc, FDst, 32);
  Assert.IsTrue(FDst.Width > 0);
end;


procedure TTestJanFXBitmap.Test_Trace_NoCrash;
begin
  Trace(FSrc, 1);
  Assert.IsTrue(FSrc.Width > 0);
end;


procedure TTestJanFXBitmap.Test_SplitBlur_NoCrash;
begin
  SplitBlur(FSrc, 2);
  Assert.IsTrue(FSrc.Width > 0);
end;


{ --- Color channel --- }

procedure TTestJanFXBitmap.Test_KeepRed;
var
  r, g, b: Byte;
begin
  KeepRed(FSrc, 1.0);
  GetPixelRGB(FSrc, 0, 0, r, g, b);
  Assert.AreEqual(Byte(100), r, 'KeepRed: R should remain');
  Assert.AreEqual(Byte(0), g, 'KeepRed: G should be zeroed');
  Assert.AreEqual(Byte(0), b, 'KeepRed: B should be zeroed');
end;


procedure TTestJanFXBitmap.Test_KeepGreen;
var
  r, g, b: Byte;
begin
  KeepGreen(FSrc, 1.0);
  GetPixelRGB(FSrc, 0, 0, r, g, b);
  Assert.AreEqual(Byte(0), r, 'KeepGreen: R should be zeroed');
  Assert.AreEqual(Byte(150), g, 'KeepGreen: G should remain');
  Assert.AreEqual(Byte(0), b, 'KeepGreen: B should be zeroed');
end;


procedure TTestJanFXBitmap.Test_KeepBlue;
var
  r, g, b: Byte;
begin
  KeepBlue(FSrc, 1.0);
  GetPixelRGB(FSrc, 0, 0, r, g, b);
  Assert.AreEqual(Byte(0), r, 'KeepBlue: R should be zeroed');
  Assert.AreEqual(Byte(0), g, 'KeepBlue: G should be zeroed');
  Assert.AreEqual(Byte(200), b, 'KeepBlue: B should remain');
end;


{ --- Marble --- }

procedure TTestJanFXBitmap.Test_Marble_NoCrash;
begin
  FDst.Assign(FSrc);
  marble(FSrc, FDst, 5.0, 10);
  Assert.IsTrue(FDst.Width > 0);
end;


{ --- Fractals --- }

procedure TTestJanFXBitmap.Test_MandelBrot_NoCrash;
begin
  MandelBrot(FSrc, 32);
  Assert.IsTrue(FSrc.Width > 0);
end;


procedure TTestJanFXBitmap.Test_DrawMandelJulia_NoCrash;
begin
  DrawMandelJulia(FSrc, -2.0, -1.25, 1.25, 1.25, 32, True);
  Assert.IsTrue(FSrc.Width > 0);
end;


{ --- Convolution --- }

procedure TTestJanFXBitmap.Test_ConvolveFilter_AllTypes;
var
  i, j: Integer;
  Bmp: TBitmap;
begin
  { Test all 9 filter types with all 3 edge types }
  for i:= 0 to 8 do
    for j:= 0 to 2 do
    begin
      Bmp:= TBitmap.Create;
      TRY
        SetupBitmap(Bmp, 16, 16, 100, 150, 200);
        ConvolveFilter(i, j, Bmp);
        Assert.IsTrue(Bmp.Width > 0, Format('ConvolveFilter(%d,%d) should not crash', [i, j]));
      FINALLY
        FreeAndNil(Bmp);
      END;
    end;
end;


{ --- Shadows --- }

procedure TTestJanFXBitmap.Test_Shadows_NoCrash;
var
  Bmp: TBitmap;
begin
  Bmp:= TBitmap.Create;
  TRY
    SetupBitmap(Bmp, 32, 32, 128, 128, 128);
    ShadowDownRight(Bmp);
    Assert.IsTrue(Bmp.Width > 0, 'ShadowDownRight should not crash');
  FINALLY
    FreeAndNil(Bmp);
  END;

  Bmp:= TBitmap.Create;
  TRY
    SetupBitmap(Bmp, 32, 32, 128, 128, 128);
    ShadowDownLeft(Bmp);
    Assert.IsTrue(Bmp.Width > 0, 'ShadowDownLeft should not crash');
  FINALLY
    FreeAndNil(Bmp);
  END;

  Bmp:= TBitmap.Create;
  TRY
    SetupBitmap(Bmp, 32, 32, 128, 128, 128);
    shadowUpRight(Bmp);
    Assert.IsTrue(Bmp.Width > 0, 'shadowUpRight should not crash');
  FINALLY
    FreeAndNil(Bmp);
  END;

  Bmp:= TBitmap.Create;
  TRY
    SetupBitmap(Bmp, 32, 32, 128, 128, 128);
    shadowUpLeft(Bmp);
    Assert.IsTrue(Bmp.Width > 0, 'shadowUpLeft should not crash');
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


{ --- Parameter validation / crash prevention --- }

procedure TTestJanFXBitmap.Test_Mosaic_ZeroSize;
begin
  { size=0 should not crash (guarded by EXIT) }
  Mosaic(FSrc, 0);
  Assert.IsTrue(FSrc.Width > 0, 'Mosaic(0) should exit gracefully');
end;


procedure TTestJanFXBitmap.Test_Posterize_ZeroAmount;
begin
  { amount=0 would cause division by zero without guard }
  FDst.PixelFormat:= pf24bit;
  FDst.SetSize(FSrc.Width, FSrc.Height);
  Posterize(FSrc, FDst, 0);
  Assert.IsTrue(True, 'Posterize(0) should not crash');
end;


procedure TTestJanFXBitmap.Test_FishEye_ZeroAmount;
begin
  { amount=0 would cause division by zero without guard }
  FDst.Assign(FSrc);
  FishEye(FSrc, FDst, 0);
  Assert.IsTrue(True, 'FishEye(0) should not crash');
end;


procedure TTestJanFXBitmap.Test_Wave_ZeroAmount;
begin
  { amount=0 would cause division by zero in fangle }
  Wave(FSrc, 0, 1, 0);
  Assert.IsTrue(True, 'Wave(0) should not crash');
end;


procedure TTestJanFXBitmap.Test_MakeSeamlessClip_ZeroSeam;
begin
  { seam=0 would cause division by zero }
  MakeSeamlessClip(FSrc, 0);
  Assert.IsTrue(True, 'MakeSeamlessClip(0) should not crash');
end;


procedure TTestJanFXBitmap.Test_Marble_ZeroTurbulence;
begin
  { turbulence=0 would cause division by zero in mod }
  FDst.Assign(FSrc);
  marble(FSrc, FDst, 5.0, 0);
  Assert.IsTrue(True, 'marble(turbulence=0) should not crash');
end;


{ --- CopyBMP --- }

procedure TTestJanFXBitmap.Test_CopyBMP;
var
  r, g, b: Byte;
begin
  CopyBMP(FDst, FSrc);
  Assert.AreEqual(FSrc.Width,  FDst.Width,  'CopyBMP: width should match');
  Assert.AreEqual(FSrc.Height, FDst.Height, 'CopyBMP: height should match');
  Assert.AreEqual(pf24bit, FDst.PixelFormat, 'CopyBMP: should force pf24bit');
  GetPixelRGB(FDst, 0, 0, r, g, b);
  Assert.AreEqual(Byte(100), r, 'CopyBMP: pixel R should match source');
end;


{ --- Seamless --- }

procedure TTestJanFXBitmap.Test_Seamless_NoCrash;
begin
  Seamless(FSrc, 4);
  Assert.IsTrue(FSrc.Width > 0, 'Seamless should not crash');
end;


{ --- TexturizeTile/Overlap --- }

procedure TTestJanFXBitmap.Test_TexturizeTile_NoCrash;
begin
  TexturizeTile(FSrc, 8);
  Assert.IsTrue(FSrc.Width > 0, 'TexturizeTile should not crash');
end;


procedure TTestJanFXBitmap.Test_TexturizeOverlap_NoCrash;
begin
  TexturizeOverlap(FSrc, 8);
  Assert.IsTrue(FSrc.Width > 0, 'TexturizeOverlap should not crash');
end;


end.
