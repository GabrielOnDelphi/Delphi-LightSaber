UNIT LightVcl.Graph.ResizeGr32;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   Image resizers based on Graphics32 (GR32) Transform library.

   REQUIREMENTS:
      Input bitmap must be pf24bit.
      Requires Graphics32 library: github.com/graphics32/graphics32

   PERFORMANCE:
      Resize down: GR32.Transform is ~6x faster than JanFX.Stretch (0.23s vs 1.45s)
      Resize up: GR32 is ~5.5x slower and produces sharper (less smooth) results.
      Recommendation: Use GR32 for downscaling, other resamplers for upscaling.

   KERNELS:
      LanczosKernel (7) and MitchellKernel (5) produce best quality results.
      SplineKernel (3) has known issues - do not use.

   ToDo: Try additional algorithms from:
     http://delphi-kb.blogspot.de/2011/05/resize-tbitmap-quickly.html
     http://www.davdata.nl/math/bmresize.html

   Tester: c:\MyProjects\Projects GRAPHICS Resamplers\GLOBAL Tester\
-------------------------------------------------------------------------------------------------------------}

INTERFACE
{ $I GR32.Inc}

USES
  System.SysUtils, System.Classes, Vcl.Graphics, System.Types,
  GR32, GR32_Transforms, GR32_Resamplers,
  LightCore;

CONST
  { Resamplers - determines the interpolation method }
  NearestResampler = 0;   { Fastest, lowest quality - pixelated results }
  LinearResampler  = 1;   { Fast, basic bilinear interpolation }
  DraftResampler   = 2;   { Fast draft quality }
  KernelResampler  = 3;   { Highest quality - uses kernel functions below }

  { Kernels - to be used with KernelResampler only }
  BoxKernel          = 00;   { Jagged edges }
  LinearKernel       = 01;   { Jagged edges }
  CosineKernel       = 02;   { Slightly jagged }
  SplineKernel       = 03;   { WARNING: Known issues - do not use! }
  CubicKernel        = 04;   { Good quality }
  MitchellKernel     = 05;   { Recommended - good balance of sharpness and smoothness }
  AlbrechtKernel     = 06;   { Good quality }
  LanczosKernel      = 07;   { Recommended - best quality, slightly slower }
  GaussianKernel     = 08;   { Smooth, may blur edges }
  BlackmanKernel     = 09;   { Good quality }
  HannKernel         = 10;   { Good quality }
  HammingKernel      = 11;   { Good quality }
  SinshKernel        = 12;   { Good quality }
  HermiteKernel      = 13;   { Good quality }

  { Default kernel for best quality - LanczosKernel recommended over SplineKernel }
  DefaultKernel      = LanczosKernel;


TYPE
  TGr32Stretch= class(TPersistent)
  private
  protected
    Src: TBitmap32;
    Dst: TBitmap32;
    AffineTransformation: TAffineTransformation;
    Transformation: TTransformation;
  public
    ScaleX, ScaleY: Extended;           { Scale multiplier: 1.0 -> No magnification. 1.5 -> The image is enlarged with 50%, etc }
    constructor Create(Resampler, Kernel: Integer);
    destructor Destroy; override;
    procedure StretchImage(Image: string; ExifRotate: Boolean);     overload;
    procedure StretchImage(BMP : TBitmap);     overload;
  end;

{ Quick resize using GR32. ScaleX/ScaleY are multipliers (1.0 = no change, 2.0 = double size).
  Uses KernelResampler with LanczosKernel by default for best quality. }
procedure StretchGr32(BMP: TBitmap; ScaleX, ScaleY: Double; aResampler: Byte = KernelResampler; aKernel: Byte = DefaultKernel);


IMPLEMENTATION

USES LightCore.Math, LightVcl.Graph.Loader;


{ Creates a GR32-based image stretcher with specified resampler and kernel.
  Resampler: Use NearestResampler..KernelResampler constants.
  Kernel: Use BoxKernel..HermiteKernel constants (only used with KernelResampler).
  Recommended: KernelResampler with LanczosKernel or MitchellKernel. }
constructor TGr32Stretch.Create(Resampler, Kernel: Integer);
begin
 inherited Create;

 ScaleX:= 1;
 ScaleY:= 1;
 Src:= NIL;
 Dst:= NIL;
 AffineTransformation:= NIL;

 TRY
   Src:= TBitmap32.Create;
   Dst:= TBitmap32.Create;

   { Set resampler on source bitmap }
   TCustomResamplerClass(ResamplerList[Resampler]).Create(Src);

   { If using KernelResampler, set the kernel function }
   if Src.Resampler is TKernelResampler
   then TKernelResampler(Src.Resampler).Kernel:= TCustomKernelClass(KernelList[Kernel]).Create;

   { Create affine transformation for scaling }
   AffineTransformation:= TAffineTransformation.Create;
   Transformation:= AffineTransformation;
 EXCEPT
   FreeAndNil(AffineTransformation);
   FreeAndNil(Dst);
   FreeAndNil(Src);
   RAISE;
 END;
end;


destructor TGr32Stretch.Destroy;
begin
 FreeAndNil(AffineTransformation);
 FreeAndNil(Src);
 FreeAndNil(Dst);
 inherited;
end;



{ Loads image from file and stretches it according to ScaleX/ScaleY.
  ExifRotate: If TRUE, auto-rotates JPEG images based on EXIF orientation.
  Raises exception if file cannot be loaded. }
procedure TGr32Stretch.StretchImage(Image: string; ExifRotate: Boolean);
VAR Loader: TBitmap;
begin
 Assert(Image <> '', 'StretchImage: Image filename cannot be empty');

 Loader:= LoadGraph(Image, ExifRotate, True);
 if Loader = NIL
 then RAISE Exception.Create('StretchImage: Failed to load image: ' + Image);

 TRY
   StretchImage(Loader);
 FINALLY
   FreeAndNil(Loader);
 END;
end;



{ Stretches bitmap in-place according to ScaleX/ScaleY multipliers.
  BMP must be pf24bit format. ScaleX/ScaleY must be > 0.
  Example: ScaleX=2.0, ScaleY=2.0 doubles the image size. }
procedure TGr32Stretch.StretchImage(BMP: TBitmap);
VAR H, W: Integer;
begin
  Assert(BMP <> NIL, 'StretchImage: BMP parameter cannot be nil');

  if BMP.PixelFormat <> pf24bit
  then RAISE Exception.Create('StretchImage: GR32 requires pf24bit pixel format');

  if ScaleX <= 0
  then RAISE Exception.Create('StretchImage: ScaleX must be > 0, got: ' + Real2Str(ScaleX));

  if ScaleY <= 0
  then RAISE Exception.Create('StretchImage: ScaleY must be > 0, got: ' + Real2Str(ScaleY));

  { Convert VCL bitmap to GR32 bitmap }
  Src.Assign(BMP);
  Dst.SetSize(RoundEx(BMP.Width * ScaleX), RoundEx(BMP.Height * ScaleY));

  { Configure transformation }
  AffineTransformation.Clear;
  AffineTransformation.SrcRect:= FloatRect(0, 0, Src.Width - 1, Src.Height - 1);

  { Apply scaling if needed }
  if (ScaleX <> 1) OR (ScaleY <> 1)
  then AffineTransformation.Scale(ScaleX, ScaleY);

  { Perform the transformation }
  Dst.BeginUpdate;
  Dst.Clear(clRed32);  { Clear with red to make any gaps visible during debugging }
  Transform(Dst, Src, Transformation);
  Dst.EndUpdate;

  { Get the actual transformed bounds }
  W:= RoundEx(AffineTransformation.GetTransformedBounds.Right);
  H:= RoundEx(AffineTransformation.GetTransformedBounds.Bottom);

  { Copy result back to input bitmap }
  BMP.SetSize(W, H);
  BMP.Canvas.CopyMode:= cmSrcCopy;
  BMP.Canvas.CopyRect(Rect(0, 0, W, H), Dst.Canvas, Rect(0, 0, W, H));
end;




{ Quick one-call resize using GR32.
  ScaleX/ScaleY: Multipliers (1.0 = no change, 2.0 = double, 0.5 = half).
  aResampler: Resampling method (default: KernelResampler for best quality).
  aKernel: Kernel function (default: LanczosKernel for best quality).
  BMP must be pf24bit format. }
procedure StretchGr32(BMP: TBitmap; ScaleX, ScaleY: Double; aResampler: Byte = KernelResampler; aKernel: Byte = DefaultKernel);
VAR Gr32: TGr32Stretch;
begin
  Assert(BMP <> NIL, 'StretchGr32: BMP parameter cannot be nil');

  Gr32:= TGr32Stretch.Create(aResampler, aKernel);
  TRY
    Gr32.ScaleX:= ScaleX;
    Gr32.ScaleY:= ScaleY;
    Gr32.StretchImage(BMP);
  FINALLY
    FreeAndNil(Gr32);
  END;
end;



end.



