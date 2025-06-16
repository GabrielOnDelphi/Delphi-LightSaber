UNIT LightVcl.Graph.ResizeGr32;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------

   Image resizers
   Based on GR32.Transform
   The input must be pf24bit.

   SPEED:
      Resize down:
        GR32.Transform needs 0.23sec.
        JanFX.Stretch needs 1.45 sec for same image.   (GR32 6 times faster than JanFX.Stretch)

      Resize up:
        However, when resizing up, GR32 is 5.5 times slower (and also too sharp).   (Both tested with best resamplers)

   ToDo: This page lists multiple algorithms. Try them:
     http://delphi-kb.blogspot.de/2011/05/resize-tbitmap-quickly.html
     http://www.davdata.nl/math/bmresize.html

   Tester: c:\MyProjects\Projects GRAPHICS Resamplers\GLOBAL Tester\
--------------------------------------------------------------------------------------------------}

INTERFACE
{ $I GR32.Inc}

USES
  System.SysUtils, System.Classes, Vcl.Graphics, System.Types,
  GR32, GR32_Transforms, GR32_Resamplers, LightCore;

CONST
  { Resamplers }
  NearestResampler = 0;
  LinearResampler  = 1;
  DraftResampler   = 2;
  KernelResampler  = 3;

  { Kernels - to be used with KernelResampler }
  BoxKernel          = 00;                                                                         { Jagged }
  LinearKernel       = 01;                                                                         { Jagged }
  CosineKernel       = 02;                                                                         { Little jagged }
  SplineKernel       = 03;                                                                         { Do not use! The documentation says it is an issue with it. }
  CubicKernel        = 04;                                                                         { 4 to 13 they are all the same }
  MitchellKernel     = 05;
  AlbrechtKernel     = 06;
  LanczosKernel      = 07;
  GaussianKernel     = 08;
  BlackmanKernel     = 09;
  HannKernel         = 10;
  HammingKernel      = 11;
  SinshKernel        = 12;
  HermiteKernel      = 13;


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

procedure StretchGr32(BMP: TBitmap; ScaleX, ScaleY: Double; aKernelResampler: Byte = KernelResampler; aSplineKernel: Byte = SplineKernel);


IMPLEMENTATION

USES LightCore.Math, LightVcl.Graph.Loader;


constructor TGr32Stretch.Create(Resampler, Kernel: Integer);
begin
 inherited Create;

 ScaleX:= 1;
 ScaleY:= 1;
 Src:= TBitmap32.Create;
 Dst:= TBitmap32.Create;

 { Resampler }
 TCustomResamplerClass(ResamplerList[Resampler]).Create(Src);
 if Src.Resampler is TKernelResampler
 then TKernelResampler(Src.Resampler).Kernel:= TCustomKernelClass(KernelList[Kernel]).Create;      { 5 an 7 is a good kernel }

 { Transformations }
 AffineTransformation := TAffineTransformation.Create;
 Transformation       := AffineTransformation;
end;


destructor TGr32Stretch.Destroy;
begin
 FreeAndNil(AffineTransformation);
 FreeAndNil(Src);
 FreeAndNil(Dst);
 inherited;
end;



{ MAIN }
procedure TGr32Stretch.StretchImage(Image: string; ExifRotate: Boolean);
VAR Loader: TBitmap;
begin
 Loader:= LoadGraph(Image, ExifRotate, True);
 TRY
   StretchImage(Loader);
 FINALLY
   FreeAndNil(Loader);
 END;
end;



procedure TGr32Stretch.StretchImage(BMP: TBitmap);
VAR H, W: Integer;
begin
  if BMP.PixelFormat <> pf24bit then RAISE Exception.Create('StretchGr32 requires pf24bit');
  if (ScaleX <= 0) then RAISE Exception.Create('Invalid scaling factor X! '+ Real2Str(ScaleX));
  if (ScaleY <= 0) then RAISE Exception.Create('Invalid scaling factor Y! '+ Real2Str(ScaleY));

  Src.Assign(BMP);
  Dst.SetSize(RoundEx(BMP.Width* ScaleX), RoundEx(BMP.Height* ScaleY));

  AffineTransformation.Clear;
  AffineTransformation.SrcRect:= FloatRect(0, 0, Src.Width - 1, Src.Height - 1);

  { I need to recompute the rotation center if I resize the image }
  if (ScaleX <> 1) OR (ScaleY <> 1)
  then AffineTransformation.Scale (ScaleX, ScaleY);

  Dst.BeginUpdate;
  Dst.Clear(clRed32);
  Transform(Dst, Src, Transformation);
  Dst.EndUpdate;

  W:= RoundEx(AffineTransformation.GetTransformedBounds.Right);
  H:= RoundEx(AffineTransformation.GetTransformedBounds.Bottom);

  BMP.SetSize(w, h);    // BMP.Assign(Dst) del
  BMP.Canvas.CopyMode:= cmSrcCopy;         { default }
  BMP.Canvas.CopyRect(rect(0, 0, W, H), Dst.Canvas, rect(0, 0, W, H));
end;




{ QUICK }
procedure StretchGr32(BMP: TBitmap; ScaleX, ScaleY:Double; aKernelResampler: Byte = KernelResampler; aSplineKernel: Byte = SplineKernel);
VAR Gr32: TGr32Stretch;
begin
  Gr32:= TGr32Stretch.Create(aKernelResampler, aSplineKernel);
  TRY
    Gr32.ScaleX:= ScaleX;
    Gr32.ScaleY:= ScaleY;
    Gr32.StretchImage(BMP);
  FINALLY
   FreeAndNil(Gr32);
  END;
end;



end.



