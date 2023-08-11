UNIT cGraphFx.RotateGr32;

{=============================================================================================================
   Gabriel Moraru
   2023.08.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------
  The best function I have found is RotateBitmapGR32 (antialising, fastest).
  However, it seems that RotateBitmapGDIP is even better but it doesn't work.

  Functionalities:
   * Rotate image
   * Has antialising

  AdjustSize parameter:
    True : then the size of BMP will adjusted to hold the entire rotated image.
    False: then the size of BMP will remain the same, thereofre the rotated image will be cropped.

  Tester:
    c:\MyProjects\Projects GRAPHICS\Rotate, flip\RotateTester.dpr
    c:\MyProjects\Packages\CubicCommonControls\cGraphRotate.pas conclusions.png
==================================================================================================}

//  see: f:\Pas\MMedia Rotate Image Component\

INTERFACE
{ $I GR32.Inc}

USES
  System.SysUtils, Vcl.Graphics, GR32, GR32_Transforms, GR32_Resamplers, cGraphResize32;


procedure RotateBitmapGR32 (Bmp: TBitmap32; Angle: Single; AdjustSize: Boolean= True; BkColor: TColor = clPurple; Transparent: Boolean = FALSE; ResamplerKernel: Integer= HermiteKernel); overload;
procedure RotateBitmapGR32 (Bmp: TBitmap;   Angle: Single; AdjustSize: Boolean= True; BkColor: TColor = clPurple; Transparent: Boolean = FALSE; ResamplerKernel: Integer= HermiteKernel); overload;
procedure RotateBitmapGR32 (Source, Destination: TBitmap; Angle: Single; X, Y: Integer; ResamplerClass: integer= 3; ResamplerKernel: Integer= HermiteKernel);          overload;{ Angle in Deg }


IMPLEMENTATION


{--------------------------------------------------------------------------------------------------
   ROTATE
   We rotate the Src image and we paste transparently (this means that the empty triangle resulted from rotation are transparent) in Dest at the XY coordinates
--------------------------------------------------------------------------------------------------}
procedure RotateBitmapGR32(Source, Destination: TBitmap; Angle: Single; X, Y: Integer; ResamplerClass: integer= 3; ResamplerKernel: integer= HermiteKernel);   { Angle in Deg }
var
   Src: TBitmap32;
   Dst: TBitmap32;
   RotCenterX, RotCenterY: Extended;   // rotation center
   AffineTransformation: TAffineTransformation;
begin
 Source.PixelFormat:= pf24bit; { Doesn't work without this! I tried. }

 { Create  }
 Src:= TBitmap32.Create;
 Dst:= TBitmap32.Create;
 AffineTransformation := TAffineTransformation.Create;
 TRY
  SetBorderTransparent(Src, Src.BoundsRect);                                          { make the Source border pixels transparent while keeping their RGB components }
  { Resampler }
  TCustomResamplerClass(ResamplerList[ResamplerClass]).Create(Src);
  if Src.Resampler is TKernelResampler
  then TKernelResampler(Src.Resampler).Kernel:= TCustomKernelClass(KernelList[ResamplerKernel]).Create;   { 5= one of the kernels }
  Src.Assign(Source);

  { Rotate image }
  AffineTransformation.Clear;
  AffineTransformation.SrcRect:= FloatRect(0, 0, Src.Width - 1, Src.Height - 1);
  RotCenterX := Src.Width  * 0.5;     { Rotation coordinates. 0.5 means the middle of the image }
  RotCenterY := Src.Height * 0.5;

  if Angle <> 0
  then AffineTransformation.Rotate(RotCenterX, RotCenterY, Angle);

  { Shift image }
  { The rotated image will take more space. Shift it so it won't go out of 'viewport' }
  AffineTransformation.Translate( -AffineTransformation.GetTransformedBounds.Left, -AffineTransformation.GetTransformedBounds.Top);
  Dst.SetSizeFrom(Destination);
  Dst.Assign(Destination);
  Dst.BeginUpdate;
  Dst.Clear($00000000);
  Transform(Dst, Src, AffineTransformation);
  Dst.EndUpdate;

 { We reuse here the source to decrease memory footprint.
   We move the background in the source, and then we paint over it the destination.
   In the end we put the mixed result in OutputBMP }
  Src.Assign(Destination);
  Dst.CombineMode:= cmBlend;
  Dst.DrawMode:= dmBlend;
  Src.Draw(X, Y, Dst);                                                { http://graphics32.org/documentation/Docs/Units/GR32/Classes/TCustomBitmap32/Methods/Draw.htm }
  Destination.Assign(Src);
 FINALLY
  FreeAndNil(AffineTransformation);
  FreeAndNil(Src);
  FreeAndNil(Dst);
 END;
end;




procedure RotateBitmapGR32(Bmp: TBitmap32; Angle: Single; AdjustSize: Boolean= True; BkColor: TColor = clPurple; Transparent: Boolean = FALSE; ResamplerKernel: Integer= HermiteKernel);
VAR
  Dst: TBitmap32;
  Transformation: TAffineTransformation;
begin
  SetBorderTransparent(Bmp, Bmp.BoundsRect);                                                       { Added by me }

  Dst := TBitmap32.Create;
  Transformation := TAffineTransformation.Create;
  TRY
    { Setup transformation }
    Transformation.BeginUpdate;
    Transformation.SrcRect := FloatRect(0, 0, Bmp.Width, Bmp.Height);
    Transformation.Translate(-0.5 * Bmp.Width, -0.5 * Bmp.Height);
    Transformation.Rotate(0, 0, -Angle);

    if AdjustSize
    then
      with Transformation.GetTransformedBounds
       DO Dst.SetSize(Round(Right - Left), Round(Bottom - Top))
    else
      Dst.SetSize(Bmp.Width, Bmp.Height);

    Transformation.Translate(0.5 * Dst.Width, 0.5 * Dst.Height);
    Transformation.EndUpdate;

    { Resampler }                                                        { Added by me }
    Bmp.BeginUpdate;
    TCustomResamplerClass(ResamplerList[KernelResampler]).Create(Bmp);
    if Bmp.Resampler is TKernelResampler
    then TKernelResampler(Bmp.Resampler).Kernel:= TCustomKernelClass(KernelList[ResamplerKernel]).Create;   { 5= one of the kernels }
    Bmp.EndUpdate;

    Dst.Clear(Color32(BkColor));

    if NOT Transparent
    then Bmp.DrawMode := dmTransparent;

    { Transform }
    Transform(Dst, Bmp, Transformation);

    { Output }
    Bmp.OuterColor := Color32(BkColor);
    Bmp.Assign(Dst);

    if Transparent
    then Bmp.DrawMode := dmTransparent;
  FINALLY
    FreeAndNil(Transformation);
    FreeAndNil(Dst);
  end;
end;



procedure RotateBitmapGR32 (Bmp: TBitmap; Angle: Single; AdjustSize: Boolean= True; BkColor: TColor = clPurple; Transparent: Boolean = FALSE; ResamplerKernel: Integer= HermiteKernel);
VAR
  Dest: TBitmap32;
begin
  Dest := TBitmap32.Create;
  TRY
    //Bmp.Transparent;
    Dest.Assign(Bmp);
    RotateBitmapGR32(Dest, Angle, AdjustSize, BkColor, Transparent, ResamplerKernel);
    Bmp.Assign(Dest);
    Bmp.Transparent := Transparent;
  FINALLY
    FreeAndNil(Dest);
  end;
end;


end.
