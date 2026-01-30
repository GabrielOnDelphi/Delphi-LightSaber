UNIT LightVcl.Graph.FX.RotateGr32;

{=============================================================================================================
   Gabriel Moraru
   2026.01.30
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
  High-quality image rotation using Graphics32 library with anti-aliasing support.
  The best function I have found is RotateBitmapGR32 (antialiasing, fastest).

  Functionalities:
   * Rotate image at arbitrary angles
   * Has antialiasing via configurable resampler kernels

  AdjustSize parameter:
    True : The output bitmap will be resized to hold the entire rotated image without cropping.
    False: The output bitmap keeps original dimensions; the rotated image will be cropped.

  Transparent parameter:
    Controls whether the output bitmap has transparency enabled after rotation.
    When True, empty areas (corners) from rotation remain transparent.
    When False, they are filled with BkColor.

  ResamplerKernel parameter:
    Controls the resampling quality. Use constants from LightVcl.Graph.ResizeGr32:
    HermiteKernel (13) is the default - good balance of quality and speed.
    See LightVcl.Graph.ResizeGr32 for all available kernels.

  Tester:
    c:\MyProjects\Projects GRAPHICS\Rotate, flip\RotateTester.dpr
    c:\MyProjects\LightSaber\cGraphRotate.pas conclusions.png
==================================================================================================}

INTERFACE
{ $I GR32.Inc}

USES
  System.SysUtils, System.Types, Vcl.Graphics,
  LightVcl.Graph.ResizeGr32, GR32, GR32_Transforms, GR32_Resamplers;


procedure RotateBitmapGR32 (Bmp: TBitmap32; Angle: Single; AdjustSize: Boolean= True; BkColor: TColor = clPurple; Transparent: Boolean = FALSE; ResamplerKernel: Integer= HermiteKernel); overload;
procedure RotateBitmapGR32 (Bmp: TBitmap;   Angle: Single; AdjustSize: Boolean= True; BkColor: TColor = clPurple; Transparent: Boolean = FALSE; ResamplerKernel: Integer= HermiteKernel); overload;
procedure RotateBitmapGR32 (Source, Destination: TBitmap; Angle: Single; X, Y: Integer; ResamplerClass: integer= 3; ResamplerKernel: Integer= HermiteKernel);          overload;{ Angle in Deg }


IMPLEMENTATION


{--------------------------------------------------------------------------------------------------
   ROTATE AND COMPOSITE

   Rotates the Source image by the specified Angle (in degrees), then composites it
   transparently onto the Destination bitmap at coordinates (X, Y).
   The empty triangular areas from rotation remain transparent during blending.

   Parameters:
     Source          - Input bitmap to rotate (will be converted to pf24bit)
     Destination     - Target bitmap where the rotated image will be composited
     Angle           - Rotation angle in degrees (positive = counter-clockwise)
     X, Y            - Position on Destination where rotated image will be drawn
     ResamplerClass  - Resampler type (default: KernelResampler=3 for best quality)
     ResamplerKernel - Kernel for KernelResampler (default: HermiteKernel)
--------------------------------------------------------------------------------------------------}
procedure RotateBitmapGR32(Source, Destination: TBitmap; Angle: Single; X, Y: Integer; ResamplerClass: integer= 3; ResamplerKernel: integer= HermiteKernel);
var
   Src: TBitmap32;
   Dst: TBitmap32;
   RotCenterX, RotCenterY: Extended;
   AffineTransformation: TAffineTransformation;
begin
 Assert(Source <> NIL, 'RotateBitmapGR32: Source bitmap cannot be nil');
 Assert(Destination <> NIL, 'RotateBitmapGR32: Destination bitmap cannot be nil');

 Source.PixelFormat:= pf24bit; { GR32 requires 24-bit or 32-bit pixel format }

 Src:= TBitmap32.Create;
 Dst:= TBitmap32.Create;
 AffineTransformation:= TAffineTransformation.Create;
 TRY
  { First assign the source, then configure resampler and transparency }
  Src.Assign(Source);

  { Configure resampler for quality anti-aliasing }
  TCustomResamplerClass(ResamplerList[ResamplerClass]).Create(Src);
  if Src.Resampler is TKernelResampler
  then TKernelResampler(Src.Resampler).Kernel:= TCustomKernelClass(KernelList[ResamplerKernel]).Create;

  { Make border pixels transparent to avoid edge artifacts during rotation }
  SetBorderTransparent(Src, Src.BoundsRect);

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




{--------------------------------------------------------------------------------------------------
   ROTATE IN PLACE (TBitmap32)

   Rotates the input TBitmap32 in place by the specified angle.

   Parameters:
     Bmp             - Input/output TBitmap32 (modified in place)
     Angle           - Rotation angle in degrees (positive = clockwise)
     AdjustSize      - True: resize output to fit rotated image. False: keep original size (crops)
     BkColor         - Background color for empty areas after rotation
     Transparent     - Controls transparency mode of output bitmap
     ResamplerKernel - Kernel for quality anti-aliasing (default: HermiteKernel)
--------------------------------------------------------------------------------------------------}
procedure RotateBitmapGR32(Bmp: TBitmap32; Angle: Single; AdjustSize: Boolean= True; BkColor: TColor = clPurple; Transparent: Boolean = FALSE; ResamplerKernel: Integer= HermiteKernel);
VAR
  Dst: TBitmap32;
  Transformation: TAffineTransformation;
begin
  Assert(Bmp <> NIL, 'RotateBitmapGR32: Bitmap cannot be nil');

  { Make border pixels transparent to avoid edge artifacts during rotation }
  SetBorderTransparent(Bmp, Bmp.BoundsRect);

  Dst:= TBitmap32.Create;
  Transformation:= TAffineTransformation.Create;
  TRY
    { Setup affine transformation: translate to origin, rotate, then translate back }
    Transformation.BeginUpdate;
    Transformation.SrcRect:= FloatRect(0, 0, Bmp.Width, Bmp.Height);
    Transformation.Translate(-0.5 * Bmp.Width, -0.5 * Bmp.Height);
    Transformation.Rotate(0, 0, -Angle);

    if AdjustSize
    then Dst.SetSize(Round(Transformation.GetTransformedBounds.Right - Transformation.GetTransformedBounds.Left), Round(Transformation.GetTransformedBounds.Bottom - Transformation.GetTransformedBounds.Top))
    else Dst.SetSize(Bmp.Width, Bmp.Height);

    Transformation.Translate(0.5 * Dst.Width, 0.5 * Dst.Height);
    Transformation.EndUpdate;

    { Configure resampler for quality anti-aliasing }
    Bmp.BeginUpdate;
    TCustomResamplerClass(ResamplerList[KernelResampler]).Create(Bmp);
    if Bmp.Resampler is TKernelResampler
    then TKernelResampler(Bmp.Resampler).Kernel:= TCustomKernelClass(KernelList[ResamplerKernel]).Create;
    Bmp.EndUpdate;

    { Fill destination with background color }
    Dst.Clear(Color32(BkColor));

    { When NOT Transparent: use dmTransparent during transform for proper blending,
      but the final output will have BkColor in empty areas }
    if NOT Transparent
    then Bmp.DrawMode:= dmTransparent;

    { Perform the rotation transform }
    Transform(Dst, Bmp, Transformation);

    { Copy result back to input bitmap }
    Bmp.OuterColor:= Color32(BkColor);
    Bmp.Assign(Dst);

    { When Transparent: enable transparent drawing mode on the output }
    if Transparent
    then Bmp.DrawMode:= dmTransparent;
  FINALLY
    FreeAndNil(Transformation);
    FreeAndNil(Dst);
  end;
end;



{--------------------------------------------------------------------------------------------------
   ROTATE IN PLACE (TBitmap)

   Convenience wrapper that rotates a standard VCL TBitmap in place.
   Internally converts to TBitmap32, performs rotation, and converts back.

   Parameters:
     Bmp             - Input/output TBitmap (modified in place)
     Angle           - Rotation angle in degrees (positive = clockwise)
     AdjustSize      - True: resize output to fit rotated image. False: keep original size (crops)
     BkColor         - Background color for empty areas after rotation
     Transparent     - Controls Transparent property of output TBitmap
     ResamplerKernel - Kernel for quality anti-aliasing (default: HermiteKernel)
--------------------------------------------------------------------------------------------------}
procedure RotateBitmapGR32(Bmp: TBitmap; Angle: Single; AdjustSize: Boolean= True; BkColor: TColor = clPurple; Transparent: Boolean = FALSE; ResamplerKernel: Integer= HermiteKernel);
VAR
  Dest: TBitmap32;
begin
  Assert(Bmp <> NIL, 'RotateBitmapGR32: Bitmap cannot be nil');

  Dest:= TBitmap32.Create;
  TRY
    Dest.Assign(Bmp);
    RotateBitmapGR32(Dest, Angle, AdjustSize, BkColor, Transparent, ResamplerKernel);
    Bmp.Assign(Dest);
    Bmp.Transparent:= Transparent;
  FINALLY
    FreeAndNil(Dest);
  end;
end;


end.
