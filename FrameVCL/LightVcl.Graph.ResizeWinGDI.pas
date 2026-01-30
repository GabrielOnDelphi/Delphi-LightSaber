UNIT LightVcl.Graph.ResizeWinGDI;

{=============================================================================================================
   Gabriel Moraru
   2026.01.30
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Image resizer using Windows GDI+ API.

   Quality: High quality bicubic interpolation with pixel offset and smoothing.
   Performance: Slower than StretchBlt but produces smoother results for some images.
   Requirements: GDI+ (available on Windows XP and later).

   Tester: c:\MyProjects\Projects GRAPHICS Resamplers\GLOBAL Tester\
--------------------------------------------------------------------------------------------------}

INTERFACE

USES
  Winapi.Windows,
  System.SysUtils, Vcl.Graphics;

{ Resizes Source bitmap to OutWidth x OutHeight and stores result in Dest.
  Uses GDI+ high quality interpolation. Both Source and Dest must be valid bitmaps.
  Dest will be resized and its contents replaced. }
procedure ResizeBitmapGDI(Source, Dest: TBitmap; OutWidth, OutHeight: Integer);


IMPLEMENTATION

USES
  GDIPAPI,
  GDIPOBJ;


{ Resizes a bitmap using GDI+ high quality interpolation.
  Source: Input bitmap to resize.
  Dest: Output bitmap that will receive the resized image.
  OutWidth/OutHeight: Target dimensions (must be > 0). }
procedure ResizeBitmapGDI(Source, Dest: TBitmap; OutWidth, OutHeight: Integer);
var
  GpSrc, GpDst: TGPBitmap;
  Graphics: TGPGraphics;
  HBmp: HBITMAP;
begin
  Assert(Source <> NIL, 'ResizeBitmapGDI: Source bitmap cannot be nil');
  Assert(Dest <> NIL, 'ResizeBitmapGDI: Dest bitmap cannot be nil');
  Assert(OutWidth > 0, 'ResizeBitmapGDI: OutWidth must be > 0');
  Assert(OutHeight > 0, 'ResizeBitmapGDI: OutHeight must be > 0');

  GpSrc:= NIL;
  GpDst:= NIL;
  Graphics:= NIL;

  TRY
    { Create GDI+ bitmap from source VCL bitmap handle }
    GpSrc:= TGPBitmap.Create(Source.Handle, 0);

    { Create destination GDI+ bitmap with target dimensions }
    GpDst:= TGPBitmap.Create(OutWidth, OutHeight);

    { Create graphics context for drawing }
    Graphics:= TGPGraphics.Create(GpDst);

    { Configure for highest quality output }
    Graphics.SetInterpolationMode(InterpolationModeHighQuality);
    Graphics.SetPixelOffsetMode(PixelOffsetModeHighQuality);
    Graphics.SetSmoothingMode(SmoothingModeHighQuality);

    { Draw scaled image }
    Graphics.DrawImage(GpSrc, 0, 0, GpDst.GetWidth, GpDst.GetHeight);

    { Convert back to VCL bitmap }
    GpDst.GetHBITMAP(0, HBmp);
    Dest.Handle:= HBmp;
  FINALLY
    FreeAndNil(Graphics);
    FreeAndNil(GpDst);
    FreeAndNil(GpSrc);
  END;
end;


end.



