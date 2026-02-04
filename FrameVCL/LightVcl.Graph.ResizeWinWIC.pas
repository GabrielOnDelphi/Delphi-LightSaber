UNIT LightVcl.Graph.ResizeWinWIC;

{=============================================================================================================
   Gabriel Moraru
   2026.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Based on MS Windows WIC which is implemented in Delphi since 2010.

   Image quality:
     Same as StretchBlt but the time and RAM for WIC is way worse.
--------------------------------------------------------------------------------------------------------------

 TESTER:
      c:\Projects\LightSaber ImageResampler Test\ResamplerTester.dpr

-------------------------------------------------------------------------------------------------------------}
INTERFACE

USES
   System.Classes, System.SysUtils, Vcl.Graphics;

procedure ResizeBitmapWic(const Bitmap: TBitmap; const NewWidth, NewHeight: Integer); // pf32


IMPLEMENTATION

{ Resizes a bitmap using Windows Imaging Component (WIC).
  Uses wipmHighQualityCubic interpolation for best quality (bicubic interpolation).
  The bitmap is converted to pf32bit to preserve transparency for PNGs. }
procedure ResizeBitmapWic(const Bitmap: TBitmap; const NewWidth, NewHeight: Integer); // pf32
var
  vImage, v2: TWICImage;
begin
  Assert(Bitmap <> NIL, 'ResizeBitmapWic: Bitmap is NIL');
  Assert(NewWidth > 0, 'ResizeBitmapWic: NewWidth must be positive');
  Assert(NewHeight > 0, 'ResizeBitmapWic: NewHeight must be positive');

  v2:= NIL;
  vImage:= TWICImage.Create;
  try
    // Ensure the input is pf32bit to preserve transparency for PNGs
    if Bitmap.PixelFormat <> pf32bit
    then Bitmap.PixelFormat:= pf32bit;

    vImage.Assign(Bitmap);
    v2:= vImage.CreateScaledCopy(NewWidth, NewHeight, wipmHighQualityCubic);
    Bitmap.Assign(v2);
  finally
    FreeAndNil(v2);
    FreeAndNil(vImage);
  end;
end;


end.
