UNIT LightVcl.Graph.ResizeWinWIC;

{=============================================================================================================
   Gabriel Moraru
   2024.12
   www.GabrielMoraru.com
   See Copyright file
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
   System.Classes, System.SysUtils, VCL.GraphUtil, Vcl.Graphics;

procedure ResizeBitmapWic(const Bitmap: TBitmap; const NewWidth, NewHeight: Integer); // pf32


IMPLEMENTATION

procedure ResizeBitmapWic(const Bitmap: TBitmap; const NewWidth, NewHeight: Integer); // pf32
var
  vImage, v2: TWICImage;
begin
  v2 := NIL;
  vImage := TWICImage.Create;
  try
    // Ensure the input is pf32bit to preserve transparency for PNGs
    if Bitmap.PixelFormat <> pf32bit
    then Bitmap.PixelFormat := pf32bit;

    vImage.Assign(Bitmap);
    v2 := vImage.CreateScaledCopy(NewWidth, NewHeight, wipmHighQualityCubic);
    Bitmap.Assign(v2);
  finally
    v2.Free;
    vImage.Free;
  end;
end;


end.
