UNIT LightVcl.Graph.Loader.Resolution;

{=============================================================================================================
   Gabriel Moraru
   2026.09.10
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
  The VCL-bound remainder of the image-resolution reader.

  Everything that only parses a file header - GetImageRes, GetJpgSize, GetPNGSize, GetGIFSize, GetBmpSize,
  GetBmpHeader and the TBitmapHeader record - now lives in LightCore.Graph.Loader.Resolution.pas, so it can
  be used from a console tool, an Android build, or a DUnitX test that does not link the VCL.

  Only GetBitsPerPixel stays here: its parameter is Vcl.Graphics.TBitmap.
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
   Vcl.Graphics;

 function  GetBitsPerPixel(BMP:TBitmap): Integer;



 IMPLEMENTATION

 USES LightVcl.Graph.Util;



function GetBitsPerPixel(BMP: TBitmap): Integer;
begin
 Assert(Assigned(BMP), 'GetBitsPerPixel: BMP is nil');
 case BMP.PixelFormat of
   pf1Bit  : result:= 1;
   pf4Bit  : result:= 4;
   pf8Bit  : result:= 8;
   pf15Bit : result:= 15;
   pf16Bit : result:= 16;
   pf24Bit : result:= 24;
   pf32Bit : result:= 32;
   pfDevice: result:= LightVcl.Graph.Util.GetDeviceColorDepth;
  else
    Result:= -1;
  end;
end;


end.
