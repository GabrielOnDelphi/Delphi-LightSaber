UNIT LightVcl.Graph.ResizeFMX;

{=============================================================================================================
   Gabriel Moraru
   2024.12
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
   Image resizers based on Embarcadero's code (FMX)
   Terribly slow.
   Does not work with PNG images.
--------------------------------------------------------------------------------------------------------------

 TESTER:
      c:\Projects\LightSaber ImageResampler Test\ResamplerTester.dpr

-------------------------------------------------------------------------------------------------------------}
INTERFACE

USES
   System.Classes, System.SysUtils,
   VCL.GraphUtil, Vcl.Graphics, FMX.Graphics, FMX.Surfaces; //, Vcl.Imaging.pngimage;


procedure ResizeFMX (BMP: Vcl.Graphics.TBitmap; NewWidth, NewHeight: Integer);
function  ResizeFmxF(InputBMP: Vcl.Graphics.TBitmap; NewWidth, NewHeight: Integer): Vcl.Graphics.TBitmap;

IMPLEMENTATION

procedure ResizeFMX(BMP: Vcl.Graphics.TBitmap; NewWidth, NewHeight: Integer);
VAR
   fBMP: FMX.Graphics.TBitmap;
   MS: TMemoryStream;
   FMXBitmapData: TBitmapData;
   Y: Integer;
begin
  fBMP := FMX.Graphics.TBitmap.Create;
  MS := TMemoryStream.Create;
  TRY
    // Assign VCL bitmap to FMX bitmap
    BMP.PixelFormat := pf32bit;
    BMP.SaveToStream(MS);
    //del BMP.PixelFormat := pf24bit;
    MS.Position := 0;
    fBMP.LoadFromStream(MS);

    // Resize FMX bitmap using CreateThumbnail
    fBMP := fBMP.CreateThumbnail(NewWidth, NewHeight);

    // Ensure VCL Bitmap (BMP) has the same size and format
    BMP.PixelFormat := pf32bit;
    BMP.SetSize(fBMP.Width, fBMP.Height);

    // Output FMX bitmap data back to the VCL bitmap
    if NOT fBMP.Map(TMapAccess.Read, FMXBitmapData) then
      RAISE Exception.Create('Failed to map FMX bitmap data');
    TRY
      for Y := 0 to BMP.Height - 1 do
        Move(FMXBitmapData.GetScanline(Y)^, BMP.ScanLine[Y]^, BMP.Width * 4); // Copy each row
    FINALLY
      fBMP.Unmap(FMXBitmapData);
    END;
  FINALLY
    FreeAndNil(fBMP);
    FreeAndNil(MS);
  END;
end;

function ResizeFmxF(InputBMP: Vcl.Graphics.TBitmap; NewWidth, NewHeight: Integer): Vcl.Graphics.TBitmap;
VAR
   fBMP: FMX.Graphics.TBitmap;
   MS: TMemoryStream;
   FMXBitmapData: TBitmapData;
   Y: Integer;
begin
  Result := Vcl.Graphics.TBitmap.Create;
  fBMP := FMX.Graphics.TBitmap.Create;
  MS := TMemoryStream.Create;
  TRY
    // Assign VCL bitmap to FMX bitmap
    InputBMP.PixelFormat := pf32bit;
    InputBMP.SaveToStream(MS);
    InputBMP.PixelFormat := pf24bit;
    MS.Position := 0;
    fBMP.LoadFromStream(MS);
    // Resize FMX bitmap using CreateThumbnail
    fBMP := fBMP.CreateThumbnail(NewWidth, NewHeight);
    // Ensure Result bitmap has the same size and format
    Result.PixelFormat := pf32bit;
    Result.SetSize(fBMP.Width, fBMP.Height);
    // Copy FMX bitmap data to Result bitmap
    if NOT fBMP.Map(TMapAccess.Read, FMXBitmapData) then
      RAISE Exception.Create('Failed to map FMX bitmap data');
    TRY
      for Y := 0 to Result.Height - 1 do
        Move(FMXBitmapData.GetScanline(Y)^, Result.ScanLine[Y]^, Result.Width * 4); // Copy each row
    FINALLY
      fBMP.Unmap(FMXBitmapData);
    END;
  FINALLY
    FreeAndNil(fBMP);
    FreeAndNil(MS);
  END;
end;



end.
