UNIT LightVcl.Graph.ResizeFMX;

{=============================================================================================================
   Gabriel Moraru
   2026.01.30
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Image resizers based on Embarcadero's code (FMX)

   Performance: Terribly slow compared to Windows StretchBlt.
   Compatibility: Does not work with PNG images (use WIC-based loaders instead).
   Pixel format: Converts images to pf32bit during processing.

   Prefer LightVcl.Graph.ResizeWinBlt.Stretch for VCL applications.
--------------------------------------------------------------------------------------------------------------

 TESTER:
      c:\Projects\LightSaber ImageResampler Test\ResamplerTester.dpr

-------------------------------------------------------------------------------------------------------------}
INTERFACE

USES
   System.Classes, System.SysUtils,
   VCL.GraphUtil, Vcl.Graphics, FMX.Graphics, FMX.Surfaces;

{ Resizes bitmap in-place using FMX CreateThumbnail.
  BMP will be converted to pf32bit. }
procedure ResizeFMX(BMP: Vcl.Graphics.TBitmap; NewWidth, NewHeight: Integer);

{ Creates a NEW resized copy of InputBMP. Caller must free the result.
  InputBMP is not modified. Returns pf32bit bitmap. }
function  ResizeFmxF(InputBMP: Vcl.Graphics.TBitmap; NewWidth, NewHeight: Integer): Vcl.Graphics.TBitmap;

IMPLEMENTATION

{ Resizes a VCL bitmap in-place using FMX's CreateThumbnail method.
  The bitmap is converted to pf32bit during processing.
  Note: CreateThumbnail returns a new object, so we must free the original. }
procedure ResizeFMX(BMP: Vcl.Graphics.TBitmap; NewWidth, NewHeight: Integer);
VAR
   fBMP, fResized: FMX.Graphics.TBitmap;
   MS: TMemoryStream;
   FMXBitmapData: TBitmapData;
   Y: Integer;
begin
  Assert(BMP <> NIL, 'ResizeFMX: BMP parameter cannot be nil');
  Assert(NewWidth > 0, 'ResizeFMX: NewWidth must be > 0');
  Assert(NewHeight > 0, 'ResizeFMX: NewHeight must be > 0');

  fBMP:= NIL;
  fResized:= NIL;
  MS:= TMemoryStream.Create;
  TRY
    fBMP:= FMX.Graphics.TBitmap.Create;

    { Convert VCL bitmap to FMX bitmap via stream }
    BMP.PixelFormat:= pf32bit;
    BMP.SaveToStream(MS);
    MS.Position:= 0;
    fBMP.LoadFromStream(MS);

    { CreateThumbnail returns a NEW bitmap - original fBMP must be freed separately }
    fResized:= fBMP.CreateThumbnail(NewWidth, NewHeight);

    { Ensure VCL Bitmap has the correct size and format }
    BMP.PixelFormat:= pf32bit;
    BMP.SetSize(fResized.Width, fResized.Height);

    { Copy FMX bitmap data back to VCL bitmap }
    if NOT fResized.Map(TMapAccess.Read, FMXBitmapData)
    then RAISE Exception.Create('ResizeFMX: Failed to map FMX bitmap data');

    TRY
      for Y:= 0 to BMP.Height - 1 do
        Move(FMXBitmapData.GetScanline(Y)^, BMP.ScanLine[Y]^, BMP.Width * 4);
    FINALLY
      fResized.Unmap(FMXBitmapData);
    END;
  FINALLY
    FreeAndNil(fResized);
    FreeAndNil(fBMP);
    FreeAndNil(MS);
  END;
end;

{ Creates a NEW resized copy of InputBMP without modifying the original.
  Returns a pf32bit bitmap. Caller is responsible for freeing the result. }
function ResizeFmxF(InputBMP: Vcl.Graphics.TBitmap; NewWidth, NewHeight: Integer): Vcl.Graphics.TBitmap;
VAR
   TempBMP: Vcl.Graphics.TBitmap;
   fBMP, fResized: FMX.Graphics.TBitmap;
   MS: TMemoryStream;
   FMXBitmapData: TBitmapData;
   Y: Integer;
begin
  Assert(InputBMP <> NIL, 'ResizeFmxF: InputBMP parameter cannot be nil');
  Assert(NewWidth > 0, 'ResizeFmxF: NewWidth must be > 0');
  Assert(NewHeight > 0, 'ResizeFmxF: NewHeight must be > 0');

  Result:= NIL;
  TempBMP:= NIL;
  fBMP:= NIL;
  fResized:= NIL;
  MS:= TMemoryStream.Create;
  TRY
    { Create a temporary copy to avoid modifying InputBMP }
    TempBMP:= Vcl.Graphics.TBitmap.Create;
    TempBMP.Assign(InputBMP);
    TempBMP.PixelFormat:= pf32bit;

    fBMP:= FMX.Graphics.TBitmap.Create;

    { Convert VCL bitmap to FMX bitmap via stream }
    TempBMP.SaveToStream(MS);
    MS.Position:= 0;
    fBMP.LoadFromStream(MS);

    { CreateThumbnail returns a NEW bitmap - original fBMP must be freed separately }
    fResized:= fBMP.CreateThumbnail(NewWidth, NewHeight);

    { Create result bitmap with correct size and format }
    Result:= Vcl.Graphics.TBitmap.Create;
    TRY
      Result.PixelFormat:= pf32bit;
      Result.SetSize(fResized.Width, fResized.Height);

      { Copy FMX bitmap data to Result bitmap }
      if NOT fResized.Map(TMapAccess.Read, FMXBitmapData)
      then RAISE Exception.Create('ResizeFmxF: Failed to map FMX bitmap data');

      TRY
        for Y:= 0 to Result.Height - 1 do
          Move(FMXBitmapData.GetScanline(Y)^, Result.ScanLine[Y]^, Result.Width * 4);
      FINALLY
        fResized.Unmap(FMXBitmapData);
      END;
    EXCEPT
      FreeAndNil(Result);
      RAISE;
    END;
  FINALLY
    FreeAndNil(fResized);
    FreeAndNil(fBMP);
    FreeAndNil(TempBMP);
    FreeAndNil(MS);
  END;
end;



end.
