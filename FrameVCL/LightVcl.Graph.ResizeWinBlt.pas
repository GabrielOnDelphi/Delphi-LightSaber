UNIT LightVcl.Graph.ResizeWinBlt;

{=============================================================================================================
   Gabriel Moraru
   2026.01.30
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------

   Image resizer using Windows StretchBlt API

   TESTER:
      c:\Projects\LightSaber ImageResampler Test\ResamplerTester.dpr

-------------------------------------------------------------------------------------------------------------}
INTERFACE

USES
   Winapi.Windows, System.SysUtils, Vcl.Graphics,
   LightVcl.Graph.Bitmap;

 { Not proportional }
 function  StretchF         (BMP: TBitmap; OutWidth, OutHeight: Integer): TBitmap;                            { Best of all algorithms. 2019.08 }
 procedure Stretch          (BMP: TBitmap; OutWidth, OutHeight: Integer);


IMPLEMENTATION




{-------------------------------------------------------------------------------------------------------------
   Based on MS Windows StretchBlt
   BEST quality resampler (see tester)

   Zoom: In/Out
   Keep aspect ratio: No
   Stretch provided in: pixels

   Resize down: VERY smooth. Better than JanFX.SmoothResize.
   Resize up: better (sharper) than JanFX.SmoothResize
   Time: similar to JanFx

   Note: BitBlt only does copy, NO STRETCH.

   Returns: A NEW bitmap that caller must free.
   Asserts if source is too small (< 12 pixels) — caller must validate before calling.

   https://msdn.microsoft.com/en-us/library/windows/desktop/dd162950(v=vs.85).aspx
-------------------------------------------------------------------------------------------------------------}
function StretchF(BMP: TBitmap; OutWidth, OutHeight: Integer): TBitmap;
begin
 Assert(BMP <> NIL, 'StretchF: BMP parameter cannot be nil');
 Assert(OutWidth > 0, 'StretchF: OutWidth must be > 0');
 Assert(OutHeight > 0, 'StretchF: OutHeight must be > 0');

 { Windows StretchBlt may produce artifacts or crash with very small images.
   Callers must validate input size before calling. This assert catches programming errors. }
 Assert((BMP.Width >= 12) AND (BMP.Height >= 12), 'StretchF: Source image too small ('+ IntToStr(BMP.Width)+ 'x'+ IntToStr(BMP.Height)+ '). Caller must validate size >= 12 pixels.');

 Result:= TBitmap.Create;
 TRY
  Result.PixelFormat:= BMP.PixelFormat;  { Preserve the same pixel format as the original image }
  SetLargeSize(Result, OutWidth, OutHeight);
  SetStretchBltMode(Result.Canvas.Handle, HALFTONE);
  SetBrushOrgEx(Result.Canvas.Handle, 0, 0, NIL);
  StretchBlt(Result.Canvas.Handle, 0, 0, Result.Width, Result.Height,
             BMP.Canvas.Handle, 0, 0, BMP.Width, BMP.Height, SRCCOPY);
 EXCEPT
  FreeAndNil(Result);
  RAISE;
 END;
end;


{ Uses MS Windows StretchBlt.
  Asserts if source is too small (< 12 pixels) — caller must validate before calling. }
procedure Stretch(BMP: TBitmap; OutWidth, OutHeight: Integer);
VAR Temp: TBitmap;
begin
  Assert(BMP <> NIL, 'Stretch: BMP parameter cannot be nil');

  Temp:= StretchF(BMP, OutWidth, OutHeight);
  TRY
    BMP.Assign(Temp);
  FINALLY
    FreeAndNil(Temp);
  END;
end;


end.
