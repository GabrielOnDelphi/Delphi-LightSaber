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
   Winapi.Windows, System.SysUtils, Vcl.Dialogs, Vcl.Graphics,
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

   Returns: A NEW bitmap that caller must free, or NIL if source is too small.

   https://msdn.microsoft.com/en-us/library/windows/desktop/dd162950(v=vs.85).aspx
-------------------------------------------------------------------------------------------------------------}
function StretchF(BMP: TBitmap; OutWidth, OutHeight: Integer): TBitmap;
begin
 Assert(BMP <> NIL, 'StretchF: BMP parameter cannot be nil');
 Assert(OutWidth > 0, 'StretchF: OutWidth must be > 0');
 Assert(OutHeight > 0, 'StretchF: OutHeight must be > 0');

 { Windows StretchBlt may produce artifacts or crash with very small images }
 if (BMP.Width < 12) OR (BMP.Height < 12) then
  begin
   ShowMessage('Cannot stretch images under 12 pixels!');
   EXIT(NIL);
  end;

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
  Note: StretchF returns NIL for very small images (< 12 pixels). In this case,
  the original bitmap is left unchanged. }
procedure Stretch(BMP: TBitmap; OutWidth, OutHeight: Integer);
VAR Temp: TBitmap;
begin
  Assert(BMP <> NIL, 'Stretch: BMP parameter cannot be nil');

  Temp:= StretchF(BMP, OutWidth, OutHeight);
  if Temp = NIL
  then EXIT;  { StretchF returns NIL for images < 12 pixels - leave BMP unchanged }

  TRY
    BMP.Assign(Temp);
  FINALLY
    FreeAndNil(Temp);
  END;
end;


end.
