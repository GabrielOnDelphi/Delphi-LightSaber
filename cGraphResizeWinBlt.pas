UNIT cGraphResizeWinBlt;

{=============================================================================================================
   Gabriel Moraru
   2024.12
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------

   Image resizer using Windows StretchBlt API

   TESTER:
      c:\Projects\LightSaber ImageResampler Test\ResamplerTester.dpr

-------------------------------------------------------------------------------------------------------------}
INTERFACE

USES
   Winapi.Windows, System.SysUtils, Vcl.Dialogs, Vcl.Graphics,
   cGraphBitmap, cGraphResizeParams;

 { Not proportional }
 function  StretchF         (BMP: TBitmap; OutWidth, OutHeight: Integer): TBitmap;                            { Best of all algorithms. 2019.08 }
 procedure Stretch          (BMP: TBitmap; OutWidth, OutHeight: Integer);


IMPLEMENTATION

USES
  cmMath, cGraphLoader, cGraphFX;


{-------------------------------------------------------------------------------------------------------------
   Based on MS Windows StretchBlt
   BEST (see tester)

   Zoom: In/Out
   Keep aspect ration: No
   Stretch provided in: pixels

   Resize down: VERY smooth. Better than JanFX.SmoothResize.
   Resize up: better (sharper) than JanFX.SmoothResize
   Time: similar to JanFx

   BitBlt only does copy. NO STRETCH

   https://msdn.microsoft.com/en-us/library/windows/desktop/dd162950(v=vs.85).aspx
-------------------------------------------------------------------------------------------------------------}
function StretchF(BMP: TBitmap; OutWidth, OutHeight: Integer): TBitmap;
begin
 if (BMP.Width < 12) OR (BMP.Height< 12) then
  begin                                                  { 'WinStretchBltF' will crash if the image size is too small (below 10 pixels)}
   ShowMessage('Cannot stretch images under 12 pixels!');
   EXIT(NIL);
  end;

 Result:= TBitmap.Create;
 TRY
  Result.PixelFormat:= BMP.PixelFormat; { Make sure we use the same pixel format as the original image }
  SetLargeSize(Result, OutWidth, OutHeight);
  SetStretchBltMode(Result.Canvas.Handle, HALFTONE);
  SetBrushOrgEx    (Result.Canvas.Handle, 0,0, NIL);
  StretchBlt(Result.Canvas.Handle, 0, 0, Result.Width, Result.Height, BMP.Canvas.Handle, 0, 0, BMP.Width, BMP.Height, SRCCOPY);
 EXCEPT
  FreeAndNil(Result); { Free the bitmap if something goes wrong }
  RAISE;
 END;
end;


{ Uses MS Windows StretchBlt }
procedure Stretch(BMP: TBitmap; OutWidth, OutHeight: Integer);
VAR Temp: TBitmap;
begin
  Temp:= StretchF(BMP, OutWidth, OutHeight);
  TRY
    BMP.Assign(Temp);
  FINALLY
    FreeAndNil(Temp);
  END;
end;


end.
