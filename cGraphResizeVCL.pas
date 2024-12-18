UNIT cGraphResizeVCL;

{=============================================================================================================
   Gabriel Moraru
   2024.12
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Image resizers based on Embarcadero's code
   ScaleImage is the BEST at scale down.
--------------------------------------------------------------------------------------------------------------

 TESTER:
      c:\Projects\LightSaber ImageResampler Test\ResamplerTester.dpr
-------------------------------------------------------------------------------------------------------------}
INTERFACE

USES
   System.Classes, System.SysUtils, VCL.GraphUtil, Vcl.Graphics;



 procedure ScaleImage    (CONST InpBMP, OutBMP: TBitmap; const ScaleAmount: Double);

 procedure CanvasStretch (CONST InpBMP, OutBMP: TBitmap);                           overload;  { Not proportional }
 procedure CanvasStretch (CONST BMP: TBitmap; CONST OutWidth, OutHeight: Integer);  overload;  { Not proportional }
 procedure CanvasStretch (CONST BMP: TBitmap; CONST OutWidth: Integer);             overload;  { Proportional }



IMPLEMENTATION



{-------------------------------------------------------------------------------------------------------------
  Based on ScaleImage

  Parameters:
     ScaleAmount = use 1.2 to increase size with 20%
-------------------------------------------------------------------------------------------------------------}
procedure ScaleImage(CONST InpBMP, OutBMP: TBitmap; const ScaleAmount: Double);
begin
  VCL.GraphUtil.ScaleImage(InpBMP, OutBMP, ScaleAmount);
end;



{-------------------------------------------------------------------------------------------------------------
  Based on Canvas.StretchDraw

  The operation is WAY MUCH faster than JanFX but the image quality is not as great.
     Stretch down: The results are quite good
     Stretch up: The results are pixelated but NOT horrible!
-------------------------------------------------------------------------------------------------------------}

{ Not proportional }
procedure CanvasStretch (CONST InpBMP, OutBMP: TBitmap);
begin
 OutBMP.Canvas.StretchDraw(Rect(0, 0, OutBMP.Width, OutBMP.Height), InpBMP);
end;


{ Not proportional }
procedure CanvasStretch (CONST BMP: TBitmap; CONST OutWidth, OutHeight: Integer);
begin
 Assert((BMP.Width> 0) AND (BMP.Height> 0), 'Invalid image size in CanvasStretch');

 VAR TempBMP:= TBitmap.Create;
 TRY
  TempBMP.SetSize(OutWidth, OutHeight);
  TempBMP.Canvas.StretchDraw(Rect(0, 0, TempBMP.Width, TempBMP.Height), BMP);
  BMP.Assign(TempBMP);
 FINALLY
  FreeAndNil(TempBMP);
 END;
end;


{ Proportional }
procedure CanvasStretch (CONST BMP: TBitmap; CONST OutWidth: Integer);
VAR
   Ratio: double;
   OutHeight: Integer;
begin
 Ratio:= BMP.Width / BMP.Height;
 OutHeight:= round(OutWidth / Ratio);

 CanvasStretch(BMP, OutWidth, OutHeight);
end;



end.
