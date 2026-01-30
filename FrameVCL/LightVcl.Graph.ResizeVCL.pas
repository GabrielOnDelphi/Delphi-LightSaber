UNIT LightVcl.Graph.ResizeVCL;

{=============================================================================================================
   Gabriel Moraru
   2026.01.30
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Image resizers based on Embarcadero's code.
   ScaleImage is the BEST at scale down.

   These are thin wrappers around VCL.GraphUtil functions, providing a consistent interface
   and parameter validation.

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
  Wrapper for VCL.GraphUtil.ScaleImage.
  Scales InpBMP by ScaleAmount and stores the result in OutBMP.

  Parameters:
     InpBMP      - Source bitmap (must not be nil, must have positive dimensions)
     OutBMP      - Destination bitmap (must not be nil)
     ScaleAmount - Scale factor: 1.0 = no change, 1.2 = 120% (20% larger), 0.5 = 50% (half size)
                   Must be > 0.
-------------------------------------------------------------------------------------------------------------}
procedure ScaleImage(CONST InpBMP, OutBMP: TBitmap; const ScaleAmount: Double);
begin
  Assert(InpBMP <> NIL, 'ScaleImage: InpBMP parameter cannot be nil');
  Assert(OutBMP <> NIL, 'ScaleImage: OutBMP parameter cannot be nil');
  Assert(InpBMP.Width > 0, 'ScaleImage: InpBMP.Width must be > 0');
  Assert(InpBMP.Height > 0, 'ScaleImage: InpBMP.Height must be > 0');
  Assert(ScaleAmount > 0, 'ScaleImage: ScaleAmount must be > 0');

  VCL.GraphUtil.ScaleImage(InpBMP, OutBMP, ScaleAmount);
end;



{-------------------------------------------------------------------------------------------------------------
  Based on Canvas.StretchDraw.

  The operation is WAY faster than JanFX but the image quality is not as great.
     Stretch down: The results are quite good
     Stretch up: The results are pixelated but NOT horrible!
-------------------------------------------------------------------------------------------------------------}

{ Not proportional - stretches InpBMP to fill OutBMP's current dimensions }
procedure CanvasStretch (CONST InpBMP, OutBMP: TBitmap);
begin
  Assert(InpBMP <> NIL, 'CanvasStretch: InpBMP parameter cannot be nil');
  Assert(OutBMP <> NIL, 'CanvasStretch: OutBMP parameter cannot be nil');
  Assert(InpBMP.Width > 0, 'CanvasStretch: InpBMP.Width must be > 0');
  Assert(InpBMP.Height > 0, 'CanvasStretch: InpBMP.Height must be > 0');
  Assert(OutBMP.Width > 0, 'CanvasStretch: OutBMP.Width must be > 0');
  Assert(OutBMP.Height > 0, 'CanvasStretch: OutBMP.Height must be > 0');

  OutBMP.Canvas.StretchDraw(Rect(0, 0, OutBMP.Width, OutBMP.Height), InpBMP);
end;


{ Not proportional - resizes BMP in-place to the specified dimensions }
procedure CanvasStretch (CONST BMP: TBitmap; CONST OutWidth, OutHeight: Integer);
begin
  Assert(BMP <> NIL, 'CanvasStretch: BMP parameter cannot be nil');
  Assert(BMP.Width > 0, 'CanvasStretch: BMP.Width must be > 0');
  Assert(BMP.Height > 0, 'CanvasStretch: BMP.Height must be > 0');
  Assert(OutWidth > 0, 'CanvasStretch: OutWidth must be > 0');
  Assert(OutHeight > 0, 'CanvasStretch: OutHeight must be > 0');

  VAR TempBMP:= TBitmap.Create;
  TRY
    TempBMP.SetSize(OutWidth, OutHeight);
    TempBMP.Canvas.StretchDraw(Rect(0, 0, TempBMP.Width, TempBMP.Height), BMP);
    BMP.Assign(TempBMP);
  FINALLY
    FreeAndNil(TempBMP);
  END;
end;


{ Proportional - resizes BMP to OutWidth, computing height to maintain aspect ratio }
procedure CanvasStretch (CONST BMP: TBitmap; CONST OutWidth: Integer);
VAR
   Ratio: Double;
   OutHeight: Integer;
begin
  Assert(BMP <> NIL, 'CanvasStretch: BMP parameter cannot be nil');
  Assert(BMP.Width > 0, 'CanvasStretch: BMP.Width must be > 0');
  Assert(BMP.Height > 0, 'CanvasStretch: BMP.Height must be > 0');
  Assert(OutWidth > 0, 'CanvasStretch: OutWidth must be > 0');

  Ratio:= BMP.Width / BMP.Height;
  OutHeight:= Round(OutWidth / Ratio);

  CanvasStretch(BMP, OutWidth, OutHeight);
end;



end.
