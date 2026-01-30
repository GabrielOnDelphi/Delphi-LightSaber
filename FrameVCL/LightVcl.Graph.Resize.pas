UNIT LightVcl.Graph.Resize;

{=============================================================================================================
   Gabriel Moraru
   2026.01.30
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Image resizers
   They all use Windows StretchBlt in LightVcl.Graph.ResizeWin.pas

   Parameters of the resize operation are stored in a LightVcl.Graph.ResizeParams.RResizeParams record.
   The record is filled with data from GUI in LightVcl.Graph.ResizeParamEdt.pas.

   ToDo: I should use cGraphStretch32.GR32.Transform to resize down and JanFX to resize up.
   ToDo: Make a program that compares:  cGraphStretchCanvas, janFXStretch.SmoothResize, janFXStretch.Stretch, GR32.Transform
--------------------------------------------------------------------------------------------------------------

 TESTER:
      c:\Projects\LightSaber ImageResampler Test\ResamplerTester.dpr
      c:\MyProjects\Projects GRAPH Resamplers\Tester for LightVcl.Graph.ResizeParam\

 TEST RESULTS

  Resize down:
     GR32.Transform: 0.23sec.
     JanFX.Stretch : 1.45 sec

  Resize up:
     GR32 is 5.5 times slower (and also too sharp).
     (Both tested with best resamplers)

  See this good article:
     What goes in a Delphi VCL TGraphic, TPicture, TImage, and how to convert between graphic file formats?
     http://www.awaresystems.be/techtalks/003_graphic_picture_image.html

     Delphi Images
     http://mc-computing.com/languages/delphi/images.html

     How to draw fadeout text?
     http://stackoverflow.com/questions/20582226/how-to-draw-fade-out-text-on-a-custom-tgraphiccontrol

     Resize image using GDI+
     http://stackoverflow.com/questions/33608134/fast-way-to-resize-an-image-mixing-fmx-and-vcl-code?noredirect=1#comment54999135_33608134

  Also see this:
     http://www.soft-gems.net/index.php/controls/windows-xp-theme-manager
     http://www.efg2.com/Lab/Library/Delphi/Graphics/Color.htm
     http://www.davdata.nl/math/bmresize.html (A Bitmap Resize Algorithm)
-------------------------------------------------------------------------------------------------------------}
INTERFACE

USES
   Winapi.Windows, System.SysUtils, Vcl.Dialogs, Vcl.Graphics,
   LightVcl.Graph.Bitmap, LightVcl.Graph.ResizeParams, LightVcl.Graph.ResizeWinBlt;

 { Proportional }
 procedure SmartStretch     (BMP: TBitmap; ResizeOpp: RResizeParams);              overload;
 procedure SmartStretch     (BMP: TBitmap; CONST MaxWidth, MaxHeight: Integer);    overload;
 procedure SmartStretch     (BMP: TBitmap; CONST MaxWidth, MaxHeight, FitTolerance:Integer);  overload;
 procedure SmartStretch     (BMP: TBitmap; CONST MaxWidth, MaxHeight: Integer; ResizeOp: TResizeOp); overload;
 procedure SmartStretchCrop (BMP: TBitmap; CONST MaxWidth, MaxHeight: Integer);                               { Resizes the image to fill the viewport. Then it crops whatever went out of the viewport }

 procedure StretchPercent   (BMP: TBitmap; CONST ResizePercent: Integer);          overload;
 procedure StretchPercentX  (BMP: TBitmap; CONST ResizeTimes: Single);             overload;

 procedure StretchProport   (BMP: TBitmap; CONST OutWidth: Integer);               overload;                  { Proportional. Height will be auto adjusted to match width }
 procedure StretchProport   (BMP: TBitmap; CONST MaxWidth, MaxHeight: Integer);    overload;
 function  StretchProportF  (BMP: TBitmap; CONST MaxWidth, MaxHeight: Integer): TBitmap;    overload;

 function  LoadAndStretch   (CONST FileName: string; ResizeOpp: RResizeParams; UseWic: Boolean= TRUE): TBitmap;           overload; { Loads image and also stretch the output image to the required dimensions }
 function  LoadAndStretch   (CONST FileName: string; CONST MaxWidth, MaxHeight: Integer; UseWic: Boolean= TRUE): TBitmap; overload;


IMPLEMENTATION

USES
  LightCore.Math, LightVcl.Graph.Loader, LightVcl.Graph.FX;



{--------------------------------------------------------------------------------------------------
   MAIN FUNCTION
   SmartStretch: Resizes a bitmap using the parameters specified in RResizeParams.
   The RResizeParams record handles various resize modes (Fit, Fill, ForceWidth, etc.)
   and computes the optimal output dimensions while preserving aspect ratio when appropriate.
--------------------------------------------------------------------------------------------------}
procedure SmartStretch(BMP: TBitmap; ResizeOpp: RResizeParams);
begin
  Assert(BMP <> NIL, 'SmartStretch: BMP parameter cannot be nil');

  ResizeOpp.ComputeOutputSize(BMP.Width, BMP.Height);
  Stretch(BMP, ResizeOpp.OutW, ResizeOpp.OutH);
end;


{ Resizes bitmap proportionally to fit within MaxWidth x MaxHeight boundaries.
  Uses default resize settings (roAutoDetect mode). }
procedure SmartStretch(BMP: TBitmap; CONST MaxWidth, MaxHeight: Integer);
VAR ResizeOpp: RResizeParams;
begin
  Assert(BMP <> NIL, 'SmartStretch: BMP parameter cannot be nil');
  Assert(MaxWidth > 0, 'SmartStretch: MaxWidth must be > 0');
  Assert(MaxHeight > 0, 'SmartStretch: MaxHeight must be > 0');

  ResizeOpp.Reset;
  ResizeOpp.MaxWidth := MaxWidth;
  ResizeOpp.MaxHeight:= MaxHeight;

  ResizeOpp.ComputeOutputSize(BMP.Width, BMP.Height);
  Stretch(BMP, ResizeOpp.OutW, ResizeOpp.OutH);
end;


{ Resizes bitmap using the specified resize operation mode.
  ResizeOp determines how the image fits within MaxWidth x MaxHeight:
    roFit  - Adds black bars if needed, never crops
    roFill - Fills the area completely, may crop edges
    etc. (see TResizeOp for all modes) }
procedure SmartStretch(BMP: TBitmap; CONST MaxWidth, MaxHeight: Integer; ResizeOp: TResizeOp);
VAR ResizeOpp: RResizeParams;
begin
  Assert(BMP <> NIL, 'SmartStretch: BMP parameter cannot be nil');
  Assert(MaxWidth > 0, 'SmartStretch: MaxWidth must be > 0');
  Assert(MaxHeight > 0, 'SmartStretch: MaxHeight must be > 0');

  ResizeOpp.Reset;
  ResizeOpp.MaxWidth  := MaxWidth;
  ResizeOpp.MaxHeight := MaxHeight;
  ResizeOpp.ResizeOpp := ResizeOp;

  ResizeOpp.ComputeOutputSize(BMP.Width, BMP.Height);
  Stretch(BMP, ResizeOpp.OutW, ResizeOpp.OutH);
end;


{ Resizes the image to completely fill the viewport (no black bars), then crops
  any portions that extend beyond MaxWidth x MaxHeight. The cropping is centered,
  removing equal amounts from opposite edges. }
procedure SmartStretchCrop(BMP: TBitmap; CONST MaxWidth, MaxHeight: Integer);
VAR ResizeOpp: RResizeParams;
begin
  Assert(BMP <> NIL, 'SmartStretchCrop: BMP parameter cannot be nil');
  Assert(MaxWidth > 0, 'SmartStretchCrop: MaxWidth must be > 0');
  Assert(MaxHeight > 0, 'SmartStretchCrop: MaxHeight must be > 0');

  ResizeOpp.Reset;
  ResizeOpp.MaxWidth  := MaxWidth;
  ResizeOpp.MaxHeight := MaxHeight;
  ResizeOpp.ResizeOpp := roFill;

  ResizeOpp.ComputeOutputSize(BMP.Width, BMP.Height);
  Stretch(BMP, ResizeOpp.OutW, ResizeOpp.OutH);
  CropBitmap(BMP, MaxWidth, MaxHeight);
end;



{ Resizes bitmap with custom FitTolerance percentage.
  FitTolerance: When falling back from Fill to Fit mode, this percentage determines
  how much "over-fitting" is allowed to reduce black bars (typically 10%). }
procedure SmartStretch(BMP: TBitmap; CONST MaxWidth, MaxHeight, FitTolerance: Integer);
VAR ResizeOpp: RResizeParams;
begin
  Assert(BMP <> NIL, 'SmartStretch: BMP parameter cannot be nil');
  Assert(MaxWidth > 0, 'SmartStretch: MaxWidth must be > 0');
  Assert(MaxHeight > 0, 'SmartStretch: MaxHeight must be > 0');
  Assert((FitTolerance >= 0) AND (FitTolerance <= 100), 'SmartStretch: FitTolerance must be 0-100');

  ResizeOpp.Reset;
  ResizeOpp.MaxWidth := MaxWidth;
  ResizeOpp.MaxHeight:= MaxHeight;
  ResizeOpp.FitTolerance:= FitTolerance;

  ResizeOpp.ComputeOutputSize(BMP.Width, BMP.Height);
  Stretch(BMP, ResizeOpp.OutW, ResizeOpp.OutH);
end;


{ Loads an image from disk and resizes it according to ResizeOpp parameters.
  Returns the loaded and resized bitmap. Caller is responsible for freeing.
  Returns NIL if the image could not be loaded (corrupted file, unsupported format).
  UseWic: TRUE for faster WIC-based loading (recommended), FALSE for VCL loaders. }
function LoadAndStretch(CONST FileName: string; ResizeOpp: RResizeParams; UseWic: Boolean= TRUE): TBitmap;
begin
 Result:= LightVcl.Graph.Loader.LoadGraph(FileName, UseWic);
 if Result = NIL
 then EXIT;  { LoadGraph returns NIL for corrupted images instead of crashing }

 TRY
   SmartStretch(Result, ResizeOpp);
 EXCEPT
   FreeAndNil(Result);
   RAISE;
 END;
end;


{ Loads an image from disk and resizes it to fit within MaxWidth x MaxHeight.
  Returns the loaded and resized bitmap. Caller is responsible for freeing.
  Returns NIL if the image could not be loaded. }
function LoadAndStretch(CONST FileName: string; CONST MaxWidth, MaxHeight: Integer; UseWic: Boolean= TRUE): TBitmap;
begin
 Result:= LightVcl.Graph.Loader.LoadGraph(FileName, UseWic);
 if Result = NIL
 then EXIT;  { LoadGraph returns NIL for corrupted images instead of crashing }

 TRY
   SmartStretch(Result, MaxWidth, MaxHeight);
 EXCEPT
   FreeAndNil(Result);
   RAISE;
 END;
end;




{--------------------------------------------------------------------------------------------------
 PURE FIT (Proportional resize)

 Zoom               : In/Out
 Keep aspect ratio  : Yes
 Stretch provided in: pixels

 Resizes image to fit within MaxWidth x MaxHeight while preserving aspect ratio.
 The image will be enlarged/shrunk as much as possible WITHOUT exceeding either limit.
 Result: At least one dimension equals its max, the other is smaller or equal.
--------------------------------------------------------------------------------------------------}
procedure StretchProport(BMP: TBitmap; CONST MaxWidth, MaxHeight: Integer);
VAR
   NewWidth, NewHeight: Integer;
begin
 Assert(BMP <> NIL, 'StretchProport: BMP parameter cannot be nil');
 Assert(MaxWidth > 0, 'StretchProport: MaxWidth must be > 0');
 Assert(MaxHeight > 0, 'StretchProport: MaxHeight must be > 0');
 Assert(BMP.Width > 0, 'StretchProport: BMP.Width must be > 0');
 Assert(BMP.Height > 0, 'StretchProport: BMP.Height must be > 0');

 { Compare aspect ratios using cross-multiplication to avoid division by zero }
 if Int64(MaxWidth) * BMP.Height = Int64(MaxHeight) * BMP.Width
 then
   { Same ratio - scale to exact target size }
   begin
    NewWidth := MaxWidth;
    NewHeight:= MaxHeight;
   end
 else
   if AspectIsSmaller(BMP, MaxWidth, MaxHeight)
   then
    { Image is narrower (portrait-ish) relative to target - constrain by height }
    begin
     NewWidth := MulDiv(MaxHeight, BMP.Width, BMP.Height);
     NewHeight:= MaxHeight;
    end
   else
    { Image is wider (landscape-ish) relative to target - constrain by width }
    begin
     NewHeight:= MulDiv(MaxWidth, BMP.Height, BMP.Width);
     NewWidth := MaxWidth;
    end;

 Stretch(BMP, NewWidth, NewHeight);
end;


{ Proportional resize to specified width. Height is automatically computed
  to maintain aspect ratio. }
procedure StretchProport(BMP: TBitmap; CONST OutWidth: Integer);
VAR OutHeight: Integer;
begin
 Assert(BMP <> NIL, 'StretchProport: BMP parameter cannot be nil');
 Assert(OutWidth > 0, 'StretchProport: OutWidth must be > 0');
 Assert(BMP.Width > 0, 'StretchProport: BMP.Width must be > 0');
 Assert(BMP.Height > 0, 'StretchProport: BMP.Height must be > 0');

 { Use MulDiv to avoid floating-point and preserve precision }
 OutHeight:= MulDiv(OutWidth, BMP.Height, BMP.Width);
 Stretch(BMP, OutWidth, OutHeight);
end;


{ Creates a NEW proportionally resized copy of the input bitmap.
  Caller is responsible for freeing the returned bitmap. }
function StretchProportF(BMP: TBitmap; CONST MaxWidth, MaxHeight: Integer): TBitmap;
begin
 Assert(BMP <> NIL, 'StretchProportF: BMP parameter cannot be nil');

 Result:= TBitmap.Create;
 TRY
   Result.Assign(BMP);
   StretchProport(Result, MaxWidth, MaxHeight);
 EXCEPT
   FreeAndNil(Result);
   RAISE;
 END;
end;





{--------------------------------------------------------------------------------------------------
 Resize by delta percentage (positive = enlarge, negative = shrink)

 Zoom               : In/out
 Keep aspect ratio  : Yes
 Stretch provided in: % delta

 Examples:
   ResizePercent = +50  -> Image becomes 150% of original (50% larger)
   ResizePercent =   0  -> No change
   ResizePercent = -25  -> Image becomes 75% of original (25% smaller)
   ResizePercent = -99  -> Image becomes 1% of original
   ResizePercent <=-100 -> Invalid (would result in zero or negative size)
--------------------------------------------------------------------------------------------------}
procedure StretchPercent(BMP: TBitmap; CONST ResizePercent: Integer);
VAR
   NewWidth, NewHeight: Integer;
begin
 Assert(BMP <> NIL, 'StretchPercent: BMP parameter cannot be nil');

 if ResizePercent <= -100
 then RAISE Exception.Create('StretchPercent: ResizePercent must be > -100 (cannot reduce to zero or negative size)');

 if ResizePercent = 0
 then EXIT;  { No resize needed }

 { Recalculate width/height: NewSize = OrigSize + (OrigSize * Percent / 100) }
 NewWidth := BMP.Width  + Round(ProcentNormal(ResizePercent, BMP.Width));
 NewHeight:= BMP.Height + Round(ProcentNormal(ResizePercent, BMP.Height));

 Stretch(BMP, NewWidth, NewHeight);
end;


{--------------------------------------------------------------------------------------------------
 Resize by multiplication factor

 Zoom               : In/out
 Keep aspect ratio  : Yes
 Stretch provided in: multiplier

 Examples:
   ResizeTimes = 2.0  -> Image becomes 200% of original (2x larger)
   ResizeTimes = 1.0  -> No change (1x = original)
   ResizeTimes = 0.5  -> Image becomes 50% of original (half size)
   ResizeTimes = 1.5  -> Image becomes 150% of original
   ResizeTimes <= 0   -> Invalid
--------------------------------------------------------------------------------------------------}
procedure StretchPercentX(BMP: TBitmap; CONST ResizeTimes: Single);
begin
 Assert(BMP <> NIL, 'StretchPercentX: BMP parameter cannot be nil');

 if ResizeTimes <= 0
 then RAISE Exception.Create('StretchPercentX: ResizeTimes must be > 0');

 if SameValue(ResizeTimes, 1.0)
 then EXIT;  { 1x means no resize }

 { Convert multiplier to delta percentage: 2x -> +100%, 0.5x -> -50% }
 VAR DeltaPercent:= RoundEx((ResizeTimes - 1.0) * 100);
 StretchPercent(BMP, DeltaPercent);
end;


end.
