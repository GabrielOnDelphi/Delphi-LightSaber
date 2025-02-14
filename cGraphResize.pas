UNIT cGraphResize;

{=============================================================================================================
   Gabriel Moraru
   2024.12
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
   Image resizers
   They all use Windows StretchBlt in cGraphResizeWin.pas

   Parameters of the resize operation are stored in a cGraphResizeParams.RResizeParams record.
   The record is filled with data from GUI in cGraphResizeParamEdt.pas.

   ToDo: I should use cGraphStretch32.GR32.Transform to resize down and JanFX to resize up.
   ToDo: Make a program that compares:  cGraphStretchCanvas, janFXStretch.SmoothResize, janFXStretch.Stretch, GR32.Transform
--------------------------------------------------------------------------------------------------------------

 TESTER:
      c:\Projects\LightSaber ImageResampler Test\ResamplerTester.dpr
      c:\MyProjects\Projects GRAPH Resamplers\Tester for cGraphResizeParam\

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
   cGraphBitmap, cGraphResizeParams, cGraphResizeWinBlt;

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
  cmMath, cGraphLoader, cGraphFX;



{--------------------------------------------------------------------------------------------------
   MAIN FUNCTION
--------------------------------------------------------------------------------------------------}
procedure SmartStretch(BMP: TBitmap; ResizeOpp: RResizeParams);
begin
  ResizeOpp.ComputeOutputSize(BMP.Width, BMP.Height);
  Stretch(BMP, ResizeOpp.OutW, ResizeOpp.OutH);
end;


procedure SmartStretch(BMP: TBitmap; CONST MaxWidth, MaxHeight: Integer);
VAR ResizeOpp: RResizeParams;
begin
  ResizeOpp.Reset;
  ResizeOpp.MaxWidth := MaxWidth;
  ResizeOpp.MaxHeight:= MaxHeight;

  ResizeOpp.ComputeOutputSize(BMP.Width, BMP.Height);
  Stretch(BMP, ResizeOpp.OutW, ResizeOpp.OutH);
end;


procedure SmartStretch (BMP: TBitmap; CONST MaxWidth, MaxHeight: Integer; ResizeOp: TResizeOp);
VAR ResizeOpp: RResizeParams;
begin
  ResizeOpp.Reset;
  ResizeOpp.MaxWidth  := MaxWidth;
  ResizeOpp.MaxHeight := MaxHeight;
  ResizeOpp.ResizeOpp := ResizeOp;

  ResizeOpp.ComputeOutputSize(BMP.Width, BMP.Height);
  Stretch(BMP, ResizeOpp.OutW, ResizeOpp.OutH);
end;


{ Resizes the image to fill the viewport. Then it crops whatever went out of the viewport }
procedure SmartStretchCrop(BMP: TBitmap; CONST MaxWidth, MaxHeight: Integer);
VAR ResizeOpp: RResizeParams;
begin
  ResizeOpp.Reset;
  ResizeOpp.MaxWidth  := MaxWidth;
  ResizeOpp.MaxHeight := MaxHeight;
  ResizeOpp.ResizeOpp := roFill;

  ResizeOpp.ComputeOutputSize(BMP.Width, BMP.Height);
  Stretch(BMP, ResizeOpp.OutW, ResizeOpp.OutH);
  CropBitmap(BMP, MaxWidth, MaxHeight)
end;



procedure SmartStretch(BMP: TBitmap; CONST MaxWidth, MaxHeight, FitTolerance: Integer);
VAR ResizeOpp: RResizeParams;
begin
  ResizeOpp.Reset;
  ResizeOpp.MaxWidth := MaxWidth;
  ResizeOpp.MaxHeight:= MaxHeight;
  ResizeOpp.FitTolerance:= FitTolerance;

  ResizeOpp.ComputeOutputSize(BMP.Width, BMP.Height);
  Stretch(BMP, ResizeOpp.OutW, ResizeOpp.OutH);
end;


function LoadAndStretch(CONST FileName: string; ResizeOpp: RResizeParams; UseWic: Boolean= TRUE): TBitmap;         { Loads image and also stretch the output image to the required dimensions }
begin
 Result:= cGraphLoader.LoadGraph(FileName, UseWic);
 TRY
   SmartStretch(Result, ResizeOpp);
 EXCEPT
   FreeAndNil(Result);
   RAISE;
 END;
end;


function LoadAndStretch(CONST FileName: string; CONST MaxWidth, MaxHeight: Integer; UseWic: Boolean= TRUE): TBitmap;         { Loads image and also stretch the output image to the required dimensions }
begin
 Result:= cGraphLoader.LoadGraph(FileName, UseWic);
 TRY
   SmartStretch(Result, MaxWidth, MaxHeight);
 EXCEPT
   FreeAndNil(Result);
   RAISE;
 END;
end;




{--------------------------------------------------------------------------------------------------
 PURE  FIT

 Zoom               : In/Out
 Keep aspect ration : Yes
 Stretch provided in: pixels

 Enlarge image to fit the MaxWidth/MaxHeight size, without changing its aspect ratio.
 The image will be enlarged as much as possible BUT WITHOUT crossing over the MaxWidth or MaxHeight limit.
--------------------------------------------------------------------------------------------------}
procedure StretchProport(BMP: TBitmap; CONST MaxWidth, MaxHeight: Integer);
VAR
   NewWidth, NewHeight: Integer;
begin
 Assert(MaxWidth > 0);
 if MaxWidth/MaxHeight = BMP.Width/BMP.Height
 then
   { Same ratio }
   begin
    NewWidth := MaxWidth;
    NewHeight:= MaxHeight;                                        { if display and disk image have the same aspect ratio then just resize }
   end
 else
   if AspectIsSmaller(BMP, MaxWidth, MaxHeight)
   then
    { Landscape (relative to desktop) }
    BEGIN
     NewWidth := MulDiv(MaxHeight, BMP.Width, BMP.Height);        { Determine new width of the image }
     NewHeight:= MaxHeight;
    END
   else
    { Portret (relative to desktop) }
    BEGIN
     NewHeight:= MulDiv(MaxWidth, BMP.Height, BMP.Width);
     NewWidth := MaxWidth;
    END;

  Stretch(BMP, NewWidth, NewHeight);
end;


{ Proportional. Height will be auto adjusted to match width }
procedure StretchProport (BMP: TBitmap; CONST OutWidth: Integer);
VAR
   Ratio: double;
   OutHeight: Integer;
begin
 Ratio:= BMP.Width / BMP.Height;
 OutHeight:= round(OutWidth / Ratio);
 Stretch(BMP, OutWidth, OutHeight);
end;


function StretchProportF(BMP: TBitmap; CONST MaxWidth, MaxHeight: Integer): TBitmap;
begin
 Result:= TBitmap.Create;
 Result.Assign(BMP);
 StretchProport(Result, MaxWidth, MaxHeight);
end;





{--------------------------------------------------------------------------------------------------
 Increases the size of the image with a  percent

 Zoom               : In/out
 Keep aspect ration : Yes
 Stretch provided in: %
--------------------------------------------------------------------------------------------------}
procedure StretchPercent(BMP: TBitmap; CONST ResizePercent: Integer);
VAR
   NewWidth, NewHeight: Integer;
begin
 if ResizePercent <= -100
 then RAISE Exception.Create('Invalid ResizePercent parameter!');                            { If I set ResizePercent to -100% I will get an image of size 0,0 }

 if ResizePercent = 0 then EXIT;

 { Recalculate width/height }
 NewWidth := BMP.Width + Round(ProcentNormal(ResizePercent, BMP.Width ));
 NewHeight:= BMP.Height+ Round(ProcentNormal(ResizePercent, BMP.Height ));

 Stretch(BMP, NewWidth, NewHeight);
end;


{--------------------------------------------------------------------------------------------------
 Increases the size of the image x times

 Zoom               : In/out
 Keep aspect ration : Yes
 Stretch provided in: %
--------------------------------------------------------------------------------------------------}
procedure StretchPercentX(BMP: TBitmap; CONST ResizeTimes: Single);
begin
 if ResizeTimes = 1 then EXIT;    { 1x means no resize }
 VAR TimesToPercent:= RoundEx(ResizeTimes * 100);
 StretchPercent(BMP, TimesToPercent);
end;


end.
