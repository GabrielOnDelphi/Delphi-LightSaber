UNIT cGraphResizeParams;

{=============================================================================================================
   Gabriel Moraru
   2023.08.05
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
   Parameters for resamplers in cGraphResize.pas
   The record is filled with data from GUI (provoded by cGraphResizeParamEdt.pas).
   It also computes the optimal output width/height size based on user's input.
   Saves itself to a stream.

   External dependencies: None

   TESTER:
       c:\Projects\LightSaber ImageResampler Test\ResamplerTester.dpr
       c:\MyProjects\Projects GRAPH Resamplers\Tester for cGraphResizeParam\
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
   System.SysUtils, cGraphBitmap, ccStreamBuff;

TYPE
 TResizeOp=
   (roAutoDetect,   // Auto decide if we use Fill or Fit. Example: if the algorithm has to cut too much from the image then use Fit. If the image is almost the same size as the screen and the algorithm has to cut only a bit from the image, use Fill.
    roCustom,       // The image will be resized according to the ResizeFactor. Example: 0.9 to decrease image to 90% of its original size. User 1.1 to increase image with 10%
    roNone,         // No zoom (original img size)
    roFill,         // Remove black stripes. The parts of img that go out of the screen are cropped.
    roFit,          // Adds black stripes. Does not cut from image.
    roForceWidth,   // The image will have the Width  specified by MaxWidth_.  The Width  will be automatically adjusted to match the width  (aspect ratio is kept)
    roForceHeight,  // The image will have the Height specified by MaxHeight. The Height will be automatically adjusted to match the height (aspect ratio is kept)
    roStretch       // The image will have the W/H specified by MaxWidth_/MaxHeight. This will distort the aspect ratio!
    );

{ OLD FORMATS!! Do not used them anymore }
TYPE
   TFillType_ = (ftAuto,        // Let the program decide if it will use Fill or Fit. Example: if the algorithm has to cut too much from the image then use Fit. If the image is almost the same size as the screen and the algorithm has to cut only a bit from the image, use Fill.
                ftFill,         // Remove black stripes. The parts of img that go out of the screen are cropped.
                ftFit);         // Adds black stripes. Does not cut from image );

   TResizeOpp_= (rsAutoDetect,
                rsResizeUp,
                rsResizeDown,   // The image will be resized according to the ResizeFactor.
                rsForceWidth,   // The image will have the Width  specified by MaxWidth_. The height will be automatically adjusted to match the width  (aspect ratio is kept)
                rsForceHeight,  // The image will have the height specified by MaxWidth_. The Width  will be automatically adjusted to match the height (aspect ratio is kept)
                rsForceWH,      // The image will have the W/H specified by MaxWidth_/MaxHeight. This will distort the aspect ratio!
                rsNone,         // No zoom (original img size)
                rsCustom);      // NOT IMPLEMENTED. Example: zoom -80%

 PResizeParams = ^RResizeParams;
 RResizeParams= record
  private
    procedure computeAutodetect(InpW, InpH: Integer);
    procedure computeFit       (InpW, InpH: Integer);
    procedure computeFill      (InpW, InpH: Integer);
  public
    { Input }
    ResizeOpp    : TResizeOp;
    MaxZoomUse   : Boolean;    // Shows if this feature is enabled/disabled. Only used in roAutoDetect mode
    MaxZoomVal   : Integer;    // The actual value
    CustomZoom   : Single;     // 1.0 means no zoom. 1.5 means 50% zoom
    ForcedWidth  : Integer;    // Applies only when roForceWidth is used
    ForcedHeight : Integer;
    ResizePanoram: Boolean;
    FitTolerance : Byte;       // If we fail to FILL the image, then we have to fall back to FIT which we don't want becauseof the black bars. But we can still try to partially get rid of these bars, by over-fitting (over zooming) the image on the screen, with 10%
    MaxWidth     : Integer;
    MaxHeight    : Integer;
    { Output }
    OutW         : Integer;
    OutH         : Integer;
    procedure Reset;                                           { Reset to default values }
    procedure ComputeOutputSize(InpW, InpH: Integer);

    procedure WriteToStream (IOStream: TCubicBuffStream);
    procedure ReadFromStream(IOStream: TCubicBuffStream);

    procedure Convert(Res: TResizeOpp_; FT: TFillType_);        { Used to convert from the old format used by BioniX v12 to the new format }
 end;


IMPLEMENTATION

USES
  Math, cmMath;


procedure RResizeParams.Reset;
begin
  ResizeOpp     := roAutoDetect;
  MaxZoomVal    := 50;
  MaxZoomUse    := TRUE;
  CustomZoom    := 1.5;    { times }
  MaxWidth      := -7777;  { This will be provided by TWallpaperBase.LoadFromFile(MaxWidth, MaxHeight: Integer) }
  MaxHeight     := -7777;
  OutW          := -7777;
  OutH          := -7777;
  FitTolerance  := 10;     { % }
  ResizePanoram := FALSE;
  ForcedWidth   := 800;
  ForcedHeight  := 600;
end;




{--------------------------------------------------------------------------------------------------
   Compute output
--------------------------------------------------------------------------------------------------}

procedure RResizeParams.ComputeFill(InpW, InpH: Integer);
var ZoomRatio, ARi, ARo: Single;
begin
 ARi:= InpW / InpH;              // Aspect Ratio of Input image
 ARo:= MaxWidth / MaxHeight;     // Aspect Ratio of Output port

 if ARi <= ARo
 then
   begin
    ZoomRatio:= InpW / MaxWidth;
    OutW:= MaxWidth;
    OutH:= RoundEx(InpH / ZoomRatio);
   end
 else
    begin
     ZoomRatio:= InpH / MaxHeight;
     OutH:= MaxHeight;
     OutW:= RoundEx(InpW / ZoomRatio);
    end
end;



procedure RResizeParams.ComputeFit(InpW, InpH: Integer);
var ZoomRatio, ARi, ARo: Single;
begin
 ARi:= InpW / InpH;              // Aspect Ratio of Input image
 ARo:= MaxWidth / MaxHeight;     // Aspect Ratio of Output port

 if ARi <= ARo
 then
   begin
    ZoomRatio:= InpH / MaxHeight;
    OutH:= MaxHeight;
    OutW:= RoundEx(InpW / ZoomRatio);
   end
 else
   begin
    ZoomRatio:= InpW / MaxWidth;
    OutW:= MaxWidth;
    OutH:= RoundEx(InpH / ZoomRatio);
   end;
end;



procedure RResizeParams.computeAutodetect(InpW, InpH: Integer);
VAR
   OutputArea: Integer;
   MaxMultiplicator, Multiplicator: Single;
   OverShoot: Single;
   ZoomIn: Boolean;
begin
  Assert(MaxZoomVal >= -99, 'Cannot decrease size with 100%');

  { No magnification }
  if (InpW= MaxWidth) AND (InpH= MaxHeight) then
   begin
    OutW:= InpW;
    OutH:= InpH;
    EXIT;  { Return the image as it is }
   end;

 //InputArea := InpW * InpH;
  OutputArea:= MaxWidth * MaxHeight;

  { FILL }
  ComputeFill(InpW, InpH);                   { First we try to fill the entire screen }
  ZoomIn:= (OutW > InpW) OR (OutH > InpH);

  { MaxZoom}                                 { We limit the zoom to the max value specified by the user: }
  if ZoomIn                                  { ...we must appply Zoom in }
  AND MaxZoomUse
  AND (MaxZoomVal <> 0) then                 { ...but the use wants MaxZoom }
   begin
     Multiplicator:= Min((MaxWidth / InpW), (MaxHeight / InpH));
     MaxMultiplicator:= (MaxZoomVal / 100) +1;  { Convert zoom factor from pecents to decimal. Example: 50% = 1.5x zoom }

     if Multiplicator   > MaxMultiplicator
     then Multiplicator:= MaxMultiplicator;

     { We apply zoom BUT not more that what the user wanted }
     if  (InpW * Multiplicator < MaxWidth)
     AND (InpH * Multiplicator < Maxheight) then
      begin
       OutW:= RoundEx(InpW * Multiplicator);
       OutH:= RoundEx(InpH * Multiplicator);
      end;
   end;

  { Overshoot! }                             { If after zoom, we loose more than 10% of the image, we go back to Fit }
  Overshoot:= cmMath.ProcentRepresent(OutW*OutH, OutputArea);
  if Overshoot > 105 then
   begin
    ComputeFit(InpW, InpH);

    { Still, zoom in 10% }                   { So, we failed to FILL and we have to use FIT, but we can still try to increase the size of the image with 10% in order to get rid of some of the black bars }
    if NOT ZoomIn                            { Don't try to increase with 10% if we ALREADY had to increase the image size }
    AND (FitTolerance > 0) then
     begin
      OutW:= OutW + RoundEx(ProcentNormal(FitTolerance, OutW));
      OutH:= OutH + RoundEx(ProcentNormal(FitTolerance, OutH));
     end;
   end;
end;



{ Calculates which are the output W/H in such way that it fits the requirements of the user without distorting the aspect ratio.
  InpW/InpH is the size of the input BMP }
procedure RResizeParams.ComputeOutputSize(InpW, InpH: Integer);
VAR
   Ratie: Real;
begin
  if (MaxWidth <= 0)
  then RAISE Exception.Create('Invalid OutWidth  in TImgResizer');
  if (MaxHeight <= 0)
  then RAISE Exception.Create('Invalid OutHeight in TImgResizer');

  { Panoramic images will not be resized (but will still be compressed) }
  if cGraphBitmap.IsPanoramic(InpW, InpH)
  AND NOT ResizePanoram then
   begin
    OutW:= InpW;
    OutH:= InpH;
    EXIT;
   end;

  case ResizeOpp of

    { AUTODETECT }
    roAutoDetect:
      computeAutodetect(InpW, InpH);

    { NO RESIZE }
    roNone:
     begin
      OutW:= InpW;
      OutH:= InpH;
     end;

    { CUSTOM ZOOM }
    roCustom:
      begin
       OutW:= RoundEx(InpW * CustomZoom); { Same zoom for both W and H }
       OutH:= RoundEx(InpH * CustomZoom);
      end;

    { sssss }
    roFit:
       ComputeFit(InpW, InpH);

    roFill:
       ComputeFill(InpW, InpH);

    { SAME W/H }
    roStretch:
      begin
       OutW:= MaxWidth;
       OutH:= MaxHeight;
      end;

    { SAME WIDTH }
    roForceWidth:
        begin
         Ratie:= InpW / InpH;
         OutW:= ForcedWidth;
         OutH:= RoundEx(ForcedWidth / Ratie);
        end;

    { SAME HEIGHT }
    roForceHeight:
        begin
         Ratie:= InpH / InpW;
         OutW:= RoundEx(ForcedHeight /Ratie);
         OutH:= ForcedHeight;
        end;

     else
      RAISE Exception.Create('Unknown case.');
  end;
end;


{ Used to convert from the old format used by BioniX v12 to the new format }
procedure RResizeParams.Convert(Res: TResizeOpp_; FT: TFillType_);
begin
  case Res of                                 // THE DOCUMENTATION BELOW IS FOR THE OLD FORMAT!
    rsAutoDetect : ResizeOpp:= roAutoDetect;
    rsResizeUp   : ResizeOpp:= roNone;
    rsResizeDown : ResizeOpp:= roNone;        // The image will be resized according to the ResizeFactor.
    rsForceWidth : ResizeOpp:= roForceWidth;  // The image will have the Width  specified by MaxWidth_. The height will be automatically adjusted to match the width  (aspect ratio is kept)
    rsForceHeight: ResizeOpp:= roForceHeight; // The image will have the height specified by MaxWidth_. The Width  will be automatically adjusted to match the height (aspect ratio is kept)
    rsForceWH    : ResizeOpp:= roStretch;     // The image will have the W/H specified by MaxWidth_/MaxHeight. This will distort the aspect ratio!
    rsNone       : ResizeOpp:= roNone;        // No zoom (original img size)
    rsCustom     : ResizeOpp:= roCustom;      // NOT IMPLEMENTED. Example: zoom -80%
  end;

  case FT of
    ftAuto: ResizeOpp:= roAutoDetect;
    ftFill: ResizeOpp:= roFill;
    ftFit : ResizeOpp:= roFit;
  end;
end;






{--------------------------------------------------------------------------------------------------
   Stream
--------------------------------------------------------------------------------------------------}

procedure RResizeParams.WriteToStream(IOStream: TCubicBuffStream);
begin
{ These are not saved, because they should be recalculated:
   MaxWidth, MaxHeight: Integer;
   OutW, OutH         : Integer;   }
   IOStream.WriteByte    (Ord(ResizeOpp));
   IOStream.WriteBoolean (MaxZoomuse);
   IOStream.WriteInteger (MaxZoomVal);
   IOStream.WriteSingle  (CustomZoom);
   IOStream.WriteInteger (ForcedWidth);
   IOStream.WriteInteger (ForcedHeight);
   IOStream.WriteBoolean (ResizePanoram);
   IOStream.WriteByte    (FitTolerance);
   IOStream.WritePadding(32);
end;


procedure RResizeParams.ReadFromStream(IOStream: TCubicBuffStream);
begin
   ResizeOpp    := TResizeOp(IOStream.ReadByte);
   MaxZoomUse   := IOStream.ReadBoolean;
   MaxZoomVal   := IOStream.ReadInteger;
   CustomZoom   := IOStream.ReadSingle;
   ForcedWidth  := IOStream.ReadInteger;
   ForcedHeight := IOStream.ReadInteger;
   ResizePanoram:= IOStream.ReadBoolean;
   FitTolerance := IOStream.ReadByte;
   IOStream.ReadPadding(32);
end;


end.
