UNIT LightVcl.Graph.ResizeParams;

{=============================================================================================================
   Gabriel Moraru
   2026.01.30
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Parameters for resamplers in LightVcl.Graph.Resize.pas
   The record is filled with data from GUI (provided by LightVcl.Graph.ResizeParamEdt.pas).
   It also computes the optimal output width/height size based on user's input.
   Saves itself to a stream.

   Usage:
     1. Call Reset to initialize with default values
     2. Set MaxWidth/MaxHeight to target dimensions
     3. Set ResizeOpp to desired resize mode
     4. Call ComputeOutputSize(InputWidth, InputHeight)
     5. Read OutW/OutH for computed output dimensions

   External dependencies: None

   TESTER:
       c:\Projects\LightSaber ImageResampler Test\ResamplerTester.dpr
       c:\MyProjects\Projects GRAPH Resamplers\Tester for LightVcl.Graph.ResizeParam\
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
   System.SysUtils, LightVcl.Graph.Bitmap, LightCore.StreamBuff;

CONST
  UNINITIALIZED_SIZE = -7777;  { Sentinel value indicating OutW/OutH haven't been computed yet }

TYPE
 { Resize operation modes }
 TResizeOp=
   (roAutoDetect,   { Auto-select Fill or Fit based on how much would be cropped. Uses FitTolerance. }
    roCustom,       { Resize by CustomZoom factor. 1.0 = no change, 1.5 = 150%, 0.5 = 50% }
    roNone,         { No resize - keep original dimensions }
    roFill,         { Fill viewport completely, crop edges that overflow (no black bars) }
    roFit,          { Fit within viewport, add black bars if needed (no cropping) }
    roForceWidth,   { Force output to ForcedWidth, compute height to maintain aspect ratio }
    roForceHeight,  { Force output to ForcedHeight, compute width to maintain aspect ratio }
    roStretch       { Force exact MaxWidth x MaxHeight, distorts aspect ratio! }
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
 RResizeParams = record
  private
    procedure computeAutodetect(InpW, InpH: Integer);
    procedure computeFit(InpW, InpH: Integer);
    procedure computeFill(InpW, InpH: Integer);
  public
    { Input parameters - set these before calling ComputeOutputSize }
    ResizeOpp    : TResizeOp;   { Resize mode (roFit, roFill, roAutoDetect, etc.) }
    MaxZoomUse   : Boolean;     { Enable max zoom limit in roAutoDetect mode }
    MaxZoomVal   : Integer;     { Max zoom percentage (50 = 150% max). Only used with MaxZoomUse. }
    CustomZoom   : Single;      { Zoom factor for roCustom mode (1.0 = 100%, 1.5 = 150%, 0.5 = 50%) }
    ForcedWidth  : Integer;     { Target width for roForceWidth mode }
    ForcedHeight : Integer;     { Target height for roForceHeight mode }
    ResizePanoram: Boolean;     { If FALSE, panoramic images are not resized }
    FitTolerance : Byte;        { Percentage to over-zoom when falling back from Fill to Fit (reduces black bars) }
    MaxWidth     : Integer;     { Target viewport width }
    MaxHeight    : Integer;     { Target viewport height }
    { Output - computed by ComputeOutputSize }
    OutW         : Integer;     { Computed output width (UNINITIALIZED_SIZE if not computed) }
    OutH         : Integer;     { Computed output height (UNINITIALIZED_SIZE if not computed) }

    procedure Reset;                                      { Initialize all fields to default values }
    procedure ComputeOutputSize(InpW, InpH: Integer);     { Compute OutW/OutH based on input dimensions }

    procedure WriteToStream(IOStream: TLightStream);
    procedure ReadFromStream(IOStream: TLightStream);

    procedure Convert(Res: TResizeOpp_; FT: TFillType_);  { Convert from old BioniX v12 format }
 end;


IMPLEMENTATION

USES
  Math, LightCore.Math;


{ Initialize all fields to sensible defaults.
  Call this before setting custom values. }
procedure RResizeParams.Reset;
begin
  ResizeOpp     := roAutoDetect;
  MaxZoomVal    := 50;          { 50% max zoom = 1.5x magnification limit }
  MaxZoomUse    := TRUE;
  CustomZoom    := 1.5;         { Default 150% for roCustom mode }
  MaxWidth      := 1920;        { Default HD width - typically overridden by caller }
  MaxHeight     := 1200;        { Default height - typically overridden by caller }
  OutW          := UNINITIALIZED_SIZE;  { Sentinel: not yet computed }
  OutH          := UNINITIALIZED_SIZE;  { Sentinel: not yet computed }
  FitTolerance  := 10;          { 10% over-zoom tolerance when falling back to Fit }
  ResizePanoram := FALSE;       { Don't resize panoramic images by default }
  ForcedWidth   := 800;         { Default for roForceWidth mode }
  ForcedHeight  := 600;         { Default for roForceHeight mode }
end;




{--------------------------------------------------------------------------------------------------
   Compute output - Private helper methods
   These assume validation has been done by ComputeOutputSize
--------------------------------------------------------------------------------------------------}

{ Fill mode: Scale image to completely fill the viewport.
  At least one dimension equals the viewport, the other may exceed it (will be cropped).
  No black bars, but edges may be cut off. }
procedure RResizeParams.ComputeFill(InpW, InpH: Integer);
var ZoomRatio, ARi, ARo: Single;
begin
 Assert(InpH > 0, 'ComputeFill: InpH must be > 0');
 Assert(MaxHeight > 0, 'ComputeFill: MaxHeight must be > 0');
 Assert(MaxWidth > 0, 'ComputeFill: MaxWidth must be > 0');

 ARi:= InpW / InpH;              { Aspect Ratio of Input image }
 ARo:= MaxWidth / MaxHeight;     { Aspect Ratio of Output viewport }

 if ARi <= ARo
 then
   begin
    { Image is narrower than viewport - constrain by width }
    ZoomRatio:= InpW / MaxWidth;
    OutW:= MaxWidth;
    OutH:= RoundEx(InpH / ZoomRatio);
   end
 else
   begin
    { Image is wider than viewport - constrain by height }
    ZoomRatio:= InpH / MaxHeight;
    OutH:= MaxHeight;
    OutW:= RoundEx(InpW / ZoomRatio);
   end;
end;



{ Fit mode: Scale image to fit entirely within the viewport.
  At least one dimension equals the viewport, the other is smaller.
  No cropping, but may have black bars on sides or top/bottom. }
procedure RResizeParams.ComputeFit(InpW, InpH: Integer);
var ZoomRatio, ARi, ARo: Single;
begin
 Assert(InpH > 0, 'ComputeFit: InpH must be > 0');
 Assert(MaxHeight > 0, 'ComputeFit: MaxHeight must be > 0');
 Assert(MaxWidth > 0, 'ComputeFit: MaxWidth must be > 0');

 ARi:= InpW / InpH;              { Aspect Ratio of Input image }
 ARo:= MaxWidth / MaxHeight;     { Aspect Ratio of Output viewport }

 if ARi <= ARo
 then
   begin
    { Image is narrower than viewport - constrain by height }
    ZoomRatio:= InpH / MaxHeight;
    OutH:= MaxHeight;
    OutW:= RoundEx(InpW / ZoomRatio);
   end
 else
   begin
    { Image is wider than viewport - constrain by width }
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
  Overshoot:= LightCore.Math.ProcentRepresent(OutW*OutH, OutputArea);
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



{ Calculates output dimensions (OutW/OutH) based on input dimensions and current settings.
  InpW/InpH: Size of the input image (must be > 0).
  After calling, read OutW and OutH for the computed output dimensions.
  Raises exception if parameters are invalid. }
procedure RResizeParams.ComputeOutputSize(InpW, InpH: Integer);
VAR
   Ratio: Real;
begin
  { Validate viewport dimensions }
  if MaxWidth <= 0
  then RAISE Exception.Create('ComputeOutputSize: MaxWidth must be > 0');

  if MaxHeight <= 0
  then RAISE Exception.Create('ComputeOutputSize: MaxHeight must be > 0');

  { Validate input dimensions }
  if InpW <= 0
  then RAISE Exception.Create('ComputeOutputSize: Input width must be > 0');

  if InpH <= 0
  then RAISE Exception.Create('ComputeOutputSize: Input height must be > 0');

  { Panoramic images are not resized by default (but may still be compressed) }
  if LightVcl.Graph.Bitmap.IsPanoramic(InpW, InpH) AND NOT ResizePanoram
  then
   begin
    OutW:= InpW;
    OutH:= InpH;
    EXIT;
   end;

  case ResizeOpp of
    roAutoDetect:
      computeAutodetect(InpW, InpH);

    roNone:
      begin
       OutW:= InpW;
       OutH:= InpH;
      end;

    roCustom:
      begin
       if CustomZoom <= 0
       then RAISE Exception.Create('ComputeOutputSize: CustomZoom must be > 0');

       OutW:= RoundEx(InpW * CustomZoom);
       OutH:= RoundEx(InpH * CustomZoom);
      end;

    roFit:
      ComputeFit(InpW, InpH);

    roFill:
      ComputeFill(InpW, InpH);

    roStretch:
      begin
       OutW:= MaxWidth;
       OutH:= MaxHeight;
      end;

    roForceWidth:
      begin
       if ForcedWidth <= 0
       then RAISE Exception.Create('ComputeOutputSize: ForcedWidth must be > 0');

       Ratio:= InpW / InpH;
       OutW:= ForcedWidth;
       OutH:= RoundEx(ForcedWidth / Ratio);
      end;

    roForceHeight:
      begin
       if ForcedHeight <= 0
       then RAISE Exception.Create('ComputeOutputSize: ForcedHeight must be > 0');

       Ratio:= InpH / InpW;
       OutW:= RoundEx(ForcedHeight / Ratio);
       OutH:= ForcedHeight;
      end;

    else
      RAISE Exception.Create('ComputeOutputSize: Unknown resize operation.');
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
   Stream I/O
   Note: MaxWidth, MaxHeight, OutW, OutH are NOT saved - they should be recalculated.
--------------------------------------------------------------------------------------------------}

procedure RResizeParams.WriteToStream(IOStream: TLightStream);
begin
   Assert(IOStream <> NIL, 'WriteToStream: IOStream cannot be nil');

   IOStream.WriteByte(Ord(ResizeOpp));
   IOStream.WriteBoolean(MaxZoomUse);
   IOStream.WriteInteger(MaxZoomVal);
   IOStream.WriteSingle(CustomZoom);
   IOStream.WriteInteger(ForcedWidth);
   IOStream.WriteInteger(ForcedHeight);
   IOStream.WriteBoolean(ResizePanoram);
   IOStream.WriteByte(FitTolerance);
   IOStream.WritePadding;
end;


procedure RResizeParams.ReadFromStream(IOStream: TLightStream);
VAR OpByte: Byte;
begin
   Assert(IOStream <> NIL, 'ReadFromStream: IOStream cannot be nil');

   { Read and validate enum value }
   OpByte:= IOStream.ReadByte;
   if Integer(OpByte) > Ord(High(TResizeOp))
   then RAISE Exception.Create('ReadFromStream: Invalid ResizeOp value in stream: ' + IntToStr(OpByte));
   ResizeOpp:= TResizeOp(OpByte);

   MaxZoomUse   := IOStream.ReadBoolean;
   MaxZoomVal   := IOStream.ReadInteger;
   CustomZoom   := IOStream.ReadSingle;
   ForcedWidth  := IOStream.ReadInteger;
   ForcedHeight := IOStream.ReadInteger;
   ResizePanoram:= IOStream.ReadBoolean;
   FitTolerance := IOStream.ReadByte;
   IOStream.ReadPadding;
end;


end.
