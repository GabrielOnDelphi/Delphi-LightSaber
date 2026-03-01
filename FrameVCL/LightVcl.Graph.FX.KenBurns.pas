UNIT LightVcl.Graph.FX.KenBurns;

{=============================================================================================================
   2026.03.01
   Author: Gabriel Moraru
--------------------------------------------------------------------------------------------------------------
   Ken Burns effect: smooth pan/zoom animation over a static image.

   Pure rendering logic with no application dependencies. Reusable in any VCL application.

   Workflow:
     1. Load source image at oversized resolution (e.g., 1.5x viewport) for zoom headroom
     2. Call GenerateRandomPath to create a random pan/zoom trajectory
     3. On each frame timer tick, call KenBurnsFrame with current progress (0..1)
     4. The function interpolates between start/end crop rectangles and renders to Output

   All coordinates use normalized 0..1 space so the engine is resolution-independent.
==================================================================================================}

INTERFACE

USES
  Winapi.Windows, System.SysUtils, System.Types, System.Math, Vcl.Graphics;

TYPE
  TEasingType = (etLinear, etSmoothStep, etEaseInOut, etEaseIn, etEaseOut);

  { Defines the pan/zoom trajectory as two normalized crop rectangles.
    At progress=0 we show StartRect; at progress=1 we show EndRect.
    Coordinates are normalized (0..1) relative to the source image dimensions. }
  RKenBurnsPath = record
    StartRect: TRectF;
    EndRect  : TRectF;
  end;

{ Generates a random pan/zoom path within the source image.
  ZoomRange: fractional zoom difference (0.1 = 10% zoom change, 0.2 = 20%, etc.)
  PanRange:  fractional pan distance (0.15 = center can drift up to 15% of image) }
function  GenerateRandomPath(ZoomRange, PanRange: Single): RKenBurnsPath;

{ Renders a single Ken Burns frame.
  Source:   oversized source bitmap (loaded at > viewport resolution)
  Output:   viewport-sized bitmap (receives the rendered frame)
  Progress: animation progress 0..1
  Path:     the pan/zoom trajectory
  Easing:   easing function to apply to progress }
procedure KenBurnsFrame(Source, Output: TBitmap; Progress: Single;
            const Path: RKenBurnsPath; Easing: TEasingType);

{ Applies easing function to a linear 0..1 value }
function  ApplyEasing(T: Single; Easing: TEasingType): Single;

{ Linearly interpolates between two rectangles }
function  LerpRectF(const A, B: TRectF; T: Single): TRectF;

{ Returns a human-readable name for the easing type (for UI combo boxes) }
function  EasingName(Easing: TEasingType): string;

{ Converts easing index (0-based) to TEasingType }
function  IndexToEasing(Index: Integer): TEasingType;


IMPLEMENTATION


function ApplyEasing(T: Single; Easing: TEasingType): Single;
begin
 { Clamp input }
 if T < 0 then T:= 0;
 if T > 1 then T:= 1;

 case Easing of
   etLinear    : Result:= T;
   etSmoothStep: Result:= T * T * (3.0 - 2.0 * T);
   etEaseInOut :
     if T < 0.5
     then Result:= 2.0 * T * T
     else Result:= 1.0 - Power(-2.0 * T + 2.0, 2) / 2.0;
   etEaseIn    : Result:= T * T;
   etEaseOut   : Result:= 1.0 - (1.0 - T) * (1.0 - T);
  else
   Result:= T;
 end;
end;


function LerpRectF(const A, B: TRectF; T: Single): TRectF;
begin
 Result.Left  := A.Left   + (B.Left   - A.Left)   * T;
 Result.Top   := A.Top    + (B.Top    - A.Top)    * T;
 Result.Right := A.Right  + (B.Right  - A.Right)  * T;
 Result.Bottom:= A.Bottom + (B.Bottom - A.Bottom) * T;
end;


{ Clamps a normalized rect so it stays within 0..1 bounds }
procedure ClampRect(var R: TRectF);
begin
 if R.Left < 0 then
  begin
   R.Right:= R.Right - R.Left;
   R.Left := 0;
  end;
 if R.Top < 0 then
  begin
   R.Bottom:= R.Bottom - R.Top;
   R.Top   := 0;
  end;
 if R.Right > 1.0 then
  begin
   R.Left := R.Left - (R.Right - 1.0);
   R.Right:= 1.0;
  end;
 if R.Bottom > 1.0 then
  begin
   R.Top   := R.Top - (R.Bottom - 1.0);
   R.Bottom:= 1.0;
  end;

 { Final safety clamp }
 if R.Left   < 0   then R.Left  := 0;
 if R.Top    < 0   then R.Top   := 0;
 if R.Right  > 1.0 then R.Right := 1.0;
 if R.Bottom > 1.0 then R.Bottom:= 1.0;
end;


function GenerateRandomPath(ZoomRange, PanRange: Single): RKenBurnsPath;
VAR
   StartSize, EndSize: Single;
   CenterX, CenterY: Single;
   DriftX, DriftY: Single;
begin
 { Ensure non-negative parameters }
 ZoomRange:= Abs(ZoomRange);
 PanRange := Abs(PanRange);

 { One rect is "wide" (full or near-full image), the other is "tight" (zoomed in).
   This guarantees visible zoom movement throughout the animation. }
 StartSize:= 1.0 - ZoomRange * 0.2 * Random;        { Wide: 94-100% of image }
 EndSize  := 1.0 - ZoomRange * (0.6 + 0.4 * Random); { Tight: zoom in noticeably }

 { Randomly swap so sometimes we zoom in, sometimes we zoom out }
 if Random > 0.5 then
  begin
   DriftX:= StartSize;   { Reuse DriftX as temp }
   StartSize:= EndSize;
   EndSize:= DriftX;
  end;

 { Clamp sizes to reasonable range }
 if StartSize < 0.4 then StartSize:= 0.4;
 if StartSize > 1.0 then StartSize:= 1.0;
 if EndSize   < 0.4 then EndSize  := 0.4;
 if EndSize   > 1.0 then EndSize  := 1.0;

 { Random center position for start rect }
 CenterX:= 0.5 + (Random - 0.5) * PanRange;
 CenterY:= 0.5 + (Random - 0.5) * PanRange;

 { Build start rect centered at (CenterX, CenterY) }
 Result.StartRect.Left  := CenterX - StartSize / 2;
 Result.StartRect.Top   := CenterY - StartSize / 2;
 Result.StartRect.Right := CenterX + StartSize / 2;
 Result.StartRect.Bottom:= CenterY + StartSize / 2;

 { Stronger pan drift for end rect — use full PanRange }
 DriftX:= (Random - 0.5) * PanRange * 1.5;
 DriftY:= (Random - 0.5) * PanRange * 1.5;

 { Build end rect with drift }
 Result.EndRect.Left  := (CenterX + DriftX) - EndSize / 2;
 Result.EndRect.Top   := (CenterY + DriftY) - EndSize / 2;
 Result.EndRect.Right := (CenterX + DriftX) + EndSize / 2;
 Result.EndRect.Bottom:= (CenterY + DriftY) + EndSize / 2;

 { Ensure both rects stay within 0..1 }
 ClampRect(Result.StartRect);
 ClampRect(Result.EndRect);
end;


procedure KenBurnsFrame(Source, Output: TBitmap; Progress: Single;
            const Path: RKenBurnsPath; Easing: TEasingType);
VAR
   EasedT: Single;
   CropRect: TRectF;
   SrcX, SrcY, SrcW, SrcH: Integer;
   OldMode: Integer;
begin
 Assert(Source <> NIL, 'KenBurnsFrame: Source bitmap is nil');
 Assert(Output <> NIL, 'KenBurnsFrame: Output bitmap is nil');
 Assert(Source.Width  > 0, 'KenBurnsFrame: Source has zero width');
 Assert(Source.Height > 0, 'KenBurnsFrame: Source has zero height');
 Assert(Output.Width  > 0, 'KenBurnsFrame: Output has zero width');
 Assert(Output.Height > 0, 'KenBurnsFrame: Output has zero height');

 { Clamp progress }
 if Progress < 0 then Progress:= 0;
 if Progress > 1 then Progress:= 1;

 { Apply easing }
 EasedT:= ApplyEasing(Progress, Easing);

 { Interpolate current crop rectangle }
 CropRect:= LerpRectF(Path.StartRect, Path.EndRect, EasedT);

 { Convert normalized coords to pixel coords on source }
 SrcX:= Round(CropRect.Left   * Source.Width);
 SrcY:= Round(CropRect.Top    * Source.Height);
 SrcW:= Round(CropRect.Width  * Source.Width);
 SrcH:= Round(CropRect.Height * Source.Height);

 { Safety clamp to source dimensions }
 if SrcX < 0 then SrcX:= 0;
 if SrcY < 0 then SrcY:= 0;
 if SrcX + SrcW > Source.Width  then SrcW:= Source.Width  - SrcX;
 if SrcY + SrcH > Source.Height then SrcH:= Source.Height - SrcY;
 if SrcW < 1 then SrcW:= 1;
 if SrcH < 1 then SrcH:= 1;

 { High-quality stretch from source crop to output }
 OldMode:= SetStretchBltMode(Output.Canvas.Handle, HALFTONE);
 TRY
   SetBrushOrgEx(Output.Canvas.Handle, 0, 0, NIL);
   StretchBlt(
     Output.Canvas.Handle,
       0, 0, Output.Width, Output.Height,
     Source.Canvas.Handle,
       SrcX, SrcY, SrcW, SrcH,
     SRCCOPY);
 FINALLY
   SetStretchBltMode(Output.Canvas.Handle, OldMode);
 END;
end;


function EasingName(Easing: TEasingType): string;
begin
 case Easing of
   etLinear    : Result:= 'Linear';
   etSmoothStep: Result:= 'Smooth';
   etEaseInOut : Result:= 'Ease In/Out';
   etEaseIn    : Result:= 'Ease In';
   etEaseOut   : Result:= 'Ease Out';
  else
   Result:= 'Unknown';
 end;
end;


function IndexToEasing(Index: Integer): TEasingType;
begin
 if (Index < Ord(Low(TEasingType))) OR (Index > Ord(High(TEasingType)))
 then EXIT(etSmoothStep);

 Result:= TEasingType(Index);
end;


end.
