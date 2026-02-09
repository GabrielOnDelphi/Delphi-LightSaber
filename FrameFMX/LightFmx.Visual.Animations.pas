UNIT LightFmx.Visual.Animations;

{=============================================================================================================
   2026.01.31
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Celebration Confetti Animation

   Displays falling confetti animation for celebrating achievements.
   Each confetti piece has:
     - Fall animation: Drops from above the control to below with quadratic easing
     - Drift animation: Horizontal wave motion with auto-reverse loop
     - Rotation animation: Spins as it falls
     - Fade animation: Fades out during the last 40% of the fall
   Random variations in size, starting position, duration, and colors.
   Automatic cleanup when animations complete.

   Requires Randomize;

   ShowConfetti(MyForm, 1.0);
=============================================================================================================}

INTERFACE

USES
  FMX.Types;

TYPE
  TConfetti = class
    class procedure OnAnimFinish(Sender: TObject);
    { Parent can be TForm or TControl }
    class procedure ShowConfetti(Parent: TFmxObject; SizeMultiplier: Single; ConfettiCount: Integer);
  end;


IMPLEMENTATION

USES
  System.SysUtils, System.Classes, System.UITypes, System.Math,
  FMX.Controls, FMX.Forms, FMX.Objects, FMX.Ani, FMX.Graphics;

CONST
  ConfettiColors: array[0..6] of TAlphaColor = (
    TAlphaColorRec.Gold,
    TAlphaColorRec.Crimson,
    TAlphaColorRec.Dodgerblue,
    TAlphaColorRec.Lime,
    TAlphaColorRec.Magenta,
    TAlphaColorRec.Orange,
    TAlphaColorRec.Cyan);


class procedure TConfetti.OnAnimFinish(Sender: TObject);
VAR
  Anim: TFloatAnimation;
  Piece: TFmxObject;
begin
  Anim:= Sender as TFloatAnimation;
  Piece:= Anim.Parent;
  if Assigned(Piece) then
  begin
    Piece.Parent:= nil;  // Detach from parent so form's DeleteChildren won't double-free
    // ForceQueue ensures destruction happens after current animation processing completes
    TThread.ForceQueue(nil, procedure
    begin
      FreeAndNil(Piece);
    end);
  end;
end;


{ SizeMultiplier: 1.0 = default size (width 8-16, height 6-12)
                  1.5 = 50% larger confetti
                  0.8 = 20% smaller confetti }
class procedure TConfetti.ShowConfetti(Parent: TFmxObject; SizeMultiplier: Single; ConfettiCount: Integer);
VAR
  i: Integer;
  Confetti: TRectangle;
  FallAnim: TFloatAnimation;
  DriftAnim: TFloatAnimation;
  RotateAnim: TFloatAnimation;
  FadeAnim: TFloatAnimation;
  StartX, Duration, FinalY: Single;
  BaseWidth, BaseHeight: Single;
  ParentWidth, ParentHeight: Single;
begin
  Assert(Parent <> NIL, 'ShowConfetti: Parent cannot be nil');

  // Get parent dimensions (forms and controls have different properties)
  if Parent is TControl
  then
    begin
      ParentWidth:= TControl(Parent).Width;
      ParentHeight:= TControl(Parent).Height;
    end
  else if Parent is TCommonCustomForm
  then
    begin
      ParentWidth:= TCommonCustomForm(Parent).ClientWidth;
      ParentHeight:= TCommonCustomForm(Parent).ClientHeight;
    end
  else
    raise Exception.Create('ShowConfetti: Parent must be TControl or TForm');

  // Base sizes scaled by multiplier
  BaseWidth:= 8 * SizeMultiplier;
  BaseHeight:= 6 * SizeMultiplier;

  for i:= 0 to ConfettiCount - 1 do
  begin
    // Create confetti piece
    Confetti:= TRectangle.Create(Parent);
    Confetti.Parent:= Parent;
    Confetti.Width:= BaseWidth + Random(Round(8 * SizeMultiplier));
    Confetti.Height:= BaseHeight + Random(Round(6 * SizeMultiplier));
    Confetti.Fill.Color:= ConfettiColors[Random(Length(ConfettiColors))];
    Confetti.Stroke.Kind:= TBrushKind.None;
    Confetti.XRadius:= 2;
    Confetti.YRadius:= 2;

    // Random starting position (spread across top of parent)
    StartX:= Random(Round(ParentWidth));
    Confetti.Position.X:= StartX;
    Confetti.Position.Y:= -20 - Random(100);  // Start above the parent, staggered
    Confetti.RotationAngle:= Random(360);
    Confetti.Opacity:= 0.9;

    // Fall animation (Y position)
    Duration:= 2.0 + Random * 2.0;  // 2-4 seconds
    FinalY:= ParentHeight + 50;

    FallAnim:= TFloatAnimation.Create(Confetti);
    FallAnim.Parent:= Confetti;
    FallAnim.PropertyName:= 'Position.Y';
    FallAnim.StartValue:= Confetti.Position.Y;
    FallAnim.StopValue:= FinalY;
    FallAnim.Duration:= Duration;
    FallAnim.AnimationType:= TAnimationType.In;
    FallAnim.Interpolation:= TInterpolationType.Quadratic;

    // Horizontal drift animation (slight wave motion)
    DriftAnim:= TFloatAnimation.Create(Confetti);
    DriftAnim.Parent:= Confetti;
    DriftAnim.PropertyName:= 'Position.X';
    DriftAnim.StartValue:= StartX;
    DriftAnim.StopValue:= StartX + (Random(100) - 50);  // Drift -50 to +50
    DriftAnim.Duration:= Duration;
    DriftAnim.AutoReverse:= TRUE;
    DriftAnim.Loop:= TRUE;

    // Rotation animation
    RotateAnim:= TFloatAnimation.Create(Confetti);
    RotateAnim.Parent:= Confetti;
    RotateAnim.PropertyName:= 'RotationAngle';
    RotateAnim.StartValue:= Confetti.RotationAngle;
    RotateAnim.StopValue:= Confetti.RotationAngle + 360 + Random(720);
    RotateAnim.Duration:= Duration;

    // Fade out animation (starts at 60% of duration)
    FadeAnim:= TFloatAnimation.Create(Confetti);
    FadeAnim.Parent:= Confetti;
    FadeAnim.PropertyName:= 'Opacity';
    FadeAnim.StartValue:= 0.9;
    FadeAnim.StopValue:= 0;
    FadeAnim.Delay:= Duration * 0.6;
    FadeAnim.Duration:= Duration * 0.4;

    // Clean up confetti when fall animation finishes
    FallAnim.OnFinish:= TConfetti.OnAnimFinish;

    // Start all animations
    FallAnim.Start;
    DriftAnim.Start;
    RotateAnim.Start;
    FadeAnim.Start;
  end;
end;


end.
