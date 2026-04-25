UNIT LightFmx.Visual.Animations;

{=============================================================================================================
   2026.04
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   FMX micro-animations.

   Classes:
     TConfetti       - falling confetti celebration (rectangles drop + drift + spin + fade)
     TBubbleAnim     - chat bubble fade-in + StopAnimationsIn helper
     TBtnAnim        - button press squish (Scale 0.92 -> 1.0, Back interpolation)
     TTypingDots     - three pulsing dots "AI is thinking" indicator
     TExitAnim       - cascade UI elements off-screen on form close (drops TreeView items individually)
     TAnimDoneBridge - internal helper to route anonymous TProc into TFloatAnimation.OnFinish

   All helpers are short (150-400ms for entry, ~550ms for exit drop), non-blocking,
   self-cleaning via parent-ownership. Pure FMX.Ani, no Skia dependency.

   Friend-class hack inside implementation: TControlAccess = class(TControl)
     TControl's Scale / RotationCenter are PROTECTED; generic code that touches
     arbitrary TControls uses this cast to reach them.

   Known gotchas (see also CLAUDE.md files of dependent projects):
     * Never animate Scale.X/Y on an auto-resizing text bubble inside a scrollbox
       (realign cascade crashes on freed FTextLabel).
     * Never parent a runtime helper (typing dots, overlay) to a container that
       later receives DeleteChildren without FreeNotification.
     * Animations on FMX native-presentation controls (TEdit, TMemo,
       TAutoSizeBoxText) must be stopped BEFORE the control enters BeforeDestruction;
       otherwise a property write during TPresentedTextControl.FreeStyle crashes.
     * TConfetti/TExitAnim use Random; caller must have Randomize called once at startup.

   Example usage:
     TConfetti.ShowConfetti(MyForm, 1.0, 30);
     TBubbleAnim.SlideIn(MyBubble, FALSE);
     TBtnAnim.Squish(MyButton);
     FDots := TTypingDots.Create(MyForm, MyForm.Container, 20, 300);
     TExitAnim.FallDown(MainLayout, Self, procedure begin Close end);
=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Layouts, FMX.Objects;

TYPE
  { Falling confetti. Each piece: fall (Y quadratic In) + drift (X auto-reverse loop)
    + rotation + fade-out over last 40% of duration. Auto-cleans on finish. }
  TConfetti = class
    class procedure OnAnimFinish(Sender: TObject);
    class procedure ShowConfetti(Parent: TFmxObject; SizeMultiplier: Single; ConfettiCount: Integer);    { Parent can be TForm or TControl. }
  end;


  { Bubble entry: Opacity-only fade (0 -> 1, 220 ms). Layout-neutral.
    FromRight kept for API compatibility; currently unused.

    IMPORTANT: a TFloatAnimation that is still ticking when its parent enters
    BeforeDestruction crashes inside TPresentedTextControl.FreeStyle. Before
    calling DeleteChildren on a container that holds animated bubbles, call
    TBubbleAnim.StopAnimationsIn(Container). }
  TBubbleAnim = class
    class procedure SlideIn(aBubble: TControl; FromRight: Boolean);
    class procedure StopAnimationsIn(Root: TFmxObject);
  end;


  { Button press feel: quick shrink + bounce back (200 ms, Back interpolation).
    Animations owned by the button; safe to call repeatedly. }
  TBtnAnim = class
    class procedure Squish(aBtn: TControl);
  end;


  { Three pulsing dots shown while waiting for an AI response.
    NOTE: animations only advance when the main thread is idle. If the caller
    blocks the UI thread during the AI call, the dots will freeze until control
    returns to the message loop. Convert AI call to async for full effect. }
  TTypingDots = class(TComponent)
  strict private
    FLayout: TLayout;
    procedure MakeDot(Index: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent; AParent: TFmxObject; X, Y: Single); reintroduce;
    destructor  Destroy; override;
    procedure Hide;
    property  Layout: TLayout read FLayout;
  end;


  { Internal helper: holds an anonymous OnDone callback and exposes it as a TNotifyEvent
    so it can be wired to TFloatAnimation.OnFinish (which is of-object). }
  TAnimDoneBridge = class(TComponent)
  strict private
    FOnDone: TProc;
  public
    constructor Create(AOwner: TComponent; ADone: TProc); reintroduce;
    procedure Fire(Sender: TObject);
  end;


  { Exit drop-out: cascade individual UI elements off the bottom edge, then fire OnDone.

    Rather than dropping aContainer as one big block (which hides per-element motion),
    we iterate aContainer's direct children. For any TTreeView encountered, we dive
    into its items so each category/lesson row falls separately. Each target gets a
    staggered Position.Y + RotationAngle animation; OnDone fires after the last one.

    Use from an OnCloseQuery handler: set CanClose=FALSE, trigger FallDown, and inside
    OnDone call Close to actually dismiss the form. Skip on mobile. }
  TExitAnim = class
    class procedure FallDown(aContainer: TControl; OwnerForm: TCommonCustomForm; OnDone: TProc);
  end;


IMPLEMENTATION

USES
  System.UITypes, System.Types, System.Math, System.Generics.Collections,
  FMX.Ani, FMX.Graphics;


{ Friend-class access: TControl's Scale / RotationCenter are PROTECTED on the
  base class but made published by subclasses (TRectangle, TButton, TCircle, ...).
  Going through a descendant alias lets us touch them generically. }
TYPE
  TControlAccess = class(TControl);


CONST
  ConfettiColors: array[0..6] of TAlphaColor = (
    TAlphaColorRec.Gold,
    TAlphaColorRec.Crimson,
    TAlphaColorRec.Dodgerblue,
    TAlphaColorRec.Lime,
    TAlphaColorRec.Magenta,
    TAlphaColorRec.Orange,
    TAlphaColorRec.Cyan);



{=============================================================================================================
   CONFETTI
=============================================================================================================}

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



{=============================================================================================================
   BUBBLE SLIDE-IN
=============================================================================================================}

class procedure TBubbleAnim.SlideIn(aBubble: TControl; FromRight: Boolean);
CONST
  Dur = 0.22;
VAR
  Fade: TFloatAnimation;
begin
  if aBubble = NIL then EXIT;

  TControlAccess(aBubble).Opacity:= 0;

  Fade:= TFloatAnimation.Create(aBubble);
  Fade.Parent       := aBubble;
  Fade.PropertyName := 'Opacity';
  Fade.StartValue   := 0;
  Fade.StopValue    := 1;
  Fade.Duration     := Dur;
  Fade.Start;
end;


{ Walk Root's children and disable any TAnimation found. Call BEFORE freeing
  a subtree that contains animated children. A still-ticking TFloatAnimation
  that fires while a control is in BeforeDestruction crashes FMX presentation. }
class procedure TBubbleAnim.StopAnimationsIn(Root: TFmxObject);
VAR i: Integer;
begin
  if Root = NIL then EXIT;
  for i:= 0 to Root.ChildrenCount - 1 do
    begin
      if Root.Children[i] is TAnimation
      then TAnimation(Root.Children[i]).Enabled:= FALSE;

      // Recurse: some controls wrap their animations deeper in the tree.
      StopAnimationsIn(Root.Children[i]);
    end;
end;



{=============================================================================================================
   BUTTON SQUISH
=============================================================================================================}

class procedure TBtnAnim.Squish(aBtn: TControl);
CONST
  Dur = 0.2;
VAR
  SX, SY: TFloatAnimation;
  Acc   : TControlAccess;
begin
  if aBtn = NIL then EXIT;

  Acc:= TControlAccess(aBtn);
  Acc.RotationCenter.X:= 0.5;
  Acc.RotationCenter.Y:= 0.5;
  Acc.Scale.X:= 0.92;
  Acc.Scale.Y:= 0.92;

  SX:= TFloatAnimation.Create(aBtn);
  SX.Parent       := aBtn;
  SX.PropertyName := 'Scale.X';
  SX.StartValue   := 0.92;
  SX.StopValue    := 1.0;
  SX.Duration     := Dur;
  SX.AnimationType:= TAnimationType.Out;
  SX.Interpolation:= TInterpolationType.Back;

  SY:= TFloatAnimation.Create(aBtn);
  SY.Parent       := aBtn;
  SY.PropertyName := 'Scale.Y';
  SY.StartValue   := 0.92;
  SY.StopValue    := 1.0;
  SY.Duration     := Dur;
  SY.AnimationType:= TAnimationType.Out;
  SY.Interpolation:= TInterpolationType.Back;

  SX.Start;
  SY.Start;
end;



{=============================================================================================================
   TYPING DOTS
=============================================================================================================}

constructor TTypingDots.Create(AOwner: TComponent; AParent: TFmxObject; X, Y: Single);
VAR i: Integer;
begin
  inherited Create(AOwner);
  FLayout:= TLayout.Create(Self);           // OWNED BY SELF, not by AParent chain
  FLayout.Stored:= FALSE;
  FLayout.FreeNotification(Self);           // null FLayout if FMX frees it under us (DeleteChildren)
  FLayout.Parent:= AParent;
  FLayout.Position.X:= X;
  FLayout.Position.Y:= Y;
  FLayout.Width:= 60;
  FLayout.Height:= 20;
  FLayout.HitTest:= FALSE;
  for i:= 0 to 2 do MakeDot(i);
end;


destructor TTypingDots.Destroy;
begin
  if Assigned(FLayout) then
    begin
      // Stop the looping dot animations BEFORE freeing. Otherwise a still-ticking
      // TFloatAnimation fires a property write on a TCircle in BeforeDestruction,
      // crashing inside TStyledControl.InternalFreeStyle. Same crash class as bubbles.
      TBubbleAnim.StopAnimationsIn(FLayout);
      FLayout.RemoveFreeNotification(Self);
      FreeAndNil(FLayout);
    end;
  inherited;
end;


procedure TTypingDots.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  // If our layout got freed externally (e.g. ClearBubbles -> DeleteChildren),
  // drop the reference so our own destructor won't double-free.
  if (Operation = opRemove)
  AND (AComponent = FLayout)
  then FLayout:= NIL;
end;


procedure TTypingDots.Hide;
begin
  if Assigned(FLayout) then FLayout.Visible:= FALSE;
end;


procedure TTypingDots.MakeDot(Index: Integer);
VAR
  Dot : TCircle;
  Anim: TFloatAnimation;
begin
  Dot:= TCircle.Create(FLayout);
  Dot.Parent       := FLayout;
  Dot.Width        := 10;
  Dot.Height       := 10;
  Dot.Position.X   := Index * 18;
  Dot.Position.Y   := 5;
  Dot.Fill.Color   := $FF808080;
  Dot.Stroke.Kind  := TBrushKind.None;
  Dot.HitTest      := FALSE;

  Anim:= TFloatAnimation.Create(Dot);
  Anim.Parent       := Dot;
  Anim.PropertyName := 'Opacity';
  Anim.StartValue   := 0.3;
  Anim.StopValue    := 1.0;
  Anim.Duration     := 0.5;
  Anim.Delay        := Index * 0.15;
  Anim.AutoReverse  := TRUE;
  Anim.Loop         := TRUE;
  Anim.Start;
end;



{=============================================================================================================
   BRIDGE (anonymous -> of-object)
=============================================================================================================}

constructor TAnimDoneBridge.Create(AOwner: TComponent; ADone: TProc);
begin
  inherited Create(AOwner);
  FOnDone:= ADone;
end;


{ Fire() is called by FMX.Ani from inside TAnimation.DoFinish, which itself runs
  inside TAnimationManager's animation-processing loop. We're therefore ON the
  main-thread call stack of the animation manager when this method executes.

  WHY Done() MUST BE DEFERRED (do not make this synchronous — see crash below):

    Typical caller: TExitAnim.FallDown. Done() closes the form, which triggers
    form destruction, which destroys all the form's children — including every
    TControl that hosted a fall-down animation, and the animations themselves
    (they are owned components of those controls).

    If Done() runs synchronously here:
      1. Animation.OnFinish fires -> we land in Fire() -> call Done().
      2. Done() -> Close -> CloseQuery (allowed by FExitAnimating guard)
         -> form.Destroy -> recursive child destruction.
      3. One of the destroyed children is the control whose animation we're
         still inside the OnFinish of. That control's OwnedComponents include
         the animation itself -> TAnimation.Destroy runs -> BeforeDestruction.
      4. TFmxObject.BeforeDestruction iterates FFreeNotifies (offset $5C in
         the 32-bit layout). FMX's AnimationManager is registered there to
         be notified when observed controls die.
      5. Execution eventually returns up the stack to TAnimationManager, which
         still holds a pointer to the just-freed animation in its running list.
         FastMM has filled that block with $80 -> dereferencing its VMT reads
         from $80808080, the virtual call hits $8080808C, and we AV inside
         TFmxObject.BeforeDestruction (the classic "read of address 0x8080808c"
         crash on shutdown).

    Observed symptom: project raised $C0000005 with 'access violation at
    <anim_unit_addr>: read of address 0x8080808c' inside TFmxObject.BeforeDestruction,
    callstack = TFmxObject.BeforeDestruction -> @BeforeDestruction -> TObject.Free.

  FIX: post Done() to the main-thread message queue via TThread.ForceQueue. The
  current OnFinish/manager-loop call stack unwinds fully first; AnimationManager
  finishes processing the running list and drops its pointer to this animation.
  Only then does the queued closure run, close the form, and cascade destruction.
  No stale pointers, no AV.

  WHY NOT FREE THE BRIDGE: Bridge is owned by OwnerForm (TComponent ownership);
  it dies with the form. Freeing it here, or queuing a deferred free, causes a
  use-after-free at finalization time because OwnerForm's component-list walk
  would then touch a gone-early child. }
procedure TAnimDoneBridge.Fire(Sender: TObject);
VAR Done: TProc;
begin
  Done:= FOnDone;
  FOnDone:= NIL;                    // prevent double-invocation if Fire() is reached twice
  if Assigned(Sender)
  AND (Sender is TAnimation)
  then TAnimation(Sender).OnFinish:= NIL;   // detach so Bridge isn't reused by the same animation

  if Assigned(Done)
  then TThread.ForceQueue(NIL, procedure
       begin
         Done();
       end);
end;



{=============================================================================================================
   EXIT FALL-DOWN

   HISTORICAL BUG (FIXED) — READ BEFORE MODIFYING THIS SECTION
   ---------------------------------------------------------------------------------------------------------
   Symptom: AV $C0000005 at shutdown, 'read of address 0x8080808c', inside TFmxObject.BeforeDestruction.
            0x80808080 is FastMM's freed-block fill pattern; +$0C = offset into a vtable that no longer
            exists because the object was freed. The crashing frame iterates a list at struct offset $5C
            on TFmxObject (FFreeNotifies) and calls a virtual method on each entry.

   Trigger: TExitAnim.FallDown used to reparent cascade targets (buttons, layouts, TreeViewItems) OUT of
            their original parents and INTO OwnerForm. The goal was to escape the original parent's clip
            rect so controls could fall past the bottom edge of a scrollbox.

   Root cause: reparenting breaks mutual FreeNotify relationships between siblings. Most dangerously
               TTreeView <-> TTreeViewItem: TreeView items register FreeNotify with their TreeView (and
               vice versa via internal FMX machinery). After FallDown:
                  - TreeView becomes a direct child of OwnerForm.
                  - Each TreeViewItem also becomes a direct child of OwnerForm.
                  - Form.FChildren now lists them as siblings, in some order.

               At program shutdown (NOT at CloseQuery!):
                  DoneApplication -> TComponent.DestroyComponents -> TLightForm.Destroy ->
                  TCustomForm.Destroy -> TFmxObject.DoDeleteChildren walks form's FChildren list
                  freeing each in order. Whichever sibling dies first leaves a dangling pointer in
                  the other's FFreeNotifies. When the survivor is freed, its BeforeDestruction
                  iterates FFreeNotifies and dereferences the freed entry -> AV on vmtBeforeDestruction.

               madExcept report for the definitive crash showed:
                  TFmxObject.BeforeDestruction +$BC  (inside FFreeNotifies iteration)
                  @BeforeDestruction
                  TControl.Destroy      <-- Y (freed sibling inside FFreeNotifies)
                  TFmxObject.DoDeleteChildren
                  TControl.Destroy      <-- X (X frees Y as its child)
                  TFmxObject.DoDeleteChildren
                  TCustomForm.Destroy -> TLightForm.Destroy
                  TComponent.DestroyComponents
                  DoneApplication -> @Halt0 -> LearnAssist.dpr initialization

   Fix attempts (chronological):
     [1] 2026-04-21 — ForceQueue Done() in TAnimDoneBridge.Fire.
         Addressed reentrancy of Close inside TAnimation.OnFinish.
         Result: did not eliminate crash. Stack unchanged. Kept anyway —
         still-correct on its own merits (defers destruction out of the
         animation-manager callback).
     [2] 2026-04-21 — Treat TTreeView as a single leaf in CollectFallTargets
         (stop diving into TTreeViewItems).
         Rationale: TTreeView/TTreeViewItem hold documented mutual FreeNotify
         links — reparenting items out breaks destruction.
         Result: did not eliminate crash on its own, but still correct: other
         sibling FreeNotify links exist (TMultiView, TSvgButton internals,
         styled controls) so TTreeView-only handling was insufficient.
     [3] 2026-04-21 — Remove child reparenting entirely. Animate in place.
         Rationale: any reparenting flattens form.FChildren and breaks
         destruction order for FreeNotify-linked siblings we cannot enumerate
         up front. LearnAssist's main-form layouts have ClipChildren=False,
         so in-place fall is visually acceptable.
         Result: FIX CONFIRMED by user on 2026-04-21. Shutdown AV is gone.
         Current state of the code.

   DO NOT, under any circumstances:
     * Reintroduce Child.Parent := OwnerForm in FallDown without a paired
       RestoreParents pass that runs BEFORE Close. Any such change revives
       the shutdown AV at $8080808C.
     * Re-add TTreeView item diving (CollectTreeItems) without the same
       restore-before-close safeguard.
=============================================================================================================}

{ Builds the list of controls that will physically fall.
  Rules:
    * TLayout -> transparent; descend into its children (layouts have no visual).
    * anything else visible -> taken as a leaf and added as-is.

  DO NOT dive into TTreeView to collect its items. TTreeView and TTreeViewItem
  hold mutual FreeNotify relationships; reparenting items out of their TreeView
  breaks destruction ordering at program finalization and triggers an AV at
  $8080808C inside TFmxObject.BeforeDestruction (freed sibling in FFreeNotifies).
  Treat TTreeView as a single leaf — it falls as one unit. Other direct children
  (buttons, panels, labels) still cascade individually. }
procedure CollectFallTargets(aRoot: TFmxObject; aList: TList<TControl>);
VAR
  i    : Integer;
  Child: TControl;
begin
  if aRoot = NIL then EXIT;
  for i:= 0 to aRoot.ChildrenCount - 1 do
    if aRoot.Children[i] is TControl then
      begin
        Child:= TControl(aRoot.Children[i]);
        if NOT Child.Visible then Continue;

        if Child.ClassType = TLayout
        then CollectFallTargets(Child, aList)   // transparent: descend
        else aList.Add(Child);
      end;
end;


class procedure TExitAnim.FallDown(aContainer: TControl; OwnerForm: TCommonCustomForm; OnDone: TProc);
CONST
  Dur     = 0.55;
  Stagger = 0.045;   // per-item delay — total cascade stays under ~1s for ~15 items
VAR
  Targets     : TList<TControl>;
  i           : Integer;
  Child       : TControl;
  TargetY     : Single;
  MaxDelayIdx : Integer;
  YAnim       : TFloatAnimation;
  RotAnim     : TFloatAnimation;
  Bridge      : TAnimDoneBridge;
begin
  if (aContainer = NIL) OR (OwnerForm = NIL) then
    begin
      if Assigned(OnDone) then OnDone();
      EXIT;
    end;

  Targets:= TList<TControl>.Create;
  TRY
    CollectFallTargets(aContainer, Targets);

    // No targets found — fire OnDone immediately so the caller still closes.
    if Targets.Count = 0 then
      begin
        if Assigned(OnDone) then OnDone();
        EXIT;
      end;

    MaxDelayIdx := Targets.Count - 1;  // last in list has highest stagger delay

    for i:= 0 to Targets.Count - 1 do
      begin
        Child:= Targets[i];

        // DO NOT reparent. Reparenting targets into OwnerForm creates a flat sibling structure under the form; during program finalization
        // (DoDeleteChildren cascade inside DoneApplication), controls that held mutual FreeNotify links with their original parent/sibling subtree die in the wrong order, leaving stale pointers in FFreeNotifies that crash
        // TFmxObject.BeforeDestruction at $8080808C. Staying in-place accepts that controls inside a clipping parent may vanish early during the fall, but avoids the shutdown AV. LearnAssist's main-form layouts
        // do not clip children (ClipChildren=False by default), so the visual impact is negligible here.

        // Compute target: fall a full client-height below current position, so regardless of parent offset the control visibly drops past the bottom.
        TargetY:= Child.Position.Y + OwnerForm.ClientHeight + 50;

        // Break alignment so Position.Y animation is visible. Set rotation pivot to center.
        Child.Align:= TAlignLayout.None;
        TControlAccess(Child).RotationCenter.X:= 0.5;
        TControlAccess(Child).RotationCenter.Y:= 0.5;

        YAnim:= TFloatAnimation.Create(Child);
        YAnim.Parent       := Child;
        YAnim.PropertyName := 'Position.Y';
        YAnim.StartValue   := Child.Position.Y;
        YAnim.StopValue    := TargetY;
        YAnim.Duration     := Dur;
        YAnim.Delay        := i * Stagger;
        YAnim.AnimationType:= TAnimationType.In;
        YAnim.Interpolation:= TInterpolationType.Quadratic;

        RotAnim:= TFloatAnimation.Create(Child);
        RotAnim.Parent       := Child;
        RotAnim.PropertyName := 'RotationAngle';
        RotAnim.StartValue   := 0;
        RotAnim.StopValue    := Random(24) - 12;   // -12..+12 degrees
        RotAnim.Duration     := Dur;
        RotAnim.Delay        := i * Stagger;
        RotAnim.AnimationType:= TAnimationType.In;
        RotAnim.Interpolation:= TInterpolationType.Quadratic;

        // Hook OnDone to the last-starting Y animation (largest delay).
        if i = MaxDelayIdx then
          begin
            Bridge:= TAnimDoneBridge.Create(OwnerForm, OnDone);
            YAnim.OnFinish:= Bridge.Fire;
          end;

        YAnim.Start;
        RotAnim.Start;
      end;
  FINALLY
    Targets.Free;
  END;
end;


end.
