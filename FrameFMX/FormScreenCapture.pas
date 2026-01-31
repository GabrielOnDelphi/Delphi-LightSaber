UNIT FormScreenCapture;


{=============================================================================================================
   2026.01.31
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Screen Capture with Rectangle Selection


   Cross-platform screenshot capture allowing user to select rectangular areas.
   Features glossy overlay effect for non-selected areas.
=============================================================================================================}

{_done
  1. Added DrawGlossyOverlay procedure that creates a multi-layered glossy effect:
    - Top-down gradient: Simulates light coming from above with a white highlight at the top fading to transparent
    - Horizontal light streaks: 3 subtle horizontal bands simulating glass reflections
    - Vignette effect: Radial gradient that darkens the edges for depth
    - Cool tint: Subtle RoyalBlue overlay for a modern look
  2. Updated pbOverlayPaint:
    - Increased base dimming opacity from 0.3 to 0.35 for better visibility
    - Calls DrawGlossyOverlay after drawing the dimmed screenshot
    - Added selection glow effect: White inner border + blue outer glow around the selected area

  Visual effect breakdown:
  - The covered (non-selected) areas now have a polished, glass-like appearance
  - Light appears to come from the top of the screen
  - Edges are subtly darkened (vignette) for depth
  - The selected area "pops" with a glowing border effect

  ┌───────────┬────────────┬─────────────┬───────────────────────────────────────┐
  │ Location  │ Parameter  │   Current   │                Effect                 │
  ├───────────┼────────────┼─────────────┼───────────────────────────────────────┤
  │ Line ~203 │ BlurAmount │ 1.5         │ Blur intensity (was 2.5, now reduced) │
  ├───────────┼────────────┼─────────────┼───────────────────────────────────────┤
  │ Line ~225 │ Opacity    │ 0.92        │ How visible the blurred background is │
  ├───────────┼────────────┼─────────────┼───────────────────────────────────────┤
  │ Line ~233 │ $30000000  │ ~19% black  │ Dark overlay to dim background        │
  ├───────────┼────────────┼─────────────┼───────────────────────────────────────┤
  │ Line ~245 │ $40FFFFFF  │ ~25% white  │ Top shine brightness                  │
  ├───────────┼────────────┼─────────────┼───────────────────────────────────────┤
  │ Line ~267 │ $0C87CEEB  │ ~5% SkyBlue │ Color tint (Aero signature)           │
  ├───────────┼────────────┼─────────────┼───────────────────────────────────────┤
  │ Line ~287 │ $25000000  │ ~15% black  │ Edge darkness (vignette)              │
  └───────────┴────────────┴─────────────┴───────────────────────────────────────┘
  Quick adjustments:
  - Less blur: Change 1.5 to 1.0 or 0.5
  - No color tint: Change $0C87CEEB to $00000000
  - No vignette: Change $25000000 to $00000000
  - Stronger top shine: Change $40FFFFFF to $60FFFFFF   }

INTERFACE

USES
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Filter,
  LightFmx.Visual.ScreenCapture;

TYPE
  TOverlayStyle = (osFrostedGlass, osGlossy, osSimpleDim);

  TfrmScreenCapture = class(TForm)
    layMain: TLayout;
    imgScreenshot: TImageControl;
    pbOverlay: TPaintBox;
    SelectionRect: TSelection;
    layInstructions: TLayout;
    lblInstructions: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure pbOverlayPaint(Sender: TObject; Canvas: TCanvas);
    procedure SelectionRectChange(Sender: TObject);
  private
    FSCManager: TScreenCaptureManager;
    FBlurredScreenshot: TBitmap;
    FOverlayStyle: TOverlayStyle;
    procedure UpdateInstructions;
    function  IsCaptureKeyPressed(Key: Word; Shift: TShiftState): Boolean;
    procedure ShowCaptureFlash;
    procedure CreateBlurredScreenshot;
    procedure DrawFrostedGlassOverlay(Canvas: TCanvas; const FullRect: TRectF);
    procedure DrawGlossyOverlay(Canvas: TCanvas; const FullRect: TRectF);
  public
    destructor Destroy; override;
    procedure StartCapture;
    property OverlayStyle: TOverlayStyle read FOverlayStyle write FOverlayStyle;
  end;


procedure ScreenCaptureNow(OnComplete: TScreenCaptureCallback);


IMPLEMENTATION {$R *.fmx}

USES
  {$IFDEF MSWINDOWS} Winapi.Windows{$ENDIF}
  {$IFDEF MACOS}, Macapi.AppKit {$ENDIF};






{-------------------------------------------------------------------------------------------------------------
   Utils
-------------------------------------------------------------------------------------------------------------}
procedure ScreenCaptureNow(OnComplete: TScreenCaptureCallback);
begin
  VAR FOverlay:= TfrmScreenCapture.Create(nil);
  try
    FOverlay.StartCapture;
    FOverlay.ShowModal;

    if Assigned(OnComplete)
    then OnComplete(FOverlay.FSCManager.GetCapturedImages);
  finally
    FOverlay.Free;
  end;
end;




{-------------------------------------------------------------------------------------------------------------
   Form
-------------------------------------------------------------------------------------------------------------}

procedure TfrmScreenCapture.FormCreate(Sender: TObject);
begin
  FSCManager:= TScreenCaptureManager.Create;
  FBlurredScreenshot:= NIL;
  FOverlayStyle:= osFrostedGlass;  // Default to frosted glass
  layInstructions.Visible:= FSCManager.CaptureTipShown < 3;
end;


procedure TfrmScreenCapture.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Don't free FSCManager here - it's needed for the callback after ShowModal returns
  // FSCManager will be freed when the form is destroyed
  Action:= TCloseAction.caHide;   // Don't set caFree - form is manually freed by ScreenCaptureNow
end;


destructor TfrmScreenCapture.Destroy;
begin
  FreeAndNil(FBlurredScreenshot);
  FreeAndNil(FSCManager);
  inherited;
end;


procedure TfrmScreenCapture.StartCapture;
VAR
  LastRect: TRectF;
begin
  // Hide form temporarily to capture clean screenshot
  // Note: ProcessMessages is used here intentionally to ensure the form is fully hidden
  // before taking the screenshot. Async alternatives would complicate the flow.
  Hide;
  Application.ProcessMessages;
  Sleep(100);

  // Capture screen
  FSCManager.StartCapture;

  // Validate screenshot was captured
  if NOT Assigned(FSCManager.Screenshot) OR FSCManager.Screenshot.IsEmpty
  then EXIT;

  // Display screenshot in image control (hidden, used only as source for painting)
  imgScreenshot.Bitmap.Assign(FSCManager.Screenshot);

  // Create blurred version for frosted glass effect
  if FOverlayStyle = osFrostedGlass
  then CreateBlurredScreenshot;

  // Set last selection if available and within current screen bounds
  LastRect:= FSCManager.LastSelectionRect;
  if NOT LastRect.IsEmpty then
    if (LastRect.Right <= FSCManager.Screenshot.Width) AND
       (LastRect.Bottom <= FSCManager.Screenshot.Height)
    then SelectionRect.BoundsRect:= LastRect;

  // Show form
  Show;
  UpdateInstructions;
end;


// Create a blurred copy of the screenshot for frosted glass effect
procedure TfrmScreenCapture.CreateBlurredScreenshot;
var
  Filter: TFilter;
begin
  FreeAndNil(FBlurredScreenshot);
  FBlurredScreenshot := FMX.Graphics.TBitmap.Create(0, 0);

  Filter := TFilterManager.FilterByName('GaussianBlur');
  if Assigned(Filter) then
  begin
    Filter.ValuesAsBitmap['Input'] := FSCManager.Screenshot;
    // ═══════════════════════════════════════════════════════════════════════
    // BLUR INTENSITY: Adjust this value to control blur strength
    //   0.5 = very subtle blur (almost sharp)
    //   1.0 = light blur
    //   1.5 = moderate blur (recommended for subtle frosted glass)
    //   2.5 = strong blur (original Vista/7 style)
    //   4.0 = very strong blur
    // ═══════════════════════════════════════════════════════════════════════
    Filter.ValuesAsFloat['BlurAmount'] := 1.5;
    FBlurredScreenshot.Assign(Filter.ValuesAsBitmap['Output']);
  end
  else
    // Fallback if filter not available
    FBlurredScreenshot.Assign(FSCManager.Screenshot);
end;


// Draw frosted glass overlay effect (Windows Vista/7 Aero style)
procedure TfrmScreenCapture.DrawFrostedGlassOverlay(Canvas: TCanvas; const FullRect: TRectF);
var
  GradientBrush: TBrush;
  GradientPoint: TGradientPoint;
begin
  // ═══════════════════════════════════════════════════════════════════════════
  // FROSTED GLASS TUNING PARAMETERS
  // ═══════════════════════════════════════════════════════════════════════════

  // 1. Draw blurred screenshot as base (the key frosted glass element)
  //    OPACITY: 0.0=invisible, 1.0=fully visible. Lower = more faded/dimmed
  if Assigned(FBlurredScreenshot) then
    Canvas.DrawBitmap(FBlurredScreenshot, FBlurredScreenshot.BoundsF, FullRect, 0.92, True)
  else
    Canvas.DrawBitmap(FSCManager.Screenshot, FSCManager.Screenshot.BoundsF, FullRect, 0.4, True);

  // 2. Dark tint overlay - dims the background
  //    Format: $AARRGGBB where AA=alpha (00-FF), higher alpha = darker
  //    $20000000 = very subtle, $30000000 = subtle, $50000000 = noticeable
  Canvas.Fill.Kind := TBrushKind.Solid;
  Canvas.Fill.Color := $30000000;
  Canvas.FillRect(FullRect, 0, 0, [], 1.0);

  // 3. Top highlight reflection (characteristic Aero shine)
  //    TOP SHINE: $40FFFFFF = ~25% white at top, fades down
  //    Set to $00FFFFFF to disable shine entirely
  GradientBrush := TBrush.Create(TBrushKind.Gradient, TAlphaColorRec.White);
  try
    GradientBrush.Gradient.Style := TGradientStyle.Linear;
    GradientBrush.Gradient.Points.Clear;

    GradientPoint := GradientBrush.Gradient.Points.Add as TGradientPoint;
    GradientPoint.Color := $40FFFFFF;  // TOP EDGE: increase alpha (40->60) for brighter shine
    GradientPoint.Offset := 0;

    GradientPoint := GradientBrush.Gradient.Points.Add as TGradientPoint;
    GradientPoint.Color := $10FFFFFF;
    GradientPoint.Offset := 0.15;      // How far down the shine extends (0.15 = 15% of height)

    GradientPoint := GradientBrush.Gradient.Points.Add as TGradientPoint;
    GradientPoint.Color := $00FFFFFF;
    GradientPoint.Offset := 0.4;

    Canvas.Fill.Assign(GradientBrush);
    Canvas.FillRect(FullRect, 0, 0, [], 1.0);
  finally
    GradientBrush.Free;
  end;

  // 4. Color tint (Aero's signature blue color)
  //    TINT COLOR: $0C87CEEB = SkyBlue at ~5% alpha
  //    Change color: $0C4169E1 = RoyalBlue, $0C00CED1 = DarkTurquoise
  //    Set to $00000000 to disable tint
  Canvas.Fill.Kind := TBrushKind.Solid;
  Canvas.Fill.Color := $0C87CEEB;
  Canvas.FillRect(FullRect, 0, 0, [], 1.0);

  // 5. Vignette effect (darker at edges for depth)
  //    EDGE DARKNESS: $25000000 = ~15% black at edges
  //    Set to $00000000 to disable vignette
  GradientBrush := TBrush.Create(TBrushKind.Gradient, TAlphaColorRec.Black);
  try
    GradientBrush.Gradient.Style := TGradientStyle.Radial;
    GradientBrush.Gradient.Points.Clear;

    GradientPoint := GradientBrush.Gradient.Points.Add as TGradientPoint;
    GradientPoint.Color := $00000000;  // Center: transparent
    GradientPoint.Offset := 0;

    GradientPoint := GradientBrush.Gradient.Points.Add as TGradientPoint;
    GradientPoint.Color := $00000000;
    GradientPoint.Offset := 0.6;       // Vignette starts at 60% from center

    GradientPoint := GradientBrush.Gradient.Points.Add as TGradientPoint;
    GradientPoint.Color := $25000000;  // EDGE DARKNESS: increase (25->40) for darker edges
    GradientPoint.Offset := 1.0;

    Canvas.Fill.Assign(GradientBrush);
    Canvas.FillEllipse(
      TRectF.Create(
        FullRect.Left - FullRect.Width * 0.1,
        FullRect.Top - FullRect.Height * 0.1,
        FullRect.Right + FullRect.Width * 0.1,
        FullRect.Bottom + FullRect.Height * 0.1
      ),
      1.0
    );
  finally
    GradientBrush.Free;
  end;
end;


// Draw glossy overlay effect on non-selected areas
procedure TfrmScreenCapture.DrawGlossyOverlay(Canvas: TCanvas; const FullRect: TRectF);
VAR
  GradientBrush: TBrush;
  GradientPoint: TGradientPoint;
  i: Integer;
  StripHeight: Single;
  StripRect: TRectF;
  AlphaValue: Single;
begin
  // 1. Top-down gradient (simulates light from above)
  GradientBrush := TBrush.Create(TBrushKind.Gradient, TAlphaColorRec.White);
  try
    GradientBrush.Gradient.Style := TGradientStyle.Linear;
    GradientBrush.Gradient.Points.Clear;

    // Top highlight (white with ~12% alpha)
    GradientPoint := GradientBrush.Gradient.Points.Add as TGradientPoint;
    GradientPoint.Color := $1FFFFFFF;
    GradientPoint.Offset := 0;

    // Middle fade (white with ~2% alpha)
    GradientPoint := GradientBrush.Gradient.Points.Add as TGradientPoint;
    GradientPoint.Color := $05FFFFFF;
    GradientPoint.Offset := 0.4;

    // Bottom (transparent)
    GradientPoint := GradientBrush.Gradient.Points.Add as TGradientPoint;
    GradientPoint.Color := $00FFFFFF;
    GradientPoint.Offset := 1.0;

    Canvas.Fill.Assign(GradientBrush);
    Canvas.FillRect(FullRect, 0, 0, [], 1.0);
  finally
    GradientBrush.Free;
  end;

  // 2. Subtle horizontal light streaks (glass reflection effect)
  Canvas.Fill.Kind := TBrushKind.Solid;
  for i := 0 to 2 do
  begin
    AlphaValue := 0.025 - (i * 0.007);
    if AlphaValue > 0 then
    begin
      StripHeight := FullRect.Height * 0.06;
      StripRect := TRectF.Create(
        FullRect.Left,
        FullRect.Top + (FullRect.Height * 0.08) + (i * StripHeight * 3),
        FullRect.Right,
        FullRect.Top + (FullRect.Height * 0.08) + (i * StripHeight * 3) + StripHeight
      );
      // White with calculated alpha
      Canvas.Fill.Color := (Round(AlphaValue * 255) shl 24) or $00FFFFFF;
      Canvas.FillRect(StripRect, 0, 0, [], 1.0);
    end;
  end;

  // 3. Vignette effect (darker edges for depth)
  GradientBrush := TBrush.Create(TBrushKind.Gradient, TAlphaColorRec.Black);
  try
    GradientBrush.Gradient.Style := TGradientStyle.Radial;
    GradientBrush.Gradient.Points.Clear;

    // Center (transparent black)
    GradientPoint := GradientBrush.Gradient.Points.Add as TGradientPoint;
    GradientPoint.Color := $00000000;
    GradientPoint.Offset := 0;

    // Start darkening
    GradientPoint := GradientBrush.Gradient.Points.Add as TGradientPoint;
    GradientPoint.Color := $00000000;
    GradientPoint.Offset := 0.55;

    // Edge (black with ~20% alpha)
    GradientPoint := GradientBrush.Gradient.Points.Add as TGradientPoint;
    GradientPoint.Color := $33000000;
    GradientPoint.Offset := 1.0;

    Canvas.Fill.Assign(GradientBrush);
    Canvas.FillEllipse(
      TRectF.Create(
        FullRect.Left - FullRect.Width * 0.15,
        FullRect.Top - FullRect.Height * 0.15,
        FullRect.Right + FullRect.Width * 0.15,
        FullRect.Bottom + FullRect.Height * 0.15
      ),
      1.0
    );
  finally
    GradientBrush.Free;
  end;

  // 4. Subtle cool tint for modern look (RoyalBlue with ~4% alpha)
  Canvas.Fill.Kind := TBrushKind.Solid;
  Canvas.Fill.Color := $0A4169E1;
  Canvas.FillRect(FullRect, 0, 0, [], 1.0);
end;


procedure TfrmScreenCapture.pbOverlayPaint(Sender: TObject; Canvas: TCanvas);
VAR
  FullRect, SelectRect, DrawRect: TRectF;
begin
  if NOT Assigned(FSCManager) OR NOT Assigned(FSCManager.Screenshot) then EXIT;

  FullRect:= pbOverlay.LocalRect;
  SelectRect:= SelectionRect.BoundsRect;

  Canvas.BeginScene;
  try
    // Apply selected overlay style
    case FOverlayStyle of
      osFrostedGlass:
        DrawFrostedGlassOverlay(Canvas, FullRect);

      osGlossy:
        begin
          // Draw the entire screenshot dimmed (base layer)
          Canvas.DrawBitmap(FSCManager.Screenshot, FSCManager.Screenshot.BoundsF, FullRect, 0.35, True);
          DrawGlossyOverlay(Canvas, FullRect);
        end;

      osSimpleDim:
        // Just dim the background
        Canvas.DrawBitmap(FSCManager.Screenshot, FSCManager.Screenshot.BoundsF, FullRect, 0.35, True);
    end;

    // Draw the selected area at full brightness (lift the fog)
    if NOT SelectRect.IsEmpty then
      begin
        DrawRect:= SelectRect;
        // Clip selection to visible area (handles partial selections at screen edges)
        DrawRect.Intersect(FullRect);
        if DrawRect.IsEmpty
        then EXIT;  // Selection is completely outside visible area

        // Draw selection with full brightness
        Canvas.DrawBitmap(FSCManager.Screenshot, DrawRect, DrawRect, 1.0, True);

        // Add subtle glow around selection (inner white border, ~70% alpha)
        Canvas.Stroke.Kind := TBrushKind.Solid;
        Canvas.Stroke.Color := $B2FFFFFF;
        Canvas.Stroke.Thickness := 2;
        Canvas.DrawRect(
          TRectF.Create(DrawRect.Left - 1, DrawRect.Top - 1, DrawRect.Right + 1, DrawRect.Bottom + 1),
          0, 0, [], 1.0
        );

        // Outer glow (DodgerBlue with ~40% alpha: $661E90FF)
        Canvas.Stroke.Color := $661E90FF;
        Canvas.Stroke.Thickness := 3;
        Canvas.DrawRect(
          TRectF.Create(DrawRect.Left - 3, DrawRect.Top - 3, DrawRect.Right + 3, DrawRect.Bottom + 3),
          0, 0, [], 1.0
        );
      end;
  finally
    Canvas.EndScene;
  end;
end;


procedure TfrmScreenCapture.SelectionRectChange(Sender: TObject);
begin
  // Repaint overlay when selection changes
  if Assigned(pbOverlay)
  then pbOverlay.Repaint;
end;


procedure TfrmScreenCapture.UpdateInstructions;
VAR Msg: string;
begin
  Msg:= '1. Select a rectangle on your screen' + #13#10;

  {$IFDEF MSWINDOWS}
  Msg:= Msg + '2. Press Ctrl+P to capture the selected area' + #13#10; {$ENDIF}
  {$IFDEF MACOS}
  Msg:= Msg + '2. Press Cmd+P to capture the selected area' + #13#10; {$ENDIF}
  {$IF DEFINED(IOS) OR DEFINED(ANDROID)}
  Msg:= Msg + '2. Tap to capture the selected area' + #13#10; {$ENDIF}

  Msg:= Msg + '3. Press ESC when done';

  lblInstructions.Text:= Msg;
end;


function TfrmScreenCapture.IsCaptureKeyPressed(Key: Word; Shift: TShiftState): Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result:= (Key = Ord('P')) AND (ssCtrl in Shift); {$ENDIF}
  {$IFDEF MACOS}
  Result:= (Key = Ord('P')) AND (ssCommand in Shift);  {$ENDIF}
  {$IF DEFINED(IOS) OR DEFINED(ANDROID)}
  Result:= FALSE; {$ENDIF}
end;


procedure TfrmScreenCapture.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  // Hide instructions after first keypress
  if layInstructions.Visible then
    begin
      FSCManager.CaptureTipShown:= FSCManager.CaptureTipShown+1;
      layInstructions.Visible:= FSCManager.CaptureTipShown < 3;
    end;

  // ESC - finish capturing (use ModalResult for modal forms)
  if Key = vkEscape then
    begin
      ModalResult:= mrOk;
      EXIT;
    end;

  // Ctrl+P / Cmd+P - capture selected area
  if IsCaptureKeyPressed(Key, Shift) then
    begin
      if FSCManager.CaptureSelectedArea(SelectionRect.BoundsRect) then
        begin
          ShowCaptureFlash;  // Visual feedback
          UpdateInstructions;
        end;
      Key:= 0;
    end;
end;


procedure TfrmScreenCapture.ShowCaptureFlash;
begin
  // Visual feedback: briefly flash the selection to confirm capture
  // Note: ProcessMessages is used here for immediate visual feedback during the flash
  // animation. A timer-based approach would be more complex for this simple effect.
  SelectionRect.Opacity:= 0.3;
  Application.ProcessMessages;
  Sleep(100);
  SelectionRect.Opacity:= 1.0;
  Application.ProcessMessages;
  Sleep(100);
  SelectionRect.Opacity:= 0.3;
  Application.ProcessMessages;
  Sleep(100);
  SelectionRect.Opacity:= 1.0;
end;


end.
