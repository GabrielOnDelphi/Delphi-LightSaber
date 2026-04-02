UNIT LightFmx.Visual.CheckBox;

{=============================================================================================================
   Gabriel Moraru
   2026.04
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   FMX TCheckBox with AutoSize support — mirrors TLabel.AutoSize from FMX.StdCtrls.

   FMX's TCheckBox has no AutoSize property. When placed in a layout with Align=Left,
   it keeps whatever Width was last set, wasting space. This component measures the
   text + checkbox indicator from the applied style and adjusts Width automatically.

   Implementation follows Embarcadero's TLabel.FitSize pattern (FMX.StdCtrls.pas):
   - CalcTextObjectSize measures text via TTextLayout (inherited from TPresentedTextControl)
   - Indicator width is captured from the style at ApplyStyle time (theme-aware, no hardcoding)
   - Reentrance guard (FInFitSize) prevents infinite Resize loops
   - Deferred flag (FNeedFitSize) handles the case where Scene/TextObject isn't ready yet
   - Respects alignment constraints (does not resize when Client/Contents-aligned)

   VCL counterpart: LightVcl.Visual.CheckBox.pas (uses Canvas.TextWidth + StyleServices)
=============================================================================================================}

INTERFACE

USES
  System.Classes, System.Types,
  FMX.Types, FMX.StdCtrls, FMX.Controls;

TYPE
  TLightCheckBox = class(TCheckBox)
  private
    FAutoSize: Boolean;
    FInFitSize: Boolean;       // Reentrance guard: FitSize changes Width which triggers Resize which calls FitSize
    FNeedFitSize: Boolean;     // Deferred: style not yet applied when AutoSize was requested
    FIndicatorWidth: Single;   // Checkbox glyph container width, captured from style in ApplyStyle
    procedure SetAutoSize(const Value: Boolean);
    procedure FitSize;
  protected
    procedure ApplyStyle; override;
    procedure DoChanged; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
  end;


procedure Register;


IMPLEMENTATION

CONST
  FALLBACK_INDICATOR_WIDTH = 24;  // Used when FindStyleResource('background') fails


constructor TLightCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSize:= FALSE;  // Must be false initially — no Scene/TextObject available yet
  FIndicatorWidth:= FALLBACK_INDICATOR_WIDTH;
end;


procedure TLightCheckBox.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
    begin
      FAutoSize:= Value;
      if FAutoSize
      then FitSize;
    end;
end;


{ Capture the indicator width from the applied style, then auto-size if needed.
  The checkbox style contains a left-aligned TLayout holding the 'background' element.
  Its width varies by theme (20px in Win10Modern, 15px in Dark). }
procedure TLightCheckBox.ApplyStyle;
VAR
  BgObj: TFmxObject;
  ParentCtrl: TControl;
begin
  inherited;

  // Get indicator width from the 'background' element's parent layout
  BgObj:= FindStyleResource('background');
  if (BgObj is TControl) AND Assigned(TControl(BgObj).ParentControl) then
    begin
      ParentCtrl:= TControl(BgObj).ParentControl;
      FIndicatorWidth:= ParentCtrl.Width + ParentCtrl.Margins.Left + ParentCtrl.Margins.Right;
    end
  else
    FIndicatorWidth:= FALLBACK_INDICATOR_WIDTH;

  if FAutoSize OR FNeedFitSize
  then FitSize;
end;


{ Called when Text or TextSettings change. Trigger re-measurement. }
procedure TLightCheckBox.DoChanged;
begin
  inherited;
  if NOT (csLoading in ComponentState) AND FAutoSize
  then FitSize;
end;


procedure TLightCheckBox.Resize;
begin
  inherited;
  if FAutoSize
  then FitSize;
end;


{ Core auto-sizing logic, adapted from TLabel.FitSize (FMX.StdCtrls.pas line 3878).
  CalcTextObjectSize measures the text area only (inherited from TPresentedTextControl).
  We add FIndicatorWidth to get the total checkbox width. }
procedure TLightCheckBox.FitSize;
VAR
  LSize: TSizeF;
  Rect: TRectF;
begin
  if FInFitSize OR (Align in [TAlignLayout.Client, TAlignLayout.Contents]) then EXIT;

  FInFitSize:= TRUE;
  TRY
    if CalcTextObjectSize(Width, LSize) then
      begin
        FNeedFitSize:= FALSE;
        Rect.TopLeft:= Position.Point;

        // Width: text + indicator (unless vertically stacked — then parent controls width)
        if NOT (Align in VerticalStackAlignments + [TAlignLayout.VertCenter]) then
          begin
            if Text = ''
            then Rect.Width:= FIndicatorWidth
            else Rect.Width:= LSize.cx + FIndicatorWidth;
          end
        else
          Rect.Width:= Width;

        // Height: text height (unless horizontally stacked — then parent controls height)
        if NOT (Align in HorizontalStackAlignments + [TAlignLayout.HorzCenter]) then
          Rect.Height:= LSize.cy
        else
          Rect.Height:= Height;

        BoundsRect:= Rect;
      end
    else
      FNeedFitSize:= TRUE;  // Style not ready — defer until ApplyStyle
  FINALLY
    FInFitSize:= FALSE;
  END;
end;



procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TLightCheckBox]);
end;


end.
