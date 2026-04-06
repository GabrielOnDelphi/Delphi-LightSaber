UNIT LightFmx.Visual.SvgFlatButton;

{-------------------------------------------------------------------------------------------------------------
   2026.04
   www.GabrielMoraru.com

   Borderless button with SVG icon (TPath).
   Designed for modern UIs (Slack-style sidebar buttons, action tiles, etc.).

   Hover visual feedback (3 effects combined):
     1. Background fill  - subtle tint of the skin's accent color (Fill.Color)
     2. Icon + text      - change from normal (skin foreground) to accent color
     3. Icon glow        - TGlowEffect in accent color appears under the icon

   Toggle mode keeps all 3 effects active until untoggled.
   Press feedback dims the whole button via Opacity.

   Colors are extracted from the active FMX style:
     - Normal color:    buttonstyle > text > NormalColor (what TButton uses for text)
     - Highlight color: 'selectioncolor' or 'selection' or 'glow' style resource (skin accent)

   Architecture:
     TFlatButton       (TRectangle - transparent background, rounded corners)
       TPath           (SVG icon, stroke-based, HitTest=False)
         TGlowEffect   (accent-colored glow, enabled on hover/toggle)
       TLabel          (text, HitTest=False)

   Usage:
     btn:= TFlatButton.Create(Self);
     btn.Parent:= SomeLayout;
     btn.LoadSvgPath(SVG_NavBar_Settings);   // SVG path data constant
     btn.TextLabel.Text:= 'Settings';
     btn.IconPosition:= ipTop;             // icon above text
     btn.IsToggled:= True;                 // toggle mode

   Call ApplyThemeColors after loading a new FMX style (must be called from main thread).
   For standard buttons with icon overlay, see LightFmx.Visual.SvgButton instead.
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.UITypes, System.Classes, System.Types, System.Math, System.Messaging,
  FMX.Styles, FMX.Styles.Objects, FMX.Types, FMX.Controls, FMX.Objects, FMX.StdCtrls, FMX.Graphics, FMX.Effects;

TYPE
  TIconPosition = (ipLeft,      // Icon of the left, text after
                   ipTop,       // Icon on top, text under
                   ipCenter);   // Icon in the middle of the button. No text.

  TFlatButton = class(TRectangle)
  private
    FIconPath: TPath;
    FGlow: TGlowEffect;
    FLabel: TLabel;
    FIsToggled: Boolean;
    FIconPosition: TIconPosition;
    FHoverBgEnabled: Boolean;       { When FALSE, background stays transparent on hover/toggle }
    FNormalColor: TAlphaColor;      { Icon/text color in normal state (from buttonstyle text NormalColor) }
    FHighlightColor: TAlphaColor;   { Icon/text color on hover/toggle (from skin's accent/selection color) }
    FHoverBgColor: TAlphaColor;     { Background fill on hover/toggle (FHighlightColor at low alpha) }
    FAutoCompact     : Boolean;
    FIsCompacted     : Boolean;
    FApplyingCompact : Boolean;       { Guard: Resize → EvaluateAutoCompact → ApplyCompact → Width change → Resize }
    FExpandedWidth   : Single;        { Width before compacting — restored on expand }
    FExpandedIconPos : TIconPosition; { IconPosition before compacting — restored on expand }
    function  GetText: string;
    procedure SetText(CONST Value: string);
    function  GetSvgData: string;
    procedure SetSvgData(CONST Value: string);
    procedure SetIconPosition(Value: TIconPosition);
    procedure SetIsToggled(Value: Boolean);
    procedure SetHoverBgEnabled(Value: Boolean);
    procedure ApplyColors;          { Applies all visual changes based on current hover/toggle state }
    procedure UpdateIconSize;       { Recalculates icon dimensions from button size and position mode }
    procedure HandleStyleChanged(const Sender: TObject; const M: System.Messaging.TMessage);
    procedure SetCompact(Value: Boolean);
    procedure SetAutoCompact(Value: Boolean);
    procedure ApplyCompact(MakeCompact: Boolean);
    procedure EvaluateAutoCompact;
    procedure HandleFormResize(const Sender: TObject; const M: System.Messaging.TMessage);
  protected
    procedure Loaded; override;
    procedure Resize; override;
    procedure Paint; override;
    procedure DoMouseEnter; override;   { Triggers highlight on mouse hover }
    procedure DoMouseLeave; override;   { Reverts to normal colors }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp  (Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadSvgPath(CONST SvgPathData: string);
    procedure ApplyThemeColors;     { Re-reads colors from the active skin. Call after style changes. }
    property Icon: TPath read FIconPath;
    property TextLabel: TLabel read FLabel;
  published

    property Text: string read GetText write SetText;   { Visible button label text (stored in FLabel). For the icon, use SvgData or LoadSvgPath. }
    property SvgData: string read GetSvgData write SetSvgData;
    property IconPosition: TIconPosition read FIconPosition write SetIconPosition default ipLeft;
    property IsToggled: Boolean read FIsToggled write SetIsToggled default FALSE;

    property HoverBgEnabled: Boolean read FHoverBgEnabled write SetHoverBgEnabled default TRUE;    { When TRUE (default), hover/toggle tints the background with the accent color. FALSE = background always transparent (icon/text/glow still change). }
    property Compact: Boolean read FIsCompacted write SetCompact default FALSE;                     { Manual compact toggle. TRUE = icon-only + shrink width. Ignored when AutoCompact is TRUE. }
    property AutoCompact: Boolean read FAutoCompact write SetAutoCompact default FALSE;             { Self-managing compact. Watches hosting form width and compacts below HIGH_WIDTH (1024px). }
  end;


procedure Register;


IMPLEMENTATION

USES
  FMX.Forms,
  LightFmx.Common.Styles;


CONST
  HOVER_BG_ALPHA = $28;    { ~16% opacity - subtle background tint on hover/toggle }



{-------------------------------------------------------------------------------------------------------------
   STYLE COLOR EXTRACTION
   These functions probe the active FMX style to extract colors that match the current skin.
-------------------------------------------------------------------------------------------------------------}

{ Returns the normal text color that TButton uses in the active style.
  Probes buttonstyle > text > NormalColor (e.g. claWhite on CalypsoSE Dark).
  Falls back to 'foregroundcolor' resource, then to a theme-based default.
  This ensures our flat button text matches standard TButton text. }
function GetButtonNormalTextColor: TAlphaColor;
VAR
  ActiveStyle, BtnStyle, TextObj: TFmxObject;
begin
  ActiveStyle:= TStyleManager.ActiveStyle(NIL);
  if ActiveStyle <> NIL then
    begin
      { Primary: button style -> text -> NormalColor }
      BtnStyle:= ActiveStyle.FindStyleResource('buttonstyle');
      if BtnStyle <> NIL then
        begin
          TextObj:= BtnStyle.FindStyleResource('text');
          if (TextObj <> NIL) AND (TextObj is TButtonStyleTextObject)
          then EXIT(TButtonStyleTextObject(TextObj).NormalColor);
        end;

      { Fallback: foregroundcolor resource }
      TextObj:= ActiveStyle.FindStyleResource('foregroundcolor');
      if (TextObj <> NIL) AND (TextObj is TColorObject)
      then EXIT(TColorObject(TextObj).Color);
    end;

  { No style loaded - use safe defaults }
  if IsDarkStyle
  then Result:= TAlphaColors.White
  else Result:= TAlphaColorRec.Dimgray;
end;


{ Returns the highlight/accent color (fully opaque) from the active style.
  This is the color used for hover effects (icon, text, glow, background tint).
  Probes style resources in priority order:
    1. 'selectioncolor' (TColorObject) - some styles define this
    2. 'selection' (TBrushObject) - e.g. Nero Dark has Brush.Color = x7F0377D0 (blue)
    3. 'glow' (TColorObject) - accent/glow color
    4. 'foregroundcolor' (TColorObject) - text color as last resort
  Always returns fully opaque (strips embedded alpha from the style resource). }
function GetStyleHighlightColor: TAlphaColor;   //todo: later: move this in a dedicated file
VAR
  ActiveStyle, Obj: TFmxObject;
begin
  ActiveStyle:= TStyleManager.ActiveStyle(NIL);
  if ActiveStyle <> NIL then
    begin
      Obj:= ActiveStyle.FindStyleResource('selectioncolor');
      if (Obj <> NIL) AND (Obj is TColorObject)
      then EXIT(TColorObject(Obj).Color OR TAlphaColor($FF000000));

      { TBrushObject 'selection' - used by Nero Dark, CalypsoSE, etc. }
      Obj:= ActiveStyle.FindStyleResource('selection');
      if (Obj <> NIL) AND (Obj is TBrushObject)
      then EXIT(TBrushObject(Obj).Brush.Color OR TAlphaColor($FF000000));

      Obj:= ActiveStyle.FindStyleResource('glow');
      if (Obj <> NIL) AND (Obj is TColorObject)
      then EXIT(TColorObject(Obj).Color OR TAlphaColor($FF000000));

      Obj:= ActiveStyle.FindStyleResource('foregroundcolor');
      if (Obj <> NIL) AND (Obj is TColorObject)
      then EXIT(TColorObject(Obj).Color OR TAlphaColor($FF000000));
    end;

  Result:= TAlphaColorRec.Gray;
end;


{ Returns the glow color from the active style's 'glow' resource.
  Falls back to GetStyleHighlightColor if no 'glow' resource exists.
  The skin may define a different glow color than the selection/accent color. }
function GetStyleGlowColor: TAlphaColor;
VAR
  ActiveStyle, Obj: TFmxObject;
begin
  ActiveStyle:= TStyleManager.ActiveStyle(NIL);
  if ActiveStyle <> NIL then
    begin
      Obj:= ActiveStyle.FindStyleResource('glow');
      if (Obj <> NIL) AND (Obj is TColorObject)
      then EXIT(TColorObject(Obj).Color OR TAlphaColor($FF000000));
    end;

  { No 'glow' resource - fall back to the general highlight color }
  Result:= GetStyleHighlightColor;
end;



{-------------------------------------------------------------------------------------------------------------
   CONSTRUCTOR / LIFECYCLE
-------------------------------------------------------------------------------------------------------------}

constructor TFlatButton.Create(AOwner: TComponent);
begin
  inherited;

  { Background rectangle - transparent by default, tinted on hover/toggle }
  Fill.Kind:= TBrushKind.Solid;
  Fill.Color:= TAlphaColors.Null;     { Transparent in normal state }
  Stroke.Kind:= TBrushKind.None;      { No border - "flat" look }
  XRadius:= 6;
  YRadius:= 6;
  Cursor:= crHandPoint;
  Padding.Rect:= TRectF.Create(1, 1, 1, 1);

  { Icon - stroke-based TPath using SVG path data (e.g. Tabler Icons).
    Named 'Icon' so the FMX streaming system reuses it instead of creating a duplicate. }
  FIconPath:= TPath.Create(Self);
  FIconPath.Name:= 'Icon';
  FIconPath.SetSubComponent(TRUE);
  FIconPath.Stored:= FALSE;
  FIconPath.Parent:= Self;
  FIconPath.HitTest:= FALSE;          { Runtime: clicks pass through to the TRectangle }
  FIconPath.Locked:= TRUE;            { Design time: designer skips locked controls during hit-testing, so clicks select the parent TFlatButton }
  FIconPath.Fill.Kind:= TBrushKind.None;
  FIconPath.Stroke.Kind:= TBrushKind.Solid;
  FIconPath.Stroke.Thickness:= 1.5;
  FIconPath.WrapMode:= TPathWrapMode.Fit;
  FIconPath.Visible:= FALSE;          { Hidden until SvgData is set }

  { Glow effect - child of FIconPath, shown on hover/toggle.
    Creates an accent-colored halo around the icon strokes. }
  FGlow:= TGlowEffect.Create(Self);
  FGlow.Parent:= FIconPath;
  FGlow.Softness:= 0.2;
  FGlow.Opacity := 0.9;
  FGlow.Enabled:= FALSE;              { Off in normal state }

  { Label - font color manually controlled (removed from StyledSettings).
    Named 'Caption' so the FMX streaming system reuses it. }
  FLabel:= TLabel.Create(Self);
  FLabel.Name:= 'Caption';
  FLabel.Text:= '';              { TTextControl.SetName auto-sets Text to Name; reset it }
  FLabel.SetSubComponent(TRUE);
  FLabel.Stored:= FALSE;
  FLabel.Parent:= Self;
  FLabel.HitTest:= FALSE;
  FLabel.Locked:= TRUE;
  FLabel.Align:= TAlignLayout.Client;
  FLabel.TextSettings.HorzAlign:= TTextAlign.Leading;
  FLabel.StyledSettings:= FLabel.StyledSettings - [TStyledSetting.FontColor];

  SetIconPosition(ipLeft);            { Canonical path — sets FIconPath.Align + margins }
  FIsToggled:= FALSE;
  FHoverBgEnabled:= TRUE;

  ApplyThemeColors;

  { Auto-refresh colors when the active FMX style changes (no manual RefreshAllThemeColors needed) }
  TMessageManager.DefaultManager.SubscribeToMessage(TStyleChangedMessage, HandleStyleChanged);

  { AutoCompact: react to hosting form resize — compacts when form width < HIGH_WIDTH }
  TMessageManager.DefaultManager.SubscribeToMessage(TSizeChangedMessage, HandleFormResize);
end;


destructor TFlatButton.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TStyleChangedMessage, HandleStyleChanged);
  TMessageManager.DefaultManager.Unsubscribe(TSizeChangedMessage, HandleFormResize);
  inherited;
end;


procedure TFlatButton.HandleStyleChanged(const Sender: TObject; const M: System.Messaging.TMessage);
begin
  ApplyThemeColors;
end;


{ Called after FMX streaming finishes (both at design time and runtime).
  Re-reads skin colors because streaming may have overwritten our values. }
procedure TFlatButton.Loaded;
begin
  inherited;
  ApplyThemeColors;
  UpdateIconSize;
  EvaluateAutoCompact;
end;


{ Called when button Width or Height changes (runtime resize, alignment, streaming).
  Recalculates the icon dimensions to match the new button size. }
procedure TFlatButton.Resize;
begin
  inherited;
  UpdateIconSize;
  if FAutoCompact and not FApplyingCompact then
    EvaluateAutoCompact;
end;


{ Recalculates icon Width/Height based on button size and current position mode.
  ipLeft:   icon Width = button inner height (square icon filling vertical space)
  ipTop:    no action — Client alignment fills the space above the Bottom-aligned label
  ipCenter: no action — Client alignment fills the button automatically }
procedure TFlatButton.UpdateIconSize;
VAR InnerH: Single;
begin
  case FIconPosition of
    ipLeft:
      begin
        InnerH:= Height - Padding.Top - Padding.Bottom;
        FIconPath.Width:= Max(InnerH, 1);
      end;
    ipTop:    ;  { Client alignment }
    ipCenter: ;  { Client alignment }
  end;
end;



{-------------------------------------------------------------------------------------------------------------
   PAINTING
-------------------------------------------------------------------------------------------------------------}

{ Draws a faint dotted border at design time so the button outline is visible in the form designer.
  At runtime this does nothing extra (csDesigning is not set). }
procedure TFlatButton.Paint;
begin
  inherited;
  if csDesigning in ComponentState then
    begin
      Canvas.Stroke.Kind:= TBrushKind.Solid;
      Canvas.Stroke.Color:= $30808080;
      Canvas.Stroke.Dash:= TStrokeDash.Dot;
      Canvas.Stroke.Thickness:= 1;
      Canvas.DrawRect(GetShapeRect, XRadius, YRadius, AllCorners, AbsoluteOpacity);
      Canvas.Stroke.Dash:= TStrokeDash.Solid;   { Restore shared canvas state }
    end;
end;



{-------------------------------------------------------------------------------------------------------------
   TEXT
-------------------------------------------------------------------------------------------------------------}

function TFlatButton.GetText: string;
begin
  Result:= FLabel.Text;
end;


procedure TFlatButton.SetText(CONST Value: string);
begin
  FLabel.Text:= Value;
end;




{-------------------------------------------------------------------------------------------------------------
   SVG DATA
-------------------------------------------------------------------------------------------------------------}

function TFlatButton.GetSvgData: string;
begin
  Result:= FIconPath.Data.Data;
end;


{ Sets the SVG path data and shows/hides the icon accordingly }
procedure TFlatButton.SetSvgData(CONST Value: string);
begin
  FIconPath.Data.Data:= Value;
  FIconPath.Visible:= Value <> '';

  { In ipCenter mode, label is only shown when there's no icon }
  if FIconPosition = ipCenter
  then FLabel.Visible:= NOT FIconPath.Visible;
end;


{ Convenience wrapper - same as setting SvgData property }
procedure TFlatButton.LoadSvgPath(CONST SvgPathData: string);
begin
  SvgData:= SvgPathData;
end;



{-------------------------------------------------------------------------------------------------------------
   THEME / COLORS

   ApplyThemeColors reads colors from the active skin (call after style changes).
   ApplyColors applies the correct visual state based on hover/toggle.

   Three things change on hover/toggle:
     1. Fill.Color       - background gets a subtle tint (FHoverBgColor)
     2. Icon + Label     - switch from FNormalColor to FHighlightColor
     3. FGlow            - enabled (accent-colored halo under the icon)
-------------------------------------------------------------------------------------------------------------}

{ Reads colors from the active FMX style. Call after loading a new skin. }
procedure TFlatButton.ApplyThemeColors;
begin
  FNormalColor   := GetButtonNormalTextColor;             { e.g. claWhite on dark skins }
  FHighlightColor:= GetStyleHighlightColor;               { e.g. #0377D0 blue on Nero Dark }
  FHoverBgColor  := ReplaceColorAlpha(FHighlightColor, HOVER_BG_ALPHA);  { Same blue at ~16% for background }
  FGlow.GlowColor:= GetStyleGlowColor;
  ApplyColors;
end;


{ Applies all visual changes based on current state.
  Called from: DoMouseEnter, DoMouseLeave, SetIsToggled, ApplyThemeColors. }
procedure TFlatButton.ApplyColors;
VAR Active: Boolean;
begin
  Active:= FIsToggled or IsMouseOver;

  { 1. Background fill - subtle accent tint or transparent (controlled by HoverBgEnabled) }
  if Active AND FHoverBgEnabled
  then Fill.Color:= FHoverBgColor
  else Fill.Color:= TAlphaColors.Null;

  { 2. Icon stroke + label text - accent color or normal }
  if Active then
    begin
      FIconPath.Stroke.Color:= FHighlightColor;
      FLabel.TextSettings.FontColor:= FHighlightColor;
    end
  else
    begin
      FIconPath.Stroke.Color:= FNormalColor;
      FLabel.TextSettings.FontColor:= FNormalColor;
    end;

  { 3. Icon glow }
  FGlow.Enabled:= Active;
end;


{KEEP
Makes a dull background. Is ~30% from hover color.   c:\Projects\LightSaber\FrameFMX\del _ 30 percent hover color.png

procedure TFlatButton.ApplyThemeColors;
VAR Highlight: TAlphaColor;
begin
  FIconPath.Stroke.Color:= GetThemeTextColor;
  Highlight:= GetStyleHighlightColor;

  FHoverColor      := ReplaceColorAlpha(Highlight, $30);   // ~19% - subtle hover hint
  FToggleColor     := ReplaceColorAlpha(Highlight, $48);   // ~28% - visible active state
  FToggleHoverColor:= ReplaceColorAlpha(Highlight, $60);   // ~38% - active + hover

  UpdateAnimValues;

  if FIsToggled
  then Fill.Color:= FToggleColor
  else Fill.Color:= TAlphaColors.Null;
end;}



{-------------------------------------------------------------------------------------------------------------
   ICON POSITION
-------------------------------------------------------------------------------------------------------------}

procedure TFlatButton.SetHoverBgEnabled(Value: Boolean);
begin
  if FHoverBgEnabled = Value then EXIT;
  FHoverBgEnabled:= Value;
  ApplyColors;
end;


{ Switches the icon layout relative to the text label.
  ipLeft:   icon on the left, text right-aligned (default - sidebar buttons)
  ipTop:    icon above, text centered below (tile buttons)
  ipCenter: icon centered, label hidden (icon-only square buttons) }
procedure TFlatButton.SetIconPosition(Value: TIconPosition);
begin
  FIconPosition:= Value;

  case Value of
    ipLeft:
      begin
        FIconPath.Align:= TAlignLayout.Left;
        FIconPath.Margins.Rect:= TRectF.Create(0, 0, 6, 0);
        FLabel.Visible:= TRUE;
        FLabel.Align:= TAlignLayout.Client;
        FLabel.TextSettings.HorzAlign:= TTextAlign.Leading;
      end;
    ipTop:
      begin
        FLabel.Visible:= TRUE;
        FLabel.Height:= 20;
        FLabel.Align:= TAlignLayout.Bottom;  { Label sits at the bottom with fixed height }
        FLabel.TextSettings.HorzAlign:= TTextAlign.Center;
        FIconPath.Align:= TAlignLayout.Client;  { Icon fills remaining space above the label }
        FIconPath.Margins.Rect:= TRectF.Create(0, 0, 0, 0);
      end;
    ipCenter:
      begin
        FIconPath.Align:= TAlignLayout.Client;  { Fill the button; WrapMode=Fit keeps aspect ratio }
        FIconPath.Margins.Rect:= TRectF.Create(0, 0, 0, 0);
        FLabel.Visible:= NOT FIconPath.Visible;  { Show label only when there's no icon }
        FLabel.Align:= TAlignLayout.Client;
        FLabel.TextSettings.HorzAlign:= TTextAlign.Center;
      end;
  end;

  UpdateIconSize;
end;



{-------------------------------------------------------------------------------------------------------------
   COMPACT / AUTO-COMPACT
   Compact:     manual toggle — caller controls when to compact (e.g., sidebar buttons responding to MultiView state).
   AutoCompact: self-managing — button watches hosting form's width via TSizeChangedMessage and
                compacts when below HIGH_WIDTH (1024). Initial state evaluated in Resize (when first parented).
   When AutoCompact is TRUE, manual Compact writes are ignored — AutoCompact owns the state.
-------------------------------------------------------------------------------------------------------------}

procedure TFlatButton.SetCompact(Value: Boolean);
begin
  if FAutoCompact then EXIT;  { AutoCompact owns the state — ignore manual writes }
  ApplyCompact(Value);
end;


procedure TFlatButton.SetAutoCompact(Value: Boolean);
begin
  if FAutoCompact = Value then EXIT;
  FAutoCompact:= Value;
  EvaluateAutoCompact;
end;


{ Checks hosting form width and compacts/expands accordingly.
  Called from: Resize (initial parenting), Loaded (streamed), SetAutoCompact, HandleFormResize. }
procedure TFlatButton.EvaluateAutoCompact;
begin
  if not FAutoCompact then EXIT;
  if (Scene = nil) then EXIT;
  if not (Scene.GetObject is TCommonCustomForm) then EXIT;
  ApplyCompact(TCommonCustomForm(Scene.GetObject).ClientWidth < HIGH_WIDTH);
end;


{ Responds to hosting form resize — filters by Scene.GetObject so we ignore other forms' resizes. }
procedure TFlatButton.HandleFormResize(const Sender: TObject; const M: System.Messaging.TMessage);
begin
  if not FAutoCompact then EXIT;
  if (Scene = nil) or (Sender <> Scene.GetObject) then EXIT;
  ApplyCompact(TCommonCustomForm(Sender).ClientWidth < HIGH_WIDTH);
end;


{ Switches between expanded (icon+text, original width) and compacted (icon-only, square).
  Width is only modified when Align allows it (Right, Left, MostRight, MostLeft, None). }
procedure TFlatButton.ApplyCompact(MakeCompact: Boolean);
begin
  if MakeCompact = FIsCompacted then EXIT;
  FApplyingCompact:= True;
  TRY
    if MakeCompact then
      begin
        FExpandedWidth:= Width;
        FExpandedIconPos:= FIconPosition;
        FIsCompacted:= True;
        SetIconPosition(ipCenter);
        if Align in [TAlignLayout.Right, TAlignLayout.Left, TAlignLayout.MostRight, TAlignLayout.MostLeft, TAlignLayout.None]
        then Width:= Height;
      end
    else
      begin
        FIsCompacted:= False;
        SetIconPosition(FExpandedIconPos);
        if Align in [TAlignLayout.Right, TAlignLayout.Left, TAlignLayout.MostRight, TAlignLayout.MostLeft, TAlignLayout.None]
        then Width:= FExpandedWidth;
      end;
  FINALLY
    FApplyingCompact:= False;
  END;
end;



{-------------------------------------------------------------------------------------------------------------
   TOGGLE / MOUSE INTERACTION
-------------------------------------------------------------------------------------------------------------}

{ Toggles the button's active state. When toggled, the button stays highlighted
  (accent color + glow + background) until untoggled. }
procedure TFlatButton.SetIsToggled(Value: Boolean);
begin
  FIsToggled:= Value;
  ApplyColors;
end;


{ Mouse enters the button - switch to highlighted state }
procedure TFlatButton.DoMouseEnter;
begin
  inherited;
  ApplyColors;
end;


{ Mouse leaves the button - revert to normal (unless toggled) }
procedure TFlatButton.DoMouseLeave;
begin
  inherited;
  Opacity:= 1.0;   { Restore; press feedback is meaningless when mouse has left }
  ApplyColors;
end;


{ Left mouse down - dim the whole button for press feedback }
procedure TFlatButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Button = TMouseButton.mbLeft
  then Opacity:= 0.7;
end;


{ Left mouse up - restore full opacity }
procedure TFlatButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Button = TMouseButton.mbLeft
  then Opacity:= 1.0;
end;




procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TFlatButton]);
end;


end.
