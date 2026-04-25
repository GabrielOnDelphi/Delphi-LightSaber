UNIT LightFmx.Visual.SvgFlatButton;

{-------------------------------------------------------------------------------------------------------------
   2026.04.25
   www.GabrielMoraru.com
-------------------------------------------------------------------------------------------------------------

   Borderless button with SVG icon (TPath).
   Designed for modern UIs (Slack-style sidebar buttons, action tiles, etc.).

   HOVER
     Hover visual feedback (3 effects combined):
       1. Background fill  - subtle tint of the skin's accent color (Fill.Color)
       2. Icon + text      - change from normal (skin foreground) to accent color
       3. Icon glow        - TGlowEffect in accent color appears under the icon

   TOGGLE
     Toggle mode keeps all 3 effects active until untoggled.
     Press feedback dims the whole button via Opacity.

   COLORS
     Colors are extracted from the active FMX style:
       - Normal color:    buttonstyle > text > NormalColor (what TButton uses for text)
       - Highlight color: 'selectioncolor' or 'selection' or 'glow' style resource (skin accent)

     Call ApplyThemeColors after loading a new FMX style (must be called from main thread).
     For standard buttons with icon overlay, see LightFmx.Visual.SvgButton instead.

   AUTO-COMPACT
       Compact           -> Manual toggle — caller controls when to compact (e.g., sidebar buttons responding to MultiView state).
       AutoCompact       -> Self-managing — button watches hosting form's client width via TSizeChangedMessage.
                            Initial state evaluated in Resize (when first parented).
       CompactThreshold  -> ctTablet (default): compact below HIGH_WIDTH (1024 px).
                            ctPhone: compact below COMPACT_WIDTH (600 px).

       When AutoCompact is TRUE, manual Compact is ignored — AutoCompact owns the state.

   COMPOSITS
     TSvgButton        (TRectangle - transparent background, rounded corners)
       TPath           (SVG icon, stroke-based, HitTest=False)
         TGlowEffect   (accent-colored glow, enabled on hover/toggle)
       TLabel          (text, HitTest=False)

   Usage:
     btn:= TSvgButton.Create(Self);
     btn.Parent:= SomeLayout;
     btn.LoadSvgPath(SVG_NavBar_Settings);   // SVG path data constant
     btn.TextLabel.Text:= 'Settings';
     btn.IconPosition:= ipTop;             // icon above text
     btn.IsToggled:= True;                 // toggle mode



   Tester: c:\Projects\LightSaber\Demo\FMX\Demos\

-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.UITypes, System.Classes, System.Types, System.Math, System.Messaging,
  FMX.Forms, FMX.Styles, FMX.Types, FMX.Controls, FMX.Objects, FMX.StdCtrls, FMX.Graphics, FMX.Effects;

TYPE
  TIconPosition = (ipLeft,      // Icon of the left, text after
                   ipTop,       // Icon on top, text under
                   ipCenter);   // Icon in the middle of the button. No text.

  TCompactChangedEvent = procedure(Sender: TObject; IsCompact: Boolean) of object;

  { Do NOT reorder — ordinal values are streamed into DFMs via the default clause on CompactThreshold.
    Append new values at the end only. }
  TCompactThreshold = (ctPhone,    // Compact when hosting form client width < COMPACT_WIDTH (600 px)
                       ctTablet);  // Compact when hosting form client width < HIGH_WIDTH (1024 px)

  TSvgButton = class(TRectangle)
  private
    FIconPath: TPath;
    FGlow: TGlowEffect;
    FLabel: TLabel;
    FIsToggled: Boolean;
    FIconPosition: TIconPosition;
    FHoverBackground: Boolean;         { When FALSE, background stays transparent on hover/toggle }
    FNormalColor: TAlphaColor;         { Icon/text color in normal state (from buttonstyle text NormalColor) }
    FHighlightColor: TAlphaColor;      { Icon/text color on hover/toggle (from skin's accent/selection color) }
    FHoverBgColor: TAlphaColor;        { Background fill on hover/toggle (FHighlightColor at low alpha) }
    FAutoCompact      : Boolean;
    FCompactThreshold : TCompactThreshold;
    FIsCompacted      : Boolean;
    FApplyingCompact  : Boolean;       { Guard: Resize → EvaluateAutoCompact → ApplyCompact → Width change → Resize }
    FExpandedWidth    : Single;        { Width before compacting — restored on expand }
    FExpandedIconPos  : TIconPosition; { IconPosition before compacting — restored on expand }
    FOnCompactChanged : TCompactChangedEvent;
    function  GetText: string;
    procedure SetText(CONST Value: string);
    function  GetSvgData: string;
    procedure SetSvgData(CONST Value: string);
    procedure SetIconPosition(Value: TIconPosition);
    procedure SetIsToggled(Value: Boolean);
    procedure SetHoverBackground(Value: Boolean);
    procedure ApplyColors;          { Applies all visual changes based on current hover/toggle state }
    procedure UpdateIconSize;       { Recalculates icon dimensions from button size and position mode }
    procedure HandleStyleChanged(const Sender: TObject; const M: System.Messaging.TMessage);
    procedure SetCompact(Value: Boolean);
    procedure SetAutoCompact(Value: Boolean);
    procedure SetCompactThreshold(Value: TCompactThreshold);
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
    property SvgData: string read GetSvgData write SetSvgData;   //todo 1: let me also enter directly an svg file. the program will extract the svg path from it.
    property IconPosition: TIconPosition read FIconPosition write SetIconPosition default ipLeft;
    property IsToggled: Boolean read FIsToggled write SetIsToggled default FALSE;  // IsPressed

    property HoverBackground: Boolean read FHoverBackground write SetHoverBackground default TRUE;    { When TRUE (default), hover/toggle tints the background with the accent color. FALSE = background always transparent (icon/text/glow still change). }
    property Compact: Boolean read FIsCompacted write SetCompact stored FALSE default FALSE;        { Manual compact toggle. TRUE = icon-only + shrink width. Ignored when AutoCompact is TRUE. Not streamed to DFM — always stores expanded Width. }
    property AutoCompact: Boolean read FAutoCompact write SetAutoCompact default FALSE;             { Self-managing compact. Watches hosting form client width and compacts based on CompactThreshold. }
    property CompactThreshold: TCompactThreshold read FCompactThreshold write SetCompactThreshold default ctTablet;  { ctTablet = compact below 1024 px, ctPhone = compact below 600 px. Only used when AutoCompact is TRUE. }
    property OnCompactChanged: TCompactChangedEvent read FOnCompactChanged write FOnCompactChanged; { Fired after compact state changes — use to sync UI (e.g. checkbox) with auto-compact transitions. }
  end;


{ Recursively sets Compact on all TSvgButton descendants of Parent.
  Use in a FormResize handler to switch sidebar/toolbar buttons between icon+text and icon-only.
  NOTE: Buttons with AutoCompact=TRUE are skipped silently — they self-manage via CompactThreshold. }
procedure CompactSvgButtons(Parent: TFmxObject; Compact: Boolean);


procedure Register;


IMPLEMENTATION

USES
  System.SysUtils,
  LightFmx.Common.Styles, LightFmx.Common.Screen;


CONST
  HOVER_BG_ALPHA = $28;    { ~16% opacity - subtle background tint on hover/toggle }




{-------------------------------------------------------------------------------------------------------------
   CONSTRUCTOR / LIFECYCLE
-------------------------------------------------------------------------------------------------------------}

constructor TSvgButton.Create(AOwner: TComponent);
begin
  inherited;

  { Background rectangle - transparent by default, tinted on hover/toggle }
  Fill.Kind:= TBrushKind.Solid;
  Fill.Color:= TAlphaColors.Null;     { Transparent in normal state }
  Stroke.Kind:= TBrushKind.None;      { No border - "flat" look }
  XRadius:= 6;
  YRadius:= 6;
  Padding.Rect:= TRectF.Create(1, 1, 1, 1);
  ClipChildren:= TRUE;                { Label text that overflows the button is clipped mid-character instead of trimmed at word boundary }

  { Icon - stroke-based TPath using SVG path data (e.g. Tabler Icons).
    Named 'Icon' so the FMX streaming system reuses it instead of creating a duplicate. }
  FIconPath:= TPath.Create(Self);
  FIconPath.Name:= 'Icon';
  FIconPath.SetSubComponent(TRUE);
  FIconPath.Stored:= FALSE;
  FIconPath.Parent:= Self;
  FIconPath.HitTest:= FALSE;          { Runtime: clicks pass through to the TRectangle }
  FIconPath.Locked:= TRUE;            { Design time: designer skips locked controls during hit-testing, so clicks select the parent TSvgButton }
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
  FLabel.TextSettings.WordWrap:= FALSE;                { Single line — overflow handled by ClipChildren }
  FLabel.TextSettings.Trimming:= TTextTrimming.None;   { Don't drop trailing words/chars; let the caller see as much text as fits }
  FLabel.StyledSettings:= FLabel.StyledSettings - [TStyledSetting.FontColor, TStyledSetting.Other];  { Other covers WordWrap/Trimming — exclude so skin changes don't undo them }

  SetIconPosition(ipLeft);            { Canonical path — sets FIconPath.Align + margins }
  FIsToggled:= FALSE;
  FHoverBackground:= TRUE;
  FCompactThreshold:= ctTablet;       { Default: compact below HIGH_WIDTH (1024 px) }

  ApplyThemeColors;

  if NOT (csDesigning in ComponentState) then
    begin
      { Auto-refresh colors when the active FMX style changes (no manual RefreshAllThemeColors needed) }
      TMessageManager.DefaultManager.SubscribeToMessage(TStyleChangedMessage, HandleStyleChanged);

      { AutoCompact: react to hosting form resize — compacts when form width < HIGH_WIDTH }
      TMessageManager.DefaultManager.SubscribeToMessage(TSizeChangedMessage, HandleFormResize);
    end;
end;


destructor TSvgButton.Destroy;
begin
  if NOT (csDesigning in ComponentState) then
    begin
      TMessageManager.DefaultManager.Unsubscribe(TStyleChangedMessage, HandleStyleChanged);
      TMessageManager.DefaultManager.Unsubscribe(TSizeChangedMessage, HandleFormResize);
    end;
  inherited;
end;


procedure TSvgButton.HandleStyleChanged(const Sender: TObject; const M: System.Messaging.TMessage);
begin
  if csDesigning in ComponentState then EXIT;
  ApplyThemeColors;
end;


{ Called after FMX streaming finishes (both at design time and runtime).
  Re-reads skin colors because streaming may have overwritten our values. }
procedure TSvgButton.Loaded;
begin
  inherited;
  ApplyThemeColors;
  UpdateIconSize;
  if Scene <> nil then
    begin
      EvaluateAutoCompact;
      EXIT;
    end;
  if csDesigning in ComponentState then EXIT;   { Designer never auto-compacts — skip the deferred queue }

  { Scene may be nil here (e.g., button inside a TFrame parented after streaming).
    Defer evaluation to the next message loop iteration, by which time the frame is attached to its form. }
  TThread.ForceQueue(nil,
    procedure
    begin
      if csDestroying in ComponentState then EXIT;
      EvaluateAutoCompact;
    end);
end;


{ Called when button Width or Height changes (runtime resize, alignment, streaming).
  Recalculates the icon dimensions to match the new button size. }
procedure TSvgButton.Resize;
begin
  inherited;
  UpdateIconSize;
  if FAutoCompact
  AND NOT FApplyingCompact
  then EvaluateAutoCompact;
end;


{ Recalculates icon Width/Height based on button size and current position mode.
  ipLeft:   icon Width = button inner height (square icon filling vertical space)
  ipTop:    no action — Client alignment fills the space above the Bottom-aligned label
  ipCenter: no action — Client alignment fills the button automatically }
procedure TSvgButton.UpdateIconSize;
VAR InnerH: Single;
begin
  case FIconPosition of
    ipLeft:
      begin
        InnerH:= Height - Padding.Top - Padding.Bottom;
        FIconPath.Width:= Max(InnerH, 1);
      end;
    ipTop:
      begin
        InnerH:= Height - Padding.Top - Padding.Bottom - FLabel.Height;
        FIconPath.Height:= Max(InnerH, 1);
      end;
    ipCenter: ;  { Client alignment fills button }
  end;
end;



{-------------------------------------------------------------------------------------------------------------
   PAINTING
-------------------------------------------------------------------------------------------------------------}

{ Draws a faint dotted border at design time so the button outline is visible in the form designer.
  At runtime this does nothing extra (csDesigning is not set). }
procedure TSvgButton.Paint;
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

function TSvgButton.GetText: string;
begin
  Result:= FLabel.Text;
end;


procedure TSvgButton.SetText(CONST Value: string);
begin
  FLabel.Text:= Value;
end;




{-------------------------------------------------------------------------------------------------------------
   SVG DATA
-------------------------------------------------------------------------------------------------------------}

function TSvgButton.GetSvgData: string;
begin
  Result:= FIconPath.Data.Data;
end;


{ Sets the SVG path data and shows/hides the icon accordingly }
procedure TSvgButton.SetSvgData(CONST Value: string);
begin
  FIconPath.Data.Data:= Value;
  FIconPath.Visible:= Value <> '';

  { In ipCenter mode, label is only shown when there's no icon }
  if FIconPosition = ipCenter
  then FLabel.Visible:= NOT FIconPath.Visible;
end;


{ Convenience wrapper - same as setting SvgData property }
procedure TSvgButton.LoadSvgPath(CONST SvgPathData: string);
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
procedure TSvgButton.ApplyThemeColors;
begin
  FNormalColor   := GetButtonNormalTextColor;             { e.g. claWhite on dark skins }
  FHighlightColor:= GetStyleHighlightColor;               { e.g. #0377D0 blue on Nero Dark }
  FHoverBgColor  := ReplaceColorAlpha(FHighlightColor, HOVER_BG_ALPHA);  { Same blue at ~16% for background }
  FGlow.GlowColor:= GetStyleGlowColor;
  ApplyColors;
end;


{ Applies all visual changes based on current state.
  Called from: DoMouseEnter, DoMouseLeave, SetIsToggled, ApplyThemeColors. }
procedure TSvgButton.ApplyColors;
VAR Active: Boolean;
begin
  Active:= FIsToggled or IsMouseOver;

  { 1. Background fill - subtle accent tint or transparent (controlled by HoverBackground) }
  if Active AND FHoverBackground
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



procedure TSvgButton.SetHoverBackground(Value: Boolean);
begin
  if FHoverBackground = Value then EXIT;
  FHoverBackground:= Value;
  ApplyColors;
end;



{-------------------------------------------------------------------------------------------------------------
   ICON POSITION
-------------------------------------------------------------------------------------------------------------}

{ Switches the icon layout relative to the text label.
  ipLeft:   icon on the left, text right-aligned (default - sidebar buttons)
  ipTop:    icon above, text centered below (tile buttons)
  ipCenter: icon centered, label hidden (icon-only square buttons)

  When the button is currently compacted, the new value is remembered as the user's
  expanded preference and applied on the next expand — visual stays compact for now. }
procedure TSvgButton.SetIconPosition(Value: TIconPosition);
begin
  if not FApplyingCompact then
    begin
      FExpandedIconPos:= Value;        { Track user intent — survives compact cycles }
      if FIsCompacted then EXIT;       { Visual stays compact; queued for next expand }
    end;

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
        FLabel.Align:= TAlignLayout.Bottom;     { Align BEFORE Height — prevents Client layout from overriding the explicit 20 }
        FLabel.Height:= 20;
        FLabel.TextSettings.HorzAlign:= TTextAlign.Center;
        FIconPath.Align:= TAlignLayout.Top;     { Top + explicit Height avoids Client/Bottom ordering dependency }
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
-------------------------------------------------------------------------------------------------------------}
procedure TSvgButton.SetCompact(Value: Boolean);
begin
  if FAutoCompact then EXIT;  { AutoCompact owns the state — ignore manual writes }
  ApplyCompact(Value);
end;


procedure TSvgButton.SetAutoCompact(Value: Boolean);
begin
  if FAutoCompact = Value then EXIT;
  FAutoCompact:= Value;
  if FAutoCompact
  then EvaluateAutoCompact     { Just turned on — react to current form width }
  else
    if FIsCompacted            { Just turned off — restore expanded state so manual control is clean }
    then ApplyCompact(False);
end;


procedure TSvgButton.SetCompactThreshold(Value: TCompactThreshold);
begin
  if FCompactThreshold = Value then EXIT;
  FCompactThreshold:= Value;
  EvaluateAutoCompact;
end;


{ Returns TRUE when the hosting form's client width is below the configured threshold. }
function ShouldCompactFor(ClientWidth: Single; Threshold: TCompactThreshold): Boolean;
begin
  case Threshold of
    ctPhone : Result:= WidthFitsPhone(ClientWidth);       // < COMPACT_WIDTH (600)
    ctTablet: Result:= NOT WidthFitsDesktop(ClientWidth); // < HIGH_WIDTH (1024)
  else
    raise ERangeError.CreateFmt('TCompactThreshold value %d not handled', [Ord(Threshold)]);
  end;
end;


{ Checks hosting form width and compacts/expands accordingly.
  Called from: Resize (initial parenting), Loaded (streamed), SetAutoCompact, HandleFormResize. }
procedure TSvgButton.EvaluateAutoCompact;
var Form: TCommonCustomForm;
begin
  if not FAutoCompact then EXIT;
  if csDesigning in ComponentState then EXIT;  { Never compact at design time — corrupts saved IconPosition }
  if (Scene = nil) then EXIT;
  if not (Scene.GetObject is TCommonCustomForm) then EXIT;
  Form:= TCommonCustomForm(Scene.GetObject);
  ApplyCompact(ShouldCompactFor(Form.ClientWidth, FCompactThreshold));
end;


{ Responds to hosting form resize — filters by Scene.GetObject so we ignore other forms' resizes. }
procedure TSvgButton.HandleFormResize(const Sender: TObject; const M: System.Messaging.TMessage);
begin
  if csDesigning in ComponentState then EXIT;
  if not FAutoCompact then EXIT;
  if (Scene = nil) or (Sender <> Scene.GetObject) then EXIT;
  if not (Sender is TCommonCustomForm) then EXIT;
  ApplyCompact(ShouldCompactFor(TCommonCustomForm(Sender).ClientWidth, FCompactThreshold));
end;


{ Switches between expanded (icon+text, original width) and compacted (icon-only, square).
  Width is only modified when Align allows it (Right, Left, MostRight, MostLeft, None). }
procedure TSvgButton.ApplyCompact(MakeCompact: Boolean);
begin
  if MakeCompact = FIsCompacted then EXIT;
  FApplyingCompact:= True;
  TRY
    if MakeCompact then
      begin
        FExpandedWidth:= Width;
        { FExpandedIconPos is maintained by SetIconPosition — no need to capture here }
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
  if Assigned(FOnCompactChanged)
  then FOnCompactChanged(Self, FIsCompacted);
end;



{-------------------------------------------------------------------------------------------------------------
   TOGGLE / MOUSE INTERACTION
-------------------------------------------------------------------------------------------------------------}

{ Toggles the button's active state. When toggled, the button stays highlighted
  (accent color + glow + background) until untoggled. }
procedure TSvgButton.SetIsToggled(Value: Boolean);
begin
  if FIsToggled = Value then EXIT;
  FIsToggled:= Value;
  ApplyColors;
end;


{ Mouse enters the button - switch to highlighted state }
procedure TSvgButton.DoMouseEnter;
begin
  inherited;
  ApplyColors;
end;


{ Mouse leaves the button - revert to normal (unless toggled) }
procedure TSvgButton.DoMouseLeave;
begin
  inherited;
  Opacity:= 1.0;   { Restore; press feedback is meaningless when mouse has left }
  ApplyColors;
end;


{ Left mouse down - dim the whole button for press feedback }
procedure TSvgButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Button = TMouseButton.mbLeft
  then Opacity:= 0.7;
end;


{ Left mouse up - restore full opacity }
procedure TSvgButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Button = TMouseButton.mbLeft
  then Opacity:= 1.0;
end;




procedure CompactSvgButtons(Parent: TFmxObject; Compact: Boolean);
VAR
  i: Integer;
  Child: TFmxObject;
begin
  for i:= 0 to Parent.ChildrenCount-1 do
    begin
      Child:= Parent.Children[i];
      if Child is TSvgButton
      then TSvgButton(Child).Compact:= Compact
      else
        if Child.ChildrenCount > 0
        then CompactSvgButtons(Child, Compact);
    end;
end;


procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TSvgButton]);
end;


end.
