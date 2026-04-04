UNIT LightFmx.Visual.SvgFlatButton;

{-------------------------------------------------------------------------------------------------------------
   2026.04
   www.GabrielMoraru.com

   Flat pill-style button with SVG icon and optional text label.
   Designed for modern UIs (Slack-style sidebar buttons, action tiles, etc.).

   Features:
     - Smooth hover highlight via TColorAnimation (desktop; visual on tablet stylus too)
     - Press feedback via opacity dimming
     - Toggle mode (button stays active/highlighted)
     - Theme-aware: adapts icon color and hover tints for light/dark styles
     - Icon position: left of text (default) or above text

   Architecture:
     TFlatButton (TRectangle - transparent background, rounded corners)
       TPath           (SVG icon, stroke-based, HitTest=False)
       TLabel          (text, HitTest=False, styled by FMX theme)
       TColorAnimation (hover effect on Fill.Color)

   Usage:
     btn:= TFlatButton.Create(Self);
     btn.Parent:= SomeLayout;
     btn.Width:= 120;
     btn.Height:= 36;
     btn.LoadSvgPath(SVG_NavBar_Settings);   // SVG path data constant
     btn.TextLabel.Text:= 'Settings';
     btn.OnClick:= HandleClick;
     // btn.IconPosition:= ipTop;             // icon above text
     // btn.IsToggled:= True;                 // toggle mode

   Call ApplyThemeColors after loading a new FMX style (must be called from main thread).
   For standard buttons with icon overlay, see LightFmx.Visual.SvgButton instead.
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.UITypes, System.Classes, System.Types,
  FMX.Types, FMX.Controls, FMX.Objects, FMX.StdCtrls, FMX.Ani, FMX.Graphics;

TYPE
  TIconPosition = (ipLeft, ipTop);

  { Borderless button with SVG icon (TPath), optional label, hover animation, and toggle mode.
    Descends from TRectangle. All child controls are created and owned in the constructor. }
  TFlatButton = class(TRectangle)
  private
    FIconPath: TPath;
    FLabel: TLabel;
    FHoverAnim: TColorAnimation;
    FIsToggled: Boolean;
    FIconPosition: TIconPosition;
    FHoverColor: TAlphaColor;
    FToggleColor: TAlphaColor;
    FToggleHoverColor: TAlphaColor;
    procedure SetIconPosition(Value: TIconPosition);
    procedure SetIsToggled(Value: Boolean);
    procedure UpdateAnimValues;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp  (Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadSvgPath(CONST SvgPathData: string);
    procedure ApplyThemeColors;
    property IconPosition: TIconPosition read FIconPosition write SetIconPosition;
    property IsToggled: Boolean read FIsToggled write SetIsToggled;
    property Icon: TPath read FIconPath;
    property TextLabel: TLabel read FLabel;
  end;


procedure Register;


IMPLEMENTATION

USES
  LightFmx.Common.Styles;


constructor TFlatButton.Create(AOwner: TComponent);
begin
  inherited;

  { Background rectangle }
  Fill.Kind:= TBrushKind.Solid;
  Fill.Color:= TAlphaColors.Null;
  Stroke.Kind:= TBrushKind.None;
  XRadius:= 6;
  YRadius:= 6;
  Cursor:= crHandPoint;
  Padding.Rect:= TRectF.Create(8, 6, 8, 6);

  { Icon (stroke-based TPath, same pattern as SvgButtons.pas) }
  FIconPath:= TPath.Create(Self);
  FIconPath.Parent:= Self;
  FIconPath.HitTest:= FALSE;
  FIconPath.Fill.Kind:= TBrushKind.None;
  FIconPath.Stroke.Kind:= TBrushKind.Solid;
  FIconPath.Stroke.Thickness:= 1.5;
  FIconPath.WrapMode:= TPathWrapMode.Fit;
  FIconPath.Width:= 20;
  FIconPath.Height:= 20;
  FIconPath.Align:= TAlignLayout.Left;
  FIconPath.Margins.Right:= 6;
  FIconPath.Visible:= FALSE;

  { Label }
  FLabel:= TLabel.Create(Self);
  FLabel.Parent:= Self;
  FLabel.HitTest:= FALSE;
  FLabel.Align:= TAlignLayout.Client;
  FLabel.TextSettings.HorzAlign:= TTextAlign.Leading;

  { Hover animation }
  FHoverAnim:= TColorAnimation.Create(Self);
  FHoverAnim.Parent:= Self;
  FHoverAnim.PropertyName:= 'Fill.Color';
  FHoverAnim.Trigger:= 'IsMouseOver=true';
  FHoverAnim.TriggerInverse:= 'IsMouseOver=false';
  FHoverAnim.Duration:= 0.15;

  FIconPosition:= ipLeft;
  FIsToggled:= FALSE;

  ApplyThemeColors;
end;


procedure TFlatButton.LoadSvgPath(CONST SvgPathData: string);
begin
  FIconPath.Data.Data:= SvgPathData;
  FIconPath.Visible:= SvgPathData <> '';
end;


procedure TFlatButton.ApplyThemeColors;
begin
  if IsDarkStyle then
    begin
      FIconPath.Stroke.Color:= TAlphaColors.White;
      FHoverColor:=      $20FFFFFF;   // 12% white overlay
      FToggleColor:=     $30FFFFFF;   // 19% white
      FToggleHoverColor:= $40FFFFFF;  // 25% white
    end
  else
    begin
      FIconPath.Stroke.Color:= TAlphaColorRec.Dimgray;
      FHoverColor:=      $18000000;   // 9% black overlay
      FToggleColor:=     $28000000;   // 16% black
      FToggleHoverColor:= $38000000;  // 22% black
    end;

  UpdateAnimValues;

  if FIsToggled
  then Fill.Color:= FToggleColor
  else Fill.Color:= TAlphaColors.Null;
end;


{ Updates the hover animation Start/Stop values based on toggle state }
procedure TFlatButton.UpdateAnimValues;
begin
  if FIsToggled then
    begin
      FHoverAnim.StartValue:= FToggleColor;
      FHoverAnim.StopValue:= FToggleHoverColor;
    end
  else
    begin
      FHoverAnim.StartValue:= TAlphaColors.Null;
      FHoverAnim.StopValue:= FHoverColor;
    end;
end;


procedure TFlatButton.SetIconPosition(Value: TIconPosition);
begin
  FIconPosition:= Value;
  case Value of
    ipLeft:
      begin
        FIconPath.Align:= TAlignLayout.Left;
        FIconPath.Margins.Rect:= TRectF.Create(0, 0, 6, 0);
        FLabel.TextSettings.HorzAlign:= TTextAlign.Leading;
      end;
    ipTop:
      begin
        FIconPath.Align:= TAlignLayout.Top;
        FIconPath.Margins.Rect:= TRectF.Create(0, 0, 0, 4);
        FLabel.TextSettings.HorzAlign:= TTextAlign.Center;
      end;
  end;
  FLabel.Align:= TAlignLayout.Client;
end;


procedure TFlatButton.SetIsToggled(Value: Boolean);
begin
  FIsToggled:= Value;
  FHoverAnim.Stop;       { Cancel any in-progress hover animation to avoid color snap }
  UpdateAnimValues;

  if Value
  then Fill.Color:= FToggleColor
  else Fill.Color:= TAlphaColors.Null;
end;


procedure TFlatButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Button = TMouseButton.mbLeft
  then Opacity:= 0.7;
end;


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
