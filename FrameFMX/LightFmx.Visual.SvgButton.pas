UNIT LightFmx.Visual.SvgButton;

{-------------------------------------------------------------------------------------------------------------
   2026.04
   www.GabrielMoraru.com

   Helpers to attach SVG icons (TPath) to standard FMX buttons (TButton, TSpeedButton).
   The icon is a stroke-based TPath child created inside the button, using SVG path data
   strings (e.g. Tabler Icons). Useful for sidebar/navigation buttons that switch between
   icon-only (collapsed) and text (expanded) modes.

   Usage:
     AttachSvgToButton(btnSettings, SVG_NavBar_Settings);   // one-time setup
     ToggleSvgIcon(btnSettings, True);                      // show icon, hide text
     ToggleSvgIcon(btnSettings, False);                     // restore text, hide icon

   Note: ToggleSvgIcon caches the button text on first icon-show and restores it
   on icon-hide. The icon stroke color is matched to the theme text color each time
   the icon is shown, so it stays theme-aware across style changes.

   For flat pill-style buttons (no border, hover animation, toggle mode), see
   LightFmx.Visual.SvgFlatButton instead.
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.UITypes, System.Classes,
  FMX.Types, FMX.Graphics, FMX.StdCtrls, FMX.Objects;

{ Creates a TPath child inside Button with the given SVG path data.
  The icon starts hidden. Call ToggleSvgIcon to show/hide it.
  Idempotent: calling twice on the same button updates the SVG data without creating a duplicate. }
procedure AttachSvgToButton(Button: TCustomButton; CONST SvgPathData: string; IconSize: Single = 20);

{ Switches between icon-only and text mode.
  IconOnly=True:  caches button text, shows SVG icon, matches stroke to theme text color.
  IconOnly=False: restores cached text, hides SVG icon. }
procedure ToggleSvgIcon(Button: TCustomButton; IconOnly: Boolean);



IMPLEMENTATION



procedure AttachSvgToButton(Button: TCustomButton; CONST SvgPathData: string; IconSize: Single = 20);
VAR
  Path: TPath;
  Existing: TComponent;
begin
  { Reuse existing icon if already attached (idempotent) }
  Existing:= Button.FindComponent(Button.Name + '_SvgIcon');
  if Existing is TPath then
    begin
      TPath(Existing).Data.Data:= SvgPathData;
      EXIT;
    end;

  Path:= TPath.Create(Button);
  Path.Name:= Button.Name + '_SvgIcon';
  Path.Parent:= Button;
  Path.Data.Data:= SvgPathData;
  Path.Fill.Kind:= TBrushKind.None;
  Path.Stroke.Kind:= TBrushKind.Solid;
  Path.Stroke.Color:= TAlphaColorRec.Gray;   { Neutral default; ToggleSvgIcon sets the theme color }
  Path.Stroke.Thickness:= 1.5;
  Path.WrapMode:= TPathWrapMode.Fit;
  Path.HitTest:= FALSE;             { Clicks pass through to the button }
  Path.Align:= TAlignLayout.Center;
  Path.Width:= IconSize;
  Path.Height:= IconSize;
  Path.Visible:= FALSE;             { Shown only when ToggleSvgIcon(btn, True) is called }
  Path.TagString:= Button.Text;     { Cache the button's original text for later restore }
end;


procedure ToggleSvgIcon(Button: TCustomButton; IconOnly: Boolean);
VAR
  SvgIcon: TComponent;
  TextObj: TFmxObject;
begin
  SvgIcon:= Button.FindComponent(Button.Name + '_SvgIcon');
  if SvgIcon is TPath then
    begin
      TPath(SvgIcon).Visible:= IconOnly;
      if IconOnly then
        begin
          { Cache text before clearing (in case it changed since AttachSvgToButton) }
          if Button.Text <> ''
          then TPath(SvgIcon).TagString:= Button.Text;
          Button.Text:= '';

          { Match the icon stroke to the theme's text color }
          TextObj:= Button.FindStyleResource('text');
          if TextObj is TText
          then TPath(SvgIcon).Stroke.Color:= TText(TextObj).Color;
        end
      else
        { Restore the cached text }
        Button.Text:= TPath(SvgIcon).TagString;
    end;
end;


end.
