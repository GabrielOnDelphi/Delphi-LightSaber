UNIT LightFmx.Common.Styles;

{=============================================================================================================
   2026.03
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Hint: Under VCL use TControl.IsLightStyleColor
   
   
   See https://github.com/HemulGM/WindowDarkMode
=============================================================================================================}


INTERFACE

USES
   System.SysUtils, System.UITypes,
   FMX.Styles, FMX.Types, FMX.Controls, FMX.Graphics;

// SCREEN SIZE
function IsPhoneScreen  : Boolean;   { Width < COMPACT_WIDTH (600) — phone }
function IsTabletScreen : Boolean;   { Width in COMPACT_WIDTH..HIGH_WIDTH (600–1024) — tablet }
function IsDesktopScreen: Boolean;   { Width >= HIGH_WIDTH (1024+) — desktop }

// THEME
function IsStyleCompatible(const StyleFile: string): Boolean;

// THEME COLOR
function IsDarkStyle: Boolean;

function GetThemeTextColor(Scene: IScene): TAlphaColor; overload;     { Returns the foreground text color from the active style }
function GetThemeTextColor: TAlphaColor; overload;
function GetThemeBackgroundColor(OUT Color: TAlphaColor): Boolean;    { Returns the form background color from the active style. FALSE if unavailable (image-based skins like Calypso/Jet). }

function GetButtonNormalTextColor: TAlphaColor;  { Returns the normal text color that TButton uses — buttonstyle > text > NormalColor. Falls back to foregroundcolor or theme default. }
function GetStyleHighlightColor: TAlphaColor;    { Returns the highlight/accent color from the active style (selectioncolor → selection → glow → foregroundcolor). Always fully opaque. }
function GetStyleGlowColor: TAlphaColor;         { Returns the glow color from 'glow' resource. Falls back to GetStyleHighlightColor if not found. }

function GenerateThemePalette(BaseColor: TAlphaColor; IsDark: Boolean): TArray<TAlphaColor>;


// COLORS
function ReplaceColorAlpha(Color: TAlphaColor; NewAlpha: Byte): TAlphaColor; inline;


IMPLEMENTATION

USES
   FMX.Objects, FMX.Utils, System.UIConsts,
   FMX.Platform, FMX.Forms,
   FMX.Styles.Objects;

CONST
  COMPACT_WIDTH = 600;  // Below this width, hide most buttons and use popup menu.
  HiGH_WIDTH    = 1024; // Below this with, show some buttons but some in "Compact" mode (only icon, no text). Over this resolution we show all.



{ IFMXScreenService returns physical screen size on mobile (Android/iOS).
  Screen.Size can return 0 on mobile if called before the main form is fully created,
  because FMX populates it lazily from the platform's screen metrics. }
function GetScreenWidth: Single;
VAR ScreenSvc: IFMXScreenService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenSvc)
  then Result:= ScreenSvc.GetScreenSize.X
  else Result:= Screen.Size.Width;
end;


function IsPhoneScreen: Boolean;
begin
  Result:= GetScreenWidth < COMPACT_WIDTH;
end;


function IsTabletScreen: Boolean;
VAR W: Single;
begin
  W:= GetScreenWidth;
  Result:= (W >= COMPACT_WIDTH) AND (W < HiGH_WIDTH);
end;


function IsDesktopScreen: Boolean;
begin
  Result:= GetScreenWidth >= HiGH_WIDTH;
end;



{ Returns a copy of Color with the alpha channel replaced.
  Used to create semi-transparent variants, e.g. a highlight color at 16% opacity for hover backgrounds. }
function ReplaceColorAlpha(Color: TAlphaColor; NewAlpha: Byte): TAlphaColor; inline;
begin
  Result:= Color;
  TAlphaColorRec(Result).A:= NewAlpha;
end;


{ Probes the style file to check if it's compatible with the current platform.
  Returns True if compatible or if no descriptor exists (older universal styles). }
function IsStyleCompatible(const StyleFile: string): Boolean;
var
  StyleObj: TFmxObject;
  Description: TStyleDescription;
begin
  Result:= False;

  StyleObj := TStyleStreaming.LoadFromFile(StyleFile);
  try
    if StyleObj <> nil then
    begin
      Description := TStyleManager.FindStyleDescriptor(StyleObj);
      if Description <> nil then
      begin
        {$IFDEF MSWINDOWS}
        Result:= Description.PlatformTarget.Contains('[MSWINDOWS]')
           OR Description.PlatformTarget.Contains('[ANY]')
           OR (Description.PlatformTarget = '');
        {$ENDIF}
        {$IF DEFINED(MACOS) AND NOT DEFINED(IOS)}
        Result:= Description.PlatformTarget.Contains('[MACOS]')
           OR Description.PlatformTarget.Contains('[ANY]')
           OR (Description.PlatformTarget = '');
        {$ENDIF}
        {$IFDEF IOS}
        Result:= Description.PlatformTarget.Contains('[IOS')      // Matches [IOS7], [IOSALTERNATE]
           OR Description.PlatformTarget.Contains('[ANY]')
           OR (Description.PlatformTarget = '');
        {$ENDIF}
        {$IFDEF ANDROID}
        Result:= Description.PlatformTarget.Contains('[ANDROID]')
           OR Description.PlatformTarget.Contains('[ANY]')
           OR (Description.PlatformTarget = '');
        {$ENDIF}
        {$IFDEF LINUX}
        Result:= Description.PlatformTarget.Contains('[LINUX]')
           OR Description.PlatformTarget.Contains('[ANY]')
           OR (Description.PlatformTarget = '');
        {$ENDIF}
      end
      else
        Result:= True;   // No descriptor = older universal style
    end;
  finally
    FreeAndNil(StyleObj);
  end;
end;
 




{ Returns the foreground text color from the style accessible via a control's Scene.
  Falls back to Dimgray if no style or no 'foregroundcolor' resource found. }
function GetThemeTextColor(Scene: IScene): TAlphaColor;
VAR StyleObj: TFmxObject;
begin
  StyleObj:= NIL;
  if Assigned(Scene) AND Assigned(Scene.StyleBook)
  then StyleObj:= Scene.StyleBook.Style.FindStyleResource('foregroundcolor');

  if Assigned(StyleObj)
  AND (StyleObj is TColorObject)
  then Result:= TColorObject(StyleObj).Color
  else Result:= TAlphaColorRec.Dimgray;
end;


{ Returns the foreground text color from the global active style.
  Falls back to Dimgray if no style or no 'foregroundcolor' resource found. }
function GetThemeTextColor: TAlphaColor;
VAR
  ActiveStyle: TFmxObject;
  StyleObj: TFmxObject;
begin
  ActiveStyle:= TStyleManager.ActiveStyle(NIL);
  if ActiveStyle <> NIL
  then StyleObj:= ActiveStyle.FindStyleResource('foregroundcolor')
  else StyleObj:= NIL;

  if Assigned(StyleObj) AND (StyleObj is TColorObject)
  then Result:= TColorObject(StyleObj).Color
  else Result:= TAlphaColorRec.Dimgray;
end;


{ Determines if the active FMX style is a dark theme.
  Checks TStyleDescription.PlatformTarget for [DARKSTYLE] tag.
     This is authoritative and handles image-based skins (e.g. Calypso Dark) where
     the backgroundstyle is a TStyleObject with no Fill.Color to probe.
  Fallback 1. Background probe: reads Fill.Color from 'backgroundstyle' (TRectangle) or Brush.Color from 'background' (TBrushObject) and tests luminance < 0.5.
  Fallback 2. Text probe: if text color is light (luminance > 0.5), the theme must be dark.
  Returns False when no style is loaded or detection fails. }
function IsDarkStyle: Boolean;
VAR
  ActiveStyle: TFmxObject;
  BgObj: TFmxObject;
  Description: TStyleDescription;
begin
  Result:= False;
  ActiveStyle:= TStyleManager.ActiveStyle(NIL);
  if ActiveStyle = NIL then EXIT;

  // Primary: check style metadata (handles image-based skins like Calypso Dark)
  Description:= TStyleManager.FindStyleDescriptor(ActiveStyle);
  if Description <> NIL
  then EXIT(Description.PlatformTarget.Contains('[DARKSTYLE]'));

  // Fallback 1: probe 'backgroundstyle' rectangle fill color
  BgObj:= ActiveStyle.FindStyleResource('backgroundstyle');
  if (BgObj <> NIL) AND (BgObj is TRectangle)
  then EXIT(Luminance(TRectangle(BgObj).Fill.Color) < 0.5);

  // Fallback 2: probe 'background' brush color
  BgObj:= ActiveStyle.FindStyleResource('background');
  if (BgObj <> NIL) AND (BgObj is TBrushObject)
  then EXIT(Luminance(TBrushObject(BgObj).Brush.Color) < 0.5);

  // Safety net: if label text is light, theme must be dark
  BgObj:= ActiveStyle.FindStyleResource('text');
  if (BgObj <> NIL) AND (BgObj is TText)
  then EXIT(Luminance(TText(BgObj).Color) > 0.5);
end;



{ Returns the form background color from the active style.
  Probes 'backgroundstyle' (TRectangle in most styles) then 'background' (TBrushObject).
  Returns FALSE for image-based styles (Calypso, Jet, Diamond) where no solid color is available. }
function GetThemeBackgroundColor(out Color: TAlphaColor): Boolean;
VAR
  ActiveStyle, BgObj: TFmxObject;
begin
  Result:= False;
  ActiveStyle:= TStyleManager.ActiveStyle(NIL);
  if ActiveStyle = NIL then EXIT;

  BgObj:= ActiveStyle.FindStyleResource('backgroundstyle');
  if (BgObj <> NIL) AND (BgObj is TRectangle) then
    begin
      Color:= TRectangle(BgObj).Fill.Color;
      EXIT(True);
    end;

  BgObj:= ActiveStyle.FindStyleResource('background');
  if (BgObj <> NIL) AND (BgObj is TBrushObject) then
    begin
      Color:= TBrushObject(BgObj).Brush.Color;
      EXIT(True);
    end;
end;



{ Returns the normal text color that TButton uses in the active style.
  Probes buttonstyle > text > NormalColor (e.g. claWhite on CalypsoSE Dark).
  Falls back to 'foregroundcolor' resource, then to a theme-based default.
  This ensures flat button text matches standard TButton text. }
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
  Probes style resources in priority order:
    1. 'selectioncolor' (TColorObject) - some styles define this
    2. 'selection' (TBrushObject) - e.g. Nero Dark has Brush.Color = x7F0377D0 (blue)
    3. 'glow' (TColorObject) - accent/glow color
    4. 'foregroundcolor' (TColorObject) - text color as last resort
  Always returns fully opaque (strips embedded alpha from the style resource). }
function GetStyleHighlightColor: TAlphaColor;
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
   PALETTE GENERATION
-------------------------------------------------------------------------------------------------------------}
{ Generates a palette of colors derived from BaseColor, adjusted for theme visibility.
  Light themes: darker shades (visible on light backgrounds).
  Dark themes: lighter tints (visible on dark backgrounds).
  Falls back to neutral grays if BaseColor is black/near-black. }
function GenerateThemePalette(BaseColor: TAlphaColor; IsDark: Boolean): TArray<TAlphaColor>;
CONST
  STEPS = 10;
VAR
  H, S, L: Single;
  MinL, MaxL, StepSize: Single;
begin
  RGBtoHSL(BaseColor, H, S, L);

  // Black/near-black: generate neutral grays since HSL variations produce nothing useful
  if (L < 0.05) AND (S < 0.1) then
    begin
      SetLength(Result, 8);
      for VAR i:= 0 to 7 do
        begin
          if IsDark
          then Result[i]:= HSLtoRGB(0, 0, 0.30 + i * 0.08)
          else Result[i]:= HSLtoRGB(0, 0, 0.15 + i * 0.08);
          TAlphaColorRec(Result[i]).A:= $FF;
        end;
      EXIT;
    end;

  // Normal: sweep lightness across a range appropriate for the theme
  if IsDark
  then begin MinL:= 0.35; MaxL:= 0.85; end
  else begin MinL:= 0.15; MaxL:= 0.65; end;

  StepSize:= (MaxL - MinL) / (STEPS - 1);
  SetLength(Result, STEPS);
  for VAR i:= 0 to STEPS - 1 do
    begin
      Result[i]:= HSLtoRGB(H, S, MinL + i * StepSize);
      TAlphaColorRec(Result[i]).A:= $FF;
    end;
end;


end.
