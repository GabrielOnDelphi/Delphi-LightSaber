UNIT LightFmx.Common.Styles;

{=============================================================================================================
   2026.06.10
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Hint: Under VCL use TControl.IsLightStyleColor
   
   
   See https://github.com/HemulGM/WindowDarkMode
=============================================================================================================}


INTERFACE

USES
   System.SysUtils, System.UITypes,
   FMX.Styles, FMX.Types, FMX.Controls, FMX.Graphics;

// THEME

// Overload with diagnostic out-param. LoadFailed=TRUE means the file could not be parsed
// (corrupted, wrong format, access denied) — this is distinct from "loaded but
// PlatformTarget excludes the current OS". Callers showing user dialogs should
// surface a different error message for each case.
function IsStyleCompatible(const StyleFile: string; out LoadFailed: Boolean): Boolean; overload;

// Back-compat thin wrapper: returns FALSE for both incompatible AND load-failed.
function IsStyleCompatible(const StyleFile: string): Boolean; overload;

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


{ Returns a copy of Color with the alpha channel replaced.
  Used to create semi-transparent variants, e.g. a highlight color at 16% opacity for hover backgrounds. }
function ReplaceColorAlpha(Color: TAlphaColor; NewAlpha: Byte): TAlphaColor; inline;
begin
  Result:= Color;
  TAlphaColorRec(Result).A:= NewAlpha;
end;


{ Probes the style file. Reports compatibility AND distinguishes load failure
  (corrupted file, wrong format, access denied) from "loaded but incompatible
  with current platform" via the LoadFailed out-param. }
function IsStyleCompatible(const StyleFile: string; out LoadFailed: Boolean): Boolean;
var
  StyleObj   : TFmxObject;
  Description: TStyleDescription;
begin
  Result:= False;
  LoadFailed:= False;

  TRY
    StyleObj := TStyleStreaming.LoadFromFile(StyleFile);
  EXCEPT
    // TStyleStreaming.LoadFromFile RAISES (EStyleException for bad format, EFOpenError for
    // locked/inaccessible files) — it does not return nil for unreadable files. Convert the
    // raise into the documented LoadFailed contract: this probe is called from LoadLastStyle
    // in the DPR BEFORE Application.Run, where an escaping exception aborts startup with no UI.
    // The failure is surfaced to the user and the log by the caller (see FormSkinsDisk.LoadStyleFromFile).
    on E: Exception do
      begin
        LoadFailed:= True;
        EXIT;
      end;
  END;

  if StyleObj = nil then
    begin
      LoadFailed:= True;   // file unreadable / corrupted / wrong format
      EXIT;
    end;

  try
    Description := TStyleManager.FindStyleDescriptor(StyleObj);
    if Description = nil then


        EXIT(true);  // No descriptor = older universal style — assume compatible


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
  finally
    FreeAndNil(StyleObj);
  end;
end;


function IsStyleCompatible(const StyleFile: string): Boolean;
var Discard: Boolean;
begin
  Result:= IsStyleCompatible(StyleFile, Discard);
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
  Tries four signals in order; first one that yields a usable answer wins.
    1. Background-rectangle probe: 'backgroundstyle' TRectangle.Fill.Color luminance < 0.5.
       Catches most real-world solid-background dark styles (CalypsoSE Dark, Nero Dark, custom dark styles).
    2. Background-brush probe: 'background' TBrushObject.Brush.Color luminance < 0.5.
       Catches older styles that use TBrushObject instead of TRectangle.
    3. Text-color probe: 'text' TText.Color luminance > 0.5 (light text ⇒ dark theme).
       Catches IMAGE-based skins (Jet, Calypso, Diamond) — these have a TStyleObject for 'backgroundstyle' (bitmap fill, not a probe-able solid color) but their 'text' resource is still a TText whose Color is set to claWhite (or similar light tone) on dark variants.
    4. PlatformTarget '[DARKSTYLE]' tag.
       Last-resort: relies on the style author having opted into the Sydney-era metadata tag. Many older dark styles omit it.
  Returns False when no style is loaded or none of the four signals fire.

  Why probes come before the tag: the [DARKSTYLE] PlatformTarget marker is opt-in (Sydney-era metadata) and most third-party dark styles ship without it. Trusting the tag first would misclassify them as light. Reading the actual visible color is authoritative — the tag is only useful when there is nothing else to read.

  Why three probes instead of one: different style file vintages encode the chrome background differently. Modern styles paint it with a TRectangle, older styles with a TBrushObject, and image-based skins (Jet, Calypso, Diamond) paint it with a tiled bitmap that has no probe-able solid color at all. The TText probe is the fallback that catches image-based skins, because even when the background is a bitmap the label text color is still a plain TAlphaColor that reveals the theme. }
function IsDarkStyle: Boolean;
VAR
  ActiveStyle: TFmxObject;
  Obj: TFmxObject;
  Description: TStyleDescription;
begin
  Result:= False;
  ActiveStyle:= TStyleManager.ActiveStyle(NIL);
  if ActiveStyle = NIL then EXIT;

  // 1. Probe 'backgroundstyle' rectangle fill color
  Obj:= ActiveStyle.FindStyleResource('backgroundstyle');
  if (Obj <> NIL) AND (Obj is TRectangle)
  then EXIT(Luminance(TRectangle(Obj).Fill.Color) < 0.5);

  // 2. Probe 'background' brush color
  Obj:= ActiveStyle.FindStyleResource('background');
  if (Obj <> NIL) AND (Obj is TBrushObject)
  then EXIT(Luminance(TBrushObject(Obj).Brush.Color) < 0.5);

  // 3. Image-based skin: 'backgroundstyle' is a TStyleObject (bitmap) — no solid color to probe.
  //    Image-based skins (Jet, Calypso, Diamond, ...) DO have a top-level 'labelstyle' that is a
  //    TLayout with a single TText child whose FontColor IS the chrome's foreground text color.
  //    Walk the layout's children one level for the TText. Light text (Luminance > 0.5) on an
  //    image-based skin ⇒ the chrome bitmaps are dark.
  //    Note: FindStyleResource second arg is Clone, not Recurse — pass nothing / False.
  Obj:= ActiveStyle.FindStyleResource('labelstyle');
  if Obj <> NIL then
   for VAR i:= 0 to Obj.ChildrenCount - 1 do
    if Obj.Children[i] is TText
    then EXIT(Luminance(TText(Obj.Children[i]).Color) > 0.5);

  // 4. No usable color probe — fall back to the [DARKSTYLE] tag in the style metadata
  Description:= TStyleManager.FindStyleDescriptor(ActiveStyle);
  if Description <> NIL
  then EXIT(Description.PlatformTarget.Contains('[DARKSTYLE]'));
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
