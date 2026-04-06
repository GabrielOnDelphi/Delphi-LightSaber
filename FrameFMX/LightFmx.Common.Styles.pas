UNIT LightFmx.Common.Styles;

{=============================================================================================================
   2026.03
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Hint: Under VCL use TControl.IsLightStyleColor
=============================================================================================================}


INTERFACE

USES
   System.SysUtils, System.UITypes,
   FMX.Styles, FMX.Types, FMX.Controls, FMX.Graphics;

CONST
  COMPACT_WIDTH = 600;  // Below this width, hide most buttons and use popup menu.
  HiGH_WIDTH    = 1024; // Below this with, show some buttons but some in "Compact" mode (only icon, no text). Over this resolution we show all.


function IsStyleCompatible(const StyleFile: string): Boolean;
function IsDarkStyle: Boolean;

// THEME COLOR
function GetThemeTextColor(Scene: IScene): TAlphaColor; overload;     { Returns the foreground text color from the active style }
function GetThemeTextColor: TAlphaColor; overload;

function GenerateThemePalette(BaseColor: TAlphaColor; IsDark: Boolean): TArray<TAlphaColor>;

// COLORS
function ReplaceColorAlpha(Color: TAlphaColor; NewAlpha: Byte): TAlphaColor; inline;


IMPLEMENTATION

USES
   FMX.Objects, FMX.Utils, System.UIConsts;



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


{ Probes the background color of the active style to determine if it is dark.
  Checks 'backgroundstyle' (TRectangle) and 'background' (TBrushObject) resources.
  Returns False when no style is loaded or no background resource found. }
function IsDarkStyle: Boolean;
VAR
  ActiveStyle: TFmxObject;
  BgObj: TFmxObject;
begin
  Result:= False;
  ActiveStyle:= TStyleManager.ActiveStyle(NIL);
  if ActiveStyle = NIL then EXIT;

  // Try 'backgroundstyle' first (TRectangle in most FMX styles)
  BgObj:= ActiveStyle.FindStyleResource('backgroundstyle');
  if (BgObj <> NIL) AND (BgObj is TRectangle)
  then EXIT(Luminance(TRectangle(BgObj).Fill.Color) < 0.5);

  // Fallback: try 'background' (TBrushObject in some styles)
  BgObj:= ActiveStyle.FindStyleResource('background');
  if (BgObj <> NIL) AND (BgObj is TBrushObject)
  then EXIT(Luminance(TBrushObject(BgObj).Brush.Color) < 0.5);
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
