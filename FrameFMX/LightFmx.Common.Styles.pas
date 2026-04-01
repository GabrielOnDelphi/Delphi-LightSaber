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

function IsStyleCompatible(const StylePath: string): Boolean;

{ Returns the foreground text color from the active style }
function GetThemeTextColor(Scene: IScene): TAlphaColor; overload;
function GetThemeTextColor: TAlphaColor; overload;

{ Returns True if the currently active style has a dark background.
  Uses FMX.Utils.Luminance (ITU-R BT.709) on the style's background color. }
function IsDarkStyle: Boolean;

IMPLEMENTATION

USES
   FMX.Objects, FMX.Utils;


{ Probes the style file to check if it's compatible with the current platform.
  Returns True if compatible or if no descriptor exists (older universal styles). }
function IsStyleCompatible(const StylePath: string): Boolean;
var
  StyleObj: TFmxObject;
  Description: TStyleDescription;
begin
  Result:= False;

  StyleObj := TStyleStreaming.LoadFromFile(StylePath);
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


end.
