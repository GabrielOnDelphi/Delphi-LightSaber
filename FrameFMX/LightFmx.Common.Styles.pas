UNIT LightFmx.Common.Styles;

{=============================================================================================================
   2026.02
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Hint: Under VCL use TControl.IsLightStyleColor
=============================================================================================================}


INTERFACE

USES
   System.Classes, System.SysUtils, System.UITypes,
   FMX.Styles, FMX.Types, FMX.Controls, FMX.Graphics;

function IsStyleCompatible(const StylePath: string): Boolean;

{ Returns the foreground text color from the active style.
  Overload 1: Queries via a control's Scene (use from within a styled control).
  Overload 2: Queries the global active style (use from anywhere). }
function GetThemeTextColor(Scene: IScene): TAlphaColor; overload;
function GetThemeTextColor: TAlphaColor; overload;

{ Returns True if the currently active style has a dark background.
  Uses FMX.Utils.Luminance (ITU-R BT.709) on the style's background color. }
function IsDarkStyle: Boolean;

IMPLEMENTATION

USES
   FMX.Objects, FMX.Utils,
   LightCore.IO, LightCore.TextFile, LightCore, LightFmx.Common.AppData;


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
        {$IFDEF MACOS}
        Result:= Description.PlatformTarget.Contains('[MACOS]');
        {$ENDIF}
      end
      else
        Result:= True;   // No descriptor = older universal style
    end;
  finally
    FreeAndNil(StyleObj);
  end;
end;

{

:7c61bf1c ; C:\Projects\Project LearnAssist\LearnAssist src\FastMM_FullDebugMode.dll
:7c61c16c FastMM_FullDebugMode.GetRawStackTrace + 0xc4
FastMM4.DebugGetMem(???)
:0069ea6a DebugGetMem + $7E
:0068734a @GetMem + $A
:006de46b Format + $13
:006de450 Format + $20
:00a426bd TPresentationProxyFactory.GeneratePresentationName + $A9
:00a53378 TPresentedControl.DefinePresentationName + $1C
:00a52c39 TPresentedControl.AfterConstruction + $25
:00a8cb72 TPresentedTextControl.AfterConstruction + $A
:00a91374 TCheckBox.Create + $68
:007c6281 TReader.ReadComponent + $8D
:007c6560 TReader.ReadDataInner + $90
:007c64ca TReader.ReadData + $6E
:007d3ef6 TComponent.ReadState + $6
:007c6560 TReader.ReadDataInner + $90
:007c6499 TReader.ReadData + $3D
:007d3ef6 TComponent.ReadState + $6
:008d6505 TStyleStreaming.LoadFromStream + $1B9
:008d62fc TStyleStreaming.LoadFromFile + $48
LightFmx.Common.Styles.IsStyleCompatible('C:\Projects\Project LearnAssist\LearnAssist src\System\Skins\WedgewoodLight.style')
FormSkinsDisk.LoadStyleFromFile('WedgewoodLight.style')
FormSkinsDisk.LoadLastStyle('')
LearnAssist.LearnAssist
:75d85d49 KERNEL32.BaseThreadInitThunk + 0x19
:772ad83b ntdll.RtlInitializeExceptionChain + 0x6b
:772ad7c1 ;


}



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
