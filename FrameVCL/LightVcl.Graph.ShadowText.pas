unit LightVcl.Graph.ShadowText;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
  Draws shadow under text using DrawShadowText API from ComCtl32.dll.
  If the API is not available the shadow is drawn manually.

  Requires:
     Comctl32.DLL v6 (Windows Vista or up)
     App with manifest

  Features:
     * Uses lazy loading for DLL/function.
     * Fallback for XP or no manifest.
     * Handles system colors (clBtnFace, etc.) correctly via ColorToRGB.

  Note:
     The lazy loading is not thread-safe. If thread safety is required,
     call GetDrawShadowTextFn() once from the main thread during startup.

  TESTER:
     LightSaber\Demo\VCL\Demo cGraphText.pas\VCL_Demo_cGraphText.dpr
=============================================================================================================}

INTERFACE

USES
  WinApi.Windows, System.SysUtils, System.Classes,
  Vcl.Graphics;

{ Draws text with shadow at specified X, Y coordinates.
  TextColor/ShadowColor: Accepts both RGB colors and system colors (clBtnFace, etc.).
  ShadowDist: Shadow offset in pixels (applied to both X and Y). }
function DrawShadowText(Canvas: TCanvas; const Text: string; X, Y: Integer; TextColor, ShadowColor: TColor; ShadowDist: Integer = 2): Integer; overload;

{ Draws text with shadow within specified rectangle with DrawText flags.
  DrawFlags: Standard DrawText flags (DT_LEFT, DT_CENTER, DT_WORDBREAK, etc.).
  Note: DT_MODIFYSTRING should be avoided as Text is const. }
function DrawShadowText(Canvas: TCanvas; const Text: string; TextRect: TRect; TextColor, ShadowColor: TColor; ShadowDist: Integer; DrawFlags: DWORD = DT_LEFT or DT_END_ELLIPSIS): Integer; overload;

IMPLEMENTATION
USES LightVcl.Common.WinVersion;

TYPE
  TDrawShadowTextFn = function(
    hdc: HDC;
    pszText: LPCWSTR;
    cch: UINT;
    const pRect: PRect;
    dwFlags: DWORD;
    crText: COLORREF;
    crShadow: COLORREF;
    ixOffset: Integer;
    iyOffset: Integer): Integer; stdcall;

var
  GComCtl32Handle: HMODULE = 0;
  GDrawShadowTextFn: TDrawShadowTextFn = nil;


{ Lazy-loads the DrawShadowText function from ComCtl32.dll.
  Returns nil if the DLL or function is not available (Windows XP without manifest, etc.).
  Note: Not thread-safe. Call once from main thread if concurrent access is needed. }
function GetDrawShadowTextFn: TDrawShadowTextFn;
begin
  if not Assigned(GDrawShadowTextFn) then
  begin
    if (GComCtl32Handle = 0) and IsWindowsXPUp
    then GComCtl32Handle:= LoadLibrary('ComCtl32.dll');

    if GComCtl32Handle <> 0
    then @GDrawShadowTextFn := GetProcAddress(GComCtl32Handle, 'DrawShadowText');
    { If GetProcAddress fails, GDrawShadowTextFn remains nil and fallback will be used }
  end;

  Result:= GDrawShadowTextFn;
end;


{ Fallback implementation when Windows DrawShadowText API is unavailable.
  Draws the shadow by rendering the text twice: first offset for shadow, then at original position.
  TextRect is VAR because DrawText with certain flags may modify it. }
function DrawShadowTextFallback(
  Canvas: TCanvas;
  const Text: string;
  var TextRect: TRect;
  TextColor, ShadowColor: TColor;
  ShadowOffsetX, ShadowOffsetY: Integer;
  DrawFlags: DWORD): Integer;
var
  OrigFontColor: TColor;
  OrigBkMode: Integer;
  ShadowRect: TRect;
  SafeFlags: DWORD;
begin
  OrigBkMode := SetBkMode(Canvas.Handle, TRANSPARENT);
  OrigFontColor := Canvas.Font.Color;

  { Remove DT_MODIFYSTRING flag - it modifies the buffer but Text is const }
  SafeFlags:= DrawFlags AND (NOT DT_MODIFYSTRING);

  { Draw Shadow first (behind the text) }
  ShadowRect := TextRect;
  Inc(ShadowRect.Left,   ShadowOffsetX);
  Inc(ShadowRect.Top,    ShadowOffsetY);
  Inc(ShadowRect.Right,  ShadowOffsetX);
  Inc(ShadowRect.Bottom, ShadowOffsetY);

  Canvas.Font.Color := ShadowColor;
  DrawText(Canvas.Handle, PChar(Text), Length(Text), ShadowRect, SafeFlags);

  { Draw Foreground text on top }
  Canvas.Font.Color := TextColor;
  Result := DrawText(Canvas.Handle, PChar(Text), Length(Text), TextRect, SafeFlags);

  { Restore original canvas state }
  Canvas.Font.Color := OrigFontColor;
  SetBkMode(Canvas.Handle, OrigBkMode);
end;


function DrawShadowText(
  Canvas: TCanvas;
  const Text: string;
  X, Y: Integer;
  TextColor, ShadowColor: TColor;
  ShadowDist: Integer = 2
): Integer;
var
  TextRect: TRect;
  DrawFn: TDrawShadowTextFn;
begin
  Assert(Canvas <> NIL, 'DrawShadowText: Canvas cannot be nil');

  TextRect := Rect(X, Y, X + Canvas.TextWidth(Text), Y + Canvas.TextHeight(Text));
  DrawFn := GetDrawShadowTextFn(); //  Call as function

  if Assigned(DrawFn)
  then Result:= DrawFn(
        Canvas.Handle,
        PWideChar(Text),
        Length(Text),
        @TextRect,
        0,
        ColorToRGB(TextColor),     { Convert system colors (clBtnFace, etc.) to RGB }
        ColorToRGB(ShadowColor),
        ShadowDist, ShadowDist)
  else
    Result:= DrawShadowTextFallback(Canvas, Text, TextRect, TextColor, ShadowColor, ShadowDist, ShadowDist, 0);
end;


function DrawShadowText(Canvas: TCanvas; const Text: string; TextRect: TRect; TextColor, ShadowColor: TColor; ShadowDist: Integer; DrawFlags: DWORD = DT_LEFT or DT_END_ELLIPSIS): Integer;
VAR DrawFn: TDrawShadowTextFn;
begin
  Assert(Canvas <> NIL, 'DrawShadowText: Canvas cannot be nil');

  DrawFn := GetDrawShadowTextFn(); //   Call as function

  if Assigned(DrawFn) then 
    Result:= DrawFn(
         Canvas.Handle,
         PWideChar(Text),
         Length(Text),
         @TextRect,
         DrawFlags,
         ColorToRGB(TextColor),     { Convert system colors (clBtnFace, etc.) to RGB }
         ColorToRGB(ShadowColor),
         ShadowDist, ShadowDist)
  else
    Result:= DrawShadowTextFallback(Canvas, Text, TextRect, TextColor, ShadowColor, ShadowDist, ShadowDist, DrawFlags);
end;


// Free DLL on application exit (if loaded)
procedure FinalizeShadowText;
begin
  if GComCtl32Handle <> 0 then
  begin
    FreeLibrary(GComCtl32Handle);
    GComCtl32Handle := 0;
    GDrawShadowTextFn := nil;
  end;
end;


initialization
finalization
  FinalizeShadowText; // ToDo: try to get rid of this
end.
