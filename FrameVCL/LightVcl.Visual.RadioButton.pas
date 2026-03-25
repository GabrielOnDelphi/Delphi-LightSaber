UNIT LightVcl.Visual.RadioButton;

{=============================================================================================================
   2026.03
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
  A radio button that autoresizes exactly like TLabel
  It intercepts CMTextChanged where it recomputes the new Width

  Features:
      + property AutoSize -> Autoresize width to fix the text inside

  Important:
    The control will NOT be automatically resized IF you call LoadForm(self) in FormCreate (canvas not ready)..
    You need to call LoadForm(self) in LateInitialize.

  Similar: https://stackoverflow.com/questions/9678029/automatically-resize-a-delphi-button

  Tester: c:\Projects\Testers\Cubic VCL tester GLOBAL\

  Issue:
  https://stackoverflow.com/questions/47476603/major-flaw-radio-buttons-are-randomly-checked-when-showing-their-parent-form

=============================================================================================================}

// issue: Set a huge font: it will not resize its height to match the font height

INTERFACE

{.$DEBUGINFO ON}

USES
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.StdCtrls;

TYPE
  TLightRadioButton = class(TRadioButton)
  private
    FAutoSize: Boolean;
    procedure AdjustBounds;
    procedure setAutoSize(b: Boolean);  reintroduce;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
     procedure Loaded; override;
  public
     constructor Create(AOwner: TComponent); override;
  published
     property AutoSize: Boolean read FAutoSize write setAutoSize stored TRUE;
  end;


procedure Register;

IMPLEMENTATION

USES
  Vcl.Themes;

const
  FALLBACK_GLYPH_WIDTH = 13;  // Classic (unthemed) radio button glyph width at 96 DPI
  GLYPH_GAP            = 4;   // Gap between glyph and text


{ Returns the radio button glyph width for the given DPI, querying the current VCL style/theme.
  Same approach used by VCL's own TCheckListBox (Vcl.CheckLst.pas). }
function GetSysRadioWidth(DPI: Integer): Integer;
var
  DC: HDC;
  LSize: TSize;
  LStyle: TCustomStyleServices;
begin
  LStyle:= StyleServices;
  if LStyle.Enabled then
  begin
    DC:= CreateCompatibleDC(0);
    try
      if LStyle.GetElementSize(DC, LStyle.GetElementDetails(tbRadioButtonUncheckedNormal), esActual, LSize, DPI) AND (LSize.Width > 0)
      then EXIT(LSize.Width);

      { Fallback to system style if custom style returns zero }
      LStyle:= TStyleManager.SystemStyle;
      if LStyle.GetElementSize(DC, LStyle.GetElementDetails(tbRadioButtonUncheckedNormal), esActual, LSize, DPI) AND (LSize.Width > 0)
      then EXIT(LSize.Width);
    finally
      DeleteDC(DC);
    end;
  end;

  { Final fallback for classic (unthemed) Windows }
  Result:= MulDiv(FALLBACK_GLYPH_WIDTH, DPI, 96);
end;


constructor TLightRadioButton.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FAutoSize:= FALSE;   { This must be false, otherwise AdjustBounds will be executed when we drop the control on a form and we don't have a Parent at that point. }
end;


procedure TLightRadioButton.AdjustBounds;
VAR
   DC: HDC;
   Canvas: TCanvas;
   DPI: Integer;
   GlyphWidth: Integer;
begin
 if not (csReading in ComponentState) and FAutoSize then
   begin
    // We need a canvas but this control has none. So we need to "produce" one.
    // Use HWND_DESKTOP instead of Handle - controls on inactive TPageControl tabs have no valid handle.
    // https://stackoverflow.com/questions/59107255/autoresizing-tcheckbox-like-tlabel
    Canvas:= TCanvas.Create;
    DC:= GetDC(HWND_DESKTOP);
    TRY
      Canvas.Handle:= DC;
      Canvas.Font:= Font;
      DPI:= GetDeviceCaps(DC, LOGPIXELSX);
      GlyphWidth:= GetSysRadioWidth(DPI) + MulDiv(GLYPH_GAP, DPI, 96);
      Width:= Canvas.TextWidth(Caption) + GlyphWidth;
    FINALLY
      Canvas.Handle:= 0;
      ReleaseDC(HWND_DESKTOP, DC);
      FreeAndNil(Canvas);
    END;
   end;
end;


procedure TLightRadioButton.setAutoSize(b: Boolean);
begin
  if FAutoSize <> b then
  begin
    FAutoSize := b;
    if b then AdjustBounds;
  end;
end;


procedure TLightRadioButton.CMTextChanged(var Message:TMessage);
begin
  inherited;
  AdjustBounds;
end;


procedure TLightRadioButton.CMFontChanged(var Message:TMessage);
begin
  inherited;
  if AutoSize
  then AdjustBounds;
end;


procedure TLightRadioButton.Loaded;
begin
  inherited Loaded;
  AdjustBounds;
end;


procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TLightRadioButton]);
end;


end.