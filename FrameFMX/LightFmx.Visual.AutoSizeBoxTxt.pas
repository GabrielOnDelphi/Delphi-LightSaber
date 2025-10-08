UNIT LightFmx.Visual.AutoSizeBoxTxt;

{-------------------------------------------------------------------------------------------------------------
   GabrielMoraru.com
   2025.07
--------------------------------------------------------------------------------------------------------------
   A TRectangle that automatically adjusts its height to fit its internal text.
   Has the equivalent of an AutoSize property (but not exposed/published)
   Look a bit like a TWhatsApp dialog bubble.
   Has a nice shadow effect.

   Demo: Test Bubble\BubbleTest.dpr
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Types, System.Classes, System.Math, System.UITypes,
  FMX.StdCtrls, FMX.Effects, FMX.Graphics, FMX.Types, FMX.Controls, FMX.Objects, FMX.Controls.Presentation,
  FMX.TextLayout;

CONST
  CResizeTolerance  = 0.5; // Use a small, reasonable value for float comparison
  CTextHeightBuffer = 1.0; // Small buffer to prevent text from being cut (e.g., antialiasing issues)
  WhatsAppGreen = $FFE4F3E2;
  WhatsAppGrey      = $FFEFEFEF;

TYPE
  TBoxType = (bxUser, bxModel);

  TAutoSizeBoxTxt = class(TRectangle)
  private
    FCurrentWidth: Single; // To track width change
    FTextLabel: TLabel;    // The internal control that holds the text
    FText: string;
    FBoxType: TBoxType;
    procedure SetText(const Value: string);
    procedure SetBoxType(Value: TBoxType);
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Text: string read FText write SetText;
    procedure UpdateHeight;
    property BoxType: TBoxType read FBoxType write SetBoxType;
  end;


procedure Register;


IMPLEMENTATION



constructor TAutoSizeBoxTxt.Create(AOwner: TComponent);
var
  Shadow: TShadowEffect;
begin
  inherited Create(AOwner);

  // Flag set to -1 ensures the first system Resize event runs the calculation.
  FCurrentWidth := -1;
  Align:= TAlignLayout.Top;

  // The Bubble
  Fill.Color   := WhatsAppGreen; // Matches xFFE4F3E2 (WhatsApp green/user answer)
  Stroke.Color := $FF006300;
  Stroke.Kind  := TBrushKind.Solid;
  XRadius      := 11;        // Corner size
  YRadius      := 11;
  Padding.Rect := TRectF.Create(5, 5, 5, 5);
  Margins.Rect := TRectF.Create(5, 5, 5, 5);

  // Shadow effect
  Shadow := TShadowEffect.Create(Self);
  Shadow.Parent := Self;
  Shadow.Distance  := 3.0;
  Shadow.Direction := 45.0;
  Shadow.Softness  := 0.30;
  Shadow.Opacity   := 0.60;
  Shadow.ShadowColor := TAlphaColor($FF000000); // claBlack

  // Internal label
  FTextLabel := TLabel.Create(Self);
  FTextLabel.Parent   := Self;
  FTextLabel.Align    := TAlignLayout.Top; // This is important!
  FTextLabel.AutoSize := True;             // The key for the label to wrap and set its height
  FTextLabel.WordWrap := True;
  FTextLabel.HitTest  := False;
  FTextLabel.Stored   := False;            //  tells the Form Designer not to write the component's state to the .fmx or .dfm file when the parent component (TAutoHeightRectangle) is created at design time. Since the TAutoHeightRectangle's constructor already creates and configures the internal FTextLabel, we don't want the DFM to create it again, which would lead to duplicate controls and memory leaks if the component user were to place the control on a form and save it.
  FTextLabel.TextSettings.Font.Size := 20;

  // Set initial text directly to the label. Do NOT call SetText or Resize here.
  FText := 'Self-sizing WhatApp-like bubble. ' +
    'We are checking if the TAutoHeightRectangle component can correctly detect the wrapped height of its TLabel child and adjust its own Height property accordingly when the width of the boxConversation changes. ' + #13#10+
    'The goal is to eliminate the manual size calculation from the form unit.';
  FTextLabel.Text := FText;
end;


procedure TAutoSizeBoxTxt.SetText(const Value: string);
begin
  if FText <> Value then
    begin
      FText := Value;
      FTextLabel.Text := Value;
      // Directly update height on text change (handles startup and runtime text updates reliably)
      UpdateHeight;
      // In FMX, setting Text often triggers an internal RGN_Change (Region Change) message which leads to a Resize, so we just set the flag and let the system handle it.
    end;
end;


// New method to perform the actual height adjustment.
procedure TAutoSizeBoxTxt.UpdateHeight;
VAR LMinHeight: Single;
begin
  if Self.Width <= 0 then Exit;

  // Compute the required height for the TRectangle (Bubble)
  // FTextLabel.Height is now guaranteed to be correct after inherited Resize has run.
  LMinHeight := FTextLabel.Height + Self.Padding.Top + Self.Padding.Bottom + CTextHeightBuffer;

  // Set the new height, overriding any parent/alignment setting.
  Self.Height := Ceil(LMinHeight);
end;


procedure TAutoSizeBoxTxt.Resize;
begin
  // 1. Call inherited first. This is where FMX is forced to update FTextLabel.Height based on the current Self.Width and the wrapped text.
  inherited Resize;

  // 2. Check if the calculation needs to run:
  //    a) Initial state (FCurrentWidth = -1), OR
  //    b) Explicit width change
  if Assigned(Parent)
  and (FCurrentWidth < 0) or (Abs(Self.Width - FCurrentWidth) > CResizeTolerance) then
    begin
      FCurrentWidth:= Width;
      UpdateHeight;
    end;
end;


procedure TAutoSizeBoxTxt.SetBoxType(Value: TBoxType);
begin
  //if FBoxType = Value then EXIT;
  FBoxType := Value;

  case FBoxType of
    bxUser:
      begin
        Margins.Rect := TRectF.Create(40, 5, 5, 5);
        Fill.Color   := WhatsAppGreen;            // WhatsApp green
        Stroke.Kind  := TBrushKind.None;
      end;
    bxModel:
      begin
        Margins.Rect := TRectF.Create(5, 5, 40, 5);
        Fill.Color   := WhatsAppGrey;
        Stroke.Kind  := TBrushKind.None;
      end;
  end;
end;


procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TAutoSizeBoxTxt]);
end;


end.
