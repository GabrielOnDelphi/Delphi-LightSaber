UNIT LightFmx.Visual.AutoHeightRectangle;

{-------------------------------------------------------------------------------------------------------------
   GabrielMoraru.com
   2025.07
--------------------------------------------------------------------------------------------------------------
   A TRectangle that automatically adjusts its height to fit its internal text
   Look a bit like a TWhatsApp dialog bubble.
   Has a nice shadow effect.
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Types, System.Classes, System.Math, System.UITypes,
  FMX.StdCtrls, FMX.Effects, FMX.Graphics, FMX.Types, FMX.Controls, FMX.Objects, FMX.Controls.Presentation;

TYPE
  TAutoHeightRectangle = class(TRectangle)
  private
    FInitialWidth: Single; // To track width change
    FTextLabel: TLabel;    // The internal control that holds the text
    FText: string;
    procedure SetText(const Value: string);
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Text: string read FText write SetText;
  end;


procedure Register;


IMPLEMENTATION

// A small constant tolerance to detect width changes, replacing the internal FmxSizeTolerance.
const
  CResizeTolerance  = 0.5; // Use a small, reasonable value for float comparison
  CTextHeightBuffer = 1.0; // Small buffer to prevent text from being cut (e.g., antialiasing issues)


constructor TAutoHeightRectangle.Create(AOwner: TComponent);
var
  Shadow: TShadowEffect;
begin
  inherited Create(AOwner);
  FInitialWidth := -1; // Initialize with an invalid width

  // The Bubble
  Fill.Color   := $FFE4F3E2; // Matches xFFE4F3E2 (WhatsApp green/user answer) [cite: 3]
  Stroke.Color := $FF006300; // Matches xFF006300 [cite: 4]
  Stroke.Kind  := TBrushKind.Solid;

  XRadius := 11;   // Corner size
  YRadius := 11;

  // Padding based on DFM Rectangle1 [cite: 3]
  Padding.Rect := TRectF.Create(3, 3, 3, 3);

  // Margins
  Margins.Rect := TRectF.Create(3, 3, 3, 3);
  
  // Shadow effect
  Shadow := TShadowEffect.Create(Self);
  Shadow.Parent := Self;
  Shadow.Distance  := 3.0;
  Shadow.Direction := 45.0;
  Shadow.Softness  := 0.30;
  Shadow.Opacity   := 0.60;
  Shadow.ShadowColor := TAlphaColor($FF000000); // claBlack 

  // Create the internal TLabel
  FTextLabel := TLabel.Create(Self);
  FTextLabel.Parent   := Self;
  FTextLabel.Align    := TAlignLayout.Top; // This is important!
  FTextLabel.AutoSize := True;             // The key for the label to wrap and set its height
  FTextLabel.WordWrap := True;
  FTextLabel.HitTest  := False;
  FTextLabel.TextSettings.Font.Size := 20;
end;


procedure TAutoHeightRectangle.SetText(const Value: string);
begin
  if FText <> Value then
    begin
      FText := Value;
      FTextLabel.Text := Value;
      Resize;  // Allow the internal FTextLabel to update its size based on the new text.
    end;
end;


procedure TAutoHeightRectangle.Resize;
var
  LMinHeight: Single;
begin
  // Call inherited first to let the internal TLabel resize and wrap
  inherited Resize;

  // Check if the width has actually changed since the last resize.
  if Abs(Self.Width - FInitialWidth) > CResizeTolerance
  then
    begin
      FInitialWidth := Self.Width; // Update the reference width

      // Compute the required height for the TRectangle (Bubble)
      // Height = Child Height + Top Padding + Bottom Padding + Buffer
      LMinHeight := FTextLabel.Height + Self.Padding.Top + Self.Padding.Bottom + CTextHeightBuffer;

      // Set the new height, overriding any parent/alignment setting.
      Self.Height := Ceil(LMinHeight);
    end;
end;


procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TAutoHeightRectangle]);
end;


end.
