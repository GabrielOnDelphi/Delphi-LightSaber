UNIT LightFmx.Visual.AutoSizeBoxTxt;

{-------------------------------------------------------------------------------------------------------------
   GabrielMoraru.com
   2025.07
--------------------------------------------------------------------------------------------------------------
   A TRectangle that automatically adjusts its height to fit its internal text.
   Has the equivalent of an AutoSize property (but not exposed/published)
   Look a bit like a TWhatsApp dialog bubble.

   Demo: Test Bubble\BubbleTest.dpr
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Types, System.Classes, System.Math,
  FMX.StdCtrls, FMX.Graphics, FMX.Types, FMX.Controls, FMX.Objects, FMX.Controls.Presentation,
  LightFmx.Visual.AutoSizeBox;

TYPE
  TAutoSizeBoxTxt = class(TAutoSizeBox)
  private
    FTextLabel: TLabel;    // The internal control that holds the text
    FText: string;
    procedure SetText(const Value: string);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    property Text: string read FText write SetText;
    procedure UpdateSize; override;
  end;


procedure Register;


IMPLEMENTATION



constructor TAutoSizeBoxTxt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner); 

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
      UpdateSize;    // After loading the image, recalculate the size based on the new image dimensions  // In FMX, setting Text often triggers an internal RGN_Change (Region Change) message which leads to a Resize, so we just set the flag and let the system handle it.
    end;
end;


// Perform the actual height adjustment.
procedure TAutoSizeBoxTxt.UpdateSize;
VAR LMinHeight: Single;
begin
  ParentWidthCache := GetParentContentWidth;
  
  // Width must be set for text wrapping/height calculation to be meaningful
  if Width <= 0 then Exit; 

  // Compute the required height for the TRectangle (Bubble)
  // FTextLabel.Height is now guaranteed to be correct after inherited Resize has run.
  LMinHeight := FTextLabel.Height + Self.Padding.Top + Self.Padding.Bottom + CTextHeightBuffer;

  // Set the new height, overriding any parent/alignment setting.
  // This sets Self.Height, which does NOT trigger Resize via a width change, thus avoiding a loop.
  Self.Height := Ceil(LMinHeight);

  // CRITICAL: Update the cache *after* setting the size, using the parent's current width.
  // This is what prevents the loop, as Resize will now compare the new width to this cached value.
  if Assigned(Parent)
  then ParentWidthCache:= (Parent as TControl).Width;
end;




procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TAutoSizeBoxTxt]);
end;


end.
