UNIT LightFmx.Visual.AutosizeBoxText;

{=============================================================================================================
   2026.01.31
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Extends TAutoSizeBox with text/caption capabilities using an internal TLabel.
   The box automatically adjusts its height based on wrapped text content.
   Use MakeTextBubble() for quick creation of chat-style message bubbles.
=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Classes, System.Math,
  FMX.StdCtrls, FMX.Graphics, FMX.Types, FMX.Controls, FMX.Objects,
  LightFmx.Visual.AutoSizeBox;

TYPE
  TAutosizeBoxText = class(TAutoSizeBox)
  private
    FTextLabel: TLabel;
    FText: string;
    procedure SetText(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    property Text: string read FText write SetText;
    procedure UpdateSize; override;
  end;


function MakeTextBubble(Parent: TControl; const Text: string; BoxType: TBoxType): TAutosizeBoxText;
procedure Register;


IMPLEMENTATION


constructor TAutosizeBoxText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Internal label for displaying text
  FTextLabel:= TLabel.Create(Self);
  FTextLabel.Parent  := Self;
  FTextLabel.Align   := TAlignLayout.Top;  // Top alignment ensures proper height calculation with AutoSize
  FTextLabel.AutoSize:= TRUE;
  FTextLabel.WordWrap:= TRUE;
  FTextLabel.HitTest := FALSE;
  FTextLabel.Stored  := FALSE;  // Prevents Form Designer from serializing this internal component. Without this, loading a form would create duplicate labels (one from FMX, one from constructor).
  FTextLabel.TextSettings.Font.Size:= 20;

  // Set initial demo text directly. Do NOT call SetText here to avoid premature UpdateSize.
  FText:= 'Self-sizing WhatsApp-like bubble. '+
    'The TAutosizeBoxText component detects the wrapped height of its internal TLabel and adjusts its own Height accordingly when the parent width changes. '+#13#10+
    'This eliminates manual size calculation from the form unit.';
  FTextLabel.Text:= FText;
end;


{ Sets the bubble text and triggers height recalculation.
  Note: In FMX, setting Text may trigger internal region change messages. }
procedure TAutosizeBoxText.SetText(const Value: string);
begin
  if FText <> Value then
    begin
      FText:= Value;
      FTextLabel.Text:= Value;
      // In FMX, setting Text often triggers an internal RGN_Change (Region Change) message which leads to a Resize
      UpdateSize;
    end;
end;


{ Calculates and sets the box height based on label height plus padding.
  Uses Ceil to ensure the text is never clipped. }
procedure TAutosizeBoxText.UpdateSize;
VAR MinHeight: Single;
begin
  if Width <= 0 then EXIT;
  MinHeight:= FTextLabel.Height + Self.Padding.Top + Self.Padding.Bottom + CTextHeightBuffer;
  Self.Height:= Ceil(MinHeight);
end;






{------------------------------------------------------------------------------------------------------------
   FACTORY FUNCTION
------------------------------------------------------------------------------------------------------------}

{ Creates a text bubble and adds it to the parent container.
  Position.Y is set high to ensure it appears at the bottom when using Top alignment. }
function MakeTextBubble(Parent: TControl; const Text: string; BoxType: TBoxType): TAutosizeBoxText;
begin
  Result:= TAutosizeBoxText.Create(Parent);
  Result.Parent:= Parent;
  Result.Stored:= FALSE;
  Result.BoxType:= BoxType;
  Result.Text:= Text;
  Result.Position.Y:= 99999999;  // Push to bottom of parent (Top-aligned controls sort by Y position)
  Result.UpdateSize;
end;


procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TAutosizeBoxText]);
end;


end.
