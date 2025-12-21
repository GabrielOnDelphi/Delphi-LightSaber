UNIT LightFmx.Visual.AutosizeBox;

{-------------------------------------------------------------------------------------------------------------
   GabrielMoraru.com
   2025.12
--------------------------------------------------------------------------------------------------------------
   A TRectangle that automatically adjusts its height to fit its internal text.
   Look a bit like a TWhatsApp dialog bubble.
   Implements the equivalent of an AutoSize property (but not exposed/published).
   Intended to be used in an AI discussion.

   Do not instantiate. Contains abstract method "UpdateSize". Use TAutoImageBubble instead.

   Demo: LightSaber\Demo\FMX\Demo AutoHeightRectangle\FMX_Demo_AutoSizeRect.dpr
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Types, System.Classes, System.UITypes,
  FMX.Effects, FMX.Graphics, FMX.Types, FMX.Controls, FMX.Objects, FMX.Controls.Presentation;

CONST
  CResizeTolerance  = 1;               // Use a small, reasonable value for float comparison
  CTextHeightBuffer = 1.0;             // Small buffer to prevent text from being cut (e.g., antialiasing issues)
  WhatsAppGreen     = $FFE4F3E2;
  WhatsAppGrey      = $FFEFEFEF;

TYPE
  TBoxType = (bxUser, bxModel);

TYPE
  TAutoSizeBox = class(TRectangle)
  private
    procedure setBoxType(Value: TBoxType);
  protected
    FBoxType: TBoxType;
    procedure Resize; override;
    function getParentContentWidth: Single;
  public
    CurScale: Single;
    constructor Create(AOwner: TComponent); override;
    procedure UpdateSize; virtual; abstract;
    property BoxType: TBoxType read FBoxType write setBoxType;
  end;


IMPLEMENTATION
USES FMX.Layouts;


constructor TAutosizeBox.Create(AOwner: TComponent);
var
  Shadow: TShadowEffect;
begin
  inherited Create(AOwner);

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

  FBoxType:= bxModel; // Default alignment
end;


// This aligns the box to the left/right based on Value
procedure TAutoSizeBox.setBoxType(Value: TBoxType);
begin
  //if FBoxType = Value then EXIT; // Allow re-setting to force margin/color update
  FBoxType:= Value;

  case FBoxType of
    bxUser:
      begin
        // Wide LEFT margin pushes the bubble to the right (User side)
        Margins.Rect := TRectF.Create(40, 5, 5, 5);
        Fill.Color   := WhatsAppGreen;
        Stroke.Kind  := TBrushKind.None;
      end;
    bxModel:
      begin
        // Wide RIGHT margin pushes the bubble to the left (Bot side)
        Margins.Rect := TRectF.Create(5, 5, 40, 5);
        Fill.Color   := WhatsAppGrey;
        Stroke.Kind  := TBrushKind.None;
      end;
  end;
  
  // Margins change can affect the control's effective width, so force a height update.
  UpdateSize;
end;


procedure TAutoSizeBox.Resize;
begin
  inherited Resize;
  if Parent = NIL then EXIT;
  UpdateSize;   // UpdateSize recalculates Self.Width and Self.Height based on CurrentParentWidth, and then updates FParentWidthCache at the end of its execution, breaking the loop.
end;


 // The width available for content is the Parent's width minus margins and scrollbar.
function TAutoSizeBox.getParentContentWidth: Single;
begin
  Result:= 0;
  if Parent = NIL then EXIT;
  Result:= (Parent as TControl).Width- Self.Margins.Left - Self.Margins.Right;  // fallback: use visible client area (parent width minus margins)

  if Result < 0
  then Result := 0;
end;


end.
