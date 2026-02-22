UNIT LightFmx.Visual.AutosizeBox;

{=============================================================================================================
   2026.01.31
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   A TRectangle that automatically adjusts its height to fit its internal content.
   Styled like a WhatsApp dialog bubble with rounded corners and shadow.
   Implements auto-sizing behavior (not exposed/published as a property).
   Intended for AI chat-style conversations.

   Abstract class - do not instantiate directly. Contains abstract method "UpdateSize".
   Use TAutosizeBoxText or TAutoImageBubble instead.

   Demo: LightSaber\Demo\FMX\Demo AutoHeightRectangle\FMX_Demo_AutoSizeRect.dpr
=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Types, System.Classes, System.UITypes,
  FMX.Effects, FMX.Graphics, FMX.Types, FMX.Controls, FMX.Objects, FMX.Controls.Presentation;

CONST
  CResizeTolerance  = 1;               // Use a small, reasonable value for float comparison
  CTextHeightBuffer = 1.0;             // Small buffer to prevent text from being cut (e.g., antialiasing issues)
  CShadowBottomExtent = 6;             // Shadow vertical extent below bubble: Distance(3)*Sin(45°) + Softness blur ≈ 6px. Used by spacer in scroll containers.
  WhatsAppGreen     = $FFE4F3E2;
  WhatsAppGrey      = $FFEFEFEF;
  WhatsAppBlue      = $FFE3F2FD;       // Light blue for lesson content bubbles

TYPE
  TBoxType = (bxUser, bxModel, bxContent);

TYPE
  TAutoSizeBox = class(TRectangle)
  private
    procedure setBoxType(Value: TBoxType);
  protected
    FBoxType: TBoxType;
    procedure Resize; override;
    function getParentContentWidth: Single;
  public
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
  Fill.Color   := WhatsAppGreen;
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
  Shadow.ShadowColor := TAlphaColorRec.Black;
  Shadow.Stored:= False;  // Prevent streaming to FMX file (avoids duplicate children)

  FBoxType:= bxModel; // Default alignment
end;


{ Aligns the bubble to left/right and sets color based on BoxType.
  bxUser: Right-aligned (user messages), green background.
  bxModel: Left-aligned (AI/bot messages), grey background. }
procedure TAutoSizeBox.setBoxType(Value: TBoxType);
begin
  //if FBoxType = Value then EXIT; // Allow re-setting to force margin/color update
  FBoxType:= Value;
  Stroke.Kind:= TBrushKind.None;

  case FBoxType of
    bxUser:
      begin
        // Wide LEFT margin pushes the bubble to the right (User side)
        Margins.Rect:= TRectF.Create(40, 5, 5, 5);
        Fill.Color  := WhatsAppGreen;
      end;
    bxModel:
      begin
        // Wide RIGHT margin pushes the bubble to the left (Bot side)
        Margins.Rect:= TRectF.Create(5, 5, 40, 5);
        Fill.Color  := WhatsAppGrey;
      end;
    bxContent:
      begin
        // Left-aligned like bxModel, but light blue for lesson content
        Margins.Rect:= TRectF.Create(5, 5, 40, 5);
        Fill.Color  := WhatsAppBlue;
      end;
  end;

  // Margins change affects effective width, so recalculate height
  UpdateSize;
end;


{ Called when the control is resized (e.g., parent width changes).
  Triggers UpdateSize to recalculate our height based on new available width.
  Note: UpdateSize may cause a feedback loop; implementations should guard against infinite recursion. }
procedure TAutoSizeBox.Resize;
begin
  inherited Resize;
  if Parent = NIL then EXIT;
  UpdateSize;
end;


{ Returns the available width for content (parent width minus our margins).
  Used to determine how much horizontal space we have for text wrapping. }
function TAutoSizeBox.getParentContentWidth: Single;
begin
  Result:= 0;
  if Parent = NIL then EXIT;
  Result:= (Parent as TControl).Width - Self.Margins.Left - Self.Margins.Right;

  if Result < 0
  then Result:= 0;
end;


end.
