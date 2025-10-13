UNIT LightFmx.Visual.AutoSizeBox;

{-------------------------------------------------------------------------------------------------------------
   GabrielMoraru.com
   2025.07
--------------------------------------------------------------------------------------------------------------
   A TRectangle that automatically adjusts its height to fit its internal component (text or image).
   Has the equivalent of an AutoSize property (but not exposed/published)
   Looks a bit like a TWhatsApp dialog bubble, with a nice shadow effect.

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
  WhatsAppGreen     = $FFE4F3E2;
  WhatsAppGrey      = $FFEFEFEF;

TYPE
  TBoxType = (bxUser, bxModel);

TYPE
  TAutoSizeBox = class(TRectangle)
  private
    FBoxType: TBoxType;  
    procedure SetBoxType(Value: TBoxType);
  protected
    ParentWidthCache: Single; // Caches Self.Width to track width change
    procedure Resize; override;
    function GetParentContentWidth: Single;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateSize; virtual; abstract;
    property BoxType: TBoxType read FBoxType write SetBoxType;
  end;




IMPLEMENTATION



constructor TAutoSizeBox.Create(AOwner: TComponent);
var
  Shadow: TShadowEffect;
begin
  inherited Create(AOwner);

  // Flag set to -1 ensures the first system Resize event runs the calculation.
  ParentWidthCache := -1; // Initialize the cache
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

  FBoxType := bxModel; // Default alignment


end;



procedure TAutoSizeBox.SetBoxType(Value: TBoxType);
begin
  //if FBoxType = Value then EXIT; // Allow re-setting to force margin/color update
  FBoxType := Value;

  case FBoxType of
    bxUser:
      begin
        // Wide LEFT margin pushes the bubble to the right (User side)
        Margins.Rect := TRectF.Create(40, 5, 5, 5);
        Fill.Color   := WhatsAppGreen;            // WhatsApp green
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
var CurrentParentWidth: Single;
begin
  // 1. Call inherited first. This is where FMX is forced to update FTextLabel.Height based on the current Self.Width and the wrapped text.
  inherited Resize;

  // 2. Only re-run UpdateSize if the *Parent's* width has changed (e.g., window resize, scrollbar change).
  // The goal is to detect an external size change, not a change caused by us setting our own Width/Height.
  if Assigned(Parent) then
  begin
    CurrentParentWidth:= (Parent as TControl).Width;

    // Check if parent width has changed (or if it's the initial run)
    if (ParentWidthCache < 0)
    OR (Abs(CurrentParentWidth - ParentWidthCache) > CResizeTolerance)
    then UpdateSize;   // UpdateSize recalculates Self.Width and Self.Height based on CurrentParentWidth, and then updates FParentWidthCache at the end of its execution, breaking the loop.
  end;
end;


function TAutoSizeBox.GetParentContentWidth: Single;
begin
  if (Parent = nil) or ((Parent as TControl).Width <= 0)
  then Result:= 0 // Not ready
  else Result := (Parent as TControl).Width - Self.Margins.Left - Self.Margins.Right;     // The width available for content is the Parent's width minus our margins.
end;


end.
