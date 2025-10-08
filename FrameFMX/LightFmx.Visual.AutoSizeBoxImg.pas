UNIT LightFmx.Visual.AutoSizeBoxImg;

{-------------------------------------------------------------------------------------------------------------
    GabrielMoraru.com
    2025.07
--------------------------------------------------------------------------------------------------------------
    A TRectangle that automatically adjusts its size to fit a proportionally scaled image.
    *** Designed as a drop-in replacement for TAutoHeightRectangle, using only Align=Top
    *** and margins for horizontal positioning.

     Now UpdateSize relies on Parent.Width.

    Demo: Test Bubble\BubbleTest.dpr
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Types, System.Classes, System.Math, System.UITypes,
  FMX.StdCtrls, FMX.Effects, FMX.Graphics, FMX.Types, FMX.Controls, FMX.Objects, FMX.Controls.Presentation,
  FMX.TextLayout, LightFmx.Visual.AutoSizeBoxTxt, LightFmx.Graph;

TYPE

  TAutoImageBubble = class(TRectangle)
  private
    FImage: TImage;
    FBoxType: TBoxType;
    FBoundBox: TRect;
    FParentWidthCache: Single; // Caches parent width to detect container resize
    procedure SetBoxType(Value: TBoxType);
    procedure UpdateSize;
    function GetParentContentWidth: Single;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadImage(FileName: string; aBoundBox: TRect);
    property BoundBox: TRect read FBoundBox;
    property BoxType: TBoxType read FBoxType write SetBoxType;

    const MaxImageWidthRatio = 0.90;      // Maximum image width ratio relative to the parent's content box
  end;


procedure Register;


IMPLEMENTATION

CONST
  WhatsAppGrey      = $FFEFEFEF;


constructor TAutoImageBubble.Create(AOwner: TComponent);
var
  Shadow: TShadowEffect;
begin
  inherited Create(AOwner);
  FParentWidthCache := -1; // Initialize the cache
  
  Align := TAlignLayout.Top; // CRITICAL: Always Align Top to stack vertically in a VertScrollBox

  // The Bubble
  Fill.Color   := WhatsAppGrey;
  Stroke.Color := $FF006300;
  Stroke.Kind  := TBrushKind.None; // Changed back to None for standard WhatsApp look
  XRadius      := 11;
  YRadius      := 11;
  Padding.Rect := TRectF.Create(5, 5, 5, 5);
  Margins.Rect := TRectF.Create(5, 5, 5, 5); // Base margins, adjusted in SetBoxType

  // Shadow effect
  Shadow := TShadowEffect.Create(Self);
  Shadow.Parent := Self;
  Shadow.Distance  := 3.0;
  Shadow.Direction := 45.0;
  Shadow.Softness  := 0.30;
  Shadow.Opacity   := 0.60;
  Shadow.ShadowColor := TAlphaColor($FF000000);

  // Internal Image control
  FImage := TImage.Create(Self);
  FImage.Parent := Self;
  FImage.Align := TAlignLayout.Client;
  FImage.WrapMode := TImageWrapMode.Fit; // Fit the image within the control
  FImage.HitTest := False;
  FImage.Stored := False;

  Self.BoxType := bxModel; // Default alignment
end;


function TAutoImageBubble.GetParentContentWidth: Single;
begin
  if (Parent = nil) or ((Parent as TControl).Width <= 0)
  then Result := 0 // Not ready
  else
    // The width available for content is the Parent's width minus our margins.
    Result := (Parent as TControl).Width - Self.Margins.Left - Self.Margins.Right;
end;


// Crop the figure bound box from the input image
procedure TAutoImageBubble.LoadImage(FileName: string; aBoundBox: TRect);
begin
  FBoundBox:= aBoundBox;
  CropBitmap(FileName, aBoundBox, FImage);
  UpdateSize;    // After loading the image, recalculate the size based on the new image dimensions
end;


// Core logic: scale proportionally and set bubble size.
// This runs whenever the image loads, or when the parent container resizes.
procedure TAutoImageBubble.UpdateSize;
var
  ParentContentWidth: Single;
  MaxWidth: Single;
  Scale   : Single;
  ImgW, ImgH: Integer;
begin
  ParentContentWidth := GetParentContentWidth;
  
  // Exit if parent is not ready or image is empty
  if (ParentContentWidth <= 0) or (FImage.Bitmap.IsEmpty) then
  begin
    // Fallback size if image is missing or parent is not ready
    Self.Width := 200 + Self.Padding.Left + Self.Padding.Right;
    Self.Height:= 150 + Self.Padding.Top + Self.Padding.Bottom;
    Exit;
  end;

  ImgW := FImage.Bitmap.Width;
  ImgH := FImage.Bitmap.Height;

  // Calculate the actual MaxWidth for the image based on the parent's available content area.
  MaxWidth := ParentContentWidth * MaxImageWidthRatio;
  MaxWidth := Max(50, MaxWidth); // Ensure minimum width

  // 1. Calculate the scaled dimensions of the image
  Scale := Min(1.0, MaxWidth / ImgW);
  
  // Calculate final bubble dimensions
  Self.Width  := Ceil(ImgW * Scale) + Self.Padding.Left + Self.Padding.Right;
  Self.Height := Ceil(ImgH * Scale) + Self.Padding.Top + Self.Padding.Bottom;
  
  // CRITICAL: Update the cache *after* setting the size, using the parent's current width.
  // This is what prevents the loop, as Resize will now compare the new width to this cached value.
  if Assigned(Parent) then
    FParentWidthCache := (Parent as TControl).Width;
end;


procedure TAutoImageBubble.Resize;
var
  CurrentParentWidth: Single;
begin
  // 1. Call inherited first. This is where FMX is forced to update FTextLabel.Height based on the current Self.Width and the wrapped text.
  inherited Resize;

  // 2. Only re-run UpdateSize if the *Parent's* width has changed (e.g., window resize, scrollbar change).
  // The goal is to detect an external size change, not a change caused by us setting our own Width/Height.
  if Assigned(Parent) then
  begin
    CurrentParentWidth:= (Parent as TControl).Width;
    
    // Check if parent width has changed (or if it's the initial run)
    if (FParentWidthCache < 0) or (Abs(CurrentParentWidth - FParentWidthCache) > CResizeTolerance) then
    begin
      // UpdateSize recalculates Self.Width and Self.Height based on CurrentParentWidth,
      // and then updates FParentWidthCache at the end of its execution, breaking the loop.
      UpdateSize;
    end;
  end;
end;


procedure TAutoImageBubble.SetBoxType(Value: TBoxType);
begin
  //if FBoxType = Value then EXIT;
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
        // Wide RIGHT margin pushes the bubble to the left (Model side)
        Margins.Rect := TRectF.Create(5, 5, 40, 5);
        Fill.Color   := WhatsAppGrey;
        Stroke.Kind  := TBrushKind.None;
      end;
  end;
  
  // Changing margins may change ParentContentWidth, so force a size update.
  UpdateSize;
end;


procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TAutoImageBubble]);
end;


end.
