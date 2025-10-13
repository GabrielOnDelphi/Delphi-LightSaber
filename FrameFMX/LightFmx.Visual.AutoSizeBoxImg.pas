UNIT LightFmx.Visual.AutoSizeBoxImg;

{-------------------------------------------------------------------------------------------------------------
    GabrielMoraru.com
    2025.07
--------------------------------------------------------------------------------------------------------------
    A TRectangle that automatically adjusts its size to fit its internal image.
    Derives from TAutoSizeBox.
    Looks a bit like a TWhatsApp dialog bubble, with a nice shadow effect.

    Demo: Test Bubble\BubbleTest.dpr
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Types, System.Classes, System.Math,
  FMX.Graphics, FMX.Types, FMX.Controls, FMX.Objects, FMX.Controls.Presentation,
  LightFmx.Visual.AutoSizeBox, LightFmx.Graph;

TYPE
  TAutoImageBubble = class(TAutoSizeBox)
  private
    FImage: TImage;
    FBoundBox: TRectF;
  protected
    CONST MaxImageWidthRatio = 0.95;      // Maximum image width ratio relative to the parent's content box
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadImage(FileName: string; aBoundBox: TRectF);
    procedure UpdateSize;  override;
  end;


procedure Register;


IMPLEMENTATION



constructor TAutoImageBubble.Create(AOwner: TComponent);
begin
  inherited Create(AOwner); 

  // Internal Image control
  FImage := TImage.Create(Self);
  FImage.Parent := Self;
  FImage.Align := TAlignLayout.Client;
  FImage.WrapMode := TImageWrapMode.Fit; // Fit the image within the control
  FImage.HitTest := False;
  FImage.Stored := False;
end;


// Crop the figure bound box from the input image
procedure TAutoImageBubble.LoadImage(FileName: string; aBoundBox: TRectF);
begin
  FBoundBox:= aBoundBox;
  CropBitmap(FileName, aBoundBox, FImage);
  UpdateSize;    // After loading the image, recalculate the size based on the new image dimensions  // In FMX, setting Text often triggers an internal RGN_Change (Region Change) message which leads to a Resize, so we just set the flag and let the system handle it.
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
    Width := 200 + Padding.Left + Padding.Right;
    Height:= 150 + Padding.Top  + Padding.Bottom;
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
  Width  := Ceil(ImgW * Scale) + Padding.Left + Padding.Right;
  Height := Ceil(ImgH * Scale) + Padding.Top  + Padding.Bottom;
  
  // CRITICAL: Update the cache *after* setting the size, using the parent's current width.
  // This is what prevents the loop, as Resize will now compare the new width to this cached value.
  if Assigned(Parent)
  then ParentWidthCache := (Parent as TControl).Width;
end;





procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TAutoImageBubble]);
end;


end.
