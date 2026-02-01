UNIT LightFmx.Visual.AutosizeBoxImg;

{=============================================================================================================
   2026.01.31
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Extends TAutoSizeBox with image display capabilities.
   The box automatically scales and adjusts its size to fit an image while maintaining aspect ratio.
   Images are cropped from a source file using a bounding box before display.

   Demo: LightSaber\Demo\FMX\Demo AutoHeightRectangle\FMX_Demo_AutoSizeRect.dpr
=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Types, System.Classes, System.Math,
  FMX.Graphics, FMX.Types, FMX.Controls, FMX.Objects,
  LightFmx.Visual.AutoSizeBox, LightFmx.Graph;

TYPE
  TAutosizeBoxImg = class(TAutoSizeBox)
  private
    FImage: TImage;
    FUpdatingSize: Boolean;
  protected
    FCurScale: Single;
    CONST MaxImageWidthRatio = 0.95;      // Maximum image width ratio relative to the parent's content box
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadImage(const FileName: string; const aBoundBox: TRectF);
    procedure UpdateSize; override;
  end;


procedure Register;


IMPLEMENTATION



constructor TAutosizeBoxImg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FImage          := TImage.Create(Self);
  FImage.Parent   := Self;
  FImage.Align    := TAlignLayout.Client;
  FImage.WrapMode := TImageWrapMode.Fit; // Fit!
  FImage.HitTest  := False;
  FImage.Stored   := False;
end;


{ Loads and crops an image from file using the specified bounding box.
  Sets BoxType to bxModel (image bubbles always appear on the model/left side).
  Triggers UpdateSize to recalculate dimensions based on the new image. }
procedure TAutosizeBoxImg.LoadImage(const FileName: string; const aBoundBox: TRectF);
begin
  FBoxType:= bxModel;
  CropBitmap(FileName, aBoundBox, FImage);
  UpdateSize;
end;


{ Scales image proportionally and sets bubble size.
  Called when the image loads or when the parent container resizes.
  Uses FUpdatingSize flag to prevent recursive calls during resize. }
procedure TAutosizeBoxImg.UpdateSize;
var
  ParentContentWidth: Single;
  MaxWidth: Single;
  NewWidth, NewHeight: Single;
begin
  if Parent = NIL then EXIT;
  if FImage.Bitmap.IsEmpty then EXIT;
  if FUpdatingSize then EXIT;

  FUpdatingSize:= True;
  try
    ParentContentWidth:= GetParentContentWidth;

    // Fallback size if parent is not ready (width not yet calculated)
    if ParentContentWidth <= 0 then
      begin
        Width := 200;
        Height:= 150;
        EXIT;
      end;

    // Calculate max image width based on parent's available content area
    MaxWidth:= ParentContentWidth * MaxImageWidthRatio;
    MaxWidth:= Max(50, MaxWidth);

    // Scale image proportionally (never upscale beyond 100%)
    FCurScale:= Min(1.0, MaxWidth / FImage.Bitmap.Width);

    // Calculate final bubble dimensions including padding
    NewWidth := Ceil(FImage.Bitmap.Width  * FCurScale) + Padding.Left + Padding.Right;
    NewHeight:= Ceil(FImage.Bitmap.Height * FCurScale) + Padding.Top  + Padding.Bottom;

    // Adjust right margin to keep bubble left-aligned (model side)
    Margins.Right:= ParentContentWidth - NewWidth;

    SetBounds(Position.X, Position.Y, NewWidth, NewHeight);
  finally
    FUpdatingSize:= False;
  end;
end;


procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TAutosizeBoxImg]);
end;


end.
