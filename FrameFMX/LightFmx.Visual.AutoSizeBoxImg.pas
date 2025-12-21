UNIT LightFmx.Visual.AutosizeBoxImg;

{-------------------------------------------------------------------------------------------------------------
    GabrielMoraru.com
    2025.07
--------------------------------------------------------------------------------------------------------------
    A TRectangle that automatically adjusts its size to fit its internal image.
    Derives from TAutoSizeBox.
    Looks a bit like a TWhatsApp dialog bubble, with a nice shadow effect.

    Demo: LightSaber\Demo\FMX\Demo AutoHeightRectangle\FMX_Demo_AutoSizeRect.dpr
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Types, System.Classes, System.Math,
  FMX.Graphics, FMX.Types, FMX.Controls, FMX.Objects,
  LightFmx.Visual.AutoSizeBox, LightFmx.Graph;
{
type
  TControlHack = class(TControl);   }

TYPE
  TAutosizeBoxImg = class(TAutoSizeBox)
  private
    FImage: TImage;
    FBoundBox: TRectF;   //used?
    FUpdatingSize: Boolean;
  protected
    CONST MaxImageWidthRatio = 0.95;      // Maximum image width ratio relative to the parent's content box
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadImage(FileName: string; aBoundBox: TRectF);
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


// Crop the figure bound box from the input image
procedure TAutosizeBoxImg.LoadImage(FileName: string; aBoundBox: TRectF);
begin
  FBoxType := bxModel;    // Image bubble is always 'Model' side
  FBoundBox:= aBoundBox;  //used?
  CropBitmap(FileName, aBoundBox, FImage);

  //if (Parent is TControl) then TControlHack(Parent).Realign;

  UpdateSize;             // After loading the image, recalculate the size based on the new image dimensions  // In FMX, setting Text often triggers an internal RGN_Change (Region Change) message which leads to a Resize, so we just set the flag and let the system handle it.
end;


// Core logic: scale img proportionally and set bubble size.
// Called whenever the image loads, or when the parent container resizes.

procedure TAutosizeBoxImg.UpdateSize;
var
  ParentContentWidth: Single;
  MaxWidth: Single;
  NewWidth, NewHeight: Single;
begin
  Assert(Parent <> NIL);
  if FImage.Bitmap.IsEmpty then EXIT;

  if FUpdatingSize then Exit;
  FUpdatingSize:= True;
  try
    ParentContentWidth:= GetParentContentWidth;

    // Exit if parent is not ready
    if ParentContentWidth <= 0 then
      begin
        Width := 200;        // Fallback size if image is missing or parent is not ready
        Height:= 150;
        Exit;
      end;

    // Calculate the actual MaxWidth for the image based on the parent's available content area.
    MaxWidth := ParentContentWidth * MaxImageWidthRatio;
    MaxWidth := Max(50, MaxWidth); // Ensure minimum width

    // 1. Calculate the scaled dimensions of the image
    CurScale := Min(1.0, MaxWidth / FImage.Bitmap.Width);

    // Calculate final bubble dimensions
    NewWidth  := Ceil(FImage.Bitmap.Width  * CurScale) + Padding.Left + Padding.Right;
    NewHeight := Ceil(FImage.Bitmap.Height * CurScale) + Padding.Top  + Padding.Bottom;

    Margins.Right:= ParentContentWidth - NewWidth;

    SetBounds(Position.x, Position.y, NewWidth, NewHeight);
  finally
    FUpdatingSize:= False;
  end;
end;


procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TAutosizeBoxImg]);
end;


end.
