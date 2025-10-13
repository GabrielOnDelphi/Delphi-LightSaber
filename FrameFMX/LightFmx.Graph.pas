UNIT LightFmx.Graph;

{-------------------------------------------------------------------------------------------------------------
    GabrielMoraru.com
    2025.09
--------------------------------------------------------------------------------------------------------------
    Graphics utilities for FMX
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Types, System.UITypes, FMX.Graphics, FMX.Types, FMX.Objects;

procedure GetImageResolution(FileName: string; Out Width, Height: Integer);
procedure LoadImage         (FileName: string; Image: TImage; Color: TAlphaColor= TAlphaColorRec.DeepPink);

procedure FillBitmap   (BMP: TBitmap; Color: TAlphaColor);
function  CreateBitmap (Width, Height: Integer; BkgClr: TAlphaColor= TAlphaColorRec.Black): TBitmap;

function  CropBitmap   (InputBMP: TBitmap; CropRect: TRectF): TBitmap;        overload;
function  CropBitmap   (FileName: string;  CropRect: TRectF): TBitmap;        overload;
procedure CropBitmap   (FileName: string;  CropRect: TRectF; Image: TImage);  overload;



IMPLEMENTATION

USES
   LightFmx.Common.AppData;



procedure GetImageResolution(FileName: string; Out Width, Height: Integer);
VAR
   Bmp: TBitmap;
begin
  Width := 0;
  Height:= 0;
  if NOT FileExists(FileName) then EXIT;

  TRY
    try
      Bmp:= TBitmap.CreateFromFile(FileName);
      Width := Bmp.Width;
      Height:= Bmp.Height;
    finally
      FreeAndNil(Bmp);
    end;
  EXCEPT
    AppData.RamLog.AddError('Cannot open image: '+ FileName);
  END;
end;


// Load a file into TImage. If file is not found, it returns a pink image anyway.
procedure LoadImage(FileName: string; Image: TImage; Color: TAlphaColor= TAlphaColorRec.DeepPink);
VAR Bitmap: TBitmap;
begin
  if FileExists(FileName)
  then
    begin
      Bitmap:= TBitmap.Create;
      Bitmap.LoadFromFile(FileName);       // Supports JPG, PNG, BMP
    end
  else
    Bitmap:= CreateBitmap(77, 77, Color);  // Fake the image for debugging

  TRY
    Image.Bitmap.Assign(Bitmap);
  FINALLY
    FreeAndNil(Bitmap);
  END;
end;



{-------------------------------------------------------------------------------
  TBitmap Cropping (OPTIMIZED using CopyFromBitmap)
-------------------------------------------------------------------------------}

// We copy the CropRect area from InputBMP to the start of ResultBmp.
function CropBitmap(InputBMP: TBitmap; CropRect: TRectF): TBitmap;
begin
  Assert((CropRect.Width > 0) and (CropRect.Height > 0));
  Result:= TBitmap.Create;
  TRY
    Result.SetSize(Round(CropRect.Width), Round(CropRect.Height));  // Match the area being cropped
    Result.CopyFromBitmap(InputBMP, CropRect.Round, 0, 0);          // Using CopyFromBitmap for faster, pure pixel copying.
  EXCEPT
    Result.Free;
    RAISE;
  END;
end;


function CropBitmap(FileName: string; CropRect: TRectF): TBitmap;
VAR SrcBmp: TBitmap;
begin
  SrcBmp := TBitmap.Create;
  try
    SrcBmp.LoadFromFile(FileName);
    Result:= CropBitmap(SrcBmp, CropRect);
  finally
    SrcBmp.Free;
  end;
end;


// Load the image from file, then crops it then assign it to a TImage
procedure CropBitmap(FileName: string; CropRect: TRectF; Image: TImage);
VAR CropBmp: TBitmap;
begin
  Assert(NOT CropRect.IsEmpty, 'BoundBox not defined!');

  CropBmp:= CropBitmap(FileName, CropRect);
  try
    Image.Bitmap.Assign(CropBmp);    // Assign the cropped bitmap to the TImage control (done only once)
  finally
    CropBmp.Free;
  end;
end;



{ Fill bitmap with the specified color }
procedure FillBitmap(BMP: TBitmap; Color: TAlphaColor);
var R: TRectF;
begin
  BMP.Canvas.BeginScene;
  try
    R := TRectF.Create(0, 0, BMP.Width, BMP.Height);
    BMP.Canvas.Fill.Color:= Color;
    BMP.Canvas.Fill.Kind := TBrushKind.Solid;
    BMP.Canvas.FillRect(R, 0, 0, [], 1);
  finally
    BMP.Canvas.EndScene;
  end;
end;


function CreateBitmap(Width, Height: Integer; BkgClr: TAlphaColor= TAlphaColorRec.Black): TBitmap;
begin
  TRY
    Result:= TBitmap.Create(Width, Height);
  EXCEPT
    FreeAndNil(Result); { Free result ONLY in case of error }
    RAISE;
  END;

  FillBitmap(Result, BkgClr);
end;




end.


