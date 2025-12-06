UNIT LightFmx.Graph;

{-------------------------------------------------------------------------------------------------------------
    GabrielMoraru.com
    2025.09
--------------------------------------------------------------------------------------------------------------
    Graphics utilities for FMX


    AI said: TBitmapCodecManager looks at the file extension you provided (.jpg).
    It finds the registered JPEG encoder for the specific platform (Windows WIC or Android Bitmap API) and compresses the image data into valid JPEG format before writing it to the disk.
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Types, System.UITypes, FMX.Graphics, FMX.Types, FMX.Objects;

procedure GetImageResolution(FileName: string; Out Width, Height: Integer);
procedure LoadImage         (FileName: string; Image: TImage; Color: TAlphaColor= TAlphaColorRec.DeepPink);   overload;
function  LoadImage         (FileName: string): TBitmap;                                                      overload;

procedure FillBitmap   (BMP: TBitmap; Color: TAlphaColor);
function  CreateBitmap (Width, Height: Integer; BkgClr: TAlphaColor= TAlphaColorRec.Black): TBitmap;

function  CropBitmap   (InputBMP: TBitmap; CropRect: TRectF): TBitmap;        overload;
function  CropBitmap   (FileName: string;  CropRect: TRectF): TBitmap;        overload;
procedure CropBitmap   (FileName: string;  CropRect: TRectF; Image: TImage);  overload;



IMPLEMENTATION

USES
   LightFmx.Common.AppData;



function LoadImage(FileName: string): TBitmap;
begin
  if (FileName = '') or (NOT FileExists(FileName)) then EXIT(NIL);

  try
    Result:= TBitmap.CreateFromFile(FileName);
  except
    on E: Exception do
    begin
      AppData.RamLog.AddError('LoadImage failed: ' + E.Message);
      FreeAndNil(Result);
    end;
  end;
end;


// Load a file into TImage. If file is not found, it returns a pink image.
// Supports JPG, PNG, BMP
procedure LoadImage(FileName: string; Image: TImage; Color: TAlphaColor= TAlphaColorRec.DeepPink);
VAR Bitmap: TBitmap;
begin
  // Check file existence first
  if FileExists(FileName) then
  begin
    Bitmap := LoadImage(FileName); // Use the function above for consistency
  end
  else
    Bitmap := nil;

  if Bitmap = nil then
    Bitmap := CreateBitmap(77, 77, Color);  // Fake the image for debugging

  TRY
    Image.Bitmap.Assign(Bitmap);
  FINALLY
    FreeAndNil(Bitmap);
  END;
end;


// NOT TESTED!
function LoadThumbnail(CONST FileName: string; TargetWidth, TargetHeight: Single): TBitmap;
begin
  Result := TBitmap.Create;
  try
    if FileExists(FileName) then
    begin
      // 1. Try to load a thumbnail (Hardware efficient scaling)
      // This calculates the nearest power-of-2 downsampling to fit the target
      Result.LoadThumbnailFromFile(FileName, TargetWidth, TargetHeight);
    end;
  except
    on E: Exception do
    begin
      // Handle corrupt files gracefully
      AppData.RamLog.AddError('SafeLoadBitmap failed: ' + E.Message);
      FreeAndNil(Result); // Return nil if failed
    end;
  end;
end;


(* original code
   works on windows with PNGs but not on android. maybe the jpg I try to open is corrupted?

// Supports JPG, PNG, BMP
procedure GetImageResolution_old(FileName: string; Out Width, Height: Integer);
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
end; *)


procedure GetImageResolution(FileName: string; out Width, Height: Integer);
var
  Size: TPointF;
begin
  Width := 0;
  Height := 0;
  if not FileExists(FileName) then Exit;

  try
    // TBitmapCodecManager is the fastest way to get size without decoding the whole image
    Size := TBitmapCodecManager.GetImageSize(FileName);
    Width := Trunc(Size.X);
    Height := Trunc(Size.Y);
  except
    on E: Exception do
    begin
      AppData.RamLog.AddError('Cannot get image resolution for ' + FileName + ': ' + E.Message);
      
      // Fallback: Try full load if CodecManager fails (rare but possible with some formats)
      try
        var Bmp := TBitmap.CreateFromFile(FileName);
        try
          Width := Bmp.Width;
          Height:= Bmp.Height;
        finally
          Bmp.Free;
        end;
      except
         // Give up
      end;
    end;
  end;
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
  SrcBmp := LoadImage(FileName); // Use our safe loader
  if SrcBmp = nil then Exit(nil);
  
  try
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
    if CropBmp <> nil then
      Image.Bitmap.Assign(CropBmp)
    else
      Image.Bitmap.Clear(TAlphaColorRec.Null); 
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


