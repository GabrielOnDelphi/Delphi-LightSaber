UNIT LightFmx.Graph;

{=============================================================================================================
   2026.01.31
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
    Graphics utilities for FMX.
    Supports loading/saving images, cropping, and basic bitmap operations.

    Note: TBitmapCodecManager handles encoding/decoding based on file extension (.jpg, .png, etc).
    It uses platform-specific codecs (Windows WIC, Android Bitmap API) for compression.
=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Types, System.UITypes, FMX.Graphics, FMX.Types, FMX.Objects;

procedure GetImageResolution(FileName: string; Out Width, Height: Integer);
procedure LoadImage         (FileName: string; Image: TImage; Color: TAlphaColor= TAlphaColorRec.DeepPink);   overload;
function  LoadImage         (FileName: string): TBitmap;                                                      overload;
procedure SaveBitmap        (BMP: TBitmap; FileName: string);

procedure FillBitmap   (BMP: TBitmap; Color: TAlphaColor);
function  CreateBitmap (Width, Height: Integer; BkgClr: TAlphaColor= TAlphaColorRec.Black): TBitmap;

function  CropBitmap   (InputBMP: TBitmap; CropRect: TRectF): TBitmap;        overload;
function  CropBitmap   (FileName: string;  CropRect: TRectF): TBitmap;        overload;
procedure CropBitmap   (FileName: string;  CropRect: TRectF; Image: TImage);  overload;



IMPLEMENTATION

USES
   LightFmx.Common.AppData, FMX.DialogService;



{ Loads a bitmap from file. Returns nil if file doesn't exist or loading fails.
  Caller is responsible for freeing the returned bitmap. }
function LoadImage(FileName: string): TBitmap;
begin
  Result:= NIL;
  if (FileName = '') or (NOT FileExists(FileName))
  then EXIT;

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


{ Loads a file into TImage. If file is not found or loading fails, assigns a colored placeholder.
  Supports JPG, PNG, BMP and other formats supported by TBitmapCodecManager. }
procedure LoadImage(FileName: string; Image: TImage; Color: TAlphaColor= TAlphaColorRec.DeepPink);
var
  Bitmap: TBitmap;
begin
  Assert(Assigned(Image), 'LoadImage: Image parameter cannot be nil');

  if FileExists(FileName)
  then Bitmap:= LoadImage(FileName)
  else Bitmap:= NIL;

  if Bitmap = NIL
  then Bitmap:= CreateBitmap(77, 77, Color);  { Create placeholder for debugging }

  try
    Image.Bitmap.Assign(Bitmap);
  finally
    FreeAndNil(Bitmap);
  end;
end;


{ NOT TESTED! Internal helper - not exported.
  Loads a thumbnail using hardware-efficient scaling.
  Returns nil if file doesn't exist or loading fails.
  Caller is responsible for freeing the returned bitmap. }
function LoadThumbnail(const FileName: string; TargetWidth, TargetHeight: Single): TBitmap;
begin
  Result:= NIL;
  if NOT FileExists(FileName)
  then EXIT;

  try
    Result:= TBitmap.Create;
    { LoadThumbnailFromFile uses nearest power-of-2 downsampling for efficiency }
    Result.LoadThumbnailFromFile(FileName, TargetWidth, TargetHeight);
  except
    on E: Exception do
    begin
      AppData.RamLog.AddError('LoadThumbnail failed: ' + E.Message);
      FreeAndNil(Result);
    end;
  end;
end;


{ Gets image dimensions without fully decoding the image (fast).
  Uses TBitmapCodecManager with fallback to full load if that fails.
  Returns 0,0 if file doesn't exist or cannot be read.

  Note: Old implementation using TBitmap.CreateFromFile worked on Windows with PNGs
  but failed on Android with some JPGs (possibly corrupted files). }
procedure GetImageResolution(FileName: string; out Width, Height: Integer);
var
  Size: TPointF;
  Bmp: TBitmap;
begin
  Width := 0;
  Height:= 0;
  if NOT FileExists(FileName)
  then EXIT;

  try
    { TBitmapCodecManager.GetImageSize is the fastest way - reads header only }
    Size  := TBitmapCodecManager.GetImageSize(FileName);
    Width := Trunc(Size.X);
    Height:= Trunc(Size.Y);
  except
    on E: Exception do
    begin
      AppData.RamLog.AddError('Cannot get image resolution for ' + FileName + ': ' + E.Message);

      { Fallback: Full load if CodecManager fails (rare but possible with some formats) }
      Bmp:= NIL;
      try
        Bmp:= TBitmap.CreateFromFile(FileName);
        Width := Bmp.Width;
        Height:= Bmp.Height;
      finally
        FreeAndNil(Bmp);
      end;
      { Note: If fallback also fails, Width/Height remain 0 }
    end;
  end;
end;


{ Saves bitmap to file. Format is determined by file extension (jpg, png, bmp, etc).
  Uses TBitmapCodecManager internally for platform-appropriate encoding.
  Shows error message dialog if save fails. }
procedure SaveBitmap(BMP: TBitmap; FileName: string);
var
  TempBMP: TBitmap;
begin
  Assert(Assigned(BMP), 'SaveBitmap: BMP parameter cannot be nil');
  Assert(FileName <> '', 'SaveBitmap: FileName cannot be empty');

  TempBMP:= TBitmap.Create;
  try
    TempBMP.Assign(BMP);
    try
      TempBMP.SaveToFile(FileName);
    except
      on E: Exception do
      begin
        TDialogService.ShowMessage('Save Failed: ' + E.Message);
        EXIT;
      end;
    end;
  finally
    FreeAndNil(TempBMP);
  end;
end;






{-------------------------------------------------------------------------------
  TBitmap Cropping (OPTIMIZED using CopyFromBitmap)
-------------------------------------------------------------------------------}

{ Crops a region from InputBMP. Returns a new bitmap containing only the cropped area.
  Uses CopyFromBitmap for fast, direct pixel copying without canvas operations.
  Caller is responsible for freeing the returned bitmap. }
function CropBitmap(InputBMP: TBitmap; CropRect: TRectF): TBitmap;
begin
  Assert(Assigned(InputBMP), 'CropBitmap: InputBMP parameter cannot be nil');
  Assert((CropRect.Width > 0) and (CropRect.Height > 0), 'CropBitmap: CropRect must have positive dimensions');

  Result:= TBitmap.Create;
  try
    Result.SetSize(Round(CropRect.Width), Round(CropRect.Height));
    Result.CopyFromBitmap(InputBMP, CropRect.Round, 0, 0);
  except
    FreeAndNil(Result);
    raise;
  end;
end;


{ Loads image from file and crops it. Returns nil if file cannot be loaded.
  Caller is responsible for freeing the returned bitmap. }
function CropBitmap(FileName: string; CropRect: TRectF): TBitmap;
var
  SrcBmp: TBitmap;
begin
  SrcBmp:= LoadImage(FileName);
  if SrcBmp = NIL
  then EXIT(NIL);

  try
    Result:= CropBitmap(SrcBmp, CropRect);
  finally
    FreeAndNil(SrcBmp);
  end;
end;


{ Loads image from file, crops it, and assigns the result to a TImage.
  Clears the Image if file cannot be loaded. }
procedure CropBitmap(FileName: string; CropRect: TRectF; Image: TImage);
var
  CropBmp: TBitmap;
begin
  Assert(Assigned(Image), 'CropBitmap: Image parameter cannot be nil');
  Assert(NOT CropRect.IsEmpty, 'CropBitmap: CropRect not defined!');

  CropBmp:= CropBitmap(FileName, CropRect);
  try
    if CropBmp <> NIL
    then Image.Bitmap.Assign(CropBmp)
    else Image.Bitmap.Clear(TAlphaColorRec.Null);
  finally
    FreeAndNil(CropBmp);
  end;
end;



{ Fills the entire bitmap with the specified color. }
procedure FillBitmap(BMP: TBitmap; Color: TAlphaColor);
var
  R: TRectF;
begin
  Assert(Assigned(BMP), 'FillBitmap: BMP parameter cannot be nil');

  BMP.Canvas.BeginScene;
  try
    R:= TRectF.Create(0, 0, BMP.Width, BMP.Height);
    BMP.Canvas.Fill.Color:= Color;
    BMP.Canvas.Fill.Kind := TBrushKind.Solid;
    BMP.Canvas.FillRect(R, 0, 0, [], 1);
  finally
    BMP.Canvas.EndScene;
  end;
end;


{ Creates a new bitmap with the specified dimensions and fills it with the background color.
  Caller is responsible for freeing the returned bitmap. }
function CreateBitmap(Width, Height: Integer; BkgClr: TAlphaColor= TAlphaColorRec.Black): TBitmap;
begin
  Assert((Width > 0) and (Height > 0), 'CreateBitmap: Width and Height must be positive');

  Result:= NIL;
  try
    Result:= TBitmap.Create(Width, Height);
    FillBitmap(Result, BkgClr);
  except
    FreeAndNil(Result);
    raise;
  end;
end;


end.


