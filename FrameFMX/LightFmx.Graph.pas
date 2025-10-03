UNIT LightFmx.Graph;


INTERFACE

USES
  System.SysUtils, System.Types, System.UITypes,
  FMX.Graphics, FMX.Types;

procedure GetImageResolution(FileName: string; Out Width, Height: Integer);

procedure FillBitmap(BMP: TBitmap; Color: TAlphaColor);
function  CreateBitmap (Width, Height: Integer; BkgClr: TAlphaColor= TAlphaColorRec.Black): TBitmap;


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


