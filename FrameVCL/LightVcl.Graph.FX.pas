UNIT LightVcl.Graph.FX;

{=============================================================================================================
   Gabriel Moraru
   2023.08.05
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
  Graphic effects:
    * Crop image
    * Tile image
    * Flip image
    * Enhance image (Darkness, Brightness, Contrast, Saturation)

  External dependencies:
    * None (had VclGraphUtil)
---------------------------------------------------------------------------------------------------}

INTERFACE

USES
   Winapi.Windows, System.SysUtils, Vcl.Graphics, LightCore.StreamBuff;

TYPE
  RTileType = record
    Horizon : Boolean;
    Vertical: Boolean;
    OneRow  : Boolean;
  end;

  PTileParams= ^RTileParams;          { Parameters for the 'Tile' routine }
  RTileParams = record
    TileType      : RTileType;
    TileAuto      : Boolean;          { False = No tile, True = Auto Tile (small images will be tiled. If wallpaper's height at least 40% of desktop's height, then 'Perfect Tile' effect will be applied (see the manual for details)). }
    TileThreshold : Integer; {%}      { Any image with an area higher or equal with desktop's size is considered 'normal'. Any image with area between 99% and this value is considered 'small' and it will be stretched to fill the desktop. Any image below this value is considered 'tiny' (to small to be stretched). }
    procedure Reset;
    procedure WriteToStream (Stream: TCubicBuffStream);
    procedure ReadFromStream(Stream: TCubicBuffStream);
  end;

 PEnhanceParams = ^REnhanceParams;    { Parameters for the 'Enhance' routine }
 REnhanceParams= record
    Smooth     : Boolean;
    Darkness   : ShortInt;            {-128..127}
    Brightness : Smallint;            {-32768..32767}
    Contrast   : Smallint;
    Saturation : Smallint;
    procedure Reset;
    procedure WriteToStream (Stream: TCubicBuffStream);
    procedure ReadFromStream(Stream: TCubicBuffStream);
   end;

 { Crop }
 procedure CropBitmap        (BMP: TBitmap; X, Y, W, H: Integer);       overload;                 { XYWH are crop coordinates }
 procedure CropBitmap        (BMP: TBitmap; W, H: Integer);             overload;                 { The image is centered before crop (crop edges in equal proportions) }
 procedure CropBitmap        (BMP: TBitmap; CONST MasterBMP: TBitmap);  overload;                 { Crop the 'BMP' to fit into the MasterBMP }

 { Tile }
 function  TileBitmap        (BMP: TBitmap; CONST OutWidth, OutHeight: Integer): TBitmap;
 procedure TileBitmapMirror  (BMP: TBitmap; CONST OutWidth, OutHeight: Integer; TileType: RTileType); { Use a mirror effect for every odd image }

 { Mirror }
 procedure FlipDown          (Bmp: TBitmap);       { 16ms }
 procedure FlipRight         (Bmp: TBitmap);       { 18ms }

 

IMPLEMENTATION

USES
   System.Types, LightVcl.Graph.Bitmap;




{---------------------------------------------------------------------------------------------------
   CROP
---------------------------------------------------------------------------------------------------}

procedure CropBitmap(BMP: TBitmap; X, Y, W, H: Integer);                                        { XYWH are crop coordinates }
begin
 Assert(W > 0);
 Assert(H > 0);

 if BMP.Width < W
 then W:= BMP.Width;
 if BMP.Height < h
 then h:= BMP.Height;

 BitBlt(BMP.Canvas.Handle, 0, 0, W, H, BMP.Canvas.Handle, X, Y, SRCCOPY);
 BMP.Width := W;
 BMP.Height:= H;
end;



{ The image is centered before crop (crop edges in equal proportions) }
procedure CropBitmap(BMP: TBitmap; W, H: Integer);
VAR HalfX, HalfY: Integer;
begin
 HalfX:= (BMP.Width  - W) DIV 2;
 HalfY:= (BMP.Height - H) DIV 2;

 if HalfX < 0
 then HalfX:= 0;
 if HalfY < 0
 then HalfY:= 0;

 CropBitmap(BMP, HalfX, HalfY, W, H);
end;



procedure CropBitmap(BMP: TBitmap; CONST MasterBMP: TBitmap);     { Crop the 'ToCropBMP' to fit into the MasterBMP. Basically, it is identical with the procedure above but instead of an integer parameter I give a BMP parameter }
VAR HalfX, HalfY: Integer;
begin
 HalfX:= Round((BMP.Width  - MasterBMP.Width)  / 2);
 HalfY:= Round((BMP.Height - MasterBMP.Height) / 2);

 if HalfX < 0
 then HalfX:= 0;
 if HalfY < 0
 then HalfY:= 0;

 CropBitmap(BMP, HalfX, HalfY, MasterBMP.Width, MasterBMP.Height);
end;








{---------------------------------------------------------------------------------------------------
   TILE
---------------------------------------------------------------------------------------------------}
function TileBitmap(BMP: TBitmap; CONST OutWidth, OutHeight: Integer): TBitmap;                     { Tile a image without using a secondary bitmap }
VAR
   TileX, TileY: integer;
begin
 { Create bitmap and set its size }
 Result:= CreateBitmap(OutWidth, OutHeight);
 if Result= NIL then EXIT;

 for TileX:= 0 TO (OutWidth div BMP.Width) do
  for TileY:= 0 TO (OutHeight div BMP.Height) do
   Result.Canvas.Draw (TileX* BMP.Width, TileY* BMP.Height, BMP);
end;



{ Use a mirror effect for every odd image }
procedure TileBitmapMirror(BMP: TBitmap; CONST OutWidth, OutHeight: Integer; TileType: RTileType);
VAR
   w, h, iCol, iRow: Integer;
   OutputBMP, MirrorBMP, MirrorDownBMP, DoubleMirror: TBitmap;

   procedure StitchBitmap(CurrBitmap: TBitmap);
   begin
     OutputBMP.Canvas.Draw (iCol* w, iRow* h, CurrBitmap)
   end;

begin
 MirrorBMP    := NIL;
 MirrorDownBMP:= NIL;
 DoubleMirror := NIL;

 OutputBMP:= CreateBitmap(OutWidth, OutHeight);
 TRY
  //FillBitmap(OutputBMP, TileType.BkgColor);   del
  w:= BMP.Width;
  h:= BMP.Height;

  { Flip image horizontally }
  if TileType.Horizon then
   begin
    MirrorBMP:= TBitmap.Create;
    MirrorBMP.Assign(BMP);               { Make a copy of input image }
    FlipRight (MirrorBMP);
   end;

  { Flip image vertically (head down) }
  if TileType.Vertical then
   begin
    MirrorDownBMP:= TBitmap.Create;
    MirrorDownBMP.Assign(BMP);
    FlipDown (MirrorDownBMP);
   end;

  { Flip image vertically and horizontally }
  if TileType.Vertical
  AND TileType.Horizon then
   begin
    DoubleMirror:= TBitmap.Create;
    DoubleMirror.Assign(BMP);
    FlipDown  (DoubleMirror);
    FlipRight (DoubleMirror);
   end;


 if TileType.OneRow
 then
  { Flip only horizontally }
  for iCol:= 0 TO (OutWidth div w) DO
     if (iCol mod 2 = 0)                                                        { Even columns }
     then OutputBMP.Canvas.Draw (iCol* w, (OutHeight - h) DIV 2, BMP)
     else
        if TileType.Horizon
        then OutputBMP.Canvas.Draw (iCol* w, (OutHeight - h) DIV 2, MirrorBMP)  { Apply mirrored BMP }
        else OutputBMP.Canvas.Draw (iCol* w, (OutHeight - h) DIV 2, BMP)        { Apply normal BMP }

 else
  { Flip horizontally & also vertically }
  for iRow:= 0 TO (OutHeight div h) DO
   for iCol:= 0 TO (OutWidth div w) DO
      if (iRow mod 2 = 0)                                                       { Randurile Even }
      then
          if (iCol mod 2 = 0)                                                   { Even columns }
          then StitchBitmap( BMP)
          else
             if TileType.Horizon
             then StitchBitmap( MirrorBMP)                                      { Apply mirrored BMP }
             else StitchBitmap( BMP)                                            { Apply normal BMP }
      else                                                                      { Randurile impare }
          if (iCol mod 2 = 0)                                                   { Even columns }
          then
             if TileType.Vertical
             then StitchBitmap( MirrorDownBMP)
             else StitchBitmap( BMP)
          else
             if TileType.Horizon
             AND TileType.Vertical
             then StitchBitmap( DoubleMirror)
             else
               if TileType.Horizon
               then StitchBitmap( MirrorBMP)
               else
                 if TileType.Vertical
                 then StitchBitmap( MirrorDownBMP)
                 else StitchBitmap( BMp);

   BMP.Assign(OutputBMP);
 FINALLY
   FreeAndNil(MirrorDownBMP);
   FreeAndNil(MirrorBMP);
   FreeAndNil(DoubleMirror);
   FreeAndNil(OutputBMP);
 END;
end;










{-------------------------------------------------------------------------------------------------------------
   MIRROR

     Tester: c:\MyProjects\Projects GRAPHICS\Rotate, flip\RotateTester.dpr
-------------------------------------------------------------------------------------------------------------}

procedure FlipDown(Bmp: TBitmap);     { 16ms }    { Same as JanFx.FlipDown (20ms) }
var dx, dy: integer;
begin
 dx:= Bmp.Width;
 dy:= Bmp.Height;
 Bmp.Canvas.CopyRect(Rect(0,0,dx,dy), Bmp.Canvas,Rect(0,dy,dx,0));
end;



{ Mirror left-right }
procedure FlipRight(Bmp: TBitmap);       { 18ms }
var dx, dy: integer;
begin
 dx:= Bmp.Width;
 dy:= Bmp.Height;
 Bmp.Canvas.CopyRect(Rect(0,0,dx,dy),  Bmp.Canvas,Rect(dx,0,0,dy));
end;






{-------------------------------------------------------------------------------------------------------------
   TILE
-------------------------------------------------------------------------------------------------------------}

procedure RTileParams.Reset;
begin
 TileAuto          := TRUE;
 TileType.Vertical := FALSE;
 TileType.Horizon  := TRUE;
 TileType.OneRow   := TRUE;
 TileThreshold     := 40;{%}                  { Any image with an area higher or equal with desktop's size is considered 'normal'. Any image with are between 99% and this value is considered 'small' and it will be stretched to fill the desktop. Any image below this value is considered 'tiny' (to small to be stretched). }
end;


procedure RTileParams.WriteToStream(Stream: TCubicBuffStream);
begin
 Stream.WriteBoolean (TileAuto);
 Stream.WriteBoolean (TileType.Horizon);
 Stream.WriteBoolean (TileType.OneRow);
 Stream.WriteBoolean (TileType.Vertical);
 Stream.WriteInteger (TileThreshold);
 Stream.WritePadding(32);
end;


procedure RTileParams.ReadFromStream(Stream: TCubicBuffStream);
begin
 TileAuto         := Stream.ReadBoolean;
 TileType.Horizon := Stream.ReadBoolean;
 TileType.OneRow  := Stream.ReadBoolean;
 TileType.Vertical:= Stream.ReadBoolean;
 TileThreshold    := Stream.ReadInteger;
                     Stream.ReadPadding(32);
end;







{-------------------------------------------------------------------------------------------------------------
   ENHANCE
-------------------------------------------------------------------------------------------------------------}

procedure REnhanceParams.Reset;
begin
 Smooth     := FALSE;
 Darkness   := 0;
 Brightness := 0;
 Contrast   := 0;
 Saturation := 255;
end;


procedure REnhanceParams.WriteToStream(Stream: TCubicBuffStream);
begin
 Stream.WriteSmallInt (Brightness);
 Stream.WriteSmallInt (Contrast);
 Stream.WriteSmallInt (Saturation);
 Stream.WriteShortInt (Darkness);
 Stream.WriteBoolean  (Smooth);
 Stream.WritePadding(32);
end;


procedure REnhanceParams.ReadFromStream(Stream: TCubicBuffStream);
begin
 Brightness := Stream.ReadSmallInt;
 Contrast   := Stream.ReadSmallInt;
 Saturation := Stream.ReadSmallInt;
 Darkness   := Stream.ReadShortInt;
 Smooth     := Stream.ReadBoolean;
 Stream.ReadPadding(32);
end;


end.
