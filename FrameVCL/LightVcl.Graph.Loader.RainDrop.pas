UNIT LightVcl.Graph.Loader.RainDrop;

{=============================================================================================================
   Gabriel Moraru
   2023.08.05
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
  Reads/writes files in RainDrop format (own format).
  It saves itself to disk to a binaray file that contains:
    * The original image as a JPG stream
    * A 2D array of bits (TPixelMap)

  See editor:
     LightVcl.Graph.Loader.RainEditor.pas
     c:\Myprojects\BIONIX\Projects\Project - Effects RainDrop\RainDropEditor\RainDropEditor.dpr
-----------------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Classes, vcl.Graphics, Vcl.Imaging.jpeg, LightCore.StreamBuff;

TYPE
   TPixelMap= array of array of Boolean;  { Map of bits. True bits correspond to a pink pixel (where we draw water) }

CONST
   RainDropExt   = '.RainDrop';
   RainDropFlt   = 'RainDrop animations|*.RainDrop';
   RainDropEditor= 'RainDropEditor.exe';
   RainMagicNo   = 'Shelter';


TYPE
  TWaterDamping = 1..99;

  PRaindropParams= ^RRaindropParams;
  RRaindropParams= record
  private
    FDamping: TWaterDamping;
    procedure SetDamping(Value: TWaterDamping);
  public
    TargetFPS     : Integer;
    WaveAplitude  : Integer;
    WaveTravelDist: Integer;
    DropInterval  : Integer;
    property  Damping: TWaterDamping read FDamping write SetDamping;
    procedure Reset;

    procedure Load(Stream: TCubicBuffStream);
    procedure Save(Stream: TCubicBuffStream);
  end;

  TRainShelter = class(TObject)
   private
    procedure Clear;
   public
     Params    : RRaindropParams;
     OrigImage : TBitmap;
     FileName  : string;
     PixelMap  : TPixelMap;
     function  ImportImage (aFileName: string): Boolean;
     function  LoadFromFile(aFileName: string): Boolean;
     procedure SaveToFile  (aFileName: string; Mask: TBitmap);
     procedure Save(Mask: TBitmap);
     destructor Destroy; override;
  end;

function IsRainDrop       (CONST FileName: string): Boolean;
function DetectRainShelter(CONST FileName: string): Boolean;
function LoadRainShelter  (CONST FileName: string): TBitmap;


IMPLEMENTATION

USES
   LightVcl.Graph.Convert, LightVcl.Graph.Loader;

CONST
   Verification=  $50505050;



function DetectRainShelter(CONST FileName: string): Boolean;   { Unused }
VAR
   Stream: TCubicBuffStream;
begin
 Stream:= TCubicBuffStream.CreateRead(FileName);
 TRY
   Result:= Stream.ReadHeaderTry(RainMagicNo, 1);
 FINALLY
  FreeAndNil(Stream);
 END;
end;


function IsRainDrop(CONST FileName: string): Boolean;
VAR sExtension: string;
begin
 sExtension:= ExtractFileExt(FileName);
 Result:= SameText(sExtension, RainDropExt);
end;


function LoadRainShelter(CONST FileName: string): TBitmap;
VAR
   Obj: TRainShelter;
begin
 Result:= TBitmap.Create;
 Obj:= TRainShelter.Create;
 TRY
   Obj.LoadFromFile(FileName);
   Result.Assign(Obj.OrigImage);
 FINALLY
  FreeAndNil(Obj);
 END;
end;













destructor TRainShelter.Destroy;
begin
 Clear;
 inherited;
end;


procedure TRainShelter.Clear;
begin
 FileName:= '';
 FreeAndNil(OrigImage);
end;




function TRainShelter.ImportImage(aFileName: string): Boolean;
begin
 Clear;
 FileName:= aFileName;
 OrigImage:= LightVcl.Graph.Loader.LoadGraph(aFileName, TRUE, TRUE);
 Result:= OrigImage <> NIL;
end;


procedure TRainShelter.Save(Mask: TBitmap);
begin
  if NOT IsRainDrop(FileName)
  then SaveToFile(ChangeFileExt(FileName, RainDropExt), Mask)
  else SaveToFile(FileName, Mask);
end;



procedure TRainShelter.SaveToFile(aFileName: string; Mask: TBitmap);
VAR
   x, y, w, h: Integer;
   b: Boolean;
   Stream: TCubicBuffStream;
begin
 Assert(Mask <> NIL);
 Assert(OrigImage <> NIL);
 FileName:= aFileName;

 { Prepare output stream }
 Stream:= TCubicBuffStream.CreateWrite(aFileName);
 TRY
   Stream.WriteHeader(RainMagicNo, 1);

   { Write img resolution }
   w:= OrigImage.Width;
   h:= OrigImage.Height;
   Stream.WriteCardinal(w);
   Stream.WriteCardinal(h);
   Params.Save(Stream);
   Stream.WritePadding(940);

   { Write original image to the output stream }
   VAR JpgStream:= LightVcl.Graph.Convert.Bmp2JpgStream(OrigImage);
   TRY
     JpgStream.Position:= 0;

     { Write size and JPG }
     VAR Count:= JpgStream.Size;
     Stream.WriteInteger(Count);         // Write the size of the original input image
     Stream.CopyFrom(JpgStream, Count);  // Write the whole JPG stream
   FINALLY
     FreeAndNil(JpgStream);
   END;

   { Write extras }
   Stream.WriteCardinal(Verification); // Verification

   { Write pink pixels }
   for x := 0 to w-1 do
    for y := 0 to h-1 do
      begin
       b:= Mask.Canvas.Pixels[x, y]= clFuchsia;
       Stream.WriteBoolean(b);
      end;

 FINALLY
   FreeAndNil(Stream);
 END;
end;




function TRainShelter.LoadFromFile(aFileName: string): Boolean;
VAR
   Count: Integer;
   Version: Word;
   x, y: Integer;
   w, h: Integer;
   JpegImg: TJpegImage;
   Stream: TCubicBuffStream;
begin
 Result:= FALSE;
 Clear;
 FileName:= aFileName;

 { Open inp stream }
 Stream:= TCubicBuffStream.CreateRead(FileName);
 TRY
   Version:= Stream.ReadHeader(RainMagicNo);
   case Version of
    0: Result:= FALSE; //'Invalid magic number!'
    1: begin
        { Read size }
        w:= Stream.ReadCardinal;
        h:= Stream.ReadCardinal;
        Params.Load(Stream);
        Stream.ReadPadding(940);
        Count:= Stream.ReadInteger;

        OrigImage:= TBitmap.Create;
        VAR JpgStream:= TMemoryStream.Create;
        JpegImg:= TJpegImage.Create;
        TRY
          //Assert(Count = 111791);
          JpgStream.CopyFrom(Stream, Count);
          JpgStream.Position := 0;
          JpegImg.LoadFromStream(JpgStream);
          //JpgStream.SaveToFile(AppData.ExeFolder+ '1.jpg');
          OrigImage.Assign(JpegImg);         { it must be a bitmap otherwise we get "Can only modify an image if it contains a bitmap": https://stackoverflow.com/questions/35703863/why-cant-i-draw-on-my-images-canvas }

          if (w <> OrigImage.Width) OR (h <> OrigImage.Height)
          then RAISE Exception.Create('Invalid BMP size!');
        FINALLY
          FreeAndNil(JpegImg);
          FreeAndNil(JpgStream);
        END;

        // Verification
        if Stream.ReadCardinal <> Verification
        then RAISE Exception.Create('Invalid verification!');

        { Read pink pixels }
        SetLength(PixelMap, w, h);
        for x := 0 to w-1 do
         for y := 0 to h-1 do
           PixelMap[x, y]:= Stream.ReadBoolean;

        Result:= TRUE;
       end;
   end;
 FINALLY
   freeAndNil(Stream);
 END;
end;













{ PARAMETERS }

procedure RRaindropParams.Reset;
begin
 TargetFPS      := 24;
 FDamping       := 15;    { Default damping = 15 }
 WaveAplitude   := 1;
 WaveTravelDist := 50;
 DropInterval   := 150;
end;


procedure RRaindropParams.Save(Stream: TCubicBuffStream);
begin
  Stream.WriteInteger(FDamping);
  Stream.WriteInteger(TargetFPS);
  Stream.WriteInteger(WaveAplitude);
  Stream.WriteInteger(WaveTravelDist);
  Stream.WriteInteger(DropInterval);
  Stream.WritePadding(64);
end;


procedure RRaindropParams.Load(Stream: TCubicBuffStream);
begin
  Damping       := Stream.ReadInteger;
  TargetFPS     := Stream.ReadInteger;
  WaveAplitude  := Stream.ReadInteger;
  WaveTravelDist:= Stream.ReadInteger;
  DropInterval  := Stream.ReadInteger;
  Stream.ReadPadding(64);
end;


procedure RRaindropParams.SetDamping(Value: TWaterDamping);
begin
  if  (Value >= Low(TWaterDamping))
  and (Value <= High(TWaterDamping))
  then FDamping := Value;
end;




end.
