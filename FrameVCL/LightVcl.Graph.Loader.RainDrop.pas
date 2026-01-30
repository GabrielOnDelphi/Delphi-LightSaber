UNIT LightVcl.Graph.Loader.RainDrop;

{=============================================================================================================
   Gabriel Moraru
   2026.01.30
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
  Reads/writes files in RainDrop format (own binary format).
  It saves itself to disk to a binary file that contains:
    * The original image as a JPG stream
    * A 2D array of bits (TPixelMap) - True bits correspond to pink pixels where water drops appear

  See editor:
     LightVcl.Graph.Loader.RainEditorForm.pas
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

    procedure Load(Stream: TLightStream);
    procedure Save(Stream: TLightStream);
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
function LoadRainShelter  (CONST FileName: string): TBitmap;


IMPLEMENTATION

USES
   LightVcl.Graph.Convert, LightVcl.Graph.Loader;

CONST
   Verification= $50505050;   { File integrity marker written after the JPG stream }


function IsRainDrop(CONST FileName: string): Boolean;
VAR sExtension: string;
begin
 sExtension:= ExtractFileExt(FileName);
 Result:= SameText(sExtension, RainDropExt);
end;


{---------------------------------------------------------------------------------------------------------------
   LoadRainShelter
   Loads a RainDrop file and returns the original embedded image as a TBitmap.
   Returns NIL if the file cannot be loaded or is invalid.
   Caller is responsible for freeing the returned TBitmap.
---------------------------------------------------------------------------------------------------------------}
function LoadRainShelter(CONST FileName: string): TBitmap;
VAR
   Obj: TRainShelter;
begin
 Result:= NIL;
 Obj:= TRainShelter.Create;
 TRY
   if Obj.LoadFromFile(FileName) AND (Obj.OrigImage <> NIL) then
     begin
       Result:= TBitmap.Create;
       Result.Assign(Obj.OrigImage);
     end;
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
 SetLength(PixelMap, 0);   { Reset pixel map to prevent stale data }
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


CONST CurrVersion = 3;

{---------------------------------------------------------------------------------------------------------------
   SaveToFile
   Saves the RainDrop file containing:
     1. File header (signature + version)
     2. Image dimensions (width, height)
     3. Animation parameters (damping, FPS, wave settings)
     4. Original image as embedded JPG stream
     5. Verification marker
     6. Pixel map (boolean array marking water drop positions - pink pixels)
   Note: Pink pixels (clFuchsia) in Mask indicate where water drops will appear.
---------------------------------------------------------------------------------------------------------------}
procedure TRainShelter.SaveToFile(aFileName: string; Mask: TBitmap);
VAR
   x, y, w, h: Integer;
   b: Boolean;
   Stream: TLightStream;
begin
 Assert(Mask <> NIL, 'TRainShelter.SaveToFile: Mask bitmap cannot be nil');
 Assert(OrigImage <> NIL, 'TRainShelter.SaveToFile: OrigImage cannot be nil');
 FileName:= aFileName;

 { Prepare output stream }
 Stream:= TLightStream.CreateWrite(aFileName);
 TRY
   Stream.WriteHeader(RainMagicNo, CurrVersion);

   { Write img resolution }
   w:= OrigImage.Width;
   h:= OrigImage.Height;
   Stream.WriteCardinal(w);
   Stream.WriteCardinal(h);
   Params.Save(Stream);
   Stream.WritePadding;

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




{---------------------------------------------------------------------------------------------------------------
   LoadFromFile
   Loads a RainDrop file and populates OrigImage, PixelMap, and Params.
   Returns TRUE on success, FALSE if the file signature/version doesn't match.
   Raises an exception if the file is corrupted (size mismatch or verification failure).
---------------------------------------------------------------------------------------------------------------}
function TRainShelter.LoadFromFile(aFileName: string): Boolean;
VAR
   Count: Integer;
   x, y: Integer;
   w, h: Integer;
   JpegImg: TJpegImage;
   Stream: TLightStream;
begin
 Result:= FALSE;
 Clear;
 FileName:= aFileName;

 Stream:= TLightStream.CreateRead(FileName);
 TRY
   if Stream.ReadHeader(RainMagicNo, CurrVersion) then
     begin
        { Read image dimensions }
        w:= Stream.ReadCardinal;
        h:= Stream.ReadCardinal;
        Params.Load(Stream);
        Stream.ReadPadding;
        Count:= Stream.ReadInteger;

        { Load embedded JPG and convert to bitmap }
        OrigImage:= TBitmap.Create;
        VAR JpgStream:= TMemoryStream.Create;
        JpegImg:= TJpegImage.Create;
        TRY
          JpgStream.CopyFrom(Stream, Count);
          JpgStream.Position:= 0;
          JpegImg.LoadFromStream(JpgStream);
          OrigImage.Assign(JpegImg);   { Must be a bitmap for canvas operations }

          if (w <> OrigImage.Width) OR (h <> OrigImage.Height)
          then RAISE Exception.Create('RainDrop file corrupted: image dimensions mismatch');
        FINALLY
          FreeAndNil(JpegImg);
          FreeAndNil(JpgStream);
        END;

        { Verify file integrity }
        if Stream.ReadCardinal <> Verification
        then RAISE Exception.Create('RainDrop file corrupted: verification marker invalid');

        { Load pixel map (water drop positions) }
        SetLength(PixelMap, w, h);
        for x:= 0 to w-1 do
         for y:= 0 to h-1 do
           PixelMap[x, y]:= Stream.ReadBoolean;

        Result:= TRUE;
     end;
 FINALLY
   FreeAndNil(Stream);
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


procedure RRaindropParams.Save(Stream: TLightStream);
begin
  Stream.WriteInteger(FDamping);
  Stream.WriteInteger(TargetFPS);
  Stream.WriteInteger(WaveAplitude);
  Stream.WriteInteger(WaveTravelDist);
  Stream.WriteInteger(DropInterval);
  Stream.WritePadding;
end;


procedure RRaindropParams.Load(Stream: TLightStream);
begin
  Damping       := Stream.ReadInteger;
  TargetFPS     := Stream.ReadInteger;
  WaveAplitude  := Stream.ReadInteger;
  WaveTravelDist:= Stream.ReadInteger;
  DropInterval  := Stream.ReadInteger;
  Stream.ReadPadding;
end;


{---------------------------------------------------------------------------------------------------------------
   SetDamping
   Validates and sets the water damping value. Values outside the valid range (1-99)
   are clamped to the nearest boundary rather than silently ignored.
---------------------------------------------------------------------------------------------------------------}
procedure RRaindropParams.SetDamping(Value: TWaterDamping);
begin
  { Clamp to valid range instead of silently ignoring invalid values }
  if Value < Low(TWaterDamping)
  then FDamping:= Low(TWaterDamping)
  else if Value > High(TWaterDamping)
  then FDamping:= High(TWaterDamping)
  else FDamping:= Value;
end;




end.
