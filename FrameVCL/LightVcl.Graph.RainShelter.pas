UNIT LightVcl.Graph.RainShelter;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
  Reads/writes files in RainDrop format (own binary format).
  The file contains:
    * The original image as a JPG stream
    * A 2D array of bits (TPixelMap) - True bits correspond to pink pixels where water drops appear.

  The animation is done by pluginRainDrop.pas

  See editor:
     LightVcl.Graph.Loader.RainEditorForm.pas
-----------------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Classes,
  vcl.Graphics, Vcl.Imaging.Jpeg, LightCore.StreamBuff, LightVcl.Graph.RainDropParams;

TYPE
   TPixelMap= array of array of Boolean;  { Map of bits. True bits correspond to a pink pixel (where we draw water) }

CONST
   RainDropExt   = '.RainDrop';
   RainDropFlt   = 'RainDrop animations|*.RainDrop';
   RainDropEditor= 'RainDropEditor.exe';


TYPE
  TRainShelter = class(TObject)
   private
     CONST ClassSignature = 'TRainShelter';
     procedure Clear;
   public
     OrigImage : TBitmap;
     FileName  : string;
     PixelMap  : TPixelMap;
     Params    : RRaindropParams;
     destructor Destroy; override;

     function  ImportImage (aFileName: string): Boolean;
     function  LoadFromFile(aFileName: string): Boolean;
     procedure SaveToFile  (aFileName: string; Mask: TBitmap);
     procedure Save(Mask: TBitmap);

     class function LoadBitmap(const FileName: string): TBitmap; static;
  end;

function IsRainShelter       (CONST FileName: string): Boolean;


IMPLEMENTATION

USES
   LightVcl.Graph.Convert, LightVcl.Graph.Loader;

CONST
  Verification= $50505050;   { File integrity marker written after the JPG stream }



{-------------------------------------------------------------------------------------------------------------
   UTILS
-------------------------------------------------------------------------------------------------------------}
function IsRainShelter(CONST FileName: string): Boolean;
VAR sExtension: string;
begin
 sExtension:= ExtractFileExt(FileName);
 Result:= SameText(sExtension, RainDropExt);
end;




{-------------------------------------------------------------------------------------------------------------
   CTOR/DTOP
-------------------------------------------------------------------------------------------------------------}
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




{-------------------------------------------------------------------------------------------------------------
   SaveToFile
-------------------------------------------------------------------------------------------------------------}
CONST
  CurrVersion = 5;

procedure TRainShelter.Save(Mask: TBitmap);
begin
  if NOT IsRainShelter(FileName)
  then SaveToFile(ChangeFileExt(FileName, RainDropExt), Mask)
  else SaveToFile(FileName, Mask);
end;


{ Note: Pink pixels (clFuchsia) in Mask indicate where water drops will appear }
procedure TRainShelter.SaveToFile(aFileName: string; Mask: TBitmap);
VAR
   x, y, w, h: Integer;
   b: Boolean;
   Stream: TLightStream;
begin
 Assert(Mask      <> NIL, 'TRainShelter.SaveToFile: Mask bitmap cannot be nil');
 Assert(OrigImage <> NIL, 'TRainShelter.SaveToFile: OrigImage cannot be nil');
 FileName:= aFileName;

 { Prepare output stream }
 Stream:= TLightStream.CreateWrite(aFileName);
 TRY
   Stream.WriteHeader(ClassSignature, CurrVersion);

   { Write img resolution }
   w:= OrigImage.Width;
   h:= OrigImage.Height;
   Stream.WriteCardinal(w);
   Stream.WriteCardinal(h);
   Params.Save(Stream);
   Stream.WritePaddingValidation;

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

   Stream.WriteCardinal(Verification);

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


{-------------------------------------------------------------------------------------------------------------
   Raises an exception if the file is corrupted (size mismatch or verification failure).
-------------------------------------------------------------------------------------------------------------}
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
   if Stream.ReadHeader(ClassSignature, CurrVersion) then
     begin
        { Read image dimensions }
        w:= Stream.ReadCardinal;
        h:= Stream.ReadCardinal;
        Params.Load(Stream);
        Stream.ReadPaddingValidation;
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





{ Loads a RainDrop file and returns the original embedded image as a TBitmap }
class function TRainShelter.LoadBitmap(CONST FileName: string): TBitmap;
VAR
   Obj: TRainShelter;
begin
  Result:= NIL;
  Obj:= TRainShelter.Create;
  TRY
    if Obj.LoadFromFile(FileName)
    AND (Obj.OrigImage <> NIL) then
      begin
        Result:= TBitmap.Create;
        Result.Assign(Obj.OrigImage);
      end;
  FINALLY
    FreeAndNil(Obj);
  END;
end;



end.
