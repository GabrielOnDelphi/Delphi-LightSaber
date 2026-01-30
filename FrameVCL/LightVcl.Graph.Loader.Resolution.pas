UNIT LightVcl.Graph.Loader.Resolution;

{=============================================================================================================
   Gabriel Moraru
   2026.01.30
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
  Retrieve image resolution without decoding the entire image.
  This is done by opening the binary file and reading the file header.
  Super mega fast!

  Supported formats: JPG, PNG, GIF, BMP.
  Returns -1 for Width/Height when format is not supported or file is corrupted.
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
   System.SysUtils, System.Classes, System.AnsiStrings, Vcl.Graphics;

TYPE
 TBitmapHeader = Record
   Signature     : Array[0..1] Of AnsiChar;
   Size          : Integer;
   Reserved      : Integer;
   OffBits       : Integer;
   StructSize    : Integer;
   Width         : Integer;
   Height        : Integer;
   Planes        : Word;
   BitCount      : Word;
   Compression   : Integer;
   ImageSize     : Integer;
   XPelsPerMeter : Integer;
   YPelsPerMeter : Integer;
   ClrUsed       : Integer;
   ClrImportant  : Integer;
  end;

 
 { Image resolution all }
 procedure GetImageRes(CONST FileName: string; OUT Width, Height: Integer);                  overload;
 procedure GetImageRes(CONST FileName: string; Stream: TStream; OUT Width, Height: Integer); overload;

 { Image resolution by stream }
 function  GetJpgSize (Stream: TStream; OUT Width, Height: Integer): Boolean;           overload;
 procedure GetPNGSize (Stream: TStream; OUT Width, Height: Integer);                    overload;
 procedure GetGIFSize (Stream: TStream; OUT Width, Height: Integer);                    overload;
 procedure GetBmpSize (Stream: TStream; OUT Width, Height: Integer);                    overload;

 { Image resolution by file }
 function  GetJPGSize (CONST Filename: string; OUT Width, Height: Integer): Boolean;    overload;
 procedure GetPNGSize (CONST FullName: string; OUT Width, Height: Integer);             overload;
 procedure GetGIFSize (CONST FullName: string; OUT Width, Height: Integer);             overload;
 procedure GetBmpSize (CONST FullName: string; OUT Width, Height: Integer);             overload;

 { BMP info }
 function  GetBmpHeader   (Stream: TStream): TBitmapHeader;   // Used by GetBmpSize
 function  GetBitsPerPixel(BMP:TBitmap): Integer;



 IMPLEMENTATION

 USES LightCore.Binary, LightVcl.Graph.Util, LightCore.IO;

 


{--------------------------------------------------------------------------------------------------
   GET IMAGE RESOLUTION ALL
   GetImageRes MUST return -1 to indicate that it tried to read the file but it failed. Otherwise, if it would return 0, the Playlist will keep reading img resolution in grid.draw (many times per second)
--------------------------------------------------------------------------------------------------}
procedure GetImageRes(CONST FileName: string; OUT Width, Height: Integer);
begin
 TRY
   if IsJpg(FileName)
   then GetJPGSize(FileName, Width, Height)
   else
    if IsPng(FileName)
    then GetPNGSize(FileName, Width, Height)
    else
     if IsGif(FileName)
     then GetGIFSize(FileName, Width, Height)
     else
      if Isbmp(FileName)
      then GetBmpSize(FileName, Width, Height)
      else
        begin
          Width := -1;    //DO NOT RAISE AN exception HERE IF THE FILE TYPE IS UNSUPORTED! Let the caller decide what to do. This could happen when bionix downloads and URL and the server returns a HTML instead of an image!
          Height:= -1;
        end;
 EXCEPT
  //todo 1: trap only specific exceptions
  //DO NOTHING    or: write this directly in LightCore, LightCore.Time, LightCore.Types, LightVcl.Common.SystemTime, LightVcl.Common.Clipboard, LightVcl.Common.Dialogs log
  Width := -1;    //DO NOT RAISE AN exception HERE IF THE FILE TYPE IS UNSUPORTED! Let the caller decide what to do. This could happen when bionix downloads and URL and the server returns a HTML instead of an image!
  Height:= -1;
 END;
end;


procedure GetImageRes(CONST FileName: string; Stream: TStream; OUT Width, Height: Integer);
begin
 if IsJpg(FileName)
 then GetJPGSize(Stream, Width, Height)
 else
  if IsPng(FileName)
  then GetPNGSize(Stream, Width, Height)
  else
   if IsGif(FileName)
   then GetGIFSize(Stream, Width, Height)
   else
    if Isbmp(FileName)
    then GetBmpSize(Stream, Width, Height)
    else
      begin
        Width := -1;    //CONST  LocalFileSizeUnknown = -2; SEEMS NOT TO BE TRUE ANYMORE       { The file was downloaded but I could not retrive its size } move this to bionix
        Height:= -1;
      end;
end;













{--------------------------------------------------------------------------------------------------
   JPG
   Parses JPEG stream to find dimensions in SOF (Start Of Frame) marker.
   JPEG structure: SOI marker ($FFD8) followed by segments.
   Each segment: $FF + marker type + 2-byte length + data.
   SOF markers ($FFC0..$FFC3) contain frame dimensions.
--------------------------------------------------------------------------------------------------}

function GetJpgSize(Stream: TStream; OUT Width, Height: Integer): Boolean;
VAR
   n: integer;
   b: byte;
   w: Word;
begin
 Width := -1;
 Height:= -1;
 Result:= FALSE;

 { Check empty file }
 n:= Stream.Size-8;
 if n<= 0 then EXIT(FALSE);

 Stream.Position:= 0;   { This is mandatory because we don't know who used this object before and if it left it unwinded }

 { Check invalid format }
 Stream.Read(w, 2);
 if w<>$D8FF then EXIT(FALSE);

 Stream.Read(b, 1);
 while (Stream.Position< n) AND (b= $FF) DO
  begin
    Stream.Read(b, 1);
    case b of
      $C0..$C3:
       begin
         Stream.Seek(3, soFromCurrent);

         { Get Height }
         Stream.Read(w,2);
         height := swap(w);

         { Get Width }
         Stream.Read(w, 2);
         width := swap(w);

         { Success }
         EXIT(TRUE);
       end;
      $D0..$D9, $01:
       begin
         Stream.Seek(1, soFromCurrent);
         Stream.Read(b, 1);
       end;
      $FF: Stream.Read(b, 1);                                                                     { Keep reading }
      else
       begin
         Stream.Read(w, 2);
         Stream.Seek(swap(w)-2, soFromCurrent);
         Stream.Read(b, 1);
       end;
    end;
  end;
end;


function GetJPGSize(CONST Filename: string; OUT Width, Height: Integer): Boolean;                  { Works perfectly }
VAR
   DiskStream: TFileStream;
begin
 DiskStream:= TFileStream.Create(Filename, fmOpenRead OR fmShareDenyNone);
 TRY
   Result:= GetJPGSize(DiskStream, Width, Height);
 FINALLY
  FreeAndNil(DiskStream);
 END;
end;





{--------------------------------------------------------------------------------------------------
   PNG
--------------------------------------------------------------------------------------------------}

procedure GetPNGSize(Stream: TStream; OUT Width, Height: Integer);   //source   http://www.delphidabbler.com/tips/201      http://stackoverflow.com/questions/15209076/how-to-get-dimensions-of-image-file-in-delphi
TYPE
   TPNGSig = array[0..7] of Byte;
CONST
   ValidSig: TPNGSig = (137,80,78,71,13,10,26,10);  {or as hex: 89 50 4E 47 0D 0A 1A 0A }
VAR
   FileSign: TPNGSig;
   i: integer;
begin
 Stream.Position:= 0;   { This is mandatory because we don't know who used this object before and if it left it unwinded }

 { Transfer data from MemoryStream to FileSign }
 if Stream.Read(FileSign[0], SizeOf(FileSign)) <> 8 then
    begin
     Width  := -1;
     Height := -1;
     EXIT;
    end;

 { Check if the signature is valid }
 for i:= Low(FileSign) to High(FileSign) DO
   if FileSign[i] <> ValidSig[i] then
    begin
     Width  := -1;
     Height := -1;
     EXIT;
    end;

 { Read image dimensions from IHDR chunk.
   PNG structure: 8-byte signature + 4-byte chunk length + 4-byte chunk type ("IHDR") + chunk data.
   IHDR chunk data starts at byte 16: Width (4 bytes) at offset 16, Height (4 bytes) at offset 20.
   We read 2-byte (Word) values at offset 18 and 22 because PNG uses big-endian 32-bit integers,
   and ReadMotorolaWord reads the high 16 bits which contain the value for images < 65536 pixels. }
 Stream.Seek(18, 0);
 Width := LightCore.Binary.ReadMotorolaWord(Stream);

 Stream.Seek(22, 0);
 Height := LightCore.Binary.ReadMotorolaWord(Stream);
end;


procedure GetPNGSize(const FullName: string; OUT Width, Height: Integer);
VAR Stream: TFileStream;
begin
  Stream := TFileStream.Create(FullName, fmOpenRead OR fmShareDenyNone);
  TRY
    GetPNGSize(Stream, Width, Height);
  FINALLY
    FreeAndNil(Stream);
  end;
end;




{--------------------------------------------------------------------------------------------------
   GIF
--------------------------------------------------------------------------------------------------}


procedure GetGIFSize(Stream: TStream; OUT Width, Height: Integer);                          { Also see this (for GIF): http://www.delphipages.com/forum/showthread.php?t=202158 }
TYPE
  TGIFHeader = record
    Sig: array[0..5] of Ansichar;
    ScreenWidth, ScreenHeight: Word;
    Flags, Background, Aspect: Byte;
  end;

  TGIFImageBlock = record
    Left, Top, Width, Height: Word;
    Flags: Byte;
  end;

VAR
   Header: TGifHeader;
   ImageBlock: TGifImageBlock;
   nResult: integer;
   x: integer;
   car: AnsiChar;
   DimensionsFound: boolean;
begin
 Width  := -1;
 Height := -1;
 Stream.Position:= 0;   { This is mandatory because we don't know who used this object before and if it left it unwinded }

 { Read header and ensure valid file. }
 nResult:= Stream.Read(Header, SizeOf(TGifHeader));
 if (nResult <> SizeOf(TGifHeader))
 OR (System.AnsiStrings.StrLComp('GIF', Header.Sig, 3) <> 0)
 then Exit;   { Image file invalid }

 { Skip color map, if there is one.
   Bit 7 of Flags indicates presence of Global Color Table.
   Bits 0-2 indicate size: 2^(N+1) colors, 3 bytes each (RGB). }
 if (Header.Flags and $80) > 0 then
  begin
   x := 3 * (1 shl ((Header.Flags and 7) + 1));
   if Stream.Position + x >= Stream.Size then EXIT;  { Color map thrashed }
   Stream.Seek(x, soFromCurrent);
  end;

 DimensionsFound := False;
 FillChar(ImageBlock, SizeOf(TGIFImageBlock), #0);

 { Step through blocks. }
 Stream.Read(car, 1);

 WHILE (Stream.Position < Stream.Size) AND NOT DimensionsFound DO
  begin
    if car = ',' then { Found image }
      begin
        nResult:= Stream.Read(ImageBlock, SizeOf(TGIFImageBlock));
        if nResult <> SizeOf(TGIFImageBlock) then Exit;                       { Invalid image block encountered }
        Width := ImageBlock.Width;
        Height:= ImageBlock.Height;
        DimensionsFound := True;
      end;
    Stream.Read(car, 1);
  end;
end;



procedure GetGIFSize(CONST FullName: string; OUT Width, Height: Integer);
VAR
   Stream: TFileStream;
begin
 Stream := TFileStream.Create(FullName, fmOpenRead OR fmShareDenyNone);
 TRY
   GetGIFSize(Stream, Width, Height);
 FINALLY
   FreeAndNil(Stream);
 END;
end;


(*
moved to cFrameServer
function IsAnimatedGif(CONST FileName: string): Boolean;  { NOTE!! I won't be able to get resolution for images that have wrong extension (image is PNG but the extension is JPG) }
VAR
   GIF: TGIFImage;
begin
 if NOT IsGIF(FileName) then EXIT(FALSE);

 GIF := TGIFImage.Create;
 TRY
  TRY
   GIF.LoadFromFile(FileName);     { This takes ~12sec for a 50MB GIF }    // <-----   bug report here:   Bug D49A0000 StackOVerflow in GifImg.pas
   Result:= Gif.Images.Count> 1;
  except_
    EXIT(FALSE);   { Don't crash on invalid images }
  END;
 FINALLY
   FreeAndNil(GIF);
 END;
end; *)

{Other snippets:
       http://www.delphipages.com/forum/showthread.php?t=117489 - Nu merge. Complete garbage.
       http://www.swissdelphicenter.ch/torry/showcode.php?id=1896 - Nu merge cu majoritatea JPEG-urilor.
       http://www.delphi3000.com/articles/article_3460.asp - Works with 90% of jpegs   }







{--------------------------------------------------------------------------------------------------
   BMP
--------------------------------------------------------------------------------------------------}

{ Retrieves BMP dimensions from file by reading the BMP header.
  Source: http://www.delphipages.com/forum/archive/index.php/t-202158.html }
procedure GetBmpSize(CONST FullName: string; OUT Width, Height: Integer);
VAR
   H: TBitmapHeader;
   Stream: TFileStream;
begin
 Stream:= TFileStream.Create(FullName, fmOpenRead OR fmShareDenyNone);
 TRY
  H:= GetBmpHeader(Stream);
  Width := H.Width;
  Height:= H.Height;
 FINALLY
   FreeAndNil(Stream);
 END;
end;



procedure GetBmpSize(Stream: TStream; OUT Width, Height: Integer);
VAR H: TBitmapHeader;
begin
 H:= GetBmpHeader(Stream);
 Width := H.Width;
 Height:= H.Height;
end;



{ Reads and parses a BMP file header from a stream.
  BMP format: 2-byte signature ("BM") followed by BITMAPFILEHEADER and BITMAPINFOHEADER.
  Returns a TBitmapHeader record with -1 values if the signature is invalid. }
function GetBmpHeader(Stream: TStream): TBitmapHeader;
VAR
   CTemp: AnsiChar;
   Lng: LongInt;
   Wrd: Word;
begin
 CTemp := '.';
 Stream.Position:= 0;   { Reset stream position - mandatory because we don't know who used this object before }

 Stream.ReadBuffer(CTemp,1);
 Result.Signature[0]:= CTemp;

 Stream.ReadBuffer(CTemp,1);
 Result.Signature[1]:= CTemp;

 if Result.Signature = 'BM' then
  begin
   Stream.ReadBuffer(Lng,4); //Size
   Result.Size := Lng;

   Stream.ReadBuffer(Lng,4); //Reserved
   Result.Reserved := Lng;

   Stream.ReadBuffer(Lng,4); //OffBits
   Result.OffBits := Lng;

   Stream.ReadBuffer(Lng,4); //StructSize
   Result.StructSize := Lng;

   Stream.ReadBuffer(Lng,4); //Width
   Result.Width := Lng;

   Stream.ReadBuffer(Lng,4); //Height
   Result.Height := Lng;

   Stream.ReadBuffer(Wrd,2); //Planes
   Result.Planes := Wrd;

   Stream.ReadBuffer(Wrd,2); //BitCount
   Result.BitCount:= Wrd;

   Stream.ReadBuffer(Lng,4); //Compression
   Result.Compression := Lng;

   Stream.ReadBuffer(Lng,4); //Image Size
   Result.ImageSize := Lng;

   Stream.ReadBuffer(Lng,4); //XPels
   Result.XPelsPerMeter := Lng;

   Stream.ReadBuffer(Lng,4); //YPels
   Result.YPelsPerMeter := Lng;

   Stream.ReadBuffer(Lng,4); //CLRUsed
   Result.ClrUsed := Lng;

   Stream.ReadBuffer(Lng,4); //CLRImportant
   Result.ClrImportant := Lng;
  end
 else
  begin
   { Invalid BMP signature - return -1 for all size-related fields }
   Result.Size          := -1;
   Result.Reserved      := -1;
   Result.OffBits       := -1;
   Result.StructSize    := -1;
   Result.Width         := -1;
   Result.Height        := -1;
   Result.Planes        := 0;
   Result.BitCount      := 0;
   Result.Compression   := -1;
   Result.ImageSize     := -1;
   Result.XPelsPerMeter := -1;
   Result.YPelsPerMeter := -1;
   Result.ClrUsed       := -1;
   Result.ClrImportant  := -1;
  end;
end;



function GetBitsPerPixel(BMP: TBitmap): Integer;
begin
 case BMP.PixelFormat of
   pf1Bit  : result:= 1;
   pf4Bit  : result:= 4;
   pf8Bit  : result:= 8;
   pf15Bit : result:= 15;
   pf16Bit : result:= 16;
   pf24Bit : result:= 24;
   pf32Bit : result:= 32;
   pfDevice: result:= LightVcl.Graph.Util.GetDeviceColorDepth;
  else
    Result:= -1;
  end;
end;


  

end.
