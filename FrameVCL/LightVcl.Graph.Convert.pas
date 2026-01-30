UNIT LightVcl.Graph.Convert;

{=============================================================================================================
   Gabriel Moraru
   2026.01.30
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
  Image format conversion utilities:
    - BMP to JPG conversion (to memory, stream, or file)
    - JPG to BMP conversion
    - JPG recompression with quality control
    - TGraphic to JPG conversion
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Imaging.Jpeg;

CONST
   { Average image quality. Higher values, better results. To test if 70 is indeed the best value.  }
   DelphiJpgQuality = 70;


{-------------------------------------------------------------------------------------------------------------
   CONVERTERS
-------------------------------------------------------------------------------------------------------------}
 function  Bmp2Jpg   (BMP: TBitmap;                     CompressFactor: Integer= DelphiJpgQuality): TJpegImage; overload;
 procedure Bmp2Jpg   (BMP: TBitmap; OutputFile: string; CompressFactor: Integer= DelphiJpgQuality);             overload;

 procedure Graph2Jpg (Graph: TGraphic; OutputFile: string; CompressFactor: Integer= DelphiJpgQuality);
 function  Jpeg2Bmp  (JPG: TJpegImage): TBitmap;

{-------------------------------------------------------------------------------------------------------------
   COMPRESSORS
-------------------------------------------------------------------------------------------------------------}
 function  Bmp2JpgStream (BMP: TBitmap; CompressFactor: Integer= DelphiJpgQuality): TStream;
 function  CompressBmp   (BMP: TBitmap; CompressFactor: Integer= DelphiJpgQuality): Integer;

 function  Recompress    (Input: TJPEGImage; OUT Outp: TJPEGImage; CompressFactor: Integer= DelphiJpgQuality): Integer;  overload;
 function  Recompress    (Jpg: TJPEGImage; CompressFactor: Integer= DelphiJpgQuality): Integer;          overload;

 IMPLEMENTATION

USES
   LightCore.IO, LightCore;





{--------------------------------------------------------------------------------------------------
   CONVERT TO JPG
--------------------------------------------------------------------------------------------------}

{ Recompresses a JPG image with a new quality factor.
  Creates a NEW JPG image (Outp) with the specified compression - caller must free Outp.
  Returns the size in bytes of the resulting compressed image.

  Note: This function modifies the Input image (sets CompressionQuality and calls Compress).
  If you need to preserve the original, pass a copy. }
function Recompress(Input: TJPEGImage; OUT Outp: TJPEGImage; CompressFactor: Integer= DelphiJpgQuality): Integer;    { Old name: CompressJpg2RAM }
VAR
   Stream: TMemoryStream;
begin
 if Input = NIL
 then raise Exception.Create('Recompress: Input parameter cannot be nil');

 Outp:= NIL;  { Initialize OUT parameter }
 Stream:= TMemoryStream.Create;
 TRY
   Input.CompressionQuality:= CompressFactor;
   Input.Compress;
   Input.SaveToStream(Stream);
   Result:= Stream.Size;

   { Create the new image }
   Stream.Position := 0;
   Outp:= TJPEGImage.Create;
   TRY
     Outp.LoadFromStream(Stream);
   EXCEPT
     FreeAndNil(Outp);
     RAISE;
   END;
 FINALLY
   FreeAndNil(Stream);
 END;
end;


{ Recompresses a JPG image in-place with a new quality factor.
  Modifies the Jpg parameter directly.
  Returns the size in bytes of the resulting compressed image. }
function Recompress(Jpg: TJPEGImage; CompressFactor: Integer= DelphiJpgQuality): Integer;
begin
 if Jpg = NIL
 then raise Exception.Create('Recompress: Jpg parameter cannot be nil');

 VAR Stream:= TMemoryStream.Create;
 TRY
   Jpg.CompressionQuality:= CompressFactor;
   Jpg.Compress;
   Jpg.SaveToStream(Stream);
   Result:= Stream.Size;
 FINALLY
   FreeAndNil(Stream);
 END;
end;




{ Calculates what size a BMP would be if converted to JPEG with given compression.
  Returns the size in bytes of the hypothetical compressed image.
  Does not modify the input BMP or produce any output file. }
function CompressBmp(BMP: TBitmap; CompressFactor: Integer= DelphiJpgQuality): Integer;
VAR
   Jpg: TJPEGImage;
   Stream: TMemoryStream;
begin
  if BMP = NIL
  then raise Exception.Create('CompressBmp: BMP parameter cannot be nil');

  Jpg:= TJPEGImage.Create;
  Stream:= TMemoryStream.Create;
  try
    Jpg.CompressionQuality:= CompressFactor;
    Jpg.Assign(BMP);
    Jpg.SaveToStream(Stream);
    Result:= Stream.Size;
  FINALLY
    FreeAndNil(Stream);
    FreeAndNil(Jpg);
  END;
end;


{ Converts a bitmap to JPEG and returns it as a memory stream.
  Caller is responsible for freeing the returned stream.
  Stream position is at the end after return - seek to 0 before reading. }
function Bmp2JpgStream(BMP: TBitmap; CompressFactor: Integer= DelphiJpgQuality): TStream;
VAR
   JpegImg: TJpegImage;
begin
 if BMP = NIL
 then raise Exception.Create('Bmp2JpgStream: BMP parameter cannot be nil');

 Result:= TMemoryStream.Create;   { TStream is abstract, use TMemoryStream }
 TRY
   JpegImg := TJpegImage.Create;
   TRY
    JpegImg.PixelFormat:= jf24Bit;
    JpegImg.CompressionQuality := CompressFactor;  { Higher values, better results }
    JpegImg.Assign(BMP);
    JpegImg.SaveToStream(Result);
   FINALLY
    FreeAndNil(JpegImg);
   END;
 EXCEPT
   FreeAndNil(Result);  { Free stream on failure }
   RAISE;
 END;
end;






{--------------------------------------------------------------------------------------------------
   CONVERT BETWEEN BMP & JPG
--------------------------------------------------------------------------------------------------}

{ Converts a JPEG image to a 24-bit DIB bitmap.
  Caller is responsible for freeing the returned bitmap.
  Note: This can be slow for large images (several seconds). }
function Jpeg2Bmp(JPG: TJpegImage): TBitmap;
begin
   if JPG = NIL
   then raise Exception.Create('Jpeg2Bmp: JPG parameter cannot be nil');

   Result:= TBitmap.Create;
   TRY
     Result.PixelFormat := pf24bit;
     Result.HandleType:= bmDIB;
     Result.Assign(JPG);    { This takes 4 sec for a large image }
   EXCEPT
      FreeAndNil(Result);   { We only free the result in case of failure }
      Raise;
   END;
end;



{ Converts a bitmap to a JPEG image in memory.
  Caller is responsible for freeing the returned TJpegImage.
  CompressFactor: 1=lowest quality/smallest, 100=highest quality/largest. }
function Bmp2Jpg(BMP: TBitmap; CompressFactor: Integer= DelphiJpgQuality): TJpegImage;
begin
 if BMP = NIL
 then raise Exception.Create('Bmp2Jpg: BMP parameter cannot be nil');

 Result:= TJPEGImage.Create;
 TRY
   Result.CompressionQuality:= CompressFactor; { 100 = best quality }
   Result.Assign(BMP);
 EXCEPT
   FreeAndNil(Result);   { We only free the result in case of failure }
   Raise;
 END;
end;


{ Save the bitmap as JPEG directly to disk.
  OutputFile is the full path (including filename) for the output JPEG file.
  Creates the output directory if it doesn't exist. }
procedure Bmp2Jpg(BMP: TBitmap; OutputFile: string; CompressFactor: Integer= DelphiJpgQuality);
VAR
   OutFolder: string;
begin
 if BMP = NIL
 then raise Exception.Create('Bmp2Jpg: BMP parameter cannot be nil');
 if OutputFile = ''
 then raise Exception.Create('Bmp2Jpg: OutputFile parameter cannot be empty');

 var JPG:= Bmp2Jpg(BMP, CompressFactor);
 TRY
  OutFolder:= ExtractFilePath(OutputFile);

  { If no folder specified (just filename), save to current directory }
  if (OutFolder = '') OR ForceDirectoriesB(OutFolder)
  then JPG.SaveToFile(OutputFile)
  else RAISE Exception.Create('Cannot create output folder!'+ CRLFw+ OutFolder);
 FINALLY
  FreeAndNil(JPG);
 END;
end;


{ Saves any TGraphic (TBitmap, TIcon, TMetafile, etc.) as a JPEG file.
  OutputFile is the full path including filename for the output JPEG.
  Note: Does not create parent directories - use Bmp2Jpg for that functionality. }
procedure Graph2Jpg(Graph: TGraphic; OutputFile: string; CompressFactor: Integer= DelphiJpgQuality);
VAR JPG: TJpegImage;
begin
 if Graph = NIL
 then raise Exception.Create('Graph2Jpg: Graph parameter cannot be nil');
 if OutputFile = ''
 then raise Exception.Create('Graph2Jpg: OutputFile parameter cannot be empty');

 JPG:= TJpegImage.Create;
 TRY
  JPG.CompressionQuality:= CompressFactor;
  JPG.Assign(Graph);
  JPG.SaveToFile(OutputFile);
 FINALLY
  FreeAndNil(JPG);
 END;
end;





{$WARN GARBAGE OFF}              {Silence the: 'W1011 Text after final END' warning }

{ unused? }
{ Compress the specified image using the specified compression factor.
  Returns the size of the resulted compressed image & the compressed image as JPG }  {
function CompressBmp2Ram(InputBMP: TBitmap; OUT OutBMP: TBitmap; CompressFactor: Integer): Integer;
VAR
   tmpQStream: TMemoryStream;
   Jpg: TJPEGImage;
begin
 Jpg:= TJPEGImage.Create;
 tmpQStream:= TMemoryStream.Create;
 TRY
   Jpg.Assign(InputBMP);
   Jpg.CompressionQuality:= CompressFactor;
   Jpg.Compress;                 //this takes time
   Jpg.SaveToStream(tmpQStream);
   Result:= tmpQStream.Size;

   // Output
   tmpQStream.Position := 0;
   OutBMP:= TBitmap.Create;
   OutBMP.Assign(Jpg);
 FINALLY
   FreeAndNil(tmpQStream);
   FreeAndNil(Jpg);
 END;
end; }



{ Compress the specified image to jpg using the specified compression factor.
  Returns the resulted compressed image (as BMP) & its size.
function CompressBmp2RAM(BMP: TBitmap; OUT ResultedSize: Integer; CompressFactor: Integer= DelphiJpgQuality): TBitmap;
VAR Jpg: TJPEGImage;
begin
 Result:= TBitmap.Create;
 TRY
    Result.PixelFormat:= pf24bit;
    Jpg:= TJPEGImage.Create;
    Jpg.PixelFormat:= jf24Bit;
    Jpg.Assign(BMP);
    ResultedSize:= Recompress(Jpg, CompressFactor); // takes 4 seconds to compress: d:\Digital camera\SELECTATE\Comparatie camere foto\1\Fuji FinePix F550.JPG
    Result.Assign(Jpg);  // takes 3 seconds to assign
  FINALLY
    FreeAndNil(Jpg);
  END;
end;     }

end.
