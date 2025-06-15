UNIT LightVcl.Graph.Convert;

{=============================================================================================================
   Gabriel Moraru
   2023.08.05
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
  Converts between JPG and BMP
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Imaging.Jpeg;

CONST
   { Average image quality. Higher values, better results. To test if 70 is indeed the best value.  }
   DelphiJpgQuality = 70;


{-------------------------------------------------------------------------------------------------------------
   CONVERTORS
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

 function  Recompress    (Input, Outp: TJPEGImage; CompressFactor: Integer= DelphiJpgQuality): Integer;  overload;
 function  Recompress    (Jpg: TJPEGImage; CompressFactor: Integer= DelphiJpgQuality): Integer;          overload;

 IMPLEMENTATION

USES
   ccIO, ccCore;





{--------------------------------------------------------------------------------------------------
   CONVERT TO JPG
--------------------------------------------------------------------------------------------------}

{ Compresses a JPG image. The output is also a JPG image.
  Returns the size of the new compressed JPG image AND the size of the image after it was compressed.
  This is done without actually saving the image to disk.}
function Recompress(Input, Outp: TJPEGImage; CompressFactor: Integer= DelphiJpgQuality): Integer;    { Old name: CompressJpg2RAM }
begin
 VAR Stream:= TMemoryStream.Create;
 TRY
   Input.CompressionQuality:= CompressFactor;
   Input.Compress;
   Input.SaveToStream(Stream);
   Result:= Stream.Size;

   { Create the new image }
   Stream.Position := 0;
   Outp:= TJPEGImage.Create;
   Outp.LoadFromStream(Stream);
 FINALLY
   FreeAndNil(Stream);
 END;
end;


function Recompress(Jpg: TJPEGImage; CompressFactor: Integer= DelphiJpgQuality): Integer;
begin
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




{ Compress the specified image to jpg using the specified compression factor.
  Returns the size of the resulted compressed image }
function CompressBmp(BMP: TBitmap; CompressFactor: Integer= DelphiJpgQuality): Integer;
begin
  VAR Jpg:= TJPEGImage.Create;
  VAR Stream:= TMemoryStream.Create;
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


function Bmp2JpgStream(BMP: TBitmap; CompressFactor: Integer= DelphiJpgQuality): TStream;
VAR
   JpegImg: TJpegImage;
begin
 Result:= TStream.create;
 JpegImg := TJpegImage.Create;
 TRY
  JpegImg.PixelFormat:= jf24Bit;
  JpegImg.CompressionQuality := CompressFactor;  { Higher values, better results }
  JpegImg.Assign(BMP);
  JpegImg.SaveToStream(Result);
 FINALLY
  FreeAndNil(JpegImg);
 END;
end;






{--------------------------------------------------------------------------------------------------
   CONVERT BETWEEN BMP & JPG
--------------------------------------------------------------------------------------------------}
function Jpeg2Bmp(JPG: TJpegImage): TBitmap;
begin
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



function Bmp2Jpg(BMP: TBitmap; CompressFactor: Integer= DelphiJpgQuality): TJpegImage;
begin
 Assert(BMP<> NIL);
 Result:= TJPEGImage.Create;
 TRY
   Result.CompressionQuality:= CompressFactor; { 100 = best }
   //Result.PixelFormat:= jf24Bit;
   Result.Assign(BMP);
   EXCEPT
      FreeAndNil(Result);   { We only free the result in case of failure }
      Raise;
   END;
end;


{ Save the image directly to disk.
  JpgOutPath is the full path (incl name) for the output jpeg file. }
procedure Bmp2Jpg(BMP: TBitmap; OutputFile: string; CompressFactor: Integer= DelphiJpgQuality);
begin
 var JPG:= Bmp2Jpg(BMP, CompressFactor);
 TRY
  var OutFolder:= ExtractFilePath(OutputFile);

  if ForceDirectoriesB(OutFolder)
  then JPG.SaveToFile(OutputFile)
  else RAISE Exception.Create('Cannot create output folder!'+ CRLFw+ OutFolder);
 FINALLY
  FreeAndNil(JPG);
 END;
end;


{ Save the specified TGraphic as Jpg }
procedure Graph2Jpg(Graph: TGraphic; OutputFile: string; CompressFactor: Integer= DelphiJpgQuality);
VAR JPG: TJpegImage;
begin
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
