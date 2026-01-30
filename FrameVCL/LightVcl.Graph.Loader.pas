UNIT LightVcl.Graph.Loader;

{=============================================================================================================
   Gabriel Moraru
   2026.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
  Helps you load common file formats (GIF, JPG, BMP, PNG, WB1, RainDrop, JPG2K) from disk.
  LoadGraph is the main function.
  The functions will not fail if the input image is corrupted. Instead it will simply output the error to the log.
  This is useful if we want to process thousands of images at batch because we don't want to stop everytime we encounter a broken image.

  Speed
    The functions support WIC which is much faster that the classic VCL loading functions
    (for example for JPG, WIC is at least 8x faster than Delphi's JPG decoding function).

  External dependencies:
     * CCR Exif lib -> Github.com/exilon/ccr-exif
     * Jpeg2000 lib
     * FastJpg

     Undefine CCRExif, FastJpg, and Jpg2000 if you don't want to download and compile those libraries. In this case, some functionality will not be available.

  TESTER:
     c:\Myprojects\Project Testers\gr LoadGraph\
     c:\MyProjects\Projects GRAPHICS Resamplers\GLOBAL Tester\TEST IMAGES\
-------------------------------------------------------------------------------------------------------------}

{PNG: see this: http://talkdelphi.blogspot.com/2009_03_01_archive.html }
{todo: is it possible to extract the EXIF information from a jpeg file without loading the file in memory? }

INTERFACE

USES
   Winapi.Wincodec, System.SysUtils, System.Math, System.Types, System.Classes,
   Vcl.Graphics, Vcl.Imaging.Jpeg, Vcl.Imaging.PngImage, Vcl.ExtCtrls, Vcl.Imaging.GIFImg
   {$IFDEF CCRExif},CCR.Exif{$ENDIF};

CONST
   DelphiJpgQuality = 70; { Average image quality. Seems that 70 is indeed the best value }

   {$IFNDEF CCRExif}
   TYPE
    TJpegImageEx= TJpegImage;
   {$ENDIF}

{-------------------------------------------------------------------------------------------------------------
   MAIN FUNCTION
-------------------------------------------------------------------------------------------------------------}
 function  LoadGraph     (CONST FileName: string; ExifRotate: Boolean = True; UseWic: Boolean = True): TBitmap;  overload;
 function  LoadGraph     (CONST FileName: string; OUT FrameCount: Cardinal): TBitmap; overload;
 procedure LoadGraphToImg(CONST FileName: string; Image: TImage; ExifRotate: Boolean = True; UseWic: Boolean = TRUE);

{-------------------------------------------------------------------------------------------------------------
   VCL LOADERS
-------------------------------------------------------------------------------------------------------------}
 function  LoadFromResource    (CONST RsrcName: string): TJPEGImage;                                   { Load an image from a resource file }
 function  LoadTPicture        (CONST FileName: string): TPicture;                                     { Based on TPicture. Better than LoadGraph. }
 procedure LoadToTImage        (CONST FileName: string; ExifRotate: Boolean; Image: TImage);           { Loads a file directly into a TImage component }


{-------------------------------------------------------------------------------------------------------------
   SPECIFIC LOADERS
   These functions are based on VCL (which is slower than WIC)
-------------------------------------------------------------------------------------------------------------}
 {$IFDEF Jpg2000}
 function  LoadJ2K (CONST FileName: string): TBitmap; {$ENDIF}
 function  LoadJpg (CONST FileName: string; Scale: TJPEGScale= jsFullSize): TBitmap;
 function  LoadPNG (CONST FileName: string): TBitmap;                           overload;
 function  LoadGIF (CONST FileName: string): TBitmap;                           overload;  { Load GIF and convert it to BMP }
 function  LoadGIF (CONST FileName: string; OUT FrameCount: Cardinal): TBitmap; overload;
 function  LoadWB1 (CONST FileName: string): TBitmap;
 function  LoadICO (CONST FileName: string): TBitmap;
 function  LoadEMF (CONST FileName: string): TBitmap;
 function  LoadBMP (CONST FileName: string): TBitmap;                                      { Loads a BMP and suppress error messages }

 // JPG THUMBNAIL LOADER
 function  ExtractThumbnailJpg(CONST FileName: string; ThumbWidth, ThumbHeight: integer; OUT ResolutionX, ResolutionY: Integer): TBitmap;                   { Extracts from a JPEG image using scaling. The scale is automatically chosen based on the original image size and required thumb size } { Old name: LoadJpgThumbnail }
 function  ExtractThumbnail   (CONST FileName: string; ThumbWidth: integer; OUT ResolutionX, ResolutionY: Integer; OUT FrameCount: Cardinal): TBitmap; overload; { Extracts the thumbnail from a gif, avi, jpg, png, etc file }
 function  ExtractThumbnail   (CONST FileName: string; ThumbWidth: Integer): TBitmap;   overload;

 function  loadGraphWic       (CONST FileName: string): TBitmap;                            { Supports: GIF, PNG, JPG. Use LoadGraph instead }

{-------------------------------------------------------------------------------------------------------------
   GRAY LOADERS
-------------------------------------------------------------------------------------------------------------}
 function  LoadGraphAsGrayScale(FileName: string): TBitmap;       overload;
 procedure LoadGraphAsGrayScale(FileName: string; BMP: TBitmap);  overload;


{-------------------------------------------------------------------------------------------------------------
   Image format utils
-------------------------------------------------------------------------------------------------------------}
 function  DetectGraphSignature(CONST FileName: string): Integer;
 function  CheckValidImage     (CONST FileName: string): Boolean;
 {$IFDEF CCRExif}
 function  GetExif             (CONST FileName: string): TExifData; {$ENDIF}


IMPLEMENTATION

USES
   {$IFDEF Jpg2000}OpenJpeg2000Bitmap,{$ENDIF} // Download OpenJpeg Pas library from: www.github.com/galfar/PasJpeg2000
   {$IFDEF FastJpg}FastJpegDecHelper,{$ENDIF}
   LightVcl.Graph.Resize, LightVcl.Graph.ResizeVCL, LightVcl.Graph.Loader.Resolution, LightVcl.Graph.UtilGray,
   LightVcl.Graph.Loader.WB1, LightVcl.Graph.Loader.RainDrop, LightCore.IO, LightVcl.Common.IO, LightVcl.Graph.FX.Rotate,
   LightCore.AppData, LightCore, LightCore.Time, LightCore.Types, LightVcl.Graph.GrabAviFrame,
   LightVcl.Graph.Gif;




{-------------------------------------------------------------------------------------------------------------
  IMAGE LOADERS

  Load image based on signature and not on file extension.
  Supported formats: GIF, JPG (+Exif), BMP, PNG, J2K, WB1
  Guarantees not to crash if the image is bad.
-------------------------------------------------------------------------------------------------------------}





{-------------------------------------------------------------------------------------------------------------
   LOAD WIC
-------------------------------------------------------------------------------------------------------------}

{ Uses Vcl.Graphics.TWICImage
  At least 8x faster than Delphi's JPG function
  Formats:
    Should work with: BMP, GIF, ICO, JPEG, PNG, TIF and Windows Media Photo
    Tested with animated GIF, PNG, JPG.
    Fails with JPEG2K, WB1
  Source: https://stackoverflow.com/questions/53382075/jpeg-to-bmp-conversion-takes-unreasonable-amount-of-time/53387410#53387410 }
function loadGraphWic(CONST FileName: string): TBitmap;   //Todo: test with BMP
VAR
   wic: TWICImage;
begin
  Result:= NIL;
  Assert(FileExistsMsg(FileName));

  wic := TWICImage.Create;
  TRY

    TRY
      wic.LoadFromFile(FileName);
    EXCEPT
      on E: Exception do
       begin
        { Don't crash on invalid images. Show the problem in log }
        AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
        EXIT(NIL);
       end;
    END;

    Result := TBitmap.Create;
    TRY
      Result.Assign(wic);
    EXCEPT
      on E: Exception do
       begin
        AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
        FreeAndNil(Result);
       end;
    END;

  FINALLY
    FreeAndNil(wic);
  END;
end;



//ToDo: open JPG in Wic and in JpegDecHelper. which one is faster?
{-------------------------------------------------------------------------------------------------------------
  MAIN LOADER

  Supports:
     GIF, PNG, JPG, JPEG2K, BMP, WB1, TIF and Windows Media Photo

  Speed:
     Fast
     The only library better is \Third party packages\JpegTurbo\ but I have to deliver a DLL.

  Decoders:
     JPG: via AsmJpegDec wich is VERY fast compared with Delphi and WIC but it is not working with all jpeg images. In this case we fall back to WIC or the standard LoadGraph loader (WIC).
     GIF: via WIC
     PNG: via WIC

  Warning:
     Looks like WIC will convert all images to pf32. I tried it with a 8bit (grayscale) bitmap and it converted the image to pf32.
-------------------------------------------------------------------------------------------------------------}
{$IFDEF CCRExif}
function GetExif(CONST FileName: string): TExifData;
begin
 Assert(FileExistsMsg(FileName));

 // Prevents this issue: Cannot open file "D:\FTP\imageWilkinDriveway.jpg". The process cannot access the file because it is being used by another process.
 // The check is better to be done in LoadGraph: if (Result <> NIL) AND ExifRotate then
 // Delete this check here as soon as it is confirmed that check works!

 Result:= TExifData.Create;
 TRY
   Result.LoadFromGraphic(FileName);
 EXCEPT
   on E: Exception do     //todo 1: trap only specific exceptions
    begin
     FreeAndNil(Result);
     AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
    end;
 END;
end;
{$ELSE}
  //RAISE Exception.Create('CCREXIF NOT AVAILABLE');
{$ENDIF}


function LoadGraph(CONST FileName: string; ExifRotate: Boolean = True; UseWic: Boolean = TRUE): TBitmap;
//todo 1: CAN I LOAD THE JPEG WITH WIC AND THEN REOPEN IT AND CHECK IF THERE IS EXIF INSIDE?
VAR
   Signature: Integer;
begin
 Assert(FileExistsMsg(FileName));

 TRY
  { Detect image by signature, not by extension }
  Signature:= DetectGraphSignature(FileName);
 EXCEPT
  on E: Exception do
   begin
    AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
    EXIT(NIL);       { Do not crash on failure }
   end;
 END;

 { Before loading an image (any image) check if its file extension matches the binary header.
   Note: No need to do it for jpeg images with png ext. The algorithm already recognizes that and loads the image just fine. }
 if IsJpg(FileName) AND (Signature= 2) then
  begin
   AppDataCore.LogWarn(FileName+ CRLF+ ' is a PNG file with invalid file extension (jpg). You can fix this by changing the extension from JPG to PNG.');
   EXIT(NIL);
  end;

 { Create the right type of loader }
 case Signature of
   { BMP}
   1 : Result:= LoadBMP(FileName); { Don't use WIC to load BMP files! For misterious reasons, WIC is terrible slow when loading bmp files! }
   { PNG}
   2 : if UseWic
       then Result:= loadGraphWic(FileName)
       else Result:= LoadPNG(FileName);
   { GIF}
   3 : if UseWic
       then Result:= loadGraphWic(FileName)
       else Result:= LoadGIF(FileName);
   { JPG}
   4 : begin
        {$IFDEF FastJpg}
          Result:= FastJpegDecHelper.FastJpgDecode(FileName);
          if Result = NIL then { Not all jpegs are supported by JpegDecHelper. In this case we fall back to WIC or the standard LoadGraph loader (WIC). }
        {$ELSE}
          AppDataCore.LogWarn('FastJpg not available! Download this library and set the compiler switch.');
        {$ENDIF}
        {ToDo 1: is JpegDecHelper indeed faster than WIC? }
          if UseWic
          then Result:= loadGraphWic(FileName)
          else Result:= LoadJpg(FileName);

        { Support for  EXIF Jpegs }
        if (Result <> NIL) AND ExifRotate then
         begin
           {$IFDEF CCRExif}
           VAR ExifData:= GetExif(FileName);
           LightVcl.Graph.FX.Rotate.RotateExif(Result, ExifData);
           FreeAndNil(ExifData);
           {$ELSE}
             AppDataCore.LogVerb('CCRExif not available! EXIF rotation skipped.');
           {$ENDIF}
         end;
       end;

   { Jpeg2000 }
   {$IFDEF Jpg2000}
     5 : Result:= LoadJ2K(FileName);
   {$ELSE}
     5 : RAISE Exception.Create('Jpeg2000 not supported yet on Win 64 bit!');
   {$ENDIF}

   { WB1}
   6 : Result:= LoadWB1(FileName);

   { RainDrop }
   7 : Result:= LoadRainShelter(FileName);
  else

    { TIF or something else? }
  Result:= loadGraphWic(FileName);
 end;
end;


{ Use it for animated files }
function LoadGraph(CONST FileName: string; OUT FrameCount: Cardinal): TBitmap;                  { FrameCount is -1 in case of error (gif cannot be decoded), 0 if the image is not a gif, 1 for static gifs and >1 for animated gifs }
VAR Signature: Integer;
begin
 Assert(FileExistsMsg(FileName));
 Signature:= DetectGraphSignature(FileName);                                                          { Detect image by signature, not by extension }
 FrameCount:= 0;

 if Signature = 3
 then Result:= LoadGIF(FileName, FrameCount)
 else Result:= LoadGraph  (FileName, True);
end;



procedure LoadGraphToImg(CONST FileName: string; Image: TImage; ExifRotate: Boolean = True; UseWic: Boolean = TRUE);
begin
    Assert(Image <> NIL, 'LoadGraphToImg: Image is NIL');
    VAR BMP:= LoadGraph(FileName, ExifRotate, UseWic);
    TRY
      if BMP <> NIL
      then Image.Picture.Assign(BMP);
    FINALLY
      FreeAndNil(BMP);
    END;
end;


function LoadGraphAsGrayScale(FileName: string): TBitmap;
begin
  Result:= LoadGraph(FileName, FALSE, TRUE);
  TRY
    if NOT HasGrayscalePalette(Result)
    then LightVcl.Graph.UtilGray.ConvertToGrayscale(Result);
  EXCEPT
    FreeAndNil(Result);
  END;
end;


procedure LoadGraphAsGrayScale(FileName: string; BMP: TBitmap);
begin
  Assert(BMP <> NIL);
  VAR Temp:= LoadGraph(FileName, FALSE, TRUE);
  TRY
    if NOT HasGrayscalePalette(Temp)
    then LightVcl.Graph.UtilGray.ConvertToGrayscale(Temp);
    BMP.Assign(Temp);
  FINALLY
    FreeAndNil(Temp);
  END;
end;



(*
{ Same as the 'standard' LoadGraph but it uses the special LoadJpg function for JPGs in order to retrieve the EXIF information.
  Use it when you need exif info from JPEG files, but there could also be other files there. }
function LoadGraph(CONST FileName: string; OUT ExifData: TExifData): TBitmap;
VAR Signature: Integer;
begin
 Assert(FileExistsMsg(FileName));
 Signature:= DetectGraphSignature(FileName);                                     { Detect image by signature, not by extension }

 if Signature = 4
 then
  begin
   Result:= LoadJpgBorland(FileName, ExifRotate, jsFullSize);
   { Support for EXIF Jpegs }
   if ExifRotate then
    begin
     ExifData:= GetExif(FileName);
     cGraphics.RotateExif(Result, ExifData);
    end;
  end
 else
    Result:= LoadGraph(FileName, True);
end;
*)






{--------------------------------------------------------------------------------------------------
   VCL LOADERS
   note: TImage.LoadFromFile supports JPG2K (confirm that)
--------------------------------------------------------------------------------------------------}

{ Loads a file directly into a TImage component }
procedure LoadToTImage(CONST FileName: string; ExifRotate: Boolean; Image: TImage);
VAR BMP: TBitmap;
begin
 Assert(Image <> NIL, 'LoadToTImage: Image is NIL');
 BMP:= LoadGraph(FileName, ExifRotate);
 TRY
   if BMP <> NIL
   then Image.Picture.Bitmap.Assign(BMP);
 FINALLY
   FreeAndNil(BMP);
 end;
end;



{ Uses TPicture's facilities to load the image.
  Does not support Jpg2K.
  Takes less RAM than LoadGraph but it is slower. }
function LoadTPicture(CONST FileName: string): TPicture;
begin
 Assert(FileExistsMsg(FileName));
 Result:= TPicture.Create;
 TRY
   Result.LoadFromFile(FileName);
 EXCEPT
   on E: Exception do     //todo 1: trap only specific exceptions
    begin
     AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
     FreeAndNil(Result);    { Don't crash on invalid images }
    end;
 end;
end;



{ Load a JPG image from a resource file }
function LoadFromResource(CONST RsrcName: string): TJPEGImage;
VAR
   Stream: TStream;
begin
 Result := TJPEGImage.Create;
 TRY
  Stream := TResourceStream.Create(HInstance, RsrcName, RT_RCDATA);
  TRY
    Result.LoadFromStream(Stream);
  FINALLY
    FreeAndNil(Stream);
  END;

 EXCEPT
  FreeAndNil(Result);
  RAISE;
 END;
end;




{--------------------------------------------------------------------------------------------------
   LOAD THUMBANAIL
--------------------------------------------------------------------------------------------------}

{ Extracts the thumbnail from a gif, avi, jpg, png, etc file. Very fast!
  If the file is animated it extracts the frame from the middle of the animation.

  The scale is automatically chosen based on the original image size and required thumb size.
  The higher the scaling factor, the faster the loading time.

  Exif:
     The image will be automatically rotated according to the EXIF data

  Parameters:
     ThumbWidth is the desired size of the thumbnail. Small images will actually be resized up if they are smaller than ThumbWidth.
     ResolutionX, ResolutionY returns the ORIGINAL resolution (before thumbnail)
     Log can be NIL

  Result:
     Returns the thumbnail or NIL in case of failure.
} // old name: LoadJpgThumbnail
function ExtractThumbnailJpg(CONST FileName: string; ThumbWidth, ThumbHeight: Integer; OUT ResolutionX, ResolutionY: Integer): TBitmap;
VAR
   Ratio: Real;
   JpgLoader: TJpegImageEx;
   SizeDecrease: Integer;
begin
 Assert(FileExistsMsg(FileName));

 JpgLoader:= TJpegImageEx.Create;
 TRY
   // Autodetect the best Scale factor
   LightVcl.Graph.Loader.Resolution.GetImageRes(FileName, ResolutionX, ResolutionY);
   if (ResolutionX < 0) OR  (ResolutionY < 0)  then EXIT(NIL);

   if ThumbHeight= -1
   then SizeDecrease:= trunc( ResolutionX/ThumbWidth )
   else SizeDecrease:= trunc( min(ResolutionX/ThumbWidth, ResolutionY/ThumbHeight) );
   case SizeDecrease of
     0..1: JpgLoader.Scale:= jsFullSize;
     2..3: JpgLoader.Scale:= jsHalf;
     4..7: JpgLoader.Scale:= jsQuarter;
   else
     JpgLoader.Scale:= jsEighth;
   end;

   TRY
     JpgLoader.LoadFromFile(FileName);
   EXCEPT
     on E: Exception do  { Don't crash on invalid images. Common encountered errors: EInvalidGraphic (JPEG error #53), EReadError (Stream read error), etc }                                                                                     //todo: trap only specific exceptions
      begin
       AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
       EXIT(NIL);
      end;
   END;

   // We don't want small or huge images
   if (JpgLoader.Width <= 0) or (JpgLoader.Width  > 32768)
   or (JpgLoader.Height<= 0) or (JpgLoader.Height > 32768)
   then Exit(NIL);

   // We have the thumbnail but it might not have the EXACT size we specified in the ExtractThumbnailJpg so we resize one more time
   Result:= TBitmap.Create;
   TRY
     Result.HandleType:= bmDIB;
     Result.Assign(JpgLoader);  // This is where the (decoding) time is actually wasted.

     {$IFDEF CCRExif}
     // Rotate EXIF
     LightVcl.Graph.FX.Rotate.RotateExif(Result, JpgLoader.ExifData);
     {$ENDIF}

     if ThumbHeight= -1 then
      begin
        Ratio:= ResolutionX / ResolutionY;
        ThumbHeight:= Round(ThumbWidth / Ratio);
      end;

     SmartStretch(Result, ThumbWidth, ThumbHeight);
   EXCEPT
     on E: Exception do      { Don't crash on invalid images. Common encountered errors: EInvalidGraphic (JPEG error #53), EReadError (Stream read error), etc }                                                                                     //todo: trap only specific exceptions
      begin
       AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
       FreeAndNil(Result);   { We only free the result in case of failure }
      end;
   END;

 FINALLY
   FreeAndNil(JpgLoader);
 end;
end;


{-------------------------------------------------------------------------------
   Same as above
     BUT In this function, the Height will be ignored. The thumbnail will be proportionally stretched to fit into the size specified by ThumbWidth
     If you set ThumbHeight to -1 then the Height will be ignored.
     The thumbnail will be proportionally stretched to fit into the size specified by ThumbWidth

   Called on Playlist.DrawCell or Playlist.Click
-------------------------------------------------------------------------------}
function ExtractThumbnail(const FileName: string; ThumbWidth: integer; OUT ResolutionX, ResolutionY: Integer; OUT FrameCount: Cardinal): TBitmap;
begin
 Assert(FileExistsMsg(FileName));
 FrameCount:= 1;   // 1 for: jpeg, png, normal gif, bmp

 { TRY TO OPEN STATIC/ANIMATED IMAGE }
 if IsAnimated(FileName)
 then
   begin
    Result:= LightVcl.Graph.GrabAviFrame.GetVideoPlayerLogo;  // Returns placeholder for animated files (GIF/AVI)
    FrameCount:= 2;   // 2 for avi/gif
   end
 else
    if IsJpg(FileName)
    then
     begin
      Result:= ExtractThumbnailJpg(FileName, ThumbWidth, -1, ResolutionX, ResolutionY); // This will do "RotateEXIF"
      EXIT;     // Jpegs are special because a thumbnail was already generated for them by ExtractThumbnailJpg. So, we do no further processing
     end
    else
     Result:= LoadGraph(FileName, TRUE);

 { GET RESOLUTION }
 if  (Result<> NIL)
 and (Result.Height > 0)                                                          { Tratez cazul Ric Koval - totusi nu ar trebuie sa mi sa intample asta niciodata }
 and (Result.Width  > 0)
 then
   begin
     // Get resolution before we resize !!!
     ResolutionX:= Result.Width;
     ResolutionY:= Result.Height;

     // Resize
     StretchProport(Result, ThumbWidth)
   end
 else
   begin
     ResolutionX:= -1;
     ResolutionY:= -1;
     FrameCount:= 0;     { 0 = extraction error in ConvertGIF. 1= static gif/jpg. >1= animated gif }
   end;
end;


{ Save as above but it does not return the original image resolution }
function ExtractThumbnail(CONST FileName: string; ThumbWidth: Integer): TBitmap;
VAR ResolutionX, ResolutionY: Integer;
    FrameCount: Cardinal;
begin
 Result:= ExtractThumbnail(FileName, ThumbWidth, ResolutionX, ResolutionY, FrameCount);
end;







{---------------------------------------------------------------------------------------------------
   LOAD JPG
----------------------------------------------------------------------------------------------------
   Speed:
      This function is too slow. Use WIC instead. A 1.44MB jpeg file takes 4.2 seconds to load.
   Thumbnail:
      The function can load the JPEG at a reduced scale with 'Scale'
   Exif:
       The image is automatically rotated if RotateExif is true.
       The Exif data is output to ExifData. You MUST free the object after using the function!
---------------------------------------------------------------------------------------------------}

function LoadJpg(CONST FileName: string; Scale: TJPEGScale = jsFullSize): TBitmap;
VAR
   JPG: TJpegImage;
begin
 Assert(FileExistsMsg(FileName));

 JPG:= TJpegImage.Create;
 TRY
   TRY
     JPG.Scale:= Scale;           // jsFullSize, jsHalf, jsQuarter, jsEighth
     JPG.LoadFromFile(FileName);
   EXCEPT
     on E: Exception do  { Don't crash on invalid images. Common encountered errors: EInvalidGraphic (JPEG error #53), EReadError (Stream read error), etc }                                                                                     //todo: trap only specific exceptions
      begin
       AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
       FreeAndNil(JPG);   { We only free the result in case of failure }
       EXIT(NIL);
      end;
   END;

   // We don't want small or huge images
   if (JPG.Width <= 0) or (JPG.Width  > 32768)
   or (JPG.Height<= 0) or (JPG.Height > 32768)
   then Exit(NIL);

   Result:= TBitmap.Create;
   TRY
     Result.PixelFormat:= pf24bit;  //? necessary or is default 24?
     Result.HandleType := bmDIB;
     Result.Assign(JPG);  // This takes 4 sec
   EXCEPT
     on E: Exception do  { Don't crash on invalid images. Common encountered errors: EInvalidGraphic (JPEG error #53), EReadError (Stream read error), etc }                                                                                     //todo: trap only specific exceptions
      begin
       AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
       FreeAndNil(Result);   { We only free the result in case of failure }
      end;
   end;

 FINALLY
   FreeAndNil(JPG);
 end;
end;







{$IFDEF Jpg2000}
function LoadJ2K(CONST FileName: string): TBitmap;
VAR JP2: TJpeg2000Bitmap;
begin
 Assert(FileExistsMsg(FileName));

 JP2:= TJpeg2000Bitmap.Create;
 TRY

   TRY
     // JP2.Scale:= Scale;           // jsFullSize, jsHalf, jsQuarter, jsEighth
     JP2.LoadFromFile(FileName);
   EXCEPT
     on E: Exception do  { Don't crash on invalid images. Common encountered errors: EInvalidGraphic (JPEG error #53), EReadError (Stream read error), etc }                                                                                     //todo: trap only specific exceptions
      begin
       AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
       FreeAndNil(JP2);   { We only free the result in case of failure }
       EXIT(NIL);
      end;
   end;

   if (JP2.Width <= 0) or (JP2.Width  > 32768)
   or (JP2.Height<= 0) or (JP2.Height > 32768)
   then Exit(NIL);

   Result:= TBitmap.Create;
   TRY
     Result.Assign(JP2)
   EXCEPT
     on E: Exception do  { Don't crash on invalid images. Common encountered errors: EInvalidGraphic (JPEG error #53), EReadError (Stream read error), etc }                                                                                     //todo: trap only specific exceptions
      begin
       AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
       FreeAndNil(Result);   { We only free the result in case of failure }
      end;
   end;

 FINALLY
   FreeAndNil(JP2);
 end;
end;
{$ENDIF}





{--------------------------------------------------------------------------------------------------
   LOAD GIF
--------------------------------------------------------------------------------------------------}

function LoadGIF(CONST FileName: string): TBitmap;           { Load GIF and convert it to BMP }
var Dummy: Cardinal;
begin
 Result:= LoadGIF(FileName, Dummy);
end;


{ Load GIF and convert it to BMP.
  It also returns the number of frames }
function LoadGIF(CONST FileName: string; OUT FrameCount: Cardinal): TBitmap;
VAR GIF: TGIFImage;
begin
 Assert(FileExistsMsg(FileName));

 Result:= NIL;
 FrameCount:= 0;   { 0 means error. Do not change this value. bxWallpaper relies on it to detect if the frame count was extracted (FrameCount>0) or not (FrameCount= 0) }

 GIF:= TGIFImage.Create;
 TRY

   TRY
     GIF.LoadFromFile(FileName);    { This takes ~12sec for a 50MB GIF }
   EXCEPT
     on E: Exception do  { Don't crash on invalid images. Common encountered errors: EInvalidGraphic (JPEG error #53), EReadError (Stream read error), etc }                                                                                     //todo: trap only specific exceptions
      begin
       AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
       FreeAndNil(GIF);   { We only free the result in case of failure }
       EXIT(NIL);
      end;
   END;

   // We don't want small or huge images
   if (gif.Width <= 0) or (gif.Width  > 32768)
   or (gif.Height<= 0) or (gif.Height > 32768)
   then Exit(NIL);

   Result:= TBitmap.Create;
   TRY
     Result.Assign(GIF);
     FrameCount:= Gif.Images.Count; { This returns 1 for static images }
   EXCEPT
     on E: Exception do  { Don't crash on invalid images. Common encountered errors: EInvalidGraphic (JPEG error #53), EReadError (Stream read error), etc }                                                                                     //todo: trap only specific exceptions
      begin
       AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
       FreeAndNil(Result);   { We only free the result in case of failure }
      end;
   end;

 FINALLY
   FreeAndNil(GIF);
 end;
end;






{--------------------------------------------------------------------------------------------------
   LOAD PNG
--------------------------------------------------------------------------------------------------}
TYPE
  TPNGObject= class(TPNGImage);

function LoadPNG(CONST FileName: string): TBitmap;
VAR PNG: TPNGObject;
begin
 Assert(FileExistsMsg(FileName));

 PNG:= TPNGObject.Create;
 TRY

   TRY
     PNG.LoadFromFile(FileName);
   EXCEPT
     on E: Exception do  { Don't crash on invalid images. Common encountered errors: EInvalidGraphic (JPEG error #53), EReadError (Stream read error), etc }                                                                                     //todo: trap only specific exceptions
      begin
       AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
       FreeAndNil(PNG);   { We only free the result in case of failure }
       EXIT(NIL);
      end;
   end;

   // We don't want small or huge images
   if (PNG.Width <= 0) or (PNG.Width  > 32768)
   or (PNG.Height<= 0) or (PNG.Height > 32768)
   then Exit(NIL);

   Result:= TBitmap.Create;
   TRY
     Result.Assign(PNG)
   EXCEPT
     on E: Exception do  { Don't crash on invalid images. Common encountered errors: EInvalidGraphic (JPEG error #53), EReadError (Stream read error), etc }                                                                                     //todo: trap only specific exceptions
      begin
       AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
       FreeAndNil(Result);   { We only free the result in case of failure }
      end;
   end;

 FINALLY
   FreeAndNil(PNG);
 end;
end;


(*procedure LoadPNG(CONST FileName: string; BMP: TBitmap);   { The BMP object must exist }
VAR PNG: TPNGObject;
begin
 if not fileexists(FileName) then raise__;
 PNG:= TPNGObject.Create;
 TRY
  TRY

   TRY
     PNG.LoadFromFile(FileName);
   except_
     EXIT(NIL);    { Don't crash on invalid images }
   end;

   { if (PNG.Width < 1) OR (PNG.Width >= 32768)
     OR (PNG.Height< 1) OR (PNG.Height >= 32768)
     then EXIT(NIL); }
   BMP.Assign(PNG)

   EXCEPT
     on E: Exception do  { Don't crash on invalid images. Common encountered errors: EInvalidGraphic (JPEG error #53), EReadError (Stream read error), etc }                                                                                     //todo: trap only specific exceptions
      begin
       AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
       FreeAndNil(Result_);   { We only free the result in case of failure }
      end;
   END;
 FINALLY
   FreeAndNil(PNG);
 end;
end; *)




{-------------------------------------------------------------------------------------------------------------
   LOAD OTHERS
-------------------------------------------------------------------------------------------------------------}
{ Loads a BMP and suppress error messages }
function LoadBMP(CONST FileName: string): TBitmap;
begin
  Assert(FileExistsMsg(FileName));

  Result:= TBitmap.Create;
  TRY
    Result.LoadFromFile(FileName);
   EXCEPT
     on E: Exception do  { Don't crash on invalid images. Common encountered errors: EInvalidGraphic (JPEG error #53), EReadError (Stream read error), etc }                                                                                     //todo: trap only specific exceptions
      begin
       AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
       FreeAndNil(Result);   { We only free the result in case of failure }
       EXIT(NIL);
      end;
   end;

  if (Result.Width< 1)
  OR (Result.Height< 1)
  then FreeAndNil(Result);
end;


function LoadEMF(CONST FileName: string): TBitmap;                                          { Converts a Enhanced Metafile (*BMP }
VAR Metafile: TMetafile;
    MetaCanvas: TMetafileCanvas;
begin
  Assert(FileExistsMsg(FileName));
  Result:= NIL;

  Metafile := TMetaFile.Create;
  TRY

    TRY
      Metafile.LoadFromFile(FileName);
   EXCEPT
     on E: Exception do  { Don't crash on invalid images. Common encountered errors: EInvalidGraphic (JPEG error #53), EReadError (Stream read error), etc }                                                                                     //todo: trap only specific exceptions
      begin
       AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
       FreeAndNil(MetaFile);   { We only free the result in case of failure }
       EXIT(NIL);
      end;
   end;

    MetaCanvas:= TMetafileCanvas.Create(Metafile, 0);
    TRY
     Result:= TBitmap.Create;
     TRY
      Result.Height:= Metafile.Height;
      Result.Width := Metafile.Width;
      Result.Canvas.Draw(0, 0, Metafile);
     EXCEPT
       on E: Exception do  { Don't crash on invalid images. Common encountered errors: EInvalidGraphic (JPEG error #53), EReadError (Stream read error), etc }                                                                                     //todo: trap only specific exceptions
        begin
         AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
         FreeAndNil(Result);   { We only free the result in case of failure }
        end;
     end;

    FINALLY
      FreeAndNil(MetaCanvas);   { We only free the result in case of failure }
    end;

  FINALLY
    FreeAndNil(Metafile);
  end;
end;


function LoadWB1(CONST FileName: string): TBitmap;
VAR WB1: LightVcl.Graph.Loader.WB1.TWb1Obj;
begin
 Assert(FileExistsMsg(FileName));

 WB1:= TWb1Obj.Create;
 TRY

   TRY
     WB1.LoadFromFile(FileName);
   EXCEPT
     on E: Exception do  { Don't crash on invalid images. Common encountered errors: EInvalidGraphic (JPEG error #53), EReadError (Stream read error), etc }                                                                                     //todo: trap only specific exceptions
      begin
       AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
       FreeAndNil(WB1);   { We only free the result in case of failure }
       EXIT(NIL);
      end;
   end;

   // We don't want small or huge images
   if (WB1.InternalJPG.Width <= 0) or (WB1.InternalJPG.Width  > 32768)
   or (WB1.InternalJPG.Height<= 0) or (WB1.InternalJPG.Height > 32768)
   then Exit(NIL);

   Result:= TBitmap.Create;
   TRY
     Result.Assign(WB1.InternalJPG)
   EXCEPT
     on E: Exception do  { Don't crash on invalid images. Common encountered errors: EInvalidGraphic (JPEG error #53), EReadError (Stream read error), etc }                                                                                     //todo: trap only specific exceptions
      begin
       AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
       FreeAndNil(Result);   { We only free the result in case of failure }
      end;
   end;

 FINALLY
   FreeAndNil(WB1);
 end;
end;


function LoadICO(CONST FileName: string): TBitmap;
VAR ICO1: TIcon;
begin
 Assert(FileExistsMsg(FileName));
 Result:= NIL;

 ICO1:= TIcon.Create;
 TRY

   TRY
     ICO1.LoadFromFile(FileName);
   EXCEPT
     on E: Exception do  { Don't crash on invalid images. Common encountered errors: EInvalidGraphic (JPEG error #53), EReadError (Stream read error), etc }                                                                                     //todo: trap only specific exceptions
      begin
       AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
       FreeAndNil(ICO1);   { We only free the result in case of failure }
       EXIT(NIL);
      end;
   end;

   Result:= TBitmap.Create;
   TRY
     Result.Assign(ICO1);
   EXCEPT
     on E: Exception do  { Don't crash on invalid images. Common encountered errors: EInvalidGraphic (JPEG error #53), EReadError (Stream read error), etc }                                                                                     //todo: trap only specific exceptions
      begin
       AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
       FreeAndNil(Result);   { We only free the result in case of failure }
      end;
   end;

 FINALLY
   FreeAndNil(ICO1);
 end;
end;






{-------------------------------------------------------------------------------------------------------------
   DetectGraphSignature
-------------------------------------------------------------------------------------------------------------}
function CheckValidImage (CONST FileName: string): Boolean;
VAR BMP: TBitmap;
begin
  BMP:= LoadGraph(FileName, True);
  Result := BMP <> NIL;
  FreeAndNil(BMP);
end;


//Source: http://stackoverflow.com/questions/959160/load-jpg-gif-bitmap-and-convert-to-bitmap
function DetectGraphSignature(CONST FileName: string): Integer;
VAR
  FS: TFileStream;
  FirstBytes: AnsiString;
begin
 if IsJp2(FileName)           {ToDo 5: detect J2K and WB1 by signature }
 then Result:= 5
 else

 if IsWB1(FileName)           {ToDo 5: detect J2K and WB1 by signature }
 then Result:= 6
 else

   { Detect by signature }
   begin
     Assert(FileExistsMsg(FileName));
     FS:= TFileStream.Create(FileName, fmOpenRead OR fmShareDenyNone);  { This could fail if the file is locked }
     TRY

       TRY
         SetLength(FirstBytes, 8);
         FS.Read(FirstBytes[1], 8);

         if system.COPY(FirstBytes, 1, 2) = 'BM'                                                    { BMP }
         then Result:= 1
         else
          if FirstBytes = #137'PNG'#13#10#26#10                                                     { PNG }
          then Result:= 2
          else
            if system.COPY(FirstBytes, 1, 3) = 'GIF'                                                { GIF }
            then Result:= 3
            else
              if system.COPY(FirstBytes, 1, 2) = #$FF#$D8                                           { JPG }
              then Result:= 4
              else
                if system.COPY(FirstBytes, 1, 7) = RainMagicNo                                      { RainShelter }
                then Result:= 7
                else Result:= 0;
      EXCEPT
        on E: Exception do  { Don't crash on invalid images. Common encountered errors: EInvalidGraphic (JPEG error #53), EReadError (Stream read error), etc }                                                                                     //todo: trap only specific exceptions
         begin
          AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
          Result:= -1;   { We only free the result in case of failure }
         end;
      END;

     FINALLY
       FreeAndNil(FS);
     END;
   end;
end;


{ Not tested!
  Also see: http://smatters.com/dcraw/  }
function DetectGraphSignatureWIC(const WIC: TWICImage): string;
begin
  Assert(WIC <> NIL, 'DetectGraphSignatureWIC: WIC is NIL');
  Result:= '';

  case WIC.ImageFormat of
    wifBmp     : Result := 'Bitmap';
    wifPng     : Result := 'PNG';
    wifJpeg    : Result := 'JPEG';
    wifGif     : Result := 'GIF';
    wifTiff    : Result := 'TIFF';
    wifWMPhoto : Result := 'JPEG XR';
    wifOther   :
    begin
      if GUIDToString(WIC.EncoderContainerFormat) = GUIDToString(GUID_ContainerFormatIco)
      then Result := 'Icon'
      else Result := 'other';
    end;
  end;
end;



end.
