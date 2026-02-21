unit FastJpegDecHelper;
{-------------------------------------------------------------------------------------------------------------
   Heracle BioSoft SRL
   2026.02.21

   Helper functions for jpegdec

   Used by: cGraphLoader.pas
   Not all jpegs are supported by JpegDecHelper. In this case we fall back to WIC or the standard LoadGraph loader (WIC).
   Tester: c:\Myprojects\Project Testers\gr LoadGraph\
-------------------------------------------------------------------------------------------------------------}
 
INTERFACE

USES
  Winapi.Windows, System.Classes, SysUtils, VCL.Graphics, FastJpegDec;

function FastJpgDecode(FileName: string; OUT ErrorType: string): TBitmap;  overload;
function FastJpgDecode(FileName: string): TBitmap;                         overload;



IMPLEMENTATION


function FastJpgDecode(FileName: string; OUT ErrorType: string): TBitmap;
VAR
   Img: PJpegDecode;
   res: TJpegDecodeError;
   Stream: TMemoryStream;
begin
  Img:= NIL;
  Result:= NIL;
  Stream:= TMemoryStream.Create;
  TRY
    if Length(FileName) > MAX_PATH then { TMemoryStream does not support long paths }
     begin
      ErrorType:= 'File name too long!';
      EXIT(NIL);
     end;

    TRY
     Stream.LoadFromFile(FileName);
    EXCEPT
     { Here we get an error when the file is blocked: "Cannot open file D:\FTP\imageVeeriMac.jpg. The process cannot access the file because it is being used by another process"
       It is not our fault that the file is blocked so we swalow this exeption. }
     ON E: EFOpenError DO
      begin
       //cvRichLog.AppLog.AddError(E.Message);
       EXIT(NIL);
      end;
    END;

    if Stream.Size < 10 then
     begin
      ErrorType:= 'File is too small!';
      EXIT(NIL);
     end;

    Stream.Position:= 0;
    TRY
      res:= JpegDecode(Stream.Memory, Stream.Size, Img);          //I could also use: res:= JpegDecode(Stream, Stream.Size);
      case res of
       JPEG_SUCCESS:
        begin
         TRY
          Result:= Img.ToBitmap; // This will raise an EOutOfResources for large files!
         EXCEPT
          //me: This will cause a known memory leak: https://stackoverflow.com/questions/60670632/why-do-i-leak-memory-in-eoutofresources?noredirect=1#comment107345449_60670632
          on EOutOfResources do
             ErrorType:= 'JPEG_OUTOFMEM!';
         END;
        end;

       JPEG_EOF                : ErrorType:= 'JPEG_EOF!';
       JPEG_OUTOFMEM           : ErrorType:= 'JPEG_OUTOFMEM!';
       JPEG_CPUNOTSUPPORTED    : ErrorType:= 'JPEG_CPUNOTSUPPORTED!';
       JPEG_BADFILE            : ErrorType:= 'JPEG_BADFILE!';
       JPEG_FORMATNOTSUPPORTED : ErrorType:= 'JPEG_FORMATNOTSUPPORTED!';       // Not all jpegs are supported. In this case the caller should fall back to WIC.
      end;
    EXCEPT
      on E: Exception DO //me: I had a case where an 'Invalid floating point operation' was caused by an invalid jpg
        ErrorType:= E.message;
    END;
  FINALLY
    if Img <> nil
    then Img.Free; //me: this is a magic method of TJpegDecode! It does not overide the classic "Free". So it cannot be replaced my FreeAndNil(). We also musch check for NIL before we call Free on it.
    FreeAndNil(Stream);
  END;
end;


function FastJpgDecode(FileName: string): TBitmap;
VAR ErrorType: string; // dummy
begin
  Result:= FastJpgDecode(FileName, ErrorType);
end;

end.
