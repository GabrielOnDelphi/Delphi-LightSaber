UNIT LightVcl.Graph.Loader.WBC;

{=============================================================================================================
   Gabriel Moraru
   2026.09.10
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Decoder for WBC file format - the VCL half.

   The decoder itself is framework-neutral and lives in LightCore.Graph.Loader.WBC.pas (class TWbcObj).
   Only GetJpeg stays here, because TJpegImage is declared in vcl.imaging.Jpeg and the RTL has no
   framework-neutral JPEG class. It is a class helper, so WbcObj.GetJpeg(0) still compiles unchanged.

   How to use it:
     USES LightCore.Graph.Loader.WBC, LightVcl.Graph.Loader.WBC;

     WbcObj:= TWbcObj.Create;
     WbcObj.LoadFromFile('test.wbc');
     Jpg:= WbcObj.GetJpeg(0);

--------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Classes, vcl.imaging.Jpeg,
  LightCore.Graph.Loader.WBC;

TYPE
  TWbcObjHelper = class helper for TWbCObj
    function GetJpeg(CONST Index: Integer): TJpegImage;                                            { Get access to the specified JPEG }
  end;


IMPLEMENTATION


{--------------------------------------------------------------------------------------------------
   ACCESS
--------------------------------------------------------------------------------------------------}

function TWbcObjHelper.GetJpeg(CONST Index: Integer): TJpegImage;
VAR Stream: TMemoryStream;
begin
 Stream:= GetJpgStream(Index);
 TRY
  Result:= TJpegImage.Create;
  TRY
    Stream.Position:= 0;
    Result.LoadFromStream(stream);
  EXCEPT
    FreeAndNil(Result);   { Don't leak the result when the stream contains a broken JPEG }
    RAISE;
  END;
 FINALLY
  FreeAndNil(Stream);
 END;
end;


end.
