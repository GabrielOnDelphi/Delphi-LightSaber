UNIT ccEncodeMime;

{=============================================================================================================
   2023.01
   www.GabrielMoraru.com
   See Copyright file
==============================================================================================================

   MIME ENCODING LIB
   Expands EncdDec.pas

   Don't use EncdDecd directly it was updated for Unicode:
       http://stackoverflow.com/questions/21883152/how-to-encode-strings-with-encddec-library/21883731#21883731

=============================================================================================================}

INTERFACE

USES
  System.Classes, SysUtils, Soap.EncdDecd;

 function MimeString           (CONST Input: string): string;
 function DeMimeString         (CONST Input: string): string;

 function MimeStringA          (const Input: AnsiString): AnsiString;
 function DeMimeStringA        (const Input: AnsiString): AnsiString;


IMPLEMENTATION


USES
   ccStreamMem;



{--------------------------------------------------------------------------------------------------
   MIME ENCODE
--------------------------------------------------------------------------------------------------}
function MimeString(const Input: string): string;                                                             { Solution: David H }
var
  InStr, OutStr: TStringStream;
begin
  InStr := TStringStream.Create(Input, TEncoding.UTF8);
  TRY
    OutStr := TStringStream.Create('');
    TRY
      EncodeStream(InStr, OutStr);
      Result := OutStr.DataString;
    FINALLY
      FreeAndNil(OutStr);
    end;
  FINALLY
    FreeAndNil(InStr);
  end;
end;

function DeMimeString(const Input: string): string;
var
  InStr, OutStr: TStringStream;
begin
  InStr := TStringStream.Create(Input);
  try
    OutStr := TStringStream.Create('', TEncoding.UTF8);
    try
      DecodeStream(InStr, OutStr);
      Result := OutStr.DataString;
    finally
      FreeAndNil(OutStr);
    end;
  finally
    FreeAndNil(InStr);
  end;
end;





function MimeStringA(const Input: AnsiString): AnsiString;                                       { Used by cpCertificate.pas }
var
  InStr, OutStr: TCubicMemStream;
begin
  InStr := TCubicMemStream.Create;
  try
    InStr.WriteChars(Input);
    InStr.Position:= 0;

    OutStr := TCubicMemStream.Create;
    try
      EncodeStream(InStr, OutStr);

      OutStr.Position:= 0;
      Result := OutStr.ReadStringA(OutStr.Size);
    finally
      FreeAndNil(OutStr);
    end;
  finally
    FreeAndNil(InStr);
  end;
end;


function DeMimeStringA(const Input: AnsiString): AnsiString;
var
  InStr, OutStr: TCubicMemStream;
begin
  InStr := TCubicMemStream.Create;
  TRY
    InStr.WriteChars(Input);
    InStr.Position:= 0;

    OutStr := TCubicMemStream.Create;
    try
      DecodeStream(InStr, OutStr);

      OutStr.Position:= 0;
      Result := OutStr.ReadStringA(OutStr.Size);
    FINALLY
      FreeAndNil(OutStr);
    end;
  FINALLY
    FreeAndNil(InStr);
  end;
end;


end.
