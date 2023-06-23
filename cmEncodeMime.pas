UNIT cmEncodeMime;

{=============================================================================================================
   2023.01
   See Copyright.txt
==============================================================================================================

   MIME ENCODING LIB
   Expands EncdDec.pas

   Don't use EncdDecd directly it was updated for Unicode:
       http://stackoverflow.com/questions/21883152/how-to-encode-strings-with-encddec-library/21883731#21883731

=============================================================================================================}

INTERFACE

USES
  System.Classes, SysUtils;

 { MIME }
 function MimeString           (CONST Input: string): string;
 function DeMimeString         (CONST Input: string): string;

 function MimeStringA          (const Input: AnsiString): AnsiString;
 function DeMimeStringA        (const Input: AnsiString): AnsiString;

 { ENCRYPTION }
 function  BetterEncode        (CONST s: string; CONST StartKey: WORD): string;
 function  BetterDecode        (CONST s: string; CONST StartKey: WORD): string;
 function  EncodeDecode_NOT    (CONST s: string): string;   { Rudimentary Encryption }


IMPLEMENTATION


USES
   Soap.EncdDecd, ccStreamMem;




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





function MimeStringA(const Input: AnsiString): AnsiString;                                       { Used by cpProteusCertificate.pas }
var
  InStr, OutStr: TCubicMemStream;
begin
  InStr := TCubicMemStream.Create;
  try
    InStr.WriteCharacters(Input);
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
    InStr.WriteCharacters(Input);
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






{--------------------------------------------------------------------------------------------------
   RUDIMENTARY STRING ENCRYPTION
--------------------------------------------------------------------------------------------------}
function EncodeDecode_NOT(CONST s: String) : string;
VAR CharIndex : integer;
begin
  Result:= s;
  for CharIndex:= 1 to Length(s) DO
    Result[CharIndex]:= chr(NOT (ord(s[CharIndex])));
end;





{--------------------------------------------------------------------------------------------------
   STRING ENCRYPTION

   Disadvantage: The resulted encodded string is twice as long as the input string
   Advantages: The result is always alphanumeric. Example: 12345 -> 364944BC8C
--------------------------------------------------------------------------------------------------}
{$R-}
{$Q-}
CONST
   FixedEncodeConstant= 75845;    { If you change this constant, any previous encoded string will decode properly anymore }


function BetterEncode (CONST s: string; CONST StartKey: WORD): string;                             { Encrypt }
VAR b  : BYTE;
    i  : INTEGER;
    key: WORD;
BEGIN
 key := StartKey;
 RESULT := '';
 FOR i := 1 TO LENGTH(s) DO
  Begin
   b := BYTE(s[i]) XOR (key SHR 8);
   key := (b + key) * FixedEncodeConstant;
   RESULT := RESULT + IntToHex(b, 2)
  End
END;


function BetterDecode (CONST s: string; CONST StartKey: WORD): string;                             { Decrypt }
VAR b  : BYTE;
    i  : INTEGER;
    key: WORD;
BEGIN
 Result:= '';

 if Length(s) MOD 2 <> 0
 then RAISE exception.Create('The length of the input string to decrypt must be divisible by 2');

 key:= StartKey;
 RESULT:= '';
 for i:= 1 TO LENGTH(s) DIV 2 DO
  BEGIN
   b:= StrToIntDef('$' + system.COPY(s, 2*i-1, 2), 0);
   Result:= Result + CHAR( b XOR (key SHR 8) );
   key:= (b + key) * FixedEncodeConstant
  END
END;

{$R+}
{$Q+}


end.
