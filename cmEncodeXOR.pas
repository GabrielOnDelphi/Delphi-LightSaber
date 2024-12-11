UNIT cmEncodeXOR;

{=============================================================================================================
   2024.12
   See Copyright.txt
==============================================================================================================

   RUDIMENTARY XOR STRINGS ENCRYPTION
   Don't use it to protect sensitive data!

=============================================================================================================}

INTERFACE

USES
   System.Classes, System.SysUtils, {System.NetEncoding, Soap.EncdDecd, }ccCore;

 { CHAR_SHIFT ENCRYPTION }
 function  SimpleDecode      (CONST s: string): string;
 function  SimpleEncode      (CONST s: string): string;
 function  EncodeDecode_NOT  (CONST s: string): string;

 { A BIT BETTER ENCRYPTION }
 function  BetterEncode      (CONST s: string; CONST StartKey: WORD): string;
 function  BetterDecode      (CONST s: string; CONST StartKey: WORD): string;

 { XOR ENCRYPTION}
 function  EncodeDecode_XOR  (CONST s: string         ; Key: Byte): string;      overload;     { Torry Encode/Decode}
 function  EncodeDecode_XOR  (CONST Bytes: TBytesArray; Key: Byte): TBytesArray; overload;
 function  EncodeDecode_XOR  (CONST s: AnsiString     ; Key: Byte): AnsiString;  overload

 function  EncodeXorText     (CONST PlainText: string; Key: Byte): string;
 function  DecodeXorText     (CONST EncodedArr: array of Byte; Key: Byte): string;

IMPLEMENTATION


{--------------------------------------------------------------------------------------------------
   It shifts all ASCII codes with 1
--------------------------------------------------------------------------------------------------}
function SimpleDecode(CONST s: string): string;
VAR i: Integer;
    CurChar: BYTE;
begin
 Result:= '';
 for i:= 1 TO Length(s) DO
  begin
   CurChar:= ord(s[i]);
   if CurChar= 0
   then Result:= Result+ 'ÿ'    { ÿ is #255 }
   else Result:= Result+ char(curchar-1);
  end;
end;

function SimpleEncode(CONST s: string): string;
VAR i: Integer;
    CurChar: BYTE;
begin
 Result:= '';
 for i:= 1 TO Length(s) DO
  begin
   CurChar:= ord(s[i]);
   if CurChar= 255
   then Result:= Result+ #0
   else Result:= Result+ char(curchar+1);
  end;
end;



{--------------------------------------------------------------------------------------------------
   NOT ENCRYPTION
--------------------------------------------------------------------------------------------------}
function EncodeDecode_NOT(CONST s: String) : string;
VAR CharIndex : integer;
begin
  Result:= s;
  for CharIndex:= 1 to Length(s) DO
    Result[CharIndex]:= chr(NOT (ord(s[CharIndex])));
end;




{--------------------------------------------------------------------------------------------------
   Advantages: The result is always alphanumeric. Example: 12345 -> 364944BC8C
   Disadvantage: The resulted encodded string is twice as long as the input string
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




{--------------------------------------------------------------------------------------------------
   XOR ENCRYPTION
--------------------------------------------------------------------------------------------------}
{ToDo: Test it with Unicode text! }
{ Use any byte number as key. }
function EncodeDecode_XOR(CONST s: string; Key: Byte): string;
VAR i: Integer;
begin
  Result:= s;
  for i:= 1 to Length(s)     { indexed in 1 }
    DO Result[i]:= Char(Key XOR Ord(s[i]));
end;


function EncodeDecode_XOR(CONST Bytes: TBytesArray; Key: Byte): TBytesArray;
VAR i: Integer;
begin
  Result:= Bytes;
  for i:= 0 to High(Bytes)    { indexed in 0 }
    DO Result[i]:= Key XOR Bytes[i];
end;


function EncodeDecode_XOR(CONST s: AnsiString; Key: Byte): AnsiString;
VAR i: Integer;
begin
  Result:= s;
  for i:= 1 to Length(s)
    DO Result[i]:= AnsiChar(Key XOR Ord(s[i]));
end;





{ Utility for the above XOR functions.
  Use it to encode text/passwords that you want to store in your PAS files as const strings.
  Usually you should not do that because anyone can look into the exe file with a hax editor.

  Generates a line of Delphi code like this:
       const X: array[0..2] of Byte = ($54, $4D, $59);

  Example don't:
     const Psw = 'ABC'; // Don't do this

  Example do:
     const Psw = array[0..2] of Byte = ($22, $21, $20);  // The ABC text encoded. Use the EncodeXorText to obtain this line of code.
}

function EncodeXorText(CONST PlainText: string; Key: Byte): string;
begin
  VAR Encoded:= EncodeDecode_XOR(PlainText, 99);
  Result:= 'array[0..'+IntToStr(Length(Encoded)-1)+'] of Byte = (';

  VAR sOut:= '';
  for VAR I := 1 to Length(Encoded) do
    begin
      sOut := sOut + '$'+ IntToHex( Ord(Encoded[i]) );
      if I < Length(Encoded)
      then sOut := sOut + ', ';     // Add comma except for the last element
    end;

  sOut:= ReplaceString(sOut, '$0', '$');  // Too many zeroes. Remove unused ones to make the string shorter.
  sOut:= ReplaceString(sOut, '$0', '$');

  Result:= Result+ sOut+ ');';
end;


{ Converts the text encoded by the function above, back to the original text }
function DecodeXorText(CONST EncodedArr: array of Byte; Key: Byte): string;
VAR I: Integer;
begin
  SetLength(Result, Length(EncodedArr));
  for I := 0 to High(EncodedArr) do
    Result[I + 1] := Char(EncodedArr[I] XOR Key); // XOR with the same key
end;



end.




