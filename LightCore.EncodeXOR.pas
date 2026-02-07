UNIT LightCore.EncodeXOR;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
==============================================================================================================

   RUDIMENTARY XOR STRING ENCRYPTION
   Don't use it to protect sensitive data!

   Warning: SimpleDecode/SimpleEncode and EncodeDecode_NOT only work correctly with ASCII characters (0-255).
   Unicode characters above 255 will be truncated. Use the XOR functions with AnsiString for binary-safe operations.

=============================================================================================================}

INTERFACE

USES
   System.SysUtils,
   LightCore, LightCore.Types;

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
 function  EncodeDecode_XOR  (CONST s: AnsiString     ; Key: Byte): AnsiString;  overload;
 function  EncodeDecode_XOR  (CONST Bytes: TBytesArray; CONST Key: string): TBytesArray; overload;  { Multi-byte XOR with string key }

 function  EncodeXorText     (CONST PlainText: string; Key: Byte): string;
 function  DecodeXorText     (CONST EncodedArr: array of Byte; Key: Byte): string;

 { SEE ALSO
 System.IOUtils.TFile.Encrypt
 https://docwiki.embarcadero.com/Libraries/Alexandria/en/System.IOUtils.TFile.Encrypt
 Encrypt a given file using the operating system-provided facilities.}

IMPLEMENTATION


{--------------------------------------------------------------------------------------------------
   Shifts all ASCII codes down by 1. Inverse of SimpleEncode.
   Wraps around: #0 becomes #255.
   Warning: Only works correctly with ASCII (chars 0-255). Unicode chars are truncated.
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
   then Result:= Result+ #255   { ÿ is #255 }
   else Result:= Result+ char(curchar-1);
  end;
end;


{ Shifts all ASCII codes up by 1. Inverse of SimpleDecode.
  Wraps around: #255 becomes #0.
  Warning: Only works correctly with ASCII (chars 0-255). Unicode chars are truncated. }
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
   Applies bitwise NOT to each character. Self-inverse: applying twice returns original.
   Warning: Only works correctly with ASCII (chars 0-255). Unicode chars are truncated.
--------------------------------------------------------------------------------------------------}
function EncodeDecode_NOT(CONST s: string): string;
VAR CharIndex : integer;
begin
  Result:= s;
  for CharIndex:= 1 to Length(s) DO
    Result[CharIndex]:= chr(NOT (ord(s[CharIndex])));
end;




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


{ Multi-byte XOR encryption using a string key.
  Each byte is XORed with a rotating key derived from the string.
  Self-inverse: applying twice with the same key returns original.
  Key must be non-empty. ASCII characters only (0-255). }
function EncodeDecode_XOR(CONST Bytes: TBytesArray; CONST Key: string): TBytesArray;
VAR
  i, KeyLen: Integer;
  KeyBytes: TBytesArray;
begin
  if Key = ''
  then EXIT(Bytes);

  KeyLen:= Length(Key);
  SetLength(KeyBytes, KeyLen);
  for i:= 1 to KeyLen DO
    KeyBytes[i-1]:= Ord(Key[i]) AND $FF;

  Result:= Bytes;
  for i:= 0 to High(Bytes) DO
    Result[i]:= Bytes[i] XOR KeyBytes[i MOD KeyLen];
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

{ Generates Delphi code for an encoded byte array constant.
  Use this to encode text/passwords for storage in source files.
  Example output: array[0..2] of Byte = ($22, $21, $20);
  Returns empty string if PlainText is empty. }
function EncodeXorText(const PlainText: string; Key: Byte): string;
var
  Encoded: string;
  sOut: string;
  i: Integer;
  HexValue: string;
begin
  if PlainText = ''
  then EXIT('');

  Encoded:= EncodeDecode_XOR(PlainText, Key);
  Result:= 'array[0..' + IntToStr(Length(Encoded) - 1) + '] of Byte = (';

  sOut:= '';
  for i:= 1 to Length(Encoded) do
  begin
    { Format as $XX with leading zero removal, but preserve $0 for byte value 0 }
    HexValue:= IntToHex(Ord(Encoded[i]), 2);
    if (Length(HexValue) = 2) AND (HexValue[1] = '0') AND (HexValue[2] <> '0')
    then HexValue:= HexValue[2]; { Remove leading zero only if second digit is non-zero }

    sOut:= sOut + '$' + HexValue;
    if i < Length(Encoded)
    then sOut:= sOut + ', ';
  end;

  Result:= Result + sOut + ');';
end;


{ Converts the text encoded by the function above, back to the original text }
function DecodeXorText(CONST EncodedArr: array of Byte; Key: Byte): string;
VAR I: Integer;
begin
  SetLength(Result, Length(EncodedArr));
  for I := 0 to High(EncodedArr) do
    Result[I + 1] := Char(EncodedArr[I] XOR Key); // XOR with the same key
end;




{--------------------------------------------------------------------------------------------------
   CHAINED XOR ENCRYPTION
   Uses a rolling key derived from each encoded byte, providing better diffusion than simple XOR.
   Advantages: The result is always alphanumeric hex. Example: 12345 -> 364944BC8C
   Disadvantage: The resulting encoded string is twice as long as the input string.
   The same StartKey must be used for both encoding and decoding.
--------------------------------------------------------------------------------------------------}
{$R-}{$Q-}
CONST
   FixedEncodeConstant= 75845;    { If you change this constant, any previous encoded string will not be properly decode anymore }

{ Encrypts a string using chained XOR with a rolling key.
  Output is always a hex string (alphanumeric), twice the length of input. }
function BetterEncode (CONST s: string; CONST StartKey: WORD): string; { Encrypt }
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


{ Decrypts a string encoded by BetterEncode.
  Input must be a valid hex string with even length (each original byte is represented as 2 hex chars).
  Raises exception if input length is odd or contains invalid hex characters. }
function BetterDecode (CONST s: string; CONST StartKey: WORD): string; { Decrypt }
VAR b  : BYTE;
    i  : INTEGER;
    key: WORD;
    HexPair: string;
    ParsedValue: Integer;
BEGIN
 if Length(s) MOD 2 <> 0
 then RAISE exception.Create('The length of the input string to decrypt must be divisible by 2');

 key:= StartKey;
 RESULT:= '';
 for i:= 1 TO LENGTH(s) DIV 2 DO
  BEGIN
   HexPair:= '$' + system.COPY(s, 2*i-1, 2);
   ParsedValue:= StrToIntDef(HexPair, -1);
   if ParsedValue < 0
   then RAISE Exception.Create('Invalid hex sequence in encrypted string: ' + HexPair);
   b:= ParsedValue;
   Result:= Result + CHAR( b XOR (key SHR 8) );
   key:= (b + key) * FixedEncodeConstant
  END
END;

{$R+}
{$Q+}


end.




