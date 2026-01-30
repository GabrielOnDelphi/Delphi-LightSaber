UNIT LightCore.EncodeMime;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
==============================================================================================================

   MIME/BASE64 ENCODING LIB

   Provides functions for MIME (Base64) encoding/decoding of strings.
   This unit wraps Soap.EncdDecd to provide simpler, safer string-based operations.

   Two variants are provided:
     - MimeString/DeMimeString: For Unicode strings (uses UTF-8 encoding internally)
     - MimeStringA/DeMimeStringA: For AnsiString (raw byte operations)

   Don't use EncdDecd directly - it was updated for Unicode:
       http://stackoverflow.com/questions/21883152/how-to-encode-strings-with-encddec-library/21883731#21883731

=============================================================================================================}

INTERFACE

USES
  System.Classes, System.SysUtils, Soap.EncdDecd;

 { Unicode string encoding - handles UTF-8 conversion automatically }
 function MimeString           (CONST Input: string): string;
 function DeMimeString         (CONST Input: string): string;

 { AnsiString encoding - raw byte operations, no character set conversion }
 function MimeStringA          (CONST Input: AnsiString): AnsiString;
 function DeMimeStringA        (CONST Input: AnsiString): AnsiString;


IMPLEMENTATION

USES
   LightCore.StreamMem;



{--------------------------------------------------------------------------------------------------
   UNICODE STRING MIME ENCODING

   These functions handle Unicode strings by converting to/from UTF-8 internally.
   The Base64 output is ASCII-safe and can be transmitted through any text channel.

   Example:
     Encoded:= MimeString('Hello World');  // Returns 'SGVsbG8gV29ybGQ='
     Original:= DeMimeString(Encoded);     // Returns 'Hello World'
--------------------------------------------------------------------------------------------------}

{ Encodes a Unicode string to Base64.
  The input is first converted to UTF-8 bytes, then those bytes are Base64-encoded.
  Returns an empty string if Input is empty.
  Solution credit: David Heffernan (StackOverflow) }
function MimeString(CONST Input: string): string;
var
  InStr, OutStr: TStringStream;
begin
  if Input = '' then EXIT('');

  InStr:= TStringStream.Create(Input, TEncoding.UTF8);
  try
    OutStr:= TStringStream.Create('');
    try
      EncodeStream(InStr, OutStr);
      Result:= OutStr.DataString;
    finally
      FreeAndNil(OutStr);
    end;
  finally
    FreeAndNil(InStr);
  end;
end;


{ Decodes a Base64 string back to Unicode.
  The Base64 input is decoded to bytes, then interpreted as UTF-8 to produce the result.
  Returns an empty string if Input is empty. }
function DeMimeString(CONST Input: string): string;
var
  InStr, OutStr: TStringStream;
begin
  if Input = '' then EXIT('');

  InStr:= TStringStream.Create(Input);
  try
    OutStr:= TStringStream.Create('', TEncoding.UTF8);
    try
      DecodeStream(InStr, OutStr);
      Result:= OutStr.DataString;
    finally
      FreeAndNil(OutStr);
    end;
  finally
    FreeAndNil(InStr);
  end;
end;



{--------------------------------------------------------------------------------------------------
   ANSISTRING MIME ENCODING

   These functions handle raw AnsiString bytes without any character set conversion.
   Useful for binary data or legacy systems that work with AnsiString.

   Used by: cpCertificate.pas

   Implementation note:
     WriteChars writes raw bytes without a length prefix.
     ReadCharsA reads raw bytes without expecting a length prefix.
     These must be paired correctly (both use count-based, not length-prefixed operations).
--------------------------------------------------------------------------------------------------}

{ Encodes an AnsiString to Base64.
  The raw bytes of the input are Base64-encoded without any character set conversion.
  Returns an empty string if Input is empty. }
function MimeStringA(CONST Input: AnsiString): AnsiString;
var
  InStr, OutStr: TCubicMemStream;
begin
  if Input = '' then EXIT('');

  InStr:= TCubicMemStream.Create;
  try
    InStr.WriteChars(Input);
    InStr.Position:= 0;

    OutStr:= TCubicMemStream.Create;
    try
      EncodeStream(InStr, OutStr);

      { ReadCharsA reads raw bytes without expecting a length prefix.
        This matches WriteChars which also doesn't write a length prefix.
        We pass OutStr.Size as SafetyLimit since we know the exact data size. }
      OutStr.Position:= 0;
      Result:= OutStr.ReadCharsA(OutStr.Size, OutStr.Size);
    finally
      FreeAndNil(OutStr);
    end;
  finally
    FreeAndNil(InStr);
  end;
end;


{ Decodes a Base64 AnsiString back to raw bytes.
  Returns an empty string if Input is empty. }
function DeMimeStringA(CONST Input: AnsiString): AnsiString;
var
  InStr, OutStr: TCubicMemStream;
begin
  if Input = '' then EXIT('');

  InStr:= TCubicMemStream.Create;
  try
    InStr.WriteChars(Input);
    InStr.Position:= 0;

    OutStr:= TCubicMemStream.Create;
    try
      DecodeStream(InStr, OutStr);

      { ReadCharsA reads raw bytes without expecting a length prefix.
        This matches WriteChars which also doesn't write a length prefix.
        We pass OutStr.Size as SafetyLimit since we know the exact data size. }
      OutStr.Position:= 0;
      Result:= OutStr.ReadCharsA(OutStr.Size, OutStr.Size);
    finally
      FreeAndNil(OutStr);
    end;
  finally
    FreeAndNil(InStr);
  end;
end;


end.
