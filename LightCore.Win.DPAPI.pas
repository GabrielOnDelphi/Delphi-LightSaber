UNIT LightCore.Win.DPAPI;

{$IFNDEF MSWINDOWS}
  {$MESSAGE FATAL 'LightCore.Win.DPAPI is Windows-only. Its package LightCore.Win builds for Win32 and Win64 only.'}
{$ENDIF}

{=============================================================================================================
   2026.11.05
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Encrypts a short text (a password, an api key) for the current Windows user, through the Windows Data
   Protection API (DPAPI): CryptProtectData and CryptUnprotectData in Crypt32.dll.
   Windows derives the key from the logon of the current Windows user, so the program stores no key of its own.

   ProtectString returns one line of Base64 text that fits into an INI value.
   TryUnprotectString turns it back into the original text.

   Who can decrypt (https://learn.microsoft.com/en-us/windows/win32/api/dpapi/nf-dpapi-cryptprotectdata):
     "Typically, only a user with the same logon credential as the user who encrypted the data can decrypt the data.
      In addition, the encryption and decryption usually must be done on the same computer."
     Any program that runs as the same Windows user CAN decrypt it.
     CRYPTPROTECT_LOCAL_MACHINE is never passed: with it "Any user on the computer ... can use CryptUnprotectData".

   Entropy: an extra secret of the calling program (for example 'MyCompany.MyProgram').
   A text protected with one entropy is refused with another, so a second program that uses this unit cannot read
   the value of the first one by accident. '' = no entropy.

   Tampering (https://learn.microsoft.com/en-us/windows/win32/api/dpapi/nf-dpapi-cryptunprotectdata):
     the function "may return ERROR_INVALID_DATA, ERROR_INVALID_PARAMETER, or in some cases may succeed with
     corrupted output". So a damaged value can come back as a wrong text, not only as FALSE.

   The byte arrays that hold the plain text are filled with zeros before they are released.
   A Delphi string cannot be wiped for sure, so the caller's own strings are not covered.

   Windows-only unit, package LightCore.Win. DPAPI exists on no other platform.
   Thread safety: no shared state; safe to call from any thread.
=============================================================================================================}

INTERFACE


 { Returns the DPAPI blob of PlainText as one line of Base64 text. PlainText = '' gives ''.
   Raises EOSError when CryptProtectData fails. }
 function ProtectString(CONST PlainText, Entropy: string): string;

 { FALSE when ProtectedText is empty, is no Base64, or DPAPI refuses it (another Windows user, another PC, another
   entropy, damaged text). Never raises for bad input. PlainText is '' when the result is FALSE. }
 function TryUnprotectString(CONST ProtectedText, Entropy: string; OUT PlainText: string): Boolean;


IMPLEMENTATION

USES
   Winapi.Windows, System.SysUtils, System.NetEncoding;


CONST
   { dpapi.h. Not declared in Delphi 13 (c:\Delphi\Delphi 13\source\ has no CryptProtectData). }
   CRYPTPROTECT_UI_FORBIDDEN = $1;

{ The C signatures: https://learn.microsoft.com/en-us/windows/win32/api/dpapi/nf-dpapi-cryptprotectdata and
  .../nf-dpapi-cryptunprotectdata. pPromptStruct is always nil: the prompt flow "is deprecated and will be removed
  in April 2027. Passing NULL ... will use the non-interactive path". }
function CryptProtectData(pDataIn: PDATA_BLOB; szDataDescr: PWideChar; pOptionalEntropy: PDATA_BLOB; pvReserved: Pointer;
                          pPromptStruct: Pointer; dwFlags: DWORD; pDataOut: PDATA_BLOB): BOOL; stdcall; external 'crypt32.dll' name 'CryptProtectData';

function CryptUnprotectData(pDataIn: PDATA_BLOB; ppszDataDescr: PPWideChar; pOptionalEntropy: PDATA_BLOB; pvReserved: Pointer;
                            pPromptStruct: Pointer; dwFlags: DWORD; pDataOut: PDATA_BLOB): BOOL; stdcall; external 'crypt32.dll' name 'CryptUnprotectData';


procedure WipeBytes(VAR Bytes: TBytes);
begin
  if Length(Bytes) > 0
  then FillChar(Bytes[0], Length(Bytes), 0);
  SetLength(Bytes, 0);
end;


{ Points Blob at Bytes. Returns nil for an empty array, which DPAPI reads as "no entropy". }
function BlobOf(CONST Bytes: TBytes; VAR Blob: DATA_BLOB): PDATA_BLOB;
begin
  if Length(Bytes) = 0 then
    begin
      Blob.cbData:= 0;
      Blob.pbData:= NIL;
      EXIT(NIL);
    end;
  Blob.cbData:= Length(Bytes);
  Blob.pbData:= @Bytes[0];
  Result:= @Blob;
end;


{ Copies the output blob of DPAPI into a byte array, zeros it, and frees it with LocalFree (both Microsoft pages:
  pDataOut.pbData must be freed with LocalFree). }
function TakeOutBlob(VAR OutBlob: DATA_BLOB): TBytes;
begin
  TRY
    SetLength(Result, OutBlob.cbData);
    if OutBlob.cbData > 0 then
      begin
        Move(OutBlob.pbData^, Result[0], OutBlob.cbData);
        FillChar(OutBlob.pbData^, OutBlob.cbData, 0);
      end;
  FINALLY
    LocalFree(HLOCAL(OutBlob.pbData));
    OutBlob.pbData:= NIL;
    OutBlob.cbData:= 0;
  END;
end;


function ProtectString(CONST PlainText, Entropy: string): string;
VAR
  Plain, Salt, Cipher: TBytes;
  InBlob, SaltBlob, OutBlob: DATA_BLOB;
begin
  if PlainText = '' then EXIT('');

  Plain:= TEncoding.UTF8.GetBytes(PlainText);
  Salt := TEncoding.UTF8.GetBytes(Entropy);
  TRY
    BlobOf(Plain, InBlob);
    OutBlob.cbData:= 0;
    OutBlob.pbData:= NIL;

    if NOT CryptProtectData(@InBlob, NIL, BlobOf(Salt, SaltBlob), NIL, NIL, CRYPTPROTECT_UI_FORBIDDEN, @OutBlob)
    then RaiseLastOSError;

    Cipher:= TakeOutBlob(OutBlob);
    Result:= TNetEncoding.Base64String.EncodeBytesToString(Cipher);   // one line: Base64String never breaks the text, TNetEncoding.Base64 does every 76 characters
  FINALLY
    WipeBytes(Plain);
  END;
end;


function TryUnprotectString(CONST ProtectedText, Entropy: string; OUT PlainText: string): Boolean;
VAR
  Plain, Salt, Cipher: TBytes;
  InBlob, SaltBlob, OutBlob: DATA_BLOB;
begin
  PlainText:= '';
  if ProtectedText = '' then EXIT(FALSE);

  { The decoder skips every character outside the Base64 alphabet (System.NetEncoding.pas, TCustomBase64Encoding.DecodeBytes), so it never raises. }
  Cipher:= TNetEncoding.Base64String.DecodeStringToBytes(ProtectedText);
  if Length(Cipher) = 0 then EXIT(FALSE);

  Salt:= TEncoding.UTF8.GetBytes(Entropy);
  BlobOf(Cipher, InBlob);
  OutBlob.cbData:= 0;
  OutBlob.pbData:= NIL;

  if NOT CryptUnprotectData(@InBlob, NIL, BlobOf(Salt, SaltBlob), NIL, NIL, CRYPTPROTECT_UI_FORBIDDEN, @OutBlob)
  then EXIT(FALSE);

  Plain:= TakeOutBlob(OutBlob);
  TRY
    { Corrupted output (see the header) may be no valid UTF-8, and TEncoding.UTF8 then raises EEncodingError
      (System.SysUtils.pas, TEncoding.GetString: "if (Length(Bytes) > 0) and (Len = 0) then raise"). That is bad input: the answer is FALSE. }
    TRY
      PlainText:= TEncoding.UTF8.GetString(Plain);
    EXCEPT
      on EEncodingError do
        PlainText:= '';
    END;
    Result:= PlainText <> '';
  FINALLY
    WipeBytes(Plain);
  END;
end;


end.
