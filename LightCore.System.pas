UNIT LightCore.System;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Programmer's helper and System tools
=============================================================================================================}

INTERFACE

USES
   System.AnsiStrings, System.Character, System.SysUtils, System.Math, System.IOUtils, System.StrUtils,
   System.Classes, System.Types, LightCore.Types;


{=============================================================================================================
   DEVELOP UTILS
=============================================================================================================}
 procedure NotImplemented;
 procedure EmptyDummy;

 procedure DisposeAndNil(VAR P: Pointer);
 procedure FillZeros(VAR IntArray: TIntegerDynArray);


{=============================================================================================================
   SYSTEM
=============================================================================================================}
 function GetResourceAsString(CONST ResName: string): AnsiString;    { Extract a resource from self (exe) }
 function GetSystemLanguageName: string;
 function GetSystemLanguageNameShort: string;





IMPLEMENTATION
USES
  {$IFDEF MSWINDOWS}Winapi.Windows,{$ENDIF}  // GetLocaleInfo, LOCALE_USER_DEFAULT, LOCALE_SLANGUAGE
  System.SysConst;                            // SUnknown (used in non-Windows GetSystemLanguageName)




{============================================================================================================
   UTILS
============================================================================================================}

{ Similar to FreeAndNil but it works on pointers.
  Dispose releases the memory allocated for a pointer variable allocated using System.New. }
procedure DisposeAndNil(VAR P: Pointer);
begin
 System.Dispose(p);
 p:= NIL;
end;


{ Fills all elements of a dynamic integer array with zeros.
  Note: Uses FillChar on the array data, not the array reference. }
procedure FillZeros(VAR IntArray: TIntegerDynArray);
begin
 if Length(IntArray) > 0
 then FillChar(IntArray[0], Length(IntArray) * SizeOf(Integer), 0);
end;


procedure EmptyDummy;
begin
 //Does nothing
end;


procedure NotImplemented;
begin
 RAISE Exception.Create('Not implemented yet.');
end;


{ Extract a resource from self (the executable).
  Returns the resource content as AnsiString.
  Raises exception if resource is empty.
  Raises EResNotFound if resource doesn't exist. }
function GetResourceAsString(CONST ResName: string): AnsiString;
VAR
   ResStream: TResourceStream;
begin
  ResStream:= TResourceStream.Create(HInstance, ResName, RT_RCDATA);
  TRY
    ResStream.Position:= 0;
    if ResStream.Size = 0
    then raise Exception.Create('GetResourceAsString');
    SetLength(Result, ResStream.Size);
    ResStream.ReadBuffer(Result[1], ResStream.Size);
  FINALLY
    FreeAndNil(ResStream);
  END;
end;


// Returns the language of the operating system in this format: "English (United States)".
// Uses Windows API directly for better Win11 compatibility (TLanguages can return <unknown>).
function GetSystemLanguageName: string;
{$IFDEF MSWINDOWS}
VAR
  Buffer: array[0..255] of Char;
begin
  if GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_SLANGUAGE, Buffer, Length(Buffer)) > 0
  then Result:= Buffer
  else Result:= 'English';  // Fallback if API fails
end;
{$ELSE}
var
  Languages: TLanguages;
  Locale: TLocaleID;
begin
  Locale:= TLanguages.UserDefaultLocale;
  Languages:= TLanguages.Create;
  try
    Result:= Languages.NameFromLocaleID[Locale];
    if (Result = '') OR (Result = SUnknown)
    then Result:= 'English';  // Fallback
  finally
    FreeAndNil(Languages);
  end;
end;
{$ENDIF}


// Returns the language of the operating system in this format: "English"
function GetSystemLanguageNameShort: string;
var
  FullName: string;
  P: Integer;
begin
  FullName := GetSystemLanguageName;
  P := Pos('(', FullName);
  if P > 0
  then Result := Trim(Copy(FullName, 1, P - 1))
  else Result := FullName;
end;


end.

