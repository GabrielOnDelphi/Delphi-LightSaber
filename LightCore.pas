UNIT LightCore;

{=============================================================================================================
   www.GabrielMoraru.com
   2026.01.30
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   - String manipulation (string conversions, sub-string detection, word manipulation, cut, copy, split, wrap, etc)
   - Programmer's helper

   See also LightCore.WrapString.pas
=============================================================================================================}

INTERFACE

USES
   System.AnsiStrings, System.Character, System.SysUtils, System.Math, System.IOUtils, System.StrUtils,
   System.Classes, System.Types, LightCore.Types;

{ Enters }
CONST
   CR                = #13;         { $0D. Used on Mac OS, Apple II family, ZX Spectrum }
   LF                = #10;         { $0A  Used on Unix (Linux, OS X, FreeBSD, AIX, Xenix, etc.), BeOS, Amiga, RISC OS }
   CRLFw             = #13#10;      { Used on Windows, OS/2, Symbian OS, Palm OS }
   CRLF              = sLineBreak;  { Cross platform }
   LBRK              = CRLF+CRLF;

{ Special characters }
CONST
   TAB             = #9;
   ESC             = #27;
   Space           = #32;         { $20 }
   Quote           = #39;
   CopyrightSymbol = '©';
   GradCelsius     = '°';
   Euro            = #8364;       { Euro Sign: Alt+0128.  Unicode Number: 8364 }


{=============================================================================================================
   ENTER
   Also see cmPlatformFile.pas
 ============================================================================================================}
 function  CRLFToEnter         (CONST s: string): string;     // old name: FixCRLF
 function  EnterToCRLF         (CONST s: string): string;     // Replaces #13#10 with CRLF

 function  ReplaceLonellyCR    (CONST s, ReplaceWith: string): string;
 function  ReplaceLonellyLF    (CONST s, ReplaceWith: string): string;

 function  LinuxEnter2Win      (CONST s: string): string; deprecated 'Use System.SysUtils.AdjustLineBreaks instead.';
 function  TrimEnters          (CONST s: string): string;                                                         { Remove enter characters (#10 and #13) from the beginning and the end of the string }
 function  RemoveEnters        (CONST s: string): string;         overload;                                       { Works both with Linux, Windows and half-enter characters }
 function  RemoveEnters        (CONST s: Ansistring): Ansistring; overload;

 function  ReplaceEnters       (CONST s, ReplaceWith: string): string;
 function  RemoveLastEnter     (CONST s: string): string;         overload;                                       { Cuts the last Enter from a string }
 function  RemoveLastEnter     (CONST s: AnsiString): AnsiString; overload;


{=============================================================================================================
   REPLACE
 ============================================================================================================}
 function  ReplaceUnicodeChars (CONST s: string; ReplaceWith: char): String;                                      { Replace all Unicode characters withsomething else }
 function  ReplaceCharF        (CONST s: string; CONST SearchFor, ReplaceWith: char): string;
 procedure ReplaceChar         (var   s: string; CONST SearchFor, ReplaceWith: Char);         overload;
 procedure ReplaceChar         (var   s: AnsiString; CONST SearchFor, ReplaceWith: AnsiChar);  overload;
 function  ReplaceStringAnsi   (CONST s, SearchFor, ReplaceWith: AnsiString): AnsiString;
 function  ReplaceString       (CONST s, SearchFor, ReplaceWith: string): string;
 function  ReplaceBetween      (CONST s, TagStart, TagEnd, ReplaceWith: string; Start: Integer; EliminateTags: Boolean; OUT LastPos: Integer): string;
 function  SearchBetween       (CONST s, TagStart, TagEnd: string; Start: Integer = 1): Integer;


{=============================================================================================================
   CLEAN STRINGS
 ============================================================================================================}
 function  RemoveNonAlphanum   (CONST s: string): string;                                                         { Keep only letters and numbers }
 function  RemoveFormatings    (CONST s: string): string;
 function  RemoveLowChars      (CONST s: string): string;                 overload;
 function  RemoveLowChars      (CONST s: AnsiString): AnsiString;         overload;
 function  RemoveSpaces        (CONST s: string): string;                 overload;
 function  RemoveSpaces        (CONST s: Ansistring): Ansistring;         overload;
 function  RemoveTabs          (CONST s: string): string;

 function  RemoveLastSpace     (CONST s: string): string;                                                         { Cuts the last SPACE from a string. If there are more spaces only the last one is cut }
 function  RemoveLastChar      (CONST s: string): string;                 overload;
 function  RemoveLastChar      (CONST s, StrToRemove: string): string;    overload;
 function  RemoveLastChar      (CONST s: AnsiString): AnsiString;         overload;
 function  RemoveFirstChar     (CONST s: string; Char: Char): string;                                             { Remove first character from the string but ONLY if it is Char }

 function  RemoveNumbers       (CONST s: string): string;                 overload;                               { Eliminates numbers from the specified string }
 function  RemoveNumbers       (CONST s: AnsiString): AnsiString;         overload;

 function  TrimUntil           (CONST s: string; Limiter: Char): string;                                          { Remove any characters from the beginning and the end of a string until Limiter is found. Limiter is kept. Example: if Limiter is $ and the string is 'xxxxx$ThisIsMyString$@@@@@@'. then the result will be '$ThisIsMyString$' }
 function  TrimUntilDiff       (CONST s: string; Limiter: Char): string;                                          { Remove characters from both ends of a string until something different than Limiter is found. Example:  TrimUntilDiff('--ACGT--', '-') returns 'ACGT' }

 function  Retabulate          (CONST s, Delimiter: string; SpaceCount: Integer): string;                         { Converts multiple spaces to Tab or other similar separator. For example Retabulate('xx   xx  yy, 3, Tab') will convert the first 3 spaces to tab but not also the next 2 spaces }
 function  ReplaceNbsp         (CONST s, ReplaceWith: string): string;


{=============================================================================================================
   WORDS
 ============================================================================================================}
 function  IsWordSeparator     (CONST aChar: Char): Boolean;                                                      { Returns true if the specified char is a word separator .;?,! }
 function  CopyWords           (CONST s: string; MaxChars: Integer): string;                                      { Copy from s all complete words. The result will not have more than MaxChars characters. }
 procedure ReplaceShortWords   (var   s: string; MinLength: Integer; FilterIfNoWovels: Boolean);                  { This procedure will replace short words (length < MinLength) with spaces.   It also filters words that only contain consonants }
 function  ReplaceWholeWords   (const InputStr, OldWord, NewWord: string; const Delimiters: array of Char): string; overload;
 function  ReplaceWholeWords   (const InputStr, OldWord, NewWord: string): string;                                  overload;
 function  WordCountStrict     (CONST s: string): Integer;
 function  WordCount           (CONST s: string): Integer;


{=============================================================================================================
   CUT
 ============================================================================================================}
 function  CutInclude2Left     (CONST s, SearchFor: string): string;                                              { Delete all chars from end of MATCH to Left  - including the match }
 function  CutInclude2Right    (CONST s, SearchFor: string): string;                                              { Delete all chars from beg of MATCH to Right - including the match }
 function  CutExcludeLeft      (CONST s, SearchFor: string): string;                                              { Delete all chars from beg of MATCH to Left  - excluding the match }
 function  CutExcludeRight     (CONST s, SearchFor: string): string;                                              { Delete all chars from end of MATCH to Right - excluding the match }


 {=============================================================================================================
   COPY
 ============================================================================================================}
 function  CopyTo              (CONST s: String; iFrom: Integer; CONST sTo: string; IncludeMarker: Boolean= TRUE; CopyAllMarkerNotFound: Boolean= FALSE; MarkerOffset: Integer= 1): string; overload;
 function  CopyFromTo          (CONST s, sFrom, sTo: string; IncludeMarkers: Boolean= FALSE): string;

 function  CopyFrom            (CONST s, sFrom: string;     Count: Integer; IncludeMarker: Boolean= TRUE; SearchOffset: Integer= 1): string;     overload;  { Find sFrom in s. Returns the string from the postion where the text was found, to the end. }
 function  CopyFrom            (CONST s, sFrom: AnsiString; Count: Integer; IncludeMarker: Boolean= TRUE; SearchOffset: Integer= 1): AnsiString; overload;

 // COPY
 function  CopyTo              (CONST s: string;     iFrom, iTo: integer): string;     overload;                  { Copy the text between iFrom and ending at iTo (including) }
 function  CopyTo              (CONST s: AnsiString; iFrom, iTo: integer): AnsiString; overload;                  { Copy the text between iFrom and ending at iTo (including) }


{=============================================================================================================
   SPLIT
 ============================================================================================================}
 function  SplitText           (CONST Text, Delimiter: string): TStringList;                                      { Splits a text in lines and puts the lines in a TStringList } {Note: Exista System.StrUtils.SplitString } { Old name: SplitStrings }
 procedure SplitLine           (CONST Text, Delimiter: string; OUT sField, sValue: string);    overload;          { Split a string in its components. For example 'ClientName=Bubu' will return in 'ClientName' and 'Bubu' }
 procedure SplitStrings        (CONST Text: string; TSL: TStringList);                         overload;          { Split a string in multiple rows every time the #13#10 char is found (I took this code from Embarcadero's TStringList.Text:= s ) }
 procedure SplitStringAtPos    (CONST Text: string; CONST Pos: Integer; OUT s1, s2: string);   overload;          { Split a string in two substrings at the specified position. The char at Pos will be included in the first string. }
 procedure SplitStringAtPos    (CONST Text: AnsiString; CONST Pos: Integer; OUT s1, s2: AnsiString); overload;
 procedure SplitStringList     (StringList: TStrings; OUT OutList1, OutList2: TStringArray);                      { Split each row of the provided StringList into two parts. The two resulted strings are placed in an ArrayOfStrings }
 procedure SplitStringListI    (StringList: TStrings; OUT OutList1: TStringArray; OUT OutList2: System.Types.TIntegerDynArray);   { Split each row of the provided StringList into two parts. The two resulted strings are placed in an ArrayOfStrings }


{============================================================================================================
   STRING POS
 ============================================================================================================}
 function  Find                (CONST Needle, Haystack: string; PartialSearch: Boolean= False; CaseSens: Boolean= False): boolean;

 function  CountAppearance     (CONST Needle, Haystack: string; CaseSensit: Boolean): integer;     overload;
 function  CountAppearance     (CONST Niddle: Char;     CONST Haystack: string)    : Integer;      overload;
 function  CountAppearance     (CONST Niddle: AnsiChar; CONST Haystack: AnsiString): Integer;      overload;

 function  LastPos             (CONST Niddle, S: string): Integer;                                 overload;  { Return the position of the last occurence of a substring in String. Not tested. Also see 'EndsStr' }
 function  LastPos             (CONST Niddle: Char; CONST S: String): Integer;                     overload;

 function  PosAtLeast          (CONST Niddle, S: string; AtLeast: Integer): Boolean;                          { Returns true if the specified string appears at least x times }
 function  PosInsensitive      (CONST Niddle, Haystack: string): Integer;                          overload;
 function  PosInsensitive      (CONST Niddle, Haystack: AnsiString): Integer;                      overload;

 function  LastChar            (CONST s: string): string;                                                     { Returns the last char in the string but checks first if the string is empty (so it won't crash). Returns '' if the string is empty }
 function  FirstChar           (CONST s: string): string;                                                     { Returns the first char in the string but checks first if the string is empty (so it won't crash). Returns '' if the string is empty }
 function  FirstCharIs         (CONST s: string; c: Char): Boolean;
 function  LastCharIs          (CONST s: string; c: Char): Boolean;
 function  FirstNonSpace       (CONST s: string): Integer;                                                    { Returns the position of the first character that is no a space.  For example: '  Earth' returns 3. }


{============================================================================================================
   CONVERSION TO NUMBERS
============================================================================================================}
 function  i2s                 (Value: Integer):           string; overload;  inline;
 function  i2s                 (Value, MaxVal: integer):   string; overload;                                  { Add the specified number of zeros before the string. See LeadingZerosAuto help for details }
 function  i2s                 (Value: Int64)  :           string; overload;                                  { int64 can hold up to 9223372036854775807 }
 function  i2sHuman            (Value: Int64)  :           string;                                            { Retunrs something like: 1= 1st, 2= 2nd, 3= 3rd, 4= 4th }
 function  ExtractIntFromStr   (const s: string): Integer;                                                    { Extracts a number from a string. Works only if the number is at the beginning of the string. Example '123xxx' }
 function  Real2Str            (CONST ExtValue: Extended; Decimals: Byte = 1; HideNulMantisa: Boolean= True): string;
 function  Rectangle2Str       (CONST Rect: TRect): string;
 function  FormatBytes         (CONST Size: Int64; CONST Decimals: Integer= 1): string;                       { Format bytes to KB, MB, GB, TB }
 function  FormatBytesMB       (CONST Size: Int64; CONST Decimals: Integer= 1): string;                       { Same as above but the function will never return values formated in GB range. More exactly instead of 10GB it will return 10240MB }
 function  FormatNumber        (CONST Size: Int64; CONST Decimals: Integer= 1): string;                       { It will return 1K for 1000, 1M for 1000000 and so on }
 function  BoolToStrYesNo      (CONST B: Boolean): string;


{============================================================================================================
   NUMBERS
============================================================================================================}
 function  FixNumber           (CONST s: string): Integer;                                                    { Converts a text that contains an invalid number to a valid number. For example  '!2345' will return '2345' }
 function  StringIsInteger     (CONST s: string): Boolean;
 function  CharIsNumber        (CONST c: char)  : Boolean;
 procedure SplitNumber_Start   (CONST s: string; OUT Text, Number: string);                                   { Splits a string that STARTS with a number into its parts. Example: 01_Render   ->  01 + _Render   }
 procedure SplitNumber_End     (CONST s: string; OUT Text, Number: string);                                   { Splits a string that ENDS     in a number into its parts. Example: Document12  ->  Document + 12                                    }
 function  IncrementStringNo   (CONST s: string): string;                                                     { Receive a number as string. return the same number but incremented with 1. automatically adjust the leading zeros }
 function  IncrementStringNoEx (CONST s: string): string;                                                     { Similar with IncrementStringNo but this version also accepts invalid numbers. If the input string doesn't end with a valid number, append 0 at its end. Then extracts the end number and increase it. Example: 0zzz will return 0zzz0, while xxx33 will retun xxx34 }
 function  LastLetterInString  (CONST s: string): Integer;                                                    { Returns the postion of the last non-number character in a string. For example 9d9ad8f7ax0000 returns 10 (the position of x) }
 function  StringSumm          (CONST s: AnsiString): Cardinal;   overload;
 function  StringSumm          (CONST s: String): Cardinal;       overload;                                   { Compute the summ of all characters in the string }


{=============================================================================================================
   OTHERS
 ============================================================================================================}
 function  InsertCharEvery     (CONST c: char; CONST Target: string; Every: Integer): string;                 { Insert a char into TargetStr every x characters }
 function  DoubleQuoteStr      (CONST s: string): string;
 function  Reverse             (CONST s: String): string; deprecated 'LightCore.Reverse is deprecated. Use System.StrUtils.ReverseString';

 function  CharInArray         (CONST c: Char; const Chars: TCharArray): Boolean;
 function  CharIsLetter        (CONST c: char): Boolean;
 function  IsUpcaseLetter      (CONST c: Char): Boolean;
 function  IsUpcase            (CONST c: Char): Boolean;     { Works only with letters. }

 // COPY from/to marker
 function  ExtractTextBetween  (CONST s, TagStart, TagEnd: string): string;                                   { Extract the text between the tags. For example '<H>Title</H>' will return 'Title' is iFrom= '<H>' and iTo= '</H>' }


{=============================================================================================================
   COMPARE/SIMILARITY/SORT
 ============================================================================================================}
 function  FileNameNaturalSort (s1, s2: String): Integer;                                                     { Natural compare two filenames }
 {$IFDEF MSWINDOWS}
 function  StrCmpLogicalW      (psz1, psz2: PWideChar): Integer; stdcall; external 'shlwapi.dll'; {$ENDIF}    { Natural compare two strings. Digits in the strings are considered as numerical content rather than text. This test is not case-sensitive. Use it like this: StrCmpLogicalW(PChar(s1), PChar(s2));  see: http://stackoverflow.com/questions/1024515/delphi-is-it-necessary-to-convert-string-to-widestring.  }
 function  FuzzyStringCompare  (CONST s1, s2: string): Integer;                                                     { Text similarity. The function checks if any identical characters is in the near of the actual compare position.  }
 function  LevenshteinDistance (CONST s1, s2: string): Integer;                                                     { Returns the minimum number of single-character edits (insert, delete, substitute) to transform s1 into s2. }
 function  LevenshteinSimilarity(CONST s1, s2: string): Integer;                                                    { Returns similarity as percentage (0-100). Based on Levenshtein distance. }


{=============================================================================================================
   GENERATE - MAKE STRING
 ============================================================================================================}
 function  MakeStringLongRight (CONST s, c: AnsiChar; ForcedLength: integer): AnsiString;   overload;
 function  MakeStringLongRight (CONST s, c: Char;     ForcedLength: integer): string;       overload;
 function  MakeStringLongRight (CONST s, Pad: string; ForcedLength: integer): string;       overload;         { Make sure the string has ForcedLength. If not, add some extra characters at its end to make it that long  }
 function  MakeStringLongLeft  (CONST s, Pad: string; ForcedLength: integer): string;                         { Make sure the string has ForcedLength. If not, add some extra characters at its front to make it that long  }

 function  LeadingZeros        (CONST s: string; ForcedLength: integer): string;                              { insert (ForcedLength-1) zeros in front of the specified string. ForcedLength shows which is the desired lenght of the new string. Example: LeadingZeros('a', 4) will result in '000a'  }
 function  LeadingZeros2       (CONST s: string; ForcedLength: integer): string; { Not tested }
 function  LeadingZerosAuto    (CONST s: string; MaxValue: integer): string;                                  { Same as above except_ that the user doesn't have to specify how many zeros to add. Instead the function will determine this automaticxally based on the number received as parameter. For example LeadingZeros('1', 50) will generate '01' but LeadingZeros('1', 500) will generate '001' }


{=============================================================================================================
   GENERATE - RANDOM STRINGS
 ============================================================================================================}
 function  GenerateString        (RepeatTimes: Integer; C: char): string; deprecated 'Use System.StringOfChar instead';    { Exista System.StrUtils.DupeString and StuffString                                       Returns the concatenation of a string with itself a specified number of repeats. }
 function  GenerateUniqueString  (Len: Integer=32): string;

 function  GenerateRandomWord    (Len: Integer=16; StartWithVowel: Boolean= FALSE): string;
 function  GenerateRandString    (minLen, maxLen: Integer): string;                                           { This will return all printable craracters (from 65 to 125) }
 function  GenerateRandStringLet (Len: Integer): string;                                                      { This will return ONLY letters and numbers } { YOU MUST call randomize before calling this function! }


{=============================================================================================================
   GENERATE - LISTS OF NAMES
 ============================================================================================================}
 function  GetRandomPersonName: string;                                                                       { Returns a random name in a 100 unique name list }
 function  GetRandomStreetName: string;
 function  GetRockBands: TStringList;


{=============================================================================================================
   UNICODE
 ============================================================================================================}
 function  UnicodeToAnsi       (CONST str: UnicodeString; codePage: Integer): RawByteString;
 function  AddNullToStr        (CONST Path: string): string;


{=============================================================================================================
   STRING RAM SIZE
 ============================================================================================================}
 function  GetStringSize       (CONST s: string): Integer;                                                    { Returns the length of a given string in bytes }
 function  GetStringRAMSize    (CONST s: string): Integer;          overload;
 function  GetStringRAMSize    (CONST s: AnsiString): Integer;      overload;


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
  {$IFDEF MSWINDOWS}Winapi.Windows,{$ENDIF}
  System.SysConst;  // For SUnknown constant

{ Don't add any dependecies to LightSaber here if possible in order to keep LightCore as single-file library }





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
var
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





{============================================================================================================
   STRING  -  CONVERSII
============================================================================================================}

{ It works with signs like: '+1' and '-1' }
{$Hints Off}                                            {Needed to silence  "Value assigned to 'iTemp' never used" }
function StringIsInteger(CONST s: string): Boolean;
VAR iTemp, E: integer;
begin
 Val(s, iTemp, E);
 Result:= E= 0;
end;
{$Hints On}


function CharIsNumber(CONST c: char): Boolean;
begin
 Result:= CharInSet(c, Numbers);
end;


function CharIsLetter(CONST c: char): Boolean;
begin
 Result:= CharInSet(c, Alphabet);
end;


function CharInArray(const C: Char; const Chars: TCharArray): Boolean;
VAR Ch: Char;
begin
  for Ch in Chars do
    if Ch = C then
      Exit(True);
  Result := False;
end;




{ Split a string (could be a filename) that ENDS in a number into its parts. Example:
           Document_12   ->   Document_  +  12
           NewMelody08   ->   NewMelody  +  08                              }
procedure SplitNumber_End(CONST s: string; OUT Text, Number: string);
VAR i: Integer;
begin
 Number:= s;
 for i:= Length(s) DownTO 1 DO                                                                     { Search from end to front. Need to find where string ends and digits start. Example: 'Monkey 02' }
   if NOT CharIsNumber(s[i]) then
    begin                                                                                          { Found a letter. Split text in two }
     Text  := CopyTo(s, 1, i);
     Number:= system.COPY(s, i+1, MaxInt);
     EXIT;
    end;
end;


{ Split a string that STARTS in a number into its parts. Example:    01_Render ->  01 + _Render   }
procedure SplitNumber_Start(CONST s: string; OUT Text, Number: string);
VAR i: Integer;
begin
 for i:= 1 TO Length(s) DO                                                     { Search from end to front. I need to see where the strings ends and where the digits start. Ex: 'Monkey 13' }
   if NOT CharIsNumber(s[i]) then
    begin                                                                      { Letter found. Split text in two. }
     Number:= system.COPY(s, 1, i-1);
     Text  := system.COPY(s, i, MaxInt);
     EXIT;
    end;
end;


{ Returns the position of the last non-digit character in a string.
  Scans from end towards beginning, skipping trailing digits.
  Example: '9d9ad8f7ax0000' returns 10 (the position of 'x').
  Raises exception if string is empty.
  Raises ERangeError if string contains only digits. }
function LastLetterInString(CONST s: string): Integer;
begin
 if s = ''
 then raise Exception.Create('LastLetterInString: Empty string');

 Result:= Length(s);
 while (Result > 0) AND s[Result].IsDigit DO
   Dec(Result);

 if Result = 0
 then raise ERangeError.Create('LastLetterInString: String contains only digits');
end;


function IncrementStringNo(CONST s: string): string;                           { Receives a valid number represented as string. Returns the same number but incremented with 1. automatically adjust the leading zeros. The function raises an error if the input text does not represent a number }
VAR i, Zeros, iNumar, OldLength: Integer;
begin
 if s= '' then raise exception.Create(s+ ' is not a valid number!');

 { Keep 0s }
 Zeros:= 0;
 for i:= 1 TO Length(s) DO
   if s[i]= '0'                                                                { Check if user put zeros in front of the number }
   then inc(Zeros)                                                             { If so, count them }
   else Break;

 iNumar:= StrToInt(s);                                                         { Let it RAISE an error if the string is not a valid number }
 OldLength:= Length(IntToStr(iNumar));
 inc(iNumar);

 if Length(IntToStr(iNumar)) > OldLength
 then Dec(Zeros);                                                              { we switched from 9 to 10 or 99 to 100, etc }
 Result:= StringOfChar('0', Zeros)+ IntToStr(iNumar);
end;



{ Similar with IncrementStringNo but this version also accepts invalid numbers. If the input string doesn't end with a valid number, append 0 at its end. Then extracts the end number and increase it. Example: 0zzz will return 0zzz0, while xxx33 will retun xxx34 }
function IncrementStringNoEx(CONST s: string): string;
VAR
   Text, Number: string;
begin
 if s= '' then EXIT('0');

 SplitNumber_End(s, Text, Number);
 if Number= '' then EXIT(s+ '0');                                              { If string does not contain a number }

 Result:= Text+ IncrementStringNo(Number);
end;



function StringSumm(CONST s: AnsiString): Cardinal;
VAR i: Integer;
begin
 Result:= 0;
 for i:= 1 to Length(s) DO
   Result:= Result+ Ord(s[i]);
end;


{ Compute the summ of all characters in the string }
function StringSumm(CONST s: String): Cardinal;
VAR i: Integer;
begin
 Result:= 0;
 for i:= 1 to Length(s) DO
   Result:= Result+ Ord(s[i]);
end;


{ Extracts a number from a mixed string. Works only if the number is at the beginning of the string. Example '123xxx' }
function ExtractIntFromStr(const s: string): Integer;
var
  RetCode: Integer;
begin
  Val(s, Result, RetCode); // RetCode is the extracted no OR position where a failure (non-numeric character) occured
  if RetCode > 0
  then Val(Copy(s, 1, RetCode - 1), Result, RetCode);
end;




{============================================================================================================
   STRING CONVERSIONS
============================================================================================================}

function i2s(Value: integer): string;
begin
 Result:= IntToStr(Value);
end;


{ As above, but additionally it adds a number of zeros as prefix.
  The number of zeros is determine this automaticxally based on the MaxVal.
  Example:
     i2s('1', 5)    -> '1'
     i2s('1', 500)  -> '001'
}
function i2s(Value, MaxVal: integer): string;
begin
 Result:= IntToStr(Value);
 Result:= LeadingZerosAuto(Result, MaxVal);
end;


function i2s(Value: Int64): string;
begin
 Result:= IntToStr(Value);
end;


{ Converts a number to its ordinal string representation.
  Examples: 1='1st', 2='2nd', 3='3rd', 4='4th', 11='11th', 21='21st', 22='22nd'.
  Handles special cases for 11th, 12th, 13th which use 'th' suffix. }
function i2sHuman(Value: Int64): string;
VAR
   LastTwoDigits, LastDigit: Integer;
begin
 LastTwoDigits:= Abs(Value) mod 100;
 LastDigit:= Abs(Value) mod 10;

 // Special case: 11, 12, 13 always use 'th'
 if (LastTwoDigits >= 11) AND (LastTwoDigits <= 13)
 then EXIT(IntToStr(Value) + 'th');

 case LastDigit of
    1 : Result:= IntToStr(Value) + 'st';
    2 : Result:= IntToStr(Value) + 'nd';
    3 : Result:= IntToStr(Value) + 'rd';
  else Result:= IntToStr(Value) + 'th';
 end;
end;


function Rectangle2Str(CONST Rect: TRect): string;
begin
 Result:= 'Top: '+ IntToStr(Rect.Top)+ ',  Left: '+ IntToStr(Rect.Left)+ ',  Bottom: '+ IntToStr(Rect.Bottom)+ ',  Right: '+ IntToStr(Rect.Right);
end;


{ Converts a real number to string.
  HideNulMantisa = True  -> This will hide the fractional part (numbers after coma) if it is 0. Example: 3.0 returns '3'
  HideNulMantisa = False -> This shows the decimals. Example: 3.0 returns '3.0'

  Already exists:
        System.SysUtils.FloatToStrF. Example: System.SysUtils.FloatToStrF(x, ffFixed, 7, Decimals);
        http://www.delphibasics.co.uk/rtl.asp?name=floattostrf }
function Real2Str(CONST ExtValue: Extended; Decimals: Byte = 1; HideNulMantisa: Boolean= True): string;
VAR ComaPos, i: Integer;
begin
 Assert(Decimals > 0, 'You need to specify at least one digit after the comma; otherwise use RoundEx()');
 Assert(NOT System.Math.IsNaN(ExtValue), 'Float is NAN!');
 Result:= FloatToStrF(ExtValue, ffFixed, 16, Decimals);

 ComaPos:= Pos(FormatSettings.DecimalSeparator, Result);
 Assert(ComaPos > 1, 'Decimal separator not found!');

 Result:= system.COPY(Result, 1, ComaPos+ Decimals);

 if HideNulMantisa then
  begin
   { Cut 0s from the end }
   ComaPos:= Length(Result);
   for i:= ComaPos downto 1 DO
    if Result[i] <> '0' then
     begin
      ComaPos:= i;
      Break;
     end;

   if Result[ComaPos]= FormatSettings.DecimalSeparator
   then Dec(ComaPos);

   Result:= System.COPY(Result, 1, ComaPos);
  end;
end;


{ Formats the size of a file from bytes to KB, MB, GB, TB } { Old name was: FormatFileSize }
function FormatBytes(CONST Size: Int64; CONST Decimals: Integer= 1): string;
begin
 if Size = 0
 then Result:= '0 Bytes' else

 if Size< 1024
 then Result:= IntToStr(Size)+ ' bytes' else

 if (Size>= KB) AND (Size< MB)
 then Result:= Real2Str(Size / KB, Decimals)+ ' KB' else

 if (Size>= MB) AND (Size< GB)
 then Result:= Real2Str(Size / MB, Decimals)+ ' MB' else

 if (Size>= GB) AND (Size< TB)
 then Result:= Real2Str(Size / GB, Decimals)+ ' GB' else

 if (Size>= TB)
 then Result:= Real2Str(Size / TB, Decimals)+ ' TB'
 else raise Exception.Create('Negative file size!');
end;


{ Same as above but the function will never return values formated in GB range. More exactly instead of 10GB it will return 10240MB }
function FormatBytesMB(CONST Size: Int64; CONST Decimals: Integer= 1): string;
begin
 if Size = 0
 then Result:= '0 Bytes' else

 if Size< 1024
 then Result:= IntToStr(Size)+ ' bytes' else

 if (Size>= KB) AND (Size< MB)
 then Result:= Real2Str(Size / KB, Decimals)+ ' KB' else

 Result:= Real2Str(Size / MB, Decimals)+ ' MB';
end;



function FormatNumber(CONST Size: Int64; CONST Decimals: Integer= 1): string;
begin
 if Size = 0
 then Result:= '0' else

 if Size< 1000
 then Result:= IntToStr(Size) else

 if (Size>= 1000) AND (Size< 1000000)
 then Result:= Real2Str(Size / 1000, Decimals)+ ' K' else

 if (Size>= 1000000) AND (Size< 1000000000)
 then Result:= Real2Str(Size / 1000000, Decimals)+ ' M' else

 if (Size>= 1000000000) AND (Size< 1000000000000)
 then Result:= Real2Str(Size / 1000000000, Decimals)+ ' G' else

 if (Size>= 1000000000000)
 then Result:= Real2Str(Size / 1000000000000, Decimals)+ ' T'
 else raise Exception.Create('Negative file size!');
end;





function BoolToStrYesNo(CONST B: Boolean): string;
begin
 if B
 then Result := 'Yes'
 else Result := 'No';
end;





{============================================================================================================
   STRING
============================================================================================================}
{$IFNDEF UNICODE}
function ASCII2ANSI(CONST AText: string): string;
CONST MaxLength = 255;
VAR PText : PChar;
begin
  PText:= StrAlloc(MaxLength);
  StrPCopy(PText,AText);
  OEMToChar(PText, PText);                                                                         {32Bit}
  Result:=StrPas(PText);
  StrDispose(PText);
end;

function ANSI2ASCII(CONST AText: string):string;
CONST MaxLength = 255;
VAR PText : PChar;
begin
  PText:=StrAlloc(MaxLength);
  StrPCopy(PText,AText);
  CharToOEM(PText,PText);                                                                          {32Bit}
  Result:=StrPas(PText);
  StrDispose(PText);
end;

{$ELSE}

//This is cross platform.
// NEEDS TESTING!
function UnicodeToAnsi(CONST str: UnicodeString; CodePage: Integer): RawByteString;
var
  Encoding: TEncoding;
  Bytes: TBytes;
begin
  Result := '';
  if str <> '' then
  begin
    if CodePage = 0
    then CodePage := DefaultSystemCodePage;
    // Create the appropriate TEncoding instance for the specified code page
    Encoding := TEncoding.GetEncoding(CodePage);
    try
      // Convert the UnicodeString to bytes using the specified encoding
      Bytes := Encoding.GetBytes(str);
      // Set the result with the raw bytes and the correct code page
      SetString(Result, PAnsiChar(@Bytes[0]), Length(Bytes));
      SetCodePage(Result, CodePage, False);
    finally
      Encoding.Free;
    end;
  end;
end;
{$ENDIF}



// see: https://stackoverflow.com/questions/39960356/how-to-create-a-procedure-like-setlength-that-also-zeros-the-memory
{
procedure SetLengthZero(VAR X; NewSize: Integer);
begin
 SetLength(x, 0);             // First we clear up all existing data in X
 SetLength(x, NewSize);       // Following a call to SetLength, S is guaranteed to reference a unique string or array. All NEW elements are zeroed@


  For a short string variable, SetLength simply sets the length-indicator character (the character at S[0]) to the given value. In this case, NewLength must be a value from 0 through 255.
  For a long string variable, SetLength reallocates the string referenced by S to the given length. Existing characters in the string are preserved, but the content of newly allocated space is undefined.
  For a dynamic array variable, SetLength reallocates the array referenced by S to the given length. Existing elements in the array are preserved and newly allocated space is set to 0 or nil. For multidimensional dynamic arrays, SetLength may take more than one-length parameter (up to the number of array dimensions). Each parameter specifies the number of elements along a particular dimension.
  Following a call to SetLength, S is guaranteed to reference a unique string or array -- that is, a string or array with a reference count of one. If there is not enough memory available to reallocate the variable, SetLength raises an EOutOfMemory exception.

end; }


{ Adds the C NULL character at the end of this Pascal string }
function AddNullToStr(CONST Path: string): string;
begin
  Result:= '';
  if Path = '' then EXIT;
  if Path[Length(Path)] <> #0
  then Result := Path + #0
  else Result := Path;
end;


TYPE           { This is declared in System but is not available so I had to redeclare it here }
  StrRec = packed record
    codePage: Word;
    elemSize: Word;
    refCnt: Longint;
    length: Longint;
  end;


{ Returns the length of a given string in bytes, including the size for the string header.
  http://stackoverflow.com/questions/10910631/how-to-calculate-actual-memory-used-by-string-variable }
function GetStringRAMSize(CONST s: string): Integer;
begin
 Result:= ByteLength(s);
 if Result > 0
 then Inc(Result, SizeOf(StrRec) + SizeOf(Char));
end;


function GetStringRAMSize(CONST s: AnsiString): Integer;
begin
 Result := Length(S) * StringElementSize(S);
 if Result > 0
 then Inc(Result, SizeOf(StrRec) + StringElementSize(s));
end;


{ Returns the length of a given string in bytes }
function GetStringSize(CONST s: string): Integer;
begin
 Result:= SizeOf(Char) * Length(s);
end;






{ Efficiently insert a char into Target every x characters. It processes a 10MB string in under 1 sec (inserting every 5 chars) }
function InsertCharEvery(CONST c: char; CONST Target: string; Every: Integer): string;
VAR
   NewLength, Counter, i, iTarget: Integer;
begin
 Counter:= 1;
 iTarget:= 1;
 Assert(Every > 0);

 { Allocate RAM }
 NewLength:= Length(Target)+ (Length(Target) DIV Every);
 if Length(Target) mod Every = 0                          { Make sure the resulted string never ends with CharToInsert }
 then Dec(NewLength);

 SetLength(Result, NewLength);                            { Prealocate ram for the resulted string. This will be faster than allocating a new string every time and prevents memory fragmentation }
 //Result:= StringOfChar('?', NewLength);                 // For debugging only!

 for i:= 1 to Length(Result) DO
  if Counter <= Every
  then
   begin
    Result[i]:= target[iTarget];
    Inc(iTarget);
    Inc(Counter);
   end
  else
   begin
    Counter:= 1;
    Result[i]:= c;
   end;
end;









{ --- REPLACE --- }
procedure ReplaceChar(VAR s: string; CONST SearchFor, ReplaceWith: Char);                { procedure }
VAR i: Integer;
begin
 for i:= 1 TO Length(s) DO
  if   s[I] = SearchFor
  then s[i]:= ReplaceWith;
end;

procedure ReplaceChar(VAR s: AnsiString; CONST SearchFor, ReplaceWith: AnsiChar);        { procedure }
VAR i: Integer;
begin
 for i:= 1 TO Length(s) DO
  if   s[I] = SearchFor
  then s[i]:= ReplaceWith;
end;


function ReplaceCharF(CONST s: string; CONST SearchFor, ReplaceWith: Char): string;       { function }
VAR i: Integer;
begin
 Result:= s;
 for i:= 1 TO Length(Result) DO
  if   Result[I] = SearchFor
  then Result[i]:= ReplaceWith;
end;


{ Replaces text between TagStart and TagEnd with ReplaceWith.
  - Start: position to begin searching from
  - EliminateTags: if True, removes the tags; if False, keeps them
  - LastPos: outputs the position after the replacement (for looping)
  - Returns original string if tags not found.
  Call repeatedly using LastPos as next Start to replace all occurrences. }
function ReplaceBetween(CONST s, TagStart, TagEnd, ReplaceWith: string; Start: Integer; EliminateTags: Boolean; OUT LastPos: Integer): string;
VAR
   iTagStart1, iTagStart2, iTagEndPos: Integer;
   sRemaining: string;
begin
  LastPos:= -1;

  iTagStart1:= Pos(TagStart, s, Start);                         // Where the TagStart begins
  if iTagStart1 < 1 then EXIT(s);

  iTagStart2:= iTagStart1 + Length(TagStart) - 1;               // Where the TagStart ends

  // Search for TagEnd in the remaining text (after TagStart)
  sRemaining:= Copy(s, iTagStart2 + 1, High(Integer));
  iTagEndPos:= PosInsensitive(TagEnd, sRemaining);

  // TagEnd not found - return original string
  if iTagEndPos < 1
  then EXIT(s);

  // Convert relative position to absolute position in original string
  iTagEndPos:= iTagEndPos + iTagStart2;                         // Where the TagEnd begins (absolute)

  if EliminateTags
  then Result:= Copy(s, 1, iTagStart1 - 1) + ReplaceWith        // Exclude TagStart
  else Result:= Copy(s, 1, iTagStart2) + ReplaceWith;           // Include TagStart

  LastPos:= Length(Result);

  if EliminateTags
  then Result:= Result + Copy(s, iTagEndPos + Length(TagEnd), High(Integer))  // Exclude TagEnd
  else Result:= Result + Copy(s, iTagEndPos, High(Integer));                  // Include TagEnd
end;



{ Similar to the above function. But does not replace the text.
  The function returns where TagStart was found. It stops after the "first found". }
function SearchBetween(CONST s, TagStart, TagEnd: string; Start: Integer = 1): Integer;
var
  iTagStart1, iTagStart2: Integer;
begin
  Result:= -1;

  // Start tag
  iTagStart1 := Pos(TagStart, s, Start);
  if iTagStart1 > 0 then
    begin
      iTagStart2 := iTagStart1 + Length(TagStart) - 1;

      // End tag
      if Pos(TagEnd, s, iTagStart2 + 1) > 0
      then Result := iTagStart1;
    end;
end;


function ReplaceString(CONST s, SearchFor, ReplaceWith: string): string;
begin
 Result:= StringReplace(s, SearchFor, ReplaceWith, [rfReplaceAll, rfIgnoreCase]);
end;


function ReplaceStringAnsi(CONST s, SearchFor, ReplaceWith: AnsiString): AnsiString;
begin
 Result:= StringReplace(s, SearchFor, ReplaceWith, [rfReplaceAll, rfIgnoreCase]);
end;



{ --- CLEAN STRING --- }

{ Removes all SPACE characters. The resulted string will get shorter. }
function RemoveSpaces (CONST s: string): string;
begin
 result:= ReplaceText(s, ' ', '');
end;


function RemoveSpaces (CONST s: Ansistring): Ansistring;
begin
 result:= System.AnsiStrings.StringReplace(s, ' ', '', [rfReplaceAll]);
end;


function RemoveTabs (CONST s: string): string;
begin
 Result:= StringReplace(s, Tab, '',[rfReplaceAll]);
end;


function RemoveEnters(CONST s: string): string;                                          { Works both with Linux, Windows and half-enter characters }
begin
 Result:= StringReplace(s,      #10, '', [rfReplaceAll]);
 Result:= StringReplace(Result, #13, '', [rfReplaceAll]);
end;


function RemoveEnters(CONST s: Ansistring): Ansistring;                                  { Works both with Linux, Windows and half-enter characters }
begin
 Result:= System.AnsiStrings.StringReplace(s,      #10, '', [rfReplaceAll]);
 Result:= System.AnsiStrings.StringReplace(Result, #13, '', [rfReplaceAll]);
end;


{ Replaces all line breaks (CRLF, CR, LF) with the specified string.
  Handles Windows CRLF, Unix LF, and old Mac CR line endings.
  CRLF is replaced as a single unit, not as two separate replacements. }
function ReplaceEnters(CONST s, ReplaceWith: string): string;
begin
 // First replace CRLF as a unit, then individual CR and LF
 Result:= StringReplace(s, CRLFw, ReplaceWith, [rfReplaceAll]);
 Result:= StringReplace(Result, #13, ReplaceWith, [rfReplaceAll]);
 Result:= StringReplace(Result, #10, ReplaceWith, [rfReplaceAll]);
end;


{ Cuts the last Enter from a string }
function RemoveLastEnter(CONST s: string): string;
VAR Len, TotalEnters: Integer;
begin
 TotalEnters:= 0;
 Len:= Length(s);

 if Len> 0
 then
  begin
   if CharInSet(s[Len], [CR, LF])
   then Inc(TotalEnters);

   if (Len-1> 0) AND CharInSet(s[Len-1], [CR, LF])
   then Inc(TotalEnters);

   if TotalEnters > 0
   then Result:= system.COPY(s, 1, Len-TotalEnters)
   else Result:= s
  end
 else
   Result:= s;
end;


function RemoveLastEnter(CONST s: AnsiString): AnsiString;
VAR Len, TotalEnters: Integer;
begin
 TotalEnters:= 0;
 Len:= Length(s);

 if Len> 0
 then
  begin
   if CharInSet(s[Len], [CR, LF])
   then Inc(TotalEnters);

   if (Len-1> 0) AND CharInSet(s[Len-1], [CR, LF])
   then Inc(TotalEnters);

   if TotalEnters > 0
   then Result:= system.COPY(s, 1, Len-TotalEnters)
   else Result:= s
  end
 else
   Result:= s;
end;


{ Cuts the last SPACE from a string.
  If there are more spaces only the last one is cut. }
function RemoveLastSpace(CONST s: string): string;
VAR Len: Integer;
begin
 Len:= Length(s);
 if Len > 0
 then
   if  (s[Len]= ' ')
   then Result:= system.COPY(s, 1, Len-1)
   else Result:= s
 else Result:= s;
end;


{ Example: for 'PinkFloydX' it returns 'PinkFloyd' }
function RemoveLastChar(CONST s: string): string;
begin
 Result:= system.COPY(s, 1, Length(s)-1);
end;


function RemoveLastChar(CONST s: AnsiString): AnsiString;     { ANSI version }
begin
 Result:= system.COPY(s, 1, Length(s)-1);
end;


{ Removes the StrToRemove from s if found.
  Example:
     Input parameters: 'PinkFloyd-Ummagumma', '-Ummagumma'
     Output: 'PinkFloyd' }
function RemoveLastChar(CONST s, StrToRemove: string): string;
VAR LastPost: Integer;
begin
 LastPost:= s.LastIndexOf(StrToRemove);   // Note: LastIndexOf is indexed in 0 instead of 1 !
 if LastPost > 0
 then Result:= system.COPY(s, 1, LastPost)
 else Result:= s;
end;


function  RemoveFirstChar(CONST s: string; Char: Char): string;
begin
 if (s > '') AND (s[1] = Char)
 then Result:= system.COPY(s, 2, MaxInt)                              // remove first '/' if there's one
 else Result:= s;
end;









{ Eliminate numbers from the specified string }
function RemoveNumbers(CONST s: string): string;
VAR
   BuffPos, i: Integer;
begin
 BuffPos:= 1;
 SetLength(Result, Length(s));   { Preallocate space }

 for i:= 1 to Length(s) DO
  if NOT CharInSet(s[i], Numbers) then
    begin
     Result[BuffPos]:= s[i];
     Inc(BuffPos);
    end;

 SetLength(Result, BuffPos-1);    { Cut down the prealocated buffer that we haven't used }
end;


{ Eliminate numbers from the specified string. Does not prealocate memory so it is much shower. Make it fast! }
function RemoveNumbers(CONST s: AnsiString): AnsiString;
VAR i: Integer;
begin
 Result:= '';
 for i:= 1 to Length(s) DO
  if NOT CharInSet(s[i], Numbers)
  then Result:= Result+ s[i];    {TODO 2: this is used by CleanSequenceFormatings and it is terribly slow }
end;


{ Converts a text that contains an invalid number to a valid number. For example  '!2345' will return '2345' }
function FixNumber(CONST s: string): Integer;
VAR
  I: Integer;
  LocalS: string;
begin
 I := 1;
 LocalS:= S;

 WHILE I <= Length(LocalS) DO
   if NOT CharIsNumber(LocalS[I])
   AND (LocalS[I] <> '-')
   then Delete(LocalS, I, 1)
   else Inc(I);

 Result:= StrToIntDef(LocalS, 0);
end;


{ Remove chars under 32, except ENTER }
function RemoveLowChars(CONST s: string): string;
VAR i: Integer;
begin
 Result:= '';
 for i:= 1 to Length(s) DO
    if (Ord(s[i]) >= 32)
    OR (s[i] = LF)
    OR (s[i] = CR)
    OR (s[i] = Tab)
    then Result:= Result+ s[i];
end;


function RemoveLowChars(CONST s: AnsiString): AnsiString;
VAR i: Integer;
begin
 Result:= '';
 for i:= 1 to Length(s) DO
    if (Ord(s[i]) >= 32)
    OR (s[i] = LF)
    OR (s[i] = CR)
    OR (s[i] = Tab)
    then Result:= Result+ s[i];
end;



function ReplaceUnicodeChars(CONST S: string; ReplaceWith: char): String; { Replace Unicode characters with something else }
VAR i: Integer;
begin
 Result:= S;
 for i:= 1 to Length(Result) DO
    if (Ord(Result[i]) > 255)
    then Result[i]:= ReplaceWith;
end;


{ Remove ascii characters below 32, the enters and the tabs. Keep spaces. }
function RemoveFormatings(CONST s: string): string;
VAR i: Integer;
begin
 Result:= '';
 for i:= 1 to Length(s) DO
   if Ord(s[i]) > 31
   then Result:= Result+ s[i];
end;


{ Keep only letters and numbers }
function RemoveNonAlphanum(CONST s: string): string;
VAR i: Integer;
begin
 Result:= '';
 for i:= 1 to Length(s) DO
   if CharInSet(s[i], AlphabetNo)
   then Result:= Result+ s[i];
end;



{ Remove enter characters (#10 / #13) from the beginning and the end of the string.
  Also see: System.SysUtils.TStringHelper.TrimStart  &  System.SysUtils.TStringHelper.TrimEnd }
function TrimEnters(CONST s: string): string;
VAR i: Integer;
begin
 Result:= s;

 { Front }
 for i:= 1 to Length(Result) DO
  if (Result[i]<> CR) AND (Result[i]<> LF) then                                                    { Find first non-enter character }
   begin
    Result:= system.COPY(Result, i, Length(Result));
    Break;
   end;

 { End }
 for i:= Length(Result) downto 1 DO
  if (Result[i]<> CR) AND (Result[i]<> LF) then                                                    { Find first non-enter character but this time from the end towards beginning }
   begin
    Result:= system.COPY(Result, 1, i);
    Break;
   end;
end;



{ Remove characters from both ends of a string until Limiter is found. Limiter is kept.
  Example: if Limiter is $ and the string is 'xxxxx$ThisIsMyString$@@@@@@'. Then the result will be '$ThisIsMyString$' }
function TrimUntil(CONST s: string; Limiter: Char): string;
VAR i: Integer;
begin
 Result:= s;

 for i:= 1 to Length(Result) DO
  if Result[i]= Limiter then                                                              { Find first Limiter character }
   begin
    Result:= system.COPY(Result, i, Length(Result));
    Break;
   end;

 for i:= Length(Result) downto 1 DO
  if Result[i]= Limiter then                                                              { Find last Limiter character }
   begin
    Result:= system.COPY(Result, 1, i);
    Break;
   end;
end;


{ Remove characters from both ends of a string until something different than Limiter is found.
  Example:  TrimUntilDiff('--ACGT--', '-') returns 'ACGT' }
function TrimUntilDiff(CONST s: string; Limiter: char): string;
VAR i, Start, Stop: Integer;
begin
 Start:= 1;
 Stop := s.Length;

 for i:= 1 to Length(s) DO
  if s[i] <> Limiter then                                                                 { Find first character that is NOT Limiter }
   begin
    Start:= i;
    Break;
   end;

 for i:= Length(s) downto 1 DO
  if s[i] <> Limiter then                                                                 { Find last character that is NOT Limiter }
   begin
    Stop:= i;
    Break;
   end;

 Result:= CopyTo(s, start, stop);
end;


{ Converts multiple spaces to Tab or other similar separator.
  For example Retabulate('xx   xx  yy, 3, Tab') will convert the first 3 spaces to tab but not also the next 2 spaces }
function Retabulate(CONST s, Delimiter: string; SpaceCount: Integer): string;
VAR
   Spaces: string;
   i: Integer;
begin
 Result:= '';
 Spaces:= '';

 for i:= 1 to Length(s) DO
  if s[i] <> ' '
  then
   begin
     if Length(Spaces) >= SpaceCount
     then Result:= Result+ Delimiter+ s[i]
     else Result:= Result+ Spaces   + s[i];
     Spaces:= '';
   end
  else
    Spaces:= Spaces+ s[i];
end;


// Replace character #160 (A0) with space
function ReplaceNbsp(CONST s, ReplaceWith: string): string;
VAR i: integer;
begin
 if s= '' then EXIT('');
 Result:= '';

 for i:= 1 to Length(s) DO
  if (s[i]= #160 {A0})
  then Result:= Result+ ReplaceWith
  else Result:= Result+ s[i];
end;







{============================================================================================================
 ENTERS
=============================================================================================================

 Enter format on:
    Win: 0D0A = CR LF = $D $A
    Mac: 0D
    Nix: 0A
============================================================================================================}

function ReplaceLonellyCR(CONST s, ReplaceWith: string): string;
VAR i: integer;
begin
 if s= '' then EXIT('');
 Result:= '';

 { Check all chars except the last }
 for i:= 1 to Length(s)-1 DO
  if (s[i]= CR) AND (s[i+1]<> LF)
  then Result:= Result+ ReplaceWith
  else Result:= Result+ s[i];

 { Check the last char }
 if NOT s.EndsWith(CR)
 then Result:= Result+ LastChar(s);
end;


function ReplaceLonellyLF(CONST s, ReplaceWith: string): string;
VAR i: integer;
begin
 if s= '' then EXIT('');
 Result:= '';

 { Check the first char }
 if s[1]<> LF
 then Result:= s[1];

 for i:= 2 to Length(s) DO
  if  (s[i]= LF{A}) AND (s[i-1]<> CR{D})
  then Result:= Result+ ReplaceWith
  else Result:= Result+ s[i];
end;



{ Convert Linux enters to Windows enters
  Deprecated. Instead use System.SysUtils.AdjustLineBreaks(s, tlbsCRLF) }
function LinuxEnter2Win(CONST s: string): string;
VAR i: integer;
begin
 if s= '' then EXIT('');

 { Check the first char }
 Result:= s[1];

 { Check all chars except the last }
 for i:= 2 to Length(s)-1 DO
  if  (s[i]= #10) AND (s[i+1]<> #13) AND (s[i-1]<> #13)
  then Result:= Result+ CRLFw   {CRLF = $D$A}
  else Result:= Result+ s[i];

 { Check also the last char }
 if s[Length(s)]= #10
  then Result:= Result+ CRLFw
  else Result:= Result+ s[Length(s)];
end;


{ Converts the CRLF text to the actual CRLF (binary 13/10)
  Warning: we don't replace individual CR, LF groups, because there is a high chance we will find that in our text. Example: CRysis }
function CRLFToEnter(CONST s: string): string;
begin
 Result:= StringReplace(s     , 'CRLF ' , 'CRLF', [rfReplaceAll]);
 Result:= StringReplace(Result, ' CRLF' , 'CRLF', [rfReplaceAll]);
 Result:= StringReplace(Result, 'CRLF'  ,  CRLF , [rfReplaceAll]);
end;


{ Replaces #13 with the text 'CR' and #10 with the text 'LF' }
function EnterToCRLF (CONST s: string): string;
begin
 Result:= StringReplace(s  , CRLFw, ' CRLF ', [rfReplaceAll]);
 //I need spaces arround CRLF because of the cTranslator.pas
 //The user/DeepL will see better the text to be translated if there are spaces arround CRLF
end;






{============================================================================================================
   SPLIT STRING
   http://stackoverflow.com/questions/2625707/split-a-string-into-an-array-of-strings-based-on-a-delimiter
=============================================================================================================

{ Note: Exists System.StrUtils.SplitString and Classes.ExtractStrings }
function SplitText(CONST Text, Delimiter: string): TStringList;                                    { Splits a text in lines and puts the lines in a TStringList } {Note: Exista System.StrUtils.SplitString } { Old name: SplitStrings }
begin
 Result:= TStringList.Create;
 Result.Text:= StringReplace( Text, Delimiter, #13#10, [rfReplaceAll] );
end;


{ Split a line of text in its components.
  For example 'ClientName=RogerWaters' will return 'ClientName' and 'RogerWaters' }
procedure SplitLine(CONST Text, Delimiter: string; OUT sField, sValue: string);   // Old name: SplitString
VAR FoundAt: Integer;
begin
 FoundAt:= Pos(Delimiter, Text);
 sField := Trim( CopyTo(Text, 1, FoundAt-1) );
 sValue := Trim( system.COPY (Text, FoundAt+ Length(Delimiter), MaxInt) );
end;


{ Split a string in multiple rows every time the #13#10 char is found (I took this code from Embarcadero's TStringList.Text:= s ) }
procedure SplitStrings(CONST Text: string; TSL: TStringList);                  // Old name: SplitString
VAR
   P, Start: PChar;
   S: string;
begin
 P := Pointer(Text);
 if P <> NIL then
   begin
    WHILE P^ <> #0 DO    // This is a lot faster than using StrPos/AnsiStrPos when SeqNameBreak is the default (#13#10)
     begin
       Start := P;
       WHILE NOT CharInSet(P^, [#0, #10, #13])
         DO Inc(P);
       SetString(S, Start, P - Start);
       TSL.Add(S);
       if P^ = #13 then Inc(P);
       if P^ = #10 then Inc(P);
     end;
   end;
end;


procedure SplitStringList(StringList: TStrings; OUT OutList1, OutList2: TStringArray);             { Split each row of the provided StringList into two parts. The two resulted strings are placed in an ArrayOfStrings }
VAR
   i: Integer;
begin
 SetLength(OutList1, StringList.Count);
 SetLength(OutList2, StringList.Count);

 for i:= 0 to StringList.Count-1
  DO SplitLine(StringList[i], ',', OutList1[i], OutList2[i]);
end;


{ Split each row of StringList into string + integer parts (comma-delimited).
  Example: 'Name,123' splits into OutList1='Name', OutList2=123.
  Invalid integers default to 0. }
procedure SplitStringListI(StringList: TStrings; OUT OutList1: TStringArray; OUT OutList2: System.Types.TIntegerDynArray);
VAR
   i: Integer;
   s: string;
begin
 Assert(StringList <> NIL, 'StringList cannot be nil');
 SetLength(OutList1, StringList.Count);
 SetLength(OutList2, StringList.Count);

 for i:= 0 to StringList.Count - 1 DO
  begin
   SplitLine(StringList[i], ',', OutList1[i], s);
   OutList2[i]:= StrToIntDef(s, 0);  // Use StrToIntDef to avoid exceptions
  end;
end;


procedure SplitStringAtPos(CONST Text: string; CONST Pos: Integer; OUT s1, s2: string);            { Split a string in two substrings at the specified position. The char at Pos will be included in the first string. }
begin
 s1:= system.COPY(Text, 1, Pos);
 s2:= system.COPY(Text, Pos+1, Length(Text));
end;


procedure SplitStringAtPos(CONST Text: AnsiString; CONST Pos: Integer; OUT s1, s2: AnsiString);    { Split a string in two substrings at the specified position. The char at Pos will be included in the first string. }
begin
 s1:= system.COPY(Text, 1, Pos);
 s2:= system.COPY(Text, Pos+1, Length(Text));
end;










{ Works only with letters. It might be faster than IsUpcase }
function IsUpcaseLetter(CONST C: Char): Boolean;
begin
 Result:= CharInSet(c, LightCore.Types.LettersUpCase);
end;


function IsUpcase(CONST C: Char): Boolean;     { Works only with letters. }
begin
 Result:= c = UpCase(c);
end;




{ Insert (ForcedLength-1) zeros in front of the specified string. ForcedLength shows which is the desired lenght of the new string. Example: LeadingZeros('a', 4) will result in '000a'
  Note: you can also do it like this:   To convert an integer to a string with minimum length, use the Str procedure:  Str(123:6, s); // s is set to '   123' }
function LeadingZeros(CONST s: string; ForcedLength: integer): string;
begin
 Result:= s;
 WHILE Length(Result)< ForcedLength DO
  Result:= '0'+ Result;
end;


{ Alternative implementation using Format. Pads numeric string with leading zeros.
  Example: LeadingZeros2('42', 5) returns '00042'. }
function LeadingZeros2(CONST s: string; ForcedLength: Integer): string;
VAR NumValue: Integer;
begin
 NumValue:= StrToIntDef(s, 0);
 Result:= Format('%.*d', [ForcedLength, NumValue]);
end;



{ Same as above except that the user doesn't have to specify how many zeros to add.
  Instead the function will determine this automaticxally based on the MaxValue. For example LeadingZeros('1', 50) will generate '01' but LeadingZeros('1', 500) will generate '001'.   Note: you can also do it like this:   To convert an integer to a string with minimum length, use the Str procedure:  Str(123:6, s); // s is set to '   123' }
function LeadingZerosAuto(CONST s: string; MaxValue: integer): string;
VAR ForcedLength: Integer;
begin
 ForcedLength:= Length(IntToStr(MaxValue));
 Result:= LeadingZeros(s, ForcedLength);
end;



{ This is fast }
{ Note: you can also do it like this:   To convert an integer to a string with minimum length, use the Str procedure:  Str(123:6, s); // s is set to '   123' }
{ Make sure the string has ForcedLength. If not, add some extra characters at its end to make it that long  }
function MakeStringLongRight(CONST s, c: Char; ForcedLength: integer): string;
begin
 if Length(s) >= ForcedLength then EXIT(s);

 Result:= StringOfChar(c, ForcedLength- Length(s));
 Result:= s+ Result;
end;


{ Make sure the string has ForcedLength. If not, add some extra characters at its end to make it that long  }
function MakeStringLongRight(CONST s, c: AnsiChar; ForcedLength: integer): AnsiString;
begin
 if Length(s) >= ForcedLength then EXIT(s);

 Result:= StringOfChar(c, ForcedLength- Length(s));
 Result:= s+ Result;
end;


{ Make sure the string has ForcedLength. If not, add some extra characters at its end to make it that long  }
{TODO: This is slow. Use DupeString. }
function MakeStringLongRight(CONST s, Pad: string; ForcedLength: integer): string;
begin
 Assert(Pad > '');
 Result:= s;
 WHILE Length(Result)< ForcedLength
  DO Result:= Result+ Pad;
 Result:= system.COPY(Result, 1, ForcedLength);                                                           { Trim to the specified lenght }
end;


{ Makes sure the string has a total of ForcedLength chars. If not, add some extra characters at its front to make it that long  }
function MakeStringLongLeft (CONST s, Pad: string; ForcedLength: integer): string;
begin
 Result:= s;
 WHILE Length(Result)< ForcedLength
  DO Result:= Pad+ Result;
 Result:= system.COPY(Result, 1, ForcedLength);                                                           { Trim to the specified lenght }
end;


{ Exists: System.StrUtils.DupeString and StuffString }
function GenerateString(RepeatTimes: Integer; C: char): string;
begin
 Result:= System.StringOfChar(C, RepeatTimes);
end;














{ Generates a random string with length between minLen and maxLen (inclusive).
  Returns printable ASCII characters (codes 65-124: A-Z, some symbols, a-z).
  IMPORTANT: Call Randomize once before using this function. }
function GenerateRandString(minLen, maxLen: Integer): string;
var
  i, Len: Integer;
begin
  Assert(minLen > 0, 'minLen must be positive');
  Assert(maxLen >= minLen, 'maxLen must be >= minLen');

  // Random length between minLen and maxLen
  Len:= minLen + Random(maxLen - minLen + 1);
  SetLength(Result, Len);
  for i:= 1 to Len DO
    Result[i]:= Chr(65 + Random(60));  // ASCII 65-124
end;


{
 Example of output: '3D5AF2BAB9F94491B4B922DD20FFB259'

 String length       Permutations
     8                 9.5E+11    (950 billion)
    10                 6.6E+14    (660 trillion)
    12                 4.0E+17
    16                 8.5E+22
    20                 7.9E+27
    32                 1.7E+39

 More about random num generators:
    http://delphi.about.com/od/delphitips2009/qt/delphi-unique-random-number-generator-challenge.htm
    http://stackoverflow.com/questions/15312704/gettempfilename-creates-an-empty-file
 }
function GenerateUniqueString(Len: Integer=32): string;
begin
 Assert(Len <= 32, 'Max 32 chars allowed in unique strings!');

 Result:= System.IOUtils.TPath.GetGUIDFileName;
 Result:= system.COPY(Result, 1, Len);
end;


{ This will return ONLY literary strings (letters and numbers).
  YOU MUST call randomize before calling this function! }                                                     { Old name: GenerateRandomString }
function GenerateRandStringLet(Len: Integer): string;
VAR str: string;
    i: Integer;
begin
  str:= 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';    //string with all possible chars
  SetLength(Result, Len);
  for i:= 1 to len DO
    Result[i]:= str[Random(Length(str)) + 1];
end;



{ Returns a string that is composed from an alternation of consonants and vowels:
  Example: 'AMERUTIS' }
function GenerateRandomWord(Len: Integer=16; StartWithVowel: Boolean= FALSE): string;
CONST
   sVowels: string= 'AEIOUAEIOUY';
   sConson: string= 'BCDFGHJKLMNPRSTVBCDFGHJKLMNPQRSTVWXZ';    { Some characters are more preffered so we put them twice in the string. WXZ are not so likable so I put them once. }
VAR
   i: Integer;
   B: Boolean;
begin
  B:= StartWithVowel;
  SetLength(Result, Len);
  for i:= 1 to len DO
   begin
    if B
    then Result[i]:= sVowels[Random(Length(sVowels)) + 1]
    else Result[i]:= sConson[Random(Length(sConson)) + 1];
    B:= NOT B;
   end;
end;



{ Returns a random name from a list of 200 unique names }
function GetRandomPersonName: string;
CONST
   Names: array[0..199] of string = ('Aaron','Abigail','Adam','Alan','Albert','Alexander','Alexis','Alice','Amanda','Amber','Amy','Andrea','Andrew','Angela','Ann','Anna','Anthony','Arthur','Ashley','Austin','Barbara','Benjamin','Betty','Beverly','Billy','Bobby','Brandon','Brenda','Brian','Brittany','Bruce','Bryan','Carl','Carol','Carolyn','Catherine','Charles',
                                     'Charlotte','Cheryl','Christian','Christina','Christine','Christopher','Cynthia','Daniel','Danielle','David','Deborah','Debra','Denise','Dennis','Diana','Diane','Donald','Donna','Doris','Dorothy','Douglas','Dylan','Edward','Elijah','Elizabeth','Emily','Emma','Eric','Ethan','Eugene','Evelyn','Frances','Frank','Gabriel','Gary','George','Gerald','Gloria','Grace','Gregory','Hannah','Harold','Heather','Helen','Henry',
                                     'Isabella','Jack','Jacob','Jacqueline','James','Janet','Janice','Jason','Jean','Jeffrey','Jennifer','Jeremy','Jerry','Jesse','Jessica','Joan','Joe','John','Jonathan','Jordan','Jose','Joseph','Joshua','Joyce','Juan','Judith','Judy','Julia','Julie','Justin','Karen','Katherine','Kathleen','Kathryn','Kayla','Keith','Kelly','Kenneth','Kevin','Kimberly','Kyle','Larry','Laura',
                                     'Lauren','Lawrence','Linda','Lisa','Logan','Lori','Louis','Madison','Margaret','Maria','Marie','Marilyn','Mark','Martha','Mary','Mason','Matthew','Megan','Melissa','Michael','Michelle','Nancy','Natalie','Nathan','Nicholas','Nicole','Noah','Olivia','Pamela','Patricia','Patrick','Paul','Peter','Philip','Rachel','Ralph','Randy','Raymond','Rebecca','Richard','Robert','Roger','Ronald','Roy','Russell','Ruth','Ryan','Samantha','Samuel','Sandra','Sara','Sarah','Scott','Sean','Sharon','Shirley','Sophia','Stephanie','Stephen','Steven','Susan','Teresa','Terry','Theresa','Thomas','Timothy','Tyler','Victoria','Vincent','Virginia','Walter','Wayne','William','Willie','Zachary');
begin
  Result:= Names[Random(Length(Names))];
end;


function GetRandomStreetName: string;
CONST
   Names: array[0..40] of string = ('Abbey Road', 'Abbotswell Street', 'Abingdon Street', 'Acacia Road', 'Acorn Street', 'Acton Street', 'Adam Street', 'Adelaide Place', 'Admiral Street', 'Agnes Street', 'Albany Street', 'Albemarle Street', 'Albert Cottages', 'Albert Mews', 'Albert Road', 'Albion Mews', 'Alexander Street', 'Alfred Mews', 'Allen Street', 'Allington Street', 'Alma Road', 'Amberley Road', 'Anchor Alley', 'Angel Count', 'Ann Street', 'Anstey Road', 'Beech Street', 'Belgrave Road', 'Belgrave Street', 'Belgrave Terrace', 'Bell Court', 'Bell Yard', 'Belmont Road', 'Bendall Street', 'Bendmore Road', 'Bennett Street', 'Bentinck Street', 'Beresford Street', 'Berkley Street', 'Berwick Street', 'Birdcage Walk');
begin
  Result:= Names[Random(Length(Names))];
end;


{ Returns a TStringList containing names of classic rock bands.
  Caller is responsible for freeing the returned list. }
function GetRockBands: TStringList;
begin
  Result:= TStringList.Create;
  Result.Add('Pink Floyd');
  Result.Add('The Moody Blues');
  Result.Add('Fleetwood Mac');
  Result.Add('Queen');
  Result.Add('The Doors');
  Result.Add('Led Zeppelin');
  Result.Add('The Beatles');
  Result.Add('The Zombies');
  Result.Add('The Pretenders');
  Result.Add('Foreigner');
  Result.Add('The Animals');
  Result.Add('Arcade Fire');
  Result.Add('The Byrds');
  Result.Add('The Who');
  Result.Add('Boston');
  Result.Add('The Rolling Stones');
  Result.Add('Metallica');
  Result.Add('The Beach Boys');
  Result.Add('The Yardbirds');
  Result.Add('Lynyrd Skynyrd');
  Result.Add('Def Leppard');
  Result.Add('The Small Faces');
  Result.Add('The Velvet Underground');
  Result.Add('Radiohead');
  Result.Add('Black Sabbath');
  Result.Add('The Troggs');
  Result.Add('Kansas');
  Result.Add('The Police');
  Result.Add('The Jimi Hendrix Experience');
  Result.Add('Rush');
  Result.Add('The Hollies');
  Result.Add('Tom Petty and the Heartbreakers');
  Result.Add('The Eagles');
  Result.Add('The Cars');
  Result.Add('Deep Purple');
  Result.Add('The Clash');
  Result.Add('The Kinks');
  Result.Add('The Ramones');
  Result.Add('Jethro Tull');
  Result.Add('The Cult');
  Result.Add('Nirvana');
  Result.Add('U2');
  Result.Add('Pearl Jam');
  Result.Add('Soundgarden');
  Result.Add('Foo Fighters');
end;



















{============================================================================================================
   STRING SORT
============================================================================================================}

{ Natural compare two strings (for filename sorting).
  Digits in strings are compared as numbers, not characters.
  Example: 'pic1, pic2, pic10' sorts correctly (not 'pic1, pic10, pic2').
  Returns: negative if s1 < s2, 0 if equal, positive if s1 > s2.
  Also see: StrCmpLogicalW for Windows API alternative. }
function FileNameNaturalSort(s1, s2: String): Integer;

  { Extracts leading number from txt starting at position n.
    Removes the extracted portion from txt. }
  function ExtractNr(n: Integer; VAR txt: String): Int64;
  begin
    while (n <= Length(txt)) AND (txt[n] >= '0') AND (txt[n] <= '9') DO
      Inc(n);
    Result:= StrToInt64Def(system.COPY(txt, 1, n - 1), 0);
    Delete(txt, 1, n - 1);
  end;

VAR b: Boolean;
begin
  Result:= 0;
  s1:= LowerCase(s1);
  s2:= LowerCase(s2);
  if (s1 <> s2) and (s1 <> '') AND (s2 <> '') then
   begin
    b:= False;
    while (not b) DO
     begin
      if  CharIsNumber(s1[1])
      AND CharIsNumber(s2[1])
      then Result:= Sign(ExtractNr(1, s1) - ExtractNr(1, s2))
      else Result:= Sign(Integer(s1[1])   - Integer(s2[1]));

      b:= (Result <> 0) OR (Min(Length(s1), Length(s2)) <  2);
      if not b then
       begin
        Delete(s1,1,1);
        Delete(s2,1,1);
       end;
    end;
  end;

  if Result = 0 then
    if (Length(s1) = 1) AND (Length(s2) = 1)
    then Result:= Sign(Integer(s1[1]) - Integer(s2[1]))
    else Result:= Sign(Length(s1) - Length(s2));
end;


{============================================================================================================
   STRING COMPARE
============================================================================================================

 Levenshtein Distance:
  - Counts minimum edits (insert, delete, substitute) to transform string A into B
  - Returns a distance (lower = more similar)
  - Example: "John" → "Jon" = distance 1 (one deletion)

  FuzzyStringCompare:
  - Counts matching characters in approximately similar positions
  - Uses a tolerance window (about 1/3 of string length)
  - Returns a percentage (0-100, higher = more similar)
  - Greedy positional matching, not edit-based

============================================================================================================}

{
  FuzzyStringCompare calculates an approximate similarity percentage between two strings.
  It uses a heuristic to count matching characters that are roughly in similar positions, allowing for a tolerance based on string lengths. The tolerance is about one-third of the longer string's length plus the length difference.
  The algorithm greedily matches characters from the shorter string to unique positions in the longer string, if within the tolerance distance and not previously matched. It advances positions and backtracks when necessary to skip unmatched sections.

  Comparisons are case-sensitive, as in the original. You might want to lowecase your strings.

  Examples:
  - 'John' and 'John' → 100%
  - 'John' and 'Jon' → 75%

  Also see: github.com/DavidMoraisFerreira/FuzzyWuzzy.pas }
function FuzzyStringCompare(const S1, S2: string): Integer;
var
  StrLong, StrShort: string;
  Matches: Integer;           // Number of matching characters found
  Pos1, Pos2: Integer;        // Current positions in StrLong and StrShort
  LenLong, LenShort: Integer; // Lengths of the longer and shorter strings
  I: Integer;                 // Loop counter for initialization
  Tolerance: Integer;         // Allowed position difference (fuzz factor)
  Tested: TArray<Boolean>;    // Tracks matched positions in StrLong (dynamic to handle any length)
begin
  // Ensure StrLong is the longer string
  if Length(S1) >= Length(S2)
  then
    begin
      StrLong := S1;
      StrShort:= S2;
    end
  else
    begin
      StrLong := S2;
      StrShort:= S1;
    end;

  LenLong := Length(StrLong);
  LenShort:= Length(StrShort);

  // Early exit for empty short string
  if LenShort = 0 then Exit(0);

  // Calculate tolerance: ~1/3 of longer length + length difference
  Tolerance := LenLong div 3 + (LenLong - LenShort);

  // Initialize dynamic array for tested positions
  SetLength(Tested, LenLong + 1);
  for I := 1 to LenLong do
    Tested[I] := False;

  Pos1 := 1;
  Pos2 := 1;
  Matches := 0;

  repeat
    if not Tested[Pos1] then
      begin
        // Check for match within tolerance
        if (StrLong[Pos1] = StrShort[Pos2]) and (Abs(Pos1 - Pos2) <= Tolerance) then
          begin
            Tested[Pos1] := True;
            Inc(Matches);
            Inc(Pos1);
            Inc(Pos2);
            if Pos1 > LenLong then Pos1 := 1;
          end
        else
          begin
            // No match: advance Pos1
            Inc(Pos1);
            if Pos1 > LenLong then
            begin
              // Backtrack to last matched or start
              Pos1 := LenLong;
              while (Pos1 > 1) and not Tested[Pos1] do
                Dec(Pos1);
              Inc(Pos2);
            end;
          end;
      end
    else
      begin
        // Position already matched: skip it
        Inc(Pos1);
        if Pos1 > LenLong then
        begin
          // Backtrack to last matched or start
          Pos1 := LenLong;
          repeat
            Dec(Pos1);
          until (Pos1 = 1) or Tested[Pos1];
          Inc(Pos2);
        end;
      end;
  until Pos2 > LenShort;

  // Compute percentage based on longer string
  Result := 100 * Matches div LenLong;
end;


function LevenshteinDistance(const s1, s2: string): Integer;
var
  Arr: array of array of integer;
  LenS1, LenS2, i1, i2: integer;
begin
  LenS1 := length(s1);
  LenS2 := length(s2);
  if LenS1 = 0 then Exit(LenS2);
  if LenS2 = 0 then Exit(LenS1);

  SetLength(Arr, LenS1 + 1, LenS2 + 1);
  for i1:= 0 to LenS1 do Arr[i1, 0] := i1;
  for i2:= 0 to LenS2 do Arr[0, i2] := i2;

  for i1:= 1 to LenS1 do
    for i2:= 1 to LenS2 do
      Arr[i1, i2]:= Min(Min(Arr[i1-1, i2] + 1, Arr[i1,i2-1]+1), Arr[i1-1,i2-1] + Integer(s1[i1] <> s2[i2]));

  Result:= Arr[LenS1, LenS2];   // Distance
end;


{ Returns similarity as percentage (0-100) based on Levenshtein distance.
  100 = identical strings, 0 = completely different.
  Case-sensitive. Lowercase your strings before calling if needed. }
function LevenshteinSimilarity(const s1, s2: string): Integer;
var
  MaxLen, Distance: Integer;
begin
  MaxLen:= Max(Length(s1), Length(s2));
  if MaxLen = 0
  then EXIT(100);  // Both empty = identical

  Distance:= LevenshteinDistance(s1, s2);
  Result:= Round(100 * (1 - Distance / MaxLen));
end;



{-----------------------------------------------------------------------------

 Naming conventions:
   INCLUDE -> indicates that the portion where I find the match is cut out
   EXCLUDE -> indicates that the portion where I find the match is kept

-----------------------------------------------------------------------------}

{
  |---|match|----------------|
  |-- CUT --|----- KEEP -----|   }
function  CutInclude2Left (CONST s, SearchFor: string): string;    { delete all chars from end of MATCH to Left }
VAR match: Integer;
begin
 Result:= '';
 match:= Pos(LowerCase(SearchFor), LowerCase(s));
 if match> 0
 then Result:= system.COPY(s, match+ Length(SearchFor), MaxInt);
end;



{  |----------------|match|xxx|
   |------ KEEP ----|-- CUT --|  }
function  CutInclude2Right(CONST s, SearchFor: string): string;    { delete all chars from beg of MATCH to Right }
VAR match: Integer;
begin
 Result:= '';
 match:= Pos(LowerCase(SearchFor), LowerCase(s));
 if match> 0
 then Result:= system.COPY(s, 1, match-1);
end;



{     |---|Match|-----------------|                                old name: CutIn2Left
      |CUT|--------- KEEP --------|  }
function  CutExcludeLeft(CONST s, SearchFor: string): string;     { delete all chars from beginning of MATCH to Left }
VAR match: Integer;
begin
 Result:= '';
 match:= Pos(LowerCase(SearchFor), LowerCase(s));
 if match> 0
 then Result:= system.COPY(s, match, MaxInt);
end;



{     |------ KEEP ---------Match|xxx|
                                 |CUT|  }
function  CutExcludeRight(CONST s, SearchFor: string): string;                { delete all chars from end of MATCH to Right }
VAR match: Integer;
begin
 Result:= '';
 match:= Pos(LowerCase(SearchFor), LowerCase(s));
 if match> 0
 then Result:= system.COPY(s, 1, match+ Length(SearchFor)-1);
end;












{ Copy the text between iFrom and iTo (including) }
function CopyTo(CONST s: AnsiString; iFrom, iTo: integer): AnsiString;
begin
 Result:= system.Copy(s, iFrom, iTo-iFrom+1);                                 { +1 to include the character at position 'iFrom' }
end;


{ Copy the text between iFrom and ending at iTo (including) }
function CopyTo(CONST s: string; iFrom, iTo: integer): string;
begin
 Result:= system.COPY(s, iFrom, iTo-iFrom+1);   { +1 in order to include char at 'iFrom' }
end;



{ Find sFrom in s. Returns the string from the postion where the text was found, to the end.
  Similar to the classic 'Copy' but as start position we use a string instead of an integer.

  IncludeMarker = true  -> copy from the point where the sFrom was found (so the sFrom is included in the result)
  IncludeMarker = false -> copy starts after the sFrom (so the sFrom is not included in the result)

  SearchOffset is the offset in s where the search starts.

  Examples:
     CopyFrom('123:456', ':', True , 1) will return ':456'
     CopyFrom('123:456', ':', False, 1) will return '456'
  }
function CopyFrom(CONST s, sFrom: string; Count: Integer; IncludeMarker: Boolean= TRUE; SearchOffset: Integer= 1): string;
VAR iFrom: Integer;
begin
 iFrom:= Pos(sFrom, s, SearchOffset);
 if iFrom > 0
 then
   if IncludeMarker
   then Result:= system.COPY(s, iFrom, Count)
   else Result:= system.COPY(s, iFrom+ Length(sFrom), Count)
 else Result:= '';
end;


function CopyFrom(CONST s, sFrom: AnsiString; Count: Integer; IncludeMarker: Boolean= TRUE; SearchOffset: Integer= 1): AnsiString;
VAR iFrom: Integer;
begin
 iFrom:= Pos(sFrom, s, SearchOffset);
 if iFrom > 0
 then
   if IncludeMarker
   then Result:= system.COPY(s, iFrom, Count)
   else Result:= system.COPY(s, iFrom+ Length(sFrom), Count)
 else Result:= '';
end;


{ Returns what is between From/To markers.
  If the ending (sTo) is not found, we copy until the end of the string.

  Example:
     CopyFromTo('abcdX1234Ydcba', 'X', 'Y') will return '12345' }
function CopyFromTo(CONST s, sFrom, sTo: string; IncludeMarkers: Boolean= FALSE): string;
VAR iFrom, iTo: Integer;
begin
 iFrom:= System.Pos(sFrom, s);
 if iFrom > 0
 then
  begin
   if NOT IncludeMarkers
   then iFrom:= iFrom+ Length(sFrom);

   iTo:= Pos(sTo, s, iFrom+1)-1;

   if IncludeMarkers
   then iTo:= iTo+ Length(sTo);

   if iTo < 1              { If the ending marker (sTo) was not found, copy until the end of the string }
   then iTo:= maxint;
   Result:= system.COPY(s, iFrom, iTo-iFrom+1)
  end
 else Result:= '';
end;



{ Extract the text between the tags.
  For example '<H>Title</H>' will return 'Title' is iFrom= '<H>' and iTo= '</H>' }
function ExtractTextBetween(CONST s, TagStart, TagEnd: string): string;
begin
 Result:= CopyFromTo(s, TagStart, TagEnd, False);
end;




 { Copy the text between iFrom and sTo string was found.
   IncludeMarker toggles the inclusion/exclusion of sTo in the final result.
   if the sTo string is not found, the function will return an empty string unless CopyAllMarkerNotFound is true }
function CopyTo(CONST s: String; iFrom: Integer; CONST sTo: string; IncludeMarker: Boolean= TRUE; CopyAllMarkerNotFound: Boolean= FALSE; MarkerOffset: Integer= 1): String;
VAR iTo: Integer;
begin
 if MarkerOffset< iFrom
 then MarkerOffset:= iFrom;

 iTo:= Pos(sTo, s, MarkerOffset);

 if NOT IncludeMarker
 then iTo:= iTo- Length(sTo);

 if iTo < 1
 then
   if CopyAllMarkerNotFound
   then Result:= system.COPY(s, iFrom, maxint)
   else EXIT('')
 else Result:= system.COPY(s, iFrom, iTo- ifrom+ 1);
end;













{============================================================================================================
   Replace short words (length < MinLength) with spaces.
   It also filters out words that only contain consonants.
============================================================================================================}
procedure ReplaceShortWords(VAR s: string; MinLength: Integer; FilterIfNoWovels: Boolean);     // Used by Words Extractor
VAR
   StartPos, EndPos: Integer;
   Lungime, i, Index: Integer;
   VowelFound: Boolean;
   sWord: string;
begin
  Index   := 1;
  Lungime := Length(s) ;
  WHILE Index <= Lungime DO
   begin

     // Find word start
     WHILE (Index <= Lungime) AND (IsWordSeparator(s[Index]))
       DO Inc(Index);
     StartPos:= Index;

     // Find word end
     //if Index > Lungime then Break;
     WHILE (Index <= Lungime) AND (NOT IsWordSeparator(s[Index]))
        DO Inc(Index);
     EndPos:= Index-1;

     // Retrieve word
     //if Index > Lungime then Break;
     sWord:= CopyTo(s, StartPos, EndPos);

     // Filter word if too short
     if length(sWord) <= MinLength     //todo: we don't need to copy the word. do directly: EndPos-StartPos < MinLength-1
     then
        for i:= StartPos to EndPos
         DO s[i]:= ' '      // we "delete" this word.
     else
      if FilterIfNoWovels then
       begin
         // Filter word if it only contains consonants
         VowelFound:= FALSE;
         for i:= StartPos to EndPos do
           if CharInSet(s[i], Vowels) then
             begin
              VowelFound:= true;
              break;
             end;
         if NOT VowelFound then
          for i:= StartPos to EndPos
           DO s[i]:= ' ';       // we "delete" this word.
       end;

   end;
end;


function ReplaceWholeWords(const InputStr, OldWord, NewWord: string; const Delimiters: array of Char): string;
var
  DelimiterSet: TSysCharSet;
  StartPos, OldWordLength, InputLength: Integer;
begin
  Result := InputStr;
  OldWordLength := Length(OldWord);
  InputLength := Length(Result);

  DelimiterSet := [];
  for var Ch in Delimiters do
    Include(DelimiterSet, ansichar(Ch));

  StartPos := 1;
  while StartPos <= InputLength do
  begin
    StartPos := PosEx(OldWord, Result, StartPos);

    if StartPos = 0 then
      Break;

    // Check if the matched word is surrounded by delimiter characters
    if ((StartPos = 1) or CharInSet(Result[StartPos - 1], DelimiterSet)) and
       (((StartPos + OldWordLength - 1) = InputLength)
         or CharInSet(Result[StartPos + OldWordLength], DelimiterSet)) then
    begin
      // Replace the whole word
      Delete(Result, StartPos, OldWordLength);
      Insert(NewWord, Result, StartPos);
      StartPos := StartPos + Length(NewWord);
    end
    else
      // Move the start position after the current match
      Inc(StartPos);
  end;
end;


function ReplaceWholeWords(const InputStr, OldWord, NewWord: string): string;
var
  Delimiters: array of Char;
  Index, i: Integer;
begin
  // Define custom delimiters
  Index:= 0;
  SetLength(Delimiters, 255);
  for i := 0 to 255 do
  begin
    if not (Char(i).IsLetterOrDigit) then
    begin
      Delimiters[Index] := Char(i);
      Inc(Index);
    end;
  end;
  SetLength(Delimiters, Index); // Truncate unused memory

  Result:= ReplaceWholeWords(InputStr, OldWord, NewWord, Delimiters);
end;



{ Returns the first char in the string but checks first if the string is empty (so it won't crash).
  Returns '' if the string is empty.
  Also see: FirstCharIs. }
function FirstChar(CONST s: string): string;
begin
 if Length(s) > 0
 then Result:= s[1]
 else Result:= '';
end;


{ Returns the last char in the string but checks first if the string is empty (so it won't crash).
  Returns '' if the string is empty }
function LastChar(CONST s: string): string;
begin
 if Length(s) > 0
 then Result:= s[Length(s)]
 else Result:= '';
end;


{ Returns True if the first character is aChar }
function FirstCharIs(CONST s: string; c: Char): Boolean;
begin
 Result:= (Length(s)> 0) AND (s[1]= c);
end;


{ Returns True if the last character is aChar }
function LastCharIs(CONST s: string; c: Char): Boolean;
begin
 Result:= (Length(s)> 0) AND (s[Length(s)]= c);
end;


{ Returns the position of the first character that is no a space.  For example: '  Earth' returns 3. }
function FirstNonSpace(CONST s: string): Integer;
begin
 for VAR i:= 1 to Length(s) DO
    if s[i] <> ' '
    then EXIT(i);
 Result:= -1;
end;





{-----------------------------------------------------------------------------
   WORDS
-----------------------------------------------------------------------------}

{ Copy from s all complete words (a word is separate by spaces).
  The result will be truncated to MaxChars characters. }
function CopyWords(CONST s: string; MaxChars: Integer): string;
VAR EndString, i: Integer;
    NextChar: char;
begin
 Assert(MaxChars > 0);
 EndString:= MaxChars;

 if Length(s) > MaxChars then
  begin
   NextChar:= s[MaxChars+1];

   if (s[MaxChars] <> ' ') AND (NextChar <> ' ')
   then
     begin
      for i:= MaxChars downto 1 DO
       if s[i]= ' ' then
        begin
         EndString:= i;
         Break;
        end
     end
   else
    if (s[MaxChars] = ' ')
    OR (s[MaxChars] <> ' ') AND (NextChar = ' ')
    then EndString:= MaxChars;
  end;

 Result:= CopyTo(s, 1, EndString);
 Result:= TrimRight(Result);
end;



{ A 'word' is separated by space, tab and enter. }
function WordCountStrict(CONST s: string): Integer;
VAR
   WordSeparatorSet: Set of AnsiChar;       // We will set on only the above characters
   index  : Integer;                        // Used to scan along the string
   NewWord: Boolean;                        // Indicates whether we are in the middle of a word
begin
 WordSeparatorSet:= [LF, TAB, CR, #32];     // Turn on the TAB, CR, LF and BLANK characters in our word separator set
 Result:= 0;                                // Start with 0 words
 NewWord:= FALSE;                           // Scan the string character by character looking for word separators

 for index:= 1 to Length(s) DO
   if CharInSet(s[index], WordSeparatorSet) // Have we found a separator character?
   then
    begin
     if NewWord
     then Inc(Result);                      // Separator found - have we moved from a word?Yes - we have ended another word
     NewWord:= false;                       // Indicate that we are not in a word anymore
    end
   else
     NewWord:= true;                        // Separator not found - we are in a word

 if NewWord then Inc(Result);               // Finally, were we still in a word at the end of the string? if so, we must add one to the word count since we did not meet a separator
end;



{ A 'word' is separated by space, tab, enter AND other special characters.
  47ms on a 1.8MB TXT file (AMD 4GHz). }
function WordCount(CONST s: string): Integer;   // Old name: CountWords
VAR Ix: Integer;
begin
  Result := 0;
  Ix     := 1;
  WHILE Ix <= Length(s) DO
   begin
     WHILE (Ix <= Length(s)) AND (IsWordSeparator(s[Ix]))
       DO Inc(Ix);

     if Ix <= Length(s) then
      begin
       Inc(Result);
       while (Ix <= Length(s)) AND (NOT IsWordSeparator(s[Ix]))
         DO Inc(Ix);
      end;
   end;
end;


{ Returns true if the specified char is a word separator (space, enter, signs, etc) }   // Old name: InSeparatorList
function IsWordSeparator(CONST aChar: Char): Boolean;
begin
  IsWordSeparator := CharInSet(aChar, [#0..#31, ' ', '.', ',', '?', '=', '<', '>', '!', ':', ';', '(', ')', '/', '\']);
end;





{-----------------------------------------------------------------------------
   COUNT
   Count the number of occurrences of a substring within a string.

  Speed test on:
    10MB file
    AMD 4GHz
-----------------------------------------------------------------------------}

{ Speed:
    7ms    - case sensitive mode
    50ms   - case insensitive mode }
function CountAppearance(CONST Needle, Haystack: string; CaseSensit: Boolean): integer;
VAR
   s2: string;
   Offset: integer;
   Flags: TReplaceFlags;
begin
 { CASE SENSITIVE }
 if CaseSensit
 then
   begin
    Result := 0;
    Offset := PosEx(Needle, Haystack, 1);
    while Offset <> 0 DO
     begin
      inc(result);
      Offset := PosEx(Needle, Haystack, Offset + length(Needle));
     end;
   end
 else
   { CASE INSENSITIVE }                                  { Faster BUT key sensitive. Occurrences of a substring in a string.  }
   if (Length(Needle) = 0) OR (Length(Haystack) = 0)
   then Result := 0
   else
    begin
     Flags:= [rfReplaceAll, rfIgnoreCase];
     s2:= StringReplace(Haystack, Needle, '', Flags);
     Result:= (Length(Haystack) - Length(s2)) div Length(Needle);
    end;
end;


{ 7ms }
function CountAppearance(CONST Niddle: AnsiChar; CONST Haystack: AnsiString): integer;
VAR i: Integer;
begin
 Result:= 0;
 for i := 1 to Length(Haystack) DO
   if Haystack[i] = Niddle
   then inc(Result);
end;


{ Case sensitive
  7ms
  http://delphi.cjcsoft.net//viewthread.php?tid=43892 }
function CountAppearance(CONST Niddle: char; CONST Haystack: string): integer;
VAR p: PChar;
begin
  Result := 0;
  p := PChar(Pointer(Haystack));
  while p <> NIL do
   begin
    p := StrScan(p, Niddle);
    if p <> nil then
     begin
      inc(Result);
      inc(p);
     end;
   end;
end;









function PosInsensitive(CONST Niddle, Haystack: string): Integer;    {TODO: Probably it is slow! Need better alternative}
begin
 Result:= Pos(UpperCase(Niddle), UpperCase(Haystack));
end;


function PosInsensitive(CONST Niddle, Haystack: AnsiString): Integer;
begin
 Result:= Pos(UpperCase(Niddle), UpperCase(Haystack));
end;


{ Returns the position of the last occurence of a substring in String. Also see 'EndsStr' }
function LastPos(CONST Niddle, S: String): Integer;
begin
 Result:= S.LastIndexOf(Niddle)+1;    { +1 because is indexed in zero instead of 1, as normal Delphi strings are }
end;


function LastPos(CONST Niddle: Char; CONST S: String): Integer;  // not tested!
VAR i: Integer;
begin
 Result:= 0;
 for i:= s.Length downto 1 do
  if s[i] = Niddle
  then EXIT(i);
end;



{ Returns true if the specified string appears at least x times }
function PosAtLeast(CONST Niddle, S: string; AtLeast: Integer): Boolean;
VAR Found, Total: Integer;
begin
 Found:= 1;
 Total:= 0;

 REPEAT
   Found:= PosEx(Niddle, s, Found);
   if Found> 0 then
    begin
     Inc(Found);
     Inc(Total);
    end;
 UNTIL (Found= 0) OR (Total> AtLeast);

 Result:= Total> AtLeast;
end;



{ Universal search function }
function Find(CONST Needle, Haystack: string; PartialSearch: Boolean= False; CaseSens: Boolean= False): boolean;
begin
 if CaseSens
 then
    { Case sensitive }
    if PartialSearch
    then Result:= Pos    (Needle, Haystack) > 0
    else Result:= SameStr(Needle, Haystack)
 else
    { Case insensitive }
    if PartialSearch
    then Result:= PosInsensitive(Needle, Haystack) > 0
    else Result:= SameText      (Needle, Haystack);
end;














function DoubleQuoteStr(CONST S: string): string;
begin
  Result := '"' + S + '"';
end;



function Reverse(CONST S : String): String;
VAR i : Integer;
begin
 Result:= '';
 if s= '' then EXIT;
 for i:= Length(S) DownTo 1
  DO Result:= Result+ s[i];
end;





{$IFNDEF UNICODE}
function CharInSet(C: AnsiChar; CONST CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ENDIF}

(* My version. Could be faster for short strings.

function LastPos(SubStr, S: string): Integer;     {...return the position of the last occurence of a substring in String?}
var
  Found, Len, Poz: integer;
begin
  Poz := Length(S);
  Len := Length(SubStr);
  Found := 0;
  while (Poz > 0) AND (Found = 0) DO
   begin
    if Copy(S, Poz, Len) = SubStr
    then Found := Poz;
    Dec(Poz);
   end;
  LastPos:= Found;
end; *)



end.

