UNIT ccCore;

{=============================================================================================================
   CubicDesign
   2022.10.04
   See Copyright.txt

   Over 200 functions for:
     - String manipulation (string conversions, sub-string detection, word manipulation, cut, copy, split, wrap, etc)
     - Programmer's helper
     - Form manipulation
     - Advanced message boxes
     - Easy message boxes
     - DateTime utilities
     - etc

=============================================================================================================}

INTERFACE

USES
   Winapi.Windows, Winapi.Messages, Winapi.MMSystem, System.AnsiStrings, System.Character, System.SysUtils, System.Math,
   System.IOUtils, System.StrUtils, System.Classes, System.Types, System.TimeSpan, System.DateUtils, Generics.Collections,
   Vcl.Themes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Graphics;

{ Enters }
CONST
   {Note: For crossplatform use System.sLineBreak }
   CR               = #13;         { $0D. Used on Mac OS, Apple II family, ZX Spectrum }
   LF               = #10;         { $0A  Used on Unix (Linux, OS X, FreeBSD, AIX, Xenix, etc.), BeOS, Amiga, RISC OS }
   CRLF             = #13#10;      { Used on Windows, OS/2, Symbian OS, Palm OS }
   LBRK             = CRLF+CRLF;

{ Special characters }
CONST
   TAB              = #9;
   ESC              = #27;
   Space            = #32;         { $20 }
   Quote            = #39;
   CopyrightSymbol  = '�';
   GradCelsius      = '�';
   Euro             = #8364;       { Euro Sign: Alt+0128.  Unicode Number: 8364 }

{ Indexes }
CONST
   IndexedIn1       = 1;
   IndexedIn0       = 0;
   IndexDiff        = 1;
   HeaderOverhead   = 1;
   HeaderRow0       = 0;

{ Extra VK constants that are missing from Delphi's Win dows API interface (Windows.pas unit)
  More virtual keys here: http://delphi.about.com/od/objectpascalide/l/blvkc.htm }
CONST
   VK_NULL          = 0;
   VK_PRIOR         = $21;       { PAGE UP }
   VK_NEXT          = $22;       { PAGE DOWN }
   VK_pgUp          = $21;       { PAGE UP }
   VK_pgDwn         = $22;       { PAGE DOWN }
   VK_COPYRIGHT     = 169;       { Type ALT 0169 to get � }
   VK_REGISTERED    = 174;       { Type ALT 0174 to get � }
   VK_SEMICOLON     = 186;
   VK_EQUAL         = 187;
   VK_COMMA         = 188;
   VK_MINUS         = 189;
   VK_PERIOD        = 190;
   VK_SLASH         = 191;
   VK_BACKQUOTE     = 192;
   VK_LEFTBRACKET   = 219;
   VK_BBACKSLASH    = 220;
   VK_RIGHTBRACKET  = 221;
   VK_QUOTE         = 222;
   //VK_ENTER       = VK_RETURN; { #13 }

   Numbers        = ['0'..'9'];
   LettersLowCase = ['a'..'z'];    // LettersSmall
   LettersUpCase  = ['A'..'Z'];    // LettersCapital
   LettersSigns   = [' ', '~', '`', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '-', '_', '+', '=', '[', ']', '{', '}', ';', ':', '''', '"', '<', '>', ',', '.', '/', '?', '\', '|'];
   FullAlfabet    = ['a'..'z', 'A'..'Z'];
   FullAlfabNmbr  = ['a'..'z', 'A'..'Z', '0'..'9'];
   Vowels         = ['a', 'e', 'i','o', 'u', 'y', 'A', 'E', 'I', 'O', 'U', 'Y'];
   LettersSpecial = [#10, #13, #9]; { CR, LF, TAB }

{ Mine }
CONST
   LazyWrite    = TRUE;
   InstantWrite = FALSE;
   ctCanCreate  = TRUE;
   ctCantCreate = FALSE;
   SHOW         = TRUE;
   HIDE         = FALSE;
   MinINT       = Low(Integer);               { -2147483648 }
   OFF          = FALSE;
   ON_          = TRUE;

{ Units }
CONST
   KB        = 1024;
   MB        = 1048576;
   GB        = 1073741824;
   TB: Int64 = 1099511627776;

{ Range limits on int type vars }          { int64 can hold up to 9223372036854775807 }
CONST
   MAXSMALLINT = high(smallint);
   MINSMALLINT = low(smallint);
   MINWORD     = low(word);
   MAXSHORTINT = high(shortint);
   MINSHORTINT = low(shortint);
   MINBYTE     = low(byte);
   MAXLONGWORD = high(longword);
   MINLONGWORD = low(longword);
   MAXSTRING   = MaxInt;

   RegStartUpKey= 'Software\Microsoft\Windows\CurrentVersion\Run';
   UM_ENSURERESTORED = WM_USER+ 65;                                        { For 'Run Single Instance' }
   MSG_LateInitialize= WM_APP + 4711;

{ COLORS (in BGR format) }
CONST
   clBlueBkg     = TColor($15100F);           { To be used as backgound when I display images }
   clBlueAlmost  = Tcolor($F03030);
   clBlueBleo    = Tcolor($FEF800);
   clBlueNight   = Tcolor($3F0000);
   clBlueDark    = TColor($770000);
   clBlueGreen   = TColor($CCCC00);
   clBlueLight   = Tcolor($F6B0B0);
   clBlueLt      = TColor($FFCC99);
   clBlueNaval   = TColor($CC9933);
   clBlueSky     = TColor($F0CAA6);
   clBluePale    = TColor($FFFFCC);
   clBlueSlab    = TColor($B3B67E);           { Faded blue }
   clBlueSea     = TColor($B90F0B);
   clBlueSeaGreen= TColor($CCFF00);           { Bleo but a bit more green in it }
   clPlum        = TColor($B16778);           { Plum blue }
   clPlumLt      = TColor($BD85C7);

   clLimeDark    = TColor($00CF00);
   clGreenOk     = TColor($BBEEBB);           { To be used as background for controls to singnal that data (a path in a teditbox) is ok }
   clGreenDark   = Tcolor($005F00);
   clGreenFade   = TColor($79FF91);
   clKhaki       = TColor($669999);
   clMustard     = TColor($00DDDD);           { Dark yellow }
   clGreenWashed = TColor($CCFFCC);           { A very very fade green }
   clOliveGreen  = TColor($009966);

   clOrange      = TColor($0078FF);
   clOrangeDark  = TColor($0058DF);
   clOrangeDk    = TColor($0099CC);
   clOrangeGray  = TColor($80A8F0);
   clOrangeLt    = TColor($99CCFF);

   clPink        = TColor($F255FF);           { A bit lighter than clPurpleLight }
   clPinkLight   = TColor($F793FF);
   clPurpleDk    = TColor($5E005E);
   clPurpleLight = TColor($AA22CC);           { A bit lighter than Purple }
   clPurpleFaded = TColor($CA62FC);           { This is almost pink }
   clPurpleWashed= TColor($cAa2eC);           { This is almost pink }
   clPumpkin     = TColor($0099FF);
   clButterfly   = TColor($EF10B8);           { Purple with some extra red in it }

   clRedBrick    = TColor($003399);
   clRedBaron    = TColor($0033FF);
   clRedBright   = Tcolor($4040FF);           { A bit brighter than the classic clRed }
   clRedDark     = Tcolor($00003F);
   clRedFade     = Tcolor($D0D0FF);           { Frez }
   clRose        = TColor($5E24F4);           { Intense }
   clRoseLt      = TColor($9966FF);           { Frez frez }
   clBrown       = TColor($6058A0);
   clBrownLt     = TColor($688FB0);
   clBurntSienna = TColor($000088);           { Almost coagulated blood }

   clTealDk      = TColor($999933);
   clViolet      = TColor($FF33FF);
   clVioletDk    = TColor($993399);
   clVioletLt    = TColor($FFCCFF);
   clCyanLt      = TColor($FFFF99);

   clYellowGreen = TColor($00FFCC);
   clYellowLight = TColor($99ffff);
   clYellowPale  = TColor($CCFFFF);
   clHoney       = TColor($1CAEE6);           { Dark yellow with some red in it }

   clCream       = TColor($F0FBFF);
   clPeach       = TColor($647EF9);
   clSilverDark  = TColor($a0a0a0);
   clSilverLight = TColor($F3F2F2);           { Very light silver }
   clGrayMedium  = TColor($A4A0A0);

{ Bitwise constants for TControl.Tag }
CONST
   DontTranslate= 128; { 128 = binary 1000 0000 } {Note: in Delphi11 we can write it directly as a binary literal: %10000000 }


TYPE
  TStringArray       = array of string;
  EnterType          = (etUnknown, etWin, etNix, etMac);
  TConvertNotifyKind = (nkMax, nkProgress);
  TConvertNotify     = procedure(Kind: TConvertNotifyKind; Value: LongInt);
  TNotifyMsgEvent    = procedure(Self: TObject; Msg: string) of object;    { For general use }

TYPE
  TCustomControlEx= class(TCustomControl)    { Interposer: Control that exposes the Canvas property. It can be used to access TPanel's canvas property for example }
   public
      property Canvas;
   end;


{=============================================================================================================
   STRINGS / ENTER
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

 function  IsMacFile           (InStream: TStream): Boolean;                                                      { Returns true if the Enter is format from a single CR character }
 function  GetEnterType        (InStream: TStream): EnterType;
 function  GetEnterTypeS       (InputFile: string): string;

 procedure WinToUnix           (InStream: TStream; OutStream: TStream; Notify: TConvertNotify);   overload;
 procedure UnixToWin           (InStream: TStream; OutStream: TStream; Notify: TConvertNotify);   overload;
 procedure MacToWin            (InStream: TStream; OutStream: TStream);                           overload;

 procedure WinToUnix           (InputFile, OutputFile: String; Notify: TConvertNotify);           overload;
 procedure UnixToWin           (InputFile, OutputFile: String; Notify: TConvertNotify);           overload;
 function  MacToWin            (InputFile, OutputFile: string): Boolean;                          overload;       { CR to CRLF }

{=============================================================================================================
   STRINGS
 ============================================================================================================}

 // REPLACE
 function  ReplaceUnicodeChars (CONST s: string; ReplaceWith: char): String;                                      { Replace all Unicode characters withsomething else }
 function  ReplaceCharF        (CONST s: string; CONST SearchFor, ReplaceWith: char): string;
 procedure ReplaceChar         (var   s: string;  CONST SearchFor, ReplaceWith: Char);         overload;
 procedure ReplaceChar         (var   s: AnsiString; CONST SearchFor, ReplaceWith: AnsiChar);  overload;
 function  ReplaceStringAnsi   (CONST s, SearchFor, ReplaceWith: AnsiString): AnsiString;
 function  ReplaceString       (CONST s, SearchFor, ReplaceWith: string): string;

 // CLEAN STRINGS
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

 // COMPARE
 function  StringFuzzyCompare  (s1, s2: string): Integer;                                                         { The function checks if any identical characters is in the near of the actual compare position }
 function  FileNameNaturalSort (s1, s2: String): Integer;                                                         { Natural compare two filenames }
 function  StrCmpLogicalW      (psz1, psz2: PWideChar): Integer; stdcall; external 'shlwapi.dll';                 { Natural compare two filenames. Digits in the strings are considered as numerical content rather than text. This test is not case-sensitive. Use it like this: StrCmpLogicalW(PChar(s1), PChar(s2));  see: http://stackoverflow.com/questions/1024515/delphi-is-it-necessary-to-convert-string-to-widestring.  }

 // WORDS
 function  IsWordSeparator     (CONST aChar: Char): Boolean;                                                      { Returns true if the specified char is a word separator .;?,! }
 function  CopyWords           (CONST s: string; MaxChars: Integer): string;                                      { Copy from s all complete words. The result will not have more than MaxChars characters. }
 procedure ReplaceShortWords   (var   s: string; MinLength: Integer; FilterIfNoWovels: Boolean);                  { This procedure will replace short words (length < MinLength) with spaces.   It also filters words that only contain consonants }
 function  WordCountStrict     (CONST s: string): Integer;
 function  WordCount           (CONST s: string): Integer;

 // COPY from/to a marker
 function  ExtractTextBetween  (CONST s, TagStart, TagEnd: string): string;                                       { Extract the text between the tags. For example '<H>Title</H>' will return 'Title' is iFrom= '<H>' and iTo= '</H>' }

 function  CopyTo              (CONST s: String; iFrom: Integer; sTo: string; IncludeMarker: Boolean= TRUE; CopyAllMarkerNotFound: Boolean= FALSE; MarkerOffset: Integer= 1): string; overload;
 function  CopyFromTo          (CONST s: string; sFrom, sTo: string;          IncludeMarkers: Boolean= FALSE; Offset: Integer= 1): string;       overload;

 function  CopyFrom            (CONST s, sFrom: string;     Count: Integer; IncludeMarker: Boolean= TRUE; SearchOffset: Integer= 1): string;     overload;  { Find sFrom in s. Returns the string from the postion where the text was found, to the end. }
 function  CopyFrom            (CONST s, sFrom: AnsiString; Count: Integer; IncludeMarker: Boolean= TRUE; SearchOffset: Integer= 1): AnsiString; overload;

 // COPY
 function  CopyTo              (CONST s: string;     iFrom, iTo: integer): string;     overload;                  { Copy the text between iFrom and ending at iTo (including) }
 function  CopyTo              (CONST s: AnsiString; iFrom, iTo: integer): AnsiString; overload;                  { Copy the text between iFrom and ending at iTo (including) }

 function  ExtractTopLines     (CONST Text: string; Count: Integer; IgnoreEmptyLines: Boolean= TRUE): string;     { Returns the top x lines from a text (multiple lines) }

 // CUT
 function  CutInclude2Left     (CONST s, SearchFor: string): string;                                              { Delete all chars from end of MATCH to Left  - including the match }
 function  CutInclude2Right    (CONST s, SearchFor: string): string;                                              { Delete all chars from beg of MATCH to Right - including the match }
 function  CutExcludeLeft      (CONST s, SearchFor: string): string;                                              { Delete all chars from beg of MATCH to Left  - excluding the match }
 function  CutExcludeRight     (CONST s, SearchFor: string): string;                                              { Delete all chars from end of MATCH to Right - excluding the match }

 // SPLIT
 function  SplitText           (CONST Text, Delimiter: string): TStringList;                                      { Splits a text in lines and puts the lines in a TStringList } {Note: Exista System.StrUtils.SplitString } { Old name: SplitStrings }
 procedure SplitString         (CONST Text, Delimiter: string; OUT sField, sValue: string);    overload;          { Split a string in its components. For example 'ClientName=Bubu' will return in 'ClientName' and 'Bubu' }
 procedure SplitString         (CONST Text: string; TSL: TStringList);                         overload;          { Split a string in multiple rows every time the #13#10 char is found (I took this code from Embarcadero's TStringList.Text:= s ) }
 procedure SplitStringList     (StringList: TStrings; OUT OutList1, OutList2: TStringArray);                      { Split each row of the provided StringList into two parts. The two resulted strings are placed in an ArrayOfStrings }
 procedure SplitStringListI    (StringList: TStrings; OUT OutList1: TStringArray; OUT OutList2: System.Types.TIntegerDynArray);   { Split each row of the provided StringList into two parts. The two resulted strings are placed in an ArrayOfStrings }
 procedure SplitStringAtPos    (CONST Text: string; CONST Pos: Integer; OUT s1, s2: string);   overload;          { Split a string in two substrings at the specified position. The char at Pos will be included in the first string. }
 procedure SplitStringAtPos    (CONST Text: AnsiString; CONST Pos: Integer; OUT s1, s2: AnsiString);     overload;

 // GENERATE RAND STRING
 function  GenerateString      (CONST RepeatTimes: Integer; C: char): string; deprecated 'Use System.StringOfChar instead';    { Exista System.StrUtils.DupeString and StuffString                                       Returns the concatenation of a string with itself a specified number of repeats. }
 function  GenerateUniqueString(CONST Len: Integer=32): string;

 function  GenerateRandomWord  (CONST Len: Integer=16; StartWithVowel: Boolean= FALSE): string;
 function  GenerateRandString  (minLen, maxLen: Integer): string;                                                 { This will return all printable craracters (from 65 to 125) }
 function  GenerateRandStringLet (CONST Len: Integer): string;                                                    { This will return ONLY letters and numbers } { YOU MUST call randomize before calling this function! }
 procedure GenerateRandomTextFile(CONST aFilename: string; NoOfLines: Integer);                                   { Creates a file that contains random strings. NoOfLines=10000000 creates a files of about 140MB }

 // OTHERS
 function  InsertCharEvery     (CONST c: char; CONST Target: string; Every: Integer): string;                   { Insert a char into TargetStr every x characters }
 function  DoubleQuoteStr      (CONST s: string): string;
 function  Reverse             (CONST s: String): string; deprecated 'ccCore.Reverse is deprecated. Use System.StrUtils.ReverseString';
 function  GetRandomPersonName: string;   { Returns a random name in a 100 unique name list }
 function  GetRandomStreetName: string;


 // Shorten text and put ellipsis in it
 // ShortenString & GetEllipsisText moved to cmEllipsisText.pas

 // MAKE STRING
 function  MakeStringLongRight (CONST s: AnsiString; c: AnsiChar; CONST ForcedLength: integer): AnsiString;   overload;
 function  MakeStringLongRight (CONST s: string; c: Char; CONST ForcedLength: integer): string;               overload;
 function  MakeStringLongRight (CONST s, Pad: string; CONST ForcedLength: integer): string;                   overload;           { Make sure the string has ForcedLength. If not, add some extra characters at its end to make it that long  }
 function  MakeStringLongLeft  (CONST s, Pad: string; CONST ForcedLength: integer): string;                   { Make sure the string has ForcedLength. If not, add some extra characters at its front to make it that long  }

 function  LeadingZeros        (CONST s: string; CONST ForcedLength: integer): string;                        { insert (ForcedLength-1) zeros in front of the specified string. ForcedLength shows which is the desired lenght of the new string. Example: LeadingZeros('a', 4) will result in '000a'  }
 function  LeadingZeros2       (CONST s: string; CONST ForcedLength: integer): string; { Not tested }
 function  LeadingZerosAuto    (CONST s: string; CONST MaxValue: integer): string;                            { Same as above except_ that the user doesn't have to specify how many zeros to add. Instead the function will determine this automaticxally based on the number received as parameter. For example LeadingZeros('1', 50) will generate '01' but LeadingZeros('1', 500) will generate '001' }

 // WRAP
 //See cmWrapString.pas

 // TStringList
 function  String2TSL          (s: string): TStringList;                                                      { Converts a string to a TStringList. In other words it breaks the text to multiple lines. I need to call Free after this! }

 // PCHAR
 function  UnicodeToAnsi       (CONST str: UnicodeString; codePage: Integer): RawByteString;                  { netestat }
 function  AddNullToStr        (CONST Path: string): string;

 // RAM
 function  GetStringSize       (CONST s: string): Integer;                                                    { Returns the length of a given string in bytes }
 function  GetStringRAMSize    (CONST s: string): Integer;          overload;
 function  GetStringRAMSize    (CONST s: AnsiString): Integer;      overload;


{============================================================================================================
   STRINGS: POS
 ============================================================================================================}
 function  Find                (CONST Needle, Haystack: string; PartialSearch: Boolean= False; CaseSens: Boolean= False): boolean;
 function  FindLine            (CONST Needle, Haystack: string): string;                                      { Looks for Needle (partial search) into the MultipleLines. When needle it found then it returns the whole line that contained the Needle. MultipleLines is a string what contains multiple lines of text separated by enter. } // Old name: ExtractLine. THE PARAMETER ORDER WAS CHANGED

 function  CountAppearance     (CONST Needle, Haystack: string; CaseSensit: Boolean): integer;     overload;
 function  CountAppearance     (CONST Niddle: Char;     CONST Haystack: string)    : Integer;      overload;
 function  CountAppearance     (CONST Niddle: AnsiChar; CONST Haystack: AnsiString): Integer;      overload;

 function  LastPos             (CONST Niddle, S: string): Integer;                                 overload;  { Return the position of the last occurence of a substring in String. Not tested. Also see 'EndsStr' }
 function  LastPos             (CONST Niddle: Char; CONST S: String): Integer;                          overload;

 function  PosAtLeast          (CONST Niddle, S: string; AtLeast: Integer): Boolean;                          { Returns true if the specified string appears at least x times }
 function  PosInsensitive      (CONST Niddle, Haystack: string): Integer;                          overload;
 function  PosInsensitive      (CONST Niddle, Haystack: AnsiString): Integer;                      overload;

 function  LastChar            (CONST s: string): string;                                                     { Returns the last char in the string but checks first if the string is empty (so it won't crash). Returns '' if the string is empty }
 function  FirstChar           (CONST s: string): string;                                                     { Returns the first char in the string but checks first if the string is empty (so it won't crash). Returns '' if the string is empty }
 function  FirstCharIs         (CONST s: string; c: Char): Boolean;
 function  LastCharIs          (CONST s: string; c: Char): Boolean;


{============================================================================================================
   STRINGS: CONVERSION TO NUMBERS
============================================================================================================}
 function  i2s                 (CONST Value: Integer):           string; overload;  inline;
 function  i2s                 (CONST Value, MaxValue: integer): string; overload;                            { Add the specified number of zeros before the string. See LeadingZerosAuto help for details }
 function  i2s                 (CONST Value: Int64)  :           string; overload;                            { int64 can hold up to 9223372036854775807 }
 function  i2sHuman            (CONST Value: Int64)  :           string;                                      { Retunrs something like: 1= 1st, 2= 2nd, 3= 3rd, 4= 4th }
 function  Real2Str            (CONST ExtValue: Extended; Decimals: Byte = 1; HideNulMantisa: Boolean= True): string;
 function  Str2Cardinal        (CONST S: string; Default: Integer): Cardinal;
 function  Rectangle2Str       (CONST Rect: TRect): string;
 function  FormatBytes         (CONST Size: Int64; CONST Decimals: Integer= 1): string;                       { Format bytes to KB, MB, GB, TB }
 function  FormatBytesMB       (CONST Size: Int64; CONST Decimals: Integer= 1): string;                       { Same as above but the function will never return values formated in GB range. More exactly instead of 10GB it will return 10240MB }
 function  FormatNumber        (CONST Size: Int64; CONST Decimals: Integer= 1): string;                       { It will return 1K for 1000, 1M for 1000000 and so on }
 function  BoolToStrYesNo      (CONST B: Boolean): string;


{============================================================================================================
   STRINGS: NUMBERS
============================================================================================================}
 function  FixNumber           (      s: string): Integer;                                                    { Converts a text that contains an invalid number to a valid number. For example  '!2345' will return '2345' }
 function  StringIsInteger     (CONST s: string): Boolean;                                                    { Varianta 2 }
 function  StringIsInteger2    (CONST s: string): Boolean;                                                    { Varianta 3 }
 function  CharIsNumber        (CONST c: char)  : Boolean;
 procedure SplitNumber_Start   (CONST s: string; OUT Text, Number: string);                                   { Splits a string that STARTS with a number into its parts. Example: 01_Render   ->  01 + _Render   }
 procedure SplitNumber_End     (CONST s: string; OUT Text, Number: string);                                   { Splits a string that ENDS     in a number into its parts. Example: Document12  ->  Document + 12                                    }
 function  IncrementStringNo   (CONST s: string): string;                                                     { Receive a number as string. return the same number but incremented with 1. automatically adjust the leading zeros }
 function  IncrementStringNoEx (CONST s: string): string;                                                     { Similar with IncrementStringNo but this version also accepts invalid numbers. If the input string doesn't end with a valid number, append 0 at its end. Then extracts the end number and increase it. Example: 0zzz will return 0zzz0, while xxx33 will retun xxx34 }
 function  LastLetterInString  (CONST s: string): Integer;                                                    { Returns the postion of the last non-number character in a string. For example 9d9ad8f7ax0000 returns 10 (the position of x) }
 function  StringSumm          (CONST s: AnsiString): Cardinal;   overload;
 function  StringSumm          (CONST s: String): Cardinal;       overload;                                   { Compute the summ of all characters in the string }






{=============================================================================================================
  DEVELOP UTILS
=============================================================================================================}

 procedure EmptyDummy;             overload;
 procedure EmptyDummy(i: Integer); overload;
 procedure NotImplemented;

 { SysUtils }
 procedure DisposeAndNil(VAR P: Pointer);
 procedure FillZeros(VAR IntArray: TIntegerDynArray);

 procedure DelayEx(CONST ms : cardinal);
 procedure CursorBusy;
 procedure CursorNotBusy;

 function  GetResourceAsString(CONST ResName: string): AnsiString;    { Extract a resource from self (exe) }
 procedure RefreshNow(Ctrl: TControl);






{=============================================================================================================
  MESSAGE BOXES
=============================================================================================================}
 function  MesajGeneric   (CONST MessageText: string; Title: string= ''; Icon: Integer = MB_ICONINFORMATION or MB_OK): integer;
 procedure Mesaj          (CONST MessageText: string);
 procedure MesajInfo      (CONST MessageText: string; CONST Title: string= '');
 procedure MesajWarning   (CONST MessageText: string);
 procedure MesajError     (CONST MessageText: string);
 procedure MesajErrDetail (CONST MessageText, Where: string);
 function  MesajYesNo     (CONST MessageText: string; CONST Title: string= ''): Boolean;    { Returns True if the user presses the YES btn }

 procedure MesajTaskDLG   (CONST MessageText, Title: string);



 
{=============================================================================================================
   FORM POSITION
=============================================================================================================}
 Procedure CorrectFormPositionScreen (Form: TForm);                                                { Make sure that the child window is not outside the Screen }
 Procedure CorrectMDIFormPosition    (ParentForm: TForm);
 Procedure CorrectFormPositionMonitor(Form: TForm; Monitor: TMonitor);  deprecated 'Use Form.MakeFullyVisible(Monitor)';

 Procedure CorrectCtrlPosition       (aChild, aParent: TControl);                                     overload;
 Procedure CorrectCtrlPosition       (aChild: TControl; CONST aParentWidth, aParentHeight: Integer);  overload;
 procedure CenterInvalidChild        (aChild, Parent: TControl);                                   { Center Chiald in Parent window but only if Child has 'bad' coordinates }
 procedure CenterChild               (aChild, Parent: TControl);                                   { Center Chiald in Parent window }
 procedure CenterChildX              (aChild, Parent: TControl);
 procedure CenterForm                (Form: TForm);                                                { You can also use Form.Position:= poMainFormCenter or poScreenCenter' }



{=============================================================================================================
   VCL Controls, Menus & Actions
   Moved to: cmVclUtils.pas
=============================================================================================================}



{=============================================================================================================
   TIME
=============================================================================================================}
TYPE
    { Returns the next day in the week }
    TWeekDays = (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);   // Days enumeration
    TWeekDaysHelper = record helper for TWeekDays
       function NextDay: TWeekDays;
       function ToString: string;
    end;

CONST
   NullDate     = -700000;                    { https://stackoverflow.com/questions/14985037/delphi-how-to-determine-and-empty-tdatetime-value }
   Second       = 1000;                       { Miliseconds per sec. Already exists: MSecsPerSec }
   Minute       = 60;                         { Miliseconds per min. Already exists System.SysUtils.SecsPerMin }
   Hour         = 3600;                       { Miliseconds per hour }
   Day          = 86400;
   MSecsPerMin  = 60000;
   MinutesPerDay= 24*60;


 // Current time
 function TodayIs: string;                                         { Returns today as date based on Locale. Example: Montag }
 function CurrentDateToString(ShowSeconds: Boolean): string;       { Returns today as date & time. Example: 31.12.2021 - 16:50 }
 function CurrentTimeToString(ShowSeconds: Boolean): string;       { Returns time in short format (no seconds). Example: 16:50 }
 function TimeToString(CONST T: TDateTime; ShowSeconds: Boolean): string;

 function CurrentYear: Word;
 function CurrentMonth: Word;
 function CurrentDay: Word;
 function CurrentHour: Word;

 // DECODE
 function DecodeHour             (Time: TTime): Word;
 function DecodeMinute           (Time: TTime): Word;
 function DecodeSecond           (Time: TTime): Word;

 // COMPARISON
 function SameDateEx             (Time1, Time2: TDateTime): boolean;  deprecated 'Use System.DateUtils.SameDate instead';
 function EarlierThan            (Date1, Date2: TDateTime): boolean;                           { Returns true if Date1 is smaller (earlier) than Date2 }
 function DaysBetweenEx          (CONST MilestoneDate, CurrentDate: TDateTime): Double; deprecated 'Use System.DateUtils.DaySpan instead';
 function NewDaysBetween         (CONST MilestoneDate, CurrentDate: TdateTime): Double;
 function SecondsBetweenEx       (CONST MilestoneDate, CurrentDate: TDateTime): Int64;         { Returns the number of seconds between two specified TDateTime values.  The difference between this function and the Embarcadero one is that it returns zero if CurrentDate >= MilestoneDate }
 function DateIsToday            (Year, Month, Day: word): Boolean;                            { Returns true if the specified date is today }

 // STRING CONVERSIONS
 function  StringToSeconds       (CONST s: String): Integer;                                   { Converts a string formated like 'hh:mm:ss' to seconds.  }
 function  StringIsDate          (CONST s: string): Boolean;
 function  StringIsTime          (CONST s: string): Boolean;
 function  DateToStrUS           (CONST Time: TDateTime): string;                              { converts date to string, using the US (YYY.MM.DD) format }

 // CONVERSIONS
 procedure SecondsToTime         (CONST Seconds : Cardinal; VAR D, H, M, S: Cardinal);
 function  SecondsToTimeAuto     (CONST Seconds : Cardinal): string;
 function  MiliSecToTimeAuto     (CONST MSeconds: Cardinal): string;
 function  mSecondsToTime        (mSeconds: Cardinal): string;

 function  Date2FormatAuto       (CONST Time: TDateTime): string;
 function  DateTimeToMilliseconds(CONST ADateTime: TDateTime): Int64;
 function  DateToTime_           (CONST aDate : TDateTime): string;

 // CONVERSIONS
 function  Date2Cardinal         (xDate: TDate): Cardinal;
 function  Cardinal2Date         (xCardinal: Cardinal): TDate;





{============================================================================================================
   SOUNDS
============================================================================================================}
 procedure PlayWinSound (SystemSoundName: string);
 procedure PlaySoundFile(FileName: string);
 procedure PlayResSound (RESName: String; uFlags: Integer);
 procedure Bip(Frecv, Timp: integer);
 procedure BipConfirmation;
 procedure BipConfirmationShort;
 procedure BipError;
 procedure BipErrorShort;
 procedure Bip30;
 procedure Bip50;
 procedure Bip100;
 procedure Bip300;
 procedure BipCoconuts;
 procedure MakeSound(Frequency, Duration: Integer; Volume: Byte);   { writes tone to memory and plays it}




IMPLEMENTATION

USES
  ccStreamBuff;








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


procedure FillZeros(VAR IntArray: TIntegerDynArray);
begin
 FillChar(IntArray, Length(IntArray)*SizeOf(Integer), 0);
end;


procedure RefreshNow(Ctrl: TControl);
begin
  Ctrl.Refresh;
end;


procedure DelayEx(CONST ms : Cardinal);                                                            { Non-blocking sleep/delay.  Applica tion.ProcessMessages could create a lot of problems!!! }
VAR Count: Cardinal;
begin
 Count:= GetTickCount;  // GetTickCount accuracy is > 15ms. Use cDebug TimerStart instead.     https://blogs.msdn.microsoft.com/oldnewthing/20050902-00/?p=34333
 REPEAT
  Sleep(5); { without this I have 100% CPU utilization because the loop is too tight }
  Application.ProcessMessages;     // https://blog.dummzeuch.de/2018/09/29/calling-application-processmessages-in-a-delphi-program/
 UNTIL (GetTickCount-Count)>= ms;
end;


procedure EmptyDummy;
begin
 //Do nothing
end;


procedure EmptyDummy(i: Integer);
begin
 mesaj(IntToStr(i));
end;


procedure NotImplemented;
begin
 RAISE Exception.Create('Not implemented yet.');
end;



















{=============================================================================================================
   FIX FORM POSITION
=============================================================================================================}

{ Brings the form back into the screen, IF it was outside the screen.
  Usage:  CorrectFormScreenPosition(Self).
  It is automaticalled by cvIniFile.LoadForm.

  Screen.WorkArea -> Specifies the work area on the Primary monitor.
  DesktopWidth    -> Determines the width of the desktop. The desktop is defined as the entire virtual desktop, which includes all monitors in the system. On a single-monitor system, DesktopWidth corresponds to Width. }
Procedure CorrectFormPositionScreen(Form: TForm);                                                             { Old name: RepairPositionOnScreen }
begin
 CorrectCtrlPosition(Form, Screen.DesktopWidth, Screen.DesktopHeight);
end;


{ Make sure that the specified form fits into the specified monitor. }
Procedure CorrectFormPositionMonitor(Form: TForm; Monitor: TMonitor);
begin
 Form.MakeFullyVisible(Monitor);
end;


{ Center form in screen }
procedure CenterForm(Form: TForm);
VAR
   XPos, YPos : Integer;
Begin
  Xpos := (Screen.Width - Form.Width)  DIV 2;
  YPos := (Screen.Height- Form.Height) DIV 2;
  Form.Left:= Xpos;
  Form.Top := YPos;
End;




{ Centers the child into the parent, IF the child was outside the screen.

  Note! The Top is relative to parent's client area.
  In other words, if the Parent has a toolbar/panel (height = 200) aligned to its top and the Child form is
  imediatelly under that panel, Child's top will be 0, not 500!
  Usage: CorrectFormScreenPosition(Self, ParentForm)  }
Procedure CorrectCtrlPosition(aChild, aParent: TControl);
begin
 CorrectCtrlPosition(aChild, aParent.ClientWidth, aParent.ClientHeight);
end;


Procedure CorrectCtrlPosition(aChild: TControl; CONST aParentWidth, aParentHeight: Integer);  {TODO 2: this won't be aligned correctly when it is under the parent bottom and the parent height is big (over 1000 pixels) }
begin
 if aChild.Top < 0
 then aChild.Top := 0;               { It was too high, show it imediatelly under the top }
 if aChild.Top  > aParentHeight-10
 then aChild.Top := aParentHeight- (aChild.Height DIV 2);  { It was too low, put_its top in the middle of the parent (to be clear, we don't center the whole child into the parent; we only put child's top in the midle of the parent) }

 if aChild.Left < 0
 then aChild.Left:= 0;
 if aChild.Left > aParentWidth-10
 then aChild.Left:= aParentWidth- (aChild.Width DIV 2);

 if aChild.Height > aParentHeight
 then aChild.Height:= aParentHeight;
 if aChild.Width  > aParentWidth
 then aChild.Width:= aParentWidth;
end;


{ Center Chiald in Parent window but only if Child has 'bad' coordinates }
procedure CenterInvalidChild(aChild, Parent: TControl);
VAR iTop: Integer;
begin
 if (aChild.Top < -10)
 OR (aChild.Left< -10)
 OR (aChild.Left> Parent.Width) then   { But only if goes out of screen so the user can't find it }
  begin
   aChild.Left:= (Parent.ClientWidth - aChild.Width)  div 2;
   iTop:= Parent.ClientHeight- aChild.Height;
   //if Parent.ToolBar.Visible then iTop:= iTop- Parent.ToolBar.Height;
   aChild.Top:= iTop div 2;
  end;
end;


{ Center Child in Parent window }
{ToDo: We should take into consideration the controls that are aligned (alleft, alright) }
procedure CenterChild(aChild, Parent: TControl);
VAR Left, Top: Integer;
begin
 Left:= (Parent.ClientWidth - aChild.Width) div 2;

 if Left < 0                                { Happens when the child is bigger than the parent }
 then Left:= Parent.Left - Left;
 aChild.Left:= Left;


 Top:= (Parent.ClientHeight- aChild.Height) div 2;

 if Top < 0                                 { Happens when the child is bigger than the parent }
 then Top:= Parent.Top - Top;
 aChild.Top:= Top;
end;


{ Center Child in Parent window, bot only on the X axis }
procedure CenterChildX(aChild, Parent: TControl);
begin
 aChild.Left:= (Parent.ClientWidth - aChild.Width) div 2;  {todo: We should take into consideration the controls that are aligned (alleft, alright) }
end;


{ Enumerate through all children form of ParentFrom and ensure their position/size }
Procedure CorrectMDIFormPosition(ParentForm: TForm);
VAR i: Integer;
begin
  for I := ParentForm.MDIChildCount-1 downto 0 do
    begin
     { Vertical }
     if ParentForm.MDIChildren[I].Top < 0
     then ParentForm.MDIChildren[I].Top := 0;
     if ParentForm.MDIChildren[I].Height > ParentForm.ClientHeight
     then ParentForm.MDIChildren[I].Height:= ParentForm.ClientHeight-10;

     { Horizontal }
     if ParentForm.MDIChildren[I].Left < 0
     then ParentForm.MDIChildren[I].Left:= 0;
     if ParentForm.MDIChildren[I].Width > ParentForm.ClientWidth
     then ParentForm.MDIChildren[I].Width := ParentForm.ClientWidth;
    end;
end;














{============================================================================================================
   APP UTILS
============================================================================================================}

{ Extract a resource from self }
function GetResourceAsString(CONST ResName: string): AnsiString;
VAR
   ResStream: TResourceStream;
begin
  ResStream := TResourceStream.Create(HInstance, ResName, RT_RCDATA);
  TRY
    ResStream.Position := 0;
    SetLength(Result, ResStream.Size);
    ResStream.ReadBuffer(Result[1], ResStream.Size);
  FINALLY
    FreeAndNil(ResStream);
  END;
end;



procedure CursorBusy;
begin
 Screen.Cursor:= crHourGlass;
end;


procedure CursorNotBusy;
begin
 Screen.Cursor:= crDefault;
end;




















{============================================================================================================
   TIME
============================================================================================================}
function mSecondsToTime(mSeconds: Cardinal): string;                                                { folosit in WindowsUpTime }
begin
  Result:= SecondsToTimeAuto(mSeconds DIV 1000);
end;



function DateToTime_(CONST aDate: TDateTime): string;
begin
 Result:= FormatDateTime('d:hh:nn:ss', aDate);
end;



procedure SecondsToTime (CONST Seconds: Cardinal; VAR D, H, M, S: Cardinal);
begin
 D := (Seconds DIV SecsPerDay);
 H := (Seconds DIV SecsPerHour) - (D* 24);
 M := (Seconds DIV SecsPerMin)  - (D* 24* SecsPerMin) - (H* 60);
 S :=  Seconds                  - (D* SecsPerday)     - (H* 3600)- (M*60);
end;


{ check if I call this in my projects. if not, delete it!
function SecondsToTime_FormatLong (CONST Seconds: Cardinal): string;     // in TDateTime, o secunda e egal cu 1.1574074074e-05        1sec:= frac(real(StrToTime('00:00:01')))
VAR D, H, M, S: Cardinal;
begin
 Result:= '';
 SecondsToTime(Seconds, D, H, M, S);

 if D > 0
 then Result:= IntToStr(D) + ' days ';

 Result:= Result + IntToStr(H) + 'h ' + IntToStr(M) + 'm ' + IntToStr(S)+ 's';
end;}


{ Converts seconds to time, but showing the shortest string possible. For example: it will convert 59 to '59s' and 61 to '1m 01s' }
function SecondsToTimeAuto(CONST Seconds: Cardinal): string; // Old name: SecondsToTime_FormatAuto
VAR D, H, M, S: Cardinal;
begin
 Result:= '';

 D := (Seconds DIV SecsPerDay);
 H := (Seconds DIV SecsPerHour) - (D* 24);

 { Add days }
 if D> 0 then Result:= IntToStr(D) + ' days ';

 { Add hours }
 if H> 0 then Result:= Result+ IntToStr(H) + 'h ';

 { Add minutes but ONLY if I have less than 30 days }
 if D<= 7 then
  begin
   M := (Seconds DIV SecsPerMin)  - (D* 24* SecsPerMin) - (H* 60);
   Result:= Result+ IntToStr(M) + 'm ';

   { Add seconds ONLY if I don't add days (keep text short) }
   if (D= 0) then
    begin
      S := Seconds- (D* SecsPerday) - (H* 3600)- (M* 60);
      Result:= Result {+ ' ' del } + IntToStr(S)+ 's';
    end;
  end;
end;



function MiliSecToTimeAuto(CONST MSeconds: Cardinal): string;     //old name: mSecondsToTime
begin
 if MSeconds< 1000                                                                                 { under 1 sec }
 then Result:= IntToStr(MSeconds)+ 'ms'
 else
   if MSeconds< 60000                                                                              { under 1 minute }
   then Result:= Real2Str(MSeconds / 1000, 3)+ 'sec'
   else Result:= SecondsToTimeAuto(MSeconds DIV 1000);
end;



function Date2FormatAuto(CONST Time: TDateTime): string;
VAR D: Integer;
    H, M, S: Word;
begin
 Result:= '';

 D := Trunc(Time);
 H := HourOf(Time);
 M := MinuteOf(Time);
 S := SecondOf(Time);

 { Add days }
 if D> 0 then Result:= IntToStr(D) + ' days ';

 { Add hours }
 if H> 0 then Result:= Result+ IntToStr(H) + 'h:';

 { Always add minutes }
 Result:= Result+ IntToStr(M) + 'm';

 { Add seconds ONLY if I don't add days (keep text short) }
 if D= 0 then Result:= Result + ' :' + IntToStr(S)+ 's';
end;


function DateToStrUS(CONST Time: TDateTime): string; { converts date to string, using the US (YYY.MM.DD) format }
VAR aYear, aMonth, aDay : Word;
begin
  DecodeDate(Now, aYear, aMonth, aDay);

  Result:= IntToStr(aYear)+ '.';
  if aMonth < 10
  then Result:= Result+ '0'+IntToStr(aMonth)+ '.'
  else Result:= Result+ IntToStr(aMonth)+ '.';

  if aDay < 10
  then Result:= Result+ '0'+IntToStr(aDay)
  else Result:= Result+ IntToStr(aDay);
end;




function SameDateEx(Time1, Time2: TDateTime): boolean;     { We cannot compare two TDateTimes in Delphi because of Real precision. CompareDateTime also won't work. See: http://stackoverflow.com/questions/38705011/odd-behavior-when-comparing-two-tdatetime-vars }
VAR
   Year1, Month1, Day1, Hour1, Min1, Sec1, Msec1: Word;
   Year2, Month2, Day2, Hour2, Min2, Sec2, Msec2: Word;
begin
 DecodeDateTime(Time1, Year1, Month1, Day1, Hour1, Min1, Sec1, Msec1);
 DecodeDateTime(Time2, Year2, Month2, Day2, Hour2, Min2, Sec2, Msec2);
 Result:= (Year1 = Year2) AND (Month1 = Month2) AND (Day1 = Day2) AND (Hour1 = Hour2) AND (Min1 = Min2) AND (Sec1 = Sec2) AND (Msec1 = Msec2);
end;


function EarlierThan(Date1, Date2: TDateTime): boolean;      { Returns true if Date1 is smaller (earlier) than Date2 }
begin
 Result:= System.DateUtils.CompareDateTime(Date1, Date2) = -1;
end;





function DecodeHour(Time: TTime): Word;
VAR wMin, wSec, wMsec: word;
begin
 DecodeTime(Time, Result, wMin, wSec, wMsec);
end;


function DecodeMinute(Time: TTime): Word;
VAR wHor, wSec, wMsec: word;
begin
 DecodeTime(Time, wHor, Result, wSec, wMsec);
end;


function DecodeSecond(Time: TTime): Word;
VAR wHor, wMin, wMsec: word;
begin
 DecodeTime(Time, wHor, wMin, Result, wMsec);
end;



{ Converts a string formatted like 'hh:mm:ss' or 'mm:ss' to seconds.
  Returns -1 is the string does not contain a valid time.

    StringToSeconds('00:01:30')     // returns 90     (sec)
    StringToSeconds('01:30')        // returns 5400   (sec)
    StringToSeconds('10')           // returns 864000 (sec)
    StringToSeconds('1.30')         // returns -1
    StringToSeconds('x')            // returns -1 }
function StringToSeconds(CONST s: String): integer;
VAR
  TimeSpan: TTimeSpan;
begin
  TRY
   TimeSpan:= System.TimeSpan.TTimeSpan.Parse(s);
   Result  := Round(TimeSpan.TotalSeconds);
  EXCEPT
   Result:= -1;
  end;
end;


{...check if a string is a valid date or time?}
function StringIsDate(CONST s: string): Boolean;
begin
  Result:= True;
  TRY
    StrToDate(s);
  EXCEPT
    Result:= False;
  END;
end;

{...check if a string is a valid date or time?}
function StringIsTime(CONST s: string): Boolean;
begin
  Result:= True;
  TRY
    StrToTime(s);
  EXCEPT
    Result:= False;
  end;
end;







function CurrentYear: Word;
VAR aMonth, aDay : Word;
begin
  DecodeDate(Now, Result, aMonth, aDay);
end;


function CurrentMonth: Word;
VAR aYear, aDay : Word;
begin
  DecodeDate(Now, aYear, Result, aDay);
end;


function CurrentDay: Word;
VAR aMonth, aYear : Word;
begin
  DecodeDate(Now, aYear, aMonth, Result);
end;


function CurrentHour: Word;
VAR aMin, aSec, mSec: Word;
begin
  DecodeTime(Now, Result, aMin, aSec, mSec);
end;


{ Returns today as date (string) based on Locale. Example: Montag }
function TodayIs: string;
VAR
 pFormatModel, pResult: PChar;
begin
 Getmem(pResult, 20);
 pFormatModel:= 'dddddddddd';   { ddd => only the day is shown }
 GetDateFormat(LOCALE_SYSTEM_DEFAULT, 0, NIL, pFormatModel, pResult, 20);
 result:= pResult;
 FreeMemory(pResult);
end;


{ Returns today as date AND time. Example: 31.12.2021 - 16:50 }
function CurrentDateToString(ShowSeconds: Boolean): string;
VAR
  Present: TDateTime;
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  Present:= Now;
  DecodeDate(Present, Year, Month, Day);
  DecodeTime(Present, Hour, Min, Sec, MSec);
  Result:= IntToStr(Day)+'.'+IntToStr(Month)+'.'+IntToStr(Year)+' - '+ TimeToString(Present, FALSE);
end;


{ Returns time in short format (no seconds). Example: 16:50 }
function CurrentTimeToString(ShowSeconds: Boolean): string;
begin
  Result:= TimeToString(Now, ShowSeconds);
end;


{ Returns time in short format. Example: 16:50 }
function TimeToString(CONST T: TDateTime; ShowSeconds: Boolean): string;
VAR
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(T, Hour, Min, Sec, MSec);

  if Hour < 10
  then Result:= '0'+ IntToStr(Hour)
  else Result:= IntToStr(Hour);

  if Min < 10
  then Result:= Result+ ':'+ '0'+IntToStr(Min)
  else Result:= Result+ ':'+ IntToStr(Min);

  if ShowSeconds then
    if Sec < 10
    then Result:= Result+ ':'+ '0'+IntToStr(Sec)
    else Result:= Result+ ':'+ IntToStr(Sec);
end;




function DateIsToday(Year, Month, Day: word): Boolean;   { Returns true if the specified date is today }
VAR NewDate: TDateTime;
begin
 NewDate:= EncodeDate(Year, Month, Day);
 Result:= IsToday(NewDate);
end;


function Date2Cardinal(xDate: TDate): Cardinal;
begin
 Result := DaysBetween(xDate, 0);                        { Calculate how many days are between xDate and 01/01/1899 }
end;


function Cardinal2Date(xCardinal: Cardinal): TDate;      { Date returned is in US format }
begin
 Result := IncDay(0, xCardinal);
end;





{ Returns the next day in the week }
function TWeekDaysHelper.NextDay: TWeekDays;
var
    Next: Integer;
begin
    Next := Ord(Self) + 1;

    if Next <= Ord(High(TWeekDays))
    then Result := TWeekDays(Next)
    else Result := TWeekDays.Monday;
end;

function TWeekDaysHelper.ToString: string;
begin
  case Self of
     Monday    : Result := 'Monday';
     Tuesday   : Result := 'Tuesday';
     Wednesday : Result := 'Wednesday';
     Thursday  : Result := 'Thursday';
     Friday    : Result := 'Friday';
     Saturday  : Result := 'Saturday';
     Sunday    : Result := 'Sunday';
  end;
end;





{ Copied from DateUtils.PAS where it is marked as 'Internal'.
  These functions in DateUtils.pas are not accurate. See this of details: http://stackoverflow.com/questions/17109814/why-datetimetomilliseconds-in-dateutils-pas-is-marked-as-internal }
function DateTimeToMilliseconds(CONST ADateTime: TDateTime): Int64;
var
  LTimeStamp: TTimeStamp;
begin
  LTimeStamp := DateTimeToTimeStamp(ADateTime);
  Result := LTimeStamp.Date;
  Result := (Result * MSecsPerDay) + LTimeStamp.Time;
end;


{ Returns the number of seconds between two specified TDateTime values.
  The difference between this function and Embarcadero.DateUtils.SecondsBetween is that it returns zero if CurrentDate >= MilestoneDate.
  These functions in DateUtils.pas are not accurate. See this of details: http://stackoverflow.com/questions/17109814/why-datetimetomilliseconds-in-dateutils-pas-is-marked-as-internal }
function SecondsBetweenEx(CONST MilestoneDate, CurrentDate: TDateTime): Int64;
begin
  if CurrentDate < MilestoneDate
  then Result := (DateTimeToMilliseconds(MilestoneDate) - DateTimeToMilliseconds(CurrentDate)) DIV (MSecsPerSec)
  else Result := 0;
end;



{ Comparing '31 Dec 1999 23:59' and '1 Jan 2000 00:01' (2 minutes difference):
     DaySpan        returns 0.03  (or something like this)
     DaysBetween    returns 0
     NewDaysBetween retunrs 1,03 }
function DaysBetweenEx(CONST MilestoneDate, CurrentDate: TdateTime): Double;
begin
 RAISE exception.Create('DaysBetweenEx is deprecated ');
end;


function NewDaysBetween(CONST MilestoneDate, CurrentDate: TdateTime): Double;
begin
  if SameDate(MilestoneDate, CurrentDate)
  then Result := DaySpan(MilestoneDate, CurrentDate)
  else Result := DaySpan(MilestoneDate, CurrentDate) + 1;
end;

{ Exaclty the same as DaysBetweenEx but uses a different approach to calculate it
function DaySpanEx(CONST MilestoneDate, CurrentDate: TDateTime): Double;
begin
  Result := Abs(DateTimeToMilliseconds(MilestoneDate) - DateTimeToMilliseconds(CurrentDate)) / MSecsPerDay;
end; }













{============================================================================================================
   MESSAGES
============================================================================================================}

{ Displays a message box with an approriate icon (info, warning, error, ask).
  Cross-platform.

  IMPORTANT:
    Application.MessageBox uses the handle of the current active window.
    This can of course create problems if the current window (is not the main form and) is closed.
    The message box will also be closed. So, don't use it in a from created by a thread.

  Blocking
    The MessageBox function is not blocking per se, it merely creates a dialog box with its own message loop.
    To test, show the current time in a TTimer. It will still paint the time, even if a MessageBox is shown.
    However, a MessageBox is still synchronous so it will block a 'for' loop.
    In this case use FromAsyncMessage if you want a non-blocking message box.

  See:
    https://stackoverflow.com/questions/60241794/why-tapplication-messagebox-closes-automatically?noredirect=1#comment106557585_60241794
    https://stackoverflow.com/questions/15696885/why-does-a-messagebox-not-block-the-application-on-a-synchronized-thread  // PRINTED!
    https://stackoverflow.com/questions/1256963/if-messagebox-related-are-synchronous-why-doesnt-my-message-loop-freeze
    http://www.delphigroups.info/2/11/544013.html
    https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-messagebox?redirectedfrom=MSDN
 }
function MesajGeneric(CONST MessageText: string; Title: string= ''; Icon: Integer= MB_ICONINFORMATION or MB_OK): integer;  { 'Title' will appear in window's caption }
{.$WARN SYMBOL_PLATFORM OFF}
{$IFDEF msWindows}
 begin
  if MessageText= '' then EXIT(0);
  if Title= '' then Title:= Application.Title;
  Result:= Application.MessageBox(PCHAR(CRLFToEnter(MessageText)), PChar(Title), Icon); //icon =  MB_ICONINFORMATION or MB_OK
 end;
{$ELSE}
  MessageDlg(MessageText, mtInformation, [mbOk], 0);    //todo: make also this based on icon
{$ENDIF}
{.$WARN SYMBOL_PLATFORM ON}



procedure Mesaj(CONST MessageText: string);   { 'Title' will appear in window's caption } //todo: get rid of if. use MesajInfo instead
 begin
  MesajInfo(MessageText, '');
 end;



procedure MesajInfo(CONST MessageText: string; CONST Title: string= '');
begin
 MesajGeneric(MessageText, Title, MB_ICONINFORMATION or MB_OK);
end;



procedure MesajWarning(CONST MessageText: string);
begin
 MesajGeneric(MessageText, Application.Title+' - Warning', MB_ICONWARNING or MB_OK);
end;



procedure MesajError(CONST MessageText: string);                                                   { afiseaza un mesaj cu icon de eroare pe ecran. If the MessageText is empty then dispaly nothing }
begin
 MesajGeneric(MessageText, Application.Title+' - Error', MB_ICONERROR or MB_OK);
end;



procedure MesajErrDetail(CONST MessageText, Where: string);                                                    { afiseaza un mesaj cu icon de eroare pe ecran }
VAR sMsg: string;
begin
 sMsg:= MessageText+
         LBRK+ 'Please report this error to us and the exact steps to reproduce it and we will fix it.'+
         CRLF+ 'Hint: press Control+C to copy this message to clipboard.';

 MesajGeneric(sMsg, 'Error in '+  Where, MB_ICONERROR or MB_OK);
end;



//todo: rename this to  MesajAsk
{ Returns True if the user presses the YES btn }
function MesajYesNo(CONST MessageText: string; CONST Title: string= ''): Boolean;
begin
 if MessageText= ''
 then RAISE Exception.Create('No message provided for MesajYesNo() !');
 Result:= MesajGeneric(MessageText, Title, MB_ICONQUESTION or MB_YESNO) = mrYes;      { FUCKING IMPORTANT! Always check for mrYes, never for mrNo. This is why: http://capnbry.net/blog/?p=46 }
end;



{============================================================================================================
  There is a problem with TTaskDialog. It doesn't break long strings! So it is not good for showing long strings/debugging.

  When you have to display longer strings, MessageBox is much better than TTaskDialog because TTaskDialog will wrap your strings too soon (its width tends to be much smaller than MessageBox's width).     So, don't use TTaskDialog for 'normal' messages!
  But you can use TTaskDialog.cxWidth to customize the size of the dialog -> https://stackoverflow.com/questions/33302622/how-can-i-make-the-showmessage-dialog-wider-so-it-fits-the-text

  Documentation about TTaskDialog: http://stackoverflow.com/questions/4979556/how-to-use-the-ttaskdialog
============================================================================================================}
procedure MesajTaskDLG(CONST MessageText, Title: string);
{$WARN SYMBOL_PLATFORM OFF}
VAR
   s: string;
   Dlg: TTaskDialog;
begin
 if MessageText= '' then EXIT;

 if (Win32MajorVersion >= 6)
 AND UseLatestCommonDialogs
 AND StyleServices.Enabled
 then
  begin
   Dlg:= TTaskDialog.Create(Application);
   TRY
     if Title= ''
     then s := Application.Title
     else s := Title;
     Dlg.Caption:= s;
     Dlg.Title  := s;
     Dlg.Text   := CRLFToEnter(MessageText);
     Dlg.CommonButtons := [tcbOk];
     Dlg.Execute;
   FINALLY
     FreeAndNil(Dlg);
   END
  end
 else
   MesajInfo(MessageText, Title);
end;
{$WARN SYMBOL_PLATFORM ON}

























{============================================================================================================
   STRING  -  CONVERSII
============================================================================================================}

{ Also see: ccBinary.StringIsHexNumber
  Doesn't work with Real numbers!
  It works with signs like: '+1' and '-1' }
function StringIsInteger(CONST S: string): Boolean;
begin
  Result:= True;
  TRY
    StrToInt(S);
  EXCEPT
    //todo 1: trap only specific exceptions (EConvertError)
    Result:= FALSE;
  END;
end;


{$Hints Off}       {Needed to silence  "Value assigned to 'iTemp' never used" }
function  StringIsInteger2(CONST s: string): Boolean;
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




{ Split a string (could be a filename) that ENDS in a number into its parts. Example:
           Document_12   ->   Document_  +  12
           NewMelody08   ->   NewMelody  +  08                              }
procedure SplitNumber_End(CONST s: string; OUT Text, Number: string);
VAR i: Integer;
begin
 Number:= s;
 for i:= Length(s) DownTO 1 DO                                                                     { cauta de la coada spre cap.  ->  tre sa vad unde se termina sirul si unde incep digitii. Exemplu: 'Monkey 02' }
   if NOT CharIsNumber(s[i]) then
    begin                                                                                          { -> am dat de litere. Impart textul in doua }
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


{ Returns the postion of the last non-number character in a string. For example 9d9ad8f7ax0000 returns 10 (the position of x) } {Old name: LastCharInString}
function LastLetterInString(CONST s: string): Integer;
begin
 Result:= Length(s);
 WHILE NOT s[Result].IsDigit
  DO Dec(Result);
end;


function IncrementStringNo(CONST s: string): string;                           { Receives a valid number represented as string. Returns the same number but incremented with 1. automatically adjust the leading zeros. The function raises an error if the input text does not represent a number }
VAR i, Zeros, iNumar, OldLength: Integer;
begin
 if s= '' then raise exception.Create(s+ ' is not a valid number!');

 { Keep 0s }
 Zeros:= 0;
 for i:= 1 TO Length(s) DO
   if s[i]= '0'                                                                { vad daca userul a pus zerouri in fata numarului }
   then inc(Zeros)                                                             { daca da, le copiez }
   else Break;

 iNumar:= StrToInt(s);                                                         { Let it RAISE an error if the string is not a valid number }
 OldLength:= Length(IntToStr(iNumar));
 inc(iNumar);

 if Length(IntToStr(iNumar)) > OldLength
 then Dec(Zeros);                                                              { we switched from 9 to 10 or 99 to 100, etc }
 Result:= StringOfChar('0', Zeros)+ IntToStr(iNumar);;
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






{============================================================================================================
   STRING CONVERSIONS
============================================================================================================}

function i2s(CONST Value: integer): string;
begin
 Result:= IntToStr(Value);
end;


{ As above, but additionally it adds the specified number of zeros before the string. See LeadingZerosAuto for details }
function i2s(CONST Value, MaxValue: integer): string;
begin
 Result:= IntToStr(Value);
 Result:= LeadingZerosAuto(Result, 99);
end;


function i2s(CONST Value: Int64): string;
begin
 Result:= IntToStr(Value);
end;


function i2sHuman(CONST Value: Int64): string;       { 1= 1st, 2= 2nd, 3= 3rd, 4= 4th }
begin
 case Value of
    1 : Result:= '1st';
    2 : Result:= '2nd';
    3 : Result:= '3rd';
  else Result:= IntToStr(Value)+ 'th';
 end;
end;


function Str2Cardinal(CONST S: string; Default: Integer): Cardinal;
begin
 Result:= 0;
 mesajinfo('Str2Cardinal-Not implemented yet!');    { Should be easy to implement.Convert each char in a number, multiply with 10, 100, 1000, etc, and add it to Result }
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


{ }
function Real2StrEx(CONST ExtValue: Extended; Decimals: Byte = 1): string;
VAR ComaPos: Integer;
begin
   Assert(NOT System.Math.IsNaN(ExtValue), 'Float is NAN!');
   Result:= FloatToStrF(ExtValue, ffFixed, 16, Decimals);
   ComaPos:= Pos(FormatSettings.DecimalSeparator, Result);
   Result:= system.COPY(Result, 1, ComaPos+ Decimals);
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
 then Result:= Real2Str(Size / TB, Decimals)+ ' TB'; //del else Result:= ' Huge size!';
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
 then Result:= Real2Str(Size / 1000000000000, Decimals)+ ' T';
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

{$else}
{ FROM  https://forums.codegear.com/thread.jspa?messageID=21694
  needs to be tested (for real!) }
function UnicodeToAnsi(CONST str: UnicodeString; codePage: Integer): RawByteString;
VAR len: Integer;
begin
 Result := '';
 if str <> '' then
  begin
   if codePage = 0 then codePage := DefaultSystemCodePage;
   len := WideCharToMultiByte(codePage, 0, PWideChar(str),
   Length(str)+1, nil, 0, nil, nil);
   if len > 1 then
   begin
   SetLength(Result, len - 1);
   SetCodePage(Result, codePage, False);
   WideCharToMultiByte(codePage, 0, PWideChar(str),
   Length(str)+1, PAnsiChar(Result), len, nil, nil);
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


function ReplaceEnters(CONST s, ReplaceWith: string): string;
begin
 Result:= StringReplace(s, #10, ReplaceWith, [rfReplaceAll]);
 Result:= StringReplace(Result     , #13, ReplaceWith, [rfReplaceAll]);
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
function FixNumber(s: string): Integer;
VAR I: Integer;
begin
 I := 1;

 WHILE I <= Length(S) DO
   if NOT CharIsNumber(S[I])
   AND (S[I] <> '-')
   then Delete(S, I, 1)
   else Inc(I);

 Result:= StrToIntDef(S, 0);
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
   if CharInSet(s[i], FullAlfabNmbr)
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



{ Convert Linux enters to Windows enters }
function LinuxEnter2Win(CONST s: string): string;    {Use System.SysUtils.AdjustLineBreaks(s, tlbsCRLF) !! }
VAR i: integer;
begin
 if s= '' then EXIT('');

 { Cheack the first char }
 Result:= s[1];

 { Check all chars except_ the last }
 for i:= 2 to Length(s)-1 DO
  if  (s[i]= #10) AND (s[i+1]<> #13) AND (s[i-1]<> #13)
  then Result:= Result+ CRLF   {CRLF = $D$A}
  else Result:= Result+ s[i];

 { Cheack also the last char }
 if s[Length(s)]= #10
  then Result:= Result+ CRLF
  else Result:= Result+ s[Length(s)];
end;


{ Converts the CRLF text to the actual CRLF (binary 13/10)
  Warning: we don't replace individual CR, LF groups, because there is a high chance we will find that in our text. Example: CRysis }
function CRLFToEnter(CONST s: string): string;
begin
 Result:= StringReplace(s  , 'CRLF ' , 'CRLF', [rfReplaceAll]);
 Result:= StringReplace(Result, ' CRLF' , 'CRLF', [rfReplaceAll]);
 Result:= StringReplace(Result, 'CRLF'  ,  CRLF , [rfReplaceAll]);
end;


{ Replaces #13 with the text 'CR' and #10 with the text 'LF' }
function EnterToCRLF (CONST s: string): string;
begin
 Result:= StringReplace(s  , CRLF, ' CRLF ', [rfReplaceAll]);
 //I need spaces arround CRLF because of the cTranslator.pas
 //The user/DeepL will see better the text to be translated if there are spaces arround CRLF
end;


{ Detects if the specified stream contains Windows or Linux enters }
function GetEnterType(InStream: TStream): EnterType;
VAR
   B1, B2: Byte;
begin
 if NOT Assigned(InStream)
 then raise exception.Create('Input stream not assigned!');

 Result:= etUnknown;
 WHILE InStream.Position < InStream.Size-1 DO
  begin
   InStream.Read(B1, 1);
   case B1 of
    $0D:                           { Look for Mac }
     begin
      InStream.Read(B2, 1);        { Check next char }
      if B2= $0A
      then EXIT(etWin)             { Win: 0D0A }
      else EXIT(etMac);            { Mac: 0D }
     end;
    $0A: EXIT(etNix);              { Linux: 0A }
   end;
  end;
end;


function GetEnterTypeS(InputFile: string): string;
VAR
   InpStream: TCubicBuffStream;
begin
 if NOT FileExists(InputFile)
 then raise exception.Create('Input file does not exist!');

 InpStream:= TCubicBuffStream.Create(InputFile, fmOpenRead);
 TRY
  case GetEnterType(InpStream) of
    etWin: result:= 'Win';
    etNix: result:= 'Nix';
    etMac: result:= 'Mac';
   else
     result:= 'unknown';
  end;
 FINALLY
  FreeAndNil(InpStream);
 END;
end;


{ Returns true if the Enter is format from a single CR character }
function IsMacFile(InStream: TStream): Boolean;
VAR
   B: Byte;
begin
 if NOT Assigned(InStream) then raise exception.Create('Input stream not assigned!');

 Result:= FALSE;
 WHILE InStream.Position < InStream.Size-1 DO
  begin
   InStream.Read(B, 1);
   if B= $0D then
     begin
      InStream.Read(B, 1);
      if (B <> $0A) then EXIT(TRUE);        { Make sure the next char is NOT a LF char }
     end;
  end;
end;



{ Convert all ENTERs in this file from Linux (LF) to Windows (CRLF) format.
  Gives feedback (so you can update a progress bar) as the file is processed.
  EXISTS: System.SysUtils.AdjustLineBreaks() }
procedure UnixToWin(InStream: TStream; OutStream: TStream; Notify: TConvertNotify);
VAR
   B, NewB: Byte; Value: LongInt;
begin
 if NOT Assigned(InStream)  then raise exception.Create('Input stream not assigned!');
 if NOT Assigned(OutStream) then raise exception.Create('Output stream not assigned!');

 if Assigned(Notify)
 then Notify(nkMax, InStream.Size);

 Value := 0;
 WHILE InStream.Position < InStream.Size DO
  begin
   InStream.Read(B, 1);
   case B of
    $0A: begin
          NewB := $0D;
          OutStream.Write(NewB, 1);
          NewB := $0A;
          OutStream.Write(NewB, 1);
         end;
   else
     OutStream.Write(B, 1);
  end;

  if Assigned(Notify) then
   begin
    Inc(Value);
    Notify(nkProgress, Value);
   end;
 end;

 if (Value = InStream.Size)
 AND Assigned(Notify)
 then Notify(nkProgress, 0);
end;



procedure WinToUnix(InStream: TStream; OutStream: TStream; Notify: TConvertNotify);
VAR
   B: Byte; Value: LongInt;
begin
 if NOT Assigned(InStream)  then raise exception.Create('Input stream not assigned!');
 if NOT Assigned(OutStream) then raise exception.Create('Output stream not assigned!');

 if Assigned(Notify)
 then Notify(nkMax, InStream.Size);

 Value := 0;
 while InStream.Position < InStream.Size DO
  begin
   InStream.Read(B, 1);
   if B <> $0A
   then OutStream.Write(B, 1);

   if Assigned(Notify) then
    begin
     Inc(Value);
     Notify(nkProgress, Value);
    end;
  end;

 OutStream.Seek(1, soFromEnd);
 OutStream.Read(B, 1);

 if B <> $0D then
  begin
   B := $0D;
   OutStream.Write(B, 1);
  end;

 if (Value = InStream.Size)
 AND Assigned(Notify)
 then Notify(nkProgress, 0);
end;



procedure MacToWin(InStream: TStream; OutStream: TStream);
VAR
   B, NewB: Byte;
begin
 if NOT Assigned(InStream)  then raise exception.Create('Input stream not assigned!');
 if NOT Assigned(OutStream) then raise exception.Create('Output stream not assigned!');

 WHILE InStream.Position < InStream.Size DO
  begin
   InStream.Read(B, 1);
   case B of
    $0D: begin
          NewB := $0D;
          OutStream.Write(NewB, 1);
          NewB := $0A;
          OutStream.Write(NewB, 1);
         end;
   else
     OutStream.Write(B, 1);
  end;
 end;
end;



procedure WinToUnix(InputFile, OutputFile: String; Notify: TConvertNotify);
VAR
   InpStream: TCubicBuffStream;
   OutStream: TCubicBuffStream;
begin
 if NOT FileExists(InputFile)
 then raise exception.Create('Input file does not exist!');

 InpStream:= TCubicBuffStream.Create(InputFile, fmOpenRead);
 OutStream:= TCubicBuffStream.Create(OutputFile, fmOpenWrite OR fmCreate);
 TRY
  WinToUnix(InpStream, OutStream, Notify);
 FINALLY
  FreeAndNil(InpStream);
  FreeAndNil(OutStream);
 END;
end;



procedure UnixToWin(InputFile, OutputFile: String; Notify: TConvertNotify);
VAR
   InpStream: TCubicBuffStream;
   OutStream: TCubicBuffStream;
begin
 if NOT FileExists(InputFile)
 then raise exception.Create('Input file does not exist!');

 InpStream:= TCubicBuffStream.Create(InputFile, fmOpenRead);
 OutStream:= TCubicBuffStream.Create(OutputFile, fmOpenWrite OR fmCreate);
 TRY
  UnixToWin(InpStream, OutStream, Notify);
 FINALLY
  FreeAndNil(InpStream);
  FreeAndNil(OutStream);
 END;
end;



function MacToWin(InputFile, OutputFile: string): Boolean;                                         { CR to CRLF. Not tested! }
VAR
   InpStream: TCubicBuffStream;
   OutStream: TCubicBuffStream;
begin
 if NOT FileExists(InputFile)
 then raise exception.Create('Input file does not exist!');

 InpStream:= TCubicBuffStream.Create(InputFile, fmOpenRead);
 TRY
  Result:= IsMacFile(InpStream);
  if Result then
   begin
    OutStream:= TCubicBuffStream.Create(OutputFile, fmOpenWrite OR fmCreate);
    TRY
     InpStream.Position:= 0;    { Needs reset because of IsMacFile }
     MacToWin(InpStream, OutStream);
    FINALLY
     FreeAndNil(OutStream);
    END;
   end;
 FINALLY
  FreeAndNil(InpStream);
 END;
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


procedure SplitString(CONST Text, Delimiter: string; OUT sField, sValue: string);                  { Split a string in its components. For example 'ClientName=Bubu' will return in 'ClientName' and 'Bubu' }
VAR FoundAt: Integer;
begin
 FoundAt:= Pos(Delimiter, Text);
 sField:= trim( CopyTo(Text, 1, FoundAt-1));
 sValue:= trim( system.COPY (Text, FoundAt+ Length(Delimiter), MaxInt));
end;


procedure SplitString(CONST Text: string; TSL: TStringList);                                       { Split a string in multiple rows every time the #13#10 char is found (I took this code from Embarcadero's TStringList.Text:= s ) }
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
  DO SplitString(StringList[i], ',', OutList1[i], OutList2[i]);
end;


procedure SplitStringListI(StringList: TStrings; OUT OutList1: TStringArray; OUT OutList2: System.Types.TIntegerDynArray);  { Split each row of the provided StringList into two parts. The two resulted strings are placed in an ArrayOfStrings }
VAR
   i: Integer;
   s: string;
begin
 SetLength(OutList1, StringList.Count);
 SetLength(OutList2, StringList.Count);

 for i:= 0 to StringList.Count-1 DO
  begin
   SplitString(StringList[i], ',', OutList1[i], s);
   OutList2[i]:= StrToInt(s);
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


function String2TSL(s: string): TStringList;                                                       { Converts a string to a TStringList. Need to call Free after this! }
begin
 Result:= TStringList.Create;
 Result.Text:= s;
end;




















{ Works only with letters. It might be faster than IsUpcase }
function IsUpcaseLetter(CONST C: Char): Boolean;
begin
 Result:= CharInSet(c, ccCore.LettersUpCase);
end;


function IsUpcase(CONST C: Char): Boolean;     { Works only with letters. }
begin
 Result:= c = UpCase(c);
end;




{ Insert (ForcedLength-1) zeros in front of the specified string. ForcedLength shows which is the desired lenght of the new string. Example: LeadingZeros('a', 4) will result in '000a'
  Note: you can also do it like this:   To convert an integer to a string with minimum length, use the Str procedure:  Str(123:6, s); // s is set to '   123' }
function LeadingZeros(CONST s: string; CONST ForcedLength: integer): string;
begin
 Result:= s;
 WHILE Length(Result)< ForcedLength DO
  Result:= '0'+ Result;
end;


{TEST IT !!!!!!!!!}
function LeadingZeros2(CONST s: string; CONST ForcedLength: integer): string;
begin
 Result:= Format(s+'<%'+IntToStr(ForcedLength)+'d>', [s]);
end;



{ Same as above except_ that the user doesn't have to specify how many zeros to add.
  Instead the function will determine this automaticxally based on the number received as parameter. For example LeadingZeros('1', 50) will generate '01' but LeadingZeros('1', 500) will generate '001'.   Note: you can also do it like this:   To convert an integer to a string with minimum length, use the Str procedure:  Str(123:6, s); // s is set to '   123' }
function LeadingZerosAuto(CONST s: string; CONST MaxValue: integer): string;
VAR ForcedLength: Integer;
begin
 ForcedLength:= Length(IntToStr(MaxValue));
 Result:= LeadingZeros(s, ForcedLength);
end;



{ This is fast }
{ Note: you can also do it like this:   To convert an integer to a string with minimum length, use the Str procedure:  Str(123:6, s); // s is set to '   123' }
{ Make sure the string has ForcedLength. If not, add some extra characters at its end to make it that long  }
function MakeStringLongRight(CONST s: string; c: Char; CONST ForcedLength: integer): string;
begin
 if Length(s) >= ForcedLength then EXIT(s);

 Result:= StringOfChar(c, ForcedLength- Length(s));
 Result:= s+ Result;
end;


{ Make sure the string has ForcedLength. If not, add some extra characters at its end to make it that long  }
function MakeStringLongRight(CONST s: AnsiString; c: AnsiChar; CONST ForcedLength: integer): AnsiString;
begin
 if Length(s) >= ForcedLength then EXIT(s);

 Result:= StringOfChar(c, ForcedLength- Length(s));
 Result:= s+ Result;
end;


{ Make sure the string has ForcedLength. If not, add some extra characters at its end to make it that long  }
{TODO: This is slow. Use DupeString. }
function MakeStringLongRight(CONST s, Pad: string; CONST ForcedLength: integer): string;
begin
 Assert(Pad > '');
 Result:= s;
 WHILE Length(Result)< ForcedLength
  DO Result:= Result+ Pad;
 Result:= system.COPY(Result, 1, ForcedLength);                                                           { Trim to the specified lenght }
end;


function MakeStringLongLeft (CONST s: string; CONST Pad: string; CONST ForcedLength: integer): string; { Make sure the string has a total of ForcedLength chars. If not, add some extra characters at its front to make it that long  }
begin
 Result:= s;
 WHILE Length(Result)< ForcedLength
  DO Result:= Pad+ Result;
 Result:= system.COPY(Result, 1, ForcedLength);                                                           { Trim to the specified lenght }
end;


{ Exists: System.StrUtils.DupeString and StuffString }
function GenerateString(CONST RepeatTimes: Integer; C: char): string;
begin
 Result:= System.StringOfChar(C, RepeatTimes);
end;








{ RANDOM/UNIQUE STRINGS }
function GenerateRandString(minLen, maxLen: Integer): string;      { This will return all printable craracters (from 65 to 125) }      { YOU MUST call randomize before calling this function! }
var
  i: Integer;
begin
  Assert(minlen > 0);
  SetLength(Result, minLen + Random(maxlen - 4));
  for i := 1 to Length(Result)
   DO Result[i] := Chr(65 + Random(60));
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
function GenerateUniqueString(CONST Len: Integer=32): string;
begin
 Result:= System.IOUtils.TPath.GetGUIDFileName;

 if Len > 32
 then RAISE Exception.Create('Maximum supported length is 64!')
 else Result:= system.COPY(Result, 1, Len);
end;


{ Creates a file that contains random strings. NoOfLines=10000000 creates a files of about 140MB }
procedure GenerateRandomTextFile(CONST aFilename: string; NoOfLines: Integer);
VAR
   t: Textfile;
   i: integer;
begin
  Assignfile(t, aFilename); Rewrite(t);

  for i := 1 to NoOfLines
   DO Writeln(t, GenerateRandString(5, 20));
  Closefile(t);
end;


{ This will return ONLY literary strings (letters and numbers).
  YOU MUST call randomize before calling this function! }                                                     { Old name: GenerateRandomString }
function GenerateRandStringLet(CONST Len: Integer): string;
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
function GenerateRandomWord(CONST Len: Integer=16; StartWithVowel: Boolean= FALSE): string;
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




















{============================================================================================================
   STRING COMPARE
============================================================================================================}

{ Natural compare two strings.
  Example: We have 3 files: pic1, pic2, pic10
     Delphi sort: pic1 pic10 pic2.
     Natural compare sort: pic1 pic2 pic10
  Source: http://www.delphi3000.com/articles/article_5295.asp?SK=                                                                                       // old name: StrNaturalCompare

  Also see: StrCmpLogicalW }
function FileNameNaturalSort(s1, s2: String): Integer;

  function ExtractNr(n: Integer; VAR txt: String): Int64;
  begin
    while (n = Length(txt)) AND ((txt[n]>= '0') and (txt[n]<= '9'))
      do n:= n + 1;
    Result:= StrToInt64Def(system.COPY(txt, 1, n - 1), 0);
    Delete(txt, 1, (n - 1));
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




{ The function checks if any identical characters is near of the actual compare position.
  This is calculated in a formula depending on the length of the strings (diff).
  http://www.helli.de/index.php/tips-and-tricks-delphi-108/fuzzy-compare-delphi-110.html

  Example:
   "John" and "John" = 100%
   "John" and "Jon"  = 75%
   "Jim"  and "James" = 40%
   "Luke Skywalker" and "Darth Vader" = 0% (Hmmm...)

  Keywords: similar compare words text. }
function StringFuzzyCompare (s1, s2: string): Integer;
VAR hit: Integer;                                        // Number of identical chars
    p1, p2: Integer;                                     // Position count
    l1, l2: Integer;                                     // Length of strings
    pt: Integer;                                         // for counter
    diff: Integer;                                       // unsharp factor
    hstr: string;                                        // help VAR for swap strings
    test: array [1..255] of Boolean;                     // Array shows if position is tested
begin
  if Length(s1) < Length(s2) then
   begin                                                 // Test Length and swap, if s1 is smaller we alway search along the longer string
    hstr:= s2;
    s2:= s1;
    s1:= hstr;
   end;

  l1:= Length (s1);                                      // store length of strings to speed up the function
  l2:= Length (s2);
  p1:= 1; p2:= 1;
  hit:= 0;
  diff:= Max (l1, l2) div 3 + ABS (l1 - l2);             // calc the unsharp factor depending on the length of the strings. Its about a third of the whole length

  for pt:= 1 to l1
    do test[pt]:= False;                                 // init the test array

  repeat                                                 // loop through the string
    if not test[p1]
    then
     begin                                               // position tested?
       if (s1[p1] = s2[p2]) and (ABS(p1-p2) <= diff)
       then
        begin                                            // found a matching character?
         test[p1]:= True;
         Inc (hit);                                      // increment the hit count next positions
         Inc (p1); Inc (p2);
         if p1 > l1
         then p1:= 1;
        end
       else
        begin
         test[p1]:= False;                               // Set test array
         Inc (p1);
         if p1 > l1 then
          begin                                          // Loop back to next test position if end of the string
           while (p1 > 1) and not (test[p1])
             do Dec (p1);
           Inc (p2)
          end;
        end;
      end
    else
      begin
       Inc (p1);
       if p1 > l1 then
        begin                                            // Loop back to next test position if end of string
         repeat Dec (p1); until (p1 = 1) or test[p1];
         Inc (p2);
        end;
      end;
  until p2 > Length(s2);
  Result:= 100 * hit DIV l1;                             // calc procentual value
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




function CutRight(CONST s: AnsiString; FromPos, ToPos: integer): AnsiString;  { Copy the text between FromPos and ending at ToPos. The char at ToPos is also copied. }
begin
 Result:= system.COPY(s, FromPos, ToPos-FromPos+1);                           { +1 ca sa includa si valoarea de la potitia 'FromPos' }
end;











{ Copy the text between iFrom and iTo (including) }
function CopyTo(CONST s: AnsiString; iFrom, iTo: integer): AnsiString;
begin
 Result:= system.Copy(s, iFrom, iTo-iFrom+1);                                 { +1 ca sa includa si valoarea de la potitia 'iFrom' }
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
function CopyFromTo(CONST s: string; sFrom, sTo: string; IncludeMarkers: Boolean= FALSE; Offset: Integer= 1): string;
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

   if iTo < 1              { If the ending sFrom (sTo) was not found, copy until the end of the string }
   then iTo:= maxint;
   Result:= system.COPY(s, iFrom, iTo-iFrom+1)
  end
 else Result:= '';
end;



{ Extract the text between the tags.
  For example '<H>Title</H>' will return 'Title' is iFrom= '<H>' and iTo= '</H>' }
function ExtractTextBetween(CONST s, TagStart, TagEnd: string): string;
begin
 Result:= CopyFromTo(s, TagStart, TagEnd, False, 1);
end;




 { Copy the text between iFrom and sTo string was found.
   IncludeMarker toggles the inclusion/exclusion of sTo in the final result.
   if the sTo string is not found, the function will return an empty string unless CopyAllMarkerNotFound is true }
function CopyTo(CONST s: String; iFrom: Integer; sTo: string; IncludeMarker: Boolean= TRUE; CopyAllMarkerNotFound: Boolean= FALSE; MarkerOffset: Integer= 1): String;
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













{-----------------------------------------------------------------------------
   EXTRACT
-----------------------------------------------------------------------------}
{ Returns the top x lines from a text (multiple lines) }
function ExtractTopLines(CONST Text: string; Count: Integer; IgnoreEmptyLines: Boolean= TRUE): string;
VAR
   TSL: TStringList;
   s: string;
   Total: Integer;
begin
 Total:= 0;
 Result:= '';
 TSL:= TStringList.Create;
 TRY
  TSL.Text:= Text;

  for s in TSL DO
    if IgnoreEmptyLines AND (s > '')
    OR NOT IgnoreEmptyLines then
     begin
       Inc(Total);
       Result:= Result+ s+ crlf;
       if Total = Count then Break;
     end;

  Result:= RemoveLastEnter(Result);
 FINALLY
  FreeAndNil(TSL);
 END;
end;


{ Looks for Needle (partial search) into the Haystack.
  When needle it found then it returns the whole line that contained the Needle.
  Haystack is a string what contains multiple lines of text separated by enter. } // Old name: ExtractLine
function FindLine(CONST Needle, Haystack: string): string;
VAR
   TSL: TStringList;
   s: string;
begin
 Result:= '';
 TSL:= String2TSL(Haystack);
 TRY
  for s in TSL DO
    if Pos(Needle, s) > 0
    then EXIT(s);
 FINALLY
  FreeAndNil(TSL);
 END;
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


{ A 'word' is separate by spaces AND other special characters.
  1077ms on a 1.8MB TXT file (AMD 4GHz). }
function WordCountSlow(CONST s: string): Integer;
VAR
   sArray: TStringDynArray;
begin
  sArray:= System.StrUtils.SplitString(s, '.,? =<>*!-:;()/\'+cr+lf);
  Result:= Length(sArray);
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


{ Don't use: Slow! 15ms }
function CountAppearanceC_(CONST Niddle: char; CONST Haystack: string): integer;
VAR i: Integer;
begin
 Result:= 0;
 for i := 1 to Length(Haystack) DO
   if Haystack[i] = Niddle
   then inc(Result);
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



{ Returns a random name in a 100 unique names list }
function GetRandomPersonName: string;
CONST
   Names: array[0..199] of string = ('Aaron','Abigail','Adam','Alan','Albert','Alexander','Alexis','Alice','Amanda','Amber','Amy','Andrea','Andrew','Angela','Ann','Anna','Anthony','Arthur','Ashley','Austin','Barbara','Benjamin','Betty','Beverly','Billy','Bobby','Brandon','Brenda','Brian','Brittany','Bruce','Bryan','Carl','Carol','Carolyn','Catherine','Charles','Charlotte','Cheryl','Christian','Christina','Christine','Christopher','Cynthia','Daniel','Danielle','David','Deborah','Debra','Denise','Dennis','Diana','Diane','Donald','Donna','Doris','Dorothy','Douglas','Dylan','Edward','Elijah','Elizabeth','Emily','Emma','Eric','Ethan','Eugene','Evelyn','Frances','Frank','Gabriel','Gary','George','Gerald','Gloria','Grace','Gregory','Hannah','Harold','Heather','Helen','Henry','Isabella','Jack','Jacob','Jacqueline','James','Janet','Janice','Jason','Jean','Jeffrey','Jennifer','Jeremy','Jerry','Jesse','Jessica','Joan','Joe','John','Jonathan','Jordan','Jose','Joseph','Joshua','Joyce','Juan','Judith','Judy','Julia','Julie','Justin','Karen','Katherine','Kathleen','Kathryn','Kayla','Keith','Kelly','Kenneth','Kevin','Kimberly','Kyle','Larry','Laura','Lauren','Lawrence','Linda','Lisa','Logan','Lori','Louis','Madison','Margaret','Maria','Marie','Marilyn','Mark','Martha','Mary','Mason','Matthew','Megan','Melissa','Michael','Michelle','Nancy','Natalie','Nathan','Nicholas','Nicole','Noah','Olivia','Pamela','Patricia','Patrick','Paul','Peter','Philip','Rachel','Ralph','Randy','Raymond','Rebecca','Richard','Robert','Roger','Ronald','Roy','Russell','Ruth','Ryan','Samantha','Samuel','Sandra','Sara','Sarah','Scott','Sean','Sharon','Shirley','Sophia','Stephanie','Stephen','Steven','Susan','Teresa','Terry','Theresa','Thomas','Timothy','Tyler','Victoria','Vincent','Virginia','Walter','Wayne','William','Willie','Zachary');
begin
  Result:= Names[Random(High(Names))];
end;

function GetRandomStreetName: string;
CONST
   Names: array[0..40] of string = ('Abbey Road', 'Abbotswell Street', 'Abingdon Street', 'Acacia Road', 'Acorn Street', 'Acton Street', 'Adam Street', 'Adelaide Place', 'Admiral Street', 'Agnes Street', 'Albany Street', 'Albemarle Street', 'Albert Cottages', 'Albert Mews', 'Albert Road', 'Albion Mews', 'Alexander Street', 'Alfred Mews', 'Allen Street', 'Allington Street', 'Alma Road', 'Amberley Road', 'Anchor Alley', 'Angel Count', 'Ann Street', 'Anstey Road', 'Beech Street', 'Belgrave Road', 'Belgrave Street', 'Belgrave Terrace', 'Bell Court', 'Bell Yard', 'Belmont Road', 'Bendall Street', 'Bendmore Road', 'Bennett Street', 'Bentinck Street', 'Beresford Street', 'Berkley Street', 'Berwick Street', 'Birdcage Walk');
begin
  Result:= Names[Random(High(Names))];
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




























{============================================================================================================
                                    AUDIO
============================================================================================================}
procedure PlayWinSound(SystemSoundName: string);
begin
 Winapi.MMSystem.PlaySound(PChar(SystemSoundName), 0, SND_ASYNC);
end;


{ All available constants are defined in the registry under the path HKEY_CURRENT_USER -> AppEvents -> Schemes -> Apps -> .Default. Here, depending on the installed applications and your Windows version, you can surely find the one or another sound file and the associated constant.
  System sounds:
    SystemEXCLAMATION        - Note
    SystemHAND               - Critical Stop
    SystemQUESTION           - Question
    SystemSTART              - Windows-Start
    SystemEXIT               - Windows-Shutdown
    SystemASTERIX            - played when a popup alert is displayed, like a warning message.
    RESTOREUP                - Enlarge
    RESTOREDOWN              - Shrink
    MENUCOMMAND              - Menu
    MENUPOPUP                - Pop-Up)
    MAXIMIZE                 - Maximize)
    MINIMIZE                 - Minimize)
    MAILBEEP                 - New Mail)
    OPEN                     - Open Application)
    CLOSE                    - Close Application)
    AppGPFAULT               - Program Error
    Notification             - played when a default notification from a program or app is displayed.
    -----
    Calendar Reminder        - played when a Calendar event is taking place.
    Critical Battery Alarm   - played when your battery reaches its critical level.
    Critical Stop            - played when a fatal error occurs.
    Default Beep             - played for multiple reasons, depending on what you do. For example, it will play if you try to select a parent window before closing the active one.
    Desktop Mail Notif       - played when you receive a message in your desktop email client.
    Device Connect           - played when you connect a device to your computer. For example, when you insert a memory stick.
    Device Disconnect        - played when you disconnect a device from your computer.
    Device Connect Failed    - played when something happened with the device that you were trying to connect.
    Exclamation              - played when you try to do something that is not supported by Windows.
    Instant Message Notif    - played when you receive an instant message.
    Low Battery Alarm        - played when the battery is running low.
    Message Nudge            - played when you receive a BUZZ in an instant message.
    New Fax Notification     - played when you receive a fax via your fax-modem.
    New Mail Notification    - played when you receive an email message.
    New Text Message Notif   - played when you receive a text message.
    NFP Completion           - played when the transfer of data via NFC between your Windows device and another device is completed.
    NFP Connection           - played when your Windows device is connecting to another device via NFC.
    System Notification      - played when a system notification is displayed.

 Flags are:
    SND_SYNC  =0 = Start playing, and wait for the sound to finish
    SND_ASYNC =1 = Start playing, and don't wait to return
    SND_LOOP  =8 = Keep looping the sound until another sound is played  }
procedure PlaySoundFile(FileName: string);
begin
 if FileExists(FileName)
 then PlaySound(pchar(FileName), 0, SND_ASYNC or SND_FILENAME);    { Also exists sndPlaySound but it is obsolete! } { Why 0 for the second parameter: hmod:  Handle to the Executeble file that contains the resource to be loaded. This parameter must be NULL unless SND_RESOURCE is specified in fdwSound. }
end;



{How to load a PlaySoundFile in a resource:
   FileName: 'SOUNDS.RC'
   Body    : #define WAVE WAVEFILE
             SOUND1 WAVE "updating.wav"
   Compiler: BRCC32.EXE -foSOUND32.RES SOUNDS.RC    }
procedure PlayResSound(RESName: String; uFlags: Integer);
VAR hResInfo,hRes: Thandle;
    lpGlob: Pchar;
Begin
 hResInfo:= FindResource(HInstance,PChar(RESName),MAKEINTRESOURCE('WAVEFILE'));
 if hResInfo = 0 then
  begin
    MesajError('Could not find resource'+ CRLF+ RESName);
    EXIT;
  end;

 hRes:=LoadResource(HInstance,hResinfo);
 if hRes = 0 then
  begin
    MesajError('Could not load resource'+ CRLF+ RESName);
    EXIT;
  end;

 lpGlob:=LockResource(hRes);
 if lpGlob=Nil then
  begin
    MesajError('Bad resource'+ CRLF+ RESName);
    EXIT;
  end;

 uFlags:= snd_Memory or uFlags;
 SndPlaySound(lpGlob,uFlags);
 UnlockResource(hRes);
 FreeResource(hRes);
End;


// Note! The sound is not heard if the time is too short (like 35 ms)
procedure Bip(Frecv, Timp: integer);
begin
 WinApi.Windows.Beep(Frecv, Timp);
end;

procedure BipError;
begin
  WinApi.Windows.Beep(700, 70);
  Sleep(50);
  WinApi.Windows.Beep(300, 300);
end;

procedure BipConfirmation;
begin
  WinApi.Windows.Beep(1100, 120);
  Sleep(10);
  WinApi.Windows.Beep(1900, 170);
end;

procedure BipConfirmationShort;
begin
  WinApi.Windows.Beep(1000, 55);
  Sleep(3);
  WinApi.Windows.Beep(1900, 135);
end;

procedure BipErrorShort;
begin
  WinApi.Windows.Beep(700, 50);
  Sleep(5);
  WinApi.Windows.Beep(400, 110);
end;

procedure Bip30;
begin
 WinApi.Windows.Beep(800, 30);
end;

procedure Bip50;
begin
 WinApi.Windows.Beep(800, 50);
end;

procedure Bip100;
begin
 WinApi.Windows.Beep(800, 100);
end;

procedure Bip300;
begin
 WinApi.Windows.Beep(800, 300);
end;

procedure BipCoconuts;
begin
 bip(1000, 30); bip(1200, 40);
 bip(890 , 25); bip(760 , 40);
 bip(1000, 30); bip(1200, 40);
 bip(890 , 25); bip(760 , 40);
end;


{ Writes tone to memory and plays it.   Hz/mSec }
procedure MakeSound(Frequency, Duration: Integer; Volume: Byte);
VAR
  WaveFormatEx: TWaveFormatEx;
  MS: TMemoryStream;
  i, TempInt, DataCount, RiffCount: integer;
  SoundValue: byte;
  w: double;   // omega ( 2 * pi * frequency)
CONST
  Mono: Word = $0001;
  SampleRate: Integer = 11025; // 8000, 11025, 22050, or 44100
  RiffId: string = 'RIFF';
  WaveId: string = 'WAVE';
  FmtId: string = 'fmt ';
  DataId: string = 'data';
begin
  if Volume> 127 then Volume:= 127;
  if Frequency > (0.6 * SampleRate) then
  begin
    Mesaj(Format('Sample rate of %d is too low to play a tone of %dHz', [SampleRate, Frequency]));
    EXIT;
  end;
  with WaveFormatEx do
  begin
    wFormatTag     := WAVE_FORMAT_PCM;
    nChannels      := Mono;
    nSamplesPerSec := SampleRate;
    wBitsPerSample := $0008;
    nBlockAlign    := (nChannels * wBitsPerSample) div 8;
    nAvgBytesPerSec:= nSamplesPerSec * nBlockAlign;
    cbSize         := 0;
  end;
  MS:= TMemoryStream.Create;
  with MS do
  begin
    {Calculate length of sound data and of file data}
    DataCount := (Duration * SampleRate) div 1000;                              // sound data
    RiffCount := Length(WaveId)+ Length(FmtId) + SizeOf(DWORD)+
          SizeOf(TWaveFormatEx)+ Length(DataId)+ SizeOf(DWORD)+ DataCount;      // file data
    {write out the wave header}
    Write(RiffId[1], 4);                                                        // 'RIFF'
    Write(RiffCount, SizeOf(DWORD));                                            // file data size
    Write(WaveId[1], Length(WaveId));                                           // 'WAVE'
    Write(FmtId [1], Length(FmtId));                                            // 'fmt '
    TempInt := SizeOf(TWaveFormatEx);
    Write(TempInt, SizeOf(DWORD));                                              // TWaveFormat data size
    Write(WaveFormatEx, SizeOf(TWaveFormatEx));                                 // WaveFormatEx record
    Write(DataId[1], Length(DataId));                                           // 'data'
    Write(DataCount, SizeOf(DWORD));                                            // sound data size
    {calculate and write out the tone signal} // now the data values
    w := 2 * Pi * Frequency;                                                    // omega
    for i := 0 to DataCount - 1 do
     begin
       SoundValue := 127 + trunc(Volume * sin(i * w / SampleRate));              // wt = w * i / SampleRate
       Write(SoundValue, 1);
     end;
    sndPlaySound(MS.Memory, SND_MEMORY or SND_SYNC); {now play the sound}
    FreeAndNil(MS);
  end;
end;


end.
