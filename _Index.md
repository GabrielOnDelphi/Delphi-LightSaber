# LightSaber -- public API index

> **GENERATED FILE -- do not edit by hand.** Regenerate with `Build_Index.cmd`
> after adding/renaming units or public routines. It maps every public
> `function`/`procedure` (from each unit's INTERFACE section) to its unit, so you
> can find existing functionality fast. The signature here is a POINTER -- open the
> unit to confirm the exact current declaration before relying on it.
>
> Scope: the reusable library units only (`LightCore.*`, `LightVcl.*`, `LightFmx.*`).
> Excludes Form*.pas UI plumbing, tests, demos, and External\ 3rd-party code.

## LightCore.AppData (27)

```pascal
function getLastUsedFolder: string;
procedure setShowOnError(const Value: Boolean);
procedure setHideHint(const Value: Integer); virtual;
procedure loadSettings; virtual;
procedure saveSettings; virtual;
procedure defaultSettings; virtual;
procedure setHintType(const Value: THintType); virtual;
procedure AfterConstruction; override;
function CheckSysDir: Boolean;
procedure Minimize; virtual;
function BetaTesterMode: Boolean;
function IsHardCodedExp(Year, Month, Day: word): Boolean;
procedure LogEmptyRow;
procedure LogBold (CONST Msg: string);
procedure LogError (CONST Msg: string);
procedure LogHint (CONST Msg: string);
procedure LogImpo (CONST Msg: string);
procedure LogInfo (CONST Msg: string);
procedure LogMsg (CONST Msg: string);
procedure LogVerb (CONST Msg: string);
procedure LogWarn (CONST Msg: string);
procedure LogClear;
procedure PopUpLogWindow;
function CommandLinePath: string;
procedure ExtractPathFromCmdLine(MixedInput: string; OUT Path, Parameters: string);
function FindCmdLineSwitch(const Switch: string; IgnoreCase: Boolean): Boolean; deprecated 'Use System.SysUtils.FindCmdLineSwitch';
function ExeName: string;
```

## LightCore.Binary (49)

```pascal
function HexToInt (CONST HexStr: string): Longint;
function BinToInt (CONST Value: String): Integer;
function IntToByte (CONST i: Integer): Byte;
function StringIsHexNumber (CONST s: string): boolean; { Returns TRUE if the input parameter is a valid hex number (has the '$0123ABCDEF' or '0123ABCDEF' format) }
function IntToBin (CONST IntNumber, Digits: Integer): string;
function WordToBin (Value: Word):string;
function ByteToBin (Value: Byte):string;
procedure SwapWord (VAR TwoBytes : Word); assembler;
Procedure SwapCardinal (VAR aCardinal: Cardinal); assembler; { Swap32bits unsigned integer}
Procedure SwapInt (VAR aInteger : Longint); assembler; { Swap32bits signed integer} {INTEL= Little ENDIAN}
procedure SwapWord (VAR TwoBytes: Word); inline;
Procedure SwapCardinal (VAR aCardinal: Cardinal); inline; { Swap32bits unsigned integer}
Procedure SwapInt (VAR aInteger: Integer); inline; { Swap32bits signed integer} {INTEL= Little ENDIAN}
function SwapUInt64 (Value: UInt64): UInt64; inline;
function SwapInt64 (Value: Int64 ): Int64; assembler;
function SwapCardinalF (aCardinal: Cardinal): Cardinal; assembler; { reverse the order of bytes in eax }
function SwapWordF (twoBytes: Word): Word; assembler;
function ReverseByte (b: Byte): Byte; inline; { "Inverts" a binary nunber, so, if the number is "1101", I need to end with "1011". }
function ReverseByte2 (b: Byte): Byte; { This should be the fastest since it uses a LUT - http://stackoverflow.com/questions/14400845/how-can-i-bit-reflect-a-byte-in-delphi }
function ReverseByte3 (b: Byte): Byte; inline;
function RotateRight64 (Value : int64 ; N : Integer): int64; inline;
function RotateLeft64 (Value : int64 ; N : Integer): int64; inline;
function RotateRight32 (Value : dword ; N : Integer): dword; inline;
function RotateLeft32 (Value : dword ; N : Integer): dword; inline;
function MakeWord (B1, B2: byte): Word; inline;
function MakeByte (b1, b2, b3, b4, b5, b6, b7, b8: Boolean): Byte; inline; { B1 is MSB }
function MakeCardinal_ (MSB, b2, b3, b4: Cardinal): Cardinal; { Make a cardinal number from 4 bytes. It merges the bytes in the order the were given }
function MakeCardinal_Slow(MSB, b2, b3, b4: Cardinal): Cardinal;
function MakeCardinal (CONST Hex: String): Cardinal; overload; { Make a cardinal number from a special string. This string contains 4 substrings, each of 2 chars long. Each substring represents a hex number. The order of the hex numbers is MSB. Example: FF332211 }
function MakeCardinal (Hex1, Hex2, hex3, hex4: String): Cardinal; overload; { Make a cardinal number from strings representing HEX numbers. The order of the parameters is MSB }
function SerializeWord (W: Word): String; {$ENDIF} { Does the opposite of MakeWord: Converts the bytes that form this number into their ASCII equivalent. The result is in 'big endian' order. Note that Intel uses 'lil endian'! Exemple: for number 65280 (1111111100000000) the function will return #255 + #0. }
function SerializeCardinal(C: Cardinal): string; { Does the opposite of MakeCardinal: Converts the bytes that form this number into their ASCII equivalent. The result is in 'big endian' order. Note that Intel uses 'lil endian'! Exemple: for number 65280 (1111111100000000) the function will return #255 + #0. }
function GetBit (Value: Cardinal; BitPos: Byte): Boolean; inline; { The BitPos numbering starts from left (7) to right (0). For example for number 254 (11111110), the bit at pos 0 si 0 and the bit at pos 7 (MSB) is 1 }
function ClearBit (Value: Cardinal; BitPos: Byte): Cardinal; inline;
function SetBit (Value: Cardinal; BitPos: Byte): Cardinal; inline;
function ToggleBit (Value: Cardinal; BitPos: Byte; TurnOn: Boolean): Cardinal; inline;
function GetByte (BytePos: Byte; C: Cardinal): Byte; overload; { Byte order (position): 1 2 3 4. For example GetByte(3, $AAFFCC) returns $CC }
function GetByte (BytePos: Byte; i: Integer ): Byte; overload;
function GetByte (BytePos: Byte; W: Word): Byte; overload;
function GetBits (Value: Cardinal; BitFrom, BitTo: Byte): Cardinal;
procedure ChangeByteOrder (VAR Data; Size : Integer); inline;
function Base255to256 (cInput: Cardinal): Cardinal; inline; { http://stackoverflow.com/questions/5680895/i-need-to-convert-a-number-from-base-255-to-base-256 }
function Base256to255 (cInput: Cardinal): Cardinal; inline;
function EnsureByte (b: Integer): Byte; inline; overload; { Make sure that i is in 'byte' range. In other words, returns 0 if i < 0 and 255 if i > 255. Otherwise return i }
function EnsureByte (b: Real): Byte; inline; overload;
function Ensure100 (i: integer): Byte; inline; overload; { Makes sure that the 'I' is not lower than 0 and not higher than 100 }
function Ensure100 (s: Single): Single; inline; overload; { Makes sure that the 'S' is not lower than 0 and not higher than 100 }
function ReadMotorolaWord (Stream: TStream): Word;
function ReadMotorolaCardinal(Stream: TStream): Cardinal; { 4-byte big-endian unsigned (e.g. PNG IHDR width/height) }
```

## LightCore.Debugger (14)

```pascal
function IsRunningUnderDelphiDebugger: Boolean;
function CompilerOptimization : Boolean;
function CompilerOptimizationS: String;
function HighDpiAwarenessS : String; { Windows: returns the current process's DPI awareness state. See implementation for possible values. }
procedure GenerateCrashNIL;
procedure GenerateCrashException;
procedure GenerateLeak;
procedure TimerStart; { use it with: SetPriorityMax }
function TimerElapsed: Double; { In miliseconds }
function TimerElapsedS: string; { In miliseconds or seconds }
function ShowTransferSpeed(FileSize: Cardinal): string; { Shows the disk/internet speed }
procedure LogFile_Init (FullFileName: string); { Init the log }
procedure LogFile_Add (s: string); { Write in Log }
function GenerateCompilerReport: string;
```

## LightCore.Download (6)

```pascal
procedure Reset; // Load default values in these fields
procedure DownloadToFile (CONST URL, SaveTo: string; OUT ErrorMsg: string; CustomHeaders: TNetHeaders = nil; HttpOptions: PHttpOptions = NIL);
function DownloadToStream(CONST URL: string; OUT ErrorMsg: string; CustomHeaders: TNetHeaders = nil; HttpOptions: PHttpOptions = NIL): TMemoryStream;
function DownloadAsString(CONST URL: string; OUT ErrorMsg: string; CustomHeaders: TNetHeaders = nil; HttpOptions: PHttpOptions = NIL): string; overload;
function DownloadAsString(CONST URL: string): string; overload;
function DownloadImageToFile(CONST URL, LocalPath: string; OUT DownloadedSize: Int64): Boolean;
```

## LightCore.EncodeCRC (4)

```pascal
function CRC32_U (CONST s: string) : Cardinal; { For Unicode strings - uses UTF-8 encoding, does not match Total Commander }
function CRC32 (CONST s: AnsiString) : Cardinal; overload; { Compatible with Total Commander 9.0a }
function CRC32 (CONST Bytes: TBytesArray): Cardinal; overload;
function CRC32Stream(AStream: TStream) : Cardinal; { For streams - processes in 64KB chunks }
```

## LightCore.EncodeMime (4)

```pascal
function MimeString (CONST Input: string): string;
function DeMimeString (CONST Input: string): string;
function MimeStringA (CONST Input: AnsiString): AnsiString;
function DeMimeStringA (CONST Input: AnsiString): AnsiString;
```

## LightCore.EncodeXOR (11)

```pascal
function SimpleDecode (CONST s: string): string;
function SimpleEncode (CONST s: string): string;
function EncodeDecode_NOT (CONST s: string): string;
function BetterEncode (CONST s: string; CONST StartKey: WORD): string;
function BetterDecode (CONST s: string; CONST StartKey: WORD): string;
function EncodeDecode_XOR (CONST s: string ; Key: Byte): string; overload; { Torry Encode/Decode}
function EncodeDecode_XOR (CONST Bytes: TBytesArray; Key: Byte): TBytesArray; overload;
function EncodeDecode_XOR (CONST s: AnsiString ; Key: Byte): AnsiString; overload;
function EncodeDecode_XOR (CONST Bytes: TBytesArray; CONST Key: string): TBytesArray; overload; { Multi-byte XOR with string key }
function EncodeXorText (CONST PlainText: string; Key: Byte): string;
function DecodeXorText (CONST EncodedArr: array of Byte; Key: Byte): string;
```

## LightCore.ExceptionLogger (2)

```pascal
procedure InstallExceptionLogger(CONST LogFileName: string = 'Exceptions.log');
function ExceptionLogPath: string; { exposed for diagnostics — read the value from outside if you want to surface it in the UI / About box }
```

## LightCore.Graphics (2)

```pascal
function DetectGraphFormatBySig(CONST FileName: string): string; { Returns canonical extension ('.jpg', '.png', '.gif', '.bmp', '.pdf', '.tif', '.webp', '.jp2', '.wb1', '.RainDrop') or '' on unknown/error. }
function DetectGraphSignature (CONST FileName: string): Integer; { Legacy integer code. 0=unknown (incl. PDF/TIFF/WEBP not in legacy range), 1=BMP, 2=PNG, 3=GIF, 4=JPG, 5=JP2, 6=WB1, 7=RainDrop. }
```

## LightCore.HTML (26)

```pascal
function GetBodyFromHtml (CONST AHTML: string): string; { Get the HTML code contained between the <Body> tags }
function GenerateHTMLHeader (CONST Title, MetaDescription, Keywords, CssFile: string): string; { Old name: GenerateStandardHTMLHeader }
function HtmlEscape (CONST S: string): string; { Escape &<>" so plain text is safe inside HTML markup and double-quoted attribute values }
function HtmlUnescape (CONST S: string): string; { Reverse of HtmlEscape: decode ONLY the four entities it emits (&amp; &lt; &gt; &quot;); &amp; last }
function FindQuoteStart (CONST HtmlTag: string; StartAt: Integer): Integer;
function FindQuoteEnd (CONST HtmlTag: string; StartAt: Integer): Integer;
function ExtractLine (CONST HtmlBody, LineStart: string): string;
function StripAllTags (CONST S: string): string; { Remove all HTML / XML type tags from a string of text. }
function ExtractTagsData (CONST HtmlBody: string; TagName: string): TStringList; { Example: for '<div>xxx</div>' it returns xxx }
function ReplaceAttribValue (HtmlTag, Attrib, NewValue: string): string;
function ExtractAttribValue (CONST HtmlTag, Attrib: string): string; //old name: ExtractTagValue? { Extract the value of a user defined Attrib from a single HTML line (if you provide an entire HTML body then the function will only return the first Attrib found). }
function ExtractAttribValueIE (CONST HtmlTag, Attrib: string): string; { This fixes: IE nu pune quotes arround 'blank'. Example: target=_blank }
function ExtractAhrefTags (CONST HtmlBody: string): TStringList; { Parse HTML file and extract <A HRef> tag. Return a list of TAGS }
function ExtractURLs (CONST HtmlBody: string): TStringList; { Returns a list of links (that start with http://) }
function ExtractURLsText (Text : string; AdreseExtrase: TStringlist): integer; { Extracts URLs from a TEXT. It searches the 'www' and 'http' strings }
function LinkHasNoFollow (CONST Link: string): Boolean;
function LinkOpensInNewWind (CONST Link: string): Boolean;
function MakeLinkRelativeToRoot (CONST MasterPageUrl, Link: string): string; { Rewrites an URLs located in the MasterPage (/x/master/index.html) that points to 'slave' to look like this... }
function MakeLinksRelativeToRoot (CONST HTMLBody, HtmlUrl: string): string;
function ColapseUrlDots (sURL: string): string;
function LineIsMeta (CONST HtmlLine, MetaName: string): Boolean; { Returns true if this line is the specified 'meta'. For example: <meta name="Keywords" content=""> }
function LineIsTitle (CONST HtmlLine: string): Boolean;
function SanitizeText (CONST URL: string): UTF8String; { Encode a string into a URL query string parameter (as per web forms). One would expect that Indy contains well-tested functions to handle this. Well, Indy contains some functions to help with this, but they may not work quite as you expect. In fact, they may not be much use at all. }
function IsSafeHtmlChar (CONST Ch: Integer): Boolean;
function FixHtmlFormatings(Body: TStringList): string; overload;
procedure FixHtmlFormatings(CONST FileName: string); overload;
```

## LightCore.INIFile (15)

```pascal
function ReadDate (CONST Section, Ident: string; Default: TDateTime): TDateTime; reintroduce;
procedure WriteDate(CONST Section, Ident: string; Value : TDateTime); reintroduce;
function ValueExists(CONST Ident: string): Boolean; reintroduce; overload;
function ReadDate (CONST Ident: string; Default: TDateTime): TDateTime; reintroduce; overload;
procedure WriteDate (CONST Ident: string; Value: TDateTime); reintroduce; overload;
function Read (CONST Ident: string; Default: string): string; overload;
procedure Write (const Ident, Value: String); overload;
function Read (const Ident: string; Default: Integer= 0): Integer; overload;
procedure Write (const Ident: string; Value: Integer); overload;
function Read (CONST Ident: string; Default: Boolean= TRUE): Boolean; overload;
procedure Write (const Ident: string; Value: Boolean); overload;
function ReadFont (CONST Ident: string): FontStruct;
procedure WriteFont (CONST Ident: string; Font: FontStruct);
function Read (const Ident: string; Default: Double): Double; overload;
procedure Write (const Ident: string; Value: Double); overload;
```

## LightCore.INIFileQuick (10)

```pascal
procedure WriteInteger (CONST Identifier: string; i: Integer);
function ReadInteger (CONST Identifier: string; DefaultVal: Integer= 0): Integer;
procedure WriteString (CONST Identifier, s: string);
function ReadString (CONST Identifier: string; DefaultVal: string= ''): string;
procedure WriteBool (CONST Identifier: string; b: Boolean);
function ReadBoolean (CONST Identifier: string; DefaultVal: Boolean= FALSE): Boolean;
procedure WriteDbl (CONST Identifier: string; d: Double);
function ReadDbl (CONST Identifier: string; DefaultVal: Double= 0.0): Double;
procedure WriteDate (CONST Identifier: string; d: TDateTime);
function ReadDate (CONST Identifier: string; DefaultVal: TDateTime): TDateTime;
```

## LightCore.Internet.Ftp (8)

```pascal
procedure Clear;
procedure Read (Stream: TLightStream);
procedure Write(Stream: TLightStream);
procedure DoProgress(const Msg: string);
procedure UploadFolder (const LocalDir, RemoteDir, Filter: string); {todo 1: report a percentage via OnProgress, not just per-file text }
procedure ChangeDirForce (const SubDir: string);
procedure NavigateTo (const RemotePath: string);
function DirectoryExists (const Dir: string): Boolean;
```

## LightCore.Internet (48)

```pascal
function UrlEncode (CONST URL: string): string; { Convert unsafe characters. For example space is converted to %20 } { Prepare text to be used in 'a href' link }
function UrlExtractDomain (CONST URL: string): string; { Removes the HTTP and WWW part. Example: www.stuff.com/img.jpg -> stuff.com }
function UrlExtractDomainRelaxed (CONST URL: string): string;
function UrlExtractDomainWWW (CONST URL: string): string; { Removes the HTTP part. Example: http://www.stuff.com/img.jpg -> www.stuff.com }
function UrlExtractProtAndDomain (CONST URL: string): string; { Removes everyting after the .com. Example: http://www.stuff.com/img.jpg -> http://www.stuff.com }
function UrlRemoveStart (CONST URL: string): string; { http://www.stuff.com/img.jpg -> stuff.com/img.jpg }
function URLExtractLastFolder (CONST URL: string): string;
function URLExtractPrevFolder (CONST URL: string): string;
function UrlExtractFilePath (CONST URL: string): string;
function ExtractFilePath_FromURL (CONST Url: string): string; { This function is compatible with Windows - can be used to write local files }
function UrlExtractFileName (CONST URL: string; CleanServerCommands: Boolean= TRUE): string; { Ex: www.stuff.com/test/img.jpg?uniq=0 -> 'img.jpg' }
function GenerateLocalFilenameFromURL (CONST URL: string; UniqueChars: Integer= 6): string;
function GetReferer (CONST URL: string): string; { www.cams.de:80/down/Image.php?w=1200 -> www.cams.de/down/ }
function UrlExtractResource (CONST URL: string): string; { www.stuff.com/test/img.jpg -> /test/img.jpg }
function UrlExtractResourceParams (CONST URL: string): string; { www.cams.de/getImage.php?w=1200 -> /getImage.php?w=1200 }
function CleanServerCommands (CONST URL: string): string;
function UrlRemovePort (CONST URL: string): string; { Example www.Domain.com:80 -> www.Domain.com }
function UrlExtractPort (CONST URL: string): Integer; { Example www.Domain.com:80 -> 80 }
function UrlRemoveHttp (CONST URL: string): string; { http://www.domain/image.jpg -> www.domain/image.jp }
function SameWebSite (CONST URL1, URL2: string): Boolean; { Returns true if the two URLs belong to the same website }
function SameSubWebSite (CONST URL1, URL2: string): Boolean; { Returns true if the URL1 belongs to URL2 or one of its subdomains }
function FileIsInFolder (CONST MainURL, aFile: string): Boolean; { Returns true if the aFile is located of MainURL or one of its subfolders }
procedure ExpandURLs (ShortUrls: TStringList; CONST MainUrl: string); { Expand all urls in the list to a full http path. Example: If the MainURL is 'www.dnabaser.com/tools/' then 'download/' is expanded to 'www.dnabaser.com/tools/download/' }
function ExpandURL (CONST ShortUrl, MainUrl: string): string;
function UrlToLocalPath (CONST URL: string): string; { Converts http://www.Domain.com/download/setup.exe to Domain.com\download\setup.exe }
function URLMakeNonRelativeProtocol(CONST URL: string): string; { Convert from Protocol-Relative to http }
function UrlCorrectInvalidChars_ (CONST URL, ReplaceWith: string): string;
function ValidURL (CONST URL: string): Boolean; { Returns True if string contain only valid chars AND strats with www or http } //old name: UrlContainsValidChars
function ValidUrlChars(CONST URL: string): Boolean; { Returns True if string seems to be a valid URL (does not contain invalid chars such as: < >. Does not check it the string starts with HTTP/WWW }
function UrlForceHttp (CONST URL: string): string; { Add 'HTTP' in from of the URL if it isn't there already }
function CheckHttpStart (CONST URL: string): Boolean; { Check if the URL starts with HTTP }
function CheckWwwStart (CONST URL: string): Boolean; { Check if the URL starts with 'WWW' }
function CheckURLStart (CONST URL: string): Boolean; { Check if the URL starts with HTTP or with www }
function IsURL (CONST s: string): Boolean; deprecated 'Use CheckURLStart instead'; { Returns True if text starts wit http/www/ftp }
function IsWebPage (CONST URL: string): Boolean; { Returns true for /1/2/3.html but not for /1/2/ or for /1/2 }
function ExtractIpFrom (CONST aString: string): string; { finds a IP address in a random string }
function CollectIPAddress (CONST HTMLBody: string): string; { Extract address from HTML file }
function IpExtractPort (CONST Address: string): string;
function SplitIpFromAdr (CONST Address: string): string;
function ServerStatus2String (Status: Integer): string;
function ValidateIpAddress (CONST Address: string): Boolean;
function ValidateProxyAdr (CONST Address: string): Boolean;
function ValidatePort (CONST Port: string) : Boolean;
function ExtractProxyFrom (Line: string): string; { Tries to extract a proxy address from a line of garbage text }
function ExtractProxiesFrom(CONST Text: string): string; { Tries to extract multiple proxies from a string (more than one line) of garbage text. Returns a list of proxies separated by enter }
function GetExternalIp(CONST ScriptAddress: string= 'http://checkip.dyndns.org'): string;
procedure OpenURL(const URL: string); { Opens URL in the default browser. Cross-platform (Win/macOS/Android/iOS). }
Procedure CreateUrl (CONST FullFileName, sFullURL: string); { Creates an .URL file }
```

## LightCore.IO (130)

```pascal
function TrailLinuxPathEx (CONST Path: string): string; { Adds / in front and at the end of the path }
function TrailLinuxPath (CONST Path: string): string; { Adds / at the end of the path }
function TrimLastLinuxSeparator (CONST Path: string): string;
function Convert2LinuxPath (CONST DosPath : string): string;
function Convert2DosPath (CONST LinuxPath: string): string;
function IsFolder (CONST FullPath: string): boolean; { Tells if FullPath is a folder or file. THE FOLDER/FILE MUST EXISTS!!! }
function DirectoryExists (CONST Directory: String; FollowLink: Boolean= TRUE): Boolean; { Corectez un bug in functia 'DirectoryExists' care imi intoarce true pentru un folder care nu exista, de exemplu 'c:\Program Files\ '. Bug-ul apare cand calea se termina cu un caracter SPACE. }
function FileNameIsValid_ (CONST FileName: string): Boolean; deprecated 'Use System.IOUtils.TPath.HasValidFileNameChars instead.'
function PathNameIsValid (CONST Path: string): Boolean; { TPath.HasValidPathChars is bugged - Returns FALSE if the path contains invalid characters. Tells nothing about the existence of the folder }
function IsUnicode (CONST Path: string): boolean; { Returns True if this path seems to be UNICODE }
function ExtractLastFolder (FullPath: string): string; { exemplu pentru c:\windows\system intoarce doar 'system' }
function ExtractParentFolder (CONST Folder: string): string;
function ExtractFirstFolder (CONST Folder: string): string; { For c:\1\2\3\ returns 1\. From c:\1 it returns '' }
function TrimLastFolder (CONST DirPath: string): string; { exemplu pentru c:\windows\system intoarce doar 'c:\windows\' }
function ExtractRelativePath_ (CONST FullPath, RelativeTo: string): string; deprecated 'Use System.SysUtils.ExtractRelativePath instead' { Returns truncated path, relative to 'RelativeTo'. Example: ExtractRelativePath('c:\windows\system32\user32.dll', 'c:\windows') returns system32\user32.dll }
function ShortenFileName (CONST FullPath: String; MaxLength: Integer= MAXPATH): string; { Returns a valid path, but with shorter filename }
function CheckPathLength (CONST FullPath: string; MaxLength: Integer= MAXPATH): Boolean;
function ForcePathDelimiters (CONST Path, Delimiter: string; SetAtBegining, SetAtEnd: Boolean): string; { Old name: UniversalPathDelimiters }
function Trail (CONST Path: string): string; { Replacement for includeTrailingPathDelimiter }
function SameFolder(Path1, Path2: string): Boolean; { Receives two folders. Ex: C:\Test1\ and C:\teSt1 will return true }
function SameFolderFromFile(Path1, Path2: string): Boolean; { Receives two partial or complete file names and compare their folders. Ex: C:\Test1 and C:\teSt1\me.txt will return true }
function IsSubfolder(Path1: String; Path2: String): Boolean;
procedure ForceDirectoriesE (CONST Folder: string);
function ForceDirectoriesB (CONST Folder: string): Boolean; { Replacement for System.SysUtils.ForceDirectories - elimina problema: " { Do not call ForceDirectories with an empty string. Doing so causes ForceDirectories to raise an exception" }
function ForceDirectories (CONST Folder: string): Integer;
function CorrectFolder (CONST Folder : string; ReplaceWith: char= ' '): string; { Folder is single folder. Example '\test\' }
function CorrectFilename (CONST FileName: string; ReplaceWith: char= ' '): string; { Correct invalid characters in a filename. FileName = File name without path }
function ShortenPath (CONST LongPath: String; MaxChars: Integer): String; { Also exists: FileCtrl.MinimizeName, DrawStringEllipsis }
function GetTempFolder : string;
function GetMyDocuments : string; { See this for macosx: http://www.malcolmgroves.com/blog/?p=865 }
function GetMyPictures : string;
function GetHomePath: string;
function GetDocumentsPath: string;
function GetSharedDocumentsPath: string;
function GetMusicPath: string;
function GetMoviesPath: string;
function GetDownloadsPath: string;
function GetLibraryPath: string;
function GetCachePath: string;
function GetPublicPath: string;
function GetRandomFileName: string;
function GetTempFileName: string;
function ListDirectoriesOf (CONST aFolder: string; CONST ReturnFullPath, DigSubdirectories: Boolean): TStringList; { if DigSubdirectories is false, it will return only the top level directories, else it will return also the subdirectories of subdirectories. Returned folders are FullPath. Works also with Hidden/System folders }
function ListFilesAndFolderOf(CONST aFolder: string; CONST ReturnFullPath: Boolean): TStringList;
function ListFilesOf (CONST aFolder, FileType: string; CONST ReturnFullPath, DigSubdirectories: Boolean; ExcludeFolders: TStrings= nil): TStringList;
function FolderIsEmpty (CONST FolderName: string): Boolean; { Check if folder is empty }
function CountFilesInFolder (CONST Path: string; CONST SearchSubFolders, CountHidden: Boolean): Cardinal;
function GetTimestampFileName(CONST Folder, Prefix, Extension: string): string;
function IncrementFileNameEx (CONST FileName: string; StartAt, NumberLength: Integer): string; { Same sa IncrementFileName but it automatically adds a number if the file doesn't already ends with a number }
function IncrementFileName (CONST FileName: string; AddDash: Boolean = false): string; { Receives a file name that ends in a number. returns the same filename plus the number incremented with one. }
function MakeUniqueFolderName (CONST RootPath, FolderName: string): string; { Returns a unique path ended with a number. Old name: Getnewfoldername }
function ChangeFilePath (CONST FullFileName, NewPath: string): string; { Change file path to NewPath }
function AppendNumber2Filename(CONST FileName: string; StartAt, NumberLength: Integer): string; { Add the number at the end of the filename. Example: AppendNumber2Filename('Log.txt', 1) will output 'Log1.txt' }
function FileEndsInNumber (CONST FileName: string): Boolean; { Returns true is the filename ends with a number. Example: MyFile02.txt returns TRUE }
function AppendToFileName (CONST FileName, ApendedText: string): string; { Add a string between the name and the extension. Example: if I add the string ' backup' to the file 'Shell32.DLL' -> 'Shell32 backup.DLL' }
function AppendBeforeName (CONST FullPath, ApendedText: string): string; { Add a string between the last directory separator and the beginning of the file name. Example: if I add the string ' backup' to the file 'c:\Shell32.DLL' -> 'c:\backup Shell32.DLL' -> 'c:\backup Shell32.DLL' -> 'c:\backup Shell32.DLL'. }
function ReplaceOnlyName (CONST FileName, newName: string): string; { Replaces ONLY the name of a file (no extension) in a full path. Example: C:\Name.dll -> C:\NewName.dll }
function RemoveDrive (CONST FullPath: string): string; { C:\MyDocuments\1.doc -> MyDocuments\1.doc }
function ExtractDrive (CONST FullPath: string): string; { C:\MyDocuments\1.doc -> C }
function ExtractFilePath (CONST FullPath: string; AcceptInvalidPaths: Boolean= TRUE): string;{ Same as the old ExtractFilePath but uses the new GetDirectoryName function instead }
function ExtractOnlyName (CONST FileName: string): string; { Extracts the name of a file. If the name is of type name+extension+extension, only the last extension will be removed. Use the new IOUtils library }
function RemoveLastExtension (CONST FileName: string): string; { Extrage numele fisierului din nume+extensie. Daca numele este de tipul nume+extensie+extensie, doar ultima extensie este eliminata }
function ForceExtension (CONST FileName, Ext: string): string; { Makes sure that the 'FileName' file has the extension set to 'Ext'. The 'Ext' parameter should be like: '.txt' }
function ExtractFileExtUp (CONST FileName: string): string; { Case insensitive version }
function AppendFileExtension (CONST FileName, Ext: string): string;
function IsThisType (CONST AFile, FileType: string) : Boolean; { Returns true if the specified file is of the 'FileType' type }
function IsVideo (CONST AGraphFile: string) : Boolean; { Video files supported by FFVCL (cFrameServerAVI) }
function IsVideoGeneric(CONST AGraphFile: string) : Boolean; { Generic video file detection. It doesn't mean I have support for all those files in my app }
function IsGIF (CONST AGraphFile: string) : Boolean;
function IsJpg (CONST AGraphFile: string) : Boolean;
function IsJp2 (CONST AGraphFile: string) : Boolean;
function IsBMP (CONST AGraphFile: string) : Boolean;
function IsWebP (CONST AGraphFile: string) : Boolean;
function IsPNG (CONST AGraphFile: string) : Boolean;
function IsWB1 (CONST AGraphFile: string) : Boolean;
function IsWBC (CONST AGraphFile: string) : Boolean;
function IsICO (CONST AGraphFile: string) : Boolean;
function IsEMF (CONST AGraphFile: string) : Boolean;
function IsWMF (CONST AGraphFile: string) : Boolean;
function IsRainDrop (CONST AGraphFile: string) : Boolean;
function IsImage (CONST AGraphFile: string) : Boolean; { Returns TRUE if the file has a good/known extension and it can be converted to BMP }
function IsImage2Bmp (CONST AGraphFile: string) : Boolean;
function IsText (CONST FileName: string) : Boolean;
function IsDocument (CONST FileName: string) : Boolean;
function IsDfm (CONST FileName: string) : Boolean;
function IsPas (CONST FileName: string) : Boolean;
function IsDpr (CONST FileName: string) : Boolean;
function IsDpk (CONST FileName: string) : Boolean;
function IsDelphi (CONST FileName: string) : Boolean;
function IsExec (CONST FileName: string) : Boolean;
function ExtensionToMimeType (CONST FileName: string): string; // Determines the MIME type of a file based on its extension
function ExtensionFromMimeType(CONST MimeType: string): string; // Determines the file extension based on a given MIME type
function CompareStreams (A, B: TStream; BufferSize: Integer = 4096): Boolean;
function CompareFiles (CONST FileA, FileB: TFileName; BufferSize: Integer = 4096): Boolean;
procedure CopyFileTop (CONST SourceName, DestName: string; CopyBytes: Int64); { Copy only CopyBytes bytes from the begining of the file }
procedure AppendTo (CONST MasterFile, SegmentFile, Separator: string; SeparatorFirst: Boolean= TRUE); { Append Segment to Master. Master must exists. }
procedure MergeFiles (CONST InpFile1, InpFile2, OutputFile, Separator: string; SeparatorFirst: Boolean= TRUE); overload;
function MergeFiles (CONST Folder, FileType, OutputFile, Separator: string; DigSubdirectories: Boolean= FALSE; SeparatorFirst: Boolean= TRUE): Integer; overload;
function BytesToFile (CONST FileName: string; CONST Data: TBytes; CONST Overwrite: Boolean= TRUE): Boolean;
function CopyFile (CONST SourceName, DestName: string): Boolean;
function FileCopyQuick (CONST From_FullPath, To_DestFolder: string): boolean; { in this function you don't have to provide the full path for the second parameter but only the destination folder }
function FileMoveTo (CONST From_FullPath, To_FullPath : string): Boolean;
function FileMoveToDir (CONST From_FullPath, To_DestFolder: string): Boolean;
function AtomicReplaceFile (CONST TempName, FinalName: string): Boolean; { Atomically swap an already-written TempName into FinalName — for crash-safe "write to temp then rename" saves. See body. }
function CopyFolder (CONST FromFolder, ToFolder : String; Overwrite: Boolean= True; CONST FileType: string= '*.*'): integer; { copy a folder and all its files and subfolders }
function MoveFolderRel (CONST FromFolder, ToRelFolder: string; Overwrite: Boolean= True): string;
procedure MoveFolder (CONST FromFolder, ToFolder : String; SilentOverwrite: Boolean= True);
function MoveFolderSlow (CONST FromFolder, ToFolder : String; Overwrite: boolean): Integer; deprecated 'Use TDirectory.Move() instead.';
function BackupFileIncrement (CONST FileName: string; CONST DestFolder: string= ''; const NewExtension: string= '.bak'): string; { Creates a copy of this file in the new folder. Automatically increments its name. Returns '' in case of copy failure }
function BackupFileBak (CONST FileName: string): Boolean; { Creates a copy of this file, and appends as file extension. Ex: File.txt -> File.txt.bak }
function BackupFileDate (CONST FileName: string; TimeStamp: Boolean= TRUE; Overwrite: Boolean = TRUE): Boolean; overload; { Create a copy of the specified file in the same folder. The '_backup' string is attached at the end of the filename }
function BackupFileDate (CONST FileName, DestFolder: string; TimeStamp: Boolean= TRUE; Overwrite: Boolean = TRUE): Boolean; overload;
procedure EmptyDirectory (CONST Path: string); { Delete all files in the specified folder, but don't delete the folder itself. It will search also in subfolders }
procedure DeleteFolder (CONST Path: string);
procedure RemoveEmptyFolders (CONST RootFolder: string); { Not tested! Delete all empty folders / sub-folders (any sub level) under the provided "rootFolder" }
function TryDeleteFile (CONST FileName: string): Boolean;
function GetFileSize (CONST FileName: string): Int64;
function GetFileSizeFormat(CONST FileName: string): string; { Same as GetFileSize but returns the size in b/kb/mb/etc }
function GetFolderSize (CONST Folder: string; CONST FileType: string= '*.*'; DigSubdirectories: Boolean= TRUE): Int64;
function ExtractTimeFromFileName (CONST FileName: string): TTime; { The time must be at the end of the file name. Example: 'MyPicture 20-00.jpg'. Returns -1 if the time could not be extracted. }
function DateToStr_IO (CONST DateTime: TDateTime): string; { Original name: StrTimeToSeconds_unsafe }
function TimeToStr_IO (CONST DateTime: TDateTime): string;
function DateTimeToStr_IO (CONST DateTime: TDateTime): string; overload; { Used to conver Date/Time to a string that is safe to use in a path. For example, instead of '2013/01/01' 15:32 it will return '2013-01-01 15,32' }
function DateTimeToStr_IO: string; overload;
function ExtractDriveLetter (CONST Path: string): char; deprecated 'Use IOUtils.TDirectory.GetDirectoryRoot instead.' { Returns #0 for invalid or network paths. GetDirectoryRoot returns something like: 'C:\' }
function ValidDriveLetter (CONST Drive: Char): Boolean; { Returns false if the drive letter is not in ['A'..'Z'] }
function DriveProtected (CONST Drive: Char): Boolean; { Attempt to create temporary file on specified drive. If created, the temporary file is deleted. } {old name: IsDiskWriteProtected }
function Drive2Byte (CONST Drive: Char): Byte; { Converts the drive letter to the number of that drive. Example drive "A:" is 1 and drive "C:" is 3 }
function Drive2Char (CONST DriveNumber: Byte): Char; { Converts the drive number to the letter of that drive. Example drive 1 is "A:" floppy }
function GetLogicalDrives: TStringDynArray; inline;
```

## LightCore.IOPlatformFile (9)

```pascal
function IsMacFile (InStream: TStream): Boolean; { Returns true if the Enter is format from a single CR character }
function GetEnterType (InStream: TStream): EnterType;
function GetEnterTypeS (CONST InputFile: string): string;
procedure WinToUnix (InStream: TStream; OutStream: TStream; Notify: TConvertNotify); overload;
procedure UnixToWin (InStream: TStream; OutStream: TStream; Notify: TConvertNotify); overload;
procedure MacToWin (InStream: TStream; OutStream: TStream); overload;
procedure WinToUnix (CONST InputFile, OutputFile: String; Notify: TConvertNotify); overload;
procedure UnixToWin (CONST InputFile, OutputFile: String; Notify: TConvertNotify); overload;
function MacToWin (CONST InputFile, OutputFile: string): Boolean; overload; { CR to CRLF }
```

## LightCore.LogLinesAbstract (18)

```pascal
procedure ReadFromStream_v5(Stream: TLightStream); { Current reader }
procedure WriteToStream (Stream: TLightStream); { Current writer }
procedure readFromStream_v5(Stream: TLightStream);
function getItem(Index: Integer): PLogLine; virtual; abstract;
procedure addInternal(Value: PLogLine); inline;
procedure acquireReadLock; virtual;
procedure releaseReadLock; virtual;
procedure Clear; virtual; abstract;
function Count: Integer; virtual; abstract;
function CountFiltered(Verbosity: TLogVerbLvl): Integer; virtual;
function Row2FilteredRow(Row: Integer; Verbosity: TLogVerbLvl): Integer; virtual;
function GetFilteredSlice(Verbosity: TLogVerbLvl; SkipCount: Integer;
procedure ForEachLocked(Proc: TLogLineProc); virtual;
function Add (Value: PLogLine): Integer; virtual; abstract;
function AddNewLine(CONST Msg: string; Level: TLogVerbLvl; Bold: Boolean = FALSE; Indent: Integer = 0): PLogLine; virtual; abstract;
function SnapshotAndClear: TAbstractLogLines; virtual; abstract;
procedure ReadFromStream(Stream: TLightStream); virtual;
procedure WriteToStream (Stream: TLightStream); virtual;
```

## LightCore.LogLinesM (10)

```pascal
function getItem(Index: Integer): PLogLine; override;
procedure acquireReadLock; override;
procedure releaseReadLock; override;
procedure Clear; override;
function Count: Integer; override;
function AddNewLine(CONST Msg: string; Level: TLogVerbLvl; Bold: Boolean = FALSE; Indent: Integer = 0): PLogLine; override;
function Add(Value: PLogLine): Integer; override;
function SnapshotAndClear: TAbstractLogLines; override;
procedure ReadFromStream(Stream: TLightStream); override;
procedure WriteToStream (Stream: TLightStream); override;
```

## LightCore.LogLinesS (6)

```pascal
function getItem(Index: Integer): PLogLine; override;
procedure Clear; override;
function Count: Integer; override;
function AddNewLine(CONST Msg: string; Level: TLogVerbLvl; Bold: Boolean = FALSE; Indent: Integer = 0): PLogLine; override;
function Add (Value: PLogLine): Integer; override;
function SnapshotAndClear: TAbstractLogLines; override;
```

## LightCore.LogRam (35)

```pascal
procedure Populate;
procedure PopUpWindow;
function TryAcquire: Boolean; { Atomic 0->1 transition. TRUE = caller won and must queue the repaint; FALSE = a repaint is already pending, skip. }
procedure Release; { Atomic reset to 0. Called from inside the queued closure on the main thread, BEFORE Populate, so emissions during paint can re-queue. }
procedure setMaxEntries (Value: Integer);
procedure setSaveInterval(Value: Integer);
function prepareString(CONST Msg: string): string;
procedure CheckAndSaveToDisk;
procedure tryOverflowSave; { Helper for CheckAndSaveToDisk; assumes FAutoSaveLock is held. }
procedure tryPeriodicSave; { Helper for CheckAndSaveToDisk; assumes FAutoSaveLock is held. }
function GetObserver: ILogObserver; { Thread-safe FLogObserver read }
procedure Clear;
function Count(Filtered: Boolean; Filter: TLogVerbLvl): Integer;
procedure AddBold (CONST Msg: string);
procedure AddMsg (CONST Msg: string; Bold: Boolean = FALSE);
procedure AddMsgInt (CONST Msg: string; i: Integer; Bold: Boolean = FALSE);
procedure AddEmptyRow;
procedure AddDebug (CONST Msg: string; Bold: Boolean = FALSE);
procedure AddVerb (CONST Msg: string; Bold: Boolean = FALSE);
procedure AddHint (CONST Msg: string; Bold: Boolean = FALSE);
procedure AddInfo (CONST Msg: string; Bold: Boolean = FALSE); { Default level }
procedure AddImpo (CONST Msg: string; Bold: Boolean = FALSE);
procedure AddWarn (CONST Msg: string; Bold: Boolean = FALSE);
procedure AddError (CONST Msg: string; Bold: Boolean = FALSE);
function GetAsText: string;
procedure SaveAsText (CONST FullPath: string);
function LoadFromStream(Stream: TLightStream): Boolean;
procedure SaveToStream (Stream: TLightStream);
procedure SaveToFile (CONST FullPath: string);
function LoadFromFile(CONST FullPath: string): Boolean;
procedure RegisterLogObserver(Observer: ILogObserver);
procedure UnregisterLogObserver;
procedure NotifyLogObserver;
procedure NotifyLogObserverAndShow;
procedure PopUpWindow;
```

## LightCore.LogTypes (1)

```pascal
function Verbosity2String(Verbosity: TLogVerbLvl): string;
```

## LightCore.Math (31)

```pascal
function Min3 (CONST a, b, c: integer): Integer;
function Min3S (CONST a, b, c: Single): Single;
function Find_Max (CONST a, b, c: integer): Integer; overload;
function Find_Max (CONST a, b, c: Cardinal): Cardinal; overload;
function Find_Max (CONST a, b, c: Double): Double ; overload;
procedure NotHigherThan (VAR iInput: Integer; MaxVal: Integer); inline; { Clamps iInput to be no greater than MaxVal }
procedure NotSmallerThan (VAR iInput: Integer; MinVal: Integer); overload; inline; { Clamps iInput to be no less than MinVal }
procedure NotSmallerThan (VAR iInput: Real ; MinVal: Integer); overload; inline;
procedure EnsureZero (VAR i: Integer); inline;
procedure EnsureRange (VAR i: Integer; CONST Min, Max: Integer); inline;
function ProcentNormal (CONST WhatIs, From: Extended): Extended; { What is 10% of 20? Answer: 2}
function ProcentRepresent(CONST xRepresent, From: Int64) : Extended; overload;
function ProcentRepresent(CONST xRepresent, From: Extended): Extended; overload;
function RoundEx (CONST X: Extended): LongInt; { If fractional part is >= 0.5 then the number is rounded up, else down. }
function RoundUp (CONST X: Extended): LongInt; { This will round up a rational number }
function RoundDown (CONST X: Extended): Longint;
function RoundTo (CONST X: Extended; ToCategory: Integer): Longint; { Rounds a number up to the specified category. For example RoundTo(72, 5)=75 and RoundTo(72, 10)=80 }
function BeautifyPrice(Total: Extended): Extended; {Example: Rounds 341 to 344.9 and 346 to 349.9 }
function SameValue (R1, R2: Real): Boolean;
function Median (MX: TDoubleDynArray): Double; overload; { Use it like this: Median(TDoubleDynArray.Create(4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2)) }
function Median (MX: System.Types.TIntegerDynArray): Integer; overload;
function Mean (MX: System.Types.TIntegerDynArray): Integer; { Average of numbers }
function Interq10 (MX: System.Types.TIntegerDynArray): Integer; { Approximate 10th percentile }
function Interq25 (MX: System.Types.TIntegerDynArray): Integer; { Approximate 25th percentile (Q1) }
function Interq75 (MX: System.Types.TIntegerDynArray): Integer; { Approximate 75th percentile (Q3) }
function Interq90 (MX: System.Types.TIntegerDynArray): Integer; { Approximate 90th percentile }
function Factorial (CONST n: byte): Int64; { WARNING: n must be <= 20 to avoid Int64 overflow. Formula: n! = n*(n-1)*(n-2)*...*3*2*1 }
function Combinations (CONST n, r: integer): Int64; { Combinations of n taken r at the time= n! / (n-r)! * r! }
function FastModulo(const X, Y: Integer): Integer; assembler; { https://forum.lazarus.freepascal.org/index.php/topic,36342.15.html }
function MixBytes(FG, BG, BlendPower: byte): Byte; { This function mixes two bytes According to value of TRANS. The value of TRANS is between 0 (result then will be equal to FG) and 255 (result then will be equal to BG) }
function GenerateRandomBoolean: Boolean;
```

## LightCore.MRU (8)

```pascal
procedure SetMaxItems(Value: Integer);
procedure SetFileName(Value: string);
procedure TrimToMaxItems;
procedure DoChanged;
function AddToMRU(CONST aFileName: string): Boolean;
procedure Clear;
function Count: Integer;
function GetItem(Index: Integer): string;
```

## LightCore (130)

```pascal
function CRLFToEnter (CONST s: string): string; // old name: FixCRLF
function EnterToCRLF (CONST s: string): string; // Replaces #13#10 with CRLF
function ReplaceLonellyCR (CONST s, ReplaceWith: string): string;
function ReplaceLonellyLF (CONST s, ReplaceWith: string): string;
function LinuxEnter2Win (CONST s: string): string; deprecated 'Use System.SysUtils.AdjustLineBreaks instead.';
function TrimEnters (CONST s: string): string; { Remove enter characters (#10 and #13) from the beginning and the end of the string }
function RemoveEnters (CONST s: string): string; overload; { Works both with Linux, Windows and half-enter characters }
function RemoveEnters (CONST s: Ansistring): Ansistring; overload;
function ReplaceEnters (CONST s, ReplaceWith: string): string;
function RemoveLastEnter (CONST s: string): string; overload; { Cuts the last Enter from a string }
function RemoveLastEnter (CONST s: AnsiString): AnsiString; overload;
function ReplaceUnicodeChars (CONST s: string; ReplaceWith: char): String; { Replace all Unicode characters withsomething else }
function ReplaceCharF (CONST s: string; CONST SearchFor, ReplaceWith: char): string;
procedure ReplaceChar (var s: string; CONST SearchFor, ReplaceWith: Char); overload;
procedure ReplaceChar (var s: AnsiString; CONST SearchFor, ReplaceWith: AnsiChar); overload;
function ReplaceStringAnsi (CONST s, SearchFor, ReplaceWith: AnsiString): AnsiString;
function ReplaceString (CONST s, SearchFor, ReplaceWith: string): string;
function ReplaceBetween (CONST s, TagStart, TagEnd, ReplaceWith: string; Start: Integer; EliminateTags: Boolean; OUT LastPos: Integer): string;
function SearchBetween (CONST s, TagStart, TagEnd: string; Start: Integer = 1): Integer;
function RemoveNonAlphanum (CONST s: string): string; { Keep only letters and numbers }
function RemoveFormatings (CONST s: string): string;
function RemoveLowChars (CONST s: string): string; overload;
function RemoveLowChars (CONST s: AnsiString): AnsiString; overload;
function RemoveSpaces (CONST s: string): string; overload;
function RemoveSpaces (CONST s: Ansistring): Ansistring; overload;
function RemoveTabs (CONST s: string): string;
function RemoveLastSpace (CONST s: string): string; { Cuts the last SPACE from a string. If there are more spaces only the last one is cut }
function RemoveLastChar (CONST s: string): string; overload;
function RemoveLastChar (CONST s, StrToRemove: string): string; overload;
function RemoveLastChar (CONST s: AnsiString): AnsiString; overload;
function RemoveFirstChar (CONST s: string; Char: Char): string; { Remove first character from the string but ONLY if it is Char }
function RemoveNumbers (CONST s: string): string; overload; { Eliminates numbers from the specified string }
function RemoveNumbers (CONST s: AnsiString): AnsiString; overload;
function TrimUntil (CONST s: string; Limiter: Char): string; { Remove any characters from the beginning and the end of a string until Limiter is found. Limiter is kept. Example: if Limiter is $ and the string is 'xxxxx$ThisIsMyString$@@@@@@'. then the result will be '$ThisIsMyString$' }
function TrimUntilDiff (CONST s: string; Limiter: Char): string; { Remove characters from both ends of a string until something different than Limiter is found. Example: TrimUntilDiff('--ACGT--', '-') returns 'ACGT' }
function Retabulate (CONST s, Delimiter: string; SpaceCount: Integer): string; { Converts multiple spaces to Tab or other similar separator. For example Retabulate('xx xx yy, 3, Tab') will convert the first 3 spaces to tab but not also the next 2 spaces }
function ReplaceNbsp (CONST s, ReplaceWith: string): string;
function IsWordSeparator (CONST aChar: Char): Boolean; { Returns true if the specified char is a word separator .;?,! }
function CopyWords (CONST s: string; MaxChars: Integer): string; { Copy from s all complete words. The result will not have more than MaxChars characters. }
procedure ReplaceShortWords (var s: string; MinLength: Integer; FilterIfNoWovels: Boolean); { This procedure will replace short words (length < MinLength) with spaces. It also filters words that only contain consonants }
function ReplaceWholeWords (const InputStr, OldWord, NewWord: string; const Delimiters: array of Char): string; overload;
function ReplaceWholeWords (const InputStr, OldWord, NewWord: string): string; overload;
function WordCountStrict (CONST s: string): Integer;
function WordCount (CONST s: string): Integer;
function CutInclude2Left (CONST s, SearchFor: string): string; { Delete all chars from end of MATCH to Left - including the match }
function CutInclude2Right (CONST s, SearchFor: string): string; { Delete all chars from beg of MATCH to Right - including the match }
function CutExcludeLeft (CONST s, SearchFor: string): string; { Delete all chars from beg of MATCH to Left - excluding the match }
function CutExcludeRight (CONST s, SearchFor: string): string; { Delete all chars from end of MATCH to Right - excluding the match }
function CopyTo (CONST s: String; iFrom: Integer; CONST sTo: string; IncludeMarker: Boolean= TRUE; CopyAllMarkerNotFound: Boolean= FALSE; MarkerOffset: Integer= 1): string; overload;
function CopyFromTo (CONST s, sFrom, sTo: string; IncludeMarkers: Boolean= FALSE): string;
function CopyFrom (CONST s, sFrom: string; Count: Integer; IncludeMarker: Boolean= TRUE; SearchOffset: Integer= 1): string; overload; { Find sFrom in s. Returns the string from the postion where the text was found, to the end. }
function CopyFrom (CONST s, sFrom: AnsiString; Count: Integer; IncludeMarker: Boolean= TRUE; SearchOffset: Integer= 1): AnsiString; overload;
function CopyTo (CONST s: string; iFrom, iTo: integer): string; overload; { Copy the text between iFrom and ending at iTo (including) }
function CopyTo (CONST s: AnsiString; iFrom, iTo: integer): AnsiString; overload; { Copy the text between iFrom and ending at iTo (including) }
function SplitText (CONST Text, Delimiter: string): TStringList; { Splits a text in lines and puts the lines in a TStringList } {Note: Exista System.StrUtils.SplitString } { Old name: SplitStrings }
procedure SplitLine (CONST Text, Delimiter: string; OUT sField, sValue: string); overload; { Split a string in its components. For example 'ClientName=Bubu' will return in 'ClientName' and 'Bubu' }
procedure SplitStrings (CONST Text: string; TSL: TStringList); overload; { Split a string in multiple rows every time the #13#10 char is found (I took this code from Embarcadero's TStringList.Text:= s ) }
procedure SplitStringAtPos (CONST Text: string; CONST Pos: Integer; OUT s1, s2: string); overload; { Split a string in two substrings at the specified position. The char at Pos will be included in the first string. }
procedure SplitStringAtPos (CONST Text: AnsiString; CONST Pos: Integer; OUT s1, s2: AnsiString); overload;
procedure SplitStringList (StringList: TStrings; OUT OutList1, OutList2: TStringArray); { Split each row of the provided StringList into two parts. The two resulted strings are placed in an ArrayOfStrings }
procedure SplitStringListI (StringList: TStrings; OUT OutList1: TStringArray; OUT OutList2: System.Types.TIntegerDynArray); { Split each row of the provided StringList into two parts. The two resulted strings are placed in an ArrayOfStrings }
function Find (CONST Needle, Haystack: string; PartialSearch: Boolean= False; CaseSens: Boolean= False): boolean;
function CountAppearance (CONST Needle, Haystack: string; CaseSensit: Boolean): integer; overload;
function CountAppearance (CONST Niddle: Char; CONST Haystack: string) : Integer; overload;
function CountAppearance (CONST Niddle: AnsiChar; CONST Haystack: AnsiString): Integer; overload;
function LastPos (CONST Niddle, S: string): Integer; overload; { Return the position of the last occurence of a substring in String. Not tested. Also see 'EndsStr' }
function LastPos (CONST Niddle: Char; CONST S: String): Integer; overload;
function PosAtLeast (CONST Niddle, S: string; AtLeast: Integer): Boolean; { Returns true if the specified string appears at least x times }
function PosInsensitive (CONST Niddle, Haystack: string): Integer; overload;
function PosInsensitive (CONST Niddle, Haystack: AnsiString): Integer; overload;
function LastChar (CONST s: string): string; { Returns the last char in the string but checks first if the string is empty (so it won't crash). Returns '' if the string is empty }
function FirstChar (CONST s: string): string; { Returns the first char in the string but checks first if the string is empty (so it won't crash). Returns '' if the string is empty }
function FirstCharIs (CONST s: string; c: Char): Boolean;
function LastCharIs (CONST s: string; c: Char): Boolean;
function FirstNonSpace (CONST s: string): Integer; { Returns the position of the first character that is no a space. For example: ' Earth' returns 3. }
function i2s (Value: Integer): string; overload; inline;
function i2s (Value, MaxVal: integer): string; overload; { Add the specified number of zeros before the string. See LeadingZerosAuto help for details }
function i2s (Value: Int64) : string; overload; { int64 can hold up to 9223372036854775807 }
function i2sHuman (Value: Int64) : string; { Retunrs something like: 1= 1st, 2= 2nd, 3= 3rd, 4= 4th }
function ExtractIntFromStr (const s: string): Integer; { Extracts a number from a string. Works only if the number is at the beginning of the string. Example '123xxx' }
function Real2Str (CONST ExtValue: Extended; Decimals: Byte = 1; HideNulMantisa: Boolean= True): string;
function Rectangle2Str (CONST Rect: TRect): string;
function FormatBytes (CONST Size: Int64; CONST Decimals: Integer= 1): string; { Format bytes to KB, MB, GB, TB }
function FormatBytesMB (CONST Size: Int64; CONST Decimals: Integer= 1): string; { Same as above but the function will never return values formated in GB range. More exactly instead of 10GB it will return 10240MB }
function FormatNumber (CONST Size: Int64; CONST Decimals: Integer= 1): string; { It will return 1K for 1000, 1M for 1000000 and so on }
function BoolToStrYesNo (CONST B: Boolean): string;
function FixNumber (CONST s: string): Integer; { Converts a text that contains an invalid number to a valid number. For example '!2345' will return '2345' }
function StringIsInteger (CONST s: string): Boolean;
function CharIsNumber (CONST c: char) : Boolean;
procedure SplitNumber_Start (CONST s: string; OUT Text, Number: string); { Splits a string that STARTS with a number into its parts. Example: 01_Render -> 01 + _Render }
procedure SplitNumber_End (CONST s: string; OUT Text, Number: string); { Splits a string that ENDS in a number into its parts. Example: Document12 -> Document + 12 }
function IncrementStringNo (CONST s: string): string; { Receive a number as string. return the same number but incremented with 1. automatically adjust the leading zeros }
function IncrementStringNoEx (CONST s: string): string; { Similar with IncrementStringNo but this version also accepts invalid numbers. If the input string doesn't end with a valid number, append 0 at its end. Then extracts the end number and increase it. Example: 0zzz will return 0zzz0, while xxx33 will retun xxx34 }
function LastLetterInString (CONST s: string): Integer; { Returns the postion of the last non-number character in a string. For example 9d9ad8f7ax0000 returns 10 (the position of x) }
function StringSumm (CONST s: AnsiString): Cardinal; overload;
function StringSumm (CONST s: String): Cardinal; overload; { Compute the summ of all characters in the string }
function InsertCharEvery (CONST c: char; CONST Target: string; Every: Integer): string; { Insert a char into TargetStr every x characters }
function DoubleQuoteStr (CONST s: string): string;
function Reverse (CONST s: String): string; deprecated 'LightCore.Reverse is deprecated. Use System.StrUtils.ReverseString';
function CharInArray (CONST c: Char; const Chars: TCharArray): Boolean;
function CharIsLetter (CONST c: char): Boolean;
function IsUpcaseLetter (CONST c: Char): Boolean;
function IsUpcase (CONST c: Char): Boolean; { Works only with letters. }
function ExtractTextBetween (CONST s, TagStart, TagEnd: string): string; { Extract the text between the tags. For example '<H>Title</H>' will return 'Title' is iFrom= '<H>' and iTo= '</H>' }
function FileNameNaturalSort (s1, s2: String): Integer; { Natural compare two filenames }
function StrCmpLogicalW (psz1, psz2: PWideChar): Integer; stdcall; external 'shlwapi.dll'; {$ENDIF} { Natural compare two strings. Digits in the strings are considered as numerical content rather than text. This test is not case-sensitive. Use it like this: StrCmpLogicalW(PChar(s1), PChar(s2)); see: http://stackoverflow.com/questions/1024515/delphi-is-it-necessary-to-convert-string-to-widestring. }
function FuzzyStringCompare (CONST s1, s2: string): Integer; { Text similarity. The function checks if any identical characters is in the near of the actual compare position. }
function LevenshteinDistance (CONST s1, s2: string): Integer; { Returns the minimum number of single-character edits (insert, delete, substitute) to transform s1 into s2. }
function LevenshteinSimilarity (CONST s1, s2: string): Integer; { Returns similarity as percentage (0-100). Based on Levenshtein distance. }
function LevenshteinSimilarityCase(CONST s1, s2: string): Integer;
function MakeStringLongRight (CONST s, c: AnsiChar; ForcedLength: integer): AnsiString; overload;
function MakeStringLongRight (CONST s, c: Char; ForcedLength: integer): string; overload;
function MakeStringLongRight (CONST s, Pad: string; ForcedLength: integer): string; overload; { Make sure the string has ForcedLength. If not, add some extra characters at its end to make it that long }
function MakeStringLongLeft (CONST s, Pad: string; ForcedLength: integer): string; { Make sure the string has ForcedLength. If not, add some extra characters at its front to make it that long }
function LeadingZeros (CONST s: string; ForcedLength: integer): string; { insert (ForcedLength-1) zeros in front of the specified string. ForcedLength shows which is the desired lenght of the new string. Example: LeadingZeros('a', 4) will result in '000a' }
function LeadingZeros2 (CONST s: string; ForcedLength: integer): string; { Not tested }
function LeadingZerosAuto (CONST s: string; MaxValue: integer): string; { Same as above except_ that the user doesn't have to specify how many zeros to add. Instead the function will determine this automaticxally based on the number received as parameter. For example LeadingZeros('1', 50) will generate '01' but LeadingZeros('1', 500) will generate '001' }
function GenerateString (RepeatTimes: Integer; C: char): string; deprecated 'Use System.StringOfChar instead'; { Exista System.StrUtils.DupeString and StuffString Returns the concatenation of a string with itself a specified number of repeats. }
function GenerateUniqueString (Len: Integer=32): string;
function GenerateRandomWord (Len: Integer=16; StartWithVowel: Boolean= FALSE): string;
function GenerateRandString (minLen, maxLen: Integer): string; { This will return all printable craracters (from 65 to 125) }
function GenerateRandStringLet (Len: Integer): string; { This will return ONLY letters and numbers } { YOU MUST call randomize before calling this function! }
function GetRandomPersonName: string; { Returns a random name in a 100 unique name list }
function GetRandomStreetName: string;
function GetRockBands: TStringList;
function UnicodeToAnsi (CONST str: UnicodeString; codePage: Integer): RawByteString;
function AddNullToStr (CONST Path: string): string;
function GetStringSize (CONST s: string): Integer; { Returns the length of a given string in bytes }
function GetStringRAMSize (CONST s: string): Integer; overload;
function GetStringRAMSize (CONST s: AnsiString): Integer; overload;
```

## LightCore.Pascal (14)

```pascal
function ExtractObjectName(Line: string): String;
function IsMethod (CONST CodeLine: string): Boolean;
function AddUnitToUses (PasBody: TStringList; CONST UnitToAdd: string): Boolean;
function FindSection (PasBody: TStringList; bInterface: Boolean): Integer; { Find the INTERFACE/IMPLEMENTATION section }
function SeparateComments (var CodeLine: string; out Comment: string): Boolean;
function LineIsAComment (Line: string): Boolean;
function CountComments (CONST FileName: string): Integer;
function RelaxedSearch ( CodeLine, Instruction: string): Boolean;
function RelaxedSearchI (CONST CodeLine, Instruction: string): Integer;
function IsKeyword ( CodeLine, Instruction: string): Boolean; // Find delphi keywords (that don't require a semicolon) like: begin, try, except.
function IsReservedKeyword(CONST CodeLine: string): Boolean;
function FindLine (CONST Needle: string; Haystack: TStringList; StartAt: integer): Integer; // Returns the first line where the 'Needle' was found
function WordPos (CONST Needle, HayStack: string): Integer; // Returns the position where Needle was found in the haystack. BUT does so only if the character in front of the Needle is a non-alphabetic character (a-z, A-Z)
function RelaxedSearchEx (Query: string; Haystack: TStringList; StartAt: Integer = 0): Integer;
```

## LightCore.Platform (10)

```pascal
function OsType: string;
function OsArchitecture: string;
function OsIsMobile: Boolean;
function OsVersion: string;
function AppBitness: string;
function AppBitnessEx: string;
function AppIs64Bit: Boolean;
function GeneratePlatformRep: string;
function GenerateAppBitnessRep: string; {$IFDEF FRAMEWORK_FMX}
function GenerateDeviceRep: string; {$ENDIF}
```

## LightCore.Reports (2)

```pascal
function GenerateCoreReport: string; // Global report
function GenerateAppRep: string;
```

## LightCore.RttiSetToString (2)

```pascal
function SetToString(Info: PTypeInfo; const SetParam): string;
procedure StringToSet(Info: PTypeInfo; var SetParam; const Value: string);
```

## LightCore.SearchResult (9)

```pascal
procedure AddNewPos(LinePos, ColumnPos: Integer; CONST CodeLine: string); overload;
procedure AddNewPos(LinePos, ColumnPos: Integer; CONST CodeLine, Offender, WarningMsg: string); overload;
procedure AddNewPos(CONST WarningMsg: string); overload; { For file-wide issues }
function AsString: string; { Full report with positions and messages }
function PositionsAsString: string; { Comma-separated list of line numbers }
function Found: Boolean; { True if any positions recorded }
function Count: Integer;
procedure Clear;
function Last: TSearchResult;
```

## LightCore.StrBuilder (5)

```pascal
procedure AddChar(Ch: Char);
procedure AddEnter;
procedure AddString(const S: string); { Bulk append. Uses doubling growth (vs. AddChar's linear growth) so it scales to huge buffers without quadratic SetLength cost. }
function AsText: string;
procedure Clear;
```

## LightCore.StreamBuff (70)

```pascal
function readSignature: AnsiString; // The LiSa string for "Light Saber'.
procedure checkSafetyLimit(Count: Cardinal);
procedure WriteHeader (CONST Signature: AnsiString; Version: Word);
function ReadHeader (CONST Signature: AnsiString; Version: Word): Boolean; overload;
function ReadHeader (CONST Signature: AnsiString): Word; overload;
procedure WriteCheckPoint(CONST s: AnsiString= '');
procedure ReadCheckPointE(CONST s: AnsiString= ''); // Raises an exception
function ReadCheckPoint (CONST s: AnsiString= ''): Boolean;
function ReadEnter: Boolean;
procedure WriteEnter;
procedure ReadPadding0 (Bytes: Integer= FrozenPaddingSize); // Does not check them for validity
procedure ReadPaddingValidation (Bytes: Integer= FrozenPaddingSize); // Raises an exception if the buffer does not contain the signature
procedure WritePaddingValidation (Bytes: Integer= FrozenPaddingSize); // Raises an exception if the padding does not match the SafetyPaddingStr string. Usefule to detect file corruption. }
procedure WritePadding0 (Bytes: Integer= FrozenPaddingSize); // Writes zeroes as padding bytes.
function ReadBoolean : Boolean;
function ReadByte : Byte;
function ReadCardinal: Cardinal;
function ReadDate : TDateTime; // This is a DOUBLE
function ReadDouble : Double;
function ReadInteger : Integer;
function ReadInt64 : Int64;
function ReadShortInt: ShortInt;
function ReadSingle : Single;
function ReadSmallInt: Smallint;
function ReadUInt64 : UInt64;
function ReadWord : Word;
procedure WriteBoolean (b: Boolean);
procedure WriteByte (b: Byte);
procedure WriteCardinal (c: Cardinal);
procedure WriteDate (d: TDateTime);
procedure WriteDouble (d: Double);
procedure WriteInt64 (i: Int64);
procedure WriteInteger (i: Integer);
procedure WriteShortInt (s: ShortInt);
procedure WriteSingle (s: Single);
procedure WriteSmallInt (s: SmallInt);
procedure WriteUInt64 (i: UInt64);
procedure WriteWord (w: Word);
function ReadRect: TRect;
procedure WriteRect(Rect: TRect);
function ReadRectF: TRectF;
procedure WriteRectF(Rect: TRectF);
procedure ReadIntegers (out List: TIntegerArray);
procedure WriteIntegers(const List: TIntegerArray);
procedure ReadDoubles (out List: TDoubleArray);
procedure WriteDoubles(const List: TDoubleArray);
function RevReadCardinal: Cardinal; { REVERSE READ - read 4 bytes and swap their position. For Motorola format. }
function RevReadInteger : Integer;
function RevReadWord : Word; { REVERSE READ - read 2 bytes and swap their position. For Motorola format. }
procedure WriteString(CONST s: string);
function ReadString(SafetyLimit: Cardinal = 1*KB): string; overload;
procedure WriteStringA (CONST s: AnsiString);
function TryReadStringA(Count: Cardinal): AnsiString; { This is the relaxed version. It won't raise an error if there is not enough data (Len) to read }
function ReadStringACnt(Count: Cardinal; SafetyLimit: Cardinal = 1*KB): AnsiString; { It will raise an error if there is not enough data (Len) to read }
function ReadStringA (SafetyLimit: Cardinal = 1*KB): AnsiString; { It automatically detects the length of the string }
function ReadStrings: TStringList; overload;
procedure ReadStrings (TSL: TStrings); overload;
procedure WriteStrings (TSL: TStrings);
procedure WriteChars (CONST s: AnsiString); overload;
procedure WriteChars (CONST s: string); overload;
function ReadCharsA (Count: Cardinal; SafetyLimit: Cardinal = 1*KB): AnsiString;
function ReadChars (Count: Cardinal): string;
procedure PushString (CONST s: string);
function ReadStringCnt(Count: Cardinal; SafetyLimit: Cardinal = 1*KB): string; overload; { Read 'Len' characters }
function AsBytes: TBytes;
function AsString: AnsiString;
procedure PushBytesCnt (CONST Buffer: TBytes);
function ReadByteChunk: TBytes;
procedure PushAnsi (CONST s: AnsiString);
procedure PushBytes(CONST Bytes: TBytes);
```

## LightCore.StreamFile (69)

```pascal
function ReadSignature: AnsiString; // The LiSa string for "Light Saber'.
procedure WriteHeader (CONST Signature: AnsiString; Version: Word);
function ReadHeader (CONST Signature: AnsiString; Version: Word): Boolean; overload;
function ReadHeader (CONST Signature: AnsiString): Word; overload;
procedure WriteCheckPoint(CONST s: AnsiString= '');
procedure ReadCheckPointE(CONST s: AnsiString= ''); // Raises an exception
function ReadCheckPoint (CONST s: AnsiString= ''): Boolean;
function ReadEnter: Boolean;
procedure WriteEnter;
procedure WritePadding0(Bytes: Integer= FrozenPaddingSize);
procedure ReadPadding0 (Bytes: Integer= FrozenPaddingSize);
procedure WritePadding (Bytes: Integer= FrozenPaddingSize);
procedure ReadPadding (Bytes: Integer= FrozenPaddingSize); // Raises an exception if the buffer does not contain the signature
function ReadBoolean : Boolean;
function ReadByte : Byte;
function ReadCardinal: Cardinal;
function ReadDate : TDateTime; // This is a DOUBLE
function ReadDouble : Double;
function ReadInteger : Integer;
function ReadInt64 : Int64;
function ReadShortInt: ShortInt;
function ReadSingle : Single;
function ReadSmallInt: Smallint;
function ReadUInt64 : UInt64;
function ReadWord : Word;
procedure WriteBoolean (b: Boolean);
procedure WriteByte (b: Byte);
procedure WriteCardinal (c: Cardinal);
procedure WriteDate (d: TDateTime);
procedure WriteDouble (d: Double);
procedure WriteInt64 (i: Int64);
procedure WriteInteger (i: Integer);
procedure WriteShortInt (s: ShortInt);
procedure WriteSingle (s: Single);
procedure WriteSmallInt (s: SmallInt);
procedure WriteUInt64 (i: UInt64);
procedure WriteWord (w: Word);
function ReadRect: TRect;
procedure WriteRect(Rect: TRect);
function ReadRectF: TRectF;
procedure WriteRectF(Rect: TRectF);
procedure ReadIntegers (out List: TIntegerArray);
procedure WriteIntegers(const List: TIntegerArray);
procedure ReadDoubles (out List: TDoubleArray);
procedure WriteDoubles(const List: TDoubleArray);
function RevReadCardinal: Cardinal; { REVERSE READ - read 4 bytes and swap their position. For Motorola format. }
function RevReadInteger : Integer;
function RevReadWord : Word; { REVERSE READ - read 2 bytes and swap their position. For Motorola format. }
procedure WriteString (CONST s: string);
function ReadString(SafetyLimit: Cardinal = 1*KB): string; overload;
procedure WriteStringA (CONST s: AnsiString);
function TryReadStringA(Count: Cardinal): AnsiString; { This is the relaxed version. It won't raise an error if there is not enough data (Len) to read }
function ReadStringACnt(Count: Cardinal; SafetyLimit: Cardinal): AnsiString; { It will raise an error if there is not enough data (Len) to read }
function ReadStringA (SafetyLimit: Cardinal = 1*KB): AnsiString; { It automatically detects the length of the string }
function ReadStrings: TStringList; overload;
procedure ReadStrings (TSL: TStrings); overload;
procedure WriteStrings (TSL: TStrings);
procedure WriteChars (CONST s: AnsiString); overload;
procedure WriteChars (CONST s: string); overload;
function ReadCharsA (Count: Cardinal; SafetyLimit: Cardinal = 1*KB): AnsiString;
function ReadChars (Count: Cardinal): string;
procedure PushString (CONST s: string);
function ReadStringCnt (Count: Cardinal; SafetyLimit: Cardinal = 1*KB): string; overload; { Read 'Len' characters }
function AsBytes: TBytes;
function AsString: AnsiString;
procedure PushBytesCnt (CONST Buffer: TBytes);
function ReadByteChunk: TBytes;
procedure PushAnsi (CONST s: AnsiString);
procedure PushBytes(CONST Bytes: TBytes);
```

## LightCore.StreamMem (78)

```pascal
function ReadSignature: AnsiString; // The LiSa string for "Light Saber'.
procedure AfterConstruction; override; // Sets StringListSafetyLimit default for ALL construction paths (incl. plain Create), so ReadStrings works out of the box
procedure WriteHeader (CONST Signature: AnsiString; Version: Word);
function ReadHeader (CONST Signature: AnsiString; Version: Word): Boolean; overload;
function ReadHeader (CONST Signature: AnsiString): Word; overload;
procedure WriteCheckPoint(CONST s: AnsiString= '');
procedure ReadCheckPointE(CONST s: AnsiString= ''); // Raises an exception
function ReadCheckPoint (CONST s: AnsiString= ''): Boolean;
function ReadEnter: Boolean;
procedure WriteEnter;
procedure ReadPadding0 (Bytes: Integer= FrozenPaddingSize); // Does not check them for validity
procedure ReadPaddingValidation (Bytes: Integer= FrozenPaddingSize); // Raises an exception if the buffer does not contain the signature
procedure WritePaddingValidation (Bytes: Integer= FrozenPaddingSize); // Raises an exception if the padding does not match the SafetyPaddingStr string. Usefule to detect file corruption. }
procedure WritePadding0 (Bytes: Integer= FrozenPaddingSize); // Writes zeroes as padding bytes.
function ReadBoolean : Boolean;
function ReadByte : Byte;
function ReadCardinal: Cardinal;
function ReadDate : TDateTime; // This is a DOUBLE
function ReadDouble : Double;
function ReadInteger : Integer;
function ReadInt64 : Int64;
function ReadShortInt: ShortInt;
function ReadSingle : Single;
function ReadSmallInt: Smallint;
function ReadUInt64 : UInt64;
function ReadWord : Word;
procedure WriteBoolean (b: Boolean);
procedure WriteByte (b: Byte);
procedure WriteCardinal (c: Cardinal);
procedure WriteDate (d: TDateTime);
procedure WriteDouble (d: Double);
procedure WriteInt64 (i: Int64);
procedure WriteInteger (i: Integer);
procedure WriteShortInt (s: ShortInt);
procedure WriteSingle (s: Single);
procedure WriteSmallInt (s: SmallInt);
procedure WriteUInt64 (i: UInt64);
procedure WriteWord (w: Word);
function ReadRect: TRect;
procedure WriteRect(Rect: TRect);
function ReadRectF: TRectF;
procedure WriteRectF(Rect: TRectF);
procedure ReadIntegers (out List: TIntegerArray);
procedure WriteIntegers(const List: TIntegerArray);
procedure ReadDoubles (out List: TDoubleArray);
procedure WriteDoubles(const List: TDoubleArray);
function RevReadCardinal: Cardinal; { REVERSE READ - read 4 bytes and swap their position. For Motorola format. }
function RevReadInteger : Integer;
function RevReadSmallInt: SmallInt;
function RevReadWord : Word; { REVERSE READ - read 2 bytes and swap their position. For Motorola format. }
procedure WriteString (CONST s: string);
function ReadString(SafetyLimit: Cardinal = 1*KB): string; overload;
procedure WriteStringA (CONST s: AnsiString);
function TryReadStringA(Count: Cardinal): AnsiString; { This is the relaxed version. It won't raise an error if there is not enough data (Len) to read }
function ReadStringACnt(Count: Cardinal; SafetyLimit: Cardinal): AnsiString; { It will raise an error if there is not enough data (Len) to read }
function ReadStringA (SafetyLimit: Cardinal = 1*KB): AnsiString; { It automatically detects the length of the string }
function ReadStrings: TStringList; overload;
procedure ReadStrings (TSL: TStrings); overload;
procedure WriteStrings (TSL: TStrings);
procedure WriteChars (CONST s: AnsiString); overload;
procedure WriteChars (CONST s: string); overload;
function ReadCharsA (Count: Cardinal; SafetyLimit: Cardinal = 1*KB): AnsiString;
function ReadChars (Count: Cardinal): string;
procedure WriteChar(CONST c: AnsiChar);
function ReadChar: AnsiChar;
procedure PushString (CONST s: string);
function ReadStringCnt (Count: Cardinal; SafetyLimit: Cardinal = 1*KB): string; overload; { Read 'Len' characters }
function AsBytes: TBytes;
function AsString: AnsiString;
procedure PushBytesCnt (CONST Buffer: TBytes);
function ReadByteChunk: TBytes;
procedure PushAnsi (CONST s: AnsiString);
procedure PushBytes(CONST Bytes: TBytes);
procedure PushData (CONST s: AnsiString); overload; { Clears stream first, then writes }
procedure PushData (CONST Bytes: TBytes); overload; { Clears stream first, then writes }
procedure LoadFromFile(CONST FileName: string);
procedure SaveToFile (CONST FileName: string);
function StringFromStream(MemStream: TMemoryStream; Count: Integer= 0; Pos: Integer= 0): string;
```

## LightCore.StringList (17)

```pascal
procedure RemoveDuplicateString(CONST s: string); { Removes the first occurrence of the specified string (case-insensitive) }
procedure RemoveDuplicateFile(CONST FileName: string); { Removes the first occurrence of the specified filename (case-insensitive) }
procedure RemoveDuplicates; { THIS WILL SORT THE LIST !!! }
procedure RemoveEmptyLines;
function RemoveLines (const BadWord: string): Integer;
function KeepLines (const KeepText: string): Integer;
procedure Trim; { Trim empty spaces, tab, enters, etc on each line }
procedure Shuffle;
function FindLine(const Needle: string): Integer; { Find line that contains the specified text }
procedure SortReverse;
function HighestString: string;
function Concatenate(const Separator: string): String;
function GetTopLines(Count: Integer; IgnoreEmptyLines: Boolean= TRUE): string;
procedure RemoveTopLines(const aCount: Integer);
function String2TSL (CONST s: string): TStringList; { Converts a string to a TStringList. In other words it breaks the text to multiple lines. I need to call Free after this! }
function ExtractTopLines(CONST Text: string; Count: Integer; IgnoreEmptyLines: Boolean= TRUE): string; { Returns the top x lines from a text (multiple lines) }
function FindLine (CONST Needle, Haystack: string): string;
```

## LightCore.StringListA (2)

```pascal
function GetTextStr: AnsiString;
procedure SetTextStr(const Value: AnsiString);
```

## LightCore.System (7)

```pascal
procedure NotImplemented;
procedure EmptyDummy;
procedure DisposeAndNil(VAR P: Pointer);
procedure FillZeros(VAR IntArray: TIntegerDynArray);
function GetResourceAsString(CONST ResName: string): AnsiString; { Extract a resource from self (exe) }
function GetSystemLanguageName: string;
function GetSystemLanguageNameShort: string;
```

## LightCore.TextFile (21)

```pascal
procedure StringToFile (CONST FileName: string; CONST aString: String; WriteOp: TWriteOperation= woOverwrite; WritePreamble: TWritePreamble= wpAuto);
function StringFromFile (CONST FileName: string; Enc: TEncoding= Nil): string;
function StringFromFileExists (CONST FileName: string): String; { Read file IF it exists. Otherwise, return '' }
function StringFromFileTSL (CONST FileName: string; Enc: TEncoding= NIL): TStringList; { Returns a TSL instead of a string. The caller has to free the result }
function StringFromFileA (CONST FileName: string): AnsiString; { Read a WHOLE file and return its content as String. Also see this: http://www.fredshack.com/docs/delphi.html }
procedure StringToFileA (CONST FileName: string; CONST aString: AnsiString; WriteOp: TWriteOperation); overload;
procedure StringToFileA (CONST FileName: string; CONST aString: String; WriteOp: TWriteOperation); overload;
function StringFromFileStart (CONST FileName: string; Count: Cardinal): AnsiString;
function FirstLineFromFile (CONST FileName: string): string;
function CountLines (CONST Filename: string; CONST BufferSize: Cardinal= 128000): Int64; { Opens a LARGE text file and counts how many lines it has. It does this by loading a small portion of the file in a RAM buffer }
function CountCharAppearance (CONST FileName: string; C: AnsiChar): Int64;
procedure GenerateRandomTextFile(CONST Filename: string; NoOfLines: Integer);
function FileHasBOM (CONST FileName: string): Boolean;
function DetectFileEncoding (CONST FileName: string): TEncoding;
function IsValidUtf8File (CONST FileName: string): Boolean;
function IsValidUtf8Stream (Stream: TStream): Boolean;
function ContainsUnicodeChars(CONST S: string): Boolean;
function ConvertToAnsi (CONST FileName: string): Boolean;
procedure ConvertToUTF (CONST FileName: string);
function ForceAddBOM (CONST FileName: string): Boolean;
function ForceRemoveBOM (CONST FileName: string): Boolean;
```

## LightCore.Time (33)

```pascal
function TodayIs: string; { Returns today as date based on Locale. Example: Montag }
function CurrentDateToString{(ShowSeconds: Boolean)}: string; { Returns today as date & time. Example: 31.12.2021 - 16:50 }
function CurrentTimeToString(ShowSeconds: Boolean): string; { Returns time in short format (no seconds). Example: 16:50 }
function TimeToString(CONST T: TDateTime; ShowSeconds: Boolean): string;
function CurrentYear: Word;
function CurrentMonth: Word;
function CurrentDay: Word;
function CurrentHour: Word;
function DecodeHour (Time: TTime): Word;
function DecodeMinute (Time: TTime): Word;
function DecodeSecond (Time: TTime): Word;
function GetUniversalDateFormat: TFormatSettings;
function SameDateEx (Time1, Time2: TDateTime): boolean; deprecated 'Use System.DateUtils.SameDate instead';
function EarlierThan (Date1, Date2: TDateTime): boolean; { Returns true if Date1 is smaller (earlier) than Date2 }
function DaysBetweenEx (CONST MilestoneDate, CurrentDate: TDateTime): Double; deprecated 'Use System.DateUtils.DaySpan instead';
function NewDaysBetween (CONST MilestoneDate, CurrentDate: TdateTime): Double;
function SecondsBetweenEx (CONST MilestoneDate, CurrentDate: TDateTime): Int64; { Returns the number of seconds between two specified TDateTime values. The difference between this function and the Embarcadero one is that it returns zero if CurrentDate >= MilestoneDate }
function DateIsToday (Year, Month, Day: word): Boolean; { Returns true if the specified date is today }
function StringToSeconds (CONST s: String): Integer; { Converts a string formated like 'hh:mm:ss' to seconds. }
function StringIsDate (CONST s: string): Boolean;
function StringIsTime (CONST s: string): Boolean;
function DateToStrUS (CONST Time: TDateTime): string; { converts date to string, using the US (YYY.MM.DD) format }
procedure SecondsToTime (Seconds : Cardinal; VAR D, H, M, S: Cardinal);
function MiliSecToTimeAuto (MSeconds: Cardinal): string;
function mSecondsToTime (mSeconds: Cardinal): string;
function ShowTimeNice (Seconds: Cardinal): string; overload;
function ShowTimeNice (Seconds: Integer): string; overload; { Prevents Delphi from resolving Integer to the TDateTime overload }
function ShowTimeNice (Seconds: Int64): string; overload; { Prevents Delphi from resolving Int64 to the TDateTime overload }
function ShowTimeNice (aTime: TDateTime): string; overload;
function DateTimeToMilliseconds(aDateTime: TDateTime): Int64;
function DateToTime_ (aDate : TDateTime): string;
function Date2Cardinal (xDate: TDate): Cardinal;
function Cardinal2Date (xCardinal: Cardinal): TDate;
```

## LightCore.Types (6)

```pascal
procedure Add(const Element: Double);
procedure Sort;
function Average: Single;
procedure Add(const Element: Integer);
function NextDay: TWeekDays;
function ToString: string;
```

## LightCore.WrapString (5)

```pascal
function WrapStringForced (CONST s: string; MaxRowLength: integer): string; {old name WrapString } { Makes sure that none of the lines of 's' is longer than RowLenght chars } { Also exists: System.SysUtils.WrapText }
function WrapStringForcedA(CONST s: AnsiString; MaxRowLength: integer): AnsiString;
function UnWrapString (CONST s: string): string;
function UnwrapText ( s: string; Separators: string; RemoveExtraEnters, AddExtraSpace: Boolean): string;
function TruncateToWord (CONST s: string; MaxChars: Integer): string;
```

## LightFmx.Common.AppData.Form (25)

```pascal
procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
procedure btnOsBackClick(Sender: TObject);
procedure SetGuiProperties(Form: TForm);
procedure HandleVKStateChange(const Sender: TObject; const M: TMessage);
procedure QueuedPostInitialize; { Deferred from Loaded via ForceQueue. A NAMED method (not an anonymous block) so Destroy can cancel the pending entry with TThread.RemoveQueuedEvents — an anonymous TThreadProcedure cannot be removed, and a form freed before the queue drains would be dereferenced (use-after-free) when the block finally runs. }
procedure RegisterInsetsListener; { Subscribe (once, app-wide) to FMX's inset-changed callback so padding is re-applied the moment insets arrive and on every later change. See ApplyAndroidWindowInsets. }
function CloseTopmostSecondary: Boolean; { Closes the top-most visible non-embedded secondary TLightForm, if any. Used by the Back handler so a non-modally-shown form is dismissed instead of backgrounding the app. }
procedure CreateToolbar;
function HandleBackButton: Boolean; virtual;
procedure FormKeyPress(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState); // We can use this later in the destructor to know how to save the form: asPosOnly/asFull
procedure Loaded; override;
procedure DoClose(var Action: TCloseAction); override;
procedure AfterConstruction; override;
function CloseQuery: Boolean; override;
procedure FormPreRelease; virtual;
procedure FormPostInitialize; virtual; { Runs once after the form is fully loaded AND the message loop is pumping (first paint done). Override for startup work that should not block the window from appearing — e.g. loading a large file. FMX counterpart of the VCL WM_POSTINIT hook. }
procedure saveBeforeExit; { Idempotent (FFormSaved guard). Public so TAppData.Destroy can save all still-open forms while AppData is alive — Application-owned forms are otherwise destroyed AFTER AppData's finalization. }
procedure ShowModal; reintroduce;
procedure LoadForm; virtual;
procedure SaveForm; virtual;
procedure EmbedIn(ALayout: TControl; AParent: TFmxObject); { Marks form as embedded and reparents ALayout into AParent (Client-aligned). }
function FormCloseQueryEmbedded: Boolean; virtual;
procedure MainFormCaption(aCaption: string);
procedure MaximizeVertically;
procedure ApplyAndroidWindowInsets;
```

## LightFmx.Common.AppData (17)

```pascal
function getLogForm: TfrmRamLog;
procedure setHintType(const aHintType: THintType); override;
procedure Run;
procedure Minimize; override;
procedure Restore; // Counterpart of Minimize (mirrors the VCL TAppData.Restore). Also clears StartMinim — without it, one Minimize would latch StartMinim=TRUE in the INI forever and the app would start minimized on every subsequent launch.
function RunFileAtStartup(const FilePath: string; Active: Boolean): Boolean;
function RunSelfAtStartUp(Active: Boolean): Boolean;
function GetAppVersion: string;
procedure CreateMainForm (aClass: TComponentClass; OUT aReference; aAutoState: TAutoState = asPosOnly);
procedure CreateForm (aClass: TComponentClass; OUT aReference; aAutoState: TAutoState = asPosOnly; AOwner: TComponent = NIL); overload;
procedure CreateForm (aClass: TComponentClass; aAutoState: TAutoState = asPosOnly; AOwner: TComponent = NIL); overload;
procedure CreateEmbedded (aClass: TComponentClass; OUT aReference; AOwner: TComponent = NIL); // For forms whose Container is reparented into a host. See TLightForm.CreateEmbedded.
procedure CreateFormHidden(aClass: TComponentClass; OUT aReference);
procedure CreateFormModal (aClass: TComponentClass); // Problem in Android with modal forms!
function GetAutoState(Form: TForm): TAutoState; // Called from TLightForm.Loaded
procedure ShowModal(aForm: TForm); // Available everywhere except Android
procedure SetMaxPriority;
```

## LightFmx.Common.CamUtils (11)

```pascal
procedure RequestCameraPermission(const AOnGranted: TProc);
procedure RequestStorageReadPermission(const AOnGranted: TProc); // For image picking on Android 13+
procedure AddToPhotosAlbum(const ABitmap: TBitmap); // Saves to gallery, handles indexing
procedure ScanMediaFile(const AFileName: string); // If manually saving files
procedure PickImageFromGallery; // Opens the Photos/Gallery picker (Android, iOS)
procedure PickAnyFileFromStorage(const MimeType: string = '*/*'); // Opens system file UI: SAF on Android, UIDocumentPickerViewController on iOS
function GetPublicPicturesFolder: string;
function SetupImagePickerCallback (const AOnImageSelected: TImageSelectedEvent): TMessageSubscriptionId;
function SetupAnyFilePickerCallback(const AOnFileSelected : TFileSelectedEvent ): TMessageSubscriptionId;
function SubscribeToIncomingFileIntents(const AOnFileReceived: TFileSelectedEvent): TMessageSubscriptionId;
procedure ProcessLaunchIntent(const AOnFileReceived: TFileSelectedEvent);
```

## LightFmx.Common.CenterControl (7)

```pascal
procedure CenterFormOnDesktop(Form: TForm);
procedure EnsureFormVisibleOnScreen(Form: TForm);
procedure CenterFormOnParent(Form: TForm; AParent: TForm);
procedure CenterControl(Ctrl: TControl); overload;
procedure CenterControl(Ctrl: TControl; const AParentWidth, AParentHeight: Single); overload;
procedure EnsureControlVisible(Ctrl: TControl); overload;
procedure EnsureControlVisible(Ctrl: TControl; const AParentWidth, AParentHeight: Single); overload;
```

## LightFmx.Common.CrashHandler (5)

```pascal
procedure InstallCrashHandler; // Idempotent. Call once early in DPR.
function CrashLogPath: string; // <AppDataFolder>\crash.log. Returns '' if InstallCrashHandler has not been called yet.
function HasPendingCrashLog: Boolean; // True if a crash.log from a previous session exists.
function ReadPendingCrashLog: string; // File contents, or '' if no file.
procedure ClearPendingCrashLog; // Deletes the file. Safe to call when no file exists.
```

## LightFmx.Common.Dialogs (7)

```pascal
procedure GenericMessage(CONST MessageText: string; CONST Caption: string= ''; DlgType: TMsgDlgType= TMsgDlgType.mtCustom);
procedure MessageInfo (CONST MessageText: string; CONST Caption: string= '');
procedure MessageWarning(CONST MessageText: string; CONST Caption: string= '');
procedure MessageError (CONST MessageText: string; CONST Caption: string= ''); overload;
procedure MessageError (CONST MessageText, Where: string; CONST Caption: string); overload;
procedure MessageYesNo(CONST MessageText: string; CONST Caption: string= ''; CONST Callback: TProc<Boolean>= NIL);
function ResolveErrorCaption(CONST Where, Caption: string): string;
```

## LightFmx.Common.DialogsBlocking (6)

```pascal
procedure GenericMessage (CONST MessageText: string; CONST Caption: string= ''; DlgType: TMsgDlgType= TMsgDlgType.mtCustom);
procedure MessageInfo (CONST MessageText: string; CONST Caption: string= '');
procedure MessageWarning (CONST MessageText: string; CONST Caption: string= '');
procedure MessageError (CONST MessageText: string; CONST Caption: string= ''); overload;
procedure MessageError (CONST MessageText, Where: string; CONST Caption: string); overload;
function MessageYesNo (CONST MessageText: string; CONST Caption: string= ''): Boolean;
```

## LightFmx.Common.Graph (18)

```pascal
procedure GetImageResolution(FileName: string; Out Width, Height: Integer);
procedure LoadImage (FileName: string; Image: TImage; Color: TAlphaColor= TAlphaColorRec.DeepPink); overload;
procedure LoadImage (const Bytes: TBytes; Image: TImage); overload;
function LoadImage (FileName: string): TBitmap; overload;
function LoadImage (const Bytes: TBytes): TBitmap; overload;
procedure SaveBitmap (BMP: TBitmap; FileName: string);
procedure FillBitmap (BMP: TBitmap; Color: TAlphaColor);
function CreateBitmap (Width, Height: Integer; BkgClr: TAlphaColor= TAlphaColorRec.Black): TBitmap;
function CropBitmap (InputBMP: TBitmap; CropRect: TRectF): TBitmap; overload;
function CropBitmap (FileName: string; CropRect: TRectF): TBitmap; overload;
procedure CropBitmap (FileName: string; CropRect: TRectF; Image: TImage); overload;
function CropBitmap (const Bytes: TBytes; CropRect: TRectF): TBitmap; overload;
procedure CropBitmap (const Bytes: TBytes; CropRect: TRectF; Image: TImage); overload;
function FlipBitmapVertical (InputBMP: TBitmap): TBitmap;
procedure FlipBitmapVerticalP (Bitmap: TBitmap);
function FlipBitmapHorizontal (InputBMP: TBitmap): TBitmap;
procedure FlipBitmapHorizontalP(Bitmap: TBitmap);
function BitmapToBytes (BMP: TBitmap; Extension: string = '.png'): TBytes;
```

## LightFmx.Common.Helpers (3)

```pascal
function GetParentForm(Control: TControl; TopForm: Boolean = True): TCommonCustomForm;
function FindImmediateParentForm(Obj: TFmxObject): TCommonCustomForm;
function CopyToClipboard(CONST s: string): Boolean;
```

## LightFmx.Common.IniFile (15)

```pascal
procedure readCtrlPos (Ctrl: TControl);
procedure writeCtrlPos (Ctrl: TControl);
procedure writeSplitter(Comp: TComponent);
procedure readSplitter (Comp: TComponent);
function IsSupported(WinCtrl: TComponent): Boolean; virtual;
procedure SaveForm (Form: TForm; AutoState: TAutoState= asPosOnly); { Save ALL supported controls on this form }
procedure LoadForm (Form: TForm; AutoState: TAutoState= asPosOnly);
function Read (CONST Ident: string; Font: TFont): Boolean; overload;
procedure Write (CONST Ident: string; Font: TFont); overload;
function ReadColor (CONST Ident: string; Default: TColor): TColor;
procedure WriteColor (CONST Ident: string; Value: TColor);
function WriteComp (Comp: TComponent): Boolean; virtual;
function ReadComp (Comp: TComponent): Boolean; virtual;
procedure ReadGroup (WinCtrl: TControl);
procedure WriteGroup (WinCtrl: TControl);
```

## LightFmx.Common.LogFilter (6)

```pascal
function getVerbosity: TLogVerbLvl;
procedure setVerbosity (Value: TLogVerbLvl);
procedure setLog (Value: TLogViewer);
procedure setShowDebugMsg(Value: Boolean);
procedure TrackBarChange(Sender: TObject);
procedure Register;
```

## LightFmx.Common.LogForm (15)

```pascal
procedure btnClearClick (Sender: TObject);
procedure chkScrollDownChange (Sender: TObject);
procedure FormClose (Sender: TObject; var Action: TCloseAction);
procedure FormDestroy (Sender: TObject);
procedure mnuCopyAllClick (Sender: TObject);
procedure mnuCopyClick (Sender: TObject);
procedure mnuCopyFilteredClick(Sender: TObject);
procedure mnuCopySelectedClick(Sender: TObject);
procedure chkLogOnErrorChange (Sender: TObject);
procedure chkShowTimeChange (Sender: TObject);
procedure chkShowDateChange (Sender: TObject);
procedure FormCreate (Sender: TObject);
procedure btnReportClick(Sender: TObject);
procedure LoadSettings;
procedure SaveSettings;
```

## LightFmx.Common.LogViewer (25)

```pascal
procedure setShowDate(const Value: Boolean);
procedure setShowTime(const Value: Boolean);
procedure resizeColumns;
procedure scrollToBottom;
procedure setVerbFilter(const Value: TLogVerbLvl);
function getLineFiltered(Row: Integer): PLogLine;
procedure MyDrawColumnCell(Sender: TObject; const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF; const Row: Integer; const Value: TValue; const State: TGridDrawStates);
function AddColumn(aHeader: string): TStringColumn;
procedure RemoveAllColumns;
procedure Resize; override;
procedure setUpRows;
function DefinePresentationName: string; override;
procedure Clear;
procedure RegisterVerbFilter(TrackBar: TFmxObject); {TLogVerbFilter}
procedure ConstructInternalRamLog;
procedure AssignExternalRamLog(ExternalLog: TRamLog);
procedure ObserveAppDataLog;
procedure Populate;
procedure PopUpWindow;
function Count: Integer;
procedure CopyAll;
procedure CopyCurLine;
procedure CopyVisible;
function Verbosity2Color(Verbosity: TLogVerbLvl; IsDark: Boolean = False): TAlphaColor;
procedure Register;
```

## LightFmx.Common.Screen (8)

```pascal
function ScreenScale: Single;
function IsPhoneScreen : Boolean; { Width < COMPACT_WIDTH (600) — phone }
function IsTabletScreen : Boolean; { Width in COMPACT_WIDTH..HIGH_WIDTH (600–1024) — tablet }
function IsDesktopScreen: Boolean; { Width >= HIGH_WIDTH (1024+) — desktop }
function WidthFitsPhone (W: Single): Boolean; { W < COMPACT_WIDTH (600) }
function WidthFitsTablet (W: Single): Boolean; { COMPACT_WIDTH ≤ W < HIGH_WIDTH }
function WidthFitsDesktop(W: Single): Boolean; { W ≥ HIGH_WIDTH (1024) }
function GenerateScreenResolutionRep: string;
```

## LightFmx.Common.Styles (11)

```pascal
function IsStyleCompatible(const StyleFile: string; out LoadFailed: Boolean): Boolean; overload;
function IsStyleCompatible(const StyleFile: string): Boolean; overload;
function IsDarkStyle: Boolean;
function GetThemeTextColor(Scene: IScene): TAlphaColor; overload; { Returns the foreground text color from the active style }
function GetThemeTextColor: TAlphaColor; overload;
function GetThemeBackgroundColor(OUT Color: TAlphaColor): Boolean; { Returns the form background color from the active style. FALSE if unavailable (image-based skins like Calypso/Jet). }
function GetButtonNormalTextColor: TAlphaColor; { Returns the normal text color that TButton uses — buttonstyle > text > NormalColor. Falls back to foregroundcolor or theme default. }
function GetStyleHighlightColor: TAlphaColor; { Returns the highlight/accent color from the active style (selectioncolor → selection → glow → foregroundcolor). Always fully opaque. }
function GetStyleGlowColor: TAlphaColor; { Returns the glow color from 'glow' resource. Falls back to GetStyleHighlightColor if not found. }
function GenerateThemePalette(BaseColor: TAlphaColor; IsDark: Boolean): TArray<TAlphaColor>;
function ReplaceColorAlpha(Color: TAlphaColor; NewAlpha: Byte): TAlphaColor; inline;
```

## LightFmx.Common.SysTray (16)

```pascal
procedure TrayWndProc(var Msg: TMessage);
procedure FormWndProc(var Msg: TMessage);
procedure AppWndProc (var Msg: TMessage);
procedure ShowContextMenu;
procedure ApplyToShell; // NIM_MODIFY with current icon + tip
procedure Install;
procedure Uninstall;
procedure HookForm(const AForm: TCommonCustomForm);
procedure UnhookForm;
procedure SetTooltip(const S: string);
procedure SetColorIcon(Preset: TTrayColor);
procedure SetSparklineIcon(const Bars: array of Int64; StartIdx, ActiveCnt: Integer; MaxY: Int64; R, G, B: Byte);
procedure SetIcon(AIcon: THandle);
procedure LoadIconFromResource(const ResName: string); // LoadIcon from app .res; 0 = default icon
procedure UseFormIcon(const AForm: TCommonCustomForm); // grab form's GCL_HICONSM as tray icon
procedure ShowBalloonHint(const Title, Text: string; Icon: TBalloonIconType = biInfo);
```

## LightFmx.Visual.AnimatedMemo (6)

```pascal
procedure TimerTick(Sender: TObject);
procedure SetAnimatedText(const Value: string);
procedure SetInterval(const Value: Integer);
procedure ClearAnimation; // Clears pending text and stops animation
function IsAnimating: Boolean; // Returns True if animation is in progress
procedure Register;
```

## LightFmx.Visual.Animations (4)

```pascal
procedure MakeDot(Index: Integer);
procedure Notification(AComponent: TComponent; Operation: TOperation); override;
procedure Hide;
procedure Fire(Sender: TObject);
```

## LightFmx.Visual.AutoGrowMemo (9)

```pascal
function ComputeNeededHeight: Single;
procedure AdjustParentHeight;
procedure OnChangeHandler(Sender: TObject);
procedure SetMinHeight (const Value: Single);
procedure SetMaxHeight (const Value: Single);
procedure SetInternalPadding(const Value: Single);
procedure Resize; override;
procedure Loaded; override;
procedure Register;
```

## LightFmx.Visual.AutoSizeBox (5)

```pascal
procedure setBoxType(Value: TBoxType);
procedure Resize; override;
procedure ApplyTextColor; virtual;
function getParentContentWidth: Single;
procedure UpdateSize; virtual; abstract;
```

## LightFmx.Visual.AutoSizeBoxImg (4)

```pascal
procedure LoadImage(const FileName: string; const aBoundBox: TRectF); overload;
procedure LoadImage(const Bytes: TBytes; const aBoundBox: TRectF); overload;
procedure UpdateSize; override;
procedure Register;
```

## LightFmx.Visual.AutosizeBoxText (5)

```pascal
procedure SetText(const Value: string);
procedure ApplyTextColor; override;
procedure UpdateSize; override;
function MakeTextBubble(Parent: TControl; const Text: string; BoxType: TBoxType): TAutosizeBoxText;
procedure Register;
```

## LightFmx.Visual.CheckBox (6)

```pascal
procedure SetAutoSize(const Value: Boolean);
procedure FitSize;
procedure ApplyStyle; override;
procedure DoChanged; override;
procedure Resize; override;
procedure Register;
```

## LightFmx.Visual.ColorPalette (16)

```pascal
procedure Rebuild;
procedure SetItemSize(const Value: Single);
procedure SetItemSpacing(const Value: Single);
procedure ApplySelectionVisual(AShape: TShape);
procedure ClearSelectionVisual;
procedure SwatchMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
function CalcLuminance(Color: TAlphaColor): Single;
procedure Resize; override;
procedure Loaded; override;
procedure SetColors(const AColors: array of TAlphaColor);
procedure AddColor(AColor: TAlphaColor);
procedure RemoveColorAt(Index: Integer);
procedure ClearColors;
procedure SelectColor(AColor: TAlphaColor);
procedure ClearSelection;
procedure Register;
```

## LightFmx.Visual.ComboBox (10)

```pascal
function SelectedItem: string; { Returns the selected item. Raises assertion if no item selected! Use SelectedItemSafe to avoid exceptions. }
function SelectedItemSafe: string; { Returns the selected item. Returns '' if no item selected }
function SelectedItemForce: string; { Returns the selected item. If no item is selected, selects the first item first }
function SelectedObject: TObject; { Returns the object associated with selected item, or NIL }
function SelectItem(const ItemText: string): Integer; { Select by text. Case insensitive. Returns index or -1 if not found }
function SelectFirstItem: Boolean; { Select first item. Returns True if list is not empty }
function SelectObject(AObject: TObject): Boolean; { Select by object. Returns True if found }
function SelectDualItem(const ScreenName: string): Integer; { Select by screen name (Value). Returns index or -1 }
function SelectedDualItem: string; { Returns screen name (Value) of selected dual item }
procedure Register;
```

## LightFmx.Visual.DropDownSearch (16)

```pascal
procedure showDropDown;
procedure FilterItems;
procedure HandleArrowKeys(Key: Word);
procedure endSearch (Sender: TObject);
procedure EditExit (Sender: TObject);
procedure EditTyping(Sender: TObject);
procedure SetHost;
procedure SetHeightAuto(Box: TListBox; MaxHeightPercent: Integer);
procedure KeyDown(var Key: Word; var KeyChar: Char; Shift: TShiftState); override;
procedure Click; override;
procedure PopulateDictionary(Words: TStringList);
procedure AddDemoStrings;
function SelectedString: string;
function SelectedObject: TObject;
function WordCount: Integer;
procedure Register;
```

## LightFmx.Visual.LabeledEdit (8)

```pascal
function GetLabelText: string;
procedure SetLabelText(const Value: string);
function GetEditText: string;
procedure SetEditText(const Value: string);
function GetEdit: TEdit;
function GetLabel: TLabel;
procedure Resize; override;
procedure Register;
```

## LightFmx.Visual.Layout (5)

```pascal
procedure Resize; override;
procedure SetVisibleAtRuntime(const Value: Boolean);
procedure Loaded; override;
procedure DoRealign; override;
procedure Register;
```

## LightFmx.Visual.LayoutResponsive (12)

```pascal
procedure SetMaxWidth(const Value: Single);
procedure SetLabelWidth(const Value: Single);
procedure SetRowHeight(const Value: Single);
procedure DoRealign; override;
procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
procedure SetBreakWidth(const Value: Single);
procedure SetLabelWidth(const Value: Single);
procedure SetRowHeight(const Value: Single);
procedure SetGap(const Value: Single);
procedure DoRealign; override;
procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
procedure Register;
```

## LightFmx.Visual.Panel (3)

```pascal
procedure SetVisibleAtRuntime(const Value: Boolean);
procedure Loaded; override;
procedure Register;
```

## LightFmx.Visual.PopupList (7)

```pascal
procedure ListBoxChange (Sender: TObject);
procedure PopupClosed (Sender: TObject);
procedure ListBoxMouseMove (Sender: TObject; Shift: TShiftState; X, Y: Single);
procedure ListBoxMouseLeave(Sender: TObject);
procedure ClearHover;
function AddItem(const Text: string): TListBoxItem;
procedure Open;
```

## LightFmx.Visual.ScreenCapture (5)

```pascal
procedure LoadLastSelection;
procedure SaveLastSelection;
procedure StartCapture;
function CaptureSelectedArea(CONST SelectionRect: TRectF; ScaleX: Single = 1.0; ScaleY: Single = 1.0): Boolean;
function GetCapturedImages: TObjectList<FMX.Graphics.TBitmap>;
```

## LightFmx.Visual.SearchListBox (17)

```pascal
function GetSearchText: string;
procedure SetSearchText(const Value: string);
function GetSelectedIndex: Integer;
procedure SetSelectedIndex(const Value: Integer);
function GetSelectedText: string;
function GetSelectedTag: Integer;
function GetItemCount: Integer;
procedure SearchEditChangeTracking(Sender: TObject);
procedure ListBoxChange(Sender: TObject);
procedure FilterItems;
procedure Resize; override;
procedure Loaded; override;
procedure SetItems(const Items: array of string);
procedure AddItem(const Text: string; Tag: Integer = 0);
procedure Clear;
function SelectByTag(Tag: Integer): Boolean;
procedure Register;
```

## LightFmx.Visual.SpinBox (14)

```pascal
function GetLabelText: string;
procedure SetLabelText(const Value: string);
function GetValue: Double;
procedure SetValue(const Value: Double);
function GetMax: Double;
procedure SetMax(const Value: Double);
function GetMin: Double;
procedure SetMin(const Value: Double);
function GetIncrement: Double;
procedure SetIncrement(const Value: Double);
function GetSpinBox: TSpinBox;
function GetLabel: TLabel;
procedure Resize; override;
procedure Register;
```

## LightFmx.Visual.SvgButton (2)

```pascal
procedure AttachSvgToButton(Button: TCustomButton; CONST SvgPathData: string; IconSize: Single = 20);
procedure ToggleSvgIcon(Button: TCustomButton; IconOnly: Boolean);
```

## LightFmx.Visual.SvgFlatButton (28)

```pascal
procedure DeferredEvaluateAutoCompact(Sender: TObject);
function GetText: string;
procedure SetText(CONST Value: string);
function GetSvgData: string;
procedure SetSvgData(CONST Value: string);
procedure SetIconPosition(Value: TIconPosition);
procedure SetIsToggled(Value: Boolean);
procedure SetHoverBackground(Value: Boolean);
procedure ApplyColors; { Applies all visual changes based on current hover/toggle state }
procedure UpdateIconSize; { Recalculates icon dimensions from button size and position mode }
procedure HandleStyleChanged(const Sender: TObject; const M: System.Messaging.TMessage);
procedure SetCompact(Value: Boolean);
procedure SetAutoCompact(Value: Boolean);
procedure SetCompactThreshold(Value: TCompactThreshold);
procedure ApplyCompact(MakeCompact: Boolean);
procedure EvaluateAutoCompact;
procedure HandleFormResize(const Sender: TObject; const M: System.Messaging.TMessage);
procedure Loaded; override;
procedure Resize; override;
procedure Paint; override;
procedure DoMouseEnter; override; { Triggers highlight on mouse hover }
procedure DoMouseLeave; override; { Reverts to normal colors }
procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
procedure MouseUp (Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
procedure LoadSvgPath(CONST SvgPathData: string);
procedure ApplyThemeColors; { Re-reads colors from the active skin. Call after style changes. }
procedure CompactSvgButtons(Parent: TFmxObject; Compact: Boolean);
procedure Register;
```

## LightVcl.Common.CenterControl (12)

```pascal
procedure CorrectFormPositionDesktop(Form: TForm);
procedure CorrectFormPositionMainMonitor(Form: TForm); deprecated 'Use Form.MakeFullyVisible(Screen.PrimaryMonitor)';
procedure CorrectFormPositionMonitor(Form: TForm; Monitor: TMonitor); deprecated 'Use Form.MakeFullyVisible(Monitor)';
procedure CenterForm(Form: TForm); overload;
procedure CenterForm(Form, Parent: TForm); overload;
procedure CenterFormOnMainFormMonitor(Form: TForm);
procedure CorrectMDIFormPosition(ParentForm: TForm);
Procedure CorrectCtrlPosition(Ctrl: TControl; CONST ParentWidth, ParentHeight: Integer); overload;
procedure CorrectCtrlPosition(Ctrl, Parent: TControl); overload;
procedure CenterInvalidChild(Ctrl, Parent: TControl);
procedure CenterChild(Ctrl, Parent: TControl);
procedure CenterChildX(Ctrl, Parent: TControl);
```

## LightVcl.Common.Clipboard (3)

```pascal
function StringToClipboard (CONST Str: string; CONST MaxRetries: Integer= 20): Boolean; { Returns True if it succeeded in writing to the clipboard }
function StringFromClipboard (CONST MaxWaitTime: Cardinal= 5000): string; { Returns clipboard text or empty string if unavailable/timeout }
function StringFromClipboardTSL(CONST MaxWaitTime: Cardinal= 5000): TStringList; { Returns NIL if clipboard has no text; caller must free result }
```

## LightVcl.Common.CpuMonitor (5)

```pascal
function Usage: Integer; { Collect a sample and return current CPU usage (0..100). Returns -1 on API error. }
function Average: Integer; { Rolling average of last CPU_HISTORY_SIZE samples (0..100). Returns 0 if no data. }
procedure OnTimer(Sender: TObject);
procedure Reset;
function CpuIsBusy: Boolean; { Returns TRUE when CPU >= HighCpuThreshold or on battery. Meaning: wallpaper change should be skipped. }
```

## LightVcl.Common.CpuUsageProcess (3)

```pascal
function FileTimeToInt64(CONST FT: TFileTime): Int64;
function CoreUsage: Single; { Per-core usage (0..100). One core at max on 4-core CPU returns 100% }
function CpuUsage: Single; { Total CPU usage (0..100). One core at max on 4-core CPU returns 25% }
```

## LightVcl.Common.Debugger (10)

```pascal
procedure AntiDebug; assembler;
procedure AntiProcDump; assembler;
procedure ExitIfUnderDebugger (ProjectFileName: string);
procedure HaltApplication(UserMessage : string);
function IsDebuggerPresent: Boolean;
procedure OutputDebugStr (s: string); overload;
procedure OutputDebugStr (s: string; i: Integer); overload;
procedure OutputDebugStr (s: string; r: Real); overload;
function LastErrorMsgStr: string;
function FixEmbarcaderoAtomTableMemLeak: Boolean; Deprecated 'Use in-program leak fixing: 3rdPartyPkg.AtomGarbageCollector.pas.GarbageCollectAtoms(Log)'
```

## LightVcl.Common.Dialogs (7)

```pascal
function MesajGeneric (CONST MessageText: string; Title: string= ''; Icon: Integer= -1): Integer;
procedure MessageInfo (CONST MessageText: string; CONST Title: string= '');
procedure MessageWarning (CONST MessageText: string; CONST Title: string= '');
procedure MessageError (CONST MessageText: string; CONST Title: string= '');
procedure MesajErrDetail (CONST MessageText, Where: string);
function MesajYesNo (CONST MessageText: string; CONST Title: string= ''): Boolean;
procedure MesajTaskDlg (CONST MessageText, Title: string);
```

## LightVcl.Common.EllipsisText (7)

```pascal
function GetAverageCharSize(Canvas: TCanvas): TPoint;
function ShortenString (CONST s: String; MaxLength: Integer): String; { Replace long text with ellipsis. Also exists: FileCtrl.MinimizeName }
function DrawStringEllipsis (CONST s: string; Canvas: TCanvas; ARect: TRect): integer; overload;
function DrawStringEllipsis (CONST s: string; Canvas: TCanvas): integer; overload;
function GetEllipsisText (CONST s: String; Canvas: TCanvas; MaxWidth, MaxHeight: Integer; Font: TFont = nil; PathEllipsis: Boolean = False; TextFormat: TTextFormat = []): String; overload;
function GetEllipsisText (CONST s: String; Handle: HDC; MaxWidth, MaxHeight: Integer; PathEllipsis: Boolean = False; TextFormat: LongWord = 0): string; overload;
function GetEllipsisText (CONST s: string; Canvas: TCanvas; MaxWidth: Integer): string; overload;
```

## LightVcl.Common.EnvironmentVar (4)

```pascal
function ExpandEnvironmentStrings(CONST Vars: string): string;
function SetEnvironmentVars(CONST Name, Value: string; User: Boolean = True): Boolean;
function GetEnvironmentVars(CONST Name: string; User: Boolean = True): string; overload;
function GetEnvironmentVars(TSL: TStrings): Boolean; overload;
```

## LightVcl.Common.ExecuteProc (5)

```pascal
function ExecuteProcMsg(ExeFile: string): Boolean;
function ExecuteProc(ExeFile: string; WindowState: Integer = SW_SHOWNORMAL): Boolean;
function ExecuteAndWait(ExeFile: string; Params: string = ''; Hide: Boolean = FALSE; WaitTime: Cardinal = INFINITE): Cardinal;
function ExecuteAndGetOut(CONST CmdLine: string; Work: string = 'C:\'): string;
procedure ExecuteAndGetOutDyn(CONST CmdLine: string; CONST Output: TProc<string>;
```

## LightVcl.Common.ExecuteShell (9)

```pascal
function ExecuteFile(CONST ExeFile: string; Params: string = '';
function ExecuteFileEx(CONST ExeFile: string; Params: string = '';
function ExecuteFileAndWait(CONST ExeFile: string; Params: string = '';
function ExecuteAsAdmin(CONST ExeFile: string; Params: string = ''; hWnd: HWND = 0): Boolean;
procedure ExecuteURL(URL: string);
procedure ExecuteSendEmail(EmailAddress: string);
procedure ExecuteExplorer(Path: string);
function ExecuteExplorerSelect(FileName: string): Boolean;
procedure ExecuteControlPanel_ScreenRes;
```

## LightVcl.Common.ExeVersion (2)

```pascal
function GetVersionInfoFile(CONST FileName: string; OUT FixedInfo: TVSFixedFileInfo): Boolean;
function GetVersionInfo(CONST FileName: string; ShowBuildNo: Boolean = False): string;
```

## LightVcl.Common.GuiSettings (3)

```pascal
procedure Load;
procedure Save;
function GetSettingsPath: string;
```

## LightVcl.Common.IniFile (15)

```pascal
procedure readCtrlPos (Ctrl: TControl);
procedure writeCtrlPos (Ctrl: TControl);
procedure writeSplitter(Comp: TComponent);
procedure readSplitter (Comp: TComponent);
function IsSupported(WinCtrl: TComponent): Boolean; virtual;
procedure SaveForm (Form: TForm; AutoState: TAutoState = asPosOnly); { Save ALL supported controls on this form }
procedure LoadForm (Form: TForm; AutoState: TAutoState = asPosOnly);
function Read (CONST Ident: string; Font: TFont): Boolean; overload;
procedure Write (CONST Ident: string; Font: TFont); overload;
function ReadColor (CONST Ident: string; Default: TColor): TColor;
procedure WriteColor (CONST Ident: string; Value: TColor);
function WriteComp (Comp: TComponent): Boolean; virtual;
function ReadComp (Comp: TComponent): Boolean; virtual;
procedure ReadGroup (WinCtrl: TWinControl);
procedure WriteGroup (WinCtrl: TWinControl);
```

## LightVcl.Common.IO (47)

```pascal
function DirectoryExistMsg (CONST Path: string): Boolean;
function FileExistsMsg (CONST FileName: string): Boolean;
function ForceDirectoriesMsg (CONST FullPath: string): Boolean; { Wrapper around LightCore.IO.ForceDirectories. Returns True if directory exists or was created, False on failure. Shows error dialog on failure. }
procedure MoveFolderMsg (CONST FromFolder, ToFolder: String; SilentOverwrite: Boolean);
function DeleteFileWithMsg (CONST FileName: string): Boolean;
function GetProgramFilesDir : string;
function GetDesktopFolder : string;
function GetStartMenuFolder : string;
function GetMyDocumentsAPI : string; deprecated 'Use GetMyDocuments instead';
function GetMyPicturesAPI : string; deprecated 'Use GetMyPictures instead';
function GetWinSysDir : string;
function GetWinDir : string; { Returns Windows folder }
function GetTaskManager : string;
function GetSpecialFolder (CONST OS_SpecialFolder: string): string; overload; { SHELL FOLDERS. Retrieving the entire list of default shell folders from registry }
function GetSpecialFolder (CSIDL: Integer; ForceFolder: Boolean = FALSE): string; overload; { uses SHFolder }
function GetSpecialFolders: TStringList; { Get a list of ALL special folders. }
function FolderIsSpecial (CONST Path: string): Boolean; { Returns True if the parameter is a special folder such us 'c:\My Documents' }
function GetPosAfterExtendedPrefix(CONST Path: string): Integer;
function SelectAFolder (VAR Folder: string; CONST Title: string = ''; CONST Options: TFileDialogOptions= [fdoPickFolders, fdoForceFileSystem, fdoPathMustExist, fdoDefaultNoMiniMode]): Boolean; overload;
function PromptToSaveFile (VAR FileName: string; CONST Filter: string = ''; CONST DefaultExt: string= ''; CONST Title: string= ''): Boolean;
function PromptToLoadFile (VAR FileName: string; CONST Filter: string = ''; CONST Title: string= ''): Boolean;
function PromptForFileName(VAR FileName: string; SaveDialog: Boolean; CONST Filter: string = ''; CONST DefaultExt: string= ''; CONST Title: string= ''; CONST InitialDir: string = ''): Boolean;
function GetSaveDialog (CONST FileName, Filter, DefaultExt: string; CONST Caption: string= ''): TSaveDialog;
function GetOpenDialog (CONST FileName, Filter, DefaultExt: string; CONST Caption: string= ''): TOpenDialog;
function RecycleItem (CONST ItemName: string; CONST DeleteToRecycle: Boolean= TRUE; CONST ShowConfirm: Boolean= TRUE; CONST TotalSilence: Boolean= FALSE): Boolean;
function FileOperation (CONST Source, Dest: string; Op, Flags: Integer): Boolean; { Performs: Copy, Move, Delete, Rename on files + folders via WinAPI}
function FileMoveTo (CONST From_FullPath, To_FullPath: string): Boolean; { Moves a file to a new location, overwriting if exists }
function FileMoveToDir (CONST From_FullPath, To_DestFolder: string; Overwrite: Boolean): Boolean; { Moves a file to a destination folder }
function FileAge (CONST FileName: string): TDateTime;
function FileTimeToDateTimeStr (FTime: TFileTime; CONST DFormat, TFormat: string): string;
procedure SetCompressionAtr (CONST FileName: string; const CompressionFormat: byte= 1);
function GetFileSizeEx (hFile: THandle; VAR FileSize: Int64): BOOL; stdcall; external kernel32;
function FileIsLockedR (CONST FileName: string): Boolean;
function FileIsLockedRW (CONST FileName: string): Boolean; { Returns true if the file cannot be open for reading and writing } { old name: FileInUse }
function CanCreateFile (CONST FileName: string): Boolean;
function CanWriteToFolder (CONST Folder: string; const FileName: String = 'TempFile.Delete.Me'): Boolean; { Tests folder write access by creating a temporary file. The temp file is auto-deleted on close. WARNING: If FileName already exists, it WILL be overwritten! }
function CanWriteToFolderMsg (CONST Folder: string): Boolean;
function GetDriveType (CONST Path: string): Integer;
function GetDriveTypeS (CONST Path: string): string; { Returns drive type asstring }
function GetVolumeLabel (CONST Drive: Char): string; { Returns volume label of a disk }
function DiskInDrive (CONST Path: string): Boolean; overload; { From www.gnomehome.demon.nl/uddf/pages/disk.htm#disk0 . Also see http://community.borland.com/article/0,1410,15921,00.html }
function DiskInDrive (CONST DriveNo: Byte): Boolean; overload; { THIS IS VERY SLOW IF THE DISK IS NOT IN DRIVE! The GUI will freeze until the drive responds. }
function ValidDrive (CONST Drive: Char): Boolean; { Peter Below (TeamB). http://www.codinggroups.com/borland-public-delphi-rtl-win32/7618-windows-no-disk-error.html }
function PathHasValidColon (const Path: string): Boolean;
function DriveFreeSpace (CONST Drive: Char): Int64;
function DriveFreeSpaceS (CONST Drive: Char): string;
function DriveFreeSpaceF (CONST FullPath: string): Int64; { Same as DriveFreeSpace but this accepts a full filename/directory path. It will automatically extract the drive }
```

## LightVcl.Common.Keyboard (10)

```pascal
procedure SimulateKeyDown (Key : byte);
procedure SimulateKeyUp (Key : byte);
procedure SimulateKeystroke(Key: byte; extra: Cardinal);
procedure SendKeys(s: string);
procedure SendText(text: string); { Set the focus to a control and send it a string}
procedure CapsLock; { Toggle the 'CAP Lock'}
function GetModifierKeyState: TShiftState;
function IsCtrlDown : Boolean;
function IsShiftDown: Boolean;
function IsAltDown : Boolean;
```

## LightVcl.Common.KeybShortcuts (6)

```pascal
procedure ShowConflicts;
function ShiftState2Modifier(CONST Shift: TShiftState): Word;
function GetShortCutKey (ShortCut: TShortCut):Word;
function GetShortCutModifier(ShortCut: TShortCut):Word;
function RegisterHotShortCut(CONST hHandle: THandle; CONST Atom: integer; CONST ShortCut: TShortCut):Boolean;
procedure ShowShortcutsInButtons(aForm: TForm); // Show keyboard shortcuts for all buttons in a form that have an action assigned to them
```

## LightVcl.Common.LogFilter (7)

```pascal
function getVerbosity: TLogVerbLvl; //Note: The "master" of the verbosity events is the Grid not the trackbar
procedure setVerbosity (Value: TLogVerbLvl);
procedure setLog (Value: TLogViewer);
procedure setShowDebugMsg(Value: Boolean);
procedure CreateWnd; override;
procedure TrackBarChange(Sender: TObject);
procedure Register;
```

## LightVcl.Common.LogViewer (27)

```pascal
function CalculateColWidth(const ATextLength: Integer; const ACaption: string): Integer;
procedure setShowDate(const Value: Boolean);
procedure setShowTime(const Value: Boolean);
procedure FixFixedRow;
procedure resizeColumns;
procedure scrollToBottom;
procedure setVerbFilter(const Value: TLogVerbLvl);
function getLineFiltered(Row: Integer): PLogLine;
procedure scrollBarChange(Sender: TObject);
procedure refreshVisibleSlice;
procedure CreateWnd; override;
procedure Resize; override;
procedure setUpRows;
procedure GridDrawCell(Sender: TObject; ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
procedure AssignExternalRamLog(ExternalLog: TRamLog);
procedure Clear;
procedure RegisterVerbFilter(TrackBar: TPanel{TLogVerbFilter});
procedure Populate;
procedure PopUpWindow;
procedure ChangeScrollBarVisibility(aVisible: boolean);
procedure SaveAsRtf(const FullPath: string);
function Count: Integer;
procedure CopyAll;
procedure CopyVisible;
procedure CopyCurLine;
function Verbosity2Color (Verbosity: TLogVerbLvl; IsDark: Boolean = False): TColor;
procedure Register;
```

## LightVcl.Common.MemoInputBox (1)

```pascal
function MemoInputQuery(const aCaption, aPrompt: string; VAR Value: string; CONST xScale: Integer= 2): Boolean; { The smaller xScale the larger the width of the window }
```

## LightVcl.Common.MutexSingleInstance (3)

```pascal
procedure FreeMutex;
function IsSingleInstance(CONST MutexName: string): Boolean;
procedure CreateMutexOrDie(CONST MutexName: string); { Not recommended - calls Application.Terminate }
```

## LightVcl.Common.PopUp (1)

```pascal
procedure Popup(X, Y: Integer); override;
```

## LightVcl.Common.PowerUtils (18)

```pascal
function SystemSleep (ForceCritical: Boolean= FALSE): Boolean;
function SystemHibernate(ForceCritical: Boolean= FALSE): Boolean;
function SetSuspendState(Hibernate, ForceCritical, DisableWakeEvent: Boolean): Boolean; stdcall; external 'powrprof.dll' name 'SetSuspendState';
function WinExit (Flags: integer): boolean; // Shut down, restart or logs off Windows
function WinShutDown (Force, Reboot: Boolean): Boolean; overload;
procedure WinShutDown; overload;
function IsHibernateAllowed : Boolean; stdcall; external 'powrprof.dll' name 'IsPwrHibernateAllowed'; // Does not work on WinNT 4.0 or Win95.
function IsPwrSuspendAllowed : Boolean; stdcall; external 'powrprof.dll' name 'IsPwrSuspendAllowed';
function IsPwrShutdownAllowed: Boolean; stdcall; external 'powrprof.dll' name 'IsPwrShutdownAllowed';
function InitSystemShutdown(CONST ComputerName: WideString; Reboot, Force: Boolean; const Msg: string; TimeOut: Cardinal=0): Boolean; // Shut down, restart a machine with showing an optional warning message.
function PowerStatus: TPowerType;
function PowerStatusString: string;
function BatteryLeft : Integer;
function BatteryAsText: string;
procedure MonitorsOff;
procedure MonitorsSleep;
function TurnScreenSaverOn: Boolean;
function IsScreenSaverOn: Boolean;
```

## LightVcl.Common.Process (2)

```pascal
function ProcessRunning (CONST ExeFileName: string): Boolean;
function KillProcess (CONST ExeName: string): Boolean;
```

## LightVcl.Common.ProcessCpuMonitor (8)

```pascal
function FindPrevSample(PID: DWORD; CONST Name: string): Integer;
function FindSmoothed(CONST Name: string): Integer;
procedure UpdateSmoothed(CONST Name: string; RawCpu: Single);
procedure DecayUnseen(CONST Agg: array of RAggregated; AggCount: Integer);
procedure PruneSmoothed;
procedure Sample;
function GetTopCpu(Index: Integer): RProcessInfo;
function GetTopRam(Index: Integer): RProcessInfo;
```

## LightVcl.Common.Registry (23)

```pascal
function Convert_HKey2Str (CONST Key: HKEY): string;
function Convert_Str2HKey (CONST Key: string): HKEY;
function RegKeyExist (CONST Root: HKEY; CONST Key: string): Boolean;
function RegValueExist (CONST Root: HKEY; CONST Key, ValueName: string) : Boolean;
function RegHasSubKeys (CONST Root: HKEY; CONST Key: string): Boolean;
function RegDeleteKey (CONST Root: HKEY; CONST Key: string): Boolean;
function RegClearKey (CONST Root: HKEY; CONST Key: string): Boolean; { Deletes all value/name pairs inside the key but don't delete the key }
function RegDeleteValue (CONST Root: HKEY; CONST Key, ValueName: string): Boolean;
function RegWriteString (CONST Root: HKEY; CONST Key, ValueName, ValueData: string; Lazy: Boolean= TRUE): Boolean; { Writes a string to the specified key in the registry }
function RegWriteInteger (CONST Root: HKEY; CONST Key, ValueName: string; ValueData: Integer; Lazy: Boolean= TRUE): Boolean; { Writes a integer to the specified key in the registry }
function RegWriteBool (CONST Root: HKEY; CONST Key, ValueName: string; ValueData: Boolean; Lazy: Boolean= TRUE): Boolean;
function RegWriteDate (CONST Root: HKEY; CONST Key, ValueName: string; ValueData: TDate; Lazy: Boolean= TRUE): Boolean;
function RegReadDate (CONST Root: HKEY; CONST Key, ValueName: string; CanCreate: Boolean= FALSE): TDateTime;
function RegReadBool (CONST Root: HKEY; CONST Key, ValueName: string; DefValData: Boolean= FALSE): Boolean;
function RegReadInteger (CONST Root: HKEY; CONST Key, ValueName: string; DefValData: Integer= 0): Integer;
function RegReadString (CONST Root: HKEY; CONST Key, ValueName: string; CONST DefValData: string= ''): string;
function RegEnumSubKeys (CONST Root: HKEY; const Key: string): TStringList; { ! }
function RegWriteValuePairs (CONST Root: HKEY; CONST Key: string; Pairs : TStringList; CONST Delimiter: char; Lazy : Boolean= TRUE) : Boolean;
function RegReadValuePairs (CONST Root: HKEY; CONST Key: string; Pairs : TStringList; CanCreate: Boolean= FALSE): Boolean; { Enumerate all name/values pairs contained in the specified key }
function RegReadValueNames (CONST Root: HKEY; CONST Key: string; ValueNames: TStringList; CanCreate: Boolean= FALSE): Boolean; { Returns all values contained in the specified key } //I think the old name was RegReadNames
function RegReadValueDatas (CONST Root: HKEY; CONST Key: string; ValueDatas: TStringList; CanCreate: Boolean= FALSE): Boolean; { Returns all keys contained in the specified path }
function RegReadMultiSzString (CONST Root: HKEY; CONST Key, KeyName: string; CanCreate: Boolean= FALSE): string; { Reads a REG_MULTI_SZ value From the Registry. This will return strings separated by ENTER }
function RegReadMultiSzStringSP(CONST Root: HKEY; CONST Key, KeyName: string; CanCreate: Boolean= FALSE): string; { This will return strings separated by SPACE }
```

## LightVcl.Common.Reports (7)

```pascal
function GenerateVCLReport: string;
function GenerateWinSysRep: string;
function GenerateWinPathRep: string;
function GenerateWinPathRepEx: string;
function GenerateHardwareRep: string;
function GenerateHardwareRepTSL: TStringList;
function ScreenResApi: string;
```

## LightVcl.Common.Shell (25)

```pascal
function GetAssociatedApp (const FileExtension: string): string; // Old name: AplicatieAsociata
function AssociateWith (CONST FileExtension, AsociationName: string; CONST ForAllUsers: Boolean= FALSE; ShowError: Boolean= FALSE; Notify: Boolean= TRUE): Boolean; { Associate a application with an extension. EXAMPLE: FileExtension:= '.txt' / AsociationName:= 'Metapad' } { EXAMPLE: FileExtension:= '.txt' / AsociationName:= 'Metapad' / ShowError show an error messajge is the program cannot write to registry }
function AssociationReset (CONST FileExtension: string; CONST ForAllUsers: Boolean): Boolean;
procedure AssociateSelf_ShellMenu (CONST ShowError: Boolean); //Old name: AssociateWithShell
function AddContextMenu (CONST CommandName, Extensions: string): Boolean; overload;
procedure AddContextMenu (CONST GUID: TGUID; CONST ShellExtDll, FileExt, UtilityName: string); overload;
procedure RemoveContextMenu(CONST GUID: TGUID; CONST FileExt, UtilityName: string);
procedure InvokePropertiesDialog(CONST FileName: string); { Shows the standard file properties dialog like in Windows Explorer }
procedure InvokeStartMenu(Form: TForm); { Activate Windows Start button from code }
procedure ShowTaskBar(ShowIt: Boolean);
procedure AddFile2TaskbarMRU(FileName: string); { Add the file to 'recent open files' menu that appears when right clicking on program's button in TaskBar }
function IsTaskbarAutoHideOn : Boolean;
procedure CreateShortcut (CONST ShortCutName: string; OnDesktop: Boolean);
procedure CreateShortcutEx (CONST ShortCutName, ShortcutTo: string; OnDesktop: Boolean); { Full parameters }
procedure CreateShortcut_SendTo (CONST ShortcutName : string); { Add your application in the "Send To" menu and processing the file http://delphi.about.com/od/adptips2006/qt/app2sendtomenu.htm }
function DeleteDesktopShortcut (CONST ShortcutName: string): Boolean;
function DeleteStartMenuShortcut (CONST ShortcutName: string): Boolean;
function ExtractPathFromLnkFile (CONST LnkFile: WideString): string;
function RestoreOriginalSCF_Association: Boolean; { Make 'Show Desktop' icon in Quick Launch to work again (to show desktop) }
function RemoveShowDesktopFile: Boolean;
procedure RestoreShowDesktopFile;
function InstallINF (CONST PathName: string; hParent: HWND): Boolean; { Example: InstallINF('C:\driver.inf', 0) }
procedure AddUninstaller(CONST UninstallerExePath, ProductName: string); { Uninstaller= Full path to the EXE file that represents the uninstaller; ProductName= the uninstaller will be listed with this name. Keep it simple without special chars like '\'. Example: 'BioniX Wallpaper' }
function IsApiFunctionAvailable(const DLLname, FuncName: string; VAR p: pointer): Boolean; { Returns True if FuncName exists in DLLname }
function ExtractIconFromFile(IcoFileName: String): THandle; { Extract icon from file }
```

## LightVcl.Common.Sound (14)

```pascal
procedure PlayWinSound (CONST SystemSoundName: string);
procedure PlaySoundFile(CONST FileName: string);
procedure PlayResSound (CONST ResName: String; uFlags: Integer);
procedure PlayTone(Frequency, Duration: Integer; Volume: Byte); { Writes tone to memory and plays it } // Old name: MakeSound
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
```

## LightVcl.Common.System (22)

```pascal
function ServiceStart (CONST aMachine, aServiceName: string): Boolean;
function ServiceStop (CONST aMachine, aServiceName: string): Boolean;
function ServiceGetStatus (CONST sMachine, sService: string): DWord;
function ServiceGetStatusName(CONST sMachine, sService: string): string;
function InstallFont(CONST FontFileName: string): Boolean;
procedure UseUninstalledFont(CONST FontFile: string); { Use a font without installing it. DON'T FORGET TO RELEASE IT when you close the program }
procedure FreeUninstalledFont(CONST FontFile: string);
function GetComputerName: string;
function GetHostName : string;
function GetLogonName : string;
function GetDomainName : String; { for home users it shows the computer name (Qosmio) }
function GetUserName (AllowExceptions: Boolean = False): string; // NOT TESTED
function GetUserNameEx (ANameFormat: Cardinal): string; { source http://stackoverflow.com/questions/8446940/how-to-get-fully-qualified-domain-name-on-windows-in-delphi }
function GetDisplayModes: string; { Returns the resolutions supported }
procedure PrintScreenActiveWnd;
procedure PrintScreenFull;
procedure JiggleMouse;
procedure CursorBusy;
procedure CursorNotBusy;
function GetWin32ErrorString(ErrorCode: DWORD): string;
function BiosDate: string;
function BiosID: string;
```

## LightVcl.Common.SystemConsole (1)

```pascal
procedure SetConsoleColor(AColor: TColor);
```

## LightVcl.Common.SystemPermissions (6)

```pascal
function AppElevationLevel: Integer;
function AppHasAdminRights: Boolean;
function SetPrivilege(CONST PrivilegeName: string; bEnabled : Boolean): Boolean;
function IsUserAdmin: Boolean;
function CurrentUserHasAdminRights: Boolean;
function OsHasNTSecurity: Boolean;
```

## LightVcl.Common.SystemSecurity (2)

```pascal
procedure Hack_DisableSystemKeys(Disable: Boolean);
function CalculatePasswordStrength(const Password: string): Integer;
```

## LightVcl.Common.SystemTime (7)

```pascal
procedure DelayEx(CONST ms : cardinal);
function GetSysFileTime: TDateTime; { gets current date from system file - prevents cracking}
function SystemTimeIsInvalid: Boolean; { returns true if the system time is bigger than current clock time }
procedure CurrentSysTimeStore(CONST SecretKey: string); { Stores current system clock time to a hidden registry key }
function CurrentSysTimeValid(CONST SecretKey: string): Boolean; { Read the last saved system time and compares it with current clock. If current value is smaller than the stored value it means that the clock time was set back }
function WindowsUpTime: TDateTime;
function UserIdleTime: Cardinal;
```

## LightVcl.Common.Translate (17)

```pascal
procedure WriteComponent(Component: TComponent; CONST Section: string; Ini: TMemIniFile);
procedure ReadComponent (Component: TComponent; CONST Section: string; Ini: TMemIniFile);
procedure WriteProperty (Component: TComponent; CONST ParentName, PropertyType, Section: string; Ini: TMemIniFile);
procedure ReadProperty (Component: TComponent; CONST ParentName, PropertyType, Section: string; Ini: TMemIniFile);
procedure setCurLanguage(const Value: string);
function getCurLanguage: string;
procedure saveFormTranslation (Form: TForm; Ini: TMemIniFile);
function ReadString(CONST Identifier, DefaultVal: string): string;
procedure WriteString(const Identifier, s: string);
procedure LoadDefaultTranslation;
procedure LoadLastTranslation;
procedure LoadTranslation (Form: TForm; ForceLoad: Boolean= FALSE); overload;
procedure LoadTranslation (ForceLoad: Boolean= FALSE); overload;
procedure SaveTranslation (CONST FileName: string; Overwrite: Boolean= TRUE);
function DefaultLang : string; // Returns 'English.ini'
function GetLangFolder : string;
function trs(CONST s: string): string; //ToDo: For strings use Delphi's built in facility
```

## LightVcl.Common.VclUtils (28)

```pascal
procedure MenuVisibility (Item: TMenuItem; Enabled, Visible: Boolean);
procedure SetChildVisibility(ParentMenu: TMenuItem; Enabled, Visible: Boolean); overload; { Change the visibility for all children of ParentMenu }
procedure SetChildVisibility(ParentMenu: TMenuItem; Visible: Boolean); overload;
function AddSubMenu (ParentMenu: TMenuItem; CONST Caption: string; Event: TNotifyEvent): TMenuItem; { Add a sub-menu item to a menu item. Also returns a pointer to that menu. I don't have to free it. The owner will free it. }
procedure RemoveSubmenus (ParentMenu: TMenuItem);
function HasAction (Component: TComponent): Boolean; { Returns true if this component (TMenuItem, TButton), etc has an action assigned to it. }
procedure ActionVisibility (Item: TAction; Show: Boolean);
procedure AlignCaptionToLeft(Handle: HWND); { Align caption of the specified control to left. Example of usage: AlignCaptionToLeft(Button1.Handle)) }
procedure ScrollAppTitle (DirectionLeft: Boolean); { use it in a timer set it at 250ms }
procedure ScrollFormCaption (Form: TForm); { use it in a timer set it at 250ms }
procedure BlinkControl (Control: TControl); { Makes the specified control to blink 5 times, to attract user's attention }
function CreateControl (ControlClass: TControlClass; const ControlName: string; Parent: TWinControl; X, Y, W, H: Integer): TControl;
procedure DoubleBuffer (Control: TComponent; Enable: Boolean); { Activate/deactivate double buffering for all controls owned by the specified control. aControl can be a form, panel, box, etc }
procedure EnableDisable (Control: TWinControl; Enable: Boolean); { Enable/disable all controls in the specified control }
function FindControlAtPos (ScreenPos: TPoint): TControl;
function FindSubcontrolAtPos(Control: TControl; ScreenPos, AClientPos: TPoint): TControl;
procedure PushControlDown (BottomCtrl, TopControl: TControl); { Makes sure that BottomCtrl is under the TopControl control. Useful to set splitters under their conected controls } { old name: SetCtrlUnder }
function CanFocus (Control: TWinControl): Boolean;
procedure SetFocus (Control: TWinControl);
procedure RefreshNow(Ctrl: TControl);
function CopyControl2Bitmap (Control: TWinControl): TBitmap; { Copy the image of a VCL control (tbutton, tmemo, tcalendar) to a bitmap }
function CopyControl2Png (Control: TWinControl): TPngImage;
procedure CopyParentImage (Control: TControl; Dest: TCanvas);
function ShowComponentState (Component: TComponent): string;
function ShowControlState (Control: TControl): string;
function ShowInheritanceTree(Control: TControl): string;
function SetActivePage (PageControl: TPageControl; CONST PageName: string): TTabSheet; { Set the active tab for the specified PageControl, but instead of using an index we use a string }
procedure ToggleCheckbox (CheckBox: TCheckBox; BasedOn: TButtonControl); { Disable and uncheck CheckBox if BasedOn is checked }
```

## LightVcl.Common.Window (21)

```pascal
function IsApplicationRunning (CONST ClassName: string): Boolean;
function ForceForegroundWindow (WndHandle: HWND): Boolean; { Brings window on top of other windows. You need to call ForceRestoreWindow first }
function ForceRestoreWindow (WndHandle: HWND; Immediate: Boolean): Boolean;
function FindWindowByTitle (CONST WindowTitle: string; PartialSearch: Boolean= TRUE; CaseSens: Boolean= FALSE): Hwnd;
function FindTopWindowByClass (CONST ClassName: string): THandle;
function FindChildWindowByClass(Parent: HWnd; CONST ClassName: string): THandle; { http://www.delphipages.com/forum/showthread.php?t=6119 }
function FindChildForm (Parent: TForm; CONST ClassName: string): THandle;
function GetTextFromHandle(hWND: THandle): string;
procedure SetWindowPosToFront (WndHandle: HWND); { Set the specified windows in top of all other windows in the system }
procedure SetWindowPosToBack (WndHandle: HWND);
procedure KeepOnTop (Form: TForm; TopStyle: HWnd); overload;
procedure KeepOnTop (Handle: HWND; StayOnTop: Boolean); overload; { Not tested }
procedure MaximizeForm (Form: TForm; Maximize: Boolean); { Make form normal or maximized/ontop }
procedure MinimizeAllExcept(CONST ExceptApp: HWND);
procedure MinAllWnd_ByShell;
procedure MinAllWnd_ByShell2; { Minimize All Windows by sending a message to Shelltray }
procedure MinAllWnd_ByHandle(ApplicationWindow: HWnd); { Minimizes by iterating window handles }
procedure MinAllWnd_ByWinMKey; { Simulate Win + M }
function RestoreWindowByName (CONST ClassName: string): Boolean;
procedure RestoreWindow (WndHandle: HWND);
procedure Remove_X_Button (FormHandle: THandle);
```

## LightVcl.Common.WindowMetrics (15)

```pascal
function GetCaptionHeight (Handle: HWnd): Integer;
function GetMainMenuHeight (Handle: HWnd): Integer; { Height, in pixels, of a single-line menu bar }
function GetFrameSize (Handle: HWnd): Integer;
function GetWinBorderWidth (Handle: HWnd): Integer; { Width (pixels) of a window border }
function GetWinBorderHeight (Handle: HWnd): Integer; { Height (pixels) of a window border }
function GetWin3DBorderWidth (Handle: HWnd): Integer; { Width (pixels) of a window (with 3D look) border }
function GetWin3DBorderHeight (Handle: HWnd): Integer; { Height (pixels) of a window (with 3D look) border }
function HorizScrollBarVisible (WindowHandle: THandle): Boolean;
function VertScrollBarVisible (WindowHandle: THandle): Boolean;
function GetScrollbarSize: Integer; deprecated 'Use GetScrollBarWidth instead.'; // GetWinScrollBarWidth
procedure SetScrollbarWidth(Width: Integer);
function GetScrollBarWidth (Handle: HWnd): Integer;
function GetNumScrollLines: Integer;
procedure SetProportionalThumbV (ScrollBar: TScrollBar; OwnerClientHeight: Integer);
procedure SetProportionalThumbH (ScrollBar: TScrollBar; OwnerClientWidth : Integer);
```

## LightVcl.Common.WinVersion (16)

```pascal
function IsWindowsXP : Boolean;
function IsWindowsXPUp : Boolean;
function IsWindowsVista : Boolean;
function IsWindowsVistaUp: Boolean;
function IsWindows7 : Boolean;
function IsWindows7Up : Boolean;
function IsWindows8 : Boolean;
function IsWindows8Up : Boolean;
function IsWindows10 : Boolean;
function IsWindows10Up : Boolean;
function IsWindows11 : Boolean;
function IsWindows11Up : Boolean;
function GetOSName: string;
function GetOSDetails: string;
function IsNTKernel : Boolean;
function GenerateReport: string; { For testing }
```

## LightVcl.Common.WinVersionApi (5)

```pascal
procedure GetWinVersion (OUT MajVersion, MinVersion: Cardinal); overload;
function GetWinVersion: string; overload;
function GetWinVerNetServer: string; { Alternative to GetWinVersion }
function GetWinVersionEx: string;
function GenerateReport: string; { For testing }
```

## LightVcl.Common.WMIResolution (1)

```pascal
function GetMonitorInfoWMI: TMonitorInfo;
```

## LightVcl.Graph.Alpha (6)

```pascal
procedure AlphaBlendBitmaps (Source: TBitmap; Destination: TCanvas; DestRect: TRect; Opacity: Byte); overload;
procedure AlphaBlendBitmaps (MainBitmap, SmallBitmap: TBitmap; CONST Transparency, x, y: Integer); overload; { Transparency for SmallBitmap is between 0% and 100%. The SmallBitmap image MUST be smaller than MainBitmap. XY are the coordinates where the small image will be blend in the main image }
function TransparencyBlend (MainBitmap, SmallBitmap: TBitmap; CONST TransparentColor: TColor; CONST x, y: Integer): TBitmap; { Mix two images. Pixels of 'TransparentColor' color in the SmallBitmap are 100% transparent. The SmallBitmap image MUST be smaller than MainBitmap. XY are the coordinates where the small image will be blend in the main image }
procedure DrawTransparentBitmap(Source: TBitmap; Destination: TCanvas; DestRect: TRect; Opacity: Byte); overload; deprecated 'Call Vcl.GraphUtil.DrawTransparentBitmap directly!';
procedure DrawTransparentBitmap(Source: TBitmap; SourceRect: TRect; Destination: TCanvas; DestRect: TRect; Opacity: Byte); overload; deprecated 'Call Vcl.GraphUtil.DrawTransparentBitmap directly!';
procedure GetTransparentBitmapFromImagelist(ImageList: TImageList; Index:integer; Bitmap: TBitmap);
```

## LightVcl.Graph.Bitmap (26)

```pascal
procedure Clear; overload;
procedure Clear(CONST aName: string; aSize: Integer; aColor: TColor); overload;
procedure AssignTo(Font: TFont);
function CreateBitmap (Width, Height: Integer; PixelFormat: TPixelFormat= pf24bit): TBitmap;
function CreateBlankBitmap (Width, Height: Integer; BkgClr: TColor= clBlack; PixelFormat: TPixelFormat= pf24bit): TBitmap; // old name: GetBlankImage
procedure SetLargeSize (BMP: TBitmap; CONST Width, Height: Integer);
procedure ClearImage (Img: TImage);
procedure ClearBitmap (BMP: TBitmap);
procedure FillBitmap (BMP: TBitmap; Color: TColor);
procedure CenterBitmap (Source, InDest: TBitmap); { Center Source into Dest, without resizing any of them. The source can be bigger than destination or other way around. }
procedure EnlargeCanvas (BMP: TBitmap; NewWidth, NewHeight: Integer; CanvasFillColor: TColor); { Enlarge the canvas of the specified image and put the original image in the center of the new canvas }
procedure CenterText (BMP: TBitmap; CONST Text: string); overload;
procedure CenterText (BMP: TBitmap; CONST Text: string; aFont: RFont); overload;
procedure CenterText (BMP: TBitmap; CONST Text: string; CONST FontName: string; FontSize: Integer; FontColor: TColor); overload;
function PredictBitmapRamSize(NewWidth, NewHeight: Integer): Cardinal; overload;
function PredictBitmapRamSize(BMP: TBitmap; CONST NewWidth, NewHeight: Integer): Cardinal; overload; { Get the size of the bitmap without actually increasing its size }
function GetBitmapRamSize (BMP: TBitmap): Int64;
function IsPanoramic (BMP: TBitmap): Boolean; overload;
function IsPanoramic (Width, Height: Integer): Boolean; overload;
function GetImageScale (InputImage, DesktopSize: TBitmap; Tile: RTileParams): TImageScale; overload;
function GetImageScale (InputImage: TBitmap; DesktopWidth, DesktopHeight: Integer; Tile: RTileParams): TImageScale; overload;
function AspectIsSmaller (SrcBMP: TBitmap; CONST Width, Height: Integer): Boolean; overload; { Compare the aspect ratio of the specified image with the AR of the monitor (for example) }
function AspectIsSmaller (Width, Height, SrcWidth, SrcHeight: Integer): Boolean; overload;
function AspectOrientation (BMP: TBitmap): TImgOrientation; overload;
function AspectOrientation (Width, Height: Integer): TImgOrientation; overload;
function IsLandscape (Width, Height: Integer): Boolean;
```

## LightVcl.Graph.BkgColor (8)

```pascal
procedure FadeBorder (InpBmp, OutBMP: TBitmap; BkgClrParams: RBkgColorParams; Border: TBorderSet); { Fades the borders of BMP to the specified color. }
procedure FadeBorderAuto (InpBmp, OutBMP: TBitmap; BkgClrParams: RBkgColorParams);
procedure ApplyBorderRectOut (BMP: TBitmap; BorderSize: Integer; FrameColor: TColor; DarkenFrame: Boolean= FALSE);
procedure ApplyBorderRectIn (BMP: TBitmap; BorderSize: Integer; FrameColor: TColor);
function HasBlackBorder (BMP: TBitmap; CONST Tolerance: Byte= 5): Boolean; { Returns True if the border was black }
procedure RemoveBorder (BMP: TBitmap); { Removes the black border that surrounds an image. The returned image will have same size. The black border will be replaced with content from image (adjacent line). Only works if the border is 1 pixel wide! }
function LineIsBlack (BMP: TBitmap; RowNo: Integer; Tolerance: Byte= 3): Boolean; { Returns True if the specified line contains only black pixels }
function GetBorderDominantColor(BMP: TBitmap; Border: TBorderType; Tolerance: Integer= 8): TColor;
```

## LightVcl.Graph.BkgColorEditor (16)

```pascal
procedure FormDestroy (Sender: TObject);
procedure btnBackgroundClrClick (Sender: TObject);
procedure SettingsChanged (Sender: TObject);
procedure btnResetClick (Sender: TObject);
procedure lblExplainClick (Sender: TObject);
procedure btnApplyClick (Sender: TObject);
procedure FormCreate (Sender: TObject);
procedure btnOkClick (Sender: TObject);
procedure btnCancelClick (Sender: TObject);
procedure btnAdvancedClick (Sender: TObject);
procedure FormClose(Sender: TObject; var Action: TCloseAction);
function GetEffectColor: TEffectColor;
function GetFillType: TFillType;
function GetEffectShape: TEffectShape;
procedure ObjectFromGUI;
procedure GuiFromObject;
```

## LightVcl.Graph.BkgColorParams (3)

```pascal
procedure Reset;
procedure ReadFromStream(IOStream: TLightStream);
procedure WriteToStream (IOStream: TLightStream);
```

## LightVcl.Graph.Cache (17)

```pascal
procedure setCacheFolder (Value: string);
procedure setThumbWidth (Value: Integer);
procedure setThumbHeight (Value: Integer);
function MakeThumb(CONST FileName, ShortThumbName: string): Boolean;
function AddToCache (const FileName: string): string; { add this image into the cache and return its FULL path }
function DeleteImage (CONST FileName: string): Boolean; { Delete the original image from disk then the thumbnail from cache. Returns true if the image was deleted. Search to see if the image exists in cache before deleting it. }
function DeleteThumb (CONST ThumbShortName: string): Boolean; overload; { Delete only the thumbnail from cache. Returns true if the image was deleted. Search to see if the image exists in cache before deleting it. }
function DeleteThumb (CONST Position: integer): Boolean; overload; { Delete only the thumbnail from cache. Returns true if the image was deleted. Search to see if the image exists in cache before deleting it. }
function FormatName (iName, NameLength: Integer): string; { format the file name of the new added thumbnail so it will be 9 chars long }
procedure SaveDB; { save the cache DB to disk }
procedure LoadDB;
function GetThumbFor (CONST BigPicture: string): string; { If the specified image already has a thumbnail then return its FULL path, if not, create a thumb for it and return the path }
function ImagePosDB (FileName: string; OUT ShortThumPath: string): Integer; overload; { There is a thumnail for this image in the database? If yes, returns its position in DB }
function ImagePosDB (FileName: string): Integer; overload; { There is a thumnail for this image in the database? If yes, returns its position in DB }
function ThumbPosDB (ShortThumbName: string): Integer; { Returns the position of this thumbnail in DB }
function MaintainCache: Integer; { Delete all thumbnails from cache for which the original image does not exist anymore. Returns the number of deleted thumbnails }
procedure ClearCache; { Clears the cache. Deletes all thumbnails and resets the database }
```

## LightVcl.Graph.Convert (8)

```pascal
function Bmp2Jpg (BMP: TBitmap; CompressFactor: Integer= DelphiJpgQuality): TJpegImage; overload;
procedure Bmp2Jpg (BMP: TBitmap; OutputFile: string; CompressFactor: Integer= DelphiJpgQuality); overload;
procedure Graph2Jpg (Graph: TGraphic; OutputFile: string; CompressFactor: Integer= DelphiJpgQuality);
function Jpeg2Bmp (JPG: TJpegImage): TBitmap;
function Bmp2JpgStream (BMP: TBitmap; CompressFactor: Integer= DelphiJpgQuality): TStream;
function CompressBmp (BMP: TBitmap; CompressFactor: Integer= DelphiJpgQuality): Integer;
function Recompress (Input: TJPEGImage; OUT Outp: TJPEGImage; CompressFactor: Integer= DelphiJpgQuality): Integer; overload;
function Recompress (Jpg: TJPEGImage; CompressFactor: Integer= DelphiJpgQuality): Integer; overload;
```

## LightVcl.Graph.Desktop (21)

```pascal
procedure SysForceTileWallpaper; { Force Windows wallpaper style to Tile if the user has more than one monitor. If the style is not 'Tile' then the wallpaper will not be centered properly on dual monitor systems }
procedure SetDesktopWallpaperStyle(Style: MSWallStyle; Lazy: Boolean= true);
procedure ShowDesktopIcons (CONST Show: Boolean);
procedure BlankDesktop(Logo: string= ''; bkgColor: TColor= clBlack); { It will blank all monitors. The blank is permanent (cur wallpaper is 'lost') }
procedure RefreshDesktopQuick;
procedure RefreshDesktop; { Forces Windows to fully redrawn the wallpaper. Useful to call after the user changes the wallpaper style. }
function GetShellWindow: HWND;
function ProgManHandle : HWND;
function GetDesktopHandle: HWND;
function PaintOverIcons(X,Y : Integer; Bmp : TBitmap): Boolean; { Paints over icons on Win7 and under icons in Win 8+ } // old name: DrawOnDesktop
procedure WriteTextOnDesktopOver (x, y: integer; Text: string; FontName: string; Size: integer; Color: tcolor); { Write on desktop over icons and windows }
procedure WriteTextOnDesktopUnder(x, y: integer; Text: string); { Write on wallpaper below icons and windows }
procedure WriteTextOverAllDesktop(x, y: integer; Text: string; FontName: string; Size: integer; Color: TColor);
function SetWallpaperBroadcast(FileName: string): Boolean; { IO }
function SetWallpaper (FileName: string): Boolean; { IO }
function SetWallpaper0 (FileName: string): Boolean; { IO }
procedure SetSystemColor (PropertyToChange: Integer; Color: TColor); { Sets a color (system wide), for example the color of the window Caption. }
function GetDesktopResolutionAPI: TRect; { The simplest way is: Screen.DesktopWidth, Screen.DesktopHeight }
function GetDPI: Integer;
function DrawOnWindow (Handle: HWND; X, Y: Integer; BMP : TBitmap): Boolean;
function DrawOnWindowBitBlt (Handle: HWND; X, Y: Integer; BMP : TBitmap): Boolean; { It is not faster than the one above, without bitblt }
```

## LightVcl.Graph.FX.Gradient (4)

```pascal
procedure VistaGradient (BMP: TBitmap; const c1: Byte; const c3, c4: TColor; const Center, Reverse: Boolean); { http://rmklever.com/?tag=thumbnails }
procedure GradientFill (Control: TWinControl; Color1, Color2: TColor; Orientation: Integer= GRADIENT_FILL_RECT_V; Transparency: Word= 255); { Draws a gradient over a canvas }
procedure GradientFillCanvas(ACanvas: TCanvas; const AStartColor, AEndColor: TColor; const ARect: TRect; const Direction: TGradientDirection); deprecated 'Call Vcl.GraphUtil.GradientFillCanvas directly!';
procedure DrawRedPattern(BMP: TBitmap); { Draws a nice rectangle patern }
```

## LightVcl.Graph.FX (13)

```pascal
procedure Reset;
procedure WriteToStream (Stream: TLightStream);
procedure ReadFromStream(Stream: TLightStream);
procedure Reset;
procedure WriteToStream (Stream: TLightStream);
procedure ReadFromStream(Stream: TLightStream);
procedure CropBitmap (BMP: TBitmap; X, Y, W, H: Integer); overload; { XYWH are crop coordinates }
procedure CropBitmap (BMP: TBitmap; W, H: Integer); overload; { The image is centered before crop (crop edges in equal proportions) }
procedure CropBitmap (BMP: TBitmap; CONST MasterBMP: TBitmap); overload; { Crop the 'BMP' to fit into the MasterBMP }
function TileBitmap (BMP: TBitmap; CONST OutWidth, OutHeight: Integer): TBitmap;
procedure TileBitmapMirror (BMP: TBitmap; CONST OutWidth, OutHeight: Integer; TileType: RTileType); { Use a mirror effect for every odd image }
procedure FlipDown (Bmp: TBitmap); { 16ms }
procedure FlipRight (Bmp: TBitmap); { 18ms }
```

## LightVcl.Graph.FX.Rotate (10)

```pascal
procedure Reset;
procedure WriteToStream (Stream: TLightStream);
procedure ReadFromStream (Stream: TLightStream);
procedure RotateExif (BMP: TBitmap; Exif: TExifData);
procedure RotateBitmap (BMP: TBitmap; Degs: Single; AdjustSize: Boolean= TRUE; BkColor: TColor = clNone);
procedure RotateBitmapGDI (BMP: TBitmap; Degs: Single; AdjustSize: Boolean= TRUE; BkColor: TColor = clNone); { Uses GDI+ }
procedure RotateBitmapSWT (BMP: TBitmap; Rads: Single; AdjustSize: Boolean= TRUE; BkColor: TColor = clNone); deprecated 'Use LightVcl.Graph.FX.RotateBitmap'; { No antialiasing }
procedure RotateBitmapJanFX (BMP: TBitmap; Degs: Single; BkColor: TColor = clNone); deprecated 'Use LightVcl.Graph.FX.RotateBitmap'; { This is slow even if I rotate the image at right angles (90, 180, 270) }
procedure RotateBitmapPLG (BMP: TBitmap; Rads: Single; AdjustSize: Boolean= TRUE; BkColor: TColor = clNone); deprecated 'Use LightVcl.Graph.FX.RotateBitmap'; { 39ms. No antialising }
procedure RotateBitmapBLT (BMP: TBitmap; Rads: Single; lWidth, lHeight: Longint); // Worst!
```

## LightVcl.Graph.FX.RotateGr32 (3)

```pascal
procedure RotateBitmapGR32 (Bmp: TBitmap32; Angle: Single; AdjustSize: Boolean= True; BkColor: TColor = clPurple; Transparent: Boolean = FALSE; ResamplerKernel: Integer= HermiteKernel); overload;
procedure RotateBitmapGR32 (Bmp: TBitmap; Angle: Single; AdjustSize: Boolean= True; BkColor: TColor = clPurple; Transparent: Boolean = FALSE; ResamplerKernel: Integer= HermiteKernel); overload;
procedure RotateBitmapGR32 (Source, Destination: TBitmap; Angle: Single; X, Y: Integer; ResamplerClass: integer= 3; ResamplerKernel: Integer= HermiteKernel); overload;{ Angle in Deg }
```

## LightVcl.Graph.Gif (6)

```pascal
procedure saveFrame (Frame: TBitmap; FrameNo: Integer; OutputFolder: string);
function Open(aFileName: string): Boolean;
function SaveFrames(OutputFolder: string): Boolean;
function ExtractFrame(FrameNo: Cardinal): TBitmap;
function IsAnimated (CONST AGraphFile: string): Boolean;
function ExtractMiddleFrame(CONST FileName: string; OUT FrameCount: Cardinal): TBitmap;
```

## LightVcl.Graph.GrabAviFrame (1)

```pascal
function GetVideoPlayerLogo: TBitmap;
```

## LightVcl.Graph.Loader (23)

```pascal
function LoadGraph (CONST FileName: string; ExifRotate: Boolean = True; UseWic: Boolean = True): TBitmap; overload;
function LoadGraph (CONST FileName: string; OUT FrameCount: Cardinal): TBitmap; overload;
procedure LoadGraphToImg(CONST FileName: string; Image: TImage; ExifRotate: Boolean = True; UseWic: Boolean = TRUE);
function LoadFromResource (CONST RsrcName: string): TJPEGImage; { Load an image from a resource file }
function LoadTPicture (CONST FileName: string): TPicture; { Based on TPicture. Better than LoadGraph. }
procedure LoadToTImage (CONST FileName: string; ExifRotate: Boolean; Image: TImage); { Loads a file directly into a TImage component }
function LoadJ2K (CONST FileName: string): TBitmap; {$ENDIF}
function LoadJpg (CONST FileName: string; Scale: TJPEGScale= jsFullSize): TBitmap;
function LoadPNG (CONST FileName: string): TBitmap; overload;
function LoadGIF (CONST FileName: string): TBitmap; overload; { Load GIF and convert it to BMP }
function LoadGIF (CONST FileName: string; OUT FrameCount: Cardinal): TBitmap; overload;
function LoadWB1 (CONST FileName: string): TBitmap;
function LoadICO (CONST FileName: string): TBitmap;
function LoadEMF (CONST FileName: string): TBitmap;
function LoadBMP (CONST FileName: string): TBitmap; { Loads a BMP and suppress error messages }
function ExtractThumbnailJpg(CONST FileName: string; ThumbWidth, ThumbHeight: integer; OUT ResolutionX, ResolutionY: Integer): TBitmap; { Extracts from a JPEG image using scaling. The scale is automatically chosen based on the original image size and required thumb size } { Old name: LoadJpgThumbnail }
function ExtractThumbnail (CONST FileName: string; ThumbWidth: integer; OUT ResolutionX, ResolutionY: Integer; OUT FrameCount: Cardinal): TBitmap; overload; { Extracts the thumbnail from a gif, avi, jpg, png, etc file }
function ExtractThumbnail (CONST FileName: string; ThumbWidth: Integer): TBitmap; overload;
function loadGraphWic (CONST FileName: string): TBitmap; { Supports: GIF, PNG, JPG. Use LoadGraph instead }
function LoadGraphAsGrayScale(FileName: string): TBitmap; overload;
procedure LoadGraphAsGrayScale(FileName: string; BMP: TBitmap); overload;
function CheckValidImage (CONST FileName: string): Boolean;
function GetExif (CONST FileName: string): TExifData;
```

## LightVcl.Graph.Loader.Resolution (12)

```pascal
procedure GetImageRes(CONST FileName: string; OUT Width, Height: Integer); overload;
procedure GetImageRes(CONST FileName: string; Stream: TStream; OUT Width, Height: Integer); overload;
function GetJpgSize (Stream: TStream; OUT Width, Height: Integer): Boolean; overload;
procedure GetPNGSize (Stream: TStream; OUT Width, Height: Integer); overload;
procedure GetGIFSize (Stream: TStream; OUT Width, Height: Integer); overload;
procedure GetBmpSize (Stream: TStream; OUT Width, Height: Integer); overload;
function GetJPGSize (CONST Filename: string; OUT Width, Height: Integer): Boolean; overload;
procedure GetPNGSize (CONST FullName: string; OUT Width, Height: Integer); overload;
procedure GetGIFSize (CONST FullName: string; OUT Width, Height: Integer); overload;
procedure GetBmpSize (CONST FullName: string; OUT Width, Height: Integer); overload;
function GetBmpHeader (Stream: TStream): TBitmapHeader; // Used by GetBmpSize
function GetBitsPerPixel(BMP:TBitmap): Integer;
```

## LightVcl.Graph.Loader.Thread (6)

```pascal
procedure DoHandleexception;
procedure PushPicture(APicture: TBitmap);
procedure ProcessFile(const AFileName: string);
procedure Execute; override;
procedure Handleexception; virtual;
function PopPicture: TBitmap;
```

## LightVcl.Graph.Loader.WB1 (4)

```pascal
procedure JpegNeeded;
procedure Clear;
function LoadFromFile(CONST FullFileName: string): Boolean;
procedure SaveAsJpg(const FullFileName: string);
```

## LightVcl.Graph.Loader.WBC (5)

```pascal
function GetJpgStream(CONST Index: Integer): TMemoryStream;
procedure Clear;
function LoadFromFile(CONST FullFileName: string): Boolean;
procedure ExtractJpegs(CONST OutputFolder: string);
function GetJpeg(const Index: Integer): TJpegImage; { Get access to the specified JPEG }
```

## LightVcl.Graph.RainDropParamEditorForm (8)

```pascal
procedure trkRainChange (Sender: TObject);
procedure btnAdvancedClick(Sender: TObject);
procedure FormDestroy (Sender: TObject);
procedure FormKeyPress (Sender: TObject; var Key: Char);
procedure FormClose (Sender: TObject; var Action: TCloseAction);
procedure SettingChanged (Sender: TObject);
procedure ObjectFromGUI;
procedure GuiFromObject;
```

## LightVcl.Graph.RainDropParams (5)

```pascal
procedure Reset;
procedure Load(IniFile: TIniFileEx); overload; // For BioniX
procedure Save(IniFile: TIniFileEx); overload;
procedure Load(Stream: TLightStream); overload; // for RainDrop binary files
procedure Save(Stream: TLightStream); overload;
```

## LightVcl.Graph.RainShelter (6)

```pascal
procedure Clear;
function ImportImage (aFileName: string): Boolean;
function LoadFromFile(aFileName: string): Boolean;
procedure SaveToFile (aFileName: string; Mask: TBitmap);
procedure Save(Mask: TBitmap);
function IsRainShelter (CONST FileName: string): Boolean;
```

## LightVcl.Graph.Resize (12)

```pascal
procedure SmartStretch (BMP: TBitmap; ResizeOpp: RResizeParams); overload;
procedure SmartStretch (BMP: TBitmap; CONST MaxWidth, MaxHeight: Integer); overload;
procedure SmartStretch (BMP: TBitmap; CONST MaxWidth, MaxHeight, FitTolerance:Integer); overload;
procedure SmartStretch (BMP: TBitmap; CONST MaxWidth, MaxHeight: Integer; ResizeOp: TResizeOp); overload;
procedure SmartStretchCrop (BMP: TBitmap; CONST MaxWidth, MaxHeight: Integer); { Resizes the image to fill the viewport. Then it crops whatever went out of the viewport }
procedure StretchPercent (BMP: TBitmap; CONST ResizePercent: Integer); overload;
procedure StretchPercentX (BMP: TBitmap; CONST ResizeTimes: Single); overload;
procedure StretchProport (BMP: TBitmap; CONST OutWidth: Integer); overload; { Proportional. Height will be auto adjusted to match width }
procedure StretchProport (BMP: TBitmap; CONST MaxWidth, MaxHeight: Integer); overload;
function StretchProportF (BMP: TBitmap; CONST MaxWidth, MaxHeight: Integer): TBitmap; overload;
function LoadAndStretch (CONST FileName: string; ResizeOpp: RResizeParams; UseWic: Boolean= TRUE): TBitmap; overload; { Loads image and also stretch the output image to the required dimensions }
function LoadAndStretch (CONST FileName: string; CONST MaxWidth, MaxHeight: Integer; UseWic: Boolean= TRUE): TBitmap; overload;
```

## LightVcl.Graph.ResizeFMX (2)

```pascal
procedure ResizeFMX(BMP: Vcl.Graphics.TBitmap; NewWidth, NewHeight: Integer);
function ResizeFmxF(InputBMP: Vcl.Graphics.TBitmap; NewWidth, NewHeight: Integer): Vcl.Graphics.TBitmap;
```

## LightVcl.Graph.ResizeGr32 (3)

```pascal
procedure StretchImage(Image: string; ExifRotate: Boolean); overload;
procedure StretchImage(BMP : TBitmap); overload;
procedure StretchGr32(BMP: TBitmap; ScaleX, ScaleY: Double; aResampler: Byte = KernelResampler; aKernel: Byte = DefaultKernel);
```

## LightVcl.Graph.ResizeParamFrame (4)

```pascal
procedure GUIChanged (Sender: TObject);
procedure GUIFromObject(ResizeParams: PResizeParams); { Set the GUI according to the values stored in the ResizeParams record }
procedure ObjectFromGUI(ResizeParams: PResizeParams); { Load values from the GUI into the ResizeParams record }
procedure Register;
```

## LightVcl.Graph.ResizeParams (8)

```pascal
procedure computeAutodetect(InpW, InpH: Integer);
procedure computeFit(InpW, InpH: Integer);
procedure computeFill(InpW, InpH: Integer);
procedure Reset; { Initialize all fields to default values }
procedure ComputeOutputSize(InpW, InpH: Integer); { Compute OutW/OutH based on input dimensions }
procedure WriteToStream(IOStream: TLightStream);
procedure ReadFromStream(IOStream: TLightStream);
procedure Convert(Res: TResizeOpp_; FT: TFillType_); { Convert from old BioniX v12 format }
```

## LightVcl.Graph.ResizeVCL (4)

```pascal
procedure ScaleImage (CONST InpBMP, OutBMP: TBitmap; const ScaleAmount: Double);
procedure CanvasStretch (CONST InpBMP, OutBMP: TBitmap); overload; { Not proportional }
procedure CanvasStretch (CONST BMP: TBitmap; CONST OutWidth, OutHeight: Integer); overload; { Not proportional }
procedure CanvasStretch (CONST BMP: TBitmap; CONST OutWidth: Integer); overload; { Proportional }
```

## LightVcl.Graph.ResizeWinBlt (2)

```pascal
function StretchF (BMP: TBitmap; OutWidth, OutHeight: Integer): TBitmap; { Best of all algorithms. 2019.08 }
procedure Stretch (BMP: TBitmap; OutWidth, OutHeight: Integer);
```

## LightVcl.Graph.ResizeWinGDI (1)

```pascal
procedure ResizeBitmapGDI(Source, Dest: TBitmap; OutWidth, OutHeight: Integer);
```

## LightVcl.Graph.ResizeWinThumb (4)

```pascal
procedure SetFile(const Value: String);
procedure SetSize(Value: Integer);
procedure GenerateThumbnail;
procedure GenerateThumbnail2;
```

## LightVcl.Graph.ResizeWinWIC (1)

```pascal
procedure ResizeBitmapWic(const Bitmap: TBitmap; const NewWidth, NewHeight: Integer); // pf32
```

## LightVcl.Graph.ShadowText (2)

```pascal
function DrawShadowText(Canvas: TCanvas; const Text: string; X, Y: Integer; TextColor, ShadowColor: TColor; ShadowDist: Integer = 2): Integer; overload;
function DrawShadowText(Canvas: TCanvas; const Text: string; TextRect: TRect; TextColor, ShadowColor: TColor; ShadowDist: Integer; DrawFlags: DWORD = DT_LEFT or DT_END_ELLIPSIS): Integer; overload;
```

## LightVcl.Graph.Text (14)

```pascal
procedure DrawTextCentered (Canvas: TCanvas; CONST Text: string; Rect: TRect);
function CenterTextX (BMP: TBitmap; CONST Text: string): Integer; overload; { Returns the X coordinate where we must draw the text if we want to have the text centered in bitmap }
function CenterTextY (BMP: TBitmap; CONST Text: string): Integer; overload;
function CenterTextX (Canvas: TCanvas; CONST Text: string; R: TRect): Integer; overload;
function CenterTextY (Canvas: TCanvas; CONST Text: string; R: TRect): Integer; overload;
function GetFontHeight (aFont : TFont) : integer; { Get font height when we don't have a canvas }
procedure DrawTextShadowBox (BMP: TBitmap; CONST Text: string; AlignTop: Boolean; ShadowColor: TColor= clTextShadow; ShadowOpacity: Byte= 20; Blur: Byte= 2); overload;
procedure DrawTextShadowBox (aCanvas: TCanvas; CONST Text: string; X, Y: Integer; ShadowColor: TColor= clTextShadow; ShadowOpacity: Byte= 20; Blur: Byte= 2); overload;
procedure DrawTextShadow3DSoft (aCanvas: TCanvas; CONST Text: string; X, Y: Integer; ShadowColor: TColor= clTextShadow); { Faster. No transparency }
procedure DrawTextShadow3DHard (aCanvas: TCanvas; CONST Text: string; X, Y: Integer; ShadowColor: TColor= clTextShadow; TopShadow: Boolean= FALSE);
procedure DrawTextOutline (aCanvas: TCanvas; CONST Text: string; X, Y: Integer; SolidMiddle: Boolean= TRUE); { Very poor }
procedure DrawTextXOR (DC: HDC; Font: TFont; CONST Text: string; X, Y: integer); { Very poor }
procedure ShadowDownLeft (BMP: TBitmap); deprecated 'Use JanFX directly';
procedure ShadowDownRight (BMP: TBitmap); deprecated 'Use JanFX directly';
```

## LightVcl.Graph.Util (32)

```pascal
function DarkenColor (aColor: TColor; Percent: Byte): TColor; { make a color more dark or light. Percent arata la cat la suta din luminozitate sa pun noua valoare. O= totally dark, 100= unchanged }
function LightenColor (aColor: TColor; Percent: Byte): TColor;
function ChangeBrightness (aSource: TColor; Change: Integer): TColor;
function ChangeColor (aSourceColor, aDestClr: TColor; Change: Integer): TColor; { Change Source color towards Destination. Change is in 'percents'. }
function SimilarColor (Color1, Color2: TColor; Tolerance: Integer): Boolean; { Checks is two colors are similar }
function ComplementaryColor (aColor: TColor): TColor;
function ColorToHtml (aColor: TColor): string;
function HtmlToColor (aColor: string): TColor;
function RGB2Color (R,G,B: Integer): Cardinal; deprecated 'Use WinApi.Windows.RGB instead.' { !!!!! uses WinApi.Windows.RGB(R,G,B) }
procedure SplitColor2RGB (aColor: TColor; OUT R,G,B: Byte);
function Integer2Color (i: Integer): TColor; { Tries to make a color from an integer. The color are 'lighted' in this order: BGR. }
function BlendColors (Color1, Color2: TColor; A: Byte): TColor; { Mixing two colors. Usage NewColor:= Blend(Color1, Color2, blending level 0 to 100). 0 = fully Color2, 100 = fully Color1. Source: http://rmklever.com/?cat=6 }
function CombinePixels (Pixels: PByte; Weights: PInteger; Size: Cardinal): Integer; { NOT TESTED UNDER WIN64. IT MIGHT WORK! }
function MixColors (FG, BG: TColor; BlendPower: Byte): TColor; { 0 = fully BG, 255 = fully FG }
procedure ReplaceColor (BMP: TBitmap; OldColor, NewColor: TColor); overload;
procedure ReplaceColor (BMP: TBitmap; OldColor, NewColor: TColor; ToleranceR, ToleranceG, ToleranceB: Byte); overload;
function GetAverageColor (BMP: TBitmap; Fast: Boolean): TColor;
function GetAverageColorPf8 (BMP: TBitmap): Byte;
procedure AntialisedLine(Canvas: TCanvas; CONST AX1, AY1, AX2, AY2: Real; Color: TColor);
procedure DrawPolygon (Canvas: TCanvas; Color: TColor; const Points: array of integer);
procedure DrawTriangle (Canvas: TCanvas; Width, Height, BorderDistance: Integer); { Draw a triangle (like a Play button) the that fits into the specified control }
procedure DrawBorder (Control: TWinControl); { Draw red frame arround the specified control }
procedure DrawCheck (Canvas: TCanvas; Location : TPoint; Size: Integer; Shadow: Boolean = True); deprecated 'Call directly VCLGhraphUtil instead.' { Draws checkmarks of any Size at Location with/out a shadow. }
procedure DrawChevron (Canvas: TCanvas; Direction: TScrollDirection; Location: TPoint; Size: Integer); deprecated 'Call directly VCLGhraphUtil instead.' { Draws arrows that look like ">" which can point in any TScrollDirection }
procedure DrawArrow (Canvas: TCanvas; Direction: TScrollDirection; Location: TPoint; Size: Integer); deprecated 'Call directly VCLGhraphUtil instead.' { Draws a solid triangular arrow that can point in any TScrollDirection }
function WindowsThemesEnabled: Boolean;
function VclStylesEnabled: Boolean;
function ThemeColorBkg: TColor;
function ThemeColorHilight: TColor;
function ThemeColorButtonFace: TColor;
function ThemeGetPanelElementColor(Elem: TElementColor): TColor; { Use it like this: cl:= ThemeGetPanelElementColor; if cl= clNone then UseDefaultColor }
function GetDeviceColorDepth: Integer;
```

## LightVcl.Graph.UtilGray (7)

```pascal
function RGB2Gray (Color: TColor): TColor;
procedure ConvertToGrayscale(BMP: TBitmap);
function GetAverageColorPf8 (GrayBmp: TBitmap): Byte; overload;
function GetAverageColorPf32(GrayBmp: TBitmap): Byte; overload;
procedure SetBitmapGrayPalette(BMP: TBitmap);
function HasGrayscalePalette(const FileName: string): Boolean; overload;
function HasGrayscalePalette(BMP: TBitmap): Boolean; overload;
```

## LightVcl.Internet.Common (22)

```pascal
function ParseURL (CONST lpszUrl: string): TStringArray; { Breaks an URL in all its subcomponents. Example: ParseURL('http://login:password@somehost.somedomain.com/some_path/something_else.html?param1=val&param2=val') }
function PathIsURLA(pszPath: PAnsiChar): BOOL; stdcall; {$EXTERNALSYM PathIsURLW} { $HPPEMIT '#include <shlwapi.h>'}
function PathIsURLW(pszPath: PWideChar): BOOL; stdcall; { from here: https://msdn.microsoft.com/en-us/library/windows/desktop/bb773724(v=vs.85).aspx. But it is not good at all because it only checks if path starts with http and if it contains space. Otherwise it accepts all other characters. So I use it in conjunction with my own function }
function CheckURLStartMsg (CONST URL: string): Boolean; { Check if the URL starts with HTTP or with www }
function GetLocalIP: string; overload;
function GetLocalIP(OUT HostName, IpAddress, ErrorMsg: string): Boolean; overload;
function ResolveAddress (CONST HostName: String; out Address: DWORD): Boolean;
function GenerateInternetRep: string; // Generate report
function CoCreateGuid(var guid: TGUID): HResult; stdcall; far external 'ole32.dll';
function IE_EnableProxy(const Server: String): Boolean;
function IE_DisableProxy: Boolean;
function IE_GetProxySettings(OUT ProxyAdr, ProxyPort: string; OUT IsEnabled: boolean): Boolean;
procedure IE_DeleteCache;
procedure IE_EndSession;
procedure IE_SetProxy(CONST Proxy: string); { Change IE proxy settings globally }
function PCConnected2Internet: Boolean; { From here: http://www.delphipages.com/forum/showthread.php?t=198159 }
function ProgramConnect2Internet: Integer; overload; { Legacy: google.com + the 60 s download default. Returns: -1 = PC not connected, 0 = connected but this app is blocked by the firewall, 1 = this app can reach the Internet}
function ProgramConnect2Internet(const TestURL: string; TimeoutMs: Integer= ConnectivityProbeTimeout; const ExpectBody: string= ''): Integer; overload; { Caller-set endpoint + timeout, so a startup check gets a verdict in seconds instead of the 60 s download default. Returns: -1 = PC not connected (WinInet); 0 = PC online but NO reply came back (this exe is firewall-blocked, or the endpoint is down); 1 = reached the endpoint and the body matched (genuinely online); 2 = reached the endpoint (HTTP 200) but the body was NOT ExpectBody -> a captive portal or a content-rewriting proxy is in the path, which is NOT a firewall block. Pass ConnectivityProbeURL for a fast, light default. ExpectBody='' = any HTTP 200 counts as 1 (state 2 never occurs); set it (e.g. ConnectivityProbeBody) to tell a genuine reply apart from a portal/proxy interception.}
function ProgramConnect2InternetS: string;
function TestProgramConnection(ShowMsgOnSuccess: Boolean= FALSE): Integer;
function IsPortOpened(const Host: string; Port: Integer): Boolean; { Here's something very simple with which you can check a port status(opened/closed) on remote host. Add WinSock to uses clause}
Procedure CreateUrlOnDesktop (CONST ShortFileName, sFullURL: string);
```

## LightVcl.Internet.CommonWebDown (1)

```pascal
function GetUnsplashImage(CONST URL, LocalFile: string): Boolean;
```

## LightVcl.Internet.Download.Indy (4)

```pascal
procedure Execute; override;
function DownloadFile (CONST URL, Referer, DestinationFile: string; OUT ErrorMsg: String): Boolean; { Downloads file using Indy with HTTPS support. Streams directly to disk. }
function DownloadThread (CONST URL, DestinationFile: string; OUT ErrorMsg: String): Boolean; { WARNING: Blocks GUI! Uses RAM. No HTTPS support. }
function DownloadThread2(CONST URL, DestinationFile: string; OUT ErrorMsg: String): Boolean; { Threaded download. App won't close until done. No HTTPS support. }
```

## LightVcl.Internet.Download.Thread (4)

```pascal
procedure SetURL(CONST Value: string);
procedure DoDownloadDone; { Thread-safe event trigger }
procedure Execute; override;
function DownloadSuccess: Boolean; { Returns True if download completed successfully }
```

## LightVcl.Internet.Download.WinInet (3)

```pascal
function DownloadAsString (CONST URL: string; Referer: string= ''): string;
function DownloadBytes (CONST Url, Referer: String; OUT Data: TBytes; PostData: String= ''; SSL: Boolean = FALSE): Cardinal; overload; { TESTED OK }
function DownloadToFile (CONST URL, Referer, DestinationFile: string): Cardinal; overload; { It can be used with text or binary files }
```

## LightVcl.Internet.Email (16)

```pascal
function OpenDefaultEmail(CONST Recipient, Subject, Mesaj: String): Cardinal; { This will open the default email program in 'Compose' mode }
function OpenDefaultEmailEx(CONST Subject, Body, FileName, SenderName, SenderEMail, RecipientName, RecipientEMail: AnsiString): Integer;
function ExtractEmailEngine (CONST SmallText : string; StartPos: integer; var EndPos: integer): string; { extract the first email address encountered from a string }
function ExtractFirstEmailAdr (CONST SmallText : string): string; { extract the first email address encountered from a string }
function ExtractAllEmailAdr (HugeString: string; AdreseExtrase: TStringlist): Integer;
function ExtractEmailFromThunderbirdFile(CONST HugeText: string; CONST ToField, FromField, CcField, BccField: Boolean; OutputList: TStringList): Integer;
function ValidateEmailAddress ( Email: String; OUT FailCode, FailPosition: Integer) : Boolean; overload; { Returns nothing if the email is valid else return the reason. SuggestCorrection=TRUE, the program will try to suggest a corrected version of this address }
function ValidateEmailAddress (CONST Email : string; SuggestCorrection: boolean): string; overload; { Returns nothing if the email is valid else return the reason. SuggestCorrection=TRUE, the program will try to suggest a corrected version of this address }
function ValidateEmailAddress (CONST Email : string): Boolean; overload;
function CorrectEmailAddress ( Email : String; OUT Suggestion: String; MaxCorrections : Integer = 5) : Boolean;
function EmailHasManyNumbers (CONST Email : string; CONST Ratio: integer): Boolean;
function FailCode2Str (Code : Integer) : string; { convert the integer fail codes to understandeble message strings }
function CheckIfThunderbirdFile(CONST HugeText: String): Boolean;
procedure SplitEmailAddress (CONST EmailAddress: string; out user, domain: string); { Returns the 'user' and 'domain' part of an email address - the @ charecter is not included }
function EmailSortByDomain (CONST InptList: TStrings): TStringList; { SORT }
procedure SendEmail (CONST sTo, sSubject, sBody: string);
```

## LightVcl.Internet.EmailSender (1)

```pascal
function SendEmail( SMTP: TIdSMTP;
```

## LightVcl.Internet.HTML (2)

```pascal
procedure SetFieldValue (aForm: IHTMLFormElement; const fieldName: string; const newValue: string; const Instance: integer= 0);
function GetFormByNumber (Document: IHTMLDocument2; formNumber: integer): IHTMLFormElement;
```

## LightVcl.Internet.HTMLImg (8)

```pascal
function ExtractIMGTags (CONST HTMLBody: string): TStringList; { Extract <IMG> tags images from HTML body } { Old name: ExtractImage_Img }
function ExtractImagesFromIMG (CONST HTMLBody: string): TStringList; { Extract images from <IMG src> tags } { Old name: ExtractImage_Img }
function ExtractImagesFromAHREF(CONST HtmlBody: string): TStringList; { Extract images from '<a href>' tags } { Old name: ExtractImage_Href }
function ExtractImages (CONST HtmlBody: string): TStringList; { Extract images from '<a href>' and <img>'. } { Old name: ExtractImages }
function MakeImgRelativePaths(CONST HtmlBody, RelativeTo: string): string; { Locates all IMG tags in a HTML document and converts their SRC (paths) from full path to relative path }
function MakeImgRelativePath (CONST HtmlLine, RelativeTo: string): string;
function MakeImgFullPath (CONST HtmlLine, Base: string): string;
function ExpandRelativePaths (CONST HtmlBody, Base: string): string;
```

## LightVcl.Internet.HtmlWriter (9)

```pascal
function GetContent: string;
procedure AddContent(CONST s: string); { append 's' to the current content without inserting a ENTER }
procedure AddContentNewLine(CONST s: string);
procedure SaveToFile(CONST FileName: string);
procedure GenerateContent;
procedure Reset;
procedure AddBodyLine(CONST s: string); { add 's' in a new line to the current Body }
procedure AddBodyLineB(CONST s: string); { add 's' in a new line to the current Body. Also add a <BR> tag }
procedure Register;
```

## LightVcl.TranslatorAPI (18)

```pascal
function GetProviderName: string; virtual; abstract;
function TranslateText(const Text, TargetLang: string; const SourceLang: string = ''): string; virtual; abstract;
function TranslateBatch(const Texts: TArray<string>; const TargetLang: string; const SourceLang: string = ''): TArray<string>; virtual; abstract;
function TestConnection: Boolean; virtual; abstract;
function GetLanguageCode(const LanguageName: string): string; virtual;
function GetLanguageName(const LanguageCode: string): string; virtual;
function EstimateCharacters(const Texts: TArray<string>): Integer;
procedure TranslateINIFile(const SourceFile, TargetFile, TargetLang: string; TranslateEmptyOnly: Boolean = FALSE);
function GetAPIURL: string;
function DoHttpPost(const URL: string; const RequestBody: TStringStream): string;
function ParseTranslationResponse(const JSONResponse: string): TArray<string>;
function GetProviderName: string; override;
function TranslateText(const Text, TargetLang: string; const SourceLang: string = ''): string; override;
function TranslateBatch(const Texts: TArray<string>; const TargetLang: string; const SourceLang: string = ''): TArray<string>; override;
function TestConnection: Boolean; override;
function LanguageNameToDeepL(const LanguageName: string): string;
function DeepLToLanguageName(const DeepLCode: string): string;
function GetSupportedLanguages: TArray<string>;
```

## LightVcl.Visual.ActivityIndicator (2)

```pascal
procedure DrawFrame; override;
procedure Register;
```

## LightVcl.Visual.AppData (35)

```pascal
procedure setGuiProperties(Form: TForm);
procedure setFont(aFont: TFont);
function getGlobalLog: TfrmRamLog;
procedure writeAppDataFolder;
procedure writeInstallationFolder;
procedure setHintType(const aHintType: THintType); override;
procedure setHideHint(const Value: Integer); override;
procedure RegisterUninstaller;
procedure Run;
procedure ResurrectInstance(CONST CommandLine: string);
function InstanceRunning: Boolean;
procedure SetSingleInstanceName(var Params: TCreateParams);
function ExtractData(VAR Msg: TWMCopyData; OUT FirstInstance: string): Boolean;
function PromptToSaveFile (VAR FileName: string; CONST Filter: string = ''; CONST DefaultExt: string= ''; CONST Title: string= ''): Boolean;
function PromptToLoadFile (VAR FileName: string; CONST Filter: string = ''; CONST Title: string= ''): Boolean;
function PromptForFileName(VAR FileName: string; SaveDialog: Boolean; CONST Filter: string = ''; CONST DefaultExt: string= ''; CONST Title: string= ''; CONST InitialDir: string = ''): Boolean;
procedure Restore;
procedure Restart;
procedure SelfDelete;
procedure Minimize; override;
function RunFileAtStartUp(CONST FilePath: string; Active: Boolean): Boolean;
function RunSelfAtStartUp(Active: Boolean): Boolean;
function ReadAppDataFolder(CONST UninstalledApp: string): string; //used by Uninstaller App
function ReadInstallationFolder(CONST UninstalledApp: string): string;
procedure CreateMainForm (aClass: TFormClass; MainFormOnTaskbar: Boolean= FALSE; Show: Boolean= TRUE; AutoState: TAutoState= asPosOnly); overload;
procedure CreateMainForm (aClass: TFormClass; OUT Reference; MainFormOnTaskbar: Boolean= FALSE; Show: Boolean= TRUE; AutoState: TAutoState= asPosOnly); overload;
procedure CreateForm (aClass: TFormClass; OUT Reference; Show: Boolean= TRUE; AutoState: TAutoState= asPosOnly; Owner: TWinControl = NIL; Parented: Boolean= FALSE; CreateBeforeMainForm: Boolean= FALSE);
procedure CreateFormHidden(aClass: TFormClass; OUT Reference; AutoState: TAutoState= asPosOnly; ParentWnd: TWinControl = NIL);
procedure CreateFormModal (aClass: TFormClass; OUT Reference; AutoState: TAutoState= asPosOnly; ParentWnd: TWinControl= NIL); overload; // Do I need this?
procedure CreateFormModal (aClass: TFormClass; AutoState: TAutoState= asPosOnly; ParentWnd: TWinControl= NIL); overload;
procedure CreateFormModalCentered(aClass: TFormClass);
procedure SetMaxPriority;
procedure HideFromTaskbar;
function GetVersionInfoMajor: Word;
function GetVersionInfoMinor: Word;
```

## LightVcl.Visual.AppDataForm (13)

```pascal
procedure WMPostInit(var Msg: TMessage); message WM_POSTINIT;
procedure Loaded; override;
procedure DoDestroy; override;
procedure DoClose(VAR Action: TCloseAction); override;
procedure FormKeyPress(Sender: TObject; var Key: Char); // We can use this later in the destructor to know how to save the form: asPosOnly/asFull
procedure WMEndSession(VAR Msg: TWMEndSession); message WM_ENDSESSION; { Save form state on Windows logoff/shutdown, but only when Msg.EndSession is TRUE (a canceled logoff must not save). The FFormSaved guard in saveBeforeExit prevents a double-save when CloseQuery already ran via WM_QUERYENDSESSION. }
function CloseQuery: boolean; override;
procedure FormPostInitialize; virtual; // Takes place after the form was fully created
procedure FormPreRelease; virtual; // Takes place before the form is destroyed. It is guaranteed to be called excetly once.
procedure saveBeforeExit; // Idempotent (FFormSaved guard). Public so TAppData.Destroy can save all still-open forms while AppData is alive â€” Application-owned forms are otherwise destroyed AFTER AppData's finalization (in Vcl.Forms' finalization).
procedure LoadForm; virtual;
procedure SaveForm; virtual;
procedure MainFormCaption(const Caption: string);
```

## LightVcl.Visual.AssociateExt (6)

```pascal
procedure CreateWnd; override;
procedure CreateWindowHandle(const Params: TCreateParams); override;
procedure chkAllUsersClick (Sender: TObject);
procedure btnAssociateClick (Sender: TObject);
procedure btnAssociateDelClick(Sender: TObject);
procedure Register;
```

## LightVcl.Visual.CalendarCanvas (41)

```pascal
procedure SetCalendarDate (aDate: TDateTime);
procedure SetMonth (Value: Integer);
procedure SetDay (Value: Integer);
procedure SetYear (Value: Integer);
function JulDate1stWeek(JD : TDateTime) : TDateTime;
function WeekNo (JDate : TDateTime): Integer;
function GetWeekNumber:Integer;
function GetDayOfYear: Integer;
function GetDaysInYear:Integer;
procedure SetGermanDate (Value: Boolean); // RW: this one sets the german date
procedure SetShowWeeks (Value: Boolean); // RW: adapted DayOfWeek-function to fit german date
function rDayOfWeek (vDate: TDateTime) : Integer;
procedure SetColHoliday (Value: TColor);
procedure SetColSunday (Value: TColor);
procedure SetColSaturday(Value: TColor);
procedure SetColMarked (Value: TColor);
procedure SetHolidays (Value: TStrings); // RW: build string lists
procedure SetMarkdays (Value: TStrings);
function CheckHoliday (DateList: TStrings; sd: string; m: integer) : Boolean;
procedure SetFont (const Value: TFont);
function GetFont: TFont; // RW: returns TRUE if parameter denotes a special day
procedure DateChange;
procedure DrawDateInHeader;
procedure DrawDaysHeader;
procedure DrawDates;
procedure DrawWeeks;
procedure DrawFocusFrame (nIndex : Integer);
procedure LoadDateArray;
function GetMonthBegin: Integer;
function SetDate (nDays : Integer): Boolean;
function GetRectFromIndex (nIndex : Integer): TRect;
function GetIndexFromDate: Integer;
function GetIndexFromPoint (nLeft : Integer ; nTop : Integer) : Integer;
function ValidDate (aDate: TDateType) : Boolean;
function DaysInMonth (nMonth, nYear : Integer): Integer;
function IsLeapYear (AYear: Integer): Boolean;
function IsValidDate (yy,mm,dd: Integer): Boolean;
function WeeksFirstDay(aDate: TDateTime): TDateTime; { This function helps to Filter/Query a Table by Weeks}
function WeeksLastDay (aDate: TDateTime): TDateTime; { This function helps to Filter/Query a Table by Weeks}
procedure Paint;
function CalculateDayOfYear(y, m, d : Word): Integer;
```

## LightVcl.Visual.CaptionedThumb (10)

```pascal
Procedure CMMouseEnter(var msg: TMessage); message CM_MOUSEENTER;
Procedure CMMouseLeave(var msg: TMessage); message CM_MOUSELEAVE;
procedure setCaption(const Value: string);
procedure setSelected(const Value: Boolean);
procedure Paint; override;
procedure Click; override;
function GetClientHeight: Integer;
function GetClientWidth : Integer;
procedure Register;
function CreateThumbnail(Owner, Parent: TWinControl; CONST FileName: string): TLightCaptionThumbnail;
```

## LightVcl.Visual.CheckBox (6)

```pascal
procedure AdjustBounds;
procedure setAutoSize(b: Boolean); reintroduce; // Don't overload. We want to hide original method. http://docwiki.embarcadero.com/RADStudio/Sydney/en/Methods_(Delphi)
procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
procedure Loaded; override;
procedure Register;
```

## LightVcl.Visual.CheckListBox (6)

```pascal
function SelectedItem: string; { Returns the selected item text, or empty string if nothing is selected }
function SelectedItemForce: string; { Returns the selected item. If no item is selected, selects the first item first }
function SelectItem(CONST ItemText: string): Integer;
function FindItem (const ItemText: string): Integer;
function CheckItem(const ItemText: string; Checked: Boolean): Integer; { Sets the checkbox state for the item. Returns -1 if not found }
procedure Register;
```

## LightVcl.Visual.ComboBox (11)

```pascal
function SelectedItem: string; { Returns the selected item. Raises exception if no item selected! }
function SelectedItemSafe: string; { Returns the selected item. No exception if no item selected }
function SelectedItemForce: string; { Returns the selected item. If no item is selected, selects the first item first }
function SelectedObject: TObject;
function SelectItem(CONST ItemText: string): Integer;
function SelectFirstItem: Boolean;
function SelectObject(AObject: TObject): Boolean;
procedure DrawItem (Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
function SelectDualItem (const ScreenName: string): Integer;
function SelectedDualItem: string;
procedure Register;
```

## LightVcl.Visual.CountDown (14)

```pascal
procedure setStartValue(Value: Cardinal);
procedure TimerTimer(Sender: TObject);
procedure setResolution(Value: Cardinal);
function getResolution: Cardinal;
procedure Tick; { Call this to advance the countdown }
function TimeLeftNice: Integer; { If interval under 15 minutes, show time in seconds (max 3 digits: up to 999 seconds) else show time in minutes }
procedure Reset; { Resets the TimeLeft. Does not start the timer! }
procedure Restart; { Resets the TimeLeft and start the timer }
procedure Start;
procedure Stop;
function Resume: Boolean; { Resume the countdown from where it was left BUT ONLY if when we called Stop, it was enabled/running }
function Enabled: Boolean;
function TimeLeftS: string;
procedure Register;
```

## LightVcl.Visual.CreationOrderTester (11)

```pascal
procedure Loaded; override;
procedure CreateWnd; override;
procedure DestroyWnd; override;
procedure CreateWindowHandle(const Params: TCreateParams); override;
procedure SetParent(AParent: TWinControl); override; { SetParent is called during construction AND also during destruction with aParent=nil }
procedure AfterConstruction; override;
procedure BeforeDestruction; override;
procedure WriteLog(CONST s: string);
procedure WriteControlState;
procedure WriteComponentState;
procedure Register;
```

## LightVcl.Visual.DirectoryListBox (6)

```pascal
procedure SetShowSystem(Value: boolean);
procedure SetShowHidden(Value: boolean);
function ReadDirectoryNames(const ParentDirectory: string; DirectoryList: TStringList): Integer;
function getDirectory: string;
procedure BuildList; override; { Modified version of original Borland procedure, to support "Show hidden/system folders" }
procedure Register;
```

## LightVcl.Visual.DropDownSearch (16)

```pascal
procedure showDropDown;
procedure endSearch(Sender: TObject);
procedure FilterItems;
procedure HandleArrowKeys(Key: Word);
procedure WMKillFocus(var Message: TWMKillFocus); message WM_KillFocus;
procedure SetHost;
procedure KeyDown(var Key: Word; Shift: TShiftState); override;
procedure Change; override;
procedure Click; override;
procedure AfterConstruction; override;
procedure PopulateDictionary(Words: TStringList);
procedure AddDemoStrings;
function SelectedString: string;
function SelectedObject: TObject;
function WordCount: Integer;
procedure Register;
```

## LightVcl.Visual.Edit (9)

```pascal
procedure SetCheckFileEx(CONST Value: Boolean);
procedure SetCheckDirEx (CONST Value: Boolean);
procedure Change; override;
procedure Click; override;
procedure KeyPress (VAR Key: Char); override;
procedure Loaded; override;
procedure UpdateBkgColor;
procedure SetTextNoEvent(CONST aText: string);
procedure Register;
```

## LightVcl.Visual.FileFilter (2)

```pascal
procedure Change; override;
procedure Register;
```

## LightVcl.Visual.FileListBox (19)

```pascal
procedure Change; override;
function MoveSelectionUp : Boolean; { Returns true if it succesfully moved the selection }
function MoveSelectionDwn: Boolean;
procedure InvertSelection;
procedure DeselectAll;
procedure SelectFirstItem;
procedure SelectItemClick(CONST ItemPos: Integer); { Deselect all current items and select specified item then simulate a click on it }
procedure SimulateClickCurItem; { Simulate a click on current item }
procedure SimulateClickCurItemForce; { Simulate a click on current item. If no item is selected, then select the first on }
procedure SelectItem (CONST ItemPos: Integer); overload; { Deselect all current items and select specified item }
procedure SelectItem (ItemName: string); overload; { Select specified item. Full path accepted (will be stripped) }
function SelectionCount: Integer; { Returns the number of selected items }
function SelectedItem: string;
procedure DeleteCurrentFile; { To Recycle Bin }
procedure DeleteSelectedAndFocus;
procedure KeyUp (VAR Key: Word; Shift: TShiftState); override;
procedure ReadFileNames; override;
procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
procedure Register;
```

## LightVcl.Visual.FloatSpinEdit (21)

```pascal
procedure KeyPress(var Key: Char); override;
procedure setValue(aValue: Real);
function getHint: string;
procedure setHint(aValue: string);
function getShowHint: boolean;
procedure setShowHint(aValue: boolean);
function checkMinMax(NewValue: Real): Real;
function getFilteredText: string;
procedure CMExit(var Message: TCMExit); message CM_EXIT;
procedure UpdateEditorValue;
procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
procedure ControlEnter(Sender: TObject);
function GetEnabled: boolean; override;
procedure SetEnabled(aValue: boolean); override;
procedure WMSetFocus(VAR Message: TWMSetFocus); message WM_SetFocus;
procedure UpDownChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: Integer; Direction: TUpDownDirection);
procedure EditChanged(Sender: TObject);
function ValueAsString(HowManyDecimals: Byte): string; overload;
function ValueAsString: string; overload;
procedure Register;
function Real2Str(CONST ExtValue: Extended; Decimals: Byte = 1): string;
```

## LightVcl.Visual.FreeDiskSpace (3)

```pascal
procedure CreateWnd; override;
function DriveCount: Integer;
procedure Register;
```

## LightVcl.Visual.GradientPanel (5)

```pascal
procedure SetGradientFill(const Value: Boolean);
procedure SetColor1(const Value: TColor);
procedure SetColor2(const Value: TColor);
procedure Paint; override;
procedure Register;
```

## LightVcl.Visual.GraphChart (18)

```pascal
procedure Clear; { Clear the data but does not resize the array }
procedure DoDrawLegend;
function HighestDpX: Double; { Find highest value among all axis }
procedure Loaded; override;
procedure Resize; override;
procedure FakeLinearDataPoints(var Serie: TSerie);
procedure FakeDataPoints (var Serie: TSerie);
procedure FakeDataPointsWsk (var Serie, Serie2: TSerie); { Wiskers }
procedure FakeDataPointsB (var Serie: TSerie);
procedure Clear;
function CreateSerie(const LegendText: string; Color: TColor; PlotType: TPlotType= [plConnLines]; TotalDataPoints: Integer= 0): TSerie;
procedure PrintXLabels;
procedure PrintYLabels;
procedure DrawChart;
procedure FakeData;
procedure Register;
function FindNonZeroPoint(DataPoints: TDataPoints): Cardinal; { Stop on the first non-empty cell }
function FindMax (DataPoints: TDataPoints): Integer; { Stop on the first non-empty cell }
```

## LightVcl.Visual.Groupbox (2)

```pascal
procedure Paint; override;
procedure Register;
```

## LightVcl.Visual.INIFile (5)

```pascal
function IsSupported(WinCtrl: TComponent): Boolean; override;
function WriteComp(Comp: TComponent): Boolean; override;
function ReadComp (Comp: TComponent): Boolean; override;
procedure SaveForm (Form: TLightForm);
procedure LoadForm (Form: TLightForm);
```

## LightVcl.Visual.LabelEdit (4)

```pascal
procedure setValid(const Value: TValidity);
procedure Change; override;
procedure KeyPress (VAR Key: Char); override;
procedure Register;
```

## LightVcl.Visual.ListBox (38)

```pascal
procedure Resize; override;
procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
procedure LBADDSTRING(var M: TMessage); message LB_ADDSTRING;
procedure LBDELETE(var M: TMessage); message LB_DELETESTRING;
procedure LBINSERT(var M: TMessage); message LB_INSERTSTRING;
procedure KeyDown(var Key: Word; Shift: TShiftState); override;
procedure SetHeightAuto(MaxHeight: Integer; aForm: TControl);
function SelectedItemI: Integer;
function SelectedItemForce: string; { Same as SelectedItem but if no item is selected then force the first item selected }
function SelectedItem: string; { returns the item the cursor is on }
function SelectedItems: string; { return all selected text }
function SelectedObject: TObject;
function SelCountEx: Integer; { does the same thing as 'SelCount' but also works when MultiSelect=False. SelCount does not work if MultiSelect=False }
procedure DeSelectAll;
procedure SelectNext;
procedure SelectFirstItem;
procedure SelectLastItem;
procedure SelectItemSafe(Index: Integer); { Try to select the specified item. If the index is invalid no error is thrown and the closes item is selected }
procedure SelectItem(Obj: TObject);
procedure SwapItems(x, y: Integer);
procedure MoveUp; { Move current item up }
procedure MoveDown;
function FindItem(const aText: string): Integer;
procedure DeleteFirst(iCount: Integer); { Delete first x items }
procedure DeleteSelected(FreeObject: Boolean= FALSE); reintroduce;
function RemoveDuplicates: Integer;
function RemoveEmptyLines: Integer;
function RemoveLongLines(CONST iLength: integer): Integer; { Remove all lines that are longer than iLength }
procedure Trim;
procedure NeedScrollBar; { Calculate whether the ScrollBar is needed or not. Must call 'OnResize' 'OnAddItems' 'OnAssignItems 'OnChanged' }
procedure ScrollAtTheEnd;
function VisibleItems: Integer;
procedure LoadFromFile(FileName: string);
procedure SaveToFile (FileName: string);
procedure MoveItemsTo(ListBox: TCubicListBox; iCount: Integer); { Move the first x items from the curent listbox to the bottom of another listbox }
procedure AddLimit (Msg: string);
procedure Register;
```

## LightVcl.Visual.LogForm (14)

```pascal
procedure btnClearClick (Sender: TObject);
procedure chkLogOnErrorClick (Sender: TObject);
procedure chkShowDateClick (Sender: TObject);
procedure chkShowTimeClick (Sender: TObject);
procedure FormClose (Sender: TObject; var Action: TCloseAction);
procedure FormDestroy (Sender: TObject);
procedure mnuCopyAllClick (Sender: TObject);
procedure mnuCopyClick (Sender: TObject);
procedure mnuCopyFilteredClick (Sender: TObject);
procedure mnuCopySelectedClick (Sender: TObject);
procedure chkScrollDownClick(Sender: TObject);
procedure LoadSettings;
procedure SaveSettings;
procedure FormPostInitialize; override; // Called after the main form was fully initialized
```

## LightVcl.Visual.LstEditor (4)

```pascal
procedure SaveToFile (CONST FileName: string);
procedure LoadFromFile (CONST FileName: string);
procedure ClearAllRows;
procedure Register;
```

## LightVcl.Visual.Memo (55)

```pascal
procedure SetAutoScrollBars(Value: Boolean);
procedure WMSize(var Message: TWMSize); message WM_SIZE;
procedure Change; override;
procedure KeyDown (VAR Key: Word; Shift: TShiftState); override; { "Select All" (CTRL+A) functionality }
procedure KeyPress (VAR Key: Char); override;
procedure CreateWnd; override;
procedure SelectLine(Line : Integer); overload;
procedure SelectLine(aText: string); overload; { Select line containing specified text }
procedure SelectCurrentLine;
procedure RemoveSelection;
procedure MoveCaretToChar(CharIndex: Integer);
procedure Randomize;
procedure SortLines;
function VisibleLines: Integer;
function CountNonEmptyLines: Integer;
function CountWords: Integer;
procedure SwapLines(x, y: Integer);
procedure CopyToClipboardAll;
function Search(SrcStr: string): Boolean;
procedure AddInteger (Number: Integer);
procedure AddString (CONST aText: string);
procedure AddEntry (CONST aText: string; Number: Integer);
procedure AddSeparator;
function GetWordUnderCaret: string;
function SelectWordUnderCaret: string;
procedure WMSetText(var Message: TWMSetText); message WM_SETTEXT;
procedure WMPaste (var Message: TWMPaste); message WM_PASTE;
procedure RemoveLastEmptyLine;
procedure RemoveEmptyLines;
procedure RemoveEmptyLinesEx; { Applies trim before removing empty lines. This way more lines will become empty }
function RemoveDuplicates: Integer;
function RemoveLines (const BadWord : string; PartialMatch: Boolean): Integer; { Remove lines that contain the specified text }
function KeepLines (const KeepText: string): Integer; { Keep lines that contain the specified text }
procedure KeepFirstLines(const HowManyLines: Integer);
procedure Trim; { Trim empty spaces and control caracters at the begining/end of each line }
function FindLine(aText: string): Integer; { Find line containing the specified text } { Case insensitive }
procedure CenterInView(LineNum: Integer); overload; { Center the specified line in the middle of the view }
procedure CenterInView; overload;
procedure ScrollAtEnd;
procedure ScrollAtTop;
procedure MoveCursorAtEOL; { Move cursor at the end of the current row }
function CharToLine(const CharNo: Integer): Integer;
function CursorToChar(CONST MousePosition: TPoint): Integer;
function ConvertCaretToChar: Integer;
function CurLineToCharNo: Integer;
function LineToCharNo(const Line: Integer): Integer;
function CurrentLine: Integer; { Line containing the cursor }
procedure LineUp; { Move current line up }
procedure LineDown;
function LineLenght: Integer; { Returns number of characters that can fit on a line }
function MaxVisLines: Integer; { Returns the number of lines that fits in the TMemo }
procedure SetMargins(CONST LeftMargin, RightMargin, TopBottomMargin: Integer); { Set margins }
procedure UpdateAutoScrollBars; { Show/hide vertical scrollbar based on content. Requires ScrollBars = ssVertical or ssBoth }
function LoadFromFile(FileName: string): Boolean; { Loadtext from specified file IF the file exists. Otherwise, don't show an error }
procedure Register;
```

## LightVcl.Visual.MinimalPathLabel (4)

```pascal
procedure SetCaption(Value: string);
procedure UpdateMinimizedCaption;
procedure Resize; override;
procedure Register;
```

## LightVcl.Visual.MsgDispatcher (14)

```pascal
procedure setTime(Value: Integer);
procedure TimerTimer (Sender: TObject);
procedure ClickExecute(Sender: TObject);
procedure CloseExecute(Sender: TObject);
procedure MuteExecute (Sender: TObject);
procedure PlayTopMsg; { When there are no Winapi.Messages, I stop the timer }
procedure StartAnimation;
procedure CreateWnd; override;
procedure AddMessage(sMsg: string);
procedure ClearMessages; { Clear all existing messages }
procedure AddBlank;
procedure SetTimerNextRow; { Set time out based on the number of words in the next string that I have to display }
procedure SetImageList(Value: TImageList);
procedure Register;
```

## LightVcl.Visual.Panel (6)

```pascal
procedure Paint; override;
procedure ResetToFirstCtrl;
function FirstControl: TControl;
function NextControl: TControl; { Returns the controls in phisycal order (sorted by .Top) }
function LastControl: TControl;
procedure Register;
```

## LightVcl.Visual.PathEdit (33)

```pascal
procedure btnCreateClick (Sender: TObject);
procedure btnBrowseClick (Sender: TObject);
procedure btnExploreClick (Sender: TObject);
procedure btnApplyClick (Sender: TObject);
procedure edtPathChange (Sender: TObject); //override;
procedure edtKeyPress (Sender: TObject; var Key: Char);
procedure edtPathOnClick (Sender: TObject);
procedure edtKeyDown (Sender: TObject; var Key: Word; Shift: TShiftState);
procedure setFileList (const Value: TFileListBox);
procedure setDirListBox (const Value: TDirectoryListBox);
function canWriteToFolder(const Folder: string): Boolean;
procedure setPath (const Value: string);
procedure setInputType (const Value: TInputType);
procedure setOpenSrc (const Value: Boolean);
procedure setShowCreate (const Value: Boolean);
procedure setReadOnly (const Value: Boolean);
function getPath: string;
function isFilterStored: Boolean;
function getReadOnly: Boolean;
procedure inputTypeChanged;
procedure setShowApply(const Value: Boolean);
procedure PathChanged;
procedure CheckPathValidity; { Makes the control green when the path exists and red when it doesn't exist }
procedure Click; override;
procedure CreateWnd; override;
procedure SetEnabled(Value: Boolean); override;
procedure Notification(AComponent: TComponent; Operation: TOperation); override;
procedure NavigateUp;
function PathHasValidChars: string; { Returns '' when the input string (path) is valid and and error msg when it is not valid }
function PathIsValid: string; { Returns '' when the input string (path) is valid and and error msg when it is not valid }
function PathIsValidMsg: boolean; { Returns true when the input string (path) is valid and and error msg when it is not valid }
function GetFiles(CONST FileType: string; CONST ReturnFullPath, DigSubdirectories: Boolean; ExcludeFolders: TStrings= nil): TStringList;
procedure Register;
```

## LightVcl.Visual.ProxyList (13)

```pascal
function GetConnectionType: TConType;
procedure btnSaveProxyClick(Sender: TObject);
procedure ConectionTypeChanged(Sender: TObject);
procedure btnLocateClick(Sender: TObject);
procedure btnAutoDetectClick(Sender: TObject);
procedure btnCleanClick(Sender: TObject);
procedure CreateWnd; override;
procedure LoadProxyFile(CONST aFileName: string);
procedure Save; { Save content to disk }
procedure LoadSettings (CONST IniFile: TIniFileEx);
procedure SaveSettings (CONST IniFile: TIniFileEx);
procedure CleanList;
procedure Register;
```

## LightVcl.Visual.RadioButton (6)

```pascal
procedure AdjustBounds;
procedure setAutoSize(b: Boolean); reintroduce;
procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
procedure Loaded; override;
procedure Register;
```

## LightVcl.Visual.RichEdit (9)

```pascal
procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
procedure AddFormated(s: string); { Accepts string that contain enters }
procedure CopyAll;
procedure Add(s: string); { Use it instead of Self.Lines.Add() }
procedure ScrollToBottom; { Scroll at the end of the text }
procedure ScrollTo(Line: Integer);
function GetCurrentLine: Integer; { The line containing the caret }
function GetFirstVisibleLine: Integer;
procedure Register;
```

## LightVcl.Visual.RichEditResize (4)

```pascal
procedure ResizeRichEdit;
procedure CreateWnd; override;
procedure Change; override;
procedure Register;
```

## LightVcl.Visual.RichLog (27)

```pascal
function getVerbosityI: Integer;
procedure setVerbosityI(const Value: Integer);
procedure Add(s: string);
procedure AddFormated(s: string);
procedure ScrollDown;
function IsDark: Boolean;
procedure addColorMsg (CONST Mesaj: string; TextColor: TColor);
procedure RemoveLastEmptyRows;
procedure CopyAll;
procedure SaveAsRtf (CONST FullPath: string);
procedure AppendToFile(CONST FullPath: string);
function LoadFromFile(CONST FullPath: string): Boolean;
procedure AddVerb (CONST Mesaj: string);
procedure AddHint (CONST Mesaj: string);
procedure AddInfo (CONST Mesaj: string);
procedure AddImpo (CONST Mesaj: string);
procedure AddWarn (CONST Mesaj: string);
procedure AddError (CONST Mesaj: string);
procedure AddBold (CONST Mesaj: string);
procedure AddMsg (CONST Mesaj: string); overload;
procedure AddMsg (CONST Mesaj: string; MsgType: TLogVerb); overload;
procedure AddMsgInt (CONST Mesaj: string; i: Integer);
procedure AddInteger (CONST i: Integer);
procedure AddFromFile (CONST FileName: string; MsgType: TLogVerb);
procedure AddEmptyRow;
procedure AddDateStamp; { Adds the date/time, on a new line }
procedure Register;
```

## LightVcl.Visual.RichLogTrack (6)

```pascal
function getVerbosity: TLogVerb;
procedure setVerbosity(Value: TLogVerb);
procedure setRichLog (const Value: TRichLog);
procedure CreateWnd; override;
procedure TrackBarChange(Sender: TObject);
procedure Register;
```

## LightVcl.Visual.RichLogUtils (2)

```pascal
function Verbosity2String(Verbosity: TLogVerb): string;
function Verb2Color(Verb: TLogVerb; IsDark: Boolean = False): TColor;
```

## LightVcl.Visual.RichRamLog (28)

```pascal
procedure setVerbosity(const Value: TLogVerb);
procedure addNewMessage(const Level, Mesaj: string);
function getContent(Line: string): string;
function getLog(MsgType: Char): string; { UNUSED }
function getText: string;
procedure setText(Value: string);
procedure Clear(ClearVisualLog: Boolean);
procedure Update;
function Count: Integer;
procedure importRawData(const RamLogRawLines: string); { Access to the unformated lines of the log } { One ore more lines of data in TRamLog format (#1# text_text_text). Used by Baser to restore Log from disk }
procedure LoadFromStream(Stream: TLightStream); overload;
procedure LoadFromStream(Stream: TCubicMemStream); overload;
procedure SaveToStream (Stream: TLightStream); overload;
procedure SaveToStream (Stream: TCubicMemStream); overload;
procedure AddBold (CONST Mesaj: string);
procedure AddMsgInt (CONST Mesaj: string; i: Integer);
procedure AddMsgLvl (CONST Mesaj: string; MsgType: TLogVerb);
procedure AddEmptyRow;
procedure AddMsg (CONST Mesaj: string);
procedure AddVerb (CONST Mesaj: string);
procedure AddHint (CONST Mesaj: string);
procedure AddInfo (CONST Mesaj: string);
procedure AddImpo (CONST Mesaj: string);
procedure AddWarn (CONST Mesaj: string);
procedure AddError (CONST Mesaj: string);
procedure Append (RamLog: TRamLog);
procedure ExportTo (aRichLog: TRichLog);
procedure SaveAsTextFile(CONST FullPath: string); { Save plain text to file }
```

## LightVcl.Visual.ScrollBox (10)

```pascal
Procedure WMVScroll(Var Message : TWMScroll); message WM_VScroll;
Procedure WMHScroll(Var Message : TWMScroll); message WM_HScroll;
Procedure VScroll(Pos: integer; EventType : TVScrollEventType); virtual;
Procedure HScroll(Pos: integer; EventType : THScrollEventType); virtual;
function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override; {DOESN'T WORK!}
procedure ResetToFirstCtrl;
function FirstControl: TControl;
function NextControl: TControl; { Returns the controls in physical order (sorted by .Top) }
function LastControl: TControl;
procedure Register;
```

## LightVcl.Visual.SpinEdit (13)

```pascal
procedure Change; override;
function getValue: Integer;
procedure setValue (const Value: Integer);
procedure setCaption1(const Value: string);
procedure setCaption2(const Value: string);
function getCaption1: string;
function getCaption2: string;
procedure SetWidth;
procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
procedure CreateWnd; override;
procedure CreateWindowHandle(const Params: TCreateParams); override;
procedure SetParent(aParent: TWinControl); override; { SetParent is called during construction AND also during destruction with aParent=nil }
procedure Register;
```

## LightVcl.Visual.SpinEditDelayed (5)

```pascal
procedure TimerTimer(Sender: TObject);
procedure SetDelay(CONST aValue: Integer);
procedure Change; override;
procedure StopTimer; { Used by LoadForm to stop the even from triggering some seconds later after the value of this spinbox was loaded from the INI file }
procedure Register;
```

## LightVcl.Visual.Splitter (18)

```pascal
procedure AllocateLineDC;
procedure CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
procedure DrawLine;
function FindControl: TControl;
procedure FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
procedure ReleaseLineDC;
procedure SetBeveled(Value: Boolean);
procedure UpdateControlSize;
procedure UpdateSize(X, Y: Integer);
function CanResize(var NewSize: Integer): Boolean; reintroduce;
function DoCanResize(var NewSize: Integer): Boolean; virtual;
procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
procedure Paint; override;
procedure RequestAlign; override;
procedure StopSizing; dynamic;
procedure Register;
```

## LightVcl.Visual.StatusBar (1)

```pascal
procedure Register;
```

## LightVcl.Visual.StringGrid (33)

```pascal
procedure setHighlight (CONST Value: string);
procedure WndProc(VAR Message: TMessage); override;
procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
function MouseOnSortPos (MouseX, MouseColumn: Integer): Boolean; { This is a helper function for Sort. It checks if the mouse has the right coordinates. If the mouse is between two cells it means the user wants to resize the cell not to sort it }
procedure NaturalSort (SortCol: Integer); { In this procedure the algorithm expects numbers on that colum and not text }
procedure FastSort (SortCol: Integer; CaseSensitive: Boolean);
procedure CreateWnd; override;
procedure SortColumn (CONST ColumnToSort: Integer);
procedure SwapRowText(i, j: Integer);
procedure ReverseOrder; { Swap ROWS (and data/objects associated with those rows) }
procedure Save;
procedure SaveAsCsv (CONST aFileName: string; CONST Delimiter: Char= ','); { Save the entire content to disk (including headers). The difference between this and SaveToFile is that SaveToFile also save the size of }
procedure SaveToFile (CONST aFileName: string);
function LoadFromFile (CONST aFileName: string): Boolean;
function LoadHeaderWidths (CONST aFileName: string): Boolean;
procedure SaveHeaderWidths (CONST aFileName: string);
function GetContentAsHTML (Truncate: Integer): string; overload;
function GetContentAsHTML (Rectangle: TRect; SplitEvery: Integer) : string; overload;
function GetContent (Rectangle: TRect; Delimiter: Char= ','): string;
function GetAllContent (Delimiter: Char= ','): string;
function GetSelectionContent(Delimiter: Char= ','): string; { Returns the content of cell in the specified rectangle. The cells are separated by 'Delimiter' }
procedure CopySel2Clipboard (Delimiter: Char= Tab);
procedure CopyColumn;
procedure PasteFromClipboard; { untested }
procedure ImportColumnFromClpbrd; { Put the clipboard content into the grid, at the current Column, starting at the current Row. }
procedure ImportCSV(CsvBody: AnsiString; Separator: Char= ','); {$ENDIF AllowCSV}
function WholeGridRect: TRect;
procedure ToggleHeader; { Make the top horizontal header visible/invisible }
procedure Help; { how to use this grid }
procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
procedure ExpandCenteredColumns;
procedure EndEdit (ACol, ARow: Longint); { NOTA: Nu merge daca AlwaysShowEditro= True }
procedure Register;
```

## LightVcl.Visual.StringGridBase (64)

```pascal
procedure setAutoRowHgt (const Value: Boolean); { Update row's height based on the size of the current font used }
procedure setSelColor (CONST Value: TColor);
procedure setClrTextDisabled (CONST Value: TColor);
procedure setClrTextEnabled (CONST Value: TColor);
procedure setCursorTextColor (CONST Value: TColor);
procedure setContentHints (CONST Value: Boolean);
function getCount : Integer;
procedure doAutoRowHeight;
procedure SizeLargestColumn;
procedure setCursorPos(CONST Value: Integer);
function getCursorPos: Integer;
function SelectCell(ACol, ARow: Integer): Boolean; override;
function FixCursor: Integer;
procedure DrawCellText(aCol: Integer; aRect: TRect; s: string);
procedure KeyDown (var Key: Word; Shift: TShiftState); override;
procedure DrawChevron(x, y: Integer);
procedure CreateWnd; override;
procedure Clear; virtual;
procedure ClearAll;
procedure SetProportionalScroll; { NETESTATA ! } { Getting a TScrollbar control to Show a proportional thumb }
function IsVerticalScrollVisible: Boolean;
function IsHorizontalScrollVisible: Boolean;
procedure MouseMove (Shift: TShiftState; X, Y: Integer); override;
function FindNext (CONST Text: string; CaseSens: boolean): Boolean; { Search all cells. Highlight the cell where text was found. If the text could not be found anymore, start from the beginning. }
function FindOnRow (ARow: Integer; CONST Text: string; CaseSens: boolean): Integer; { intoarce coloana in care a gasit string-ul }
function FindOnCol (CONST AColumn: Integer; CONST Text: string; CaseSens: boolean): integer; { intoarce linia in care a gasit string-ul }
function FindUniqueEntries (CONST ByColumn: integer): Integer; { retruns the number of unique entries on the specified column. For example if that colum 1 contains name of persons, it will return how many uqinue name appears in the table }
procedure SelectAll;
function SelCount: Integer;
function MoveSelectedUp : Boolean; { Move selected row up one position. Returns TRUE if was possible to move the row up }
function MoveSelectedDown: Boolean; { Move selected row dwn one position. Returns TRUE if was possible to move the row dwn }
procedure MoveCursorDown;
procedure MoveCursorUp; { Move cursor to the previous row (if the up row is not header or -1 ) }
function IsEmpty: Boolean;
function GetItem (Index: Integer): TObject; virtual;
procedure DeleteRow (ARow: Longint); override; //reintroduce;
procedure DeleteCol (ACol: Longint); virtual;
procedure DeleteSelectedRows;
procedure FreeObject(const Line: integer);
procedure DeleteCurrentRow; dynamic;
procedure InsertRow (BelowRow: Integer);
procedure InsertColumn(AfterColumn: Integer);
procedure AddRowAndFocus;
procedure ClearRow(aRow: Integer);
function RowIsVisible (aRow: integer): Boolean;
procedure ShowCellHint (X,Y:Integer); { pop-up a hint with the cell's content }
procedure CMFontChanged(VAR Message: TMessage); message CM_FONTCHANGED;
procedure ClearObjAssignment;
function ResizeColText (ACol: Integer): Integer;
function ResizeColHeader(ACol: Integer): Integer;
procedure ResizeColHeaders; { Resize all columns to match the length of the header text }
procedure ResizeToFitAll; { Resize column to match the longes string found in any of its cells } { Hasn't been tested } { A resize is also done in HeaderCell }
procedure ScrollToBottom;
procedure ScrollTo(aRow: Integer);
procedure FixFixedRow;
procedure InvalidateCurRow;
procedure InvalidateRow(ARow: Longint);
procedure InvalidateCol(ACol: Longint);
procedure InvalidateCell(ACol, ARow: Longint);
procedure InvalidateGrid;
procedure FillCell(ACol, ARow: Integer; aColor: TColor); overload; { Fill color }
procedure FillCell(Rect: TRect; aColor: TColor; State: TGridDrawState); overload;
procedure HeaderCell(ACol: Integer; const Value: string; AutoSize: Boolean= TRUE); { Write text to the header (Row 0) and autoresize the column to match the text inside the header }
procedure Register;
```

## LightVcl.Visual.StringGridScrollFix (3)

```pascal
procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
function SelectCell(ACol, ARow: Longint): Boolean; override;
```

## LightVcl.Visual.StrongPasswordEdit (4)

```pascal
procedure UpdateBackgroundColor;
procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
procedure Change; override;
procedure Register;
```

## LightVcl.Visual.ThumbViewerM (32)

```pascal
procedure setRecord(index: Integer; Pointer: ThumbPtr);
function getRecord(index: Integer): ThumbPtr;
procedure ReleaseBitmap(const Index: Integer); { UNUSED }{ Release the cache at specified position }
procedure Clear; override;
function GetRamSize (Phumb: ThumbPtr): Int64; { Returns memory requirements for specified record }
function GetTotalRamSize: Int64;
procedure Resize; override;
procedure DrawCell(aCol, aRow: Longint; aRect: TRect; aState: TGridDrawState); override;
procedure TopLeftChanged;override;
procedure CreateWnd; override;
function getActiveItem: Integer; { Unused }{ Retuns the Index of the seleted thumb }
procedure setActiveItem(CONST Index: Integer); { Unused }
procedure Clear;
procedure SetCellSize;
procedure SetThumbWidth (CONST Width : integer);
procedure SetThumbHeight (CONST Height: integer);
procedure SetCellSpacing (CONST Value: integer);
procedure ComputeCellCount;
function NecessaryRows: Integer; { Number of rows necessary to display all images }
function HasThumb (CONST Index: Integer): Boolean; { UNUSED }
function Thumb (CONST Index: Integer): TThumb; { UNUSED }
function GetPhumb (CONST ACol, ARow: Integer): ThumbPtr;
function GetSelectedPhumb: ThumbPtr; { Get phumb for selected cell }
function SelectedFile: string;
function Cells2ListIndex(CONST ACol, ARow: Integer): Integer;
procedure ListIndex2Cells(Index: Integer; OUT ACol, ARow: Integer);
procedure LoadFolder (CONST Dir: string);
procedure AddPicture (CONST FilePath: string); { Renamed from LoadFromFile } { Append a single thumbnail at the end of an existing list of thumbnails }
procedure AssignThumbsToCells; { Refresh Grid }
procedure WMThumbnailReady(var AMessage: TMessage); message WM_THUMBNAIL_NOTIFY; { Receive message from worker thread }
function Statistics: string;
procedure Register;
```

## LightVcl.Visual.TimeLine (9)

```pascal
function GetDuration: Integer;
procedure SetDuration(Value: Integer);
procedure SetMarkerPos(Value: Integer);
procedure ProgressMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
function PixelsPerUnit: Double;
procedure UpdateMarkerPosition;
procedure Loaded; override;
procedure Resize; override;
procedure Register;
```

## LightVcl.Visual.Timer (5)

```pascal
procedure Restart; { Sets the countdown back to zero and starts the timer }
procedure Reset; { Sets the countdown back to zero and starts the timer ONLY if was enabled before }
procedure Stop;
procedure ResetTimer(Timer: TTimer);
procedure Register;
```

## LightVcl.Visual.ToolBox (9)

```pascal
procedure CloseButtonClick(Sender: TObject);
procedure TopBarMouseDown (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
procedure TopBarMouseMove (Sender: TObject; Shift: TShiftState; X, Y: Integer);
procedure TopBarMouseUp (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
function getCaption: string;
procedure setCaption(CONST Value: string);
procedure CreateWnd; override;
procedure ShowIt(Parent: TControl);
procedure Register;
```

## LightVcl.Visual.TrayIcon (4)

```pascal
procedure PutIconInTaskbar;
procedure PutIconInSystray;
procedure PutIconInSystrayBalloon; { This will also show the balloon IF BalloonHint is not empty. }
procedure Register;
```

_2889 public routines across 216 units._
