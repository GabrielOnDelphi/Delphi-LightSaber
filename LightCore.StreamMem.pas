UNIT LightCore.StreamMem;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Extends TMemoryStream.
   Allows you to read and write data directly into a memory stream.

   Article: www.GabrielMoraru.com/saving-an-object-to-disk-file
--------------------------------------------------------------------------------------------------------------

   [FEATURES]

      This class adds new functionality that does not exist in Delphi's original stream classes:
       - Stream versioning
       - Read/Write Boolean
       - Read/Write String
       - Read/Write Integer
       - Read/Write Cardinal
       - Read/Write Date
       - Read/Write Mac files (inverted endianness)
       - etc

      Stream versioning:
        This class offers embedded support for file versioning and signature (magic number).
        By using WriteHeader you can put a unique identifier in each of your file.
        Later you can user ReadHeader to verify that your program is trying to read a file that is yours. This prevent the user from feeding the wrong file an input into your program.
        Even more, this class allows you to expand your class - add new fields that are saved to disk while keeping the file format backward compatible.


    [HOW TO USE IT]

       Call Load in your class constructor.
       Call Save in your class destructor.

       procedure TMyClass.Load(Stream: TCubicMemStream);
       begin
         if NOT Stream.ReadHeader('MySignature', 1)          // Is the file valid?
         then RAISE Exception.Create('Cannot load data!');
         FSomeField:= Stream.ReadInteger;                    // Save your fields
         ...
       end;

       procedure TLessons.Save(Stream: TCubicMemStream);
       begin
         Stream.WriteHeader('MySignature', 1);
         Stream.WriteInteger(FSomeField);                    // Write your fields
         ...
       end;

       See the attached demo project for a full example.

--------------------------------------------------------------------------------------------------------------

   WARNING
      TMemoryStream loads the entire contents of a file into the memory so don't use it with large files.
      For large files or buffered access, use TLightStream from LightCore.StreamBuff.pas instead.

--------------------------------------------------------------------------------------------------------------

   Read vs ReadBuffer:
      Read treats Count as an upper bound.
      The ReadBuffer, by contrast, raises an exception if Count bytes cannot be read.
      So this is better if I don't accept errors in my files.

   Also see:
      TBinaryReader / TBinaryWriter
         http://docwiki.embarcadero.com/CodeExamples/Tokyo/en/TBinaryReader_and_TBinaryWriter_(Delphi)
         Disadvantage: the class can only read or write from a file (not both at the same time).
      TLightStream
         Buffered file stream version

--------------------------------------------------------------------------------------------------------------

   DEMO/TESTER
      LightSaber\Demo\Core\Demo LightCore StreamBuffer\Demo_FileStream.dpr

=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Classes, System.Types, System.Math, LightCore, LightCore.Types;

TYPE
  TCubicMemStream= class(TMemoryStream)
   private
     CONST LisaMagicNumber: Cardinal= $6153694C;
     CONST FrozenPaddingSize = 64;                // NEVER-EVER MODIFY THIS CONSTANT! All files saved with this constant will not work anymore. Enough for 16 Integer variables.
     function ReadSignature: AnsiString;          // The LiSa string for "Light Saber'.  // Old number: $4C695361
   public
     StringListSafetyLimit: Cardinal;             // Don't try to read more than this
     constructor CreateFromStream(Stream: TMemoryStream; FromPos: Int64 = 0);

     { Header }
     procedure WriteHeader (CONST Signature: AnsiString; Version: Word);
     function  ReadHeader  (CONST Signature: AnsiString; Version: Word): Boolean;  overload;
     function  ReadHeader  (CONST Signature: AnsiString): Word;                    overload;

     { Check point }
     procedure WriteCheckPoint(CONST s: AnsiString= '');
     procedure ReadCheckPointE(CONST s: AnsiString= '');     // Raises an exception
     function  ReadCheckPoint (CONST s: AnsiString= ''): Boolean;

     function  ReadEnter: Boolean;
     procedure WriteEnter;

     { Padding }
     procedure ReadPadding0           (Bytes: Integer= FrozenPaddingSize);    // Does not check them for validity
     procedure ReadPaddingValidation  (Bytes: Integer= FrozenPaddingSize);    // Raises an exception if the buffer does not contain the signature
     procedure WritePaddingValidation (Bytes: Integer= FrozenPaddingSize);    // Raises an exception if the padding does not match the SafetyPaddingStr string. Usefule to detect file corruption. }
     procedure WritePadding0          (Bytes: Integer= FrozenPaddingSize);    // Writes zeroes as padding bytes.

     { Numeric }
     function  ReadBoolean : Boolean;
     function  ReadByte    : Byte;
     function  ReadCardinal: Cardinal;
     function  ReadDate    : TDateTime;                      // This is a DOUBLE
     function  ReadDouble  : Double;
     function  ReadInteger : Integer;
     function  ReadInt64   : Int64;
     function  ReadShortInt: ShortInt;
     function  ReadSingle  : Single;
     function  ReadSmallInt: Smallint;
     function  ReadUInt64  : UInt64;
     function  ReadWord    : Word;

     procedure WriteBoolean  (b: Boolean);
     procedure WriteByte     (b: Byte);
     procedure WriteCardinal (c: Cardinal);
     procedure WriteDate     (d: TDateTime);
     procedure WriteDouble   (d: Double);
     procedure WriteInt64    (i: Int64);
     procedure WriteInteger  (i: Integer);
     procedure WriteShortInt (s: ShortInt);
     procedure WriteSingle   (s: Single);
     procedure WriteSmallInt (s: SmallInt);
     procedure WriteUInt64   (i: UInt64);
     procedure WriteWord     (w: Word);

     { Complex structures }
     function  ReadRect: TRect;
     procedure WriteRect(Rect: TRect);

     function  ReadRectF: TRectF;
     procedure WriteRectF(Rect: TRectF);

     procedure ReadIntegers (out List: TIntegerArray);
     procedure WriteIntegers(const List: TIntegerArray);

     procedure ReadDoubles (out List: TDoubleArray);
     procedure WriteDoubles(const List: TDoubleArray);

     { Reverse read }
     function  RevReadCardinal: Cardinal;                                   { REVERSE READ - read 4 bytes and swap their position. For Motorola format. }
     function  RevReadInteger : Integer;
     function  RevReadSmallInt: SmallInt;
     function  RevReadWord    : Word;                                       { REVERSE READ - read 2 bytes and swap their position. For Motorola format. }

     { Unicode }
     procedure WriteString  (CONST s: string);
     function  ReadString(SafetyLimit: Cardinal = 1*KB): string;  overload;

     { ANSI }
     procedure WriteStringA  (CONST s: AnsiString);
     function  TryReadStringA(Count: Cardinal): AnsiString;                { This is the relaxed version. It won't raise an error if there is not enough data (Len) to read }
     function  ReadStringACnt(Count: Cardinal; SafetyLimit: Cardinal): AnsiString;       { It will raise an error if there is not enough data (Len) to read }
     function  ReadStringA   (SafetyLimit: Cardinal = 1*KB): AnsiString;                        { It automatically detects the length of the string }

    { TSL }
     function  ReadStrings: TStringList;                       overload;
     procedure ReadStrings  (TSL: TStrings);                   overload;
     procedure WriteStrings (TSL: TStrings);

    { Chars }
     procedure WriteChars   (CONST s: AnsiString);             overload;
     procedure WriteChars   (CONST s: string);                 overload;
     function  ReadCharsA   (Count: Cardinal; SafetyLimit: Cardinal = 1*KB): AnsiString;
     function  ReadChars    (Count: Cardinal): string;

     { Char }
     procedure WriteChar(CONST c: AnsiChar);
     function  ReadChar: AnsiChar;

     { Strings without length }
     procedure PushString   (CONST s: string);
     function  ReadStringCnt   (Count: Cardinal; SafetyLimit: Cardinal = 1*KB): string;     overload;     { Read 'Len' characters }

     { Raw }
     function  AsBytes: TBytes;
     function  AsString: AnsiString;

     procedure PushBytesCnt (CONST Buffer: TBytes);
     function  ReadByteChunk: TBytes;

     procedure PushAnsi (CONST s: AnsiString);
     procedure PushBytes(CONST Bytes: TBytes);
     procedure PushData (CONST s: AnsiString);        overload;            { Clears stream first, then writes }
     procedure PushData (CONST Bytes: TBytes);        overload;            { Clears stream first, then writes }

     { Disk }
     procedure LoadFromFile(CONST FileName: string);
     procedure SaveToFile  (CONST FileName: string);
  end;


function StringFromStream(MemStream: TMemoryStream; Count: Integer= 0; Pos: Integer= 0): string;


IMPLEMENTATION
USES
   LightCore.Binary, LightCore.AppData, LightCore.StreamBuff;


{--------------------------------------------------------------------------------------------------
   CTOR
--------------------------------------------------------------------------------------------------}

{ Creates the object and copies data from the specified stream.
  FromPos = 0 (default): copy entire stream
  FromPos > 0: copy from that position to end
  Pass Stream.Position to copy from current position (old CreateFromStreamPos behavior) }
constructor TCubicMemStream.CreateFromStream(Stream: TMemoryStream; FromPos: Int64 = 0);
var
  SavedPos: Int64;
  BytesToCopy: Int64;
begin
  inherited Create;
  Assert(Stream <> NIL);
  StringListSafetyLimit:= 1 * MB;

  if FromPos < 0 then RAISE Exception.Create('TCubicMemStream!');

  BytesToCopy:= Stream.Size - FromPos;
  if BytesToCopy <= 0 then EXIT;

  // Save source position, seek to FromPos
  SavedPos:= Stream.Position;
  Stream.Position:= FromPos;

  CopyFrom(Stream, BytesToCopy);

  // Restore source stream position
  Stream.Position:= SavedPos;

  Position:= 0;
end;



{-------------------------------------------------------------------------------------------------------------
   HEADER FORMAT:

     4 bytes (Card): LiSa (always the same)
     4 bytes (Card): Length of the magic signature
       bytes (Ansi): Magic signature
     2 bytes (Word): File version number.

     This new file header is more reliable because we check
       the magic number  - this is fixed for all files
       the signature
       the file version

--------------------------------------------------------------------------------------------------------------
     'L' = 0x4C
     'i' = 0x69
     'S' = 0x53
     'a' = 0x61
-------------------------------------------------------------------------------------------------------------}

procedure TCubicMemStream.WriteHeader(CONST Signature: AnsiString; Version: Word);
begin
  Assert(Length(Signature) <= 64, 'The Signature cannot be larger the 64 chars!');
  WriteCardinal  (LisaMagicNumber);  // Write fixed magic no  "LiSa"
  WriteStringA   (Signature);        // Write signature
  WriteWord      (Version);          // Write the file version number
end;


{ Returns the version or 0 in case of error.
  No exception will be raised unless when the file is smaller than what we want to read. }
function TCubicMemStream.ReadHeader(CONST Signature: AnsiString): Word;
VAR
  MagicNo: Cardinal;
  FileSignature: AnsiString;
begin
  Assert(Signature > '', 'TCubicMemStream - No signature provided!');

  // Read "LiSa" magic no
  TRY
    MagicNo:= ReadCardinal;
  EXCEPT
    on E: Exception DO
      begin
        if AppDataCore <> NIL
        then AppDataCore.LogError('Cannot read magic number for: ' + String(Signature) + ' - ' + E.Message);
        EXIT(0);
      end;
  END;
  if MagicNo <> LisaMagicNumber then EXIT(0);

  // Read signature
  TRY
    FileSignature:= ReadSignature;
  EXCEPT
    on E: Exception DO
    begin
      if AppDataCore <> NIL
      then AppDataCore.LogError('Cannot read stream signature for: ' + String(Signature) + ' - ' + E.Message);
      EXIT(0);
    end;
  END;
  if FileSignature = '' then
    begin
      if AppDataCore <> NIL
      then AppDataCore.LogError('Cannot read file signature: ' + string(Signature));
      EXIT(0);
    end;
  if FileSignature <> Signature then
    begin
      if AppDataCore <> NIL
      then AppDataCore.LogError('Signature mismatch: ' + string(Signature));
      EXIT(0);
    end;

  // Read the version number
  TRY
    Result:= ReadWord;
  EXCEPT
    on E: Exception DO
    begin
      if AppDataCore <> NIL
      then AppDataCore.LogError('Cannot read stream version for: ' + String(Signature) + ' - ' + E.Message);
      EXIT(0);
    end;
  END;
end;


{ Returns True if signature & version number matches. }
function TCubicMemStream.ReadHeader(CONST Signature: AnsiString; Version: Word): Boolean;
VAR FileVersion: Word;
begin
  Assert(Version> 0 , 'TCubicMemStream - Version must be > 0!');

  FileVersion:= ReadHeader(Signature);
  Result:= (FileVersion > 0) AND (Version = FileVersion);
end;


{ A dedicated function to read the signature.
  It raises an error if we try to read too many chars, which can happen when we read random data (file corrupted or version changed) }
function TCubicMemStream.ReadSignature: AnsiString;
VAR Count: Cardinal;
begin
  // First, find out how many characters to read
  ReadBuffer(Count, SizeOf(Count));

  // Check size
  if Count > 64 then
    begin
      if AppDataCore <> NIL
      then AppDataCore.LogError('ReadSignature: Signature larger than 64 bytes: '+ IntToStr(Count)+' bytes');
      EXIT('');
    end;

  // Enough data to read?
  if Count > Size- Position then
    begin
      if AppDataCore <> NIL
      then AppDataCore.LogError('ReadSignature: Signature length > file size!');
      EXIT('');
    end;

  // Do the actual string reading
  Result:= ReadStringACnt(Count, 128);
end;



{-------------------------------------------------------------------------------------------------------------
   Write a checkpoint into the stream.
   Used for debugging.
   Write this from time to time to your file so if you screwup, you check from time to time to see if you are still reading the correct data.
-------------------------------------------------------------------------------------------------------------}
CONST
   ctCheckPoint= '<*>Checkpoint<*>';


{ For debugging. Write a checkpoint entry (just a string) from time to time to your file so if you screw up, you check from time to time to see if you are still reading the correct data. }
procedure TCubicMemStream.ReadCheckPointE(CONST s: AnsiString= '');
begin
  if NOT ReadCheckPoint(s)
  then RAISE Exception.Create('Checkpoint failure! ' + CRLF + string(s));
end;

function TCubicMemStream.ReadCheckPoint(CONST s: AnsiString= ''): Boolean;
begin
  Result:= ReadStringA = ctCheckPoint+ s;
end;

procedure TCubicMemStream.WriteCheckPoint(CONST s: AnsiString= '');
begin
  WriteStringA(ctCheckPoint+ s);
end;




{--------------------------------------------------------------------------------------------------
   PADDING
   It is important to leave some space at the end of your file (aka padding bytes).
   If you later (as your program evolves) need to save extra data into your file,
     you use the padding bytes. This way you don't need to change your file format.
-------------------------------------------------------------------------------------------------------------}

// Writes zeroes as padding bytes.
procedure TCubicMemStream.WritePadding0(Bytes: Integer);
VAR b: TBytes;
begin
  if Bytes> 0 then
    begin
      SetLength(b, Bytes);
      FillChar (b[0], Bytes, #0);
      WriteBuffer(b[0], Bytes);
    end;
end;


{ Reads padding bytes. For backwards compatibility, does NOT validate the content.
  Use ReadPaddingValidation if you need validation. }
procedure TCubicMemStream.ReadPadding0(Bytes: Integer);
VAR b: TBytes;
begin
  if Bytes> 0 then
    begin
      Assert(Bytes + Position <= Size, 'Read beyond stream!');
      SetLength(b, Bytes);
      ReadBuffer(b[0], Bytes);
    end;
end;



CONST
  SafetyPaddingStr: AnsiString= '<##LightSaber - Pattern of exactly 64 bytes for safety check.##>';   //This string is exactly 64 chars long

{ Read/write a string as padding bytes.
  ReadPadding raises an exception if the padding does not match the SafetyPaddingStr string. Usefule to detect file corruption. }
procedure TCubicMemStream.WritePaddingValidation(Bytes: Integer);
VAR
  b: TBytes;
  i, CheckPointSize: Integer;
begin
  SetLength(b, Bytes);
  CheckPointSize:= Length(SafetyPaddingStr);

  // Copy the string to the byte array (up to the available bytes or string length)
  for i := 0 to Min(Bytes, CheckPointSize) - 1
    do b[i] := Byte(SafetyPaddingStr[i + 1]);

  // Fill the rest of the buffer with zeros
  if Bytes > CheckPointSize
  then FillChar(b[CheckPointSize], Bytes - CheckPointSize, #0);

  // Write the buffer to the stream
  WriteBuffer(b[0], Bytes);
end;


{ Reads padding bytes WITH validation. Raises an exception if the buffer does not contain the signature. }
procedure TCubicMemStream.ReadPaddingValidation(Bytes: Integer);
VAR
  b: TBytes;
  CheckPointSize: Integer;
  i: Integer;
  MinSize: Integer;
begin
  if Bytes > 0 then
  begin
    Assert(Bytes + Position <= Size, 'Read beyond stream!');

    SetLength(b, Bytes);
    ReadBuffer(b[0], Bytes);
    CheckPointSize:= Length(SafetyPaddingStr);

    // Check if the beginning of the buffer matches the string
    MinSize:= Min(CheckPointSize, Bytes);
    for i := 0 to MinSize- 1 do
        if b[i] <> Byte(SafetyPaddingStr[i + 1])
        then RAISE Exception.Create('Invalid checkpoint!!');
  end;
end;




{-----------
   ENTER
------------}
function TCubicMemStream.ReadEnter: Boolean;   { Returns TRUE if the byte read is CR LF }
VAR Byte1, Byte2: Byte;
begin
  ReadBuffer(Byte1, 1);
  ReadBuffer(Byte2, 1);
  Result:= (Byte1= Byte(#13)) AND (Byte2= Byte(#10));
end;


procedure TCubicMemStream.WriteEnter;
VAR
  Byte1, Byte2: Byte;
begin
  Byte1 := Byte(#13);  // CR
  Byte2 := Byte(#10);  // LF
  WriteBuffer(Byte1, 1);
  WriteBuffer(Byte2, 1);
end;




{--------------------------------------------------------------------------------------------------
   UNICODE STRINGS
--------------------------------------------------------------------------------------------------}
procedure TCubicMemStream.WriteString(CONST s: string);
VAR
  Count: cardinal;
  UTF: UTF8String;
begin
  UTF := UTF8String(s);

  { Write length }
  Count := Length(UTF);
  WriteBuffer(Count, SizeOf(Count));

  { Write string }
  if Count > 0
  then WriteBuffer(UTF[1], Count);
end;


function TCubicMemStream.ReadString(SafetyLimit: Cardinal = 1*KB): string;   { Works for both Delphi7 and Delphi UNICODE }
VAR
   Count: Cardinal;
   UTF: UTF8String;
begin
  ReadBuffer(Count, 4);                        { Read length }

  if Count > SafetyLimit
  then RAISE Exception.CreateFmt('String too large: %d bytes', [Count]);

  if Count > 0
  then
    begin
      SetLength(UTF, Count);                   { Read string }
      ReadBuffer(UTF[1], Count);
      Result:= string(UTF);
    end
  else
    Result:= '';
end;




{--------------------------------------------------------------------------------------------------
   CHARS
--------------------------------------------------------------------------------------------------}

{ Writes raw characters to file (no length prefix).
  Unlike WriteStringA, this does NOT write the string length first.
  Use when writing C-style strings or fixed-length character data. }
procedure TCubicMemStream.WriteChars(CONST s: AnsiString);
begin
  if Length(s) > 0
  then WriteBuffer(s[1], Length(s));
end;


procedure TCubicMemStream.WriteChars(CONST s: string);                     { Works for both Delphi7 and Delphi UNICODE }    // old name: WriteStringANoLen
VAR UTF: UTF8String;
begin
  UTF := UTF8String(s);
  if Length(UTF) > 0
  then WriteBuffer(UTF[1], Length(UTF));
end;


function TCubicMemStream.ReadChars(Count: Cardinal): string;        { Works for both Delphi7 and Delphi UNICODE }
VAR UTF: UTF8String;
begin
  if Count < 1 then EXIT('');

  SetLength(UTF, Count);
  ReadBuffer(UTF[1], Count);
  Result:= string(UTF);
end;


{ Reads raw characters from file (without length prefix).
  Count specifies how many bytes to read.
  SafetyLimit prevents reading excessively large strings.
  Returns empty string if Count is 0.
  
  Used for reading C++ strings (the length of the string is not written to disk)  }
function TCubicMemStream.ReadCharsA(Count: Cardinal; SafetyLimit: Cardinal = 1*KB): AnsiString;
begin
  if Count = 0 then EXIT('');

  if Count > SafetyLimit
  then RAISE Exception.CreateFmt('String too large: %d bytes', [Count]);

  SetLength(Result, Count);
  ReadBuffer(Result[1], Count);
 { Alternative:  Result:= Read(Pointer(s)^, Count)= Count;     <--- Don't use this! Ever. See explanation from A Buchez:   http://stackoverflow.com/questions/6411246/pointers-versus-s1 }
end;




{--------------------------------------------------------------------------------------------------
   STRING LIST
--------------------------------------------------------------------------------------------------}

procedure TCubicMemStream.WriteStrings(TSL: TStrings);
begin
  Assert(TSL <> NIL, 'TCubicMemStream.WriteStrings: TSL is nil');
  WriteString(TSL.Text);
end;


procedure TCubicMemStream.ReadStrings(TSL: TStrings);
begin
  Assert(TSL <> NIL, 'TCubicMemStream.ReadStrings: TSL is nil');
  TSL.Text:= ReadString(StringListSafetyLimit);
end;


{ Creates and returns a new TStringList populated from the stream.
  IMPORTANT: Caller is responsible for freeing the returned TStringList. }
function TCubicMemStream.ReadStrings: TStringList;
begin
  Result:= TStringList.Create;
  try
  Result.Text:= ReadString(StringListSafetyLimit);
  except
    FreeAndNil(Result);
    raise;
  end;
end;




{--------------------------------------------------------------------------------------------------
   NUMBERS
--------------------------------------------------------------------------------------------------}

{ BOOLEAN }
procedure TCubicMemStream.WriteBoolean(b: Boolean);
begin
  WriteBuffer(b, 1);
end;

function TCubicMemStream.ReadBoolean: Boolean;
VAR b: byte;
begin
  ReadBuffer(b, 1);    { Valid values for a Boolean are 0 and 1. If you put a different value into a Boolean variable then future behaviour is undefined. You should read into a byte variable b and assign b <> 0 into the Boolean. Or sanitise by casting the byte to ByteBool. Or you may choose to validate the value read from the file and reject anything other than 0 and 1. http://stackoverflow.com/questions/28383736/cannot-read-boolean-value-with-tmemorystream }
  Result:= b <> 0;
end;




{ BYTE }
procedure TCubicMemStream.WriteByte(b: Byte);
begin
  WriteBuffer(b, 1);
end;

function TCubicMemStream.ReadByte: Byte;
begin
  ReadBuffer(Result, 1);
end;



{ SHORTINT - Signed 8-bit: -128..127 }
procedure TCubicMemStream.WriteShortInt(s: ShortInt);
begin
  WriteBuffer(s, 1);
end;

function TCubicMemStream.ReadShortInt: ShortInt;
begin
  ReadBuffer(Result, 1);
end;


{ SMALLINT - Signed 16-bit: -32768..32767 }
procedure TCubicMemStream.WriteSmallInt(s: SmallInt);
begin
  WriteBuffer(s, 2);
end;

function TCubicMemStream.ReadSmallInt: SmallInt;
begin
  ReadBuffer(Result, 2);
end;


{ WORD }
procedure TCubicMemStream.WriteWord(w: Word);
begin
  WriteBuffer(w, 2);
end;

function TCubicMemStream.ReadWord: Word;
begin
  ReadBuffer(Result, 2);
end;


{ CARDINAL }
procedure TCubicMemStream.WriteCardinal(c: Cardinal);
begin
  WriteBuffer(c, 4);
end;

function TCubicMemStream.ReadCardinal: Cardinal;    { Cardinal IS NOT a fundamental type BUT its size is 32 bits across 64-bit and 32-bit platforms.  }
begin
  ReadBuffer(Result, 4);
end;


{ INTEGER }
procedure TCubicMemStream.WriteInteger(i: Integer);
begin
  WriteBuffer(i, 4);
end;

function TCubicMemStream.ReadInteger: Integer;
begin
  ReadBuffer(Result, 4);
end;


{ INT64 }
procedure TCubicMemStream.WriteInt64(i: Int64);
begin
  WriteBuffer(i, 8);
end;

function TCubicMemStream.ReadInt64: Int64;
begin
 ReadBuffer(Result, 8);
end;


{ UINT64 - The size of UInt64 is 64 bits across all 64-bit and 32-bit platforms. }
procedure TCubicMemStream.WriteUInt64(i: UInt64);
begin
  WriteBuffer(i, 8);
end;

function TCubicMemStream.ReadUInt64: UInt64;
begin
  ReadBuffer(Result, 8);
end;




{--------------------------------------------------------------------------------------------------
   FLOATS
--------------------------------------------------------------------------------------------------}
{ SINGLE - 32-bit floating point }
function TCubicMemStream.ReadSingle: Single;
begin
  ReadBuffer(Result, 4);
end;

procedure TCubicMemStream.WriteSingle(s: Single);
begin
  WriteBuffer(s, 4);
end;


{ DOUBLE - 64-bit floating point }
function TCubicMemStream.ReadDouble: Double;
begin
  ReadBuffer(Result, 8);
end;

procedure TCubicMemStream.WriteDouble(d: Double);
begin
  WriteBuffer(d, 8);
end;




{ DATE }

function TCubicMemStream.ReadDate: TDateTime;
begin
  ReadBuffer(Result, 8);
end;

procedure TCubicMemStream.WriteDate(d: TDateTime);
begin
  WriteBuffer(d, 8);             { The size of Double is 8 bytes }
end;




{--------------------------------------------------------------------------------------------------
   COMPLEX STRUCTURES
--------------------------------------------------------------------------------------------------}
function TCubicMemStream.ReadRect: TRect;
begin
  ReadBuffer(Result.Left  , 4);
  ReadBuffer(Result.Top   , 4);
  ReadBuffer(Result.Right , 4);
  ReadBuffer(Result.Bottom, 4);
end;

procedure TCubicMemStream.WriteRect(Rect: TRect);
begin
  WriteBuffer(Rect.Left  , 4);
  WriteBuffer(Rect.Top   , 4);
  WriteBuffer(Rect.Right , 4);
  WriteBuffer(Rect.Bottom, 4);
end;



function TCubicMemStream.ReadRectF: TRectF;
begin
  ReadBuffer(Result.Left  , 4);
  ReadBuffer(Result.Top   , 4);
  ReadBuffer(Result.Right , 4);
  ReadBuffer(Result.Bottom, 4);
end;

procedure TCubicMemStream.WriteRectF(Rect: TRectF);
begin
  WriteBuffer(Rect.Left  , 4);
  WriteBuffer(Rect.Top   , 4);
  WriteBuffer(Rect.Right , 4);
  WriteBuffer(Rect.Bottom, 4);
end;



procedure TCubicMemStream.ReadIntegers(out List: TIntegerArray);
VAR
  i: Integer;
  Count: Integer;
begin
  Count:= ReadInteger;
  SetLength(List, Count);
  for i:= 0 to High(List) DO
    List[i]:= ReadInteger;
end;

procedure TCubicMemStream.WriteIntegers(const List: TIntegerArray);
VAR Int: Integer;
begin
  WriteInteger(Length(List));
  for Int in List DO
    WriteInteger(Int);
end;



procedure TCubicMemStream.ReadDoubles(out List: TDoubleArray);
VAR
  i: Integer;
  Count: Integer;
begin
  Count:= ReadInteger;
  SetLength(List, Count);
  for i:= 0 to High(List) DO
    List[i]:= ReadDouble;
end;

procedure TCubicMemStream.WriteDoubles(const List: TDoubleArray);
VAR Dbl: Double;
begin
  WriteInteger(Length(List));
  for Dbl in List DO
    WriteDouble(Dbl);
end;




{--------------------------------------------------------------------------------------------------
   READ MACINTOSH
--------------------------------------------------------------------------------------------------}
{ REVERSE READ - read 4 bytes and swap their position
  LongWord = Cardinal }
function TCubicMemStream.RevReadCardinal: Cardinal;    // Old name: RevReadLongWord
begin
  ReadBuffer(Result, 4);
  SwapCardinal(Result);        { SwapCardinal will correctly swap the byte order of the 32-bit value regardless whether the number is signed or unsigned }
end;


function TCubicMemStream.RevReadInteger: Integer;
begin
  ReadBuffer(Result, 4);
  SwapInt(Result);
end;


function TCubicMemStream.RevReadSmallInt: SmallInt;
begin
  ReadBuffer(Result, 2);
  Result:= Swap(Result);
end;


function TCubicMemStream.RevReadWord: Word; { REVERSE READ - read 2 bytes and swap their position }
begin
  ReadBuffer(Result, 2);
  Result:= Swap(Result);     { Exchanges high order byte with the low order byte of an integer or word. In Delphi code, Swap exchanges the high-order bytes with the low-order bytes of the argument. X is an expression of type SmallInt, as a 16-bit value, or Word. This is provided for backward compatibility only. }
  //Also see: LightCore.Binary.SwapWord
end;




{--------------------------------------------------------------------------------------------------
   RAW DATA
---------------------------------------------------------------------------------------------------
   Read/write raw data to file.
--------------------------------------------------------------------------------------------------}

{ Writes the string to file but does not write its length.
  In most cases you will want to use WriteString instead of PushString. }             { Used in cmEncodeMime.pas }
procedure TCubicMemStream.PushString(CONST s: string);    //  old name: WriteStringNoLen
VAR UTF: UTF8String;
begin
  UTF := UTF8String(s);
  if Length(UTF) > 0
  then WriteBuffer(UTF[1], Length(UTF));
end;


{ Write raw AnsiString data to file. The length is NOT written! }
procedure TCubicMemStream.PushAnsi(CONST s: AnsiString);   // old name: WriteStringANoLen
begin
  if Length(s) > 0
  then WriteBuffer(s[1], Length(s));
end;


{ Read the raw content of the file and return it as string (for debugging).
  Note: This reads raw bytes, not a length-prefixed string. Use ReadStringA for that. }
function TCubicMemStream.AsString: AnsiString;
begin
  if Size = 0 then RAISE Exception.Create('TCubicMemStream is empty!');
  Position:= 0;
  Result:= ReadCharsA(Size, Size); // this sets the size
end;


function TCubicMemStream.ReadStringACnt(Count: Cardinal; SafetyLimit: Cardinal): AnsiString;  { We need to specify the length of the string }
VAR TotalBytes: Cardinal;
begin
  if Count > SafetyLimit
  then RAISE Exception.CreateFmt('String too large: %d bytes', [Count]);

  if Count > 0
  then
    begin
     SetLength(Result, Count);                                             { Initialize the result }
     TotalBytes:= Read(Result[1], Count);                                  { Read is used in cases where the number of bytes to read from the stream is not necessarily fixed. It attempts to read up to Count bytes into buffer and returns the number of bytes actually read.  }

     if TotalBytes= 0
     then Result:= ''
     else
       if TotalBytes < Count                                               { If there is not enough data to read... }
       then SetLength(Result, TotalBytes);                               { ...set the buffer to whater I was able to read }
    end
  else Result:= '';
end;


{ Returns the content of the ENTIRE stream as a byte array.
  Returns empty array if stream is empty. }
function TCubicMemStream.AsBytes: TBytes;          { Put the content of the stream into a string }
begin
  Position:= 0;            // Reset stream position
  SetLength(Result, Size); // Allocate size
  if Size > 0
  then Read(Result[0], Size);   // Read content of stream
end;


{ TBYTES }
procedure TCubicMemStream.PushBytes(CONST Bytes: TBytes);
begin
  Assert(Length(Bytes) > 0, 'Buffer is zero!!');
  WriteBuffer(Bytes[0], Length(Bytes));
end;


{ Writes "Buffer" in the stream, at the current pos. The amount of data to be stored is also written down to the stream. }
procedure TCubicMemStream.PushBytesCnt(CONST Buffer: TBytes); // old name: WriteBytes
begin
  Assert(Length(Buffer) > 0, 'Buffer is zero!');
  WriteCardinal(Length(Buffer));
  PushBytes(Buffer);
end;


{ Reads a chunk of data from the current pos of the stream. The amount of data to read is also retrieved from the stream. }
function TCubicMemStream.ReadByteChunk: TBytes;
VAR Count: Cardinal;
begin
  Count:= ReadCardinal;
  if Count > 0 then
  begin
    SetLength(Result, Count);
    ReadBuffer(Result[0], Count);
  end
  else
    SetLength(Result, 0);
end;


{ Write raw data to stream, replacing any existing content.
  Clears the stream first, then writes the provided data. }
procedure TCubicMemStream.PushData(CONST s: AnsiString);
begin
  Clear;       { Sets the Memory property to nil (Delphi). Sets the Position property to 0. Sets the Size property to 0. }
  if Length(s) > 0
  then WriteBuffer(s[1], Length(s));
end;


procedure TCubicMemStream.PushData(CONST Bytes: TBytes);
begin
  Clear;           { Sets the Memory property to nil (Delphi). Sets the Position property to 0. Sets the Size property to 0. }
  if Length(Bytes) > 0
  then WriteBuffer(Bytes[0], Length(Bytes));
end;




{--------------------------------------------------------------------------------------------------
   ANSI STRINGS
--------------------------------------------------------------------------------------------------}
procedure TCubicMemStream.WriteStringA(CONST s: AnsiString);
VAR Count: Cardinal;
begin
  Count:= Length(s);
  WriteBuffer(Count, SizeOf(Count));
  if Count > 0                                                            { This makes sure 's' is not empty. Else I will get a RangeCheckError at runtime }
  then WriteBuffer(s[1], Count);
end;


{ It automatically detects the length of the string }
function TCubicMemStream.ReadStringA(SafetyLimit: Cardinal = 1*KB): AnsiString;
VAR Count: Cardinal;
begin
  ReadBuffer(Count, SizeOf(Count));  { First, find out how many characters to read }

 if Count > Cardinal(Size - Position)
 then RAISE Exception.CreateFmt('String length (%d) exceeds remaining file size!', [Count]);

  if Count > SafetyLimit
  then RAISE Exception.CreateFmt('String too large: %d bytes', [Count]);

  if Count > 0 
  then
    begin
      SetLength(Result, Count);
      ReadBuffer(Result[1], Count);
    end
  else
    Result:= '';
end;


{  STRING WITHOUT LENGTH
   Read a string when its size is unknown (not written in the stream).
   We need to specify the string size from outside.
   This is the relaxed/safe version. It won't raise an error if there is not enough data (Len) to read }
function TCubicMemStream.TryReadStringA(Count: Cardinal): AnsiString;
VAR
  ReadBytes: Cardinal;
  AvailableBytes: Int64;
begin
 if Count = 0 then EXIT('');

 // Limit Count to available data
  AvailableBytes:= Size - Position;
 if Count > Cardinal(AvailableBytes)
 then Count:= Cardinal(AvailableBytes);

 if Count = 0 then EXIT('');

  SetLength(Result, Count);
  ReadBytes:= Read(Result[1], Count);
  if ReadBytes <> Count
  then SetLength(Result, ReadBytes);
end;


{ Read a string from file. The length of the string will be provided from outside. }
function TCubicMemStream.ReadStringCnt(Count: Cardinal; SafetyLimit: Cardinal = 1*KB): string;
VAR UTF: UTF8String;
begin
 if Count > 0
 then
   begin
     if (Count+ Position > Size)
     then RAISE exception.Create('TCubicMemStream-Invalid string size! '+ IntToStr(Count));

     if (Count > SafetyLimit)
     then RAISE Exception.CreateFmt('String too large: %d bytes', [Count]);

     SetLength(UTF, Count);
     ReadBuffer(UTF[1], Count);
     Result:= string(UTF);
   end
 else
   Result:= '';
end;




{--------------------------------------------------------------------------------------------------
   CHAR
--------------------------------------------------------------------------------------------------}
function TCubicMemStream.ReadChar: AnsiChar;                                    { Read one single Ansi character }
begin
  ReadBuffer(Result, 1);
end;

procedure TCubicMemStream.WriteChar(CONST C: AnsiChar);
begin
  WriteBuffer(c, 1);
end;




{--------------------------------------------------------------------------------------------------
   I/O
--------------------------------------------------------------------------------------------------}
procedure TCubicMemStream.LoadFromFile(CONST FileName: string);
var
  FileStream: TLightStream;
begin
  FileStream:= TLightStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(FileStream);
  finally
    FreeAndNil(FileStream);
  end;
end;


procedure TCubicMemStream.SaveToFile(CONST FileName: string);
var
  FileStream: TLightStream;
begin
  FileStream:= TLightStream.Create(FileName, fmCreate);
  try
    SaveToStream(FileStream);
  finally
    FreeAndNil(FileStream);
  end;
end;




{--------------------------------------------------------------------------------------------------
   UTILS / CONVERSIONS
--------------------------------------------------------------------------------------------------}

{ Reads a portion of a memory stream and returns it as an ASCII string.
  Count: Number of bytes to read (0 = read from Pos to end of stream)
  Pos: Starting position in the stream (default = 0) }
function StringFromStream(MemStream: TMemoryStream; Count: Integer = 0; Pos: Integer = 0): string;
var
  StringBytes: TBytes;
begin
  Assert(MemStream <> nil, 'StringFromStream - MemStream is nil!');
  Assert(Pos >= 0, 'StringFromStream - Pos cannot be negative!');
  Assert(Count >= 0, 'StringFromStream - Count cannot be negative!');
  Assert(Pos <= MemStream.Size, 'StringFromStream - Pos beyond stream size!');

  if (Count = 0)
  OR (Count > MemStream.Size - Pos)
  then Count := MemStream.Size - Pos;  // Ensure Count is within bounds of the stream

  if Count = 0
  then EXIT('');  { Return empty string for empty content }

  SetLength(StringBytes, Count);
  MemStream.Position := Pos;  // Ensure we're at the correct position in the stream
  MemStream.ReadBuffer(StringBytes[0], Count);
  Result := TEncoding.ASCII.GetString(StringBytes);
end;


end.
