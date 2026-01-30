UNIT LightCore.StreamFile;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Extends TFileStream.
   May be used as a drop-in replacement for TFileStream.
   For better performance with multiple consecutive small reads/writes, use TLightStream instead.

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

       procedure TMyClass.Load(Stream: TLightFileStream);
       begin
         if NOT Stream.ReadHeader('MySignature', 1)          // Is the file valid?
         then RAISE Exception.Create('Cannot load data!');
         FSomeField:= Stream.ReadInteger;                    // Save your fields
         ...
       end;

       procedure TLessons.Save(Stream: TLightFileStream);
       begin
         Stream.WriteHeader('MySignature', 1);
         Stream.WriteInteger(FSomeField);                    // Write your fields
         ...
       end;

       See the attached demo project for a full example.


   [CONSTRUCTORS]

      Use the classic constructor:
         Create(FileName, fmOpenRead)                -> to read
         Create(FileName, fmOpenWrite OR fmCreate)   -> to write to existing or create new file if none exists
      Or use the new constructor:
        constructor CreateRead (FileName);           -> to read
        constructor CreateWrite(FileName);           -> to write to existing or create new file if none exists

      File Mode:
         fmOpenRead      - Open the file for reading only.
         fmOpenWrite     - Open the file for writing only. Writing to the file completely REPLACES the current contents.
         fmOpenReadWrite - Open the file to modify the current contents rather than replace them.
         fmCreate        - Create a file with the given name. If a file with the given name exists, override the existing file and open it in write mode. Destroys any file that's already there. (size will be zero)

         Both fmOpenWrite and fmOpenReadWrite can be used to append a file except that fmOpenWrite won't allow reading the file.
         One should remember to set the file position to the end of file prior writing it. Otherwise it will replace the existing data.

--------------------------------------------------------------------------------------------------------------

   SPEED
      TLightFileStream is slower than TLightStream (buffered).
      Use TLightStream from LightCore.StreamBuff.pas for better performance.

--------------------------------------------------------------------------------------------------------------

   DEMO/TESTER
      LightSaber\Demo\Core\Demo LightCore StreamBuffer\Demo_FileStream.dpr

=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Classes, System.Types, System.Math, LightCore, LightCore.Types;

TYPE
  TLightFileStream= class(TFileStream)
   private
     CONST LisaMagicNumber: Cardinal= $6153694C;
     CONST FrozenPaddingSize = 64;                // NEVER-EVER MODIFY THIS CONSTANT! All files saved with this constant will not work anymore. Enough for 16 Integer variables.
     function ReadSignature: AnsiString;          // The LiSa string for "Light Saber'.  // Old number: $4C695361
   public
     constructor CreateRead (CONST FileName: string);
     constructor CreateWrite(CONST FileName: string);

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
     procedure WritePadding0(Bytes: Integer= FrozenPaddingSize);
     procedure ReadPadding0 (Bytes: Integer= FrozenPaddingSize);

     procedure WritePadding (Bytes: Integer= FrozenPaddingSize);
     procedure ReadPadding  (Bytes: Integer= FrozenPaddingSize);          // Raises an exception if the buffer does not contain the signature

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
  end;


  { Backwards compatibility alias - DEPRECATED, use TLightFileStream instead }
  TCubicFileStream = TLightFileStream;



IMPLEMENTATION
USES
   LightCore.Binary, LightCore.AppData;


{--------------------------------------------------------------------------------------------------
   CTOR
--------------------------------------------------------------------------------------------------}
constructor TLightFileStream.CreateRead(CONST FileName: string);
begin
  inherited Create(FileName, fmOpenRead);
end;


constructor TLightFileStream.CreateWrite(CONST FileName: string);
begin
  inherited Create(FileName, fmOpenWrite OR fmCreate);
end;



{-------------------------------------------------------------------------------------------------------------
   HEADER FORMAT:

     4 bytes (Card): LiSa (always the same)
     4 bytes (Card): Length of the magic signature
       bytes (Ansi): Magic signature
     2 bytes (Word): File version number.

     This new file header is more reliable because we check
       the magic number  - this is fixed for all TLightFileStream files
       the signature
       the file version

--------------------------------------------------------------------------------------------------------------
     'L' = 0x4C
     'i' = 0x69
     'S' = 0x53
     'a' = 0x61
-------------------------------------------------------------------------------------------------------------}

procedure TLightFileStream.WriteHeader(CONST Signature: AnsiString; Version: Word);
begin
  Assert(Length(Signature) <= 64, 'The Signature cannot be larger the 64 chars!');
  WriteCardinal  (LisaMagicNumber);  // Write fixed magic no  "LiSa"
  WriteStringA   (Signature);        // Write signature
  WriteWord      (Version);          // Write the file version number
end;


{ Returns the version or 0 in case of error.
  No exception will be raised unless when the file is smaller than what we want to read. }
function TLightFileStream.ReadHeader(CONST Signature: AnsiString): Word;
VAR
  MagicNo: Cardinal;
  FileSignature: AnsiString;
begin
  Assert(Signature > '', 'TLightFileStream - No signature provided!');

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
function TLightFileStream.ReadHeader(CONST Signature: AnsiString; Version: Word): Boolean;
VAR FileVersion: Word;
begin
  Assert(Version> 0 , 'TLightFileStream - Version must be > 0!');

  FileVersion:= ReadHeader(Signature);
  Result:= (FileVersion > 0) AND (Version = FileVersion);
end;


{ A dedicated function to read the signature.
  It raises an error if we try to read too many chars, which can happen when we read random data (file corrupted or version changed) }
function TLightFileStream.ReadSignature: AnsiString;
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


{ For debugging. Write a checkpoint entry (just a string) from time to time to your file so if you screwup, you check from time to time to see if you are still reading the correct data. }
procedure TLightFileStream.ReadCheckPointE(CONST s: AnsiString= '');
begin
  if NOT ReadCheckPoint(s)
  then raise Exception.Create('Checkpoint failure! '+ crlf+ string(s));
end;

function TLightFileStream.ReadCheckPoint(CONST s: AnsiString= ''): Boolean;
begin
  Result:= ReadStringA = ctCheckPoint+ s;
end;

procedure TLightFileStream.WriteCheckPoint(CONST s: AnsiString= '');
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
procedure TLightFileStream.WritePadding0(Bytes: Integer);
VAR b: TBytes;
begin
  if Bytes> 0 then
    begin
      SetLength(b, Bytes);
      FillChar (b[0], Bytes, #0);
      WriteBuffer(b[0], Bytes);
    end;
end;

// Reads the padding bytes back and does not check them for validity
procedure TLightFileStream.ReadPadding0(Bytes: Integer);
VAR b: TBytes;
begin
  if Bytes> 0 then
    begin
      SetLength(b, Bytes);
      ReadBuffer(b[0], Bytes);
    end;
end;



CONST
  SafetyPaddingStr: AnsiString= '<##LightSaber - Pattern of exactly 64 bytes for safety check.##>';   //This string is exactly 64 chars long

{ Read/write a string as padding bytes.
  ReadPadding raises an exception if the padding does not match the SafetyPaddingStr string. Useful to detect file corruption. }
procedure TLightFileStream.WritePadding(Bytes: Integer);
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


procedure TLightFileStream.ReadPadding(Bytes: Integer);
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
function TLightFileStream.ReadEnter: Boolean;   { Returns TRUE if the byte read is CR LF }
VAR Byte1, Byte2: Byte;
begin
  ReadBuffer(Byte1, 1);
  ReadBuffer(Byte2, 1);
  Result:= (Byte1= Byte(#13)) AND (Byte2= Byte(#10));
end;


procedure TLightFileStream.WriteEnter;
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
procedure TLightFileStream.WriteString(CONST s: string);
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


function TLightFileStream.ReadString(SafetyLimit: Cardinal = 1*KB): string;   { Works for both Delphi7 and Delphi UNICODE }
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
procedure TLightFileStream.WriteChars(CONST s: AnsiString);
begin
  if Length(s) > 0
  then WriteBuffer(s[1], Length(s));
end;


procedure TLightFileStream.WriteChars(CONST s: string);                     { Works for both Delphi7 and Delphi UNICODE }    // old name: WriteStringANoLen
VAR UTF: UTF8String;
begin
  UTF := UTF8String(s);
  if Length(UTF) > 0
  then WriteBuffer(UTF[1], Length(UTF));
end;


function TLightFileStream.ReadChars(Count: Cardinal): string;        { Works for both Delphi7 and Delphi UNICODE }
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
  Returns empty string if Count is 0. }
function TLightFileStream.ReadCharsA(Count: Cardinal; SafetyLimit: Cardinal = 1*KB): AnsiString;
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

procedure TLightFileStream.WriteStrings(TSL: TStrings);
begin
  Assert(TSL <> NIL, 'TLightFileStream.WriteStrings: TSL is nil');
  WriteString(TSL.Text);
end;


procedure TLightFileStream.ReadStrings(TSL: TStrings);
begin
  Assert(TSL <> NIL, 'TLightFileStream.ReadStrings: TSL is nil');
  TSL.Text:= ReadString;
end;


{ Creates and returns a new TStringList populated from the stream.
  IMPORTANT: Caller is responsible for freeing the returned TStringList. }
function TLightFileStream.ReadStrings: TStringList;
begin
  Result:= TStringList.Create;
  try
    Result.Text:= ReadString;
  except
    FreeAndNil(Result);
    raise;
  end;
end;




{--------------------------------------------------------------------------------------------------
   NUMBERS
--------------------------------------------------------------------------------------------------}

{ BOOLEAN }
procedure TLightFileStream.WriteBoolean(b: Boolean);
begin
  WriteBuffer(b, 1);
end;

function TLightFileStream.ReadBoolean: Boolean;
VAR b: byte;
begin
  ReadBuffer(b, 1);    { Valid values for a Boolean are 0 and 1. If you put a different value into a Boolean variable then future behaviour is undefined. You should read into a byte variable b and assign b <> 0 into the Boolean. Or sanitise by casting the byte to ByteBool. Or you may choose to validate the value read from the file and reject anything other than 0 and 1. http://stackoverflow.com/questions/28383736/cannot-read-boolean-value-with-tmemorystream }
  Result:= b <> 0;
end;




{ BYTE }
procedure TLightFileStream.WriteByte(b: Byte);
begin
  WriteBuffer(b, 1);
end;

function TLightFileStream.ReadByte: Byte;
begin
  ReadBuffer(Result, 1);
end;



{ SHORTINT - Signed 8-bit: -128..127 }
procedure TLightFileStream.WriteShortInt(s: ShortInt);
begin
  WriteBuffer(s, 1);
end;

function TLightFileStream.ReadShortInt: ShortInt;
begin
  ReadBuffer(Result, 1);
end;


{ SMALLINT - Signed 16-bit: -32768..32767 }
procedure TLightFileStream.WriteSmallInt(s: SmallInt);
begin
  WriteBuffer(s, 2);
end;

function TLightFileStream.ReadSmallInt: SmallInt;
begin
  ReadBuffer(Result, 2);
end;


{ WORD }
procedure TLightFileStream.WriteWord(w: Word);
begin
  WriteBuffer(w, 2);
end;

function TLightFileStream.ReadWord: Word;
begin
  ReadBuffer(Result, 2);
end;


{ CARDINAL }
procedure TLightFileStream.WriteCardinal(c: Cardinal);
begin
  WriteBuffer(c, 4);
end;

function TLightFileStream.ReadCardinal: Cardinal;    { Cardinal IS NOT a fundamental type BUT its size is 32 bits across 64-bit and 32-bit platforms.  }
begin
  ReadBuffer(Result, 4);
end;


{ INTEGER }
procedure TLightFileStream.WriteInteger(i: Integer);
begin
  WriteBuffer(i, 4);
end;

function TLightFileStream.ReadInteger: Integer;
begin
  ReadBuffer(Result, 4);
end;


{ INT64 }
procedure TLightFileStream.WriteInt64(i: Int64);
begin
  WriteBuffer(i, 8);
end;

function TLightFileStream.ReadInt64: Int64;
begin
 ReadBuffer(Result, 8);
end;


{ UINT64 - The size of UInt64 is 64 bits across all 64-bit and 32-bit platforms. }
procedure TLightFileStream.WriteUInt64(i: UInt64);
begin
  WriteBuffer(i, 8);
end;

function TLightFileStream.ReadUInt64: UInt64;
begin
  ReadBuffer(Result, 8);
end;




{--------------------------------------------------------------------------------------------------
   FLOATS
--------------------------------------------------------------------------------------------------}
{ SINGLE - 32-bit floating point }
function TLightFileStream.ReadSingle: Single;
begin
  ReadBuffer(Result, 4);
end;

procedure TLightFileStream.WriteSingle(s: Single);
begin
  WriteBuffer(s, 4);
end;


{ DOUBLE - 64-bit floating point }
function TLightFileStream.ReadDouble: Double;
begin
  ReadBuffer(Result, 8);
end;

procedure TLightFileStream.WriteDouble(d: Double);
begin
  WriteBuffer(d, 8);
end;




{ DATE }

function TLightFileStream.ReadDate: TDateTime;
begin
  ReadBuffer(Result, 8);
end;

procedure TLightFileStream.WriteDate(d: TDateTime);
begin
  WriteBuffer(d, 8);             { The size of Double is 8 bytes }
end;




{--------------------------------------------------------------------------------------------------
   COMPLEX STRUCTURES
--------------------------------------------------------------------------------------------------}
function TLightFileStream.ReadRect: TRect;
begin
  ReadBuffer(Result.Left  , 4);
  ReadBuffer(Result.Top   , 4);
  ReadBuffer(Result.Right , 4);
  ReadBuffer(Result.Bottom, 4);
end;

procedure TLightFileStream.WriteRect(Rect: TRect);
begin
  WriteBuffer(Rect.Left  , 4);
  WriteBuffer(Rect.Top   , 4);
  WriteBuffer(Rect.Right , 4);
  WriteBuffer(Rect.Bottom, 4);
end;



function TLightFileStream.ReadRectF: TRectF;
begin
  ReadBuffer(Result.Left  , 4);
  ReadBuffer(Result.Top   , 4);
  ReadBuffer(Result.Right , 4);
  ReadBuffer(Result.Bottom, 4);
end;

procedure TLightFileStream.WriteRectF(Rect: TRectF);
begin
  WriteBuffer(Rect.Left  , 4);
  WriteBuffer(Rect.Top   , 4);
  WriteBuffer(Rect.Right , 4);
  WriteBuffer(Rect.Bottom, 4);
end;



procedure TLightFileStream.ReadIntegers(out List: TIntegerArray);
VAR
  i: Integer;
  Count: Integer;
begin
  Count:= ReadInteger;
  SetLength(List, Count);
  for i:= 0 to High(List) DO
    List[i]:= ReadInteger;
end;

procedure TLightFileStream.WriteIntegers(const List: TIntegerArray);
VAR Int: Integer;
begin
  WriteInteger(Length(List));
  for Int in List DO
    WriteInteger(Int);
end;



procedure TLightFileStream.ReadDoubles(out List: TDoubleArray);
VAR
  i: Integer;
  Count: Integer;
begin
  Count:= ReadInteger;
  SetLength(List, Count);
  for i:= 0 to High(List) DO
    List[i]:= ReadDouble;
end;

procedure TLightFileStream.WriteDoubles(const List: TDoubleArray);
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
function TLightFileStream.RevReadCardinal: Cardinal;    // Old name: RevReadLongWord
begin
  ReadBuffer(Result, 4);
  SwapCardinal(Result);        { SwapCardinal will correctly swap the byte order of the 32-bit value regardless whether the number is signed or unsigned }
end;


function TLightFileStream.RevReadInteger: Integer;
begin
  ReadBuffer(Result, 4);
  SwapInt(Result);
end;


function TLightFileStream.RevReadWord: Word; { REVERSE READ - read 2 bytes and swap their position }
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
procedure TLightFileStream.PushString(CONST s: string);    //  old name: WriteStringNoLen
VAR UTF: UTF8String;
begin
  UTF := UTF8String(s);
  if Length(UTF) > 0
  then WriteBuffer(UTF[1], Length(UTF));
end;


{ Write raw AnsiString data to file. The length is NOT written!
  Does nothing if string is empty. }
procedure TLightFileStream.PushAnsi(CONST s: AnsiString);
begin
  if Length(s) > 0
  then WriteBuffer(s[1], Length(s));
end;


{ Read the raw content of the file and return it as string (for debugging) }
function TLightFileStream.AsString: AnsiString;
begin
  if Size = 0 then RAISE Exception.Create('TLightFileStream is empty!');
  Position:= 0;
  Result:= ReadStringA(Size);
end;


function TLightFileStream.ReadStringACnt(Count: Cardinal; SafetyLimit: Cardinal): AnsiString;  { We need to specify the length of the string }
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
function TLightFileStream.AsBytes: TBytes;
begin
  Position:= 0;
  SetLength(Result, Size);
  if Size > 0
  then ReadBuffer(Result[0], Size);
end;


{ TBYTES }
procedure TLightFileStream.PushBytes(CONST Bytes: TBytes);
begin
  Assert(Length(Bytes) > 0, 'Buffer is zero!!');
  WriteBuffer(Bytes[0], Length(Bytes));
end;


{ Writes "Buffer" in the stream, at the current pos. The amount of data to be stored is also written down to the stream. }
procedure TLightFileStream.PushBytesCnt(CONST Buffer: TBytes); // old name: WriteBytes
begin
  Assert(Length(Buffer) > 0, 'Buffer is zero!');
  WriteCardinal(Length(Buffer));
  PushBytes(Buffer);
end;


{ Reads a chunk of data from the current pos of the stream. The amount of data to read is also retrieved from the stream. }
function TLightFileStream.ReadByteChunk: TBytes;
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




{--------------------------------------------------------------------------------------------------
   ANSI STRINGS
--------------------------------------------------------------------------------------------------}
procedure TLightFileStream.WriteStringA(CONST s: AnsiString);
VAR Count: Cardinal;
begin
  Count:= Length(s);
  WriteBuffer(Count, SizeOf(Count));
  if Count > 0                                                            { This makes sure 's' is not empty. Else I will get a RangeCheckError at runtime }
  then WriteBuffer(s[1], Count);
end;


{ Reads an AnsiString with length prefix (written by WriteStringA).
  SafetyLimit prevents reading excessively large strings. }
function TLightFileStream.ReadStringA(SafetyLimit: Cardinal = 1*KB): AnsiString;
VAR Count: Cardinal;
begin
 ReadBuffer(Count, SizeOf(Count));                     // First, read the string length

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


{ Reads a string when its size is known externally (no length prefix in stream).
  This is the relaxed/safe version - won't raise error if insufficient data.
  Returns whatever could be read (may be shorter than Count). }
function TLightFileStream.TryReadStringA(Count: Cardinal): AnsiString;
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
function TLightFileStream.ReadStringCnt(Count: Cardinal; SafetyLimit: Cardinal = 1*KB): string;
VAR UTF: UTF8String;
begin
 if Count > 0
 then
   begin
     if (Count+ Position > Size)
     then RAISE exception.Create('TLightFileStream-Invalid string size! '+ IntToStr(Count));

     if (Count > SafetyLimit)
     then RAISE Exception.CreateFmt('String too large: %d bytes', [Count]);

     SetLength(UTF, Count);
     ReadBuffer(UTF[1], Count);
     Result:= string(UTF);
   end
 else
   Result:= '';
end;



end.
