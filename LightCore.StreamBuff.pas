UNIT LightCore.StreamBuff;

{=============================================================================================================
   2025.09
   www.GabrielMoraru.com
   Article:    https://gabrielmoraru.com/saving-an-object-to-disk-file/
--------------------------------------------------------------------------------------------------------------
   Description
      Extends TBufferedFileStream.
      It may be used as a drop-in replacement for TBufferedFileStream/TFileStream.
      https://gabrielmoraru.com/saving-an-object-to-disk-file/

      TBufferedFileStream optimizes multiple consecutive small reads or writes.
      It will not give performance gain for random or large reads/writes.

      This class adds new functionality that does not exist in Delphi's original stream classes:
       - Read/WriteBoolean
       - Read/WriteString (Ansi/Unicode)
       - Read/WriteInteger
       - Read/WriteCardinal
       - Read/WriteDate
       - Read/Write mac files (inverted byte endianness)
       - etc

   Constructor:
       TLightStream.Create(FileName, fmOpenRead)                  -> to read
       TLightStream.Create(FileName, fmOpenWrite OR fmCreate)     -> to write to existing or create new file if none exists
     or:
       constructor CreateRead (FileName);                             -> to read
       constructor CreateWrite(FileName);                             -> to write to existing or create new file if none exists

--------------------------------------------------------------------------------------------------------------
   TBufferedFileStream info:
      TBufferedFileStream provides VERY fast reading/writing access to a file.
      It is optimized for multiple consecutive small reads or writes.
      TBufferedFileStream will not give performance gain, when there are random position reads or writes, or large reads or writes.

   File Mode:
      fmCreate        - Create a file with the given name. If a file with the given name exists, override the existing file and open it in write mode. Destroys any file that's already there. (size will be zero)
      fmOpenRead      - Open the file for reading only.
      fmOpenWrite     - Open the file for writing only. Writing to the file completely REPLACES the current contents.
      fmOpenReadWrite - Open the file to modify the current contents rather than replace them.

      Both fmOpenWrite and fmOpenReadWrite can be used to append a file except that fmOpenWrite won't allow reading the file.
      One should remember to set the file position to the end of file prior writing it. Otherwise it will replace the existing data.

   Speed
      When reading character by character, the new System.Classes.TBufferedFileStream seems to be 210% faster than LightCore.StreamBuffHefferman

   Full tester:
      LightSaber\Demo\Core\Demo LightCore StreamBuffer\Demo_FileStream.dpr
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Classes, System.Types, System.Math, LightCore, LightCore.Time, LightCore.Types;

TYPE
  TLightStream= class(System.Classes.TBufferedFileStream)
   private
     CONST LisaMagicNumber: Cardinal= $6153694C; // The LiSa string for "Light Saber'.  // Old number: $4C695361
     CONST FrozenPaddingSize = 64;  // NEVER-EVER MODIFY THIS CONSTANT! All files saved with this constant will not work anymore. Enough for 16 Integer variables.
   public
     StringSafetyLimit: Cardinal;   // If we try to read a string larger than this size then we are probably doing something wrong. Set it to zero to disable it.

     constructor CreateRead (CONST FileName: string);
     constructor CreateWrite(CONST FileName: string);

     { Header }
     procedure WriteHeader    (CONST Signature: AnsiString; Version: Word);
     procedure ReadHeader     (CONST Signature: AnsiString; Version: Word);
     function  TryReadHeader  (CONST Signature: AnsiString; Version: Word): Boolean;  overload;
     function  TryReadHeader  (CONST Signature: AnsiString): Word;                    overload;

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
     function  ReadString: string;  overload;

     { ANSI }
     procedure WriteStringA (CONST s: AnsiString);
     function  TryReadStringA (Count: Cardinal): AnsiString;                  { This is the relaxed version. It won't raise an error if there is not enough data (Len) to read }
     function  ReadStringA  (Count: Cardinal): AnsiString;     overload;    { It will raise an error if there is not enough data (Len) to read }
     function  ReadStringA: AnsiString;                        overload;    { It automatically detects the length of the string }

    { TSL }
     function  ReadStrings: TStringList;                       overload;
     procedure ReadStrings  (TSL: TStrings);                   overload;
     procedure WriteStrings (TSL: TStrings);

    { Chars }
     procedure WriteChars   (CONST s: AnsiString);             overload;
     procedure WriteChars   (CONST s: string);                 overload;
     function  ReadCharsA   (Count: Cardinal): AnsiString;
     function  ReadChars    (Count: Cardinal): string;

     { Strings without length }
     procedure PushString   (CONST s: string);
     function  ReadString   (CONST Count: Cardinal): string;     overload;     { Read 'Len' characters }

     { Raw }
     function  AsBytes: TBytes;
     function  AsString: AnsiString;

     procedure PushBytesCnt (CONST Buffer: TBytes);
     function  ReadByteChunk: TBytes;

     procedure PushAnsi (CONST s: AnsiString);
     procedure PushBytes(CONST Bytes: TBytes);
  end;



IMPLEMENTATION
USES
   LightCore.Binary, LightCore.AppData;


{--------------------------------------------------------------------------------------------------
   CTOR
--------------------------------------------------------------------------------------------------}
constructor TLightStream.CreateRead(CONST FileName: string);
begin
  StringSafetyLimit:= 50 * MB; // If we try to read a string larger than this size then we are probably doing something wrong.
  inherited Create(FileName, fmOpenRead, 1*MB);
end;


constructor TLightStream.CreateWrite(CONST FileName: string);
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
       the magic number  - this is fixed for all TLightStream files
       the signature
       the file version

--------------------------------------------------------------------------------------------------------------
     'L' = 0x4C
     'i' = 0x69
     'S' = 0x53
     'a' = 0x61
-------------------------------------------------------------------------------------------------------------}

procedure TLightStream.WriteHeader(CONST Signature: AnsiString; Version: Word);
begin
  WriteCardinal(LisaMagicNumber);    // Write fixed magic no  "LiSa"
  WriteStringA(Signature);           // Write signature
  WriteWord(Version);                // Write the file version number
end;


{ Returns the version or 0 in case of errors. }
function TLightStream.TryReadHeader(CONST Signature: AnsiString): Word;
VAR
  MagicNo: Cardinal;
  sStreamSignature: AnsiString;
begin
  Assert(Signature > '', 'No signature!');
  sStreamSignature:= '';

  // Read LiSa magic no
  TRY
    MagicNo := ReadCardinal;
  EXCEPT
    on E: Exception DO
      begin
        AppDataCore.LogError('Cannot read magic number for: '+ String(Signature) + ' - '+ E.Message);
        EXIT(0);
      end;
  END;
  if MagicNo <> LisaMagicNumber then EXIT(0);

  // Read signature
  TRY
    sStreamSignature:= ReadStringA;
  EXCEPT
    on E: Exception DO
    begin
      AppDataCore.LogError('Cannot read stream signature for: '+ String(Signature) + ' - '+ E.Message);
      EXIT(0);
    end;
  END;
  if sStreamSignature <> Signature then EXIT(0);

  // Read the version number
  TRY
    Result:= ReadWord;
  EXCEPT
    on E: Exception DO
    begin
      AppDataCore.LogError('Cannot read stream version for: '+ String(Signature) + ' - '+ E.Message);
      EXIT(0);
    end;
  END;
end;


{ Returns True if signature & version number matches.
  No exception will be raised unless if the file smaller than what we want to read. }
function TLightStream.TryReadHeader(CONST Signature: AnsiString; Version: Word): Boolean;
VAR lVersion: Word;
begin
 Assert(Signature > '', 'Signature is empty!');
 Assert(Version   > 0 , 'Version must be > 0');

 lVersion:= TryReadHeader(Signature);
 Result:= (lVersion > 0) AND (Version = lVersion);
end;


// This will raise an exception if anything goes wrong
procedure TLightStream.ReadHeader(CONST Signature: AnsiString; Version: Word);
VAR
  MagicNo: Cardinal;
  FileVersion: Word;
  FileSignature: AnsiString;
begin
  // Read magic no
  MagicNo := ReadCardinal;
  if MagicNo <> LisaMagicNumber
  then RAISE Exception.Create('Magic number mismatch!');

  // Read signature
  FileSignature:= ReadStringA;
  if FileSignature <> Signature
  then RAISE Exception.Create('Header signature expected: '+ string(Signature)+ '. Found: '+ string(FileSignature));

  // Read the version number
  FileVersion:= ReadWord;
  if FileVersion <> Version
  then RAISE Exception.Create('Header version expected: '+ IntToStr(Version)+ '. Found: '+ IntToStr(FileVersion));
end;



{-------------------------------------------------------------------------------------------------------------
   Write a checkpoint into the stream.
   Used for debugging.
   Write this from time to time to your file so if you screwup, you check from time to time to see if you are still reading the correct data.
-------------------------------------------------------------------------------------------------------------}
CONST
   ctCheckPoint= '<*>Checkpoint<*>';


{ For debugging. Write a scheckpoint entry (just a string) from time to time to your file so if you screwup, you check from time to time to see if you are still reading the correct data. }
procedure TLightStream.ReadCheckPointE(CONST s: AnsiString= '');
begin
  if NOT ReadCheckPoint(s)
  then raise Exception.Create('Checkpoint failure! '+ crlf+ string(s));
end;

function TLightStream.ReadCheckPoint(CONST s: AnsiString= ''): Boolean;
begin
  Result:= ReadStringA = ctCheckPoint+ s;
end;

procedure TLightStream.WriteCheckPoint(CONST s: AnsiString= '');
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
procedure TLightStream.WritePadding0(Bytes: Integer);
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
procedure TLightStream.ReadPadding0(Bytes: Integer);
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
  ReadPadding raises an exception if the padding does not match the SafetyPaddingStr string. Usefule to detect file corruption. }
procedure TLightStream.WritePadding(Bytes: Integer);
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


procedure TLightStream.ReadPadding(Bytes: Integer);
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
function TLightStream.ReadEnter: Boolean;   { Returns TRUE if the byte read is CR LF }
VAR Byte1, Byte2: Byte;
begin
  ReadBuffer(Byte1, 1);
  ReadBuffer(Byte2, 1);
  Result:= (Byte1= Byte(#13)) AND (Byte2= Byte(#10));
end;


procedure TLightStream.WriteEnter;
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
procedure TLightStream.WriteString(CONST s: string);
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


function TLightStream.ReadString: string;   { Works for both Delphi7 and Delphi UNICODE }
VAR
   Count: Cardinal;
   UTF: UTF8String;
begin
  ReadBuffer(Count, 4);                        { Read length }

  if (StringSafetyLimit > 0)
  AND (Count > StringSafetyLimit)
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

{ Writes a bunch of chars from the file.
  Why 'chars' and not 'string'? This function writes C++ strings (the length of the string was not written to disk also) and not real Delphi strings. }
procedure TLightStream.WriteChars(CONST s: AnsiString);
begin
  Assert(s<> '', 'TLightStream.WriteChars - The string is empty');       { This makes sure 's' is not empty. Else I will get a RangeCheckError at runtime }
  WriteBuffer(s[1], Length(s));
end;


procedure TLightStream.WriteChars(CONST s: string);                     { Works for both Delphi7 and Delphi UNICODE }    // old name: WriteStringANoLen
VAR UTF: UTF8String;
begin
  UTF := UTF8String(s);
  if Length(UTF) > 0
  then WriteBuffer(UTF[1], Length(UTF));
end;


function TLightStream.ReadChars(Count: Cardinal): string;        { Works for both Delphi7 and Delphi UNICODE }
VAR UTF: UTF8String;
begin
  if Count < 1 then EXIT('');

  SetLength(UTF, Count);
  ReadBuffer(UTF[1], Count);
  Result:= string(UTF);
end;


{ Reads a bunch of chars from the file. Why 'ReadChars' and not 'ReadString'? This function reads C++ strings (the length of the string was not written to disk also) and not real Delphi strings. So, i have to give the number of chars to read as parameter. IMPORTANT: The function will reserve memory for s.}
function TLightStream.ReadCharsA(Count: Cardinal): AnsiString;
begin
  if Count= 0
  then RAISE Exception.Create('Count is zero!');     { We cannot do s[1] on an empty string so we added 'Count = 0' as protection. }

  if (StringSafetyLimit > 0)
  AND (Count > StringSafetyLimit)
  then RAISE Exception.CreateFmt('String too large: %d bytes', [Count]);

  SetLength(Result, Count);
  ReadBuffer(Result[1], Count);
 { Alternative:  Result:= Read(Pointer(s)^, Count)= Count;     <--- Don't use this! Ever. See explanation from A Buchez:   http://stackoverflow.com/questions/6411246/pointers-versus-s1 }
end;




{--------------------------------------------------------------------------------------------------
   STRING LIST
--------------------------------------------------------------------------------------------------}

procedure TLightStream.WriteStrings(TSL: TStrings);
begin
  WriteString(TSL.Text);
end;


procedure TLightStream.ReadStrings(TSL: TStrings);
begin
  TSL.Text:= ReadString;
end;


function TLightStream.ReadStrings: TStringList;
begin
  Result:= TStringList.Create;
  Result.Text:= ReadString;
end;




{--------------------------------------------------------------------------------------------------
   NUMBERS
--------------------------------------------------------------------------------------------------}

{ BOOLEAN }
procedure TLightStream.WriteBoolean(b: Boolean);
begin
  WriteBuffer(b, 1);
end;

function TLightStream.ReadBoolean: Boolean;
VAR b: byte;
begin
  ReadBuffer(b, 1);    { Valid values for a Boolean are 0 and 1. If you put a different value into a Boolean variable then future behaviour is undefined. You should read into a byte variable b and assign b <> 0 into the Boolean. Or sanitise by casting the byte to ByteBool. Or you may choose to validate the value read from the file and reject anything other than 0 and 1. http://stackoverflow.com/questions/28383736/cannot-read-boolean-value-with-tmemorystream }
  Result:= b <> 0;
end;




{ BYTE }
procedure TLightStream.WriteByte(b: Byte);
begin
  WriteBuffer(b, 1);
end;

function TLightStream.ReadByte: Byte;
begin
  ReadBuffer(Result, 1);
end;



{ SIGNED }
procedure TLightStream.WriteShortInt(s: ShortInt);     //Signed 8bit: -128..127
begin
  Write(s, 1);
end;

function TLightStream.ReadShortInt: ShortInt;
begin
  Read(Result, 1);
end;



{}
procedure TLightStream.WriteSmallInt(s: SmallInt);     //Signed 16bit: -32768..32767
begin
  Write(s, 2);
end;

function TLightStream.ReadSmallInt: SmallInt;
begin
  Read(Result, 2);
end;


{ WORD }
procedure TLightStream.WriteWord(w: Word);
begin
  WriteBuffer(w, 2);
end;

function TLightStream.ReadWord: Word;
begin
  ReadBuffer(Result, 2);
end;


{ CARDINAL }
procedure TLightStream.WriteCardinal(c: Cardinal);
begin
  WriteBuffer(c, 4);
end;

function TLightStream.ReadCardinal: Cardinal;    { Cardinal IS NOT a fundamental type BUT its size is 32 bits across 64-bit and 32-bit platforms.  }
begin
  ReadBuffer(Result, 4);
end;


{ INTEGER }
procedure TLightStream.WriteInteger(i: Integer);
begin
  WriteBuffer(i, 4);
end;

function TLightStream.ReadInteger: Integer;
begin
  ReadBuffer(Result, 4);
end;


{ INT64 }
procedure TLightStream.WriteInt64(i: Int64);
begin
  WriteBuffer(i, 8);
end;

function TLightStream.ReadInt64: Int64;
begin
 ReadBuffer(Result, 8);
end;


{ UINT64 - The size of UInt64 is 64 bits across all 64-bit and 32-bit platforms. }
procedure TLightStream.WriteUInt64(i: UInt64);
begin
  WriteBuffer(i, 8);
end;

function TLightStream.ReadUInt64: UInt64;
begin
  ReadBuffer(Result, 8);
end;




{--------------------------------------------------------------------------------------------------
   FLOATS
--------------------------------------------------------------------------------------------------}
function TLightStream.ReadSingle: Single;
begin
  Read(Result, 4);
end;

procedure TLightStream.WriteSingle(s: Single);
begin
  Write(s, 4);
end;



function TLightStream.ReadDouble: Double;
begin
  Read(Result, 8);
end;

procedure TLightStream.WriteDouble(d: Double);
begin
  Write(d, 8);
end;




{ DATE }

function TLightStream.ReadDate: TDateTime;
begin
  ReadBuffer(Result, 8);
end;

procedure TLightStream.WriteDate(d: TDateTime);
begin
  WriteBuffer(d, 8);             { The size of Double is 8 bytes }
end;




{--------------------------------------------------------------------------------------------------
   COMPLEX STRUCTURES
--------------------------------------------------------------------------------------------------}
function TLightStream.ReadRect: TRect;
begin
  ReadBuffer(Result.Left  , 4);
  ReadBuffer(Result.Top   , 4);
  ReadBuffer(Result.Right , 4);
  ReadBuffer(Result.Bottom, 4);
end;

procedure TLightStream.WriteRect(Rect: TRect);
begin
  WriteBuffer(Rect.Left  , 4);
  WriteBuffer(Rect.Top   , 4);
  WriteBuffer(Rect.Right , 4);
  WriteBuffer(Rect.Bottom, 4);
end;



function TLightStream.ReadRectF: TRectF;
begin
  ReadBuffer(Result.Left  , 4);
  ReadBuffer(Result.Top   , 4);
  ReadBuffer(Result.Right , 4);
  ReadBuffer(Result.Bottom, 4);
end;

procedure TLightStream.WriteRectF(Rect: TRectF);
begin
  WriteBuffer(Rect.Left  , 4);
  WriteBuffer(Rect.Top   , 4);
  WriteBuffer(Rect.Right , 4);
  WriteBuffer(Rect.Bottom, 4);
end;



procedure TLightStream.ReadIntegers(out List: TIntegerArray);
VAR
  i: Integer;
  Count: Integer;
begin
  Count:= ReadInteger;
  SetLength(List, Count);
  for i:= 0 to High(List) DO
    List[i]:= ReadInteger;
end;

procedure TLightStream.WriteIntegers(const List: TIntegerArray);
VAR Int: Integer;
begin
  WriteInteger(Length(List));
  for Int in List DO
    WriteInteger(Int);
end;



procedure TLightStream.ReadDoubles(out List: TDoubleArray);
VAR
  i: Integer;
  Count: Integer;
begin
  Count:= ReadInteger;
  SetLength(List, Count);
  for i:= 0 to High(List) DO
    List[i]:= ReadDouble;
end;

procedure TLightStream.WriteDoubles(const List: TDoubleArray);
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
function TLightStream.RevReadCardinal: Cardinal;    // Old name: RevReadLongWord
begin
  ReadBuffer(Result, 4);
  SwapCardinal(Result);        { SwapCardinal will correctly swap the byte order of the 32-bit value regardless whether the number is signed or unsigned }
end;


function TLightStream.RevReadInteger: Integer;
begin
  ReadBuffer(Result, 4);
  SwapInt(Result);
end;


function TLightStream.RevReadWord: Word; { REVERSE READ - read 2 bytes and swap their position }
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
procedure TLightStream.PushString(CONST s: string);    //  old name: WriteStringNoLen
VAR UTF: UTF8String;
begin
  UTF := UTF8String(s);
  if Length(UTF) > 0
  then WriteBuffer(UTF[1], Length(UTF));
end;


{ Write raw data to file. The length is not written! }
procedure TLightStream.PushAnsi(CONST s: AnsiString);   // old name: WriteStringANoLen
begin
  Assert(s<> '', 'WriteStringA - The string is empty');   { Make sure 's' is not empty, otherwise we get a RangeCheckError at runtime }
  WriteBuffer(s[1], Length(s));
end;


{ Read the raw content of the file and return it as string (for debugging) }
function TLightStream.AsString: AnsiString;
begin
  Position:= 0;
  Result:= ReadStringA(Size);
end;


function TLightStream.ReadStringA(Count: Cardinal): AnsiString;  { We need to specify the length of the string }
VAR TotalBytes: Cardinal;
begin
  if (StringSafetyLimit > 0)
  AND (Count > StringSafetyLimit)
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


{ Returns the content of the ENTIRE stream }
function TLightStream.AsBytes: TBytes;          { Put the content of the stream into a string }
begin
  Position:= 0;            // Reset stream position
  SetLength(Result, Size); // Allocate size
  Read(Result[0], Size);   // Read content of stream
end;


{ TBYTES }
procedure TLightStream.PushBytes(CONST Bytes: TBytes);
begin
  Assert(Length(Bytes) > 0, 'Buffer is zero!!');
  WriteBuffer(Bytes[0], Length(Bytes));
end;


{ Writes "Buffer" in the stream, at the current pos. The amount of data to be stored is also written down to the stream. }
procedure TLightStream.PushBytesCnt(CONST Buffer: TBytes); // old name: WriteBytes
begin
  Assert(Length(Buffer) > 0, 'Buffer is zero!');
  WriteCardinal(Length(Buffer));
  PushBytes(Buffer);
end;


{ Reads a chunk of data from the current pos of the stream. The amount of data to read is also retrieved from the stream. }
function TLightStream.ReadByteChunk: TBytes;
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
procedure TLightStream.WriteStringA(CONST s: AnsiString);
VAR Count: Cardinal;
begin
  Count:= Length(s);
  WriteBuffer(Count, SizeOf(Count));
  if Count > 0                                                            { This makes sure 's' is not empty. Else I will get a RangeCheckError at runtime }
  then WriteBuffer(s[1], Count);
end;


{ It automatically detects the length of the string }
function TLightStream.ReadStringA: AnsiString;
VAR Count: Cardinal;
begin
 ReadBuffer(Count, SizeOf(Count));                                          { First, find out how many characters to read }

 Assert(Count<= Size- Position, 'TReadCachedStream: String lenght > file size!');

 if (StringSafetyLimit > 0)
 AND (Count > StringSafetyLimit)
 then RAISE Exception.CreateFmt('String too large: %d bytes', [Count]);

 Result:= ReadStringA(Count);        { Do the actual strign reading }
end;


{  STRING WITHOUT LENGTH
   Read a string when its size is unknown (not written in the stream).
   We need to specify the string size from outside.
   This is the relaxed/safe version. It won't raise an error if there is not enough data (Len) to read }
function TLightStream.TryReadStringA(Count: Cardinal): AnsiString;   // Old name: ReadStringAR
VAR ReadBytes: Cardinal;
begin
 Assert(Count<= Size- Position, 'TReadCachedStream: String lenght > file size!');

 if Count = 0
 then Result:= ''
 else
  begin
   SetLength(Result, Count);             { Initialize the result }
   ReadBytes:= Read(Result[1], Count);
   if ReadBytes <> Count                 { Not enough data to read? }
   then SetLength(Result, ReadBytes);
  end;
end;


{ Read a string from file. The length of the string will be provided from outside. }
function TLightStream.ReadString(CONST Count: Cardinal): string;
VAR UTF: UTF8String;
begin
 if Count > 0
 then
   begin
     if (Count+ Position > Size)
     then RAISE exception.Create('TLightStream-Invalid string size! '+ IntToStr(Count));

     if (StringSafetyLimit > 0)
     AND (Count > StringSafetyLimit)
     then RAISE Exception.CreateFmt('String too large: %d bytes', [Count]);

     SetLength(UTF, Count);
     ReadBuffer(UTF[1], Count);
     Result:= string(UTF);
   end
 else
   Result:= '';
end;



end.
