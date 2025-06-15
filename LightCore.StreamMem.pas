UNIT LightCore.StreamMem;

{=============================================================================================================
   2024.10
   www.GabrielMoraru.com
   https://gabrielmoraru.com/saving-an-object-to-disk-file/
--------------------------------------------------------------------------------------------------------------
   Description
      Class extender for TMemoryStream
      
      This class adds new functionality that does not exist in Delphi's original stream classes:
      - Read/WriteBoolean
      - Read/WriteString (Ansi/Unicode)
      - Read/WriteInteger
      - Read/WriteCardinal
      - Read/WriteDate
      - Read/Write mac files (inverted byte endianness)
      - etc

      Allows you to read and write data directly into a memory stream.
      Allows reading directly non-Intel (Macintosh) data and raw data

   Warning
      TMemoryStream loads the entire contents of a file into the memory so don't use it with large files.

   Dependencies
      This file must remain free of any LightSaber dependencies.

  --------------------------------------------------------------------------------------------------

   Documentation:
      Wrtiting string to TMemoryStream - Pointer to string - http://stackoverflow.com/questions/3808104/wrtiting-string-to-tmemorystream-pointer-to-string

      Read vs ReadBuffer:
        Read treats Count as an upper bound.
        The ReadBuffer, by contrast, raises an exception if Count bytes cannot be read. So this is better if I don't accept errors in my files.

   Also see TBinaryReader / TBinaryWriter
     http://docwiki.embarcadero.com/CodeExamples/Tokyo/en/TBinaryReader_and_TBinaryWriter_(Delphi)

=============================================================================================================}

{$WARN DUPLICATE_CTOR_DTOR OFF}                                                                               {Silence the: W1029 Duplicate constructor  with identical parameters will be inacessible from C++ }

INTERFACE

USES
   System.SysUtils, System.Classes;

TYPE
  TCubicMemStream= class(TMemoryStream)
   private
   public
     constructor CreateFromStream   (Stream: TMemoryStream);
     constructor CreateFromStreamPos(Stream: TMemoryStream);

     { Header }
     function  ReadMagicNo  (CONST MagicNo: AnsiString): Boolean;
     procedure WriteMagicNo (CONST MagicNo: AnsiString);
	 
     function  ReadCheckPoint: Boolean;
     procedure WriteCheckPoint;
     {}
     function  ReadLF: Boolean;
     procedure WriteLF;
     { Padding }
     procedure ReadPadding     (CONST Bytes: Integer= 1024);
     procedure WritePadding    (CONST Bytes: Integer= 1024);

     { Numeric }
     function  ReadBoolean : Boolean;
     function  ReadByte    : Byte;
     function  ReadCardinal: Cardinal;
     function  ReadDate    : TDateTime;      { This is a DOUBLE }
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

     { Unicode }
     procedure WriteString   (CONST s: string);                            { Works for both Delphi7 and Delphi UNICODE }
     function  ReadString: string;                                         { Works for both Delphi7 and Delphi UNICODE }

	 { ANSI }
     procedure WriteStringA(CONST s: AnsiString);
     function  ReadStringA (CONST Len : integer): AnsiString; overload;    { We need to specify the length of the string }
     function  ReadStringA : AnsiString;                      overload;    { It automatically detects the length of the string }

	 { TSL }
     function  ReadStrings: TStringList;                      overload;
     procedure ReadStrings  (TSL: TStrings);                  overload;
     procedure WriteStrings (TSL: TStrings);

     { Chars }
     procedure WriteChars(CONST s: AnsiString);               overload;    { Write the string but don't write its length }
     procedure WriteChars(CONST s: string);                   overload;
     function  ReadCharsA(Count: Integer): AnsiString;
     function  ReadChars (Count: Integer): string;                         { Works for both Delphi7 and Delphi UNICODE }


     { Char }
     procedure WriteChar(CONST c: AnsiChar);
     function  ReadChar: AnsiChar;

     { Reverse read }
     function  RevReadCardinal: Cardinal;                                  { REVERSE READ - read 4 bytes and swap their position }
     function  RevReadInteger : Integer;
     function  RevReadSmallInt: Smallint;
     function  RevReadWord    : Word;                                      { REVERSE READ - read 2 bytes and swap their position }

     { Raw }
     function  AsBytes: TBytes;
     function  AsStringU: String;                                          { Returns the content of the stream as a string }
     function  AsString: AnsiString;

     procedure WriteByteChunk(CONST Buffer: TBytes);
     function  ReadByteChunk: TBytes;

     procedure PushData(CONST s: AnsiString); overload;                    { Put binary data (or text) into the stream }
     procedure PushData(CONST Bytes: TBytes);  overload;

     { Disk }
     procedure LoadFromFile(CONST FileName: string);
     procedure SaveToFile  (CONST FileName: string);
  end;


function StringFromStream(MemStream: TMemoryStream; Count: Integer= 0; Pos: Integer= 0): string;


IMPLEMENTATION
USES
   LightCore.Binary, LightCore.StreamBuff;


constructor TCubicMemStream.CreateFromStream(Stream: TMemoryStream);
begin
  inherited Create;
  Assert(Stream <> NIL);
  CopyFrom(Stream, Stream.Size);
  Position := 0;
end;


{ Creates the object and copy the data from the specified stream, from its current pos }
constructor TCubicMemStream.CreateFromStreamPos(Stream: TMemoryStream);
var
  CurrentPos: Int64;
  BytesToCopy: Int64;
begin
  inherited Create;

  Assert(Stream <> NIL);

  CurrentPos := Stream.Position;

  // Calculate how much data remains in the stream from the current position
  BytesToCopy := Stream.Size - CurrentPos;
  Assert(BytesToCopy > 0, 'TCubicMemStream - Nothing to copy!');

  CopyFrom(Stream, BytesToCopy);

  // Restore the stream's position
  Stream.Position := CurrentPos;

  Position := 0;
end;





{--------------------------------------------------------------------------------------------------
   MAGIC NO
   Obsolete. Still used in Bionix. 
   Use ReadMagicVer instead.
---------------------------------------------------------------------------------------------------
   This can be used as signature for your stream/file.
   It is already recommended that the first bytes in a stream/file are a signature
--------------------------------------------------------------------------------------------------}
function TCubicMemStream.ReadMagicNo(CONST MagicNo: AnsiString): Boolean;       { Read a string from disk and compare it with MagicNo. Retursn TRUE if it matches }
begin
 Result:= ReadCharsA(Length(MagicNo)) = MagicNo;
end;

procedure TCubicMemStream.WriteMagicNo(CONST MagicNo: AnsiString);
begin
 Assert(MagicNo > '', 'Magic number is empty!');
 WriteChars(MagicNo);
end;








{--------------------------------------------------------------------------------------------------
   Write a checkpoint into the stream.
   Used for debugging.
--------------------------------------------------------------------------------------------------}
CONST
   ctCheckPoint= '<*>Checkpoint<*>';    { For debugging. Write this from time to time to your file so if you screwup, you check from time to time to see if you are still reading the correct data. }


{ For debugging. Write a scheckpoint entry (just a string) from time to time to your file so if you screwup, you check from time to time to see if you are still reading the correct data. }
function TCubicMemStream.ReadCheckPoint: Boolean;
begin
 Result:= ReadStringA = ctCheckPoint;
end;

procedure TCubicMemStream.WriteCheckPoint;
begin
 WriteStringA(ctCheckPoint);
end;




{--------------------------------------------------------------------------------------------------
   PADDING
   It is important to read/write some padding bytes.
   If you later (as your program evolves) need to save extra data into your file, you use the padding bytes. This way you don't need to change your file format.
--------------------------------------------------------------------------------------------------}

// Writes zeroes as padding bytes.
procedure TCubicMemStream.WritePadding(CONST Bytes: Integer);
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
procedure TCubicMemStream.ReadPadding(CONST Bytes: Integer);
VAR b: TBytes;
begin
 if Bytes> 0 then
  begin
   SetLength(b, Bytes);
   ReadBuffer(b[0], Bytes);
  end;
end;









{--------------------------------------------------------------------------------------------------
   UNICODE STRINGS
--------------------------------------------------------------------------------------------------}
procedure TCubicMemStream.WriteString(CONST s: string);                        { Works for both Delphi7 and Delphi UNICODE }
VAR
  Len: cardinal;
  UTF: UTF8String;
begin
 UTF := UTF8String(s);

 { Write length }
 Len := Length(UTF);
 WriteBuffer(Len, SizeOf(Len));

 { Write string }
 if Len > 0
 then WriteBuffer(UTF[1], Len);
end;

function TCubicMemStream.ReadString: string;     { Works for both Delphi7 and Delphi UNICODE }
VAR
   Len: Cardinal;
   UTF: UTF8String;
begin
 ReadBuffer(Len, 4);                                                        { Read length }

 if Len > 0
 then
   begin
     SetLength(UTF, Len);                                                     { Read string }
     ReadBuffer(UTF[1], Len);
     Result:= string(UTF);
   end
 else
   Result:= '';
end;




{--------------------------------------------------------------------------------------------------
   CHARS
--------------------------------------------------------------------------------------------------}

{ Writes a bunch of chars from the file. Why 'chars' and not 'string'? This function writes C++ strings (the length of the string was not written to disk also) and not real Delphi strings. }
procedure TCubicMemStream.WriteChars(CONST s: AnsiString);                  { Write the string but don't write its length }
begin
 Assert(s<> '', 'TCubicMemStream.WriteChars - The string is empty');   { This makes sure 's' is not empty. Else I will get a RangeCheckError at runtime }
 WriteBuffer(s[1], Length(s));
end;


procedure TCubicMemStream.WriteChars(CONST s: string);                      { Works for both Delphi7 and Delphi UNICODE }    // old name: WriteStringANoLen
VAR UTF: UTF8String;
begin
 UTF := UTF8String(s);
 if Length(UTF) > 0
 then WriteBuffer(UTF[1], Length(UTF));
end;


function TCubicMemStream.ReadChars(Count: Integer): string;        { Works for both Delphi7 and Delphi UNICODE }
VAR UTF: UTF8String;
begin
 if Count < 1 then EXIT('');

 SetLength(UTF, Count);
 ReadBuffer(UTF[1], Count);
 Result:= string(UTF);
end;


{ Reads a bunch of chars from the file. Why 'ReadChars' and not 'ReadString'? This function reads C++ strings (the length of the string was not written to disk also) and not real Delphi strings. So, i have to give the number of chars to read as parameter. IMPORTANT: The function will reserve memory for s.}
function TCubicMemStream.ReadCharsA(Count: Integer): AnsiString;
begin
 if Count= 0 then RAISE Exception.Create('Count is zero!');                     { It gives a range check error if we try s[1] on an empty string so we added 'Count = 0' as protection. }

 SetLength(Result, Count);
 ReadBuffer(Result[1], Count);
{ Alternative:  Result:= Read(Pointer(s)^, Count)= Count;     <--- Don't use this! Ever. See explanation from A Buchez:   http://stackoverflow.com/questions/6411246/pointers-versus-s1 }
end;







{--------------------------------------------------------------------------------------------------
   STRING LIST
--------------------------------------------------------------------------------------------------}

procedure TCubicMemStream.WriteStrings(TSL: TStrings);
begin
  WriteString(TSL.Text);
end;


procedure TCubicMemStream.ReadStrings(TSL: TStrings);
begin
  TSL.Text:= ReadString;
end;


function TCubicMemStream.ReadStrings: TStringList;
begin
 Result:= TStringList.Create;
 Result.Text:= ReadString;
end;




{--------------------------------------------------------------------------------------------------
   ENTER
--------------------------------------------------------------------------------------------------}
function TCubicMemStream.ReadLF: Boolean;                                    { Returns TRUE if the byte read is LF }
VAR aByte: Byte;
begin
 ReadBuffer(aByte, 1);
 Result:= aByte= Byte(#10);
end;

procedure TCubicMemStream.WriteLF;
VAR aByte: Byte;
begin
 aByte:= Byte(#10);
 WriteBuffer(abyte, 1);
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



{ SIGNED }
procedure TCubicMemStream.WriteShortInt(s: ShortInt);     //Signed 8bit: -128..127
begin
 Write(s, 1);
end;

function TCubicMemStream.ReadShortInt: ShortInt;
begin
 Read(Result, 1);
end;



{}
procedure TCubicMemStream.WriteSmallInt(s: SmallInt);     //Signed 16bit: -32768..32767
begin
 Write(s, 2);
end;

function TCubicMemStream.ReadSmallInt: SmallInt;
begin
 Read(Result, 2);
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


{ UINT64 }
{ UInt64 Defines a 64-bit unsigned integer type. UInt64 represents a subset of the natural numbers. The range for the UInt64 type is from 0 through 2^64-1. The size of UInt64 is 64 bits across all 64-bit and 32-bit platforms. }
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


{ FLOATS }

function TCubicMemStream.ReadSingle: Single;
begin
  Read(Result, 4);     
end;

procedure TCubicMemStream.WriteSingle(s: Single);
begin
  Write(s, 4);                                                             
end;



function TCubicMemStream.ReadDouble: Double;
begin
 Read(Result, 8);
end;

procedure TCubicMemStream.WriteDouble(d: Double);
begin
 Write(d, 8);
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
   READ MACINTOSH
--------------------------------------------------------------------------------------------------}
{ REVERSE READ - read 4 bytes and swap their position
  LongWord = Cardinal }
function TCubicMemStream.RevReadCardinal: Cardinal;  // Old name: RevReadLongWord
begin
  ReadBuffer(Result, 4);
  SwapCardinal(Result);        { SwapCardinal will correctly swap the byte order of the 32-bit value regardless whether the number is signed or unsigned }
end;


function TCubicMemStream.RevReadInteger: Integer;  // old name: RevReadLongInt
begin
  ReadBuffer(Result, 4);    // Warning! LongInt is 8 byte on Linux, but I read 4!
  SwapInt(Result);
end;


function TCubicMemStream.RevReadWord: Word;          { REVERSE READ - read 2 bytes and swap their position }
begin
  ReadBuffer(Result, 2);
  Result:= Swap(Result);                             { Exchanges high order byte with the low order byte of an integer or word. In Delphi code, Swap exchanges the high-order bytes with the low-order bytes of the argument. X is an expression of type SmallInt, as a 16-bit value, or Word. This is provided for backward compatibility only. }
  //Also see: LightCore.Binary.SwapWord
end;


function TCubicMemStream.RevReadSmallInt: SmallInt;
begin
  ReadBuffer(Result, 2);
  Result:= Swap(Result);                             { Exchanges high order byte with the low order byte of an integer or word. In Delphi code, Swap exchanges the high-order bytes with the low-order bytes of the argument. X is an expression of type SmallInt, as a 16-bit value, or Word. This is provided for backward compatibility only. }
end;


(*
// BAD NAME!
{ Read 4 bytes (UInt4) and swap their position }        // Old name RevReadInt                      // Used in 'UNIT ReadSCF'
function TCubicMemStream.RevReadCardinal_: Cardinal;
begin
 ReadBuffer(Result, 4);
 SwapCardinal(Result);
end;   *)





{--------------------------------------------------------------------------------------------------
   PUSH/LOAD DATA DIRECTLY INTO THE STREAM
---------------------------------------------------------------------------------------------------
   Write raw data to file.

   How to use it:
      SetLength(MyArray, 10);
      FStream.ReadData(MyArray[0], Length(Buffer));
    OR:
      SetLength(s, 10);
      ReadBuffer(s[1], Length(Buffer));
--------------------------------------------------------------------------------------------------}

{ Write raw data to file }
procedure TCubicMemStream.PushData(CONST s: AnsiString);    //ToDo: this should have an overload that saves an array of bytes instead of AnsiString
begin
 Clear;       { Sets the Memory property to nil (Delphi). Sets the Position property to 0. Sets the Size property to 0. }
 WriteBuffer(s[1], Length(s));
end;


procedure TCubicMemStream.PushData(CONST Bytes: TBytes);
begin
 Clear;           { Sets the Memory property to nil (Delphi). Sets the Position property to 0. Sets the Size property to 0. }
 WriteBuffer(Bytes[0], Length(Bytes));
end;




{--------------------------------------------------------------------------------------------------
   RAW
--------------------------------------------------------------------------------------------------}
{ Read the raw content of the file and return it as string (for debugging) }
function TCubicMemStream.AsString: AnsiString;    
begin
 Position:= 0;
 Result:= ReadStringA(Size);
end;


function TCubicMemStream.AsStringU: string;          {THIS SHOULD NEVER BE USED. TO BE DELETED! }
begin
 Result:= string(AsString);
end;


{ Returns the content of the ENTIRE stream }
function TCubicMemStream.AsBytes: TBytes;          { Put the content of the stream into a string }
begin
 Position:= 0;            // Reset stream position
 SetLength(Result, Size); // Allocate size
 Read(Result[0], Size);   // Read content of stream
end;


{ Writes "Buffer" in the stream, at the current pos. The amount of data to be stored is also written down to the stream. }
procedure TCubicMemStream.WriteByteChunk(CONST Buffer: TBytes); // old name: WriteBytes
begin
 WriteCardinal(Length(Buffer));
 WriteBuffer(Buffer[0], High(Buffer));
end;

{ Reads a chunk of data from the current pos of the stream. The amount of data to read is also retrieved from the stream. }
function TCubicMemStream.ReadByteChunk: TBytes;
VAR Cnt: Cardinal;
begin
 Cnt:= ReadCardinal;
 SetLength(Result, Cnt);
 ReadBuffer(Result[0], Cnt);
end;








{--------------------------------------------------------------------------------------------------
   ANSI STRINGS
--------------------------------------------------------------------------------------------------}

{ It automatically detects the length of the string }
function TCubicMemStream.ReadStringA: AnsiString;         
VAR Len: Cardinal;
begin
 ReadBuffer(Len, SizeOf(Len));                                                        { First, find out how many characters to read }
 Assert(Len<= Size- Position, 'TReadCachedStream: String lenght > string size!');
 Result:= ReadStringA(Len);        { Do the actual strign reading }
end;


procedure TCubicMemStream.WriteStringA(CONST s: AnsiString);
VAR Len: Cardinal;
begin
 Len:= Length(s);
 WriteBuffer(Len, SizeOf(Len));
 if Len > 0                                                                 { This makes sure 's' is not empty. Else I will get a RangeCheckError at runtime }
 then WriteBuffer(s[1], Len);
end;

{  STRING WITHOUT LENGTH
   Read a string when its size is unknown (not written in the stream).
   We need to specify the string size from outside. }
function TCubicMemStream.ReadStringA(CONST Len: integer): AnsiString;
VAR ReadBytes: Integer;
begin
 Assert(Len> -1, 'TCubicMemStream.ReadStringA-String size is: '+ IntToStr(Len));

 if (Len+ Position > Size)
 then RAISE exception.Create('TCubicMemStream-Invalid string size: '+ IntToStr(Len));

 if Len= 0
 then Result:= ''
 else
  begin
   SetLength(Result, Len);                                                  { Initialize the result }
   ReadBytes:= Read(Result[1], Len);
   if ReadBytes<> Len                                                       { Not enough data to read? }
   then SetLength(Result, ReadBytes);
  end;
end;



{ CHAR }
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
begin
  VAR FileStream := TCubicBuffStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;
 

procedure TCubicMemStream.SaveToFile(CONST FileName: string);
begin
  VAR FileStream := TCubicBuffStream.Create(FileName, fmCreate);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;






{--------------------------------------------------------------------------------------------------
   UTILS / CONVERSIONS
--------------------------------------------------------------------------------------------------}

function StringFromStream(MemStream: TMemoryStream; Count: Integer = 0; Pos: Integer = 0): string;
var
  StringBytes: TBytes;
begin
  Assert(Count+Pos <= MemStream.Size, 'StringFromStream - Count higher than stream size!');
  if (Count = 0)
  OR (Count > MemStream.Size - Pos)
  then Count:= MemStream.Size - Pos;  // Ensure Count is within bounds of the stream
  SetLength(StringBytes, Count);
  MemStream.Position := Pos;  // Ensure we're at the correct position in the stream
  MemStream.ReadBuffer(StringBytes[0], Count);
  Result := TEncoding.ASCII.GetString(StringBytes);
end;


end.
