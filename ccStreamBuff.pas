UNIT ccStreamBuff;

{=============================================================================================================
   CubicDesign
   2022-04-03

   Extends TBufferedFileStream.
   This class adds new functionality that does not exist in Delphi's original stream classes:
   - Read/WriteBoolean
   - Read/WriteString (Ansi/Unicode)
   - Read/WriteInteger
   - Read/WriteCardinal
   - Read/WriteDate
   - Read/Write mac files (inverted byte endianness)
   - etc
   It may be used as a drop-in replacement for TFileStream.

=============================================================================================================
   TBufferedFileStream:
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

     So use:
       TCubicBuffStream.Create(FileName, fmOpenRead)                  -> to read
       TCubicBuffStream.Create(FileName, fmOpenWrite OR fmCreate)     -> to write to existing or create new file if none exists
     or use:
       constructor CreateRead (FileName);                             -> to read
       constructor CreateWrite(FileName);                             -> to write to existing or create new file if none exists

   Speed
     When reading character by character, the new System.Classes.TBufferedFileStream seems to be 210% faster than ccStreamBuffHefferman

   Tester:
     CubicCommonControls\Demo\cc StreamBuff\
=============================================================================================================}

{$WARN DUPLICATE_CTOR_DTOR OFF}

INTERFACE

USES
   Winapi.Windows, System.Types, System.SysUtils, System.Classes;

TYPE
  TCubicBuffStream= class(System.Classes.TBufferedFileStream)
    public
     MagicNo: AnsiString; // Obsolete!

     { Strings }
     procedure WriteStringU     (CONST s: string);
     procedure WriteStringA     (CONST s: AnsiString);
     procedure WriteStringUNoLen(CONST s: string);
     procedure WriteStringANoLen(CONST s: AnsiString);                       { Write the string but don't write its length }
     procedure WriteChars       (CONST s: AnsiString);
     procedure WriteStrings     (TSL: TStrings);
     procedure WriteEnter;

     function  ReadStringAR     (CONST Len: integer): AnsiString;            { This is the relaxed version. It won't raise an error if there is not enough data (Len) to read }
     function  ReadStringA      (CONST Len: integer): AnsiString; overload;  { It will raise an error if there is not enough data (Len) to read }
     function  ReadStringA: AnsiString;                           overload;  { It automatically detects the length of the string }
     function  ReadEnter: Boolean;
     function  ReadChars  (Count: Longint): AnsiString;
     procedure ReadStrings(TSL: TStrings);
     function  ReadStringU: string;
     { Reverse read }
     function  RevReadLongword: Cardinal;                                    { REVERSE READ - read 4 bytes and swap their position. For Motorola format. }
     function  RevReadLongInt : Longint;
     function  RevReadWord    : Word;                                        { REVERSE READ - read 2 bytes and swap their position. For Motorola format. }
     function  RevReadInt     : Cardinal;                                    { REVERSE READ - read 4 bytes and swap their position - reads a UInt4 }
     {}
     function  ReadUInt64  : UInt64;
     function  ReadInteger : Longint;
     function  ReadInt64   : Int64;
     function  ReadCardinal: Cardinal;
     function  ReadBoolean : Boolean;
     function  ReadByte    : Byte;
     function  ReadWord    : Word;
     function  ReadDate    : TDateTime;
     function  ReadStringUNoLen (CONST Len: Integer): string;
     function  ReadSmallInt: Smallint;
     function  ReadShortInt: ShortInt;
     function  ReadBytes   : TByteDynArray;

     function  ReadSingle  : Single;
     function  ReadDouble  : Double;
     {}
     procedure WriteUInt64   (const i: UInt64);
     procedure WriteInt64    (const i: Int64);
     procedure WriteInteger  (CONST i: Longint);
     procedure WriteBoolean  (CONST b: bool);
     procedure WriteCardinal (CONST c: Cardinal);
     procedure WriteDate     (CONST d: TDateTime);
     procedure WriteByte     (CONST b: Byte);
     procedure WriteWord     (CONST w: Word);
     procedure WriteSmallInt (const s: SmallInt);
     procedure WriteShortInt (const s: ShortInt);
     procedure WriteBytes    (const Buffer: TByteDynArray);
     procedure WriteSingle   (const s: Single);
     procedure WriteDouble   (const d: Double);

     { Header OLD }
     function  ReadMagicVer: Word;
     function  ReadMagicNo(const MagicNo: AnsiString): Boolean;
     procedure WriteMagicNo(const MagicNo: AnsiString);
     procedure WriteMagicVer(const MVersion: Word);

     { Header NEW }
     function  ReadHeader (CONST Signature: AnsiString): Word;                   overload;
     function  ReadHeader (CONST Signature: AnsiString; Version: Word): Boolean; overload;
     procedure WriteHeader(CONST Signature: AnsiString; Version: Word);

     function  ReadCheckPoint: Boolean;
     procedure WriteCheckPoint;

     procedure ReadPadding  (CONST Bytes: Integer);
     procedure WritePadding (CONST Bytes: Integer= 1024);

     { BT }
     function  AsString: AnsiString;
     procedure PushData(CONST Data: AnsiString);                                             { Put binary data (or text) into the stream }
     function  CountAppearance(C: AnsiChar): Int64;

     {}
     constructor CreateRead (CONST FileName: string);
     constructor CreateWrite(CONST FileName: string);
  end;



{ Read the first Count characters from a file }
function StringFromFileStart (CONST FileName: string; Count: Cardinal): AnsiString;


IMPLEMENTATION
USES
   ccCore, ccBinary;


{--------------------------------------------------------------------------------------------------
   MAGIC NO
   Obsolete. Still used in Bionix. Use ReadMagicVer instead.
--------------------------------------------------------------------------------------------------}

{ Read a string from file and compare it with MagicNo.
  Return TRUE if it matches (it means we read the correct file format). }
function TCubicBuffStream.ReadMagicNo(CONST MagicNo: AnsiString): Boolean;
VAR s: AnsiString;
begin
 s:= ReadStringA(Length(MagicNo));
 Result:= MagicNo = s;
end;


procedure TCubicBuffStream.WriteMagicNo(CONST MagicNo: AnsiString);
begin
 Assert(MagicNo > '', 'Magic number is empty!');
 Write(MagicNo[1], Length(MagicNo));
end;



{ MAGIC NUMBER
  Read the first x chars in a file and compares it with MagicNo.
  If matches then reads another reads the FileVersion word.
  Returns the FileVersion. If magicno fails, it returns zero }
function TCubicBuffStream.ReadMagicVer: Word;
VAR s: AnsiString;
begin
 Assert(MagicNo > '', 'MagicNo is empty!');

 s:= ReadStringA(Length(MagicNo));
 if s = MagicNo
 then Result:= ReadWord
 else Result:= 0;
end;


procedure TCubicBuffStream.WriteMagicVer(CONST MVersion: Word);
begin
 Assert(MagicNo > '', 'Magic number is empty!');
 if MVersion= 0
 then RAISE Exception.Create('MagicVersion must be higher than 0!');

 WriteBuffer(MagicNo[1], Length(MagicNo));
 WriteWord(MVersion);
end;




constructor TCubicBuffStream.CreateRead(CONST FileName: string);
begin
 inherited Create(FileName, fmOpenRead, 1*mb);
end;


constructor TCubicBuffStream.CreateWrite(CONST FileName: string);
begin
 inherited Create(FileName, fmOpenWrite OR fmCreate);
end;












{ Reads file signature and version number. Returns True if found correct data.
  Read the first x chars in a file and compares it with MagicNo.
  If matches then reads another reads the FileVersion word.
  Returns the FileVersion. If magicno fails, it returns zero }
function TCubicBuffStream.ReadHeader(CONST Signature: AnsiString; Version: Word): Boolean;
VAR s: AnsiString;
begin
 Assert(Signature > '', 'Signature is empty!');
 Assert(Version   > 0 , 'Version must be > 0');

 s:= ReadStringA;
 Result:= s = Signature;
 if Result
 then Result:= ReadWord = Version;
end;


{ Reads file signature and version number.
  Returns version number or 0 on fail.
  Useful when we have multiple file versions }
function TCubicBuffStream.ReadHeader(CONST Signature: AnsiString): Word;
VAR s: AnsiString;
begin
 Assert(Signature > '', 'Signature is empty!');

 s:= ReadStringA;
 if s = Signature
 then Result:= ReadWord
 else Result:= 0;
end;


procedure TCubicBuffStream.WriteHeader(CONST Signature: AnsiString; Version: Word);
begin
 Assert(Signature > '', 'Signature is empty!');
 Assert(Version> 0, 'Version must be higher than 0!');

 WriteStringA(Signature);
 WriteWord(Version);
end;



{--------------------------------------------------------------------------------------------------
   PADDING
   It is important to read/write some padding bytes.
   If you later (as your program evolves) need to save extra data into your file, you use the padding bytes. This way you don't need to change your file format.
--------------------------------------------------------------------------------------------------}
procedure TCubicBuffStream.WritePadding(CONST Bytes: Integer= 1024);
VAR b: TBytes;
begin
 if Bytes> 0 then
  begin
   SetLength(b, Bytes);
   FillChar (b[0], Bytes, #0);
   WriteBuffer(b[0], Bytes);
  end;
end;


procedure TCubicBuffStream.ReadPadding(CONST Bytes: Integer);
VAR b: TBytes;
begin
 if Bytes> 0 then
  begin
   SetLength(b, Bytes);
   ReadBuffer(b[0], Bytes);
  end;
end;







{--------------------------------------------------------------------------------------------------
   ASCII STRINGS
--------------------------------------------------------------------------------------------------}
function TCubicBuffStream.ReadStringA(CONST Len: integer): AnsiString; { You need to specify the length of the string }
VAR TotalBytes: Integer;
begin
 if Len> 0
 then
  begin
   SetLength(Result, Len);                                              { Initialize the result }
   //FillChar(Result[1], Len, '?'); DEBUG ONLY!
   TotalBytes:= Read(Result[1], Len);                                   { Read is used in cases where the number of bytes to read from the stream is not necessarily fixed. It attempts to read up to Count bytes into buffer and returns the number of bytes actually read.  }

   if TotalBytes= 0
   then Result:= ''
   else
     if TotalBytes < Len                                                { If there is not enough data to read... }
     then SetLength(Result, TotalBytes);                                { ...set the buffer to whater I was able to read }
  end
 else Result:= '';
end;


{ It automatically detects the length of the string }                   { Old name: ReadStringU }
function TCubicBuffStream.ReadStringA: AnsiString;
VAR Len: Cardinal;
begin
 ReadBuffer(Len, 4);                                                                         { First, find out how many characters to read }
 Assert(Len<= Size- Position, 'TReadCachedStream: String lenght > string size!');
 Result:= ReadStringA(Len);
end;


procedure TCubicBuffStream.WriteStringA(CONST s: AnsiString);
VAR Len: Cardinal;
begin
 Len:= Length(s);
 WriteBuffer(Len, 4);
 if Len > 0                                                                                  { This makes sure 's' is not empty (nothing to read). Else we will get a RangeCheckError at runtime }
 then WriteBuffer(s[1], Len);
end;




{--------------------------------------------------------------------------------------------------
   UNICODE STRINGS
--------------------------------------------------------------------------------------------------}
procedure TCubicBuffStream.WriteStringU(CONST s: string);
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


function TCubicBuffStream.ReadStringU: string;
VAR
   Len: Cardinal;
   UTF: UTF8String;
begin
 ReadBuffer(Len, 4);                                                                         { Read length }
 if Len > 0
 then
  begin
   SetLength(UTF, Len);                                                                      { Read string }
   ReadBuffer(UTF[1], Len);
   Result:= string(UTF);
  end
 else Result:= '';
end;





{--------------------------------------------------------------------------------------------------
   SPECIAL STRINGS
--------------------------------------------------------------------------------------------------}

{ TSL }
procedure TCubicBuffStream.WriteStrings(TSL: TStrings);
begin
 WriteStringU(TSL.Text);
end;


procedure TCubicBuffStream.ReadStrings(TSL: TStrings);
begin
 TSL.Text:= ReadStringU;
end;



{ ENTER }
function TCubicBuffStream.ReadEnter: Boolean;   { Returns TRUE if the byte read is LF }
VAR Byte1, Byte2: Byte;
begin
 ReadBuffer(Byte1, 1);
 ReadBuffer(Byte2, 1);
 Result:= (Byte1= Byte(#13)) AND (Byte2= Byte(#10));
end;


procedure TCubicBuffStream.WriteEnter;
VAR W: Word;
begin
 W:= $0D0A;
 WriteBuffer(w, 2);
end;



{--------------------------------------------------------------------------------------------------
   CHARS
--------------------------------------------------------------------------------------------------}

{ Writes a bunch of chars from the file. Why 'chars' and not 'string'? This function writes C++ strings (the length of the string was not written to disk also) and not real Delphi strings. }
procedure TCubicBuffStream.WriteChars(CONST s: AnsiString);
begin
 Assert(Length(s) > 0);
 WriteBuffer(s[1], Length(s));
end;

{ Reads a bunch of chars from the file. Fedra. Why 'ReadChars' and not 'ReadString'? This function reads C++ strings (the length of the string was not written to disk also) and not real Delphi strings. So, i have to give the number of chars to read as parameter. IMPORTANT: The function will reserve memory for s. }
function TCubicBuffStream.ReadChars(Count: Longint): AnsiString;
begin
 if Count= 0 then RAISE Exception.Create('Count is zero!');                     { It gives a range check error if you try s[1] on an empty string so we added 'Count = 0' as protection. }
 SetLength(Result, Count);
 ReadBuffer(Result[1], Count);
{ Alternative:  Result:= Read(Pointer(s)^, Count)= Count;     <--- Don't use this! Ever. See explanation from A Buchez:   http://stackoverflow.com/questions/6411246/pointers-versus-s1 }
end;











{--------------------------------------------------------------------------------------------------
   INTEGERS
--------------------------------------------------------------------------------------------------}

procedure TCubicBuffStream.WriteCardinal(CONST c: Cardinal);
begin
 WriteBuffer(c, 4);
end;

function TCubicBuffStream.ReadCardinal: Cardinal;    { Cardinal IS NOT a fundamental type BUT its size is 32 bits across 64-bit and 32-bit platforms.  }
begin
 ReadBuffer(Result, 4);
end;



procedure TCubicBuffStream.WriteInteger(CONST i: Integer);
begin
 WriteBuffer(i, 4);                                                                          { Longint = Fundamental integer type. Its size will not change! }
end;

function TCubicBuffStream.ReadInteger: Integer;
begin
 ReadBuffer(Result, 4);
end;



procedure TCubicBuffStream.WriteInt64(CONST i: Int64);
begin
 WriteBuffer(i, 8);
end;

function TCubicBuffStream.ReadInt64: Int64;
begin
 ReadBuffer(Result, 8);
end;



procedure TCubicBuffStream.WriteUInt64(CONST i: UInt64);
begin
 WriteBuffer(i, 8);                                                                          { Longint = Fundamental integer type. Its size will not change! }
end;

function TCubicBuffStream.ReadUInt64: UInt64;
begin
 ReadBuffer(Result, 8);
end;








{ BYTE }

procedure TCubicBuffStream.WriteByte(CONST b: Byte);
begin
 WriteBuffer(b, 1);
end;

function TCubicBuffStream.ReadByte: Byte;
begin
 ReadBuffer(Result, 1);
end;





{ BOOLEAN }

procedure TCubicBuffStream.WriteBoolean(CONST b: bool);
begin
 WriteBuffer(b, 1);
end;

function TCubicBuffStream.ReadBoolean: Boolean;
VAR b: byte;
begin
 ReadBuffer(b, 1);    { Valid values for a Boolean are 0 and 1. If you put a different value into a Boolean variable then future behaviour is undefined. You should read into a byte variable b and assign b <> 0 into the Boolean. Or sanitise by casting the byte to ByteBool. Or you may choose to validate the value read from the file and reject anything other than 0 and 1. http://stackoverflow.com/questions/28383736/cannot-read-boolean-value-with-tmemorystream }
 Result:= b <> 0;
end;



procedure TCubicBuffStream.WriteShortInt(CONST s: ShortInt);     //Signed 8bit: -128..127
begin
 Write(s, 1);
end;

function TCubicBuffStream.ReadShortInt: ShortInt;
begin
 Read(Result, 1);
end;



procedure TCubicBuffStream.WriteSmallInt(CONST s: SmallInt);     //Signed 16bit: -32768..32767
begin
 Write(s, 2);
end;

function TCubicBuffStream.ReadSmallInt: SmallInt;
begin
 Read(Result, 2);
end;





{ WORD }

procedure TCubicBuffStream.WriteWord(CONST w: Word);
begin
 WriteBuffer(w, 2);
end;

function TCubicBuffStream.ReadWord: Word;
begin
 ReadBuffer(Result, 2);
end;







{ BYTES }

procedure TCubicBuffStream.WriteBytes(CONST Buffer: TByteDynArray);
begin
 WriteCardinal(Length(Buffer));
 WriteBuffer(Buffer[0], High(Buffer));
end;

function TCubicBuffStream.ReadBytes: TByteDynArray;
VAR Cnt: Cardinal;
begin
 Cnt:= ReadCardinal;
 SetLength(Result, Cnt);
 ReadBuffer(Result[0], Cnt);
end;





{ FLOATS }

function TCubicBuffStream.ReadSingle: Single;
begin
 Read(Result, 4);
end;

procedure TCubicBuffStream.WriteSingle(CONST s: Single);
begin
 Write(s, 4);
end;



function TCubicBuffStream.ReadDouble: Double;
begin
 Read(Result, 8);
end;

procedure TCubicBuffStream.WriteDouble(CONST d: Double);
begin
 Write(d, 8);
end;




{ DATE }

function TCubicBuffStream.ReadDate: TDateTime;
begin
 ReadBuffer(Result, 8);
end;


procedure TCubicBuffStream.WriteDate(CONST d: TDateTime);
begin
 WriteBuffer(d, 8);  { The size of Double is 8 bytes }
end;












{--------------------------------------------------------------------------------------------------
   READ MACINTOSH
--------------------------------------------------------------------------------------------------}
function TCubicBuffStream.RevReadLongword: Cardinal;                                         { REVERSE READ - read 4 bytes and swap their position }
begin
  ReadBuffer( Result, 4);
  SwapCardinal(Result);
end;


function TCubicBuffStream.RevReadLongInt: Longint;
begin
  ReadBuffer(Result, 4);
  SwapInt(Result);
end;


function TCubicBuffStream.RevReadWord: Word;                                                 { REVERSE READ - read 2 bytes and swap their position }   // old name ReadWordSwap
begin
  ReadBuffer(Result, 2);
  Result:= Swap(Result);                                                                     { Exchanges high order byte with the low order byte of an integer or word. In Delphi code, Swap exchanges the high-order bytes with the low-order bytes of the argument. X is an expression of type SmallInt, as a 16-bit value, or Word. This is provided for backward compatibility only. }
  //Also see: ccBinary.SwapWord
end;


function TCubicBuffStream.RevReadInt: Cardinal;                                              { REVERSE READ - read 4 bytes and swap their position - reads a UInt4. Used in 'UNIT ReadSCF' }
begin
 ReadBuffer(Result, 4);
 SwapCardinal(Result);
end;







{--------------------------------------------------------------------------------------------------
   PUSH/LOAD DATA DIRECTLY INTO THE STREAM
--------------------------------------------------------------------------------------------------}

{ Read the raw content of the file and return it as string (for debugging) }
function TCubicBuffStream.AsString: AnsiString;
begin
 Position:= 0;
 Result:= ReadStringA(Size);
end;


{ Write raw data to file }
procedure TCubicBuffStream.PushData(CONST Data: AnsiString);   //ToDo: this should have an overload that saves an array of bytes instead of AnsiString
begin
 WriteBuffer(Data[1], Length(Data));
end;










{--------------------------------------------------------------------------------------------------
   Others
--------------------------------------------------------------------------------------------------}
CONST
   ctCheckPoint= '<*>Checkpoint<*>';    { For debugging. Write this from time to time to your file so if you screwup, you check from time to time to see if you are still reading the correct data. }


{ For debugging. Write a scheckpoint entry (just a string) from time to time to your file so if you screwup, you check from time to time to see if you are still reading the correct data. }
function TCubicBuffStream.ReadCheckPoint: Boolean;
begin
 Result:= ReadStringA = ctCheckPoint;
end;

procedure TCubicBuffStream.WriteCheckPoint;
begin
 WriteStringA(ctCheckPoint);
end;





{--------------------------------------------------------------------------------------------------
   Special functions
   STRING WITHOUT LENGTH
--------------------------------------------------------------------------------------------------}
procedure TCubicBuffStream.WriteStringANoLen(CONST s: AnsiString);                           { Write the string but don't write its length }
begin
 Assert(s<> '', 'WriteStringA - The string is empty');                                       { This makes sure 's' is not empty. Else I will get a RangeCheckError at runtime }
 WriteBuffer(s[1], Length(s));
end;


function TCubicBuffStream.ReadStringAR(CONST Len: integer): AnsiString;                      { This is the relaxed/safe version. It won't raise an error if there is not enough data (Len) to read }
VAR ReadBytes: Integer;
begin
 Assert(Len> -1, 'TCubicBuffStream-String size is: '+ IntToStr(Len));

 if (Len+ Position > Size)
 then raise Exception.Create('TCubicBuffStream-Invalid string size!');

 if Len= 0
 then Result:= ''
 else
  begin
   SetLength(Result, Len);                                                                   { Initialize the result }
   ReadBytes:= Read(Result[1], Len);
   if ReadBytes<> Len                                                                        { Not enough data to read? }
   then SetLength(Result, ReadBytes);
  end;
end;



{ Writes the string to file but does not write its length.
  In most cases you will want to use WriteString instead of WriteStringUNoLen. }             { Used in cmEncodeMime.pas }
procedure TCubicBuffStream.WriteStringUNoLen(CONST s: string);
VAR UTF: UTF8String;
begin
 UTF := UTF8String(s);
 if Length(UTF) > 0
 then WriteBuffer(UTF[1], Length(UTF));
end;


{ Read a string from file. The length of the string will be provided from outside. }
function TCubicBuffStream.ReadStringUNoLen(CONST Len: Integer): string;
VAR UTF: UTF8String;
begin
 if Len > 0
 then
  begin
   SetLength(UTF, Len);
   ReadBuffer(UTF[1], Len);
   Result:= string(UTF);
  end
 else Result:= '';
end;


{ Read the first Count characters from a file.
  Returns ANSI string. }
function StringFromFileStart (CONST FileName: string; Count: Cardinal): AnsiString;
VAR StreamFile: TCubicBuffStream;
begin
 SetLength(Result, Count);

 StreamFile:= TCubicBuffStream.Create(FileName, fmOpenRead);                                          { <--------- EFCreateError:   Cannot create file "blablabla". Access is denied. }
 TRY
   Result:= StreamFile.ReadStringAR(Count);
 FINALLY
   FreeAndNil(StreamFile);
 END;
end;


function TCubicBuffStream.CountAppearance(C: AnsiChar): Int64;    { Used by TFasParser.CountSequences }
var
   s: AnsiString;    { When I open the file I don't know how many sequences I have inside. CountSequences reads all sequences to find out how many I have }
   BuffPo: Int64;
begin
 Result:= 0;
 BuffPo:= 0;
 Position:= 0;

 WHILE BuffPo< Size DO
  begin
   s:= ReadStringA(1024*KB);
   Inc(BuffPo, 1024*KB);
   Result:= Result+ Cardinal(ccCore.CountAppearance(c, s));
  end;
end;



(* put it back

function TCubicBuffStream.CountLines: Int64;{ NOTE!!! ccCore.CountLines is 5.2 times faster than this, but that function does not handle well Mac/Linux files!!! }
  procedure ReadLn;
  VAR
     ValidChar, i: Integer;                 { ValidChar = the position of the caracter BEFORE the enter }
  begin
   ValidChar:= Length(Buff);                { I set it to the end of the buffer because I need it this way when I read the last line in the FastQ file and there is no ENTER at the end (the file doesn't end with an enter) }

   { Find the first enter }
   for i:= BuffPos to Length(Buff) DO
    if Buff[i] in [CR, LF] then
     begin
      ValidChar:= i;
      Break;
     end;
   BuffPos:= ValidChar;                     { Skip over the ENTER character }

   { Find additional enter characters }
   WHILE (BuffPos+1 <= Length(Buff) )       { there is more data to read? }
   AND (Buff[BuffPos+1] in [CR, LF])
    DO inc(BuffPos);

   inc(BuffPos);   //Now BuffPos points to the first char in the next row
  end;

begin
 Result:= 0;

 FirstLine;
 fillBuffer;

 WHILE BuffPos < Length(Buff) DO  {This checks for EOF }
  begin
   ReadLn;       {TODO 1: speed improvement: modify the ReadLine function to a procedure. I don't need to actually dead the text. I only need to find the enters }
   Inc(Result);
  end;
end; *)





end.
