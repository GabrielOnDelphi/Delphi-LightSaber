UNIT cmStreamMem;

{=============================================================================================================
   2023.01
   See Copyright.txt
==============================================================================================================

   Description
      Class extender for TMemoryStream
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

INTERFACE
USES
   Winapi.Windows, IdGlobal, System.SysUtils, System.Classes;

TYPE
  TCubicMemStream= class(TMemoryStream)
    private
    public
     { Header }
     function  ReadMagicNo      (CONST MagicNo: AnsiString): Boolean;
     procedure WriteMagicNo     (CONST MagicNo: AnsiString);
     function  ReadCheckPoint   : Boolean;
     procedure WriteCheckPoint;

     {}
     function  ReadEnter: Boolean;
     procedure WriteEnter;
     procedure ReadPadding      (CONST Bytes: Integer= 1024);
     procedure WritePadding     (CONST Bytes: Integer= 1024);

     { Strings }                                                                { Write the string but don't write its length }
     function  ReadCharactersA  (CONST Lungime: Integer): AnsiString;
     function  ReadCharacters   (CONST Lungime: Integer): string;
     procedure WriteCharacters  (CONST s: string);        overload;
     procedure WriteCharacters  (CONST s: AnsiString);    overload;

     procedure ReadStringList   (TSL: TStringList);       overload;
     procedure ReadStringList   (TSL: TStrings);          overload;
     procedure WriteStringList  (CONST TSL: TStringList); overload;
     procedure WriteStringList  (CONST TSL: TStrings);    overload;

     function  ReadStringU: string;                                             { Works for both Delphi7 and Delphi UNICODE }
     function  ReadStringA: AnsiString;                   overload;             { It automatically detects the length of the string }
     function  ReadStringA      (CONST Lungime: integer): AnsiString; overload; { You need to specify the length of the string }
     procedure WriteStringU     (CONST s: string);                              { Works for both Delphi7 and Delphi UNICODE }
     procedure WriteStringA     (CONST s: AnsiString);

     function  ReadChar: AnsiChar;
     procedure WriteChar(CONST c: AnsiChar);

     { Integers }
     function  ReadInteger : Longint;
     function  ReadInt64   : Int64;
     function  ReadUInt64  : UInt64;
     procedure WriteUInt64 (CONST i: UInt64);
     procedure WriteInt64  (CONST i: Int64);
     procedure WriteInteger(CONST i: Longint);

     function  ReadCardinal: Cardinal;
     function  ReadBoolean : Boolean;
     function  ReadByte    : Byte;
     function  ReadWord    : Word;

     procedure WriteBoolean (CONST b: bool);
     procedure WriteCardinal(CONST c: Cardinal);
     procedure WriteByte    (CONST aByte: Byte);
     procedure WriteWord    (CONST aWord: Word);

     { Reals }
     function  ReadDate : TDateTime;                                { This is a DOUBLE }
     procedure WriteDate(CONST aDate: TDateTime);

     function  ReadSingle : Single;
     procedure WriteSingle(CONST Sngl: Single);

     { Reverse read }
     function  RevReadLongWord: Cardinal;                           { REVERSE READ - read 4 bytes and swap their position }
     function  RevReadLongInt : Longint;
     function  RevReadSmallInt: Smallint;
     function  RevReadWord    : Word;                               { REVERSE READ - read 2 bytes and swap their position }
     function  ReadRevInt     : Cardinal;                           { REVERSE READ - read 4 bytes and swap their position - reads a UInt4 }

     { Raw }
     function  AsStringU: String;                                   { Returns the content of the stream as a string }
     function  AsString: AnsiString;
     function  AsBytes: TIdBytes;

     procedure PushData(CONST s: AnsiString); overload;             { Put binary data (or text) into the stream }
     procedure PushData(CONST Bytes: TIDBytes);  overload;
  end;

IMPLEMENTATION





{--------------------------------------------------------------------------------------------------
   UTILS
--------------------------------------------------------------------------------------------------}

{$IFDEF CPUx86}
{ Swap32bits unsigned integer. $AABBCCDD becomes $DDCCBBAA }
{ This code does not work on Win64 }
{ See this for details: http://docwiki.embarcadero.com/RADStudio/XE8/en/Conditional_compilation_%28Delphi%29 }
procedure SwapCardinal(VAR aCardinal: Cardinal); assembler;
asm
  mov ecx, [eax]
  bswap ecx
  mov [eax], ecx
end;
{$ELSE}
procedure SwapCardinal(VAR aCardinal: Cardinal);
begin
 aCardinal := Swap(aCardinal SHR 16) OR (Swap(aCardinal) SHL 16);  { This code was tested ok on Win64 }
end;
{$ENDIF}

{$IFDEF CPUx86}
procedure SwapInt(VAR LongVar: Longint);                           { It will not work with negative numbers becuase the sign will be swapped also.  Longint = signed 32 integer (classic integer).      Ex: $AABBCCDD becomes $DDCCBBAA }
asm
  mov ecx, [eax]                                                   { This code does not work on Win64 }
  bswap ecx
  mov [eax], ecx
end;
{$ELSE}
procedure SwapInt(VAR LongVar: Longint);
begin
  LongVar := Swap(LongVar SHR 16) OR (Swap(LongVar) SHL 16);       { This code was tested ok on Win64 }  // https://www.safaribooksonline.com/library/view/delphi-in-a/1565926595/re314.html
end;
{$ENDIF}
















{--------------------------------------------------------------------------------------------------
   UNICODE STRINGS
--------------------------------------------------------------------------------------------------}
procedure TCubicMemStream.WriteStringU(CONST s: string);                        { Works for both Delphi7 and Delphi UNICODE }
VAR
  Lungime: cardinal;
  UTF: UTF8String;
begin
 UTF := UTF8String(s);

 { Write length }
 Lungime := Length(UTF);
 WriteBuffer(Lungime, SizeOf(Lungime));

 { Write string }
 if Lungime > 0
 then WriteBuffer(UTF[1], Lungime);
end;


CONST MB = 1048576;
function TCubicMemStream.ReadStringU: string;                                   { Works for both Delphi7 and Delphi UNICODE }
VAR
   Lungime: Cardinal;
   UTF: UTF8String;
begin
 ReadBuffer(Lungime, 4);                                                        { Read length }

 Assert(Lungime < 50*MB, 'Project file too large: '+ IntToStr(lungime));        { ATENTION WITH THIS LINE !!!! }

 if Lungime > 0
 then
  begin
   SetLength(UTF, Lungime);                                                     { Read string }
   ReadBuffer(UTF[1], Lungime);
   Result:= string(UTF);
  end
 else Result:= '';
end;





{--------------------------------------------------------------------------------------------------
   ASCII STRINGS
--------------------------------------------------------------------------------------------------}
function TCubicMemStream.ReadStringA: AnsiString;                               { It automatically detects the length of the string }
VAR Lungime: LongInt;
begin
 ReadBuffer(Lungime, 4);                                                        { First, find out how many characters to read }
 Result:= ReadStringA(Lungime);                                                 { Do the actual strign reading }
end;

procedure TCubicMemStream.WriteStringA(CONST s: AnsiString);
VAR Lungime: cardinal;
begin
 Lungime:= Length(s);
 WriteBuffer(Lungime, SizeOf(Lungime));
 if Lungime > 0                                                                 { This makes sure 's' is not empty. Else I will get a RangeCheckError at runtime }
 then WriteBuffer(s[1], Lungime);
end;




function TCubicMemStream.ReadChar: AnsiChar;                                    { Read one single Ansi character }
begin
 ReadBuffer(Result, 1);
end;

procedure TCubicMemStream.WriteChar(CONST C: AnsiChar);
begin
 WriteBuffer(c, 1);
end;





{--------------------------------------------------------------------------------------------------
   STRING WITHOUT LENGTH
--------------------------------------------------------------------------------------------------}
function TCubicMemStream.ReadStringA(CONST Lungime: integer): AnsiString;       { You need to specify the length of the string }
begin
 Assert(Lungime> -1, 'TCubicMemStream.ReadStringA-String size is: '+ IntToStr(Lungime));

 if (Lungime+ Position > Size)
 then RAISE exception.Create('TCubicMemStream-Invalid string size: '+ IntToStr(Lungime));

 if Lungime= 0
 then Result:= ''
 else
  begin
   SetLength(Result, Lungime);                                                  { Initialize the result }
   ReadBuffer(Result[1], Lungime);
  end;
end;




procedure TCubicMemStream.WriteCharacters(CONST s: AnsiString);                 { Write the string but don't write its length }
begin
 Assert(s<> '', 'TCubicMemStream.WriteCharacters - The string is empty');       { This makes sure 's' is not empty. Else I will get a RangeCheckError at runtime }
 WriteBuffer(s[1], Length(s));
end;


procedure TCubicMemStream.WriteCharacters(CONST s: string);                     { Works for both Delphi7 and Delphi UNICODE }    // old name: WriteStringANoLen
VAR UTF: UTF8String;
begin
 UTF := UTF8String(s);
 if Length(UTF) > 0
 then WriteBuffer(UTF[1], Length(UTF));
end;




function TCubicMemStream.ReadCharacters(CONST Lungime: Integer): string;        { Works for both Delphi7 and Delphi UNICODE }
VAR UTF: UTF8String;
begin
 if Lungime < 1 then EXIT('');

 SetLength(UTF, Lungime);
 ReadBuffer(UTF[1], Lungime);
 Result:= string(UTF);
end;


function TCubicMemStream.ReadCharactersA(CONST Lungime: Integer): AnsiString;
begin
 if Lungime < 1 then EXIT('');

 SetLength(Result, Lungime);
 ReadBuffer(Result[1], Lungime);
end;







{--------------------------------------------------------------------------------------------------
   STRING LIST
--------------------------------------------------------------------------------------------------}
procedure TCubicMemStream.WriteStringList(CONST TSL: TStringList);
begin
 WriteStringU(TSL.Text);
end;

procedure TCubicMemStream.WriteStringList(CONST TSL: TStrings);
begin
 WriteStringU(TSL.Text);
end;


procedure TCubicMemStream.ReadStringList(TSL: TStringList);
begin
 TSL.Text:= ReadStringU;
end;

procedure TCubicMemStream.ReadStringList(TSL: TStrings);
begin
 TSL.Text:= ReadStringU;
end;




{--------------------------------------------------------------------------------------------------
   AS STRING
--------------------------------------------------------------------------------------------------}

function TCubicMemStream.AsString: AnsiString;       { Put the content of the stream into a string }
begin
 Position:= 0;
 Result:= ReadStringA(Size);
end;


function TCubicMemStream.AsBytes: TIdBytes;          { Put the content of the stream into a string }
begin
 Position:=0;    // Reset stream position
 SetLength(result, Size);    // Allocate size
 Read(result[0], Size);    // Read content of stream
end;


function TCubicMemStream.AsStringU: string;          {THIS SHOULD NEVER BE USED. TO BE DELETED! }
begin
 Result:= string(AsString);
end;




{--------------------------------------------------------------------------------------------------
   ENTER
--------------------------------------------------------------------------------------------------}
function TCubicMemStream.ReadEnter: Boolean;                                    { Returns TRUE if the byte read is LF }
VAR aByte: Byte;
begin
 ReadBuffer(aByte, 1);
 Result:= aByte= Byte(LF);
end;


procedure TCubicMemStream.WriteEnter;
VAR aByte: Byte;
begin
 aByte:= Byte(LF);
 WriteBuffer(abyte, 1);
end;







{--------------------------------------------------------------------------------------------------
   NUMBERS
--------------------------------------------------------------------------------------------------}
function TCubicMemStream.ReadRevInt: Cardinal;                                  { REVERSE READ - read 4 bytes and swap their position - reads a UInt4. Used in 'UNIT ReadSCF' }
begin
 ReadBuffer(Result, 4);
 SwapCardinal(Result);
end;

function TCubicMemStream.ReadInt64: Int64;
begin
 ReadBuffer(Result, 8);
end;




function TCubicMemStream.ReadUInt64: UInt64;
begin
 ReadBuffer(Result, 8);
end;

procedure TCubicMemStream.WriteUInt64(CONST i: UInt64);                         { UInt64 Defines a 64-bit unsigned integer type. UInt64 represents a subset of the natural numbers. The range for the UInt64 type is from 0 through 2^64-1. The size of UInt64 is 64 bits across all 64-bit and 32-bit platforms. }
begin
 WriteBuffer(i, 8);                                                             { Longint = Fundamental integer type. Its size will not change! }
end;

procedure TCubicMemStream.WriteInt64(CONST i: Int64);
begin
 WriteBuffer(i, 8);
end;





function TCubicMemStream.ReadInteger: Longint;
begin
 ReadBuffer(Result, 4);
end;

procedure TCubicMemStream.WriteInteger(CONST i: Longint);
begin
 WriteBuffer(i, 4);                                                             { Longint = Fundamental integer type. Its size will not change! }
end;



procedure TCubicMemStream.WriteBoolean(CONST b: bool);
begin
 WriteBuffer(b, 1);
end;

function TCubicMemStream.ReadBoolean: Boolean;
VAR b: byte;
begin
 ReadBuffer(b, 1);                                                              { Valid values for a Boolean are 0 and 1. If you put a different value into a Boolean variable then future behaviour is undefined. You should read into a byte variable b and assign b <> 0 into the Boolean. Or sanitise by casting the byte to ByteBool. Or you may choose to validate the value read from the file and reject anything other than 0 and 1. http://stackoverflow.com/questions/28383736/cannot-read-boolean-value-with-tmemorystream }
 Result:= b <> 0;
end;



procedure TCubicMemStream.WriteCardinal(CONST c: Cardinal);
begin
 WriteBuffer(c, 4);
end;

function TCubicMemStream.ReadCardinal: Cardinal;                                { Cardinal IS NOT a fundamental type!! }
begin
 ReadBuffer(Result, 4);
end;



procedure TCubicMemStream.WriteByte(CONST aByte: Byte);
begin
 WriteBuffer(aByte, 1);
end;

function TCubicMemStream.ReadByte: Byte;
begin
 ReadBuffer(Result, 1);
end;



procedure TCubicMemStream.WriteWord(CONST aWord: Word);
begin
 WriteBuffer(aWord, 2);
end;

function TCubicMemStream.ReadWord: Word;
begin
 ReadBuffer(Result, 2);
end;











{ DATE }
function TCubicMemStream.ReadDate: TDateTime;
VAR Temp: Double;
begin
 ReadBuffer(Temp, 8);                                                           { The size of Double is 8 bytes }
 Result:= Temp;
end;


procedure TCubicMemStream.WriteDate(CONST aDate: TDateTime);
VAR Temp: Double;
begin
 Temp:= aDate;
 WriteBuffer(Temp, 8);                                                          { The size of Double is 8 bytes }
end;




{ SINGLE }

function TCubicMemStream.ReadSingle: Single;
begin
 Read(Result, 4);                                                               { The size of Double is 8 bytes }
end;


procedure TCubicMemStream.WriteSingle(CONST Sngl: Single);
begin
 Write(sngl, 4);                                                                { The size of Double is 8 bytes }
end;






{--------------------------------------------------------------------------------------------------
   READ MACINTOSH
--------------------------------------------------------------------------------------------------}
function TCubicMemStream.RevReadLongword: Cardinal;  { REVERSE READ - read 4 bytes and swap their position }
begin
  ReadBuffer( Result, 4);
  SwapCardinal(Result);
end;


function TCubicMemStream.RevReadLongInt: Longint;
begin
  ReadBuffer(Result, 4);
  SwapInt(Result);
end;


function TCubicMemStream.RevReadWord: Word;          { REVERSE READ - read 2 bytes and swap their position }
begin
  ReadBuffer(Result, 2);
  Result:= Swap(Result);                             { Exchanges high order byte with the low order byte of an integer or word. In Delphi code, Swap exchanges the high-order bytes with the low-order bytes of the argument. X is an expression of type SmallInt, as a 16-bit value, or Word. This is provided for backward compatibility only. }
end;


function TCubicMemStream.RevReadSmallInt: SmallInt;
begin
  ReadBuffer(Result, 2);
  Result:= Swap(Result);                             { Exchanges high order byte with the low order byte of an integer or word. In Delphi code, Swap exchanges the high-order bytes with the low-order bytes of the argument. X is an expression of type SmallInt, as a 16-bit value, or Word. This is provided for backward compatibility only. }
end;




{--------------------------------------------------------------------------------------------------
                     PUSH/LOAD DATA DIRECTLY INTO THE STREAM
---------------------------------------------------------------------------------------------------
   How to use ReadData:
      Buffer: array of Byte;
      SetLength(Buffer, 10);
      FStream.ReadData(Buffer[0], Length(Buffer));
    OR:
      Buffer: string;
      SetLength(Buffer, 10);
      ReadBuffer(Buffer[1], Length(Buffer));
--------------------------------------------------------------------------------------------------}

procedure TCubicMemStream.PushData(CONST s: AnsiString);
begin
 Clear;                                                           { Sets the Memory property to nil (Delphi). Sets the Position property to 0. Sets the Size property to 0. }
 WriteBuffer(s[1], Length(s));
end;


procedure TCubicMemStream.PushData(CONST Bytes: TIDBytes);
begin
 Clear;                                                           { Sets the Memory property to nil (Delphi). Sets the Position property to 0. Sets the Size property to 0. }
 WriteBuffer(Bytes[0], Length(Bytes));
end;









{--------------------------------------------------------------------------------------------------
   PADDING
--------------------------------------------------------------------------------------------------}
procedure TCubicMemStream.WritePadding(CONST Bytes: Integer= 1024);
VAR s: string;
begin
 if Bytes> 0 then
  begin
   s:= StringOfChar(#0, Bytes);
   WriteBuffer(s[1], Bytes);
  end;
end;


procedure TCubicMemStream.ReadPadding(CONST Bytes: Integer= 1024);
VAR s: string;
begin
 if Bytes> 0 then
  begin
   SetLength(s, Bytes);
   ReadBuffer(s[1], Bytes);
  end;
end;








{--------------------------------------------------------------------------------------------------
   MAGIC NO
   This can be used as signature for your stream/file.
   It is already recommended that the first bytes in a stream/file are a signature
--------------------------------------------------------------------------------------------------}
function TCubicMemStream.ReadMagicNo(CONST MagicNo: AnsiString): Boolean;       { Read a string from disk and compare it with MagicNo. Retursn TRUE if it matches }
begin
 Result:= ReadCharactersA(Length(MagicNo)) = MagicNo;
end;


procedure TCubicMemStream.WriteMagicNo(CONST MagicNo: AnsiString);
begin
 Assert(MagicNo > '', 'Magic number is empty!');
 WriteCharacters(MagicNo);
end;






{--------------------------------------------------------------------------------------------------
   Write a checkpoint into the stream.
   Used for debugging.
--------------------------------------------------------------------------------------------------}
CONST
   ctCheckPoint= '<*>Checkpoint<*>';


function TCubicMemStream.ReadCheckPoint: Boolean;
begin
 Result:= ReadStringA = ctCheckPoint;
end;


procedure TCubicMemStream.WriteCheckPoint;
begin
 WriteStringA(ctCheckPoint);
end;



end.

