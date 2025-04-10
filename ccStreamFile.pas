UNIT ccStreamFile;

{=============================================================================================================
   2025.02
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Description
      Extends TFileStream. It may be used as a drop-in replacement for TFileStream.
      https://gabrielmoraru.com/saving-an-object-to-disk-file/
--------------------------------------------------------------------------------------------------------------
  Features:
     Direct read/write bytes, cardinals, words, integers, strings to a (binary) file.
     Supports both ANSI and Unicode.

  Large files
     It loads the entire contents of a file into the memory so don't use it with huge (hundreds MB) files.
     If the file is large or you want to read only a portion of the file, consider ccStreamBuff.pas

  Speed
     TCubicBuffStream is much faster!
     We don't use SizeOf. We always use constant values when reading/writing from disk. This way, we can always read our data even if Delphi changes the size of Integer.

  Read vs ReadBuffer:
     Read treats Count as an upper bound.
     The ReadBuffer, by contrast, raises an exception if Count bytes cannot be read.
     So this is better if I don't accept errors in my files.

  Also see TBinaryReader / TBinaryWriter
     http://docwiki.embarcadero.com/CodeExamples/Tokyo/en/TBinaryReader_and_TBinaryWriter_(Delphi)
     Disadvantage: the class can only read or write from a file (not both at the same time).
=============================================================================================================}

INTERFACE

{ $I Frameworks.inc}

USES
   System.SysUtils, System.Classes;

TYPE
  TCubicFileStream= class(TFileStream)
    public
     { Strings }
     procedure WriteString         (CONST s: string);                                        { Old name: WriteString }
     procedure WriteStringNoLen    (CONST s: string);
     procedure WriteStringList     (CONST TSL: TStringList);
     procedure WriteStringANoLen   (CONST s: AnsiString);                                    { Write the string but don't write its length }
     procedure WriteStringA        (CONST s: AnsiString);
     procedure WriteEnter;

     function  ReadString: string;                                                           { Old name: ReadString }
     function  ReadStringA  (CONST Len: integer): AnsiString; overload;                      { It will raise an error if there is not enough data (Len) to read }
     function  ReadStringAR (CONST Len: integer): AnsiString;                                { This is the relaxed version. It won't raise an error if there is not enough data (Len) to read }
     function  ReadStringA: AnsiString;                       overload;                      { It automatically detects the length of the string }
     procedure ReadStringList (TSL: TStringList);
     function  ReadEnter: Boolean;
     {}
     function  RevReadLongword : Cardinal;                                                   { REVERSE READ - read 4 bytes and swap their position. For Motorola format. }
     function  RevReadInt      : Integer;
     function  RevReadWord     : Word;                                                       { REVERSE READ - read 2 bytes and swap their position. For Motorola format. }
     {}
     function  ReadInteger     : Longint;
     function  ReadInt64       : Int64;
     function  ReadUInt64      : UInt64;
     function  ReadCardinal    : Cardinal;
     function  ReadRevInt      : Cardinal;                                                   { REVERSE READ - read 4 bytes and swap their position - reads a UInt4 }
     function  ReadBoolean     : Boolean;
     function  ReadByte        : Byte;
     function  ReadWord        : Word;
     function  ReadDate        : TDateTime;
     procedure ReadPadding     (CONST Bytes: Integer);
     function  ReadStringNoLen (CONST Len: Integer): string;
     {}
     procedure WriteUInt64     (const i: UInt64);
     procedure WriteInteger    (CONST i: Longint);
     procedure WriteBoolean    (CONST b: Boolean);
     procedure WriteCardinal   (CONST c: Cardinal);
     procedure WritePadding    (CONST Bytes: Integer);
     procedure WriteDate       (CONST aDate: TDateTime);
     procedure WriteByte       (CONST aByte: Byte);
     procedure WriteWord       (CONST aWord: Word);

     function  ReadCheckPoint: Boolean;
     procedure WriteCheckPoint;
     function  ReadMagicNo     (CONST MagicNo: AnsiString): Boolean;
     procedure WriteMagicNo    (CONST MagicNo: AnsiString);

     function  AsStringU: String;                                                            { Returns the content of the stream as a string }
     function  AsString: AnsiString;
     procedure PushData(CONST Data: AnsiString);                                             { Put binary data (or text) into the stream }
  end;


{ Read the first Count characters from a file }
function StringFromFileStart (CONST FileName: string; Count: Cardinal): AnsiString;


IMPLEMENTATION
USES ccBinary;

CONST ctCheckPoint= '<*>Checkpoint<*>';                                                      { For debugging. Write this from time to time to your file so if you screwup, you check from time to time to see if you are still reading the correct data. }



 



{--------------------------------------------------------------------------------------------------
   MAGIC NO
--------------------------------------------------------------------------------------------------}

{ Read a string from file and compare it with MagicNo.
  Return TRUE if it matches (it means we read the correct file format). }
function TCubicFileStream.ReadMagicNo(CONST MagicNo: AnsiString): Boolean;
VAR MagNo: AnsiString;
begin
 MagNo:= ReadStringA(Length(MagicNo));
 Result:= MagicNo = MagicNo;
end;


procedure TCubicFileStream.WriteMagicNo(CONST MagicNo: AnsiString);
begin
 Assert(MagicNo > '', 'Magic number is empty!');
 Write(MagicNo[1], Length(MagicNo));
end;








{--------------------------------------------------------------------------------------------------
   PADDING
   It is important to read/write some padding bytes.
   If you later (as your program evolves) need to save extra data into your file, you use the padding bytes. This way you don't need to change your file format.
--------------------------------------------------------------------------------------------------}
procedure TCubicFileStream.WritePadding(CONST Bytes: Integer);
VAR s: string;
begin
 if Bytes> 0 then
  begin
   s:= StringOfChar(#0, Bytes); // GenerateString(Bytes, #0);
   WriteBuffer(s[1], Bytes);
  end;
end;

procedure TCubicFileStream.ReadPadding(CONST Bytes: Integer);
VAR s: string;
begin
 if Bytes> 0 then
  begin
   SetLength(s, Bytes);
   ReadBuffer(s[1], Bytes);
  end;
end;







{--------------------------------------------------------------------------------------------------
   ASCII STRINGS
--------------------------------------------------------------------------------------------------}
{ It automatically detects the length of the string }
function TCubicFileStream.ReadStringA: AnsiString;
VAR Len: LongInt;
begin
 ReadBuffer(Len, 4);                                                                         { First, find out how many characters to read }
 Result:= ReadStringA(Len);                                                                  { Do the actual strign reading }
end;


procedure TCubicFileStream.WriteStringA(CONST s: AnsiString);
VAR Len: cardinal;
begin
 Len:= Length(s);
 WriteBuffer(Len, SizeOf(Len));
 if Len > 0                                                                                  { This makes sure 's' is not empty (nothing to read). Else we will get a RangeCheckError at runtime }
 then WriteBuffer(s[1], Len);
end;





{--------------------------------------------------------------------------------------------------
   UNICODE STRINGS
--------------------------------------------------------------------------------------------------}
procedure TCubicFileStream.WriteString(CONST s: string);
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


function TCubicFileStream.ReadString: string;
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








{}
procedure TCubicFileStream.WriteStringList(CONST TSL: TStringList);
begin
 WriteString(TSL.Text);
end;


procedure TCubicFileStream.ReadStringList(TSL: TStringList);
begin
 TSL.Text:= String(ReadStringA);
end;





{}
function TCubicFileStream.ReadEnter: Boolean;   { Returns TRUE if the byte read is LF }
VAR aByte: Byte;
begin
 ReadBuffer(aByte, 1);
 Result:= aByte= Byte(#10);
end;


procedure TCubicFileStream.WriteEnter;
VAR aByte: Byte;
begin
 aByte:= Byte(#10);
 WriteBuffer(abyte, 1);
end;













{--------------------------------------------------------------------------------------------------
   NUMBERS
--------------------------------------------------------------------------------------------------}
function TCubicFileStream.ReadRevInt: Cardinal;                                              { REVERSE READ - read 4 bytes and swap their position - reads a UInt4. Used in 'UNIT ReadSCF' }
begin
 ReadBuffer(Result, 4);
 SwapCardinal(Result);
end;

function TCubicFileStream.ReadInt64: Int64;
begin
 ReadBuffer(Result, 8);
end;




function TCubicFileStream.ReadUInt64: UInt64;
begin
 ReadBuffer(Result, 8);
end;

procedure TCubicFileStream.WriteUInt64(CONST i: UInt64);
begin
 WriteBuffer(i, 8);                                                                          { Longint = Fundamental integer type. Its size will not change! }
end;




function TCubicFileStream.ReadInteger: Longint;
begin
 ReadBuffer(Result, 4);
end;

procedure TCubicFileStream.WriteInteger(CONST i: Longint);
begin
 WriteBuffer(i, 4);                                                                          { Longint = Fundamental integer type. Its size will not change! }
end;




procedure TCubicFileStream.WriteCardinal(CONST c: Cardinal);
begin
 WriteBuffer(c, 4);
end;

function TCubicFileStream.ReadCardinal: Cardinal;    { Cardinal IS NOT a fundamental type BUT its size is 32 bits across 64-bit and 32-bit platforms.  }
begin
 ReadBuffer(Result, 4);
end;




procedure TCubicFileStream.WriteBoolean(CONST b: Boolean);
begin
 WriteBuffer(b, 1);
end;

function TCubicFileStream.ReadBoolean: Boolean;
VAR b: byte;
begin
 ReadBuffer(b, 1);    { Valid values for a Boolean are 0 and 1. If you put a different value into a Boolean variable then future behaviour is undefined. You should read into a byte variable b and assign b <> 0 into the Boolean. Or sanitise by casting the byte to ByteBool. Or you may choose to validate the value read from the file and reject anything other than 0 and 1. http://stackoverflow.com/questions/28383736/cannot-read-boolean-value-with-tmemorystream }
 Result:= b <> 0;
end;




procedure TCubicFileStream.WriteByte(CONST aByte: Byte);
begin
 WriteBuffer(aByte, 1);
end;

function TCubicFileStream.ReadByte: Byte;
begin
 ReadBuffer(Result, 1);
end;




procedure TCubicFileStream.WriteWord(CONST aWord: Word);
begin
 WriteBuffer(aWord, 2);
end;

function TCubicFileStream.ReadWord: Word;
begin
 ReadBuffer(Result, 2);
end;









{   TIME   }
function TCubicFileStream.ReadDate: TDateTime;
VAR Temp: Double;
begin
 ReadBuffer(Temp, 8);                                                                        { The size of Double is 8 bytes }
 Result:= Temp;
end;


procedure TCubicFileStream.WriteDate(CONST aDate: TDateTime);
VAR Temp: Double;
begin
 Temp:= aDate;
 WriteBuffer(Temp, 8);                                                                       { The size of Double is 8 bytes }
end;










{--------------------------------------------------------------------------------------------------
                                READ MACINTOSH
--------------------------------------------------------------------------------------------------}
function TCubicFileStream.RevReadLongword: Cardinal;                                         { REVERSE READ - read 4 bytes and swap their position }
begin
  ReadBuffer( Result, 4);
  SwapCardinal(Result);
end;


function TCubicFileStream.RevReadInt: Integer;  // old name: RevReadLongInt
begin
  ReadBuffer(Result, 4);
  SwapInt(Result);
end;


function TCubicFileStream.RevReadWord: Word;                                                 { REVERSE READ - read 2 bytes and swap their position }
begin
  ReadBuffer(Result, 2);
  Result:= Swap(Result);                                                                     { Exchanges high order byte with the low order byte of an integer or word. In Delphi code, Swap exchanges the high-order bytes with the low-order bytes of the argument. X is an expression of type SmallInt, as a 16-bit value, or Word. This is provided for backward compatibility only. }
end;




{--------------------------------------------------------------------------------------------------
   PUSH/LOAD DATA DIRECTLY INTO THE STREAM
--------------------------------------------------------------------------------------------------}

{ Read the raw content of the file and return it as string (for debugging) }
function TCubicFileStream.AsString: AnsiString;
begin
 Position:= 0;
 Result:= ReadStringA(Size);
end;


{ Write raw data to file }
procedure TCubicFileStream.PushData(CONST Data: AnsiString); // ToDo: this should have an overload that saves an array of bytes instead of AnsiString
begin
 WriteBuffer(Data[1], Length(Data));
end;


{ THIS SHOULD NEVER BE USED. TO BE DELETED! }
function TCubicFileStream.AsStringU: string;
begin
 Result:= string(AsString);
end;











{--------------------------------------------------------------------------------------------------
                                Others
--------------------------------------------------------------------------------------------------}

{ For debugging. Write a scheckpoint entry (just a string) from time to time to your file so if you screwup, you check from time to time to see if you are still reading the correct data. }
function TCubicFileStream.ReadCheckPoint: Boolean;
begin
 Result:= ReadStringA = ctCheckPoint;
end;

procedure TCubicFileStream.WriteCheckPoint;
begin
 WriteStringA(ctCheckPoint);
end;





{--------------------------------------------------------------------------------------------------
   Special functions
   STRING WITHOUT LENGTH
--------------------------------------------------------------------------------------------------}
procedure TCubicFileStream.WriteStringANoLen(CONST s: AnsiString);                           { Write the string but don't write its length }
begin
 Assert(s<> '', 'WriteStringA - The string is empty');                                       { This makes sure 's' is not empty. Else I will get a RangeCheckError at runtime }
 WriteBuffer(s[1], Length(s));
end;


function TCubicFileStream.ReadStringA(CONST Len: integer): AnsiString;                       { You need to specify the length of the string }
begin
 Assert(Len> -1, 'TCubicFileStream-String size is: '+ IntToStr(Len));

 if (Len+ Position > Size)
 then raise exception.Create('TCubicFileStream-Invalid string size: '+ IntToStr(Len));

 if Len= 0
 then Result:= ''
 else
  begin
   SetLength(Result, Len);                                                                   { Initialize the result }
   ReadBuffer(Result[1], Len);
  end;
end;


function TCubicFileStream.ReadStringAR(CONST Len: integer): AnsiString;                      { This is the relaxed/safe version. It won't raise an error if there is not enough data (Len) to read }
VAR ReadBytes: Integer;
begin
 Assert(Len> -1, 'TCubicFileStream-String size is: '+ IntToStr(Len));

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
  In most cases you will want to use WriteString instead of WriteStringNoLen }
procedure TCubicFileStream.WriteStringNoLen(CONST s: string);
VAR UTF: UTF8String;
begin
 UTF := UTF8String(s);
 if Length(UTF) > 0
 then WriteBuffer(UTF[1], Length(UTF));
end;


{ Read a string from file. The length of the string will be provided from outside. }
function TCubicFileStream.ReadStringNoLen(CONST Len: Integer): string;
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
VAR StreamFile: TCubicFileStream;
begin
 SetLength(Result, Count);

 StreamFile:= TCubicFileStream.Create(FileName, fmOpenRead);                                          { <--------- EFCreateError:   Cannot create file "blablabla". Access is denied. }
 TRY
   Result:= StreamFile.ReadStringAR(Count);
 FINALLY
   FreeAndNil(StreamFile);
 END;
end;


end.





