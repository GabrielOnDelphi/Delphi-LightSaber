UNIT ccBinary;
 
{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

   Util functions for:
    - String to hex, hex to string conversions (and many others)
    - Binary numbers swapping
    - Data serialization
    - Bit manipulation (set bit, etc)
    - Reverse bits
    - Endianess
    - etc

   See also lazarus TBits: https://wiki.freepascal.org/Bit_manipulation
=============================================================================================================}

INTERFACE

USES
   {$IFDEF MSWINDOWS}Winapi.Windows,{$ENDIF}
   System.SysUtils, System.Classes, System.Types;
 
CONST
   HexNumbers = ['0'..'9', 'a'..'f', 'A'..'F'];


{--------------------------------------------------------------------------------------------------
   STR 2 NUMBER CONVERSIONS
--------------------------------------------------------------------------------------------------}
 function  HexToInt         (CONST HexStr: string): Longint;
 function  BinToInt         (CONST Value: String): Integer;
 function  IntToByte        (CONST i: Integer): Byte;

 function StringIsHexNumber (CONST s: string): boolean;                                              { Returns TRUE if the input parameter is a valid hex number (has the '$0123ABCDEF' or '0123ABCDEF' format) }

 
{--------------------------------------------------------------------------------------------------
   NUMBER 2 STR CONVERSIONS
--------------------------------------------------------------------------------------------------}
 function  IntToBin         (CONST IntNumber, Digits: Integer): string;  
 function  WordToBin        (Value: Word):string;
 function  ByteToBin        (Value: Byte):string;


 
{--------------------------------------------------------------------------------------------------
   BINARY
--------------------------------------------------------------------------------------------------}
 {$IFDEF CPUx86}                                                                                    { Contitional compilation: http://docwiki.embarcadero.com/RADStudio/XE8/en/Conditional_compilation_%28Delphi%29 }
 procedure SwapWord     (VAR TwoBytes : Word);                 assembler;
 Procedure SwapCardinal (VAR aCardinal: Cardinal);             assembler;                           { Swap32bits unsigned integer}
 Procedure SwapInt      (VAR aInteger : Longint);              assembler;                           { Swap32bits signed integer} {INTEL= Little ENDIAN}
 {$ELSE}
 procedure SwapWord     (VAR TwoBytes: word);                  inline;
 Procedure SwapCardinal (VAR aCardinal: Cardinal);             inline;                              { Swap32bits unsigned integer}
 Procedure SwapInt      (VAR LongVAR : integer);               inline;                              { Swap32bits signed integer} {INTEL= Little ENDIAN}
 {$ENDIF}
 function  SwapUInt64   (Value: UInt64): UInt64;               inline;  { Not tested }
 {$IFDEF MSWINDOWS}
 function  SwapInt64    (Value: Int64 ): Int64;                assembler;
 function  SwapCardinalF(aCardinal: Cardinal): Cardinal;       assembler;                           { reverse the order of bytes in eax }
 function  SwapWordF    (twoBytes: Word): Word;                assembler;
 {$ENDIF}
 function  ReverseByte  (      b: Byte): Byte;                 inline;                              { "Inverts" a binary nunber, so, if the number is "1101", I need to end with "1011".  }
 function  ReverseByte2 (CONST b: Byte): Byte;                 { DO NOT INLINE! }                   { This should be the fastest since it uses a LUT - http://stackoverflow.com/questions/14400845/how-can-i-bit-reflect-a-byte-in-delphi }
 function  ReverseByte3 (CONST b: Byte): Byte;                 inline;

 function  RotateRight64 (Value : int64 ; N : Integer): int64; inline;
 function  RotateLeft64  (Value : int64 ; N : Integer): int64; inline;
 function  RotateRight32 (Value : dword ; N : Integer): dword; inline;
 function  RotateLeft32  (Value : dword ; N : Integer): dword; inline;

 function  MakeWord         (B1, B2: byte): Word;              inline;
 function  MakeByte         (CONST b1, b2, b3, b4, b5, b6, b7, b8: Boolean): Byte;  inline;         { B1 is MSB }
 function  MakeCardinal_    (MSB, b2, b3, b4: Cardinal): Cardinal;                                  { Make a cardinal number from 4 bytes. It merges the bytes in the order the were given }
 function  MakeCardinal_Slow(MSB, b2, b3, b4: Cardinal): Cardinal;

 function  MakeCardinal     (CONST Hex: String): Cardinal;         overload;                        { Make a cardinal number from a special string. This string contains 4 substrings, each of 2 chars long. Each substring represents a hex number. The order of the hex numbers is MSB. Example: FF332211 }
 function  MakeCardinal     (Hex1, Hex2, hex3, hex4: String): Cardinal;  overload;                  { Make a cardinal number from strings representing HEX numbers. The order of the parameters is MSB }
 {$IFDEF MSWINDOWS}
 function  SerializeWord    (W: Word): String;  {$ENDIF}                                            { Does the opposite of MakeWord:     Converts the bytes that form this number into their ASCII equivalent. The result is in 'big endian' order. Note that Intel uses 'lil endian'! Exemple: for number 65280 (1111111100000000) the function will return #255 + #0. }
 function  SerializeCardinal(C: Cardinal): string;                                                  { Does the opposite of MakeCardinal: Converts the bytes that form this number into their ASCII equivalent. The result is in 'big endian' order. Note that Intel uses 'lil endian'! Exemple: for number 65280 (1111111100000000) the function will return #255 + #0. }

 function  GetBit      (CONST Value: Cardinal; CONST BitPos: Byte): Boolean;    inline;             { The BitPos numbering starts from left (7) to right (0). For example for number 254 (11111110), the bit at pos 0 si 0 and the bit at pos 7 (MSB) is 1 }
 function  ClearBit    (CONST Value: Cardinal; CONST BitPos: Byte): Cardinal;   inline;
 function  SetBit      (CONST Value: Cardinal; CONST BitPos: Byte): Cardinal;   inline;
 function  ToggleBit   (CONST Value: Cardinal; CONST BitPos: Byte; CONST TurnOn: Boolean): Cardinal; inline;

 function  GetByte     (BytePos: Byte; C: Cardinal): Byte;           overload;                      { Byte order (position): 1 2 3 4.   For example GetByte(3, $AAFFCC) returns $CC }
 function  GetByte     (BytePos: Byte; i: Integer ): Byte;           overload;
 function  GetByte     (BytePos: Byte; W: Word): Byte;               overload;
 function  GetBits     (CONST Value: Cardinal; const BitFrom, BitTo: Byte): Cardinal;

 procedure ChangeByteOrder(VAR Data; Size : Integer);      inline;
 function  Base255to256(CONST cInput: Cardinal): Cardinal; inline;                                  { http://stackoverflow.com/questions/5680895/i-need-to-convert-a-number-from-base-255-to-base-256 }
 function  Base256to255(CONST cInput: Cardinal): Cardinal; inline;

 function  EnsureByte  (b: Integer): Byte;                 inline; overload;                        { Make sure that i is in 'byte' range. In other words, returns 0 if i < 0 and 255 if i > 255. Otherwise return i }
 function  EnsureByte  (b: Real): Byte;                    inline; overload;

 function  Ensure100   (i: integer): Byte;                 inline; overload;                        { Makes sure that the 'I' is not lower than 0 and not higher than 100 }
 function  Ensure100   (s: Single): Single;                inline; overload;                        { Makes sure that the 'S' is not lower than 0 and not higher than 100 }

 function  ReadMotorolaWord(Stream: TStream): Word;



IMPLEMENTATION


{-------------------------------------------------------------------------------------------------------------
    CONVERSIONS
-------------------------------------------------------------------------------------------------------------}

{ Also see IntToHex (by Delphi) }
function HexToInt(CONST HexStr: string): LongInt;
var
  i: integer;
  cTmp: Char;
begin
  if HexStr = '' then RAISE EConvertError.Create('HexToInt3 - Empty hex string!');

  Result:= 0;
  for i:= 1 to Length(HexStr) do begin
    cTmp := HexStr[i];
    case cTmp of
      '0'..'9': Result := 16 * Result + (Ord(cTmp) - $30);
      'A'..'F': Result := 16 * Result + (Ord(cTmp) - $37);
      'a'..'f': Result := 16 * Result + (Ord(cTmp) - $57);
    else
      RAISE EConvertError.Create('Illegal character in hex string');
    end;
  end;
end;


function StringIsHexNumber(CONST s: string): boolean;  { Returns TRUE if the input parameter is a valid hex number (has the '$0123ABCDEF' or '0123ABCDEF' format) }
VAR Start, i: Integer;
begin
 if s= '' then RAISE exception.Create('The hex string is empty!');
 Result:= TRUE;

 if s[1]= '$'
 then Start:= 2
 else Start:= 1;

 for i:= Start to Length(s) DO
  if NOT CharInSet(s[i], HexNumbers)
  then EXIT(FALSE);
end;







function ByteToBin(Value:Byte):string;
CONST
  Bits : array[1..8] of byte = (128,64,32,16,8,4,2,1);
  var i: integer;
begin
  Result:='00000000';
  if (Value<>0) then
  for i:=1 to 8 do
    if (Value and Bits[i])<>0 
 then Result[i]:='1';
end;


function WordToBin(Value:Word):string;
CONST
  Bits : array[1..16] of Word = (32768,16384,8192,4096,2048,1024,512,256,128,64,32,16,8,4,2,1);
  var i: integer;
begin
  Result:='0000000000000000';
  if (Value<>0) then
  for i:=1 to 16 do
    if (Value and Bits[i])<>0 
    then Result[i]:='1';
end;


{ Shows which bits are enabled in IntegerNumber. The endianess is irrelevant. The bits are shown exaclty in the order found: b0, b1, b2... }
function IntToBin(CONST IntNumber, Digits: Integer): string;
begin
 if Digits= 0
 then Result:= ''
 else
   if  (IntNumber AND (1 SHL (Digits-1)))>0
   then result:='1'+IntToBin(IntNumber, Digits-1)
   else result:='0'+IntToBin(IntNumber, Digits-1)
end;


{ Truncates an integer number so that it fits into a byte. It the number is higher than 255 is truncated to 255. It ir is negative it is set to 0. }
function IntToByte(CONST i: Integer): Byte;
begin
  if i > 255
  then Result:= 255
  else
    if i< 0
    then Result:= 0
    else Result:= i;
end;


function BinToInt(CONST Value: String): Integer;
VAR i,Size: Integer;
begin
  Result:= 0;
  Size:= Length(Value);
  for i:= Size downto 1 DO
    if Value[i]='1'
    then Result:= Result+(1 shl (Size-i));
end;




{-----------------------------------------------------------------------------------------------------------------------
   BINARY CONV

------------------------------------------------------------------------------------------------------------------------
  Exista System.Swap -> Exchanges high order byte with the low order byte of an word. If the argument is a 32-bit value then byte 3 and byte 2 are unaffected!
  INTEL uses little ENDIAN

  READ HERE:: http://codeverge.com/embarcadero.delphi.basm/fastest-best-way-to-reverse-byte-orde/1096017
-----------------------------------------------------------------------------------------------------------------------}

{$IFDEF MSWINDOWS} //ASM instrauctions not available on Android
{ WORD }
function SwapWordF(TwoBytes: word): Word; assembler;    { NOT TESTED! }
asm
  {$IFDEF CPUX64}
  mov rax, rcx
  {$ENDIF}
  xchg   al, ah
end;  {$ENDIF}


{$IFDEF CPUx86}
{ See:
     System.Swap
     http://stackoverflow.com/questions/5133938/procedure-that-swaps-the-bytes-low-high-of-a-word-variable
     http://docwiki.embarcadero.com/RADStudio/XE8/en/Conditional_compilation_%28Delphi%29 }
procedure SwapWord(VAR TwoBytes: Word); assembler;
asm
  PUSH EBX     // save EBX
  Mov EBX, TwoBytes
  Mov AX, [EBX]
  XCHG AL,AH
  Mov [EBX], AX
  POP EBX     // restore EBX
end;
{$ELSE}
procedure SwapWord(VAR TwoBytes: Word);
begin
  TwoBytes := Lo(TwoBytes) SHL 8 + Hi(TwoBytes);   { This code works ok on Win64 }
end;
{$ENDIF}




{ CARDINAL }
{$IFDEF MSWINDOWS}   //ASM instrauctions not available on Android
function SwapCardinalF(aCardinal: Cardinal): Cardinal; assembler;   { NOT TESTED! }
asm
  {$IFDEF CPUX64}
  mov rax, rcx
  {$ENDIF}
  bswap eax
end;
{$ENDIF}


{ Swap32bits unsigned integer. $AABBCCDD becomes $DDCCBBAA
  See this for details: http://docwiki.embarcadero.com/RADStudio/XE8/en/Conditional_compilation_%28Delphi%29 }
{ It will correctly swap the byte order of the 32-bit value regardless whether the number is signed or unsigned }
{$IFDEF CPUx86}
procedure SwapCardinal(VAR aCardinal: Cardinal); assembler;
asm
  mov ecx, [eax]
  bswap ecx
  mov [eax], ecx
end;
{$ELSE}
procedure SwapCardinal(VAR aCardinal: Cardinal);
begin
 aCardinal := Swap(aCardinal SHR 16) OR (Swap(aCardinal) SHL 16);    { This code works ok on Win64 }
end;
{$ENDIF}



{ It will correctly swap the byte order of the 32-bit value regardless whether the number is signed or unsigned }
{$IFDEF CPUx86}   // code cloned also in cmStreamMem
procedure SwapInt(VAR aInteger: integer);
asm
  mov ecx, [eax]
  bswap ecx
  mov [eax], ecx
end;
{$ELSE}
procedure SwapInt(VAR LongVar: integer);
begin
  LongVar := Swap(LongVar SHR 16) OR (Swap(LongVar) SHL 16);         { This code was tested ok on Win64 }  // https://www.safaribooksonline.com/library/view/delphi-in-a/1565926595/re314.html
end;
{$ENDIF}



{$IFDEF MSWINDOWS}   //ASM instrauctions not available on Android
function SwapInt64(Value: Int64): Int64;
 {$IF Defined(CPUX86)}
 asm
  MOV     EDX,[DWORD PTR EBP + 12]
  MOV     EAX,[DWORD PTR EBP + 8]
  BSWAP   EAX
  XCHG    EAX,EDX
  BSWAP   EAX
 end;
 {$ELSEIF Defined(CPUX64)}
 asm
   MOV    RAX,RCX
   BSWAP  RAX
 end;
 {$ELSE}
   {$Message Fatal 'Unsupported architecture'}  {TODO 2: do this in all functions that are platform conditionated }        { Contitional compilation: http://docwiki.embarcadero.com/RADStudio/XE8/en/Conditional_compilation_%28Delphi%29 }
   { NOT TESTED! }
   function SwapInt64 (Value: Int64): Int64;   { Not tested }    { Source: http://www.progtown.com/topic1912234-swap-int64-without-asm-insertions.html }
   var P: PInteger;
   begin
     Result:= (Value shl 32) or (Value shr 32);
     P:= @Result;
     P ^:= (Swap (P ^) shl 16) or (Swap (P ^ shr 16));
     Inc (P);
     P ^:= (Swap (P ^) shl 16) or (Swap (P ^ shr 16));
   end;
 {$ENDIF}
{$ENDIF}



function SwapUInt64(Value: UInt64): UInt64;   { Not tested }
begin
 { Source: http://stackoverflow.com/questions/2882434/how-to-convert-big-endian-and-how-to-flip-the-highest-bit/2882606#2882606 }
 raise exception.Create('SwapUInt64-Not implemented!');
end;


{
function SwapInt64F2(CONST X : int64) : int64; register;
  doesn't work!!!!
  http://www.merlyn.demon.co.uk/del-bits.htm
asm
  mov EDX, dword ptr [X] ;
  mov EDX, dword ptr [X+4]
  bswap EDX ;
  bswap EDX
end;

procedure SwapInt64(var X: Int64);

  doesn't work!!!!
  I still see data corruption when I use optimization
  http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_26794624.html?cid=239#a34799142

asm
  mov EBX,dword ptr [X]
  mov ECX,dword ptr [X+4]
  bswap EBX
  bswap ECX
  mov [X],dword ptr ECX
  mov [X+4],dword ptr EBX
end; }



function ReverseByte(b: Byte): Byte;                                                               { "Inverts" a binary nunber, so, if the number is "1101", I need to end with "1011".   http://webcache.googleusercontent.com/search?q=cache:zuCvhGuAgr4J:www.experts-exchange.com/Programming/Game/Q_20497309.html+reverse+of+a+number+binary&cd=3&hl=en&ct=clnk&gl=us&client=firefox-a s}
VAR i: Byte;
begin
  for i:= 0 to 3 DO
    if Odd((b shr i) XOR (b SHR (7 - i)))
    then b:= b XOR ((1 SHL i) OR (1 SHL (7 - i)));
  Result := b;
end;


function ReverseByte3(CONST b : Byte): Byte;
TYPE BS= set of 0..7;
VAR
   K: byte;
   Q: BS;
begin
  Q:= [];
  for K := 0 to 7 do
    if 7-K in BS(b)
    then Include(Q, K);
  Result:= byte(Q)
end;


function ReverseByte2(CONST b: Byte): Byte;                                                        { This should be the fastest since it uses a LUT - http://stackoverflow.com/questions/14400845/how-can-i-bit-reflect-a-byte-in-delphi }
CONST
  Table: array [Byte] of Byte = (
    0,128,64,192,32,160,96,224,16,144,80,208,48,176,112,240,8,136,72,200,40,168,104,232,24,152,88,216,56,184,120,248,
    4,132,68,196,36,164,100,228,20,148,84,212,52,180,116,244,12,140,76,204,44,172,108,236,28,156,92,220,60,188,124,252,
    2,130,66,194,34,162,98,226,18,146,82,210,50,178,114,242,10,138,74,202,42,170,106,234,26,154,90,218,58,186,122,250,
    6,134,70,198,38,166,102,230,22,150,86,214,54,182,118,246,14,142,78,206,46,174,110,238,30,158,94,222,62,190,126,254,
    1,129,65,193,33,161,97,225,17,145,81,209,49,177,113,241,9,137,73,201,41,169,105,233,25,153,89,217,57,185,121,249,
    5,133,69,197,37,165,101,229,21,149,85,213,53,181,117,245,13,141,77,205,45,173,109,237,29,157,93,221,61,189,125,253,
    3,131,67,195,35,163,99,227,19,147,83,211,51,179,115,243,11,139,75,203,43,171,107,235,27,155,91,219,59,187,123,251,
    7,135,71,199,39,167,103,231,23,151,87,215,55,183,119,247,15,143,79,207,47,175,111,239,31,159,95,223,63,191,127,255);
begin
  Result := Table[b];
end;


procedure ChangeByteOrder(VAR Data; Size : Integer);
VAR ptr : PAnsiChar;
    i : Integer;
    c : AnsiChar;
begin
 ptr := @Data;
 for i := 0 to (Size shr 1)-1 do
 begin
   c := ptr^;
   ptr^ := (ptr+1)^;
   (ptr+1)^ := c;
   Inc(ptr, 2);
 end;
end;





{-------------------------------------------------------------------------------------------------------------
   SINGLE BIT OP
-------------------------------------------------------------------------------------------------------------}

function GetBit(CONST Value: Cardinal; CONST BitPos: Byte): Boolean;    // Tested. Works ok
begin
  Result := (Value AND (1 shl BitPos)) <> 0;
end;


function ClearBit(CONST Value: Cardinal; CONST BitPos: Byte): Cardinal;
begin
 Result := Value AND NOT (1 shl BitPos);
end;


{Set bit:
  0=00000000 00000001  - first octed
  1=00000000 00000010
  2=00000000 00000100
  3=00000000 00001000
  4=00000000 00010000
  5=00000000 00100000
  6=00000000 01000000
  7=00000000 10000000
  8=00000001 00000000  - next octet
  etc }
function SetBit(CONST Value: Cardinal; CONST BitPos: Byte): Cardinal;    // Tested. Works ok
begin
 Result := Value OR (1 shl BitPos);
end;


function ToggleBit(CONST Value: Cardinal; CONST BitPos: Byte; CONST TurnOn: Boolean): DWord;
begin
 {$Warnings off}
 Result := (Value OR (1 shl BitPos)) XOR (Cardinal(NOT TurnOn) shl BitPos);       
 {$Warnings on}
end;


function MakeByte(CONST b1, b2, b3, b4, b5, b6, b7, b8: Boolean): Byte;                            { B1 is MSB }
begin
 Result:= 0;
 if b8 then Result:= Result OR (1 shl 0);
 if b7 then Result:= Result OR (1 shl 1);
 if b6 then Result:= Result OR (1 shl 2);
 if b5 then Result:= Result OR (1 shl 3);
 if b4 then Result:= Result OR (1 shl 4);
 if b3 then Result:= Result OR (1 SHL 5);
 if b2 then Result:= Result OR (1 shl 6);
 if b1 then Result:= Result OR (1 shl 7);
end;


{ Get a specific byte from a longer number }
function GetByte(BytePos: Byte; C: Cardinal): Byte;                                                { Byte order (position): 1 2 3 4. So 1 is MSB.  For example GetByte(3, $AAFFCC) returns $CC }
begin
 CASE BytePos of
   1: Result:= Byte(C shr 24);
   2: Result:= Byte(C shr 16);
   3: Result:= Byte(C shr 8);
   4: Result:= Byte(C);
  else
   RAISE exception.Create('Invalid byte position') at @GetByte;
 end;
end;


function GetByte(BytePos: Byte; i: Integer): Byte;      { Extract a Byte from an integer.  The order of the bytes in an integer is this: 1 2 3 4. Example:  GetByte(4, 255) will return 255.   GetByte(4, 256) will return 1 }
begin
 CASE BytePos of
   1: Result:= Byte(I shr 24);
   2: Result:= Byte(I shr 16);
   3: Result:= Byte(I shr 8);
   4: Result:= Byte(I);
  else
   RAISE exception.Create('Invalid byte position') at @GetByte;
 end;
end;
{ also can be done as:
 b:= i mod 256;
 g:=(i mod 65536) div 256;
 r:=(i div 65536) mod 256; but it is probably slower }


function GetByte(BytePos: Byte; W: Word): Byte;                                                { Byte order (position): 1 2 }
begin
 CASE BytePos of
   1: Result:= hi(W);
   2: Result:= lo(W);
  else
   RAISE exception.Create('Invalid byte position') at @GetByte;
 end;
end;


function GetBits(const Value: Cardinal; const BitFrom, BitTo: Byte): Cardinal;
var i, j : Byte;
begin
 Result:=0;
  j:=0;
  for i := BitFrom to BitTo do
  begin
    if GetBit(Value, i) then
       Result:=Result+ (Cardinal(1) shl j);
    inc(j);
  end;
end;


function EnsureByte(b: Integer): Byte;    { Make sure that i is in 'byte' range. In other words, returns 0 if i < 0 and 255 if i > 255. Otherwise return i }
begin
 if b < 0
 then Result:= 0
 else
   if b > 255
   then Result:= 255
   else Result:= b;
end;



{-------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------}

function EnsureByte(b: Real): Byte;
begin
 if b < 0
 then Result:= 0
 else
   if b > 255
   then Result:= 255
   else Result:= round(b);
end;


{ Makes sure that the 'I' is not lower than 0 and not higher than 100 }
function Ensure100(i: integer): Byte;
CONST
   MinINT = -2147483648;
begin
 case i of
  MinInt..-1: Result:= 0;
  0..100    : Result:= i;
  else        Result:= 100;
 end;
 {
 if i> 100
 then Result:= 100  else
   if i< 0
   then Result:= 0
   else Result:= i; }
end;


function Ensure100(s: Single): Single;          { Makes sure that the 'S' is not lower than 0 and not higher than 100 }
begin
 if s> 100
 then Result:= 100
 else
   if s< 0
   then Result:= 0
   else Result:= s;
end;



{-------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------}

{ ROTATE }
function RotateRight64(Value: int64 ; N : Integer): int64;   { Source: http://www.merlyn.demon.co.uk/del-bits.htm }
begin
 Result := (Value shr N) + (Value shl (64-N))
end;

function RotateLeft64(Value: int64 ; N : Integer): int64;
begin
 Result := (Value shl N) + (Value shr (64-N))
end;

function RotateRight32(Value : dword ; N : Integer): dword;
begin
 Result := (Value shr N) + (Value shl (32-N))
end;

function RotateLeft32(Value : dword ; N : Integer): dword;
begin
 Result := (Value shl N) + (Value shr (32-N))
end;





{-------------------------------------------------------------------------------------------------------------
    MAKE CARDINAL
-------------------------------------------------------------------------------------------------------------}

TYPE
  CardinalRec = packed record
    case Integer of
      0: (Lo, Hi: Word);
      1: (Words: array [1..2] of Word);
      2: (Bytes: array [1..4] of Byte);
   end;

{ Make a cardinal number from 4 bytes. It merges the bytes in the order the were given.
  See MakeCardinal, which is faster }
function MakeCardinal_Slow(MSB, b2, b3, b4: Cardinal): Cardinal;
begin
 Result:= b4;
 Result:= Result+ (b3  SHL  8);
 Result:= Result+ (b2  SHL 16);
 Result:= Result+ (MSB SHL 24);
end;


function MakeCardinal_(MSB, b2, b3, b4: Cardinal): Cardinal;                      { Make a cardinal number from 4 bytes. The order of the parameters is MSB }
begin
 CardinalRec(Result).Bytes[1] := b4;                                              { In Intel cardinals the first byte is LSB }
 CardinalRec(Result).Bytes[2] := b3;
 CardinalRec(Result).Bytes[3] := b2;
 CardinalRec(Result).Bytes[4] := MSB;
end;


function MakeCardinal (Hex1, Hex2, Hex3, Hex4: String): Cardinal;                 { Make a cardinal number from strings representing Hex numbers. The order of the parameters is MSB }
begin
 CardinalRec(Result).Bytes[1] := HexToInt(Hex4);                                  { In Intel cardinals the first byte is LSB }
 CardinalRec(Result).Bytes[2] := HexToInt(Hex3);
 CardinalRec(Result).Bytes[3] := HexToInt(Hex2);
 CardinalRec(Result).Bytes[4] := HexToInt(Hex1);
end;


function MakeCardinal (CONST Hex: String): Cardinal;                              { Make a cardinal number from a special string. This string contains 4 substrings, each of 2 chars long. Each substring represents a Hex number. The order of the Hex numbers is MSB. Example: FF332211 }
VAR Hex1, Hex2, Hex3, Hex4: string;
begin
 Hex1:= system.COPY(Hex, 1, 2);
 Hex2:= system.COPY(Hex, 3, 2);
 Hex3:= system.COPY(Hex, 5, 2);
 Hex4:= system.COPY(Hex, 7, 2);

 Result:= MakeCardinal(Hex1, Hex2, Hex3, Hex4);
end;


function MakeWord(B1, B2: byte): Word;
begin
 Result:= 256* B1;                                                                { Details here: http://docwiki.embarcadero.com/RADStudio/XE3/en/Internal_Data_Formats }
 Result:= Result+ B2;
end;




{-------------------------------------------------------------------------------------------------------------
    SERIALIZATION
-------------------------------------------------------------------------------------------------------------}

{ Does the opposite of MakeWord: converts the bytes that form this number into their ASCII equivalent.
  The result is in 'big endian' order. Note that Intel uses 'lil endian'!
  Exemple: for number 65280 (1111111100000000) the function will return #255 + #0. }
{$IFDEF MSWINDOWS}
function SerializeWord(W: Word): String;
begin
 Result:=         Chr(HiByte(W));
 Result:= Result+ Chr(LoByte(W));
end;  {$ENDIF}


{ Converts the bytes that form this number into their ASCII equivalent.
  The result is in 'big endian' order. Note that Intel uses 'lil endian'!
  Exemple: for number 65280 (1111111100000000) the function will return #255 + #0. }
function SerializeCardinal(C: Cardinal): string;
begin
 Result:=         IntToHex(CardinalRec(c).Bytes[4], 2);
 Result:= Result+ IntToHex(CardinalRec(c).Bytes[3], 2);
 Result:= Result+ IntToHex(CardinalRec(c).Bytes[2], 2);
 Result:= Result+ IntToHex(CardinalRec(c).Bytes[1], 2);
end;






function Base255to256(CONST cInput: Cardinal): Cardinal;
VAR MSB, b2, b3, b4: Byte;
begin
 MSB:= Byte(cInput SHR 24);
 b2 := Byte(cInput SHR 16);
 b3 := Byte(cInput SHR  8);
 b4 := Byte(cInput);

 Result:= 0;
 Result:= Result+ MSB * 16581375;                                                 { b1 is MSB, b8 is LSB }
 Result:= Result+  b2 * 65025;
 Result:= Result+  b3 * 255;
 Result:= Result+  b4;
end;


function Base256to255(CONST cInput: Cardinal): Cardinal;                          { MUST SEE THIS:  http://stackoverflow.com/questions/6015477/i-cannot-use-shl-shift-left-with-int64-variables }
VAR MSB, b2, b3, b4: Byte;
begin
 b4 :=  cInput               mod 255;
 b3 := (cInput DIV 255)      mod 255;
 b2 := (cInput DIV 65025)    mod 255;
 MSB:= (cInput DIV 16581375) mod 255;

 { Make cardinal }
 CardinalRec(Result).Bytes[1] := b4;                                              { Little endian. First byte is LSB.   INTEL= Little ENDIAN}
 CardinalRec(Result).Bytes[2] := b3;
 CardinalRec(Result).Bytes[3] := b2;
 CardinalRec(Result).Bytes[4] := MSB;
end;


function ReadMotorolaWord(Stream: TStream): Word;
 TYPE
   TMotorolaWord = record
     case Byte of
       0: (Value: Word);
       1: (Byte1, Byte2: Byte);
   end;

 VAR
    MW: TMotorolaWord;
begin
  { It would probably be better to just read these two bytes in normally and then do a small ASM routine to swap them.  But we aren't talking about reading entire files, so I doubt the performance gain would be worth the trouble. }
  Stream.Read(MW.Byte2, SizeOf(Byte));
  Stream.Read(MW.Byte1, SizeOf(Byte));
  Result:= MW.Value;
end;

(*
SEE THIS:
    http://stackoverflow.com/questions/6015477/i-cannot-use-shl-shift-left-with-int64-variables

int64 can hold up to 9223372036854775807

function Base255to256(CONST cInput: Int64): Int64;
VAR MSB, b2, b3, b4, b5, b6, b7, b8: Byte;
begin
 MSB:= Byte(cInput SHR 56);                                                                        { b1 is MSB, b8 is LSB }
 b2 := Byte(cInput SHR 48);
 b3 := Byte(cInput SHR 40);
 b4 := Byte(cInput SHR 32);
 b5 := Byte(cInput SHR 24);
 b6 := Byte(cInput SHR 16);
 b7 := Byte(cInput SHR  8);
 b8 := Byte(cInput);
 Result:=  0;
 Result:=         MSB * 70110209207109375;
 Result:= Result+  b2 * 274941996890625;
 Result:= Result+  b3 * 1078203909375;
 Result:= Result+  b4 * 4228250625;
 Result:= Result+  b5 * 16581375;
 Result:= Result+  b6 * 65025;
 Result:= Result+  b7 * 255;
 Result:= Result+  b8;
end;

*)

end.







