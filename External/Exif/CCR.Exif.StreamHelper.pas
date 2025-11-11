{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing image metadata               }
{ Version 1.5.3                                                                        }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is CCR.Exif.StreamHelper.pas.                                      }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2014 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

{$I CCR.Exif.inc}
unit CCR.Exif.StreamHelper;

interface

uses
  SysUtils, Classes;

type
  {$IF not Declared(TBytes)}
  TBytes = array of Byte;           //added in D2007
  {$IFEND}

  {$IF not Declared(UnicodeString)}
  UnicodeString = WideString;       //added in D2009
  {$IFEND}

  PDoubleArray = ^TDoubleArray;
  TDoubleArray = array[0..High(TByteArray) div 8] of Double;

  PShortIntArray = ^TShortIntArray;
  TShortIntArray = array[0..High(TByteArray)] of ShortInt;

  PSmallIntArray = ^TSmallIntArray;
  TSmallIntArray = array[0..High(TWordArray)] of SmallInt;

  PLongWordArray = ^TLongWordArray;
  TLongWordArray = array[0..High(TWordArray) div 2] of LongWord;

  PLongIntArray = ^TLongIntArray;
  TLongIntArray = array[0..High(TWordArray) div 2] of LongInt;

  TEndianness = (SmallEndian, BigEndian);

  TStreamHelper = class helper for TStream
    function ReadByte(var Value: Byte): Boolean; overload; inline;
    function ReadByte: Byte; overload; inline;
    function ReadSmallInt(Endianness: TEndianness; var Value: SmallInt): Boolean; overload;
    function ReadSmallInt(Endianness: TEndianness): SmallInt; overload; inline;
    function ReadWord(Endianness: TEndianness; var Value: Word): Boolean; overload;
    function ReadWord(Endianness: TEndianness): Word; overload; inline;
    function ReadLongInt(Endianness: TEndianness; var Value: LongInt): Boolean; overload;
    function ReadLongInt(Endianness: TEndianness): LongInt; overload; inline;
    function ReadLongWord(Endianness: TEndianness; var Value: LongWord): Boolean; overload;
    function ReadLongWord(Endianness: TEndianness): LongWord; overload; inline;
    function ReadDouble(Endianness: TEndianness): Double;
    {$IF Declared(ShortString)}
    function ReadShortString(var S: ShortString): Boolean; overload;
    function ReadShortString: ShortString; overload; inline;
    {$IFEND}
    function TryReadBuffer(var Buffer; Count: Integer): Boolean;
    function TryReadHeader(const Header; HeaderSize: Byte;
      AlwaysResetPosition: Boolean = False): Boolean;
    procedure WriteByte(Value: Byte); overload; inline;
    {$IF Declared(AnsiChar)}
    procedure WriteByte(Value: AnsiChar); overload; inline;
    {$ELSE}
    procedure WriteByte(Value: Char); overload;
    {$IFEND}
    procedure WriteWord(Value: Word; Endianness: TEndianness);
    procedure WriteSmallInt(Value: SmallInt; Endianness: TEndianness);
    procedure WriteLongInt(Value: LongInt; Endianness: TEndianness);
    procedure WriteLongWord(Value: LongWord; Endianness: TEndianness);
    procedure WriteDouble(Value: Double; Endianness: TEndianness);
    {$IF Declared(UTF8String)}
    procedure WriteUTF8Chars(const S: UTF8String); overload;
    {$IFEND}
    procedure WriteUTF8Chars(const S: TBytes); overload;
    procedure WriteUTF8Chars(const S: UnicodeString); overload;
    procedure WriteUTF8Chars(const S: UnicodeString; const Args: array of const); overload;
    procedure WriteWideChars(const S: UnicodeString; Endianness: TEndianness);
  end;

function SwapLongInt(const Value: LongInt): LongInt;
function SwapLongWord(const Value: LongWord): LongWord;
function SwapSingle(const Value: Single): Single;
function SwapDouble(const Value: Double): Double;

function UnicodeFormat(const S: UnicodeString; const Args: array of const): UnicodeString;
function UnicodeSameText(const S1, S2: UnicodeString): Boolean; inline;

implementation

uses RTLConsts, SysConst;

{$IFDEF WIN32}
function SwapLongInt(const Value: LongInt): LongInt;
asm
  BSWAP EAX
end;

function SwapLongWord(const Value: LongWord): LongWord;
asm
  BSWAP EAX
end;
{$ELSE}
function SwapLongInt(const Value: LongInt): LongInt;
begin
  LongRec(Result).Bytes[0] := LongRec(Value).Bytes[3];
  LongRec(Result).Bytes[1] := LongRec(Value).Bytes[2];
  LongRec(Result).Bytes[2] := LongRec(Value).Bytes[1];
  LongRec(Result).Bytes[3] := LongRec(Value).Bytes[0];
end;

function SwapLongWord(const Value: LongWord): LongWord;
begin
  LongRec(Result).Bytes[0] := LongRec(Value).Bytes[3];
  LongRec(Result).Bytes[1] := LongRec(Value).Bytes[2];
  LongRec(Result).Bytes[2] := LongRec(Value).Bytes[1];
  LongRec(Result).Bytes[3] := LongRec(Value).Bytes[0];
end;
{$ENDIF}

function SwapSingle(const Value: Single): Single;
begin
  LongRec(Result).Bytes[0] := LongRec(Value).Bytes[3];
  LongRec(Result).Bytes[1] := LongRec(Value).Bytes[2];
  LongRec(Result).Bytes[2] := LongRec(Value).Bytes[1];
  LongRec(Result).Bytes[3] := LongRec(Value).Bytes[0];
end;

function SwapDouble(const Value: Double): Double;
var
  I: Integer;
begin
  for I := SizeOf(Double) - 1 downto 0 do
    PByteArray(@Result)[I] := PByteArray(@Value)[SizeOf(Double) - 1 - I];
end;

function UnicodeFormat(const S: UnicodeString; const Args: array of const): UnicodeString;
begin
{$IFDEF UNICODE}
  Result := Format(S, Args);
{$ELSE}
  Result := WideFormat(S, Args);
{$ENDIF}
end;

function UnicodeSameText(const S1, S2: UnicodeString): Boolean; 
begin
{$IFDEF UNICODE}
  Result := SameText(S1, S2);
{$ELSE}
  Result := WideSameText(S1, S2);
{$ENDIF}
end;

{ TStreamHelper }

function TStreamHelper.ReadByte(var Value: Byte): Boolean;
begin
  Result := (Read(Value, 1) = 1);
end;

function TStreamHelper.ReadByte: Byte;
begin
  ReadBuffer(Result, 1);
end;

function TStreamHelper.ReadWord(Endianness: TEndianness; var Value: Word): Boolean;
begin
  Result := TryReadBuffer(Value, 2);
  if Result and (Endianness = BigEndian) then
    Value := Swap(Value)
end;

function TStreamHelper.ReadWord(Endianness: TEndianness): Word;
begin
  if not ReadWord(Endianness, Result) then
    raise EReadError.CreateRes(@SReadError);
end;

function TStreamHelper.ReadDouble(Endianness: TEndianness): Double;
begin
  ReadBuffer(Result, 8);
  if Endianness = BigEndian then
    Result := SwapDouble(Result);
end;

function TStreamHelper.ReadLongInt(Endianness: TEndianness; var Value: LongInt): Boolean;
begin
  Result := TryReadBuffer(Value, 4);
  if Result and (Endianness = BigEndian) then
    Value := SwapLongInt(Value);
end;

function TStreamHelper.ReadLongInt(Endianness: TEndianness): LongInt;
begin
  if not ReadLongInt(Endianness, Result) then
    raise EReadError.CreateRes(@SReadError);
end;

function TStreamHelper.ReadLongWord(Endianness: TEndianness;
  var Value: LongWord): Boolean;
begin
  Result := TryReadBuffer(Value, 4);
  if Result and (Endianness = BigEndian) then
    Value := SwapLongWord(Value);
end;

function TStreamHelper.ReadLongWord(Endianness: TEndianness): LongWord;
begin
  if not ReadLongWord(Endianness, Result) then
    raise EReadError.CreateRes(@SReadError);
end;

{$IF Declared(ShortString)}
function TStreamHelper.ReadShortString(var S: ShortString): Boolean;
begin
  Result := (Read(S[0], 1) = 1) and (Read(S[1], Byte(S[0])) = Byte(S[0]));
end;

function TStreamHelper.ReadShortString: ShortString;
begin
  if not ReadShortString(Result) then
    raise EReadError.CreateRes(@SReadError);
end;
{$IFEND}

function TStreamHelper.ReadSmallInt(Endianness: TEndianness;
  var Value: SmallInt): Boolean;
begin
  Result := TryReadBuffer(Value, 2);
  if Result and (Endianness = BigEndian) then
    Value := Swap(Value)
end;

function TStreamHelper.ReadSmallInt(Endianness: TEndianness): SmallInt;
begin
  if not ReadSmallInt(Endianness, Result) then
    raise EReadError.CreateRes(@SReadError);
end;

function TStreamHelper.TryReadBuffer(var Buffer; Count: Integer): Boolean;
var
  BytesRead: Integer;
begin
  BytesRead := Read(Buffer, Count);
  Result := (BytesRead = Count);
  if not Result then Seek(-BytesRead, soCurrent);
end;

function TStreamHelper.TryReadHeader(const Header; HeaderSize: Byte;
  AlwaysResetPosition: Boolean = False): Boolean;
var
  Buffer: array[Byte] of Byte;
  BytesRead: Integer;
begin
  BytesRead := Read(Buffer, HeaderSize);
  Result := (BytesRead = HeaderSize) and CompareMem(@Buffer, @Header, HeaderSize);
  if not Result or AlwaysResetPosition then Seek(-BytesRead, soCurrent);
end;

procedure TStreamHelper.WriteByte(Value: Byte);
begin
  WriteBuffer(Value, 1);
end;

{$IF Declared(AnsiChar)}
procedure TStreamHelper.WriteByte(Value: AnsiChar);
begin
  WriteBuffer(Value, 1);
end;
{$ELSE}
procedure TStreamHelper.WriteByte(Value: Char);
begin
  WriteBuffer(Value, 1);
end;
{$IFEND}

procedure TStreamHelper.WriteDouble(Value: Double; Endianness: TEndianness);
begin
  if Endianness = BigEndian then
    Value := SwapDouble(Value);
  WriteBuffer(Value, 8);
end;

procedure TStreamHelper.WriteWideChars(const S: UnicodeString; Endianness: TEndianness);
var
  Ch: WideChar;
begin
  if Endianness = SmallEndian then
    WriteBuffer(Pointer(S)^, Length(S) * 2)
  else
    for Ch in S do
    begin
      WriteBuffer(WordRec(Ch).Hi, 1);
      WriteBuffer(WordRec(Ch).Lo, 1);
    end;
end;

procedure TStreamHelper.WriteWord(Value: Word; Endianness: TEndianness);
begin
  if Endianness = SmallEndian then
    WriteBuffer(Value, 2)
  else
  begin
    WriteBuffer(WordRec(Value).Hi, 1);
    WriteBuffer(WordRec(Value).Lo, 1);
  end;
end;

procedure TStreamHelper.WriteLongWord(Value: LongWord; Endianness: TEndianness);
begin
  if Endianness = BigEndian then
    Value := SwapLongWord(Value);
  WriteBuffer(Value, 4);
end;

procedure TStreamHelper.WriteSmallInt(Value: SmallInt; Endianness: TEndianness);
begin
  if Endianness = SmallEndian then
    WriteBuffer(Value, 2)
  else
  begin
    WriteBuffer(WordRec(Value).Hi, 1);
    WriteBuffer(WordRec(Value).Lo, 1);
  end;
end;

procedure TStreamHelper.WriteLongInt(Value: LongInt; Endianness: TEndianness);
begin
  if Endianness = BigEndian then
    Value := SwapLongInt(Value);
  WriteBuffer(Value, 4);
end;

{$IF Declared(UTF8String)}
procedure TStreamHelper.WriteUTF8Chars(const S: UTF8String);
begin
  WriteBuffer(Pointer(S)^, Length(S));
end;
{$IFEND}

procedure TStreamHelper.WriteUTF8Chars(const S: TBytes);
begin
  WriteBuffer(Pointer(S)^, Length(S));
end;

procedure TStreamHelper.WriteUTF8Chars(const S: UnicodeString);
begin
  {$IF Declared(UTF8String)}
  WriteUTF8Chars(UTF8Encode(S));
  {$ELSE}
  WriteUTF8Chars(TEncoding.UTF8.GetBytes(S));
  {$IFEND}
end;

procedure TStreamHelper.WriteUTF8Chars(const S: UnicodeString;
  const Args: array of const);
begin
  WriteUTF8Chars(UnicodeFormat(S, Args));
end;

end.
