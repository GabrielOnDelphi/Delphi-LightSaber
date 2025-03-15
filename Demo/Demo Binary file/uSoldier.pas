UNIT uSoldier;

{-------------------------------------------------------------------------------------------------------------
   Name: 
   Author: GM 08.2024
--------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES System.SysUtils, System.Generics.Collections,
     ccStreamBuff, ccStreamBuff2;

TYPE
  TSoldier = class(TObject)
  private
    CONST StreamSignature: AnsiString= 'TSoldier';
  public
    Life : Integer;
    Ammo : Integer;
    Speed: Integer;
    Name : string;
    Gun  : string;

    procedure Load(Stream: TCubicBuffStream2); virtual;
    procedure Save(Stream: TCubicBuffStream2); virtual;
  end;


IMPLEMENTATION


procedure TSoldier.Save(Stream: TCubicBuffStream2);
begin
  Stream.WriteHeader(StreamSignature, 2);  // Header & version number

  Stream.WriteInteger(Life);
  Stream.WriteInteger(Ammo);
  Stream.WriteString(Name);
  Stream.WriteString(Gun);
  //Stream.WriteInteger(Speed);

  Stream.WritePadding(94);
end;


procedure TSoldier.Load(Stream: TCubicBuffStream2);
VAR Version: Word;
begin
  if NOT Stream.ReadHeaderVersion(StreamSignature, Version) then EXIT;   // Header & version number

  Life   := Stream.ReadInteger;
  Ammo   := Stream.ReadInteger;
  Name   := Stream.ReadString;

  if Version = 2
  then
    begin
      Gun:= Stream.ReadString;
      Stream.ReadPadding(94);
    end
  else
    Stream.ReadPadding(100);
  //Speed  := Stream.ReadInteger;
end;


end.
