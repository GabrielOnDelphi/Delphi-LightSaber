UNIT uSoldier_v1;

{=============================================================================================================
   2025.03.16
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   This demonstrates how to save/load a simple object from a binary file.
=============================================================================================================}

INTERFACE

USES System.SysUtils, System.Generics.Collections,
     LightCore.StreamBuff, LightCore.StreamBuff2;

TYPE
  TSoldier = class(TObject)
  private
    CONST StreamSignature: AnsiString= 'TSoldier';
  public
    Life : Integer;
    Ammo : Integer;
    Name : string;

    procedure Load(Stream: TCubicBuffStream2); virtual;
    procedure Save(Stream: TCubicBuffStream2); virtual;
  end;


IMPLEMENTATION


procedure TSoldier.Save(Stream: TCubicBuffStream2);
begin
  Stream.WriteHeader(StreamSignature, 1);  // Header & version number

  Stream.WriteInteger(Life);
  Stream.WriteInteger(Ammo);
  Stream.WriteString(Name);

  Stream.WritePaddingDef;
end;


procedure TSoldier.Load(Stream: TCubicBuffStream2);
VAR Version: Word;
begin
  if NOT Stream.ReadHeaderVersion(StreamSignature, Version) then EXIT;   // Header & version number

  Life   := Stream.ReadInteger;
  Ammo   := Stream.ReadInteger;
  Name   := Stream.ReadString;

  Stream.ReadPaddingDef;
end;


end.
