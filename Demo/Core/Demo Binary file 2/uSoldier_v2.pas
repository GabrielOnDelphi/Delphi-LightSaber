UNIT uSoldier_v2;

{=============================================================================================================
   2025.03.16
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   This demonstrates how to save/load a simple object from a binary file, using versioning.
=============================================================================================================}

INTERFACE

USES System.SysUtils, System.Generics.Collections,
     LightCore.StreamBuff, LightCore.StreamBuff2;

TYPE
  TSoldier2 = class(TObject)
  private
    LoadedVersion: Word;
    CONST StreamSignature: AnsiString= 'TSoldier';
    procedure Load_v1(Stream: TCubicBuffStream2);
    procedure Load_v2(Stream: TCubicBuffStream2);
  public
    Life : Integer;
    Ammo : Integer;
    Speed: Integer;
    Name : string;
    Gun  : string;  // GunType

    procedure Load(Stream: TCubicBuffStream2); virtual;
    procedure Save(Stream: TCubicBuffStream2); virtual;

    function ShowVersion: string;
  end;


IMPLEMENTATION


procedure TSoldier2.Save(Stream: TCubicBuffStream2);
begin
  Stream.WriteHeader(StreamSignature, 2);  // Header & version number

  Stream.WriteInteger(Life);
  Stream.WriteInteger(Ammo);
  Stream.WriteString(Name);
  Stream.WriteInteger(Speed);
  Stream.WriteString(Gun);
end;


procedure TSoldier2.Load(Stream: TCubicBuffStream2);
VAR Version: Word;
begin
  if NOT Stream.ReadHeaderVersion(StreamSignature, Version) then EXIT;   // Header & version number

  case Version of
    1: Load_v1(Stream);
    2: Load_v2(Stream);
  end;

end;


procedure TSoldier2.Load_v1(Stream: TCubicBuffStream2);
begin
  Life := Stream.ReadInteger;
  Ammo := Stream.ReadInteger;
  Name := Stream.ReadString;

  Stream.ReadPaddingDef;
  LoadedVersion:= 1;
end;


procedure TSoldier2.Load_v2(Stream: TCubicBuffStream2);
begin
  Life  := Stream.ReadInteger;
  Ammo  := Stream.ReadInteger;
  Name  := Stream.ReadString;
  Speed := Stream.ReadInteger;
  Gun   := Stream.ReadString;

  LoadedVersion:= 2;
end;


function TSoldier2.ShowVersion: string;
begin
  Result:= 'Loaded version is '+ IntToStr(LoadedVersion);
end;

end.
