UNIT uSoldier_v2;

{=============================================================================================================
   2025.03.16
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   This demonstrates how to save/load a simple object from a binary file, using versioning.
=============================================================================================================}

INTERFACE

USES System.SysUtils,
     LightCore.StreamBuff;

TYPE
  TSoldier2 = class(TObject)
  private
    LoadedVersion: Word;   // Detected version
    CONST CurVersion = 2;
    CONST ClassSignature: AnsiString= 'TSoldier2';
    procedure Load_v1(Stream: TLightStream);
    procedure Load_v2(Stream: TLightStream);
  public
    Life : Integer;
    Ammo : Integer;
    Speed: Integer;
    Name : string;
    Gun  : string;  // GunType

    procedure Load(Stream: TLightStream); virtual;
    procedure Save(Stream: TLightStream); virtual;

    function ShowVersion: string;
  end;


IMPLEMENTATION

procedure TSoldier2.Save(Stream: TLightStream);
begin
  Stream.WriteHeader(ClassSignature, CurVersion);  // Header & version number

  Stream.WriteInteger(Life);
  Stream.WriteInteger(Ammo);
  Stream.WriteString(Name);
  Stream.WriteInteger(Speed);
  Stream.WriteString(Gun);
end;


procedure TSoldier2.Load(Stream: TLightStream);
VAR Version: Word;
begin
  Version:= Stream.TryReadHeader(ClassSignature);   // Header & version number

  case Version of
    1: Load_v1(Stream);
    2: Load_v2(Stream);
  end;

end;


procedure TSoldier2.Load_v1(Stream: TLightStream);
begin
  Life := Stream.ReadInteger;
  Ammo := Stream.ReadInteger;
  Name := Stream.ReadString;

  Stream.ReadPadding;
  LoadedVersion:= 1;
end;


procedure TSoldier2.Load_v2(Stream: TLightStream);
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
