UNIT uSoldier_v1;

{=============================================================================================================
   2025.03.16
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   This demonstrates how to save/load a simple object from a binary file.
=============================================================================================================}

INTERFACE

USES System.SysUtils,
     LightCore.StreamBuff;

TYPE
  TSoldier = class(TObject)
  private
    CONST ClassSignature: AnsiString= 'TSoldier1';
  public
    Life : Integer;
    Ammo : Integer;
    Name : string;

    procedure Load(Stream: TLightStream); virtual;
    procedure Save(Stream: TLightStream); virtual;
  end;


IMPLEMENTATION


CONST CurVersion = 1;

procedure TSoldier.Save(Stream: TLightStream);
begin
  Stream.WriteHeader(ClassSignature, CurVersion);  // Header & version number

  Stream.WriteInteger(Life);
  Stream.WriteInteger(Ammo);
  Stream.WriteString(Name);

  Stream.WritePadding;
end;


procedure TSoldier.Load(Stream: TLightStream);
begin
  if NOT stream.TryReadHeader(ClassSignature, CurVersion) then EXIT;   // Header & version number

  Life   := Stream.ReadInteger;
  Ammo   := Stream.ReadInteger;
  Name   := Stream.ReadString;

  Stream.ReadPadding;
end;


end.
