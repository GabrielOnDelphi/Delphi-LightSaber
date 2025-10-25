UNIT uSoldier_v3;

{=============================================================================================================
   2025.09
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   This demonstrates how to save/load a simple object from a binary file, with versioning (posibility to add more data).
=============================================================================================================}

INTERFACE

USES System.SysUtils, System.Contnrs, System.Classes,
     LightCore.StreamBuff;

TYPE
  TGun = class
    Name: string;
    Ammo: Integer;
    CONST ClassSignature: AnsiString= 'TGun';
    CONST GunVersion = 1;
    procedure Load(Stream: TLightStream);
    procedure Save(Stream: TLightStream);
  end;

  TGuns = class(TObjectList)   // OwnsObjects is True by default
    ActiveGun: Integer;
    CONST ClassSignature: AnsiString= 'TGuns';
    function ActiveGunName: string;
    procedure Load(Stream: TLightStream);
    procedure Save(Stream: TLightStream);
  end;


TYPE
  TSoldier3 = class(TObject)
  private
    LoadedVersion: Word;
    CONST CurVersion = 3;
    CONST ClassSignature: AnsiString= 'TSoldier3';
    procedure Load_v3(Stream: TLightStream);
    procedure Load_v1(Stream: TLightStream);
    procedure Load_v2(Stream: TLightStream);
  public
    Life : Integer;
    Speed: Integer;
    Ammo : Integer;
    Name : string;
    Guns : TGuns;  // List of guns

    constructor Create;
    destructor Destroy; override;

    procedure Load(Stream: TLightStream); virtual;
    procedure Save(Stream: TLightStream); virtual;

    function ShowVersion: string;
  end;


IMPLEMENTATION


constructor TSoldier3.Create;
begin
  inherited;
  Guns:= TGuns.Create(TRUE);
end;

destructor TSoldier3.Destroy;
begin
  Guns.Free;
  inherited;
end;



procedure TSoldier3.Save(Stream: TLightStream);
begin
  Stream.WriteHeader(ClassSignature, CurVersion);  // Header & version number

  Stream.WriteInteger(Life);
  Stream.WriteString(Name);
  Stream.WriteInteger(Speed);
  Guns.Save(Stream);
end;


procedure TSoldier3.Load(Stream: TLightStream);
begin
  LoadedVersion:= Stream.ReadHeader(ClassSignature);
  if LoadedVersion = 0 then EXIT;   // -1 if reading the header failed.

  case LoadedVersion of
    1: Load_v1(Stream);
    2: Load_v2(Stream);
    3: Load_v3(Stream);
  end;
end;


procedure TSoldier3.Load_v1(Stream: TLightStream);
begin
  Life := Stream.ReadInteger;
  Ammo := Stream.ReadInteger;
  Name := Stream.ReadString;

  Stream.ReadPadding;
end;


procedure TSoldier3.Load_v2(Stream: TLightStream);
VAR
   Gun : string;
begin
  Life  := Stream.ReadInteger;
  Ammo  := Stream.ReadInteger;
  Name  := Stream.ReadString;
  Speed := Stream.ReadInteger;
  Gun   := Stream.ReadString;
  //todo: Gun:= FindInList;
end;


procedure TSoldier3.Load_v3(Stream: TLightStream);
begin
  Life  := Stream.ReadInteger;
  Name  := Stream.ReadString;
  Speed := Stream.ReadInteger;
  Guns.Load(Stream);
end;



function TSoldier3.ShowVersion: string;
begin
  Result:= 'Loaded version is '+ IntToStr(LoadedVersion);
end;






{ TGun }
procedure TGun.Save(Stream: TLightStream);
begin
  Stream.WriteHeader(ClassSignature, GunVersion);  // Header & version number

  Stream.WriteInteger(Ammo);
  Stream.WriteString(Name);
end;


procedure TGun.Load(Stream: TLightStream);
begin
  Stream.ReadHeader(ClassSignature, GunVersion);

  Ammo := Stream.ReadInteger;
  Name := Stream.ReadString;
end;






{ TGuns }

function TGuns.ActiveGunName: string;
begin
  if Count = 0
  then Result:= 'No gun loaded'
  else Result:= (Self[ActiveGun] as TGun).Name;
end;


procedure TGuns.Save(Stream: TLightStream);
begin
  Stream.WriteHeader(ClassSignature, 1);  // Header & version number

  Stream.WriteInteger(ActiveGun);
  Stream.WriteInteger(Count);
  for VAR i:= 0 to Count-1 DO
    (Self[i] as TGun).Save(Stream);
end;


procedure TGuns.Load(Stream: TLightStream);
VAR
   Gun: TGun;
   iCount: Integer;
begin
  Stream.ReadHeader(ClassSignature, 1);

  ActiveGun:= Stream.ReadInteger;
  iCount   := Stream.ReadInteger;

  for VAR i:= 0 to iCount-1 DO
    begin
      Gun:= TGun.Create;
      Gun.Load(Stream);
      Self.Add(Gun);
    end;
end;


end.
