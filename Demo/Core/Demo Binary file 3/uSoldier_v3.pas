UNIT uSoldier_v3;

{=============================================================================================================
   2025.03.16
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   This demonstrates how to save/load a simple object from a binary file, using versioning.
=============================================================================================================}

INTERFACE

USES System.SysUtils, System.Contnrs, System.Classes,
     LightCore.StreamBuff, LightCore.StreamBuff2;

TYPE
  TGun = class
    Name: string;
    Ammo: Integer;
    CONST StreamSignature: AnsiString= 'TGun';
    procedure Load(Stream: TCubicBuffStream2);
    procedure Save(Stream: TCubicBuffStream2);
  end;

  TGuns = class(TObjectList)   // OwnsObjects is True by default
    ActiveGun: Integer;
    CONST StreamSignature: AnsiString= 'TGuns';
    function ActiveGunName: string;
    procedure Load(Stream: TCubicBuffStream2);
    procedure Save(Stream: TCubicBuffStream2);
  end;


TYPE
  TSoldier3 = class(TObject)
  private
    LoadedVersion: Word;
    procedure Load_v3(Stream: TCubicBuffStream2);
    CONST StreamSignature: AnsiString= 'TSoldier';
    procedure Load_v1(Stream: TCubicBuffStream2);
    procedure Load_v2(Stream: TCubicBuffStream2);
  public
    Life : Integer;
    Speed: Integer;
    Name : string;
    Guns : TGuns;  // List of guns

    constructor Create;
    destructor Destroy; override;

    procedure Load(Stream: TCubicBuffStream2); virtual;
    procedure Save(Stream: TCubicBuffStream2); virtual;

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



procedure TSoldier3.Save(Stream: TCubicBuffStream2);
begin
  Stream.WriteHeader(StreamSignature, 3);  // Header & version number

  Stream.WriteInteger(Life);
  Stream.WriteString(Name);
  Stream.WriteInteger(Speed);
  Guns.Save(Stream);
end;


procedure TSoldier3.Load(Stream: TCubicBuffStream2);
begin
  if NOT Stream.ReadHeaderVersion(StreamSignature, LoadedVersion) then EXIT;   // Header & version number

  case LoadedVersion of
    1: Load_v1(Stream);
    2: Load_v2(Stream);
    3: Load_v3(Stream);
  end;
end;


procedure TSoldier3.Load_v1(Stream: TCubicBuffStream2);
VAR Ammo: Integer;
begin
  Life := Stream.ReadInteger;
  Ammo := Stream.ReadInteger;
  Name := Stream.ReadString;

  Stream.ReadPaddingDef;
end;


procedure TSoldier3.Load_v2(Stream: TCubicBuffStream2);
VAR
   Ammo: Integer;
   Gun : string;
begin
  Life  := Stream.ReadInteger;
  Ammo  := Stream.ReadInteger;
  Name  := Stream.ReadString;
  Speed := Stream.ReadInteger;
  Gun   := Stream.ReadString;
  //todo: Gun   :=  =FindInList;
end;


procedure TSoldier3.Load_v3(Stream: TCubicBuffStream2);
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
CONST GunVersion = 1;

procedure TGun.Save(Stream: TCubicBuffStream2);
begin
  Stream.WriteHeader(StreamSignature, GunVersion);  // Header & version number

  Stream.WriteInteger(Ammo);
  Stream.WriteString(Name);
end;

procedure TGun.Load(Stream: TCubicBuffStream2);
begin
  Stream.ReadHeader(StreamSignature, GunVersion);

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

procedure TGuns.Save(Stream: TCubicBuffStream2);
begin
  Stream.WriteHeader(StreamSignature, 1);  // Header & version number

  Stream.WriteInteger(ActiveGun);
  Stream.WriteInteger(Count);
  for VAR i:= 0 to Count-1 DO
    (Self[i] as TGun).Save(Stream);
end;


procedure TGuns.Load(Stream: TCubicBuffStream2);
VAR
   iCount: Integer;
   Gun: TGun;
begin
  Stream.ReadHeader(StreamSignature, 1);

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
