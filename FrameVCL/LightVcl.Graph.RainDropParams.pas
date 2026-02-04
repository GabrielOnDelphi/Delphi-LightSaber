UNIT LightVcl.Graph.RainDropParams;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
  Parameters for TRainShelter animation

  Editor: LightSaber\FrameVCL\LightVcl.Graph.RainEditorForm.pas
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Classes, LightCore.StreamBuff, LightCore.INIFile;

TYPE
  TWaterDamping = 1..99;

  PRaindropParams= ^RRaindropParams;
  RRaindropParams= record
  private
  public
    TargetFPS       : Integer;
    WaveAplitude    : Integer;
    WaveTravelDist  : Integer;
    DropInterval    : Integer;
    Damping         : TWaterDamping;
    AdvancedMode    : Boolean;         { Remember if editor was in advanced mode }
    MouseDrops      : Boolean;         { Create drops at mouse cursor position }
    MouseDropInterv : Integer;         { Interval in ms between mouse drops }

    procedure Reset;

    procedure Load(IniFile: TIniFileEx);    overload;  // For BioniX
    procedure Save(IniFile: TIniFileEx);    overload;

    procedure Load(Stream: TLightStream);   overload;  // for RainDrop binary files
    procedure Save(Stream: TLightStream);   overload;
  end;


IMPLEMENTATION






procedure RRaindropParams.Reset;
begin
 TargetFPS       := 24;
 Damping         := 15;    { Default damping = 15 }
 WaveAplitude    := 1;
 WaveTravelDist  := 50;
 DropInterval    := 150;
 AdvancedMode    := FALSE;
 MouseDrops      := FALSE;
 MouseDropInterv := 100;   { Default 100ms between mouse drops }
end;


procedure RRaindropParams.Save(Stream: TLightStream);
begin
  Stream.WriteInteger(Damping);
  Stream.WriteInteger(TargetFPS);
  Stream.WriteInteger(WaveAplitude);
  Stream.WriteInteger(WaveTravelDist);
  Stream.WriteInteger(DropInterval);
  Stream.WriteBoolean(AdvancedMode);
  Stream.WriteBoolean(MouseDrops);
  Stream.WriteInteger(MouseDropInterv);
  Stream.WritePaddingValidation;
end;


procedure RRaindropParams.Load(Stream: TLightStream);
begin
  Damping        := Stream.ReadInteger;
  TargetFPS      := Stream.ReadInteger;
  WaveAplitude   := Stream.ReadInteger;
  WaveTravelDist := Stream.ReadInteger;
  DropInterval   := Stream.ReadInteger;
  AdvancedMode   := Stream.ReadBoolean;
  MouseDrops     := Stream.ReadBoolean;
  MouseDropInterv:= Stream.ReadInteger;
  Stream.ReadPaddingValidation;
end;



{---------------------------------------------------------------------------------------------------------------
   INI persistence
   Save/Load parameters to/from monitor INI file (like PhotoPile and other plugins)
---------------------------------------------------------------------------------------------------------------}
procedure RRaindropParams.Save(IniFile: TIniFileEx);
begin
 IniFile.Write('RainDropParams.TargetFPS'      , TargetFPS);
 IniFile.Write('RainDropParams.Damping'        , Damping);
 IniFile.Write('RainDropParams.WaveAplitude'   , WaveAplitude);
 IniFile.Write('RainDropParams.WaveTravelDist' , WaveTravelDist);
 IniFile.Write('RainDropParams.DropInterval'   , DropInterval);
 IniFile.Write('RainDropParams.AdvancedMode'   , AdvancedMode);
 IniFile.Write('RainDropParams.MouseDrops'     , MouseDrops);
 IniFile.Write('RainDropParams.MouseDropInterv', MouseDropInterv);
end;

// The TIniFileEx looks like this: 'C:\Users\trei\AppData\Roaming\BioniX Wallpaper Changer\BioniX Wallpaper Changer.ini', 'MONITOR\LEN61DB\{4d36e96e-e325-11ce-bfc1-08002be10318}\0005'
procedure RRaindropParams.Load(IniFile: TIniFileEx);
begin
   TargetFPS      := IniFile.Read('RainDropParams.TargetFPS'      , 24);
   Damping        := IniFile.Read('RainDropParams.Damping'        , 15);
   WaveAplitude   := IniFile.Read('RainDropParams.WaveAplitude'   , 1);
   WaveTravelDist := IniFile.Read('RainDropParams.WaveTravelDist' , 50);
   DropInterval   := IniFile.Read('RainDropParams.DropInterval'   , 150);
   AdvancedMode   := IniFile.Read('RainDropParams.AdvancedMode'   , FALSE);
   MouseDrops     := IniFile.Read('RainDropParams.MouseDrops'     , FALSE);
   MouseDropInterv:= IniFile.Read('RainDropParams.MouseDropInterv', 100);
end;



end.
