UNIT LightVcl.Common.GuiSettings;

{=============================================================================================================
   2025.05
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Saves settings for TfrmSettings in FormSettings.pas
--------------------------------------------------------------------------------------------------------------
   This is a template file.
   Do not link to it! Instead copy it in your project!
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, LightCore.StreamBuff;

TYPE

  TGuiSettings = class(TObject)
  private
    CONST Signature: AnsiString= 'TGuiSettings';
  public
    bUser: Boolean;
    iUser: Integer;

    procedure Load;
    procedure Save;
  end;


IMPLEMENTATION

USES
   LightCore.AppData;


procedure TGuiSettings.Save;
begin
  VAR SettingsFile:= AppDataCore.AppDataFolder(TRUE)+ 'GUISettings.bin';

  VAR Stream:= TLightStream.CreateWrite(SettingsFile);    { This will give an AV if the file cannot be saved (folder readonly) }
  TRY
    Stream.WriteHeader(Signature, 2);
    Stream.WriteBoolean(bUser);
    Stream.WriteInteger(iUser);

    Stream.WritePadding;
  FINALLY
    FreeAndNil(Stream);
  END;
end;


procedure TGuiSettings.Load;
begin
  VAR SettingsFile:= AppDataCore.AppDataFolder(TRUE)+ 'GUISettings.bin';
  if NOT FileExists(SettingsFile) then EXIT;

  VAR Stream:= TLightStream.CreateRead(SettingsFile);
  TRY
    if NOT stream.TryReadHeader(Signature, 2) then EXIT;
    bUser  := Stream.ReadBoolean;
    iUser  := Stream.ReadInteger;

    Stream.ReadPadding;
  FINALLY
    FreeAndNil(Stream);
  END;
end;



end.
