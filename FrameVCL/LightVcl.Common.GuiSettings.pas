UNIT LightVcl.Common.GuiSettings;

{=============================================================================================================
   2026.01.29
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Saves/loads GUI settings to a binary file using TLightStream.

   This is a TEMPLATE file intended to demonstrate the pattern for saving settings.
   Do not link to it directly! Instead, copy it to your project and customize the
   fields (bUser, iUser) to match your application's settings.

   Usage:
     1. Copy this file to your project
     2. Rename the class and add your settings fields
     3. Update Save/Load to write/read your fields
     4. Call Load on application startup, Save on shutdown

   File format uses a header with signature and version for forward compatibility.
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, LightCore.StreamBuff;

TYPE
  { Template class for storing GUI settings.
    Copy and modify this class for your specific application settings. }
  TGuiSettings = class(TObject)
  private
    CONST
      Signature: AnsiString = 'TGuiSettings';
      CurrentVersion = 2;
      SettingsFileName = 'GUISettings.bin';
  public
    { Example settings fields - replace with your own }
    bUser: Boolean;
    iUser: Integer;

    constructor Create;
    procedure Load;
    procedure Save;
    function GetSettingsPath: string;
  end;


IMPLEMENTATION

USES
   LightCore.AppData;


{---------------------------------------------------------------------------------------------------------------
   Constructor

   Initializes settings to default values.
   Override in your copy to set application-specific defaults.
---------------------------------------------------------------------------------------------------------------}
constructor TGuiSettings.Create;
begin
  inherited Create;
  { Set default values for all settings }
  bUser:= False;
  iUser:= 0;
end;


{---------------------------------------------------------------------------------------------------------------
   GetSettingsPath

   Returns the full path to the settings file.
---------------------------------------------------------------------------------------------------------------}
function TGuiSettings.GetSettingsPath: string;
begin
  Result:= AppDataCore.AppDataFolder(TRUE) + SettingsFileName;
end;


{---------------------------------------------------------------------------------------------------------------
   Save

   Saves current settings to binary file.
   Uses TLightStream with header for version compatibility.

   Raises:
     Exception if file cannot be created (e.g., folder is read-only)

   Note: When adding new fields, increment CurrentVersion and handle backward
   compatibility in Load.
---------------------------------------------------------------------------------------------------------------}
procedure TGuiSettings.Save;
VAR
  Stream: TLightStream;
  SettingsFile: string;
begin
  SettingsFile:= GetSettingsPath;

  Stream:= TLightStream.CreateWrite(SettingsFile);
  TRY
    Stream.WriteHeader(Signature, CurrentVersion);
    Stream.WriteBoolean(bUser);
    Stream.WriteInteger(iUser);
    Stream.WritePadding;
  FINALLY
    FreeAndNil(Stream);
  END;
end;


{---------------------------------------------------------------------------------------------------------------
   Load

   Loads settings from binary file.
   If file doesn't exist (first run), keeps default values.

   Raises:
     Exception if file exists but has invalid format or incompatible version

   Note: When reading older versions, you may need to provide default values
   for fields that didn't exist in that version.
---------------------------------------------------------------------------------------------------------------}
procedure TGuiSettings.Load;
VAR
  Stream: TLightStream;
  SettingsFile: string;
begin
  SettingsFile:= GetSettingsPath;

  { First run - no settings file yet, keep defaults }
  if NOT FileExists(SettingsFile) then EXIT;

  Stream:= TLightStream.CreateRead(SettingsFile);
  TRY
    if NOT Stream.ReadHeader(Signature, CurrentVersion)
    then raise Exception.Create('TGuiSettings.Load: Invalid or incompatible settings file: ' + SettingsFile);

    bUser:= Stream.ReadBoolean;
    iUser:= Stream.ReadInteger;
    Stream.ReadPadding;
  FINALLY
    FreeAndNil(Stream);
  END;
end;


end.
