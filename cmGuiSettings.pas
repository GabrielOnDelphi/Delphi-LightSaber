UNIT cmGuiSettings;

{-------------------------------------------------------------------------------------------------------------
   Saves settings for TfrmSettings in FormSettings.pas
   Author: GM 08.2024
--------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------}

{.$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

{This is a main template file. Do not link to it. Copy it in your project! }

INTERFACE

USES
  System.SysUtils, System.Classes, ccStreamBuff;

TYPE
  THintType = (htOff,           // Turn off the embeded help system
               htTooltips,      // Show help as tool-tips
               htStatBar);      // Show help in status bar

  TGuiSettings = class(TObject)
  private
    CONST Signature: AnsiString= 'TGuiSettings';
  public
    AutoStartUp  : Boolean;       // Start app at Windows startup
    StartMinim   : Boolean;       // Start minimized
    Minimize2Tray: Boolean;       // Minimize to tray
    HintType     : THintType;     // Turn off the embeded help system
    HideHint     : Integer;       // Hide hint after x ms.
    UserPath     : string;        // User defined path where to save files

    constructor Create;
    destructor Destroy; override;

    procedure Load;
    procedure Save;
  end;


IMPLEMENTATION

USES
   ccIO, cbAppData;

constructor TGuiSettings.Create;
begin
  inherited Create;
  AutoStartUp  := TRUE;
  StartMinim   := FALSE;
  Minimize2Tray:= TRUE;              // Minimize to tray
  HintType     := htTooltips;        // Turn off the embeded help system
  HideHint     := 4000;              // Hide hint after x ms.
  UserPath     := '';                // User defined path where to save files
end;


destructor TGuiSettings.Destroy;
begin
  inherited Destroy;
end;


procedure TGuiSettings.Save;
begin
  VAR SettingsFile:= AppData.AppDataFolder(TRUE)+ 'GuiSettings.bin';

  VAR Stream:= TCubicBuffStream.CreateWrite(SettingsFile);    { This will give an AV if the file cannot be saved (folder readonly) }
  TRY
    Stream.WriteHeader(Signature, 1);
    Stream.WriteBoolean(AutoStartUp);
    Stream.WriteBoolean(StartMinim);
    Stream.WriteBoolean(Minimize2Tray);
    Stream.WriteInteger(Ord(HintType));
    Stream.WriteInteger(HideHint);
    Stream.WriteStringU(UserPath);

    Stream.WritePaddingDef;
  FINALLY
    FreeAndNil(Stream);
  END;
end;


procedure TGuiSettings.Load;
begin
  VAR SettingsFile:= AppData.AppDataFolder(TRUE)+ 'GuiSettings.bin';
  if NOT FileExists(SettingsFile) then EXIT;

  { Save self }
  VAR Stream:= TCubicBuffStream.CreateRead(SettingsFile);
  TRY
    Stream.ReadHeader(Signature, 1);
 
    AutoStartUp  := Stream.ReadBoolean;
    StartMinim   := Stream.ReadBoolean;
    Minimize2Tray:= Stream.ReadBoolean;             // Minimize to tray
    HintType     := THintType(Stream.ReadInteger);  // Turn off the embeded help system
    HideHint     := Stream.ReadInteger;             // Hide hint after x ms.
    UserPath     := Stream.ReadStringU;             // User defined path where to save files

    Stream.ReadPaddingDef;
  FINALLY
    FreeAndNil(Stream);
  END;
end;



end.
