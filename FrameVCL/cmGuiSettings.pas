UNIT cmGuiSettings;

{-------------------------------------------------------------------------------------------------------------
   Saves settings for TfrmSettings in FormSettings.pas
   Author: GM 08.2024

--------------------------------------------------------------------------------------------------------------

   This is a template file.
   Do not link to it! Instead copy it in your project!
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, ccStreamBuff2;

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
   ccAppData, cbAppDataVCL
;


procedure TGuiSettings.Save;
begin
  VAR SettingsFile:= AppData.AppDataFolder(TRUE)+ 'GUISettings.bin';

  VAR Stream:= TCubicBuffStream2.CreateWrite(SettingsFile);    { This will give an AV if the file cannot be saved (folder readonly) }
  TRY
    Stream.WriteHeader(Signature, 1);
    Stream.WriteBoolean(bUser);
    Stream.WriteInteger(iUser);

    Stream.WritePaddingDef;
  FINALLY
    FreeAndNil(Stream);
  END;
end;


procedure TGuiSettings.Load;
begin
  VAR SettingsFile:= AppData.AppDataFolder(TRUE)+ 'GUISettings.bin';
  if NOT FileExists(SettingsFile) then EXIT;

  VAR Stream:= TCubicBuffStream2.CreateRead(SettingsFile);
  TRY
    Stream.ReadHeader(Signature, 1);
 
    bUser  := Stream.ReadBoolean;
    iUser  := Stream.ReadInteger;

    Stream.ReadPaddingDef;
  FINALLY
    FreeAndNil(Stream);
  END;
end;



end.
