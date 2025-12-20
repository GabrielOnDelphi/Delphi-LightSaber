UNIT LightCore.INIFileQuick;

{=============================================================================================================
   2025.12
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Stand-alone functions to QUICKLY save/load data to INI file.
   No need to manually create the TIniFile object anymore.

   Warning:
      Each time we use one of these function, it opens the INI file.
      This could be slow. Don't abuse it!

   Section name
      The section name is automatically retrieve from 'AppData.AppName'.
      The Identifier MUST be unique in the entire section!

   File path
      The file is saved in application's folder (%AppData%).
=============================================================================================================}

INTERFACE

USES
   System.SysUtils;


procedure WriteInteger (CONST Identifier: string; i: Integer);
function  ReadInteger  (CONST Identifier: string; DefaultVal: Integer= 0): Integer;

procedure WriteString  (CONST Identifier, s: string);
function  ReadString   (CONST Identifier: string; DefaultVal: string= ''): string;

procedure WriteBool    (CONST Identifier: string; b: Boolean);
function  ReadBoolean  (CONST Identifier: string; DefaultVal: Boolean= FALSE): Boolean;

procedure WriteDbl     (CONST Identifier: string; d: Double);
function  ReadDbl      (CONST Identifier: string; DefaultVal: Double= 0.0): Double;

procedure WriteDate    (CONST Identifier: string; d: TDateTime);
function  ReadDate     (CONST Identifier: string; DefaultVal: TDateTime): Double;



IMPLEMENTATION

USES
   LightCore.AppData, LightCore.INIFile;




procedure WriteInteger(CONST Identifier: string; i: Integer);
VAR IniFile: TIniFileEx;
begin
  IniFile:= TIniFileEx.Create(AppDataCore.AppName, AppDataCore.IniFile);
  TRY
    IniFile.WriteInteger(AppDataCore.AppName, Identifier, i)
  FINALLY
    FreeAndNil(IniFile);
  END;
end;

function ReadInteger(CONST Identifier: string; DefaultVal: Integer): Integer;
VAR IniFile: TIniFileEx;
begin
  IniFile:= TIniFileEx.Create(AppDataCore.AppName, AppDataCore.IniFile);
  TRY
    Result:= IniFile.ReadInteger(AppDataCore.AppName, Identifier, DefaultVal);
  FINALLY
    FreeAndNil(IniFile);
  END;
end;




{ TIniFile bug: Cannot put spaces at the beggining of a 'value'. The spaces will be trimmed }
procedure WriteString(CONST Identifier, s: string);
VAR IniFile: TIniFileEx;
begin
  IniFile:= TIniFileEx.Create(AppDataCore.AppName, AppDataCore.IniFile);
  TRY
    IniFile.WriteString(AppDataCore.AppName, Identifier, s)
  FINALLY
    FreeAndNil(IniFile);
  END;
end;

function ReadString(CONST Identifier: string; DefaultVal: string): string;
VAR IniFile: TIniFileEx;
begin
  IniFile:= TIniFileEx.Create(AppDataCore.AppName, AppDataCore.IniFile);
  TRY
    Result:= IniFile.ReadString(AppDataCore.AppName, Identifier, DefaultVal);
  FINALLY
    FreeAndNil(IniFile);
  END;
end;




procedure WriteBool(CONST Identifier: string; b: Boolean);
VAR IniFile: TIniFileEx;
begin
  IniFile:= TIniFileEx.Create(AppDataCore.AppName, AppDataCore.IniFile);
  TRY
    IniFile.WriteBool(AppDataCore.AppName, Identifier, b)
  FINALLY
    FreeAndNil(IniFile);
  END;
end;

function ReadBoolean(CONST Identifier: string; DefaultVal: Boolean= FALSE): Boolean;
VAR IniFile: TIniFileEx;
begin
  IniFile:= TIniFileEx.Create(AppDataCore.AppName, AppDataCore.IniFile);
  TRY
    Result:= IniFile.ReadBool(AppDataCore.AppName, Identifier, DefaultVal);
  FINALLY
    FreeAndNil(IniFile);
  END;
end;




procedure WriteDbl (CONST Identifier: string; d: Double);
VAR IniFile: TIniFileEx;
begin
  IniFile:= TIniFileEx.Create(AppDataCore.AppName, AppDataCore.IniFile);
  TRY
    IniFile.Write(Identifier, d)
  FINALLY
    FreeAndNil(IniFile);
  END;
end;

function  ReadDbl (CONST Identifier: string; DefaultVal: Double= 0.0): Double;
VAR IniFile: TIniFileEx;
begin
  IniFile:= TIniFileEx.Create(AppDataCore.AppName, AppDataCore.IniFile);
  TRY
    Result:= IniFile.Read(Identifier, DefaultVal);
  FINALLY
    FreeAndNil(IniFile);
  END;
end;





procedure WriteDate (CONST Identifier: string; d: TDateTime);
VAR IniFile: TIniFileEx;
begin
  IniFile:= TIniFileEx.Create(AppDataCore.AppName, AppDataCore.IniFile);
  TRY
    IniFile.WriteDate(Identifier, d)
  FINALLY
    FreeAndNil(IniFile);
  END;
end;

function  ReadDate (CONST Identifier: string; DefaultVal: TDateTime): Double;
VAR IniFile: TIniFileEx;
begin
  IniFile:= TIniFileEx.Create(AppDataCore.AppName, AppDataCore.IniFile);
  TRY
    Result:= IniFile.ReadDate(Identifier, DefaultVal);
  FINALLY
    FreeAndNil(IniFile);
  END;
end;


end.
