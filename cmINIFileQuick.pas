UNIT cmINIFileQuick;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
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
   System.SysUtils, Vcl.Graphics, System.IniFiles;


procedure WriteInteger (CONST Identifier: string; i: Integer);
function  ReadInteger  (CONST Identifier: string; DefaultVal: Integer= 0): Integer;

procedure WriteString  (CONST Identifier, s: string);
function  ReadString   (CONST Identifier: string; DefaultVal: string= ''): string;

procedure WriteBool    (CONST Identifier: string; b: Boolean);
function  ReadBoolean  (CONST Identifier: string; DefaultVal: Boolean= FALSE): Boolean;

procedure WriteDbl     (CONST Identifier: string; d: Double);
function  ReadDbl      (CONST Identifier: string; DefaultVal: Double= 0.0): Double;

procedure WriteDateEx  (CONST Identifier: string; d: TDateTime);
function  ReadDateEx   (CONST Identifier: string; DefaultVal: TDateTime): Double;

function  ReadFont     (CONST Identifier: string; Font: TFont): Boolean;          { Result: If the INI file does not contains informations about font then this function will  return FALSE and no modification will be done to the 'Font' object passed as parameter. }
procedure WriteFont    (CONST Identifier: string; Font: TFont);



IMPLEMENTATION

USES
   cbAppData, cbINIFile;





procedure WriteInteger(CONST Identifier: string; i: Integer);
VAR IniFile: TIniFileApp;
begin
 IniFile:= TIniFileApp.Create(AppData.AppName, AppData.IniFile);
 TRY
   IniFile.WriteInteger(AppData.AppName, Identifier, i)
 FINALLY
   FreeAndNil(IniFile);
 END;
end;

function ReadInteger(CONST Identifier: string; DefaultVal: Integer): Integer;
VAR IniFile: TIniFileApp;
begin
 IniFile:= TIniFileApp.Create(AppData.AppName, AppData.IniFile);
 TRY
   Result:= IniFile.ReadInteger(AppData.AppName, Identifier, DefaultVal);
 FINALLY
   FreeAndNil(IniFile);
 END;
end;




{ TIniFile bug: Cannot put spaces at the beggining of a 'value'. The spaces will be trimmed }
procedure WriteString(CONST Identifier, s: string);
VAR IniFile: TIniFileApp;
begin
 IniFile:= TIniFileApp.Create(AppData.AppName, AppData.IniFile);
 TRY
   IniFile.WriteString(AppData.AppName, Identifier, s)
 FINALLY
   FreeAndNil(IniFile);
 END;
end;

function ReadString(CONST Identifier: string; DefaultVal: string): string;
VAR IniFile: TIniFileApp;
begin
 IniFile:= TIniFileApp.Create(AppData.AppName, AppData.IniFile);
 TRY
   Result:= IniFile.ReadString(AppData.AppName, Identifier, DefaultVal);
 FINALLY
   FreeAndNil(IniFile);
 END;
end;





procedure WriteBool(CONST Identifier: string; b: Boolean);
VAR IniFile: TIniFileApp;
begin
 IniFile:= TIniFileApp.Create(AppData.AppName, AppData.IniFile);
 TRY
   IniFile.WriteBool(AppData.AppName, Identifier, b)
 FINALLY
   FreeAndNil(IniFile);
 END;
end;

function ReadBoolean(CONST Identifier: string; DefaultVal: Boolean= FALSE): Boolean;
VAR IniFile: TIniFileApp;
begin
 IniFile:= TIniFileApp.Create(AppData.AppName, AppData.IniFile);
 TRY
   Result:= IniFile.ReadBool(AppData.AppName, Identifier, DefaultVal);
 FINALLY
   FreeAndNil(IniFile);
 END;
end;




procedure WriteDbl (CONST Identifier: string; d: Double);
VAR IniFile: TIniFileApp;
begin
 IniFile:= TIniFileApp.Create(AppData.AppName, AppData.IniFile);
 TRY
   IniFile.Write(Identifier, d)
 FINALLY
   FreeAndNil(IniFile);
 END;
end;

function  ReadDbl (CONST Identifier: string; DefaultVal: Double= 0.0): Double;
VAR IniFile: TIniFileApp;
begin
 IniFile:= TIniFileApp.Create(AppData.AppName, AppData.IniFile);
 TRY
   Result:= IniFile.Read(Identifier, DefaultVal);
 FINALLY
   FreeAndNil(IniFile);
 END;
end;





procedure WriteDateEx (CONST Identifier: string; d: TDateTime);
VAR IniFile: TIniFileApp;
begin
 IniFile:= TIniFileApp.Create(AppData.AppName, AppData.IniFile);
 TRY
   IniFile.WriteDateEx(Identifier, d)
 FINALLY
   FreeAndNil(IniFile);
 END;
end;

function  ReadDateEx (CONST Identifier: string; DefaultVal: TDateTime): Double;
VAR IniFile: TIniFileApp;
begin
 IniFile:= TIniFileApp.Create(AppData.AppName, AppData.IniFile);
 TRY
   Result:= IniFile.ReadDateEx(Identifier, DefaultVal);
 FINALLY
   FreeAndNil(IniFile);
 END;
end;




function ReadFont(CONST Identifier: string; Font: TFont): Boolean;
VAR IniFile: TIniFileApp;
begin
 IniFile:= TIniFileApp.Create(AppData.AppName, AppData.IniFile);
 TRY
   Result:= IniFile.Read(Identifier, Font);
 FINALLY
   FreeAndNil(IniFile);
 END;
end;

procedure WriteFont(CONST Identifier: string; Font: TFont);
VAR IniFile: TIniFileApp;
begin
 IniFile:= TIniFileApp.Create(AppData.AppName, AppData.IniFile);
 TRY
   IniFile.Write(Identifier, Font)
 FINALLY
   FreeAndNil(IniFile);
 END;
end;



end.
