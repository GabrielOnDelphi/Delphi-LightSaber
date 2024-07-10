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


procedure WriteInteger (Identifier: string; i: Integer);
function  ReadInteger  (Identifier: string; DefaultVal: Integer= 0): Integer;

procedure WriteString  (Identifier, s: string);
function  ReadString   (Identifier: string; DefaultVal: string= ''): string;

procedure WriteBool    (Identifier: string; b: Boolean);
function  ReadBoolean  (Identifier: string; DefaultVal: Boolean= FALSE): Boolean;

procedure WriteDbl     (Identifier: string; d: Double);
function  ReadDbl      (Identifier: string; DefaultVal: Double= 0.0): Double;

procedure WriteDateEx  (Identifier: string; d: TDateTime);
function  ReadDateEx   (Identifier: string; DefaultVal: TDateTime): Double;

function  ReadFont     (CONST Identifier: string; Font: TFont): Boolean;          { Result: If the INI file does not contains informations about font then this function will  return FALSE and no modification will be done to the 'Font' object passed as parameter. }
procedure WriteFont    (CONST Identifier: string; Font: TFont);



IMPLEMENTATION

USES
   cbAppData, cbIniFile; //, ccINIFile;





procedure WriteInteger(Identifier: string; i: Integer);
VAR IniFile: TIniFileVcl;
begin
 IniFile:= TIniFileVcl.Create(AppData.AppName, AppData.IniFile);
 TRY
   IniFile.WriteInteger(AppData.AppName, Identifier, i)
 FINALLY
   FreeAndNil(IniFile);
 END;
end;

function ReadInteger(Identifier: string; DefaultVal: Integer): Integer;
VAR IniFile: TIniFileVcl;
begin
 IniFile:= TIniFileVcl.Create(AppData.AppName, AppData.IniFile);
 TRY
   Result:= IniFile.ReadInteger(AppData.AppName, Identifier, DefaultVal);
 FINALLY
   FreeAndNil(IniFile);
 END;
end;




{ TIniFile bug: Cannot put spaces at the beggining of a 'value'. The spaces will be trimmed }
procedure WriteString(Identifier, s: string);
VAR IniFile: TIniFileVcl;
begin
 IniFile:= TIniFileVcl.Create(AppData.AppName, AppData.IniFile);
 TRY
   IniFile.WriteString(AppData.AppName, Identifier, s)
 FINALLY
   FreeAndNil(IniFile);
 END;
end;

function ReadString(Identifier: string; DefaultVal: string): string;
VAR IniFile: TIniFileVcl;
begin
 IniFile:= TIniFileVcl.Create(AppData.AppName, AppData.IniFile);
 TRY
   Result:= IniFile.ReadString(AppData.AppName, Identifier, DefaultVal);
 FINALLY
   FreeAndNil(IniFile);
 END;
end;





procedure WriteBool(Identifier: string; b: Boolean);
VAR IniFile: TIniFileVcl;
begin
 IniFile:= TIniFileVcl.Create(AppData.AppName, AppData.IniFile);
 TRY
   IniFile.WriteBool(AppData.AppName, Identifier, b)
 FINALLY
   FreeAndNil(IniFile);
 END;
end;

function ReadBoolean(Identifier: string; DefaultVal: Boolean= FALSE): Boolean;
VAR IniFile: TIniFileVcl;
begin
 IniFile:= TIniFileVcl.Create(AppData.AppName, AppData.IniFile);
 TRY
   Result:= IniFile.ReadBool(AppData.AppName, Identifier, DefaultVal);
 FINALLY
   FreeAndNil(IniFile);
 END;
end;




procedure WriteDbl (Identifier: string; d: Double);
VAR IniFile: TIniFileVcl;
begin
 IniFile:= TIniFileVcl.Create(AppData.AppName, AppData.IniFile);
 TRY
   IniFile.Write(Identifier, d)
 FINALLY
   FreeAndNil(IniFile);
 END;
end;

function  ReadDbl (Identifier: string; DefaultVal: Double= 0.0): Double;
VAR IniFile: TIniFileVcl;
begin
 IniFile:= TIniFileVcl.Create(AppData.AppName, AppData.IniFile);
 TRY
   Result:= IniFile.Read(Identifier, DefaultVal);
 FINALLY
   FreeAndNil(IniFile);
 END;
end;





procedure WriteDateEx (Identifier: string; d: TDateTime);
VAR IniFile: TIniFileVcl;
begin
 IniFile:= TIniFileVcl.Create(AppData.AppName, AppData.IniFile);
 TRY
   IniFile.WriteDateEx(Identifier, d)
 FINALLY
   FreeAndNil(IniFile);
 END;
end;

function  ReadDateEx (Identifier: string; DefaultVal: TDateTime): Double;
VAR IniFile: TIniFileVcl;
begin
 IniFile:= TIniFileVcl.Create(AppData.AppName, AppData.IniFile);
 TRY
   Result:= IniFile.ReadDateEx(Identifier, DefaultVal);
 FINALLY
   FreeAndNil(IniFile);
 END;
end;




function ReadFont(CONST Identifier: string; Font: TFont): Boolean;
VAR IniFile: TIniFilevcl;
begin
 IniFile:= TIniFilevcl.Create(AppData.AppName, AppData.IniFile);
 TRY
   Result:= IniFile.Read(Identifier, Font);
 FINALLY
   FreeAndNil(IniFile);
 END;
end;

procedure WriteFont(CONST Identifier: string; Font: TFont);
VAR IniFile: TIniFileVcl;
begin
 IniFile:= TIniFileVcl.Create(AppData.AppName, AppData.IniFile);
 TRY
   IniFile.Write(Identifier, Font)
 FINALLY
   FreeAndNil(IniFile);
 END;
end;



end.
