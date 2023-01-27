UNIT cmINIFileQuick;

{=============================================================================================================
   2023.01
   See Copyright.txt
==============================================================================================================

   Stand-alone functions to QUICKLY save/load data to INI file.
   No need to manually create the TIniFile object anymore.
   Each time we use one of these function, it opens the INI file. This could be slow. Don't abuse it.

   The Identifier MUST be unique in the entire section!
   The section is the 'AppData.AppName' constant.
   The file is saved in application's (AppData) folder.

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
   ccAppData, ccINIFile;





procedure WriteInteger(Identifier: string; i: Integer);
VAR IniFile: TCubicIniFile;
begin
 IniFile:= TCubicIniFile.Create(AppData.AppName);
 TRY
   IniFile.WriteInteger(AppData.AppName, Identifier, i)
 FINALLY
   FreeAndNil(IniFile);
 END;
end;

function ReadInteger(Identifier: string; DefaultVal: Integer): Integer;
VAR IniFile: TCubicIniFile;
begin
 IniFile:= TCubicIniFile.Create(AppData.AppName);
 TRY
   Result:= IniFile.ReadInteger(AppData.AppName, Identifier, DefaultVal);
 FINALLY
   FreeAndNil(IniFile);
 END;
end;




{ TIniFile bug: Cannot put spaces at the beggining of a 'value'. The spaces will be trimmed }
procedure WriteString(Identifier, s: string);
VAR IniFile: TCubicIniFile;
begin
 IniFile:= TCubicIniFile.Create(AppData.AppName);
 TRY
   IniFile.WriteString(AppData.AppName, Identifier, s)
 FINALLY
   FreeAndNil(IniFile);
 END;
end;

function ReadString(Identifier: string; DefaultVal: string): string;
VAR IniFile: TCubicIniFile;
begin
 IniFile:= TCubicIniFile.Create(AppData.AppName);
 TRY
   Result:= IniFile.ReadString(AppData.AppName, Identifier, DefaultVal);
 FINALLY
   FreeAndNil(IniFile);
 END;
end;





procedure WriteBool(Identifier: string; b: Boolean);
VAR IniFile: TCubicIniFile;
begin
 IniFile:= TCubicIniFile.Create(AppData.AppName);
 TRY
   IniFile.WriteBool(AppData.AppName, Identifier, b)
 FINALLY
   FreeAndNil(IniFile);
 END;
end;

function ReadBoolean(Identifier: string; DefaultVal: Boolean= FALSE): Boolean;
VAR IniFile: TCubicIniFile;
begin
 IniFile:= TCubicIniFile.Create(AppData.AppName);
 TRY
   Result:= IniFile.ReadBool(AppData.AppName, Identifier, DefaultVal);
 FINALLY
   FreeAndNil(IniFile);
 END;
end;




procedure WriteDbl (Identifier: string; d: Double);
VAR IniFile: TCubicIniFile;
begin
 IniFile:= TCubicIniFile.Create(AppData.AppName);
 TRY
   IniFile.Write(Identifier, d)
 FINALLY
   FreeAndNil(IniFile);
 END;
end;

function  ReadDbl (Identifier: string; DefaultVal: Double= 0.0): Double;
VAR IniFile: TCubicIniFile;
begin
 IniFile:= TCubicIniFile.Create(AppData.AppName);
 TRY
   Result:= IniFile.Read(Identifier, DefaultVal);
 FINALLY
   FreeAndNil(IniFile);
 END;
end;





procedure WriteDateEx (Identifier: string; d: TDateTime);
VAR IniFile: TCubicIniFile;
begin
 IniFile:= TCubicIniFile.Create(AppData.AppName);
 TRY
   IniFile.WriteDateEx(Identifier, d)
 FINALLY
   FreeAndNil(IniFile);
 END;
end;

function  ReadDateEx (Identifier: string; DefaultVal: TDateTime): Double;
VAR IniFile: TCubicIniFile;
begin
 IniFile:= TCubicIniFile.Create(AppData.AppName);
 TRY
   Result:= IniFile.ReadDateEx(Identifier, DefaultVal);
 FINALLY
   FreeAndNil(IniFile);
 END;
end;




function ReadFont(CONST Identifier: string; Font: TFont): Boolean;
VAR IniFile: TCubicIniFile;
begin
 IniFile:= TCubicIniFile.Create(AppData.AppName);
 TRY
   Result:= IniFile.Read(Identifier, Font);
 FINALLY
   FreeAndNil(IniFile);
 END;
end;

procedure WriteFont(CONST Identifier: string; Font: TFont);
VAR IniFile: TCubicIniFile;
begin
 IniFile:= TCubicIniFile.Create(AppData.AppName);
 TRY
   IniFile.Write(Identifier, Font)
 FINALLY
   FreeAndNil(IniFile);
 END;
end;



end.
