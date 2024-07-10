UNIT ccINIFile;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

  Features:
     * Extends the capabilities of TIniFile.
     * We don't need to provide a section name (except for the constructor). All values are written to that single section.
     * Provides a lots of overloads where we don't need to provide a section name, in preparation for TIniFileVcl


  Reminder: TIniFile limitations:
     - Cannot put spaces at the beggining of a 'value'. The spaces will be trimmed. This behavior does not happen if you use a TMemInifile.      https://stackoverflow.com/questions/3702647/reading-inifile-to-stringlist-problem-spaces-problem
     - Cannot handle quotes and enters
     - Issues with Unicode strings
     Some of these limitations are not in TMemIniFile.
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.UITypes, System.IniFiles, System.IOUtils;

TYPE
  FontStruct = record
    Name  : string;
    Size  : Integer;
    Style : TFontStyle;
    Color : TColor;
  end;
  
TYPE
 TIniFileEx = class(TIniFile)        // Old name: TCubicIniFile
  protected
    FSection: string;
  public
    constructor Create (CONST SectionName, FileName: string);                    virtual;

    function  ValueExists(CONST Ident: string): Boolean;                         reintroduce; overload;

    { Data/Time }
    function  ReadDateEx (CONST Ident: string; Default: TDateTime): TDateTime;
    procedure WriteDateEx(CONST Ident: string;   Value: TDateTime);

    { OLD. BROKEN - Depends on user's FormatSettings }
    function  ReadDate_   (CONST Ident: string; Default: TDateTime): TDateTime;  deprecated 'Use ReadDateEx instead';  // http://docwiki.embarcadero.com/RADStudio/Sydney/en/Methods_(Delphi)
    procedure WriteDate_  (CONST Ident: string;   Value: TDateTime);             deprecated 'Use ReadDateEx instead';

    { String }
    function  Read       (CONST Ident: string; Default: string): string;         overload;
    procedure Write      (const Ident, Value: String);                           overload;

    { Integer }
    function  Read       (const Ident: string; Default: Integer= 0): Integer;    overload;
    procedure Write      (const Ident: string; Value: Integer);                  overload;

    { Bool }
    function  Read       (CONST Ident: string; Default: Boolean= TRUE): Boolean; overload;
    procedure Write      (const Ident: string; Value: Boolean);                  overload;

    function  Read       (CONST Ident: string): FontStruct;                      overload;
    procedure Write      (CONST Ident: string; Font: FontStruct);                overload;

    { Float }
    function  Read       (const Ident: string; Default: Double): Double;         overload;
    procedure Write      (const Ident: string; Value: Double);                   overload;
  end;



IMPLEMENTATION

USES
   ccIO;



{-------------------------------------------------------------------------------------------------------------
   MAIN
-------------------------------------------------------------------------------------------------------------}
constructor TIniFileEx.Create(CONST SectionName, FileName: string);
begin
 TDirectory.CreateDirectory(ExtractFilePath(FileName));  // Make sure the path exists otherwise we get an error.
 inherited Create(FileName);
 FSection:= SectionName;

 (*DEL
 TRY
   ForceDirectories(ExtractFilePath(Path));
 EXCEPT
   //todo 1: trap only specific exceptions
   Mesa jErrDetail ('Cannot create folder: '+ Path, 'TIniFileEx.Create');
 END; *)
end;




{---------------
   FONT
----------------}

//ToDo: Return value of function 'TIniFileEx.Read' might be undefined
function TIniFileEx.Read(CONST Ident: string): FontStruct;
begin
 if ValueExists(FSection, Ident+ 'Name') then
   begin
    Result.Name   := ReadString  (FSection,              Ident+ 'Name',    'Arial');
    Result.Color  := TColor      (ReadInteger (FSection, Ident+ 'Color',   0));
    Result.Size   := ReadInteger (FSection,              Ident+ 'Size',    8);
    Result.Style  := TFontStyle  (BYTE(ReadInteger (FSection, Ident+ 'Style',   0)) );
   end;
end;

procedure TIniFileEx.Write(CONST Ident: string; Font: FontStruct);
begin
  WriteString (FSection, Ident,  '');      // I need this here so I can find the font by its identifier (name). Otherwise it will be filtered out by TIniFileCubic.Read: if ValueExists(FSection, Comp.Name) then
  WriteString (FSection, Ident + 'Name',    Font.Name);
  WriteInteger(FSection, Ident + 'Color',   Font.Color);
  WriteInteger(FSection, Ident + 'Size',    Font.Size);
  WriteInteger(FSection, Ident + 'Style',   Byte(Font.Style));
end;



{---------------
   DATE
----------------}
function GetUniversalDateFormat: TFormatSettings;
begin
  Result:= TFormatSettings.Create;
  Result.DateSeparator:= '-';
  Result.TimeSeparator:= ':';
  Result.ShortDateFormat:= 'YYYY-MM-DD';          // The date is saved in the ShortDateFormat. We don't care about LongDateFormat here
end;


{ Deprecated!

  BUG: stackoverflow.com/questions/20419347/delphi-inifiles-readdatetime
  The date format will not depend anymore on user's regional settings
  This method cannot be simply named Write because it will conflict with Write(Real).
  I had a case where it wrote Write(2.0) to the ini file as a date (1900-01-01)
  The Default MUST be in the 0.0 format otherwise the wrong function will be called (the one for integer) }
procedure TIniFileEx.WriteDate_(CONST Ident: string; Value: TDateTime);
begin
  inherited WriteDate(FSection, Ident, Value);
end;

function TIniFileEx.ReadDate_(CONST Ident: string; Default: TDateTime): TDateTime;   { The Default MUST be in the 0.0 format otherwise the wrong function will be called (the one for integer) }
begin
  Result:= inherited ReadDate(FSection, Ident, default);
end;



procedure TIniFileEx.WriteDateEx(CONST Ident: string; Value: TDateTime);
begin
  WriteFloat(FSection, Ident, Value);
end;

function TIniFileEx.ReadDateEx(CONST Ident: string; Default: TDateTime): TDateTime;
begin
  Result:= TDateTime(ReadFloat(FSection, Ident, default));
end;






{----------------------
   DEFAULT OVERLOADS
-----------------------}

function TIniFileEx.ValueExists(const Ident: string): Boolean;
begin
 Result:= inherited ValueExists(FSection, Ident);
end;



function TIniFileEx.Read(const Ident: string; Default: Boolean= TRUE): Boolean;
begin
 Result:= inherited ReadBool(FSection, Ident, Default);
end;

procedure TIniFileEx.Write(const Ident: string; Value: Boolean);
begin
 inherited WriteBool(FSection, Ident, Value);
end;



function TIniFileEx.Read(CONST Ident: string; Default: Integer= 0): Integer;
begin
 Result:= inherited ReadInteger(FSection, Ident, Default);
end;

procedure TIniFileEx.Write(CONST Ident: string; Value: Integer);
begin
  inherited WriteInteger(FSection, Ident, Value);
end;




function TIniFileEx.Read(CONST Ident: string; Default: string): string;
begin
 Result:= inherited ReadString(FSection, Ident, Default);
end;

procedure TIniFileEx.Write(const Ident, Value: String);
begin
 inherited WriteString(FSection, Ident, Value);
end;



function TIniFileEx.Read(const Ident: string; Default: Double): Double;
begin
 Result:= inherited ReadFloat(FSection, Ident, Default);
end;

procedure TIniFileEx.Write(const Ident: string; Value: Double);
begin
 inherited WriteFloat(FSection, Ident, Value);
end;




end.

