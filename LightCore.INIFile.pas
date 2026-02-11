UNIT LightCore.INIFile;

{=============================================================================================================
   2026.01.29
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

  Features:
     * Extends the capabilities of TIniFile.
     * We don't need to provide a section name (except for the constructor). All values are written to that single section.
     * Provides a lots of overloads where we don't need to provide a section name, in preparation for TIniFileApp


  Reminder: TIniFile limitations:
     - Cannot put spaces at the beginning of a 'value'. The spaces will be trimmed. This behavior does not happen if you use a TMemInifile.      https://stackoverflow.com/questions/3702647/reading-inifile-to-stringlist-problem-spaces-problem
     - Cannot handle quotes and enters
     - Issues with Unicode strings
     Some of these limitations are not in TMemIniFile.
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.UITypes, System.IniFiles, System.IOUtils; //, LightCore, LightCore.Time;

TYPE
  FontStruct = record
    Name  : string;
    Size  : Integer;
    Style : TFontStyles;   { Set of TFontStyle - can hold multiple styles like [fsBold, fsItalic] }
    Color : TColor;
  end;

TYPE
{
  BUG: stackoverflow.com/questions/20419347/delphi-inifiles-readdatetime
  The date format will not depend anymore on user's regional settings
  This method cannot be simply named Write because it will conflict with Write(Real).
  I had a case where it wrote Write(2.0) to the ini file as a date (1900-01-01)
  The Default MUST be in the 0.0 format otherwise the wrong function will be called (the one for integer) }
 { Extends TIniFile to handle dates using float format (independent of regional settings).
   See: stackoverflow.com/questions/20419347/delphi-inifiles-readdatetime }
 TIniFileDt = class(TIniFile)
  public
    function  ReadDate (CONST Section, Ident: string; Default: TDateTime): TDateTime; reintroduce;
    procedure WriteDate(CONST Section, Ident: string; Value  : TDateTime);            reintroduce;
  end;


 TIniFileEx = class(TIniFile)        // Old name: TCubicIniFile
  protected
    FSection: string;
  public
    constructor Create (CONST SectionName, FileName: string);                    virtual;

    function  ValueExists(CONST Ident: string): Boolean;                         reintroduce; overload;

    { Data/Time }
    function  ReadDate   (CONST Ident: string; Default: TDateTime): TDateTime;   reintroduce; overload;
    procedure WriteDate  (CONST Ident: string;   Value: TDateTime);              reintroduce; overload;

    { String }
    function  Read       (CONST Ident: string; Default: string): string;         overload;
    procedure Write      (const Ident, Value: String);                           overload;

    { Integer }
    function  Read       (const Ident: string; Default: Integer= 0): Integer;    overload;
    procedure Write      (const Ident: string;   Value: Integer);                overload;

    { Bool }
    function  Read       (CONST Ident: string; Default: Boolean= TRUE): Boolean; overload;
    procedure Write      (const Ident: string;   Value: Boolean);                overload;

    function  ReadFont   (CONST Ident: string): FontStruct;
    procedure WriteFont  (CONST Ident: string; Font: FontStruct);

    { Float }
    function  Read       (const Ident: string; Default: Double): Double;         overload;
    procedure Write      (const Ident: string;   Value: Double);                 overload;
  end;


IMPLEMENTATION

USES
   LightCore.IO;



{-------------------------------------------------------------------------------------------------------------
   MAIN
-------------------------------------------------------------------------------------------------------------}
constructor TIniFileEx.Create(CONST SectionName, FileName: string);
begin
 TDirectory.CreateDirectory(ExtractFilePath(FileName));  // Make sure the path exists otherwise we get an error.
 inherited Create(FileName);
 FSection:= SectionName;
end;




{---------------
   FONT
---------------}

{ Reads font settings from INI file. Returns defaults if font entry not found. }
function TIniFileEx.ReadFont(CONST Ident: string): FontStruct;
begin
 if ValueExists(FSection, Ident+ 'Name')
 then
   begin
     Result.Name  := ReadString (FSection, Ident+ 'Name',  'Arial');
     Result.Color := TColor     (ReadInteger(FSection, Ident+ 'Color', 0));
     Result.Size  := ReadInteger(FSection, Ident+ 'Size',  8);
     Result.Style := TFontStyles(Byte(ReadInteger(FSection, Ident+ 'Style', 0)));
   end
 else
   begin
     { Return sensible defaults }
     Result.Name  := 'Arial';
     Result.Color := TColor(0);   { clBlack }
     Result.Size  := 8;
     Result.Style := [];
   end;
end;

{ Writes font settings to INI file.
  Note: Writes empty string for Ident so TIniFileVCL.ReadFont can find the font by identifier. }
procedure TIniFileEx.WriteFont(CONST Ident: string; Font: FontStruct);
begin
  WriteString (FSection, Ident,            '');
  WriteString (FSection, Ident + 'Name',   Font.Name);
  WriteInteger(FSection, Ident + 'Color',  Font.Color);
  WriteInteger(FSection, Ident + 'Size',   Font.Size);
  WriteInteger(FSection, Ident + 'Style',  Byte(Font.Style));
end;



{---------------
   DATE
---------------}
function TIniFileEx.ReadDate(const Ident: string; Default: TDateTime): TDateTime;
begin
  Result:= inherited ReadDate(FSection, Ident, Default);
end;

procedure TIniFileEx.WriteDate(const Ident: string; Value: TDateTime);
begin
  inherited writedate(FSection, Ident, Value);
end;







{----------------------
   DEFAULT OVERLOADS
----------------------}

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









{-------------------------------------------------------------------------------------------------------------
   TIniFileDt
   Overrides date read/write to use float format (independent of regional settings).
-------------------------------------------------------------------------------------------------------------}
function TIniFileDt.ReadDate(const Section, Ident: string; Default: TDateTime): TDateTime;
begin
  Result:= TDateTime(ReadFloat(Section, Ident, Default));
end;


procedure TIniFileDt.WriteDate(const Section, Ident: string; Value: TDateTime);
begin
  WriteFloat(Section, Ident, Value);
end;



end.

