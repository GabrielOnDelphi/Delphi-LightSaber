UNIT ccINIFile;

{=======================================================================================================================
  CubicDesign
  2022-04-03

  Features:
     - Extends the capabilities of TIniFile.
     - Functions for easily accessing application's default INI file.

  Setup:
     Before using it you must set the AppData.AppName global var like this: AppData:= TAppData.Create('MyCollApp').
     The class will use that name to automatically determine the INI file name/path which is %AppData%\AppData.AppName.Ini.
     Example: If the AppData.AppName is set to "DelphiLightSaber" the ini file will be "c:\Users\UserName\AppData\Roaming\DelphiLightSaber\DelphiLightSaber.ini"
     See ccAppData.pas for details.

  Reminder: TIniFile limitations:
     - Cannot put spaces at the beggining of a 'value'. The spaces will be trimmed. This behavior does not happen if you use a TMemInifile.      https://stackoverflow.com/questions/3702647/reading-inifile-to-stringlist-problem-spaces-problem
     - Cannot handle quotes and enters
     - Issues with Unicode strings
     Some of these limitations are not in TMemIniFile.
=======================================================================================================================}

INTERFACE

USES
   System.SysUtils, Vcl.Graphics, System.UITypes, System.IniFiles, ccCore;

TYPE
  FontStruct = record
    Name  : string;
    Size  : Integer;
    Style : TFontStyle;
    Color : TColor;
  end;
  
TYPE
 TCubicIniFile = class(TIniFile)
  protected
    FSection: string;
  public
    constructor Create (SectionName: string; ForcedPath: string= '');  virtual;
    class function AsString: string;
    function  ValueExists(CONST Ident: string): Boolean;                         reintroduce; overload;

    { Data/Time }
    function  ReadDateEx (CONST Ident: string; Default: TDateTime): TDateTime;
    procedure WriteDateEx(CONST Ident: string;   Value: TDateTime);

    { OLD. BROKEN - Depends on user's FormatSettings }
    function  ReadDate_   (CONST Ident: string; Default: TDateTime): TDateTime;    deprecated 'Use ReadDateEx instead';  // http://docwiki.embarcadero.com/RADStudio/Sydney/en/Methods_(Delphi)
    procedure WriteDate_  (CONST Ident: string;   Value: TDateTime);               deprecated 'Use ReadDateEx instead';

    { String }
    function  Read       (CONST Ident: string; Default: string): string;         overload;
    procedure Write      (const Ident, Value: String);                           overload;

    { Integer }
    function  Read       (const Ident: string; Default: Longint= 0): Longint;    overload;
    procedure Write      (const Ident: string; Value: Longint);                  overload;

    { Bool }
    function  Read       (CONST Ident: string; Default: Boolean= TRUE): Boolean; overload;
    procedure Write      (const Ident: string; Value: Boolean);                  overload;

    { Font }
    function  Read       (CONST Ident: string; Font: TFont): Boolean;            overload;  { Result: If the INI file does not contains informations about font then this function will  return FALSE and no modification will be done to the 'Font' object passed as parameter. }
    procedure Write      (CONST Ident: string; Font: TFont);                     overload;

    function  Read       (CONST Ident: string): FontStruct;                      overload;
    procedure Write      (CONST Ident: string; Font: FontStruct);                overload;

    { Float }      { Overrides existing method with a new version that does not require a section name }
    function  Read       (const Ident: string; Default: Double): Double; {reintroduce_;} overload;
    procedure Write      (const Ident: string; Value: Double);           {reintroduce_;} overload;

    { Color }
    function  ReadColor  (CONST Ident: string; Default: TColor): TColor;
    procedure WriteColor (CONST Ident: string; Value: TColor);
  end;



IMPLEMENTATION

USES
   ccAppData, ccIO;






{-----------------------------------------------------------------------------------------------------------------------
   MAIN
-----------------------------------------------------------------------------------------------------------------------}
constructor TCubicIniFile.Create(SectionName: string; ForcedPath: string= '');                                { Open INI file for writing }
VAR Path: string;
begin
 if ForcedPath= ''
 then Path:= AppData.IniFile
 else Path:= ForcedPath;

 inherited Create(Path);

 TRY
   ForceDirectories(ExtractFilePath(Path));
 EXCEPT
   //todo 1: trap only specific exceptions
   MesajErrDetail ('Cannot create folder: '+ Path, 'TCubicIniFile.Create');
 END;

 FSection:= SectionName;
end;




{---------------
   COLOR
----------------}
function TCubicIniFile.ReadColor(CONST Ident: string; Default: Tcolor): TColor;
begin
 Result:= StringToColor(ReadString(FSection, Ident, ColorToString(Default)));
end;


procedure TCubicIniFile.WriteColor(CONST Ident: string; Value: Tcolor);
begin
 WriteString(FSection, Ident, ColorToString(Value));
end;




{---------------
   FONT
----------------}
procedure TCubicIniFile.Write(CONST Ident: string; Font: TFont);
begin
  WriteString (FSection, Ident,  '');      // I need this here so I can find the font by its identifier (name). Otherwise it will be filtered out by TCubicIniFileEx.Read: if ValueExists(FSection, Comp.Name) then
  WriteString (FSection, Ident + 'Name',    Font.Name);
  WriteInteger(FSection, Ident + 'CharSet', Font.CharSet);
  WriteInteger(FSection, Ident + 'Color',   Font.Color);
  WriteInteger(FSection, Ident + 'Size',    Font.Size);
  WriteInteger(FSection, Ident + 'Style',   Byte(Font.Style));
end;

procedure TCubicIniFile.Write(CONST Ident: string; Font: FontStruct);
begin
  WriteString (FSection, Ident,  '');      // I need this here so I can find the font by its identifier (name). Otherwise it will be filtered out by TCubicIniFileEx.Read: if ValueExists(FSection, Comp.Name) then
  WriteString (FSection, Ident + 'Name',    Font.Name);
  WriteInteger(FSection, Ident + 'Color',   Font.Color);
  WriteInteger(FSection, Ident + 'Size',    Font.Size);
  WriteInteger(FSection, Ident + 'Style',   Byte(Font.Style));
end;


function TCubicIniFile.Read(CONST Ident: string; Font: TFont): Boolean;
{ Result: If the INI file does not contains informations about font then this function will
  return FALSE and no modification will be done to the 'Font' object passed as parameter. }
begin
 Result:= ValueExists(FSection, Ident+ 'Name');
 if Result then
   begin
    Font.Name   := ReadString  (FSection,              Ident+ 'Name',    'Arial');
    Font.CharSet:= TFontCharSet(ReadInteger (FSection, Ident+ 'CharSet', 0));
    Font.Color  := TColor      (ReadInteger (FSection, Ident+ 'Color',   0));
    Font.Size   := ReadInteger (FSection,              Ident+ 'Size',    8);
    Font.Style  := TFontStyles (BYTE
                               (ReadInteger (FSection, Ident+ 'Style',   0)) );
   end;
end;


//ToDo: Return value of function 'TCubicIniFile.Read' might be undefined
function TCubicIniFile.Read(CONST Ident: string): FontStruct;
begin
 if ValueExists(FSection, Ident+ 'Name') then
   begin
    Result.Name   := ReadString  (FSection,              Ident+ 'Name',    'Arial');
    Result.Color  := TColor      (ReadInteger (FSection, Ident+ 'Color',   0));
    Result.Size   := ReadInteger (FSection,              Ident+ 'Size',    8);
    Result.Style  := TFontStyle  (BYTE(ReadInteger (FSection, Ident+ 'Style',   0)) );
   end;
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


{ BUG!
  https://stackoverflow.com/questions/20419347/delphi-inifiles-readdatetime

  The date format will not depend anymore on user's regional settings
  Cannot be named simply Write because it will conflict with Write(Real). I had a case where it wrote Write(2.0) to the ini file as a date (1900-01-01)
  The Default MUST be in the 0.0 format otherwise the wrong function will be called (the one for integer) }
procedure TCubicIniFile.WriteDate_(CONST Ident: string; Value: TDateTime);
begin
  inherited WriteDate(FSection, Ident, Value);
  //WriteString(FSection, Ident, DateToStr(Value, GetUniversalDateFormat));
end;

function TCubicIniFile.ReadDate_(CONST Ident: string; Default: TDateTime): TDateTime;   { The Default MUST be in the 0.0 format otherwise the wrong function will be called (the one for integer) }
begin
  Result:= inherited ReadDate(FSection, Ident, default);
end;



procedure TCubicIniFile.WriteDateEx(CONST Ident: string; Value: TDateTime);
begin
  WriteFloat(FSection, Ident, Value);
end;

function TCubicIniFile.ReadDateEx(CONST Ident: string; Default: TDateTime): TDateTime;
begin
  Result:= TDateTime(ReadFloat(FSection, Ident, default));
end;






{----------------------
   DEFAULT OVERLOADS
-----------------------}

function TCubicIniFile.ValueExists(const Ident: string): Boolean;
begin
 Result:= inherited ValueExists(FSection, Ident);
end;



function TCubicIniFile.Read(const Ident: string; Default: Boolean= TRUE): Boolean;
begin
 Result:= inherited ReadBool(FSection, Ident, Default);
end;

procedure TCubicIniFile.Write(const Ident: string; Value: Boolean);
begin
 inherited WriteBool(FSection, Ident, Value);
end;



function TCubicIniFile.Read(CONST Ident: string; Default: Integer= 0): Longint;
begin
 Result:= inherited ReadInteger(FSection, Ident, Default);
end;

procedure TCubicIniFile.Write(CONST Ident: string; Value: Integer);
begin
  inherited WriteInteger(FSection, Ident, Value);
end;




function TCubicIniFile.Read(CONST Ident: string; Default: string): string;
begin
 Result:= inherited ReadString(FSection, Ident, Default);
end;

procedure TCubicIniFile.Write(const Ident, Value: String);
begin
 inherited WriteString(FSection, Ident, Value);
end;



function TCubicIniFile.Read(const Ident: string; Default: Double): Double;
begin
 Result:= inherited ReadFloat(FSection, Ident, Default);
end;

procedure TCubicIniFile.Write(const Ident: string; Value: Double);
begin
 inherited WriteFloat(FSection, Ident, Value);
end;



{ Returns the entire content of the default INI file, as string.
  Don't forget to force the app to save its INI file to disk before loading the file. }             { Old name: IniFileToString }
class function TCubicIniFile.AsString: string;
begin
 Result:= StringFromFile(AppData.IniFile);
end;



end.
