UNIT cbLogLinesAbstract;

{=============================================================================================================
   Gabriel Moraru
   2024.10
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

   For the new log (the one based on TStringGrid)

   Tester:
     c:\Myprojects\LightSaber\Demo\LightLog\
=============================================================================================================}

INTERFACE

{$I Frameworks.inc}

USES
   System.SysUtils, System.Classes,
   {$IFDEF FRAMEWORK_VCL}
   Vcl.Graphics,
   {$Endif}
   cbLogUtils, ccStreamBuff;

type
  PLogLine=^RLogLine;

  RLogLine= record
    Msg   : string;
    Level : TLogVerbLvl;
    Indent: Integer;          { How many spaces are used to indent the message }
    Bold  : Boolean;
    Color : TColor;           { If -1 the use color specified in 'Level'. If > -1 then it overrides the color specified by 'Level' }
    Time  : TDateTime;
  private
    procedure ReadFromStream(Stream: TCubicBuffStream);
    procedure WriteToStream (Stream: TCubicBuffStream);
  end;


  { Abstract Base Class }
  TAbstractLogLines = class
  protected
    FList: TList;
    function getItem(Index: Integer): PLogLine; virtual; abstract;
  public
    procedure Clear; virtual; abstract;
    function  AddNewLine(Msg: string; Level: TLogVerbLvl; Bold: Boolean = FALSE; Color: TColor = 0): PLogLine; virtual; abstract;
    function  Add(Value: PLogLine): Integer; virtual; abstract;

    function  Row2FilteredRow(Row: Integer; Verbosity: TLogVerbLvl): Integer; virtual; abstract;
    property  Items[Index: Integer]: PLogLine read getItem; default;
    function  Count: Integer; virtual; abstract;

    procedure ReadFromStream(Stream: TCubicBuffStream); virtual;
    procedure WriteToStream (Stream: TCubicBuffStream); virtual;
  end;



IMPLEMENTATION




{-------------------------------------------------------------------------------------------------------------
   ABSTRACT CLASS
-------------------------------------------------------------------------------------------------------------}
procedure TAbstractLogLines.ReadFromStream(Stream: TCubicBuffStream);
VAR
   Line: PLogLine;
   iCount, i: Integer;
begin
  iCount := Stream.ReadInteger;

  for i := 0 to iCount - 1 do
  begin
    New(Line);
    Line.ReadFromStream(Stream);
    Add(Line);
  end;
end;


procedure TAbstractLogLines.WriteToStream(Stream: TCubicBuffStream);
VAR i: Integer;
begin
  Stream.WriteInteger(FList.Count);
  for i := 0 to FList.Count - 1 do
    PLogLine(FList[i]).WriteToStream(Stream);
end;



{-------------------------------------------------------------------------------------------------------------
   RLogLine
-------------------------------------------------------------------------------------------------------------}
procedure RLogLine.ReadFromStream(Stream: TCubicBuffStream);
begin
  Msg := Stream.ReadString;
  Level := TLogVerbLvl(Stream.ReadInteger);
  Indent := Stream.ReadInteger;
  Bold := Stream.ReadBoolean;
  Color := Stream.ReadInteger;
  Time := Stream.ReadDate;
  { Padding }
  Stream.ReadInteger;
  Stream.ReadInteger;
  Stream.ReadBoolean;
end;


procedure RLogLine.WriteToStream(Stream: TCubicBuffStream);
begin
  Stream.WriteString (Msg);
  Stream.WriteInteger(Ord(Level));
  Stream.WriteInteger(Indent);
  Stream.WriteBoolean(Bold);
  Stream.WriteInteger(Color);
  Stream.WriteDate(Time);
  { Padding }
  Stream.WriteInteger(0);
  Stream.WriteInteger(0);
  Stream.WriteBoolean(FALSE);
end;



end.
