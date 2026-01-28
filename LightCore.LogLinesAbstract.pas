UNIT LightCore.LogLinesAbstract;

{=============================================================================================================
   2025.03
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   For the new log (the one based on TStringGrid)

   Tester:
     LightSaber\Demo\LightLog\
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Classes,
   LightCore.LogTypes, LightCore.StreamBuff;

type
  PLogLine=^RLogLine;

  RLogLine= record
    Msg   : string;
    Level : TLogVerbLvl;
    Indent: Integer;          { How many spaces are used to indent the message }
    Bold  : Boolean;
    Time  : TDateTime;
  private
    procedure ReadFromStream_v5(Stream: TLightStream);  { Current reader }
    procedure WriteToStream    (Stream: TLightStream);  { Current writer }
  end;


  { List of lines }
  TAbstractLogLines = class
  private
    procedure readFromStream_v5(Stream: TLightStream);
  protected
    FList: TList;
    CONST StreamSign  = 'TLogLines';
    function getItem(Index: Integer): PLogLine;                                             virtual; abstract;
  public
    CONST CurVer= 5;
    procedure Clear;                                                                        virtual; abstract;
    function  Count: Integer;                                                               virtual; abstract;

    function  Row2FilteredRow(Row: Integer; Verbosity: TLogVerbLvl): Integer;               virtual; abstract;
    property  Items[Index: Integer]: PLogLine read getItem; default;

    function  Add       (Value: PLogLine): Integer;                                         virtual; abstract;
    function  AddNewLine(Msg: string; Level: TLogVerbLvl; Bold: Boolean = False): PLogLine; virtual; abstract;

    procedure ReadFromStream(Stream: TLightStream); virtual;
    procedure WriteToStream (Stream: TLightStream); virtual;
  end;



IMPLEMENTATION


{-------------------------------------------------------------------------------------------------------------
   ABSTRACT CLASS
-------------------------------------------------------------------------------------------------------------}

{ Read specific version }
procedure TAbstractLogLines.readFromStream_v5(Stream: TLightStream);
VAR
   Line: PLogLine;
   iCount, i: Integer;
begin
  iCount := Stream.ReadInteger;

  for i := 0 to iCount - 1 do
  begin
    New(Line);
    Line.ReadFromStream_v5(Stream);
    Add(Line);
  end;
end;


{ Read }
procedure TAbstractLogLines.ReadFromStream(Stream: TLightStream);
VAR StreamVer: Word;
begin
  StreamVer:= Stream.ReadHeader(StreamSign);
  if StreamVer = 0 then EXIT;

  if StreamVer= CurVer
  then readFromStream_v5(Stream)
  else RAISE Exception.Create('Unsupported stream version.');

  Stream.ReadPadding;
end;


{ Write }
procedure TAbstractLogLines.WriteToStream(Stream: TLightStream);
VAR i: Integer;
begin
  Stream.WriteHeader(StreamSign, CurVer);

  Stream.WriteInteger(FList.Count);
  for i := 0 to FList.Count - 1 do
    PLogLine(FList[i]).WriteToStream(Stream);

  Stream.WritePadding;
end;



{-------------------------------------------------------------------------------------------------------------
   RLogLine

   We don't write a header and version no for each line because we would waste too much space.
   Instead, the parent (TRamLog) is responsible to do this.
-------------------------------------------------------------------------------------------------------------}
procedure RLogLine.ReadFromStream_v5(Stream: TLightStream);
begin
  Msg    := Stream.ReadString;
  Level  := TLogVerbLvl(Stream.ReadInteger);
  Indent := Stream.ReadInteger;
  Bold   := Stream.ReadBoolean;
  Time   := Stream.ReadDate;
  Stream.ReadPadding(8);   { Padding }
end;


procedure RLogLine.WriteToStream(Stream: TLightStream);
begin
  Stream.WriteString (Msg);
  Stream.WriteInteger(Ord(Level));
  Stream.WriteInteger(Indent);
  Stream.WriteBoolean(Bold);
  Stream.WriteDate(Time);
  Stream.WritePadding(8);    { Padding }
end;



end.

