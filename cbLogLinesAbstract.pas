UNIT cbLogLinesAbstract;

{=============================================================================================================
   Gabriel Moraru
   2024.10
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

   For the new log (the one based on TStringGrid)

   Tester:
     LightSaber\Demo\LightLog\
=============================================================================================================}

INTERFACE

{$I Frameworks.inc}

USES
   System.SysUtils, System.Classes, System.UITypes,
   cbLogTypes, ccStreamBuff2;

type
  PLogLine=^RLogLine;

  RLogLine= record
    Msg   : string;
    Level : TLogVerbLvl;
    Indent: Integer;          { How many spaces are used to indent the message }
    Bold  : Boolean;
    Time  : TDateTime;
  private
    procedure ReadFromStream_v1(Stream: TCubicBuffStream2);  { Current reader }
    procedure WriteToStream    (Stream: TCubicBuffStream2);  { Current writer }
  end;


  { Abstract case class }
  TAbstractLogLines = class
  private
    procedure readFromStream_v1(Stream: TCubicBuffStream2);
  protected
    FList: TList;
    const
      StreamSign  = 'TLogLines';
      CurVer      = 2;
    function getItem(Index: Integer): PLogLine;                                             virtual; abstract;
  public
    procedure Clear;                                                                        virtual; abstract;
    function  Count: Integer;                                                               virtual; abstract;

    function  Row2FilteredRow(Row: Integer; Verbosity: TLogVerbLvl): Integer;               virtual; abstract;
    property  Items[Index: Integer]: PLogLine read getItem;                   default;

    function  Add       (Value: PLogLine): Integer;                                         virtual; abstract;
    function  AddNewLine(Msg: string; Level: TLogVerbLvl; Bold: Boolean = False): PLogLine; virtual; abstract;

    procedure ReadFromStream(Stream: TCubicBuffStream2); virtual;
    procedure WriteToStream (Stream: TCubicBuffStream2); virtual;
  end;



IMPLEMENTATION


{-------------------------------------------------------------------------------------------------------------
   ABSTRACT CLASS
-------------------------------------------------------------------------------------------------------------}
procedure TAbstractLogLines.ReadFromStream(Stream: TCubicBuffStream2);
VAR
   StreamVer: Word;
begin
  if NOT Stream.ReadHeaderVersion(StreamSign, StreamVer)
  then RAISE Exception.Create('Unknown stream signature.');

  case StreamVer of
     CurVer: readFromStream_v1(Stream);
   else
     RAISE Exception.Create('Usupported stream version.');
  end;

  Stream.ReadPaddingDef;
end;


procedure TAbstractLogLines.readFromStream_v1(Stream: TCubicBuffStream2);
VAR
   Line: PLogLine;
   iCount, i: Integer;
begin
  iCount := Stream.ReadInteger;

  for i := 0 to iCount - 1 do
  begin
    New(Line);
    Line.ReadFromStream_v1(Stream);
    Add(Line);
  end;
end;


procedure TAbstractLogLines.WriteToStream(Stream: TCubicBuffStream2);
VAR i: Integer;
begin
  Stream.WriteInteger(FList.Count);
  for i := 0 to FList.Count - 1 do
    PLogLine(FList[i]).WriteToStream(Stream);
end;



{-------------------------------------------------------------------------------------------------------------
   RLogLine

   We don't write a header and version no for each line because we would waste too much space.
   Instead, the parent (TRamLog) is responsible to do this.
-------------------------------------------------------------------------------------------------------------}
procedure RLogLine.ReadFromStream_v1(Stream: TCubicBuffStream2);
begin
  Msg    := Stream.ReadString;
  Level  := TLogVerbLvl(Stream.ReadInteger);
  Indent := Stream.ReadInteger;
  Bold   := Stream.ReadBoolean;
  Time   := Stream.ReadDate;
  Stream.ReadPadding(8);  { Padding }
end;


procedure RLogLine.WriteToStream(Stream: TCubicBuffStream2);
begin
  Stream.WriteString (Msg);
  Stream.WriteInteger(Ord(Level));
  Stream.WriteInteger(Indent);
  Stream.WriteBoolean(Bold);
  Stream.WriteDate(Time);
  Stream.WritePadding(8);    { Padding }
end;



end.

