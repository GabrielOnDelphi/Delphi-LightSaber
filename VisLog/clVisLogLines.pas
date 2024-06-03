UNIT clVisLogLines;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

   For the new log (the one based on TStringGrid)

   Tester:
     c:\Myprojects\Packages\LightSaber\Demo\LightLog\
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Classes, Vcl.Graphics,
   clVisLogUtils, ccStreamBuff;

type
  PLogLine=^RLogLine;

  RLogLine= record
   Msg   : string;
   Level : TLogVerbLvl;
   Indent: Integer;          { How many spaces are used to indent the message }
   Bold  : Boolean;
   Color : TColor;           { If -1 the use color specified in 'Level'. If > -1 then it overrides the color specified by 'Level' }
   Time  : TDateTime;
   procedure ReadFromStream(Stream: TCubicBuffStream);
   procedure WriteToStream (Stream: TCubicBuffStream);
  end;

  TLogLines=class(TList)
  private
    function Get(Index: Integer): PLogLine;
  public
    procedure Clear; override;
    destructor Destroy; override;

    procedure ReadFromStream(Stream: TCubicBuffStream);
    procedure WriteToStream (Stream: TCubicBuffStream);

    function GetFilteredRow(Row: Integer; Verbosity: TLogVerbLvl): Integer;        { Converts the 'on scree' row to its corresponding visible line. When AppData.LogVerbosity is set to max (Verbose) then the correspondence is 1:1 }

    function AddNewLine(Msg: string; Level: TLogVerbLvl; Bold: Boolean= FALSE; Color: TColor= -1): PLogLine;
    function Add(Value: PLogLine): Integer;
    property Items[Index: Integer]: PLogLine read Get; default;
  end;




IMPLEMENTATION


destructor TLogLines.Destroy;
begin
 Clear;             { Free the allocated memory for lines }
 inherited;
end;


procedure TLogLines.Clear;
VAR i: Integer;
begin
 for i:= 0 to Count - 1
   DO Dispose(Items[i]);

 inherited;
end;










function TLogLines.Add(Value: PLogLine): Integer;
begin
  Result:= inherited Add(Value);
end;



function TLogLines.AddNewLine(Msg: string; Level: TLogVerbLvl; Bold: Boolean= FALSE; Color: TColor= -1): PLogLine;
begin
 New(Result);

 Result.Msg   := Msg;
 Result.Level := Level;
 Result.Bold  := Bold;
 Result.Time  := Now;
 Result.Color := Color;                                                { If -1 the use color specified in 'Level'. If > -1 then it overrides the color specified by 'Level' }
 Result.Indent:= 0;

 Add(Result);
end;







function TLogLines.Get(Index: Integer): PLogLine;
begin
  Result:= PLogLine(inherited Get(Index));
end;



function TLogLines.GetFilteredRow;
var
  i, Total: Integer;
begin
 Result:= -1;
 Total := -1;
 for i:= 0 to Count-1 DO
  begin
   if PLogLine(Items[i]).Level >= Verbosity then
    begin
     Inc(Total);
     Result:= i;
    end;
   if Total = Row then EXIT;
  end;
 if total < row then Result:= -1;
end;








procedure TLogLines.ReadFromStream(Stream: TCubicBuffStream);
VAR
   Line: PLogLine;
   iCount, i: Integer;
begin
 iCount:= Stream.ReadInteger;

 for i:= 0 to iCount -1 DO
  begin
   New(Line);
   Line.ReadFromStream(Stream);
   Add(Line);
  end;
end;


procedure TLogLines.WriteToStream(Stream: TCubicBuffStream);
VAR i: Integer;
begin
 Stream.WriteInteger(Count);
 for i:= 0 to Count -1
  DO Self[i].WriteToStream(Stream);
end;














procedure RLogLine.ReadFromStream;
begin
 Msg   := Stream.ReadStringU;
 Level := TLogVerbLvl(Stream.ReadInteger);
 Indent:= Stream.ReadInteger;
 Bold  := Stream.ReadBoolean;
 Color := Stream.ReadInteger;
 Time  := Stream.ReadDate;
 { Padding }
 Stream.ReadInteger;
 Stream.ReadInteger;
 Stream.ReadBoolean;
end;


procedure RLogLine.WriteToStream;
begin
 Stream.WriteStringU (Msg);
 Stream.WriteInteger (Ord(Level));
 Stream.WriteInteger (Indent);
 Stream.WriteBoolean (Bold);
 Stream.WriteInteger (Color);
 Stream.WriteDate    (Time);
 { Padding }
 Stream.WriteInteger(0);
 Stream.WriteInteger(0);
 Stream.WriteBoolean(FALSE);
end;



end.
