UNIT LightCore.LogLinesS;

{=============================================================================================================
   Gabriel Moraru
   2024.10
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
   Single-Threaded Version of LogLines
   For the new log (the one based on TStringGrid)

   Tester:
     LightSaber\Demo\LightLog\
=============================================================================================================}

INTERFACE
{ $I Frameworks.inc}

USES
   System.SysUtils, System.Classes,
   LightCore.LogTypes, LightCore.LogLinesAbstract;

TYPE
  TLogLinesSingleThreaded = class(TAbstractLogLines)
  private
  protected
    function getItem(Index: Integer): PLogLine; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;
    function Count: Integer; override;
    function Row2FilteredRow(Row: Integer; Verbosity: TLogVerbLvl): Integer; override;

    function AddNewLine(Msg: string; Level: TLogVerbLvl; Bold: Boolean = FALSE): PLogLine; override;
    function Add       (Value: PLogLine): Integer; override;
  end;


IMPLEMENTATION



{-------------------------------------------------------------------------------------------------------------
  CTOR
-------------------------------------------------------------------------------------------------------------}
constructor TLogLinesSingleThreaded.Create;
begin
  inherited Create;
  FList := TList.Create;
end;


destructor TLogLinesSingleThreaded.Destroy;
begin
  Clear;             { Free the allocated memory for lines }
  FreeAndNil(FList);
  inherited;
end;


function TLogLinesSingleThreaded.getItem(Index: Integer): PLogLine;
begin
  Result := PLogLine(FList[Index]);
end;


procedure TLogLinesSingleThreaded.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    Dispose(PLogLine(FList[i]));
  FList.Clear;
end;


function TLogLinesSingleThreaded.Count: Integer;
begin
  Result := FList.Count;
end;



{-------------------------------------------------------------------------------------------------------------
   ADD
-------------------------------------------------------------------------------------------------------------}
function TLogLinesSingleThreaded.Add(Value: PLogLine): Integer;
begin
  Result := FList.Add(Value);
end;


function TLogLinesSingleThreaded.AddNewLine(Msg: string; Level: TLogVerbLvl; Bold: Boolean = FALSE): PLogLine;
begin
  New(Result);
  Result.Msg   := Msg;
  Result.Level := Level;
  Result.Bold  := Bold;
  Result.Time  := Now;
  Result.Indent:= 0;

  Add(Result);
end;



{-------------------------------------------------------------------------------------------------------------
   ACCESS
-------------------------------------------------------------------------------------------------------------}
{ Convert a row number in the filtered view (which only shows rows meeting the verbosity criteria) to the corresponding index in the full list of log lines.
  For example, if you have 10 log lines but only 5 meet the verbosity criteria, this function allows you to find the actual index of the 3rd visible row in the full list of log lines. }
function TLogLinesSingleThreaded.Row2FilteredRow(Row: Integer; Verbosity: TLogVerbLvl): Integer;
var
  i, Total: Integer;
begin
  Result := -1;
  Total  := -1;
  for i  := 0 to FList.Count - 1 do
  begin
    if PLogLine(FList[i]).Level >= Verbosity then
      begin
        Inc(Total);
        Result := i;
      end;

    if Total = Row then Exit;
  end;

  if Total < Row then Result := -1;
end;



end.
