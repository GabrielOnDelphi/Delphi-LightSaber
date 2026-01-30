UNIT LightCore.LogLinesS;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Single-Threaded Version of LogLines

   This is the non-thread-safe implementation of TAbstractLogLines.
   Use this when all log operations occur on a single thread (typically the main thread).
   For multi-threaded scenarios, use TLogLinesMultiThreaded from LightCore.LogLinesM.pas.

   The class stores log entries as PLogLine pointers in a TList.
   Memory for each log line is allocated with New() and freed with Dispose() on Clear/Destroy.

   Inherits ReadFromStream/WriteToStream from TAbstractLogLines (no override needed).

   Tester:
     LightSaber\Demo\LightLog\
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Classes,
   LightCore.LogTypes, LightCore.LogLinesAbstract;

TYPE
  TLogLinesSingleThreaded = class(TAbstractLogLines)
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
   CONSTRUCTOR / DESTRUCTOR
-------------------------------------------------------------------------------------------------------------}
constructor TLogLinesSingleThreaded.Create;
begin
  inherited Create;
  FList:= TList.Create;
end;


destructor TLogLinesSingleThreaded.Destroy;
begin
  Clear;             { Free the allocated memory for lines }
  FreeAndNil(FList);
  inherited;
end;


function TLogLinesSingleThreaded.getItem(Index: Integer): PLogLine;
begin
  Result:= PLogLine(FList[Index]);
end;


{ Disposes all log line records and clears the list. }
procedure TLogLinesSingleThreaded.Clear;
var
  i: Integer;
begin
  for i:= 0 to FList.Count - 1 do
    Dispose(PLogLine(FList[i]));
  FList.Clear;
end;


function TLogLinesSingleThreaded.Count: Integer;
begin
  Result:= FList.Count;
end;



{-------------------------------------------------------------------------------------------------------------
   ADD
-------------------------------------------------------------------------------------------------------------}

{ Adds an externally-created log line pointer to the list.
  The caller is responsible for allocating the PLogLine with New().
  The list takes ownership and will Dispose() it on Clear/Destroy. }
function TLogLinesSingleThreaded.Add(Value: PLogLine): Integer;
begin
  Assert(Value <> NIL, 'TLogLinesSingleThreaded.Add: Value cannot be nil');
  Result:= FList.Add(Value);
end;


{ Creates a new log line record, populates it, and adds it to the list.
  Returns the pointer to the newly created line. }
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

{ Converts a row number in the filtered view to the actual index in the full list.
  The filtered view only shows rows meeting the verbosity threshold.

  Example: If you have 10 log lines but only 5 meet the verbosity criteria,
  this function finds the actual index of the Nth visible row.

  Returns: The actual list index, or -1 if the filtered row doesn't exist. }
function TLogLinesSingleThreaded.Row2FilteredRow(Row: Integer; Verbosity: TLogVerbLvl): Integer;
var
  i, VisibleCount: Integer;
begin
  Result:= -1;
  VisibleCount:= -1;
  for i:= 0 to FList.Count - 1 do
  begin
    if PLogLine(FList[i]).Level >= Verbosity
    then begin
      Inc(VisibleCount);
      Result:= i;
    end;

    if VisibleCount = Row
    then EXIT;
  end;

  if VisibleCount < Row
  then Result:= -1;
end;



end.
