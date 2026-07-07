UNIT LightCore.LogLinesS;

{=============================================================================================================
   2026.05.07
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
  { Single-threaded log lines store. Inherits CountFiltered, Row2FilteredRow, and
    GetFilteredSlice from the abstract base — the lock hooks (acquireReadLock / releaseReadLock) stay as no-ops, so iteration runs lock-free. }
  TLogLinesSingleThreaded = class(TAbstractLogLines)
  protected
    function getItem(Index: Integer): PLogLine; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;
    function Count: Integer; override;

    function AddNewLine(CONST Msg: string; Level: TLogVerbLvl; Bold: Boolean = FALSE; Indent: Integer = 0): PLogLine; override;
    function Add       (Value: PLogLine): Integer; override;

    function SnapshotAndClear: TAbstractLogLines; override;
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
  if FList <> NIL    { NIL when the constructor raised before TList.Create finished (partially-constructed object) }
  then Clear;        { Free the allocated memory for lines }
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
function TLogLinesSingleThreaded.AddNewLine(CONST Msg: string; Level: TLogVerbLvl; Bold: Boolean = FALSE; Indent: Integer = 0): PLogLine;
begin
  New(Result);
  Result.Msg   := Msg;
  Result.Level := Level;
  Result.Bold  := Bold;
  Result.Time  := Now;
  Result.Indent:= Indent;

  Add(Result);
end;



{-------------------------------------------------------------------------------------------------------------
   ACCESS

   Row2FilteredRow / GetFilteredSlice / CountFiltered now live in TAbstractLogLines.
   The single-threaded path leaves the lock hooks as no-ops, so iteration is lock-free.
-------------------------------------------------------------------------------------------------------------}

{ Atomic snapshot+clear: transfers all PLogLine pointers into a new instance and
  leaves Self empty without disposing them. Caller owns the returned snapshot. }
function TLogLinesSingleThreaded.SnapshotAndClear: TAbstractLogLines;
VAR
  Snapshot: TLogLinesSingleThreaded;
  i: Integer;
begin
  Snapshot:= TLogLinesSingleThreaded.Create;
  TRY
    Snapshot.FList.Capacity:= FList.Count;
    for i:= 0 to FList.Count - 1 do
      Snapshot.FList.Add(FList[i]);
    FList.Clear;   { Pointers transferred — do NOT Dispose; the snapshot owns them now. }
  EXCEPT
    FreeAndNil(Snapshot);
    RAISE;
  END;
  Result:= Snapshot;
end;


end.
