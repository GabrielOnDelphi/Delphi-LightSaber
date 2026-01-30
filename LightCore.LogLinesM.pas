UNIT LightCore.LogLinesM;

{=============================================================================================================
   2026.01.29
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Multi-Threaded Version of LogLines

   This is the thread-safe implementation of TAbstractLogLines.
   Use this when log operations may occur from multiple threads simultaneously.

   Thread Safety Implementation:
     Uses TMultiReadExclusiveWriteSynchronizer (MREWS) which allows:
     - Multiple concurrent readers (BeginRead/EndRead)
     - Exclusive writer access (BeginWrite/EndWrite)
     - Writers wait for readers to finish; readers wait for writers

   Individual Operation Safety:
     Each method (Add, Count, Clear, getItem, etc.) is individually thread-safe.

   IMPORTANT - Iteration Limitation (TOCTOU):
     While individual operations are thread-safe, iterating over the list is NOT atomic.
     Example of unsafe pattern:
       for i:= 0 to Lines.Count-1 do  // Count acquired with read lock
         DoSomething(Lines[i]);        // Item access with separate read lock
     Between these calls, another thread could modify the list.

     Safe patterns:
     - Use Row2FilteredRow() which iterates under a single lock
     - For bulk operations, consider using WriteToStream/ReadFromStream
     - Or accept that iteration may see a slightly inconsistent view

   Tester:
     LightSaber\Demo\LightLog\
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Classes,
   LightCore.LogTypes, LightCore.StreamBuff, LightCore.LogLinesAbstract;

TYPE
  TLogLinesMultiThreaded = class(TAbstractLogLines)
  private
    FLock: TMultiReadExclusiveWriteSynchronizer;
  protected
    function getItem(Index: Integer): PLogLine; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;
    function Count: Integer; override;
    function CountFiltered(Verbosity: TLogVerbLvl): Integer; override;
    function Row2FilteredRow(Row: Integer; Verbosity: TLogVerbLvl): Integer; override;

    function AddNewLine(Msg: string; Level: TLogVerbLvl; Bold: Boolean = FALSE): PLogLine; override;
    function Add(Value: PLogLine): Integer; override;

    procedure ReadFromStream(Stream: TLightStream); override;
    procedure WriteToStream (Stream: TLightStream); override;
  end;


IMPLEMENTATION



{-------------------------------------------------------------------------------------------------------------
   CONSTRUCTOR / DESTRUCTOR
-------------------------------------------------------------------------------------------------------------}
constructor TLogLinesMultiThreaded.Create;
begin
  inherited Create;
  FList:= TList.Create;
  FLock:= TMultiReadExclusiveWriteSynchronizer.Create;
end;


destructor TLogLinesMultiThreaded.Destroy;
begin
  Clear;              { Free the allocated memory for lines }
  FreeAndNil(FList);
  FreeAndNil(FLock);
  inherited;
end;


{-------------------------------------------------------------------------------------------------------------
   ITEM ACCESS
   Note: Each access acquires/releases the lock independently.
   For safe iteration, use Row2FilteredRow or WriteToStream which hold the lock for the entire operation.
-------------------------------------------------------------------------------------------------------------}
function TLogLinesMultiThreaded.getItem(Index: Integer): PLogLine;
begin
  FLock.BeginRead;
  try
    Result:= PLogLine(FList[Index]);
  finally
    FLock.EndRead;
  end;
end;


{ Disposes all log line records and clears the list.
  Acquires exclusive write lock to prevent concurrent access during cleanup. }
procedure TLogLinesMultiThreaded.Clear;
var
  i: Integer;
begin
  FLock.BeginWrite;
  try
    for i:= 0 to FList.Count - 1 do
      Dispose(PLogLine(FList[i]));
    FList.Clear;
  finally
    FLock.EndWrite;
  end;
end;


{ Returns current count. Note: value may change immediately after return if another thread modifies the list. }
function TLogLinesMultiThreaded.Count: Integer;
begin
  FLock.BeginRead;
  try
    Result:= FList.Count;
  finally
    FLock.EndRead;
  end;
end;


{ Returns count of lines matching the verbosity filter.
  Thread-safe: holds lock for the entire iteration, unlike Count + Items[] pattern. }
function TLogLinesMultiThreaded.CountFiltered(Verbosity: TLogVerbLvl): Integer;
var
  i: Integer;
begin
  FLock.BeginRead;
  try
    Result:= 0;
    for i:= 0 to FList.Count - 1 do
      if PLogLine(FList[i]).Level >= Verbosity
      then Inc(Result);
  finally
    FLock.EndRead;
  end;
end;


{-------------------------------------------------------------------------------------------------------------
   ADD
-------------------------------------------------------------------------------------------------------------}

{ Adds an externally-created log line pointer to the list.
  The caller is responsible for allocating the PLogLine with New().
  The list takes ownership and will Dispose() it on Clear/Destroy. }
function TLogLinesMultiThreaded.Add(Value: PLogLine): Integer;
begin
  FLock.BeginWrite;
  try
    Result:= FList.Add(Value);
  finally
    FLock.EndWrite;
  end;
end;


{ Creates a new log line record, populates it, and adds it to the list.
  The record is created OUTSIDE the lock (New is thread-safe for memory allocation).
  Only the list operation is protected, minimizing lock contention.
  Returns the pointer to the newly created line. }
function TLogLinesMultiThreaded.AddNewLine(Msg: string; Level: TLogVerbLvl; Bold: Boolean = FALSE): PLogLine;
begin
  New(Result);
  Result.Msg   := Msg;
  Result.Level := Level;
  Result.Bold  := Bold;
  Result.Time  := Now;
  Result.Indent:= 0;

  FLock.BeginWrite;
  try
    FList.Add(Result);   { Use FList.Add directly to avoid double-locking (Add method also acquires the lock) }
  finally
    FLock.EndWrite;
  end;
end;

{-------------------------------------------------------------------------------------------------------------
   FILTERED ACCESS
-------------------------------------------------------------------------------------------------------------}

{ Converts a row number in the filtered view to the actual index in the full list.
  The filtered view only shows rows meeting the verbosity threshold.

  Example: If you have 10 log lines but only 5 meet the verbosity criteria,
  this function finds the actual index of the Nth visible row.

  Thread Safety: This method is FULLY thread-safe for iteration because it holds
  a single read lock for the entire operation, unlike Count+getItem patterns.

  Returns: The actual list index, or -1 if the filtered row doesn't exist. }
function TLogLinesMultiThreaded.Row2FilteredRow(Row: Integer; Verbosity: TLogVerbLvl): Integer;
var
  i, VisibleCount: Integer;
begin
  FLock.BeginRead;
  try
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
  finally
    FLock.EndRead;
  end;
end;








{-------------------------------------------------------------------------------------------------------------
   STREAM I/O
   These methods hold the lock for the entire operation, ensuring atomic serialization.
-------------------------------------------------------------------------------------------------------------}

{ Reads log lines from stream. Acquires write lock for the entire operation
  because it modifies the list (adds items via inherited implementation). }
procedure TLogLinesMultiThreaded.ReadFromStream(Stream: TLightStream);
begin
  FLock.BeginWrite;
  try
    inherited ReadFromStream(Stream);
  finally
    FLock.EndWrite;
  end;
end;


{ Writes log lines to stream. Acquires read lock for the entire operation
  to ensure a consistent snapshot is written. }
procedure TLogLinesMultiThreaded.WriteToStream(Stream: TLightStream);
begin
  FLock.BeginRead;
  try
    inherited WriteToStream(Stream);
  finally
    FLock.EndRead;
  end;
end;



end.
