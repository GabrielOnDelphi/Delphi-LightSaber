UNIT LightCore.LogLinesM;

{=============================================================================================================
   2026.05.07
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
  { Multi-threaded log lines store. Filtered iteration (CountFiltered, Row2FilteredRow,
    GetFilteredSlice) is inherited from TAbstractLogLines — this subclass overrides only
    the lock hooks (acquireReadLock / releaseReadLock) to attach the MREWS read lock.
    That keeps the iteration logic in a single place while preserving the lock-free single-threaded path. }
  TLogLinesMultiThreaded = class(TAbstractLogLines)
  private
    FLock: TMultiReadExclusiveWriteSynchronizer;
  protected
    function getItem(Index: Integer): PLogLine; override;

    { Lock hooks — override base no-ops with MREWS BeginRead / EndRead.
      No nested-write hazard exists in our code paths anymore; readFromStream_v5
      uses the non-virtual addInternal so it doesn't dispatch back through Add. }
    procedure acquireReadLock; override;
    procedure releaseReadLock; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;
    function Count: Integer; override;

    function AddNewLine(CONST Msg: string; Level: TLogVerbLvl; Bold: Boolean = FALSE; Indent: Integer = 0): PLogLine; override;
    function Add(Value: PLogLine): Integer; override;

    function  SnapshotAndClear: TAbstractLogLines; override;

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


{ Lock hooks — override the base no-ops to acquire/release the MREWS read lock.
  Called by the inherited CountFiltered / Row2FilteredRow / GetFilteredSlice. }
procedure TLogLinesMultiThreaded.acquireReadLock;
begin
  FLock.BeginRead;
end;

procedure TLogLinesMultiThreaded.releaseReadLock;
begin
  FLock.EndRead;
end;


{-------------------------------------------------------------------------------------------------------------
   ADD
-------------------------------------------------------------------------------------------------------------}

{ Adds an externally-created log line pointer to the list.
  The caller is responsible for allocating the PLogLine with New().
  The list takes ownership and will Dispose() it on Clear/Destroy. }
function TLogLinesMultiThreaded.Add(Value: PLogLine): Integer;
begin
  Assert(Value <> NIL, 'TLogLinesMultiThreaded.Add: Value cannot be nil');
  FLock.BeginWrite;
  try
    Result:= FList.Add(Value);
  finally
    FLock.EndWrite;
  end;
end;


{ Creates a new log line record (cheap, lock-free), then inserts the pointer under
  write lock. The record is private to this thread until FList.Add returns. }
function TLogLinesMultiThreaded.AddNewLine(CONST Msg: string; Level: TLogVerbLvl; Bold: Boolean = FALSE; Indent: Integer = 0): PLogLine;
begin
  New(Result);
  Result.Msg   := Msg;
  Result.Level := Level;
  Result.Bold  := Bold;
  Result.Time  := Now;
  Result.Indent:= Indent;

  FLock.BeginWrite;
  try
    FList.Add(Result);   { Use FList.Add directly to avoid double-locking (Add method also acquires the lock) }
  finally
    FLock.EndWrite;
  end;
end;

{-------------------------------------------------------------------------------------------------------------
   FILTERED ACCESS

   Bodies of CountFiltered, Row2FilteredRow, GetFilteredSlice live in TAbstractLogLines.
   This class only provides the lock hook overrides above, which the inherited
   bodies call to obtain/release the MREWS read lock.
-------------------------------------------------------------------------------------------------------------}








{-------------------------------------------------------------------------------------------------------------
   STREAM I/O
   These methods hold the lock for the entire operation, ensuring atomic serialization.

   Why these aren't templated through acquireReadLock/releaseReadLock hooks like the
   filtered methods are: WriteToStream takes a SHARED read lock, ReadFromStream takes
   an EXCLUSIVE write lock — two different MREWS operations, not one parameterizable
   variant.
-------------------------------------------------------------------------------------------------------------}

{ Reads log lines from stream. Acquires write lock for the entire operation
  because it modifies the list (adds items via inherited implementation).

  No reentrancy hazard: the inherited body calls readFromStream_v5, which appends
  via addInternal (non-virtual, non-locking) — not the public virtual Add. This
  was historically a real concern (the loop went through TLogLinesMultiThreaded.Add
  → FLock.BeginWrite a second time, relying on RTL MREWS write reentrancy). The
  addInternal indirection eliminates the dependency, so a future migration to a
  non-reentrant primitive (TLightweightMREW / SRWLOCK / pthread_rwlock) won't
  silently break this path. }
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


{ Atomic snapshot+clear under exclusive lock: transfers all PLogLine pointers into
  a new instance and leaves Self empty without disposing them. No writes can happen
  between snapshotting and clearing because BeginWrite holds the lock for the entire
  operation. Caller owns the returned snapshot. }
function TLogLinesMultiThreaded.SnapshotAndClear: TAbstractLogLines;
VAR
  Snapshot: TLogLinesMultiThreaded;
  i: Integer;
begin
  Snapshot:= TLogLinesMultiThreaded.Create;
  TRY
    FLock.BeginWrite;
    try
      Snapshot.FList.Capacity:= FList.Count;
      for i:= 0 to FList.Count - 1 do
        Snapshot.FList.Add(FList[i]);
      FList.Clear;   { Pointers transferred — do NOT Dispose; the snapshot owns them now. }
    finally
      FLock.EndWrite;
    end;
  EXCEPT
    FreeAndNil(Snapshot);
    RAISE;
  END;
  Result:= Snapshot;
end;


end.
