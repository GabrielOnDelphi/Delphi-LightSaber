UNIT LightCore.LogLinesAbstract;

{=============================================================================================================
   2026.05.07
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Abstract base class for log line storage.

   This class defines the interface for log line collections, with two concrete implementations:
     - TLogLinesSingleThreaded (LightCore.LogLinesS.pas) - for single-threaded use
     - TLogLinesMultiThreaded (LightCore.LogLinesM.pas) - for multi-threaded use

   The RLogLine record stores individual log entries with message, verbosity level,
   timestamp, indentation, and bold flag.

   Stream Format (Version 5):
     - Header with signature "TLogLines" and version
     - Integer count of lines
     - Each line: Msg, Level, Indent, Bold, Time, 8-byte padding
     - Footer padding

   Tester:
     LightSaber\Demo\LightLog\
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Classes,
   LightCore.LogTypes, LightCore.StreamBuff;

type
  PLogLine=^RLogLine;

  { Callback for ForEachLocked — invoked once per line, in order, while the read lock is held. }
  TLogLineProc = reference to procedure (Line: PLogLine);

  RLogLine= record
    Msg   : string;
    Level : TLogVerbLvl;
    Indent: Integer;          { Spaces used to indent the message in the visual log.
                                Read/written by the binary stream format. Set via the
                                Indent parameter on AddNewLine (default 0). Visual log
                                viewers may render a left-margin prefix proportional to
                                this value; viewers that ignore it just see normal text. }
    Bold  : Boolean;
    Time  : TDateTime;
  private
    procedure ReadFromStream_v5(Stream: TLightStream);  { Current reader }
    procedure WriteToStream    (Stream: TLightStream);  { Current writer }
  end;


  { List of lines }
  TAbstractLogLines = class abstract
  private
    procedure readFromStream_v5(Stream: TLightStream);
  protected
    FList: TList;
    CONST StreamSign  = 'TLogLines';
    function getItem(Index: Integer): PLogLine;                                             virtual; abstract;

    { Non-virtual, non-locking append. Used by readFromStream_v5 which already holds
      the write lock (in MT mode, via TLogLinesMultiThreaded.ReadFromStream).
      Calling the public virtual Add inside readFromStream_v5 would dispatch back into
      TLogLinesMultiThreaded.Add and acquire the write lock a second time on the same
      thread — works today only because TMultiReadExclusiveWriteSynchronizer happens to
      be reentrant for write (FWriteRecursionCount in System.SysUtils.pas, D13.1), and
      would self-deadlock on a future migration to a non-reentrant primitive like
      TLightweightMREW (Win32 SRWLOCK / pthread_rwlock).
      addInternal bypasses that hazard entirely by writing directly to FList. }
    procedure addInternal(Value: PLogLine); inline;

    { Lock hooks — template method pattern. Single-threaded subclass leaves them as no-ops;
      multi-threaded subclass overrides to BeginRead / EndRead. The shared filtered-iteration
      logic in the base class wraps every walk in acquireReadLock / releaseReadLock so the
      body can live in one place instead of being duplicated S vs M.
      One virtual call per filter operation (not per iteration) — negligible cost. }
    procedure acquireReadLock; virtual;
    procedure releaseReadLock; virtual;
  public
    CONST CurVer= 5;
    procedure Clear;                                                                        virtual; abstract;
    function  Count: Integer;                                                               virtual; abstract;

    { Filtered access. Bodies live here in the base class and use acquireReadLock /
      releaseReadLock to opt into thread safety in the multi-threaded subclass. }
    function  CountFiltered(Verbosity: TLogVerbLvl): Integer;                               virtual;
    function  Row2FilteredRow(Row: Integer; Verbosity: TLogVerbLvl): Integer;               virtual;
    function  GetFilteredSlice(Verbosity: TLogVerbLvl; SkipCount: Integer;
                               var Buffer: array of PLogLine): Integer;                     virtual;

    { Walks every line in order, invoking Proc for each, while holding the read lock
      for the whole walk. Use this for full-log exports (GetAsText, SaveAsRtf, etc.)
      where a per-element Lines[i] loop would be a TOCTOU bug in MT mode.

      READ-ONLY CONTRACT (will self-deadlock if violated):
        Proc must NOT call back into any TAbstractLogLines mutation method on the
        same thread — that means no Add, AddNewLine, Clear, SnapshotAndClear,
        ReadFromStream. The MT subclass holds the MREWS read lock; a nested write
        attempt from Proc would block waiting for that read lock to release, but
        the read lock cannot release until Proc returns. Classic self-deadlock.
        Proc may freely read Line fields; that's the entire purpose. }
    procedure ForEachLocked(Proc: TLogLineProc);                                            virtual;
    property  Items[Index: Integer]: PLogLine read getItem; default;

    function  Add       (Value: PLogLine): Integer;                                                                  virtual; abstract;
    function  AddNewLine(CONST Msg: string; Level: TLogVerbLvl; Bold: Boolean = FALSE; Indent: Integer = 0): PLogLine; virtual; abstract;

    { Atomic write+swap: returns a new TAbstractLogLines holding the previous content,
      and leaves Self empty. The returned snapshot is owned by the caller and must be freed.
      In multi-threaded mode, this is performed under a single exclusive lock so no entries
      can be added between snapshotting and clearing. }
    function  SnapshotAndClear: TAbstractLogLines;                                          virtual; abstract;

    procedure ReadFromStream(Stream: TLightStream); virtual;
    procedure WriteToStream (Stream: TLightStream); virtual;
  end;



IMPLEMENTATION

USES
   LightCore.Types;  


CONST
  { Hard ceiling on a single log message's UTF-8 byte length on disk. 
    Log lines legitimately exceed the default TLightStream 1*KB SafetyLimit (stack traces,
    JSON dumps, exception messages with embedded payloads), but a corrupted length
    prefix could otherwise trigger an enormous allocation. 16 MB is comfortably
    above any realistic log line yet small enough to fail fast on garbage input. }
  MaxLogMsgBytes = 16 * 1024 * KB;


{-------------------------------------------------------------------------------------------------------------
   ABSTRACT CLASS
-------------------------------------------------------------------------------------------------------------}

{ Non-virtual, non-locking append. See declaration comment for the reentrancy
  hazard this avoids. Caller is responsible for whatever locking is appropriate
  in the surrounding context (readFromStream_v5 is wrapped by ReadFromStream which
  in MT mode holds the write lock for the whole operation). }
procedure TAbstractLogLines.addInternal(Value: PLogLine);
begin
  Assert(Value <> NIL, 'addInternal: Value cannot be nil');
  FList.Add(Value);
end;


{ Lock hooks — no-ops in the base/single-threaded class. The multi-threaded subclass
  overrides them to BeginRead / EndRead. Keeping the bodies empty means the
  TLogLinesSingleThreaded path pays only for two virtual calls per filter operation
  (about a nanosecond each) — no actual lock acquisition. }
procedure TAbstractLogLines.acquireReadLock;
begin
  { intentionally empty — see comment above }
end;

procedure TAbstractLogLines.releaseReadLock;
begin
  { intentionally empty — see comment above }
end;


{ Returns the number of log lines that meet the verbosity threshold.
  Lock hook makes this thread-safe in TLogLinesMultiThreaded; no-op in S. }
function TAbstractLogLines.CountFiltered(Verbosity: TLogVerbLvl): Integer;
var
  i: Integer;
begin
  acquireReadLock;
  try
    Result:= 0;
    for i:= 0 to FList.Count - 1 do
      if PLogLine(FList[i]).Level >= Verbosity
      then Inc(Result);
  finally
    releaseReadLock;
  end;
end;


{ Converts a row number in the filtered view to the actual index in the full list.
  The filtered view only shows rows meeting the verbosity threshold.

  Example: If you have 10 log lines but only 5 meet the verbosity criteria,
  this function finds the actual index of the Nth visible row.

  Returns: The actual list index, or -1 if the filtered row doesn't exist.
  Negative Row values are treated as "not found" and return -1.

  Lock hook makes this thread-safe in TLogLinesMultiThreaded; no-op in S. }
function TAbstractLogLines.Row2FilteredRow(Row: Integer; Verbosity: TLogVerbLvl): Integer;
var
  i, VisibleCount: Integer;
begin
  if Row < 0 then EXIT(-1);   { Contract guard: negative Row is invalid input. }

  acquireReadLock;
  try
    VisibleCount:= 0;
    for i:= 0 to FList.Count - 1 do
      if PLogLine(FList[i]).Level >= Verbosity then
      begin
        if VisibleCount = Row
        then EXIT(i);
        Inc(VisibleCount);
      end;
    Result:= -1;   { Row is past the end of the filtered view. }
  finally
    releaseReadLock;
  end;
end;


{ Walks every line in order under a single read lock. The callback receives the
  raw PLogLine pointer; in MT mode the lock is held for the duration, so the
  callback must finish before any new Add can proceed. Keep the callback short. }
procedure TAbstractLogLines.ForEachLocked(Proc: TLogLineProc);
var
  i: Integer;
begin
  Assert(Assigned(Proc), 'ForEachLocked: Proc cannot be nil');
  acquireReadLock;
  try
    for i:= 0 to FList.Count - 1 do
      Proc(PLogLine(FList[i]));
  finally
    releaseReadLock;
  end;
end;


{ Atomically captures up to Length(Buffer) PLogLine pointers in order, including
  only entries with Level >= Verbosity, after skipping the first SkipCount qualifying entries.
  Returns the number of slots actually filled in Buffer.
  Lock hook makes this thread-safe in TLogLinesMultiThreaded; no-op in S. }
function TAbstractLogLines.GetFilteredSlice(Verbosity: TLogVerbLvl; SkipCount: Integer;
                                            var Buffer: array of PLogLine): Integer;
var
  i, Skipped, BufLen: Integer;
  Line: PLogLine;
begin
  Assert(SkipCount >= 0, 'GetFilteredSlice: SkipCount must be non-negative');
  Result:= 0;
  BufLen:= Length(Buffer);
  if BufLen = 0 then EXIT;

  acquireReadLock;
  try
    Skipped:= 0;
    for i:= 0 to FList.Count - 1 do
      begin
        Line:= PLogLine(FList[i]);
        if Line.Level < Verbosity then CONTINUE;
        if Skipped < SkipCount
        then begin Inc(Skipped); CONTINUE; end;
        Buffer[Result]:= Line;
        Inc(Result);
        if Result >= BufLen then EXIT;
      end;
  finally
    releaseReadLock;
  end;
end;


{ Read specific version.
  If Line.ReadFromStream_v5 raises (truncated stream, etc.), Dispose the freshly
  allocated Line to avoid leaking it. Items already added earlier in the loop are
  owned by FList and will be released on Clear/Destroy.

  Uses addInternal (non-virtual, non-locking) instead of the public virtual Add.
  In MT mode, ReadFromStream already holds the write lock for the whole operation,
  so calling Add here would acquire the lock a second time on the same thread —
  see the addInternal declaration comment for the full rationale. }
procedure TAbstractLogLines.readFromStream_v5(Stream: TLightStream);
VAR
   Line: PLogLine;
   iCount, i: Integer;
begin
  iCount := Stream.ReadInteger;

  for i := 0 to iCount - 1 do
  begin
    New(Line);
    TRY
      Line.ReadFromStream_v5(Stream);
    EXCEPT
      Dispose(Line);
      RAISE;
    END;
    addInternal(Line);
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

  Stream.ReadPaddingValidation;
end;


{ Write }
procedure TAbstractLogLines.WriteToStream(Stream: TLightStream);
VAR i: Integer;
begin
  Stream.WriteHeader(StreamSign, CurVer);

  Stream.WriteInteger(FList.Count);
  for i := 0 to FList.Count - 1 do
    PLogLine(FList[i]).WriteToStream(Stream);

  Stream.WritePaddingValidation;
end;



{-------------------------------------------------------------------------------------------------------------
   RLogLine

   We don't write a header and version no for each line because we would waste too much space.
   Instead, the parent (TRamLog) is responsible to do this.
-------------------------------------------------------------------------------------------------------------}
procedure RLogLine.ReadFromStream_v5(Stream: TLightStream);
begin
  Msg    := Stream.ReadString(MaxLogMsgBytes);   { Override default 1*KB ceiling — log messages can be very large. }
  Level  := TLogVerbLvl(Stream.ReadInteger);
  Indent := Stream.ReadInteger;
  Bold   := Stream.ReadBoolean;
  Time   := Stream.ReadDate;
  Stream.ReadPaddingValidation(8);   { Padding }
end;


procedure RLogLine.WriteToStream(Stream: TLightStream);
begin
  Stream.WriteString (Msg);
  Stream.WriteInteger(Ord(Level));
  Stream.WriteInteger(Indent);
  Stream.WriteBoolean(Bold);
  Stream.WriteDate(Time);
  Stream.WritePaddingValidation(8);    { Padding }
end;



end.

