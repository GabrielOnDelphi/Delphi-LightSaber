UNIT LightCore.LogRam;

{=============================================================================================================
   2026.07.07
   www.GabrielMoraru.com
   Last update: 2026.07.07 - Overflow path: the "lost the coalesce race" closure now runs Populate itself
                             before freeing the snapshot (the previous free-only closure could run before
                             the race winner's Populate was even queued -> use-after-free on cached
                             PLogLine pointers). prepareString caps messages at MaxLogMsgChars so saved
                             .logbin files always stay below the reader's 16 MB per-message ceiling.
                2026.05.07 - Bold parameter added to all AddXxx (default FALSE for back-compat; AddBold
                             is now a thin shim over AddInfo(Msg, TRUE)). CheckAndSaveToDisk split into
                             tryOverflowSave + tryPeriodicSave helper procedures (both assume the caller
                             holds FAutoSaveLock; the public CheckAndSaveToDisk is the single entry
                             point that takes/releases the lock). Lines is now a read-only property
                             (was a public field — reassigning it externally would leak the previous
                             instance and break thread-safety). LoadFromFile no longer double-reads the
                             header (single straight-through read). SaveSnapshotToFile uses
                             TRamLog.StreamSign instead of a duplicated literal. ILogObserver.Populate
                             cache-replacement contract documented. DefaultMaxEntries /
                             DefaultSaveInterval moved to public const.
                2026.05.06 - Snapshot dispose race fix (overflow path hands snapshot ownership to
                             the queued Populate closure so the observer's PLogLine cache is
                             refreshed before disposal). GetAsText now atomic via Lines.ForEachLocked.
                2026.05.05 - Coalesced cross-thread observer notifications. POSIX trace channel.
                             RegisterLogObserver double-register guard. GetAsText O(N) via TCStringBuilder.
                             prepareString fast-path for line-break-free messages. Removed redundant
                             GetObserver call in NotifyLogObserverAndShow. MaxEntries / SaveInterval
                             now tunable via public properties. Moved observer lifetime contract
                             next to ILogObserver declaration. Tightened data-loss window comment.
--------------------------------------------------------------------------------------------------------------

   A simple but effective log (non-visual).
   Its data can be displayed by an observer (such as TLogViewer in Light.Common.LogViewer.pas), but it can also work alone without being connected to an observer.
   It can easily hold up to 1 million entries. Being a good citizen, when it reaches this number it saves existing data to disk and then clears it from RAM.

   Verbosity:
     Supports several verbosity levels (verbose, info, warnings, errors, etc)

   Thread Safety:
     The MultiThreaded parameter in Create determines whether the underlying Lines storage is thread-safe.
     When MultiThreaded=TRUE:
       - Individual Add/Clear/Count operations are thread-safe (via TLogLinesMultiThreaded MREWS lock)
       - Observer registration/access is serialized via FObserverLock
       - Auto-save (overflow + periodic) is serialized via FAutoSaveLock to prevent
         concurrent writes to the same file (which would fail with sharing violations)
       - Observer notifications are synchronized to the main thread via TThread.Queue
       - GetAsText holds the read lock for the entire walk via Lines.ForEachLocked

     Observer lifetime contract: see comment immediately above the ILogObserver
     interface declaration in the INTERFACE section.

   Tester:
     LightSaber\Demo\LightLog\
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.DateUtils, System.Classes, System.SyncObjs,
   LightCore.LogLinesAbstract, LightCore.LogLinesS, LightCore.LogLinesM, LightCore.LogTypes, LightCore.StreamBuff;

TYPE
  { Observer lifetime contract (CRITICAL):
      ILogObserver is typically implemented by a TComponent descendant (e.g., TLogViewer).
      TComponent's _AddRef/_Release return -1 — interface references do NOT keep the
      observer alive. The observer MUST:
        1. Call UnregisterLogObserver before its destructor returns, AND
        2. Set a guard flag (e.g., FFormDestroying) and check it in Populate/PopUpWindow
           so any TThread.Queue closures already posted bail out instead of touching
           a freed object.
      See FFormDestroying in LightFmx.Common.LogViewer / LightVcl.Common.LogViewer.

    Populate cache-replacement contract (CRITICAL for the overflow-save path):
      Populate is called from CheckAndSaveToDisk RIGHT BEFORE the snapshot is freed.
      The overflow path correctness depends on Populate REPLACING any cached
      PLogLine pointers (e.g., the FVisibleLines slice in TLogViewer) with fresh
      pointers from the live FRamLog.Lines, SYNCHRONOUSLY before it returns.
      If an implementation defers cache rebuild to a later message-loop tick or to
      an async paint event, the snapshot will be disposed while the viewer is still
      holding pointers into it — use-after-free.
      The reference implementation in LightFmx.Common.LogViewer.TLogViewer.Populate
      calls SetLength(FVisibleLines, ...) + GetFilteredSlice inline, which is the
      pattern any new viewer implementation should follow. }
  ILogObserver = interface
    ['{A1B2C3D4-E5F6-4321-8765-9876543210AB}']
    procedure Populate;
    procedure PopUpWindow;
  end;

  { Coalescing flag for cross-thread observer notifications.

    Why this exists:
      A worker thread emitting thousands of log messages would otherwise post one
      TThread.Queue closure per message, flooding the main-thread queue and forcing
      the visual log to repaint N times. The flag lets the first emitter post a
      single repaint while subsequent emitters silently skip (the pending repaint
      will pick up their entries when it eventually runs).

    Why it lives on the heap behind an interface (not as a TRamLog field):
      The queued closure must NOT capture Self. The existing observer-lifetime
      contract (see TRamLog.Destroy comments) explicitly allows TRamLog to be
      destroyed while a queued closure is still in the main-thread queue —
      the closure relies only on captured locals. If the flag were a Self field,
      resetting it from the closure would be use-after-free.
      An interface gives the closure its own refcount on the flag's storage,
      independent of TRamLog's lifetime. }
  INotifyCoalesceFlag = interface
    ['{B2C3D4E5-F6A7-4321-8765-9876543210CD}']
    function  TryAcquire: Boolean;   { Atomic 0->1 transition. TRUE = caller won and must queue the repaint; FALSE = a repaint is already pending, skip. }
    procedure Release;               { Atomic reset to 0. Called from inside the queued closure on the main thread, BEFORE Populate, so emissions during paint can re-queue. }
  end;

  TRamLog = class(TObject)
   private
     FLines        : TAbstractLogLines;  // Backing storage exposed via the read-only Lines property below. Allocated in Create; freed in Destroy.
     FLogObserver  : ILogObserver;       // GUI element that observes this log
     FObserverLock : TCriticalSection;   // Serializes FLogObserver register/unregister/read in MT mode
     FAutoSaveLock : TCriticalSection;   // Serializes overflow+periodic disk saves (prevents file sharing violations)
     FLastSaveTime : TDateTime;
     FSaveInterval : Integer;            // Interval in seconds between periodic disk saves. Default 60s. Tunable via SaveInterval property.
     FMaxEntries   : Integer;            // Maximum number of entries before overflow-save+clear. Default 1_000_000. Tunable via MaxEntries property.
     FNotifyFlag   : INotifyCoalesceFlag; // Coalesces background-thread observer notifications. See INotifyCoalesceFlag for full rationale. Written only by the constructor; read concurrently by every Add path.
     const
       StreamSign         = 'TRamLog';   { Header signature for binary save/load. Unit-private — accessed by SaveSnapshotToFile in the same unit via Delphi's unit-scope visibility. }
     procedure setMaxEntries  (Value: Integer);
     procedure setSaveInterval(Value: Integer);
   protected
     function prepareString(CONST Msg: string): string;
     procedure CheckAndSaveToDisk;
     procedure tryOverflowSave;     { Helper for CheckAndSaveToDisk; assumes FAutoSaveLock is held. }
     procedure tryPeriodicSave;     { Helper for CheckAndSaveToDisk; assumes FAutoSaveLock is held. }
     function GetObserver: ILogObserver;  { Thread-safe FLogObserver read }
   public
     const
       DefaultMaxEntries  = 1000000;     // Default value for FMaxEntries — used in constructor and exposed for callers that want to compare/restore.
       DefaultSaveInterval= 60;          // Default value for FSaveInterval (seconds).
     var
       ShowOnError: Boolean; // Automatically show the visual log form when warnings/errors are added to the log. This way the user is informed about the problems.
     constructor Create(aShowOnError: Boolean; Observer: ILogObserver; MultiThreaded: Boolean= FALSE);
     destructor Destroy;  override;
     procedure Clear;

     function Count(Filtered: Boolean; Filter: TLogVerbLvl): Integer;

     procedure AddBold    (CONST Msg: string);
     procedure AddMsg     (CONST Msg: string; Bold: Boolean = FALSE);
     procedure AddMsgInt  (CONST Msg: string; i: Integer; Bold: Boolean = FALSE);
     procedure AddEmptyRow;

     procedure AddDebug   (CONST Msg: string; Bold: Boolean = FALSE);
     procedure AddVerb    (CONST Msg: string; Bold: Boolean = FALSE);
     procedure AddHint    (CONST Msg: string; Bold: Boolean = FALSE);
     procedure AddInfo    (CONST Msg: string; Bold: Boolean = FALSE);     { Default level }
     procedure AddImpo    (CONST Msg: string; Bold: Boolean = FALSE);
     procedure AddWarn    (CONST Msg: string; Bold: Boolean = FALSE);
     procedure AddError   (CONST Msg: string; Bold: Boolean = FALSE);
     {}
     function  GetAsText: string;
     procedure SaveAsText  (CONST FullPath: string);
     {}
     function  LoadFromStream(Stream: TLightStream): Boolean;
     procedure SaveToStream  (Stream: TLightStream);

     procedure SaveToFile  (CONST FullPath: string);
     function  LoadFromFile(CONST FullPath: string): Boolean;

    // OBSERVER
    procedure RegisterLogObserver(Observer: ILogObserver);
    procedure UnregisterLogObserver;
    procedure NotifyLogObserver;
    procedure NotifyLogObserverAndShow;

    procedure PopUpWindow;

    { Read-only access to the underlying line storage.
      Was a public field before 2026.05; reassigning it externally would leak the
      previous instance and break the thread-safety guarantees set up in Create.
      Callers may freely call Lines.Count / Lines[i] / Lines.GetFilteredSlice / etc.,
      but they cannot replace the storage object — use Clear or LoadFromFile for that. }
    property Lines: TAbstractLogLines read FLines;

    { Tunable thresholds — sane defaults set in the constructor; override at any time.
      MaxEntries  : maximum number of in-RAM entries before the next AddXxx triggers an
                    overflow save (to LargeLogSave.logbin) and clears RAM. Lower this on
                    memory-constrained devices; raise it if you want to keep more history
                    in RAM for GetAsText/SaveAsText operations.
      SaveInterval: seconds between periodic crash-recovery saves to PeriodicLogSave.logbin.
                    Lower for tighter durability; raise to reduce disk traffic.
      Setters Assert a positive value — zero or negative would trigger the corresponding
      save-and-clear on every Add, which is almost certainly a caller mistake. }
    property MaxEntries  : Integer read FMaxEntries   write setMaxEntries;
    property SaveInterval: Integer read FSaveInterval write setSaveInterval;
  end;


IMPLEMENTATION

USES
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  LightCore, LightCore.TextFile, LightCore.AppData, LightCore.StrBuilder;

CONST
  CurrentVersion = 5;

  { Hard ceiling on a single stored message, in UTF-16 chars.
    The binary READER (RLogLine.ReadFromStream_v5 in LightCore.LogLinesAbstract) rejects any
    message whose UTF-8 length exceeds MaxLogMsgBytes = 16 MB, but WriteString is unbounded —
    a longer message would produce a .logbin that can never be loaded back (every LoadFromFile
    raises 'String too large'), silently poisoning the crash-recovery files.
    Worst-case UTF-16 -> UTF-8 expansion is 3 bytes per code unit, so 4M chars <= 12 MB —
    comfortably under the reader's 16 MB ceiling. }
  MaxLogMsgChars = 4*1024*1024;


{ Trace channel for save-path failures.
  Cannot use AppDataCore.LogError here -- that recurses through TRamLog.AddXxx, potentially
  re-entering CheckAndSaveToDisk and causing infinite recursion if disk I/O is failing.

  Platform routing:
    Windows: OutputDebugString — visible in DebugView/IDE Event Log.
    POSIX  : Writeln(ErrOutput) — goes to stderr, visible in Console.app (macOS),
             logcat for stdout/stderr-bridged Android, or the terminal on desktop Linux.
             Wrapped in try/except because ErrOutput may be unavailable in a GUI app
             with no console attached; we prefer silent loss of the trace over
             crashing the save path. }
procedure TraceSaveError(const Msg: string);
begin
  {$IFDEF MSWINDOWS}
  OutputDebugString(PChar('TRamLog: ' + Msg));
  {$ELSE}
  try
    Writeln(ErrOutput, 'TRamLog: ', Msg);
  except
    { No console / closed handle / etc. — swallow; this is best-effort diagnostics. }
  end;
  {$ENDIF}
end;


type
  { Concrete INotifyCoalesceFlag.

    State machine:
      0 (idle)   -> TryAcquire by any thread -> 1 (queued).  Winner posts a closure.
      1 (queued) -> TryAcquire by anybody    -> stays 1.     Loser skips; the already-
                                                             queued closure represents them.
      1 (queued) -> Release from closure     -> 0 (idle).    Done before Populate.

    Why a lock-free CAS instead of a TCriticalSection:
      The hot path is "every background-thread Add". A critical section would serialize
      all emitters on every call; CAS is a single LOCK CMPXCHG and lets all emitters
      proceed in parallel — only the (rare) winner does extra work.

    Why TInterlocked (not direct AtomicCmpExchange):
      Project style — TInterlocked is the documented public API. The intrinsic call
      compiles to the same instruction.

    Lifetime:
      Held by both TRamLog (via FNotifyFlag) and any in-flight closure (via captured
      LFlag local). When TRamLog is destroyed before the closure runs, the field is
      auto-released and the closure's local keeps the object alive until it finishes. }
  TNotifyCoalesceFlag = class(TInterfacedObject, INotifyCoalesceFlag)
  private
    FState: Integer;   { 0 = idle, 1 = repaint queued or running. Manipulated only via TInterlocked. }
  public
    function  TryAcquire: Boolean;
    procedure Release;
  end;

{ Atomic 0->1. CompareExchange returns the OLD value of FState; old=0 means we won
  the race and must queue. Old=1 means another thread already queued — skip. }
function TNotifyCoalesceFlag.TryAcquire: Boolean;
begin
  Result:= TInterlocked.CompareExchange(FState, 1, 0) = 0;
end;

{ Atomic 1->0 (or 0->0; idempotent). Called from the queued closure BEFORE Populate
  runs so any Add that arrives mid-paint sees state=0 and re-queues a fresh repaint —
  guarantees no entry is silently dropped from the visual. }
procedure TNotifyCoalesceFlag.Release;
begin
  TInterlocked.Exchange(FState, 0);
end;


{ Saves a snapshot list (typically from SnapshotAndClear) to disk.
  Used by the overflow path so we save the snapshot — not the live list — and avoid
  the data-loss window between SaveToFile and a subsequent Clear.
  The snapshot is owned by the caller; this routine only reads from it.
  Forward-declared above CheckAndSaveToDisk; full body lives near the other I/O routines. }
procedure SaveSnapshotToFile(Snapshot: TAbstractLogLines; const FullPath: string); forward;


{-------------------------------------------------------------------------------------------------------------
   CONSTRUCTOR / DESTRUCTOR

   Parameters:
     aShowOnError  - When TRUE, automatically shows the observer window (if registered) when warnings/errors are logged
     Observer      - Optional GUI observer (like TLogViewer) that displays log entries. Can be NIL.
     MultiThreaded - When TRUE, uses TLogLinesMultiThreaded with MREWS locking for thread-safe operations.
                     When FALSE (default), uses TLogLinesSingleThreaded without locking overhead.
-------------------------------------------------------------------------------------------------------------}
constructor TRamLog.Create(aShowOnError: Boolean; Observer: ILogObserver; MultiThreaded: Boolean= FALSE);
begin
  inherited Create;

  ShowOnError:= aShowOnError;
  FSaveInterval:= DefaultSaveInterval;
  FMaxEntries  := DefaultMaxEntries;
  FLastSaveTime:= Now;

  FObserverLock:= TCriticalSection.Create;
  FAutoSaveLock:= TCriticalSection.Create;
  FNotifyFlag  := TNotifyCoalesceFlag.Create;   { Created here; never reassigned. Auto-released in Destroy via interface refcount. }

  if MultiThreaded
  then FLines:= TLogLinesMultiThreaded.Create
  else FLines:= TLogLinesSingleThreaded.Create;

  if Observer <> NIL
  then RegisterLogObserver(Observer);
end;


destructor TRamLog.Destroy;
begin
  { Detach observer reference. NOTE: Already-queued TThread.Queue closures captured
    the interface locally (see NotifyLogObserver/PopUpWindow) and will still fire —
    they rely on the observer's own lifetime guard (e.g., FFormDestroying in TLogViewer).
    See "Observer lifetime contract" in the unit header. }
  if Assigned(FObserverLock) then
    begin
      FObserverLock.Enter;
      try
        FLogObserver:= NIL;
      finally
        FObserverLock.Leave;
      end;
    end;

  FreeAndNil(FLines);
  FreeAndNil(FObserverLock);
  FreeAndNil(FAutoSaveLock);
  inherited;
end;


procedure TRamLog.Clear;
begin
  Lines.Clear;   { Call Clear to empty the Items array and set the Count to 0. Clear also frees the memory used to store the Items array and sets the Capacity to 0. }
  NotifyLogObserver;
end;




{-------------------------------------------------------------------------------------------------------------
   STUFF
-------------------------------------------------------------------------------------------------------------}

{ Returns the number of log lines.
  Filtered=False: Returns total count.
  Filtered=True: Returns count of lines meeting the verbosity threshold.
  Uses Lines.CountFiltered which is thread-safe (holds lock during iteration). }
function TRamLog.Count(Filtered: Boolean; Filter: TLogVerbLvl): Integer;
begin
  if Filtered
  then Result:= Lines.CountFiltered(Filter)
  else Result:= Lines.Count;
end;


{ Tunable thresholds — setters Assert positivity to catch caller mistakes early.
  No lock taken: a stale read by CheckAndSaveToDisk on the next Add is harmless
  (worst case: one extra or skipped save under FAutoSaveLock serialization). }
procedure TRamLog.setMaxEntries(Value: Integer);
begin
  Assert(Value > 0, 'TRamLog.MaxEntries must be > 0. A non-positive value would trigger an overflow save on every Add.');
  FMaxEntries:= Value;
end;

procedure TRamLog.setSaveInterval(Value: Integer);
begin
  Assert(Value > 0, 'TRamLog.SaveInterval must be > 0 (seconds). A non-positive value would trigger a periodic save on every Add.');
  FSaveInterval:= Value;
end;


{-------------------------------------------------------------------------------------------------------------
   OBSERVER METHODS

   Thread safety:
     FLogObserver is an interface field. Assigning/reading it in MT mode without a
     lock can tear (especially on platforms where interface assignment is not a
     single atomic instruction) and can race with concurrent _AddRef/_Release.
     FObserverLock serializes all access.

   Lifetime:
     The observer is typically a TComponent (TLogViewer descends from TPanel/TStringGrid).
     TComponent's _AddRef returns -1 — the captured interface in TThread.Queue closures
     does NOT keep the observer alive. The observer must call UnregisterLogObserver
     before its destructor returns AND maintain a "destroying" guard flag that its
     Populate/PopUpWindow methods check. See unit header for full contract.
-------------------------------------------------------------------------------------------------------------}

{ Thread-safe read of FLogObserver. Returns NIL if no observer is registered.
  The returned interface holds an _AddRef on the observer's iface slot, but for a
  TComponent that is a no-op — the observer's true lifetime is owned by its parent. }
function TRamLog.GetObserver: ILogObserver;
begin
  FObserverLock.Enter;
  try
    Result:= FLogObserver;
  finally
    FObserverLock.Leave;
  end;
end;


{ Registers a GUI observer (like TLogViewer) to receive log updates.
  Only one observer can be registered at a time.

  Double-register guard:
    Assert fires in DEBUG builds if a second observer registers without the first
    having called UnregisterLogObserver. Silently overwriting is dangerous: the
    previous observer keeps thinking it is attached but receives no notifications,
    and worse, its destructor's UnregisterLogObserver call may then nil out the
    NEW (legitimate) observer. Catching the misuse early is cheap. In RELEASE
    builds the assertion is compiled out and the new observer wins, preserving
    legacy behavior.

  IMPORTANT: The caller (typically a TComponent) is responsible for calling
  UnregisterLogObserver in its destructor — interface refcounting does NOT
  manage the observer's lifetime. }
procedure TRamLog.RegisterLogObserver(Observer: ILogObserver);
begin
  Assert(Observer <> NIL, 'RegisterLogObserver: Observer cannot be nil. Call UnregisterLogObserver to detach.');
  FObserverLock.Enter;
  try
    Assert((FLogObserver = NIL) or (FLogObserver = Observer),
           'RegisterLogObserver: an observer is already registered. Call UnregisterLogObserver on the previous one first.');
    FLogObserver:= Observer;
  finally
    FObserverLock.Leave;
  end;
end;


procedure TRamLog.UnregisterLogObserver;
begin
  FObserverLock.Enter;
  try
    FLogObserver:= NIL;
  finally
    FObserverLock.Leave;
  end;
end;


{ Notify the registered observer that the log has changed.

  Main-thread caller:
    Direct synchronous call to LObserver.Populate. Cheapest path.

  Background-thread caller:
    Posts a closure to the main thread via TThread.Queue. Two design constraints:

    1. The closure must NOT capture Self.
       The observer-lifetime contract (see TRamLog.Destroy) explicitly allows
       TRamLog to be destroyed while a queued closure is still in the main-thread
       queue. Touching any field of Self from inside the closure would be a
       use-after-free. So both pieces of state the closure needs — the observer
       and the coalescing flag — are captured as local interface variables;
       neither dereferences Self.

    2. The captured ILogObserver does NOT extend the observer's lifetime.
       The observer is typically a TComponent (TLogViewer) whose _AddRef is a
       no-op. The observer must guard against late callbacks itself via a
       FFormDestroying flag (or similar) and call UnregisterLogObserver before
       its destructor returns. See unit header for the full contract.

  Coalescing:
    A worker emitting thousands of messages would otherwise post thousands of
    closures, each forcing a full Populate. We collapse the burst to a single
    repaint via INotifyCoalesceFlag — see that interface's declaration for the
    full rationale and state machine. }
procedure TRamLog.NotifyLogObserver;
VAR
  LObserver: ILogObserver;
  LFlag    : INotifyCoalesceFlag;
begin
  LObserver:= GetObserver;
  if NOT Assigned(LObserver) then EXIT;

  if TThread.CurrentThread.ThreadID = MainThreadID
  then LObserver.Populate                            { Direct call: no queue, no closure, no coalescing needed }
  else
    begin
      { Cross-thread path. Both LObserver and LFlag are captured by the closure.
        Critically, the closure references ONLY these locals — it does NOT touch
        any field of Self. That keeps the closure safe to run after TRamLog is
        destroyed (see INotifyCoalesceFlag declaration for full rationale). }
      LFlag:= FNotifyFlag;
      if LFlag.TryAcquire   { Won the 0->1 race? Then we are the one repaint for this burst. }
      then TThread.Queue(NIL, procedure
        begin
          { Release BEFORE Populate so any Add() that fires while Populate is
            running will see state=0, win its own TryAcquire, and schedule a
            follow-up repaint that captures the new entries. Releasing AFTER
            Populate would silently drop those mid-paint entries from the visual. }
          LFlag.Release;
          LObserver.Populate;
        end);
    end;
end;


{ Convenience for warning/error paths: notify the observer of new content, then
  pop the window into view if ShowOnError is set.

  No early-out via GetObserver here — both NotifyLogObserver and PopUpWindow
  already guard themselves with their own GetObserver/Assigned check, so a
  pre-check would just acquire FObserverLock a redundant third time. }
procedure TRamLog.NotifyLogObserverAndShow;
begin
  NotifyLogObserver;
  if ShowOnError
  then PopUpWindow;
end;


procedure TRamLog.PopUpWindow;
VAR LObserver: ILogObserver;
begin
  LObserver:= GetObserver;
  if NOT Assigned(LObserver) then EXIT;

  if TThread.CurrentThread.ThreadID = MainThreadID
  then LObserver.PopUpWindow
  else TThread.Queue(NIL, procedure begin LObserver.PopUpWindow; end);
end;








{-------------------------------------------------------------------------------------------------------------
   ADD GENERIC MESSAGE
-------------------------------------------------------------------------------------------------------------}

{ Convenience for the lvInfos + Bold=TRUE pattern. Equivalent to AddInfo(Msg, TRUE).
  Kept as a distinct entry point for readability ("AddBold" telegraphs intent more
  clearly than "AddInfo with a Boolean") and for backward compatibility with existing callers. }
procedure TRamLog.AddBold(CONST Msg: string);
begin
  AddInfo(Msg, TRUE);
end;


procedure TRamLog.AddMsg(CONST Msg: string; Bold: Boolean = FALSE);
begin
  Lines.AddNewLine(PrepareString(Msg), lvInfos, Bold);
  CheckAndSaveToDisk;
  NotifyLogObserver;
end;


procedure TRamLog.AddMsgInt(CONST Msg: string; i: Integer; Bold: Boolean = FALSE);   { Adds a message text followed by an integer }
begin
  Lines.AddNewLine(PrepareString(Msg) + IntToStr(i), lvInfos, Bold);
  CheckAndSaveToDisk;
  NotifyLogObserver;
end;


procedure TRamLog.AddEmptyRow;
begin
  AddMsg('');
end;



{-------------------------------------------------------------------------------------------------------------
   ADD MESSAGE BY VERBOSITY
-------------------------------------------------------------------------------------------------------------}
procedure TRamLog.AddDebug(CONST Msg: string; Bold: Boolean = FALSE);       { Relevance 0 }
begin
  Lines.AddNewLine(PrepareString(Msg), lvDebug, Bold);
  CheckAndSaveToDisk;
  NotifyLogObserver;
end;


procedure TRamLog.AddVerb(CONST Msg: string; Bold: Boolean = FALSE);        { Relevance 1 }
begin
  Lines.AddNewLine(PrepareString(Msg), lvVerbose, Bold);
  CheckAndSaveToDisk;
  NotifyLogObserver;
end;


procedure TRamLog.AddHint(CONST Msg: string; Bold: Boolean = FALSE);        { Relevance 2 }
begin
  Lines.AddNewLine(PrepareString(Msg), lvHints, Bold);
  CheckAndSaveToDisk;
  NotifyLogObserver;
end;


procedure TRamLog.AddInfo(CONST Msg: string; Bold: Boolean = FALSE);        { Relevance 3 }
begin
  Lines.AddNewLine(PrepareString(Msg), lvInfos, Bold);
  CheckAndSaveToDisk;
  NotifyLogObserver;
end;


procedure TRamLog.AddImpo(CONST Msg: string; Bold: Boolean = FALSE);        { Relevance 4 }
begin
  Lines.AddNewLine(PrepareString(Msg), lvImportant, Bold);
  CheckAndSaveToDisk;
  NotifyLogObserver;
end;


procedure TRamLog.AddWarn(CONST Msg: string; Bold: Boolean = FALSE);        { Relevance 5 }
begin
  Lines.AddNewLine(PrepareString(Msg), lvWarnings, Bold);
  CheckAndSaveToDisk;
  NotifyLogObserverAndShow;
end;


procedure TRamLog.AddError(CONST Msg: string; Bold: Boolean = FALSE);       { Relevance 6 }
begin
  Lines.AddNewLine(PrepareString(Msg), lvErrors, Bold);
  CheckAndSaveToDisk;
  NotifyLogObserverAndShow;
end;


{-------------------------------------------------------------------------------------------------------------
   CHECK AND SAVE TO DISK

   Auto-save triggers:
     1. When log exceeds MaxEntries (default 1 million; tunable via MaxEntries property) -
        saves to LargeLogSave.logbin and clears RAM.
     2. When FSaveInterval seconds have passed since last save (default 60s; tunable via
        SaveInterval property) - saves to PeriodicLogSave.logbin.

   Concurrency model (MT mode):
     FAutoSaveLock.TryEnter ensures only ONE thread runs the save logic at a time.
     Other threads skip the save and let entries accumulate briefly until the next call.
     This prevents:
       - Concurrent writes to the same file (TLightStream.CreateWrite uses dwShareMode=0,
         which makes overlapping CreateFileW calls fail with ERROR_SHARING_VIOLATION)
       - Multiple threads each calling SnapshotAndClear (data integrity)

   Atomic snapshot+clear (overflow path):
     SnapshotAndClear performs the swap under exclusive lock, so no entries can be
     added between the snapshot capture and the list-clear. The snapshot is then
     written to disk while the live list (now empty) is free to accept new entries
     concurrently — no data-loss window between persist and clear.

   Exception handling (project rule: log+reraise, never silently swallow):
     Disk-full / permission / sharing failures are common, expected failure modes
     for a backup save — they get logged via OutputDebugString and swallowed so the
     calling AddXxx returns normally. Unknown exception classes are re-raised per
     project rules — they may indicate real bugs (AV, EOutOfMemory, etc.) that
     must surface.

   Note: FLastSaveTime access is intentionally not locked. Worst case is an extra
   save or a skipped interval (both harmless under FAutoSaveLock serialization).
-------------------------------------------------------------------------------------------------------------}
procedure TRamLog.CheckAndSaveToDisk;
begin
  if NOT FAutoSaveLock.TryEnter then EXIT;   { Another thread is already saving — skip. }
  TRY
    tryOverflowSave;
    tryPeriodicSave;
  FINALLY
    FAutoSaveLock.Leave;
  END;
end;


{ Overflow save: atomic snapshot+clear, then write the snapshot to disk. Caller
  (CheckAndSaveToDisk) holds FAutoSaveLock for the duration.

  Observer cache-dangling fix:
    TLogViewer caches PLogLine pointers in its FVisibleLines (populated by
    GetFilteredSlice during Populate). Those pointers are owned by Self.Lines
    until SnapshotAndClear transfers them to Snapshot. If we freed Snapshot
    before the queued Populate ran, a paint between Free and Populate would
    dereference disposed memory. We hand Snapshot ownership to the queued closure
    so disposal happens AFTER Populate has refreshed the observer's pointer cache
    with fresh pointers from the (now empty / refilled) live list.

  Closure capture rules: same as NotifyLogObserver — capture LObserver and LFlag
  as locals; never reference Self from inside the closure (TRamLog may be destroyed
  before the closure fires). See INotifyCoalesceFlag declaration for full rationale. }
procedure TRamLog.tryOverflowSave;
VAR
  Snapshot: TAbstractLogLines;
  HandedOff: Boolean;
  LObserver: ILogObserver;
  LFlag    : INotifyCoalesceFlag;
begin
  if Lines.Count <= FMaxEntries then EXIT;

  Snapshot:= Lines.SnapshotAndClear;   { Atomic — no data can be added between capture and clear }
  HandedOff:= FALSE;
  TRY
    TRY
      SaveSnapshotToFile(Snapshot, TAppDataCore.AppDataFolder + 'LargeLogSave.logbin');
    EXCEPT
      on E: EFCreateError do TraceSaveError('Overflow save (file create) failed: ' + E.Message);
      on E: EWriteError   do TraceSaveError('Overflow save (write) failed: '       + E.Message);
      on E: EStreamError  do TraceSaveError('Overflow save (stream) failed: '      + E.Message);
      on E: Exception     do
        begin
          TraceSaveError('Overflow save (unexpected ' + E.ClassName + '): ' + E.Message);
          RAISE;   { Re-raise unknown exception types per project policy }
        end;
    END;

    LObserver:= GetObserver;
    if NOT Assigned(LObserver)
    then begin
      { No observer to refresh — safe to free the snapshot in the FINALLY below;
        nobody is caching its pointers. }
    end
    else if TThread.CurrentThread.ThreadID = MainThreadID
    then LObserver.Populate                            { Synchronous — refresh is done before we return; safe to free in FINALLY. }
    else
      begin
        LFlag:= FNotifyFlag;
        { Mark HandedOff:=TRUE BEFORE TThread.Queue. If Queue raises (e.g., OOM),
          the FINALLY's free path is suppressed — we'd rather leak Snapshot than
          risk a double-free if Queue partially succeeded. }
        if LFlag.TryAcquire
        then begin
          { Hand ownership of Snapshot to the closure. The closure runs Populate
            (refreshes the cache via GetFilteredSlice) and then frees the snapshot. }
          HandedOff:= TRUE;
          TThread.Queue(NIL, procedure
            begin
              LFlag.Release;
              TRY
                LObserver.Populate;
              FINALLY
                FreeAndNil(Snapshot);   { Free AFTER Populate has refreshed the observer's pointer cache. }
              END;
            end);
        end
        else
          begin
            { Lost the coalesce race: the flag is 1, so another repaint closure was
              ACQUIRED — but we cannot assume it is already IN the queue. The winner
              sets the flag and only then calls TThread.Queue (two separate steps in
              NotifyLogObserver); it can be preempted between them for the entire
              duration of our SnapshotAndClear + disk save. A bare "free the snapshot"
              closure queued now could therefore run BEFORE the winner's Populate lands,
              disposing records the observer still caches (FGrid.Objects / FVisibleLines)
              — use-after-free on the next paint.

              So this closure refreshes the observer's pointer cache ITSELF, then frees —
              same pattern as the winning branch, minus the flag Release (we do not own
              the flag; the winner will Release it). The extra Populate is harmless:
              overflow fires once per MaxEntries adds, so coalescing it buys nothing. }
            HandedOff:= TRUE;
            TThread.Queue(NIL, procedure
              begin
                TRY
                  LObserver.Populate;
                FINALLY
                  FreeAndNil(Snapshot);   { Free AFTER our own Populate refreshed the observer's pointer cache. }
                END;
              end);
          end;
      end;
  FINALLY
    if NOT HandedOff
    then FreeAndNil(Snapshot);
  END;
end;


{ Periodic save: dumps current state for crash recovery (no clear). Caller
  (CheckAndSaveToDisk) holds FAutoSaveLock for the duration.
  FLastSaveTime is intentionally not locked — worst case is an extra save or a
  skipped interval, both harmless under FAutoSaveLock serialization. }
procedure TRamLog.tryPeriodicSave;
begin
  if SecondsBetween(Now, FLastSaveTime) < FSaveInterval then EXIT;

  TRY
    SaveToFile(TAppDataCore.AppDataFolder + 'PeriodicLogSave.logbin');
    FLastSaveTime:= Now;
  EXCEPT
    on E: EFCreateError do TraceSaveError('Periodic save (file create) failed: ' + E.Message);
    on E: EWriteError   do TraceSaveError('Periodic save (write) failed: '       + E.Message);
    on E: EStreamError  do TraceSaveError('Periodic save (stream) failed: '      + E.Message);
    on E: Exception     do
      begin
        TraceSaveError('Periodic save (unexpected ' + E.ClassName + '): ' + E.Message);
        RAISE;   { Re-raise unknown exception types per project policy }
      end;
  END;
end;


{-------------------------------------------------------------------------------------------------------------
   TEXT
-------------------------------------------------------------------------------------------------------------}

{ Returns all log lines as a single string, separated by CRLF.

  Performance:
    Uses TCStringBuilder.AddString (bulk Move + doubling growth) for amortized O(N)
    cost. The previous implementation built Result via repeated `Result := Result + X`,
    which is O(N^2): each concat allocates a fresh string and copies the entire
    accumulated content. For a 1M-line log that was effectively unfinishable.

  CRLF placement:
    We emit CRLF *before* every line except the first instead of *after* every line
    plus a trailing trim. Saves one full-string copy (RemoveLastEnter) at the end.

  Thread Safety:
    Walks via Lines.ForEachLocked, which holds the read lock for the entire
    iteration. No TOCTOU window — the snapshot is consistent even if other
    threads are calling AddXxx concurrently. Background Add* calls block briefly
    behind the read lock; keep the resulting string short-lived if you call
    GetAsText on a hot path.

    No separate Lines.Count call. Pre-sizing the builder would require a second
    lock acquisition for a value that may be stale by the time ForEachLocked
    starts walking. TCStringBuilder's default 10000-char initial capacity plus
    doubling growth handles arbitrary sizes — the savings of a perfect pre-size
    are not worth the extra lock round-trip. }
function TRamLog.GetAsText: string;
VAR
  SB: TCStringBuilder;
  First: Boolean;
begin
  SB:= TCStringBuilder.Create;   { Default capacity; doubling growth absorbs any size. }
  TRY
    First:= TRUE;
    Lines.ForEachLocked(procedure (Line: PLogLine)
      begin
        if First
        then First:= FALSE
        else SB.AddEnter;          { CRLF separator BEFORE every line except the first }
        SB.AddString(Line.Msg);
      end);
    Result:= SB.AsText;
  FINALLY
    FreeAndNil(SB);
  END;
end;



{ Prepares a message string for storage by replacing all line breaks with spaces.
  This ensures each log entry is a single line, which simplifies display and filtering.

  Fast path:
    Most log messages contain no line breaks. ReplaceEnters always runs three
    StringReplace passes (CRLF, CR, LF), each allocating a fresh copy of the
    string even when nothing matches. We pre-scan for line-break characters
    and short-circuit when none are present — saves three full-string scans
    plus three allocations on every Add* call in the common case. }
function TRamLog.prepareString(CONST Msg: string): string;
begin
  if (Pos(#10, Msg) = 0) and (Pos(#13, Msg) = 0)
  then Result:= Msg                         { No line breaks — return verbatim, no allocation }
  else Result:= ReplaceEnters(Msg, ' ');    { Has line breaks — use the full replacement path }

  { Cap the stored length so the binary save stays loadable — see MaxLogMsgChars. }
  if Length(Result) > MaxLogMsgChars then
    begin
      SetLength(Result, MaxLogMsgChars);
      { Don't cut a surrogate pair in half — drop a trailing lone high surrogate. }
      if (Result[MaxLogMsgChars] >= #$D800) and (Result[MaxLogMsgChars] <= #$DBFF)
      then SetLength(Result, MaxLogMsgChars-1);
      Result:= Result + ' [truncated]';
    end;
end;








{-------------------------------------------------------------------------------------------------------------
    I/O
-------------------------------------------------------------------------------------------------------------}
procedure TRamLog.SaveAsText(CONST FullPath: string);
begin
  StringToFile(FullPath, GetAsText, woOverwrite, wpAuto);
end;



{ Loads log data from a stream.
  Returns TRUE if the stream was successfully read (correct signature and version).
  Returns FALSE if the stream has an incompatible version or invalid signature.

  IMPORTANT: This method clears any existing entries before loading, so the resulting
  state is the loaded data only — symmetric with LoadFromFile. If you want to append
  loaded entries to an existing list, call Lines.ReadFromStream directly.

  Stream format (nested structure):
    TRamLog header ('TRamLog', CurrentVersion)
      TLogLines header ('TLogLines', CurVer)
        [Log line records...]
      TLogLines padding
    TRamLog padding }
function TRamLog.LoadFromStream(Stream: TLightStream): Boolean;
VAR StreamVer: Word;
begin
  StreamVer:= Stream.ReadHeader(StreamSign);
  Result:= StreamVer = CurrentVersion;
  if Result then
    begin
      Lines.Clear;                         { Replace, not append — matches LoadFromFile }
      Lines.ReadFromStream(Stream);
      Stream.ReadPaddingValidation;
    end;
end;


{ Saves all log data to a stream in binary format.
  See LoadFromStream for stream format documentation. }
procedure TRamLog.SaveToStream(Stream: TLightStream);
begin
  Stream.WriteHeader(StreamSign, CurrentVersion);
  Lines.WriteToStream(Stream);
  Stream.WritePaddingValidation;
end;


{ Saves a snapshot list (typically from SnapshotAndClear) to disk.
  Used by the overflow path so we save the snapshot — not the live list — and avoid
  the data-loss window between SaveToFile and a subsequent Clear.
  The snapshot is owned by the caller; this routine only reads from it.
  Mirrors the TRamLog stream format (TRamLog.StreamSign header + nested TLogLines block).
  Uses TRamLog.StreamSign directly so a future rename of the signature stays in one place. }
procedure SaveSnapshotToFile(Snapshot: TAbstractLogLines; const FullPath: string);
begin
  Assert(Snapshot <> NIL, 'SaveSnapshotToFile: Snapshot cannot be nil');

  VAR Stream:= TLightStream.CreateWrite(FullPath);
  TRY
    Stream.WriteHeader(TRamLog.StreamSign, CurrentVersion);
    Snapshot.WriteToStream(Stream);
    Stream.WritePaddingValidation;
  FINALLY
    FreeAndNil(Stream);
  END;
end;


{ Loads log data from a binary file.
  Returns TRUE on success, FALSE if file doesn't exist or has incompatible format.

  Validates the header BEFORE clearing existing entries. If the file is missing or
  has the wrong signature/version, the existing in-memory log is left untouched.

  Implementation: reads the header in-place (no rewind) — passing the validated
  header check straight to the body read. The earlier version peeked the header,
  rewound to position 0, and let LoadFromStream re-read the same header — wasted
  I/O and left a window where the second read could see different bytes if the file
  was being concurrently rewritten. }
function TRamLog.LoadFromFile(const FullPath: string): Boolean;
VAR
  StreamVer: Word;
begin
  Result:= FALSE;
  if NOT FileExists(FullPath) then EXIT;

  VAR Stream:= TLightStream.CreateRead(FullPath);
  TRY
    StreamVer:= Stream.ReadHeader(StreamSign);
    if StreamVer <> CurrentVersion then EXIT;   { Bad signature/version — leave existing in-memory log untouched. }

    FLines.Clear;                          { Replace, not append — matches LoadFromStream semantics. }
    FLines.ReadFromStream(Stream);
    Stream.ReadPaddingValidation;
    Result:= TRUE;
  FINALLY
    FreeAndNil(Stream);
  END;

  if Result then NotifyLogObserver;
end;


{ Saves all log data to a binary file. Creates the file if it doesn't exist. }
procedure TRamLog.SaveToFile(const FullPath: string);
begin
 VAR Stream:= TLightStream.CreateWrite(FullPath);
 TRY
   SaveToStream(Stream);
 FINALLY
   FreeAndNil(Stream);
 END;
end;


end.
