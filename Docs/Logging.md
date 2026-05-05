# LightSaber Logging

Source of truth for the LightSaber logging subsystem. Code-level invariants live in `LightCore.LogRam.pas` and the LogLines units; this file is the architectural overview a caller needs before touching any of it.

## Architecture (3 layers)

```
TRamLog                         (LightCore.LogRam.pas)        - non-visual log
  |
  +-- Lines: TAbstractLogLines  (LightCore.LogLinesAbstract.pas) - storage abstraction
  |     |
  |     +-- TLogLinesSingleThreaded  (LightCore.LogLinesS.pas)   - lock-free
  |     +-- TLogLinesMultiThreaded   (LightCore.LogLinesM.pas)   - MREWS-locked
  |
  +-- ILogObserver              - GUI viewer attaches here
        |
        +-- TLogViewer (VCL)    (FrameVCL/LightVcl.Common.LogViewer.pas)
        +-- TLogViewer (FMX)    (FrameFMX/LightFmx.Common.LogViewer.pas)
```

`TRamLog` holds the messages and broadcasts changes to one observer. The visual viewers implement `ILogObserver` and render the data on demand.

## Quick start

The canonical entry point is `AppData.RamLog`:

```pascal
uses LightCore.AppData;            // for AppDataCore
// or LightFmx.Common.AppData;     // FMX

AppData.RamLog.AddInfo('Job started');
AppData.RamLog.AddWarn('Cache miss');
AppData.RamLog.AddError('Disk full');     // Auto-pops the log window if ShowOnError=TRUE
```

Verbosity levels (lowest → highest):
`lvDebug` → `lvVerbose` → `lvHints` → `lvInfos` → `lvImportant` → `lvWarnings` → `lvErrors`.

`Add*` methods map 1:1 to those levels. `AddInfo` is the default. `AddWarn` and `AddError` additionally call `NotifyLogObserverAndShow` — if `ShowOnError=TRUE` the visual log window pops up.

## Single-threaded vs Multi-threaded mode

Selected at construction time:

```pascal
RamLog := TRamLog.Create(ShowOnError, Observer, MultiThreaded);
```

| Mode | Storage class | Locking | Use when |
|---|---|---|---|
| `MultiThreaded=FALSE` (default) | `TLogLinesSingleThreaded` | None (lock hooks are no-ops) | All logging happens on the main thread |
| `MultiThreaded=TRUE` | `TLogLinesMultiThreaded` | MREWS read/write | Workers/threads call `Add*` directly |

The `AppData` constructor accepts the same flag and creates `RamLog` with the matching mode.

In MT mode every individual operation (`Add`, `Count`, filtered iteration) is internally synchronized. **Iterating manually with `for i := 0 to Lines.Count-1 do Lines[i]` is still NOT atomic** — use the bulk methods `CountFiltered`, `Row2FilteredRow`, `GetFilteredSlice`, `ForEachLocked`, or `WriteToStream`, all of which hold the lock for the entire walk.

`ForEachLocked(proc(Line: PLogLine))` is the simplest option for a full walk. Used by `TRamLog.GetAsText` and `TLogViewer.SaveAsRtf`. Keep the callback short — it runs under the read lock, so background `Add*` calls block for its duration.

## Observer lifetime contract (CRITICAL)

`ILogObserver` is typically implemented by a `TComponent` descendant (`TLogViewer`). `TComponent._AddRef` returns -1 — interface references do **not** keep the observer alive. The observer **must**:

1. Call `RamLog.UnregisterLogObserver` before its destructor returns, **and**
2. Set a destruction-guard flag (e.g. `FFormDestroying`) and check it in `Populate` / `PopUpWindow` so any `TThread.Queue` closures still in flight bail out instead of touching freed memory.

`TLogViewer` already does both. If you write a custom observer, you must too — otherwise a worker thread emitting a log message during shutdown will dereference a freed object.

## Auto-save (overflow + periodic)

`TRamLog` writes to disk on its own. Two triggers, controlled by tunable properties:

| Property | Default | Triggers when... | Output file |
|---|---|---|---|
| `MaxEntries` | 1,000,000 | `Lines.Count > MaxEntries` on any `Add*` | `AppDataFolder\LargeLogSave.logbin` (then RAM is cleared) |
| `SaveInterval` | 60 seconds | seconds-since-last-save ≥ `SaveInterval` | `AppDataFolder\PeriodicLogSave.logbin` (RAM kept) |

Both properties have `Assert` guards (must be `> 0`).

The overflow path uses `SnapshotAndClear` under exclusive lock, so no entries are lost between snapshot and clear. The periodic path is best-effort — disk-full / sharing / permission errors are logged via `OutputDebugString` (Windows) or `stderr` (POSIX) and swallowed; unknown exception classes are re-raised.

**Snapshot-dispose deferral (custom observers, take note):** when overflow fires from a worker thread, `CheckAndSaveToDisk` writes the snapshot to disk and then queues a closure that runs `Populate` *and only then* disposes the snapshot. This protects observers that cache `PLogLine` pointers (e.g. `TLogViewer`'s `FGrid.Objects` in VCL, `FVisibleLines` in FMX) — by the time the snapshot's records are freed, `Populate` has already refreshed those caches with new pointers from the (empty) live list. If you write a custom observer that caches `PLogLine` between Populate calls, the framework guarantees those pointers stay valid until your next Populate runs.

If you don't want any disk activity, raise `MaxEntries` to a huge number and `SaveInterval` to `MaxInt`.

## Coalesced cross-thread notifications

Worker threads emitting thousands of messages would otherwise post one `TThread.Queue` closure per message and force the visual log to repaint N times. Instead, the first emitter posts a single repaint via an atomic flag (`INotifyCoalesceFlag`); subsequent emitters in the same burst silently skip. The flag is released **before** `Populate` runs, so any `Add` arriving mid-paint can re-queue a fresh repaint and not be lost.

The coalesce flag lives behind an interface (not a `TRamLog` field) on purpose: queued closures must not capture `Self`, since `TRamLog` is allowed to be destroyed while a closure is still pending in the main-thread queue.

## Verbosity levels and colors

`TLogVerbLvl` is defined in `LightCore.LogTypes.pas`. Both viewers map levels to colors via `Verbosity2Color(Verbosity, IsDark)`. VCL detects the theme via `IsLightStyleColor(clWindow)`; FMX uses `IsDarkStyle` from `LightFmx.Common.Styles.pas` (cached in `FIsDarkCache`, refreshed once per `setUpRows`).

| Level | Method | Light theme | Dark theme |
|---|---|---|---|
| `lvDebug` | `AddDebug` | gray (#909090) | gray (#909090) |
| `lvVerbose` | `AddVerb` | silver (#808080) | light gray (#B0B0B0) |
| `lvHints` | `AddHint` | gray (#707070) | silver (#C0C0C0) |
| `lvInfos` | `AddInfo` | black | white |
| `lvImportant` | `AddImpo` | dark orange | light orange |
| `lvWarnings` | `AddWarn` | orange | orange |
| `lvErrors` | `AddError` | red | red |

`AddBold`, `AddMsg`, `AddMsgInt`, `AddEmptyRow` are convenience helpers that all post at `lvInfos` (with `AddBold` setting the bold flag).

The visual viewer's verbosity property filters which levels are displayed — messages still get added to the underlying `TRamLog`, they just don't appear on screen until the filter is lowered.

## Persistence

| Method | What it does |
|---|---|
| `LoadFromFile` / `SaveToFile` | Binary format (`.logbin`); preferred |
| `LoadFromStream` / `SaveToStream` | Same format on an open `TLightStream` |
| `GetAsText` / `SaveAsText` | Plain text dump (one CRLF-separated line per entry) |
| `Clear` | Empty in-RAM list and notify observer |

Stream format version is currently 5 — see the `Stream Format (Version 5)` block in `LightCore.LogLinesAbstract.pas`. Backward compatibility for binary loading is a project rule; never change the on-disk layout without bumping the version and providing a v4-or-older reader.

## TRamLog public API summary

```pascal
constructor Create(aShowOnError: Boolean; Observer: ILogObserver; MultiThreaded: Boolean = FALSE);

// Add by verbosity level
procedure AddDebug   (CONST Msg: string);
procedure AddVerb    (CONST Msg: string);
procedure AddHint    (CONST Msg: string);
procedure AddInfo    (CONST Msg: string);    // default
procedure AddImpo    (CONST Msg: string);
procedure AddWarn    (CONST Msg: string);    // also notifies observer to pop window
procedure AddError   (CONST Msg: string);    // also notifies observer to pop window

// Convenience
procedure AddBold    (CONST Msg: string);
procedure AddMsg     (CONST Msg: string);
procedure AddMsgInt  (CONST Msg: string; i: Integer);
procedure AddEmptyRow;

// Tunable thresholds
property MaxEntries  : Integer;   // default 1,000,000
property SaveInterval: Integer;   // default 60 (seconds)

// Lifecycle
property ShowOnError : Boolean;
procedure Clear;
function  Count(Filtered: Boolean; Filter: TLogVerbLvl): Integer;

// I/O
function  GetAsText: string;
procedure SaveAsText  (CONST FullPath: string);
function  LoadFromFile(CONST FullPath: string): Boolean;
procedure SaveToFile  (CONST FullPath: string);

// Observer
procedure RegisterLogObserver(Observer: ILogObserver);
procedure UnregisterLogObserver;
procedure NotifyLogObserver;
procedure NotifyLogObserverAndShow;
procedure PopUpWindow;
```

## TLogViewer (VCL and FMX)

Drop a `TLogViewer` on a form and it just works — it creates its own internal `TRamLog`. To attach it to the application-wide `AppData.RamLog` instead:

```pascal
// VCL
LogViewer.AssignExternalRamLog(AppData.RamLog);

// FMX
LogViewer.ObserveAppDataLog;
```

Both layers ship a pre-built form (`TfrmRamLog` in `LightVcl.Visual.LogForm.pas` / `LightFmx.Common.LogForm.pas`) that hosts the viewer. The FMX `AppData.FormLog` property is lazy-created on first access; the VCL equivalent is created the same way through `CreateForm`.

User actions exposed by the viewer:

- Clipboard: `CopyAll`, `CopyVisible`, `CopyCurLine`
- Export (VCL only): `SaveAsRtf`
- Toggle date/time columns: `ShowDate`, `ShowTime`
- Filter level: `Verbosity` (writes the property → re-populates)

## Demos

Working examples:

```
Demo\VCL\Demo LightLog\VCL_Demo_Log.dpr
Demo\FMX\Demo LightLog\FMX_Demo_Log.dpr
```

(Note: some unit headers reference the path as `LightSaber\Demo\LightLog\` — that is stale; the demos live under `Demo\VCL\` and `Demo\FMX\`.)

## Pitfalls and traps

### `Stream.ReadString(N)` is NOT a fixed-length read

After 2026-05-05, `TLightStream.ReadString` accepts an optional `SafetyLimit` parameter. So `Stream.ReadString(10)` now compiles but means *"read a length-prefixed string with a 10-byte ceiling"*, **not** *"read 10 bytes"*. For a no-length raw read use `ReadStringCnt(N)` / `ReadStringACnt(N)`. Two demo files were already affected (now fixed); see `LightCore.StreamBuff.pas` interface comments for the full convention.

When loading binary log files, log messages can be very long (stack traces, JSON payloads). The log loader passes `MaxLogMsgBytes = 16 MB` explicitly to `ReadString` — don't lower it unless you also re-encode existing `.logbin` files.

### Observer "Populate" can fire after the observer is freed

If you implement a custom `ILogObserver` and forget the destruction guard described above, expect a hard-to-reproduce AV during shutdown. The framework provides hooks (`UnregisterLogObserver`, `FormDestroying`); use them.

### MaxEntries silently clears RAM

When the in-memory log crosses `MaxEntries` (default 1M), `TRamLog` saves the snapshot to `LargeLogSave.logbin` and **empties the in-RAM list**. If your code reads `RamLog.Lines` after each `Add` expecting to see all history, it won't. Either lower `MaxEntries` deliberately, or read `LargeLogSave.logbin` for older entries.

### Locked-walk methods block writers

`GetAsText`, `SaveAsRtf`, `WriteToStream`, and any caller of `ForEachLocked` hold the MREWS read lock for the entire iteration. In MT mode this means background-thread `Add*` calls block until the walk finishes. For a 1M-line log this can be hundreds of milliseconds. If a worker thread is on a tight deadline, prefer `GetFilteredSlice` (returns up to a fixed number of pointers per call) over a full-log walk.

`SaveAsRtf` works around this by snapshotting message values (not pointers) under the lock, then doing the slow TRichEdit work outside it. Callers writing similar long-running exports should follow the same pattern.

## Related units

- `LightCore.LogTypes.pas` — `TLogVerbLvl` enum, `Verbosity2String`, `DefaultVerbosity`
- `LightCore.LogRam.pas` — `TRamLog`, `ILogObserver`, `INotifyCoalesceFlag`
- `LightCore.LogLinesAbstract.pas` — `TAbstractLogLines`, `RLogLine`
- `LightCore.LogLinesS.pas` — single-threaded storage
- `LightCore.LogLinesM.pas` — multi-threaded storage (MREWS)
- `FrameVCL\LightVcl.Common.LogViewer.pas` — VCL viewer
- `FrameFMX\LightFmx.Common.LogViewer.pas` — FMX viewer
- `FrameVCL\LightVcl.Visual.LogForm.pas` — VCL host form `TfrmRamLog`
- `FrameFMX\LightFmx.Common.LogForm.pas` — FMX host form `TfrmRamLog`
