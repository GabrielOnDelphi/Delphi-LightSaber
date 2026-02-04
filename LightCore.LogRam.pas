UNIT LightCore.LogRam;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   A simple but effective log (non-visual).
   Its data can be displayed by an observer (such as TLogViewer in Light.Common.LogViewer.pas), but it can also work alone without being connected to an observer.
   It can easily hold up to 1 million entries. Being a good citizen, when it reaches this number it saves existing data to disk and then clears it from RAM.

   Verbosity:
     Supports several verbosity levels (verbose, info, warnings, errors, etc)

   Thread Safety:
     The MultiThreaded parameter in Create determines whether the underlying Lines storage is thread-safe.
     When MultiThreaded=TRUE:
       - Individual Add/Clear/Count operations are thread-safe
       - Observer notifications are synchronized to the main thread via TThread.Queue
       - GetAsText iteration is NOT atomic (documented TOCTOU issue)
       - Periodic save checks (CheckAndSaveToDisk) access FLastSaveTime without lock - acceptable for approximate timing

   Tester:
     LightSaber\Demo\LightLog\
=============================================================================================================}

//ToDo: TRamLog must save its data to disk every x seconds. For this we don't start a timer (resource intensive), but each time we receive a message, we check when we saved the log last time. If more than x seconds have passed, we save it now.
//ToDo: I need a routine to also manually save the data to disk, on demand, in case I want to do it more often that each x seconds.
//ToDo: TRamLog must cope with multi-threading environment, for example, multiple threads could send data to the log.
//ToDo: TRamLog must be able to receive data (AddMsg) even when it is busy to write to disk.


//Proposal:
// When the user sends a message to TRamLog, the message is posted (thread safely) in a queue. When the TRamLog has time, it collects the messages from the queus, combines them in the chronologic order (maybe with the thread ID), and saves them to disk.
// In the constructor, there should be a switch to indicate if the log needs to operate in threaded or non-threaded mode.
// It would be much more efficient probably, not to create the queue if we are not in multithreaded mode.

INTERFACE

USES
   System.SysUtils, System.DateUtils, System.Classes,
   LightCore.LogLinesAbstract, LightCore.LogLinesS, LightCore.LogLinesM, LightCore.LogTypes, LightCore.StreamBuff;

TYPE
  ILogObserver = interface
    ['{A1B2C3D4-E5F6-4321-8765-9876543210AB}']
    procedure Populate;
    procedure PopUpWindow;
  end;

  TRamLog = class(TObject)
   private
     FLogObserver: ILogObserver;    // This is the GUI element that observes this log
     FLastSaveTime: TDateTime;
     FSaveInterval: Integer; // Interval in seconds for auto-save
     const
      StreamSign  = 'TRamLog';
      MaxEntries  = 1000000; // Maximum number of entries before saving and clearing
   protected
     function prepareString(CONST Msg: string): string;
     procedure CheckAndSaveToDisk;
   public
     Lines: TAbstractLogLines;
     ShowOnError: Boolean; // Automatically show the visual log form when warnings/errors are added to the log. This way the user is informed about the problems.
     constructor Create(aShowOnError: Boolean; Observer: ILogObserver; MultiThreaded: Boolean= FALSE);
     destructor Destroy;  override;
     procedure Clear;

     function Count(Filtered: Boolean; Filter: TLogVerbLvl): Integer;

     procedure AddBold    (const Msg: string);
     procedure AddMsg     (CONST Msg: string);
     procedure AddMsgInt  (CONST Msg: string; i: Integer);
     procedure AddEmptyRow;

     procedure AddDebug   (CONST Msg: string);
     procedure AddVerb    (CONST Msg: string);
     procedure AddHint    (CONST Msg: string);
     procedure AddInfo    (CONST Msg: string);                             { Default level }
     procedure AddImpo    (CONST Msg: string);
     procedure AddWarn    (CONST Msg: string);
     procedure AddError   (CONST Msg: string);
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
  end;


IMPLEMENTATION

USES
  LightCore, LightCore.TextFile, LightCore.AppData;

CONST CurrentVersion = 5;


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
  FSaveInterval:= 60;  { Auto-save interval in seconds }
  FLastSaveTime:= Now;

  if MultiThreaded
  then Lines:= TLogLinesMultiThreaded.Create
  else Lines:= TLogLinesSingleThreaded.Create;

  if Observer <> NIL
  then RegisterLogObserver(Observer);
end;


destructor TRamLog.Destroy;
begin
  FreeAndNil(Lines);
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


{-------------------------------------------------------------------------------------------------------------
   OBSERVER METHODS
-------------------------------------------------------------------------------------------------------------}
{ Registers a GUI observer (like TLogViewer) to receive log updates.
  Only one observer can be registered at a time. Registering a new observer
  automatically unregisters any previous observer (interface reference counting
  handles cleanup automatically). }
procedure TRamLog.RegisterLogObserver(Observer: ILogObserver);
begin
  FLogObserver:= Observer;
end;


procedure TRamLog.UnregisterLogObserver;
begin
  FLogObserver:= NIL;
end;


{ For the multithreaded case: VCL updates must be synchronized to the main thread. }
procedure TRamLog.NotifyLogObserver;
begin
  if Assigned(FLogObserver) then
    if TThread.CurrentThread.ThreadID = MainThreadID
    then FLogObserver.Populate
    else TThread.Queue(nil, procedure begin FLogObserver.Populate; end);  // TThread.Queue schedules the call on the main thread without blocking the caller, suitable for frequent log updates from background threads.
end;


procedure TRamLog.NotifyLogObserverAndShow;
begin
  if Assigned(FLogObserver) then
   begin
     NotifyLogObserver;
     if ShowOnError
     then PopUpWindow;
   end;
end;


procedure TRamLog.PopUpWindow;
begin
  if Assigned(FLogObserver) then
    if TThread.CurrentThread.ThreadID = MainThreadID
      then FLogObserver.PopUpWindow
      else TThread.Queue(nil, procedure begin FLogObserver.PopUpWindow; end);
end;








{-------------------------------------------------------------------------------------------------------------
   ADD GENERIC MESSAGE
-------------------------------------------------------------------------------------------------------------}
procedure TRamLog.AddBold (CONST Msg: string);
begin
  Lines.AddNewLine(PrepareString(Msg), lvInfos, TRUE);
  CheckAndSaveToDisk;
  NotifyLogObserver;
end;


procedure TRamLog.AddMsg(CONST Msg: string);
begin
  Lines.AddNewLine(PrepareString(Msg), lvInfos, FALSE);
  CheckAndSaveToDisk;
  NotifyLogObserver;
end;


procedure TRamLog.AddMsgInt(const Msg: string; i: Integer);   { Adds a message text followed by an integer }
begin
  Lines.AddNewLine(PrepareString(Msg) + IntToStr(i), lvInfos, FALSE);
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
procedure TRamLog.AddDebug(CONST Msg: string);                             { Relevance 0 }
begin
  Lines.AddNewLine(PrepareString(Msg), lvDebug, FALSE);
  CheckAndSaveToDisk;
  NotifyLogObserver;
end;


procedure TRamLog.AddVerb(CONST Msg: string);                               { Relevance 1 }
begin
  Lines.AddNewLine(PrepareString(Msg), lvVerbose, FALSE);
  CheckAndSaveToDisk;
  NotifyLogObserver;
end;


procedure TRamLog.AddHint(CONST Msg: string);                               { Relevance 2 }
begin
  Lines.AddNewLine(PrepareString(Msg), lvHints, FALSE);
  CheckAndSaveToDisk;
  NotifyLogObserver;
end;


procedure TRamLog.AddInfo(CONST Msg: string);                               { Relevance 3 }
begin
  Lines.AddNewLine(PrepareString(Msg), lvInfos, FALSE);
  CheckAndSaveToDisk;
  NotifyLogObserver;
end;


procedure TRamLog.AddImpo(CONST Msg: string);                               { Relevance 4 }
begin
  Lines.AddNewLine(PrepareString(Msg), lvImportant, FALSE);
  CheckAndSaveToDisk;
  NotifyLogObserver;
end;


procedure TRamLog.AddWarn(CONST Msg: string);                               { Relevance 5 }
begin
  Lines.AddNewLine(PrepareString(Msg), lvWarnings, FALSE);
  CheckAndSaveToDisk;
  NotifyLogObserverAndShow;
end;


procedure TRamLog.AddError(CONST Msg: string);                              { Relevance 6 }
begin
  Lines.AddNewLine(PrepareString(Msg), lvErrors, FALSE);
  CheckAndSaveToDisk;
  NotifyLogObserverAndShow;
end;


{-------------------------------------------------------------------------------------------------------------
   CHECK AND SAVE TO DISK

   Auto-save triggers:
     1. When log exceeds MaxEntries (1 million) - saves to LargeLogSave.logbin and clears RAM
     2. When FSaveInterval seconds have passed since last save - saves to PeriodicLogSave.logbin

   Note: FLastSaveTime access is not locked in multi-threaded mode. This is acceptable because:
     - Worst case is an extra save or a skipped save interval, both harmless
     - Locking on every message would significantly impact performance
-------------------------------------------------------------------------------------------------------------}
procedure TRamLog.CheckAndSaveToDisk;
begin
  { Save and clear if we've exceeded the maximum entry count }
  if Lines.Count > MaxEntries then
    TRY
      SaveToFile(TAppDataCore.AppDataFolder + 'LargeLogSave.logbin');
      Lines.Clear;
    EXCEPT
      { Silently ignore save errors - don't let backup failure crash the application.
        The log data remains in memory and will be saved on next successful attempt. }
    END;

  { Periodic save for crash recovery }
  if SecondsBetween(Now, FLastSaveTime) >= FSaveInterval then
    TRY
      SaveToFile(TAppDataCore.AppDataFolder + 'PeriodicLogSave.logbin');
      FLastSaveTime:= Now;
    EXCEPT
      { Silently ignore periodic save errors }
    END;
end;


{-------------------------------------------------------------------------------------------------------------
   TEXT
-------------------------------------------------------------------------------------------------------------}

{ Returns all log lines as a single string, separated by CRLF.

  Thread Safety Warning:
    In multi-threaded mode, this iteration has a TOCTOU (Time-Of-Check-To-Time-Of-Use) issue.
    Between reading Lines.Count and accessing Lines[i], another thread could modify the list.
    This could cause index-out-of-bounds errors or return inconsistent data.

  For thread-safe alternatives:
    - Use Lines.WriteToStream which holds a lock for the entire operation
    - Accept that the output may represent a "fuzzy" snapshot of the log }
function TRamLog.GetAsText: string;
var
  i: Integer;
begin
  Result:= '';
  for i:= 0 to Lines.Count-1 do
    Result:= Result + Lines[i].Msg + CRLF;

  Result:= LightCore.RemoveLastEnter(Result);  { Remove trailing line break }
end;



{ Prepares a message string for storage by replacing all line breaks with spaces.
  This ensures each log entry is a single line, which simplifies display and filtering. }
function TRamLog.prepareString(CONST Msg: string): string;
begin
  Result:= ReplaceEnters(Msg , ' ');
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




{ Loads log data from a binary file.
  Returns TRUE on success, FALSE if file doesn't exist or has incompatible format.
  Note: Existing log entries are cleared BEFORE loading. If the load fails after Clear,
  the previous data is lost. This is intentional - a fresh start on load. }
function TRamLog.LoadFromFile(const FullPath: string): Boolean;
begin
 if NOT FileExists(FullPath)
 then EXIT(FALSE);

 Clear;

 VAR Stream:= TLightStream.CreateRead(FullPath);
 TRY
   Result:= LoadFromStream(Stream);
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
