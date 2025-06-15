UNIT ccLogRam;

{=============================================================================================================
   2025.05
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   A simple but effective log (non-visual).
   Its data can be displayed by TLogGrid, but it can also work alone without being connected to a TLogGrid.
   It can easily hold up to 1 million entries. Being a good citizen, when it reaches this number it saves existing data to disk and then clears it from RAM.

   Verbosity:
     Supports several verbosity levels (verbose, info, warnings, errors, etc)

   Future plans
     Replaces the TRamLog.

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
   ccLogLinesAbstract, ccLogLinesS, ccLogLinesM, ccLogTypes, ccStreamBuff2;

TYPE
  ILogObserver = interface
    ['{A1B2C3D4-E5F6-4321-8765-9876543210AB}']
    procedure Populate;
    procedure PopUpWindow;
  end;

  TRamLog = class(TObject)
   private
     FLogObserver: ILogObserver;
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
     function  LoadFromStream(Stream: TCubicBuffStream2): Boolean;
     procedure SaveToStream  (Stream: TCubicBuffStream2);

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
  ccCore, ccTextFile, LighCore.AppData;


{-------------------------------------------------------------------------------------------------------------
   CTOR
-------------------------------------------------------------------------------------------------------------}
constructor TRamLog.Create(aShowOnError: Boolean; Observer: ILogObserver; MultiThreaded: Boolean= FALSE);
begin
  inherited Create;

  ShowOnError := aShowOnError;
  FSaveInterval := 60;  // Save every 60 seconds
  FLastSaveTime := Now;

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
{ Filtered False: Returns the number of rows
  Filtered True: Returns the number of rows that we can extract from RamLog after we took into consideration the verbosity filter }
function TRamLog.Count(Filtered: Boolean; Filter: TLogVerbLvl): Integer;
begin
  Result := 0;
  if Filtered
  then
    for VAR i := 0 to Lines.Count - 1 do
      begin
       if Lines[i].Level >= Filter
       then Inc(Result);
      end
  else
    Result:= Lines.Count;
end;


{-------------------------------------------------------------------------------------------------------------
   OBSERVER METHODS
-------------------------------------------------------------------------------------------------------------}
procedure TRamLog.RegisterLogObserver(Observer: ILogObserver);
begin
  FLogObserver := NIL; // explicitly set to nil before assigning a new observer to prevent memory leaks or undefined behavior with multiple observers. Ensure proper lifecycle management of observers.
  FLogObserver := Observer;
end;


procedure TRamLog.UnregisterLogObserver;
begin
  FLogObserver := NIL;
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
-------------------------------------------------------------------------------------------------------------}
procedure TRamLog.CheckAndSaveToDisk;
begin
  if Lines.Count > MaxEntries then
  begin
    SaveToFile(TAppDataCore.AppDataFolder+ 'LargeLogSave.txt');
    Lines.Clear;
  end;

  if SecondsBetween(Now, FLastSaveTime) >= FSaveInterval then  // Default 60 sec
  begin
    SaveToFile(TAppDataCore.AppDataFolder+ 'PeriodicLogSave.txt');
    FLastSaveTime := Now;
  end;
end;


{-------------------------------------------------------------------------------------------------------------
   TEXT
-------------------------------------------------------------------------------------------------------------}
{ Returns all lines, even if a filter is applied }
function TRamLog.GetAsText: string;
VAR i: Integer;
begin
 Result:= '';
 for i:= 0 to Lines.Count-1
   DO Result:= Result+ Lines[i].Msg+ CRLF;

 {Cut the last enter}
 Result:= ccCore.RemoveLastEnter(Result);
end;



function TRamLog.prepareString(CONST Msg: string): string;
begin
 Result:= ReplaceEnters (Msg , ' ');
 //Result:= StringOfChar(' ', Indent)+ Result;
end;









{-------------------------------------------------------------------------------------------------------------
    I/O
-------------------------------------------------------------------------------------------------------------}
procedure TRamLog.SaveAsText(CONST FullPath: string);
begin
  StringToFile(FullPath, GetAsText, woOverwrite, wpAuto);
end;



function TRamLog.LoadFromStream(Stream: TCubicBuffStream2): Boolean;
VAR StreamVer: Word;
begin
  Result:= Stream.ReadHeaderVersion(StreamSign, StreamVer);
  if NOT Result then EXIT;
  if StreamVer <= 3 then EXIT;    // Old/unsupported version

  Lines.ReadFromStream(Stream);
  Stream.ReadPaddingDef;
end;

procedure TRamLog.SaveToStream(Stream: TCubicBuffStream2);
begin
  Stream.WriteHeader(StreamSign, TAbstractLogLines.CurVer);
  Lines.WriteToStream(Stream);
  Stream.WritePaddingdef;
end;




function TRamLog.LoadFromFile(const FullPath: string): Boolean;
begin
 Clear;

 VAR Stream:= TCubicBuffStream2.CreateRead(FullPath);
 TRY
   Result:= LoadFromStream(Stream);
 FINALLY
   FreeAndNil(Stream);
 END;

 if Result then NotifyLogObserver;
end;


procedure TRamLog.SaveToFile(const FullPath: string);
begin
 VAR Stream:= TCubicBuffStream2.CreateWrite(FullPath);
 TRY
   SaveToStream(Stream);
 FINALLY
   FreeAndNil(Stream);
 END;
end;


end.
