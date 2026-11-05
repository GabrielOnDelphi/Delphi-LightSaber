unit Test.LightCore.LogRam;

{=============================================================================================================
   2026.10.13
   Unit tests for LightCore.LogRam.pas
   Tests TRamLog - the non-visual log class
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  LightCore.LogTypes,
  LightCore.LogRam,
  LightCore.StreamBuff;

type
  [TestFixture]
  TTestLogRam = class
  private
    FLog: TRamLog;
    FTestDir: string;
    procedure CleanupTestDir;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Basic Operations }
    [Test]
    procedure TestCreate;

    [Test]
    procedure TestCreate_MultiThreaded;

    [Test]
    procedure TestClear;

    { Add Message Tests }
    [Test]
    procedure TestAddMsg;

    [Test]
    procedure TestAddBold;

    [Test]
    procedure TestAddMsgInt;

    [Test]
    procedure TestAddEmptyRow;

    { Verbosity Level Tests }
    [Test]
    procedure TestAddDebug;

    [Test]
    procedure TestAddVerb;

    [Test]
    procedure TestAddHint;

    [Test]
    procedure TestAddInfo;

    [Test]
    procedure TestAddImpo;

    [Test]
    procedure TestAddWarn;

    [Test]
    procedure TestAddError;

    { Count Tests }
    [Test]
    procedure TestCount_Unfiltered;

    [Test]
    procedure TestCount_Filtered;

    { Text Export Tests }
    [Test]
    procedure TestGetAsText;

    [Test]
    procedure TestSaveAsText;

    { Stream I/O Tests }
    [Test]
    procedure TestSaveLoadStream;

    { File I/O Tests }
    [Test]
    procedure TestSaveLoadFile;

    { Enter Handling Tests }
    [Test]
    procedure TestMessageWithEnters;

    { File Not Found Tests }
    [Test]
    procedure TestLoadFromFile_NotExists;

    { Observer Tests }
    [Test]
    procedure TestRegisterObserver;

    [Test]
    procedure TestUnregisterObserver;

    [Test]
    procedure TestTwoObservers_BothNotified;

    [Test]
    procedure TestUnregisterOneObserver_OtherStaysAttached;

    [Test]
    procedure TestUnregisterAll_DetachesEveryObserver;

    [Test]
    procedure TestRegisterSameObserverTwice_NotifiedOnce;

    [Test]
    procedure TestPopUpWindow_OnlyNewestObserver;

    [Test]
    procedure TestOverflow_PopulatesEveryObserver;

    { Overflow save Tests }
    [Test]
    procedure TestOverflow_SecondLog_DoesNotOverwriteAppLogFile;

    [Test]
    procedure TestOverflow_SameLogTwice_KeepsFirstSave;

    [Test]
    procedure TestOverflowFile_AppLogKeepsName_OthersDiffer;

    { Periodic save Tests }
    [Test]
    procedure TestPeriodicSave_SecondLog_DoesNotWriteAppLogFile;

    [Test]
    procedure TestPeriodicSave_AppLog_StillWritesFile;

    { Timestamp Tests }
    [Test]
    procedure TestTimestampRecorded;

    { Verbosity Preservation Tests }
    [Test]
    procedure TestVerbosityPreservedOnSaveLoad;

    { Multi-threaded Log Tests }
    [Test]
    procedure TestMultiThreadedAddMsg;
  end;

  { Mock observer for testing }
  TMockLogObserver = class(TInterfacedObject, ILogObserver)
  public
    PopulateCallCount: Integer;
    PopUpWindowCallCount: Integer;
    procedure Populate;
    procedure PopUpWindow;
  end;

  { Mock observer whose lifetime its interface references do NOT manage (TComponent._AddRef/_Release do not count), like the real TLogViewer. }
  TMockComponentObserver = class(TComponent, ILogObserver)
  public
    PopulateCallCount: Integer;
    PopUpWindowCallCount: Integer;
    procedure Populate;
    procedure PopUpWindow;
  end;

implementation

uses
  LightCore.AppData;


procedure TTestLogRam.Setup;
begin
  FTestDir:= TPath.Combine(TPath.GetTempPath, 'LogRamTest_' + TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FTestDir);

  { Initialize AppDataCore if not already done }
  if AppDataCore = nil
  then AppDataCore:= TAppDataCore.Create('LogRamTest');

  FLog:= TRamLog.Create(False, nil, False);
end;


procedure TTestLogRam.TearDown;
begin
  FreeAndNil(FLog);
  CleanupTestDir;
end;


procedure TTestLogRam.CleanupTestDir;
begin
  if TDirectory.Exists(FTestDir)
  then TDirectory.Delete(FTestDir, True);
end;


{ Basic Operations }

procedure TTestLogRam.TestCreate;
begin
  Assert.IsNotNull(FLog);
  Assert.IsNotNull(FLog.Lines);
  Assert.AreEqual(0, FLog.Lines.Count);
end;

procedure TTestLogRam.TestCreate_MultiThreaded;
var
  MTLog: TRamLog;
begin
  MTLog:= TRamLog.Create(False, nil, True);
  try
    Assert.IsNotNull(MTLog);
    Assert.IsNotNull(MTLog.Lines);
  finally
    FreeAndNil(MTLog);
  end;
end;

procedure TTestLogRam.TestClear;
begin
  FLog.AddMsg('Test 1');
  FLog.AddMsg('Test 2');
  Assert.AreEqual(2, FLog.Lines.Count);

  FLog.Clear;
  Assert.AreEqual(0, FLog.Lines.Count);
end;


{ Add Message Tests }

procedure TTestLogRam.TestAddMsg;
begin
  FLog.AddMsg('Test Message');
  Assert.AreEqual(1, FLog.Lines.Count);
  Assert.AreEqual('Test Message', FLog.Lines[0].Msg);
  Assert.AreEqual(lvInfos, FLog.Lines[0].Level);
  Assert.IsFalse(FLog.Lines[0].Bold);
end;

procedure TTestLogRam.TestAddBold;
begin
  FLog.AddBold('Bold Message');
  Assert.AreEqual(1, FLog.Lines.Count);
  Assert.IsTrue(FLog.Lines[0].Bold);
end;

procedure TTestLogRam.TestAddMsgInt;
begin
  FLog.AddMsgInt('Count: ', 42);
  Assert.AreEqual(1, FLog.Lines.Count);
  Assert.AreEqual('Count: 42', FLog.Lines[0].Msg);
end;

procedure TTestLogRam.TestAddEmptyRow;
begin
  FLog.AddMsg('Before');
  FLog.AddEmptyRow;
  FLog.AddMsg('After');

  Assert.AreEqual(3, FLog.Lines.Count);
  Assert.AreEqual('', FLog.Lines[1].Msg);
end;


{ Verbosity Level Tests }

procedure TTestLogRam.TestAddDebug;
begin
  FLog.AddDebug('Debug message');
  Assert.AreEqual(lvDebug, FLog.Lines[0].Level);
end;

procedure TTestLogRam.TestAddVerb;
begin
  FLog.AddVerb('Verbose message');
  Assert.AreEqual(lvVerbose, FLog.Lines[0].Level);
end;

procedure TTestLogRam.TestAddHint;
begin
  FLog.AddHint('Hint message');
  Assert.AreEqual(lvHints, FLog.Lines[0].Level);
end;

procedure TTestLogRam.TestAddInfo;
begin
  FLog.AddInfo('Info message');
  Assert.AreEqual(lvInfos, FLog.Lines[0].Level);
end;

procedure TTestLogRam.TestAddImpo;
begin
  FLog.AddImpo('Important message');
  Assert.AreEqual(lvImportant, FLog.Lines[0].Level);
end;

procedure TTestLogRam.TestAddWarn;
begin
  FLog.AddWarn('Warning message');
  Assert.AreEqual(lvWarnings, FLog.Lines[0].Level);
end;

procedure TTestLogRam.TestAddError;
begin
  FLog.AddError('Error message');
  Assert.AreEqual(lvErrors, FLog.Lines[0].Level);
end;


{ Count Tests }

procedure TTestLogRam.TestCount_Unfiltered;
begin
  FLog.AddDebug('Debug');
  FLog.AddInfo('Info');
  FLog.AddError('Error');

  Assert.AreEqual(3, FLog.Count(False, lvDebug));
end;

procedure TTestLogRam.TestCount_Filtered;
begin
  FLog.AddDebug('Debug');
  FLog.AddInfo('Info');
  FLog.AddWarn('Warning');
  FLog.AddError('Error');

  { Filter to warnings and above }
  Assert.AreEqual(2, FLog.Count(True, lvWarnings));

  { Filter to errors only }
  Assert.AreEqual(1, FLog.Count(True, lvErrors));

  { Filter to debug (all) }
  Assert.AreEqual(4, FLog.Count(True, lvDebug));
end;


{ Text Export Tests }

procedure TTestLogRam.TestGetAsText;
var
  Text: string;
begin
  FLog.AddMsg('Line 1');
  FLog.AddMsg('Line 2');
  FLog.AddMsg('Line 3');

  Text:= FLog.GetAsText;

  Assert.IsTrue(Pos('Line 1', Text) > 0);
  Assert.IsTrue(Pos('Line 2', Text) > 0);
  Assert.IsTrue(Pos('Line 3', Text) > 0);
end;

procedure TTestLogRam.TestSaveAsText;
var
  FilePath, Content: string;
begin
  FLog.AddMsg('Test line 1');
  FLog.AddMsg('Test line 2');

  FilePath:= TPath.Combine(FTestDir, 'log.txt');
  FLog.SaveAsText(FilePath);

  Assert.IsTrue(FileExists(FilePath));
  Content:= TFile.ReadAllText(FilePath);
  Assert.IsTrue(Pos('Test line 1', Content) > 0);
  Assert.IsTrue(Pos('Test line 2', Content) > 0);
end;


{ Stream I/O Tests }

procedure TTestLogRam.TestSaveLoadStream;
var
  Stream: TLightStream;
  FilePath: string;
  NewLog: TRamLog;
begin
  FLog.AddMsg('Message 1');
  FLog.AddWarn('Warning 1');
  FLog.AddError('Error 1');

  FilePath:= TPath.Combine(FTestDir, 'log.dat');
  Stream:= TLightStream.CreateWrite(FilePath);
  try
    FLog.SaveToStream(Stream);
  finally
    FreeAndNil(Stream);
  end;

  { Load into new log }
  NewLog:= TRamLog.Create(False, nil, False);
  try
    Stream:= TLightStream.CreateRead(FilePath);
    try
      Assert.IsTrue(NewLog.LoadFromStream(Stream));
    finally
      FreeAndNil(Stream);
    end;

    Assert.AreEqual(3, NewLog.Lines.Count);
    Assert.AreEqual('Message 1', NewLog.Lines[0].Msg);
    Assert.AreEqual('Warning 1', NewLog.Lines[1].Msg);
    Assert.AreEqual('Error 1', NewLog.Lines[2].Msg);
  finally
    FreeAndNil(NewLog);
  end;
end;


{ File I/O Tests }

procedure TTestLogRam.TestSaveLoadFile;
var
  FilePath: string;
  NewLog: TRamLog;
begin
  FLog.AddMsg('File Test 1');
  FLog.AddInfo('File Test 2');
  FLog.AddError('File Test 3');

  FilePath:= TPath.Combine(FTestDir, 'logfile.dat');
  FLog.SaveToFile(FilePath);

  Assert.IsTrue(FileExists(FilePath));

  { Load into new log }
  NewLog:= TRamLog.Create(False, nil, False);
  try
    Assert.IsTrue(NewLog.LoadFromFile(FilePath));
    Assert.AreEqual(3, NewLog.Lines.Count);
    Assert.AreEqual('File Test 1', NewLog.Lines[0].Msg);
  finally
    FreeAndNil(NewLog);
  end;
end;


{ Enter Handling Tests }

procedure TTestLogRam.TestMessageWithEnters;
begin
  { Messages with enters should have them replaced with spaces }
  FLog.AddMsg('Line1'#13#10'Line2');
  Assert.AreEqual('Line1 Line2', FLog.Lines[0].Msg);
end;


{ File Not Found Tests }

procedure TTestLogRam.TestLoadFromFile_NotExists;
var
  NewLog: TRamLog;
begin
  NewLog:= TRamLog.Create(False, nil, False);
  try
    { Loading a non-existent file should return False, not raise an exception }
    Assert.IsFalse(NewLog.LoadFromFile(TPath.Combine(FTestDir, 'NonExistentFile.dat')));
    Assert.AreEqual(0, NewLog.Lines.Count);
  finally
    FreeAndNil(NewLog);
  end;
end;


{ Mock Observer Implementation }

procedure TMockLogObserver.Populate;
begin
  Inc(PopulateCallCount);
end;

procedure TMockLogObserver.PopUpWindow;
begin
  Inc(PopUpWindowCallCount);
end;


procedure TMockComponentObserver.Populate;
begin
  Inc(PopulateCallCount);
end;

procedure TMockComponentObserver.PopUpWindow;
begin
  Inc(PopUpWindowCallCount);
end;


{ Observer Tests }

procedure TTestLogRam.TestRegisterObserver;
var
  Observer: TMockLogObserver;
  LogWithObserver: TRamLog;
begin
  Observer:= TMockLogObserver.Create;
  { Note: Observer is reference counted via ILogObserver, so no manual Free needed }
  LogWithObserver:= TRamLog.Create(False, Observer, False);
  try
    LogWithObserver.AddMsg('Test');
    { Observer's Populate should be called when message is added }
    Assert.IsTrue(Observer.PopulateCallCount >= 1, 'Observer.Populate should be called on AddMsg');
  finally
    FreeAndNil(LogWithObserver);
  end;
end;

procedure TTestLogRam.TestUnregisterObserver;
var
  Observer: TMockLogObserver;
  ObserverRef: ILogObserver;  { Keep a strong reference to prevent premature freeing }
  LogWithObserver: TRamLog;
  CallCountBefore: Integer;
begin
  Observer:= TMockLogObserver.Create;
  ObserverRef:= Observer;  { This prevents the observer from being freed when UnregisterLogObserver drops the log's reference to it }
  LogWithObserver:= TRamLog.Create(False, Observer, False);
  try
    LogWithObserver.AddMsg('Before unregister');
    CallCountBefore:= Observer.PopulateCallCount;

    LogWithObserver.UnregisterLogObserver;
    LogWithObserver.AddMsg('After unregister');

    { Populate count should not increase after unregistering }
    Assert.AreEqual(CallCountBefore, Observer.PopulateCallCount,
      'Observer should not be notified after unregistering');
  finally
    FreeAndNil(LogWithObserver);
  end;
end;


{ Two viewers on one log: the case of TAppData.CreateMainForm (the hidden log form observes AppData.RamLog) plus a TLogViewer on the main form that calls AssignExternalRamLog(AppData.RamLog).
  Both must receive every change. }
procedure TTestLogRam.TestTwoObservers_BothNotified;
var
  ObserverA, ObserverB: TMockLogObserver;
  RefA, RefB: ILogObserver;   { Strong references, so the log is never the last owner of a mock }
  LogTwo: TRamLog;
begin
  ObserverA:= TMockLogObserver.Create;
  RefA:= ObserverA;
  ObserverB:= TMockLogObserver.Create;
  RefB:= ObserverB;

  LogTwo:= TRamLog.Create(False, RefA, False);
  try
    LogTwo.RegisterLogObserver(RefB);
    ObserverA.PopulateCallCount:= 0;
    ObserverB.PopulateCallCount:= 0;

    LogTwo.AddInfo('Seen by both');

    Assert.AreEqual(1, ObserverA.PopulateCallCount, 'The first observer must still be notified after a second one registered');
    Assert.AreEqual(1, ObserverB.PopulateCallCount, 'The second observer must be notified too');
  finally
    FreeAndNil(LogTwo);
  end;
end;


{ What a TLogViewer does in its destructor: detach itself and leave the other viewers of the log attached.
  The observer is registered through the implicit class -> interface conversion (as Demo LightLog does) and unregistered through "as ILogObserver" (as TLogViewer does), so the test also proves that both give the same reference. }
procedure TTestLogRam.TestUnregisterOneObserver_OtherStaysAttached;
var
  Leaving, Staying: TMockComponentObserver;

  { The hidden interface temporaries made here (the implicit conversion, the "as" casts) are released when THIS routine returns, while both mocks are still alive.
    Released after FreeAndNil of a mock, TComponent._Release would read the freed object. }
  procedure RegisterBothThenUnregisterOne;
  var LogOne: TRamLog;
  begin
    LogOne:= TRamLog.Create(False, NIL, False);
    try
      LogOne.RegisterLogObserver(Leaving);
      LogOne.RegisterLogObserver(Staying as ILogObserver);

      LogOne.UnregisterLogObserver(Leaving as ILogObserver);
      LogOne.AddInfo('After one observer left');
    finally
      FreeAndNil(LogOne);
    end;
  end;

begin
  Leaving:= TMockComponentObserver.Create(NIL);
  Staying:= TMockComponentObserver.Create(NIL);
  try
    RegisterBothThenUnregisterOne;

    Assert.AreEqual(0, Leaving.PopulateCallCount, 'The unregistered observer must not be notified');
    Assert.AreEqual(1, Staying.PopulateCallCount, 'The other observer must stay attached');
  finally
    FreeAndNil(Staying);
    FreeAndNil(Leaving);
  end;
end;


{ The parameterless UnregisterLogObserver detaches every observer (Demo LightLog relies on it to add 1 million lines without repaints). }
procedure TTestLogRam.TestUnregisterAll_DetachesEveryObserver;
var
  ObserverA, ObserverB: TMockLogObserver;
  RefA, RefB: ILogObserver;
  LogAll: TRamLog;
begin
  ObserverA:= TMockLogObserver.Create;
  RefA:= ObserverA;
  ObserverB:= TMockLogObserver.Create;
  RefB:= ObserverB;

  LogAll:= TRamLog.Create(False, RefA, False);
  try
    LogAll.RegisterLogObserver(RefB);
    LogAll.UnregisterLogObserver;
    ObserverA.PopulateCallCount:= 0;
    ObserverB.PopulateCallCount:= 0;

    LogAll.AddInfo('Nobody listens');

    Assert.AreEqual(0, ObserverA.PopulateCallCount, 'The first observer must be detached');
    Assert.AreEqual(0, ObserverB.PopulateCallCount, 'The second observer must be detached');
  finally
    FreeAndNil(LogAll);
  end;
end;


{ Registering the same observer twice must not create a second entry: it would Populate twice per change, and one UnregisterLogObserver(Observer) would leave it attached. }
procedure TTestLogRam.TestRegisterSameObserverTwice_NotifiedOnce;
var
  Observer: TMockLogObserver;
  ObserverRef: ILogObserver;
  LogSame: TRamLog;
begin
  Observer:= TMockLogObserver.Create;
  ObserverRef:= Observer;

  LogSame:= TRamLog.Create(False, ObserverRef, False);
  try
    LogSame.RegisterLogObserver(ObserverRef);
    Observer.PopulateCallCount:= 0;

    LogSame.AddInfo('Once');
    Assert.AreEqual(1, Observer.PopulateCallCount, 'An observer registered twice must be notified once per change');

    LogSame.UnregisterLogObserver(ObserverRef);
    LogSame.AddInfo('After unregister');
    Assert.AreEqual(1, Observer.PopulateCallCount, 'One UnregisterLogObserver must detach an observer that was registered twice');
  finally
    FreeAndNil(LogSame);
  end;
end;


{ ShowOnError pops up the window of the NEWEST observer only. When it leaves, the older one gets the pop-ups again. }
procedure TTestLogRam.TestPopUpWindow_OnlyNewestObserver;
var
  Older, Newer: TMockLogObserver;
  RefOlder, RefNewer: ILogObserver;
  LogPop: TRamLog;
begin
  Older:= TMockLogObserver.Create;
  RefOlder:= Older;
  Newer:= TMockLogObserver.Create;
  RefNewer:= Newer;

  LogPop:= TRamLog.Create(True, RefOlder, False);   { ShowOnError = True }
  try
    LogPop.RegisterLogObserver(RefNewer);

    LogPop.AddError('First error');
    Assert.AreEqual(0, Older.PopUpWindowCallCount, 'Only the newest observer may pop up its window');
    Assert.AreEqual(1, Newer.PopUpWindowCallCount, 'The newest observer must pop up its window');

    LogPop.UnregisterLogObserver(RefNewer);
    LogPop.AddError('Second error');
    Assert.AreEqual(1, Older.PopUpWindowCallCount, 'With the newest observer gone, the older one must pop up its window');
    Assert.AreEqual(1, Newer.PopUpWindowCallCount, 'An unregistered observer must not pop up');
  finally
    FreeAndNil(LogPop);
  end;
end;


{ The overflow path frees the old lines right after it refreshes the observers, so EVERY observer must be refreshed there (each one caches pointers to those lines).
  The save writes the log's own overflow file into the AppData folder of the test EXE; the test removes it again. }
procedure TTestLogRam.TestOverflow_PopulatesEveryObserver;
var
  ObserverA, ObserverB: TMockLogObserver;
  RefA, RefB: ILogObserver;
  LogFull: TRamLog;
begin
  ObserverA:= TMockLogObserver.Create;
  RefA:= ObserverA;
  ObserverB:= TMockLogObserver.Create;
  RefB:= ObserverB;

  LogFull:= TRamLog.Create(False, RefA, False);
  try
    LogFull.RegisterLogObserver(RefB);
    LogFull.MaxEntries:= 2;
    LogFull.AddInfo('Line 1');
    LogFull.AddInfo('Line 2');
    ObserverA.PopulateCallCount:= 0;
    ObserverB.PopulateCallCount:= 0;

    LogFull.AddInfo('Line 3');   { 3 lines > MaxEntries: the overflow path moves the lines to a snapshot, refreshes the observers, frees the snapshot; then AddInfo notifies the observers again }

    Assert.AreEqual(0, LogFull.Lines.Count, 'The overflow path must have moved every line out of RAM');
    Assert.AreEqual(2, ObserverA.PopulateCallCount, 'The first observer must be refreshed by the overflow path and by the notification');
    Assert.AreEqual(2, ObserverB.PopulateCallCount, 'The second observer must be refreshed by the overflow path and by the notification');
  finally
    if FileExists(LogFull.OverflowFile(1))
    then System.SysUtils.DeleteFile(LogFull.OverflowFile(1));
    FreeAndNil(LogFull);
  end;
end;


{ Overflow save Tests

  When a log holds more than MaxEntries lines, the next Add saves every line to a .logbin file in the AppData folder and empties the RAM.
  The application log (AppDataCore.RamLog) saves to LargeLogSave.logbin.
  Each test removes the files it created; a file that was there before the test stays. }

{ TRUE when the .logbin file FileName holds a line whose text is Msg. }
function FileHoldsLine(CONST FileName, Msg: string): Boolean;
var
  Loaded: TRamLog;
  i: Integer;
begin
  Result:= FALSE;
  Loaded:= TRamLog.Create(False, nil, False);
  try
    Assert.IsTrue(Loaded.LoadFromFile(FileName), 'The overflow file must load: ' + FileName);
    for i:= 0 to Loaded.Lines.Count-1 do
      if Loaded.Lines[i].Msg = Msg
      then EXIT(TRUE);
  finally
    FreeAndNil(Loaded);
  end;
end;


{ Adds two lines to a log whose MaxEntries is 1, so the second Add runs the overflow save. The saved lines are 'Padding' and Marker. }
procedure OverflowWith(Log: TRamLog; CONST Marker: string);
begin
  Log.MaxEntries:= 1;
  Log.AddInfo('Padding');
  Log.AddInfo(Marker);
  Assert.AreEqual(0, Log.Lines.Count, 'The overflow save must have emptied the RAM');
end;


{ A second log that overflows must not overwrite the overflow file of the application log.
  The application log's overflow here must be its FIRST in this process (part 1 = LargeLogSave.logbin): no other test may overflow AppDataCore.RamLog. }
procedure TTestLogRam.TestOverflow_SecondLog_DoesNotOverwriteAppLogFile;
var
  AppFile, MarkerA, MarkerB: string;
  AppFileWasThere: Boolean;
  AppLogMax: Integer;
  SecondLog: TRamLog;
begin
  Assert.IsNotNull(AppDataCore, 'The test runner must create AppDataCore');
  AppFile:= TAppDataCore.AppDataFolder + 'LargeLogSave.logbin';
  AppFileWasThere:= FileExists(AppFile);
  MarkerA:= 'Application log ' + TGUID.NewGuid.ToString;
  MarkerB:= 'Second log '      + TGUID.NewGuid.ToString;

  AppLogMax:= AppDataCore.RamLog.MaxEntries;
  SecondLog:= TRamLog.Create(False, nil, False);
  try
    AppDataCore.RamLog.Clear;                  { Exactly two lines, so the second Add is the one that overflows }
    OverflowWith(AppDataCore.RamLog, MarkerA);
    OverflowWith(SecondLog, MarkerB);

    Assert.IsTrue(FileHoldsLine(AppFile, MarkerA), 'LargeLogSave.logbin must still hold the overflow of the application log, not the overflow of the second log');
    Assert.AreNotEqual(AppFile, SecondLog.OverflowFile(1), 'The second log must have an overflow file of its own');
    Assert.IsTrue(FileHoldsLine(SecondLog.OverflowFile(1), MarkerB), 'The overflow file of the second log must hold its lines');
  finally
    AppDataCore.RamLog.MaxEntries:= AppLogMax;
    if FileExists(SecondLog.OverflowFile(1))
    then System.SysUtils.DeleteFile(SecondLog.OverflowFile(1));
    FreeAndNil(SecondLog);
    if NOT AppFileWasThere AND FileExists(AppFile)
    then System.SysUtils.DeleteFile(AppFile);
  end;
end;


{ A log that overflows twice must not overwrite its own first overflow file: the second overflow goes to a numbered file. }
procedure TTestLogRam.TestOverflow_SameLogTwice_KeepsFirstSave;
var
  Marker1, Marker2: string;
  LogTwice: TRamLog;
begin
  Marker1:= 'First overflow '  + TGUID.NewGuid.ToString;
  Marker2:= 'Second overflow ' + TGUID.NewGuid.ToString;

  LogTwice:= TRamLog.Create(False, nil, False);
  try
    OverflowWith(LogTwice, Marker1);
    OverflowWith(LogTwice, Marker2);

    Assert.IsTrue(FileHoldsLine(LogTwice.OverflowFile(1), Marker1), 'The second overflow must not overwrite the first overflow file of the same log');
    Assert.IsTrue(FileHoldsLine(LogTwice.OverflowFile(2), Marker2), 'The second overflow must be saved to the second overflow file');
  finally
    if FileExists(LogTwice.OverflowFile(1)) then System.SysUtils.DeleteFile(LogTwice.OverflowFile(1));
    if FileExists(LogTwice.OverflowFile(2)) then System.SysUtils.DeleteFile(LogTwice.OverflowFile(2));
    FreeAndNil(LogTwice);
  end;
end;


{ The application log keeps the file name LargeLogSave.logbin; every other log gets a different name. }
procedure TTestLogRam.TestOverflowFile_AppLogKeepsName_OthersDiffer;
var
  LogA, LogB: TRamLog;
begin
  Assert.AreEqual(TAppDataCore.AppDataFolder + 'LargeLogSave.logbin',     AppDataCore.RamLog.OverflowFile(1));
  Assert.AreEqual(TAppDataCore.AppDataFolder + 'LargeLogSave (2).logbin', AppDataCore.RamLog.OverflowFile(2));

  LogA:= TRamLog.Create(False, nil, False);
  LogB:= TRamLog.Create(False, nil, False);
  try
    Assert.AreNotEqual(LogA.OverflowFile(1), LogB.OverflowFile(1), 'Two logs must not share an overflow file');
    Assert.AreNotEqual(AppDataCore.RamLog.OverflowFile(1), LogA.OverflowFile(1), 'A second log must not use the application log''s overflow file');

    LogA.OverflowName:= 'MyToolLog';
    Assert.AreEqual(TAppDataCore.AppDataFolder + 'MyToolLog.logbin', LogA.OverflowFile(1), 'A caller may name the overflow file');
  finally
    FreeAndNil(LogB);
    FreeAndNil(LogA);
  end;
end;


{ Periodic save Tests

  PeriodicLogSave.logbin in the AppData folder holds the periodic save of the APPLICATION log (AppDataCore.RamLog).
  A program can have more logs (one per tool form, a private preview log); if those saved there too, the last one to save would overwrite the application log's save.
  Both tests leave the file in the AppData folder of the test EXE. }

{ A second TRamLog whose save interval has passed must not write the application log's file. }
procedure TTestLogRam.TestPeriodicSave_SecondLog_DoesNotWriteAppLogFile;
var
  SaveFile: string;
  FileWasThere: Boolean;
  TimeBefore: TDateTime;
  SecondLog: TRamLog;
  AppLogInterval: Integer;
begin
  Assert.IsNotNull(AppDataCore, 'The test runner must create AppDataCore');
  SaveFile:= TAppDataCore.AppDataFolder + 'PeriodicLogSave.logbin';

  AppLogInterval:= AppDataCore.RamLog.SaveInterval;
  AppDataCore.RamLog.SaveInterval:= MaxInt;         { The application log must not write the file during this test }
  SecondLog:= TRamLog.Create(False, nil, False);
  try
    FileWasThere:= FileExists(SaveFile);
    if FileWasThere
    then TimeBefore:= TFile.GetLastWriteTime(SaveFile)
    else TimeBefore:= 0;

    SecondLog.SaveInterval:= 1;
    Sleep(1100);                                     { SecondsBetween truncates: wait more than one full second }
    SecondLog.AddInfo('Line of the second log');     { The save interval has passed: this Add would run the periodic save }

    Assert.AreEqual(FileWasThere, FileExists(SaveFile), 'A second TRamLog must not create the application log''s PeriodicLogSave.logbin');
    if FileWasThere
    then Assert.AreEqual(TimeBefore, TFile.GetLastWriteTime(SaveFile), 'A second TRamLog must not overwrite the application log''s PeriodicLogSave.logbin');
  finally
    FreeAndNil(SecondLog);
    AppDataCore.RamLog.SaveInterval:= AppLogInterval;
  end;
end;


{ The application log keeps its periodic save in PeriodicLogSave.logbin. }
procedure TTestLogRam.TestPeriodicSave_AppLog_StillWritesFile;
var
  SaveFile, Marker: string;
  AppLogInterval: Integer;
  Loaded: TRamLog;
  i: Integer;
  Found: Boolean;
begin
  Assert.IsNotNull(AppDataCore, 'The test runner must create AppDataCore');
  SaveFile:= TAppDataCore.AppDataFolder + 'PeriodicLogSave.logbin';
  Marker:= 'AppLog periodic save ' + TGUID.NewGuid.ToString;

  AppLogInterval:= AppDataCore.RamLog.SaveInterval;
  AppDataCore.RamLog.SaveInterval:= 1;
  try
    Sleep(1100);                                     { SecondsBetween truncates: wait more than one full second }
    AppDataCore.RamLog.AddInfo(Marker);              { The save interval has passed: this Add runs the periodic save }
  finally
    AppDataCore.RamLog.SaveInterval:= AppLogInterval;
  end;

  Assert.IsTrue(FileExists(SaveFile), 'The application log must write PeriodicLogSave.logbin');

  Loaded:= TRamLog.Create(False, nil, False);
  try
    Assert.IsTrue(Loaded.LoadFromFile(SaveFile), 'PeriodicLogSave.logbin must load');
    Found:= FALSE;
    for i:= 0 to Loaded.Lines.Count-1 do
      if Loaded.Lines[i].Msg = Marker
      then Found:= TRUE;
    Assert.IsTrue(Found, 'PeriodicLogSave.logbin must hold the line just added to the application log');
  finally
    FreeAndNil(Loaded);
  end;
end;


{ Timestamp Tests }

procedure TTestLogRam.TestTimestampRecorded;
var
  TimeBefore, TimeAfter: TDateTime;
begin
  TimeBefore:= Now;
  FLog.AddMsg('Timestamp test');
  TimeAfter:= Now;

  { Timestamp should be between before and after }
  Assert.IsTrue(FLog.Lines[0].Time >= TimeBefore, 'Timestamp should be >= time before add');
  Assert.IsTrue(FLog.Lines[0].Time <= TimeAfter, 'Timestamp should be <= time after add');
end;


{ Verbosity Preservation Tests }

procedure TTestLogRam.TestVerbosityPreservedOnSaveLoad;
var
  FilePath: string;
  NewLog: TRamLog;
begin
  { Add messages with different verbosity levels }
  FLog.AddDebug('Debug msg');
  FLog.AddVerb('Verbose msg');
  FLog.AddHint('Hint msg');
  FLog.AddInfo('Info msg');
  FLog.AddImpo('Important msg');
  FLog.AddWarn('Warning msg');
  FLog.AddError('Error msg');

  FilePath:= TPath.Combine(FTestDir, 'verbosity_test.dat');
  FLog.SaveToFile(FilePath);

  NewLog:= TRamLog.Create(False, nil, False);
  try
    Assert.IsTrue(NewLog.LoadFromFile(FilePath));
    Assert.AreEqual(7, NewLog.Lines.Count);

    { Verify each verbosity level was preserved }
    Assert.AreEqual(lvDebug, NewLog.Lines[0].Level);
    Assert.AreEqual(lvVerbose, NewLog.Lines[1].Level);
    Assert.AreEqual(lvHints, NewLog.Lines[2].Level);
    Assert.AreEqual(lvInfos, NewLog.Lines[3].Level);
    Assert.AreEqual(lvImportant, NewLog.Lines[4].Level);
    Assert.AreEqual(lvWarnings, NewLog.Lines[5].Level);
    Assert.AreEqual(lvErrors, NewLog.Lines[6].Level);
  finally
    FreeAndNil(NewLog);
  end;
end;


{ Multi-threaded Log Tests }

procedure TTestLogRam.TestMultiThreadedAddMsg;
var
  MTLog: TRamLog;
begin
  MTLog:= TRamLog.Create(False, nil, True);  { MultiThreaded = True }
  try
    { Basic operations should work the same in multi-threaded mode }
    MTLog.AddMsg('Message 1');
    MTLog.AddDebug('Debug 1');
    MTLog.AddError('Error 1');

    Assert.AreEqual(3, MTLog.Lines.Count);
    Assert.AreEqual('Message 1', MTLog.Lines[0].Msg);
    Assert.AreEqual(lvDebug, MTLog.Lines[1].Level);
    Assert.AreEqual(lvErrors, MTLog.Lines[2].Level);
  finally
    FreeAndNil(MTLog);
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestLogRam);

end.
