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
  The save writes LargeLogSave.logbin into the AppData folder of the test EXE; the test removes it again if it was not there before. }
procedure TTestLogRam.TestOverflow_PopulatesEveryObserver;
var
  ObserverA, ObserverB: TMockLogObserver;
  RefA, RefB: ILogObserver;
  LogFull: TRamLog;
  SaveFile: string;
  FileWasThere: Boolean;
begin
  SaveFile:= TAppDataCore.AppDataFolder + 'LargeLogSave.logbin';
  FileWasThere:= FileExists(SaveFile);

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
    FreeAndNil(LogFull);
    if NOT FileWasThere AND FileExists(SaveFile)
    then System.SysUtils.DeleteFile(SaveFile);
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
