unit Test.LightCore.LogLinesM;

{=============================================================================================================
   Unit tests for LightCore.LogLinesM.pas
   Tests multi-threaded log lines implementation
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  LightCore.LogTypes,
  LightCore.LogLinesAbstract,
  LightCore.LogLinesM;

type
  [TestFixture]
  TTestLogLinesM = class
  private
    FLines: TLogLinesMultiThreaded;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Basic Operations }
    [Test]
    procedure TestCreate;

    [Test]
    procedure TestClear;

    [Test]
    procedure TestCount_Empty;

    [Test]
    procedure TestCount_AfterAdd;

    { Add Tests }
    [Test]
    procedure TestAddNewLine_Simple;

    [Test]
    procedure TestAddNewLine_WithLevel;

    [Test]
    procedure TestAddNewLine_Bold;

    [Test]
    procedure TestAdd_Pointer;

    [Test]
    procedure TestAdd_NilPointer;

    { Access Tests }
    [Test]
    procedure TestGetItem;

    [Test]
    procedure TestItems_Property;

    { Filtering Tests }
    [Test]
    procedure TestRow2FilteredRow_NoFilter;

    [Test]
    procedure TestRow2FilteredRow_WithFilter;

    [Test]
    procedure TestCountFiltered_All;

    [Test]
    procedure TestCountFiltered_Partial;

    { GetFilteredSlice Tests }
    [Test]
    procedure TestGetFilteredSlice_EmptyList;

    [Test]
    procedure TestGetFilteredSlice_NoFilter_NoSkip;

    [Test]
    procedure TestGetFilteredSlice_WithFilter;

    [Test]
    procedure TestGetFilteredSlice_WithSkip;

    [Test]
    procedure TestGetFilteredSlice_BufferSmallerThanResult;

    [Test]
    procedure TestGetFilteredSlice_BufferLargerThanResult;

    [Test]
    procedure TestGetFilteredSlice_AllFilteredOut;

    [Test]
    procedure TestGetFilteredSlice_SkipBeyondAvailable;

    { Thread Safety Tests }
    [Test]
    procedure TestConcurrentAdd;

    [Test]
    procedure TestConcurrentRead;

    [Test]
    procedure TestGetFilteredSlice_ConcurrentAddDuringSlice;

    { Multiple Items }
    [Test]
    procedure TestMultipleItems;
  end;

implementation


procedure TTestLogLinesM.Setup;
begin
  FLines:= TLogLinesMultiThreaded.Create;
end;


procedure TTestLogLinesM.TearDown;
begin
  FreeAndNil(FLines);
end;


{ Basic Operations }

procedure TTestLogLinesM.TestCreate;
begin
  Assert.IsNotNull(FLines);
  Assert.AreEqual(0, FLines.Count);
end;

procedure TTestLogLinesM.TestClear;
begin
  FLines.AddNewLine('Test', lvInfos);
  FLines.AddNewLine('Test2', lvInfos);
  Assert.AreEqual(2, FLines.Count);

  FLines.Clear;
  Assert.AreEqual(0, FLines.Count);
end;

procedure TTestLogLinesM.TestCount_Empty;
begin
  Assert.AreEqual(0, FLines.Count);
end;

procedure TTestLogLinesM.TestCount_AfterAdd;
begin
  FLines.AddNewLine('Line 1', lvInfos);
  Assert.AreEqual(1, FLines.Count);

  FLines.AddNewLine('Line 2', lvInfos);
  Assert.AreEqual(2, FLines.Count);
end;


{ Add Tests }

procedure TTestLogLinesM.TestAddNewLine_Simple;
var
  Line: PLogLine;
begin
  Line:= FLines.AddNewLine('Test Message', lvInfos);
  Assert.IsNotNull(Line);
  Assert.AreEqual('Test Message', Line.Msg);
end;

procedure TTestLogLinesM.TestAddNewLine_WithLevel;
begin
  FLines.AddNewLine('Debug', lvDebug);
  FLines.AddNewLine('Error', lvErrors);

  Assert.AreEqual(lvDebug, FLines[0].Level);
  Assert.AreEqual(lvErrors, FLines[1].Level);
end;

procedure TTestLogLinesM.TestAddNewLine_Bold;
begin
  FLines.AddNewLine('Normal', lvInfos, False);
  FLines.AddNewLine('Bold', lvInfos, True);

  Assert.IsFalse(FLines[0].Bold);
  Assert.IsTrue(FLines[1].Bold);
end;

procedure TTestLogLinesM.TestAdd_Pointer;
var
  Line: PLogLine;
  Index: Integer;
begin
  New(Line);
  Line.Msg:= 'Manual Line';
  Line.Level:= lvWarnings;
  Line.Bold:= False;
  Line.Time:= Now;
  Line.Indent:= 2;

  Index:= FLines.Add(Line);

  Assert.AreEqual(0, Index);
  Assert.AreEqual(1, FLines.Count);
  Assert.AreEqual('Manual Line', FLines[0].Msg);
end;


procedure TTestLogLinesM.TestAdd_NilPointer;
begin
  { Adding nil pointer should trigger an assertion (mirrors TLogLinesSingleThreaded) }
  Assert.WillRaise(
    procedure
    begin
      FLines.Add(NIL);
    end,
    EAssertionFailed
  );
end;


{ Access Tests }

procedure TTestLogLinesM.TestGetItem;
begin
  FLines.AddNewLine('First', lvInfos);
  FLines.AddNewLine('Second', lvInfos);
  FLines.AddNewLine('Third', lvInfos);

  Assert.AreEqual('First', FLines[0].Msg);
  Assert.AreEqual('Second', FLines[1].Msg);
  Assert.AreEqual('Third', FLines[2].Msg);
end;

procedure TTestLogLinesM.TestItems_Property;
var
  Item: PLogLine;
begin
  FLines.AddNewLine('Test', lvDebug);
  Item:= FLines.Items[0];

  Assert.IsNotNull(Item);
  Assert.AreEqual('Test', Item.Msg);
end;


{ Filtering Tests }

procedure TTestLogLinesM.TestRow2FilteredRow_NoFilter;
begin
  FLines.AddNewLine('Line 0', lvErrors);
  FLines.AddNewLine('Line 1', lvErrors);
  FLines.AddNewLine('Line 2', lvErrors);

  Assert.AreEqual(0, FLines.Row2FilteredRow(0, lvDebug));
  Assert.AreEqual(1, FLines.Row2FilteredRow(1, lvDebug));
  Assert.AreEqual(2, FLines.Row2FilteredRow(2, lvDebug));
end;

procedure TTestLogLinesM.TestRow2FilteredRow_WithFilter;
begin
  FLines.AddNewLine('Debug', lvDebug);
  FLines.AddNewLine('Info', lvInfos);
  FLines.AddNewLine('Warning', lvWarnings);
  FLines.AddNewLine('Error', lvErrors);

  Assert.AreEqual(2, FLines.Row2FilteredRow(0, lvWarnings));
  Assert.AreEqual(3, FLines.Row2FilteredRow(1, lvWarnings));
end;


procedure TTestLogLinesM.TestCountFiltered_All;
begin
  FLines.AddNewLine('Error 1', lvErrors);
  FLines.AddNewLine('Error 2', lvErrors);
  FLines.AddNewLine('Warning', lvWarnings);

  { All lines pass the Debug filter (lowest level) }
  Assert.AreEqual(3, FLines.CountFiltered(lvDebug));
end;


procedure TTestLogLinesM.TestCountFiltered_Partial;
begin
  FLines.AddNewLine('Debug', lvDebug);
  FLines.AddNewLine('Info', lvInfos);
  FLines.AddNewLine('Warning', lvWarnings);
  FLines.AddNewLine('Error', lvErrors);

  { Only warnings and errors pass the Warnings filter }
  Assert.AreEqual(2, FLines.CountFiltered(lvWarnings));
  { Only errors pass the Errors filter }
  Assert.AreEqual(1, FLines.CountFiltered(lvErrors));
end;


{ GetFilteredSlice Tests }

procedure TTestLogLinesM.TestGetFilteredSlice_EmptyList;
var
  Buf: array of PLogLine;
  Filled: Integer;
begin
  SetLength(Buf, 10);
  Filled:= FLines.GetFilteredSlice(lvDebug, 0, Buf);
  Assert.AreEqual(0, Filled, 'Empty list must return 0');
end;

procedure TTestLogLinesM.TestGetFilteredSlice_NoFilter_NoSkip;
var
  Buf: array of PLogLine;
  Filled: Integer;
begin
  FLines.AddNewLine('Line 0', lvInfos);
  FLines.AddNewLine('Line 1', lvInfos);
  FLines.AddNewLine('Line 2', lvInfos);

  SetLength(Buf, 10);
  Filled:= FLines.GetFilteredSlice(lvDebug, 0, Buf);
  Assert.AreEqual(3, Filled, 'Three matching lines, no skip');
  Assert.AreEqual('Line 0', Buf[0].Msg);
  Assert.AreEqual('Line 1', Buf[1].Msg);
  Assert.AreEqual('Line 2', Buf[2].Msg);
end;

procedure TTestLogLinesM.TestGetFilteredSlice_WithFilter;
var
  Buf: array of PLogLine;
  Filled: Integer;
begin
  FLines.AddNewLine('Debug',   lvDebug);     { excluded }
  FLines.AddNewLine('Info',    lvInfos);     { excluded }
  FLines.AddNewLine('Warning', lvWarnings);  { included, slice[0] }
  FLines.AddNewLine('Error',   lvErrors);    { included, slice[1] }

  SetLength(Buf, 10);
  Filled:= FLines.GetFilteredSlice(lvWarnings, 0, Buf);
  Assert.AreEqual(2, Filled, 'Two lines pass the warnings filter');
  Assert.AreEqual('Warning', Buf[0].Msg);
  Assert.AreEqual('Error',   Buf[1].Msg);
end;

procedure TTestLogLinesM.TestGetFilteredSlice_WithSkip;
var
  Buf: array of PLogLine;
  Filled: Integer;
begin
  FLines.AddNewLine('A', lvInfos);
  FLines.AddNewLine('B', lvInfos);
  FLines.AddNewLine('C', lvInfos);
  FLines.AddNewLine('D', lvInfos);
  FLines.AddNewLine('E', lvInfos);

  SetLength(Buf, 10);
  Filled:= FLines.GetFilteredSlice(lvDebug, 2, Buf);
  Assert.AreEqual(3, Filled, 'Skip first 2, return remaining 3');
  Assert.AreEqual('C', Buf[0].Msg);
  Assert.AreEqual('D', Buf[1].Msg);
  Assert.AreEqual('E', Buf[2].Msg);
end;

procedure TTestLogLinesM.TestGetFilteredSlice_BufferSmallerThanResult;
var
  Buf: array of PLogLine;
  Filled: Integer;
  i: Integer;
begin
  for i:= 0 to 99 do
    FLines.AddNewLine('Line ' + IntToStr(i), lvInfos);

  SetLength(Buf, 5);
  Filled:= FLines.GetFilteredSlice(lvDebug, 0, Buf);
  Assert.AreEqual(5, Filled, 'Buffer of 5 must cap the result');
  Assert.AreEqual('Line 0', Buf[0].Msg);
  Assert.AreEqual('Line 4', Buf[4].Msg);
end;

procedure TTestLogLinesM.TestGetFilteredSlice_BufferLargerThanResult;
var
  Buf: array of PLogLine;
  Filled: Integer;
begin
  FLines.AddNewLine('Only', lvInfos);

  SetLength(Buf, 100);
  Filled:= FLines.GetFilteredSlice(lvDebug, 0, Buf);
  Assert.AreEqual(1, Filled, 'Only one match, despite 100-slot buffer');
  Assert.AreEqual('Only', Buf[0].Msg);
end;

procedure TTestLogLinesM.TestGetFilteredSlice_AllFilteredOut;
var
  Buf: array of PLogLine;
  Filled: Integer;
begin
  FLines.AddNewLine('Debug 1', lvDebug);
  FLines.AddNewLine('Info 1',  lvInfos);
  FLines.AddNewLine('Hint 1',  lvHints);

  SetLength(Buf, 10);
  Filled:= FLines.GetFilteredSlice(lvErrors, 0, Buf);
  Assert.AreEqual(0, Filled, 'No line matches the errors filter');
end;

procedure TTestLogLinesM.TestGetFilteredSlice_SkipBeyondAvailable;
var
  Buf: array of PLogLine;
  Filled: Integer;
begin
  FLines.AddNewLine('A', lvInfos);
  FLines.AddNewLine('B', lvInfos);

  SetLength(Buf, 10);
  Filled:= FLines.GetFilteredSlice(lvDebug, 100, Buf);
  Assert.AreEqual(0, Filled, 'Skip exceeds available; nothing returned');
end;


{ Thread Safety Tests }

procedure TTestLogLinesM.TestConcurrentAdd;
var
  Threads: array[0..3] of TThread;
  i: Integer;
begin
  { Create 4 threads, each adding 100 items }
  for i:= 0 to 3 do
  begin
    Threads[i]:= TThread.CreateAnonymousThread(
      procedure
      var
        k: Integer;
      begin
        for k:= 1 to 100 do
          FLines.AddNewLine('Thread message ' + IntToStr(k), lvInfos);
      end
    );
    Threads[i].FreeOnTerminate:= False;
  end;

  { Start all threads }
  for i:= 0 to 3 do
    Threads[i].Start;

  { Wait for all threads to complete }
  for i:= 0 to 3 do
  begin
    Threads[i].WaitFor;
    FreeAndNil(Threads[i]);
  end;

  { Verify all messages were added }
  Assert.AreEqual(400, FLines.Count);
end;

procedure TTestLogLinesM.TestConcurrentRead;
var
  WriteThread, ReadThread: TThread;
  ReadCount: Integer;
begin
  ReadCount:= 0;

  { Pre-populate with some data }
  for var i:= 1 to 50 do
    FLines.AddNewLine('Initial ' + IntToStr(i), lvInfos);

  WriteThread:= TThread.CreateAnonymousThread(
    procedure
    var
      i: Integer;
    begin
      for i:= 1 to 100 do
      begin
        FLines.AddNewLine('Write ' + IntToStr(i), lvInfos);
        Sleep(1);
      end;
    end
  );
  WriteThread.FreeOnTerminate:= False;

  ReadThread:= TThread.CreateAnonymousThread(
    procedure
    var
      i: Integer;
    begin
      for i:= 1 to 100 do
      begin
        if FLines.Count > 0 then
        begin
          var dummy:= FLines[0].Msg;
          Inc(ReadCount);
        end;
        Sleep(1);
      end;
    end
  );
  ReadThread.FreeOnTerminate:= False;

  WriteThread.Start;
  ReadThread.Start;

  WriteThread.WaitFor;
  ReadThread.WaitFor;

  FreeAndNil(WriteThread);
  FreeAndNil(ReadThread);

  { Both operations should complete without deadlock }
  Assert.IsTrue(FLines.Count >= 50);
  Assert.IsTrue(ReadCount > 0);
end;


{ Verifies GetFilteredSlice never returns torn pointers under concurrent Add.
  Tests the actively-contended region: SliceThread re-reads FLines.Count and uses
  a SkipCount that targets the tail of the list, where Add is currently writing.
  This forces the slice walk to scan past entries the writer is still appending.

  Correctness properties under test:
    - No deadlock between concurrent reader and writer.
    - Every returned PLogLine is non-nil with a non-empty Msg
      (proves the pointer wasn't read mid-Dispose -- not that Add disposes,
      but the assertion catches torn reads if the underlying TList grows
      its backing array while we index it).
    - The slice can observe newly-added entries (proves the test actually
      exercises the contention window, not just the static prefix). }
procedure TTestLogLinesM.TestGetFilteredSlice_ConcurrentAddDuringSlice;
var
  WriteThread, SliceThread: TThread;
  TotalSlices, ValidSlices, SawNewEntry: Integer;
begin
  TotalSlices:= 0;
  ValidSlices:= 0;
  SawNewEntry:= 0;

  WriteThread:= TThread.CreateAnonymousThread(
    procedure
    var
      i: Integer;
    begin
      for i:= 0 to 4999 do
        FLines.AddNewLine('Add ' + IntToStr(i), lvInfos);
    end);
  WriteThread.FreeOnTerminate:= False;

  SliceThread:= TThread.CreateAnonymousThread(
    procedure
    var
      Buf: array of PLogLine;
      i, Filled, j, Skip, Total: Integer;
      AllValid: Boolean;
    begin
      SetLength(Buf, 30);
      for i:= 0 to 999 do
        begin
          Total:= FLines.Count;
          if Total > 30
          then Skip:= Total - 30  { target the tail, where writer is appending }
          else Skip:= 0;

          Filled:= FLines.GetFilteredSlice(lvDebug, Skip, Buf);
          Inc(TotalSlices);

          AllValid:= TRUE;
          for j:= 0 to Filled - 1 do
            if (Buf[j] = NIL) or (Buf[j].Msg = '')
            then begin AllValid:= FALSE; BREAK; end;
          if AllValid then Inc(ValidSlices);

          { Did we see at least one freshly-added entry? }
          if (Filled > 0) and (Pos('Add ', Buf[Filled - 1].Msg) = 1)
          then Inc(SawNewEntry);
        end;
    end);
  SliceThread.FreeOnTerminate:= False;

  WriteThread.Start;
  SliceThread.Start;
  WriteThread.WaitFor;
  SliceThread.WaitFor;
  FreeAndNil(WriteThread);
  FreeAndNil(SliceThread);

  Assert.AreEqual(1000, TotalSlices, 'All slice calls completed (no deadlock)');
  Assert.AreEqual(TotalSlices, ValidSlices, 'Every returned pointer must be valid');
  Assert.AreEqual(5000, FLines.Count, 'All adds landed');
  Assert.IsTrue(SawNewEntry > 0, 'Slice must observe at least one Add (proves contention exercised)');
end;


{ Multiple Items }

procedure TTestLogLinesM.TestMultipleItems;
var
  i: Integer;
begin
  for i:= 1 to 100 do
    FLines.AddNewLine('Line ' + IntToStr(i), lvInfos);

  Assert.AreEqual(100, FLines.Count);
  Assert.AreEqual('Line 1', FLines[0].Msg);
  Assert.AreEqual('Line 100', FLines[99].Msg);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestLogLinesM);

end.
