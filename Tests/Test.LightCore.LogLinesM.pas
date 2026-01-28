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

    { Thread Safety Tests }
    [Test]
    procedure TestConcurrentAdd;

    [Test]
    procedure TestConcurrentRead;

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


{ Thread Safety Tests }

procedure TTestLogLinesM.TestConcurrentAdd;
var
  Threads: array[0..3] of TThread;
  i, j: Integer;
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
