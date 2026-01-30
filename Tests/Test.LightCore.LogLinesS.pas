unit Test.LightCore.LogLinesS;

{=============================================================================================================
   2026.01.30
   Unit tests for LightCore.LogLinesS.pas
   Tests single-threaded log lines implementation
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  LightCore.LogTypes,
  LightCore.LogLinesAbstract,
  LightCore.LogLinesS,
  LightCore.StreamBuff;

type
  [TestFixture]
  TTestLogLinesS = class
  private
    FLines: TLogLinesSingleThreaded;
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
    procedure TestAddNewLine_Time;

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
    procedure TestRow2FilteredRow_NotFound;

    [Test]
    procedure TestRow2FilteredRow_EmptyList;

    [Test]
    procedure TestCountFiltered_All;

    [Test]
    procedure TestCountFiltered_Partial;

    [Test]
    procedure TestCountFiltered_None;

    { Multiple Items }
    [Test]
    procedure TestMultipleItems;

    [Test]
    procedure TestClearReleasesMemory;

    { Stream I/O Tests }
    [Test]
    procedure TestWriteReadStream;

    [Test]
    procedure TestReadStream_InvalidVersion;
  end;

implementation


procedure TTestLogLinesS.Setup;
begin
  FLines:= TLogLinesSingleThreaded.Create;
end;


procedure TTestLogLinesS.TearDown;
begin
  FreeAndNil(FLines);
end;


{ Basic Operations }

procedure TTestLogLinesS.TestCreate;
begin
  Assert.IsNotNull(FLines);
  Assert.AreEqual(0, FLines.Count);
end;

procedure TTestLogLinesS.TestClear;
begin
  FLines.AddNewLine('Test', lvInfos);
  FLines.AddNewLine('Test2', lvInfos);
  Assert.AreEqual(2, FLines.Count);

  FLines.Clear;
  Assert.AreEqual(0, FLines.Count);
end;

procedure TTestLogLinesS.TestCount_Empty;
begin
  Assert.AreEqual(0, FLines.Count);
end;

procedure TTestLogLinesS.TestCount_AfterAdd;
begin
  FLines.AddNewLine('Line 1', lvInfos);
  Assert.AreEqual(1, FLines.Count);

  FLines.AddNewLine('Line 2', lvInfos);
  Assert.AreEqual(2, FLines.Count);
end;


{ Add Tests }

procedure TTestLogLinesS.TestAddNewLine_Simple;
var
  Line: PLogLine;
begin
  Line:= FLines.AddNewLine('Test Message', lvInfos);
  Assert.IsNotNull(Line);
  Assert.AreEqual('Test Message', Line.Msg);
end;

procedure TTestLogLinesS.TestAddNewLine_WithLevel;
begin
  FLines.AddNewLine('Debug', lvDebug);
  FLines.AddNewLine('Error', lvErrors);

  Assert.AreEqual(lvDebug, FLines[0].Level);
  Assert.AreEqual(lvErrors, FLines[1].Level);
end;

procedure TTestLogLinesS.TestAddNewLine_Bold;
begin
  FLines.AddNewLine('Normal', lvInfos, False);
  FLines.AddNewLine('Bold', lvInfos, True);

  Assert.IsFalse(FLines[0].Bold);
  Assert.IsTrue(FLines[1].Bold);
end;

procedure TTestLogLinesS.TestAddNewLine_Time;
var
  Before, After: TDateTime;
begin
  Before:= Now;
  FLines.AddNewLine('Test', lvInfos);
  After:= Now;

  Assert.IsTrue(FLines[0].Time >= Before);
  Assert.IsTrue(FLines[0].Time <= After);
end;

procedure TTestLogLinesS.TestAdd_Pointer;
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
  Assert.AreEqual(2, FLines[0].Indent);
end;

procedure TTestLogLinesS.TestAdd_NilPointer;
begin
  { Adding nil pointer should trigger an assertion }
  Assert.WillRaise(
    procedure
    begin
      FLines.Add(NIL);
    end,
    EAssertionFailed
  );
end;


{ Access Tests }

procedure TTestLogLinesS.TestGetItem;
begin
  FLines.AddNewLine('First', lvInfos);
  FLines.AddNewLine('Second', lvInfos);
  FLines.AddNewLine('Third', lvInfos);

  Assert.AreEqual('First', FLines[0].Msg);
  Assert.AreEqual('Second', FLines[1].Msg);
  Assert.AreEqual('Third', FLines[2].Msg);
end;

procedure TTestLogLinesS.TestItems_Property;
var
  Item: PLogLine;
begin
  FLines.AddNewLine('Test', lvDebug);
  Item:= FLines.Items[0];

  Assert.IsNotNull(Item);
  Assert.AreEqual('Test', Item.Msg);
  Assert.AreEqual(lvDebug, Item.Level);
end;


{ Filtering Tests }

procedure TTestLogLinesS.TestRow2FilteredRow_NoFilter;
begin
  FLines.AddNewLine('Line 0', lvErrors);
  FLines.AddNewLine('Line 1', lvErrors);
  FLines.AddNewLine('Line 2', lvErrors);

  { When all lines pass the filter, row index = filtered index }
  Assert.AreEqual(0, FLines.Row2FilteredRow(0, lvDebug));
  Assert.AreEqual(1, FLines.Row2FilteredRow(1, lvDebug));
  Assert.AreEqual(2, FLines.Row2FilteredRow(2, lvDebug));
end;

procedure TTestLogLinesS.TestRow2FilteredRow_WithFilter;
begin
  FLines.AddNewLine('Debug', lvDebug);      { Index 0, filtered out }
  FLines.AddNewLine('Info', lvInfos);       { Index 1, filtered out }
  FLines.AddNewLine('Warning', lvWarnings); { Index 2, visible row 0 }
  FLines.AddNewLine('Error', lvErrors);     { Index 3, visible row 1 }

  { Filter to show only warnings and above }
  Assert.AreEqual(2, FLines.Row2FilteredRow(0, lvWarnings));  { First visible = index 2 }
  Assert.AreEqual(3, FLines.Row2FilteredRow(1, lvWarnings));  { Second visible = index 3 }
end;

procedure TTestLogLinesS.TestRow2FilteredRow_NotFound;
begin
  FLines.AddNewLine('Info', lvInfos);
  FLines.AddNewLine('Debug', lvDebug);

  { No errors in the list, so row 0 of errors filter returns -1 }
  Assert.AreEqual(-1, FLines.Row2FilteredRow(0, lvErrors));
end;

procedure TTestLogLinesS.TestRow2FilteredRow_EmptyList;
begin
  { Empty list should return -1 for any row }
  Assert.AreEqual(-1, FLines.Row2FilteredRow(0, lvDebug));
  Assert.AreEqual(-1, FLines.Row2FilteredRow(5, lvDebug));
end;


procedure TTestLogLinesS.TestCountFiltered_All;
begin
  FLines.AddNewLine('Error 1', lvErrors);
  FLines.AddNewLine('Error 2', lvErrors);
  FLines.AddNewLine('Warning', lvWarnings);

  { All lines pass the Debug filter (lowest level) }
  Assert.AreEqual(3, FLines.CountFiltered(lvDebug));
end;


procedure TTestLogLinesS.TestCountFiltered_Partial;
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


procedure TTestLogLinesS.TestCountFiltered_None;
begin
  FLines.AddNewLine('Debug', lvDebug);
  FLines.AddNewLine('Info', lvInfos);

  { No errors in the list }
  Assert.AreEqual(0, FLines.CountFiltered(lvErrors));
end;


{ Multiple Items }

procedure TTestLogLinesS.TestMultipleItems;
var
  i: Integer;
begin
  for i:= 1 to 100 do
    FLines.AddNewLine('Line ' + IntToStr(i), lvInfos);

  Assert.AreEqual(100, FLines.Count);
  Assert.AreEqual('Line 1', FLines[0].Msg);
  Assert.AreEqual('Line 100', FLines[99].Msg);
end;

procedure TTestLogLinesS.TestClearReleasesMemory;
var
  i: Integer;
begin
  { Add many items }
  for i:= 1 to 1000 do
    FLines.AddNewLine('Line ' + IntToStr(i), lvInfos);

  Assert.AreEqual(1000, FLines.Count);

  { Clear should release all memory }
  FLines.Clear;
  Assert.AreEqual(0, FLines.Count);

  { Should be able to add again }
  FLines.AddNewLine('After clear', lvInfos);
  Assert.AreEqual(1, FLines.Count);
end;


{ Stream I/O Tests }

procedure TTestLogLinesS.TestWriteReadStream;
var
  FilePath: string;
  Stream: TLightStream;
  NewLines: TLogLinesSingleThreaded;
begin
  { Add some test data }
  FLines.AddNewLine('Message 1', lvDebug);
  FLines.AddNewLine('Message 2', lvWarnings, True);
  FLines.AddNewLine('Message 3', lvErrors);

  { Write to stream }
  FilePath:= TPath.Combine(TPath.GetTempPath, 'TestLogLines_' + TGUID.NewGuid.ToString + '.dat');
  Stream:= TLightStream.CreateWrite(FilePath);
  try
    FLines.WriteToStream(Stream);
  finally
    FreeAndNil(Stream);
  end;

  { Read into new instance }
  NewLines:= TLogLinesSingleThreaded.Create;
  try
    Stream:= TLightStream.CreateRead(FilePath);
    try
      NewLines.ReadFromStream(Stream);
    finally
      FreeAndNil(Stream);
    end;

    Assert.AreEqual(3, NewLines.Count);
    Assert.AreEqual('Message 1', NewLines[0].Msg);
    Assert.AreEqual(lvDebug, NewLines[0].Level);
    Assert.AreEqual('Message 2', NewLines[1].Msg);
    Assert.IsTrue(NewLines[1].Bold);
    Assert.AreEqual(lvWarnings, NewLines[1].Level);
    Assert.AreEqual('Message 3', NewLines[2].Msg);
    Assert.AreEqual(lvErrors, NewLines[2].Level);
  finally
    FreeAndNil(NewLines);
    DeleteFile(FilePath);
  end;
end;

procedure TTestLogLinesS.TestReadStream_InvalidVersion;
var
  FilePath: string;
  Stream: TLightStream;
begin
  { Write an invalid header }
  FilePath:= TPath.Combine(TPath.GetTempPath, 'TestInvalidVersion_' + TGUID.NewGuid.ToString + '.dat');
  Stream:= TLightStream.CreateWrite(FilePath);
  try
    Stream.WriteHeader('TLogLines', 99);  { Invalid version }
    Stream.WritePadding;
  finally
    FreeAndNil(Stream);
  end;

  { Reading should raise an exception for unsupported version }
  Stream:= TLightStream.CreateRead(FilePath);
  try
    Assert.WillRaise(
      procedure
      begin
        FLines.ReadFromStream(Stream);
      end,
      Exception
    );
  finally
    FreeAndNil(Stream);
    DeleteFile(FilePath);
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestLogLinesS);

end.
