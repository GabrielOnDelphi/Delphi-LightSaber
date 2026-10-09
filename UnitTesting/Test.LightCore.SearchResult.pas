unit Test.LightCore.SearchResult;

{=============================================================================================================
   2026.01.30
   Unit tests for LightCore.SearchResult
   Tests IDE position tracking and search result collection classes

   Requires: TESTINSIGHT compiler directive for TestInsight integration
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Generics.Collections,
  LightCore.SearchResult;

type
  [TestFixture]
  TTestSearchResult = class
  private
    FSearchResult: TSearchResult;
    FSearchResults: TSearchResults;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { TSearchResult - Constructor Tests }
    [Test]
    procedure TestCreate_SetsFileName;

    [Test]
    procedure TestCreate_InitializesEmptyPositions;

    { TSearchResult - AddNewPos Tests }
    [Test]
    procedure TestAddNewPos_FullOverload;

    [Test]
    procedure TestAddNewPos_SimpleOverload;

    [Test]
    procedure TestAddNewPos_WarningOnlyOverload;

    [Test]
    procedure TestAddNewPos_MultiplePositions;

    { TSearchResult - Found/Count Tests }
    [Test]
    procedure TestFound_WhenEmpty;

    [Test]
    procedure TestFound_WhenNotEmpty;

    [Test]
    procedure TestCount_WhenEmpty;

    [Test]
    procedure TestCount_AfterAddingPositions;

    { TSearchResult - Clear Tests }
    [Test]
    procedure TestClear_RemovesAllPositions;

    [Test]
    procedure TestClear_OnEmptyList;

    { TSearchResult - AsString Tests }
    [Test]
    procedure TestAsString_SinglePosition;

    [Test]
    procedure TestAsString_MultiplePositions;

    [Test]
    procedure TestAsString_WhenEmpty;

    { TSearchResult - PositionsAsString Tests }
    [Test]
    procedure TestPositionsAsString_SinglePosition;

    [Test]
    procedure TestPositionsAsString_MultiplePositions;

    [Test]
    procedure TestPositionsAsString_WhenEmpty;

    { TSearchResult - Property Tests }
    [Test]
    procedure TestFileName_ReturnsCorrectValue;

    [Test]
    procedure TestPositions_ReturnsListReference;

    { TSearchResults - Tests }
    [Test]
    procedure TestSearchResults_Create;

    [Test]
    procedure TestSearchResults_AddAndRetrieve;

    [Test]
    procedure TestSearchResults_Last_SingleItem;

    [Test]
    procedure TestSearchResults_Last_MultipleItems;

    [Test]
    procedure TestSearchResults_OwnsObjects;
  end;

implementation

uses
  System.Classes;

TYPE
  { A TSearchResult that writes its FileName into Log when it is destroyed, so a test can see whether an owner freed it }
  TLoggedSearchResult = class(TSearchResult)
  private
    FLog: TStrings;
  public
    constructor Create(CONST aFileName: string; aLog: TStrings);
    destructor Destroy; override;
  end;


constructor TLoggedSearchResult.Create(CONST aFileName: string; aLog: TStrings);
begin
  inherited Create(aFileName);
  FLog:= aLog;
end;


destructor TLoggedSearchResult.Destroy;
begin
  FLog.Add(FileName);
  inherited Destroy;
end;


{ TTestSearchResult }

procedure TTestSearchResult.Setup;
begin
  FSearchResult:= TSearchResult.Create('TestFile.pas');
  FSearchResults:= TSearchResults.Create;
end;


procedure TTestSearchResult.TearDown;
begin
  FreeAndNil(FSearchResult);
  FreeAndNil(FSearchResults);
end;


{ Constructor Tests }

procedure TTestSearchResult.TestCreate_SetsFileName;
begin
  Assert.AreEqual('TestFile.pas', FSearchResult.FileName);
end;


procedure TTestSearchResult.TestCreate_InitializesEmptyPositions;
begin
  Assert.IsNotNull(FSearchResult.Positions);
  Assert.AreEqual(0, FSearchResult.Positions.Count);
end;


{ AddNewPos Tests }

procedure TTestSearchResult.TestAddNewPos_FullOverload;
var
  Pos: TIDEPosition;
begin
  FSearchResult.AddNewPos(10, 5, 'var x: Integer;', 'x', 'Variable not used');

  Assert.AreEqual(1, FSearchResult.Count);
  Pos:= FSearchResult.Positions[0];
  Assert.AreEqual(10, Pos.LinePos);
  Assert.AreEqual(5, Pos.ColumnPos);
  Assert.AreEqual('var x: Integer;', Pos.CodeLine);
  Assert.AreEqual('x', Pos.Offender);
  Assert.AreEqual('Variable not used', Pos.WarningMsg);
end;


procedure TTestSearchResult.TestAddNewPos_SimpleOverload;
var
  Pos: TIDEPosition;
begin
  FSearchResult.AddNewPos(25, 10, 'procedure Foo;');

  Assert.AreEqual(1, FSearchResult.Count);
  Pos:= FSearchResult.Positions[0];
  Assert.AreEqual(25, Pos.LinePos);
  Assert.AreEqual(10, Pos.ColumnPos);
  Assert.AreEqual('procedure Foo;', Pos.CodeLine);
  Assert.AreEqual('', Pos.WarningMsg);
  Assert.AreEqual('', Pos.Offender);
end;


procedure TTestSearchResult.TestAddNewPos_WarningOnlyOverload;
var
  Pos: TIDEPosition;
begin
  FSearchResult.AddNewPos('File contains deprecated code');

  Assert.AreEqual(1, FSearchResult.Count);
  Pos:= FSearchResult.Positions[0];
  Assert.AreEqual(1, Pos.LinePos);
  Assert.AreEqual(1, Pos.ColumnPos);
  Assert.AreEqual('', Pos.CodeLine);
  Assert.AreEqual('File contains deprecated code', Pos.WarningMsg);
  Assert.AreEqual('', Pos.Offender);
end;


procedure TTestSearchResult.TestAddNewPos_MultiplePositions;
begin
  FSearchResult.AddNewPos(10, 1, 'Line 10');
  FSearchResult.AddNewPos(20, 2, 'Line 20');
  FSearchResult.AddNewPos(30, 3, 'Line 30');

  Assert.AreEqual(3, FSearchResult.Count);
  Assert.AreEqual(10, FSearchResult.Positions[0].LinePos);
  Assert.AreEqual(20, FSearchResult.Positions[1].LinePos);
  Assert.AreEqual(30, FSearchResult.Positions[2].LinePos);
end;


{ Found/Count Tests }

procedure TTestSearchResult.TestFound_WhenEmpty;
begin
  Assert.IsFalse(FSearchResult.Found);
end;


procedure TTestSearchResult.TestFound_WhenNotEmpty;
begin
  FSearchResult.AddNewPos(1, 1, 'code');
  Assert.IsTrue(FSearchResult.Found);
end;


procedure TTestSearchResult.TestCount_WhenEmpty;
begin
  Assert.AreEqual(0, FSearchResult.Count);
end;


procedure TTestSearchResult.TestCount_AfterAddingPositions;
begin
  FSearchResult.AddNewPos(1, 1, 'Line 1');
  Assert.AreEqual(1, FSearchResult.Count);

  FSearchResult.AddNewPos(2, 1, 'Line 2');
  Assert.AreEqual(2, FSearchResult.Count);

  FSearchResult.AddNewPos(3, 1, 'Line 3');
  Assert.AreEqual(3, FSearchResult.Count);
end;


{ Clear Tests }

procedure TTestSearchResult.TestClear_RemovesAllPositions;
begin
  FSearchResult.AddNewPos(1, 1, 'Line 1');
  FSearchResult.AddNewPos(2, 1, 'Line 2');
  FSearchResult.AddNewPos(3, 1, 'Line 3');

  Assert.AreEqual(3, FSearchResult.Count);

  FSearchResult.Clear;

  Assert.AreEqual(0, FSearchResult.Count);
  Assert.IsFalse(FSearchResult.Found);
end;


procedure TTestSearchResult.TestClear_OnEmptyList;
begin
  FSearchResult.Clear;
  Assert.AreEqual(0, FSearchResult.Count);
end;


{ AsString Tests }

procedure TTestSearchResult.TestAsString_SinglePosition;
var
  Output: string;
begin
  FSearchResult.AddNewPos(10, 1, '  var x: Integer;', 'x', 'Unused variable');
  Output:= FSearchResult.AsString;

  { Header line, the trimmed code line, then 'Offender WarningMsg'; the last CRLF is removed }
  Assert.AreEqual('   Line 10:'           + #13#10 +
                  '   var x: Integer;'    + #13#10 +
                  '   x Unused variable', Output);
end;


procedure TTestSearchResult.TestAsString_MultiplePositions;
var
  Output: string;
begin
  FSearchResult.AddNewPos(10, 1, 'Line 10 code', '', 'Warning 1');
  FSearchResult.AddNewPos(20, 1, 'Line 20 code', '', 'Warning 2');
  Output:= FSearchResult.AsString;

  { Each position gives 3 lines. The Offender is empty, so the third line is 3 spaces + ' ' + WarningMsg. }
  Assert.AreEqual('   Line 10:'     + #13#10 +
                  '   Line 10 code' + #13#10 +
                  '    Warning 1'   + #13#10 +
                  '   Line 20:'     + #13#10 +
                  '   Line 20 code' + #13#10 +
                  '    Warning 2', Output);
end;


procedure TTestSearchResult.TestAsString_WhenEmpty;
begin
  Assert.AreEqual('', FSearchResult.AsString);
end;


{ PositionsAsString Tests }

procedure TTestSearchResult.TestPositionsAsString_SinglePosition;
begin
  FSearchResult.AddNewPos(42, 1, 'code');
  Assert.AreEqual('42', FSearchResult.PositionsAsString);
end;


procedure TTestSearchResult.TestPositionsAsString_MultiplePositions;
begin
  FSearchResult.AddNewPos(10, 1, 'code');
  FSearchResult.AddNewPos(20, 1, 'code');
  FSearchResult.AddNewPos(30, 1, 'code');
  Assert.AreEqual('10, 20, 30', FSearchResult.PositionsAsString);
end;


procedure TTestSearchResult.TestPositionsAsString_WhenEmpty;
begin
  Assert.AreEqual('', FSearchResult.PositionsAsString);
end;


{ Property Tests }

procedure TTestSearchResult.TestFileName_ReturnsCorrectValue;
var
  SR: TSearchResult;
begin
  SR:= TSearchResult.Create('C:\Projects\MyFile.pas');
  try
    Assert.AreEqual('C:\Projects\MyFile.pas', SR.FileName);
  finally
    FreeAndNil(SR);
  end;
end;


procedure TTestSearchResult.TestPositions_ReturnsListReference;
var
  List: TList<TIDEPosition>;
begin
  List:= FSearchResult.Positions;
  Assert.IsNotNull(List);
  Assert.AreSame(List, FSearchResult.Positions, 'Every read must return the same list object');

  { A list taken BEFORE the add sees the add: it is the object's own list, not a copy }
  FSearchResult.AddNewPos(7, 3, 'code');
  Assert.AreEqual(1, List.Count, 'The list read earlier must see the new position');
  Assert.AreEqual(7, List[0].LinePos);

  { A change made through the reference reaches the object }
  List.Clear;
  Assert.AreEqual(0, FSearchResult.Count, 'Clearing the returned list must clear the object''s positions');
end;


{ TSearchResults Tests }

procedure TTestSearchResult.TestSearchResults_Create;
var
  Fresh: TSearchResults;
begin
  Fresh:= TSearchResults.Create;
  try
    Assert.AreEqual(0, Fresh.Count, 'A new collection must be empty');
    { TSearchResults declares no constructor, so it keeps the one of TObjectList<T>, which owns its items }
    Assert.IsTrue(Fresh.OwnsObjects, 'A new collection must own the results it holds');
  finally
    FreeAndNil(Fresh);
  end;
end;


procedure TTestSearchResult.TestSearchResults_AddAndRetrieve;
var
  SR: TSearchResult;
begin
  SR:= TSearchResult.Create('File1.pas');
  SR.AddNewPos(10, 1, 'code');

  FSearchResults.Add(SR);

  Assert.AreEqual(1, FSearchResults.Count);
  Assert.AreEqual('File1.pas', FSearchResults[0].FileName);
  Assert.AreEqual(1, FSearchResults[0].Count);
end;


procedure TTestSearchResult.TestSearchResults_Last_SingleItem;
var
  SR: TSearchResult;
begin
  SR:= TSearchResult.Create('OnlyFile.pas');
  FSearchResults.Add(SR);

  Assert.AreSame(SR, FSearchResults.Last);
end;


procedure TTestSearchResult.TestSearchResults_Last_MultipleItems;
var
  SR1, SR2, SR3: TSearchResult;
begin
  SR1:= TSearchResult.Create('File1.pas');
  SR2:= TSearchResult.Create('File2.pas');
  SR3:= TSearchResult.Create('File3.pas');

  FSearchResults.Add(SR1);
  FSearchResults.Add(SR2);
  FSearchResults.Add(SR3);

  Assert.AreSame(SR3, FSearchResults.Last);
  Assert.AreEqual('File3.pas', FSearchResults.Last.FileName);
end;


procedure TTestSearchResult.TestSearchResults_OwnsObjects;
var
  SR: TSearchResult;
  DestroyLog: TStringList;
begin
  DestroyLog:= TStringList.Create;
  try
    SR:= TLoggedSearchResult.Create('Test.pas', DestroyLog);
    SR.AddNewPos(1, 1, 'test');

    FSearchResults.Add(SR);
    FSearchResults.Clear;

    Assert.AreEqual(0, FSearchResults.Count);
    { The owner must have freed the result: its destructor wrote its FileName into the log }
    Assert.AreEqual(1, DestroyLog.Count, 'Clear must free the result the collection owns');
    Assert.AreEqual('Test.pas', DestroyLog[0]);
  finally
    FreeAndNil(DestroyLog);
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestSearchResult);

end.
