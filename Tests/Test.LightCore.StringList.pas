unit Test.LightCore.StringList;

{=============================================================================================================
   Unit tests for LightCore.StringList
   Tests TStringList class helper functions
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  LightCore.StringList;

type
  [TestFixture]
  TTestStringList = class
  private
    FSL: TStringList;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { RemoveEmptyLines Tests }
    [Test]
    procedure TestRemoveEmptyLines;

    [Test]
    procedure TestRemoveEmptyLines_AllEmpty;

    [Test]
    procedure TestRemoveEmptyLines_NoneEmpty;

    { RemoveDuplicates Tests }
    [Test]
    procedure TestRemoveDuplicates;

    [Test]
    procedure TestRemoveDuplicates_CaseSensitive;

    [Test]
    procedure TestRemoveDuplicateString;

    [Test]
    procedure TestRemoveDuplicateString_CaseInsensitive;

    [Test]
    procedure TestRemoveDuplicateString_NotFound;

    { Trim Tests }
    [Test]
    procedure TestTrim;

    [Test]
    procedure TestTrim_MixedWhitespace;

    { Shuffle Tests }
    [Test]
    procedure TestShuffle;

    [Test]
    procedure TestShuffle_PreservesAllItems;

    { Sort Tests }
    [Test]
    procedure TestSortReverse;

    [Test]
    procedure TestSortReverse_Numbers;

    { HighestString Tests }
    [Test]
    procedure TestHighestString;

    [Test]
    procedure TestHighestString_Empty;

    [Test]
    procedure TestHighestString_SingleItem;

    { FindLine Tests }
    [Test]
    procedure TestFindLine;

    [Test]
    procedure TestFindLine_FirstLine;

    [Test]
    procedure TestFindLine_CaseSensitive;

    { RemoveLines Tests }
    [Test]
    procedure TestRemoveLines;

    [Test]
    procedure TestRemoveLines_CaseInsensitive;

    [Test]
    procedure TestRemoveLines_NoMatch;

    { KeepLines Tests }
    [Test]
    procedure TestKeepLines;

    [Test]
    procedure TestKeepLines_CaseInsensitive;

    [Test]
    procedure TestKeepLines_NoMatch;

    { Concatenate Tests }
    [Test]
    procedure TestConcatenate;

    [Test]
    procedure TestConcatenate_Empty;

    [Test]
    procedure TestConcatenate_SingleItem;

    { GetTopLines Tests }
    [Test]
    procedure TestGetTopLines;

    [Test]
    procedure TestGetTopLines_MoreThanAvailable;

    [Test]
    procedure TestGetTopLines_WithEmptyLines;

    [Test]
    procedure TestGetTopLines_IncludeEmpty;

    { RemoveTopLines Tests }
    [Test]
    procedure TestRemoveTopLines;

    [Test]
    procedure TestRemoveTopLines_All;

    { Standalone Functions Tests }
    [Test]
    procedure TestString2TSL;

    [Test]
    procedure TestString2TSL_Empty;

    [Test]
    procedure TestExtractTopLines;

    [Test]
    procedure TestFindLine_StandAlone;

    [Test]
    procedure TestFindLine_StandAlone_NotFound;
  end;

implementation

procedure TTestStringList.Setup;
begin
  FSL := TStringList.Create;
end;

procedure TTestStringList.TearDown;
begin
  FreeAndNil(FSL);
end;

procedure TTestStringList.TestRemoveEmptyLines;
begin
  FSL.Add('Line1');
  FSL.Add('');
  FSL.Add('Line2');
  FSL.Add('');
  FSL.Add('');
  FSL.Add('Line3');

  FSL.RemoveEmptyLines;

  Assert.AreEqual(3, FSL.Count, 'Should have 3 non-empty lines');
  Assert.AreEqual('Line1', FSL[0]);
  Assert.AreEqual('Line2', FSL[1]);
  Assert.AreEqual('Line3', FSL[2]);
end;

procedure TTestStringList.TestRemoveDuplicates;
begin
  FSL.Add('Apple');
  FSL.Add('Banana');
  FSL.Add('Apple');
  FSL.Add('Cherry');
  FSL.Add('Banana');

  FSL.RemoveDuplicates;

  Assert.AreEqual(3, FSL.Count, 'Should have 3 unique items');
  { Note: RemoveDuplicates sorts the list }
end;

procedure TTestStringList.TestRemoveDuplicateString;
begin
  FSL.Add('First');
  FSL.Add('Second');
  FSL.Add('Third');

  FSL.RemoveDuplicateString('Second');

  Assert.AreEqual(2, FSL.Count, 'Should have 2 items after removal');
  Assert.AreEqual('First', FSL[0]);
  Assert.AreEqual('Third', FSL[1]);
end;

procedure TTestStringList.TestTrim;
begin
  FSL.Add('  Spaces  ');
  FSL.Add(#9 + 'Tab' + #9);
  FSL.Add('Normal');

  FSL.Trim;

  Assert.AreEqual('Spaces', FSL[0], 'Spaces should be trimmed');
  Assert.AreEqual('Tab', FSL[1], 'Tabs should be trimmed');
  Assert.AreEqual('Normal', FSL[2], 'Normal string should be unchanged');
end;

procedure TTestStringList.TestShuffle;
var
  OriginalText: string;
  ShuffledText: string;
  i: Integer;
  OrderChanged: Boolean;
begin
  for i:= 0 to 9 do
    FSL.Add(IntToStr(i));

  OriginalText:= FSL.Text;

  { Shuffle multiple times to ensure at least one changes the order }
  OrderChanged:= False;
  for i:= 1 to 10 do
  begin
    FSL.Shuffle;
    ShuffledText:= FSL.Text;
    if ShuffledText <> OriginalText
    then
    begin
      OrderChanged:= True;
      Break;
    end;
    { Reset for next try }
    FSL.Clear;
    for var j:= 0 to 9 do
      FSL.Add(IntToStr(j));
  end;

  Assert.AreEqual(10, FSL.Count, 'Count should remain the same after shuffle');
  { With 10 items, probability of same order after 10 shuffles is astronomically low }
  Assert.IsTrue(OrderChanged, 'Shuffle should change order at least once in 10 tries');
end;

procedure TTestStringList.TestSortReverse;
begin
  FSL.Add('A');
  FSL.Add('C');
  FSL.Add('B');

  FSL.SortReverse;

  Assert.AreEqual('C', FSL[0], 'First should be C (highest)');
  Assert.AreEqual('B', FSL[1], 'Second should be B');
  Assert.AreEqual('A', FSL[2], 'Third should be A (lowest)');
end;

procedure TTestStringList.TestHighestString;
begin
  FSL.Add('ABC');
  FSL.Add('ABCD');
  FSL.Add('AB');

  Assert.AreEqual('ABCD', FSL.HighestString, 'ABCD is lexicographically highest');
end;

procedure TTestStringList.TestFindLine;
begin
  FSL.Add('First line');
  FSL.Add('Second line with keyword');
  FSL.Add('Third line');

  Assert.AreEqual(1, FSL.FindLine('keyword'), 'Should find keyword in line 1');
  Assert.AreEqual(-1, FSL.FindLine('notfound'), 'Should return -1 for not found');
end;

procedure TTestStringList.TestRemoveLines;
var
  Removed: Integer;
begin
  FSL.Add('Keep this');
  FSL.Add('Remove this bad line');
  FSL.Add('Keep this too');
  FSL.Add('Another bad line here');

  Removed := FSL.RemoveLines('bad');

  Assert.AreEqual(2, Removed, 'Should remove 2 lines');
  Assert.AreEqual(2, FSL.Count, 'Should have 2 lines remaining');
end;

procedure TTestStringList.TestKeepLines;
var
  Removed: Integer;
begin
  FSL.Add('Keep: important');
  FSL.Add('Remove: garbage');
  FSL.Add('Keep: also important');

  Removed := FSL.KeepLines('important');

  Assert.AreEqual(1, Removed, 'Should remove 1 line');
  Assert.AreEqual(2, FSL.Count, 'Should have 2 lines remaining');
end;

procedure TTestStringList.TestConcatenate;
begin
  FSL.Add('A');
  FSL.Add('B');
  FSL.Add('C');

  Assert.AreEqual('A;B;C;', FSL.Concatenate(';'), 'Should concatenate with semicolons');
end;

procedure TTestStringList.TestGetTopLines;
begin
  FSL.Add('First');
  FSL.Add('Second');
  FSL.Add('Third');
  FSL.Add('Fourth');

  Assert.AreEqual('First'#13#10'Second', FSL.GetTopLines(2), 'Should return top 2 lines');
end;

procedure TTestStringList.TestRemoveTopLines;
begin
  FSL.Add('First');
  FSL.Add('Second');
  FSL.Add('Third');
  FSL.Add('Fourth');

  FSL.RemoveTopLines(2);

  Assert.AreEqual(2, FSL.Count, 'Should have 2 lines remaining');
  Assert.AreEqual('Third', FSL[0], 'First remaining should be Third');
end;

procedure TTestStringList.TestString2TSL;
var
  TSL: TStringList;
begin
  TSL := String2TSL('Line1'#13#10'Line2'#13#10'Line3');
  try
    Assert.AreEqual(3, TSL.Count, 'Should have 3 lines');
    Assert.AreEqual('Line1', TSL[0]);
    Assert.AreEqual('Line2', TSL[1]);
    Assert.AreEqual('Line3', TSL[2]);
  finally
    FreeAndNil(TSL);
  end;
end;

procedure TTestStringList.TestExtractTopLines;
var
  Text, Result: string;
begin
  Text := 'First'#13#10'Second'#13#10'Third'#13#10'Fourth';
  Result := ExtractTopLines(Text, 2);

  Assert.AreEqual('First'#13#10'Second', Result, 'Should extract top 2 lines');
end;

procedure TTestStringList.TestFindLine_StandAlone;
var
  Haystack, ResultStr: string;
begin
  Haystack:= 'First line'#13#10'Second with needle'#13#10'Third line';

  ResultStr:= FindLine('needle', Haystack);

  Assert.AreEqual('Second with needle', ResultStr, 'Should find and return the line containing needle');
end;


{ Additional Tests }

procedure TTestStringList.TestRemoveEmptyLines_AllEmpty;
begin
  FSL.Add('');
  FSL.Add('');
  FSL.Add('');

  FSL.RemoveEmptyLines;

  Assert.AreEqual(0, FSL.Count, 'All lines should be removed');
end;


procedure TTestStringList.TestRemoveEmptyLines_NoneEmpty;
begin
  FSL.Add('Line1');
  FSL.Add('Line2');
  FSL.Add('Line3');

  FSL.RemoveEmptyLines;

  Assert.AreEqual(3, FSL.Count, 'No lines should be removed');
end;


procedure TTestStringList.TestRemoveDuplicates_CaseSensitive;
begin
  FSL.Add('Apple');
  FSL.Add('apple');
  FSL.Add('APPLE');

  FSL.RemoveDuplicates;

  { RemoveDuplicates is case-sensitive, so all three are unique }
  Assert.AreEqual(3, FSL.Count, 'Case variations should be considered unique');
end;


procedure TTestStringList.TestRemoveDuplicateString_CaseInsensitive;
begin
  FSL.Add('First');
  FSL.Add('SECOND');
  FSL.Add('Third');

  { RemoveDuplicateString uses SameText which is case-insensitive }
  FSL.RemoveDuplicateString('second');

  Assert.AreEqual(2, FSL.Count, 'Should remove despite case difference');
  Assert.AreEqual('First', FSL[0]);
  Assert.AreEqual('Third', FSL[1]);
end;


procedure TTestStringList.TestRemoveDuplicateString_NotFound;
begin
  FSL.Add('First');
  FSL.Add('Second');
  FSL.Add('Third');

  FSL.RemoveDuplicateString('Fourth');

  Assert.AreEqual(3, FSL.Count, 'Nothing should be removed if string not found');
end;


procedure TTestStringList.TestTrim_MixedWhitespace;
begin
  FSL.Add(#9#32'Mixed'#32#9);
  FSL.Add(#13#10'NewLines'#13#10);
  FSL.Add('  Multiple   Spaces  ');

  FSL.Trim;

  Assert.AreEqual('Mixed', FSL[0]);
  Assert.AreEqual('NewLines', FSL[1]);
  Assert.AreEqual('Multiple   Spaces', FSL[2], 'Only leading/trailing spaces removed');
end;


procedure TTestStringList.TestShuffle_PreservesAllItems;
var
  i: Integer;
  OriginalItems: TStringList;
begin
  for i:= 1 to 20 do
    FSL.Add('Item' + IntToStr(i));

  OriginalItems:= TStringList.Create;
  try
    OriginalItems.Assign(FSL);
    OriginalItems.Sort;

    FSL.Shuffle;
    FSL.Sort;

    { After sorting both, they should be identical }
    Assert.AreEqual(OriginalItems.Text, FSL.Text, 'Shuffle should preserve all items');
  finally
    FreeAndNil(OriginalItems);
  end;
end;


procedure TTestStringList.TestSortReverse_Numbers;
begin
  FSL.Add('1');
  FSL.Add('10');
  FSL.Add('2');
  FSL.Add('20');

  FSL.SortReverse;

  { Lexicographic reverse order: 20, 2, 10, 1 }
  Assert.AreEqual('20', FSL[0]);
  Assert.AreEqual('2', FSL[1]);
  Assert.AreEqual('10', FSL[2]);
  Assert.AreEqual('1', FSL[3]);
end;


procedure TTestStringList.TestHighestString_Empty;
begin
  Assert.AreEqual('', FSL.HighestString, 'Empty list should return empty string');
end;


procedure TTestStringList.TestHighestString_SingleItem;
begin
  FSL.Add('OnlyOne');

  Assert.AreEqual('OnlyOne', FSL.HighestString, 'Single item should be returned');
end;


procedure TTestStringList.TestFindLine_FirstLine;
begin
  FSL.Add('First with target');
  FSL.Add('Second line');
  FSL.Add('Third line');

  Assert.AreEqual(0, FSL.FindLine('target'), 'Should find target in first line');
end;


procedure TTestStringList.TestFindLine_CaseSensitive;
begin
  FSL.Add('Line with TARGET');
  FSL.Add('Line with target');

  { FindLine uses Pos which is case-sensitive }
  Assert.AreEqual(1, FSL.FindLine('target'), 'Should find lowercase target in second line');
  Assert.AreEqual(0, FSL.FindLine('TARGET'), 'Should find uppercase TARGET in first line');
end;


procedure TTestStringList.TestRemoveLines_CaseInsensitive;
var
  Removed: Integer;
begin
  FSL.Add('Line with BAD word');
  FSL.Add('Line with bad word');
  FSL.Add('Clean line');

  { RemoveLines uses PosInsensitive }
  Removed:= FSL.RemoveLines('bad');

  Assert.AreEqual(2, Removed, 'Should remove both lines regardless of case');
  Assert.AreEqual(1, FSL.Count);
  Assert.AreEqual('Clean line', FSL[0]);
end;


procedure TTestStringList.TestRemoveLines_NoMatch;
var
  Removed: Integer;
begin
  FSL.Add('Line one');
  FSL.Add('Line two');

  Removed:= FSL.RemoveLines('notfound');

  Assert.AreEqual(0, Removed, 'Should remove nothing');
  Assert.AreEqual(2, FSL.Count);
end;


procedure TTestStringList.TestKeepLines_CaseInsensitive;
var
  Removed: Integer;
begin
  FSL.Add('KEEP this');
  FSL.Add('Remove this');
  FSL.Add('keep this too');

  { KeepLines uses PosInsensitive }
  Removed:= FSL.KeepLines('keep');

  Assert.AreEqual(1, Removed, 'Should remove one line');
  Assert.AreEqual(2, FSL.Count);
end;


procedure TTestStringList.TestKeepLines_NoMatch;
var
  Removed: Integer;
begin
  FSL.Add('Line one');
  FSL.Add('Line two');

  Removed:= FSL.KeepLines('notfound');

  Assert.AreEqual(2, Removed, 'Should remove all lines');
  Assert.AreEqual(0, FSL.Count);
end;


procedure TTestStringList.TestConcatenate_Empty;
begin
  Assert.AreEqual('', FSL.Concatenate(';'), 'Empty list should return empty string');
end;


procedure TTestStringList.TestConcatenate_SingleItem;
begin
  FSL.Add('Only');

  Assert.AreEqual('Only;', FSL.Concatenate(';'), 'Single item with separator');
end;


procedure TTestStringList.TestGetTopLines_MoreThanAvailable;
begin
  FSL.Add('First');
  FSL.Add('Second');

  Assert.AreEqual('First'#13#10'Second', FSL.GetTopLines(10), 'Should return all available lines');
end;


procedure TTestStringList.TestGetTopLines_WithEmptyLines;
begin
  FSL.Add('First');
  FSL.Add('');
  FSL.Add('Second');
  FSL.Add('');
  FSL.Add('Third');

  { By default IgnoreEmptyLines=TRUE, so empty lines are skipped }
  Assert.AreEqual('First'#13#10'Second', FSL.GetTopLines(2), 'Should skip empty lines');
end;


procedure TTestStringList.TestGetTopLines_IncludeEmpty;
begin
  FSL.Add('First');
  FSL.Add('');
  FSL.Add('Second');

  { With IgnoreEmptyLines=FALSE, empty lines are counted }
  Assert.AreEqual('First'#13#10'', FSL.GetTopLines(2, FALSE), 'Should include empty lines');
end;


procedure TTestStringList.TestRemoveTopLines_All;
begin
  FSL.Add('First');
  FSL.Add('Second');
  FSL.Add('Third');

  FSL.RemoveTopLines(3);

  Assert.AreEqual(0, FSL.Count, 'All lines should be removed');
end;


procedure TTestStringList.TestString2TSL_Empty;
var
  TSL: TStringList;
begin
  TSL:= String2TSL('');
  try
    { TStringList.Text='' results in 0 items }
    Assert.AreEqual(0, TSL.Count, 'Empty string should create empty list');
  finally
    FreeAndNil(TSL);
  end;
end;


procedure TTestStringList.TestFindLine_StandAlone_NotFound;
var
  Haystack, ResultStr: string;
begin
  Haystack:= 'First line'#13#10'Second line'#13#10'Third line';

  ResultStr:= FindLine('notfound', Haystack);

  Assert.AreEqual('', ResultStr, 'Should return empty string when not found');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestStringList);

end.
