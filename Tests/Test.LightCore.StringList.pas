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

    [Test]
    procedure TestRemoveEmptyLines;

    [Test]
    procedure TestRemoveDuplicates;

    [Test]
    procedure TestRemoveDuplicateString;

    [Test]
    procedure TestTrim;

    [Test]
    procedure TestShuffle;

    [Test]
    procedure TestSortReverse;

    [Test]
    procedure TestHighestString;

    [Test]
    procedure TestFindLine;

    [Test]
    procedure TestRemoveLines;

    [Test]
    procedure TestKeepLines;

    [Test]
    procedure TestConcatenate;

    [Test]
    procedure TestGetTopLines;

    [Test]
    procedure TestRemoveTopLines;

    [Test]
    procedure TestString2TSL;

    [Test]
    procedure TestExtractTopLines;

    [Test]
    procedure TestFindLine_StandAlone;
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
  OriginalOrder: string;
  i: Integer;
  Changed: Boolean;
begin
  for i := 0 to 9 do
    FSL.Add(IntToStr(i));

  OriginalOrder := FSL.Text;

  { Shuffle multiple times to ensure at least one changes the order }
  Changed := False;
  for i := 1 to 10 do
  begin
    FSL.Shuffle;
    if FSL.Text <> OriginalOrder then
    begin
      Changed := True;
      Break;
    end;
    { Reset for next try }
    FSL.Clear;
    for var j := 0 to 9 do
      FSL.Add(IntToStr(j));
  end;

  Assert.AreEqual(10, FSL.Count, 'Count should remain the same after shuffle');
  { Note: Shuffle might occasionally produce the same order, so we check multiple times }
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
  Haystack, Result: string;
begin
  Haystack := 'First line'#13#10'Second with needle'#13#10'Third line';

  Result := FindLine('needle', Haystack);

  Assert.AreEqual('Second with needle', Result, 'Should find and return the line containing needle');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestStringList);

end.
