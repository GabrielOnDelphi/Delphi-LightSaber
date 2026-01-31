unit Test.LightVcl.Visual.Memo;

{=============================================================================================================
   Unit tests for LightVcl.Visual.Memo.pas (TCubicMemo)

   Note: TCubicMemo is a VCL control that requires a parent window handle for many operations.
   Tests that require GUI context are marked with [Category('GUI')].
   These tests create a temporary form to host the memo control.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.StdCtrls,
  LightVcl.Visual.Memo;

type
  [TestFixture]
  TTestCubicMemo = class
  private
    FForm: TForm;
    FMemo: TCubicMemo;
    procedure SetupMemo;
    procedure TeardownMemo;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { Basic Tests }
    [Test]
    procedure TestCreate;

    [Test]
    procedure TestDefaultSearchOptions;

    [Test]
    procedure TestDefaultTypeMode;

    { AddString/AddInteger Tests }
    [Test]
    [Category('GUI')]
    procedure TestAddString;

    [Test]
    [Category('GUI')]
    procedure TestAddInteger;

    [Test]
    [Category('GUI')]
    procedure TestAddEntry;

    { Line Operations Tests }
    [Test]
    [Category('GUI')]
    procedure TestSortLines;

    [Test]
    [Category('GUI')]
    procedure TestSwapLines;

    [Test]
    [Category('GUI')]
    procedure TestSwapLines_SameIndex;

    [Test]
    [Category('GUI')]
    procedure TestSwapLines_OutOfBounds;

    [Test]
    [Category('GUI')]
    procedure TestRemoveEmptyLines;

    [Test]
    [Category('GUI')]
    procedure TestRemoveEmptyLinesEx;

    [Test]
    [Category('GUI')]
    procedure TestRemoveDuplicates;

    [Test]
    [Category('GUI')]
    procedure TestRemoveDuplicates_NoDuplicates;

    [Test]
    [Category('GUI')]
    procedure TestRemoveLines_PartialMatch;

    [Test]
    [Category('GUI')]
    procedure TestRemoveLines_ExactMatch;

    [Test]
    [Category('GUI')]
    procedure TestKeepLines;

    [Test]
    [Category('GUI')]
    procedure TestKeepFirstLines;

    [Test]
    [Category('GUI')]
    procedure TestKeepFirstLines_ZeroOrNegative;

    [Test]
    [Category('GUI')]
    procedure TestTrim;

    [Test]
    [Category('GUI')]
    procedure TestFindLine;

    [Test]
    [Category('GUI')]
    procedure TestFindLine_NotFound;

    { Count Tests }
    [Test]
    [Category('GUI')]
    procedure TestCountNonEmptyLines;

    [Test]
    [Category('GUI')]
    procedure TestCountWords;

    [Test]
    [Category('GUI')]
    procedure TestCountWords_Empty;

    { Selection Tests }
    [Test]
    [Category('GUI')]
    procedure TestSelectLine;

    [Test]
    [Category('GUI')]
    procedure TestSelectLine_OutOfBounds;

    [Test]
    [Category('GUI')]
    procedure TestSelectLine_ByText;

    [Test]
    [Category('GUI')]
    procedure TestRemoveSelection;

    { Search Tests }
    [Test]
    [Category('GUI')]
    procedure TestSearch_Found;

    [Test]
    [Category('GUI')]
    procedure TestSearch_NotFound;

    [Test]
    [Category('GUI')]
    procedure TestSearch_CaseInsensitive;

    [Test]
    [Category('GUI')]
    procedure TestSearch_Wrap;

    { Scroll Tests }
    [Test]
    [Category('GUI')]
    procedure TestScrollAtEnd;

    [Test]
    [Category('GUI')]
    procedure TestScrollAtTop;

    { CenterInView Tests }
    [Test]
    [Category('GUI')]
    procedure TestCenterInView_OutOfBounds;

    { CurrentLine Tests }
    [Test]
    [Category('GUI')]
    procedure TestCurrentLine;

    { Randomize Tests }
    [Test]
    [Category('GUI')]
    procedure TestRandomize;
  end;

implementation

{ TTestCubicMemo }

procedure TTestCubicMemo.Setup;
begin
  // Basic setup - GUI-dependent tests will call SetupMemo
end;

procedure TTestCubicMemo.TearDown;
begin
  TeardownMemo;
end;

procedure TTestCubicMemo.SetupMemo;
begin
  if FForm = nil then
  begin
    FForm:= TForm.Create(nil);
    FForm.Width:= 400;
    FForm.Height:= 300;

    FMemo:= TCubicMemo.Create(FForm);
    FMemo.Parent:= FForm;
    FMemo.Align:= alClient;

    FForm.Show;
    Application.ProcessMessages;
  end;
end;

procedure TTestCubicMemo.TeardownMemo;
begin
  if FForm <> nil then
  begin
    FreeAndNil(FMemo);
    FreeAndNil(FForm);
  end;
end;


{ Basic Tests }

procedure TTestCubicMemo.TestCreate;
var
  Memo: TCubicMemo;
begin
  Memo:= TCubicMemo.Create(nil);
  try
    Assert.IsNotNull(Memo);
  finally
    FreeAndNil(Memo);
  end;
end;

procedure TTestCubicMemo.TestDefaultSearchOptions;
var
  Memo: TCubicMemo;
begin
  Memo:= TCubicMemo.Create(nil);
  try
    Assert.IsTrue(soIgnoreCase in Memo.SearchOptions, 'soIgnoreCase should be default');
    Assert.IsTrue(soWrap in Memo.SearchOptions, 'soWrap should be default');
    Assert.IsFalse(soFromStart in Memo.SearchOptions, 'soFromStart should not be default');
  finally
    FreeAndNil(Memo);
  end;
end;

procedure TTestCubicMemo.TestDefaultTypeMode;
var
  Memo: TCubicMemo;
begin
  Memo:= TCubicMemo.Create(nil);
  try
    Assert.AreEqual(tmInsert, Memo.TypeMode, 'Default TypeMode should be tmInsert');
  finally
    FreeAndNil(Memo);
  end;
end;


{ AddString/AddInteger Tests }

procedure TTestCubicMemo.TestAddString;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.AddString('Hello');
  FMemo.AddString('World');
  Assert.AreEqual(2, FMemo.Lines.Count);
  Assert.AreEqual('Hello', FMemo.Lines[0]);
  Assert.AreEqual('World', FMemo.Lines[1]);
end;

procedure TTestCubicMemo.TestAddInteger;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.AddInteger(42);
  FMemo.AddInteger(-10);
  Assert.AreEqual(2, FMemo.Lines.Count);
  Assert.AreEqual('42', FMemo.Lines[0]);
  Assert.AreEqual('-10', FMemo.Lines[1]);
end;

procedure TTestCubicMemo.TestAddEntry;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.AddEntry('Count: ', 5);
  Assert.AreEqual(1, FMemo.Lines.Count);
  Assert.AreEqual('Count: 5', FMemo.Lines[0]);
end;


{ Line Operations Tests }

procedure TTestCubicMemo.TestSortLines;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('Zebra');
  FMemo.Lines.Add('Apple');
  FMemo.Lines.Add('Mango');
  FMemo.SortLines;
  Assert.AreEqual('Apple', FMemo.Lines[0]);
  Assert.AreEqual('Mango', FMemo.Lines[1]);
  Assert.AreEqual('Zebra', FMemo.Lines[2]);
end;

procedure TTestCubicMemo.TestSwapLines;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('First');
  FMemo.Lines.Add('Second');
  FMemo.Lines.Add('Third');
  FMemo.SwapLines(0, 2);
  Assert.AreEqual('Third', FMemo.Lines[0]);
  Assert.AreEqual('Second', FMemo.Lines[1]);
  Assert.AreEqual('First', FMemo.Lines[2]);
end;

procedure TTestCubicMemo.TestSwapLines_SameIndex;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('First');
  FMemo.Lines.Add('Second');
  FMemo.SwapLines(0, 0);  // Should do nothing
  Assert.AreEqual('First', FMemo.Lines[0]);
  Assert.AreEqual('Second', FMemo.Lines[1]);
end;

procedure TTestCubicMemo.TestSwapLines_OutOfBounds;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('First');
  FMemo.Lines.Add('Second');
  FMemo.SwapLines(-1, 0);  // Should do nothing - negative index
  FMemo.SwapLines(0, 100); // Should do nothing - index too large
  Assert.AreEqual('First', FMemo.Lines[0]);
  Assert.AreEqual('Second', FMemo.Lines[1]);
end;

procedure TTestCubicMemo.TestRemoveEmptyLines;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('First');
  FMemo.Lines.Add('');
  FMemo.Lines.Add('Second');
  FMemo.Lines.Add('');
  FMemo.Lines.Add('');
  FMemo.Lines.Add('Third');
  FMemo.RemoveEmptyLines;
  Assert.AreEqual(3, FMemo.Lines.Count);
  Assert.AreEqual('First', FMemo.Lines[0]);
  Assert.AreEqual('Second', FMemo.Lines[1]);
  Assert.AreEqual('Third', FMemo.Lines[2]);
end;

procedure TTestCubicMemo.TestRemoveEmptyLinesEx;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('First');
  FMemo.Lines.Add('   ');          // Whitespace only - should be removed after trim
  FMemo.Lines.Add('Second');
  FMemo.Lines.Add('');
  FMemo.RemoveEmptyLinesEx;
  Assert.AreEqual(2, FMemo.Lines.Count);
  Assert.AreEqual('First', FMemo.Lines[0]);
  Assert.AreEqual('Second', FMemo.Lines[1]);
end;

procedure TTestCubicMemo.TestRemoveDuplicates;
var
  Removed: Integer;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('Apple');
  FMemo.Lines.Add('Banana');
  FMemo.Lines.Add('apple');         // Duplicate (case-insensitive)
  FMemo.Lines.Add('Cherry');
  FMemo.Lines.Add('BANANA');        // Duplicate (case-insensitive)
  Removed:= FMemo.RemoveDuplicates;
  Assert.AreEqual(2, Removed);
  Assert.AreEqual(3, FMemo.Lines.Count);
end;

procedure TTestCubicMemo.TestRemoveDuplicates_NoDuplicates;
var
  Removed: Integer;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('Apple');
  FMemo.Lines.Add('Banana');
  FMemo.Lines.Add('Cherry');
  Removed:= FMemo.RemoveDuplicates;
  Assert.AreEqual(0, Removed);
  Assert.AreEqual(3, FMemo.Lines.Count);
end;

procedure TTestCubicMemo.TestRemoveLines_PartialMatch;
var
  Removed: Integer;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('Hello World');
  FMemo.Lines.Add('Goodbye World');
  FMemo.Lines.Add('Hello There');
  FMemo.Lines.Add('Something Else');
  Removed:= FMemo.RemoveLines('Hello', True);
  Assert.AreEqual(2, Removed);
  Assert.AreEqual(2, FMemo.Lines.Count);
  Assert.AreEqual('Goodbye World', FMemo.Lines[0]);
  Assert.AreEqual('Something Else', FMemo.Lines[1]);
end;

procedure TTestCubicMemo.TestRemoveLines_ExactMatch;
var
  Removed: Integer;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('Hello');
  FMemo.Lines.Add('Hello World');
  FMemo.Lines.Add('hello');           // Case-insensitive match
  FMemo.Lines.Add('Something Else');
  Removed:= FMemo.RemoveLines('Hello', False);
  Assert.AreEqual(2, Removed);
  Assert.AreEqual(2, FMemo.Lines.Count);
end;

procedure TTestCubicMemo.TestKeepLines;
var
  Removed: Integer;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('Apple Pie');
  FMemo.Lines.Add('Banana Split');
  FMemo.Lines.Add('Apple Crisp');
  FMemo.Lines.Add('Cherry Tart');
  Removed:= FMemo.KeepLines('Apple');
  Assert.AreEqual(2, Removed);
  Assert.AreEqual(2, FMemo.Lines.Count);
  Assert.AreEqual('Apple Pie', FMemo.Lines[0]);
  Assert.AreEqual('Apple Crisp', FMemo.Lines[1]);
end;

procedure TTestCubicMemo.TestKeepFirstLines;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('Line 1');
  FMemo.Lines.Add('Line 2');
  FMemo.Lines.Add('Line 3');
  FMemo.Lines.Add('Line 4');
  FMemo.Lines.Add('Line 5');
  FMemo.KeepFirstLines(3);
  Assert.AreEqual(3, FMemo.Lines.Count);
  Assert.AreEqual('Line 1', FMemo.Lines[0]);
  Assert.AreEqual('Line 2', FMemo.Lines[1]);
  Assert.AreEqual('Line 3', FMemo.Lines[2]);
end;

procedure TTestCubicMemo.TestKeepFirstLines_ZeroOrNegative;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('Line 1');
  FMemo.Lines.Add('Line 2');
  FMemo.KeepFirstLines(0);   // Should do nothing
  Assert.AreEqual(2, FMemo.Lines.Count);
  FMemo.KeepFirstLines(-5);  // Should do nothing
  Assert.AreEqual(2, FMemo.Lines.Count);
end;

procedure TTestCubicMemo.TestTrim;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('  Leading spaces');
  FMemo.Lines.Add('Trailing spaces  ');
  FMemo.Lines.Add('  Both sides  ');
  FMemo.Trim;
  Assert.AreEqual('Leading spaces', FMemo.Lines[0]);
  Assert.AreEqual('Trailing spaces', FMemo.Lines[1]);
  Assert.AreEqual('Both sides', FMemo.Lines[2]);
end;

procedure TTestCubicMemo.TestFindLine;
var
  Index: Integer;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('First');
  FMemo.Lines.Add('Second');
  FMemo.Lines.Add('Third');
  Index:= FMemo.FindLine('Second');
  Assert.AreEqual(1, Index);
  // Case-insensitive
  Index:= FMemo.FindLine('THIRD');
  Assert.AreEqual(2, Index);
end;

procedure TTestCubicMemo.TestFindLine_NotFound;
var
  Index: Integer;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('First');
  FMemo.Lines.Add('Second');
  Index:= FMemo.FindLine('NotHere');
  Assert.AreEqual(-1, Index);
end;


{ Count Tests }

procedure TTestCubicMemo.TestCountNonEmptyLines;
var
  Count: Integer;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('First');
  FMemo.Lines.Add('');
  FMemo.Lines.Add('Second');
  FMemo.Lines.Add('');
  FMemo.Lines.Add('Third');
  Count:= FMemo.CountNonEmptyLines;
  Assert.AreEqual(3, Count);
end;

procedure TTestCubicMemo.TestCountWords;
var
  Count: Integer;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('Hello World');
  FMemo.Lines.Add('This is a test');
  Count:= FMemo.CountWords;
  Assert.AreEqual(6, Count);
end;

procedure TTestCubicMemo.TestCountWords_Empty;
var
  Count: Integer;
begin
  SetupMemo;
  FMemo.Clear;
  Count:= FMemo.CountWords;
  Assert.AreEqual(0, Count);
end;


{ Selection Tests }

procedure TTestCubicMemo.TestSelectLine;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('First');
  FMemo.Lines.Add('Second');
  FMemo.Lines.Add('Third');
  FMemo.SelectLine(1);
  Assert.AreEqual('Second', FMemo.SelText);
end;

procedure TTestCubicMemo.TestSelectLine_OutOfBounds;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('First');
  FMemo.Lines.Add('Second');
  FMemo.SelectLine(-1);  // Should do nothing
  Assert.AreEqual(0, FMemo.SelLength);
  FMemo.SelectLine(100); // Should do nothing
  Assert.AreEqual(0, FMemo.SelLength);
end;

procedure TTestCubicMemo.TestSelectLine_ByText;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('Apple');
  FMemo.Lines.Add('Banana');
  FMemo.Lines.Add('Cherry');
  FMemo.SelectLine('Banana');
  Assert.AreEqual('Banana', FMemo.SelText);
end;

procedure TTestCubicMemo.TestRemoveSelection;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('Hello World');
  FMemo.SelStart:= 0;
  FMemo.SelLength:= 5;
  FMemo.RemoveSelection;
  Assert.AreEqual(0, FMemo.SelStart);
  Assert.AreEqual(0, FMemo.SelLength);
end;


{ Search Tests }

procedure TTestCubicMemo.TestSearch_Found;
var
  Found: Boolean;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('Hello World');
  FMemo.Lines.Add('Goodbye World');
  FMemo.SearchOptions:= [soFromStart];
  Found:= FMemo.Search('World');
  Assert.IsTrue(Found);
  Assert.AreEqual('World', FMemo.SelText);
end;

procedure TTestCubicMemo.TestSearch_NotFound;
var
  Found: Boolean;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('Hello World');
  FMemo.SearchOptions:= [soFromStart];
  Found:= FMemo.Search('NotHere');
  Assert.IsFalse(Found);
end;

procedure TTestCubicMemo.TestSearch_CaseInsensitive;
var
  Found: Boolean;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('Hello World');
  FMemo.SearchOptions:= [soIgnoreCase, soFromStart];
  Found:= FMemo.Search('WORLD');
  Assert.IsTrue(Found);
  Assert.AreEqual('World', FMemo.SelText);
end;

procedure TTestCubicMemo.TestSearch_Wrap;
var
  Found: Boolean;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('First Hello');
  FMemo.Lines.Add('Second Hello');
  FMemo.SearchOptions:= [soWrap];

  // Move cursor past first occurrence
  FMemo.SelStart:= 20;
  FMemo.SelLength:= 0;

  // Search should wrap to beginning
  Found:= FMemo.Search('First');
  Assert.IsTrue(Found);
end;


{ Scroll Tests }

procedure TTestCubicMemo.TestScrollAtEnd;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('Line 1');
  FMemo.Lines.Add('Line 2');
  FMemo.Lines.Add('Line 3');
  FMemo.ScrollAtEnd;
  Assert.AreEqual(Length(FMemo.Text), FMemo.SelStart);
end;

procedure TTestCubicMemo.TestScrollAtTop;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('Line 1');
  FMemo.Lines.Add('Line 2');
  FMemo.Lines.Add('Line 3');
  FMemo.SelStart:= 10;
  FMemo.ScrollAtTop;
  Assert.AreEqual(0, FMemo.SelStart);
end;


{ CenterInView Tests }

procedure TTestCubicMemo.TestCenterInView_OutOfBounds;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('Line 1');
  FMemo.Lines.Add('Line 2');
  // Should not raise exception for out-of-bounds
  FMemo.CenterInView(-1);
  FMemo.CenterInView(100);
  Assert.Pass('CenterInView did not crash with out-of-bounds values');
end;


{ CurrentLine Tests }

procedure TTestCubicMemo.TestCurrentLine;
begin
  SetupMemo;
  FMemo.Clear;
  FMemo.Lines.Add('Line 1');
  FMemo.Lines.Add('Line 2');
  FMemo.Lines.Add('Line 3');
  FMemo.SelStart:= 0;
  Assert.AreEqual(0, FMemo.CurrentLine);
end;


{ Randomize Tests }

procedure TTestCubicMemo.TestRandomize;
var
  OriginalLines: TStringList;
  i: Integer;
  Changed: Boolean;
begin
  SetupMemo;
  FMemo.Clear;
  for i:= 1 to 20 do
    FMemo.Lines.Add('Line ' + IntToStr(i));

  OriginalLines:= TStringList.Create;
  try
    OriginalLines.Assign(FMemo.Lines);

    // Randomize multiple times to ensure at least one change
    System.Randomize;
    FMemo.Randomize;

    // Check that at least one line has moved (very unlikely all stay in place)
    Changed:= False;
    for i:= 0 to FMemo.Lines.Count - 1 do
      if FMemo.Lines[i] <> OriginalLines[i] then
      begin
        Changed:= True;
        Break;
      end;

    // With 20 lines, probability of no change is extremely low
    Assert.IsTrue(Changed or (FMemo.Lines.Count <= 1), 'Randomize should shuffle lines');
    Assert.AreEqual(20, FMemo.Lines.Count, 'Line count should remain the same');
  finally
    FreeAndNil(OriginalLines);
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestCubicMemo);

end.
