unit Test.LightCore.WrapString;

{=============================================================================================================
   Unit tests for LightCore.WrapString
   Tests string wrapping and truncation utilities

   Requires: TESTINSIGHT compiler directive for TestInsight integration
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils;

type
  [TestFixture]
  TTestWrapString = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { WrapStringForced Tests }
    [Test]
    procedure TestWrapStringForced_Basic;

    [Test]
    procedure TestWrapStringForced_ShortString;

    [Test]
    procedure TestWrapStringForced_ExactLength;

    [Test]
    procedure TestWrapStringForced_Empty;

    [Test]
    procedure TestWrapStringForced_SingleChar;

    [Test]
    procedure TestWrapStringForced_MultipleWraps;

    { WrapStringForcedA Tests }
    [Test]
    procedure TestWrapStringForcedA_Basic;

    { TruncateToWord Tests }
    [Test]
    procedure TestTruncateToWord_Basic;

    [Test]
    procedure TestTruncateToWord_ExactBoundary;

    [Test]
    procedure TestTruncateToWord_NoSpaces;

    [Test]
    procedure TestTruncateToWord_Empty;

    [Test]
    procedure TestTruncateToWord_ShortString;

    { UnWrapString Tests }
    [Test]
    procedure TestUnWrapString_Basic;

    [Test]
    procedure TestUnWrapString_WithTabs;

    [Test]
    procedure TestUnWrapString_Empty;

    { UnwrapText Tests }
    [Test]
    procedure TestUnwrapText_Basic;

    [Test]
    procedure TestUnwrapText_WithSeparators;

    [Test]
    procedure TestUnwrapText_RemoveExtraEnters;

    [Test]
    procedure TestUnwrapText_AddExtraSpace;

    [Test]
    procedure TestUnwrapText_ShortString;
  end;

implementation

uses
  LightCore,
  LightCore.WrapString;


procedure TTestWrapString.Setup;
begin
  { No setup needed }
end;


procedure TTestWrapString.TearDown;
begin
  { No teardown needed }
end;


{ WrapStringForced Tests }

procedure TTestWrapString.TestWrapStringForced_Basic;
var
  Input, Output: string;
begin
  Input:= 'ABCDEFGHIJ';
  Output:= WrapStringForced(Input, 5);
  { Should wrap at position 5 }
  Assert.IsTrue(Pos(CRLF, Output) > 0, 'Should contain line break');
  Assert.AreEqual(5, Pos(CRLF, Output) - 1, 'First line should be 5 chars');
end;


procedure TTestWrapString.TestWrapStringForced_ShortString;
var
  Input, Output: string;
begin
  Input:= 'ABC';
  Output:= WrapStringForced(Input, 10);
  Assert.AreEqual('ABC', Output, 'Short string should not be wrapped');
end;


procedure TTestWrapString.TestWrapStringForced_ExactLength;
var
  Input, Output: string;
begin
  Input:= 'ABCDE';
  Output:= WrapStringForced(Input, 5);
  { Exactly 5 chars - should have line break at end }
  Assert.AreEqual('ABCDE' + CRLF, Output);
end;


procedure TTestWrapString.TestWrapStringForced_Empty;
var
  Output: string;
begin
  Output:= WrapStringForced('', 10);
  Assert.AreEqual('', Output);
end;


procedure TTestWrapString.TestWrapStringForced_SingleChar;
var
  Output: string;
begin
  Output:= WrapStringForced('X', 10);
  Assert.AreEqual('X', Output);
end;


procedure TTestWrapString.TestWrapStringForced_MultipleWraps;
var
  Input, Output: string;
  Lines: Integer;
begin
  Input:= 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';  { 26 chars }
  Output:= WrapStringForced(Input, 10);
  { 26 chars / 10 = 2 full lines + 6 remainder }
  Lines:= 1;
  for var i:= 1 to Length(Output) do
    if Output[i] = #10
    then Inc(Lines);
  Assert.AreEqual(3, Lines, 'Should have 3 lines (2 breaks + final segment)');
end;


{ WrapStringForcedA Tests }

procedure TTestWrapString.TestWrapStringForcedA_Basic;
var
  Input, Output: AnsiString;
begin
  Input:= 'ABCDEFGHIJ';
  Output:= WrapStringForcedA(Input, 5);
  { A CRLF after every 5th character, the last one included (as TestWrapStringForced_ExactLength shows) }
  Assert.AreEqual(AnsiString('ABCDE'#13#10'FGHIJ'#13#10), Output, 'The break must come after every 5 characters');
end;


{ TruncateToWord Tests }

procedure TTestWrapString.TestTruncateToWord_Basic;
var
  Input, Output: string;
begin
  Input:= 'Hello World Test';
  Output:= TruncateToWord(Input, 13);
  { The cut falls inside 'Test', so the result ends after the last whole word. The RTL WrapText keeps the space at the end
    of the line it breaks (c:\Delphi\Delphi 13\source\rtl\sys\System.SysUtils.pas:26939). A hard cut would give 'Hello World T'. }
  Assert.AreEqual('Hello World ', Output, 'Truncation must stop at a word boundary');
end;


procedure TTestWrapString.TestTruncateToWord_ExactBoundary;
var
  Input, Output: string;
begin
  Input:= 'Hello World';
  Output:= TruncateToWord(Input, 100);
  Assert.AreEqual('Hello World', Output, 'Should return full string when under limit');

  { MaxChars = Length: the whole text fits }
  Assert.AreEqual('Hello World', TruncateToWord(Input, 11), 'A text exactly MaxChars long must stay whole');

  { MaxChars = Length - 1: the last word no longer fits and is dropped whole (a hard cut would give 'Hello Worl') }
  Assert.AreEqual('Hello ', TruncateToWord(Input, 10), 'One character too long must drop the last word');
end;


procedure TTestWrapString.TestTruncateToWord_NoSpaces;
var
  Input, Output: string;
begin
  Input:= 'ABCDEFGHIJKLMNOP';
  Output:= TruncateToWord(Input, 10);
  Assert.AreEqual('ABCDEFGHIJ', Output, 'Should hard truncate the first 10 characters when no word boundary');
end;


procedure TTestWrapString.TestTruncateToWord_Empty;
var
  Output: string;
begin
  Output:= TruncateToWord('', 10);
  Assert.AreEqual('', Output);
end;


procedure TTestWrapString.TestTruncateToWord_ShortString;
var
  Output: string;
begin
  Output:= TruncateToWord('Hi', 100);
  Assert.AreEqual('Hi', Output);
end;


{ UnWrapString Tests }

procedure TTestWrapString.TestUnWrapString_Basic;
var
  Input, Output: string;
begin
  Input:= 'Line1' + CRLF + 'Line2' + CRLF + 'Line3';
  Output:= UnWrapString(Input);
  Assert.AreEqual('Line1Line2Line3', Output);
end;


procedure TTestWrapString.TestUnWrapString_WithTabs;
var
  Input, Output: string;
begin
  Input:= 'A' + Tab + 'B' + Tab + 'C';
  Output:= UnWrapString(Input);
  Assert.AreEqual('A B C', Output);
end;


procedure TTestWrapString.TestUnWrapString_Empty;
var
  Output: string;
begin
  Output:= UnWrapString('');
  Assert.AreEqual('', Output);
end;


{ UnwrapText Tests }

procedure TTestWrapString.TestUnwrapText_Basic;
var
  Input, Output: string;
begin
  Input:= 'Line1' + CRLF + 'Line2';
  Output:= UnwrapText(Input, '.', False, False);
  { Without separators at line end, lines should be joined, every character kept (the last one included) }
  Assert.AreEqual('Line1Line2', Output, 'Lines should be joined');
end;


procedure TTestWrapString.TestUnwrapText_WithSeparators;
var
  Input, Output: string;
begin
  Input:= 'Sentence.' + CRLF + 'Next';
  Output:= UnwrapText(Input, '.', False, False);
  { Period before line break should preserve the break }
  Assert.IsTrue(Pos(CRLF, Output) > 0, 'Line break after period should be kept');
end;


procedure TTestWrapString.TestUnwrapText_RemoveExtraEnters;
var
  Input, Output: string;
begin
  Input:= 'Line1' + CRLF + CRLF + CRLF + CRLF + 'Line2';
  { '#enter' makes a line break a separator, so a break that follows another break is kept.
    Without RemoveExtraEnters the result would be 'Line1' + 3 x CRLF + 'Line2'. The 4 breaks
    collapse to 2, the first is joined (it follows '1'), the second is kept. }
  Output:= UnwrapText(Input, '#enter', True, False);
  Assert.AreEqual('Line1' + CRLF + 'Line2', Output, 'RemoveExtraEnters must collapse the empty lines');
end;


procedure TTestWrapString.TestUnwrapText_AddExtraSpace;
var
  Input, Output: string;
begin
  Input:= 'Word1' + CRLF + 'Word2';
  Output:= UnwrapText(Input, '', False, True);
  Assert.IsTrue(Pos(' ', Output) > 0, 'Should add space when joining lines');
end;


procedure TTestWrapString.TestUnwrapText_ShortString;
var
  Output: string;
begin
  Output:= UnwrapText('AB', '.', False, False);
  Assert.AreEqual('AB', Output);

  Output:= UnwrapText('A', '.', False, False);
  Assert.AreEqual('A', Output);

  Output:= UnwrapText('', '.', False, False);
  Assert.AreEqual('', Output);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestWrapString);

end.
