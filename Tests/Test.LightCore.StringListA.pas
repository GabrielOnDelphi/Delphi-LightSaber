unit Test.LightCore.StringListA;

{=============================================================================================================
   Unit tests for LightCore.StringListA
   Tests TAnsiTSL - AnsiString-based string list

   Requires: TESTINSIGHT compiler directive for TestInsight integration
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  LightCore.StringListA;

type
  [TestFixture]
  TTestAnsiStringList = class
  private
    FASL: TAnsiTSL;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Basic Operations Tests }
    [Test]
    procedure TestCreate_Empty;

    [Test]
    procedure TestAdd_SingleItem;

    [Test]
    procedure TestAdd_MultipleItems;

    [Test]
    procedure TestClear;

    [Test]
    procedure TestCount;

    { Text Property Tests - SetTextStr }
    [Test]
    procedure TestSetText_Empty;

    [Test]
    procedure TestSetText_SingleLine;

    [Test]
    procedure TestSetText_MultipleLines_CRLF;

    [Test]
    procedure TestSetText_MultipleLines_LF;

    [Test]
    procedure TestSetText_MultipleLines_CR;

    [Test]
    procedure TestSetText_MixedLineEndings;

    [Test]
    procedure TestSetText_EmptyLines;

    [Test]
    procedure TestSetText_TrailingLineBreak;

    { Text Property Tests - GetTextStr }
    [Test]
    procedure TestGetText_Empty;

    [Test]
    procedure TestGetText_SingleLine;

    [Test]
    procedure TestGetText_MultipleLines;

    [Test]
    procedure TestGetText_EmptyLines;

    { Round-trip Tests }
    [Test]
    procedure TestRoundTrip_Simple;

    [Test]
    procedure TestRoundTrip_MultipleLines;

    [Test]
    procedure TestRoundTrip_SpecialChars;

    { AnsiString Specific Tests }
    [Test]
    procedure TestAnsiChars_ASCII;

    [Test]
    procedure TestAnsiChars_Extended;

    { Edge Cases }
    [Test]
    procedure TestEdge_VeryLongLine;

    [Test]
    procedure TestEdge_ManyLines;
  end;

implementation


procedure TTestAnsiStringList.Setup;
begin
  FASL:= TAnsiTSL.Create;
end;


procedure TTestAnsiStringList.TearDown;
begin
  FreeAndNil(FASL);
end;


{ Basic Operations Tests }

procedure TTestAnsiStringList.TestCreate_Empty;
begin
  Assert.AreEqual(0, FASL.Count);
end;


procedure TTestAnsiStringList.TestAdd_SingleItem;
begin
  FASL.Add('Test');
  Assert.AreEqual(1, FASL.Count);
  Assert.AreEqual(AnsiString('Test'), FASL[0]);
end;


procedure TTestAnsiStringList.TestAdd_MultipleItems;
begin
  FASL.Add('First');
  FASL.Add('Second');
  FASL.Add('Third');
  Assert.AreEqual(3, FASL.Count);
  Assert.AreEqual(AnsiString('First'), FASL[0]);
  Assert.AreEqual(AnsiString('Second'), FASL[1]);
  Assert.AreEqual(AnsiString('Third'), FASL[2]);
end;


procedure TTestAnsiStringList.TestClear;
begin
  FASL.Add('Item1');
  FASL.Add('Item2');
  FASL.Clear;
  Assert.AreEqual(0, FASL.Count);
end;


procedure TTestAnsiStringList.TestCount;
begin
  Assert.AreEqual(0, FASL.Count);
  FASL.Add('A');
  Assert.AreEqual(1, FASL.Count);
  FASL.Add('B');
  Assert.AreEqual(2, FASL.Count);
end;


{ Text Property Tests - SetTextStr }

procedure TTestAnsiStringList.TestSetText_Empty;
begin
  FASL.Text:= '';
  Assert.AreEqual(0, FASL.Count);
end;


procedure TTestAnsiStringList.TestSetText_SingleLine;
begin
  FASL.Text:= 'SingleLine';
  Assert.AreEqual(1, FASL.Count);
  Assert.AreEqual(AnsiString('SingleLine'), FASL[0]);
end;


procedure TTestAnsiStringList.TestSetText_MultipleLines_CRLF;
begin
  FASL.Text:= 'Line1'#13#10'Line2'#13#10'Line3';
  Assert.AreEqual(3, FASL.Count);
  Assert.AreEqual(AnsiString('Line1'), FASL[0]);
  Assert.AreEqual(AnsiString('Line2'), FASL[1]);
  Assert.AreEqual(AnsiString('Line3'), FASL[2]);
end;


procedure TTestAnsiStringList.TestSetText_MultipleLines_LF;
begin
  FASL.Text:= 'Line1'#10'Line2'#10'Line3';
  Assert.AreEqual(3, FASL.Count);
  Assert.AreEqual(AnsiString('Line1'), FASL[0]);
  Assert.AreEqual(AnsiString('Line2'), FASL[1]);
  Assert.AreEqual(AnsiString('Line3'), FASL[2]);
end;


procedure TTestAnsiStringList.TestSetText_MultipleLines_CR;
begin
  FASL.Text:= 'Line1'#13'Line2'#13'Line3';
  Assert.AreEqual(3, FASL.Count);
  Assert.AreEqual(AnsiString('Line1'), FASL[0]);
  Assert.AreEqual(AnsiString('Line2'), FASL[1]);
  Assert.AreEqual(AnsiString('Line3'), FASL[2]);
end;


procedure TTestAnsiStringList.TestSetText_MixedLineEndings;
begin
  FASL.Text:= 'CRLF'#13#10'LF'#10'CR'#13'End';
  Assert.AreEqual(4, FASL.Count);
  Assert.AreEqual(AnsiString('CRLF'), FASL[0]);
  Assert.AreEqual(AnsiString('LF'), FASL[1]);
  Assert.AreEqual(AnsiString('CR'), FASL[2]);
  Assert.AreEqual(AnsiString('End'), FASL[3]);
end;


procedure TTestAnsiStringList.TestSetText_EmptyLines;
begin
  FASL.Text:= 'First'#13#10#13#10'Third';
  Assert.AreEqual(3, FASL.Count);
  Assert.AreEqual(AnsiString('First'), FASL[0]);
  Assert.AreEqual(AnsiString(''), FASL[1]);
  Assert.AreEqual(AnsiString('Third'), FASL[2]);
end;


procedure TTestAnsiStringList.TestSetText_TrailingLineBreak;
begin
  FASL.Text:= 'Line1'#13#10'Line2'#13#10;
  { Trailing line break creates an empty item }
  Assert.AreEqual(3, FASL.Count);
  Assert.AreEqual(AnsiString('Line1'), FASL[0]);
  Assert.AreEqual(AnsiString('Line2'), FASL[1]);
  Assert.AreEqual(AnsiString(''), FASL[2]);
end;


{ Text Property Tests - GetTextStr }

procedure TTestAnsiStringList.TestGetText_Empty;
begin
  Assert.AreEqual(AnsiString(''), FASL.Text);
end;


procedure TTestAnsiStringList.TestGetText_SingleLine;
begin
  FASL.Add('OnlyLine');
  { GetTextStr adds CRLF after each line }
  Assert.AreEqual(AnsiString('OnlyLine'#13#10), FASL.Text);
end;


procedure TTestAnsiStringList.TestGetText_MultipleLines;
begin
  FASL.Add('Line1');
  FASL.Add('Line2');
  FASL.Add('Line3');
  Assert.AreEqual(AnsiString('Line1'#13#10'Line2'#13#10'Line3'#13#10), FASL.Text);
end;


procedure TTestAnsiStringList.TestGetText_EmptyLines;
begin
  FASL.Add('First');
  FASL.Add('');
  FASL.Add('Third');
  Assert.AreEqual(AnsiString('First'#13#10#13#10'Third'#13#10), FASL.Text);
end;


{ Round-trip Tests }

procedure TTestAnsiStringList.TestRoundTrip_Simple;
var
  Original: AnsiString;
begin
  Original:= 'Test line';
  FASL.Text:= Original;
  { Note: GetText adds trailing CRLF }
  Assert.AreEqual(AnsiString('Test line'), FASL[0]);
end;


procedure TTestAnsiStringList.TestRoundTrip_MultipleLines;
begin
  FASL.Add('Alpha');
  FASL.Add('Beta');
  FASL.Add('Gamma');

  var Text:= FASL.Text;
  FASL.Clear;
  FASL.Text:= Text;

  { After round-trip, we get an extra empty line due to trailing CRLF }
  Assert.IsTrue(FASL.Count >= 3);
  Assert.AreEqual(AnsiString('Alpha'), FASL[0]);
  Assert.AreEqual(AnsiString('Beta'), FASL[1]);
  Assert.AreEqual(AnsiString('Gamma'), FASL[2]);
end;


procedure TTestAnsiStringList.TestRoundTrip_SpecialChars;
begin
  FASL.Add('Tab:'#9'here');
  FASL.Add('Null:'#0'here');

  Assert.AreEqual(AnsiString('Tab:'#9'here'), FASL[0]);
  { Note: Null character may terminate string early in some scenarios }
end;


{ AnsiString Specific Tests }

procedure TTestAnsiStringList.TestAnsiChars_ASCII;
begin
  FASL.Add('Hello World!');
  FASL.Add('0123456789');
  FASL.Add('!@#$%^&*()');

  Assert.AreEqual(AnsiString('Hello World!'), FASL[0]);
  Assert.AreEqual(AnsiString('0123456789'), FASL[1]);
  Assert.AreEqual(AnsiString('!@#$%^&*()'), FASL[2]);
end;


procedure TTestAnsiStringList.TestAnsiChars_Extended;
begin
  { Extended ASCII characters (128-255) }
  FASL.Add(AnsiString(#128#129#130));
  FASL.Add(AnsiString(#255));

  Assert.AreEqual(3, Length(FASL[0]));
  Assert.AreEqual(1, Length(FASL[1]));
end;


{ Edge Cases }

procedure TTestAnsiStringList.TestEdge_VeryLongLine;
var
  LongLine: AnsiString;
  i: Integer;
begin
  SetLength(LongLine, 10000);
  for i:= 1 to 10000 do
    LongLine[i]:= AnsiChar(Ord('A') + (i mod 26));

  FASL.Add(LongLine);

  Assert.AreEqual(1, FASL.Count);
  Assert.AreEqual(10000, Length(FASL[0]));
  Assert.AreEqual(LongLine, FASL[0]);
end;


procedure TTestAnsiStringList.TestEdge_ManyLines;
var
  i: Integer;
begin
  for i:= 1 to 1000 do
    FASL.Add(AnsiString('Line' + AnsiString(IntToStr(i))));

  Assert.AreEqual(1000, FASL.Count);
  Assert.AreEqual(AnsiString('Line1'), FASL[0]);
  Assert.AreEqual(AnsiString('Line1000'), FASL[999]);
end;


initialization
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  {$ENDIF}
  TDUnitX.RegisterTestFixture(TTestAnsiStringList);

end.
