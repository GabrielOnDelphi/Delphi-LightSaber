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
  System.Classes,
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


{ The byte values of S as decimal numbers separated by spaces, for example '65 13 10'.
  Compares bytes without any code-page conversion. }
function ByteCodes(CONST S: AnsiString): string;
begin
  Result:= '';
  for var i:= 1 to Length(S) do
    begin
      if Result <> ''
      then Result:= Result + ' ';
      Result:= Result + IntToStr(Ord(S[i]));
    end;
end;


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
  Assert.AreEqual(AnsiString(''), FASL.Text, 'An empty list must give an empty Text');

  { Text written back into the new list must leave it empty }
  FASL.Text:= FASL.Text;
  Assert.AreEqual(0, FASL.Count);
end;


procedure TTestAnsiStringList.TestAdd_SingleItem;
begin
  FASL.Add('Test');
  Assert.AreEqual(1, FASL.Count);
  Assert.AreEqual(AnsiString('Test'), FASL[0]);
  Assert.AreEqual(AnsiString('Test'#13#10), FASL.Text, 'GetTextStr must write the added item and a CRLF');
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
  Assert.AreEqual(AnsiString('First'#13#10'Second'#13#10'Third'#13#10), FASL.Text, 'GetTextStr must write the items in order, each followed by CRLF');
end;


procedure TTestAnsiStringList.TestClear;
begin
  FASL.Text:= 'Item1'#13#10'Item2';
  Assert.AreEqual(2, FASL.Count);
  FASL.Clear;
  Assert.AreEqual(0, FASL.Count);
  Assert.AreEqual(AnsiString(''), FASL.Text, 'A cleared list must give an empty Text');
end;


procedure TTestAnsiStringList.TestCount;
begin
  Assert.AreEqual(0, FASL.Count);
  FASL.Text:= 'A'#13#10'B';
  Assert.AreEqual(2, FASL.Count);
  FASL.Add('C');
  Assert.AreEqual(3, FASL.Count);

  { Setting Text replaces the items, it does not append }
  FASL.Text:= 'D';
  Assert.AreEqual(1, FASL.Count);
  Assert.AreEqual(AnsiString('D'), FASL[0]);
end;


{ Text Property Tests - SetTextStr }

procedure TTestAnsiStringList.TestSetText_Empty;
begin
  { The list is filled first, so the test proves that SetTextStr clears the old items }
  FASL.Add('Old1');
  FASL.Add('Old2');

  FASL.Text:= '';
  Assert.AreEqual(0, FASL.Count, 'Setting an empty Text must remove the old items');
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
VAR
  Rtl: TStringList;
begin
  { A trailing line break does NOT create an empty item - this test used to expect 3 items.
    TAnsiTSL.SetTextStr walks the text exactly the way the RTL does: read up to CR or LF, add the
    line, then step over CR and over LF; when the text ends right after that pair the loop simply
    stops (TAnsiTSL.SetTextStr against TStrings.SetTextStr, c:\Delphi\Delphi 13\source\rtl\common\System.Classes.pas).
    The check below runs the same text through the RTL TStringList so the two can never drift. }
  FASL.Text:= 'Line1'#13#10'Line2'#13#10;
  Assert.AreEqual(2, FASL.Count);
  Assert.AreEqual(AnsiString('Line1'), FASL[0]);
  Assert.AreEqual(AnsiString('Line2'), FASL[1]);

  Rtl:= TStringList.Create;
  try
    Rtl.Text:= 'Line1'#13#10'Line2'#13#10;
    Assert.AreEqual(Rtl.Count, FASL.Count, 'TAnsiTSL must split text exactly like the RTL TStringList');
  finally
    FreeAndNil(Rtl);
  end;
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
  Assert.AreEqual(1, FASL.Count);
  Assert.AreEqual(AnsiString('Test line'), FASL[0]);

  { GetTextStr gives the line back, with a trailing CRLF }
  Assert.AreEqual(AnsiString('Test line'#13#10), FASL.Text);
end;


procedure TTestAnsiStringList.TestRoundTrip_MultipleLines;
begin
  FASL.Add('Alpha');
  FASL.Add('Beta');
  FASL.Add('Gamma');

  var Text:= FASL.Text;
  Assert.AreEqual(AnsiString('Alpha'#13#10'Beta'#13#10'Gamma'#13#10), Text);
  FASL.Clear;
  FASL.Text:= Text;

  { The trailing CRLF does not create an empty item (see TestSetText_TrailingLineBreak) }
  Assert.AreEqual(3, FASL.Count);
  Assert.AreEqual(AnsiString('Alpha'), FASL[0]);
  Assert.AreEqual(AnsiString('Beta'), FASL[1]);
  Assert.AreEqual(AnsiString('Gamma'), FASL[2]);
  Assert.AreEqual(Text, FASL.Text, 'Text -> Text must give the same text back');
end;


procedure TTestAnsiStringList.TestRoundTrip_SpecialChars;
VAR
  Text: AnsiString;
begin
  FASL.Add('Tab:'#9'here');
  FASL.Add('Null:'#0'here');
  FASL.Add('After');

  Assert.AreEqual(AnsiString('Tab:'#9'here'), FASL[0]);

  { GetTextStr copies every byte, the #0 included }
  Text:= FASL.Text;
  Assert.AreEqual('84 97 98 58 9 104 101 114 101 13 10 78 117 108 108 58 0 104 101 114 101 13 10 65 102 116 101 114 13 10', ByteCodes(Text), 'GetTextStr must keep the tab and the #0');

  { SetTextStr splits only at CR and LF: a #0 is a character of the line, as in the RTL TStrings.SetTextStr (c:\Delphi\Delphi 13\source\rtl\common\System.Classes.pas:7462-7470) }
  FASL.Text:= Text;
  Assert.AreEqual(3, FASL.Count, 'The #0 must not end the text');
  Assert.AreEqual('84 97 98 58 9 104 101 114 101', ByteCodes(FASL[0]), 'Tab line');
  Assert.AreEqual('78 117 108 108 58 0 104 101 114 101', ByteCodes(FASL[1]), 'Null line');
  Assert.AreEqual(AnsiString('After'), FASL[2]);
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

  Assert.AreEqual(AnsiString('Hello World!'#13#10'0123456789'#13#10'!@#$%^&*()'#13#10), FASL.Text, 'GetTextStr must write the ASCII lines unchanged');

  FASL.Text:= 'Hello World!'#13#10'0123456789'#13#10'!@#$%^&*()';
  Assert.AreEqual(3, FASL.Count);
  Assert.AreEqual(AnsiString('!@#$%^&*()'), FASL[2], 'SetTextStr must read the ASCII lines unchanged');
end;


procedure TTestAnsiStringList.TestAnsiChars_Extended;
VAR
  High1, High2: AnsiString;
begin
  { Extended ASCII characters (128-255), built byte by byte so no code-page conversion can change them }
  SetLength(High1, 3);
  High1[1]:= AnsiChar(128);
  High1[2]:= AnsiChar(129);
  High1[3]:= AnsiChar(130);
  SetLength(High2, 1);
  High2[1]:= AnsiChar(255);

  FASL.Add(High1);
  FASL.Add(High2);

  Assert.AreEqual('128 129 130', ByteCodes(FASL[0]));
  Assert.AreEqual('255', ByteCodes(FASL[1]));

  { Through the class's own Text routines, both ways }
  Assert.AreEqual('128 129 130 13 10 255 13 10', ByteCodes(FASL.Text), 'GetTextStr must copy the high bytes unchanged');
  FASL.Text:= FASL.Text;
  Assert.AreEqual(2, FASL.Count);
  Assert.AreEqual('128 129 130', ByteCodes(FASL[0]), 'SetTextStr must copy the high bytes unchanged');
  Assert.AreEqual('255', ByteCodes(FASL[1]));
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

  { Through the class's own Text routines, both ways }
  Assert.AreEqual(LongLine + #13#10, FASL.Text, 'GetTextStr must write the whole long line and a CRLF');
  FASL.Text:= LongLine;
  Assert.AreEqual(1, FASL.Count);
  Assert.AreEqual(LongLine, FASL[0], 'SetTextStr must read the whole long line');
end;


procedure TTestAnsiStringList.TestEdge_ManyLines;
var
  i: Integer;
  Expected: AnsiString;
begin
  Expected:= '';
  for i:= 1 to 1000 do
    begin
      FASL.Add(AnsiString('Line' + AnsiString(IntToStr(i))));
      Expected:= Expected + AnsiString('Line' + IntToStr(i)) + #13#10;
    end;

  Assert.AreEqual(1000, FASL.Count);
  Assert.AreEqual(AnsiString('Line1'), FASL[0]);
  Assert.AreEqual(AnsiString('Line1000'), FASL[999]);

  { Through the class's own Text routines, both ways }
  Assert.AreEqual(Expected, FASL.Text, 'GetTextStr must write all 1000 lines');
  FASL.Text:= Expected;
  Assert.AreEqual(1000, FASL.Count, 'SetTextStr must read back all 1000 lines');
  Assert.AreEqual(AnsiString('Line500'), FASL[499]);
  Assert.AreEqual(AnsiString('Line1000'), FASL[999]);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestAnsiStringList);

end.
