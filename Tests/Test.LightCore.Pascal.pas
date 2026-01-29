unit Test.LightCore.Pascal;

{=============================================================================================================
   Unit tests for LightCore.Pascal
   Tests Pascal source file parsing utilities

   Requires: TESTINSIGHT compiler directive for TestInsight integration
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  LightCore.Pascal;

type
  [TestFixture]
  TTestLightCorePascal = class
  private
    FTestLines: TStringList;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { LineIsAComment Tests }
    [Test]
    procedure TestLineIsAComment_SlashSlash;

    [Test]
    procedure TestLineIsAComment_CurlyBrace;

    [Test]
    procedure TestLineIsAComment_ParenStar;

    [Test]
    procedure TestLineIsAComment_NotComment;

    [Test]
    procedure TestLineIsAComment_EmptyLine;

    [Test]
    procedure TestLineIsAComment_WithLeadingSpaces;

    { SeparateComments Tests }
    [Test]
    procedure TestSeparateComments_HasComment;

    [Test]
    procedure TestSeparateComments_NoComment;

    [Test]
    procedure TestSeparateComments_OnlyComment;

    { ExtractObjectName Tests }
    [Test]
    procedure TestExtractObjectName_WithDot;

    [Test]
    procedure TestExtractObjectName_MultipleDots;

    [Test]
    procedure TestExtractObjectName_NoDot;

    [Test]
    procedure TestExtractObjectName_EmptyString;

    { IsMethod Tests }
    [Test]
    procedure TestIsMethod_Function;

    [Test]
    procedure TestIsMethod_Procedure;

    [Test]
    procedure TestIsMethod_Constructor;

    [Test]
    procedure TestIsMethod_Destructor;

    [Test]
    procedure TestIsMethod_Class;

    [Test]
    procedure TestIsMethod_Record;

    [Test]
    procedure TestIsMethod_PointerType;

    [Test]
    procedure TestIsMethod_NotMethod;

    { FindSection Tests }
    [Test]
    procedure TestFindSection_Interface;

    [Test]
    procedure TestFindSection_Implementation;

    [Test]
    procedure TestFindSection_NotFound;

    { FindLine Tests }
    [Test]
    procedure TestFindLine_Found;

    [Test]
    procedure TestFindLine_NotFound;

    [Test]
    procedure TestFindLine_WithStartAt;

    [Test]
    procedure TestFindLine_NilList;

    [Test]
    procedure TestFindLine_InvalidStartAt;

    { WordPos Tests }
    [Test]
    procedure TestWordPos_Found;

    [Test]
    procedure TestWordPos_NotFound;

    [Test]
    procedure TestWordPos_PartOfWord;

    [Test]
    procedure TestWordPos_AtBeginning;

    [Test]
    procedure TestWordPos_AtEnd;

    { RelaxedSearch Tests }
    [Test]
    procedure TestRelaxedSearch_ExactMatch;

    [Test]
    procedure TestRelaxedSearch_IgnoreSpaces;

    [Test]
    procedure TestRelaxedSearch_IgnoreCase;

    [Test]
    procedure TestRelaxedSearch_WithComment;

    [Test]
    procedure TestRelaxedSearch_NoMatch;

    { RelaxedSearchI Tests }
    [Test]
    procedure TestRelaxedSearchI_Found;

    [Test]
    procedure TestRelaxedSearchI_NotFound;

    { IsKeyword Tests }
    [Test]
    procedure TestIsKeyword_Begin;

    [Test]
    procedure TestIsKeyword_NotKeyword;

    [Test]
    procedure TestIsKeyword_PartOfWord;

    { IsReservedKeyword Tests }
    [Test]
    procedure TestIsReservedKeyword_Begin;

    [Test]
    procedure TestIsReservedKeyword_End;

    [Test]
    procedure TestIsReservedKeyword_Try;

    [Test]
    procedure TestIsReservedKeyword_NotReserved;

    { RelaxedSearchEx Tests }
    [Test]
    procedure TestRelaxedSearchEx_SimpleSearch;

    [Test]
    procedure TestRelaxedSearchEx_ANDOperator;

    [Test]
    procedure TestRelaxedSearchEx_OROperator;

    [Test]
    procedure TestRelaxedSearchEx_NOTOperator;

    [Test]
    procedure TestRelaxedSearchEx_NilHaystack;

    [Test]
    procedure TestRelaxedSearchEx_EmptyQuery;

    [Test]
    procedure TestRelaxedSearchEx_NotFound;

    { AddUnitToUses Tests }
    [Test]
    procedure TestAddUnitToUses_NilList;
  end;

implementation


procedure TTestLightCorePascal.Setup;
begin
  FTestLines:= TStringList.Create;
end;


procedure TTestLightCorePascal.TearDown;
begin
  FreeAndNil(FTestLines);
end;


{ LineIsAComment Tests }

procedure TTestLightCorePascal.TestLineIsAComment_SlashSlash;
begin
  Assert.IsTrue(LineIsAComment('// This is a comment'));
  Assert.IsTrue(LineIsAComment('//Comment without space'));
end;


procedure TTestLightCorePascal.TestLineIsAComment_CurlyBrace;
begin
  Assert.IsTrue(LineIsAComment('{ This is a comment }'));
  Assert.IsTrue(LineIsAComment('{Comment}'));
end;


procedure TTestLightCorePascal.TestLineIsAComment_ParenStar;
begin
  Assert.IsTrue(LineIsAComment('(* This is a comment *)'));
  Assert.IsTrue(LineIsAComment('(*Comment*)'));
end;


procedure TTestLightCorePascal.TestLineIsAComment_NotComment;
begin
  Assert.IsFalse(LineIsAComment('var x: Integer;'));
  Assert.IsFalse(LineIsAComment('procedure Test;'));
end;


procedure TTestLightCorePascal.TestLineIsAComment_EmptyLine;
begin
  Assert.IsFalse(LineIsAComment(''));
  Assert.IsFalse(LineIsAComment('   '));
end;


procedure TTestLightCorePascal.TestLineIsAComment_WithLeadingSpaces;
begin
  Assert.IsTrue(LineIsAComment('  // Comment with leading spaces'));
  Assert.IsTrue(LineIsAComment('    { Curly with spaces }'));
end;


{ SeparateComments Tests }

procedure TTestLightCorePascal.TestSeparateComments_HasComment;
var
  CodeLine, Comment: string;
begin
  CodeLine:= 'x:= 5; // Set x to 5';
  Assert.IsTrue(SeparateComments(CodeLine, Comment));
  Assert.AreEqual('x:= 5; ', CodeLine);
  Assert.AreEqual('// Set x to 5', Comment);
end;


procedure TTestLightCorePascal.TestSeparateComments_NoComment;
var
  CodeLine, Comment: string;
begin
  CodeLine:= 'x:= 5;';
  Assert.IsFalse(SeparateComments(CodeLine, Comment));
  Assert.AreEqual('x:= 5;', CodeLine);
  Assert.AreEqual('', Comment);
end;


procedure TTestLightCorePascal.TestSeparateComments_OnlyComment;
var
  CodeLine, Comment: string;
begin
  CodeLine:= '// Just a comment';
  Assert.IsTrue(SeparateComments(CodeLine, Comment));
  Assert.AreEqual('', CodeLine);
  Assert.AreEqual('// Just a comment', Comment);
end;


{ ExtractObjectName Tests }

procedure TTestLightCorePascal.TestExtractObjectName_WithDot;
begin
  Assert.AreEqual('Form1', ExtractObjectName('Form1.Close'));
end;


procedure TTestLightCorePascal.TestExtractObjectName_MultipleDots;
begin
  Assert.AreEqual('Form1.Button1', ExtractObjectName('Form1.Button1.SetFocus'));
end;


procedure TTestLightCorePascal.TestExtractObjectName_NoDot;
begin
  Assert.AreEqual('', ExtractObjectName('Button1'));
end;


procedure TTestLightCorePascal.TestExtractObjectName_EmptyString;
begin
  Assert.AreEqual('', ExtractObjectName(''));
end;


{ IsMethod Tests }

procedure TTestLightCorePascal.TestIsMethod_Function;
begin
  Assert.IsTrue(IsMethod('function GetValue: Integer;'));
  Assert.IsTrue(IsMethod('FUNCTION GetValue: Integer;'));
end;


procedure TTestLightCorePascal.TestIsMethod_Procedure;
begin
  Assert.IsTrue(IsMethod('procedure DoSomething;'));
  Assert.IsTrue(IsMethod('PROCEDURE DoSomething;'));
end;


procedure TTestLightCorePascal.TestIsMethod_Constructor;
begin
  Assert.IsTrue(IsMethod('constructor Create;'));
  Assert.IsTrue(IsMethod('constructor TMyClass.Create(x: Integer);'));
end;


procedure TTestLightCorePascal.TestIsMethod_Destructor;
begin
  Assert.IsTrue(IsMethod('destructor Destroy; override;'));
end;


procedure TTestLightCorePascal.TestIsMethod_Class;
begin
  Assert.IsTrue(IsMethod('TMyClass = class(TObject)'));
  Assert.IsTrue(IsMethod('TMyClass = class;'));
end;


procedure TTestLightCorePascal.TestIsMethod_Record;
begin
  Assert.IsTrue(IsMethod('TMyRecord = record'));
  Assert.IsTrue(IsMethod('TMyRecord = packed record'));
end;


procedure TTestLightCorePascal.TestIsMethod_PointerType;
begin
  Assert.IsTrue(IsMethod('PMyRecord = ^TMyRecord;'));
end;


procedure TTestLightCorePascal.TestIsMethod_NotMethod;
begin
  Assert.IsFalse(IsMethod('var x: Integer;'));
  Assert.IsFalse(IsMethod('x:= 5;'));
  Assert.IsFalse(IsMethod('begin'));
end;


{ FindSection Tests }

procedure TTestLightCorePascal.TestFindSection_Interface;
begin
  FTestLines.Add('unit Test;');
  FTestLines.Add('');
  FTestLines.Add('INTERFACE');
  FTestLines.Add('');
  FTestLines.Add('uses SysUtils;');

  Assert.AreEqual(2, FindSection(FTestLines, True));
end;


procedure TTestLightCorePascal.TestFindSection_Implementation;
begin
  FTestLines.Add('unit Test;');
  FTestLines.Add('INTERFACE');
  FTestLines.Add('uses SysUtils;');
  FTestLines.Add('IMPLEMENTATION');
  FTestLines.Add('end.');

  Assert.AreEqual(3, FindSection(FTestLines, False));
end;


procedure TTestLightCorePascal.TestFindSection_NotFound;
begin
  FTestLines.Add('unit Test;');
  FTestLines.Add('end.');

  Assert.AreEqual(-1, FindSection(FTestLines, True));
end;


{ FindLine Tests }

procedure TTestLightCorePascal.TestFindLine_Found;
begin
  FTestLines.Add('Line one');
  FTestLines.Add('Line two');
  FTestLines.Add('Target line');
  FTestLines.Add('Line four');

  Assert.AreEqual(2, FindLine('Target', FTestLines, 0));
end;


procedure TTestLightCorePascal.TestFindLine_NotFound;
begin
  FTestLines.Add('Line one');
  FTestLines.Add('Line two');

  Assert.AreEqual(-1, FindLine('NotHere', FTestLines, 0));
end;


procedure TTestLightCorePascal.TestFindLine_WithStartAt;
begin
  FTestLines.Add('Target line');
  FTestLines.Add('Other line');
  FTestLines.Add('Target line again');

  Assert.AreEqual(2, FindLine('Target', FTestLines, 1));
end;


procedure TTestLightCorePascal.TestFindLine_NilList;
begin
  Assert.AreEqual(-1, FindLine('Test', NIL, 0));
end;


procedure TTestLightCorePascal.TestFindLine_InvalidStartAt;
begin
  FTestLines.Add('Line one');

  Assert.AreEqual(-1, FindLine('Line', FTestLines, -1));
  Assert.AreEqual(-1, FindLine('Line', FTestLines, 10));
end;


{ WordPos Tests }

procedure TTestLightCorePascal.TestWordPos_Found;
begin
  Assert.AreEqual(8, WordPos('word', 'find a word here'));
end;


procedure TTestLightCorePascal.TestWordPos_NotFound;
begin
  Assert.AreEqual(0, WordPos('missing', 'find a word here'));
end;


procedure TTestLightCorePascal.TestWordPos_PartOfWord;
begin
  { Should NOT match 'Test' when it's part of 'Testing' }
  Assert.AreEqual(0, WordPos('Test', 'Testing something'));
  { Should NOT match 'Test' when it's part of 'ATest' }
  Assert.AreEqual(0, WordPos('Test', 'ATest value'));
end;


procedure TTestLightCorePascal.TestWordPos_AtBeginning;
begin
  Assert.AreEqual(1, WordPos('Hello', 'Hello world'));
end;


procedure TTestLightCorePascal.TestWordPos_AtEnd;
begin
  Assert.AreEqual(7, WordPos('world', 'Hello world'));
end;


{ RelaxedSearch Tests }

procedure TTestLightCorePascal.TestRelaxedSearch_ExactMatch;
begin
  Assert.IsTrue(RelaxedSearch('x:= 5;', 'x:=5;'));
end;


procedure TTestLightCorePascal.TestRelaxedSearch_IgnoreSpaces;
begin
  Assert.IsTrue(RelaxedSearch('x := 5;', 'x:=5;'));
  Assert.IsTrue(RelaxedSearch('x  :=  5  ;', 'x:=5;'));
end;


procedure TTestLightCorePascal.TestRelaxedSearch_IgnoreCase;
begin
  Assert.IsTrue(RelaxedSearch('X:= 5;', 'x:=5;'));
  Assert.IsTrue(RelaxedSearch('BEGIN', 'begin'));
end;


procedure TTestLightCorePascal.TestRelaxedSearch_WithComment;
begin
  Assert.IsTrue(RelaxedSearch('x:= 5; // comment', 'x:=5;'));
end;


procedure TTestLightCorePascal.TestRelaxedSearch_NoMatch;
begin
  Assert.IsFalse(RelaxedSearch('x:= 5;', 'y:=5;'));
end;


{ RelaxedSearchI Tests }

procedure TTestLightCorePascal.TestRelaxedSearchI_Found;
begin
  Assert.IsTrue(RelaxedSearchI('call SetFocus;', 'setfocus') > 0);
end;


procedure TTestLightCorePascal.TestRelaxedSearchI_NotFound;
begin
  Assert.AreEqual(-1, RelaxedSearchI('call Something;', 'setfocus'));
end;


{ IsKeyword Tests }

procedure TTestLightCorePascal.TestIsKeyword_Begin;
begin
  Assert.IsTrue(IsKeyword('begin', 'begin'));
  Assert.IsTrue(IsKeyword('  begin', 'begin'));
  Assert.IsTrue(IsKeyword('BEGIN', 'begin'));
end;


procedure TTestLightCorePascal.TestIsKeyword_NotKeyword;
begin
  Assert.IsFalse(IsKeyword('x:= 5;', 'begin'));
end;


procedure TTestLightCorePascal.TestIsKeyword_PartOfWord;
begin
  { Should not match 'except' in 'ExceptObject' }
  Assert.IsFalse(IsKeyword('ExceptObject', 'except'));
end;


{ IsReservedKeyword Tests }

procedure TTestLightCorePascal.TestIsReservedKeyword_Begin;
begin
  Assert.IsTrue(IsReservedKeyword('begin'));
  Assert.IsTrue(IsReservedKeyword('  begin'));
end;


procedure TTestLightCorePascal.TestIsReservedKeyword_End;
begin
  Assert.IsTrue(IsReservedKeyword('end;'));
  Assert.IsTrue(IsReservedKeyword('end.'));
end;


procedure TTestLightCorePascal.TestIsReservedKeyword_Try;
begin
  Assert.IsTrue(IsReservedKeyword('try'));
  Assert.IsTrue(IsReservedKeyword('except'));
  Assert.IsTrue(IsReservedKeyword('finally'));
end;


procedure TTestLightCorePascal.TestIsReservedKeyword_NotReserved;
begin
  Assert.IsFalse(IsReservedKeyword('x:= 5;'));
  Assert.IsFalse(IsReservedKeyword('procedure Test;'));
end;


{ RelaxedSearchEx Tests }

procedure TTestLightCorePascal.TestRelaxedSearchEx_SimpleSearch;
begin
  FTestLines.Add('First line');
  FTestLines.Add('Target line');
  FTestLines.Add('Third line');

  Assert.AreEqual(1, RelaxedSearchEx('target', FTestLines));
end;


procedure TTestLightCorePascal.TestRelaxedSearchEx_ANDOperator;
begin
  FTestLines.Add('if condition');
  FTestLines.Add('if condition then action');
  FTestLines.Add('then something');

  Assert.AreEqual(1, RelaxedSearchEx('if[AND]then', FTestLines));
end;


procedure TTestLightCorePascal.TestRelaxedSearchEx_OROperator;
begin
  FTestLines.Add('something else');
  FTestLines.Add('begin code');
  FTestLines.Add('more stuff');

  Assert.AreEqual(1, RelaxedSearchEx('begin[OR]end', FTestLines));
end;


procedure TTestLightCorePascal.TestRelaxedSearchEx_NOTOperator;
begin
  FTestLines.Add('procedure virtual');
  FTestLines.Add('procedure simple');
  FTestLines.Add('function test');

  Assert.AreEqual(1, RelaxedSearchEx('procedure[NOT]virtual', FTestLines));
end;


procedure TTestLightCorePascal.TestRelaxedSearchEx_NilHaystack;
begin
  Assert.AreEqual(-1, RelaxedSearchEx('test', NIL));
end;


procedure TTestLightCorePascal.TestRelaxedSearchEx_EmptyQuery;
begin
  FTestLines.Add('Some line');
  Assert.AreEqual(-1, RelaxedSearchEx('', FTestLines));
end;


procedure TTestLightCorePascal.TestRelaxedSearchEx_NotFound;
begin
  FTestLines.Add('Line one');
  FTestLines.Add('Line two');

  Assert.AreEqual(-1, RelaxedSearchEx('notfound', FTestLines));
end;


{ AddUnitToUses Tests }

procedure TTestLightCorePascal.TestAddUnitToUses_NilList;
begin
  Assert.IsFalse(AddUnitToUses(NIL, 'SomeUnit'));
end;


initialization
  TDUnitX.RegisterTestFixture(TTestLightCorePascal);

end.
