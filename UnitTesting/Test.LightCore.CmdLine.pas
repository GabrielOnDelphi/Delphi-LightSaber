unit Test.LightCore.CmdLine;

{=============================================================================================================
   Unit tests for LightCore.CmdLine
   Tests TCmdLineParser - declarative command-line parser (switches, positionals, generated usage text)

   All tests go through Parse(array of string), the overload made for testing.
   The parameterless Parse only forwards ParamStr(1..ParamCount) to it.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  LightCore.CmdLine;

type
  [TestFixture]
  TTestLightCoreCmdLine = class
  private
    Parser: TCmdLineParser;
    procedure RegisterStandardSet;   { --visible flag, --wait int 1200, --selector str }
  public
    [Setup]    procedure Setup;
    [TearDown] procedure TearDown;

    { Flags }
    [Test] procedure TestFlag_NotGiven_False;
    [Test] procedure TestFlag_Given_True;

    { String switches }
    [Test] procedure TestStr_NotGiven_ReturnsDefault;
    [Test] procedure TestStr_Given_ReturnsValue;
    [Test] procedure TestStr_ValueLooksLikeSwitch_IsConsumedAsValue;

    { Integer switches }
    [Test] procedure TestInt_NotGiven_ReturnsDefault;
    [Test] procedure TestInt_Given_ReturnsValue;
    [Test] procedure TestInt_NegativeValue_Accepted;
    [Test] procedure TestInt_BadNumber_ParseFails;

    { Grammar }
    [Test] procedure TestValueSwitch_LastOnLine_ParseFails;
    [Test] procedure TestUnknownSwitch_ParseFails;
    [Test] procedure TestSwitchName_CaseInsensitive;
    [Test] procedure TestSingleDashParam_IsPositionalNotSwitch;

    { Positionals }
    [Test] procedure TestPositionals_CollectedInOrder_MixedWithSwitches;
    [Test] procedure TestPositionals_TooMany_ParseFails;
    [Test] procedure TestPositionals_MissingOne_NamesTheMissingOne;
    [Test] procedure TestPositionals_NoneRegistered_UnexpectedArgument;
    [Test] procedure TestPositional_OptionalNotGiven_ReturnsEmpty;
    [Test] procedure TestPositional_IndexZero_Raises;

    { Given }
    [Test] procedure TestGiven_ExplicitDefaultValue_StillGiven;

    { Re-parse }
    [Test] procedure TestParseTwice_StateIsReset;
    [Test] procedure TestFailedParse_KeepsPositionalsCollectedBeforeTheError;

    { Programmer errors }
    [Test] procedure TestRegisterTwice_Raises;
    [Test] procedure TestRegisterWithoutDoubleDash_Raises;
    [Test] procedure TestQueryUnregistered_Raises;
    [Test] procedure TestQueryWrongKind_Raises;

    { Usage text }
    [Test] procedure TestUsageText_ContainsEverySwitchAndValueName;

    { Full realistic line }
    [Test] procedure TestFullCommandLine_AllFieldsLand;
  end;

implementation


procedure TTestLightCoreCmdLine.Setup;
begin
  Parser:= TCmdLineParser.Create;
end;


procedure TTestLightCoreCmdLine.TearDown;
begin
  FreeAndNil(Parser);
end;


procedure TTestLightCoreCmdLine.RegisterStandardSet;
begin
  Parser.AddFlag('--visible' , 'Show the window.');
  Parser.AddInt ('--wait'    , 'Settle time.', 1200, '<ms>');
  Parser.AddStr ('--selector', 'CSS region.', '', '<css>');
end;


{ Flags }

procedure TTestLightCoreCmdLine.TestFlag_NotGiven_False;
begin
  RegisterStandardSet;
  Assert.IsTrue(Parser.Parse([]), Parser.Error);
  Assert.IsFalse(Parser.Flag('--visible'));
  Assert.IsFalse(Parser.Given('--visible'));
end;


procedure TTestLightCoreCmdLine.TestFlag_Given_True;
begin
  RegisterStandardSet;
  Assert.IsTrue(Parser.Parse(['--visible']), Parser.Error);
  Assert.IsTrue(Parser.Flag('--visible'));
  Assert.IsTrue(Parser.Given('--visible'));
end;


{ String switches }

procedure TTestLightCoreCmdLine.TestStr_NotGiven_ReturnsDefault;
begin
  Parser.AddStr('--out', 'Output.', 'default.txt');
  Assert.IsTrue(Parser.Parse([]), Parser.Error);
  Assert.AreEqual('default.txt', Parser.Str('--out'));
end;


procedure TTestLightCoreCmdLine.TestStr_Given_ReturnsValue;
begin
  RegisterStandardSet;
  Assert.IsTrue(Parser.Parse(['--selector', 'div.content']), Parser.Error);
  Assert.AreEqual('div.content', Parser.Str('--selector'));
end;


procedure TTestLightCoreCmdLine.TestStr_ValueLooksLikeSwitch_IsConsumedAsValue;
begin
  { The param after a value switch is ALWAYS its value - documented grammar rule }
  RegisterStandardSet;
  Assert.IsTrue(Parser.Parse(['--selector', '--visible']), Parser.Error);
  Assert.AreEqual('--visible', Parser.Str('--selector'));
  Assert.IsFalse(Parser.Flag('--visible'), '--visible was eaten as the selector value, it is not a flag here');
end;


{ Integer switches }

procedure TTestLightCoreCmdLine.TestInt_NotGiven_ReturnsDefault;
begin
  RegisterStandardSet;
  Assert.IsTrue(Parser.Parse([]), Parser.Error);
  Assert.AreEqual(1200, Parser.Int('--wait'));
end;


procedure TTestLightCoreCmdLine.TestInt_Given_ReturnsValue;
begin
  RegisterStandardSet;
  Assert.IsTrue(Parser.Parse(['--wait', '500']), Parser.Error);
  Assert.AreEqual(500, Parser.Int('--wait'));
end;


procedure TTestLightCoreCmdLine.TestInt_NegativeValue_Accepted;
begin
  { '-5' does not start with '--', and the value slot is consumed blindly anyway }
  RegisterStandardSet;
  Assert.IsTrue(Parser.Parse(['--wait', '-5']), Parser.Error);
  Assert.AreEqual(-5, Parser.Int('--wait'));
end;


procedure TTestLightCoreCmdLine.TestInt_BadNumber_ParseFails;
begin
  RegisterStandardSet;
  Assert.IsFalse(Parser.Parse(['--wait', 'fast']));
  Assert.Contains(Parser.Error, 'needs a number');
  Assert.Contains(Parser.Error, 'fast');
end;


{ Grammar }

procedure TTestLightCoreCmdLine.TestValueSwitch_LastOnLine_ParseFails;
begin
  RegisterStandardSet;
  Assert.IsFalse(Parser.Parse(['--wait']));
  Assert.Contains(Parser.Error, 'needs a value');
end;


procedure TTestLightCoreCmdLine.TestUnknownSwitch_ParseFails;
begin
  RegisterStandardSet;
  Assert.IsFalse(Parser.Parse(['--vissible']), 'A typo must fail loudly, not be ignored');
  Assert.Contains(Parser.Error, 'Unknown switch');
  Assert.Contains(Parser.Error, '--vissible');
end;


procedure TTestLightCoreCmdLine.TestSwitchName_CaseInsensitive;
begin
  RegisterStandardSet;
  Assert.IsTrue(Parser.Parse(['--WAIT', '77', '--Visible']), Parser.Error);
  Assert.AreEqual(77, Parser.Int('--wait'));
  Assert.IsTrue(Parser.Flag('--visible'));
end;


procedure TTestLightCoreCmdLine.TestSingleDashParam_IsPositionalNotSwitch;
begin
  { Only '--' marks a switch. '-x' is a positional. }
  Parser.SetPositionals(1, 1, '<file>');
  Assert.IsTrue(Parser.Parse(['-x']), Parser.Error);
  Assert.AreEqual('-x', Parser.Positional(1));
end;


{ Positionals }

procedure TTestLightCoreCmdLine.TestPositionals_CollectedInOrder_MixedWithSwitches;
begin
  RegisterStandardSet;
  Parser.SetPositionals(2, 2, '<url> <outfile>');
  Assert.IsTrue(Parser.Parse(['http://x.com', '--wait', '9', 'out.txt', '--visible']), Parser.Error);
  Assert.AreEqual(2, Parser.PositionalCount);
  Assert.AreEqual('http://x.com', Parser.Positional(1));
  Assert.AreEqual('out.txt', Parser.Positional(2));
  Assert.AreEqual(9, Parser.Int('--wait'));
  Assert.IsTrue(Parser.Flag('--visible'));
end;


procedure TTestLightCoreCmdLine.TestPositionals_TooMany_ParseFails;
begin
  Parser.SetPositionals(2, 2, '<url> <outfile>');
  Assert.IsFalse(Parser.Parse(['a', 'b', 'c']));
  Assert.Contains(Parser.Error, 'Too many arguments');
  Assert.Contains(Parser.Error, 'c');
end;


procedure TTestLightCoreCmdLine.TestPositionals_MissingOne_NamesTheMissingOne;
begin
  Parser.SetPositionals(2, 2, '<url> <outfile.txt>');
  Assert.IsFalse(Parser.Parse(['http://x.com']));
  Assert.AreEqual('Missing argument: <outfile.txt>', Parser.Error);
end;


procedure TTestLightCoreCmdLine.TestPositionals_NoneRegistered_UnexpectedArgument;
begin
  RegisterStandardSet;
  Assert.IsFalse(Parser.Parse(['stray']));
  Assert.Contains(Parser.Error, 'Unexpected argument');
  Assert.Contains(Parser.Error, 'stray');
end;


procedure TTestLightCoreCmdLine.TestPositional_OptionalNotGiven_ReturnsEmpty;
begin
  Parser.SetPositionals(1, 3, '<in> <out> <log>');
  Assert.IsTrue(Parser.Parse(['in.txt', 'out.txt']), Parser.Error);
  Assert.AreEqual(2, Parser.PositionalCount);
  Assert.AreEqual('', Parser.Positional(3));
end;


procedure TTestLightCoreCmdLine.TestPositional_IndexZero_Raises;
begin
  Parser.SetPositionals(1, 1, '<file>');
  Assert.IsTrue(Parser.Parse(['x']), Parser.Error);
  Assert.WillRaise(
    procedure
    begin
      Parser.Positional(0);
    end,
    ECmdLineParser);
end;


{ Given }

procedure TTestLightCoreCmdLine.TestGiven_ExplicitDefaultValue_StillGiven;
begin
  RegisterStandardSet;
  Assert.IsTrue(Parser.Parse(['--wait', '1200']), Parser.Error);
  Assert.AreEqual(1200, Parser.Int('--wait'));
  Assert.IsTrue(Parser.Given('--wait'), 'Explicitly passing the default value still counts as given');
end;


{ Re-parse }

procedure TTestLightCoreCmdLine.TestParseTwice_StateIsReset;
begin
  RegisterStandardSet;
  Parser.SetPositionals(0, 2, '<a> <b>');

  Assert.IsTrue(Parser.Parse(['--visible', '--wait', '9', 'pos1']), Parser.Error);
  Assert.IsTrue(Parser.Flag('--visible'));
  Assert.AreEqual(9, Parser.Int('--wait'));
  Assert.AreEqual(1, Parser.PositionalCount);

  Assert.IsTrue(Parser.Parse([]), Parser.Error);
  Assert.IsFalse(Parser.Flag('--visible'), 'Second Parse must clear the first run');
  Assert.AreEqual(1200, Parser.Int('--wait'), 'Second Parse must restore the default');
  Assert.AreEqual(0, Parser.PositionalCount);
  Assert.AreEqual('', Parser.Error);
end;


procedure TTestLightCoreCmdLine.TestFailedParse_KeepsPositionalsCollectedBeforeTheError;
begin
  { UncensoredClaude depends on this: on a bad switch it recovers the already-seen output file
    and writes the error message into it }
  Parser.SetPositionals(2, 2, '<url> <outfile>');
  Assert.IsFalse(Parser.Parse(['http://x.com', 'out.txt', '--nope']));
  Assert.AreEqual('http://x.com', Parser.Positional(1));
  Assert.AreEqual('out.txt', Parser.Positional(2));
end;


{ Programmer errors }

procedure TTestLightCoreCmdLine.TestRegisterTwice_Raises;
begin
  Parser.AddFlag('--x', 'First.');
  Assert.WillRaise(
    procedure
    begin
      Parser.AddInt('--x', 'Second, other kind, same name.');
    end,
    ECmdLineParser);
end;


procedure TTestLightCoreCmdLine.TestRegisterWithoutDoubleDash_Raises;
begin
  Assert.WillRaise(
    procedure
    begin
      Parser.AddFlag('visible', 'Missing dashes.');
    end,
    ECmdLineParser);
end;


procedure TTestLightCoreCmdLine.TestQueryUnregistered_Raises;
begin
  RegisterStandardSet;
  Assert.IsTrue(Parser.Parse([]), Parser.Error);
  Assert.WillRaise(
    procedure
    begin
      Parser.Flag('--nope');
    end,
    ECmdLineParser);
end;


procedure TTestLightCoreCmdLine.TestQueryWrongKind_Raises;
begin
  RegisterStandardSet;
  Assert.IsTrue(Parser.Parse([]), Parser.Error);
  Assert.WillRaise(
    procedure
    begin
      Parser.Flag('--wait');   { --wait is an Int switch, not a flag }
    end,
    ECmdLineParser);
end;


{ Usage text }

procedure TTestLightCoreCmdLine.TestUsageText_ContainsEverySwitchAndValueName;
VAR Usage: string;
begin
  RegisterStandardSet;
  Usage:= Parser.UsageText;
  Assert.Contains(Usage, 'Switches:');
  Assert.Contains(Usage, '--visible');
  Assert.Contains(Usage, '--wait <ms>');
  Assert.Contains(Usage, '--selector <css>');
  Assert.Contains(Usage, 'Settle time.');
  Assert.Contains(Usage, 'Show the window.');
end;


{ Full realistic line }

procedure TTestLightCoreCmdLine.TestFullCommandLine_AllFieldsLand;
begin
  { The UncensoredClaude.exe switch set, end to end }
  Parser.SetPositionals(2, 2, '<url> <outfile.txt>');
  Parser.AddFlag('--visible'   , 'h');
  Parser.AddInt ('--wait'      , 'h', 1200, '<ms>');
  Parser.AddInt ('--timeout'   , 'h', 30000, '<ms>');
  Parser.AddStr ('--selector'  , 'h', '', '<css>');
  Parser.AddInt ('--max-chars' , 'h', 0, '<n>');
  Parser.AddFlag('--raw'       , 'h');
  Parser.AddFlag('--no-rewrite', 'h');

  Assert.IsTrue(Parser.Parse(['https://old.reddit.com/r/delphi', 'c:\temp\out.txt', '--wait', '2500', '--selector', 'div.sitetable', '--raw']), Parser.Error);

  Assert.AreEqual('https://old.reddit.com/r/delphi', Parser.Positional(1));
  Assert.AreEqual('c:\temp\out.txt', Parser.Positional(2));
  Assert.AreEqual(2500, Parser.Int('--wait'));
  Assert.AreEqual(30000, Parser.Int('--timeout'), 'Untouched switch keeps its default');
  Assert.AreEqual('div.sitetable', Parser.Str('--selector'));
  Assert.AreEqual(0, Parser.Int('--max-chars'));
  Assert.IsTrue(Parser.Flag('--raw'));
  Assert.IsFalse(Parser.Flag('--visible'));
  Assert.IsFalse(Parser.Flag('--no-rewrite'));
end;


initialization
  TDUnitX.RegisterTestFixture(TTestLightCoreCmdLine);

end.
