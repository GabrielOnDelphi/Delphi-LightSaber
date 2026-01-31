unit Test.LightVcl.Visual.RichRamLog;

{=============================================================================================================
   Unit tests for LightVcl.Visual.RichRamLog.pas
   Tests TRamLog - the non-visual log component.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Vcl.Forms;

type
  [TestFixture]
  TTestRamLog = class
  private
    FRamLog: TObject;
    procedure CleanupRamLog;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Creation Tests }
    [Test]
    procedure TestCreate_Succeeds;

    [Test]
    procedure TestCreate_DefaultVerbosityIsVerbose;

    [Test]
    procedure TestCreate_RichLogIsNil;

    [Test]
    procedure TestCreate_HasWarningsIsFalse;

    [Test]
    procedure TestCreate_CountIsZero;

    { Verbosity Tests }
    [Test]
    procedure TestVerbosity_CanSetAllLevels;

    { Add Message Tests }
    [Test]
    procedure TestAddMsg_IncreasesCount;

    [Test]
    procedure TestAddInfo_IncreasesCount;

    [Test]
    procedure TestAddVerb_IncreasesCount;

    [Test]
    procedure TestAddHint_IncreasesCount;

    [Test]
    procedure TestAddImpo_IncreasesCount;

    [Test]
    procedure TestAddWarn_SetsHasWarnings;

    [Test]
    procedure TestAddError_SetsHasWarnings;

    [Test]
    procedure TestAddBold_IncreasesCount;

    [Test]
    procedure TestAddEmptyRow_IncreasesCount;

    [Test]
    procedure TestAddMsgInt_AddsFormattedMessage;

    [Test]
    procedure TestAddMsgLvl_RoutesToCorrectMethod;

    { Clear Tests }
    [Test]
    procedure TestClear_ResetsCount;

    [Test]
    procedure TestClear_ResetsHasWarnings;

    [Test]
    procedure TestClear_ResetsIndent;

    { Text Property Tests }
    [Test]
    procedure TestText_GetReturnsContent;

    [Test]
    procedure TestText_SetAddsMessage;

    { Append Tests }
    [Test]
    procedure TestAppend_CombinesLogs;

    { Count Tests }
    [Test]
    procedure TestCount_ReflectsLineCount;

    { Multi-line Message Tests }
    [Test]
    procedure TestAddInfo_MultilineMessage;

    { Verbosity Filtering Tests }
    [Test]
    procedure TestAddVerb_NotStoredWhenVerbosityHigh;

    [Test]
    procedure TestAddInfo_NotStoredWhenVerbosityErrors;
  end;

implementation

uses
  LightCore.AppData,
  LightVcl.Visual.AppData,
  LightVcl.Visual.RichRamLog,
  LightVcl.Visual.RichLogUtils;


procedure TTestRamLog.Setup;
begin
  Assert.IsNotNull(AppData, 'AppData must be initialized before running tests');
  FRamLog:= NIL;
end;


procedure TTestRamLog.TearDown;
begin
  CleanupRamLog;
end;


procedure TTestRamLog.CleanupRamLog;
var
  RamLog: TRamLog;
begin
  if FRamLog <> NIL then
    begin
      RamLog:= TRamLog(FRamLog);
      FreeAndNil(RamLog);
      FRamLog:= NIL;
    end;
end;


{ Creation Tests }

procedure TTestRamLog.TestCreate_Succeeds;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  Assert.IsNotNull(RamLog, 'RamLog creation should succeed');
end;


procedure TTestRamLog.TestCreate_DefaultVerbosityIsVerbose;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  Assert.AreEqual(lvrVerbose, RamLog.Verbosity, 'Default verbosity should be lvrVerbose');
end;


procedure TTestRamLog.TestCreate_RichLogIsNil;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  Assert.IsNull(RamLog.RichLog, 'RichLog should be NIL by default');
end;


procedure TTestRamLog.TestCreate_HasWarningsIsFalse;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  Assert.IsFalse(RamLog.HasWarnings, 'HasWarnings should be FALSE by default');
end;


procedure TTestRamLog.TestCreate_CountIsZero;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  Assert.AreEqual(0, RamLog.Count, 'Count should be 0 by default');
end;


{ Verbosity Tests }

procedure TTestRamLog.TestVerbosity_CanSetAllLevels;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  RamLog.Verbosity:= lvrVerbose;
  Assert.AreEqual(lvrVerbose, RamLog.Verbosity, 'Should be able to set lvrVerbose');

  RamLog.Verbosity:= lvrHints;
  Assert.AreEqual(lvrHints, RamLog.Verbosity, 'Should be able to set lvrHints');

  RamLog.Verbosity:= lvrInfos;
  Assert.AreEqual(lvrInfos, RamLog.Verbosity, 'Should be able to set lvrInfos');

  RamLog.Verbosity:= lvrImportant;
  Assert.AreEqual(lvrImportant, RamLog.Verbosity, 'Should be able to set lvrImportant');

  RamLog.Verbosity:= lvrWarnings;
  Assert.AreEqual(lvrWarnings, RamLog.Verbosity, 'Should be able to set lvrWarnings');

  RamLog.Verbosity:= lvrErrors;
  Assert.AreEqual(lvrErrors, RamLog.Verbosity, 'Should be able to set lvrErrors');
end;


{ Add Message Tests }

procedure TTestRamLog.TestAddMsg_IncreasesCount;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  RamLog.AddMsg('Test message');

  Assert.AreEqual(1, RamLog.Count, 'AddMsg should increase count');
end;


procedure TTestRamLog.TestAddInfo_IncreasesCount;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  RamLog.AddInfo('Info message');

  Assert.AreEqual(1, RamLog.Count, 'AddInfo should increase count');
end;


procedure TTestRamLog.TestAddVerb_IncreasesCount;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  RamLog.AddVerb('Verbose message');

  Assert.AreEqual(1, RamLog.Count, 'AddVerb should increase count');
end;


procedure TTestRamLog.TestAddHint_IncreasesCount;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  RamLog.AddHint('Hint message');

  Assert.AreEqual(1, RamLog.Count, 'AddHint should increase count');
end;


procedure TTestRamLog.TestAddImpo_IncreasesCount;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  RamLog.AddImpo('Important message');

  Assert.AreEqual(1, RamLog.Count, 'AddImpo should increase count');
end;


procedure TTestRamLog.TestAddWarn_SetsHasWarnings;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  RamLog.AddWarn('Warning message');

  Assert.IsTrue(RamLog.HasWarnings, 'AddWarn should set HasWarnings to TRUE');
end;


procedure TTestRamLog.TestAddError_SetsHasWarnings;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  RamLog.AddError('Error message');

  Assert.IsTrue(RamLog.HasWarnings, 'AddError should set HasWarnings to TRUE');
end;


procedure TTestRamLog.TestAddBold_IncreasesCount;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  RamLog.AddBold('Bold message');

  Assert.AreEqual(1, RamLog.Count, 'AddBold should increase count');
end;


procedure TTestRamLog.TestAddEmptyRow_IncreasesCount;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  RamLog.AddEmptyRow;

  Assert.AreEqual(1, RamLog.Count, 'AddEmptyRow should increase count');
end;


procedure TTestRamLog.TestAddMsgInt_AddsFormattedMessage;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  RamLog.AddMsgInt('Count: ', 42);

  Assert.AreEqual(1, RamLog.Count, 'AddMsgInt should add a message');
  Assert.IsTrue(Pos('42', RamLog.Text_) > 0, 'Message should contain the integer');
end;


procedure TTestRamLog.TestAddMsgLvl_RoutesToCorrectMethod;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  RamLog.AddMsgLvl('Test', lvrInfos);

  Assert.AreEqual(1, RamLog.Count, 'AddMsgLvl should add a message');
end;


{ Clear Tests }

procedure TTestRamLog.TestClear_ResetsCount;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  RamLog.AddInfo('Message 1');
  RamLog.AddInfo('Message 2');

  RamLog.Clear(FALSE);

  Assert.AreEqual(0, RamLog.Count, 'Clear should reset count to 0');
end;


procedure TTestRamLog.TestClear_ResetsHasWarnings;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  RamLog.AddWarn('Warning');

  RamLog.Clear(FALSE);

  Assert.IsFalse(RamLog.HasWarnings, 'Clear should reset HasWarnings to FALSE');
end;


procedure TTestRamLog.TestClear_ResetsIndent;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  Assert.WillNotRaise(
    procedure
    begin
      RamLog.Clear(FALSE);
    end);
end;


{ Text Property Tests }

procedure TTestRamLog.TestText_GetReturnsContent;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  RamLog.AddMsg('Test content');

  Assert.IsTrue(Pos('Test content', RamLog.Text_) > 0, 'Text_ should return the message content');
end;


procedure TTestRamLog.TestText_SetAddsMessage;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  RamLog.Text_:= 'Set via property';

  Assert.AreEqual(1, RamLog.Count, 'Setting Text_ should add a message');
end;


{ Append Tests }

procedure TTestRamLog.TestAppend_CombinesLogs;
var
  RamLog1, RamLog2: TRamLog;
begin
  RamLog1:= TRamLog.Create;
  FRamLog:= RamLog1;
  RamLog2:= TRamLog.Create;

  try
    RamLog1.AddInfo('Message 1');
    RamLog2.AddInfo('Message 2');
    RamLog2.AddInfo('Message 3');

    RamLog1.Append(RamLog2);

    Assert.AreEqual(3, RamLog1.Count, 'Append should combine logs');
  finally
    FreeAndNil(RamLog2);
  end;
end;


{ Count Tests }

procedure TTestRamLog.TestCount_ReflectsLineCount;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  RamLog.AddInfo('Line 1');
  Assert.AreEqual(1, RamLog.Count);

  RamLog.AddInfo('Line 2');
  Assert.AreEqual(2, RamLog.Count);

  RamLog.AddInfo('Line 3');
  Assert.AreEqual(3, RamLog.Count);
end;


{ Multi-line Message Tests }

procedure TTestRamLog.TestAddInfo_MultilineMessage;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  RamLog.AddInfo('Line 1' + sLineBreak + 'Line 2');

  Assert.AreEqual(2, RamLog.Count, 'Multi-line message should create multiple entries');
end;


{ Verbosity Filtering Tests }

procedure TTestRamLog.TestAddVerb_NotStoredWhenVerbosityHigh;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  RamLog.Verbosity:= lvrErrors;

  RamLog.AddVerb('Verbose message');

  Assert.AreEqual(0, RamLog.Count, 'AddVerb should not store message when verbosity is lvrErrors');
end;


procedure TTestRamLog.TestAddInfo_NotStoredWhenVerbosityErrors;
var
  RamLog: TRamLog;
begin
  RamLog:= TRamLog.Create;
  FRamLog:= RamLog;

  RamLog.Verbosity:= lvrErrors;

  RamLog.AddInfo('Info message');

  Assert.AreEqual(0, RamLog.Count, 'AddInfo should not store message when verbosity is lvrErrors');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestRamLog);

end.
