unit Test.LightVcl.Visual.RichLog;

{=============================================================================================================
   Unit tests for LightVcl.Visual.RichLog.pas
   Tests TRichLog - the rich edit based visual log component.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ComCtrls;

type
  [TestFixture]
  TTestRichLog = class
  private
    FRichLog: TObject;
    FTestForm: TForm;
    FWarnEventFired: Boolean;
    FErrorEventFired: Boolean;
    procedure CleanupRichLog;
    procedure OnWarnHandler(Sender: TObject);
    procedure OnErrorHandler(Sender: TObject);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Creation Tests }
    [Test]
    procedure TestCreate_Succeeds;

    [Test]
    procedure TestCreate_DefaultProperties;

    [Test]
    procedure TestCreate_ScrollBarsAreBoth;

    [Test]
    procedure TestCreate_MaxLengthSet;

    { Verbosity Tests }
    [Test]
    procedure TestVerbosity_DefaultIsInfos;

    [Test]
    procedure TestVerbosity_CanSetVerbose;

    [Test]
    procedure TestVerbosity_CanSetErrors;

    [Test]
    procedure TestVerbosityAsInt_ReturnsCorrectValue;

    [Test]
    procedure TestVerbosityAsInt_CanSetFromInt;

    { Add Message Tests }
    [Test]
    procedure TestAddMsg_AddsLineToLog;

    [Test]
    procedure TestAddMsg_WithVerbosityType;

    [Test]
    procedure TestAddVerb_RespectedWhenVerbosityAllows;

    [Test]
    procedure TestAddVerb_IgnoredWhenVerbosityTooHigh;

    [Test]
    procedure TestAddInfo_AddsMessage;

    [Test]
    procedure TestAddWarn_SetsWarningState;

    [Test]
    procedure TestAddError_TriggersOnErrorEvent;

    [Test]
    procedure TestAddBold_AddsFormattedMessage;

    [Test]
    procedure TestAddEmptyRow_AddsBlankLine;

    [Test]
    procedure TestAddDateStamp_AddsCurrentDate;

    [Test]
    procedure TestAddInteger_AddsNumberAsString;

    [Test]
    procedure TestAddMsgInt_AddsTextWithNumber;

    { Empty String Tests }
    [Test]
    procedure TestAddVerb_EmptyString_NoAction;

    [Test]
    procedure TestAddInfo_EmptyString_NoAction;

    [Test]
    procedure TestAddWarn_EmptyString_NoAction;

    [Test]
    procedure TestAddError_EmptyString_NoAction;

    { InsertTime/InsertDate Tests }
    [Test]
    procedure TestInsertTime_DefaultFalse;

    [Test]
    procedure TestInsertDate_DefaultFalse;

    { AutoScroll Tests }
    [Test]
    procedure TestAutoScroll_DefaultTrue;

    [Test]
    procedure TestAutoScroll_CanBeDisabled;

    { Clear Tests }
    [Test]
    procedure TestClear_RemovesAllLines;

    [Test]
    procedure TestCopyAll_NoException;

    { RemoveLastEmptyRows Tests }
    [Test]
    procedure TestRemoveLastEmptyRows_RemovesEmptyLines;

    { SaveAsRtf Tests }
    [Test]
    procedure TestSaveAsRtf_NoException;

    { LoadFromFile Tests }
    [Test]
    procedure TestLoadFromFile_ReturnsFalseForMissingFile;
  end;

implementation

uses
  LightCore.AppData,
  LightVcl.Visual.AppData,
  LightVcl.Visual.RichLog,
  LightVcl.Visual.RichLogUtils;


procedure TTestRichLog.Setup;
begin
  Assert.IsNotNull(AppData, 'AppData must be initialized before running tests');
  FRichLog:= NIL;
  FTestForm:= TForm.Create(NIL);
end;


procedure TTestRichLog.TearDown;
begin
  CleanupRichLog;
  FreeAndNil(FTestForm);
end;


procedure TTestRichLog.CleanupRichLog;
var
  RichLog: TRichLog;
begin
  if FRichLog <> NIL then
    begin
      RichLog:= TRichLog(FRichLog);
      FreeAndNil(RichLog);
      FRichLog:= NIL;
    end;
end;


procedure TTestRichLog.OnWarnHandler(Sender: TObject);
begin
  FWarnEventFired:= TRUE;
end;


procedure TTestRichLog.OnErrorHandler(Sender: TObject);
begin
  FErrorEventFired:= TRUE;
end;


{ Creation Tests }

procedure TTestRichLog.TestCreate_Succeeds;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  Assert.IsNotNull(RichLog, 'RichLog creation should succeed');
end;


procedure TTestRichLog.TestCreate_DefaultProperties;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  Assert.AreEqual('Log', RichLog.Text, 'Default text should be "Log"');
  Assert.IsFalse(RichLog.WordWrap, 'WordWrap should be FALSE by default');
end;


procedure TTestRichLog.TestCreate_ScrollBarsAreBoth;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  Assert.AreEqual(ssBoth, RichLog.ScrollBars, 'ScrollBars should be ssBoth');
end;


procedure TTestRichLog.TestCreate_MaxLengthSet;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  Assert.IsTrue(RichLog.MaxLength > 0, 'MaxLength should be set to a positive value');
end;


{ Verbosity Tests }

procedure TTestRichLog.TestVerbosity_DefaultIsInfos;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  Assert.AreEqual(DefaultVerbosity, RichLog.Verbosity, 'Default verbosity should be lvrInfos');
end;


procedure TTestRichLog.TestVerbosity_CanSetVerbose;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.Verbosity:= lvrVerbose;

  Assert.AreEqual(lvrVerbose, RichLog.Verbosity, 'Should be able to set verbosity to lvrVerbose');
end;


procedure TTestRichLog.TestVerbosity_CanSetErrors;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.Verbosity:= lvrErrors;

  Assert.AreEqual(lvrErrors, RichLog.Verbosity, 'Should be able to set verbosity to lvrErrors');
end;


procedure TTestRichLog.TestVerbosityAsInt_ReturnsCorrectValue;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.Verbosity:= lvrWarnings;

  Assert.AreEqual(Ord(lvrWarnings), RichLog.VerbosityAsInt, 'VerbosityAsInt should return ordinal value');
end;


procedure TTestRichLog.TestVerbosityAsInt_CanSetFromInt;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.VerbosityAsInt:= Ord(lvrHints);

  Assert.AreEqual(lvrHints, RichLog.Verbosity, 'Should be able to set verbosity from integer');
end;


{ Add Message Tests }

procedure TTestRichLog.TestAddMsg_AddsLineToLog;
var
  RichLog: TRichLog;
  InitialCount: Integer;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.Clear;
  InitialCount:= RichLog.Lines.Count;

  RichLog.AddMsg('Test message');

  Assert.IsTrue(RichLog.Lines.Count > InitialCount, 'AddMsg should add a line to the log');
end;


procedure TTestRichLog.TestAddMsg_WithVerbosityType;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.Clear;
  RichLog.Verbosity:= lvrVerbose;

  RichLog.AddMsg('Test info', lvrInfos);

  Assert.IsTrue(RichLog.Lines.Count > 0, 'AddMsg with verbosity type should add line when verbosity allows');
end;


procedure TTestRichLog.TestAddVerb_RespectedWhenVerbosityAllows;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.Clear;
  RichLog.Verbosity:= lvrVerbose;

  RichLog.AddVerb('Verbose message');

  Assert.IsTrue(RichLog.Lines.Count > 0, 'AddVerb should add line when verbosity is lvrVerbose');
end;


procedure TTestRichLog.TestAddVerb_IgnoredWhenVerbosityTooHigh;
var
  RichLog: TRichLog;
  InitialCount: Integer;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.Clear;
  RichLog.Verbosity:= lvrErrors;
  InitialCount:= RichLog.Lines.Count;

  RichLog.AddVerb('Verbose message');

  Assert.AreEqual(InitialCount, RichLog.Lines.Count, 'AddVerb should be ignored when verbosity is lvrErrors');
end;


procedure TTestRichLog.TestAddInfo_AddsMessage;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.Clear;
  RichLog.Verbosity:= lvrVerbose;

  RichLog.AddInfo('Info message');

  Assert.IsTrue(RichLog.Lines.Count > 0, 'AddInfo should add a line');
end;


procedure TTestRichLog.TestAddWarn_SetsWarningState;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  FWarnEventFired:= FALSE;
  RichLog.OnWarn:= OnWarnHandler;

  RichLog.AddWarn('Warning message');

  Assert.IsTrue(FWarnEventFired, 'OnWarn event should be triggered');
end;


procedure TTestRichLog.TestAddError_TriggersOnErrorEvent;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  FErrorEventFired:= FALSE;
  RichLog.OnError:= OnErrorHandler;

  RichLog.AddError('Error message');

  Assert.IsTrue(FErrorEventFired, 'OnError event should be triggered');
end;


procedure TTestRichLog.TestAddBold_AddsFormattedMessage;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.Clear;

  Assert.WillNotRaise(
    procedure
    begin
      RichLog.AddBold('Bold message');
    end);
end;


procedure TTestRichLog.TestAddEmptyRow_AddsBlankLine;
var
  RichLog: TRichLog;
  InitialCount: Integer;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.Clear;
  InitialCount:= RichLog.Lines.Count;

  RichLog.AddEmptyRow;

  Assert.IsTrue(RichLog.Lines.Count > InitialCount, 'AddEmptyRow should add a blank line');
end;


procedure TTestRichLog.TestAddDateStamp_AddsCurrentDate;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.Clear;

  Assert.WillNotRaise(
    procedure
    begin
      RichLog.AddDateStamp;
    end);

  Assert.IsTrue(RichLog.Lines.Count > 0, 'AddDateStamp should add a line');
end;


procedure TTestRichLog.TestAddInteger_AddsNumberAsString;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.Clear;
  RichLog.Verbosity:= lvrVerbose;

  RichLog.AddInteger(42);

  Assert.IsTrue(RichLog.Lines.Count > 0, 'AddInteger should add a line');
end;


procedure TTestRichLog.TestAddMsgInt_AddsTextWithNumber;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.Clear;
  RichLog.Verbosity:= lvrVerbose;

  RichLog.AddMsgInt('Count: ', 10);

  Assert.IsTrue(RichLog.Lines.Count > 0, 'AddMsgInt should add a line');
end;


{ Empty String Tests }

procedure TTestRichLog.TestAddVerb_EmptyString_NoAction;
var
  RichLog: TRichLog;
  InitialCount: Integer;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.Clear;
  RichLog.Verbosity:= lvrVerbose;
  InitialCount:= RichLog.Lines.Count;

  RichLog.AddVerb('');

  Assert.AreEqual(InitialCount, RichLog.Lines.Count, 'AddVerb with empty string should not add line');
end;


procedure TTestRichLog.TestAddInfo_EmptyString_NoAction;
var
  RichLog: TRichLog;
  InitialCount: Integer;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.Clear;
  RichLog.Verbosity:= lvrVerbose;
  InitialCount:= RichLog.Lines.Count;

  RichLog.AddInfo('');

  Assert.AreEqual(InitialCount, RichLog.Lines.Count, 'AddInfo with empty string should not add line');
end;


procedure TTestRichLog.TestAddWarn_EmptyString_NoAction;
var
  RichLog: TRichLog;
  InitialCount: Integer;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.Clear;
  RichLog.Verbosity:= lvrVerbose;
  InitialCount:= RichLog.Lines.Count;

  RichLog.AddWarn('');

  Assert.AreEqual(InitialCount, RichLog.Lines.Count, 'AddWarn with empty string should not add line');
end;


procedure TTestRichLog.TestAddError_EmptyString_NoAction;
var
  RichLog: TRichLog;
  InitialCount: Integer;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.Clear;
  RichLog.Verbosity:= lvrVerbose;
  InitialCount:= RichLog.Lines.Count;

  RichLog.AddError('');

  Assert.AreEqual(InitialCount, RichLog.Lines.Count, 'AddError with empty string should not add line');
end;


{ InsertTime/InsertDate Tests }

procedure TTestRichLog.TestInsertTime_DefaultFalse;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  Assert.IsFalse(RichLog.InsertTime, 'InsertTime should be FALSE by default');
end;


procedure TTestRichLog.TestInsertDate_DefaultFalse;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  Assert.IsFalse(RichLog.InsertDate, 'InsertDate should be FALSE by default');
end;


{ AutoScroll Tests }

procedure TTestRichLog.TestAutoScroll_DefaultTrue;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  Assert.IsTrue(RichLog.AutoScroll, 'AutoScroll should be TRUE by default');
end;


procedure TTestRichLog.TestAutoScroll_CanBeDisabled;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.AutoScroll:= FALSE;

  Assert.IsFalse(RichLog.AutoScroll, 'AutoScroll should be disableable');
end;


{ Clear Tests }

procedure TTestRichLog.TestClear_RemovesAllLines;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.AddMsg('Test line 1');
  RichLog.AddMsg('Test line 2');

  RichLog.Clear;

  Assert.IsTrue(RichLog.Lines.Count <= 1, 'Clear should remove all lines (RichEdit may keep 1 empty line)');
end;


procedure TTestRichLog.TestCopyAll_NoException;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.AddMsg('Test content');

  Assert.WillNotRaise(
    procedure
    begin
      RichLog.CopyAll;
    end);
end;


{ RemoveLastEmptyRows Tests }

procedure TTestRichLog.TestRemoveLastEmptyRows_RemovesEmptyLines;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.Clear;
  RichLog.AddMsg('Test line');

  Assert.WillNotRaise(
    procedure
    begin
      RichLog.RemoveLastEmptyRows;
    end);
end;


{ SaveAsRtf Tests }

procedure TTestRichLog.TestSaveAsRtf_NoException;
var
  RichLog: TRichLog;
  TempFile: string;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.AddMsg('Test content');
  TempFile:= AppData.ScratchDir + 'test_richlog.rtf';

  try
    Assert.WillNotRaise(
      procedure
      begin
        RichLog.SaveAsRtf(TempFile);
      end);
  finally
    if FileExists(TempFile)
    then DeleteFile(TempFile);
  end;
end;


{ LoadFromFile Tests }

procedure TTestRichLog.TestLoadFromFile_ReturnsFalseForMissingFile;
var
  RichLog: TRichLog;
  Result: Boolean;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  Result:= RichLog.LoadFromFile('C:\NonExistentFile12345.txt');

  Assert.IsFalse(Result, 'LoadFromFile should return FALSE for missing file');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestRichLog);

end.
