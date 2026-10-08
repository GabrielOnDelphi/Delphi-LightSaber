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
  System.UITypes,      { TScrollStyle: ssNone, ssBoth }
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

    { Copy Tests }
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
  Winapi.Windows,
  Winapi.Messages,
  System.IOUtils,
  Vcl.Clipbrd,
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

  { TScrollStyle sits inside a SCOPEDENUMS ON region of System.UITypes - that unit switches scoped
    enumerations on at line 19 and off again at line 940, and TScrollStyle is declared at line 938 -
    so the value must carry its type in front. Vcl.StdCtrls does publish a bare ssBoth constant at
    line 451, but it is marked deprecated there. }
  Assert.AreEqual(TScrollStyle.ssBoth, RichLog.ScrollBars, 'ScrollBars should be ssBoth');
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

  RichLog.Clear;
  RichLog.Verbosity:= lvrVerbose;
  RichLog.AddVerb('VerboseLine');

  Assert.AreEqual(lvrVerbose, RichLog.Verbosity, 'Should be able to set verbosity to lvrVerbose');
  Assert.IsTrue(Pos('VerboseLine', RichLog.Text) > 0, 'At lvrVerbose a verbose message must be shown');
end;


procedure TTestRichLog.TestVerbosity_CanSetErrors;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.Clear;
  RichLog.Verbosity:= lvrErrors;
  RichLog.AddWarn('WarnLine');
  RichLog.AddError('ErrorLine');

  Assert.AreEqual(lvrErrors, RichLog.Verbosity, 'Should be able to set verbosity to lvrErrors');
  Assert.AreEqual(0, Pos('WarnLine', RichLog.Text), 'At lvrErrors a warning must be filtered out');
  Assert.IsTrue(Pos('ErrorLine', RichLog.Text) > 0, 'At lvrErrors an error must be shown');
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
  P: Integer;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.Clear;
  RichLog.AddBold('Bold message');

  P:= RichLog.FindText('Bold message', 0, Length(RichLog.Text), []);
  Assert.IsTrue(P >= 0, 'AddBold must add the text');
  RichLog.SelStart := P;
  RichLog.SelLength:= Length('Bold message');
  Assert.IsTrue(fsBold in RichLog.SelAttributes.Style, 'AddBold must write the text in bold');
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

  Assert.WillNotRaiseAny(
    procedure
    begin
      RichLog.AddDateStamp;
    end,
    'RichLog.AddDateStamp must not raise');

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
  Msg: TMsg;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.HandleNeeded;
  while PeekMessage(Msg, RichLog.Handle, WM_VSCROLL, WM_VSCROLL, PM_REMOVE) do;   { Drop any scroll already queued }

  { ScrollDown posts WM_VSCROLL to the control: with AutoScroll off, nothing may be posted }
  RichLog.AutoScroll:= FALSE;
  RichLog.AddMsg('Line without scroll');
  Assert.IsFalse(PeekMessage(Msg, RichLog.Handle, WM_VSCROLL, WM_VSCROLL, PM_REMOVE), 'AutoScroll=FALSE must not scroll');

  RichLog.AutoScroll:= TRUE;
  RichLog.AddMsg('Line with scroll');
  Assert.IsTrue(PeekMessage(Msg, RichLog.Handle, WM_VSCROLL, WM_VSCROLL, PM_REMOVE), 'AutoScroll=TRUE must scroll');
end;


{ Copy Tests }

procedure TTestRichLog.TestCopyAll_NoException;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.Clear;
  RichLog.AddMsg('Test content');
  Clipboard.Clear;

  RichLog.CopyAll;
  Assert.AreEqual('Test content', Trim(Clipboard.AsText), 'CopyAll must put the whole log on the clipboard');
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
  RichLog.Lines.Add('');
  RichLog.Lines.Add('');
  Assert.AreEqual('', RichLog.Lines[RichLog.Lines.Count-1], 'Precondition: the log ends with an empty row');

  RichLog.RemoveLastEmptyRows;
  Assert.AreEqual('Test line', RichLog.Lines[RichLog.Lines.Count-1], 'The empty rows at the end must be gone');
end;


{ SaveAsRtf Tests }

procedure TTestRichLog.TestSaveAsRtf_NoException;
var
  RichLog: TRichLog;
  TempFile, Content: string;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.AddMsg('Test content');
  { There is no AppData.ScratchDir in LightSaber - the whole repository has no such property.
    The system temporary folder is what this test actually needs. }
  TempFile:= System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetTempPath, 'test_richlog.rtf');

  try
    RichLog.SaveAsRtf(TempFile);
    Assert.IsTrue(FileExists(TempFile), 'SaveAsRtf must write the file');
    Content:= TFile.ReadAllText(TempFile);
    Assert.IsTrue(Content.StartsWith('{\rtf'), 'The file must be RTF, not plain text');
    Assert.IsTrue(Pos('Test content', Content) > 0, 'The file must hold the log text');
  finally
    if FileExists(TempFile)
    then System.SysUtils.DeleteFile(TempFile);
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
