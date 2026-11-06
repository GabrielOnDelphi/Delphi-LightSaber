unit Test.LightVcl.Visual.RichLog;

{=============================================================================================================
   2026.11.06
   Unit tests for LightVcl.Visual.RichLog.pas
   Tests TRichLog - the rich edit view of a TRamLog (virtual scrolling, verbosity filter, events).

   Every control sits on a hidden TForm.Create(NIL) that is never shown.
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
  Vcl.StdCtrls,
  Vcl.ComCtrls;

type
  [TestFixture]
  TTestRichLog = class
  private
    FRichLog: TObject;
    FTestForm: TForm;
    FWarnEventFired: Boolean;
    FErrorEventFired: Boolean;
    FWarnCount: Integer;
    FErrorCount: Integer;
    procedure CleanupRichLog;
    procedure OnWarnHandler(Sender: TObject);
    procedure OnErrorHandler(Sender: TObject);
    function  makeLog(aHeight: Integer = 0): TObject;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Creation Tests }
    [Test]
    procedure TestCreate_DefaultProperties;

    [Test]
    procedure TestCreate_NoOwnVerticalScrollBar;

    [Test]
    procedure TestCreate_MaxLengthSet;

    [Test]
    procedure TestCreate_OwnsARamLog;

    [Test]
    procedure TestWordWrap_ForcedOff;

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

    [Test]
    procedure TestVerbosity_Lowered_ShowsHiddenLine;

    [Test]
    procedure TestVerbosity_FiltersForcedMessagesToo;

    [Test]
    procedure TestVerbosity_LowestAlsoShowsDebug;

    { Level conversion Tests }
    [Test]
    procedure TestVerb2Level_EveryLevel;

    [Test]
    procedure TestLevel2Verb_EveryLevel;

    [Test]
    procedure TestLevel2Color_UsesRichLogPalette;

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

    [Test]
    procedure TestAddFromFile_OneEntryPerLine;

    { Empty String Tests }
    [Test]
    procedure TestAddVerb_EmptyString_NoAction;

    [Test]
    procedure TestAddInfo_EmptyString_NoAction;

    [Test]
    procedure TestAddWarn_EmptyString_NoAction;

    [Test]
    procedure TestAddError_EmptyString_NoAction;

    { The TRamLog behind the control }
    [Test]
    procedure TestRamLogLine_AppearsInControl;

    [Test]
    procedure TestClear_EmptiesControlAndLog;

    [Test]
    procedure TestMultiLineMsg_OneEntrySeveralLines;

    [Test]
    procedure TestBreakKinds_EachIsOneLine;

    [Test]
    procedure TestTrailingBreak_Dropped;

    [Test]
    procedure TestReset_LoadFromFile_RedrawsWithoutEvents;

    { Events }
    [Test]
    procedure TestOnWarn_FiresOnceForHiddenLine;

    [Test]
    procedure TestOnError_NotFiredAgainOnVerbosityChange;

    { External log }
    [Test]
    procedure TestExternalLog_FreeingRichLog_LeavesLogWorking;

    [Test]
    procedure TestExternalLog_ExistingLinesDrawn_NoEvents;

    [Test]
    procedure TestAssignExternalRamLog_Nil_Raises;

    [Test]
    procedure TestPopUpWindow_FormDestroying_DoesNothing;

    { InsertTime/InsertDate Tests }
    [Test]
    procedure TestInsertTime_DefaultFalse;

    [Test]
    procedure TestInsertDate_DefaultFalse;

    [Test]
    procedure TestInsertTime_UsesStoredTime;

    [Test]
    procedure TestInsertDate_UsesStoredDate;

    { Virtual scrolling Tests }
    [Test]
    procedure TestAutoScroll_DefaultTrue;

    [Test]
    procedure TestAutoScroll_CanBeDisabled;

    [Test]
    procedure TestVirtualScroll_HoldsOnePage;

    [Test]
    procedure TestVirtualScroll_TopEntryDrawsFirstEntries;

    [Test]
    procedure TestAutoScroll_KeepsLastEntryOnScreen;

    [Test]
    procedure TestAppendAndTrim_WholeEntriesOnly;

    [Test]
    procedure TestAppend_EntryOfCapLines_DrawnWhole;

    [Test]
    procedure TestAppend_PageOfOneLine_ShowsNewestEntry;

    [Test]
    procedure TestKeyboard_PageDownPageUp;

    [Test]
    procedure TestMouseWheel_ScrollsEntries;

    [Test]
    procedure TestScrollBar_VisibleOnlyWhenNeeded;

    [Test]
    procedure TestScrollBar_Padding_NoAlignPerMessage;

    [Test]
    procedure TestScrollBar_PaddingRight_TextNotUnderBar;

    [Test]
    procedure TestNewsBody_OneEntryPerLine_AllReachable;

    [Test]
    procedure TestTallEntry_CutAtPageBottom;

    { DFM Tests }
    [Test]
    procedure TestDfmLines_RealTextBecomesEntry;

    [Test]
    procedure TestDfmLines_PlaceholderIgnored;

    { Copy Tests }
    [Test]
    procedure TestCopyAll_NoException;

    { SaveAsRtf Tests }
    [Test]
    procedure TestSaveAsRtf_NoException;

    [Test]
    procedure TestSaveAsRtf_MultiLineEntries;

    [Test]
    procedure TestSaveAsRtf_BigLog_NotCut;

    { AppendToFile / LoadFromFile Tests }
    [Test]
    procedure TestAppendToFile_WritesTheLog;

    [Test]
    procedure TestLoadFromFile_ReturnsFalseForMissingFile;

    [Test]
    procedure TestLoadFromFile_TextLinesBecomeEntries;
  end;

implementation

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.Math,
  System.IOUtils,
  Vcl.Clipbrd,
  LightCore.AppData,
  LightCore.LogTypes,
  LightCore.LogRam,
  LightVcl.Visual.AppData,
  LightVcl.Visual.RichLog,
  LightVcl.Visual.RichLogUtils;


TYPE
  { Reaches the protected mouse wheel handler of TControl }
  TRichLogAccess = class(TRichLog);

  { Reaches the protected observer list of TRamLog }
  TRamLogAccess = class(TRamLog);

  { Counts the align passes of the control }
  TRichLogAlignCounter = class(TRichLog)
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
  public
    AlignCount: Integer;
  end;


procedure TRichLogAlignCounter.AlignControls(AControl: TControl; var Rect: TRect);
begin
  Inc(AlignCount);
  inherited AlignControls(AControl, Rect);
end;


CONST
  ManyEntries = 200;   { Far more entries than a 120 px high control can show }


procedure TTestRichLog.Setup;
begin
  Assert.IsNotNull(AppData, 'AppData must be initialized before running tests');
  FRichLog:= NIL;
  FWarnCount:= 0;
  FErrorCount:= 0;
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
  Inc(FWarnCount);
end;


procedure TTestRichLog.OnErrorHandler(Sender: TObject);
begin
  FErrorEventFired:= TRUE;
  Inc(FErrorCount);
end;


{ A TRichLog on the hidden test form, with a window handle. aHeight 0 keeps the default height. }
function TTestRichLog.makeLog(aHeight: Integer = 0): TObject;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;
  if aHeight > 0
  then RichLog.Height:= aHeight;
  RichLog.Width:= 300;
  RichLog.HandleNeeded;
  Result:= RichLog;
end;


{ The child scroll bar of the control (it is a private field; it is the only TScrollBar on the control) }
function ChildScrollBar(RichLog: TRichLog): TScrollBar;
var
  i: Integer;
begin
  Result:= NIL;
  for i:= 0 to RichLog.ControlCount - 1 do
    if RichLog.Controls[i] is TScrollBar
    then EXIT(TScrollBar(RichLog.Controls[i]));
  Assert.IsNotNull(Result, 'The control must hold its own TScrollBar');
end;


{ The vertical distance between the tops of line 0 and line 1, measured on the control itself. 0 if the control has fewer than 2 lines. }
function MeasuredLinePitch(RichLog: TRichLog): Integer;
var
  P0, P1: TPoint;
  Index1: Integer;
begin
  Result:= 0;
  if RichLog.Lines.Count < 2 then EXIT;
  Index1:= SendMessage(RichLog.Handle, EM_LINEINDEX, 1, 0);
  SendMessage(RichLog.Handle, EM_POSFROMCHAR, WPARAM(@P0), 0);
  SendMessage(RichLog.Handle, EM_POSFROMCHAR, WPARAM(@P1), Index1);
  Result:= P1.Y - P0.Y;
end;


{ The height in which text is visible: the format rectangle of the rich edit, cut at the bottom of the client area }
function VisiblePageHeight(RichLog: TRichLog): Integer;
var
  FormatRect: TRect;
begin
  SendMessage(RichLog.Handle, EM_GETRECT, 0, LPARAM(@FormatRect));
  Result:= Min(FormatRect.Bottom, RichLog.ClientHeight) - FormatRect.Top;
end;


{ A routine of its own, so the copy of the observer list (interface references) is released when it returns, before the test frees an observer }
function ObserverCount(Log: TRamLog): Integer;
begin
  Result:= Length(TRamLogAccess(Log).GetObservers);
end;


{ Adds Count info entries 'Entry 0', 'Entry 1', ... to the log of RichLog }
procedure AddEntries(RichLog: TRichLog; Count: Integer);
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    RichLog.RamLog.AddInfo('Entry ' + IntToStr(i));
end;


function LastLine(RichLog: TRichLog): string;
begin
  Assert.IsTrue(RichLog.Lines.Count > 0, 'The control must show at least one line');
  Result:= RichLog.Lines[RichLog.Lines.Count - 1];
end;


function TempFilePath(CONST FileName: string): string;
begin
  { There is no AppData.ScratchDir in LightSaber - the whole repository has no such property.
    The system temporary folder is what these tests actually need. }
  Result:= TPath.Combine(TPath.GetTempPath, FileName);
end;


{-------------------------------------------------------------------------------------------------------------
   Creation Tests
-------------------------------------------------------------------------------------------------------------}

procedure TTestRichLog.TestCreate_DefaultProperties;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog(makeLog);

  { The constructor writes the placeholder 'Log' (it shows in the form designer); the first draw of the empty log removes it }
  Assert.AreEqual('', RichLog.Text, 'The view of an empty log must be empty');
  Assert.IsFalse(RichLog.WordWrap, 'WordWrap should be FALSE by default');
  Assert.AreEqual(0, RichLog.TopEntry, 'An empty log is drawn from its first entry');
end;


{ The vertical scroll bar belongs to the TRamLog (a child TScrollBar), so the rich edit window has none of its own.
  ScrollBars keeps ssBoth for the DFMs that store it; the horizontal one stays, always visible (disabled when not needed), so the page height never changes.
  The check runs with more entries than fit: a rich edit clears WS_VSCROLL from its style while it hides an unneeded bar. }
procedure TTestRichLog.TestCreate_NoOwnVerticalScrollBar;
var
  RichLog: TRichLog;
  Style: Cardinal;
begin
  RichLog:= TRichLog(makeLog(120));
  AddEntries(RichLog, ManyEntries);

  { TScrollStyle sits inside a SCOPEDENUMS ON region of System.UITypes, so the value must carry its type in front }
  Assert.AreEqual(System.UITypes.TScrollStyle.ssBoth, RichLog.ScrollBars, 'ScrollBars should stay ssBoth');   { Unit-qualified: Vcl.StdCtrls declares a deprecated TScrollStyle of its own }
  Style:= Cardinal(GetWindowLong(RichLog.Handle, GWL_STYLE));
  Assert.IsTrue((Style AND WS_VSCROLL) = 0,      'The rich edit window must not have its own vertical scroll bar');
  Assert.IsTrue((Style AND WS_HSCROLL) <> 0,     'The horizontal scroll bar stays');
  Assert.IsTrue((Style AND WS_CLIPCHILDREN) <> 0, 'The rich edit must not paint over its child scroll bar');
end;


procedure TTestRichLog.TestCreate_MaxLengthSet;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  Assert.AreEqual($7FFFFFF0, RichLog.MaxLength, 'The constructor must lift the text limit to $7FFFFFF0');
  { TCustomEdit.CreateWnd passes FMaxLength to the window: c:\Delphi\Delphi 13\source\vcl\Vcl.StdCtrls.pas:3426 }
  Assert.AreEqual($7FFFFFF0, Integer(SendMessage(RichLog.Handle, EM_GETLIMITTEXT, 0, 0)), 'The rich edit window must carry the same limit');
end;


procedure TTestRichLog.TestCreate_OwnsARamLog;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog(makeLog);

  Assert.IsNotNull(RichLog.RamLog, 'A dropped TRichLog must work at once: it creates its own TRamLog');
  Assert.IsFalse(RichLog.RamLog.ShowOnError, 'The own log must not pop up a window on an error');
  Assert.AreEqual(1, Length(TRamLogAccess(RichLog.RamLog).GetObservers), 'The control must observe its own log');
end;


{ The page math counts one drawn line per paragraph, so the control never wraps.
  ES_AUTOHSCROLL is what keeps a rich edit from wrapping (TCustomMemo.CreateParams removes it for WordWrap). }
procedure TTestRichLog.TestWordWrap_ForcedOff;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog(makeLog);
  RichLog.WordWrap:= TRUE;   { Recreates the window }
  RichLog.HandleNeeded;

  Assert.IsTrue((Cardinal(GetWindowLong(RichLog.Handle, GWL_STYLE)) AND ES_AUTOHSCROLL) <> 0, 'The window must not wrap, whatever WordWrap says');
  RichLog.RamLog.AddInfo(StringOfChar('W', 2000));
  Assert.AreEqual(1, RichLog.Lines.Count, 'A long entry stays one line');
end;


{-------------------------------------------------------------------------------------------------------------
   Verbosity Tests
-------------------------------------------------------------------------------------------------------------}

procedure TTestRichLog.TestVerbosity_DefaultIsInfos;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  Assert.AreEqual(lvrInfos, RichLog.Verbosity, 'Default verbosity should be lvrInfos');
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


{ The line stays in the TRamLog while the slider hides it, so lowering the slider brings it back }
procedure TTestRichLog.TestVerbosity_Lowered_ShowsHiddenLine;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog(makeLog);
  RichLog.Verbosity:= lvrErrors;
  RichLog.AddInfo('Hidden info');
  Assert.AreEqual(0, RichLog.Lines.Count, 'At lvrErrors the info line is hidden');
  Assert.AreEqual(1, RichLog.RamLog.Count(FALSE, lvDebug), 'The hidden line is still in the log');

  RichLog.Verbosity:= lvrInfos;
  Assert.AreEqual(1, RichLog.Lines.Count, 'At lvrInfos the info line must be drawn');
  Assert.AreEqual('Hidden info', RichLog.Lines[0]);
end;


{ Decision B of the TRichLog rewrite: AddMsg with one parameter, AddBold and AddEmptyRow store an Info line, and the slider filters it like any other }
procedure TTestRichLog.TestVerbosity_FiltersForcedMessagesToo;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog(makeLog);
  RichLog.Verbosity:= lvrImportant;
  RichLog.AddMsg('Plain message');
  RichLog.AddBold('Bold message');
  RichLog.AddEmptyRow;
  RichLog.AddImpo('Important message');

  Assert.AreEqual(4, RichLog.RamLog.Count(FALSE, lvDebug), 'All four lines reach the log');
  Assert.AreEqual(1, RichLog.Lines.Count, 'Above lvrInfos only the important line is drawn');
  Assert.AreEqual('Important message', RichLog.Lines[0]);
end;


{ TLogVerb has no Debug level: the lowest slider position also shows debug lines (prompt file, "Step 0 findings"; C:\Projects\Biology\Baser\!DnaBaser src\RichLogBridge.pas:114-115) }
procedure TTestRichLog.TestVerbosity_LowestAlsoShowsDebug;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog(makeLog);
  RichLog.RamLog.AddDebug('Debug line');
  Assert.AreEqual(0, RichLog.Lines.Count, 'At lvrInfos a debug line is hidden');

  RichLog.Verbosity:= lvrVerbose;
  Assert.AreEqual(1, RichLog.Lines.Count, 'At lvrVerbose the debug line must be drawn');
  Assert.AreEqual('Debug line', RichLog.Lines[0]);
end;


{-------------------------------------------------------------------------------------------------------------
   Level conversion Tests
   The outside source for every entry: the mapping written in C:\Projects\LightSaber\Temp\Work package - TRichLog rewrite.md ("lvrVerbose -> lvDebug, the others one to one"; "lvDebug -> lvrVerbose colour of Verb2Color")
   and C:\Projects\Biology\Baser\!DnaBaser src\RichLogBridge.pas:114-115 (lvDebug and lvVerbose both shown as lvrVerbose).
-------------------------------------------------------------------------------------------------------------}

procedure TTestRichLog.TestVerb2Level_EveryLevel;
begin
  Assert.AreEqual(lvDebug,     Verb2Level(lvrVerbose),   'The lowest slider position filters at lvDebug');
  Assert.AreEqual(lvHints,     Verb2Level(lvrHints));
  Assert.AreEqual(lvInfos,     Verb2Level(lvrInfos));
  Assert.AreEqual(lvImportant, Verb2Level(lvrImportant));
  Assert.AreEqual(lvWarnings,  Verb2Level(lvrWarnings));
  Assert.AreEqual(lvErrors,    Verb2Level(lvrErrors));
end;


procedure TTestRichLog.TestLevel2Verb_EveryLevel;
begin
  Assert.AreEqual(lvrVerbose,   Level2Verb(lvDebug),   'A debug line is drawn as a verbose line');
  Assert.AreEqual(lvrVerbose,   Level2Verb(lvVerbose));
  Assert.AreEqual(lvrHints,     Level2Verb(lvHints));
  Assert.AreEqual(lvrInfos,     Level2Verb(lvInfos));
  Assert.AreEqual(lvrImportant, Level2Verb(lvImportant));
  Assert.AreEqual(lvrWarnings,  Level2Verb(lvWarnings));
  Assert.AreEqual(lvrErrors,    Level2Verb(lvErrors));
end;


{ TRichLog keeps its own palette (Verb2Color), not the one of TLogViewer (Verbosity2Color).
  The 12 literals are a pinned copy of the palette chosen for TRichLog (BGR values of clGray, clDkGray and clSilver from System.UITypes.pas, of clPurpleDk, clOrangeDark, clPurpleFaded and clOrange from LightCore.Colors.pas): a new colour must be changed here too. }
procedure TTestRichLog.TestLevel2Color_UsesRichLogPalette;
var
  Dark: Boolean;
begin
  { Light background }
  Assert.AreEqual($808080, Integer(Verb2Color(lvrVerbose,   FALSE)), 'Light lvrVerbose: gray');
  Assert.AreEqual($808080, Integer(Verb2Color(lvrHints,     FALSE)), 'Light lvrHints: dark gray');
  Assert.AreEqual($000000, Integer(Verb2Color(lvrInfos,     FALSE)), 'Light lvrInfos: black');
  Assert.AreEqual($5E005E, Integer(Verb2Color(lvrImportant, FALSE)), 'Light lvrImportant: dark purple');
  Assert.AreEqual($0058DF, Integer(Verb2Color(lvrWarnings,  FALSE)), 'Light lvrWarnings: dark orange');
  Assert.AreEqual($0000FF, Integer(Verb2Color(lvrErrors,    FALSE)), 'Light lvrErrors: red');

  { Dark background }
  Assert.AreEqual($C0C0C0, Integer(Verb2Color(lvrVerbose,   TRUE)), 'Dark lvrVerbose: silver');
  Assert.AreEqual($B0B0B0, Integer(Verb2Color(lvrHints,     TRUE)), 'Dark lvrHints: light gray');
  Assert.AreEqual($FFFFFF, Integer(Verb2Color(lvrInfos,     TRUE)), 'Dark lvrInfos: white');
  Assert.AreEqual($CA62FC, Integer(Verb2Color(lvrImportant, TRUE)), 'Dark lvrImportant: faded purple');
  Assert.AreEqual($0078FF, Integer(Verb2Color(lvrWarnings,  TRUE)), 'Dark lvrWarnings: orange');
  Assert.AreEqual($0000FF, Integer(Verb2Color(lvrErrors,    TRUE)), 'Dark lvrErrors: red');

  { Level2Color takes the colour of the slider level that shows the stored level }
  for Dark in [FALSE, TRUE] do
    begin
      Assert.AreEqual(Integer(Verb2Color(lvrVerbose,   Dark)), Integer(Level2Color(lvDebug,     Dark)), 'lvDebug');
      Assert.AreEqual(Integer(Verb2Color(lvrVerbose,   Dark)), Integer(Level2Color(lvVerbose,   Dark)), 'lvVerbose');
      Assert.AreEqual(Integer(Verb2Color(lvrHints,     Dark)), Integer(Level2Color(lvHints,     Dark)), 'lvHints');
      Assert.AreEqual(Integer(Verb2Color(lvrInfos,     Dark)), Integer(Level2Color(lvInfos,     Dark)), 'lvInfos');
      Assert.AreEqual(Integer(Verb2Color(lvrImportant, Dark)), Integer(Level2Color(lvImportant, Dark)), 'lvImportant');
      Assert.AreEqual(Integer(Verb2Color(lvrWarnings,  Dark)), Integer(Level2Color(lvWarnings,  Dark)), 'lvWarnings');
      Assert.AreEqual(Integer(Verb2Color(lvrErrors,    Dark)), Integer(Level2Color(lvErrors,    Dark)), 'lvErrors');
    end;
  Assert.AreEqual(Integer(clRed),   Integer(Level2Color(lvErrors, FALSE)), 'An error is red');
  Assert.AreEqual(Integer(clBlack), Integer(Level2Color(lvInfos,  FALSE)), 'An info line is black on a light background');
end;


{-------------------------------------------------------------------------------------------------------------
   Add Message Tests
-------------------------------------------------------------------------------------------------------------}

procedure TTestRichLog.TestAddMsg_AddsLineToLog;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog(makeLog);
  RichLog.AddMsg('Test message');

  Assert.AreEqual(1, RichLog.RamLog.Count(FALSE, lvDebug), 'AddMsg writes ONE entry into the TRamLog');
  Assert.AreEqual('Test message', RichLog.RamLog.Lines[0].Msg, 'AddMsg stores the text as written');
  Assert.AreEqual(lvInfos, RichLog.RamLog.Lines[0].Level, 'AddMsg with one parameter stores an Info line');
  Assert.AreEqual(1, RichLog.Lines.Count, 'The entry is drawn once');
  Assert.AreEqual('Test message', RichLog.Lines[0]);
end;


{ Each slider level goes to the Add routine of the same name, which stores the TRamLog level of that name }
procedure TTestRichLog.TestAddMsg_WithVerbosityType;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog(makeLog);
  RichLog.AddMsg('Verbose line',   lvrVerbose);
  RichLog.AddMsg('Hint line',      lvrHints);
  RichLog.AddMsg('Info line',      lvrInfos);
  RichLog.AddMsg('Important line', lvrImportant);
  RichLog.AddMsg('Warning line',   lvrWarnings);
  RichLog.AddMsg('Error line',     lvrErrors);

  Assert.AreEqual(6, RichLog.RamLog.Count(FALSE, lvDebug), 'Each call writes one entry');
  Assert.AreEqual(lvVerbose,   RichLog.RamLog.Lines[0].Level, 'lvrVerbose stores lvVerbose');
  Assert.AreEqual(lvHints,     RichLog.RamLog.Lines[1].Level, 'lvrHints stores lvHints');
  Assert.AreEqual(lvInfos,     RichLog.RamLog.Lines[2].Level, 'lvrInfos stores lvInfos');
  Assert.AreEqual(lvImportant, RichLog.RamLog.Lines[3].Level, 'lvrImportant stores lvImportant');
  Assert.AreEqual(lvWarnings,  RichLog.RamLog.Lines[4].Level, 'lvrWarnings stores lvWarnings');
  Assert.AreEqual(lvErrors,    RichLog.RamLog.Lines[5].Level, 'lvrErrors stores lvErrors');
  Assert.AreEqual('Verbose line', RichLog.RamLog.Lines[0].Msg, 'The text is stored as written');
  Assert.AreEqual('Error line',   RichLog.RamLog.Lines[5].Msg, 'The text is stored as written');
end;


procedure TTestRichLog.TestAddVerb_RespectedWhenVerbosityAllows;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog(makeLog);
  RichLog.Verbosity:= lvrVerbose;
  RichLog.AddVerb('Verbose message');

  Assert.AreEqual(lvVerbose, RichLog.RamLog.Lines[0].Level, 'AddVerb stores a Verbose line');
  Assert.AreEqual(1, RichLog.Lines.Count, 'At lvrVerbose the verbose line is drawn');
  Assert.AreEqual('Verbose message', RichLog.Lines[0]);
end;


{ lvrHints is the lowest slider level that hides a verbose line }
procedure TTestRichLog.TestAddVerb_IgnoredWhenVerbosityTooHigh;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog(makeLog);
  RichLog.Verbosity:= lvrHints;
  RichLog.AddVerb('Verbose message');

  Assert.AreEqual(0, RichLog.Lines.Count, 'At lvrHints a verbose line is not drawn');
  Assert.AreEqual(1, RichLog.RamLog.Count(FALSE, lvDebug), 'The hidden line still reaches the log');
  Assert.AreEqual(lvVerbose, RichLog.RamLog.Lines[0].Level, 'AddVerb stores a Verbose line');
end;


procedure TTestRichLog.TestAddInfo_AddsMessage;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog(makeLog);
  RichLog.AddInfo('Info message');

  Assert.AreEqual(lvInfos, RichLog.RamLog.Lines[0].Level, 'AddInfo stores an Info line');
  Assert.AreEqual(1, RichLog.Lines.Count, 'At the default lvrInfos the info line is drawn');
  Assert.AreEqual('Info message', RichLog.Lines[0]);
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
  RichLog.AddInfo('Plain message');
  RichLog.AddBold('Bold message');

  P:= RichLog.FindText('Bold message', 0, Length(RichLog.Text), []);
  Assert.IsTrue(P >= 0, 'AddBold must add the text');
  RichLog.SelStart := P;
  RichLog.SelLength:= Length('Bold message');
  Assert.IsTrue(fsBold in RichLog.SelAttributes.Style, 'AddBold must write the text in bold');

  RichLog.SelStart := 0;
  RichLog.SelLength:= Length('Plain message');
  Assert.IsFalse(fsBold in RichLog.SelAttributes.Style, 'The line before the bold one must stay plain');
end;


{ An empty entry is drawn as an empty row. It is checked between two lines: TRichEditStrings.GetCount does not count an empty LAST line. }
procedure TTestRichLog.TestAddEmptyRow_AddsBlankLine;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.Clear;
  RichLog.AddMsg('Above');
  RichLog.AddEmptyRow;
  RichLog.AddMsg('Below');

  Assert.AreEqual(3, RichLog.RamLog.Count(FALSE, lvDebug), 'AddEmptyRow is an entry of the log');
  Assert.AreEqual('', RichLog.RamLog.Lines[1].Msg, 'The entry is empty (no "CANNOT BE EMPTY" space any more)');
  Assert.AreEqual(3, RichLog.Lines.Count, 'Three entries, three rows');
  Assert.AreEqual('Above', RichLog.Lines[0]);
  Assert.AreEqual('',      RichLog.Lines[1], 'The empty entry is an empty row');
  Assert.AreEqual('Below', RichLog.Lines[2]);
end;


{ A date stamp is an Info line now (decision B), so the slider filters it }
procedure TTestRichLog.TestAddDateStamp_AddsCurrentDate;
var
  RichLog: TRichLog;
  DayBefore, DayAfter, Line: string;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.Clear;
  DayBefore:= DateToStr(Date);
  RichLog.AddDateStamp;
  DayAfter:= DateToStr(Date);      { The run may cross midnight }

  Assert.AreEqual(1, RichLog.Lines.Count, 'AddDateStamp must add exactly one line');
  Line:= RichLog.Lines[0];
  Assert.IsTrue((Line = DayBefore) OR (Line = DayAfter), 'The line must be today''s date. Got: ' + Line);
  Assert.AreEqual(lvInfos, RichLog.RamLog.Lines[0].Level, 'The date stamp is an Info line');

  RichLog.Verbosity:= lvrErrors;
  Assert.AreEqual(0, RichLog.Lines.Count, 'Above lvrInfos the date stamp is hidden');
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
  RichLog.AddInteger(-7);

  Assert.AreEqual(2, RichLog.Lines.Count, 'Each AddInteger must add one line');
  Assert.AreEqual('42', RichLog.Lines[0], 'AddInteger must log the number it was given');
  Assert.AreEqual('-7', RichLog.Lines[1], 'AddInteger must log a negative number with its sign');
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

  Assert.AreEqual(1, RichLog.Lines.Count, 'AddMsgInt must add one line');
  Assert.AreEqual('Count: 10', RichLog.Lines[0], 'AddMsgInt must log the text followed by the number');
end;


procedure TTestRichLog.TestAddFromFile_OneEntryPerLine;
var
  RichLog: TRichLog;
  FilePath: string;
begin
  RichLog:= TRichLog(makeLog);
  FilePath:= TempFilePath('Test_RichLog_AddFromFile.txt');
  TFile.WriteAllText(FilePath, 'File line 1'#13#10'File line 2'#13#10'File line 3');
  try
    RichLog.AddFromFile(FilePath, lvrWarnings);
  finally
    System.SysUtils.DeleteFile(FilePath);
  end;

  Assert.AreEqual(3, RichLog.RamLog.Count(FALSE, lvDebug), 'Each line of the file is one entry');
  Assert.AreEqual(lvWarnings, RichLog.RamLog.Lines[2].Level, 'The entries get the level given to AddFromFile');
  Assert.AreEqual(3, RichLog.Lines.Count);
  Assert.AreEqual('File line 3', RichLog.Lines[2]);
end;


{-------------------------------------------------------------------------------------------------------------
   Empty String Tests
-------------------------------------------------------------------------------------------------------------}

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
  Assert.AreEqual(0, RichLog.RamLog.Count(FALSE, lvDebug), 'AddVerb with empty string should not reach the log');
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
  Assert.AreEqual(0, RichLog.RamLog.Count(FALSE, lvDebug), 'AddInfo with empty string should not reach the log');
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
  RichLog.OnWarn:= OnWarnHandler;
  InitialCount:= RichLog.Lines.Count;

  RichLog.AddWarn('');

  Assert.AreEqual(InitialCount, RichLog.Lines.Count, 'AddWarn with empty string should not add line');
  Assert.AreEqual(0, RichLog.RamLog.Count(FALSE, lvDebug), 'AddWarn with empty string should not reach the log');
  Assert.AreEqual(0, FWarnCount, 'No line reached the log, so no OnWarn');
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
  RichLog.OnError:= OnErrorHandler;
  InitialCount:= RichLog.Lines.Count;

  RichLog.AddError('');

  Assert.AreEqual(InitialCount, RichLog.Lines.Count, 'AddError with empty string should not add line');
  Assert.AreEqual(0, RichLog.RamLog.Count(FALSE, lvDebug), 'AddError with empty string should not reach the log');
  Assert.AreEqual(0, FErrorCount, 'No line reached the log, so no OnError');
end;


{-------------------------------------------------------------------------------------------------------------
   The TRamLog behind the control
-------------------------------------------------------------------------------------------------------------}

{ A line written to the TRamLog directly, past the TRichLog methods, must be drawn }
procedure TTestRichLog.TestRamLogLine_AppearsInControl;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog(makeLog);
  RichLog.RamLog.AddInfo('From the log');

  Assert.AreEqual(1, RichLog.Lines.Count);
  Assert.AreEqual('From the log', RichLog.Lines[0]);
end;


procedure TTestRichLog.TestClear_EmptiesControlAndLog;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog(makeLog);
  RichLog.AddInfo('One');
  RichLog.AddInfo('Two');
  Assert.AreEqual(2, RichLog.Lines.Count, 'Precondition: two lines drawn');

  RichLog.Clear;
  Assert.AreEqual(0, RichLog.RamLog.Count(FALSE, lvDebug), 'Clear must empty the log');
  Assert.AreEqual(0, RichLog.Lines.Count, 'Clear must empty the control');
  Assert.AreEqual('', RichLog.Text);

  RichLog.AddInfo('Three');
  Assert.AreEqual(1, RichLog.Lines.Count, 'The control works after a Clear');
  Assert.AreEqual('Three', RichLog.Lines[0]);
end;


procedure TTestRichLog.TestMultiLineMsg_OneEntrySeveralLines;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog(makeLog);
  RichLog.AddMsg('Line A'#13#10'Line B'#13#10'Line C');

  Assert.AreEqual(1, RichLog.RamLog.Count(FALSE, lvDebug), 'A multi-line message is one entry');
  Assert.AreEqual(3, RichLog.Lines.Count, 'It is drawn on three lines');
  Assert.AreEqual('Line A', RichLog.Lines[0]);
  Assert.AreEqual('Line B', RichLog.Lines[1]);
  Assert.AreEqual('Line C', RichLog.Lines[2]);
end;


{ A lone LF, a CRLF and a lone CR are each one line break: Lines.Add would have raised EOutOfResources on the lone ones }
procedure TTestRichLog.TestBreakKinds_EachIsOneLine;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog(makeLog(200));
  RichLog.AddInfo('A'#10'B');
  RichLog.AddInfo('C'#13#10'D');
  RichLog.AddInfo('E'#13'F');

  Assert.AreEqual(3, RichLog.RamLog.Count(FALSE, lvDebug));
  Assert.AreEqual(6, RichLog.Lines.Count, 'Three entries of two lines each');
  Assert.AreEqual('A', RichLog.Lines[0]);
  Assert.AreEqual('B', RichLog.Lines[1]);
  Assert.AreEqual('C', RichLog.Lines[2]);
  Assert.AreEqual('D', RichLog.Lines[3]);
  Assert.AreEqual('E', RichLog.Lines[4]);
  Assert.AreEqual('F', RichLog.Lines[5]);
end;


{ FormUpdaterNotifier writes AddMsg(Headline + CRLF): no blank row may follow the headline }
procedure TTestRichLog.TestTrailingBreak_Dropped;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog(makeLog);
  RichLog.AddMsg('Headline'#13#10);
  RichLog.AddMsg('Body');

  Assert.AreEqual(2, RichLog.Lines.Count, 'The trailing break of an entry is not a row');
  Assert.AreEqual('Headline', RichLog.Lines[0]);
  Assert.AreEqual('Body',     RichLog.Lines[1]);
end;


{ TRamLog.LoadFromFile replaces the lines. The file has MORE lines than the log had, so only the reset counter can tell.
  The old line must go, and the loaded error must not fire OnError: it did not arrive, it was loaded. }
procedure TTestRichLog.TestReset_LoadFromFile_RedrawsWithoutEvents;
var
  RichLog: TRichLog;
  Source: TRamLog;
  FilePath: string;
begin
  FilePath:= TempFilePath('Test_RichLog_Reset.logbin');
  Source:= TRamLog.Create(FALSE, NIL);
  try
    Source.AddInfo('Loaded 1');
    Source.AddError('Loaded 2');
    Source.AddInfo('Loaded 3');
    Source.SaveToFile(FilePath);
  finally
    FreeAndNil(Source);
  end;

  RichLog:= TRichLog(makeLog);
  RichLog.OnError:= OnErrorHandler;
  RichLog.AddInfo('Before the load');
  try
    Assert.IsTrue(RichLog.RamLog.LoadFromFile(FilePath), 'Precondition: the file loads');
  finally
    System.SysUtils.DeleteFile(FilePath);
  end;

  Assert.AreEqual(3, RichLog.Lines.Count, 'Only the loaded lines are drawn');
  Assert.AreEqual('Loaded 1', RichLog.Lines[0], 'The line from before the load is gone');
  Assert.AreEqual('Loaded 3', RichLog.Lines[2]);
  Assert.AreEqual(0, FErrorCount, 'A loaded error line fires no OnError');
end;


{-------------------------------------------------------------------------------------------------------------
   Events
-------------------------------------------------------------------------------------------------------------}

{ Decision 5: OnWarn fires for every warning that reaches the log, also one the slider hides }
procedure TTestRichLog.TestOnWarn_FiresOnceForHiddenLine;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog(makeLog);
  RichLog.OnWarn:= OnWarnHandler;
  RichLog.Verbosity:= lvrErrors;

  RichLog.RamLog.AddWarn('Hidden warning');
  Assert.AreEqual(0, RichLog.Lines.Count, 'Precondition: the warning is hidden');
  Assert.AreEqual(1, FWarnCount, 'OnWarn fires once for the hidden warning');
end;


{ A redraw (here: a Verbosity change) shows the lines again, but they did not arrive again }
procedure TTestRichLog.TestOnError_NotFiredAgainOnVerbosityChange;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog(makeLog);
  RichLog.OnError:= OnErrorHandler;
  RichLog.OnWarn := OnWarnHandler;

  RichLog.RamLog.AddError('The error');
  RichLog.RamLog.AddWarn('The warning');
  Assert.AreEqual(1, FErrorCount, 'OnError fires once for the error');
  Assert.AreEqual(1, FWarnCount,  'OnWarn fires once for the warning');

  RichLog.Verbosity:= lvrErrors;
  RichLog.Verbosity:= lvrVerbose;
  RichLog.InsertTime:= TRUE;
  Assert.AreEqual(1, FErrorCount, 'A redraw fires no OnError');
  Assert.AreEqual(1, FWarnCount,  'A redraw fires no OnWarn');
  Assert.AreEqual(2, RichLog.Lines.Count, 'Precondition: both lines are drawn again');
end;


{-------------------------------------------------------------------------------------------------------------
   External log
-------------------------------------------------------------------------------------------------------------}

{ The observer lifetime contract: the destructor detaches the TRichLog (and only it) from a log it does not own }
procedure TTestRichLog.TestExternalLog_FreeingRichLog_LeavesLogWorking;
var
  RichLog, Second: TRichLog;
  ExternalLog: TRamLog;
begin
  ExternalLog:= TRamLog.Create(FALSE, NIL);
  Second:= NIL;
  try
    RichLog:= TRichLog(makeLog);
    RichLog.AssignExternalRamLog(ExternalLog);
    Assert.AreSame(ExternalLog, RichLog.RamLog, 'The control must show the external log');

    { A second view of the same log: freeing the first one must not detach it }
    Second:= TRichLog.Create(NIL);
    Second.Parent:= FTestForm;
    Second.Width:= 300;
    Second.HandleNeeded;
    Second.AssignExternalRamLog(ExternalLog);
    Assert.AreEqual(2, ObserverCount(ExternalLog), 'Precondition: both views observe the log');

    ExternalLog.AddInfo('Shared line');
    Assert.AreEqual('Shared line', RichLog.Lines[0], 'A line of the external log must be drawn');
    Assert.AreEqual('Shared line', Second.Lines[0], 'Both views draw the line');

    FRichLog:= NIL;
    FreeAndNil(RichLog);

    Assert.AreEqual(1, ObserverCount(ExternalLog), 'Only the freed control is detached');
    ExternalLog.AddInfo('After the control is gone');   { Would call Populate on freed memory if it stayed registered }
    Assert.AreEqual(2, ExternalLog.Count(FALSE, lvDebug), 'The log keeps working and keeps its lines');
    Assert.AreEqual(2, Second.Lines.Count, 'The second view still follows the log');
    Assert.AreEqual('After the control is gone', Second.Lines[1]);
  finally
    CleanupRichLog;          { Every view before the log: an external log must outlive its observers }
    FreeAndNil(Second);
    FreeAndNil(ExternalLog);
  end;
end;


{ Lines already in the log are drawn when the control attaches, but they did not arrive while it watched: no event }
procedure TTestRichLog.TestExternalLog_ExistingLinesDrawn_NoEvents;
var
  RichLog: TRichLog;
  ExternalLog: TRamLog;
begin
  ExternalLog:= TRamLog.Create(FALSE, NIL);
  try
    ExternalLog.AddError('Old error');
    ExternalLog.AddInfo('Old info');

    RichLog:= TRichLog(makeLog);
    RichLog.OnError:= OnErrorHandler;
    RichLog.AssignExternalRamLog(ExternalLog);

    Assert.AreEqual(2, RichLog.Lines.Count, 'The lines already in the log are drawn');
    Assert.AreEqual('Old error', RichLog.Lines[0]);
    Assert.AreEqual(0, FErrorCount, 'Attaching fires no OnError for old lines');

    ExternalLog.AddError('New error');
    Assert.AreEqual(1, FErrorCount, 'A new error fires OnError');

    FRichLog:= NIL;
    FreeAndNil(RichLog);    { Before the log: an external log must outlive its observers }
  finally
    FreeAndNil(ExternalLog);
  end;
end;


procedure TTestRichLog.TestAssignExternalRamLog_Nil_Raises;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog(makeLog);
  Assert.WillRaise(
    procedure
    begin
      RichLog.AssignExternalRamLog(NIL);
    end,
    Exception);
  Assert.IsNotNull(RichLog.RamLog, 'A failed assignment keeps the own log');
end;


{ The guard of the observer lifetime contract. The form stays hidden, so nothing shows on screen. }
procedure TTestRichLog.TestPopUpWindow_FormDestroying_DoesNothing;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog(makeLog);
  RichLog.FormDestroying:= TRUE;
  RichLog.PopUpWindow;
  Assert.IsFalse(FTestForm.Visible, 'PopUpWindow must not show the form while it is being destroyed');

  RichLog.FormDestroying:= FALSE;
  RichLog.RamLog.AddInfo('Populate after the flag is cleared');
  Assert.AreEqual(1, RichLog.Lines.Count, 'Precondition: the control draws again once the flag is cleared');
end;


{-------------------------------------------------------------------------------------------------------------
   InsertTime/InsertDate Tests
-------------------------------------------------------------------------------------------------------------}

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


{ The time prefix is the time stored in the entry (RLogLine.Time), not the time of the draw, and switching it on redraws the lines already shown }
procedure TTestRichLog.TestInsertTime_UsesStoredTime;
var
  RichLog: TRichLog;
  Stored: string;
begin
  RichLog:= TRichLog(makeLog);
  RichLog.AddInfo('Timed line');
  RichLog.RamLog.Lines[0].Time:= EncodeDate(2001, 2, 3) + EncodeTime(4, 5, 6, 0);
  Stored:= TimeToStr(RichLog.RamLog.Lines[0].Time);

  RichLog.InsertTime:= TRUE;
  Assert.AreEqual(Stored + '  Timed line', RichLog.Lines[0], 'The stored time must prefix the line');

  RichLog.InsertTime:= FALSE;
  Assert.AreEqual('Timed line', RichLog.Lines[0], 'Switching the prefix off redraws the line without it');
end;


{ The date prefix is the date stored in the entry (RLogLine.Time), and switching it on redraws the lines already shown }
procedure TTestRichLog.TestInsertDate_UsesStoredDate;
var
  RichLog: TRichLog;
  Stored: TDateTime;
begin
  RichLog:= TRichLog(makeLog);
  RichLog.AddInfo('Dated line');
  Stored:= EncodeDate(2001, 2, 3) + EncodeTime(4, 5, 6, 0);
  RichLog.RamLog.Lines[0].Time:= Stored;

  RichLog.InsertDate:= TRUE;
  Assert.AreEqual(DateToStr(Stored) + ' Dated line', RichLog.Lines[0], 'The stored date must prefix the line');

  RichLog.InsertTime:= TRUE;
  Assert.AreEqual(DateToStr(Stored) + ' ' + TimeToStr(Stored) + '  Dated line', RichLog.Lines[0], 'The date comes first, then the time');

  RichLog.InsertDate:= FALSE;
  RichLog.InsertTime:= FALSE;
  Assert.AreEqual('Dated line', RichLog.Lines[0], 'Switching the prefixes off redraws the line without them');
end;


{-------------------------------------------------------------------------------------------------------------
   Virtual scrolling Tests
-------------------------------------------------------------------------------------------------------------}

procedure TTestRichLog.TestAutoScroll_DefaultTrue;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  Assert.IsTrue(RichLog.AutoScroll, 'AutoScroll should be TRUE by default');
end;


{ AutoScroll FALSE: the view stays on the first page while entries arrive }
procedure TTestRichLog.TestAutoScroll_CanBeDisabled;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog(makeLog(120));
  RichLog.AutoScroll:= FALSE;
  AddEntries(RichLog, ManyEntries);

  Assert.AreEqual(0, RichLog.TopEntry, 'Without AutoScroll the view stays at the top');
  Assert.AreEqual('Entry 0', RichLog.Lines[0]);
  Assert.AreEqual(0, Pos('Entry ' + IntToStr(ManyEntries - 1), RichLog.Text), 'The last entry is not on the first page');
end;


{ Decision A: the rich edit holds only the entries that fit on screen, and none of them is out of view }
procedure TTestRichLog.TestVirtualScroll_HoldsOnePage;
var
  RichLog: TRichLog;
  Pitch, Shown, PageHeight: Integer;
begin
  RichLog:= TRichLog(makeLog(120));
  AddEntries(RichLog, ManyEntries);

  Shown:= RichLog.Lines.Count;
  Assert.IsTrue(Shown > 1, 'A 120 px control shows more than one line. Shown: ' + IntToStr(Shown));
  Assert.IsTrue(Shown < ManyEntries, 'The control must not hold all ' + IntToStr(ManyEntries) + ' entries. Shown: ' + IntToStr(Shown));

  Pitch:= MeasuredLinePitch(RichLog);
  Assert.IsTrue(Pitch > 0, 'Precondition: the rich edit reports its line positions');
  Assert.IsTrue(Shown * Pitch <= RichLog.ClientHeight, 'Every drawn line must be inside the client area. Lines: ' + IntToStr(Shown) + ', pitch: ' + IntToStr(Pitch) + ', client height: ' + IntToStr(RichLog.ClientHeight));

  { The page is full: the drawn lines fit into the visible height, and one more line would not }
  PageHeight:= VisiblePageHeight(RichLog);
  Assert.IsTrue(Shown * Pitch <= PageHeight, 'Every drawn line must be visible. Lines: ' + IntToStr(Shown) + ', pitch: ' + IntToStr(Pitch) + ', visible height: ' + IntToStr(PageHeight));
  Assert.IsTrue((Shown + 1) * Pitch > PageHeight, 'The page must be full: one more line would fit. Lines: ' + IntToStr(Shown) + ', pitch: ' + IntToStr(Pitch) + ', visible height: ' + IntToStr(PageHeight));
  Assert.AreEqual(0, Integer(SendMessage(RichLog.Handle, EM_GETFIRSTVISIBLELINE, 0, 0)), 'The rich edit itself must not be scrolled');
end;


procedure TTestRichLog.TestVirtualScroll_TopEntryDrawsFirstEntries;
var
  RichLog: TRichLog;
  Shown: Integer;
begin
  RichLog:= TRichLog(makeLog(120));
  AddEntries(RichLog, ManyEntries);
  Shown:= RichLog.Lines.Count;

  RichLog.TopEntry:= 0;
  Assert.AreEqual('Entry 0', RichLog.Lines[0], 'Scrolled to the top, the first entry is drawn first');
  Assert.AreEqual('Entry ' + IntToStr(Shown - 1), LastLine(RichLog), 'The page holds the next entries in order');
  Assert.AreEqual(0, ChildScrollBar(RichLog).Position, 'The scroll bar follows');

  RichLog.TopEntry:= 50;
  Assert.AreEqual('Entry 50', RichLog.Lines[0]);
  Assert.AreEqual(50, ChildScrollBar(RichLog).Position);

  RichLog.TopEntry:= MaxInt;   { Past the end: clamped to the last page }
  Assert.AreEqual('Entry ' + IntToStr(ManyEntries - 1), LastLine(RichLog), 'The last page ends with the last entry');
  Assert.AreEqual(Shown, RichLog.Lines.Count, 'The last page is a full page');
end;


procedure TTestRichLog.TestAutoScroll_KeepsLastEntryOnScreen;
var
  RichLog: TRichLog;
  i, Pitch: Integer;
  LastPos: TPoint;
begin
  RichLog:= TRichLog(makeLog(120));
  for i:= 0 to ManyEntries - 1 do
    begin
      RichLog.RamLog.AddInfo('Entry ' + IntToStr(i));
      if (i = 5) OR (i = 50) OR (i = ManyEntries - 1)
      then Assert.AreEqual('Entry ' + IntToStr(i), LastLine(RichLog), 'The newest entry must be the last drawn line');
    end;

  { On screen: the page holds only the last entries, and the last drawn line ends inside the client area }
  Pitch:= MeasuredLinePitch(RichLog);
  Assert.IsTrue(Pitch > 0, 'Precondition: the rich edit reports its line positions');
  Assert.IsTrue(RichLog.Lines.Count * Pitch <= RichLog.ClientHeight, 'Every drawn line must be inside the client area. Lines: ' + IntToStr(RichLog.Lines.Count));
  SendMessage(RichLog.Handle, EM_POSFROMCHAR, WPARAM(@LastPos), SendMessage(RichLog.Handle, EM_LINEINDEX, RichLog.Lines.Count - 1, 0));
  Assert.IsTrue(LastPos.Y + Pitch <= RichLog.ClientHeight, 'The last entry must be on screen. Its top: ' + IntToStr(LastPos.Y) + ', client height: ' + IntToStr(RichLog.ClientHeight));
  Assert.AreEqual(ManyEntries - RichLog.Lines.Count, RichLog.TopEntry, 'The page ends with the last entry: its top entry is the total minus the drawn entries');

  Assert.AreEqual(ChildScrollBar(RichLog).Max, ChildScrollBar(RichLog).Position, 'The scroll bar is at the bottom');
  Assert.AreEqual(ChildScrollBar(RichLog).Position, RichLog.TopEntry);
end;


{ Append + trim must delete whole entries, counting a paragraph break as one character.
  Entry i has (i mod 3) + 1 lines, named 'E<i>.<line>'. After the run the drawn lines must be whole, consecutive entries that end with the last one. }
procedure TTestRichLog.TestAppendAndTrim_WholeEntriesOnly;
CONST
  Total = 60;
var
  RichLog: TRichLog;
  i, j, Row, Entry, LineNo, Pitch, PageHeight: Integer;
  Msg: string;
begin
  RichLog:= TRichLog(makeLog(120));
  for i:= 0 to Total - 1 do
    begin
      Msg:= '';
      for j:= 0 to i mod 3 do
        begin
          if j > 0 then Msg:= Msg + #10;
          Msg:= Msg + 'E' + IntToStr(i) + '.' + IntToStr(j);
        end;
      RichLog.RamLog.AddInfo(Msg);
    end;

  Assert.IsTrue(RichLog.Lines.Count > 3, 'The page holds more than the last entry. Drawn lines: ' + IntToStr(RichLog.Lines.Count));

  { Walk the drawn lines backward from the last line of the last entry }
  Entry := Total - 1;
  LineNo:= Entry mod 3;
  for Row:= RichLog.Lines.Count - 1 downto 0 do
    begin
      Assert.AreEqual('E' + IntToStr(Entry) + '.' + IntToStr(LineNo), RichLog.Lines[Row], 'Row ' + IntToStr(Row));
      Dec(LineNo);
      if LineNo < 0 then
        begin
          Dec(Entry);
          LineNo:= Entry mod 3;
        end;
    end;
  Assert.AreEqual(Entry mod 3, LineNo, 'The first drawn row must be the first line of an entry: no entry is cut at the top');
  Assert.IsTrue(Entry >= 0, 'Precondition: the first entries were trimmed off the page');

  { The page is full: Entry, the one above the first drawn row, has (Entry mod 3) + 1 lines and would not fit }
  Pitch:= MeasuredLinePitch(RichLog);
  Assert.IsTrue(Pitch > 0, 'Precondition: the rich edit reports its line positions');
  PageHeight:= VisiblePageHeight(RichLog);
  Assert.IsTrue(RichLog.Lines.Count * Pitch <= PageHeight, 'Every drawn line must be visible');
  Assert.IsTrue((RichLog.Lines.Count + (Entry mod 3) + 1) * Pitch > PageHeight, 'Too many entries were trimmed: E' + IntToStr(Entry) + ' would still fit above the page');
end;


{ An entry exactly one page tall (Cap screen lines) arrives at the end: every entry above it is trimmed, and it is drawn whole.
  The next entry trims it in turn. The append path must give the page that a full redraw of the last page gives. }
procedure TTestRichLog.TestAppend_EntryOfCapLines_DrawnWhole;
var
  RichLog: TRichLog;
  Cap, i, Pitch: Integer;
  Msg: string;
begin
  RichLog:= TRichLog(makeLog(120));
  AddEntries(RichLog, ManyEntries);
  Cap:= RichLog.Lines.Count;   { One-line entries at the end of the log: the page holds Cap of them }
  Pitch:= MeasuredLinePitch(RichLog);
  Assert.IsTrue(Cap > 2, 'Precondition: the page holds several lines. Cap: ' + IntToStr(Cap));
  Assert.IsTrue((Cap + 1) * Pitch > VisiblePageHeight(RichLog), 'Precondition: Cap lines fill the page');

  Msg:= '';
  for i:= 0 to Cap - 1 do
    begin
      if i > 0 then Msg:= Msg + #13#10;
      Msg:= Msg + 'Tall.' + IntToStr(i);
    end;
  RichLog.RamLog.AddInfo(Msg);

  Assert.AreEqual(Cap, RichLog.Lines.Count, 'The page shows the tall entry, and only it');
  for i:= 0 to Cap - 1 do
    Assert.AreEqual('Tall.' + IntToStr(i), RichLog.Lines[i], 'Row ' + IntToStr(i));
  Assert.AreEqual(ManyEntries, RichLog.TopEntry, 'The tall entry is the top entry of the page');
  Assert.AreEqual(ManyEntries, ChildScrollBar(RichLog).Position, 'The scroll bar follows');
  Assert.AreEqual(ChildScrollBar(RichLog).Max, ChildScrollBar(RichLog).Position, 'The scroll bar is at the bottom');

  RichLog.RamLog.AddInfo('After the tall entry');
  Assert.AreEqual(1, RichLog.Lines.Count, 'The tall entry and the new one do not fit together: the tall one is trimmed');
  Assert.AreEqual('After the tall entry', RichLog.Lines[0]);
  Assert.AreEqual(ManyEntries + 1, RichLog.TopEntry);

  RichLog.TopEntry:= MaxInt;   { Draws the last page again, from the log }
  Assert.AreEqual(1, RichLog.Lines.Count, 'A full redraw of the last page gives the same page');
  Assert.AreEqual('After the tall entry', RichLog.Lines[0]);
  Assert.AreEqual(ManyEntries + 1, RichLog.TopEntry);
end;


{ A page of one screen line: each new entry replaces the one on screen; a taller entry is cut to its first line }
procedure TTestRichLog.TestAppend_PageOfOneLine_ShowsNewestEntry;
var
  RichLog: TRichLog;
  i, Pitch: Integer;
begin
  RichLog:= TRichLog(makeLog(120));
  AddEntries(RichLog, 2);
  Pitch:= MeasuredLinePitch(RichLog);
  Assert.IsTrue(Pitch > 0, 'Precondition: the rich edit reports its line positions');

  RichLog.Height:= RichLog.Height - RichLog.ClientHeight + Pitch + Pitch div 2;   { A client area one and a half lines high }
  Assert.AreEqual(1, RichLog.Lines.Count, 'Precondition: the page holds one screen line. Client height: ' + IntToStr(RichLog.ClientHeight) + ', pitch: ' + IntToStr(Pitch));
  Assert.AreEqual('Entry 1', RichLog.Lines[0], 'The page ends with the last entry');

  for i:= 2 to 9 do
    begin
      RichLog.RamLog.AddInfo('Entry ' + IntToStr(i));
      Assert.AreEqual(1, RichLog.Lines.Count, 'One line on a one-line page, after entry ' + IntToStr(i));
      Assert.AreEqual('Entry ' + IntToStr(i), RichLog.Lines[0], 'The newest entry is on screen');
      Assert.AreEqual(i, RichLog.TopEntry);
    end;
  Assert.AreEqual(9, ChildScrollBar(RichLog).Position, 'The scroll bar follows');
  Assert.AreEqual(ChildScrollBar(RichLog).Max, ChildScrollBar(RichLog).Position, 'The scroll bar is at the bottom');

  RichLog.RamLog.AddInfo('Two'#13#10'lines');
  Assert.AreEqual(1, RichLog.Lines.Count, 'An entry taller than the page is cut at the bottom of the page');
  Assert.AreEqual('Two', RichLog.Lines[0]);
  Assert.AreEqual(10, RichLog.TopEntry);
end;


{ Page Down and Page Up move the TRamLog position, not the rich edit text.
  One page step is the page less one entry: the last entry of the old page stays on screen as the first of the new one. }
procedure TTestRichLog.TestKeyboard_PageDownPageUp;
var
  RichLog: TRichLog;
  Shown, AfterPgDn: Integer;
begin
  RichLog:= TRichLog(makeLog(120));
  RichLog.AutoScroll:= FALSE;
  AddEntries(RichLog, ManyEntries);
  Shown:= RichLog.Lines.Count;   { One-line entries: the page holds Shown of them }
  Assert.IsTrue(Shown > 2, 'Precondition: the page shows several entries. Shown: ' + IntToStr(Shown));

  RichLog.Perform(WM_KEYDOWN, VK_NEXT, 0);
  AfterPgDn:= RichLog.TopEntry;
  Assert.AreEqual(Shown - 1, AfterPgDn, 'Page Down moves by the page less one entry');
  Assert.AreEqual('Entry ' + IntToStr(AfterPgDn), RichLog.Lines[0], 'The page starts with the new top entry');

  RichLog.Perform(WM_KEYDOWN, VK_NEXT, 0);
  Assert.AreEqual(2 * (Shown - 1), RichLog.TopEntry, 'A second Page Down moves by the same step');

  RichLog.Perform(WM_KEYDOWN, VK_PRIOR, 0);
  Assert.AreEqual(Shown - 1, RichLog.TopEntry, 'Page Up moves back by the same step');

  RichLog.Perform(WM_KEYDOWN, VK_PRIOR, 0);
  Assert.AreEqual(0, RichLog.TopEntry, 'Page Up goes back to the top');
  Assert.AreEqual('Entry 0', RichLog.Lines[0]);
end;


{ One wheel notch moves the view by the Windows setting "lines per notch" (Mouse.WheelScrollLines), counted in entries.
  A setting of 0 or less, or of a page or more, moves by the page less one entry, as Page Down does. }
procedure TTestRichLog.TestMouseWheel_ScrollsEntries;
var
  RichLog: TRichLog;
  Shown, Notch, AfterDown: Integer;
begin
  RichLog:= TRichLog(makeLog(120));
  RichLog.AutoScroll:= FALSE;
  AddEntries(RichLog, ManyEntries);
  Shown:= RichLog.Lines.Count;   { One-line entries: the page holds Shown of them }
  Assert.IsTrue(Shown > 2, 'Precondition: the page shows several entries. Shown: ' + IntToStr(Shown));

  Notch:= Mouse.WheelScrollLines;
  if (Notch <= 0) OR (Notch >= Shown)
  then Notch:= Shown - 1;

  Assert.IsTrue(TRichLogAccess(RichLog).DoMouseWheelDown([], Point(1, 1)), 'The wheel is handled');
  AfterDown:= RichLog.TopEntry;
  Assert.AreEqual(Notch, AfterDown, 'One notch down. Windows setting: ' + IntToStr(Mouse.WheelScrollLines) + ' lines, page: ' + IntToStr(Shown));
  Assert.AreEqual('Entry ' + IntToStr(AfterDown), RichLog.Lines[0]);

  Assert.IsTrue(TRichLogAccess(RichLog).DoMouseWheelDown([], Point(1, 1)), 'The wheel is handled');
  Assert.AreEqual(2 * Notch, RichLog.TopEntry, 'A second notch moves by the same step');

  Assert.IsTrue(TRichLogAccess(RichLog).DoMouseWheelUp([], Point(1, 1)), 'The wheel is handled');
  Assert.AreEqual(Notch, RichLog.TopEntry, 'One notch up moves back by the same step');

  Assert.IsTrue(TRichLogAccess(RichLog).DoMouseWheelUp([], Point(1, 1)), 'The wheel is handled');
  Assert.AreEqual(0, RichLog.TopEntry, 'Wheel up by the same amount goes back to the top');
end;


procedure TTestRichLog.TestScrollBar_VisibleOnlyWhenNeeded;
var
  RichLog: TRichLog;
  Bar: TScrollBar;
  FormatRect: TRect;
begin
  RichLog:= TRichLog(makeLog(120));
  RichLog.AddInfo('One line');
  Bar:= ChildScrollBar(RichLog);
  Assert.IsFalse(Bar.Visible, 'One entry fits: no scroll bar');

  AddEntries(RichLog, ManyEntries);
  Assert.IsTrue(Bar.Visible, 'More entries than fit: the scroll bar shows');
  Assert.AreEqual(Ord(alRight), Ord(Bar.Align));

  SendMessage(RichLog.Handle, EM_GETRECT, 0, LPARAM(@FormatRect));
  Assert.IsTrue(FormatRect.Right <= RichLog.ClientWidth - Bar.Width, 'The text must not run under the scroll bar. Format rect right: ' + IntToStr(FormatRect.Right) + ', ClientWidth: ' + IntToStr(RichLog.ClientWidth) + ', bar width: ' + IntToStr(Bar.Width));
end;


{ A Padding moves the child bar inside the client area: the align pass places it in the rectangle that AdjustClientRect gives (c:\Delphi\Delphi 13\source\vcl\Vcl.Controls.pas, TWinControl.AlignControls).
  WndProc puts the bar back after a message only when it is out of that place, so a message must not start an align pass. }
procedure TTestRichLog.TestScrollBar_Padding_NoAlignPerMessage;
var
  RichLog: TRichLogAlignCounter;
  Bar: TScrollBar;
  Before: Integer;
begin
  RichLog:= TRichLogAlignCounter.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;
  RichLog.Height:= 120;
  RichLog.Width:= 300;
  RichLog.Padding.Top:= 5;
  RichLog.Padding.Right:= 7;
  RichLog.HandleNeeded;
  AddEntries(RichLog, ManyEntries);

  Bar:= ChildScrollBar(RichLog);
  Assert.IsTrue(Bar.Visible, 'Precondition: more entries than fit, so the scroll bar shows');
  Assert.AreEqual(5, Bar.Top, 'The bar starts below the top padding');
  Assert.AreEqual(RichLog.ClientWidth - 7, Bar.Left + Bar.Width, 'The bar ends at the right padding');

  Before:= RichLog.AlignCount;
  RichLog.Perform(WM_NULL, 0, 0);
  RichLog.Perform(WM_NULL, 0, 0);
  Assert.AreEqual(Before, RichLog.AlignCount, 'A message must not start an align pass while the bar is in its place');
end;


{ A right Padding moves the child bar inwards: the text must still stop left of the bar }
procedure TTestRichLog.TestScrollBar_PaddingRight_TextNotUnderBar;
var
  RichLog: TRichLog;
  Bar: TScrollBar;
  FormatRect: TRect;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;
  RichLog.Height:= 120;
  RichLog.Width:= 300;
  RichLog.Padding.Right:= 20;
  RichLog.HandleNeeded;
  AddEntries(RichLog, ManyEntries);

  Bar:= ChildScrollBar(RichLog);
  Assert.IsTrue(Bar.Visible, 'Precondition: more entries than fit, so the scroll bar shows');
  Assert.AreEqual(RichLog.ClientWidth - 20, Bar.Left + Bar.Width, 'Precondition: the padding moves the bar inwards');

  SendMessage(RichLog.Handle, EM_GETRECT, 0, LPARAM(@FormatRect));
  Assert.IsTrue(FormatRect.Right <= Bar.Left, 'The text must not run under the scroll bar. Format rect right: ' + IntToStr(FormatRect.Right) + ', bar left: ' + IntToStr(Bar.Left));
end;


{ One line of a news body, much wider than the 652 px log of FormUpdaterNotifier.dfm }
function NewsLine(Index: Integer): string;
begin
  Result:= 'News line ' + IntToStr(Index) + ': ' + StringOfChar('w', 200);
end;


{ FormUpdaterNotifier.PopulateNews writes AddMsg(Headline + CRLF), then the news body one entry per line (AddNewsBody), because TRichLog cuts an entry taller than its page.
  The control has the size, font and WordWrap of the log in FormUpdaterNotifier.dfm; the body has 40 lines, each wider than the control.
  Every line must be reachable by scrolling, whole and on one screen line, with no time prefix and no blank row after the headline. }
procedure TTestRichLog.TestNewsBody_OneEntryPerLine_AllReachable;
const
  BodyLines = 40;
  LinePrefix = 'News line ';
var
  RichLog: TRichLog;
  Seen: array[1..BodyLines] of Boolean;
  Top, i, k, LineNo: Integer;
  Line: string;
  Bitmap: Vcl.Graphics.TBitmap;      { Winapi.Windows declares a TBitmap record too }
begin
  RichLog:= TRichLog(makeLog(209));
  RichLog.Width:= 652;
  RichLog.Font.Name:= 'Arial';
  RichLog.Font.Height:= -12;
  RichLog.WordWrap:= FALSE;

  Bitmap:= Vcl.Graphics.TBitmap.Create;
  try
    Bitmap.Canvas.Font:= RichLog.Font;
    Assert.IsTrue(Bitmap.Canvas.TextWidth(NewsLine(1)) > RichLog.ClientWidth, 'A body line must be wider than the control');
  finally
    FreeAndNil(Bitmap);
  end;

  RichLog.AddMsg('Headline'#13#10);
  for i:= 1 to BodyLines do
    RichLog.AddMsg(NewsLine(i));

  Assert.IsTrue(ChildScrollBar(RichLog).Visible, '41 entries do not fit on one page: the scroll bar shows');
  Assert.IsTrue(GetWindowLong(RichLog.Handle, GWL_STYLE) AND WS_HSCROLL <> 0, 'A horizontal scroll bar reaches the end of a long line');

  RichLog.TopEntry:= 0;
  Assert.AreEqual('Headline', RichLog.Lines[0], 'No time prefix');
  Assert.AreEqual(NewsLine(1), RichLog.Lines[1], 'No blank row after the headline');

  { Scroll through every page; each drawn body line must be whole }
  for i:= 1 to BodyLines do
    Seen[i]:= FALSE;
  for Top:= 0 to BodyLines do
    begin
      RichLog.TopEntry:= Top;
      for k:= 0 to RichLog.Lines.Count - 1 do
        begin
          Line:= RichLog.Lines[k];
          if Line = 'Headline' then CONTINUE;
          Assert.AreEqual(1, Pos(LinePrefix, Line), 'Only the headline and the body lines are drawn. Line: ' + Line);
          LineNo:= StrToInt(Copy(Line, Length(LinePrefix) + 1, Pos(':', Line) - Length(LinePrefix) - 1));
          Assert.AreEqual(NewsLine(LineNo), Line, 'A body line is drawn whole, on one screen line');
          Seen[LineNo]:= TRUE;
        end;
    end;

  for i:= 1 to BodyLines do
    Assert.IsTrue(Seen[i], 'Body line ' + IntToStr(i) + ' must be reachable by scrolling');

  RichLog.TopEntry:= MaxInt;
  Assert.AreEqual(NewsLine(BodyLines), LastLine(RichLog), 'The last page ends with the last body line');
end;


{ The old PopulateNews wrote the whole news body as ONE entry.
  An entry taller than the page is cut at the bottom of the page (header of LightVcl.Visual.RichLog.pas, section "Virtual scrolling"), so its last line is on no page. }
procedure TTestRichLog.TestTallEntry_CutAtPageBottom;
const
  BodyLines = 40;
var
  RichLog: TRichLog;
  Body: string;
  i: Integer;
begin
  RichLog:= TRichLog(makeLog(209));
  RichLog.Width:= 652;
  Body:= '';
  for i:= 1 to BodyLines do
    Body:= Body + NewsLine(i) + sLineBreak;
  RichLog.AddMsg(Body);

  Assert.IsTrue(RichLog.Lines.Count < BodyLines, 'The page is shorter than the entry. Drawn lines: ' + IntToStr(RichLog.Lines.Count));
  Assert.AreEqual(NewsLine(1), RichLog.Lines[0], 'The entry is drawn from its first line');
  Assert.IsFalse(ChildScrollBar(RichLog).Visible, 'One entry: there is nothing to scroll to');

  RichLog.TopEntry:= MaxInt;
  Assert.AreEqual(0, Pos(NewsLine(BodyLines), RichLog.Text), 'The last line of the entry is on no page');
end;


{-------------------------------------------------------------------------------------------------------------
   DFM Tests
   The DFM loader writes Lines.Strings straight into the rich edit (past the log), then calls Loaded.
   ReadComponent on an existing TRichLog runs the same path: csLoading, the properties, then Loaded (System.Classes.pas, TReader.ReadRootComponent).
-------------------------------------------------------------------------------------------------------------}

procedure LoadFromDfmText(RichLog: TRichLog; CONST DfmText: string);
var
  TextStream: TStringStream;
  BinStream: TMemoryStream;
begin
  TextStream:= TStringStream.Create(DfmText);
  BinStream:= TMemoryStream.Create;
  try
    ObjectTextToBinary(TextStream, BinStream);
    BinStream.Position:= 0;
    BinStream.ReadComponent(RichLog);
  finally
    FreeAndNil(BinStream);
    FreeAndNil(TextStream);
  end;
end;


procedure TTestRichLog.TestDfmLines_RealTextBecomesEntry;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog(makeLog);
  LoadFromDfmText(RichLog,
    'object RichLogDfmReal: TRichLog'      + sLineBreak +
    '  Lines.Strings = ('                  + sLineBreak +
    '    ''Real text'''                    + sLineBreak +
    '    ''Second line'')'                 + sLineBreak +
    'end');

  Assert.AreEqual(1, RichLog.RamLog.Count(FALSE, lvDebug), 'The DFM text becomes ONE entry of the own log');
  Assert.AreEqual('Real text'#13#10'Second line', RichLog.RamLog.Lines[0].Msg);
  Assert.AreEqual(2, RichLog.Lines.Count, 'It is drawn on two lines');
  Assert.AreEqual('Real text', RichLog.Lines[0]);

  RichLog.AddInfo('Next');
  Assert.AreEqual('Next', LastLine(RichLog), 'The control keeps working after the DFM load');
end;


{ The three placeholders of the product header (Log, Log:, RichLog), in other letter case, and with spaces around }
procedure TTestRichLog.TestDfmLines_PlaceholderIgnored;
CONST
  Placeholders: array[0..4] of string = ('Log', 'Log:', 'RichLog', 'log', ' Log ');
var
  RichLog: TRichLog;
  Placeholder: string;
begin
  for Placeholder in Placeholders do
    begin
      RichLog:= TRichLog.Create(FTestForm);
      try
        RichLog.Parent:= FTestForm;
        RichLog.Width:= 300;
        RichLog.HandleNeeded;
        LoadFromDfmText(RichLog,
          'object RichLogDfmPlaceholder: TRichLog' + sLineBreak +
          '  Lines.Strings = ('                    + sLineBreak +
          '    ''' + Placeholder + ''')'           + sLineBreak +
          'end');

        Assert.AreEqual(0, RichLog.RamLog.Count(FALSE, lvDebug), 'The placeholder "' + Placeholder + '" is not an entry');
        Assert.AreEqual('', RichLog.Text, 'The placeholder "' + Placeholder + '" is not drawn');
      finally
        FreeAndNil(RichLog);
      end;
    end;
end;


{-------------------------------------------------------------------------------------------------------------
   Copy Tests
-------------------------------------------------------------------------------------------------------------}

procedure TTestRichLog.TestCopyAll_NoException;
var
  RichLog: TRichLog;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.Clear;
  RichLog.AddMsg('Test content');
  RichLog.Verbosity:= lvrErrors;   { CopyAll reads the log, not the screen: a hidden line is copied too }
  Clipboard.Clear;

  RichLog.CopyAll;
  Assert.AreEqual('Test content', Trim(Clipboard.AsText), 'CopyAll must put the whole log on the clipboard');
end;


{-------------------------------------------------------------------------------------------------------------
   SaveAsRtf Tests
-------------------------------------------------------------------------------------------------------------}

procedure TTestRichLog.TestSaveAsRtf_NoException;
var
  RichLog: TRichLog;
  TempFile, Content: string;
begin
  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  RichLog.AddMsg('Test content');
  TempFile:= TempFilePath('test_richlog.rtf');

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


{ The file holds the whole log (also the lines the slider hides), with the line breaks inside an entry kept }
procedure TTestRichLog.TestSaveAsRtf_MultiLineEntries;
var
  RichLog: TRichLog;
  Reader: TRichEdit;
  TempFile: string;
begin
  RichLog:= TRichLog(makeLog);
  RichLog.AddInfo('A'#10'B');
  RichLog.AddVerb('Hidden verbose');
  RichLog.AddWarn('C'#13#10'D');
  TempFile:= TempFilePath('Test_RichLog_SaveAsRtf.rtf');

  Reader:= TRichEdit.Create(NIL);
  try
    RichLog.SaveAsRtf(TempFile);
    Reader.Visible:= FALSE;
    Reader.Parent:= FTestForm;
    Reader.Lines.LoadFromFile(TempFile);

    Assert.AreEqual(5, Reader.Lines.Count, 'Three entries on five lines');
    Assert.AreEqual('A', Reader.Lines[0]);
    Assert.AreEqual('B', Reader.Lines[1]);
    Assert.AreEqual('Hidden verbose', Reader.Lines[2], 'A line the slider hides is saved too');
    Assert.AreEqual('C', Reader.Lines[3]);
    Assert.AreEqual('D', Reader.Lines[4]);
  finally
    FreeAndNil(Reader);
    if FileExists(TempFile)
    then System.SysUtils.DeleteFile(TempFile);
  end;
end;


{ A log bigger than 64K characters must be saved whole: the temporary rich edit gets the same text limit as TRichLog }
procedure TTestRichLog.TestSaveAsRtf_BigLog_NotCut;
CONST
  Total = 3000;   { 3000 entries of about 45 characters: about 135,000 characters }
var
  RichLog: TRichLog;
  TempFile, Content: string;
  i: Integer;
begin
  RichLog:= TRichLog(makeLog);
  for i:= 0 to Total - 1 do
    RichLog.RamLog.AddInfo('Big log entry number ' + IntToStr(i) + ' of the test.');
  TempFile:= TempFilePath('Test_RichLog_BigLog.rtf');
  try
    RichLog.SaveAsRtf(TempFile);
    Content:= TFile.ReadAllText(TempFile);
    Assert.IsTrue(Pos('Big log entry number 0 of', Content) > 0, 'The first entry is in the file');
    Assert.IsTrue(Pos('Big log entry number ' + IntToStr(Total - 1) + ' of', Content) > 0, 'The last entry is in the file: the text was not cut');
  finally
    if FileExists(TempFile)
    then System.SysUtils.DeleteFile(TempFile);
  end;
end;


{-------------------------------------------------------------------------------------------------------------
   AppendToFile / LoadFromFile Tests
-------------------------------------------------------------------------------------------------------------}

procedure TTestRichLog.TestAppendToFile_WritesTheLog;
var
  RichLog: TRichLog;
  TempFile, Content: string;
begin
  RichLog:= TRichLog(makeLog);
  RichLog.AddInfo('Appended 1');
  RichLog.AddVerb('Appended hidden');
  TempFile:= TempFilePath('Test_RichLog_Append.txt');
  TFile.WriteAllText(TempFile, 'Already there'#13#10);
  try
    RichLog.AppendToFile(TempFile);
    Content:= TFile.ReadAllText(TempFile);
    { The old content stays; the whole log follows, also the line the slider hides; the file ends with a line break, so the next append starts on a new line }
    Assert.AreEqual('Already there'#13#10'Appended 1'#13#10'Appended hidden'#13#10, Content, 'The log is appended whole, one entry per line');
  finally
    System.SysUtils.DeleteFile(TempFile);
  end;
end;


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


{ Each text line of the file becomes one Info entry, and the lines already in the log are replaced }
procedure TTestRichLog.TestLoadFromFile_TextLinesBecomeEntries;
var
  RichLog: TRichLog;
  TempFile: string;
begin
  RichLog:= TRichLog(makeLog);
  RichLog.AddWarn('Before the load');
  TempFile:= TempFilePath('Test_RichLog_Load.txt');
  TFile.WriteAllText(TempFile, 'Loaded A'#13#10'Loaded B'#13#10'Loaded C');
  try
    Assert.IsTrue(RichLog.LoadFromFile(TempFile), 'The file exists');
  finally
    System.SysUtils.DeleteFile(TempFile);
  end;

  Assert.AreEqual(3, RichLog.RamLog.Count(FALSE, lvDebug), 'One entry per line, the old entry replaced');
  Assert.AreEqual(lvInfos, RichLog.RamLog.Lines[1].Level, 'A loaded line is an Info entry');
  Assert.AreEqual(3, RichLog.Lines.Count);
  Assert.AreEqual('Loaded A', RichLog.Lines[0]);
  Assert.AreEqual('Loaded C', RichLog.Lines[2]);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestRichLog);

end.
