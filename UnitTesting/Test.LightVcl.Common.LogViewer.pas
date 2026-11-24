unit Test.LightVcl.Common.LogViewer;

{=============================================================================================================
   Unit tests for LightVcl.Common.LogViewer
   Tests log viewer component functionality, filtering, and clipboard operations
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.Graphics,
  LightCore.LogTypes,
  LightCore.LogRam;

type
  [TestFixture]
  TTestLogViewer = class
  private
    FTestForm: TForm;
    FRunFolder: string;
    procedure checkSavedRtf(CONST MultiLineMsg: string);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor Tests }
    [Test]
    procedure TestCreate_InitializesRamLog;

    [Test]
    procedure TestCreate_DefaultProperties;

    { AssignExternalRamLog Tests }
    [Test]
    procedure TestAssignExternalRamLog_NilParameter_ShouldRaise;

    [Test]
    procedure TestAssignExternalRamLog_ValidLog_Assigns;

    [Test]
    procedure TestAssignExternalRamLog_SecondViewerOnSameLog;

    { SaveAsRtf Tests }
    [Test]
    procedure TestSaveAsRtf_EmptyPath_ShouldRaise;

    [Test]
    procedure TestSaveAsRtf_LoneLF_SavedAsTwoLines;

    [Test]
    procedure TestSaveAsRtf_CRLF_SavedAsTwoLines;

    { Verbosity2Color Tests }
    [Test]
    procedure TestVerbosity2Color_Debug;

    [Test]
    procedure TestVerbosity2Color_Verbose;

    [Test]
    procedure TestVerbosity2Color_Errors;

    [Test]
    procedure TestVerbosity2Color_AllLevels;

    { Count Tests }
    [Test]
    procedure TestCount_EmptyLog;

    [Test]
    procedure TestCount_WithEntries;

    { Verbosity Filter Tests }
    [Test]
    procedure TestVerbosityFilter_DefaultValue;

    { Follow Tests }
    [Test]
    procedure TestFollow_AtBottom_ShowsTheNewestLine;
    [Test]
    procedure TestFollow_ScrolledUp_StaysOnTheSameLines;
    [Test]
    procedure TestFollow_BackAtTheBottom_FollowsAgain;
    [Test]
    procedure TestFollow_RingDropWhileScrolledUp_KeepsTheSameTopLine;
    [Test]
    procedure TestFollow_AutoScrollOff_NeverFollows;

    { Refresh timer + time format Tests }
    [Test]
    procedure TestRefreshInterval_DefaultOff_RedrawsAtOnce;
    [Test]
    procedure TestRefreshInterval_On_RedrawsOnTheTimer;
  end;

implementation

uses
  System.IOUtils,
  Winapi.Windows,
  Vcl.ComCtrls, Vcl.Controls, Vcl.StdCtrls,
  LightVcl.Common.LogViewer,
  Test.RunFolder;

CONST
  RowHeight = 22;    { TLogViewer.CreateWnd sets DefaultRowHeight to 22 }
  PageRows  = 5;     { The test viewer shows 5 data rows under the header }
  RunPrefix = 'TestVclLogViewer';


{ A viewer with a window, 5 data rows high, on Form }
function NewViewer(Form: TForm): TLogViewer;
begin
  Result:= TLogViewer.Create(Form);
  Result.Parent:= Form;
  Result.Height:= RowHeight * (PageRows + 1);
  Result.HandleNeeded;
end;


{ The viewer's own scroll bar. Setting its Position is what a user's drag does: it fires OnChange. }
function ScrollBarOf(Viewer: TLogViewer): TScrollBar;
VAR i: Integer;
begin
  for i:= 0 to Viewer.ControlCount - 1 do
    if Viewer.Controls[i] is TScrollBar
    then EXIT(TScrollBar(Viewer.Controls[i]));
  Result:= NIL;
  Assert.Fail('The viewer has no scroll bar');
end;


procedure AddLines(Viewer: TLogViewer; First, Last: Integer);
VAR i: Integer;
begin
  for i:= First to Last do
    Viewer.RamLog.AddInfo('L' + IntToStr(i));
end;


{ The first visible message }
function TopLine(Viewer: TLogViewer): string;
VAR Text: string;
begin
  Text:= Viewer.VisibleText;
  if Pos(sLineBreak, Text) > 0
  then Result:= Copy(Text, 1, Pos(sLineBreak, Text) - 1)
  else Result:= Text;
end;

{ FRunFolder is <temp>\TestVclLogViewer_<GUID>\ (Test.RunFolder): the SaveAsRtf tests write there; TearDown deletes it }
procedure TTestLogViewer.Setup;
begin
  FRunFolder:= NewRunFolder(RunPrefix);
  FTestForm:= TForm.Create(NIL);
  FTestForm.Width:= 400;
  FTestForm.Height:= 300;
end;

procedure TTestLogViewer.TearDown;
begin
  FreeAndNil(FTestForm);
  DeleteRunFolder(FRunFolder, RunPrefix);
  FRunFolder:= '';
end;

{ Constructor Tests }

procedure TTestLogViewer.TestCreate_InitializesRamLog;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    Assert.IsNotNull(LogViewer.RamLog, 'RamLog should be initialized');
  finally
    FreeAndNil(LogViewer);
  end;
end;

procedure TTestLogViewer.TestCreate_DefaultProperties;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    Assert.IsFalse(LogViewer.ShowTime, 'ShowTime should default to False');
    Assert.IsFalse(LogViewer.ShowDate, 'ShowDate should default to False');
    Assert.IsTrue(LogViewer.AutoScroll, 'AutoScroll should default to True');
    Assert.AreEqual(lvVerbose, LogViewer.Verbosity, 'Verbosity should default to lvVerbose');
    Assert.AreEqual('hh:nn', LogViewer.TimeFormat, 'TimeFormat should default to hh:nn, as before the option existed');
    Assert.AreEqual(0, LogViewer.RefreshInterval, 'The redraw timer should be off by default');
  finally
    FreeAndNil(LogViewer);
  end;
end;

{ AssignExternalRamLog Tests }

procedure TTestLogViewer.TestAssignExternalRamLog_NilParameter_ShouldRaise;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    Assert.WillRaise(
      procedure
      begin
        LogViewer.AssignExternalRamLog(NIL);
      end,
      Exception);
  finally
    FreeAndNil(LogViewer);
  end;
end;

procedure TTestLogViewer.TestAssignExternalRamLog_ValidLog_Assigns;
var
  LogViewer: TLogViewer;
  ExternalLog: TRamLog;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  ExternalLog:= TRamLog.Create(FALSE, NIL);
  try
    LogViewer.Parent:= FTestForm;
    ExternalLog.AddInfo('Test message');

    LogViewer.AssignExternalRamLog(ExternalLog);

    Assert.AreSame(ExternalLog, LogViewer.RamLog, 'RamLog should be assigned to external log');
  finally
    FreeAndNil(LogViewer);
    FreeAndNil(ExternalLog);
  end;
end;


{ The case of TAppData.CreateMainForm: the hidden log window already shows AppData.RamLog, then a viewer on the main form attaches to the same log.
  Then the first viewer is freed: its destructor must detach only itself. If it stayed attached, the AddInfo below would call Populate on the freed viewer. }
procedure TTestLogViewer.TestAssignExternalRamLog_SecondViewerOnSameLog;
var
  First, Second: TLogViewer;
  ExternalLog: TRamLog;
begin
  ExternalLog:= TRamLog.Create(FALSE, NIL);   { ShowOnError FALSE: no window may pop up during a test run }
  First := TLogViewer.Create(FTestForm);
  Second:= TLogViewer.Create(FTestForm);
  try
    First.Parent := FTestForm;
    Second.Parent:= FTestForm;

    First.AssignExternalRamLog(ExternalLog);
    Second.AssignExternalRamLog(ExternalLog);
    Assert.AreSame(ExternalLog, First.RamLog,  'The first viewer must still show the shared log');
    Assert.AreSame(ExternalLog, Second.RamLog, 'The second viewer must show the shared log');

    FreeAndNil(First);
    ExternalLog.AddInfo('After the first viewer is gone');
    Assert.AreEqual(1, Second.Count, 'The second viewer must still read the shared log');
  finally
    FreeAndNil(Second);
    FreeAndNil(First);
    FreeAndNil(ExternalLog);
  end;
end;

{ SaveAsRtf Tests }

procedure TTestLogViewer.TestSaveAsRtf_EmptyPath_ShouldRaise;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    Assert.WillRaise(
      procedure
      begin
        LogViewer.SaveAsRtf('');
      end,
      Exception);
  finally
    FreeAndNil(LogViewer);
  end;
end;


{ Saves a log of three entries, the middle one MultiLineMsg ('Line A' + a break + 'Line B'), then reads the RTF file back into a rich edit.
  The file must hold four paragraphs in order: the break inside the entry is kept, and no line is lost or doubled. }
procedure TTestLogViewer.checkSavedRtf(CONST MultiLineMsg: string);
var
  LogViewer: TLogViewer;
  Reader: TRichEdit;
  TempFile: string;
begin
  TempFile:= FRunFolder + 'Test_LogViewer_SaveAsRtf.rtf';
  LogViewer:= TLogViewer.Create(FTestForm);
  Reader:= TRichEdit.Create(NIL);
  try
    LogViewer.Parent:= FTestForm;
    LogViewer.RamLog.AddInfo('First');
    LogViewer.RamLog.AddInfo(MultiLineMsg);
    LogViewer.RamLog.AddImpo('Last');   { Not AddError: the log of a TLogViewer has ShowOnError TRUE, and an error would show the test form }
    Assert.AreEqual(3, LogViewer.RamLog.Count(FALSE, lvDebug), 'Precondition: the multi-line message is one entry');

    LogViewer.SaveAsRtf(TempFile);
    Assert.IsTrue(FileExists(TempFile), 'SaveAsRtf must write the file');
    Assert.IsTrue(TFile.ReadAllText(TempFile).StartsWith('{\rtf'), 'The file must be RTF');

    Reader.Visible:= FALSE;
    Reader.Parent:= FTestForm;
    Reader.PlainText:= FALSE;
    Reader.Lines.LoadFromFile(TempFile);

    Assert.AreEqual(4, Reader.Lines.Count, 'Three entries, one of them on two lines, are four lines');
    Assert.AreEqual('First',  Reader.Lines[0]);
    Assert.AreEqual('Line A', Reader.Lines[1]);
    Assert.AreEqual('Line B', Reader.Lines[2]);
    Assert.AreEqual('Last',   Reader.Lines[3]);
  finally
    FreeAndNil(Reader);
    FreeAndNil(LogViewer);
  end;
end;


procedure TTestLogViewer.TestSaveAsRtf_LoneLF_SavedAsTwoLines;
begin
  checkSavedRtf('Line A'#10'Line B');
end;


procedure TTestLogViewer.TestSaveAsRtf_CRLF_SavedAsTwoLines;
begin
  checkSavedRtf('Line A'#13#10'Line B');
end;

{ Verbosity2Color Tests }

procedure TTestLogViewer.TestVerbosity2Color_Debug;
begin
  Assert.AreEqual(TColor($909090), Verbosity2Color(lvDebug), 'Debug should be light gray');
end;

procedure TTestLogViewer.TestVerbosity2Color_Verbose;
begin
  Assert.AreEqual(TColor($808080), Verbosity2Color(lvVerbose), 'Verbose should be silver');
end;

procedure TTestLogViewer.TestVerbosity2Color_Errors;
begin
  Assert.AreEqual(clRed, Verbosity2Color(lvErrors), 'Errors should be red');
end;

procedure TTestLogViewer.TestVerbosity2Color_AllLevels;
VAR
  i, j: Integer;
begin
  { A color of 0 is clBlack, which lvInfos legitimately uses in light mode, so "not zero" proves
    nothing. What the mapping must guarantee is that every level gets its OWN color.
    The loops run over Integer, not over TLogVerbLvl: Succ() on the last enumeration member is a
    range check error in a build that has range checking on. }
  for i:= Ord(Low(TLogVerbLvl)) to Ord(High(TLogVerbLvl)) do
    for j:= i+1 to Ord(High(TLogVerbLvl)) do
      begin
        Assert.AreNotEqual(Integer(Verbosity2Color(TLogVerbLvl(i), FALSE)), Integer(Verbosity2Color(TLogVerbLvl(j), FALSE)), 'Light mode: two levels share one color');
        Assert.AreNotEqual(Integer(Verbosity2Color(TLogVerbLvl(i), TRUE )), Integer(Verbosity2Color(TLogVerbLvl(j), TRUE )), 'Dark mode: two levels share one color');
      end;

  { Anchor the three levels a user actually recognises by color }
  Assert.AreEqual(Integer(clBlack), Integer(Verbosity2Color(lvInfos , FALSE)), 'lvInfos should be black in light mode');
  Assert.AreEqual(Integer(clWhite), Integer(Verbosity2Color(lvInfos , TRUE )), 'lvInfos should be white in dark mode');
  Assert.AreEqual(Integer(clRed)  , Integer(Verbosity2Color(lvErrors, FALSE)), 'lvErrors should be red');
end;

{ Count Tests }

procedure TTestLogViewer.TestCount_EmptyLog;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    Assert.AreEqual(0, LogViewer.Count, 'Count should be 0 for empty log');
  finally
    FreeAndNil(LogViewer);
  end;
end;

procedure TTestLogViewer.TestCount_WithEntries;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;

    LogViewer.RamLog.AddDebug('Debug 1');
    LogViewer.RamLog.AddInfo('Message 1');
    LogViewer.RamLog.AddInfo('Message 2');
    LogViewer.RamLog.AddError('Error 1');

    { Count is the number of lines at or above Verbosity (lvVerbose by default), so the debug line is not counted }
    Assert.AreEqual(3, LogViewer.Count, 'Two infos and one error are at or above lvVerbose; the debug line is not');

    LogViewer.Verbosity:= lvErrors;
    Assert.AreEqual(1, LogViewer.Count, 'Only the error line is at or above lvErrors');
  finally
    FreeAndNil(LogViewer);
  end;
end;

{ Verbosity Filter Tests }

procedure TTestLogViewer.TestVerbosityFilter_DefaultValue;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    Assert.AreEqual(lvVerbose, LogViewer.Verbosity, 'Default verbosity should be lvVerbose');
  finally
    FreeAndNil(LogViewer);
  end;
end;


{ Follow Tests }

procedure TTestLogViewer.TestFollow_AtBottom_ShowsTheNewestLine;
var Viewer: TLogViewer;
begin
  Viewer:= NewViewer(FTestForm);
  try
    AddLines(Viewer, 1, 50);
    Assert.IsTrue(Viewer.Following, 'A new viewer follows the newest line');
    Assert.AreEqual(50, Viewer.LineCount);
    Assert.AreEqual(50 - PageRows, Viewer.TopRow);
    Assert.AreEqual('L46' + sLineBreak + 'L47' + sLineBreak + 'L48' + sLineBreak + 'L49' + sLineBreak + 'L50', Viewer.VisibleText);
  finally
    FreeAndNil(Viewer);
  end;
end;


procedure TTestLogViewer.TestFollow_ScrolledUp_StaysOnTheSameLines;
var Viewer: TLogViewer;
begin
  Viewer:= NewViewer(FTestForm);
  try
    AddLines(Viewer, 1, 50);
    ScrollBarOf(Viewer).Position:= 10;     { The user scrolls up }
    Assert.IsFalse(Viewer.Following, 'A user who scrolled up must not be followed');
    Assert.AreEqual('L11', TopLine(Viewer));

    AddLines(Viewer, 51, 80);
    Assert.AreEqual(10, Viewer.TopRow, 'New lines must not move the view of a user who scrolled up');
    Assert.AreEqual('L11', TopLine(Viewer));
    Assert.AreEqual(80, Viewer.LineCount);
  finally
    FreeAndNil(Viewer);
  end;
end;


procedure TTestLogViewer.TestFollow_BackAtTheBottom_FollowsAgain;
var
  Viewer: TLogViewer;
  Bar: TScrollBar;
begin
  Viewer:= NewViewer(FTestForm);
  try
    AddLines(Viewer, 1, 50);
    Bar:= ScrollBarOf(Viewer);
    Bar.Position:= 10;
    Bar.Position:= Bar.Max;                { The user scrolls back to the bottom }
    Assert.IsTrue(Viewer.Following, 'At the bottom the view must follow again');

    AddLines(Viewer, 51, 51);
    Assert.AreEqual('L47', TopLine(Viewer), 'The newest line must be in view');
    Assert.IsTrue(Viewer.VisibleText.EndsWith('L51'));
  finally
    FreeAndNil(Viewer);
  end;
end;


{ The ring drops old lines above the user's view: the view must move back by the dropped lines, so the same line stays on top }
procedure TTestLogViewer.TestFollow_RingDropWhileScrolledUp_KeepsTheSameTopLine;
var Viewer: TLogViewer;
begin
  Viewer:= NewViewer(FTestForm);
  try
    Viewer.RamLog.RingSize:= 100;
    AddLines(Viewer, 1, 100);
    ScrollBarOf(Viewer).Position:= 20;
    Assert.AreEqual('L21', TopLine(Viewer));

    AddLines(Viewer, 101, 101);            { 101 > 100: the ring drops 1 + 10 lines }
    Assert.AreEqual(90, Viewer.LineCount, 'Precondition: the ring dropped 11 lines');
    Assert.AreEqual(9, Viewer.TopRow, 'TopRow must move back by the 11 dropped lines');
    Assert.AreEqual('L21', TopLine(Viewer), 'The same line must stay at the top of the view');
    Assert.IsFalse(Viewer.Following);
  finally
    FreeAndNil(Viewer);
  end;
end;


procedure TTestLogViewer.TestFollow_AutoScrollOff_NeverFollows;
var Viewer: TLogViewer;
begin
  Viewer:= NewViewer(FTestForm);
  try
    Viewer.AutoScroll:= FALSE;
    AddLines(Viewer, 1, 50);
    Assert.IsFalse(Viewer.Following);
    Assert.AreEqual(0, Viewer.TopRow, 'With AutoScroll off the view stays at the top');
    Assert.AreEqual('L1', TopLine(Viewer));
  finally
    FreeAndNil(Viewer);
  end;
end;


{ Refresh timer + time format Tests }

procedure TTestLogViewer.TestRefreshInterval_DefaultOff_RedrawsAtOnce;
var Viewer: TLogViewer;
begin
  Viewer:= NewViewer(FTestForm);
  try
    Assert.AreEqual(0, Viewer.RefreshInterval);
    AddLines(Viewer, 1, 1);
    Assert.AreEqual('L1', Viewer.VisibleText, 'Without the timer a new line must be in view at once');
  finally
    FreeAndNil(Viewer);
  end;
end;


procedure TTestLogViewer.TestRefreshInterval_On_RedrawsOnTheTimer;
var
  Viewer: TLogViewer;
  Start: UInt64;
begin
  Viewer:= NewViewer(FTestForm);
  try
    AddLines(Viewer, 1, 1);
    Viewer.RefreshInterval:= 50;
    AddLines(Viewer, 2, 2);
    Assert.AreEqual('L1', Viewer.VisibleText, 'With the timer on, a new line must wait for the timer');

    Start:= GetTickCount64;
    while (Viewer.LineCount < 2) AND (GetTickCount64 - Start < 3000) do
      begin
        Application.ProcessMessages;
        Sleep(5);
      end;
    Assert.AreEqual('L1' + sLineBreak + 'L2', Viewer.VisibleText, 'The timer must redraw the view');

    AddLines(Viewer, 3, 3);
    Viewer.RefreshInterval:= 0;            { Switching the timer off redraws what is waiting }
    Assert.IsTrue(Viewer.VisibleText.EndsWith('L3'));
  finally
    FreeAndNil(Viewer);
  end;
end;



initialization
  TDUnitX.RegisterTestFixture(TTestLogViewer);

end.
