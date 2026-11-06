unit Test.LightVcl.Visual.RichLogTrack;

{=============================================================================================================
   Unit tests for LightVcl.Visual.RichLogTrack.pas
   Tests TRichLogTrckbr - the verbosity trackbar controller for TRichLog.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  [TestFixture]
  TTestRichLogTrack = class
  private
    FTestForm: TForm;
    FTrackbar: TObject;
    FRichLog: TObject;
    procedure Cleanup;
    procedure OnVerbChangedHandler(Sender: TObject);
  private
    FVerbChangedCount: Integer;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Creation Tests }
    [Test]
    procedure TestCreate_HasTrackBar;

    [Test]
    procedure TestCreate_HasLabel;

    [Test]
    procedure TestCreate_DefaultDimensions;

    { TrackBar Tests }
    [Test]
    procedure TestTrackBar_MinIsZero;

    [Test]
    procedure TestTrackBar_MaxIsErrorsOrdinal;

    [Test]
    procedure TestTrackBar_DefaultPositionIsInfos;

    { Verbosity Property Tests }
    [Test]
    procedure TestVerbosity_ReturnsTrackbarPosition;

    [Test]
    procedure TestVerbosity_CanSetAllLevels;

    { Log Assignment Tests }
    [Test]
    procedure TestLog_CanAssignRichLog;

    [Test]
    procedure TestLog_CanAssignNil;

    [Test]
    procedure TestLog_SynchronizesVerbosity;

    [Test]
    procedure TestLog_FreedLog_LogBecomesNil;

    [Test]
    procedure TestLog_AttachedBeforeWindow_KeepsInfos;

    [Test]
    procedure TestRecreateWnd_KeepsLevel;

    [Test]
    procedure TestTrackBar_Move_RedrawsLogLines;

    { OnVerbChanged Event Tests }
    [Test]
    procedure TestOnVerbChanged_CanBeAssigned;
  end;

implementation

uses
  LightCore.AppData,
  LightVcl.Visual.AppData,
  LightVcl.Visual.RichLog,
  LightVcl.Visual.RichLogTrack,
  LightVcl.Visual.RichLogUtils;


TYPE
  { Counts the windows created for the panel }
  TRichLogTrckbrCounter = class(TRichLogTrckbr)
  protected
    procedure CreateWnd; override;
  public
    WndCount: Integer;
  end;


procedure TRichLogTrckbrCounter.CreateWnd;
begin
  Inc(WndCount);
  inherited CreateWnd;
end;


procedure TTestRichLogTrack.Setup;
begin
  Assert.IsNotNull(AppData, 'AppData must be initialized before running tests');
  FTrackbar:= NIL;
  FRichLog:= NIL;
  FVerbChangedCount:= 0;
  FTestForm:= TForm.Create(NIL);
end;


procedure TTestRichLogTrack.TearDown;
begin
  Cleanup;
  FreeAndNil(FTestForm);
end;


procedure TTestRichLogTrack.Cleanup;
var
  Trackbar: TRichLogTrckbr;
  RichLog: TRichLog;
begin
  if FRichLog <> NIL then
    begin
      RichLog:= TRichLog(FRichLog);
      FreeAndNil(RichLog);
      FRichLog:= NIL;
    end;

  if FTrackbar <> NIL then
    begin
      Trackbar:= TRichLogTrckbr(FTrackbar);
      FreeAndNil(Trackbar);
      FTrackbar:= NIL;
    end;
end;


procedure TTestRichLogTrack.OnVerbChangedHandler(Sender: TObject);
begin
  Inc(FVerbChangedCount);
end;


{ The label is a private field; it is the only TLabel on the panel }
function VerbosityLabel(Trackbar: TWinControl): TLabel;
var
  i: Integer;
begin
  Result:= NIL;
  for i:= 0 to Trackbar.ControlCount - 1 do
    if Trackbar.Controls[i] is TLabel
    then EXIT(TLabel(Trackbar.Controls[i]));
  Assert.IsNotNull(Result, 'The panel must hold a TLabel');
end;


{ Creation Tests }

procedure TTestRichLogTrack.TestCreate_HasTrackBar;
var
  Trackbar: TRichLogTrckbr;
begin
  Trackbar:= TRichLogTrckbr.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;
  Assert.IsTrue(Trackbar.Handle <> 0, 'The control needs a window handle: CreateWnd is what fills in Min, Max and Position');

  Assert.IsNotNull(Trackbar.TrackBar, 'Should have internal TrackBar component');
  Assert.AreSame(TObject(Trackbar), TObject(Trackbar.TrackBar.Parent), 'The trackbar must sit on the panel');
  Assert.AreSame(TObject(Trackbar), TObject(Trackbar.TrackBar.Owner), 'The panel must own (and free) the trackbar');
  Assert.AreEqual('VerbosityTrackbar', Trackbar.TrackBar.Name, 'The trackbar needs its name so its position can be saved to the INI file');
  Assert.AreEqual(Ord(alRight), Ord(Trackbar.TrackBar.Align), 'Trackbar alignment');

  { Moving the inner trackbar itself must reach the label: its OnChange is wired to the panel }
  Trackbar.TrackBar.Position:= Ord(lvrErrors);
  Assert.AreEqual('Log verbosity: Errors', VerbosityLabel(Trackbar).Caption, 'The label must follow the trackbar');
end;


procedure TTestRichLogTrack.TestCreate_HasLabel;
var
  Trackbar: TRichLogTrckbr;
  HasLabel: Boolean;
  i: Integer;
begin
  Trackbar:= TRichLogTrckbr.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;
  Assert.IsTrue(Trackbar.Handle <> 0, 'The control needs a window handle: CreateWnd is what fills in Min, Max and Position');

  HasLabel:= FALSE;
  for i:= 0 to Trackbar.ControlCount - 1 do
    if Trackbar.Controls[i] is TLabel then
      begin
        HasLabel:= TRUE;
        Break;
      end;

  Assert.IsTrue(HasLabel, 'Should have a TLabel component');
  Assert.AreEqual('Log verbosity: Info', VerbosityLabel(Trackbar).Caption, 'Before the slider is ever moved, the label shows the starting level');
end;


procedure TTestRichLogTrack.TestCreate_DefaultDimensions;
var
  Trackbar: TRichLogTrckbr;
begin
  Trackbar:= TRichLogTrckbr.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;
  Assert.IsTrue(Trackbar.Handle <> 0, 'The control needs a window handle: CreateWnd is what fills in Min, Max and Position');

  { CreateWnd sets the size on the first handle creation }
  Assert.AreEqual(260, Trackbar.Width,  'Default Width set by CreateWnd');
  Assert.AreEqual(27,  Trackbar.Height, 'Default Height set by CreateWnd');
end;


{ TrackBar Tests }

{ TrackBar is a published subcomponent, so a DFM can store another Min and Max: CreateWnd sets them back }
procedure TTestRichLogTrack.TestTrackBar_MinIsZero;
var
  Trackbar: TRichLogTrckbr;
begin
  Trackbar:= TRichLogTrckbr.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;
  Trackbar.TrackBar.Min:= 3;
  Trackbar.TrackBar.Max:= 8;
  Assert.AreEqual(3, Trackbar.TrackBar.Min, 'Precondition: Min is not 0 before the window exists');
  Assert.IsTrue(Trackbar.Handle <> 0, 'The control needs a window handle: CreateWnd is what fills in Min, Max and Position');

  Assert.AreEqual(0, Trackbar.TrackBar.Min, 'TrackBar.Min should be 0');
end;


procedure TTestRichLogTrack.TestTrackBar_MaxIsErrorsOrdinal;
var
  Trackbar: TRichLogTrckbr;
begin
  Trackbar:= TRichLogTrckbr.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;
  Assert.IsTrue(Trackbar.Handle <> 0, 'The control needs a window handle: CreateWnd is what fills in Min, Max and Position');

  Assert.AreEqual(Ord(lvrErrors), Trackbar.TrackBar.Max, 'TrackBar.Max should be Ord(lvrErrors)');
end;


procedure TTestRichLogTrack.TestTrackBar_DefaultPositionIsInfos;
var
  Trackbar: TRichLogTrckbr;
begin
  Trackbar:= TRichLogTrckbr.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;
  Assert.IsTrue(Trackbar.Handle <> 0, 'The control needs a window handle: CreateWnd is what fills in Min, Max and Position');

  Assert.AreEqual(Ord(lvrInfos), Trackbar.TrackBar.Position, 'TrackBar.Position should default to lvrInfos');
end;


{ Verbosity Property Tests }

procedure TTestRichLogTrack.TestVerbosity_ReturnsTrackbarPosition;
var
  Trackbar: TRichLogTrckbr;
begin
  Trackbar:= TRichLogTrckbr.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;
  Assert.IsTrue(Trackbar.Handle <> 0, 'The control needs a window handle: CreateWnd is what fills in Min, Max and Position');

  { Move the inner trackbar directly, away from DefaultVerbosity (lvrInfos), and read Verbosity back }
  Trackbar.TrackBar.Position:= Ord(lvrErrors);
  Assert.AreEqual(lvrErrors, Trackbar.Verbosity, 'Verbosity must follow TrackBar.Position (lvrErrors)');

  Trackbar.TrackBar.Position:= Ord(lvrHints);
  Assert.AreEqual(lvrHints, Trackbar.Verbosity, 'Verbosity must follow TrackBar.Position (lvrHints)');
end;


procedure TTestRichLogTrack.TestVerbosity_CanSetAllLevels;
var
  Trackbar: TRichLogTrckbr;
begin
  Trackbar:= TRichLogTrckbr.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;
  Assert.IsTrue(Trackbar.Handle <> 0, 'The control needs a window handle: CreateWnd is what fills in Min, Max and Position');

  Trackbar.Verbosity:= lvrVerbose;
  Assert.AreEqual(lvrVerbose, Trackbar.Verbosity, 'Should be able to set lvrVerbose');

  Trackbar.Verbosity:= lvrHints;
  Assert.AreEqual(lvrHints, Trackbar.Verbosity, 'Should be able to set lvrHints');

  Trackbar.Verbosity:= lvrErrors;
  Assert.AreEqual(lvrErrors, Trackbar.Verbosity, 'Should be able to set lvrErrors');
end;


{ Log Assignment Tests }

procedure TTestRichLogTrack.TestLog_CanAssignRichLog;
var
  Trackbar: TRichLogTrckbr;
  RichLog: TRichLog;
begin
  Trackbar:= TRichLogTrckbr.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;
  Assert.IsTrue(Trackbar.Handle <> 0, 'The control needs a window handle: CreateWnd is what fills in Min, Max and Position');

  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  Trackbar.Log:= RichLog;

  Assert.AreEqual(TComponent(RichLog), TComponent(Trackbar.Log), 'Should be able to assign RichLog');
end;


procedure TTestRichLogTrack.TestLog_CanAssignNil;
var
  Trackbar: TRichLogTrckbr;
  RichLog: TRichLog;
begin
  Trackbar:= TRichLogTrckbr.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;
  Assert.IsTrue(Trackbar.Handle <> 0, 'The control needs a window handle: CreateWnd is what fills in Min, Max and Position');

  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  { Attach, then detach: the detached log must no longer follow the trackbar }
  Trackbar.Log:= RichLog;
  Trackbar.Log:= NIL;
  Assert.IsNull(Trackbar.Log, 'Log must be NIL after assigning NIL');

  Trackbar.Verbosity:= lvrErrors;
  Assert.AreEqual(lvrErrors, Trackbar.Verbosity, 'The trackbar must still work without a log');
  Assert.AreEqual(lvrInfos, RichLog.Verbosity, 'A detached log must keep its verbosity');
end;


procedure TTestRichLogTrack.TestLog_SynchronizesVerbosity;
var
  Trackbar: TRichLogTrckbr;
  RichLog: TRichLog;
begin
  Trackbar:= TRichLogTrckbr.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;
  Assert.IsTrue(Trackbar.Handle <> 0, 'The control needs a window handle: CreateWnd is what fills in Min, Max and Position');

  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  Trackbar.Verbosity:= lvrWarnings;
  Trackbar.Log:= RichLog;

  Assert.AreEqual(lvrWarnings, RichLog.Verbosity, 'Assigning Log should synchronize verbosity');
end;


{ The slider must forget a log that is freed while attached, or its next move writes to freed memory.
  The log has no owner: so only a free notification can tell the slider (an owner passes opRemove to every component it owns, System.Classes.pas, TComponent.Notification). }
procedure TTestRichLogTrack.TestLog_FreedLog_LogBecomesNil;
var
  Trackbar: TRichLogTrckbr;
  RichLog: TRichLog;
begin
  Trackbar:= TRichLogTrckbr.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;
  Assert.IsTrue(Trackbar.Handle <> 0, 'The control needs a window handle: CreateWnd is what fills in Min, Max and Position');

  RichLog:= TRichLog.Create(NIL);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;
  Trackbar.Log:= RichLog;
  Assert.IsNotNull(Trackbar.Log, 'Precondition: the log is attached');

  FRichLog:= NIL;
  FreeAndNil(RichLog);
  Assert.IsNull(Trackbar.Log, 'Freeing the attached log must set the Log of the slider to NIL');

  Trackbar.Verbosity:= lvrErrors;   { Would write to the freed log if the slider still pointed at it }
  Assert.AreEqual('Log verbosity: Errors', VerbosityLabel(Trackbar).Caption, 'The slider keeps working without its log');
end;


{ The DFM loader, and a form built in code, can attach the log before the slider has a window.
  Once the window exists, the log must show the level the slider shows. }
procedure TTestRichLogTrack.TestLog_AttachedBeforeWindow_KeepsInfos;
var
  Trackbar: TRichLogTrckbr;
  RichLog: TRichLog;
begin
  Trackbar:= TRichLogTrckbr.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;

  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  Assert.IsFalse(Trackbar.HandleAllocated, 'Precondition: the slider has no window yet');
  Trackbar.Log:= RichLog;
  Assert.IsTrue(Trackbar.Handle <> 0, 'The control needs a window handle: CreateWnd is what fills in Min, Max and Position');

  Assert.AreEqual(lvrInfos, Trackbar.Verbosity, 'The slider starts at lvrInfos');
  Assert.AreEqual(lvrInfos, RichLog.Verbosity, 'The log must show the level of the slider');
end;


{ The VCL destroys and creates the window again (RecreateWnd) when, for example, a window style changes.
  The new window must keep the level the user chose, in the slider, the label and the log, and it is no change the user made. }
procedure TTestRichLogTrack.TestRecreateWnd_KeepsLevel;
var
  Trackbar: TRichLogTrckbrCounter;
  RichLog: TRichLog;
begin
  Trackbar:= TRichLogTrckbrCounter.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;
  Assert.IsTrue(Trackbar.Handle <> 0, 'The control needs a window handle: CreateWnd is what fills in Min, Max and Position');

  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;
  Trackbar.Log:= RichLog;
  Trackbar.OnVerbChanged:= OnVerbChangedHandler;

  Trackbar.Verbosity:= lvrErrors;
  Assert.AreEqual(lvrErrors, RichLog.Verbosity, 'Precondition: the log follows the slider');
  Assert.AreEqual(1, FVerbChangedCount, 'Precondition: the user changed the level once');

  Trackbar.RecreateWnd;
  Assert.IsTrue(Trackbar.Handle <> 0, 'The window is created again');   { The hidden form does not create it by itself }
  Assert.AreEqual(2, Trackbar.WndCount, 'Precondition: CreateWnd ran a second time');

  Assert.AreEqual(lvrErrors, Trackbar.Verbosity, 'The slider keeps the level the user chose');
  Assert.AreEqual(lvrErrors, RichLog.Verbosity, 'The log keeps the level the user chose');
  Assert.AreEqual('Log verbosity: Errors', VerbosityLabel(Trackbar).Caption, 'The label keeps the level the user chose');
  Assert.AreEqual(1, FVerbChangedCount, 'A new window is no change the user made: OnVerbChanged must not fire');
end;


{ The slider filters the lines the log already holds: moving it redraws them (the TRichLog view of a TRamLog) }
procedure TTestRichLogTrack.TestTrackBar_Move_RedrawsLogLines;
var
  Trackbar: TRichLogTrckbr;
  RichLog: TRichLog;
begin
  Trackbar:= TRichLogTrckbr.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;
  Assert.IsTrue(Trackbar.Handle <> 0, 'The control needs a window handle: CreateWnd is what fills in Min, Max and Position');

  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;
  RichLog.HandleNeeded;
  Trackbar.Log:= RichLog;

  RichLog.AddInfo('Info line');
  RichLog.AddError('Error line');
  Assert.AreEqual(2, RichLog.Lines.Count, 'At the default position both lines are drawn');

  Trackbar.TrackBar.Position:= Ord(lvrErrors);
  Assert.AreEqual(1, RichLog.Lines.Count, 'At lvrErrors only the error line is drawn');
  Assert.AreEqual('Error line', RichLog.Lines[0]);

  Trackbar.TrackBar.Position:= Ord(lvrVerbose);
  Assert.AreEqual(2, RichLog.Lines.Count, 'Moving the slider down brings the info line back');
  Assert.AreEqual('Info line', RichLog.Lines[0]);
end;


{ OnVerbChanged Event Tests }

procedure TTestRichLogTrack.TestOnVerbChanged_CanBeAssigned;
var
  Trackbar: TRichLogTrckbr;
begin
  Trackbar:= TRichLogTrckbr.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;
  Assert.IsTrue(Trackbar.Handle <> 0, 'The control needs a window handle: CreateWnd is what fills in Min, Max and Position');

  Trackbar.OnVerbChanged:= OnVerbChangedHandler;
  Trackbar.Verbosity:= lvrErrors;   { Default is lvrInfos, so the position really changes }

  Assert.AreEqual(1, FVerbChangedCount, 'Changing the verbosity must fire OnVerbChanged once');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestRichLogTrack);

end.
