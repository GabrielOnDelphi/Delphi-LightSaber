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
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Creation Tests }
    [Test]
    procedure TestCreate_Succeeds;

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


procedure TTestRichLogTrack.Setup;
begin
  Assert.IsNotNull(AppData, 'AppData must be initialized before running tests');
  FTrackbar:= NIL;
  FRichLog:= NIL;
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
  { Handler for OnVerbChanged event testing }
end;


{ Creation Tests }

procedure TTestRichLogTrack.TestCreate_Succeeds;
var
  Trackbar: TRichLogTrckbr;
begin
  Trackbar:= TRichLogTrckbr.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;

  Assert.IsNotNull(Trackbar, 'TRichLogTrckbr creation should succeed');
end;


procedure TTestRichLogTrack.TestCreate_HasTrackBar;
var
  Trackbar: TRichLogTrckbr;
begin
  Trackbar:= TRichLogTrckbr.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;

  Assert.IsNotNull(Trackbar.TrackBar, 'Should have internal TrackBar component');
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

  HasLabel:= FALSE;
  for i:= 0 to Trackbar.ControlCount - 1 do
    if Trackbar.Controls[i] is TLabel then
      begin
        HasLabel:= TRUE;
        Break;
      end;

  Assert.IsTrue(HasLabel, 'Should have a TLabel component');
end;


procedure TTestRichLogTrack.TestCreate_DefaultDimensions;
var
  Trackbar: TRichLogTrckbr;
begin
  Trackbar:= TRichLogTrckbr.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;

  Assert.IsTrue(Trackbar.Width > 0, 'Width should be positive');
  Assert.IsTrue(Trackbar.Height > 0, 'Height should be positive');
end;


{ TrackBar Tests }

procedure TTestRichLogTrack.TestTrackBar_MinIsZero;
var
  Trackbar: TRichLogTrckbr;
begin
  Trackbar:= TRichLogTrckbr.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;

  Assert.AreEqual(0, Trackbar.TrackBar.Min, 'TrackBar.Min should be 0');
end;


procedure TTestRichLogTrack.TestTrackBar_MaxIsErrorsOrdinal;
var
  Trackbar: TRichLogTrckbr;
begin
  Trackbar:= TRichLogTrckbr.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;

  Assert.AreEqual(Ord(lvrErrors), Trackbar.TrackBar.Max, 'TrackBar.Max should be Ord(lvrErrors)');
end;


procedure TTestRichLogTrack.TestTrackBar_DefaultPositionIsInfos;
var
  Trackbar: TRichLogTrckbr;
begin
  Trackbar:= TRichLogTrckbr.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;

  Assert.AreEqual(Ord(DefaultVerbosity), Trackbar.TrackBar.Position, 'TrackBar.Position should default to DefaultVerbosity');
end;


{ Verbosity Property Tests }

procedure TTestRichLogTrack.TestVerbosity_ReturnsTrackbarPosition;
var
  Trackbar: TRichLogTrckbr;
begin
  Trackbar:= TRichLogTrckbr.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;

  Assert.AreEqual(TLogVerb(Trackbar.TrackBar.Position), Trackbar.Verbosity, 'Verbosity should match TrackBar position');
end;


procedure TTestRichLogTrack.TestVerbosity_CanSetAllLevels;
var
  Trackbar: TRichLogTrckbr;
begin
  Trackbar:= TRichLogTrckbr.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;

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

  RichLog:= TRichLog.Create(FTestForm);
  RichLog.Parent:= FTestForm;
  FRichLog:= RichLog;

  Trackbar.Log:= RichLog;

  Assert.AreEqual(TComponent(RichLog), TComponent(Trackbar.Log), 'Should be able to assign RichLog');
end;


procedure TTestRichLogTrack.TestLog_CanAssignNil;
var
  Trackbar: TRichLogTrckbr;
begin
  Trackbar:= TRichLogTrckbr.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;

  Assert.WillNotRaise(
    procedure
    begin
      Trackbar.Log:= NIL;
    end);
end;


procedure TTestRichLogTrack.TestLog_SynchronizesVerbosity;
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

  Trackbar.Verbosity:= lvrWarnings;
  Trackbar.Log:= RichLog;

  Assert.AreEqual(lvrWarnings, RichLog.Verbosity, 'Assigning Log should synchronize verbosity');
end;


{ OnVerbChanged Event Tests }

procedure TTestRichLogTrack.TestOnVerbChanged_CanBeAssigned;
var
  Trackbar: TRichLogTrckbr;
begin
  Trackbar:= TRichLogTrckbr.Create(FTestForm);
  Trackbar.Parent:= FTestForm;
  FTrackbar:= Trackbar;

  Trackbar.OnVerbChanged:= OnVerbChangedHandler;

  Assert.IsTrue(Assigned(Trackbar.OnVerbChanged), 'OnVerbChanged should be assignable');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestRichLogTrack);

end.
