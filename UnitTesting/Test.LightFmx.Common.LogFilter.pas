unit Test.LightFmx.Common.LogFilter;

{=============================================================================================================
   2026.01.31
   Unit tests for LightFmx.Common.LogFilter.pas
   Tests verbosity filter control functionality
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  FMX.Forms,
  FMX.Types,
  FMX.StdCtrls,
  LightCore.LogTypes;

type
  [TestFixture]
  TTestLogVerbFilter = class
  private
    FTestForm: TForm;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor Tests }
    [Test]
    procedure TestCreate_InitializesTrackBar;

    [Test]
    procedure TestCreate_DefaultProperties;

    [Test]
    procedure TestCreate_DefaultDimensions;

    { Verbosity Tests }
    [Test]
    procedure TestVerbosity_SetAndGet;

    [Test]
    procedure TestVerbosity_AllLevels;

    { ShowDebugMsg Tests }
    [Test]
    procedure TestShowDebugMsg_DefaultFalse;

    [Test]
    procedure TestShowDebugMsg_EnablesDebugLevel;

    [Test]
    procedure TestShowDebugMsg_DisablesDebugLevel;

    { TrackBar Configuration Tests }
    [Test]
    procedure TestTrackBar_HasCorrectRange;

    [Test]
    procedure TestTrackBar_HasName;

    { Log Property Tests }
    [Test]
    procedure TestLog_DefaultNil;

    [Test]
    procedure TestLog_AssignSyncsVerbosity;

    [Test]
    procedure TestLog_AssignRegistersFilter;
  end;

implementation

uses
  LightFmx.Common.LogFilter, LightFmx.Common.LogViewer;


procedure TTestLogVerbFilter.Setup;
begin
  FTestForm:= TForm.Create(NIL);
  FTestForm.ClientWidth:= 400;
  FTestForm.ClientHeight:= 300;
end;


procedure TTestLogVerbFilter.TearDown;
begin
  FreeAndNil(FTestForm);
end;


{ Constructor Tests }

procedure TTestLogVerbFilter.TestCreate_InitializesTrackBar;
var
  Filter: TLogVerbFilter;
begin
  Filter:= TLogVerbFilter.Create(FTestForm);
  try
    Filter.Parent:= FTestForm;
    Assert.IsNotNull(Filter.TrackBar, 'TrackBar should be initialized');
  finally
    FreeAndNil(Filter);
  end;
end;


procedure TTestLogVerbFilter.TestCreate_DefaultProperties;
var
  Filter: TLogVerbFilter;
begin
  Filter:= TLogVerbFilter.Create(FTestForm);
  try
    Filter.Parent:= FTestForm;
    Assert.IsFalse(Filter.ShowDebugMsg, 'ShowDebugMsg should default to False');
    Assert.IsNull(Filter.Log, 'Log should be nil by default');
  finally
    FreeAndNil(Filter);
  end;
end;


procedure TTestLogVerbFilter.TestCreate_DefaultDimensions;
var
  Filter: TLogVerbFilter;
begin
  Filter:= TLogVerbFilter.Create(FTestForm);
  try
    Filter.Parent:= FTestForm;
    Assert.AreEqual(Single(260), Filter.Width, 'Default width should be 260');
    Assert.AreEqual(Single(27), Filter.Height, 'Default height should be 27');
  finally
    FreeAndNil(Filter);
  end;
end;


{ Verbosity Tests }

procedure TTestLogVerbFilter.TestVerbosity_SetAndGet;
var
  Filter: TLogVerbFilter;
begin
  Filter:= TLogVerbFilter.Create(FTestForm);
  try
    Filter.Parent:= FTestForm;

    Filter.Verbosity:= lvWarnings;
    Assert.AreEqual(lvWarnings, Filter.Verbosity, 'Verbosity should be set to lvWarnings');

    Filter.Verbosity:= lvErrors;
    Assert.AreEqual(lvErrors, Filter.Verbosity, 'Verbosity should be set to lvErrors');
  finally
    FreeAndNil(Filter);
  end;
end;


procedure TTestLogVerbFilter.TestVerbosity_AllLevels;
var
  Filter: TLogVerbFilter;
  Level: TLogVerbLvl;
begin
  Filter:= TLogVerbFilter.Create(FTestForm);
  try
    Filter.Parent:= FTestForm;
    Filter.ShowDebugMsg:= TRUE;  { Enable debug level access }

    { Test all verbosity levels can be set and retrieved }
    for Level:= Low(TLogVerbLvl) to High(TLogVerbLvl) do
      begin
        Filter.Verbosity:= Level;
        Assert.AreEqual(Level, Filter.Verbosity, 'Verbosity level ' + Verbosity2String(Level) + ' should round-trip');
      end;
  finally
    FreeAndNil(Filter);
  end;
end;


{ ShowDebugMsg Tests }

procedure TTestLogVerbFilter.TestShowDebugMsg_DefaultFalse;
var
  Filter: TLogVerbFilter;
begin
  Filter:= TLogVerbFilter.Create(FTestForm);
  try
    Filter.Parent:= FTestForm;
    Assert.IsFalse(Filter.ShowDebugMsg, 'ShowDebugMsg should default to False');
    Assert.AreEqual(Single(1), Filter.TrackBar.Min, 'TrackBar.Min should be 1 when ShowDebugMsg is False');
  finally
    FreeAndNil(Filter);
  end;
end;


procedure TTestLogVerbFilter.TestShowDebugMsg_EnablesDebugLevel;
var
  Filter: TLogVerbFilter;
begin
  Filter:= TLogVerbFilter.Create(FTestForm);
  try
    Filter.Parent:= FTestForm;

    Filter.ShowDebugMsg:= TRUE;

    Assert.IsTrue(Filter.ShowDebugMsg, 'ShowDebugMsg should be True');
    Assert.AreEqual(Single(0), Filter.TrackBar.Min, 'TrackBar.Min should be 0 when ShowDebugMsg is True');
  finally
    FreeAndNil(Filter);
  end;
end;


procedure TTestLogVerbFilter.TestShowDebugMsg_DisablesDebugLevel;
var
  Filter: TLogVerbFilter;
begin
  Filter:= TLogVerbFilter.Create(FTestForm);
  try
    Filter.Parent:= FTestForm;

    Filter.ShowDebugMsg:= TRUE;
    Filter.ShowDebugMsg:= FALSE;

    Assert.IsFalse(Filter.ShowDebugMsg, 'ShowDebugMsg should be False');
    Assert.AreEqual(Single(1), Filter.TrackBar.Min, 'TrackBar.Min should be 1 when ShowDebugMsg is False');
  finally
    FreeAndNil(Filter);
  end;
end;


{ TrackBar Configuration Tests }

procedure TTestLogVerbFilter.TestTrackBar_HasCorrectRange;
var
  Filter: TLogVerbFilter;
begin
  Filter:= TLogVerbFilter.Create(FTestForm);
  try
    Filter.Parent:= FTestForm;

    Assert.AreEqual(Single(1), Filter.TrackBar.Min, 'TrackBar.Min should be 1 by default');
    Assert.AreEqual(Single(Ord(High(TLogVerbLvl))), Filter.TrackBar.Max, 'TrackBar.Max should match highest verbosity level');
  finally
    FreeAndNil(Filter);
  end;
end;


procedure TTestLogVerbFilter.TestTrackBar_HasName;
var
  Filter: TLogVerbFilter;
begin
  Filter:= TLogVerbFilter.Create(FTestForm);
  try
    Filter.Parent:= FTestForm;
    Assert.AreEqual('VerbosityTrackbar', Filter.TrackBar.Name, 'TrackBar should have name for INI file saving');
  finally
    FreeAndNil(Filter);
  end;
end;


{ Log Property Tests }

procedure TTestLogVerbFilter.TestLog_DefaultNil;
var
  Filter: TLogVerbFilter;
begin
  Filter:= TLogVerbFilter.Create(FTestForm);
  try
    Filter.Parent:= FTestForm;
    Assert.IsNull(Filter.Log, 'Log should be nil by default');
  finally
    FreeAndNil(Filter);
  end;
end;


procedure TTestLogVerbFilter.TestLog_AssignSyncsVerbosity;
var
  Filter: TLogVerbFilter;
  LogViewer: TLogViewer;
begin
  Filter:= TLogVerbFilter.Create(FTestForm);
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    Filter.Parent:= FTestForm;
    LogViewer.Parent:= FTestForm;

    { Set LogViewer to a specific verbosity before assigning }
    LogViewer.Verbosity:= lvWarnings;

    { Assign Log to Filter }
    Filter.Log:= LogViewer;

    { Filter should sync to LogViewer's verbosity }
    Assert.AreEqual(lvWarnings, Filter.Verbosity, 'Filter verbosity should sync to LogViewer verbosity on assignment');
  finally
    FreeAndNil(Filter);
    FreeAndNil(LogViewer);
  end;
end;


procedure TTestLogVerbFilter.TestLog_AssignRegistersFilter;
var
  Filter: TLogVerbFilter;
  LogViewer: TLogViewer;
begin
  Filter:= TLogVerbFilter.Create(FTestForm);
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    Filter.Parent:= FTestForm;
    LogViewer.Parent:= FTestForm;

    { Assign Log to Filter }
    Filter.Log:= LogViewer;

    { Changing LogViewer verbosity should update Filter's trackbar }
    LogViewer.Verbosity:= lvErrors;

    { The bidirectional link should sync verbosity }
    Assert.AreEqual(lvErrors, Filter.Verbosity, 'Filter should stay in sync when LogViewer verbosity changes');
  finally
    FreeAndNil(Filter);
    FreeAndNil(LogViewer);
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestLogVerbFilter);

end.
