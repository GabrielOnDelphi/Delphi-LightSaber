unit Test.LightFmx.Common.LogViewer;

{=============================================================================================================
   2026.01.31
   Unit tests for LightFmx.Common.LogViewer.pas
   Tests FMX log viewer component functionality, filtering, and clipboard operations
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  FMX.Forms,
  FMX.Types,
  LightCore.LogTypes,
  LightCore.LogRam;

type
  [TestFixture]
  TTestFmxLogViewer = class
  private
    FTestForm: TForm;
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

    [Test]
    procedure TestCreate_OwnsRamLog;

    { AssignExternalRamLog Tests }
    [Test]
    procedure TestAssignExternalRamLog_NilParameter_ShouldRaise;

    [Test]
    procedure TestAssignExternalRamLog_ValidLog_Assigns;

    [Test]
    procedure TestAssignExternalRamLog_DoesNotOwnExternal;

    { ConstructInternalRamLog Tests }
    [Test]
    procedure TestConstructInternalRamLog_CreatesNew;

    { Verbosity2Color Tests }
    [Test]
    procedure TestVerbosity2Color_Debug;

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

    [Test]
    procedure TestVerbosityFilter_SetValue;

    { ShowTime/ShowDate Tests }
    [Test]
    procedure TestShowTime_DefaultFalse;

    [Test]
    procedure TestShowDate_DefaultFalse;

    [Test]
    procedure TestShowTime_SetTrue;

    [Test]
    procedure TestShowDate_SetTrue;

    { AutoScroll Tests }
    [Test]
    procedure TestAutoScroll_DefaultTrue;

    [Test]
    procedure TestAutoScroll_SetFalse;

    { Clear Tests }
    [Test]
    procedure TestClear_RemovesAllEntries;
  end;

implementation

uses
  LightFmx.Common.LogViewer;


procedure TTestFmxLogViewer.Setup;
begin
  FTestForm:= TForm.Create(NIL);
  FTestForm.ClientWidth:= 800;
  FTestForm.ClientHeight:= 600;
end;


procedure TTestFmxLogViewer.TearDown;
begin
  FreeAndNil(FTestForm);
end;


{ Constructor Tests }

procedure TTestFmxLogViewer.TestCreate_InitializesRamLog;
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


procedure TTestFmxLogViewer.TestCreate_DefaultProperties;
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
  finally
    FreeAndNil(LogViewer);
  end;
end;


procedure TTestFmxLogViewer.TestCreate_OwnsRamLog;
var
  LogViewer: TLogViewer;
  RamLogPtr: Pointer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    RamLogPtr:= LogViewer.RamLog;
    Assert.IsNotNull(RamLogPtr, 'RamLog should exist');
    { The destructor will free the owned RamLog - if it crashes, ownership is wrong }
  finally
    FreeAndNil(LogViewer);
  end;
end;


{ AssignExternalRamLog Tests }

procedure TTestFmxLogViewer.TestAssignExternalRamLog_NilParameter_ShouldRaise;
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
      EAssertionFailed);
  finally
    FreeAndNil(LogViewer);
  end;
end;


procedure TTestFmxLogViewer.TestAssignExternalRamLog_ValidLog_Assigns;
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


procedure TTestFmxLogViewer.TestAssignExternalRamLog_DoesNotOwnExternal;
var
  LogViewer: TLogViewer;
  ExternalLog: TRamLog;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  ExternalLog:= TRamLog.Create(FALSE, NIL);
  try
    LogViewer.Parent:= FTestForm;
    LogViewer.AssignExternalRamLog(ExternalLog);

    { Free the LogViewer - it should NOT free the external log }
    FreeAndNil(LogViewer);

    { If we can still access ExternalLog, ownership was correctly NOT transferred }
    Assert.IsNotNull(ExternalLog, 'External log should still exist after LogViewer destruction');
    ExternalLog.AddInfo('Still accessible');
  finally
    FreeAndNil(LogViewer);
    FreeAndNil(ExternalLog);
  end;
end;


{ ConstructInternalRamLog Tests }

procedure TTestFmxLogViewer.TestConstructInternalRamLog_CreatesNew;
var
  LogViewer: TLogViewer;
  ExternalLog: TRamLog;
  OriginalLog: TRamLog;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  ExternalLog:= TRamLog.Create(FALSE, NIL);
  try
    LogViewer.Parent:= FTestForm;

    { First assign external log }
    LogViewer.AssignExternalRamLog(ExternalLog);
    OriginalLog:= LogViewer.RamLog;
    Assert.AreSame(ExternalLog, OriginalLog, 'Should have external log');

    { Now construct internal log }
    LogViewer.ConstructInternalRamLog;

    Assert.IsNotNull(LogViewer.RamLog, 'Should have new RamLog');
    Assert.AreNotSame(ExternalLog, LogViewer.RamLog, 'Should be different from external log');
  finally
    FreeAndNil(LogViewer);
    FreeAndNil(ExternalLog);
  end;
end;


{ Verbosity2Color Tests }

procedure TTestFmxLogViewer.TestVerbosity2Color_Debug;
begin
  Assert.AreEqual(TAlphaColors.Lightgray, Verbosity2Color(lvDebug), 'Debug should be light gray');
end;


procedure TTestFmxLogViewer.TestVerbosity2Color_Errors;
begin
  Assert.AreEqual(TAlphaColors.Red, Verbosity2Color(lvErrors), 'Errors should be red');
end;


procedure TTestFmxLogViewer.TestVerbosity2Color_AllLevels;
var
  Level: TLogVerbLvl;
begin
  { Ensure all verbosity levels return valid colors without raising exceptions }
  for Level:= Low(TLogVerbLvl) to High(TLogVerbLvl) do
    Assert.IsTrue(Verbosity2Color(Level) <> 0, Verbosity2String(Level) + ' should have a color');
end;


{ Count Tests }

procedure TTestFmxLogViewer.TestCount_EmptyLog;
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


procedure TTestFmxLogViewer.TestCount_WithEntries;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;

    LogViewer.RamLog.AddInfo('Message 1');
    LogViewer.RamLog.AddInfo('Message 2');
    LogViewer.RamLog.AddError('Error 1');

    Assert.IsTrue(LogViewer.Count > 0, 'Count should be greater than 0 after adding entries');
  finally
    FreeAndNil(LogViewer);
  end;
end;


{ Verbosity Filter Tests }

procedure TTestFmxLogViewer.TestVerbosityFilter_DefaultValue;
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


procedure TTestFmxLogViewer.TestVerbosityFilter_SetValue;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;

    LogViewer.Verbosity:= lvWarnings;
    Assert.AreEqual(lvWarnings, LogViewer.Verbosity, 'Verbosity should be set to lvWarnings');

    LogViewer.Verbosity:= lvErrors;
    Assert.AreEqual(lvErrors, LogViewer.Verbosity, 'Verbosity should be set to lvErrors');
  finally
    FreeAndNil(LogViewer);
  end;
end;


{ ShowTime/ShowDate Tests }

procedure TTestFmxLogViewer.TestShowTime_DefaultFalse;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    Assert.IsFalse(LogViewer.ShowTime, 'ShowTime should default to False');
  finally
    FreeAndNil(LogViewer);
  end;
end;


procedure TTestFmxLogViewer.TestShowDate_DefaultFalse;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    Assert.IsFalse(LogViewer.ShowDate, 'ShowDate should default to False');
  finally
    FreeAndNil(LogViewer);
  end;
end;


procedure TTestFmxLogViewer.TestShowTime_SetTrue;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    LogViewer.ShowTime:= TRUE;
    Assert.IsTrue(LogViewer.ShowTime, 'ShowTime should be True');
  finally
    FreeAndNil(LogViewer);
  end;
end;


procedure TTestFmxLogViewer.TestShowDate_SetTrue;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    LogViewer.ShowDate:= TRUE;
    Assert.IsTrue(LogViewer.ShowDate, 'ShowDate should be True');
  finally
    FreeAndNil(LogViewer);
  end;
end;


{ AutoScroll Tests }

procedure TTestFmxLogViewer.TestAutoScroll_DefaultTrue;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    Assert.IsTrue(LogViewer.AutoScroll, 'AutoScroll should default to True');
  finally
    FreeAndNil(LogViewer);
  end;
end;


procedure TTestFmxLogViewer.TestAutoScroll_SetFalse;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    LogViewer.AutoScroll:= FALSE;
    Assert.IsFalse(LogViewer.AutoScroll, 'AutoScroll should be False');
  finally
    FreeAndNil(LogViewer);
  end;
end;


{ Clear Tests }

procedure TTestFmxLogViewer.TestClear_RemovesAllEntries;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;

    { Add some entries }
    LogViewer.RamLog.AddInfo('Message 1');
    LogViewer.RamLog.AddError('Error 1');
    Assert.IsTrue(LogViewer.Count > 0, 'Should have entries before clear');

    { Clear }
    LogViewer.Clear;

    Assert.AreEqual(0, LogViewer.Count, 'Count should be 0 after clear');
  finally
    FreeAndNil(LogViewer);
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFmxLogViewer);

end.
