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

    { SaveAsRtf Tests }
    [Test]
    procedure TestSaveAsRtf_EmptyPath_ShouldRaise;

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
  end;

implementation

uses
  LightVcl.Common.LogViewer;

procedure TTestLogViewer.Setup;
begin
  FTestForm:= TForm.Create(NIL);
  FTestForm.Width:= 400;
  FTestForm.Height:= 300;
end;

procedure TTestLogViewer.TearDown;
begin
  FreeAndNil(FTestForm);
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
begin
  { Ensure all verbosity levels return valid colors without raising exceptions }
  Assert.IsTrue(Verbosity2Color(lvDebug) <> 0, 'Debug should have a color');
  Assert.IsTrue(Verbosity2Color(lvVerbose) <> 0, 'Verbose should have a color');
  Assert.IsTrue(Verbosity2Color(lvHints) <> 0, 'Hints should have a color');
  Assert.IsTrue(Verbosity2Color(lvInfos) <> 0, 'Infos should have a color');
  Assert.IsTrue(Verbosity2Color(lvImportant) <> 0, 'Important should have a color');
  Assert.IsTrue(Verbosity2Color(lvWarnings) <> 0, 'Warnings should have a color');
  Assert.IsTrue(Verbosity2Color(lvErrors) <> 0, 'Errors should have a color');
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

    LogViewer.RamLog.AddInfo('Message 1');
    LogViewer.RamLog.AddInfo('Message 2');
    LogViewer.RamLog.AddError('Error 1');

    Assert.IsTrue(LogViewer.Count > 0, 'Count should be greater than 0 after adding entries');
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


initialization
  TDUnitX.RegisterTestFixture(TTestLogViewer);

end.
