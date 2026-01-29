unit Test.LightCore.Debugger;

{=============================================================================================================
   Unit tests for LightCore.Debugger.pas
   Tests debugging utilities, timing, logging, and compiler info functions.

   Requires: TESTINSIGHT compiler directive for TestInsight integration
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  LightCore.Debugger;

type
  [TestFixture]
  TTestDebugger = class
  private
    FTestDir: string;
    procedure CleanupTestDir;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Timer Tests }
    [Test]
    procedure TestTimerStartDoesNotRaise;

    [Test]
    procedure TestTimerElapsedReturnsNonNegative;

    [Test]
    procedure TestTimerElapsedMeasuresTime;

    [Test]
    procedure TestTimerElapsedSReturnsNonEmpty;

    [Test]
    procedure TestTimerElapsedSContainsUnit;

    [Test]
    procedure TestTimerElapsedSFormat_Nanoseconds;

    [Test]
    procedure TestTimerElapsedSFormat_Milliseconds;

    { Compiler Info Tests }
    [Test]
    procedure TestCompilerOptimizationReturnsBoolean;

    [Test]
    procedure TestCompilerOptimizationSContainsKeyword;

    [Test]
    procedure TestCompilerOptimizationSContainsState;

    { Debugger Detection }
    [Test]
    procedure TestIsRunningUnderDelphiDebuggerReturnsBoolean;

    { Report Generation }
    [Test]
    procedure TestGenerateCompilerReportNotEmpty;

    [Test]
    procedure TestGenerateCompilerReportContainsHeader;

    [Test]
    procedure TestGenerateCompilerReportContainsFields;

    { Log File Tests }
    [Test]
    procedure TestLogFile_InitCreatesFile;

    [Test]
    procedure TestLogFile_InitCreatesDirectory;

    [Test]
    procedure TestLogFile_AddWritesToFile;

    [Test]
    procedure TestLogFile_AddMultipleLines;

    [Test]
    procedure TestLogFile_AddBeforeInitDoesNotCrash;

    { Transfer Speed }
    [Test]
    procedure TestShowTransferSpeedFormat;

    [Test]
    procedure TestShowTransferSpeedContainsUnit;

    [Test]
    procedure TestShowTransferSpeedWithZeroTime;
  end;

implementation

uses
  LightCore.AppData;


procedure TTestDebugger.Setup;
begin
  FTestDir:= TPath.Combine(TPath.GetTempPath, 'DebuggerTest_' + TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FTestDir);

  { Initialize AppDataCore if not already done }
  if AppDataCore = nil
  then AppDataCore:= TAppDataCore.Create('DebuggerTest');
end;


procedure TTestDebugger.TearDown;
begin
  CleanupTestDir;
end;


procedure TTestDebugger.CleanupTestDir;
begin
  if TDirectory.Exists(FTestDir)
  then TDirectory.Delete(FTestDir, True);
end;


{ Timer Tests }

procedure TTestDebugger.TestTimerStartDoesNotRaise;
begin
  { TimerStart should not raise any exception }
  Assert.WillNotRaiseAny(
    procedure
    begin
      TimerStart;
    end
  );
end;


procedure TTestDebugger.TestTimerElapsedReturnsNonNegative;
VAR
  Elapsed: Double;
begin
  TimerStart;
  Elapsed:= TimerElapsed;

  Assert.IsTrue(Elapsed >= 0, 'Elapsed time must be non-negative');
end;


procedure TTestDebugger.TestTimerElapsedMeasuresTime;
VAR
  Elapsed: Double;
begin
  TimerStart;
  Sleep(50);  { Wait 50ms }
  Elapsed:= TimerElapsed;

  { Should have elapsed at least 40ms (allowing for timing variance) }
  Assert.IsTrue(Elapsed >= 40, Format('Expected >= 40ms, got %.2fms', [Elapsed]));
end;


procedure TTestDebugger.TestTimerElapsedSReturnsNonEmpty;
VAR
  Elapsed: string;
begin
  TimerStart;
  Elapsed:= TimerElapsedS;

  Assert.IsNotEmpty(Elapsed, 'TimerElapsedS should return non-empty string');
end;


procedure TTestDebugger.TestTimerElapsedSContainsUnit;
VAR
  Elapsed: string;
begin
  TimerStart;
  Sleep(1);
  Elapsed:= TimerElapsedS;

  { Should contain one of the time units }
  Assert.IsTrue(
    (Pos('ns', Elapsed) > 0) OR
    (Pos('us', Elapsed) > 0) OR
    (Pos('ms', Elapsed) > 0) OR
    (Pos('s', Elapsed) > 0) OR
    (Pos('m', Elapsed) > 0),
    'Result should contain a time unit: ' + Elapsed
  );
end;


procedure TTestDebugger.TestTimerElapsedSFormat_Nanoseconds;
VAR
  Elapsed: string;
begin
  { Very short time should show nanoseconds or microseconds }
  TimerStart;
  { No sleep - immediate }
  Elapsed:= TimerElapsedS;

  { Result should be valid (non-empty) }
  Assert.IsNotEmpty(Elapsed);
end;


procedure TTestDebugger.TestTimerElapsedSFormat_Milliseconds;
VAR
  Elapsed: string;
begin
  TimerStart;
  Sleep(10);  { 10ms should result in 'ms' unit }
  Elapsed:= TimerElapsedS;

  { For 10ms delay, should show in milliseconds }
  Assert.IsTrue(Pos('ms', Elapsed) > 0, 'Expected milliseconds unit for 10ms delay: ' + Elapsed);
end;


{ Compiler Info Tests }

procedure TTestDebugger.TestCompilerOptimizationReturnsBoolean;
VAR
  OptEnabled: Boolean;
begin
  OptEnabled:= CompilerOptimization;
  { Just verify it returns a valid boolean without exception }
  Assert.IsTrue((OptEnabled = True) OR (OptEnabled = False));
end;


procedure TTestDebugger.TestCompilerOptimizationSContainsKeyword;
VAR
  OptString: string;
begin
  OptString:= CompilerOptimizationS;

  Assert.IsTrue(Pos('Compiler optimization', OptString) > 0,
    'Should contain "Compiler optimization": ' + OptString);
end;


procedure TTestDebugger.TestCompilerOptimizationSContainsState;
VAR
  OptString: string;
begin
  OptString:= CompilerOptimizationS;

  Assert.IsTrue(
    (Pos('enabled', OptString) > 0) OR (Pos('disabled', OptString) > 0),
    'Should contain "enabled" or "disabled": ' + OptString
  );
end;


{ Debugger Detection }

procedure TTestDebugger.TestIsRunningUnderDelphiDebuggerReturnsBoolean;
VAR
  UnderDebugger: Boolean;
begin
  UnderDebugger:= IsRunningUnderDelphiDebugger;
  { Just verify it returns without exception }
  Assert.IsTrue((UnderDebugger = True) OR (UnderDebugger = False));
end;


{ Report Generation }

procedure TTestDebugger.TestGenerateCompilerReportNotEmpty;
VAR
  Report: string;
begin
  Report:= GenerateCompilerReport;

  Assert.IsNotEmpty(Report, 'Compiler report should not be empty');
end;


procedure TTestDebugger.TestGenerateCompilerReportContainsHeader;
VAR
  Report: string;
begin
  Report:= GenerateCompilerReport;

  Assert.IsTrue(Pos('COMPILER', Report) > 0, 'Report should contain COMPILER header');
end;


procedure TTestDebugger.TestGenerateCompilerReportContainsFields;
VAR
  Report: string;
begin
  Report:= GenerateCompilerReport;

  Assert.IsTrue(Pos('RunningUnderDelphi', Report) > 0, 'Report should contain RunningUnderDelphi');
  Assert.IsTrue(Pos('CompilerOptim', Report) > 0, 'Report should contain CompilerOptim');
  Assert.IsTrue(Pos('AppBitnessEx', Report) > 0, 'Report should contain AppBitnessEx');
end;


{ Log File Tests }

procedure TTestDebugger.TestLogFile_InitCreatesFile;
VAR
  LogPath: string;
begin
  LogPath:= TPath.Combine(FTestDir, 'test_init.log');
  LogFile_Init(LogPath);

  Assert.IsTrue(FileExists(LogPath), 'LogFile_Init should create the log file');
end;


procedure TTestDebugger.TestLogFile_InitCreatesDirectory;
VAR
  LogPath, SubDir: string;
begin
  SubDir:= TPath.Combine(FTestDir, 'subdir1\subdir2');
  LogPath:= TPath.Combine(SubDir, 'nested.log');
  LogFile_Init(LogPath);

  Assert.IsTrue(TDirectory.Exists(SubDir), 'LogFile_Init should create parent directories');
  Assert.IsTrue(FileExists(LogPath), 'LogFile_Init should create the log file');
end;


procedure TTestDebugger.TestLogFile_AddWritesToFile;
VAR
  LogPath, Content: string;
begin
  LogPath:= TPath.Combine(FTestDir, 'test_add.log');
  LogFile_Init(LogPath);

  LogFile_Add('Test message ABC');

  Content:= TFile.ReadAllText(LogPath);
  Assert.IsTrue(Pos('Test message ABC', Content) > 0,
    'Log file should contain the added message');
end;


procedure TTestDebugger.TestLogFile_AddMultipleLines;
VAR
  LogPath, Content: string;
begin
  LogPath:= TPath.Combine(FTestDir, 'test_multiple.log');
  LogFile_Init(LogPath);

  LogFile_Add('Line 1');
  LogFile_Add('Line 2');
  LogFile_Add('Line 3');

  Content:= TFile.ReadAllText(LogPath);
  Assert.IsTrue(Pos('Line 1', Content) > 0, 'Should contain Line 1');
  Assert.IsTrue(Pos('Line 2', Content) > 0, 'Should contain Line 2');
  Assert.IsTrue(Pos('Line 3', Content) > 0, 'Should contain Line 3');
end;


procedure TTestDebugger.TestLogFile_AddBeforeInitDoesNotCrash;
begin
  { Calling LogFile_Add before LogFile_Init should not crash }
  Assert.WillNotRaiseAny(
    procedure
    begin
      LogFile_Add('This should be silently ignored');
    end
  );
end;


{ Transfer Speed }

procedure TTestDebugger.TestShowTransferSpeedFormat;
VAR
  Speed: string;
begin
  TimerStart;
  Sleep(100);  { Simulate 100ms transfer }
  Speed:= ShowTransferSpeed(1024 * 1024);  { 1 MB }

  Assert.IsNotEmpty(Speed, 'ShowTransferSpeed should return non-empty string');
end;


procedure TTestDebugger.TestShowTransferSpeedContainsUnit;
VAR
  Speed: string;
begin
  TimerStart;
  Sleep(100);
  Speed:= ShowTransferSpeed(1024 * 1024);

  Assert.IsTrue(Pos('Speed', Speed) > 0, 'Should contain "Speed"');
  Assert.IsTrue(Pos('/sec', Speed) > 0, 'Should contain "/sec"');
end;


procedure TTestDebugger.TestShowTransferSpeedWithZeroTime;
VAR
  Speed: string;
begin
  { Test with immediate measurement (near-zero time) }
  TimerStart;
  Speed:= ShowTransferSpeed(1024);

  { Should handle division by zero gracefully }
  Assert.IsNotEmpty(Speed, 'Should return something even with zero time');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestDebugger);

end.
