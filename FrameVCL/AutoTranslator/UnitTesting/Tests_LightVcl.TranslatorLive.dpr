program Tests_LightVcl.TranslatorLive;

{=====================================================
When TESTINSIGHT is defined:
  - Test output is redirected to the TestInsight panel inside the Delphi IDE
  - Console output is suppressed or minimal

When TESTINSIGHT is not defined:
  - Test results are written to the console/stdout
  - Useful for command-line builds, CI/CD pipelines, or when running tests outside the IDE
=====================================================}

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}

{$STRONGLINKTYPES ON}

uses
  {$IFDEF DEBUG}
  FastMM4,  { Must be the first unit: FastMM4 does not install once another unit has allocated memory }
  {$ENDIF}
  System.SysUtils,
  Vcl.Forms,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  {$ENDIF }
  DUnitX.TestFramework,
  Test.RunnerChecks in '..\..\..\UnitTesting\Test.RunnerChecks.pas',
  LightCore.AppData,
  Test.LightVcl.TranslatorLive in 'Test.LightVcl.TranslatorLive.pas',  LightVcl.TranslatorAPI in '..\LightVcl.TranslatorAPI.pas',
  LightVcl.Visual.AppData in '..\..\LightVcl.Visual.AppData.pas';

{$IFNDEF TESTINSIGHT}
var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger: ITestLogger;
{$ENDIF}

begin
  {$IFDEF DEBUG}
  FastMM4.SuppressMessageBoxes:= TRUE;  { Leaks and heap errors go only to <exe>_MemoryManager_EventLog.txt; a message box would stop the unattended run }
  {$IFNDEF TESTINSIGHT}
  if FastMM4.FastMM_GetInstallationState = FastMM4.mmisInstalled
  then System.Writeln('FastMM4 ', FastMM4.FastMMVersion, ': installed')
  else System.Writeln('FastMM4: NOT INSTALLED (FastMM_FullDebugMode.dll not found, or FastMM4 is not the first unit) - leaks and heap errors go undetected');
  {$ENDIF}
  {$ENDIF}
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;

  // Initialize AppData for tests that require it
  AppData:= TAppData.Create('LightVclTranslatorLiveTests');
  TRY

{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
{$ELSE}
  try
    // Check command line options
    TDUnitX.CheckCommandLine;

    // This runner's tests read no fixture folder (they write their INI files into the Windows temp folder). Add one here when a test starts to read sample files from disk.
    FixturesMissing([]);

    // Create the test runner
    runner:= TDUnitX.CreateRunner;

    // Add loggers
    logger:= TDUnitXConsoleLogger.Create(True);
    runner.AddLogger(logger);

    // Generate NUnit compatible XML results
    nunitLogger:= TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);

    runner.FailsOnNoAsserts:= TRUE;   { A [Test] with zero Assert calls is a fake test. With no DeepL key every test ends in Assert.Pass, which DUnitX counts as passed (DUnitX.TestRunner.pas:758-762) }

    // Run tests
    results:= runner.Execute;
    if not results.AllPassed then
      System.ExitCode:= EXIT_ERRORS;

    {$IFNDEF CI}
    // Wait for input if running interactively
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Press ENTER to continue...');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      begin
        System.Writeln(E.ClassName, ': ', E.Message);
        if System.ExitCode = EXIT_OK
        then System.ExitCode:= EXIT_ERRORS;   { An exception of the runner itself (no test registered, a crash in a logger) must not end the run with exit code 0. A bad option keeps the EXIT_OPTIONS_ERROR that CheckCommandLine set. }
      end;
  end;
{$ENDIF}
  FINALLY
    FreeAndNil(AppData);
  END;
end.
