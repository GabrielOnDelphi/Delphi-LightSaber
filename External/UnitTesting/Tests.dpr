// TEMPLATE FILE
program Tests;

{=====================================================
   2026.02
   GabrielMoraru.com

   Tests for the core units in the AppName application.

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
  System.SysUtils,
  Vcl.Forms,
  Vcl.Dialogs,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  {$ENDIF }
  DUnitX.TestFramework,
  { LightSaber dependencies }
  LightCore.AppData,
  LightVcl.Visual.AppData,
  LightCore.StreamBuff;

{$IFNDEF TESTINSIGHT}
var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger: ITestLogger;
{$ENDIF}

var
  MainForm: TForm;

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown:= True;

  // Initialize AppData for tests that require it
  AppData:= TAppData.Create('AppNameTests', 'AppNameTests');
  TRY

  // Create a dummy main form - required by AppData methods that access MainForm
  Application.CreateForm(TForm, MainForm);
  MainForm.Visible:= FALSE;

{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
{$ELSE}
  try
    // Check command line options
    TDUnitX.CheckCommandLine;

    // Create the test runner
    runner:= TDUnitX.CreateRunner;

    // Add loggers
    logger:= TDUnitXConsoleLogger.Create(True);
    runner.AddLogger(logger);

    // Generate NUnit compatible XML results
    nunitLogger:= TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);

    runner.FailsOnNoAsserts:= FALSE;

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
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
{$ENDIF}
  FINALLY
    FreeAndNil(MainForm);
    FreeAndNil(AppData);
  END;
end.
