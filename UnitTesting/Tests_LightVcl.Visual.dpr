program Tests_LightVcl.Visual;

{=====================================================
   Tests for units in the LightVcl.Visual.dpk package.
   This package contains VCL visual controls and the TAppData class.

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
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  {$ENDIF }
  DUnitX.TestFramework,
  { Source units }
  LightCore.AppData,
  LightVcl.Visual.AppData in '..\FrameVCL\LightVcl.Visual.AppData.pas',
  LightVcl.Visual.AppDataForm in '..\FrameVCL\LightVcl.Visual.AppDataForm.pas',
  LightVcl.Visual.LogForm in '..\FrameVCL\LightVcl.Visual.LogForm.pas',
  LightVcl.Visual.ActivityIndicator in '..\FrameVCL\LightVcl.Visual.ActivityIndicator.pas',
  LightVcl.Graph.Util in '..\FrameVCL\LightVcl.Graph.Util.pas',
  { Test units }
  Test.LightVcl.Visual.AppData in 'Test.LightVcl.Visual.AppData.pas',
  Test.LightVcl.Visual.ActivityIndicator in 'Test.LightVcl.Visual.ActivityIndicator.pas';

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
  AppData:= TAppData.Create('LightVclVisualTests');
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
