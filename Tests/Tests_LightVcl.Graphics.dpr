program Tests_LightVcl.Graphics;

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
  System.SysUtils,
  Vcl.Forms,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  {$ENDIF }
  DUnitX.TestFramework,
  LightCore.AppData,
  LightVcl.Visual.AppData in '..\FrameVCL\LightVcl.Visual.AppData.pas',
  Test.LightVcl.Graph.FX.Gradient in 'Test.LightVcl.Graph.FX.Gradient.pas',
  LightVcl.Graph.FX.Gradient in '..\FrameVCL\LightVcl.Graph.FX.Gradient.pas',
  Test.LightVcl.Graph.FX in 'Test.LightVcl.Graph.FX.pas',
  LightVcl.Graph.FX in '..\FrameVCL\LightVcl.Graph.FX.pas',
  Test.LightVcl.Graph.Desktop in 'Test.LightVcl.Graph.Desktop.pas',
  LightVcl.Graph.Desktop in '..\FrameVCL\LightVcl.Graph.Desktop.pas',
  Test.LightVcl.Graph.UtilGray in 'Test.LightVcl.Graph.UtilGray.pas',
  LightVcl.Graph.UtilGray in '..\FrameVCL\LightVcl.Graph.UtilGray.pas',
  Test.LightVcl.Graph.Alpha in 'Test.LightVcl.Graph.Alpha.pas',
  LightVcl.Graph.Alpha in '..\FrameVCL\LightVcl.Graph.Alpha.pas',
  Test.LightVcl.Graph.Cache in 'Test.LightVcl.Graph.Cache.pas',
  LightVcl.Graph.Cache in '..\FrameVCL\LightVcl.Graph.Cache.pas',
  Test.LightVcl.Graph.Text in 'Test.LightVcl.Graph.Text.pas',
  LightVcl.Graph.Text in '..\FrameVCL\LightVcl.Graph.Text.pas';

{$IFNDEF TESTINSIGHT}
var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger: ITestLogger;
{$ENDIF}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;

  // Initialize AppData for tests that require it
  AppData:= TAppData.Create('LightVclGraphicsTests');
  TRY

{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
{$ELSE}
  try
    // Check command line options
    TDUnitX.CheckCommandLine;

    // Create the test runner
    runner := TDUnitX.CreateRunner;

    // Add loggers
    logger := TDUnitXConsoleLogger.Create(True);
    runner.AddLogger(logger);

    // Generate NUnit compatible XML results
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);

    runner.FailsOnNoAsserts := False;

    // Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

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
    FreeAndNil(AppData);
  END;
end.
