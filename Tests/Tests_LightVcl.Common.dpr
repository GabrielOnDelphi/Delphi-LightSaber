program Tests_LightVcl.Common;

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
  Test.LightVcl.Common.CenterControl in 'Test.LightVcl.Common.CenterControl.pas',
  Test.LightVcl.Common.Debugger in 'Test.LightVcl.Common.Debugger.pas',
  Test.LightVcl.Common.Dialogs in 'Test.LightVcl.Common.Dialogs.pas',
  Test.LightVcl.Common.EllipsisText in 'Test.LightVcl.Common.EllipsisText.pas',
  LightVcl.Common.CenterControl in '..\FrameVCL\LightVcl.Common.CenterControl.pas',
  LightVcl.Common.Debugger in '..\FrameVCL\LightVcl.Common.Debugger.pas',
  LightVcl.Common.Dialogs in '..\FrameVCL\LightVcl.Common.Dialogs.pas',
  LightVcl.Common.EllipsisText in '..\FrameVCL\LightVcl.Common.EllipsisText.pas',
  Test.LightVcl.Common.IO in 'Test.LightVcl.Common.IO.pas',
  Test.LightVcl.Common.EnvironmentVar in 'Test.LightVcl.Common.EnvironmentVar.pas',
  LightVcl.Common.EnvironmentVar in '..\FrameVCL\LightVcl.Common.EnvironmentVar.pas',
  Test.LightVcl.Common.ExecuteProc in 'Test.LightVcl.Common.ExecuteProc.pas',
  LightVcl.Common.ExecuteProc in '..\FrameVCL\LightVcl.Common.ExecuteProc.pas',
  Test.LightVcl.Common.ExecuteShell in 'Test.LightVcl.Common.ExecuteShell.pas',
  LightVcl.Common.ExecuteShell in '..\FrameVCL\LightVcl.Common.ExecuteShell.pas',
  Test.LightVcl.Common.ExeVersion in 'Test.LightVcl.Common.ExeVersion.pas',
  LightVcl.Common.ExeVersion in '..\FrameVCL\LightVcl.Common.ExeVersion.pas',
  Test.LightVcl.Common.IniFile in 'Test.LightVcl.Common.IniFile.pas',
  LightVcl.Common.IniFile in '..\FrameVCL\LightVcl.Common.IniFile.pas',
  Test.LightVcl.Common.Keyboard in 'Test.LightVcl.Common.Keyboard.pas',
  LightVcl.Common.Keyboard in '..\FrameVCL\LightVcl.Common.Keyboard.pas',
  Test.LightVcl.Common.Clipboard in 'Test.LightVcl.Common.Clipboard.pas',
  LightVcl.Common.Clipboard in '..\FrameVCL\LightVcl.Common.Clipboard.pas';

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
  AppData:= TAppData.Create('LightVclCommonTests');
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
