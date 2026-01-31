program Tests_LightVcl.Internet;

{=====================================================
   Tests for units in the LightVcl.Internet.dpk package.
   This package contains internet-related functionality:
   downloads, email, HTML processing, and the updater.

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
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  {$ENDIF }
  DUnitX.TestFramework,
  { Source units }
  LightVcl.Internet.Common in '..\FrameVCL\LightVcl.Internet.Common.pas',
  LightVcl.Internet.CommonWebDown in '..\FrameVCL\LightVcl.Internet.CommonWebDown.pas',
  LightVcl.Internet.Download.Indy in '..\FrameVCL\LightVcl.Internet.Download.Indy.pas',
  LightVcl.Internet.Download.Thread in '..\FrameVCL\LightVcl.Internet.Download.Thread.pas',
  LightVcl.Internet.Download.WinInet in '..\FrameVCL\LightVcl.Internet.Download.WinInet.pas',
  LightVcl.Internet.Email in '..\FrameVCL\LightVcl.Internet.Email.pas',
  LightVcl.Internet.EmailSender in '..\FrameVCL\LightVcl.Internet.EmailSender.pas',
  LightVcl.Internet.HTML in '..\FrameVCL\LightVcl.Internet.HTML.pas',
  LightVcl.Internet.HTMLImg in '..\FrameVCL\LightVcl.Internet.HTMLImg.pas',
  LightVcl.Internet.HtmlWriter in '..\FrameVCL\LightVcl.Internet.HtmlWriter.pas';
  { Test units - add here as tests are created }
  // Test.LightVcl.Internet.Common in 'Test.LightVcl.Internet.Common.pas';

{$IFNDEF TESTINSIGHT}
var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger: ITestLogger;
{$ENDIF}

begin
  ReportMemoryLeaksOnShutdown:= True;

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
end.
