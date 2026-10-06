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
  {$IFDEF DEBUG}
  FastMM4,  { Must be the first unit: FastMM4 does not install once another unit has allocated memory }
  {$ENDIF}
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  {$ENDIF }
  DUnitX.TestFramework,
  LightCore.AppData,              { TAppDataCore.Unattended }
  { Source units }
  LightVcl.Internet.Common in '..\FrameVCL\LightVcl.Internet.Common.pas',
  LightVcl.Internet.Download.Indy in '..\FrameVCL\LightVcl.Internet.Download.Indy.pas',
  LightVcl.Internet.Email in '..\FrameVCL\LightVcl.Internet.Email.pas',
  LightVcl.Internet.HTML in '..\FrameVCL\LightVcl.Internet.HTML.pas',
  LightCore.Internet.EmailSender in '..\LightCore.Internet.EmailSender.pas',
  LightCore.Internet.Ftp in '..\LightCore.Internet.Ftp.pas',
  { Test units - add here as tests are created }
  Test.LightCore.Internet.Ftp in 'Test.LightCore.Internet.Ftp.pas',
  Test.LightVcl.Internet.Download.Indy in 'Test.LightVcl.Internet.Download.Indy.pas';
  // Test.LightVcl.Internet.Common in 'Test.LightVcl.Internet.Common.pas';

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
  ReportMemoryLeaksOnShutdown:= True;
  TAppDataCore.Unattended:= TRUE;  { Nobody is at the keyboard: MesajGeneric returns 0 at once instead
                                     of putting a modal box on screen, and ShowModal/Show are bypassed.
                                     Without it one library routine that reports a failure with a box
                                     stops the whole run dead until somebody clicks it. }

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
