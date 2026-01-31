program Tests_LightFmx;

{=====================================================
   Unit tests for LightFmx units
   2026.01.31

   When TESTINSIGHT is defined:
     - Test output is redirected to the TestInsight panel inside the Delphi IDE

   When TESTINSIGHT is not defined:
     - Test results are written to the console/stdout
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
  { Test units }
  Test.LightFmx.Common.CamUtils in 'Test.LightFmx.Common.CamUtils.pas',
  { Source units }
  LightCore.IO in '..\LightCore.IO.pas',
  LightFmx.Common.CamUtils in '..\FrameFMX\LightFmx.Common.CamUtils.pas';

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
    TDUnitX.CheckCommandLine;

    runner:= TDUnitX.CreateRunner;

    logger:= TDUnitXConsoleLogger.Create(True);
    runner.AddLogger(logger);

    nunitLogger:= TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);

    runner.FailsOnNoAsserts:= FALSE;

    results:= runner.Execute;
    if not results.AllPassed then
      System.ExitCode:= EXIT_ERRORS;

    {$IFNDEF CI}
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
