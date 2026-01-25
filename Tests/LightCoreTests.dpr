program LightCoreTests;

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
  Test.LightCore.EncodeCRC in 'Test.LightCore.EncodeCRC.pas',
  Test.LightCore.EncodeXOR in 'Test.LightCore.EncodeXOR.pas',
  Test.LightCore.StringList in 'Test.LightCore.StringList.pas',
  Test.LightCore.Internet in 'Test.LightCore.Internet.pas',
  Test.LightCore.StreamFile in 'Test.LightCore.StreamFile.pas',
  Test.LightCore.StreamMem in 'Test.LightCore.StreamMem.pas',
  Test.LightCore.Binary in 'Test.LightCore.Binary.pas',
  Test.LightCore.Math in 'Test.LightCore.Math.pas',
  Test.LightCore.Time in 'Test.LightCore.Time.pas',
  Test.LightCore.HTML in 'Test.LightCore.HTML.pas',
  Test.LightCore.Types in 'Test.LightCore.Types.pas',
  Test.LightCore.TextFile in 'Test.LightCore.TextFile.pas',
  Test.LightCore.StrBuilder in 'Test.LightCore.StrBuilder.pas',
  { Source units }
  LightCore in '..\LightCore.pas',
  LightCore.Types in '..\LightCore.Types.pas',
  LightCore.EncodeCRC in '..\LightCore.EncodeCRC.pas',
  LightCore.EncodeXOR in '..\LightCore.EncodeXOR.pas',
  LightCore.StringList in '..\LightCore.StringList.pas',
  LightCore.Internet in '..\LightCore.Internet.pas',
  LightCore.StreamFile in '..\LightCore.StreamFile.pas',
  LightCore.Binary in '..\LightCore.Binary.pas',
  LightCore.IO in '..\LightCore.IO.pas',
  LightCore.HTML in '..\LightCore.HTML.pas',
  LightCore.Download in '..\LightCore.Download.pas',
  LightCore.StreamMem in '..\LightCore.StreamMem.pas',
  LightCore.Math in '..\LightCore.Math.pas',
  LightCore.Time in '..\LightCore.Time.pas',
  LightCore.TextFile in '..\LightCore.TextFile.pas',
  LightCore.StrBuilder in '..\LightCore.StrBuilder.pas',
  LightCore.StreamBuff in '..\LightCore.StreamBuff.pas';

{$IFNDEF TESTINSIGHT}
var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger: ITestLogger;
{$ENDIF}

begin
  ReportMemoryLeaksOnShutdown := True;

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
end.
