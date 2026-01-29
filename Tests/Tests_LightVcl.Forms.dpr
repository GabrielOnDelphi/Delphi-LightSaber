program Tests_LightVcl.Forms;

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
  {$ENDIF }
  DUnitX.TestFramework,
  LightCore.AppData,
  LightVcl.Visual.AppData in '..\FrameVCL\LightVcl.Visual.AppData.pas',
  Test.FormAbout in 'Test.FormAbout.pas',
  Test.FormAsyncMessage in 'Test.FormAsyncMessage.pas',
  Test.FormDrawing in 'Test.FormDrawing.pas',
  Test.FormEmailComposer in 'Test.FormEmailComposer.pas',
  Test.FormEmailServer in 'Test.FormEmailServer.pas',
  Test.FormPower in 'Test.FormPower.pas',
  Test.FormProxyList in 'Test.FormProxyList.pas',
  Test.FormReminder in 'Test.FormReminder.pas',
  Test.FormRichLog in 'Test.FormRichLog.pas',
  Test.FormSkinsDisk in 'Test.FormSkinsDisk.pas',
  Test.FormSkinsRes in 'Test.FormSkinsRes.pas',
  Test.FormSplashScreen in 'Test.FormSplashScreen.pas',
  Test.FormTranslEditor in 'Test.FormTranslEditor.pas',
  Test.FormTranslSelector in 'Test.FormTranslSelector.pas',
  Test.FormUniversalEula in 'Test.FormUniversalEula.pas',
  Test.FrameExplorer in 'Test.FrameExplorer.pas',
  LightVcl.Common.IO in '..\FrameVCL\LightVcl.Common.IO.pas',
  FormAbout in '..\FrameVCL\FormAbout.pas',
  FormAsyncMessage in '..\FrameVCL\FormAsyncMessage.pas',
  FormDrawing in '..\FrameVCL\FormDrawing.pas',
  FormEmailComposer in '..\FrameVCL\FormEmailComposer.pas',
  FormEmailServer in '..\FrameVCL\FormEmailServer.pas',
  FormPower in '..\FrameVCL\FormPower.pas',
  FormProxyList in '..\FrameVCL\FormProxyList.pas',
  FormReminder in '..\FrameVCL\FormReminder.pas',
  FormRichLog in '..\FrameVCL\FormRichLog.pas',
  FormSkinsDisk in '..\FrameVCL\FormSkinsDisk.pas',
  FormSkinsRes in '..\FrameVCL\FormSkinsRes.pas',
  FormSplashScreen in '..\FrameVCL\FormSplashScreen.pas',
  FormTranslEditor in '..\FrameVCL\FormTranslEditor.pas',
  FormTranslSelector in '..\FrameVCL\FormTranslSelector.pas',
  FormUniversalEula in '..\FrameVCL\FormUniversalEula.pas',
  FrameExplorer in '..\FrameVCL\FrameExplorer.pas',
  LightVcl.Common.Translate in '..\FrameVCL\LightVcl.Common.Translate.pas';

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

  // Initialize AppData for tests that require it (e.g., FormAbout tests)
  AppData:= TAppData.Create('LightVclFormsTests');
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
