program Tests_LightVcl.Translator;

{=====================================================
   Unit Tests for Translation System

   Tests:
     - FormTranslEditor (Translation Editor Form)
     - FormTranslSelector (Language Selector Form)
     - LightVcl.Common.TranslatorAPI (DeepL API Client)
     - LightVcl.Common.Translate (Translation Engine)

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
  LightVcl.Common.IO in '..\FrameVCL\LightVcl.Common.IO.pas',

  { Units under test }
  LightVcl.Common.Translate in '..\FrameVCL\LightVcl.Common.Translate.pas',
  LightVcl.Common.TranslatorAPI in '..\FrameVCL\LightVcl.Common.TranslatorAPI.pas',
  FormTranslEditor in '..\FrameVCL\FormTranslEditor.pas',
  FormTranslSelector in '..\FrameVCL\FormTranslSelector.pas',
  FormDeepLSettings in '..\FrameVCL\FormDeepLSettings.pas',

  { Test units }
  Test.FormTranslEditor in 'Test.FormTranslEditor.pas',
  Test.FormTranslSelector in 'Test.FormTranslSelector.pas',
  Test.LightVcl.Common.TranslatorAPI in 'Test.LightVcl.Common.TranslatorAPI.pas';

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
  AppData:= TAppData.Create('LightTranslatorTests');
  TAppData.TEST_MODE:= TRUE;  // Bypass ShowModal/Show calls in tests
  TRY

  // Create a dummy main form - required by AppData.CreateFormHidden
  Application.CreateForm(TForm, MainForm);
  MainForm.Visible:= FALSE;

  // Initialize Translator for form tests
  Translator:= TTranslator.Create(AppData);

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

    runner.FailsOnNoAsserts:= False;

    // Run tests
    results:= runner.Execute;
    if not results.AllPassed
    then System.ExitCode:= EXIT_ERRORS;

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
    FreeAndNil(Translator);
    FreeAndNil(MainForm);
    FreeAndNil(AppData);
  END;
end.
