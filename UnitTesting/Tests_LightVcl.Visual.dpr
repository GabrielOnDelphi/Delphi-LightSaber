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
  Test.LightVcl.Visual.ActivityIndicator in 'Test.LightVcl.Visual.ActivityIndicator.pas',
  LightVcl.Visual.CalendarCanvas in '..\FrameVCL\LightVcl.Visual.CalendarCanvas.pas',
  Test.LightVcl.Visual.CalendarCanvas in 'Test.LightVcl.Visual.CalendarCanvas.pas',
  LightVcl.Visual.CheckBox in '..\FrameVCL\LightVcl.Visual.CheckBox.pas',
  Test.LightVcl.Visual.CheckBox in 'Test.LightVcl.Visual.CheckBox.pas',
  LightVcl.Visual.Edit in '..\FrameVCL\LightVcl.Visual.Edit.pas',
  Test.LightVcl.Visual.Edit in 'Test.LightVcl.Visual.Edit.pas',
  LightVcl.Visual.ListBox in '..\FrameVCL\LightVcl.Visual.ListBox.pas',
  Test.LightVcl.Visual.ListBox in 'Test.LightVcl.Visual.ListBox.pas',
  LightVcl.Visual.Memo in '..\FrameVCL\LightVcl.Visual.Memo.pas',
  Test.LightVcl.Visual.Memo in 'Test.LightVcl.Visual.Memo.pas',
  LightVcl.Visual.MinimalPathLabel in '..\FrameVCL\LightVcl.Visual.MinimalPathLabel.pas',
  Test.LightVcl.Visual.MinimalPathLabel in 'Test.LightVcl.Visual.MinimalPathLabel.pas',
  LightVcl.Visual.Panel in '..\FrameVCL\LightVcl.Visual.Panel.pas',
  Test.LightVcl.Visual.Panel in 'Test.LightVcl.Visual.Panel.pas',
  LightVcl.Visual.PathEdit in '..\FrameVCL\LightVcl.Visual.PathEdit.pas',
  Test.LightVcl.Visual.PathEdit in 'Test.LightVcl.Visual.PathEdit.pas',
  LightVcl.Visual.RichEdit in '..\FrameVCL\LightVcl.Visual.RichEdit.pas',
  Test.LightVcl.Visual.RichEdit in 'Test.LightVcl.Visual.RichEdit.pas',
  LightVcl.Visual.RichEditResize in '..\FrameVCL\LightVcl.Visual.RichEditResize.pas',
  Test.LightVcl.Visual.RichEditResize in 'Test.LightVcl.Visual.RichEditResize.pas',
  LightVcl.Visual.RichLog in '..\FrameVCL\LightVcl.Visual.RichLog.pas',
  Test.LightVcl.Visual.RichLog in 'Test.LightVcl.Visual.RichLog.pas',
  LightVcl.Visual.RichLogTrack in '..\FrameVCL\LightVcl.Visual.RichLogTrack.pas',
  Test.LightVcl.Visual.RichLogTrack in 'Test.LightVcl.Visual.RichLogTrack.pas',
  LightVcl.Visual.RichLogUtils in '..\FrameVCL\LightVcl.Visual.RichLogUtils.pas',
  Test.LightVcl.Visual.RichLogUtils in 'Test.LightVcl.Visual.RichLogUtils.pas',
  LightVcl.Visual.RichRamLog in '..\FrameVCL\LightVcl.Visual.RichRamLog.pas',
  Test.LightVcl.Visual.RichRamLog in 'Test.LightVcl.Visual.RichRamLog.pas',
  LightVcl.Visual.Timer in '..\FrameVCL\LightVcl.Visual.Timer.pas',
  Test.LightVcl.Visual.Timer in 'Test.LightVcl.Visual.Timer.pas';

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
  TAppDataCore.Unattended:= TRUE;  { Nobody is at the keyboard: MesajGeneric returns 0 at once instead
                                     of putting a modal box on screen, and ShowModal/Show are bypassed.
                                     Without it one library routine that reports a failure with a box
                                     stops the whole run dead until somebody clicks it. }
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
    AppData.Free;    // DON'T use FreeAndNil — it nils the variable before the destructor runs, but form destructors (e.g. TfrmRamLog.SaveSettings) still need AppData during teardown. Same pattern as in LightVcl.Visual.AppData.pas finalization.
    AppData:= NIL;
  END;
end.
