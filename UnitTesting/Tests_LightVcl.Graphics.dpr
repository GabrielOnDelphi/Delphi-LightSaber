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
  Test.LightVcl.Graph.FX.Rotate in 'Test.LightVcl.Graph.FX.Rotate.pas',
  LightVcl.Graph.FX.Rotate in '..\FrameVCL\LightVcl.Graph.FX.Rotate.pas',
  Test.LightVcl.Graph.Desktop in 'Test.LightVcl.Graph.Desktop.pas',
  LightVcl.Graph.Desktop in '..\FrameVCL\LightVcl.Graph.Desktop.pas',
  Test.LightVcl.Graph.UtilGray in 'Test.LightVcl.Graph.UtilGray.pas',
  LightVcl.Graph.UtilGray in '..\FrameVCL\LightVcl.Graph.UtilGray.pas',
  Test.LightVcl.Graph.Alpha in 'Test.LightVcl.Graph.Alpha.pas',
  LightVcl.Graph.Alpha in '..\FrameVCL\LightVcl.Graph.Alpha.pas',
  Test.LightVcl.Graph.Cache in 'Test.LightVcl.Graph.Cache.pas',
  LightVcl.Graph.Cache in '..\FrameVCL\LightVcl.Graph.Cache.pas',
  Test.LightVcl.Graph.Text in 'Test.LightVcl.Graph.Text.pas',
  LightVcl.Graph.Text in '..\FrameVCL\LightVcl.Graph.Text.pas',
  Test.LightVcl.Graph.ResizeWinBlt in 'Test.LightVcl.Graph.ResizeWinBlt.pas',
  LightVcl.Graph.ResizeWinBlt in '..\FrameVCL\LightVcl.Graph.ResizeWinBlt.pas',
  Test.LightVcl.Graph.Loader.Resolution in 'Test.LightVcl.Graph.Loader.Resolution.pas',
  LightVcl.Graph.Loader.Resolution in '..\FrameVCL\LightVcl.Graph.Loader.Resolution.pas',
  LightVcl.Graph.Util in '..\FrameVCL\LightVcl.Graph.Util.pas',
  LightVcl.Graph.Bitmap in '..\FrameVCL\LightVcl.Graph.Bitmap.pas',
  Test.LightVcl.Graph.Bitmap in 'Test.LightVcl.Graph.Bitmap.pas',
  LightVcl.Graph.BkgColorParams in '..\FrameVCL\LightVcl.Graph.BkgColorParams.pas',
  Test.LightVcl.Graph.BkgColorParams in 'Test.LightVcl.Graph.BkgColorParams.pas',
  LightVcl.Graph.BkgColor in '..\FrameVCL\LightVcl.Graph.BkgColor.pas',
  Test.LightVcl.Graph.BkgColor in 'Test.LightVcl.Graph.BkgColor.pas',
  LightVcl.Graph.Convert in '..\FrameVCL\LightVcl.Graph.Convert.pas',
  Test.LightVcl.Graph.Convert in 'Test.LightVcl.Graph.Convert.pas',
  LightVcl.Graph.Gif in '..\FrameVCL\LightVcl.Graph.Gif.pas',
  Test.LightVcl.Graph.Gif in 'Test.LightVcl.Graph.Gif.pas',
  LightVcl.Graph.GrabAviFrame in '..\FrameVCL\LightVcl.Graph.GrabAviFrame.pas',
  Test.LightVcl.Graph.GrabAviFrame in 'Test.LightVcl.Graph.GrabAviFrame.pas',
  LightVcl.Graph.Loader.Thread in '..\FrameVCL\LightVcl.Graph.Loader.Thread.pas',
  Test.LightVcl.Graph.Loader.Thread in 'Test.LightVcl.Graph.Loader.Thread.pas',
  LightVcl.Graph.ShadowText in '..\FrameVCL\LightVcl.Graph.ShadowText.pas',
  Test.LightVcl.Graph.ShadowText in 'Test.LightVcl.Graph.ShadowText.pas',
  LightVcl.Graph.FX.RotateGr32 in '..\FrameVCL\LightVcl.Graph.FX.RotateGr32.pas',
  Test.LightVcl.Graph.FX.RotateGr32 in 'Test.LightVcl.Graph.FX.RotateGr32.pas',
  LightVcl.Graph.ResizeParams in '..\FrameVCL\LightVcl.Graph.ResizeParams.pas',
  Test.LightVcl.Graph.ResizeParams in 'Test.LightVcl.Graph.ResizeParams.pas',
  LightVcl.Graph.ResizeParamFrame in '..\FrameVCL\LightVcl.Graph.ResizeParamFrame.pas',
  Test.LightVcl.Graph.ResizeParamFrame in 'Test.LightVcl.Graph.ResizeParamFrame.pas',
  LightVcl.Graph.ResizeVCL in '..\FrameVCL\LightVcl.Graph.ResizeVCL.pas',
  Test.LightVcl.Graph.ResizeVCL in 'Test.LightVcl.Graph.ResizeVCL.pas',
  LightVcl.Graph.ResizeGr32 in '..\FrameVCL\LightVcl.Graph.ResizeGr32.pas',
  Test.LightVcl.Graph.ResizeGr32 in 'Test.LightVcl.Graph.ResizeGr32.pas',
  LightVcl.Graph.ResizeFMX in '..\FrameVCL\LightVcl.Graph.ResizeFMX.pas',
  Test.LightVcl.Graph.ResizeFMX in 'Test.LightVcl.Graph.ResizeFMX.pas',
  LightVcl.Graph.ResizeWinGDI in '..\FrameVCL\LightVcl.Graph.ResizeWinGDI.pas',
  Test.LightVcl.Graph.ResizeWinGDI in 'Test.LightVcl.Graph.ResizeWinGDI.pas',
  LightVcl.Graph.ResizeWinThumb in '..\FrameVCL\LightVcl.Graph.ResizeWinThumb.pas',
  Test.LightVcl.Graph.ResizeWinThumb in 'Test.LightVcl.Graph.ResizeWinThumb.pas',
  LightVcl.Graph.ResizeWinWIC in '..\FrameVCL\LightVcl.Graph.ResizeWinWIC.pas',
  Test.LightVcl.Graph.ResizeWinWIC in 'Test.LightVcl.Graph.ResizeWinWIC.pas',
  LightVcl.Graph.Resize in '..\FrameVCL\LightVcl.Graph.Resize.pas',
  Test.LightVcl.Graph.Resize in 'Test.LightVcl.Graph.Resize.pas',
  Test.LightVcl.Graph.Util in 'Test.LightVcl.Graph.Util.pas',
  LightVcl.Graph.RainDropParams in '..\FrameVCL\LightVcl.Graph.RainDropParams.pas',
  LightVcl.Graph.RainShelter in '..\FrameVCL\LightVcl.Graph.RainShelter.pas',
  Test.LightVcl.Graph.RainShelter in 'Test.LightVcl.Graph.RainShelter.pas',
  LightVcl.Graph.Loader in '..\FrameVCL\LightVcl.Graph.Loader.pas',
  Test.LightVcl.Graph.Loader in 'Test.LightVcl.Graph.Loader.pas';

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
  TAppDataCore.TEST_MODE:= TRUE;   { Headless run: MesajGeneric returns 0 at once instead of putting a
                                     modal box on screen (LightVcl.Common.Dialogs.pas:96), and
                                     ShowModal/Show are bypassed. Without it a library routine that
                                     reports a missing file - there are 16 Assert(FileExistsMsg(..))
                                     calls in LightVcl.Graph.Loader.pas alone - stops the whole run
                                     dead until somebody clicks the box. Measured 2026-09-03: 15
                                     minutes frozen. Tests_LightFmx.dpr and Tests_LightVcl.Forms.dpr
                                     already did this; these five did not. }
  TRY

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
    FreeAndNil(AppData);
  END;
end.
