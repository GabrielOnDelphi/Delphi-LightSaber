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
  { Test units }
  Test.LightFmx.Common.AppData        in 'Test.LightFmx.Common.AppData.pas',
  Test.LightFmx.Common.AppData.Form   in 'Test.LightFmx.Common.AppData.Form.pas',
  Test.LightFmx.Visual.SvgFlatButton  in 'Test.LightFmx.Visual.SvgFlatButton.pas',
  Test.LightFmx.Common.Dialogs        in 'Test.LightFmx.Common.Dialogs.pas',
  Test.LightFmx.Visual.AutoSizeBoxImg in 'Test.LightFmx.Visual.AutoSizeBoxImg.pas',
  Test.LightFmx.Common.CenterControl  in 'Test.LightFmx.Common.CenterControl.pas',
  Test.LightFmx.Common.Helpers        in 'Test.LightFmx.Common.Helpers.pas',
  Test.LightFmx.Common.IniFile        in 'Test.LightFmx.Common.IniFile.pas',
  Test.LightFmx.Common.LogFilter      in 'Test.LightFmx.Common.LogFilter.pas',
  Test.LightFmx.Common.LogViewer      in 'Test.LightFmx.Common.LogViewer.pas',
  Test.LightFmx.Graph                 in 'Test.LightFmx.Graph.pas',
  Test.LightFmx.LogForm               in 'Test.LightFmx.LogForm.pas',
  Test.LightFmx.Visual.AnimatedMemo   in 'Test.LightFmx.Visual.AnimatedMemo.pas',
  Test.LightFmx.Visual.Animations     in 'Test.LightFmx.Visual.Animations.pas',
  Test.LightFmx.Visual.AutoSizeBox    in 'Test.LightFmx.Visual.AutoSizeBox.pas',
  Test.LightFmx.Visual.ComboBox       in 'Test.LightFmx.Visual.ComboBox.pas',
  Test.LightFmx.Visual.DropDownSearch in 'Test.LightFmx.Visual.DropDownSearch.pas',
  Test.LightFmx.Visual.LabeledEdit    in 'Test.LightFmx.Visual.LabeledEdit.pas',
  Test.LightFmx.Visual.Layout         in 'Test.LightFmx.Visual.Layout.pas',
  Test.LightFmx.Visual.Panel          in 'Test.LightFmx.Visual.Panel.pas',
  { Source units }
  LightCore.AppData                   in '..\LightCore.AppData.pas',
  LightCore.IO                        in '..\LightCore.IO.pas',
  LightFmx.Common.AppData             in '..\FrameFMX\LightFmx.Common.AppData.pas',
  LightFmx.Common.AppData.Form        in '..\FrameFMX\LightFmx.Common.AppData.Form.pas',
  LightFmx.Common.CamUtils            in '..\FrameFMX\LightFmx.Common.CamUtils.pas',
  LightFmx.Common.Styles              in '..\FrameFMX\LightFmx.Common.Styles.pas',
  LightFmx.Common.Screen              in '..\FrameFMX\LightFmx.Common.Screen.pas',
  LightFmx.Common.Dialogs             in '..\FrameFMX\LightFmx.Common.Dialogs.pas',
  LightFmx.Common.Graph               in '..\FrameFMX\LightFmx.Common.Graph.pas',
  LightFmx.Visual.SvgFlatButton       in '..\FrameFMX\LightFmx.Visual.SvgFlatButton.pas',
  LightFmx.Visual.AutoSizeBox         in '..\FrameFMX\LightFmx.Visual.AutoSizeBox.pas',
  LightFmx.Visual.AutoSizeBoxImg      in '..\FrameFMX\LightFmx.Visual.AutoSizeBoxImg.pas',
  LightFmx.Visual.ScreenCapture       in '..\FrameFMX\LightFmx.Visual.ScreenCapture.pas',
  FormScreenCapture                   in '..\FrameFMX\FormScreenCapture.pas',
  Test.FormScreenCapture              in 'Test.FormScreenCapture.pas';

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

  // AppData is required by the TAppData/TLightForm tests. Freed in LightFmx.Common.AppData's FINALIZATION.
  AppData:= TAppData.Create('LightFmxTests');
  TAppDataCore.Unattended:= TRUE;   // Nobody at the keyboard: bypass ShowModal/Show

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
