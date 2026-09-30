program Tests_LightCore;

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
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  {$ENDIF }
  DUnitX.TestFramework,
  { Test units - existing }
  Test.LightCore.EncodeCRC in 'Test.LightCore.EncodeCRC.pas',
  Test.LightCore.EncodeXOR in 'Test.LightCore.EncodeXOR.pas',
  Test.LightCore.StringList in 'Test.LightCore.StringList.pas',
  Test.LightCore.Internet in 'Test.LightCore.Internet.pas',
  Test.LightCore.Internet.CommonWebDown in 'Test.LightCore.Internet.CommonWebDown.pas',
  Test.LightCore.Internet.Email in 'Test.LightCore.Internet.Email.pas',
  Test.LightCore.StreamFile in 'Test.LightCore.StreamFile.pas',
  Test.LightCore.StreamMem in 'Test.LightCore.StreamMem.pas',
  Test.LightCore.Binary in 'Test.LightCore.Binary.pas',
  Test.LightCore.Math in 'Test.LightCore.Math.pas',
  Test.LightCore.Time in 'Test.LightCore.Time.pas',
  Test.LightCore.HTML in 'Test.LightCore.HTML.pas',
  Test.LightCore.Types in 'Test.LightCore.Types.pas',
  Test.LightCore.TextFile in 'Test.LightCore.TextFile.pas',
  Test.LightCore.StrBuilder in 'Test.LightCore.StrBuilder.pas',
  Test.LightCore.StreamBuff in 'Test.LightCore.StreamBuff.pas',
  { Test units - new }
  Test.LightCore.Core in 'Test.LightCore.Core.pas',
  Test.LightCore.AppData in 'Test.LightCore.AppData.pas',
  Test.LightCore.INIFile in 'Test.LightCore.INIFile.pas',
  Test.LightCore.INIFileQuick in 'Test.LightCore.INIFileQuick.pas',
  Test.LightCore.MRU in 'Test.LightCore.MRU.pas',
  Test.LightCore.IO in 'Test.LightCore.IO.pas',
  Test.LightCore.EncodeMime in 'Test.LightCore.EncodeMime.pas',
  Test.LightCore.Download in 'Test.LightCore.Download.pas',
  Test.LightCore.Download.Thread in 'Test.LightCore.Download.Thread.pas',
  Test.LightCore.LogTypes in 'Test.LightCore.LogTypes.pas',
  Test.LightCore.LogLinesS in 'Test.LightCore.LogLinesS.pas',
  Test.LightCore.LogLinesM in 'Test.LightCore.LogLinesM.pas',
  Test.LightCore.LogRam in 'Test.LightCore.LogRam.pas',
  Test.LightCore.Debugger in 'Test.LightCore.Debugger.pas',
  Test.LightCore.CmdLine in 'Test.LightCore.CmdLine.pas',
  Test.ciUpdaterRec in 'Test.ciUpdaterRec.pas',
  Test.LightCore.IOPlatformFile in 'Test.LightCore.IOPlatformFile.pas',
  Test.LightCore.Pascal in 'Test.LightCore.Pascal.pas',
  Test.LightCore.Platform in 'Test.LightCore.Platform.pas',
  Test.LightCore.Reports in 'Test.LightCore.Reports.pas',
  Test.LightCore.RttiSetToString in 'Test.LightCore.RttiSetToString.pas',
  Test.LightCore.SearchResult in 'Test.LightCore.SearchResult.pas',
  Test.LightCore.StringListA in 'Test.LightCore.StringListA.pas',
  Test.LightCore.WrapString in 'Test.LightCore.WrapString.pas',
  Test.LightCore.CompilerVersions in 'Test.LightCore.CompilerVersions.pas',
  Test.LightCore.CamUtils in 'Test.LightCore.CamUtils.pas',
  Test.LightCore.Graph.Loader.Resolution in 'Test.LightCore.Graph.Loader.Resolution.pas',
  Test.LightCore.Graph.ResizeParams in 'Test.LightCore.Graph.ResizeParams.pas',
  Test.LightCore.Graph.BkgColorParams in 'Test.LightCore.Graph.BkgColorParams.pas',
  Test.LightCore.Graph.RainDropParams in 'Test.LightCore.Graph.RainDropParams.pas',
  Test.LightCore.WinVersion in 'Test.LightCore.WinVersion.pas',
  {$IFDEF MSWINDOWS}
  Test.LightCore.Process in 'Test.LightCore.Process.pas',
  Test.LightCore.ExeVersion in 'Test.LightCore.ExeVersion.pas',
  Test.LightCore.Sound in 'Test.LightCore.Sound.pas',
  Test.LightCore.Win.Sound in 'Test.LightCore.Win.Sound.pas',
  Test.LightCore.Win.Registry in 'Test.LightCore.Win.Registry.pas',
  Test.LightCore.Keyboard in 'Test.LightCore.Keyboard.pas',
  Test.LightCore.EnvironmentVar in 'Test.LightCore.EnvironmentVar.pas',
  Test.LightCore.Win.EnvironmentVar in 'Test.LightCore.Win.EnvironmentVar.pas',
  Test.LightCore.Win.Download in 'Test.LightCore.Win.Download.pas',
  Test.LightCore.SystemPermissions in 'Test.LightCore.SystemPermissions.pas',
  Test.LightCore.Win.SystemPermissions in 'Test.LightCore.Win.SystemPermissions.pas',
  Test.LightCore.Win.WinVersionApi in 'Test.LightCore.Win.WinVersionApi.pas',
  {$ENDIF}
  { Source units }
  LightCore in '..\LightCore.pas',
  LightCore.Types in '..\LightCore.Types.pas',
  LightCore.EncodeCRC in '..\LightCore.EncodeCRC.pas',
  LightCore.EncodeXOR in '..\LightCore.EncodeXOR.pas',
  LightCore.StringList in '..\LightCore.StringList.pas',
  LightCore.Internet in '..\LightCore.Internet.pas',
  LightCore.Internet.CommonWebDown in '..\LightCore.Internet.CommonWebDown.pas',
  LightCore.Internet.Email in '..\LightCore.Internet.Email.pas',
  LightCore.StreamFile in '..\LightCore.StreamFile.pas',
  LightCore.Binary in '..\LightCore.Binary.pas',
  LightCore.IO in '..\LightCore.IO.pas',
  LightCore.HTML in '..\LightCore.HTML.pas',
  LightCore.Download in '..\LightCore.Download.pas',
  LightCore.Download.Thread in '..\LightCore.Download.Thread.pas',
  {$IFDEF MSWINDOWS}
  LightCore.Win.Download in '..\LightCore.Win.Download.pas',
  {$ENDIF}
  LightCore.StreamMem in '..\LightCore.StreamMem.pas',
  LightCore.Math in '..\LightCore.Math.pas',
  LightCore.Time in '..\LightCore.Time.pas',
  LightCore.TextFile in '..\LightCore.TextFile.pas',
  LightCore.StrBuilder in '..\LightCore.StrBuilder.pas',
  LightCore.StreamBuff in '..\LightCore.StreamBuff.pas',
  LightCore.AppData in '..\LightCore.AppData.pas',
  LightCore.INIFile in '..\LightCore.INIFile.pas',
  LightCore.INIFileQuick in '..\LightCore.INIFileQuick.pas',
  LightCore.MRU in '..\LightCore.MRU.pas',
  LightCore.EncodeMime in '..\LightCore.EncodeMime.pas',
  LightCore.LogTypes in '..\LightCore.LogTypes.pas',
  LightCore.LogLinesAbstract in '..\LightCore.LogLinesAbstract.pas',
  LightCore.LogLinesS in '..\LightCore.LogLinesS.pas',
  LightCore.LogLinesM in '..\LightCore.LogLinesM.pas',
  LightCore.LogRam in '..\LightCore.LogRam.pas',
  LightCore.CompilerVersions in '..\LightCore.CompilerVersions.pas',
  LightCore.Debugger in '..\LightCore.Debugger.pas',
  LightCore.CmdLine in '..\LightCore.CmdLine.pas',
  LightCore.Platform in '..\LightCore.Platform.pas',
  LightCore.IOPlatformFile in '..\LightCore.IOPlatformFile.pas',
  LightCore.Pascal in '..\LightCore.Pascal.pas',
  LightCore.Reports in '..\LightCore.Reports.pas',
  LightCore.RttiSetToString in '..\LightCore.RttiSetToString.pas',
  LightCore.SearchResult in '..\LightCore.SearchResult.pas',
  LightCore.StringListA in '..\LightCore.StringListA.pas',
  LightCore.WrapString in '..\LightCore.WrapString.pas',
  LightCore.CamUtils in '..\LightCore.CamUtils.pas',
  LightCore.Graph.Loader.Resolution in '..\LightCore.Graph.Loader.Resolution.pas',
  LightCore.Graph.ResizeParams in '..\LightCore.Graph.ResizeParams.pas',
  LightCore.Graph.BkgColorParams in '..\LightCore.Graph.BkgColorParams.pas',
  LightCore.Graph.RainDropParams in '..\LightCore.Graph.RainDropParams.pas',
  LightCore.WinVersion in '..\LightCore.WinVersion.pas',
  {$IFDEF MSWINDOWS}
  LightCore.Win.WinVersionApi in '..\LightCore.Win.WinVersionApi.pas',
  {$ENDIF}
  LightCore.Process in '..\LightCore.Process.pas',
  LightCore.ExeVersion in '..\LightCore.ExeVersion.pas',
  LightCore.Sound in '..\LightCore.Sound.pas',
  {$IFDEF MSWINDOWS}
  LightCore.Win.Sound in '..\LightCore.Win.Sound.pas',
  LightCore.Win.Registry in '..\LightCore.Win.Registry.pas',
  {$ENDIF}
  LightCore.Keyboard in '..\LightCore.Keyboard.pas',
  LightCore.EnvironmentVar in '..\LightCore.EnvironmentVar.pas',
  {$IFDEF MSWINDOWS}
  LightCore.Win.EnvironmentVar in '..\LightCore.Win.EnvironmentVar.pas',
  {$ENDIF}
  LightCore.SystemPermissions in '..\LightCore.SystemPermissions.pas',
  {$IFDEF MSWINDOWS}
  LightCore.Win.SystemPermissions in '..\LightCore.Win.SystemPermissions.pas',
  {$ENDIF}
  ciUpdaterRec in '..\Updater\ciUpdaterRec.pas';

{$IFNDEF TESTINSIGHT}
var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger: ITestLogger;
{$ENDIF}

begin
  ReportMemoryLeaksOnShutdown:= True;

  { Initialize AppDataCore - required by TLightStream and other units that use logging }
  AppDataCore:= TAppDataCore.Create('LightCoreTests');
  TAppDataCore.Unattended:= TRUE;  { Nobody is at the keyboard: MesajGeneric returns 0 at once instead
                                     of putting a modal box on screen, and ShowModal/Show are bypassed.
                                     Without it one library routine that reports a failure with a box
                                     stops the whole run dead until somebody clicks it. }
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

      runner.FailsOnNoAsserts := FALSE;

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
    FreeAndNil(AppDataCore);
  END;
end.
