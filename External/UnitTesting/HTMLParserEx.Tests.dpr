program HTMLParserEx.Tests;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  {$IFDEF DEBUG}
  FastMM4,  { Must be the first unit: FastMM4 does not install once another unit has allocated memory }
  {$ENDIF}
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnit,
  {$ELSE}
  DUnitTestRunner,
  {$ENDIF }
  HTMLParserEx.Tests.Main in 'HTMLParserEx.Tests.Main.pas',
  HtmlParserEx in '..\HtmlParserEx.pas';

{$R *.RES}

begin
  {$IFDEF DEBUG}
  FastMM4.SuppressMessageBoxes:= TRUE;  { Leaks and heap errors go only to <exe>_MemoryManager_EventLog.txt; a message box would stop the unattended run }
  if IsConsole then
    if FastMM4.FastMM_GetInstallationState = FastMM4.mmisInstalled
    then Writeln('FastMM4 ', FastMM4.FastMMVersion, ': installed')
    else Writeln('FastMM4: NOT INSTALLED (FastMM_FullDebugMode.dll not found, or FastMM4 is not the first unit) - leaks and heap errors go undetected');
  {$ENDIF}

 {$IFDEF TESTINSIGHT}  //TestInsight IDE Plugin enabled
    TestInsight.DUnit.RunRegisteredTests;
  {$ELSE}  //Standard DUnit tests (typically from an automated build process)
    DUnitTestRunner.RunRegisteredTests;

    if IsConsole then
    begin
      {$WARN SYMBOL_PLATFORM OFF}
      if DebugHook <> 0 then //Running within the IDE
      begin
        // Allow developer to view console results
        Writeln('Hit any key to exit');
        Readln;
      end;
      {$WARN SYMBOL_PLATFORM ON}
    end;
  {$ENDIF}
end.
