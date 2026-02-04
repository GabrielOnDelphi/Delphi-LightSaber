program HTMLParserEx.Tests;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnit,
  {$ELSE}
  DUnitTestRunner,
  {$ENDIF }
  HTMLParserEx.Tests.Main in 'HTMLParserEx.Tests.Main.pas',
  HtmlParserEx in '..\HtmlParserEx.pas';

{$R *.RES}

begin
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
