UNIT Test.RunnerChecks;

{=============================================================================================================
   Checks shared by every LightSaber test runner (UnitTesting\Tests_*.dpr and FrameVCL\AutoTranslator\UnitTesting\Tests_*.dpr).

   The fixture check.
     A fixture is a sample file or folder on disk that a test reads.
     A runner calls FixturesMissing once, before it runs the tests, with the paths its tests read (relative to the EXE folder).
     Each missing path is printed. The runner then exits with EXIT_FIXTURES_MISSING, unless the command line holds
     --allow-missing-fixtures.
     Why a runner and not the test: DUnitX cannot mark a test Ignored at run time, and an Assert.Pass skip counts as passed.

   The switch --allow-missing-fixtures.
     DUnitX rejects an option it does not know: TCommandLineParser.InternalParse adds "unknown option"
     (c:\Delphi\Delphi 13\source\DunitX\DUnitX.CommandLine.Parser.pas:265), and TDUnitX.CheckCommandLine then sets
     EXIT_OPTIONS_ERROR and raises ECommandLineError (DUnitX.TestFramework.pas:738-739).
     So the switch is registered in the initialization section of this unit, which runs before the runner calls CheckCommandLine.
     Every runner links this unit, so the run script can pass the switch to every test EXE.
=============================================================================================================}

INTERFACE

CONST
  EXIT_FIXTURES_MISSING = 2;   { DUnitX uses 0 (EXIT_OK), 1 (EXIT_ERRORS) and 100 (EXIT_OPTIONS_ERROR): DUnitX.TestFramework.pas:619-621 }

VAR
  AllowMissingFixtures: Boolean = FALSE;   { TRUE when --allow-missing-fixtures is on the command line }

function FixturesMissing(CONST Paths: array of string): Boolean;

IMPLEMENTATION

USES
  System.SysUtils,
  DUnitX.CommandLine.Options;


{ Returns TRUE when at least one path (file or folder, relative to the EXE folder) does not exist. Prints each missing one. }
function FixturesMissing(CONST Paths: array of string): Boolean;
VAR
  Path, FullPath: string;
begin
  Result:= FALSE;
  for Path in Paths do
    begin
      FullPath:= ExpandFileName(ExtractFilePath(ParamStr(0)) + Path);
      if NOT FileExists(FullPath) AND NOT DirectoryExists(FullPath) then
        begin
          Result:= TRUE;
          System.Writeln('MISSING FIXTURE: ', FullPath);
        end;
    end;

  if Result then
    if AllowMissingFixtures
    then System.Writeln('Fixtures are missing, but --allow-missing-fixtures is on: the exit code ignores them')
    else System.Writeln('Fixtures are missing: the run fails with exit code ', EXIT_FIXTURES_MISSING, ' (pass --allow-missing-fixtures to allow it)');
end;


{ The shape of the DUnitX option 'dontshowignored' (DUnitX.OptionsDefinition.pas:90-95): a switch with no value. }
procedure RegisterAllowMissingFixtures;
VAR
  Def: IOptionDefinition;
begin
  Def:= TOptionsRegistry.RegisterOption<Boolean>('allow-missing-fixtures', 'amf', 'Do not fail the run when a fixture folder or file is missing',
          procedure(Value: Boolean)
          begin
            AllowMissingFixtures:= TRUE;
          end);
  Def.HasValue:= FALSE;
end;


INITIALIZATION
  RegisterAllowMissingFixtures;

end.
