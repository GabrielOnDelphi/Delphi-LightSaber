unit Test.LightVcl.Common.ExecuteShell;

{=============================================================================================================
   Unit tests for LightVcl.Common.ExecuteShell.pas
   Tests ShellExecute-based process execution functions.

   Note: These tests execute real processes.
   Some functions (ExecuteURL, ExecuteSendEmail) are not tested as they open external apps.
   Everything launched by default is hidden (cmd.exe with SW_HIDE) or is a raise-path, so
   the suite can run unattended. The one test that would open a VISIBLE window
   (Test_ExecuteExplorerSelect_ValidFile) is behind the INTERACTIVE_TESTS define.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  Winapi.Windows,
  LightVcl.Common.ExecuteShell;

type
  [TestFixture]
  TTestExecuteFile = class
  private
    FTestDir: string;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { ExecuteFile tests }
    [Test]
    procedure Test_ExecuteFile_EmptyPath;

    [Test]
    procedure Test_ExecuteFile_InvalidPath;

    [Test]
    procedure Test_ExecuteFile_ValidPath;

    [Test]
    procedure Test_ExecuteFile_NoErrorMsg;

    { ExecuteFileEx tests }
    [Test]
    procedure Test_ExecuteFileEx_EmptyPath;

    [Test]
    procedure Test_ExecuteFileEx_ValidPath;

    { ExecuteFileAndWait tests }
    [Test]
    procedure Test_ExecuteFileAndWait_EmptyPath;

    [Test]
    procedure Test_ExecuteFileAndWait_SimpleCommand;

    { ExecuteAsAdmin tests - limited due to UAC }
    [Test]
    procedure Test_ExecuteAsAdmin_EmptyPath;

    { ExecuteExplorerSelect tests }
    [Test]
    procedure Test_ExecuteExplorerSelect_EmptyPath;

    [Test]
    procedure Test_ExecuteExplorerSelect_NonExistentFile;

    [Test]
    procedure Test_ExecuteExplorerSelect_ValidFile;
  end;

implementation


procedure TTestExecuteFile.Setup;
begin
  FTestDir:= TPath.Combine(TPath.GetTempPath, 'ExecuteFileTest_' + TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FTestDir);
end;


procedure TTestExecuteFile.TearDown;
begin
  if TDirectory.Exists(FTestDir)
  then TDirectory.Delete(FTestDir, True);
end;


{ Full path to cmd.exe. The Execute* functions raise when the file does not exist,
  so the relative 'cmd.exe' (resolved by the shell, not by FileExists) cannot be used anymore. }
function CmdExePath: string;
begin
  Result:= System.SysUtils.GetEnvironmentVariable('ComSpec');   { Qualified: Winapi.Windows also exports GetEnvironmentVariable }
end;


{ ExecuteFile tests }

procedure TTestExecuteFile.Test_ExecuteFile_EmptyPath;
begin
  { ExecuteFile raises on a non-existing file (guard added in the 2026.01 hardening) }
  Assert.WillRaise(
    procedure
    begin
      ExecuteFile('');
    end,
    Exception);
end;


procedure TTestExecuteFile.Test_ExecuteFile_InvalidPath;
begin
  Assert.WillRaise(
    procedure
    begin
      ExecuteFile('C:\NonExistent\Invalid.exe', '');
    end,
    Exception);
end;


procedure TTestExecuteFile.Test_ExecuteFile_ValidPath;
VAR
  Success: Boolean;
begin
  { Execute cmd.exe with immediate exit }
  Success:= ExecuteFile(CmdExePath, '/c exit', SW_HIDE);
  Assert.IsTrue(Success, 'Should successfully execute cmd.exe');
end;


procedure TTestExecuteFile.Test_ExecuteFile_NoErrorMsg;
begin
  { Same as Test_ExecuteFile_InvalidPath, but with an explicit WindowState: the FileExists guard
    raises before ShellExecute is reached, whatever the window state asks for. }
  Assert.WillRaise(
    procedure
    begin
      ExecuteFile('C:\Invalid\Path.exe', '', SW_HIDE);
    end,
    Exception);
end;


{ ExecuteFileEx tests }

procedure TTestExecuteFile.Test_ExecuteFileEx_EmptyPath;
begin
  Assert.WillRaise(
    procedure
    begin
      ExecuteFileEx('');
    end,
    Exception);
end;


procedure TTestExecuteFile.Test_ExecuteFileEx_ValidPath;
VAR
  Success: Boolean;
begin
  Success:= ExecuteFileEx(CmdExePath, '/c exit', SW_HIDE);
  Assert.IsTrue(Success, 'Should successfully execute cmd.exe via ShellExecuteEx');
end;


{ ExecuteFileAndWait tests }

procedure TTestExecuteFile.Test_ExecuteFileAndWait_EmptyPath;
begin
  Assert.WillRaise(
    procedure
    begin
      ExecuteFileAndWait('');
    end,
    Exception);
end;


procedure TTestExecuteFile.Test_ExecuteFileAndWait_SimpleCommand;
VAR
  Success: Boolean;
begin
  { Execute cmd.exe with immediate exit and wait }
  Success:= ExecuteFileAndWait(CmdExePath, '/c exit', TRUE, 5000);
  Assert.IsTrue(Success, 'Should successfully execute and wait for cmd.exe');
end;


{ ExecuteAsAdmin tests }

procedure TTestExecuteFile.Test_ExecuteAsAdmin_EmptyPath;
begin
  Assert.WillRaise(
    procedure
    begin
      ExecuteAsAdmin('');
    end,
    Exception);
end;

{ Note: Cannot test ExecuteAsAdmin with valid path as it triggers UAC dialog }


{ ExecuteExplorerSelect tests }

procedure TTestExecuteFile.Test_ExecuteExplorerSelect_EmptyPath;
begin
  Assert.WillRaise(
    procedure
    begin
      ExecuteExplorerSelect('');
    end,
    Exception);
end;


procedure TTestExecuteFile.Test_ExecuteExplorerSelect_NonExistentFile;
begin
  { Covers the FileExists guard with a NON-empty path. Test_..._EmptyPath alone does not:
    '' would be rejected by ILCreateFromPath further down even if the guard were removed,
    so only a well-formed path to a missing file proves the guard itself still fires. }
  Assert.WillRaise(
    procedure
    begin
      ExecuteExplorerSelect(TPath.Combine(FTestDir, 'no-such-file.txt'));
    end,
    Exception);
end;


procedure TTestExecuteFile.Test_ExecuteExplorerSelect_ValidFile;
{$IFDEF INTERACTIVE_TESTS}
VAR
  TestFile: string;
  Success: Boolean;
{$ENDIF}
begin
  { OPT-IN: this one opens a real Explorer window.

    ExecuteExplorerSelect ends in SHOpenFolderAndSelectItems, so there is no way to reach
    its success path without a window appearing - and that window ACTIVATES itself. During
    an unattended run it steals the keyboard focus from whatever is being typed elsewhere,
    and TearDown then deletes the very folder it is left showing. Same reasoning as the
    ExecuteAsAdmin note above (UAC dialog), which is already excluded for its side effect.

    The raise-path is still covered unconditionally by the two tests above.

    To run it deliberately, compile the test project with the define:
      delphi-compiler.exe Tests_LightVcl.Common.dproj --define=INTERACTIVE_TESTS }
  {$IFDEF INTERACTIVE_TESTS}
  TestFile:= TPath.Combine(FTestDir, 'testfile.txt');
  TFile.WriteAllText(TestFile, 'test');

  Success:= ExecuteExplorerSelect(TestFile);
  Assert.IsTrue(Success, 'Should successfully select file in Explorer');
  {$ELSE}
  Assert.Pass('Skipped: opens a real Explorer window and steals keyboard focus. Compile with INTERACTIVE_TESTS to run it.');
  {$ENDIF}
end;


initialization
  TDUnitX.RegisterTestFixture(TTestExecuteFile);

end.
