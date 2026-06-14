unit Test.LightVcl.Common.ExecuteShell;

{=============================================================================================================
   Unit tests for LightVcl.Common.ExecuteShell.pas
   Tests ShellExecute-based process execution functions.

   Note: These tests execute real processes.
   Some functions (ExecuteURL, ExecuteSendEmail) are not tested as they open external apps.
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
      ExecuteFile('C:\NonExistent\Invalid.exe', '', FALSE);
    end,
    Exception);
end;


procedure TTestExecuteFile.Test_ExecuteFile_ValidPath;
VAR
  Success: Boolean;
begin
  { Execute cmd.exe with immediate exit }
  Success:= ExecuteFile(CmdExePath, '/c exit', FALSE, SW_HIDE);
  Assert.IsTrue(Success, 'Should successfully execute cmd.exe');
end;


procedure TTestExecuteFile.Test_ExecuteFile_NoErrorMsg;
begin
  { Non-existing file raises BEFORE any dialog could be shown }
  Assert.WillRaise(
    procedure
    begin
      ExecuteFile('C:\Invalid\Path.exe', '', FALSE, SW_HIDE);
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
  Success:= ExecuteFileEx(CmdExePath, '/c exit', FALSE, SW_HIDE);
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


procedure TTestExecuteFile.Test_ExecuteExplorerSelect_ValidFile;
VAR
  TestFile: string;
  Success: Boolean;
begin
  { Create a test file }
  TestFile:= TPath.Combine(FTestDir, 'testfile.txt');
  TFile.WriteAllText(TestFile, 'test');

  { This will open Explorer - just verify it doesn't crash }
  Success:= ExecuteExplorerSelect(TestFile);

  { Note: We can't easily verify Explorer opened, just that function returned }
  { The function should succeed if file exists }
  Assert.IsTrue(Success, 'Should successfully select file in Explorer');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestExecuteFile);

end.
