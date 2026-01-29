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
  TTestExecuteShell = class
  private
    FTestDir: string;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { ExecuteShell tests }
    [Test]
    procedure Test_ExecuteShell_EmptyPath;

    [Test]
    procedure Test_ExecuteShell_InvalidPath;

    [Test]
    procedure Test_ExecuteShell_ValidPath;

    [Test]
    procedure Test_ExecuteShell_NoErrorMsg;

    { ExecuteShellEx tests }
    [Test]
    procedure Test_ExecuteShellEx_EmptyPath;

    [Test]
    procedure Test_ExecuteShellEx_ValidPath;

    { ExecuteShellAndWait tests }
    [Test]
    procedure Test_ExecuteShellAndWait_EmptyPath;

    [Test]
    procedure Test_ExecuteShellAndWait_SimpleCommand;

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


procedure TTestExecuteShell.Setup;
begin
  FTestDir:= TPath.Combine(TPath.GetTempPath, 'ExecuteShellTest_' + TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FTestDir);
end;


procedure TTestExecuteShell.TearDown;
begin
  if TDirectory.Exists(FTestDir)
  then TDirectory.Delete(FTestDir, True);
end;


{ ExecuteShell tests }

procedure TTestExecuteShell.Test_ExecuteShell_EmptyPath;
begin
  Assert.IsFalse(ExecuteShell(''));
end;


procedure TTestExecuteShell.Test_ExecuteShell_InvalidPath;
begin
  { Invalid path should fail - test with ShowErrorMsg=FALSE to avoid dialog }
  Assert.IsFalse(ExecuteShell('C:\NonExistent\Invalid.exe', '', FALSE));
end;


procedure TTestExecuteShell.Test_ExecuteShell_ValidPath;
VAR
  Success: Boolean;
begin
  { Execute cmd.exe with immediate exit }
  Success:= ExecuteShell('cmd.exe', '/c exit', FALSE, SW_HIDE);
  Assert.IsTrue(Success, 'Should successfully execute cmd.exe');
end;


procedure TTestExecuteShell.Test_ExecuteShell_NoErrorMsg;
begin
  { Test that ShowErrorMsg=FALSE doesn't show dialog }
  Assert.IsFalse(ExecuteShell('C:\Invalid\Path.exe', '', FALSE, SW_HIDE));
  Assert.Pass('No dialog was shown');
end;


{ ExecuteShellEx tests }

procedure TTestExecuteShell.Test_ExecuteShellEx_EmptyPath;
begin
  Assert.IsFalse(ExecuteShellEx(''));
end;


procedure TTestExecuteShell.Test_ExecuteShellEx_ValidPath;
VAR
  Success: Boolean;
begin
  Success:= ExecuteShellEx('cmd.exe', '/c exit', FALSE, SW_HIDE);
  Assert.IsTrue(Success, 'Should successfully execute cmd.exe via ShellExecuteEx');
end;


{ ExecuteShellAndWait tests }

procedure TTestExecuteShell.Test_ExecuteShellAndWait_EmptyPath;
begin
  Assert.IsFalse(ExecuteShellAndWait(''));
end;


procedure TTestExecuteShell.Test_ExecuteShellAndWait_SimpleCommand;
VAR
  Success: Boolean;
begin
  { Execute cmd.exe with immediate exit and wait }
  Success:= ExecuteShellAndWait('cmd.exe', '/c exit', TRUE, 5000);
  Assert.IsTrue(Success, 'Should successfully execute and wait for cmd.exe');
end;


{ ExecuteAsAdmin tests }

procedure TTestExecuteShell.Test_ExecuteAsAdmin_EmptyPath;
begin
  Assert.IsFalse(ExecuteAsAdmin(''));
end;

{ Note: Cannot test ExecuteAsAdmin with valid path as it triggers UAC dialog }


{ ExecuteExplorerSelect tests }

procedure TTestExecuteShell.Test_ExecuteExplorerSelect_EmptyPath;
begin
  Assert.IsFalse(ExecuteExplorerSelect(''));
end;


procedure TTestExecuteShell.Test_ExecuteExplorerSelect_ValidFile;
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
  TDUnitX.RegisterTestFixture(TTestExecuteShell);

end.
