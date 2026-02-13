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


{ ExecuteFile tests }

procedure TTestExecuteFile.Test_ExecuteFile_EmptyPath;
begin
  Assert.IsFalse(ExecuteFile(''));
end;


procedure TTestExecuteFile.Test_ExecuteFile_InvalidPath;
begin
  { Invalid path should fail - test with ShowErrorMsg=FALSE to avoid dialog }
  Assert.IsFalse(ExecuteFile('C:\NonExistent\Invalid.exe', '', FALSE));
end;


procedure TTestExecuteFile.Test_ExecuteFile_ValidPath;
VAR
  Success: Boolean;
begin
  { Execute cmd.exe with immediate exit }
  Success:= ExecuteFile('cmd.exe', '/c exit', FALSE, SW_HIDE);
  Assert.IsTrue(Success, 'Should successfully execute cmd.exe');
end;


procedure TTestExecuteFile.Test_ExecuteFile_NoErrorMsg;
begin
  { Test that ShowErrorMsg=FALSE doesn't show dialog }
  Assert.IsFalse(ExecuteFile('C:\Invalid\Path.exe', '', FALSE, SW_HIDE));
  Assert.Pass('No dialog was shown');
end;


{ ExecuteFileEx tests }

procedure TTestExecuteFile.Test_ExecuteFileEx_EmptyPath;
begin
  Assert.IsFalse(ExecuteFileEx(''));
end;


procedure TTestExecuteFile.Test_ExecuteFileEx_ValidPath;
VAR
  Success: Boolean;
begin
  Success:= ExecuteFileEx('cmd.exe', '/c exit', FALSE, SW_HIDE);
  Assert.IsTrue(Success, 'Should successfully execute cmd.exe via ShellExecuteEx');
end;


{ ExecuteFileAndWait tests }

procedure TTestExecuteFile.Test_ExecuteFileAndWait_EmptyPath;
begin
  Assert.IsFalse(ExecuteFileAndWait(''));
end;


procedure TTestExecuteFile.Test_ExecuteFileAndWait_SimpleCommand;
VAR
  Success: Boolean;
begin
  { Execute cmd.exe with immediate exit and wait }
  Success:= ExecuteFileAndWait('cmd.exe', '/c exit', TRUE, 5000);
  Assert.IsTrue(Success, 'Should successfully execute and wait for cmd.exe');
end;


{ ExecuteAsAdmin tests }

procedure TTestExecuteFile.Test_ExecuteAsAdmin_EmptyPath;
begin
  Assert.IsFalse(ExecuteAsAdmin(''));
end;

{ Note: Cannot test ExecuteAsAdmin with valid path as it triggers UAC dialog }


{ ExecuteExplorerSelect tests }

procedure TTestExecuteFile.Test_ExecuteExplorerSelect_EmptyPath;
begin
  Assert.IsFalse(ExecuteExplorerSelect(''));
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
