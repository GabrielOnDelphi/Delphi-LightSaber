unit Test.LightVcl.Common.Process;

{=============================================================================================================
   Unit tests for LightVcl.Common.Process.pas
   Tests process query and termination functions using TlHelp32 API.

   Note:
     - These tests interact with real system processes.
     - ProcessRunning tests use commonly available Windows processes (explorer.exe, csrss.exe).
     - KillProcess is tested against a dummy process to avoid killing important processes.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Winapi.Windows,
  LightVcl.Common.Process;

type
  [TestFixture]
  TTestProcess = class
  public
    { ProcessRunning tests }
    [Test]
    procedure Test_ProcessRunning_EmptyParam_RaisesException;

    [Test]
    procedure Test_ProcessRunning_NonExistentProcess_ReturnsFalse;

    [Test]
    procedure Test_ProcessRunning_ExplorerProcess_ReturnsTrue;

    [Test]
    procedure Test_ProcessRunning_CaseInsensitive;

    [Test]
    procedure Test_ProcessRunning_WithFullPath_ExtractsFilename;

    [Test]
    procedure Test_ProcessRunning_CurrentProcess_ReturnsTrue;

    { KillProcess tests }
    [Test]
    procedure Test_KillProcess_EmptyParam_RaisesException;

    [Test]
    procedure Test_KillProcess_NonExistentProcess_ReturnsTrue;

    [Test]
    procedure Test_KillProcess_ProtectedProcess_ReturnsFalse;
  end;

implementation


{ ProcessRunning tests }

procedure TTestProcess.Test_ProcessRunning_EmptyParam_RaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      ProcessRunning('');
    end,
    Exception);
end;


procedure TTestProcess.Test_ProcessRunning_NonExistentProcess_ReturnsFalse;
begin
  { A randomly named process should not exist }
  Assert.IsFalse(ProcessRunning('ThisProcessDefinitelyDoesNotExist_12345.exe'));
end;


procedure TTestProcess.Test_ProcessRunning_ExplorerProcess_ReturnsTrue;
begin
  { explorer.exe is almost always running on Windows desktop systems }
  Assert.IsTrue(ProcessRunning('explorer.exe'),
    'explorer.exe should be running on a Windows desktop system');
end;


procedure TTestProcess.Test_ProcessRunning_CaseInsensitive;
begin
  { Test case insensitivity - explorer.exe should match regardless of case }
  Assert.IsTrue(ProcessRunning('EXPLORER.EXE'),
    'ProcessRunning should be case-insensitive');
  Assert.IsTrue(ProcessRunning('Explorer.Exe'),
    'ProcessRunning should be case-insensitive');
end;


procedure TTestProcess.Test_ProcessRunning_WithFullPath_ExtractsFilename;
begin
  { Even when passing a full path, only the filename should be matched }
  Assert.IsTrue(ProcessRunning('C:\Windows\explorer.exe'),
    'Should find explorer.exe when full path is provided');
  Assert.IsTrue(ProcessRunning('D:\FakePath\explorer.exe'),
    'Should find explorer.exe even with non-existent path prefix');
end;


procedure TTestProcess.Test_ProcessRunning_CurrentProcess_ReturnsTrue;
VAR
  CurrentExeName: string;
begin
  { The test runner process itself should be running }
  CurrentExeName:= ExtractFileName(ParamStr(0));
  Assert.IsTrue(ProcessRunning(CurrentExeName),
    'Current process should be detected as running: ' + CurrentExeName);
end;


{ KillProcess tests }

procedure TTestProcess.Test_KillProcess_EmptyParam_RaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      KillProcess('');
    end,
    Exception);
end;


procedure TTestProcess.Test_KillProcess_NonExistentProcess_ReturnsTrue;
begin
  { Killing a non-existent process should return True (nothing to kill = success) }
  Assert.IsTrue(KillProcess('ThisProcessDefinitelyDoesNotExist_12345.exe'),
    'KillProcess should return True when no matching processes exist');
end;


procedure TTestProcess.Test_KillProcess_ProtectedProcess_ReturnsFalse;
begin
  { Attempting to kill a protected system process (like csrss.exe) should fail.
    Note: This test may behave differently depending on elevation level.
    On most systems, csrss.exe cannot be terminated without special privileges. }
  if ProcessRunning('csrss.exe')
  then Assert.IsFalse(KillProcess('csrss.exe'),
         'KillProcess should fail for protected system processes (csrss.exe)')
  else Assert.Pass('csrss.exe not found - skipping protected process test');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestProcess);

end.
