unit Test.LightVcl.Common.ExecuteProc;

{=============================================================================================================
   Unit tests for LightVcl.Common.ExecuteProc.pas
   Tests process execution functions using CreateProcess API.

   Note: These tests execute real processes (cmd.exe, notepad.exe).
   Some tests are designed to complete quickly to avoid long test runs.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  LightVcl.Common.ExecuteProc;

type
  [TestFixture]
  TTestExecuteProc = class
  public
    { ExecuteProc tests }
    [Test]
    procedure Test_ExecuteProc_EmptyPath;

    [Test]
    procedure Test_ExecuteProc_InvalidPath;

    [Test]
    procedure Test_ExecuteProc_ValidPath;

    { ExecuteAndWait tests }
    [Test]
    procedure Test_ExecuteAndWait_EmptyPath;

    [Test]
    procedure Test_ExecuteAndWait_SimpleCommand;

    [Test]
    procedure Test_ExecuteAndWait_WithParams;

    [Test]
    procedure Test_ExecuteAndWait_ExitCode;

    { ExecuteAndGetOut tests }
    [Test]
    procedure Test_ExecuteAndGetOut_EmptyCommand;

    [Test]
    procedure Test_ExecuteAndGetOut_Echo;

    [Test]
    procedure Test_ExecuteAndGetOut_Dir;

    [Test]
    procedure Test_ExecuteAndGetOut_MultiLineOutput;

    [Test]
    procedure Test_ExecuteAndGetOut_WorkingDir;

    { ExecuteAndGetOutDyn tests }
    [Test]
    procedure Test_ExecuteAndGetOutDyn_EmptyCommand;

    [Test]
    procedure Test_ExecuteAndGetOutDyn_Echo;

    [Test]
    procedure Test_ExecuteAndGetOutDyn_CallbackReceivesOutput;
  end;

implementation


{ ExecuteProc tests }

procedure TTestExecuteProc.Test_ExecuteProc_EmptyPath;
begin
  Assert.IsFalse(ExecuteProc(''));
end;


procedure TTestExecuteProc.Test_ExecuteProc_InvalidPath;
begin
  { Non-existent executable should fail }
  Assert.IsFalse(ExecuteProc('C:\NonExistent\Invalid.exe'));
end;


procedure TTestExecuteProc.Test_ExecuteProc_ValidPath;
VAR
  Success: Boolean;
begin
  { Launch a simple Windows command that exits immediately }
  Success:= ExecuteProc('cmd.exe /c exit', SW_HIDE);
  Assert.IsTrue(Success, 'Should successfully launch cmd.exe');
end;


{ ExecuteAndWait tests }

procedure TTestExecuteProc.Test_ExecuteAndWait_EmptyPath;
begin
  Assert.AreEqual(Cardinal(0), ExecuteAndWait(''));
end;


procedure TTestExecuteProc.Test_ExecuteAndWait_SimpleCommand;
VAR
  ExitCode: Cardinal;
begin
  { cmd.exe /c exit returns immediately with exit code 0 }
  ExitCode:= ExecuteAndWait('cmd.exe', '/c exit', TRUE, 5000);
  Assert.AreEqual(Cardinal(0), ExitCode);
end;


procedure TTestExecuteProc.Test_ExecuteAndWait_WithParams;
VAR
  ExitCode: Cardinal;
begin
  { Test with explicit parameters }
  ExitCode:= ExecuteAndWait('cmd.exe', '/c echo test', TRUE, 5000);
  Assert.AreEqual(Cardinal(0), ExitCode);
end;


procedure TTestExecuteProc.Test_ExecuteAndWait_ExitCode;
VAR
  ExitCode: Cardinal;
begin
  { cmd.exe /c exit 42 should return exit code 42 }
  ExitCode:= ExecuteAndWait('cmd.exe', '/c exit 42', TRUE, 5000);
  Assert.AreEqual(Cardinal(42), ExitCode);
end;


{ ExecuteAndGetOut tests }

procedure TTestExecuteProc.Test_ExecuteAndGetOut_EmptyCommand;
begin
  Assert.AreEqual('', ExecuteAndGetOut(''));
end;


procedure TTestExecuteProc.Test_ExecuteAndGetOut_Echo;
VAR
  Output: string;
begin
  Output:= ExecuteAndGetOut('echo Hello World');
  Assert.IsTrue(Output.Trim.Contains('Hello World'), 'Output should contain "Hello World"');
end;


procedure TTestExecuteProc.Test_ExecuteAndGetOut_Dir;
VAR
  Output: string;
begin
  { dir should produce some output }
  Output:= ExecuteAndGetOut('dir /b', 'C:\Windows');
  Assert.IsTrue(Length(Output) > 0, 'dir command should produce output');
  Assert.IsTrue(Output.Contains('System32') or Output.Contains('system32'),
    'Windows directory should contain System32');
end;


procedure TTestExecuteProc.Test_ExecuteAndGetOut_MultiLineOutput;
VAR
  Output: string;
  LineCount: Integer;
begin
  { echo with multiple lines }
  Output:= ExecuteAndGetOut('echo Line1 & echo Line2 & echo Line3');
  LineCount:= 0;
  if Output.Contains('Line1') then Inc(LineCount);
  if Output.Contains('Line2') then Inc(LineCount);
  if Output.Contains('Line3') then Inc(LineCount);
  Assert.AreEqual(3, LineCount, 'Should capture all three lines');
end;


procedure TTestExecuteProc.Test_ExecuteAndGetOut_WorkingDir;
VAR
  Output: string;
begin
  { cd should return current directory }
  Output:= ExecuteAndGetOut('cd', 'C:\Windows');
  Assert.IsTrue(Output.ToUpper.Contains('WINDOWS'), 'Working directory should be Windows');
end;


{ ExecuteAndGetOutDyn tests }

procedure TTestExecuteProc.Test_ExecuteAndGetOutDyn_EmptyCommand;
begin
  { Should not crash with empty command }
  ExecuteAndGetOutDyn('', NIL, FALSE, TRUE, 100);
  Assert.Pass('Did not crash with empty command');
end;


procedure TTestExecuteProc.Test_ExecuteAndGetOutDyn_Echo;
VAR
  ReceivedOutput: string;
begin
  ReceivedOutput:= '';

  ExecuteAndGetOutDyn('cmd.exe /c echo TestOutput',
    procedure(Text: string)
    begin
      ReceivedOutput:= ReceivedOutput + Text;
    end,
    FALSE, TRUE, 100);

  Assert.IsTrue(ReceivedOutput.Contains('TestOutput'),
    'Callback should receive echo output');
end;


procedure TTestExecuteProc.Test_ExecuteAndGetOutDyn_CallbackReceivesOutput;
VAR
  CallbackCount: Integer;
begin
  CallbackCount:= 0;

  ExecuteAndGetOutDyn('cmd.exe /c echo Line1 & echo Line2',
    procedure(Text: string)
    begin
      Inc(CallbackCount);
    end,
    FALSE, TRUE, 100);

  Assert.IsTrue(CallbackCount >= 1, 'Callback should be called at least once');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestExecuteProc);

end.
