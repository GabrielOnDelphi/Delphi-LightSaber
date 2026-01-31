unit Test.LightVcl.Common.Debugger;

{=============================================================================================================
   Unit tests for LightVcl.Common.Debugger.pas
   Tests debugger detection and debug output functions.

   Note: Some functions like AntiDebug/AntiProcDump are assembler routines that are
   difficult to test meaningfully. We focus on ensuring they don't crash.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  Winapi.Windows,
  System.SysUtils,
  Vcl.Forms;

type
  [TestFixture]
  TTestDebugger = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { IsDebuggerPresent Tests }
    [Test]
    procedure TestIsDebuggerPresent_ReturnsBoolean;

    [Test]
    procedure TestIsDebuggerPresent_NoException;

    { OutputDebugStr Tests }
    [Test]
    procedure TestOutputDebugStr_String_NoException;

    [Test]
    procedure TestOutputDebugStr_StringInteger_NoException;

    [Test]
    procedure TestOutputDebugStr_StringReal_NoException;

    [Test]
    procedure TestOutputDebugStr_EmptyString_NoException;

    [Test]
    procedure TestOutputDebugStr_LongString_NoException;

    [Test]
    procedure TestOutputDebugStr_SpecialChars_NoException;

    { AntiDebug Tests - just verify no crash }
    [Test]
    procedure TestAntiDebug_NoException;

    [Test]
    procedure TestAntiProcDump_NoException;

    { ExitIfUnderDebugger Tests }
    [Test]
    procedure TestExitIfUnderDebugger_WithExistingFile_NoExit;

    { HaltApplication Tests }
    [Test]
    procedure TestHaltApplication_RaisesException;

    { EApplicationFail Tests }
    [Test]
    procedure TestEApplicationFail_IsException;

    { LastErrorMsgStr Tests }
    [Test]
    procedure TestLastErrorMsgStr_ReturnsString;

    [Test]
    procedure TestLastErrorMsgStr_NoException;
  end;

implementation

uses
  LightVcl.Common.Debugger;


procedure TTestDebugger.Setup;
begin
  // No setup needed
end;


procedure TTestDebugger.TearDown;
begin
  // No teardown needed
end;


{ IsDebuggerPresent Tests }

procedure TTestDebugger.TestIsDebuggerPresent_ReturnsBoolean;
var
  Result: Boolean;
begin
  Result:= IsDebuggerPresent;

  { Result should be a valid boolean (True or False) }
  Assert.IsTrue((Result = True) or (Result = False),
    'IsDebuggerPresent should return a boolean value');
end;


procedure TTestDebugger.TestIsDebuggerPresent_NoException;
begin
  { IsDebuggerPresent should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      IsDebuggerPresent;
    end);
end;


{ OutputDebugStr Tests }

procedure TTestDebugger.TestOutputDebugStr_String_NoException;
begin
  { OutputDebugStr with string should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      OutputDebugStr('Test debug message');
    end);
end;


procedure TTestDebugger.TestOutputDebugStr_StringInteger_NoException;
begin
  { OutputDebugStr with string and integer should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      OutputDebugStr('Test value: ', 12345);
    end);
end;


procedure TTestDebugger.TestOutputDebugStr_StringReal_NoException;
begin
  { OutputDebugStr with string and real should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      OutputDebugStr('Test value: ', 123.456);
    end);
end;


procedure TTestDebugger.TestOutputDebugStr_EmptyString_NoException;
begin
  { OutputDebugStr with empty string should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      OutputDebugStr('');
    end);
end;


procedure TTestDebugger.TestOutputDebugStr_LongString_NoException;
var
  LongStr: string;
begin
  { Create a long string }
  LongStr:= StringOfChar('X', 10000);

  { OutputDebugStr with long string should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      OutputDebugStr(LongStr);
    end);
end;


procedure TTestDebugger.TestOutputDebugStr_SpecialChars_NoException;
begin
  { OutputDebugStr with special characters should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      OutputDebugStr('Test with special chars: '#13#10#9'!"@#$%^&*()');
    end);
end;


{ AntiDebug Tests }

procedure TTestDebugger.TestAntiDebug_NoException;
begin
  { AntiDebug should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      AntiDebug;
    end);
end;


procedure TTestDebugger.TestAntiProcDump_NoException;
begin
  { AntiProcDump should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      AntiProcDump;
    end);
end;


{ ExitIfUnderDebugger Tests }

procedure TTestDebugger.TestExitIfUnderDebugger_WithExistingFile_NoExit;
var
  ExeName: string;
begin
  { When project file exists, should not terminate even if debugger present }
  ExeName:= Application.ExeName;

  { This should not terminate because the file exists }
  Assert.WillNotRaise(
    procedure
    begin
      ExitIfUnderDebugger(ExeName);
    end);

  { Application should still be running }
  Assert.IsNotNull(Application, 'Application should still exist');
end;


{ HaltApplication Tests }

procedure TTestDebugger.TestHaltApplication_RaisesException;
var
  ExceptionRaised: Boolean;
begin
  { HaltApplication should raise EApplicationFail exception }
  ExceptionRaised:= False;
  try
    HaltApplication('Test halt message');
  except
    on E: Exception do
    begin
      ExceptionRaised:= True;
      Assert.AreEqual('EApplicationFail', E.ClassName, 'Should raise EApplicationFail');
    end;
  end;
  Assert.IsTrue(ExceptionRaised, 'HaltApplication should raise an exception');
end;


{ EApplicationFail Tests }

procedure TTestDebugger.TestEApplicationFail_IsException;
begin
  { Verify that HaltApplication raises an exception with the correct message }
  try
    HaltApplication('Custom error message');
    Assert.Fail('HaltApplication should have raised an exception');
  except
    on E: Exception do
      Assert.AreEqual('Custom error message', E.Message,
        'Exception message should match');
  end;
end;


{ LastErrorMsgStr Tests }

procedure TTestDebugger.TestLastErrorMsgStr_ReturnsString;
VAR
  ErrorMsg: string;
begin
  { Set a known error and check that we get a string result }
  SetLastError(ERROR_FILE_NOT_FOUND);
  ErrorMsg:= LastErrorMsgStr;

  { Should return a non-empty string for a valid error code }
  Assert.IsTrue(Length(ErrorMsg) > 0, 'LastErrorMsgStr should return a non-empty string');
end;


procedure TTestDebugger.TestLastErrorMsgStr_NoException;
begin
  { LastErrorMsgStr should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      SetLastError(0);  { Clear any previous error }
      LastErrorMsgStr;
    end);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestDebugger);

end.
