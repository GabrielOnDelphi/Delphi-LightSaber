unit Test.LightCore.Win.EnvironmentVar;

{=============================================================================================================
   Unit tests for LightCore.Win.EnvironmentVar.pas
   Tests environment variable read/write operations and string expansion.

   Note: Some tests modify user environment variables. These are cleaned up in TearDown.
   Machine-level tests are skipped unless running with admin privileges.

   The tests of ListEnvironmentVars are in Test.LightCore.EnvironmentVar.pas.
=============================================================================================================}

interface
{$IFDEF MSWINDOWS}

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Winapi.Windows,
  LightCore.Win.EnvironmentVar;

type
  [TestFixture]
  TTestWinEnvironmentVar = class
  private
    CONST TEST_VAR_NAME = 'LIGHTSABER_TEST_VAR';
    CONST TEST_VAR_VALUE = 'TestValue123';
  public
    [TearDown]
    procedure TearDown;

    { ExpandEnvironmentStrings tests }
    [Test]
    procedure Test_ExpandEnvironmentStrings_EmptyString;

    [Test]
    procedure Test_ExpandEnvironmentStrings_NoVars;

    [Test]
    procedure Test_ExpandEnvironmentStrings_SingleVar;

    [Test]
    procedure Test_ExpandEnvironmentStrings_MultipleVars;

    [Test]
    procedure Test_ExpandEnvironmentStrings_NonExistentVar;

    [Test]
    procedure Test_ExpandEnvironmentStrings_NestedPath;

    { GetEnvironmentVars (single variable) tests }
    [Test]
    procedure Test_GetEnvironmentVars_EmptyName;

    [Test]
    procedure Test_GetEnvironmentVars_NonExistent;

    [Test]
    procedure Test_GetEnvironmentVars_ExistingUserVar;

    [Test]
    procedure Test_GetEnvironmentVars_ExpandsReference;

    { SetEnvironmentVars tests }
    [Test]
    procedure Test_SetEnvironmentVars_EmptyName;

    [Test]
    procedure Test_SetEnvironmentVars_UserVar;

    [Test]
    procedure Test_SetEnvironmentVars_UpdatesProcessEnv;

    [Test]
    procedure Test_SetEnvironmentVars_ReadBack;

    { Round-trip tests }
    [Test]
    procedure Test_SetAndGet_RoundTrip;

    [Test]
    procedure Test_SetAndGet_SpecialChars;

    [Test]
    procedure Test_SetAndGet_EmptyValue;

    [Test]
    procedure Test_SetAndGet_LongValue;
  end;
{$ENDIF}

implementation
{$IFDEF MSWINDOWS}

uses
  System.Win.Registry;


procedure TTestWinEnvironmentVar.TearDown;
VAR
  Reg: TRegistry;
begin
  { Clean up test environment variable if it exists }
  Reg:= TRegistry.Create(KEY_WRITE);
  TRY
    Reg.RootKey:= HKEY_CURRENT_USER;
    if Reg.OpenKey('Environment', False) then
      begin
        if Reg.ValueExists(TEST_VAR_NAME)
        then Reg.DeleteValue(TEST_VAR_NAME);
      end;
  FINALLY
    FreeAndNil(Reg);
  END;

  { Also clear from process environment }
  SetEnvironmentVariable(PChar(TEST_VAR_NAME), NIL);
end;


{ ExpandEnvironmentStrings tests }

procedure TTestWinEnvironmentVar.Test_ExpandEnvironmentStrings_EmptyString;
begin
  Assert.AreEqual('', ExpandEnvironmentStrings(''));
end;


procedure TTestWinEnvironmentVar.Test_ExpandEnvironmentStrings_NoVars;
begin
  Assert.AreEqual('Hello World', ExpandEnvironmentStrings('Hello World'));
  Assert.AreEqual('C:\Temp\File.txt', ExpandEnvironmentStrings('C:\Temp\File.txt'));
end;


procedure TTestWinEnvironmentVar.Test_ExpandEnvironmentStrings_SingleVar;
VAR
  Expanded: string;
begin
  { %TEMP% should expand to an actual path }
  Expanded:= ExpandEnvironmentStrings('%TEMP%');
  Assert.IsFalse(Expanded.Contains('%'), 'TEMP should be expanded');
  Assert.IsTrue(Expanded.Length > 0, 'Expanded value should not be empty');
end;


procedure TTestWinEnvironmentVar.Test_ExpandEnvironmentStrings_MultipleVars;
VAR
  Expanded: string;
begin
  { Test multiple variables in one string }
  Expanded:= ExpandEnvironmentStrings('%TEMP%\%USERNAME%');
  Assert.IsFalse(Expanded.Contains('%TEMP%'), 'TEMP should be expanded');
  Assert.IsFalse(Expanded.Contains('%USERNAME%'), 'USERNAME should be expanded');
end;


procedure TTestWinEnvironmentVar.Test_ExpandEnvironmentStrings_NonExistentVar;
VAR
  Expanded: string;
begin
  { Non-existent variables should remain as-is }
  Expanded:= ExpandEnvironmentStrings('%NONEXISTENT_VAR_12345%');
  Assert.AreEqual('%NONEXISTENT_VAR_12345%', Expanded);
end;


procedure TTestWinEnvironmentVar.Test_ExpandEnvironmentStrings_NestedPath;
VAR
  Expanded: string;
begin
  Expanded:= ExpandEnvironmentStrings('%USERPROFILE%\Documents\Test');
  Assert.IsFalse(Expanded.Contains('%'), 'Variables should be expanded');
  Assert.IsTrue(Expanded.EndsWith('\Documents\Test'), 'Path suffix should be preserved');
end;


{ GetEnvironmentVars (single variable) tests }

procedure TTestWinEnvironmentVar.Test_GetEnvironmentVars_EmptyName;
begin
  Assert.AreEqual('', GetEnvironmentVars('', True));
  Assert.AreEqual('', GetEnvironmentVars('', False));
end;


procedure TTestWinEnvironmentVar.Test_GetEnvironmentVars_NonExistent;
begin
  Assert.AreEqual('', GetEnvironmentVars('NONEXISTENT_VAR_XYZZY_12345', True));
end;


procedure TTestWinEnvironmentVar.Test_GetEnvironmentVars_ExistingUserVar;
VAR
  Value: string;
begin
  { First set a known value }
  SetEnvironmentVars(TEST_VAR_NAME, TEST_VAR_VALUE, True);

  { Now read it back }
  Value:= GetEnvironmentVars(TEST_VAR_NAME, True);
  Assert.AreEqual(TEST_VAR_VALUE, Value);
end;


{ A user variable stored as REG_EXPAND_SZ, like the user's Path, holds references such as %SystemRoot%. The value is written straight into the registry (not into the process environment), so the test also proves GetEnvironmentVars reads the registry. TearDown deletes it. }
procedure TTestWinEnvironmentVar.Test_GetEnvironmentVars_ExpandsReference;
VAR
  Reg: TRegistry;
begin
  Reg:= TRegistry.Create(KEY_WRITE);
  TRY
    Reg.RootKey:= HKEY_CURRENT_USER;
    Assert.IsTrue(Reg.OpenKey('Environment', False), 'Could not open HKCU\Environment');
    Reg.WriteExpandString(TEST_VAR_NAME, '%SystemRoot%\LightSaberTest');
  FINALLY
    FreeAndNil(Reg);
  END;

  Assert.AreEqual(System.SysUtils.GetEnvironmentVariable('SystemRoot') +'\LightSaberTest', GetEnvironmentVars(TEST_VAR_NAME, True), 'The %SystemRoot% reference must be expanded');
end;


{ SetEnvironmentVars tests }

procedure TTestWinEnvironmentVar.Test_SetEnvironmentVars_EmptyName;
begin
  Assert.IsFalse(SetEnvironmentVars('', 'SomeValue', True));
end;


procedure TTestWinEnvironmentVar.Test_SetEnvironmentVars_UserVar;
VAR
  Success: Boolean;
begin
  Success:= SetEnvironmentVars(TEST_VAR_NAME, TEST_VAR_VALUE, True);
  Assert.IsTrue(Success, 'SetEnvironmentVars should succeed for user variable');
end;


procedure TTestWinEnvironmentVar.Test_SetEnvironmentVars_UpdatesProcessEnv;
VAR
  Buffer: array[0..255] of Char;
  Len: DWORD;
begin
  { Set environment variable }
  SetEnvironmentVars(TEST_VAR_NAME, TEST_VAR_VALUE, True);

  { Verify it's in the process environment }
  Len:= Winapi.Windows.GetEnvironmentVariable(PChar(TEST_VAR_NAME), Buffer, Length(Buffer));
  Assert.IsTrue(Len > 0, 'Variable should be in process environment');
  Assert.AreEqual(TEST_VAR_VALUE, string(Buffer));
end;


procedure TTestWinEnvironmentVar.Test_SetEnvironmentVars_ReadBack;
VAR
  Reg: TRegistry;
  Value: string;
begin
  { Set via our function }
  SetEnvironmentVars(TEST_VAR_NAME, TEST_VAR_VALUE, True);

  { Read directly from registry to verify }
  Reg:= TRegistry.Create(KEY_READ);
  TRY
    Reg.RootKey:= HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly('Environment') then
      begin
        Value:= Reg.ReadString(TEST_VAR_NAME);
        Assert.AreEqual(TEST_VAR_VALUE, Value, 'Value should be in registry');
      end
    else
      Assert.Fail('Could not open Environment key');
  FINALLY
    FreeAndNil(Reg);
  END;
end;


{ Round-trip tests }

procedure TTestWinEnvironmentVar.Test_SetAndGet_RoundTrip;
VAR
  Value: string;
begin
  Assert.IsTrue(SetEnvironmentVars(TEST_VAR_NAME, TEST_VAR_VALUE, True));
  Value:= GetEnvironmentVars(TEST_VAR_NAME, True);
  Assert.AreEqual(TEST_VAR_VALUE, Value);
end;


procedure TTestWinEnvironmentVar.Test_SetAndGet_SpecialChars;
CONST
  SPECIAL_VALUE = 'Path with spaces & special=chars; "quotes"';
VAR
  Value: string;
begin
  Assert.IsTrue(SetEnvironmentVars(TEST_VAR_NAME, SPECIAL_VALUE, True));
  Value:= GetEnvironmentVars(TEST_VAR_NAME, True);
  Assert.AreEqual(SPECIAL_VALUE, Value);
end;


procedure TTestWinEnvironmentVar.Test_SetAndGet_EmptyValue;
VAR
  Value: string;
begin
  { Set non-empty first, then set empty }
  SetEnvironmentVars(TEST_VAR_NAME, TEST_VAR_VALUE, True);
  Assert.IsTrue(SetEnvironmentVars(TEST_VAR_NAME, '', True));

  Value:= GetEnvironmentVars(TEST_VAR_NAME, True);
  Assert.AreEqual('', Value);
end;


procedure TTestWinEnvironmentVar.Test_SetAndGet_LongValue;
VAR
  LongValue: string;
  Value: string;
  i: Integer;
begin
  { Create a long value (1000 chars) }
  LongValue:= '';
  for i:= 1 to 100 do
    LongValue:= LongValue + '0123456789';

  Assert.IsTrue(SetEnvironmentVars(TEST_VAR_NAME, LongValue, True));
  Value:= GetEnvironmentVars(TEST_VAR_NAME, True);
  Assert.AreEqual(LongValue, Value);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestWinEnvironmentVar);
{$ENDIF}

end.
