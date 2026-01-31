unit Test.LightVcl.Common.EnvironmentVar;

{=============================================================================================================
   Unit tests for LightVcl.Common.EnvironmentVar.pas
   Tests environment variable read/write operations and string expansion.

   Note: Some tests modify user environment variables. These are cleaned up in TearDown.
   Machine-level tests are skipped unless running with admin privileges.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  LightVcl.Common.EnvironmentVar;

type
  [TestFixture]
  TTestEnvironmentVar = class
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

    { GetEnvironmentVars (TStrings) tests }
    [Test]
    procedure Test_GetEnvironmentVars_TStrings_NotEmpty;

    [Test]
    procedure Test_GetEnvironmentVars_TStrings_ContainsPath;

    [Test]
    procedure Test_GetEnvironmentVars_TStrings_Format;

    { GetEnvironmentVars (single variable) tests }
    [Test]
    procedure Test_GetEnvironmentVars_EmptyName;

    [Test]
    procedure Test_GetEnvironmentVars_NonExistent;

    [Test]
    procedure Test_GetEnvironmentVars_ExistingUserVar;

    [Test]
    procedure Test_GetEnvironmentVars_Path_User;

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

implementation

uses
  System.Win.Registry;


procedure TTestEnvironmentVar.TearDown;
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

procedure TTestEnvironmentVar.Test_ExpandEnvironmentStrings_EmptyString;
begin
  Assert.AreEqual('', ExpandEnvironmentStrings(''));
end;


procedure TTestEnvironmentVar.Test_ExpandEnvironmentStrings_NoVars;
begin
  Assert.AreEqual('Hello World', ExpandEnvironmentStrings('Hello World'));
  Assert.AreEqual('C:\Temp\File.txt', ExpandEnvironmentStrings('C:\Temp\File.txt'));
end;


procedure TTestEnvironmentVar.Test_ExpandEnvironmentStrings_SingleVar;
VAR
  Expanded: string;
begin
  { %TEMP% should expand to an actual path }
  Expanded:= ExpandEnvironmentStrings('%TEMP%');
  Assert.IsFalse(Expanded.Contains('%'), 'TEMP should be expanded');
  Assert.IsTrue(Expanded.Length > 0, 'Expanded value should not be empty');
end;


procedure TTestEnvironmentVar.Test_ExpandEnvironmentStrings_MultipleVars;
VAR
  Expanded: string;
begin
  { Test multiple variables in one string }
  Expanded:= ExpandEnvironmentStrings('%TEMP%\%USERNAME%');
  Assert.IsFalse(Expanded.Contains('%TEMP%'), 'TEMP should be expanded');
  Assert.IsFalse(Expanded.Contains('%USERNAME%'), 'USERNAME should be expanded');
end;


procedure TTestEnvironmentVar.Test_ExpandEnvironmentStrings_NonExistentVar;
VAR
  Expanded: string;
begin
  { Non-existent variables should remain as-is }
  Expanded:= ExpandEnvironmentStrings('%NONEXISTENT_VAR_12345%');
  Assert.AreEqual('%NONEXISTENT_VAR_12345%', Expanded);
end;


procedure TTestEnvironmentVar.Test_ExpandEnvironmentStrings_NestedPath;
VAR
  Expanded: string;
begin
  Expanded:= ExpandEnvironmentStrings('%USERPROFILE%\Documents\Test');
  Assert.IsFalse(Expanded.Contains('%'), 'Variables should be expanded');
  Assert.IsTrue(Expanded.EndsWith('\Documents\Test'), 'Path suffix should be preserved');
end;


{ GetEnvironmentVars (TStrings) tests }

procedure TTestEnvironmentVar.Test_GetEnvironmentVars_TStrings_NotEmpty;
VAR
  EnvList: TStringList;
  Success: Boolean;
begin
  EnvList:= TStringList.Create;
  TRY
    Success:= GetEnvironmentVars(EnvList);
    Assert.IsTrue(Success, 'GetEnvironmentVars should succeed');
    Assert.IsTrue(EnvList.Count > 0, 'Environment should have at least one variable');
  FINALLY
    FreeAndNil(EnvList);
  END;
end;


procedure TTestEnvironmentVar.Test_GetEnvironmentVars_TStrings_ContainsPath;
VAR
  EnvList: TStringList;
  i: Integer;
  FoundPath: Boolean;
begin
  EnvList:= TStringList.Create;
  TRY
    GetEnvironmentVars(EnvList);

    FoundPath:= FALSE;
    for i:= 0 to EnvList.Count - 1 do
      if EnvList[i].ToUpper.StartsWith('PATH=') then
        begin
          FoundPath:= TRUE;
          Break;
        end;

    Assert.IsTrue(FoundPath, 'Environment should contain PATH variable');
  FINALLY
    FreeAndNil(EnvList);
  END;
end;


procedure TTestEnvironmentVar.Test_GetEnvironmentVars_TStrings_Format;
VAR
  EnvList: TStringList;
  i: Integer;
begin
  EnvList:= TStringList.Create;
  TRY
    GetEnvironmentVars(EnvList);

    { Each entry should be in NAME=VALUE format }
    for i:= 0 to EnvList.Count - 1 do
      Assert.IsTrue(EnvList[i].Contains('='),
        'Entry should contain = separator: ' + EnvList[i]);
  FINALLY
    FreeAndNil(EnvList);
  END;
end;


{ GetEnvironmentVars (single variable) tests }

procedure TTestEnvironmentVar.Test_GetEnvironmentVars_EmptyName;
begin
  Assert.AreEqual('', GetEnvironmentVars('', True));
  Assert.AreEqual('', GetEnvironmentVars('', False));
end;


procedure TTestEnvironmentVar.Test_GetEnvironmentVars_NonExistent;
begin
  Assert.AreEqual('', GetEnvironmentVars('NONEXISTENT_VAR_XYZZY_12345', True));
end;


procedure TTestEnvironmentVar.Test_GetEnvironmentVars_ExistingUserVar;
VAR
  Value: string;
begin
  { First set a known value }
  SetEnvironmentVars(TEST_VAR_NAME, TEST_VAR_VALUE, True);

  { Now read it back }
  Value:= GetEnvironmentVars(TEST_VAR_NAME, True);
  Assert.AreEqual(TEST_VAR_VALUE, Value);
end;


procedure TTestEnvironmentVar.Test_GetEnvironmentVars_Path_User;
VAR
  Value: string;
begin
  { PATH may exist in user environment (though often empty or not present) }
  { This test verifies the function doesn't crash on valid variable names }
  Value:= GetEnvironmentVars('Path', True);
  { No assertion on value since PATH might not exist in user env }
  Assert.Pass('GetEnvironmentVars did not raise exception');
end;


{ SetEnvironmentVars tests }

procedure TTestEnvironmentVar.Test_SetEnvironmentVars_EmptyName;
begin
  Assert.IsFalse(SetEnvironmentVars('', 'SomeValue', True));
end;


procedure TTestEnvironmentVar.Test_SetEnvironmentVars_UserVar;
VAR
  Success: Boolean;
begin
  Success:= SetEnvironmentVars(TEST_VAR_NAME, TEST_VAR_VALUE, True);
  Assert.IsTrue(Success, 'SetEnvironmentVars should succeed for user variable');
end;


procedure TTestEnvironmentVar.Test_SetEnvironmentVars_UpdatesProcessEnv;
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


procedure TTestEnvironmentVar.Test_SetEnvironmentVars_ReadBack;
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

procedure TTestEnvironmentVar.Test_SetAndGet_RoundTrip;
VAR
  Value: string;
begin
  Assert.IsTrue(SetEnvironmentVars(TEST_VAR_NAME, TEST_VAR_VALUE, True));
  Value:= GetEnvironmentVars(TEST_VAR_NAME, True);
  Assert.AreEqual(TEST_VAR_VALUE, Value);
end;


procedure TTestEnvironmentVar.Test_SetAndGet_SpecialChars;
CONST
  SPECIAL_VALUE = 'Path with spaces & special=chars; "quotes"';
VAR
  Value: string;
begin
  Assert.IsTrue(SetEnvironmentVars(TEST_VAR_NAME, SPECIAL_VALUE, True));
  Value:= GetEnvironmentVars(TEST_VAR_NAME, True);
  Assert.AreEqual(SPECIAL_VALUE, Value);
end;


procedure TTestEnvironmentVar.Test_SetAndGet_EmptyValue;
VAR
  Value: string;
begin
  { Set non-empty first, then set empty }
  SetEnvironmentVars(TEST_VAR_NAME, TEST_VAR_VALUE, True);
  Assert.IsTrue(SetEnvironmentVars(TEST_VAR_NAME, '', True));

  Value:= GetEnvironmentVars(TEST_VAR_NAME, True);
  Assert.AreEqual('', Value);
end;


procedure TTestEnvironmentVar.Test_SetAndGet_LongValue;
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
  TDUnitX.RegisterTestFixture(TTestEnvironmentVar);

end.
