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
    procedure RemoveTestVar;
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
begin
  RemoveTestVar;
end;


{ Deletes the test variable from HKCU\Environment and from the process environment }
procedure TTestWinEnvironmentVar.RemoveTestVar;
VAR
  Reg: TRegistry;
begin
  { KEY_READ is needed too: ValueExists calls RegQueryValueEx (c:\Delphi\Delphi 13\source\rtl\common\System.Win.Registry.pas:893 -> :555), which needs KEY_QUERY_VALUE, and KEY_WRITE alone lacks it (c:\Delphi\Delphi 13\source\rtl\win\Winapi.Windows.pas:3702) }
  Reg:= TRegistry.Create(KEY_READ OR KEY_WRITE);
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
  { The expected value is read from the process environment through the RTL, not through the WinAPI expansion }
  Assert.IsNotEmpty(System.SysUtils.GetEnvironmentVariable('TEMP'), 'Precondition: the TEMP environment variable');
  Expanded:= ExpandEnvironmentStrings('%TEMP%');
  Assert.AreEqual(System.SysUtils.GetEnvironmentVariable('TEMP'), Expanded, '%TEMP% must expand to the TEMP variable');
end;


procedure TTestWinEnvironmentVar.Test_ExpandEnvironmentStrings_MultipleVars;
VAR
  Expanded: string;
begin
  { Test multiple variables in one string }
  Assert.IsNotEmpty(System.SysUtils.GetEnvironmentVariable('TEMP'), 'Precondition: the TEMP environment variable');
  Assert.IsNotEmpty(System.SysUtils.GetEnvironmentVariable('USERNAME'), 'Precondition: the USERNAME environment variable');
  Expanded:= ExpandEnvironmentStrings('%TEMP%\%USERNAME%');
  Assert.AreEqual(System.SysUtils.GetEnvironmentVariable('TEMP') + '\' + System.SysUtils.GetEnvironmentVariable('USERNAME'), Expanded, 'Both variables must expand, and the text between them must stay');
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
  Assert.IsNotEmpty(System.SysUtils.GetEnvironmentVariable('USERPROFILE'), 'Precondition: the USERPROFILE environment variable');
  Expanded:= ExpandEnvironmentStrings('%USERPROFILE%\Documents\Test');
  Assert.AreEqual(System.SysUtils.GetEnvironmentVariable('USERPROFILE') + '\Documents\Test', Expanded, 'The variable must expand and the path suffix must stay');
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
  Reg: TRegistry;
begin
  RemoveTestVar;
  Reg:= TRegistry.Create(KEY_READ);
  TRY
    Reg.RootKey:= HKEY_CURRENT_USER;
    Assert.IsTrue(Reg.OpenKeyReadOnly('Environment'), 'Could not open HKCU\Environment');
    Assert.IsFalse(Reg.ValueExists(TEST_VAR_NAME), 'Precondition: the variable is not in HKCU\Environment');
    Assert.AreEqual('', System.SysUtils.GetEnvironmentVariable(TEST_VAR_NAME), 'Precondition: the variable is not in the process environment');

    Success:= SetEnvironmentVars(TEST_VAR_NAME, TEST_VAR_VALUE, True);
    Assert.IsTrue(Success, 'SetEnvironmentVars should succeed for user variable');

    { Both places it writes, read back without GetEnvironmentVars: the process environment and HKCU\Environment as a REG_SZ value }
    Assert.AreEqual(TEST_VAR_VALUE, System.SysUtils.GetEnvironmentVariable(TEST_VAR_NAME), 'The process environment must hold the value');
    Assert.IsTrue(Reg.GetDataType(TEST_VAR_NAME) = rdString, 'The value must be a REG_SZ');
    Assert.AreEqual(TEST_VAR_VALUE, Reg.ReadString(TEST_VAR_NAME), 'HKCU\Environment must hold the value');
  FINALLY
    FreeAndNil(Reg);
  END;
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
