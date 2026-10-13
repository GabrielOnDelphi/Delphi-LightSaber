unit Test.LightCore.Win.SystemTime;

{=============================================================================================================
   Unit tests for LightCore.Win.SystemTime.pas
   Tests the registry-based clock rollback detection functions.

   Note: These tests verify:
   - Parameter validation works correctly
   - Registry-based time protection functions work correctly

   Test environment requirements:
   - Write access to HKEY_CURRENT_USER registry
=============================================================================================================}

interface
{$IFDEF MSWINDOWS}

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Winapi.Windows,
  LightCore.Win.SystemTime,
  LightCore.Win.Registry;

type
  [TestFixture]
  TTestSystemTime = class
  private
  const
    TestRegistryKey = 'Software\LightSaber\UnitTests\SystemTime';
  public
    [TearDown]
    procedure TearDown;

    { CurrentSysTimeStore Tests }
    [Test]
    procedure Test_CurrentSysTimeStore_RaisesOnEmptyKey;

    [Test]
    procedure Test_CurrentSysTimeStore_StoresValue;

    { CurrentSysTimeValid Tests }
    [Test]
    procedure Test_CurrentSysTimeValid_RaisesOnEmptyKey;

    [Test]
    procedure Test_CurrentSysTimeValid_ReturnsTrueOnFirstRun;

    [Test]
    procedure Test_CurrentSysTimeValid_ReturnsTrueAfterStore;

    [Test]
    procedure Test_CurrentSysTimeValid_Integration;
  end;
{$ENDIF}


implementation
{$IFDEF MSWINDOWS}


procedure TTestSystemTime.TearDown;
begin
  { Clean up test registry key after each test }
  RegDeleteKey(HKEY_CURRENT_USER, TestRegistryKey);
end;


{ CurrentSysTimeStore Tests }

procedure TTestSystemTime.Test_CurrentSysTimeStore_RaisesOnEmptyKey;
begin
  Assert.WillRaise(
    procedure
    begin
      CurrentSysTimeStore('');
    end,
    Exception,
    'CurrentSysTimeStore should raise exception on empty key');
end;


procedure TTestSystemTime.Test_CurrentSysTimeStore_StoresValue;
VAR
  StoredTime: TDateTime;
  CurrentTime: TDateTime;
begin
  CurrentTime:= Now;
  CurrentSysTimeStore(TestRegistryKey);

  StoredTime:= RegReadDate(HKEY_CURRENT_USER, TestRegistryKey, 'System');

  { Stored time should be very close to current time (within 1 second) }
  Assert.IsTrue(Abs(StoredTime - CurrentTime) < (1 / SecsPerDay),
    'Stored time should be close to current time');
end;


{ CurrentSysTimeValid Tests }

procedure TTestSystemTime.Test_CurrentSysTimeValid_RaisesOnEmptyKey;
begin
  Assert.WillRaise(
    procedure
    begin
      CurrentSysTimeValid('');
    end,
    Exception,
    'CurrentSysTimeValid should raise exception on empty key');
end;


procedure TTestSystemTime.Test_CurrentSysTimeValid_ReturnsTrueOnFirstRun;
VAR
  IsValid: Boolean;
  UniqueKey: string;
begin
  { Use a unique key that definitely doesn't exist }
  UniqueKey:= TestRegistryKey + '\NonExistent_' + IntToStr(GetTickCount);

  IsValid:= CurrentSysTimeValid(UniqueKey);

  Assert.IsTrue(IsValid,
    'CurrentSysTimeValid should return True on first run (no stored time)');
end;


procedure TTestSystemTime.Test_CurrentSysTimeValid_ReturnsTrueAfterStore;
VAR
  IsValid: Boolean;
begin
  { Store current time }
  CurrentSysTimeStore(TestRegistryKey);

  { Check validity immediately after }
  IsValid:= CurrentSysTimeValid(TestRegistryKey);

  Assert.IsTrue(IsValid,
    'CurrentSysTimeValid should return True immediately after storing');
end;


procedure TTestSystemTime.Test_CurrentSysTimeValid_Integration;
VAR
  IsValid: Boolean;
begin
  { First check - should be valid (first run) }
  IsValid:= CurrentSysTimeValid(TestRegistryKey);
  Assert.IsTrue(IsValid, 'First run should be valid');

  { Store current time }
  CurrentSysTimeStore(TestRegistryKey);

  { Second check - should be valid (time hasn't gone backwards) }
  IsValid:= CurrentSysTimeValid(TestRegistryKey);
  Assert.IsTrue(IsValid, 'Second check should be valid');

  { Wait a tiny bit and check again }
  Sleep(100);
  IsValid:= CurrentSysTimeValid(TestRegistryKey);
  Assert.IsTrue(IsValid, 'Check after 100ms should still be valid');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestSystemTime);
{$ENDIF}

end.
