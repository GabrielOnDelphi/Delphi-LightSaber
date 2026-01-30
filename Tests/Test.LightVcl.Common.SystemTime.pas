unit Test.LightVcl.Common.SystemTime;

{=============================================================================================================
   Unit tests for LightVcl.Common.SystemTime.pas
   Tests Windows uptime, user idle time, and clock rollback detection functions.

   Note: These tests verify:
   - Functions return valid values without crashing
   - Time functions return reasonable values
   - Parameter validation works correctly
   - Registry-based time protection functions work correctly

   Test environment requirements:
   - Windows Vista or later (for GetTickCount64)
   - Write access to HKEY_CURRENT_USER registry
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.DateUtils,
  Winapi.Windows,
  LightVcl.Common.SystemTime,
  LightVcl.Common.Registry;

type
  [TestFixture]
  TTestSystemTime = class
  private
  const
    TestRegistryKey = 'Software\LightSaber\UnitTests\SystemTime';
  public
    [TearDown]
    procedure TearDown;

    { WindowsUpTime Tests }
    [Test]
    procedure Test_WindowsUpTime_ReturnsPositiveValue;

    [Test]
    procedure Test_WindowsUpTime_ReturnsReasonableValue;

    [Test]
    procedure Test_WindowsUpTime_IsConsistent;

    { UserIdleTime Tests }
    [Test]
    procedure Test_UserIdleTime_ReturnsValue;

    [Test]
    procedure Test_UserIdleTime_ReturnsReasonableValue;

    { GetSysFileTime Tests }
    [Test]
    procedure Test_GetSysFileTime_ReturnsValue;

    [Test]
    procedure Test_GetSysFileTime_ReturnsReasonableDate;

    { SystemTimeIsInvalid Tests }
    [Test]
    procedure Test_SystemTimeIsInvalid_ReturnsBoolean;

    [Test]
    procedure Test_SystemTimeIsInvalid_NormallyReturnsFalse;

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

    { DelayEx Tests - Limited testing due to ProcessMessages }
    [Test]
    procedure Test_DelayEx_DoesNotCrash;

    [Test]
    procedure Test_DelayEx_WaitsApproximateTime;
  end;


implementation


procedure TTestSystemTime.TearDown;
begin
  { Clean up test registry key after each test }
  RegDeleteKey(HKEY_CURRENT_USER, TestRegistryKey);
end;


{ WindowsUpTime Tests }

procedure TTestSystemTime.Test_WindowsUpTime_ReturnsPositiveValue;
VAR
  UpTime: TDateTime;
begin
  UpTime:= WindowsUpTime;
  Assert.IsTrue(UpTime > 0, 'WindowsUpTime should return a positive value');
end;


procedure TTestSystemTime.Test_WindowsUpTime_ReturnsReasonableValue;
VAR
  UpTime: TDateTime;
  UpTimeDays: Double;
begin
  { System should have been up for at least a few seconds }
  UpTime:= WindowsUpTime;
  UpTimeDays:= UpTime;

  { Convert to seconds for easier comparison }
  Assert.IsTrue(UpTimeDays * SecsPerDay > 1, 'System should have been up for at least 1 second');

  { System shouldn't report more than 10 years of uptime (sanity check) }
  Assert.IsTrue(UpTimeDays < 3650, 'System uptime should be less than 10 years');
end;


procedure TTestSystemTime.Test_WindowsUpTime_IsConsistent;
VAR
  UpTime1, UpTime2: TDateTime;
begin
  { Two consecutive calls should return similar values }
  UpTime1:= WindowsUpTime;
  Sleep(10);
  UpTime2:= WindowsUpTime;

  { UpTime2 should be >= UpTime1 (allowing for small timing variations) }
  Assert.IsTrue(UpTime2 >= UpTime1 - (1 / SecsPerDay),
    'Second uptime reading should be >= first reading');
end;


{ UserIdleTime Tests }

procedure TTestSystemTime.Test_UserIdleTime_ReturnsValue;
VAR
  IdleTime: Cardinal;
begin
  { Just verify it returns without crashing }
  IdleTime:= UserIdleTime;
  Assert.Pass('UserIdleTime returned: ' + IntToStr(IdleTime) + ' seconds');
end;


procedure TTestSystemTime.Test_UserIdleTime_ReturnsReasonableValue;
VAR
  IdleTime: Cardinal;
begin
  IdleTime:= UserIdleTime;

  { User idle time should be less than system uptime }
  { Maximum reasonable idle time is 1 year in seconds }
  Assert.IsTrue(IdleTime < 365 * 24 * 60 * 60,
    'User idle time should be reasonable (< 1 year)');
end;


{ GetSysFileTime Tests }

procedure TTestSystemTime.Test_GetSysFileTime_ReturnsValue;
VAR
  SysTime: TDateTime;
begin
  { This may return 0 if no suitable system file is found, which is valid }
  SysTime:= GetSysFileTime;
  Assert.Pass('GetSysFileTime returned: ' + DateTimeToStr(SysTime));
end;


procedure TTestSystemTime.Test_GetSysFileTime_ReturnsReasonableDate;
VAR
  SysTime: TDateTime;
begin
  SysTime:= GetSysFileTime;

  if SysTime > 0 then
  begin
    { If we got a value, it should be in a reasonable range }
    { Not before year 2000 }
    Assert.IsTrue(YearOf(SysTime) >= 2000,
      'System file time should be after year 2000. Got: ' + DateTimeToStr(SysTime));

    { Not more than 1 day in the future (accounting for time zones) }
    Assert.IsTrue(SysTime <= Now + 1,
      'System file time should not be more than 1 day in the future');
  end
  else
    Assert.Pass('GetSysFileTime returned 0 (no suitable system file found)');
end;


{ SystemTimeIsInvalid Tests }

procedure TTestSystemTime.Test_SystemTimeIsInvalid_ReturnsBoolean;
VAR
  IsInvalid: Boolean;
begin
  { Just verify it returns without crashing }
  IsInvalid:= SystemTimeIsInvalid;
  Assert.Pass('SystemTimeIsInvalid returned: ' + BoolToStr(IsInvalid, True));
end;


procedure TTestSystemTime.Test_SystemTimeIsInvalid_NormallyReturnsFalse;
VAR
  IsInvalid: Boolean;
begin
  { On a normal system, the clock should be valid }
  IsInvalid:= SystemTimeIsInvalid;
  Assert.IsFalse(IsInvalid,
    'On a normal system, SystemTimeIsInvalid should return False');
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


{ DelayEx Tests }

procedure TTestSystemTime.Test_DelayEx_DoesNotCrash;
begin
  { Just verify it executes without crashing }
  { Using a very short delay to keep tests fast }
  DelayEx(10);
  Assert.Pass('DelayEx completed without crashing');
end;


procedure TTestSystemTime.Test_DelayEx_WaitsApproximateTime;
VAR
  StartTick, EndTick: UInt64;
  ElapsedMs: UInt64;
  RequestedMs: Cardinal;
begin
  RequestedMs:= 50;

  StartTick:= GetTickCount64;
  DelayEx(RequestedMs);
  EndTick:= GetTickCount64;

  ElapsedMs:= EndTick - StartTick;

  { Should wait at least the requested time (allowing for small variations) }
  Assert.IsTrue(ElapsedMs >= RequestedMs - 5,
    Format('DelayEx should wait at least %d ms. Elapsed: %d ms', [RequestedMs - 5, ElapsedMs]));

  { Should not wait excessively longer (allowing for ProcessMessages overhead) }
  Assert.IsTrue(ElapsedMs < RequestedMs + 100,
    Format('DelayEx should not wait much longer than %d ms. Elapsed: %d ms', [RequestedMs, ElapsedMs]));
end;


initialization
  TDUnitX.RegisterTestFixture(TTestSystemTime);

end.
