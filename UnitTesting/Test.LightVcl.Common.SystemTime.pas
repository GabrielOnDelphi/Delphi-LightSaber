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
    procedure Test_SystemTimeIsInvalid_MatchesSystemFileTime;

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
    procedure Test_DelayEx_ZeroReturnsAtOnce;

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

{ Injects a zero-distance mouse move, which resets the input-idle timer that GetLastInputInfo reads.
  Returns FALSE when Windows refuses the injection (UIPI: a foreground window of higher integrity). }
function InjectUserInput: Boolean;
VAR
  Input: TInput;
begin
  FillChar(Input, SizeOf(Input), 0);
  Input.Itype:= INPUT_MOUSE;
  Input.mi.dwFlags:= MOUSEEVENTF_MOVE;   { dx = dy = 0: the cursor does not move }
  Result:= SendInput(1, Input, SizeOf(Input)) = 1;
end;


procedure TTestSystemTime.Test_UserIdleTime_ReturnsValue;
VAR
  IdleTime: Cardinal;
begin
  if NOT InjectUserInput then
    begin
      Assert.Pass('SendInput was refused - the idle timer cannot be reset on this desktop');
      EXIT;
    end;

  { The input arrived a few milliseconds ago: zero whole seconds of idle time }
  IdleTime:= UserIdleTime;
  Assert.AreEqual(0, Integer(IdleTime), 'UserIdleTime right after an input event');
end;


procedure TTestSystemTime.Test_UserIdleTime_ReturnsReasonableValue;
VAR
  IdleTime: Cardinal;
begin
  if NOT InjectUserInput then
    begin
      Assert.Pass('SendInput was refused - the idle timer cannot be reset on this desktop');
      EXIT;
    end;

  { 1.1 seconds after the input the result is 1 second (0 if the person at the PC touched the mouse meanwhile).
    A result in milliseconds would be about 1100. }
  Sleep(1100);
  IdleTime:= UserIdleTime;
  Assert.IsTrue(IdleTime <= 2, 'UserIdleTime must count SECONDS. 1.1 s after an input it returned ' + IntToStr(IdleTime));
end;


{ GetSysFileTime Tests }

{ The files GetSysFileTime documents for an NT kernel, in its order: the SOFTWARE registry hive, then
  pagefile.sys on C: to F:. A normal user cannot see the hive, so on most PCs the answer is c:\pagefile.sys.
  Returns the first visible one and its last-write time (local time, read by the RTL's FileAge). }
function ExpectedSysFile(out FileName: string; out FileTime: TDateTime): Boolean;
VAR
  Buffer: array[0..MAX_PATH - 1] of Char;
  WinDir, Candidate: string;
  Candidates: TArray<string>;
begin
  GetWindowsDirectory(Buffer, MAX_PATH);
  WinDir:= IncludeTrailingPathDelimiter(Buffer);
  Candidates:= [WinDir + 'system32\config\software', WinDir + 'config\software',
                'c:\pagefile.sys', 'd:\pagefile.sys', 'e:\pagefile.sys', 'f:\pagefile.sys'];

  FileName:= '';
  FileTime:= 0;
  for Candidate in Candidates DO
    if FileExists(Candidate) then
      begin
        FileName:= Candidate;
        EXIT(System.SysUtils.FileAge(FileName, FileTime));
      end;
  Result:= FALSE;
end;


procedure TTestSystemTime.Test_GetSysFileTime_ReturnsValue;
VAR
  SysTime, Expected: TDateTime;
  FileName: string;
begin
  if NOT ExpectedSysFile(FileName, Expected) then
    begin
      Assert.Pass('None of the system files GetSysFileTime uses is readable on this PC');
      EXIT;
    end;

  SysTime:= GetSysFileTime;
  Assert.AreEqual(Expected, SysTime, 2 / SecsPerDay, 'GetSysFileTime must return the last-write time of ' + FileName);
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

procedure TTestSystemTime.Test_SystemTimeIsInvalid_MatchesSystemFileTime;
VAR
  FileTime: TDateTime;
  FileName: string;
begin
  if NOT ExpectedSysFile(FileName, FileTime) then
    begin
      Assert.Pass('None of the system files GetSysFileTime uses is readable on this PC');
      EXIT;
    end;

  { The clock is "invalid" exactly when it is earlier than the system file's last-write time }
  Assert.IsTrue((Now < FileTime) = SystemTimeIsInvalid, 'SystemTimeIsInvalid must compare Now with the time of ' + FileName + ': ' + DateTimeToStr(FileTime));
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

procedure TTestSystemTime.Test_DelayEx_ZeroReturnsAtOnce;
VAR
  StartTick, ElapsedMs: UInt64;
begin
  StartTick:= GetTickCount64;
  DelayEx(0);
  ElapsedMs:= GetTickCount64 - StartTick;

  { One pass of the loop: Sleep(1) plus one message pump, well under 50 ms }
  Assert.IsTrue(ElapsedMs < 50, 'DelayEx(0) must return at once. Elapsed: ' + IntToStr(ElapsedMs) + ' ms');
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
