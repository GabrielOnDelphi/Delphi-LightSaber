unit Test.LightVcl.Common.SystemTime;

{=============================================================================================================
   Unit tests for LightVcl.Common.SystemTime.pas
   Tests DelayEx.

   The tests of WindowsUpTime, UserIdleTime, GetSysFileTime and SystemTimeIsInvalid are in Test.LightCore.SystemTime.pas.
   The tests of CurrentSysTimeStore and CurrentSysTimeValid are in Test.LightCore.Win.SystemTime.pas.

   Test environment requirements:
   - Windows Vista or later (for GetTickCount64)
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Winapi.Windows,
  LightVcl.Common.SystemTime;

type
  [TestFixture]
  TTestSystemTime = class
  public
    { DelayEx Tests - Limited testing due to ProcessMessages }
    [Test]
    procedure Test_DelayEx_ZeroReturnsAtOnce;

    [Test]
    procedure Test_DelayEx_WaitsApproximateTime;
  end;


implementation


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
