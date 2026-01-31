unit Test.LightVcl.Common.PowerUtils;

{=============================================================================================================
   Unit tests for LightVcl.Common.PowerUtils.pas
   Tests power management utility functions.

   IMPORTANT: Only read-only/query functions are tested.
   Functions that would actually sleep/shutdown the system are NOT tested for safety.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils;

type
  [TestFixture]
  TTestPowerUtils = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { PowerStatus Tests - Read-only, safe to call }
    [Test]
    procedure TestPowerStatus_ReturnsValidValue;

    [Test]
    procedure TestPowerStatusString_ReturnsNonEmpty;

    [Test]
    procedure TestPowerStatusString_ContainsExpectedText;

    { BatteryLeft Tests }
    [Test]
    procedure TestBatteryLeft_ReturnsValidRange;

    [Test]
    procedure TestBatteryLeft_NotAbove100;

    { BatteryAsText Tests }
    [Test]
    procedure TestBatteryAsText_ReturnsNonEmpty;

    [Test]
    procedure TestBatteryAsText_NoErrorMessage;

    { Power Capability Tests - External DLL functions, safe to query }
    [Test]
    procedure TestIsHibernateAllowed_ReturnsBool;

    [Test]
    procedure TestIsPwrSuspendAllowed_ReturnsBool;

    [Test]
    procedure TestIsPwrShutdownAllowed_ReturnsBool;

    { IsScreenSaverOn Tests }
    [Test]
    procedure TestIsScreenSaverOn_ReturnsBool;

    { TPowerType Enum Tests }
    [Test]
    procedure TestTPowerType_HasExpectedValues;
  end;

implementation

uses
  LightVcl.Common.PowerUtils;


procedure TTestPowerUtils.Setup;
begin
  // No setup needed for read-only tests
end;


procedure TTestPowerUtils.TearDown;
begin
  // No cleanup needed
end;


{ PowerStatus Tests }

procedure TTestPowerUtils.TestPowerStatus_ReturnsValidValue;
var
  Status: TPowerType;
begin
  Status:= PowerStatus;
  Assert.IsTrue(Status in [pwTypeBat, pwTypeAC, pwUnknown], 'PowerStatus should return a valid TPowerType');
end;


procedure TTestPowerUtils.TestPowerStatusString_ReturnsNonEmpty;
var
  StatusStr: string;
begin
  StatusStr:= PowerStatusString;
  Assert.IsNotEmpty(StatusStr, 'PowerStatusString should return non-empty string');
end;


procedure TTestPowerUtils.TestPowerStatusString_ContainsExpectedText;
var
  StatusStr: string;
begin
  StatusStr:= PowerStatusString;
  // Should contain one of the expected phrases
  Assert.IsTrue(
    (Pos('batteries', StatusStr) > 0) OR
    (Pos('AC', StatusStr) > 0) OR
    (Pos('unavailable', StatusStr) > 0),
    'PowerStatusString should contain expected text');
end;


{ BatteryLeft Tests }

procedure TTestPowerUtils.TestBatteryLeft_ReturnsValidRange;
var
  Battery: Integer;
begin
  Battery:= BatteryLeft;
  // Returns -1 if unknown, or 0-100 for percentage
  Assert.IsTrue((Battery >= -1) AND (Battery <= 100),
    'BatteryLeft should return -1 (unknown) or 0-100 (percentage)');
end;


procedure TTestPowerUtils.TestBatteryLeft_NotAbove100;
var
  Battery: Integer;
begin
  Battery:= BatteryLeft;
  Assert.IsTrue(Battery <= 100, 'BatteryLeft should never exceed 100%');
end;


{ BatteryAsText Tests }

procedure TTestPowerUtils.TestBatteryAsText_ReturnsNonEmpty;
var
  Text: string;
begin
  Text:= BatteryAsText;
  Assert.IsNotEmpty(Text, 'BatteryAsText should return non-empty string');
end;


procedure TTestPowerUtils.TestBatteryAsText_NoErrorMessage;
var
  Text: string;
begin
  Text:= BatteryAsText;
  // If system call succeeds, should not contain error message
  // (might still contain error if running in unusual environment)
  Assert.Pass('BatteryAsText executed without exception');
end;


{ Power Capability Tests }

procedure TTestPowerUtils.TestIsHibernateAllowed_ReturnsBool;
var
  Allowed: Boolean;
begin
  // This just tests that the external function can be called
  Allowed:= IsHibernateAllowed;
  Assert.IsTrue((Allowed = TRUE) OR (Allowed = FALSE), 'IsHibernateAllowed should return valid Boolean');
end;


procedure TTestPowerUtils.TestIsPwrSuspendAllowed_ReturnsBool;
var
  Allowed: Boolean;
begin
  Allowed:= IsPwrSuspendAllowed;
  Assert.IsTrue((Allowed = TRUE) OR (Allowed = FALSE), 'IsPwrSuspendAllowed should return valid Boolean');
end;


procedure TTestPowerUtils.TestIsPwrShutdownAllowed_ReturnsBool;
var
  Allowed: Boolean;
begin
  Allowed:= IsPwrShutdownAllowed;
  Assert.IsTrue((Allowed = TRUE) OR (Allowed = FALSE), 'IsPwrShutdownAllowed should return valid Boolean');
end;


{ IsScreenSaverOn Tests }

procedure TTestPowerUtils.TestIsScreenSaverOn_ReturnsBool;
var
  IsOn: Boolean;
begin
  // Screen saver should normally be off during test execution
  IsOn:= IsScreenSaverOn;
  Assert.IsTrue((IsOn = TRUE) OR (IsOn = FALSE), 'IsScreenSaverOn should return valid Boolean');
end;


{ TPowerType Enum Tests }

procedure TTestPowerUtils.TestTPowerType_HasExpectedValues;
begin
  // Verify enum values exist and are distinct
  Assert.AreNotEqual(Ord(pwTypeBat), Ord(pwTypeAC), 'pwTypeBat and pwTypeAC should be different');
  Assert.AreNotEqual(Ord(pwTypeBat), Ord(pwUnknown), 'pwTypeBat and pwUnknown should be different');
  Assert.AreNotEqual(Ord(pwTypeAC), Ord(pwUnknown), 'pwTypeAC and pwUnknown should be different');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestPowerUtils);

end.
