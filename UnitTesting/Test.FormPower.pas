unit Test.FormPower;

{=============================================================================================================
   Unit tests for TPowerSettings (LightVcl.Common.CpuMonitor).
   Tests CpuIsBusy logic, default values, and Reset behavior.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  LightVcl.Common.CpuMonitor;

type
  [TestFixture]
  TTestPowerSettings = class
  private
    FSettings: TPowerSettings;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { CpuIsBusy Tests }
    [Test]
    procedure TestCpuIsBusy_FalseWhenBelowThreshold;

    [Test]
    procedure TestCpuIsBusy_TrueWhenAboveThreshold;

    [Test]
    procedure TestCpuIsBusy_TrueWhenAtThreshold;

    [Test]
    procedure TestCpuIsBusy_BatteryCheckDisabled;

    { Default Values }
    [Test]
    procedure TestDefaultValues;

    { Reset }
    [Test]
    procedure TestReset;
  end;

implementation


procedure TTestPowerSettings.Setup;
begin
  FSettings:= TPowerSettings.Create;
end;


procedure TTestPowerSettings.TearDown;
begin
  FreeAndNil(FSettings);
end;


{ CpuIsBusy Tests }

procedure TTestPowerSettings.TestCpuIsBusy_FalseWhenBelowThreshold;
begin
  FSettings.CheckBatteries:= FALSE;
  FSettings.HighCpuThreshold:= 100;
  FSettings.LastCpuUsage:= 50;

  Assert.IsFalse(FSettings.CpuIsBusy,
    'CpuIsBusy should return False when CPU usage is below threshold');
end;


procedure TTestPowerSettings.TestCpuIsBusy_TrueWhenAboveThreshold;
begin
  FSettings.CheckBatteries:= FALSE;
  FSettings.HighCpuThreshold:= 50;
  FSettings.LastCpuUsage:= 80;

  Assert.IsTrue(FSettings.CpuIsBusy,
    'CpuIsBusy should return True when CPU usage is above threshold');
end;


procedure TTestPowerSettings.TestCpuIsBusy_TrueWhenAtThreshold;
begin
  FSettings.CheckBatteries:= FALSE;
  FSettings.HighCpuThreshold:= 50;
  FSettings.LastCpuUsage:= 50;

  Assert.IsTrue(FSettings.CpuIsBusy,
    'CpuIsBusy should return True when CPU usage equals threshold');
end;


procedure TTestPowerSettings.TestCpuIsBusy_BatteryCheckDisabled;
begin
  FSettings.CheckBatteries:= FALSE;
  FSettings.HighCpuThreshold:= 100;
  FSettings.LastCpuUsage:= 0;

  Assert.IsFalse(FSettings.CpuIsBusy,
    'CpuIsBusy should return False when battery check is disabled and CPU is low');
end;


{ Default Values }

procedure TTestPowerSettings.TestDefaultValues;
begin
  Assert.AreEqual(TRUE, FSettings.CheckBatteries, 'Default CheckBatteries should be True');
  Assert.AreEqual(TRUE, FSettings.NotifyPowerChange, 'Default NotifyPowerChange should be True');
  Assert.AreEqual(80, FSettings.HighCpuThreshold, 'Default HighCpuThreshold should be 80');
  Assert.AreEqual(0, FSettings.LastCpuUsage, 'Default LastCpuUsage should be 0');
end;


{ Reset }

procedure TTestPowerSettings.TestReset;
begin
  FSettings.HighCpuThreshold:= 50;
  FSettings.CheckBatteries:= FALSE;

  FSettings.Reset;

  Assert.AreEqual(80, FSettings.HighCpuThreshold, 'HighCpuThreshold should be 80 after Reset');
  Assert.AreEqual(TRUE, FSettings.CheckBatteries, 'CheckBatteries should be True after Reset');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestPowerSettings);

end.
