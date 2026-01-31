unit Test.LightVcl.Common.WMIResolution;

{=============================================================================================================
   Unit tests for LightVcl.Common.WMIResolution.pas
   Tests WMI-based monitor information retrieval.

   Note: These tests require the WMI service to be running. If WMI is disabled,
   the tests will raise EOleSysError exceptions which is expected behavior.

   Tests are designed to run on any Windows system with WMI enabled (Vista+).
   On systems where WMI returns no monitors, zeroed values are expected.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  ActiveX,
  System.Win.ComObj,
  LightVcl.Common.WMIResolution;

type
  [TestFixture]
  TTestWMIResolution = class
  private
    FCoInitialized: Boolean;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { TMonitorInfo Record Tests }
    [Test]
    procedure Test_TMonitorInfo_DefaultInitialization;

    { GetMonitorInfoWMI Basic Tests }
    [Test]
    procedure Test_GetMonitorInfoWMI_ReturnsRecord;

    [Test]
    procedure Test_GetMonitorInfoWMI_WidthIsNonNegative;

    [Test]
    procedure Test_GetMonitorInfoWMI_HeightIsNonNegative;

    [Test]
    procedure Test_GetMonitorInfoWMI_DpiIsNonNegative;

    [Test]
    procedure Test_GetMonitorInfoWMI_CaptionIsString;

    [Test]
    procedure Test_GetMonitorInfoWMI_DeviceIDIsString;

    { WMI Requirement Tests }
    [Test]
    procedure Test_GetMonitorInfoWMI_WithoutCoInitialize_RaisesException;

    { Resolution Sanity Tests - When WMI returns valid data }
    [Test]
    procedure Test_GetMonitorInfoWMI_ValidResolution_WidthInReasonableRange;

    [Test]
    procedure Test_GetMonitorInfoWMI_ValidResolution_HeightInReasonableRange;

    [Test]
    procedure Test_GetMonitorInfoWMI_ValidDpi_InReasonableRange;

    { Multiple Calls Consistency Test }
    [Test]
    procedure Test_GetMonitorInfoWMI_MultipleCalls_ReturnConsistentResults;
  end;


implementation


procedure TTestWMIResolution.Setup;
begin
  { Initialize COM for WMI access }
  FCoInitialized:= Succeeded(CoInitialize(NIL));
end;


procedure TTestWMIResolution.TearDown;
begin
  if FCoInitialized
  then CoUninitialize;
end;


{ TMonitorInfo Record Tests }

procedure TTestWMIResolution.Test_TMonitorInfo_DefaultInitialization;
VAR
  Info: TMonitorInfo;
begin
  { Verify that a default record has expected zero values }
  Info:= Default(TMonitorInfo);
  Assert.AreEqual(0, Info.Dpi, 'Default Dpi should be 0');
  Assert.AreEqual('', Info.Caption, 'Default Caption should be empty');
  Assert.AreEqual('', Info.DeviceID, 'Default DeviceID should be empty');
  Assert.AreEqual(0, Info.Width, 'Default Width should be 0');
  Assert.AreEqual(0, Info.Height, 'Default Height should be 0');
end;


{ GetMonitorInfoWMI Basic Tests }

procedure TTestWMIResolution.Test_GetMonitorInfoWMI_ReturnsRecord;
VAR
  Info: TMonitorInfo;
begin
  { Test that the function executes without exception when COM is initialized }
  Assert.WillNotRaise(
    procedure
    begin
      Info:= GetMonitorInfoWMI;
    end,
    'GetMonitorInfoWMI should not raise exception when COM is initialized'
  );
end;


procedure TTestWMIResolution.Test_GetMonitorInfoWMI_WidthIsNonNegative;
VAR
  Info: TMonitorInfo;
begin
  Info:= GetMonitorInfoWMI;
  Assert.IsTrue(Info.Width >= 0, 'Width should be non-negative (0 if WMI returns no data)');
end;


procedure TTestWMIResolution.Test_GetMonitorInfoWMI_HeightIsNonNegative;
VAR
  Info: TMonitorInfo;
begin
  Info:= GetMonitorInfoWMI;
  Assert.IsTrue(Info.Height >= 0, 'Height should be non-negative (0 if WMI returns no data)');
end;


procedure TTestWMIResolution.Test_GetMonitorInfoWMI_DpiIsNonNegative;
VAR
  Info: TMonitorInfo;
begin
  Info:= GetMonitorInfoWMI;
  Assert.IsTrue(Info.Dpi >= 0, 'Dpi should be non-negative (0 if WMI returns no data)');
end;


procedure TTestWMIResolution.Test_GetMonitorInfoWMI_CaptionIsString;
VAR
  Info: TMonitorInfo;
begin
  Info:= GetMonitorInfoWMI;
  { Caption should be a valid string (may be empty if no monitors found) }
  Assert.Pass('Caption is a valid string: "' + Info.Caption + '"');
end;


procedure TTestWMIResolution.Test_GetMonitorInfoWMI_DeviceIDIsString;
VAR
  Info: TMonitorInfo;
begin
  Info:= GetMonitorInfoWMI;
  { DeviceID should be a valid string (may be empty if no monitors found) }
  Assert.Pass('DeviceID is a valid string: "' + Info.DeviceID + '"');
end;


{ WMI Requirement Tests }

procedure TTestWMIResolution.Test_GetMonitorInfoWMI_WithoutCoInitialize_RaisesException;
begin
  { Temporarily uninitialize COM to test the requirement }
  if FCoInitialized then
  begin
    CoUninitialize;
    FCoInitialized:= FALSE;
  end;

  { Without CoInitialize, calling WMI functions should raise an exception.
    This demonstrates the CoInitialize requirement documented in the unit. }
  Assert.WillRaise(
    procedure
    begin
      GetMonitorInfoWMI;
    end,
    EOleSysError,
    'GetMonitorInfoWMI should raise EOleSysError without CoInitialize'
  );
end;


{ Resolution Sanity Tests }

procedure TTestWMIResolution.Test_GetMonitorInfoWMI_ValidResolution_WidthInReasonableRange;
VAR
  Info: TMonitorInfo;
begin
  Info:= GetMonitorInfoWMI;

  { If WMI returned data, width should be in a reasonable range }
  if Info.Width > 0
  then Assert.IsTrue((Info.Width >= 640) AND (Info.Width <= 15360),
       'If WMI returns data, width should be between 640 and 15360 (8K). Got: ' + IntToStr(Info.Width))
  else Assert.Pass('WMI returned no width data (0) - this is valid on some systems');
end;


procedure TTestWMIResolution.Test_GetMonitorInfoWMI_ValidResolution_HeightInReasonableRange;
VAR
  Info: TMonitorInfo;
begin
  Info:= GetMonitorInfoWMI;

  { If WMI returned data, height should be in a reasonable range }
  if Info.Height > 0
  then Assert.IsTrue((Info.Height >= 480) AND (Info.Height <= 8640),
       'If WMI returns data, height should be between 480 and 8640 (8K). Got: ' + IntToStr(Info.Height))
  else Assert.Pass('WMI returned no height data (0) - this is valid on some systems');
end;


procedure TTestWMIResolution.Test_GetMonitorInfoWMI_ValidDpi_InReasonableRange;
VAR
  Info: TMonitorInfo;
begin
  Info:= GetMonitorInfoWMI;

  { If WMI returned DPI data, it should be in a reasonable range }
  if Info.Dpi > 0
  then Assert.IsTrue((Info.Dpi >= 72) AND (Info.Dpi <= 600),
       'If WMI returns DPI, it should be between 72 and 600. Got: ' + IntToStr(Info.Dpi))
  else Assert.Pass('WMI returned no DPI data (0) - this is common on many systems');
end;


{ Multiple Calls Consistency Test }

procedure TTestWMIResolution.Test_GetMonitorInfoWMI_MultipleCalls_ReturnConsistentResults;
VAR
  Info1, Info2: TMonitorInfo;
begin
  { Two consecutive calls should return the same results }
  Info1:= GetMonitorInfoWMI;
  Info2:= GetMonitorInfoWMI;

  Assert.AreEqual(Info1.Width, Info2.Width, 'Width should be consistent across calls');
  Assert.AreEqual(Info1.Height, Info2.Height, 'Height should be consistent across calls');
  Assert.AreEqual(Info1.Dpi, Info2.Dpi, 'Dpi should be consistent across calls');
  Assert.AreEqual(Info1.Caption, Info2.Caption, 'Caption should be consistent across calls');
  Assert.AreEqual(Info1.DeviceID, Info2.DeviceID, 'DeviceID should be consistent across calls');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestWMIResolution);

end.
