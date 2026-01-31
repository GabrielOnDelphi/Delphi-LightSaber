unit Test.LightVcl.Common.WinVersionApi;

{=============================================================================================================
   Unit tests for LightVcl.Common.WinVersionApi.pas
   Tests alternative Windows version detection functions using various APIs.

   Note: These tests verify:
   - Functions return valid values without crashing
   - Version strings are in expected format
   - All three detection methods produce consistent results
   - GenerateReport includes all detection methods
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  LightVcl.Common.WinVersionApi;

type
  [TestFixture]
  TTestWinVersionApi = class
  public
    { GetWinVersion Tests (RtlGetVersion API) }
    [Test]
    procedure Test_GetWinVersion_Proc_ReturnsValidVersion;

    [Test]
    procedure Test_GetWinVersion_Func_ReturnsNonEmptyString;

    [Test]
    procedure Test_GetWinVersion_Func_ContainsDot;

    [Test]
    procedure Test_GetWinVersion_MajorAtLeast6;

    { GetWinVersionEx Tests (GetVersionEx API) }
    [Test]
    procedure Test_GetWinVersionEx_ReturnsNonEmptyString;

    [Test]
    procedure Test_GetWinVersionEx_ReturnsKnownOS;

    [Test]
    procedure Test_GetWinVersionEx_NotUnknownOS;

    { GetWinVerNetServer Tests (NetServerGetInfo API) }
    [Test]
    procedure Test_GetWinVerNetServer_ReturnsNonEmptyString;

    [Test]
    procedure Test_GetWinVerNetServer_ContainsDot;

    [Test]
    procedure Test_GetWinVerNetServer_NotUnknownOS;

    { Cross-Method Consistency Tests }
    [Test]
    procedure Test_Consistency_GetWinVersion_MatchesNetServer;

    [Test]
    procedure Test_Consistency_AllMethodsReturnModernWindows;

    { GenerateReport Tests }
    [Test]
    procedure Test_GenerateReport_ReturnsNonEmptyString;

    [Test]
    procedure Test_GenerateReport_ContainsSysUtilsSection;

    [Test]
    procedure Test_GenerateReport_ContainsNetServerSection;

    [Test]
    procedure Test_GenerateReport_ContainsGetWinVersionSection;

    [Test]
    procedure Test_GenerateReport_ContainsGetWinVersionExSection;

    [Test]
    procedure Test_GenerateReport_HasSubstantialContent;

    { Modern Windows Tests }
    [Test]
    procedure Test_ModernWindows_MajorVersionAtLeast6;

    [Test]
    procedure Test_ModernWindows_GetWinVersionEx_IsWin7OrLater;
  end;


implementation


{ GetWinVersion Tests }

procedure TTestWinVersionApi.Test_GetWinVersion_Proc_ReturnsValidVersion;
VAR
  MajVer, MinVer: Cardinal;
begin
  GetWinVersion(MajVer, MinVer);

  { RtlGetVersion should always succeed on modern Windows and return non-zero major version }
  Assert.IsTrue(MajVer > 0, 'Major version should be greater than 0. Got: ' + IntToStr(MajVer));
  { Minor version can be 0, so we just verify the call completed }
  Assert.Pass('GetWinVersion returned: ' + IntToStr(MajVer) + '.' + IntToStr(MinVer));
end;


procedure TTestWinVersionApi.Test_GetWinVersion_Func_ReturnsNonEmptyString;
VAR
  VersionStr: string;
begin
  VersionStr:= GetWinVersion;
  Assert.IsTrue(VersionStr <> '', 'GetWinVersion should return non-empty string');
end;


procedure TTestWinVersionApi.Test_GetWinVersion_Func_ContainsDot;
VAR
  VersionStr: string;
begin
  VersionStr:= GetWinVersion;
  Assert.IsTrue(Pos('.', VersionStr) > 0,
    'GetWinVersion should return version in Major.Minor format. Got: ' + VersionStr);
end;


procedure TTestWinVersionApi.Test_GetWinVersion_MajorAtLeast6;
VAR
  MajVer, MinVer: Cardinal;
begin
  GetWinVersion(MajVer, MinVer);

  { Windows Vista (6.0) is the minimum for modern development }
  Assert.IsTrue(MajVer >= 6,
    'Major version should be at least 6 (Vista). Got: ' + IntToStr(MajVer));
end;


{ GetWinVersionEx Tests }

procedure TTestWinVersionApi.Test_GetWinVersionEx_ReturnsNonEmptyString;
VAR
  OSName: string;
begin
  OSName:= GetWinVersionEx;
  Assert.IsTrue(OSName <> '', 'GetWinVersionEx should return non-empty string');
end;


procedure TTestWinVersionApi.Test_GetWinVersionEx_ReturnsKnownOS;
VAR
  OSName: string;
  ValidNames: array of string;
  i: Integer;
  Found: Boolean;
begin
  OSName:= GetWinVersionEx;

  { List of valid OS names that GetWinVersionEx can return }
  ValidNames:= ['95', '98', '2000', 'XP', '2003', 'Vista', '2008', '7', '8', '8.1', '10', '11', 'Unknown OS'];

  Found:= False;
  for i:= Low(ValidNames) to High(ValidNames) do
    if OSName = ValidNames[i] then
    begin
      Found:= True;
      Break;
    end;

  Assert.IsTrue(Found,
    'GetWinVersionEx should return a known OS name. Got: "' + OSName + '"');
end;


procedure TTestWinVersionApi.Test_GetWinVersionEx_NotUnknownOS;
VAR
  OSName: string;
begin
  OSName:= GetWinVersionEx;

  { On modern Windows with proper manifest, should not return Unknown OS }
  Assert.AreNotEqual('Unknown OS', OSName,
    'GetWinVersionEx should detect the OS on modern systems');
end;


{ GetWinVerNetServer Tests }

procedure TTestWinVersionApi.Test_GetWinVerNetServer_ReturnsNonEmptyString;
VAR
  VersionStr: string;
begin
  VersionStr:= GetWinVerNetServer;
  Assert.IsTrue(VersionStr <> '', 'GetWinVerNetServer should return non-empty string');
end;


procedure TTestWinVersionApi.Test_GetWinVerNetServer_ContainsDot;
VAR
  VersionStr: string;
begin
  VersionStr:= GetWinVerNetServer;

  { Should return version in Major.Minor format, or "Unknown OS" on failure }
  if VersionStr <> 'Unknown OS' then
    Assert.IsTrue(Pos('.', VersionStr) > 0,
      'GetWinVerNetServer should return version in Major.Minor format. Got: ' + VersionStr)
  else
    Assert.Pass('GetWinVerNetServer returned Unknown OS (may indicate network service issue)');
end;


procedure TTestWinVersionApi.Test_GetWinVerNetServer_NotUnknownOS;
VAR
  VersionStr: string;
begin
  VersionStr:= GetWinVerNetServer;

  { On most systems, NetServerGetInfo should succeed }
  Assert.AreNotEqual('Unknown OS', VersionStr,
    'GetWinVerNetServer should return valid version on most systems');
end;


{ Cross-Method Consistency Tests }

procedure TTestWinVersionApi.Test_Consistency_GetWinVersion_MatchesNetServer;
VAR
  RtlVersion, NetVersion: string;
begin
  RtlVersion:= GetWinVersion;
  NetVersion:= GetWinVerNetServer;

  { Both should return same Major.Minor (e.g., "10.0") if both succeed }
  if (RtlVersion <> '0.0') and (NetVersion <> 'Unknown OS') then
    Assert.AreEqual(RtlVersion, NetVersion,
      'GetWinVersion and GetWinVerNetServer should return consistent results')
  else
    Assert.Pass('Skipped: one or both methods returned failure value');
end;


procedure TTestWinVersionApi.Test_Consistency_AllMethodsReturnModernWindows;
VAR
  MajVer, MinVer: Cardinal;
  OSName: string;
begin
  { All methods should indicate Windows 7 or later for Delphi 13 compatibility }
  GetWinVersion(MajVer, MinVer);
  OSName:= GetWinVersionEx;

  Assert.IsTrue(MajVer >= 6, 'RtlGetVersion major should be >= 6');

  { GetWinVersionEx should return Win7 or later }
  Assert.IsTrue(
    (OSName = '7') OR (OSName = '8') OR (OSName = '8.1') OR
    (OSName = '10') OR (OSName = '11') OR (OSName = '2008'),
    'GetWinVersionEx should return Win7 or later. Got: ' + OSName);
end;


{ GenerateReport Tests }

procedure TTestWinVersionApi.Test_GenerateReport_ReturnsNonEmptyString;
VAR
  Report: string;
begin
  Report:= GenerateReport;
  Assert.IsTrue(Report <> '', 'GenerateReport should return non-empty string');
end;


procedure TTestWinVersionApi.Test_GenerateReport_ContainsSysUtilsSection;
VAR
  Report: string;
begin
  Report:= GenerateReport;
  Assert.IsTrue(Pos('[SysUtils.Win32MajorVersion]', Report) > 0,
    'Report should contain SysUtils section');
end;


procedure TTestWinVersionApi.Test_GenerateReport_ContainsNetServerSection;
VAR
  Report: string;
begin
  Report:= GenerateReport;
  Assert.IsTrue(Pos('[GetWinVerNetServer]', Report) > 0,
    'Report should contain NetServer section');
end;


procedure TTestWinVersionApi.Test_GenerateReport_ContainsGetWinVersionSection;
VAR
  Report: string;
begin
  Report:= GenerateReport;
  Assert.IsTrue(Pos('[GetWinVersion]', Report) > 0,
    'Report should contain GetWinVersion section');
end;


procedure TTestWinVersionApi.Test_GenerateReport_ContainsGetWinVersionExSection;
VAR
  Report: string;
begin
  Report:= GenerateReport;
  Assert.IsTrue(Pos('[GetWinVersionEx]', Report) > 0,
    'Report should contain GetWinVersionEx section');
end;


procedure TTestWinVersionApi.Test_GenerateReport_HasSubstantialContent;
VAR
  Report: string;
begin
  Report:= GenerateReport;

  { Report should have substantial content - at least 200 chars with all 4 sections }
  Assert.IsTrue(Length(Report) > 200,
    'GenerateReport should return substantial content. Got length: ' + IntToStr(Length(Report)));
end;


{ Modern Windows Tests }

procedure TTestWinVersionApi.Test_ModernWindows_MajorVersionAtLeast6;
VAR
  MajVer, MinVer: Cardinal;
begin
  GetWinVersion(MajVer, MinVer);

  { Delphi 13 requires at least Windows Vista (6.0) }
  Assert.IsTrue(MajVer >= 6,
    'Delphi 13 requires at least Windows Vista (major version 6). Got: ' + IntToStr(MajVer));
end;


procedure TTestWinVersionApi.Test_ModernWindows_GetWinVersionEx_IsWin7OrLater;
VAR
  OSName: string;
begin
  OSName:= GetWinVersionEx;

  { Delphi 13 typically runs on Win7 or later }
  Assert.IsTrue(
    (OSName = '7') OR (OSName = '8') OR (OSName = '8.1') OR
    (OSName = '10') OR (OSName = '11'),
    'Expected Windows 7 or later. Got: ' + OSName);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestWinVersionApi);

end.
