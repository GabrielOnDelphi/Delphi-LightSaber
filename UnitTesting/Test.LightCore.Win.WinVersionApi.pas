unit Test.LightCore.Win.WinVersionApi;

{=============================================================================================================
   Unit tests for LightCore.Win.WinVersionApi.pas
   Tests alternative Windows version detection functions using various APIs.

   2026.10.07
   The expected version comes from TOSVersion, which reads the registry, not the three APIs tested here.
=============================================================================================================}

interface
{$IFDEF MSWINDOWS}

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Winapi.Windows,
  LightCore.Win.WinVersionApi;

type
  [TestFixture]
  TTestWinVersionApi = class
  public
    { GetWinVersion Tests (RtlGetVersion API) }
    [Test]
    procedure Test_GetWinVersion_Proc_ReturnsValidVersion;

    [Test]
    procedure Test_GetWinVersion_Func_MatchesTOSVersion;

    [Test]
    procedure Test_GetWinVersion_Proc_MatchesTOSVersion;

    [Test]
    procedure Test_GetWinVersion_MajorAtLeast6;

    { GetWinVersionEx Tests (GetVersionEx API) }
    [Test]
    procedure Test_GetWinVersionEx_MatchesBuildNumber;

    [Test]
    procedure Test_GetWinVersionEx_NotUnknownOS;

    { GetWinVerNetServer Tests (NetServerGetInfo API) }
    [Test]
    procedure Test_GetWinVerNetServer_MatchesTOSVersion;

    [Test]
    procedure Test_GetWinVerNetServer_NotUnknownOS;

    { Cross-Method Consistency Tests }
    [Test]
    procedure Test_Consistency_GetWinVersion_MatchesNetServer;

    [Test]
    procedure Test_Consistency_AllMethodsReturnModernWindows;

    { GenerateReport Tests }
    [Test]
    procedure Test_GenerateReport_HoldsEachMethodValue;

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
{$ENDIF}


implementation
{$IFDEF MSWINDOWS}


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


{ TOSVersion reads Major/Minor from the registry (CurrentMajorVersionNumber), not through RtlGetVersion, so it is an independent source }
function ExpectedMajorMinor: string;
begin
  Result:= IntToStr(TOSVersion.Major) + '.' + IntToStr(TOSVersion.Minor);
end;


procedure TTestWinVersionApi.Test_GetWinVersion_Func_MatchesTOSVersion;
begin
  Assert.AreEqual(ExpectedMajorMinor, GetWinVersion, 'GetWinVersion must return Major.Minor of the running Windows');
end;


procedure TTestWinVersionApi.Test_GetWinVersion_Proc_MatchesTOSVersion;
VAR
  MajVer, MinVer: Cardinal;
begin
  GetWinVersion(MajVer, MinVer);
  Assert.AreEqual(TOSVersion.Major, Integer(MajVer), 'Major version');
  Assert.AreEqual(TOSVersion.Minor, Integer(MinVer), 'Minor version');
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

{ GetWinVersionEx names the Windows that GetVersionEx reports to THIS exe. Without a Windows 10 entry in the manifest GetVersionEx reports 6.2 build 9200 (Windows 8), so the expected name is computed from the build GetVersionEx itself returns.
  Release builds: 7 = 7600, 8 = 9200, 8.1 = 9600, 10 = 10240, 11 = 22000. }
procedure TTestWinVersionApi.Test_GetWinVersionEx_MatchesBuildNumber;
VAR
  Info: TOSVersionInfo;
  Build: Cardinal;
  Expected: string;
begin
  Info:= Default(TOSVersionInfo);
  Info.dwOSVersionInfoSize:= SizeOf(Info);
  Assert.IsTrue(GetVersionEx(Info), 'GetVersionEx failed');
  Build:= Info.dwBuildNumber AND $FFFF;

  if Build >= 22000 then Expected:= '11'  else
  if Build >= 10240 then Expected:= '10'  else
  if Build >=  9600 then Expected:= '8.1' else
  if Build >=  9200 then Expected:= '8'   else
  if Build >=  7600 then Expected:= '7'
  else Expected:= '';

  if Expected = ''
  then Assert.Pass('Older than Windows 7 (build ' + IntToStr(Build) + ') - no expected name')
  else Assert.AreEqual(Expected, GetWinVersionEx, 'GetWinVersionEx must name the Windows of build ' + IntToStr(Build));
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

procedure TTestWinVersionApi.Test_GetWinVerNetServer_MatchesTOSVersion;
begin
  Assert.AreEqual(ExpectedMajorMinor, GetWinVerNetServer, 'GetWinVerNetServer must return Major.Minor of the running Windows');
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

procedure TTestWinVersionApi.Test_GenerateReport_HoldsEachMethodValue;
CONST
  CRLF = #13#10;
  TAB  = #9;
VAR
  Report: string;
begin
  Report:= GenerateReport;
  Assert.IsTrue(Pos('[GetWinVerNetServer]' + CRLF + TAB + GetWinVerNetServer + CRLF, Report) > 0, 'Report must hold the GetWinVerNetServer value. Report: ' + Report);
  Assert.IsTrue(Pos('[GetWinVersion]'      + CRLF + TAB + GetWinVersion      + CRLF, Report) > 0, 'Report must hold the GetWinVersion value. Report: ' + Report);
  Assert.IsTrue(Pos('[GetWinVersionEx]'    + CRLF + TAB + GetWinVersionEx    + CRLF, Report) > 0, 'Report must hold the GetWinVersionEx value. Report: ' + Report);
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

  { The old test demanded more than 200 characters. That number was a guess: the four sections
    together are only 122 characters on Windows 11, because the values in them are short.
    What the report must really contain is one heading per detection method. }
  Assert.IsTrue(Pos('[SysUtils.Win32MajorVersion]', Report) > 0, 'Report must hold the SysUtils.Win32MajorVersion section');
  Assert.IsTrue(Pos('[GetWinVerNetServer]'       , Report) > 0, 'Report must hold the GetWinVerNetServer section');
  Assert.IsTrue(Pos('[GetWinVersion]'            , Report) > 0, 'Report must hold the GetWinVersion section');
  Assert.IsTrue(Pos('[GetWinVersionEx]'          , Report) > 0, 'Report must hold the GetWinVersionEx section');
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
{$ENDIF}

end.
