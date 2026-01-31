unit Test.LightVcl.Common.WinVersion;

{=============================================================================================================
   Unit tests for LightVcl.Common.WinVersion.pas
   Tests Windows version detection functions.

   Note: These tests verify:
   - Functions return valid boolean values without crashing
   - Version detection hierarchy is consistent (if IsWindowsXUp, then IsWindowsX-1Up should also be true)
   - Utility functions return non-empty strings
   - Only one exact version function returns true at a time
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  LightVcl.Common.WinVersion;

type
  [TestFixture]
  TTestWinVersion = class
  public
    { Exact Version Tests - verify mutual exclusivity }
    [Test]
    procedure Test_ExactVersions_AtMostOneTrue;

    { Version Hierarchy Tests - verify consistency }
    [Test]
    procedure Test_VersionHierarchy_VistaUp_ImpliesXPUp;

    [Test]
    procedure Test_VersionHierarchy_Win7Up_ImpliesVistaUp;

    [Test]
    procedure Test_VersionHierarchy_Win8Up_ImpliesWin7Up;

    [Test]
    procedure Test_VersionHierarchy_Win10Up_ImpliesWin8Up;

    [Test]
    procedure Test_VersionHierarchy_Win11Up_ImpliesWin10Up;

    { Individual Version Function Tests }
    [Test]
    procedure Test_IsWindowsXP_ReturnsBoolean;

    [Test]
    procedure Test_IsWindowsXPUp_ReturnsBoolean;

    [Test]
    procedure Test_IsWindowsVista_ReturnsBoolean;

    [Test]
    procedure Test_IsWindowsVistaUp_ReturnsBoolean;

    [Test]
    procedure Test_IsWindows7_ReturnsBoolean;

    [Test]
    procedure Test_IsWindows7Up_ReturnsBoolean;

    [Test]
    procedure Test_IsWindows8_ReturnsBoolean;

    [Test]
    procedure Test_IsWindows8Up_ReturnsBoolean;

    [Test]
    procedure Test_IsWindows10_ReturnsBoolean;

    [Test]
    procedure Test_IsWindows10Up_ReturnsBoolean;

    [Test]
    procedure Test_IsWindows11_ReturnsBoolean;

    [Test]
    procedure Test_IsWindows11Up_ReturnsBoolean;

    { Utility Function Tests }
    [Test]
    procedure Test_IsNTKernel_ReturnsTrue;

    [Test]
    procedure Test_GetOSName_ReturnsNonEmptyString;

    [Test]
    procedure Test_GetOSDetails_ReturnsNonEmptyString;

    [Test]
    procedure Test_GenerateReport_ReturnsNonEmptyString;

    [Test]
    procedure Test_GenerateReport_ContainsExpectedSections;

    { Modern Windows Tests - we should be at least Windows 7 for Delphi 13 }
    [Test]
    procedure Test_ModernWindows_AtLeastWin7Up;
  end;


implementation


{ Exact Version Tests }

procedure TTestWinVersion.Test_ExactVersions_AtMostOneTrue;
VAR
  TrueCount: Integer;
begin
  { Only one exact version function should return true }
  TrueCount:= 0;
  if IsWindowsXP    then Inc(TrueCount);
  if IsWindowsVista then Inc(TrueCount);
  if IsWindows7     then Inc(TrueCount);
  if IsWindows8     then Inc(TrueCount);
  if IsWindows10    then Inc(TrueCount);
  if IsWindows11    then Inc(TrueCount);

  Assert.IsTrue(TrueCount <= 1,
    'At most one exact version function should return True. Found: ' + IntToStr(TrueCount));
end;


{ Version Hierarchy Tests }

procedure TTestWinVersion.Test_VersionHierarchy_VistaUp_ImpliesXPUp;
begin
  if IsWindowsVistaUp
  then Assert.IsTrue(IsWindowsXPUp, 'If VistaUp, then XPUp should also be true');
  Assert.Pass('Hierarchy check passed');
end;


procedure TTestWinVersion.Test_VersionHierarchy_Win7Up_ImpliesVistaUp;
begin
  if IsWindows7Up
  then Assert.IsTrue(IsWindowsVistaUp, 'If Win7Up, then VistaUp should also be true');
  Assert.Pass('Hierarchy check passed');
end;


procedure TTestWinVersion.Test_VersionHierarchy_Win8Up_ImpliesWin7Up;
begin
  if IsWindows8Up
  then Assert.IsTrue(IsWindows7Up, 'If Win8Up, then Win7Up should also be true');
  Assert.Pass('Hierarchy check passed');
end;


procedure TTestWinVersion.Test_VersionHierarchy_Win10Up_ImpliesWin8Up;
begin
  if IsWindows10Up
  then Assert.IsTrue(IsWindows8Up, 'If Win10Up, then Win8Up should also be true');
  Assert.Pass('Hierarchy check passed');
end;


procedure TTestWinVersion.Test_VersionHierarchy_Win11Up_ImpliesWin10Up;
begin
  if IsWindows11Up
  then Assert.IsTrue(IsWindows10Up, 'If Win11Up, then Win10Up should also be true');
  Assert.Pass('Hierarchy check passed');
end;


{ Individual Version Function Tests }

procedure TTestWinVersion.Test_IsWindowsXP_ReturnsBoolean;
VAR
  Result: Boolean;
begin
  Result:= IsWindowsXP;
  { Just verify it returns a valid boolean without crashing }
  Assert.Pass('IsWindowsXP returned: ' + BoolToStr(Result, True));
end;


procedure TTestWinVersion.Test_IsWindowsXPUp_ReturnsBoolean;
VAR
  Result: Boolean;
begin
  Result:= IsWindowsXPUp;
  Assert.Pass('IsWindowsXPUp returned: ' + BoolToStr(Result, True));
end;


procedure TTestWinVersion.Test_IsWindowsVista_ReturnsBoolean;
VAR
  Result: Boolean;
begin
  Result:= IsWindowsVista;
  Assert.Pass('IsWindowsVista returned: ' + BoolToStr(Result, True));
end;


procedure TTestWinVersion.Test_IsWindowsVistaUp_ReturnsBoolean;
VAR
  Result: Boolean;
begin
  Result:= IsWindowsVistaUp;
  Assert.Pass('IsWindowsVistaUp returned: ' + BoolToStr(Result, True));
end;


procedure TTestWinVersion.Test_IsWindows7_ReturnsBoolean;
VAR
  Result: Boolean;
begin
  Result:= IsWindows7;
  Assert.Pass('IsWindows7 returned: ' + BoolToStr(Result, True));
end;


procedure TTestWinVersion.Test_IsWindows7Up_ReturnsBoolean;
VAR
  Result: Boolean;
begin
  Result:= IsWindows7Up;
  Assert.Pass('IsWindows7Up returned: ' + BoolToStr(Result, True));
end;


procedure TTestWinVersion.Test_IsWindows8_ReturnsBoolean;
VAR
  Result: Boolean;
begin
  Result:= IsWindows8;
  Assert.Pass('IsWindows8 returned: ' + BoolToStr(Result, True));
end;


procedure TTestWinVersion.Test_IsWindows8Up_ReturnsBoolean;
VAR
  Result: Boolean;
begin
  Result:= IsWindows8Up;
  Assert.Pass('IsWindows8Up returned: ' + BoolToStr(Result, True));
end;


procedure TTestWinVersion.Test_IsWindows10_ReturnsBoolean;
VAR
  Result: Boolean;
begin
  Result:= IsWindows10;
  Assert.Pass('IsWindows10 returned: ' + BoolToStr(Result, True));
end;


procedure TTestWinVersion.Test_IsWindows10Up_ReturnsBoolean;
VAR
  Result: Boolean;
begin
  Result:= IsWindows10Up;
  Assert.Pass('IsWindows10Up returned: ' + BoolToStr(Result, True));
end;


procedure TTestWinVersion.Test_IsWindows11_ReturnsBoolean;
VAR
  Result: Boolean;
begin
  Result:= IsWindows11;
  Assert.Pass('IsWindows11 returned: ' + BoolToStr(Result, True));
end;


procedure TTestWinVersion.Test_IsWindows11Up_ReturnsBoolean;
VAR
  Result: Boolean;
begin
  Result:= IsWindows11Up;
  Assert.Pass('IsWindows11Up returned: ' + BoolToStr(Result, True));
end;


{ Utility Function Tests }

procedure TTestWinVersion.Test_IsNTKernel_ReturnsTrue;
begin
  { All modern Windows versions (2000+) use NT kernel }
  Assert.IsTrue(IsNTKernel, 'Modern Windows should use NT kernel');
end;


procedure TTestWinVersion.Test_GetOSName_ReturnsNonEmptyString;
VAR
  OSName: string;
begin
  OSName:= GetOSName;
  Assert.IsTrue(OSName <> '', 'GetOSName should return non-empty string');
  Assert.IsTrue(Pos('Windows', OSName) > 0, 'GetOSName should contain "Windows"');
end;


procedure TTestWinVersion.Test_GetOSDetails_ReturnsNonEmptyString;
VAR
  Details: string;
begin
  Details:= GetOSDetails;
  Assert.IsTrue(Details <> '', 'GetOSDetails should return non-empty string');
  Assert.IsTrue(Pos('Major', Details) > 0, 'GetOSDetails should contain version info');
end;


procedure TTestWinVersion.Test_GenerateReport_ReturnsNonEmptyString;
VAR
  Report: string;
begin
  Report:= GenerateReport;
  Assert.IsTrue(Report <> '', 'GenerateReport should return non-empty string');
  Assert.IsTrue(Length(Report) > 100, 'GenerateReport should return substantial content');
end;


procedure TTestWinVersion.Test_GenerateReport_ContainsExpectedSections;
VAR
  Report: string;
begin
  Report:= GenerateReport;
  Assert.IsTrue(Pos('[GetWinVersion Is]', Report) > 0, 'Report should contain version section');
  Assert.IsTrue(Pos('[GetOSDetails]', Report) > 0, 'Report should contain details section');
  Assert.IsTrue(Pos('IsWindowsXP', Report) > 0, 'Report should contain IsWindowsXP');
  Assert.IsTrue(Pos('IsWindows11', Report) > 0, 'Report should contain IsWindows11');
end;


{ Modern Windows Tests }

procedure TTestWinVersion.Test_ModernWindows_AtLeastWin7Up;
begin
  { Delphi 13 requires at least Windows 7 }
  Assert.IsTrue(IsWindows7Up, 'Delphi 13 requires at least Windows 7');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestWinVersion);

end.
