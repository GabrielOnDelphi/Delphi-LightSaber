unit Test.LightCore.WinVersion;

{=============================================================================================================
   Unit tests for LightCore.WinVersion.pas
   Tests Windows version detection functions.

   Note: These tests verify:
   - Functions return valid boolean values without crashing
   - Version detection hierarchy is consistent (if IsWindowsXUp, then IsWindowsX-1Up should also be true)
   - Utility functions return non-empty strings
   - Only one exact version function returns true at a time
   - Off Windows (compiled only when MSWINDOWS is not defined): every IsWindowsXX routine returns False
   The IsNTKernel test is in Test.LightVcl.Common.WinVersion.pas, next to the routine.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  LightCore.WinVersion;

type
  [TestFixture]
  TTestCoreWinVersion = class
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

    { Utility Function Tests }
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

    { Not Windows - compiled only when MSWINDOWS is not defined }
    {$IFNDEF MSWINDOWS}
    [Test]
    procedure Test_NotWindows_EveryIsWindowsRoutineIsFalse;
    {$ENDIF}
  end;


implementation


{ Exact Version Tests }

procedure TTestCoreWinVersion.Test_ExactVersions_AtMostOneTrue;
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

procedure TTestCoreWinVersion.Test_VersionHierarchy_VistaUp_ImpliesXPUp;
begin
  Assert.IsTrue(IsWindowsVistaUp, 'Delphi 13 requires at least Win7, so VistaUp must be True');
  Assert.IsTrue(IsWindowsXPUp, 'If VistaUp, then XPUp should also be true');
end;


procedure TTestCoreWinVersion.Test_VersionHierarchy_Win7Up_ImpliesVistaUp;
begin
  Assert.IsTrue(IsWindows7Up, 'Delphi 13 requires at least Win7');
  Assert.IsTrue(IsWindowsVistaUp, 'If Win7Up, then VistaUp should also be true');
end;


procedure TTestCoreWinVersion.Test_VersionHierarchy_Win8Up_ImpliesWin7Up;
begin
  if IsWindows8Up
  then Assert.IsTrue(IsWindows7Up, 'If Win8Up, then Win7Up should also be true')
  else Assert.IsFalse(IsWindows8Up, 'Win8Up is false - hierarchy not applicable');
end;


procedure TTestCoreWinVersion.Test_VersionHierarchy_Win10Up_ImpliesWin8Up;
begin
  if IsWindows10Up
  then Assert.IsTrue(IsWindows8Up, 'If Win10Up, then Win8Up should also be true')
  else Assert.IsFalse(IsWindows10Up, 'Win10Up is false - hierarchy not applicable');
end;


procedure TTestCoreWinVersion.Test_VersionHierarchy_Win11Up_ImpliesWin10Up;
begin
  if IsWindows11Up
  then Assert.IsTrue(IsWindows10Up, 'If Win11Up, then Win10Up should also be true')
  else Assert.IsFalse(IsWindows11Up, 'Win11Up is false - hierarchy not applicable');
end;


{ Utility Function Tests }

procedure TTestCoreWinVersion.Test_GetOSName_ReturnsNonEmptyString;
VAR
  OSName: string;
begin
  OSName:= GetOSName;
  Assert.IsTrue(OSName <> '', 'GetOSName should return non-empty string');
  Assert.IsTrue(Pos('Windows', OSName) > 0, 'GetOSName should contain "Windows"');
end;


procedure TTestCoreWinVersion.Test_GetOSDetails_ReturnsNonEmptyString;
VAR
  Details: string;
begin
  Details:= GetOSDetails;
  Assert.IsTrue(Details <> '', 'GetOSDetails should return non-empty string');
  Assert.IsTrue(Pos('Major', Details) > 0, 'GetOSDetails should contain version info');
end;


procedure TTestCoreWinVersion.Test_GenerateReport_ReturnsNonEmptyString;
VAR
  Report: string;
begin
  Report:= GenerateReport;
  Assert.IsTrue(Report <> '', 'GenerateReport should return non-empty string');
  Assert.IsTrue(Length(Report) > 100, 'GenerateReport should return substantial content');
end;


procedure TTestCoreWinVersion.Test_GenerateReport_ContainsExpectedSections;
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

procedure TTestCoreWinVersion.Test_ModernWindows_AtLeastWin7Up;
begin
  { Delphi 13 requires at least Windows 7 }
  Assert.IsTrue(IsWindows7Up, 'Delphi 13 requires at least Windows 7');
end;


{ Not Windows - compiled only when MSWINDOWS is not defined }

{$IFNDEF MSWINDOWS}
procedure TTestCoreWinVersion.Test_NotWindows_EveryIsWindowsRoutineIsFalse;
begin
  { Pins the IsWindowsOS guard of LightCore.WinVersion }
  Assert.IsFalse(IsWindowsXP,      'IsWindowsXP must be False off Windows');
  Assert.IsFalse(IsWindowsXPUp,    'IsWindowsXPUp must be False off Windows');
  Assert.IsFalse(IsWindowsVista,   'IsWindowsVista must be False off Windows');
  Assert.IsFalse(IsWindowsVistaUp, 'IsWindowsVistaUp must be False off Windows');
  Assert.IsFalse(IsWindows7,       'IsWindows7 must be False off Windows');
  Assert.IsFalse(IsWindows7Up,     'IsWindows7Up must be False off Windows');
  Assert.IsFalse(IsWindows8,       'IsWindows8 must be False off Windows');
  Assert.IsFalse(IsWindows8Up,     'IsWindows8Up must be False off Windows');
  Assert.IsFalse(IsWindows10,      'IsWindows10 must be False off Windows');
  Assert.IsFalse(IsWindows10Up,    'IsWindows10Up must be False off Windows');
  Assert.IsFalse(IsWindows11,      'IsWindows11 must be False off Windows');
  Assert.IsFalse(IsWindows11Up,    'IsWindows11Up must be False off Windows');
end;
{$ENDIF}


initialization
  TDUnitX.RegisterTestFixture(TTestCoreWinVersion);

end.
