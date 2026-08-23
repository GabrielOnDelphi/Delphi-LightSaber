unit Test.LightVcl.Common.ExeVersion;

{=============================================================================================================
   Unit tests for LightVcl.Common.ExeVersion.pas
   Tests executable version info retrieval functions.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Winapi.Windows,
  LightVcl.Common.ExeVersion;

type
  [TestFixture]
  TTestExeVersion = class
  private
    function VersionedFile: string;
  public
    { GetVersionInfoFile tests }
    [Test]
    procedure Test_GetVersionInfoFile_EmptyFileName;

    [Test]
    procedure Test_GetVersionInfoFile_NonExistentFile;

    [Test]
    procedure Test_GetVersionInfoFile_ValidExe;

    { GetVersionInfo tests }
    [Test]
    procedure Test_GetVersionInfo_EmptyFileName;

    [Test]
    procedure Test_GetVersionInfo_NonExistentFile;

    [Test]
    procedure Test_GetVersionInfo_ValidExe;

    [Test]
    procedure Test_GetVersionInfo_WithBuildNo;

    [Test]
    procedure Test_GetVersionInfo_FormatCheck;
  end;

implementation


{-------------------------------------------------------------------------------------------------------------
   These tests used to hardcode %WINDIR%\System32\notepad.exe. On Windows 11 that file is GONE: notepad ships as
   a Store app, and the only thing left under that name is an execution alias in
   %LOCALAPPDATA%\Microsoft\WindowsApps, which is a zero-length reparse point carrying no version resource.
   Measured 2026-08-22: the file is absent from System32, SysWOW64 and the Windows folder alike, and
   GetFileVersionInfoSize returns 0 with LastError = 2 (ERROR_FILE_NOT_FOUND).

   The test EXE itself is not usable as a stand-in either - it is built with no version resource, even though
   the .dproj carries VerInfo_IncludeVerInfo = true.

   So probe a short list instead of trusting one name. explorer.exe is the natural first choice (a real EXE, and
   outside System32, so WOW64 redirection cannot move it); the two DLLs are there only so a machine without a
   desktop shell still has a subject. Selection is by FileExists alone - asking GetVersionInfoFile which candidate
   works would make the test verify itself.
-------------------------------------------------------------------------------------------------------------}
function TTestExeVersion.VersionedFile: string;
CONST
   Candidates: array[0..2] of string = ('\explorer.exe', '\System32\kernel32.dll', '\System32\shell32.dll');
VAR
   WinDir: string;
   i: Integer;
begin
  WinDir:= GetEnvironmentVariable('WINDIR');

  for i:= Low(Candidates) to High(Candidates) do
    if FileExists(WinDir + Candidates[i])
    then EXIT(WinDir + Candidates[i]);

  Result:= '';
end;


{ GetVersionInfoFile tests }

procedure TTestExeVersion.Test_GetVersionInfoFile_EmptyFileName;
VAR
  FixedInfo: TVSFixedFileInfo;
begin
  Assert.WillRaise(
    procedure
    begin
      GetVersionInfoFile('', FixedInfo);
    end,
    Exception);
end;


procedure TTestExeVersion.Test_GetVersionInfoFile_NonExistentFile;
VAR
  FixedInfo: TVSFixedFileInfo;
  Success: Boolean;
begin
  { Non-existent file should return False (not raise) }
  Success:= GetVersionInfoFile('C:\NonExistent\Invalid.exe', FixedInfo);
  Assert.IsFalse(Success);
end;


procedure TTestExeVersion.Test_GetVersionInfoFile_ValidExe;
VAR
  FixedInfo: TVSFixedFileInfo;
  Success: Boolean;
  Subject: string;
begin
  Subject:= VersionedFile;
  Assert.IsNotEmpty(Subject, 'No system file with version info was found - see VersionedFile');

  Success:= GetVersionInfoFile(Subject, FixedInfo);
  Assert.IsTrue(Success, 'Should successfully get version info from ' + Subject);
  Assert.IsTrue(FixedInfo.dwFileVersionMS > 0, 'Version should be non-zero');
end;


{ GetVersionInfo tests }

procedure TTestExeVersion.Test_GetVersionInfo_EmptyFileName;
begin
  Assert.WillRaise(
    procedure
    begin
      GetVersionInfo('');
    end,
    Exception);
end;


procedure TTestExeVersion.Test_GetVersionInfo_NonExistentFile;
begin
  { Non-existent file should raise exception }
  Assert.WillRaise(
    procedure
    begin
      GetVersionInfo('C:\NonExistent\Invalid.exe');
    end,
    Exception);
end;


procedure TTestExeVersion.Test_GetVersionInfo_ValidExe;
VAR
  Version: string;
  Subject: string;
begin
  Subject:= VersionedFile;
  Assert.IsNotEmpty(Subject, 'No system file with version info was found - see VersionedFile');

  Version:= GetVersionInfo(Subject);
  Assert.IsNotEmpty(Version, 'Version should not be empty');
  Assert.IsTrue(Pos('.', Version) > 0, 'Version should contain dots');
end;


procedure TTestExeVersion.Test_GetVersionInfo_WithBuildNo;
VAR
  VersionShort, VersionLong: string;
  Subject: string;
begin
  Subject:= VersionedFile;
  Assert.IsNotEmpty(Subject, 'No system file with version info was found - see VersionedFile');

  VersionShort:= GetVersionInfo(Subject, False);
  VersionLong := GetVersionInfo(Subject, True);

  Assert.IsTrue(Length(VersionLong) > Length(VersionShort),
    'Version with build should be longer');
  Assert.StartsWith(VersionShort, VersionLong, 'The long form must be the short form plus the build number');
end;


procedure TTestExeVersion.Test_GetVersionInfo_FormatCheck;
VAR
  Version: string;
  Subject: string;
  DotCount: Integer;
  i: Integer;
begin
  Subject:= VersionedFile;
  Assert.IsNotEmpty(Subject, 'No system file with version info was found - see VersionedFile');

  { Without build number: should have 2 dots (Major.Minor.Release) }
  Version:= GetVersionInfo(Subject, False);
  DotCount:= 0;
  for i:= 1 to Length(Version) do
    if Version[i] = '.'
    then Inc(DotCount);
  Assert.AreEqual(2, DotCount, 'Version without build should have 2 dots');

  { With build number: should have 3 dots (Major.Minor.Release.Build) }
  Version:= GetVersionInfo(Subject, True);
  DotCount:= 0;
  for i:= 1 to Length(Version) do
    if Version[i] = '.'
    then Inc(DotCount);
  Assert.AreEqual(3, DotCount, 'Version with build should have 3 dots');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestExeVersion);

end.
