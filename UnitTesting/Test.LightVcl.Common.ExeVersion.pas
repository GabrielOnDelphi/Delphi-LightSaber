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
  NotepadPath: string;
begin
  { notepad.exe should have version info }
  NotepadPath:= GetEnvironmentVariable('WINDIR') + '\System32\notepad.exe';

  Success:= GetVersionInfoFile(NotepadPath, FixedInfo);
  Assert.IsTrue(Success, 'Should successfully get version info from notepad.exe');
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
  NotepadPath: string;
begin
  NotepadPath:= GetEnvironmentVariable('WINDIR') + '\System32\notepad.exe';

  Version:= GetVersionInfo(NotepadPath);
  Assert.IsNotEmpty(Version, 'Version should not be empty');
  Assert.IsTrue(Pos('.', Version) > 0, 'Version should contain dots');
end;


procedure TTestExeVersion.Test_GetVersionInfo_WithBuildNo;
VAR
  VersionShort, VersionLong: string;
  NotepadPath: string;
begin
  NotepadPath:= GetEnvironmentVariable('WINDIR') + '\System32\notepad.exe';

  VersionShort:= GetVersionInfo(NotepadPath, False);
  VersionLong:= GetVersionInfo(NotepadPath, True);

  Assert.IsTrue(Length(VersionLong) > Length(VersionShort),
    'Version with build should be longer');
end;


procedure TTestExeVersion.Test_GetVersionInfo_FormatCheck;
VAR
  Version: string;
  NotepadPath: string;
  DotCount: Integer;
  i: Integer;
begin
  NotepadPath:= GetEnvironmentVariable('WINDIR') + '\System32\notepad.exe';

  { Without build number: should have 2 dots (Major.Minor.Release) }
  Version:= GetVersionInfo(NotepadPath, False);
  DotCount:= 0;
  for i:= 1 to Length(Version) do
    if Version[i] = '.'
    then Inc(DotCount);
  Assert.AreEqual(2, DotCount, 'Version without build should have 2 dots');

  { With build number: should have 3 dots (Major.Minor.Release.Build) }
  Version:= GetVersionInfo(NotepadPath, True);
  DotCount:= 0;
  for i:= 1 to Length(Version) do
    if Version[i] = '.'
    then Inc(DotCount);
  Assert.AreEqual(3, DotCount, 'Version with build should have 3 dots');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestExeVersion);

end.
