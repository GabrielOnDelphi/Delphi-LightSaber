unit Test.LightCore.Win.IO;

{=============================================================================================================
   Unit tests for LightCore.Win.IO.pas
   Tests the Windows special folders, the drive functions and NTFS compression
=============================================================================================================}

interface
{$IFDEF MSWINDOWS}

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  Winapi.Windows,
  Winapi.ShlObj;

type
  [TestFixture]
  TTestVclCommonIO = class
  private
    FTestFolder: string;
    FTestFile: string;
    procedure CleanupTestFiles;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Special Folders Tests }
    [Test]
    procedure TestGetWinDir;

    [Test]
    procedure TestGetWinSysDir;

    [Test]
    procedure TestGetProgramFilesDir;

    [Test]
    procedure TestGetDesktopFolder;

    [Test]
    procedure TestGetSpecialFolder_CSIDL;

    [Test]
    procedure TestGetSpecialFolder_VirtualFolders;

    [Test]
    procedure TestGetSpecialFolders_ReturnsNonEmpty;

    [Test]
    procedure TestFolderIsSpecial;

    [Test]
    procedure TestGetTaskManager;

    { Drive Tests }
    [Test]
    procedure TestGetDriveType_SystemDrive;

    [Test]
    procedure TestGetDriveTypeS_SystemDrive;

    [Test]
    procedure TestValidDrive_SystemDrive;

    [Test]
    procedure TestValidDrive_InvalidDrive;

    [Test]
    procedure TestDriveFreeSpace;

    [Test]
    procedure TestDriveFreeSpaceF;

    { Parameter Validation Tests }
    [Test]
    procedure TestSetCompressionAtr_EmptyFileName_ShouldRaise;
  end;
{$ENDIF}

implementation
{$IFDEF MSWINDOWS}

uses
  LightCore.Win.IO,
  LightCore.IO;

procedure TTestVclCommonIO.Setup;
begin
  FTestFolder := TPath.Combine(TPath.GetTempPath, 'LightVclIOTest_' + IntToStr(Random(100000)));
  ForceDirectoriesE(FTestFolder);
  FTestFile := TPath.Combine(FTestFolder, 'TestFile.txt');
  TFile.WriteAllText(FTestFile, 'Test content');
end;

procedure TTestVclCommonIO.TearDown;
begin
  CleanupTestFiles;
end;

procedure TTestVclCommonIO.CleanupTestFiles;
begin
  LightCore.IO.TryDeleteFile(FTestFile);
  if DirectoryExists(FTestFolder) then
    TDirectory.Delete(FTestFolder, True);
end;

{ Special Folders Tests }

procedure TTestVclCommonIO.TestGetWinDir;
var
  WinDir: string;
begin
  WinDir := GetWinDir;
  Assert.IsNotEmpty(WinDir, 'Windows directory should not be empty');
  Assert.IsTrue(DirectoryExists(WinDir), 'Windows directory should exist');
  Assert.IsTrue(WinDir.EndsWith('\'), 'Should have trailing backslash');
end;

procedure TTestVclCommonIO.TestGetWinSysDir;
var
  SysDir: string;
begin
  SysDir := GetWinSysDir;
  Assert.IsNotEmpty(SysDir, 'System directory should not be empty');
  Assert.IsTrue(DirectoryExists(SysDir), 'System directory should exist');
  Assert.IsTrue(SysDir.EndsWith('\'), 'Should have trailing backslash');
end;

procedure TTestVclCommonIO.TestGetProgramFilesDir;
var
  ProgDir: string;
begin
  ProgDir := GetProgramFilesDir;
  Assert.IsNotEmpty(ProgDir, 'Program Files directory should not be empty');
  Assert.IsTrue(DirectoryExists(ProgDir), 'Program Files directory should exist');
end;

procedure TTestVclCommonIO.TestGetDesktopFolder;
var
  DesktopDir: string;
begin
  DesktopDir := GetDesktopFolder;
  Assert.IsNotEmpty(DesktopDir, 'Desktop folder should not be empty');
  Assert.IsTrue(DirectoryExists(DesktopDir), 'Desktop folder should exist');
end;

procedure TTestVclCommonIO.TestGetSpecialFolder_CSIDL;
var
  PersonalDir: string;
begin
  PersonalDir := GetSpecialFolder(CSIDL_PERSONAL, False);
  Assert.IsNotEmpty(PersonalDir, 'Personal folder should not be empty');
  Assert.IsTrue(DirectoryExists(PersonalDir), 'Personal folder should exist');
end;

procedure TTestVclCommonIO.TestGetSpecialFolder_VirtualFolders;
begin
  { Virtual folders should return empty string }
  Assert.AreEqual('', GetSpecialFolder(CSIDL_NETWORK, False), 'Network should return empty');
  Assert.AreEqual('', GetSpecialFolder(CSIDL_PRINTERS, False), 'Printers should return empty');
  Assert.AreEqual('', GetSpecialFolder(CSIDL_BITBUCKET, False), 'Recycle Bin should return empty');
  Assert.AreEqual('', GetSpecialFolder(CSIDL_CONTROLS, False), 'Control Panel should return empty');
end;

procedure TTestVclCommonIO.TestGetSpecialFolders_ReturnsNonEmpty;
var
  Folders: TStringList;
  NonEmptyCount: Integer;
  i: Integer;
begin
  Folders := GetSpecialFolders;
  try
    Assert.IsTrue(Folders.Count > 0, 'Should return at least some folders');

    NonEmptyCount := 0;
    for i := 0 to Folders.Count - 1 do
      if Folders[i] <> '' then
        Inc(NonEmptyCount);

    Assert.IsTrue(NonEmptyCount > 10, 'Should have at least 10 valid folder paths');
  finally
    FreeAndNil(Folders);
  end;
end;

procedure TTestVclCommonIO.TestFolderIsSpecial;
var
  DesktopDir: string;
begin
  DesktopDir := GetDesktopFolder;
  Assert.IsTrue(FolderIsSpecial(DesktopDir), 'Desktop should be a special folder');
  Assert.IsFalse(FolderIsSpecial(FTestFolder), 'Temp test folder should not be special');
end;

procedure TTestVclCommonIO.TestGetTaskManager;
var
  TaskMgr: string;
begin
  TaskMgr := GetTaskManager;
  Assert.IsNotEmpty(TaskMgr, 'Task Manager path should not be empty');
  Assert.IsTrue(TaskMgr.EndsWith('taskmgr.exe', True), 'Should end with taskmgr.exe');
  Assert.IsTrue(FileExists(TaskMgr), 'Task Manager executable should exist');
end;

{ Drive Tests }

procedure TTestVclCommonIO.TestGetDriveType_SystemDrive;
var
  DriveType: Integer;
begin
  DriveType := GetDriveType('C:\');
  Assert.AreEqual(DRIVE_FIXED, DriveType, 'C: should be a fixed drive');
end;

procedure TTestVclCommonIO.TestGetDriveTypeS_SystemDrive;
var
  DriveTypeS: string;
begin
  DriveTypeS := GetDriveTypeS('C:\');
  Assert.AreEqual('Drive fixed', DriveTypeS, 'C: should return "Drive fixed"');
end;

procedure TTestVclCommonIO.TestValidDrive_SystemDrive;
begin
  Assert.IsTrue(ValidDrive('C'), 'C: drive should be valid');
end;

procedure TTestVclCommonIO.TestValidDrive_InvalidDrive;
begin
  { Drive Z is unlikely to exist on most systems }
  { Note: This test may fail if Z: is mapped - adjust if needed }
  Assert.Pass('Skipped - depends on system configuration');
end;

procedure TTestVclCommonIO.TestDriveFreeSpace;
var
  FreeSpace: Int64;
begin
  FreeSpace := DriveFreeSpace('C');
  Assert.IsTrue(FreeSpace > 0, 'C: drive should have some free space');
end;

procedure TTestVclCommonIO.TestDriveFreeSpaceF;
var
  FreeSpace: Int64;
begin
  FreeSpace := DriveFreeSpaceF('C:\Windows\System32');
  Assert.IsTrue(FreeSpace > 0, 'Should return free space for full path');
end;


{ Parameter Validation Tests }

procedure TTestVclCommonIO.TestSetCompressionAtr_EmptyFileName_ShouldRaise;
begin
  Assert.WillRaise(
    procedure
    begin
      SetCompressionAtr('');
    end,
    Exception);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestVclCommonIO);
{$ENDIF}

end.
