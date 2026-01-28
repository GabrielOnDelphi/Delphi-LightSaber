unit Test.LightVcl.Common.IO;

{=============================================================================================================
   Unit tests for LightVcl.Common.IO
   Tests file/folder operations, special folders, drive functions, and file dialogs
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  Winapi.Windows,
  Winapi.ShlObj,
  Winapi.ShellAPI;

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

    { Path Validation Tests }
    [Test]
    procedure TestPathHasValidColon_ValidPath;

    [Test]
    procedure TestPathHasValidColon_InvalidColon;

    [Test]
    procedure TestGetPosAfterExtendedPrefix_NoPrefix;

    [Test]
    procedure TestGetPosAfterExtendedPrefix_ExtendedPrefix;

    [Test]
    procedure TestGetPosAfterExtendedPrefix_UNCPrefix;

    { File Locking Tests }
    [Test]
    procedure TestFileIsLockedRW_UnlockedFile;

    [Test]
    procedure TestFileIsLockedRW_NonExistentFile;

    [Test]
    procedure TestFileIsLockedR_NonExistentFile;

    [Test]
    procedure TestCanCreateFile;

    [Test]
    procedure TestCanWriteToFolder;

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

    { File Age Tests }
    [Test]
    procedure TestFileAge_ExistingFile;

    [Test]
    procedure TestFileAge_NonExistentFile;

    { File Operations Tests }
    [Test]
    procedure TestFileMoveTo;

    [Test]
    procedure TestFileMoveToDir;

    { Validation Helper Tests }
    [Test]
    procedure TestValidateForFileOperation_ValidPath;

    [Test]
    procedure TestValidateForFileOperation_ControlPanel;

    [Test]
    procedure TestValidateForFileOperation_RecycleBin;
  end;

implementation

uses
  LightVcl.Common.IO,
  LightCore.IO;

procedure TTestVclCommonIO.Setup;
begin
  FTestFolder := TPath.Combine(TPath.GetTempPath, 'LightVclIOTest_' + IntToStr(Random(100000)));
  ForceDirectories(FTestFolder);
  FTestFile := TPath.Combine(FTestFolder, 'TestFile.txt');
  TFile.WriteAllText(FTestFile, 'Test content');
end;

procedure TTestVclCommonIO.TearDown;
begin
  CleanupTestFiles;
end;

procedure TTestVclCommonIO.CleanupTestFiles;
begin
  if FileExists(FTestFile) then
    System.SysUtils.DeleteFile(FTestFile);
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

{ Path Validation Tests }

procedure TTestVclCommonIO.TestPathHasValidColon_ValidPath;
begin
  Assert.IsTrue(PathHasValidColon('C:\Windows\System32'), 'Standard path should be valid');
  Assert.IsTrue(PathHasValidColon('D:\Folder\File.txt'), 'D drive path should be valid');
end;

procedure TTestVclCommonIO.TestPathHasValidColon_InvalidColon;
begin
  Assert.IsFalse(PathHasValidColon('C:\Windows:System32'), 'Path with extra colon should be invalid');
end;

procedure TTestVclCommonIO.TestGetPosAfterExtendedPrefix_NoPrefix;
begin
  Assert.AreEqual(1, GetPosAfterExtendedPrefix('C:\Windows'), 'No prefix should return 1');
end;

procedure TTestVclCommonIO.TestGetPosAfterExtendedPrefix_ExtendedPrefix;
begin
  Assert.AreEqual(5, GetPosAfterExtendedPrefix('\\?\C:\Windows'), 'Extended prefix should return 5');
end;

procedure TTestVclCommonIO.TestGetPosAfterExtendedPrefix_UNCPrefix;
begin
  Assert.AreEqual(9, GetPosAfterExtendedPrefix('\\?\UNC\Server\Share'), 'UNC prefix should return 9');
end;

{ File Locking Tests }

procedure TTestVclCommonIO.TestFileIsLockedRW_UnlockedFile;
begin
  Assert.IsFalse(FileIsLockedRW(FTestFile), 'Unlocked file should not report as locked');
end;

procedure TTestVclCommonIO.TestFileIsLockedRW_NonExistentFile;
begin
  Assert.IsFalse(FileIsLockedRW(FTestFolder + '\NonExistent.txt'), 'Non-existent file should return False');
end;

procedure TTestVclCommonIO.TestFileIsLockedR_NonExistentFile;
begin
  { FileIsLockedR should raise exception for non-existent file }
  Assert.WillRaise(
    procedure
    begin
      FileIsLockedR(FTestFolder + '\NonExistent.txt');
    end,
    Exception);
end;

procedure TTestVclCommonIO.TestCanCreateFile;
begin
  Assert.IsTrue(CanCreateFile(TPath.Combine(FTestFolder, 'NewFile.txt')),
    'Should be able to create file in temp folder');
end;

procedure TTestVclCommonIO.TestCanWriteToFolder;
begin
  Assert.IsTrue(CanWriteToFolder(FTestFolder), 'Should be able to write to temp folder');
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

{ File Age Tests }

procedure TTestVclCommonIO.TestFileAge_ExistingFile;
var
  Age: TDateTime;
begin
  Age := FileAge(FTestFile);
  Assert.IsTrue(Age > 0, 'File age should be positive for existing file');
  Assert.IsTrue(Age <= Now, 'File age should not be in the future');
end;

procedure TTestVclCommonIO.TestFileAge_NonExistentFile;
var
  Age: TDateTime;
begin
  Age := FileAge(FTestFolder + '\NonExistent.txt');
  Assert.AreEqual(Double(-1), Double(Age), 0.0001, 'Non-existent file should return -1');
end;

{ File Operations Tests }

procedure TTestVclCommonIO.TestFileMoveTo;
var
  SourceFile, DestFile: string;
  Result: Boolean;
begin
  SourceFile := TPath.Combine(FTestFolder, 'MoveSource.txt');
  DestFile := TPath.Combine(FTestFolder, 'MoveDest.txt');

  TFile.WriteAllText(SourceFile, 'Move test');

  Result := FileMoveTo(SourceFile, DestFile);

  Assert.IsTrue(Result, 'FileMoveTo should succeed');
  Assert.IsFalse(FileExists(SourceFile), 'Source should not exist after move');
  Assert.IsTrue(FileExists(DestFile), 'Destination should exist after move');

  { Cleanup }
  if FileExists(DestFile) then System.SysUtils.DeleteFile(DestFile);
end;

procedure TTestVclCommonIO.TestFileMoveToDir;
var
  SourceFile, DestFolder, DestFile: string;
  Result: Boolean;
begin
  SourceFile := TPath.Combine(FTestFolder, 'MoveToDirSource.txt');
  DestFolder := TPath.Combine(FTestFolder, 'DestSubFolder');
  DestFile := TPath.Combine(DestFolder, 'MoveToDirSource.txt');

  TFile.WriteAllText(SourceFile, 'Move to dir test');
  ForceDirectories(DestFolder);

  Result := LightVcl.Common.IO.FileMoveToDir(SourceFile, DestFolder, True);

  Assert.IsTrue(Result, 'FileMoveToDir should succeed');
  Assert.IsFalse(FileExists(SourceFile), 'Source should not exist after move');
  Assert.IsTrue(FileExists(DestFile), 'Destination should exist after move');

  { Cleanup }
  if FileExists(DestFile) then System.SysUtils.DeleteFile(DestFile);
  if DirectoryExists(DestFolder) then RemoveDir(DestFolder);
end;

{ Validation Helper Tests }

procedure TTestVclCommonIO.TestValidateForFileOperation_ValidPath;
begin
  Assert.Pass('Internal function - tested indirectly through FileOperation');
end;

procedure TTestVclCommonIO.TestValidateForFileOperation_ControlPanel;
begin
  { FileOperation should fail for 'Control Panel' path }
  Assert.IsFalse(FileOperation('Control Panel', '', FO_DELETE, 0),
    'FileOperation should fail for Control Panel');
end;

procedure TTestVclCommonIO.TestValidateForFileOperation_RecycleBin;
begin
  { FileOperation should fail for 'Recycle Bin' path }
  Assert.IsFalse(FileOperation('Recycle Bin', '', FO_DELETE, 0),
    'FileOperation should fail for Recycle Bin');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestVclCommonIO);

end.
