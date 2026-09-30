unit Test.LightCore.Win.IO;

{=============================================================================================================
   Unit tests for LightCore.Win.IO.pas
   Tests the Windows special folders
=============================================================================================================}

interface
{$IFDEF MSWINDOWS}

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
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


initialization
  TDUnitX.RegisterTestFixture(TTestVclCommonIO);
{$ENDIF}

end.
