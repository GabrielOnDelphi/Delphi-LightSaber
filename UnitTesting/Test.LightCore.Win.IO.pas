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
  Winapi.ActiveX,
  Winapi.KnownFolders,
  LightCore.Win.IO,
  LightCore.IO;


{ A variable of the process environment block. Qualified, because Winapi.Windows also declares a GetEnvironmentVariable (the API, with three parameters) }
function EnvVar(CONST Name: string): string;
begin
  Result:= System.SysUtils.GetEnvironmentVariable(Name);
end;


{ TRUE if both paths name the same folder: compared without case and without a trailing backslash }
function SamePath(CONST Path1, Path2: string): Boolean;
begin
  Result:= SameText(ExcludeTrailingPathDelimiter(Path1), ExcludeTrailingPathDelimiter(Path2));
end;


{ The path of a known folder, read through SHGetKnownFolderPath, a Shell API that LightCore.Win.IO does not call.
  The Shell allocates the string; the caller frees it with CoTaskMemFree. }
function KnownFolderPath(CONST FolderID: TGUID): string;
VAR
  Path: PWideChar;
  Res: HRESULT;
begin
  Path:= NIL;
  Res:= SHGetKnownFolderPath(FolderID, 0, 0, Path);
  TRY
    Assert.AreEqual(S_OK, Res, 'SHGetKnownFolderPath failed');
    Result:= Path;
  FINALLY
    CoTaskMemFree(Path);
  END;
end;


{ TRUE if List holds Path (compared as SamePath does). FALSE for an empty Path, which would match an empty line }
function ListHasPath(List: TStrings; CONST Path: string): Boolean;
VAR Item: string;
begin
  if Path = '' then EXIT(FALSE);
  for Item in List do
    if SamePath(Item, Path)
    then EXIT(TRUE);
  Result:= FALSE;
end;


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
  Assert.IsNotEmpty(EnvVar('WINDIR'), 'Precondition: the WINDIR environment variable');
  Assert.IsTrue(SamePath(EnvVar('WINDIR'), WinDir), 'GetWinDir must return %WINDIR%. Got: ' + WinDir);
  Assert.IsTrue(DirectoryExists(WinDir), 'Windows directory should exist');
  Assert.IsTrue(WinDir.EndsWith('\'), 'Should have trailing backslash');
end;

procedure TTestVclCommonIO.TestGetWinSysDir;
var
  SysDir: string;
begin
  SysDir := GetWinSysDir;
  Assert.IsNotEmpty(EnvVar('WINDIR'), 'Precondition: the WINDIR environment variable');
  Assert.IsTrue(SamePath(EnvVar('WINDIR') + '\System32', SysDir), 'GetWinSysDir must return %WINDIR%\System32. Got: ' + SysDir);
  Assert.IsTrue(DirectoryExists(SysDir), 'System directory should exist');
  Assert.IsTrue(SysDir.EndsWith('\'), 'Should have trailing backslash');
end;

{ GetProgramFilesDir reads the registry. The expected value comes from the process environment block instead }
procedure TTestVclCommonIO.TestGetProgramFilesDir;
var
  ProgDir: string;
begin
  ProgDir := GetProgramFilesDir;
  Assert.IsNotEmpty(EnvVar('ProgramFiles'), 'Precondition: the ProgramFiles environment variable');
  Assert.IsTrue(SamePath(EnvVar('ProgramFiles'), ProgDir), 'GetProgramFilesDir must return %ProgramFiles% (' + EnvVar('ProgramFiles') + '). Got: ' + ProgDir);
  Assert.IsTrue(ProgDir.EndsWith('\'), 'Should have trailing backslash');
  Assert.IsTrue(DirectoryExists(ProgDir), 'Program Files directory should exist');
end;

procedure TTestVclCommonIO.TestGetDesktopFolder;
var
  DesktopDir: string;
begin
  DesktopDir := GetDesktopFolder;
  Assert.IsTrue(SamePath(KnownFolderPath(FOLDERID_Desktop), DesktopDir), 'GetDesktopFolder must return the known folder Desktop (' + KnownFolderPath(FOLDERID_Desktop) + '). Got: ' + DesktopDir);
  Assert.IsTrue(DesktopDir.EndsWith('\'), 'Should have trailing backslash');
  Assert.IsTrue(DirectoryExists(DesktopDir), 'Desktop folder should exist');
end;

procedure TTestVclCommonIO.TestGetSpecialFolder_CSIDL;
var
  PersonalDir: string;
begin
  PersonalDir := GetSpecialFolder(CSIDL_PERSONAL, False);
  Assert.IsTrue(SamePath(KnownFolderPath(FOLDERID_Documents), PersonalDir), 'CSIDL_PERSONAL must give the known folder Documents (' + KnownFolderPath(FOLDERID_Documents) + '). Got: ' + PersonalDir);
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
    { GetSpecialFolders adds one line per CSIDL it asks for, 48 of them, also when a folder does not exist on this PC (an empty line) }
    Assert.AreEqual(48, Folders.Count, 'One line per CSIDL');

    NonEmptyCount := 0;
    for i := 0 to Folders.Count - 1 do
      if Folders[i] <> '' then
        Inc(NonEmptyCount);

    Assert.IsTrue(NonEmptyCount > 10, 'Should have at least 10 valid folder paths');

    { Folders that exist on every Windows PC, compared with values that do not come from GetSpecialFolder }
    Assert.IsTrue(ListHasPath(Folders, EnvVar('WINDIR')),       'CSIDL_WINDOWS = %WINDIR%');
    Assert.IsTrue(ListHasPath(Folders, EnvVar('APPDATA')),      'CSIDL_APPDATA = %APPDATA%');
    Assert.IsTrue(ListHasPath(Folders, EnvVar('LOCALAPPDATA')), 'CSIDL_LOCAL_APPDATA = %LOCALAPPDATA%');
    Assert.IsTrue(ListHasPath(Folders, EnvVar('PROGRAMDATA')),  'CSIDL_COMMON_APPDATA = %PROGRAMDATA%');
    Assert.IsTrue(ListHasPath(Folders, EnvVar('USERPROFILE')),  'CSIDL_PROFILE = %USERPROFILE%');
    Assert.IsTrue(ListHasPath(Folders, KnownFolderPath(FOLDERID_Documents)),    'CSIDL_PERSONAL = the known folder Documents');
    Assert.IsTrue(ListHasPath(Folders, KnownFolderPath(FOLDERID_Desktop)),      'CSIDL_DESKTOPDIRECTORY = the known folder Desktop');
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
VAR
  Drives: DWORD;
  Letter: Char;
begin
  { GetLogicalDrives has one bit per drive letter in use (bit 0 = A:). Pick a letter that is not in use. }
  Drives:= Winapi.Windows.GetLogicalDrives;   { The bit mask, not the LightCore.Win.IO list of the same name }
  for Letter:= 'Z' downto 'D' do
    if (Drives AND (DWORD(1) shl (Ord(Letter) - Ord('A')))) = 0 then
      begin
        Assert.IsFalse(ValidDrive(Letter), Letter + ': is not in use, so it must not be a valid drive');
        EXIT;
      end;

  Assert.Pass('Every drive letter from D: to Z: is in use');
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
