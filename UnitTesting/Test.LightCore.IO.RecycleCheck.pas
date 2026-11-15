unit Test.LightCore.IO.RecycleCheck;

{=============================================================================================================
   2026.11.14
   Unit tests for the Recycle Bin checks of LightCore.IO.pas: NoRecycleBinReason, DriveHasRecycleBin, RecycleItemRefusal.
   These routines delete nothing, so this fixture is registered for every run and may name real protected folders.
   The tests of RecycleItem itself, which deletes, are in Test.LightVcl.Common.IO.RecycleItem.pas,
   registered only with the switch --delete-tests.

   One test makes a subst drive for a moment (subst.exe <free letter>: <Temp folder>) and removes it again.
   subst only maps a drive letter: it creates and deletes no file.

   Run only this fixture:
     C:\Projects\LightSaber\UnitTesting\Tests_LightCore.exe --run:Test.LightCore.IO.RecycleCheck.TTestRecycleCheck
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  Winapi.Windows,
  System.SysUtils,
  System.IOUtils,
  LightCore.IO;

type
  [TestFixture]
  TTestRecycleCheck = class
  private
    function tempFolder: string;
    function profile: string;
  public
    { NoRecycleBinReason, DriveHasRecycleBin }
    [Test]
    procedure DriveHasRecycleBin_SystemDriveRoot_True;
    [Test]
    procedure DriveHasRecycleBin_MissingFileOnSystemDrive_True;
    [Test]
    procedure DriveHasRecycleBin_LongPathPrefix_True;
    [Test]
    procedure DriveHasRecycleBin_EmptyPath_False;
    [Test]
    procedure DriveHasRecycleBin_UnknownDriveLetter_False;
    [Test]
    procedure DriveHasRecycleBin_UncPath_False;
    [Test]
    procedure DriveHasRecycleBin_SubstDrive_False;

    { RecycleItemRefusal }
    [Test]
    procedure RecycleItemRefusal_NoPath_Refused;
    [Test]
    procedure RecycleItemRefusal_ProtectedFolders_Refused;
    [Test]
    procedure RecycleItemRefusal_OneProtectedInList_RefusesAll;
    [Test]
    procedure RecycleItemRefusal_WildcardOverProtectedFolder_Refused;
    [Test]
    procedure RecycleItemRefusal_WildcardOutsideFileName_Refused;
    [Test]
    procedure RecycleItemRefusal_WildcardInOrdinaryFolder_Accepted;
    [Test]
    procedure RecycleItemRefusal_LongPathPrefix_Refused;
    [Test]
    procedure RecycleItemRefusal_UncFile_Accepted;
    [Test]
    procedure RecycleItemRefusal_FileDirectlyUnderDriveRoot_Accepted;
    [Test]
    procedure RecycleItemRefusal_OrdinaryFolder_Accepted;
    [Test]
    procedure RecycleItemRefusal_RelativePath_ExpandedInPathList;
    [Test]
    procedure RecycleItemRefusal_List_DoubleNullNoTrailingBackslash;
  end;


implementation


{ The system drive as 'C:'. }
function systemDrive: string;
begin
  Result:= GetEnvironmentVariable('SystemDrive');
  if Result = ''
  then Result:= 'C:';
end;


{ A drive letter that Windows does not use now ('Q:'), or '' when all are used. Read from GetLogicalDrives, not from the code under test. }
function freeDriveLetter: string;
VAR
  Used: DWORD;
  Letter: Char;
begin
  Used:= Winapi.Windows.GetLogicalDrives;   { LightCore.IO has a GetLogicalDrives of its own, which returns the drive names }
  for Letter:= 'Z' downto 'D' do
    if (Used AND (DWORD(1) shl (Ord(Letter) - Ord('A')))) = 0
    then EXIT(Letter + ':');
  Result:= '';
end;


{ Runs CmdLine with no window and waits up to 10 s. TRUE when the program ends with exit code 0. }
function runProgram(CONST CmdLine: string): Boolean;
VAR
  StartInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  Cmd: string;
  ProcExit: DWORD;
begin
  Cmd:= CmdLine;
  UniqueString(Cmd);   { CreateProcessW may write into the command-line buffer }
  FillChar(StartInfo, SizeOf(StartInfo), 0);
  StartInfo.cb:= SizeOf(StartInfo);
  if NOT CreateProcess(NIL, PChar(Cmd), NIL, NIL, FALSE, CREATE_NO_WINDOW, NIL, NIL, StartInfo, ProcInfo)
  then EXIT(FALSE);
  TRY
    WaitForSingleObject(ProcInfo.hProcess, 10000);
    Result:= GetExitCodeProcess(ProcInfo.hProcess, ProcExit) AND (ProcExit = 0);
  FINALLY
    CloseHandle(ProcInfo.hThread);
    CloseHandle(ProcInfo.hProcess);
  END;
end;


function newName: string;
begin
  Result:= 'ZZ-NoSuch-'+ TGUID.NewGuid.ToString;
end;


{ The Temp folder, with a trailing backslash }
function TTestRecycleCheck.tempFolder: string;
begin
  Result:= IncludeTrailingPathDelimiter(TPath.GetTempPath);
end;


function TTestRecycleCheck.profile: string;
begin
  Result:= GetEnvironmentVariable('USERPROFILE');
  Assert.IsNotEmpty(Result, 'Precondition: USERPROFILE must be set');
end;




{ # NoRecycleBinReason, DriveHasRecycleBin }

procedure TTestRecycleCheck.DriveHasRecycleBin_SystemDriveRoot_True;
begin
  Assert.AreEqual('', NoRecycleBinReason(systemDrive + '\'), 'The system drive is a fixed disk');
  Assert.IsTrue(DriveHasRecycleBin(systemDrive + '\'), 'The system drive has a Recycle Bin');
end;


{ GetVolumePathName ignores invalid trailing path elements, so a path that does not exist still names its volume }
procedure TTestRecycleCheck.DriveHasRecycleBin_MissingFileOnSystemDrive_True;
VAR Path: string;
begin
  Path:= systemDrive + '\'+ newName + '\file.txt';
  Assert.IsFalse(FileExists(Path), 'Precondition: '+ Path+ ' must not exist');
  Assert.IsTrue(DriveHasRecycleBin(Path), 'A missing file on the system drive is on a drive with a Recycle Bin: '+ Path);
end;


procedure TTestRecycleCheck.DriveHasRecycleBin_LongPathPrefix_True;
begin
  Assert.IsTrue(DriveHasRecycleBin('\\?\'+ systemDrive + '\Windows'), 'The \\?\ prefix is stripped before the drive check');
end;


procedure TTestRecycleCheck.DriveHasRecycleBin_EmptyPath_False;
begin
  Assert.IsFalse(DriveHasRecycleBin(''),    'An empty path has no Recycle Bin');
  Assert.IsFalse(DriveHasRecycleBin('   '), 'A path of spaces has no Recycle Bin');
end;


procedure TTestRecycleCheck.DriveHasRecycleBin_UnknownDriveLetter_False;
VAR
  Letter, Reason: string;
begin
  Letter:= freeDriveLetter;
  Assert.IsNotEmpty(Letter, 'Precondition: a free drive letter');

  Reason:= NoRecycleBinReason(Letter + '\file.txt');
  Assert.IsFalse(DriveHasRecycleBin(Letter + '\file.txt'), 'A drive letter Windows does not know has no Recycle Bin: '+ Letter);
  Assert.IsTrue(Pos(Letter, Reason) > 0, 'The reason must name the drive. Reason: '+ Reason);
end;


procedure TTestRecycleCheck.DriveHasRecycleBin_UncPath_False;
VAR
  P, Reason: string;
  Paths: TArray<string>;
begin
  Paths:= ['\\localhost\'+ systemDrive[1] + '$\Windows',
           '\\server\share\folder\file.txt',
           '\\?\UNC\localhost\'+ systemDrive[1] + '$\Windows'];
  for P in Paths do
   begin
    Reason:= NoRecycleBinReason(P);
    Assert.IsFalse(DriveHasRecycleBin(P), 'A UNC path has no Recycle Bin: '+ P);
    Assert.IsTrue(Pos('not on a drive letter', Reason) > 0, 'Wrong reason for '+ P+ ': '+ Reason);
   end;
end;


{ A subst letter reports DRIVE_FIXED, like C:, so only the QueryDosDevice check can refuse it.
  The test makes the subst drive itself, so it knows the letter is one. }
procedure TTestRecycleCheck.DriveHasRecycleBin_SubstDrive_False;
VAR
  Letter, Target, Reason: string;
begin
  Letter:= freeDriveLetter;
  Assert.IsNotEmpty(Letter, 'Precondition: a free drive letter');
  Target:= ExcludeTrailingPathDelimiter(TPath.GetTempPath);

  Assert.IsTrue(runProgram('subst.exe '+ Letter+ ' "'+ Target+ '"'), 'Precondition: subst.exe must map '+ Letter+ ' to '+ Target);
  TRY
    Assert.IsTrue(System.SysUtils.DirectoryExists(Letter + '\'), 'Precondition: the subst drive '+ Letter+ ' must exist');
    Assert.IsTrue(Winapi.Windows.GetDriveType(PChar(Letter + '\')) = DRIVE_FIXED, 'Precondition: a subst drive reports DRIVE_FIXED, so the drive type alone cannot refuse it');

    Reason:= NoRecycleBinReason(Letter + '\file.txt');
    Assert.IsFalse(DriveHasRecycleBin(Letter + '\file.txt'), 'A subst drive has no Recycle Bin: '+ Letter);
    Assert.IsTrue(Pos('subst', Reason) > 0, 'The reason must name subst. Reason: '+ Reason);
  FINALLY
    runProgram('subst.exe '+ Letter+ ' /D');
  END;
  Assert.IsFalse(System.SysUtils.DirectoryExists(Letter + '\'), 'The test must remove its subst drive '+ Letter);
end;




{ # RecycleItemRefusal }

procedure TTestRecycleCheck.RecycleItemRefusal_NoPath_Refused;
VAR
  P, PathList: string;
  Paths: TArray<string>;
begin
  Paths:= ['', #0, #0#0, '   '#0'  '];
  for P in Paths do
   begin
    Assert.AreNotEqual('', RecycleItemRefusal(P, PathList), 'A list with no path must be refused');
    Assert.AreEqual('', PathList, 'A refused list gives no PathList');
   end;
end;


procedure TTestRecycleCheck.RecycleItemRefusal_ProtectedFolders_Refused;
VAR
  P: string;
  Paths: TArray<string>;
begin
  Paths:= [systemDrive + '\',
           GetEnvironmentVariable('SystemRoot'),
           GetEnvironmentVariable('ProgramFiles'),
           ExtractFileDir(profile),                     { the Users folder }
           profile,
           profile + '\',
           TPath.Combine(profile, 'Documents')];
  for P in Paths do
   begin
    Assert.IsTrue(System.SysUtils.DirectoryExists(P), 'Precondition: '+ P+ ' must exist');
    Assert.AreNotEqual('', RecycleItemRefusal(P), 'RecycleItemRefusal must refuse the folder "'+ P+ '"');
   end;
end;


procedure TTestRecycleCheck.RecycleItemRefusal_OneProtectedInList_RefusesAll;
VAR PathList: string;
begin
  Assert.AreNotEqual('', RecycleItemRefusal(tempFolder + newName + '.txt'#0 + profile + #0, PathList), 'One protected folder in the list refuses the whole list');
  Assert.AreEqual('', PathList, 'A refused list gives no PathList');
end;


procedure TTestRecycleCheck.RecycleItemRefusal_WildcardOverProtectedFolder_Refused;
VAR
  P: string;
  Paths: TArray<string>;
begin
  Paths:= [profile + '\*',
           systemDrive + '\*.txt',
           TPath.Combine(profile, 'Documents\*')];
  for P in Paths do
    Assert.AreNotEqual('', RecycleItemRefusal(P), 'A wildcard over a protected folder must be refused: '+ P);
end;


procedure TTestRecycleCheck.RecycleItemRefusal_WildcardOutsideFileName_Refused;
VAR Reason: string;
begin
  Reason:= RecycleItemRefusal(tempFolder + '*\file.txt');
  Assert.IsTrue(Pos('wildcard', Reason) > 0, 'A wildcard in the folder part must be refused. Reason: '+ Reason);
end;


procedure TTestRecycleCheck.RecycleItemRefusal_WildcardInOrdinaryFolder_Accepted;
begin
  Assert.AreEqual('', RecycleItemRefusal(tempFolder + newName + '\*.txt'), 'A wildcard inside an ordinary folder is accepted');
end;


procedure TTestRecycleCheck.RecycleItemRefusal_LongPathPrefix_Refused;
VAR Reason: string;
begin
  Reason:= RecycleItemRefusal('\\?\'+ systemDrive + '\'+ newName + '\file.txt');
  Assert.IsTrue(Pos('prefix', Reason) > 0, 'SHFileOperation fails on the \\?\ prefix, so it must be refused. Reason: '+ Reason);
end;


{ A file is not given to the folder guard, and the drive is not checked, so a UNC file is accepted although it has no Recycle Bin }
procedure TTestRecycleCheck.RecycleItemRefusal_UncFile_Accepted;
VAR P: string;
begin
  P:= '\\localhost\'+ systemDrive[1] + '$\'+ newName + '\file.txt';
  Assert.IsFalse(DriveHasRecycleBin(P), 'Precondition: a UNC path has no Recycle Bin');
  Assert.AreEqual('', RecycleItemRefusal(P), 'A UNC file is accepted: RecycleItem deletes it for good');
end;


{ FolderDeleteRefusal refuses a FOLDER directly under a drive root; a FILE there must not be refused }
procedure TTestRecycleCheck.RecycleItemRefusal_FileDirectlyUnderDriveRoot_Accepted;
VAR P: string;
begin
  P:= systemDrive + '\'+ newName + '.txt';
  Assert.IsFalse(System.SysUtils.DirectoryExists(P), 'Precondition: '+ P+ ' must not be a folder');
  Assert.AreEqual('', RecycleItemRefusal(P), 'A file directly under the drive root is accepted');
end;


procedure TTestRecycleCheck.RecycleItemRefusal_OrdinaryFolder_Accepted;
VAR P: string;
begin
  P:= ExcludeTrailingPathDelimiter(TPath.GetTempPath);
  Assert.IsTrue(System.SysUtils.DirectoryExists(P), 'Precondition: '+ P+ ' must exist');
  Assert.AreEqual('', RecycleItemRefusal(P), 'The Temp folder is not a protected folder');
end;


{ Microsoft: a relative path in pFrom is deleted for good, even with FOF_ALLOWUNDO. So PathList must hold the full path }
procedure TTestRecycleCheck.RecycleItemRefusal_RelativePath_ExpandedInPathList;
VAR
  Name, PathList: string;
begin
  Name:= newName + '.txt';
  Assert.AreEqual('', RecycleItemRefusal(Name, PathList), 'A relative file name is accepted');
  Assert.AreEqual(IncludeTrailingPathDelimiter(GetCurrentDir) + Name + #0#0, PathList, 'PathList must hold the full path, then two #0');
end;


{ The shape BioniX passes: each path ended with #0. A folder keeps no trailing backslash }
procedure TTestRecycleCheck.RecycleItemRefusal_List_DoubleNullNoTrailingBackslash;
VAR
  Folder, FileName, PathList: string;
begin
  Folder  := tempFolder + newName;
  FileName:= tempFolder + newName + '.txt';
  Assert.AreEqual('', RecycleItemRefusal(Folder + '\'#0 + FileName + #0, PathList), 'A list of two ordinary paths is accepted');
  Assert.AreEqual(Folder + #0 + FileName + #0#0, PathList, 'PathList: each path ended with #0, a second #0 at the end, no trailing backslash');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestRecycleCheck);

end.
