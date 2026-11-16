unit Test.LightCore.IO.RecycleCheck;

{=============================================================================================================
   2026.11.15
   Unit tests for the Recycle Bin checks of LightCore.IO.pas: NoRecycleBinReason, DriveHasRecycleBin, RecycleItemRefusal,
   and for FolderDeleteRefusal on other names of a protected folder (a junction, a subst letter, an 8.3 short name, a prefix,
   a stream name, the share named Users).
   These routines delete nothing, so this fixture is registered for every run and may name real protected folders.
   The tests of RecycleItem itself, which deletes, are in Test.LightVcl.Common.IO.RecycleItem.pas,
   registered only with the switch --delete-tests.

   Two tests make a subst drive for a moment (subst.exe <free letter>: <folder>) and remove it again.
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

    { FolderDeleteRefusal on another name of a protected folder. It deletes nothing, so it may see real protected folders }
    [Test]
    procedure FolderDeleteRefusal_ProfileThroughJunction_Refused;
    [Test]
    procedure FolderDeleteRefusal_ProfileThroughSubstOfDriveRoot_Refused;
    [Test]
    procedure FolderDeleteRefusal_ShortNames_Refused;
    [Test]
    procedure FolderDeleteRefusal_DeviceAndSharePrefixes_Refused;
    [Test]
    procedure RecycleItemRefusal_JunctionThatDeniesReading_Refused;
    [Test]
    procedure FolderDeleteRefusal_StreamName_Refused;
    [Test]
    procedure RecycleItemRefusal_WildcardInsideJunction_Refused;
    [Test]
    procedure FolderDeleteRefusal_UsersShare_Refused;
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

  TRY
    { Inside the TRY: when runProgram gives up after 10 s, subst may still map the letter, and the FINALLY must remove it }
    Assert.IsTrue(runProgram('subst.exe '+ Letter+ ' "'+ Target+ '"'), 'Precondition: subst.exe must map '+ Letter+ ' to '+ Target);
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




{ # FolderDeleteRefusal: other names of a protected folder }

{ The 8.3 short name of Path, read from Windows, or '' when Windows gives none }
function shortName(CONST Path: string): string;
VAR Len: DWORD;
begin
  Len:= GetShortPathName(PChar(Path), NIL, 0);   { the size needed, terminating null included }
  if Len = 0 then EXIT('');
  SetLength(Result, Len);
  Len:= GetShortPathName(PChar(Path), PChar(Result), Len);
  if (Len = 0) OR (Len >= DWORD(Length(Result))) then EXIT('');
  SetLength(Result, Len);
end;


{ The junction "Documents and Settings" -> Users, which Windows makes on every install.
  Through it, the real profile has a name the guard cannot tell from an ordinary folder without reading the disk. }
function junctionToUsers: string;
VAR Attr: DWORD;
begin
  Result:= systemDrive + '\Documents and Settings';
  Attr:= GetFileAttributes(PChar(Result));
  Assert.IsTrue((Attr <> INVALID_FILE_ATTRIBUTES) AND ((Attr AND FILE_ATTRIBUTE_REPARSE_POINT) <> 0), 'Precondition: '+ Result+ ' must be a junction');
end;


procedure TTestRecycleCheck.FolderDeleteRefusal_ProfileThroughJunction_Refused;
VAR
  Alias, P: string;
  Paths: TArray<string>;
begin
  Assert.IsTrue(SameText(ExtractFileDir(profile), systemDrive + '\Users'), 'Precondition: the profile must be in '+ systemDrive + '\Users: '+ profile);
  Alias:= TPath.Combine(junctionToUsers, ExtractFileName(profile));
  Assert.IsTrue(System.SysUtils.DirectoryExists(Alias), 'Precondition: the profile must be reachable through the junction: '+ Alias);

  { Folders that exist: both guards must refuse them }
  Paths:= [Alias, Alias + '\', TPath.Combine(Alias, 'AppData')];
  for P in Paths do
   begin
    Assert.IsTrue(System.SysUtils.DirectoryExists(P), 'Precondition: '+ P+ ' must exist');
    Assert.AreNotEqual('', FolderDeleteRefusal(P), 'FolderDeleteRefusal must refuse the profile reached through a junction: "'+ P+ '"');
    Assert.AreNotEqual('', RecycleItemRefusal(P),  'RecycleItemRefusal must refuse the profile reached through a junction: "'+ P+ '"');
   end;

  { A folder directly inside the profile that does not exist: still refused by its place }
  P:= TPath.Combine(Alias, newName);
  Assert.AreNotEqual('', FolderDeleteRefusal(P), 'FolderDeleteRefusal must refuse a folder directly inside the profile reached through a junction: "'+ P+ '"');
end;


{ subst <letter>: C:\ gives the whole system drive a second letter, so <letter>:\Users\<name> is the real profile }
procedure TTestRecycleCheck.FolderDeleteRefusal_ProfileThroughSubstOfDriveRoot_Refused;
VAR
  Letter, Alias, P: string;
  Paths: TArray<string>;
begin
  Letter:= freeDriveLetter;
  Assert.IsNotEmpty(Letter, 'Precondition: a free drive letter');
  Assert.IsTrue(SameText(ExtractFileDir(profile), systemDrive + '\Users'), 'Precondition: the profile must be in '+ systemDrive + '\Users: '+ profile);

  TRY
    { Inside the TRY: when runProgram gives up after 10 s, subst may still map the letter, and the FINALLY must remove it }
    Assert.IsTrue(runProgram('subst.exe '+ Letter+ ' '+ systemDrive + '\'), 'Precondition: subst.exe must map '+ Letter+ ' to '+ systemDrive + '\');
    Alias:= Letter + '\Users\'+ ExtractFileName(profile);
    Assert.IsTrue(System.SysUtils.DirectoryExists(Alias), 'Precondition: the profile must be reachable through the subst drive: '+ Alias);

    Paths:= [Alias, TPath.Combine(Alias, 'AppData')];
    for P in Paths do
     begin
      Assert.AreNotEqual('', FolderDeleteRefusal(P), 'FolderDeleteRefusal must refuse the profile reached through a subst drive: "'+ P+ '"');
      Assert.AreNotEqual('', RecycleItemRefusal(P),  'RecycleItemRefusal must refuse the profile reached through a subst drive: "'+ P+ '"');
     end;
  FINALLY
    runProgram('subst.exe '+ Letter+ ' /D');
  END;
  Assert.IsFalse(System.SysUtils.DirectoryExists(Letter + '\'), 'The test must remove its subst drive '+ Letter);
end;


procedure TTestRecycleCheck.FolderDeleteRefusal_ShortNames_Refused;
VAR
  Long, Short: string;
  Tested: Integer;
  Paths: TArray<string>;
begin
  Paths:= [TPath.Combine(junctionToUsers, ExtractFileName(profile)),   { C:\DOCUME~1\<name> - the profile }
           GetEnvironmentVariable('ProgramFiles'),
           GetEnvironmentVariable('ProgramData')];
  Tested:= 0;
  for Long in Paths do
   begin
    Short:= shortName(Long);
    if (Short = '') OR SameText(Short, Long) then Continue;   { 8.3 names may be switched off on the drive }
    Inc(Tested);
    Assert.AreNotEqual('', FolderDeleteRefusal(Short), 'FolderDeleteRefusal must refuse the short name "'+ Short+ '" of "'+ Long+ '"');
   end;
  if Tested = 0
  then Assert.Pass('No 8.3 short name on this drive: nothing to test');
end;


procedure TTestRecycleCheck.FolderDeleteRefusal_DeviceAndSharePrefixes_Refused;
VAR
  Name, Letter, P: string;
  Paths: TArray<string>;
begin
  Name  := ExtractFileName(profile);
  Letter:= systemDrive[1];
  Assert.IsTrue(SameText(ExtractFileDir(profile), systemDrive + '\Users'), 'Precondition: the profile must be in '+ systemDrive + '\Users: '+ profile);

  Paths:= ['\\.\'+ systemDrive + '\Users\'+ Name,
           '\\?\'+ systemDrive + '\Users\'+ Name + '\Documents',
           '\\localhost\'+ Letter + '$\Users\'+ Name,
           '\\localhost\'+ Letter + '$\Users\'+ Name + '\AppData',
           '\\?\UNC\localhost\'+ Letter + '$\Users\'+ Name,
           '\\localhost\'+ Letter + '$\Windows',
           '\\localhost\'+ Letter + '$\Program Files'];
  for P in Paths do
    Assert.AreNotEqual('', FolderDeleteRefusal(P), 'FolderDeleteRefusal must refuse "'+ P+ '"');
end;


{ C:\Documents and Settings denies reading to everyone, so a folder test that opens it for reading answers "not a folder".
  RecycleItemRefusal must still see a folder, a junction directly under the drive root, and refuse it }
procedure TTestRecycleCheck.RecycleItemRefusal_JunctionThatDeniesReading_Refused;
VAR Junction: string;
begin
  Junction:= junctionToUsers;
  Assert.AreNotEqual('', RecycleItemRefusal(Junction), 'RecycleItemRefusal must refuse the junction "'+ Junction+ '"');
  Assert.AreNotEqual('', RecycleItemRefusal(Junction + '\'), 'RecycleItemRefusal must refuse the junction "'+ Junction+ '\"');
end;




{ # FolderDeleteRefusal, RecycleItemRefusal: a stream name, a wildcard inside a junction, the share named Users }

{ The attributes Windows reports for Path, read without the code under test. INVALID_FILE_ATTRIBUTES when the name names nothing }
function attributesOf(CONST Path: string): DWORD;
begin
  Result:= GetFileAttributes(PChar(Path));
end;


function isFolderName(CONST Path: string): Boolean;
VAR Attr: DWORD;
begin
  Attr:= attributesOf(Path);
  Result:= (Attr <> INVALID_FILE_ATTRIBUTES) AND ((Attr AND FILE_ATTRIBUTE_DIRECTORY) <> 0);
end;


{ NTFS opens a folder under the name of its index stream too: C:\Users\Name::$INDEX_ALLOCATION is the profile itself,
  and GetFileAttributes reports it as a folder. ExtractFileDir splits such a name at the colon (C:\Users\Name::),
  so neither the place rules nor the disk pass see the real parent folder.
  The stream name of the drive root has no folder precondition: inside this test program GetFileAttributes fails on it
  with ERROR_INVALID_NAME (123), while PowerShell gets a folder for the same name. It must be refused by its shape either way }
procedure TTestRecycleCheck.FolderDeleteRefusal_StreamName_Refused;
VAR
  P, Missing: string;
  Paths: TArray<string>;
begin
  Paths:= [GetEnvironmentVariable('SystemRoot') + '::$INDEX_ALLOCATION',    { the Windows folder }
           profile + '::$INDEX_ALLOCATION',                                 { the profile }
           profile + ':$I30:$INDEX_ALLOCATION',                             { the profile, the index named in full }
           TPath.Combine(profile, 'Documents') + '::$INDEX_ALLOCATION'];    { a folder directly inside the profile }
  for P in Paths do
   begin
    Assert.IsTrue(isFolderName(P), 'Precondition: Windows must report "'+ P+ '" as a folder. Attributes: $'+ IntToHex(attributesOf(P), 8));
    Assert.AreNotEqual('', FolderDeleteRefusal(P), 'FolderDeleteRefusal must refuse the stream name "'+ P+ '"');
    Assert.AreNotEqual('', RecycleItemRefusal(P),  'RecycleItemRefusal must refuse the stream name "'+ P+ '"');
    Assert.AreNotEqual('', RecycleItemRefusal(P + '\*'), 'RecycleItemRefusal must refuse a wildcard inside the stream name "'+ P+ '"');
   end;

  { A stream name is refused by its shape, also where Windows reports no folder under it }
  Missing:= tempFolder + newName + '::$INDEX_ALLOCATION';
  Assert.IsFalse(isFolderName(Missing), 'Precondition: '+ Missing+ ' must not exist');
  Paths:= [systemDrive + '\::$INDEX_ALLOCATION',    { the drive root }
           Missing];
  for P in Paths do
   begin
    Assert.AreNotEqual('', FolderDeleteRefusal(P), 'FolderDeleteRefusal must refuse the stream name "'+ P+ '"');
    Assert.AreNotEqual('', RecycleItemRefusal(P),  'RecycleItemRefusal must refuse the stream name "'+ P+ '"');
   end;
end;


{ C:\ProgramData\Desktop and C:\ProgramData\Documents are junctions Windows makes for old programs,
  to C:\Users\Public\Desktop and C:\Users\Public\Documents. Neither junction is a protected place by its name,
  but a wildcard inside one names the content of its target. RecycleItemRefusal refuses a folder that is itself a junction;
  the wildcard form must be refused the same way.
  The precondition reads the attributes, not DirectoryExists, which with FollowLink TRUE opens the link for reading }
procedure TTestRecycleCheck.RecycleItemRefusal_WildcardInsideJunction_Refused;
VAR
  J: string;
  Attr: DWORD;
  Junctions: TArray<string>;
begin
  Junctions:= [TPath.Combine(GetEnvironmentVariable('ProgramData'), 'Desktop'),
               TPath.Combine(GetEnvironmentVariable('ProgramData'), 'Documents')];
  for J in Junctions do
   begin
    Attr:= attributesOf(J);
    Assert.IsTrue((Attr <> INVALID_FILE_ATTRIBUTES) AND ((Attr AND FILE_ATTRIBUTE_DIRECTORY) <> 0) AND ((Attr AND FILE_ATTRIBUTE_REPARSE_POINT) <> 0),
                  'Precondition: '+ J+ ' must be a junction');
    Assert.AreNotEqual('', RecycleItemRefusal(J), 'Precondition: RecycleItemRefusal refuses the junction itself: "'+ J+ '"');
    Assert.AreNotEqual('', RecycleItemRefusal(J + '\*'), 'RecycleItemRefusal must refuse a wildcard inside the junction "'+ J+ '"');
   end;
end;


{ Windows can share C:\Users under the name Users. Over that share every profile has a UNC name (\\localhost\Users\Name),
  and no pass of FolderDeleteRefusal maps that name back to C:\Users.
  The profile itself, directly under the share root, is already refused; a folder directly inside the profile must be refused too.
  Skipped where the share is not reachable, so the test holds whichever way the guard learns the share }
procedure TTestRecycleCheck.FolderDeleteRefusal_UsersShare_Refused;
VAR
  Name, Share, P: string;
  Paths: TArray<string>;
begin
  Name := ExtractFileName(profile);
  Share:= '\\localhost\Users\'+ Name;
  Assert.IsTrue(SameText(ExtractFileDir(profile), systemDrive + '\Users'), 'Precondition: the profile must be in '+ systemDrive + '\Users: '+ profile);
  if NOT isFolderName(Share) then
   begin
    Assert.Pass('The share \\localhost\Users is not reachable on this PC: nothing to test');
    EXIT;
   end;

  Paths:= [Share + '\Documents',
           Share + '\AppData',
           '\\127.0.0.1\Users\'+ Name + '\Documents'];
  for P in Paths do
   begin
    Assert.IsTrue(isFolderName(P), 'Precondition: '+ P+ ' must exist');
    Assert.AreNotEqual('', FolderDeleteRefusal(P), 'FolderDeleteRefusal must refuse a folder directly inside a profile reached over the Users share: "'+ P+ '"');
    Assert.AreNotEqual('', RecycleItemRefusal(P),  'RecycleItemRefusal must refuse a folder directly inside a profile reached over the Users share: "'+ P+ '"');
   end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestRecycleCheck);

end.
