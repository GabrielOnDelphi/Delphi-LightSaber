unit Test.LightCore.IO.DeleteGuard;

{=============================================================================================================
   2026.11.13
   Unit tests for the delete guard of LightCore.IO.pas: FolderDeleteRefusal, CheckFolderDeletable,
   DeleteFolder and EmptyDirectory (refused paths, junctions removed as links).

   THESE TESTS DELETE FILES. Claude never runs them: rule in C:\Projects\.claude\rules\delphi-code.md, section "Unit Testing".
   So the fixture is registered only when the runner gets the switch --delete-tests:
     C:\Projects\LightSaber\UnitTesting\Tests_LightCore.exe --delete-tests --run:Test.LightCore.IO.DeleteGuard
   Without the switch, a plain run, Run-Tests.ps1 and --run:Test.LightCore.IO never see this fixture.

   How the tests stay safe even when the guard is broken:
   - A protected folder that exists (the drive root, the Windows folder, the real profile) goes ONLY to
     FolderDeleteRefusal and CheckFolderDeletable, which delete nothing.
   - DeleteFolder and EmptyDirectory get only '' and protected-shaped paths that do NOT exist,
     so a broken guard deletes nothing and the test turns red.
   - Every path a test deletes lies inside FRoot, a folder the test made in this run under the Temp folder,
     and is checked against FRoot before the delete. TearDown removes only the names it made, never recursively.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  DUnitX.CommandLine.Options,
  Winapi.Windows,
  System.SysUtils,
  System.StrUtils,
  System.IOUtils,
  LightCore.IO;

type
  [TestFixture]
  TTestDeleteGuard = class
  private
    FRoot: string;       { The throwaway folder of this run. Everything a test deletes is inside it }
    FTarget: string;     { FRoot\Target - the folder the junctions point to }
    FKeep: string;       { FRoot\Target\keep.txt - must survive every delete }
    FVictim: string;     { FRoot\Victim - the folder that DeleteFolder / EmptyDirectory get }
    FLink: string;       { FRoot\Victim\link - a junction to FTarget }
    FRootLink: string;   { FRoot\RootLink - a junction to FTarget, given to the routines as the folder itself }
    procedure buildTarget;
    procedure buildVictim;
    procedure assertInsideRoot(CONST Path: string);
    procedure waitGone(CONST Path: string);
    function  missingProtectedPaths: TArray<string>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { FolderDeleteRefusal and CheckFolderDeletable delete nothing, so they may see real protected folders }
    [Test]
    procedure FolderDeleteRefusal_EmptyPath_Refused;
    [Test]
    procedure FolderDeleteRefusal_RealProtectedFolders_Refused;
    [Test]
    procedure FolderDeleteRefusal_MissingProtectedShapes_Refused;
    [Test]
    procedure FolderDeleteRefusal_OrdinaryFolders_Accepted;
    [Test]
    procedure CheckFolderDeletable_DriveRoot_RaisesAndNamesPath;
    [Test]
    procedure CheckFolderDeletable_OwnFolder_DeletesNothing;

    { The routines that delete: protected paths that do not exist only }
    [Test]
    procedure DeleteFolder_ProtectedPaths_RaiseBeforeAnyFileCall;
    [Test]
    procedure EmptyDirectory_ProtectedPaths_RaiseBeforeAnyFileCall;

    { Junctions, inside FRoot only }
    [Test]
    procedure DeleteFolder_JunctionInside_RemovesLinkKeepsTarget;
    [Test]
    procedure EmptyDirectory_JunctionInside_RemovesLinkKeepsTarget;
    [Test]
    procedure DeleteFolder_FolderIsJunction_RemovesOnlyTheLink;
    [Test]
    procedure EmptyDirectory_FolderIsJunction_Raises;
  end;


implementation


CONST
  DeleteTestsSwitch = 'delete-tests';


{ The system drive as 'C:'. }
function systemDrive: string;
begin
  Result:= GetEnvironmentVariable('SystemDrive');
  if Result = ''
  then Result:= 'C:';
end;


{ The root of a drive letter that Windows does not use now ('Q:\'), or '' when all are used. }
function freeDriveRoot: string;
VAR
  Used: DWORD;
  Letter: Char;
begin
  Used:= Winapi.Windows.GetLogicalDrives;   { LightCore.IO has a GetLogicalDrives of its own, which returns the drive names }
  for Letter:= 'Z' downto 'D' do
    if (Used AND (DWORD(1) shl (Ord(Letter) - Ord('A')))) = 0
    then EXIT(Letter + ':\');
  Result:= '';
end;


function isReparsePoint(CONST Path: string): Boolean;
VAR Attr: DWORD;
begin
  Attr:= GetFileAttributes(PChar(Path));
  Result:= (Attr <> INVALID_FILE_ATTRIBUTES) AND ((Attr AND FILE_ATTRIBUTE_REPARSE_POINT) <> 0);
end;


{ Makes the junction Link -> Target with "mklink /J", which needs no administrator rights. TRUE when cmd.exe ends with exit code 0. }
function makeJunction(CONST Link, Target: string): Boolean;
VAR
  StartInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  CmdLine: string;
  ProcExit: DWORD;
begin
  CmdLine:= 'cmd.exe /c mklink /J "'+ Link+ '" "'+ Target+ '"';
  UniqueString(CmdLine);   { CreateProcessW may write into the command-line buffer }
  FillChar(StartInfo, SizeOf(StartInfo), 0);
  StartInfo.cb:= SizeOf(StartInfo);
  if NOT CreateProcess(NIL, PChar(CmdLine), NIL, NIL, FALSE, CREATE_NO_WINDOW, NIL, NIL, StartInfo, ProcInfo)
  then EXIT(FALSE);
  TRY
    WaitForSingleObject(ProcInfo.hProcess, 10000);
    Result:= GetExitCodeProcess(ProcInfo.hProcess, ProcExit) AND (ProcExit = 0);
  FINALLY
    CloseHandle(ProcInfo.hThread);
    CloseHandle(ProcInfo.hProcess);
  END;
end;




{ # Setup }

procedure TTestDeleteGuard.Setup;
begin
  FRoot    := TPath.Combine(TPath.GetTempPath, 'LightCoreDeleteGuard_'+ TGUID.NewGuid.ToString);
  FTarget  := TPath.Combine(FRoot, 'Target');
  FKeep    := TPath.Combine(FTarget, 'keep.txt');
  FVictim  := TPath.Combine(FRoot, 'Victim');
  FLink    := TPath.Combine(FVictim, 'link');
  FRootLink:= TPath.Combine(FRoot, 'RootLink');

  Assert.IsTrue(StartsText(TPath.GetTempPath, FRoot), 'Setup: the test folder must lie in the Temp folder: '+ FRoot);
  Assert.IsFalse(DirectoryExists(FRoot), 'Setup: the test folder must be new: '+ FRoot);
  Assert.IsTrue(System.SysUtils.ForceDirectories(FRoot), 'Setup: cannot create '+ FRoot);
  Assert.AreEqual('', FolderDeleteRefusal(FRoot), 'Setup: the test folder itself must be deletable: '+ FRoot);
end;


{ Removes only the names this fixture made, deepest first, and never recursively:
  RemoveDir fails on a folder that is not empty and removes a junction as a link, so nothing outside FRoot can go. }
procedure TTestDeleteGuard.TearDown;

  function inside(CONST Path: string): Boolean;
  begin
    Result:= StartsText(IncludeTrailingPathDelimiter(FRoot), Path);
  end;

  procedure removeFile(CONST Path: string);
  begin
    if inside(Path) AND FileExists(Path)
    then System.SysUtils.DeleteFile(Path);
  end;

  procedure removeDir(CONST Path: string);
  begin
    if inside(Path) OR SameText(Path, FRoot)
    then System.SysUtils.RemoveDir(Path);
  end;

begin
  if FRoot = '' then EXIT;
  removeFile(TPath.Combine(FVictim, 'Sub\inner.txt'));
  removeFile(TPath.Combine(FVictim, 'plain.txt'));
  removeDir (FLink);
  removeDir (TPath.Combine(FVictim, 'Sub'));
  removeDir (FVictim);
  removeDir (FRootLink);
  removeFile(FKeep);
  removeDir (FTarget);
  removeDir (FRoot);
end;


{ FRoot\Target\keep.txt }
procedure TTestDeleteGuard.buildTarget;
begin
  Assert.IsTrue(System.SysUtils.ForceDirectories(FTarget), 'Precondition: cannot create '+ FTarget);
  TFile.WriteAllText(FKeep, 'must survive');
  Assert.IsTrue(FileExists(FKeep), 'Precondition: '+ FKeep+ ' must exist');
end;


{ FRoot\Victim with a file, a subfolder with a file, and the junction FRoot\Victim\link -> FRoot\Target }
procedure TTestDeleteGuard.buildVictim;
begin
  buildTarget;
  Assert.IsTrue(System.SysUtils.ForceDirectories(TPath.Combine(FVictim, 'Sub')), 'Precondition: cannot create '+ FVictim);
  TFile.WriteAllText(TPath.Combine(FVictim, 'plain.txt'), 'to be deleted');
  TFile.WriteAllText(TPath.Combine(FVictim, 'Sub\inner.txt'), 'to be deleted');

  Assert.IsTrue(makeJunction(FLink, FTarget), 'Precondition: mklink /J must create the junction '+ FLink);
  Assert.IsTrue(isReparsePoint(FLink), 'Precondition: '+ FLink+ ' must be a junction');
  Assert.IsTrue(FileExists(TPath.Combine(FLink, 'keep.txt')), 'Precondition: the target file must be visible through the junction');
end;


procedure TTestDeleteGuard.assertInsideRoot(CONST Path: string);
begin
  Assert.IsTrue(StartsText(IncludeTrailingPathDelimiter(FRoot), ExpandFileName(Path)), 'A test deletes only inside its own folder '+ FRoot+ ', not '+ Path);
end;


{ RemoveDirectory can return before the folder is gone (another process may hold a handle). Waits up to 3 seconds. }
procedure TTestDeleteGuard.waitGone(CONST Path: string);
VAR i: Integer;
begin
  for i:= 1 to 300 do
   begin
    if NOT DirectoryExists(Path) then EXIT;
    Sleep(10);
   end;
end;


{ Protected-shaped paths that do not exist: a free drive root, a top-level folder, a user profile.
  A broken guard gets FALSE from DirectoryExists on them, so it deletes nothing. }
function TTestDeleteGuard.missingProtectedPaths: TArray<string>;
VAR
  Profile, NoSuch: string;
begin
  Profile:= GetEnvironmentVariable('USERPROFILE');
  Assert.IsNotEmpty(Profile, 'Precondition: USERPROFILE must be set');
  Assert.IsNotEmpty(freeDriveRoot, 'Precondition: a free drive letter');
  NoSuch:= 'ZZ-NoSuch-'+ TGUID.NewGuid.ToString;

  Result:= [freeDriveRoot,                                                    { a drive root }
            systemDrive + '\'+ NoSuch,                                        { a folder directly under a drive root }
            TPath.Combine(ExtractFileDir(Profile), NoSuch),                   { a user profile }
            TPath.Combine(Profile, NoSuch)];                                  { a folder directly inside a user profile }
end;




{ # FolderDeleteRefusal, CheckFolderDeletable }

procedure TTestDeleteGuard.FolderDeleteRefusal_EmptyPath_Refused;
begin
  Assert.AreNotEqual('', FolderDeleteRefusal(''),    'An empty path must be refused');
  Assert.AreNotEqual('', FolderDeleteRefusal('   '), 'A path of spaces must be refused');
end;


procedure TTestDeleteGuard.FolderDeleteRefusal_RealProtectedFolders_Refused;
VAR
  Profile, P: string;
  Paths: TArray<string>;
begin
  Profile:= GetEnvironmentVariable('USERPROFILE');
  Assert.IsNotEmpty(Profile, 'Precondition: USERPROFILE must be set');

  Paths:= [systemDrive + '\',                                     { the drive root }
           '\\?\'+ systemDrive + '\',                             { the drive root, long-path prefix }
           '\\localhost\'+ systemDrive[1] + '$',                  { the drive root, as an administrative share }
           '\\server\share',                                      { a share root }
           '\\server\share\Folder',                               { a folder directly under a share root }
           systemDrive + '\Projects',                             { a folder directly under a drive root }
           GetEnvironmentVariable('SystemRoot'),                  { the Windows folder }
           GetEnvironmentVariable('ProgramFiles'),
           GetEnvironmentVariable('ProgramData'),
           ExtractFileDir(Profile),                               { the Users folder }
           Profile,                                               { the user profile }
           Profile + '\',                                         { the same, trailing backslash }
           TPath.Combine(Profile, 'Documents'),                   { a folder directly inside the profile }
           TPath.Combine(Profile, 'AppData')];
  for P in Paths do
    Assert.AreNotEqual('', FolderDeleteRefusal(P), 'FolderDeleteRefusal must refuse "'+ P+ '"');
end;


procedure TTestDeleteGuard.FolderDeleteRefusal_MissingProtectedShapes_Refused;
VAR P: string;
begin
  for P in missingProtectedPaths do
    Assert.AreNotEqual('', FolderDeleteRefusal(P), 'FolderDeleteRefusal must refuse a path that does not exist too: "'+ P+ '"');
end;


{ The guard must not refuse what the callers really delete: a folder in Temp, a cache in AppData, a folder inside the Windows folder }
procedure TTestDeleteGuard.FolderDeleteRefusal_OrdinaryFolders_Accepted;
VAR Profile: string;
begin
  Profile:= GetEnvironmentVariable('USERPROFILE');
  Assert.IsNotEmpty(Profile, 'Precondition: USERPROFILE must be set');

  Assert.AreEqual('', FolderDeleteRefusal(FRoot), 'The test folder in Temp must be accepted');
  Assert.AreEqual('', FolderDeleteRefusal(FVictim), 'A folder that does not exist yet, inside the test folder, must be accepted');
  Assert.AreEqual('', FolderDeleteRefusal(TPath.Combine(Profile, 'AppData\Roaming\ZZ-SomeApp\Thumbs')), 'A cache folder in AppData must be accepted');
  Assert.AreEqual('', FolderDeleteRefusal(TPath.Combine(GetEnvironmentVariable('SystemRoot'), 'Temp\ZZ-NoSuch')), 'A folder INSIDE the Windows folder is not refused');
end;


procedure TTestDeleteGuard.CheckFolderDeletable_DriveRoot_RaisesAndNamesPath;
VAR
  Root, Msg: string;
begin
  Root:= systemDrive + '\';
  Msg:= '';
  TRY
    CheckFolderDeletable(Root);
  EXCEPT
    on E: EProtectedFolder do Msg:= E.Message;
  END;
  Assert.AreNotEqual('', Msg, 'CheckFolderDeletable must raise EProtectedFolder on '+ Root);
  Assert.IsTrue(Pos(Root, Msg) > 0, 'The message must name the refused path. Message: '+ Msg);
end;


procedure TTestDeleteGuard.CheckFolderDeletable_OwnFolder_DeletesNothing;
begin
  buildTarget;
  CheckFolderDeletable(FTarget);   { must not raise }
  Assert.IsTrue(FileExists(FKeep), 'CheckFolderDeletable must delete nothing');
end;




{ # DeleteFolder, EmptyDirectory: refused paths }

procedure TTestDeleteGuard.DeleteFolder_ProtectedPaths_RaiseBeforeAnyFileCall;
VAR P: string;
begin
  Assert.WillRaise(procedure begin DeleteFolder(''); end, EProtectedFolder, 'DeleteFolder must refuse an empty path');

  for P in missingProtectedPaths do
   begin
    Assert.IsFalse(DirectoryExists(P), 'Precondition: '+ P+ ' must not exist');
    Assert.WillRaise(procedure begin DeleteFolder(P); end, EProtectedFolder, 'DeleteFolder must refuse "'+ P+ '" even though it does not exist');
   end;
end;


procedure TTestDeleteGuard.EmptyDirectory_ProtectedPaths_RaiseBeforeAnyFileCall;
VAR P: string;
begin
  Assert.WillRaise(procedure begin EmptyDirectory(''); end, EProtectedFolder, 'EmptyDirectory must refuse an empty path');

  for P in missingProtectedPaths do
   begin
    Assert.IsFalse(DirectoryExists(P), 'Precondition: '+ P+ ' must not exist');
    Assert.WillRaise(procedure begin EmptyDirectory(P); end, EProtectedFolder, 'EmptyDirectory must refuse "'+ P+ '" even though it does not exist');
   end;
end;




{ # DeleteFolder, EmptyDirectory: junctions }

procedure TTestDeleteGuard.DeleteFolder_JunctionInside_RemovesLinkKeepsTarget;
begin
  buildVictim;
  assertInsideRoot(FVictim);

  DeleteFolder(FVictim);
  waitGone(FVictim);

  Assert.IsFalse(DirectoryExists(FVictim), 'DeleteFolder must remove the folder');
  Assert.IsTrue(FileExists(FKeep), 'The file of the junction target must survive: '+ FKeep);
end;


procedure TTestDeleteGuard.EmptyDirectory_JunctionInside_RemovesLinkKeepsTarget;
begin
  buildVictim;
  assertInsideRoot(FVictim);

  EmptyDirectory(FVictim);

  Assert.IsTrue(DirectoryExists(FVictim), 'EmptyDirectory must leave the folder in place');
  Assert.IsTrue(TDirectory.IsEmpty(FVictim), 'EmptyDirectory must leave the folder empty');
  Assert.IsTrue(FileExists(FKeep), 'The file of the junction target must survive: '+ FKeep);
end;


procedure TTestDeleteGuard.DeleteFolder_FolderIsJunction_RemovesOnlyTheLink;
begin
  buildTarget;
  Assert.IsTrue(makeJunction(FRootLink, FTarget), 'Precondition: mklink /J must create the junction '+ FRootLink);
  Assert.IsTrue(isReparsePoint(FRootLink), 'Precondition: '+ FRootLink+ ' must be a junction');
  assertInsideRoot(FRootLink);

  DeleteFolder(FRootLink);
  waitGone(FRootLink);

  Assert.IsFalse(isReparsePoint(FRootLink), 'DeleteFolder must remove the junction');
  Assert.IsTrue(FileExists(FKeep), 'The file of the junction target must survive: '+ FKeep);
end;


procedure TTestDeleteGuard.EmptyDirectory_FolderIsJunction_Raises;
begin
  buildTarget;
  Assert.IsTrue(makeJunction(FRootLink, FTarget), 'Precondition: mklink /J must create the junction '+ FRootLink);
  Assert.IsTrue(isReparsePoint(FRootLink), 'Precondition: '+ FRootLink+ ' must be a junction');
  assertInsideRoot(FRootLink);

  Assert.WillRaise(procedure begin EmptyDirectory(FRootLink); end, EProtectedFolder, 'EmptyDirectory must refuse a folder that is itself a junction');

  Assert.IsTrue(FileExists(FKeep), 'The file of the junction target must survive: '+ FKeep);
  Assert.IsTrue(isReparsePoint(FRootLink), 'The refused junction must stay in place');
end;




{ # Registration: only with --delete-tests }

{ Read here, in the initialization section, before DUnitX parses its options, so the fixture can stay unregistered. }
function deleteTestsRequested: Boolean;
VAR i: Integer;
begin
  for i:= 1 to ParamCount do
    if SameText(ParamStr(i), '--'+ DeleteTestsSwitch)
    then EXIT(TRUE);
  Result:= FALSE;
end;


{ DUnitX refuses an option it does not know (exit code 100), so the switch is registered.
  The shape of the DUnitX option 'dontshowignored' (DUnitX.OptionsDefinition.pas): a switch with no value. }
procedure registerDeleteTestsSwitch;
VAR Def: IOptionDefinition;
begin
  Def:= TOptionsRegistry.RegisterOption<Boolean>(DeleteTestsSwitch, 'dlt', 'Register the fixture Test.LightCore.IO.DeleteGuard, whose tests delete files. Only Gabriel starts it',
          procedure(Value: Boolean)
          begin
          end);
  Def.HasValue:= FALSE;
end;


INITIALIZATION
  registerDeleteTestsSwitch;
  if deleteTestsRequested
  then TDUnitX.RegisterTestFixture(TTestDeleteGuard);

end.
