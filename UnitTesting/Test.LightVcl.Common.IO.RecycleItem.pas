unit Test.LightVcl.Common.IO.RecycleItem;

{=============================================================================================================
   2026.11.15
   Unit tests for RecycleItem (FrameVCL\LightVcl.Common.IO.pas): what it sends to the Recycle Bin, what it deletes for good
   (a path with no Recycle Bin), and what it refuses.
   The checks it runs first (RecycleItemRefusal, DriveHasRecycleBin) are tested without any delete in
   Test.LightCore.IO.RecycleCheck.pas.

   THESE TESTS DELETE FILES: they send throwaway files to the Recycle Bin. Claude never runs them:
   rule in C:\Projects\.claude\rules\delphi-code.md, section "Unit Testing".
   So the fixture is registered only when the runner gets the switch --delete-tests:
     C:\Projects\LightSaber\UnitTesting\Tests_LightVcl.Common.exe --delete-tests --run:Test.LightVcl.Common.IO.RecycleItem
   Without the switch, a plain run and --run:Test.LightVcl.Common.IO never see this fixture.

   How the tests stay safe even when RecycleItem is broken:
   - RecycleItem gets ONLY paths this fixture made in this run: files and folders inside FRoot (a new folder under
     the Temp folder), and FTopFolder (a new, empty folder directly under the system drive root).
     No real protected folder ever reaches RecycleItem; those are tested in Test.LightCore.IO.RecycleCheck.pas.
   - A path is checked against FRoot or FTopFolder before RecycleItem gets it.
   - TearDown removes only the names it made, never recursively.
   - The refused paths are throwaway files too: a broken refusal destroys a throwaway file, and the test turns red.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  DUnitX.CommandLine.Options,
  Winapi.Windows,
  System.SysUtils,
  System.StrUtils,
  System.IOUtils;

type
  [TestFixture]
  TTestRecycleItem = class
  private
    FRoot: string;        { The throwaway folder of this run, under the Temp folder }
    FTarget: string;      { FRoot\Target - the folder the junction points to }
    FKeep: string;        { FRoot\Target\keep.txt - must survive }
    FLink: string;        { FRoot\Link - a junction to FTarget }
    FTopFolder: string;   { <system drive>\ZZ-LightSaber-RecycleItem-<guid> - a top-level folder, empty, made by the test }
    FOldDir: string;      { The current folder before a test changed it }
    FStarted: TDateTime;  { Local time when Setup ran. foundInRecycleBin reads only the $I files written after it }
    function  makeFile(CONST Name: string): string;
    procedure assertOwnPath(CONST Path: string);
    function  foundInRecycleBin(CONST OriginalPath: string): Boolean;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { Throwaway items that must reach the Recycle Bin }
    [Test]
    procedure RecycleItem_File_ReachesRecycleBin;
    [Test]
    procedure RecycleItem_TwoFilesInList_BothReachRecycleBin;
    [Test]
    procedure RecycleItem_RelativePath_ReachesRecycleBin;
    [Test]
    procedure RecycleItem_Folder_ReachesRecycleBin;

    { A throwaway item on a path with no Recycle Bin: deleted for good }
    [Test]
    procedure RecycleItem_UncPath_DeletedForGood;

    { Throwaway items that must be refused and stay where they are }
    [Test]
    procedure RecycleItem_TopLevelFolder_RefusedFolderStays;
    [Test]
    procedure RecycleItem_FolderIsJunction_RefusedTargetStays;
    [Test]
    procedure RecycleItem_OneRefusedInList_NothingDeleted;
  end;


implementation

uses
  LightCore.IO,
  LightVcl.Common.IO;


CONST
  DeleteTestsSwitch = 'delete-tests';


{ The system drive as 'C:'. }
function systemDrive: string;
begin
  Result:= GetEnvironmentVariable('SystemDrive');
  if Result = ''
  then Result:= 'C:';
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

procedure TTestRecycleItem.Setup;
begin
  FRoot     := TPath.Combine(TPath.GetTempPath, 'LightVclRecycleItem_'+ TGUID.NewGuid.ToString);
  FTarget   := TPath.Combine(FRoot, 'Target');
  FKeep     := TPath.Combine(FTarget, 'keep.txt');
  FLink     := TPath.Combine(FRoot, 'Link');
  FTopFolder:= systemDrive + '\ZZ-LightSaber-RecycleItem-'+ TGUID.NewGuid.ToString;
  FOldDir   := GetCurrentDir;
  FStarted  := Now;

  Assert.IsTrue(StartsText(TPath.GetTempPath, FRoot), 'Setup: the test folder must lie in the Temp folder: '+ FRoot);
  Assert.IsFalse(System.SysUtils.DirectoryExists(FRoot), 'Setup: the test folder must be new: '+ FRoot);
  Assert.IsTrue(System.SysUtils.ForceDirectories(FRoot), 'Setup: cannot create '+ FRoot);
  Assert.IsTrue(DriveHasRecycleBin(FRoot), 'Setup: the Temp folder must be on a drive with a Recycle Bin, or the tests that recycle cannot pass: '+ FRoot);
end;


{ Removes only the names this fixture made, never recursively: DeleteFile for a file, RemoveDir for a folder.
  RemoveDir fails on a folder that is not empty and removes a junction as a link, so nothing outside FRoot can go. }
procedure TTestRecycleItem.TearDown;

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

VAR
  Name: string;
  Names: TArray<string>;
begin
  SetCurrentDir(FOldDir);
  if FRoot = '' then EXIT;

  Names:= ['a.txt', 'b.txt', 'c.txt', 'relative.txt', 'unc.txt', 'Folder\inner.txt'];
  for Name in Names do
    removeFile(TPath.Combine(FRoot, Name));
  removeDir (TPath.Combine(FRoot, 'Folder'));
  removeDir (FLink);
  removeFile(FKeep);
  removeDir (FTarget);
  removeDir (FRoot);

  { The top-level folder: only the exact name made in Setup, and only when it is empty (RemoveDir) }
  if StartsText(systemDrive + '\ZZ-LightSaber-RecycleItem-', FTopFolder)
  then System.SysUtils.RemoveDir(FTopFolder);
end;


{ FRoot\Name with some text. Returns the full path }
function TTestRecycleItem.makeFile(CONST Name: string): string;
begin
  Result:= TPath.Combine(FRoot, Name);
  System.SysUtils.ForceDirectories(ExtractFileDir(Result));
  TFile.WriteAllText(Result, 'throwaway file of '+ FRoot);
  Assert.IsTrue(FileExists(Result), 'Precondition: cannot create '+ Result);
end;


procedure TTestRecycleItem.assertOwnPath(CONST Path: string);
begin
  Assert.IsTrue(StartsText(IncludeTrailingPathDelimiter(FRoot), ExpandFileName(Path))
             OR SameText(ExpandFileName(Path), FTopFolder),
                'A test gives RecycleItem only its own paths ('+ FRoot+ ', '+ FTopFolder+ '), not '+ Path);
end;


{ TRUE when an index file $I* of the Recycle Bin of the drive of OriginalPath names OriginalPath.
  Each recycled item gets a $I file under <drive>\$Recycle.Bin\<user SID>\, which holds the original path as UTF-16
  from byte 28 (measured: C:\Projects\Projects System\RecycleBin.exe\HandOver.md).
  The folder of another user cannot be listed: FindFirst fails there, and TDirectory.WalkThroughDirectory
  (c:\Delphi\Delphi 13\source\rtl\common\System.IOUtils.pas) then returns no file, with no exception.
  Only the $I files written since one minute before Setup are read: the Recycle Bin of this PC held 25,994 of them,
  and reading them all took 82.7 s per call. The last-write time of a $I file is its deletion time (measured: within 2 ms
  of the deletion time stored inside the file). }
function TTestRecycleItem.foundInRecycleBin(CONST OriginalPath: string): Boolean;
VAR
  BinRoot, SidFolder, IndexFile, Stored: string;
  Bytes: TBytes;
  Since: TDateTime;
begin
  BinRoot:= ExtractFileDrive(ExpandFileName(OriginalPath)) + '\$Recycle.Bin';
  Assert.IsTrue(System.SysUtils.DirectoryExists(BinRoot), 'Precondition: the Recycle Bin folder must exist: '+ BinRoot);
  Since:= FStarted - 1 / MinsPerDay;
  for SidFolder in TDirectory.GetDirectories(BinRoot) do
    for IndexFile in TDirectory.GetFiles(SidFolder, '$I*',
                       function(CONST Path: string; CONST SearchRec: TSearchRec): Boolean
                       begin
                         Result:= SearchRec.TimeStamp >= Since;
                       end) do
     begin
      Bytes:= TFile.ReadAllBytes(IndexFile);
      if Length(Bytes) <= 28 then Continue;
      Stored:= TEncoding.Unicode.GetString(Bytes, 28, Length(Bytes) - 28);
      if Pos(#0, Stored) > 0
      then Stored:= Copy(Stored, 1, Pos(#0, Stored) - 1);
      if SameText(Stored, ExpandFileName(OriginalPath))
      then EXIT(TRUE);
     end;
  Result:= FALSE;
end;




{ # Items that must reach the Recycle Bin }

procedure TTestRecycleItem.RecycleItem_File_ReachesRecycleBin;
VAR F: string;
begin
  F:= makeFile('a.txt');
  assertOwnPath(F);

  Assert.IsTrue(RecycleItem(F, TRUE, FALSE, TRUE), 'RecycleItem must succeed on a throwaway file');
  Assert.IsFalse(FileExists(F), 'The file must be gone from its folder');
  Assert.IsTrue(foundInRecycleBin(F), 'The file must be in the Recycle Bin: '+ F);
end;


{ The shape BioniX passes: each path ended with #0 }
procedure TTestRecycleItem.RecycleItem_TwoFilesInList_BothReachRecycleBin;
VAR A, B: string;
begin
  A:= makeFile('a.txt');
  B:= makeFile('b.txt');
  assertOwnPath(A);
  assertOwnPath(B);

  Assert.IsTrue(RecycleItem(A + #0 + B + #0, TRUE, FALSE, TRUE), 'RecycleItem must succeed on a list of two throwaway files');
  Assert.IsFalse(FileExists(A), 'The first file must be gone');
  Assert.IsFalse(FileExists(B), 'The second file must be gone');
  Assert.IsTrue(foundInRecycleBin(A), 'The first file must be in the Recycle Bin: '+ A);
  Assert.IsTrue(foundInRecycleBin(B), 'The second file must be in the Recycle Bin: '+ B);
end;


{ Microsoft: with a relative path, FO_DELETE deletes for good even with FOF_ALLOWUNDO. RecycleItem expands it first }
procedure TTestRecycleItem.RecycleItem_RelativePath_ReachesRecycleBin;
VAR F: string;
begin
  F:= makeFile('relative.txt');
  assertOwnPath(F);
  Assert.IsTrue(SetCurrentDir(FRoot), 'Precondition: cannot make '+ FRoot+ ' the current folder');

  Assert.IsTrue(RecycleItem('relative.txt', TRUE, FALSE, TRUE), 'RecycleItem must succeed on a relative path');
  Assert.IsFalse(FileExists(F), 'The file must be gone');
  Assert.IsTrue(foundInRecycleBin(F), 'The file must be in the Recycle Bin, not deleted for good: '+ F);
end;


{ A folder with a trailing backslash, the shape the DNA Baser dereplicator passes }
procedure TTestRecycleItem.RecycleItem_Folder_ReachesRecycleBin;
VAR Folder: string;
begin
  makeFile('Folder\inner.txt');
  Folder:= TPath.Combine(FRoot, 'Folder');
  assertOwnPath(Folder);

  Assert.IsTrue(RecycleItem(Folder + '\', TRUE, FALSE, TRUE), 'RecycleItem must succeed on a throwaway folder');
  Assert.IsFalse(System.SysUtils.DirectoryExists(Folder), 'The folder must be gone');
  Assert.IsTrue(foundInRecycleBin(Folder), 'The folder must be in the Recycle Bin: '+ Folder);
end;




{ # Items that must be refused }

{ The same throwaway file, named through the administrative share of its own drive. A UNC path has no Recycle Bin,
  and the caller asked to delete, so RecycleItem deletes the file for good }
procedure TTestRecycleItem.RecycleItem_UncPath_DeletedForGood;
VAR F, Unc: string;
begin
  F:= makeFile('unc.txt');
  assertOwnPath(F);
  Unc:= '\\localhost\'+ F[1] + '$'+ Copy(F, 3, MaxInt);   { C:\x\unc.txt -> \\localhost\C$\x\unc.txt }
  if NOT FileExists(Unc) then
   begin
    Assert.IsTrue(FileExists(F), 'Skip path: the file must still exist');
    Assert.Pass('Skipped: the administrative share is not reachable on this PC: '+ Unc);
   end;
  Assert.IsFalse(DriveHasRecycleBin(Unc), 'Precondition: a UNC path has no Recycle Bin');

  Assert.IsTrue(RecycleItem(Unc, TRUE, FALSE, TRUE), 'RecycleItem must delete a UNC path although it has no Recycle Bin');
  Assert.IsFalse(FileExists(F), 'The file must be gone: '+ F);
  Assert.IsFalse(foundInRecycleBin(F), 'A file on a UNC path cannot reach the Recycle Bin: '+ F);
end;


{ FolderDeleteRefusal refuses a folder directly under a drive root. The folder is empty and made by this test }
procedure TTestRecycleItem.RecycleItem_TopLevelFolder_RefusedFolderStays;
begin
  if NOT System.SysUtils.CreateDir(FTopFolder) then
   begin
    Assert.IsFalse(System.SysUtils.DirectoryExists(FTopFolder), 'Skip path: the folder must not exist');
    Assert.Pass('Skipped: this user cannot create a folder directly under '+ systemDrive + '\');
   end;
  assertOwnPath(FTopFolder);

  Assert.IsFalse(RecycleItem(FTopFolder, TRUE, FALSE, TRUE), 'RecycleItem must refuse a folder directly under a drive root');
  Assert.IsTrue(System.SysUtils.DirectoryExists(FTopFolder), 'The refused folder must still exist: '+ FTopFolder);
end;


procedure TTestRecycleItem.RecycleItem_FolderIsJunction_RefusedTargetStays;
begin
  Assert.IsTrue(System.SysUtils.ForceDirectories(FTarget), 'Precondition: cannot create '+ FTarget);
  TFile.WriteAllText(FKeep, 'must survive');
  Assert.IsTrue(makeJunction(FLink, FTarget), 'Precondition: mklink /J must create the junction '+ FLink);
  Assert.IsTrue(isReparsePoint(FLink), 'Precondition: '+ FLink+ ' must be a junction');
  assertOwnPath(FLink);

  Assert.IsFalse(RecycleItem(FLink, TRUE, FALSE, TRUE), 'RecycleItem must refuse a folder that is itself a junction');
  Assert.IsTrue(isReparsePoint(FLink), 'The refused junction must stay in place');
  Assert.IsTrue(FileExists(FKeep), 'The file of the junction target must survive: '+ FKeep);
end;


{ One refused path refuses the whole list: the throwaway file before it must stay too. The refused path is a junction }
procedure TTestRecycleItem.RecycleItem_OneRefusedInList_NothingDeleted;
VAR F: string;
begin
  F:= makeFile('c.txt');
  assertOwnPath(F);
  Assert.IsTrue(System.SysUtils.ForceDirectories(FTarget), 'Precondition: cannot create '+ FTarget);
  TFile.WriteAllText(FKeep, 'must survive');
  Assert.IsTrue(makeJunction(FLink, FTarget), 'Precondition: mklink /J must create the junction '+ FLink);
  Assert.IsTrue(isReparsePoint(FLink), 'Precondition: '+ FLink+ ' must be a junction');
  assertOwnPath(FLink);

  Assert.IsFalse(RecycleItem(F + #0 + FLink + #0, TRUE, FALSE, TRUE), 'RecycleItem must refuse the list');
  Assert.IsTrue(FileExists(F), 'The accepted file of a refused list must still exist: '+ F);
  Assert.IsTrue(isReparsePoint(FLink), 'The refused junction must stay in place');
  Assert.IsTrue(FileExists(FKeep), 'The file of the junction target must survive: '+ FKeep);
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
  The shape of the DUnitX option 'dontshowignored' (DUnitX.OptionsDefinition.pas): a switch with no value.
  WARNING: UnitTesting\Test.LightCore.IO.DeleteGuard.pas registers the same names, 'delete-tests' and 'dlt'.
  TOptionsRegistry.RegisterOption raises "Options : delete-tests already registered" on a name it already holds
  (c:\Delphi\Delphi 13\source\DunitX\DUnitX.CommandLine.Options.pas, RegisterOption), and here it raises inside
  INITIALIZATION, so an EXE that links both units dies at start. Today Tests_LightCore.dpr links only DeleteGuard
  and Tests_LightVcl.Common.dpr only this unit. A project that links both must register the switch in one place only. }
procedure registerDeleteTestsSwitch;
VAR Def: IOptionDefinition;
begin
  Def:= TOptionsRegistry.RegisterOption<Boolean>(DeleteTestsSwitch, 'dlt', 'Register the fixture Test.LightVcl.Common.IO.RecycleItem, whose tests send files to the Recycle Bin. Only Gabriel starts it',
          procedure(Value: Boolean)
          begin
          end);
  Def.HasValue:= FALSE;
end;


INITIALIZATION
  registerDeleteTestsSwitch;
  if deleteTestsRequested
  then TDUnitX.RegisterTestFixture(TTestRecycleItem);

end.
