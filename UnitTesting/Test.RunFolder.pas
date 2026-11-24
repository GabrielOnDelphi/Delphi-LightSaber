UNIT Test.RunFolder;

{=============================================================================================================
   A safe folder for one fixture run, and the only way a test deletes it.

   Rule (C:\Projects\.claude\rules\delphi-code.md, section "Unit Testing"): a TearDown deletes only inside
   a folder the test created in that run, and checks before each delete that the full path starts with
   that folder. A file or a folder with a fixed name straight in %TEMP% fails the first half: it may belong
   to another program or to an older run.

   Two roots, same folder shape <Root>\<Prefix>_<GUID>\:
     NewRunFolder        - root is the system temp folder. For a test whose code under test deletes nothing.
     NewDeleteTestFolder - root is C:\test delete\. For a test of a routine that deletes files or folders
                           (DeleteFolder, EmptyDirectory, RecycleItem, a cache purge and the like).
                           Every path the test hands to such a routine must lie inside this folder.
   Call the New* routine in Setup and the matching Delete* routine in TearDown.
   DeleteRunFolder / DeleteDeleteTestFolder delete the folder with all its content, and refuse (assertion
   failure) any folder that does not have exactly that shape under their own root.
   DeleteFileInFolder deletes one file, and refuses (assertion failure) a path outside the given folder.

   The same code is in C:\Projects\BioniX\SourceCode\BioniX VCL\UnitTesting\TestFolderGuard.pas.
   This unit has a different name, because Tests_BioniX.dproj links some LightSaber test units too.
=============================================================================================================}

INTERFACE

function  NewRunFolder   (CONST Prefix: string): string;
function  IsRunFolder    (CONST Folder, Prefix: string): Boolean;
procedure DeleteRunFolder(CONST Folder, Prefix: string);

function  NewDeleteTestFolder   (CONST Prefix: string): string;
function  IsDeleteTestFolder    (CONST Folder, Prefix: string): Boolean;
procedure DeleteDeleteTestFolder(CONST Folder, Prefix: string);

function  IsInsideFolder    (CONST Path, Folder: string): Boolean;
procedure DeleteFileInFolder(CONST FileName, Folder: string);


CONST
  DeleteTestRoot = 'C:\test delete\';   { The only place where a test lets the code under test delete files }


IMPLEMENTATION

USES
  {$IFDEF POSIX}
  Posix.Unistd,   { Lets the compiler inline System.SysUtils.DeleteFile on Linux64 (hint H2443 without it) }
  {$ENDIF}
  System.SysUtils, System.IOUtils, System.StrUtils,
  DUnitX.TestFramework;


function tempRoot: string;
begin
  Result:= IncludeTrailingPathDelimiter(TPath.GetTempPath);
  { An empty TEMP would give a root such as 'C:\'. Refuse it: the run folder must sit below a real temp folder. }
  if Length(ExcludeTrailingPathDelimiter(Result)) < 4
  then Assert.Fail('Test.RunFolder: the temp folder looks like a drive root: "'+ Result +'"');
end;


{ Creates <Root><Prefix>_<GUID>\ and returns it, with the trailing path delimiter. Root ends with PathDelim.
  PathDelim, not '\': the ProteusServer Lib tests also target Linux64. }
function newRunFolderIn(CONST Root, Prefix: string): string;
VAR GUID: TGUID;
begin
  Assert.IsTrue(Prefix > '', 'Test.RunFolder: empty prefix');
  CreateGUID(GUID);
  Result:= Root + Prefix + '_' + GUIDToString(GUID) + PathDelim;
  ForceDirectories(Result);
  Assert.IsTrue(DirectoryExists(Result), 'Test.RunFolder: cannot create '+ Result);
end;


{ TRUE when Folder is <Root><Prefix>_<GUID in braces>\ and nothing else: no subfolder, no '..' }
function isRunFolderIn(CONST Root, Folder, Prefix: string): Boolean;
VAR
  Head, Rest: string;
begin
  if Prefix = '' then EXIT(FALSE);
  Head:= Root + Prefix + '_';
  if NOT StartsText(Head, Folder) then EXIT(FALSE);
  if Pos('..', Folder) > 0 then EXIT(FALSE);

  Rest:= Copy(Folder, Length(Head) + 1, MaxInt);       // the GUID, then a path delimiter
  if (Rest = '') OR (Rest[Length(Rest)] <> PathDelim) then EXIT(FALSE);
  Rest:= Copy(Rest, 1, Length(Rest) - 1);
  if (Pos('\', Rest) > 0) OR (Pos('/', Rest) > 0) then EXIT(FALSE);
  if Length(Rest) <> 38 then EXIT(FALSE);              // GUIDToString gives 38 characters, braces included

  TRY
    Result:= NOT IsEqualGUID(StringToGUID(Rest), TGUID.Empty);   // StringToGUID raises EConvertError for anything that is not a GUID
  EXCEPT
    on EConvertError do Result:= FALSE;
  END;
end;


procedure deleteRunFolderIn(CONST Root, Folder, Prefix: string);
begin
  if Folder = '' then EXIT;                             { Setup failed before it made the folder }
  Assert.IsTrue(isRunFolderIn(Root, Folder, Prefix), 'Test.RunFolder refuses to delete a folder this run did not create: '+ Folder);
  if DirectoryExists(Folder)
  then TDirectory.Delete(Folder, TRUE);                 { Clears the read-only attribute of each file before it deletes it }
end;




{-------------------------------------------------------------------------------------------------------------
   Under the system temp folder
-------------------------------------------------------------------------------------------------------------}

function NewRunFolder(CONST Prefix: string): string;
begin
  Result:= newRunFolderIn(tempRoot, Prefix);
end;


function IsRunFolder(CONST Folder, Prefix: string): Boolean;
begin
  Result:= isRunFolderIn(tempRoot, Folder, Prefix);
end;


procedure DeleteRunFolder(CONST Folder, Prefix: string);
begin
  deleteRunFolderIn(tempRoot, Folder, Prefix);
end;




{-------------------------------------------------------------------------------------------------------------
   Under C:\test delete\
-------------------------------------------------------------------------------------------------------------}

function NewDeleteTestFolder(CONST Prefix: string): string;
begin
  Result:= newRunFolderIn(DeleteTestRoot, Prefix);
end;


function IsDeleteTestFolder(CONST Folder, Prefix: string): Boolean;
begin
  Result:= isRunFolderIn(DeleteTestRoot, Folder, Prefix);
end;


procedure DeleteDeleteTestFolder(CONST Folder, Prefix: string);
begin
  deleteRunFolderIn(DeleteTestRoot, Folder, Prefix);
end;




{ TRUE when Path lies below Folder (Folder ends with PathDelim) }
function IsInsideFolder(CONST Path, Folder: string): Boolean;
begin
  Result:= (Folder > '')
       AND (Folder[Length(Folder)] = PathDelim)
       AND StartsText(Folder, Path)
       AND (Length(Path) > Length(Folder))
       AND (Pos('..', Path) = 0);
end;


procedure DeleteFileInFolder(CONST FileName, Folder: string);
begin
  Assert.IsTrue(IsInsideFolder(FileName, Folder), 'Test.RunFolder refuses to delete a file outside the run folder: '+ FileName);
  if FileExists(FileName)
  then System.SysUtils.DeleteFile(FileName);
end;


end.
