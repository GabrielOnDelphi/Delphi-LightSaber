UNIT cmIO;

{=============================================================================================================
   Gabriel Moraru
   2024.06
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Extension for ccIO. 
   Shows error messages (dialog boxes) when the I/O peeration failed.
==================================================================================================}

INTERFACE
{$WARN UNIT_PLATFORM ON}   { OFF: Silence the 'W1005 Unit Vcl.FileCtrl is specific to a platform' warning }

USES
  System.StrUtils, System.IOUtils, System.SysUtils;


 function  DirectoryExistMsg    (CONST Path: string): Boolean;
 function  FileExistsMsg        (CONST FileName: string): Boolean;
 function  ForceDirectoriesMsg  (CONST FullPath: string): Boolean;                                 { RETURNS:  -1 = Error creating the directory.   0 = Directory already exists.  +1 = Directory created succesfully }
 procedure MoveFolderMsg        (CONST FromFolder, ToFolder: String; SilentOverwrite: Boolean);
 function  DeleteFileWithMsg    (CONST FileName: string): Boolean;
 {$IFDEF MSWINDOWS}
 function  GetPosAfterExtendedPrefix(CONST Path: string): Integer;
 function  PathHasValidColon    (const Path: string): Boolean;
 {$ENDIF}


IMPLEMENTATION

USES
  ccCore, cbDialogs, ccIO;



{$IFDEF MSWINDOWS}
function GetPosAfterExtendedPrefix(const Path: string): Integer;
CONST
  FCExtendedPrefix: string = '\\?\';        // DO NOT LOCALIZE
  FCExtendedUNCPrefix: string = '\\?\UNC\'; // DO NOT LOCALIZE
VAR
  Prefix: TPathPrefixType;
begin
  Prefix := TPath.GetExtendedPrefix(Path);
  case Prefix of
    TPathPrefixType.pptNoPrefix: Result := 1;
    TPathPrefixType.pptExtended: Result := Length(FCExtendedPrefix) + 1;
    TPathPrefixType.pptExtendedUNC: Result := Length(FCExtendedUNCPrefix) + 1;
  else
    Result := 1;
  end;
end;
{$ENDIF MSWINDOWS}


{$IFDEF MSWINDOWS}
{ Copied from IOUtils.TPath.HasPathValidColon }
function PathHasValidColon(const Path: string): Boolean;
VAR
   StartIdx: Integer;
begin
  Result := True;
  if Trim(Path) <> '' then // DO NOT LOCALIZE
  begin
    StartIdx := GetPosAfterExtendedPrefix(Path);
    if TPath.IsDriveRooted(Path)
    then Inc(StartIdx, 2);

    Result := PosEx(TPath.VolumeSeparatorChar, Path, StartIdx) = 0;
  end;
end;
{$ENDIF MSWINDOWS}




function DirectoryExistMsg(CONST Path: string): Boolean;                                           { Directory Exist }
begin
  Result:= DirectoryExists(Path);
  if NOT Result then
    if Path= ''
    then MesajError('DirectoryExistMsg: No folder specified!')
    else
      if Pos(':', Path)< 1                                                                             { check if the user has given a full path as c:\xxx }
      then MesajError('A relative path was provided instead of a full path!'+ CRLFw+ Path)
      else MesajError('Folder does not exist:'+ CRLFw+ Path);
end;


{ Shows an error message if the folder cannot be created. }
function ForceDirectoriesMsg(CONST FullPath: string): Boolean;
begin
  Result:= ccIO.ForceDirectories(FullPath) >= 0;
  if NOT Result
  then MesajError('Cannot create folder: '+ FullPath+ CRLFw+ 'Probably you are trying to write to a folder to which you don''t have write permissions, or, the folder you want to create is invalid.');
end;



 { File Exists }
function FileExistsMsg(CONST FileName: string): Boolean;
begin
 Result:= FileExists(FileName);
 if NOT Result then
 if FileName= ''
 then MesajError('No file specified!')
 else MesajError('File does not exist!'+ CRLFw+ FileName);
end;



function DeleteFileWithMsg(const FileName: string): Boolean;
begin
 Result:= DeleteFile(FileName);
 if NOT Result
 then MesajError('Cannot delete file '+CRLFw+ FileName);
end;



{ Example:  MoveFolderMsg('c:\Documents', 'C:\Backups') }
procedure MoveFolderMsg(CONST FromFolder, ToFolder: String; SilentOverwrite: Boolean);      { Also see: http://www.swissdelphicenter.ch/en/showcode.php?id=152 }
begin
 if DirectoryExists(ToFolder) then
   if SilentOverwrite
   then
     begin
       CopyFolder(FromFolder, ToFolder, True);
       Deletefolder(ToFolder);  { This is slow. Do a direct file move for each file. }
     end
   else
     { Move raises an exception if the destination folder already exists, so we have to delete the Destination folder first. But for this we need to ask the user. }
     if MesajYesNo('Cannot move '+ FromFolder +'. Destination folder already exists:'+ ToFolder+ CRLFw+
                   'Press Yes to delete Destination folder. Press No to cancel the opperation.') then
       begin
         DeleteFolder(ToFolder);
         TDirectory.Move(FromFolder, ToFolder);
       end;
end;




end.

