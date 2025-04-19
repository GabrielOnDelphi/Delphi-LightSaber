UNIT cmIO;

{=============================================================================================================
   www.GabrielMoraru.com
   2024.06
--------------------------------------------------------------------------------------------------------------
   Extension for ccIO.
   VCL only!
   Shows error messages (dialog boxes) when the I/O peeration failed.
==================================================================================================}

INTERFACE
{ $WARN UNIT_PLATFORM OFF}   { OFF: Silence the 'W1005 Unit Vcl.FileCtrl is specific to a platform' warning }

USES
  Winapi.Windows, Winapi.ShellAPI, Winapi.ShlObj,
  System.StrUtils, System.IOUtils, System.SysUtils, System.Win.Registry, System.Classes,
  Vcl.Consts, Vcl.Controls, Vcl.Dialogs, Vcl.Forms, Vcl.FileCtrl;


{--------------------------------------------------------------------------------------------------
   OPPERATIONS WITH MESSAGE
--------------------------------------------------------------------------------------------------}
 function  DirectoryExistMsg    (CONST Path: string): Boolean;
 function  FileExistsMsg        (CONST FileName: string): Boolean;
 function  ForceDirectoriesMsg  (CONST FullPath: string): Boolean;                                 { RETURNS:  -1 = Error creating the directory.   0 = Directory already exists.  +1 = Directory created succesfully }

 procedure MoveFolderMsg        (CONST FromFolder, ToFolder: String; SilentOverwrite: Boolean);
 function  DeleteFileWithMsg    (CONST FileName: string): Boolean;


{--------------------------------------------------------------------------------------------------
   SPECIAL FOLDERS
--------------------------------------------------------------------------------------------------}
 function  GetProgramFilesDir    : string;
 function  GetDesktopFolder      : string;
 function  GetStartMenuFolder    : string;
 function  GetMyDocumentsAPI     : string; deprecated 'Use GetMyDocuments instead';
 function  GetMyPicturesAPI      : string; deprecated 'Use GetMyPictures  instead';
 function  GetWinSysDir          : string;
 function  GetWinDir             : string; { Returns Windows folder }
 function  GetTaskManager        : string;

 function  GetSpecialFolder (CONST OS_SpecialFolder: string): string;                 overload;          { SHELL FOLDERS.  Retrieving the entire list of default shell folders from registry }
 function  GetSpecialFolder (CSIDL: Integer; ForceFolder: Boolean = FALSE): string;   overload;          { uses SHFolder }
 function  GetSpecialFolders: TStringList;                                                               { Get a list of ALL special folders. }

 function  FolderIsSpecial  (CONST Path: string): Boolean;                                               { Returns True if the parameter is a special folder such us 'c:\My Documents' }


{--------------------------------------------------------------------------------------------------
   UNC
--------------------------------------------------------------------------------------------------}
 function  GetPosAfterExtendedPrefix(CONST Path: string): Integer;

{--------------------------------------------------------------------------------------------------
   OPEN/SAVE dialogs
--------------------------------------------------------------------------------------------------}
 function SelectAFolder    (VAR Folder: string; CONST Title: string = ''; CONST Options: TFileDialogOptions= [fdoPickFolders, fdoForceFileSystem, fdoPathMustExist, fdoDefaultNoMiniMode]): Boolean; overload;

 function PromptToSaveFile (VAR FileName: string; CONST Filter: string = ''; CONST DefaultExt: string= ''; CONST Title: string= ''): Boolean;
 function PromptToLoadFile (VAR FileName: string; CONST Filter: string = '';                               CONST Title: string= ''): Boolean;

 function PromptForFileName(VAR FileName: string; SaveDialog: Boolean; CONST Filter: string = ''; CONST DefaultExt: string= ''; CONST Title: string= ''; CONST InitialDir: string = ''): Boolean;

 function GetSaveDialog    (CONST FileName, Filter, DefaultExt: string; CONST Caption: string= ''): TSaveDialog;
 function GetOpenDialog    (CONST FileName, Filter, DefaultExt: string; CONST Caption: string= ''): TOpenDialog;


{--------------------------------------------------------------------------------------------------
   API OPERATIONS
--------------------------------------------------------------------------------------------------}
 function RecycleItem           (CONST ItemName: string; CONST DeleteToRecycle: Boolean= TRUE; CONST ShowConfirm: Boolean= TRUE; CONST TotalSilence: Boolean= FALSE): Boolean;
 function FileOperation         (CONST Source, Dest: string; Op, Flags: Integer): Boolean;                     { Performs: Copy, Move, Delete, Rename on files + folders via WinAPI}

 function FileAge               (CONST FileName: string): TDateTime;
 function FileTimeToDateTimeStr (FTime: TFileTime; CONST DFormat, TFormat: string): string;

 procedure SetCompressionAtr    (CONST FileName: string; const CompressionFormat: byte= 1);


{--------------------------------------------------------------------------------------------------
   FILE SIZE
--------------------------------------------------------------------------------------------------}
 function  GetFileSizeEx       (hFile: THandle; VAR FileSize: Int64): BOOL; stdcall; external kernel32;


{--------------------------------------------------------------------------------------------------
   FILE ACCESS
--------------------------------------------------------------------------------------------------}
 function  FileIsLockedR        (CONST FileName: string): Boolean;
 function  FileIsLockedRW       (CONST FileName: string): Boolean;                    { Returns true if the file cannot be open for reading and writing } { old name: FileInUse }
 function  CanCreateFile        (CONST FileName: string): Boolean;

 function  CanWriteToFolder     (CONST Folder: string; const FileName: String = 'TempFile.Delete.Me'): Boolean;    { Returns true if it can write that file to disk. ATTENTION it will overwrite the file if it already exists ! }
 function  CanWriteToFolderMsg  (CONST Folder: string): Boolean;


{--------------------------------------------------------------------------------------------------
   DRIVES
--------------------------------------------------------------------------------------------------}
 function  GetDriveType       (CONST Path: string): Integer;
 function  GetDriveTypeS      (CONST Path: string): string;                           { Returns drive type asstring }
 function  GetVolumeLabel     (CONST Drive: Char): string;                            { Returns volume label of a disk }

 { Validity }
 function  DiskInDrive        (CONST Path: string): Boolean; overload;                { From www.gnomehome.demon.nl/uddf/pages/disk.htm#disk0 . Also see http://community.borland.com/article/0,1410,15921,00.html }
 function  DiskInDrive        (CONST DriveNo: Byte): Boolean; overload;               { THIS IS VERY SLOW IF THE DISK IS NOT IN DRIVE! The GUI will freeze until the drive responds. }
 function  ValidDrive         (CONST Drive: Char): Boolean;                           { Peter Below (TeamB). http://www.codinggroups.com/borland-public-delphi-rtl-win32/7618-windows-no-disk-error.html }
 
 function  PathHasValidColon    (const Path: string): Boolean;
 
 {}
 function  DriveFreeSpace     (CONST Drive: Char): Int64;
 function  DriveFreeSpaceS    (CONST Drive: Char): string;
 function  DriveFreeSpaceF    (CONST FullPath: string): Int64;                        { Same as DriveFreeSpace but this accepts a full filename/directory path. It will automatically extract the drive }



IMPLEMENTATION

USES
  ccCore, cbDialogs, cbRegistry, cbVersion, ccIO;


{ Copied from IOUtils.TPath.HasPathValidColon where it is PRIVATE }
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


function DirectoryExistMsg(CONST Path: string): Boolean;                                           { Directory Exist }
begin
  Result:= DirectoryExists(Path);
  if NOT Result then
    if Path= ''
    then MessageError('DirectoryExistMsg: No folder specified!')
    else
      if Pos(':', Path)< 1                                                                             { check if the user has given a full path as c:\xxx }
      then MessageError('A relative path was provided instead of a full path!'+ CRLFw+ Path)
      else MessageError('Folder does not exist:'+ CRLFw+ Path);
end;


{ Shows an error message if the folder cannot be created. }
function ForceDirectoriesMsg(CONST FullPath: string): Boolean;
begin
  Result:= ccIO.ForceDirectories(FullPath) >= 0;
  if NOT Result
  then MessageError('Cannot create folder: '+ FullPath+ CRLFw+ 'Probably you are trying to write to a folder to which you don''t have write permissions, or, the folder you want to create is invalid.');
end;



 { File Exists }
function FileExistsMsg(CONST FileName: string): Boolean;
begin
 Result:= FileExists(FileName);
 if NOT Result then
 if FileName= ''
 then MessageError('No file specified!')
 else MessageError('File does not exist!'+ CRLFw+ FileName);
end;



function DeleteFileWithMsg(const FileName: string): Boolean;
begin
 Result:= DeleteFile(FileName);
 if NOT Result
 then MessageError('Cannot delete file '+CRLFw+ FileName);
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




{_______________________________________________________________________________________________________________________

Q: What is the difference between the new TFileOpenDialog and the old TOpenDialog?
A: TOpenDialog will delegate the work to TFileOpenDialog if following conditions are met:

    Running on Windows Vista or later.
    Dialogs.UseLatestCommonDialogs global boolean variable is true (default is true).
    No dialog template is specified.
    OnIncludeItem, OnClose and OnShow events are all not assigned.

http://stackoverflow.com/questions/6236275/what-is-the-difference-between-the-new-tfileopendialog-and-the-old-topendialog
________________________________________________________________________________________________________________________

TFileDialogOption
   fdoOverWritePrompt    = Prompt before overwriting an existing file of the same name when saving a file. This is a default for save dialogs.
   fdoPickFolders        = Choose folders rather than files.
   fdoForceFileSystem    = Returned items must be file system items.
   fdoAllNonStorageItems = Allow users to choose any item in the Shell namespace. This flag cannot be combined with fdoForceFileSystem.
   fdoNoValidate         = Do not check for situations preventing applications from opening selected files, such as sharing violations or access denied errors.
   fdoAllowMultiSelect   = Allow selecting multiple items in an open dialog.
   fdoPathMustExist      = Items returned must be in an existing folder. This is a default.
   fdoFileMustExist      = Items returned must exist. This is a default value for open dialogs.
   fdoCreatePrompt       = Prompt for creation if returned item in save dialog does not exist. This does not create the item.
   fdoShareAware         = For a sharing violation opening a file, call the application back for guidance. This flag is overridden by fdoNoValidate.
   fdoNoReadOnlyReturn   = Do not return read-only items.
   fdoHideMRUPlaces      = Hide places of recently opened or saved items.
   fdoHidePinnedPlaces   = Hide pinned places from which users can choose.
   fdoNoDereferenceLinks = Shortcuts are not treated as their target items, allowing applications to open .lnk files.
   fdoDontAddToRecent    = Do not add the item being opened or saved to the list of recent places.
   fdoForceShowHidden    = Show hidden items.
   fdoForcePreviewPaneOn = Display the preview pane.
   fdoDefaultNoMiniMode  = Open save dialog box in expanded mode in which users can browse folders. Expanded mode is set and unset by clicking the button in the lower-left corner of a save dialog box.

  SAVE RELATED
   fdoStrictFileTypes    = The file extension of a saved file being must match the selected file type.
   fdoNoTestFileCreate   = Do not test creation of returned item from save dialogs. If not set, the calling application must handle errors discovered in the creation test.
   fdoNoChangeDir        = Unused.
_______________________________________________________________________________________________________________________}

{$IFDEF MSWindows}
{ Keywords: FolderDialog, BrowseForFolder
  stackoverflow.com/questions/19501772
  Works with UNC paths

  Also see:
    since Delphi 10/Seattle
    (it is effectively the same thing as the TFileOpenDialog approach, but with less boilerplate code)
    function SelectDirectory(const StartDirectory: string; out Directories: TArray<string>; Options: TSelectDirFileDlgOpts = []; const Title: string = ''; const FolderNameLabel: string = ''; const OkButtonLabel: string = ''): Boolean; overload;
    https://stackoverflow.com/questions/68286754/where-is-the-modern-looking-selectdirectory-function
}
function SelectAFolder(VAR Folder: string; CONST Title: string = ''; CONST Options: TFileDialogOptions= [fdoPickFolders, fdoForceFileSystem, fdoPathMustExist, fdoDefaultNoMiniMode]): Boolean;    { intoarce true daca userul a dat OK si false daca userul a dat cancel } { Keywords: FolderDialog, BrowseForFolder}  { http://stackoverflow.com/questions/19501772/i-need-a-decent-open-folder-dialog#19501961 }
VAR Dlg: TFileOpenDialog;
begin
 { Win Vista and up }
 if cbVersion.IsWindowsVistaUp then
  begin
   Dlg:= TFileOpenDialog.Create(NIL);   { Class for Vista and newer Windows operating systems style file open dialogs }
    TRY
      Dlg.Options       := Options;               //[fdoPickFolders, fdoPathMustExist, fdoForceFileSystem]; // YMMV
      Dlg.DefaultFolder := Folder;
      Dlg.FileName      := Folder;
      if Title > '' then Dlg.Title:= Title;
      Result            := Dlg.Execute;
      if Result
      then Folder:= Dlg.FileName;
    FINALLY
      FreeAndNil(Dlg);
    END;
  end
 else
   { Win XP or down }
   Result:= vcl.FileCtrl.SelectDirectory('', ExtractFileDrive(Folder), Folder, [sdNewUI, sdShowEdit, sdNewFolder], nil); { This shows the 'Edit folder' editbox at the bottom of the dgl window }

 if Result
 then Folder:= Trail(Folder);
end;
{$ENDIF}






{--------------------------------------------------------------------------------------------------
   SPECIAL FOLDERS
--------------------------------------------------------------------------------------------------}
function GetWinDir: string; { Returns Windows folder }
VAR  Windir: PChar;
begin
  Windir:= GetMemory(256);
  GetWindowsDirectory(windir, 256);
  Result:= IncludeTrailingPathDelimiter(Windir);
  FreeMemory(Windir);
end;


function GetWinSysDir: string;
VAR SysDir: PChar;
begin
   SysDir := StrAlloc(MAX_PATH);
   GetSystemDirectory(SysDir, MAX_PATH);
   Result := string(SysDir);
   Result := Trail(Result);
   StrDispose(SysDir);
end;


function GetProgramFilesDir: string;
begin
  Result:= Trail(RegReadString(HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\Windows\CurrentVersion', 'ProgramFilesDir'));
end;


function GetDesktopFolder: string;
begin
 Result:= Trail(GetSpecialFolder(CSIDL_DESKTOPDIRECTORY));
end;


function GetStartMenuFolder: string;
begin
 Result:= Trail(GetSpecialFolder(CSIDL_STARTMENU));
end;


function GetMyDocumentsAPI: string;
begin
 Result:= Trail(GetSpecialFolder(CSIDL_PERSONAL));
end;


function GetMyPicturesAPI: string;
VAR s: string;
begin
 s:= GetSpecialFolder(CSIDL_MYPICTURES);
 if s= ''
 then Result:= ''
 else Result:= Trail(GetSpecialFolder(CSIDL_MYPICTURES));
end;


{ Retrieve the shell folders from registry
  DOCS:
  Calling the API function is safer, because the registry structure may change. It has not changed in W2k, but it may happen.
  SHGetFolderSpecialLocation uses the registry now, but may read its data from any other structure in a future version of Windows.
  The published API is the "right" way to access these data, because Microsoft has to support it for a long time.
  Writing application that use undocumented features expose them to compatibility issues.
  The folders retrieved should include:
    csShellAppData =    'AppData';
    csShellCache =      'Cache';
    csShellCookies =    'Cookies';
    csShellDesktop =    'Desktop';
    csShellFavorites =  'Favorites';
    csShellFonts =      'Fonts';
    csShellHistory =    'History';
    csShellLocalApp =   'Local AppData';
    csShellNetHood =    'NetHood';
    csShellPersonal =   'Personal';
    csShellPrintHood =  'PrintHood';
    csShellPrograms =   'Programs';
    csShellRecent =     'Recent';
    csShellSendTo =     'SendTo';
    csShellStartMenu =  'Start Menu';
    csShellStartUp =    'Startup';
    csShellTemplates =  'Templates';                                          }
function GetSpecialFolder (CONST OS_SpecialFolder: string): string;
VAR Reg: TRegistry;
begin
  Result:= '';
  reg := TRegistry.Create(KEY_READ);
  TRY
   reg.RootKey := HKEY_CURRENT_USER;
   reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders', FALSE);
   Result:= reg.ReadString(OS_SpecialFolder);                                                      { for example OS_SpecialFolder= 'Start Menu' }
   reg.CloseKey;
  FINALLY
   FreeAndNil(reg);
  END;
end;


{--------------------------------------------------------------------------------------------------
 uses SHFolder
 As recommended by Borland in this doc: VistaUACandDelphi.pdf
 Minimum operating systems: Windows 95 with Internet Explorer 5.0, Windows 98 with Internet Explorer 5.0, Windows 98 Second Edition (SE), Windows NT 4.0 with Internet Explorer 5.0, Windows NT 4.0 with Service Pack 4 (SP4)

 SPECIAL FOLDERS CONSTANTS:
   Full list of 'Special folder constants' are available here:  http://msdn.microsoft.com/en-us/library/bb762494(VS.85).aspx
   'Special folder constants' are declared in ShlObj and SHFolder (as duplicates). SHFolder.pas is the interface for Shfolder.dll.
--------------------------------------------------------------------------------------------------}
{
del
function GetSpecialFolder (CSIDL: Integer; ForceFolder: Boolean = FALSE): string;
VAR i: Integer;
begin
 SetLength(Result, MAX_PATH);
 if ForceFolder
 then ShGetFolderPath(0, CSIDL OR CSIDL_FLAG_CREATE, 0, 0, PChar(Result))
 else ShGetFolderPath(0, CSIDL, 0, 0, PChar(Result));

 i:= Pos(#0, Result);
 if i> 0
 then SetLength(Result, pred(i));

 Result:= Trail (Result);
end;  }


function GetSpecialFolder(CSIDL: Integer; ForceFolder: Boolean = False): string;
var
  Buffer: array[0..MAX_PATH - 1] of Char; // Use a fixed-size buffer.
begin
  case CSIDL of
   { Some IDs like CSIDL_NETWORK represents special folder differs from standard filesystem folders and lacks a physical path.
     ShGetFolderPath is designed to return file system paths and cannot handle virtual folders like CSIDL_NETWORK. It will fail for CSIDL_NETWORK. For such folders, you need to use APIs that interact with the Shell namespace rather than file paths. For example: SHGetSpecialFolderLocation / SHGetFolderLocation }
    CSIDL_NETWORK         : EXIT('');  // "Network Neighborhood" or "Network" virtual folder.
    CSIDL_PRINTERS        : EXIT('');  // "Printers" virtual folder, showing installed printers.
    CSIDL_BITBUCKET       : EXIT('');  // "Recycle Bin," which is not a single physical location.
    CSIDL_CONTROLS        : EXIT('');  // "Control Panel," which is a purely virtual location.
    CSIDL_DRIVES          : EXIT('');  // "My Computer" or "This PC," listing drives and devices.
    CSIDL_COMPUTERSNEARME : EXIT('');  // "Computers Near Me," showing nearby network computers.
    CSIDL_INTERNET        : EXIT('');  // "Internet" virtual folder (usually unused in modern Windows).
    CSIDL_CONNECTIONS     : EXIT('');  // Network and Dial-up Connections
    CSIDL_COMMON_OEM_LINKS: EXIT('');  // Links to All Users OEM specific apps
  end;

  if ForceFolder
  then ShGetFolderPath(0, CSIDL or CSIDL_FLAG_CREATE, 0, 0, Buffer)
  else ShGetFolderPath(0, CSIDL, 0, 0, Buffer);

  // Convert null-terminated buffer to string.
  Result := StrPas(Buffer);

  // Ensure the result ends with a trailing path delimiter if necessary.
  if ForceFolder
  then Result:= Trail(Result);
end;


{ Get a list of ALL special folders. Used by Uninstaller. }
function GetSpecialFolders: TStringList;
begin
 Result:= TStringList.Create;                                                          //  FROM ShlObj.pas
 Result.Add(GetSpecialFolder(CSIDL_DESKTOP                 , FALSE));                  // <desktop>
 Result.Add(GetSpecialFolder(CSIDL_PROGRAMS                , FALSE));                  // Start Menu\Programs
 Result.Add(GetSpecialFolder(CSIDL_PERSONAL                , FALSE));                  // My Documents
 Result.Add(GetSpecialFolder(CSIDL_FAVORITES               , FALSE));                  // <user name>\Favorites
 Result.Add(GetSpecialFolder(CSIDL_STARTUP                 , FALSE));                  // Start Menu\Programs\Startup
 Result.Add(GetSpecialFolder(CSIDL_RECENT                  , FALSE));                  // <user name>\Recent
 Result.Add(GetSpecialFolder(CSIDL_SENDTO                  , FALSE));                  // <user name>\SendTo
 Result.Add(GetSpecialFolder(CSIDL_STARTMENU               , FALSE));                  // <user name>\Start Menu
 Result.Add(GetSpecialFolder(CSIDL_MYDOCUMENTS             , FALSE));                  // Personal was just a silly name for My Documents
 Result.Add(GetSpecialFolder(CSIDL_MYMUSIC                 , FALSE));                  // "My Music" folder
 Result.Add(GetSpecialFolder(CSIDL_MYVIDEO                 , FALSE));                  // "My Videos" folder
 Result.Add(GetSpecialFolder(CSIDL_DESKTOPDIRECTORY        , FALSE));                  // <user name>\Desktop
 Result.Add(GetSpecialFolder(CSIDL_NETHOOD                 , FALSE));                  // <user name>\nethood
 Result.Add(GetSpecialFolder(CSIDL_FONTS                   , FALSE));                  // windows\fonts
 Result.Add(GetSpecialFolder(CSIDL_TEMPLATES               , FALSE));
 Result.Add(GetSpecialFolder(CSIDL_COMMON_STARTMENU        , FALSE));                  // All Users\Start Menu
 Result.Add(GetSpecialFolder(CSIDL_COMMON_PROGRAMS         , FALSE));                  // All Users\Start Menu\Programs
 Result.Add(GetSpecialFolder(CSIDL_COMMON_STARTUP          , FALSE));                  // All Users\Startup
 Result.Add(GetSpecialFolder(CSIDL_COMMON_DESKTOPDIRECTORY , FALSE));                  // All Users\Desktop
 Result.Add(GetSpecialFolder(CSIDL_APPDATA                 , FALSE));                  // <user name>\Application Data
 Result.Add(GetSpecialFolder(CSIDL_PRINTHOOD               , FALSE));                  // <user name>\PrintHood
 Result.Add(GetSpecialFolder(CSIDL_LOCAL_APPDATA           , FALSE));                  // <user name>\Local Settings\Applicaiton Data (non roaming)
 Result.Add(GetSpecialFolder(CSIDL_ALTSTARTUP              , FALSE));                  // non localized startup
 Result.Add(GetSpecialFolder(CSIDL_COMMON_ALTSTARTUP       , FALSE));                  // non localized common startup
 Result.Add(GetSpecialFolder(CSIDL_COMMON_FAVORITES        , FALSE));
 Result.Add(GetSpecialFolder(CSIDL_INTERNET_CACHE          , FALSE));
 Result.Add(GetSpecialFolder(CSIDL_COOKIES                 , FALSE));
 Result.Add(GetSpecialFolder(CSIDL_HISTORY                 , FALSE));
 Result.Add(GetSpecialFolder(CSIDL_COMMON_APPDATA          , FALSE));                  // All Users\Application Data
 Result.Add(GetSpecialFolder(CSIDL_WINDOWS                 , FALSE));                  // GetWindowsDirectory()
 Result.Add(GetSpecialFolder(CSIDL_SYSTEM                  , FALSE));                  // GetSystemDirectory()
 Result.Add(GetSpecialFolder(CSIDL_PROGRAM_FILES           , FALSE));                  // C:\Program Files
 Result.Add(GetSpecialFolder(CSIDL_MYPICTURES              , FALSE));                  // C:\Program Files\My Pictures
 Result.Add(GetSpecialFolder(CSIDL_PROFILE                 , FALSE));                  // USERPROFILE
 Result.Add(GetSpecialFolder(CSIDL_SYSTEMX86               , FALSE));                  // x86 system directory on RISC
 Result.Add(GetSpecialFolder(CSIDL_PROGRAM_FILESX86        , FALSE));                  // x86 C:\Program Files on RISC
 Result.Add(GetSpecialFolder(CSIDL_PROGRAM_FILES_COMMON    , FALSE));                  // C:\Program Files\Common
 Result.Add(GetSpecialFolder(CSIDL_PROGRAM_FILES_COMMONX86 , FALSE));                  // x86 Program Files\Common on RISC
 Result.Add(GetSpecialFolder(CSIDL_COMMON_TEMPLATES        , FALSE));                  // All Users\Templates
 Result.Add(GetSpecialFolder(CSIDL_COMMON_DOCUMENTS        , FALSE));                  // All Users\Documents
 Result.Add(GetSpecialFolder(CSIDL_COMMON_ADMINTOOLS       , FALSE));                  // All Users\Start Menu\Programs\Administrative Tools
 Result.Add(GetSpecialFolder(CSIDL_ADMINTOOLS              , FALSE));                  // <user name>\Start Menu\Programs\Administrative Tools
 Result.Add(GetSpecialFolder(CSIDL_COMMON_MUSIC            , FALSE));                  // All Users\My Music
 Result.Add(GetSpecialFolder(CSIDL_COMMON_PICTURES         , FALSE));                  // All Users\My Pictures
 Result.Add(GetSpecialFolder(CSIDL_COMMON_VIDEO            , FALSE));                  // All Users\My Video
 Result.Add(GetSpecialFolder(CSIDL_RESOURCES               , FALSE));                  // Resource Direcotry
 Result.Add(GetSpecialFolder(CSIDL_RESOURCES_LOCALIZED     , FALSE));                  // Localized Resource Direcotry
 Result.Add(GetSpecialFolder(CSIDL_CDBURN_AREA             , FALSE));                  // USERPROFILE\Local Settings\Application Data\Microsoft\CD Burning
end;


{ Returns True if the parameter is a special folder such us 'c:\My Documents' }
function FolderIsSpecial(const Path: string): Boolean;
VAR s: string;
    SpecialFolders: TStringList;
begin
 Result:= FALSE;
 SpecialFolders:= GetSpecialFolders;
 TRY
  for s in SpecialFolders DO
   if SameFolder(Path, s)
   then EXIT(TRUE);
 FINALLY
  FreeAndNil(SpecialFolders);
 END;
end;


{ Returns the taskmgr.exe file }
function GetTaskManager: String;
begin
 Result:= GetWinSysDir+ 'taskmgr.exe';
end;



{--------------------------------------------------------------------------------------------------
   DELETE FILE
   Deletes a file/folder to RecycleBin.
   Old name: Trashafile
   Note related to UNC: The function won't move a file to the RecycleBin if the file is UNC. MAYBE it was moved to the remote's computer RecycleBin
--------------------------------------------------------------------------------------------------}
function RecycleItem(CONST ItemName: string; CONST DeleteToRecycle: Boolean= TRUE; CONST ShowConfirm: Boolean= TRUE; CONST TotalSilence: Boolean= FALSE): Boolean;
VAR
   SHFileOpStruct: TSHFileOpStruct;
begin
 FillChar(SHFileOpStruct, SizeOf(SHFileOpStruct), #0);
 SHFileOpStruct.wnd              := Application.MainForm.Handle;                                   { Others are using 0. But Application.MainForm.Handle is better because otherwise, the 'Are you sure you want to delete' will be hidden under program's window }
 SHFileOpStruct.wFunc            := FO_DELETE;
 SHFileOpStruct.pFrom            := PChar(ItemName+ #0);                                           { ATENTION!   This last #0 is MANDATORY. See this for details: http://stackoverflow.com/questions/6332259/i-cannot-delete-files-to-recycle-bin  -   Although this member is declared as a single null-terminated string, it is actually a buffer that can hold multiple null-delimited file names. Each file name is terminated by a single NULL character. The last file name is terminated with a double NULL character ("\0\0") to indicate the end of the buffer }
 SHFileOpStruct.pTo              := NIL;
 SHFileOpStruct.hNameMappings    := NIL;

 if DeleteToRecycle
 then SHFileOpStruct.fFlags:= SHFileOpStruct.fFlags OR FOF_ALLOWUNDO;

 if TotalSilence
 then SHFileOpStruct.fFlags:= SHFileOpStruct.fFlags OR FOF_NO_UI
 else
   if NOT ShowConfirm
   then SHFileOpStruct.fFlags:= SHFileOpStruct.fFlags OR FOF_NOCONFIRMATION;

 Result:= SHFileOperation(SHFileOpStruct)= 0;

 //DEBUG ONLY if Result<> 0 then Mesaj('last error: ' + IntToStr(Result)+ CRLF+ 'last error message: '+ SysErrorMessage(Result));
 //if fos.fAnyOperationsAborted = True then Result:= -1;
end;


{ Ensure the current path is valid and can be used with 'FileOperation' }
function _validateForFileOperation(CONST sPath: string): Boolean;
begin
  Result:=
      (sPath <> 'Control Panel')
  AND (sPath <> 'Recycle Bin')
  AND (Length(sPath) > 0)
  AND (Pos('nethood', sPath) <= 0);
end;


{ Performs: Copy, Move, Delete, Rename on files + folders via WinAPI.
  Example: FileOperation(FileOrFolder, '', FO_DELETE, FOF_ALLOWUNDO)  }
function FileOperation(CONST Source, Dest : string; Op, Flags: Integer): Boolean;
VAR
  SHFileOpStruct : TSHFileOpStruct;
  src, dst : string;
  OpResult : integer;
begin
  Result:= _validateForFileOperation(Source);
  if NOT Result then EXIT;

  {setup file op structure}
  FillChar(SHFileOpStruct, SizeOf(SHFileOpStruct), #0);
  src := source + #0#0;
  dst := dest   + #0#0;

  SHFileOpStruct.Wnd := 0;
  SHFileOpStruct.wFunc := op;
  SHFileOpStruct.pFrom := PChar(src);
  SHFileOpStruct.pTo := PChar(dst);
  SHFileOpStruct.fFlags := flags;
  case Op of                                                          {set title for simple progress dialog}
    FO_COPY   : SHFileOpStruct.lpszProgressTitle := 'Copying...';
    FO_DELETE : SHFileOpStruct.lpszProgressTitle := 'Deleting...';
    FO_MOVE   : SHFileOpStruct.lpszProgressTitle := 'Moving...';
    FO_RENAME : SHFileOpStruct.lpszProgressTitle := 'Renaming...';
   end;
  OpResult := 1;
  TRY
    OpResult := SHFileOperation(SHFileOpStruct);
  FINALLY
    Result:= (OpResult = 0);                                          {report success / failure}
  END;
end;



procedure SetCompressionAtr(const FileName: string; const CompressionFormat: byte= 1);
CONST
  FSCTL_SET_COMPRESSION = $9C040;
  {
  COMPRESSION_FORMAT_NONE = 0;
  COMPRESSION_FORMAT_DEFAULT = 1;
  COMPRESSION_FORMAT_LZNT1 = 2; }
VAR
   Handle: THandle;
   Flags: DWORD;
   BytesReturned: DWORD;
begin
  if DirectoryExists(FileName)
  then Flags := FILE_FLAG_BACKUP_SEMANTICS
  else
    if FileExists(FileName)
    then Flags := 0
    else raise exception.CreateFmt('%s does not exist', [FileName]);

  Handle := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, Flags, 0);
  if Handle=0
  then RaiseLastOSError;

  if not DeviceIoControl(Handle, FSCTL_SET_COMPRESSION, @CompressionFormat, SizeOf(Comp), nil, 0, BytesReturned, nil) then
   begin
    CloseHandle(Handle);
    RaiseLastOSError;
   end;

  CloseHandle(Handle);
end;






{--------------------------------------------------------------------------------------------------
   GET FILE SIZE
--------------------------------------------------------------------------------------------------}
{ Used by GetSysFileTime in csSystem.pas
  REPLACEMENT
    For System.SysUtils.FileAge which is not working with 'c:\pagefile.sys'.
    Details dee: http://stackoverflow.com/questions/3825077/fileage-is-not-working-with-c-pagefile-sys
}
function FileAge(CONST FileName: string): TDateTime;
VAR
  LocalFileTime: TFileTime;
  SystemTime   : TSystemTime;
  SRec         : TSearchRec;
begin
 FindFirst(FileName, faAnyFile, SRec);
 TRY
   TRY
     {$WARN SYMBOL_PLATFORM OFF}   { OFF: Silence the 'W1005 Unit Vcl.FileCtrl is specific to a platform' warning }
     FileTimeToLocalFileTime(SRec.FindData.ftLastWriteTime, LocalFileTime);
     FileTimeToSystemTime(LocalFileTime, SystemTime);
     Result := SystemTimeToDateTime(SystemTime);
   EXCEPT   //todo 1: trap only specific exceptions
     on e: Exception do Result:= -1;
   END;
 FINALLY
   FindClose(SRec);
 END;
end;


function FileTimeToDateTimeStr(FTime: TFileTime; CONST DFormat, TFormat: string): string;
var
  SysTime       : TSystemTime;
  DateTime      : TDateTime;
  LocalFileTime : TFileTime;
begin
  FileTimeToLocalFileTime(Ftime, LocalFileTime);
  FileTimeToSystemTime(LocalFileTime, SysTime);
  DateTime := SystemTimeToDateTime(SysTime);
  Result   := FormatDateTime(DFormat + ' ' + TFormat, DateTime);
end;









{-------------------------------------------------------------------------------------------------------------
   Prompt To Save/Load File

   These functions are also duplicated in TAppData.
   The difference is that there, those functions read/write the LastUsedFolder var so the app can remmeber last use folder.

   Example: PromptToSaveFile(s, cGraphUtil.JPGFtl, 'txt')

   DefaultExt:
     Extensions longer than three characters are not supported!
     Do not include the dot (.)
-------------------------------------------------------------------------------------------------------------}
function PromptToSaveFile(VAR FileName: string; CONST Filter: string = ''; CONST DefaultExt: string= ''; CONST Title: string= ''): Boolean;
VAR InitialDir: string;
begin
 if FileName > '' then
   if IsFolder(FileName)
   then InitialDir:= FileName
   else InitialDir:= ExtractFilePath(FileName);

 Result:= PromptForFileName(FileName, TRUE, Filter, DefaultExt, Title, InitialDir);
end;


Function PromptToLoadFile(VAR FileName: string; CONST Filter: string = ''; CONST Title: string= ''): Boolean;
VAR InitialDir: string;
begin
 if FileName > '' then
   if IsFolder(FileName)
   then InitialDir:= FileName
   else InitialDir:= ExtractFilePath(FileName);

 Result:= PromptForFileName(FileName, FALSE, Filter, '', Title, InitialDir);
end;


{ Based on Vcl.Dialogs.PromptForFileName.
  AllowMultiSelect cannot be true, because I return a single file name (cannot return a Tstringlist)  }
Function PromptForFileName(VAR FileName: string; SaveDialog: Boolean; CONST Filter: string = ''; CONST DefaultExt: string= ''; CONST Title: string= ''; CONST InitialDir: string = ''): Boolean;
VAR
  Dialog: TOpenDialog;
begin
  if SaveDialog
  then Dialog := TSaveDialog.Create(NIL)
  else Dialog := TOpenDialog.Create(NIL);
  TRY
    { Options }
    Dialog.Options := Dialog.Options + [ofEnableSizing, ofForceShowHidden];
    if SaveDialog
    then Dialog.Options := Dialog.Options + [ofOverwritePrompt]
    else Dialog.Options := Dialog.Options + [ofFileMustExist];

    Dialog.Title := Title;
    Dialog.DefaultExt := DefaultExt;

    if Filter = ''
    then Dialog.Filter := Vcl.Consts.sDefaultFilter
    else Dialog.Filter := Filter;

    if InitialDir= ''
    then Dialog.InitialDir:= GetMyDocuments
    else Dialog.InitialDir:= InitialDir;

    Dialog.FileName := FileName;

    Result := Dialog.Execute;

    if Result
    then FileName:= Dialog.FileName;
  FINALLY
    FreeAndNil(Dialog);
  END;
end;






{-------------------------------------------------------------------------------------------------------------
   TFileOpenDlg

   Example: PromptToSaveFile(s, ccCore.FilterTxt, 'txt')
   Note: You might want to use PromptForFileName instead
-------------------------------------------------------------------------------------------------------------}

Function GetOpenDialog(CONST FileName, Filter, DefaultExt: string; CONST Caption: string= ''): TOpenDialog;
begin
 Result:= TOpenDialog.Create(NIL);
 Result.Filter:= Filter;
 Result.FilterIndex:= 0;
 Result.Options:= [ofFileMustExist, ofEnableSizing, ofForceShowHidden];
 Result.DefaultExt:= DefaultExt;
 Result.FileName:= FileName;
 Result.Title:= Caption;

 if FileName= ''
 then Result.InitialDir:= GetMyDocuments
 else Result.InitialDir:= ExtractFilePath(FileName);
end;


{ Example: SaveDialog(ccCore.FilterTxt, 'csv');  }
Function GetSaveDialog(CONST FileName, Filter, DefaultExt: string; CONST Caption: string= ''): TSaveDialog;
begin
 Result:= TSaveDialog.Create(NIL);
 Result.Filter:= Filter;
 Result.FilterIndex:= 0;
 Result.Options:= [ofOverwritePrompt, ofHideReadOnly, ofFileMustExist, ofEnableSizing];  //  - ofNoChangeDir  { When a user displays the open dialog, whether InitialDir is used or not, the dialog alters the program's current working directory while the user is changing directories before clicking on the Ok/Open button. Upon closing the dialog, the current working directly is not reset to its original value unless the ofNoChangeDir option is specified.  }
 Result.DefaultExt:= DefaultExt;
 Result.FileName:= FileName;
 Result.Title:= Caption;

 if FileName= ''
 then Result.InitialDir:= GetMyDocuments
 else Result.InitialDir:= ExtractFilePath(FileName);
end;





{--------------------------------------------------------------------------------------------------
   FILE LOCKED?
--------------------------------------------------------------------------------------------------}


{ Returns true if the file cannot be open for reading and writing }                                           { old name: FileInUse }
function FileIsLockedRW(CONST FileName: string): Boolean;
VAR hFileRes: HFILE;
begin
 if NOT FileExists(FileName) then EXIT(FALSE);                       { If files doesn't exist it cannot be locked! }

 hFileRes := CreateFile(PChar(FileName), GENERIC_READ OR GENERIC_WRITE, 0, NIL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
 Result := (hFileRes = INVALID_HANDLE_VALUE);
 if NOT Result then CloseHandle(hFileRes);
end;


function FileIsLockedR(CONST FileName: string): Boolean;                   { Returns true if the file cannot be open for reading }
VAR hFileRes: HFILE;
begin
 if NOT FileExists(FileName)
 then RAISE exception.Create('File does not exist!'+ CRLFw+ FileName);

 hFileRes := CreateFile(PChar(FileName), GENERIC_READ, 0, NIL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
 Result := (hFileRes = INVALID_HANDLE_VALUE);
 if NOT Result then CloseHandle(hFileRes);
end;


function CanCreateFile(const FileName: String): Boolean;
begin
  Result := CanWriteToFolder(ExtractFilePath(FileName), ExtractFileName(FileName));
end;



{--------------------------------------------------------------------------------------------------
   FOLDER LOCKED?
--------------------------------------------------------------------------------------------------}

{ Formats a error message complaining that we cannot write to a file. }
function ShowMsg_CannotWriteTo(CONST sPath: string): string;                                                  { old name: ReturnCannotWriteTo }
begin
 Result:= 'Cannot write to "'+ sPath+ '"'
           +LBRK+ 'Possilbe cause:'
           +CRLF+ ' * the file/folder is read-only'
           +CRLF+ ' * the file/folder is locked by other program'
           +CRLF+ ' * you don''t have necessary privileges to write there'
           +CRLF+ ' * the drive is not ready'

           +LBRK+ 'You can try to:'
           +CRLF+ ' * use a different folder'
           +CRLF+ ' * change the privileges (or contact the admin to do it)'
           +CRLF+ ' * run the program with elevated rights (as administrator)'
end;


function CanWriteToFolderMsg(CONST Folder: string): Boolean;
begin
 Result:= CanWriteToFolder(Folder);
 if NOT Result
 then MessageWarning(ShowMsg_CannotWriteTo(Folder));
end;


{ Returns true if it can write to that folder }
function CanWriteToFolder(CONST Folder: string; const FileName: String = 'TempFile.Delete.Me'): Boolean;  // Old name: TestWriteAccess
VAR Handle: THandle;
begin
  Handle:= Winapi.Windows.CreateFile(PChar(trail(Folder)+FileName),
             GENERIC_READ or GENERIC_WRITE,                             // GENERIC_READ or GENERIC_WRITE = tests full write permissions, including the ability to create, open, and modify files. Using GENERIC_WRITE alone might be more accurate if we are only testing write permissions, but using both ensures that the file can be fully accessed.
             0,
             nil,
             CREATE_ALWAYS,                                             // CREATE_ALWAYS: Creates a new file or overwrites an existing one if it already exists. CREATE_NEW: Fails if the file already exists, requiring an additional check or error handling.
             FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE,
             0);

  Result:= Handle <> INVALID_HANDLE_VALUE;
  if Result then Winapi.Windows.CloseHandle(Handle);
end;







{--------------------------------------------------------------------------------------------------
   FILE COPY/MOVE
--------------------------------------------------------------------------------------------------}
function FileMoveTo(CONST From_FullPath, To_FullPath: string): boolean;
begin
 {if Overwrite
 then Flag:= MOVEFILE_REPLACE_EXISTING
 else Flag:= xxxx }
 Result:= MoveFileEx(PChar(From_FullPath), PChar(To_FullPath), MOVEFILE_REPLACE_EXISTING);
end;


{ Same as FileMoveTo but the user will provide a folder for the second parameter instead of a full path (folder = file name) } { Old name: FileMoveQuick }
{ If destination folder does not exists it is created }
function FileMoveToDir(CONST From_FullPath, To_DestFolder: string; Overwrite: Boolean): boolean;
VAR Op: Cardinal;
begin
 if Overwrite
 then Op:= MOVEFILE_REPLACE_EXISTING
 else Op:= 0;

 ForceDirectories(To_DestFolder);       { If destination folder does not exists it won't be created bu also no error will be raised. So, I create it here }
 Result:= MoveFileEx(PChar(From_FullPath), PChar(Trail(To_DestFolder)+ ExtractFileName(From_FullPath)), Op);
end;





{--------------------------------------------------------------------------------------------------
   DRIVE
--------------------------------------------------------------------------------------------------}
{ Returns drive type. Path can be something like 'C:\' or '\\netdrive\' }
function GetDriveType(CONST Path: string): Integer;
begin
 Result:= winapi.Windows.GetDriveType(PChar(Trail(Path)));    { Help page: https://msdn.microsoft.com/en-us/library/windows/desktop/aa364939%28v=vs.85%29.aspx }
end;


function GetDriveTypeS(CONST Path: string): string;
begin
 case GetDriveType(Path) of
   DRIVE_UNKNOWN     : Result:= 'The drive type cannot be determined.';
   DRIVE_NO_ROOT_DIR : Result:= 'The root path is invalid'; // for example, there is no volume mounted at the specified path.
   DRIVE_REMOVABLE   : Result:= 'Drive Removable';
   DRIVE_FIXED       : Result:= 'Drive fixed';
   DRIVE_REMOTE      : Result:= 'Remote Drive';
   DRIVE_CDROM       : Result:= 'CD ROM Drive';
   DRIVE_RAMDISK     : Result:= 'RAM Drive';
 end;
end;


{ Source: TeamB }
function ValidDrive(CONST Drive: Char): Boolean;
VAR mask: String;
    sRec: TSearchRec;
    oldMode: Cardinal;
    retCode: Integer;
begin
 oldMode:= SetErrorMode( SEM_FAILCRITICALERRORS );
 mask:= Drive+ ':\*.*';
 {$I-}                                                      { don't raise exceptions if we fail }
 retCode:= FindFirst( mask, faAnyfile, SRec );              { %%%% THIS IS VERY SLOW IF THE DISK IS NOT IN DRIVE !!!!!! }
 if retcode= 0
 then FindClose( SRec );
 {$I+}
 Result := Abs(retcode) in [ERROR_SUCCESS,ERROR_FILE_NOT_FOUND,ERROR_NO_MORE_FILES];
 SetErrorMode( oldMode );
end;






function GetVolumeLabel(CONST Drive: Char): string;
var
  OldErrorMode: Integer;
  NotUsed, VolFlags: DWORD;
  Buf: array [0..MAX_PATH] of Char;    //ok
begin
  Result:= '';
  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  TRY
    Buf[0] := #$00;
    if GetVolumeInformation(PChar(Drive + ':\'), Buf, DWORD(sizeof(Buf)), nil, NotUsed, VolFlags, nil, 0)
    then SetString(Result, Buf, StrLen(Buf))
    else Result := '';

    if Drive < 'a'
    then Result := AnsiUpperCase(Result)                                                           { Converts a string to uppercase. }
    else Result := AnsiLowerCase(Result);

    Result := Format('[%s]', [Result]);
  FINALLY
    SetErrorMode(OldErrorMode);
  end;
end;




{ Not tested with network drives!
  Source www.gnomehome.demon.nl/uddf/pages/disk.htm#disk0 .
  Also see community.borland.com/article/0,1410,15921,00.html }
function DiskInDrive(CONST Path: string): Boolean;
VAR
   DriveNumber: Byte;
   DriveType: Integer;
begin
  DriveType:= GetDriveType(Path);

  if DriveType < DRIVE_REMOVABLE
  then Result:= FALSE                                                                               { This happens when a network drive is offline }
  else
    if DriveType = DRIVE_REMOTE
    then Result:= TRUE                                                                              {TODO 2: I need a function that checks if the network drive is connected }
    else
     begin
      DriveNumber:= Drive2Byte(Path[1]);
      RESULT:= DiskInDrive(DriveNumber);
     end;
end;


function DiskInDrive(CONST DriveNo: Byte): BOOLEAN;                                                 { THIS IS VERY SLOW IF THE DISK IS NOT IN DRIVE! The GUI will freeze until the drive responds.    Solution: http://stackoverflow.com/questions/1438923/faster-directoryexists-function }
VAR ErrorMode  : Word;
begin
  RESULT:= FALSE;
  ErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  TRY
    if DiskSize(DriveNo) <> -1
    THEN RESULT:= TRUE;
  FINALLY
    SetErrorMode(ErrorMode);
  END;
END;





function DriveFreeSpace(CONST Drive: CHAR): Int64;
VAR DriveNo: Byte;
begin
 DriveNo:= Drive2Byte(drive);

 if  ValidDrive(drive)
 AND DiskInDrive(DriveNo)
 then Result:= DiskFree(DriveNo)
 else Result:= 0;
end;


function DriveFreeSpaceS(CONST Drive: CHAR): string;
begin
 Result:= FormatBytes(DriveFreeSpace(Drive), 1);
end;


{ Same as DriveFreeSpace but this accepts a full filename/directory path. It will automatically extract the drive }
function DriveFreeSpaceF(CONST FullPath: string): Int64;
begin
 Result:= DriveFreeSpace(System.IOUtils.TDirectory.GetDirectoryRoot(FullPath)[1]);                  { GetDirectoryRoot returns something like: 'C:\' }
end;


{ The function determines the index in the string where the "real" path begins, after accounting for Windows' extended path prefixes:

    \\?\           (for local paths)
    \\?\UNC\       (for UNC/network paths)

  Example:
    For "\\?\C:\Documents\file.txt" it returns 5, indicating the actual path starts at index 5 (C:\Documents\file.txt).
}
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



end.


