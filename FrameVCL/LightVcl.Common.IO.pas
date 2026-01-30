UNIT LightVcl.Common.IO;

{=============================================================================================================
   2026.01.29
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Extension for LightCore.IO.pas
   Framework: VCL only
   Shows error messages (dialog boxes) when the I/O operation failed.
==================================================================================================}

INTERFACE
{ $WARN UNIT_PLATFORM OFF}   { OFF: Silence the 'W1005 Unit Vcl.FileCtrl is specific to a platform' warning }

USES
  Winapi.Windows, Winapi.ShellAPI, Winapi.ShlObj,
  System.StrUtils, System.IOUtils, System.SysUtils, System.Win.Registry, System.Classes,
  Vcl.Consts, Vcl.Controls, Vcl.Dialogs, Vcl.Forms, Vcl.FileCtrl;


{--------------------------------------------------------------------------------------------------
   OPERATIONS WITH MESSAGE
--------------------------------------------------------------------------------------------------}
 function  DirectoryExistMsg    (CONST Path: string): Boolean;
 function  FileExistsMsg        (CONST FileName: string): Boolean;
 function  ForceDirectoriesMsg  (CONST FullPath: string): Boolean;                                 { Wrapper around LightCore.IO.ForceDirectories. Returns True if directory exists or was created, False on failure. Shows error dialog on failure. }

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
 function FileMoveTo            (CONST From_FullPath, To_FullPath: string): Boolean;                           { Moves a file to a new location, overwriting if exists }
 function FileMoveToDir         (CONST From_FullPath, To_DestFolder: string; Overwrite: Boolean): Boolean;     { Moves a file to a destination folder }

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

 function  CanWriteToFolder     (CONST Folder: string; const FileName: String = 'TempFile.Delete.Me'): Boolean;    { Tests folder write access by creating a temporary file. The temp file is auto-deleted on close. WARNING: If FileName already exists, it WILL be overwritten! }
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

 { Free space }
 function  DriveFreeSpace     (CONST Drive: Char): Int64;
 function  DriveFreeSpaceS    (CONST Drive: Char): string;
 function  DriveFreeSpaceF    (CONST FullPath: string): Int64;                        { Same as DriveFreeSpace but this accepts a full filename/directory path. It will automatically extract the drive }



IMPLEMENTATION

USES
  LightCore, LightVcl.Common.Registry, LightCore.IO, LightVcl.Common.Dialogs, LightVcl.Common.WinVersion;


{--------------------------------------------------------------------------------------------------
   Validates that a path doesn't contain illegal colon characters beyond the drive letter.
   A colon is only valid as the second character of a drive specification (e.g., 'C:\').
   Additional colons in the path (e.g., 'C:\folder:name') are invalid on Windows.

   Handles extended path prefixes ('\\?\' and '\\?\UNC\') correctly.

   Returns: True if path has valid colon usage, False if extra colons found.
   Note: Copied from System.IOUtils.TPath.HasPathValidColon (which is private).
--------------------------------------------------------------------------------------------------}
function PathHasValidColon(const Path: string): Boolean;
VAR
  StartIdx: Integer;
begin
  Result:= True;
  if Trim(Path) <> ''
  then
    begin
      StartIdx:= GetPosAfterExtendedPrefix(Path);
      if TPath.IsDriveRooted(Path)
      then Inc(StartIdx, 2);  { Skip past drive letter and colon }

      Result:= PosEx(TPath.VolumeSeparatorChar, Path, StartIdx) = 0;
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
  Result:= LightCore.IO.ForceDirectories(FullPath) >= 0;
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



{ Moves FromFolder to ToFolder using TDirectory.Move.
  If ToFolder already exists:
    - SilentOverwrite=True: copies all contents to ToFolder and deletes FromFolder
    - SilentOverwrite=False: prompts user to confirm deletion of ToFolder before moving
  Example: MoveFolderMsg('c:\Documents', 'C:\Backups', True) }
procedure MoveFolderMsg(CONST FromFolder, ToFolder: String; SilentOverwrite: Boolean);
begin
 if FromFolder = ''
 then raise Exception.Create('MoveFolderMsg: FromFolder parameter cannot be empty');

 if ToFolder = ''
 then raise Exception.Create('MoveFolderMsg: ToFolder parameter cannot be empty');

 if DirectoryExists(ToFolder) then
   if SilentOverwrite
   then
     begin
       CopyFolder(FromFolder, ToFolder, True);
       Deletefolder(FromFolder);
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
{--------------------------------------------------------------------------------------------------
   Shows a folder selection dialog and returns the selected folder path.

   Parameters:
     Folder  - VAR: Input as initial folder, output as selected folder (with trailing backslash).
     Title   - Optional dialog title.
     Options - TFileDialogOptions flags (Vista+ only). Defaults include fdoPickFolders.

   Returns: True if user selected a folder, False if cancelled.

   Implementation:
     - Vista+: Uses modern TFileOpenDialog with IFileDialog interface.
     - XP/older: Falls back to older SelectDirectory from Vcl.FileCtrl.

   Supports UNC paths on Vista and later.

   Alternative: Since Delphi 10 Seattle, Vcl.FileCtrl.SelectDirectory has an overload
   that provides similar functionality with less boilerplate code.
--------------------------------------------------------------------------------------------------}
function SelectAFolder(VAR Folder: string; CONST Title: string = ''; CONST Options: TFileDialogOptions= [fdoPickFolders, fdoForceFileSystem, fdoPathMustExist, fdoDefaultNoMiniMode]): Boolean;
VAR
  Dlg: TFileOpenDialog;
begin
 { Windows Vista and later - use modern dialog }
 if LightVcl.Common.WinVersion.IsWindowsVistaUp
 then
   begin
     Dlg:= TFileOpenDialog.Create(NIL);
     TRY
       Dlg.Options:= Options;
       Dlg.DefaultFolder:= Folder;
       Dlg.FileName:= Folder;
       if Title <> ''
       then Dlg.Title:= Title;
       Result:= Dlg.Execute;
       if Result
       then Folder:= Dlg.FileName;
     FINALLY
       FreeAndNil(Dlg);
     END;
   end
 else
   { Windows XP and earlier - use legacy dialog }
   Result:= Vcl.FileCtrl.SelectDirectory('', ExtractFileDrive(Folder), Folder, [sdNewUI, sdShowEdit, sdNewFolder], nil);

 if Result
 then Folder:= Trail(Folder);
end;
{$ENDIF}






{--------------------------------------------------------------------------------------------------
   SPECIAL FOLDERS
--------------------------------------------------------------------------------------------------}
function GetWinDir: string;
VAR
  Buffer: array[0..MAX_PATH - 1] of Char;
begin
  GetWindowsDirectory(Buffer, MAX_PATH);
  Result:= IncludeTrailingPathDelimiter(Buffer);
end;


function GetWinSysDir: string;
VAR
  Buffer: array[0..MAX_PATH - 1] of Char;
begin
  GetSystemDirectory(Buffer, MAX_PATH);
  Result:= Trail(Buffer);
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
begin
 Result:= Trail(GetSpecialFolder(CSIDL_MYPICTURES));
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
   if reg.OpenKeyReadOnly('Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders') then
     begin
       Result:= reg.ReadString(OS_SpecialFolder);                                                  { for example OS_SpecialFolder= 'Start Menu' }
       reg.CloseKey;
     end;
  FINALLY
   FreeAndNil(reg);
  END;
end;


{--------------------------------------------------------------------------------------------------
 Retrieves the path of a special folder using the Shell API (SHGetFolderPath).

 Parameters:
   CSIDL - The CSIDL constant identifying the special folder (defined in ShlObj.pas).
          See: https://docs.microsoft.com/en-us/windows/win32/shell/csidl
   ForceFolder - If True, creates the folder if it doesn't exist (uses CSIDL_FLAG_CREATE).
                 When True, also ensures result has a trailing path delimiter.

 Returns:
   The full path to the special folder, or empty string for virtual folders
   (CSIDL_NETWORK, CSIDL_PRINTERS, etc.) that have no filesystem path.

 Note: This function is preferred over reading from the registry (GetSpecialFolder string overload)
       because the API is guaranteed to remain stable across Windows versions.
--------------------------------------------------------------------------------------------------}
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


{ Returns a list of all known Windows special folder paths.
  The caller is responsible for freeing the returned TStringList.
  Some entries may be empty strings for folders that don't exist on the current system.
  Used by uninstaller to identify protected system folders. }
function GetSpecialFolders: TStringList;
begin
 Result:= TStringList.Create;
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


{ Returns True if Path matches any Windows special folder (Desktop, Documents, etc.).
  Uses case-insensitive comparison via SameFolder.
  Useful for preventing operations on protected system locations. }
function FolderIsSpecial(const Path: string): Boolean;
VAR
  Folder: string;
  SpecialFolders: TStringList;
begin
 Result:= FALSE;
 SpecialFolders:= GetSpecialFolders;
 TRY
   for Folder in SpecialFolders do
     if SameFolder(Path, Folder)
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
   DELETE FILE/FOLDER TO RECYCLE BIN
   Deletes a file or folder to the Recycle Bin using Windows Shell API (SHFileOperation).

   Parameters:
     ItemName        - Full path to the file or folder to delete. Cannot be empty.
     DeleteToRecycle - If True, moves to Recycle Bin (FOF_ALLOWUNDO). If False, permanently deletes.
     ShowConfirm     - If True, shows Windows confirmation dialog before deletion.
     TotalSilence    - If True, suppresses all UI (FOF_NO_UI) including progress dialogs and errors.
                       Takes precedence over ShowConfirm.

   Returns: True if deletion succeeded, False otherwise.

   Note: UNC paths may not work correctly - the file might be moved to the remote computer's
         Recycle Bin rather than the local one, or the operation may fail silently.
--------------------------------------------------------------------------------------------------}
function RecycleItem(CONST ItemName: string; CONST DeleteToRecycle: Boolean= TRUE; CONST ShowConfirm: Boolean= TRUE; CONST TotalSilence: Boolean= FALSE): Boolean;
VAR
   SHFileOpStruct: TSHFileOpStruct;
   WndHandle: HWND;
begin
 if ItemName = ''
 then raise Exception.Create('RecycleItem: ItemName parameter cannot be empty');

 FillChar(SHFileOpStruct, SizeOf(SHFileOpStruct), #0);

 { Get a valid window handle - use MainForm if available, otherwise use Application.Handle or 0 }
 if Assigned(Application.MainForm)
 then WndHandle := Application.MainForm.Handle
 else WndHandle := Application.Handle;  { Fallback to avoid nil access if MainForm not yet created }

 SHFileOpStruct.wnd              := WndHandle;
 SHFileOpStruct.wFunc            := FO_DELETE;
 { pFrom requires double-null termination. PChar adds one null; we add another.
   This format supports multiple files, each separated by a single null, with double-null at end. }
 SHFileOpStruct.pFrom            := PChar(ItemName + #0);
 SHFileOpStruct.pTo              := NIL;
 SHFileOpStruct.hNameMappings    := NIL;

 if DeleteToRecycle
 then SHFileOpStruct.fFlags:= SHFileOpStruct.fFlags OR FOF_ALLOWUNDO;

 if TotalSilence
 then SHFileOpStruct.fFlags:= SHFileOpStruct.fFlags OR FOF_NO_UI
 else
   if NOT ShowConfirm
   then SHFileOpStruct.fFlags:= SHFileOpStruct.fFlags OR FOF_NOCONFIRMATION;

 Result:= SHFileOperation(SHFileOpStruct) = 0;
end;


{ Validates that the path is usable with SHFileOperation.
  Returns False for:
    - Empty paths
    - Windows virtual folder names ('Control Panel', 'Recycle Bin')
    - Paths containing 'nethood' (Network Neighborhood shortcuts) }
function _validateForFileOperation(CONST sPath: string): Boolean;
begin
  Result:= (Length(sPath) > 0)
       AND (sPath <> 'Control Panel')
       AND (sPath <> 'Recycle Bin')
       AND (Pos('nethood', LowerCase(sPath)) = 0);
end;


{--------------------------------------------------------------------------------------------------
   Performs file operations (Copy, Move, Delete, Rename) using Windows Shell API (SHFileOperation).
   This provides the standard Windows behavior including progress dialogs and undo support.

   Parameters:
     Source - Source file or folder path. Cannot be empty or a virtual folder.
     Dest   - Destination path. Can be empty for FO_DELETE operations.
     Op     - Operation constant: FO_COPY, FO_DELETE, FO_MOVE, or FO_RENAME (from ShellAPI).
     Flags  - Operation flags like FOF_ALLOWUNDO, FOF_NOCONFIRMATION, etc.

   Returns: True if operation succeeded, False otherwise.

   Example: FileOperation('C:\Temp\file.txt', '', FO_DELETE, FOF_ALLOWUNDO)
--------------------------------------------------------------------------------------------------}
function FileOperation(CONST Source, Dest: string; Op, Flags: Integer): Boolean;
VAR
  SHFileOpStruct: TSHFileOpStruct;
  SourceBuf, DestBuf: string;
begin
  Result:= _validateForFileOperation(Source);
  if NOT Result then EXIT;

  FillChar(SHFileOpStruct, SizeOf(SHFileOpStruct), #0);

  { SHFileOperation requires double-null terminated strings.
    Adding #0 here, combined with PChar's implicit null terminator, creates the required format. }
  SourceBuf:= Source + #0;
  DestBuf:= Dest + #0;

  SHFileOpStruct.Wnd:= 0;
  SHFileOpStruct.wFunc:= Op;
  SHFileOpStruct.pFrom:= PChar(SourceBuf);
  SHFileOpStruct.pTo:= PChar(DestBuf);
  SHFileOpStruct.fFlags:= Flags;

  case Op of
    FO_COPY  : SHFileOpStruct.lpszProgressTitle:= 'Copying...';
    FO_DELETE: SHFileOpStruct.lpszProgressTitle:= 'Deleting...';
    FO_MOVE  : SHFileOpStruct.lpszProgressTitle:= 'Moving...';
    FO_RENAME: SHFileOpStruct.lpszProgressTitle:= 'Renaming...';
  end;

  Result:= SHFileOperation(SHFileOpStruct) = 0;
end;



{--------------------------------------------------------------------------------------------------
   Sets the NTFS compression attribute on a file or folder.

   Parameters:
     FileName - Full path to the file or folder. Must exist.
     CompressionFormat - Compression level:
       0 = COMPRESSION_FORMAT_NONE (disable compression)
       1 = COMPRESSION_FORMAT_DEFAULT (enable with default algorithm)
       2 = COMPRESSION_FORMAT_LZNT1 (enable with LZNT1 algorithm)

   Raises: Exception if file/folder doesn't exist, or EOSError if operation fails.

   Note: Only works on NTFS volumes. Has no effect on FAT/FAT32/exFAT.
--------------------------------------------------------------------------------------------------}
procedure SetCompressionAtr(const FileName: string; const CompressionFormat: byte = 1);
CONST
  FSCTL_SET_COMPRESSION = $9C040;
VAR
   Handle: THandle;
   Flags: DWORD;
   BytesReturned: DWORD;
begin
  if FileName = ''
  then raise Exception.Create('SetCompressionAtr: FileName parameter cannot be empty');

  if DirectoryExists(FileName)
  then Flags:= FILE_FLAG_BACKUP_SEMANTICS  { Required to open directories }
  else
    if FileExists(FileName)
    then Flags:= 0
    else raise Exception.CreateFmt('SetCompressionAtr: ''%s'' does not exist', [FileName]);

  Handle:= CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, Flags, 0);
  if Handle = INVALID_HANDLE_VALUE
  then RaiseLastOSError;

  TRY
    if not DeviceIoControl(Handle, FSCTL_SET_COMPRESSION, @CompressionFormat, SizeOf(CompressionFormat), nil, 0, BytesReturned, nil)
    then RaiseLastOSError;
  FINALLY
    CloseHandle(Handle);
  END;
end;






{--------------------------------------------------------------------------------------------------
   FILE AGE / LAST MODIFICATION TIME
--------------------------------------------------------------------------------------------------}
{--------------------------------------------------------------------------------------------------
   Returns the last modification time of a file.

   This function uses FindFirst/FindData instead of System.SysUtils.FileAge because
   the standard FileAge fails on system files like 'c:\pagefile.sys' that cannot be opened.
   FindFirst works because it reads file metadata without opening the file.

   Parameters:
     FileName - Full path to the file.

   Returns:
     TDateTime of the file's last modification time, or -1 if:
       - File doesn't exist
       - Access is denied
       - Date/time conversion fails
--------------------------------------------------------------------------------------------------}
function FileAge(CONST FileName: string): TDateTime;
VAR
  LocalFileTime: TFileTime;
  SystemTime: TSystemTime;
  SRec: TSearchRec;
begin
 Result:= -1;

 if FindFirst(FileName, faAnyFile, SRec) <> 0
 then EXIT;

 TRY
   TRY
     {$WARN SYMBOL_PLATFORM OFF}
     FileTimeToLocalFileTime(SRec.FindData.ftLastWriteTime, LocalFileTime);
     FileTimeToSystemTime(LocalFileTime, SystemTime);
     Result:= SystemTimeToDateTime(SystemTime);
   EXCEPT
     on E: EConvertError do Result:= -1;
   END;
 FINALLY
   FindClose(SRec);
 END;
end;


{ Converts a Windows FILETIME to a formatted date/time string.
  FTime is converted from UTC to local time before formatting.
  Example: FileTimeToDateTimeStr(FTime, 'yyyy-mm-dd', 'hh:nn:ss') returns '2024-01-15 14:30:45' }
function FileTimeToDateTimeStr(FTime: TFileTime; CONST DFormat, TFormat: string): string;
VAR
  SysTime: TSystemTime;
  LocalFileTime: TFileTime;
begin
  FileTimeToLocalFileTime(FTime, LocalFileTime);
  FileTimeToSystemTime(LocalFileTime, SysTime);
  Result:= FormatDateTime(DFormat + ' ' + TFormat, SystemTimeToDateTime(SysTime));
end;









{-------------------------------------------------------------------------------------------------------------
   PROMPT TO SAVE/LOAD FILE DIALOGS

   Convenience wrappers around standard Windows Open/Save dialogs.
   If FileName contains a path, that path is used as the initial directory.
   If FileName is a folder path, it's used directly as the initial directory.

   Note: Similar functions exist in TAppData that also remember the last used folder
         across application sessions.

   Parameters:
     FileName   - VAR: Input as initial filename/path, output as selected filename.
     Filter     - File type filter. Example: 'Text files|*.txt|All files|*.*'
     DefaultExt - Extension added if user doesn't specify one (without dot, max 3 chars).
     Title      - Dialog window title.

   Returns: True if user selected a file, False if cancelled.

   Example: PromptToSaveFile(s, 'JPEG Images|*.jpg;*.jpeg', 'jpg', 'Save Image')
-------------------------------------------------------------------------------------------------------------}
function PromptToSaveFile(VAR FileName: string; CONST Filter: string = ''; CONST DefaultExt: string= ''; CONST Title: string= ''): Boolean;
VAR InitialDir: string;
begin
 InitialDir:= '';
 if FileName <> '' then
   if IsFolder(FileName)
   then InitialDir:= FileName
   else InitialDir:= ExtractFilePath(FileName);

 Result:= PromptForFileName(FileName, TRUE, Filter, DefaultExt, Title, InitialDir);
end;


function PromptToLoadFile(VAR FileName: string; CONST Filter: string = ''; CONST Title: string= ''): Boolean;
VAR InitialDir: string;
begin
 InitialDir:= '';
 if FileName <> '' then
   if IsFolder(FileName)
   then InitialDir:= FileName
   else InitialDir:= ExtractFilePath(FileName);

 Result:= PromptForFileName(FileName, FALSE, Filter, '', Title, InitialDir);
end;


{ Core implementation for Open/Save file dialogs.
  Based on Vcl.Dialogs.PromptForFileName but with added options (ofEnableSizing, ofForceShowHidden).
  Does not support multi-select because it returns a single filename. }
function PromptForFileName(VAR FileName: string; SaveDialog: Boolean; CONST Filter: string = ''; CONST DefaultExt: string= ''; CONST Title: string= ''; CONST InitialDir: string = ''): Boolean;
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

   Example: PromptToSaveFile(s, LightCore.FilterTxt, 'txt')
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


{ Example: SaveDialog(LightCore.FilterTxt, 'csv');  }
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
   FILE LOCKING TESTS

   These functions test whether a file can be opened with specific access modes.
   They work by attempting to open the file with exclusive access (share mode = 0).
--------------------------------------------------------------------------------------------------}

{ Tests if a file is locked for read+write access.
  Returns True if the file cannot be opened with GENERIC_READ + GENERIC_WRITE.
  Returns False if: file doesn't exist, or file can be opened exclusively.
  Use case: Check before attempting to modify a file that might be in use. }
function FileIsLockedRW(CONST FileName: string): Boolean;
VAR hFileRes: HFILE;
begin
 if NOT FileExists(FileName) then EXIT(FALSE);  { Non-existent files aren't "locked" }

 hFileRes:= CreateFile(PChar(FileName), GENERIC_READ OR GENERIC_WRITE, 0, NIL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
 Result:= (hFileRes = INVALID_HANDLE_VALUE);
 if NOT Result then CloseHandle(hFileRes);
end;


{ Tests if a file is locked for read access.
  Returns True if the file cannot be opened with GENERIC_READ.
  Raises Exception if the file doesn't exist.
  Use case: Check if a file can be read (e.g., before backup operations). }
function FileIsLockedR(CONST FileName: string): Boolean;
VAR hFileRes: HFILE;
begin
 if NOT FileExists(FileName)
 then raise Exception.Create('FileIsLockedR: File does not exist!' + CRLFw + FileName);

 hFileRes:= CreateFile(PChar(FileName), GENERIC_READ, 0, NIL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
 Result:= (hFileRes = INVALID_HANDLE_VALUE);
 if NOT Result then CloseHandle(hFileRes);
end;


function CanCreateFile(const FileName: String): Boolean;
begin
  Result := CanWriteToFolder(ExtractFilePath(FileName), ExtractFileName(FileName));
end;



{--------------------------------------------------------------------------------------------------
   FOLDER WRITE ACCESS TESTS
--------------------------------------------------------------------------------------------------}

{ Builds a user-friendly error message explaining why write access may have failed.
  Lists common causes (permissions, locks, read-only) and suggested solutions. }
function ShowMsg_CannotWriteTo(CONST sPath: string): string;
begin
 Result:= 'Cannot write to "' + sPath + '"'
           + LBRK + 'Possible causes:'
           + CRLF + ' * the file/folder is read-only'
           + CRLF + ' * the file/folder is locked by another program'
           + CRLF + ' * you don''t have necessary privileges to write there'
           + CRLF + ' * the drive is not ready'

           + LBRK + 'You can try to:'
           + CRLF + ' * use a different folder'
           + CRLF + ' * change the privileges (or contact the admin to do it)'
           + CRLF + ' * run the program with elevated rights (as administrator)';
end;


{ Tests folder write access and shows a warning dialog if access is denied. }
function CanWriteToFolderMsg(CONST Folder: string): Boolean;
begin
 Result:= CanWriteToFolder(Folder);
 if NOT Result
 then MessageWarning(ShowMsg_CannotWriteTo(Folder));
end;


{--------------------------------------------------------------------------------------------------
   Tests if the application can write files to a folder.

   Works by attempting to create a temporary file with FILE_FLAG_DELETE_ON_CLOSE,
   which is automatically deleted when the handle is closed.

   Parameters:
     Folder   - The folder path to test. Trailing backslash is added automatically.
     FileName - Name for the temporary test file. Default is 'TempFile.Delete.Me'.
                WARNING: If a file with this name exists, it will be overwritten!

   Returns: True if file creation succeeded (folder is writable), False otherwise.
--------------------------------------------------------------------------------------------------}
function CanWriteToFolder(CONST Folder: string; const FileName: String = 'TempFile.Delete.Me'): Boolean;
VAR
  Handle: THandle;
begin
  Handle:= Winapi.Windows.CreateFile(
             PChar(Trail(Folder) + FileName),
             GENERIC_READ or GENERIC_WRITE,
             0,                                           { Exclusive access }
             nil,
             CREATE_ALWAYS,                               { Overwrite if exists }
             FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE,
             0);

  Result:= Handle <> INVALID_HANDLE_VALUE;
  if Result
  then Winapi.Windows.CloseHandle(Handle);
end;







{--------------------------------------------------------------------------------------------------
   FILE MOVE OPERATIONS
   Using Windows API MoveFileEx for reliable cross-volume moves with overwrite support.
--------------------------------------------------------------------------------------------------}

{ Moves a file to a new full path, overwriting the destination if it exists.
  Uses MoveFileEx with MOVEFILE_REPLACE_EXISTING flag.
  Returns True if successful, False otherwise. }
function FileMoveTo(CONST From_FullPath, To_FullPath: string): Boolean;
begin
 if From_FullPath = ''
 then raise Exception.Create('FileMoveTo: From_FullPath parameter cannot be empty');

 if To_FullPath = ''
 then raise Exception.Create('FileMoveTo: To_FullPath parameter cannot be empty');

 Result:= MoveFileEx(PChar(From_FullPath), PChar(To_FullPath), MOVEFILE_REPLACE_EXISTING);
end;


{ Moves a file to a destination folder, keeping the original filename.
  Creates the destination folder if it doesn't exist.

  Parameters:
    From_FullPath  - Full path to the source file.
    To_DestFolder  - Destination folder (not full path). Trailing slash optional.
    Overwrite      - If True, overwrites existing file at destination.

  Returns: True if successful, False otherwise.
  Raises Exception if parameters are empty or folder cannot be created. }
function FileMoveToDir(CONST From_FullPath, To_DestFolder: string; Overwrite: Boolean): Boolean;
VAR
  Flags: Cardinal;
begin
 if From_FullPath = ''
 then raise Exception.Create('FileMoveToDir: From_FullPath parameter cannot be empty');

 if To_DestFolder = ''
 then raise Exception.Create('FileMoveToDir: To_DestFolder parameter cannot be empty');

 if Overwrite
 then Flags:= MOVEFILE_REPLACE_EXISTING
 else Flags:= 0;

 if NOT System.SysUtils.ForceDirectories(To_DestFolder)
 then raise Exception.Create('FileMoveToDir: Cannot create destination folder: ' + To_DestFolder);

 Result:= MoveFileEx(PChar(From_FullPath), PChar(Trail(To_DestFolder) + ExtractFileName(From_FullPath)), Flags);
end;





{--------------------------------------------------------------------------------------------------
   DRIVE INFORMATION
   Functions for querying drive types, validity, and free space.
--------------------------------------------------------------------------------------------------}

{ Returns the drive type constant for a given path.
  Path can be a drive letter with backslash ('C:\') or UNC path ('\\server\share\').
  Returns: DRIVE_UNKNOWN, DRIVE_NO_ROOT_DIR, DRIVE_REMOVABLE, DRIVE_FIXED,
           DRIVE_REMOTE, DRIVE_CDROM, or DRIVE_RAMDISK. }
function GetDriveType(CONST Path: string): Integer;
begin
 Result:= Winapi.Windows.GetDriveType(PChar(Trail(Path)));
end;


{ Returns a human-readable description of the drive type. }
function GetDriveTypeS(CONST Path: string): string;
begin
 case GetDriveType(Path) of
   DRIVE_UNKNOWN    : Result:= 'The drive type cannot be determined.';
   DRIVE_NO_ROOT_DIR: Result:= 'The root path is invalid';
   DRIVE_REMOVABLE  : Result:= 'Drive Removable';
   DRIVE_FIXED      : Result:= 'Drive fixed';
   DRIVE_REMOTE     : Result:= 'Remote Drive';
   DRIVE_CDROM      : Result:= 'CD ROM Drive';
   DRIVE_RAMDISK    : Result:= 'RAM Drive';
 end;
end;


{ Checks if a drive letter represents a valid, accessible drive.
  WARNING: This function can be VERY SLOW if the drive exists but has no media
  (e.g., empty CD-ROM drive or disconnected USB). The GUI may freeze during the check.
  Uses SetErrorMode to suppress Windows error dialogs for missing media. }
function ValidDrive(CONST Drive: Char): Boolean;
VAR
  Mask: string;
  SRec: TSearchRec;
  OldMode: Cardinal;
  RetCode: Integer;
begin
 OldMode:= SetErrorMode(SEM_FAILCRITICALERRORS);
 TRY
   Mask:= Drive + ':\*.*';
   {$I-}
   RetCode:= FindFirst(Mask, faAnyfile, SRec);
   if RetCode = 0
   then FindClose(SRec);
   {$I+}
   Result:= Abs(RetCode) in [ERROR_SUCCESS, ERROR_FILE_NOT_FOUND, ERROR_NO_MORE_FILES];
 FINALLY
   SetErrorMode(OldMode);
 END;
end;






{ Returns the volume label of a drive, formatted as '[LabelName]'.
  If drive letter is uppercase (A-Z), label is returned uppercase.
  If drive letter is lowercase (a-z), label is returned lowercase.
  Returns '[]' if volume has no label or drive is inaccessible. }
function GetVolumeLabel(CONST Drive: Char): string;
VAR
  OldErrorMode: Integer;
  NotUsed, VolFlags: DWORD;
  Buf: array[0..MAX_PATH] of Char;
begin
  Result:= '';
  OldErrorMode:= SetErrorMode(SEM_FAILCRITICALERRORS);
  TRY
    Buf[0]:= #0;
    if GetVolumeInformation(PChar(Drive + ':\'), Buf, DWORD(SizeOf(Buf)), nil, NotUsed, VolFlags, nil, 0)
    then SetString(Result, Buf, StrLen(Buf))
    else Result:= '';

    if Drive < 'a'
    then Result:= AnsiUpperCase(Result)
    else Result:= AnsiLowerCase(Result);

    Result:= Format('[%s]', [Result]);
  FINALLY
    SetErrorMode(OldErrorMode);
  END;
end;


{ Checks if a disk/media is present in the specified drive.
  For remote/network drives, always returns True (network connectivity not verified).
  For local drives, delegates to the Byte overload which uses DiskSize.
  WARNING: Can be slow for removable drives without media! }
function DiskInDrive(CONST Path: string): Boolean;
VAR
  DriveNumber: Byte;
  DriveType: Integer;
begin
  DriveType:= GetDriveType(Path);

  if DriveType < DRIVE_REMOVABLE
  then Result:= FALSE  { Unknown drive or no root directory }
  else
    if DriveType = DRIVE_REMOTE
    then Result:= TRUE  { Assume network drives are available; TODO: verify connectivity }
    else
      begin
        DriveNumber:= Drive2Byte(Path[1]);
        Result:= DiskInDrive(DriveNumber);
      end;
end;


{ Checks if a disk is present in the drive specified by drive number (1=A, 2=B, 3=C, etc.).
  WARNING: This can be VERY SLOW if the drive has no media (empty CD-ROM, disconnected USB).
  The GUI may freeze until the drive responds or times out. }
function DiskInDrive(CONST DriveNo: Byte): Boolean;
VAR ErrorMode  : Word;
begin
  Result:= FALSE;
  ErrorMode:= SetErrorMode(SEM_FAILCRITICALERRORS);
  TRY
    if DiskSize(DriveNo) <> -1
    then Result:= TRUE;
  FINALLY
    SetErrorMode(ErrorMode);
  END;
end;





{ Returns free space on a drive in bytes. Returns 0 if drive is invalid or has no media. }
function DriveFreeSpace(CONST Drive: Char): Int64;
VAR DriveNo: Byte;
begin
 DriveNo:= Drive2Byte(Drive);

 if ValidDrive(Drive) 
 AND DiskInDrive(DriveNo)
 then Result:= DiskFree(DriveNo)
 else Result:= 0;
end;


{ Returns free space on a drive as a formatted string (e.g., '15.3 GB'). }
function DriveFreeSpaceS(CONST Drive: Char): string;
begin
 Result:= FormatBytes(DriveFreeSpace(Drive), 1);
end;


{ Returns free space for the drive containing the specified path.
  Extracts the drive letter from a full path (file or directory) automatically.
  Example: DriveFreeSpaceF('C:\Windows\System32\file.txt') returns free space on C: }
function DriveFreeSpaceF(CONST FullPath: string): Int64;
begin
 Result:= DriveFreeSpace(System.IOUtils.TDirectory.GetDirectoryRoot(FullPath)[1]);
end;


{--------------------------------------------------------------------------------------------------
   Determines the starting index of the actual path after any Windows extended path prefix.

   Windows extended path prefixes:
     '\\?\'     - Extended prefix for local paths (bypasses MAX_PATH limit)
     '\\?\UNC\' - Extended prefix for UNC/network paths

   Returns:
     1 for paths without prefix         ('C:\Folder\file.txt' -> 1)
     5 for extended local paths         ('\\?\C:\Folder\file.txt' -> 5, points to 'C:\...')
     9 for extended UNC paths           ('\\?\UNC\Server\Share' -> 9, points to 'Server\...')

   Used by PathHasValidColon to correctly validate paths with extended prefixes.
--------------------------------------------------------------------------------------------------}
function GetPosAfterExtendedPrefix(const Path: string): Integer;
CONST
  ExtendedPrefix: string = '\\?\';
  ExtendedUNCPrefix: string = '\\?\UNC\';
VAR
  Prefix: TPathPrefixType;
begin
  Prefix:= TPath.GetExtendedPrefix(Path);
  case Prefix of
    TPathPrefixType.pptNoPrefix    : Result:= 1;
    TPathPrefixType.pptExtended    : Result:= Length(ExtendedPrefix) + 1;
    TPathPrefixType.pptExtendedUNC : Result:= Length(ExtendedUNCPrefix) + 1;
  else
    Result:= 1;
  end;
end;



end.