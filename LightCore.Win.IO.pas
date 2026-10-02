UNIT LightCore.Win.IO;

{$IFNDEF MSWINDOWS}
  {$MESSAGE FATAL 'LightCore.Win.IO is Windows-only. Its package LightCore.Win builds for Win32 and Win64 only.'}
{$ENDIF}

{=============================================================================================================
   2026.10.01
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   Windows special folders, drives and NTFS compression

   Provides:
     - The Windows folder, the System folder, Program Files, the Desktop and the Start Menu
     - Any special folder, named by its CSIDL constant (Shell API) or by its value name under the registry key Shell Folders
     - The list of all the special folders, and a test for whether a path is one of them
     - The full path of the Task Manager, taskmgr.exe
     - Drives, named by their letter: the type, the volume label, whether the drive is valid and holds a disk, the free space
     - The NTFS compression attribute of a file or folder

   See also:
     LightCore.IO.pas             - platform-neutral file and folder routines, among them GetMyDocuments and GetMyPictures
     LightVcl.Common.IO.pas       - file and folder routines of a VCL application, the file dialogs, and the routines that show an error message

   Windows-only unit, package LightCore.Win. Every routine asks Windows for a path, a drive letter or an NTFS attribute: through GetWindowsDirectory, GetSystemDirectory, SHGetFolderPath with a CSIDL constant, the Windows registry, GetDriveType, GetVolumeInformation or DeviceIoControl. None of these exists off Windows, so a build for Android, macOS or iOS stops at the top of this unit with a fatal compiler message.
=============================================================================================================}

INTERFACE
USES
   Winapi.Windows, Winapi.ShlObj, System.SysUtils, System.Win.Registry, System.Classes;


{--------------------------------------------------------------------------------------------------
   SPECIAL FOLDERS
--------------------------------------------------------------------------------------------------}
 function  GetProgramFilesDir    : string;
 function  GetDesktopFolder      : string;
 function  GetStartMenuFolder    : string;
 function  GetWinSysDir          : string;
 function  GetWinDir             : string; { Returns Windows folder }
 function  GetTaskManager        : string;

 function  GetSpecialFolder (CONST OS_SpecialFolder: string): string;                 overload;          { SHELL FOLDERS.  Retrieving the entire list of default shell folders from registry }
 function  GetSpecialFolder (CSIDL: Integer; ForceFolder: Boolean = FALSE): string;   overload;          { uses SHFolder }
 function  GetSpecialFolders: TStringList;                                                               { Get a list of ALL special folders. }

 function  FolderIsSpecial  (CONST Path: string): Boolean;                                               { Returns True if the parameter is a special folder such us 'c:\My Documents' }


{--------------------------------------------------------------------------------------------------
   API OPERATIONS
--------------------------------------------------------------------------------------------------}
 procedure SetCompressionAtr    (CONST FileName: string; const CompressionFormat: byte= 1);


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

 { Free space }
 function  DriveFreeSpace     (CONST Drive: Char): Int64;
 function  DriveFreeSpaceS    (CONST Drive: Char): string;
 function  DriveFreeSpaceF    (CONST FullPath: string): Int64;                        { Same as DriveFreeSpace but this accepts a full filename/directory path. It will automatically extract the drive }



IMPLEMENTATION

USES
   Winapi.ActiveX, System.IOUtils, LightCore, LightCore.Win.Registry, LightCore.IO;


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
  Res: HRESULT;
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

  Buffer[0]:= #0;   { ShGetFolderPath leaves the buffer undefined on failure - StrPas on stack garbage could over-read }

  if ForceFolder
  then Res:= ShGetFolderPath(0, CSIDL or CSIDL_FLAG_CREATE, 0, 0, Buffer)
  else Res:= ShGetFolderPath(0, CSIDL, 0, 0, Buffer);

  if Failed(Res) then EXIT('');   { E.g. CSIDL not available on this system (some entries of GetSpecialFolders) }

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
 TRY
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
 EXCEPT
   FreeAndNil(Result);   { Don't leak the list if an Add raises before the caller receives it }
   RAISE;
 END;
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
    if GetVolumeInformation(PChar(Drive + ':\'), Buf, DWORD(Length(Buf)), nil, NotUsed, VolFlags, nil, 0)   { Buffer size is in TCHARs, not bytes }
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



end.
