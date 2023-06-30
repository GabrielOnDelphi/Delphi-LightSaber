UNIT ccIO;

{=============================================================================================================
   CubicDesign
   2022-04-03
   See Copyright.txt

   Super useful functions for file/folder/disk manipulation:
     - Copy files
     - File/Folder exists
     - Get special Windows folders (My Documents, etc)
     - Prompt user to select a file/folder
     - List specified files (*.jpg for ex) in a folder and all its sub-folders
     - Increment the numbers in a filename (good for incremental backups)
     - Append strings to file name
     - Read text from files to a string variable
     - Compare files
     - Merge files
     - Sort lines in a file
     - Drive manipulation (IsDiskInDrive, etc)
     - etc
==============================================================================================================

  EXISTS:
    procedure ProcessPath (FullFileName, Drive, DirPart, FilePart)    // Parses a file name into its constituent parts.
    procedure CutFirstDirectory(VAR S: TFileName)
    procedure FileGetSymLinkTarget                                    // Reads the contents of a symbolic link. The result is returned in the symbolic link record given by SymLinkRec.

  ------------------------------------------------
  Maximum Path Length Limitation
    In the Windows API (with some exceptions), the maximum length for a path is MAX_PATH,
    which is defined as 260 characters. A local path is structured in the following order:
    drive letter, colon, backslash, name components separated by backslashes, and a terminating null character.
    Example: "D:\some 256-character-path-string<NUL>" -> 256

  Using long paths
    The Windows API has many functions that also have Unicode versions to permit an extended-length path
    for a maximum total path length of 32,767 characters.
    In order to name a path with a long name you need to use the magic \\?\ prefix, and use the Unicode version of the API.
    For example, "\\?\D:\very long path".

  Relative paths
    Relative paths are always limited to a total of MAX_PATH characters.

  Enable Long Paths in Win10
    Starting in Windows 10.1607, MAX_PATH limitations have been removed from common Win32 file and directory functions.
    However, you must opt-in to the new behavior.

  From Microsoft documentation:
    https://stackoverflow.com/questions/6996711/how-to-create-directories-in-windows-with-path-length-greater-than-256/59641690#59641690

  IOUtils
    TFile.FCMinFileNameLen = 12.
    There is a problem in IOUtils. It cannot be used in conjunction with Max_Path.
    It uses InternalCheckDirPathParam all over the place!
    So, instead of using MAX_PATH we use MAXPATH declared below

    Details: https://stackoverflow.com/questions/44141996/tdirectory-getdirectoryroot-does-not-accept-paths-of-max-path-characters
  ------------------------------------------------

  SEE THIS FOR Mac OSX
     http://www.malcolmgroves.com/blog/?p=865

  TESTER:
     c:\MyProjects\Packages\CubicCommonControls\UNC Tester\
==================================================================================================}


INTERFACE
{$WARN UNIT_PLATFORM OFF}   {Silence the 'W1005 Unit Vcl.FileCtrl is specific to a platform' warning }

USES
  winapi.Windows, Winapi.ShellAPI, Winapi.ShlObj, System.Win.Registry, System.Masks, System.Types, Vcl.Consts,
  System.StrUtils, System.IOUtils, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Dialogs, Vcl.Forms, Vcl.FileCtrl;

CONST
  DigSubdirectories = TRUE;
  UseFullPath       = TRUE;
  InvalidFileCharWin= ['/','*','?','<','>','|', '"', ':', '\'];     { Characters that are invalid for file names OR for folder (just single folders) }
  InvalidPathChar   = ['/','*','?','<','>','|', '"'];               { Characters that are invalid for a FULL path }
  {$IFDEF MSWINDOWS}
    MAXPATH= MAX_PATH- 12;                                          { Check like this:  if Path < MAXPATH then... }
  {$ELSE}
    MAXPATH= MAX_PATH;
  {$ENDIF}

   { FILTERS }
   FilterTxt     = 'TXT file|*.TXT';
   FilterRtf     = 'RTF file|*.RTF';
   {}
   fltCsv        = '*.csv';
   FilterCsv     = 'CSV file|'+ fltCsv;
   {}
   FilterAllFiles= 'All files|*.*';                                 { See also Vcl.Consts.SDefaultFilter }
   {}
   fltIni        = '*.ini';
   FilterIni     = 'INI file|'+ fltIni;
   FilterTransl  = 'Translation file|'+ fltIni;
   {}
   FilterHtm     = 'HTM|*.htm';
   FilterHtml    = 'HTML|*.html';
   {}
   ctFltSounds   = 'Sound (Wav, mp3, midi, wma)|*.wav;*.mp3;*.mi*;*.au;*.wma';
   ctFltIcons    = 'Icons (*.ico)|*.ico';
   {}
   FilterApps    = 'Applications|*.exe';
   Executables   = '*.exe;*.cmd;*.bat;*.dll;*.ocx';




{--------------------------------------------------------------------------------------------------
   MULTIPLATFORM / LINUX PATHS
--------------------------------------------------------------------------------------------------}
 { Trail }
 function  TrailLinuxPathEx         (CONST Path: string): string;  { Adds / in front and at the end of the path }
 function  TrailLinuxPath           (CONST Path: string): string;  { Adds / at the end of the path }     // old name: URLTrail
 function  TrimLastLinuxSeparator   (CONST Path: string): string;

 { CONVERSION }
 function  Convert2LinuxPath        (DosPath  : string): string;
 function  Convert2DosPath          (LinuxPath: string): string;



{--------------------------------------------------------------------------------------------------
   EXISTS
--------------------------------------------------------------------------------------------------}
 function  IsFolder             (CONST FullPath: string): boolean;                                 { Tells if FullPath is a folder or file. THE FOLDER/FILE MUST EXISTS!!! }
 function  DirectoryExists      (CONST Directory: String; FollowLink: Boolean= TRUE): Boolean;     { Corectez un bug in functia 'DirectoryExists' care imi intoarce true pentru un folder care nu exista, de exemplu 'c:\Program Files\ '. Bug-ul apare cand calea se termina cu un caracter SPACE. }
 function  DirectoryExistMsg    (CONST Path: string): Boolean;
 function  FileExistsMsg        (CONST FileName: string): Boolean;


{--------------------------------------------------------------------------------------------------
   VALIDITY
--------------------------------------------------------------------------------------------------}
 function  FileNameIsValid_     (CONST FileName: string): Boolean; deprecated 'Use System.IOUtils.TPath.HasValidFileNameChars instead.'
 function  PathNameIsValid      (CONST Path: string): Boolean;    // TPath.HasValidPathChars is bugged     { Returns FALSE if the path contains invalid characters. Tells nothing about the existence of the folder }
 function  IsUnicode            (CONST Path: string): boolean;                                     { returns True if this path seems to be UNICODE }
 function  PathHasValidColon    (const Path: string): Boolean;


{--------------------------------------------------------------------------------------------------
   PROCESS PATH
--------------------------------------------------------------------------------------------------}
 function  ExtractLastFolder    (FullPath: string): string;                                        { exemplu pentru c:\windows\system intoarce doar 'system' }
 function  ExtractParentFolder  (Folder: string): string;
 function  ExtractFirstFolder   (Folder: string): string;                                          { For c:\1\2\3\ returns 1\.  From c:\1 it returns ''  }

 function  TrimLastFolder       (CONST DirPath: string): string;                                   { exemplu pentru c:\windows\system intoarce doar 'c:\windows\' }
 function  ExtractRelativePath_ (CONST FullPath, RelativeTo: string): string; deprecated 'Use System.SysUtils.ExtractRelativePath instead'                     { Returns truncated path, relative to 'RelativeTo'. Example:  ExtractRelativePath('c:\windows\system32\user32.dll', 'c:\windows') returns system32\user32.dll }

 function  ShortenFileName      (CONST FullPath: String; MaxLength: Integer= MAXPATH): string;     { Returns a valid path, but with shorter filename }
 function  CheckPathLength      (CONST FullPath: string; MaxLength: Integer= MAXPATH): Boolean;

 { Path delimiters }
 function  ForcePathDelimiters  (CONST Path, Delimiter: string; SetAtBegining, SetAtEnd: Boolean): string;  { Old name: UniversalPathDelimiters }
 function  Trail                (CONST Path: string): string;                                      { inlocuitor pt includeTrailingPathDelimiter }

 function  SameFolder(Path1, Path2: string): Boolean;                                              { Receives two folders. Ex:  C:\Test1\ and C:\teSt1 will return true }
 function  SameFolderFromFile(Path1, Path2: string): Boolean;                                      { Receives two partial or complete file names and compare their folders. Ex:  C:\Test1 and C:\teSt1\me.txt will return true }



{--------------------------------------------------------------------------------------------------
   CREATE FOLDERS
--------------------------------------------------------------------------------------------------}
 function  ForceDirectoriesB    (FullPath: string): Boolean;                                       { inlocuitor pt System.SysUtils.ForceDirectories - elimina problema: " { Do not call ForceDirectories with an empty string. Doing so causes ForceDirectories to raise an exception" }
 function  ForceDirectories     (CONST FullPath: string): Integer;
 function  ForceDirectoriesMsg  (CONST FullPath: string): Boolean;                                 { RETURNS:  -1 = Error creating the directory.   0 = Directory already exists.  +1 = Directory created succesfully }



{--------------------------------------------------------------------------------------------------
   FIX PATH
--------------------------------------------------------------------------------------------------}//del function  CorrectPath          (CONST FullPath: string; ReplaceWith: char): string;               { Old name:  RemoveInvalidPathChars  }
 function  CorrectFolder        (CONST Folder  : string; ReplaceWith: char= ' '): string;          { Folder is single folder. Example '\test\' }
 function  CorrectFilename      (CONST FileName: string; ReplaceWith: char= ' '): string;          { Correct invalid characters in a filename. FileName = File name without path }
 function  ShortenText          (CONST LongPath: String; MaxChars: Integer): String;               { Also exists: FileCtrl.MinimizeName, DrawStringEllipsis }


{--------------------------------------------------------------------------------------------------
   SPECIAL FOLDERS
--------------------------------------------------------------------------------------------------}
 function  GetTempFolder         : string;
 function  GetWinDir             : string;                                                         { Intoarce calea directorului Windows }
 function  GetProgramFilesDir    : string;
 function  GetWinSysDir          : string;                                                         { Intoarce calea directorului System/System32 }
 function  GetTaskManager        : String;
 function  GetMyDocuments        : string;  {See this for macosx: http://www.malcolmgroves.com/blog/?p=865 }
 function  GetMyPictures         : string;
 function  GetDesktopFolder      : string;
 function  GetStartMenuFolder    : string;

 { New: }
 function  GetHomePath: string;
 function  GetDocumentsPath: string;
 function  GetSharedDocumentsPath: string;
 function  GetMusicPath: string;
 function  GetMoviesPath: string;
 function  GetDownloadsPath: string;

 function  GetMyDocumentsAPI     : string; deprecated 'Use GetMyDocuments instead';
 function  GetMyPicturesAPI      : string; deprecated 'Use GetMyPictures  instead';

 function  GetSpecialFolder    (OS_SpecialFolder: string): string;                       overload;          { SHELL FOLDERS.  Retrieving the entire list of default shell folders from registry }
 function  GetSpecialFolder    (CSIDL: Integer; ForceFolder: Boolean = FALSE): string;   overload;          { uses SHFolder }
 function  GetSpecialFolders: TStringList;                                                         { Get a list of ALL special folders. }
 function  FolderIsSpecial     (const Path: string): Boolean;                                      { Returns True if the parameter is a special folder such us 'c:\My Documents' }


{--------------------------------------------------------------------------------------------------
   OPEN/SAVE dialogs
--------------------------------------------------------------------------------------------------}
 function SelectAFolder    (VAR Folder: string; CONST Title: string = ''; CONST Options: TFileDialogOptions= [fdoPickFolders, fdoForceFileSystem, fdoPathMustExist, fdoDefaultNoMiniMode]): Boolean; overload;

 function PromptToSaveFile (VAR FileName: string; Filter: string = ''; DefaultExt: string= ''; Title: string= ''): Boolean;
 function PromptToLoadFile (VAR FileName: string; Filter: string = '';                         Title: string= ''): Boolean;

 function PromptForFileName(VAR FileName: string; SaveDialog: Boolean; Filter: string = ''; DefaultExt: string= ''; Title: string= ''; InitialDir: string = ''): Boolean;

 function GetSaveDialog    (FileName, Filter, DefaultExt: string; Caption: string= ''): TSaveDialog;
 function GetOpenDialog    (FileName, Filter, DefaultExt: string; Caption: string= ''): TOpenDialog;


{--------------------------------------------------------------------------------------------------
   LIST FOLDER CONTENT
--------------------------------------------------------------------------------------------------}
 function  CountFilesInFolder  (CONST Path: string; CONST SearchSubFolders, CountHidden: Boolean): Cardinal;
 function  FindFirstFile       (CONST aFolder, Ext: string): string;                               { Find first file in the specified folder }
 function  ListDirectoriesOf   (CONST aFolder: string; CONST ReturnFullPath, DigSubdirectories: Boolean): TStringList;   { if DigSubdirectories is false, it will return only the top level directories, else it will return also the subdirectories of subdirectories. Returned folders are FullPath. Works also with Hidden/System folders }
 function  ListFilesAndFolderOf(CONST aFolder: string; CONST ReturnFullPath: Boolean): TStringList;
 function  ListFilesOf         (CONST aFolder, FileType: string; CONST ReturnFullPath, DigSubdirectories: Boolean): TStringList;
 function  FolderIsEmpty       (CONST FolderName: string): Boolean;                               { Check if folder is empty }


{--------------------------------------------------------------------------------------------------
   FILE AUTO-NAME
--------------------------------------------------------------------------------------------------}
 function  IncrementFileNameEx  (CONST FileName: string; StartAt, NumberLength: Integer): string;  { Same sa IncrementFileName but it automatically adds a number if the file doesn't already ends with a number }
 function  IncrementFileName    (CONST FileName: string): string;                                  { Receives a file name that ends in a number. returns the same filename plus the number incremented with one. }
 function  MakeUniqueFolderName (CONST RootPath, FolderName: string): string;                      { Returns a unique path ended with a number. Old name:  Getnewfoldername }
 function  ChangeFilePath       (CONST FullFileName, NewPath: string): string;                     { change file path to NewPath }
 function  AppendNumber2Filename(CONST FileName: string; StartAt, NumberLength: Integer): string;  { Add the number at the end of the filename. Example: AppendNumber2Filename('Log.txt', 1) will output 'Log1.txt' }
 function  FileEndsInNumber     (CONST FileName: string): Boolean;                                 { Returns true is the filename ends with a number. Example: MyFile02.txt returns TRUE }
 // function GetUniqueFileName: string;      //replaced with      ccCore.GenerateUniqueString


{--------------------------------------------------------------------------------------------------
   CHANGE FILENAME
--------------------------------------------------------------------------------------------------}
 function  AppendToFileName    (CONST FileName, ApendedText: string): string;                      { add a string between the name and the extension. Example: if I add the string ' backup' to the file 'Shell32.DLL' -> 'Shell32 backup.DLL' }
 function  AppendBeforeName    (CONST FullPath, ApendedText: string): string;                      { add a string between the last directory separator and the beginning of the file name. Example: if I add the string ' backup' to the file 'c:\Shell32.DLL' -> 'c:\backup Shell32.DLL' -> 'c:\backup Shell32.DLL' -> 'c:\backup Shell32.DLL'. }
 function  ReplaceOnlyName     (CONST FileName, newName: string): string;                          { replaces ONLY the name of a file (no extension) in a full path. Example: C:\Name.dll -> C:\NewName.dll }


{--------------------------------------------------------------------------------------------------
   EXTRACT FILENAME
--------------------------------------------------------------------------------------------------}
 function  RemoveDrive         (CONST FullPath: string): string;                                   { C:\MyDocuments\1.doc  ->  MyDocuments\1.doc }
 function  ExtractDrive        (CONST FullPath: string): string;                                   { C:\MyDocuments\1.doc  ->  C }
 function  ExtractFilePath     (CONST FullPath: string; AcceptInvalidPaths: Boolean= TRUE): string;{ Same as the old ExtractFilePath but uses the new GetDirectoryName function instead  }
 function  ExtractOnlyName     (CONST FileName: string): string;                                   { Extracts the name of a file. If the name is of type name+extension+extension, only the last extension will be removed. Use the new IOUtils library }


{--------------------------------------------------------------------------------------------------
   FILE EXTENSION
--------------------------------------------------------------------------------------------------}
 function  RemoveLastExtension (CONST FileName: string): string;                                   { Extrage numele fisierului din nume+extensie. Daca numele este de tipul nume+extensie+extensie, doar ultima extensie este eliminata }
 function  ForceExtension      (CONST FileName, Ext: string): string;                              { makes sure that the 'FileName' file has the extension set to 'Ext'. The 'Ext' parameter should be like: '.txt' }
 function  ExtractFileExtUp    (CONST FileName: string): string;                                   { Case insensitive version }


{--------------------------------------------------------------------------------------------------
   FILE - DETECT FILE TYPE
--------------------------------------------------------------------------------------------------}
 function IsVideo       (CONST AGraphFile: string): Boolean;            { Video files supported by FFVCL (cFrameServerAVI) }
 function IsVideoGeneric(CONST AGraphFile: string) : Boolean;           { Generic video file detection. It doesn't mean I have support for all those files in my app }
 function IsGIF         (CONST AGraphFile: string) : Boolean;

 function IsJpg   (CONST AGraphFile: string) : Boolean;
 function IsJp2   (CONST AGraphFile: string) : Boolean;
 function IsBMP   (CONST AGraphFile: string) : Boolean;
 function IsWB1   (CONST AGraphFile: string) : Boolean;
 function IsWBC   (CONST AGraphFile: string) : Boolean;
 function IsPNG   (CONST AGraphFile: string) : Boolean;
 function IsICO   (CONST AGraphFile: string) : Boolean;
 function IsEMF   (CONST AGraphFile: string) : Boolean;
 function IsWMF   (CONST AGraphFile: string) : Boolean;
 function IsImage (CONST AGraphFile: string) : Boolean;          { returns TRUE if the file has a good/known extension and it can be converted to BMP }
 function IsImage2Bmp(CONST AGraphFile: string) : Boolean;

 function IsDfm   (CONST FileName  : string) : Boolean;
 function IsPas   (CONST FileName  : string) : Boolean;
 function IsDpr   (CONST FileName  : string) : Boolean;
 function IsDpk   (CONST FileName  : string) : Boolean;
 function IsDelphi(CONST FileName  : string) : Boolean;
 function IsExec  (CONST FileName  : string) : Boolean;


{--------------------------------------------------------------------------------------------------
   FILE TEXT
--------------------------------------------------------------------------------------------------}
 TYPE
   TWriteOperation= (woAppend, woOverwrite);

 function  StringFromFileTSL    (CONST FileName: string; Enc: TEncoding= NIL): TStringList;        { Returns a TSL instead of a string. The caller has to free the result }
 function  StringFromFileExists (CONST FileName: string): String;                                  { Read file IF it exists. Otherwise, return '' }
 function  StringFromFileA      (CONST FileName: string): AnsiString;                              { Read a WHOLE file and return its content as String. Also see this: http://www.fredshack.com/docs/delphi.html }
 function  StringFromFile       (CONST FileName: string; Enc: TEncoding= Nil): string;
 procedure StringToFile         (CONST FileName: string; CONST aString: String;     CONST WriteOp: TWriteOperation= woOverwrite; WritePreamble: Boolean= FALSE);
 procedure StringToFileA        (CONST FileName: string; CONST aString: AnsiString; CONST WriteOp: TWriteOperation);  overload;
 procedure StringToFileA        (CONST FileName: string; CONST aString: String;     CONST WriteOp: TWriteOperation);      overload;


{--------------------------------------------------------------------------------------------------
   FILE BINARY COMPARE
--------------------------------------------------------------------------------------------------}
 function  CompareStreams      (A, B: TStream; BufferSize: Integer = 4096): Boolean;
 function  CompareFiles        (CONST FileA, FileB: TFileName; BufferSize: Integer = 4096): Boolean;


{--------------------------------------------------------------------------------------------------
   FILE MERGE
--------------------------------------------------------------------------------------------------}
 procedure CopyFilePortion     (CONST SourceName, DestName: string; CONST CopyBytes: int64);         { copy only CopyBytes bytes from the begining of the file }
 procedure AppendTo            (CONST MasterFile, SegmentFile, Separator: string; SeparatorFirst: Boolean= TRUE);                           { Append Segment to Master. Master must exists. }
 procedure MergeFiles          (CONST Input1, Input2, Output, Separator: string; SeparatorFirst: Boolean= TRUE);        { Merge file 'Input1' and file 'Input2' in a new file 'Output'. So, the difference between this procedure and AppendTo is that this proc does not modify the original input file(s) }
 function  MergeAllFiles       (CONST Folder, FileType, OutputFile, Separator: string; DigSubdirectories: Boolean= FALSE; SeparatorFirst: Boolean= TRUE): Integer;       { Merge all files in the specified folder.   FileType can be something like '*.*' or '*.exe;*.bin' }


 { OTHERS }
 function  CountLines          (CONST Filename: string; CONST BufferSize: Cardinal= 128000): Int64;                     { Opens a LARGE text file and counts how many lines it has. It does this by loading a small portion of the file in a RAM buffer }
 function  WriteBinFile        (CONST FileName: string; CONST Data: TBytes; CONST Overwrite: Boolean= TRUE): Boolean;
 procedure SetCompressionAtr   (CONST FileName: string; const CompressionFormat: USHORT= 1);    // http://stackoverflow.com/questions/7002575/how-can-i-set-a-files-compression-attribute-in-delphi
{System.IOUtils.TFile.Encrypt https://docwiki.embarcadero.com/Libraries/Alexandria/en/System.IOUtils.TFile.Encrypt - Encrypt a given file using the operating system-provided facilities.}


{--------------------------------------------------------------------------------------------------
   COPY/MOVE
--------------------------------------------------------------------------------------------------}
 {COPY FILES}
 function  FileCopyTo          (CONST sFrom, sTo: string; Overwrite: boolean): boolean;
 function  FileCopyQuick       (CONST From_FullPath, To_DestFolder: string; Overwrite: boolean): boolean;     { in this function you don't have to provide the full path for the second parameter but only the destination folder }

 {MOVE FILES}
 function  FileMoveTo          (CONST From_FullPath, To_FullPath  : string): boolean;
 function  FileMoveToDir       (CONST From_FullPath, To_DestFolder: string; Overwrite: Boolean): Boolean;

 {FOLDERS}
 function  CopyFolder          (CONST FromFolder, ToFolder   : String; Overwrite: Boolean= True; FileType: string= '*.*'): integer;          { copy a folder and all its files and subfolders }
 function  MoveFolderRel       (CONST FromFolder, ToRelFolder: string; Overwrite: Boolean= True): string;
 procedure MoveFolder          (CONST FromFolder, ToFolder   : String; SilentOverwrite: Boolean= True);
 function  MoveFolderSlow      (CONST FromFolder, ToFolder   : String; Overwrite: boolean): Integer; deprecated 'Use TDirectory.Move() instead.';



{--------------------------------------------------------------------------------------------------
   BACKUP
--------------------------------------------------------------------------------------------------}
 function BackupFileIncrement  (CONST FileName: string; DestFolder: string= ''): string;               { Creates a copy of this file in the new folder.  Automatically increments its name. Returns '' in case of copy failure }
 function BackupFileDate       (CONST FileName: string;             TimeStamp: Boolean= TRUE; Overwrite: Boolean = TRUE): Boolean;  overload;     { Create a copy of the specified file in the same folder. The '_backup' string is attached at the end of the filename }
 function BackupFileDate       (CONST FileName, DestFolder: string; TimeStamp: Boolean= TRUE; Overwrite: Boolean = TRUE): Boolean;  overload;
 function BackupFileBak        (CONST FileName: string): Boolean; { Create a copy of this file, and appends as file extension. Ex: File.txt -> File.txt.bak }



{--------------------------------------------------------------------------------------------------
   DELETE
--------------------------------------------------------------------------------------------------}

 {DELETE FOLDERS}
 procedure EmptyDirectory      (CONST Path: string);                                                          { Delete all files in the specified folder, but don't delete the folder itself. It will search also in subfolders }
 procedure DeleteFolder        (CONST Path: string);
 procedure RemoveEmptyFolders  (CONST RootFolder: string);                                                    { NETESTATA! Delete all empty folders / sub-folders (any sub level) under the provided "rootFolder" }

 {DELETE FILES}
 function  RecycleItem(CONST ItemName: string; CONST DeleteToRecycle: Boolean= TRUE; CONST ShowConfirm: Boolean= TRUE; CONST TotalSilence: Boolean= FALSE): Boolean;
 function  DeleteFileWithMsg   (CONST FileName: string): Boolean;

 {API OPERATIONS}
 function FileOperation        (CONST Source, Dest: string; Op, Flags: Integer): Boolean;                     { Performs: Copy, Move, Delete, Rename on files + folders via WinAPI}



{--------------------------------------------------------------------------------------------------
   FILE ACCESS
--------------------------------------------------------------------------------------------------}
 function  FileIsLockedR   (FileName: string): Boolean;
 function  FileIsLockedRW  (FileName: string): Boolean;                                            { Returns true if the file cannot be open for reading and writing } { old name: FileInUse }
 function  TestWriteAccess (FileOrFolder: string): Boolean;                                        { Returns true if it can write that file to disk. ATTENTION it will overwrite the file if it already exists ! }
 function  TestWriteAccessMsg(CONST FileOrFolder: string): Boolean;                                { USER HAS WRITE ACCESS? Returns an error message instead of boolean}
 function  IsDirectoryWriteable(CONST Dir: string): Boolean;    { Source: https://stackoverflow.com/questions/3599256/how-can-i-use-delphi-to-test-if-a-directory-is-writeable }
 function  CanCreateFile   (AString:String): Boolean;
 function  ShowMsg_CannotWriteTo(CONST sPath: string): string;                                       { Also see  IsDiskWriteProtected }


{--------------------------------------------------------------------------------------------------
   FILE SIZE
--------------------------------------------------------------------------------------------------}
 function  GetFolderSize    (aFolder: string; FileType: string= '*.*'; DigSubdirectories: Boolean= TRUE): Int64;

 function  GetFileSizeEx    (hFile: THandle; VAR FileSize: Int64): BOOL; stdcall; external kernel32;
 function  GetFileSize      (CONST aFilename: string): Int64;
 function  GetFileSize_     (CONST aFilename: string): Int64;

 function  GetFileSizeFormat(CONST sFilename: string): string;                                     { Same as GetFileSize but returns the size in b/kb/mb/etc }


{--------------------------------------------------------------------------------------------------
   FILE TIME
--------------------------------------------------------------------------------------------------}
 function  FileTimeToDateTimeStr(FTime: TFileTime; DFormat: string; TFormat: string): string;
 function  FileAge(CONST FileName: string): TDateTime;
 function  ExtractTimeFromFileName(FileName: string): TTime;                                       { The time must be at the end of the file name. Example: 'MyPicture 20-00.jpg'. Returns -1 if the time could not be extracted. }
 function  DateToStr_IO(CONST DateTime: TDateTime): string;                                        { Original name: StrTimeToSeconds_unsafe }
 function  TimeToStr_IO(CONST DateTime: TDateTime): string;
 function  DateTimeToStr_IO(CONST DateTime: TDateTime): string;    overload;                       { Used to conver Date/Time to a string that is safe to use in a path. For example, instead of '2013/01/01' 15:32 it will return '2013-01-01 15,32' }
 function  DateTimeToStr_IO: string;                               overload;




{--------------------------------------------------------------------------------------------------
   DRIVES
--------------------------------------------------------------------------------------------------}
 function GetDriveType (CONST Path: string): Integer;   { Returns drive type. Path can be something like 'C:\' or '\\netdrive\' }
 function GetDriveTypeS(CONST Path: string): string;    { Returns drive type as string }

 { Validity }
 function  DiskInDrive        (CONST Path: string): Boolean; overload;                              { From www.gnomehome.demon.nl/uddf/pages/disk.htm#disk0 . Also see http://community.borland.com/article/0,1410,15921,00.html }
 function  DiskInDrive        (CONST DriveNo: Byte): Boolean; overload;                            { THIS IS VERY SLOW IF THE DISK IS NOT IN DRIVE! The GUI will freeze until the drive responds. }
 function  ValidDrive         (CONST Drive: Char): Boolean;                                        { Peter Below (TeamB). http://www.codinggroups.com/borland-public-delphi-rtl-win32/7618-windows-no-disk-error.html }
 function  ValidDriveLetter   (CONST Drive: Char): Boolean;                                        { Returns false if the drive letter is not in ['A'..'Z'] }
 function  DriveProtected     (CONST Drive: Char): BOOLEAN;                                        { Attempt to create temporary file on specified drive. If created, the temporary file is deleted. } {old name: IsDiskWriteProtected }

 {}
 function  GetVolumeLabel     (CONST Drive: Char): string;                                         { function returns volume label of a disk }
 function  ExtractDriveLetter (CONST Path: string): char; deprecated  'Use IOUtils.TDirectory.GetDirectoryRoot instead.'   { Returns #0 for invalid or network paths. GetDirectoryRoot returns something like:  'C:\' }

 {}
 function  DriveFreeSpace     (CONST Drive: Char): Int64;
 function  DriveFreeSpaceS    (CONST Drive: Char): string;
 function  DriveFreeSpaceF    (CONST FullPath: string): Int64;                                     { Same as DriveFreeSpace but this accepts a full filename/directory path. It will automatically extract the drive }

 { Convert }
 function  Drive2Byte         (CONST Drive: Char): Byte;                                           { Converts the drive letter to the number of that drive. Example drive "A:" is 1 and drive "C:" is 3 }
 function  Drive2Char         (CONST DriveNumber: Byte): Char;                                     { Converts the drive number to the letter of that drive. Example drive 1 is "A:" floppy }
 function  GetLogicalDrives: TStringDynArray;  inline;


IMPLEMENTATION

USES
  ccRegistry,
  ccWinVersion,
  ccAppData,
  ccCore;








{--------------------------------------------------------------------------------------------------
   LINUX
--------------------------------------------------------------------------------------------------}

{ Adds / in front and at the end of the path }
function TrailLinuxPathEx(CONST Path: string): string;
begin
 Result:= Path;
 if Path > '' then
  begin
   if FirstChar(Result) <> '/'
   then Result:= '/'+ Result;

   if LastChar(Result) <> '/'
   then Result:= Result+ '/';
  end;
end;


{ Adds / at the end of the path }
function TrailLinuxPath(CONST Path: string): string;
begin
 if (Path > '')
 AND (LastChar(Path) <> '/')
 then Result:= Path+ '/'
 else Result:= Path;
end;



function TrimLastLinuxSeparator(CONST Path: string): string;
begin
 if LastChar(Path) = '/'
 then Result:= ccCore.RemoveLastChar(Path)
 else Result:= Path;
end;


{ Converts DOS path to Linux path. Does not handle C: but only the \ separators }
function Convert2LinuxPath(DosPath: string): string;     // old name: MakeLinuxPath
begin
 Result:= ReplaceCharF(DosPath, '\', '/');;
end;



function Convert2DosPath(LinuxPath: string): string;
begin
 Result:= ReplaceCharF(LinuxPath, '/', '\');
end;









{--------------------------------------------------------------------------------------------------
   FOLDER
--------------------------------------------------------------------------------------------------}

{ Returns True if this path seems to be Unicode }
function IsUnicode (CONST Path: string): boolean;
begin
 Result:= pos('?', Path)> 0;   {WTF?}
end;


{ Tells if FullPath is an EXISTING folder or file. If the FullPath  represents a folder that does not exist, the function will return False!
  Works with UNC paths
  https://stackoverflow.com/questions/63606215/how-to-check-if-given-path-is-file-or-folder }
function IsFolder(CONST FullPath: string): boolean;
begin
 Result:= {NOT FileExists(FullPath) AND} DirectoryExists(FullPath);
 {del
 // We need both checks for the case where FullPath is not a valid file/folder. In other words, we cannot use only Result:= DirectoryExists(FullPath);
 if FileExists(FullPath)
 then Result:= FALSE
 else Result:= DirectoryExists(FullPath);  }
end;


{ Appends the 'delimiter' at the ends of the string IF it doesn't already exists there. Works with UNC paths }
function ForcePathDelimiters(CONST Path, Delimiter: string; SetAtBegining, SetAtEnd: Boolean): string;
begin
 Assert(Path > '');

 Result:= Path;
 if SetAtBegining AND (Result[1]<> Delimiter)
 then Result:= Delimiter+ Result;

 if SetAtEnd AND (Result[Length(Result)]<> Delimiter)
 then Result:= Result+ Delimiter;
end;


{ Works with UNC paths }
function Trail(CONST Path: string): string;
begin
 if Path= '' then EXIT('');      { I may encounter this when I do this:  ExtractLastFolder('c:\'). ExtractLastFolder will return '' }
 Result:= IncludeTrailingPathDelimiter(Path)
end;


{ Check if folder is empty }
function FolderIsEmpty(const FolderName: string): Boolean;
begin
 Result:= TDirectory.IsEmpty(FolderName);
end;


{ Ex: C:\Test1\ and C:\teSt1 will return true }
function SameFolder(Path1, Path2: string): Boolean;
begin
 Path1:= Trail(Path1);
 Path2:= Trail(Path2);

 Result:= SameText(Path1, Path2);
end;


{ Receives two partial or complete file names and compare their folders.
  Ex:  C:\Test1 and C:\teSt1\me.txt will return true }
function SameFolderFromFile(Path1, Path2: string): Boolean;
begin
 Path1:= ExtractFilePath(Path1);
 Path2:= ExtractFilePath(Path2);

 Path1:= Trail(Path1);
 Path2:= Trail(Path2);

 Result:= SameText(Path1, Path2);
end;


{ Returns a path that is not longer than MAX_PATH allowed in Windows
  It does this by shortening the filename.
  The caller must make sure that the resulted file name won't be too short (0 chars)!
  IMPORTANT! We cannot use TPath here because it cannot handle long file names. Details: http://stackoverflow.com/questions/31427260/how-to-handle-very-long-file-names?noredirect=1#comment50831709_31427260

  Also exists:
       FileCtrl.MinimizeName (if you require pixels)
       cGraphics.DrawStringEllipsis
       ccCore.ShortenString
       ccIO.ShortenFileName
}
function ShortenFileName(CONST FullPath: String; MaxLength: Integer= MAXPATH): string;
VAR
   FilePath, ShortenedFileName: string;
   ResultedFileLength: Integer;
begin
  ResultedFileLength:= Length(FullPath);
  if ResultedFileLength > MaxLength
  then
   begin
    FilePath:= Trail(System.SysUtils.ExtractFilePath(FullPath));             { IMPORTANT! We cannot use TPath here because it cannot handle long file names }
    ResultedFileLength:= MaxLength - Length(FilePath) - Length(ExtractFileExt(FullPath));
    ShortenedFileName := system.COPY(FullPath, Length(FilePath)+ 1, ResultedFileLength);
    Result:= FilePath+ ShortenedFileName+ ExtractFileExt(FullPath);
   end
  else Result:= FullPath;
end;


function CheckPathLength(const FullPath: string; MaxLength: Integer= MAXPATH): Boolean;
begin
 {$IFDEF MSWINDOWS}
 Result:= TPath.IsExtendedPrefixed(FullPath)    { Checks whether a given path has an extended prefix. Call IsExtendedPrefixed to check whether the given path contains an extension prefix. Paths prefixed with \\?\ or \\?\UNC\ are Windows-specific and can be of very big lengths and not restricted to 255 characters (MAX_PATH). It is a common case today to manage paths longer than 255 characters. Prefixing those with \\?\ solves the problem. }
       OR (NOT TPath.IsExtendedPrefixed(FullPath) AND (Length(FullPath) < MaxLength));
 {$ENDIF MSWINDOWS}

 {$IFDEF POSIX}
 Result:= (Length(UTF8Encode(FullPath)) < MaxLength)  // Check the length in bytes on POSIX
 {$ENDIF POSIX}
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

{$WARN SYMBOL_PLATFORM OFF}
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
 if ccWinVersion.IsWindowsVistaUp then
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
{$WARN SYMBOL_PLATFORM On}






{-------------------------------------------------------------------------------------------------------------
   Prompt To Save/Load File
   Example: PromptToSaveFile(s, cGraphUtil.JPGFtl, 'txt')

   DefaultExt. Only for TSaveDialog. Extensions longer than three characters are not supported! Do not include the period (.) that divides the file name and its extension.
-------------------------------------------------------------------------------------------------------------}
function PromptToSaveFile(VAR FileName: string; Filter: string = ''; DefaultExt: string= ''; Title: string= ''): Boolean;
VAR InitialDir: string;
begin
 if FileName > '' then
   if IsFolder(FileName)
   then InitialDir:= FileName
   else InitialDir:= ExtractFilePath(FileName);

 Result:= PromptForFileName(FileName, TRUE, Filter, DefaultExt, Title, InitialDir);
end;


{ AllowMultiSelect cannot be true, because I return a single file name (cannot return a Tstringlist)  }
//ToDo 1: Implement two variables: AppLastFile and AppLastFolder
Function PromptToLoadFile(VAR FileName: string; Filter: string = ''; Title: string= ''): Boolean;
VAR InitialDir: string;
begin
 if FileName > '' then
   if IsFolder(FileName)
   then InitialDir:= FileName
   else InitialDir:= ExtractFilePath(FileName);

 Result:= PromptForFileName(FileName, FALSE, Filter, '', Title, InitialDir);
end;


{ Based on Vcl.Dialogs.PromptForFileName }
Function PromptForFileName(VAR FileName: string; SaveDialog: Boolean; Filter: string = ''; DefaultExt: string= ''; Title: string= ''; InitialDir: string = ''): Boolean;
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
    then Dialog.InitialDir:= AppData.AppDataFolder
    else Dialog.InitialDir:= InitialDir;

    Dialog.FileName := FileName;

    Result := Dialog.Execute;

    if Result then
      begin
        FileName:= Dialog.FileName;
        AppData.LastUsedFolder:= ExtractFilePath(FileName);
      end;
  FINALLY
    FreeAndNil(Dialog);
  END;
end;






{-------------------------------------------------------------------------------------------------------------
   TFileOpenDlg

   Example: PromptToSaveFile(s, ccCore.FilterTxt, 'txt')
   Note: You might want to use PromptForFileName instead
-------------------------------------------------------------------------------------------------------------}
Function GetOpenDialog(FileName, Filter, DefaultExt: string; Caption: string= ''): TOpenDialog;
begin
 Result:= TOpenDialog.Create(NIL);
 Result.Filter:= Filter;
 Result.FilterIndex:= 0;
 Result.Options:= [ofFileMustExist, ofEnableSizing, ofForceShowHidden];
 Result.DefaultExt:= DefaultExt;
 Result.FileName:= FileName;
 Result.Title:= Caption;

 if FileName= ''
 then Result.InitialDir:= AppData.AppDataFolder
 else Result.InitialDir:= ExtractFilePath(FileName);
end;


{ Example: SaveDialog(ccCore.FilterTxt, 'csv');  }
Function GetSaveDialog(FileName, Filter, DefaultExt: string; Caption: string= ''): TSaveDialog;
begin
 Result:= TSaveDialog.Create(NIL);
 Result.Filter:= Filter;
 Result.FilterIndex:= 0;
 Result.Options:= [ofOverwritePrompt, ofHideReadOnly, ofFileMustExist, ofEnableSizing];  //  - ofNoChangeDir  { When a user displays the open dialog, whether InitialDir is used or not, the dialog alters the program's current working directory while the user is changing directories before clicking on the Ok/Open button. Upon closing the dialog, the current working directly is not reset to its original value unless the ofNoChangeDir option is specified.  }
 Result.DefaultExt:= DefaultExt;
 Result.FileName:= FileName;
 Result.Title:= Caption;

 if FileName= ''
 then Result.InitialDir:= AppData.AppDataFolder
 else Result.InitialDir:= ExtractFilePath(FileName);
end;








{--------------------------------------------------------------------------------------------------
   FOLDER VALIDITY
   Works with UNC paths
   Correct invalid characters in a path. Path = path with filename, like: c:\my docs\MyFile.txt
--------------------------------------------------------------------------------------------------}
function CorrectFolder(CONST Folder: string; ReplaceWith: Char): string;                                                                                                     { Old name: CorrectPath, RemoveInvalidPathChars }
VAR i: Integer;
begin
 {TODO: Make it work with UNC paths! }
 Result:= Folder;
 for i:= 1 to Length(Result) DO
   if CharInSet(Result[I], InvalidPathChar)
   OR (Result[i] < ' ')                                                                            { tot ce e sub SPACE }
   then Result[i]:= ReplaceWith;
end;


{
  Returns FALSE if the path is too short or contains invalid characters.
  Tells nothing about the existence of the folder.
  Note: TPath.HasValidPathChars is bugged. https://stackoverflow.com/questions/45346525/why-tpath-hasvalidpathchars-accepts-as-valid-char-in-a-path/45346869#45346869
}
function PathNameIsValid(CONST Path: string): Boolean;
VAR i: Integer;
begin
 {ToDo: Accept UNC paths like: \??\Windows. For this check for the \?? patern }
 Result:= Length(Path) > 0;                                                                        { Minimum I can have 'C:' }
 for i:= 1 to Length(Path) DO
  if CharInSet(Path[I], InvalidPathChar)
  then EXIT(FALSE);
end;


{ DOESN'T WORK WITH UNC PATHS !!!!!!!!!
  Deprecated 'Use System.IOUtils.TPath.HasValidFileNameChars instead.'
  HasValidFileNameChars only work with file names, not also with full paths. }
function FileNameIsValid_(CONST FileName: string): Boolean;
VAR i, Spaces: Integer;
begin
 if Length(FileName) > 0
 then Result:= TRUE
 else EXIT(FALSE);

 { File name cannot contain only spaces }
 Spaces:= 0;
 for i := 1 to Length(FileName) DO
    if FileName[i]= ' '
    then Inc(Spaces)
    else break;
 if Spaces= Length(FileName)
 then EXIT(FALSE);

 { Check invalid chars }
 for i := 1 to Length(FileName) DO
   if CharInSet(FileName[i], InvalidFileCharWin)
   OR (Ord(FileName[i]) < 32)
   then EXIT(FALSE);
end;


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
    TPathPrefixType.pptNoPrefix:
      Result := 1;
    TPathPrefixType.pptExtended:
      Result := Length(FCExtendedPrefix) + 1;
    TPathPrefixType.pptExtendedUNC:
      Result := Length(FCExtendedUNCPrefix) + 1;
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






{--------------------------------------------------------------------------------------------------
   FOLDER EXISTENCE
--------------------------------------------------------------------------------------------------}

{  This corrects a bug in the original 'DirectoryExists' which returns true for a folder that does not exist.
   The bug appears when the path ends with a SPACE. Exemple: 'c:\Program Files\ '
   Works with UNC paths! }
function DirectoryExists(CONST Directory: String; FollowLink: Boolean= TRUE): Boolean;
begin
 Result:= (LastChar(Directory)<> ' ')                                                              { Don't accept Space at the end of a path (after the backslash) }
      AND System.SysUtils.DirectoryExists(Directory, FollowLink);
end;


function DirectoryExistMsg(CONST Path: string): Boolean;                                           { Directory Exist }
begin
 Result:= DirectoryExists(Path);
 if NOT Result then
 if Path= ''
 then MesajError('DirectoryExistMsg: No folder specified!')
 else
  if Pos(':', Path)< 1                                                                             { check if the user has given a full path as c:\xxx }
  then MesajError('A relative path was provided instead of a full path!'+ CRLF+ Path)
  else MesajError('Folder does not exist:'+ CRLF+ Path);
end;



{--------------------------------------------------------------------------------------------------
   CREATE FOLDER

   Tries to create the specified folder
   Works with UNC paths.
   Writing on a readonly folder: ignores the ReadOnly attribute (same for H and S attributes)

   Returns:
     False if the path is invalid.
     False if the drive is readonly.

     Raises exception if parameter is invalid
     Raises exception if parameter is empty
     Raises exception if drive is invalid
--------------------------------------------------------------------------------------------------}
function ForceDirectoriesB(FullPath: string): Boolean;
begin
  TDirectory.CreateDirectory(FullPath);
  Result:= DirectoryExists(FullPath);
end;


// Works with UNC paths
function ForceDirectories(CONST FullPath: string): Integer;
{RETURNS:
  -1 = Error creating the directory
   0 = Directory already exists
  +1 = Directory created succesfully  }
begin
 Assert(FullPath> '', 'ForceDirectories - Parameter is empty!');
 if DirectoryExists (FullPath)
 then Result:= 0
 else
    if ForceDirectoriesB(FullPath)
    then Result:= +1
    else Result:= -1;
end;


{ Shows an error message if the folder cannot be created. }
function ForceDirectoriesMsg(CONST FullPath: string): boolean;
begin
 Result:= ForceDirectories(FullPath) >= 0;
 if NOT Result
 then MesajError('Cannot create folder: '+ FullPath+ CRLF+ 'Probably you are trying to write to a folder to which you don''t have write permissions, or, the folder you want to create is invalid.');
end;






{--------------------------------------------------------------------------------------------------
   FILE - DETECT FILE TYPE
--------------------------------------------------------------------------------------------------}

{ Video files supported by FFVCL (cFrameServerAVI) }
function IsVideo(CONST AGraphFile: string): Boolean;
VAR sExtension: string;
begin
 sExtension:= ExtractFileExtUp(AGraphFile);
 Result:=
    (sExtension= '.AVI')  OR
    (sExtension= '.MKV')  OR
    (sExtension= '.MPEG') OR
    (sExtension= '.MP4')  OR
    (sExtension= '.MP' )  OR
    (sExtension= '.MPG')  OR
    (sExtension= '.WMV')  OR
    (sExtension= '.VOB')  OR
    (sExtension= '.ASF')  OR
    (sExtension= '.OGM')  OR
    (sExtension= '.AVS')  OR
    (sExtension= '.MOV')  OR
    (sExtension= '.3GP')  OR
    (sExtension= '.RM' )  OR
    (sExtension= '.RMVB') OR
    (sExtension= '.NSV')  OR
    (sExtension= '.TP' )  OR
    (sExtension= '.TS' )  OR
    (sExtension= '.FLV')  OR
    (sExtension= '.DAT')  OR
    (sExtension= '.AVM');
end;


{ Generic video file detection. It doesn't mean I have support for all those files in my app }
function IsVideoGeneric(CONST AGraphFile: string): Boolean;
VAR sExtension: string;
begin
 sExtension:= ExtractFileExtUp(AGraphFile);
 Result:=
    { GLOBAL }
    (sExtension= '.AVI')  OR
    (sExtension= '.MKV')  OR
    (sExtension= '.DIVX') OR
    (sExtension= '.VOB')  OR
    { MOTION PICT }
    (sExtension= '.MPG')  OR
    (sExtension= '.MPEG') OR
    (sExtension= '.MP4')  OR
    (sExtension= '.MP2')  OR
    (sExtension= '.MP')   OR
    (sExtension= '.M4P')  OR
    { MS }
    (sExtension= '.ASF')  OR
    (sExtension= '.WMA')  OR
    (sExtension= '.WM' )  OR
    (sExtension= '.ASX')  OR
    (sExtension= '.WMV')  OR
    (sExtension= '.WVX')  OR
    (sExtension= '.WMX')  OR
    (sExtension= '.WPL')  OR
    (sExtension= '.WMD')  OR
    (sExtension= '.IVF')  OR
    (sExtension= '.WAX')  OR
    (sExtension= '.M1V')  OR
    (sExtension= '.DRV-MS') OR
    { WEB }
    (sExtension= '.WEBM') OR
    (sExtension= '.F4V')  OR
    (sExtension= '.FLV')  OR
    { MAC }
    (sExtension= '.OGV')  OR
    (sExtension= '.QT')   OR
    (sExtension= '.MOV')  OR
    (sExtension= '.RM')   OR
    { MOBILE }
    (sExtension= '.AMV')  OR
    (sExtension= '.3GP')  OR
    (sExtension= '.NSV')  OR
    { others }
    (sExtension= '.AVM')  OR
    (sExtension= '.AVS')  OR
    (sExtension= '.DAT')  OR
    (sExtension= '.RP' )  OR
    (sExtension= '.OGM')  OR
    (sExtension= '.RMVB') OR
    (sExtension= '.TS' );
end;


function IsGIF(CONST AGraphFile: string): Boolean;
begin
 Result:= ExtractFileExtUp(AGraphFile)= '.GIF';
end;


function IsJpg(CONST AGraphFile: string): Boolean;
VAR sExtension: string;
begin
 sExtension:= ExtractFileExtUp(AGraphFile);
 Result:= (sExtension= '.JPG') OR (sExtension= '.JPEG') OR (sExtension= '.JPE') OR (sExtension= '.JFIF') OR (sExtension= '.JP');
end;


function IsJp2(CONST AGraphFile: string): Boolean;
VAR sExtension: string;
begin
 sExtension:= ExtractFileExtUp(AGraphFile);
 Result:= (sExtension= '.J2K') OR (sExtension= '.JPC') OR (sExtension= '.JP2')
end;


function IsBMP(CONST AGraphFile: string): Boolean;
begin
 Result:= ExtractFileExtUp(AGraphFile)= '.BMP';
end;


function IsWB1(CONST AGraphFile: string): Boolean;
begin
 Result:= ExtractFileExtUp(AGraphFile)= '.WB1';
end;


function IsWBC(CONST AGraphFile: string): Boolean;
begin
 Result:= ExtractFileExtUp(AGraphFile)= '.WBC';
end;


function IsPNG(CONST AGraphFile: string): Boolean;
begin
 Result:= ExtractFileExtUp(AGraphFile)= '.PNG';
end;


function IsICO(CONST AGraphFile: string): Boolean;
begin
 Result:= ExtractFileExtUp(AGraphFile)= '.ICO';
end;


function IsEMF(CONST AGraphFile: string): Boolean;
begin
 Result:= ExtractFileExtUp(AGraphFile)= '.EMF';
end;


function IsWMF(CONST AGraphFile: string): Boolean;
begin
 Result:= ExtractFileExtUp(AGraphFile)= '.WMF';
end;


function IsImage(CONST AGraphFile: string): Boolean;
begin
 Result:=
    IsJpg(AGraphFile)
 OR IsJp2(AGraphFile)
 OR IsBMP(AGraphFile)
 OR IsGIF(AGraphFile)
 OR IsPNG(AGraphFile)
 OR IsBMP(AGraphFile);
end;


{ Returns TRUE if the file has a known extension and it can be converted to BMP }
function IsImage2Bmp(CONST AGraphFile: string): Boolean;
begin
 Result:=
    IsImage(AGraphFile)
 OR IsEMF(AGraphFile)
 OR IsWMF(AGraphFile)
 OR IsICO(AGraphFile);
end;





function IsExec(CONST FileName: string) : Boolean;
begin
 Result:=
    (ExtractFileExtUp(FileName)= '.BAT') OR
    (ExtractFileExtUp(FileName)= '.CMD') OR
    (ExtractFileExtUp(FileName)= '.COM') OR
    (ExtractFileExtUp(FileName)= '.CPL') OR
    (ExtractFileExtUp(FileName)= '.DLL') OR
    (ExtractFileExtUp(FileName)= '.EXE') OR
    (ExtractFileExtUp(FileName)= '.JAR') OR
    (ExtractFileExtUp(FileName)= '.MSI') OR
    (ExtractFileExtUp(FileName)= '.MSP') OR
    (ExtractFileExtUp(FileName)= '.PIF') OR
    (ExtractFileExtUp(FileName)= '.PS1') OR   // A Windows PowerShell script. Runs PowerShell commands in the order specified in the file.
    (ExtractFileExtUp(FileName)= '.PS2') OR   // A Windows PowerShell script. Runs PowerShell commands in the order specified in the file
    (ExtractFileExtUp(FileName)= '.SCR') OR
    (ExtractFileExtUp(FileName)= '.VBE') OR
    (ExtractFileExtUp(FileName)= '.WS')  OR
    (ExtractFileExtUp(FileName)= '.WSF') OR
    (ExtractFileExtUp(FileName)= '.WSC') OR   //Windows Script Component and Windows Script Host control files. Used along with with Windows Script files.
    (ExtractFileExtUp(FileName)= '.WSH') OR   //Windows Script Component and Windows Script Host control files. Used along with with Windows Script files.
    (ExtractFileExtUp(FileName)= '.VBS');
end;


function IsPas(CONST FileName: string) : Boolean;
begin
 Result:= ExtractFileExtUp(FileName)= '.PAS';
end;


function IsDfm(CONST FileName: string) : Boolean;
begin
 Result:= ExtractFileExtUp(FileName)= '.DFM';
end;


function IsDpr(CONST FileName: string) : Boolean;
begin
 Result:= ExtractFileExtUp(FileName)= '.DPR';
end;


function IsDpk(CONST FileName: string) : Boolean;
begin
 Result:= ExtractFileExtUp(FileName)= '.DPK';
end;


function IsDelphi(CONST FileName: string) : Boolean;
begin
 Result:= IsPas(FileName)
       OR IsDpk(FileName)
       OR IsDpr(FileName);
end;





{--------------------------------------------------------------------------------------------------
   FILE
--------------------------------------------------------------------------------------------------}

{ Example: ChangeFilePath (c:\test\1.txt, d:\data) will return 'd:\data\1.txt'
  Works with UNC paths. }
function ChangeFilePath(CONST FullFileName, NewPath: string): string;
begin
 Result:= ExtractFileName(FullFileName);
 Result:= Trail(NewPath)+ Result;
end;


{ Inserts a string between name and ext.
  Ex: AppendBeforeName('Shell32.DLL', 'Backup ')  -->  'Backup Shell32.DLL'
  Works with UNC paths. }
function AppendBeforeName(CONST FullPath, ApendedText: string): string;
begin
 Result:= ExtractFilePath (FullPath)+
          ApendedText+
          ExtractFileName (FullPath);
end;


{ ReplaceExtension
  Makes sure that the 'FileName' file has indicated extension.
  If file already has an extension, it is replaced by the indicated one.
  The 'Ext' parameter should be like: '.txt' }
function ForceExtension(CONST FileName, Ext: string): string;
begin
 Result:= ExtractOnlyName(FileName);                                      { Extact only file name... }
 Result:= ExtractFilePath(FileName)+ Result+ Ext;                         { ... and append new ext to it }
end;


{ Replaces the old System.SysUtils.ExtractFileDir
  If AcceptInvalidPaths= True, it will raise an exception when the path is empty or has invalid chars }
function ExtractFilePath(CONST FullPath: string; AcceptInvalidPaths: Boolean= TRUE): string;
begin
 if (FullPath > '')
 AND PathNameIsValid(FullPath)
 then Result:= Trail(TPath.GetDirectoryName(FullPath)) { WARNING: GetDirectoryName crashes when the filename is too long!!!!!!!!!!!! }
 else
   if AcceptInvalidPaths
   then Result:= ''
   else RAISE Exception.Create('The path is invalid!' + CRLF+ FullPath);    { GetDirectoryName shows an error message if the path is empty but the debuger won't stop. So I force the stop. }
end;


{ FILES WITH MULTI EXTENSIONS
  Works also when the file has multiple extensions (nume+extensie+extensie). It removes only the last ext. }
function ExtractOnlyName(CONST FileName: string): string;
VAR
   iPos: integer;
   s: string;
begin
 s:= ExtractFileName(FileName);
 iPos:= LastPos('.', s);       { It may happen that the user provides a file name without extension (I personally use this alot) }
 if iPos < 1
 then Result:= s
 else Result:= CopyTo(s, 1, iPos-1);
end;


{ Removed the last file extension.
  If the name is of type name +extension +extension, only the last extension will be removed.
  Works with UNC paths. }
function RemoveLastExtension(CONST FileName: string): string;
begin
 Result:= Tpath.GetFileNameWithoutExtension(FileName);
end;


{ Case insensitive version of ExtractFileExt }
function ExtractFileExtUp(CONST FileName: string): string;      // Old name: ExtractFileExtIns
begin
 Result:= UpperCase(ExtractFileExt(FileName));
end;


{ Returns truncated path, relative to 'RelativeTo'.
  deprecated 'Use System.SysUtils.ExtractRelativePath instead'
  Example:  ExtractRelativePath('c:\windows\system32\user32.dll', 'c:\windows') returns system32\user32.dll.
  Works with UNC paths }
function ExtractRelativePath_(CONST FullPath, RelativeTo: string): string;
begin
 Result:= System.COPY(FullPath, Length(RelativeTo)+1, MaxInt);
end;


{ Example: C:\MyDocuments\1.doc  ->  C }
function ExtractDrive(CONST FullPath: string): string;
VAR i: Integer;
begin
 I:= Pos(':', FullPath);
 if I = 2
 then Result:= FullPath[1]
 else Result:= '';
end;


{ Example: C:\MyDocuments\1.doc  ->  MyDocuments\1.doc }
function RemoveDrive(CONST FullPath: string): string;
VAR i: Integer;
begin
 I:= Pos(':', FullPath);
 if I > 0
 then Result:= system.copy(FullPath, I + 2, Length(FullPath)) { +1 to jump over ':' and another +1 to jump over '\' }
 else Result:= '';
end;


{ Receives the full path of a file. Replaces ONLY its name (keeps the folder and the extension).
  Ex: C:\test\Name.dll -> C:\test\NewName.dll
  Works with UNC paths }
function ReplaceOnlyName(CONST FileName, newName: string): string;
begin
 Result:= ExtractFilePath (FileName)+ newName+ ExtractFileExt  (FileName);
end;


{ Adauga un sir intre nume si extensie. Exemplu: daca adaug sirul ' backup' la fisierul 'c:\1\Shell32.DLL' ->  'c:\1\Shell32 backup.DLL'
  Works with UNC paths }
function AppendToFileName(CONST FileName, ApendedText: string): string;
begin
 Result:= ExtractFilePath (FileName)+
          ExtractOnlyName (FileName)+ ApendedText+
          ExtractFileExt  (FileName);
end;


{--------------------------------------------------------------------------------------------------
   INCREMENT FOLDER NAME
     Adds a number at the end of the path.
     If a folder with the same name already exists, it keeps incrementing the number until a unique (unalocated) folder name is found
     NOTE: when used with an UNC path, if the drive is not online, the result will be indentic with the input (no change, no increment)
     Old name: IncrementFolderName
--------------------------------------------------------------------------------------------------}
function MakeUniqueFolderName(CONST RootPath, FolderName: string): string;
VAR i: integer;
begin
  Result:= Trail(RootPath) + FolderName;
  i := 0;
  WHILE DirectoryExists(Result) DO
   begin      { check if dir exists and if so add a number and try again }
    inc(i);
    Result:= Trail( Trail(RootPath) + FolderName + ' ' + IntToStr(i) );
   end;
end;


{--------------------------------------------------------------------------------------------------
   INCREMENT FILE NAME
     Increments the number contained in the file name (at its end).
     If the file does not contain a number, a 1 is automatically added.
--------------------------------------------------------------------------------------------------}
function IncrementFileName (CONST FileName: string): string;                      // Works with UNC paths
VAR outFileName, outFileNumber: string;
begin
 SplitNumber_End(ExtractOnlyName(FileName), outFileName, outFileNumber);
 Result:= ExtractFilePath(FileName)+ outFileName+ IncrementStringNoEx(outFileNumber)+ ExtractFileExt(FileName);
end;


{ Same as IncrementFileName but it automatically adds a number if the file doesn't already ends with a number. //ok  Works with UNC paths }
function IncrementFileNameEx (CONST FileName: string; StartAt, NumberLength: Integer): string;
begin
 if FileEndsInNumber(FileName)
 then Result:= IncrementFileName(FileName)
 else Result:= AppendNumber2Filename(FileName, StartAt, NumberLength);
end;







{-------------------------------------------------------------------------------------------------------------
   BACKUP
-------------------------------------------------------------------------------------------------------------}

{ Create a copy of the specified file in the same folder.
  It adds a number to the file name. If a file with the same name exists, it keeps incrementing until it finds the first empty slot.
  Returns '' in case of copy failure }
function BackupFileIncrement (CONST FileName: string; DestFolder: string= ''): string;
begin
 Assert(FileExistsMsg(FileName));

 if DestFolder= ''
 then Result:= FileName                                       // Keep file in same folder
 else Result:= Trail(DestFolder)+ ExtractFileName(FileName);  // Build new path

 if DestFolder<> ''
 then ForceDirectoriesMsg(DestFolder);

 REPEAT    { Increment file name until a file with same name does not exist anymore }
   Result:= IncrementFileNameEx(Result, 1, 3);
 UNTIL NOT FileExists(Result);

 if NOT FileCopyTo(FileName, Result, TRUE)
 then Result:= '';
end;


{ Create a copy of the specified file in the same folder.
  It adds the cur date to the file name.
  Old name:  FileMakeBackup }
function BackupFileDate(CONST FileName: string; TimeStamp: Boolean= TRUE; Overwrite : Boolean = TRUE): Boolean;
begin
 Result:= BackupFileDate(FileName, ExtractFilePath(FileName), TimeStamp, Overwrite)
end;


{ Create a copy of the specified file in the same folder. }  { Old name:  FileMakeBackup }
function BackupFileDate(CONST FileName, DestFolder: string; TimeStamp: Boolean= TRUE; Overwrite: Boolean = TRUE): Boolean;
VAR BackupName: string;
begin
 BackupName:= Trail(DestFolder)+ ExtractOnlyName(FileName);

 if TimeStamp
 then BackupName:= BackupName+ '  '+ DateTimeToStr_IO(Now)+ ExtractFileExt(FileName)
 else BackupName:= BackupName+ ' - backup'+ ExtractFileExt(FileName);

 Result:= FileCopyTo(FileName, BackupName, Overwrite);
end;


{ Create a copy of this file, and appends as file extension. Ex: File.txt -> File.txt.bak }
function BackupFileBak(CONST FileName: string): Boolean;
begin
 Result:= FileCopyTo(FileName, FileName+'.bak', TRUE);
end;






{ Returns true is the filename ends with a number.
  Example: MyFile02.txt returns TRUE.
  Works with UNC paths. }
function FileEndsInNumber(CONST FileName: string): Boolean;
VAR ShortName: string;
begin
  ShortName:= ExtractOnlyName(FileName);
  Result:= CharIsNumber(ShortName[Length(ShortName)]);
end;


{ Same as above but the user can specify how long the number should be.
  For example if sNumber is 1 and ForeceLength is 3, then the result will be 001.
  Works with UNC paths }
function AppendNumber2Filename(CONST FileName: string; StartAt, NumberLength: Integer): string;
VAR sPath, sExt: string;
begin
 sPath:= ExtractFilePath(FileName);
 sExt := ExtractFileExt (FileName);
 Result:= sPath+ ExtractOnlyName(FileName)+ '_'+LeadingZeros(IntToStr(StartAt), NumberLength)+ sExt;
end;










{--------------------------------------------------------------------------------------------------
    VALIDATE FILE NAME
--------------------------------------------------------------------------------------------------}
{ Correct invalid characters in a filename. FileName = File name without path.
  UNC test does not apply to this function because the function only accepts filenames which are not UNC }
function CorrectFilename(CONST FileName: string; ReplaceWith: Char= ' '): string;
VAR i: Integer;
begin
 Result:= FileName;

 for i:= 1 to Length(Result) DO
   if CharInSet(Result[I], InvalidFileCharWin)
   OR (Result[i] < ' ')
   then Result[i]:= ReplaceWith;

 Result:= Trim(Result);
end;


{$WARN SYMBOL_PLATFORM OFF}
{ Find first file in the specified folder }  // Works with UNC paths
function FindFirstFile(CONST aFolder, Ext: string): string;
const
 //faCompressed         = $0800;
   faNotContentIndexed  = $2000;
VAR
   SR: TSearchRec;
   Permission: Integer;
begin
 Result:= '';
 Permission:= faAnyFile- faDirectory+ faCompressed+ faNotContentIndexed;
 if (FindFirst(Trail(aFolder)+ ext, Permission, SR)= 0)
 then Result:= SR.Name;
 FindClose(sr);
end;
{$WARN SYMBOL_PLATFORM ON}


 { File Exists }
function FileExistsMsg(CONST FileName: string): Boolean;
begin
 Result:= FileExists(FileName);
 if NOT Result then
 if FileName= ''
 then MesajError('No file specified!')
 else MesajError('File does not exist!'+ CRLF+ FileName);
end;










{--------------------------------------------------------------------------------------------------
   FILE TIME
--------------------------------------------------------------------------------------------------}
function FileTimeToDateTimeStr(FTime: TFileTime; DFormat, TFormat: string): string;
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


{$WARN SYMBOL_PLATFORM OFF}
{ REPLACEMENT
    for System.SysUtils.FileAge which is not working with 'c:\pagefile.sys'.
    For details dee: http://stackoverflow.com/questions/3825077/fileage-is-not-working-with-c-pagefile-sys
}
function FileAge(CONST FileName: string): TDateTime;
VAR
  LocalFileTime     : TFileTime;
  SystemTime        : TSystemTime;
  SRec              : TSearchRec;
begin
 FindFirst(FileName, faAnyFile, SRec);
 TRY
   TRY
     FileTimeToLocalFileTime(SRec.FindData.ftLastWriteTime, LocalFileTime);
     FileTimeToSystemTime(LocalFileTime, SystemTime);
     Result := SystemTimeToDateTime(SystemTime);
   EXCEPT   //todo 1: trap only specific exceptions
     on e: Exception do
       Result:= -1;
   END;
 FINALLY
   FindClose(SRec);
 END;
end;
{$WARN SYMBOL_PLATFORM On}



{ The time must be at the end of the file name.
  Example: 'MyPicture 20-00.jpg'.
  Returns 0 if the time could not be extracted. }
function ExtractTimeFromFileName(FileName: string): TTime;
VAR s: string;
begin
 s:= ExtractOnlyName(FileName);
 if Length(s) <= 5 then EXIT(-1);    { File name patter is invalid (too short) }

 s:= CopyTo(s, Length(s)- 5, MaxInt);
 ReplaceChar(s, '-', ':');
 TRY
  FormatSettings.TimeSeparator:= ':';
  Result:= StrToTimeDef(s, 0);   { Don't fail if the string is invalid. Bionix relies on this! }
 EXCEPT //todo 1: trap only specific exceptions
  Result:= -1;
 END;
end;


function DateToStr_IO(CONST DateTime: TDateTime): string;                                          { Original name: StrTimeToSeconds_unsafe }
begin
 Result:= FormatDateTime('YYYY-MM-DD', DateTime);
end;


function TimeToStr_IO(CONST DateTime: TDateTime): string;
begin
 Result:= FormatDateTime('hh.mm.ss', DateTime);
end;


{ Used to conver Date/Time to a string that is safe to use in a path. For example, instead of '2013/01/01' 15:32 it will return '2013-01-01 15,32' }
function DateTimeToStr_IO(CONST DateTime: TDateTime): string;
begin
 Result:= FormatDateTime('YYYY-MM-DD hh.mm.ss', DateTime);                                           // http://www.delphibasics.co.uk/RTL.asp?Name=FormatDateTime
end;


function DateTimeToStr_IO: string;
begin
 Result:= FormatDateTime('YYYY-MM-DD hh.mm.ss', Now);
end;





{--------------------------------------------------------------------------------------------------
   GET FILE SIZE
--------------------------------------------------------------------------------------------------}
{ Returns the size of all files in a folder }
function GetFolderSize(aFolder: string; FileType: string= '*.*'; DigSubdirectories: Boolean= TRUE): Int64;
VAR
   i: Integer;
   TSL: TStringList;
begin
 Result:= 0;
 TSL:= ListFilesOf(aFolder, FileType, TRUE, DigSubdirectories);
 TRY
  for i:= 0 to TSL.Count-1 DO
   Result:= Result+ GetFileSize(TSL[i]);
 FINALLY
  FreeAndNil(TSL);
 END;
end;


{ Works with >4GB files
  Best
  Source: http://stackoverflow.com/questions/1642220/getting-size-of-a-file-in-delphi-2010-or-later }
function GetFileSize(const aFilename: String): Int64;
VAR
   info: TWin32FileAttributeData;
begin
 if GetFileAttributesEx(PWideChar(aFileName), GetFileExInfoStandard, @info)
 then Result:= Int64(info.nFileSizeLow) or Int64(info.nFileSizeHigh shl 32)
 else Result:= -1;
end;


{ Works with >4GB files ?
  NOT TESTED.
  Uses Win API.
  Source: ? }
function GetFileSize_(CONST aFilename: string): Int64;
VAR aHandle: THandle;
begin
 aHandle:= CreateFile(PChar(aFilename), GENERIC_READ, FILE_SHARE_READ, NIL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

 if aHandle = INVALID_HANDLE_VALUE
 then Result:= -1
 else
  begin
   GetFileSizeEx(aHandle, Result);
   FileClose(aHandle);
  end;
end;


{ Same as GetFileSize but returns the size in b/kb/mb/etc }
function GetFileSizeFormat(CONST sFilename: string): string;
begin
 if FileExists(sFilename)
 then Result:= ccCore.FormatBytes(GetFileSize(sFilename), 2)
 else Result:= '0 (file does not exist)';
end;







{--------------------------------------------------------------------------------------------------
   COMPARE
--------------------------------------------------------------------------------------------------}

{ Compare two files }
function CompareStreams(A, B: TStream; BufferSize: Integer = 4096): Boolean;  // Source: jcl
var
  BufferA, BufferB: array of Byte;
  ByteCountA, ByteCountB: Integer;
begin
  SetLength(BufferA, BufferSize);
  try
    SetLength(BufferB, BufferSize);
    try
      repeat
        ByteCountA := A.Read(BufferA[0], BufferSize);
        ByteCountB := B.Read(BufferB[0], BufferSize);

        Result := (ByteCountA = ByteCountB);
        Result := Result and CompareMem(BufferA, BufferB, ByteCountA);
      until (ByteCountA <> BufferSize) or (ByteCountB <> BufferSize) or not Result;
    finally
      SetLength(BufferB, 0);
    end;
  finally
    SetLength(BufferA, 0);
  end;
end;


{ Compare two files }
function CompareFiles(CONST FileA, FileB: TFileName; BufferSize: Integer = 4096): Boolean;
VAR A, B: TStream;
begin
  A:= TFileStream.Create(FileA, fmOpenRead or fmShareDenyWrite);
  TRY
    B:= TFileStream.Create(FileB, fmOpenRead or fmShareDenyWrite);
    TRY
      Result := CompareStreams(A, B, BufferSize);
    FINALLY
      FreeAndNil(B);
    end;
  FINALLY
    FreeAndNil(A);
  end;
end;


{Set the 'c' attibute on the specified file.
  As a result, the file will be compressed.
  Source: http://stackoverflow.com/questions/7002575/how-can-i-set-a-files-compression-attribute-in-delphi }






{--------------------------------------------------------------------------------------------------
  FILE ACCESS
  Also see: IsDiskWriteProtected
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


{ Returns true if the file cannot be open for reading and writing }                                           { old name: FileInUse }
function FileIsLockedRW(FileName: string): Boolean;
VAR hFileRes: HFILE;
begin
 if NOT FileExists(FileName) then EXIT(FALSE);                       { If files doesn't exist it cannot be locked! }

 hFileRes := CreateFile(PChar(FileName), GENERIC_READ OR GENERIC_WRITE, 0, NIL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
 Result := (hFileRes = INVALID_HANDLE_VALUE);
 if NOT Result then CloseHandle(hFileRes);
end;


function FileIsLockedR(FileName: string): Boolean;                   { Returns true if the file cannot be open for reading }
VAR hFileRes: HFILE;
begin
 if NOT FileExists(FileName)
 then RAISE exception.Create('File does not exist!'+ crlf+ FileName);

 hFileRes := CreateFile(PChar(FileName), GENERIC_READ, 0, NIL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
 Result := (hFileRes = INVALID_HANDLE_VALUE);
 if NOT Result then CloseHandle(hFileRes);
end;


{ Do we have write access to this file/folder? }
function TestWriteAccessMsg(CONST FileOrFolder: string): Boolean;
begin
 Result:= TestWriteAccess(FileOrFolder);
 if NOT Result
 then MesajWarning(ShowMsg_CannotWriteTo(FileOrFolder));
end;


{ Returns true if it can write that file to disk. ATTENTION it will overwrite the file if it already exists ! }
function TestWriteAccess(FileOrFolder: string): Boolean;
VAR myFile: FILE;
    wOldErrorMode: Word;                                                 { Stop Windows from Displaying Critical Error Messages:   http://delphi.about.com/cs/adptips2002/a/bltip0302_3.htm }
    CreateNew: Boolean;
begin
 Result:= DirectoryExists( ExtractFilePath(FileOrFolder) );
 {       AICI AR TREBUIE SA FORTEZ DIRECTORUL DACA DRIVE-UL E READY }
 if NOT Result then Exit;

 wOldErrorMode:= SetErrorMode( SEM_FAILCRITICALERRORS );                 { tell windows to ignore critical errors and save current error mode }
 TRY
  if IsFolder(FileOrFolder)
  then FileOrFolder:= Trail(FileOrFolder)+ 'WriteAccessTest.DeleteMe';

  Assign(myFile, FileOrFolder);
  FileMode:= fmOpenReadWrite;                                            { Read/Write access }
  CreateNew:= NOT FileExists(FileOrFolder);

  if CreateNew                                                           { The file does not exist, I need to create one }
  then
    begin
     {$I-}
     Rewrite(myFile);                                                    { Creates a new file and opens it. If an external file with the same name already exists, it is deleted and a new empty file is created in its place. }
     {$I+}
    end
  else
    begin
     {$I-}
     Reset(myFile);                                                      { Opens an existing file. Just open it and close it. I don't write nothing to it. }
     {$I+}
    end;

  Result:= (IOResult = 0);
  if Result then
   begin
    Close(myFile);
    if CreateNew
    then System.SysUtils.DeleteFile(FileOrFolder);                       { I created, so I delete it. }
   end;
 FINALLY
   SetErrorMode( wOldErrorMode );                                        { go back to previous error mode }
 END;
end;


{ Same as TestWriteAccess
  Source: https://stackoverflow.com/questions/3599256/how-can-i-use-delphi-to-test-if-a-directory-is-writeable }
function IsDirectoryWriteable(const Dir: string): Boolean;
var
  FileName: String;
  H: THandle;
begin
  FileName := IncludeTrailingPathDelimiter(Dir) + 'IsDirectoryWriteable_Check.tmp';
  if FileExists(FileName)
  AND NOT DeleteFile(FileName)
  then Raise Exception.Create('File is locked!'+ CRLF+ FileName);

  H := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, 0, NIL, CREATE_NEW, FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE, 0);
  Result:= H <> INVALID_HANDLE_VALUE;
  if Result
  then CloseHandle(H);

  DeleteFile(FileName);
end;


function CanCreateFile(AString:String): Boolean;
VAR h : integer;
begin
   h := CreateFile(Pchar(AString), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
   Result:= h>= 0;
   if Result then FileClose(h);
end;





{--------------------------------------------------------------------------------------------------
   SPECIAL FOLDERS
--------------------------------------------------------------------------------------------------}

function GetProgramFilesDir: string;
    (*
    { This function is copied from ccRegistry, but we don't want to depend on that unit, so... }
    function RegReadString (CONST Root: HKEY; CONST Key, ValueName: string; DefValData: String= ''): string;
    VAR Rg: TRegistry;
    begin
     Result:= '';
     Rg:= TRegistry.Create(KEY_READ);
     TRY
       Rg.RootKey:= Root;
       if  Rg.OpenKey(Key, FALSE)
       AND Rg.ValueExists(ValueName)
       then Result:= Rg.ReadString(ValueName)
       else Result:= DefValData;
       Rg.CloseKey;
     FINALLY
       FreeAndNil(Rg);
     END;
    end;    *)

begin
 Result:= Trail(RegReadString(HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\Windows\CurrentVersion', 'ProgramFilesDir'));
end;


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





function GetMyDocuments: String;
begin
 Result:= Trail(TPath.GetDocumentsPath);
end;


function GetMyPictures: string;
begin
 Result:= Trail(TPath.GetPicturesPath);
end;


function GetMusicPath: string;
begin
 Result:= Trail(TPath.GetMusicPath);
end;


function GetMoviesPath: string;
begin
 Result:= Trail(TPath.GetMoviesPath);
end;


function GetDownloadsPath: string;
begin
 Result:= Trail(TPath.GetDownloadsPath);
end;


function GetDesktopFolder: string;
begin
 Result:= Trail(GetSpecialFolder(CSIDL_DESKTOPDIRECTORY));
end;


function GetTempFolder: String;
begin
 Result:= TPath.GetTempPath;    /////////////  Trail(); ?
end;


{
  Returns either the home path of the user or the application's writable scratch directory or storage.

  You should use GetHomePath to store settings per user. For example: TFile.WriteAllText(TPath.GetHomePath() + TPath.DirectorySeparatorChar + 'sample.txt', s);

  On Windows        points to the user’s application data folder.
  On Linux & OS X   points to the user’s home folder, as defined by the $(HOME) environment variable.
  On iOS & Android  points to the device-specific location of the sandbox for the application; the iOS home location is individually defined for each application instance and for each iOS device.

  Windows XP       C:\Documents and Settings\<username>\Application Data  CSIDL_APPDATA
  Windows Vista+   C:\Users\<username>\AppData\Roaming                    FOLDERID_RoamingAppData
  OS X             /Users/<username>
  iOS Device       /private/var/mobile/Containers/Data/Application/<application ID>
  iOS Simulator    /Users/<username>/Library/Developer/CoreSimulator/Devices/<Device ID>/data/Containers/Data/Application/<application ID>
  Android          /data/data/<application ID>/files
  Linux            /home/<username>  Home Folder }
function GetHomePath: string;
begin
 Result:= TPath.GetHomePath;
end;

function GetDocumentsPath: string;
begin
 Result:= TPath.GetDocumentsPath;
end;

function GetSharedDocumentsPath: string;
begin
 Result:= TPath.GetDocumentsPath;
end;


{
  Returns the path to a directory to store any data that your application needs store, regardless of the user, such as files, caches, resources, and preferences.

  Windows,      it points to the folder that contains the executable file.  C:\Program Files\<application folder>
  OSX & iOS,    it points to the library directory.
  Android,      it points to the device-specific location of the sandbox for the application; the iOS home location is individually defined for each application instance and for each iOS device. }
function GetLibraryPath: string;
begin
 Result:= TPath.GetLibraryPath;
end;


{
  Returns the path to the directory where your application can store cache files.

  On Windows and OS X - points to a user-specific, application-agnostic directory.
  On iOS and Android  - points to an application-specific, user-specific directory.

  Windows XP            C:\Documents and Settings\<username>\Local Settings\Application Data  CSIDL_LOCAL_APPDATA
  Windows Vista+        C:\Users\<username>\AppData\Local                                     FOLDERID_LocalAppData }
function GetCachePath: string;
begin
 Result:= TPath.GetCachePath;
end;


{
  PublicPath returns the path to the directory where you can store application data that can be shared with other applications.
  Note:
    In desktop applications, "shared" means "shared between different users".
    In mobile applications,  "shared" means "shared between different applications".

  Windows       it points to a system-wide directory.
  OS X          it points to a user-specific, application-agnostic directory.
  iOS Device    it returns an empty string as this directory is currently not supported.
  iOS Simulator it points to an application-specific, user-specific directory.
  Android       it points to an application-specific, user-specific directory.

  Windows XP       C:\Documents and Settings\All Users\Application Data       CSIDL_COMMON_APPDATA
  Windows Vista+   C:\ProgramData                                             FOLDERID_ProgramData
  OS X             /Users/<username>/Public
  iOS Device       -
  iOS Simulator    /Users/<username>/Library/Developer/CoreSimulator/Devices/<Device ID>/data/Containers/Data/Application/<application ID>/Public  NSSharedPublicDirectory
  Android          /storage/emulated/0/Android/data/<application ID>/files            }
function GetPublicPath: string;
begin
 Result:= TPath.GetPublicPath;
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


{ Retrievwa the shell folders from registry }
function GetSpecialFolder (OS_SpecialFolder: string): string;
{ DOCS:
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

{
function GetSpecialFolder_old (CSIDL: Integer): WideString;                                        { DEL
CONST SHGFP_TYPE_CURRENT = 0;
VAR path: array [0..Max_Path] of char;
begin
 if ShGetFolderPath(0, CSIDL, 0, SHGFP_TYPE_CURRENT, @path[0])= S_ok
 then Result:= Trail (Path)
 else Result:= '';
end;     }


{--------------------------------------------------------------------------------------------------
 uses SHFolder
 As recommended by Borland in this doc: VistaUACandDelphi.pdf
 Minimum operating systems: Windows 95 with Internet Explorer 5.0, Windows 98 with Internet Explorer 5.0, Windows 98 Second Edition (SE), Windows NT 4.0 with Internet Explorer 5.0, Windows NT 4.0 with Service Pack 4 (SP4)

 SPECIAL FOLDERS CONSTANTS:
   Full list of 'Special folder constants' are available here:  http://msdn.microsoft.com/en-us/library/bb762494(VS.85).aspx
   'Special folder constants' are declared in ShlObj and SHFolder (as duplicates). SHFolder.pas is the interface for Shfolder.dll.
--------------------------------------------------------------------------------------------------}
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
end;


{ Get a list of ALL special folders. Used by Uninstaller. }
function GetSpecialFolders: TStringList;
begin
 Result:= TStringList.Create;                                                          //  FROM ShlObj.pas
 Result.Add(GetSpecialFolder(CSIDL_DESKTOP                 , FALSE));                  // <desktop>
 Result.Add(GetSpecialFolder(CSIDL_INTERNET                , FALSE));                  // Internet Explorer (icon on desktop)
 Result.Add(GetSpecialFolder(CSIDL_PROGRAMS                , FALSE));                  // Start Menu\Programs
 Result.Add(GetSpecialFolder(CSIDL_CONTROLS                , FALSE));                  // My Computer\Control Panel
 Result.Add(GetSpecialFolder(CSIDL_PRINTERS                , FALSE));                  // My Computer\Printers
 Result.Add(GetSpecialFolder(CSIDL_PERSONAL                , FALSE));                  // My Documents
 Result.Add(GetSpecialFolder(CSIDL_FAVORITES               , FALSE));                  // <user name>\Favorites
 Result.Add(GetSpecialFolder(CSIDL_STARTUP                 , FALSE));                  // Start Menu\Programs\Startup
 Result.Add(GetSpecialFolder(CSIDL_RECENT                  , FALSE));                  // <user name>\Recent
 Result.Add(GetSpecialFolder(CSIDL_SENDTO                  , FALSE));                  // <user name>\SendTo
 Result.Add(GetSpecialFolder(CSIDL_BITBUCKET               , FALSE));                  // <desktop>\Recycle Bin
 Result.Add(GetSpecialFolder(CSIDL_STARTMENU               , FALSE));                  // <user name>\Start Menu
 Result.Add(GetSpecialFolder(CSIDL_MYDOCUMENTS             , FALSE));                  // Personal was just a silly name for My Documents
 Result.Add(GetSpecialFolder(CSIDL_MYMUSIC                 , FALSE));                  // "My Music" folder
 Result.Add(GetSpecialFolder(CSIDL_MYVIDEO                 , FALSE));                  // "My Videos" folder
 Result.Add(GetSpecialFolder(CSIDL_DESKTOPDIRECTORY        , FALSE));                  // <user name>\Desktop
 Result.Add(GetSpecialFolder(CSIDL_DRIVES                  , FALSE));                  // My Computer
 Result.Add(GetSpecialFolder(CSIDL_NETWORK                 , FALSE));                  // Network Neighborhood (My Network Places)
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
 Result.Add(GetSpecialFolder(CSIDL_CONNECTIONS             , FALSE));                  // Network and Dial-up Connections
 Result.Add(GetSpecialFolder(CSIDL_COMMON_MUSIC            , FALSE));                  // All Users\My Music
 Result.Add(GetSpecialFolder(CSIDL_COMMON_PICTURES         , FALSE));                  // All Users\My Pictures
 Result.Add(GetSpecialFolder(CSIDL_COMMON_VIDEO            , FALSE));                  // All Users\My Video
 Result.Add(GetSpecialFolder(CSIDL_RESOURCES               , FALSE));                  // Resource Direcotry
 Result.Add(GetSpecialFolder(CSIDL_RESOURCES_LOCALIZED     , FALSE));                  // Localized Resource Direcotry
 Result.Add(GetSpecialFolder(CSIDL_COMMON_OEM_LINKS        , FALSE));                  // Links to All Users OEM specific apps
 Result.Add(GetSpecialFolder(CSIDL_CDBURN_AREA             , FALSE));                  // USERPROFILE\Local Settings\Application Data\Microsoft\CD Burning
 Result.Add(GetSpecialFolder(CSIDL_COMPUTERSNEARME         , FALSE));                  // Computers Near Me (computered from Workgroup membership)
 Result.Add(GetSpecialFolder(CSIDL_FLAG_CREATE             , FALSE));                  // combine with CSIDL_ value to force folder creation in SHGetFolderPath
 Result.Add(GetSpecialFolder(CSIDL_FLAG_DONT_VERIFY        , FALSE));                  // combine with CSIDL_ value to return an unverified folder path
 Result.Add(GetSpecialFolder(CSIDL_FLAG_DONT_UNEXPAND      , FALSE));                  // combine with CSIDL_ value to avoid unexpanding environment variables
 Result.Add(GetSpecialFolder(CSIDL_FLAG_NO_ALIAS           , FALSE));                  // combine with CSIDL_ value to insure non-alias versions of the pidl
 Result.Add(GetSpecialFolder(CSIDL_FLAG_PER_USER_INIT      , FALSE));                  // combine with CSIDL_ value to indicate per-user init (eg. upgrade)
 Result.Add(GetSpecialFolder(CSIDL_FLAG_MASK               , FALSE));                  // mask for all possible flag values
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


function GetTaskManager: String;
begin
 Result:= GetWinSysDir+ 'taskmgr.exe';
end;


function GetRandomFileName: string;
begin
 Result:= TPath.GetRandomFileName;
end;


function GetTempFileName: string;
begin
 Result:= TPath.GetTempFileName;
end;














{--------------------------------------------------------------------------------------------------
   PROCESS FOLDER STRING
--------------------------------------------------------------------------------------------------}

{ It accepts both incomplete and complete paths (only folder and folder + filename).
  NOTE! When a dir path is provided (without file name) it MUST end with a '\' separator else the function will treat the last folder as being the file name (without extension)!!!
  Example: 'c:\windows\system\me.txt' returns 'system'
  Works with UNC paths! }
function ExtractLastFolder(FullPath: string): string;
VAR i: Integer;
begin
 Result:= '';
 FullPath:= ExtractFilePath(FullPath);
 FullPath:= RemoveLastChar(FullPath);

 for i:= Length(FullPath) downto 1 DO                { Find first \ starting from the end of the string }
  if FullPath[i]= '\'
  then EXIT(system.COPY(FullPath, i+1, MaxInt));
end;


{ For c:\1\2\3\ returns c:\1\2
  http://stackoverflow.com/questions/22640879/how-to-get-path-to-the-parent-folder-of-a-certain-directory }
function ExtractParentFolder(Folder: string): string;
begin
 Result:= TDirectory.GetParent(ExcludeTrailingPathDelimiter(Folder))
end;


{ For 'c:\1\2\3\' returns '1\'.
  For 'c:\1' it returns ''.
  For '\1\' returns the same.
  http://stackoverflow.com/questions/22640879/how-to-get-path-to-the-parent-folder-of-a-certain-directory }
function ExtractFirstFolder(Folder: string): string;
VAR iPos: Integer;
begin
 iPos:= Pos(':\', Folder);
 if iPos > 0
 then Result:= system.COPY(Folder, iPos+2, MaxInt)
 else Result:= Folder;

 Result:= CopyTo(Result, 1, '\', TRUE, TRUE, 2); {  copy until the first \ }
end;


{ Does the same as the function above but it accepts a file as input.
  Works with UNC paths

  Also exists:
     IOUtils.TDirectory.GetParent.
     But is simply sucks. See http://stackoverflow.com/questions/35429699/system-ioutils-tdirectory-getparent-odd-behavior
     Also GetParent raises an exception if the given path is invalid or the directory DOES NOT exist
     So stick with TrimLastFolder.
 }
function TrimLastFolder(CONST DirPath: string): string;  { Example: for 'c:\windows\system' returns only 'c:\windows\'. It also works with incomplete paths like 'windows\system' }
VAR i: Integer;
begin
{$IFDEF MSWINDOWS}
 Result:= ExcludeTrailingPathDelimiter(DirPath);
 i:= Length(Result);
 WHILE (i> 0) AND (Result[i]<> '\')                      { Search from tail to head until you find the first delimiter }
  DO Dec(i);

 Result:= Trail(CopyTo(Result, 1, i));
{$ELSE}
 Not available on Mac!
{$ENDIF}
end;                                                     { Could also be implemented as: Copy(RootFull, 1, LastPos('\', RootFull)- 1) }











{--------------------------------------------------------------------------------------------------
   READ/WRITE TEXT
   Also see: ccStreamFile.pas.StringFromFileStart
--------------------------------------------------------------------------------------------------}

{ Write Unicode strings to a UTF8 file.
  FileName must be a fulll path. If the path does not exist it is created.
  It can also write a preamble.
  Based on: http://stackoverflow.com/questions/35710087/how-to-save-classic-delphi-string-to-disk-and-read-them-back/36106740#36106740  }
procedure StringToFile(CONST FileName: string; CONST aString: String; CONST WriteOp: TWriteOperation= woOverwrite; WritePreamble: Boolean= FALSE);
VAR
   Stream: TFileStream;
   Preamble: TBytes;
   sUTF8: RawByteString;
   aMode: Integer;
begin
 ForceDirectories(ExtractFilePath(FileName));

 if (WriteOp= woAppend) AND FileExists(FileName)
 then aMode := fmOpenReadWrite
 else aMode := fmCreate;

 Stream := TFileStream.Create(filename, aMode, fmShareDenyWrite);   { Allow read during our writes }
 TRY
  sUTF8 := Utf8Encode(aString);                                     { UTF16 to UTF8 encoding conversion. It will convert UnicodeString to WideString }

  if (aMode = fmCreate) AND WritePreamble then
   begin
    preamble := TEncoding.UTF8.GetPreamble;
    Stream.WriteBuffer( PAnsiChar(preamble)^, Length(preamble));
   end;

  if aMode = fmOpenReadWrite
  then Stream.Position:= Stream.Size;                               { Go to the end }

  Stream.WriteBuffer( PAnsiChar(sUTF8)^, Length(sUTF8) );
 FINALLY
   FreeAndNil(Stream);
 END;
end;


{ Tries to autodetermine the file type (ANSI, UTF8, UTF16, etc). Works with UNC paths.
  If it cannot detect the correct encoding automatically, we can force it to what we want by setting the second paramater.
      Example: System.SysUtils.TEncoding.UTF8
      However this is buggy! It will raise an exception if the file is ANSI but it contains high characters such as ½ (#189)
      See: https://stackoverflow.com/questions/35708827/what-could-cause-no-mapping-for-the-unicode-character-exists-in-the-target-mult }
function StringFromFile(CONST FileName: string; Enc: TEncoding= NIL): String;
begin
  if Enc= NIL
  then Result:= System.IOUtils.TFile.ReadAllText(FileName)
  else Result:= System.IOUtils.TFile.ReadAllText(FileName, Enc);
end;


{ Read a WHOLE file and return its content as AnsiString.
  The function will not try to autodetermine file's type.
  It will simply read the file as ANSI.

  Also see this: http://www.fredshack.com/docs/delphi.html }
function StringFromFileA(CONST FileName: string): AnsiString;
VAR Stream: TFileStream;
begin
 Result:= '';

 Stream:= TFileStream.Create(FileName, fmOpenRead OR fmShareDenyNone);
 TRY
   if Stream.Size>= High(Longint) then
    begin
     MesajError('File is larger than 2GB! Only files below 2GB are supported.'+ CRLF+ FileName);
     EXIT;
    end;

   SetString(Result, NIL, Stream.Size);
   Stream.ReadBuffer(Pointer(Result)^, Stream.Size);
 FINALLY
   FreeAndNil(Stream);
 END;
end;


procedure StringToFileA (CONST FileName: string; CONST aString: AnsiString; CONST WriteOp: TWriteOperation);
VAR
   Stream: TFileStream;
   aMode: Integer;
begin
 ForceDirectories(ExtractFilePath(FileName));

 if (WriteOp= woAppend) AND FileExists(FileName)
 then aMode := fmOpenReadWrite
 else aMode := fmCreate;

 Stream := TFileStream.Create(filename, aMode, fmShareDenyWrite);   { Allow read during our writes }
 TRY
  if aMode = fmOpenReadWrite
  then Stream.Position:= Stream.Size; { Go to the end }

  Stream.WriteBuffer( PAnsiChar(aString)^, Length(aString) );
 FINALLY
   FreeAndNil(Stream);
 END;
end;


procedure StringToFileA (CONST FileName: string; CONST aString: String; CONST WriteOp: TWriteOperation);
begin
 StringToFileA(FileName, AnsiString(aString), WriteOp);
end;


{ Read file IF it exists. Otherwise, return '' }
function StringFromFileExists(CONST FileName: string): String;          // Works with UNC paths
begin
 if FileExists(FileName)
 then Result:= StringFromFile(FileName)
 else Result:= '';
end;


function StringFromFileTSL(CONST FileName: string; Enc: TEncoding= NIL): TStringList;    // Works with UNC paths
begin
 Result:= TStringList.Create;
 Result.Text:= StringFromFile(FileName, Enc);
end;




{-----------------------------------------------------------------------------------------------------------------------
   TEXT LINES
-----------------------------------------------------------------------------------------------------------------------}

{ Opens a LARGE text file and counts how many lines it has.
  It does this by loading a small portion of the file in a RAM buffer.
  Does not handle well Mac/Linux files!!!

  Speed test on 150MB file:
     TFileStream (this): 15ms
     TCubicBuffStream: 78ms
     Delphi's ReadLn: 8518ms
  Speed tester here: c:\MyProjects\Packages\CubicCommonControls-Testers\CountLines tester\Tester.exe

  Buffer speed:
     1KB   = 31ms
     32KB  = 14.7ms
     64KB  = 14.5ms   <-- BEST
     256KB = 15ms
     512KB = 15ms
     2MB   = 16ms
   Same speed for SSD and HDD.

  See also: http://www.delphipages.com/forum/showthread.php?t=201629 }
{TODO 2: use buffered file }

function CountLines(CONST Filename: string; CONST BufferSize: Cardinal= 128000): Int64;            { Source: http://www.delphipages.com/forum/showthread.php?t=201629 }
VAR
   FS: TFileStream;
   bytes: array of Byte;
   i, iRead: Integer;
begin
 Result := 0;
 Assert(FileExists(FileName));
 SetLength(Bytes, BufferSize);
 FS := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
 TRY
  iRead := FS.Read(bytes[0], BufferSize);
  while iRead > 0 do
   begin
    for i := 0 to iRead - 1 do
     if (bytes[i] = 10)
     then Inc(Result);

    iRead := FS.Read(bytes[0], BufferSize);
   end;

  if FS.Size > 0 then
   begin
    { see if last line ends with a linefeed character }
    if FS.Size >= BufferSize
    then FS.Position :=  FS.Size-BufferSize         // Update to XE7. It was: FS.Seek(-BufferSize, soFromEnd)
    else FS.Position := 0;

    iRead := FS.Read(bytes[0], BufferSize);
    i := iRead - 1;

    { skip bytes < 9 or equal to Ctrl+Z (26) }
    WHILE (i > -1) AND ((bytes[i] < 9) OR (bytes[i] = 26))
     DO Dec(i);

    if (i > -1) AND (bytes[i] <> 10)
    then Inc(Result);
   end;

 FINALLY
  FreeAndNil(FS);
 END;
end;












{-----------------------------------------------------------------------------------------------------------------------
   READ/WRITE TO A BINARY FILE
-----------------------------------------------------------------------------------------------------------------------}

{ Writes binary data in "Data" to disk file.
  Returns TRUE if eveything is OK }
function WriteBinFile (CONST FileName: string; CONST Data: TBytes; CONST Overwrite: Boolean= TRUE): Boolean;
VAR StreamFile: TFileStream;
    AccessType: Word;
begin
 if Overwrite
 then AccessType:= fmCreate
 else
   if FileExists(FileName)
   then AccessType:= fmOpenWrite
   else AccessType:= fmCreate;

 StreamFile:= TFileStream.Create(FileName, AccessType);    { <--------- EFCreateError:   Cannot create file "blablabla". Access is denied. }
 TRY
   StreamFile.Position:= StreamFile.Size;  { Jump at the end of the file }   //it was: Seek(StreamFile.Size, soFromCurrent);
   Result:= NOT StreamFile.Write(Data, Length(Data))= Length(Data);
 FINALLY
   FreeAndNil(StreamFile);
 END;
end;








{--------------------------------------------------------------------------------------------------
   FILE COPY/MOVE
--------------------------------------------------------------------------------------------------}

{ In this function you don't have to provide the full path for the second parameter but only the destination folder }
function FileCopyQuick(CONST From_FullPath, To_DestFolder: string; Overwrite: boolean): boolean;
begin
 Overwrite:= NOT Overwrite;
 Result:= CopyFile(PChar(From_FullPath), PChar(To_DestFolder+ ExtractFileName(From_FullPath)), Overwrite)
end;


function FileCopyTo(CONST sFrom, sTo: string; Overwrite: Boolean): boolean;
begin
 Overwrite:= NOT Overwrite;  { Bug: this crapy M$ function has the Owervrite parameter backwards, so we have to flip it. }
 Result:= CopyFile(PChar(sFrom), PChar(sTo), Overwrite);
end;


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


function DeleteFileWithMsg(const FileName: string): Boolean;
begin
 Result:= DeleteFile(FileName);
 if NOT Result
 then MesajError('Cannot delete file '+CRLF+ FileName);
end;







{--------------------------------------------------------------------------------------------------
   FOLDER COPY/MOVE
--------------------------------------------------------------------------------------------------}

{ Copy its CONTENT, all its files and subfolders.
  Returns how many files were not copied. So it returns 0 for 'ok'. }
function CopyFolder(CONST FromFolder, ToFolder : String; Overwrite: Boolean= True; FileType: string= '*.*'): integer;
VAR
  s, Dst : string;
  TSL: TStringList;
begin
 Result:= 0;
 Dst := Trail(ToFolder);
 ForceDirectories(Dst);

 TSL:= ListFilesOf(FromFolder, FileType, TRUE, DigSubdirectories);
 TRY
   for s in TSL do
     if NOT FileCopyTo(s, Dst+ ExtractFileName(s), Overwrite)
     then inc(Result);
  FINALLY
    FreeAndNil(TSL);
  end;
end;


{ Example:  MoveFolder('c:\Documents', 'C:\Backups') }
procedure MoveFolder(CONST FromFolder, ToFolder: String; SilentOverwrite: Boolean);      { Also see: http://www.swissdelphicenter.ch/en/showcode.php?id=152 }
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
     if MesajYesNo('Cannot move '+ FromFolder +'. Destination folder already exists:'+ ToFolder+ CRLF+ 'Press Yes to delete Destination folder. Press No to cancel the opperation.')
     then
      begin
       Deletefolder(ToFolder);
       TDirectory.Move(FromFolder, ToFolder);
      end;
end;


{Moves the content of the FromFolder to the destination folder. The destination folder MUST be an incomplete path!
 Returns the location where the folder was moved.
 Example:  MoveFolder('c:\Documents\NewDocuments', 'OldDocuments') will move the NewDocuments folder to 'c:\Documents\OldDocuments'}
function MoveFolderRel(CONST FromFolder, ToRelFolder: string; Overwrite: Boolean): string;
begin
 if Pos(':', ToRelFolder) > 0
 then Raise Exception.Create('The input folder cannot be a full path!'+ CRLF+ ToRelFolder);

 Result:= TrimLastFolder(FromFolder) + ToRelFolder;
 MoveFolder(FromFolder, Result, Overwrite);
end;


{ This is obsolete. Very slow. Intoarce un numar care arata cate fisiere nu au fost copiate (probleme) }
function MoveFolderSlow(CONST FromFolder, ToFolder: String; Overwrite: boolean): integer;
begin
 Result:= CopyFolder(FromFolder, ToFolder, Overwrite);
 DeleteFolder(FromFolder);
end;


{  Copy only CopyBytes bytes from the begining of the file.
   If destination exists it is overwriten.

   Extended version of JclFileUtils.BuildFileList:
   function parameter Path can include multiple FileMasks as: c:\aaa\*.pas; pro*.dpr; *.d??
   FileMask Seperator = ';'

   We could use also Indy WinApi.CopyFile: https://docs.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-copyfile?redirectedfrom=MSDN
   }
procedure CopyFilePortion(CONST SourceName, DestName: string; CONST CopyBytes: int64);
VAR FDst, FSrc: File;    {TODO 2: Use TFileStream instead of File }
    Buf: array[1..10*MB] of Byte;
    NumRead, NumWritten: Integer;
begin
 NumWritten:= 0;
 TRY
   { SOURCE FILE }
   AssignFile(FSrc, SourceName);
   FileMode:= fmOpenRead;                           { Access mode on files opened by the Reset procedure. Be sure to reset FileMode before calling Reset with a read-only file. }
   Reset     (FSrc, 1);                             { In Delphi code, Reset opens the existing external file with the name assigned to F using the mode specified by the global FileMode variable. An error results if no existing external file of the given name exists or if the file cannot be opened with the current file mode. If F is already open, it is first closed and then reopened. The current file position is set to the beginning of the file.  }

   { DESTINATION FILE }
   AssignFile(FDst, DestName);
   Rewrite(FDst, 1);                                { In Delphi code, Rewrite creates a new external file with the name assigned to F. F is a variable of any file type associated with an external file using AssignFile. RecSize is an optional expression that can be specified only if F is an untyped file. If F is an untyped file, RecSize specifies the record size to be used in data transfers. If RecSize is omitted, a default record size of 128 bytes is assumed. If an external file with the same name already exists, it is deleted and a new empty file is created in its place. }

   REPEAT
    BlockRead (FSrc, Buf, sizeof(Buf), NumRead);
    BlockWrite(FDst, Buf, NumRead, NumWritten);
   UNTIL (NumRead = 0) OR (NumWritten <> NumRead);

 FINALLY
   CloseFile(FDst);
   CloseFile(FSrc);
 END;
end;


{ Append Segment to Master.
  Separator is a text (ex CRLF) that will be added before Segment files if SeparatorFirst= true }
procedure AppendTo(CONST MasterFile, SegmentFile, Separator: string; SeparatorFirst: Boolean= TRUE);
VAR
   MasterStream, SegmentStream: TFileStream;
   UTF: UTF8String;

  procedure AddSeparator;
  begin
    if Separator > '' then
     begin
      UTF := UTF8String(Separator);
      MasterStream.WriteBuffer(UTF[1], Length(Separator));
     end;
  end;

begin
  Assert(FileExists(MasterFile) , 'MasterFile does not exist: ' + MasterFile);
  Assert(FileExists(SegmentFile), 'SegmentFile does not exist: '+ SegmentFile);
  Assert(NOT SameFileName(SegmentFile, MasterFile), 'SegmentFile = MasterFile!');

  MasterStream := TFileStream.Create(MasterFile, fmOpenWrite or fmShareExclusive);
  TRY
    SegmentStream:= TFileStream.Create(SegmentFile, fmOpenRead or fmShareDenyWrite);
    TRY
      MasterStream.Position:= MasterStream.Size;   { Move cursor at the end of the file }
      if SeparatorFirst                            { Add separator before Segment }
      then AddSeparator;

      MasterStream.CopyFrom(SegmentStream, 0);

      if NOT SeparatorFirst                        { Add separator after Segment }
      then AddSeparator;

    FINALLY
      FreeAndNil(SegmentStream);
    END;
  FINALLY
    FreeAndNil(MasterStream);
  END;
end;


{ Merge file 'Input1' and file 'Input2' in a new file 'Output'.
  So, the difference between this procedure and AppendTo is that this proc does not modify the original input file(s) }
procedure MergeFiles(CONST Input1, Input2, Output, Separator: string; SeparatorFirst: Boolean= TRUE);
begin
 FileCopyTo(Input1, Output, TRUE);
 AppendTo(Output, Input2, Separator, SeparatorFirst);
end;


function MergeAllFiles(CONST Folder, FileType, OutputFile, Separator: string; DigSubdirectories: Boolean= FALSE; SeparatorFirst: Boolean= TRUE): Integer;       { Merge all files in the specified folder. FileType can be something like '*.*' or '*.exe;*.bin'. Returns the number of files merged }    { Separator is a text (ex CRLF) that will be added AFTER each Segment file }
VAR TSL: TStringList;
    CurFile: String;
begin
 TSL:= ListFilesOf(Folder, FileType, TRUE, DigSubdirectories);
 TRY
   Result:= TSL.Count;

   if NOT FileExists(OutputFile)                           { The masterfile must exist otherwise 'AppendTo' will fail }
   then StringToFile(OutputFile, '', woOverwrite, FALSE);

   for CurFile in TSL
    DO AppendTo(OutputFile, CurFile, Separator, SeparatorFirst);
 FINALLY
  FreeAndNil(TSL);
 END;
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
  case Op of                                                                                       {set title for simple progress dialog}
    FO_COPY   : SHFileOpStruct.lpszProgressTitle := 'Copying...';
    FO_DELETE : SHFileOpStruct.lpszProgressTitle := 'Deleting...';
    FO_MOVE   : SHFileOpStruct.lpszProgressTitle := 'Moving...';
    FO_RENAME : SHFileOpStruct.lpszProgressTitle := 'Renaming...';
   end;
  OpResult := 1;
  TRY
    OpResult := SHFileOperation(SHFileOpStruct);
  FINALLY
    Result:= (OpResult = 0);                                                                       {report success / failure}
  END;
end;



{--------------------------------------------------------------------------------------------------
   DELETE FOLDER
--------------------------------------------------------------------------------------------------}
{ Deletes all files (only files!) in the specified folder and subfolders, but don't delete the folder itself or the subfolders.
  Works with UNC paths }
procedure EmptyDirectory(CONST Path: string);
begin
 if System.SysUtils.DirectoryExists(Path) then
  begin
   TDirectory.Delete(Path, TRUE);
   Sleep(80);        { We need a delay here because the TDirectory.Delete is asynchron. The function seems to return before it finished deleting the folder. Details: http://stackoverflow.com/questions/42809389/tdirectory-delete-seems-to-be-asynchronous?noredirect=1#comment72732153_42809389 }
   if ForceDirectories(Path) < 0
   then raise exception.Create('EmptyDirectory - Cannot reconstruct directory!');
  end;
end;


{ DeleteFolder should be silent if folder not found.
  Works with UNC paths. }
procedure DeleteFolder(CONST Path: string);
begin
 if DirectoryExists(Path)
 then TDirectory.Delete(Path, TRUE);
end;


{ Delete all empty folders / sub-folders (any sub level) under the provided "rootFolder".
  Works with UNC paths. }
procedure RemoveEmptyFolders(const RootFolder: string);
var
  SRec: TSearchRec;
  listDir: TStringList;
  cnt: integer;

  { List folder in TStringList }
  procedure GetFolder(Path : string) ;
  var
    sPath, sSearch: string;
    listSubDir: TStringList;
    cnt: Integer;
  begin
    sPath := IncludeTrailingPathDelimiter(Path) ;
    sSearch := sPath+'*.*';

    { Get folder from root }
    if FindFirst(sSearch, faAnyFile, SRec) = 0 then
    TRY
      repeat
        if ((SRec.Attr and faDirectory) = faDirectory) and (SRec.Name <> '.') and (SRec.Name <> '..') then
        begin
          listDir.Add(sPath+sRec.Name) ;
        end;
      until FindNext(SRec) <> 0;
    FINALLY
     System.SysUtils.FindClose(SRec);
    END;

    { Find SubDirs }
    listSubDir := TStringList.Create;
    TRY
      if FindFirst(sSearch, faAnyFile, SRec) = 0 then
      TRY
        REPEAT
          if ((SRec.Attr and faDirectory) = faDirectory) and (SRec.Name <> '.') and (SRec.Name <> '..')
          then listSubDir.Add(sPath + SRec.Name) ;
        UNTIL  FindNext(SRec) <> 0;
      FINALLY
       System.SysUtils.FindClose(SRec);
      END;

      for cnt := 0 to listSubDir.Count - 1 DO
        GetFolder(listSubDir[cnt]) ;

    FINALLY
       FreeAndNil ( listSubDir )
    END;
  end;

begin
  listDir:= TStringList.Create;
  TRY
    { List }
    GetFolder(RootFolder) ;

    { Sort}
    ListDir.Sort;

    { Delete }
    for cnt:= 0 to listDir.Count-1 DO
      if TDirectory.IsEmpty(listDir[cnt])
      then RemoveDir(listDir[cnt]);
  FINALLY
     FreeAndNil (listDir)
  END;
end;













{--------------------------------------------------------------------------------------------------
   LIST FILES/FOLDERS
--------------------------------------------------------------------------------------------------}

{ if DigSubdirectories is false, it will return only the top level directories, else it will return also the subdirectories of subdirectories.
  Works also with Hidden/System folders. Source Marco Cantu Delphi 2010 HandBook
  Works with UNC paths}
function ListDirectoriesOf(CONST aFolder: string; CONST ReturnFullPath, DigSubdirectories: Boolean): TStringList;
VAR
  i: Integer;
  strPath: string;
  pathList: system.Types.TStringDynArray;
begin
 if NOT System.IOUtils.TDirectory.Exists (aFolder)
 then RAISE exception.Create('Folder does not exist! '+ crlf+ aFolder);

 Result:= TStringList.Create;

 if DigSubdirectories
 then pathList:= TDirectory.GetDirectories(aFolder, TSearchOption.soAllDirectories, NIL)
 else pathList:= TDirectory.GetDirectories(aFolder, TSearchOption.soTopDirectoryOnly, NIL);
 for strPath in pathList
  DO Result.Add(Trail(strPath));  { Trail is mandatory for ExtractLastFolder to work properly }

 { Remove full path }
 if NOT ReturnFullPath then
  for i:= 0 to Result.Count-1 DO
   Result[i]:= ExtractLastFolder(Result[i]);
end;


function ListFilesAndFolderOf(CONST aFolder: string; CONST ReturnFullPath: Boolean): TStringList;
VAR
   i: Integer;
   s: string;
   List: system.Types.TStringDynArray;
begin
 if NOT System.IOUtils.TDirectory.Exists (aFolder)
 then RAISE Exception.Create('Folder does not exist! '+ CRLF+ aFolder);

 Result:= TStringList.Create;

 List:= TDirectory.GetDirectories(aFolder, TSearchOption.soTopDirectoryOnly, NIL);
 for s in List
  DO Result.Add(Trail(s));  { Trail is mandatory for ExtractLastFolder to work properly }

 SetLength(List, 0);
 List:= TDirectory.GetFiles (aFolder);
 for s in List DO
  if s <> ''
  then Result.Add(s);

 { Remove full path }
 if NOT ReturnFullPath then
  for i:= 0 to Result.Count-1 DO
   Result[i]:= ExtractLastFolder(Result[i]);
end;


{ If DigSubdirectories is false, it will return only the top level files,
  else it will return also the files in subdirectories of subdirectories.
  If FullPath is true the returned files will have full path.
  FileType can be something like '*.*' or '*.exe;*.bin'
  Will show also the Hidden/System files.
  Based on code from Marco Cantu Delphi 2010 HandBook.

  Works with UNC paths. }
function ListFilesOf(CONST aFolder, FileType: string; CONST ReturnFullPath, DigSubdirectories: Boolean): TStringList;
VAR
  i: Integer;
  s: string;
  SubFolders, FileList: TStringDynArray;
  MaskArray: TStringDynArray;
  Predicate: TDirectory.TFilterPredicate;

   procedure ListFiles(CONST aFolder: string);
   VAR strFile: string;
   begin
    Predicate:=
          function(const Path: string; const SearchRec: TSearchRec): Boolean
          VAR Mask: string;
          begin
            for Mask in MaskArray DO
              if System.Masks.MatchesMask(SearchRec.Name, Mask)
              then EXIT(TRUE);
            EXIT(FALSE);
          end;

    // Long paths will raise an EPathTooLongexception exception, so we simply don't process those folders
    if Length(aFolder) > MAXPATH
    then EXIT;

    FileList:= TDirectory.GetFiles (aFolder, Predicate);
    for strFile in FileList DO
     if strFile<> ''         { Bug somewhere here: it returns two empty entries ('') here. Maybe the root folder?  }
     then Result.Add(strFile);
   end;

begin
 { We need this in order to prevent the EPathTooLongexception (reported by some users) }
 if aFolder.Length >= MAXPATH then
  begin
   MesajError('Path is longer than '+ IntToStr(MAXPATH)+ ' characters!');
   EXIT(NIL);
  end;

 if NOT System.IOUtils.TDirectory.Exists (aFolder)
 then RAISE exception.Create('Folder does not exist! '+ CRLF+ aFolder);

 Result:= TStringList.Create;

 { Split FileType in subcomponents }
 MaskArray:= System.StrUtils.SplitString(FileType, ';');

 { Search the parent folder }
 ListFiles(aFolder);

 { Search in all subfolders }
 if DigSubdirectories then
  begin
   SubFolders:= TDirectory.GetDirectories(aFolder, TSearchOption.soAllDirectories, NIL);
   for s in SubFolders DO
     if ccIO.DirectoryExists(s)  { This solves the problem caused by broken 'Symbolic Link' folders }
     then ListFiles(s);
  end;

 { Remove full path }
 if NOT ReturnFullPath then
  for i:= 0 to Result.Count-1 DO
   Result[i]:= TPath.GetFileName(Result[i]);
end;



{ COUNT FILES }
{$IFDEF msWindows}
{$WARN SYMBOL_PLATFORM OFF}
function CountFilesInFolder(CONST Path: string; CONST SearchSubFolders, CountHidden: Boolean): Cardinal;  // Works with UNC paths
var
  StrArray     : system.Types.TStringDynArray;
  SearchOption : System.IOUtils.TSearchOption;
  Predicate    : TDirectory.TFilterPredicate;
begin
  if SearchSubFolders
  then SearchOption:= System.IOUtils.TSearchOption.soAllDirectories
  else SearchOption:= System.IOUtils.TSearchOption.soTopDirectoryOnly;

  Predicate:= function(const Path: string; const SearchRec: TSearchRec): Boolean
               begin
                Result := (SearchRec.Attr and faHidden)=0;
               end;

  if CountHidden
  then StrArray := System.IOUtils.TDirectory.GetFiles( Path, '*', SearchOption )              { Note: Raises exception here is path not found or UNC drive offline }
  else StrArray := System.IOUtils.TDirectory.GetFiles( Path, '*', SearchOption, Predicate);

  Result:= length(StrArray);
end;
{$WARN SYMBOL_PLATFORM On}
{$ENDIF}















{--------------------------------------------------------------------------------------------------
   DRIVE
--------------------------------------------------------------------------------------------------}
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


function GetDriveType(CONST Path: string): Integer;   { Path can be something like 'C:\' or '\\netdrive\'. The folder MUST be trailed!!!! }
begin
 Result:= winapi.windows.GetDriveType(PChar(Trail(Path)));                                   { Help page: https://msdn.microsoft.com/en-us/library/windows/desktop/aa364939%28v=vs.85%29.aspx }
end;


function GetDriveTypeS(CONST Path: string): string;
begin
 case GetDriveType(path) of
   DRIVE_UNKNOWN     : Result:= 'The drive type cannot be determined.';
   DRIVE_NO_ROOT_DIR : Result:= 'The root path is invalid'; // for example, there is no volume mounted at the specified path.
   DRIVE_REMOVABLE   : Result:= 'Drive Removable';
   DRIVE_FIXED       : Result:= 'Drive fixed';
   DRIVE_REMOTE      : Result:= 'Remote Drive';
   DRIVE_CDROM       : Result:= 'CD ROM Drive';
   DRIVE_RAMDISK     : Result:= 'RAM Drive';
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


function DriveProtected(CONST Drive: Char):  Boolean;                                               { Attempt to create temporary file on specified drive. If created, the temporary file is deleted. see: http://stackoverflow.com/questions/15312704/gettempfilename-creates-an-empty-file }
VAR
   Directory: string;
begin
 Directory := Drive + ':\TestDrive002964982363';
 Result:= NOT ForceDirectoriesB(Directory);
 if NOT Result
 then RemoveDir(Directory);
END;


{ Returns false if the drive letter is not in ['A'..'Z'] }
function ValidDriveLetter(CONST Drive: Char): Boolean;
begin
 Result:= CharInSet(Upcase(Drive), ['A'..'Z']);
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







{--------------------------------------------------------------------------------------------------
   DRIVE - Conversion
--------------------------------------------------------------------------------------------------}

{ Returns #0 for invalid or network paths }
function ExtractDriveLetter(CONST Path: string): char;
VAR s: string;
begin
 Result:= #0;
 s:= ExtractFileDrive(Path);                                                                       { If the given path contains neither style of path prefix, the result is an empty string. }
 if s<> '' then
   if CharInSet(s[1], FullAlfabet)                                                                 { We don't accept network paths (\\) }
   then Result:= UpCase(s[1]);
end;


{ Converts the drive letter to the number of that drive. Example drive "A:" is 1 and drive "C:" is 3 }
function Drive2Byte(CONST Drive: char): Byte;
begin
 Result:= ORD( UpCase(Drive) )- ORD('A')+ 1;                                                       { 'A'=1, 'B'=2,  }
end;


{ Converts the drive number to the letter of that drive. Example drive 1 is "A:" floppy }
function Drive2Char(CONST DriveNumber: Byte): Char;
begin
 Result:= Char( DriveNumber+ ORD('A')- 1);                                                         { 'A'=1, 'B'=2,  }
end;


function GetLogicalDrives: TStringDynArray;
begin
 Result:= System.IOUtils.TDirectory.GetLogicalDrives;
end;



















procedure SetCompressionAtr(const FileName: string; const CompressionFormat: USHORT= 1);
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
   SHORTEN TEXT
--------------------------------------------------------------------------------------------------}

{ Only show the start and the end of the path with ellipses in-between
  Also exists:
       FileCtrl.MinimizeName: Shortens a fully qualified path name so that it can be drawn with a specified length limit.
       cGraphics.DrawStringEllipsis }
function ShortenText(CONST LongPath: String; MaxChars: Integer): String;    //ok  Works with UNC paths   //old name ShortenPath
VAR TotalLength, FLength: Integer;
begin
  TotalLength:= Length(LongPath);
  if TotalLength > MaxChars then
  begin
   FLength:= (MaxChars Div 2) - 2;
   Result := system.COPY(LongPath, 0, fLength)
             + '...'
             + system.COPY(LongPath, TotalLength-fLength, TotalLength);
   end
  else Result:= LongPath;
end;




end.

