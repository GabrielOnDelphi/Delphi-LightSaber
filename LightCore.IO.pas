UNIT LightCore.IO;

{=============================================================================================================
   www.GabrielMoraru.com
   2025.10
   See Copyright file
--------------------------------------------------------------------------------------------------------------

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

    Cross-platform ready: stamped 2025.10
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
    WinApi.Windows.MAX_PATH is declared as MAX_PATH = 260;

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
     c:\MyProjects\LightSaber\UNC Tester\
==================================================================================================}

INTERFACE
//del {.$WARN UNIT_PLATFORM ON}   { OFF: Silence the 'W1005 Unit Vcl.FileCtrl is specific to a platform' warning }

USES
  System.Generics.Collections,
  System.Generics.Defaults,
  System.Diagnostics,
  System.Math,
  System.Masks,
  System.Types,
  System.StrUtils,
  System.IOUtils,
  System.SysUtils,
  System.Classes;

CONST
  DigSubdirectories = TRUE;
  UseFullPath       = TRUE;
  {$IFDEF MSWINDOWS}
   MAXPATH= 260-12;
  {$ELSE}
   MAXPATH= 4095;
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

   { GRAPH FILTERS }
   // Also see this FMX function: TBitmapCodecManager.GetFileTypes.
   JPG           = '*.jpg;*.jpeg;*.jpe;*.jp;*.jfif';
   JPGFtl        = 'JPEG|'+ JPG;

   JPG2k         = '*.J2K;*JPC;*.JP2';
   JPG2kFtl      = 'JPEG 2000|'+ JPG2k;

   JPGAll        = JPG+ ';'+ JPG2k;
   JPGAllFlt     = JPGFtl+ '|'+ JPG2kFtl;

   BmpType       = '*.BMP';   // Also you can use: GraphicExtension(TBitmap)
   BMPFtl        = 'Bitmaps|'+ BmpType;

   PNG           = '*.PNG';
   PNGFtl        = 'PNG|'+ PNG;

   GIF           = '*.GIF';
   GIFFtl        = 'GIF|'+ GIF;

   XMF           = '*.EMF;*.WMF';
   XMFFtl        = 'Microsoft EMF/WMF|'+ XMF;

   AllImg        = JPG+ ';'+ JPG2k+ ';'+ BmpType+ ';'+ PNG+ ';'+ GIF;
   AllImgFlt     = 'Images|'+ AllImg;

   AllImgFltLong =  AllImgFlt+ '|'+ JPGFtl+ '|'+ JPG2kFtl+ '|'+ BMPFtl+ '|'+ PNGFtl+ '|'+ GIFFtl+ '|'+ XMFFtl;



{--------------------------------------------------------------------------------------------------
   MULTIPLATFORM / LINUX PATHS
--------------------------------------------------------------------------------------------------}
 { Trail }
 function  TrailLinuxPathEx         (CONST Path: string): string;                                  { Adds / in front and at the end of the path }
 function  TrailLinuxPath           (CONST Path: string): string;                                  { Adds / at the end of the path }
 function  TrimLastLinuxSeparator   (CONST Path: string): string;

 { CONVERSION }
 function  Convert2LinuxPath        (CONST DosPath  : string): string;
 function  Convert2DosPath          (CONST LinuxPath: string): string;


{--------------------------------------------------------------------------------------------------
   EXISTS
--------------------------------------------------------------------------------------------------}
 function  IsFolder             (CONST FullPath: string): boolean;                                 { Tells if FullPath is a folder or file. THE FOLDER/FILE MUST EXISTS!!! }
 function  DirectoryExists      (CONST Directory: String; FollowLink: Boolean= TRUE): Boolean;     { Corectez un bug in functia 'DirectoryExists' care imi intoarce true pentru un folder care nu exista, de exemplu 'c:\Program Files\ '. Bug-ul apare cand calea se termina cu un caracter SPACE. }


{--------------------------------------------------------------------------------------------------
   VALIDITY
--------------------------------------------------------------------------------------------------}
 function  FileNameIsValid_     (CONST FileName: string): Boolean; deprecated 'Use System.IOUtils.TPath.HasValidFileNameChars instead.'
 function  PathNameIsValid      (CONST Path: string): Boolean;                                     { TPath.HasValidPathChars is bugged - Returns FALSE if the path contains invalid characters. Tells nothing about the existence of the folder }
 function  IsUnicode            (CONST Path: string): boolean;                                     { Returns True if this path seems to be UNICODE }


{--------------------------------------------------------------------------------------------------
   PROCESS PATH
--------------------------------------------------------------------------------------------------}
 function  ExtractLastFolder    (FullPath: string): string;                                        { exemplu pentru c:\windows\system intoarce doar 'system' }
 function  ExtractParentFolder  (CONST Folder: string): string;
 function  ExtractFirstFolder   (CONST Folder: string): string;                                    { For c:\1\2\3\ returns 1\.  From c:\1 it returns ''  }

 function  TrimLastFolder       (CONST DirPath: string): string;                                   { exemplu pentru c:\windows\system intoarce doar 'c:\windows\' }
 function  ExtractRelativePath_ (CONST FullPath, RelativeTo: string): string; deprecated 'Use System.SysUtils.ExtractRelativePath instead'                     { Returns truncated path, relative to 'RelativeTo'. Example:  ExtractRelativePath('c:\windows\system32\user32.dll', 'c:\windows') returns system32\user32.dll }

 function  ShortenFileName      (CONST FullPath: String; MaxLength: Integer= MAXPATH): string;     { Returns a valid path, but with shorter filename }
 function  CheckPathLength      (CONST FullPath: string; MaxLength: Integer= MAXPATH): Boolean;

 { Path delimiters }
 function  ForcePathDelimiters  (CONST Path, Delimiter: string; SetAtBegining, SetAtEnd: Boolean): string;  { Old name: UniversalPathDelimiters }
 function  Trail                (CONST Path: string): string;                                      { Replacement for includeTrailingPathDelimiter }

 //See also: SysUtil.SameFileName
 function  SameFolder(Path1, Path2: string): Boolean;                                              { Receives two folders. Ex:  C:\Test1\ and C:\teSt1 will return true }
 function  SameFolderFromFile(Path1, Path2: string): Boolean;                                      { Receives two partial or complete file names and compare their folders. Ex:  C:\Test1 and C:\teSt1\me.txt will return true }
 function  IsSubfolder(Path1: String; Path2: String): Boolean;


{--------------------------------------------------------------------------------------------------
   CREATE FOLDERS
--------------------------------------------------------------------------------------------------}
 procedure ForceDirectoriesE    (CONST Folder: string);
 function  ForceDirectoriesB    (CONST Folder: string): Boolean;                                 { Replacement for System.SysUtils.ForceDirectories - elimina problema: " { Do not call ForceDirectories with an empty string. Doing so causes ForceDirectories to raise an exception" }
 function  ForceDirectories     (CONST Folder: string): Integer;


{--------------------------------------------------------------------------------------------------
   FIX PATH
--------------------------------------------------------------------------------------------------}{ Old name:  RemoveInvalidPathChars  }
 function  CorrectFolder        (CONST Folder  : string; ReplaceWith: char= ' '): string;          { Folder is single folder. Example '\test\' }
 function  CorrectFilename      (CONST FileName: string; ReplaceWith: char= ' '): string;          { Correct invalid characters in a filename. FileName = File name without path }
 function  ShortenText          (CONST LongPath: String; MaxChars: Integer): String;               { Also exists: FileCtrl.MinimizeName, DrawStringEllipsis }


{--------------------------------------------------------------------------------------------------
   SPECIAL FOLDERS
--------------------------------------------------------------------------------------------------}
 function  GetTempFolder         : string;
 function  GetMyDocuments        : string;                                                         { See this for macosx: http://www.malcolmgroves.com/blog/?p=865 }
 function  GetMyPictures         : string;

 { New: }
 function  GetHomePath: string;
 function  GetDocumentsPath: string;
 function  GetSharedDocumentsPath: string;
 function  GetMusicPath: string;
 function  GetMoviesPath: string;
 function  GetDownloadsPath: string;


{--------------------------------------------------------------------------------------------------
   LIST FOLDER CONTENT
--------------------------------------------------------------------------------------------------}
 function  ListDirectoriesOf   (CONST aFolder: string; CONST ReturnFullPath, DigSubdirectories: Boolean): TStringList;   { if DigSubdirectories is false, it will return only the top level directories, else it will return also the subdirectories of subdirectories. Returned folders are FullPath. Works also with Hidden/System folders }
 function  ListFilesAndFolderOf(CONST aFolder: string; CONST ReturnFullPath: Boolean): TStringList;
 function  ListFilesOf         (CONST aFolder, FileType: string; CONST ReturnFullPath, DigSubdirectories: Boolean; ExcludeFolders: TStrings= nil): TStringList;
 function  FolderIsEmpty       (CONST FolderName: string): Boolean;                               { Check if folder is empty }
 function  CountFilesInFolder  (CONST Path: string; CONST SearchSubFolders, CountHidden: Boolean): Cardinal;


{--------------------------------------------------------------------------------------------------
   FILE AUTO-NAME
--------------------------------------------------------------------------------------------------}
 function  IncrementFileNameEx  (CONST FileName: string; StartAt, NumberLength: Integer): string;  { Same sa IncrementFileName but it automatically adds a number if the file doesn't already ends with a number }
 function  IncrementFileName    (CONST FileName: string; AddDash: Boolean = false): string;        { Receives a file name that ends in a number. returns the same filename plus the number incremented with one. }
 function  MakeUniqueFolderName (CONST RootPath, FolderName: string): string;                      { Returns a unique path ended with a number. Old name:  Getnewfoldername }
 function  ChangeFilePath       (CONST FullFileName, NewPath: string): string;                     { Change file path to NewPath }
 function  AppendNumber2Filename(CONST FileName: string; StartAt, NumberLength: Integer): string;  { Add the number at the end of the filename. Example: AppendNumber2Filename('Log.txt', 1) will output 'Log1.txt' }
 function  FileEndsInNumber     (CONST FileName: string): Boolean;                                 { Returns true is the filename ends with a number. Example: MyFile02.txt returns TRUE }
 // function GetUniqueFileName: string;      //replaced by LightCore.GenerateUniqueString


{--------------------------------------------------------------------------------------------------
   CHANGE FILENAME
--------------------------------------------------------------------------------------------------}
 function  AppendToFileName    (CONST FileName, ApendedText: string): string;                      { Add a string between the name and the extension. Example: if I add the string ' backup' to the file 'Shell32.DLL' -> 'Shell32 backup.DLL' }
 function  AppendBeforeName    (CONST FullPath, ApendedText: string): string;                      { Add a string between the last directory separator and the beginning of the file name. Example: if I add the string ' backup' to the file 'c:\Shell32.DLL' -> 'c:\backup Shell32.DLL' -> 'c:\backup Shell32.DLL' -> 'c:\backup Shell32.DLL'. }
 function  ReplaceOnlyName     (CONST FileName, newName: string): string;                          { Replaces ONLY the name of a file (no extension) in a full path. Example: C:\Name.dll -> C:\NewName.dll }


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
 function  ForceExtension      (CONST FileName, Ext: string): string;                              { Makes sure that the 'FileName' file has the extension set to 'Ext'. The 'Ext' parameter should be like: '.txt' }
 function  ExtractFileExtUp    (CONST FileName: string): string;                                   { Case insensitive version }
 function  AppendFileExtension (CONST FileName, Ext: string): string;


{--------------------------------------------------------------------------------------------------
   FILE TYPE
--------------------------------------------------------------------------------------------------}
 function IsThisType    (CONST AFile, FileType: string) : Boolean;                                 { Returns true if the specified file is of the 'FileType' type }

 function IsVideo       (CONST AGraphFile: string) : Boolean;                                      { Video files supported by FFVCL (cFrameServerAVI) }
 function IsVideoGeneric(CONST AGraphFile: string) : Boolean;                                      { Generic video file detection. It doesn't mean I have support for all those files in my app }
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
 function IsImage (CONST AGraphFile: string) : Boolean;                                            { Returns TRUE if the file has a good/known extension and it can be converted to BMP }
 function IsImage2Bmp(CONST AGraphFile: string) : Boolean;

 function IsDfm   (CONST FileName  : string) : Boolean;
 function IsPas   (CONST FileName  : string) : Boolean;
 function IsDpr   (CONST FileName  : string) : Boolean;
 function IsDpk   (CONST FileName  : string) : Boolean;
 function IsDelphi(CONST FileName  : string) : Boolean;
 function IsExec  (CONST FileName  : string) : Boolean;

 function ExtensionToMimeType  (const FileName: string): string;   // Determines the MIME type of a file based on its extension
 function ExtensionFromMimeType(const MimeType: string): string;   // Determines the file extension based on a given MIME type


{--------------------------------------------------------------------------------------------------
   FILE BINARY COMPARE
--------------------------------------------------------------------------------------------------}
 function  CompareStreams     (A, B: TStream; BufferSize: Integer = 4096): Boolean;
 function  CompareFiles       (CONST FileA, FileB: TFileName; BufferSize: Integer = 4096): Boolean;


{--------------------------------------------------------------------------------------------------
   FILE MERGE
--------------------------------------------------------------------------------------------------}
 procedure CopyFileTop        (CONST SourceName, DestName: string; CopyBytes: Int64);                             { Copy only CopyBytes bytes from the begining of the file }
 procedure AppendTo           (CONST MasterFile, SegmentFile, Separator: string; SeparatorFirst: Boolean= TRUE);  { Append Segment to Master. Master must exists. }
 procedure MergeFiles         (CONST InpFile1, InpFile2, OutputFile, Separator: string;                                    SeparatorFirst: Boolean= TRUE);          overload;
 function  MergeFiles         (CONST Folder, FileType,   OutputFile, Separator: string; DigSubdirectories: Boolean= FALSE; SeparatorFirst: Boolean= TRUE): Integer; overload;


 { OTHERS }
 function  BytesToFile       (CONST FileName: string; CONST Data: TBytes; CONST Overwrite: Boolean= TRUE): Boolean;
{System.IOUtils.TFile.Encrypt https://docwiki.embarcadero.com/Libraries/Alexandria/en/System.IOUtils.TFile.Encrypt - Encrypt a given file using the operating system-provided facilities.}


{--------------------------------------------------------------------------------------------------
   COPY/MOVE
--------------------------------------------------------------------------------------------------}
 {COPY FILES}
 function  CopyFile           (CONST SourceName, DestName: string): Boolean;
 function  FileCopyQuick      (CONST From_FullPath, To_DestFolder: string): boolean;     { in this function you don't have to provide the full path for the second parameter but only the destination folder }

 {MOVE FILES}
 function  FileMoveTo         (CONST From_FullPath, To_FullPath  : string): Boolean;
 function  FileMoveToDir      (CONST From_FullPath, To_DestFolder: string): Boolean;

 {FOLDERS}
 function  CopyFolder         (CONST FromFolder, ToFolder   : String; Overwrite: Boolean= True; CONST FileType: string= '*.*'): integer;          { copy a folder and all its files and subfolders }
 function  MoveFolderRel      (CONST FromFolder, ToRelFolder: string; Overwrite: Boolean= True): string;
 procedure MoveFolder         (CONST FromFolder, ToFolder   : String; SilentOverwrite: Boolean= True);
 function  MoveFolderSlow     (CONST FromFolder, ToFolder   : String; Overwrite: boolean): Integer; deprecated 'Use TDirectory.Move() instead.';


{--------------------------------------------------------------------------------------------------
   BACKUP
--------------------------------------------------------------------------------------------------}
 function BackupFileIncrement (CONST FileName: string; CONST DestFolder: string= ''; const NewExtension: string= '.bak'): string; { Creates a copy of this file in the new folder.  Automatically increments its name. Returns '' in case of copy failure }
 function BackupFileBak       (CONST FileName: string): Boolean;                                                            { Creates a copy of this file, and appends as file extension. Ex: File.txt -> File.txt.bak }
 function BackupFileDate      (CONST FileName: string;             TimeStamp: Boolean= TRUE; Overwrite: Boolean = TRUE): Boolean;  overload;     { Create a copy of the specified file in the same folder. The '_backup' string is attached at the end of the filename }
 function BackupFileDate      (CONST FileName, DestFolder: string; TimeStamp: Boolean= TRUE; Overwrite: Boolean = TRUE): Boolean;  overload;


{--------------------------------------------------------------------------------------------------
   DELETE
--------------------------------------------------------------------------------------------------}
 procedure EmptyDirectory     (CONST Path: string);                                                          { Delete all files in the specified folder, but don't delete the folder itself. It will search also in subfolders }
 procedure DeleteFolder       (CONST Path: string);
 procedure RemoveEmptyFolders (CONST RootFolder: string);                                                    { NETESTATA! Delete all empty folders / sub-folders (any sub level) under the provided "rootFolder" }


{--------------------------------------------------------------------------------------------------
   FILE SIZE
--------------------------------------------------------------------------------------------------}
 function  GetFileSize      (CONST FileName: string): Int64;
 function  GetFileSizeFormat(CONST FileName: string): string;                                     { Same as GetFileSize but returns the size in b/kb/mb/etc }
 function  GetFolderSize    (CONST Folder: string; CONST FileType: string= '*.*'; DigSubdirectories: Boolean= TRUE): Int64;


{--------------------------------------------------------------------------------------------------
   FILE TIME
--------------------------------------------------------------------------------------------------}
 function  ExtractTimeFromFileName (CONST FileName: string): TTime;                                { The time must be at the end of the file name. Example: 'MyPicture 20-00.jpg'. Returns -1 if the time could not be extracted. }
 function  DateToStr_IO            (CONST DateTime: TDateTime): string;                            { Original name: StrTimeToSeconds_unsafe }
 function  TimeToStr_IO            (CONST DateTime: TDateTime): string;
 function  DateTimeToStr_IO        (CONST DateTime: TDateTime): string;  overload;                 { Used to conver Date/Time to a string that is safe to use in a path. For example, instead of '2013/01/01' 15:32 it will return '2013-01-01 15,32' }
 function  DateTimeToStr_IO: string;                                     overload;


{--------------------------------------------------------------------------------------------------
   DRIVES
--------------------------------------------------------------------------------------------------}
 function  ExtractDriveLetter (CONST Path: string): char; deprecated  'Use IOUtils.TDirectory.GetDirectoryRoot instead.'   { Returns #0 for invalid or network paths. GetDirectoryRoot returns something like:  'C:\' }
 { Validity }
 function  ValidDriveLetter   (CONST Drive: Char): Boolean;                                        { Returns false if the drive letter is not in ['A'..'Z'] }
 function  DriveProtected     (CONST Drive: Char): Boolean;                                        { Attempt to create temporary file on specified drive. If created, the temporary file is deleted. } {old name: IsDiskWriteProtected }
 { Convert }
 function  Drive2Byte         (CONST Drive: Char): Byte;                                           { Converts the drive letter to the number of that drive. Example drive "A:" is 1 and drive "C:" is 3 }
 function  Drive2Char         (CONST DriveNumber: Byte): Char;                                     { Converts the drive number to the letter of that drive. Example drive 1 is "A:" floppy }
 function  GetLogicalDrives: TStringDynArray;  inline;



IMPLEMENTATION

USES
  LightCore, LightCore.Time;


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
 then Result:= LightCore.RemoveLastChar(Path)
 else Result:= Path;
end;


{ Converts DOS path to Linux path. Does not handle C: but only the \ separators }
function Convert2LinuxPath(CONST DosPath: string): string;     // old name: MakeLinuxPath
begin
 Result:= ReplaceCharF(DosPath, '\', '/');;
end;



function Convert2DosPath(CONST LinuxPath: string): string;
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
 Result:= IncludeTrailingPathDelimiter(Path);
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


{ The order of the parameters does not matter: Path1 could be a subfolder or Path2 or the viceversa }
function IsSubfolder(Path1: String; Path2: String): Boolean;
var
  Length1: Integer;
  Length2: Integer;
begin
  Result := False;
  Length1 := Length(Path1);
  Length2 := Length(Path2);
  If (Length1>0) and (Length2>0) then
   begin
     Path1 := UpperCase(ExcludeTrailingPathDelimiter(Path1));
     Path2 := UpperCase(ExcludeTrailingPathDelimiter(Path2));
     If Length1 > Length2
     then Result := (Pos(Path2, Path1) = 1)
     else Result := (Pos(Path1, Path2) = 1)
   end;
end;


{ Returns a path that is not longer than MAX_PATH allowed in Windows
  It does this by shortening the filename.
  The caller must make sure that the resulted file name won't be too short (0 chars)!
  IMPORTANT! We cannot use TPath here because it cannot handle long file names. Details: http://stackoverflow.com/questions/31427260/how-to-handle-very-long-file-names?noredirect=1#comment50831709_31427260

  Also exists:
       FileCtrl.MinimizeName (if you require pixels)
       cGraphics.DrawStringEllipsis
       LightCore.ShortenString
       LightCore.IO.ShortenFileName
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








{--------------------------------------------------------------------------------------------------
   FOLDER VALIDITY
   Works with UNC paths
   Correct invalid characters in a path. Path = path with filename, like: c:\my docs\MyFile.txt
--------------------------------------------------------------------------------------------------}
function CorrectFolder(CONST Folder: string; ReplaceWith: Char): string;                                                                                                     { Old name: CorrectPath, RemoveInvalidPathChars }
VAR i: Integer;
    InvalidChars: TCharArray;
begin
 {TODO: Make it work with UNC paths! }
 InvalidChars := TPath.GetInvalidPathChars;
 Result:= Folder;
 for i:= 1 to Length(Result) DO
   if (Result[i] < ' ') or CharInArray(Result[i], InvalidChars)
   then Result[i]:= ReplaceWith;
end;


{
  Returns FALSE if the path is too short or contains invalid characters.
  Tells nothing about the existence of the folder.
  Note: TPath.HasValidPathChars is bugged. https://stackoverflow.com/questions/45346525/why-tpath-hasvalidpathchars-accepts-as-valid-char-in-a-path/45346869#45346869
}
function PathNameIsValid(CONST Path: string): Boolean;
begin
 {ToDo: Accept UNC paths like: \??\Windows. For this check for the \?? patern }
 Result := TPath.HasValidPathChars(Path, False);
end;


{ DOESN'T WORK WITH UNC PATHS !!!!!!!!!
  Deprecated 'Use System.IOUtils.TPath.HasValidFileNameChars instead.'
  HasValidFileNameChars only work with file names, not also with full paths. }
function FileNameIsValid_(CONST FileName: string): Boolean;
VAR i, Spaces: Integer;
    InvalidChars: TCharArray;
begin
 InvalidChars := TPath.GetInvalidFileNameChars;
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
   if CharInArray(FileName[i], InvalidChars)
   OR (Ord(FileName[i]) < 32)
   then EXIT(FALSE);
end;






{--------------------------------------------------------------------------------------------------
   FOLDER EXISTENCE
--------------------------------------------------------------------------------------------------}

{  This corrects a bug in the original 'DirectoryExists' which returns true for a folder that does not exist.
   The bug appears when the path ends with a SPACE. Exemple: 'c:\Program Files\ '
   Works with UNC paths! }
function DirectoryExists(CONST Directory: String; FollowLink: Boolean= TRUE): Boolean;
begin
  Result:= System.SysUtils.DirectoryExists(Directory, FollowLink)
  {$IFDEF MSWINDOWS}
       AND (LastChar(Directory)<> ' ')                                                              { Don't accept Space at the end of a path (after the backslash) }
  {$ENDIF}
end;






{--------------------------------------------------------------------------------------------------
   CREATE FOLDER

   Tries to create the specified folder. Does not crashes if
   Works with UNC paths.
   Writing on a readonly folder: It ignores the ReadOnly attribute (same for H and S attributes)

   Returns:
     False if the path is invalid.
     False if the drive is readonly.
--------------------------------------------------------------------------------------------------}
function ForceDirectoriesB(CONST Folder: string): Boolean;
begin
  TRY
    TDirectory.CreateDirectory(Folder);
  EXCEPT
    on EInOutError DO EXIT(FALSE);   // Handle I/O errors (e.g., no write permission)
    else RAISE;
    {
    For any other exceptions, raise.
    Example:
      on EArgumentException DO RAISE;  // Re-raise exception for invalid characters in path
      on EInOutArgumentException DO RAISE;  //   'Path is empty'.    }
  END;
  Result:= DirectoryExists(Folder);
end;

//
//Project Tester_LightCore.IO.exe raised exception class  with message

{--------------------------------------------------------------------------------------------------
   Raises exception if parameter is invalid
   Raises exception if parameter is empty
   Raises exception if drive is invalid
--------------------------------------------------------------------------------------------------}
procedure ForceDirectoriesE(CONST Folder: string);
begin
  TDirectory.CreateDirectory(Folder);
end;


// Works with UNC paths
function ForceDirectories(CONST Folder: string): Integer;
{RETURNS:
  -1 = Error creating the directory
   0 = Directory already exists
  +1 = Directory created succesfully  }
begin
  Assert(Folder> '', 'ForceDirectories - Parameter is empty!');
  if TDirectory.Exists(Folder)
  then Result:= 0
  else
    if ForceDirectoriesB(Folder)
    then Result:= +1
    else Result:= -1;
end;







{--------------------------------------------------------------------------------------------------
   DETECT FILE TYPE
--------------------------------------------------------------------------------------------------}

{ Returns true if the specified file is of the 'FileType' type.
  Example: Check if a file is BMP:
           IsThisType('c:\Test.bMP', 'BmP') will return true }
function IsThisType(CONST AFile, FileType: string) : Boolean;
VAR sExtension: string;
begin
  sExtension:= ExtractFileExt(AFile);
  Result:= SameText(sExtension, '.'+FileType)
end;


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



// MIME TYPE
function ExtensionToMimeType(const FileName: string): string;
begin
  var Ext:= LowerCase(ExtractFileExt(FileName));

  if Ext = '.txt'  then Result := 'text/plain' else
  if Ext = '.md'   then Result := 'text/markdown' else
  if Ext = '.pdf'  then Result := 'application/pdf' else
  if Ext = '.png'  then Result := 'image/png' else
  if Ext = '.gif'  then Result := 'image/gif' else
  if IsJpg(Ext)    then Result := 'image/jpeg'

  else Result := 'application/octet-stream'; // Default to octet-stream for unknown types
end;


function ExtensionFromMimeType(const MimeType: string): string;
begin
  if MimeType = 'text/plain'      then Result := '.txt' else
  if MimeType = 'text/markdown'   then Result := '.md' else
  if MimeType = 'application/pdf' then Result := '.pdf' else
  if MimeType = 'image/jpeg'      then Result := '.jpg' else
  if MimeType = 'image/png'       then Result := '.png' else
  if MimeType = 'image/gif'       then Result := '.gif'
  else Result := '';
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



// DELPHI

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
  If file already has an extension, it is replaced by the indicated one, if not, the new extension is added.
  If the file is called file.tar.gz, only GZ will be replaced.

  Compared to System.SysUtils.ChangeFileExt():
    1. The 'Ext' parameter may contain the dot, or not: '.txt' or 'txt'.
    2. ChangeFileExt accepts Ext to be empty which is not ok! }
function ForceExtension(CONST FileName, Ext: string): string;
begin
  if Ext = '' then RAISE Exception.Create('ForceExtension - No extension provided!');

  { Append new extension }
  Result:= ExtractFilePath(FileName)+ ExtractOnlyName(FileName);
  if Ext[1] = '.'
  then Result:= Result+ Ext
  else Result:= Result+ '.'+Ext;
end;


{ Replaces the old System.SysUtils.ExtractFileDir
  If AcceptInvalidPaths= True, it will not raise an exception when the path is empty or has invalid chars }
function ExtractFilePath(CONST FullPath: string; AcceptInvalidPaths: Boolean= TRUE): string;
begin
  if (FullPath > '')
  AND PathNameIsValid(FullPath)
  then Result:= Trail(TPath.GetDirectoryName(FullPath)) { WARNING: GetDirectoryName crashes when the filename is too long!!!!! }
  else
    if AcceptInvalidPaths
    then Result:= ''
    else RAISE Exception.Create('The path is invalid!' + CRLFw+ FullPath);    { GetDirectoryName shows an error message if the path is empty but the debuger won't stop. So I force the stop. }
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


{ Appends a new file extension to the FileName but only if the last extension is not already the same as Ext.
  In other words, if it alreayd has the wanted extension, then don't add it again.
  Example:  AppendFileExtension('MyFile.pas',     '.bak') returns 'MyFile.pas.bak'
            AppendFileExtension('MyFile.pas.bak', '.bak') returns 'MyFile.pas.bak' }
function AppendFileExtension (CONST FileName, Ext: string): string;
begin
  var CurExt:= ExtractFileExt(FileName);
  if SameText(CurExt, Ext)
  then Result:= FileName
  else Result:= FileName+ Ext;
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
function IncrementFileName (CONST FileName: string; AddDash: Boolean = false): string;                      // Works with UNC paths
VAR outFileName, outFileNumber: string;
begin
 SplitNumber_End(ExtractOnlyName(FileName), outFileName, outFileNumber);
 Result:= ExtractFilePath(FileName)+ outFileName+ IncrementStringNoEx(outFileNumber);
 if AddDash
 then Result:= Result+ '-'+ ExtractFileExt(FileName)
 else Result:= Result+ ExtractFileExt(FileName);
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
  Returns '' in case of failure }
function BackupFileIncrement (CONST FileName: string; CONST DestFolder: string= ''; const NewExtension: string= '.bak'): string;
begin
 Assert(FileExists(FileName));

 if DestFolder= ''
 then Result:= FileName                                       // Keep file in same folder
 else Result:= Trail(DestFolder)+ ExtractFileName(FileName);  // Build new path

 if DestFolder<> '' then
   if NOT ForceDirectoriesB(DestFolder)
   then EXIT('');

 Result:= AppendFileExtension(Result, NewExtension);

 WHILE FileExists(Result) DO { Increment file name until a file with same name does not exist anymore }
   Result:= IncrementFileNameEx(Result, 1, 3);

 TFile.Copy(FileName, Result, TRUE);
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

 TFile.Copy(FileName, BackupName, Overwrite);
 result:= TRUE;
end;


{ Create a copy of this file, and appends as file extension. Ex: File.txt -> File.txt.bak }
function BackupFileBak(CONST FileName: string): Boolean;
begin
 Result:= TRUE;
 TFile.Copy(FileName, FileName+'.bak', TRUE);
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
    InvalidChars: TCharArray;
begin
 InvalidChars := TPath.GetInvalidFileNameChars;
 Result:= FileName;

 for i:= 1 to Length(Result) DO
   if CharInArray(Result[I], InvalidChars)
   OR (Result[i] < ' ')
   then Result[i]:= ReplaceWith;

 Result:= Trim(Result);
end;











{--------------------------------------------------------------------------------------------------
   FILE TIME
--------------------------------------------------------------------------------------------------}

{ The time must be at the end of the file name.
  Example: 'MyPicture 20-00.jpg'.
  Returns 0 if the time could not be extracted. }
function ExtractTimeFromFileName(CONST FileName: string): TTime;
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
function GetFolderSize(CONST Folder: string; CONST FileType: string= '*.*'; DigSubdirectories: Boolean= TRUE): Int64;
VAR
   i: Integer;
   TSL: TStringList;
begin
 Result:= 0;
 TSL:= ListFilesOf(Folder, FileType, TRUE, DigSubdirectories);
 TRY
  for i:= 0 to TSL.Count-1 DO
   Result:= Result+ GetFileSize(TSL[i]);
 FINALLY
  FreeAndNil(TSL);
 END;
end;


{ Same as GetFileSize but returns the size in b/kb/mb/etc }
function GetFileSizeFormat(CONST FileName: string): string;
begin
 if FileExists(FileName)
 then Result:= LightCore.FormatBytes(GetFileSize(FileName), 2)
 else Result:= '0 (file does not exist)';
end;


{ It works with >4GB files.
  On error, return -1 instead of raising an exception }
function GetFileSize(CONST FileName: String): Int64;
begin
  try
    Result:= TFile.GetSize(FileName);
  except
    Result:= -1;
  end;
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
   SPECIAL FOLDERS
--------------------------------------------------------------------------------------------------}






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


function GetTempFolder: String;
begin
 Result:= TPath.GetTempPath;    //  Trail(); ?
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
  if FullPath[i]= PathDelim
  then EXIT(system.COPY(FullPath, i+1, MaxInt));
end;


{ For c:\1\2\3\ returns c:\1\2
  http://stackoverflow.com/questions/22640879/how-to-get-path-to-the-parent-folder-of-a-certain-directory }
function ExtractParentFolder(CONST Folder: string): string;
begin
 Result:= TDirectory.GetParent(ExcludeTrailingPathDelimiter(Folder))
end;


{ For 'c:\1\2\3\' returns '1\'.
  For 'c:\1' it returns ''.
  For '\1\' returns the same.
  http://stackoverflow.com/questions/22640879/how-to-get-path-to-the-parent-folder-of-a-certain-directory }
function ExtractFirstFolder(CONST Folder: string): string;
VAR iPos: Integer;
begin
 iPos:= Pos(':' + PathDelim, Folder);
 if iPos > 0
 then Result:= system.COPY(Folder, iPos+2, MaxInt)
 else Result:= Folder;

 Result:= CopyTo(Result, 1, PathDelim, TRUE, TRUE, 2); {  copy until the first \ }
end;


{ Does the same as the function above but it accepts a file as input.
  Works with UNC paths.
  Crossplatform.

  Also exists:
     IOUtils.TDirectory.GetParent.
     But is simply sucks. See http://stackoverflow.com/questions/35429699/system-ioutils-tdirectory-getparent-odd-behavior
     Also GetParent raises an exception if the given path is invalid or the directory DOES NOT exist
     So stick with TrimLastFolder.

  Example: for 'c:\windows\system' returns only 'c:\windows\'. It also works with incomplete paths like 'windows\system'
 }
function TrimLastFolder(const DirPath: string): string;
var
  TempPath: string;
  SeparatorPos: Integer;
begin
  TempPath := ExcludeTrailingPathDelimiter(DirPath); // Remove trailing delimiter if present
  SeparatorPos := LastDelimiter(TPath.DirectorySeparatorChar, TempPath); // Find the last directory separator
  if SeparatorPos > 0 then
    Result := IncludeTrailingPathDelimiter(Copy(TempPath, 1, SeparatorPos - 1)) // Get the path up to the last folder
  else
    Result := ''; // If no separator is found, return an empty string
end;







{-----------------------------------------------------------------------------------------------------------------------
   READ/WRITE TO A BINARY FILE
-----------------------------------------------------------------------------------------------------------------------}

{ Writes binary data in "Data" to disk file.
  Returns TRUE if eveything is OK }
function BytesToFile (CONST FileName: string; CONST Data: TBytes; CONST Overwrite: Boolean= TRUE): Boolean;    // Old name: WriteBinFile
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
function CopyFile(const SourceName, DestName: string): Boolean;
begin
  try
    TFile.Copy(SourceName, DestName, True); // Copy file, overwrite if destination exists
    Result:= TFile.Exists(DestName);       // Check if file exists at destination
  except
    Result:= FALSE;
  end;
end;


{ In this function you don't have to provide the full path for the second parameter but only the destination folder }
function FileCopyQuick(CONST From_FullPath, To_DestFolder: string): boolean;
begin
  Result:= CopyFile(From_FullPath, To_DestFolder+ ExtractFileName(From_FullPath));
end;


function FileMoveTo(CONST From_FullPath, To_FullPath: string): boolean;
begin
  Result:= TRUE;
  TFile.Move(From_FullPath, To_FullPath);
end;


{ Same as FileMoveTo but the user will provide a folder for the second parameter instead of a full path (folder = file name) } { Old name: FileMoveQuick }
{ If destination folder does not exists it is created }
function FileMoveToDir(CONST From_FullPath, To_DestFolder: string): boolean;
begin
 Result:= TRUE;
 ForceDirectories(To_DestFolder);
 TFile.Move(From_FullPath, Trail(To_DestFolder+ ExtractFileName(From_FullPath)));
end;





{--------------------------------------------------------------------------------------------------
   FOLDER COPY/MOVE
--------------------------------------------------------------------------------------------------}

{ Copy its CONTENT, all its files and subfolders.
  Returns how many files were not copied. So it returns 0 for 'ok'. }
function CopyFolder(CONST FromFolder, ToFolder : String; Overwrite: Boolean= True; CONST FileType: string= '*.*'): integer;
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
      TFile.Copy(s, Dst+ ExtractFileName(s), Overwrite);
   FINALLY
     FreeAndNil(TSL);
   end;
end;


{ Example: MoveFolder('c:\Documents', 'C:\Backups').
  It will overwrite all files in 'ToFolder' without asking.
  If you want feedback from user use LightVcl.Common.IO.Win.MoveFolderMsg }
procedure MoveFolder(CONST FromFolder, ToFolder: String; SilentOverwrite: Boolean);      { Also see: http://www.swissdelphicenter.ch/en/showcode.php?id=152 }
begin
  if NOT DirectoryExists(ToFolder) then EXIT;

  CopyFolder(FromFolder, ToFolder, SilentOverwrite);
  DeleteFolder(ToFolder);  { This is slow. Do a direct file move for each file. }
end;


{Moves the content of the FromFolder to the destination folder. The destination folder MUST be an incomplete path!
 Returns the location where the folder was moved.
 Example:  MoveFolder('c:\Documents\NewDocuments', 'OldDocuments') will move the NewDocuments folder to 'c:\Documents\OldDocuments'}
function MoveFolderRel(CONST FromFolder, ToRelFolder: string; Overwrite: Boolean): string;
begin
 if Pos(':', ToRelFolder) > 0
 then Raise Exception.Create('The input folder cannot be a full path!'+ CRLFw+ ToRelFolder);

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
   We could use also Indy WinApi.CopyFile: https://docs.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-copyfile?redirectedfrom=MSDN }
procedure CopyFileTop(const SourceName, DestName: string; CopyBytes: Int64);    //NOT TESTED YET!
const
  BufferSize = 10 * 1024 * 1024;  // 10 MB buffer
var
  SrcStream, DestStream: TFileStream;
  Buffer: TBytes;
  NumToCopy, TotalCopied: Int64;
begin
  SetLength(Buffer, BufferSize);  // Initialize buffer with the specified size
  SrcStream := TFileStream.Create(SourceName, fmOpenRead or fmShareDenyWrite);
  try
    DestStream := TFileStream.Create(DestName, fmCreate);
    try
      TotalCopied := 0;
      while TotalCopied < CopyBytes do
      begin
        NumToCopy := Min(BufferSize, CopyBytes - TotalCopied);
        SrcStream.ReadBuffer(Buffer[0], NumToCopy);
        DestStream.WriteBuffer(Buffer[0], NumToCopy);
        Inc(TotalCopied, NumToCopy);
      end;
    finally
      DestStream.Free;
    end;
  finally
    SrcStream.Free;
  end;
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


{ Merge only 2 files and output a third file.
  This procedure does not change the two input files, while the 'AppendTo' procedure does. }
procedure MergeFiles(CONST InpFile1, InpFile2, OutputFile, Separator: string; SeparatorFirst: Boolean= TRUE);
begin
 TFile.Copy(InpFile1, OutputFile, TRUE);
 AppendTo(OutputFile, InpFile2, Separator, SeparatorFirst);
end;


{ Merge all files in the specified folder.
  FileType can be something like '*.*' or '*.exe;*.bin' }
function MergeFiles(CONST Folder, FileType, OutputFile, Separator: string; DigSubdirectories: Boolean= FALSE; SeparatorFirst: Boolean= TRUE): Integer;       { Merge all files in the specified folder. FileType can be something like '*.*' or '*.exe;*.bin'. Returns the number of files merged }    { Separator is a text (ex CRLF) that will be added AFTER each Segment file }
VAR TSL: TStringList;
    CurFile: String;
    OutputStream: TFileStream;
begin
 TSL:= ListFilesOf(Folder, FileType, TRUE, DigSubdirectories);
 TRY
   Result:= TSL.Count;

   // The masterfile must exist otherwise 'AppendTo' will fail.
   // So, if it doesn't exists, we create it.
   if NOT FileExists(OutputFile) then
     TRY
       OutputStream:= TFileStream.Create(OutputFile, fmOpenWrite or fmShareExclusive);
     FINALLY
       FreeAndNil(OutputStream);
     END;

   for CurFile in TSL
    DO AppendTo(OutputFile, CurFile, Separator, SeparatorFirst);
 FINALLY
  FreeAndNil(TSL);
 END;
end;










{--------------------------------------------------------------------------------------------------
   DELETE FOLDER
--------------------------------------------------------------------------------------------------}
{ Deletes all files (only files!) in the specified folder and subfolders, but don't delete the folder itself or the subfolders.
  Works with UNC paths.

  We need a delay here because the TDirectory.Delete is asynchron.
  The function seems to return before it finished deleting the folder:
    Details: http://stackoverflow.com/questions/42809389/tdirectory-delete-seems-to-be-asynchronous?noredirect=1#comment72732153_42809389
    Answer: Perhaps the Remarks section on the msdn page about RemoveDirectory gives us a clue? (msdn.microsoft.com/en-us/library/windows/desktop/…) The RemoveDirectory function marks a directory for deletion on close. Therefore, the directory is not removed until the last handle to the directory is closed. This indicates that the call may return before the directory has actually been deleted }
procedure EmptyDirectory(const Path: string);
CONST
  MaxWaitTime = 6000; // Maximum time to wait for directory deletion (in milliseconds)
VAR
  Stopwatch: TStopwatch;
begin
  if System.SysUtils.DirectoryExists(Path) then
  begin
    TDirectory.Delete(Path, True);
    Stopwatch := TStopwatch.StartNew;
    while System.SysUtils.DirectoryExists(Path) do
    begin
      Sleep(10);   // Time to wait between checks (in milliseconds)
      if Stopwatch.ElapsedMilliseconds >= MaxWaitTime then
        RAISE Exception.Create('EmptyDirectory - Directory deletion timed out!'+ IntToStr(MaxWaitTime)+ ' sec');
    end;
    if NOT ForceDirectories(Path) < 0
    then RAISE Exception.Create('EmptyDirectory - Cannot reconstruct directory!');
  end;
end;


{ DeleteFolder should be silent if folder not found.
  Works with UNC paths. }
procedure DeleteFolder(CONST Path: string);
begin
 if DirectoryExists(Path)
 then TDirectory.Delete(Path, TRUE);
end;


{ Delete all empty folders / sub-folders (any sub level) under the provided "rootFolder". }
procedure RemoveEmptyFolders (CONST RootFolder: string);
var
  Dirs: TStringDynArray;
  SubDir: string;
begin
  Dirs:= TDirectory.GetDirectories(RootFolder, '*', TSearchOption.soAllDirectories);

  // Sort by length descending to delete deepest first
  TArray.Sort<string>(Dirs, TComparer<string>.Construct(
      function (const L, R: string): Integer
      begin
        Result := Length(R) - Length(L);
        if Result = 0
        then Result := CompareStr(R, L);
      end));

  for SubDir in Dirs do
    if TDirectory.IsEmpty(SubDir)
    then TDirectory.Delete(SubDir);
end;


{ Delete all empty folders / sub-folders (any sub level) under the provided "rootFolder".
  Works with UNC paths. }
procedure RemoveEmptyFolders_Alternative(const RootFolder: string);
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
 then RAISE Exception.Create('Folder does not exist! '+ CRLFw+ aFolder);

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
 then RAISE Exception.Create('Folder does not exist! '+ CRLFw+ aFolder);

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
function ListFilesOf(CONST aFolder, FileType: string; CONST ReturnFullPath, DigSubdirectories: Boolean; ExcludeFolders: TStrings= nil): TStringList;
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

   function IsExcluded(const CheckFor: string): Boolean;
   begin
     if ExcludeFolders = nil
     then Exit(False);

     for var sExcluded in ExcludeFolders do
       if SameFolder(sExcluded, CheckFor)
       or IsSubfolder(sExcluded, CheckFor)
       then Exit(True);
     Result:= False;
   end;

begin
 { We need this in order to prevent the EPathTooLongexception (reported by some users) }
 if aFolder.Length >= MAXPATH
 then RAISE Exception.Create('Path is longer than '+ IntToStr(MAXPATH)+ ' characters!');

 if NOT System.IOUtils.TDirectory.Exists (aFolder)
 then RAISE exception.Create('Folder does not exist! '+ CRLFw+ aFolder);

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
     if IsExcluded(s)
     then EmptyDummy
     else
       if LightCore.IO.DirectoryExists(s)  { This solves the problem caused by broken 'Symbolic Link' folders }
       then ListFiles(s);
  end;

 { Remove full path }
 if NOT ReturnFullPath then
   for i:= 0 to Result.Count-1 DO
     Result[i]:= TPath.GetFileName(Result[i]);
end;



{ COUNT FILES }
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
                 {$IFDEF MSWINDOWS}
                 Result := CountHidden or ((SearchRec.Attr and faHidden) = 0);
                 {$ELSE}
                 // Unix-based systems: Files starting with '.' are considered hidden
                 Result := CountHidden or (SearchRec.Name[1] <> '.');
                 {$ENDIF}
               end;

  if CountHidden
  then StrArray := System.IOUtils.TDirectory.GetFiles( Path, '*', SearchOption )              { Note: Raises exception here is path not found or UNC drive offline }
  else StrArray := System.IOUtils.TDirectory.GetFiles( Path, '*', SearchOption, Predicate);

  Result:= length(StrArray);
end;
{$WARN SYMBOL_PLATFORM On}













{--------------------------------------------------------------------------------------------------
   DRIVES
--------------------------------------------------------------------------------------------------}
function DriveProtected(CONST Drive: Char):  Boolean;                                               { Attempt to create temporary file on specified drive. If created, the temporary file is deleted. see: http://stackoverflow.com/questions/15312704/gettempfilename-creates-an-empty-file }
VAR
   Directory: string;
begin
 Directory := Drive + ':' + PathDelim + 'TestDrive002964982363';
 Result:= NOT ForceDirectoriesB(Directory);
 if NOT Result
 then RemoveDir(Directory);
END;


{ Returns false if the drive letter is not in ['A'..'Z'] }
function ValidDriveLetter(CONST Drive: Char): Boolean;
begin
 Result:= CharInSet(Upcase(Drive), ['A'..'Z']);
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
   if CharIsLetter(s[1])                                                                           { We don't accept network paths (\\) }
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
