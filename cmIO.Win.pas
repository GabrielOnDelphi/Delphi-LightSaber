UNIT cmIO.Win;

{=============================================================================================================
   Gabriel Moraru
   2024.06
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

   I/O functions that are not cross-platform.

==================================================================================================}

INTERFACE
{$WARN UNIT_PLATFORM ON}   {Silence the 'W1005 Unit Vcl.FileCtrl is specific to a platform' warning }

USES
  Winapi.Windows, Winapi.ShellAPI, Winapi.ShlObj,
  System.Win.Registry, System.SysUtils, System.Classes,
  Vcl.Consts, Vcl.Controls, Vcl.Dialogs, Vcl.Forms, Vcl.FileCtrl;

(*
CONST
  {$IFDEF MSWINDOWS}
    MAXPATH= MAX_PATH- 12;                                          { Check like this:  if Path < MAXPATH then... }
  {$ELSE}
    MAXPATH= MAX_PATH;
  {$ENDIF}   *)


{--------------------------------------------------------------------------------------------------
   SPECIAL FOLDERS
--------------------------------------------------------------------------------------------------}
 function  GetProgramFilesDir    : string; 
 function  GetDesktopFolder      : string;
 function  GetStartMenuFolder    : string;
 function  GetMyDocumentsAPI: string; deprecated 'Use GetMyDocuments instead';
 function  GetMyPicturesAPI : string; deprecated 'Use GetMyPictures  instead';
 function  GetWinSysDir: string;
 function  GetWinDir: string; { Returns Windows folder }

 function  GetSpecialFolder (CONST OS_SpecialFolder: string): string;                 overload;          { SHELL FOLDERS.  Retrieving the entire list of default shell folders from registry }
 function  GetSpecialFolder (CSIDL: Integer; ForceFolder: Boolean = FALSE): string;   overload;          { uses SHFolder }
 function  GetSpecialFolders: TStringList;                                                               { Get a list of ALL special folders. }

 function  FolderIsSpecial  (const Path: string): Boolean;                                               { Returns True if the parameter is a special folder such us 'c:\My Documents' }



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
 function RecycleItem      (CONST ItemName: string; CONST DeleteToRecycle: Boolean= TRUE; CONST ShowConfirm: Boolean= TRUE; CONST TotalSilence: Boolean= FALSE): Boolean;
 function FileOperation    (CONST Source, Dest: string; Op, Flags: Integer): Boolean;                     { Performs: Copy, Move, Delete, Rename on files + folders via WinAPI}
 function GetFileSizeEx    (hFile: THandle; VAR FileSize: Int64): BOOL; stdcall; external kernel32;
 function FileAge          (CONST FileName: string): TDateTime;


IMPLEMENTATION
USES
  cbRegistry,
  cbWinVersion,
  ccIO;
  //DONT CREATE CIRCULAR REFFERENCE TO cbAppData HERE!



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

{.$WARN SYMBOL_PLATFORM OFF}
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
 if cbWinVersion.IsWindowsVistaUp then
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
{.$WARN SYMBOL_PLATFORM On}










{--------------------------------------------------------------------------------------------------
   SPECIAL FOLDERS
--------------------------------------------------------------------------------------------------}

function GetProgramFilesDir: string;
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




{$WARN SYMBOL_PLATFORM OFF}
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
{$WARN SYMBOL_PLATFORM On}










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





end.

