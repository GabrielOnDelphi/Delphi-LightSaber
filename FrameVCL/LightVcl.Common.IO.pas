UNIT LightVcl.Common.IO;

{=============================================================================================================
   2026.10.01
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Extension for LightCore.IO.pas
   Framework: VCL only
   Shows error messages (dialog boxes) when the I/O operation failed.

   See also:
     LightCore.Win.IO.pas   - the Windows special folders: Windows, System, Program Files, Desktop, Start Menu, and any folder named by a CSIDL constant; the drives (type, volume label, free space) and NTFS compression
==================================================================================================}

INTERFACE
{ $WARN UNIT_PLATFORM OFF}   { OFF: Silence the 'W1005 Unit Vcl.FileCtrl is specific to a platform' warning }

USES
  Winapi.Windows, Winapi.ShellAPI, Winapi.ShlObj,
  System.IOUtils, System.SysUtils,
  Vcl.Consts, Vcl.Controls, Vcl.Dialogs, Vcl.Forms, Vcl.FileCtrl;


{--------------------------------------------------------------------------------------------------
   OPERATIONS WITH MESSAGE
--------------------------------------------------------------------------------------------------}
 function  DirectoryExistMsg    (CONST Path: string): Boolean;
 function  FileExistsMsg        (CONST FileName: string): Boolean;
 function  ForceDirectoriesMsg  (CONST FullPath: string): Boolean;                                 { Wrapper around LightCore.IO.ForceDirectoriesB. Returns True if directory exists or was created, False on failure. Shows error dialog on failure. }

 procedure MoveFolderMsg        (CONST FromFolder, ToFolder: String; SilentOverwrite: Boolean);
 function  DeleteFileWithMsg    (CONST FileName: string): Boolean;


{--------------------------------------------------------------------------------------------------
   SPECIAL FOLDERS
--------------------------------------------------------------------------------------------------}
 function  GetMyDocumentsAPI     : string; deprecated 'Use GetMyDocuments instead';
 function  GetMyPicturesAPI      : string; deprecated 'Use GetMyPictures  instead';


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


{--------------------------------------------------------------------------------------------------
   FILE SIZE
--------------------------------------------------------------------------------------------------}
 function  GetFileSizeEx       (hFile: THandle; VAR FileSize: Int64): BOOL; stdcall; external kernel32;


{--------------------------------------------------------------------------------------------------
   FILE ACCESS
--------------------------------------------------------------------------------------------------}
 function  CanWriteToFolderMsg  (CONST Folder: string): Boolean;



IMPLEMENTATION

USES
  Winapi.ActiveX,
  LightCore, LightCore.Win.IO, LightCore.IO, LightVcl.Common.Dialogs, LightCore.WinVersion;


function DirectoryExistMsg(CONST Path: string): Boolean;                                           { Directory Exist }
begin
  Result:= DirectoryExists(Path);
  if NOT Result then
    if Path= ''
    then MessageError('DirectoryExistMsg: No folder specified!')
    else
      if (Pos(':', Path) < 1) AND (Pos('\\', Path) <> 1)                                               { full paths start with a drive ('c:\xxx') or are UNC ('\\server\share') }
      then MessageError('A relative path was provided instead of a full path!'+ CRLFw+ Path)
      else MessageError('Folder does not exist:'+ CRLFw+ Path);
end;


{ Shows an error message if the folder cannot be created. }
function ForceDirectoriesMsg(CONST FullPath: string): Boolean;
begin
  Result:= LightCore.IO.ForceDirectoriesB(FullPath);
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
    - SilentOverwrite=True: copies all contents to ToFolder and deletes FromFolder.
      If some files could not be copied, FromFolder is KEPT and an error dialog is shown.
    - SilentOverwrite=False: prompts user to confirm deletion of ToFolder before moving
  Example: MoveFolderMsg('c:\Documents', 'C:\Backups', True) }
procedure MoveFolderMsg(CONST FromFolder, ToFolder: String; SilentOverwrite: Boolean);
VAR FailedCount: Integer;
begin
 if FromFolder = ''
 then raise Exception.Create('MoveFolderMsg: FromFolder parameter cannot be empty');

 if ToFolder = ''
 then raise Exception.Create('MoveFolderMsg: ToFolder parameter cannot be empty');

 if DirectoryExists(ToFolder) then
   begin
     if SilentOverwrite
     then
       begin
         FailedCount:= CopyFolder(FromFolder, ToFolder, True);
         if FailedCount > 0
         then MessageError(IntToStr(FailedCount)+ ' file(s) could not be copied to '+ ToFolder+ CRLFw+ 'The source folder was NOT deleted: '+ FromFolder)   { Deleting the source would destroy the files that were never copied }
         else DeleteFolder(FromFolder);
       end
     else
       { Move raises an exception if the destination folder already exists, so we have to delete the Destination folder first. But for this we need to ask the user. }
       if MesajYesNo('Cannot move '+ FromFolder +'. Destination folder already exists:'+ ToFolder+ CRLFw+
                     'Press Yes to delete Destination folder. Press No to cancel the opperation.') then
         begin
           DeleteFolder(ToFolder);
           TDirectory.Move(FromFolder, ToFolder);
         end;
   end
 else
   { Destination does not exist - the plain move. Without this branch the function silently did NOTHING in the most common case. }
   TDirectory.Move(FromFolder, ToFolder);
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
 if LightCore.WinVersion.IsWindowsVistaUp
 then
   begin
     Dlg:= TFileOpenDialog.Create(NIL);
     TRY
       Dlg.Options:= Options;
       Dlg.DefaultFolder:= Folder;
       { Note: Do NOT set Dlg.FileName here. Setting FileName to a full directory path
         puts it in the filename edit box, which can interfere with DefaultFolder navigation
         in folder-pick mode. DefaultFolder (which maps to IFileDialog.SetFolder) is
         sufficient to navigate to the initial directory.
		Dlg.FileName:= Folder;  }
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
function GetMyDocumentsAPI: string;
begin
 Result:= Trail(GetSpecialFolder(CSIDL_PERSONAL));
end;


function GetMyPicturesAPI: string;
begin
 Result:= Trail(GetSpecialFolder(CSIDL_MYPICTURES));
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

 { SHFileOperation returns 0 (success) even when the user pressed 'No' in the confirmation dialog.
   The abort is reported separately in fAnyOperationsAborted. See learn.microsoft.com/en-us/windows/win32/api/shellapi/nf-shellapi-shfileoperationa }
 Result:= (SHFileOperation(SHFileOpStruct) = 0) AND NOT SHFileOpStruct.fAnyOperationsAborted;
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

  { SHFileOperation returns 0 (success) even when the user aborted (confirmation or progress dialog).
    The abort is reported separately in fAnyOperationsAborted. See learn.microsoft.com/en-us/windows/win32/api/shellapi/nf-shellapi-shfileoperationa }
  Result:= (SHFileOperation(SHFileOpStruct) = 0) AND NOT SHFileOpStruct.fAnyOperationsAborted;
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


{ On Vista+ the VCL shows TOpenDialog/TSaveDialog through the IFileDialog COM object.
  When that COM object cannot be created - COM not initialized on the calling thread, or initialized as MTA (the shell dialog is STA-only) -
  the VCL swallows the CoCreateInstance failure and Execute returns FALSE without showing any dialog
  (Vcl.Dialogs.TCustomFileDialog.Execute ignores the failed CreateFileDialog).
  That was the old 'nothing happens' bug: no dialog, no exception - it looked like the user pressed Cancel.
  Fix: probe the COM object first; if it cannot be created, show the classic dialog instead - that one is plain WinAPI (GetOpenFileName), no COM involved. }
function ExecuteDialogSafe(Dialog: TOpenDialog): Boolean;
VAR
   Probe: IFileDialog;
   SavedLatest: Boolean;
begin
  if Succeeded(CoCreateInstance(CLSID_FileOpenDialog, NIL, CLSCTX_INPROC_SERVER, IID_IFileDialog, Probe))
  then
    begin
      Probe:= NIL;
      Result:= Dialog.Execute;
    end
  else
    begin
      SavedLatest:= UseLatestCommonDialogs;
      UseLatestCommonDialogs:= FALSE;      { Routes Execute to the classic GetOpenFileName dialog }
      TRY
        Result:= Dialog.Execute;
      FINALLY
        UseLatestCommonDialogs:= SavedLatest;
      END;
    end;
end;


{ Core implementation for Open/Save file dialogs.
  Based on Vcl.Dialogs.PromptForFileName but with added options (ofEnableSizing, ofForceShowHidden).
  Does not support multi-select because it returns a single filename. }
function PromptForFileName(VAR FileName: string; SaveDialog: Boolean; CONST Filter: string = ''; CONST DefaultExt: string= ''; CONST Title: string= ''; CONST InitialDir: string = ''): Boolean;
VAR
  Dialog: TOpenDialog;
begin
  Assert(GetCurrentThreadId = MainThreadID, 'PromptForFileName must be called from the main thread!');

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

    Result := ExecuteDialogSafe(Dialog);

    if Result
    then FileName:= Dialog.FileName;
  FINALLY
    FreeAndNil(Dialog);
  END;
end;






{-------------------------------------------------------------------------------------------------------------
   TFileOpenDlg

   Example: PromptToSaveFile(s, LightCore.IO.FilterTxt, 'txt')
   Note: You might want to use PromptForFileName instead
-------------------------------------------------------------------------------------------------------------}

Function GetOpenDialog(CONST FileName, Filter, DefaultExt: string; CONST Caption: string= ''): TOpenDialog;
begin
 Result:= TOpenDialog.Create(NIL);
 TRY
   Result.Filter:= Filter;
   Result.FilterIndex:= 0;
   Result.Options:= [ofFileMustExist, ofEnableSizing, ofForceShowHidden];
   Result.DefaultExt:= DefaultExt;
   Result.FileName:= FileName;
   Result.Title:= Caption;

   if FileName= ''
   then Result.InitialDir:= GetMyDocuments
   else Result.InitialDir:= ExtractFilePath(FileName);
 EXCEPT
   FreeAndNil(Result);   { Don't leak the dialog if a setup step raises before the caller receives it }
   RAISE;
 END;
end;


{ Example: SaveDialog(LightCore.FilterTxt, 'csv');  }
Function GetSaveDialog(CONST FileName, Filter, DefaultExt: string; CONST Caption: string= ''): TSaveDialog;
begin
 Result:= TSaveDialog.Create(NIL);
 TRY
   Result.Filter:= Filter;
   Result.FilterIndex:= 0;
   { No ofFileMustExist here: it demands an EXISTING file, which contradicts saving under a new name. OPENFILENAME docs: "It cannot be used with a Save As dialog box." }
   Result.Options:= [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing];  //  - ofNoChangeDir  { When a user displays the open dialog, whether InitialDir is used or not, the dialog alters the program's current working directory while the user is changing directories before clicking on the Ok/Open button. Upon closing the dialog, the current working directly is not reset to its original value unless the ofNoChangeDir option is specified.  }
   Result.DefaultExt:= DefaultExt;
   Result.FileName:= FileName;
   Result.Title:= Caption;

   if FileName= ''
   then Result.InitialDir:= GetMyDocuments
   else Result.InitialDir:= ExtractFilePath(FileName);
 EXCEPT
   FreeAndNil(Result);   { Don't leak the dialog if a setup step raises before the caller receives it }
   RAISE;
 END;
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

 if NOT LightCore.IO.ForceDirectoriesB(To_DestFolder)
 then raise Exception.Create('FileMoveToDir: Cannot create destination folder: ' + To_DestFolder);

 Result:= MoveFileEx(PChar(From_FullPath), PChar(Trail(To_DestFolder) + ExtractFileName(From_FullPath)), Flags);
end;



end.
