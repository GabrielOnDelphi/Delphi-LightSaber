UNIT LightVcl.Common.ExecuteShell;

{=============================================================================================================
   SYSTEM - Execute Process (ShellExecute)
   
   2026.01.29
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
==============================================================================================================

   Execute processes using the ShellExecute/ShellExecuteEx API.

   ShellExecute vs CreateProcess:
      CreateProcess - More complex but provides full control over process creation, handles,
                      environment, and I/O redirection. Required for capturing stdout/stderr.
      ShellExecute  - Simpler API but limited control. Cannot capture output.
                      Supports file associations, URLs, mailto:, .lnk files.
                      Use ShellExecuteEx for more information about launched application.

   Functions:
      ExecuteShell         - Launch using ShellExecute with error reporting
      ExecuteShellEx       - Launch using ShellExecuteEx with process handle
      ExecuteShellAndWait  - Launch and wait for completion using ShellExecuteEx
      ExecuteAsAdmin       - Launch with administrator elevation (runas verb)

   Utility functions:
      ExecuteURL           - Open URL in default browser
      ExecuteSendEmail     - Open default email client with address
      ExecuteExplorer      - Open Windows Explorer at path
      ExecuteExplorerSelect - Open Explorer and select a specific file
      ExecuteControlPanel_ScreenRes - Open display settings

   For cross-platform see:
      Jedi.Execute - www.delphicorner.f9.co.uk/articles/wapi4.htm

   In this group:
     * LightVcl.Common.Shell.pas
     * csSystem.pas
     * csWindow.pas
     * LightVcl.Common.WindowMetrics.pas
     * LightVcl.Common.ExecuteProc.pas
     * LightVcl.Common.ExecuteShell.pas

----------------------------------------------------------------------------------------------------
   NOTES:
     PARAMS needs to be in quotes: "c:\Test.doc". Use DoubleQuoteStr to quote the "Params" string.
     It can execute LNK files: "C:\Delphi3.lnk"
     This function will open the "CPU assembly window" if the "Debug spawned processes" is
     active and OS is Win7.

   Object Verbs:
     The verbs available for an object are essentially the items that you find on an object's
     shortcut menu. To find which verbs are available, look in the registry under
     HKEY_CLASSES_ROOT\CLSID\{object_clsid\Shell\verb

   Commonly available verbs:
     edit       Launches an editor and opens the document for editing.
     find       Initiates a search starting from the specified directory.
     open       Launches an application. If this file is not an executable, its associated
                application is launched.
     print      Prints the document file.
     properties Displays the object's properties.
     runas      Launches an application as Administrator. User Account Control (UAC) will
                prompt the user for consent to run the application elevated or enter the
                credentials of an administrator account.

----------------------------------------------------------------------------------------------------

  Tester:
     c:\MyProjects\Project Testers\Tester Execute program (ShellExecute, CreateProcess)\

=============================================================================================================}

INTERFACE

USES
    WinApi.Windows, Winapi.ShellAPI, Winapi.ShlObj, System.SysUtils, VCL.Forms;


{ ShellExecute wrappers }

{ Executes file/program using ShellExecute. Supports URLs, .lnk files, document associations.
  WindowState: SW_HIDE, SW_SHOWNORMAL, SW_SHOWMINIMIZED, SW_SHOWMAXIMIZED, etc.
  Returns True if successful. }
function ExecuteShell(CONST ExeFile: string; Params: string = '';
  ShowErrorMsg: Boolean = TRUE; WindowState: Integer = SW_SHOWNORMAL): Boolean;

{ Executes file using ShellExecuteEx. Provides access to process handle.
  Returns True if successful. }
function ExecuteShellEx(CONST ExeFile: string; Params: string = '';
  ShowErrorMsg: Boolean = TRUE; WindowState: Integer = SW_SHOWNORMAL): Boolean;

{ Executes file and waits for completion.
  WaitTime: Maximum wait time in milliseconds, or INFINITE.
  Returns True if successful. }
function ExecuteShellAndWait(CONST ExeFile: string; Params: string = '';
  Hide: Boolean = FALSE; WaitTime: Cardinal = INFINITE): Boolean;

{ Executes file with administrator elevation (UAC prompt).
  Returns True if user granted elevation and process started. }
function ExecuteAsAdmin(CONST ExeFile: string; Params: string = ''; hWnd: HWND = 0): Boolean;


{ ShellExecute utility functions }

procedure ExecuteURL(URL: string);
procedure ExecuteSendEmail(EmailAddress: string);
procedure ExecuteExplorer(Path: string);
function  ExecuteExplorerSelect(FileName: string): Boolean;
procedure ExecuteControlPanel_ScreenRes;


IMPLEMENTATION

USES
   LightCore.IO, LightVcl.Common.IO, LightVcl.Common.Dialogs;


CONST
   { ShellExecute returns value > 32 on success }
   SE_SUCCESS_THRESHOLD = 32;


{---------------------------------------------------------------------------------------------------------------
   ExecuteShell

   Executes a file/program using ShellExecute API.
   Supports file associations, URLs, .lnk files, mailto: links, etc.

   Parameters:
     ExeFile      - File path, URL, or document to execute
     Params       - Command line parameters (use quotes for paths with spaces)
     ShowErrorMsg - If True, displays error message on failure
     WindowState  - Window display state (SW_SHOWNORMAL, SW_HIDE, SW_MINIMIZE, etc.)
                    See: http://msdn.microsoft.com/en-us/library/windows/desktop/ms633548

   Returns:
     True if execution succeeded (return value > 32)

   ShellExecute return codes (values <= 32 indicate failure):
     0  - Out of memory or resources
     2  - ERROR_FILE_NOT_FOUND
     3  - ERROR_PATH_NOT_FOUND
     5  - ERROR_ACCESS_DENIED
     8  - ERROR_NOT_ENOUGH_MEMORY
     26 - SE_ERR_SHARE (sharing violation)
     27 - SE_ERR_ASSOCINCOMPLETE (incomplete file association)
     28 - SE_ERR_DDETIMEOUT (DDE timeout)
     29 - SE_ERR_DDEFAIL (DDE transaction failed)
     30 - SE_ERR_DDEBUSY (DDE busy)
     31 - SE_ERR_NOASSOC (no application associated)
     32 - SE_ERR_DLLNOTFOUND

   Note: Does not work well with .scr files in config mode.
   See: https://stackoverflow.com/questions/46672282
---------------------------------------------------------------------------------------------------------------}
function ExecuteShell(CONST ExeFile: string; Params: string = ''; ShowErrorMsg: Boolean = TRUE; WindowState: Integer = WinApi.Windows.SW_SHOWNORMAL): Boolean;
VAR
   RetCode: Integer;
   WorkingFolder, Msg: string;
begin
  if not FileExists(ExeFile) then raise Exception.Create('ExecuteShell no file');

  WorkingFolder:= ExtractFilePath(ExeFile);

  { Using Pointer(Params) instead of PChar(Params) handles empty string correctly.
    See: http://stackoverflow.com/questions/3048188/shellexecute-not-working-from-ide-but-works-otherwise }
  RetCode:= ShellExecute(0, 'open', PChar(ExeFile), Pointer(Params), PChar(WorkingFolder), WindowState);
  Result:= RetCode > SE_SUCCESS_THRESHOLD;

  if NOT Result AND ShowErrorMsg then
    begin
      case RetCode of
        { Legacy error codes }
        0  : Msg:= 'The operating system is out of memory or resources.';
        12 : Msg:= 'Application was designed for a different operating system.';
        13 : Msg:= 'Application was designed for MS-DOS 4.0.';
        15 : Msg:= 'Attempt to load a real-mode program.';
        16 : Msg:= 'Attempt to load a second instance of an application with non-readonly data segments.';
        19 : Msg:= 'Attempt to load a compressed application file.';
        20 : Msg:= 'Dynamic-link library (DLL) file failure.';

        { Standard error codes }
        SE_ERR_FNF            : Msg:= 'File not found: ' + ExeFile;
        SE_ERR_PNF            : Msg:= 'Path not found!';
        SE_ERR_OOM            : Msg:= 'Out of memory!';

        { ShellExecute-specific error codes }
        SE_ERR_SHARE          : Msg:= 'A sharing violation occurred!';
        SE_ERR_ASSOCINCOMPLETE: Msg:= 'The file name association is incomplete or invalid!';
        SE_ERR_DDETIMEOUT     : Msg:= 'The DDE transaction could not be completed because the request timed out!';
        SE_ERR_DDEFAIL        : Msg:= 'The DDE transaction failed!';
        SE_ERR_DDEBUSY        : Msg:= 'The DDE transaction could not be completed because other DDE transactions were being processed!';
        SE_ERR_NOASSOC        : Msg:= 'There is no application associated with the given file name extension!';
        SE_ERR_ACCESSDENIED   : Msg:= 'The operating system denied access! Do you have admin rights?';
        SE_ERR_DLLNOTFOUND    : Msg:= 'The specified DLL was not found!';
      else
        Msg:= 'ShellExecute error ' + IntToStr(RetCode);
      end;

      MessageError(Msg);
    end;
end;


{---------------------------------------------------------------------------------------------------------------
   ExecuteShellEx

   Executes a file using ShellExecuteEx API, which provides more control and information
   than the basic ShellExecute.

   Parameters:
     ExeFile      - File path to execute
     Params       - Command line parameters
     ShowErrorMsg - If True, displays error message on failure
     WindowState  - Window display state

   Returns:
     True if execution succeeded

   See: http://stackoverflow.com/questions/4295285/how-can-i-wait-for-a-command-line-program-to-finish
---------------------------------------------------------------------------------------------------------------}
function ExecuteShellEx(CONST ExeFile: string; Params: string = '';
  ShowErrorMsg: Boolean = TRUE; WindowState: Integer = SW_SHOWNORMAL): Boolean;
VAR
   ShellInfo: TShellExecuteInfo;
begin
  if not FileExists(ExeFile) then raise Exception.Create('ExecuteShell no file');

  FillChar(ShellInfo, SizeOf(ShellInfo), 0);
  ShellInfo.cbSize:= SizeOf(ShellInfo);
  ShellInfo.fMask:= SEE_MASK_NOCLOSEPROCESS or SEE_MASK_FLAG_DDEWAIT;
  ShellInfo.Wnd:= GetActiveWindow;
  ShellInfo.lpVerb:= 'open';
  ShellInfo.lpParameters:= PChar(Params);
  ShellInfo.lpFile:= PChar(ExeFile);
  ShellInfo.nShow:= WindowState;

  Result:= ShellExecuteEx(@ShellInfo);

  if Result then
    begin
      { Close the process handle since we don't need to wait for it }
      if ShellInfo.hProcess <> 0
      then CloseHandle(ShellInfo.hProcess);
    end
  else if ShowErrorMsg then
    MessageError(SysErrorMessage(GetLastError));
end;


{---------------------------------------------------------------------------------------------------------------
   ExecuteAsAdmin

   Executes a program with administrator elevation using the 'runas' verb.
   This will trigger a UAC (User Account Control) prompt on Vista and later.

   Parameters:
     ExeFile - Full path to executable
     Params  - Command line parameters
     hWnd    - Owner window handle for UAC dialog (0 for no owner)

   Returns:
     True if user granted elevation and process started successfully.
     False if user cancelled UAC prompt or execution failed.

   See: learn.microsoft.com/en-us/windows/win32/api/shellapi/ns-shellapi-shellexecuteinfow
---------------------------------------------------------------------------------------------------------------}
function ExecuteAsAdmin(CONST ExeFile: string; Params: string = ''; hWnd: HWND = 0): Boolean;
VAR
  ShellInfo: TShellExecuteInfo;
begin
  if not FileExists(ExeFile) then raise Exception.Create('ExecuteShell no file');

  FillChar(ShellInfo, SizeOf(ShellInfo), 0);
  ShellInfo.cbSize:= SizeOf(ShellInfo);
  ShellInfo.fMask:= SEE_MASK_FLAG_DDEWAIT OR SEE_MASK_FLAG_NO_UI;
  ShellInfo.Wnd:= hWnd;
  ShellInfo.lpVerb:= 'runas';
  ShellInfo.lpFile:= PChar(ExeFile);
  ShellInfo.lpParameters:= PChar(Params);
  ShellInfo.nShow:= SW_SHOWNORMAL;

  Result:= ShellExecuteEx(@ShellInfo);
end;


{---------------------------------------------------------------------------------------------------------------
   ExecuteShellAndWait

   Executes a program using ShellExecuteEx and waits for it to finish.
   Keeps UI responsive by processing messages during the wait.

   Parameters:
     ExeFile  - Full path to executable
     Params   - Command line parameters
     Hide     - If True, launches with SW_HIDE (for console apps)
     WaitTime - Maximum time to wait in milliseconds, or INFINITE

   Returns:
     True if execution and wait succeeded

   Note:
     Uses Application.ProcessMessages to keep UI responsive during wait.
     This is intentional for UI responsiveness but be aware of reentrancy issues.

   See:
     http://stackoverflow.com/questions/4295285/how-can-i-wait-for-a-command-line-program-to-finish
     https://blogs.msdn.microsoft.com/larryosterman/2004/06/02/things-you-shouldnt-do-part-4-msgwaitformultipleobjects-is-a-very-tricky-api/
---------------------------------------------------------------------------------------------------------------}
function ExecuteShellAndWait(CONST ExeFile: string; Params: string = '';
  Hide: Boolean = FALSE; WaitTime: Cardinal = INFINITE): Boolean;
VAR
  ShellInfo: TShellExecuteInfo;
CONST
  bWaitAll = FALSE;
begin
  if not FileExists(ExeFile) then raise Exception.Create('ExecuteShell no file');

  FillChar(ShellInfo, SizeOf(ShellInfo), 0);
  ShellInfo.cbSize:= SizeOf(ShellInfo);
  ShellInfo.fMask:= SEE_MASK_NOCLOSEPROCESS or SEE_MASK_FLAG_DDEWAIT;
  ShellInfo.Wnd:= GetActiveWindow;
  ShellInfo.lpVerb:= 'open';
  ShellInfo.lpParameters:= PChar(Params);
  ShellInfo.lpFile:= PChar(ExeFile);

  if Hide
  then ShellInfo.nShow:= SW_HIDE
  else ShellInfo.nShow:= SW_SHOWNORMAL;

  if NOT ShellExecuteEx(@ShellInfo) then
    begin
      MessageError(SysErrorMessage(GetLastError));
      EXIT(FALSE);
    end;

  { Wait for process while keeping UI responsive }
  WHILE MsgWaitForMultipleObjects(1, ShellInfo.hProcess, bWaitAll, WaitTime, QS_POSTMESSAGE Or QS_SENDMESSAGE) <> WAIT_OBJECT_0 
   DO Application.ProcessMessages; // is this wise?

  CloseHandle(ShellInfo.hProcess);
  Result:= TRUE;
end;


{---------------------------------------------------------------------------------------------------------------
   UTILITY FUNCTIONS
---------------------------------------------------------------------------------------------------------------}


{---------------------------------------------------------------------------------------------------------------
   ExecuteURL

   Opens a URL in the default web browser.
   Encodes special characters that may cause issues in URLs.

   Parameters:
     URL - The URL to open (http://, https://, etc.)
---------------------------------------------------------------------------------------------------------------}
procedure ExecuteURL(URL: string);
begin
  if URL = '' then EXIT;

  { Encode special characters that may cause issues }
  URL:= StringReplace(URL, '"', '%22', [rfReplaceAll]);

  { Use ShellExecute directly - URLs don't pass the FileExists check in ExecuteShell }
  ShellExecute(0, 'open', PChar(URL), NIL, NIL, SW_SHOWNORMAL);
end;


{---------------------------------------------------------------------------------------------------------------
   ExecuteSendEmail

   Opens the default email client with a new message to the specified address.

   Parameters:
     EmailAddress - Email address to send to (without mailto: prefix)
---------------------------------------------------------------------------------------------------------------}
procedure ExecuteSendEmail(EmailAddress: string);
begin
  if EmailAddress = '' then EXIT;

  { ShellExecute with mailto: protocol opens default email client }
  ShellExecute(0, NIL, PChar('mailto:' + EmailAddress), NIL, NIL, SW_SHOW);
end;


{---------------------------------------------------------------------------------------------------------------
   ExecuteExplorer

   Opens Windows Explorer at the specified path.
   Uses 'explore' verb to open Explorer view (not just browse).

   Parameters:
     Path - Directory path to explore
---------------------------------------------------------------------------------------------------------------}
procedure ExecuteExplorer(Path: string);
begin
  if Path = '' then EXIT;

  if DirectoryExistMsg(Path)
  then ShellExecute(0, 'explore', PChar(Path), NIL, NIL, SW_SHOW);
end;


{---------------------------------------------------------------------------------------------------------------
   ExecuteExplorerSelect

   Opens Windows Explorer at the file's location and selects the file.
   Uses SHOpenFolderAndSelectItems API for proper selection behavior.

   Parameters:
     FileName - Full path to file to select

   Returns:
     True if Explorer was opened and file selected successfully

   See: http://stackoverflow.com/questions/15300999/open-windows-explorer-directory-select-a-specific-file-in-delphi
---------------------------------------------------------------------------------------------------------------}
function ExecuteExplorerSelect(FileName: string): Boolean;
VAR
  ItemIDList: PItemIDList;
begin
  Result:= FALSE;
  if not FileExists(FileName) then RAISE Exception.Create('ExecuteShell no file');

  ItemIDList:= ILCreateFromPath(PChar(FileName));
  if ItemIDList <> NIL then
    TRY
      Result:= SHOpenFolderAndSelectItems(ItemIDList, 0, NIL, 0) = S_OK;
    FINALLY
      ILFree(ItemIDList);
    END;
end;


{---------------------------------------------------------------------------------------------------------------
   ExecuteControlPanel_ScreenRes

   Opens Windows Control Panel to the Screen Resolution / Display Settings page.

   Uses CreateProcess to launch control.exe with the display settings applet.

   See:
     http://stackoverflow.com/questions/18919089/how-to-execute-items-in-control-panel
     http://msdn.microsoft.com/en-us/library/windows/desktop/cc144191
---------------------------------------------------------------------------------------------------------------}
procedure ExecuteControlPanel_ScreenRes;
VAR
  App: string;
  CmdLine: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  App:= GetWinDir + 'system32\control.exe';
  if NOT FileExistsMsg(App) then EXIT;

  CmdLine:= App + ' desk.cpl,Settings@Settings';

  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb:= SizeOf(StartupInfo);

  if CreateProcess(NIL, PChar(CmdLine), NIL, NIL, FALSE, 0, NIL, NIL, StartupInfo, ProcessInfo) then
    begin
      { Close handles - we don't need to wait for control panel }
      CloseHandle(ProcessInfo.hThread);
      CloseHandle(ProcessInfo.hProcess);
    end
  else
    RaiseLastOSError;
end;


end.
