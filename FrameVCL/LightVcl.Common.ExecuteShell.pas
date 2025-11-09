UNIT LightVcl.Common.ExecuteShell;

{=============================================================================================================
   SYSTEM - Execute Process
   2023.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
==============================================================================================================

   Execute a process, using the ShellExecute API

   ShellExecute vs CreateProcess
      CreateProcess - is not as straigth forward as ShellExecute but it allows you more control.
      ShellExecute  - is easyer to use but you don't have a lot of control over it.
      To obtain information about the application that is launched as a result of calling ShellExecute, use ShellExecuteEx.

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
     This function will open the "CPU assembly window" if the "Debug spawned processes" is active and OS is Win7.

   Object Verbs
     The verbs available for an object are essentially the items that you find on an object's shortcut menu. To find which verbs are available, look in the registry under
     HKEY_CLASSES_ROOT\CLSID\{object_clsid\Shell\verb

   Commonly available verbs:
     edit       Launches an editor and opens the document for editing.
     find       Initiates a search starting from the specified directory.
     open       Launches an application. If this file is not an executable file, its associated application is launched.
     print      Prints the document file.
     properties Displays the object's properties.
     runas      Launches an application as Administrator. User Account Control (UAC) will prompt the user for consent to run the application elevated or enter the credentials of an administrator account used to run the application.

----------------------------------------------------------------------------------------------------

  Tester
     c:\MyProjects\Project Testers\Tester Execute program (ShellExecute, CreateProcess)\

=============================================================================================================}

INTERFACE

USES
    WinApi.Windows, Winapi.ShellAPI, Winapi.ShlObj, System.SysUtils, VCL.Forms;

 { ShellExecute }
 function  ExecuteShell    (CONST ExeFile: string; Params: string= ''; ShowErrorMsg: Boolean= TRUE; WindowState: Integer= SW_SHOWNORMAL): Boolean;  { WindowState can be  as defined in WinApi.Windows.pas: SW_HIDE, SW_SHOWNORMAL, SW_NORMAL, SW_SHOWMINIMIZED, SW_SHOWMAXIMIZED, SW_MAXIMIZE, SW_SHOWNOACTIVATE, SW_SHOW, SW_MINIMIZE, SW_SHOWMINNOACTIVE, SW_SHOWNA, SW_RESTORE, SW_SHOWDEFAULT, SW_MAX }
 function  ExecuteShellEx  (CONST ExeFile: string; Params: string= ''; ShowErrorMsg: Boolean= TRUE; WindowState: Integer= SW_SHOWNORMAL): Boolean;
 function  ExecuteAndWait  (CONST ExeFile: string; Params: string= ''; Hide: Boolean= FALSE; WaitTime: Cardinal= INFINITE): Boolean;
 function  ExecuteAsAdmin  (CONST ExeFile: string; Params: string= ''; hWnd: HWND= 0): Boolean;

 { ShellExecute Utils }
 procedure ExecuteURL      (URL: string);
 procedure ExecuteSendEmail(EmailAddress: string);
 procedure ExecuteExplorer (Path: string);                     { This will open Explorer in the specified folder. 'Execute' will browse the folder not explore }
 function  ExecuteExplorerSelect(FileName: string): Boolean;   { Execute Win Explorer and select the specified file }
 procedure ExecuteControlPanel_ScreenRes;                      { Open Control Panel to 'Screen Resolution' tab }


IMPLEMENTATION

USES
   LightCore.IO, LightVcl.Common.IO, LightVcl.Common.Dialogs;



{-------------------------------------------------------------------------------------------------------------
 WindowState
     Can be as defined in Windows.pas:
     SW_HIDE, SW_SHOWNORMAL, SW_NORMAL, SW_SHOWMINIMIZED, SW_SHOWMAXIMIZED, SW_MAXIMIZE, SW_SHOWNOACTIVATE, SW_SHOW, SW_MINIMIZE, SW_SHOWMINNOACTIVE, SW_SHOWNA, SW_RESTORE, SW_SHOWDEFAULT, SW_MAX

 Description of each parameter:  http://msdn.microsoft.com/en-us/library/windows/desktop/ms633548%28v=vs.85%29.aspx

 Official documentation:
      https://msdn.microsoft.com/en-US/bb762153?f=255&MSPPError=-2147217396
 More:
     http://tekreaders.com/blog/2011/08/03/shellexecute-in-delphi-launch-external-applications/

 This does not work well with *.scr programs: https://stackoverflow.com/questions/46672282/how-to-run-a-screensaver-in-config-mode-with-shellexecute

 ShellExecute return codes:
   If the function succeeds, it sets the hInstApp member of the SHELLEXECUTEINFO structure to a value greater than 32. If the function fails, hInstApp is set to the SE_ERR_XXX error value that best indicates the cause of the failure. Although hInstApp is declared as an HINSTANCE for compatibility with 16-bit Windows applications, it is not a true HINSTANCE. It can be cast only to an int and can be compared only to either the value 32 or the SE_ERR_XXX error codes.
   The SE_ERR_XXX error values are provided for compatibility with ShellExecute.
   To retrieve more accurate error information, use GetLastError. It may return one of the following values:
      // Error code definitions for the Win32 API functions
      { 02  ERROR_FILE_NOT_FOUND    : Msg:= 'The specified file was not found.';
      { 03  ERROR_PATH_NOT_FOUND    : Msg:= 'The specified path was not found.';
      { 08  ERROR_NOT_ENOUGH_MEMORY : Msg:= 'There is not enough memory to perform the specified action!';
      { 11  ERROR_BAD_FORMAT        : Msg:= 'The .exe file is invalid (non-Win32 .EXE or error in .EXE image).';
      { 32  ERROR_SHARING_VIOLATION : Msg:= 'A sharing violation occurred!';
      {1156 ERROR_DDE_FAIL          : Msg:= 'The Dynamic Data Exchange (DDE) transaction failed!';
            ERROR_ACCESS_DENIED     : Msg:= 'Access to the specified file is denied!';
            ERROR_CANCELLED         : Msg:= 'The function prompted the user for additional information, but the user canceled the request!';
            ERROR_DLL_NOT_FOUND     : Msg:= 'One of the library files necessary to run the application can't be found!';
            ERROR_NO_ASSOCIATION    : Msg:= 'There is no application associated with the specified file name extension!'
-------------------------------------------------------------------------------------------------------------}
function ExecuteShell(CONST ExeFile: string; Params: string= ''; ShowErrorMsg: Boolean= TRUE; WindowState: Integer= WinApi.Windows.SW_SHOWNORMAL): Boolean;
VAR
   i: integer;
   WorkingFolder, Msg: string;
begin
 WorkingFolder:= ExtractFilePath(ExeFile);
 i:= ShellExecute(0, 'open', PChar(ExeFile), Pointer(Params), PChar(WorkingFolder), WindowState);   //  See this about using 'Pointer' instead of 'PChar': http://stackoverflow.com/questions/3048188/shellexecute-not-working-from-ide-but-works-otherwise
 Result:= i > 32;
 if NOT Result AND ShowErrorMsg then
  begin
   case i of
      // What are these?
      0  : Msg:= 'The operating system is out of memory or resources.';
      12 : Msg:= 'Application was designed for a different operating system.';
      13 : Msg:= 'Application was designed for MS-DOS 4.0';
      15 : Msg:= 'Attempt to load a real-mode program.';
      16 : Msg:= 'Attempt to load a second instance of an application with non-readonly data segments.';
      19 : Msg:= 'Attempt to load a compressed application file.';
      20 : Msg:= 'Dynamic-link library (DLL) file failure.';

      // Regular WinExec codes
      { 02} SE_ERR_FNF            : Msg:= 'Exe file not found!'+ ExeFile;
      { 03} SE_ERR_PNF            : Msg:= 'Path not found!';
      { 08} SE_ERR_OOM            : Msg:= 'Out of memory!';

      // Error values for ShellExecute beyond the regular WinExec() codes
      { 26} SE_ERR_SHARE          : Msg:= 'A sharing violation occurred!';
      { 27} SE_ERR_ASSOCINCOMPLETE: Msg:= 'The file name association is incomplete or invalid!';
      { 28} SE_ERR_DDETIMEOUT     : Msg:= 'The DDE transaction could not be completed because the request timed out!';
      { 29} SE_ERR_DDEFAIL        : Msg:= 'The DDE transaction failed!';
      { 30} SE_ERR_DDEBUSY        : Msg:= 'The DDE transaction could not be completed because other DDE transactions were being processed!';
      { 31} SE_ERR_NOASSOC        : Msg:= 'There is no application associated with the given file name extension!';

      { 05} SE_ERR_ACCESSDENIED   : Msg:= 'The operating system denied access! Do you have admin rights?';       // https://answers.microsoft.com/en-us/windows/forum/windows_7-windows_programs/getting-error-shellexecuteex-failed-code-5-access/3af7bea3-5733-426c-9e12-6ec68bf7b38b?auth=1
      { 32} SE_ERR_DLLNOTFOUND    : Msg:= 'The specified DLL was not found!'
     else
        Msg:= 'ShellExecute error '+ IntToStr(i);
   end;

   MessageError(Msg);
  end;
end;



{ Source: http://stackoverflow.com/questions/4295285/how-can-i-wait-for-a-command-line-program-to-finish }
function ExecuteShellEx(CONST ExeFile: string; Params: string= ''; ShowErrorMsg: Boolean= TRUE; WindowState: Integer= SW_SHOWNORMAL): Boolean;
VAR
   sei: TShellExecuteInfo;
   Ph: DWORD;
CONST
   bWaitAll = FALSE;    { Explanation for this: https://blogs.msdn.microsoft.com/larryosterman/2004/06/02/things-you-shouldnt-do-part-4-msgwaitformultipleobjects-is-a-very-tricky-api/ }
begin
 FillChar(sei, SizeOf(sei), 0);
 sei.cbSize := SizeOf(sei);
 sei.fMask  := SEE_MASK_NOCLOSEPROCESS or SEE_MASK_FLAG_DDEWAIT;
 sei.Wnd    := GetActiveWindow;
 sei.lpVerb := 'open';
 sei.lpParameters := PChar(Params);
 sei.lpFile := PChar(ExeFile);
 sei.nShow  := WindowState;

 Result:= ShellExecuteEx(@sei);
 if Result
 then Ph:= sei.hProcess
 else
  begin
   if ShowErrorMsg
   then MessageError(SysErrorMessage(GetLastError));
   EXIT(FALSE);
  end;

 CloseHandle(Ph);
 Result := true;
end;



// learn.microsoft.com/en-us/windows/win32/api/shellapi/ns-shellapi-shellexecuteinfow
function ExecuteAsAdmin(CONST ExeFile: string; Params: string= ''; hWnd: HWND= 0): Boolean; { Source: Delphi Handbook / Marco Cantu - Works fine }
VAR ShellInfo: TShellExecuteInfo;
begin
 FillChar(ShellInfo, SizeOf(ShellInfo), 0);
 ShellInfo.cbSize:= SizeOf(ShellInfo);
 ShellInfo.fMask:= SEE_MASK_FLAG_DDEWAIT OR SEE_MASK_FLAG_NO_UI;
 ShellInfo.Wnd:= hWnd;                           // Optional. A handle to the owner window, used to display and position any UI that the system might produce while executing this function.
 ShellInfo.lpVerb:= 'runas';
 ShellInfo.lpFile:= PChar(ExeFile);
 ShellInfo.lpParameters:= PChar(Params);
 ShellInfo.nShow:= SW_SHOWNORMAL;

 Result:= ShellExecuteEx(@ShellInfo);
end;



{ It seems to return '42' after a sucesful execution
function ExecuteAsAdmin_Old(hWnd: HWND; aFile: String; Params: String): Integer;
begin
 Result:= ShellExecute(hWnd, 'runas', PWideChar(aFile), PWideChar(Params), nil, SW_SHOWNORMAL);
end; }





{-------------------------------------------------------------------------------------------------------------
 Execute a program and wait for it to finish.
 Based on ShellExecuteEx.
 You can use INFINITE for WaitTime.

 http://stackoverflow.com/questions/4295285/how-can-i-wait-for-a-command-line-program-to-finish
 TESTER: c:\MyProjects\Project Testers\Tester ExecuteAndWait\
-------------------------------------------------------------------------------------------------------------}
function ExecuteAndWait(CONST ExeFile: string; Params: string= ''; Hide: Boolean= FALSE; WaitTime: Cardinal= INFINITE): Boolean;
VAR
   sei: TShellExecuteInfo;
   Ph: DWORD;
CONST
  bWaitAll = FALSE;  { Explanation for this: https://blogs.msdn.microsoft.com/larryosterman/2004/06/02/things-you-shouldnt-do-part-4-msgwaitformultipleobjects-is-a-very-tricky-api/ }
begin
  FillChar(sei, SizeOf(sei), 0);
  with sei do
   begin
    cbSize := SizeOf(sei);
    fMask := SEE_MASK_NOCLOSEPROCESS or SEE_MASK_FLAG_DDEWAIT;
    Wnd := GetActiveWindow;
    lpVerb := 'open';
    lpParameters := PChar(Params);
    lpFile := PChar(ExeFile);
    if Hide
    then nShow := SW_HIDE
    else nShow := SW_SHOWNORMAL;
   end;

  if ShellExecuteEx(@sei)
  then Ph:= sei.hProcess
  else
   begin
    MessageError(SysErrorMessage(GetLastError));
    EXIT(FALSE);
   end;

  WHILE MsgWaitForMultipleObjects(1, sei.hProcess, bWaitAll, WaitTime, (QS_POSTMESSAGE Or QS_SENDMESSAGE)) <> WAIT_OBJECT_0
   DO Application.ProcessMessages;

  CloseHandle(Ph);
  Result := TRUE;
end;










{-------------------------------------------------------------------------------------------------------------
   SHELLEXEC UTILS
-------------------------------------------------------------------------------------------------------------}
procedure ExecuteURL(URL: string);
begin
  {ToDo 1: encode all other special URL chars}
  URL := StringReplace(URL, '"', '%22', [rfReplaceAll]);
  ExecuteShell(URL, '', TRUE, SW_SHOWNORMAL);
end;


procedure ExecuteSendEmail(EmailAddress: string);
begin
 Winapi.ShellAPI.ShellExecute(0, 'mailto:', PChar(EmailAddress), NIL, NIL, SW_SHOW);
end;


procedure ExecuteExplorer(Path: string);                                                     { This will open Explorer in the specified folder. 'Execute' will browse the folder not explore }
begin
 if DirectoryExistMsg(Path)
 then Winapi.ShellAPI.ShellExecute(0, 'explore', PChar(path), NIL, NIL, SW_SHOW);
end;


function ExecuteExplorerSelect(FileName: string): boolean;                                   { Execute Win Explorer and select the specified file. http://stackoverflow.com/questions/15300999/open-windows-explorer-directory-select-a-specific-file-in-delphi  }
VAR IIDL: PItemIDList;
begin
  Result := FALSE;
  IIDL   := ILCreateFromPath(PChar(FileName));
  if IIDL <> NIL then
    TRY
      Result := SHOpenFolderAndSelectItems(IIDL, 0, NIL, 0) = S_OK;
    FINALLY
      ILFree(IIDL);
    END;
end;


{ Open Control Panel to 'Screen Resolution' tab
  BETTER ALTERNATIVE:
     http://stackoverflow.com/questions/18919089/how-to-execute-items-in-control-panel
  more:
     http://msdn.microsoft.com/en-us/library/windows/desktop/cc144191%28v=vs.85%29.aspx }
procedure ExecuteControlPanel_ScreenRes;
VAR
  App        : String;
  Params     : String;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  App:= GetWinDir+ 'system32\control.exe';
  if NOT FileExistsMsg(App) then EXIT;

  Params := 'desk.cpl,Settings@Settings';
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  if NOT CreateProcess(NIL, PChar(App+' '+Params), nil, nil, False, 0, nil, nil, StartupInfo, ProcessInfo)
  then RaiseLastOSError;
end;


end.
