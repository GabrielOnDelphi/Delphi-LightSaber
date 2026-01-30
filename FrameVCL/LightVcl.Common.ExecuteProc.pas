UNIT LightVcl.Common.ExecuteProc;

{=============================================================================================================
   Execute Process
   
   2026.01.29
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
==============================================================================================================

   Execute external processes using the CreateProcess API (recommended over ShellExecute).

   ShellExecute vs CreateProcess:
      CreateProcess - More complex but provides full control over process creation, handles,
                      environment, and I/O redirection. Required for capturing stdout/stderr.
      ShellExecute  - Simpler API but limited control. Cannot capture output.
                      Use ShellExecuteEx for more information about launched application.

   Functions:
      ExecuteProc         - Launch process without waiting (fire and forget)
      ExecuteProcMsg      - Launch process with error message on failure
      ExecuteAndWait      - Launch process and wait for completion with exit code
      ExecuteAndGetOut    - Launch process and capture stdout (static, after completion)
      ExecuteAndGetOutDyn - Launch process and capture stdout dynamically (while running)

   Notes:
      - ExecuteProc cannot run programs requiring admin rights (UAC elevation)
      - For elevated execution, use ShellExecute with 'runas' verb
      - All functions accept command line parameters in ExeFile string

   For cross-platform see:
      Jedi.Execute - www.delphicorner.f9.co.uk/articles/wapi4.htm

   In this group:
     * LightVcl.Common.Shell.pas
     * csSystem.pas
     * csWindow.pas
     * LightVcl.Common.WindowMetrics.pas
     * LightVcl.Common.ExecuteProc.pas
     * LightVcl.Common.ExecuteShell.pas
     * csProcess.pas

   Tester:
      c:\MyProjects\Project Testers\Tester Execute program (ShellExecute, CreateProcess)\

=============================================================================================================}

INTERFACE
USES
    WinApi.Windows, System.SysUtils, Vcl.Forms;


{ Executes a process and shows error message if it fails }
function ExecuteProcMsg(ExeFile: string): Boolean;

{ Executes a process without waiting. Returns True if process was created.
  WindowState: SW_SHOWNORMAL, SW_HIDE, SW_MINIMIZE, etc. }
function ExecuteProc(ExeFile: string; WindowState: Integer = SW_SHOWNORMAL): Boolean;

{ Executes a process and waits for it to complete.
  Returns the process exit code.
  WaitTime: Maximum wait time in milliseconds, or INFINITE.
  Hide: If True, hides console window (for console apps only). }
function ExecuteAndWait(ExeFile: string; Params: string = ''; Hide: Boolean = FALSE; WaitTime: Cardinal = INFINITE): Cardinal;

{ Executes a command and captures its stdout after completion.
  CmdLine: Command to execute (passed to cmd.exe /C)
  Work: Working directory for the process }
function ExecuteAndGetOut(CONST CmdLine: string; Work: string = 'C:\'): string;

{$IFDEF MSWINDOWS}
{ Executes a command and captures its stdout dynamically while running.
  Output: Callback procedure receiving output text chunks.
  AntiFreeze: If True, calls Application.ProcessMessages during wait (use with caution).
  Timeout: Poll interval in milliseconds. }
procedure ExecuteAndGetOutDyn(CONST CmdLine: string; CONST Output: TProc<string>;
  AntiFreeze: Boolean; Hide: Boolean = TRUE; Timeout: Cardinal = 100);
{$ENDIF}



IMPLEMENTATION

USES
   LightVcl.Common.IO, LightCore, LightVcl.Common.Dialogs;


{---------------------------------------------------------------------------------------------------------------
   ExecuteProcMsg

   Executes a process using CreateProcess and shows an error message if it fails.
   Wrapper around ExecuteProc that provides user feedback on failure.

   Parameters:
     ExeFile - Full path to executable, may include command line arguments

   Returns:
     True if process was created successfully
---------------------------------------------------------------------------------------------------------------}
function ExecuteProcMsg(ExeFile: string): Boolean;
begin
  Result:= ExecuteProc(ExeFile);
  if NOT Result
  then MessageError('Cannot execute file' + CRLFw + ExeFile);
end;


{---------------------------------------------------------------------------------------------------------------
   ExecuteProc

   Creates a new process using CreateProcess API.
   The function returns immediately after process creation (does not wait for completion).

   Accepts command line parameters in ExeFile. Example: 'C:\MyApp.exe /c /debug'
   Does NOT support: mailto: links, .lnk files, URLs, or document associations.
   For those use cases, use ShellExecute instead.

   WARNING: Cannot run programs requiring admin rights (UAC elevation).
   See: https://stackoverflow.com/questions/14130282/launch-an-exe-with-elevated-privileges

   Parameters:
     ExeFile     - Executable path with optional command line arguments
     WindowState - Window display state (SW_SHOWNORMAL, SW_HIDE, SW_MINIMIZE, etc.)

   Returns:
     True if process was created successfully
---------------------------------------------------------------------------------------------------------------}
function ExecuteProc(ExeFile: string; WindowState: Integer = SW_SHOWNORMAL): Boolean;
VAR
  SI: TStartupInfo;
  PI: TProcessInformation;
begin
  if ExeFile = ''
  then raise Exception.Create('ExecuteProc: ExeFile parameter cannot be empty');

  { UniqueString ensures ExeFile has unique reference - required because
    CreateProcess may modify the command line buffer }
  UniqueString(ExeFile);

  ZeroMemory(@SI, SizeOf(SI));
  SI.cb:= SizeOf(SI);
  SI.dwFlags:= STARTF_USESHOWWINDOW;
  SI.wShowWindow:= WindowState;

  Result:= CreateProcess(NIL, PChar(ExeFile), NIL, NIL, FALSE, CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, NIL, NIL, SI, PI);

  if Result then
    begin
      { Close handles immediately - we don't need to wait or interact with the process }
      CloseHandle(PI.hThread);
      CloseHandle(PI.hProcess);
    end;
end;


{---------------------------------------------------------------------------------------------------------------
   ExecuteAndWait

   Executes a program and waits for it to finish.
   Keeps UI responsive by processing messages during the wait.

   Parameters:
     ExeFile  - Full path to executable
     Params   - Command line parameters (separate from ExeFile)
     Hide     - If True, hides console window. Only affects console applications;
                most GUI applications ignore this flag.
     WaitTime - Maximum time to wait in milliseconds, or INFINITE

   Returns:
     Process exit code

   Raises:
     EOSError if CreateProcess fails

   Note:
     Uses Application.ProcessMessages to keep UI responsive during wait.
     This is intentional for UI responsiveness but be aware of reentrancy issues.

   See:
     https://github.com/jrsoftware/issrc/blob/master/Projects/SetupLdr.dpr
     https://stackoverflow.com/questions/26775794/hide-process-window-with-createprocess
---------------------------------------------------------------------------------------------------------------}
function ExecuteAndWait(ExeFile: string; Params: string = ''; Hide: Boolean = FALSE; WaitTime: Cardinal = INFINITE): Cardinal;
VAR
  CmdLine: string;
  ExitCode: Cardinal;
  dwCreationFlags: Cardinal;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  if ExeFile = ''
  then raise Exception.Create('ExecuteAndWait: ExeFile parameter cannot be empty');

  CmdLine:= '"' + ExeFile + '" ' + Params;

  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  FillChar(ProcessInfo, SizeOf(ProcessInfo), 0);

  StartupInfo.cb:= SizeOf(StartupInfo);
  if Hide then
    begin
      StartupInfo.wShowWindow:= SW_HIDE;
      StartupInfo.dwFlags:= STARTF_USESHOWWINDOW;
      dwCreationFlags:= CREATE_NO_WINDOW;
    end
  else
    dwCreationFlags:= 0;

  if NOT CreateProcess(NIL, PChar(CmdLine), NIL, NIL, FALSE, dwCreationFlags, NIL, NIL, StartupInfo, ProcessInfo)
  then RaiseLastOSError;

  CloseHandle(ProcessInfo.hThread);

  { Wait for process to terminate while keeping UI responsive.
    ProcessMessages is used intentionally here to prevent UI freeze. }
  REPEAT
    Application.ProcessMessages;
  UNTIL MsgWaitForMultipleObjects(1, ProcessInfo.hProcess, FALSE, WaitTime, QS_ALLINPUT) <> WAIT_OBJECT_0 + 1;

  { Process any remaining messages after process exits.
    Important: there may be asynchronously-sent messages still queued
    if MWFMO saw the process terminate before checking for messages. }
  Application.ProcessMessages;
  GetExitCodeProcess(ProcessInfo.hProcess, ExitCode);

  CloseHandle(ProcessInfo.hProcess);
  Result:= ExitCode;
end;


{---------------------------------------------------------------------------------------------------------------
   ExecuteAndGetOut

   Executes a DOS/console command and captures its complete stdout output.
   Waits for the command to complete before returning.

   The command is executed via 'cmd.exe /C' which closes the console after completion.

   Parameters:
     CmdLine - Command to execute (will be passed to cmd.exe /C)
     Work    - Working directory for the command

   Returns:
     Complete stdout output as a string

   Example:
     Output := ExecuteAndGetOut('dir /b');
     Output := ExecuteAndGetOut('ping localhost', 'C:\Windows');

   Note:
     Output is captured in chunks using a fixed buffer. Large outputs are handled
     correctly through the read loop.

   See:
     http://stackoverflow.com/questions/9119999/getting-output-from-a-shell-dos-app-into-a-delphi-app
---------------------------------------------------------------------------------------------------------------}
function ExecuteAndGetOut(CONST CmdLine: string; Work: string = 'C:\'): string;
VAR
  SecAtrrs: TSecurityAttributes;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  WasOK: Boolean;
  Buffer: array[0..255] of AnsiChar;
  BytesRead: Cardinal;
  ProcessCreated: Boolean;
begin
  if CmdLine = ''
  then raise Exception.Create('ExecuteAndGetOut: CmdLine parameter cannot be empty');

  Result:= '';

  { Setup security attributes to allow handle inheritance }
  SecAtrrs.nLength:= SizeOf(SecAtrrs);
  SecAtrrs.bInheritHandle:= True;
  SecAtrrs.lpSecurityDescriptor:= NIL;

  { Create pipe for stdout redirection }
  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SecAtrrs, 0);
  TRY
    { Configure startup info for hidden window with redirected output }
    FillChar(StartupInfo, SizeOf(StartupInfo), 0);
    StartupInfo.cb:= SizeOf(StartupInfo);
    StartupInfo.dwFlags:= STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    StartupInfo.wShowWindow:= SW_HIDE;
    StartupInfo.hStdInput:= GetStdHandle(STD_INPUT_HANDLE);  { Don't redirect stdin }
    StartupInfo.hStdOutput:= StdOutPipeWrite;
    StartupInfo.hStdError:= StdOutPipeWrite;

    { Create the process. /C flag closes cmd.exe when command completes }
    ProcessCreated:= CreateProcess(NIL, PChar('cmd.exe /C ' + CmdLine), NIL, NIL, TRUE, 0, NIL, PChar(Work), StartupInfo, ProcessInfo);

    { Close write end of pipe - child process has its own handle now.
      We must close our handle so ReadFile will see EOF when child exits. }
    CloseHandle(StdOutPipeWrite);

    if ProcessCreated then
      TRY
        { Read all output from the pipe }
        REPEAT
          WasOK:= WinApi.Windows.ReadFile(StdOutPipeRead, Buffer, SizeOf(Buffer) - 1, BytesRead, NIL);
          if BytesRead > 0 then
            begin
              Buffer[BytesRead]:= #0;
              Result:= Result + string(Buffer);
            end;
        UNTIL NOT WasOK or (BytesRead = 0);
        WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      FINALLY
        CloseHandle(ProcessInfo.hThread);
        CloseHandle(ProcessInfo.hProcess);
      END;
  FINALLY
    CloseHandle(StdOutPipeRead);
  END;
end;


{---------------------------------------------------------------------------------------------------------------
   ExecuteAndGetOutDyn

   Executes a DOS/console command and captures its stdout dynamically while running.
   Unlike ExecuteAndGetOut, this function provides output in real-time through a callback.

   Parameters:
     CmdLine    - Command to execute
     Output     - Callback procedure receiving output text as it becomes available.
                  May be called multiple times with chunks of output.
     AntiFreeze - If True, calls Application.ProcessMessages during wait loops.
                  WARNING: Use with caution as this can cause reentrancy issues.
     Hide       - If True, hides console window
     Timeout    - Poll interval in milliseconds (how often to check for output)

   Example:
     ExecuteAndGetOutDyn('ping localhost',
       procedure(Text: string)
       begin
         Memo1.Lines.Add(Text);
       end,
       True, True, 100);

   Note:
     Application.ProcessMessages is used when AntiFreeze=True to keep UI responsive.
     Be aware this can cause reentrancy if callback triggers operations.

   See:
     http://stackoverflow.com/questions/25723807/execute-dos-program-and-get-output-dynamically
     https://gist.github.com/mmmunk/cbe059f06c42f60724ccdcf58f60a2b9
---------------------------------------------------------------------------------------------------------------}
{$IFDEF MSWINDOWS}
{$WARN SYMBOL_PLATFORM OFF}
procedure ExecuteAndGetOutDyn(CONST CmdLine: string; CONST Output: TProc<string>;
  AntiFreeze: Boolean; Hide: Boolean = TRUE; Timeout: Cardinal = 100);
CONST
  InheritHandleSecurityAttributes: TSecurityAttributes = (nLength: SizeOf(TSecurityAttributes); bInheritHandle: True);
VAR
  FileSize: Int64;
  SI: TStartupInfo;
  PI: TProcessInformation;
  WaitRes, BytesRead: DWORD;
  ProcCreationFlags: Cardinal;
  hReadStdout, hWriteStdout: THandle;
  AnsiBuffer: array[0..1023] of AnsiChar;
begin
  if CmdLine = ''
  then raise Exception.Create('ExecuteAndGetOutDyn: CmdLine parameter cannot be empty');

  Win32Check(CreatePipe(hReadStdout, hWriteStdout, @InheritHandleSecurityAttributes, 0));
  TRY
    SI:= Default(TStartupInfo);
    SI.cb:= SizeOf(TStartupInfo);
    SI.dwFlags:= STARTF_USESTDHANDLES;
    SI.hStdOutput:= hWriteStdout;
    SI.hStdError:= hWriteStdout;

    if Hide
    then ProcCreationFlags:= CREATE_NO_WINDOW + NORMAL_PRIORITY_CLASS
    else ProcCreationFlags:= CREATE_NEW_PROCESS_GROUP + NORMAL_PRIORITY_CLASS;

    Win32Check(CreateProcess(NIL, PChar(CmdLine), NIL, NIL, True, ProcCreationFlags, NIL, NIL, SI, PI));

    TRY
      WHILE True DO
        begin
          WaitRes:= WaitForSingleObject(PI.hProcess, Timeout);
          Win32Check(WaitRes <> WAIT_FAILED);

          { Read all available output }
          WHILE True DO
            begin
              { Keep UI responsive if requested (use with caution) }
              if AntiFreeze then Application.ProcessMessages;

              Win32Check(GetFileSizeEx(hReadStdout, FileSize));
              if FileSize = 0 then Break;

              Win32Check(WinApi.Windows.ReadFile(hReadStdout, AnsiBuffer, SizeOf(AnsiBuffer) - 1, BytesRead, NIL));
              if BytesRead = 0 then Break;

              AnsiBuffer[BytesRead]:= #0;
              OemToAnsi(AnsiBuffer, AnsiBuffer);

              if Assigned(Output) then Output(string(AnsiBuffer));
            end;

          if WaitRes = WAIT_OBJECT_0 then Break;
        end;
    FINALLY
      CloseHandle(PI.hProcess);
      CloseHandle(PI.hThread);
    END;
  FINALLY
    CloseHandle(hReadStdout);
    CloseHandle(hWriteStdout);
  END;
end;
{$WARN SYMBOL_PLATFORM ON}
{$ENDIF}


end.
