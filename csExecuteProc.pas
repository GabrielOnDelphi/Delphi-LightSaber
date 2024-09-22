UNIT csExecuteProc;

{=============================================================================================================
   SYSTEM - Execute Process
   2023.01
   See Copyright.txt
==============================================================================================================

   Execute a process, using the CreateProcess API (recommended)

   ShellExecute vs CreateProcess
      CreateProcess - is not as straigth forward as ShellExecute but it allows you more control.
      ShellExecute  - is easyer to use but you don't have a lot of control over it.
      To obtain information about the application that is launched as a result of calling ShellExecute, use ShellExecuteEx.

   For cross-platform see:
      Jedi.Execute - www.delphicorner.f9.co.uk/articles/wapi4.htm

   In this group:
     * csShell.pas
     * csSystem.pas
     * csWindow.pas
     * csWindowMetrics.pas
     * csExecuteProc.pas
     * csExecuteShell.pas
     * csProcess.pas

   Tester
      c:\MyProjects\Project Testers\Tester Execute program (ShellExecute, CreateProcess)\

=============================================================================================================}

INTERFACE
USES
    WinApi.Windows, System.SysUtils, Vcl.Forms;


 function  ExecuteProcMsg     (ExeFile: string): Boolean;
 function  ExecuteProc        (ExeFile: string; WindowState: Integer= SW_SHOWNORMAL): Boolean;
 function  ExecuteAndWait     (ExeFile: string; Params: string= ''; Hide: Boolean= FALSE; WaitTime: Cardinal= INFINITE): Cardinal;

 {$IFDEF msWindows}
 procedure ExecuteAndGetOutDyn(CONST CmdLine: string; CONST Output: TProc<string>; AntiFreeze: Boolean; Hide: Boolean= TRUE; Timeout: Cardinal= 100);  { Run a DOS program and retrieve its output dynamically while it is running. } {$ENDIF}
 function  ExecuteAndGetOut   (CONST CmdLine: string; Work: string = 'C:\'): string;




IMPLEMENTATION

USES
   cmIO.Win, ccCore, cbDialogs;


{--------------------------------------------------------------------------------------------------
   CreateProcess

   Accepts also cmd line parameters. Example:  C:\MyApp.exe /c
   Does not accept:  mailto:address@yahho.com or *.lnk files or Internet pages (www)
   Shows error msg if file not found.

   Source: https://stackoverflow.com/questions/46672282/how-to-run-a-screensaver-in-config-mode-with-shellexecute-os-overrides-my-she#46672529
   Similar: https://stackoverflow.com/questions/27249995/delphi-7-shellexecute-command-not-working-in-situations
---------------------------------------------------------------------------------------------------}
function ExecuteProcMsg(ExeFile: string): Boolean;
begin
 Result:= ExecuteProc(ExeFile);                                                                     { If the function succeeds, the return value is the instance handle of the application that was run, or the handle of a dynamic data exchange (DDE) server application. If the function fails, the return value is an error value that is less than or equal to 32. The following table lists these error Value: }
 if NOT Result
 then MesajError('Cannot execute file '+ CRLFw+ ExeFile);
end;


// Warning: Cannot run a program if it requires admin rights!
// See this: https://stackoverflow.com/questions/14130282/launch-an-exe-with-elevated-privileges-from-a-normal-non-elevated-one
//     and https://stackoverflow.com/questions/74552937/delphi-createprocess-as-administrator
function ExecuteProc(ExeFile: string; WindowState: Integer= SW_SHOWNORMAL): Boolean;
VAR
  SI: TStartupInfo;
  PI: TProcessInformation;
begin
  UniqueString(ExeFile);

  ZeroMemory(@SI, SizeOf(SI));
  SI.cb := SizeOf(SI);
  SI.dwFlags := STARTF_USESHOWWINDOW;
  SI.wShowWindow := WindowState;

  Result:= CreateProcess(nil, PChar(ExeFile), nil, nil, FALSE, CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, NIL, NIL{WorkingFolder}, SI, PI);
  if Result then
   begin
    CloseHandle(PI.hThread);
    CloseHandle(PI.hProcess);
   end;
end;






{-------------------------------------------------------------------------------------------------------------
 Execute a program and wait for it to finish.

 WaitTime:
      The calling program cannot terminate until the called program exits unless WaitTime ms has passed.
      You can use INFINITE for WaitTime. Not recommended.
 Hide:
      Applies only to console (DOS) application. most GUI applications will not honnor it.

 https://github.com/jrsoftware/issrc/blob/master/Projects/SetupLdr.dpr
 https://stackoverflow.com/questions/26775794/hide-process-window-with-createprocess
-------------------------------------------------------------------------------------------------------------}

function ExecuteAndWait(ExeFile: string; Params: string= ''; Hide: Boolean= FALSE; WaitTime: Cardinal= INFINITE): Cardinal;
VAR
  CmdLine: String;
  ExitCode: Cardinal;
  dwCreationFlags: Cardinal;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  CmdLine := '"' + ExeFile + '" ' + Params;

  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  FillChar(ProcessInfo, SizeOf(ProcessInfo), #0);

  StartupInfo.cb := SizeOf(StartupInfo);
  if Hide
  then
   begin
    StartupInfo.wShowWindow := SW_HIDE;
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
    dwCreationFlags := CREATE_NO_WINDOW;
   end
  else
    dwCreationFlags := 0;

  if NOT CreateProcess(NIL, PChar(CmdLine), NIL, NIL, FALSE, dwCreationFlags, NIL, NIL, StartupInfo, ProcessInfo)
  then RaiseLastOSError;
  CloseHandle(ProcessInfo.hThread);

  { Wait for the process to terminate, processing messages in the meantime }
  REPEAT
    Application.ProcessMessages;
  UNTIL MsgWaitForMultipleObjects(1, ProcessInfo.hProcess, FALSE, WaitTime, QS_ALLINPUT) <> WAIT_OBJECT_0+1;

  { Now that the process has exited, process any remaining messages.
    There may be an asynchronously-sent "restart request" message still queued if MWFMO saw the process terminate before checking for new  messages.) }
  Application.ProcessMessages;
  GetExitCodeProcess(ProcessInfo.hProcess, ExitCode);

  CloseHandle(ProcessInfo.hProcess);
  Result := ExitCode;
end;





{-------------------------------------------------------------------------------------------------------------
  Run a DOS program and retrieve its output (static)
  Based on CreateProcess.

  http://stackoverflow.com/questions/9119999/getting-output-from-a-shell-dos-app-into-a-delphi-app
-------------------------------------------------------------------------------------------------------------}
function ExecuteAndGetOut(CONST CmdLine: string; Work: string = 'C:\'): string;                                                                                                                          { Old name: GetDosOutput }
var
  SecAtrrs: TSecurityAttributes;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  WasOK: Boolean;
  pCommandLine: array[0..255] of AnsiChar;
  BytesRead: Cardinal;
  WorkDir: string;
  Handle: Boolean;
begin
  Result := '';
  with SecAtrrs DO
   begin
    nLength := SizeOf(SecAtrrs);
    bInheritHandle := True;
    lpSecurityDescriptor := nil;
   end;

  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SecAtrrs, 0);
  TRY
    WITH StartupInfo DO
     begin
      FillChar(StartupInfo, SizeOf(StartupInfo), 0);
      cb         := SizeOf(StartupInfo);
      dwFlags    := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      wShowWindow:= SW_HIDE;                                         //SW_SHOW SW_HIDE;
      hStdInput  := GetStdHandle(STD_INPUT_HANDLE);                  // don't redirect stdin
      hStdOutput := StdOutPipeWrite;
      hStdError  := StdOutPipeWrite;
     end;
    WorkDir:= Work;
    Handle := CreateProcess(NIL, PChar('cmd.exe /C ' + CmdLine), NIL, NIL, TRUE, 0, NIL, PChar(WorkDir), StartupInfo, ProcessInfo);   { The /c parameters means that I want to close the DOS box when the program executed in it is over }
    CloseHandle(StdOutPipeWrite);

    if Handle then
      TRY
        REPEAT
          WasOK := WinApi.Windows.ReadFile(StdOutPipeRead, pCommandLine, 255, BytesRead, NIL);
          if BytesRead > 0 then
           begin
            pCommandLine[BytesRead] := #0;
            Result := Result + string(pCommandLine);                                               {Here I could do something like this: Log.AddInfo(string(pCommandLine)); }
           end;
        UNTIL NOT WasOK or (BytesRead = 0);
        WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      FINALLY
        CloseHandle(ProcessInfo.hThread);
        CloseHandle(ProcessInfo.hProcess);
      end;
  FINALLY
    CloseHandle(StdOutPipeRead);
  end;
end;




{--------------------------------------------------------------------------------------------------
  Run a DOS program and retrieve its output dynamically while it is running.
  The Output parameter is a reference to a procedure (like this):

    procedure DoOutput(Text: string);
    begin
     Mesaj(Text);
    end;

  Source:
      http://stackoverflow.com/questions/25723807/execute-dos-program-and-get-output-dynamically
--------------------------------------------------------------------------------------------------}
{$IFDEF msWindows}
{$WARN SYMBOL_PLATFORM OFF}
//also see: https://gist.github.com/mmmunk/cbe059f06c42f60724ccdcf58f60a2b9
procedure ExecuteAndGetOutDyn(CONST CmdLine: string; CONST Output: TProc<string>; AntiFreeze: Boolean; Hide: Boolean= TRUE; Timeout: Cardinal= 100);  { Run a DOS program and retrieve its output dynamically while it is running. As TimeOut use 100 (ms) }
CONST
  InheritHandleSecurityAttributes: TSecurityAttributes = (nLength: SizeOf(TSecurityAttributes); bInheritHandle: True);
VAR
  FileSize: Int64;
  si: TStartupInfo;
  pi: TProcessInformation;
  WaitRes, BytesRead: DWORD;
  ProcCreationFlags: Cardinal;
  hReadStdout, hWriteStdout: THandle;
  AnsiBuffer: array [0 .. 1024 - 1] of AnsiChar;
begin
 Win32Check(CreatePipe(hReadStdout, hWriteStdout, @InheritHandleSecurityAttributes, 0));
 TRY
   si := Default (TStartupInfo);
   si.cb := SizeOf(TStartupInfo);
   si.dwFlags := STARTF_USESTDHANDLES;
   si.hStdOutput := hWriteStdout;
   si.hStdError := hWriteStdout;

   if Hide
   then ProcCreationFlags:= CREATE_NO_WINDOW+NORMAL_PRIORITY_CLASS
   else ProcCreationFlags:= CREATE_NEW_PROCESS_GROUP+NORMAL_PRIORITY_CLASS;

   Win32Check(CreateProcess(NIL, PChar(CmdLine), NIL, NIL, True, ProcCreationFlags, NIL, NIL, si, pi));

   TRY
    WHILE True DO
     begin
      WaitRes := WaitForSingleObject(pi.hProcess, Timeout);
      Win32Check(WaitRes <> WAIT_FAILED);
      WHILE True DO
       begin
        if AntiFreeze then Application.ProcessMessages;    // Not good man! Not good!

        Win32Check(GetFileSizeEx(hReadStdout, FileSize));
        if FileSize = 0 then Break;
        Win32Check(WinApi.Windows.ReadFile(hReadStdout, AnsiBuffer, SizeOf(AnsiBuffer) - 1, BytesRead, NIL));
        if BytesRead = 0 then Break;
        AnsiBuffer[BytesRead] := #0;
        OemToAnsi(AnsiBuffer, AnsiBuffer);
        if Assigned(Output) then Output(string(AnsiBuffer));
       end;
      if WaitRes = WAIT_OBJECT_0 then Break;
     end;
   FINALLY
     CloseHandle(pi.hProcess);
     CloseHandle(pi.hThread);
   END;
 FINALLY
   CloseHandle(hReadStdout);
   CloseHandle(hWriteStdout);
 END;
end;
{$WARN SYMBOL_PLATFORM On}
{$ENDIF}





end.
