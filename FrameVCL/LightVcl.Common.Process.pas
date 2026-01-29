UNIT LightVcl.Common.Process;

{=============================================================================================================
   SYSTEM
   2024.06
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
==============================================================================================================

  System function to access:
     Running processes

  Also:  https://www.experts-exchange.com/questions/20290838/Killing-Exes-running-from-a-delphi-application.html

   In this group:
     * LightVcl.Common.Shell.pas
     * csSystem.pas
     * csWindow.pas
     * LightVcl.Common.WindowMetrics.pas
     * LightVcl.Common.ExecuteProc.pas
     * LightVcl.Common.ExecuteShell.pas

=============================================================================================================}

INTERFACE
USES
   Winapi.Windows,
   System.SysUtils, TlHelp32;


 function ProcessRunning   (CONST ExeFileName: string): Boolean;
 function KillProcess      (CONST ExeName: string): Boolean;

{
 https://stackoverflow.com/questions/42637165/bizarre-behaviour-in-simple-delphi-code
 function GetProcessExeFromHandle(hWnd: HWND): string;
 function GetProcessExeFromName  (Caption: string): string; }

IMPLEMENTATION




{ Returns True if the specified process is found running.
  Drawbacks:
     The Process.szExeFile name does not contain the full path so in case there are multiple processes with
     the same file name, but running from different paths we won't be able to differentiate between them! }
function ProcessRunning(CONST ExeFileName: string): Boolean;
VAR
  SnapshotHandle: THandle;
  Process: TProcessEntry32;
  LowExeName, LowProcName: string;
begin
  if ExeFileName = ''
  then raise Exception.Create('ProcessRunning: ExeFileName parameter cannot be empty');

  Result:= FALSE;
  LowExeName:= LowerCase(ExeFileName);
  Process.dwSize:= SizeOf(Process);

  SnapshotHandle:= CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if SnapshotHandle = INVALID_HANDLE_VALUE then EXIT;

  TRY
    if Process32First(SnapshotHandle, Process) then
      REPEAT
        LowProcName:= LowerCase(Process.szExeFile);
        if (LowProcName = LowExeName)
        OR (LowProcName = LowerCase(ExtractFileName(ExeFileName)))
        then EXIT(TRUE);
      UNTIL NOT Process32Next(SnapshotHandle, Process);
  FINALLY
    CloseHandle(SnapshotHandle);
  END;
end;


{ Terminates ALL running processes that match ExeName.
  Returns True if all matching processes were terminated successfully,
  or if no matching processes were found. }
function KillProcess(CONST ExeName: string): Boolean;
VAR
  SnapshotHandle: THandle;
  ProcEntry: TProcessEntry32;
  ProcessHandle: THandle;
begin
  if ExeName = ''
  then raise Exception.Create('KillProcess: ExeName parameter cannot be empty');

  Result:= TRUE;  { Will be set to FALSE upon any failure }

  SnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if SnapshotHandle = INVALID_HANDLE_VALUE
  then EXIT(FALSE);   // or: Log('CreateToolhelp32Snapshot failed with error: ' + SysErrorMessage(GetLastError()));

  try
    ProcEntry.dwSize := SizeOf(ProcEntry);
    if Process32First(SnapshotHandle, ProcEntry)
    then
        repeat
          // Compare the filename part of ProcEntry.szExeFile (case-insensitive)
          if SameText(ExtractFileName(ProcEntry.szExeFile), ExeName) then
          begin
            // Try to open the process with rights to terminate it
            ProcessHandle := OpenProcess(PROCESS_TERMINATE, False, ProcEntry.th32ProcessID);
            if ProcessHandle <> 0
            then // Check if OpenProcess succeeded (returns NULL on failure)
              begin
                try
                  // Attempt to terminate the process. The second parameter to TerminateProcess is the desired exit code for the process
                  if NOT TerminateProcess(ProcessHandle, 0)
                  then Result:= FALSE;// or: Log('TerminateProcess failed for PID ' + IntToStr(ProcEntry.th32ProcessID) + '. Error: ' + SysErrorMessage(GetLastError()));
                  // If one termination fails, the function will ultimately return FALSE.
                  // We could choose to 'Break;' or 'Exit(FALSE);' here if you want to stop on first failure.
                finally
                  CloseHandle(ProcessHandle); // Crucial: Always close the handle obtained by OpenProcess
                end;
              end
            else Result:= FALSE; {OpenProcess failed: Log('OpenProcess failed for PID ' + IntToStr(ProcEntry.th32ProcessID) + '. Error: ' + SysErrorMessage(GetLastError())); }
          end;
        until NOT Process32Next(SnapshotHandle, ProcEntry)
      // If the loop completes and Result is still TRUE, it means either:
      // 1. No processes matching ExeName were found.
      // 2. All found matching processes were successfully opened and TerminateProcess succeeded for them.
      // Both scenarios are considered "success" by the initial Result := TRUE;
    else
      // Process32First returned False.
      // This could mean no processes are running at all (GetLastError() would be ERROR_NO_MORE_FILES), or another error occurred.
      // Any failure of Process32First (even if just no processes) is treated as a failure of this function.
      // If we want to treat "no processes found by Process32First" as success (Result := TRUE), we add: if GetLastError() <> ERROR_NO_MORE_FILES then Result := FALSE; else Result remains TRUE.
      Result:= FALSE;  {if LastErrorValue <> ERROR_NO_MORE_FILES then Example: Log('Process32First failed with error: ' + SysErrorMessage(GetLastError())); }
  finally
    CloseHandle(SnapshotHandle); // Ensure the snapshot handle is always closed
  end;
end;


{
function GetProcessExeFromHandle(hWnd: HWND): string;
var
  ProcID: DWORD;
  ProcessInfo: TProcessInfo;
  ProcessItem: TProcessItem;
begin
  Result:= '';
  GetWindowThreadProcessId(hWnd, @ProcID);
  ProcessInfo := TProcessInfo.Create(nil);
  try
    ProcessItem := ProcessInfo.RunningProcesses.FindByID(ProcID);
    if Assigned(ProcessItem)
    then Result:= ProcessItem.ExeFile;
  finally
    FreeAndNil(ProcessInfo);
  end;
end;


function GetProcessExeFromName(Caption: string): string;
begin
  Result:= GetProcessExeFileFromHandle(FindWindow(NIL, cap));
end;  }




end.

