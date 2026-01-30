UNIT LightVcl.Common.Process;

{=============================================================================================================
   Process Management

   2026.01.30
   www.GabrielMoraru.com

==============================================================================================================

  System functions to query and control running processes:
     * ProcessRunning - Check if a process with given name is running
     * KillProcess    - Terminate all processes matching a given name

  Note:
     Process.szExeFile from TProcessEntry32 only contains the filename without path,
     so we cannot distinguish between processes with the same name running from different locations.

  Also see: https://www.experts-exchange.com/questions/20290838/Killing-Exes-running-from-a-delphi-application.html

  In this group:
     * LightVcl.Common.Shell.pas
     * csSystem.pas
     * csWindow.pas
     * LightVcl.Common.WindowMetrics.pas
     * LightVcl.Common.ExecuteProc.pas
     * LightVcl.Common.ExecuteShell.pas
     * LightVcl.Common.Process.pas

=============================================================================================================}

INTERFACE
USES
   Winapi.Windows,
   System.SysUtils, TlHelp32;


 function ProcessRunning   (CONST ExeFileName: string): Boolean;
 function KillProcess      (CONST ExeName: string): Boolean;

{ Historical note: GetProcessExeFromHandle and GetProcessExeFromName were considered
  but abandoned due to issues. See: https://stackoverflow.com/questions/42637165
  The implementations below reference TProcessInfo which is not available. }

IMPLEMENTATION




{ Returns True if the specified process is found running.

  Parameters:
    ExeFileName - Process name to search for. Can be just the filename (e.g., "notepad.exe")
                  or a full path (e.g., "C:\Windows\notepad.exe"). Only the filename part is used.

  Limitation:
    Process.szExeFile only contains the filename without path. If multiple processes with the
    same name run from different locations, we cannot distinguish between them. }
function ProcessRunning(CONST ExeFileName: string): Boolean;
VAR
  SnapshotHandle: THandle;
  Process: TProcessEntry32;
  TargetExeName: string;
begin
  if ExeFileName = ''
  then raise Exception.Create('ProcessRunning: ExeFileName parameter cannot be empty');

  Result:= FALSE;
  { Extract just the filename - szExeFile never contains path information }
  TargetExeName:= ExtractFileName(ExeFileName);
  Process.dwSize:= SizeOf(Process);

  SnapshotHandle:= CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if SnapshotHandle = INVALID_HANDLE_VALUE then EXIT;

  TRY
    if Process32First(SnapshotHandle, Process) then
      REPEAT
        if SameText(Process.szExeFile, TargetExeName)
        then EXIT(TRUE);
      UNTIL NOT Process32Next(SnapshotHandle, Process);
  FINALLY
    CloseHandle(SnapshotHandle);
  END;
end;


{ Terminates ALL running processes that match ExeName.

  Parameters:
    ExeName - Process name to kill (e.g., "notepad.exe"). Must be just the filename, not a path.

  Returns:
    TRUE  - All matching processes were terminated, or no matching processes found.
    FALSE - At least one matching process could not be terminated (insufficient rights,
            protected process, etc.), or process enumeration failed.

  Note:
    This function attempts to kill ALL instances of the named process.
    Requires PROCESS_TERMINATE right; may fail for elevated/system processes. }
function KillProcess(CONST ExeName: string): Boolean;
VAR
  SnapshotHandle: THandle;
  ProcEntry: TProcessEntry32;
  ProcessHandle: THandle;
begin
  if ExeName = ''
  then raise Exception.Create('KillProcess: ExeName parameter cannot be empty');

  Result:= TRUE;

  SnapshotHandle:= CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if SnapshotHandle = INVALID_HANDLE_VALUE
  then EXIT(FALSE);

  TRY
    ProcEntry.dwSize:= SizeOf(ProcEntry);
    if Process32First(SnapshotHandle, ProcEntry)
    then
        REPEAT
          { szExeFile only contains filename, but use ExtractFileName for safety }
          if SameText(ExtractFileName(ProcEntry.szExeFile), ExeName) then
          begin
            ProcessHandle:= OpenProcess(PROCESS_TERMINATE, False, ProcEntry.th32ProcessID);
            if ProcessHandle <> 0
            then
              begin
                TRY
                  { Exit code 0 signals normal termination }
                  if NOT TerminateProcess(ProcessHandle, 0)
                  then Result:= FALSE;
                  { Continue trying to kill other matching processes even if one fails }
                FINALLY
                  CloseHandle(ProcessHandle);
                END;
              end
            else
              Result:= FALSE; { OpenProcess failed - likely insufficient rights }
          end;
        UNTIL NOT Process32Next(SnapshotHandle, ProcEntry)
    else
      { Process32First failed - cannot enumerate processes }
      Result:= FALSE;
  FINALLY
    CloseHandle(SnapshotHandle);
  END;
end;


{-------------------------------------------------------------------------------------------------------------
   ARCHIVED CODE - Not functional (references undefined types TProcessInfo/TProcessItem)
   Kept for historical reference only.
-------------------------------------------------------------------------------------------------------------
function GetProcessExeFromHandle(hWnd: HWND): string;
var
  ProcID: DWORD;
  ProcessInfo: TProcessInfo;
  ProcessItem: TProcessItem;
begin
  Result:= '';
  GetWindowThreadProcessId(hWnd, @ProcID);
  ProcessInfo:= TProcessInfo.Create(nil);
  try
    ProcessItem:= ProcessInfo.RunningProcesses.FindByID(ProcID);
    if Assigned(ProcessItem)
    then Result:= ProcessItem.ExeFile;
  finally
    FreeAndNil(ProcessInfo);
  end;
end;


function GetProcessExeFromName(Caption: string): string;
begin
  Result:= GetProcessExeFileFromHandle(FindWindow(NIL, Caption));
end;
-------------------------------------------------------------------------------------------------------------}




end.

