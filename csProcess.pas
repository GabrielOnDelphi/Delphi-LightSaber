UNIT csProcess;

{=============================================================================================================
   SYSTEM
   2024.06
   See Copyright.txt
==============================================================================================================

  System function to access:
     Running processes

  Also:  https://www.experts-exchange.com/questions/20290838/Killing-Exes-running-from-a-delphi-application.html

   In this group:
     * csShell.pas
     * csSystem.pas
     * csWindow.pas
     * csWindowMetrics.pas
     * csExecuteProc.pas
     * csExecuteShell.pas

=============================================================================================================}

INTERFACE
USES
   Winapi.Windows,
   System.SysUtils, System.UITypes, TlHelp32;


 function ProcessRunning         (ExeFileName: string): Boolean;
 function KillTask(ExeFileName: string): Integer;

{
 https://stackoverflow.com/questions/42637165/bizarre-behaviour-in-simple-delphi-code
 function GetProcessExeFromHandle(hWnd: HWND): string;
 function GetProcessExeFromName  (Caption: string): string; }

IMPLEMENTATION




{ Returns True if the specified process if found running
  Drawnbacks:
     The Process.szExeFile name does not contain the full path so in case there are multiple processes with
     the same file name, but running from different paths we won't be able to differentiate between them! }
function ProcessRunning(ExeFileName: string): Boolean;
var
  SnapshotHandle: THandle;
  Process: TProcessEntry32;
begin
  Result := FALSE;
  ExeFileName:= LowerCase(ExeFileName);
  Process.dwSize := SizeOf(Process);

  SnapshotHandle:= CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  TRY
    if Process32First(SnapshotHandle, Process) then
      REPEAT
        VAR LowProcName:= LowerCase(Process.szExeFile);
        if (LowProcName = ExeFileName)
        OR (LowProcName = ExtractFileName(ExeFileName))
        then EXIT(True);
      UNTIL NOT Process32Next(SnapshotHandle, Process);
  FINALLY
    CloseHandle(SnapshotHandle);
  END;
end;



{ See this also: Find Out if a Program is Running - http://www.delphifaq.net/how-to-find-out-if-a-program-is-running/ }
function KillTask(ExeFileName: string): integer;
const
  PROCESS_TERMINATE=$0001;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: WinApi.TlHelp32.TProcessEntry32;
begin
  Result:= 0;

  FSnapshotHandle        := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := Sizeof(FProcessEntry32);
  ContinueLoop           := Process32First(FSnapshotHandle, FProcessEntry32);

  WHILE integer(ContinueLoop) <> 0 do
   begin
     if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) = UpperCase(ExeFileName)) OR (UpperCase(FProcessEntry32.szExeFile) = UpperCase(ExeFileName)))
     then Result := Integer(TerminateProcess(OpenProcess(PROCESS_TERMINATE, BOOL(0), FProcessEntry32.th32ProcessID), 0));
     ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
   end;

  CloseHandle(FSnapshotHandle);
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

