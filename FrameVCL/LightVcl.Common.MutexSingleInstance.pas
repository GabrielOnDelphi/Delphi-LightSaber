UNIT LightVcl.Common.MutexSingleInstance;

{=============================================================================================================
   2023.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
==============================================================================================================

   Only allow one instance of the program to run at a time.
   FreeMutex is called automatically during finalization.

   Usage:

     Project Test;
     begin
       AppData.AppName:= 'MyApp';
       if NOT IsSingleInstance(AppData.AppName) then EXIT;
       Application.Initialize;
       AppData.CreateMainForm(TfrmStarter, frmStarter);
       Application.Run;
     end.

    This unit is not deprecated, but it was replaced by AppData.InstanceRunning which is easier to use.

=============================================================================================================}

INTERFACE

USES
  WinApi.Windows,
  System.SysUtils, Vcl.Forms;

procedure FreeMutex;
function  IsSingleInstance(CONST MutexName: string): Boolean;
procedure CreateMutexOrDie(CONST MutexName: string);  { Not recommended - calls Application.Terminate }


IMPLEMENTATION

VAR
  SingleMutex: THandle = 0;


{ Creates a mutex to see if the program is already running.
  By default (lpMutexAttributes=nil) created mutexes are accessible only by the user running the process.
  We need our mutexes to be accessible to all users, so that the mutex detection can work across user sessions.
  I.e. both the current user account and the System (Service) account.
  To do this we use a security descriptor with a null DACL. }
function IsSingleInstance(CONST MutexName: string): Boolean;
CONST
  MUTEX_GLOBAL = 'Global\';  { Prefix to create object in global namespace - accessible across user sessions }
VAR
  SecurityDesc: TSecurityDescriptor;
  SecurityAttr: TSecurityAttributes;
  ErrCode: Integer;
begin
  if MutexName = ''
  then raise Exception.Create('IsSingleInstance: MutexName parameter cannot be empty');

  { Release any previously created mutex to prevent handle leak if called multiple times }
  if SingleMutex <> 0 then
    begin
      CloseHandle(SingleMutex);
      SingleMutex:= 0;
    end;

  InitializeSecurityDescriptor(@SecurityDesc, SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(@SecurityDesc, TRUE, NIL, FALSE);
  SecurityAttr.nLength:= SizeOf(SecurityAttr);
  SecurityAttr.lpSecurityDescriptor:= @SecurityDesc;
  SecurityAttr.bInheritHandle:= FALSE;

  { The mutex is created in the global namespace which makes it possible to access across user sessions }
  SingleMutex:= CreateMutex(@SecurityAttr, TRUE, PChar(MUTEX_GLOBAL + MutexName));
  ErrCode:= GetLastError;

  { If the mutex already exists, another instance is running.
    Note: CreateMutex returns a valid handle even if mutex exists, but GetLastError returns ERROR_ALREADY_EXISTS }
  if ErrCode = ERROR_ALREADY_EXISTS then
    begin
      Result:= FALSE;
      CloseHandle(SingleMutex);
      SingleMutex:= 0;
    end
  else
    Result:= TRUE;  { This is the first instance }

  { The mutex handle is kept open for the lifetime of the application.
    The system closes it automatically when the process terminates. }
end;


procedure FreeMutex;
begin
  if SingleMutex <> 0 then
   begin
    CloseHandle(SingleMutex);
    SingleMutex:= 0;
   end;
end;



{ Creates a mutex or terminates the application if it already exists.
  Not recommended - prefer IsSingleInstance with explicit handling. }
procedure CreateMutexOrDie(CONST MutexName: string);
begin
  if NOT IsSingleInstance(MutexName) then
    begin
      Application.ShowMainForm:= FALSE;  { Hide main form before terminating }
      Application.Terminate;
    end;
end;



INITIALIZATION
  { No initialization needed }

FINALIZATION
  { Note: Finalization is used here to ensure the mutex is released even if
    the application crashes or terminates unexpectedly. The OS will also
    release it when the process ends, but explicit cleanup is preferred. }
  FreeMutex;

end.

