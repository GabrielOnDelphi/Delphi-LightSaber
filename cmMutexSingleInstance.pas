UNIT cmMutexSingleInstance;

{=============================================================================================================
   2023.01
   See Copyright.txt
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
  System.SysUtils, Vcl.Forms, WinApi.Windows;

procedure FreeMutex;
function  IsSingleInstance(MutexName : string):Boolean;
procedure CreateMutexOrDie(MutexName: string);  // Utils


IMPLEMENTATION

VAR
  SingleMutex: THandle = 0;


//  Creates a mutex to see if the program is already running.
//  By default (lpMutexAttributes =nil) created mutexes are accessible only by the user running the process.
//  We need our mutexes to be accessible to all users, so that the mutex detection can work across user sessions.
//  I.e. both the current user account and the System (Service) account.
//  To do this we use a security descriptor with a null DACL.
function IsSingleInstance(MutexName : string):boolean;
CONST
    MUTEX_GLOBAL = 'Global\'; //Prefix to explicitly create the object in the global or session namespace. I.e. both client app (local user) and service (system account)
VAR SecurityDesc: TSecurityDescriptor;
    SecurityAttr: TSecurityAttributes;
    ErrCode : integer;
begin
  InitializeSecurityDescriptor(@SecurityDesc, SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(@SecurityDesc, True, nil, False);
  SecurityAttr.nLength:=SizeOf(SecurityAttr);
  SecurityAttr.lpSecurityDescriptor:=@SecurityDesc;
  SecurityAttr.bInheritHandle:=False;

  //  The mutex is created in the global name space which makes it possible to access across user sessions.
  SingleMutex := CreateMutex(@SecurityAttr, True, PChar(MUTEX_GLOBAL + MutexName));
  ErrCode := GetLastError;

  //  If the function fails, the return value is 0
  //  If the mutex is a named mutex and the object existed before this function
  //  call, the return value is a handle to the existing object, GetLastError returns ERROR_ALREADY_EXISTS.
  if {(SingleMutex = 0) or } (ErrCode = ERROR_ALREADY_EXISTS)
  then
    begin
     Result:= false;
     CloseHandle(SingleMutex);
    end
  else
    Result:= TRUE; // Mutex object has not yet been created, meaning that no previous instance has been created.

  // The Mutexhandle is not closed because we want it to exist during the lifetime of the application. The system closes the handle automatically when the process terminates.
end;


procedure FreeMutex;
begin
  if SingleMutex <> 0 then
   begin
    CloseHandle(SingleMutex);
    SingleMutex:= 0;
   end;
end;



procedure CreateMutexOrDie(MutexName: string);  { Not recommended }
begin
  if NOT IsSingleInstance(MutexName) then
   begin
    Application.ShowMainForm:= FALSE;     // The mutex did exist, so the application is running. Terminate it in this case.
    Application.Terminate;
   end;
end;



INITIALIZATION
FINALIZATION
  FreeMutex;

end.

