UNIT LightCore.Win.System;

{$IFNDEF MSWINDOWS}
  {$MESSAGE FATAL 'LightCore.Win.System is Windows-only. Its package LightCore.Win builds for Win32 and Win64 only.'}
{$ENDIF}

{=============================================================================================================
   2026.09.30
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   System-level Windows API utilities

   Provides access to:
     - Windows Services (start, stop, query status)
     - The text of a Win32 error code
     - The user name in the formats of the Windows function GetUserNameExW, for example GODZILLA\John Lennon

   See also:
     LightCore.System.pas         - computer and user names, fonts, BIOS, display modes, print screen, mouse jiggle
     LightVcl.Common.System.pas   - the busy mouse cursor of a VCL application

   Windows-only unit, package LightCore.Win. The four service routines drive the Windows Service Control Manager, GetWin32ErrorString asks the Windows function FormatMessage for the text of a Win32 error code, and GetUserNameEx calls GetUserNameExW in secur32.dll. None of these exists off Windows, so a build for Android, macOS or iOS stops at the top of this unit with a fatal compiler message.
=============================================================================================================}

INTERFACE
USES
   Winapi.Windows, Winapi.WinSvc, System.SysUtils;


{==================================================================================================
   SYSTEM SERVICES
==================================================================================================}
 function ServiceStart        (CONST aMachine, aServiceName: string): Boolean;
 function ServiceStop         (CONST aMachine, aServiceName: string): Boolean;
 function ServiceGetStatus    (CONST sMachine, sService: string): DWord;
 function ServiceGetStatusName(CONST sMachine, sService: string): string;


{==================================================================================================
   SYSTEM COMPUTER INFO
==================================================================================================}
 function  GetUserNameEx (ANameFormat: Cardinal): string;                                    { source http://stackoverflow.com/questions/8446940/how-to-get-fully-qualified-domain-name-on-windows-in-delphi }


{==================================================================================================
   SYSTEM API
==================================================================================================}
 function GetWin32ErrorString(ErrorCode: DWORD): string;



IMPLEMENTATION




function GetWin32ErrorString(ErrorCode: DWORD): string;
var
  Buffer: array[0..1023] of Char;
  LangID: Word;
begin
  if ErrorCode = ERROR_SUCCESS // ERROR_SUCCESS is 0
  then Result := 'Operation completed successfully.'
  else
    begin
      LangID := MakeLangID(LANG_NEUTRAL, SUBLANG_DEFAULT); // Default system language
      if FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS, nil,
                       ErrorCode,
                       LangID,
                       Buffer,
                       Length(Buffer) - 1, // nSize is in TCHARs, not bytes! SizeOf(Buffer) would declare 2048 chars for a 1024-char buffer and let FormatMessage overrun the stack. -1 for null terminator space.
                       nil) = 0
      then
        Result := 'Windows Error Code ' + IntToStr(ErrorCode) + ' (No system description available).'  // FormatMessage failed
      else
        begin
          Result := Buffer;
          Result := TrimRight(Result); // Remove trailing CRLF if present
        end;
    end;
end;




{--------------------------------------------------------------------------------------------------
                            GET COMPUTER INFO
--------------------------------------------------------------------------------------------------}

{ For 2, returns computer name + user name.
  Ex: GODZILLA\John Lennon }
function GetUserNameEx(ANameFormat: Cardinal): string;
{See the constants defined in WinApi.Windows.pas EXTENDED_NAME_FORMAT enum.
  NameUnknown            = 0;
  NameFullyQualifiedDN   = 1;
  NameSamCompatible      = 2;
  NameDisplay            = 3;
  NameUniqueId           = 6;
  NameCanonical          = 7;
  NameUserPrincipal      = 8;
  NameCanonicalEx        = 9;
  NameServicePrincipal   = 10;
  NameDnsDomain          = 12;}
var
  Buf: array[0..511] of WideChar; // Use WideChar for Unicode support.
  BufSize: ULONG;                 // ULONG matches the parameter type.
  Secur32: HMODULE;
  GetUserNameEx: function(NameFormat: Cardinal; lpNameBuffer: LPWSTR; var nSize: ULONG): BOOL; stdcall;
begin
  Result := '';
  Secur32 := LoadLibrary('secur32.dll'); // Explicitly load the library.
  if Secur32 = 0
  then RAISE Exception.Create('Unable to load secur32.dll.');
  try
    @GetUserNameEx := GetProcAddress(Secur32, 'GetUserNameExW'); // Use Unicode version.
    if not Assigned(GetUserNameEx) then
      raise Exception.Create('GetUserNameExW function not found in secur32.dll.');
    BufSize := Length(Buf);
    if GetUserNameEx(ANameFormat, Buf, BufSize)
    then Result := WideCharToString(Buf)
    else RaiseLastOSError; // Raise an error if the function call fails.
  finally
    FreeLibrary(Secur32); // Ensure the library is freed.
  end;
end;




{--------------------------------------------------------------------------------------------------
   SERVICES

   aMachine: UNC path (e.g., '\\ServerName') or empty string for local machine.
   aServiceName: The short service name (not display name).
   Source: BlackBox.pas
--------------------------------------------------------------------------------------------------}

function ServiceStart(CONST aMachine, aServiceName: string): boolean;
var
   h_manager,h_svc: SC_Handle;
   svc_status: TServiceStatus;
   Temp: PChar;
   dwCheckPoint: DWord;
begin
  svc_status.dwCurrentState := SERVICE_STOPPED;  { Initialize to known state }
  h_manager := OpenSCManager(PChar(aMachine), nil,SC_MANAGER_CONNECT);

  if h_manager > 0 then begin
    h_svc := OpenService(h_manager, PChar(aServiceName),
                         SERVICE_START or SERVICE_QUERY_STATUS);
    if h_svc > 0 then begin
      temp := nil;
      if (StartService(h_svc,0,temp)) then
        begin
          if (QueryServiceStatus(h_svc,svc_status)) then begin
            { Poll only while START_PENDING (MSDN pattern). The previous condition
              'while SERVICE_RUNNING <> state' looped forever when the service failed
              to start and fell back to STOPPED with a non-incrementing checkpoint
              (0 < 0 never breaks) - an infinite Sleep(0) spin. }
            while (SERVICE_START_PENDING = svc_status.dwCurrentState) do begin
              dwCheckPoint := svc_status.dwCheckPoint;
              Sleep(svc_status.dwWaitHint);
              if (not QueryServiceStatus(h_svc,svc_status)) then break;
              if (svc_status.dwCheckPoint < dwCheckPoint) then begin
                // QueryServiceStatus didn't increment dwCheckPoint
                break;
              end;
            end;
          end;
        end
      else
        QueryServiceStatus(h_svc, svc_status);  { StartService failed (e.g. service already running) - read the real state so the Result check below is meaningful }
      CloseServiceHandle(h_svc);
    end;
    CloseServiceHandle(h_manager);
  end;

  Result := (SERVICE_RUNNING = svc_status.dwCurrentState);
end;


{ Stops a Windows service and waits for it to reach STOPPED state.
  Returns TRUE if service is stopped. }
function ServiceStop(CONST aMachine, aServiceName: string): boolean;
var h_manager,h_svc   : SC_Handle;
    svc_status     : TServiceStatus;
    dwCheckPoint : DWord;
begin
  svc_status.dwCurrentState := SERVICE_RUNNING;  { Initialize to known state ('not stopped'). Without this, every failure path below (manager/service cannot be opened) made the final Result check read an UNINITIALIZED stack record - random TRUE/FALSE. }
  h_manager:=OpenSCManager(PChar(aMachine),nil,SC_MANAGER_CONNECT);

  if h_manager > 0 then begin
    h_svc := OpenService(h_manager,PChar(aServiceName), SERVICE_STOP or SERVICE_QUERY_STATUS);

    if h_svc > 0 then
     begin
       if(ControlService(h_svc,SERVICE_CONTROL_STOP,svc_status)) then
         begin
           if(QueryServiceStatus(h_svc,svc_status))then
             { Poll only while STOP_PENDING (MSDN pattern). The previous condition
               'while SERVICE_STOPPED <> state' looped forever when the service refused
               to stop and stayed RUNNING with a non-incrementing checkpoint. }
             while(SERVICE_STOP_PENDING = svc_status.dwCurrentState) do
             begin
               dwCheckPoint := svc_status.dwCheckPoint;
               Sleep(svc_status.dwWaitHint);

               if NOT QueryServiceStatus(h_svc,svc_status) then break;    // couldn't check status
               if (svc_status.dwCheckPoint < dwCheckPoint) then break;
             end;
         end
       else
         QueryServiceStatus(h_svc, svc_status);  { ControlService failed (e.g. service already stopped) - read the real state so an already-stopped service correctly returns TRUE }
       CloseServiceHandle(h_svc);
     end;
    CloseServiceHandle(h_manager);
  end;

  Result := (SERVICE_STOPPED = svc_status.dwCurrentState);
end;


// ================================
// Status Constants
// SERVICE_STOPPED
// SERVICE_RUNNING
// SERVICE_PAUSED
// SERVICE_START_PENDING
// SERVICE_STOP_PENDING
// SERVICE_CONTINUE_PENDING
// SERVICE_PAUSE_PENDING
// =================================
function ServiceGetStatus(CONST sMachine, sService: string): DWord;   { From BlackBox.pas }
var h_manager,h_svc : SC_Handle;
    service_status  : TServiceStatus;
    hStat           : DWord;
begin
  hStat := 0;
  h_manager := OpenSCManager(PChar(sMachine) ,nil,SC_MANAGER_CONNECT);

  if h_manager > 0 then begin
    h_svc := OpenService(h_manager,PChar(sService),SERVICE_QUERY_STATUS);

    if h_svc > 0 then begin
      if(QueryServiceStatus(h_svc, service_status)) then
        hStat := service_status.dwCurrentState;

      CloseServiceHandle(h_svc);
    end;
    CloseServiceHandle(h_manager);
  end;

  Result := hStat;
end;


function ServiceGetStatusName(CONST sMachine, sService: string): string;   { From BlackBox.pas }
var Cmd : string;
    Status : DWord;
begin
  Status := ServiceGetStatus(sMachine,sService);
  case Status of
    SERVICE_STOPPED         : Cmd := 'STOPPED';
    SERVICE_RUNNING         : Cmd := 'RUNNING';
    SERVICE_PAUSED          : Cmd := 'PAUSED';
    SERVICE_START_PENDING   : Cmd := 'STARTING';
    SERVICE_STOP_PENDING    : Cmd := 'STOPPING';
    SERVICE_CONTINUE_PENDING: Cmd := 'RESUMING';
    SERVICE_PAUSE_PENDING   : Cmd := 'PAUSING';
  else
    Cmd := 'UNKNOWN STATE';
  end;
  Result := Cmd;
end;


end.

