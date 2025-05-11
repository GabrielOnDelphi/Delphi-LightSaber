UNIT LightCom.SystemTime;

{=============================================================================================================
   SYSTEM TIME
   2025.01
   www.GabrielMoraru.com
=============================================================================================================}

INTERFACE
USES
   Winapi.Windows, Winapi.Messages, Winapi.WinSock, Winapi.WinSvc,
   System.Classes, System.SysUtils, System.Win.Registry, System.UITypes, System.IOUtils,
   Vcl.Controls, Vcl.Forms, Vcl.ClipBrd, Vcl.Dialogs,
   ccAppData, LightCom.AppData;


 procedure DelayEx(CONST ms : cardinal);

 function  GetSysFileTime: TDateTime;                                                              { gets current date from system file - prevents cracking}
 function  SystemTimeIsInvalid: Boolean;                                                           { returns true if the system time is bigger than current clock time }

 procedure CurrentSysTimeStore(SecretKey: string);                                                 { Stores current system clock time to a hidden registry key }
 function  CurrentSysTimeValid(SecretKey: string): Boolean;                                        { Read the last saved system time and compares it with current clock. If current value is smaller than the stored value it means that the clock time was set back }

 function  WindowsUpTime: TDateTime;
 function  UserIdleTime: Cardinal;
 { Also see Delphi function: FileAge }


IMPLEMENTATION

USES
   LightCom.Keyboard, ccIO, LightCom.IO, LightCom.Version, ccRegistry;



{--------------------------------------------------------------------------------------------------
   SYSTEM  TIME
--------------------------------------------------------------------------------------------------}
{ Also see this: http://docwiki.embarcadero.com/Libraries/XE4/en/System.Diagnostics.TStopwatch
  Time since boot
  Use it as: Date2FormatAuto(WindowsUpTime)  }
function WindowsUpTime: TDateTime;
begin
 Result:= GetTickCount / SecsPerDay / MSecsPerSec;   // GetTickCount accuracy is 15ms+  https://blogs.msdn.microsoft.com/oldnewthing/20050902-00/?p=34333
end;


{ Available only on Win2000 (and up) machines.  Details: To track a user's idle time you could hook keyboard and mouse activity. Note, however, that installing a system-wide message hook is a very invasive thing to do and should be avoided if possible, since it will require your hook DLL to be loaded into all processes. }
function UserIdleTime: Cardinal;
VAR
   liInfo: TLastInputInfo;
begin
   liInfo.cbSize := SizeOf(TLastInputInfo) ;
   GetLastInputInfo(liInfo) ;
   Result := (GetTickCount - liInfo.dwTime) DIV 1000;
end;



{ gets current date from system file - prevents user from reverse time }
function GetSysFileTime: TDateTime;
VAR strWinDir, strF: string;
begin
 Result:= 0;
 strWinDir:= '';
 strWinDir:= GetWinDir;
 Assert(DirectoryExistMsg(strWinDir));

 if LightCom.Version.IsNTKernel
 then
    begin
     strF:= strWinDir+ 'system32\config\software';                                                 { fisierul asta exista in Windowsul meu (XP) si arata data can a fost oprit Windows-ul ultima data. NU EXISTA IN WIN7!!! }
     if NOT FileExists(strF) then strF:= strWinDir+ 'config\software';
     if NOT FileExists(strF) then strF:= 'c:\pagefile.sys';
     if NOT FileExists(strF) then strF:= 'd:\pagefile.sys';
     if NOT FileExists(strF) then strF:= 'e:\pagefile.sys';
     if NOT FileExists(strF) then strF:= 'f:\pagefile.sys';
    end
 else
    begin
     strF:= strWinDir+ 'system.dat';                                                               { Platform is 'Windows 95' }
     if NOT FileExists(strF)
     then strF:= 'c:\win386.swp';
     if NOT FileExists(strF) then
       begin
        strF:= strWinDir+ 'SYSTEM.DA0';
        if NOT FileExists(strF)
        then strF:= strWinDir+ 'BOOTLOG.TXT';
       end;
    end;

 if FileExists(strF)
 then Result:= FileAge(strF);
end;



function SystemTimeIsInvalid: Boolean;                                                              { returns true if the SysFile time is bigger than current clock time }
VAR SystemTime: TDateTime;
begin
 SystemTime:= GetSysFileTime;                                                                      { exista si functia Delphi:  FileAge }
 if SystemTime= 0 then
  begin
   MessageDlg('Can''t get system time!', mtInformation, [mbOk], 0);
   SystemTime:= Now- 0.1;
  end;
 Result:= (Now < SystemTime);
end;



{ Non-blocking sleep/delay.
  Warning:
    Use it with care: Application.ProcessMessages will create a lot of problems!
    https://blog.dummzeuch.de/2018/09/29/calling-application-processmessages-in-a-delphi-program/ }
procedure DelayEx(CONST ms : Cardinal);
VAR Count: Cardinal;
begin
 Count:= GetTickCount;                    //ToDo: GetTickCount accuracy is > 15ms. Use cDebug TimerStart instead.     https://blogs.msdn.microsoft.com/oldnewthing/20050902-00/?p=34333
 REPEAT
  Sleep(1);                               // without this we get 100% CPU utilization because the loop is too tight
  Application.ProcessMessages;
 UNTIL (GetTickCount-Count)>= ms;
end;






{--------------------------------------------------------------------------------------------------
   TIME-PROTECTION
--------------------------------------------------------------------------------------------------}
procedure CurrentSysTimeStore(SecretKey: string);
begin
 RegWriteDate(HKEY_CURRENT_USER, SecretKey, 'System', Now, TRUE);
end;



function CurrentSysTimeValid(SecretKey: string): Boolean;
VAR LastTime: TDateTime;
begin
 LastTime:= RegReadDate(HKEY_CURRENT_USER, SecretKey, 'System');
 Result:= (LastTime<= Now) AND NOT SystemTimeIsInvalid;
end;





{--------------------------------------------------------------------------------------------------
   SERVICES
   aMachine is UNC path or local machine if left empty
--------------------------------------------------------------------------------------------------}
function ServiceStart(aMachine,aServiceName : string) : boolean;  { From BlackBox.pas }
var
   h_manager,h_svc: SC_Handle;
   svc_status: TServiceStatus;
   Temp: PChar;
   dwCheckPoint: DWord;
begin
  svc_status.dwCurrentState := 1;
  h_manager := OpenSCManager(PChar(aMachine), nil,SC_MANAGER_CONNECT);

  if h_manager > 0 then begin
    h_svc := OpenService(h_manager, PChar(aServiceName),
                         SERVICE_START or SERVICE_QUERY_STATUS);
    if h_svc > 0 then begin
      temp := nil;
      if (StartService(h_svc,0,temp)) then
        if (QueryServiceStatus(h_svc,svc_status)) then begin
          while (SERVICE_RUNNING <> svc_status.dwCurrentState) do begin
            dwCheckPoint := svc_status.dwCheckPoint;
            Sleep(svc_status.dwWaitHint);
            if (not QueryServiceStatus(h_svc,svc_status)) then break;
            if (svc_status.dwCheckPoint < dwCheckPoint) then begin
              // QueryServiceStatus didn't increment dwCheckPoint
              break;
            end;
          end;
        end;
      CloseServiceHandle(h_svc);
    end;
    CloseServiceHandle(h_manager);
  end;

  Result := (SERVICE_RUNNING = svc_status.dwCurrentState);
end;


function ServiceStop(aMachine,aServiceName : string) : boolean;    { From BlackBox.pas }
var h_manager,h_svc   : SC_Handle;
    svc_status     : TServiceStatus;
    dwCheckPoint : DWord;
begin
  h_manager:=OpenSCManager(PChar(aMachine),nil,SC_MANAGER_CONNECT);

  if h_manager > 0 then begin
    h_svc := OpenService(h_manager,PChar(aServiceName), SERVICE_STOP or SERVICE_QUERY_STATUS);

    if h_svc > 0 then
     begin
       if(ControlService(h_svc,SERVICE_CONTROL_STOP,svc_status)) then
         if(QueryServiceStatus(h_svc,svc_status))then
           while(SERVICE_STOPPED <> svc_status.dwCurrentState) do
           begin
             dwCheckPoint := svc_status.dwCheckPoint;
             Sleep(svc_status.dwWaitHint);

             if NOT QueryServiceStatus(h_svc,svc_status) then break;    // couldn't check status
             if (svc_status.dwCheckPoint < dwCheckPoint) then break;
           end;
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
function ServiceGetStatus(sMachine, sService: string ): DWord;   { From BlackBox.pas }
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


function ServiceGetStatusName(sMachine,sService: string ): string;   { From BlackBox.pas }
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




// ================================================
// Bios Information: Win2000/NT compatible
// ================================================
function BiosDate : string;   { From BlackBox.pas }
var Cmd : string;
    WinReg : TRegistry;
begin
  WinReg := nil;
  Cmd := '????????';

  // Win 2000/NT
  WinReg := TRegistry.Create;
  TRY
    WinReg.RootKey := HKEY_LOCAL_MACHINE;
    if WinReg.OpenKeyReadOnly('\HARDWARE\DESCRIPTION\System') then
       Cmd := WinReg.ReadString('SystemBiosDate');
  FINALLY
     FreeAndNil(WinReg);
  END;

  Result := Cmd;
end;


function BiosID : string;  { From BlackBox.pas }
var Cmd : string;
    Buffer : PChar;
    WinReg : TRegistry;
begin
  WinReg := nil;
  Cmd := '????????';

  // Win 2000/NT
  TRY
    WinReg := TRegistry.Create;
    WinReg.RootKey := HKEY_LOCAL_MACHINE;
    if WinReg.OpenKeyReadOnly('\HARDWARE\DESCRIPTION\System') then
    begin
       GetMem(Buffer,$2000);
       WinReg.ReadBinaryData('SystemBiosVersion',Buffer^,$2000);
       Cmd := WinReg.ReadString('Identifier') + ' ' + Buffer;
       FreeMem(Buffer);
    end;
  FINALLY
     FreeAndNil(WinReg);
  END;

  Result := Cmd;
end;




{-----------------------------------------------------------------------------
   MONITOR
-----------------------------------------------------------------------------}
{ Simulates mouse movement, so that the screen saver does not start.
  This is the only way to prevent the screen saver to start from Vista onwards if password protection is enabled.
  According to http://stackoverflow.com/a/1675793/49925 }
procedure JiggleMouse;
var
  Inpt: TInput;
begin
  Inpt.Itype := INPUT_MOUSE;
  Inpt.mi.dx := 0;
  Inpt.mi.dy := 0;
  Inpt.mi.mouseData := 0;
  Inpt.mi.dwFlags := MOUSEEVENTF_MOVE;
  Inpt.mi.Time := 0;
  Inpt.mi.dwExtraInfo := 0;
  SendInput(1, Inpt, SizeOf(Inpt));
end;


procedure CursorBusy;
begin
 Screen.Cursor:= crHourGlass;
end;


procedure CursorNotBusy;
begin
 Screen.Cursor:= crDefault;
end;


end.


