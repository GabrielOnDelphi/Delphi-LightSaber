UNIT cmPowerUtils;

{=============================================================================================================
   2023.01
   See Copyright.txt
==============================================================================================================

   Utility functions related to system power.
   Works on Win 7

   Parameters for SetSuspendState:
     Hibernate:
       TRUE : the system hibernates.
       FALSE: the system is suspended.

     ForceCritical:
        TRUE : the system suspends operation immediately
        FALSE: the system broadcasts a PBT_APMQUERYSUSPEND event to each application to request permission to suspend operation.

     DisableWakeEvent:
        TRUE : the system disables all wake events.
        FALSE: any system wake events remain enabled.

    Related:
      http://www.tek-tips.com/faqs.cfm?fid=6881

=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, Winapi.Messages, System.Win.ComObj, System.SysUtils, Vcl.Forms;


{==================================================================================================
   SLEEP/SHUT DOWN PC
==================================================================================================}
 function  SystemSleep    (ForceCritical: Boolean= FALSE): Boolean;
 function  SystemHybernate(ForceCritical: Boolean= FALSE): Boolean;
 function  SetSuspendState(Hibernate, ForceCritical, DisableWakeEvent: Boolean): Boolean; stdcall; external 'powrprof.dll' name 'SetSuspendState';

 function  WinExit        (Flags: integer): boolean;       // Shut down, restart or logs off Windows
 function  WinShutDown    (Force, Reboot: Boolean): Boolean;  overload;
 procedure WinShutDown;                                       overload;

 function IsHibernateAllowed  : Boolean; stdcall; external 'powrprof.dll' name 'IsPwrHibernateAllowed';    //  Does not work on WinNT 4.0 or Win95.
 function IsPwrSuspendAllowed : Boolean; stdcall; external 'powrprof.dll' name 'IsPwrSuspendAllowed';
 function IsPwrShutdownAllowed: Boolean; stdcall; external 'powrprof.dll' name 'IsPwrShutdownAllowed';

 function InitSystemShutdown(CONST ComputerName: WideString; Reboot, Force: Boolean; const Msg: string; TimeOut: Cardinal=0): Boolean;    // Shut down, restart a machine with showing an optional warning message.

 //todo: use the SetThreadExecutionState WinApi function to keep the screen on. It is the function that video players use to keep the screen on while you are watching a film.
{==================================================================================================
   POWER STATUS / BATTERY
==================================================================================================}
 {$IFDEF msWindows}
 TYPE
   TPowerType = (pwTypeBat, pwTypeAC, pwUnknown);
 function  PowerStatus: TPowerType;
 function  PowerStatusString: string;
 function  BatteryLeft : Integer;
 {$ENDIF}
 function  BatteryAsText: string;

{==================================================================================================
   SLEED MONITOR / SCREENSAVER
==================================================================================================}
 procedure MonitorsOff;
 procedure MonitorsSleep;

 function  TurnScreenSaverOn: Boolean;
 function  IsScreenSaverOn: Boolean;



IMPLEMENTATION
USES
  cmPermissions, csSystem;




{-------------------------------------------------------------------------------------------------------------
  HIBERNATE / SLEEP / POWER OFF
-------------------------------------------------------------------------------------------------------------}
function SystemSleep(ForceCritical: Boolean= FALSE): Boolean;
begin
 Result:= SetSuspendState(FALSE, ForceCritical, FALSE);
end;


function SystemHybernate(ForceCritical: Boolean= FALSE): Boolean;
begin
 Result:= SetSuspendState(TRUE, ForceCritical, FALSE);
end;


procedure WinShutDown;
VAR shell: Variant;
begin
  shell:= System.Win.ComObj.CreateOleObject('Shell.Application');
  shell.ShutdownWindows;
end;


//source: dummzeuch
function WinShutDown(Force, Reboot: Boolean): Boolean;
var
  TokenHandle: THandle;
  pToken: TTokenPrivileges;
  RetLength, Flag: DWORD;
begin
  if (Win32Platform = VER_PLATFORM_WIN32_NT)
  then
   begin
     // For WinNT and up, first we get the rights
     Flag := EWX_POWEROFF;
     OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES, TokenHandle);
     LookupPrivilegeValue(NIL, 'SeShutdownPrivilege', pToken.Privileges[0].Luid);
     pToken.PrivilegeCount := 1;
     pToken.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
     RetLength := 0;
     AdjustTokenPrivileges(TokenHandle, False, pToken, 0, PTokenPrivileges(nil)^, RetLength);
     CloseHandle(TokenHandle);
   end
  else
    Flag := EWX_SHUTDOWN; // For Win98 & Me

  if Force  then Flag := Flag or EWX_FORCE;
  if Reboot then Flag := Flag or EWX_REBOOT;

  Result := ExitWindowsEx(Flag, 0);
end;



{ Flags: one of the following must be specified: EWX_LOGOFF, EWX_REBOOT, EWX_SHUTDOWN
  Following attributes may be combined with above flags EWX_POWEROFF, EWX_FORCE  : terminate processes }
function WinExit(Flags: integer): boolean;
begin
 Result:= cmPermissions.SetPrivilege('SeShutdownPrivilege', TRUE);
 if Result then
  TRY
    Result:= ExitWindowsEx(Flags, 0);
    {$IFDEF Debug}
    if NOT Result
    then RaiseLastOSError; // handle errors...
    {$ENDIF}
  FINALLY
    cmPermissions.SetPrivilege('SeShutdownPrivilege', FALSE);
  END;
end;










procedure MonitorsOff;
{ -1 (the display is powering on)
   1 (the display is going to low power)
   2 (the display is being shut off)   }
begin
 DelayEx(2000);                                                                        { Allow user to get his hands from mouse otherwise it will accidentally move the mouse and wakeup the screen }
 SendMessage(Application.Handle, Winapi.Messages.wm_SysCommand, SC_MonitorPower, 2);   { I have successfully tested this on Windows XP and Windows 7 }
end;


procedure MonitorsSleep;
begin
 DelayEx(2000);
 SendMessage(Application.Handle, Winapi.Messages.wm_SysCommand, SC_MonitorPower, 1);
end;




function IsScreenSaverOn: Boolean;
begin
 {DOESN'T WORK!}
 Result:= (FindWindow('WindowsScreenSaverClass', NIL)<> 0) OR (FindWindow('Default Screen Saver', NIL)<> 0);    { From here: http://bobmoore.mvps.org/Win32/w32tip22.htm}
end;



function TurnScreenSaverOn: Boolean;
VAR b : bool;
begin
 Result:= FALSE;
 if SystemParametersInfo(SPI_GETSCREENSAVEACTIVE, 0, @b, 0) <> TRUE then EXIT;
 if NOT b then EXIT;
 PostMessage(GetDesktopWindow, WM_SYSCOMMAND, SC_SCREENSAVE, 0);
 Result:= TRUE;
end;














{-------------------------------------------------------------------------------------------------------------
   POWER STATUS
-------------------------------------------------------------------------------------------------------------}
{$IFDEF msWindows}
{$WARN SYMBOL_PLATFORM OFF}
function PowerStatus: TPowerType;
VAR SysPowerStatus: TSystemPowerStatus;                                                          { Details: http://msdn.microsoft.com/en-us/library/aa373232(VS.85).aspx }
begin
 Win32Check(GetSystemPowerStatus(SysPowerStatus));                                               { 0= Is on batteries /  1= Is on AC /  >1= Unknown }

 CASE SysPowerStatus.ACLineStatus of
   0: Result:= pwTypeBat;
   1: Result:= pwTypeAC;
   else Result:= pwUnknown;
 end;
end;


function BatteryLeft: Integer; { Returns left power }                                            { Percentage of full battery charge remaining. It should be from 0 to 100. If it is over 100 then it cannot get battery status }
VAR
  SysPowerStatus: TSystemPowerStatus;
begin
  Win32Check(GetSystemPowerStatus(SysPowerStatus));
  if SysPowerStatus.ACLineStatus= 3
  then Result:= -1
  else Result:= SysPowerStatus.BatteryLifePercent;
end;


function PowerStatusString: string; { Same as above but as string }
begin
 CASE PowerStatus of
  pwTypeBat: Result:= 'Running on batteries!';
  pwTypeAC : Result:= 'Running on AC.';
  pwUnknown: Result:= 'Power supply status unavailable.';
 end;
end;
{$WARN SYMBOL_PLATFORM on}
{$ENDIF}



function BatteryAsText: string;                                                                  { http://74.125.77.132/search?q=cache:bgnnaViy9n0J:www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_11011301.html+site:www.experts-exchange.com+delphi+battery+status&cd=1&hl=en&ct=clnk }
VAR SysPowerStatus : TSystemPowerStatus;
begin
 if GetSystemPowerStatus(SysPowerStatus)
 then
  begin
    Result:= '';
    if SysPowerStatus.BatteryFlag and 1 = 1 then Result:= Result+'High ';
    if SysPowerStatus.BatteryFlag and 2 = 2 then Result:= Result+'Low ';
    if SysPowerStatus.BatteryFlag and 4 = 4 then Result:= Result+'Critical ';
    if SysPowerStatus.BatteryFlag and 8 = 8 then Result:= Result+'Charging ';
    if SysPowerStatus.BatteryFlag and 128 = 128 then Result:= Result+'No system battery ';
    if SysPowerStatus.BatteryFlag and 255 = 255 then Result:= Result+'Unknown status ';
   end
 else Result:= 'Could not get the SYSTEM POWER STATUS';
end;





{-------------------------------------------------------------------------------------------------------------
   Enable/Disables a specific privilage in Widnows.
-------------------------------------------------------------------------------------------------------------}
function InitSystemShutdown(const ComputerName: WideString; Reboot, Force: Boolean; const Msg: string; TimeOut: Cardinal=0): Boolean;
begin
  Result := False;
  if cmPermissions.SetPrivilege('SeShutdownPrivilege', True) then
   TRY
     Result := InitiateSystemShutdown(PChar(ComputerName), //Machine Name
                                      PChar(Msg),          //Warning message to show
                                      TimeOut,             //Timeout to show the message
                                      Force,               //Should force apps to close?
                                      Reboot);             //Should reboot system?
   FINALLY
     cmPermissions.SetPrivilege('SeShutdownPrivilege', False);
   end;

  {$IFDEF Debug}
  if NOT Result then RaiseLastOSError;
  {$ENDIF}
end;


end.
