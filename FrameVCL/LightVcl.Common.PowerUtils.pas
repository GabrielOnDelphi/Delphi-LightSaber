UNIT LightVcl.Common.PowerUtils;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com

==============================================================================================================

   Utility functions related to system power management.
   Includes: Sleep, Hibernate, Shutdown, Monitor control, Battery status.
   Tested on: Windows 7, 10, 11

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
 function  SystemHibernate(ForceCritical: Boolean= FALSE): Boolean;
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
 {$IFDEF MSWINDOWS}
 TYPE
   TPowerType = (pwTypeBat, pwTypeAC, pwUnknown);
 function  PowerStatus: TPowerType;
 function  PowerStatusString: string;
 function  BatteryLeft : Integer;
 {$ENDIF}
 function  BatteryAsText: string;

{==================================================================================================
   SLEEP MONITOR / SCREENSAVER
==================================================================================================}
 procedure MonitorsOff;
 procedure MonitorsSleep;

 function  TurnScreenSaverOn: Boolean;
 function  IsScreenSaverOn: Boolean;



IMPLEMENTATION
USES
  LightVcl.Common.SystemPermissions, LightVcl.Common.SystemTime;



{-------------------------------------------------------------------------------------------------------------
  HIBERNATE / SLEEP / POWER OFF
-------------------------------------------------------------------------------------------------------------}
function SystemSleep(ForceCritical: Boolean= FALSE): Boolean;
begin
 Result:= SetSuspendState(FALSE, ForceCritical, FALSE);
end;


function SystemHibernate(ForceCritical: Boolean= FALSE): Boolean;
begin
 Result:= SetSuspendState(TRUE, ForceCritical, FALSE);
end;


{ Shows the Windows shutdown dialog (same as pressing Alt+F4 on desktop).
  Uses Shell.Application COM object for the standard Windows shutdown UI. }
procedure WinShutDown;
VAR
  Shell: Variant;
begin
  Shell:= System.Win.ComObj.CreateOleObject('Shell.Application');
  Shell.ShutdownWindows;
end;


{ Programmatically shuts down or reboots Windows.
  Force: If TRUE, forcefully terminates applications without asking to save.
  Reboot: If TRUE, reboots instead of powering off.
  Returns TRUE on success.
  Source: dummzeuch }
function WinShutDown(Force, Reboot: Boolean): Boolean;
VAR
  TokenHandle: THandle;
  pToken: TTokenPrivileges;
  RetLength, Flag: DWORD;
begin
  // All modern Windows versions (NT-based) require privilege adjustment
  Flag:= EWX_POWEROFF;

  if OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES, TokenHandle)
  then
    TRY
      LookupPrivilegeValue(NIL, 'SeShutdownPrivilege', pToken.Privileges[0].Luid);
      pToken.PrivilegeCount:= 1;
      pToken.Privileges[0].Attributes:= SE_PRIVILEGE_ENABLED;
      RetLength:= 0;
      AdjustTokenPrivileges(TokenHandle, FALSE, pToken, 0, PTokenPrivileges(NIL)^, RetLength);
    FINALLY
      CloseHandle(TokenHandle);
    END;

  if Force
  then Flag:= Flag or EWX_FORCE;

  if Reboot
  then Flag:= Flag or EWX_REBOOT;

  Result:= ExitWindowsEx(Flag, 0);
end;



{ Flags: one of the following must be specified: EWX_LOGOFF, EWX_REBOOT, EWX_SHUTDOWN
  Following attributes may be combined with above flags EWX_POWEROFF, EWX_FORCE  : terminate processes }
function WinExit(Flags: integer): boolean;
begin
 Result:= SetPrivilege('SeShutdownPrivilege', TRUE);
 if Result then
  TRY
    Result:= ExitWindowsEx(Flags, 0);
    {$IFDEF Debug}
    if NOT Result
    then RaiseLastOSError; // handle errors...
    {$ENDIF}
  FINALLY
    SetPrivilege('SeShutdownPrivilege', FALSE);
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




{ WARNING: This function may not work reliably on modern Windows versions.
  Screen saver window class names vary between Windows versions. }
function IsScreenSaverOn: Boolean;
begin
 Result:= (FindWindow('WindowsScreenSaverClass', NIL) <> 0)
       OR (FindWindow('Default Screen Saver', NIL) <> 0);
end;



function TurnScreenSaverOn: Boolean;
VAR
  ScreenSaverActive: BOOL;
begin
 Result:= FALSE;

 { Check if screen saver is enabled in Windows settings }
 if NOT SystemParametersInfo(SPI_GETSCREENSAVEACTIVE, 0, @ScreenSaverActive, 0) then EXIT;
 if NOT ScreenSaverActive then EXIT;

 PostMessage(GetDesktopWindow, WM_SYSCOMMAND, SC_SCREENSAVE, 0);
 Result:= TRUE;
end;














{-------------------------------------------------------------------------------------------------------------
   POWER STATUS
-------------------------------------------------------------------------------------------------------------}
{$IFDEF MSWINDOWS}
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


{ Returns battery charge percentage (0-100), or -1 if status unknown.
  Note: BatteryLifePercent can be 255 if status is unknown. }
function BatteryLeft: Integer;
VAR
  SysPowerStatus: TSystemPowerStatus;
begin
  Win32Check(GetSystemPowerStatus(SysPowerStatus));

  { ACLineStatus: 0=Battery, 1=AC, 255=Unknown
    BatteryLifePercent: 0-100 or 255 if unknown }
  if (SysPowerStatus.ACLineStatus = 255) OR (SysPowerStatus.BatteryLifePercent = 255)
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



{ Returns a human-readable string describing battery status flags.
  BatteryFlag bits: 1=High, 2=Low, 4=Critical, 8=Charging, 128=No battery, 255=Unknown }
function BatteryAsText: string;
VAR
  SysPowerStatus: TSystemPowerStatus;
  Flag: Byte;
begin
  if NOT GetSystemPowerStatus(SysPowerStatus)
  then EXIT('Could not get the SYSTEM POWER STATUS');

  Flag:= SysPowerStatus.BatteryFlag;

  // 255 means unknown status - check this first before bitwise tests
  if Flag = 255
  then EXIT('Unknown status');

  // 128 means no system battery
  if (Flag and 128) = 128
  then EXIT('No system battery');

  Result:= '';
  if (Flag and 1) = 1 then Result:= Result + 'High ';
  if (Flag and 2) = 2 then Result:= Result + 'Low ';
  if (Flag and 4) = 4 then Result:= Result + 'Critical ';
  if (Flag and 8) = 8 then Result:= Result + 'Charging ';

  Result:= Trim(Result);
  if Result = ''
  then Result:= 'Normal';
end;





{-------------------------------------------------------------------------------------------------------------
   REMOTE SHUTDOWN
   Initiates a system shutdown with optional warning message.
   Can target remote computers if appropriate permissions are granted.
-------------------------------------------------------------------------------------------------------------}

{ Initiates system shutdown with optional warning message and timeout.
  ComputerName: Target machine name (empty string = local computer).
  Reboot: If TRUE, reboots after shutdown.
  Force: If TRUE, forcefully closes applications.
  Msg: Warning message displayed to users.
  TimeOut: Seconds to display the warning before shutdown (0 = immediate). }
function InitSystemShutdown(const ComputerName: WideString; Reboot, Force: Boolean; const Msg: string; TimeOut: Cardinal=0): Boolean;
begin
  Result:= FALSE;
  if SetPrivilege('SeShutdownPrivilege', TRUE)
  then
    TRY
      Result:= InitiateSystemShutdown(
        PChar(ComputerName),  // Machine Name (empty = local)
        PChar(Msg),           // Warning message to show
        TimeOut,              // Timeout in seconds
        Force,                // Force apps to close?
        Reboot);              // Reboot after shutdown?
    FINALLY
      SetPrivilege('SeShutdownPrivilege', FALSE);
    END;

  {$IFDEF Debug}
  if NOT Result
  then RaiseLastOSError;
  {$ENDIF}
end;


end.
