UNIT LightVcl.Common.SystemTime;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   System Time Utilities

   Provides functions for:
   - Windows uptime and user idle time detection
   - System file time retrieval (anti-tampering for licensing)
   - Registry-based time validation (detects clock rollback)

   Dependencies: LightVcl.Common.IO, LightVcl.Common.WinVersion, LightVcl.Common.Registry

   Thread safety: These functions are NOT thread-safe.
=============================================================================================================}

INTERFACE
USES
   Winapi.Windows,
   System.SysUtils, System.UITypes,
   Vcl.Forms, Vcl.Dialogs;


 procedure DelayEx(CONST ms : cardinal);

 function  GetSysFileTime: TDateTime;                                                              { gets current date from system file - prevents cracking}
 function  SystemTimeIsInvalid: Boolean;                                                           { returns true if the system time is bigger than current clock time }

 procedure CurrentSysTimeStore(CONST SecretKey: string);                                           { Stores current system clock time to a hidden registry key }
 function  CurrentSysTimeValid(CONST SecretKey: string): Boolean;                                  { Read the last saved system time and compares it with current clock. If current value is smaller than the stored value it means that the clock time was set back }

 function  WindowsUpTime: TDateTime;
 function  UserIdleTime: Cardinal;
 { Also see Delphi function: FileAge }


IMPLEMENTATION

USES
   LightVcl.Common.IO, LightVcl.Common.WinVersion, LightVcl.Common.Registry;



{--------------------------------------------------------------------------------------------------
   SYSTEM  TIME
--------------------------------------------------------------------------------------------------}

{--------------------------------------------------------------------------------------------------
   Returns the time elapsed since Windows was started.
   Use with ShowTimeNice() to display as formatted string.

   Uses GetTickCount64 which doesn't overflow (GetTickCount wraps after ~49.7 days).
   GetTickCount64 requires Windows Vista or later.

   Note: GetTickCount accuracy is ~15ms. See: https://blogs.msdn.microsoft.com/oldnewthing/20050902-00/?p=34333
   Also see: System.Diagnostics.TStopwatch for high-precision timing.
--------------------------------------------------------------------------------------------------}
function WindowsUpTime: TDateTime;
begin
 Result:= GetTickCount64 / MSecsPerSec / SecsPerDay;                                               { Convert ms -> seconds -> days }
end;


{--------------------------------------------------------------------------------------------------
   Returns how long (in seconds) since the user last interacted with the system.
   Interaction includes keyboard presses and mouse movements.

   Available on Windows 2000 and later.

   Note: To track user idle time, one could hook keyboard/mouse activity system-wide,
   but installing a system-wide message hook is invasive and requires a DLL to be
   loaded into all processes. This API-based approach is much cleaner.

   Returns 0 if GetLastInputInfo fails.

   Note: GetLastInputInfo.dwTime uses GetTickCount (32-bit), which wraps after ~49.7 days.
   If the system has been running longer, and the last input was before the wrap,
   the result may be incorrect. This is a Windows API limitation.
--------------------------------------------------------------------------------------------------}
function UserIdleTime: Cardinal;
VAR
   liInfo: TLastInputInfo;
begin
   liInfo.cbSize:= SizeOf(TLastInputInfo);
   if GetLastInputInfo(liInfo)
   then Result:= (GetTickCount - liInfo.dwTime) DIV 1000                                           { GetLastInputInfo.dwTime uses 32-bit tick count }
   else Result:= 0;
end;



{--------------------------------------------------------------------------------------------------
   Gets the last modification time from a system file that changes when Windows runs.
   Used to detect if the user has rolled back the system clock (anti-tampering for licensing).

   Returns: TDateTime of the system file's last modification time, or 0 if no file was found.

   Note: This function looks for files that are modified during normal Windows operation
   (pagefile.sys, registry hives, etc.) to get a timestamp that cannot easily be manipulated.

   WARNING: Returns 0 on failure, which is a valid TDateTime (Dec 30, 1899).
   Callers should handle the 0 case explicitly.

   The Windows 9x code paths are legacy and will never execute on modern systems.
--------------------------------------------------------------------------------------------------}
function GetSysFileTime: TDateTime;
VAR
  strWinDir, strF: string;
begin
 Result:= 0;
 strWinDir:= GetWinDir;
 Assert(DirectoryExistMsg(strWinDir));

 if LightVcl.Common.WinVersion.IsNTKernel
 then
    begin
     { This file exists on XP and shows when Windows was last shutdown. Does NOT exist on Win7+! }
     strF:= strWinDir+ 'system32\config\software';
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
 then Result:= LightVcl.Common.IO.FileAge(strF);                                                   { Use IO version - works on system files that can't be opened directly }
end;



{--------------------------------------------------------------------------------------------------
   Returns TRUE if the system clock appears to have been set backwards.

   Works by comparing the current time (Now) with the modification time of a system file
   that is updated during normal Windows operation. If Now < SystemFileTime, it suggests
   the user has rolled back the system clock.

   Note: Shows a dialog if no system file could be found (should be rare).
   Also see Delphi's FileAge function.
--------------------------------------------------------------------------------------------------}
function SystemTimeIsInvalid: Boolean;
VAR SysFileTime: TDateTime;
begin
 SysFileTime:= GetSysFileTime;
 if SysFileTime = 0 then
  begin
   MessageDlg('Can''t get system time!', mtInformation, [mbOk], 0);
   SysFileTime:= Now - 0.1;                                                                        { Fallback: assume valid (slightly in the past) }
  end;
 Result:= (Now < SysFileTime);
end;



{--------------------------------------------------------------------------------------------------
   Non-blocking sleep/delay that keeps the UI responsive.

   DEPRECATION WARNING: This function uses Application.ProcessMessages which is
   generally discouraged! It can cause reentrancy issues if event handlers
   trigger during the delay (e.g., button click during delay calls button click again).

   Consider using TThread or TTask for better alternatives:
   - TThread.CreateAnonymousThread for simple background work
   - TTask.Run for parallel operations
   - PostMessage with a timer for delayed actions

   See: https://blog.dummzeuch.de/2018/09/29/calling-application-processmessages-in-a-delphi-program/

   Note: GetTickCount accuracy is ~15ms. See: https://blogs.msdn.microsoft.com/oldnewthing/20050902-00/?p=34333
--------------------------------------------------------------------------------------------------}
procedure DelayEx(CONST ms: Cardinal);
VAR
  StartTime: UInt64;
begin
 StartTime:= GetTickCount64;
 REPEAT
  Sleep(1);                                                                                        { Prevents 100% CPU utilization }
  Application.ProcessMessages;                                                                     { DISCOURAGED - see function comment }
 UNTIL (GetTickCount64 - StartTime) >= ms;
end;






{--------------------------------------------------------------------------------------------------
   TIME-PROTECTION

   These functions provide clock rollback detection for licensing/trial systems.
   Works by storing the current time in the registry and checking if the clock
   has been set backwards on subsequent runs.
--------------------------------------------------------------------------------------------------}

{--------------------------------------------------------------------------------------------------
   Stores the current system time to a hidden registry key.
   Call this periodically (e.g., on application startup or shutdown) to update the stored time.

   Parameters:
     SecretKey - Registry path under HKEY_CURRENT_USER where the time will be stored.
                 Example: 'Software\MyApp\Security'

   Raises Exception if SecretKey is empty.
--------------------------------------------------------------------------------------------------}
procedure CurrentSysTimeStore(CONST SecretKey: string);
begin
 if SecretKey = ''
 then raise Exception.Create('CurrentSysTimeStore: SecretKey parameter cannot be empty');

 RegWriteDate(HKEY_CURRENT_USER, SecretKey, 'System', Now, TRUE);
end;


{--------------------------------------------------------------------------------------------------
   Checks if the system clock is valid (hasn't been rolled back).

   Returns TRUE if:
     - The registry key doesn't exist yet (first run)
     - The current time is >= the last stored time
     - The system file time is not in the future

   Returns FALSE if the clock appears to have been set backwards.

   Parameters:
     SecretKey - Registry path to check (same as used in CurrentSysTimeStore).

   Note: RegReadDate returns -1 if the key doesn't exist. This is treated as valid
   (first run scenario).
--------------------------------------------------------------------------------------------------}
function CurrentSysTimeValid(CONST SecretKey: string): Boolean;
VAR
  LastTime: TDateTime;
begin
 if SecretKey = ''
 then raise Exception.Create('CurrentSysTimeValid: SecretKey parameter cannot be empty');

 LastTime:= RegReadDate(HKEY_CURRENT_USER, SecretKey, 'System');

 { If registry key doesn't exist (first run), RegReadDate returns -1 }
 if LastTime < 0
 then EXIT(TRUE);

 Result:= (LastTime <= Now) AND NOT SystemTimeIsInvalid;
end;



end.