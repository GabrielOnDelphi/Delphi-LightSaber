UNIT LightCore.SystemTime;

{=============================================================================================================
   2026.10.13
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   System Time Utilities

   Provides functions for:
   - Windows uptime and user idle time detection
   - System file time retrieval (anti-tampering for licensing)
   - Clock rollback detection, by comparing the clock with the time of a system file

   All four routines have a Windows body only, so far. Each returns a value, so each is declared only on Windows: a call written in an Android, macOS or iOS build fails to compile instead of getting a silent wrong answer. Off Windows this unit is empty.

   Thread safety: These functions are NOT thread-safe.

   See also:
     LightCore.Win.SystemTime.pas     - registry-based time validation (detects clock rollback)
     LightVcl.Common.SystemTime.pas   - DelayEx
=============================================================================================================}

INTERFACE

{$IFDEF MSWINDOWS}
 function  GetSysFileTime: TDateTime;                                                              { gets current date from system file - prevents cracking}
 function  SystemTimeIsInvalid: Boolean;                                                           { returns true if the system time is bigger than current clock time }

 function  WindowsUpTime: TDateTime;
 function  UserIdleTime: Cardinal;
 { Also see Delphi function: FileAge }
{$ENDIF}


IMPLEMENTATION

{$IFDEF MSWINDOWS}
USES
   Winapi.Windows, System.SysUtils, LightCore.AppData;



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

   Note: To track user idle time, one could hook keyboard/mouse activity system-wide, but installing a system-wide message hook is invasive and requires a DLL to be loaded into all processes.
   This API-based approach is much cleaner.

   Returns 0 if GetLastInputInfo fails.

   Note: GetLastInputInfo.dwTime uses GetTickCount (32-bit), which wraps after ~49.7 days.
   If the system has been running longer, and the last input was before the wrap, the result may be incorrect.
   This is a Windows API limitation.
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

   Note: This function looks for files that are modified during normal Windows operation (pagefile.sys, registry hives, etc.) to get a timestamp that cannot easily be manipulated.

   WARNING: Returns 0 on failure, which is a valid TDateTime (Dec 30, 1899).
   Callers should handle the 0 case explicitly.
--------------------------------------------------------------------------------------------------}
function GetSysFileTime: TDateTime;
VAR
  strWinDir, strF: string;
  Buffer: array[0..MAX_PATH - 1] of Char;
begin
 Result:= 0;
 if GetWindowsDirectory(Buffer, MAX_PATH) = 0 then
  begin
   AppDataCore.LogWarn('GetSysFileTime: GetWindowsDirectory failed. '+ SysErrorMessage(GetLastError));
   EXIT;
  end;
 strWinDir:= IncludeTrailingPathDelimiter(Buffer);

 { The SOFTWARE registry hive shows when Windows was last shut down. It exists on every NT Windows, but a process that is not elevated gets "access denied" (error 5), so FileExists returns FALSE for it and the search goes on to pagefile.sys }
 strF:= strWinDir+ 'system32\config\software';
 if NOT FileExists(strF) then strF:= strWinDir+ 'config\software';
 if NOT FileExists(strF) then strF:= 'c:\pagefile.sys';
 if NOT FileExists(strF) then strF:= 'd:\pagefile.sys';
 if NOT FileExists(strF) then strF:= 'e:\pagefile.sys';
 if NOT FileExists(strF) then strF:= 'f:\pagefile.sys';

 if FileExists(strF) then
   if NOT System.SysUtils.FileAge(strF, Result, FALSE)                                             { The RTL falls back to FindFirstFile on a sharing violation (System.SysUtils.FileAgeInternal), so it reads a locked file like pagefile.sys }
   then Result:= 0;
end;



{--------------------------------------------------------------------------------------------------
   Returns TRUE if the system clock appears to have been set backwards.

   Works by comparing the current time (Now) with the modification time of a system file that is updated during normal Windows operation.
   If Now < SystemFileTime, it suggests the user has rolled back the system clock.

   Note: Logs a warning if no system file could be found (should be rare).
   Also see Delphi's FileAge function.
--------------------------------------------------------------------------------------------------}
function SystemTimeIsInvalid: Boolean;
VAR SysFileTime: TDateTime;
begin
 SysFileTime:= GetSysFileTime;
 if SysFileTime = 0 then
  begin
   AppDataCore.LogWarn('SystemTimeIsInvalid: cannot read the time of any system file.');
   SysFileTime:= Now - 0.1;                                                                        { Fallback: assume valid (slightly in the past) }
  end;
 Result:= (Now < SysFileTime);
end;
{$ENDIF}



end.
