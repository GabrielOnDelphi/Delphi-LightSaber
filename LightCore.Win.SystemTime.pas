UNIT LightCore.Win.SystemTime;

{$IFNDEF MSWINDOWS}
  {$MESSAGE FATAL 'LightCore.Win.SystemTime is Windows-only. Its package LightCore.Win builds for Win32 and Win64 only.'}
{$ENDIF}

{=============================================================================================================
   2026.10.13
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Registry-based time validation (detects clock rollback)

   CurrentSysTimeStore writes the current clock time to a registry key, and CurrentSysTimeValid reads it back on a later run and tells whether the clock was set backwards meanwhile.

   Windows-only unit, package LightCore.Win. Both routines take a registry path under HKEY_CURRENT_USER and store their data in the Windows registry, which exists on no other platform. A build for Android, macOS or iOS therefore stops at the top of this unit with a fatal compiler message.

   Thread safety: These functions are NOT thread-safe.

   See also:
     LightCore.SystemTime.pas         - Windows uptime, user idle time, system file time
=============================================================================================================}

INTERFACE


 procedure CurrentSysTimeStore(CONST SecretKey: string);                                           { Stores current system clock time to a hidden registry key }
 function  CurrentSysTimeValid(CONST SecretKey: string): Boolean;                                  { Read the last saved system time and compares it with current clock. If current value is smaller than the stored value it means that the clock time was set back }


IMPLEMENTATION

USES
   Winapi.Windows, System.SysUtils, LightCore.SystemTime, LightCore.Win.Registry;



{--------------------------------------------------------------------------------------------------
   TIME-PROTECTION

   These functions provide clock rollback detection for licensing/trial systems.
   Works by storing the current time in the registry and checking if the clock has been set backwards on subsequent runs.
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

   Note: RegReadDate returns -1 if the key doesn't exist.
   This is treated as valid (first run scenario).
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
