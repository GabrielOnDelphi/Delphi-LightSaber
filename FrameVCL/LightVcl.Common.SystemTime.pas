UNIT LightVcl.Common.SystemTime;

{=============================================================================================================
   2026.10.13
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   System Time Utilities

   Provides functions for:
   - DelayEx, a delay that keeps a VCL application responsive (it calls Application.ProcessMessages)

   Dependencies: Vcl.Forms

   Thread safety: These functions are NOT thread-safe.

   See also:
     LightCore.SystemTime.pas         - Windows uptime, user idle time, system file time (anti-tampering for licensing)
     LightCore.Win.SystemTime.pas     - registry-based time validation (detects clock rollback)
=============================================================================================================}

INTERFACE
USES
   Winapi.Windows,
   System.SysUtils,
   Vcl.Forms;


 procedure DelayEx(CONST ms : cardinal);


IMPLEMENTATION



{--------------------------------------------------------------------------------------------------
   Non-blocking sleep/delay that keeps the UI responsive.

   DEPRECATION WARNING: This function uses Application.ProcessMessages which is generally discouraged!
   It can cause reentrancy issues if event handlers trigger during the delay (e.g., button click during delay calls button click again).

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



end.
