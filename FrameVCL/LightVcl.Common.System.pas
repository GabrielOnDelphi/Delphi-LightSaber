UNIT LightVcl.Common.System;

{=============================================================================================================
   2026.09.30
   www.GabrielMoraru.com

==============================================================================================================
   Busy mouse cursor of a VCL application: CursorBusy and CursorNotBusy set Screen.Cursor.

   See also:
     - LightCore.System.pas for computer and user names, fonts, BIOS, display modes, print screen and mouse jiggle
     - LightCore.Win.System.pas for Windows services, the text of a Win32 error code and GetUserNameEx
     - LightCore.Win.EnvironmentVar.pas and LightCore.EnvironmentVar.pas for environment variables
     - chHardID.pas for hardware identification

   Related units in this group:
     - LightVcl.Common.Shell.pas
     - LightVcl.Common.System.pas
     - LightVcl.Common.Window.pas
     - LightVcl.Common.WindowMetrics.pas
     - LightVcl.Common.ExecuteProc.pas
     - LightVcl.Common.ExecuteShell.pas
     - LightCore.Process.pas
     - LightVcl.Common.SystemTime
=============================================================================================================}

INTERFACE
USES
   Vcl.Controls, Vcl.Forms;


{==================================================================================================
   MOUSE
==================================================================================================}
 procedure CursorBusy;
 procedure CursorNotBusy;



IMPLEMENTATION




procedure CursorBusy;
begin
 Screen.Cursor:= crHourGlass;
end;


procedure CursorNotBusy;
begin
 Screen.Cursor:= crDefault;
end;


end.


