UNIT LightVcl.Common.WinVersion;

{=============================================================================================================
   2026.09.10
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Holds only IsNTKernel. It reads Win32Platform and VER_PLATFORM_WIN32_NT, which exist only on Windows.
   The other Windows-version routines (IsWindowsXX, GetOSName, GetOSDetails, GenerateReport) are in LightCore.WinVersion.pas.
=============================================================================================================}

INTERFACE

USES
   WinApi.Windows, System.SysUtils;


function IsNTKernel : Boolean;


IMPLEMENTATION    

function IsNTKernel: Boolean;                                                                                           { Win32Platform is defined as system var }
begin
  Result:= (Win32Platform = VER_PLATFORM_WIN32_NT);
end;


end.
