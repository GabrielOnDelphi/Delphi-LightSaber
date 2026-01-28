UNIT LightCore.Reports;

{=============================================================================================================
   2025.12
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Generates diagnostic reports for application and platform information.
   Framework agnostic.

   The report formatting is optimized for Lucida Console monospaced font!

   Tester:
      c:\Projects\LightSaber\Demo\VCL\Demo SystemReport\VCL_Demo_SystemReport.dpr
      c:\Projects\LightSaber\Demo\FMX\Demo SystemReport\FMX_Demo_SystemReport.dpr
=============================================================================================================}

INTERFACE

USES
   System.SysUtils;


function  GenerateCoreReport: string;    // Global report
function  GenerateAppRep: string;



IMPLEMENTATION
USES
   LightCore, LightCore.AppData, LightCore.Platform, LightCore.Debugger;




{--------------------------------------------------------------------------------------------------
   Summary of all reports in this unit
--------------------------------------------------------------------------------------------------}

{ Generates a comprehensive report including app paths, platform info, and compiler details. }
function GenerateCoreReport: string;
begin
  Result:= GenerateAppRep + CRLF + CRLF;
  Result:= Result + GeneratePlatformRep + CRLF + CRLF;
  Result:= Result + GenerateCompilerReport;

  { Note: GenerateDeviceRep is FMX-only and must be called separately by FMX applications }
end;


{ Generates a report of application paths and settings.
  Note: Requires AppDataCore to be created for LastUsedFolder. }
function GenerateAppRep: string;
begin
  Result:= ' [APPDATA PATHS]' + CRLF;
  Result:= Result + '  AppName: '          + Tab + Tab + TAppDataCore.AppName + CRLF;
  Result:= Result + '  AppFolder: '        + Tab + Tab + TAppDataCore.AppFolder + CRLF;
  Result:= Result + '  AppSysDir: '        + Tab + Tab + TAppDataCore.AppSysDir + CRLF;
  Result:= Result + '  AppDataFolder: '    + Tab + TAppDataCore.AppDataFolder + CRLF;
  Result:= Result + '  AppDataFolderAll: ' + Tab + TAppDataCore.AppDataFolderAllUsers + CRLF;
  Result:= Result + '  IniFile: '          + Tab + Tab + TAppDataCore.IniFile + CRLF;
  Result:= Result + '  ExeShortName: '     + Tab + TAppDataCore.ExeShortName + CRLF;

  if AppDataCore <> NIL
  then Result:= Result + '  LastUsedFolder: ' + Tab + AppDataCore.LastUsedFolder
  else Result:= Result + '  LastUsedFolder: ' + Tab + '(AppDataCore not initialized)';
end;


end.

