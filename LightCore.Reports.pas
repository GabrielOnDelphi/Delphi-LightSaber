UNIT LightCore.Reports;

{=============================================================================================================
   2025.12
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   Generates reports.
   Framework agnostic.

   The report formating is optimized for Lucinda Console monospaced font!

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

function GenerateCoreReport: string;
begin
 Result:= Result+ GenerateAppRep+ CRLF+ CRLF;
 Result:= Result+ GeneratePlatformRep+ CRLF+ CRLF;
 Result:= Result+ GenerateCompilerReport;
  {$IFDEF FRAMEWORK_FMX}
 Result:= Result+ CRLF;
 Result:= Result+ GenerateDeviceRep;  {$ENDIF}
end;


function GenerateAppRep: string;
begin
  TAppDataCore.AppDataFolder(True);
  Result:= ' [APPDATA PATHS]'+ CRLF;
  Result:= Result+'  AppName: '          + Tab+Tab+ TAppDataCore.AppName+ CRLF;
  Result:= Result+'  AppFolder: '        + Tab+Tab+ TAppDataCore.AppFolder+ CRLF;
  Result:= Result+'  AppSysDir: '        + Tab+Tab+ TAppDataCore.AppSysDir+ CRLF;
  Result:= Result+'  AppDataFolder: '    + Tab+     TAppDataCore.AppDataFolder+ CRLF;
  Result:= Result+'  AppDataFolderAll: ' + Tab+     TAppDataCore.AppDataFolderAllUsers+ CRLF;
  Result:= Result+'  IniFile: '          + Tab+Tab+ TAppDataCore.IniFile+ CRLF;
  Result:= Result+'  ExeShortName: '     + Tab+     TAppDataCore.ExeShortName+ CRLF;
  Result:= Result+'  LastUsedFolder: '   + Tab+      AppDataCore.LastUsedFolder;
end;


end.

