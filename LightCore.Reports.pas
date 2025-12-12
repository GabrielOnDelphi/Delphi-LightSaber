UNIT LightCore.Reports;

{=============================================================================================================
   2025.12
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   Generate reports related to
      App
      Platforms

   The report formating is optimized for Lucinda Console monospaced font!

   Tester:
      c:\Projects\LightSaber\Demo\VCL\Demo SystemReport\VCL_Demo_SystemReport.dpr
=============================================================================================================}

INTERFACE

USES
   System.Classes, System.SysUtils;


function  GenerateReport: string;

function  GenerateAppRep:       string;
function  GeneratePlatformRep:  string;


IMPLEMENTATION
USES
   LightCore, LightCore.AppData, LightCore.Platform;





{--------------------------------------------------------------------------------------------------
   Summary of all reports in this unit
--------------------------------------------------------------------------------------------------}

function GenerateReport: string;
begin
 Result:= '=< APP REPORT >='+ CRLF;
 Result:= Result+ CRLF;
 Result:= Result+ GenerateAppRep      + CRLF+ CRLF;
 Result:= Result+ GeneratePlatformRep + CRLF+ CRLF;
end;




{--------------------------------------------------------------------------------------------------
   INDIVIDUAL REPORTS
--------------------------------------------------------------------------------------------------}

{ The AppDataPath parameter lets the user provide the data path in case the app is not using the Default data path }
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


function GeneratePlatformRep: string;
begin
  Result:= ' [PLATFORM OS]'+ CRLF;
  Result:= Result+'  Platform: '         + Tab+Tab+ OsType+ CRLF;
  Result:= Result+'  OsArchitecture: '   + Tab    + OsArchitecture+ CRLF;

  Result:= Result+ CRLF;
  Result:= Result+' [APP BITNESS]'+ CRLF;
  Result:= Result+'  AppBitness:    '    + Tab    + AppBitness+ CRLF;
  Result:= Result+'  AppBitnessEx: '     + Tab    + AppBitnessEx+ CRLF;
  Result:= Result+'  Is64Bit: '          + Tab+Tab+ BoolToStr(AppIs64Bit, TRUE);
end;


end.

