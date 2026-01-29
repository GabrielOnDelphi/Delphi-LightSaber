UNIT LightVCL.Common.Reports;

{=============================================================================================================
   2025.12
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   Generate reports related to
      Win OS
      Hardware

   The report formatting is optimized for Lucida Console monospaced font!

   Tester:
      c:\Projects\LightSaber\Demo\VCL\Demo SystemReport\VCL_Demo_SystemReport.dpr
=============================================================================================================}

INTERFACE

USES
   Winapi.MultiMon, Winapi.ShlObj, Winapi.SHFolder,
   System.Classes, System.SysUtils, Vcl.Forms;


 function  GenerateVCLReport: string;

 function  GenerateWinSysRep:    string;
 function  GenerateWinPathRep:   string;
 function  GenerateWinPathRepEx: string;
 function  GenerateHardwareRep:  string;
 function  GenerateHardwareRepTSL: TStringList;
 function  ScreenResApi:         string;


IMPLEMENTATION
USES
   MonitorHelper,
   LightCore, LightCore.Reports, LightCore.Time, LightCore.Platform,
   LightVcl.Common.SystemTime, LightVcl.Common.System, LightVcl.Common.IO, LightVcl.Common.WinVersion, LightVcl.Common.SystemPermissions;




{--------------------------------------------------------------------------------------------------
   Core report + VCL report
--------------------------------------------------------------------------------------------------}

function GenerateVCLReport: string;
begin
 Result:= Result+ '=< CORE REPORT >='+ CRLF+ CRLF;
 Result:= Result+ LightCore.Reports.GenerateCoreReport+ CRLF;
 Result:= Result+ CRLF;
 
 Result:= Result+ '=< VCL REPORT >='+ CRLF;
 Result:= Result+ CRLF;

 Result:= Result+ GenerateWinSysRep   + CRLF+ CRLF;
 Result:= Result+ GenerateHardwareRep + CRLF+ CRLF;     { Before calling this we need to enter a valid key into chHardID: chHardID.HDIDValid:= TRUE;  }
 Result:= Result+ GenerateWinPathRep  + CRLF+ CRLF;
 Result:= Result+ GenerateWinPathRepEx+ CRLF+ CRLF;
 Result:= Result+ MonitorHelper.GenerateScreenReport;  // 3rdParty MonitorHelper.pas
end;




{--------------------------------------------------------------------------------------------------
   INDIVIDUAL REPORTS
--------------------------------------------------------------------------------------------------}

{Note: A super detailed report can be obtained via Hardware Extractor ID which can be obtained from https://www.soft.tahionic.com/download-hdd_id/index.html }
function GenerateHardwareRep: string;
begin
 Result:= ' [HARDWARE]'+ CRLF;
 Result:= Result+'  User name: '      + Tab+ Tab+ GetUserName+ CRLF;
 Result:= Result+'  UserName Ex: '    + Tab+ Tab+ GetUserNameEx(2)+ CRLF;
 Result:= Result+'  Computer name: '  + Tab+ GetComputerName+ CRLF;                 { Also see GetComputerNameEx:   http://stackoverflow.com/questions/30778736/how-to-get-the-full-computer-name-in-inno-setup/30779280#30779280 }
 Result:= Result+'  Domain name: '    + Tab+ Tab+ GetDomainName+ CRLF;
 Result:= Result+'  Host name: '      + Tab+ Tab+ GetHostName+ CRLF;
 Result:= Result+'  Total monitors: ' + Tab+ IntToStr(Screen.MonitorCount)+ CRLF;
 Result:= Result+'  Screen res:     ' + Tab+ IntToStr(Screen.Width)+ 'x'+ IntToStr(Screen.Height)+ CRLF;
 Result:= Result+'  '+ ScreenResApi+ CRLF;
 Result:= Result+'  Free space: '     + Tab+ Tab+ DriveFreeSpaceS ('C');
 //Result:= Result+'  Local IP: '     + Tab+ Tab+LightVcl.Internet.GetLocalIP+ CRLF;
end;


{ THIS WILL RETURN THE VIRTUALIZED RESOLUTION (when high DPI is set).
  It is useless if we want to get the real resolution.
  http://stackoverflow.com/questions/7077572/get-current-native-screen-resolution-of-all-monitors-in-delphi-directx }
function ScreenResApi: string;
VAR MonInfo: TMonitorInfo;
begin
 if Application.MainForm = NIL
 then raise Exception.Create('ScreenResApi: MainForm is nil. This happens when code runs during OnFormCreate.');

 MonInfo.cbSize:= SizeOf(MonInfo);
 GetMonitorInfo(MonitorFromWindow(Application.MainForm.Handle, MONITOR_DEFAULTTONEAREST), @MonInfo);
 Result:= Format('Monitor res (API): %dx%d', [MonInfo.rcMonitor.Right - MonInfo.rcMonitor.Left, MonInfo.rcMonitor.Bottom - MonInfo.rcMonitor.Top]);
end;


{ Returns hardware report as TStringList.
  Note: Caller must free the returned TStringList! }
function GenerateHardwareRepTSL: TStringList;
begin
 Result:= TStringList.Create;
 Result.Text:= GenerateHardwareRep;
end;


function GenerateWinSysRep: string;
begin
 Result:= ' [SYSTEM/OS]'+ CRLF;
 Result:= Result+'  OS platform: '          + Tab+ Tab+ LightVcl.Common.WinVersion.GetOSName+ CRLF;
 Result:= Result+'  App has admin rights: ' + BoolToStr(AppHasAdminRights, TRUE)+ CRLF;
 Result:= Result+'  Invalid system time: '  + Tab+ BoolToStr(SystemTimeIsInvalid , TRUE)+ CRLF;
 Result:= Result+'  Windows up time: '      + Tab+ ShowTimeNice(WindowsUpTime);
end;


{ Useful paths }
function GenerateWinPathRep: string;
begin
 Result:= ' [PATHS]'+ CRLF;
 Result:= Result+'  Windows: '              + Tab+Tab + GetSpecialFolder(CSIDL_WINDOWS)+ CRLF;
 Result:= Result+'  System: '               + Tab+Tab + GetSpecialFolder(CSIDL_SYSTEM)+ CRLF;
 Result:= Result+'  COMMON APPDATA: '       + Tab     + GetSpecialFolder(CSIDL_COMMON_APPDATA)+ CRLF;
 Result:= Result+'  Program Files: '        + Tab     + GetSpecialFolder(CSIDL_PROGRAM_FILES)+ CRLF;          { C:\Program Files }
 Result:= Result+'  Program Files cmn: '    + Tab     + GetSpecialFolder(CSIDL_PROGRAM_FILES_COMMON)+ CRLF;   { C:\Program Files\Common }
 Result:= Result+'  APPDATA: '              + Tab+Tab + GetSpecialFolder(CSIDL_APPDATA)+ CRLF;
 Result:= Result+'  LOCAL APPDATA: '        + Tab+      GetSpecialFolder(CSIDL_LOCAL_APPDATA)+ CRLF;
 Result:= Result+'  COMMON DOCUMENTS: '     + Tab     + GetSpecialFolder(CSIDL_COMMON_DOCUMENTS)+ CRLF;       { All Users\Documents }
 Result:= Result+'  PERSONAL: '             + Tab+Tab + GetSpecialFolder(CSIDL_PERSONAL);
end;


{ Some other less useful paths }
function GenerateWinPathRepEx: string;
begin
 Result:= ' [PATHS 2]'+ CRLF;
 Result:= Result+'  COMMON DESKTOP DIR: ' + Tab     + GetSpecialFolder(CSIDL_COMMON_DESKTOPDIRECTORY)+ CRLF;
 Result:= Result+'  DESKTOP DIR: '        + Tab+Tab + GetSpecialFolder(CSIDL_DESKTOPDIRECTORY)+ CRLF; // // <user name>\Desktop
 Result:= Result+'  PROGRAMS: '           + Tab+Tab + GetSpecialFolder(CSIDL_PROGRAMS)+ CRLF;
 Result:= Result+'  STARTUP: '            + Tab+Tab + GetSpecialFolder(CSIDL_STARTUP)+ CRLF;
 Result:= Result+'  STARTMENU: '          + Tab+Tab + GetSpecialFolder(CSIDL_STARTMENU)+ CRLF;
 Result:= Result+'  COMMON STARTMENU: '   + Tab     + GetSpecialFolder(CSIDL_COMMON_STARTMENU)+ CRLF;
 Result:= Result+'  COMMON STARTUP: '     + Tab     + GetSpecialFolder(CSIDL_COMMON_STARTUP)+ CRLF;
 Result:= Result+'  COMMON PROGRAMS: '    + Tab     + GetSpecialFolder(CSIDL_COMMON_PROGRAMS)+ CRLF;
 Result:= Result+'  FONTS:      '         + Tab+Tab + GetSpecialFolder(CSIDL_FONTS);
end;


end.

