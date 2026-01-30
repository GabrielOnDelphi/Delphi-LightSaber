UNIT  LightVcl.Common.WinVersionApi;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   Alternative functions for Windows version detection (based on RtlGetVersion API).
   Prefer LightVcl.Common.WinVersion.pas for most use cases since it relies on the more reliable TOSVersion.

   This library provides 3 ways to get Windows version:
      1. GetWinVersion     - Uses RtlGetVersion from NtDLL.dll (most reliable, returns Major.Minor)
      2. GetWinVersionEx   - Uses deprecated GetVersionEx API (returns OS name like "XP", "10", "11")
      3. GetWinVerNetServer - Uses NetServerGetInfo from Netapi32.dll (returns Major.Minor)

   A 4th alternative is GetKernel32Version which uses GetFileVersionInfo on kernel32.dll
   https://github.com/aehimself/AEFramework/blob/master/3rdParty/OSVersion.pas

   Note: GetWinVersionEx differs from the other functions - it returns the Windows OS name
   (e.g., "XP", "Vista", "10", "11") rather than version numbers.

   Also see:
      LightVcl.Common.WinVersion   - Preferred: uses TOSVersion, provides IsWindows* functions
      LightVcl.Common.ExeVersion   - Gets version info from executable files

   Tester:
       c:\Projects\LightSaber\Demo\Demo Detect WinVer\
=============================================================================================================}

INTERFACE

USES
   WinApi.Windows, System.SysUtils;


procedure GetWinVersion (OUT MajVersion, MinVersion: Cardinal);  overload;
function  GetWinVersion: string;                                 overload;

function  GetWinVerNetServer: string;   { Alternative to GetWinVersion }
function  GetWinVersionEx: string;

function  GenerateReport: string; { For testing }



IMPLEMENTATION
USES LightCore;


{-------------------------------------------------------------------------------------------------------------
   Build number thresholds for Windows version detection.
   These are approximate lower bounds for each Windows release.
   Used by GetWinVersionEx to determine OS name from build number.

   Note: These are heuristic values, not precise boundaries.
   Some builds may overlap between versions, but this provides reasonable detection.
-------------------------------------------------------------------------------------------------------------}
CONST
  BuildThreshold_Win95   = 900;    { Windows 95: builds ~950-1080 }
  BuildThreshold_Win98   = 1900;   { Windows 98: builds ~1998-2222 }
  BuildThreshold_Win2000 = 2000;   { Windows 2000: builds 2195+ }
  BuildThreshold_WinXP   = 2500;   { Windows XP: builds 2600+ }
  BuildThreshold_Win2003 = 3000;   { Windows Server 2003: builds 3790+ }
  BuildThreshold_Vista   = 5000;   { Windows Vista: builds 6000+ }
  BuildThreshold_Win2008 = 6000;   { Windows Server 2008: builds 6001+ }
  BuildThreshold_Win7    = 7000;   { Windows 7: builds 7600+ }
  BuildThreshold_Win8    = 8000;   { Windows 8: builds 9200+ }
  BuildThreshold_Win81   = 9000;   { Windows 8.1: builds 9600+ }
  BuildThreshold_Win10   = 9100;   { Windows 10: builds 10240+ (first release: July 2015) }
  BuildThreshold_Win11   = 22000;  { Windows 11: builds 22000+ (first release: October 2021) }




{-------------------------------------------------------------------------------------------------------------
   Read RTL Version directly from NTDLL.dll via RtlGetVersion API.
   This is the most reliable method as it bypasses the manifest-based version reporting.

   Returns:
     - On Win10: returns 10.0
     - On Win11: returns 10.0 (Win11 shares major version with Win10, use build number to differentiate)
     - On failure: returns 0.0 (if RtlGetVersion not found or call fails)

   Note: This function always succeeds on Windows 2000 and later.
-------------------------------------------------------------------------------------------------------------}
procedure GetWinVersion(OUT MajVersion, MinVersion: Cardinal);
TYPE
   pfnRtlGetVersion = function(var RTL_OSVERSIONINFOEXW): LONG; stdcall;
VAR
   Ver: RTL_OSVERSIONINFOEXW;
   RtlGetVersion: pfnRtlGetVersion;
begin
  MajVersion:= 0;
  MinVersion:= 0;

  @RtlGetVersion := GetProcAddress(GetModuleHandle('ntdll.dll'), 'RtlGetVersion');
  if Assigned(RtlGetVersion) then
  begin
    ZeroMemory(@ver, SizeOf(ver));
    ver.dwOSVersionInfoSize := SizeOf(ver);

    if RtlGetVersion(ver) = 0 then
     begin
      MajVersion:= ver.dwMajorVersion;
      MinVersion:= ver.dwMinorVersion;
     end;
  end;
end;


function GetWinVersion: string;
var MajVersion, MinVersion: Cardinal;
begin
 GetWinVersion(MajVersion, MinVersion);
 result:= IntToStr(MajVersion)+ '.'+ IntToStr(MinVersion);
end;


{-------------------------------------------------------------------------------------------------------------
   Gets Windows version name using the (deprecated) GetVersionEx API.
   Unlike GetWinVersion, this returns the OS NAME (e.g., "XP", "Vista", "10", "11")
   rather than version numbers.

   IMPORTANT: GetVersionEx is deprecated starting with Windows 8.1.
   Results may be inaccurate on older Delphi versions without proper manifest.

   Verified: Works on Windows 10/11 when app has proper manifest.

   Returns: OS name string ("95", "98", "XP", "Vista", "7", "8", "8.1", "10", "11")
            or "Unknown OS" on failure.

   Implementation note: The function extracts the build number from the version string
   and uses threshold comparisons to determine the OS name. This is a heuristic approach.
-------------------------------------------------------------------------------------------------------------}
{ TODO: Consider simplifying by using OsVinfo.dwMajorVersion/dwMinorVersion/dwBuildNumber directly
        instead of parsing the formatted Buffer string. This would be cleaner but requires
        careful handling of the platform ID checks. }
function GetWinVersionEx: String;
var
  i: integer;
  s, Temp: string;
  OsVinfo: TOSVERSIONINFO;
  Buffer: array[0..255] of Char;
begin
  Result:='Unknown OS';

  ZeroMemory(@OsVinfo, SizeOf(OsVinfo));
  OsVinfo.dwOSVersionInfoSize := SizeOf(TOSVERSIONINFO);
  if GetVersionEx(OsVinfo)
  then
    begin
      if OsVinfo.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS then
        if (OsVinfo.dwMajorVersion = 4) and (OsVinfo.dwMinorVersion > 0)
        then StrFmt(Buffer, 'Windows 98 - Version %d.%.2d, Build %d, %s', [OsVinfo.dwMajorVersion, OsVinfo.dwMinorVersion, OsVinfo.dwBuildNumber and $FFFF, OSVinfo.szCSDVersion])
        else StrFmt(Buffer, 'Windows 95 - Version %d.%d, Build %d, %s',   [OsVinfo.dwMajorVersion, OsVinfo.dwMinorVersion, OsVinfo.dwBuildNumber and $FFFF, OSVinfo.szCSDVersion]);
      if OsVinfo.dwPlatformId = VER_PLATFORM_WIN32_NT
      then StrFmt(Buffer, 'Microsoft Windows NT Version %d.%.2d, Build %d, %s', [OsVinfo.dwMajorVersion, OsVinfo.dwMinorVersion, OsVinfo.dwBuildNumber and $FFFF, OSVinfo.szCSDVersion]);
    end
  else
    StrCopy(Buffer, 'Error retrieving Win version!');

 { Parse the formatted string to extract build number }
 s:= Buffer;
 i:= Pos('Build', s);
 if i > 0 then
   begin
    Temp:= copy(s, i+6, maxint);   { Example: 'Microsoft Windows NT Version 10.00, Build 22621, ' }
    i:= ExtractIntFromStr(Temp);   { Extract build number from the string }

    { Determine OS name based on build number thresholds (evaluated in ascending order) }
    if i >  BuildThreshold_Win95   then Result:= '95';
    if i >  BuildThreshold_Win98   then Result:= '98';
    if i >  BuildThreshold_Win2000 then Result:= '2000';
    if i >  BuildThreshold_WinXP   then Result:= 'XP';
    if i >  BuildThreshold_Win2003 then Result:= '2003';
    if i >  BuildThreshold_Vista   then Result:= 'Vista';
    if i >  BuildThreshold_Win2008 then Result:= '2008';
    if i >  BuildThreshold_Win7    then Result:= '7';
    if i >  BuildThreshold_Win8    then Result:= '8';
    if i >  BuildThreshold_Win81   then Result:= '8.1';
    if i >  BuildThreshold_Win10   then Result:= '10';
    if i >= BuildThreshold_Win11   then Result:= '11';
   end;
end;




{-------------------------------------------------------------------------------------------------------------
   NetServerGetInfo - Alternative method using Netapi32.dll
   Uses the NetServerGetInfo API to retrieve server information including OS version.

   Returns: Version string in "Major.Minor" format (e.g., "10.0" on Win10/11)
            or "Unknown OS" if the API call fails.

   Note: The MAJOR_VERSION_MASK ($0F) is applied because the high bits of sv101_version_major
   may contain additional flags.

   Tested: On Win10/11 returns "10.0"
-------------------------------------------------------------------------------------------------------------}
TYPE
  NET_API_STATUS = DWORD;

  _SERVER_INFO_101 = record
    sv101_platform_id: DWORD;
    sv101_name: LPWSTR;
    sv101_version_major: DWORD;
    sv101_version_minor: DWORD;
    sv101_type: DWORD;
    sv101_comment: LPWSTR;
  end;

 SERVER_INFO_101   = _SERVER_INFO_101;
 PSERVER_INFO_101  = ^SERVER_INFO_101;
 LPSERVER_INFO_101 = PSERVER_INFO_101;

CONST
  MAJOR_VERSION_MASK = $0F;

function NetServerGetInfo(servername: LPWSTR; level: DWORD; var bufptr): NET_API_STATUS; stdcall; external 'Netapi32.dll';
function NetApiBufferFree(Buffer: LPVOID): NET_API_STATUS; stdcall; external 'Netapi32.dll';


function GetWinVerNetServer: string;
VAR
   Buffer: PSERVER_INFO_101;
begin
  Result:='Unknown OS';
  Buffer:= NIL;
  if NetServerGetInfo(nil, 101, Buffer) = NO_ERROR then
  TRY
    Result:= Format('%d.%d', [Buffer.sv101_version_major and MAJOR_VERSION_MASK, Buffer.sv101_version_minor]);
  FINALLY
    NetApiBufferFree(Buffer);
  END;
end;









{-------------------------------------------------------------------------------------------------------------
   GenerateReport
   For testing: generates a comprehensive report of all version detection methods.
   Useful for comparing results across different Windows versions and Delphi manifests.
-------------------------------------------------------------------------------------------------------------}
function GenerateReport: string;
CONST
   CRLF = #13#10;
   TAB  = #9;
VAR
   vMajor, vMinor: Cardinal;
begin
 Result:= '';

 { SysUtils.Win32MajorVersion - depends on app manifest
   Delphi XE7  -> On XE7/Win10 returns 6.2 which is WRONG (no Win10 manifest)
   Delphi 10.2 -> Reports correct version because manifest declares Win10 compatibility }
 Result:= Result+ '[SysUtils.Win32MajorVersion]'+CRLF;
 Result:= Result+ Tab+ IntToStr(System.SysUtils.Win32MajorVersion)+ '.'+ IntToStr(System.SysUtils.Win32MinorVersion)+ CRLF;
 Result:= Result+ CRLF;

 { GetWinVerNetServer - via Netapi32.dll }
 Result:= Result+ '[GetWinVerNetServer]'+ CRLF;
 Result:= Result+ Tab+ GetWinVerNetServer+ CRLF;
 Result:= Result+ CRLF;

 { GetWinVersion - via RtlGetVersion (most reliable) }
 GetWinVersion(vMajor, vMinor);
 Result:= Result+ '[GetWinVersion]'+ CRLF;
 Result:= Result+ Tab+ IntToStr(vMajor)+ '.'+IntToStr(vMinor)+ CRLF;
 Result:= Result+ CRLF;

 { GetWinVersionEx - via GetVersionEx API (returns OS name, not version number) }
 Result:= Result+ '[GetWinVersionEx]'+ CRLF;
 Result:= Result+ Tab+ GetWinVersionEx+ CRLF;
 Result:= Result+ CRLF;
end;



end.
