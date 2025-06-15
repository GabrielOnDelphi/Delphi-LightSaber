UNIT  LightVcl.Common.WinVersionApi;

{=============================================================================================================
   2025.05
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   Other functions (based on RtlGetVersion API)
   Better use LightVcl.Common.WinVersion.pas instead of this because it relies on TOSVersion.

   This library provides 3 ways to get Windows version:
      Using RtlGetVersion in NtDLL.dll
      Using GetVersionEx
      Using NetServerGetInfo

   A 4th alternative is GetKernel32Version which uses GetFileVersionInfo on kernel32.dll
   https://github.com/aehimself/AEFramework/blob/master/3rdParty/OSVersion.pas

   Also see:
      LightVcl.Common.WinVersion
      LightVcl.Common.ExeVersion
      LightVcl.Common.WinVersionApi

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
USES LightCore.Core;




{-------------------------------------------------------------------------------------------------------------
   Read RTL Version directly from NTDLL
   On Win10 returns 10.0
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


{ Verified: IT WORKS ON WINDOWS 10 also }
//ToDo: don't use Buffer. Use the OsVinfo.dwMajorVersion/OsVinfo.dwMinorVersion directly
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

 s:= Buffer;
 i:= Pos('Build', s);
 if i > 0 then
   begin
    Temp:= copy(s,i+6,maxint);   //on win11 I get:  'Microsoft Windows NT Version 10.00, Build 22621, '
    i:= ExtractIntFromStr(Temp);
    if i>  900 then Result:='95';
    if i> 1900 then Result:='98';
    if i> 2000 then Result:='2000';
    if i> 2500 then Result:='XP';
    if i> 3000 then Result:='2003';
    if i> 5000 then Result:='Vista';
    if i> 6000 then Result:='2008';
    if i> 7000 then Result:='7';
    if i> 8000 then Result:='8';
    if i> 9000 then Result:='8.1';
    if i> 9100 then Result:='10';
    if i>=2200 then Result:='11';
   end;
end;




{-------------------------------------------------------------------------------------------------------------
   NetServerGetInfo
   Tested: On Win10 returns 10.0
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
  TestUnit
-------------------------------------------------------------------------------------------------------------}

function GenerateReport: string;
CONST
   CRLF = #13#10;
   TAB  = #9;
VAR
   vMajor, vMinor: Cardinal;
begin
 Result:= '';

 { Delphi XE7  -> On XE7/Win10 returns 6.2 which is WRONG
   Delphi 10.2 -> It reports the correct Win version on Win10 because the manifest generated by Delphi 10.2 says the app is Win10 ready }
 Result:= Result+ '[SysUtils.Win32MajorVersion]'+CRLF;
 Result:= Result+ Tab+ IntToStr(System.SysUtils.Win32MajorVersion)+ '.'+ IntToStr(System.SysUtils.Win32MinorVersion)+ CRLF;
 Result:= Result+ CRLF;

 Result:= Result+ '[GetWinVerNetServer]'+ CRLF;
 Result:= Result+ Tab+ GetWinVerNetServer+ CRLF;
 Result:= Result+ CRLF;

 GetWinVersion(vMajor, vMinor);
 Result:= Result+ '[GetWinVersion]'+ CRLF;     { Works on Win10 }
 Result:= Result+ Tab+ IntToStr(vMajor)+ '.'+IntToStr(vMinor)+ CRLF;
 Result:= Result+ CRLF;
end;



end.
