UNIT cbVersion;

{=============================================================================================================
   2025.03
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Features:
     * Expands the TOSVersion. Use it to get Windows OS version.
     * GetVersionInfoFile(FileName) which returns info about an exe file.
==============================================================================================================

   Microsoft Window releases:
      Windows Serv 2019       10.0
      Windows 10              10.0 *
      Windows Serv 2016       10.0
      Windows 8.1             6.3
      Windows Serv 2012 R2    6.3
      -                               Manifest barrier!
      Windows 8               6.2
      Windows Serv 2012       6.2
      Windows 7               6.1  *
      Windows Serv 2008 R2    6.1
      Windows Vista           6.0
      Windows Serv 2008       6.0
      Windows XP 64-Bit       5.2  *
      Windows Serv 2003 R2    5.2
      Windows Serv 2003       5.2
      Windows XP              5.1  *
      Windows 2000            5.0
      Windows Millenium       4.9
      Windows 98              4.1
      Windows 95              4.0
      Windows NT 4.0          4.0
      Windows NT 3.51         3.51
      Windows NT 3.5          3.5
      Windows NT 3.1          3.1
     ---------------------------------------------
      https://techthoughts.info/windows-version-numbers/
     ---------------------------------------------

     Manifest barrier (from dummzeuch):
        Starting with Windows 8 the GerVersionEx function is lying. Quote ( https://docs.microsoft.com/en-us/windows/desktop/api/sysinfoapi/nf-sysinfoapi-getversionexa )
        "With the release of Windows 8.1, the behavior of the GetVersionEx API has changed in the value it will return for the operating system version.
        The value returned by the GetVersionEx function now depends on how the application is manifested.
        Applications not manifested for Windows 8.1 or Windows 10 will return the Windows 8 OS version value (6.2).
        Once an application is manifested for a given operating system version, GetVersionEx will always return the version that the application is manifested for in future releases. To manifest your applications  for Windows 8.1 or Windows 10"

        So, we can only get the correct version, if the Delphi IDE has a manifest telling Windows that it supports the version installed.
        This of course will not work if the Delphi version is older than the Windows version (e.g. Delphi 2007 won't know about anything newer than Windows XP).
        Instead we now use GetFileVersionInfo on kernel32.dll.
        https://docs.microsoft.com/en-us/windows/desktop/sysinfo/getting-the-system-version


     For support for Win11 see:
        https://stackoverflow.com/questions/68510685/how-to-detect-windows-11-using-delphi-10-3-3

     Tester:
        c:\MyProjects\Project Testers\cbVersion.pas\
=============================================================================================================}

INTERFACE
{ $I Frameworks.inc}

USES
   WinApi.Windows, System.SysUtils;


{-------------------------------------------------------------------------------------------------------------
   EXE file version
-------------------------------------------------------------------------------------------------------------}
function GetVersionInfoFile(CONST FileName: string; VAR FixedInfo: TVSFixedFileInfo): Boolean;

{-------------------------------------------------------------------------------------------------------------
   Check for specific OS version
-------------------------------------------------------------------------------------------------------------}
function  IsWindowsXP     : Boolean;
function  IsWindowsXPUp   : Boolean;
function  IsWindowsVista  : Boolean;
function  IsWindowsVistaUp: Boolean;
function  IsWindows7      : Boolean;
function  IsWindows7Up    : Boolean;
function  IsWindows8      : Boolean;
function  IsWindows8Up    : Boolean;
function  IsWindows10     : Boolean;
function  IsWindows10Up   : Boolean;
function  IsWindows11     : Boolean;


{-------------------------------------------------------------------------------------------------------------
   OS utils
-------------------------------------------------------------------------------------------------------------}
function GetOSName: string;
function GetOSDetails: string;
function Architecture: string;
function Is64Bit    : Boolean;
function IsNTKernel : Boolean;

function GenerateReport: string; { For testing }


IMPLEMENTATION    

const
   TAB  = #9;
   CRLF = #13#10;
   Win10FirstRel = 10240;  //July 29, 2015
   Win11FirstRel = 22000;  //October 4, 2021





{ Returns information (including version no) about an EXE file.
  Source: JCL }
function GetVersionInfoFile(CONST FileName: string; VAR FixedInfo: TVSFixedFileInfo): Boolean;
VAR
  Buffer: string;
  DummyHandle: DWORD;    { The DummyHandle parameter is ignored by GetFileVersionInfo }
  InfoSize, FixInfoLen: DWORD;
  FixInfoBuf: PVSFixedFileInfo;
begin
  Result := False;
  InfoSize := GetFileVersionInfoSize(PChar(FileName), DummyHandle);
  if InfoSize > 0 then
   begin
    FixInfoLen := 0;
    FixInfoBuf := Nil;

    SetLength(Buffer, InfoSize);
    if  GetFileVersionInfo(PChar(FileName), DummyHandle, InfoSize, Pointer(Buffer))
    AND VerQueryValue(Pointer(Buffer), '\', Pointer(FixInfoBuf), FixInfoLen)
    AND (FixInfoLen = SizeOf(TVSFixedFileInfo)) then
      begin
        Result := True;
        FixedInfo := FixInfoBuf^;
      end;
  end;
end;






{ Win XP }
function IsWindowsXP: Boolean;
begin
 Result:= (TOSVersion.Major = 5)
     AND ((TOSVersion.Build = 1) OR (TOSVersion.Build = 2));
end;

function IsWindowsXPUp: Boolean;
begin
 Result:= (TOSVersion.Major = 5) AND (TOSVersion.Build >= 1)
       OR (TOSVersion.Major > 5);
end;



{ Vista }
function IsWindowsVista: Boolean;
begin
 Result:= (TOSVersion.Major = 6) AND (TOSVersion.Build = 0);
end;

function IsWindowsVistaUp: Boolean;
begin
 Result:= (TOSVersion.Major = 6) AND (TOSVersion.Build >= 0)
       OR (TOSVersion.Major > 6);
end;



{ Win 7 }
function IsWindows7: Boolean;
begin
 Result:= (TOSVersion.Major = 6) AND (TOSVersion.Build = 1);
end;

function IsWindows7Up: Boolean;
begin
 Result:= (TOSVersion.Major = 6) AND (TOSVersion.Build >= 1)
       OR (TOSVersion.Major > 6);
end;



{ Win 8 }
function IsWindows8: Boolean;
begin
 Result:= (TOSVersion.Major = 6)
     AND ((TOSVersion.Build = 2) OR (TOSVersion.Build = 3));
end;

function IsWindows8Up: Boolean;
begin
 Result:= (TOSVersion.Major = 6) AND (TOSVersion.Build >= 2)
       OR (TOSVersion.Major > 6);
end;




{ Win 10//11 }
function IsWindows10: Boolean;
begin
 Result:= (TOSVersion.Major = 10) AND (TOSVersion.Build < Win11FirstRel);
end;

function IsWindows10Up: Boolean;
begin
 Result:= (TOSVersion.Major = 10) AND (TOSVersion.Build >= Win10FirstRel);
end;

function IsWindows11: Boolean;
begin
 Result:= (TOSVersion.Major = 10) AND (TOSVersion.Build >= Win11FirstRel);
end;









{-------------------------------------------------------------------------------------------------------------
   UTILS
-------------------------------------------------------------------------------------------------------------}

function Is64Bit: Boolean;
begin
  Result := TOSVersion.Architecture = arIntelX64
end;


function IsNTKernel: Boolean;                                                                                           { Win32Platform is defined as system var }
begin
 Result:= (Win32Platform = VER_PLATFORM_WIN32_NT);
end;


function Architecture: string;
begin
 case TOSVersion.Architecture of
    arIntelX86: Result := 'Intel 32bit';
    arIntelX64: Result := 'AMD 64bit';
    arARM32   : Result := 'ARM 32bit';
    arARM64   : Result := 'ARM 64bit';   //supported on Delphi 10+
   else
      Result:= 'Unknown architecture';
 end;
end;


{ On Delphi 11/Win10 it returns: Windows 10 (Version 10.0, Build 19041, 64-bit Edition) }
function GetOSName: String;
begin
 Result:= TOSVersion.ToString;
end;


function GetOSDetails: String;
begin
 Result:= '';
 Result:= Result+ '  '+ TOSVersion.ToString+ CRLF;  // Also see TOSVersion.Name & TOSVersion.Build
 Result:= Result+ '  '+ 'Major/Minor '  + IntToStr(TOSVersion.Major)+ '.'+ IntToStr(TOSVersion.Minor)+ '  Service Pack (Major/Minor): '+ IntToStr(TOSVersion.ServicePackMajor)+ '/'+ IntToStr(TOSVersion.ServicePackMinor)+ CRLF;
 Result:= Result+ '  '+ 'Architecture: '+ Architecture+ CRLF;  {x32/x64}
end;





{-------------------------------------------------------------------------------------------------------------
  Results:

    Delphi 10.4 (Sydney) Win7


    _________________________________________________

    Delphi 10.4 (Sydney) / Win10 (cris)


    _________________________________________________

    Delphi 12 (Alexandria) / Win10 (fntc)

    [GetWinVersion Is]
        IsWindowsXP: 	False
        IsWinVista: 	False
        IsWindows7: 	False
        IsWindows8: 	False
        IsWindows10: 	True
        IsWindows11: 	False

        IsWindowsXPUp: 	True
        IsWinVistaUp: 	True
        IsWindows7Up: 	True
        IsWindows8Up: 	True
        IsWindows10Up: 	True

    [GetOSDetails]
      Windows 10 (Version 10.0, Build 19041, 64-Bit-Edition)
      Major/Minor 10.0  Service Pack (Major/Minor): 0/0
      Architecture: AMD 64bit


-------------------------------------------------------------------------------------------------------------}

function GenerateReport: string;
begin
 Result:= '';

 Result:= Result+ '[GetWinVersion Is]'+ CRLF;
 Result:= Result+ Tab+ 'IsWindowsXP: '      + Tab+ BoolToStr(IsWindowsXP, TRUE)+ CRLF;
 Result:= Result+ Tab+ 'IsWinVista: '       + Tab+ BoolToStr(IsWindowsVista, TRUE)+ CRLF;
 Result:= Result+ Tab+ 'IsWindows7: '       + Tab+ BoolToStr(IsWindows7, TRUE)+ CRLF;
 Result:= Result+ Tab+ 'IsWindows8: '       + Tab+ BoolToStr(IsWindows8, TRUE)+ CRLF;
 Result:= Result+ Tab+ 'IsWindows10: '      + Tab+ BoolToStr(IsWindows10, TRUE)+ CRLF;
 Result:= Result+ Tab+ 'IsWindows11: '      + Tab+ BoolToStr(IsWindows11, TRUE)+ CRLF;

 Result:= Result+ CRLF;
 Result:= Result+ Tab+ 'IsWindowsXPUp: '    + Tab+ BoolToStr(IsWindowsXPUp, TRUE)+ CRLF;
 Result:= Result+ Tab+ 'IsWinVistaUp: '     + Tab+ BoolToStr(IsWindowsVistaUp, TRUE)+ CRLF;
 Result:= Result+ Tab+ 'IsWindows7Up: '     + Tab+ BoolToStr(IsWindows7Up, TRUE)+ CRLF;
 Result:= Result+ Tab+ 'IsWindows8Up: '     + Tab+ BoolToStr(IsWindows8Up, TRUE)+ CRLF;
 Result:= Result+ Tab+ 'IsWindows10Up: '    + Tab+ BoolToStr(IsWindows10Up, TRUE)+ CRLF;
 Result:= Result+ CRLF;

 Result:= Result+ '[GetOSDetails]'+ CRLF;
 Result:= Result+ GetOSDetails+ CRLF;
end;


end.
