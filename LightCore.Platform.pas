UNIT LightCore.Platform;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Platform detection utilities for cross-platform applications.

   Features:
     - OS type, version, and architecture detection
     - Application bitness detection (32/64-bit)
     - Platform reports for diagnostics
   Device information (GenerateDeviceRep) is in LightFmx.Common.Screen.

   Supported Platforms:
     - Windows (Desktop, WinRT)
     - macOS
     - Linux
     - iOS
     - Android

   Documentation:
      Conditional compilation: https://docwiki.embarcadero.com/RADStudio/Athens/en/Conditional_compilation

   Tester:
      c:\Projects\LightSaber\Demo\VCL\Demo SystemReport\VCL_Demo_SystemReport.dpr
      c:\Projects\LightSaber\Demo\FMX\Demo SystemReport\FMX_Demo_SystemReport.dpr
=============================================================================================================}

INTERFACE

USES
   System.SysUtils;


// OS
function OsType: string;
function OsArchitecture: string;
function OsIsMobile: Boolean;
function OsVersion: string;

// APP
function AppBitness: string;
function AppBitnessEx: string;
function AppIs64Bit: Boolean;

// Reports
function GeneratePlatformRep: string;
function GenerateAppBitnessRep: string;




IMPLEMENTATION
USES LightCore;






{-------------------------------------------------------------------------------------------------------------
   OS
-------------------------------------------------------------------------------------------------------------}
{ Tells us under which platform this program runs }
{ Returns the OS platform name as a user-friendly string }
function OsType: string;
begin
  case TOSVersion.Platform of
    pfWindows: Result := 'Windows';
    pfAndroid: Result := 'Android';
    pfIOS:     Result := 'iOS';
    pfMacOS:   Result := 'macOS';
    pfLinux:   Result := 'Linux';
    pfWinRT:   Result := 'WinRT';
  else
    Result := 'Unknown';
  end;
end;


{ Returns the specific OS version number
  Example:  "10.0" for Windows 10/11, "5.1.1" for Android}
function OsVersion: string;
begin
  Result := Format('%d.%d', [TOSVersion.Major, TOSVersion.Minor]);

  // Add Build/Patch information for more detail if available/relevant
  if TOSVersion.Build > 0
  then Result := Result + Format('.%d', [TOSVersion.Build]);
end;


{ Returns the OS architecture }
function OsArchitecture: string;
begin
  case TOSVersion.Architecture of
    arIntelX86: Result := 'Intel/AMD x86 (32-bit)';
    arIntelX64: Result := 'Intel/AMD x64 (64-bit)';
    arARM32:    Result := 'ARM (32-bit)';
    arARM64:    Result := 'ARM (64-bit)';    //supported on Delphi 10+
  else
    Result := 'Unidentified Architecture';
  end;
end;


function OsIsMobile: Boolean;
begin
  Result:= (TOSVersion.Platform = pfAndroid)
        OR (TOSVersion.Platform = pfiOS);
end;





{-------------------------------------------------------------------------------------------------------------
   APP
-------------------------------------------------------------------------------------------------------------}

{ Tells if this program is compiled as 32 or 64bit app }
function AppIs64Bit: Boolean;
begin
  {$IF Defined(CPU64BITS)}
    Result := TRUE;
  {$ELSE}
    Result := FALSE;
  {$ENDIF}
end;


{ Same as above but returns a string }
function AppBitness: String;
begin
 if AppIs64Bit
 then Result:= '64bit'
 else Result:= '32bit';
end;



{ Returns detailed CPU architecture information.
  Includes specific platform details like x86, x64, ARM32, ARM64. }
function AppBitnessEx: string;
begin
  Result:= '';

  { Desktop CPUs }
  {$IFDEF CPUX86}
    Result:= 'x86 (32-bit)';
  {$ENDIF}

  {$IFDEF CPUX64}
    Result:= 'x64 (64-bit)';
  {$ENDIF}

  { ARM CPUs (Mobile and embedded systems) }
  {$IFDEF CPUARM}
    {$IFDEF CPUARM64}
      Result:= 'ARM64 (64-bit)';
    {$ELSE}
      {$IFDEF CPUIOSARM}
        Result:= 'ARM (iOS 32-bit)';
      {$ELSE}
        Result:= 'ARM (32-bit)';
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

  { Fallback for unknown or future architectures }
  if Result = '' then
  begin
    {$IFDEF CPU64BITS}
      Result:= '64-bit (Unknown CPU)';
    {$ELSEIF Defined(CPU32BITS)}
      Result:= '32-bit (Unknown CPU)';
    {$ELSE}
      Result:= 'Unknown Architecture';
    {$ENDIF}
  end;
end;



{-------------------------------------------------------------------------------------------------------------
    REPORTS
-------------------------------------------------------------------------------------------------------------}

function GeneratePlatformRep: string;
begin
  Result:= ' [PLATFORM OS]' + CRLF;
  Result:= Result + '  Platform: '       + Tab + Tab + OsType + CRLF;
  Result:= Result + '  OsArchitecture: '   + Tab    + OsArchitecture+ CRLF;
  Result:= Result + '  OS Version: '     + Tab + Tab + OsVersion;
end;


function GenerateAppBitnessRep: string;
begin
  Result:= ' [APP BITNESS]' + CRLF;
  Result:= Result + '  AppBitness: '   + Tab + AppBitness + CRLF;
  Result:= Result + '  AppBitnessEx: ' + Tab + AppBitnessEx + CRLF;
  Result:= Result + '  Is64Bit: '      + Tab + Tab + BoolToStr(AppIs64Bit, TRUE) + CRLF;
  Result:= Result + '  Is Mobile: '    + Tab + Tab + BoolToStr(OsIsMobile, TRUE);
end;


end.
