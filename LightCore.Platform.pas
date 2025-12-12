UNIT LightCore.Platform;

{=============================================================================================================
   2025.09
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   Documentation:
      Contitional compilation: https://docwiki.embarcadero.com/RADStudio/Athens/en/Conditional_compilation

   Tester
      c:\Projects\LightSaber\Demo\VCL\Demo SystemReport\VCL_Demo_SystemReport.dpr
=============================================================================================================}

INTERFACE

USES
   System.SysUtils;


//OS
function OsType: string;
function OsArchitecture: string;
function OsIsMobile: Boolean;

//APP
function AppBitness: string;
function AppBitnessEx: string;
function AppIs64Bit: Boolean;


IMPLEMENTATION


{-------------------------------------------------------------------------------------------------------------
   OS
-------------------------------------------------------------------------------------------------------------}
{ Tells us under which platform this program runs }
function OsType: string;
begin
  case TOSVersion.Platform of
    pfWindows: Result := 'Windows';
    pfAndroid: Result := 'Android';
    pfIOS:     Result := 'iOS';
    pfMacOS:   Result := 'macOS';
    pfLinux:   Result := 'Linux';
    pfWinRT:   Result := 'pfWinRT';
  else
    Result := 'Unknown';
  end;
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
  case TOSVersion.Platform of
    pfAndroid, pfiOS: Result := True;
  else Result := False;
  end;
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



{ Tells us under which architecture was this program compiled }
function AppBitnessEx: string;
begin
  Result := ''; // Initialize Result

  //  Desktop CPUs
  {$IFDEF CPUX86}
    Result := 'x86 (32-bit)';
  {$ENDIF}

  {$IFDEF CPUX64}
    Result := 'x64 (64-bit)';
  {$ENDIF}

  // Mobile/Embedded CPUs
  // ARM (Mobile and embedded systems)
  {$IFDEF CPUARM}
    // Differentiating between ARM versions is complex; use the most specific flag when available.
    {$IFDEF CPUIOSARM}
      // Specifically for iOS/tvOS ARM 32-bit (older devices)
      Result := 'ARM (iOS 32-bit)';
    {$ENDIF}

    {$IFDEF CPUARM64}
      // General ARM 64-bit (Android, iOS, macOS (Simulators can run x64))
      Result := 'ARM64 (64-bit)';
    {$ENDIF}
  {$ENDIF}

  // Fallback
  if Result = '' then
  begin
    // Check general IFDEFs if specific CPU tags weren't found (e.g., if a new CPU tag is introduced)
    {$IFDEF CPU32BITS}
      Result := '32-bit Architecture';
    {$ELSE}
      // Nested check for 64-bit architecture
      {$IFDEF CPU64BITS}
        Result := '64-bit Architecture';
      {$ELSE}
        // Final fallback if neither 32-bit nor 64-bit was defined
        Result := 'CPU Architecture Unknown';
      {$ENDIF}
    {$ENDIF}
  end;

  // Final check to handle older ARM definitions if CPUARM wasn't specific enough
  // and we still have an empty result.
  if Result = '' then
  begin
    {$IFDEF CPUIOSARM}
      Result := 'iOS ARM';
    {$ELSE}
      {$IFDEF CPUNONIOSARM}
        Result := 'Non-iOS ARM (e.g., Android)';
      {$ENDIF}
    {$ENDIF}
  end;
end;


end.
