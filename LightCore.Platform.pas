UNIT LightCore.Platform;

{=============================================================================================================
   2025.12
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   Documentation:
      Contitional compilation: https://docwiki.embarcadero.com/RADStudio/Athens/en/Conditional_compilation

   Tester:
      c:\Projects\LightSaber\Demo\VCL\Demo SystemReport\VCL_Demo_SystemReport.dpr
      c:\Projects\LightSaber\Demo\FMX\Demo SystemReport\FMX_Demo_SystemReport.dpr
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Devices;


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
function GenerateAppBitnessRep: string; {$IFDEF FRAMEWORK_FMX}
function  GenerateDeviceRep: string;  {$ENDIF}

// Device
//function DeviceModel: string;
//function DevicePlatformDetails: string;

// Device locale
{
function LocaleLanguage: string;
function LocaleCurrencySymbol: string;
function LocaleDateFormat: string;}


IMPLEMENTATION
USES LightCore;






{$IFDEF FRAMEWORK_FMX}    // TDeviceInfo is only guaranteed to be available and functional in FMX contexts.
function GenerateDeviceRep: string;
var
  Device: TDeviceInfo;
begin
  Result:= ' [DEVICE INFO]'+ CRLF;

  Device:= TDeviceInfo.ThisDevice;
  if Device = nil then
  begin
    Result := Result + '  Error: TDeviceInfo.ThisDevice is NIL or device not recognized.';
    Exit;
  end;

  Result:= Result+'  Device ID: '                + Tab + Tab + Device.ID + CRLF;
  ///Result:= Result+'  Device Class: '          + Tab + Tab + Device.DeviceClass.ToString + CRLF;
  ///Result:= Result+'  Platform: '              + Tab + Tab + Device.Platform.ToString + CRLF;
  Result:= Result+'  Exclusive: '                + Tab + Tab + BoolToStr(Device.Exclusive, True) + CRLF;

  // Screen Metrics (Physical/Logical Sizes)
  Result:= Result+'  DPI (Pixels/Inch): '        + Tab + Device.PixelsPerInch.ToString + CRLF;
  Result:= Result+'  Aspect Ratio: '             + Tab + Format('%.3f', [Device.AspectRatio]) + CRLF;
  Result:= Result+'  Min Diagonal (in): '        + Tab + Format('%.2f', [Device.MinDiagonal]) + CRLF;
  Result:= Result+'  Max Diagonal (in): '        + Tab + Format('%.2f', [Device.MaxDiagonal]) + CRLF;

  // Screen Size (using MinLogicalSize as a proxy for reported size)
  Result:= Result+'  Logical Size (Min): '       + Tab + Format('%d x %d', [Device.MinLogicalScreenSize.cx, Device.MinLogicalScreenSize.cy]) + CRLF;
  Result:= Result+'  Physical Size (Min): '      + Tab + Format('%d x %d', [Device.MinPhysicalScreenSize.cx, Device.MinPhysicalScreenSize.cy]) + CRLF;
  {todo: AI: this crashes with "item not found"}
  {
  // Attributes Dictionary (contains OS, Version, DeviceName, etc.)
  Result:= Result+CRLF+'  [ATTRIBUTES]'+ CRLF;
  try
    // Common Attributes (Pulled from the Attributes dictionary directly)
    Result:= Result+'   DisplayName: '+ Tab+Device.Attributes[sDevAttrDisplayName]+ CRLF;
    Result:= Result+'   OPDefine: '+ Tab+Device.Attributes[sDevAttrOPDefine]+ CRLF;

    // Iterate over all attributes for maximum juice
    // NOTE: Accessing FAttributes directly is bad practice, but since TDeviceInfo is sealed and
    // there's no public iterator, we rely on the internal TAttributes property if needed,
    // or stick to the known keys. For safety, we'll try known keys first:
    Result:= Result+'   Manufacturer: '+ Tab+Device.Attributes['manufacturer']+ CRLF;
    Result:= Result+'   Model: '+ Tab+Tab+Device.Attributes['model']+ CRLF;
    Result:= Result+'   Version: '+ Tab+Tab+Device.Attributes['version']+ CRLF;
  except
    // Attributes might not exist, ignore errors.
  end; }
end;
{$ENDIF}





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
  case TOSVersion.Platform of
    pfAndroid, pfiOS: Result := True;
  else
    Result := False;
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



{ Same as above but returns a more detailed string }
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



{-------------------------------------------------------------------------------------------------------------
    REPORTS
-------------------------------------------------------------------------------------------------------------}

function GeneratePlatformRep: string;
begin
  Result:= ' [PLATFORM OS]'+ CRLF;
  Result:= Result+'  Platform: '         + Tab+Tab+ OsType+ CRLF;
  Result:= Result+'  OsArchitecture: '   + Tab    + OsArchitecture+ CRLF;
  Result:= Result+'  OS Version: '       + Tab+Tab+ OsVersion;
end;


function GenerateAppBitnessRep: string;
begin
  Result:= Result+ CRLF;
  Result:= Result+' [APP BITNESS]'+ CRLF;
  Result:= Result+'  AppBitness: '       + Tab    + AppBitness+ CRLF;
  Result:= Result+'  AppBitnessEx: '     + Tab    + AppBitnessEx+ CRLF;
  Result:= Result+'  Is64Bit: '          + Tab+Tab+ BoolToStr(AppIs64Bit, TRUE)+ CRLF;

  // DEVICE INFO
  //Result:= Result+'  Device Model: '     + Tab     + DeviceModel+ CRLF;
  //Result:= Result+'  Full Details: '     + Tab     + DevicePlatformDetails+ CRLF;
  Result:= Result+'  Is Mobile: '        + Tab+Tab+ BoolToStr(OsIsMobile, TRUE);
end;


{-------------------------------------------------------------------------------------------------------------
    LOCALES
-------------------------------------------------------------------------------------------------------------}

//ToDo: AI:  this does not exist. was a hallucination. fix it
(*
{ Returns the user's primary language and country code. Example: 'en-US' }
function LocaleLanguage: string;
begin
  Result := TLocale.Current.Language;
end;

{ Returns the user's currency symbol. Example: '$', '€', or 'RON' }
function LocaleCurrencySymbol: string;
begin
  Result := TLocale.Current.CurrencyString;
end;

{ Returns the user's date format short string. Example: 'M/d/yyyy' }
function LocaleDateFormat: string;
begin
  // TLocale doesn't expose a clean format string directly,
  // so we use the standard System.SysUtils format settings.
  Result := ShortDateFormat;
end;
        *)
end.
