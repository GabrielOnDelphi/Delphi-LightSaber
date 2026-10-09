unit Test.LightCore.Platform;

{=============================================================================================================
   2026.10.07
   Unit tests for LightCore.Platform
   Tests platform detection and reporting utilities

   The expected values come from a source the routine under test does not use: the compile target
   (conditional defines, SizeOf(Pointer)) or, on Windows, RtlGetVersion and GetNativeSystemInfo called directly.

   Requires: TESTINSIGHT compiler directive for TestInsight integration
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  LightCore.Platform;

type
  [TestFixture]
  TTestLightCorePlatform = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { OsType Tests }
    [Test]
    procedure TestOsType_ReturnsCompileTargetName;

    { OsVersion Tests }
    [Test]
    procedure TestOsVersion_MatchesRtlGetVersion;

    { OsArchitecture Tests }
    [Test]
    procedure TestOsArchitecture_MatchesNativeSystemInfo;

    { OsIsMobile Tests }
    [Test]
    procedure TestOsIsMobile_MatchesCompileTarget;

    [Test]
    procedure TestOsIsMobile_ConsistentWithOsType;

    { AppIs64Bit Tests }
    [Test]
    procedure TestAppIs64Bit_MatchesPointerSize;

    [Test]
    procedure TestAppIs64Bit_ConsistentWithBitness;

    { AppBitness Tests }
    [Test]
    procedure TestAppBitness_MatchesPointerSize;

    [Test]
    procedure TestAppBitness_ConsistentWithAppIs64Bit;

    { AppBitnessEx Tests }
    [Test]
    procedure TestAppBitnessEx_MatchesCompileTarget;

    [Test]
    procedure TestAppBitnessEx_ConsistentWithAppIs64Bit;

    { GeneratePlatformRep Tests }
    [Test]
    procedure TestGeneratePlatformRep_ContainsHeader;

    [Test]
    procedure TestGeneratePlatformRep_ContainsPlatform;

    [Test]
    procedure TestGeneratePlatformRep_ContainsArchitecture;

    [Test]
    procedure TestGeneratePlatformRep_ContainsVersion;

    { GenerateAppBitnessRep Tests }
    [Test]
    procedure TestGenerateAppBitnessRep_ContainsHeader;

    [Test]
    procedure TestGenerateAppBitnessRep_ContainsBitness;

    [Test]
    procedure TestGenerateAppBitnessRep_ContainsIs64Bit;

    [Test]
    procedure TestGenerateAppBitnessRep_ContainsIsMobile;

    { Cross-function Consistency Tests }
    [Test]
    procedure TestConsistency_BitnessMatchesArchitecture;
  end;

implementation


{$IFDEF MSWINDOWS}
{ ntdll reports the real Windows version, with no manifest-based shim }
function RtlGetVersion(var VersionInfo: TOSVersionInfoExW): LongInt; stdcall; external 'ntdll.dll';
{$ENDIF}


{ The OsType name of the platform this EXE was compiled for. IOS is tested before MACOS and ANDROID before LINUX, because the compiler also defines the second symbol for the first platform. }
function CompileTargetOsName: string;
begin
  {$IF Defined(MSWINDOWS)}
  Result:= 'Windows';
  {$ELSEIF Defined(ANDROID)}
  Result:= 'Android';
  {$ELSEIF Defined(IOS)}
  Result:= 'iOS';
  {$ELSEIF Defined(MACOS)}
  Result:= 'macOS';
  {$ELSEIF Defined(LINUX)}
  Result:= 'Linux';
  {$ELSE}
  Result:= 'Unknown';
  {$ENDIF}
end;


{ The text after LabelText on the report line that starts with LabelText (leading spaces and the tabs around the value removed).
  Returns '<label not found>' when no line starts with LabelText. }
function ReportValue(CONST Report, LabelText: string): string;
VAR
  Line: string;
begin
  for Line in Report.Split([#13#10]) do
    if TrimLeft(Line).StartsWith(LabelText)
    then EXIT(Trim(Copy(TrimLeft(Line), Length(LabelText) + 1, MaxInt)));
  Result:= '<label not found>';
end;


{ Major.Minor.Build of the running Windows, as RtlGetVersion reports it }
function ExpectedOsVersion: string;
{$IFDEF MSWINDOWS}
VAR
  Info: TOSVersionInfoExW;
begin
  Info:= Default(TOSVersionInfoExW);
  Info.dwOSVersionInfoSize:= SizeOf(Info);
  Assert.AreEqual(0, RtlGetVersion(Info), 'RtlGetVersion failed');

  Result:= IntToStr(Info.dwMajorVersion) + '.' + IntToStr(Info.dwMinorVersion);
  if Info.dwBuildNumber > 0
  then Result:= Result + '.' + IntToStr(Info.dwBuildNumber);
end;
{$ELSE}
begin
  Result:= '';
end;
{$ENDIF}


{ The CPU name OsArchitecture must give, from GetNativeSystemInfo }
function ExpectedOsArchitecture: string;
{$IFDEF MSWINDOWS}
VAR
  Info: TSystemInfo;
begin
  Info:= Default(TSystemInfo);
  GetNativeSystemInfo(Info);
  case Info.wProcessorArchitecture of
    PROCESSOR_ARCHITECTURE_INTEL: Result:= 'Intel/AMD x86 (32-bit)';
    PROCESSOR_ARCHITECTURE_AMD64: Result:= 'Intel/AMD x64 (64-bit)';
    PROCESSOR_ARCHITECTURE_ARM64: Result:= 'ARM (64-bit)';
  else
    Result:= 'Unidentified Architecture';
  end;
end;
{$ELSE}
begin
  Result:= '';
end;
{$ENDIF}


procedure TTestLightCorePlatform.Setup;
begin
  { No setup needed }
end;


procedure TTestLightCorePlatform.TearDown;
begin
  { No teardown needed }
end;


{ OsType Tests }

procedure TTestLightCorePlatform.TestOsType_ReturnsCompileTargetName;
begin
  Assert.AreEqual(CompileTargetOsName, OsType, 'OsType must name the platform this EXE was compiled for');
end;


{ OsVersion Tests }

procedure TTestLightCorePlatform.TestOsVersion_MatchesRtlGetVersion;
{$IFDEF MSWINDOWS}
begin
  Assert.AreEqual(ExpectedOsVersion, OsVersion, 'OsVersion must be Major.Minor.Build as RtlGetVersion reports it');
end;
{$ELSE}
begin
  Assert.IsTrue(CharInSet(OsVersion[1], ['0'..'9']), 'Version should start with a digit');
end;
{$ENDIF}


{ OsArchitecture Tests }

procedure TTestLightCorePlatform.TestOsArchitecture_MatchesNativeSystemInfo;
{$IFDEF MSWINDOWS}
begin
  Assert.AreEqual(ExpectedOsArchitecture, OsArchitecture, 'OsArchitecture must name the CPU that GetNativeSystemInfo reports');
end;
{$ELSE}
begin
  Assert.IsNotEmpty(OsArchitecture);
end;
{$ENDIF}


{ OsIsMobile Tests }

procedure TTestLightCorePlatform.TestOsIsMobile_MatchesCompileTarget;
begin
  {$IF Defined(ANDROID) OR Defined(IOS)}
  Assert.IsTrue(OsIsMobile, 'An Android or iOS build must report a mobile OS');
  {$ELSE}
  Assert.IsFalse(OsIsMobile, 'A desktop build must not report a mobile OS');
  {$ENDIF}
end;


procedure TTestLightCorePlatform.TestOsIsMobile_ConsistentWithOsType;
var
  OS: string;
  IsMobile: Boolean;
begin
  OS:= OsType;
  IsMobile:= OsIsMobile;

  if (OS = 'Android') OR (OS = 'iOS')
  then Assert.IsTrue(IsMobile, 'Android/iOS should be mobile')
  else
    if (OS = 'Windows') OR (OS = 'macOS') OR (OS = 'Linux')
    then Assert.IsFalse(IsMobile, 'Desktop OS should not be mobile')
    else Assert.Fail('OsType returned "' + OS + '", which is no platform this library supports');
end;


{ AppIs64Bit Tests }

procedure TTestLightCorePlatform.TestAppIs64Bit_MatchesPointerSize;
begin
  Assert.IsTrue((SizeOf(Pointer) = 8) = AppIs64Bit, 'AppIs64Bit must be TRUE exactly when a pointer is 8 bytes');
end;


procedure TTestLightCorePlatform.TestAppIs64Bit_ConsistentWithBitness;
begin
  if AppIs64Bit
  then Assert.AreEqual('64bit', AppBitness)
  else Assert.AreEqual('32bit', AppBitness);
end;


{ AppBitness Tests }

procedure TTestLightCorePlatform.TestAppBitness_MatchesPointerSize;
var
  Expected: string;
begin
  if SizeOf(Pointer) = 8
  then Expected:= '64bit'
  else Expected:= '32bit';
  Assert.AreEqual(Expected, AppBitness);
end;


procedure TTestLightCorePlatform.TestAppBitness_ConsistentWithAppIs64Bit;
begin
  if AppIs64Bit
  then Assert.AreEqual('64bit', AppBitness)
  else Assert.AreEqual('32bit', AppBitness);
end;


{ AppBitnessEx Tests }

procedure TTestLightCorePlatform.TestAppBitnessEx_MatchesCompileTarget;
var
  Expected: string;
begin
  {$IF Defined(CPUX86) OR Defined(CPUX64)}
  if SizeOf(Pointer) = 8
  then Expected:= 'x64 (64-bit)'
  else Expected:= 'x86 (32-bit)';
  Assert.AreEqual(Expected, AppBitnessEx);
  {$ELSE}
  Expected:= IntToStr(SizeOf(Pointer) * 8) + '-bit';
  Assert.IsTrue(Pos(Expected, AppBitnessEx) > 0, 'AppBitnessEx must contain ' + Expected);
  {$ENDIF}
end;


procedure TTestLightCorePlatform.TestAppBitnessEx_ConsistentWithAppIs64Bit;
var
  BitnessEx: string;
begin
  BitnessEx:= AppBitnessEx;
  if AppIs64Bit
  then Assert.IsTrue(Pos('64', BitnessEx) > 0, 'Should contain 64 for 64-bit app')
  else Assert.IsTrue(Pos('32', BitnessEx) > 0, 'Should contain 32 for 32-bit app');
end;


{ GeneratePlatformRep Tests }

procedure TTestLightCorePlatform.TestGeneratePlatformRep_ContainsHeader;
begin
  Assert.IsTrue(Pos('[PLATFORM OS]', GeneratePlatformRep) > 0);
end;


procedure TTestLightCorePlatform.TestGeneratePlatformRep_ContainsPlatform;
begin
  { Expected value: the compile target, not TOSVersion that OsType reads }
  Assert.AreEqual(CompileTargetOsName, ReportValue(GeneratePlatformRep, 'Platform:'), 'The Platform line must name the platform this EXE was compiled for');
end;


procedure TTestLightCorePlatform.TestGeneratePlatformRep_ContainsArchitecture;
begin
  {$IFDEF MSWINDOWS}
  { Expected value: GetNativeSystemInfo, not TOSVersion that OsArchitecture reads }
  Assert.AreEqual(ExpectedOsArchitecture, ReportValue(GeneratePlatformRep, 'OsArchitecture:'), 'The OsArchitecture line must name the CPU that GetNativeSystemInfo reports');
  {$ELSE}
  Assert.IsTrue(ReportValue(GeneratePlatformRep, 'OsArchitecture:').Contains('-bit'), 'The OsArchitecture line must name a bit width');
  {$ENDIF}
end;


procedure TTestLightCorePlatform.TestGeneratePlatformRep_ContainsVersion;
begin
  {$IFDEF MSWINDOWS}
  { Expected value: RtlGetVersion, not TOSVersion that OsVersion reads }
  Assert.AreEqual(ExpectedOsVersion, ReportValue(GeneratePlatformRep, 'OS Version:'), 'The OS Version line must hold Major.Minor.Build as RtlGetVersion reports it');
  {$ELSE}
  Assert.IsTrue(CharInSet(ReportValue(GeneratePlatformRep, 'OS Version:')[1], ['0'..'9']), 'The OS Version value must start with a digit');
  {$ENDIF}
end;


{ GenerateAppBitnessRep Tests }

procedure TTestLightCorePlatform.TestGenerateAppBitnessRep_ContainsHeader;
begin
  Assert.IsTrue(Pos('[APP BITNESS]', GenerateAppBitnessRep) > 0);
end;


procedure TTestLightCorePlatform.TestGenerateAppBitnessRep_ContainsBitness;
var
  Expected: string;
begin
  if SizeOf(Pointer) = 8
  then Expected:= '64bit'
  else Expected:= '32bit';
  Assert.AreEqual(Expected, ReportValue(GenerateAppBitnessRep, 'AppBitness:'), 'The AppBitness line must match the pointer size');
end;


procedure TTestLightCorePlatform.TestGenerateAppBitnessRep_ContainsIs64Bit;
var
  Expected: string;
begin
  { BoolToStr(B, TRUE) writes 'True' / 'False' }
  if SizeOf(Pointer) = 8
  then Expected:= 'True'
  else Expected:= 'False';
  Assert.AreEqual(Expected, ReportValue(GenerateAppBitnessRep, 'Is64Bit:'), 'The Is64Bit line must match the pointer size');
end;


procedure TTestLightCorePlatform.TestGenerateAppBitnessRep_ContainsIsMobile;
var
  Expected: string;
begin
  {$IF Defined(ANDROID) OR Defined(IOS)}
  Expected:= 'True';
  {$ELSE}
  Expected:= 'False';
  {$ENDIF}
  Assert.AreEqual(Expected, ReportValue(GenerateAppBitnessRep, 'Is Mobile:'), 'The Is Mobile line must match the compile target');
end;


{ Cross-function Consistency Tests }

procedure TTestLightCorePlatform.TestConsistency_BitnessMatchesArchitecture;
var
  Arch: string;
  {$IFDEF MSWINDOWS}
  IsWow64: BOOL;
  {$ENDIF}
begin
  Arch:= OsArchitecture;

  if AppIs64Bit
  then Assert.IsTrue(Pos('64-bit', Arch) > 0, '64-bit app should run on 64-bit OS architecture')
  else
    begin
      {$IFDEF MSWINDOWS}
      { A 32-bit process runs under WoW64 exactly when the OS is 64-bit }
      IsWow64:= FALSE;
      Assert.IsTrue(IsWow64Process(GetCurrentProcess, IsWow64), 'IsWow64Process failed');
      if IsWow64
      then Assert.IsTrue(Pos('64-bit', Arch) > 0, 'A 32-bit app under WoW64 runs on a 64-bit OS. Got: ' + Arch)
      else Assert.IsTrue(Pos('32-bit', Arch) > 0, 'A 32-bit app outside WoW64 runs on a 32-bit OS. Got: ' + Arch);
      {$ELSE}
      Assert.IsTrue((Pos('32-bit', Arch) > 0) OR (Pos('64-bit', Arch) > 0), 'Architecture must name its bit width. Got: ' + Arch);
      {$ENDIF}
    end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestLightCorePlatform);

end.
