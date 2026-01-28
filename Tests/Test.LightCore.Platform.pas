unit Test.LightCore.Platform;

{=============================================================================================================
   Unit tests for LightCore.Platform
   Tests platform detection and reporting utilities

   Note: Many functions return values based on the current OS/CPU, so tests verify
   output format and consistency rather than specific values.

   Requires: TESTINSIGHT compiler directive for TestInsight integration
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
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
    procedure TestOsType_ReturnsNonEmpty;

    [Test]
    procedure TestOsType_ReturnsKnownPlatform;

    { OsVersion Tests }
    [Test]
    procedure TestOsVersion_ReturnsNonEmpty;

    [Test]
    procedure TestOsVersion_ContainsDot;

    [Test]
    procedure TestOsVersion_StartsWithDigit;

    { OsArchitecture Tests }
    [Test]
    procedure TestOsArchitecture_ReturnsNonEmpty;

    [Test]
    procedure TestOsArchitecture_ContainsBitInfo;

    { OsIsMobile Tests }
    [Test]
    procedure TestOsIsMobile_ReturnsBoolean;

    [Test]
    procedure TestOsIsMobile_ConsistentWithOsType;

    { AppIs64Bit Tests }
    [Test]
    procedure TestAppIs64Bit_ReturnsBoolean;

    [Test]
    procedure TestAppIs64Bit_ConsistentWithBitness;

    { AppBitness Tests }
    [Test]
    procedure TestAppBitness_Returns32Or64;

    [Test]
    procedure TestAppBitness_ConsistentWithAppIs64Bit;

    { AppBitnessEx Tests }
    [Test]
    procedure TestAppBitnessEx_ReturnsNonEmpty;

    [Test]
    procedure TestAppBitnessEx_ContainsBitInfo;

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


procedure TTestLightCorePlatform.Setup;
begin
  { No setup needed }
end;


procedure TTestLightCorePlatform.TearDown;
begin
  { No teardown needed }
end;


{ OsType Tests }

procedure TTestLightCorePlatform.TestOsType_ReturnsNonEmpty;
begin
  Assert.IsNotEmpty(OsType);
end;


procedure TTestLightCorePlatform.TestOsType_ReturnsKnownPlatform;
var
  OS: string;
begin
  OS:= OsType;
  Assert.IsTrue(
    (OS = 'Windows') OR
    (OS = 'Android') OR
    (OS = 'iOS') OR
    (OS = 'macOS') OR
    (OS = 'Linux') OR
    (OS = 'pfWinRT') OR
    (OS = 'Unknown'),
    'OsType should return a known platform name'
  );
end;


{ OsVersion Tests }

procedure TTestLightCorePlatform.TestOsVersion_ReturnsNonEmpty;
begin
  Assert.IsNotEmpty(OsVersion);
end;


procedure TTestLightCorePlatform.TestOsVersion_ContainsDot;
begin
  Assert.IsTrue(Pos('.', OsVersion) > 0, 'Version should contain at least one dot');
end;


procedure TTestLightCorePlatform.TestOsVersion_StartsWithDigit;
var
  Ver: string;
begin
  Ver:= OsVersion;
  Assert.IsTrue(CharInSet(Ver[1], ['0'..'9']), 'Version should start with a digit');
end;


{ OsArchitecture Tests }

procedure TTestLightCorePlatform.TestOsArchitecture_ReturnsNonEmpty;
begin
  Assert.IsNotEmpty(OsArchitecture);
end;


procedure TTestLightCorePlatform.TestOsArchitecture_ContainsBitInfo;
var
  Arch: string;
begin
  Arch:= OsArchitecture;
  Assert.IsTrue(
    (Pos('32-bit', Arch) > 0) OR
    (Pos('64-bit', Arch) > 0) OR
    (Pos('Unidentified', Arch) > 0),
    'Architecture should mention bit width or be unidentified'
  );
end;


{ OsIsMobile Tests }

procedure TTestLightCorePlatform.TestOsIsMobile_ReturnsBoolean;
var
  IsMobile: Boolean;
begin
  IsMobile:= OsIsMobile;
  { Just verify it doesn't crash and returns a valid boolean }
  Assert.IsTrue((IsMobile = True) OR (IsMobile = False));
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
    then Assert.IsFalse(IsMobile, 'Desktop OS should not be mobile');
end;


{ AppIs64Bit Tests }

procedure TTestLightCorePlatform.TestAppIs64Bit_ReturnsBoolean;
var
  Is64: Boolean;
begin
  Is64:= AppIs64Bit;
  Assert.IsTrue((Is64 = True) OR (Is64 = False));
end;


procedure TTestLightCorePlatform.TestAppIs64Bit_ConsistentWithBitness;
begin
  if AppIs64Bit
  then Assert.AreEqual('64bit', AppBitness)
  else Assert.AreEqual('32bit', AppBitness);
end;


{ AppBitness Tests }

procedure TTestLightCorePlatform.TestAppBitness_Returns32Or64;
var
  Bitness: string;
begin
  Bitness:= AppBitness;
  Assert.IsTrue((Bitness = '32bit') OR (Bitness = '64bit'),
    'AppBitness should return "32bit" or "64bit"');
end;


procedure TTestLightCorePlatform.TestAppBitness_ConsistentWithAppIs64Bit;
begin
  if AppIs64Bit
  then Assert.AreEqual('64bit', AppBitness)
  else Assert.AreEqual('32bit', AppBitness);
end;


{ AppBitnessEx Tests }

procedure TTestLightCorePlatform.TestAppBitnessEx_ReturnsNonEmpty;
begin
  Assert.IsNotEmpty(AppBitnessEx);
end;


procedure TTestLightCorePlatform.TestAppBitnessEx_ContainsBitInfo;
var
  BitnessEx: string;
begin
  BitnessEx:= AppBitnessEx;
  Assert.IsTrue(
    (Pos('32-bit', BitnessEx) > 0) OR
    (Pos('64-bit', BitnessEx) > 0) OR
    (Pos('32bit', BitnessEx) > 0) OR
    (Pos('64bit', BitnessEx) > 0),
    'AppBitnessEx should contain bit information'
  );
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
  Assert.IsTrue(Pos('Platform:', GeneratePlatformRep) > 0);
end;


procedure TTestLightCorePlatform.TestGeneratePlatformRep_ContainsArchitecture;
begin
  Assert.IsTrue(Pos('Architecture:', GeneratePlatformRep) > 0);
end;


procedure TTestLightCorePlatform.TestGeneratePlatformRep_ContainsVersion;
begin
  Assert.IsTrue(Pos('OS Version:', GeneratePlatformRep) > 0);
end;


{ GenerateAppBitnessRep Tests }

procedure TTestLightCorePlatform.TestGenerateAppBitnessRep_ContainsHeader;
begin
  Assert.IsTrue(Pos('[APP BITNESS]', GenerateAppBitnessRep) > 0);
end;


procedure TTestLightCorePlatform.TestGenerateAppBitnessRep_ContainsBitness;
begin
  Assert.IsTrue(Pos('AppBitness:', GenerateAppBitnessRep) > 0);
end;


procedure TTestLightCorePlatform.TestGenerateAppBitnessRep_ContainsIs64Bit;
begin
  Assert.IsTrue(Pos('Is64Bit:', GenerateAppBitnessRep) > 0);
end;


procedure TTestLightCorePlatform.TestGenerateAppBitnessRep_ContainsIsMobile;
begin
  Assert.IsTrue(Pos('Is Mobile:', GenerateAppBitnessRep) > 0);
end;


{ Cross-function Consistency Tests }

procedure TTestLightCorePlatform.TestConsistency_BitnessMatchesArchitecture;
var
  Arch: string;
  Is64: Boolean;
begin
  Arch:= OsArchitecture;
  Is64:= AppIs64Bit;

  { Note: A 32-bit app can run on 64-bit OS (WoW64), so we can only verify
    that a 64-bit app runs on a 64-bit OS }
  if Is64 then
    Assert.IsTrue(Pos('64-bit', Arch) > 0,
      '64-bit app should run on 64-bit OS architecture');
end;


initialization
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  {$ENDIF}
  TDUnitX.RegisterTestFixture(TTestLightCorePlatform);

end.
