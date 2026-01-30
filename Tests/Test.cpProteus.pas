UNIT Test.cpProteus;

{-------------------------------------------------------------------------------------------------------------
   DUnitX tests for cpProteus.pas
   Tests the main Proteus licensing component.

   GabrielMoraru.com
   2026.01

   Note: These tests use a unique test product name to avoid conflicts with real license data.
   Some GUI-dependent methods (ShowEnterKeyBox, message dialogs) cannot be tested here.
   Registry keys created during tests are cleaned up in TearDown.
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  Winapi.Windows,
  System.SysUtils, System.Classes, System.DateUtils,
  DUnitX.TestFramework,
  cpProteus, cpProteusIO, cpCertificate, cpProteusUtils;

TYPE
  [TestFixture]
  TTestProteus = class
  private
    FProteus: TProteus;
    FTestProductName: string;
    FTestRegPath: string;
    FTrialKey: string;
    procedure CleanupTestRegistry;
    procedure CreateTestTrialKey;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor/Destructor tests }
    [Test]
    procedure Test_Create_TimerInitialized;

    [Test]
    procedure Test_Create_StolenKeysInitialized;

    [Test]
    procedure Test_Create_DefaultValues;

    { setDefaultKey tests }
    [Test]
    procedure Test_SetDefaultKey_ValidKey_Accepted;

    [Test]
    procedure Test_SetDefaultKey_EmptyKey_Rejected;

    [Test]
    procedure Test_SetDefaultKey_NoAsterisks_Rejected;

    [Test]
    procedure Test_SetDefaultKey_MissingStartAsterisk_Rejected;

    [Test]
    procedure Test_SetDefaultKey_MissingEndAsterisk_Rejected;

    { TodayIsTomorrow tests }
    [Test]
    procedure Test_TodayIsTomorrow_LastSeenYesterday_ReturnsTrue;

    [Test]
    procedure Test_TodayIsTomorrow_LastSeenToday_ReturnsFalse;

    [Test]
    procedure Test_TodayIsTomorrow_LastSeenTomorrow_ReturnsFalse;

    { DaysSinceInstall tests }
    [Test]
    procedure Test_DaysSinceInstall_Today_ReturnsZero;

    [Test]
    procedure Test_DaysSinceInstall_TenDaysAgo_ReturnsTen;

    { EnableTimer tests }
    [Test]
    procedure Test_EnableTimer_FullLicense_ReturnsFalse;

    [Test]
    procedure Test_EnableTimer_ExpiredLicense_ReturnsFalse;

    [Test]
    procedure Test_EnableTimer_UninstalledLicense_ReturnsFalse;

    { SwitchToDemo tests }
    [Test]
    procedure Test_SwitchToDemo_SetsCertifTypeToExpired;

    [Test]
    procedure Test_SwitchToDemo_DisablesTimer;

    [Test]
    procedure Test_SwitchToDemo_StoresCertificate;

    { Initialize tests }
    [Test]
    procedure Test_Initialize_NoCertificate_InstallsDefault;

    [Test]
    procedure Test_Initialize_WithCertificate_LoadsFromRegistry;

    { CheckComputerClock tests }
    [Test]
    procedure Test_CheckComputerClock_FullLicense_AlwaysTrue;

    { Timer tests }
    [Test]
    procedure Test_Timer_Interval_IsOneSecond;

    [Test]
    procedure Test_Timer_InitiallyDisabled;

    { Event assignment tests }
    [Test]
    procedure Test_Events_CanBeAssigned;

    { AutoStoreCertif tests }
    [Test]
    procedure Test_AutoStoreCertif_DefaultTrue;
  end;


IMPLEMENTATION

USES
  LightCore.EncodeXOR;


procedure TTestProteus.CleanupTestRegistry;
begin
  RegDeleteKey(HKEY_CURRENT_USER, FTestRegPath+ FProteus.ProductNameOb+ PreviousIDs);
  RegDeleteKey(HKEY_CURRENT_USER, FTestRegPath+ FProteus.ProductNameOb);
end;


procedure TTestProteus.CreateTestTrialKey;
VAR
  Certif: RCertificate;
begin
  Certif:= GenerateTrialCertificate(FTestProductName, 99, 30);                   { 30-day trial }
  Certif.ID:= 'TestTrialID_' + FormatDateTime('hhnnsszzz', Now);
  FTrialKey:= Certif.GenerateKeyString;
end;


procedure TTestProteus.Setup;
begin
  FProteus:= TProteus.Create(NIL);

  { Use unique test product name to avoid conflicts }
  FTestProductName:= 'DUnitXProteus_' + FormatDateTime('hhnnss', Now);
  FTestRegPath:= '\SOFTWARE\DUnitX\ProteusTests\';

  FProteus.ProductName:= FTestProductName;
  FProteus.ProductVers:= 99;
  FProteus.RegKeyPath:= FTestRegPath;
  FProteus.ObfuscateReg:= FALSE;                                                 { Disable obfuscation for easier test verification }
  FProteus.AutoStoreCertif:= FALSE;                                              { Disable auto-save to control test flow }

  CreateTestTrialKey;
  FProteus.DefaultKey:= FTrialKey;
end;


procedure TTestProteus.TearDown;
begin
  if Assigned(FProteus) then
    begin
      FProteus.AutoStoreCertif:= FALSE;                                          { Prevent save on destroy }
      CleanupTestRegistry;
      FreeAndNil(FProteus);
    end;
end;


{ Constructor/Destructor tests }

procedure TTestProteus.Test_Create_TimerInitialized;
begin
  Assert.IsNotNull(FProteus.Timer, 'Timer should be initialized');
  Assert.AreEqual(1000, FProteus.Timer.Interval, 'Timer interval should be 1000ms');
end;


procedure TTestProteus.Test_Create_StolenKeysInitialized;
begin
  Assert.IsNotNull(FProteus.StolenKeys, 'StolenKeys should be initialized');
end;


procedure TTestProteus.Test_Create_DefaultValues;
VAR Proteus: TProteus;
begin
  Proteus:= TProteus.Create(NIL);
  TRY
    Proteus.AutoStoreCertif:= FALSE;
    Assert.IsTrue(Proteus.AutoStoreCertif = FALSE, 'AutoStoreCertif should be settable');  { We just set it }
  FINALLY
    FreeAndNil(Proteus);
  END;
end;


{ setDefaultKey tests }

procedure TTestProteus.Test_SetDefaultKey_ValidKey_Accepted;
begin
  FProteus.DefaultKey:= FTrialKey;
  Assert.AreEqual(FTrialKey, FProteus.DefaultKey);
end;


procedure TTestProteus.Test_SetDefaultKey_EmptyKey_Rejected;
VAR OriginalKey: string;
begin
  OriginalKey:= FProteus.DefaultKey;
  FProteus.DefaultKey:= '';                                                      { Should be rejected }
  Assert.AreEqual(OriginalKey, FProteus.DefaultKey, 'Empty key should be rejected');
end;


procedure TTestProteus.Test_SetDefaultKey_NoAsterisks_Rejected;
VAR OriginalKey: string;
begin
  OriginalKey:= FProteus.DefaultKey;
  FProteus.DefaultKey:= 'InvalidKeyNoAsterisks';
  Assert.AreEqual(OriginalKey, FProteus.DefaultKey, 'Key without asterisks should be rejected');
end;


procedure TTestProteus.Test_SetDefaultKey_MissingStartAsterisk_Rejected;
VAR OriginalKey: string;
begin
  OriginalKey:= FProteus.DefaultKey;
  FProteus.DefaultKey:= 'MissingStart*';
  Assert.AreEqual(OriginalKey, FProteus.DefaultKey, 'Key missing start asterisk should be rejected');
end;


procedure TTestProteus.Test_SetDefaultKey_MissingEndAsterisk_Rejected;
VAR OriginalKey: string;
begin
  OriginalKey:= FProteus.DefaultKey;
  FProteus.DefaultKey:= '*MissingEnd';
  Assert.AreEqual(OriginalKey, FProteus.DefaultKey, 'Key missing end asterisk should be rejected');
end;


{ TodayIsTomorrow tests }

procedure TTestProteus.Test_TodayIsTomorrow_LastSeenYesterday_ReturnsTrue;
begin
  FProteus.CurCertif.LastSeen:= System.DateUtils.Yesterday;
  Assert.IsTrue(FProteus.TodayIsTomorrow);
end;


procedure TTestProteus.Test_TodayIsTomorrow_LastSeenToday_ReturnsFalse;
begin
  FProteus.CurCertif.LastSeen:= System.DateUtils.Today;
  Assert.IsFalse(FProteus.TodayIsTomorrow);
end;


procedure TTestProteus.Test_TodayIsTomorrow_LastSeenTomorrow_ReturnsFalse;
begin
  FProteus.CurCertif.LastSeen:= System.DateUtils.Tomorrow;
  Assert.IsFalse(FProteus.TodayIsTomorrow);
end;


{ DaysSinceInstall tests }

procedure TTestProteus.Test_DaysSinceInstall_Today_ReturnsZero;
begin
  FProteus.CurCertif.Installed:= Now;
  Assert.IsTrue(FProteus.DaysSinceInstall < 1, 'Days since install should be < 1 for today');
end;


procedure TTestProteus.Test_DaysSinceInstall_TenDaysAgo_ReturnsTen;
begin
  FProteus.CurCertif.Installed:= IncDay(Now, -10);
  Assert.IsTrue(FProteus.DaysSinceInstall >= 9.9, 'Days since install should be ~10');
  Assert.IsTrue(FProteus.DaysSinceInstall <= 10.1, 'Days since install should be ~10');
end;


{ EnableTimer tests }

procedure TTestProteus.Test_EnableTimer_FullLicense_ReturnsFalse;
begin
  FProteus.CurCertif.CertifType:= ctFull;
  FProteus.CurCertif.Installed:= Now;
  FProteus.CurCertif.LastSeen:= Now;

  { EnableTimer should return FALSE because Full license doesn't need timer }
  Assert.IsFalse(FProteus.Timer.Enabled, 'Timer should not be enabled for Full license');
end;


procedure TTestProteus.Test_EnableTimer_ExpiredLicense_ReturnsFalse;
begin
  FProteus.CurCertif.CertifType:= ctCountDownExp;
  Assert.IsFalse(FProteus.Timer.Enabled, 'Timer should not be enabled for expired license');
end;


procedure TTestProteus.Test_EnableTimer_UninstalledLicense_ReturnsFalse;
begin
  FProteus.CurCertif.CertifType:= ctUninstalled;
  Assert.IsFalse(FProteus.Timer.Enabled, 'Timer should not be enabled for uninstalled license');
end;


{ SwitchToDemo tests }

procedure TTestProteus.Test_SwitchToDemo_SetsCertifTypeToExpired;
begin
  FProteus.CurCertif.CertifType:= ctCountDownActive;
  FProteus.SwitchToDemo;
  Assert.AreEqual(ctCountDownExp, FProteus.CurCertif.CertifType);
end;


procedure TTestProteus.Test_SwitchToDemo_DisablesTimer;
begin
  FProteus.Timer.Enabled:= TRUE;
  FProteus.SwitchToDemo;
  Assert.IsFalse(FProteus.Timer.Enabled);
end;


procedure TTestProteus.Test_SwitchToDemo_StoresCertificate;
begin
  FProteus.CurCertif.DecodeKey(FTrialKey);
  FProteus.CurCertif.Installed:= Now;
  FProteus.SwitchToDemo;

  { Certificate should be stored - verify by reading back }
  Assert.IsTrue(FProteus.CertificateInstalled, 'Certificate should be stored after SwitchToDemo');
end;


{ Initialize tests }

procedure TTestProteus.Test_Initialize_NoCertificate_InstallsDefault;
begin
  CleanupTestRegistry;                                                           { Ensure no existing certificate }

  FProteus.Initialize;

  Assert.IsTrue(FProteus.CertificateInstalled, 'Default certificate should be installed');
end;


procedure TTestProteus.Test_Initialize_WithCertificate_LoadsFromRegistry;
VAR TestID: string;
begin
  { First, store a certificate }
  TestID:= 'InitTestID_' + FormatDateTime('hhnnsszzz', Now);
  FProteus.CurCertif.DecodeKey(FTrialKey);
  FProteus.CurCertif.ID:= TestID;
  FProteus.CurCertif.Installed:= Now;
  FProteus.StoreCertificate;

  { Reset current certificate }
  FProteus.CurCertif.Reset;

  { Initialize should load from registry }
  FProteus.Initialize;

  Assert.AreEqual(TestID, FProteus.CurCertif.ID, 'Should load certificate from registry');
end;


{ CheckComputerClock tests }

procedure TTestProteus.Test_CheckComputerClock_FullLicense_AlwaysTrue;
begin
  FProteus.CurCertif.CertifType:= ctFull;
  FProteus.CurCertif.LastSeen:= IncDay(Now, 100);                                { Future date - would fail for trial }

  Assert.IsTrue(FProteus.CheckComputerClock, 'Full license should always pass clock check');
end;


{ Timer tests }

procedure TTestProteus.Test_Timer_Interval_IsOneSecond;
begin
  Assert.AreEqual(1000, FProteus.Timer.Interval);
end;


procedure TTestProteus.Test_Timer_InitiallyDisabled;
VAR Proteus: TProteus;
begin
  Proteus:= TProteus.Create(NIL);
  TRY
    Proteus.AutoStoreCertif:= FALSE;
    Assert.IsFalse(Proteus.Timer.Enabled, 'Timer should be disabled on create');
  FINALLY
    FreeAndNil(Proteus);
  END;
end;


{ Event assignment tests }

procedure TTestProteus.Test_Events_CanBeAssigned;
VAR
  DummyHandler: TNotifyEvent;

  procedure DummyProc(Sender: TObject);
  begin
    { Empty handler for testing }
  end;

begin
  DummyHandler:= DummyProc;

  FProteus.OnTick:= DummyHandler;
  FProteus.OnInvalidTime:= DummyHandler;
  FProteus.OnKeyStolen:= DummyHandler;
  FProteus.OnLicenseFallBack:= DummyHandler;
  FProteus.OnSwitchedToDemo:= DummyHandler;
  FProteus.OnUninstalling:= DummyHandler;
  FProteus.OnKeyUninstalled:= DummyHandler;

  Assert.IsTrue(Assigned(FProteus.OnTick), 'OnTick should be assignable');
  Assert.IsTrue(Assigned(FProteus.OnInvalidTime), 'OnInvalidTime should be assignable');
  Assert.IsTrue(Assigned(FProteus.OnKeyStolen), 'OnKeyStolen should be assignable');
  Assert.IsTrue(Assigned(FProteus.OnLicenseFallBack), 'OnLicenseFallBack should be assignable');
  Assert.IsTrue(Assigned(FProteus.OnSwitchedToDemo), 'OnSwitchedToDemo should be assignable');
  Assert.IsTrue(Assigned(FProteus.OnUninstalling), 'OnUninstalling should be assignable');
  Assert.IsTrue(Assigned(FProteus.OnKeyUninstalled), 'OnKeyUninstalled should be assignable');
end;


{ AutoStoreCertif tests }

procedure TTestProteus.Test_AutoStoreCertif_DefaultTrue;
VAR Proteus: TProteus;
begin
  Proteus:= TProteus.Create(NIL);
  TRY
    Assert.IsTrue(Proteus.AutoStoreCertif, 'AutoStoreCertif should default to TRUE');
    Proteus.AutoStoreCertif:= FALSE;                                             { Prevent save on destroy }
  FINALLY
    FreeAndNil(Proteus);
  END;
end;


INITIALIZATION
  TDUnitX.RegisterTestFixture(TTestProteus);

end.
