UNIT Test.cpProteusIO;

{-------------------------------------------------------------------------------------------------------------
   DUnitX tests for cpProteusIO.pas
   Tests the base I/O class for Proteus licensing system.

   GabrielMoraru.com
   2026.01

   Note: These tests use a unique test product name to avoid conflicts with real license data.
   Registry keys created during tests are cleaned up in TearDown.
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  Winapi.Windows,
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,
  cpProteusIO, cpCertificate, cpProteusUtils;

TYPE
  [TestFixture]
  TTestProteusIO = class
  private
    FProteus: TProteusIO;
    FTestProductName: string;
    FTestRegPath: string;
    procedure CleanupTestRegistry;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor tests }
    [Test]
    procedure Test_Create_DefaultValues;

    [Test]
    procedure Test_Create_CurCertifInitialized;

    { ProductNameOb tests }
    [Test]
    procedure Test_ProductNameOb_WithObfuscation;

    [Test]
    procedure Test_ProductNameOb_WithoutObfuscation;

    [Test]
    procedure Test_ProductNameOb_IncludesVersion;

    { RegKeyPath tests }
    [Test]
    procedure Test_RegKeyPath_AddsBackslashes;

    [Test]
    procedure Test_RegKeyPath_KeepsExistingBackslashes;

    { CertifGetID tests }
    [Test]
    procedure Test_CertifGetID_EmptyString_ReturnsQuestionMarks;

    [Test]
    procedure Test_CertifGetID_InvalidKey_ReturnsQuestionMarks;

    [Test]
    procedure Test_CertifGetID_ValidKey_ReturnsID;

    { KeyIsStollen tests }
    [Test]
    procedure Test_KeyIsStollen_InvalidKey_ReturnsFalse;

    [Test]
    procedure Test_KeyIsStollen_NotInList_ReturnsFalse;

    { CertificateInstalled tests }
    [Test]
    procedure Test_CertificateInstalled_NoCertificate_ReturnsFalse;

    [Test]
    procedure Test_CertificateInstalled_AfterStore_ReturnsTrue;

    { IdAlreadyUsed tests }
    [Test]
    procedure Test_IdAlreadyUsed_NoIDs_ReturnsFalse;

    [Test]
    procedure Test_IdAlreadyUsed_AfterStore_ReturnsTrue;

    { EnumeratePreviousIDs tests }
    [Test]
    procedure Test_EnumeratePreviousIDs_Empty_ReturnsEmptyList;

    [Test]
    procedure Test_EnumeratePreviousIDs_ReturnsValidList;

    { StoreCertificate and ReadKey tests }
    [Test]
    procedure Test_StoreCertificate_ThenReadKey_RoundTrip;

    [Test]
    procedure Test_StoreCertificate_UpdatesLastSeen;
  end;


IMPLEMENTATION

USES
  System.DateUtils,
  LightCore.EncodeXOR;


{ Helper to clean up test registry keys }
procedure TTestProteusIO.CleanupTestRegistry;
begin
  RegDeleteKey(HKEY_CURRENT_USER, FTestRegPath+ FProteus.ProductNameOb+ PreviousIDs);
  RegDeleteKey(HKEY_CURRENT_USER, FTestRegPath+ FProteus.ProductNameOb);
end;


procedure TTestProteusIO.Setup;
begin
  FProteus:= TProteusIO.Create(NIL);
  FProteus.StolenKeys:= TStringList.Create;                                      { Normally initialized in TProteus.Create }

  { Use unique test product name to avoid conflicts }
  FTestProductName:= 'DUnitXTestProduct_' + FormatDateTime('hhnnss', Now);
  FTestRegPath:= '\SOFTWARE\DUnitX\ProteusTests\';

  FProteus.ProductName:= FTestProductName;
  FProteus.ProductVers:= 99;
  FProteus.RegKeyPath:= FTestRegPath;
  FProteus.ObfuscateReg:= FALSE;                                                 { Disable obfuscation for easier test verification }
end;


procedure TTestProteusIO.TearDown;
begin
  CleanupTestRegistry;
  FreeAndNil(FProteus.StolenKeys);
  FreeAndNil(FProteus);
end;


{ Constructor tests }

procedure TTestProteusIO.Test_Create_DefaultValues;
VAR Proteus: TProteusIO;
begin
  Proteus:= TProteusIO.Create(NIL);
  TRY
    Assert.AreEqual('My product', Proteus.ProductName);
    Assert.AreEqual(Byte(0), Proteus.ProductVers);
    Assert.AreEqual(DefaultRegLoc, Proteus.RegKeyPath);
    Assert.IsTrue(Proteus.ObfuscateReg);
    Assert.IsFalse(Proteus.VerboseLogActive);
  FINALLY
    FreeAndNil(Proteus);
  END;
end;


procedure TTestProteusIO.Test_Create_CurCertifInitialized;
VAR Proteus: TProteusIO;
begin
  Proteus:= TProteusIO.Create(NIL);
  TRY
    Assert.AreEqual('My product', Proteus.CurCertif.ProductName);
    Assert.AreEqual(Byte(0), Proteus.CurCertif.ProductVerMaj);
  FINALLY
    FreeAndNil(Proteus);
  END;
end;


{ ProductNameOb tests }

procedure TTestProteusIO.Test_ProductNameOb_WithObfuscation;
VAR
  Plain, Obfuscated: string;
begin
  FProteus.ObfuscateReg:= TRUE;
  FProteus.ProductName:= 'TestApp';
  FProteus.ProductVers:= 5;

  Plain:= 'TestApp5';
  Obfuscated:= FProteus.ProductNameOb;

  Assert.AreNotEqual(Plain, Obfuscated, 'Obfuscated name should differ from plain');
  Assert.AreEqual(Plain, SimpleDecode(Obfuscated), 'Should decode back to original');
end;


procedure TTestProteusIO.Test_ProductNameOb_WithoutObfuscation;
begin
  FProteus.ObfuscateReg:= FALSE;
  FProteus.ProductName:= 'TestApp';
  FProteus.ProductVers:= 5;

  Assert.AreEqual('TestApp5', FProteus.ProductNameOb);
end;


procedure TTestProteusIO.Test_ProductNameOb_IncludesVersion;
begin
  FProteus.ObfuscateReg:= FALSE;
  FProteus.ProductName:= 'MyApp';
  FProteus.ProductVers:= 123;

  Assert.AreEqual('MyApp123', FProteus.ProductNameOb);
end;


{ RegKeyPath tests }

procedure TTestProteusIO.Test_RegKeyPath_AddsBackslashes;
begin
  FProteus.RegKeyPath:= 'SOFTWARE\Test';
  Assert.AreEqual('\SOFTWARE\Test\', FProteus.RegKeyPath);
end;


procedure TTestProteusIO.Test_RegKeyPath_KeepsExistingBackslashes;
begin
  FProteus.RegKeyPath:= '\SOFTWARE\Test\';
  Assert.AreEqual('\SOFTWARE\Test\', FProteus.RegKeyPath);
end;


{ CertifGetID tests }

procedure TTestProteusIO.Test_CertifGetID_EmptyString_ReturnsQuestionMarks;
begin
  Assert.AreEqual('??????', FProteus.CertifGetID(''));
end;


procedure TTestProteusIO.Test_CertifGetID_InvalidKey_ReturnsQuestionMarks;
begin
  Assert.AreEqual('??????', FProteus.CertifGetID('InvalidKeyString'));
  Assert.AreEqual('??????', FProteus.CertifGetID('*P083TooShort*'));
end;


procedure TTestProteusIO.Test_CertifGetID_ValidKey_ReturnsID;
VAR
  Certif: RCertificate;
  KeyString: string;
begin
  Certif:= GenerateTrialCertificate(FTestProductName, 1, 30);
  Certif.ID:= 'UniqueTestID123';
  KeyString:= Certif.GenerateKeyString;

  Assert.AreEqual('UniqueTestID123', FProteus.CertifGetID(KeyString));
end;


{ KeyIsStollen tests }

procedure TTestProteusIO.Test_KeyIsStollen_InvalidKey_ReturnsFalse;
begin
  Assert.IsFalse(FProteus.KeyIsStollen('InvalidKey', FProteus.StolenKeys));
end;


procedure TTestProteusIO.Test_KeyIsStollen_NotInList_ReturnsFalse;
VAR
  Certif: RCertificate;
  KeyString: string;
begin
  Certif:= GenerateTrialCertificate(FTestProductName, 1, 30);
  KeyString:= Certif.GenerateKeyString;

  FProteus.StolenKeys.Clear;
  Assert.IsFalse(FProteus.KeyIsStollen(KeyString, FProteus.StolenKeys));
end;


{ CertificateInstalled tests }

procedure TTestProteusIO.Test_CertificateInstalled_NoCertificate_ReturnsFalse;
begin
  CleanupTestRegistry;
  Assert.IsFalse(FProteus.CertificateInstalled);
end;


procedure TTestProteusIO.Test_CertificateInstalled_AfterStore_ReturnsTrue;
begin
  FProteus.CurCertif:= GenerateTrialCertificate(FTestProductName, 99, 30);
  FProteus.CurCertif.Installed:= Now;
  FProteus.StoreCertificate;

  Assert.IsTrue(FProteus.CertificateInstalled);
end;


{ IdAlreadyUsed tests }

procedure TTestProteusIO.Test_IdAlreadyUsed_NoIDs_ReturnsFalse;
begin
  CleanupTestRegistry;
  Assert.IsFalse(FProteus.IdAlreadyUsed('SomeRandomID'));
end;


procedure TTestProteusIO.Test_IdAlreadyUsed_AfterStore_ReturnsTrue;
VAR TestID: string;
begin
  TestID:= 'TestCertificateID_' + FormatDateTime('hhnnsszzz', Now);

  FProteus.CurCertif:= GenerateTrialCertificate(FTestProductName, 99, 30);
  FProteus.CurCertif.ID:= TestID;
  FProteus.CurCertif.Installed:= Now;
  FProteus.StoreCertificate;

  Assert.IsTrue(FProteus.IdAlreadyUsed(TestID));
end;


{ EnumeratePreviousIDs tests }

procedure TTestProteusIO.Test_EnumeratePreviousIDs_Empty_ReturnsEmptyList;
VAR IDs: TStringList;
begin
  CleanupTestRegistry;

  IDs:= FProteus.EnumeratePreviousIDs;
  TRY
    Assert.AreEqual(0, IDs.Count);
  FINALLY
    FreeAndNil(IDs);
  END;
end;


procedure TTestProteusIO.Test_EnumeratePreviousIDs_ReturnsValidList;
VAR
  IDs: TStringList;
  TestID: string;
begin
  TestID:= 'EnumTestID_' + FormatDateTime('hhnnsszzz', Now);

  FProteus.CurCertif:= GenerateTrialCertificate(FTestProductName, 99, 30);
  FProteus.CurCertif.ID:= TestID;
  FProteus.CurCertif.Installed:= Now;
  FProteus.StoreCertificate;

  IDs:= FProteus.EnumeratePreviousIDs;
  TRY
    Assert.IsTrue(IDs.Count >= 1, 'Should have at least one ID');
  FINALLY
    FreeAndNil(IDs);
  END;
end;


{ StoreCertificate and ReadKey tests }

procedure TTestProteusIO.Test_StoreCertificate_ThenReadKey_RoundTrip;
VAR
  OriginalCertif: RCertificate;
  ReadCertif: RCertificate;
  StoredKey: string;
begin
  { Create and store a certificate }
  OriginalCertif:= GenerateTrialCertificate(FTestProductName, 99, 30);
  OriginalCertif.ID:= 'RoundTripTestID';
  OriginalCertif.UserName:= 'TestUser';
  OriginalCertif.Installed:= Now;

  FProteus.CurCertif:= OriginalCertif;
  FProteus.StoreCertificate;

  { Read back }
  StoredKey:= FProteus.ReadKey;
  Assert.IsNotEmpty(StoredKey, 'ReadKey should return stored key');

  { Decode and verify }
  Assert.IsTrue(ReadCertif.DecodeKey(StoredKey), 'Should decode successfully');
  Assert.AreEqual('RoundTripTestID', ReadCertif.ID);
  Assert.AreEqual('TestUser', ReadCertif.UserName);
  Assert.AreEqual(FTestProductName, ReadCertif.ProductName);
end;


procedure TTestProteusIO.Test_StoreCertificate_UpdatesLastSeen;
VAR
  BeforeStore: TDateTime;
begin
  FProteus.CurCertif:= GenerateTrialCertificate(FTestProductName, 99, 30);
  FProteus.CurCertif.Installed:= Now;
  FProteus.CurCertif.LastSeen:= EncodeDate(2000, 1, 1);                          { Old date }

  BeforeStore:= Now;
  FProteus.StoreCertificate;

  { LastSeen should be updated to approximately Now }
  Assert.IsTrue(FProteus.CurCertif.LastSeen >= BeforeStore, 'LastSeen should be updated');
  Assert.IsTrue(FProteus.CurCertif.LastSeen <= IncSecond(BeforeStore, 2), 'LastSeen should be recent');
end;


INITIALIZATION
  TDUnitX.RegisterTestFixture(TTestProteusIO);

end.
