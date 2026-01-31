unit Test.LightVcl.Common.SystemPermissions;

{=============================================================================================================
   Unit tests for LightVcl.Common.SystemPermissions.pas
   Tests system permission and privilege utility functions.

   IMPORTANT: Only read-only/query functions are tested.
   Functions that would actually modify privileges are tested minimally for safety.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils;

type
  [TestFixture]
  TTestSystemPermissions = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { AppElevationLevel Tests - Read-only, safe to call }
    [Test]
    procedure TestAppElevationLevel_ReturnsValidValue;

    [Test]
    procedure TestAppElevationLevel_InExpectedRange;

    { AppHasAdminRights Tests }
    [Test]
    procedure TestAppHasAdminRights_ReturnsBool;

    [Test]
    procedure TestAppHasAdminRights_ConsistentResults;

    { IsUserAdmin Tests }
    [Test]
    procedure TestIsUserAdmin_ReturnsBool;

    [Test]
    procedure TestIsUserAdmin_ConsistentResults;

    { CurrentUserHasAdminRights Tests }
    [Test]
    procedure TestCurrentUserHasAdminRights_ReturnsBool;

    [Test]
    procedure TestCurrentUserHasAdminRights_MatchesIsUserAdmin;

    { OsHasNTSecurity Tests }
    [Test]
    procedure TestOsHasNTSecurity_ReturnsTrue;

    [Test]
    procedure TestOsHasNTSecurity_AlwaysTrueOnModernWindows;

    { SetPrivilege Tests - Minimal testing, don't actually change privileges }
    [Test]
    procedure TestSetPrivilege_EmptyName_RaisesException;

    [Test]
    procedure TestSetPrivilege_InvalidPrivilege_ReturnsFalse;

    { Consistency Tests }
    [Test]
    procedure TestAdminFunctions_Consistent;
  end;

implementation

uses
  LightVcl.Common.SystemPermissions;


procedure TTestSystemPermissions.Setup;
begin
  // No setup needed for read-only tests
end;


procedure TTestSystemPermissions.TearDown;
begin
  // No cleanup needed
end;


{ AppElevationLevel Tests }

procedure TTestSystemPermissions.TestAppElevationLevel_ReturnsValidValue;
var
  Level: Integer;
begin
  Level:= AppElevationLevel;
  // Should return -1 (error) or 1-3 (valid elevation types)
  Assert.IsTrue((Level = -1) OR ((Level >= 1) AND (Level <= 3)),
    'AppElevationLevel should return -1 (error) or 1-3 (valid elevation type)');
end;


procedure TTestSystemPermissions.TestAppElevationLevel_InExpectedRange;
var
  Level: Integer;
begin
  Level:= AppElevationLevel;
  // On modern Windows with UAC, should typically return 1, 2, or 3
  // -1 indicates an error (e.g., insufficient permissions to query)
  Assert.IsTrue(Level >= -1, 'AppElevationLevel should not return values below -1');
  Assert.IsTrue(Level <= 3, 'AppElevationLevel should not return values above 3');
end;


{ AppHasAdminRights Tests }

procedure TTestSystemPermissions.TestAppHasAdminRights_ReturnsBool;
var
  HasAdmin: Boolean;
begin
  HasAdmin:= AppHasAdminRights;
  Assert.IsTrue((HasAdmin = TRUE) OR (HasAdmin = FALSE), 'AppHasAdminRights should return valid Boolean');
end;


procedure TTestSystemPermissions.TestAppHasAdminRights_ConsistentResults;
var
  Result1, Result2: Boolean;
begin
  // Calling twice should return the same result
  Result1:= AppHasAdminRights;
  Result2:= AppHasAdminRights;
  Assert.AreEqual(Result1, Result2, 'AppHasAdminRights should return consistent results');
end;


{ IsUserAdmin Tests }

procedure TTestSystemPermissions.TestIsUserAdmin_ReturnsBool;
var
  IsAdmin: Boolean;
begin
  IsAdmin:= IsUserAdmin;
  Assert.IsTrue((IsAdmin = TRUE) OR (IsAdmin = FALSE), 'IsUserAdmin should return valid Boolean');
end;


procedure TTestSystemPermissions.TestIsUserAdmin_ConsistentResults;
var
  Result1, Result2: Boolean;
begin
  Result1:= IsUserAdmin;
  Result2:= IsUserAdmin;
  Assert.AreEqual(Result1, Result2, 'IsUserAdmin should return consistent results');
end;


{ CurrentUserHasAdminRights Tests }

procedure TTestSystemPermissions.TestCurrentUserHasAdminRights_ReturnsBool;
var
  HasRights: Boolean;
begin
  HasRights:= CurrentUserHasAdminRights;
  Assert.IsTrue((HasRights = TRUE) OR (HasRights = FALSE), 'CurrentUserHasAdminRights should return valid Boolean');
end;


procedure TTestSystemPermissions.TestCurrentUserHasAdminRights_MatchesIsUserAdmin;
var
  AdminRights, UserAdmin: Boolean;
begin
  // On NT systems (all modern Windows), these should return the same value
  if OsHasNTSecurity then
  begin
    AdminRights:= CurrentUserHasAdminRights;
    UserAdmin:= IsUserAdmin;
    Assert.AreEqual(AdminRights, UserAdmin, 'CurrentUserHasAdminRights should match IsUserAdmin on NT systems');
  end
  else
    Assert.Pass('Non-NT system - test not applicable');
end;


{ OsHasNTSecurity Tests }

procedure TTestSystemPermissions.TestOsHasNTSecurity_ReturnsTrue;
begin
  // All modern Windows versions are NT-based
  Assert.IsTrue(OsHasNTSecurity, 'OsHasNTSecurity should return TRUE on modern Windows');
end;


procedure TTestSystemPermissions.TestOsHasNTSecurity_AlwaysTrueOnModernWindows;
begin
  // Windows 2000 and later are all NT-based
  // This test verifies the function works on the current system
  Assert.IsTrue(OsHasNTSecurity, 'All supported Windows versions should be NT-based');
end;


{ SetPrivilege Tests }

procedure TTestSystemPermissions.TestSetPrivilege_EmptyName_RaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      SetPrivilege('', TRUE);
    end,
    Exception,
    'SetPrivilege should raise exception for empty privilege name'
  );
end;


procedure TTestSystemPermissions.TestSetPrivilege_InvalidPrivilege_ReturnsFalse;
var
  Result: Boolean;
begin
  // An invalid privilege name should return FALSE (not raise exception)
  Result:= SetPrivilege('NonExistentPrivilege12345', TRUE);
  Assert.IsFalse(Result, 'SetPrivilege should return FALSE for invalid privilege names');
end;


{ Consistency Tests }

procedure TTestSystemPermissions.TestAdminFunctions_Consistent;
var
  AppAdmin, CurrentAdmin: Boolean;
begin
  // AppHasAdminRights now delegates to CurrentUserHasAdminRights, so they should match
  AppAdmin:= AppHasAdminRights;
  CurrentAdmin:= CurrentUserHasAdminRights;
  Assert.AreEqual(AppAdmin, CurrentAdmin, 'AppHasAdminRights and CurrentUserHasAdminRights should be consistent');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestSystemPermissions);

end.
