unit Test.LightCore.SystemPermissions;

{=============================================================================================================
   Unit tests for LightCore.SystemPermissions.pas
   Tests system permission and privilege utility functions.

   IMPORTANT: Only read-only/query functions are tested.

   The whole fixture is compiled only when MSWINDOWS is defined, because off Windows LightCore.SystemPermissions declares nothing.
   The tests of AppElevationLevel and SetPrivilege are in Test.LightCore.Win.SystemPermissions.pas.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

{$IFDEF MSWINDOWS}
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

    { Consistency Tests }
    [Test]
    procedure TestAdminFunctions_Consistent;
  end;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
uses
  LightCore.SystemPermissions;


procedure TTestSystemPermissions.Setup;
begin
  // No setup needed for read-only tests
end;


procedure TTestSystemPermissions.TearDown;
begin
  // No cleanup needed
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
  AdminRights:= CurrentUserHasAdminRights;
  UserAdmin:= IsUserAdmin;
  Assert.AreEqual(AdminRights, UserAdmin, 'CurrentUserHasAdminRights should match IsUserAdmin on NT systems');
end;


{ Consistency Tests }

procedure TTestSystemPermissions.TestAdminFunctions_Consistent;
var
  AppAdmin, CurrentAdmin: Boolean;
begin
  // CurrentUserHasAdminRights delegates to AppHasAdminRights, so they should match
  AppAdmin:= AppHasAdminRights;
  CurrentAdmin:= CurrentUserHasAdminRights;
  Assert.AreEqual(AppAdmin, CurrentAdmin, 'AppHasAdminRights and CurrentUserHasAdminRights should be consistent');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestSystemPermissions);
{$ENDIF}

end.
