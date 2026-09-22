unit Test.LightCore.Win.SystemPermissions;

{=============================================================================================================
   Unit tests for LightCore.Win.SystemPermissions.pas
   Tests system permission and privilege utility functions.

   IMPORTANT: Only read-only/query functions are tested.
   Functions that would actually modify privileges are tested minimally for safety.

   The whole fixture is compiled only when MSWINDOWS is defined, because LightCore.Win.SystemPermissions is Windows-only.
   The tests of AppHasAdminRights, IsUserAdmin and CurrentUserHasAdminRights are in Test.LightCore.SystemPermissions.pas.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

{$IFDEF MSWINDOWS}
uses
  DUnitX.TestFramework,
  System.SysUtils;

type
  [TestFixture]
  TTestWinSystemPermissions = class
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

    { SetPrivilege Tests - Minimal testing, don't actually change privileges }
    [Test]
    procedure TestSetPrivilege_EmptyName_RaisesException;

    [Test]
    procedure TestSetPrivilege_InvalidPrivilege_ReturnsFalse;
  end;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
uses
  LightCore.Win.SystemPermissions;


procedure TTestWinSystemPermissions.Setup;
begin
  // No setup needed for read-only tests
end;


procedure TTestWinSystemPermissions.TearDown;
begin
  // No cleanup needed
end;


{ AppElevationLevel Tests }

procedure TTestWinSystemPermissions.TestAppElevationLevel_ReturnsValidValue;
var
  Level: Integer;
begin
  Level:= AppElevationLevel;
  // Should return -1 (error) or 1-3 (valid elevation types)
  Assert.IsTrue((Level = -1) OR ((Level >= 1) AND (Level <= 3)),
    'AppElevationLevel should return -1 (error) or 1-3 (valid elevation type)');
end;


procedure TTestWinSystemPermissions.TestAppElevationLevel_InExpectedRange;
var
  Level: Integer;
begin
  Level:= AppElevationLevel;
  // On modern Windows with UAC, should typically return 1, 2, or 3
  // -1 indicates an error (e.g., insufficient permissions to query)
  Assert.IsTrue(Level >= -1, 'AppElevationLevel should not return values below -1');
  Assert.IsTrue(Level <= 3, 'AppElevationLevel should not return values above 3');
end;


{ SetPrivilege Tests }

procedure TTestWinSystemPermissions.TestSetPrivilege_EmptyName_RaisesException;
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


procedure TTestWinSystemPermissions.TestSetPrivilege_InvalidPrivilege_ReturnsFalse;
var
  Result: Boolean;
begin
  // An invalid privilege name should return FALSE (not raise exception)
  Result:= SetPrivilege('NonExistentPrivilege12345', TRUE);
  Assert.IsFalse(Result, 'SetPrivilege should return FALSE for invalid privilege names');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestWinSystemPermissions);
{$ENDIF}

end.
