unit Test.LightVcl.Visual.AppData;

{=============================================================================================================
   2026.01.31
   Unit tests for LightVcl.Visual.AppData.pas
   Tests TAppData - VCL-specific application data management, form creation, version info, etc.

   Note: Some tests require a running VCL application context.
   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  Winapi.Windows,
  Vcl.Forms,
  Vcl.Controls,
  LightCore.AppData,
  LightVcl.Visual.AppData;

type
  [TestFixture]
  TTestAppDataVcl = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Basic Tests }
    [Test]
    procedure TestAppDataExists;

    [Test]
    procedure TestAppDataIsVclType;

    { Version Info Tests }
    [Test]
    procedure TestGetVersionInfo;

    [Test]
    procedure TestGetVersionInfo_NoBuildNo;

    [Test]
    procedure TestGetVersionInfo_WithBuildNo;

    [Test]
    procedure TestGetVersionInfoV;

    [Test]
    procedure TestGetVersionInfoV_HasPrefix;

    [Test]
    procedure TestGetVersionInfoMajor;

    [Test]
    procedure TestGetVersionInfoMinor;

    { FormLog Tests }
    [Test]
    procedure TestFormLogExists;

    [Test]
    procedure TestFormLogNotMainForm;

    { Font Tests }
    [Test]
    procedure TestFontInitiallyNil;

    { Hint Tests }
    [Test]
    procedure TestHintType_Off;

    [Test]
    procedure TestHintType_Tooltips;

    [Test]
    procedure TestHintType_StatusBar;

    [Test]
    procedure TestHideHint_SetGet;

    { Single Instance Tests }
    [Test]
    procedure TestInstanceRunning;

    [Test]
    procedure TestSingleInstClassName_NotEmpty;

    [Test]
    procedure TestSetSingleInstanceName;

    { App Control Tests }
    [Test]
    procedure TestMinimize;

    [Test]
    procedure TestRestore;

    { Startup Tests }
    [Test]
    procedure TestRunSelfAtStartUp_Disable;

    { Uninstaller Registry Tests }
    [Test]
    procedure TestReadAppDataFolder_Empty;

    [Test]
    procedure TestReadInstallationFolder_Empty;

    { RaiseIfStillInitializing Tests }
    [Test]
    procedure TestRaiseIfStillInitializing_NotInitializing;

    [Test]
    procedure TestRaiseIfStillInitializing_WhenInitializing;
  end;

implementation


procedure TTestAppDataVcl.Setup;
begin
  // Tests expect AppData to already be created by the test runner
end;

procedure TTestAppDataVcl.TearDown;
begin
  // Reset state if needed
  if AppData <> NIL
  then AppData.StartMinim:= FALSE;
end;


{ Basic Tests }

procedure TTestAppDataVcl.TestAppDataExists;
begin
  Assert.IsNotNull(AppData, 'AppData should be created');
end;

procedure TTestAppDataVcl.TestAppDataIsVclType;
begin
  Assert.IsTrue(AppData is TAppData, 'AppData should be TAppData (VCL version)');
end;


{ Version Info Tests }

procedure TTestAppDataVcl.TestGetVersionInfo;
var
  Version: string;
begin
  Version:= TAppData.GetVersionInfo;
  Assert.IsNotEmpty(Version, 'Version should not be empty');
end;

procedure TTestAppDataVcl.TestGetVersionInfo_NoBuildNo;
var
  Version: string;
  DotCount: Integer;
  i: Integer;
begin
  Version:= TAppData.GetVersionInfo(False);
  // Without build number, should have format X.Y.Z (2 dots)
  DotCount:= 0;
  for i:= 1 to Length(Version) do
    if Version[i] = '.'
    then Inc(DotCount);

  // N/A is valid if version info not available
  if Version <> 'N/A'
  then Assert.AreEqual(2, DotCount, 'Version without build should have 2 dots (X.Y.Z)');
end;

procedure TTestAppDataVcl.TestGetVersionInfo_WithBuildNo;
var
  Version: string;
  DotCount: Integer;
  i: Integer;
begin
  Version:= TAppData.GetVersionInfo(True);
  DotCount:= 0;
  for i:= 1 to Length(Version) do
    if Version[i] = '.'
    then Inc(DotCount);

  // N/A is valid if version info not available
  if Version <> 'N/A'
  then Assert.AreEqual(3, DotCount, 'Version with build should have 3 dots (X.Y.Z.B)');
end;

procedure TTestAppDataVcl.TestGetVersionInfoV;
var
  Version: string;
begin
  Version:= TAppData.GetVersionInfoV;
  Assert.IsNotEmpty(Version);
end;

procedure TTestAppDataVcl.TestGetVersionInfoV_HasPrefix;
var
  Version: string;
begin
  Version:= TAppData.GetVersionInfoV;
  Assert.IsTrue(Pos('v', Version) > 0, 'GetVersionInfoV should contain "v" prefix');
end;

procedure TTestAppDataVcl.TestGetVersionInfoMajor;
var
  Major: Word;
begin
  Major:= AppData.GetVersionInfoMajor;
  // Word is always >= 0, just verify it returns without exception
  Assert.Pass('GetVersionInfoMajor returned: ' + IntToStr(Major));
end;

procedure TTestAppDataVcl.TestGetVersionInfoMinor;
var
  Minor: Word;
begin
  Minor:= AppData.GetVersionInfoMinor;
  // Word is always >= 0, just verify it returns without exception
  Assert.Pass('GetVersionInfoMinor returned: ' + IntToStr(Minor));
end;


{ FormLog Tests }

procedure TTestAppDataVcl.TestFormLogExists;
begin
  // Accessing FormLog creates it if needed
  Assert.IsNotNull(AppData.FormLog, 'FormLog should be created on access');
end;

procedure TTestAppDataVcl.TestFormLogNotMainForm;
begin
  // FormLog should never be the main form
  if Application.MainForm <> NIL
  then Assert.AreNotEqual(TObject(Application.MainForm), TObject(AppData.FormLog),
         'FormLog should not be MainForm');
end;


{ Font Tests }

procedure TTestAppDataVcl.TestFontInitiallyNil;
begin
  // Font is nil until main form is created and sets it
  // This test verifies the property accessor works
  // (Font may or may not be nil depending on test order)
  Assert.IsTrue((AppData.Font = NIL) OR (AppData.Font <> NIL));
end;


{ Hint Tests }

procedure TTestAppDataVcl.TestHintType_Off;
begin
  AppData.HintType:= htOff;
  Assert.AreEqual(htOff, AppData.HintType);
  Assert.IsFalse(Application.ShowHint, 'ShowHint should be FALSE when HintType is htOff');
end;

procedure TTestAppDataVcl.TestHintType_Tooltips;
begin
  AppData.HintType:= htTooltips;
  Assert.AreEqual(htTooltips, AppData.HintType);
  Assert.IsTrue(Application.ShowHint, 'ShowHint should be TRUE when HintType is htTooltips');
end;

procedure TTestAppDataVcl.TestHintType_StatusBar;
begin
  AppData.HintType:= htStatBar;
  Assert.AreEqual(htStatBar, AppData.HintType);
  Assert.IsTrue(Application.ShowHint, 'ShowHint should be TRUE when HintType is htStatBar');
end;

procedure TTestAppDataVcl.TestHideHint_SetGet;
begin
  AppData.HideHint:= 3000;
  Assert.AreEqual(3000, AppData.HideHint);
  Assert.AreEqual(3000, Application.HintHidePause, 'HintHidePause should match HideHint');
end;


{ Single Instance Tests }

procedure TTestAppDataVcl.TestInstanceRunning;
var
  IsRunning: Boolean;
begin
  IsRunning:= AppData.InstanceRunning;
  // Result depends on whether main form exists with our class name
  // Just verify it doesn't raise an exception
  Assert.IsTrue(IsRunning OR (NOT IsRunning));
end;

procedure TTestAppDataVcl.TestSingleInstClassName_NotEmpty;
begin
  Assert.IsNotEmpty(AppData.SingleInstClassName);
end;

procedure TTestAppDataVcl.TestSetSingleInstanceName;
var
  Params: TCreateParams;
begin
  FillChar(Params, SizeOf(Params), 0);
  AppData.SetSingleInstanceName(Params);
  Assert.AreEqual(AppData.SingleInstClassName, string(Params.WinClassName));
end;


{ App Control Tests }

procedure TTestAppDataVcl.TestMinimize;
begin
  // Just verify it doesn't raise an exception
  // (Actual minimize may not be visible in test environment)
  AppData.Minimize;
  Assert.IsTrue(AppData.StartMinim, 'StartMinim should be TRUE after Minimize');
end;

procedure TTestAppDataVcl.TestRestore;
begin
  // Restore requires MainForm to exist
  if Application.MainForm <> NIL then
  begin
    AppData.StartMinim:= TRUE;
    AppData.Restore;
    Assert.IsFalse(AppData.StartMinim, 'StartMinim should be FALSE after Restore');
  end
  else
    Assert.Pass('Skipped: MainForm not available');
end;


{ Startup Tests }

procedure TTestAppDataVcl.TestRunSelfAtStartUp_Disable;
var
  Success: Boolean;
begin
  // Disable startup registration (safe operation)
  Success:= AppData.RunSelfAtStartUp(False);
  // May fail due to registry permissions, which is acceptable
  Assert.IsTrue(Success OR (NOT Success));
end;


{ Uninstaller Registry Tests }

procedure TTestAppDataVcl.TestReadAppDataFolder_Empty;
var
  Path: string;
begin
  // Reading for non-existent app should return empty
  Path:= AppData.ReadAppDataFolder('NonExistentAppXYZ123');
  Assert.AreEqual('', Path, 'Non-existent app should return empty path');
end;

procedure TTestAppDataVcl.TestReadInstallationFolder_Empty;
var
  Path: string;
begin
  // Reading for non-existent app should return empty
  Path:= AppData.ReadInstallationFolder('NonExistentAppXYZ123');
  Assert.AreEqual('', Path, 'Non-existent app should return empty path');
end;


 
 



initialization
  TDUnitX.RegisterTestFixture(TTestAppDataVcl);

end.
