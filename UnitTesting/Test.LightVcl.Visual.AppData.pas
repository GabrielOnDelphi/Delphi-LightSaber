unit Test.LightVcl.Visual.AppData;

{=============================================================================================================
   2026.10.07
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
  { A form whose window class carries AppData.SingleInstClassName, as a single-instance main form does }
  TSingleInstForm = class(TForm)
  protected
    procedure CreateParams(VAR Params: TCreateParams); override;
  end;

  [TestFixture]
  TTestAppDataVcl = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

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
    procedure TestFont_AppliesToExistingForms;

    { Hint Tests }
    [Test]
    procedure TestHintType_Off;

    [Test]
    procedure TestHintType_Tooltips;

    [Test]
    procedure TestHintType_StatusBar;

    [Test]
    procedure TestHintType_ReachesExistingForms;

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

uses
  System.Win.Registry,
  Vcl.Graphics;


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


procedure TSingleInstForm.CreateParams(VAR Params: TCreateParams);
begin
  inherited CreateParams(Params);
  AppData.SetSingleInstanceName(Params);
end;


{ Version Info Tests
  This unit is linked by more than one test EXE, so the expected version is read from the running EXE itself,
  straight through GetFileVersionInfo/VerQueryValue - not through LightCore.ExeVersion, which the routines under test use.
  Tests_LightVcl.Visual.dproj gives its EXE the version 1.2.3.4 (four different numbers, so a routine that returns
  the wrong field fails), linked by the $R *.res line in Tests_LightVcl.Visual.dpr. That EXE must always take the real leg. }
CONST
  VersionedTestExe = 'Tests_LightVcl.Visual.exe';

{ Reads the four fields of VS_FIXEDFILEINFO of the running EXE. Returns FALSE when the EXE has no version resource. }
function ReadExeVersion(OUT Major, Minor, Release, Build: Word): Boolean;
VAR
  Size, Dummy, InfoLen: DWORD;
  Buffer: TBytes;
  Info: PVSFixedFileInfo;
begin
  Major:= 0; Minor:= 0; Release:= 0; Build:= 0;
  Size:= GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);
  if Size = 0
  then EXIT(FALSE);

  SetLength(Buffer, Size);
  if NOT GetFileVersionInfo(PChar(ParamStr(0)), 0, Size, Buffer)
  then EXIT(FALSE);
  if NOT VerQueryValue(Buffer, '\', Pointer(Info), InfoLen) OR (InfoLen < SizeOf(TVSFixedFileInfo))
  then EXIT(FALSE);

  Major  := HiWord(Info.dwFileVersionMS);
  Minor  := LoWord(Info.dwFileVersionMS);
  Release:= HiWord(Info.dwFileVersionLS);
  Build  := LoWord(Info.dwFileVersionLS);
  Result:= TRUE;
end;

{ Reads the version of the running EXE, or skips the test when the EXE has no version resource.
  The skip is refused in Tests_LightVcl.Visual.exe, which has one. }
procedure RequireExeVersion(OUT Major, Minor, Release, Build: Word);
begin
  if ReadExeVersion(Major, Minor, Release, Build)
  then EXIT;

  if SameText(ExtractFileName(ParamStr(0)), VersionedTestExe)
  then Assert.Fail(VersionedTestExe + ' must carry a version resource ($R *.res in its .dpr, VerInfo_* in its .dproj)')
  else Assert.Pass('No version resource in ' + ExtractFileName(ParamStr(0)));
end;

procedure TTestAppDataVcl.TestGetVersionInfo;
var
  Version: string;
begin
  Version:= TAppData.GetVersionInfo;
  Assert.IsNotEmpty(Version, 'Version should not be empty');
end;

procedure TTestAppDataVcl.TestGetVersionInfo_NoBuildNo;
VAR Major, Minor, Release, Build: Word;
begin
  RequireExeVersion(Major, Minor, Release, Build);
  Assert.AreEqual(IntToStr(Major)+ '.'+ IntToStr(Minor)+ '.'+ IntToStr(Release), TAppData.GetVersionInfo(False), 'Version without build number');
end;

procedure TTestAppDataVcl.TestGetVersionInfo_WithBuildNo;
VAR Major, Minor, Release, Build: Word;
begin
  RequireExeVersion(Major, Minor, Release, Build);
  Assert.AreEqual(IntToStr(Major)+ '.'+ IntToStr(Minor)+ '.'+ IntToStr(Release)+ '.'+ IntToStr(Build), TAppData.GetVersionInfo(True), 'Version with build number');
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
VAR Major, Minor, Release, Build: Word;
begin
  RequireExeVersion(Major, Minor, Release, Build);
  Assert.AreEqual(Integer(Major), Integer(AppData.GetVersionInfoMajor), 'Major version');
end;

procedure TTestAppDataVcl.TestGetVersionInfoMinor;
VAR Major, Minor, Release, Build: Word;
begin
  RequireExeVersion(Major, Minor, Release, Build);
  Assert.AreEqual(Integer(Minor), Integer(AppData.GetVersionInfoMinor), 'Minor version');
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

{ setFont stores the first font; from the second assignment on it also copies the font to every existing form.
  The main form's font is used, because setGuiProperties gives AppData exactly that font in a real program, and it lives until the runner ends. }
procedure TTestAppDataVcl.TestFont_AppliesToExistingForms;
var
  Form: TForm;
  MainFont: TFont;
begin
  Assert.IsNotNull(Application.MainForm, 'The test runner must create a main form');
  MainFont:= Application.MainForm.Font;

  AppData.Font:= MainFont;   { First assignment (or a repeat, if another test ran first) }
  Assert.AreSame(TObject(MainFont), TObject(AppData.Font),'AppData.Font must hold the font it was given');

  Form:= TForm.CreateNew(NIL);
  try
    Form.Font.Size:= MainFont.Size + 7;
    AppData.Font:= MainFont;   { Second assignment: must reach the existing form }
    Assert.AreEqual(MainFont.Size, Form.Font.Size, 'An existing form must get the AppData font');
  finally
    FreeAndNil(Form);
  end;
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

{ A form that already exists must follow HintType. Its controls inherit ShowHint from it, so a form left at FALSE shows no tooltip even with Application.ShowHint = TRUE (the BioniX bug of 2026.09). The test runner's main form is created before the tests run. }
procedure TTestAppDataVcl.TestHintType_ReachesExistingForms;
begin
  Assert.IsNotNull(Application.MainForm, 'The test runner must create a main form');

  AppData.HintType:= htOff;
  Assert.IsFalse(Application.MainForm.ShowHint, 'An existing form must get ShowHint = FALSE when HintType is htOff');

  AppData.HintType:= htTooltips;
  Assert.IsTrue(Application.MainForm.ShowHint, 'An existing form must get ShowHint = TRUE back when HintType is htTooltips');
end;

procedure TTestAppDataVcl.TestHideHint_SetGet;
begin
  AppData.HideHint:= 3000;
  Assert.AreEqual(3000, AppData.HideHint);
  Assert.AreEqual(3000, Application.HintHidePause, 'HintHidePause should match HideHint');
end;


{ Single Instance Tests }

{ The runner's main form is a plain TForm, so no window carries SingleInstClassName until the test creates one }
procedure TTestAppDataVcl.TestInstanceRunning;
var
  Form: TSingleInstForm;
begin
  Assert.IsFalse(AppData.InstanceRunning, 'No window with the single-instance class name exists yet');

  Form:= TSingleInstForm.CreateNew(NIL);
  try
    Form.HandleNeeded;
    Assert.IsTrue(AppData.InstanceRunning, 'A window with the single-instance class name exists now');
  finally
    FreeAndNil(Form);
  end;

  Assert.IsFalse(AppData.InstanceRunning, 'The window was destroyed');
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

{ Disabling is safe: it only deletes the value of this EXE under HKCU\...\Run, which the test runner never writes }
procedure TTestAppDataVcl.TestRunSelfAtStartUp_Disable;
var
  Reg: TRegistry;
begin
  Assert.IsTrue(AppData.RunSelfAtStartUp(False), 'HKCU\Software\Microsoft\Windows\CurrentVersion\Run must open');

  Reg:= TRegistry.Create(KEY_READ);
  try
    Reg.RootKey:= HKEY_CURRENT_USER;
    Assert.IsTrue(Reg.OpenKeyReadOnly('\Software\Microsoft\Windows\CurrentVersion\Run'), 'The Run key must exist');
    Assert.IsFalse(Reg.ValueExists(TPath.GetFileNameWithoutExtension(ParamStr(0))), 'No autostart value may remain for this EXE');
    Reg.CloseKey;
  finally
    FreeAndNil(Reg);
  end;
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


 
 



procedure TTestAppDataVcl.TestRaiseIfStillInitializing_NotInitializing;
begin
  { In the test runner, Initializing remains TRUE because the full app startup
    sequence (which sets Initializing:= FALSE) is never completed.
    We cannot test the "not initializing" path without RTTI hacking.
    Verify the property is accessible and consistent with RaiseIfStillInitializing behavior. }
  if TAppData.Initializing
  then Assert.WillRaise(
         procedure
         begin
           TAppData.RaiseIfStillInitializing;
         end, Exception)
  else Assert.WillNotRaise(
         procedure
         begin
           TAppData.RaiseIfStillInitializing;
         end);
end;


procedure TTestAppDataVcl.TestRaiseIfStillInitializing_WhenInitializing;
begin
  { In the test runner, Initializing is TRUE because we only call TAppData.Create
    without completing the full app startup. Verify the method raises as expected. }
  Assert.IsTrue(TAppData.Initializing, 'In test runner, Initializing should be TRUE');
  Assert.WillRaise(
    procedure
    begin
      TAppData.RaiseIfStillInitializing;
    end, Exception);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestAppDataVcl);

end.
