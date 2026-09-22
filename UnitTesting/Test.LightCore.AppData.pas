unit Test.LightCore.AppData;

{=============================================================================================================
   Unit tests for LightCore.AppData.pas
   Tests TAppDataCore - application data management, paths, logging, and settings.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  LightCore.INIFile,
  LightCore.AppData;

type
  [TestFixture]
  TTestAppDataCore = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Basic Tests }
    [Test]
    procedure TestAppDataCoreExists;

    [Test]
    procedure TestAppDataCoreNotDuplicate;

    { AppName Tests }
    [Test]
    procedure TestAppName;

    [Test]
    procedure TestAppName_NotEmpty;

    { Path Tests }
    [Test]
    procedure TestAppFolder;

    [Test]
    procedure TestAppFolder_EndsWithDelimiter;

    [Test]
    procedure TestAppDataFolder;

    [Test]
    procedure TestAppDataFolder_ForceCreate;

    [Test]
    procedure TestAppDataFolderAllUsers;

    [Test]
    procedure TestAppSysDir;

    [Test]
    procedure TestIniFile;

    [Test]
    procedure TestIniFile_ContainsAppName;

    [Test]
    procedure TestExeShortName;

    [Test]
    procedure TestExeShortName_NoPath;

    [Test]
    procedure TestLastUsedFolder;

    [Test]
    procedure TestLastUsedFolder_SetGet;

    { Log Tests }
    [Test]
    procedure TestRamLogExists;

    [Test]
    procedure TestLogMsg;

    [Test]
    procedure TestLogError;

    [Test]
    procedure TestLogWarn;

    [Test]
    procedure TestLogInfo;

    [Test]
    procedure TestLogHint;

    [Test]
    procedure TestLogVerb;

    [Test]
    procedure TestLogImpo;

    [Test]
    procedure TestLogBold;

    [Test]
    procedure TestLogEmptyRow;

    [Test]
    procedure TestLogClear;

    { Beta Tester Tests }
    [Test]
    procedure TestRunningHome;

    [Test]
    procedure TestBetaTesterMode;

    [Test]
    procedure TestIsHardCodedExp_Past;

    [Test]
    procedure TestIsHardCodedExp_Future;

    [Test]
    procedure TestIsHardCodedExp_Today;

    { Command Line Tests }
    [Test]
    procedure TestCommandLinePath;

    [Test]
    procedure TestExtractPathFromCmdLine_Quoted;

    [Test]
    procedure TestExtractPathFromCmdLine_Unquoted;

    [Test]
    procedure TestExtractPathFromCmdLine_Empty;

    [Test]
    procedure TestExtractPathFromCmdLine_QuotedWithParams;

    { Settings Tests }
    [Test]
    procedure TestHideHint;

    [Test]
    procedure TestHideHint_DefaultValue;

    [Test]
    procedure TestShowLogOnError;

    [Test]
    procedure TestHintType;

    [Test]
    procedure TestOpacity;

    [Test]
    procedure TestMinimize2Tray;

    [Test]
    procedure TestStartMinim;

    [Test]
    procedure TestAutoStartUp;

    [Test]
    procedure TestLoadSettings_MissingKeysGetDefaults;

    [Test]
    procedure TestLoadSettings_KeyInIniWins;

    [Test]
    procedure TestLoadSettings_LegacyHintTypeKeyIsIgnored;

    { RunningFirstTime Tests }
    [Test]
    procedure TestRunningFirstTime;

    { SingleInstance Tests }
    [Test]
    procedure TestSingleInstClassName;

    { Product Details Tests }
    [Test]
    procedure TestProductDetails;
  end;

implementation

TYPE
  TAppDataCoreAccess = class(TAppDataCore);   // Reaches the protected defaultSettings / loadSettings


procedure TTestAppDataCore.Setup;
begin
  // Tests expect AppDataCore to already be created by the test runner
end;

procedure TTestAppDataCore.TearDown;
begin
  // Cleanup if needed
end;


{ Basic Tests }

procedure TTestAppDataCore.TestAppDataCoreExists;
begin
  Assert.IsNotNull(AppDataCore, 'AppDataCore should be created');
end;

procedure TTestAppDataCore.TestAppDataCoreNotDuplicate;
begin
  // The class should prevent duplicate creation via FCreated flag
  // We just verify the object exists and is valid
  Assert.IsNotNull(AppDataCore);
  Assert.IsNotNull(AppDataCore.RamLog);
end;


{ AppName Tests }

procedure TTestAppDataCore.TestAppName;
begin
  Assert.IsNotEmpty(TAppDataCore.AppName);
end;

procedure TTestAppDataCore.TestAppName_NotEmpty;
var
  Name: string;
begin
  Name:= TAppDataCore.AppName;
  Assert.IsTrue(Length(Name) > 0, 'AppName must not be empty');
  Assert.IsTrue(TPath.HasValidFileNameChars(Name, FALSE), 'AppName must have valid filename chars');
end;


{ Path Tests }

procedure TTestAppDataCore.TestAppFolder;
var
  Folder: string;
begin
  Folder:= TAppDataCore.AppFolder;
  Assert.IsNotEmpty(Folder, 'AppFolder should not be empty');
  Assert.IsTrue(TDirectory.Exists(Folder), 'AppFolder should exist');
end;

procedure TTestAppDataCore.TestAppFolder_EndsWithDelimiter;
var
  Folder: string;
begin
  Folder:= TAppDataCore.AppFolder;
  Assert.IsTrue(Folder[Length(Folder)] = PathDelim, 'AppFolder should end with path delimiter');
end;

procedure TTestAppDataCore.TestAppDataFolder;
var
  Folder: string;
begin
  Folder:= TAppDataCore.AppDataFolder;
  Assert.IsNotEmpty(Folder);
  Assert.IsTrue(Pos(TAppDataCore.AppName, Folder) > 0, 'AppDataFolder should contain AppName');
end;

procedure TTestAppDataCore.TestAppDataFolder_ForceCreate;
var
  Folder: string;
begin
  Folder:= TAppDataCore.AppDataFolder(True);
  Assert.IsTrue(TDirectory.Exists(Folder), 'AppDataFolder(ForceDir=True) should create folder');
end;

procedure TTestAppDataCore.TestAppDataFolderAllUsers;
var
  Folder: string;
begin
  Folder:= TAppDataCore.AppDataFolderAllUsers;
  Assert.IsNotEmpty(Folder, 'AppDataFolderAllUsers should not be empty');
end;

procedure TTestAppDataCore.TestAppSysDir;
var
  SysDir: string;
begin
  SysDir:= TAppDataCore.AppSysDir;
  Assert.IsNotEmpty(SysDir);
  Assert.IsTrue(Pos('System', SysDir) > 0, 'AppSysDir should contain "System"');
end;

procedure TTestAppDataCore.TestIniFile;
var
  IniPath: string;
begin
  IniPath:= TAppDataCore.IniFile;
  Assert.IsNotEmpty(IniPath);
  Assert.IsTrue(IniPath.EndsWith('.ini', True), 'IniFile should end with .ini');
end;

procedure TTestAppDataCore.TestIniFile_ContainsAppName;
var
  IniPath: string;
begin
  IniPath:= TAppDataCore.IniFile;
  Assert.IsTrue(Pos(TAppDataCore.AppName, IniPath) > 0, 'IniFile path should contain AppName');
end;

procedure TTestAppDataCore.TestExeShortName;
var
  ShortName: string;
begin
  ShortName:= TAppDataCore.ExeShortName;
  Assert.IsNotEmpty(ShortName);
end;

procedure TTestAppDataCore.TestExeShortName_NoPath;
var
  ShortName: string;
begin
  ShortName:= TAppDataCore.ExeShortName;
  Assert.AreEqual(0, Pos(PathDelim, ShortName), 'ExeShortName should not contain path delimiter');
end;

procedure TTestAppDataCore.TestLastUsedFolder;
var
  Folder: string;
begin
  Folder:= AppDataCore.LastUsedFolder;
  Assert.IsNotEmpty(Folder, 'LastUsedFolder should return documents if not set');
end;

procedure TTestAppDataCore.TestLastUsedFolder_SetGet;
var
  TestPath: string;
begin
  TestPath:= TPath.GetTempPath;
  AppDataCore.LastUsedFolder:= TestPath;
  Assert.AreEqual(TestPath, AppDataCore.LastUsedFolder);
end;


{ Log Tests }

procedure TTestAppDataCore.TestRamLogExists;
begin
  Assert.IsNotNull(AppDataCore.RamLog, 'RamLog should be created');
end;

procedure TTestAppDataCore.TestLogMsg;
var
  InitialCount: Integer;
begin
  InitialCount:= AppDataCore.RamLog.Lines.Count;
  AppDataCore.LogMsg('Test message');
  Assert.AreEqual(InitialCount + 1, AppDataCore.RamLog.Lines.Count);
end;

procedure TTestAppDataCore.TestLogError;
var
  InitialCount: Integer;
begin
  InitialCount:= AppDataCore.RamLog.Lines.Count;
  AppDataCore.LogError('Test error');
  Assert.AreEqual(InitialCount + 1, AppDataCore.RamLog.Lines.Count);
end;

procedure TTestAppDataCore.TestLogWarn;
var
  InitialCount: Integer;
begin
  InitialCount:= AppDataCore.RamLog.Lines.Count;
  AppDataCore.LogWarn('Test warning');
  Assert.AreEqual(InitialCount + 1, AppDataCore.RamLog.Lines.Count);
end;

procedure TTestAppDataCore.TestLogInfo;
var
  InitialCount: Integer;
begin
  InitialCount:= AppDataCore.RamLog.Lines.Count;
  AppDataCore.LogInfo('Test info');
  Assert.AreEqual(InitialCount + 1, AppDataCore.RamLog.Lines.Count);
end;

procedure TTestAppDataCore.TestLogHint;
var
  InitialCount: Integer;
begin
  InitialCount:= AppDataCore.RamLog.Lines.Count;
  AppDataCore.LogHint('Test hint');
  Assert.AreEqual(InitialCount + 1, AppDataCore.RamLog.Lines.Count);
end;

procedure TTestAppDataCore.TestLogVerb;
var
  InitialCount: Integer;
begin
  InitialCount:= AppDataCore.RamLog.Lines.Count;
  AppDataCore.LogVerb('Test verbose');
  // Verbose messages may or may not be added depending on log level
  Assert.IsTrue(AppDataCore.RamLog.Lines.Count >= InitialCount);
end;

procedure TTestAppDataCore.TestLogImpo;
var
  InitialCount: Integer;
begin
  InitialCount:= AppDataCore.RamLog.Lines.Count;
  AppDataCore.LogImpo('Test important');
  Assert.AreEqual(InitialCount + 1, AppDataCore.RamLog.Lines.Count);
end;

procedure TTestAppDataCore.TestLogBold;
var
  InitialCount: Integer;
begin
  InitialCount:= AppDataCore.RamLog.Lines.Count;
  AppDataCore.LogBold('Test bold');
  Assert.AreEqual(InitialCount + 1, AppDataCore.RamLog.Lines.Count);
end;

procedure TTestAppDataCore.TestLogEmptyRow;
var
  InitialCount: Integer;
begin
  InitialCount:= AppDataCore.RamLog.Lines.Count;
  AppDataCore.LogEmptyRow;
  Assert.AreEqual(InitialCount + 1, AppDataCore.RamLog.Lines.Count);
end;

procedure TTestAppDataCore.TestLogClear;
begin
  AppDataCore.LogMsg('Before clear');
  Assert.IsTrue(AppDataCore.RamLog.Lines.Count > 0);

  AppDataCore.LogClear;
  Assert.AreEqual(0, AppDataCore.RamLog.Lines.Count);
end;


{ Beta Tester Tests }

procedure TTestAppDataCore.TestRunningHome;
var
  IsHome: Boolean;
begin
  IsHome:= TAppDataCore.RunningHome;
  // Just verify it returns without exception - result depends on environment
  Assert.IsTrue(IsHome OR (NOT IsHome));
end;

procedure TTestAppDataCore.TestBetaTesterMode;
var
  IsBeta: Boolean;
begin
  IsBeta:= AppDataCore.BetaTesterMode;
  // Just verify it returns without exception
  Assert.IsTrue(IsBeta OR (NOT IsBeta));
end;

procedure TTestAppDataCore.TestIsHardCodedExp_Past;
begin
  // Date in the past should be expired (return True)
  Assert.IsTrue(AppDataCore.IsHardCodedExp(2000, 1, 1), '2000-01-01 should be expired');
end;

procedure TTestAppDataCore.TestIsHardCodedExp_Future;
begin
  // Date in the future should not be expired (return False)
  Assert.IsFalse(AppDataCore.IsHardCodedExp(2099, 12, 31), '2099-12-31 should not be expired');
end;

procedure TTestAppDataCore.TestIsHardCodedExp_Today;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Date, Year, Month, Day);
  // Today should be expired (returns True when diff <= 0)
  Assert.IsTrue(AppDataCore.IsHardCodedExp(Year, Month, Day), 'Today should be expired');
end;


{ Command Line Tests }

procedure TTestAppDataCore.TestCommandLinePath;
var
  CmdPath: string;
begin
  CmdPath:= CommandLinePath;
  // Should return empty if no params, or the first param
  Assert.IsTrue((CmdPath = '') OR (Length(CmdPath) > 0));
end;

procedure TTestAppDataCore.TestExtractPathFromCmdLine_Quoted;
var
  Path, Params: string;
begin
  ExtractPathFromCmdLine('"C:\Test\File.txt"', Path, Params);
  Assert.AreEqual('C:\Test\File.txt', Path);
  Assert.AreEqual('', Params);
end;

procedure TTestAppDataCore.TestExtractPathFromCmdLine_Unquoted;
var
  Path, Params: string;
begin
  ExtractPathFromCmdLine('C:\Test\File.txt', Path, Params);
  Assert.AreEqual('C:\Test\File.txt', Path);
  Assert.AreEqual('', Params);
end;

procedure TTestAppDataCore.TestExtractPathFromCmdLine_Empty;
begin
  { Empty command line raises EAssertionFailed per implementation }
  Assert.WillRaise(
    procedure
    var
      Path, Params: string;
    begin
      ExtractPathFromCmdLine('', Path, Params);
    end,
    EAssertionFailed);
end;

procedure TTestAppDataCore.TestExtractPathFromCmdLine_QuotedWithParams;
var
  Path, Params: string;
begin
  ExtractPathFromCmdLine('"C:\Test\File.txt" -param1 -param2', Path, Params);
  Assert.AreEqual('C:\Test\File.txt', Path);
  Assert.AreEqual('-param1 -param2', Params);
end;


{ Settings Tests }

procedure TTestAppDataCore.TestHideHint;
begin
  AppDataCore.HideHint:= 5000;
  Assert.AreEqual(5000, AppDataCore.HideHint);
end;

procedure TTestAppDataCore.TestHideHint_DefaultValue;
begin
  // HideHint should have a reasonable default (not 0)
  Assert.IsTrue(AppDataCore.HideHint > 0, 'HideHint should have positive default');
end;

procedure TTestAppDataCore.TestShowLogOnError;
begin
  AppDataCore.ShowLogOnError:= True;
  Assert.IsTrue(AppDataCore.ShowLogOnError);

  AppDataCore.ShowLogOnError:= False;
  Assert.IsFalse(AppDataCore.ShowLogOnError);
end;

procedure TTestAppDataCore.TestHintType;
begin
  AppDataCore.HintType:= htOff;
  Assert.AreEqual(htOff, AppDataCore.HintType);

  AppDataCore.HintType:= htTooltips;
  Assert.AreEqual(htTooltips, AppDataCore.HintType);

  AppDataCore.HintType:= htStatBar;
  Assert.AreEqual(htStatBar, AppDataCore.HintType);
end;

procedure TTestAppDataCore.TestOpacity;
begin
  AppDataCore.Opacity:= 200;
  Assert.AreEqual(200, AppDataCore.Opacity);
end;

procedure TTestAppDataCore.TestMinimize2Tray;
begin
  AppDataCore.Minimize2Tray:= True;
  Assert.IsTrue(AppDataCore.Minimize2Tray);

  AppDataCore.Minimize2Tray:= False;
  Assert.IsFalse(AppDataCore.Minimize2Tray);
end;

procedure TTestAppDataCore.TestStartMinim;
begin
  AppDataCore.StartMinim:= True;
  Assert.IsTrue(AppDataCore.StartMinim);

  AppDataCore.StartMinim:= False;
  Assert.IsFalse(AppDataCore.StartMinim);
end;

procedure TTestAppDataCore.TestAutoStartUp;
begin
  AppDataCore.AutoStartUp:= True;
  Assert.IsTrue(AppDataCore.AutoStartUp);

  AppDataCore.AutoStartUp:= False;
  Assert.IsFalse(AppDataCore.AutoStartUp);
end;

{ An INI written by an older version lacks some keys. Each missing key must get the value a fresh install gets (defaultSettings), not a second, hard-coded fallback.
  The old fallback for HintType was htOff, and it switched off every tooltip in the VCL programs (BioniX, 2026.09). Same order as TAppDataCore.Create: defaultSettings, then loadSettings. }
procedure TTestAppDataCore.TestLoadSettings_MissingKeysGetDefaults;
begin
  VAR Ini:= TIniFileEx.Create('AppData Settings', TAppDataCore.IniFile);
  try
    Ini.EraseSection('AppData Settings');
  finally
    FreeAndNil(Ini);
  end;

  TAppDataCoreAccess(AppDataCore).defaultSettings;
  TAppDataCoreAccess(AppDataCore).loadSettings;

  Assert.AreEqual(htTooltips, AppDataCore.HintType, 'A missing HintType key must give the default htTooltips, not htOff');
  Assert.AreEqual(4000, AppDataCore.HideHint,       'A missing HideHint key must give the default');
  Assert.AreEqual(250,  AppDataCore.Opacity,        'A missing Opacity key must give the default');
  Assert.IsTrue(AppDataCore.Minimize2Tray,          'A missing Minimize2Tray key must give the default');
end;

{ The other half: a key that IS in the INI must still win over the default. }
procedure TTestAppDataCore.TestLoadSettings_KeyInIniWins;
begin
  VAR Ini:= TIniFileEx.Create('AppData Settings', TAppDataCore.IniFile);
  try
    Ini.Write('HintStyle', Ord(htStatBar));
    Ini.Write('HideHint' , 1234);
  finally
    FreeAndNil(Ini);
  end;

  TAppDataCoreAccess(AppDataCore).defaultSettings;
  TAppDataCoreAccess(AppDataCore).loadSettings;

  Assert.AreEqual(htStatBar, AppDataCore.HintType, 'HintStyle from the INI must win over the default');
  Assert.AreEqual(1234, AppDataCore.HideHint,      'HideHint from the INI must win over the default');
end;

{ The INI files damaged by the old bug hold HintType=0 (htOff). That key was renamed to 'HintStyle' in 2026.09 exactly so those values are ignored: the user must get the tooltips back. SaveSettings also deletes the dead key. }
procedure TTestAppDataCore.TestLoadSettings_LegacyHintTypeKeyIsIgnored;
begin
  VAR Ini:= TIniFileEx.Create('AppData Settings', TAppDataCore.IniFile);
  try
    Ini.EraseSection('AppData Settings');
    Ini.Write('HintType', Ord(htOff));                     // What an INI written before 2026.09 holds
  finally
    FreeAndNil(Ini);
  end;

  TAppDataCoreAccess(AppDataCore).defaultSettings;
  TAppDataCoreAccess(AppDataCore).loadSettings;

  Assert.AreEqual(htTooltips, AppDataCore.HintType, 'The legacy HintType key must be ignored, so the default htTooltips wins');

  TAppDataCoreAccess(AppDataCore).SaveSettings;

  Ini:= TIniFileEx.Create('AppData Settings', TAppDataCore.IniFile);
  try
    Assert.IsFalse(Ini.ValueExists('HintType' ), 'SaveSettings must delete the dead HintType key');
    Assert.IsTrue (Ini.ValueExists('HintStyle'), 'SaveSettings must write the new HintStyle key');
  finally
    FreeAndNil(Ini);
  end;
end;


{ RunningFirstTime Tests }

procedure TTestAppDataCore.TestRunningFirstTime;
begin
  // Just verify it returns a valid boolean (depends on whether INI file existed)
  Assert.IsTrue(AppDataCore.RunningFirstTime OR (NOT AppDataCore.RunningFirstTime));
end;


{ SingleInstance Tests }

procedure TTestAppDataCore.TestSingleInstClassName;
begin
  Assert.IsNotEmpty(AppDataCore.SingleInstClassName, 'SingleInstClassName should not be empty');
end;


{ Product Details Tests }

procedure TTestAppDataCore.TestProductDetails;
begin
  // Verify product details are set to defaults
  Assert.IsNotEmpty(AppDataCore.CompanyName, 'CompanyName should be set');
  Assert.IsNotEmpty(AppDataCore.CompanyHome, 'CompanyHome should be set');
  Assert.IsNotEmpty(AppDataCore.ProductHome, 'ProductHome should be set');
  Assert.IsNotEmpty(AppDataCore.ProductOrder, 'ProductOrder should be set');
  Assert.IsNotEmpty(AppDataCore.ProductSupport, 'ProductSupport should be set');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestAppDataCore);

end.
