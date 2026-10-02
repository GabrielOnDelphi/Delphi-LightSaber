unit Test.LightCore.Win.Shell;

{=============================================================================================================
   Unit tests for LightCore.Win.Shell.pas
   Tests Windows Shell utility functions for file associations, shortcuts and the uninstaller entry.

   Note: Many functions modify system state (registry, shortcuts) and cannot be fully tested without
   side effects. Tests focus on parameter validation and read-only operations where possible.
=============================================================================================================}

interface
{$IFDEF MSWINDOWS}

{$IFOPT C-}
  {$MESSAGE FATAL 'Test.LightCore.Win.Shell needs assertions compiled in. Without them its tests really write file associations, shortcuts and uninstall entries, and one tries to delete HKEY_CURRENT_USER\Software\Classes with all its sub-keys.'}
{$ENDIF}

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  Winapi.ActiveX,
  LightCore.Win.Shell;

type
  [TestFixture]
  TTestShell = class
  private
    FTestDir: string;
    FTestLnkFile: string;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { File Association tests }
    [Test]
    procedure Test_AssociateWith_EmptyExtension;

    [Test]
    procedure Test_AssociateWith_InvalidExtension_NoDot;

    [Test]
    procedure Test_AssociateWith_InvalidExtension_Wildcard;

    [Test]
    procedure Test_AssociationReset_EmptyExtension;

    [Test]
    procedure Test_AssociationReset_InvalidExtension_NoDot;

    { Shortcut tests }
    [Test]
    procedure Test_CreateShortcutEx_EmptyName;

    [Test]
    procedure Test_CreateShortcutEx_EmptyTarget;

    [Test]
    procedure Test_ExtractPathFromLnkFile_EmptyPath;

    [Test]
    procedure Test_ExtractPathFromLnkFile_InvalidPath;

    [Test]
    procedure Test_CreateShortcut_SendTo_EmptyName;

    [Test]
    procedure Test_DeleteDesktopShortcut_EmptyName;

    [Test]
    procedure Test_DeleteStartMenuShortcut_EmptyName;

    { Installer tests }
    [Test]
    procedure Test_AddUninstaller_EmptyPath;

    [Test]
    procedure Test_AddUninstaller_EmptyProductName;
  end;
{$ENDIF}

implementation
{$IFDEF MSWINDOWS}


procedure TTestShell.Setup;
begin
  CoInitialize(nil);
  FTestDir:= TPath.Combine(TPath.GetTempPath, 'ShellTest_' + TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FTestDir);
  FTestLnkFile:= TPath.Combine(FTestDir, 'test.lnk');
end;


procedure TTestShell.TearDown;
begin
  if TDirectory.Exists(FTestDir)
  then TDirectory.Delete(FTestDir, True);
  CoUninitialize;
end;


{ File Association tests }

procedure TTestShell.Test_AssociateWith_EmptyExtension;
begin
  Assert.WillRaise(
    procedure begin AssociateWith('', 'TestApp', FALSE, FALSE); end,
    EAssertionFailed,
    'Empty extension should raise assertion'
  );
end;


procedure TTestShell.Test_AssociateWith_InvalidExtension_NoDot;
begin
  Assert.WillRaise(
    procedure begin AssociateWith('txt', 'TestApp', FALSE, FALSE); end,
    EAssertionFailed,
    'Extension without dot should raise assertion'
  );
end;


procedure TTestShell.Test_AssociateWith_InvalidExtension_Wildcard;
begin
  Assert.WillRaise(
    procedure begin AssociateWith('.*', 'TestApp', FALSE, FALSE); end,
    EAssertionFailed,
    'Extension with wildcard should raise assertion'
  );
end;


procedure TTestShell.Test_AssociationReset_EmptyExtension;
begin
  Assert.WillRaise(
    procedure begin AssociationReset('', FALSE); end,
    EAssertionFailed,
    'Empty extension should raise assertion'
  );
end;


procedure TTestShell.Test_AssociationReset_InvalidExtension_NoDot;
begin
  Assert.WillRaise(
    procedure begin AssociationReset('txt', FALSE); end,
    EAssertionFailed,
    'Extension without dot should raise assertion'
  );
end;


{ Shortcut tests }

procedure TTestShell.Test_CreateShortcutEx_EmptyName;
begin
  Assert.WillRaise(
    procedure begin CreateShortcutEx('', ParamStr(0), TRUE); end,
    EAssertionFailed,
    'Empty shortcut name should raise assertion'
  );
end;


procedure TTestShell.Test_CreateShortcutEx_EmptyTarget;
begin
  Assert.WillRaise(
    procedure begin CreateShortcutEx('TestShortcut', '', TRUE); end,
    EAssertionFailed,
    'Empty target should raise assertion'
  );
end;


procedure TTestShell.Test_ExtractPathFromLnkFile_EmptyPath;
VAR
  Path: string;
begin
  Path:= ExtractPathFromLnkFile('');
  Assert.AreEqual('', Path, 'Empty lnk path should return empty string');
end;


procedure TTestShell.Test_ExtractPathFromLnkFile_InvalidPath;
VAR
  Path: string;
begin
  Path:= ExtractPathFromLnkFile('C:\NonExistent\Invalid.lnk');
  Assert.AreEqual('', Path, 'Invalid lnk path should return empty string');
end;


procedure TTestShell.Test_CreateShortcut_SendTo_EmptyName;
begin
  Assert.WillRaise(
    procedure begin CreateShortcut_SendTo(''); end,
    EAssertionFailed,
    'Empty shortcut name should raise assertion'
  );
end;


procedure TTestShell.Test_DeleteDesktopShortcut_EmptyName;
begin
  Assert.WillRaise(
    procedure begin DeleteDesktopShortcut(''); end,
    EAssertionFailed,
    'Empty shortcut name should raise assertion'
  );
end;


procedure TTestShell.Test_DeleteStartMenuShortcut_EmptyName;
begin
  Assert.WillRaise(
    procedure begin DeleteStartMenuShortcut(''); end,
    EAssertionFailed,
    'Empty shortcut name should raise assertion'
  );
end;


{ Installer tests }

procedure TTestShell.Test_AddUninstaller_EmptyPath;
begin
  Assert.WillRaise(
    procedure begin AddUninstaller('', 'TestProduct'); end,
    EAssertionFailed,
    'Empty path should raise assertion'
  );
end;


procedure TTestShell.Test_AddUninstaller_EmptyProductName;
begin
  Assert.WillRaise(
    procedure begin AddUninstaller('C:\Test\Uninstall.exe', ''); end,
    EAssertionFailed,
    'Empty product name should raise assertion'
  );
end;


initialization
  TDUnitX.RegisterTestFixture(TTestShell);
{$ENDIF}

end.
