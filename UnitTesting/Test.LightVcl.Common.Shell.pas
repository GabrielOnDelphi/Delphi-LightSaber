unit Test.LightVcl.Common.Shell;

{=============================================================================================================
   Unit tests for LightVcl.Common.Shell.pas
   Tests Windows Shell utility functions for file associations, shortcuts, taskbar, and context menu operations.

   Note: Many functions modify system state (registry, shortcuts) and cannot be fully tested without
   side effects. Tests focus on parameter validation and read-only operations where possible.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  System.Win.ComObj,
  Winapi.Windows,
  Winapi.ActiveX,
  Winapi.ShlObj,
  Vcl.Forms,
  LightVcl.Common.Shell;

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
    procedure Test_GetAssociatedApp_TxtExtension;

    [Test]
    procedure Test_GetAssociatedApp_UnknownExtension;

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

    { Context menu tests }
    [Test]
    procedure Test_InvokePropertiesDialog_EmptyFileName;

    { Taskbar tests }
    [Test]
    procedure Test_AddFile2TaskbarMRU_EmptyFileName;

    [Test]
    procedure Test_IsTaskbarAutoHideOn_ReturnsBoolean;

    [Test]
    procedure Test_ShowTaskBar_NoException;

    { Start menu tests }
    [Test]
    procedure Test_InvokeStartMenu_NilForm;

    { API tests }
    [Test]
    procedure Test_IsApiFunctionAvailable_Kernel32_GetProcAddress;

    [Test]
    procedure Test_IsApiFunctionAvailable_InvalidDll;

    [Test]
    procedure Test_IsApiFunctionAvailable_InvalidFunction;

    { Icon tests }
    [Test]
    procedure Test_ExtractIconFromFile_EmptyPath;

    [Test]
    procedure Test_ExtractIconFromFile_ValidExe;

    { Installer tests }
    [Test]
    procedure Test_AddUninstaller_EmptyPath;

    [Test]
    procedure Test_AddUninstaller_EmptyProductName;

    [Test]
    procedure Test_InstallINF_EmptyPath;
  end;

implementation


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

procedure TTestShell.Test_GetAssociatedApp_TxtExtension;
VAR
  App: string;
begin
  { .txt should have some associated application on most Windows systems }
  App:= GetAssociatedApp('txt');
  { We don't assert a specific value since it varies by system, just that it doesn't crash }
  Assert.Pass('GetAssociatedApp executed without error for .txt');
end;


procedure TTestShell.Test_GetAssociatedApp_UnknownExtension;
VAR
  App: string;
begin
  { Unknown extension should return empty string }
  App:= GetAssociatedApp('xyzabc123nonexistent');
  Assert.AreEqual('', App, 'Unknown extension should return empty string');
end;


procedure TTestShell.Test_AssociateWith_EmptyExtension;
begin
  Assert.WillRaise(
    procedure begin AssociateWith('', 'TestApp', FALSE, FALSE, FALSE); end,
    EAssertionFailed,
    'Empty extension should raise assertion'
  );
end;


procedure TTestShell.Test_AssociateWith_InvalidExtension_NoDot;
begin
  Assert.WillRaise(
    procedure begin AssociateWith('txt', 'TestApp', FALSE, FALSE, FALSE); end,
    EAssertionFailed,
    'Extension without dot should raise assertion'
  );
end;


procedure TTestShell.Test_AssociateWith_InvalidExtension_Wildcard;
begin
  Assert.WillRaise(
    procedure begin AssociateWith('.*', 'TestApp', FALSE, FALSE, FALSE); end,
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
    procedure begin CreateShortcutEx('', Application.ExeName, TRUE); end,
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


{ Context menu tests }

procedure TTestShell.Test_InvokePropertiesDialog_EmptyFileName;
begin
  Assert.WillRaise(
    procedure begin InvokePropertiesDialog(''); end,
    EAssertionFailed,
    'Empty filename should raise assertion'
  );
end;


{ Taskbar tests }

procedure TTestShell.Test_AddFile2TaskbarMRU_EmptyFileName;
begin
  Assert.WillRaise(
    procedure begin AddFile2TaskbarMRU(''); end,
    EAssertionFailed,
    'Empty filename should raise assertion'
  );
end;


procedure TTestShell.Test_IsTaskbarAutoHideOn_ReturnsBoolean;
VAR
  AutoHide: Boolean;
begin
  { Just verify it returns without crashing }
  AutoHide:= IsTaskbarAutoHideOn;
  Assert.Pass('IsTaskbarAutoHideOn returned: ' + BoolToStr(AutoHide, TRUE));
end;


procedure TTestShell.Test_ShowTaskBar_NoException;
begin
  { Test that showing/hiding taskbar doesn't crash }
  { Note: We immediately restore it to shown state }
  ShowTaskBar(FALSE);
  ShowTaskBar(TRUE);
  Assert.Pass('ShowTaskBar executed without exception');
end;


{ Start menu tests }

procedure TTestShell.Test_InvokeStartMenu_NilForm;
begin
  Assert.WillRaise(
    procedure begin InvokeStartMenu(NIL); end,
    EAssertionFailed,
    'Nil form should raise assertion'
  );
end;


{ API tests }

procedure TTestShell.Test_IsApiFunctionAvailable_Kernel32_GetProcAddress;
VAR
  Available: Boolean;
  FuncPtr: Pointer;
begin
  Available:= IsApiFunctionAvailable('kernel32.dll', 'GetProcAddress', FuncPtr);
  Assert.IsTrue(Available, 'GetProcAddress should be available in kernel32.dll');
  Assert.IsNotNull(FuncPtr, 'Function pointer should not be nil');
end;


procedure TTestShell.Test_IsApiFunctionAvailable_InvalidDll;
VAR
  Available: Boolean;
  FuncPtr: Pointer;
begin
  Available:= IsApiFunctionAvailable('nonexistent_dll_12345.dll', 'SomeFunction', FuncPtr);
  Assert.IsFalse(Available, 'Non-existent DLL should return FALSE');
  Assert.IsNull(FuncPtr, 'Function pointer should be nil for invalid DLL');
end;


procedure TTestShell.Test_IsApiFunctionAvailable_InvalidFunction;
VAR
  Available: Boolean;
  FuncPtr: Pointer;
begin
  Available:= IsApiFunctionAvailable('kernel32.dll', 'NonExistentFunction12345', FuncPtr);
  Assert.IsFalse(Available, 'Non-existent function should return FALSE');
  Assert.IsNull(FuncPtr, 'Function pointer should be nil for invalid function');
end;


{ Icon tests }

procedure TTestShell.Test_ExtractIconFromFile_EmptyPath;
begin
  Assert.WillRaise(
    procedure begin ExtractIconFromFile(''); end,
    EAssertionFailed,
    'Empty path should raise assertion'
  );
end;


procedure TTestShell.Test_ExtractIconFromFile_ValidExe;
VAR
  IconHandle: THandle;
begin
  { Extract icon from a system executable that should always exist }
  IconHandle:= ExtractIconFromFile(GetEnvironmentVariable('WINDIR') + '\notepad.exe');
  Assert.IsTrue(IconHandle > 0, 'Should extract icon from notepad.exe');
  if IconHandle > 0
  then DestroyIcon(IconHandle);
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


procedure TTestShell.Test_InstallINF_EmptyPath;
begin
  Assert.WillRaise(
    procedure begin InstallINF('', 0); end,
    EAssertionFailed,
    'Empty path should raise assertion'
  );
end;


initialization
  TDUnitX.RegisterTestFixture(TTestShell);

end.
