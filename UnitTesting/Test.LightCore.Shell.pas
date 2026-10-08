unit Test.LightCore.Shell;

{=============================================================================================================
   2026.10.07
   Unit tests for LightCore.Shell.pas
   Tests the taskbar routines, GetAssociatedApp and IsApiFunctionAvailable.
   Test_ShowTaskBar_HidesAndShows hides the real taskbar for a moment.
=============================================================================================================}

interface
{$IFDEF MSWINDOWS}

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  Winapi.ActiveX,
  LightCore.Shell;

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
    procedure Test_GetAssociatedApp_ProgIdCommand;

    [Test]
    procedure Test_GetAssociatedApp_UnknownExtension;

    { Taskbar tests }
    [Test]
    procedure Test_AddFile2TaskbarMRU_EmptyFileName;

    [Test]
    procedure Test_IsTaskbarAutoHideOn_MatchesAutoHideBar;

    [Test]
    procedure Test_ShowTaskBar_HidesAndShows;

    { API tests }
    [Test]
    procedure Test_IsApiFunctionAvailable_Kernel32_GetProcAddress;

    [Test]
    procedure Test_IsApiFunctionAvailable_InvalidDll;

    [Test]
    procedure Test_IsApiFunctionAvailable_InvalidFunction;
  end;
{$ENDIF}

implementation
{$IFDEF MSWINDOWS}

uses
  Winapi.Windows,
  Winapi.ShellAPI,
  System.Win.Registry;


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

{ Registers a file type of its own under HKCU\Software\Classes (which HKEY_CLASSES_ROOT merges in), the way a ProgID association is stored: '.ext' points to the ProgID, the ProgID holds shell\open\command. The keys are deleted again at the end. }
procedure TTestShell.Test_GetAssociatedApp_ProgIdCommand;
CONST
  AppPath = 'C:\Program Files\LightSaber Test\TestApp.exe';
VAR
  Ext, ProgId: string;
  Reg: TRegistry;
begin
  Ext   := 'lstest' + IntToStr(GetCurrentProcessId);
  ProgId:= 'LightSaberTest.' + Ext;

  Reg:= TRegistry.Create(KEY_READ OR KEY_WRITE);
  try
    Reg.RootKey:= HKEY_CURRENT_USER;
    try
      Assert.IsTrue(Reg.OpenKey('Software\Classes\.' + Ext, TRUE), 'Cannot create the extension key');
      Reg.WriteString('', ProgId);
      Reg.CloseKey;
      Assert.IsTrue(Reg.OpenKey('Software\Classes\' + ProgId + '\shell\open\command', TRUE), 'Cannot create the command key');
      Reg.WriteString('', '"' + AppPath + '" "%1"');
      Reg.CloseKey;

      Assert.AreEqual(AppPath, GetAssociatedApp(Ext), 'GetAssociatedApp must follow the ProgID and strip the quotes and the "%1"');
    finally
      Reg.CloseKey;
      Reg.DeleteKey('Software\Classes\' + ProgId + '\shell\open\command');
      Reg.DeleteKey('Software\Classes\' + ProgId + '\shell\open');
      Reg.DeleteKey('Software\Classes\' + ProgId + '\shell');
      Reg.DeleteKey('Software\Classes\' + ProgId);
      Reg.DeleteKey('Software\Classes\.' + Ext);
    end;
  finally
    FreeAndNil(Reg);
  end;
end;


procedure TTestShell.Test_GetAssociatedApp_UnknownExtension;
VAR
  App: string;
begin
  { Unknown extension should return empty string }
  App:= GetAssociatedApp('xyzabc123nonexistent');
  Assert.AreEqual('', App, 'Unknown extension should return empty string');
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


{ An auto-hide taskbar is registered as the autohide appbar of its screen edge, so ABM_GETAUTOHIDEBAR (a different query than the ABM_GETSTATE that IsTaskbarAutoHideOn sends) returns the taskbar window exactly when auto-hide is on.
  https://learn.microsoft.com/en-us/windows/win32/shell/abm-getautohidebar }
procedure TTestShell.Test_IsTaskbarAutoHideOn_MatchesAutoHideBar;
VAR
  Tray, AutoHideBar: HWND;
  Data: TAppBarData;
begin
  Tray:= FindWindow('Shell_TrayWnd', NIL);
  if Tray = 0
  then Assert.Pass('No taskbar window (Explorer is not running)');

  Data:= Default(TAppBarData);
  Data.cbSize:= SizeOf(Data);
  Data.hWnd:= Tray;
  Assert.IsTrue(SHAppBarMessage(ABM_GETTASKBARPOS, Data) <> 0, 'ABM_GETTASKBARPOS failed');

  AutoHideBar:= HWND(SHAppBarMessage(ABM_GETAUTOHIDEBAR, Data));   { Data.uEdge now holds the taskbar's edge }
  Assert.AreEqual<Boolean>(AutoHideBar = Tray, IsTaskbarAutoHideOn, 'IsTaskbarAutoHideOn must agree with ABM_GETAUTOHIDEBAR');
end;


procedure TTestShell.Test_ShowTaskBar_HidesAndShows;
VAR
  Tray: HWND;
begin
  Tray:= FindWindow('Shell_TrayWnd', NIL);
  if Tray = 0
  then Assert.Pass('No taskbar window (Explorer is not running)');

  try
    ShowTaskBar(FALSE);
    Assert.IsFalse(IsWindowVisible(Tray), 'ShowTaskBar(FALSE) must hide the taskbar');
    ShowTaskBar(TRUE);
    Assert.IsTrue(IsWindowVisible(Tray), 'ShowTaskBar(TRUE) must show the taskbar again');
  finally
    ShowWindow(Tray, SW_SHOW);   { Never leave the user without a taskbar, even when an assertion failed }
  end;
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


initialization
  TDUnitX.RegisterTestFixture(TTestShell);
{$ENDIF}

end.
