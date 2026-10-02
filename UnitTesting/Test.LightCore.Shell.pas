unit Test.LightCore.Shell;

{=============================================================================================================
   Unit tests for LightCore.Shell.pas
   Tests the taskbar routines, GetAssociatedApp and IsApiFunctionAvailable.
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
    procedure Test_GetAssociatedApp_TxtExtension;

    [Test]
    procedure Test_GetAssociatedApp_UnknownExtension;

    { Taskbar tests }
    [Test]
    procedure Test_AddFile2TaskbarMRU_EmptyFileName;

    [Test]
    procedure Test_IsTaskbarAutoHideOn_ReturnsBoolean;

    [Test]
    procedure Test_ShowTaskBar_NoException;

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
