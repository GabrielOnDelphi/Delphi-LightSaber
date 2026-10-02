unit Test.LightVcl.Common.Shell;

{=============================================================================================================
   Unit tests for LightVcl.Common.Shell.pas
   Tests the file properties dialog, the Start menu, INF files and ExtractIconFromFile.

   Note: Many functions modify system state (registry, shortcuts) and cannot be fully tested without
   side effects. Tests focus on parameter validation and read-only operations where possible.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  Winapi.Windows,
  Winapi.ActiveX,
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

    { Context menu tests }
    [Test]
    procedure Test_InvokePropertiesDialog_EmptyFileName;

    { Start menu tests }
    [Test]
    procedure Test_InvokeStartMenu_NilForm;

    { Icon tests }
    [Test]
    procedure Test_ExtractIconFromFile_EmptyPath;

    [Test]
    procedure Test_ExtractIconFromFile_ValidExe;

    { Installer tests }
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


{ Context menu tests }

procedure TTestShell.Test_InvokePropertiesDialog_EmptyFileName;
begin
  Assert.WillRaise(
    procedure begin InvokePropertiesDialog(''); end,
    EAssertionFailed,
    'Empty filename should raise assertion'
  );
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
  WinDir, Probe: string;
begin
  { notepad.exe is NOT on Windows 11 - it ships as a Store app, and neither c:\Windows nor
    c:\Windows\System32 holds it. explorer.exe is the file that is always there. }
  WinDir:= GetEnvironmentVariable('WINDIR');
  Probe:= WinDir + '\explorer.exe';
  if NOT FileExists(Probe)
  then Probe:= WinDir + '\System32\shell32.dll';
  Assert.IsTrue(FileExists(Probe), 'No system file to extract an icon from: ' + Probe);

  IconHandle:= ExtractIconFromFile(Probe);
  { ExtractIcon returns 1 when the file is not an executable, DLL or icon file, and NULL when the
    file holds no icon at all. Only a value above 1 is a real icon handle.
    https://learn.microsoft.com/en-us/windows/win32/api/shellapi/nf-shellapi-extracticonw }
  Assert.IsTrue(IconHandle > 1, 'Should extract an icon from ' + Probe);
  DestroyIcon(IconHandle);
end;


{ Installer tests }

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
