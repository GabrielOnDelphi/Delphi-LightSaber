unit Test.LightCore.Win.Window;

{=============================================================================================================
   Unit tests for LightCore.Win.Window.pas
   Tests window finding, visibility, and manipulation functions.

   Note: Many functions interact with the Windows shell and other applications.
   Tests are designed to verify basic functionality without modifying system state.
   Functions that minimize/restore other applications are NOT tested to avoid disruption.
=============================================================================================================}

interface
{$IFDEF MSWINDOWS}

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Winapi.Windows,
  System.Classes,
  LightCore.Win.Window;

type
  [TestFixture]
  TTestWindow = class
  private
    FTestWnd: HWND;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { FindTopWindowByClass Tests }
    [Test]
    procedure Test_FindTopWindowByClass_NonExistent_ReturnsZero;

    [Test]
    procedure Test_FindTopWindowByClass_DesktopClass_ReturnsHandle;

    { IsApplicationRunning Tests }
    [Test]
    procedure Test_IsApplicationRunning_EmptyClassName_RaisesException;

    [Test]
    procedure Test_IsApplicationRunning_NonExistentClass_ReturnsFalse;

    [Test]
    procedure Test_IsApplicationRunning_ShellClass_ReturnsTrue;

    { GetTextFromHandle Tests }
    [Test]
    procedure Test_GetTextFromHandle_ZeroHandle_ReturnsEmpty;

    [Test]
    procedure Test_GetTextFromHandle_WindowWithTitle_ReturnsTitle;

    { FindChildWindowByClass Tests }
    [Test]
    procedure Test_FindChildWindowByClass_NonExistent_ReturnsZero;

    { RestoreWindowByName Tests }
    [Test]
    procedure Test_RestoreWindowByName_EmptyClassName_RaisesException;

    [Test]
    procedure Test_RestoreWindowByName_NonExistent_ReturnsFalse;

    { RestoreWindow Tests }
    [Test]
    procedure Test_RestoreWindow_ZeroHandle_RaisesException;

    { SetWindowPos Tests }
    [Test]
    procedure Test_SetWindowPosToFront_ValidHandle_MakesTopmost;

    [Test]
    procedure Test_SetWindowPosToBack_ValidHandle_ClearsTopmost;
  end;
{$ENDIF}


implementation
{$IFDEF MSWINDOWS}


procedure TTestWindow.Setup;
begin
  FTestWnd:= AllocateHWnd(NIL);  { Hidden top-level window with the default window procedure }
  if FTestWnd = 0
  then RaiseLastOSError;
  if NOT SetWindowPos(FTestWnd, 0, 0, 0, 100, 100, SWP_NOMOVE OR SWP_NOZORDER OR SWP_NOACTIVATE)   { AllocateHWnd makes it 0x0 }
  then RaiseLastOSError;
end;


function IsTopmost(Wnd: HWND): Boolean;
begin
  Result:= GetWindowLong(Wnd, GWL_EXSTYLE) AND WS_EX_TOPMOST <> 0;
end;


{ Windows does not always let this process make a window topmost. Measured 2026-10-08 on Windows 11 (Tests_LightCore.exe started
  from a terminal): SetWindowPos(HWND_TOPMOST) returns TRUE with GetLastError = 0, yet WS_EX_TOPMOST stays clear. It happened in
  the full run, and in a run of this fixture alone once the test waited 3-5 seconds after the start of the process. A new window,
  20 retries over 1 second, and a visible WS_EX_NOACTIVATE window did not change it. In one full run the call WITH
  SWP_NOACTIVATE worked while the same call WITHOUT it did not.
  So the test first asks Windows with a probe window, straight through the API and with the same flags as the routine under test,
  and skips when Windows refuses. }
function OsAllowsTopmost(Flags: UINT): Boolean;
VAR Probe: HWND;
begin
  Probe:= AllocateHWnd(NIL);
  if Probe = 0
  then RaiseLastOSError;
  TRY
    SetWindowPos(Probe, 0, 0, 0, 100, 100, SWP_NOMOVE OR SWP_NOZORDER OR SWP_NOACTIVATE);
    SetWindowPos(Probe, HWND_TOPMOST, 0, 0, 0, 0, Flags);
    Result:= IsTopmost(Probe);
  FINALLY
    DeallocateHWnd(Probe);
  END;
end;


procedure TTestWindow.TearDown;
begin
  DeallocateHWnd(FTestWnd);
end;


{ FindTopWindowByClass Tests }

procedure TTestWindow.Test_FindTopWindowByClass_NonExistent_ReturnsZero;
VAR
  Handle: THandle;
begin
  Handle:= FindTopWindowByClass('NonExistentWindowClass12345');
  Assert.AreEqual(THandle(0), Handle, 'Non-existent class should return 0');
end;


procedure TTestWindow.Test_FindTopWindowByClass_DesktopClass_ReturnsHandle;
VAR
  Handle: THandle;
begin
  { Progman is the desktop window class, always exists }
  Handle:= FindTopWindowByClass('Progman');
  Assert.IsTrue(Handle <> 0, 'Progman class should exist on Windows');
end;


{ IsApplicationRunning Tests }

procedure TTestWindow.Test_IsApplicationRunning_EmptyClassName_RaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      IsApplicationRunning('');
    end,
    Exception,
    'Empty class name should raise exception'
  );
end;


procedure TTestWindow.Test_IsApplicationRunning_NonExistentClass_ReturnsFalse;
VAR
  Running: Boolean;
begin
  Running:= IsApplicationRunning('NonExistentWindowClass12345');
  Assert.IsFalse(Running, 'Non-existent class should return False');
end;


procedure TTestWindow.Test_IsApplicationRunning_ShellClass_ReturnsTrue;
VAR
  Running: Boolean;
begin
  { Shell_TrayWnd is the taskbar, always exists on Windows }
  Running:= IsApplicationRunning('Shell_TrayWnd');
  Assert.IsTrue(Running, 'Shell_TrayWnd should always exist');
end;


{ GetTextFromHandle Tests }

procedure TTestWindow.Test_GetTextFromHandle_ZeroHandle_ReturnsEmpty;
VAR
  Text: string;
begin
  Text:= GetTextFromHandle(0);
  Assert.AreEqual('', Text, 'Zero handle should return empty string');
end;


procedure TTestWindow.Test_GetTextFromHandle_WindowWithTitle_ReturnsTitle;
CONST
  Title = 'LightSaber test window';
begin
  if NOT SetWindowText(FTestWnd, Title)
  then RaiseLastOSError;

  Assert.AreEqual(Title, GetTextFromHandle(FTestWnd), 'GetTextFromHandle must return the whole window title');
end;


{ FindChildWindowByClass Tests }

procedure TTestWindow.Test_FindChildWindowByClass_NonExistent_ReturnsZero;
VAR
  Handle: THandle;
begin
  Handle:= FindChildWindowByClass(GetDesktopWindow, 'NonExistentClass12345');
  Assert.AreEqual(THandle(0), Handle, 'Non-existent child class should return 0');
end;


{ RestoreWindowByName Tests }

procedure TTestWindow.Test_RestoreWindowByName_EmptyClassName_RaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      RestoreWindowByName('');
    end,
    Exception,
    'Empty class name should raise exception'
  );
end;


procedure TTestWindow.Test_RestoreWindowByName_NonExistent_ReturnsFalse;
VAR
  Found: Boolean;
begin
  Found:= RestoreWindowByName('NonExistentWindowClass12345');
  Assert.IsFalse(Found, 'Non-existent window class should return False');
end;


{ RestoreWindow Tests }

procedure TTestWindow.Test_RestoreWindow_ZeroHandle_RaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      RestoreWindow(0);
    end,
    Exception,
    'Zero handle should raise exception'
  );
end;


{ SetWindowPos Tests }

procedure TTestWindow.Test_SetWindowPosToFront_ValidHandle_MakesTopmost;
begin
  Assert.IsFalse(IsTopmost(FTestWnd), 'Precondition: the test window starts not topmost');
  if NOT OsAllowsTopmost(SWP_NOMOVE OR SWP_NOSIZE OR SWP_NOACTIVATE)   { The flags SetWindowPosToFront passes }
  then Assert.Pass('Windows refuses to make a window of this process topmost right now (see OsAllowsTopmost)');

  SetWindowPosToFront(FTestWnd);

  Assert.IsTrue(IsTopmost(FTestWnd), 'SetWindowPosToFront must make the window topmost. ExStyle=' + IntToHex(GetWindowLong(FTestWnd, GWL_EXSTYLE), 8));
end;


procedure TTestWindow.Test_SetWindowPosToBack_ValidHandle_ClearsTopmost;
begin
  if NOT SetWindowPos(FTestWnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE OR SWP_NOSIZE OR SWP_NOACTIVATE)
  then RaiseLastOSError;
  if NOT IsTopmost(FTestWnd)
  then Assert.Pass('Windows refuses to make a window of this process topmost right now (see OsAllowsTopmost)');

  SetWindowPosToBack(FTestWnd);

  Assert.IsFalse(IsTopmost(FTestWnd), 'SetWindowPosToBack must clear the topmost flag');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestWindow);
{$ENDIF}

end.
