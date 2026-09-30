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
    procedure Test_GetTextFromHandle_DesktopWindow_ReturnsString;

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
    procedure Test_SetWindowPosToFront_ValidHandle_NoException;

    [Test]
    procedure Test_SetWindowPosToBack_ValidHandle_NoException;
  end;
{$ENDIF}


implementation
{$IFDEF MSWINDOWS}


procedure TTestWindow.Setup;
begin
  FTestWnd:= AllocateHWnd(NIL);  { Hidden top-level window with the default window procedure }
  if FTestWnd = 0
  then RaiseLastOSError;
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


procedure TTestWindow.Test_GetTextFromHandle_DesktopWindow_ReturnsString;
VAR
  Text: string;
begin
  { GetDesktopWindow returns a valid handle, but its title is usually empty }
  Text:= GetTextFromHandle(GetDesktopWindow);
  { Just verify it doesn't crash - desktop may or may not have text }
  Assert.Pass('GetTextFromHandle executed without error, returned: "' + Text + '"');
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

procedure TTestWindow.Test_SetWindowPosToFront_ValidHandle_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      SetWindowPosToFront(FTestWnd);
    end,
    Exception,
    'SetWindowPosToFront with valid handle should not raise exception'
  );
end;


procedure TTestWindow.Test_SetWindowPosToBack_ValidHandle_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      SetWindowPosToBack(FTestWnd);
    end,
    Exception,
    'SetWindowPosToBack with valid handle should not raise exception'
  );
end;


initialization
  TDUnitX.RegisterTestFixture(TTestWindow);
{$ENDIF}

end.
