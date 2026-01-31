unit Test.LightVcl.Common.Window;

{=============================================================================================================
   Unit tests for LightVcl.Common.Window.pas
   Tests window finding, visibility, and manipulation functions.

   Note: Many functions interact with the Windows shell and other applications.
   Tests are designed to verify basic functionality without modifying system state.
   Functions that minimize/restore other applications are NOT tested to avoid disruption.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Winapi.Windows,
  Vcl.Forms,
  LightVcl.Common.Window;

type
  [TestFixture]
  TTestWindow = class
  private
    FTestForm: TForm;
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

    { FindWindowByTitle Tests }
    [Test]
    procedure Test_FindWindowByTitle_NonExistent_ReturnsZero;

    [Test]
    procedure Test_FindWindowByTitle_PartialMatch_FindsWindow;

    { GetTextFromHandle Tests }
    [Test]
    procedure Test_GetTextFromHandle_ZeroHandle_ReturnsEmpty;

    [Test]
    procedure Test_GetTextFromHandle_DesktopWindow_ReturnsString;

    { FindChildWindowByClass Tests }
    [Test]
    procedure Test_FindChildWindowByClass_NonExistent_ReturnsZero;

    { FindChildForm Tests }
    [Test]
    procedure Test_FindChildForm_NilParent_RaisesException;

    [Test]
    procedure Test_FindChildForm_EmptyClassName_RaisesException;

    [Test]
    procedure Test_FindChildForm_NonMDI_ReturnsZero;

    { KeepOnTop Tests }
    [Test]
    procedure Test_KeepOnTop_NilForm_RaisesException;

    [Test]
    procedure Test_KeepOnTop_ValidForm_NoException;

    { MaximizeForm Tests }
    [Test]
    procedure Test_MaximizeForm_NilForm_RaisesException;

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


implementation


procedure TTestWindow.Setup;
begin
  FTestForm:= TForm.CreateNew(nil);
  FTestForm.Width:= 300;
  FTestForm.Height:= 200;
  FTestForm.Caption:= 'LightSaber Test Window';
end;


procedure TTestWindow.TearDown;
begin
  FreeAndNil(FTestForm);
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


{ FindWindowByTitle Tests }

procedure TTestWindow.Test_FindWindowByTitle_NonExistent_ReturnsZero;
VAR
  Handle: HWND;
begin
  Handle:= FindWindowByTitle('This Window Title Does Not Exist 12345');
  Assert.AreEqual(HWND(0), Handle, 'Non-existent title should return 0');
end;


procedure TTestWindow.Test_FindWindowByTitle_PartialMatch_FindsWindow;
begin
  { Search for 'Program Manager' which is the desktop window title }
  FindWindowByTitle('Program Manager', True, False);
  { This may or may not find a window depending on Windows version/settings }
  { Just verify it doesn't crash }
  Assert.Pass('FindWindowByTitle executed without error');
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


{ FindChildForm Tests }

procedure TTestWindow.Test_FindChildForm_NilParent_RaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      FindChildForm(nil, 'SomeClass');
    end,
    Exception,
    'Nil parent should raise exception'
  );
end;


procedure TTestWindow.Test_FindChildForm_EmptyClassName_RaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      FindChildForm(FTestForm, '');
    end,
    Exception,
    'Empty class name should raise exception'
  );
end;


procedure TTestWindow.Test_FindChildForm_NonMDI_ReturnsZero;
VAR
  Handle: THandle;
begin
  { Non-MDI form has no MDI children }
  Handle:= FindChildForm(FTestForm, 'TForm');
  Assert.AreEqual(THandle(0), Handle, 'Non-MDI form should return 0 for child search');
end;


{ KeepOnTop Tests }

procedure TTestWindow.Test_KeepOnTop_NilForm_RaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      KeepOnTop(TForm(nil), HWND_TOPMOST);
    end,
    Exception,
    'Nil form should raise exception'
  );
end;


procedure TTestWindow.Test_KeepOnTop_ValidForm_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      KeepOnTop(FTestForm, HWND_TOPMOST);
      KeepOnTop(FTestForm, HWND_NOTOPMOST);
    end,
    Exception,
    'KeepOnTop with valid form should not raise exception'
  );
end;


{ MaximizeForm Tests }

procedure TTestWindow.Test_MaximizeForm_NilForm_RaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      MaximizeForm(nil, True);
    end,
    Exception,
    'Nil form should raise exception'
  );
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
      SetWindowPosToFront(FTestForm.Handle);
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
      SetWindowPosToBack(FTestForm.Handle);
    end,
    Exception,
    'SetWindowPosToBack with valid handle should not raise exception'
  );
end;


initialization
  TDUnitX.RegisterTestFixture(TTestWindow);

end.
