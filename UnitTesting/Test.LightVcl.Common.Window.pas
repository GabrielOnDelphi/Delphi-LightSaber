unit Test.LightVcl.Common.Window;

{=============================================================================================================
   Unit tests for LightVcl.Common.Window.pas
   Tests window finding, visibility, and manipulation functions.
   The tests of the routines in LightCore.Win.Window.pas are in Test.LightCore.Win.Window.pas.

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

    { FindWindowByTitle Tests }
    [Test]
    procedure Test_FindWindowByTitle_NonExistent_ReturnsZero;

    [Test]
    procedure Test_FindWindowByTitle_PartialMatch_FindsWindow;

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
    procedure Test_KeepOnTop_ValidForm_SetsAndClearsTopmost;

    { MaximizeForm Tests }
    [Test]
    procedure Test_MaximizeForm_NilForm_RaisesException;

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


{ FindWindowByTitle Tests }

procedure TTestWindow.Test_FindWindowByTitle_NonExistent_ReturnsZero;
VAR
  Handle: HWND;
begin
  Handle:= FindWindowByTitle('This Window Title Does Not Exist 12345');
  Assert.AreEqual(HWND(0), Handle, 'Non-existent title should return 0');
end;


procedure TTestWindow.Test_FindWindowByTitle_PartialMatch_FindsWindow;
VAR
  FormHandle: HWND;
begin
  FormHandle:= FTestForm.Handle;   { Creates the window, titled 'LightSaber Test Window' in Setup }
  Assert.AreEqual(FormHandle, FindWindowByTitle('lightsaber test win', TRUE, FALSE), 'A partial, case-insensitive title must find the test form');
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


procedure TTestWindow.Test_KeepOnTop_ValidForm_SetsAndClearsTopmost;
begin
  KeepOnTop(FTestForm, HWND_TOPMOST);
  Assert.IsTrue(GetWindowLong(FTestForm.Handle, GWL_EXSTYLE) AND WS_EX_TOPMOST <> 0, 'KeepOnTop(HWND_TOPMOST) must make the form topmost');

  KeepOnTop(FTestForm, HWND_NOTOPMOST);
  Assert.IsTrue(GetWindowLong(FTestForm.Handle, GWL_EXSTYLE) AND WS_EX_TOPMOST = 0, 'KeepOnTop(HWND_NOTOPMOST) must clear the topmost flag');
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


initialization
  TDUnitX.RegisterTestFixture(TTestWindow);

end.
