unit Test.LightVcl.Common.CenterControl;

{=============================================================================================================
   Unit tests for LightVcl.Common.CenterControl.pas
   Tests form and control positioning/centering functions.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Types,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  [TestFixture]
  TTestCenterControl = class
  private
    FTestForm: TForm;
    FChildForm: TForm;
    FPanel: TPanel;
    FButton: TButton;
    procedure CleanupForms;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { CorrectFormPositionDesktop Tests }
    [Test]
    procedure TestCorrectFormPositionDesktop_FormInBounds;

    [Test]
    procedure TestCorrectFormPositionDesktop_FormOffLeft;

    [Test]
    procedure TestCorrectFormPositionDesktop_FormOffTop;

    [Test]
    procedure TestCorrectFormPositionDesktop_FormOffRight;

    [Test]
    procedure TestCorrectFormPositionDesktop_FormOffBottom;

    { CenterForm Tests }
    [Test]
    procedure TestCenterForm_CentersOnPrimaryMonitor;

    [Test]
    procedure TestCenterForm_WithParent_CentersInParent;

    [Test]
    procedure TestCenterForm_SmallForm;

    { CorrectCtrlPosition Tests }
    [Test]
    procedure TestCorrectCtrlPosition_ControlInBounds;

    [Test]
    procedure TestCorrectCtrlPosition_ControlOffLeft;

    [Test]
    procedure TestCorrectCtrlPosition_ControlOffTop;

    [Test]
    procedure TestCorrectCtrlPosition_ControlTooWide;

    [Test]
    procedure TestCorrectCtrlPosition_ControlTooTall;

    [Test]
    procedure TestCorrectCtrlPosition_WithParentControl;

    { CenterInvalidChild Tests }
    [Test]
    procedure TestCenterInvalidChild_ValidPosition_NoChange;

    [Test]
    procedure TestCenterInvalidChild_OffLeft_Centers;

    [Test]
    procedure TestCenterInvalidChild_OffTop_Centers;

    [Test]
    procedure TestCenterInvalidChild_OffRight_Centers;

    [Test]
    procedure TestCenterInvalidChild_OffBottom_Centers;

    { CenterChild Tests }
    [Test]
    procedure TestCenterChild_CentersHorizontally;

    [Test]
    procedure TestCenterChild_CentersVertically;

    [Test]
    procedure TestCenterChild_ChildBiggerThanParent_SetsToZero;

    { CenterChildX Tests }
    [Test]
    procedure TestCenterChildX_CentersHorizontallyOnly;

    [Test]
    procedure TestCenterChildX_DoesNotChangeTop;

    { CorrectMDIFormPosition Tests }
    [Test]
    procedure TestCorrectMDIFormPosition_NoException;
  end;

implementation

uses
  LightVcl.Common.CenterControl;


procedure TTestCenterControl.Setup;
begin
  FTestForm:= NIL;
  FChildForm:= NIL;
  FPanel:= NIL;
  FButton:= NIL;
end;


procedure TTestCenterControl.TearDown;
begin
  CleanupForms;
end;


procedure TTestCenterControl.CleanupForms;
begin
  FreeAndNil(FButton);
  FreeAndNil(FPanel);
  FreeAndNil(FChildForm);
  FreeAndNil(FTestForm);
end;


{ CorrectFormPositionDesktop Tests }

procedure TTestCenterControl.TestCorrectFormPositionDesktop_FormInBounds;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Left:= 100;
  FTestForm.Top:= 100;
  FTestForm.Width:= 400;
  FTestForm.Height:= 300;

  CorrectFormPositionDesktop(FTestForm);

  { Form should remain at same position since it's within bounds }
  Assert.AreEqual(100, FTestForm.Left, 'Left should remain 100');
  Assert.AreEqual(100, FTestForm.Top, 'Top should remain 100');
end;


procedure TTestCenterControl.TestCorrectFormPositionDesktop_FormOffLeft;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Left:= -500;
  FTestForm.Top:= 100;
  FTestForm.Width:= 400;
  FTestForm.Height:= 300;

  CorrectFormPositionDesktop(FTestForm);

  { Form should be moved to Left = 0 }
  Assert.IsTrue(FTestForm.Left >= 0, 'Left should be >= 0 after correction');
end;


procedure TTestCenterControl.TestCorrectFormPositionDesktop_FormOffTop;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Left:= 100;
  FTestForm.Top:= -500;
  FTestForm.Width:= 400;
  FTestForm.Height:= 300;

  CorrectFormPositionDesktop(FTestForm);

  { Form should be moved to Top = 0 }
  Assert.IsTrue(FTestForm.Top >= 0, 'Top should be >= 0 after correction');
end;


procedure TTestCenterControl.TestCorrectFormPositionDesktop_FormOffRight;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 400;
  FTestForm.Height:= 300;
  FTestForm.Left:= Screen.DesktopWidth + 100;
  FTestForm.Top:= 100;

  CorrectFormPositionDesktop(FTestForm);

  { Form should be moved back into desktop }
  Assert.IsTrue(FTestForm.Left + FTestForm.Width <= Screen.DesktopWidth,
    'Form right edge should be within desktop');
end;


procedure TTestCenterControl.TestCorrectFormPositionDesktop_FormOffBottom;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 400;
  FTestForm.Height:= 300;
  FTestForm.Left:= 100;
  FTestForm.Top:= Screen.DesktopHeight + 100;

  CorrectFormPositionDesktop(FTestForm);

  { Form should be moved back into desktop }
  Assert.IsTrue(FTestForm.Top + FTestForm.Height <= Screen.DesktopHeight,
    'Form bottom edge should be within desktop');
end;


{ CenterForm Tests }

procedure TTestCenterControl.TestCenterForm_CentersOnPrimaryMonitor;
var
  WorkArea: TRect;
  ExpectedLeft, ExpectedTop: Integer;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 400;
  FTestForm.Height:= 300;

  CenterForm(FTestForm);

  WorkArea:= Screen.PrimaryMonitor.WorkareaRect;
  ExpectedLeft:= WorkArea.Left + ((WorkArea.Width - FTestForm.Width) div 2);
  ExpectedTop:= WorkArea.Top + ((WorkArea.Height - FTestForm.Height) div 2);

  Assert.AreEqual(ExpectedLeft, FTestForm.Left, 'Form should be centered horizontally');
  Assert.AreEqual(ExpectedTop, FTestForm.Top, 'Form should be centered vertically');
end;


procedure TTestCenterControl.TestCenterForm_WithParent_CentersInParent;
var
  ExpectedLeft, ExpectedTop: Integer;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Left:= 100;
  FTestForm.Top:= 100;
  FTestForm.Width:= 800;
  FTestForm.Height:= 600;

  FChildForm:= TForm.CreateNew(NIL);
  FChildForm.Width:= 400;
  FChildForm.Height:= 300;

  CenterForm(FChildForm, FTestForm);

  ExpectedLeft:= FTestForm.Left + ((FTestForm.ClientWidth - FChildForm.Width) div 2);
  ExpectedTop:= FTestForm.Top + ((FTestForm.ClientHeight - FChildForm.Height) div 2);

  Assert.AreEqual(ExpectedLeft, FChildForm.Left, 'Child should be centered horizontally in parent');
  Assert.AreEqual(ExpectedTop, FChildForm.Top, 'Child should be centered vertically in parent');
end;


procedure TTestCenterControl.TestCenterForm_SmallForm;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 100;
  FTestForm.Height:= 50;

  CenterForm(FTestForm);

  { Small form should still center correctly }
  Assert.IsTrue(FTestForm.Left > 0, 'Small form should have positive Left');
  Assert.IsTrue(FTestForm.Top > 0, 'Small form should have positive Top');
end;


{ CorrectCtrlPosition Tests }

procedure TTestCenterControl.TestCorrectCtrlPosition_ControlInBounds;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 800;
  FTestForm.Height:= 600;

  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Left:= 100;
  FButton.Top:= 100;
  FButton.Width:= 100;
  FButton.Height:= 30;

  CorrectCtrlPosition(FButton, FTestForm.ClientWidth, FTestForm.ClientHeight);

  Assert.AreEqual(100, FButton.Left, 'Left should remain 100');
  Assert.AreEqual(100, FButton.Top, 'Top should remain 100');
end;


procedure TTestCenterControl.TestCorrectCtrlPosition_ControlOffLeft;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 800;
  FTestForm.Height:= 600;

  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Left:= -100;
  FButton.Top:= 100;
  FButton.Width:= 100;
  FButton.Height:= 30;

  CorrectCtrlPosition(FButton, FTestForm.ClientWidth, FTestForm.ClientHeight);

  Assert.AreEqual(0, FButton.Left, 'Left should be corrected to 0');
end;


procedure TTestCenterControl.TestCorrectCtrlPosition_ControlOffTop;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 800;
  FTestForm.Height:= 600;

  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Left:= 100;
  FButton.Top:= -100;
  FButton.Width:= 100;
  FButton.Height:= 30;

  CorrectCtrlPosition(FButton, FTestForm.ClientWidth, FTestForm.ClientHeight);

  Assert.AreEqual(0, FButton.Top, 'Top should be corrected to 0');
end;


procedure TTestCenterControl.TestCorrectCtrlPosition_ControlTooWide;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 400;
  FTestForm.Height:= 300;

  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Left:= 0;
  FButton.Top:= 0;
  FButton.Width:= 1000;
  FButton.Height:= 30;

  CorrectCtrlPosition(FButton, FTestForm.ClientWidth, FTestForm.ClientHeight);

  Assert.IsTrue(FButton.Width <= FTestForm.ClientWidth,
    'Width should be clamped to parent width');
end;


procedure TTestCenterControl.TestCorrectCtrlPosition_ControlTooTall;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 400;
  FTestForm.Height:= 300;

  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Left:= 0;
  FButton.Top:= 0;
  FButton.Width:= 100;
  FButton.Height:= 1000;

  CorrectCtrlPosition(FButton, FTestForm.ClientWidth, FTestForm.ClientHeight);

  Assert.IsTrue(FButton.Height <= FTestForm.ClientHeight,
    'Height should be clamped to parent height');
end;


procedure TTestCenterControl.TestCorrectCtrlPosition_WithParentControl;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 800;
  FTestForm.Height:= 600;

  FPanel:= TPanel.Create(FTestForm);
  FPanel.Parent:= FTestForm;
  FPanel.Width:= 400;
  FPanel.Height:= 300;

  FButton:= TButton.Create(FPanel);
  FButton.Parent:= FPanel;
  FButton.Left:= -50;
  FButton.Top:= -50;
  FButton.Width:= 100;
  FButton.Height:= 30;

  CorrectCtrlPosition(FButton, FPanel);

  Assert.AreEqual(0, FButton.Left, 'Left should be corrected to 0');
  Assert.AreEqual(0, FButton.Top, 'Top should be corrected to 0');
end;


{ CenterInvalidChild Tests }

procedure TTestCenterControl.TestCenterInvalidChild_ValidPosition_NoChange;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 800;
  FTestForm.Height:= 600;

  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Left:= 100;
  FButton.Top:= 100;
  FButton.Width:= 100;
  FButton.Height:= 30;

  CenterInvalidChild(FButton, FTestForm);

  { Position is valid, should not change }
  Assert.AreEqual(100, FButton.Left, 'Left should remain unchanged');
  Assert.AreEqual(100, FButton.Top, 'Top should remain unchanged');
end;


procedure TTestCenterControl.TestCenterInvalidChild_OffLeft_Centers;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 800;
  FTestForm.Height:= 600;

  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Left:= -100;
  FButton.Top:= 100;
  FButton.Width:= 100;
  FButton.Height:= 30;

  CenterInvalidChild(FButton, FTestForm);

  { Should be centered }
  Assert.IsTrue(FButton.Left > 0, 'Left should be centered (positive)');
end;


procedure TTestCenterControl.TestCenterInvalidChild_OffTop_Centers;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 800;
  FTestForm.Height:= 600;

  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Left:= 100;
  FButton.Top:= -100;
  FButton.Width:= 100;
  FButton.Height:= 30;

  CenterInvalidChild(FButton, FTestForm);

  { Should be centered }
  Assert.IsTrue(FButton.Top > 0, 'Top should be centered (positive)');
end;


procedure TTestCenterControl.TestCenterInvalidChild_OffRight_Centers;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 800;
  FTestForm.Height:= 600;

  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Left:= 1000;
  FButton.Top:= 100;
  FButton.Width:= 100;
  FButton.Height:= 30;

  CenterInvalidChild(FButton, FTestForm);

  { Should be centered }
  Assert.IsTrue(FButton.Left < FTestForm.Width, 'Left should be within parent');
end;


procedure TTestCenterControl.TestCenterInvalidChild_OffBottom_Centers;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 800;
  FTestForm.Height:= 600;

  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Left:= 100;
  FButton.Top:= 1000;
  FButton.Width:= 100;
  FButton.Height:= 30;

  CenterInvalidChild(FButton, FTestForm);

  { Should be centered }
  Assert.IsTrue(FButton.Top < FTestForm.ClientHeight, 'Top should be within parent');
end;


{ CenterChild Tests }

procedure TTestCenterControl.TestCenterChild_CentersHorizontally;
var
  ExpectedLeft: Integer;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 800;
  FTestForm.Height:= 600;

  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Width:= 100;
  FButton.Height:= 30;

  CenterChild(FButton, FTestForm);

  ExpectedLeft:= (FTestForm.ClientWidth - FButton.Width) div 2;
  Assert.AreEqual(ExpectedLeft, FButton.Left, 'Button should be centered horizontally');
end;


procedure TTestCenterControl.TestCenterChild_CentersVertically;
var
  ExpectedTop: Integer;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 800;
  FTestForm.Height:= 600;

  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Width:= 100;
  FButton.Height:= 30;

  CenterChild(FButton, FTestForm);

  ExpectedTop:= (FTestForm.ClientHeight - FButton.Height) div 2;
  Assert.AreEqual(ExpectedTop, FButton.Top, 'Button should be centered vertically');
end;


procedure TTestCenterControl.TestCenterChild_ChildBiggerThanParent_SetsToZero;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 200;
  FTestForm.Height:= 150;

  FPanel:= TPanel.Create(FTestForm);
  FPanel.Parent:= FTestForm;
  FPanel.Width:= 400;
  FPanel.Height:= 300;

  CenterChild(FPanel, FTestForm);

  { When child is bigger than parent, position should be 0 }
  Assert.AreEqual(0, FPanel.Left, 'Left should be 0 when child wider than parent');
  Assert.AreEqual(0, FPanel.Top, 'Top should be 0 when child taller than parent');
end;


{ CenterChildX Tests }

procedure TTestCenterControl.TestCenterChildX_CentersHorizontallyOnly;
var
  ExpectedLeft: Integer;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 800;
  FTestForm.Height:= 600;

  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Width:= 100;
  FButton.Height:= 30;

  CenterChildX(FButton, FTestForm);

  ExpectedLeft:= (FTestForm.ClientWidth - FButton.Width) div 2;
  Assert.AreEqual(ExpectedLeft, FButton.Left, 'Button should be centered horizontally');
end;


procedure TTestCenterControl.TestCenterChildX_DoesNotChangeTop;
var
  OriginalTop: Integer;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 800;
  FTestForm.Height:= 600;

  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Left:= 10;
  FButton.Top:= 50;
  FButton.Width:= 100;
  FButton.Height:= 30;

  OriginalTop:= FButton.Top;

  CenterChildX(FButton, FTestForm);

  Assert.AreEqual(OriginalTop, FButton.Top, 'Top should not change when centering X only');
end;


{ CorrectMDIFormPosition Tests }

procedure TTestCenterControl.TestCorrectMDIFormPosition_NoException;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 800;
  FTestForm.Height:= 600;

  { CorrectMDIFormPosition should not raise exception even with no MDI children }
  Assert.WillNotRaise(
    procedure
    begin
      CorrectMDIFormPosition(FTestForm);
    end);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestCenterControl);

end.
