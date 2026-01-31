unit Test.LightFmx.Common.CenterControl;

{=============================================================================================================
   Unit tests for LightFmx.Common.CenterControl.pas
   Tests form and control positioning/centering functions for FMX.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Types,
  System.Math,
  FMX.Forms,
  FMX.Controls,
  FMX.StdCtrls,
  FMX.Layouts;

type
  [TestFixture]
  TTestFmxCenterControl = class
  private
    FTestForm: TForm;
    FChildForm: TForm;
    FLayout: TLayout;
    FButton: TButton;
    procedure CleanupForms;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { CenterFormOnDesktop Tests }
    [Test]
    procedure TestCenterFormOnDesktop_CentersForm;

    [Test]
    procedure TestCenterFormOnDesktop_NilRaisesAssertion;

    { EnsureFormVisibleOnScreen Tests }
    [Test]
    procedure TestEnsureFormVisibleOnScreen_FormInBounds;

    [Test]
    procedure TestEnsureFormVisibleOnScreen_FormOffLeft;

    [Test]
    procedure TestEnsureFormVisibleOnScreen_FormOffTop;

    [Test]
    procedure TestEnsureFormVisibleOnScreen_NilRaisesAssertion;

    { CenterFormOnParent Tests }
    [Test]
    procedure TestCenterFormOnParent_CentersInParent;

    [Test]
    procedure TestCenterFormOnParent_NilFormRaisesAssertion;

    [Test]
    procedure TestCenterFormOnParent_NilParentRaisesAssertion;

    { CenterControl (with parent) Tests }
    [Test]
    procedure TestCenterControl_CentersHorizontally;

    [Test]
    procedure TestCenterControl_CentersVertically;

    [Test]
    procedure TestCenterControl_NilCtrlRaisesAssertion;

    [Test]
    procedure TestCenterControl_NoParentRaisesAssertion;

    { CenterControl (with dimensions) Tests }
    [Test]
    procedure TestCenterControlDims_CentersCorrectly;

    [Test]
    procedure TestCenterControlDims_NilCtrlRaisesAssertion;

    [Test]
    procedure TestCenterControlDims_LargerThanParent_NegativePosition;

    { EnsureControlVisible (with parent) Tests }
    [Test]
    procedure TestEnsureControlVisible_ControlInBounds;

    [Test]
    procedure TestEnsureControlVisible_ControlOffLeft;

    [Test]
    procedure TestEnsureControlVisible_ControlOffTop;

    [Test]
    procedure TestEnsureControlVisible_NilCtrlRaisesAssertion;

    [Test]
    procedure TestEnsureControlVisible_NoParentRaisesAssertion;

    { EnsureControlVisible (with dimensions) Tests }
    [Test]
    procedure TestEnsureControlVisibleDims_ControlInBounds;

    [Test]
    procedure TestEnsureControlVisibleDims_ControlOffRight;

    [Test]
    procedure TestEnsureControlVisibleDims_ControlOffBottom;

    [Test]
    procedure TestEnsureControlVisibleDims_ControlLargerThanParent;

    [Test]
    procedure TestEnsureControlVisibleDims_NilCtrlRaisesAssertion;
  end;

implementation

uses
  LightFmx.Common.CenterControl;


procedure TTestFmxCenterControl.Setup;
begin
  FTestForm:= NIL;
  FChildForm:= NIL;
  FLayout:= NIL;
  FButton:= NIL;
end;


procedure TTestFmxCenterControl.TearDown;
begin
  CleanupForms;
end;


procedure TTestFmxCenterControl.CleanupForms;
begin
  FreeAndNil(FButton);
  FreeAndNil(FLayout);
  FreeAndNil(FChildForm);
  FreeAndNil(FTestForm);
end;


{ CenterFormOnDesktop Tests }

procedure TTestFmxCenterControl.TestCenterFormOnDesktop_CentersForm;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 400;
  FTestForm.Height:= 300;

  CenterFormOnDesktop(FTestForm);

  { Form should be centered - verify it's not at 0,0 }
  Assert.IsTrue((FTestForm.Left > 0) or (FTestForm.Top > 0), 'Form should be centered on desktop');
end;


procedure TTestFmxCenterControl.TestCenterFormOnDesktop_NilRaisesAssertion;
begin
  Assert.WillRaise(
    procedure
    begin
      CenterFormOnDesktop(NIL);
    end,
    EAssertionFailed);
end;


{ EnsureFormVisibleOnScreen Tests }

procedure TTestFmxCenterControl.TestEnsureFormVisibleOnScreen_FormInBounds;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Left:= 100;
  FTestForm.Top:= 100;
  FTestForm.Width:= 400;
  FTestForm.Height:= 300;

  EnsureFormVisibleOnScreen(FTestForm);

  { Form should remain at same position since it's within bounds }
  Assert.AreEqual(100, FTestForm.Left, 'Left should remain 100');
  Assert.AreEqual(100, FTestForm.Top, 'Top should remain 100');
end;


procedure TTestFmxCenterControl.TestEnsureFormVisibleOnScreen_FormOffLeft;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Left:= -500;
  FTestForm.Top:= 100;
  FTestForm.Width:= 400;
  FTestForm.Height:= 300;

  EnsureFormVisibleOnScreen(FTestForm);

  { Form should be moved to Left >= 0 }
  Assert.IsTrue(FTestForm.Left >= 0, 'Left should be >= 0 after correction');
end;


procedure TTestFmxCenterControl.TestEnsureFormVisibleOnScreen_FormOffTop;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Left:= 100;
  FTestForm.Top:= -500;
  FTestForm.Width:= 400;
  FTestForm.Height:= 300;

  EnsureFormVisibleOnScreen(FTestForm);

  { Form should be moved to Top >= 0 }
  Assert.IsTrue(FTestForm.Top >= 0, 'Top should be >= 0 after correction');
end;


procedure TTestFmxCenterControl.TestEnsureFormVisibleOnScreen_NilRaisesAssertion;
begin
  Assert.WillRaise(
    procedure
    begin
      EnsureFormVisibleOnScreen(NIL);
    end,
    EAssertionFailed);
end;


{ CenterFormOnParent Tests }

procedure TTestFmxCenterControl.TestCenterFormOnParent_CentersInParent;
var
  ExpectedLeft, ExpectedTop: Single;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Left:= 100;
  FTestForm.Top:= 100;
  FTestForm.Width:= 800;
  FTestForm.Height:= 600;

  FChildForm:= TForm.CreateNew(NIL);
  FChildForm.Width:= 400;
  FChildForm.Height:= 300;

  CenterFormOnParent(FChildForm, FTestForm);

  ExpectedLeft:= FTestForm.Left + (FTestForm.Width - FChildForm.Width) / 2;
  ExpectedTop:= FTestForm.Top + (FTestForm.Height - FChildForm.Height) / 2;

  Assert.AreEqual(Round(ExpectedLeft), FChildForm.Left, 'Child should be centered horizontally in parent');
  Assert.AreEqual(Round(ExpectedTop), FChildForm.Top, 'Child should be centered vertically in parent');
end;


procedure TTestFmxCenterControl.TestCenterFormOnParent_NilFormRaisesAssertion;
begin
  FTestForm:= TForm.CreateNew(NIL);

  Assert.WillRaise(
    procedure
    begin
      CenterFormOnParent(NIL, FTestForm);
    end,
    EAssertionFailed);
end;


procedure TTestFmxCenterControl.TestCenterFormOnParent_NilParentRaisesAssertion;
begin
  FTestForm:= TForm.CreateNew(NIL);

  Assert.WillRaise(
    procedure
    begin
      CenterFormOnParent(FTestForm, NIL);
    end,
    EAssertionFailed);
end;


{ CenterControl (with parent) Tests }

procedure TTestFmxCenterControl.TestCenterControl_CentersHorizontally;
var
  ExpectedX: Single;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 800;
  FTestForm.Height:= 600;

  FLayout:= TLayout.Create(FTestForm);
  FLayout.Parent:= FTestForm;
  FLayout.Width:= 400;
  FLayout.Height:= 300;

  FButton:= TButton.Create(FLayout);
  FButton.Parent:= FLayout;
  FButton.Width:= 100;
  FButton.Height:= 30;

  CenterControl(FButton);

  ExpectedX:= (FLayout.Width - FButton.Width) / 2;
  Assert.AreEqual(ExpectedX, FButton.Position.X, 0.01, 'Button should be centered horizontally');
end;


procedure TTestFmxCenterControl.TestCenterControl_CentersVertically;
var
  ExpectedY: Single;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 800;
  FTestForm.Height:= 600;

  FLayout:= TLayout.Create(FTestForm);
  FLayout.Parent:= FTestForm;
  FLayout.Width:= 400;
  FLayout.Height:= 300;

  FButton:= TButton.Create(FLayout);
  FButton.Parent:= FLayout;
  FButton.Width:= 100;
  FButton.Height:= 30;

  CenterControl(FButton);

  ExpectedY:= (FLayout.Height - FButton.Height) / 2;
  Assert.AreEqual(ExpectedY, FButton.Position.Y, 0.01, 'Button should be centered vertically');
end;


procedure TTestFmxCenterControl.TestCenterControl_NilCtrlRaisesAssertion;
begin
  Assert.WillRaise(
    procedure
    begin
      CenterControl(NIL);
    end,
    EAssertionFailed);
end;


procedure TTestFmxCenterControl.TestCenterControl_NoParentRaisesAssertion;
begin
  FTestForm:= TForm.CreateNew(NIL);

  FButton:= TButton.Create(NIL);
  { FButton has no parent }

  Assert.WillRaise(
    procedure
    begin
      CenterControl(FButton);
    end,
    EAssertionFailed);
end;


{ CenterControl (with dimensions) Tests }

procedure TTestFmxCenterControl.TestCenterControlDims_CentersCorrectly;
var
  ExpectedX, ExpectedY: Single;
begin
  FTestForm:= TForm.CreateNew(NIL);

  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Width:= 100;
  FButton.Height:= 30;

  CenterControl(FButton, 400, 300);

  ExpectedX:= (400 - FButton.Width) / 2;
  ExpectedY:= (300 - FButton.Height) / 2;

  Assert.AreEqual(ExpectedX, FButton.Position.X, 0.01, 'X should be centered');
  Assert.AreEqual(ExpectedY, FButton.Position.Y, 0.01, 'Y should be centered');
end;


procedure TTestFmxCenterControl.TestCenterControlDims_NilCtrlRaisesAssertion;
begin
  Assert.WillRaise(
    procedure
    begin
      CenterControl(NIL, 400, 300);
    end,
    EAssertionFailed);
end;


procedure TTestFmxCenterControl.TestCenterControlDims_LargerThanParent_NegativePosition;
begin
  FTestForm:= TForm.CreateNew(NIL);

  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Width:= 500;   { Larger than parent width }
  FButton.Height:= 400;  { Larger than parent height }

  CenterControl(FButton, 200, 150);

  { Position should be negative when control is larger than parent }
  Assert.IsTrue(FButton.Position.X < 0, 'X should be negative when control wider than parent');
  Assert.IsTrue(FButton.Position.Y < 0, 'Y should be negative when control taller than parent');
end;


{ EnsureControlVisible (with parent) Tests }

procedure TTestFmxCenterControl.TestEnsureControlVisible_ControlInBounds;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 800;
  FTestForm.Height:= 600;

  FLayout:= TLayout.Create(FTestForm);
  FLayout.Parent:= FTestForm;
  FLayout.Width:= 400;
  FLayout.Height:= 300;

  FButton:= TButton.Create(FLayout);
  FButton.Parent:= FLayout;
  FButton.Position.X:= 100;
  FButton.Position.Y:= 100;
  FButton.Width:= 100;
  FButton.Height:= 30;

  EnsureControlVisible(FButton);

  Assert.AreEqual(Single(100), FButton.Position.X, 0.01, 'X should remain 100');
  Assert.AreEqual(Single(100), FButton.Position.Y, 0.01, 'Y should remain 100');
end;


procedure TTestFmxCenterControl.TestEnsureControlVisible_ControlOffLeft;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 800;
  FTestForm.Height:= 600;

  FLayout:= TLayout.Create(FTestForm);
  FLayout.Parent:= FTestForm;
  FLayout.Width:= 400;
  FLayout.Height:= 300;

  FButton:= TButton.Create(FLayout);
  FButton.Parent:= FLayout;
  FButton.Position.X:= -100;
  FButton.Position.Y:= 100;
  FButton.Width:= 100;
  FButton.Height:= 30;

  EnsureControlVisible(FButton);

  Assert.AreEqual(Single(0), FButton.Position.X, 0.01, 'X should be corrected to 0');
end;


procedure TTestFmxCenterControl.TestEnsureControlVisible_ControlOffTop;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 800;
  FTestForm.Height:= 600;

  FLayout:= TLayout.Create(FTestForm);
  FLayout.Parent:= FTestForm;
  FLayout.Width:= 400;
  FLayout.Height:= 300;

  FButton:= TButton.Create(FLayout);
  FButton.Parent:= FLayout;
  FButton.Position.X:= 100;
  FButton.Position.Y:= -100;
  FButton.Width:= 100;
  FButton.Height:= 30;

  EnsureControlVisible(FButton);

  Assert.AreEqual(Single(0), FButton.Position.Y, 0.01, 'Y should be corrected to 0');
end;


procedure TTestFmxCenterControl.TestEnsureControlVisible_NilCtrlRaisesAssertion;
begin
  Assert.WillRaise(
    procedure
    begin
      EnsureControlVisible(NIL);
    end,
    EAssertionFailed);
end;


procedure TTestFmxCenterControl.TestEnsureControlVisible_NoParentRaisesAssertion;
begin
  FButton:= TButton.Create(NIL);
  { FButton has no parent }

  Assert.WillRaise(
    procedure
    begin
      EnsureControlVisible(FButton);
    end,
    EAssertionFailed);
end;


{ EnsureControlVisible (with dimensions) Tests }

procedure TTestFmxCenterControl.TestEnsureControlVisibleDims_ControlInBounds;
begin
  FTestForm:= TForm.CreateNew(NIL);

  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Position.X:= 100;
  FButton.Position.Y:= 100;
  FButton.Width:= 100;
  FButton.Height:= 30;

  EnsureControlVisible(FButton, 800, 600);

  Assert.AreEqual(Single(100), FButton.Position.X, 0.01, 'X should remain 100');
  Assert.AreEqual(Single(100), FButton.Position.Y, 0.01, 'Y should remain 100');
end;


procedure TTestFmxCenterControl.TestEnsureControlVisibleDims_ControlOffRight;
begin
  FTestForm:= TForm.CreateNew(NIL);

  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Position.X:= 1000;
  FButton.Position.Y:= 100;
  FButton.Width:= 100;
  FButton.Height:= 30;

  EnsureControlVisible(FButton, 400, 300);

  { X should be clamped to (400 - 100) = 300 }
  Assert.AreEqual(Single(300), FButton.Position.X, 0.01, 'X should be clamped to parent width - control width');
end;


procedure TTestFmxCenterControl.TestEnsureControlVisibleDims_ControlOffBottom;
begin
  FTestForm:= TForm.CreateNew(NIL);

  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Position.X:= 100;
  FButton.Position.Y:= 1000;
  FButton.Width:= 100;
  FButton.Height:= 30;

  EnsureControlVisible(FButton, 400, 300);

  { Y should be clamped to (300 - 30) = 270 }
  Assert.AreEqual(Single(270), FButton.Position.Y, 0.01, 'Y should be clamped to parent height - control height');
end;


procedure TTestFmxCenterControl.TestEnsureControlVisibleDims_ControlLargerThanParent;
begin
  FTestForm:= TForm.CreateNew(NIL);

  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Position.X:= 50;
  FButton.Position.Y:= 50;
  FButton.Width:= 500;   { Larger than parent width }
  FButton.Height:= 400;  { Larger than parent height }

  EnsureControlVisible(FButton, 200, 150);

  { When control is larger than parent, position should be clamped to 0 }
  Assert.AreEqual(Single(0), FButton.Position.X, 0.01, 'X should be 0 when control wider than parent');
  Assert.AreEqual(Single(0), FButton.Position.Y, 0.01, 'Y should be 0 when control taller than parent');
end;


procedure TTestFmxCenterControl.TestEnsureControlVisibleDims_NilCtrlRaisesAssertion;
begin
  Assert.WillRaise(
    procedure
    begin
      EnsureControlVisible(NIL, 400, 300);
    end,
    EAssertionFailed);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFmxCenterControl);

end.
