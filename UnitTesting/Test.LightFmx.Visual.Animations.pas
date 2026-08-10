UNIT Test.LightFmx.Visual.Animations;

{=============================================================================================================
   Unit tests for LightFmx.Visual.Animations
   Tests confetti animation logic

   Note: Animation visual behavior cannot be fully tested without running the UI.
   These tests focus on validation logic and basic creation.
=============================================================================================================}

INTERFACE

USES
  DUnitX.TestFramework,
  System.SysUtils, System.UITypes;

TYPE
  [TestFixture]
  TTestConfetti = class
  public
    { Parameter Validation }
    [Test]
    procedure TestShowConfetti_NilParent_RaisesAssertion;

    [Test]
    procedure TestShowConfetti_ValidControl_NoException;

    [Test]
    procedure TestShowConfetti_ValidForm_NoException;

    [Test]
    procedure TestShowConfetti_InvalidParent_RaisesException;

    { Size Multiplier }
    [Test]
    procedure TestShowConfetti_ZeroConfettiCount_NoException;

    [Test]
    procedure TestShowConfetti_LargeSizeMultiplier_NoException;

    [Test]
    procedure TestShowConfetti_SmallSizeMultiplier_NoException;
  end;


IMPLEMENTATION

USES
  FMX.Forms, FMX.Controls, FMX.Objects, FMX.Types,
  LightFmx.Visual.Animations;


{ TTestConfetti }

procedure TTestConfetti.TestShowConfetti_NilParent_RaisesAssertion;
begin
  Assert.WillRaise(
    procedure
    begin
      TConfetti.ShowConfetti(nil, 1.0, 10);
    end,
    EAssertionFailed
  );
end;


procedure TTestConfetti.TestShowConfetti_ValidControl_NoException;
VAR
  Form: TForm;
  Control: TRectangle;
  ChildrenBefore: Integer;
begin
  { The control MUST hang off a form. A parentless control has Root = NIL, and TAnimation.Start then treats
    the animation as "immediate": it runs it to completion and fires DoFinish synchronously (FMX.Ani.pas,
    the 'if (Abs(FDuration) < 0.001) or (Root = nil)' branch). TConfetti.OnAnimFinish detaches the piece,
    so every confetti would be gone again before this test could count it. }
  Form:= TForm.CreateNew(nil);
  try
    Control:= TRectangle.Create(Form);
    Control.Parent:= Form;
    Control.Width:= 400;
    Control.Height:= 300;
    ChildrenBefore:= Control.ChildrenCount;
    TConfetti.ShowConfetti(Control, 1.0, 5);
    Assert.AreEqual(ChildrenBefore + 5, Control.ChildrenCount, 'Should create 5 confetti children');
  finally
    FreeAndNil(Form);
  end;
end;


procedure TTestConfetti.TestShowConfetti_ValidForm_NoException;
VAR
  Form: TForm;
  ChildrenBefore: Integer;
begin
  Form:= TForm.CreateNew(nil);
  try
    Form.ClientWidth:= 400;
    Form.ClientHeight:= 300;
    ChildrenBefore:= Form.ChildrenCount;
    TConfetti.ShowConfetti(Form, 1.0, 5);
    Assert.AreEqual(ChildrenBefore + 5, Form.ChildrenCount, 'Should create 5 confetti children on form');
  finally
    FreeAndNil(Form);
  end;
end;


procedure TTestConfetti.TestShowConfetti_InvalidParent_RaisesException;
VAR
  InvalidParent: TFmxObject;
begin
  // TFmxObject is not TControl or TCommonCustomForm
  InvalidParent:= TFmxObject.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        TConfetti.ShowConfetti(InvalidParent, 1.0, 10);
      end,
      Exception
    );
  finally
    FreeAndNil(InvalidParent);
  end;
end;


procedure TTestConfetti.TestShowConfetti_ZeroConfettiCount_NoException;
VAR
  Form: TForm;
  Control: TRectangle;
  ChildrenBefore: Integer;
begin
  Form:= TForm.CreateNew(nil);
  try
    Control:= TRectangle.Create(Form);
    Control.Parent:= Form;
    Control.Width:= 400;
    Control.Height:= 300;
    ChildrenBefore:= Control.ChildrenCount;
    TConfetti.ShowConfetti(Control, 1.0, 0);
    Assert.AreEqual(ChildrenBefore, Control.ChildrenCount, 'Zero confetti should add no children');
  finally
    FreeAndNil(Form);
  end;
end;


procedure TTestConfetti.TestShowConfetti_LargeSizeMultiplier_NoException;
VAR
  Form: TForm;
  Control: TRectangle;
  ChildrenBefore: Integer;
begin
  Form:= TForm.CreateNew(nil);
  try
    Control:= TRectangle.Create(Form);
    Control.Parent:= Form;
    Control.Width:= 400;
    Control.Height:= 300;
    ChildrenBefore:= Control.ChildrenCount;
    TConfetti.ShowConfetti(Control, 5.0, 3);
    Assert.AreEqual(ChildrenBefore + 3, Control.ChildrenCount, 'Should create 3 confetti with large multiplier');
  finally
    FreeAndNil(Form);
  end;
end;


procedure TTestConfetti.TestShowConfetti_SmallSizeMultiplier_NoException;
VAR
  Form: TForm;
  Control: TRectangle;
  ChildrenBefore: Integer;
begin
  Form:= TForm.CreateNew(nil);
  try
    Control:= TRectangle.Create(Form);
    Control.Parent:= Form;
    Control.Width:= 400;
    Control.Height:= 300;
    ChildrenBefore:= Control.ChildrenCount;
    TConfetti.ShowConfetti(Control, 0.5, 3);
    Assert.AreEqual(ChildrenBefore + 3, Control.ChildrenCount, 'Should create 3 confetti with small multiplier');
  finally
    FreeAndNil(Form);
  end;
end;


INITIALIZATION
  TDUnitX.RegisterTestFixture(TTestConfetti);

end.
