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
  Control: TRectangle;
begin
  Control:= TRectangle.Create(nil);
  try
    Control.Width:= 400;
    Control.Height:= 300;
    // Should not raise exception
    TConfetti.ShowConfetti(Control, 1.0, 5);
    // Note: Confetti pieces are created as children of Control
    // They will be freed when Control is freed
  finally
    FreeAndNil(Control);
  end;
end;


procedure TTestConfetti.TestShowConfetti_ValidForm_NoException;
VAR
  Form: TForm;
begin
  Form:= TForm.Create(nil);
  try
    Form.ClientWidth:= 400;
    Form.ClientHeight:= 300;
    // Should not raise exception
    TConfetti.ShowConfetti(Form, 1.0, 5);
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
  Control: TRectangle;
begin
  Control:= TRectangle.Create(nil);
  try
    Control.Width:= 400;
    Control.Height:= 300;
    // Zero confetti should work without error (just creates nothing)
    TConfetti.ShowConfetti(Control, 1.0, 0);
  finally
    FreeAndNil(Control);
  end;
end;


procedure TTestConfetti.TestShowConfetti_LargeSizeMultiplier_NoException;
VAR
  Control: TRectangle;
begin
  Control:= TRectangle.Create(nil);
  try
    Control.Width:= 400;
    Control.Height:= 300;
    // Large multiplier should work
    TConfetti.ShowConfetti(Control, 5.0, 3);
  finally
    FreeAndNil(Control);
  end;
end;


procedure TTestConfetti.TestShowConfetti_SmallSizeMultiplier_NoException;
VAR
  Control: TRectangle;
begin
  Control:= TRectangle.Create(nil);
  try
    Control.Width:= 400;
    Control.Height:= 300;
    // Small multiplier should work
    TConfetti.ShowConfetti(Control, 0.5, 3);
  finally
    FreeAndNil(Control);
  end;
end;


INITIALIZATION
  TDUnitX.RegisterTestFixture(TTestConfetti);

end.
