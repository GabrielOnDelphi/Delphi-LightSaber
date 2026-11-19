unit Test.LightVcl.Visual.SpinEdit;

{=============================================================================================================
   Unit tests for LightVcl.Visual.SpinEdit.pas
   Tests the input validation of TCubicSpinEdit.Change: a typed value outside MinValue..MaxValue
   paints the box orange and does NOT fire OnChange. MinValue = MaxValue (both 0 by default) means "no range".
   The value is typed through Text, because TSpinEdit.GetValue reads the text and does not clamp it.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.Graphics,
  LightCore.Colors,
  LightVcl.Visual.SpinEdit;

type
  [TestFixture]
  TTestCubicSpinEditRange = class
  private
    FForm: TForm;
    FSpin: TCubicSpinEdit;
    FOnChangeCount: Integer;
    procedure OnChangeHandler(Sender: TObject);
    procedure TypeText(const aMin, aMax: Integer; const aText: string);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { MinValue is 0: the common case (an "amount" box) }
    [Test]
    procedure Test_MinZero_BelowMin_NoOnChange;
    [Test]
    procedure Test_MinZero_AboveMax_NoOnChange;
    [Test]
    procedure Test_MinZero_InRange_FiresOnChange;

    { MaxValue is 0 }
    [Test]
    procedure Test_MaxZero_AboveMax_NoOnChange;

    { Both limits set, none 0 }
    [Test]
    procedure Test_BothSet_BelowMin_NoOnChange;

    { No range }
    [Test]
    procedure Test_NoRange_AnyValue_FiresOnChange;
  end;


implementation


procedure TTestCubicSpinEditRange.OnChangeHandler(Sender: TObject);
begin
  Inc(FOnChangeCount);
end;


procedure TTestCubicSpinEditRange.Setup;
begin
  FForm:= TForm.CreateNew(NIL);
  FSpin:= TCubicSpinEdit.Create(FForm);
  FSpin.Parent:= FForm;
  FSpin.OnChange:= OnChangeHandler;
  FOnChangeCount:= 0;
end;


procedure TTestCubicSpinEditRange.TearDown;
begin
  FreeAndNil(FForm);   { Owns FSpin }
end;


{ Sets the range, then types aText. The counter counts only the OnChange calls of the typed text. }
procedure TTestCubicSpinEditRange.TypeText(const aMin, aMax: Integer; const aText: string);
begin
  FSpin.MinValue:= aMin;
  FSpin.MaxValue:= aMax;
  FOnChangeCount:= 0;
  FSpin.Text:= aText;
end;


procedure TTestCubicSpinEditRange.Test_MinZero_BelowMin_NoOnChange;
begin
  TypeText(0, 60, '-5');
  Assert.AreEqual(0, FOnChangeCount, '-5 is below MinValue 0: OnChange must not fire');
  Assert.AreEqual(Integer(clOrange), Integer(FSpin.Color), 'An out-of-range value paints the box orange');
end;


procedure TTestCubicSpinEditRange.Test_MinZero_AboveMax_NoOnChange;
begin
  TypeText(0, 60, '75');
  Assert.AreEqual(0, FOnChangeCount, '75 is above MaxValue 60: OnChange must not fire');
  Assert.AreEqual(Integer(clOrange), Integer(FSpin.Color));
end;


procedure TTestCubicSpinEditRange.Test_MinZero_InRange_FiresOnChange;
begin
  TypeText(0, 60, '30');
  Assert.IsTrue(FOnChangeCount > 0, '30 is inside 0..60: OnChange must fire');
  Assert.AreEqual(Integer(clWindow), Integer(FSpin.Color));
end;


procedure TTestCubicSpinEditRange.Test_MaxZero_AboveMax_NoOnChange;
begin
  TypeText(-10, 0, '5');
  Assert.AreEqual(0, FOnChangeCount, '5 is above MaxValue 0: OnChange must not fire');
end;


procedure TTestCubicSpinEditRange.Test_BothSet_BelowMin_NoOnChange;
begin
  TypeText(1, 10, '-3');
  Assert.AreEqual(0, FOnChangeCount, '-3 is below MinValue 1: OnChange must not fire');
end;


procedure TTestCubicSpinEditRange.Test_NoRange_AnyValue_FiresOnChange;
begin
  TypeText(0, 0, '-5');
  Assert.IsTrue(FOnChangeCount > 0, 'MinValue = MaxValue = 0 means no range: OnChange must fire');
  Assert.AreEqual(Integer(clWindow), Integer(FSpin.Color));
end;


initialization
  TDUnitX.RegisterTestFixture(TTestCubicSpinEditRange);

end.
