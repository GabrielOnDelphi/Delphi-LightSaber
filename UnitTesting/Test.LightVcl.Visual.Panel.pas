unit Test.LightVcl.Visual.Panel;

{=============================================================================================================
   Unit tests for LightVcl.Visual.Panel.pas
   Tests TCubicPanel word wrap and control enumeration functionality.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Controls,
  LightVcl.Visual.Panel;

type
  [TestFixture]
  TTestCubicPanel = class
  private
    FPanel: TCubicPanel;
    FForm: TForm;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor Tests }
    [Test]
    procedure TestCreate_DefaultWordWrap;

    [Test]
    procedure TestCreate_DefaultGutter;

    { WordWrap Property Tests }
    [Test]
    procedure TestWordWrap_SetTrue;

    [Test]
    procedure TestWordWrap_SetFalse;

    { Gutter Property Tests }
    [Test]
    procedure TestGutter_SetValue;

    { Control Enumeration Tests }
    [Test]
    procedure TestFirstControl_ReturnsTopmost;

    [Test]
    procedure TestFirstControl_NoControls_RaisesException;

    [Test]
    procedure TestLastControl_ReturnsBottommost;

    [Test]
    procedure TestLastControl_NoControls_RaisesException;

    [Test]
    procedure TestNextControl_NoControls_RaisesException;

    [Test]
    procedure TestResetToFirstCtrl;

    [Test]
    procedure TestEnumerateControls_Order;

    { Multiple Panel Instance Tests - Verify global variable fix }
    [Test]
    procedure TestMultiplePanels_IndependentEnumeration;
  end;

implementation


{ TTestCubicPanel }

procedure TTestCubicPanel.Setup;
begin
  FForm:= TForm.Create(nil);
  FForm.Width:= 400;
  FForm.Height:= 400;

  FPanel:= TCubicPanel.Create(FForm);
  FPanel.Parent:= FForm;
  FPanel.Left:= 10;
  FPanel.Top:= 10;
  FPanel.Width:= 300;
  FPanel.Height:= 300;
end;


procedure TTestCubicPanel.TearDown;
begin
  FreeAndNil(FPanel);
  FreeAndNil(FForm);
end;


{ Constructor Tests }

procedure TTestCubicPanel.TestCreate_DefaultWordWrap;
begin
  Assert.IsTrue(FPanel.WordWrap);
end;


procedure TTestCubicPanel.TestCreate_DefaultGutter;
begin
  Assert.AreEqual(0, FPanel.Gutter);
end;


{ WordWrap Property Tests }

procedure TTestCubicPanel.TestWordWrap_SetTrue;
begin
  FPanel.WordWrap:= FALSE;
  FPanel.WordWrap:= TRUE;
  Assert.IsTrue(FPanel.WordWrap);
end;


procedure TTestCubicPanel.TestWordWrap_SetFalse;
begin
  FPanel.WordWrap:= FALSE;
  Assert.IsFalse(FPanel.WordWrap);
end;


{ Gutter Property Tests }

procedure TTestCubicPanel.TestGutter_SetValue;
begin
  FPanel.Gutter:= 50;
  Assert.AreEqual(50, FPanel.Gutter);
end;


{ Control Enumeration Tests }

procedure TTestCubicPanel.TestFirstControl_ReturnsTopmost;
var
  Lbl1, Lbl2, Lbl3: TLabel;
begin
  Lbl1:= TLabel.Create(FPanel);
  Lbl1.Parent:= FPanel;
  Lbl1.Top:= 100;
  Lbl1.Caption:= 'Middle';

  Lbl2:= TLabel.Create(FPanel);
  Lbl2.Parent:= FPanel;
  Lbl2.Top:= 10;
  Lbl2.Caption:= 'Top';

  Lbl3:= TLabel.Create(FPanel);
  Lbl3.Parent:= FPanel;
  Lbl3.Top:= 200;
  Lbl3.Caption:= 'Bottom';

  FPanel.ResetToFirstCtrl;
  Assert.AreEqual(TControl(Lbl2), FPanel.FirstControl);
end;


procedure TTestCubicPanel.TestFirstControl_NoControls_RaisesException;
begin
  FPanel.ResetToFirstCtrl;
  Assert.WillRaise(
    procedure
    begin
      FPanel.FirstControl;
    end,
    Exception);
end;


procedure TTestCubicPanel.TestLastControl_ReturnsBottommost;
var
  Lbl1, Lbl2, Lbl3: TLabel;
begin
  Lbl1:= TLabel.Create(FPanel);
  Lbl1.Parent:= FPanel;
  Lbl1.Top:= 100;

  Lbl2:= TLabel.Create(FPanel);
  Lbl2.Parent:= FPanel;
  Lbl2.Top:= 10;

  Lbl3:= TLabel.Create(FPanel);
  Lbl3.Parent:= FPanel;
  Lbl3.Top:= 200;

  Assert.AreEqual(TControl(Lbl3), FPanel.LastControl);
end;


procedure TTestCubicPanel.TestLastControl_NoControls_RaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      FPanel.LastControl;
    end,
    Exception);
end;


procedure TTestCubicPanel.TestNextControl_NoControls_RaisesException;
begin
  FPanel.ResetToFirstCtrl;
  Assert.WillRaise(
    procedure
    begin
      FPanel.NextControl;
    end,
    Exception);
end;


procedure TTestCubicPanel.TestResetToFirstCtrl;
var
  Lbl1: TLabel;
begin
  Lbl1:= TLabel.Create(FPanel);
  Lbl1.Parent:= FPanel;
  Lbl1.Top:= 10;

  FPanel.ResetToFirstCtrl;
  var First1:= FPanel.NextControl;

  FPanel.ResetToFirstCtrl;
  var First2:= FPanel.NextControl;

  Assert.AreEqual(First1, First2, 'ResetToFirstCtrl should allow re-enumeration');
end;


procedure TTestCubicPanel.TestEnumerateControls_Order;
var
  Lbl1, Lbl2, Lbl3: TLabel;
  Ctrl: TControl;
  Order: TArray<TControl>;
begin
  // Create labels with overlapping positions to test the enumeration algorithm
  Lbl1:= TLabel.Create(FPanel);
  Lbl1.Parent:= FPanel;
  Lbl1.Top:= 10;
  Lbl1.Height:= 30;
  Lbl1.Caption:= 'First';

  Lbl2:= TLabel.Create(FPanel);
  Lbl2.Parent:= FPanel;
  Lbl2.Top:= 35;  // Overlaps with Lbl1 (within its bottom edge)
  Lbl2.Height:= 30;
  Lbl2.Caption:= 'Second';

  Lbl3:= TLabel.Create(FPanel);
  Lbl3.Parent:= FPanel;
  Lbl3.Top:= 60;  // Overlaps with Lbl2
  Lbl3.Height:= 30;
  Lbl3.Caption:= 'Third';

  FPanel.ResetToFirstCtrl;
  SetLength(Order, 0);

  Ctrl:= FPanel.NextControl;
  while Ctrl <> nil do
  begin
    SetLength(Order, Length(Order) + 1);
    Order[High(Order)]:= Ctrl;
    Ctrl:= FPanel.NextControl;
  end;

  Assert.AreEqual(3, Length(Order), 'Should enumerate all 3 controls');
  Assert.AreEqual(TControl(Lbl1), Order[0], 'First should be Lbl1');
  Assert.AreEqual(TControl(Lbl2), Order[1], 'Second should be Lbl2');
  Assert.AreEqual(TControl(Lbl3), Order[2], 'Third should be Lbl3');
end;


procedure TTestCubicPanel.TestMultiplePanels_IndependentEnumeration;
var
  Panel2: TCubicPanel;
  Lbl1, Lbl2: TLabel;
begin
  // Create second panel
  Panel2:= TCubicPanel.Create(FForm);
  Panel2.Parent:= FForm;
  Panel2.Left:= 350;
  Panel2.Top:= 10;
  Panel2.Width:= 100;
  Panel2.Height:= 100;

  try
    // Add label to first panel
    Lbl1:= TLabel.Create(FPanel);
    Lbl1.Parent:= FPanel;
    Lbl1.Top:= 10;

    // Add label to second panel
    Lbl2:= TLabel.Create(Panel2);
    Lbl2.Parent:= Panel2;
    Lbl2.Top:= 20;

    // Enumerate first panel
    FPanel.ResetToFirstCtrl;
    var Ctrl1:= FPanel.NextControl;
    Assert.AreEqual(TControl(Lbl1), Ctrl1, 'First panel should return its own control');

    // Enumerate second panel - should be independent
    Panel2.ResetToFirstCtrl;
    var Ctrl2:= Panel2.NextControl;
    Assert.AreEqual(TControl(Lbl2), Ctrl2, 'Second panel should return its own control');

    // First panel should still be able to enumerate independently
    FPanel.ResetToFirstCtrl;
    var Ctrl3:= FPanel.NextControl;
    Assert.AreEqual(TControl(Lbl1), Ctrl3, 'First panel enumeration should be independent');

  finally
    FreeAndNil(Panel2);
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestCubicPanel);

end.
