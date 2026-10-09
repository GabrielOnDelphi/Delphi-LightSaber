unit Test.LightVcl.Visual.Panel;

{=============================================================================================================
   2026.10.08
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

uses
  Vcl.Graphics,
  Vcl.ExtCtrls;


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

{ Paints FPanel into a bitmap of the same size. The caller frees the bitmap. }
function PaintPanel(Panel: TCubicPanel): TBitmap;
begin
  Panel.HandleNeeded;
  Result:= TBitmap.Create;
  Result.PixelFormat:= pf24bit;
  Result.SetSize(Panel.Width, Panel.Height);
  Panel.PaintTo(Result.Canvas.Handle, 0, 0);
end;


{ Height in pixels between the first and the last row that holds a pixel different from the background
  (read in the bottom-right corner, where no text is drawn). 0 when nothing was drawn. }
function InkSpan(BMP: TBitmap): Integer;
var
  x, y, FirstRow, LastRow: Integer;
  Bkg: TColor;
begin
  Bkg:= BMP.Canvas.Pixels[BMP.Width - 3, BMP.Height - 3];
  FirstRow:= -1;
  LastRow:= -1;
  for y:= 0 to BMP.Height - 1 do
    for x:= 5 to BMP.Width - 6 do
      if BMP.Canvas.Pixels[x, y] <> Bkg then
        begin
          if FirstRow < 0
          then FirstRow:= y;
          LastRow:= y;
          Break;
        end;

  if FirstRow < 0
  then EXIT(0);
  Result:= LastRow - FirstRow + 1;
end;


{ A caption about twice as wide as the 300 pixel panel, no bevel, a solid background }
procedure PrepareCaption(Panel: TCubicPanel);
begin
  Panel.BevelOuter:= bvNone;
  Panel.ParentBackground:= FALSE;
  Panel.Caption:= 'Alpha Beta Gamma Delta Epsilon Zeta Eta Theta Iota Kappa Lambda Mu Nu Xi Omicron Pi Rho Sigma Tau Upsilon Phi Chi Psi Omega Alpha Beta Gamma Delta Epsilon';
end;


{ Height of one line of text in the panel's font }
function LineHeight(Panel: TCubicPanel): Integer;
var
  BMP: TBitmap;
begin
  BMP:= TBitmap.Create;
  try
    BMP.Canvas.Font:= Panel.Font;
    Result:= BMP.Canvas.TextHeight('Ag');
  finally
    FreeAndNil(BMP);
  end;
end;


{ WordWrap TRUE: Paint breaks the long caption into several lines }
procedure TTestCubicPanel.TestWordWrap_SetTrue;
var
  BMP: TBitmap;
begin
  PrepareCaption(FPanel);
  FPanel.WordWrap:= FALSE;
  FPanel.WordWrap:= TRUE;

  BMP:= PaintPanel(FPanel);
  try
    Assert.IsTrue(InkSpan(BMP) > LineHeight(FPanel) * 3 DIV 2, 'The caption must be drawn on more than one line. Ink span: ' + IntToStr(InkSpan(BMP)));
  finally
    FreeAndNil(BMP);
  end;
end;


{ WordWrap FALSE: the VCL paints the caption on one line }
procedure TTestCubicPanel.TestWordWrap_SetFalse;
var
  BMP: TBitmap;
  Span: Integer;
begin
  PrepareCaption(FPanel);
  FPanel.WordWrap:= FALSE;

  BMP:= PaintPanel(FPanel);
  try
    Span:= InkSpan(BMP);
    Assert.IsTrue(Span > 0, 'The caption must be drawn');
    Assert.IsTrue(Span <= LineHeight(FPanel), 'The caption must be drawn on one line. Ink span: ' + IntToStr(Span));
  finally
    FreeAndNil(BMP);
  end;
end;


{ Gutter Property Tests }

{ Gutter > 0 draws a purple vertical line at x = Gutter }
procedure TTestCubicPanel.TestGutter_SetValue;
var
  BMP: TBitmap;
begin
  FPanel.BevelOuter:= bvNone;
  FPanel.ParentBackground:= FALSE;
  FPanel.Gutter:= 50;

  BMP:= PaintPanel(FPanel);
  try
    Assert.AreEqual(Integer(clPurple), Integer(BMP.Canvas.Pixels[50, 150]), 'The vertical gutter line must be purple');
    Assert.AreNotEqual(Integer(clPurple), Integer(BMP.Canvas.Pixels[150, 150]), 'Away from the gutter lines nothing is purple');
  finally
    FreeAndNil(BMP);
  end;
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
  Lbl1, LblLow: TLabel;
begin
  { The lower label is created first, so it is Controls[0]: a walk that returns Controls[0] gives the wrong control }
  LblLow:= TLabel.Create(FPanel);
  LblLow.Parent:= FPanel;
  LblLow.Top:= 200;

  Lbl1:= TLabel.Create(FPanel);
  Lbl1.Parent:= FPanel;
  Lbl1.Top:= 10;

  FPanel.ResetToFirstCtrl;
  var First1:= FPanel.NextControl;
  Assert.AreSame(TObject(Lbl1), TObject(First1), 'After the reset NextControl must return the topmost control');

  { Without a reset the walk would continue from Lbl1. LblLow starts far below Lbl1's bottom edge, so it would answer NIL. }
  FPanel.ResetToFirstCtrl;
  var First2:= FPanel.NextControl;
  Assert.AreSame(TObject(Lbl1), TObject(First2), 'ResetToFirstCtrl must restart the walk at the topmost control');
end;


procedure TTestCubicPanel.TestEnumerateControls_Order;
var
  Lbl1, Lbl2, Lbl3: TLabel;
  Ctrl: TControl;
  Order: TArray<TControl>;
begin
  // Create labels with overlapping positions to test the enumeration algorithm
  { AutoSize must go off on every label. TLabel autosizes by default, so TCustomLabel.AdjustBounds
    would shrink each one back to the height of its text (about 15 pixels) and the Height of 30
    below would never take effect. NextControl only walks to a control that starts at or above the
    previous control's bottom edge, so with 15-pixel labels at Top 10, 35 and 60 the walk stopped
    after the first one and the test found 1 control instead of 3. }
  Lbl1:= TLabel.Create(FPanel);
  Lbl1.Parent:= FPanel;
  Lbl1.AutoSize:= FALSE;
  Lbl1.Top:= 10;
  Lbl1.Height:= 30;
  Lbl1.Caption:= 'First';

  Lbl2:= TLabel.Create(FPanel);
  Lbl2.Parent:= FPanel;
  Lbl2.AutoSize:= FALSE;
  Lbl2.Top:= 35;  // Overlaps with Lbl1 (within its bottom edge)
  Lbl2.Height:= 30;
  Lbl2.Caption:= 'Second';

  Lbl3:= TLabel.Create(FPanel);
  Lbl3.Parent:= FPanel;
  Lbl3.AutoSize:= FALSE;
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
  Lbl1a, Lbl1b, Lbl2a, Lbl2b: TLabel;

  function AddLabel(Panel: TCubicPanel; Top: Integer): TLabel;
  begin
    Result:= TLabel.Create(Panel);
    Result.Parent:= Panel;
    Result.AutoSize:= FALSE;
    Result.Top:= Top;
    Result.Height:= 30;   { 30 pixels high, the next label starts 25 pixels lower: they overlap, so NextControl walks to it }
  end;

begin
  // Create second panel
  Panel2:= TCubicPanel.Create(FForm);
  Panel2.Parent:= FForm;
  Panel2.Left:= 350;
  Panel2.Top:= 10;
  Panel2.Width:= 100;
  Panel2.Height:= 100;

  try
    Lbl1a:= AddLabel(FPanel, 10);
    Lbl1b:= AddLabel(FPanel, 35);
    Lbl2a:= AddLabel(Panel2, 20);
    Lbl2b:= AddLabel(Panel2, 45);

    { The two walks are interleaved, with no reset in between: each panel must keep its own position }
    FPanel.ResetToFirstCtrl;
    Panel2.ResetToFirstCtrl;

    Assert.AreEqual(TControl(Lbl1a), FPanel.NextControl, 'Panel 1, step 1');
    Assert.AreEqual(TControl(Lbl2a), Panel2.NextControl, 'Panel 2, step 1 - must not continue from panel 1');
    Assert.AreEqual(TControl(Lbl1b), FPanel.NextControl, 'Panel 1, step 2 - must continue from its own label');
    Assert.AreEqual(TControl(Lbl2b), Panel2.NextControl, 'Panel 2, step 2');
    Assert.IsNull(TObject(FPanel.NextControl),'Panel 1 has no third control');

  finally
    FreeAndNil(Panel2);
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestCubicPanel);

end.
