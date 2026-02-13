unit Test.FormDrawing;

{=============================================================================================================
   Unit tests for FormDrawing.pas
   Tests TDrawingForm - the desktop background painting form.

   Note: Most tests focus on form creation and property initialization.
   The initPaintingBkgWnd function requires Windows desktop access and
   is only tested for basic error handling.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ExtCtrls;

type
  [TestFixture]
  TTestFormDrawing = class
  private
    FTestForm: TObject;  // Using TObject to avoid circular reference
    procedure CleanupForm;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constants Tests }
    [Test]
    procedure TestConstant_TagDontTranslate;

    [Test]
    procedure TestConstant_MaxWorkerWSearch;

    { Form Creation Tests }
    [Test]
    procedure TestFormClassExists;

    [Test]
    procedure TestFormCreate_Succeeds;

    [Test]
    procedure TestFormCreate_NilOwner;

    { Constructor Property Tests }
    [Test]
    procedure TestConstructor_SetsVisible;

    [Test]
    procedure TestConstructor_SetsBorderStyle;

    [Test]
    procedure TestConstructor_SetsBorderIcons;

    [Test]
    procedure TestConstructor_SetsDoubleBuffered;

    [Test]
    procedure TestConstructor_SetsCaption;

    [Test]
    procedure TestConstructor_SetsName;

    [Test]
    procedure TestConstructor_SetsTag;

    [Test]
    procedure TestConstructor_SetsStyleElements;

    { Component Tests }
    [Test]
    procedure TestFormHasVideoPanel;

    { ExpandOnAllMon Property Tests }
    [Test]
    procedure TestExpandOnAllMon_DefaultValue;

    [Test]
    procedure TestExpandOnAllMon_SetTrue;

    [Test]
    procedure TestExpandOnAllMon_SetFalse;

    { ClearBkg Tests }
    [Test]
    procedure TestClearBkg_NoException;

    [Test]
    procedure TestClearBkg_UsesFormColor;

    { HidePlayer Tests }
    [Test]
    procedure TestHidePlayer_MovesOffScreen;

    [Test]
    procedure TestHidePlayer_PanelExists;

    { ShowPlayer Tests }
    [Test]
    procedure TestShowPlayer_SetsVisible;

    [Test]
    procedure TestShowPlayer_SetsPosition;

    [Test]
    procedure TestShowPlayer_WithZeroRect;

    { Repair Tests }
    [Test]
    procedure TestRepair_NoException;

    { GetHiddenWindow Function Tests }
    [Test]
    procedure TestGetHiddenWindow_ReturnsForm;

    [Test]
    procedure TestGetHiddenWindow_SetsColor;

    [Test]
    procedure TestGetHiddenWindow_SetsExpandOnAllMon;
  end;

implementation

uses
  LightCore.AppData,
  LightVcl.Visual.AppData,
  FormDrawing;


procedure TTestFormDrawing.Setup;
begin
  // Ensure AppData is initialized for logging
  Assert.IsNotNull(AppData, 'AppData must be initialized before running tests');
  FTestForm:= NIL;
end;


procedure TTestFormDrawing.TearDown;
begin
  CleanupForm;
end;


procedure TTestFormDrawing.CleanupForm;
var
  Form: TDrawingForm;
begin
  if FTestForm <> NIL then
    begin
      Form:= TDrawingForm(FTestForm);
      FreeAndNil(Form);
      FTestForm:= NIL;
    end;
end;


{ Constants Tests }

procedure TTestFormDrawing.TestConstant_TagDontTranslate;
begin
  Assert.AreEqual(128, TAG_DONT_TRANSLATE,
    'TAG_DONT_TRANSLATE should be 128 for compatibility with translator');
end;


procedure TTestFormDrawing.TestConstant_MaxWorkerWSearch;
begin
  Assert.IsTrue(MAX_WORKERW_SEARCH > 0, 'MAX_WORKERW_SEARCH should be positive');
  Assert.AreEqual(100, MAX_WORKERW_SEARCH, 'MAX_WORKERW_SEARCH should be 100');
end;


{ Form Creation Tests }

procedure TTestFormDrawing.TestFormClassExists;
begin
  Assert.IsNotNull(TDrawingForm, 'TDrawingForm class should exist');
end;


procedure TTestFormDrawing.TestFormCreate_Succeeds;
var
  Form: TDrawingForm;
begin
  Form:= TDrawingForm.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form, 'Form creation should succeed');
end;


procedure TTestFormDrawing.TestFormCreate_NilOwner;
var
  Form: TDrawingForm;
begin
  Form:= TDrawingForm.Create(NIL);
  FTestForm:= Form;

  Assert.IsNull(Form.Owner, 'Owner should be nil when created with nil');
end;


{ Constructor Property Tests }

procedure TTestFormDrawing.TestConstructor_SetsVisible;
var
  Form: TDrawingForm;
begin
  Form:= TDrawingForm.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.Visible, 'Form should be visible after creation');
end;


procedure TTestFormDrawing.TestConstructor_SetsBorderStyle;
var
  Form: TDrawingForm;
begin
  Form:= TDrawingForm.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual(bsNone, Form.BorderStyle, 'BorderStyle should be bsNone');
end;


procedure TTestFormDrawing.TestConstructor_SetsBorderIcons;
var
  Form: TDrawingForm;
begin
  Form:= TDrawingForm.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.BorderIcons = [], 'BorderIcons should be empty');
end;


procedure TTestFormDrawing.TestConstructor_SetsDoubleBuffered;
var
  Form: TDrawingForm;
begin
  Form:= TDrawingForm.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.DoubleBuffered, 'DoubleBuffered should be True');
end;


procedure TTestFormDrawing.TestConstructor_SetsCaption;
var
  Form: TDrawingForm;
begin
  Form:= TDrawingForm.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual('frmBxDraw', Form.Caption, 'Caption should be frmBxDraw');
end;


procedure TTestFormDrawing.TestConstructor_SetsName;
var
  Form: TDrawingForm;
begin
  Form:= TDrawingForm.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual('frmBxDraw', Form.Name, 'Name should be frmBxDraw');
end;


procedure TTestFormDrawing.TestConstructor_SetsTag;
var
  Form: TDrawingForm;
begin
  Form:= TDrawingForm.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual(TAG_DONT_TRANSLATE, Form.Tag, 'Tag should be TAG_DONT_TRANSLATE');
end;


procedure TTestFormDrawing.TestConstructor_SetsStyleElements;
var
  Form: TDrawingForm;
begin
  Form:= TDrawingForm.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.StyleElements = [], 'StyleElements should be empty (skins disabled)');
end;


{ Component Tests }

procedure TTestFormDrawing.TestFormHasVideoPanel;
var
  Form: TDrawingForm;
begin
  Form:= TDrawingForm.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.pnlVideoDisplay, 'Form should have pnlVideoDisplay panel');
  Assert.IsTrue(Form.pnlVideoDisplay is TPanel, 'pnlVideoDisplay should be a TPanel');
end;


{ ExpandOnAllMon Property Tests }

procedure TTestFormDrawing.TestExpandOnAllMon_DefaultValue;
var
  Form: TDrawingForm;
begin
  Form:= TDrawingForm.Create(NIL);
  FTestForm:= Form;

  // Default value should be False (Boolean default)
  Assert.IsFalse(Form.ExpandOnAllMon, 'ExpandOnAllMon should default to False');
end;


procedure TTestFormDrawing.TestExpandOnAllMon_SetTrue;
var
  Form: TDrawingForm;
begin
  Form:= TDrawingForm.Create(NIL);
  FTestForm:= Form;

  Form.ExpandOnAllMon:= TRUE;
  Assert.IsTrue(Form.ExpandOnAllMon, 'ExpandOnAllMon should be settable to True');
end;


procedure TTestFormDrawing.TestExpandOnAllMon_SetFalse;
var
  Form: TDrawingForm;
begin
  Form:= TDrawingForm.Create(NIL);
  FTestForm:= Form;

  Form.ExpandOnAllMon:= TRUE;
  Form.ExpandOnAllMon:= FALSE;
  Assert.IsFalse(Form.ExpandOnAllMon, 'ExpandOnAllMon should be settable to False');
end;


{ ClearBkg Tests }

procedure TTestFormDrawing.TestClearBkg_NoException;
var
  Form: TDrawingForm;
begin
  Form:= TDrawingForm.Create(NIL);
  FTestForm:= Form;

  { ClearBkg should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      Form.ClearBkg;
    end);
end;


procedure TTestFormDrawing.TestClearBkg_UsesFormColor;
var
  Form: TDrawingForm;
begin
  Form:= TDrawingForm.Create(NIL);
  FTestForm:= Form;

  Form.Color:= clRed;
  Form.ClearBkg;

  // After ClearBkg, the canvas brush should have the form's color
  Assert.AreEqual(clRed, TColor(Form.Canvas.Brush.Color),
    'ClearBkg should use form Color for brush');
end;


{ HidePlayer Tests }

procedure TTestFormDrawing.TestHidePlayer_MovesOffScreen;
var
  Form: TDrawingForm;
begin
  Form:= TDrawingForm.Create(NIL);
  FTestForm:= Form;

  Form.pnlVideoDisplay.Left:= 100;
  Form.HidePlayer;

  Assert.IsTrue(Form.pnlVideoDisplay.Left >= 20000,
    'HidePlayer should move panel far off-screen');
end;


procedure TTestFormDrawing.TestHidePlayer_PanelExists;
var
  Form: TDrawingForm;
begin
  Form:= TDrawingForm.Create(NIL);
  FTestForm:= Form;

  Form.HidePlayer;

  Assert.IsNotNull(Form.pnlVideoDisplay, 'Panel should still exist after HidePlayer');
end;


{ ShowPlayer Tests }

procedure TTestFormDrawing.TestShowPlayer_SetsVisible;
var
  Form: TDrawingForm;
  ViewRect: TRect;
begin
  Form:= TDrawingForm.Create(NIL);
  FTestForm:= Form;

  Form.pnlVideoDisplay.Visible:= FALSE;

  ViewRect:= Rect(100, 100, 500, 400);
  Form.ShowPlayer(ViewRect);

  Assert.IsTrue(Form.pnlVideoDisplay.Visible, 'ShowPlayer should set Visible to True');
end;


procedure TTestFormDrawing.TestShowPlayer_SetsPosition;
var
  Form: TDrawingForm;
  ViewRect: TRect;
begin
  Form:= TDrawingForm.Create(NIL);
  FTestForm:= Form;

  ViewRect:= Rect(150, 100, 500, 400);
  Form.ShowPlayer(ViewRect);

  Assert.AreEqual(150, Form.pnlVideoDisplay.Left,
    'ShowPlayer should set Left from ViewRect');
end;


procedure TTestFormDrawing.TestShowPlayer_WithZeroRect;
var
  Form: TDrawingForm;
  ViewRect: TRect;
begin
  Form:= TDrawingForm.Create(NIL);
  FTestForm:= Form;

  ViewRect:= Rect(0, 0, 0, 0);
  Form.ShowPlayer(ViewRect);

  Assert.AreEqual(0, Form.pnlVideoDisplay.Left,
    'ShowPlayer should handle zero rect');
  Assert.IsTrue(Form.pnlVideoDisplay.Visible,
    'ShowPlayer should set visible even with zero rect');
end;


{ Repair Tests }

procedure TTestFormDrawing.TestRepair_NoException;
var
  Form: TDrawingForm;
begin
  Form:= TDrawingForm.Create(NIL);
  FTestForm:= Form;

  { Repair is currently a no-op, but should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      Form.Repair;
    end);
end;


{ GetHiddenWindow Function Tests }

procedure TTestFormDrawing.TestGetHiddenWindow_ReturnsForm;
var
  Form: TDrawingForm;
begin
  Form:= TDrawingForm.GetHiddenWindow(clBlack, FALSE);
  FTestForm:= Form;

  Assert.IsNotNull(Form, 'GetHiddenWindow should return a form');
  Assert.IsTrue(Form is TDrawingForm, 'Returned form should be TDrawingForm');
end;


procedure TTestFormDrawing.TestGetHiddenWindow_SetsColor;
var
  Form: TDrawingForm;
begin
  Form:= TDrawingForm.GetHiddenWindow(clNavy, FALSE);
  FTestForm:= Form;

  Assert.AreEqual(clNavy, TColor(Form.Color), 'GetHiddenWindow should set the color');
end;


procedure TTestFormDrawing.TestGetHiddenWindow_SetsExpandOnAllMon;
var
  Form1, Form2: TDrawingForm;
begin
  Form1:= TDrawingForm.GetHiddenWindow(clBlack, TRUE);
  FTestForm:= Form1;

  Assert.IsTrue(Form1.ExpandOnAllMon, 'ExpandOnAllMon should be True when requested');

  // Clean up first form before creating second
  FreeAndNil(Form1);
  FTestForm:= NIL;

  Form2:= TDrawingForm.GetHiddenWindow(clBlack, FALSE);
  FTestForm:= Form2;

  Assert.IsFalse(Form2.ExpandOnAllMon, 'ExpandOnAllMon should be False when not requested');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFormDrawing);

end.
