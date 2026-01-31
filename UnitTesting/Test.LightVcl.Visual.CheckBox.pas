unit Test.LightVcl.Visual.CheckBox;

{=============================================================================================================
   Unit tests for LightVcl.Visual.CheckBox.pas
   Tests the TCubicCheckBox component - an auto-resizing checkbox.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ComCtrls;

type
  [TestFixture]
  TTestCubicCheckBox = class
  private
    FTestForm: TForm;
    FCheckBox: TCheckBox;  { Will be created as TCubicCheckBox }
    FPageControl: TPageControl;
    FTabSheet1: TTabSheet;
    FTabSheet2: TTabSheet;
    procedure CleanupControls;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor Tests }
    [Test]
    procedure TestCreate_AutoSizeDefaultFalse;

    [Test]
    procedure TestCreate_ValidOwner;

    [Test]
    procedure TestCreate_NilOwner;

    { AutoSize Property Tests }
    [Test]
    procedure TestAutoSize_SetTrue_AdjustsWidth;

    [Test]
    procedure TestAutoSize_SetFalse_NoAdjustment;

    [Test]
    procedure TestAutoSize_SetSameValue_NoChange;

    { Width Calculation Tests }
    [Test]
    procedure TestWidth_ShortCaption;

    [Test]
    procedure TestWidth_LongCaption;

    [Test]
    procedure TestWidth_EmptyCaption;

    [Test]
    procedure TestWidth_CaptionChange_WidthAdjusts;

    { Font Change Tests }
    [Test]
    procedure TestFontChange_WithAutoSize_AdjustsWidth;

    [Test]
    procedure TestFontChange_WithoutAutoSize_NoWidthChange;

    { PageControl Inactive Tab Tests }
    [Test]
    procedure TestPageControl_InactiveTab_StillWorks;

    { Loaded Tests }
    [Test]
    procedure TestLoaded_TriggersAdjustBounds;
  end;

implementation

uses
  Vcl.Graphics,
  LightVcl.Visual.CheckBox;


procedure TTestCubicCheckBox.Setup;
begin
  FTestForm:= NIL;
  FCheckBox:= NIL;
  FPageControl:= NIL;
  FTabSheet1:= NIL;
  FTabSheet2:= NIL;
end;


procedure TTestCubicCheckBox.TearDown;
begin
  CleanupControls;
end;


procedure TTestCubicCheckBox.CleanupControls;
begin
  { Child controls are freed by their parent }
  FreeAndNil(FTestForm);
  FCheckBox:= NIL;
  FPageControl:= NIL;
  FTabSheet1:= NIL;
  FTabSheet2:= NIL;
end;


{ Constructor Tests }

procedure TTestCubicCheckBox.TestCreate_AutoSizeDefaultFalse;
var
  CubicCheckBox: TCubicCheckBox;
begin
  FTestForm:= TForm.CreateNew(NIL);
  CubicCheckBox:= TCubicCheckBox.Create(FTestForm);
  CubicCheckBox.Parent:= FTestForm;

  Assert.IsFalse(CubicCheckBox.AutoSize, 'AutoSize should default to FALSE');
  FCheckBox:= CubicCheckBox;
end;


procedure TTestCubicCheckBox.TestCreate_ValidOwner;
var
  CubicCheckBox: TCubicCheckBox;
begin
  FTestForm:= TForm.CreateNew(NIL);
  CubicCheckBox:= TCubicCheckBox.Create(FTestForm);
  CubicCheckBox.Parent:= FTestForm;

  Assert.IsNotNull(CubicCheckBox, 'CheckBox should be created');
  Assert.AreEqual(FTestForm, CubicCheckBox.Owner, 'Owner should be set correctly');
  FCheckBox:= CubicCheckBox;
end;


procedure TTestCubicCheckBox.TestCreate_NilOwner;
var
  CubicCheckBox: TCubicCheckBox;
begin
  CubicCheckBox:= TCubicCheckBox.Create(NIL);
  try
    Assert.IsNotNull(CubicCheckBox, 'CheckBox should be created even with nil owner');
  finally
    FreeAndNil(CubicCheckBox);
  end;
end;


{ AutoSize Property Tests }

procedure TTestCubicCheckBox.TestAutoSize_SetTrue_AdjustsWidth;
var
  CubicCheckBox: TCubicCheckBox;
  OriginalWidth: Integer;
begin
  FTestForm:= TForm.CreateNew(NIL);
  CubicCheckBox:= TCubicCheckBox.Create(FTestForm);
  CubicCheckBox.Parent:= FTestForm;
  CubicCheckBox.Caption:= 'Test Caption';
  OriginalWidth:= CubicCheckBox.Width;

  CubicCheckBox.AutoSize:= TRUE;

  { Width should be adjusted to fit caption }
  Assert.AreNotEqual(OriginalWidth, CubicCheckBox.Width, 'Width should change when AutoSize is enabled');
  Assert.IsTrue(CubicCheckBox.Width > 0, 'Width should be positive');
  FCheckBox:= CubicCheckBox;
end;


procedure TTestCubicCheckBox.TestAutoSize_SetFalse_NoAdjustment;
var
  CubicCheckBox: TCubicCheckBox;
  OriginalWidth: Integer;
begin
  FTestForm:= TForm.CreateNew(NIL);
  CubicCheckBox:= TCubicCheckBox.Create(FTestForm);
  CubicCheckBox.Parent:= FTestForm;
  CubicCheckBox.Width:= 200;
  OriginalWidth:= CubicCheckBox.Width;
  CubicCheckBox.Caption:= 'Short';

  { AutoSize is already FALSE by default, setting it explicitly should not change width }
  CubicCheckBox.AutoSize:= FALSE;

  Assert.AreEqual(OriginalWidth, CubicCheckBox.Width, 'Width should not change when AutoSize is FALSE');
  FCheckBox:= CubicCheckBox;
end;


procedure TTestCubicCheckBox.TestAutoSize_SetSameValue_NoChange;
var
  CubicCheckBox: TCubicCheckBox;
begin
  FTestForm:= TForm.CreateNew(NIL);
  CubicCheckBox:= TCubicCheckBox.Create(FTestForm);
  CubicCheckBox.Parent:= FTestForm;
  CubicCheckBox.Caption:= 'Test';
  CubicCheckBox.AutoSize:= TRUE;
  var WidthAfterFirstSet:= CubicCheckBox.Width;

  { Setting same value again should not trigger adjustment }
  CubicCheckBox.AutoSize:= TRUE;

  Assert.AreEqual(WidthAfterFirstSet, CubicCheckBox.Width, 'Width should not change when setting same AutoSize value');
  FCheckBox:= CubicCheckBox;
end;


{ Width Calculation Tests }

procedure TTestCubicCheckBox.TestWidth_ShortCaption;
var
  CubicCheckBox: TCubicCheckBox;
begin
  FTestForm:= TForm.CreateNew(NIL);
  CubicCheckBox:= TCubicCheckBox.Create(FTestForm);
  CubicCheckBox.Parent:= FTestForm;
  CubicCheckBox.Caption:= 'OK';
  CubicCheckBox.AutoSize:= TRUE;

  { Width should be small for short caption }
  Assert.IsTrue(CubicCheckBox.Width < 100, 'Width should be small for short caption');
  Assert.IsTrue(CubicCheckBox.Width >= 21, 'Width should include checkbox indicator width');
  FCheckBox:= CubicCheckBox;
end;


procedure TTestCubicCheckBox.TestWidth_LongCaption;
var
  CubicCheckBox: TCubicCheckBox;
begin
  FTestForm:= TForm.CreateNew(NIL);
  CubicCheckBox:= TCubicCheckBox.Create(FTestForm);
  CubicCheckBox.Parent:= FTestForm;
  CubicCheckBox.Caption:= 'This is a very long caption that should make the checkbox quite wide';
  CubicCheckBox.AutoSize:= TRUE;

  { Width should be large for long caption }
  Assert.IsTrue(CubicCheckBox.Width > 200, 'Width should be large for long caption');
  FCheckBox:= CubicCheckBox;
end;


procedure TTestCubicCheckBox.TestWidth_EmptyCaption;
var
  CubicCheckBox: TCubicCheckBox;
begin
  FTestForm:= TForm.CreateNew(NIL);
  CubicCheckBox:= TCubicCheckBox.Create(FTestForm);
  CubicCheckBox.Parent:= FTestForm;
  CubicCheckBox.Caption:= '';
  CubicCheckBox.AutoSize:= TRUE;

  { Width should be minimal (just checkbox indicator + padding) }
  Assert.IsTrue(CubicCheckBox.Width >= 21, 'Width should include checkbox indicator width');
  Assert.IsTrue(CubicCheckBox.Width < 50, 'Width should be minimal for empty caption');
  FCheckBox:= CubicCheckBox;
end;


procedure TTestCubicCheckBox.TestWidth_CaptionChange_WidthAdjusts;
var
  CubicCheckBox: TCubicCheckBox;
  ShortCaptionWidth: Integer;
begin
  FTestForm:= TForm.CreateNew(NIL);
  CubicCheckBox:= TCubicCheckBox.Create(FTestForm);
  CubicCheckBox.Parent:= FTestForm;
  CubicCheckBox.Caption:= 'Short';
  CubicCheckBox.AutoSize:= TRUE;
  ShortCaptionWidth:= CubicCheckBox.Width;

  { Change caption to longer text }
  CubicCheckBox.Caption:= 'This is a much longer caption';

  Assert.IsTrue(CubicCheckBox.Width > ShortCaptionWidth, 'Width should increase when caption becomes longer');
  FCheckBox:= CubicCheckBox;
end;


{ Font Change Tests }

procedure TTestCubicCheckBox.TestFontChange_WithAutoSize_AdjustsWidth;
var
  CubicCheckBox: TCubicCheckBox;
  WidthWithSmallFont: Integer;
begin
  FTestForm:= TForm.CreateNew(NIL);
  CubicCheckBox:= TCubicCheckBox.Create(FTestForm);
  CubicCheckBox.Parent:= FTestForm;
  CubicCheckBox.Caption:= 'Test Caption';
  CubicCheckBox.Font.Size:= 8;
  CubicCheckBox.AutoSize:= TRUE;
  WidthWithSmallFont:= CubicCheckBox.Width;

  { Change to larger font }
  CubicCheckBox.Font.Size:= 24;

  Assert.IsTrue(CubicCheckBox.Width > WidthWithSmallFont, 'Width should increase with larger font');
  FCheckBox:= CubicCheckBox;
end;


procedure TTestCubicCheckBox.TestFontChange_WithoutAutoSize_NoWidthChange;
var
  CubicCheckBox: TCubicCheckBox;
  OriginalWidth: Integer;
begin
  FTestForm:= TForm.CreateNew(NIL);
  CubicCheckBox:= TCubicCheckBox.Create(FTestForm);
  CubicCheckBox.Parent:= FTestForm;
  CubicCheckBox.Caption:= 'Test Caption';
  CubicCheckBox.Width:= 150;
  OriginalWidth:= CubicCheckBox.Width;
  { AutoSize is FALSE by default }

  { Change font size }
  CubicCheckBox.Font.Size:= 24;

  Assert.AreEqual(OriginalWidth, CubicCheckBox.Width, 'Width should not change when AutoSize is FALSE');
  FCheckBox:= CubicCheckBox;
end;


{ PageControl Inactive Tab Tests }

procedure TTestCubicCheckBox.TestPageControl_InactiveTab_StillWorks;
var
  CubicCheckBox: TCubicCheckBox;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 500;
  FTestForm.Height:= 400;

  FPageControl:= TPageControl.Create(FTestForm);
  FPageControl.Parent:= FTestForm;
  FPageControl.Align:= alClient;

  FTabSheet1:= TTabSheet.Create(FPageControl);
  FTabSheet1.PageControl:= FPageControl;
  FTabSheet1.Caption:= 'Tab 1';

  FTabSheet2:= TTabSheet.Create(FPageControl);
  FTabSheet2.PageControl:= FPageControl;
  FTabSheet2.Caption:= 'Tab 2';

  { Place checkbox on the second (inactive) tab }
  CubicCheckBox:= TCubicCheckBox.Create(FTestForm);
  CubicCheckBox.Parent:= FTabSheet2;
  CubicCheckBox.Caption:= 'Checkbox on inactive tab';

  { Ensure first tab is active }
  FPageControl.ActivePage:= FTabSheet1;

  { Enable AutoSize - should work even on inactive tab }
  CubicCheckBox.AutoSize:= TRUE;

  { Width should still be calculated correctly }
  Assert.IsTrue(CubicCheckBox.Width > 50, 'AutoSize should work even when checkbox is on inactive tab');
  FCheckBox:= CubicCheckBox;
end;


{ Loaded Tests }

procedure TTestCubicCheckBox.TestLoaded_TriggersAdjustBounds;
var
  CubicCheckBox: TCubicCheckBox;
begin
  { This test verifies that the Loaded method triggers AdjustBounds.
    Since we can't easily simulate the streaming process, we verify that
    when AutoSize is TRUE before parent is set, the control still works correctly. }
  FTestForm:= TForm.CreateNew(NIL);
  CubicCheckBox:= TCubicCheckBox.Create(FTestForm);
  CubicCheckBox.Caption:= 'Test loaded behavior';
  CubicCheckBox.Parent:= FTestForm;
  CubicCheckBox.AutoSize:= TRUE;

  { The control should have been adjusted }
  Assert.IsTrue(CubicCheckBox.Width > 30, 'Control should be properly sized after Loaded');
  FCheckBox:= CubicCheckBox;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestCubicCheckBox);

end.
