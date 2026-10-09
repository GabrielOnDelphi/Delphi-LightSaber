unit Test.LightVcl.Visual.CheckBox;

{=============================================================================================================
   2026.10.08
   Unit tests for LightVcl.Visual.CheckBox.pas
   Tests the TLightCheckBox component - an auto-resizing checkbox.

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
  TTesTLightCheckBox = class
  private
    FTestForm: TForm;
    FCheckBox: TCheckBox;  { Will be created as TLightCheckBox }
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
  Vcl.Themes,
  LightVcl.Visual.CheckBox;

type
  TLightCheckBoxAccess = class(TLightCheckBox);   { Opens the protected Loaded }


{ The width AutoSize must give, built from three parts measured here and not by the control:
  the caption's text width, measured on a canvas of our own with the control's font;
  the checkbox glyph as the active theme reports it (the theme part tbCheckBoxUncheckedNormal, which
  c:\Delphi\Delphi 13\source\vcl\Vcl.CheckLst.pas:487 draws), or 13 pixels at 96 DPI with no theme;
  the 4-pixel gap at 96 DPI between glyph and text (GLYPH_GAP in LightVcl.Visual.CheckBox.pas). }
function ExpectedAutoWidth(CheckBox: TLightCheckBox): Integer;
var
  BMP: TBitmap;
  DPI, GlyphWidth: Integer;
  GlyphSize: TSize;
begin
  BMP:= TBitmap.Create;
  try
    BMP.Canvas.Font:= CheckBox.Font;
    DPI:= GetDeviceCaps(BMP.Canvas.Handle, LOGPIXELSX);
    if StyleServices.Enabled
    AND StyleServices.GetElementSize(BMP.Canvas.Handle, StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal), esActual, GlyphSize, DPI)
    AND (GlyphSize.Width > 0)
    then GlyphWidth:= GlyphSize.Width
    else GlyphWidth:= MulDiv(13, DPI, 96);
    Result:= BMP.Canvas.TextWidth(CheckBox.Caption) + GlyphWidth + MulDiv(4, DPI, 96);
  finally
    FreeAndNil(BMP);
  end;
end;


procedure TTesTLightCheckBox.Setup;
begin
  FTestForm:= NIL;
  FCheckBox:= NIL;
  FPageControl:= NIL;
  FTabSheet1:= NIL;
  FTabSheet2:= NIL;
end;


procedure TTesTLightCheckBox.TearDown;
begin
  CleanupControls;
end;


procedure TTesTLightCheckBox.CleanupControls;
begin
  { Child controls are freed by their parent }
  FreeAndNil(FTestForm);
  FCheckBox:= NIL;
  FPageControl:= NIL;
  FTabSheet1:= NIL;
  FTabSheet2:= NIL;
end;


{ Constructor Tests }

procedure TTesTLightCheckBox.TestCreate_AutoSizeDefaultFalse;
var
  CubicCheckBox: TLightCheckBox;
begin
  FTestForm:= TForm.CreateNew(NIL);
  CubicCheckBox:= TLightCheckBox.Create(FTestForm);
  CubicCheckBox.Parent:= FTestForm;

  Assert.IsFalse(CubicCheckBox.AutoSize, 'AutoSize should default to FALSE');
  FCheckBox:= CubicCheckBox;
end;


procedure TTesTLightCheckBox.TestCreate_ValidOwner;
var
  CubicCheckBox: TLightCheckBox;
begin
  FTestForm:= TForm.CreateNew(NIL);
  CubicCheckBox:= TLightCheckBox.Create(FTestForm);
  CubicCheckBox.Parent:= FTestForm;

  Assert.IsNotNull(CubicCheckBox, 'CheckBox should be created');
  { AreEqual is generic and cannot reconcile a TForm with a TComponent (E2532). AreSame takes
    two TObject and is the right question anyway: is the owner THIS form? }
  Assert.AreSame(TObject(FTestForm), TObject(CubicCheckBox.Owner), 'Owner should be set correctly');
  FCheckBox:= CubicCheckBox;
end;


{ AutoSize Property Tests }

procedure TTesTLightCheckBox.TestAutoSize_SetTrue_AdjustsWidth;
var
  CubicCheckBox: TLightCheckBox;
  OriginalWidth: Integer;
begin
  FTestForm:= TForm.CreateNew(NIL);
  CubicCheckBox:= TLightCheckBox.Create(FTestForm);
  CubicCheckBox.Parent:= FTestForm;
  CubicCheckBox.Caption:= 'Test Caption';
  OriginalWidth:= CubicCheckBox.Width;

  CubicCheckBox.AutoSize:= TRUE;

  { Width should be adjusted to fit caption }
  Assert.AreNotEqual(OriginalWidth, CubicCheckBox.Width, 'Width should change when AutoSize is enabled');
  Assert.AreEqual(ExpectedAutoWidth(CubicCheckBox), CubicCheckBox.Width, 'Width must be the caption text + the glyph + the gap');
  FCheckBox:= CubicCheckBox;
end;


procedure TTesTLightCheckBox.TestAutoSize_SetFalse_NoAdjustment;
var
  CubicCheckBox: TLightCheckBox;
  OriginalWidth: Integer;
begin
  FTestForm:= TForm.CreateNew(NIL);
  CubicCheckBox:= TLightCheckBox.Create(FTestForm);
  CubicCheckBox.Parent:= FTestForm;
  CubicCheckBox.Width:= 200;
  OriginalWidth:= CubicCheckBox.Width;
  CubicCheckBox.Caption:= 'Short';

  { AutoSize is already FALSE by default, setting it explicitly should not change width }
  CubicCheckBox.AutoSize:= FALSE;

  Assert.AreEqual(OriginalWidth, CubicCheckBox.Width, 'Width should not change when AutoSize is FALSE');
  FCheckBox:= CubicCheckBox;
end;


procedure TTesTLightCheckBox.TestAutoSize_SetSameValue_NoChange;
var
  CubicCheckBox: TLightCheckBox;
begin
  FTestForm:= TForm.CreateNew(NIL);
  CubicCheckBox:= TLightCheckBox.Create(FTestForm);
  CubicCheckBox.Parent:= FTestForm;
  CubicCheckBox.Caption:= 'Test';
  CubicCheckBox.AutoSize:= TRUE;

  { A width set by hand survives until the next caption or font change. Setting the same AutoSize value again must not run AdjustBounds. }
  CubicCheckBox.Width:= 500;
  CubicCheckBox.AutoSize:= TRUE;

  Assert.AreEqual(500, CubicCheckBox.Width, 'Setting the same AutoSize value must not re-run AdjustBounds');
  FCheckBox:= CubicCheckBox;
end;


{ Width Calculation Tests }

procedure TTesTLightCheckBox.TestWidth_ShortCaption;
var
  CubicCheckBox: TLightCheckBox;
begin
  FTestForm:= TForm.CreateNew(NIL);
  CubicCheckBox:= TLightCheckBox.Create(FTestForm);
  CubicCheckBox.Parent:= FTestForm;
  { A wide start, so only AdjustBounds can bring the width under 100 (the VCL default of 97 already is) }
  CubicCheckBox.Width:= 300;
  CubicCheckBox.Caption:= 'OK';
  Assert.AreEqual(300, CubicCheckBox.Width, 'Precondition: with AutoSize FALSE a caption change must not resize');
  CubicCheckBox.AutoSize:= TRUE;

  Assert.IsTrue(CubicCheckBox.Width < 100, 'Width should be small for short caption');
  Assert.IsTrue(CubicCheckBox.Width >= 21, 'Width should include checkbox indicator width');
  Assert.AreEqual(ExpectedAutoWidth(CubicCheckBox), CubicCheckBox.Width, 'Width must be the caption text + the glyph + the gap');
  FCheckBox:= CubicCheckBox;
end;


procedure TTesTLightCheckBox.TestWidth_LongCaption;
var
  CubicCheckBox: TLightCheckBox;
begin
  FTestForm:= TForm.CreateNew(NIL);
  CubicCheckBox:= TLightCheckBox.Create(FTestForm);
  CubicCheckBox.Parent:= FTestForm;
  CubicCheckBox.Caption:= 'This is a very long caption that should make the checkbox quite wide';
  CubicCheckBox.AutoSize:= TRUE;

  { Width should be large for long caption }
  Assert.IsTrue(CubicCheckBox.Width > 200, 'Width should be large for long caption');
  FCheckBox:= CubicCheckBox;
end;


procedure TTesTLightCheckBox.TestWidth_EmptyCaption;
var
  CubicCheckBox: TLightCheckBox;
begin
  FTestForm:= TForm.CreateNew(NIL);
  CubicCheckBox:= TLightCheckBox.Create(FTestForm);
  CubicCheckBox.Parent:= FTestForm;
  CubicCheckBox.Caption:= '';
  CubicCheckBox.AutoSize:= TRUE;

  { An empty caption gives Width = TextWidth('') + glyph + gap = 0 + glyph + 4
    (LightVcl.Visual.CheckBox.pas, AdjustBounds). The old lower bound of 21 was a guess and is
    above what the themed glyph actually measures here. The real floor is FALLBACK_GLYPH_WIDTH,
    which that unit documents as 13 at 96 dots per inch. }
  Assert.IsTrue(CubicCheckBox.Width >= 13, 'Width should include the checkbox glyph');
  Assert.IsTrue(CubicCheckBox.Width < 50, 'Width should be minimal for empty caption');
  FCheckBox:= CubicCheckBox;
end;


procedure TTesTLightCheckBox.TestWidth_CaptionChange_WidthAdjusts;
var
  CubicCheckBox: TLightCheckBox;
  ShortCaptionWidth: Integer;
begin
  FTestForm:= TForm.CreateNew(NIL);
  CubicCheckBox:= TLightCheckBox.Create(FTestForm);
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

procedure TTesTLightCheckBox.TestFontChange_WithAutoSize_AdjustsWidth;
var
  CubicCheckBox: TLightCheckBox;
  WidthWithSmallFont: Integer;
begin
  FTestForm:= TForm.CreateNew(NIL);
  CubicCheckBox:= TLightCheckBox.Create(FTestForm);
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


procedure TTesTLightCheckBox.TestFontChange_WithoutAutoSize_NoWidthChange;
var
  CubicCheckBox: TLightCheckBox;
  OriginalWidth: Integer;
begin
  FTestForm:= TForm.CreateNew(NIL);
  CubicCheckBox:= TLightCheckBox.Create(FTestForm);
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

procedure TTesTLightCheckBox.TestPageControl_InactiveTab_StillWorks;
var
  CubicCheckBox, RefCheckBox: TLightCheckBox;
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
  CubicCheckBox:= TLightCheckBox.Create(FTestForm);
  CubicCheckBox.Parent:= FTabSheet2;
  CubicCheckBox.Caption:= 'Checkbox on inactive tab';
  CubicCheckBox.Width:= 10;

  { The same caption on the form itself, as a reference }
  RefCheckBox:= TLightCheckBox.Create(FTestForm);
  RefCheckBox.Parent:= FTestForm;
  RefCheckBox.Caption:= CubicCheckBox.Caption;
  RefCheckBox.AutoSize:= TRUE;

  { Ensure first tab is active }
  FPageControl.ActivePage:= FTabSheet1;

  { Enable AutoSize - should work even on inactive tab }
  CubicCheckBox.AutoSize:= TRUE;

  Assert.AreNotEqual(10, CubicCheckBox.Width, 'AutoSize must resize the checkbox on the inactive tab');
  Assert.AreEqual(RefCheckBox.Width, CubicCheckBox.Width, 'The inactive tab must give the same width as the form');
  FCheckBox:= CubicCheckBox;
end;


{ Loaded Tests }

procedure TTesTLightCheckBox.TestLoaded_TriggersAdjustBounds;
var
  CubicCheckBox: TLightCheckBox;
  AutoWidth: Integer;
begin
  { Loaded (protected) is called through a cracker class: after streaming it must resize the control again. }
  FTestForm:= TForm.CreateNew(NIL);
  CubicCheckBox:= TLightCheckBox.Create(FTestForm);
  CubicCheckBox.Caption:= 'Test loaded behavior';
  CubicCheckBox.Parent:= FTestForm;
  CubicCheckBox.AutoSize:= TRUE;
  AutoWidth:= CubicCheckBox.Width;

  CubicCheckBox.Width:= 10;
  TLightCheckBoxAccess(CubicCheckBox).Loaded;

  Assert.AreEqual(AutoWidth, CubicCheckBox.Width, 'Loaded must run AdjustBounds');
  FCheckBox:= CubicCheckBox;
end;


initialization
  TDUnitX.RegisterTestFixture(TTesTLightCheckBox);

end.
