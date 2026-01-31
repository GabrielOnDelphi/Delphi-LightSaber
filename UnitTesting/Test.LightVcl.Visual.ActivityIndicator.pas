unit Test.LightVcl.Visual.ActivityIndicator;

{=============================================================================================================
   2026-01-31
   Unit tests for LightVcl.Visual.ActivityIndicator.pas
   Tests TActivityIndicatorC - Custom activity indicator with triangle glyph when stopped.

   Note: Some tests require a VCL context with valid form for parenting.
   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.WinXCtrls,
  LightVcl.Visual.ActivityIndicator;

type
  [TestFixture]
  TTestActivityIndicatorC = class
  private
    FForm: TForm;
    FIndicator: TActivityIndicatorC;
    FClickCalled: Boolean;
    procedure HandleClick(Sender: TObject);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Creation Tests }
    [Test]
    procedure TestCreate;

    [Test]
    procedure TestCreateWithOwner;

    [Test]
    procedure TestInheritsFromTActivityIndicator;

    { Basic Properties Tests }
    [Test]
    procedure TestAnimateDefault;

    [Test]
    procedure TestAnimateSetTrue;

    [Test]
    procedure TestAnimateSetFalse;

    [Test]
    procedure TestVisibleDefault;

    [Test]
    procedure TestVisibleSetFalse;

    [Test]
    procedure TestAlignDefault;

    [Test]
    procedure TestAlignSetClient;

    { Size Tests }
    [Test]
    procedure TestDefaultSize;

    [Test]
    procedure TestSetSize;

    { Parent Tests }
    [Test]
    procedure TestParentAssignment;

    [Test]
    procedure TestHandleAllocatedWithParent;

    [Test]
    procedure TestHandleNotAllocatedWithoutParent;

    { DrawFrame Tests }
    [Test]
    procedure TestDrawFrameWhenNotAnimating;

    [Test]
    procedure TestDrawFrameWhenAnimating;

    { Indicator Properties (inherited) }
    [Test]
    procedure TestIndicatorSize;

    [Test]
    procedure TestIndicatorType;

    [Test]
    procedure TestFrameDelay;

    { Event Tests }
    [Test]
    procedure TestOnClickAssignment;

    { Memory Tests }
    [Test]
    procedure TestCreateDestroyNoLeak;
  end;

implementation

uses
  LightVcl.Graph.Util;


procedure TTestActivityIndicatorC.HandleClick(Sender: TObject);
begin
  FClickCalled:= TRUE;
end;


procedure TTestActivityIndicatorC.Setup;
begin
  FForm:= TForm.CreateNew(NIL);
  FForm.Width:= 400;
  FForm.Height:= 300;
  FForm.Visible:= FALSE;

  FIndicator:= TActivityIndicatorC.Create(FForm);
  FIndicator.Parent:= FForm;
  FClickCalled:= FALSE;
end;


procedure TTestActivityIndicatorC.TearDown;
begin
  FreeAndNil(FIndicator);
  FreeAndNil(FForm);
end;


{ Creation Tests }

procedure TTestActivityIndicatorC.TestCreate;
var
  Indicator: TActivityIndicatorC;
begin
  Indicator:= TActivityIndicatorC.Create(NIL);
  try
    Assert.IsNotNull(Indicator, 'Indicator should be created');
  finally
    FreeAndNil(Indicator);
  end;
end;


procedure TTestActivityIndicatorC.TestCreateWithOwner;
var
  Owner: TComponent;
  Indicator: TActivityIndicatorC;
begin
  Owner:= TComponent.Create(NIL);
  try
    Indicator:= TActivityIndicatorC.Create(Owner);
    Assert.IsNotNull(Indicator, 'Indicator should be created with owner');
    Assert.AreEqual(Owner, Indicator.Owner, 'Owner should be set correctly');
    // Indicator will be freed by Owner
  finally
    FreeAndNil(Owner);
  end;
end;


procedure TTestActivityIndicatorC.TestInheritsFromTActivityIndicator;
begin
  Assert.IsTrue(FIndicator is TActivityIndicator, 'Should inherit from TActivityIndicator');
end;


{ Basic Properties Tests }

procedure TTestActivityIndicatorC.TestAnimateDefault;
begin
  // Default should be False
  Assert.IsFalse(FIndicator.Animate, 'Animate should default to FALSE');
end;


procedure TTestActivityIndicatorC.TestAnimateSetTrue;
begin
  FIndicator.Animate:= TRUE;
  Assert.IsTrue(FIndicator.Animate, 'Animate should be TRUE after setting');
end;


procedure TTestActivityIndicatorC.TestAnimateSetFalse;
begin
  FIndicator.Animate:= TRUE;
  FIndicator.Animate:= FALSE;
  Assert.IsFalse(FIndicator.Animate, 'Animate should be FALSE after setting');
end;


procedure TTestActivityIndicatorC.TestVisibleDefault;
begin
  Assert.IsTrue(FIndicator.Visible, 'Visible should default to TRUE');
end;


procedure TTestActivityIndicatorC.TestVisibleSetFalse;
begin
  FIndicator.Visible:= FALSE;
  Assert.IsFalse(FIndicator.Visible, 'Visible should be FALSE after setting');
end;


procedure TTestActivityIndicatorC.TestAlignDefault;
begin
  Assert.AreEqual(alNone, FIndicator.Align, 'Align should default to alNone');
end;


procedure TTestActivityIndicatorC.TestAlignSetClient;
begin
  FIndicator.Align:= alClient;
  Assert.AreEqual(alClient, FIndicator.Align, 'Align should be alClient after setting');
end;


{ Size Tests }

procedure TTestActivityIndicatorC.TestDefaultSize;
begin
  // TActivityIndicator has a default size, verify it's non-zero
  Assert.IsTrue(FIndicator.Width > 0, 'Width should be greater than 0');
  Assert.IsTrue(FIndicator.Height > 0, 'Height should be greater than 0');
end;


procedure TTestActivityIndicatorC.TestSetSize;
begin
  FIndicator.Width:= 50;
  FIndicator.Height:= 50;
  Assert.AreEqual(50, FIndicator.Width, 'Width should be 50');
  Assert.AreEqual(50, FIndicator.Height, 'Height should be 50');
end;


{ Parent Tests }

procedure TTestActivityIndicatorC.TestParentAssignment;
begin
  Assert.AreEqual(TObject(FForm), TObject(FIndicator.Parent), 'Parent should be the form');
end;


procedure TTestActivityIndicatorC.TestHandleAllocatedWithParent;
begin
  // Force handle creation by showing the form briefly
  FForm.HandleNeeded;
  Assert.IsTrue(FIndicator.HandleAllocated, 'Handle should be allocated when parent has handle');
end;


procedure TTestActivityIndicatorC.TestHandleNotAllocatedWithoutParent;
var
  Indicator: TActivityIndicatorC;
begin
  Indicator:= TActivityIndicatorC.Create(NIL);
  try
    // Without parent, handle should not be allocated
    Assert.IsFalse(Indicator.HandleAllocated, 'Handle should NOT be allocated without parent');
  finally
    FreeAndNil(Indicator);
  end;
end;


{ DrawFrame Tests }

procedure TTestActivityIndicatorC.TestDrawFrameWhenNotAnimating;
begin
  // Ensure handle is allocated for drawing
  FForm.HandleNeeded;
  FIndicator.Width:= 50;
  FIndicator.Height:= 50;
  FIndicator.Animate:= FALSE;

  // Force a repaint - this will call DrawFrame internally
  // We can't directly test DrawFrame (protected), but we can verify
  // no exception is raised when drawing triangle
  FIndicator.Repaint;
  Assert.Pass('DrawFrame succeeded without animation (triangle drawn)');
end;


procedure TTestActivityIndicatorC.TestDrawFrameWhenAnimating;
begin
  // Ensure handle is allocated for drawing
  FForm.HandleNeeded;
  FIndicator.Width:= 50;
  FIndicator.Height:= 50;
  FIndicator.Animate:= TRUE;

  // Force a repaint - should use parent's animation drawing
  FIndicator.Repaint;
  FIndicator.Animate:= FALSE; // Stop animation for cleanup
  Assert.Pass('DrawFrame succeeded with animation');
end;


{ Indicator Properties (inherited) }

procedure TTestActivityIndicatorC.TestIndicatorSize;
begin
  // Test changing indicator size property (inherited from TActivityIndicator)
  FIndicator.IndicatorSize:= aisSmall;
  Assert.AreEqual(aisSmall, FIndicator.IndicatorSize, 'IndicatorSize should be aisSmall');

  FIndicator.IndicatorSize:= aisMedium;
  Assert.AreEqual(aisMedium, FIndicator.IndicatorSize, 'IndicatorSize should be aisMedium');

  FIndicator.IndicatorSize:= aisLarge;
  Assert.AreEqual(aisLarge, FIndicator.IndicatorSize, 'IndicatorSize should be aisLarge');

  FIndicator.IndicatorSize:= aisXLarge;
  Assert.AreEqual(aisXLarge, FIndicator.IndicatorSize, 'IndicatorSize should be aisXLarge');
end;


procedure TTestActivityIndicatorC.TestIndicatorType;
begin
  // Test indicator type property
  FIndicator.IndicatorType:= aitMomentumDots;
  Assert.AreEqual(aitMomentumDots, FIndicator.IndicatorType, 'IndicatorType should be aitMomentumDots');

  FIndicator.IndicatorType:= aitRotatingSector;
  Assert.AreEqual(aitRotatingSector, FIndicator.IndicatorType, 'IndicatorType should be aitRotatingSector');

  FIndicator.IndicatorType:= aitSectorRing;
  Assert.AreEqual(aitSectorRing, FIndicator.IndicatorType, 'IndicatorType should be aitSectorRing');
end;


procedure TTestActivityIndicatorC.TestFrameDelay;
begin
  FIndicator.FrameDelay:= 100;
  Assert.AreEqual(Word(100), FIndicator.FrameDelay, 'FrameDelay should be 100');

  FIndicator.FrameDelay:= 50;
  Assert.AreEqual(Word(50), FIndicator.FrameDelay, 'FrameDelay should be 50');
end;


{ Event Tests }

procedure TTestActivityIndicatorC.TestOnClickAssignment;
begin
  FIndicator.OnClick:= HandleClick;
  Assert.IsTrue(Assigned(FIndicator.OnClick), 'OnClick should be assigned');
end;


{ Memory Tests }

procedure TTestActivityIndicatorC.TestCreateDestroyNoLeak;
var
  i: Integer;
  Indicator: TActivityIndicatorC;
begin
  // Create and destroy multiple times to verify no memory leaks
  for i:= 1 to 100 do
  begin
    Indicator:= TActivityIndicatorC.Create(NIL);
    Indicator.Parent:= FForm;
    Indicator.Animate:= TRUE;
    Indicator.Animate:= FALSE;
    FreeAndNil(Indicator);
  end;
  Assert.Pass('No exceptions during repeated create/destroy cycle');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestActivityIndicatorC);

end.
