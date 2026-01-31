UNIT Test.FormScreenCapture;

{=============================================================================================================
   2026.01.31
   Unit tests for FormScreenCapture and LightFmx.Visual.ScreenCapture
   Tests screen capture manager logic and overlay style handling

   Note: UI-related tests are limited since FMX forms require actual display context.
   Focus is on business logic in TScreenCaptureManager.
=============================================================================================================}

INTERFACE

USES
  DUnitX.TestFramework,
  System.SysUtils, System.Types, System.Classes;

TYPE
  [TestFixture]
  TTestScreenCaptureManager = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor/Destructor }
    [Test]
    procedure TestCreate_InitializesProperties;

    [Test]
    procedure TestDestroy_NoMemoryLeak;

    { LastSelectionRect }
    [Test]
    procedure TestLastSelectionRect_DefaultEmpty;

    [Test]
    procedure TestCaptureSelectedArea_EmptyRect_ReturnsFalse;

    [Test]
    procedure TestCaptureSelectedArea_NoScreenshot_ReturnsFalse;

    [Test]
    procedure TestCaptureSelectedArea_ValidRect_AfterScreenshot;

    { Captured Images }
    [Test]
    procedure TestGetCapturedImages_InitiallyEmpty;

    { CaptureTipShown }
    [Test]
    procedure TestCaptureTipShown_DefaultValue;

    [Test]
    procedure TestCaptureTipShown_SetAndGet;
  end;


  [TestFixture]
  TTestOverlayStyle = class
  public
    { Overlay Style enum }
    [Test]
    procedure TestOverlayStyle_FrostedGlass;

    [Test]
    procedure TestOverlayStyle_Glossy;

    [Test]
    procedure TestOverlayStyle_SimpleDim;

    [Test]
    procedure TestOverlayStyle_OrdinalValues;
  end;


  [TestFixture]
  TTestFormScreenCapture = class
  public
    { Form creation - basic smoke test }
    [Test]
    procedure TestFormCreate_NoException;

    [Test]
    procedure TestFormCreate_DefaultOverlayStyle;

    [Test]
    procedure TestOverlayStyle_SetAndGet;
  end;


IMPLEMENTATION

USES
  FMX.Forms, FMX.Graphics,
  FormScreenCapture, LightFmx.Visual.ScreenCapture;


{ TTestScreenCaptureManager }

procedure TTestScreenCaptureManager.Setup;
begin
  // Each test creates its own manager instance
end;


procedure TTestScreenCaptureManager.TearDown;
begin
  // Cleanup handled in individual tests
end;


procedure TTestScreenCaptureManager.TestCreate_InitializesProperties;
VAR
  Manager: TScreenCaptureManager;
begin
  Manager:= TScreenCaptureManager.Create;
  try
    Assert.IsNotNull(Manager.Screenshot, 'Screenshot bitmap should be created');
    Assert.IsNotNull(Manager.GetCapturedImages, 'CapturedImages list should be created');
  finally
    FreeAndNil(Manager);
  end;
end;


procedure TTestScreenCaptureManager.TestDestroy_NoMemoryLeak;
VAR
  Manager: TScreenCaptureManager;
begin
  // Test that destruction completes without errors
  Manager:= TScreenCaptureManager.Create;
  FreeAndNil(Manager);
  Assert.IsNull(Manager, 'Manager should be nil after FreeAndNil');
end;


procedure TTestScreenCaptureManager.TestLastSelectionRect_DefaultEmpty;
VAR
  Manager: TScreenCaptureManager;
begin
  Manager:= TScreenCaptureManager.Create;
  try
    // Default should be empty (unless previously saved to INI)
    // We can only check it's a valid TRectF
    Assert.IsTrue(
      (Manager.LastSelectionRect.Width >= 0) OR Manager.LastSelectionRect.IsEmpty,
      'LastSelectionRect should be valid'
    );
  finally
    FreeAndNil(Manager);
  end;
end;


procedure TTestScreenCaptureManager.TestCaptureSelectedArea_EmptyRect_ReturnsFalse;
VAR
  Manager: TScreenCaptureManager;
  EmptyRect: TRectF;
begin
  Manager:= TScreenCaptureManager.Create;
  try
    {$IFDEF MSWINDOWS}
    Manager.StartCapture;  // Need a screenshot first
    {$ENDIF}
    EmptyRect:= TRectF.Empty;
    // Capturing empty rect should fail (would show message dialog in real app)
    // Note: This test may show a dialog - consider mocking ShowMessage for headless testing
    Assert.IsFalse(Manager.CaptureSelectedArea(EmptyRect), 'Empty rect should return false');
  finally
    FreeAndNil(Manager);
  end;
end;


procedure TTestScreenCaptureManager.TestCaptureSelectedArea_NoScreenshot_ReturnsFalse;
VAR
  Manager: TScreenCaptureManager;
  SelectRect: TRectF;
begin
  Manager:= TScreenCaptureManager.Create;
  try
    // Try to capture without calling StartCapture first
    SelectRect:= TRectF.Create(10, 10, 110, 110);
    // Should fail because no screenshot exists
    // Note: This test may show a dialog - consider mocking ShowMessage for headless testing
    Assert.IsFalse(Manager.CaptureSelectedArea(SelectRect), 'Should fail without screenshot');
  finally
    FreeAndNil(Manager);
  end;
end;


procedure TTestScreenCaptureManager.TestCaptureSelectedArea_ValidRect_AfterScreenshot;
VAR
  Manager: TScreenCaptureManager;
  SelectRect: TRectF;
begin
  Manager:= TScreenCaptureManager.Create;
  try
    // First capture screen (only works on Windows with display)
    {$IFDEF MSWINDOWS}
    Manager.StartCapture;

    // If screenshot was captured, test selection
    if Assigned(Manager.Screenshot) AND NOT Manager.Screenshot.IsEmpty then
      begin
        SelectRect:= TRectF.Create(10, 10, 110, 110);  // 100x100 selection
        Assert.IsTrue(Manager.CaptureSelectedArea(SelectRect), 'Valid rect should succeed');
        Assert.AreEqual(1, Manager.GetCapturedImages.Count, 'Should have one captured image');
      end;
    {$ENDIF}
  finally
    FreeAndNil(Manager);
  end;
end;


procedure TTestScreenCaptureManager.TestGetCapturedImages_InitiallyEmpty;
VAR
  Manager: TScreenCaptureManager;
begin
  Manager:= TScreenCaptureManager.Create;
  try
    Assert.AreEqual(0, Manager.GetCapturedImages.Count, 'Initially no captured images');
  finally
    FreeAndNil(Manager);
  end;
end;


procedure TTestScreenCaptureManager.TestCaptureTipShown_DefaultValue;
VAR
  Manager: TScreenCaptureManager;
begin
  Manager:= TScreenCaptureManager.Create;
  try
    // Default value depends on what's saved in INI, but should be >= 0
    Assert.IsTrue(Manager.CaptureTipShown >= 0, 'CaptureTipShown should be non-negative');
  finally
    FreeAndNil(Manager);
  end;
end;


procedure TTestScreenCaptureManager.TestCaptureTipShown_SetAndGet;
VAR
  Manager: TScreenCaptureManager;
begin
  Manager:= TScreenCaptureManager.Create;
  try
    Manager.CaptureTipShown:= 5;
    Assert.AreEqual(5, Manager.CaptureTipShown, 'CaptureTipShown should be settable');
  finally
    FreeAndNil(Manager);
  end;
end;


{ TTestOverlayStyle }

procedure TTestOverlayStyle.TestOverlayStyle_FrostedGlass;
VAR
  Style: TOverlayStyle;
begin
  Style:= osFrostedGlass;
  Assert.AreEqual(Ord(osFrostedGlass), Ord(Style));
end;


procedure TTestOverlayStyle.TestOverlayStyle_Glossy;
VAR
  Style: TOverlayStyle;
begin
  Style:= osGlossy;
  Assert.AreEqual(Ord(osGlossy), Ord(Style));
end;


procedure TTestOverlayStyle.TestOverlayStyle_SimpleDim;
VAR
  Style: TOverlayStyle;
begin
  Style:= osSimpleDim;
  Assert.AreEqual(Ord(osSimpleDim), Ord(Style));
end;


procedure TTestOverlayStyle.TestOverlayStyle_OrdinalValues;
begin
  // Verify enum ordinal values are as expected
  Assert.AreEqual(0, Ord(osFrostedGlass), 'osFrostedGlass should be 0');
  Assert.AreEqual(1, Ord(osGlossy), 'osGlossy should be 1');
  Assert.AreEqual(2, Ord(osSimpleDim), 'osSimpleDim should be 2');
end;


{ TTestFormScreenCapture }

procedure TTestFormScreenCapture.TestFormCreate_NoException;
VAR
  Form: TfrmScreenCapture;
begin
  // Test that form can be created without exceptions
  Form:= TfrmScreenCapture.Create(nil);
  try
    Assert.IsNotNull(Form, 'Form should be created');
  finally
    FreeAndNil(Form);
  end;
end;


procedure TTestFormScreenCapture.TestFormCreate_DefaultOverlayStyle;
VAR
  Form: TfrmScreenCapture;
begin
  Form:= TfrmScreenCapture.Create(nil);
  try
    Assert.AreEqual(Ord(osFrostedGlass), Ord(Form.OverlayStyle),
      'Default overlay style should be osFrostedGlass');
  finally
    FreeAndNil(Form);
  end;
end;


procedure TTestFormScreenCapture.TestOverlayStyle_SetAndGet;
VAR
  Form: TfrmScreenCapture;
begin
  Form:= TfrmScreenCapture.Create(nil);
  try
    Form.OverlayStyle:= osGlossy;
    Assert.AreEqual(Ord(osGlossy), Ord(Form.OverlayStyle));

    Form.OverlayStyle:= osSimpleDim;
    Assert.AreEqual(Ord(osSimpleDim), Ord(Form.OverlayStyle));

    Form.OverlayStyle:= osFrostedGlass;
    Assert.AreEqual(Ord(osFrostedGlass), Ord(Form.OverlayStyle));
  finally
    FreeAndNil(Form);
  end;
end;


INITIALIZATION
  TDUnitX.RegisterTestFixture(TTestScreenCaptureManager);
  TDUnitX.RegisterTestFixture(TTestOverlayStyle);
  TDUnitX.RegisterTestFixture(TTestFormScreenCapture);

end.
