UNIT Test.FormScreenCapture;

{=============================================================================================================
   2026.10.07
   Unit tests for FormScreenCapture and LightFmx.Visual.ScreenCapture
   Tests screen capture manager logic, including its INI persistence

   Note: UI-related tests are limited since FMX forms require actual display context.
   Focus is on business logic in TScreenCaptureManager.
=============================================================================================================}

INTERFACE

USES
  DUnitX.TestFramework,
  System.SysUtils, System.Types, System.Classes;

TYPE
  { TScreenCaptureManager keeps 5 keys in section [ScreenCapture] of the application INI file.
    Setup saves them and removes them, so every test starts from "no INI values"; TearDown puts the
    saved values back, so the tests leave the INI file as they found it. }
  TSavedIniKeys = record
    Exists: array[0..4] of Boolean;
    Value : array[0..4] of string;
  end;

  [TestFixture]
  TTestScreenCaptureManager = class
  private
    FIniPath: string;
    FSaved: TSavedIniKeys;
    procedure DeleteIniKeys;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor/Destructor }
    [Test]
    procedure TestCreate_InitializesProperties;

    [Test]
    procedure TestDestroy_SavesCaptureTipShown;

    [Test]
    procedure TestCreate_LoadsLastSelectionRect;

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
  System.IniFiles,
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  FMX.Forms, FMX.Graphics,
  LightFmx.Common.IniFile,
  FormScreenCapture, LightFmx.Visual.ScreenCapture;

CONST
  IniSection = 'ScreenCapture';
  IniKeys: array[0..4] of string = ('CaptureTipShown', 'LastSelection_Left', 'LastSelection_Top', 'LastSelection_Right', 'LastSelection_Bottom');


{ TTestScreenCaptureManager }

procedure TTestScreenCaptureManager.DeleteIniKeys;
VAR
  Ini: TIniFile;
  i: Integer;
begin
  Ini:= TIniFile.Create(FIniPath);
  TRY
    for i:= Low(IniKeys) to High(IniKeys) DO
      Ini.DeleteKey(IniSection, IniKeys[i]);
  FINALLY
    FreeAndNil(Ini);
  END;
end;


procedure TTestScreenCaptureManager.Setup;
VAR
  AppIni: TIniFileApp;
  Ini: TIniFile;
  i: Integer;
begin
  { The same INI file the manager uses }
  AppIni:= TIniFileApp.Create(IniSection);
  TRY
    FIniPath:= AppIni.FileName;
  FINALLY
    FreeAndNil(AppIni);
  END;

  Ini:= TIniFile.Create(FIniPath);
  TRY
    for i:= Low(IniKeys) to High(IniKeys) DO
      begin
        FSaved.Exists[i]:= Ini.ValueExists(IniSection, IniKeys[i]);
        FSaved.Value[i] := Ini.ReadString(IniSection, IniKeys[i], '');
      end;
  FINALLY
    FreeAndNil(Ini);
  END;

  DeleteIniKeys;
end;


procedure TTestScreenCaptureManager.TearDown;
VAR
  Ini: TIniFile;
  i: Integer;
begin
  DeleteIniKeys;

  Ini:= TIniFile.Create(FIniPath);
  TRY
    for i:= Low(IniKeys) to High(IniKeys) DO
      if FSaved.Exists[i]
      then Ini.WriteString(IniSection, IniKeys[i], FSaved.Value[i]);
  FINALLY
    FreeAndNil(Ini);
  END;
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


procedure TTestScreenCaptureManager.TestDestroy_SavesCaptureTipShown;
VAR
  Manager: TScreenCaptureManager;
begin
  Manager:= TScreenCaptureManager.Create;
  Manager.CaptureTipShown:= 7;
  FreeAndNil(Manager);   // The destructor writes the counter to the INI file

  Manager:= TScreenCaptureManager.Create;
  try
    Assert.AreEqual(7, Manager.CaptureTipShown, 'A new manager must load the counter the old one saved on destroy');
  finally
    FreeAndNil(Manager);
  end;
end;


procedure TTestScreenCaptureManager.TestCreate_LoadsLastSelectionRect;
VAR
  Manager: TScreenCaptureManager;
  Ini: TIniFile;
begin
  Ini:= TIniFile.Create(FIniPath);
  TRY
    Ini.WriteFloat(IniSection, 'LastSelection_Left',   10);
    Ini.WriteFloat(IniSection, 'LastSelection_Top',    20);
    Ini.WriteFloat(IniSection, 'LastSelection_Right',  110);
    Ini.WriteFloat(IniSection, 'LastSelection_Bottom', 220);
  FINALLY
    FreeAndNil(Ini);
  END;

  Manager:= TScreenCaptureManager.Create;
  try
    Assert.AreEqual(Double(10),  Double(Manager.LastSelectionRect.Left),   0.001, 'Left');
    Assert.AreEqual(Double(20),  Double(Manager.LastSelectionRect.Top),    0.001, 'Top');
    Assert.AreEqual(Double(110), Double(Manager.LastSelectionRect.Right),  0.001, 'Right');
    Assert.AreEqual(Double(220), Double(Manager.LastSelectionRect.Bottom), 0.001, 'Bottom');
  finally
    FreeAndNil(Manager);
  end;
end;


procedure TTestScreenCaptureManager.TestLastSelectionRect_DefaultEmpty;
VAR
  Manager: TScreenCaptureManager;
  Ini: TIniFile;
begin
  { No keys in the INI file (Setup removed them): the rectangle is empty }
  Manager:= TScreenCaptureManager.Create;
  try
    Assert.IsTrue(Manager.LastSelectionRect.IsEmpty, 'With no INI values LastSelectionRect must be empty');
  finally
    FreeAndNil(Manager);
  end;

  { A stored rectangle with Right < Left is rejected and becomes empty too }
  Ini:= TIniFile.Create(FIniPath);
  TRY
    Ini.WriteFloat(IniSection, 'LastSelection_Left',   200);
    Ini.WriteFloat(IniSection, 'LastSelection_Top',    20);
    Ini.WriteFloat(IniSection, 'LastSelection_Right',  100);
    Ini.WriteFloat(IniSection, 'LastSelection_Bottom', 220);
  FINALLY
    FreeAndNil(Ini);
  END;

  Manager:= TScreenCaptureManager.Create;
  try
    Assert.IsTrue(Manager.LastSelectionRect.IsEmpty, 'A stored rectangle of negative width must be read as empty');
    Assert.AreEqual(Double(0), Double(Manager.LastSelectionRect.Left), 0.001, 'The rejected rectangle is TRectF.Empty');
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


{ Counts the pixels of Crop whose RGB differs from the pixel of Source at (X + OffsetX, Y + OffsetY).
  The alpha byte is left out: GDI leaves it 0 in a screen capture. }
function CountDifferentPixels(Source, Crop: FMX.Graphics.TBitmap; OffsetX, OffsetY: Integer): Integer;
VAR
  SrcData, CropData: TBitmapData;
  X, Y: Integer;
begin
  Result:= 0;
  Assert.IsTrue(Source.Map(TMapAccess.Read, SrcData), 'Source.Map failed');
  TRY
    Assert.IsTrue(Crop.Map(TMapAccess.Read, CropData), 'Crop.Map failed');
    TRY
      for Y:= 0 to Crop.Height - 1 do
        for X:= 0 to Crop.Width - 1 do
          if (SrcData.GetPixel(X + OffsetX, Y + OffsetY) AND $00FFFFFF) <> (CropData.GetPixel(X, Y) AND $00FFFFFF)
          then Inc(Result);
    FINALLY
      Crop.Unmap(CropData);
    END;
  FINALLY
    Source.Unmap(SrcData);
  END;
end;


procedure TTestScreenCaptureManager.TestCaptureSelectedArea_ValidRect_AfterScreenshot;
VAR
  Manager: TScreenCaptureManager;
  SelectRect: TRectF;
  Cropped: FMX.Graphics.TBitmap;
  {$IFDEF MSWINDOWS}
  ScreenW, ScreenH: Integer;
  {$ENDIF}
begin
  Manager:= TScreenCaptureManager.Create;
  try
    {$IFDEF MSWINDOWS}
    { StartCapture copies the primary screen, whose size Windows reports through GetSystemMetrics }
    ScreenW:= GetSystemMetrics(SM_CXSCREEN);
    ScreenH:= GetSystemMetrics(SM_CYSCREEN);
    Manager.StartCapture;

    { A monitor that sleeps or is unplugged can switch the display mode in the middle of the capture (seen once: 1920 -> 1680 wide) }
    if (ScreenW <> GetSystemMetrics(SM_CXSCREEN)) OR (ScreenH <> GetSystemMetrics(SM_CYSCREEN)) then
      begin
        Assert.Pass('The display mode changed during the capture');
        EXIT;
      end;

    Assert.IsFalse(Manager.Screenshot.IsEmpty, 'StartCapture must fill the screenshot');
    Assert.AreEqual(ScreenW, Manager.Screenshot.Width,  'Screenshot width = primary screen width');
    Assert.AreEqual(ScreenH, Manager.Screenshot.Height, 'Screenshot height = primary screen height');

    SelectRect:= TRectF.Create(10, 10, 110, 110);  // 100x100 selection
    Assert.IsTrue(Manager.CaptureSelectedArea(SelectRect), 'Valid rect should succeed');
    Assert.AreEqual(1, Manager.GetCapturedImages.Count, 'Should have one captured image');

    Cropped:= Manager.GetCapturedImages[0];
    Assert.AreEqual(100, Cropped.Width,  'The crop is as wide as the selection');
    Assert.AreEqual(100, Cropped.Height, 'The crop is as high as the selection');

    { The crop is the screenshot area that starts at (10, 10), pixel for pixel }
    Assert.AreEqual(0, CountDifferentPixels(Manager.Screenshot, Cropped, 10, 10), 'Pixels of the crop that differ from the screenshot at (10, 10)');

    Assert.AreEqual(Double(10),  Double(Manager.LastSelectionRect.Left),   0.001, 'LastSelectionRect.Left');
    Assert.AreEqual(Double(110), Double(Manager.LastSelectionRect.Bottom), 0.001, 'LastSelectionRect.Bottom');
    {$ELSE}
    Assert.Pass('StartCapture is tested on Windows only');
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
    { Setup removed the key from the INI file, so the default applies }
    Assert.AreEqual(0, Manager.CaptureTipShown, 'CaptureTipShown must default to 0 when the INI file has no value');
  finally
    FreeAndNil(Manager);
  end;
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

  { TTestFormScreenCapture is deliberately NOT registered. Its 3 tests call
    TfrmScreenCapture.Create(nil), which builds a real FMX form, and c:\Projects\CLAUDE.md says
    "No form tests" for this repository. The code is kept so the decision can be reversed with one
    line. Gabriel's call, 2026-09-04. The other fixture touches no form. }
  //TDUnitX.RegisterTestFixture(TTestFormScreenCapture);

end.
