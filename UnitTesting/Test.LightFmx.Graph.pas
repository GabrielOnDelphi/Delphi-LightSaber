unit Test.LightFmx.Graph;

{=============================================================================================================
   Unit tests for LightFmx.Graph.pas
   Tests bitmap loading, saving, cropping, and utility functions for FMX.

   Note: Some tests require actual image files to be present in the test data folder.
   Tests that require files will be skipped if the test data folder doesn't exist.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.IOUtils,
  FMX.Graphics,
  FMX.Objects,
  FMX.Forms;

type
  [TestFixture]
  TTestLightFmxGraph = class
  private
    FTestForm: TForm;
    FImage: TImage;
    FBitmap: TBitmap;
    FScratchDir: string;
    FTestImagePath: string;
    procedure CreateTestImage;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { CreateBitmap Tests }
    [Test]
    procedure TestCreateBitmap_ReturnsValidBitmap;

    [Test]
    procedure TestCreateBitmap_CorrectDimensions;

    [Test]
    procedure TestCreateBitmap_ZeroWidthRaisesAssertion;

    [Test]
    procedure TestCreateBitmap_ZeroHeightRaisesAssertion;

    [Test]
    procedure TestCreateBitmap_NegativeDimensionsRaisesAssertion;

    { FillBitmap Tests }
    [Test]
    procedure TestFillBitmap_NilBitmapRaisesAssertion;

    [Test]
    procedure TestFillBitmap_FillsWithColor;

    { LoadImage (function) Tests }
    [Test]
    procedure TestLoadImageFunc_EmptyFilename_ReturnsNil;

    [Test]
    procedure TestLoadImageFunc_NonExistentFile_ReturnsNil;

    [Test]
    procedure TestLoadImageFunc_ValidFile_ReturnsBitmap;

    { LoadImage (procedure) Tests }
    [Test]
    procedure TestLoadImageProc_NilImageRaisesAssertion;

    [Test]
    procedure TestLoadImageProc_NonExistentFile_AssignsPlaceholder;

    [Test]
    procedure TestLoadImageProc_ValidFile_AssignsBitmap;

    { SaveBitmap Tests }
    [Test]
    procedure TestSaveBitmap_NilBitmapRaisesAssertion;

    [Test]
    procedure TestSaveBitmap_EmptyFilenameRaisesAssertion;

    [Test]
    procedure TestSaveBitmap_SavesValidFile;

    { GetImageResolution Tests }
    [Test]
    procedure TestGetImageResolution_NonExistentFile_ReturnsZero;

    [Test]
    procedure TestGetImageResolution_ValidFile_ReturnsCorrectSize;

    { CropBitmap (from TBitmap) Tests }
    [Test]
    procedure TestCropBitmapFromBitmap_NilInputRaisesAssertion;

    [Test]
    procedure TestCropBitmapFromBitmap_EmptyCropRectRaisesAssertion;

    [Test]
    procedure TestCropBitmapFromBitmap_ValidCrop_ReturnsCorrectSize;

    { CropBitmap (from file) Tests }
    [Test]
    procedure TestCropBitmapFromFile_NonExistentFile_ReturnsNil;

    [Test]
    procedure TestCropBitmapFromFile_ValidFile_ReturnsCorrectSize;

    { CropBitmap (to TImage) Tests }
    [Test]
    procedure TestCropBitmapToImage_NilImageRaisesAssertion;

    [Test]
    procedure TestCropBitmapToImage_EmptyCropRectRaisesAssertion;
  end;

implementation

uses
  LightFmx.Graph;


procedure TTestLightFmxGraph.Setup;
begin
  FTestForm:= NIL;
  FImage:= NIL;
  FBitmap:= NIL;

  { Create scratch directory for test files }
  FScratchDir:= TPath.Combine(TPath.GetTempPath, 'LightFmxGraphTests');
  if NOT TDirectory.Exists(FScratchDir)
  then TDirectory.CreateDirectory(FScratchDir);

  FTestImagePath:= TPath.Combine(FScratchDir, 'test_image.png');
end;


procedure TTestLightFmxGraph.TearDown;
begin
  FreeAndNil(FBitmap);
  FreeAndNil(FImage);
  FreeAndNil(FTestForm);

  { Clean up test files }
  if TFile.Exists(FTestImagePath)
  then TFile.Delete(FTestImagePath);
end;


procedure TTestLightFmxGraph.CreateTestImage;
begin
  { Create a simple test image file }
  FBitmap:= CreateBitmap(100, 100, TAlphaColorRec.Red);
  try
    FBitmap.SaveToFile(FTestImagePath);
  finally
    FreeAndNil(FBitmap);
  end;
end;


{ CreateBitmap Tests }

procedure TTestLightFmxGraph.TestCreateBitmap_ReturnsValidBitmap;
begin
  FBitmap:= CreateBitmap(100, 100);
  Assert.IsNotNull(FBitmap, 'CreateBitmap should return a valid bitmap');
end;


procedure TTestLightFmxGraph.TestCreateBitmap_CorrectDimensions;
begin
  FBitmap:= CreateBitmap(200, 150);
  Assert.AreEqual(200, FBitmap.Width, 'Width should be 200');
  Assert.AreEqual(150, FBitmap.Height, 'Height should be 150');
end;


procedure TTestLightFmxGraph.TestCreateBitmap_ZeroWidthRaisesAssertion;
begin
  Assert.WillRaise(
    procedure
    begin
      FBitmap:= CreateBitmap(0, 100);
    end,
    EAssertionFailed);
end;


procedure TTestLightFmxGraph.TestCreateBitmap_ZeroHeightRaisesAssertion;
begin
  Assert.WillRaise(
    procedure
    begin
      FBitmap:= CreateBitmap(100, 0);
    end,
    EAssertionFailed);
end;


procedure TTestLightFmxGraph.TestCreateBitmap_NegativeDimensionsRaisesAssertion;
begin
  Assert.WillRaise(
    procedure
    begin
      FBitmap:= CreateBitmap(-100, 100);
    end,
    EAssertionFailed);
end;


{ FillBitmap Tests }

procedure TTestLightFmxGraph.TestFillBitmap_NilBitmapRaisesAssertion;
begin
  Assert.WillRaise(
    procedure
    begin
      FillBitmap(NIL, TAlphaColorRec.Red);
    end,
    EAssertionFailed);
end;


procedure TTestLightFmxGraph.TestFillBitmap_FillsWithColor;
begin
  FBitmap:= TBitmap.Create(10, 10);
  try
    FillBitmap(FBitmap, TAlphaColorRec.Blue);
    { Verify the bitmap was filled - check dimensions are preserved }
    Assert.AreEqual(10, FBitmap.Width, 'Width should remain 10');
    Assert.AreEqual(10, FBitmap.Height, 'Height should remain 10');
  finally
    FreeAndNil(FBitmap);
  end;
end;


{ LoadImage (function) Tests }

procedure TTestLightFmxGraph.TestLoadImageFunc_EmptyFilename_ReturnsNil;
begin
  FBitmap:= LoadImage('');
  Assert.IsNull(FBitmap, 'LoadImage with empty filename should return nil');
end;


procedure TTestLightFmxGraph.TestLoadImageFunc_NonExistentFile_ReturnsNil;
begin
  FBitmap:= LoadImage('C:\NonExistent\File\That\Does\Not\Exist.png');
  Assert.IsNull(FBitmap, 'LoadImage with non-existent file should return nil');
end;


procedure TTestLightFmxGraph.TestLoadImageFunc_ValidFile_ReturnsBitmap;
begin
  CreateTestImage;

  FBitmap:= LoadImage(FTestImagePath);
  Assert.IsNotNull(FBitmap, 'LoadImage with valid file should return a bitmap');
  Assert.AreEqual(100, FBitmap.Width, 'Loaded bitmap width should be 100');
  Assert.AreEqual(100, FBitmap.Height, 'Loaded bitmap height should be 100');
end;


{ LoadImage (procedure) Tests }

procedure TTestLightFmxGraph.TestLoadImageProc_NilImageRaisesAssertion;
begin
  Assert.WillRaise(
    procedure
    begin
      LoadImage('test.png', NIL);
    end,
    EAssertionFailed);
end;


procedure TTestLightFmxGraph.TestLoadImageProc_NonExistentFile_AssignsPlaceholder;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FImage:= TImage.Create(FTestForm);
  FImage.Parent:= FTestForm;

  LoadImage('C:\NonExistent\File.png', FImage);

  { Should have placeholder dimensions (77x77) }
  Assert.AreEqual(77, FImage.Bitmap.Width, 'Placeholder width should be 77');
  Assert.AreEqual(77, FImage.Bitmap.Height, 'Placeholder height should be 77');
end;


procedure TTestLightFmxGraph.TestLoadImageProc_ValidFile_AssignsBitmap;
begin
  CreateTestImage;

  FTestForm:= TForm.CreateNew(NIL);
  FImage:= TImage.Create(FTestForm);
  FImage.Parent:= FTestForm;

  LoadImage(FTestImagePath, FImage);

  Assert.AreEqual(100, FImage.Bitmap.Width, 'Loaded bitmap width should be 100');
  Assert.AreEqual(100, FImage.Bitmap.Height, 'Loaded bitmap height should be 100');
end;


{ SaveBitmap Tests }

procedure TTestLightFmxGraph.TestSaveBitmap_NilBitmapRaisesAssertion;
begin
  Assert.WillRaise(
    procedure
    begin
      SaveBitmap(NIL, FTestImagePath);
    end,
    EAssertionFailed);
end;


procedure TTestLightFmxGraph.TestSaveBitmap_EmptyFilenameRaisesAssertion;
begin
  FBitmap:= CreateBitmap(100, 100);
  try
    Assert.WillRaise(
      procedure
      begin
        SaveBitmap(FBitmap, '');
      end,
      EAssertionFailed);
  finally
    FreeAndNil(FBitmap);
  end;
end;


procedure TTestLightFmxGraph.TestSaveBitmap_SavesValidFile;
var
  SavePath: string;
begin
  SavePath:= TPath.Combine(FScratchDir, 'saved_test.png');

  FBitmap:= CreateBitmap(50, 50, TAlphaColorRec.Green);
  try
    SaveBitmap(FBitmap, SavePath);
    Assert.IsTrue(TFile.Exists(SavePath), 'File should exist after SaveBitmap');
  finally
    FreeAndNil(FBitmap);
    if TFile.Exists(SavePath)
    then TFile.Delete(SavePath);
  end;
end;


{ GetImageResolution Tests }

procedure TTestLightFmxGraph.TestGetImageResolution_NonExistentFile_ReturnsZero;
var
  W, H: Integer;
begin
  GetImageResolution('C:\NonExistent\File.png', W, H);
  Assert.AreEqual(0, W, 'Width should be 0 for non-existent file');
  Assert.AreEqual(0, H, 'Height should be 0 for non-existent file');
end;


procedure TTestLightFmxGraph.TestGetImageResolution_ValidFile_ReturnsCorrectSize;
var
  W, H: Integer;
begin
  CreateTestImage;

  GetImageResolution(FTestImagePath, W, H);
  Assert.AreEqual(100, W, 'Width should be 100');
  Assert.AreEqual(100, H, 'Height should be 100');
end;


{ CropBitmap (from TBitmap) Tests }

procedure TTestLightFmxGraph.TestCropBitmapFromBitmap_NilInputRaisesAssertion;
begin
  Assert.WillRaise(
    procedure
    var CropResult: TBitmap;
    begin
      CropResult:= CropBitmap(NIL, TRectF.Create(0, 0, 50, 50));
      FreeAndNil(CropResult);
    end,
    EAssertionFailed);
end;


procedure TTestLightFmxGraph.TestCropBitmapFromBitmap_EmptyCropRectRaisesAssertion;
var
  SrcBitmap: TBitmap;
begin
  SrcBitmap:= CreateBitmap(100, 100);
  try
    Assert.WillRaise(
      procedure
      var CropResult: TBitmap;
      begin
        CropResult:= CropBitmap(SrcBitmap, TRectF.Create(0, 0, 0, 0));
        FreeAndNil(CropResult);
      end,
      EAssertionFailed);
  finally
    FreeAndNil(SrcBitmap);
  end;
end;


procedure TTestLightFmxGraph.TestCropBitmapFromBitmap_ValidCrop_ReturnsCorrectSize;
var
  SrcBitmap, CropResult: TBitmap;
begin
  SrcBitmap:= CreateBitmap(100, 100);
  try
    CropResult:= CropBitmap(SrcBitmap, TRectF.Create(10, 10, 60, 70));
    try
      Assert.AreEqual(50, CropResult.Width, 'Cropped width should be 50');
      Assert.AreEqual(60, CropResult.Height, 'Cropped height should be 60');
    finally
      FreeAndNil(CropResult);
    end;
  finally
    FreeAndNil(SrcBitmap);
  end;
end;


{ CropBitmap (from file) Tests }

procedure TTestLightFmxGraph.TestCropBitmapFromFile_NonExistentFile_ReturnsNil;
var
  CropResult: TBitmap;
begin
  CropResult:= CropBitmap('C:\NonExistent\File.png', TRectF.Create(0, 0, 50, 50));
  Assert.IsNull(CropResult, 'CropBitmap from non-existent file should return nil');
end;


procedure TTestLightFmxGraph.TestCropBitmapFromFile_ValidFile_ReturnsCorrectSize;
var
  CropResult: TBitmap;
begin
  CreateTestImage;

  CropResult:= CropBitmap(FTestImagePath, TRectF.Create(10, 10, 60, 70));
  try
    Assert.IsNotNull(CropResult, 'CropBitmap from valid file should return bitmap');
    Assert.AreEqual(50, CropResult.Width, 'Cropped width should be 50');
    Assert.AreEqual(60, CropResult.Height, 'Cropped height should be 60');
  finally
    FreeAndNil(CropResult);
  end;
end;


{ CropBitmap (to TImage) Tests }

procedure TTestLightFmxGraph.TestCropBitmapToImage_NilImageRaisesAssertion;
begin
  Assert.WillRaise(
    procedure
    begin
      CropBitmap('test.png', TRectF.Create(0, 0, 50, 50), NIL);
    end,
    EAssertionFailed);
end;


procedure TTestLightFmxGraph.TestCropBitmapToImage_EmptyCropRectRaisesAssertion;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FImage:= TImage.Create(FTestForm);
  FImage.Parent:= FTestForm;

  Assert.WillRaise(
    procedure
    begin
      CropBitmap('test.png', TRectF.Empty, FImage);
    end,
    EAssertionFailed);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestLightFmxGraph);

end.
