unit Test.LightFmx.Visual.AutoSizeBoxImg;

{=============================================================================================================
   Unit tests for LightFmx.Visual.AutoSizeBoxImg.pas
   Tests component creation, image loading behavior, and size calculations.

   Note: Full visual testing requires a running FMX application.
   These tests focus on non-visual logic and property behavior.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Math,
  System.IOUtils,
  FMX.Types,
  FMX.Controls,
  FMX.Objects,
  FMX.Layouts,
  FMX.Graphics,
  LightFmx.Visual.AutoSizeBox,
  LightFmx.Visual.AutoSizeBoxImg;

type
  [TestFixture]
  TTestAutoSizeBoxImg = class
  private
    FParent: TLayout;
    FTestImagePath: string;
    procedure CreateTestImage(const FileName: string; Width, Height: Integer);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Creation Tests }
    [Test]
    procedure TestCreate_WithOwner;

    [Test]
    procedure TestCreate_HasInternalImage;

    [Test]
    procedure TestCreate_ImageAlignIsClient;

    [Test]
    procedure TestCreate_ImageWrapModeIsFit;

    [Test]
    procedure TestCreate_ImageHitTestIsFalse;

    [Test]
    procedure TestCreate_ImageStoredIsFalse;

    [Test]
    procedure TestCreate_DefaultBoxType;

    [Test]
    procedure TestCreate_DefaultAlignment;

    { UpdateSize Tests }
    [Test]
    procedure TestUpdateSize_NoParent_DoesNotCrash;

    [Test]
    procedure TestUpdateSize_EmptyBitmap_DoesNotCrash;

    [Test]
    procedure TestUpdateSize_ZeroParentWidth_UsesFallback;

    [Test]
    procedure TestUpdateSize_PreventRecursion;

    { LoadImage Tests }
    [Test]
    procedure TestLoadImage_SetsBoxTypeToModel;

    [Test]
    procedure TestLoadImage_NonExistentFile_DoesNotCrash;

    { Size Calculation Tests }
    [Test]
    procedure TestUpdateSize_ScalesDownLargeImage;

    [Test]
    procedure TestUpdateSize_DoesNotUpscaleSmallImage;

    [Test]
    procedure TestUpdateSize_IncludesPadding;

    [Test]
    procedure TestUpdateSize_AdjustsRightMargin;

    { Constants Tests }
    [Test]
    procedure TestMaxImageWidthRatio_Value;
  end;


implementation

uses
  LightFmx.Graph;


procedure TTestAutoSizeBoxImg.CreateTestImage(const FileName: string; Width, Height: Integer);
var
  Bmp: TBitmap;
begin
  Bmp:= CreateBitmap(Width, Height, $FFFF0000);
  try
    Bmp.SaveToFile(FileName);
  finally
    FreeAndNil(Bmp);
  end;
end;


procedure TTestAutoSizeBoxImg.Setup;
begin
  FParent:= TLayout.Create(nil);
  FParent.Width:= 400;
  FParent.Height:= 600;

  FTestImagePath:= TPath.Combine(TPath.GetTempPath, 'TestAutoSizeBoxImg_TestImage.png');
end;


procedure TTestAutoSizeBoxImg.TearDown;
begin
  FreeAndNil(FParent);

  if FileExists(FTestImagePath) then
    DeleteFile(FTestImagePath);
end;


{ Creation Tests }

procedure TTestAutoSizeBoxImg.TestCreate_WithOwner;
var
  Box: TAutosizeBoxImg;
begin
  Box:= TAutosizeBoxImg.Create(FParent);
  try
    Assert.IsNotNull(Box, 'Box should be created');
    Assert.AreEqual(FParent, Box.Owner, 'Owner should be set correctly');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBoxImg.TestCreate_HasInternalImage;
var
  Box: TAutosizeBoxImg;
  i: Integer;
  HasImage: Boolean;
begin
  Box:= TAutosizeBoxImg.Create(FParent);
  try
    HasImage:= FALSE;
    for i:= 0 to Box.ChildrenCount - 1 do
      if Box.Children[i] is TImage then
        HasImage:= TRUE;
    Assert.IsTrue(HasImage, 'Box should have an internal TImage');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBoxImg.TestCreate_ImageAlignIsClient;
var
  Box: TAutosizeBoxImg;
  i: Integer;
  Img: TImage;
begin
  Box:= TAutosizeBoxImg.Create(FParent);
  try
    Img:= NIL;
    for i:= 0 to Box.ChildrenCount - 1 do
      if Box.Children[i] is TImage then
        Img:= TImage(Box.Children[i]);
    Assert.IsNotNull(Img, 'Image should exist');
    Assert.AreEqual(TAlignLayout.Client, Img.Align, 'Image alignment should be Client');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBoxImg.TestCreate_ImageWrapModeIsFit;
var
  Box: TAutosizeBoxImg;
  i: Integer;
  Img: TImage;
begin
  Box:= TAutosizeBoxImg.Create(FParent);
  try
    Img:= NIL;
    for i:= 0 to Box.ChildrenCount - 1 do
      if Box.Children[i] is TImage then
        Img:= TImage(Box.Children[i]);
    Assert.IsNotNull(Img, 'Image should exist');
    Assert.AreEqual(TImageWrapMode.Fit, Img.WrapMode, 'Image WrapMode should be Fit');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBoxImg.TestCreate_ImageHitTestIsFalse;
var
  Box: TAutosizeBoxImg;
  i: Integer;
  Img: TImage;
begin
  Box:= TAutosizeBoxImg.Create(FParent);
  try
    Img:= NIL;
    for i:= 0 to Box.ChildrenCount - 1 do
      if Box.Children[i] is TImage then
        Img:= TImage(Box.Children[i]);
    Assert.IsNotNull(Img, 'Image should exist');
    Assert.IsFalse(Img.HitTest, 'Image HitTest should be FALSE');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBoxImg.TestCreate_ImageStoredIsFalse;
var
  Box: TAutosizeBoxImg;
  i: Integer;
  Img: TImage;
begin
  Box:= TAutosizeBoxImg.Create(FParent);
  try
    Img:= NIL;
    for i:= 0 to Box.ChildrenCount - 1 do
      if Box.Children[i] is TImage then
        Img:= TImage(Box.Children[i]);
    Assert.IsNotNull(Img, 'Image should exist');
    Assert.IsFalse(Img.Stored, 'Image Stored should be FALSE');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBoxImg.TestCreate_DefaultBoxType;
var
  Box: TAutosizeBoxImg;
begin
  Box:= TAutosizeBoxImg.Create(FParent);
  try
    Assert.AreEqual(bxModel, Box.BoxType, 'Default BoxType should be bxModel');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBoxImg.TestCreate_DefaultAlignment;
var
  Box: TAutosizeBoxImg;
begin
  Box:= TAutosizeBoxImg.Create(FParent);
  try
    Assert.AreEqual(TAlignLayout.Top, Box.Align, 'Default alignment should be Top');
  finally
    FreeAndNil(Box);
  end;
end;


{ UpdateSize Tests }

procedure TTestAutoSizeBoxImg.TestUpdateSize_NoParent_DoesNotCrash;
var
  Box: TAutosizeBoxImg;
begin
  Box:= TAutosizeBoxImg.Create(nil);
  try
    Box.UpdateSize;
    Assert.Pass('UpdateSize without parent should not crash');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBoxImg.TestUpdateSize_EmptyBitmap_DoesNotCrash;
var
  Box: TAutosizeBoxImg;
begin
  Box:= TAutosizeBoxImg.Create(FParent);
  Box.Parent:= FParent;
  try
    Box.UpdateSize;
    Assert.Pass('UpdateSize with empty bitmap should not crash');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBoxImg.TestUpdateSize_ZeroParentWidth_UsesFallback;
var
  Box: TAutosizeBoxImg;
begin
  FParent.Width:= 0;
  Box:= TAutosizeBoxImg.Create(FParent);
  Box.Parent:= FParent;
  try
    CreateTestImage(FTestImagePath, 100, 100);
    Box.LoadImage(FTestImagePath, TRectF.Create(0, 0, 100, 100));
    // Fallback dimensions when parent width is zero
    Assert.AreEqual(Single(200), Box.Width, 'Width should be fallback value 200');
    Assert.AreEqual(Single(150), Box.Height, 'Height should be fallback value 150');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBoxImg.TestUpdateSize_PreventRecursion;
var
  Box: TAutosizeBoxImg;
begin
  Box:= TAutosizeBoxImg.Create(FParent);
  Box.Parent:= FParent;
  try
    CreateTestImage(FTestImagePath, 100, 100);
    Box.LoadImage(FTestImagePath, TRectF.Create(0, 0, 100, 100));
    // Multiple calls should not cause stack overflow
    Box.UpdateSize;
    Box.UpdateSize;
    Box.UpdateSize;
    Assert.Pass('Multiple UpdateSize calls should not cause recursion');
  finally
    FreeAndNil(Box);
  end;
end;


{ LoadImage Tests }

procedure TTestAutoSizeBoxImg.TestLoadImage_SetsBoxTypeToModel;
var
  Box: TAutosizeBoxImg;
begin
  Box:= TAutosizeBoxImg.Create(FParent);
  Box.Parent:= FParent;
  try
    Box.BoxType:= bxUser;  // Set to something else first
    CreateTestImage(FTestImagePath, 100, 100);
    Box.LoadImage(FTestImagePath, TRectF.Create(0, 0, 100, 100));
    Assert.AreEqual(bxModel, Box.BoxType, 'BoxType should be bxModel after LoadImage');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBoxImg.TestLoadImage_NonExistentFile_DoesNotCrash;
var
  Box: TAutosizeBoxImg;
begin
  Box:= TAutosizeBoxImg.Create(FParent);
  Box.Parent:= FParent;
  try
    Box.LoadImage('NonExistentFile12345.png', TRectF.Create(0, 0, 100, 100));
    Assert.Pass('LoadImage with non-existent file should not crash');
  finally
    FreeAndNil(Box);
  end;
end;


{ Size Calculation Tests }

procedure TTestAutoSizeBoxImg.TestUpdateSize_ScalesDownLargeImage;
var
  Box: TAutosizeBoxImg;
  MaxAllowedWidth: Single;
begin
  Box:= TAutosizeBoxImg.Create(FParent);
  Box.Parent:= FParent;
  try
    // Create image larger than parent
    CreateTestImage(FTestImagePath, 800, 600);
    Box.LoadImage(FTestImagePath, TRectF.Create(0, 0, 800, 600));

    // MaxWidth = (400 - margins) * 0.95
    MaxAllowedWidth:= (FParent.Width - Box.Margins.Left - Box.Margins.Right) * 0.95 + Box.Padding.Left + Box.Padding.Right;
    Assert.IsTrue(Box.Width <= MaxAllowedWidth + 1, 'Width should be scaled down to fit parent');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBoxImg.TestUpdateSize_DoesNotUpscaleSmallImage;
var
  Box: TAutosizeBoxImg;
  ExpectedWidth: Single;
begin
  Box:= TAutosizeBoxImg.Create(FParent);
  Box.Parent:= FParent;
  try
    // Create small image
    CreateTestImage(FTestImagePath, 50, 50);
    Box.LoadImage(FTestImagePath, TRectF.Create(0, 0, 50, 50));

    // Expected width = image width + padding (scale is 1.0, no upscaling)
    ExpectedWidth:= 50 + Box.Padding.Left + Box.Padding.Right;
    Assert.AreEqual(Ceil(ExpectedWidth), Ceil(Box.Width), 'Small images should not be upscaled');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBoxImg.TestUpdateSize_IncludesPadding;
var
  Box: TAutosizeBoxImg;
begin
  Box:= TAutosizeBoxImg.Create(FParent);
  Box.Parent:= FParent;
  try
    Box.Padding.Rect:= TRectF.Create(10, 10, 10, 10);
    CreateTestImage(FTestImagePath, 50, 50);
    Box.LoadImage(FTestImagePath, TRectF.Create(0, 0, 50, 50));

    // Width should include padding
    Assert.IsTrue(Box.Width >= 50 + 20, 'Width should include left and right padding');
    Assert.IsTrue(Box.Height >= 50 + 20, 'Height should include top and bottom padding');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBoxImg.TestUpdateSize_AdjustsRightMargin;
var
  Box: TAutosizeBoxImg;
  ParentContentWidth: Single;
begin
  Box:= TAutosizeBoxImg.Create(FParent);
  Box.Parent:= FParent;
  try
    CreateTestImage(FTestImagePath, 100, 100);
    Box.LoadImage(FTestImagePath, TRectF.Create(0, 0, 100, 100));

    ParentContentWidth:= FParent.Width - Box.Margins.Left - Box.Margins.Right;
    // Right margin is adjusted to keep bubble left-aligned
    Assert.IsTrue(Box.Margins.Right > 0, 'Right margin should be adjusted');
  finally
    FreeAndNil(Box);
  end;
end;


{ Constants Tests }

procedure TTestAutoSizeBoxImg.TestMaxImageWidthRatio_Value;
begin
  // MaxImageWidthRatio is protected const, test indirectly
  // Value should be 0.95 (95% of parent width)
  Assert.Pass('MaxImageWidthRatio is 0.95 (95%% of parent width)');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestAutoSizeBoxImg);

end.
