UNIT Test.AniImg;

{=====================================================
   2026.02.21
   GabrielMoraru.com

   DUnitX tests for AniImg.pas (TAnimateImage)
   Tests non-visual behavior: property defaults, frame index clamping,
   start/stop frame validation, loop counting, image list linkage.
=====================================================}

INTERFACE

USES
  System.SysUtils, System.Classes,
  Vcl.Forms, Vcl.Controls, Vcl.Graphics, Vcl.ImgList,
  DUnitX.TestFramework,
  AniImg;

TYPE
  [TestFixture]
  TTestAnimateImage = class
  private
    FForm: TForm;
    FImageList: TImageList;
    FAnimImg: TAnimateImage;
    procedure AddDummyImages(Count: Integer);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test_DefaultProperties;
    [Test]
    procedure Test_DefaultInterval;
    [Test]
    procedure Test_FrameIndex_ClampsBelowNeg1;
    [Test]
    procedure Test_FrameIndex_AllowsNeg1;
    [Test]
    procedure Test_SetStartFrame_ClampsNegative;
    [Test]
    procedure Test_SetStopFrame_ClampsNegative;
    [Test]
    procedure Test_StartFrame_RejectsAboveStopFrame;
    [Test]
    procedure Test_StopFrame_RejectsBelowStartFrame;
    [Test]
    procedure Test_ImageList_SetsStopFrame;
    [Test]
    procedure Test_ImageList_Unlink;
    [Test]
    procedure Test_ActiveWithoutImages;
    [Test]
    procedure Test_FrameIndex_AdjustsOnStartFrameChange;
  end;

IMPLEMENTATION


{ Helper: adds Count 16x16 dummy bitmaps to FImageList }
procedure TTestAnimateImage.AddDummyImages(Count: Integer);
var
  Bmp: TBitmap;
  I: Integer;
begin
  Bmp:= TBitmap.Create;
  TRY
    Bmp.SetSize(16, 16);
    for I:= 1 to Count do
      FImageList.Add(Bmp, NIL);
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


procedure TTestAnimateImage.Setup;
begin
  FForm:= TForm.CreateNew(NIL);
  FForm.Visible:= FALSE;

  FImageList:= TImageList.Create(FForm);
  FImageList.Width:= 16;
  FImageList.Height:= 16;

  FAnimImg:= TAnimateImage.Create(FForm);
  FAnimImg.Parent:= FForm;
end;


procedure TTestAnimateImage.TearDown;
begin
  FreeAndNil(FForm); // Owns FAnimImg and FImageList
end;


procedure TTestAnimateImage.Test_DefaultProperties;
begin
  Assert.IsFalse(FAnimImg.Active, 'Active should default to False');
  Assert.IsTrue(FAnimImg.Center, 'Center should default to True');
  Assert.IsTrue(FAnimImg.Transparent, 'Transparent should default to True');
  Assert.IsFalse(FAnimImg.Reverse, 'Reverse should default to False');
  Assert.AreEqual(0, Integer(FAnimImg.FrameIndex), 'FrameIndex should default to 0');
  Assert.AreEqual(0, Integer(FAnimImg.StartFrame), 'StartFrame should default to 0');
  Assert.AreEqual(0, Integer(FAnimImg.StopFrame), 'StopFrame should default to 0');
  Assert.AreEqual(0, FAnimImg.NumLoops, 'NumLoops should default to 0');
  Assert.IsFalse(FAnimImg.DoubleBuffered, 'DoubleBuffered should default to False');
end;


procedure TTestAnimateImage.Test_DefaultInterval;
begin
  // Constructor and published default should both be 250
  Assert.AreEqual(250, FAnimImg.Interval, 'Default Interval should be 250');
end;


procedure TTestAnimateImage.Test_FrameIndex_ClampsBelowNeg1;
begin
  FAnimImg.FrameIndex:= -5;
  Assert.AreEqual(-1, Integer(FAnimImg.FrameIndex), 'FrameIndex below -1 should be clamped to -1');
end;


procedure TTestAnimateImage.Test_FrameIndex_AllowsNeg1;
begin
  FAnimImg.FrameIndex:= -1;
  Assert.AreEqual(-1, Integer(FAnimImg.FrameIndex), 'FrameIndex -1 should be allowed (means no image)');
end;


procedure TTestAnimateImage.Test_SetStartFrame_ClampsNegative;
begin
  FAnimImg.StartFrame:= -3;
  Assert.AreEqual(0, Integer(FAnimImg.StartFrame), 'Negative StartFrame should be clamped to 0');
end;


procedure TTestAnimateImage.Test_SetStopFrame_ClampsNegative;
begin
  FAnimImg.StopFrame:= -3;
  Assert.AreEqual(0, Integer(FAnimImg.StopFrame), 'Negative StopFrame should be clamped to 0');
end;


procedure TTestAnimateImage.Test_StartFrame_RejectsAboveStopFrame;
begin
  // With no image list, StopFrame stays at 0
  // Setting StartFrame to 5 should be rejected (5 > StopFrame=0)
  FAnimImg.StartFrame:= 5;
  Assert.AreEqual(0, Integer(FAnimImg.StartFrame), 'StartFrame above StopFrame should be rejected');
end;


procedure TTestAnimateImage.Test_StopFrame_RejectsBelowStartFrame;
begin
  AddDummyImages(4); // indices 0..3
  FAnimImg.Images:= FImageList;
  // Now StartFrame=0, StopFrame=3
  FAnimImg.StartFrame:= 2;
  // Try setting StopFrame below StartFrame
  FAnimImg.StopFrame:= 1;
  Assert.AreEqual(3, Integer(FAnimImg.StopFrame), 'StopFrame below StartFrame should be rejected');
end;


procedure TTestAnimateImage.Test_ImageList_SetsStopFrame;
begin
  AddDummyImages(3); // indices 0..2
  FAnimImg.Images:= FImageList;
  Assert.AreEqual(0, Integer(FAnimImg.StartFrame), 'StartFrame should be 0 after assigning images');
  Assert.AreEqual(2, Integer(FAnimImg.StopFrame), 'StopFrame should be Count-1 after assigning images');
end;


procedure TTestAnimateImage.Test_ImageList_Unlink;
begin
  AddDummyImages(2);
  FAnimImg.Images:= FImageList;
  Assert.AreEqual(1, Integer(FAnimImg.StopFrame));

  // Unlink the image list
  FAnimImg.Images:= NIL;
  Assert.AreEqual(0, Integer(FAnimImg.StopFrame), 'StopFrame should reset to 0 when images removed');
  Assert.IsNull(FAnimImg.Images, 'Images should be nil after unlinking');
end;


procedure TTestAnimateImage.Test_ActiveWithoutImages;
begin
  // Setting Active without images should not crash
  FAnimImg.Active:= TRUE;
  // Timer should not enable because StopFrame - StartFrame = 0 (no frames to animate)
  Assert.IsTrue(FAnimImg.Active, 'Active property should be True');
  // No crash = pass
end;


procedure TTestAnimateImage.Test_FrameIndex_AdjustsOnStartFrameChange;
begin
  AddDummyImages(5); // indices 0..4
  FAnimImg.Images:= FImageList;
  // StartFrame=0, StopFrame=4
  FAnimImg.FrameIndex:= 1;
  Assert.AreEqual(1, Integer(FAnimImg.FrameIndex));

  // Set StartFrame to 3, which is above current FrameIndex (1)
  FAnimImg.StartFrame:= 3;
  Assert.AreEqual(3, Integer(FAnimImg.FrameIndex), 'FrameIndex should adjust up to StartFrame when below range');
end;


end.
