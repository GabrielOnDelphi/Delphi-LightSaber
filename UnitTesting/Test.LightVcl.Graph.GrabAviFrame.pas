unit Test.LightVcl.Graph.GrabAviFrame;

{=============================================================================================================
   Unit tests for LightVcl.Graph.GrabAviFrame.pas
   Tests the video frame placeholder logo generation function.

   Includes TestInsight support: define TESTINSIGHT in project options.

   Note: These tests require AppDataCore to be initialized. The Setup method creates
   a temporary AppDataCore instance for testing purposes.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Vcl.Graphics;

type
  [TestFixture]
  TTestGrabAviFrame = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { GetVideoPlayerLogo Tests }
    [Test]
    procedure TestGetVideoPlayerLogo_ReturnsValidBitmap;

    [Test]
    procedure TestGetVideoPlayerLogo_CorrectDimensions;

    [Test]
    procedure TestGetVideoPlayerLogo_CorrectPixelFormat;

    [Test]
    procedure TestGetVideoPlayerLogo_BackgroundIsBlack;

    [Test]
    procedure TestGetVideoPlayerLogo_TextColorIsLime;

    [Test]
    procedure TestGetVideoPlayerLogo_MultipleCalls_NoMemoryLeak;

    { Constants Tests }
    [Test]
    procedure TestVideoFilesConstant_NotEmpty;

    [Test]
    procedure TestVideoFilesConstant_ContainsAVI;

    [Test]
    procedure TestVideoFilesConstant_ContainsMP4;

    [Test]
    procedure TestVideoFilesConstant_ContainsMKV;

    [Test]
    procedure TestVideoFilesFtlConstant_HasFilterPrefix;

    [Test]
    procedure TestVideoFilesFtlConstant_ContainsVideoFiles;
  end;

implementation

uses
  LightVcl.Graph.GrabAviFrame,
  LightCore.AppData;


procedure TTestGrabAviFrame.Setup;
begin
  { Ensure AppDataCore is created for tests that need file paths }
  if AppDataCore = NIL then
    AppDataCore:= TAppDataCore.Create('TestApp');
end;


procedure TTestGrabAviFrame.TearDown;
begin
  { AppDataCore is typically freed in finalization, no cleanup needed here }
end;


{ GetVideoPlayerLogo Tests }

procedure TTestGrabAviFrame.TestGetVideoPlayerLogo_ReturnsValidBitmap;
var
  BMP: TBitmap;
begin
  BMP:= GetVideoPlayerLogo;
  TRY
    Assert.IsNotNull(BMP, 'GetVideoPlayerLogo should return a valid bitmap');
  FINALLY
    FreeAndNil(BMP);
  END;
end;


procedure TTestGrabAviFrame.TestGetVideoPlayerLogo_CorrectDimensions;
var
  BMP: TBitmap;
begin
  BMP:= GetVideoPlayerLogo;
  TRY
    Assert.AreEqual(192, BMP.Width, 'Logo width should be 192 pixels');
    Assert.AreEqual(128, BMP.Height, 'Logo height should be 128 pixels');
  FINALLY
    FreeAndNil(BMP);
  END;
end;


procedure TTestGrabAviFrame.TestGetVideoPlayerLogo_CorrectPixelFormat;
var
  BMP: TBitmap;
begin
  BMP:= GetVideoPlayerLogo;
  TRY
    Assert.AreEqual(pf24bit, BMP.PixelFormat, 'Logo should use 24-bit pixel format');
  FINALLY
    FreeAndNil(BMP);
  END;
end;


procedure TTestGrabAviFrame.TestGetVideoPlayerLogo_BackgroundIsBlack;
var
  BMP: TBitmap;
  CornerPixel: TColor;
begin
  BMP:= GetVideoPlayerLogo;
  TRY
    { Check corner pixel which should be black (not covered by centered icon) }
    CornerPixel:= BMP.Canvas.Pixels[0, BMP.Height - 1];
    Assert.AreEqual(TColor(clBlack), CornerPixel, 'Background should be black');
  FINALLY
    FreeAndNil(BMP);
  END;
end;


procedure TTestGrabAviFrame.TestGetVideoPlayerLogo_TextColorIsLime;
var
  BMP: TBitmap;
begin
  BMP:= GetVideoPlayerLogo;
  TRY
    { Verify the font color was set correctly by checking the canvas property }
    Assert.AreEqual(TColor(clLime), BMP.Canvas.Font.Color, 'Text color should be lime');
  FINALLY
    FreeAndNil(BMP);
  END;
end;


procedure TTestGrabAviFrame.TestGetVideoPlayerLogo_MultipleCalls_NoMemoryLeak;
var
  BMP1, BMP2, BMP3: TBitmap;
begin
  { Create multiple logos and ensure they can all be freed properly }
  BMP1:= GetVideoPlayerLogo;
  BMP2:= GetVideoPlayerLogo;
  BMP3:= GetVideoPlayerLogo;
  TRY
    Assert.IsNotNull(BMP1, 'First logo should be valid');
    Assert.IsNotNull(BMP2, 'Second logo should be valid');
    Assert.IsNotNull(BMP3, 'Third logo should be valid');

    { Verify they are independent objects }
    Assert.AreNotEqual(NativeUInt(BMP1), NativeUInt(BMP2), 'Each call should create new bitmap');
    Assert.AreNotEqual(NativeUInt(BMP2), NativeUInt(BMP3), 'Each call should create new bitmap');
  FINALLY
    FreeAndNil(BMP1);
    FreeAndNil(BMP2);
    FreeAndNil(BMP3);
  END;
end;


{ Constants Tests }

procedure TTestGrabAviFrame.TestVideoFilesConstant_NotEmpty;
begin
  Assert.IsTrue(Length(VideoFiles) > 0, 'VideoFiles constant should not be empty');
end;


procedure TTestGrabAviFrame.TestVideoFilesConstant_ContainsAVI;
begin
  Assert.IsTrue(Pos('*.AVI', VideoFiles) > 0, 'VideoFiles should contain *.AVI');
end;


procedure TTestGrabAviFrame.TestVideoFilesConstant_ContainsMP4;
begin
  Assert.IsTrue(Pos('*.MP4', VideoFiles) > 0, 'VideoFiles should contain *.MP4');
end;


procedure TTestGrabAviFrame.TestVideoFilesConstant_ContainsMKV;
begin
  Assert.IsTrue(Pos('*.MKV', VideoFiles) > 0, 'VideoFiles should contain *.MKV');
end;


procedure TTestGrabAviFrame.TestVideoFilesFtlConstant_HasFilterPrefix;
begin
  Assert.IsTrue(Pos('Video Files|', VideoFilesFtl) = 1,
    'VideoFilesFtl should start with "Video Files|"');
end;


procedure TTestGrabAviFrame.TestVideoFilesFtlConstant_ContainsVideoFiles;
begin
  Assert.IsTrue(Pos(VideoFiles, VideoFilesFtl) > 0,
    'VideoFilesFtl should contain VideoFiles constant');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestGrabAviFrame);

end.
