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
    procedure TestGetVideoPlayerLogo_LeavesNoCanvasDC;

    [Test]
    procedure TestGetVideoPlayerLogo_MultipleCalls_NoMemoryLeak;

    [Test]
    procedure TestGetVideoPlayerLogo_IconIsCentered;
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


{ Thread safety: GetVideoPlayerLogo runs on the BioniX thumbnail worker (ExtractThumbnail, video branch). A canvas DC
  left on the result is what the main thread's Vcl.Graphics.FreeMemoryContexts races with when the worker frees it. }
procedure TTestGrabAviFrame.TestGetVideoPlayerLogo_LeavesNoCanvasDC;
VAR Logo: TBitmap;
begin
  Logo:= GetVideoPlayerLogo;
  TRY
    Assert.AreEqual(192, Logo.Width);
    Assert.IsFalse(Logo.Canvas.HandleAllocated, 'GetVideoPlayerLogo must not leave a DC on the canvas');
  FINALLY
    FreeAndNil(Logo);
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


{ GetVideoPlayerLogo reads AppDataCore.AppSysDir + 'video_player_icon.png' (LightVcl.Graph.GrabAviFrame.pas, GetVideoPlayerLogo).
  The test copy, UnitTesting\System\video_player_icon.png, is a solid orange (RGB 255,128,0) 48x48 square,
  so centered on the 192x128 logo it covers X 72..119 and Y 40..87. }
procedure TTestGrabAviFrame.TestGetVideoPlayerLogo_IconIsCentered;
CONST
  Orange = TColor($000080FF);   { TColor is $00BBGGRR }
VAR
  BMP: TBitmap;
begin
  Assert.IsTrue(FileExists(AppDataCore.AppSysDir + 'video_player_icon.png'), 'Test data missing: ' + AppDataCore.AppSysDir + 'video_player_icon.png');

  BMP:= GetVideoPlayerLogo;
  TRY
    Assert.AreEqual(Integer(Orange),  Integer(BMP.Canvas.Pixels[96, 64]),  'The icon must be drawn in the center');
    Assert.AreEqual(Integer(Orange),  Integer(BMP.Canvas.Pixels[72, 40]),  'Top-left pixel of the centered icon');
    Assert.AreEqual(Integer(Orange),  Integer(BMP.Canvas.Pixels[119, 87]), 'Bottom-right pixel of the centered icon');
    Assert.AreEqual(Integer(clBlack), Integer(BMP.Canvas.Pixels[71, 64]),  'Left of the icon the background stays black');
    Assert.AreEqual(Integer(clBlack), Integer(BMP.Canvas.Pixels[120, 64]), 'Right of the icon the background stays black');
  FINALLY
    FreeAndNil(BMP);
  END;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestGrabAviFrame);

end.
