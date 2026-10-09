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
  Winapi.Windows,   { Before Vcl.Graphics: Winapi.Windows also declares a TBitmap }
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
  System.Types,
  LightVcl.Graph.GrabAviFrame,
  LightCore.AppData;


CONST
  { The "Video file" text is drawn at Y = 4. The 48x48 icon starts at Y = 40 (see TestGetVideoPlayerLogo_IconIsCentered),
    so the rows 0..39 hold the text and the black background only. }
  TextBandHeight = 40;


{ The GDI objects (bitmaps, device contexts) that this process holds now }
function GdiObjectCount: Cardinal;
begin
  Result:= GetGuiResources(GetCurrentProcess, GR_GDIOBJECTS);
end;


{ Counts the pixels of the text band of a logo that are not black }
function CountTextPixels(Logo: TBitmap): Integer;
VAR
  X, Y: Integer;
  Pixel: PRGBTriple;
begin
  Result:= 0;
  for Y:= 0 to TextBandHeight- 1 do
    begin
      Pixel:= Logo.ScanLine[Y];
      for X:= 0 to Logo.Width- 1 do
        begin
          if (Pixel.rgbtRed <> 0) OR (Pixel.rgbtGreen <> 0) OR (Pixel.rgbtBlue <> 0)
          then Inc(Result);
          Inc(Pixel);
        end;
    end;
end;


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
  Assert.IsTrue(FileExists(AppDataCore.AppSysDir + 'video_player_icon.png'), 'Test data missing: ' + AppDataCore.AppSysDir + 'video_player_icon.png');

  BMP:= GetVideoPlayerLogo;
  TRY
    Assert.IsNotNull(BMP, 'GetVideoPlayerLogo should return a valid bitmap');
    Assert.AreEqual(192, BMP.Width,  'Logo width');
    Assert.AreEqual(128, BMP.Height, 'Logo height');
    Assert.AreEqual(pf24bit, BMP.PixelFormat, 'Logo pixel format');
    { The four corners lie outside the text and the icon }
    Assert.AreEqual(Integer(clBlack), Integer(BMP.Canvas.Pixels[0, 0]),     'Top-left corner is background');
    Assert.AreEqual(Integer(clBlack), Integer(BMP.Canvas.Pixels[191, 0]),   'Top-right corner is background');
    Assert.AreEqual(Integer(clBlack), Integer(BMP.Canvas.Pixels[0, 127]),   'Bottom-left corner is background');
    Assert.AreEqual(Integer(clBlack), Integer(BMP.Canvas.Pixels[191, 127]), 'Bottom-right corner is background');
    { Not blank: the icon covers the center and the text is in the band above it }
    Assert.AreNotEqual(Integer(clBlack), Integer(BMP.Canvas.Pixels[96, 64]), 'The icon must be drawn in the center');
    Assert.IsTrue(CountTextPixels(BMP) > 0, 'The text must be drawn above the icon');
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


{ The routine promises "Video file" in Verdana 9, lime on black, centered, at Y = 4. The reference draws that with plain
  VCL calls; the text band of the logo must hold the same pixels. }
procedure TTestGrabAviFrame.TestGetVideoPlayerLogo_TextColorIsLime;
CONST
  Text = 'Video file';
var
  BMP, Reference: TBitmap;
  X, Y, Different, FullLime, NotPureGreen: Integer;
  LogoPixel, RefPixel: PRGBTriple;
begin
  BMP:= GetVideoPlayerLogo;
  TRY
    { Verify the font color was set correctly by checking the canvas property }
    Assert.AreEqual(TColor(clLime), BMP.Canvas.Font.Color, 'Text color should be lime');

    Reference:= TBitmap.Create;
    TRY
      Reference.PixelFormat:= pf24bit;
      Reference.SetSize(192, TextBandHeight);
      Reference.Canvas.Brush.Color:= clBlack;
      Reference.Canvas.FillRect(Rect(0, 0, 192, TextBandHeight));
      Reference.Canvas.Font.Name := 'Verdana';
      Reference.Canvas.Font.Size := 9;
      Reference.Canvas.Font.Color:= clLime;
      Reference.Canvas.TextOut((192 - Reference.Canvas.TextWidth(Text)) DIV 2, 4, Text);

      Different:= 0;
      FullLime:= 0;
      NotPureGreen:= 0;
      for Y:= 0 to TextBandHeight- 1 do
        begin
          LogoPixel:= BMP.ScanLine[Y];
          RefPixel := Reference.ScanLine[Y];
          for X:= 0 to 191 do
            begin
              if (LogoPixel.rgbtRed <> RefPixel.rgbtRed) OR (LogoPixel.rgbtGreen <> RefPixel.rgbtGreen) OR (LogoPixel.rgbtBlue <> RefPixel.rgbtBlue)
              then Inc(Different);
              if (LogoPixel.rgbtRed = 0) AND (LogoPixel.rgbtGreen = 255) AND (LogoPixel.rgbtBlue = 0)
              then Inc(FullLime);
              if (LogoPixel.rgbtRed <> 0) OR (LogoPixel.rgbtBlue <> 0)
              then Inc(NotPureGreen);
              Inc(LogoPixel);
              Inc(RefPixel);
            end;
        end;
    FINALLY
      FreeAndNil(Reference);
    END;

    Assert.AreEqual(0, Different, 'The text band must hold "Video file" in Verdana 9 lime, centered at Y = 4');
    Assert.IsTrue(FullLime > 0, 'The text must have pixels of full lime (R 0, G 255, B 0)');
    { Lime is R 0, G 255, B 0 and the background is black. Antialiasing blends the two per channel, so it changes only
      the green channel: red and blue stay 0 in every pixel of the band. }
    Assert.AreEqual(0, NotPureGreen, 'Every pixel of the text band is a shade of lime on black');
  FINALLY
    FreeAndNil(BMP);
  END;
end;


{ Each call loads the icon into a temporary bitmap (AviLogo) and CenterBitmap reads its Handle, so it holds a DIB.
  If the routine did not free AviLogo, that DIB would stay behind after the logos are freed. }
procedure TTestGrabAviFrame.TestGetVideoPlayerLogo_MultipleCalls_NoMemoryLeak;
var
  BMP1, BMP2, BMP3: TBitmap;
  Before, WhileAlive: Cardinal;
begin
  Assert.IsTrue(FileExists(AppDataCore.AppSysDir + 'video_player_icon.png'), 'Test data missing: ' + AppDataCore.AppSysDir + 'video_player_icon.png');

  { Warm-up: the first logo may create GDI objects that the VCL keeps for later (the font cache) }
  BMP1:= GetVideoPlayerLogo;
  FreeAndNil(BMP1);

  Before:= GdiObjectCount;

  { Create multiple logos and ensure they can all be freed properly }
  BMP1:= GetVideoPlayerLogo;
  BMP2:= GetVideoPlayerLogo;
  BMP3:= GetVideoPlayerLogo;
  WhileAlive:= GdiObjectCount;
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

  Assert.IsTrue(WhileAlive >= Before + 3, 'Precondition: the count sees bitmaps. Three live logos hold three DIBs. Before: ' + IntToStr(Before) + ', while alive: ' + IntToStr(WhileAlive));
  Assert.AreEqual(Before, GdiObjectCount, 'Once the logos are freed no GDI object may be left: GetVideoPlayerLogo must free its icon bitmap');
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
