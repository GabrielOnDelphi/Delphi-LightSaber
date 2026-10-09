unit Test.LightVcl.Graph.Gif;

{=============================================================================================================
   Unit tests for LightVcl.Graph.Gif.pas
   Tests the GIF loading, frame extraction, and animation detection functions.

   Includes TestInsight support: define TESTINSIGHT in project options.

   Setup writes the two GIF files the tests need into a temporary folder that TearDown deletes:
     - test_static.gif   (1 red frame)
     - test_animated.gif (3 frames: red, blue, lime; each with a delay of GifDelay hundredths of a second)
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  Winapi.Windows,   { Before Vcl.Graphics: Winapi.Windows also declares a TBitmap }
  Vcl.Graphics;

type
  [TestFixture]
  TTestGraphGif = class
  private
    FTestFolder: string;
    FAnimatedGifPath: string;
    FStaticGifPath: string;
    /// No test calls this, and an uncalled private method is compiler hint H2219.
    /// Bring both halves back when a test needs to skip because the sample GIF files are absent.
    /// function HasTestFiles: Boolean;
    procedure WriteGif(const FileName: string; const FrameColors: array of TColor);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { TGifLoader - Constructor/Destructor Tests }
    [Test]
    procedure TestGifLoader_Create_FrameCountIsZero;

    [Test]
    procedure TestGifLoader_Destroy_NoException;

    { TGifLoader.Open Tests }
    [Test]
    procedure TestGifLoader_Open_NonExistentFile;

    [Test]
    procedure TestGifLoader_Open_InvalidPath;

    { TGifLoader.ExtractFrame Tests }
    [Test]
    procedure TestGifLoader_ExtractFrame_WithoutOpen;

    [Test]
    procedure TestGifLoader_ExtractFrame_InvalidFrameNumber;

    { TGifLoader.SaveFrames Tests }
    [Test]
    procedure TestGifLoader_SaveFrames_WithoutOpen;

    { IsAnimated Tests }
    [Test]
    procedure TestIsAnimated_NonExistentFile;

    [Test]
    procedure TestIsAnimated_EmptyPath;

    { ExtractMiddleFrame Tests }
    [Test]
    procedure TestExtractMiddleFrame_NonExistentFile;

    [Test]
    procedure TestExtractMiddleFrame_FrameCountInitialized;

    { Integration Tests - require test files }
    [Test]
    procedure TestGifLoader_Open_StaticGif;

    [Test]
    procedure TestGifLoader_Open_AnimatedGif;

    [Test]
    procedure TestGifLoader_ExtractFrame_ValidFrame;

    [Test]
    procedure TestGifLoader_FrameDelay_AfterOpen;

    [Test]
    procedure TestIsAnimated_StaticGif;

    [Test]
    procedure TestIsAnimated_AnimatedGif;

    [Test]
    procedure TestExtractMiddleFrame_AnimatedGif;
  end;

implementation

uses
  System.Types,
  Vcl.Imaging.GIFImg,
  LightVcl.Graph.Gif,
  LightCore.AppData,
  LightCore.IO;

CONST
  GifWidth  = 20;
  GifHeight = 10;
  GifDelay  = 20;   { Hundredths of a second, as stored in the GIF graphic control extension }


procedure TTestGraphGif.Setup;
begin
  { Ensure AppDataCore is created for tests }
  if AppDataCore = NIL then
    AppDataCore:= TAppDataCore.Create('TestApp');

  { The fixture writes its own GIF files, so no test depends on a file that can go missing }
  FTestFolder:= IncludeTrailingPathDelimiter(TPath.Combine(TPath.GetTempPath, 'TestGraphGif_' + TGUID.NewGuid.ToString));
  TDirectory.CreateDirectory(FTestFolder);
  FAnimatedGifPath:= FTestFolder + 'test_animated.gif';
  FStaticGifPath:= FTestFolder + 'test_static.gif';
  WriteGif(FStaticGifPath,   [clRed]);
  WriteGif(FAnimatedGifPath, [clRed, clBlue, clLime]);
end;


procedure TTestGraphGif.TearDown;
begin
  if TDirectory.Exists(FTestFolder)
  then TDirectory.Delete(FTestFolder, TRUE);
end;


{ Writes a GIF with one solid-colored GifWidth x GifHeight frame per color }
procedure TTestGraphGif.WriteGif(const FileName: string; const FrameColors: array of TColor);
VAR
  GIF: TGIFImage;
  BMP: TBitmap;
  Frame: TGIFFrame;
  GCE: TGIFGraphicControlExtension;
begin
  GIF:= TGIFImage.Create;
  TRY
    for VAR Color in FrameColors do
      begin
        BMP:= TBitmap.Create;
        TRY
          BMP.PixelFormat:= pf24bit;
          BMP.SetSize(GifWidth, GifHeight);
          BMP.Canvas.Brush.Color:= Color;
          BMP.Canvas.FillRect(Rect(0, 0, GifWidth, GifHeight));
          Frame:= GIF.Add(BMP);
        FINALLY
          FreeAndNil(BMP);
        END;
        GCE:= TGIFGraphicControlExtension.Create(Frame);   { The constructor adds itself to Frame.Extensions, which owns it }
        GCE.Delay:= GifDelay;
      end;
    GIF.SaveToFile(FileName);
  FINALLY
    FreeAndNil(GIF);
  END;
end;


/// function TTestGraphGif.HasTestFiles: Boolean;
/// begin
///   Result:= FileExists(FAnimatedGifPath) OR FileExists(FStaticGifPath);
/// end;


{ TGifLoader - Constructor/Destructor Tests }

procedure TTestGraphGif.TestGifLoader_Create_FrameCountIsZero;
var
  Loader: TGifLoader;
begin
  Loader:= TGifLoader.Create;
  TRY
    Assert.AreEqual(Cardinal(0), Loader.FrameCount, 'FrameCount should be 0 after creation');
  FINALLY
    FreeAndNil(Loader);
  END;
end;


{ The GDI objects (bitmaps, device contexts, palettes) that this process holds now }
function GdiObjectCount: Cardinal;
begin
  Result:= GetGuiResources(GetCurrentProcess, GR_GDIOBJECTS);
end;


{ An opened loader holds GDI objects. Open draws the first frame through the renderer, and TGIFRenderer.RenderFrame
  (c:\Delphi\Delphi 13\source\vcl\Vcl.Imaging.GIFImg.pas) fills the renderer's buffer bitmap (TGIFRenderer.FBuffer) with the
  palette and the bitmap of the frame (TGIFFrame.Palette, TGIFFrame.Bitmap), which belong to the TGIFImage.
  A destructor that does not free GIFImg or Renderer leaves them behind, and the count after Free shows it. }
procedure TTestGraphGif.TestGifLoader_Destroy_NoException;
var
  Loader: TGifLoader;
  Opened: Boolean;
  Before, WhileOpen: Cardinal;
begin
  { Warm-up: the first GIF drawn in the process may create GDI objects that the VCL keeps for later }
  Loader:= TGifLoader.Create;
  TRY
    Assert.IsTrue(Loader.Open(FAnimatedGifPath), 'Precondition: the 3-frame GIF opens');
  FINALLY
    FreeAndNil(Loader);
  END;

  Before:= GdiObjectCount;
  Loader:= TGifLoader.Create;
  Opened:= Loader.Open(FAnimatedGifPath);
  WhileOpen:= GdiObjectCount;
  Assert.WillNotRaiseAny(
    procedure
    begin
      FreeAndNil(Loader);
    end,
    'TGifLoader.Destroy must not raise');

  Assert.IsTrue(Opened, 'Precondition: the 3-frame GIF opens');
  Assert.IsTrue(WhileOpen > Before, 'Precondition: an opened loader holds GDI objects. Before: ' + IntToStr(Before) + ', while open: ' + IntToStr(WhileOpen));
  Assert.AreEqual(Before, GdiObjectCount, 'TGifLoader.Destroy must free every GDI object the loader made (GIFImg and Renderer)');
end;


{ TGifLoader.Open Tests }

procedure TTestGraphGif.TestGifLoader_Open_NonExistentFile;
var
  Loader: TGifLoader;
  OpenResult: Boolean;
begin
  Loader:= TGifLoader.Create;
  TRY
    OpenResult:= Loader.Open('C:\NonExistent\FakeFile.gif');
    Assert.IsFalse(OpenResult, 'Open should return False for non-existent file');
  FINALLY
    FreeAndNil(Loader);
  END;
end;


procedure TTestGraphGif.TestGifLoader_Open_InvalidPath;
var
  Loader: TGifLoader;
  OpenResult: Boolean;
begin
  Loader:= TGifLoader.Create;
  TRY
    OpenResult:= Loader.Open('');
    Assert.IsFalse(OpenResult, 'Open should return False for empty path');
  FINALLY
    FreeAndNil(Loader);
  END;
end;


{ TGifLoader.ExtractFrame Tests }

procedure TTestGraphGif.TestGifLoader_ExtractFrame_WithoutOpen;
var
  Loader: TGifLoader;
  Frame: TBitmap;
begin
  Loader:= TGifLoader.Create;
  TRY
    Frame:= Loader.ExtractFrame(0);
    Assert.IsNull(Frame, 'ExtractFrame should return NIL when Open was not called');
  FINALLY
    FreeAndNil(Loader);
  END;
end;


procedure TTestGraphGif.TestGifLoader_ExtractFrame_InvalidFrameNumber;
var
  Loader: TGifLoader;
  Frame: TBitmap;
begin
  Loader:= TGifLoader.Create;
  TRY
    { Open first, so ExtractFrame passes its "Renderer = NIL" check and reaches the frame-number check }
    Assert.IsTrue(Loader.Open(FAnimatedGifPath), 'Precondition: the 3-frame GIF opens');
    Frame:= Loader.ExtractFrame(3);   { Frames are 0..2 }
    Assert.IsNull(Frame, 'ExtractFrame(FrameCount) must return NIL');
    Frame:= Loader.ExtractFrame(999999);
    Assert.IsNull(Frame, 'ExtractFrame should return NIL for invalid frame number');
  FINALLY
    FreeAndNil(Loader);
  END;
end;


{ TGifLoader.SaveFrames Tests }

procedure TTestGraphGif.TestGifLoader_SaveFrames_WithoutOpen;
var
  Loader: TGifLoader;
  SaveResult: Boolean;
begin
  Loader:= TGifLoader.Create;
  TRY
    SaveResult:= Loader.SaveFrames(FTestFolder);
    Assert.IsFalse(SaveResult, 'SaveFrames should return False when Open was not called');
  FINALLY
    FreeAndNil(Loader);
  END;
end;


{ IsAnimated Tests }

procedure TTestGraphGif.TestIsAnimated_NonExistentFile;
begin
  Assert.WillRaise(
    procedure
    begin
      IsAnimated('C:\NonExistent\FakeFile.gif');
    end,
    EFileNotFoundException, 'IsAnimated must raise for a non-existent file');
end;


procedure TTestGraphGif.TestIsAnimated_EmptyPath;
begin
  Assert.WillRaise(
    procedure
    begin
      IsAnimated('');
    end,
    EFileNotFoundException, 'IsAnimated must raise for an empty path');
end;


{ ExtractMiddleFrame Tests }

procedure TTestGraphGif.TestExtractMiddleFrame_NonExistentFile;
var
  Frame: TBitmap;
  FrameCount: Cardinal;
begin
  Frame:= ExtractMiddleFrame('C:\NonExistent\FakeFile.gif', FrameCount);
  TRY
    Assert.IsNull(Frame, 'ExtractMiddleFrame should return NIL for non-existent file');
  FINALLY
    FreeAndNil(Frame);
  END;
end;


procedure TTestGraphGif.TestExtractMiddleFrame_FrameCountInitialized;
var
  Frame: TBitmap;
  FrameCount: Cardinal;
begin
  FrameCount:= 999;  { Set to non-zero to verify it gets reset }
  Frame:= ExtractMiddleFrame('C:\NonExistent\FakeFile.gif', FrameCount);
  TRY
    Assert.AreEqual(Cardinal(0), FrameCount, 'FrameCount should be 0 when extraction fails');
  FINALLY
    FreeAndNil(Frame);
  END;
end;


{ Integration Tests - require test files }

procedure TTestGraphGif.TestGifLoader_Open_StaticGif;
var
  Loader: TGifLoader;
  OpenResult: Boolean;
begin
  Loader:= TGifLoader.Create;
  TRY
    OpenResult:= Loader.Open(FStaticGifPath);
    { Static GIF should fail because it has only 1 frame }
    Assert.IsFalse(OpenResult, 'Open should return False for static (1-frame) GIF');
    Assert.AreEqual(Cardinal(1), Loader.FrameCount, 'The static GIF was read: it has 1 frame');
  FINALLY
    FreeAndNil(Loader);
  END;
end;


procedure TTestGraphGif.TestGifLoader_Open_AnimatedGif;
var
  Loader: TGifLoader;
  OpenResult: Boolean;
begin
  Loader:= TGifLoader.Create;
  TRY
    OpenResult:= Loader.Open(FAnimatedGifPath);
    Assert.IsTrue(OpenResult, 'Open should return True for animated GIF');
    Assert.AreEqual(Cardinal(3), Loader.FrameCount, 'The animated GIF written by Setup has 3 frames');
  FINALLY
    FreeAndNil(Loader);
  END;
end;


procedure TTestGraphGif.TestGifLoader_ExtractFrame_ValidFrame;
var
  Loader: TGifLoader;
  Frame: TBitmap;
begin
  Loader:= TGifLoader.Create;
  TRY
    if Loader.Open(FAnimatedGifPath) then
    begin
      Frame:= Loader.ExtractFrame(0);
      TRY
        Assert.IsNotNull(Frame, 'ExtractFrame(0) should return valid bitmap');
        Assert.AreEqual(GifWidth,  Frame.Width,  'Extracted frame has the width of the GIF');
        Assert.AreEqual(GifHeight, Frame.Height, 'Extracted frame has the height of the GIF');
        Assert.AreEqual(Integer(clRed), Integer(Frame.Canvas.Pixels[GifWidth DIV 2, GifHeight DIV 2]), 'Frame 0 is the red one');
      FINALLY
        FreeAndNil(Frame);
      END;
    end
    else
      Assert.Fail('Could not open test animated GIF');
  FINALLY
    FreeAndNil(Loader);
  END;
end;


procedure TTestGraphGif.TestGifLoader_FrameDelay_AfterOpen;
var
  Loader: TGifLoader;
begin
  Loader:= TGifLoader.Create;
  TRY
    if Loader.Open(FAnimatedGifPath) then
    begin
      { TGIFRenderer.FrameDelay = Delay * GIFDelayExp (12) * 100 / Speed (100)  (c:\Delphi\Delphi 13\source\vcl\Vcl.Imaging.GIFImg.pas, TGIFRenderer, "FFrameDelay := MulDiv(Delay * GIFDelayExp, 100, Speed)") }
      Assert.AreEqual(GifDelay * 12, Loader.FrameDelay, 'FrameDelay must come from the delay stored in the first frame');
    end
    else
      Assert.Fail('Could not open test animated GIF');
  FINALLY
    FreeAndNil(Loader);
  END;
end;


procedure TTestGraphGif.TestIsAnimated_StaticGif;
var
  Result: Boolean;
begin
  Result:= IsAnimated(FStaticGifPath);
  Assert.IsFalse(Result, 'IsAnimated should return False for static GIF');
end;


procedure TTestGraphGif.TestIsAnimated_AnimatedGif;
var
  Result: Boolean;
begin
  Result:= IsAnimated(FAnimatedGifPath);
  Assert.IsTrue(Result, 'IsAnimated should return True for animated GIF');
end;


procedure TTestGraphGif.TestExtractMiddleFrame_AnimatedGif;
var
  Frame: TBitmap;
  FrameCount: Cardinal;
begin
  Frame:= ExtractMiddleFrame(FAnimatedGifPath, FrameCount);
  TRY
    Assert.IsNotNull(Frame, 'ExtractMiddleFrame should return valid bitmap for animated GIF');
    Assert.AreEqual(Cardinal(3), FrameCount, 'The animated GIF written by Setup has 3 frames');
    Assert.AreEqual(GifWidth,  Frame.Width,  'Extracted frame has the width of the GIF');
    Assert.AreEqual(GifHeight, Frame.Height, 'Extracted frame has the height of the GIF');
    { Middle frame = 3 DIV 2 = frame 1, the blue one }
    Assert.AreEqual(Integer(clBlue), Integer(Frame.Canvas.Pixels[GifWidth DIV 2, GifHeight DIV 2]), 'The middle frame is the blue one');
  FINALLY
    FreeAndNil(Frame);
  END;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestGraphGif);

end.
