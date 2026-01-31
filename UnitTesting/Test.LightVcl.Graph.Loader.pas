unit Test.LightVcl.Graph.Loader;

{=============================================================================================================
   Unit tests for LightVcl.Graph.Loader.pas
   Tests image loading functions for various formats (BMP, PNG, GIF, JPG, etc.)

   Note: These tests require actual image files on disk to fully test loading functionality.
   Some tests create temporary test files, others test parameter validation and error handling.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.ExtCtrls;

type
  [TestFixture]
  TTestGraphLoader = class
  private
    FTempDir: string;
    FTempBmpFile: string;
    FTempPngFile: string;
    FTempJpgFile: string;
    FTempGifFile: string;
    procedure CreateTempBmpFile;
    procedure CreateTempPngFile;
    procedure CreateTempJpgFile;
    procedure CreateTempGifFile;
    procedure CleanupTempFiles;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { LoadGraph Tests }
    [Test]
    procedure TestLoadGraph_NonExistentFile;

    [Test]
    procedure TestLoadGraph_ValidBmpFile;

    [Test]
    procedure TestLoadGraph_ValidPngFile;

    [Test]
    procedure TestLoadGraph_ValidJpgFile;

    [Test]
    procedure TestLoadGraph_ReturnsNilForInvalidFile;

    { LoadGraph with FrameCount Tests }
    [Test]
    procedure TestLoadGraph_FrameCount_NonExistentFile;

    [Test]
    procedure TestLoadGraph_FrameCount_StaticImage;

    { LoadGraphToImg Tests }
    [Test]
    procedure TestLoadGraphToImg_NilImage;

    [Test]
    procedure TestLoadGraphToImg_ValidCall;

    { LoadToTImage Tests }
    [Test]
    procedure TestLoadToTImage_NilImage;

    { LoadBMP Tests }
    [Test]
    procedure TestLoadBMP_NonExistentFile;

    [Test]
    procedure TestLoadBMP_ValidFile;

    { LoadPNG Tests }
    [Test]
    procedure TestLoadPNG_NonExistentFile;

    [Test]
    procedure TestLoadPNG_ValidFile;

    { LoadJpg Tests }
    [Test]
    procedure TestLoadJpg_NonExistentFile;

    [Test]
    procedure TestLoadJpg_ValidFile;

    [Test]
    procedure TestLoadJpg_WithScale;

    { LoadGIF Tests }
    [Test]
    procedure TestLoadGIF_NonExistentFile;

    [Test]
    procedure TestLoadGIF_ValidFile;

    [Test]
    procedure TestLoadGIF_FrameCount;

    { loadGraphWic Tests }
    [Test]
    procedure TestLoadGraphWic_NonExistentFile;

    [Test]
    procedure TestLoadGraphWic_ValidPngFile;

    { LoadGraphAsGrayScale Tests }
    [Test]
    procedure TestLoadGraphAsGrayScale_ValidFile;

    [Test]
    procedure TestLoadGraphAsGrayScale_WithBMP_NilBMP;

    { DetectGraphSignature Tests }
    [Test]
    procedure TestDetectGraphSignature_BmpFile;

    [Test]
    procedure TestDetectGraphSignature_PngFile;

    [Test]
    procedure TestDetectGraphSignature_JpgFile;

    [Test]
    procedure TestDetectGraphSignature_GifFile;

    { CheckValidImage Tests }
    [Test]
    procedure TestCheckValidImage_ValidFile;

    [Test]
    procedure TestCheckValidImage_InvalidFile;

    { LoadTPicture Tests }
    [Test]
    procedure TestLoadTPicture_NonExistentFile;

    [Test]
    procedure TestLoadTPicture_ValidFile;

    { ExtractThumbnail Tests }
    [Test]
    procedure TestExtractThumbnail_NonExistentFile;

    [Test]
    procedure TestExtractThumbnail_ValidFile;
  end;

implementation

uses
  Vcl.Imaging.Jpeg,
  Vcl.Imaging.PngImage,
  Vcl.Imaging.GIFImg,
  LightVcl.Graph.Loader;


procedure TTestGraphLoader.Setup;
begin
  FTempDir:= TPath.Combine(TPath.GetTempPath, 'TestGraphLoader_' + TGUID.NewGuid.ToString);
  ForceDirectories(FTempDir);
  FTempBmpFile:= '';
  FTempPngFile:= '';
  FTempJpgFile:= '';
  FTempGifFile:= '';
end;


procedure TTestGraphLoader.TearDown;
begin
  CleanupTempFiles;
  if DirectoryExists(FTempDir)
  then RemoveDir(FTempDir);
end;


procedure TTestGraphLoader.CleanupTempFiles;
begin
  if (FTempBmpFile <> '') AND FileExists(FTempBmpFile)
  then DeleteFile(FTempBmpFile);
  if (FTempPngFile <> '') AND FileExists(FTempPngFile)
  then DeleteFile(FTempPngFile);
  if (FTempJpgFile <> '') AND FileExists(FTempJpgFile)
  then DeleteFile(FTempJpgFile);
  if (FTempGifFile <> '') AND FileExists(FTempGifFile)
  then DeleteFile(FTempGifFile);
end;


procedure TTestGraphLoader.CreateTempBmpFile;
var
  Bmp: TBitmap;
begin
  FTempBmpFile:= TPath.Combine(FTempDir, 'test.bmp');
  Bmp:= TBitmap.Create;
  TRY
    Bmp.Width:= 100;
    Bmp.Height:= 100;
    Bmp.PixelFormat:= pf24bit;
    Bmp.Canvas.Brush.Color:= clRed;
    Bmp.Canvas.FillRect(Rect(0, 0, 100, 100));
    Bmp.SaveToFile(FTempBmpFile);
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


procedure TTestGraphLoader.CreateTempPngFile;
var
  Png: TPngImage;
begin
  FTempPngFile:= TPath.Combine(FTempDir, 'test.png');
  Png:= TPngImage.Create;
  TRY
    Png.CreateBlank(COLOR_RGB, 8, 100, 100);
    Png.Canvas.Brush.Color:= clBlue;
    Png.Canvas.FillRect(Rect(0, 0, 100, 100));
    Png.SaveToFile(FTempPngFile);
  FINALLY
    FreeAndNil(Png);
  END;
end;


procedure TTestGraphLoader.CreateTempJpgFile;
var
  Bmp: TBitmap;
  Jpg: TJPEGImage;
begin
  FTempJpgFile:= TPath.Combine(FTempDir, 'test.jpg');
  Bmp:= TBitmap.Create;
  TRY
    Bmp.Width:= 100;
    Bmp.Height:= 100;
    Bmp.PixelFormat:= pf24bit;
    Bmp.Canvas.Brush.Color:= clGreen;
    Bmp.Canvas.FillRect(Rect(0, 0, 100, 100));

    Jpg:= TJPEGImage.Create;
    TRY
      Jpg.Assign(Bmp);
      Jpg.SaveToFile(FTempJpgFile);
    FINALLY
      FreeAndNil(Jpg);
    END;
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


procedure TTestGraphLoader.CreateTempGifFile;
var
  Gif: TGIFImage;
begin
  FTempGifFile:= TPath.Combine(FTempDir, 'test.gif');
  Gif:= TGIFImage.Create;
  TRY
    Gif.Width:= 100;
    Gif.Height:= 100;
    // Add a frame
    Gif.Add(TBitmap.Create);
    Gif.Images[0].Bitmap.Width:= 100;
    Gif.Images[0].Bitmap.Height:= 100;
    Gif.Images[0].Bitmap.Canvas.Brush.Color:= clYellow;
    Gif.Images[0].Bitmap.Canvas.FillRect(Rect(0, 0, 100, 100));
    Gif.SaveToFile(FTempGifFile);
  FINALLY
    FreeAndNil(Gif);
  END;
end;


{ LoadGraph Tests }

procedure TTestGraphLoader.TestLoadGraph_NonExistentFile;
begin
  Assert.WillRaise(
    procedure
    begin
      LoadGraph('C:\NonExistent\Path\file.bmp');
    end,
    EAssertionFailed);
end;


procedure TTestGraphLoader.TestLoadGraph_ValidBmpFile;
var
  Bmp: TBitmap;
begin
  CreateTempBmpFile;

  Bmp:= LoadGraph(FTempBmpFile);
  TRY
    Assert.IsNotNull(Bmp, 'LoadGraph should return a bitmap for valid BMP file');
    Assert.AreEqual(100, Bmp.Width, 'Width should be 100');
    Assert.AreEqual(100, Bmp.Height, 'Height should be 100');
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


procedure TTestGraphLoader.TestLoadGraph_ValidPngFile;
var
  Bmp: TBitmap;
begin
  CreateTempPngFile;

  Bmp:= LoadGraph(FTempPngFile);
  TRY
    Assert.IsNotNull(Bmp, 'LoadGraph should return a bitmap for valid PNG file');
    Assert.AreEqual(100, Bmp.Width, 'Width should be 100');
    Assert.AreEqual(100, Bmp.Height, 'Height should be 100');
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


procedure TTestGraphLoader.TestLoadGraph_ValidJpgFile;
var
  Bmp: TBitmap;
begin
  CreateTempJpgFile;

  Bmp:= LoadGraph(FTempJpgFile);
  TRY
    Assert.IsNotNull(Bmp, 'LoadGraph should return a bitmap for valid JPG file');
    Assert.AreEqual(100, Bmp.Width, 'Width should be 100');
    Assert.AreEqual(100, Bmp.Height, 'Height should be 100');
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


procedure TTestGraphLoader.TestLoadGraph_ReturnsNilForInvalidFile;
var
  Bmp: TBitmap;
  InvalidFile: string;
  Stream: TFileStream;
begin
  // Create a file with invalid image data
  InvalidFile:= TPath.Combine(FTempDir, 'invalid.bmp');
  Stream:= TFileStream.Create(InvalidFile, fmCreate);
  TRY
    Stream.WriteBuffer('INVALID DATA', 12);
  FINALLY
    FreeAndNil(Stream);
  END;

  TRY
    Bmp:= LoadGraph(InvalidFile);
    TRY
      // Should return NIL for invalid image
      Assert.IsNull(Bmp, 'LoadGraph should return NIL for invalid image data');
    FINALLY
      FreeAndNil(Bmp);
    END;
  FINALLY
    DeleteFile(InvalidFile);
  END;
end;


{ LoadGraph with FrameCount Tests }

procedure TTestGraphLoader.TestLoadGraph_FrameCount_NonExistentFile;
var
  FrameCount: Cardinal;
begin
  Assert.WillRaise(
    procedure
    begin
      LoadGraph('C:\NonExistent\Path\file.gif', FrameCount);
    end,
    EAssertionFailed);
end;


procedure TTestGraphLoader.TestLoadGraph_FrameCount_StaticImage;
var
  Bmp: TBitmap;
  FrameCount: Cardinal;
begin
  CreateTempBmpFile;

  Bmp:= LoadGraph(FTempBmpFile, FrameCount);
  TRY
    Assert.IsNotNull(Bmp, 'Should load the bitmap');
    Assert.AreEqual(Cardinal(0), FrameCount, 'FrameCount should be 0 for non-GIF');
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


{ LoadGraphToImg Tests }

procedure TTestGraphLoader.TestLoadGraphToImg_NilImage;
begin
  CreateTempBmpFile;

  Assert.WillRaise(
    procedure
    begin
      LoadGraphToImg(FTempBmpFile, NIL);
    end,
    EAssertionFailed);
end;


procedure TTestGraphLoader.TestLoadGraphToImg_ValidCall;
var
  Image: TImage;
begin
  CreateTempBmpFile;

  Image:= TImage.Create(NIL);
  TRY
    Assert.WillNotRaise(
      procedure
      begin
        LoadGraphToImg(FTempBmpFile, Image);
      end);

    Assert.IsNotNull(Image.Picture.Bitmap, 'Image should have a bitmap assigned');
  FINALLY
    FreeAndNil(Image);
  END;
end;


{ LoadToTImage Tests }

procedure TTestGraphLoader.TestLoadToTImage_NilImage;
begin
  CreateTempBmpFile;

  Assert.WillRaise(
    procedure
    begin
      LoadToTImage(FTempBmpFile, True, NIL);
    end,
    EAssertionFailed);
end;


{ LoadBMP Tests }

procedure TTestGraphLoader.TestLoadBMP_NonExistentFile;
begin
  Assert.WillRaise(
    procedure
    begin
      LoadBMP('C:\NonExistent\Path\file.bmp');
    end,
    EAssertionFailed);
end;


procedure TTestGraphLoader.TestLoadBMP_ValidFile;
var
  Bmp: TBitmap;
begin
  CreateTempBmpFile;

  Bmp:= LoadBMP(FTempBmpFile);
  TRY
    Assert.IsNotNull(Bmp, 'LoadBMP should return a bitmap');
    Assert.AreEqual(100, Bmp.Width, 'Width should be 100');
    Assert.AreEqual(100, Bmp.Height, 'Height should be 100');
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


{ LoadPNG Tests }

procedure TTestGraphLoader.TestLoadPNG_NonExistentFile;
begin
  Assert.WillRaise(
    procedure
    begin
      LoadPNG('C:\NonExistent\Path\file.png');
    end,
    EAssertionFailed);
end;


procedure TTestGraphLoader.TestLoadPNG_ValidFile;
var
  Bmp: TBitmap;
begin
  CreateTempPngFile;

  Bmp:= LoadPNG(FTempPngFile);
  TRY
    Assert.IsNotNull(Bmp, 'LoadPNG should return a bitmap');
    Assert.AreEqual(100, Bmp.Width, 'Width should be 100');
    Assert.AreEqual(100, Bmp.Height, 'Height should be 100');
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


{ LoadJpg Tests }

procedure TTestGraphLoader.TestLoadJpg_NonExistentFile;
begin
  Assert.WillRaise(
    procedure
    begin
      LoadJpg('C:\NonExistent\Path\file.jpg');
    end,
    EAssertionFailed);
end;


procedure TTestGraphLoader.TestLoadJpg_ValidFile;
var
  Bmp: TBitmap;
begin
  CreateTempJpgFile;

  Bmp:= LoadJpg(FTempJpgFile);
  TRY
    Assert.IsNotNull(Bmp, 'LoadJpg should return a bitmap');
    Assert.AreEqual(100, Bmp.Width, 'Width should be 100');
    Assert.AreEqual(100, Bmp.Height, 'Height should be 100');
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


procedure TTestGraphLoader.TestLoadJpg_WithScale;
var
  Bmp: TBitmap;
begin
  CreateTempJpgFile;

  Bmp:= LoadJpg(FTempJpgFile, jsHalf);
  TRY
    Assert.IsNotNull(Bmp, 'LoadJpg should return a bitmap');
    // With jsHalf, dimensions should be approximately halved
    Assert.IsTrue(Bmp.Width <= 100, 'Width should be <= 100 with jsHalf');
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


{ LoadGIF Tests }

procedure TTestGraphLoader.TestLoadGIF_NonExistentFile;
begin
  Assert.WillRaise(
    procedure
    begin
      LoadGIF('C:\NonExistent\Path\file.gif');
    end,
    EAssertionFailed);
end;


procedure TTestGraphLoader.TestLoadGIF_ValidFile;
var
  Bmp: TBitmap;
begin
  CreateTempGifFile;

  Bmp:= LoadGIF(FTempGifFile);
  TRY
    Assert.IsNotNull(Bmp, 'LoadGIF should return a bitmap');
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


procedure TTestGraphLoader.TestLoadGIF_FrameCount;
var
  Bmp: TBitmap;
  FrameCount: Cardinal;
begin
  CreateTempGifFile;

  Bmp:= LoadGIF(FTempGifFile, FrameCount);
  TRY
    Assert.IsNotNull(Bmp, 'LoadGIF should return a bitmap');
    Assert.IsTrue(FrameCount >= 1, 'FrameCount should be >= 1 for valid GIF');
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


{ loadGraphWic Tests }

procedure TTestGraphLoader.TestLoadGraphWic_NonExistentFile;
begin
  Assert.WillRaise(
    procedure
    begin
      loadGraphWic('C:\NonExistent\Path\file.png');
    end,
    EAssertionFailed);
end;


procedure TTestGraphLoader.TestLoadGraphWic_ValidPngFile;
var
  Bmp: TBitmap;
begin
  CreateTempPngFile;

  Bmp:= loadGraphWic(FTempPngFile);
  TRY
    Assert.IsNotNull(Bmp, 'loadGraphWic should return a bitmap');
    Assert.AreEqual(100, Bmp.Width, 'Width should be 100');
    Assert.AreEqual(100, Bmp.Height, 'Height should be 100');
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


{ LoadGraphAsGrayScale Tests }

procedure TTestGraphLoader.TestLoadGraphAsGrayScale_ValidFile;
var
  Bmp: TBitmap;
begin
  CreateTempBmpFile;

  Bmp:= LoadGraphAsGrayScale(FTempBmpFile);
  TRY
    Assert.IsNotNull(Bmp, 'LoadGraphAsGrayScale should return a bitmap');
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


procedure TTestGraphLoader.TestLoadGraphAsGrayScale_WithBMP_NilBMP;
begin
  CreateTempBmpFile;

  Assert.WillRaise(
    procedure
    begin
      LoadGraphAsGrayScale(FTempBmpFile, NIL);
    end,
    EAssertionFailed);
end;


{ DetectGraphSignature Tests }

procedure TTestGraphLoader.TestDetectGraphSignature_BmpFile;
var
  Signature: Integer;
begin
  CreateTempBmpFile;

  Signature:= DetectGraphSignature(FTempBmpFile);

  Assert.AreEqual(1, Signature, 'BMP signature should be 1');
end;


procedure TTestGraphLoader.TestDetectGraphSignature_PngFile;
var
  Signature: Integer;
begin
  CreateTempPngFile;

  Signature:= DetectGraphSignature(FTempPngFile);

  Assert.AreEqual(2, Signature, 'PNG signature should be 2');
end;


procedure TTestGraphLoader.TestDetectGraphSignature_JpgFile;
var
  Signature: Integer;
begin
  CreateTempJpgFile;

  Signature:= DetectGraphSignature(FTempJpgFile);

  Assert.AreEqual(4, Signature, 'JPG signature should be 4');
end;


procedure TTestGraphLoader.TestDetectGraphSignature_GifFile;
var
  Signature: Integer;
begin
  CreateTempGifFile;

  Signature:= DetectGraphSignature(FTempGifFile);

  Assert.AreEqual(3, Signature, 'GIF signature should be 3');
end;


{ CheckValidImage Tests }

procedure TTestGraphLoader.TestCheckValidImage_ValidFile;
var
  IsValid: Boolean;
begin
  CreateTempBmpFile;

  IsValid:= CheckValidImage(FTempBmpFile);

  Assert.IsTrue(IsValid, 'Valid BMP file should return True');
end;


procedure TTestGraphLoader.TestCheckValidImage_InvalidFile;
var
  IsValid: Boolean;
  InvalidFile: string;
  Stream: TFileStream;
begin
  InvalidFile:= TPath.Combine(FTempDir, 'invalid.dat');
  Stream:= TFileStream.Create(InvalidFile, fmCreate);
  TRY
    Stream.WriteBuffer('NOT AN IMAGE', 12);
  FINALLY
    FreeAndNil(Stream);
  END;

  TRY
    IsValid:= CheckValidImage(InvalidFile);
    Assert.IsFalse(IsValid, 'Invalid file should return False');
  FINALLY
    DeleteFile(InvalidFile);
  END;
end;


{ LoadTPicture Tests }

procedure TTestGraphLoader.TestLoadTPicture_NonExistentFile;
begin
  Assert.WillRaise(
    procedure
    begin
      LoadTPicture('C:\NonExistent\Path\file.bmp');
    end,
    EAssertionFailed);
end;


procedure TTestGraphLoader.TestLoadTPicture_ValidFile;
var
  Pic: TPicture;
begin
  CreateTempBmpFile;

  Pic:= LoadTPicture(FTempBmpFile);
  TRY
    Assert.IsNotNull(Pic, 'LoadTPicture should return a picture');
    Assert.IsNotNull(Pic.Graphic, 'Picture should have a graphic');
  FINALLY
    FreeAndNil(Pic);
  END;
end;


{ ExtractThumbnail Tests }

procedure TTestGraphLoader.TestExtractThumbnail_NonExistentFile;
begin
  Assert.WillRaise(
    procedure
    begin
      ExtractThumbnail('C:\NonExistent\Path\file.jpg', 100);
    end,
    EAssertionFailed);
end;


procedure TTestGraphLoader.TestExtractThumbnail_ValidFile;
var
  Bmp: TBitmap;
begin
  CreateTempBmpFile;

  Bmp:= ExtractThumbnail(FTempBmpFile, 50);
  TRY
    Assert.IsNotNull(Bmp, 'ExtractThumbnail should return a bitmap');
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestGraphLoader);

end.
