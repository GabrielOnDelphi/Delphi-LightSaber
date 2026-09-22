unit Test.LightVcl.Graph.Loader.Resolution;

{=============================================================================================================
   Unit tests for LightVcl.Graph.Loader.Resolution.pas

   Only the tests that need the VCL live here:
     - GetBitsPerPixel, whose parameter is Vcl.Graphics.TBitmap
     - every test that WRITES a real BMP/PNG/JPG/GIF file first, using TBitmap, TPngImage, TJPEGImage
       or TGIFImage, and then reads its resolution back

   The header-parsing routines themselves moved to LightCore.Graph.Loader.Resolution.pas, and the tests
   that build their bytes by hand moved with them, into Test.LightCore.Graph.Loader.Resolution.pas.

   Note: These tests require actual image files. Test images are created dynamically.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  LightCore.IO,
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  Vcl.Graphics;

type
  [TestFixture]
  TTestGraphLoaderResolution = class
  private
    FTempDir: string;
    FTempBmpFile: string;
    FTempPngFile: string;
    FTempJpgFile: string;
    FTempGifFile: string;
    procedure CreateTempBmpFile(Width, Height: Integer);
    procedure CreateTempPngFile(Width, Height: Integer);
    procedure CreateTempJpgFile(Width, Height: Integer);
    procedure CreateTempGifFile(Width, Height: Integer);
    procedure CleanupTempFiles;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { GetBmpSize Tests }
    [Test]
    procedure TestGetBmpSize_ValidFile;

    [Test]
    procedure TestGetBmpSize_ValidStream;

    { GetPngSize Tests }
    [Test]
    procedure TestGetPngSize_ValidFile;

    [Test]
    procedure TestGetPngSize_ValidStream;

    { GetJpgSize Tests }
    [Test]
    procedure TestGetJpgSize_ValidFile;

    [Test]
    procedure TestGetJpgSize_ValidStream;

    { GetGifSize Tests }
    [Test]
    procedure TestGetGifSize_ValidFile;

    [Test]
    procedure TestGetGifSize_ValidStream;

    { GetImageRes Tests }
    [Test]
    procedure TestGetImageRes_BmpFile;

    [Test]
    procedure TestGetImageRes_PngFile;

    [Test]
    procedure TestGetImageRes_JpgFile;

    [Test]
    procedure TestGetImageRes_GifFile;

    [Test]
    procedure TestGetImageRes_WithStream;

    { GetBmpHeader Tests }
    [Test]
    procedure TestGetBmpHeader_ValidBmp;

    { GetBitsPerPixel Tests }
    [Test]
    procedure TestGetBitsPerPixel_24Bit;

    [Test]
    procedure TestGetBitsPerPixel_32Bit;

    [Test]
    procedure TestGetBitsPerPixel_NilBitmap;     { Regression: nil should fire Assert, not AV }
  end;

implementation

uses
  Vcl.Imaging.Jpeg,
  Vcl.Imaging.PngImage,
  Vcl.Imaging.GIFImg,
  LightCore.Graph.Loader.Resolution,     { GetImageRes, GetBmpSize, GetPngSize, GetJpgSize, GetGifSize, GetBmpHeader, TBitmapHeader }
  LightVcl.Graph.Loader.Resolution;      { GetBitsPerPixel }


procedure TTestGraphLoaderResolution.Setup;
begin
  FTempDir:= TPath.Combine(TPath.GetTempPath, 'TestGraphResolution_' + TGUID.NewGuid.ToString);
  ForceDirectoriesE(FTempDir);
  FTempBmpFile:= '';
  FTempPngFile:= '';
  FTempJpgFile:= '';
  FTempGifFile:= '';
end;


procedure TTestGraphLoaderResolution.TearDown;
begin
  CleanupTempFiles;
  if DirectoryExists(FTempDir)
  then RemoveDir(FTempDir);
end;


procedure TTestGraphLoaderResolution.CleanupTempFiles;
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


procedure TTestGraphLoaderResolution.CreateTempBmpFile(Width, Height: Integer);
var
  Bmp: TBitmap;
begin
  FTempBmpFile:= TPath.Combine(FTempDir, 'test.bmp');
  Bmp:= TBitmap.Create;
  TRY
    Bmp.Width:= Width;
    Bmp.Height:= Height;
    Bmp.PixelFormat:= pf24bit;
    Bmp.Canvas.Brush.Color:= clRed;
    Bmp.Canvas.FillRect(Rect(0, 0, Width, Height));
    Bmp.SaveToFile(FTempBmpFile);
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


procedure TTestGraphLoaderResolution.CreateTempPngFile(Width, Height: Integer);
var
  Png: TPngImage;
begin
  { Use CreateBlank as a CONSTRUCTOR. The previous pattern
       Png := TPngImage.Create;
       Png.CreateBlank(...)
    leaks because CreateBlank's body calls Create again (Vcl.Imaging.pngimage.pas:4347), which
    re-runs Create's body and overwrites fCanvas + fChunkList — orphaning the first allocations. }
  FTempPngFile:= TPath.Combine(FTempDir, 'test.png');
  Png:= TPngImage.CreateBlank(COLOR_RGB, 8, Width, Height);
  TRY
    Png.SaveToFile(FTempPngFile);
  FINALLY
    FreeAndNil(Png);
  END;
end;


procedure TTestGraphLoaderResolution.CreateTempJpgFile(Width, Height: Integer);
var
  Bmp: TBitmap;
  Jpg: TJPEGImage;
begin
  FTempJpgFile:= TPath.Combine(FTempDir, 'test.jpg');
  Bmp:= TBitmap.Create;
  TRY
    Bmp.Width:= Width;
    Bmp.Height:= Height;
    Bmp.PixelFormat:= pf24bit;
    Bmp.Canvas.Brush.Color:= clGreen;
    Bmp.Canvas.FillRect(Rect(0, 0, Width, Height));

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


procedure TTestGraphLoaderResolution.CreateTempGifFile(Width, Height: Integer);
var
  Gif: TGIFImage;
  Bmp: TBitmap;
begin
  FTempGifFile:= TPath.Combine(FTempDir, 'test.gif');

  Bmp:= TBitmap.Create;
  TRY
    Bmp.Width:= Width;
    Bmp.Height:= Height;
    Bmp.PixelFormat:= pf24bit;
    Bmp.Canvas.Brush.Color:= clYellow;
    Bmp.Canvas.FillRect(Rect(0, 0, Width, Height));

    Gif:= TGIFImage.Create;
    TRY
      Gif.Assign(Bmp);
      Gif.SaveToFile(FTempGifFile);
    FINALLY
      FreeAndNil(Gif);
    END;
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


{ GetBmpSize Tests }

procedure TTestGraphLoaderResolution.TestGetBmpSize_ValidFile;
var
  Width, Height: Integer;
begin
  CreateTempBmpFile(150, 200);

  GetBmpSize(FTempBmpFile, Width, Height);

  Assert.AreEqual(150, Width, 'BMP Width should be 150');
  Assert.AreEqual(200, Height, 'BMP Height should be 200');
end;


procedure TTestGraphLoaderResolution.TestGetBmpSize_ValidStream;
var
  Width, Height: Integer;
  Stream: TFileStream;
begin
  CreateTempBmpFile(300, 250);

  Stream:= TFileStream.Create(FTempBmpFile, fmOpenRead OR fmShareDenyNone);
  TRY
    GetBmpSize(Stream, Width, Height);
  FINALLY
    FreeAndNil(Stream);
  END;

  Assert.AreEqual(300, Width, 'BMP Width should be 300');
  Assert.AreEqual(250, Height, 'BMP Height should be 250');
end;


{ GetPngSize Tests }

procedure TTestGraphLoaderResolution.TestGetPngSize_ValidFile;
var
  Width, Height: Integer;
begin
  CreateTempPngFile(400, 300);

  GetPngSize(FTempPngFile, Width, Height);

  Assert.AreEqual(400, Width, 'PNG Width should be 400');
  Assert.AreEqual(300, Height, 'PNG Height should be 300');
end;


procedure TTestGraphLoaderResolution.TestGetPngSize_ValidStream;
var
  Width, Height: Integer;
  Stream: TFileStream;
begin
  CreateTempPngFile(500, 600);

  Stream:= TFileStream.Create(FTempPngFile, fmOpenRead OR fmShareDenyNone);
  TRY
    GetPngSize(Stream, Width, Height);
  FINALLY
    FreeAndNil(Stream);
  END;

  Assert.AreEqual(500, Width, 'PNG Width should be 500');
  Assert.AreEqual(600, Height, 'PNG Height should be 600');
end;


{ GetJpgSize Tests }

procedure TTestGraphLoaderResolution.TestGetJpgSize_ValidFile;
var
  Width, Height: Integer;
  Success: Boolean;
begin
  CreateTempJpgFile(640, 480);

  Success:= GetJpgSize(FTempJpgFile, Width, Height);

  Assert.IsTrue(Success, 'GetJpgSize should return True for valid JPG');
  Assert.AreEqual(640, Width, 'JPG Width should be 640');
  Assert.AreEqual(480, Height, 'JPG Height should be 480');
end;


procedure TTestGraphLoaderResolution.TestGetJpgSize_ValidStream;
var
  Width, Height: Integer;
  Stream: TFileStream;
  Success: Boolean;
begin
  CreateTempJpgFile(800, 600);

  Stream:= TFileStream.Create(FTempJpgFile, fmOpenRead OR fmShareDenyNone);
  TRY
    Success:= GetJpgSize(Stream, Width, Height);
  FINALLY
    FreeAndNil(Stream);
  END;

  Assert.IsTrue(Success, 'GetJpgSize should return True for valid JPG');
  Assert.AreEqual(800, Width, 'JPG Width should be 800');
  Assert.AreEqual(600, Height, 'JPG Height should be 600');
end;


{ GetGifSize Tests }

procedure TTestGraphLoaderResolution.TestGetGifSize_ValidFile;
var
  Width, Height: Integer;
begin
  CreateTempGifFile(320, 240);

  GetGifSize(FTempGifFile, Width, Height);

  Assert.AreEqual(320, Width, 'GIF Width should be 320');
  Assert.AreEqual(240, Height, 'GIF Height should be 240');
end;


procedure TTestGraphLoaderResolution.TestGetGifSize_ValidStream;
var
  Width, Height: Integer;
  Stream: TFileStream;
begin
  CreateTempGifFile(200, 150);

  Stream:= TFileStream.Create(FTempGifFile, fmOpenRead OR fmShareDenyNone);
  TRY
    GetGifSize(Stream, Width, Height);
  FINALLY
    FreeAndNil(Stream);
  END;

  Assert.AreEqual(200, Width, 'GIF Width should be 200');
  Assert.AreEqual(150, Height, 'GIF Height should be 150');
end;


{ GetImageRes Tests }

procedure TTestGraphLoaderResolution.TestGetImageRes_BmpFile;
var
  Width, Height: Integer;
begin
  CreateTempBmpFile(100, 100);

  GetImageRes(FTempBmpFile, Width, Height);

  Assert.AreEqual(100, Width, 'GetImageRes BMP Width should be 100');
  Assert.AreEqual(100, Height, 'GetImageRes BMP Height should be 100');
end;


procedure TTestGraphLoaderResolution.TestGetImageRes_PngFile;
var
  Width, Height: Integer;
begin
  CreateTempPngFile(200, 150);

  GetImageRes(FTempPngFile, Width, Height);

  Assert.AreEqual(200, Width, 'GetImageRes PNG Width should be 200');
  Assert.AreEqual(150, Height, 'GetImageRes PNG Height should be 150');
end;


procedure TTestGraphLoaderResolution.TestGetImageRes_JpgFile;
var
  Width, Height: Integer;
begin
  CreateTempJpgFile(300, 250);

  GetImageRes(FTempJpgFile, Width, Height);

  Assert.AreEqual(300, Width, 'GetImageRes JPG Width should be 300');
  Assert.AreEqual(250, Height, 'GetImageRes JPG Height should be 250');
end;


procedure TTestGraphLoaderResolution.TestGetImageRes_GifFile;
var
  Width, Height: Integer;
begin
  CreateTempGifFile(400, 350);

  GetImageRes(FTempGifFile, Width, Height);

  Assert.AreEqual(400, Width, 'GetImageRes GIF Width should be 400');
  Assert.AreEqual(350, Height, 'GetImageRes GIF Height should be 350');
end;


procedure TTestGraphLoaderResolution.TestGetImageRes_WithStream;
var
  Width, Height: Integer;
  Stream: TFileStream;
begin
  CreateTempBmpFile(256, 128);

  Stream:= TFileStream.Create(FTempBmpFile, fmOpenRead OR fmShareDenyNone);
  TRY
    GetImageRes(FTempBmpFile, Stream, Width, Height);
  FINALLY
    FreeAndNil(Stream);
  END;

  Assert.AreEqual(256, Width, 'GetImageRes with stream BMP Width should be 256');
  Assert.AreEqual(128, Height, 'GetImageRes with stream BMP Height should be 128');
end;


{ GetBmpHeader Tests }

procedure TTestGraphLoaderResolution.TestGetBmpHeader_ValidBmp;
var
  Header: TBitmapHeader;
  Stream: TFileStream;
begin
  CreateTempBmpFile(512, 384);

  Stream:= TFileStream.Create(FTempBmpFile, fmOpenRead OR fmShareDenyNone);
  TRY
    Header:= GetBmpHeader(Stream);
  FINALLY
    FreeAndNil(Stream);
  END;

  Assert.AreEqual(512, Header.Width, 'Header Width should be 512');
  Assert.AreEqual(384, Header.Height, 'Header Height should be 384');
  Assert.AreEqual(24, Integer(Header.BitCount), 'Header BitCount should be 24');
end;


{ GetBitsPerPixel Tests }

procedure TTestGraphLoaderResolution.TestGetBitsPerPixel_24Bit;
var
  Bmp: TBitmap;
  BitsPerPixel: Integer;
begin
  Bmp:= TBitmap.Create;
  TRY
    Bmp.Width:= 100;
    Bmp.Height:= 100;
    Bmp.PixelFormat:= pf24bit;

    BitsPerPixel:= GetBitsPerPixel(Bmp);

    Assert.AreEqual(24, BitsPerPixel, 'pf24bit should return 24');
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


procedure TTestGraphLoaderResolution.TestGetBitsPerPixel_32Bit;
var
  Bmp: TBitmap;
  BitsPerPixel: Integer;
begin
  Bmp:= TBitmap.Create;
  TRY
    Bmp.Width:= 100;
    Bmp.Height:= 100;
    Bmp.PixelFormat:= pf32bit;

    BitsPerPixel:= GetBitsPerPixel(Bmp);

    Assert.AreEqual(32, BitsPerPixel, 'pf32bit should return 32');
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


procedure TTestGraphLoaderResolution.TestGetBitsPerPixel_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      GetBitsPerPixel(NIL);
    end,
    EAssertionFailed,
    'GetBitsPerPixel(nil) must fire its Assert, not access-violate');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestGraphLoaderResolution);

end.
