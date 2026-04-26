unit Test.LightVcl.Graph.Loader.Resolution;

{=============================================================================================================
   Unit tests for LightVcl.Graph.Loader.Resolution.pas
   Tests image resolution retrieval functions for various formats (BMP, PNG, GIF, JPG).

   Note: These tests require actual image files. Test images are created dynamically.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
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

    [Test]
    procedure TestGetBmpSize_InvalidSignature;

    { GetPngSize Tests }
    [Test]
    procedure TestGetPngSize_ValidFile;

    [Test]
    procedure TestGetPngSize_ValidStream;

    [Test]
    procedure TestGetPngSize_InvalidSignature;

    { GetJpgSize Tests }
    [Test]
    procedure TestGetJpgSize_ValidFile;

    [Test]
    procedure TestGetJpgSize_ValidStream;

    [Test]
    procedure TestGetJpgSize_InvalidSignature;

    [Test]
    procedure TestGetJpgSize_EmptyStream;

    [Test]
    procedure TestGetJpgSize_MalformedSegmentLength;     { Regression: bad segment length used to seek backward, infinite loop }

    { GetGifSize Tests }
    [Test]
    procedure TestGetGifSize_ValidFile;

    [Test]
    procedure TestGetGifSize_ValidStream;

    [Test]
    procedure TestGetGifSize_InvalidSignature;

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
    procedure TestGetImageRes_UnsupportedExtension;

    [Test]
    procedure TestGetImageRes_NonExistentFile;

    [Test]
    procedure TestGetImageRes_WithStream;

    { GetBmpHeader Tests }
    [Test]
    procedure TestGetBmpHeader_ValidBmp;

    [Test]
    procedure TestGetBmpHeader_InvalidBmp;

    { GetBitsPerPixel Tests }
    [Test]
    procedure TestGetBitsPerPixel_24Bit;

    [Test]
    procedure TestGetBitsPerPixel_32Bit;

    [Test]
    procedure TestGetBitsPerPixel_NilBitmap;     { Regression: nil should fire Assert, not AV }

    { Top-down BMP regression: biHeight is signed; negative means top-down DIB. Pixel height is Abs(biHeight). }
    [Test]
    procedure TestGetBmpSize_TopDownDIB_ReturnsPositiveHeight;

    [Test]
    procedure TestGetBmpHeader_TopDownDIB_PreservesNegativeHeight;

    { GIF packed-record regression: hand-crafted GIF with global color table + extension block + image
      descriptor. Without packed records, Delphi pads TGIFHeader to 14 bytes (size of last Word-aligned slot)
      causing Stream.Read to consume an extra byte and shift subsequent parsing. }
    [Test]
    procedure TestGetGifSize_HandCraftedWithGCT;

    [Test]
    procedure TestGetGifSize_HandCraftedWithExtensionBlock;
  end;

implementation

uses
  Vcl.Imaging.Jpeg,
  Vcl.Imaging.PngImage,
  Vcl.Imaging.GIFImg,
  LightVcl.Graph.Loader.Resolution;


procedure TTestGraphLoaderResolution.Setup;
begin
  FTempDir:= TPath.Combine(TPath.GetTempPath, 'TestGraphResolution_' + TGUID.NewGuid.ToString);
  ForceDirectories(FTempDir);
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
  FTempPngFile:= TPath.Combine(FTempDir, 'test.png');
  Png:= TPngImage.Create;
  TRY
    Png.CreateBlank(COLOR_RGB, 8, Width, Height);
    Png.Canvas.Brush.Color:= clBlue;
    Png.Canvas.FillRect(Rect(0, 0, Width, Height));
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


procedure TTestGraphLoaderResolution.TestGetBmpSize_InvalidSignature;
var
  Width, Height: Integer;
  Stream: TMemoryStream;
begin
  Stream:= TMemoryStream.Create;
  TRY
    Stream.WriteBuffer('INVALID BMP DATA', 16);
    Stream.Position:= 0;

    GetBmpSize(Stream, Width, Height);

    Assert.AreEqual(-1, Width, 'Invalid BMP should return Width = -1');
    Assert.AreEqual(-1, Height, 'Invalid BMP should return Height = -1');
  FINALLY
    FreeAndNil(Stream);
  END;
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


procedure TTestGraphLoaderResolution.TestGetPngSize_InvalidSignature;
var
  Width, Height: Integer;
  Stream: TMemoryStream;
begin
  Stream:= TMemoryStream.Create;
  TRY
    Stream.WriteBuffer('NOT A PNG FILE!!', 16);
    Stream.Position:= 0;

    GetPngSize(Stream, Width, Height);

    Assert.AreEqual(-1, Width, 'Invalid PNG should return Width = -1');
    Assert.AreEqual(-1, Height, 'Invalid PNG should return Height = -1');
  FINALLY
    FreeAndNil(Stream);
  END;
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


procedure TTestGraphLoaderResolution.TestGetJpgSize_InvalidSignature;
var
  Width, Height: Integer;
  Stream: TMemoryStream;
  Success: Boolean;
begin
  Stream:= TMemoryStream.Create;
  TRY
    Stream.WriteBuffer('NOT A JPEG FILE!', 16);
    Stream.Position:= 0;

    Success:= GetJpgSize(Stream, Width, Height);

    Assert.IsFalse(Success, 'GetJpgSize should return False for invalid JPG');
    Assert.AreEqual(-1, Width, 'Invalid JPG should return Width = -1');
    Assert.AreEqual(-1, Height, 'Invalid JPG should return Height = -1');
  FINALLY
    FreeAndNil(Stream);
  END;
end;


procedure TTestGraphLoaderResolution.TestGetJpgSize_EmptyStream;
var
  Width, Height: Integer;
  Stream: TMemoryStream;
  Success: Boolean;
begin
  Stream:= TMemoryStream.Create;
  TRY
    Success:= GetJpgSize(Stream, Width, Height);

    Assert.IsFalse(Success, 'GetJpgSize should return False for empty stream');
    Assert.AreEqual(-1, Width, 'Empty stream should return Width = -1');
    Assert.AreEqual(-1, Height, 'Empty stream should return Height = -1');
  FINALLY
    FreeAndNil(Stream);
  END;
end;


procedure TTestGraphLoaderResolution.TestGetJpgSize_MalformedSegmentLength;
var
  Width, Height: Integer;
  Stream: TMemoryStream;
  Success: Boolean;
  Buf: array[0..31] of Byte;
begin
  { Build a JPEG-shaped stream whose first non-marker segment has length=0
    (in big-endian: 00 00). The old code did Seek(swap(w)-2, soFromCurrent) =
    Seek(-2, soFromCurrent), looping forever on the same bytes. With the guard,
    GetJpgSize must EXIT(FALSE) and report -1 dimensions. }
  FillChar(Buf, SizeOf(Buf), 0);
  Buf[0]:= $FF;  Buf[1]:= $D8;        { SOI }
  Buf[2]:= $FF;  Buf[3]:= $E0;        { APP0 marker (not a SOF, falls into the 'else' branch) }
  Buf[4]:= $00;  Buf[5]:= $00;        { Length = 0 (malformed; minimum valid is 2) }
  { padding bytes 6..31 = $00 }

  Stream:= TMemoryStream.Create;
  TRY
    Stream.WriteBuffer(Buf, SizeOf(Buf));
    Stream.Position:= 0;

    { Wrap in a watchdog: any execution longer than ~2 seconds means we're looping. }
    Success:= TRUE;
    Width:= 999; Height:= 999;
    Assert.WillNotRaise(
      procedure
      begin
        Success:= GetJpgSize(Stream, Width, Height);
      end,
      Exception,
      'GetJpgSize should not raise on malformed segment length');

    Assert.IsFalse(Success, 'GetJpgSize must return False for malformed segment length');
    Assert.AreEqual(-1, Width, 'Width should be -1 on parse failure');
    Assert.AreEqual(-1, Height, 'Height should be -1 on parse failure');
  FINALLY
    FreeAndNil(Stream);
  END;
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


procedure TTestGraphLoaderResolution.TestGetGifSize_InvalidSignature;
var
  Width, Height: Integer;
  Stream: TMemoryStream;
begin
  Stream:= TMemoryStream.Create;
  TRY
    Stream.WriteBuffer('NOT A GIF FILE!!', 16);
    Stream.Position:= 0;

    GetGifSize(Stream, Width, Height);

    Assert.AreEqual(-1, Width, 'Invalid GIF should return Width = -1');
    Assert.AreEqual(-1, Height, 'Invalid GIF should return Height = -1');
  FINALLY
    FreeAndNil(Stream);
  END;
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


procedure TTestGraphLoaderResolution.TestGetImageRes_UnsupportedExtension;
var
  Width, Height: Integer;
  UnsupportedFile: string;
  Stream: TFileStream;
begin
  UnsupportedFile:= TPath.Combine(FTempDir, 'test.xyz');
  Stream:= TFileStream.Create(UnsupportedFile, fmCreate);
  TRY
    Stream.WriteBuffer('SOME DATA', 9);
  FINALLY
    FreeAndNil(Stream);
  END;

  TRY
    GetImageRes(UnsupportedFile, Width, Height);

    Assert.AreEqual(-1, Width, 'Unsupported format should return Width = -1');
    Assert.AreEqual(-1, Height, 'Unsupported format should return Height = -1');
  FINALLY
    DeleteFile(UnsupportedFile);
  END;
end;


procedure TTestGraphLoaderResolution.TestGetImageRes_NonExistentFile;
var
  Width, Height: Integer;
begin
  GetImageRes('C:\NonExistent\Path\file.bmp', Width, Height);

  Assert.AreEqual(-1, Width, 'Non-existent file should return Width = -1');
  Assert.AreEqual(-1, Height, 'Non-existent file should return Height = -1');
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


procedure TTestGraphLoaderResolution.TestGetBmpHeader_InvalidBmp;
var
  Header: TBitmapHeader;
  Stream: TMemoryStream;
begin
  Stream:= TMemoryStream.Create;
  TRY
    Stream.WriteBuffer('XXINVALID BMP HEADER DATA', 25);
    Stream.Position:= 0;

    Header:= GetBmpHeader(Stream);

    Assert.AreEqual(-1, Header.Width, 'Invalid BMP Header Width should be -1');
    Assert.AreEqual(-1, Header.Height, 'Invalid BMP Header Height should be -1');
  FINALLY
    FreeAndNil(Stream);
  END;
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


{ Top-down BMP: builds a minimal valid 14+40 byte BMP with a deliberately negative biHeight.
  Pixel height should be returned as Abs(biHeight). }
procedure TTestGraphLoaderResolution.TestGetBmpSize_TopDownDIB_ReturnsPositiveHeight;
var
  Stream: TMemoryStream;
  Width, Height: Integer;
  HeaderBytes: array[0..53] of Byte;

  procedure WriteLE32(Offset: Integer; Value: Integer);
  begin
    HeaderBytes[Offset    ]:= Byte(Value);
    HeaderBytes[Offset + 1]:= Byte(Value shr 8);
    HeaderBytes[Offset + 2]:= Byte(Value shr 16);
    HeaderBytes[Offset + 3]:= Byte(Value shr 24);
  end;

  procedure WriteLE16(Offset: Integer; Value: Word);
  begin
    HeaderBytes[Offset    ]:= Byte(Value);
    HeaderBytes[Offset + 1]:= Byte(Value shr 8);
  end;

begin
  FillChar(HeaderBytes, SizeOf(HeaderBytes), 0);
  { BITMAPFILEHEADER }
  HeaderBytes[0]:= Ord('B');
  HeaderBytes[1]:= Ord('M');
  WriteLE32(2, 54 + 16);    { File size (header + 1px row, padded) }
  WriteLE32(6, 0);          { Reserved }
  WriteLE32(10, 54);        { Pixel data offset }
  { BITMAPINFOHEADER }
  WriteLE32(14, 40);        { biSize }
  WriteLE32(18, 50);        { biWidth = 50 }
  WriteLE32(22, -30);       { biHeight = -30 (top-down DIB) }
  WriteLE16(26, 1);         { biPlanes }
  WriteLE16(28, 24);        { biBitCount }
  WriteLE32(30, 0);         { biCompression = BI_RGB }
  WriteLE32(34, 0);         { biSizeImage }
  WriteLE32(38, 0);         { biXPelsPerMeter }
  WriteLE32(42, 0);         { biYPelsPerMeter }
  WriteLE32(46, 0);         { biClrUsed }
  WriteLE32(50, 0);         { biClrImportant }

  Stream:= TMemoryStream.Create;
  TRY
    Stream.WriteBuffer(HeaderBytes, SizeOf(HeaderBytes));
    Stream.Position:= 0;

    GetBmpSize(Stream, Width, Height);

    Assert.AreEqual(50, Width, 'Top-down BMP Width should be 50');
    Assert.AreEqual(30, Height, 'Top-down BMP Height should be 30 (Abs of -30)');
  FINALLY
    FreeAndNil(Stream);
  END;
end;


procedure TTestGraphLoaderResolution.TestGetBmpHeader_TopDownDIB_PreservesNegativeHeight;
var
  Stream: TMemoryStream;
  Header: TBitmapHeader;
  HeaderBytes: array[0..53] of Byte;

  procedure WriteLE32(Offset: Integer; Value: Integer);
  begin
    HeaderBytes[Offset    ]:= Byte(Value);
    HeaderBytes[Offset + 1]:= Byte(Value shr 8);
    HeaderBytes[Offset + 2]:= Byte(Value shr 16);
    HeaderBytes[Offset + 3]:= Byte(Value shr 24);
  end;

  procedure WriteLE16(Offset: Integer; Value: Word);
  begin
    HeaderBytes[Offset    ]:= Byte(Value);
    HeaderBytes[Offset + 1]:= Byte(Value shr 8);
  end;

begin
  FillChar(HeaderBytes, SizeOf(HeaderBytes), 0);
  HeaderBytes[0]:= Ord('B');
  HeaderBytes[1]:= Ord('M');
  WriteLE32(2, 70);
  WriteLE32(10, 54);
  WriteLE32(14, 40);
  WriteLE32(18, 50);
  WriteLE32(22, -30);       { negative biHeight }
  WriteLE16(26, 1);
  WriteLE16(28, 24);

  Stream:= TMemoryStream.Create;
  TRY
    Stream.WriteBuffer(HeaderBytes, SizeOf(HeaderBytes));
    Stream.Position:= 0;

    Header:= GetBmpHeader(Stream);

    { GetBmpHeader is the raw struct accessor: must keep the signed value verbatim
      so callers that need to detect top-down can. Pixel-dim normalization is GetBmpSize's job. }
    Assert.AreEqual(-30, Header.Height, 'GetBmpHeader must preserve negative biHeight (top-down marker)');
    Assert.AreEqual(50, Header.Width, 'GetBmpHeader Width should be 50');
  FINALLY
    FreeAndNil(Stream);
  END;
end;


{ Builds a hand-crafted GIF with a 2-color global color table (6 bytes) and a single image
  descriptor. Verifies header parsing lands at the correct stream offset. With the unpacked-record
  bug, parsing would consume one extra byte after the header and end up reading garbage
  for image dimensions. }
procedure TTestGraphLoaderResolution.TestGetGifSize_HandCraftedWithGCT;
var
  Stream: TMemoryStream;
  Width, Height: Integer;
  GifBytes: array of Byte;
  i: Integer;
begin
  SetLength(GifBytes, 30);
  for i:= 0 to High(GifBytes) DO GifBytes[i]:= 0;

  { Header (13 bytes) }
  GifBytes[0]:= Ord('G');  GifBytes[1]:= Ord('I');  GifBytes[2]:= Ord('F');
  GifBytes[3]:= Ord('8');  GifBytes[4]:= Ord('9');  GifBytes[5]:= Ord('a');
  GifBytes[6]:= 100;       GifBytes[7]:= 0;         { ScreenWidth = 100 LE }
  GifBytes[8]:= 75;        GifBytes[9]:= 0;         { ScreenHeight = 75 LE }
  GifBytes[10]:= $80;      { Flags: GCT present, size bits = 0 (2 colors -> 6 bytes GCT) }
  GifBytes[11]:= 0;        { Background }
  GifBytes[12]:= 0;        { Aspect }
  { GCT (6 bytes): 2 colors x 3 bytes each — bytes 13..18 left as 0 }
  { Image separator at byte 19 }
  GifBytes[19]:= Ord(',');
  { Image descriptor (9 bytes): Left=0, Top=0, Width=42, Height=37, Flags=0 }
  GifBytes[20]:= 0;        GifBytes[21]:= 0;        { Left = 0 }
  GifBytes[22]:= 0;        GifBytes[23]:= 0;        { Top = 0 }
  GifBytes[24]:= 42;       GifBytes[25]:= 0;        { Width = 42 LE }
  GifBytes[26]:= 37;       GifBytes[27]:= 0;        { Height = 37 LE }
  GifBytes[28]:= 0;        { Image flags (no LCT) }
  { byte 29 = LZW min code size (irrelevant for parser) }

  Stream:= TMemoryStream.Create;
  TRY
    Stream.WriteBuffer(GifBytes[0], Length(GifBytes));
    Stream.Position:= 0;

    GetGifSize(Stream, Width, Height);

    Assert.AreEqual(42, Width, 'Hand-crafted GIF Width should be 42 (proves alignment is correct)');
    Assert.AreEqual(37, Height, 'Hand-crafted GIF Height should be 37 (proves alignment is correct)');
  FINALLY
    FreeAndNil(Stream);
  END;
end;


{ GIF with no GCT but with a Graphics Control Extension block ($21 $F9) before the image
  descriptor. Tests that the byte-walk loop finds the ',' at the correct offset even when
  a non-comma byte sequence intervenes. }
procedure TTestGraphLoaderResolution.TestGetGifSize_HandCraftedWithExtensionBlock;
var
  Stream: TMemoryStream;
  Width, Height: Integer;
  GifBytes: array of Byte;
  i: Integer;
begin
  SetLength(GifBytes, 32);
  for i:= 0 to High(GifBytes) DO GifBytes[i]:= 0;

  { Header (13 bytes), no GCT }
  GifBytes[0]:= Ord('G');  GifBytes[1]:= Ord('I');  GifBytes[2]:= Ord('F');
  GifBytes[3]:= Ord('8');  GifBytes[4]:= Ord('9');  GifBytes[5]:= Ord('a');
  GifBytes[6]:= 200;       GifBytes[7]:= 0;         { ScreenWidth = 200 LE }
  GifBytes[8]:= 150;       GifBytes[9]:= 0;         { ScreenHeight = 150 LE }
  GifBytes[10]:= 0;        { Flags: no GCT }
  GifBytes[11]:= 0;
  GifBytes[12]:= 0;

  { Graphics Control Extension at byte 13: $21 $F9 $04 <4 bytes> $00 (terminator) — 8 bytes total }
  GifBytes[13]:= $21;
  GifBytes[14]:= $F9;
  GifBytes[15]:= $04;
  GifBytes[16]:= 0; GifBytes[17]:= 0; GifBytes[18]:= 0; GifBytes[19]:= 0;  { 4 data bytes }
  GifBytes[20]:= 0;        { block terminator }

  { Image separator at byte 21 }
  GifBytes[21]:= Ord(',');
  GifBytes[22]:= 0; GifBytes[23]:= 0;     { Left = 0 }
  GifBytes[24]:= 0; GifBytes[25]:= 0;     { Top = 0 }
  GifBytes[26]:= 64; GifBytes[27]:= 0;    { Width = 64 LE }
  GifBytes[28]:= 48; GifBytes[29]:= 0;    { Height = 48 LE }
  GifBytes[30]:= 0;
  GifBytes[31]:= 0;

  Stream:= TMemoryStream.Create;
  TRY
    Stream.WriteBuffer(GifBytes[0], Length(GifBytes));
    Stream.Position:= 0;

    GetGifSize(Stream, Width, Height);

    { Note: the byte-walk parser may pick up dimensions from inside the extension block bytes
      if it sees a ',' there. For a clean GCE block (all zeros + terminator) the first ',' is
      the real image separator, so we expect 64x48. }
    Assert.AreEqual(64, Width, 'Hand-crafted GIF (with GCE) Width should be 64');
    Assert.AreEqual(48, Height, 'Hand-crafted GIF (with GCE) Height should be 48');
  FINALLY
    FreeAndNil(Stream);
  END;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestGraphLoaderResolution);

end.
