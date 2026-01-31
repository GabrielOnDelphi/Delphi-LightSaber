unit Test.LightVcl.Graph.Convert;

{=============================================================================================================
   Unit tests for LightVcl.Graph.Convert.pas
   Tests BMP/JPG conversion functions, compression, and stream operations.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.IOUtils,
  Vcl.Graphics,
  Vcl.Imaging.Jpeg;

type
  [TestFixture]
  TTestGraphConvert = class
  private
    FBitmap: TBitmap;
    FTempDir: string;
    procedure FillBitmapWithColor(BMP: TBitmap; Color: TColor);
    function CreateTestJpeg: TJpegImage;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Bmp2Jpg (to TJpegImage) Tests }
    [Test]
    procedure TestBmp2Jpg_BasicCall;

    [Test]
    procedure TestBmp2Jpg_NilBitmap;

    [Test]
    procedure TestBmp2Jpg_ReturnsJpeg;

    [Test]
    procedure TestBmp2Jpg_DefaultCompression;

    [Test]
    procedure TestBmp2Jpg_CustomCompression;

    { Bmp2Jpg (to file) Tests }
    [Test]
    procedure TestBmp2JpgFile_BasicCall;

    [Test]
    procedure TestBmp2JpgFile_NilBitmap;

    [Test]
    procedure TestBmp2JpgFile_EmptyOutputFile;

    [Test]
    procedure TestBmp2JpgFile_CreatesFile;

    [Test]
    procedure TestBmp2JpgFile_CreatesDirectory;

    { Jpeg2Bmp Tests }
    [Test]
    procedure TestJpeg2Bmp_BasicCall;

    [Test]
    procedure TestJpeg2Bmp_NilJpeg;

    [Test]
    procedure TestJpeg2Bmp_ReturnsBitmap;

    [Test]
    procedure TestJpeg2Bmp_Pf24Bit;

    { Graph2Jpg Tests }
    [Test]
    procedure TestGraph2Jpg_BasicCall;

    [Test]
    procedure TestGraph2Jpg_NilGraphic;

    [Test]
    procedure TestGraph2Jpg_EmptyOutputFile;

    [Test]
    procedure TestGraph2Jpg_CreatesFile;

    { Bmp2JpgStream Tests }
    [Test]
    procedure TestBmp2JpgStream_BasicCall;

    [Test]
    procedure TestBmp2JpgStream_NilBitmap;

    [Test]
    procedure TestBmp2JpgStream_ReturnsStream;

    [Test]
    procedure TestBmp2JpgStream_StreamHasContent;

    [Test]
    procedure TestBmp2JpgStream_ValidJpegData;

    { CompressBmp Tests }
    [Test]
    procedure TestCompressBmp_BasicCall;

    [Test]
    procedure TestCompressBmp_NilBitmap;

    [Test]
    procedure TestCompressBmp_ReturnsPositiveSize;

    [Test]
    procedure TestCompressBmp_HigherQualityLargerSize;

    { Recompress (single param) Tests }
    [Test]
    procedure TestRecompress_SingleParam_BasicCall;

    [Test]
    procedure TestRecompress_SingleParam_NilJpeg;

    [Test]
    procedure TestRecompress_SingleParam_ReturnsSize;

    { Recompress (OUT param) Tests }
    [Test]
    procedure TestRecompress_OutParam_BasicCall;

    [Test]
    procedure TestRecompress_OutParam_NilInput;

    [Test]
    procedure TestRecompress_OutParam_CreatesOutput;

    [Test]
    procedure TestRecompress_OutParam_OutputIsValid;

    { Roundtrip Tests }
    [Test]
    procedure TestRoundtrip_BmpToJpgToBmp;

    [Test]
    procedure TestRoundtrip_PreservesDimensions;
  end;

implementation

uses
  LightVcl.Graph.Convert;


procedure TTestGraphConvert.Setup;
begin
  FBitmap:= TBitmap.Create;
  FBitmap.Width:= 100;
  FBitmap.Height:= 80;
  FBitmap.PixelFormat:= pf24bit;
  FillBitmapWithColor(FBitmap, clRed);

  FTempDir:= TPath.Combine(TPath.GetTempPath, 'LightSaberTests_' + IntToStr(Random(100000)));
end;


procedure TTestGraphConvert.TearDown;
begin
  FreeAndNil(FBitmap);

  { Clean up temp directory }
  if TDirectory.Exists(FTempDir)
  then TDirectory.Delete(FTempDir, TRUE);
end;


procedure TTestGraphConvert.FillBitmapWithColor(BMP: TBitmap; Color: TColor);
begin
  BMP.Canvas.Brush.Color:= Color;
  BMP.Canvas.FillRect(Rect(0, 0, BMP.Width, BMP.Height));
end;


function TTestGraphConvert.CreateTestJpeg: TJpegImage;
begin
  Result:= TJpegImage.Create;
  Result.Assign(FBitmap);
end;


{ Bmp2Jpg (to TJpegImage) Tests }

procedure TTestGraphConvert.TestBmp2Jpg_BasicCall;
var
  Jpg: TJpegImage;
begin
  Jpg:= Bmp2Jpg(FBitmap);
  TRY
    Assert.IsNotNull(Jpg, 'Bmp2Jpg should return a JPEG');
  FINALLY
    FreeAndNil(Jpg);
  END;
end;


procedure TTestGraphConvert.TestBmp2Jpg_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      Bmp2Jpg(TBitmap(NIL));
    end,
    Exception);
end;


procedure TTestGraphConvert.TestBmp2Jpg_ReturnsJpeg;
var
  Jpg: TJpegImage;
begin
  Jpg:= Bmp2Jpg(FBitmap);
  TRY
    Assert.IsTrue(Jpg is TJpegImage, 'Should return TJpegImage');
  FINALLY
    FreeAndNil(Jpg);
  END;
end;


procedure TTestGraphConvert.TestBmp2Jpg_DefaultCompression;
var
  Jpg: TJpegImage;
begin
  Jpg:= Bmp2Jpg(FBitmap);
  TRY
    Assert.AreEqual(DelphiJpgQuality, Jpg.CompressionQuality,
      'Default compression should be DelphiJpgQuality');
  FINALLY
    FreeAndNil(Jpg);
  END;
end;


procedure TTestGraphConvert.TestBmp2Jpg_CustomCompression;
var
  Jpg: TJpegImage;
begin
  Jpg:= Bmp2Jpg(FBitmap, 90);
  TRY
    Assert.AreEqual(90, Jpg.CompressionQuality, 'Compression should be 90');
  FINALLY
    FreeAndNil(Jpg);
  END;
end;


{ Bmp2Jpg (to file) Tests }

procedure TTestGraphConvert.TestBmp2JpgFile_BasicCall;
var
  OutputFile: string;
begin
  TDirectory.CreateDirectory(FTempDir);
  OutputFile:= TPath.Combine(FTempDir, 'test.jpg');

  Assert.WillNotRaise(
    procedure
    begin
      Bmp2Jpg(FBitmap, OutputFile);
    end);
end;


procedure TTestGraphConvert.TestBmp2JpgFile_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      Bmp2Jpg(TBitmap(NIL), 'test.jpg');
    end,
    Exception);
end;


procedure TTestGraphConvert.TestBmp2JpgFile_EmptyOutputFile;
begin
  Assert.WillRaise(
    procedure
    begin
      Bmp2Jpg(FBitmap, '');
    end,
    Exception);
end;


procedure TTestGraphConvert.TestBmp2JpgFile_CreatesFile;
var
  OutputFile: string;
begin
  TDirectory.CreateDirectory(FTempDir);
  OutputFile:= TPath.Combine(FTempDir, 'test.jpg');

  Bmp2Jpg(FBitmap, OutputFile);

  Assert.IsTrue(TFile.Exists(OutputFile), 'File should be created');
end;


procedure TTestGraphConvert.TestBmp2JpgFile_CreatesDirectory;
var
  OutputFile: string;
  SubDir: string;
begin
  SubDir:= TPath.Combine(FTempDir, 'SubFolder');
  OutputFile:= TPath.Combine(SubDir, 'test.jpg');

  Bmp2Jpg(FBitmap, OutputFile);

  Assert.IsTrue(TDirectory.Exists(SubDir), 'Directory should be created');
  Assert.IsTrue(TFile.Exists(OutputFile), 'File should be created');
end;


{ Jpeg2Bmp Tests }

procedure TTestGraphConvert.TestJpeg2Bmp_BasicCall;
var
  Jpg: TJpegImage;
  Bmp: TBitmap;
begin
  Jpg:= CreateTestJpeg;
  TRY
    Bmp:= Jpeg2Bmp(Jpg);
    TRY
      Assert.IsNotNull(Bmp, 'Jpeg2Bmp should return a bitmap');
    FINALLY
      FreeAndNil(Bmp);
    END;
  FINALLY
    FreeAndNil(Jpg);
  END;
end;


procedure TTestGraphConvert.TestJpeg2Bmp_NilJpeg;
begin
  Assert.WillRaise(
    procedure
    begin
      Jpeg2Bmp(NIL);
    end,
    Exception);
end;


procedure TTestGraphConvert.TestJpeg2Bmp_ReturnsBitmap;
var
  Jpg: TJpegImage;
  Bmp: TBitmap;
begin
  Jpg:= CreateTestJpeg;
  TRY
    Bmp:= Jpeg2Bmp(Jpg);
    TRY
      Assert.IsTrue(Bmp is TBitmap, 'Should return TBitmap');
    FINALLY
      FreeAndNil(Bmp);
    END;
  FINALLY
    FreeAndNil(Jpg);
  END;
end;


procedure TTestGraphConvert.TestJpeg2Bmp_Pf24Bit;
var
  Jpg: TJpegImage;
  Bmp: TBitmap;
begin
  Jpg:= CreateTestJpeg;
  TRY
    Bmp:= Jpeg2Bmp(Jpg);
    TRY
      Assert.AreEqual(pf24bit, Bmp.PixelFormat, 'Should be 24-bit');
    FINALLY
      FreeAndNil(Bmp);
    END;
  FINALLY
    FreeAndNil(Jpg);
  END;
end;


{ Graph2Jpg Tests }

procedure TTestGraphConvert.TestGraph2Jpg_BasicCall;
var
  OutputFile: string;
begin
  TDirectory.CreateDirectory(FTempDir);
  OutputFile:= TPath.Combine(FTempDir, 'graph.jpg');

  Assert.WillNotRaise(
    procedure
    begin
      Graph2Jpg(FBitmap, OutputFile);
    end);
end;


procedure TTestGraphConvert.TestGraph2Jpg_NilGraphic;
begin
  Assert.WillRaise(
    procedure
    begin
      Graph2Jpg(NIL, 'test.jpg');
    end,
    Exception);
end;


procedure TTestGraphConvert.TestGraph2Jpg_EmptyOutputFile;
begin
  Assert.WillRaise(
    procedure
    begin
      Graph2Jpg(FBitmap, '');
    end,
    Exception);
end;


procedure TTestGraphConvert.TestGraph2Jpg_CreatesFile;
var
  OutputFile: string;
begin
  TDirectory.CreateDirectory(FTempDir);
  OutputFile:= TPath.Combine(FTempDir, 'graph.jpg');

  Graph2Jpg(FBitmap, OutputFile);

  Assert.IsTrue(TFile.Exists(OutputFile), 'File should be created');
end;


{ Bmp2JpgStream Tests }

procedure TTestGraphConvert.TestBmp2JpgStream_BasicCall;
var
  Stream: TStream;
begin
  Stream:= Bmp2JpgStream(FBitmap);
  TRY
    Assert.IsNotNull(Stream, 'Should return a stream');
  FINALLY
    FreeAndNil(Stream);
  END;
end;


procedure TTestGraphConvert.TestBmp2JpgStream_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      Bmp2JpgStream(NIL);
    end,
    Exception);
end;


procedure TTestGraphConvert.TestBmp2JpgStream_ReturnsStream;
var
  Stream: TStream;
begin
  Stream:= Bmp2JpgStream(FBitmap);
  TRY
    Assert.IsTrue(Stream is TMemoryStream, 'Should return TMemoryStream');
  FINALLY
    FreeAndNil(Stream);
  END;
end;


procedure TTestGraphConvert.TestBmp2JpgStream_StreamHasContent;
var
  Stream: TStream;
begin
  Stream:= Bmp2JpgStream(FBitmap);
  TRY
    Assert.IsTrue(Stream.Size > 0, 'Stream should have content');
  FINALLY
    FreeAndNil(Stream);
  END;
end;


procedure TTestGraphConvert.TestBmp2JpgStream_ValidJpegData;
var
  Stream: TStream;
  Jpg: TJpegImage;
begin
  Stream:= Bmp2JpgStream(FBitmap);
  TRY
    Stream.Position:= 0;
    Jpg:= TJpegImage.Create;
    TRY
      Assert.WillNotRaise(
        procedure
        begin
          Jpg.LoadFromStream(Stream);
        end,
        'Stream should contain valid JPEG data');
    FINALLY
      FreeAndNil(Jpg);
    END;
  FINALLY
    FreeAndNil(Stream);
  END;
end;


{ CompressBmp Tests }

procedure TTestGraphConvert.TestCompressBmp_BasicCall;
var
  Size: Integer;
begin
  Size:= CompressBmp(FBitmap);
  Assert.IsTrue(Size > 0, 'Should return positive size');
end;


procedure TTestGraphConvert.TestCompressBmp_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      CompressBmp(NIL);
    end,
    Exception);
end;


procedure TTestGraphConvert.TestCompressBmp_ReturnsPositiveSize;
var
  Size: Integer;
begin
  Size:= CompressBmp(FBitmap);
  Assert.IsTrue(Size > 0, 'Compressed size should be positive');
end;


procedure TTestGraphConvert.TestCompressBmp_HigherQualityLargerSize;
var
  SizeLow, SizeHigh: Integer;
begin
  SizeLow:= CompressBmp(FBitmap, 10);
  SizeHigh:= CompressBmp(FBitmap, 100);

  Assert.IsTrue(SizeHigh > SizeLow,
    'Higher quality should produce larger file. Low=' + IntToStr(SizeLow) + ' High=' + IntToStr(SizeHigh));
end;


{ Recompress (single param) Tests }

procedure TTestGraphConvert.TestRecompress_SingleParam_BasicCall;
var
  Jpg: TJpegImage;
  Size: Integer;
begin
  Jpg:= CreateTestJpeg;
  TRY
    Size:= Recompress(Jpg);
    Assert.IsTrue(Size > 0, 'Should return positive size');
  FINALLY
    FreeAndNil(Jpg);
  END;
end;


procedure TTestGraphConvert.TestRecompress_SingleParam_NilJpeg;
begin
  Assert.WillRaise(
    procedure
    begin
      Recompress(TJpegImage(NIL));
    end,
    Exception);
end;


procedure TTestGraphConvert.TestRecompress_SingleParam_ReturnsSize;
var
  Jpg: TJpegImage;
  Size: Integer;
begin
  Jpg:= CreateTestJpeg;
  TRY
    Size:= Recompress(Jpg, 50);
    Assert.IsTrue(Size > 0, 'Should return size > 0');
  FINALLY
    FreeAndNil(Jpg);
  END;
end;


{ Recompress (OUT param) Tests }

procedure TTestGraphConvert.TestRecompress_OutParam_BasicCall;
var
  InputJpg, OutputJpg: TJpegImage;
  Size: Integer;
begin
  InputJpg:= CreateTestJpeg;
  OutputJpg:= NIL;
  TRY
    Size:= Recompress(InputJpg, OutputJpg);
    Assert.IsTrue(Size > 0, 'Should return positive size');
  FINALLY
    FreeAndNil(InputJpg);
    FreeAndNil(OutputJpg);
  END;
end;


procedure TTestGraphConvert.TestRecompress_OutParam_NilInput;
var
  OutputJpg: TJpegImage;
begin
  Assert.WillRaise(
    procedure
    begin
      Recompress(NIL, OutputJpg);
    end,
    Exception);
end;


procedure TTestGraphConvert.TestRecompress_OutParam_CreatesOutput;
var
  InputJpg, OutputJpg: TJpegImage;
begin
  InputJpg:= CreateTestJpeg;
  OutputJpg:= NIL;
  TRY
    Recompress(InputJpg, OutputJpg);
    Assert.IsNotNull(OutputJpg, 'Output should be created');
  FINALLY
    FreeAndNil(InputJpg);
    FreeAndNil(OutputJpg);
  END;
end;


procedure TTestGraphConvert.TestRecompress_OutParam_OutputIsValid;
var
  InputJpg, OutputJpg: TJpegImage;
  Bmp: TBitmap;
begin
  InputJpg:= CreateTestJpeg;
  OutputJpg:= NIL;
  TRY
    Recompress(InputJpg, OutputJpg, 80);

    { Verify output is a valid JPEG by assigning to bitmap }
    Bmp:= TBitmap.Create;
    TRY
      Assert.WillNotRaise(
        procedure
        begin
          Bmp.Assign(OutputJpg);
        end,
        'Output should be a valid JPEG');
    FINALLY
      FreeAndNil(Bmp);
    END;
  FINALLY
    FreeAndNil(InputJpg);
    FreeAndNil(OutputJpg);
  END;
end;


{ Roundtrip Tests }

procedure TTestGraphConvert.TestRoundtrip_BmpToJpgToBmp;
var
  Jpg: TJpegImage;
  Bmp: TBitmap;
begin
  Jpg:= Bmp2Jpg(FBitmap, 100);  { High quality to minimize loss }
  TRY
    Bmp:= Jpeg2Bmp(Jpg);
    TRY
      Assert.IsNotNull(Bmp, 'Roundtrip should produce valid bitmap');
      Assert.IsTrue(Bmp.Width > 0, 'Bitmap should have width');
      Assert.IsTrue(Bmp.Height > 0, 'Bitmap should have height');
    FINALLY
      FreeAndNil(Bmp);
    END;
  FINALLY
    FreeAndNil(Jpg);
  END;
end;


procedure TTestGraphConvert.TestRoundtrip_PreservesDimensions;
var
  Jpg: TJpegImage;
  Bmp: TBitmap;
begin
  Jpg:= Bmp2Jpg(FBitmap, 100);
  TRY
    Bmp:= Jpeg2Bmp(Jpg);
    TRY
      Assert.AreEqual(FBitmap.Width, Bmp.Width, 'Width should be preserved');
      Assert.AreEqual(FBitmap.Height, Bmp.Height, 'Height should be preserved');
    FINALLY
      FreeAndNil(Bmp);
    END;
  FINALLY
    FreeAndNil(Jpg);
  END;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestGraphConvert);

end.
