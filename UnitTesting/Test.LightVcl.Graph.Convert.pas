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


CONST
  { JPEG is lossy, so a decoded channel may differ from the source. The quadrant centers that AssertQuadrants reads
    lie inside 16x16 blocks of one color (JPEG codes 8x8 blocks, 16x16 with chroma subsampling), so the error stays small. }
  JpegTolerance    = 40;
  JpegTolerance100 = 10;   { At quality 100 every quantization step is 1 }


{ Paints the four 50x40 quadrants of a 100x80 bitmap: red top-left, lime top-right, blue bottom-left, white bottom-right }
procedure PaintQuadrants(BMP: TBitmap);
begin
  BMP.Canvas.Brush.Color:= clRed;
  BMP.Canvas.FillRect(Rect(0, 0, 50, 40));
  BMP.Canvas.Brush.Color:= clLime;
  BMP.Canvas.FillRect(Rect(50, 0, 100, 40));
  BMP.Canvas.Brush.Color:= clBlue;
  BMP.Canvas.FillRect(Rect(0, 40, 50, 80));
  BMP.Canvas.Brush.Color:= clWhite;
  BMP.Canvas.FillRect(Rect(50, 40, 100, 80));
end;


{ Asserts that each channel of the pixel at (X, Y) is within Tolerance of Expected }
procedure AssertPixelNear(BMP: TBitmap; X, Y: Integer; Expected: TColor; Tolerance: Integer; CONST Msg: string);
VAR E, A: Integer;
begin
  E:= ColorToRGB(Expected);
  A:= ColorToRGB(BMP.Canvas.Pixels[X, Y]);
  Assert.IsTrue((Abs((E AND $FF) - (A AND $FF)) <= Tolerance)
            AND (Abs(((E SHR 8) AND $FF) - ((A SHR 8) AND $FF)) <= Tolerance)
            AND (Abs(((E SHR 16) AND $FF) - ((A SHR 16) AND $FF)) <= Tolerance),
    Msg + ' (expected ' + IntToHex(E, 6) + ', got ' + IntToHex(A, 6) + ')');
end;


{ Asserts that BMP is 100x80 and that its four quadrant centers hold the colors that PaintQuadrants gave them }
procedure AssertQuadrants(BMP: TBitmap; Tolerance: Integer; CONST What: string);
begin
  Assert.AreEqual(100, BMP.Width,  What + ': width');
  Assert.AreEqual(80,  BMP.Height, What + ': height');
  AssertPixelNear(BMP, 25, 20, clRed,   Tolerance, What + ': the top-left quadrant is red');
  AssertPixelNear(BMP, 75, 20, clLime,  Tolerance, What + ': the top-right quadrant is lime');
  AssertPixelNear(BMP, 25, 60, clBlue,  Tolerance, What + ': the bottom-left quadrant is blue');
  AssertPixelNear(BMP, 75, 60, clWhite, Tolerance, What + ': the bottom-right quadrant is white');
end;


{ Decodes Jpg with plain VCL calls and asserts its quadrants }
procedure AssertJpegQuadrants(Jpg: TJpegImage; Tolerance: Integer; CONST What: string);
VAR Decoded: TBitmap;
begin
  Decoded:= TBitmap.Create;
  TRY
    Decoded.Assign(Jpg);
    AssertQuadrants(Decoded, Tolerance, What);
  FINALLY
    FreeAndNil(Decoded);
  END;
end;


{ Loads a JPEG file with plain VCL calls and asserts its quadrants }
procedure AssertJpegFileQuadrants(CONST FileName: string; Tolerance: Integer; CONST What: string);
VAR Jpg: TJpegImage;
begin
  Jpg:= TJpegImage.Create;
  TRY
    Jpg.LoadFromFile(FileName);
    AssertJpegQuadrants(Jpg, Tolerance, What);
  FINALLY
    FreeAndNil(Jpg);
  END;
end;


{ Size in bytes of the JPEG that the VCL encoder (Vcl.Imaging.jpeg) makes of BMP at Quality.
  It is the reference for the routines that return or write a JPEG: they must produce exactly these bytes. }
function VclJpegSize(BMP: TBitmap; Quality: Integer): Int64;
VAR
  Jpg: TJpegImage;
  Stream: TMemoryStream;
begin
  Jpg:= TJpegImage.Create;
  Stream:= TMemoryStream.Create;
  TRY
    Jpg.Assign(BMP);
    Jpg.CompressionQuality:= Quality;
    Jpg.SaveToStream(Stream);
    Result:= Stream.Size;
  FINALLY
    FreeAndNil(Stream);
    FreeAndNil(Jpg);
  END;
end;


{ A JPEG of BMP that already holds its data compressed at Quality, and still holds BMP as its picture }
function CreateCompressedJpeg(BMP: TBitmap; Quality: Integer): TJpegImage;
begin
  Result:= TJpegImage.Create;
  TRY
    Result.Assign(BMP);
    Result.CompressionQuality:= Quality;
    Result.Compress;
  EXCEPT
    FreeAndNil(Result);
    RAISE;
  END;
end;


{ A JPEG loaded from the bytes of BMP encoded at Quality. It holds only the compressed data, so reading its picture decodes it. }
function CreateDecodedJpeg(BMP: TBitmap; Quality: Integer): TJpegImage;
VAR
  Encoder: TJpegImage;
  Stream: TMemoryStream;
begin
  Stream:= TMemoryStream.Create;
  TRY
    Encoder:= CreateCompressedJpeg(BMP, Quality);
    TRY
      Encoder.SaveToStream(Stream);
    FINALLY
      FreeAndNil(Encoder);
    END;
    Stream.Position:= 0;
    Result:= TJpegImage.Create;
    TRY
      Result.LoadFromStream(Stream);
    EXCEPT
      FreeAndNil(Result);
      RAISE;
    END;
  FINALLY
    FreeAndNil(Stream);
  END;
end;


{ All bytes of Stream }
function StreamBytes(Stream: TStream): TBytes;
begin
  SetLength(Result, Stream.Size);
  Stream.Position:= 0;
  if Length(Result) > 0
  then Stream.ReadBuffer(Result[0], Length(Result));
end;


{ Asserts that Bytes start with the JPEG start-of-image marker FF D8 and end with the end-of-image marker FF D9 }
procedure AssertJpegMarkers(CONST Bytes: TBytes; CONST What: string);
VAR N: Integer;
begin
  N:= Length(Bytes);
  Assert.IsTrue(N > 4, What + ': too short for a JPEG: ' + IntToStr(N) + ' bytes');
  Assert.AreEqual(Integer($FF), Integer(Bytes[0]),   What + ': start-of-image marker, byte 1');
  Assert.AreEqual(Integer($D8), Integer(Bytes[1]),   What + ': start-of-image marker, byte 2');
  Assert.AreEqual(Integer($FF), Integer(Bytes[N-2]), What + ': end-of-image marker, byte 1');
  Assert.AreEqual(Integer($D9), Integer(Bytes[N-1]), What + ': end-of-image marker, byte 2');
end;


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
  Stream: TMemoryStream;
begin
  PaintQuadrants(FBitmap);
  Jpg:= Bmp2Jpg(FBitmap);
  TRY
    Assert.AreEqual(100, Jpg.Width,  'The JPEG must keep the width of the source');
    Assert.AreEqual(80,  Jpg.Height, 'The JPEG must keep the height of the source');
    AssertJpegQuadrants(Jpg, JpegTolerance, 'Picture of the JPEG');

    Stream:= TMemoryStream.Create;
    TRY
      Jpg.SaveToStream(Stream);
      Assert.AreEqual(VclJpegSize(FBitmap, DelphiJpgQuality), Stream.Size, 'Saved, the JPEG holds the source encoded at the default quality');
    FINALLY
      FreeAndNil(Stream);
    END;
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
    Exception,
    'Bmp2Jpg(TBitmap(NIL)) must raise Exception');
end;


procedure TTestGraphConvert.TestBmp2Jpg_ReturnsJpeg;
var
  Jpg: TJpegImage;
  Decoded: TBitmap;
  C: Integer;
begin
  Jpg:= Bmp2Jpg(FBitmap);
  TRY
    Assert.AreEqual(100, Jpg.Width,  'The JPEG must keep the width of the source');
    Assert.AreEqual(80,  Jpg.Height, 'The JPEG must keep the height of the source');

    { Decode the JPEG again: the source is solid red, so the center pixel must still be red (JPEG is lossy, hence the tolerance) }
    Decoded:= TBitmap.Create;
    TRY
      Decoded.Assign(Jpg);
      C:= ColorToRGB(Decoded.Canvas.Pixels[50, 40]);
      Assert.IsTrue((C and $FF) > 200,           'Red channel must stay high, got pixel ' + IntToHex(C, 8));
      Assert.IsTrue(((C shr 8) and $FF) < 50,    'Green channel must stay low, got pixel ' + IntToHex(C, 8));
      Assert.IsTrue(((C shr 16) and $FF) < 50,   'Blue channel must stay low, got pixel ' + IntToHex(C, 8));
    FINALLY
      FreeAndNil(Decoded);
    END;
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
    { AreEqual is generic. DelphiJpgQuality is an untyped integer constant and CompressionQuality is
      TJPEGQualityRange, a 1..100 subrange, so no single type can be inferred (E2532). Both sides
      are cast to Integer. }
    Assert.AreEqual(Integer(DelphiJpgQuality), Integer(Jpg.CompressionQuality),
      'Default compression should be DelphiJpgQuality');
  FINALLY
    FreeAndNil(Jpg);
  END;
end;


procedure TTestGraphConvert.TestBmp2Jpg_CustomCompression;
var
  Jpg: TJpegImage;
  Stream: TMemoryStream;
begin
  PaintQuadrants(FBitmap);
  { 35 differs from DelphiJpgQuality (70) and from the TJPEGImage default (90: JPEGDefaults in
    c:\Delphi\Delphi 13\source\vcl\Vcl.Imaging.jpeg.pas:136), so an ignored CompressFactor shows }
  Jpg:= Bmp2Jpg(FBitmap, 35);
  TRY
    Assert.AreEqual(35, Integer(Jpg.CompressionQuality), 'Compression should be 35');
    Stream:= TMemoryStream.Create;
    TRY
      Jpg.SaveToStream(Stream);
      Assert.AreEqual(VclJpegSize(FBitmap, 35), Stream.Size, 'Saved, the JPEG holds the source encoded at quality 35');
      Assert.AreNotEqual(VclJpegSize(FBitmap, 90), Stream.Size, 'Quality 35 and quality 90 must give different sizes');
    FINALLY
      FreeAndNil(Stream);
    END;
  FINALLY
    FreeAndNil(Jpg);
  END;
end;


{ Bmp2Jpg (to file) Tests }

procedure TTestGraphConvert.TestBmp2JpgFile_BasicCall;
var
  OutputFile: string;
begin
  PaintQuadrants(FBitmap);
  TDirectory.CreateDirectory(FTempDir);
  OutputFile:= TPath.Combine(FTempDir, 'test.jpg');

  Bmp2Jpg(FBitmap, OutputFile);

  Assert.AreEqual(VclJpegSize(FBitmap, DelphiJpgQuality), TFile.GetSize(OutputFile), 'The file holds the source encoded at the default quality');
  AssertJpegFileQuadrants(OutputFile, JpegTolerance, 'The decoded file');
end;


procedure TTestGraphConvert.TestBmp2JpgFile_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      Bmp2Jpg(TBitmap(NIL), 'test.jpg');
    end,
    Exception,
    'Bmp2Jpg(TBitmap(NIL), test.jpg) must raise Exception');
end;


procedure TTestGraphConvert.TestBmp2JpgFile_EmptyOutputFile;
begin
  Assert.WillRaise(
    procedure
    begin
      Bmp2Jpg(FBitmap, '');
    end,
    Exception,
    'Bmp2Jpg(FBitmap, ) must raise Exception');
end;


procedure TTestGraphConvert.TestBmp2JpgFile_CreatesFile;
var
  OutputFile: string;
  Bytes: TBytes;
begin
  PaintQuadrants(FBitmap);
  TDirectory.CreateDirectory(FTempDir);
  OutputFile:= TPath.Combine(FTempDir, 'test.jpg');

  Bmp2Jpg(FBitmap, OutputFile, 30);

  Assert.IsTrue(TFile.Exists(OutputFile), 'File should be created');
  Bytes:= TFile.ReadAllBytes(OutputFile);
  AssertJpegMarkers(Bytes, 'The file');
  Assert.AreEqual(VclJpegSize(FBitmap, 30), Int64(Length(Bytes)), 'The file holds the source encoded at quality 30');
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
  PaintQuadrants(FBitmap);
  Jpg:= CreateDecodedJpeg(FBitmap, 90);   { Holds only compressed data, so Jpeg2Bmp must really decode it }
  TRY
    Bmp:= Jpeg2Bmp(Jpg);
    TRY
      Assert.IsNotNull(Bmp, 'Jpeg2Bmp should return a bitmap');
      Assert.AreEqual(pf24bit, Bmp.PixelFormat, 'Jpeg2Bmp returns a 24-bit bitmap');
      AssertQuadrants(Bmp, JpegTolerance, 'Jpeg2Bmp');
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
    Exception,
    'Jpeg2Bmp(NIL) must raise Exception');
end;


procedure TTestGraphConvert.TestJpeg2Bmp_ReturnsBitmap;
var
  Jpg: TJpegImage;
  Bmp: TBitmap;
  C: Integer;
begin
  Jpg:= CreateTestJpeg;
  TRY
    Bmp:= Jpeg2Bmp(Jpg);
    TRY
      Assert.AreEqual(100, Bmp.Width,  'The bitmap must keep the width of the JPEG');
      Assert.AreEqual(80,  Bmp.Height, 'The bitmap must keep the height of the JPEG');

      { The JPEG was made from a solid red bitmap: the center pixel must be red (JPEG is lossy, hence the tolerance) }
      C:= ColorToRGB(Bmp.Canvas.Pixels[50, 40]);
      Assert.IsTrue((C and $FF) > 200,           'Red channel must stay high, got pixel ' + IntToHex(C, 8));
      Assert.IsTrue(((C shr 8) and $FF) < 50,    'Green channel must stay low, got pixel ' + IntToHex(C, 8));
      Assert.IsTrue(((C shr 16) and $FF) < 50,   'Blue channel must stay low, got pixel ' + IntToHex(C, 8));
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
  PaintQuadrants(FBitmap);
  TDirectory.CreateDirectory(FTempDir);
  OutputFile:= TPath.Combine(FTempDir, 'graph.jpg');

  Graph2Jpg(FBitmap, OutputFile);

  Assert.AreEqual(VclJpegSize(FBitmap, DelphiJpgQuality), TFile.GetSize(OutputFile), 'The file holds the source encoded at the default quality');
  AssertJpegFileQuadrants(OutputFile, JpegTolerance, 'The decoded file');
end;


procedure TTestGraphConvert.TestGraph2Jpg_NilGraphic;
begin
  Assert.WillRaise(
    procedure
    begin
      Graph2Jpg(NIL, 'test.jpg');
    end,
    Exception,
    'Graph2Jpg(NIL, test.jpg) must raise Exception');
end;


procedure TTestGraphConvert.TestGraph2Jpg_EmptyOutputFile;
begin
  Assert.WillRaise(
    procedure
    begin
      Graph2Jpg(FBitmap, '');
    end,
    Exception,
    'Graph2Jpg(FBitmap, ) must raise Exception');
end;


procedure TTestGraphConvert.TestGraph2Jpg_CreatesFile;
var
  OutputFile: string;
  Bytes: TBytes;
begin
  PaintQuadrants(FBitmap);
  TDirectory.CreateDirectory(FTempDir);
  OutputFile:= TPath.Combine(FTempDir, 'graph.jpg');

  Graph2Jpg(FBitmap, OutputFile, 30);

  Assert.IsTrue(TFile.Exists(OutputFile), 'File should be created');
  Bytes:= TFile.ReadAllBytes(OutputFile);
  AssertJpegMarkers(Bytes, 'The file');
  Assert.AreEqual(VclJpegSize(FBitmap, 30), Int64(Length(Bytes)), 'The file holds the source encoded at quality 30');
  Assert.AreNotEqual(VclJpegSize(FBitmap, DelphiJpgQuality), Int64(Length(Bytes)), 'Quality 30 and the default quality must give different sizes');
end;


{ Bmp2JpgStream Tests }

procedure TTestGraphConvert.TestBmp2JpgStream_BasicCall;
var
  Stream: TStream;
  Jpg: TJpegImage;
begin
  PaintQuadrants(FBitmap);
  Stream:= Bmp2JpgStream(FBitmap);
  TRY
    Assert.IsNotNull(Stream, 'Should return a stream');
    Assert.AreEqual(VclJpegSize(FBitmap, DelphiJpgQuality), Stream.Size, 'The stream holds the source encoded at the default quality');
    Assert.AreEqual(Stream.Size, Stream.Position, 'The routine leaves the position at the end of the stream');

    Stream.Position:= 0;
    Jpg:= TJpegImage.Create;
    TRY
      Jpg.LoadFromStream(Stream);
      AssertJpegQuadrants(Jpg, JpegTolerance, 'The decoded stream');
    FINALLY
      FreeAndNil(Jpg);
    END;
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
    Exception,
    'Bmp2JpgStream(NIL) must raise Exception');
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
  PaintQuadrants(FBitmap);
  Stream:= Bmp2JpgStream(FBitmap, 30);
  TRY
    AssertJpegMarkers(StreamBytes(Stream), 'The stream');
    Assert.AreEqual(VclJpegSize(FBitmap, 30), Stream.Size, 'The stream holds the source encoded at quality 30');
    Assert.AreNotEqual(VclJpegSize(FBitmap, DelphiJpgQuality), Stream.Size, 'Quality 30 and the default quality must give different sizes');
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
      Assert.WillNotRaiseAny(
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
  PaintQuadrants(FBitmap);
  Size:= CompressBmp(FBitmap);

  Assert.AreEqual(VclJpegSize(FBitmap, DelphiJpgQuality), Int64(Size), 'CompressBmp returns the size of the source encoded at the default quality');
  { The uncompressed BMP stream of this 100x80 pf24bit picture is 14 + 40 + 80 * 300 = 24054 bytes }
  Assert.IsTrue(Size < 24054, 'The JPEG must be smaller than the uncompressed BMP, got ' + IntToStr(Size));
  AssertQuadrants(FBitmap, 0, 'CompressBmp must not change its input');
end;


procedure TTestGraphConvert.TestCompressBmp_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      CompressBmp(NIL);
    end,
    Exception,
    'CompressBmp(NIL) must raise Exception');
end;


procedure TTestGraphConvert.TestCompressBmp_ReturnsPositiveSize;
var
  Size: Integer;
begin
  PaintQuadrants(FBitmap);
  Size:= CompressBmp(FBitmap, 25);

  Assert.AreEqual(VclJpegSize(FBitmap, 25), Int64(Size), 'CompressBmp returns the size of the source encoded at quality 25');
  Assert.AreNotEqual(VclJpegSize(FBitmap, 100), Int64(Size), 'Quality 25 and quality 100 must give different sizes');
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
  Stream: TMemoryStream;
begin
  PaintQuadrants(FBitmap);
  Assert.IsTrue(VclJpegSize(FBitmap, 100) > VclJpegSize(FBitmap, DelphiJpgQuality), 'Precondition: quality 100 gives a larger JPEG than the default quality');

  { The JPEG already holds quality-100 data. Without its Compress call, Recompress would return that larger size. }
  Jpg:= CreateCompressedJpeg(FBitmap, 100);
  TRY
    Size:= Recompress(Jpg);

    Assert.AreEqual(VclJpegSize(FBitmap, DelphiJpgQuality), Int64(Size), 'Recompress must re-encode at the default quality');
    Assert.AreEqual(Integer(DelphiJpgQuality), Integer(Jpg.CompressionQuality), 'Recompress sets the quality on the JPEG it was given');
    Stream:= TMemoryStream.Create;
    TRY
      Jpg.SaveToStream(Stream);
      Assert.AreEqual(Int64(Size), Stream.Size, 'The JPEG is recompressed in place: it now holds exactly the returned number of bytes');
    FINALLY
      FreeAndNil(Stream);
    END;
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
    Exception,
    'Recompress(TJpegImage(NIL)) must raise Exception');
end;


procedure TTestGraphConvert.TestRecompress_SingleParam_ReturnsSize;
var
  Jpg: TJpegImage;
  Size: Integer;
begin
  PaintQuadrants(FBitmap);
  Assert.IsTrue(VclJpegSize(FBitmap, 100) > VclJpegSize(FBitmap, 50), 'Precondition: quality 100 gives a larger JPEG than quality 50');

  Jpg:= CreateCompressedJpeg(FBitmap, 100);
  TRY
    Size:= Recompress(Jpg, 50);

    Assert.AreEqual(VclJpegSize(FBitmap, 50), Int64(Size), 'Recompress must re-encode at quality 50');
    Assert.AreEqual(50, Integer(Jpg.CompressionQuality), 'Recompress sets the quality on the JPEG it was given');
  FINALLY
    FreeAndNil(Jpg);
  END;
end;


{ Recompress (OUT param) Tests }

procedure TTestGraphConvert.TestRecompress_OutParam_BasicCall;
var
  InputJpg, OutputJpg: TJpegImage;
  Size: Integer;
  Stream: TMemoryStream;
begin
  PaintQuadrants(FBitmap);
  InputJpg:= CreateCompressedJpeg(FBitmap, 100);
  OutputJpg:= NIL;
  TRY
    Size:= Recompress(InputJpg, OutputJpg);

    Assert.AreEqual(VclJpegSize(FBitmap, DelphiJpgQuality), Int64(Size), 'The returned size is that of the source re-encoded at the default quality');
    Stream:= TMemoryStream.Create;
    TRY
      OutputJpg.SaveToStream(Stream);
      Assert.AreEqual(Int64(Size), Stream.Size, 'The OUT JPEG holds exactly the re-encoded bytes');
    FINALLY
      FreeAndNil(Stream);
    END;
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
    Exception,
    'Recompress(NIL, OutputJpg) must raise Exception');
end;


procedure TTestGraphConvert.TestRecompress_OutParam_CreatesOutput;
var
  InputJpg, OutputJpg: TJpegImage;
begin
  PaintQuadrants(FBitmap);
  InputJpg:= CreateTestJpeg;
  OutputJpg:= NIL;
  TRY
    Recompress(InputJpg, OutputJpg);
    Assert.IsNotNull(OutputJpg, 'Output should be created');
    Assert.IsTrue(OutputJpg <> InputJpg, 'The OUT JPEG is a new object');
    Assert.AreEqual(100, OutputJpg.Width,  'The OUT JPEG keeps the width of the source');
    Assert.AreEqual(80,  OutputJpg.Height, 'The OUT JPEG keeps the height of the source');
    AssertJpegQuadrants(OutputJpg, JpegTolerance, 'The decoded OUT JPEG');
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
  PaintQuadrants(FBitmap);
  InputJpg:= CreateTestJpeg;
  OutputJpg:= NIL;
  TRY
    Recompress(InputJpg, OutputJpg, 80);

    { Verify output is a valid JPEG by assigning to bitmap }
    Bmp:= TBitmap.Create;
    TRY
      Assert.WillNotRaiseAny(
        procedure
        begin
          Bmp.Assign(OutputJpg);
        end,
        'Output should be a valid JPEG');
      AssertQuadrants(Bmp, JpegTolerance, 'The decoded OUT JPEG');
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
  Jpg, Reloaded: TJpegImage;
  Bmp: TBitmap;
  Stream: TMemoryStream;
begin
  PaintQuadrants(FBitmap);
  Jpg:= Bmp2Jpg(FBitmap, 100);  { High quality to minimize loss }
  Reloaded:= TJpegImage.Create;
  Stream:= TMemoryStream.Create;
  TRY
    { Through the encoded bytes, so that Jpeg2Bmp decodes them instead of copying the picture Bmp2Jpg kept }
    Jpg.SaveToStream(Stream);
    Stream.Position:= 0;
    Reloaded.LoadFromStream(Stream);

    Bmp:= Jpeg2Bmp(Reloaded);
    TRY
      Assert.IsNotNull(Bmp, 'Roundtrip should produce valid bitmap');
      Assert.AreEqual(pf24bit, Bmp.PixelFormat, 'Jpeg2Bmp returns a 24-bit bitmap');
      AssertQuadrants(Bmp, JpegTolerance100, 'Roundtrip at quality 100');
    FINALLY
      FreeAndNil(Bmp);
    END;
  FINALLY
    FreeAndNil(Stream);
    FreeAndNil(Reloaded);
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
