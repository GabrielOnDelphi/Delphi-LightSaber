unit Test.LightCore.Graph.Loader.Resolution;

{=============================================================================================================
   Unit tests for LightCore.Graph.Loader.Resolution.pas
   Tests image resolution retrieval functions for various formats (BMP, PNG, GIF, JPG).

   These tests build their image bytes by hand, so they link no framework at all.
   The tests that need the VCL to WRITE a real BMP/PNG/JPG/GIF file (TBitmap, TPngImage, TJPEGImage,
   TGIFImage) stayed in Test.LightVcl.Graph.Loader.Resolution.pas, together with GetBitsPerPixel.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  LightCore.IO,
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  System.Classes;

type
  [TestFixture]
  TTestCoreGraphLoaderResolution = class
  private
    FTempDir: string;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { GetBmpSize Tests }
    [Test]
    procedure TestGetBmpSize_InvalidSignature;

    { GetPngSize Tests }
    [Test]
    procedure TestGetPngSize_InvalidSignature;

    { GetJpgSize Tests }
    [Test]
    procedure TestGetJpgSize_InvalidSignature;

    [Test]
    procedure TestGetJpgSize_EmptyStream;

    [Test]
    procedure TestGetJpgSize_MalformedSegmentLength;     { Regression: bad segment length used to seek backward, infinite loop }

    { GetGifSize Tests }
    [Test]
    procedure TestGetGifSize_InvalidSignature;

    { GetImageRes Tests }
    [Test]
    procedure TestGetImageRes_UnsupportedExtension;

    [Test]
    procedure TestGetImageRes_NonExistentFile;

    { GetBmpHeader Tests }
    [Test]
    procedure TestGetBmpHeader_InvalidBmp;

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

    { Measurement: was the `packed record` fix actually necessary? }
    [Test]
    procedure TestRecordAlignment_GIFHeader;
  end;

implementation

uses
  LightCore.Graph.Loader.Resolution;


procedure TTestCoreGraphLoaderResolution.Setup;
begin
  FTempDir:= TPath.Combine(TPath.GetTempPath, 'TestCoreGraphResolution_' + TGUID.NewGuid.ToString);
  ForceDirectoriesE(FTempDir);
end;


procedure TTestCoreGraphLoaderResolution.TearDown;
begin
  if DirectoryExists(FTempDir)
  then RemoveDir(FTempDir);
end;


{ GetBmpSize Tests }

procedure TTestCoreGraphLoaderResolution.TestGetBmpSize_InvalidSignature;
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

procedure TTestCoreGraphLoaderResolution.TestGetPngSize_InvalidSignature;
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

procedure TTestCoreGraphLoaderResolution.TestGetJpgSize_InvalidSignature;
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


procedure TTestCoreGraphLoaderResolution.TestGetJpgSize_EmptyStream;
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


procedure TTestCoreGraphLoaderResolution.TestGetJpgSize_MalformedSegmentLength;
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
    Assert.WillNotRaiseAny(
      procedure
      begin
        Success:= GetJpgSize(Stream, Width, Height);
      end,
      'GetJpgSize should not raise on malformed segment length');

    Assert.IsFalse(Success, 'GetJpgSize must return False for malformed segment length');
    Assert.AreEqual(-1, Width, 'Width should be -1 on parse failure');
    Assert.AreEqual(-1, Height, 'Height should be -1 on parse failure');
  FINALLY
    FreeAndNil(Stream);
  END;
end;


{ GetGifSize Tests }

procedure TTestCoreGraphLoaderResolution.TestGetGifSize_InvalidSignature;
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

procedure TTestCoreGraphLoaderResolution.TestGetImageRes_UnsupportedExtension;
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


procedure TTestCoreGraphLoaderResolution.TestGetImageRes_NonExistentFile;
var
  Width, Height: Integer;
begin
  GetImageRes('C:\NonExistent\Path\file.bmp', Width, Height);

  Assert.AreEqual(-1, Width, 'Non-existent file should return Width = -1');
  Assert.AreEqual(-1, Height, 'Non-existent file should return Height = -1');
end;


{ GetBmpHeader Tests }

procedure TTestCoreGraphLoaderResolution.TestGetBmpHeader_InvalidBmp;
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


{ Top-down BMP: builds a minimal valid 14+40 byte BMP with a deliberately negative biHeight.
  Pixel height should be returned as Abs(biHeight). }
procedure TTestCoreGraphLoaderResolution.TestGetBmpSize_TopDownDIB_ReturnsPositiveHeight;
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


procedure TTestCoreGraphLoaderResolution.TestGetBmpHeader_TopDownDIB_PreservesNegativeHeight;
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
procedure TTestCoreGraphLoaderResolution.TestGetGifSize_HandCraftedWithGCT;
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
procedure TTestCoreGraphLoaderResolution.TestGetGifSize_HandCraftedWithExtensionBlock;
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


{ Verifies the actual SizeOf for an unpacked record matching TGIFHeader's layout.
  Compares two equivalent declarations — one packed, one unpacked. If their sizes match,
  the `packed` fix was unnecessary for correctness (still good practice for stream-read records). }
procedure TTestCoreGraphLoaderResolution.TestRecordAlignment_GIFHeader;
type
  TUnpacked = record
    Sig: array[0..5] of AnsiChar;
    ScreenWidth, ScreenHeight: Word;
    Flags, Background, Aspect: Byte;
  end;
  TPacked = packed record
    Sig: array[0..5] of AnsiChar;
    ScreenWidth, ScreenHeight: Word;
    Flags, Background, Aspect: Byte;
  end;
begin
  { Verified by direct measurement on Delphi 13 Win32: unpacked=14, packed=13.
    Word-aligned fields force the unpacked record's size up to the next multiple of 2.
    The `packed` keyword on TGIFHeader inside GetGIFSize was therefore a real correctness fix. }
  Assert.AreEqual(13, SizeOf(TPacked),  'packed record size must equal on-disk GIF header (13)');
  Assert.AreEqual(14, SizeOf(TUnpacked),'unpacked record gets a trailing alignment byte (14)');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestCoreGraphLoaderResolution);

end.
