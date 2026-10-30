unit Test.GifProperties;

{=============================================================================================================
   Unit tests for External\GifProperties.pas
   IsAnimatedGif counts the frames of a GIF by walking its block structure, without decoding any pixel.

   Setup writes two GIF files into a temporary folder that TearDown deletes:
     - static.gif   (1 red frame)
     - animated.gif (3 frames: red, blue, lime)
   A "cut" test copies static.gif without its last bytes, the way an interrupted download or copy
   leaves a file, or writes the few bytes of a broken GIF itself.

   A GIF that ends too early must make IsAnimatedGif return FALSE at once.
   [MaxTime(5000)] stops a call that never returns: after 5 seconds DUnitX raises ETimedOut inside
   the test thread (DUnitX.Timeout.pas). IsAnimatedGif swallows every exception and returns FALSE,
   so CheckFalseAtOnce also measures how long the call took.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  System.Diagnostics,
  Winapi.Windows,   { Before Vcl.Graphics: Winapi.Windows also declares a TBitmap }
  Vcl.Graphics;

type
  [TestFixture]
  TTestGifProperties = class
  private
    FTestFolder: string;
    FStaticGifPath: string;
    FAnimatedGifPath: string;
    procedure WriteGif(const FileName: string; const FrameColors: array of TColor);
    function CutCopy(const SourceFile: string; BytesToRemove: Integer): string;
    procedure CheckFalseAtOnce(const FileName: string);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Complete files }
    [Test]
    procedure TestIsAnimatedGif_StaticGif_ReturnsFalse;

    [Test]
    procedure TestIsAnimatedGif_AnimatedGif_ReturnsTrue;

    { Files that end too early }
    [Test]
    [MaxTime(5000)]
    procedure TestIsAnimatedGif_CutInsideImageData_ReturnsFalseAtOnce;

    [Test]
    [MaxTime(5000)]
    procedure TestIsAnimatedGif_NoTrailer_ReturnsFalseAtOnce;

    [Test]
    [MaxTime(5000)]
    procedure TestIsAnimatedGif_CutInsideComment_ReturnsFalseAtOnce;

    [Test]
    [MaxTime(5000)]
    procedure TestIsAnimatedGif_CutInsideColorTable_ReturnsFalseAtOnce;
  end;

implementation

uses
  System.Types,
  Vcl.Imaging.GIFImg,
  GifProperties,
  LightCore.IO;

CONST
  GifWidth  = 20;
  GifHeight = 10;
  GifDelay  = 20;           { Hundredths of a second, as stored in the GIF graphic control extension }
  MaxCallTime = 2000;       { Milliseconds. The call reads a file of a few hundred bytes. }
  GifTrailer = $3B;         { The last byte of a complete GIF }
  BlockTerminator = $00;    { A sub-block of length 0 ends the data of an image or extension }


procedure TTestGifProperties.Setup;
begin
  FTestFolder:= IncludeTrailingPathDelimiter(TPath.Combine(TPath.GetTempPath, 'TestGifProperties_' + TGUID.NewGuid.ToString));
  TDirectory.CreateDirectory(FTestFolder);
  FStaticGifPath:= FTestFolder + 'static.gif';
  FAnimatedGifPath:= FTestFolder + 'animated.gif';
  WriteGif(FStaticGifPath,   [clRed]);
  WriteGif(FAnimatedGifPath, [clRed, clBlue, clLime]);
end;


procedure TTestGifProperties.TearDown;
begin
  if TDirectory.Exists(FTestFolder)
  then TDirectory.Delete(FTestFolder, TRUE);
end;


{ Writes a GIF with one solid-colored GifWidth x GifHeight frame per color }
procedure TTestGifProperties.WriteGif(const FileName: string; const FrameColors: array of TColor);
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


{ Copies SourceFile without its last BytesToRemove bytes. Returns the path of the copy. }
function TTestGifProperties.CutCopy(const SourceFile: string; BytesToRemove: Integer): string;
begin
  Result:= FTestFolder + 'cut' + IntToStr(BytesToRemove) + '_' + ExtractFileName(SourceFile);
  CopyFileTop(SourceFile, Result, LightCore.IO.GetFileSize(SourceFile) - BytesToRemove);
end;


{ IsAnimatedGif must answer FALSE for a GIF with one frame that ends too early, and must answer at once }
procedure TTestGifProperties.CheckFalseAtOnce(const FileName: string);
VAR
  Watch: TStopwatch;
  Animated: Boolean;
begin
  Watch:= TStopwatch.StartNew;
  Animated:= IsAnimatedGif(FileName);
  Watch.Stop;
  Assert.IsTrue(Watch.ElapsedMilliseconds < MaxCallTime, 'IsAnimatedGif took ' + IntToStr(Watch.ElapsedMilliseconds) + ' ms: it kept reading past the end of the file');
  Assert.IsFalse(Animated, 'A GIF with one frame is not animated');
end;


{ Complete files }

procedure TTestGifProperties.TestIsAnimatedGif_StaticGif_ReturnsFalse;
begin
  Assert.IsFalse(IsAnimatedGif(FStaticGifPath), 'A GIF with 1 frame is not animated');
end;


procedure TTestGifProperties.TestIsAnimatedGif_AnimatedGif_ReturnsTrue;
begin
  Assert.IsTrue(IsAnimatedGif(FAnimatedGifPath), 'A GIF with 3 frames is animated');
end;


{ Files that end too early }

{ static.gif ends with: the last data sub-block of its image, the block terminator, the trailer.
  Without its last 3 bytes, the last data sub-block announces one byte more than the file holds. }
procedure TTestGifProperties.TestIsAnimatedGif_CutInsideImageData_ReturnsFalseAtOnce;
VAR
  Bytes: TBytes;
begin
  Bytes:= TFile.ReadAllBytes(FStaticGifPath);
  Assert.AreEqual(Integer(GifTrailer),      Integer(Bytes[High(Bytes)]),     'Precondition: static.gif ends with the GIF trailer');
  Assert.AreEqual(Integer(BlockTerminator), Integer(Bytes[High(Bytes) - 1]), 'Precondition: the trailer follows the block terminator of the image data');

  CheckFalseAtOnce(CutCopy(FStaticGifPath, 3));
end;


{ The image is complete; only the trailer is missing }
procedure TTestGifProperties.TestIsAnimatedGif_NoTrailer_ReturnsFalseAtOnce;
VAR
  Bytes: TBytes;
begin
  Bytes:= TFile.ReadAllBytes(FStaticGifPath);
  Assert.AreEqual(Integer(GifTrailer), Integer(Bytes[High(Bytes)]), 'Precondition: static.gif ends with the GIF trailer');

  CheckFalseAtOnce(CutCopy(FStaticGifPath, 1));
end;


{ A GIF header, then a comment extension whose sub-block announces 10 bytes but the file holds only 3 }
procedure TTestGifProperties.TestIsAnimatedGif_CutInsideComment_ReturnsFalseAtOnce;
VAR
  FileName: string;
  Data: TBytes;
begin
  Data:= [$47, $49, $46, $38, $39, $61,         { "GIF89a" }
          $14, $00, $0A, $00, $00, $00, $00,    { Logical screen 20 x 10, no global color table }
          $21, $FE, $0A,                        { Comment extension, a sub-block of 10 bytes... }
          $61, $62, $63];                       { ...of which only 3 are in the file }
  FileName:= FTestFolder + 'cut_comment.gif';
  Assert.IsTrue(BytesToFile(FileName, Data), 'Precondition: the test file was written');

  CheckFalseAtOnce(FileName);
end;


{ A GIF header that announces a global color table of 2 colors (6 bytes), but the file holds only 2 of those bytes }
procedure TTestGifProperties.TestIsAnimatedGif_CutInsideColorTable_ReturnsFalseAtOnce;
VAR
  FileName: string;
  Data: TBytes;
begin
  Data:= [$47, $49, $46, $38, $39, $61,         { "GIF89a" }
          $14, $00, $0A, $00, $80, $00, $00,    { Logical screen 20 x 10, global color table of 2 colors }
          $FF, $00];                            { 2 of the 6 bytes of the color table }
  FileName:= FTestFolder + 'cut_colortable.gif';
  Assert.IsTrue(BytesToFile(FileName, Data), 'Precondition: the test file was written');

  CheckFalseAtOnce(FileName);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestGifProperties);

end.
