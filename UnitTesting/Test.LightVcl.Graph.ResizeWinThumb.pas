unit Test.LightVcl.Graph.ResizeWinThumb;

{=============================================================================================================
   Unit tests for LightVcl.Graph.ResizeWinThumb.pas
   Tests TFileThumb class for Windows Shell thumbnail generation.

   Note: These tests require actual files on disk to fully test thumbnail generation.
   Some tests use mock scenarios or verify basic functionality without real files.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Types,        { Rect }
  System.IOUtils,
  Vcl.Graphics;

type
  [TestFixture]
  TTestGraphResizeWinThumb = class
  private
    FTempFile: string;
    procedure CreateTempImageFile;
    procedure DeleteTempImageFile;
    procedure Blacken(Thumb: TObject);
    procedure CheckBlank(Thumb: TObject; Size: Integer);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor/Destructor Tests }
    [Test]
    procedure TestCreate_BitmapNotNil;

    [Test]
    procedure TestCreate_DefaultWidth;

    [Test]
    procedure TestCreate_DefaultFilePath;

    { Width Property Tests }
    [Test]
    procedure TestSetWidth_ValidValue;

    [Test]
    procedure TestSetWidth_BelowMinimum_ClampsToMin;

    [Test]
    procedure TestSetWidth_AboveMaximum_ClampsToMax;

    [Test]
    procedure TestSetWidth_UpdatesBitmapSize;

    { FilePath Property Tests }
    [Test]
    procedure TestSetFilePath_ValidPath;

    [Test]
    procedure TestSetFilePath_EmptyPath;

    { GenerateThumbnail Tests }
    [Test]
    procedure TestGenerateThumbnail_EmptyFilePath;

    [Test]
    procedure TestGenerateThumbnail_NonExistentFile;

    [Test]
    procedure TestGenerateThumbnail_BitmapSizeSet;

    [Test]
    procedure TestGenerateThumbnail_WithValidFile;

    { GenerateThumbnail2 Tests }
    [Test]
    procedure TestGenerateThumbnail2_EmptyFilePath;

    [Test]
    procedure TestGenerateThumbnail2_NonExistentFile;

    [Test]
    procedure TestGenerateThumbnail2_BitmapSizeSet;

    { ThumbBmp Property Tests }
    [Test]
    procedure TestThumbBmp_NotNil;

    [Test]
    procedure TestThumbBmp_HasCorrectPixelFormat;
  end;

implementation

uses
  LightVcl.Graph.ResizeWinThumb;


procedure TTestGraphResizeWinThumb.Setup;
begin
  FTempFile:= '';
end;


procedure TTestGraphResizeWinThumb.TearDown;
begin
  DeleteTempImageFile;
end;


procedure TTestGraphResizeWinThumb.CreateTempImageFile;
var
  Bmp: TBitmap;
begin
  FTempFile:= TPath.Combine(TPath.GetTempPath, 'TestThumb_' + TGUID.NewGuid.ToString + '.bmp');
  Bmp:= TBitmap.Create;
  TRY
    Bmp.Width:= 100;
    Bmp.Height:= 100;
    Bmp.Canvas.Brush.Color:= clRed;
    Bmp.Canvas.FillRect(Rect(0, 0, 100, 100));
    Bmp.SaveToFile(FTempFile);
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


procedure TTestGraphResizeWinThumb.DeleteTempImageFile;
begin
  if (FTempFile <> '') AND FileExists(FTempFile)
  then DeleteFile(FTempFile);
  FTempFile:= '';
end;


{ Shrinks and blackens the bitmap behind the setter's back, so only a Generate call can make it blank and full-sized again }
procedure TTestGraphResizeWinThumb.Blacken(Thumb: TObject);
var
  Bmp: TBitmap;
begin
  Bmp:= TFileThumb(Thumb).ThumbBmp;
  Bmp.SetSize(10, 10);
  Bmp.Canvas.Brush.Color:= clBlack;
  Bmp.Canvas.FillRect(Rect(0, 0, 10, 10));
end;


{ A blank thumbnail: Size x Size, filled with the window colour, no icon in the centre }
procedure TTestGraphResizeWinThumb.CheckBlank(Thumb: TObject; Size: Integer);
var
  Bmp: TBitmap;
begin
  Bmp:= TFileThumb(Thumb).ThumbBmp;
  Assert.AreEqual(Size, Bmp.Width,  'Blank thumbnail width');
  Assert.AreEqual(Size, Bmp.Height, 'Blank thumbnail height');
  Assert.AreEqual(Integer(ColorToRGB(clWindow)), Integer(Bmp.Canvas.Pixels[Size DIV 2, Size DIV 2]), 'The centre of a blank thumbnail must be the window colour');
end;


{ Constructor/Destructor Tests }

procedure TTestGraphResizeWinThumb.TestCreate_BitmapNotNil;
var
  Thumb: TFileThumb;
begin
  Thumb:= TFileThumb.Create;
  TRY
    Assert.IsNotNull(Thumb.ThumbBmp, 'ThumbBmp should not be nil after creation');
    { The constructor sizes the bitmap to the default width 100 and fills it with the window colour }
    CheckBlank(Thumb, 100);
  FINALLY
    FreeAndNil(Thumb);
  END;
end;


procedure TTestGraphResizeWinThumb.TestCreate_DefaultWidth;
var
  Thumb: TFileThumb;
begin
  Thumb:= TFileThumb.Create;
  TRY
    Assert.AreEqual(100, Thumb.Width, 'Default width should be 100');
  FINALLY
    FreeAndNil(Thumb);
  END;
end;


procedure TTestGraphResizeWinThumb.TestCreate_DefaultFilePath;
var
  Thumb: TFileThumb;
begin
  Thumb:= TFileThumb.Create;
  TRY
    Assert.AreEqual('', Thumb.FilePath, 'Default FilePath should be empty');
    { With the default path GenerateThumbnail has no file to show, so it must leave a blank thumbnail }
    Blacken(Thumb);
    Thumb.GenerateThumbnail;
    CheckBlank(Thumb, 100);
  FINALLY
    FreeAndNil(Thumb);
  END;
end;


{ Width Property Tests }

procedure TTestGraphResizeWinThumb.TestSetWidth_ValidValue;
var
  Thumb: TFileThumb;
begin
  Thumb:= TFileThumb.Create;
  TRY
    Thumb.Width:= 150;
    Assert.AreEqual(150, Thumb.Width, 'Width should be 150');
  FINALLY
    FreeAndNil(Thumb);
  END;
end;


procedure TTestGraphResizeWinThumb.TestSetWidth_BelowMinimum_ClampsToMin;
var
  Thumb: TFileThumb;
begin
  Thumb:= TFileThumb.Create;
  TRY
    Thumb.Width:= 10;  // Below MinSize (52)
    Assert.AreEqual(52, Thumb.Width, 'Width should be clamped to MinSize (52)');
  FINALLY
    FreeAndNil(Thumb);
  END;
end;


procedure TTestGraphResizeWinThumb.TestSetWidth_AboveMaximum_ClampsToMax;
var
  Thumb: TFileThumb;
begin
  { This test used to be [Ignore]d. MaxSize was 65535, so SetSize clamped to 65535 and then called
    FBmp.SetSize(65535, 65535) - a 12.9 GB bitmap that Windows refuses with "The handle is invalid.",
    which made the test ERROR rather than fail. Gabriel set MaxSize to 8192 on 2026-09-04
    (LightVcl.Graph.ResizeWinThumb.pas), a size Windows really allocates - 256 MB at 32 bits - so the
    clamp now protects something and the test can run. }
  Thumb:= TFileThumb.Create;
  TRY
    Thumb.Width:= 100000;  // Above MaxSize (8192)
    Assert.AreEqual(8192, Thumb.Width, 'Width should be clamped to MaxSize (8192)');
  FINALLY
    FreeAndNil(Thumb);
  END;
end;


procedure TTestGraphResizeWinThumb.TestSetWidth_UpdatesBitmapSize;
var
  Thumb: TFileThumb;
begin
  Thumb:= TFileThumb.Create;
  TRY
    Thumb.Width:= 200;
    Assert.AreEqual(200, Thumb.ThumbBmp.Width, 'Bitmap width should match Width');
    Assert.AreEqual(200, Thumb.ThumbBmp.Height, 'Bitmap height should match Width (square)');
  FINALLY
    FreeAndNil(Thumb);
  END;
end;


{ FilePath Property Tests }

procedure TTestGraphResizeWinThumb.TestSetFilePath_ValidPath;
var
  Thumb: TFileThumb;
begin
  Thumb:= TFileThumb.Create;
  TRY
    Thumb.FilePath:= 'C:\Test\image.jpg';
    Assert.AreEqual('C:\Test\image.jpg', Thumb.FilePath, 'FilePath should be set');
  FINALLY
    FreeAndNil(Thumb);
  END;
end;


procedure TTestGraphResizeWinThumb.TestSetFilePath_EmptyPath;
var
  Thumb: TFileThumb;
begin
  Thumb:= TFileThumb.Create;
  TRY
    Thumb.FilePath:= 'C:\Test\image.jpg';
    Thumb.FilePath:= '';
    Assert.AreEqual('', Thumb.FilePath, 'FilePath should be empty');
  FINALLY
    FreeAndNil(Thumb);
  END;
end;


{ GenerateThumbnail Tests }

procedure TTestGraphResizeWinThumb.TestGenerateThumbnail_EmptyFilePath;
var
  Thumb: TFileThumb;
begin
  CreateTempImageFile;

  Thumb:= TFileThumb.Create;
  TRY
    { First a real (red) image, then clear the path: a setter that ignored '' would show the red image }
    Thumb.FilePath:= FTempFile;
    Thumb.FilePath:= '';
    Blacken(Thumb);

    Thumb.GenerateThumbnail;

    Assert.AreEqual('', Thumb.FilePath, 'FilePath must be empty');
    CheckBlank(Thumb, 100);
  FINALLY
    FreeAndNil(Thumb);
  END;
end;


procedure TTestGraphResizeWinThumb.TestGenerateThumbnail_NonExistentFile;
var
  Thumb: TFileThumb;
begin
  Thumb:= TFileThumb.Create;
  TRY
    Thumb.FilePath:= 'C:\NonExistent\File\That\Does\Not\Exist.jpg';
    Blacken(Thumb);

    Thumb.GenerateThumbnail;

    CheckBlank(Thumb, 100);
  FINALLY
    FreeAndNil(Thumb);
  END;
end;


procedure TTestGraphResizeWinThumb.TestGenerateThumbnail_BitmapSizeSet;
var
  Thumb: TFileThumb;
begin
  Thumb:= TFileThumb.Create;
  TRY
    Thumb.Width:= 150;
    Thumb.FilePath:= '';  // Empty path, but bitmap should still be sized
    { Shrink and blacken the bitmap behind the setter's back, so only GenerateThumbnail can restore it }
    Thumb.ThumbBmp.SetSize(10, 10);
    Thumb.ThumbBmp.Canvas.Brush.Color:= clBlack;
    Thumb.ThumbBmp.Canvas.FillRect(Rect(0, 0, 10, 10));
    Thumb.GenerateThumbnail;

    Assert.AreEqual(150, Thumb.ThumbBmp.Width, 'Bitmap width should be set');
    Assert.AreEqual(150, Thumb.ThumbBmp.Height, 'Bitmap height should be set');
    Assert.AreEqual(Integer(ColorToRGB(clWindow)), Integer(Thumb.ThumbBmp.Canvas.Pixels[75, 75]), 'Blank thumbnail must be filled with the window colour');
  FINALLY
    FreeAndNil(Thumb);
  END;
end;


procedure TTestGraphResizeWinThumb.TestGenerateThumbnail_WithValidFile;
var
  Thumb: TFileThumb;
begin
  CreateTempImageFile;

  Thumb:= TFileThumb.Create;
  TRY
    Thumb.Width:= 64;
    Thumb.FilePath:= FTempFile;

    Assert.WillNotRaiseAny(
      procedure
      begin
        Thumb.GenerateThumbnail;
      end,
      'Thumb.GenerateThumbnail must not raise');

    Assert.AreEqual(64, Thumb.ThumbBmp.Width, 'Thumbnail width should be 64');
    Assert.AreEqual(64, Thumb.ThumbBmp.Height, 'Thumbnail height should be 64');
    { The source is solid red, the blank fill is clWindow: a red centre proves the shell thumbnail was produced }
    Assert.AreEqual(Integer(clRed), Integer(Thumb.ThumbBmp.Canvas.Pixels[32, 32]), 'Centre pixel must be red (the source image)');
  FINALLY
    FreeAndNil(Thumb);
  END;
end;


{ GenerateThumbnail2 Tests }

procedure TTestGraphResizeWinThumb.TestGenerateThumbnail2_EmptyFilePath;
var
  Thumb: TFileThumb;
begin
  CreateTempImageFile;

  Thumb:= TFileThumb.Create;
  TRY
    { First a real image, then clear the path: a setter that ignored '' would draw the file's icon or thumbnail }
    Thumb.FilePath:= FTempFile;
    Thumb.FilePath:= '';
    Blacken(Thumb);

    Thumb.GenerateThumbnail2;

    Assert.AreEqual('', Thumb.FilePath, 'FilePath must be empty');
    CheckBlank(Thumb, 100);
  FINALLY
    FreeAndNil(Thumb);
  END;
end;


procedure TTestGraphResizeWinThumb.TestGenerateThumbnail2_NonExistentFile;
var
  Thumb: TFileThumb;
begin
  Thumb:= TFileThumb.Create;
  TRY
    Thumb.FilePath:= 'C:\NonExistent\File\That\Does\Not\Exist.jpg';
    Blacken(Thumb);

    Thumb.GenerateThumbnail2;

    { A missing file gets no icon either: the centre, where the icon would go, stays the window colour }
    CheckBlank(Thumb, 100);
  FINALLY
    FreeAndNil(Thumb);
  END;
end;


procedure TTestGraphResizeWinThumb.TestGenerateThumbnail2_BitmapSizeSet;
var
  Thumb: TFileThumb;
begin
  Thumb:= TFileThumb.Create;
  TRY
    Thumb.Width:= 120;
    Thumb.FilePath:= '';  // Empty path, but bitmap should still be sized
    { Shrink and blacken the bitmap behind the setter's back, so only GenerateThumbnail2 can restore it }
    Thumb.ThumbBmp.SetSize(10, 10);
    Thumb.ThumbBmp.Canvas.Brush.Color:= clBlack;
    Thumb.ThumbBmp.Canvas.FillRect(Rect(0, 0, 10, 10));
    Thumb.GenerateThumbnail2;

    Assert.AreEqual(120, Thumb.ThumbBmp.Width, 'Bitmap width should be set');
    Assert.AreEqual(120, Thumb.ThumbBmp.Height, 'Bitmap height should be set');
    Assert.AreEqual(Integer(ColorToRGB(clWindow)), Integer(Thumb.ThumbBmp.Canvas.Pixels[60, 60]), 'Blank thumbnail must be filled with the window colour');
  FINALLY
    FreeAndNil(Thumb);
  END;
end;


{ ThumbBmp Property Tests }

procedure TTestGraphResizeWinThumb.TestThumbBmp_NotNil;
var
  Thumb: TFileThumb;
  Bmp: TBitmap;
begin
  CreateTempImageFile;

  Thumb:= TFileThumb.Create;
  TRY
    Bmp:= Thumb.ThumbBmp;
    Assert.IsNotNull(Bmp, 'ThumbBmp should never be nil');

    Thumb.Width:= 64;
    Thumb.FilePath:= FTempFile;
    Thumb.GenerateThumbnail;

    Assert.AreSame(Bmp, Thumb.ThumbBmp, 'ThumbBmp must stay the same object: GenerateThumbnail replaces its handle, not the bitmap');
    { The source is solid red, the blank fill is clWindow: a red centre proves ThumbBmp holds the shell thumbnail }
    Assert.AreEqual(Integer(clRed), Integer(Thumb.ThumbBmp.Canvas.Pixels[32, 32]), 'ThumbBmp must hold the generated thumbnail (red centre)');
  FINALLY
    FreeAndNil(Thumb);
  END;
end;


procedure TTestGraphResizeWinThumb.TestThumbBmp_HasCorrectPixelFormat;
var
  Thumb: TFileThumb;
begin
  Thumb:= TFileThumb.Create;
  TRY
    Assert.AreEqual(pf24bit, Thumb.ThumbBmp.PixelFormat, 'ThumbBmp should be pf24bit');
  FINALLY
    FreeAndNil(Thumb);
  END;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestGraphResizeWinThumb);

end.
