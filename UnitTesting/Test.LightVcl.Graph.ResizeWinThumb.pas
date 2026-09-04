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
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor/Destructor Tests }
    [Test]
    procedure TestCreate_InitializesProperly;

    [Test]
    procedure TestCreate_BitmapNotNil;

    [Test]
    procedure TestCreate_DefaultWidth;

    [Test]
    procedure TestCreate_DefaultFilePath;

    [Test]
    procedure TestDestroy_NoMemoryLeak;

    { Width Property Tests }
    [Test]
    procedure TestSetWidth_ValidValue;

    [Test]
    procedure TestSetWidth_BelowMinimum_ClampsToMin;

    [Test]
    procedure TestSetWidth_AboveMaximum_ClampsToMax;

    [Test]
    procedure TestSetWidth_SameValue_NoChange;

    [Test]
    procedure TestSetWidth_UpdatesBitmapSize;

    { FilePath Property Tests }
    [Test]
    procedure TestSetFilePath_ValidPath;

    [Test]
    procedure TestSetFilePath_EmptyPath;

    [Test]
    procedure TestSetFilePath_SamePath_NoChange;

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


{ Constructor/Destructor Tests }

procedure TTestGraphResizeWinThumb.TestCreate_InitializesProperly;
var
  Thumb: TFileThumb;
begin
  Thumb:= TFileThumb.Create;
  TRY
    Assert.IsNotNull(Thumb, 'TFileThumb should be created');
  FINALLY
    FreeAndNil(Thumb);
  END;
end;


procedure TTestGraphResizeWinThumb.TestCreate_BitmapNotNil;
var
  Thumb: TFileThumb;
begin
  Thumb:= TFileThumb.Create;
  TRY
    Assert.IsNotNull(Thumb.ThumbBmp, 'ThumbBmp should not be nil after creation');
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
  FINALLY
    FreeAndNil(Thumb);
  END;
end;


procedure TTestGraphResizeWinThumb.TestDestroy_NoMemoryLeak;
var
  Thumb: TFileThumb;
begin
  { This test verifies the destructor runs without issues.
    Memory leak detection is handled by FastMM or similar memory manager. }
  Thumb:= TFileThumb.Create;
  Thumb.Width:= 200;
  Thumb.FilePath:= 'C:\nonexistent.bmp';
  FreeAndNil(Thumb);

  Assert.IsNull(Thumb, 'Thumb should be nil after FreeAndNil');
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


procedure TTestGraphResizeWinThumb.TestSetWidth_SameValue_NoChange;
var
  Thumb: TFileThumb;
  OriginalWidth: Integer;
begin
  Thumb:= TFileThumb.Create;
  TRY
    OriginalWidth:= Thumb.Width;
    Thumb.Width:= OriginalWidth;  // Set same value
    Assert.AreEqual(OriginalWidth, Thumb.Width, 'Width should remain unchanged');
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


procedure TTestGraphResizeWinThumb.TestSetFilePath_SamePath_NoChange;
var
  Thumb: TFileThumb;
begin
  Thumb:= TFileThumb.Create;
  TRY
    Thumb.FilePath:= 'C:\Test\image.jpg';
    Thumb.FilePath:= 'C:\Test\image.jpg';  // Same path
    Assert.AreEqual('C:\Test\image.jpg', Thumb.FilePath, 'FilePath should remain unchanged');
  FINALLY
    FreeAndNil(Thumb);
  END;
end;


{ GenerateThumbnail Tests }

procedure TTestGraphResizeWinThumb.TestGenerateThumbnail_EmptyFilePath;
var
  Thumb: TFileThumb;
begin
  Thumb:= TFileThumb.Create;
  TRY
    Thumb.FilePath:= '';

    Assert.WillNotRaiseAny(
      procedure
      begin
        Thumb.GenerateThumbnail;
      end,
      'Thumb.GenerateThumbnail must not raise');
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

    Assert.WillNotRaiseAny(
      procedure
      begin
        Thumb.GenerateThumbnail;
      end,
      'Thumb.GenerateThumbnail must not raise');
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
    Thumb.GenerateThumbnail;

    Assert.AreEqual(150, Thumb.ThumbBmp.Width, 'Bitmap width should be set');
    Assert.AreEqual(150, Thumb.ThumbBmp.Height, 'Bitmap height should be set');
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

    // Verify bitmap was generated (size should be set)
    Assert.AreEqual(64, Thumb.ThumbBmp.Width, 'Thumbnail width should be 64');
    Assert.AreEqual(64, Thumb.ThumbBmp.Height, 'Thumbnail height should be 64');
  FINALLY
    FreeAndNil(Thumb);
  END;
end;


{ GenerateThumbnail2 Tests }

procedure TTestGraphResizeWinThumb.TestGenerateThumbnail2_EmptyFilePath;
var
  Thumb: TFileThumb;
begin
  Thumb:= TFileThumb.Create;
  TRY
    Thumb.FilePath:= '';

    Assert.WillNotRaiseAny(
      procedure
      begin
        Thumb.GenerateThumbnail2;
      end,
      'Thumb.GenerateThumbnail2 must not raise');
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

    Assert.WillNotRaiseAny(
      procedure
      begin
        Thumb.GenerateThumbnail2;
      end,
      'Thumb.GenerateThumbnail2 must not raise');
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
    Thumb.GenerateThumbnail2;

    Assert.AreEqual(120, Thumb.ThumbBmp.Width, 'Bitmap width should be set');
    Assert.AreEqual(120, Thumb.ThumbBmp.Height, 'Bitmap height should be set');
  FINALLY
    FreeAndNil(Thumb);
  END;
end;


{ ThumbBmp Property Tests }

procedure TTestGraphResizeWinThumb.TestThumbBmp_NotNil;
var
  Thumb: TFileThumb;
begin
  Thumb:= TFileThumb.Create;
  TRY
    Assert.IsNotNull(Thumb.ThumbBmp, 'ThumbBmp should never be nil');
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
