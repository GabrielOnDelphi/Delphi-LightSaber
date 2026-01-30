unit Test.LightVcl.Graph.Gif;

{=============================================================================================================
   Unit tests for LightVcl.Graph.Gif.pas
   Tests the GIF loading, frame extraction, and animation detection functions.

   Includes TestInsight support: define TESTINSIGHT in project options.

   Note: Some tests require actual GIF files. Tests that require files will be skipped
   if the test files are not available. Place test GIF files in the AppSysDir folder:
     - test_static.gif (single frame GIF)
     - test_animated.gif (multi-frame animated GIF)
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  Vcl.Graphics;

type
  [TestFixture]
  TTestGraphGif = class
  private
    FTestFolder: string;
    FAnimatedGifPath: string;
    FStaticGifPath: string;
    function HasTestFiles: Boolean;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { TGifLoader - Constructor/Destructor Tests }
    [Test]
    procedure TestGifLoader_Create_NotNil;

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
  LightVcl.Graph.Gif,
  LightCore.AppData,
  LightCore.IO;


procedure TTestGraphGif.Setup;
begin
  { Ensure AppDataCore is created for tests }
  if AppDataCore = NIL then
    AppDataCore:= TAppDataCore.Create('TestApp');

  { Set up test file paths }
  FTestFolder:= AppDataCore.AppSysDir;
  FAnimatedGifPath:= FTestFolder + 'test_animated.gif';
  FStaticGifPath:= FTestFolder + 'test_static.gif';
end;


procedure TTestGraphGif.TearDown;
begin
  { No cleanup needed }
end;


function TTestGraphGif.HasTestFiles: Boolean;
begin
  Result:= FileExists(FAnimatedGifPath) OR FileExists(FStaticGifPath);
end;


{ TGifLoader - Constructor/Destructor Tests }

procedure TTestGraphGif.TestGifLoader_Create_NotNil;
var
  Loader: TGifLoader;
begin
  Loader:= TGifLoader.Create;
  TRY
    Assert.IsNotNull(Loader, 'TGifLoader.Create should return a valid object');
  FINALLY
    FreeAndNil(Loader);
  END;
end;


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


procedure TTestGraphGif.TestGifLoader_Destroy_NoException;
var
  Loader: TGifLoader;
begin
  Assert.WillNotRaise(
    procedure
    begin
      Loader:= TGifLoader.Create;
      FreeAndNil(Loader);
    end);
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
    { Even without opening, requesting a very high frame number should not crash }
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
var
  Result: Boolean;
begin
  Result:= IsAnimated('C:\NonExistent\FakeFile.gif');
  Assert.IsFalse(Result, 'IsAnimated should return False for non-existent file');
end;


procedure TTestGraphGif.TestIsAnimated_EmptyPath;
var
  Result: Boolean;
begin
  Result:= IsAnimated('');
  Assert.IsFalse(Result, 'IsAnimated should return False for empty path');
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
  if NOT FileExists(FStaticGifPath) then
  begin
    Assert.Pass('Test skipped: test_static.gif not available');
    EXIT;
  end;

  Loader:= TGifLoader.Create;
  TRY
    OpenResult:= Loader.Open(FStaticGifPath);
    { Static GIF should fail because it has only 1 frame }
    Assert.IsFalse(OpenResult, 'Open should return False for static (1-frame) GIF');
  FINALLY
    FreeAndNil(Loader);
  END;
end;


procedure TTestGraphGif.TestGifLoader_Open_AnimatedGif;
var
  Loader: TGifLoader;
  OpenResult: Boolean;
begin
  if NOT FileExists(FAnimatedGifPath) then
  begin
    Assert.Pass('Test skipped: test_animated.gif not available');
    EXIT;
  end;

  Loader:= TGifLoader.Create;
  TRY
    OpenResult:= Loader.Open(FAnimatedGifPath);
    Assert.IsTrue(OpenResult, 'Open should return True for animated GIF');
    Assert.IsTrue(Loader.FrameCount > 1, 'Animated GIF should have more than 1 frame');
  FINALLY
    FreeAndNil(Loader);
  END;
end;


procedure TTestGraphGif.TestGifLoader_ExtractFrame_ValidFrame;
var
  Loader: TGifLoader;
  Frame: TBitmap;
begin
  if NOT FileExists(FAnimatedGifPath) then
  begin
    Assert.Pass('Test skipped: test_animated.gif not available');
    EXIT;
  end;

  Loader:= TGifLoader.Create;
  TRY
    if Loader.Open(FAnimatedGifPath) then
    begin
      Frame:= Loader.ExtractFrame(0);
      TRY
        Assert.IsNotNull(Frame, 'ExtractFrame(0) should return valid bitmap');
        Assert.IsTrue(Frame.Width > 0, 'Extracted frame should have positive width');
        Assert.IsTrue(Frame.Height > 0, 'Extracted frame should have positive height');
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
  if NOT FileExists(FAnimatedGifPath) then
  begin
    Assert.Pass('Test skipped: test_animated.gif not available');
    EXIT;
  end;

  Loader:= TGifLoader.Create;
  TRY
    if Loader.Open(FAnimatedGifPath) then
    begin
      { Frame delay should be a reasonable value (typically 10-1000 ms) }
      Assert.IsTrue(Loader.FrameDelay >= 0, 'FrameDelay should be non-negative');
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
  if NOT FileExists(FStaticGifPath) then
  begin
    Assert.Pass('Test skipped: test_static.gif not available');
    EXIT;
  end;

  Result:= IsAnimated(FStaticGifPath);
  Assert.IsFalse(Result, 'IsAnimated should return False for static GIF');
end;


procedure TTestGraphGif.TestIsAnimated_AnimatedGif;
var
  Result: Boolean;
begin
  if NOT FileExists(FAnimatedGifPath) then
  begin
    Assert.Pass('Test skipped: test_animated.gif not available');
    EXIT;
  end;

  Result:= IsAnimated(FAnimatedGifPath);
  Assert.IsTrue(Result, 'IsAnimated should return True for animated GIF');
end;


procedure TTestGraphGif.TestExtractMiddleFrame_AnimatedGif;
var
  Frame: TBitmap;
  FrameCount: Cardinal;
begin
  if NOT FileExists(FAnimatedGifPath) then
  begin
    Assert.Pass('Test skipped: test_animated.gif not available');
    EXIT;
  end;

  Frame:= ExtractMiddleFrame(FAnimatedGifPath, FrameCount);
  TRY
    Assert.IsNotNull(Frame, 'ExtractMiddleFrame should return valid bitmap for animated GIF');
    Assert.IsTrue(FrameCount > 1, 'FrameCount should be greater than 1 for animated GIF');
    Assert.IsTrue(Frame.Width > 0, 'Extracted frame should have positive width');
    Assert.IsTrue(Frame.Height > 0, 'Extracted frame should have positive height');
  FINALLY
    FreeAndNil(Frame);
  END;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestGraphGif);

end.
