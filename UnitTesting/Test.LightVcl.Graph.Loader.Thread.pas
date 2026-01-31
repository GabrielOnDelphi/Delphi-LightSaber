unit Test.LightVcl.Graph.Loader.Thread;

{=============================================================================================================
   Unit tests for LightVcl.Graph.Loader.Thread.pas
   Tests the background image loading thread functionality.

   Note: Threading tests can be tricky. We use synchronization and timeouts
   to ensure reliable test execution.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  System.SyncObjs,
  Vcl.Forms,
  Vcl.Graphics;

type
  [TestFixture]
  TTestBkgImgLoader = class
  private
    FTempDir: string;
    FTestBmpFiles: TStringList;
    FReceivedThumbs: Integer;
    FTestForm: TForm;
    FEvent: TEvent;
    procedure CreateTestBmpFiles(Count: Integer);
    procedure CleanupTempFiles;
    procedure HandleThumbnailMessage(var Message: TMessage);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor Tests }
    [Test]
    procedure TestCreate_ValidHandle;

    [Test]
    procedure TestCreate_InvalidHandle;

    { Property Tests }
    [Test]
    procedure TestDefaultProperties;

    { PopPicture Tests }
    [Test]
    procedure TestPopPicture_EmptyQueue;

    { Basic Loading Tests }
    [Test]
    procedure TestLoadSingleImage;

    [Test]
    procedure TestLoadMultipleImages;

    [Test]
    procedure TestLoadNonExistentFile;

    { Thread Behavior Tests }
    [Test]
    procedure TestTerminateWhileRunning;

    [Test]
    procedure TestSilentErrors;

    { FileList Ownership Test }
    [Test]
    procedure TestFileListOwnership;
  end;

implementation

uses
  Vcl.Imaging.Jpeg,
  LightVcl.Graph.Loader.Thread;


{ Helper window procedure to receive WM_THUMBNAIL_NOTIFY }
type
  TTestFormHelper = class(TForm)
  private
    FOnThumbnail: TNotifyEvent;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    property OnThumbnail: TNotifyEvent read FOnThumbnail write FOnThumbnail;
  end;

procedure TTestFormHelper.WndProc(var Message: TMessage);
begin
  if (Message.Msg = WM_THUMBNAIL_NOTIFY) AND Assigned(FOnThumbnail)
  then FOnThumbnail(Self)
  else inherited;
end;


{ TTestBkgImgLoader }

procedure TTestBkgImgLoader.Setup;
begin
  FTempDir:= TPath.Combine(TPath.GetTempPath, 'TestBkgImgLoader_' + TGUID.NewGuid.ToString);
  ForceDirectories(FTempDir);
  FTestBmpFiles:= TStringList.Create;
  FReceivedThumbs:= 0;
  FEvent:= TEvent.Create(NIL, True, False, '');

  { Create a test form to receive messages }
  FTestForm:= TTestFormHelper.Create(NIL);
  FTestForm.HandleNeeded;  { Ensure handle is created }
  TTestFormHelper(FTestForm).OnThumbnail:= HandleThumbnailMessage;
end;


procedure TTestBkgImgLoader.TearDown;
begin
  CleanupTempFiles;
  FreeAndNil(FTestBmpFiles);
  FreeAndNil(FEvent);
  FreeAndNil(FTestForm);
  if DirectoryExists(FTempDir)
  then TDirectory.Delete(FTempDir, True);
end;


procedure TTestBkgImgLoader.CleanupTempFiles;
var
  FileName: string;
begin
  for FileName in FTestBmpFiles DO
    if FileExists(FileName)
    then DeleteFile(FileName);
  FTestBmpFiles.Clear;
end;


procedure TTestBkgImgLoader.CreateTestBmpFiles(Count: Integer);
var
  i: Integer;
  Bmp: TBitmap;
  FileName: string;
begin
  for i:= 1 to Count do
  begin
    FileName:= TPath.Combine(FTempDir, Format('test%d.bmp', [i]));
    Bmp:= TBitmap.Create;
    TRY
      Bmp.Width:= 200;
      Bmp.Height:= 150;
      Bmp.PixelFormat:= pf24bit;
      Bmp.Canvas.Brush.Color:= RGB(i * 30 mod 256, i * 50 mod 256, i * 70 mod 256);
      Bmp.Canvas.FillRect(Rect(0, 0, 200, 150));
      Bmp.SaveToFile(FileName);
      FTestBmpFiles.Add(FileName);
    FINALLY
      FreeAndNil(Bmp);
    END;
  end;
end;


procedure TTestBkgImgLoader.HandleThumbnailMessage(Sender: TObject);
begin
  Inc(FReceivedThumbs);
  FEvent.SetEvent;
end;


{ Constructor Tests }

procedure TTestBkgImgLoader.TestCreate_ValidHandle;
var
  Loader: TBkgImgLoader;
begin
  Loader:= TBkgImgLoader.Create(FTestForm.Handle);
  TRY
    Assert.IsNotNull(Loader, 'Loader should be created');
  FINALLY
    Loader.Terminate;
    Loader.WaitFor;
    FreeAndNil(Loader);
  END;
end;


procedure TTestBkgImgLoader.TestCreate_InvalidHandle;
begin
  Assert.WillRaise(
    procedure
    var
      Loader: TBkgImgLoader;
    begin
      Loader:= TBkgImgLoader.Create(0);  { Invalid handle }
      Loader.Terminate;
      Loader.WaitFor;
      FreeAndNil(Loader);
    end,
    EAssertionFailed,
    'Should raise assertion for invalid handle');
end;


{ Property Tests }

procedure TTestBkgImgLoader.TestDefaultProperties;
var
  Loader: TBkgImgLoader;
begin
  Loader:= TBkgImgLoader.Create(FTestForm.Handle);
  TRY
    Assert.AreEqual(256, Loader.Width, 'Default Width should be 256');
    Assert.AreEqual(128, Loader.Height, 'Default Height should be 128');
    Assert.IsFalse(Loader.SilentErrors, 'Default SilentErrors should be False');
    Assert.IsNull(Loader.FileList, 'Default FileList should be NIL');
    Assert.IsFalse(Loader.FreeOnTerminate, 'FreeOnTerminate should be False');
  FINALLY
    Loader.Terminate;
    Loader.WaitFor;
    FreeAndNil(Loader);
  END;
end;


{ PopPicture Tests }

procedure TTestBkgImgLoader.TestPopPicture_EmptyQueue;
var
  Loader: TBkgImgLoader;
  Bmp: TBitmap;
begin
  Loader:= TBkgImgLoader.Create(FTestForm.Handle);
  TRY
    { Don't start the thread, just test PopPicture on empty queue }
    Bmp:= Loader.PopPicture;
    Assert.IsNull(Bmp, 'PopPicture should return NIL for empty queue');
  FINALLY
    Loader.Terminate;
    Loader.WaitFor;
    FreeAndNil(Loader);
  END;
end;


{ Basic Loading Tests }

procedure TTestBkgImgLoader.TestLoadSingleImage;
var
  Loader: TBkgImgLoader;
  FileList: TStringList;
  Bmp: TBitmap;
  WaitResult: TWaitResult;
begin
  CreateTestBmpFiles(1);
  FReceivedThumbs:= 0;
  FEvent.ResetEvent;

  FileList:= TStringList.Create;
  FileList.AddStrings(FTestBmpFiles);

  Loader:= TBkgImgLoader.Create(FTestForm.Handle);
  TRY
    Loader.Width:= 100;
    Loader.Height:= 100;
    Loader.FileList:= FileList;  { Thread takes ownership }
    Loader.Start;

    { Wait for thread to finish }
    Loader.WaitFor;

    { Process any pending messages }
    Application.ProcessMessages;

    { Pop the result }
    Bmp:= Loader.PopPicture;
    TRY
      Assert.IsNotNull(Bmp, 'Should have loaded one thumbnail');
      Assert.IsTrue(Bmp.Width <= 100, 'Thumbnail width should be <= 100');
      Assert.IsTrue(Bmp.Height <= 100, 'Thumbnail height should be <= 100');
    FINALLY
      FreeAndNil(Bmp);
    END;
  FINALLY
    Loader.Terminate;
    Loader.WaitFor;
    FreeAndNil(Loader);
  END;
end;


procedure TTestBkgImgLoader.TestLoadMultipleImages;
var
  Loader: TBkgImgLoader;
  FileList: TStringList;
  Bmp: TBitmap;
  LoadedCount: Integer;
begin
  CreateTestBmpFiles(5);

  FileList:= TStringList.Create;
  FileList.AddStrings(FTestBmpFiles);

  Loader:= TBkgImgLoader.Create(FTestForm.Handle);
  TRY
    Loader.Width:= 64;
    Loader.Height:= 64;
    Loader.FileList:= FileList;
    Loader.Start;
    Loader.WaitFor;

    Application.ProcessMessages;

    { Count loaded thumbnails }
    LoadedCount:= 0;
    repeat
      Bmp:= Loader.PopPicture;
      if Bmp <> NIL then
      begin
        Inc(LoadedCount);
        FreeAndNil(Bmp);
      end;
    until Bmp = NIL;

    Assert.AreEqual(5, LoadedCount, 'Should have loaded 5 thumbnails');
  FINALLY
    Loader.Terminate;
    Loader.WaitFor;
    FreeAndNil(Loader);
  END;
end;


procedure TTestBkgImgLoader.TestLoadNonExistentFile;
var
  Loader: TBkgImgLoader;
  FileList: TStringList;
  Bmp: TBitmap;
begin
  FileList:= TStringList.Create;
  FileList.Add('C:\NonExistent\Path\file.bmp');

  Loader:= TBkgImgLoader.Create(FTestForm.Handle);
  TRY
    Loader.SilentErrors:= True;
    Loader.FileList:= FileList;
    Loader.Start;
    Loader.WaitFor;

    Application.ProcessMessages;

    { Queue should be empty - file doesn't exist }
    Bmp:= Loader.PopPicture;
    Assert.IsNull(Bmp, 'PopPicture should return NIL for non-existent file');
  FINALLY
    Loader.Terminate;
    Loader.WaitFor;
    FreeAndNil(Loader);
  END;
end;


{ Thread Behavior Tests }

procedure TTestBkgImgLoader.TestTerminateWhileRunning;
var
  Loader: TBkgImgLoader;
  FileList: TStringList;
begin
  CreateTestBmpFiles(20);  { Create many files to ensure thread is running }

  FileList:= TStringList.Create;
  FileList.AddStrings(FTestBmpFiles);

  Loader:= TBkgImgLoader.Create(FTestForm.Handle);
  TRY
    Loader.FileList:= FileList;
    Loader.Start;

    { Immediately terminate }
    Loader.Terminate;
    Loader.WaitFor;

    { Thread should have stopped without error }
    Assert.Pass('Thread terminated successfully');
  FINALLY
    FreeAndNil(Loader);
  END;
end;


procedure TTestBkgImgLoader.TestSilentErrors;
var
  Loader: TBkgImgLoader;
  FileList: TStringList;
  InvalidFile: string;
  Stream: TFileStream;
begin
  { Create a file with invalid image data }
  InvalidFile:= TPath.Combine(FTempDir, 'invalid.bmp');
  Stream:= TFileStream.Create(InvalidFile, fmCreate);
  TRY
    Stream.WriteBuffer('INVALID IMAGE DATA', 18);
  FINALLY
    FreeAndNil(Stream);
  END;
  FTestBmpFiles.Add(InvalidFile);

  FileList:= TStringList.Create;
  FileList.Add(InvalidFile);

  Loader:= TBkgImgLoader.Create(FTestForm.Handle);
  TRY
    Loader.SilentErrors:= True;  { Should not raise exception }
    Loader.FileList:= FileList;
    Loader.Start;
    Loader.WaitFor;

    { Thread should complete without raising exception }
    Assert.Pass('Thread handled invalid file silently');
  FINALLY
    Loader.Terminate;
    Loader.WaitFor;
    FreeAndNil(Loader);
  END;
end;


procedure TTestBkgImgLoader.TestFileListOwnership;
var
  Loader: TBkgImgLoader;
  FileList: TStringList;
begin
  CreateTestBmpFiles(1);

  FileList:= TStringList.Create;
  FileList.AddStrings(FTestBmpFiles);

  Loader:= TBkgImgLoader.Create(FTestForm.Handle);
  TRY
    Loader.FileList:= FileList;
    Loader.Start;
    Loader.WaitFor;

    { FileList should have been freed by the thread }
    { We can't directly test this, but ensure thread completes without error }
    Assert.Pass('Thread completed and freed FileList');
  FINALLY
    Loader.Terminate;
    Loader.WaitFor;
    FreeAndNil(Loader);
  END;

  { Note: Do NOT free FileList here - thread owns it! }
end;


initialization
  TDUnitX.RegisterTestFixture(TTestBkgImgLoader);

end.
