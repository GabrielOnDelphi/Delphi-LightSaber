unit Test.LightCore.Download.Thread;

{=============================================================================================================
   Unit tests for LightCore.Download.Thread.pas
   Tests the TWinInetObj threaded download class.

   Note: Full integration tests require network access.
   These tests focus on class construction, parameter validation, and property behavior.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes;

type
  [TestFixture]
  TTestDownloadThread = class
  private
    FDoneCount: Integer;
    FDoneSender: TObject;
    FDoneThreadID: TThreadID;
    procedure DownloadDone(Sender: TObject);
  public
    { Constructor/Destructor Tests }
    [Test]
    procedure TestCreate_InitializesProperties;

    [Test]
    procedure TestCreate_DataIsNil;

    [Test]
    procedure TestCreate_FreeOnTerminateIsFalse;

    { URL Property Tests }
    [Test]
    procedure TestSetURL_EmptyURL;

    [Test]
    procedure TestSetURL_InvalidURL_NoHttp;

    [Test]
    procedure TestSetURL_InvalidURL_TooShort;

    [Test]
    procedure TestSetURL_ValidHTTP;

    [Test]
    procedure TestSetURL_ValidHTTPS;

    [Test]
    procedure TestSetURL_HttpInMiddle;

    { DownloadSuccess Tests }
    [Test]
    procedure TestDownloadSuccess_BeforeExecute;

    { Property Access Tests }
    [Test]
    procedure TestHttpRetCode_InitiallyEmpty;

    { Event Tests }
    [Test]
    procedure TestOnDownloadDone_FiresInMainThread;
  end;

implementation

uses
  LightCore.Download.Thread;


{ Constructor/Destructor Tests }

procedure TTestDownloadThread.TestCreate_InitializesProperties;
var
  Downloader: TWinInetObj;
begin
  Downloader:= TWinInetObj.Create;
  TRY
    Assert.AreEqual('', Downloader.UserAgent, 'UserAgent should be empty');
    Assert.AreEqual('', Downloader.Header, 'Header should be empty');
    Assert.AreEqual('', Downloader.Referer, 'Referer should be empty');
    Assert.IsFalse(Downloader.SSL, 'SSL should be False');
    Assert.AreEqual('', Downloader.HttpRetCode, 'HttpRetCode should be empty');
  FINALLY
    FreeAndNil(Downloader);
  END;
end;


procedure TTestDownloadThread.TestCreate_DataIsNil;
var
  Downloader: TWinInetObj;
begin
  Downloader:= TWinInetObj.Create;
  TRY
    Assert.IsNull(Downloader.Data, 'Data should be NIL before download');
  FINALLY
    FreeAndNil(Downloader);
  END;
end;


procedure TTestDownloadThread.TestCreate_FreeOnTerminateIsFalse;
var
  Downloader: TWinInetObj;
begin
  Downloader:= TWinInetObj.Create;
  TRY
    Assert.IsFalse(Downloader.FreeOnTerminate, 'FreeOnTerminate should be False');
  FINALLY
    FreeAndNil(Downloader);
  END;
end;


{ URL Property Tests }

procedure TTestDownloadThread.TestSetURL_EmptyURL;
var
  Downloader: TWinInetObj;
begin
  Downloader:= TWinInetObj.Create;
  TRY
    Assert.WillRaise(
      procedure
      begin
        Downloader.URL:= '';
      end,
      EAssertionFailed,
      'Should raise assertion for empty URL');
  FINALLY
    FreeAndNil(Downloader);
  END;
end;


procedure TTestDownloadThread.TestSetURL_InvalidURL_NoHttp;
var
  Downloader: TWinInetObj;
begin
  Downloader:= TWinInetObj.Create;
  TRY
    Assert.WillRaise(
      procedure
      begin
        Downloader.URL:= 'ftp://example.com/file.txt';
      end,
      Exception,
      'Should raise exception for non-HTTP URL');
  FINALLY
    FreeAndNil(Downloader);
  END;
end;


procedure TTestDownloadThread.TestSetURL_InvalidURL_TooShort;
var
  Downloader: TWinInetObj;
begin
  Downloader:= TWinInetObj.Create;
  TRY
    Assert.WillRaise(
      procedure
      begin
        Downloader.URL:= 'http://x';
      end,
      Exception,
      'Should raise exception for URL too short');
  FINALLY
    FreeAndNil(Downloader);
  END;
end;


procedure TTestDownloadThread.TestSetURL_ValidHTTP;
var
  Downloader: TWinInetObj;
begin
  Downloader:= TWinInetObj.Create;
  TRY
    Assert.WillNotRaiseAny(
      procedure
      begin
        Downloader.URL:= 'http://example.com/file.txt';
      end,
      'Should accept valid HTTP URL');

    Assert.AreEqual('http://example.com/file.txt', Downloader.URL, 'URL should be set');
  FINALLY
    FreeAndNil(Downloader);
  END;
end;


procedure TTestDownloadThread.TestSetURL_ValidHTTPS;
var
  Downloader: TWinInetObj;
begin
  Downloader:= TWinInetObj.Create;
  TRY
    Assert.WillNotRaiseAny(
      procedure
      begin
        Downloader.URL:= 'https://example.com/file.txt';
      end,
      'Should accept valid HTTPS URL');

    Assert.AreEqual('https://example.com/file.txt', Downloader.URL, 'URL should be set');
  FINALLY
    FreeAndNil(Downloader);
  END;
end;


procedure TTestDownloadThread.TestSetURL_HttpInMiddle;
var
  Downloader: TWinInetObj;
begin
  Downloader:= TWinInetObj.Create;
  TRY
    Assert.WillRaise(
      procedure
      begin
        // URL has 'http' but not at the start
        Downloader.URL:= 'ftp://site.http.com/file.txt';
      end,
      Exception,
      'Should raise exception when http is not at start');
  FINALLY
    FreeAndNil(Downloader);
  END;
end;


{ DownloadSuccess Tests }

procedure TTestDownloadThread.TestDownloadSuccess_BeforeExecute;
var
  Downloader: TWinInetObj;
begin
  Downloader:= TWinInetObj.Create;
  TRY
    Assert.IsFalse(Downloader.DownloadSuccess, 'DownloadSuccess should be False before download');
  FINALLY
    FreeAndNil(Downloader);
  END;
end;


{ Property Access Tests }

procedure TTestDownloadThread.TestHttpRetCode_InitiallyEmpty;
var
  Downloader: TWinInetObj;
begin
  Downloader:= TWinInetObj.Create;
  TRY
    Assert.AreEqual('', Downloader.HttpRetCode, 'HttpRetCode should be empty initially');
  FINALLY
    FreeAndNil(Downloader);
  END;
end;


{ Event Tests }

procedure TTestDownloadThread.DownloadDone(Sender: TObject);
begin
  Inc(FDoneCount);
  FDoneSender:= Sender;
  FDoneThreadID:= TThread.CurrentThread.ThreadID;
end;


{ Needs no network: port 1 on the loopback address refuses the connection at once, so the download fails fast,
  and OnDownloadDone must fire anyway - once, with the downloader as Sender, in the main thread (Synchronize). }
procedure TTestDownloadThread.TestOnDownloadDone_FiresInMainThread;
var
  Downloader: TWinInetObj;
  Waited: Integer;
begin
  FDoneCount:= 0;
  FDoneSender:= NIL;
  FDoneThreadID:= 0;

  Downloader:= TWinInetObj.Create;
  TRY
    Downloader.URL:= 'http://127.0.0.1:1/';
    Downloader.OnDownloadDone:= DownloadDone;
    Downloader.Start;

    { Synchronize waits for the main thread, so the main thread must serve it while it waits }
    Waited:= 0;
    while NOT Downloader.Finished AND (Waited < 30000) DO
      begin
        CheckSynchronize(10);
        Inc(Waited, 10);
      end;
    Assert.IsTrue(Downloader.Finished, 'Precondition: the download thread ended within 30 s');
    Downloader.WaitFor;

    Assert.AreEqual(1, FDoneCount, 'OnDownloadDone must fire exactly once');
    Assert.AreSame(Downloader, FDoneSender, 'Sender must be the downloader');
    Assert.IsTrue(FDoneThreadID = MainThreadID, 'OnDownloadDone must run in the main thread');
    Assert.IsFalse(Downloader.DownloadSuccess, 'Precondition: the refused connection gives no data');
    Assert.AreNotEqual('', Downloader.HttpRetCode, 'The failed download must leave an error text');
  FINALLY
    FreeAndNil(Downloader);
  END;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestDownloadThread);

end.
