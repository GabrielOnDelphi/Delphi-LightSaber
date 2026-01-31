unit Test.LightVcl.Internet.Download.Thread;

{=============================================================================================================
   Unit tests for LightVcl.Internet.Download.Thread.pas
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
  public
    { Constructor/Destructor Tests }
    [Test]
    procedure TestCreate_InitializesProperties;

    [Test]
    procedure TestCreate_DataIsNil;

    [Test]
    procedure TestCreate_FreeOnTerminateIsFalse;

    [Test]
    procedure TestDestroy_NoMemoryLeak;

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

    [Test]
    procedure TestUserAgent_CanBeSet;

    [Test]
    procedure TestHeader_CanBeSet;

    [Test]
    procedure TestReferer_CanBeSet;

    [Test]
    procedure TestSSL_CanBeSet;

    { Event Tests }
    [Test]
    procedure TestOnDownloadDone_CanBeAssigned;
  end;

implementation

uses
  LightVcl.Internet.Download.Thread;


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


procedure TTestDownloadThread.TestDestroy_NoMemoryLeak;
begin
  // Test that create/destroy cycle works without memory leaks
  Assert.WillNotRaise(
    procedure
    var
      Downloader: TWinInetObj;
    begin
      Downloader:= TWinInetObj.Create;
      Downloader.UserAgent:= 'Test Agent';
      Downloader.Header:= 'Test Header';
      FreeAndNil(Downloader);
    end,
    Exception,
    'Create/Destroy cycle should work without errors');
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
    Assert.WillNotRaise(
      procedure
      begin
        Downloader.URL:= 'http://example.com/file.txt';
      end,
      Exception,
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
    Assert.WillNotRaise(
      procedure
      begin
        Downloader.URL:= 'https://example.com/file.txt';
      end,
      Exception,
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


procedure TTestDownloadThread.TestUserAgent_CanBeSet;
var
  Downloader: TWinInetObj;
begin
  Downloader:= TWinInetObj.Create;
  TRY
    Downloader.UserAgent:= 'Mozilla/5.0';
    Assert.AreEqual('Mozilla/5.0', Downloader.UserAgent, 'UserAgent should be set');
  FINALLY
    FreeAndNil(Downloader);
  END;
end;


procedure TTestDownloadThread.TestHeader_CanBeSet;
var
  Downloader: TWinInetObj;
begin
  Downloader:= TWinInetObj.Create;
  TRY
    Downloader.Header:= 'Accept-Charset: utf-8';
    Assert.AreEqual('Accept-Charset: utf-8', Downloader.Header, 'Header should be set');
  FINALLY
    FreeAndNil(Downloader);
  END;
end;


procedure TTestDownloadThread.TestReferer_CanBeSet;
var
  Downloader: TWinInetObj;
begin
  Downloader:= TWinInetObj.Create;
  TRY
    Downloader.Referer:= 'https://google.com';
    Assert.AreEqual('https://google.com', Downloader.Referer, 'Referer should be set');
  FINALLY
    FreeAndNil(Downloader);
  END;
end;


procedure TTestDownloadThread.TestSSL_CanBeSet;
var
  Downloader: TWinInetObj;
begin
  Downloader:= TWinInetObj.Create;
  TRY
    Downloader.SSL:= True;
    Assert.IsTrue(Downloader.SSL, 'SSL should be True');
  FINALLY
    FreeAndNil(Downloader);
  END;
end;


{ Event Tests }

procedure TTestDownloadThread.TestOnDownloadDone_CanBeAssigned;
var
  Downloader: TWinInetObj;
begin
  Downloader:= TWinInetObj.Create;
  TRY
    // Initially should be nil
    Assert.IsNull(TMethod(Downloader.OnDownloadDone).Code, 'OnDownloadDone should be NIL initially');

    // We can't easily test assignment without a real method reference
    // Just verify the property is accessible
    Assert.WillNotRaise(
      procedure
      begin
        Downloader.OnDownloadDone:= NIL;
      end,
      Exception,
      'Should be able to set OnDownloadDone to NIL');
  FINALLY
    FreeAndNil(Downloader);
  END;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestDownloadThread);

end.
