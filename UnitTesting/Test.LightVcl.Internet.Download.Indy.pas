unit Test.LightVcl.Internet.Download.Indy;

{=============================================================================================================
   Unit tests for LightVcl.Internet.Download.Indy.pas
   Tests the Indy-based file download functionality.

   Note: Full integration tests require network access.
   These tests focus on parameter validation, thread class creation, and error handling.

   IMPORTANT: These tests require the OpenSSL DLLs (libeay32.dll & ssleay32.dll) for HTTPS tests.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  System.Classes;

type
  [TestFixture]
  TTestDownloadIndy = class
  private
    FTempDir: string;
    function GetTempFile(const Extension: string = '.tmp'): string;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { DownloadFile Parameter Validation Tests }
    [Test]
    procedure TestDownloadFile_EmptyURL;

    [Test]
    procedure TestDownloadFile_EmptyDestination;

    [Test]
    procedure TestDownloadFile_EmptyRefererAllowed;

    { DownloadThread Parameter Validation Tests }
    [Test]
    procedure TestDownloadThread_EmptyURL;

    [Test]
    procedure TestDownloadThread_EmptyDestination;

    { DownloadThread2 Parameter Validation Tests }
    [Test]
    procedure TestDownloadThread2_EmptyURL;

    [Test]
    procedure TestDownloadThread2_EmptyDestination;

    { TSendThread Tests }
    [Test]
    procedure TestTSendThread_Create;

    [Test]
    procedure TestTSendThread_CreateDestroy;

    { Invalid URL Tests - These require network access }
    [Test]
    procedure TestDownloadFile_InvalidURL_ReturnsFalse;

    [Test]
    procedure TestDownloadThread_InvalidURL_ReturnsFalse;
  end;

implementation

uses
  LightVcl.Internet.Download.Indy;


procedure TTestDownloadIndy.Setup;
begin
  FTempDir:= TPath.Combine(TPath.GetTempPath, 'TestDownloadIndy_' + TGUID.NewGuid.ToString);
  ForceDirectories(FTempDir);
end;


procedure TTestDownloadIndy.TearDown;
begin
  if DirectoryExists(FTempDir)
  then TDirectory.Delete(FTempDir, True);
end;


function TTestDownloadIndy.GetTempFile(const Extension: string): string;
begin
  Result:= TPath.Combine(FTempDir, TGUID.NewGuid.ToString + Extension);
end;


{ DownloadFile Parameter Validation Tests }

procedure TTestDownloadIndy.TestDownloadFile_EmptyURL;
var
  ErrorMsg: string;
begin
  Assert.WillRaise(
    procedure
    begin
      DownloadFile('', '', GetTempFile, ErrorMsg);
    end,
    EAssertionFailed,
    'Should raise assertion for empty URL');
end;


procedure TTestDownloadIndy.TestDownloadFile_EmptyDestination;
var
  ErrorMsg: string;
begin
  Assert.WillRaise(
    procedure
    begin
      DownloadFile('https://example.com/file.txt', '', '', ErrorMsg);
    end,
    EAssertionFailed,
    'Should raise assertion for empty destination');
end;


procedure TTestDownloadIndy.TestDownloadFile_EmptyRefererAllowed;
var
  ErrorMsg: string;
  TempFile: string;
begin
  // Empty referer should be allowed (it's optional)
  TempFile:= GetTempFile('.txt');

  // This will fail due to network issues but should NOT raise assertion
  Assert.WillNotRaise(
    procedure
    begin
      DownloadFile('https://invalid-test-url-12345.com/file.txt', '', TempFile, ErrorMsg);
    end,
    EAssertionFailed,
    'Empty referer should be allowed');
end;


{ DownloadThread Parameter Validation Tests }

procedure TTestDownloadIndy.TestDownloadThread_EmptyURL;
var
  ErrorMsg: string;
begin
  Assert.WillRaise(
    procedure
    begin
      DownloadThread('', GetTempFile, ErrorMsg);
    end,
    EAssertionFailed,
    'Should raise assertion for empty URL');
end;


procedure TTestDownloadIndy.TestDownloadThread_EmptyDestination;
var
  ErrorMsg: string;
begin
  Assert.WillRaise(
    procedure
    begin
      DownloadThread('https://example.com/file.txt', '', ErrorMsg);
    end,
    EAssertionFailed,
    'Should raise assertion for empty destination');
end;


{ DownloadThread2 Parameter Validation Tests }

procedure TTestDownloadIndy.TestDownloadThread2_EmptyURL;
var
  ErrorMsg: string;
begin
  Assert.WillRaise(
    procedure
    begin
      DownloadThread2('', GetTempFile, ErrorMsg);
    end,
    EAssertionFailed,
    'Should raise assertion for empty URL');
end;


procedure TTestDownloadIndy.TestDownloadThread2_EmptyDestination;
var
  ErrorMsg: string;
begin
  Assert.WillRaise(
    procedure
    begin
      DownloadThread2('https://example.com/file.txt', '', ErrorMsg);
    end,
    EAssertionFailed,
    'Should raise assertion for empty destination');
end;


{ TSendThread Tests }

procedure TTestDownloadIndy.TestTSendThread_Create;
var
  Thread: TSendThread;
begin
  Thread:= TSendThread.Create;
  TRY
    Assert.IsNotNull(Thread, 'Thread should be created');
    Assert.AreEqual('', Thread.URL, 'URL should be empty initially');
    Assert.AreEqual('', Thread.DestFile, 'DestFile should be empty initially');
    Assert.AreEqual('', Thread.ErrorMsg, 'ErrorMsg should be empty initially');
  FINALLY
    FreeAndNil(Thread);
  END;
end;


procedure TTestDownloadIndy.TestTSendThread_CreateDestroy;
begin
  // Test that create/destroy cycle works without memory leaks
  Assert.WillNotRaise(
    procedure
    var
      Thread: TSendThread;
    begin
      Thread:= TSendThread.Create;
      Thread.URL:= 'https://example.com';
      Thread.DestFile:= 'C:\test.txt';
      FreeAndNil(Thread);
    end,
    Exception,
    'Create/Destroy cycle should work without errors');
end;


{ Invalid URL Tests }

procedure TTestDownloadIndy.TestDownloadFile_InvalidURL_ReturnsFalse;
var
  ErrorMsg: string;
  Result: Boolean;
  TempFile: string;
begin
  TempFile:= GetTempFile('.txt');

  // An invalid/unreachable URL should return False with an error message
  Result:= DownloadFile('https://invalid-domain-that-does-not-exist-12345.com/file.txt',
                        '', TempFile, ErrorMsg);

  Assert.IsFalse(Result, 'Should return False for invalid URL');
  Assert.IsNotEmpty(ErrorMsg, 'ErrorMsg should contain error description');
end;


procedure TTestDownloadIndy.TestDownloadThread_InvalidURL_ReturnsFalse;
var
  ErrorMsg: string;
  Result: Boolean;
  TempFile: string;
begin
  TempFile:= GetTempFile('.txt');

  // An invalid/unreachable URL should return False with an error message
  Result:= DownloadThread('https://invalid-domain-that-does-not-exist-12345.com/file.txt',
                          TempFile, ErrorMsg);

  Assert.IsFalse(Result, 'Should return False for invalid URL');
  Assert.IsNotEmpty(ErrorMsg, 'ErrorMsg should contain error description');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestDownloadIndy);

end.
