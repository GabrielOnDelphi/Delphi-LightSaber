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
  LightCore.IO,
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
  ForceDirectoriesE(FTempDir);
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

{ The caller sets URL and DestFile before Start, so the constructor must create the thread suspended.
  TThread.AfterConstruction resumes a thread created with Create(FALSE), which clears Suspended
  (c:\Delphi\Delphi 13\source\rtl\common\System.Classes.pas, TThread.AfterConstruction and TThread.InternalStart). }
procedure TTestDownloadIndy.TestTSendThread_Create;
var
  Thread: TSendThread;
begin
  Thread:= TSendThread.Create;
  TRY
    Assert.IsTrue(Thread.Handle <> 0, 'The constructor must create the OS thread');
    Assert.IsTrue(Thread.Suspended, 'The thread must wait for Start');
    Assert.IsFalse(Thread.FreeOnTerminate, 'The caller frees the thread itself');
  FINALLY
    FreeAndNil(Thread);
  END;
end;


procedure TTestDownloadIndy.TestTSendThread_CreateDestroy;
begin
  // Test that create/destroy cycle works without memory leaks
  Assert.WillNotRaiseAny(
    procedure
    var
      Thread: TSendThread;
    begin
      Thread:= TSendThread.Create;
      Thread.URL:= 'https://example.com';
      Thread.DestFile:= 'C:\test.txt';
      FreeAndNil(Thread);
    end,
    'Create/Destroy cycle should work without errors');
end;


{ Invalid URL Tests

  The URL points at port 1 of the loopback address, where nothing listens: the request fails on this PC, with no
  DNS lookup and no traffic leaving it. Plain http, so a missing OpenSSL DLL is not the cause either: TIdSSLIOHandlerSocketOpenSSL
  ignores it in pass-through mode (c:\Delphi\Delphi 13\source\Indy10\Protocols\IdSSLOpenSSL.pas:2789-2790).
  The connect fails with an Indy socket error: 10061 (connection refused), or 10013 when a firewall blocks the EXE. }
CONST
  UnreachableURL = 'http://127.0.0.1:1/file.txt';

procedure TTestDownloadIndy.TestDownloadFile_InvalidURL_ReturnsFalse;
var
  ErrorMsg: string;
  Result: Boolean;
  TempFile: string;
begin
  TempFile:= GetTempFile('.txt');

  // An invalid/unreachable URL should return False with an error message
  Result:= DownloadFile(UnreachableURL, '', TempFile, ErrorMsg);

  Assert.IsFalse(Result, 'Should return False for invalid URL');
  Assert.AreEqual(1, Pos('Socket Error #', ErrorMsg), 'ErrorMsg must carry the socket error of the failed connect. Got: ' + ErrorMsg);
  Assert.AreEqual(' (-1)', Copy(ErrorMsg, Length(ErrorMsg) - 4, 5), 'No HTTP response arrived, so the response code in ErrorMsg is -1. Got: ' + ErrorMsg);
  Assert.IsTrue(NOT FileExists(TempFile) OR (LightCore.IO.GetFileSize(TempFile) = 0), 'Nothing may be written to the destination file');
end;


procedure TTestDownloadIndy.TestDownloadThread_InvalidURL_ReturnsFalse;
var
  ErrorMsg: string;
  Result: Boolean;
  TempFile: string;
begin
  TempFile:= GetTempFile('.txt');

  // An invalid/unreachable URL should return False with an error message
  Result:= DownloadThread(UnreachableURL, TempFile, ErrorMsg);

  Assert.IsFalse(Result, 'Should return False for invalid URL');
  Assert.AreEqual(1, Pos('Socket Error #', ErrorMsg), 'ErrorMsg must carry the socket error of the failed connect. Got: ' + ErrorMsg);
  Assert.AreEqual(' (-1)', Copy(ErrorMsg, Length(ErrorMsg) - 4, 5), 'No HTTP response arrived, so the response code in ErrorMsg is -1. Got: ' + ErrorMsg);
  Assert.IsFalse(FileExists(TempFile), 'DownloadThread saves the file only after a successful Get');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestDownloadIndy);

end.
