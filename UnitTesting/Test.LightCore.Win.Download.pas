unit Test.LightCore.Win.Download;

{=============================================================================================================
   2026.10.07
   Unit tests for LightCore.Win.Download.pas
   Tests HTTP download functionality using WinINet API.

   The HTTP tests talk to TLocalHttpServer (declared in Test.LightCore.Download), a small HTTP server on
   127.0.0.1 that the test starts itself, so they check the exact content and the request without the Internet.
   Only TestDownloadBytes_HttpsUrl needs the Internet (TLocalHttpServer has no TLS). Its skip leg runs only
   when an independent TCP probe also fails to reach the host.
=============================================================================================================}

interface
{$IFDEF MSWINDOWS}

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  Winapi.Windows,
  LightCore.Win.Download;

type
  [TestFixture]
  TTestDownloadWinInet = class
  private
    FTestDir: string;
    procedure CleanupTestDir;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { DownloadAsString Tests }
    [Test]
    procedure TestDownloadAsString_ValidUrl;

    [Test]
    procedure TestDownloadAsString_InvalidUrl_ReturnsEmpty;

    [Test]
    procedure TestDownloadAsString_EmptyUrl_ReturnsEmpty;

    [Test]
    procedure TestDownloadAsString_MalformedUrl_ReturnsEmpty;

    { DownloadBytes Tests }
    [Test]
    procedure TestDownloadBytes_ValidUrl_ReturnsSuccess;

    [Test]
    procedure TestDownloadBytes_InvalidUrl_ReturnsError;

    [Test]
    procedure TestDownloadBytes_EmptyUrl_ReturnsError;

    [Test]
    procedure TestDownloadBytes_WithReferer;

    { DownloadToFile Tests }
    [Test]
    procedure TestDownloadToFile_ValidUrl_CreatesFile;

    [Test]
    procedure TestDownloadToFile_InvalidUrl_NoFileCreated;

    [Test]
    procedure TestDownloadToFile_InvalidDirectory_RaisesException;

    { SSL Tests }
    [Test]
    procedure TestDownloadBytes_HttpsUrl;

    { Edge Cases }
    [Test]
    procedure TestDownloadBytes_UrlWithQueryParams;

    [Test]
    procedure TestDownloadBytes_UrlWithPort;
  end;
{$ENDIF}

implementation
{$IFDEF MSWINDOWS}

uses
  Test.LightCore.Download;


procedure TTestDownloadWinInet.Setup;
begin
  FTestDir:= TPath.Combine(TPath.GetTempPath, 'WinInetTest_' + TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FTestDir);
end;


procedure TTestDownloadWinInet.TearDown;
begin
  CleanupTestDir;
end;


procedure TTestDownloadWinInet.CleanupTestDir;
begin
  if TDirectory.Exists(FTestDir)
  then TDirectory.Delete(FTestDir, True);
end;


{ DownloadAsString Tests }

procedure TTestDownloadWinInet.TestDownloadAsString_ValidUrl;
var
  Server: TLocalHttpServer;
  Content: string;
begin
  Server:= TLocalHttpServer.Create(LOCAL_TEST_BODY);
  try
    Content:= DownloadAsString(Server.Url('/page.html'));
    Server.Stop;
  finally
    FreeAndNil(Server);
  end;

  Assert.AreEqual(LOCAL_TEST_BODY, Content, 'DownloadAsString must return the exact body');
end;


procedure TTestDownloadWinInet.TestDownloadAsString_InvalidUrl_ReturnsEmpty;
var
  Content: string;
begin
  { Invalid domain should return empty string (silent failure) }
  Content:= DownloadAsString('http://this.domain.does.not.exist.invalid/');
  Assert.AreEqual('', Content, 'Invalid URL should return empty string');
end;


procedure TTestDownloadWinInet.TestDownloadAsString_EmptyUrl_ReturnsEmpty;
var
  Content: string;
begin
  Content:= DownloadAsString('');
  Assert.AreEqual('', Content, 'Empty URL should return empty string');
end;


procedure TTestDownloadWinInet.TestDownloadAsString_MalformedUrl_ReturnsEmpty;
var
  Content: string;
begin
  Content:= DownloadAsString('not-a-valid-url');
  Assert.AreEqual('', Content, 'Malformed URL should return empty string');
end;


{ DownloadBytes Tests }

procedure TTestDownloadWinInet.TestDownloadBytes_ValidUrl_ReturnsSuccess;
var
  Server: TLocalHttpServer;
  Data: TBytes;
  ErrorCode: Cardinal;
begin
  Server:= TLocalHttpServer.Create(LOCAL_TEST_BODY);
  try
    ErrorCode:= DownloadBytes(Server.Url, '', Data);
    Server.Stop;

    Assert.AreEqual(Cardinal(ERROR_SUCCESS), ErrorCode, 'Should return ERROR_SUCCESS');
    Assert.AreEqual(LOCAL_TEST_BODY, TEncoding.UTF8.GetString(Data), 'DownloadBytes must return the exact body');
    Assert.IsTrue(Pos('GET / HTTP/1.1', Server.Requests) = 1, 'A download without PostData must send a GET for "/". Request: ' + Server.Requests);
  finally
    FreeAndNil(Server);
  end;
end;


procedure TTestDownloadWinInet.TestDownloadBytes_InvalidUrl_ReturnsError;
var
  Data: TBytes;
  ErrorCode: Cardinal;
begin
  ErrorCode:= DownloadBytes('http://this.domain.does.not.exist.invalid/', '', Data);

  { Should return an error code (not ERROR_SUCCESS) }
  Assert.AreNotEqual(Cardinal(ERROR_SUCCESS), ErrorCode, 'Invalid URL should return error code');
end;


procedure TTestDownloadWinInet.TestDownloadBytes_EmptyUrl_ReturnsError;
var
  Data: TBytes;
  ErrorCode: Cardinal;
begin
  ErrorCode:= DownloadBytes('', '', Data);

  { Empty URL should return an error (ERROR_INTERNET_UNRECOGNIZED_SCHEME or similar) }
  Assert.AreNotEqual(Cardinal(ERROR_SUCCESS), ErrorCode, 'Empty URL should return error code');
end;


procedure TTestDownloadWinInet.TestDownloadBytes_WithReferer;
var
  Server: TLocalHttpServer;
  Data: TBytes;
  ErrorCode: Cardinal;
begin
  Server:= TLocalHttpServer.Create(LOCAL_TEST_BODY);
  try
    ErrorCode:= DownloadBytes(Server.Url, 'http://google.com/', Data);
    Server.Stop;

    Assert.AreEqual(Cardinal(ERROR_SUCCESS), ErrorCode);
    Assert.AreEqual(LOCAL_TEST_BODY, TEncoding.UTF8.GetString(Data));
    Assert.IsTrue(Pos(#13#10'Referer: http://google.com/'#13#10, Server.Requests) > 0, 'The request must carry the referer. Request: ' + Server.Requests);
  finally
    FreeAndNil(Server);
  end;
end;


{ DownloadToFile Tests }

procedure TTestDownloadWinInet.TestDownloadToFile_ValidUrl_CreatesFile;
var
  Server: TLocalHttpServer;
  FilePath: string;
  ErrorCode: Cardinal;
begin
  FilePath:= TPath.Combine(FTestDir, 'download.html');
  Server:= TLocalHttpServer.Create(LOCAL_TEST_BODY);
  try
    ErrorCode:= DownloadToFile(Server.Url('/download.html'), '', FilePath);
    Server.Stop;
  finally
    FreeAndNil(Server);
  end;

  Assert.AreEqual(Cardinal(ERROR_SUCCESS), ErrorCode);
  Assert.IsTrue(FileExists(FilePath), 'File should have been created');
  Assert.AreEqual(LOCAL_TEST_BODY, TFile.ReadAllText(FilePath, TEncoding.UTF8), 'The file must hold the exact body');
end;


procedure TTestDownloadWinInet.TestDownloadToFile_InvalidUrl_NoFileCreated;
var
  FilePath: string;
  ErrorCode: Cardinal;
begin
  FilePath:= TPath.Combine(FTestDir, 'invalid.txt');
  ErrorCode:= DownloadToFile('http://this.domain.does.not.exist.invalid/', '', FilePath);

  { Should return error and not create file }
  Assert.AreNotEqual(Cardinal(ERROR_SUCCESS), ErrorCode);
  Assert.IsFalse(FileExists(FilePath), 'File should not be created on download error');
end;


procedure TTestDownloadWinInet.TestDownloadToFile_InvalidDirectory_RaisesException;
var
  FilePath: string;
begin
  { Use an invalid directory path that cannot be created }
  FilePath:= '\\?\InvalidPath\<>:"/\|?*\file.txt';

  Assert.WillRaise(
    procedure
    begin
      DownloadToFile('http://example.com/', '', FilePath);
    end,
    Exception,
    'Should raise exception for invalid directory');
end;


{ SSL Tests }

{ TLocalHttpServer has no TLS, so this one test still needs the Internet. When the download fails, an
  independent TCP probe (System.Net.Socket, not WinINet) decides: the skip runs only if the probe cannot reach
  example.com:443 either. If the probe connects and the download fails, the download code is broken. }
procedure TTestDownloadWinInet.TestDownloadBytes_HttpsUrl;
var
  Data: TBytes;
  ErrorCode: Cardinal;
  ProbeError: string;
begin
  ErrorCode:= DownloadBytes('https://example.com/', '', Data, '', TRUE);

  if (ErrorCode <> ERROR_SUCCESS) AND NOT CanConnect('example.com', 443, ProbeError) then
    begin
      Assert.AreEqual(0, Length(Data), 'A failed download must return no data');
      Assert.Pass('No network for this EXE. DownloadBytes error ' + IntToStr(ErrorCode) + '; probe: ' + ProbeError);
    end;

  Assert.AreEqual(Cardinal(ERROR_SUCCESS), ErrorCode, 'example.com:443 is reachable, so the HTTPS download must succeed');
  Assert.IsTrue(Pos('Example Domain', TEncoding.UTF8.GetString(Data)) > 0, 'Should download the example.com page over HTTPS');
end;


{ Edge Cases }

procedure TTestDownloadWinInet.TestDownloadBytes_UrlWithQueryParams;
var
  Server: TLocalHttpServer;
  Data: TBytes;
  ErrorCode: Cardinal;
begin
  Server:= TLocalHttpServer.Create(LOCAL_TEST_BODY);
  try
    ErrorCode:= DownloadBytes(Server.Url('/img.php?param=value&other=123'), '', Data);
    Server.Stop;

    Assert.AreEqual(Cardinal(ERROR_SUCCESS), ErrorCode);
    Assert.AreEqual(LOCAL_TEST_BODY, TEncoding.UTF8.GetString(Data));
    Assert.IsTrue(Pos('GET /img.php?param=value&other=123 HTTP/1.1', Server.Requests) = 1, 'The request must keep the query parameters. Request: ' + Server.Requests);
  finally
    FreeAndNil(Server);
  end;
end;


procedure TTestDownloadWinInet.TestDownloadBytes_UrlWithPort;
var
  Server: TLocalHttpServer;
  Data: TBytes;
  ErrorCode: Cardinal;
begin
  { Server.Url always carries an explicit port that is neither 80 nor 443, so the download only reaches the server when DownloadBytes uses the port from the URL }
  Server:= TLocalHttpServer.Create(LOCAL_TEST_BODY);
  try
    ErrorCode:= DownloadBytes(Server.Url, '', Data);
    Server.Stop;
  finally
    FreeAndNil(Server);
  end;

  Assert.AreEqual(Cardinal(ERROR_SUCCESS), ErrorCode);
  Assert.AreEqual(LOCAL_TEST_BODY, TEncoding.UTF8.GetString(Data), 'Should download data from the explicit port');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestDownloadWinInet);

{$ENDIF}

end.
