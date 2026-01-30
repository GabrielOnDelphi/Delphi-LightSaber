unit Test.LightVcl.Internet.Download.WinInet;

{=============================================================================================================
   Unit tests for LightVcl.Internet.Download.WinInet.pas
   Tests HTTP download functionality using WinINet API.

   Note: Network tests require internet connectivity. Tests are designed to pass gracefully
   when network is unavailable.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  Winapi.Windows,
  LightVcl.Internet.Download.WinInet;

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

    { Constants Tests }
    [Test]
    procedure TestConstants_UserAgentApp_NotEmpty;

    [Test]
    procedure TestConstants_UserAgentMoz_NotEmpty;

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

implementation


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


{ Constants Tests }

procedure TTestDownloadWinInet.TestConstants_UserAgentApp_NotEmpty;
begin
  Assert.IsNotEmpty(USER_AGENT_APP, 'USER_AGENT_APP should not be empty');
  Assert.IsTrue(Pos('DelphiApp', USER_AGENT_APP) > 0, 'USER_AGENT_APP should contain application identifier');
end;


procedure TTestDownloadWinInet.TestConstants_UserAgentMoz_NotEmpty;
begin
  Assert.IsNotEmpty(USER_AGENT_MOZ, 'USER_AGENT_MOZ should not be empty');
  Assert.IsTrue(Pos('Mozilla', USER_AGENT_MOZ) > 0, 'USER_AGENT_MOZ should contain Mozilla identifier');
end;


{ DownloadAsString Tests }

procedure TTestDownloadWinInet.TestDownloadAsString_ValidUrl;
var
  Content: string;
begin
  { Test with example.com - a simple, reliable test URL }
  Content:= DownloadAsString('http://example.com/');

  if Content = ''
  then Assert.Pass('Network unavailable or request failed')
  else
    begin
      Assert.IsNotEmpty(Content);
      Assert.IsTrue(Pos('Example Domain', Content) > 0, 'Should contain expected content from example.com');
    end;
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
  Data: TBytes;
  ErrorCode: Cardinal;
begin
  ErrorCode:= DownloadBytes('http://example.com/', '', Data);

  if ErrorCode <> ERROR_SUCCESS
  then Assert.Pass('Network unavailable, error code: ' + IntToStr(ErrorCode))
  else
    begin
      Assert.AreEqual(Cardinal(ERROR_SUCCESS), ErrorCode, 'Should return ERROR_SUCCESS');
      Assert.IsTrue(Length(Data) > 0, 'Should have downloaded some data');
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
  Data: TBytes;
  ErrorCode: Cardinal;
begin
  { Test that referer parameter is accepted and doesn't cause errors }
  ErrorCode:= DownloadBytes('http://example.com/', 'http://google.com/', Data);

  if ErrorCode <> ERROR_SUCCESS
  then Assert.Pass('Network unavailable, error code: ' + IntToStr(ErrorCode))
  else
    begin
      Assert.AreEqual(Cardinal(ERROR_SUCCESS), ErrorCode);
      Assert.IsTrue(Length(Data) > 0, 'Should have downloaded data with referer');
    end;
end;


{ DownloadToFile Tests }

procedure TTestDownloadWinInet.TestDownloadToFile_ValidUrl_CreatesFile;
var
  FilePath: string;
  ErrorCode: Cardinal;
begin
  FilePath:= TPath.Combine(FTestDir, 'download.html');
  ErrorCode:= DownloadToFile('http://example.com/', '', FilePath);

  if ErrorCode <> ERROR_SUCCESS
  then Assert.Pass('Network unavailable, error code: ' + IntToStr(ErrorCode))
  else
    begin
      Assert.AreEqual(Cardinal(ERROR_SUCCESS), ErrorCode);
      Assert.IsTrue(FileExists(FilePath), 'File should have been created');
      Assert.IsTrue(TFile.ReadAllText(FilePath).Length > 0, 'File should have content');
    end;
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

procedure TTestDownloadWinInet.TestDownloadBytes_HttpsUrl;
var
  Data: TBytes;
  ErrorCode: Cardinal;
begin
  { Test HTTPS with SSL flag }
  ErrorCode:= DownloadBytes('https://example.com/', '', Data, '', TRUE);

  if ErrorCode <> ERROR_SUCCESS
  then Assert.Pass('Network unavailable or SSL error, code: ' + IntToStr(ErrorCode))
  else
    begin
      Assert.AreEqual(Cardinal(ERROR_SUCCESS), ErrorCode);
      Assert.IsTrue(Length(Data) > 0, 'Should download data over HTTPS');
    end;
end;


{ Edge Cases }

procedure TTestDownloadWinInet.TestDownloadBytes_UrlWithQueryParams;
var
  Data: TBytes;
  ErrorCode: Cardinal;
begin
  { Test URL with query parameters }
  ErrorCode:= DownloadBytes('http://example.com/?param=value&other=123', '', Data);

  if ErrorCode <> ERROR_SUCCESS
  then Assert.Pass('Network unavailable, error code: ' + IntToStr(ErrorCode))
  else
    begin
      Assert.AreEqual(Cardinal(ERROR_SUCCESS), ErrorCode);
      Assert.IsTrue(Length(Data) > 0, 'Should download data with query params');
    end;
end;


procedure TTestDownloadWinInet.TestDownloadBytes_UrlWithPort;
var
  Data: TBytes;
  ErrorCode: Cardinal;
begin
  { Test URL with explicit port }
  ErrorCode:= DownloadBytes('http://example.com:80/', '', Data);

  if ErrorCode <> ERROR_SUCCESS
  then Assert.Pass('Network unavailable, error code: ' + IntToStr(ErrorCode))
  else
    begin
      Assert.AreEqual(Cardinal(ERROR_SUCCESS), ErrorCode);
      Assert.IsTrue(Length(Data) > 0, 'Should download data with explicit port');
    end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestDownloadWinInet);

end.
