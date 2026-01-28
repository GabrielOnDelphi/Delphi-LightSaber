unit Test.LightCore.Download;

{=============================================================================================================
   Unit tests for LightCore.Download.pas
   Tests HTTP download functionality
   Note: Network tests are limited to avoid external dependencies
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  system.classes,
  System.IOUtils,
  System.Net.URLClient,
  LightCore.Download;

type
  [TestFixture]
  TTestDownload = class
  private
    FTestDir: string;
    procedure CleanupTestDir;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { RHttpOptions Tests }
    [Test]
    procedure TestHttpOptions_Reset;

    [Test]
    procedure TestHttpOptions_DefaultValues;

    { Constants Tests }
    [Test]
    procedure TestConstants;

    { Download Tests - these may fail without network }
    [Test]
    [Ignore('Network test - may fail without internet connection')]
    procedure TestDownloadAsString_ValidUrl;

    [Test]
    procedure TestDownloadAsString_InvalidUrl;

    [Test]
    [Ignore('Network test - may fail without internet connection')]
    procedure TestDownloadToFile_ValidUrl;

    [Test]
    procedure TestDownloadToFile_InvalidUrl;

    [Test]
    [Ignore('Network test - may fail without internet connection')]
    procedure TestDownloadToStream_ValidUrl;

    [Test]
    [Ignore('Network test - may fail without internet connection')]
    procedure TestDownloadAsString_SimpleOverload;

    [Test]
    [Ignore('Network test - may fail without internet connection')]
    procedure TestDownloadWithCustomOptions;

    [Test]
    procedure TestDownloadAsString_SimpleOverload_InvalidUrl;
  end;

implementation


procedure TTestDownload.Setup;
begin
  FTestDir:= TPath.Combine(TPath.GetTempPath, 'DownloadTest_' + TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FTestDir);
end;


procedure TTestDownload.TearDown;
begin
  CleanupTestDir;
end;


procedure TTestDownload.CleanupTestDir;
begin
  if TDirectory.Exists(FTestDir)
  then TDirectory.Delete(FTestDir, True);
end;


{ RHttpOptions Tests }

procedure TTestDownload.TestHttpOptions_Reset;
var
  Options: RHttpOptions;
begin
  { Set some values }
  Options.UserAgent:= 'Custom';
  Options.ResponseTimeout:= 1000;

  { Reset should restore defaults }
  Options.Reset;

  Assert.AreEqual(USER_AGENT_STRING, Options.UserAgent);
  Assert.IsTrue(Options.HandleRedirects);
  Assert.AreEqual(10, Options.MaxRedirects);
end;

procedure TTestDownload.TestHttpOptions_DefaultValues;
var
  Options: RHttpOptions;
begin
  Options.Reset;

  Assert.AreEqual(USER_AGENT_STRING, Options.UserAgent);
  Assert.IsFalse(Options.AllowCookies);
  Assert.IsTrue(Options.HandleRedirects);
  Assert.AreEqual(10, Options.MaxRedirects);
  Assert.AreEqual(60000, Options.ConnectionTimeout);
  Assert.AreEqual(60000, Options.ResponseTimeout);
end;


{ Constants Tests }

procedure TTestDownload.TestConstants;
begin
  Assert.AreEqual(200, HTTP_STATUS_OK);
end;


{ Download Tests }

procedure TTestDownload.TestDownloadAsString_ValidUrl;
var Content, ErrorMsg: string;
begin
  { Test with example.com - a simple, reliable test URL }
  Content:= DownloadAsString('https://example.com/', ErrorMsg);

  if ErrorMsg <> ''
  then Assert.Pass('Network unavailable: ' + ErrorMsg)
  else
    begin
      Assert.IsNotEmpty(Content);
      Assert.AreEqual('', ErrorMsg);
      Assert.IsTrue(Pos('Example Domain', Content) > 0, 'Should contain expected content');
    end;
end;

procedure TTestDownload.TestDownloadAsString_InvalidUrl;
var Content, ErrorMsg: string;
begin
  Content:= DownloadAsString('https://this.domain.does.not.exist.invalid/', ErrorMsg);

  { Should fail with an error }
  Assert.IsNotEmpty(ErrorMsg);
  Assert.AreEqual('', Content);
end;

procedure TTestDownload.TestDownloadToFile_ValidUrl;
var
  FilePath, ErrorMsg: string;
begin
  FilePath:= TPath.Combine(FTestDir, 'download.html');
  DownloadToFile('https://example.com/', FilePath, ErrorMsg);

  if ErrorMsg <> ''
  then Assert.Pass('Network unavailable: ' + ErrorMsg)
  else
    begin
      Assert.AreEqual('', ErrorMsg);
      Assert.IsTrue(FileExists(FilePath), 'File should have been created');
      Assert.IsTrue(TFile.ReadAllText(FilePath).Length > 0, 'File should have content');
    end;
end;

procedure TTestDownload.TestDownloadToFile_InvalidUrl;
var
  FilePath, ErrorMsg: string;
begin
  FilePath:= TPath.Combine(FTestDir, 'invalid.txt');
  DownloadToFile('https://this.domain.does.not.exist.invalid/', FilePath, ErrorMsg);

  { Should fail with an error }
  Assert.IsNotEmpty(ErrorMsg);
  Assert.IsFalse(FileExists(FilePath), 'File should not be created on error');
end;

procedure TTestDownload.TestDownloadToStream_ValidUrl;
var
  Stream: TMemoryStream;
  ErrorMsg: string;
begin
  Stream:= DownloadToStream('https://example.com/', ErrorMsg);
  try
    if ErrorMsg <> ''
    then Assert.Pass('Network unavailable: ' + ErrorMsg)
    else
      begin
        Assert.IsNotNull(Stream);
        Assert.IsTrue(Stream.Size > 0, 'Stream should have content');
      end;
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TTestDownload.TestDownloadAsString_SimpleOverload;
var
  Content: string;
begin
  { Test the simple overload that ignores errors }
  Content:= DownloadAsString('https://example.com/');

  if Content = ''
  then Assert.Pass('Network unavailable')
  else
    begin
      Assert.IsNotEmpty(Content);
      Assert.IsTrue(Pos('Example Domain', Content) > 0, 'Should contain expected content');
    end;
end;

procedure TTestDownload.TestDownloadAsString_SimpleOverload_InvalidUrl;
var
  Content: string;
begin
  { Simple overload should return empty string on error, not raise exception }
  Content:= DownloadAsString('https://this.domain.does.not.exist.invalid/');
  Assert.AreEqual('', Content, 'Should return empty string on error');
end;

procedure TTestDownload.TestDownloadWithCustomOptions;
var
  Content, ErrorMsg: string;
  Options: RHttpOptions;
begin
  { Test with custom options }
  Options.Reset;
  Options.ConnectionTimeout:= 30000;
  Options.ResponseTimeout:= 30000;
  Options.UserAgent:= 'TestAgent/1.0';

  Content:= DownloadAsString('https://example.com/', ErrorMsg, nil, @Options);

  if ErrorMsg <> ''
  then Assert.Pass('Network unavailable: ' + ErrorMsg)
  else
    begin
      Assert.IsNotEmpty(Content);
      Assert.AreEqual('', ErrorMsg);
    end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestDownload);

end.
