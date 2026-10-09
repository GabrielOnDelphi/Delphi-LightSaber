unit Test.LightCore.Internet.CommonWebDown;

{=============================================================================================================
   Unit tests for LightCore.Internet.CommonWebDown.pas
   Tests the Unsplash image extraction functionality.

   Note: Full integration tests require network access and valid Unsplash URLs.
   These tests focus on parameter validation and error handling.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  LightCore.IO,
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils;

type
  [TestFixture]
  TTestCommonWebDown = class
  private
    FTempDir: string;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Parameter Validation Tests }
    [Test]
    procedure TestGetUnsplashImage_EmptyURL;

    [Test]
    procedure TestGetUnsplashImage_EmptyLocalFile;

    [Test]
    procedure TestGetUnsplashImage_BothEmpty;

    { Invalid URL Tests }
    [Test]
    procedure TestGetUnsplashImage_InvalidURL_ReturnsFalse;

    [Test]
    procedure TestGetUnsplashImage_NonUnsplashURL_ReturnsFalse;
  end;

implementation

uses
  LightCore.Download,
  LightCore.Internet.CommonWebDown,
  Test.LightCore.Download;


{ How many request heads TLocalHttpServer.Requests holds: each one starts with its request line 'GET /' }
function CountRequests(CONST Requests: string): Integer;
VAR P: Integer;
begin
  Result:= 0;
  P:= Pos('GET /', Requests);
  while P > 0 do
    begin
      Inc(Result);
      P:= Pos('GET /', Requests, P + 1);
    end;
end;


procedure TTestCommonWebDown.Setup;
begin
  FTempDir:= TPath.Combine(TPath.GetTempPath, 'TestCommonWebDown_' + TGUID.NewGuid.ToString);
  ForceDirectoriesE(FTempDir);
end;


procedure TTestCommonWebDown.TearDown;
begin
  if DirectoryExists(FTempDir)
  then TDirectory.Delete(FTempDir, True);
end;


{ Parameter Validation Tests }

procedure TTestCommonWebDown.TestGetUnsplashImage_EmptyURL;
begin
  Assert.WillRaise(
    procedure
    begin
      GetUnsplashImage('', TPath.Combine(FTempDir, 'test.jpg'));
    end,
    EAssertionFailed,
    'Should raise assertion for empty URL');
end;


procedure TTestCommonWebDown.TestGetUnsplashImage_EmptyLocalFile;
begin
  Assert.WillRaise(
    procedure
    begin
      GetUnsplashImage('https://unsplash.com/photos/test', '');
    end,
    EAssertionFailed,
    'Should raise assertion for empty LocalFile');
end;


procedure TTestCommonWebDown.TestGetUnsplashImage_BothEmpty;
begin
  Assert.WillRaise(
    procedure
    begin
      GetUnsplashImage('', '');
    end,
    EAssertionFailed,
    'Should raise assertion when both parameters are empty');
end;


{ Invalid URL Tests }

procedure TTestCommonWebDown.TestGetUnsplashImage_InvalidURL_ReturnsFalse;
var
  Result: Boolean;
begin
  // This test requires network access but will fail gracefully
  // An invalid/unreachable URL should return False without crashing
  Result:= GetUnsplashImage('https://invalid-domain-that-does-not-exist-12345.com/photo',
                            TPath.Combine(FTempDir, 'test.jpg'));

  Assert.IsFalse(Result, 'Should return False for invalid/unreachable URL');
end;


{ A non-Unsplash page, served by TLocalHttpServer on 127.0.0.1: it downloads fine but has no og:image meta tag.
  GetUnsplashImage must stop at the missing tag: return FALSE, write no file and request nothing more. }
procedure TTestCommonWebDown.TestGetUnsplashImage_NonUnsplashURL_ReturnsFalse;
CONST
  PAGE_WITHOUT_IMAGE = '<html><head><meta property="og:title" content="Not an Unsplash page"></head><body>No image</body></html>';
var
  Server: TLocalHttpServer;
  LocalFile, Page, Requests: string;
  Found: Boolean;
begin
  LocalFile:= TPath.Combine(FTempDir, 'test.jpg');
  Server:= TLocalHttpServer.Create(PAGE_WITHOUT_IMAGE);
  try
    { The page must arrive, or GetUnsplashImage would leave through its "empty page" exit instead of the "no tag" exit }
    Page:= DownloadAsString(Server.Url('/precheck'));
    Found:= GetUnsplashImage(Server.Url('/photos/no-image'), LocalFile);
    Server.Stop;
    Requests:= Server.Requests;
  finally
    FreeAndNil(Server);
  end;

  Assert.AreEqual(PAGE_WITHOUT_IMAGE, Page, 'The local server must deliver the page');
  Assert.IsFalse(Found, 'A page without the og:image meta tag must give FALSE');
  Assert.IsFalse(FileExists(LocalFile), 'No file may be written');
  Assert.IsTrue(Pos('GET /photos/no-image HTTP/1.1', Requests) > 0, 'GetUnsplashImage must request the given URL. Requests: ' + Requests);
  Assert.AreEqual(2, CountRequests(Requests), 'The precheck and the page, and no image download. Requests: ' + Requests);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestCommonWebDown);

end.
