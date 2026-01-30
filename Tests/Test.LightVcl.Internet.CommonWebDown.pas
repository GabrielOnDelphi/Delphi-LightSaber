unit Test.LightVcl.Internet.CommonWebDown;

{=============================================================================================================
   Unit tests for LightVcl.Internet.CommonWebDown.pas
   Tests the Unsplash image extraction functionality.

   Note: Full integration tests require network access and valid Unsplash URLs.
   These tests focus on parameter validation and error handling.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
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
  LightVcl.Internet.CommonWebDown;


procedure TTestCommonWebDown.Setup;
begin
  FTempDir:= TPath.Combine(TPath.GetTempPath, 'TestCommonWebDown_' + TGUID.NewGuid.ToString);
  ForceDirectories(FTempDir);
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


procedure TTestCommonWebDown.TestGetUnsplashImage_NonUnsplashURL_ReturnsFalse;
var
  Result: Boolean;
begin
  // A non-Unsplash URL won't have the expected meta tags
  // This may timeout or return False - both are acceptable
  Result:= GetUnsplashImage('https://example.com',
                            TPath.Combine(FTempDir, 'test.jpg'));

  Assert.IsFalse(Result, 'Should return False for non-Unsplash URL (no og:image meta tag)');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestCommonWebDown);

end.
