unit Test.LightCore.Internet;

{=============================================================================================================
   Unit tests for LightCore.Internet
   Tests URL parsing, validation, and manipulation functions

   Requires: TESTINSIGHT compiler directive for TestInsight integration
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  LightCore.Internet;

type
  [TestFixture]
  TTestInternet = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { URL Validation Tests }
    [Test]
    [TestCase('HTTP', 'http://example.com,True')]
    [TestCase('HTTPS', 'https://example.com,True')]
    [TestCase('WWW', 'www.example.com,True')]
    [TestCase('NoProtocol', 'example.com,False')]
    [TestCase('Empty', ',False')]
    procedure TestCheckURLStart(const URL: string; Expected: Boolean);

    [Test]
    [TestCase('ValidChars', 'http://example.com/path,True')]
    [TestCase('InvalidSpace', 'http://example.com/path name,False')]
    [TestCase('InvalidLT', 'http://example.com/<path>,False')]
    procedure TestValidUrlChars(const URL: string; Expected: Boolean);

    { URL Extraction Tests }
    [Test]
    [TestCase('HTTP', 'http://www.example.com/path/file.jpg,example.com')]
    [TestCase('HTTPS', 'https://www.example.com/path,example.com')]
    [TestCase('Subdomain', 'http://sub.example.com/path,example.com')]
    procedure TestUrlExtractDomain(const URL, Expected: string);

    [Test]
    [TestCase('Simple', 'http://www.example.com/path/file.jpg,http://www.example.com')]
    [TestCase('WithPort', 'http://www.example.com:8080/path,http://www.example.com:8080')]
    procedure TestUrlExtractProtAndDomain(const URL, Expected: string);

    [Test]
    [TestCase('SimpleFile', 'http://example.com/path/file.jpg,file.jpg')]
    [TestCase('WithQuery', 'http://example.com/path/file.jpg?query=1,file.jpg')]
    [TestCase('NoPath', 'http://example.com,example.com')]
    procedure TestUrlExtractFileName(const URL, Expected: string);

    [Test]
    [TestCase('WithPath', 'http://example.com/folder/subfolder/file.jpg,/folder/subfolder/file.jpg')]
    [TestCase('RootOnly', 'http://example.com/file.jpg,/file.jpg')]
    procedure TestUrlExtractResource(const URL, Expected: string);

    { URL Modification Tests }
    [Test]
    [TestCase('AddHttp', 'www.example.com,http://www.example.com')]
    [TestCase('AlreadyHttp', 'http://example.com,http://example.com')]
    [TestCase('AlreadyHttps', 'https://example.com,https://example.com')]
    procedure TestUrlForceHttp(const URL, Expected: string);

    [Test]
    [TestCase('RemoveWWW', 'http://www.example.com/path,example.com/path')]
    [TestCase('RemoveHTTP', 'http://example.com/path,example.com/path')]
    procedure TestUrlRemoveStart(const URL, Expected: string);

    [Test]
    [TestCase('WithPort80', 'www.example.com:80,www.example.com')]
    [TestCase('WithPort8080', 'www.example.com:8080,www.example.com')]
    [TestCase('NoPort', 'www.example.com,www.example.com')]
    procedure TestUrlRemovePort(const URL, Expected: string);

    [Test]
    [TestCase('Port80', 'www.example.com:80,80')]
    [TestCase('Port8080', 'http://example.com:8080/path,8080')]
    [TestCase('NoPort', 'www.example.com,0')]
    procedure TestUrlExtractPort(const URL: string; Expected: Integer);

    { URL Comparison Tests }
    [Test]
    procedure TestSameWebSite_SameSite;

    [Test]
    procedure TestSameWebSite_DifferentSites;

    { Server Commands Tests }
    [Test]
    [TestCase('WithQuery', 'http://example.com/file.jpg?w=100,http://example.com/file.jpg')]
    [TestCase('NoQuery', 'http://example.com/file.jpg,http://example.com/file.jpg')]
    procedure TestCleanServerCommands(const URL, Expected: string);

    { IP Validation Tests }
    [Test]
    [TestCase('ValidIP', '192.168.1.1,True')]
    [TestCase('InvalidIP', '192.168.1.256,False')]
    [TestCase('TooShort', '192.168.1,False')]
    [TestCase('Empty', ',False')]
    procedure TestValidateIpAddress(const Address: string; Expected: Boolean);

    [Test]
    [TestCase('ValidPort', '80,True')]
    [TestCase('MaxPort', '65535,True')]
    [TestCase('InvalidPort', '70000,False')]
    [TestCase('NegativePort', '-1,False')]
    procedure TestValidatePort(const Port: string; Expected: Boolean);

    { URL Encoding }
    [Test]
    procedure TestUrlEncode_Space;

    [Test]
    procedure TestUrlEncode_SpecialChars;

    { IsWebPage Tests }
    [Test]
    [TestCase('HTML', 'http://example.com/page.html,True')]
    [TestCase('PHP', 'http://example.com/page.php,True')]
    [TestCase('ASP', 'http://example.com/page.asp,True')]
    [TestCase('Image', 'http://example.com/image.jpg,False')]
    [TestCase('FolderSlash', 'http://example.com/folder/,True')]
    procedure TestIsWebPage(const URL: string; Expected: Boolean);
  end;

implementation

procedure TTestInternet.Setup;
begin
  { No setup needed }
end;

procedure TTestInternet.TearDown;
begin
  { No teardown needed }
end;

procedure TTestInternet.TestCheckURLStart(const URL: string; Expected: Boolean);
begin
  Assert.AreEqual(Expected, CheckURLStart(URL));
end;

procedure TTestInternet.TestValidUrlChars(const URL: string; Expected: Boolean);
begin
  Assert.AreEqual(Expected, ValidUrlChars(URL));
end;

procedure TTestInternet.TestUrlExtractDomain(const URL, Expected: string);
begin
  Assert.AreEqual(Expected, UrlExtractDomain(URL));
end;

procedure TTestInternet.TestUrlExtractProtAndDomain(const URL, Expected: string);
begin
  Assert.AreEqual(Expected, UrlExtractProtAndDomain(URL));
end;

procedure TTestInternet.TestUrlExtractFileName(const URL, Expected: string);
begin
  Assert.AreEqual(Expected, UrlExtractFileName(URL));
end;

procedure TTestInternet.TestUrlExtractResource(const URL, Expected: string);
begin
  Assert.AreEqual(Expected, UrlExtractResource(URL));
end;

procedure TTestInternet.TestUrlForceHttp(const URL, Expected: string);
begin
  Assert.AreEqual(Expected, UrlForceHttp(URL));
end;

procedure TTestInternet.TestUrlRemoveStart(const URL, Expected: string);
begin
  Assert.AreEqual(Expected, UrlRemoveStart(URL));
end;

procedure TTestInternet.TestUrlRemovePort(const URL, Expected: string);
begin
  Assert.AreEqual(Expected, UrlRemovePort(URL));
end;

procedure TTestInternet.TestUrlExtractPort(const URL: string; Expected: Integer);
begin
  Assert.AreEqual(Expected, UrlExtractPort(URL));
end;

procedure TTestInternet.TestSameWebSite_SameSite;
begin
  Assert.IsTrue(SameWebSite('http://www.example.com/path1', 'http://www.example.com/path2'));
  Assert.IsTrue(SameWebSite('https://example.com', 'http://www.example.com'));
end;

procedure TTestInternet.TestSameWebSite_DifferentSites;
begin
  Assert.IsFalse(SameWebSite('http://example.com', 'http://other.com'));
  Assert.IsFalse(SameWebSite('http://example.com', 'http://example.org'));
end;

procedure TTestInternet.TestCleanServerCommands(const URL, Expected: string);
begin
  Assert.AreEqual(Expected, CleanServerCommands(URL));
end;

procedure TTestInternet.TestValidateIpAddress(const Address: string; Expected: Boolean);
begin
  Assert.AreEqual(Expected, ValidateIpAddress(Address));
end;

procedure TTestInternet.TestValidatePort(const Port: string; Expected: Boolean);
begin
  Assert.AreEqual(Expected, ValidatePort(Port));
end;

procedure TTestInternet.TestUrlEncode_Space;
begin
  Assert.AreEqual('hello%20world', UrlEncode('hello world'));
end;

procedure TTestInternet.TestUrlEncode_SpecialChars;
begin
  { # character should be encoded }
  Assert.IsTrue(Pos('%23', UrlEncode('test#value')) > 0, 'Hash should be encoded');
end;

procedure TTestInternet.TestIsWebPage(const URL: string; Expected: Boolean);
begin
  Assert.AreEqual(Expected, IsWebPage(URL));
end;

initialization
  TDUnitX.RegisterTestFixture(TTestInternet);

end.
