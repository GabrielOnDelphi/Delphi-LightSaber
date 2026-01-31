unit Test.LightCore.Internet;

{=============================================================================================================
   2026.01.30
   Unit tests for LightCore.Internet
   Tests URL parsing, validation, and manipulation functions

   Requires: TESTINSIGHT compiler directive for TestInsight integration
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
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
    [TestCase('HttpWithPort', 'http://example.com:8080/path,http://example.com/path')]
    [TestCase('HttpsWithPort', 'https://example.com:443/path,https://example.com/path')]
    procedure TestUrlRemovePort(const URL, Expected: string);

    [Test]
    [TestCase('Port80', 'www.example.com:80,80')]
    [TestCase('Port8080', 'http://example.com:8080/path,8080')]
    [TestCase('NoPort', 'www.example.com,0')]
    [TestCase('HttpsPort443', 'https://example.com:443/path,443')]
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
    [TestCase('ValidZeros', '0.0.0.0,True')]
    [TestCase('ValidMax', '255.255.255.255,True')]
    [TestCase('DoubleDot', '192..168.1.1,False')]
    [TestCase('LeadingDot', '.192.168.1.1,False')]
    [TestCase('TrailingDot', '192.168.1.1.,False')]
    [TestCase('TooManyOctets', '192.168.1.1.1,False')]
    [TestCase('Letters', '192.168.1.abc,False')]
    procedure TestValidateIpAddress(const Address: string; Expected: Boolean);

    [Test]
    [TestCase('ValidPort', '80,True')]
    [TestCase('MaxPort', '65535,True')]
    [TestCase('InvalidPort', '70000,False')]
    [TestCase('NegativePort', '-1,False')]
    [TestCase('ZeroPort', '0,True')]
    [TestCase('NonNumeric', 'abc,False')]
    procedure TestValidatePort(const Port: string; Expected: Boolean);

    { URL Encoding }
    [Test]
    procedure TestUrlEncode_Space;

    [Test]
    procedure TestUrlEncode_SpecialChars;

    [Test]
    procedure TestUrlEncode_ExtendedASCII;

    { IsWebPage Tests }
    [Test]
    [TestCase('HTML', 'http://example.com/page.html,True')]
    [TestCase('PHP', 'http://example.com/page.php,True')]
    [TestCase('ASP', 'http://example.com/page.asp,True')]
    [TestCase('Image', 'http://example.com/image.jpg,False')]
    [TestCase('FolderSlash', 'http://example.com/folder/,True')]
    [TestCase('HashOnly', '#,True')]
    [TestCase('DomainOnly', 'http://example.com,True')]
    procedure TestIsWebPage(const URL: string; Expected: Boolean);

    { IP Text Manipulation Tests }
    [Test]
    [TestCase('WithPort', '192.168.1.1:80,192.168.1.1')]
    [TestCase('NoPort', '192.168.1.1,192.168.1.1')]
    [TestCase('EmptyString', ',')]
    procedure TestSplitIpFromAdr(const Address, Expected: string);

    [Test]
    [TestCase('WithPort', '192.168.1.1:8080,8080')]
    [TestCase('NoPort', '192.168.1.1,')]
    procedure TestIpExtractPort(const Address, Expected: string);

    { Extract Proxy Tests }
    [Test]
    [TestCase('CleanProxy', '192.168.1.1:8080,192.168.1.1:8080')]
    [TestCase('GarbageBefore', 'xxx192.168.1.1:8080,192.168.1.1:8080')]
    [TestCase('GarbageAfter', '192.168.1.1:8080xxx,192.168.1.1:8080')]
    [TestCase('GarbageBoth', 'xxx192.168.1.1:8080xxx,192.168.1.1:8080')]
    [TestCase('NoColon', '192.168.1.1,')]
    procedure TestExtractProxyFrom(const Line, Expected: string);

    { Proxy Validation Tests }
    [Test]
    [TestCase('ValidProxy', '192.168.1.1:80,True')]
    [TestCase('InvalidIP', '999.168.1.1:80,False')]
    [TestCase('InvalidPort', '192.168.1.1:70000,False')]
    [TestCase('NoPort', '192.168.1.1,False')]
    procedure TestValidateProxyAdr(const Address: string; Expected: Boolean);

    { URL Folder Extraction Tests }
    [Test]
    [TestCase('WithFile', 'http://server.com/folder1/folder2/file.txt,folder2')]
    [TestCase('TrailingSlash', 'http://server.com/folder1/folder2/,folder2')]
    [TestCase('SingleFolder', 'http://server.com/folder1/,')]
    procedure TestURLExtractLastFolder(const URL, Expected: string);

    [Test]
    [TestCase('WithFile', 'http://server.com/folder1/folder2/file.txt,folder1')]
    [TestCase('TrailingSlash', 'http://server.com/folder1/folder2/,folder1')]
    [TestCase('SingleFolder', 'http://server.com/folder1/,')]
    procedure TestURLExtractPrevFolder(const URL, Expected: string);

    { URL Expansion Tests }
    [Test]
    procedure TestExpandURL_AbsoluteURL;

    [Test]
    procedure TestExpandURL_RootRelative;

    [Test]
    procedure TestExpandURL_PathRelative;

    [Test]
    procedure TestExpandURLs_ModifiesList;

    { Protocol-Relative URL Tests }
    [Test]
    [TestCase('ProtocolRelative', '//example.com/path,http://example.com/path')]
    [TestCase('AlreadyAbsolute', 'http://example.com/path,http://example.com/path')]
    procedure TestURLMakeNonRelativeProtocol(const URL, Expected: string);

    { FileIsInFolder Tests }
    [Test]
    procedure TestFileIsInFolder_InFolder;

    [Test]
    procedure TestFileIsInFolder_NotInFolder;

    { HTTP Status Code Tests }
    [Test]
    [TestCase('OK', '200,OK')]
    [TestCase('NotFound', '404,Not Found')]
    [TestCase('ServerError', '500,Internal Server Error')]
    [TestCase('Teapot', '418,I''m a teapot')]
    [TestCase('Unknown', '999,999')]
    procedure TestServerStatus2String(Status: Integer; const Expected: string);

    { URL Path Extraction Tests }
    [Test]
    [TestCase('WithFile', 'http://example.com/folder/file.jpg,http://example.com/folder/')]
    [TestCase('NoFile', 'http://example.com/folder/,http://example.com/folder/')]
    procedure TestUrlExtractFilePath(const URL, Expected: string);
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
  { % character should be encoded }
  Assert.IsTrue(Pos('%25', UrlEncode('100%')) > 0, 'Percent should be encoded');
end;

procedure TTestInternet.TestUrlEncode_ExtendedASCII;
VAR
  Encoded: string;
  TestChar: Char;
begin
  { Extended ASCII characters (>= 127) should be encoded }
  TestChar:= Char(128);
  Encoded:= UrlEncode('test' + TestChar);
  Assert.IsTrue(Pos('%80', Encoded) > 0, 'Extended ASCII should be encoded. Got: ' + Encoded);

  { DEL character (#127) should be encoded }
  Encoded:= UrlEncode('test'+ #127);
  Assert.IsTrue(Pos('%7F', Encoded) > 0, 'DEL character should be encoded. Got: ' + Encoded);
end;

procedure TTestInternet.TestIsWebPage(const URL: string; Expected: Boolean);
begin
  Assert.AreEqual(Expected, IsWebPage(URL));
end;

procedure TTestInternet.TestSplitIpFromAdr(const Address, Expected: string);
begin
  Assert.AreEqual(Expected, SplitIpFromAdr(Address));
end;

procedure TTestInternet.TestIpExtractPort(const Address, Expected: string);
begin
  Assert.AreEqual(Expected, IpExtractPort(Address));
end;

procedure TTestInternet.TestExtractProxyFrom(const Line, Expected: string);
begin
  Assert.AreEqual(Expected, ExtractProxyFrom(Line));
end;

procedure TTestInternet.TestValidateProxyAdr(const Address: string; Expected: Boolean);
begin
  Assert.AreEqual(Expected, ValidateProxyAdr(Address));
end;

procedure TTestInternet.TestURLExtractLastFolder(const URL, Expected: string);
begin
  Assert.AreEqual(Expected, URLExtractLastFolder(URL));
end;

procedure TTestInternet.TestURLExtractPrevFolder(const URL, Expected: string);
begin
  Assert.AreEqual(Expected, URLExtractPrevFolder(URL));
end;

procedure TTestInternet.TestExpandURL_AbsoluteURL;
begin
  { Already absolute URLs should be returned as-is }
  Assert.AreEqual('http://other.com/page', ExpandURL('http://other.com/page', 'http://example.com/folder/'));
  Assert.AreEqual('https://secure.com/path', ExpandURL('https://secure.com/path', 'http://example.com/'));
end;

procedure TTestInternet.TestExpandURL_RootRelative;
begin
  { URLs starting with / should be appended to domain only }
  Assert.AreEqual('http://example.com/images/logo.png', ExpandURL('/images/logo.png', 'http://example.com/folder/page.html'));
end;

procedure TTestInternet.TestExpandURL_PathRelative;
begin
  { Relative URLs should be appended to the base path }
  Assert.AreEqual('http://example.com/download/', ExpandURL('download/', 'http://example.com/'));
end;

procedure TTestInternet.TestExpandURLs_ModifiesList;
VAR
  URLs: TStringList;
begin
  URLs:= TStringList.Create;
  TRY
    URLs.Add('/image1.jpg');
    URLs.Add('/image2.jpg');
    ExpandURLs(URLs, 'http://example.com/folder/');

    Assert.AreEqual('http://example.com/image1.jpg', URLs[0]);
    Assert.AreEqual('http://example.com/image2.jpg', URLs[1]);
  FINALLY
    URLs.Free;
  END;
end;

procedure TTestInternet.TestURLMakeNonRelativeProtocol(const URL, Expected: string);
begin
  Assert.AreEqual(Expected, URLMakeNonRelativeProtocol(URL));
end;

procedure TTestInternet.TestFileIsInFolder_InFolder;
begin
  Assert.IsTrue(FileIsInFolder('http://test.com/images/', 'http://test.com/images/photo.jpg'));
  Assert.IsTrue(FileIsInFolder('http://test.com/images/', 'http://test.com/images/sub/photo.jpg'));
end;

procedure TTestInternet.TestFileIsInFolder_NotInFolder;
begin
  Assert.IsFalse(FileIsInFolder('http://test.com/images/', 'http://test.com/art/photo.jpg'));
  Assert.IsFalse(FileIsInFolder('http://test.com/images/', 'http://test.com/photo.jpg'));
end;

procedure TTestInternet.TestServerStatus2String(Status: Integer; const Expected: string);
begin
  Assert.AreEqual(Expected, ServerStatus2String(Status));
end;

procedure TTestInternet.TestUrlExtractFilePath(const URL, Expected: string);
begin
  Assert.AreEqual(Expected, UrlExtractFilePath(URL));
end;

initialization
  TDUnitX.RegisterTestFixture(TTestInternet);

end.
