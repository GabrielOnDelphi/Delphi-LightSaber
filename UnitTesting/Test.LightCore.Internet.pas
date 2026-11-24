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
  LightCore.Types,
  LightCore.Internet;

type
  {$IFDEF MSWINDOWS}
  { Tests of the routines that have a Windows body only (WinInet, WinSock) }
  [TestFixture]
  TTestInternetWin = class
  public
    { ParseURL Tests }
    [Test]
    procedure TestParseURL_FullURL;

    [Test]
    procedure TestParseURL_SimpleURL;

    [Test]
    procedure TestParseURL_URLWithPort;

    [Test]
    procedure TestParseURL_EmptyURL;

    [Test]
    procedure TestParseURL_HTTPSUrl;

    [Test]
    procedure TestParseURL_URLWithQueryString;

    [Test]
    procedure TestParseURL_URLWithUserPassword;

    { GetLocalIP Tests - Network dependent }
    [Test]
    [Category('Network')]
    procedure TestGetLocalIP_ReturnsNonEmpty;

    [Test]
    [Category('Network')]
    procedure TestGetLocalIP_Overload_Success;

    [Test]
    [Category('Network')]
    procedure TestGetLocalIPv4List_EveryEntryIsDottedQuad;

    { ResolveAddress must start WinSock itself. 'localhost' needs no network. }
    [Test]
    procedure TestResolveAddress_Localhost_WithoutWSAStartup;

    { A dotted address whose last octet is 128 or more is a negative LongInt: it must not raise ERangeError }
    [Test]
    procedure TestResolveAddress_DottedAddressHighOctet;
  end;
  {$ENDIF}

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


  { Regression tests for the bugs that the review runs of the effort test found.
    The list: C:\Projects\LightSaber\ToDo\Effort test - real bugs, live check.md.
    Every expected value is traced by hand from what the URL means (RFC 3986), never from the code under test. }
  [TestFixture]
  TTestInternetRegression = class
  public
    [Test]
    procedure UrlExtractProtAndDomain_EmbeddedHttp;     { X2a }
    [Test]
    procedure UrlExtractProtAndDomain_QueryNoPath;      { X2t }
    [Test]
    procedure UrlPort_ColonInQuery;                     { X2b }
    [Test]
    procedure ExpandURL_PathRelativeToFolder;           { X2d }
    [Test]
    procedure ExpandURL_SpecialForms;                   { X2d2 }
    [Test]
    procedure UrlRemoveStart_UpperCase;                 { X2e }
    [Test]
    procedure UrlExtractFileName_ColonInQuery;          { X2f }
    [Test]
    procedure UrlExtractFilePath_HostOnly;              { X2g }
    [Test]
    procedure ExtractProxyFrom_RejectsNonIp;            { X2h }
    [Test]
    procedure UrlExtractDomain_IpHost;                  { X2j2 }
    [Test]
    procedure UrlEncode_UnsafeAscii;                    { X2l }
    [Test]
    procedure ValidatePort_DecimalOnly;                 { X2m }
    [Test]
    procedure GenerateLocalFilenameFromURL_InvalidChars;{ X2o }
    [Test]
    procedure FileIsInFolder_NoTrailingSlash;           { X2w }
    [Test]
    procedure IsWebPage_QueryAndDoubleExtension;        { X2x }
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



{$IFDEF MSWINDOWS}
{ ParseURL Tests }

procedure TTestInternetWin.TestParseURL_FullURL;
VAR
  Parts: TStringArray;
begin
  Parts:= ParseURL('http://user:pass@www.example.com/path/file.html?query=value');

  Assert.AreEqual(6, Length(Parts), 'Should return 6 parts');
  Assert.AreEqual('http', Parts[0], 'Scheme should be http');
  Assert.AreEqual('www.example.com', Parts[1], 'Host should be www.example.com');
  Assert.AreEqual('user', Parts[2], 'User should be user');
  Assert.AreEqual('pass', Parts[3], 'Password should be pass');
  Assert.AreEqual('/path/file.html', Parts[4], 'Path should be /path/file.html');
  Assert.AreEqual('?query=value', Parts[5], 'ExtraInfo should be ?query=value');
end;


procedure TTestInternetWin.TestParseURL_SimpleURL;
VAR
  Parts: TStringArray;
begin
  Parts:= ParseURL('http://www.example.com/');

  Assert.AreEqual(6, Length(Parts), 'Should return 6 parts');
  Assert.AreEqual('http', Parts[0], 'Scheme should be http');
  Assert.AreEqual('www.example.com', Parts[1], 'Host should be www.example.com');
  Assert.AreEqual('', Parts[2], 'User should be empty');
  Assert.AreEqual('', Parts[3], 'Password should be empty');
  Assert.AreEqual('/', Parts[4], 'Path should be /');
  Assert.AreEqual('', Parts[5], 'ExtraInfo should be empty');
end;


procedure TTestInternetWin.TestParseURL_URLWithPort;
VAR
  Parts: TStringArray;
begin
  Parts:= ParseURL('http://www.example.com:8080/page.html');

  Assert.AreEqual('http', Parts[0], 'Scheme should be http');
  Assert.AreEqual('www.example.com', Parts[1], 'Host should be www.example.com (port extracted separately)');
  Assert.AreEqual('/page.html', Parts[4], 'Path should be /page.html');
end;


procedure TTestInternetWin.TestParseURL_EmptyURL;
VAR
  Parts: TStringArray;
begin
  Parts:= ParseURL('');

  Assert.AreEqual(6, Length(Parts), 'Should return 6 parts even for empty URL');
  Assert.AreEqual('', Parts[0], 'All parts should be empty for empty URL');
  Assert.AreEqual('', Parts[1], 'All parts should be empty for empty URL');
end;


procedure TTestInternetWin.TestParseURL_HTTPSUrl;
VAR
  Parts: TStringArray;
begin
  Parts:= ParseURL('https://secure.example.com/login');

  Assert.AreEqual('https', Parts[0], 'Scheme should be https');
  Assert.AreEqual('secure.example.com', Parts[1], 'Host should be secure.example.com');
  Assert.AreEqual('/login', Parts[4], 'Path should be /login');
end;


procedure TTestInternetWin.TestParseURL_URLWithQueryString;
VAR
  Parts: TStringArray;
begin
  Parts:= ParseURL('http://api.example.com/search?q=test&page=1&sort=date');

  Assert.AreEqual('http', Parts[0], 'Scheme should be http');
  Assert.AreEqual('api.example.com', Parts[1], 'Host should be api.example.com');
  Assert.AreEqual('/search', Parts[4], 'Path should be /search');
  Assert.IsTrue(Parts[5].Contains('q=test'), 'ExtraInfo should contain query string');
end;


procedure TTestInternetWin.TestParseURL_URLWithUserPassword;
VAR
  Parts: TStringArray;
begin
  Parts:= ParseURL('ftp://admin:secret123@ftp.example.com/files/');

  Assert.AreEqual('ftp', Parts[0], 'Scheme should be ftp');
  Assert.AreEqual('ftp.example.com', Parts[1], 'Host should be ftp.example.com');
  Assert.AreEqual('admin', Parts[2], 'User should be admin');
  Assert.AreEqual('secret123', Parts[3], 'Password should be secret123');
end;


{ GetLocalIP Tests }

procedure TTestInternetWin.TestGetLocalIP_ReturnsNonEmpty;
VAR
  IP: string;
begin
  IP:= GetLocalIP;
  { IP could be an actual address or an error message }
  Assert.IsNotEmpty(IP, 'GetLocalIP should return something (IP or error)');
end;


procedure TTestInternetWin.TestGetLocalIP_Overload_Success;
VAR
  HostName, IpAddress, ErrorMsg: string;
  Success: Boolean;
begin
  Success:= GetLocalIP(HostName, IpAddress, ErrorMsg);

  if Success then
    begin
      Assert.IsNotEmpty(HostName, 'HostName should not be empty on success');
      Assert.IsNotEmpty(IpAddress, 'IpAddress should not be empty on success');
      Assert.IsEmpty(ErrorMsg, 'ErrorMsg should be empty on success');
      { Verify IP format (basic check for dots) }
      Assert.IsTrue(IpAddress.Contains('.'), 'IP address should contain dots');
    end
  else
    begin
      Assert.IsNotEmpty(ErrorMsg, 'ErrorMsg should contain reason for failure');
    end;
end;


procedure TTestInternetWin.TestGetLocalIPv4List_EveryEntryIsDottedQuad;
VAR
  Addresses: TStringArray;
  Octets: TArray<string>;
  ErrorMsg, HostName, FirstIP, FirstError: string;
  Octet: Integer;
begin
  if NOT GetLocalIPv4List(Addresses, ErrorMsg) then
    begin
      Assert.IsNotEmpty(ErrorMsg, 'ErrorMsg should contain reason for failure');
      Assert.AreEqual(0, Integer(Length(Addresses)), 'No addresses on failure');
      EXIT;
    end;

  Assert.IsEmpty(ErrorMsg, 'ErrorMsg should be empty on success');
  Assert.IsTrue(Length(Addresses) > 0, 'A resolved host name has at least one address');
  for VAR Address in Addresses do
    begin
      Octets:= Address.Split(['.']);
      Assert.AreEqual(4, Integer(Length(Octets)), 'Not a dotted quad: '+ Address);
      for VAR S in Octets do
        begin
          Assert.IsTrue(TryStrToInt(S, Octet), 'Not a number: '+ Address);
          Assert.IsTrue((Octet >= 0) AND (Octet <= 255), 'Octet out of range: '+ Address);
        end;
    end;

  { GetLocalIP reports only the first address of the same list }
  if GetLocalIP(HostName, FirstIP, FirstError)
  then Assert.AreEqual(FirstIP, Addresses[0]);
end;


{ A host name must resolve, also for a caller that never called WSAStartup. The test calls no WinSock routine itself.
  Microsoft: gethostbyname fails with WSANOTINITIALISED without a successful WSAStartup
  (https://learn.microsoft.com/en-us/windows/win32/api/winsock2/nf-winsock2-gethostbyname).
  In this test EXE the lookup worked without it even when run alone, so the test pins the result (127.0.0.1)
  and the ERangeError that a host name raised under range checking; it does not prove the WSAStartup call. }
procedure TTestInternetWin.TestResolveAddress_Localhost_WithoutWSAStartup;
CONST
  Loopback = $0100007F;   { 127.0.0.1 in network byte order (bytes 7F 00 00 01), read as a little-endian DWORD }
VAR
  Address: Cardinal;
begin
  Assert.IsTrue(ResolveAddress('localhost', Address), 'ResolveAddress(localhost) must succeed without a WSAStartup by the caller');
  Assert.AreEqual(Cardinal(Loopback), Address, 'localhost must resolve to 127.0.0.1');
end;


{ 10.0.0.200 in network byte order is the bytes 0A 00 00 C8, read as a little-endian DWORD $C800000A - above MaxInt.
  inet_addr returns it as the signed u_long; assigned to a DWORD without a cast it raises ERangeError under range checking. }
procedure TTestInternetWin.TestResolveAddress_DottedAddressHighOctet;
CONST
  Expected = $C800000A;
VAR
  Address: Cardinal;
begin
  Assert.IsTrue(ResolveAddress('10.0.0.200', Address), 'A dotted address must convert');
  Assert.AreEqual(Cardinal(Expected), Address, '10.0.0.200 in network byte order');
end;
{$ENDIF}




{ TTestInternetRegression }

procedure TTestInternetRegression.UrlExtractProtAndDomain_EmbeddedHttp;
begin
  Assert.AreEqual('https://web.archive.org', UrlExtractProtAndDomain('https://web.archive.org/web/2020/http://example.com/'));
end;

procedure TTestInternetRegression.UrlExtractProtAndDomain_QueryNoPath;
begin
  Assert.AreEqual('http://a.com', UrlExtractProtAndDomain('http://a.com?x=1'));
  Assert.AreEqual('http://a.com', UrlExtractProtAndDomain('http://a.com#top'));
  Assert.AreEqual('a.com', UrlExtractDomain('http://a.com?x=1'));
end;

procedure TTestInternetRegression.UrlPort_ColonInQuery;
begin
  Assert.AreEqual(0, UrlExtractPort('http://example.com/img.php?t=12:30'));
  Assert.AreEqual('http://example.com/img.php?t=12:30', UrlRemovePort('http://example.com/img.php?t=12:30'));
  Assert.AreEqual(8080, UrlExtractPort('http://example.com:8080/img.php?t=12:30'));
  Assert.AreEqual('http://example.com/img.php?t=12:30', UrlRemovePort('http://example.com:8080/img.php?t=12:30'));
end;

procedure TTestInternetRegression.ExpandURL_PathRelativeToFolder;
begin
  { The example of the routine's own header comment }
  Assert.AreEqual('www.dnabaser.com/tools/download/', ExpandURL('download/', 'www.dnabaser.com/tools/'));
  Assert.AreEqual('http://example.com/folder/b.html', ExpandURL('b.html', 'http://example.com/folder/page.html'));
end;

procedure TTestInternetRegression.ExpandURL_SpecialForms;
CONST
  Main = 'http://example.com/folder/page.html';
begin
  Assert.AreEqual('http://cdn.x.com/a.js',                      ExpandURL('//cdn.x.com/a.js', Main));
  Assert.AreEqual('https://cdn.x.com/a.js',                     ExpandURL('//cdn.x.com/a.js', 'https://example.com/'));
  Assert.AreEqual('http://example.com/folder/page.html?p=2',    ExpandURL('?p=2', Main));
  Assert.AreEqual('http://example.com/folder/page.html#top',    ExpandURL('#top', Main));
  Assert.AreEqual('http://example.com/page.html?p=2',           ExpandURL('?p=2', 'http://example.com/page.html?a=1#f'));
  Assert.AreEqual('http://example.com/page.html?a=1#top',       ExpandURL('#top', 'http://example.com/page.html?a=1#f'));
  Assert.AreEqual('mailto:a@b.c',                               ExpandURL('mailto:a@b.c', Main));
end;

procedure TTestInternetRegression.UrlRemoveStart_UpperCase;
begin
  Assert.AreEqual('Example.com',   UrlRemoveStart('WWW.Example.com'));
  Assert.AreEqual('Example.com/a', UrlRemoveStart('HTTP://Example.com/a'));
end;

procedure TTestInternetRegression.UrlExtractFileName_ColonInQuery;
begin
  Assert.AreEqual('photo.jpg', UrlExtractFileName('http://x.com/photo.jpg?t=12:30'));
  Assert.AreEqual('photo.jpg?t=12:30', UrlExtractFileName('http://x.com/photo.jpg?t=12:30', FALSE));
end;

procedure TTestInternetRegression.UrlExtractFilePath_HostOnly;
begin
  Assert.AreEqual('http://example.com/', UrlExtractFilePath('http://example.com'));
  Assert.AreEqual('http://example.com/', UrlExtractFilePath('http://example.com?x=1'));
  Assert.AreEqual('http://example.com/', GetReferer('http://example.com'));
end;

procedure TTestInternetRegression.ExtractProxyFrom_RejectsNonIp;
begin
  Assert.AreEqual('', ExtractProxyFrom('1.2.3.4xx:80'));
  Assert.AreEqual('', ExtractProxyFrom('www.proxy.site.com:80'));
  { The example of the routine's own header comment }
  Assert.AreEqual('1.210.03.23:80', ExtractProxyFrom('xxxxx1.210.03.23:80xxxx'));
end;

procedure TTestInternetRegression.UrlExtractDomain_IpHost;
begin
  Assert.AreEqual('192.168.1.1', UrlExtractDomain('http://192.168.1.1/'));
  Assert.IsFalse(SameWebSite('http://192.168.1.1/', 'http://10.168.1.1/'));
end;

procedure TTestInternetRegression.UrlEncode_UnsafeAscii;
begin
  // Hex codes from the ASCII table: " 22, brace-open 7B, brace-close 7D, | 7C, ^ 5E, backtick 60
  Assert.AreEqual('my%20%22file%22.jpg', UrlEncode('my "file".jpg'));
  Assert.AreEqual('%7B%7D%7C%5E%60', UrlEncode('{}|^`'));
end;

procedure TTestInternetRegression.ValidatePort_DecimalOnly;
begin
  Assert.IsFalse(ValidatePort('$50'));
  Assert.IsFalse(ValidatePort('0x50'));
  Assert.IsFalse(ValidatePort(' 80'));
  Assert.IsFalse(ValidatePort('+80'));
  Assert.IsFalse(ValidatePort(''));
  Assert.IsTrue (ValidatePort('80'));
end;

procedure TTestInternetRegression.GenerateLocalFilenameFromURL_InvalidChars;
begin
  { '*' and ':' are not allowed in a Windows file name }
  Assert.AreEqual('x.com_a_b.jpg',           GenerateLocalFilenameFromURL('http://x.com/a*b.jpg', 0));
  Assert.AreEqual('x.com_wiki_File_Foo.jpg', GenerateLocalFilenameFromURL('http://x.com/wiki/File:Foo.jpg', 0));
end;

procedure TTestInternetRegression.FileIsInFolder_NoTrailingSlash;
begin
  Assert.IsFalse(FileIsInFolder('www.test.com/images', 'www.test.com/images2/1.jpg'));
  Assert.IsTrue (FileIsInFolder('www.test.com/images', 'www.test.com/images/1.jpg'));
end;

procedure TTestInternetRegression.IsWebPage_QueryAndDoubleExtension;
begin
  Assert.IsFalse(IsWebPage('http://x.com/a.zip?from=index.php'));
  Assert.IsFalse(IsWebPage('http://x.com/a.html.zip'));
  Assert.IsTrue (IsWebPage('http://x.com/page.php?id=1'));
  Assert.IsTrue (IsWebPage('http://a.com?x=1'));
end;


initialization
  TDUnitX.RegisterTestFixture(TTestInternet);
  TDUnitX.RegisterTestFixture(TTestInternetRegression);
  {$IFDEF MSWINDOWS}
  TDUnitX.RegisterTestFixture(TTestInternetWin);
  {$ENDIF}

end.
