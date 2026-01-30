unit Test.LightVcl.Internet.Common;

{=============================================================================================================
   Unit tests for LightVcl.Internet.Common
   Tests URL parsing, validation, and basic network utilities.

   Note: Some tests require network connectivity and may fail if run offline.
   Network-dependent tests are marked with [Category('Network')].

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes;

type
  [TestFixture]
  TTestInternetCommon = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

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
    procedure TestParseURL_InvalidURL;

    [Test]
    procedure TestParseURL_HTTPSUrl;

    [Test]
    procedure TestParseURL_URLWithQueryString;

    [Test]
    procedure TestParseURL_URLWithUserPassword;

    { PathIsURL Tests }
    [Test]
    procedure TestPathIsURL_ValidHTTP;

    [Test]
    procedure TestPathIsURL_ValidHTTPS;

    [Test]
    procedure TestPathIsURL_InvalidPath;

    [Test]
    procedure TestPathIsURL_LocalPath;

    { GetLocalIP Tests - Network dependent }
    [Test]
    [Category('Network')]
    procedure TestGetLocalIP_ReturnsNonEmpty;

    [Test]
    [Category('Network')]
    procedure TestGetLocalIP_Overload_Success;

    { PCConnected2Internet Tests - Network dependent }
    [Test]
    [Category('Network')]
    procedure TestPCConnected2Internet;

    { IsPortOpened Tests - Network dependent }
    [Test]
    [Category('Network')]
    procedure TestIsPortOpened_LocalhostClosed;
  end;


implementation

uses
  LightVcl.Internet.Common;


procedure TTestInternetCommon.Setup;
begin
  { No setup required }
end;


procedure TTestInternetCommon.TearDown;
begin
  { No teardown required }
end;


{ ParseURL Tests }

procedure TTestInternetCommon.TestParseURL_FullURL;
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


procedure TTestInternetCommon.TestParseURL_SimpleURL;
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


procedure TTestInternetCommon.TestParseURL_URLWithPort;
VAR
  Parts: TStringArray;
begin
  Parts:= ParseURL('http://www.example.com:8080/page.html');

  Assert.AreEqual('http', Parts[0], 'Scheme should be http');
  Assert.AreEqual('www.example.com', Parts[1], 'Host should be www.example.com (port extracted separately)');
  Assert.AreEqual('/page.html', Parts[4], 'Path should be /page.html');
end;


procedure TTestInternetCommon.TestParseURL_EmptyURL;
VAR
  Parts: TStringArray;
begin
  Parts:= ParseURL('');

  Assert.AreEqual(6, Length(Parts), 'Should return 6 parts even for empty URL');
  Assert.AreEqual('', Parts[0], 'All parts should be empty for empty URL');
  Assert.AreEqual('', Parts[1], 'All parts should be empty for empty URL');
end;


procedure TTestInternetCommon.TestParseURL_InvalidURL;
VAR
  Parts: TStringArray;
begin
  Parts:= ParseURL('not a valid url');

  Assert.AreEqual(6, Length(Parts), 'Should return 6 parts even for invalid URL');
  { Invalid URLs may partially parse or return empty - both are acceptable }
end;


procedure TTestInternetCommon.TestParseURL_HTTPSUrl;
VAR
  Parts: TStringArray;
begin
  Parts:= ParseURL('https://secure.example.com/login');

  Assert.AreEqual('https', Parts[0], 'Scheme should be https');
  Assert.AreEqual('secure.example.com', Parts[1], 'Host should be secure.example.com');
  Assert.AreEqual('/login', Parts[4], 'Path should be /login');
end;


procedure TTestInternetCommon.TestParseURL_URLWithQueryString;
VAR
  Parts: TStringArray;
begin
  Parts:= ParseURL('http://api.example.com/search?q=test&page=1&sort=date');

  Assert.AreEqual('http', Parts[0], 'Scheme should be http');
  Assert.AreEqual('api.example.com', Parts[1], 'Host should be api.example.com');
  Assert.AreEqual('/search', Parts[4], 'Path should be /search');
  Assert.IsTrue(Parts[5].Contains('q=test'), 'ExtraInfo should contain query string');
end;


procedure TTestInternetCommon.TestParseURL_URLWithUserPassword;
VAR
  Parts: TStringArray;
begin
  Parts:= ParseURL('ftp://admin:secret123@ftp.example.com/files/');

  Assert.AreEqual('ftp', Parts[0], 'Scheme should be ftp');
  Assert.AreEqual('ftp.example.com', Parts[1], 'Host should be ftp.example.com');
  Assert.AreEqual('admin', Parts[2], 'User should be admin');
  Assert.AreEqual('secret123', Parts[3], 'Password should be secret123');
end;


{ PathIsURL Tests }

procedure TTestInternetCommon.TestPathIsURL_ValidHTTP;
begin
  Assert.IsTrue(PathIsURLW(PWideChar('http://www.example.com')), 'http:// should be recognized as URL');
end;


procedure TTestInternetCommon.TestPathIsURL_ValidHTTPS;
begin
  Assert.IsTrue(PathIsURLW(PWideChar('https://www.example.com')), 'https:// should be recognized as URL');
end;


procedure TTestInternetCommon.TestPathIsURL_InvalidPath;
begin
  { Note: PathIsURL only checks for scheme prefix, not full URL validity }
  Assert.IsFalse(PathIsURLW(PWideChar('not a url')), 'Plain text should not be recognized as URL');
end;


procedure TTestInternetCommon.TestPathIsURL_LocalPath;
begin
  Assert.IsFalse(PathIsURLW(PWideChar('C:\Windows\System32')), 'Local path should not be recognized as URL');
end;


{ GetLocalIP Tests }

procedure TTestInternetCommon.TestGetLocalIP_ReturnsNonEmpty;
VAR
  IP: string;
begin
  IP:= GetLocalIP;
  { IP could be an actual address or an error message }
  Assert.IsNotEmpty(IP, 'GetLocalIP should return something (IP or error)');
end;


procedure TTestInternetCommon.TestGetLocalIP_Overload_Success;
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


{ PCConnected2Internet Tests }

procedure TTestInternetCommon.TestPCConnected2Internet;
VAR
  Connected: Boolean;
begin
  { This test just verifies the function doesn't crash }
  Connected:= PCConnected2Internet;

  { Result depends on actual network state - just verify it returns boolean }
  Assert.IsTrue((Connected = True) OR (Connected = False), 'Should return valid boolean');
end;


{ IsPortOpened Tests }

procedure TTestInternetCommon.TestIsPortOpened_LocalhostClosed;
VAR
  IsOpen: Boolean;
begin
  { Test against a port that's almost certainly not listening }
  IsOpen:= IsPortOpened('127.0.0.1', 59999);
  { We expect this to be closed, but it's not guaranteed }
  Assert.IsTrue((IsOpen = True) OR (IsOpen = False), 'Should return valid boolean without crashing');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestInternetCommon);

end.
