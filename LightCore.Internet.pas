UNIT LightCore.Internet;

{-------------------------------------------------------------------------------------------------------------
   2026.10.01
   www.GabrielMoraru.com

   URL utils / URL parsing and validation

   This unit adds 87 kbytes to EXE size

   Related:
      Internet Status Detector: c:\Projects-3rd_Packages\Third party packages\InternetStatusDetector.pas

   Tester:
      LightSaber\Demo\Tester Internet\
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
   System.Types,   { DWORD, for ResolveAddress }
   System.SysUtils, System.StrUtils, System.Classes, System.IniFiles,
   LightCore, LightCore.Types;


CONST
  SeparatorsHTTP = [' ', '~', '`', '!', '$', '^', '&', '*', '(', ')', '[', ']', '{', '}', ';', ':', '''', '"', '<', '>', ',', '\', '|', #10, #13, #9];

  InvalidUrlChars= [#0..#32, #127, '"', '<',  '>', '^', '`', '{', '}', '|', '\',     '[', ']'];   { details: https://stackoverflow.com/a/36667242/46207 }
  ReservedCharacters= [';', '/', '?', ':', '@', '=', '&', '#', '%'];                              { https://perishablepress.com/stop-using-unsafe-characters-in-urls/  }

  ConnectedToInternet     = 'The program CAN access the Internet.';
  CheckYourFirewallMsg    = 'The program cannot access the Internet.  Please check your antivirus/firewall.';
  ComputerCannotAccessInet= 'The computer cannot access the Internet. Please check your Internet connection.';


{--------------------------------------------------------------------------------------------------
   URL PROCESSING
--------------------------------------------------------------------------------------------------}
 // BETTER PARSER HERE: E:\Backups\My projects\2021\2021.03 Stormy\BSalsa EmbeddedWB\Source\EwbUrl.pas

 function  UrlEncode                (CONST URL: string): string;                                  { Convert unsafe characters. For example space is converted to %20 }   { Prepare text to be used in 'a href' link }

 { Extract }
 function  UrlExtractDomain         (CONST URL: string): string;                                  { Removes the HTTP and WWW part. Example:  www.stuff.com/img.jpg -> stuff.com }
 function  UrlExtractDomainRelaxed  (CONST URL: string): string;
 function  UrlExtractDomainWWW      (CONST URL: string): string;                                  { Removes the HTTP part. Example:  http://www.stuff.com/img.jpg -> www.stuff.com }
 function  UrlExtractProtAndDomain  (CONST URL: string): string;                                  { Removes everyting after the .com. Example:  http://www.stuff.com/img.jpg -> http://www.stuff.com }
 function  UrlRemoveStart           (CONST URL: string): string;                                  { http://www.stuff.com/img.jpg  ->  stuff.com/img.jpg }

 function  URLExtractLastFolder     (CONST URL: string): string;
 function  URLExtractPrevFolder     (CONST URL: string): string;
 function  UrlExtractFilePath       (CONST URL: string): string;
 function  ExtractFilePath_FromURL  (CONST Url: string): string;                                  { This function is compatible with Windows - can be used to write local files }
 function  UrlExtractFileName       (CONST URL: string; CleanServerCommands: Boolean= TRUE): string;                                  { Ex: www.stuff.com/test/img.jpg?uniq=0 -> 'img.jpg' }

 function  GenerateLocalFilenameFromURL (CONST URL: string; UniqueChars: Integer= 6): string;
 function  GetReferer               (CONST URL: string): string;                                  { www.cams.de:80/down/Image.php?w=1200 -> www.cams.de/down/ }
 function  UrlExtractResource       (CONST URL: string): string;                                  { www.stuff.com/test/img.jpg -> /test/img.jpg }
 function  UrlExtractResourceParams (CONST URL: string): string;                                  { www.cams.de/getImage.php?w=1200 -> /getImage.php?w=1200 }

 function  CleanServerCommands      (CONST URL: string): string;
 function  UrlRemovePort            (CONST URL: string): string;                                  { Example www.Domain.com:80 -> www.Domain.com }
 function  UrlExtractPort           (CONST URL: string): Integer;                                 { Example www.Domain.com:80 -> 80 }
 function  UrlRemoveHttp            (CONST URL: string): string;                                  { http://www.domain/image.jpg  ->  www.domain/image.jp  }

 {}
 function  SameWebSite              (CONST URL1, URL2: string): Boolean;                          { Returns true if the two URLs belong to the same website }
 function  SameSubWebSite           (CONST URL1, URL2: string): Boolean;                          { Returns true if the URL1 belongs to URL2 or one of its subdomains }
 function  FileIsInFolder           (CONST MainURL, aFile: string): Boolean;                      { Returns true if the aFile is located of MainURL or one of its subfolders }
 {}
 procedure ExpandURLs               (ShortUrls: TStringList; CONST MainUrl: string);                    { Expand all urls in the list to a full http path. Example: If the MainURL is 'www.dnabaser.com/tools/' then 'download/' is expanded to 'www.dnabaser.com/tools/download/' }
 function  ExpandURL                (CONST ShortUrl, MainUrl: string): string;
 function  UrlToLocalPath           (CONST URL: string): string;                                  { Converts  http://www.Domain.com/download/setup.exe to Domain.com\download\setup.exe }

 function  URLMakeNonRelativeProtocol(CONST URL: string): string;                                       { Convert from Protocol-Relative to http }


{--------------------------------------------------------------------------------------------------
   URL VALIDATION
--------------------------------------------------------------------------------------------------}
 function  UrlCorrectInvalidChars_  (CONST URL, ReplaceWith: string): string;
 function  ValidURL     (CONST URL: string): Boolean;          { Returns True if string contain only valid chars AND strats with www or http }          //old name: UrlContainsValidChars
 function  ValidUrlChars(CONST URL: string): Boolean;          { Returns True if string seems to be a valid URL (does not contain invalid chars such as: < >. Does not check it the string starts with HTTP/WWW }

 function  UrlForceHttp             (CONST URL: string): string;                                  { Add 'HTTP' in from of the URL if it isn't there already }
 function  CheckHttpStart           (CONST URL: string): Boolean;                                 { Check if the URL starts with HTTP }
 function  CheckWwwStart            (CONST URL: string): Boolean;                                 { Check if the URL starts with 'WWW' }
 function  CheckURLStart            (CONST URL: string): Boolean;                                 { Check if the URL starts with HTTP or with www }

 function  IsURL                    (CONST   s: string): Boolean; deprecated 'Use CheckURLStart instead';    { Returns True if text starts wit http/www/ftp }
 function  IsWebPage                (CONST URL: string): Boolean;                                 { Returns true for /1/2/3.html but not for /1/2/ or for /1/2 }


{--------------------------------------------------------------------------------------------------
   IP TEXT UTILS
--------------------------------------------------------------------------------------------------}
 function ExtractIpFrom       (CONST aString: string): string;                                    { finds a IP address in a random string }
 function CollectIPAddress    (CONST HTMLBody: string): string;                                   { Extract address from HTML file }
 function IpExtractPort       (CONST Address: string): string;
 function SplitIpFromAdr      (CONST Address: string): string;
 function ServerStatus2String (Status: Integer): string;


{--------------------------------------------------------------------------------------------------
   IP ADR TEXT VALIDATION
--------------------------------------------------------------------------------------------------}
 function ValidateIpAddress (CONST Address: string): Boolean;
 function ValidateProxyAdr  (CONST Address: string): Boolean;
 function ValidatePort      (CONST Port: string)   : Boolean;
 function ExtractProxyFrom  (Line: string): string;    { Tries to extract a proxy address from a line of garbage text }
 function ExtractProxiesFrom(CONST Text: string): string;    { Tries to extract multiple proxies from a string (more than one line) of garbage text. Returns a list of proxies separated by enter }


{--------------------------------------------------------------------------------------------------
   IP ADDRESS
--------------------------------------------------------------------------------------------------}
 function  GetExternalIp(CONST ScriptAddress: string= 'http://checkip.dyndns.org'): string;

 { The routines below have a Windows body only (WinInet, WinSock), so far. They are declared only on Windows, so a call written in an Android, macOS or iOS build fails to compile instead of getting a silent wrong answer. }
 {$IFDEF MSWINDOWS}
 function  GetLocalIP: string;                                              overload;
 function  GetLocalIP(OUT HostName, IpAddress, ErrorMsg: string): Boolean;  overload;
 function  ResolveAddress (CONST HostName: String; out Address: DWORD): Boolean;
 function  GenerateInternetRep: string;

 function  ParseURL(CONST lpszUrl: string): TStringArray;                   { Breaks an URL in all its subcomponents. Example: ParseURL('http://login:password@somehost.somedomain.com/some_path/something_else.html?param1=val&param2=val')   }
 {$ENDIF}


{--------------------------------------------------------------------------------------------------
   IS CONNECTED
--------------------------------------------------------------------------------------------------}
 { A lightweight endpoint for ProgramConnect2Internet: it answers HTTP 200 with the tiny fixed body 'Microsoft Connect Test'.
   It is Windows' own NCSI probe target, so it is almost never blocked and transfers only a few bytes - ideal for a fast startup 'am I online / is my exe firewalled' check, instead of downloading a whole homepage. }
 CONST
   ConnectivityProbeURL     = 'http://www.msftconnecttest.com/connecttest.txt';
   ConnectivityProbeBody    = 'Microsoft Connect Test';   { The exact body ConnectivityProbeURL returns. Pass it as ExpectBody so a captive portal (which answers 200 with its own login HTML) is reported as state 2 (intercepted), not as a firewall block. }
   ConnectivityProbeTimeout = 8000;                        { Milliseconds. A check at startup must answer quickly, so it does not use the 60 s download default. }

 {$IFDEF MSWINDOWS}
 function  PCConnected2Internet: Boolean;                                       { From here: http://www.delphipages.com/forum/showthread.php?t=198159 }
 function  ProgramConnect2Internet: Integer;                                                                       overload;   { Legacy: google.com + the 60 s download default. Returns: -1 = PC not connected, 0 = connected but this app is blocked by the firewall, 1 = this app can reach the Internet}
 function  ProgramConnect2Internet(const TestURL: string; TimeoutMs: Integer= ConnectivityProbeTimeout; const ExpectBody: string= ''): Integer;  overload;   { Caller-set endpoint + timeout, so a startup check gets a verdict in seconds instead of the 60 s download default. Returns: -1 = PC not connected (WinInet); 0 = PC online but NO reply came back (this exe is firewall-blocked, or the endpoint is down); 1 = reached the endpoint and the body matched (genuinely online); 2 = reached the endpoint (HTTP 200) but the body was NOT ExpectBody -> a captive portal or a content-rewriting proxy is in the path, which is NOT a firewall block. Pass ConnectivityProbeURL for a fast, light default. ExpectBody='' = any HTTP 200 counts as 1 (state 2 never occurs); set it (e.g. ConnectivityProbeBody) to tell a genuine reply apart from a portal/proxy interception.}
 function  ProgramConnect2InternetS: string;
 function  IsPortOpened(const Host: string; Port: Integer): Boolean;            { Here's something very simple with which you can check a port status(opened/closed) on remote host. Add WinSock to uses clause}
 {$ENDIF}
 //see: c:\Projects\Projects INTERNET\Test Internet is connected.RAR - the tester inside is cInternet-is_connected.dpr


{--------------------------------------------------------------------------------------------------
   OPEN URL IN BROWSER
--------------------------------------------------------------------------------------------------}
 procedure OpenURL(const URL: string);                               { Opens URL in the default browser. Cross-platform (Win/macOS/Android/iOS). }


{--------------------------------------------------------------------------------------------------
   CREATE .URL FILES
--------------------------------------------------------------------------------------------------}
 Procedure CreateUrl    (CONST FullFileName, sFullURL: string);      { Creates an .URL file }



IMPLEMENTATION

USES
   //LightVcl.Visual.AppData,
   LightCore.HTML, LightCore.IO, LightCore.Download
   {$IFDEF MSWINDOWS}
   , Winapi.Windows, Winapi.ShellAPI
   , Winapi.WinInet   { ParseURL, PCConnected2Internet }
   , Winapi.WinSock   { GetLocalIP, ResolveAddress, IsPortOpened }
   {$ENDIF}
   {$IFDEF MACOS}
   , Macapi.AppKit, Macapi.Helpers, Macapi.Foundation
   {$ENDIF}
   {$IFDEF ANDROID}
   , Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Net, Androidapi.JNI.JavaTypes, Androidapi.Helpers, Androidapi.JNI.App
   {$ENDIF}
   {$IFDEF IOS}
   , iOSapi.UIKit, Macapi.Helpers
   {$ENDIF};




{--------------------------------------------------------------------------------------------------
   URL PATHS
--------------------------------------------------------------------------------------------------}
function UrlForceHttp(CONST URL: string): string;                  { Add 'HTTP' in from of the URL if it isn't there already }
begin
  if CheckHttpStart(URL)
  then Result:= URL
  else Result:= 'http://'+ URL;
end;



function CheckHttpStart(CONST URL: string): Boolean;               { Check if the URL starts with 'HTTP/HTTPS' }
begin
  Result:= (PosInsensitive('http://' , URL) = 1)
        OR (PosInsensitive('https://', URL) = 1);
end;    // https://lh4.googleusercontent.com/2gVoGQ6mMbQuscGho92xw-oL-UvrpqfAYX3a9eCqJkzyNwJNZD5Jdm1a2irS6xV0s_xvXUsxnzq_Qho=w1190-h559



function CheckWwwStart(CONST URL: string): Boolean;                { Check if the URL starts with 'WWW' }
VAR Start: Integer;
begin
  Start:= PosInsensitive('www.' , URL);
  Result:= (Start > 0)
       AND (Start < 10);                                            { this is the case where the URL has 'HTTP(s)://' at the beginning }
end;



function CheckURLStart(CONST URL: string): Boolean;                { Check if the URL starts with 'HTTPs' or 'www' }
begin
  Result:= CheckHttpStart(URL)
        OR CheckWwwStart (URL);
end;



function isUrl (CONST s: string): Boolean;  { DEPREACTED USE CheckURLStart }       { Returns True if text starts wit http/www/ftp }
begin
  Result:= CheckHttpStart(s)
        OR CheckWwwStart (s);
end;



function UrlCorrectInvalidChars_(CONST URL, ReplaceWith: string): string;
VAR i: Integer;
begin
  Result:= '';
  for i:= 1 to Length(URL) DO
     if  CharInSet(URL[I], SeparatorsHTTP)
     OR  (URL[i] < ' ')                                                            { tot ce e sub SPACE }
     then Result:= Result+ ReplaceWith
     else Result:= Result+ URL[i];
end;



function ValidUrlChars (CONST URL: string): Boolean;                             { Returns True if string seems to be a valid URL (does not contain invalid chars such as: < >. Does not check it the string starts with HTTP/WWW }
VAR
   i: Integer;
begin
  Result:= TRUE;

  { Check for other invalid chars }
  for i:= 1 to Length(URL) DO
    if CharInSet(URL[I], InvalidUrlChars)
    then EXIT(FALSE);
end;



function ValidURL (CONST URL: string): Boolean;                                   { Returns True if string contain only valid chars AND strats with www or http }
begin
  Result:= ValidUrlChars(URL);
  if Result
  then Result:= CheckURLStart(URL);                                                     { Force http becasue PathIsURLW requires this }
end;



{ Returns True if the URL appears to point to a web page (HTML/PHP/ASP).
  Returns True for: /1/2/3.html, domain.com/, domain.com, #
  Returns False for: /1/2/3.zip (downloadable files) }
function IsWebPage(CONST URL: string): Boolean;
VAR
   sURL: string;
   i: Integer;
begin
  if URL = '#' then EXIT(TRUE);

  { URLs ending with '/' point to a folder (and implicitly to index.html).
    Example: 'www.test.com/175754/' }
  if LastChar(URL) = '/' then EXIT(TRUE);

  { Domain-only URLs are web pages.
    IMPORTANT: This check must come after the trailing slash check
    because UrlExtractProtAndDomain cannot handle 'domain.com/' (only 'domain.com'). }
  if UrlExtractProtAndDomain(URL) = URL
  then EXIT(TRUE);

  { Extract the last segment of the URL.
    It could be a filename (e.g., /download/setup.zip) or a folder (e.g., /download). }
  i:= LastPos('/', URL);
  if i > 0
  then sURL:= System.Copy(URL, i + 1, MaxInt)
  else sURL:= URL;

  { If no dot found, it's likely a folder path, not a file }
  if Pos('.', sURL) < 1 then EXIT(TRUE);

  { Check for common web page extensions }
  Result:= (PosInsensitive('.htm', sURL) > 0)
        OR (PosInsensitive('.asp', sURL) > 0)
        OR (PosInsensitive('.php', sURL) > 0);
end;



{ Removes everything after the domain (TLD) but keeps the protocol.
  Works with subdomains and both HTTP and HTTPS.

  Examples:
     http://www.stuff.com/img.jpg     -> http://www.stuff.com
     https://sub.500px.org/photo/m%8  -> https://sub.500px.org
     www.example.com/page             -> www.example.com  }
function UrlExtractProtAndDomain(CONST URL: string): string;
VAR StartAt, FirstSlash: integer;
begin
  if URL= ''
  then raise exception.Create('Empty URL.');
  Result:= URL;

  StartAt:= PosInsensitive('http://', Result);
  if StartAt > 0
  then StartAt:= 8
  else
   begin
     StartAt:= PosInsensitive('https://', Result);
     if StartAt > 0
     then StartAt:= 9
     else StartAt:= 1;
   end;

  FirstSlash:= PosEx('/', URL, StartAt);
  if FirstSlash> 0
  then Result:= system.COPY(URL, 1, FirstSlash-1);
end;



{ Removes the HTTP part.
  Example:  http://www.stuff.com/img.jpg -> www.stuff.com }
function UrlExtractDomainWWW(CONST URL: string): string;
begin
  Result:= UrlExtractProtAndDomain(URL);

  { Remove http/https. Test '= 1' (prefix), not '> 0': Delete(1, N) must only fire when the protocol is actually AT THE START }
  if PosInsensitive('http://', Result) = 1
  then Delete(Result, 1, 7)
  else
    if PosInsensitive('https://', Result) = 1
    then Delete(Result, 1, 8);

  Result:= urlRemovePort(Result);  // remove port. Example www.Domain.com:80 -> www.Domain.com
end;



{ Extracts only the domain (removes the HTTP, WWW and PORT)
  Note: The last '/' is not included!

  Example:
       http://www.stuff.com/img.jpg     -> stuff.com
       http://www.syb.stuff.com/img.jpg -> stuff.com }
function UrlExtractDomain(CONST URL: string): string;
begin
  Result:= UrlExtractDomainRelaxed(URL);
  Result:= Urlremoveport(Result);

  { Remove www }
  if CountAppearance('.', Result) > 1
  then Result:= LightCore.CopyFrom(Result, '.', maxint, FALSE);
end;



function UrlRemoveStart(CONST URL: string): string;         { http://www.stuff.com/img.jpg  ->  stuff.com/img.jpg }
begin
  Result:= URL;

  if CheckWwwStart(Result)
  then Result:= LightCore.CopyFrom(Result, 'www.', MaxInt, FALSE)
  else
    if (PosInsensitive('http://' , Result) = 1)
    then Result:= LightCore.CopyFrom(Result, 'http://', MaxInt, FALSE)
    else
      if (PosInsensitive('https://' , Result) = 1)
      then Result:= LightCore.CopyFrom(Result, 'https://', MaxInt, FALSE);

  Assert(Result<> '', 'Empty in UrlRemoveStart');
end;



{ Extracts the domain and subdomain (removes the HTTP and WWW part)
  Note: The last '/' is not included!

  Example:
       http://www.stuff.com/img.jpg     -> stuff.com
       http://www.syb.stuff.com/img.jpg -> sub.stuff.com }
function UrlExtractDomainRelaxed(CONST URL: string): string;
begin
  Result:= UrlExtractDomainWWW(URL);

  { Remove www. Test '= 1' (prefix), not '> 0': a domain that merely CONTAINS 'www.' (e.g. 'x.www.example.com') must not lose its first 4 chars }
  if PosInsensitive('www.', Result) = 1
  then Delete(Result, 1, 4);
end;



function UrlRemoveHttp(CONST URL: string): string;    { http://www.domain/image.jpg  ->  www.domain/image.jp  }
begin
  Result:= URL;

  { Remove http/https. Test '= 1' (prefix), not '> 0': URLs with an EMBEDDED protocol (e.g. 'www.a.com/redir?to=http://b.com') must not lose their first 7 chars }
  if PosInsensitive('http://', Result) = 1
  then Delete(Result, 1, 7)
  else
    if PosInsensitive('https://', Result) = 1
    then Delete(Result, 1, 8);
end;



{ Generates a Windows-compatible filename from a URL.
  Format: Domain_ResourceName[_UniqueID].ext

  UniqueChars: Number of random characters to append for uniqueness.
               Set to 0 to disable unique suffix.

  Example:
    http://www.bionixwallpaper.com/help/images/day%20wallpaper.png
    -> bionixwallpaper.com_help_images_day wallpaper_A1B2C3.png }
function GenerateLocalFilenameFromURL(CONST URL: string; UniqueChars: Integer= 6): string;
VAR
  Resource: string;
begin
  Resource:= UrlExtractResource(URL);
  Resource:= ReplaceString(Resource, '%20', ' ');
  Resource:= RemoveFirstChar(Resource, '/');
  Resource:= ReplaceCharF(Resource, '/', '_');

  Result:= ExtractonlyName(Resource);
  if UniqueChars > 0
  then Result:= Result + '_' + GenerateUniqueString(UniqueChars) + ExtractFileExt(Resource)
  else Result:= Result + ExtractFileExt(Resource);

  Result:= UrlExtractDomain(URL) + '_' + Result;
  Result:= CorrectFolder(Result, '_');  { Replace invalid Windows filename chars }
end;



function GetReferer(CONST URL: string): string;             { www.cams.de:80/down/Image.php?w=1200 -> www.cams.de/down/ }
begin
  Result:= UrlExtractFilePath(URL);
  Result:= urlremoveport(Result);
end;



function UrlExtractResource(CONST URL: string): string;             { www.cams.de/getImage.php?w=1200 -> /getImage.php }
begin
  Result:= LightCore.CopyTo(URL, Length(UrlExtractProtAndDomain(URL))+ 1, '?', FALSE, TRUE, 1);
end;

function UrlExtractResourceParams(CONST URL: string): string;       { www.cams.de/getImage.php?w=1200 -> /getImage.php?w=1200 }
begin
  Result:= System.COPY(URL, Length(UrlExtractProtAndDomain(URL))+ 1, MaxInt);
end;



function UrlExtractFilePath(CONST Url: string): string;
var i: Integer;
begin
  i := LastDelimiter('/', Url);
  if i > 0
  then Result := Copy(Url, 1, i)
  else Result := Url;
end;



function ExtractFilePath_FromURL(CONST Url: string): string; { This function is compatible with Windows - can be used to write local files }
begin
  Result:= CleanServerCommands(Url);      { FILTER: Remove server commands (everything after '?') from URL  }
  Result:= UrlExtractFilePath(Result);
  Result:= urlRemovePort(Result);     //  remove www.Text.com:80/folder/img.jpg&600
end;



function UrlExtractFileName(CONST URL: string; CleanServerCommands: Boolean= TRUE): string;   { Ex: www.stuff.com/test/img.jpg?uniq=0 -> 'img.jpg' }
VAR I: Integer;
begin
  I := LastDelimiter('/:', URL);
  Result := system.COPY(URL, I + 1, MaxInt);

  if CleanServerCommands
  AND (Pos('?', Result) > 0)
  then Result:= LightCore.CopyTo(Result, 1, '?', FALSE);   { This fixes this case: worldnow.com/7day_web.jpg?7439232   or   cam_1.jpg?uniq=0.63  }
end;



function CleanServerCommands(CONST URL: string): string;    { FILTER: Remove server commands (everything after '?') from URL  }           { Example: www.pexels.com/1.jpeg?h=350&amp; -> www.pexels.com/1.jpeg }
begin
  if Pos('?', URL) > 0
  then Result:= LightCore.CopyTo(url, 1, '?', FALSE)
  else Result:= url;
end;



{ Removes port from URL while preserving the path.
  Example: http://www.Domain.com:8080/path -> http://www.Domain.com/path }
function UrlRemovePort(CONST URL: string): string;
VAR
  iPortStart, iPortEnd: Integer;
begin
  iPortStart:= Pos(':', URL);
  if iPortStart > 0
  then begin
    { Skip the colon in 'http://' or 'https://' }
    if (iPortStart + 1 <= Length(URL)) AND (URL[iPortStart + 1] = '/')
    then iPortStart:= PosEx(':', URL, iPortStart + 1);

    if iPortStart > 0
    then begin
      { Find the end of port number (first slash or end of string) }
      iPortEnd:= PosEx('/', URL, iPortStart + 1);
      if iPortEnd > 0
      then Result:= System.Copy(URL, 1, iPortStart - 1) + System.Copy(URL, iPortEnd, MaxInt)
      else Result:= System.Copy(URL, 1, iPortStart - 1);  { No path after port }
    end
    else
      Result:= URL;  { No port in this URL }
  end
  else
    Result:= URL;
end;



{ Extracts port number from URL. Example: www.Domain.com:80 -> 80
  Returns 0 if no port is specified. }
function UrlExtractPort(CONST URL: string): Integer;
VAR
  sURL: string;
  iPos: Integer;
begin
  iPos:= Pos(':', URL);
  if iPos > 0
  then 
    begin
     { Skip the colon in 'http://' or 'https://' }
     if (iPos + 1 <= Length(URL)) AND (URL[iPos + 1] = '/')
     then iPos:= PosEx(':', URL, iPos + 1);

     if iPos > 0
     then
       begin
         sURL:= System.Copy(URL, iPos + 1, MaxInt);
         iPos:= Pos('/', sURL);
         if iPos > 0
         then sURL:= Copy(sURL, 1, iPos - 1);
         Result:= StrToIntDef(sURL, 0);
       end
     else
       Result:= 0;  { No port in this URL }
    end
  else
    Result:= 0;
end;



{ Extract last folder of a FTP/HTTP path
  NOTE:
    If the path is a folder (does not ends with a filename) then it MUST end with a '/'
  Example:
     http://www.server.com/folder1/folder2/file.txt    returns: 'folder2'
     http://www.server.com/folder1/folder2/            returns: 'folder2'
     http://www.server.com/folder1/                    returns: '' (single folder - no "last" folder) }
function URLExtractLastFolder(CONST URL: string): string;
VAR
   iPos, iDomainEnd: Integer;
begin
  { Find end of domain (first '/' after '://') }
  iPos:= Pos('://', URL);
  if iPos > 0
  then iDomainEnd:= Pos('/', URL, iPos + 3)
  else iDomainEnd:= Pos('/', URL);
  if iDomainEnd < 1 then EXIT('');

  { Find last slash }
  iPos:= LastPos('/', URL);
  if iPos < 1 then EXIT('');

  { Remove trailing slash or filename }
  Result:= CopyTo(URL, 1, iPos-1);

  { Find second-to-last slash }
  iPos:= LastPos('/', Result);

  { If second-to-last slash is at or before domain end, return empty (only one folder) }
  if iPos <= iDomainEnd then EXIT('');

  Result:= system.COPY(Result, iPos+1, High(Integer));
end;


{ Extracts the second-to-last folder from a URL path.
  Example:
     http://www.server.com/folder1/folder2/file.txt  returns: 'folder1'
     http://www.server.com/folder1/folder2/          returns: 'folder1'
     http://www.server.com/folder1/                  returns: '' (no prev folder) }
function URLExtractPrevFolder(CONST URL: string): string;
VAR
   iDomainEnd, Pos0, Pos1, Pos2: Integer;
begin
  { Find end of domain (first '/' after '://') }
  Pos0:= Pos('://', URL);
  if Pos0 > 0
  then iDomainEnd:= Pos('/', URL, Pos0 + 3)
  else iDomainEnd:= Pos('/', URL);
  if iDomainEnd < 1 then EXIT('');

  { Find last slash (after filename or trailing slash) }
  Pos2:= LastPos('/', URL);
  if Pos2 < 1 then EXIT('');

  { Remove everything after last slash }
  Result:= System.Copy(URL, 1, Pos2 - 1);

  { Find the slash before the last folder }
  Pos1:= LastPos('/', Result);
  if Pos1 < 1 then EXIT('');

  { If this is the domain-end slash, there's only one folder (no prev folder) }
  if Pos1 <= iDomainEnd then EXIT('');

  { Remove the last folder }
  Result:= System.Copy(Result, 1, Pos1 - 1);

  { Find the slash before the prev folder }
  Pos0:= LastPos('/', Result);
  if Pos0 < 1 then EXIT('');

  { Extract just the prev folder name }
  Result:= System.Copy(Result, Pos0 + 1, MaxInt);
end;



function UrlToLocalPath(CONST URL: string): string;   { Converts  http://www.Domain.com/download/setup.exe to Domain.com\download\setup.exe }
begin
  Result:= UrlExtractDomain(URL) + UrlExtractResource(URL);
  Result:= ReplaceCharF(Result, '/', '\');
end;



function SameWebSite(CONST URL1, URL2: string): Boolean;    { Returns true if the two URLs belong to the same website }
begin
  Result:= SameText(UrlExtractDomain (URL1), UrlExtractDomain (URL2));
end;



function SameSubWebSite(CONST URL1, URL2: string): Boolean;    { Returns true if the URL1 belongs to URL2 or one of its subdomains }
begin
  Result:= SameText(UrlExtractDomainRelaxed (URL1), UrlExtractDomainRelaxed (URL2));
end;



{ Checks if a file URL is located within a given folder URL (including subfolders).
  Performs case-sensitive prefix matching on the path.

  Example (MainURL = 'www.test.com/images/'):
     'www.test.com/images/1.jpg'     -> True
     'www.test.com/images/sub/2.jpg' -> True
     'www.test.com/art/1.jpg'        -> False }
function FileIsInFolder(CONST MainURL, aFile: string): Boolean;
VAR
  FilePath: string;
begin
  FilePath:= UrlExtractFilePath(aFile);
  Result:= System.Copy(FilePath, 1, Length(MainURL)) = MainURL;
end;



 { Convert from Protocol-Relative to http }      { http://stackoverflow.com/questions/9646407/two-forward-slashes-in-a-url-src-href-attribute }
function URLMakeNonRelativeProtocol(CONST URL: string): string;
begin
  if Pos('//', url) = 1
  then Result:= 'http://'+ system.COPY(url, 3, MaxInt)
  else Result:= url;
end;


{ Expands a relative URL to an absolute URL using MainUrl as the base.
  Handles three cases:
    1. ShortUrl already has http(s):// - returned as-is
    2. ShortUrl starts with '/' - appended to domain only
    3. ShortUrl is relative - appended to full base path

  Examples (MainUrl = 'www.dnabaser.com/tools/'):
    'download/'       -> 'www.dnabaser.com/tools/download/'
    '/images/logo.png'-> 'www.dnabaser.com/images/logo.png'
    'http://other.com'-> 'http://other.com' }
function ExpandURL(CONST ShortUrl, MainUrl: string): string;
VAR
   Base: string;
begin
  Assert(ShortUrl <> '', 'ExpandURL: ShortUrl is empty');
  Base:= UrlExtractProtAndDomain(MainUrl);

  if CheckHttpStart(ShortUrl)
  then Result:= ShortUrl  { Already absolute URL }
  else
    if FirstCharIs(ShortUrl, '/')
    then Result:= Base + ShortUrl           { Root-relative: append to domain }
    else Result:= TrailLinuxPath(Base) + ShortUrl;  { Path-relative: append to base }
end;



{ Expands all short URLs in the list to full URLs using MainUrl as base.
  Modifies the ShortUrls list in place. }
procedure ExpandURLs(ShortUrls: TStringList; CONST MainUrl: string);
VAR
   i: Integer;
begin
  Assert(ShortUrls <> NIL, 'ExpandURLs: ShortUrls is nil');

  for i:= 0 to ShortUrls.Count - 1 DO
    ShortUrls[i]:= ExpandURL(ShortUrls[i], MainUrl);
end;







{--------------------------------------------------------------------------------------------------
   IP TEXT MANIPULATION
--------------------------------------------------------------------------------------------------}
{ Extracts the IP portion from an IP:Port string.
  Example: '192.168.0.1:80' returns '192.168.0.1'
  If no port separator (:) is found, returns the entire address. }
function SplitIpFromAdr(CONST Address: string): string;
VAR
  ColonPos: Integer;
begin
  ColonPos:= Pos(':', Address);
  if ColonPos > 0
  then Result:= System.Copy(Address, 1, ColonPos - 1)
  else Result:= Address;  { No port specified, return entire address }
end;


function IpExtractPort(CONST Address: string): string;     { Example  For 192.168.0.1:80 will retun '80' }
begin
 Result:= Trim(LightCore.CopyFrom(Address, ':', High(Integer), FALSE));
end;


function ExtractIpFrom(CONST aString: string): string;                                             { finds a IP address in a random string. The IP must be like this 192.168.12.234 }
var I: Integer;
begin
  Result := '';
  for I := 1 to Length(AString) DO
    if ((AString[I]>= '0') AND (AString[I]<='9')) OR (AString[I]='.')
    then Result := Result + AString[I];
end;


{ Extracts a proxy address (IP:Port) from garbage text.
  Scans for a valid IPv4 pattern (with 3 dots) followed by a port number.
  Example: "xxxxx1.210.03.23:80xxxx" returns "1.210.03.23:80"
  Returns empty string if no valid proxy found. }
function ExtractProxyFrom(Line: string): string;
VAR
  IP, Port: string;
  i, DotCount: Integer;
  ThirdDotPos: Integer;
begin
  Result:= '';

  { Early exit if no port separator }
  if Pos(':', Line) < 1 then EXIT;

  Line:= RemoveFormatings(Line);
  IP:= SplitIpFromAdr(Line);

  if IP = '' then EXIT;

  { Count the dots - need exactly 3 for IPv4.
    Scan from end to find position of the third dot (from the right). }
  DotCount:= 0;
  ThirdDotPos:= 0;
  for i:= Length(IP) downto 1 do
  begin
    if IP[i] = '.' then
    begin
      Inc(DotCount);
      if DotCount = 3 then
      begin
        ThirdDotPos:= i;
        Break;
      end;
    end;
  end;

  if DotCount < 3 then EXIT;

  { Find first non-digit char scanning left from the third dot.
    This strips garbage characters that precede the IP address. }
  i:= ThirdDotPos - 1;
  while (i > 0) AND CharIsNumber(IP[i]) do
    Dec(i);

  { Extract the clean IP starting after the garbage }
  IP:= System.Copy(IP, i + 1, MaxInt);

  { Extract port number }
  Port:= IpExtractPort(Line);
  if Port = '' then EXIT;

  { Find end of port number (first non-digit) }
  i:= 1;
  while (i <= Length(Port)) AND CharIsNumber(Port[i]) do
    Inc(i);
  Port:= System.Copy(Port, 1, i - 1);

  if (IP <> '') AND (Port <> '')
  then Result:= IP + ':' + Port;
end;


function ExtractProxiesFrom(CONST Text: string): string;    { Tries to extract multiple proxies from a string (more than one line) of garbage text. Returns a list of proxies separated by enter }
VAR
   i: Integer;
   Line: string;
   TSL: TStringList;
begin
  Result:= '';
  TSL:= TStringList.Create;
  TRY
   TSL.Text:= Text;
   for i:= 0 to TSL.Count-1 DO
    begin
     Line:= TSL[i];
     Line:= ExtractProxyFrom(Line);
     if Line > ''
     then Result:= Result+ Line+ CRLF;
    end;
  FINALLY
   FreeAndNil(TSL);
  END;
end;


{ Extracts IP address from HTML response (e.g., "Current IP Address: 192.168.1.1").
  Looks for text after the first colon (:) delimiter. }
function CollectIPAddress(CONST HTMLBody: string): string;
CONST DELIMITER = ':';
VAR
  ColonPos: Integer;
begin
  ColonPos:= Pos(DELIMITER, HTMLBody);
  if ColonPos > 0
  then Result:= Trim(System.Copy(HTMLBody, ColonPos + 1, Length(HTMLBody)))
  else Result:= '';
end;


{ Converts HTTP status code to human-readable description.
  Returns the numeric code as string for unknown status codes. }
function ServerStatus2String(Status: Integer): string;
begin
  case Status of
    { Informational 1xx }
    100: Result:= 'Continue';
    101: Result:= 'Switching Protocols';
    102: Result:= 'Processing';
    103: Result:= 'Early Hints';
    { Successful 2xx }
    200: Result:= 'OK';
    201: Result:= 'Created';
    202: Result:= 'Accepted';
    203: Result:= 'Non-Authoritative Information';
    204: Result:= 'No Content';
    205: Result:= 'Reset Content';
    206: Result:= 'Partial Content';
    207: Result:= 'Multi-Status';
    { Redirection 3xx }
    300: Result:= 'Multiple Choices';
    301: Result:= 'Moved Permanently';
    302: Result:= 'Found';  { Was 'Moved Temporarily' }
    303: Result:= 'See Other';
    304: Result:= 'Not Modified';
    305: Result:= 'Use Proxy';
    307: Result:= 'Temporary Redirect';
    308: Result:= 'Permanent Redirect';
    { Client Error 4xx }
    400: Result:= 'Bad Request';
    401: Result:= 'Unauthorized';
    402: Result:= 'Payment Required';
    403: Result:= 'Forbidden';
    404: Result:= 'Not Found';
    405: Result:= 'Method Not Allowed';
    406: Result:= 'Not Acceptable';
    407: Result:= 'Proxy Authentication Required';
    408: Result:= 'Request Timeout';
    409: Result:= 'Conflict';
    410: Result:= 'Gone';
    411: Result:= 'Length Required';
    412: Result:= 'Precondition Failed';
    413: Result:= 'Payload Too Large';
    414: Result:= 'URI Too Long';
    415: Result:= 'Unsupported Media Type';
    416: Result:= 'Range Not Satisfiable';
    417: Result:= 'Expectation Failed';
    418: Result:= 'I''m a teapot';  { RFC 2324 }
    422: Result:= 'Unprocessable Entity';
    429: Result:= 'Too Many Requests';
    451: Result:= 'Unavailable For Legal Reasons';
    { Server Error 5xx }
    500: Result:= 'Internal Server Error';
    501: Result:= 'Not Implemented';
    502: Result:= 'Bad Gateway';
    503: Result:= 'Service Unavailable';
    504: Result:= 'Gateway Timeout';
    505: Result:= 'HTTP Version Not Supported';
    507: Result:= 'Insufficient Storage';
    508: Result:= 'Loop Detected';
  else
    Result:= IntToStr(Status);
  end;
end;




{--------------------------------------------------------------------------------------------------
   IP VALIDATION
--------------------------------------------------------------------------------------------------}
{ Validates an IPv4 address string (e.g., "192.168.1.1").
  Returns True if the address has exactly 4 octets, each 0-255. }
function ValidateIpAddress(CONST Address: string): Boolean;
CONST
  ValidChars = ['0'..'9', '.'];
VAR
  i, OctetCount, OctetValue: Integer;
  sAddress, Octet: string;
begin
  sAddress:= Trim(Address);

  if sAddress = ''
  then EXIT(FALSE);

  if (Length(sAddress) > 15) OR (sAddress[1] = '.') OR (sAddress[Length(sAddress)] = '.')
  then EXIT(FALSE);

  { Validate all characters are digits or dots }
  for i:= 1 to Length(sAddress) do
    if NOT CharInSet(sAddress[i], ValidChars)
    then EXIT(FALSE);

  { Check no consecutive dots }
  if Pos('..', sAddress) > 0
  then EXIT(FALSE);

  { Parse and validate each octet }
  OctetCount:= 0;
  Octet:= '';

  for i:= 1 to Length(sAddress) do
  begin
    if sAddress[i] = '.'
    then begin
      if Octet = ''
      then EXIT(FALSE);  { Empty octet }
      OctetValue:= StrToIntDef(Octet, -1);
      if (OctetValue < 0) OR (OctetValue > 255)
      then EXIT(FALSE);
      Inc(OctetCount);
      Octet:= '';
    end
    else
      Octet:= Octet + sAddress[i];
  end;

  { Validate last octet }
  if Octet = ''
  then EXIT(FALSE);
  OctetValue:= StrToIntDef(Octet, -1);
  if (OctetValue < 0) OR (OctetValue > 255)
  then EXIT(FALSE);
  Inc(OctetCount);

  Result:= (OctetCount = 4);
end;



function ValidatePort(CONST Port: string): Boolean;
VAR iPort: Integer;
begin
  iPort:= StrToIntDef(Port, -1);
  Result:= (iPort>= 0) AND (iPort < 65536);
end;



function ValidateProxyAdr(CONST Address: string): Boolean;
VAR IP, Port: string;
begin
  IP  := SplitIpFromAdr  (Address);
  Port:= IpExtractPort(Address);

  Result:= ValidateIpAddress(IP) AND ValidatePort(Port);
end;





{ Retrieves the external/public IP address by querying an online service.
  ScriptAddress: URL of the IP detection service (default: checkip.dyndns.org).
  Alternative providers: 'http://api.ipify.org', 'http://icanhazip.com'
  Returns empty string on failure. }
function GetExternalIp(CONST ScriptAddress: string= 'http://checkip.dyndns.org'): string;
VAR
  HtmlResponse, Body: string;
begin
  Result:= '';

  HtmlResponse:= DownloadAsString(ScriptAddress);
  if HtmlResponse = ''
  then EXIT;

  Body:= GetBodyFromHtml(HtmlResponse);
  if Body = ''
  then Body:= HtmlResponse;   { Services like api.ipify.org / icanhazip.com return the IP as PLAIN text (no <body> tag). GetBodyFromHtml returns '' for those }

  Result:= ExtractIpFrom(Body);
  if Result = ''
  then EXIT;

  { Remove line break characters (different services use different line endings) }
  Result:= ReplaceString(Result, CR, '');
  Result:= ReplaceString(Result, LF, '');
  Result:= Trim(Result);

  { ExtractIpFrom collects ALL digits/dots from the text, so a non-IP response (error page, IPv6 answer) yields garbage. Honor the 'empty string on failure' contract instead }
  if NOT ValidateIpAddress(Result)
  then Result:= '';
end;







{==================================================================================================
   .URL FILE CREATION
==================================================================================================}

{ Creates a Windows .URL shortcut file. The filename should end in .URL extension. }
procedure CreateUrl(CONST FullFileName, sFullURL: string);
VAR IniFile: TIniFile;
begin
  IniFile:= TIniFile.Create(FullFileName);
  TRY
    IniFile.WriteString('InternetShortcut', 'URL', sFullURL);
  FINALLY
    FreeAndNil(IniFile);
  END;
end;


{ Encodes a URL by converting unsafe characters to %XX hex format.
  Safe characters (ASCII 33-126 except UnsafeChars) are kept as-is.
  All other characters (control chars, non-ASCII) are percent-encoded as their UTF-8 BYTES
  (e.g. 'é' -> %C3%A9), as required by RFC 3987 and expected by modern web servers.
  Encoding the Char ordinal directly would produce Latin-1 escapes for #128..#255 and
  MALFORMED escapes like %263A (4 hex digits) for chars above #255.
  Also fixes the Indy encoding issue: stackoverflow.com/questions/5708863 }
function UrlEncode(CONST URL: string): string;
VAR
   i: Integer;
   ByteOrd: Integer;
   Utf8: UTF8String;
CONST
   UnsafeChars = ['*', '#', '%', '<', '>', ' ', '[', ']', '\', '@'];
begin
  Result:= '';
  Utf8:= UTF8Encode(URL);
  for i:= 1 to Length(Utf8) DO
    begin
      ByteOrd:= Ord(Utf8[i]);
      if  (ByteOrd > 32)
      AND (ByteOrd < 127)  { Only printable ASCII chars (33-126) }
      AND (NOT CharInSet(Utf8[i], UnsafeChars))
      then Result:= Result + Char(Utf8[i])
      else Result:= Result + '%' + IntToHex(ByteOrd, 2);
    end;
end;




{--------------------------------------------------------------------------------------------------
   OPEN URL IN BROWSER
--------------------------------------------------------------------------------------------------}

{ Opens URL in the default browser. Cross-platform. }
procedure OpenURL(const URL: string);
{$IFDEF MACOS}
var NSURL_: NSURL;
{$ENDIF}
begin
  if URL.IsEmpty then Exit;

  {$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', PChar(URL), NIL, NIL, SW_SHOWNORMAL);
  {$ENDIF}

  {$IFDEF MACOS}
  NSURL_:= TNSURL.Wrap(TNSURL.OCClass.URLWithString(StrToNSStr(URL)));
  TNSWorkspace.Wrap(TNSWorkspace.OCClass.sharedWorkspace).openURL(NSURL_);
  {$ENDIF}

  {$IFDEF ANDROID}
  TAndroidHelper.Activity.startActivity(
    TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW,
      TJnet_Uri.JavaClass.parse(StringToJString(URL))));
  {$ENDIF}

  {$IFDEF IOS}
  TUIApplication.Wrap(TUIApplication.OCClass.sharedApplication)
    .openURL(TNSURL.Wrap(TNSURL.OCClass.URLWithString(StrToNSStr(URL))));
  {$ENDIF}
end;



{$IFDEF MSWINDOWS}

{---------------------------------------------------------------------------------------------------------------
   ParseURL
   Breaks a URL into its subcomponents using Windows InternetCrackUrl API.
   Returns an array of 6 strings: [Scheme, HostName, UserName, Password, UrlPath, ExtraInfo]
   Returns empty strings for all components if URL is invalid or empty.

   Example: ParseURL('http://login:password@somehost.somedomain.com/some_path/file.html?param1=val')
            Returns: ['http', 'somehost.somedomain.com', 'login', 'password', '/some_path/file.html', '?param1=val']
---------------------------------------------------------------------------------------------------------------}
function ParseURL(const lpszUrl: string): TStringArray;    { Source: http://stackoverflow.com/questions/16703063/how-do-i-parse-a-web-url }
VAR
  lpszScheme      : array[0..INTERNET_MAX_SCHEME_LENGTH - 1]    of Char;
  lpszHostName    : array[0..INTERNET_MAX_HOST_NAME_LENGTH - 1] of Char;
  lpszUserName    : array[0..INTERNET_MAX_USER_NAME_LENGTH - 1] of Char;
  lpszPassword    : array[0..INTERNET_MAX_PASSWORD_LENGTH - 1]  of Char;
  lpszUrlPath     : array[0..INTERNET_MAX_PATH_LENGTH - 1]      of Char;
  lpszExtraInfo   : array[0..1024 - 1]                          of Char;
  lpUrlComponents : TURLComponents;
begin
  SetLength(Result, 6);

  if lpszUrl = '' then EXIT;

  ZeroMemory(@lpszScheme      , SizeOf(lpszScheme));
  ZeroMemory(@lpszHostName    , SizeOf(lpszHostName));
  ZeroMemory(@lpszUserName    , SizeOf(lpszUserName));
  ZeroMemory(@lpszPassword    , SizeOf(lpszPassword));
  ZeroMemory(@lpszUrlPath     , SizeOf(lpszUrlPath));
  ZeroMemory(@lpszExtraInfo   , SizeOf(lpszExtraInfo));
  ZeroMemory(@lpUrlComponents , SizeOf(TURLComponents));

  { The dwXxxLength fields are buffer sizes in TCHARs, NOT bytes (MSDN URL_COMPONENTSW: "Size of the scheme name, in TCHARs").
    SizeOf would report 2x the real capacity (WideChar = 2 bytes) and let InternetCrackUrlW overflow the stack buffers. }
  lpUrlComponents.dwStructSize      := SizeOf(TURLComponents);
  lpUrlComponents.lpszScheme        := lpszScheme;
  lpUrlComponents.dwSchemeLength    := Length(lpszScheme);
  lpUrlComponents.lpszHostName      := lpszHostName;
  lpUrlComponents.dwHostNameLength  := Length(lpszHostName);
  lpUrlComponents.lpszUserName      := lpszUserName;
  lpUrlComponents.dwUserNameLength  := Length(lpszUserName);
  lpUrlComponents.lpszPassword      := lpszPassword;
  lpUrlComponents.dwPasswordLength  := Length(lpszPassword);
  lpUrlComponents.lpszUrlPath       := lpszUrlPath;
  lpUrlComponents.dwUrlPathLength   := Length(lpszUrlPath);
  lpUrlComponents.lpszExtraInfo     := lpszExtraInfo;
  lpUrlComponents.dwExtraInfoLength := Length(lpszExtraInfo);

  { Parse URL - if it fails, arrays remain zeroed (empty strings) }
  if NOT InternetCrackUrl(PChar(lpszUrl), Length(lpszUrl), ICU_DECODE or ICU_ESCAPE, lpUrlComponents)
  then EXIT;   { Return empty strings on failure }

  Result[0]:= lpszScheme;                  { Protocol        (http)              }
  Result[1]:= lpszHostName;                { Host            (www.domain.com)    }
  Result[2]:= lpszUserName;                { User            ('')                }
  Result[3]:= lpszPassword;                { Password        ('')                }
  Result[4]:= lpszUrlPath;                 { Path            ('/download.html')  }
  Result[5]:= lpszExtraInfo;               { ExtraInfo       ('')                }
end;




{==================================================================================================
   IS CONNECTED
==================================================================================================}
function PCConnected2Internet: Boolean;
VAR dwConnectionTypes: DWORD;
begin
 dwConnectionTypes := INTERNET_CONNECTION_MODEM + INTERNET_CONNECTION_LAN + INTERNET_CONNECTION_PROXY;
 Result := InternetGetConnectedState(@dwConnectionTypes, 0);         { Function summary from MS: Retrieves the connected state of the local system. Minimum supported client: Windows 2000 Professional [desktop apps only] }   { API Function documentation: http://msdn.microsoft.com/en-us/library/windows/desktop/aa384702%28v=vs.85%29.aspx }
end;



function ProgramConnect2InternetS: string;
begin
 if PCConnected2Internet
 then
  begin
    Result:= LightCore.Download.DownloadAsString('http://www.google.com/');
    if Result= ''
    then Result:= CheckYourFirewallMsg
    else Result:= ConnectedToInternet
  end
 else
   Result:= ComputerCannotAccessInet;
end;



{ Returns:
           -1 if computer is not connected to internet,
            0 if local system is connected to internet but application is blocked by firewall,
            1 if application can connect to internet.
  Legacy overload - kept for backward compatibility: google.com with the 60 s download default.
  For a fast startup check use the (TestURL, TimeoutMs) overload with ConnectivityProbeURL. }
function ProgramConnect2Internet: Integer;
begin
 Result:= ProgramConnect2Internet('http://www.google.com/', 60000);
end;


{ As above, but the caller chooses the test endpoint and the timeout, so a check at startup gets a verdict in TimeoutMs (default ConnectivityProbeTimeout) instead of the 60 s download default.
  ConnectivityProbeURL is such an endpoint.
  The PCConnected2Internet gate is instant and does no traffic, so an offline PC returns -1 without waiting on the timeout.
  The verdict keys off whether an HTTP 200 came back, NOT off the body alone.
  DownloadAsString (LightCore.Download.pas) leaves ErrorMsg empty ONLY on HTTP 200; a non-200, a timeout, a DNS/TLS failure, or the firewall blocking this exe all set ErrorMsg.
  Only then does the body decide between 1 (the expected content) and 2 (a reply, but not the marker: a captive portal serving its own login HTML, or a proxy rewriting the content).
  ExpectBody='' skips the content test, so any HTTP 200 is 1 and state 2 never occurs (this keeps the legacy google.com overload a strict -1/0/1). }
function ProgramConnect2Internet(const TestURL: string; TimeoutMs: Integer; const ExpectBody: string): Integer;
VAR
  Options  : RHttpOptions;
  ErrorMsg : string;
  Body     : string;
begin
 if NOT PCConnected2Internet then EXIT(-1);      { WinInet: the PC has no network at all }

 Options.Reset;
 Options.ConnectionTimeout := TimeoutMs;
 Options.ResponseTimeout   := TimeoutMs;
 Body:= LightCore.Download.DownloadAsString(TestURL, ErrorMsg, NIL, @Options);   { ErrorMsg='' ONLY on HTTP 200; '' body + reason on any failure }

 if ErrorMsg <> ''
 then EXIT(0);                                   { no HTTP 200 came back -> this exe is blocked / the endpoint is down }

 { An HTTP 200 returned, so the request reached the Internet and came back - the firewall is NOT
   blocking this exe. Judge the CONTENT to tell a real reply from a portal/proxy interception. }
 if (ExpectBody = '') or (Pos(ExpectBody, Body) > 0)
 then Result := 1                                { expected content -> genuinely online }
 else Result := 2;                               { a 200, but not the marker -> captive portal / rewriting proxy (not a firewall block) }
end;




{==================================================================================================
   GET IP ADDRESS
==================================================================================================}

{
IsConnectedToInternet Example 2

USES WinInet   <-   This will generate error if WinInet library is not installed in the computer. Dont added to the uses clauses if not needed
function IsConnectedToInternet2: Boolean;
CONST
  INTERNET_CONNECTION_MODEM      = 1; // local system uses a modem to connect to the Internet.
  INTERNET_CONNECTION_LAN        = 2; // local system uses a local area network to connect to the Internet.
  INTERNET_CONNECTION_PROXY      = 4; // local system uses a proxy server to connect to the Internet.
  INTERNET_CONNECTION_MODEM_BUSY = 8; // local system's modem is busy with a non-Internet connection.
VAR
  dwConnectionTypes : DWORD;
BEGIN
  dwConnectionTypes :=
   INTERNET_CONNECTION_MODEM +
   INTERNET_CONNECTION_LAN +
   INTERNET_CONNECTION_PROXY;
  Result := InternetGetConnectedState(@dwConnectionTypes,0);
END;
Note: this solution only works if IE is installed, so it would fail on 'older' machines, like most Windows NT 4 computers. You app would then display an error during program startup if you referred to Wininet. Since today, there are many ways to connect to the Internet (via LAN, Dialup/RAS, ADSL, ..) propably the best way would be to test for certain IPs. Here is a link to more information on the topic, including a list of ways to find out whether an Internet connection seems to be active or not. }


{
  Get ALL local IPs?
  http://stackoverflow.com/questions/576538/delphi-how-to-get-all-local-ips - see the last answer (Remko)
}

Function GetLocalIP: string;
VAR
  HostName, IpAddress, Error: string;
begin
  if GetLocalIP(HostName, IpAddress, Error)
  then Result:= IpAddress
  else Result:= Error;
end;


function GetLocalIP(OUT HostName, IpAddress, ErrorMsg: string): Boolean;
VAR
  Addr: PAnsiChar;
  WSAData: TWSAData;
  RemoteHost: pHostEnt;
  HostNameArr: array[0..255] of AnsiChar;
begin
  Result   := False;
  HostName := '';
  IpAddress:= '';
  ErrorMsg := '';

  // Initialize WinSock
  if WSAStartup($0202, WSAData) <> 0 then
  begin
    ErrorMsg := 'WinSock initialization failed!';
    Exit;
  end;

  try
    // Retrieve the local host name
    if gethostname(HostNameArr, SizeOf(HostNameArr)) = SOCKET_ERROR then
    begin
      case WSAGetLastError of
        WSANOTINITIALISED: ErrorMsg := 'WSA Not Initialized';
        WSAENETDOWN      : ErrorMsg := 'Network subsystem is down';
        WSAEINPROGRESS   : ErrorMsg := 'A blocking operation is in progress';
      else
        ErrorMsg:= 'Unknown error retrieving host name';
      end;

      Exit;
    end;

    HostName := string(HostNameArr);

    // Get host details by name
    RemoteHost := gethostbyname(HostNameArr);
    if RemoteHost = nil then
    begin
      ErrorMsg := 'Unable to resolve host details.';
      Exit;
    end;

    // Extract the IP address
    Addr := RemoteHost^.h_addr_list^;
    while Addr <> nil do
    begin
      for VAR I := 0 to RemoteHost^.h_length - 1 do
        IpAddress := IpAddress + IntToStr(Byte(Addr[I])) + '.';

      SetLength(IpAddress, Length(IpAddress) - 1); // Remove trailing dot
      Break; // Only take the first IP address
    end;

    Result:= True;
  finally
    WSACleanup;
  end;
end;



function GenerateInternetRep: string;
var HostName, IPaddr, Error: string;
begin
 Result:= ' [INTERNET]'+ CRLF;

 Result:= Result+'  GetExternalIp: '  + Tab + GetExternalIp+ CRLF;
 Result:= Result+'  GetLocalIP: '+ CRLF;
 if GetLocalIP(HostName, IPaddr, Error)
 then
   begin
     Result:= Result+'     Host: '+ Tab + HostName + CRLF;
     Result:= Result+'     IP'    + Tab + IPaddr   + CRLF;
   end
 else
   Result:= Result+ '     FAIL! '+ Error + CRLF;
end;




{
 Check a port status(opened/closed) on remote host. Uses WinSock.
 http://www.delphigeist.com/search?updated-min=2010-01-01T00%3A00%3A00%2B02%3A00&updated-max=2011-01-01T00%3A00%3A00%2B02%3A00&max-results=37
}
function ResolveAddress(CONST HostName: String; out Address: DWORD): Boolean;
VAR
   lpHost: PHostEnt;
   AnsiHostName: AnsiString;
begin
  AnsiHostName:= AnsiString(HostName);
  Address:= DWORD(INADDR_NONE);                                     // Set default address
  TRY
    if Length(AnsiHostName) > 0 then                                // Check host name length
     begin
      Address:= inet_addr(PAnsiChar(AnsiHostName));                  // Try converting the hostname. In Delphi 7 this was PChar
      if (DWORD(Address) = DWORD(INADDR_NONE)) then                  // Check address
       begin
        lpHost := gethostbyname(PAnsiChar(AnsiHostName));            // Attempt to get host by name

        // Check host ent structure for valid ip address
        if Assigned(lpHost) and Assigned(lpHost^.h_addr_list^)
        then Address := u_long(PLongInt(lpHost^.h_addr_list^)^);     // Get the address from the list
      end;
    end;
  FINALLY
    // Check result address
    if (DWORD(Address) = DWORD(INADDR_NONE))
    then Result:= False    // Invalid host specified
    else Result:= True;   // Converted correctly
  END;
end;


function IsPortOpened(const Host: string; Port: Integer): Boolean;
const
  szSockAddr = SizeOf(TSockAddr);
var
  WinSocketData: TWSAData;
  Socket: TSocket;
  Address: TSockAddr;
  dwAddress: DWORD;
label
  lClean;
begin
  Result := False;
  if Winapi.WinSock.WSAStartup(MakeWord(1, 1), WinSocketData) = 0 then
  begin
    Address.sin_family := AF_INET;
    if NOT ResolveAddress(Host, dwAddress) then
      goto lClean;
    Address.sin_addr.S_addr := dwAddress;
    Socket := Winapi.WinSock.Socket(AF_INET, SOCK_STREAM, IPPROTO_IP);
    if Socket = INVALID_SOCKET then
      goto lClean;
    Address.sin_port := Winapi.WinSock.htons(Port);
    if Winapi.WinSock.Connect(Socket, Address, szSockAddr) = 0
    then Result := True;
    // close the socket (also on failed connect - WSACleanup only deallocates it when the process-wide refcount drops to zero)
    Winapi.WinSock.closesocket(Socket);
  end;// if WinSock.WSAStartup(MakeWord(1, 1), WinSocketData) = 0 then begin
  lClean:
    Winapi.WinSock.WSACleanup;
end;

{HOW TO USE IT:

if IsPortOpened('google.com', 80) then
  ShowMessage('google has port 80 opened')
else
  ShowMessage('google has port 80 closed???');
}

{$ENDIF}


end.
