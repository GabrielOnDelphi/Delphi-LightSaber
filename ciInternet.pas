UNIT ciInternet;

{-------------------------------------------------------------------------------------------------------------
   Gabriel Moraru
   2025
   See Copyright.txt

   URL utils / URL parsing and validation

   This unit adds 87 kbytes to EXE size

   Related:
      Internet Status Detector: c:\Projects-3rd_Packages\Third party packages\InternetStatusDetector.pas

   Tester:
      LightSaber\Demo\Tester Internet\
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
   Winapi.Windows,
   Winapi.UrlMon,
   Winapi.WinSock,  { Required by GetLocalIP }
   Winapi.WinInet,  { Required by IE_ApplySettings }
   System.SysUtils, System.StrUtils, System.Classes, System.IniFiles, System.Win.Registry,
   ccCore, cbDialogs;


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
 // BETTER PARSER HERE: c:\MyProjects\Packages\BSalsa EmbeddedWB\EwbUrl.pas

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
 function  ParseURL                 (CONST lpszUrl: string): TStringArray;                        { Breaks an URL in all its subcomponents. Example: ParseURL('http://login:password@somehost.somedomain.com/some_path/something_else.html?param1=val&param2=val')   }
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

 {$EXTERNALSYM PathIsURLA}
 function  PathIsURLA(pszPath: PAnsiChar): BOOL; stdcall;  {$EXTERNALSYM PathIsURLW}              { $HPPEMIT '#include <shlwapi.h>'}
 function  PathIsURLW(pszPath: PWideChar): BOOL; stdcall;                                         { from here: https://msdn.microsoft.com/en-us/library/windows/desktop/bb773724(v=vs.85).aspx. But it is not good at all because it only checks if path starts with http and if it contains space. Otherwise it accepts all other characters. So I use it in conjunction with my own function }

 function  UrlForceHttp             (CONST URL: string): string;                                  { Add 'HTTP' in from of the URL if it isn't there already }
 function  CheckHttpStart           (CONST URL: string): Boolean;                                 { Check if the URL starts with HTTP }
 function  CheckWwwStart            (CONST URL: string): Boolean;                                 { Check if the URL starts with 'WWW' }
 function  CheckURLStart            (CONST URL: string): Boolean;                                 { Check if the URL starts with HTTP or with www }
 function  CheckURLStartMsg         (CONST URL: string): Boolean;                                 { Check if the URL starts with HTTP or with www }

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
 function  GetLocalIP: string;                                              overload;
 function  GetLocalIP(OUT HostName, IpAddress, ErrorMsg: string): Boolean;  overload;

 function  GetExternalIp  (CONST ScriptAddress: string= 'http://checkip.dyndns.org'): string;
 function  ResolveAddress (CONST HostName: String; out Address: DWORD): Boolean;

 function GenerateInternetRep: string;  // Generate report

{--------------------------------------------------------------------------------------------------
   MAC ADDRESS
--------------------------------------------------------------------------------------------------}
 function CoCreateGuid(var guid: TGUID): HResult; stdcall; far external 'ole32.dll';
 function GetMacAddress: string;


{--------------------------------------------------------------------------------------------------
   Internet EXPLORER
--------------------------------------------------------------------------------------------------}
 function  IE_EnableProxy(const Server: String): Boolean;
 function  IE_DisableProxy: Boolean;
 function  IE_GetProxySettings(OUT ProxyAdr, ProxyPort: string; OUT IsEnabled: boolean): Boolean;
 procedure IE_DeleteCache;
 procedure IE_EndSession;
 procedure IE_SetProxy(CONST Proxy: string);   { Change IE proxy settings globally }
 //How to abort TWeBrowser navigation progress?    http://stackoverflow.com/questions/8976933/how-to-abort-twebrowser-navigation-progress


 {--------------------------------------------------------------------------------------------------
   IS CONNECTED
--------------------------------------------------------------------------------------------------}
 function  PCConnected2Internet: Boolean;                                       { From here: http://www.delphipages.com/forum/showthread.php?t=198159 }

 function  ProgramConnect2Internet: Integer;                                   { Function returns: -1 if computer is not connected to internet, 0 if local system is connected to internet but application is blocked by firewall, 1 if application can connect to internet}
 function  ProgramConnect2InternetS: string;
 function  TestProgramConnection(ShowMsgOnSuccess: Boolean= FALSE): Integer;
 function  IsPortOpened(const Host: string; Port: Integer): Boolean;            { Here's something very simple with which you can check a port status(opened/closed) on remote host. Add WinSock to uses clause}
 //see: c:\MyProjects\Projects INTERNET\Test Internet is connected\ciInternet-is_connected.dpr


{--------------------------------------------------------------------------------------------------
   CREATE .URL FILES
--------------------------------------------------------------------------------------------------}
 Procedure CreateUrl          (CONST FullFileName, sFullURL: string);                       { Creates an .URL file }
 Procedure CreateUrlOnDesktop (CONST ShortFileName, sFullURL: string);




IMPLEMENTATION

USES
   cbAppData, ciHtml, ccIO, ciDownload;


 function  PathIsUrlA; external 'shlwapi' name 'PathIsURLA';
 function  PathIsUrlW; external 'shlwapi' name 'PathIsURLW';




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


function CheckURLStartMsg(CONST URL: string): Boolean;             { Check if the URL starts with HTTP or with www }
begin
 Result:= CheckURLStart(URL);

 if NOT Result
 then MesajWarning('Invalid URL:'+ CRLFw+ URL);
end;





function isurl (CONST s: string): Boolean;  { DEPREACTED USE CheckURLStart }       { Returns True if text starts wit http/www/ftp }
begin
 Result:= CheckHttpStart(s)
       OR CheckWwwStart (s);
end;




function UrlCorrectInvalidChars_(CONST URL, ReplaceWith: string): string;
VAR i: Integer;
begin
 Result:= '';
 for i:= 1 to Length(URL) DO
  begin
   if  CharInSet(URL[I], SeparatorsHTTP)
   OR  (URL[i] < ' ')                                                            { tot ce e sub SPACE }
   then Result:= Result+ ReplaceWith
   else Result:= Result+ URL[i];
  end;
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










function IsWebPage(CONST URL: string): Boolean;   { Returns true for /1/2/3.html but not for /1/2/ or for /1/2 }
VAR
   sURL: string;
   i: Integer;
begin
 if url= '#' then EXIT(TRUE);

 { Some URLs point to a folder (and indirectly to index.html). Example: 'www.test.com/175754/' }
 if LastChar(URL) = '/' then EXIT(TRUE);                      { This also handle this case:  'domain.com/'  }

 if UrlExtractProtAndDomain(URL) = URL
 then EXIT(TRUE);  {IMPORTNAT: this comes after:  if LastChar(URL) = '/'    because it cannot handle 'domain.com/' but only 'domain.com'  }

 { Obtain the last 'thing' in the URL. It could be afile name (ex: /download/setup.zip ) or a folder (ex: /download ) }
 i:= LastPos('/', URL);
 if i > 0
 then sURL:= system.COPY(URL, i+1, MaxInt)
 else sURL:= url;

 { If . not found then it isn't a file }
 if Pos('.', sURL) < 1 then EXIT(TRUE);

 Result:= (PosInsensitive('.htm', sURL) > 1)
       OR (PosInsensitive('.asp', sURL) > 1)
       OR (PosInsensitive('.php', sURL) > 1);
end;





{ All these UrlExtract functions also work with subdomains.   }
{ Removes everyting after the .com but keeps the protocol.

     http://www.stuff.com/img.jpg     -> http://www.stuff.com
     https://sub.500px.org/photo/m%8  -> https://sub.500px.org  }

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

 { Remove http/https }
 if PosInsensitive('http://', Result) > 0
 then Delete(Result, 1, 7)
 else
   if PosInsensitive('https://', Result) > 0
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
 then Result:= ccCore.CopyFrom(Result, '.', maxint, FALSE);
end;



function UrlRemoveStart(CONST URL: string): string;         { http://www.stuff.com/img.jpg  ->  stuff.com/img.jpg }
begin
 Result:= URL;

 if CheckWwwStart(Result)
 then Result:= ccCore.CopyFrom(Result, 'www.', MaxInt, FALSE)
 else
   if (PosInsensitive('http://' , Result) = 1)
   then Result:= ccCore.CopyFrom(Result, 'http://', MaxInt, FALSE)
   else
     if (PosInsensitive('https://' , Result) = 1)
     then Result:= ccCore.CopyFrom(Result, 'https://', MaxInt, FALSE);

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

 { Remove www }
 if PosInsensitive('www.', Result) > 0
 then Delete(Result, 1, 4);
end;



function UrlRemoveHttp(CONST URL: string): string;    { http://www.domain/image.jpg  ->  www.domain/image.jp  }
begin
 Result:= URL;

 { Remove http/https }
 if PosInsensitive('http://', Result) > 0
 then Delete(Result, 1, 7)
 else
   if PosInsensitive('https://', Result) > 0
   then Delete(Result, 1, 8);
end;





{ Returns a Win-compatible filename for this online file.
  The filename will have this format: Domain\Resource.ext.
  The file can be saved to disk under this name.

  Example: http://www.bionixwallpaper.com/help/manual/images/day%20and%20night%20wallpaper.png -> }
function GenerateLocalFilenameFromURL(CONST URL: string; UniqueChars: Integer= 6): string;    // old name: GenerateLocalFilenameFromURL
VAR s: string;
begin
  s:= UrlExtractResource(URL);
  s:= ReplaceString(s, '%20', ' ');
  s:= RemoveFirstChar(s, '/');
  s:= ReplaceCharF(s, '/', '_');

  Result:= ExtractonlyName(s);
  if UniqueChars > 0
  then Result:= Result+ '_'+ GenerateUniqueString(6)+ ExtractFileExt(s)      { Add a unique identifier otherwise two online files might have the same file name }
  else Result:= Result+ ExtractFileExt(s);

  Result:= UrlExtractDomain(URL) + '_'+ Result;
  Result:= CorrectFolder(Result, '_');     { Make it compatible with Win }
end;






function GetReferer(CONST URL: string): string;             { www.cams.de:80/down/Image.php?w=1200 -> www.cams.de/down/ }
begin
 Result:= UrlExtractFilePath(URL);
 Result:= urlremoveport(Result);
end;



function UrlExtractResource(CONST URL: string): string;             { www.cams.de/getImage.php?w=1200 -> /getImage.php }
begin
 Result:= ccCore.CopyTo(URL, Length(UrlExtractProtAndDomain(URL))+ 1, '?', FALSE, TRUE, 1);
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
 then Result:= ccCore.CopyTo(Result, 1, '?', FALSE);   { This fixes this case: worldnow.com/7day_web.jpg?7439232   or   cam_1.jpg?uniq=0.63  }
end;


function CleanServerCommands(CONST URL: string): string;    { FILTER: Remove server commands (everything after '?') from URL  }           { Example: www.pexels.com/1.jpeg?h=350&amp; -> www.pexels.com/1.jpeg }
begin
 if Pos('?', URL) > 0
 then Result:= ccCore.CopyTo(url, 1, '?', FALSE)
 else Result:= url;
end;



function UrlRemovePort(CONST URL: string): string;    { Example www.Domain.com:80 -> www.Domain.com }
VAR iPos: Integer;
begin
 iPos:= Pos(':', URL);
 if ipos > 0
 then
  begin
   if URL[iPos+1]= '/'        { if the URL starts with 'http:\\' then we ignore that column (:) }
   then iPos:= PosEx(':', URL, ipos+1);

   if iPos > 0
   then Result:= system.Copy(url, 1, ipos-1)
   else Result:= url;          { There is no port in this URL }
  end
 else Result:= url;
end;


function UrlExtractPort(CONST URL: string): Integer;    { Example www.Domain.com:80 -> 80 }
VAR
   sURL: string;
   iPos: Integer;
begin
 iPos:= Pos(':', URL);
 if ipos > 0
 then
  begin
   if URL[iPos+1]= '/'        { if the URL starts with 'http:\\' then we ignore that column (:) }
   then iPos:= PosEx(':', URL, ipos+1);

   if iPos > 0
   then
    begin
     sURL:= system.Copy(url, ipos+1, MaxInt);
     iPos:= Pos('/', sURL);
     if iPos > 0
     then sURL:= Copy(sURL, 1, iPos-1);
     Result:= StrToIntDef(sURL, 0);
    end
   else Result:= 0;          { There is no port in this URL }
  end
 else Result:= 0;
end;



{ Extract last folder of a FTP/HTTP path
  NOTE:
    If the path is a folder (does not ends with a filename) then it MUST end with a '/'
  Example:
     htpp://www.server.com/folder1/folder2/file.txt    returns: 'folder2'
     htpp://www.server.com/folder1/folder2/            returns: 'folder2' }
function URLExtractLastFolder(CONST URL: string): string;
VAR
   iPos: Integer;
begin
 iPos:= LastPos('/', URL);
 if iPos < 1 then EXIT('');
 Result:= CopyTo(URL, 1, iPos-1);

 iPos:= LastPos('/', Result);
 if iPos < 1 then EXIT('');
 Result:= system.COPY(Result, iPos+1, High(Integer));
end;


{  Example:
     htpp://www.server.com/folder1/folder2/file.txt    returns: 'folder1'
     htpp://www.server.com/folder1/folder2/            returns: 'folder1' }
function URLExtractPrevFolder(CONST URL: string): string;
VAR
   Pos0, Pos1, Pos2: Integer;
begin
 Pos2:= LastPos('/', URL);
 if Pos2 < 1 then EXIT('');
 Result:= CopyTo(URL, 1, Pos2-1);

 Pos1:= LastPos('/', Result);
 if Pos1 < 1 then EXIT('');
 Result:= system.COPY(Result, 1, Pos1-1);

 Pos0:= LastPos('/', Result);
 if Pos0 < 1 then EXIT('');
 Result:= system.COPY(Result, Pos0+1, Pos1-1);
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


{ Returns true if the aFile is located of MainURL or one of its subfolders.
  Example:
     MainURL= www.test.com/images\/
     Returns True if aFile is:
                        www.test.com/images/1.jpg
                        www.test.com/images/sub/2.jpg
     Returns False if aFile is:
                        www.test.com/art/1.jpg              }
function FileIsInFolder(CONST MainURL, aFile: string): Boolean;
VAR FilePath, s: string;
begin
 FilePath:= UrlExtractFilePath(aFile);
 s:= Copy(FilePath, 1, Length(MainURL));
 Result:= s = MainURL;
end;














 { Convert from Protocol-Relative to http }      { http://stackoverflow.com/questions/9646407/two-forward-slashes-in-a-url-src-href-attribute }
function URLMakeNonRelativeProtocol(CONST URL: string): string;
begin
 if Pos('//', url) = 1
 then Result:= 'http://'+ system.COPY(url, 3, MaxInt)
 else Result:= url;
end;


{ Expand short urls to a full http path.
  Example: If the MainURL is 'www.dnabaser.com/tools/' then 'download/' is expanded to 'www.dnabaser.com/tools/download/' }
function ExpandURL(CONST ShortUrl, MainUrl: string): string;
VAR
   Base: string;
begin
  Assert(ShortUrl > '', 'ExpandURL.ShortUrl is empty!');
  Base:= UrlExtractProtAndDomain(MainUrl);

  { Build full URL }
  if CheckHttpStart(ShortUrl)            { URL is complete? }                //  http://1.bp.blogspot.com/-67xFrZBBFQo/URqHNyxRacI/AAAAAAAABoE/1lliPPXgeIo/s1600/Wallpapers-for-Desktop-.jpg
  then Result:= ShortUrl
  else
   begin
     if FirstCharIs(ShortUrl, '/')
     then Result:= Base + ShortUrl                           { ...no. Add domain in front of the filename }
     else Result:= TrailLinuxPath(Base)+ ShortUrl;           { ...no. Add domain in front of the filename }
   end;
end;


procedure ExpandURLs(ShortUrls: TStringList; CONST MainUrl: string);
VAR
   i: Integer;
   CurURL: string;
begin
  for i:= 0 to ShortUrls.Count-1 DO
   begin
    CurURL:= ShortUrls[i];
    ShortUrls[i]:= ExpandURL(CurURL, MainUrl);
   end;
end;


{ Breaks an URL in all its subcomponents. Example: ParseURL('http://login:password@somehost.somedomain.com/some_path/something_else.html?param1=val&param2=val')   }
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
  ZeroMemory(@lpszScheme      , SizeOf(lpszScheme));
  ZeroMemory(@lpszHostName    , SizeOf(lpszHostName));
  ZeroMemory(@lpszUserName    , SizeOf(lpszUserName));
  ZeroMemory(@lpszPassword    , SizeOf(lpszPassword));
  ZeroMemory(@lpszUrlPath     , SizeOf(lpszUrlPath));
  ZeroMemory(@lpszExtraInfo   , SizeOf(lpszExtraInfo));
  ZeroMemory(@lpUrlComponents , SizeOf(TURLComponents));

  lpUrlComponents.dwStructSize      := SizeOf(TURLComponents);
  lpUrlComponents.lpszScheme        := lpszScheme;
  lpUrlComponents.dwSchemeLength    := SizeOf(lpszScheme);
  lpUrlComponents.lpszHostName      := lpszHostName;
  lpUrlComponents.dwHostNameLength  := SizeOf(lpszHostName);
  lpUrlComponents.lpszUserName      := lpszUserName;
  lpUrlComponents.dwUserNameLength  := SizeOf(lpszUserName);
  lpUrlComponents.lpszPassword      := lpszPassword;
  lpUrlComponents.dwPasswordLength  := SizeOf(lpszPassword);
  lpUrlComponents.lpszUrlPath       := lpszUrlPath;
  lpUrlComponents.dwUrlPathLength   := SizeOf(lpszUrlPath);
  lpUrlComponents.lpszExtraInfo     := lpszExtraInfo;
  lpUrlComponents.dwExtraInfoLength := SizeOf(lpszExtraInfo);

  InternetCrackUrl(PChar(lpszUrl), Length(lpszUrl), ICU_DECODE or ICU_ESCAPE, lpUrlComponents);

  SetLength(Result, 6);
  Result[0]:= lpszScheme;                  { Protocol        (http)              }
  Result[1]:= lpszHostName;                { Host            (www.domain.com)    }
  Result[2]:= lpszUserName;                { User            ('')                }
  Result[3]:= lpszPassword;                { Password        ('')                }
  Result[4]:= lpszUrlPath;                 { Path            ('/download.html')  }
  Result[5]:= lpszExtraInfo;               { ExtraInfo       ('')                }
end;










{--------------------------------------------------------------------------------------------------
   IP TEXT MANIPULATION
--------------------------------------------------------------------------------------------------}
function SplitIpFromAdr(CONST Address: string): string;  { Extracts the only IP from a full IP text.  Example:  For 192.168.0.1:80 it will return '192.168.0.1' }
begin
 Result:= CopyTo(Address, 1, Pos(':', Address)-1)
end;


function IpExtractPort(CONST Address: string): string;     { Example  For 192.168.0.1:80 will retun '80' }
begin
 Result:= Trim(ccCore.CopyFrom(Address, ':', High(Integer), FALSE));
end;


function ExtractIpFrom(CONST aString: string): string;                                             { finds a IP address in a random string. The IP must be like this 192.168.12.234 }
var I: Integer;
begin
 Result := '';
 for I := 1 to Length(AString) DO
   if ((AString[I]>= '0') AND (AString[I]<='9')) OR (AString[I]='.')
   then Result := Result + AString[I];
end;


//ToDo: W521 Return value of function 'ExtractProxyFrom' might be undefined
function ExtractProxyFrom(Line: string): string;   { Tested ok! Extracts an IP from a garbage text. Example: xxxxx1.210.03.23:80xxxx returns: 1.210.03.23:80 }
VAR
  IP, Port: string;
  i, Total, ColumnPos: Integer;
begin

 ColumnPos:= Pos(':', Line);
 if ColumnPos < 1 then EXIT('');
 Line:= RemoveFormatings(Line);

 Total:= 0;
 IP:= SplitIpFromAdr  (Line);

 { Count the '.' three times to the left }
 for i:= Length(IP) downto 1 DO
  begin
    if IP[i]= '.' then Inc(Total);
    if Total = 3 then Break;
  end;

 if Total< 3 then EXIT('');

 { Find first char which is not number }
 i:= i-1;                                   { Jump on the left side of the '.' }
 if i >= 1 then                             { Here I must keep the = sign else it won't work correctly when the IP is like this:  1.2.3.4 }
   REPEAT
     Dec(i);
   UNTIL (i= 0) OR NOT CharIsNumber(IP[i]);

 IP:= ccCore.CopyTo(IP, i+1, High(Integer));

 Port:= IpExtractPort(Line);
 if Port = '' then EXIT;
 i:= 0;
 REPEAT
   Inc(i);
 UNTIL (i > Length(Port)) OR NOT CharIsNumber(Port[i]);
 Port:= ccCore.CopyTo(Port, 1, i-1);


 if (IP > '') AND (Port > '')
 then Result:= IP+ ':'+ Port   //ValidateIpAddress(IP) + ':'+ ValidatePort(Port);
 else Result:= '';   // ????????????????
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


function CollectIPAddress(CONST HTMLBody: string): string; { WTF ? }
CONST DELIMITER = ':';
begin
 Result:= Trim(system.COPY(HTMLBody, Pos(DELIMITER, HTMLBody)+1, Length(HTMLBody) ));
end;








function ServerStatus2String(Status: Integer): string;
begin
 case Status of
   // Informational 1xx
   100: Result:= 'Continue';
   101: Result:= 'Switching Protocols';
   // Successful 2xx
   200: Result:= 'OK';
   201: Result:= 'Created';
   202: Result:= 'Accepted';
   203: Result:= 'Non-Authoritative Information';
   204: Result:= 'No Content';
   205: Result:= 'Reset Content';
   206: Result:= 'Partial Content';
   // Redirection 3xx
   300: Result:= 'Multiple Choices';
   301: Result:= 'Moved Permanently';
   302: Result:= 'Moved Temporarily';
   303: Result:= 'See Other';
   304: Result:= 'Not Modified';
   305: Result:= 'Use Proxy';
   // Client Error 4xx
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
   413: Result:= 'Request Entity Too Large';
   414: Result:= 'Request-URI Too Long';
   415: Result:= 'Unsupported Media Type ';
   // Server Error 5xx
   500: Result:= 'Internal Server Error';
   501: Result:= 'Not Implemented';
   502: Result:= 'Bad Gateway';
   503: Result:= 'Service Unavailable';
   504: Result:= 'Gateway Timeout';
   505: Result:= 'HTTP Version Not Supported';
  else Result:= IntToStr(Status);
 end;
end;




{--------------------------------------------------------------------------------------------------
   IP VALIDATION
--------------------------------------------------------------------------------------------------}
function ValidateIpAddress(CONST Address: string): Boolean;
CONST
    Valid = ['0'..'9', '.'];
VAR
   I, J, Point: Integer;
   sAddress, W: string;
begin
 sAddress:= Trim(Address);
 if sAddress= '' then EXIT(FALSE);

 if (Length(sAddress) > 15) OR (sAddress[1] = '.') then EXIT(FALSE);

 I := 1;
 J := 0;
 Point := 0;
 W := '';

 REPEAT
   if CharInSet(sAddress[I], Valid) and (J < 4)
   then
    begin
      if sAddress[I] = '.' then
      begin
        Inc(Point);
        J := 0;
        TRY
          StrToInt(sAddress[I + 1]);
        except
          //todo 1: trap only specific exceptions
          EXIT(FALSE);
        END;
        W := '';
      end
     else
       begin
        W := W + sAddress[I];
        if (StrToInt(W) > 255) or (Length(W) > 3) then EXIT(FALSE);
        Inc(J);
       end;
    end
   else EXIT(FALSE);

   Inc(I);
 UNTIL I > Length(sAddress);

 Result:= Point= 3;                                                                                { Change sign to =3 to enforce loose Address address checking }
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
















function GetConnectionStatus_DEL(ErrorCode: Integer): string;                                      {DEL}
begin
 CASE ErrorCode of
   1: Result:= '';
   0: Result:= 'Unknown error';
  -1: Result:= 'No Internet connection';
  -2: Result:= 'Connection lost';
  -3: Result:= 'File not found/File cannot be downloaded';
  -4: Result:= 'Cannot open Internet connection';
 end;
end;



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
    Result:= ciDownload.GetTextFile('http://www.google.com/', '');
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
            1 if application can connect to internet}
function ProgramConnect2Internet: Integer;
begin
 if PCConnected2Internet
 then
   if ciDownload.GetTextFile('http://www.google.com/', '') > ''                                   { 1 = Can connect to internet, 0 = blocked by firewall, -1 = PC not connected to Internet }
   then Result := 1
   else Result := 0
 else
   Result := -1;
end;


 {
function ProgramConnectMsg(CONST Connection: Integer): string;                                     { same as ProgramConnect2Internet but returns a string
begin
 case Connection of
  -1: Result:= ComputerCannotAccessInet;
   0: Result:= CheckYourFirewallMsg;
  +1: Result:= 'Successfully connected to Internet';
 end;
end; }



{ if ShowMsgOnSuccess = fasle then show message ONLY if it cannot connect to Internet }
function TestProgramConnection(ShowMsgOnSuccess: Boolean= FALSE): Integer;                                        { Old name: ProgramConnectMsg }
begin
 Result:= ProgramConnect2Internet;
 case Result of
  -1: MesajWarning(ComputerCannotAccessInet);
   0: MesajError(CheckYourFirewallMsg);
  +1: if ShowMsgOnSuccess
      then MesajInfo('Successfully connected to Internet');
 end;
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
    WSACleanup; // Ensure cleanup happens
  end;
end;



{ Other IP providers= 'http://support.inmotionhosting.com/ipcheck.php' }
function GetExternalIp(CONST ScriptAddress: string= 'http://checkip.dyndns.org'): string;
begin
  Result:= GetTextFile(ScriptAddress);
  if Length(Result) = 0 then EXIT;

  Result:= ExtractIpFrom(GetBodyFromHtml(Result));
  if Length(Result) = 0 then EXIT;

  { Remove possible garbage }
  Result:= ReplaceString(Result, CR, '');   // We don't know the type of enter so we need to remove them both
  Result:= ReplaceString(Result, LF, '');
  Result:= System.SysUtils.Trim(Result);
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





{==================================================================================================
   .URL
==================================================================================================}
Procedure CreateUrl(CONST FullFileName, sFullURL: string);                                            { create a URL file - The filename should end in .URL }
begin
  with TIniFile.Create(FullFileName) DO
   TRY
     WriteString('InternetShortcut', 'URL', sFullURL);
   FINALLY
     Free;
   END;
end;


Procedure CreateUrlOnDesktop(CONST ShortFileName, sFullURL: string);                               { create a URL file - The filename should end in .URL }
VAR sDesktop: string;
    MyReg   : TRegIniFile;
begin
 { Get desktop folder }
 MyReg:= TRegIniFile.Create('Software\MicroSoft\Windows\CurrentVersion\Explorer');
 sDesktop:= MyReg.ReadString('Shell Folders','Desktop','');
 FreeAndNil(MyReg);

 { Write URL file }
 CreateUrl(Trail(sDesktop)+ ShortFileName, sFullURL);
end;





{==================================================================================================
   URL
==================================================================================================}

function UrlEncode(CONST URL: string): string;         { It also fixes the Indy encoding issue.   http://stackoverflow.com/questions/5708863/indy-is-altering-the-binary-data-in-my-url }
VAR
   i: Integer;
CONST
   UnsafeChars = ['*', '#', '%', '<', '>', ' ', '[', ']', '\', '@'];
begin
  Result := '';
  for i := 1 to Length(URL) DO
    if  (URL[i]> #32)
    AND (URL[i]<= #128)       { € = char #128}
    AND (NOT CharInSet(URL[i], UnsafeChars))
    then Result := Result + URL[i]
    else Result := Result + '%' + IntToHex(Ord(URL[i]), 2);
end;


















{--------------------------------------------------------------------------------------------------
                                  Internet EXPLORER
---------------------------------------------------------------------------------------------------

Question:
        How do I set the proxy settings in Internet Explorer without having to restart IE to make it load the settings
Answer:
        It's not the best solution, this I know, but it worked for me, and I couldn't get the BEST solution to work.

How to use it:
   You can simply execute EnableProxy('proxyserver:8080') to set a global proxy
   or you can execute EnableProxy('ftp=ftpproxyserver:2121;gopher=goproxyserver:3333;http=httpproxyserver:8080;https=httpsproxyserver:8080');
   you can of course only fill in one of the options like this EnableProxy('http=httpproxyserver:8080');

Required units:
               WinInet, Registry

Source:
       http://www.delphi3000.com/articles/article_3138.asp

Better way:
       O metoda mult mai buna e aici:   http://www.naddalim.com/forum/showthread.php?t=1454        }


procedure IE_ApplySettings;
VAR HInet: HINTERNET;
begin
  hInet:= InternetOpen(PChar(AppData.AppName), INTERNET_OPEN_TYPE_DIRECT, nil, nil, INTERNET_FLAG_OFFLINE);
  TRY
    if hInet <> NIL
    then InternetSetOption(hInet, INTERNET_OPTION_SETTINGS_CHANGED, nil, 0);
  FINALLY
    InternetCloseHandle(hInet);
  END;
end;


function IE_EnableProxy(const Server: String): Boolean;
VAR Reg : TRegistry;
begin
  Reg:= TRegistry.Create;
  TRY
   TRY
    Reg.RootKey:= HKEY_CURRENT_USER;
    Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Internet Settings', FALSE);
    Reg.WriteString('ProxyServer', Server);
    Reg.WriteBool('ProxyEnable', True);
    Reg.CloseKey;
    Result:= TRUE;
  FINALLY
   FreeAndNil(Reg);
  END;
 except                                                                                            { pe unele sisteme nu pot sa deschid cheia asta si imi ridica o eroare, asa ca folosesc un except }
  //todo 1: trap only specific exceptions
  Result:= FALSE;
 END;

 { InternetSetOption(NIL, INTERNET_OPTION_SETTINGS_CHANGED, NIL, 0);           <--  asa era original insa se pare... }
 if Result then IE_ApplySettings;                                                                  { ...ca metoda asta jos e mai buna }
end;


function IE_DisableProxy: Boolean;
VAR Reg : TRegistry;
begin
  Reg:= TRegistry.Create;
  TRY
   TRY
    Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Internet Settings', False);
    Reg.WriteBool('ProxyEnable', False);
    Reg.CloseKey;
    Result:= TRUE;
  FINALLY
   FreeAndNil(Reg);
  END;
 except                                                                                            { pe unele sisteme nu pot sa deschid cheia asta si imi ridica o eroare, asa ca folosesc un except_ }
  //todo 1: trap only specific exceptions
  Result:= FALSE;
 END;

 { InternetSetOption(NIL, INTERNET_OPTION_SETTINGS_CHANGED, NIL, 0);            <-- asa era original insa se pare... }
 if Result then IE_ApplySettings;                                                                  { ...ca metoda asta jos e mai buna }
end;





{
 READ IE PROXY SETTINGS

 DOCS:
      This is easy and fast way but note that if in future the Microsoft will
      change a key where proxy stored, you must change your code too. For a more
      complicated way, see the link below (solution based on WinInet library).

 Sursa:
      http://www.scalabium.com/faq/dct0161.htm                                  }

function IE_GetProxySettings(out ProxyAdr, ProxyPort: string; out IsEnabled: boolean): Boolean;
VAR Reg : TRegistry;
    ProxyServer: string;

{sub}procedure ParseAndBreak();
     VAR i, j: Integer;
     begin
      if (ProxyServer <> '') then
       begin
         { Extract the HTTP part }
         i:= PosInsensitive('http=', ProxyServer);
         if (i > 0) then
          begin
           Delete(ProxyServer, 1, i+5);
           j:= Pos(';', ProxyServer);
           if (j > 0)
           then ProxyServer:= system.COPY(ProxyServer, 1, j-1);
          end;

         { Break into address and port }
         i:= Pos(':', ProxyServer);
         if (i > 0) then
          begin
           ProxyPort := system.COPY(ProxyServer, i+1, Length(ProxyServer)-i);
           ProxyAdr  := system.COPY(ProxyServer,   1, i-1)
          end
       end;
     end;

begin
 Reg:= TRegistry.Create;
 TRY
  TRY
   Reg.RootKey:= HKEY_CURRENT_USER;
   Result:= Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Internet Settings', FALSE)
        AND Reg.ValueExists('ProxyEnable');

   if Result then
    begin
     ProxyServer:= Reg.ReadString ('ProxyServer');
     IsEnabled  := Reg.ReadBool   ('ProxyEnable');
     ParseAndBreak;                                                                                { PARSE }
    end;
   Reg.CloseKey;
  FINALLY
   FreeAndNil(Reg);
  END;
 except                                                                                            { pe unele sisteme nu pot sa deschid cheia asta si imi ridica o eroare, asa ca folosesc un except }
  //todo 1: trap only specific exceptions
  ProxyAdr:= 'Cannot auto-detect proxy settings.';
  Result:= FALSE;
 END;
end;




procedure setProxy(CONST ProxyIP: string);
VAR
   IP: AnsiString;
   PIInfo: PInternetProxyInfo;
begin
 IP:= AnsiString(Trim(ProxyIP));
 New(PIInfo);
 PIInfo^.dwAccessType := INTERNET_OPEN_TYPE_PROXY;
 PIInfo^.lpszProxy:= PAnsiChar(IP);
 PIInfo^.lpszProxyBypass := PAnsiChar('');
 Winapi.UrlMon.UrlMkSetSessionOption(INTERNET_OPTION_PROXY, piinfo, SizeOf(Internet_Proxy_Info), 0);
 Dispose(PIInfo);
end;


procedure IE_DeleteCache;
var
  lpEntryInfo: PInternetCacheEntryInfo;
  hCacheDir: LongWord;
  dwEntrySize: LongWord;
begin
  dwEntrySize := 0;
  FindFirstUrlCacheEntry(nil, TInternetCacheEntryInfo(nil^), dwEntrySize);
  GetMem(lpEntryInfo, dwEntrySize);
  if dwEntrySize > 0
  then lpEntryInfo^.dwStructSize := dwEntrySize;
  hCacheDir := FindFirstUrlCacheEntry(nil, lpEntryInfo^, dwEntrySize);
  if hCacheDir <> 0 then
    REPEAT
      DeleteUrlCacheEntry(lpEntryInfo^.lpszSourceUrlName);
      FreeMem(lpEntryInfo, dwEntrySize);
      dwEntrySize := 0;
      FindNextUrlCacheEntry(hCacheDir, TInternetCacheEntryInfo(nil^), dwEntrySize);
      GetMem(lpEntryInfo, dwEntrySize);
      if dwEntrySize > 0 then lpEntryInfo^.dwStructSize := dwEntrySize;
    UNTIL NOT FindNextUrlCacheEntry(hCacheDir, lpEntryInfo^, dwEntrySize);
  FreeMem(lpEntryInfo, dwEntrySize);
  FindCloseUrlCache(hCacheDir);
end;


procedure IE_EndSession;
begin
 InternetSetOption(NIL, INTERNET_OPTION_END_BROWSER_SESSION, NIL, 0);
end;

// Change IE proxy settings globally:          http://stackoverflow.com/questions/12732843/authentification-on-http-proxy-in-delphi-xe/21445091#21445091
procedure IE_SetProxy(CONST Proxy: string);
begin
 IE_DeleteCache;
 IE_EndSession;
 SetProxy(Proxy);
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
      Address:= inet_addr(PAnsiChar(AnsiHostName));                  // Try converting the hostname  // In D7 aici a fost PChar
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
  // initialize result
  Result := False;
  // create WinSocketData
  if Winapi.WinSock.WSAStartup(MakeWord(1, 1), WinSocketData) = 0 then
  begin
    // set address family
    Address.sin_family := AF_INET;
    // try to translate Host to IP address
    if NOT ResolveAddress(Host, dwAddress) then
      // faild! go to lClean label
      goto lClean;
    // set the address
    Address.sin_addr.S_addr := dwAddress;
    // create a socket
    Socket := Winapi.WinSock.Socket(AF_INET, SOCK_STREAM, IPPROTO_IP);
    // if faild to create socket
    if Socket = INVALID_SOCKET then
      // go to lClean label
      goto lClean;
    // set the port
    Address.sin_port := Winapi.WinSock.htons(Port);
    // attempt remote connection to Host on Port
    if Winapi.WinSock.Connect(Socket, Address, szSockAddr) = 0 then
     begin
      Result := True;
      // close the socket
      Winapi.WinSock.closesocket(Socket);
     end;// if WinSock.Connect(Socket, Address, szSockAddr) = 0 then begin
  end;// if WinSock.WSAStartup(MakeWord(1, 1), WinSocketData) = 0 then begin
  // label to which we jump to clean up
  lClean:
    Winapi.WinSock.WSACleanup;
end;


{HOW TO USE IT:

if IsPortOpened('google.com', 80) then
  ShowMessage('google has port 80 opened')
else
  ShowMessage('google has port 80 closed???');
}




{
 Similar WinInet resources:
   http://www.delphipages.com/threads/thread.cfm?ID=100717&G=100706
   http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_23867246.html#a22860727
   http://www.naddalim.com/forum/showthread.php?t=1454

 IdHTTP proxy:
   http://delphi.newswhat.com/geoxml/forumhistorythread?groupname=borland.public.delphi.non-technical&messageid=3f43efbc$1@newsgroups.borland.com
}









{--------------------------------------------------------------------------------------------------
                                  GET MAC
--------------------------------------------------------------------------------------------------}
function GetMacAddress: string;
var
  g: TGUID;
  i: Byte;
begin
  Result := '';
  CoCreateGUID(g);
  for i := 2 to 7 do
    Result := Result + IntToHex(g.D4[i], 2);
end;


end.
