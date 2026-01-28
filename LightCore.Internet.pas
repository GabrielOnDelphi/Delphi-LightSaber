UNIT LightCore.Internet;

{-------------------------------------------------------------------------------------------------------------
   2025
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
   System.SysUtils, System.StrUtils, System.Classes, System.IniFiles,
   LightCore;


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


{--------------------------------------------------------------------------------------------------
   CREATE .URL FILES
--------------------------------------------------------------------------------------------------}
 Procedure CreateUrl    (CONST FullFileName, sFullURL: string);      { Creates an .URL file }



IMPLEMENTATION

USES

   //LightVcl.Visual.AppData,
   LightCore.HTML, LightCore.IO, LightCore.Download;




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



{ Removes port from URL. Example: www.Domain.com:80 -> www.Domain.com }
function UrlRemovePort(CONST URL: string): string;
VAR iPos: Integer;
begin
  iPos:= Pos(':', URL);
  if iPos > 0
  then
   begin
    { Skip the colon in 'http://' or 'https://' }
    if (iPos + 1 <= Length(URL)) AND (URL[iPos + 1] = '/')
    then iPos:= PosEx(':', URL, iPos + 1);

    if iPos > 0
    then Result:= System.Copy(URL, 1, iPos - 1)
    else Result:= URL;  { No port in this URL }
  end
   else Result:= URL;
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
    else Result:= 0;  { No port in this URL }
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







{--------------------------------------------------------------------------------------------------
   IP TEXT MANIPULATION
--------------------------------------------------------------------------------------------------}
function SplitIpFromAdr(CONST Address: string): string;  { Extracts the only IP from a full IP text.  Example:  For 192.168.0.1:80 it will return '192.168.0.1' }
begin
 Result:= CopyTo(Address, 1, Pos(':', Address)-1)
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
  Example: "xxxxx1.210.03.23:80xxxx" returns "1.210.03.23:80"
  Returns empty string if no valid proxy found. }
function ExtractProxyFrom(Line: string): string;
VAR
  IP, Port: string;
  i, Total, ColumnPos: Integer;
begin
  Result:= '';  { Initialize to prevent undefined return }

  ColumnPos:= Pos(':', Line);
  if ColumnPos < 1 then EXIT;

  Line:= RemoveFormatings(Line);
  IP:= SplitIpFromAdr(Line);

  { Count the dots - need exactly 3 for IPv4 }
  Total:= 0;
  for i:= Length(IP) downto 1 do
  begin
    if IP[i] = '.'
    then Inc(Total);
    if Total = 3
    then Break;
  end;

 if Total< 3 then EXIT('');

  { Find first char which is not a number (scanning left from the third dot) }
  i:= i - 1;  { Jump to the left side of the '.' }
  if i >= 1 then 

    while (i > 0) AND CharIsNumber(IP[i]) do
      Dec(i);


  IP:= LightCore.CopyTo(IP, i + 1, High(Integer));

  { Extract port }
  Port:= IpExtractPort(Line);
  if Port = '' then EXIT;

  { Find end of port number }
  i:= 1;
  while (i <= Length(Port)) AND CharIsNumber(Port[i]) do
    Inc(i);
  Port:= LightCore.CopyTo(Port, 1, i - 1);

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





{ Other IP providers= 'http://support.inmotionhosting.com/ipcheck.php' }
function GetExternalIp(CONST ScriptAddress: string= 'http://checkip.dyndns.org'): string;
begin
  Result:= DownloadAsString(ScriptAddress);
  if Length(Result) = 0 then EXIT;

  Result:= ExtractIpFrom(GetBodyFromHtml(Result));
  if Length(Result) = 0 then EXIT;

  { Remove possible garbage }
  Result:= ReplaceString(Result, CR, '');   // We don't know the type of enter so we need to remove them both
  Result:= ReplaceString(Result, LF, '');
  Result:= System.SysUtils.Trim(Result);
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


function UrlEncode(CONST URL: string): string;         { It also fixes the Indy encoding issue.   http://stackoverflow.com/questions/5708863/indy-is-altering-the-binary-data-in-my-url }
VAR
   i: Integer;
CONST
   UnsafeChars = ['*', '#', '%', '<', '>', ' ', '[', ']', '\', '@'];
begin
  Result := '';
  for i := 1 to Length(URL) DO
    if  (URL[i]> #32)
    AND (URL[i]<= #128)       { � = char #128}
    AND (NOT CharInSet(URL[i], UnsafeChars))
    then Result := Result + URL[i]
    else Result := Result + '%' + IntToHex(Ord(URL[i]), 2);
end;


end.
