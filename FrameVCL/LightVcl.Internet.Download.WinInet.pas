UNIT LightVcl.Internet.Download.WinInet;

{-------------------------------------------------------------------------------------------------------------
   2025.05
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   DOWNLOADS A FILE FROM THE INTERNET
   Uses WinINet
--------------------------------------------------------------------------------------------------------------
    Maturity: A very old and stable Windows API.
    Control: Offers low-level control if needed.
    Windows Specific: Not portable.
    Complexity: More verbose and requires manual management of many aspects (handles, buffers, headers).
    Reliability: Can be very reliable but is also more susceptible to system-level IE/WinINet configurations or policies affecting its behavior. SSL/TLS capabilities depend on the OS version and its SChannel configuration.
--------------------------------------------------------------------------------------------------------------

   ISSUES
     - Slow: WinHTTP is much faster than WinINet! More than 10x faster, at least for multiple connections. Here: http://blog.synopse.info/post/2011/07/04/WinINet-vs-WinHTTP
     - CPU: Because of the REPEAT loop the CPU goes to 100%.
     - Freeze: The program does not exit until the download is complete!

   ALSE SEE:
       c:\Users\Public\Documents\Embarcadero\Studio\21.0\Samples\Object Pascal\RTL\HttpDownload\HttpDownloadDemo.dpr
       c:\MyProjects\Packages\BSalsa EmbeddedWB\Demos\IEDownload_Simple_Demo\

   Tester:
       c:\Projects\LightSaber\Demo\Demo Internet\
       c:\Projects\Testers\Internet download tester images\

   https://stackoverflow.com/questions/6565567/which-is-the-common-average-buffer-size-for-internetreadfile

--------------------------------------------------------------------------------------------------------------

  Alternatives:
     LightVcl.Internet, LightCore.InternetDIndy.pas - OK
     HTTPGet.pas
     UrlMon  -  UrlMon.UrlDownloadToFile (nil, <url>, <file destination path (PChar)>, 0, nil);  Don't know if it is thread-safe, UrlMon.pas is just an interface for calling UrlMon.dll, a Microsoft API. And yes, is a Borland unit \delphi6\source\rtl\Win\UrlMon.pas
     mORMot  - TSQLHttpClientWinSock.InternalReques in mORMotHttpClient.pas. But that function is not almost impossible to extract from that library

  Documentation:
     Post Request -  http://stackoverflow.com/questions/2977720/how-to-send-a-http-post-request-in-delphi-2010-using-wininet
     UNICODE      -  http://stackoverflow.com/questions/1847238/reading-web-pages-unicode
     MS API       -  http://msdn.microsoft.com/en-us/library/windows/desktop/aa385098%28v=vs.85%29.aspx
     TimeOut      -  http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Internet/Q_24986068.html
     Cryer        -  http://www.cryer.co.uk/brian/delphi/wininet.htm#InternetOpenUrl
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
   Winapi.Windows, WinApi.WinInet, Winapi.Messages,
   System.Classes, System.SysUtils; // System.Net.HttpClient;

CONST
   USER_AGENT_APP = 'DelphiApp/1.0 (Compatible; +http://GabrielMoraru.com)';
   USER_AGENT_MOZ = 'Mozilla/5.0 (compatible, MSIE 11, Windows NT 6.3; Trident/7.0; rv:11.0) like Gecko';



// Silent. Does not raises exceptions on download error
function DownloadAsString  (CONST URL: string; Referer: string= ''): string;

function DownloadBytes     (CONST Url, Referer: String; OUT Data: TBytes; PostData: String= ''; SSL: Boolean = FALSE): Cardinal;  overload;  { TESTED OK }
function DownloadToFile    (CONST URL, Referer, DestinationFile: string): Cardinal;                   overload;    { It can be used with text or binary files }


IMPLEMENTATION

USES
   LightCore, LightVcl.Common.Dialogs, LightCore.IO, LightCore.TextFile,
   LightCore.Internet, LightVcl.Internet;


//Hint: use TIdURI.URLEncode() in IdURI.pas to encode an URL.

{-----------------------------------------------------------------------------------------------------------------------
   Allows you to set referers, user agents, and other stuff.
   Source:
          http://stackoverflow.com/questions/1823542/how-to-send-a-http-post-request-in-delphi-using-wininet-api
          http://stackoverflow.com/questions/2977720/how-to-send-a-http-post-request-in-delphi-2010-using-wininet

   Returns zero on success
   If the URL is invalid, probably it will return the content of the 404 page (if the server automatically returns a 404 page).

   Header:
      Accept:
         Tells server what we accept:
         To accept text use: "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
         https://stackoverflow.com/questions/71188925/try-to-download-image-from-image-url-but-i-get-an-html-instead/71196534#71196534
         https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept
-----------------------------------------------------------------------------------------------------------------------}

//Hint: use TIdURI.URLEncode() in IdURI.pas to encode an URL.
function DownloadBytes(CONST Url, Referer: String; OUT Data: TBytes; PostData: String= ''; SSL: Boolean = FALSE): Cardinal;   { TESTED OK }
VAR
  Buffer     : array[0..High(Word)*4] of Byte; { Buffer of 260KB }
  TempBytes  : TBytes;
  sMethod    : string;
  BytesRead  : Cardinal;
  pSession   : HINTERNET;
  pConnection: HINTERNET;
  pRequest   : HINTERNET;
  Resource   : string;
  Root       : string;
  port       : Integer;
  flags      : DWord;
  Header     : string;
begin
  Result := WinApi.Windows.ERROR_SUCCESS;  // 0 indicates success
  SetLength(Data, 0);

  pSession := InternetOpen(nil {USER_AGENT}, INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if not Assigned(pSession) then
  begin
    Result := GetLastError();
    Exit;
  end;

  try
    { Autodetect port }
    port:= UrlExtractPort(URL);
    if port = 0 then
      if SSL
      then Port := INTERNET_DEFAULT_HTTPS_PORT
      else Port := INTERNET_DEFAULT_HTTP_PORT;

    { Root }
    Root:= UrlExtractDomainRelaxed(Url);
    if Root = '' then
    begin
      Result := ERROR_INTERNET_UNRECOGNIZED_SCHEME;
      Exit;
    end;

    pConnection := InternetConnect(pSession, PWideChar(Root), port, nil, nil, INTERNET_SERVICE_HTTP, 0, 0); { The second parameter of InternetConnect should contain only the name of the server, not the entire URL of the server-side script. }
    if not Assigned(pConnection) then
    begin
      Result := GetLastError();
      Exit;
    end;

    TRY
      if PostData = ''
      then sMethod := 'GET'
      else sMethod := 'POST';

      if SSL
      then flags := INTERNET_FLAG_SECURE  OR INTERNET_FLAG_KEEP_CONNECTION
      else flags := INTERNET_SERVICE_HTTP OR INTERNET_FLAG_RELOAD; // INTERNET_FLAG_RELOAD= Forces a download of the requested file, object, or directory listing from the origin server, not from the cache.; *)

      Resource := UrlExtractResourceParams(Url);  { I also need to keep the server parameters (stuff after '?') because in this case it specifies the image resolution: http://cams.sr-online.de/cgi-bin/getImage.php?w=1200 . Without 1200 I will retrieve file at 320x240 resolution }
      if Resource = ''
      then Resource := '/';

      pRequest:= HTTPOpenRequest(pConnection, PWideChar(sMethod), PWideChar(Resource), nil, nil, nil, flags, 0);  { The third parameter of HttpOpenRequest is the file name (URL) of the script }
      if not Assigned(pRequest) then
      begin
        Result := GetLastError();
        Exit;
      end;

      TRY
        Header := '';
        if Referer <> ''
        then Header := Header + 'Referer: ' + Referer + sLineBreak;
        Header := Header + 'User-Agent: Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:47.0) Gecko/20100101 Firefox/47.0' + sLineBreak;
        Header := Header + 'Accept: */*' + sLineBreak;
        Header := Header + 'Accept-Language: en-us,en;q=0.5' + sLineBreak;
        Header := Header + 'Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7' + sLineBreak;
        Header := Header + 'Keep-Alive: 70' + sLineBreak;
        Header := Header + 'Connection: keep-alive' + sLineBreak + sLineBreak;
        HttpAddRequestHeaders(pRequest, PWideChar(Header), Length(Header), HTTP_ADDREQ_FLAG_ADD);

        if not HTTPSendRequest(pRequest, nil, 0, Pointer(PostData), Length(PostData)) then
         begin
          Result := GetLastError();
          Exit;
         end;

        repeat
          ZeroMemory(@Buffer, SizeOf(Buffer));
          if not InternetReadFile(pRequest, @Buffer, SizeOf(Buffer), BytesRead) then
          begin
            Result := GetLastError();
            SetLength(Data, 0);
            Exit;
          end;

          if BytesRead = 0 then Break;

          SetLength(TempBytes, BytesRead);
          Move(Buffer[0], TempBytes[0], BytesRead);
          Data := Data + TempBytes;
        UNTIL BytesRead = 0;

      finally
        InternetCloseHandle(pRequest);
      end;
    finally
      InternetCloseHandle(pConnection);
    end;
  finally
    InternetCloseHandle(pSession);
  end;
end;


function DownloadToFile(CONST URL, Referer, DestinationFile: string): Cardinal;  { TESTED OK }
VAR BinData: TBytes;
begin
  if NOT ForceDirectoriesB(ExtractFilePath(DestinationFile))
  then raise Exception.Create('Cannot create directory: '+ ExtractFilePath(DestinationFile));

  Result:= DownloadBytes(URL, Referer, BinData);
  if Result = ERROR_SUCCESS
  then LightCore.IO.BytesToFile(DestinationFile, BinData, TRUE);
end;



{ Returns the downloaded file as string. Use it for HTML files }
function DownloadAsString(CONST URL: string; Referer: string= ''): string;     { TESTED OK }
VAR BinData: TBytes;
begin
 if DownloadBytes(URL, Referer, BinData) = 0
 then Result:= StringOf(BinData)
 else Result:= '';
end;





end.

