UNIT ciDownload;

{-------------------------------------------------------------------------------------------------------------
   Gabriel Moraru
   2024.05
   See Copyright.txt

   DOWNLOAD A FILE FROM THE INTERNET
   Alternative: ciInetDonwIndy.pas

   Uses WinInet (slow!) and System.Net.HttpClient.

   ISSUES
     - Slow: WinHTTP is much faster than WinINet !! More than 10x faster, at least for multiple connections. Here: http://blog.synopse.info/post/2011/07/04/WinINet-vs-WinHTTP
     - CPU: Because of the REPEAT loop the CPU goes to 100%.
     - Freeze: The program does not exit until the download is complete!!


   ALSE SEE:
       c:\Users\Public\Documents\Embarcadero\Studio\21.0\Samples\Object Pascal\RTL\HttpDownload\HttpDownloadDemo.dpr
       c:\MyProjects\Packages\BSalsa EmbeddedWB\Demos\IEDownload_Simple_Demo\

   Tester:
       c:\MyProjects\Project Testers\Internet download tester\
       c:\MyProjects\Project Testers\Internet download tester images\

   https://stackoverflow.com/questions/6565567/which-is-the-common-average-buffer-size-for-internetreadfile

--------------------------------------------------------------------------------------------------------------

  Alternatives:
     ciInternetDIndy.pas - OK
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
   Winapi.Windows, WinApi.WinInet, System.Classes, System.SysUtils, System.Net.HttpClient;


{ GetBinFile / GetBinFileW  <- Replaced with DownloadFile }
function GetTextFile  (CONST URL: string; Referer: string= ''): string;                                   overload;
function GetTextFile  (CONST URL, Referer, DestinationFile: String; ForceFolder: Boolean= TRUE): Boolean; overload;

function DownloadFile (CONST URL, Referer: String; OUT Data: TBytes; PostData: String= ''; SSL: Boolean = FALSE): Boolean;  overload;    { It can be used with text or binary files }
function DownloadFile (CONST URL, Referer, DestinationFile: string; ForceFolder: Boolean= TRUE): Boolean;                            overload;    { It can be used with text or binary files }


function DDownloadFile (CONST URL: string; OUT HttpRetCode: Integer): TMemoryStream; overload;
function DDownloadFile (CONST URL, SaveTo: string): Boolean; overload;


IMPLEMENTATION

USES
   ccCore, cbDialogs, ccIO, ciInternet;



{-----------------------------------------------------------------------------------------------------------------------
   Allows you to set referers, user agents, and other stuff.
   Source:
          http://stackoverflow.com/questions/1823542/how-to-send-a-http-post-request-in-delphi-using-wininet-api
          http://stackoverflow.com/questions/2977720/how-to-send-a-http-post-request-in-delphi-2010-using-wininet

   Returns false if the Internet connection failed.
   If the URL is invalid, probably it will return the content of the 404 page (if the server automatically returns a 404 page).

   Header:
      Accept:
         Tells server what we accept:
         To accept text use: "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
         https://stackoverflow.com/questions/71188925/try-to-download-image-from-image-url-but-i-get-an-html-instead/71196534#71196534
         https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept
-----------------------------------------------------------------------------------------------------------------------}

function DownloadFile(CONST Url, Referer: String; OUT Data: TBytes; PostData: String= ''; SSL: Boolean = FALSE): Boolean;   { TESTED OK }
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
  Result := FALSE;
  SetLength(Data, 0);
  pSession := InternetOpen(nil {USER_AGENT}, INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

  if Assigned(pSession) then
  TRY
    { Autodetect port }
    port:= UrlExtractPort(URL);
    if port = 0 then
      if SSL
      then Port := INTERNET_DEFAULT_HTTPS_PORT
      else Port := INTERNET_DEFAULT_HTTP_PORT;

    { Root }
    Root:= UrlExtractDomainRelaxed(Url);
    pConnection := InternetConnect(pSession, PWideChar(Root), port, nil, nil, INTERNET_SERVICE_HTTP, 0, 0); { The second parameter of InternetConnect should contain only the name of the server, not the entire URL of the server-side script. }

    if Assigned(pConnection) then
    TRY
      if (PostData = '')
      then sMethod := 'GET'
      else sMethod := 'POST';

      if SSL
      then flags := INTERNET_FLAG_SECURE  OR INTERNET_FLAG_KEEP_CONNECTION
      else flags := INTERNET_SERVICE_HTTP OR INTERNET_FLAG_RELOAD; // INTERNET_FLAG_RELOAD= Forces a download of the requested file, object, or directory listing from the origin server, not from the cache.;

      Resource := UrlExtractResourceParams(Url);  { I also need to keep the server parameters (stuff after '?') because in this case it specifies the image resolution: http://cams.sr-online.de/cgi-bin/getImage.php?w=1200 . Without 1200 I will retrieve file at 320x240 resolution }
      pRequest := HTTPOpenRequest(pConnection, PWideChar(sMethod), PWideChar(Resource), nil, nil, nil, flags, 0);  { The third parameter of HttpOpenRequest is the file name (URL) of the script }

      if Assigned(pRequest) then
        TRY
           Header:= '';
           if Referer > ''
           then Header:= Header+ 'Referer: '+ Referer+ sLineBreak;
           Header:= Header+ 'User-Agent: Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:47.0) Gecko/20100101 Firefox/47.0'+ SLineBreak;
         //Header:= Header+ 'User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36 Edg/91.0.864.59'+SLineBreak;  //  Microsoft Edge UA string
           Header:= Header+ 'Accept: *.*'+ SLineBreak;
           Header:= Header+ 'Accept-Language: en-us,en;q=0.5'+ SLineBreak;
           Header:= Header+ 'Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7'+ SLineBreak;
           Header:= Header+ 'Keep-Alive: 70'+ SLineBreak; { In windows, default is 60 sec }
           Header:= Header+ 'Connection: keep-alive'+ SlineBreak+ SLineBreak;

           HttpAddRequestHeaders(pRequest, PWideChar(Header), Length(Header), HTTP_ADDREQ_FLAG_ADD);

           Result:= HTTPSendRequest(pRequest, NIL, 0, Pointer(PostData), Length(PostData));     { The actual POST data is the forth parameter }
           if Result then
             REPEAT
              ZeroMemory(@Buffer, SizeOf(Buffer));   // Fills the buffer with 0

              { Download bytes }
              InternetReadFile(pRequest, @Buffer, SizeOf(Buffer), BytesRead);

              { We stop? }
              if BytesRead= 0 then break;

              { Convert static array to dynamic array }
              SetLength(TempBytes, BytesRead);
              Move(Buffer[0], TempBytes[0], BytesRead);

              { Merge arrays }
              Data:= Data+ TempBytes;
             UNTIL BytesRead= 0;
        FINALLY
          InternetCloseHandle(pRequest);
        END
      else
        RaiseLastOSError;

    finally
      InternetCloseHandle(pConnection);
    end;
  finally
    InternetCloseHandle(pSession);
  end;
end;





function DownloadFile(CONST URL, Referer, DestinationFile: string; ForceFolder: Boolean= TRUE): Boolean;  { TESTED OK }
VAR BinData: TBytes;
begin
 Result:= DownloadFile(URL, Referer, BinData);
 if Result then
  begin
    if ForceFolder
    AND NOT ForceDirectoriesB(ExtractFilePath(DestinationFile))
    then EXIT(FALSE);

    TRY
      ccIO.WriteBinFile(DestinationFile, BinData, TRUE);
    EXCEPT
     on EFCreateError DO    { I got this error from one user: EFCreateError. Cannot create file. Access denied. Probably it was the antivirus blockin the program. C:\Users\x\AppData\Roaming\BX\Downloaded wallpapers\tegeler-segel-club.de_webcam_tsc_webcam_98D624.jpg }
      begin
       Result:= FALSE;
       MesajError('Cannot write to '+ CRLFw+ DestinationFile+ LBRK+ 'Please make sure your antivirus is not blocking the program. Please contact us if you think this is unrelated to your antivirus.');
      end;
    END;
  end;
end;




function GetTextFile(CONST URL: string; Referer: string= ''): string;     { TESTED OK }
VAR BinData: TBytes;
begin
 if DownloadFile(URL, Referer, BinData)
 then Result:= StringOf(BinData)
 else Result:= '';
end;



function GetTextFile(CONST URL, Referer, DestinationFile: String; ForceFolder: Boolean= TRUE): Boolean;     { TESTED OK }
VAR TextBody: string;
begin
 TextBody:= GetTextFile(URL, Referer);
 Result:= TextBody > '';
 if Result
 then StringToFile(DestinationFile, TextBody, woOverwrite, ForceFolder);
end;






{-------------------------------------------------------------------------------------------------------------
  Uses Delphi\source\rtl\net\System.Net.HttpClient.pas (Embarcadero)

  How to use it:
     Stream:= DownloadFile(URL, Code);
     Result:= (HTTPReturnCode = 200) and (Stream <> nil) and (Stream.Size > 0);

  I get: Exception class ENetHTTPClientException with message 'Error sending data: (12175) A security error occurred. Probalby because HTTPS?
-------------------------------------------------------------------------------------------------------------}
type
  RHTTPOptions = Record
    UserAgent         : string;
    HandleRedirects   : Boolean;
    MaxRedirects      : Integer;
    AllowCookies      : Boolean;
    ResponseTimeout   : Integer;
    ConnectionTimeout : Integer;
    procedure Reset;         // Load default values in these fields
  end;

procedure RHTTPOptions.Reset;
begin
  UserAgent        := 'Mozilla/5.0 (compatible, MSIE 11, Windows NT 6.3; Trident/7.0; rv:11.0) like Gecko';
  AllowCookies     := False;
  HandleRedirects  := TRUE;
  MaxRedirects     := 10;
  ConnectionTimeout:= 60000;
  ResponseTimeout  := 60000;
end;




function DDownloadFile(const URL: string; OUT HttpRetCode: Integer): TMemoryStream;
var
   HttpCli: System.Net.HttpClient.THTTPClient;
   HttpOpt: RHTTPOptions;
begin
  HttpRetCode := 500;
  HttpOpt.Reset;
  HttpCli := THTTPClient.Create;
  try
    HttpCli.UserAgent         := HttpOpt.UserAgent;
    HttpCli.HandleRedirects   := HttpOpt.HandleRedirects;
    HttpCli.MaxRedirects      := HttpOpt.MaxRedirects;
    HttpCli.AllowCookies      := HttpOpt.AllowCookies;
    HttpCli.ResponseTimeout   := HttpOpt.ResponseTimeout;
    HttpCli.ConnectionTimeout := HttpOpt.ConnectionTimeout;
    Result                    := TMemoryStream.Create;
    HttpRetCode               := HttpCli.Get(URL, Result).StatusCode;
  finally
    FreeAndNil(HttpCli);
  end;
end;



function DDownloadFile(const URL, SaveTo: string): Boolean;
var
   HttpRetCode: Integer;
   Stream: TMemoryStream;
begin
  Stream:= DDownloadFile(URL, HttpRetCode);
  try
   Result:= (HttpRetCode = 200) and (Stream <> nil) and (Stream.Size > 0);
   if Result
   then Stream.SaveToFile(SaveTo);
  finally
   FreeAndNil(Stream);
  end;
end;





end.
