UNIT LightCore.Download;

{-------------------------------------------------------------------------------------------------------------
   2026.07.07
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   DOWNLOADS A FILE FROM THE INTERNET
   Uses Delphi\source\rtl\net\System.Net.HttpClient.pas (Embarcadero)
--------------------------------------------------------------------------------------------------------------
   Intended for all new HTTP/S communication.
   Platform: Windows, macOS, Linux, iOS, and Android
   Features:
      TLS versions,
      redirects,
      cookies,
      easier to use than WinInet

   ALSO SEE:
      LightVcl.Internet.Download.Indy.pas
       c:\Users\Public\Documents\Embarcadero\Studio\37.0\Samples\Object Pascal\RTL\HttpDownload\HttpDownloadDemo.dpr
       E:\Backups\My projects\2021\2021.03 Stormy\BSalsa EmbeddedWB\Demos\Various Demos\07 - IEDownload_Demo\IEDownload_Simple_Demo\

   Tester:
       c:\Projects\LightSaber\Demo\Core\Demo Internet\
       c:\Projects\Testers\Internet download tester images\

--------------------------------------------------------------------------------------------------------------
   If you get "ENetHTTPClientException 12175 - A security error occurred":
   12175 is ERROR_WINHTTP_SECURE_FAILURE (on Windows, THTTPClient uses WinHTTP) and usually points to SSL/TLS handshake problems.
   Solution:
     Make sure your OS has up-to-date root certificates.
     Set HttpClient.SecureProtocols to TLS 1.2 and TLS 1.3 (newer Delphi versions only). This unit already does it.
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.Classes, System.SysUtils, System.Net.HttpClient, System.IOUtils, System.Net.URLClient;


CONST
  HTTP_STATUS_OK = 200;

CONST
  USER_AGENT_STRING = 'DelphiApp/1.0 (MyApp HttpDownloader; +http://www.example.com)';
//USER_AGENT_STRING = 'Mozilla/5.0 (compatible, MSIE 11, Windows NT 6.3; Trident/7.0; rv:11.0) like Gecko';

TYPE
  RHttpOptions = Record
    UserAgent         : string;
    HandleRedirects   : Boolean;
    MaxRedirects      : Integer;
    AllowCookies      : Boolean;
    ResponseTimeout   : Integer; // Milliseconds
    ConnectionTimeout : Integer; // Milliseconds
    procedure Reset;             // Load default values in these fields
  end;

  PHttpOptions= ^RHttpOptions;


procedure DownloadToFile  (CONST URL, SaveTo: string; OUT ErrorMsg: string; CustomHeaders: TNetHeaders = nil; HttpOptions: PHttpOptions = NIL);
function  DownloadToStream(CONST URL: string;         OUT ErrorMsg: string; CustomHeaders: TNetHeaders = nil; HttpOptions: PHttpOptions = NIL): TMemoryStream;

function  DownloadAsString(CONST URL: string;         OUT ErrorMsg: string; CustomHeaders: TNetHeaders = nil; HttpOptions: PHttpOptions = NIL): string; overload;
function  DownloadAsString(CONST URL: string): string; overload;


function DownloadImageToFile(CONST URL, LocalPath: string; OUT DownloadedSize: Int64): Boolean;


IMPLEMENTATION

USES
  LightCore, LightCore.IO, LightCore.TextFile, LightCore.AppData, LightCore.Types;


procedure RHttpOptions.Reset;
begin
  UserAgent         := USER_AGENT_STRING;
  AllowCookies      := False; // Usually not needed for simple file/API downloads
  HandleRedirects   := TRUE;
  MaxRedirects      := 10;
  ConnectionTimeout := 60000; // 60 seconds
  ResponseTimeout   := 60000; // 60 seconds
end;


{ Returns a new TMemoryStream instance if HTTP status is 200 OK. Caller must free the returned stream.
  Returns nil otherwise.
  ErrorMsg contains a textual error description ('HTTP error 404: Not Found', 'Download error: ...'); empty = success.
  Network/HTTP errors do not raise - they are reported via ErrorMsg.

You can pass Referers like this:
  var Headers: System.Net.URLClient.TNetHeaders;
  SetLength(Headers, 1);
  Headers[0].Name := 'Referer';
  Headers[0].Value:= 'http://GabrielMoraru.com';  }

function DownloadToStream(CONST URL: string; OUT ErrorMsg: string; CustomHeaders: TNetHeaders = nil; HttpOptions: PHttpOptions = nil): TMemoryStream;
VAR
  HttpClient: THTTPClient;
  Options: RHttpOptions;
  HttpResponse: IHTTPResponse;
begin
  Result:= NIL;
  ErrorMsg:= '';  { Empty means success }

  if HttpOptions = nil
  then Options.Reset
  else Options:= HttpOptions^;

  HttpClient:= THTTPClient.Create;
  try
    HttpClient.UserAgent         := Options.UserAgent;
    HttpClient.HandleRedirects   := Options.HandleRedirects;
    HttpClient.MaxRedirects      := Options.MaxRedirects;
    HttpClient.AllowCookies      := Options.AllowCookies;
    HttpClient.ResponseTimeout   := Options.ResponseTimeout;
    HttpClient.ConnectionTimeout := Options.ConnectionTimeout;
    HttpClient.SecureProtocols   := [THTTPSecureProtocol.TLS12, THTTPSecureProtocol.TLS13];

    Result:= TMemoryStream.Create;
    try
      HttpResponse:= HttpClient.Get(URL, Result, CustomHeaders);
      if HttpResponse.StatusCode <> HTTP_STATUS_OK
      then
        begin
          ErrorMsg:= 'HTTP error ' + IntToStr(HttpResponse.StatusCode) + ': ' + HttpResponse.StatusText;
          FreeAndNil(Result);
        end;
    except
      on E: Exception do
        begin
          ErrorMsg:= 'Download error: ' + E.Message;
          FreeAndNil(Result);
        end;
    end;
  finally
    FreeAndNil(HttpClient);
  end;
end;


{ Does not raise exceptions; errors are indicated via ErrorMsg (empty = success). }
procedure DownloadToFile(CONST URL, SaveTo: string; OUT ErrorMsg: string; CustomHeaders: TNetHeaders = NIL; HttpOptions: PHttpOptions = NIL);
VAR
  Stream: TMemoryStream;
begin
  Stream:= DownloadToStream(URL, ErrorMsg, CustomHeaders, HttpOptions);
  try
    if (ErrorMsg = '') AND (Stream <> NIL)
    then
      if Stream.Size > 0
      then
        try
          LightCore.IO.ForceDirectoriesB(TPath.GetDirectoryName(SaveTo));
          Stream.SaveToFile(SaveTo);
        except
          on E: Exception do
            ErrorMsg:= 'Download succeeded, but file save failed: ' + SaveTo + ' - ' + E.Message;
        end
      else
        ErrorMsg:= 'HTTP 200 OK, but content is empty!';
  finally
    FreeAndNil(Stream);
  end;
end;


{ Edge case: If server omits charset and content is UTF-8 without BOM, it may be misdecoded.
  This is rare with modern servers. }
function DownloadAsString(const URL: string; OUT ErrorMsg: string; CustomHeaders: TNetHeaders = nil; HttpOptions: PHttpOptions = nil): string;
var
  HttpClient: THTTPClient;
  Options: RHttpOptions;
  HttpResponse: IHTTPResponse;
begin
  Result:= '';
  ErrorMsg:= '';

  if HttpOptions = nil
  then Options.Reset
  else Options:= HttpOptions^;

  HttpClient:= THTTPClient.Create;
  try
    try
      HttpClient.UserAgent         := Options.UserAgent;
      HttpClient.HandleRedirects   := Options.HandleRedirects;
      HttpClient.MaxRedirects      := Options.MaxRedirects;
      HttpClient.AllowCookies      := Options.AllowCookies;
      HttpClient.ResponseTimeout   := Options.ResponseTimeout;
      HttpClient.ConnectionTimeout := Options.ConnectionTimeout;
      HttpClient.SecureProtocols   := [THTTPSecureProtocol.TLS12, THTTPSecureProtocol.TLS13];

      HttpResponse:= HttpClient.Get(URL, nil, CustomHeaders);

      if HttpResponse.StatusCode = HTTP_STATUS_OK
      then Result:= HttpResponse.ContentAsString
      else ErrorMsg:= 'HTTP error ' + IntToStr(HttpResponse.StatusCode) + ': ' + HttpResponse.StatusText;
    except
      on E: Exception do
        ErrorMsg:= 'Download error: ' + E.Message;
    end;
  finally
    FreeAndNil(HttpClient);
  end;
end;


{ Ignores errors: returns an empty string on failure. }
function DownloadAsString(CONST URL: string): string;
VAR
  ErrorMsg: string;
begin
  Result:= DownloadAsString(URL, ErrorMsg);
  { ErrorMsg is intentionally ignored - caller doesn't want error details }
end;


{--------------------------------------------------------------------------------------------------
   Validates the downloaded file:
     - Rejects files smaller than 500 bytes
     - Detects HTML 404 pages disguised as downloadable files
   DownloadedSize is set to the file size on success, -1 on failure.
--------------------------------------------------------------------------------------------------}
function DownloadImageToFile(CONST URL, LocalPath: string; OUT DownloadedSize: Int64): Boolean;
VAR
  ErrorMsg: string;
begin
 DownloadedSize:= -1;

 DownloadToFile(URL, LocalPath, ErrorMsg);     { NOTE! If the url is not valid, this will probably download the 404 HTML file}
 Result:= ErrorMsg = '';

 { Check if the program actually downloaded an image or just a '404 file not found' HTML file }
 if Result then
   begin
     DownloadedSize:= LightCore.IO.GetFileSize(LocalPath);

     if DownloadedSize < 500 then
       begin
         AppDataCore.LogInfo('File rejected because it is too small: '+ URL);
         Result:= FALSE;
       end
     else
       begin
         { Some websites return a downloadable 404 page that looks like HTML. }
         VAR FileContent:= StringFromFile(LocalPath);
         if (DownloadedSize < 25*KB)
         AND ( (  (PosInsensitive('<html', FileContent) > 0)
              AND (PosInsensitive('<body', FileContent) > 0))
             OR (PosInsensitive('<!doctype ', FileContent) > 0)
             OR (PosInsensitive('<meta name', FileContent) > 0) ) then
           begin
             AppDataCore.LogWarn('Received HTML file from server instead of image file: '+ URL);
             Result:= FALSE;
           end;
       end;

     if NOT Result then
       begin
         DownloadedSize:= -1;             { Documented contract: DownloadedSize is -1 on failure }
         TryDeleteFile(LocalPath);        { Don't leave the rejected junk file on disk (callers treat failure as 'no file') }
       end;
   end
 else
   AppDataCore.LogWarn('Failed to download: '+ URL);
end;


end.
