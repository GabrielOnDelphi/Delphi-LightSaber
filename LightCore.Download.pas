UNIT LightCore.Download;

{-------------------------------------------------------------------------------------------------------------
   2025.05.10
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
       c:\Users\Public\Documents\Embarcadero\Studio\21.0\Samples\Object Pascal\RTL\HttpDownload\HttpDownloadDemo.dpr
       c:\MyProjects\Packages\BSalsa EmbeddedWB\Demos\IEDownload_Simple_Demo\

   Tester:
       c:\Projects\LightSaber\Demo\Demo Internet\
       c:\Projects\Testers\Internet download tester images\

--------------------------------------------------------------------------------------------------------------
   If you get "ENetHTTPClientException 12175 - A security error occurred":
   This specific error code (ERROR_WINHTTP_SECURE_FAILURE when THTTPClient uses WinHTTP on Windows, or a similar WinINet code if it uses that backend) usually points to SSL/TLS handshake problems.
   Solution:
     Ensuring your OS has up-to-date root certificates.
     Explicitly setting HttpClient.SecureProtocols in newer Delphi versions to use TLS 1.2 and TLS 1.3 (done)
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.Classes, System.SysUtils, System.Net.HttpClient, System.IOUtils, System.Net.URLClient;


CONST
  HTTP_STATUS_OK = 200; // Standard HTTP status code for "OK", defined here as System.Net.HttpClient does not provide it directly

  // Error codes for download operations
  ERROR_CLIENT_EXCEPTION = -1; // Indicates a client-side exception before the request was sent
  ERROR_SAVE_FILE_FAILED = -2; // Indicates that the download succeeded but saving the file failed
  ERROR_EMPTY_CONTENT    = -3; // Indicates that the download succeeded but the content is empty

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


IMPLEMENTATION

USES
  LightCore.IO;


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
  Returns nil otherwise. HttpRetCode contains the HTTP status or a negative error code on client exception.
  Raises ENetHTTPClientException or other exceptions on network/client errors.

You can pass Referers like this:
  var Headers: System.Net.URLClient.TNetHeaders;
  SetLength(Headers, 1);
  Headers[0].Name := 'Referer';
  Headers[0].Value:= 'http://GabrielMoraru.com';  }

function DownloadToStream(CONST URL: string; OUT ErrorMsg: string; CustomHeaders: TNetHeaders = nil; HttpOptions: PHttpOptions = nil): TMemoryStream;
VAR
  HttpClient: System.Net.HttpClient.THTTPClient;
  Options: RHttpOptions;
  HttpResponse: IHTTPResponse;
begin
  Result:= NIL;
  ErrorMsg := '';  // Empty means success

  // Handle the optional HttpOptions parameter
  if HttpOptions = nil
  then Options.Reset             // Use default options
  else Options := HttpOptions^;

  HttpClient := THTTPClient.Create;
  try
    // Configure HttpClient
    HttpClient.UserAgent         := Options.UserAgent;
    HttpClient.HandleRedirects   := Options.HandleRedirects;
    HttpClient.MaxRedirects      := Options.MaxRedirects;
    HttpClient.AllowCookies      := Options.AllowCookies;
    HttpClient.ResponseTimeout   := Options.ResponseTimeout;
    HttpClient.ConnectionTimeout := Options.ConnectionTimeout;
    HttpClient.SecureProtocols   := [THTTPSecureProtocol.TLS12, THTTPSecureProtocol.TLS13]; // Important for modern HTTPS

    Result:= TMemoryStream.Create;
    try
      HttpResponse := HttpClient.Get(URL, Result, CustomHeaders);
      if HttpResponse.StatusCode <> 200 then
      begin
        ErrorMsg:= 'HTTP error ' + IntToStr(HttpResponse.StatusCode) + ': ' + HttpResponse.StatusText;
        FreeAndNil(Result);
      end;
    except
      on E: ENetException do
      begin
        ErrorMsg:= 'Network error: ' + E.Message;
        FreeAndNil(Result);
      end;
    end;
  finally
    FreeAndNil(HttpClient);
  end;
end;


{ Downloads content to a file.
 Does not raise exceptions; errors are indicated via the return value.
 Returns
   200: Download and file saving were successful.
   Other positive values: HTTP status code if the request was sent but the status is not 200.
   ERROR_CLIENT_EXCEPTION: A client error occurred before the request was sent.
   ERROR_SAVE_FILE_FAILED: Download succeeded but saving the file failed.
   ERROR_EMPTY_CONTENT: Download succeeded but the content is empty. }
procedure DownloadToFile(CONST URL, SaveTo: string; OUT ErrorMsg: string; CustomHeaders: TNetHeaders = nil; HttpOptions: PHttpOptions = NIL);
VAR
  Stream: TMemoryStream;
begin
  Stream := DownloadToStream(URL, ErrorMsg, CustomHeaders, HttpOptions);
  try
    if (ErrorMsg = '') AND (Stream <> NIL) then
      if Stream.Size > 0
      then
        try
          LightCore.IO.ForceDirectoriesB(TPath.GetDirectoryName(SaveTo));
          Stream.SaveToFile(SaveTo); // Successfully downloaded and saved
        except
          on E: Exception
            DO ErrorMsg:= 'Download succeeded, but file save failed: '+ SaveTo
        end
      else
        ErrorMsg:= 'HTTP 200 OK, but content is empty!'
  finally
    FreeAndNil(Stream);
  end;
end;


// Edge Cases: If a server omits the charset and the content is UTF-8 without a BOM, it may be misdecoded as ISO-8859-1. However, this is rare with modern servers, and you could extend the function to pass a default encoding (e.g., ContentAsString(TEncoding.UTF8)) if needed.
function DownloadAsString(const URL: string; OUT ErrorMsg: string; CustomHeaders: TNetHeaders = nil; HttpOptions: PHttpOptions = nil): string;
var
  HttpClient: THTTPClient;
  Options: RHttpOptions;
  HttpResponse: IHTTPResponse;
begin
  Result:= '';
  ErrorMsg:= '';

  // Handle the optional HttpOptions parameter
  if HttpOptions = nil
  then Options.Reset             // Use default options
  else Options:= HttpOptions^;

  HttpClient := THTTPClient.Create;
  try
    // Apply the options (default or provided)
    HttpClient.UserAgent         := Options.UserAgent;
    HttpClient.HandleRedirects   := Options.HandleRedirects;
    HttpClient.MaxRedirects      := Options.MaxRedirects;
    HttpClient.AllowCookies      := Options.AllowCookies;
    HttpClient.ResponseTimeout   := Options.ResponseTimeout;
    HttpClient.ConnectionTimeout := Options.ConnectionTimeout;
    HttpClient.SecureProtocols   := [THTTPSecureProtocol.TLS12, THTTPSecureProtocol.TLS13];  // Important for modern HTTPS

    // Perform the GET request, no stream needed since we want a string
    HttpResponse:= HttpClient.Get(URL, nil, CustomHeaders);

    if HttpResponse.StatusCode = HTTP_STATUS_OK
    then Result:= HttpResponse.ContentAsString       // RTL handles encoding via Content-Type charset
    else ErrorMsg:= 'HTTP Error ' + IntToStr(HttpResponse.StatusCode) + ': ' + HttpResponse.StatusText;
  except
    on E: ENetException
      DO ErrorMsg := 'Network error: ' + E.Message;
  end;
  FreeAndNil(HttpClient);
end;


// Downloads content as a string, ignoring errors (returns an empty string on failure).
// Assumes UTF-8 encoding.
function DownloadAsString(CONST URL: string): string;
VAR ErrorMsg: string;
begin
  try
    Result:= DownloadAsString(URL, ErrorMsg);
  except
    on E: ENetException
      DO ErrorMsg := 'Network error: ' + E.Message;
  end;
end;


end.
