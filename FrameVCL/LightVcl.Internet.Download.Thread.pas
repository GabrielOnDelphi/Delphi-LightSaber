UNIT LightVcl.Internet.Download.Thread;

{-------------------------------------------------------------------------------------------------------------
   Gabriel Moraru
   2026.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   DOWNLOADS A FILE FROM THE INTERNET
   Uses WinInet via LightCore.Download.

   Features:
     - Asynchronous download in a background thread
     - Fires OnDownloadDone event when complete (thread-safe via Synchronize)
     - Supports HTTP and HTTPS URLs

   Usage:
     var Downloader: TWinInetObj;
     Downloader:= TWinInetObj.Create;
     Downloader.OnDownloadDone:= MyDownloadDoneHandler;
     Downloader.URL:= 'https://example.com/file.zip';
     Downloader.Start;
     // When done, Data property contains the downloaded content
     // Remember to free the Downloader when finished. Do NOT free Data - it is owned and freed by the Downloader!
--------------------------------------------------------------------------------------------------------------

   Also see:
       c:\Users\Public\Documents\Embarcadero\Studio\37.0\Samples\Object Pascal\RTL\HttpAsyncDownload\HttpAsyncDownloadDemo.dpr

   Tester:
       c:\Projects\LightSaber\Demo\Core\Demo Internet\
       c:\Projects\Testers\Internet download tester images\
-------------------------------------------------------------------------------------------------------------}

{$WARN GARBAGE OFF}                                                                                                     {Silence the: 'W1011 Text after final END' warning }

INTERFACE

USES
   System.SysUtils, System.Classes;

TYPE
 TWinInetObj = class(TThread)
  private
   FUrl: String;
   FData: TMemoryStream;                                                                                                { The downloaded content will be stored here }
   FHttpRetCode: string;                                                                                                { Error message from the download engine. Empty = success }
   FOnDownloadDone: TNotifyEvent;
   procedure SetURL(CONST Value: string);
   procedure DoDownloadDone;
  protected
   procedure Execute; override;
  public
   UserAgent: string;                                                                                                   { Reserved for future use }
   Header: string;                                                                                                      { Header to send to the server. Example: 'Accept-Charset: utf-8'#13#10; Reserved for future use }
   Referer: string;                                                                                                     { Reserved for future use }
   SSL: Boolean;                                                                                                        { Reserved for future use }

   constructor Create;
   destructor Destroy; override;

   function DownloadSuccess: Boolean;                                                                                   { TRUE when Data is not NIL and holds at least one byte }

   property URL: String read FUrl write SetUrl;
   property Data: TMemoryStream read FData;
   property HttpRetCode: string read FHttpRetCode;                                                                      { Error message from LightCore.Download.DownloadToStream (e.g., 'HTTP error 404: Not Found'). Empty = success. NOT a bare numeric code! }
   property OnDownloadDone: TNotifyEvent read FOnDownloadDone write FOnDownloadDone;
 end;


IMPLEMENTATION

USES
   LightCore.Download, LightCore;


constructor TWinInetObj.Create;
begin
  inherited Create(True);  // Create suspended
  FreeOnTerminate:= FALSE;
  FData:= NIL;
  FHttpRetCode:= '';
  FUrl:= '';
  UserAgent:= '';
  Header:= '';
  Referer:= '';
  SSL:= FALSE;
end;


destructor TWinInetObj.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;


procedure TWinInetObj.Execute;
begin
  FreeAndNil(FData);  // Free any previous download data
  FHttpRetCode:= '';

  FData:= LightCore.Download.DownloadToStream(URL, FHttpRetCode);

  // Synchronize, so the OnDownloadDone handler may touch the VCL
  if Assigned(FOnDownloadDone)
  then Synchronize(DoDownloadDone);
end;


{ Fires the OnDownloadDone event. Execute calls it through Synchronize, so it runs in the main thread. }
procedure TWinInetObj.DoDownloadDone;
begin
  if Assigned(FOnDownloadDone)
  then FOnDownloadDone(Self);
end;


function TWinInetObj.DownloadSuccess: Boolean;
begin
  Result:= (FData <> NIL) AND (FData.Size > 0);
end;


procedure TWinInetObj.SetURL(const Value: string);
begin
  Assert(Value <> '', 'TWinInetObj.SetURL: URL is empty');

  if (Length(Value) < 10)  // Minimum: http://x.co
  OR (PosInsensitive('http', Value) <> 1)
  then raise Exception.Create('Invalid URL address. The URL must start with ''http://'' or ''https://''.')
  else FURL:= Value;
end;


end.
