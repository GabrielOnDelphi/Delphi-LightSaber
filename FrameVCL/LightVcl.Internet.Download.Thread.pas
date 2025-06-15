UNIT LightVcl.Internet.Download.Thread;

{-------------------------------------------------------------------------------------------------------------
   2025.05
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   DOWNLOADS A FILE FROM THE INTERNET
   Uses WinInet.
--------------------------------------------------------------------------------------------------------------

   Also see:
       c:\Users\Public\Documents\Embarcadero\Studio\21.0\Samples\Object Pascal\RTL\HttpAsyncDownload\HttpAsyncDownloadDemo.dpr

   Tester:
       c:\Projects\LightSaber\Demo\Demo Internet\
       c:\Projects\Testers\Internet download tester images\
-------------------------------------------------------------------------------------------------------------}

{$WARN GARBAGE OFF}                                                                                                     {Silence the: 'W1011 Text after final END' warning }

INTERFACE

USES
   System.SysUtils, System.Classes;

TYPE
 TWinInetObj = class(TThread)
  private
   FUrl: String;                                                                                                        { URL that we are downloading }
   FData : TMemoryStream;                                                                                                      { The downloaded file will be here }
   FOnDownloadDone: TNotifyEvent;
   procedure SetURL(CONST Value: string);
  protected
   procedure Execute; override;
  public
   UserAgent: string;
   Header : string;                                                                                                     { Header to send to the server. Example: 'Accept-Charset: utf-8'#13#10; }
   Referer: string;
   SSL: Boolean;
   property URL  : String read FUrl write SetUrl;
   property Data : TMemoryStream read FData;
   property OnDownloadDone: TNotifyEvent read FOnDownloadDone write FOnDownloadDone;
 end;


IMPLEMENTATION

USES
   ccDownload, ccCore;


procedure TWinInetObj.Execute;
VAR HttpRetCode: string;
begin
 FData:= ccDownload.DownloadToStream(URL, HttpRetCode);
 if Assigned(FOnDownloadDone)
 then FOnDownloadDone(Self);
end;


procedure TWinInetObj.SetURL(const Value: string);
begin
 if (Length(Value) < 5)                                                                            { minimum address would be:  domain.ro/x.htm }
 OR (PosInsensitive('http', Value) <= 0)
 then raise exception.Create('Invalid URL address. The URL does not start with ''http''.')
 else FURL:= Value;
end;


end.
