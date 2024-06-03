UNIT ciDownloadThread;

{-------------------------------------------------------------------------------------------------------------
   Gabriel Moraru
   2023.06
   See Copyright.txt

   Downloads a file [ via WinInet ]

   Also see:
       c:\Users\Public\Documents\Embarcadero\Studio\21.0\Samples\Object Pascal\RTL\HttpAsyncDownload\HttpAsyncDownloadDemo.dpr

   Tester:
       c:\MyProjects\Project Testers\Internet download tester\
-------------------------------------------------------------------------------------------------------------}

{$WARN GARBAGE OFF}                                                                                                     {Silence the: 'W1011 Text after final END' warning }

INTERFACE

USES
   System.SysUtils, System.Classes;

TYPE
 TWinInetObj = class(TThread)
  private
   FUrl: String;                                                                                                        { URL that we are downloading }
   FData : TBytes;                                                                                                      { The downloaded file will be here }
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
   property Data : TBytes read FData;
   property OnDownloadDone: TNotifyEvent read FOnDownloadDone write FOnDownloadDone;
 end;


IMPLEMENTATION

USES
   ciDownload, ccCore;


procedure TWinInetObj.Execute;
begin
 ciDownload.DownloadFile(URL, Referer, fdata, '', SSL);
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
