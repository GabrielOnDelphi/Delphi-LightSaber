UNIT ciInetDonwIndy;

{-------------------------------------------------------------------------------------------------------------
   Gabriel Moraru
   2023.06
   See Copyright.txt

   DOWNLOAD A FILE FROM INTERNET Indy
   If you add it to your project, it will drag in the whole Indy and the compilation will be slower.

------------------------------------------------------------------------------------------------------------------------

  Features:
    + The CPU does not go to 100% as in the case of WinInet
    + The GUI does not freeze because I use IDAntiFreeze

  Issues:
    - For HTTPS connections it needs 2 external DLLs: libeay32.dll & ssleay32.dll
    - Size: DON'T ADD IT TO CUBIC-PACKAGE because it will add Indy to all projects and the compilation will be slow !!!!!
    - The program does not exit until the download is complete?

  Alternatives:
     ciDownload.pas
     HTTPGet.pas
     UrlMon       -  UrlMon.UrlDownloadToFile (nil, <url>, <file destination path (PChar)>, 0, nil);  Don't know if it is thread-safe, UrlMon.pas is just an interface for calling UrlMon.dll, a Microsoft API. And yes, is a Borland unit \delphi6\source\rtl\Win\UrlMon.pas
     Indy.TDownloadURL.ExecuteTarget   it can also be threaded -> https://stackoverflow.com/questions/3187446/the-connection-does-not-timeout-while-using-indy

  Documentation:
    I ALREADY HAVE CODE THAT MAKES IT THREADED:  SEE:  TPhastTask.Start

-------------------------------------------------------------------------------------------------------------}
INTERFACE

USES
   Winapi.Windows, System.SysUtils, System.AnsiStrings, System.StrUtils, Vcl.Forms, System.Classes,
   IdBaseComponent, IdComponent, IdTCPClient, IdHTTP, IdAntiFreeze;

TYPE
  TSendThread = class(TThread)
  private
    Indy: TIdHTTP;
  protected
    procedure Execute; override;
  public
    URL: String;
    ErrorMsg: string;
    DestFile: string;
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

function DownloadFile   (CONST URL, Referer, DestinationFile: string; OUT ErrorMsg: String): Boolean;         { Only suitable for small files because the whole download will be saved to RAM before writing it to disk }

function DownloadThread (CONST URL, DestinationFile: string; OUT ErrorMsg: String): Boolean;                  { Only suitable for small files because the whole download will be saved to RAM before writing it to disk }
function DownloadThread2(CONST URL, DestinationFile: string; OUT ErrorMsg: String): Boolean;
    

IMPLEMENTATION

USES
     IdGlobal,
     IdSSLOpenSSL,
     IdIOHandler,
     IdIOHandlerSocket,
     IdIOHandlerStack;

{ Only suitable for small files because the whole download will be saved to RAM before writing it to disk.
  Similar example here: https://stackoverflow.com/questions/2184473/download-a-file-from-internet-programmatically-with-an-progress-event-using-delp

The HttpWork event handler could be something like:

    procedure TFormMain.HttpWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    begin
      var Http := TIdHTTP(ASender);
      if (Pos('chunked', LowerCase(Http.Response.TransferEncoding)) = 0) AND (Http.Response.ContentLength > 0) then MemoOutput.Lines.Add('Downloaded  '+ IntToStr(100*AWorkCount div ContentLength)+ '%');
    end;
}
function DownloadFile(CONST URL, Referer, DestinationFile: string; OUT ErrorMsg: String): Boolean;
VAR
  Indy: TIDHTTP;
  Stream: TFileStream;
  IDAntiFreeze: TIDAntiFreeze;
  IOHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  Indy:= TIDHTTP.Create(NIL);
  IOHandler:= TIdSSLIOHandlerSocketOpenSSL.Create(NIL);
  IDAntiFreeze:= TIDAntiFreeze.Create(NIL);
  Stream:= TFileStream.Create(DestinationFile, fmCreate);
  TRY
    IOHandler.MaxLineAction := maException;
    IOHandler.SSLOptions.SSLVersions := [sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2];

    Indy.ConnectTimeout          := 0;                   { Indy default value: zero }
    Indy.ReadTimeout             := -1;                  { Indy default value: -1 }
    Indy.HandleRedirects         := TRUE;                { http://stackoverflow.com/questions/4549809/indy-idhttp-how-to-handle-page-redirects }
    Indy.AllowCookies            := FALSE;
    Indy.Request.UserAgent       := 'Mozilla/4.0';
    Indy.Request.Connection      := 'Keep-Alive';
    Indy.Request.ProxyConnection := 'Keep-Alive';
    Indy.Request.CacheControl    := 'no-cache';
    Indy.IOHandler               := IOHandler;
    //Indy.referer                 := Referer;
    /// Indy.OnWork:= HttpWork;

    TRY
      Indy.Get(URL, Stream);
      Result:= TRUE;
    EXCEPT
      On E: Exception DO
        begin
          Result:= FALSE;
          ErrorMsg := E.Message + ' (' + IntToStr(Indy.ResponseCode) + ')';
        end;
    END;
  FINALLY
    FreeAndNil(Stream);
    FreeAndNil(IDAntiFreeze);
    FreeAndNil(IOHandler);
    FreeAndNil(Indy);
  END;
end;









{------------------------------------------------------------------------------------------------------------}









{NOT GOOD: it will block the GUI until it finishes! }
function DownloadThread(CONST URL, DestinationFile: string; OUT ErrorMsg: String): Boolean;    { Only suitable for small files because the whole download will be saved to RAM before writing it to disk }
VAR
  Stream: TMemoryStream;
  Indy : TIDHTTP;
begin
  Indy                         := TIDHTTP.Create(NIL);
  Indy.ConnectTimeout          := 0;                                      { Indy default value: 0 }
  Indy.ReadTimeout             := -1;                                     { Indy default value: -1 }
  Indy.HandleRedirects         := TRUE;                                   { http://stackoverflow.com/questions/4549809/indy-idhttp-how-to-handle-page-redirects }
  Indy.AllowCookies            := FALSE;
  Indy.Request.UserAgent       := 'Mozilla/4.0';
  Indy.Request.Connection      := 'Keep-Alive';
  Indy.Request.ProxyConnection := 'Keep-Alive';
  Indy.Request.CacheControl    := 'no-cache';

  Stream := TMemoryStream.Create;
  TRY
    TRY
      Indy.Get(URL, Stream);

      Stream.SaveToFile(DestinationFile);
      Result:= TRUE;
    EXCEPT
      On E: exception do
        begin
          Result:= FALSE;
          ErrorMsg := E.Message + ' (' + IntToStr(Indy.ResponseCode) + ')';
        end;
    END;
  FINALLY
    FreeAndNil(Stream);
    FreeAndNil(Indy);
  END;
end;






(*

THIS WILL WORK!

VAR
  aThread: TThread;
begin
 aThread := TThread.CreateAnonymousThread(
     procedure()
     VAR strHtml: string;
     begin
      strHtml := IdHttp1.Get( 'http://www.marcocantu.com' );
      aThread.Synchronize(aThread,
        procedure()
        begin
          memo1.Lines.Add(Copy (strHtml, 1, 200));
        end);
      end);
 aThread.Start;
end;




variant:

function DownloadThread(CONST aSourceURL: string; CONST aDestFileName: string; OUT ErrorMsg: String): Boolean;
begin
  // This will start an anonymous thread to download the string content from the list of URLs
  TThread.CreateAnonymousThread(
     procedure
     var
       LUrl: string;
     begin
         Result:= DownloadThread_(aSourceURL, aDestFileName, ErrorMsg);   // DownloadString will be executed asynchronously
         {
         // Safely update the GUI using TThread.Synchronize or TThread.Queue
         TThread.Synchronize(nil,
           procedure
           begin
             memo1.Lines.Text := memo1.Lines.Text + LResult;
           end
         ); }
     end
  ).Start;
end;

*)


constructor TSendThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate:= FALSE;        //If you want to use a loop to wait for the thread to finish, without blocking the UI, set FreeOnTerminate= false

  Indy := TIdHTTP.Create;
  Indy.ConnectTimeout          := 0;                                      { Indy default value: 0 }
  Indy.ReadTimeout             := -1;                                     { Indy default value: -1 }
  Indy.HandleRedirects         := TRUE;                                   { http://stackoverflow.com/questions/4549809/indy-idhttp-how-to-handle-page-redirects }
  Indy.AllowCookies            := FALSE;
  Indy.Request.UserAgent       := 'Mozilla/4.0';
  Indy.Request.Connection      := 'Keep-Alive';
  Indy.Request.ProxyConnection := 'Keep-Alive';
  Indy.Request.CacheControl    := 'no-cache';
end;


destructor TSendThread.Destroy;
begin
  FreeAndNil(Indy);
  inherited;
end;


procedure TSendThread.Execute;
VAR Stream: TFileStream;
begin
  Stream := TFileStream.Create(DestFile, fmCreate);
  TRY
    TRY
      Indy.Get(URL, Stream);                      //Result := Indy.Get(URL);  // if I want to return a string, use this
    EXCEPT
      on E: exception
       DO ErrorMsg:= E.Message + ' (' + IntToStr(Indy.ResponseCode) + ')';
    END;
  FINALLY
    FreeAndNil(Stream);
  END;
end;




{ Disadvantage: the application won't Close until the download is over!!!! }
function DownloadThread2(CONST URL: string; CONST DestinationFile: string; OUT ErrorMsg: String): Boolean;
VAR
   Thread : TSendThread;
   ThreadHandle: THandle;
CONST
  bWaitAll = FALSE;  { Explanation for this: https://blogs.msdn.microsoft.com/larryosterman/2004/06/02/things-you-shouldnt-do-part-4-msgwaitformultipleobjects-is-a-very-tricky-api/ }
begin
  Result:= FALSE;
  Thread := TSendThread.Create;
  try
    Thread.URL := URL;
    Thread.DestFile := DestinationFile;
    Thread.ErrorMsg := ErrorMsg;
    Thread.Start;

    ThreadHandle:= Thread.Handle;
    REPEAT
      if Thread.Terminated then
       begin
        EXIT(false); //  Test this
       end;

      case MsgWaitForMultipleObjects(1, ThreadHandle, bWaitAll, INFINITE, QS_ALLINPUT) of
        WAIT_OBJECT_0:
         begin
          EXIT(TRUE);
         end;
        WAIT_OBJECT_0+1: Application.ProcessMessages;
        WAIT_FAILED:     RaiseLastOSError;
      else
        Break;
      end;
    UNTIL FALSE;
  finally
    FreeAndNil(Thread);
  end;
end;





end.


  
