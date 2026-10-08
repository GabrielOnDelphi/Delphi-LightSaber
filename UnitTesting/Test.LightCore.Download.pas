unit Test.LightCore.Download;

{=============================================================================================================
   2026.10.07
   Unit tests for LightCore.Download.pas
   Tests HTTP download functionality

   The download tests talk to TLocalHttpServer, a small HTTP server on 127.0.0.1 that the test starts itself,
   so they check the exact content without the Internet and fail when the download code is broken.
   TLocalHttpServer and CanConnect are also used by Test.LightCore.Win.Download.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.SyncObjs,
  System.Net.Socket,
  System.Net.URLClient,
  LightCore.Download;

CONST
  { The body TLocalHttpServer sends. ASCII only, so every decoder reads it the same way. }
  LOCAL_TEST_BODY = 'LightSaber download test' + #13#10 + 'Second line 0123456789';

TYPE
  { An HTTP/1.1 server on 127.0.0.1, on a free port, that answers every request with the same body and
    records the head of every request it received. One background thread accepts the connections. }
  TLocalHttpServer = class
  strict private
    FListener: TSocket;
    FThread: TThread;
    FResponse: TBytes;
    FRequests: string;
    procedure Serve;
    procedure AnswerClient(Client: TSocket);
  public
    constructor Create(CONST Body: string);
    destructor Destroy; override;
    function  Url(CONST Path: string = '/'): string;
    procedure Stop;                                  { Raises if the server thread died on an exception }
    property  Requests: string read FRequests;       { Every request head received. Read it only after Stop. }
  end;

{ TRUE if this EXE can open a TCP connection to Host:Port. A probe that uses neither WinINet nor THTTPClient. }
function CanConnect(CONST Host: string; Port: Word; OUT ErrorText: string): Boolean;


TYPE
  [TestFixture]
  TTestDownload = class
  private
    FTestDir: string;
    procedure CleanupTestDir;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { RHttpOptions Tests }
    [Test]
    procedure TestHttpOptions_Reset;

    [Test]
    procedure TestHttpOptions_DefaultValues;

    { Download Tests }
    [Test]
    procedure TestDownloadAsString_ValidUrl;

    [Test]
    procedure TestDownloadAsString_InvalidUrl;

    [Test]
    procedure TestDownloadToFile_ValidUrl;

    [Test]
    procedure TestDownloadToFile_InvalidUrl;

    [Test]
    procedure TestDownloadToStream_ValidUrl;

    [Test]
    procedure TestDownloadAsString_SimpleOverload;

    [Test]
    procedure TestDownloadWithCustomOptions;

    [Test]
    procedure TestDownloadAsString_SimpleOverload_InvalidUrl;
  end;

implementation


{-------------------------------------------------------------------------------------------------------------
   TLocalHttpServer
-------------------------------------------------------------------------------------------------------------}
constructor TLocalHttpServer.Create(CONST Body: string);
VAR
  BodyBytes: TBytes;
  Head: string;
begin
  inherited Create;
  BodyBytes:= TEncoding.UTF8.GetBytes(Body);
  Head:= 'HTTP/1.1 200 OK' + #13#10
       + 'Content-Type: text/plain; charset=utf-8' + #13#10
       + 'Content-Length: ' + IntToStr(Length(BodyBytes)) + #13#10
       + 'Connection: close' + #13#10
       + #13#10;
  FResponse:= TEncoding.ASCII.GetBytes(Head) + BodyBytes;

  FListener:= TSocket.Create(TSocketType.TCP);
  FListener.Listen('127.0.0.1', '', 0);   { Port 0 = the OS picks a free port }

  FThread:= TThread.CreateAnonymousThread(Serve);
  FThread.FreeOnTerminate:= FALSE;
  FThread.Start;
end;


destructor TLocalHttpServer.Destroy;
begin
  if FThread <> NIL then
    begin
      FThread.Terminate;
      FThread.WaitFor;
      FreeAndNil(FThread);
    end;

  { A listening TSocket counts as Connected, so TSocket.Destroy would call shutdown() on it, which raises WSAENOTCONN. ForceClosed skips shutdown(). }
  if FListener <> NIL then
    begin
      FListener.Close(TRUE);
      FreeAndNil(FListener);
    end;
  inherited;
end;


function TLocalHttpServer.Url(CONST Path: string = '/'): string;
begin
  Result:= 'http://127.0.0.1:' + IntToStr(FListener.LocalPort) + Path;
end;


{ Runs in the server thread }
procedure TLocalHttpServer.Serve;
VAR
  Client: TSocket;
begin
  while NOT TThread.CheckTerminated do
    begin
      Client:= FListener.Accept(50);   { NIL after 50 ms without a connection }
      if Client <> NIL then
        try
          AnswerClient(Client);
        finally
          FreeAndNil(Client);
        end;
    end;
end;


{ Reads the request head (up to the empty line), sends the response, closes the connection }
procedure TLocalHttpServer.AnswerClient(Client: TSocket);
VAR
  Buffer: TBytes;
  Count: Integer;
  Head: string;
begin
  SetLength(Buffer, 4096);
  Head:= '';
  repeat
    if TSocket.Select(TFDSet.Create(Client), NIL, NIL, 5000 * 1000) <> TWaitResult.wrSignaled   { Microseconds }
    then raise ESocketError.Create('TLocalHttpServer: no request within 5 s. Received so far: ' + Head);
    { The explicit [] matters: without it the call binds to the "var Bytes: array of Byte; Offset; Count" overload (Buffer[0] as a 1-byte array, Offset 4096), which returns a negative count and reads nothing (measured) }
    Count:= Client.Receive(Buffer[0], Length(Buffer), []);
    if Count > 0
    then Head:= Head + TEncoding.ASCII.GetString(Buffer, 0, Count);
  until (Count <= 0) OR (Pos(#13#10#13#10, Head) > 0);

  FRequests:= FRequests + Head;
  Client.Send(FResponse);
  { ForceClosed: skip shutdown(), which raises WSAENOTCONN when the client has already closed its side. closesocket still sends the pending data. }
  Client.Close(TRUE);
end;


procedure TLocalHttpServer.Stop;
VAR
  ErrorText: string;
begin
  if FThread = NIL then EXIT;

  FThread.Terminate;
  FThread.WaitFor;
  ErrorText:= '';
  if FThread.FatalException <> NIL
  then ErrorText:= FThread.FatalException.ClassName + ': ' + Exception(FThread.FatalException).Message;
  FreeAndNil(FThread);

  if ErrorText <> ''
  then raise Exception.Create('TLocalHttpServer: the server thread failed. ' + ErrorText);
end;




function CanConnect(CONST Host: string; Port: Word; OUT ErrorText: string): Boolean;
VAR
  Sock: TSocket;
begin
  Result:= FALSE;
  ErrorText:= '';
  Sock:= TSocket.Create(TSocketType.TCP);
  try
    try
      Sock.Connect(Host, '', '', Port);
      Sock.Close(TRUE);
      Result:= TRUE;
    except
      on E: ESocketError do
        begin
          { The failure is the answer of the probe: it is returned, not hidden }
          ErrorText:= E.ClassName + ': ' + E.Message;
        end;
    end;
  finally
    FreeAndNil(Sock);
  end;
end;




{-------------------------------------------------------------------------------------------------------------
   TTestDownload
-------------------------------------------------------------------------------------------------------------}
procedure TTestDownload.Setup;
begin
  FTestDir:= TPath.Combine(TPath.GetTempPath, 'DownloadTest_' + TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FTestDir);
end;


procedure TTestDownload.TearDown;
begin
  CleanupTestDir;
end;


procedure TTestDownload.CleanupTestDir;
begin
  if TDirectory.Exists(FTestDir)
  then TDirectory.Delete(FTestDir, True);
end;


{ RHttpOptions Tests }

procedure TTestDownload.TestHttpOptions_Reset;
var
  Options: RHttpOptions;
begin
  { Set some values }
  Options.UserAgent:= 'Custom';
  Options.ResponseTimeout:= 1000;

  { Reset should restore defaults }
  Options.Reset;

  Assert.AreEqual(USER_AGENT_STRING, Options.UserAgent);
  Assert.IsTrue(Options.HandleRedirects);
  Assert.AreEqual(10, Options.MaxRedirects);
end;

procedure TTestDownload.TestHttpOptions_DefaultValues;
var
  Options: RHttpOptions;
begin
  Options.Reset;

  Assert.AreEqual(USER_AGENT_STRING, Options.UserAgent);
  Assert.IsFalse(Options.AllowCookies);
  Assert.IsTrue(Options.HandleRedirects);
  Assert.AreEqual(10, Options.MaxRedirects);
  Assert.AreEqual(60000, Options.ConnectionTimeout);
  Assert.AreEqual(60000, Options.ResponseTimeout);
end;


{ Download Tests }

procedure TTestDownload.TestDownloadAsString_ValidUrl;
var
  Server: TLocalHttpServer;
  Content, ErrorMsg: string;
begin
  Server:= TLocalHttpServer.Create(LOCAL_TEST_BODY);
  try
    Content:= DownloadAsString(Server.Url('/page.html'), ErrorMsg);
    Server.Stop;
  finally
    FreeAndNil(Server);
  end;

  Assert.AreEqual('', ErrorMsg);
  Assert.AreEqual(LOCAL_TEST_BODY, Content, 'DownloadAsString must return the exact body');
end;

procedure TTestDownload.TestDownloadAsString_InvalidUrl;
var Content, ErrorMsg: string;
begin
  Content:= DownloadAsString('https://this.domain.does.not.exist.invalid/', ErrorMsg);

  { Should fail with an error }
  Assert.IsNotEmpty(ErrorMsg);
  Assert.AreEqual('', Content);
end;

procedure TTestDownload.TestDownloadToFile_ValidUrl;
var
  Server: TLocalHttpServer;
  FilePath, ErrorMsg: string;
begin
  FilePath:= TPath.Combine(FTestDir, 'download.html');
  Server:= TLocalHttpServer.Create(LOCAL_TEST_BODY);
  try
    DownloadToFile(Server.Url('/download.html'), FilePath, ErrorMsg);
    Server.Stop;
  finally
    FreeAndNil(Server);
  end;

  Assert.AreEqual('', ErrorMsg);
  Assert.IsTrue(FileExists(FilePath), 'File should have been created');
  Assert.AreEqual(LOCAL_TEST_BODY, TFile.ReadAllText(FilePath, TEncoding.UTF8), 'The file must hold the exact body');
end;

procedure TTestDownload.TestDownloadToFile_InvalidUrl;
var
  FilePath, ErrorMsg: string;
begin
  FilePath:= TPath.Combine(FTestDir, 'invalid.txt');
  DownloadToFile('https://this.domain.does.not.exist.invalid/', FilePath, ErrorMsg);

  { Should fail with an error }
  Assert.IsNotEmpty(ErrorMsg);
  Assert.IsFalse(FileExists(FilePath), 'File should not be created on error');
end;

procedure TTestDownload.TestDownloadToStream_ValidUrl;
var
  Server: TLocalHttpServer;
  Stream: TMemoryStream;
  ErrorMsg: string;
  Bytes: TBytes;
begin
  Stream:= NIL;
  Server:= TLocalHttpServer.Create(LOCAL_TEST_BODY);
  try
    try
      Stream:= DownloadToStream(Server.Url, ErrorMsg);
      Server.Stop;

      Assert.AreEqual('', ErrorMsg);
      Assert.IsNotNull(Stream, 'DownloadToStream must return a stream on success');
      SetLength(Bytes, Stream.Size);
      Stream.Position:= 0;
      Stream.ReadBuffer(Bytes, Length(Bytes));
      Assert.AreEqual(LOCAL_TEST_BODY, TEncoding.UTF8.GetString(Bytes), 'The stream must hold the exact body');
    finally
      FreeAndNil(Stream);
    end;
  finally
    FreeAndNil(Server);
  end;
end;

procedure TTestDownload.TestDownloadAsString_SimpleOverload;
var
  Server: TLocalHttpServer;
  Content: string;
begin
  Server:= TLocalHttpServer.Create(LOCAL_TEST_BODY);
  try
    Content:= DownloadAsString(Server.Url);
    Server.Stop;
  finally
    FreeAndNil(Server);
  end;

  Assert.AreEqual(LOCAL_TEST_BODY, Content, 'The simple overload must return the exact body');
end;

procedure TTestDownload.TestDownloadAsString_SimpleOverload_InvalidUrl;
var
  Content: string;
begin
  { Simple overload should return empty string on error, not raise exception }
  Content:= DownloadAsString('https://this.domain.does.not.exist.invalid/');
  Assert.AreEqual('', Content, 'Should return empty string on error');
end;

procedure TTestDownload.TestDownloadWithCustomOptions;
var
  Server: TLocalHttpServer;
  Content, ErrorMsg: string;
  Options: RHttpOptions;
begin
  Options.Reset;
  Options.ConnectionTimeout:= 30000;
  Options.ResponseTimeout:= 30000;
  Options.UserAgent:= 'TestAgent/1.0';

  Server:= TLocalHttpServer.Create(LOCAL_TEST_BODY);
  try
    Content:= DownloadAsString(Server.Url, ErrorMsg, nil, @Options);
    Server.Stop;

    Assert.AreEqual('', ErrorMsg);
    Assert.AreEqual(LOCAL_TEST_BODY, Content);
    Assert.IsTrue(Pos(#13#10'User-Agent: TestAgent/1.0'#13#10, Server.Requests) > 0, 'The request must carry the custom user agent. Request: ' + Server.Requests);
  finally
    FreeAndNil(Server);
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestDownload);

end.
