UNIT LightVcl.Internet.Browser;

{=============================================================================================================
   2026.09.24
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Reads ONE web page with a REAL browser (WebView2) and hands back whatever a JavaScript snippet extracts from the live DOM.

   Why a real browser instead of an HTTP download:
     - It runs the site's JavaScript, so client-rendered pages (SPAs) actually have text to read.
     - It is a normal signed-in browser session, so it clears Cloudflare where an anonymous GET gets 403.
   For a plain server-rendered page a normal download is cheaper - see LightCore.Win.Download.

   HOW TO USE
     1. Drop a TEdgeBrowser on your form (do NOT create it in code - it needs a parent window).
     2. Set Browser.UserDataFolder BEFORE the first navigation. See trap 2 below.
     3. Reader:= TWebPageReader.Create(Browser);  Reader.OnPageText:= ...;  Reader.OnFailed:= ...;
     4. Reader.ReadPage(Url, TWebPageReader.BuildExtractScript('', ''));

   The reader is EVENT-DRIVEN. It never blocks and never pumps the message loop itself.

   ------------------------------------------------------------------------------------------------------
   FOUR TRAPS, all verified in C:\Delphi\Delphi 13\source\internet\Vcl.Edge.pas (Delphi 13)
   ------------------------------------------------------------------------------------------------------
   1. ExecuteScript has three overloads and TWO of them busy-spin the UI thread at 100% CPU:
        ExecuteScript(JS, AFinishedProc)      -> repeat..until around PeekMessage
        ExecuteScript(JS, AJsonPath): string  -> calls the one above
        ExecuteScript(JS)                     -> asynchronous, raises OnExecuteScript
      This unit uses ONLY the last one.

   2. UserDataFolder defaults to a folder named after the EXE:
        TPath.Combine(CLocalAppData, TPath.GetFileName(ParamStr(0) + '.WebView2'))   in TCustomEdgeBrowser.Create
      So renaming the exe silently points it at a NEW, EMPTY profile and the logged-in session is gone.
      Always set UserDataFolder explicitly.

   3. Never hide or minimize the window that hosts the browser.
      Vcl.Edge maps the VCL Visible onto WebView2 IsVisible in TCustomEdgeBrowser.CMParentVisibleChanged, and SC_MINIMIZE forces IsVisible False in TCustomEdgeBrowser.CMSysCommand.
      Chromium throttles a non-visible page (reported ~1 s task intervals, requestAnimationFrame stopped), so the SettleDelay wait can end up reading an unfinished page.
      Park the window OFF-SCREEN instead.

   4. OnNavigationCompleted never passes the HTTP status code. Its handler in TCustomEdgeBrowser.CreateCoreWebView2ControllerCompleted reads only IsSuccess and WebErrorStatus.
      So this unit does not use OnNavigationCompleted: TNavCompletedHandler registers on the WebView2 interface directly and also reads ICoreWebView2NavigationCompletedEventArgs2.HttpStatusCode.
=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, System.SysUtils, System.Classes, System.JSON,
  Vcl.ExtCtrls, Vcl.Edge, Winapi.WebView2;

CONST
  DefaultSettleDelay = 1200;   { ms between "navigation completed" and running the extractor }
  DefaultTimeout     = 30000;  { ms for the whole job }

  { Elements that are never page content. Removed from the live DOM before the text is read.
    NOTE - "form" is deliberately NOT in this list. It looks like chrome and it is not: old.reddit.com wraps
    every single comment body in <form class="usertext">, so stripping forms silently deleted every comment
    while leaving the comment headers in place. Measured 2026-07-24. A form contributes almost no innerText
    of its own anyway, so there was never much to gain. }
  DefaultStripSelectors =
    'script,style,noscript,svg,iframe,nav,header,footer,aside,'+
    '[role=''navigation''],[role=''banner''],[role=''contentinfo''],[role=''search''],[aria-hidden=''true'']';

TYPE
  TPageOutcome = (poOK, poNavFailed, poEmpty, poTimeout, poBrowserFailed,
                  poHttpError);   { the server answered with a status of 400 or above. Info names it: 'HTTP 404 Not Found' }

  TPageTextEvent = procedure (Sender: TObject; CONST Text: string) of object;
  TPageFailEvent = procedure (Sender: TObject; Outcome: TPageOutcome; CONST Info: string) of object;

  TWebPageReader = class(TObject)
   private
     FBrowser     : TCustomEdgeBrowser;
     FSettleTimer : TTimer;
     FGuardTimer  : TTimer;
     FGraceTimer  : TTimer;    { makes a 404/410 final. See NavigationCompleted. }
     FUrl         : string;
     FScript      : string;
     FBusy        : Boolean;
     FNavigated   : Boolean;   { the navigation has been issued - do not issue it twice }
     FLastNavError: string;    { remembered, not acted on. See NavigationCompleted. }
     FLastHttpStatus: Integer; { of that same failed navigation. 0 = no HTTP response arrived }
     FNavHooked   : Boolean;   { TNavCompletedHandler is registered - see HookNavigation }
     FNavToken    : EventRegistrationToken;
     function  HookNavigation: Boolean;
     procedure BrowserCreated     (Sender: TCustomEdgeBrowser; AResult: HResult);
     procedure NavigationStarting (Sender: TCustomEdgeBrowser; Args: TNavigationStartingEventArgs);
     procedure NavigationCompleted(IsSuccess: Boolean; WebErrorStatus: COREWEBVIEW2_WEB_ERROR_STATUS; HttpStatus: Integer);
     procedure ScriptCompleted    (Sender: TCustomEdgeBrowser; AResult: HResult; CONST AResultObjectAsJson: string);
     procedure SettleFire(Sender: TObject);
     procedure GuardFire (Sender: TObject);
     procedure GraceFire (Sender: TObject);
     procedure StopTimers;
     procedure Succeed(CONST Text: string);
     procedure Fail(Outcome: TPageOutcome; CONST Info: string);
   public
     SettleDelay: Integer;         { ms. Raise it for slow single-page applications. }
     TimeOut    : Integer;         { ms. Whole job. Guarantees the caller is never left hanging. }
     OnPageText : TPageTextEvent;
     OnFailed   : TPageFailEvent;

     constructor Create(ABrowser: TCustomEdgeBrowser);
     destructor Destroy; override;

     procedure ReadPage(CONST Url, JavaScript: string);

     { Builds the standard "give me the visible text" extractor.
       ContentSelector - CSS selector of the region to read. Empty = the whole body.
       ExtraStrip      - extra CSS selectors to delete before reading (site-specific chrome). Can be empty. }
     class function BuildExtractScript(CONST ContentSelector, ExtraStrip: string): string;

     { WebView2 hands back the script result JSON-encoded: a JS string arrives wrapped in quotes and escaped. }
     class function UnwrapEnvelope(CONST Envelope: string): string;

     property Busy: Boolean read FBusy;
   end;


IMPLEMENTATION


{-------------------------------------------------------------------------------------------------------------
   NAVIGATION HANDLER - see trap 4 in the header
-------------------------------------------------------------------------------------------------------------}
TYPE
  TNavCompletedHandler = class(TInterfacedObject, ICoreWebView2NavigationCompletedEventHandler)
   private
     FReader: TWebPageReader;
   public
     constructor Create(AReader: TWebPageReader);
     function Invoke(CONST Sender: ICoreWebView2; CONST Args: ICoreWebView2NavigationCompletedEventArgs): HResult; stdcall;
   end;


constructor TNavCompletedHandler.Create(AReader: TWebPageReader);
begin
  inherited Create;
  FReader:= AReader;
end;


function TNavCompletedHandler.Invoke(CONST Sender: ICoreWebView2; CONST Args: ICoreWebView2NavigationCompletedEventArgs): HResult;
VAR
  IsSuccess     : Integer;
  WebErrorStatus: COREWEBVIEW2_WEB_ERROR_STATUS;
  HttpStatus    : Integer;
  Args2         : ICoreWebView2NavigationCompletedEventArgs2;
begin
  Result        := S_OK;
  IsSuccess     := 0;
  WebErrorStatus:= COREWEBVIEW2_WEB_ERROR_STATUS_UNKNOWN;
  HttpStatus    := 0;

  Args.Get_IsSuccess(IsSuccess);
  Args.Get_WebErrorStatus(WebErrorStatus);
  if Supports(Args, ICoreWebView2NavigationCompletedEventArgs2, Args2)
  then Args2.Get_HttpStatusCode(HttpStatus);

  FReader.NavigationCompleted(IsSuccess <> 0, WebErrorStatus, HttpStatus);
end;


{ Reason phrases from RFC 9110, section 15; 429 from RFC 6585. A status not listed is shown as a bare number. }
function HttpStatusText(Status: Integer): string;
begin
  { Microsoft: HttpStatusCode is 0 when "the navigation failed before a response was received", for example "if the hostname was not found, or if there was a network error".
    https://learn.microsoft.com/en-us/microsoft-edge/webview2/reference/win32/icorewebview2navigationcompletedeventargs2 }
  if Status = 0 then EXIT('no HTTP response');

  case Status of
    400: Result:= 'Bad Request';
    401: Result:= 'Unauthorized';
    403: Result:= 'Forbidden';
    404: Result:= 'Not Found';
    405: Result:= 'Method Not Allowed';
    408: Result:= 'Request Timeout';
    410: Result:= 'Gone';
    429: Result:= 'Too Many Requests';
    500: Result:= 'Internal Server Error';
    502: Result:= 'Bad Gateway';
    503: Result:= 'Service Unavailable';
    504: Result:= 'Gateway Timeout';
  else
    Result:= '';
  end;

  Result:= Trim('HTTP ' + IntToStr(Status) + ' ' + Result);
end;


{ A TTimer with Interval 0 never fires: TTimer.UpdateTimer in Vcl.ExtCtrls.pas calls SetTimer only when FInterval <> 0. So --wait 0 would otherwise end every read in a timeout. }
procedure RestartTimer(Timer: TTimer; Interval: Integer);
begin
  Timer.Enabled := FALSE;
  if Interval < 1
  then Timer.Interval:= 1
  else Timer.Interval:= Interval;
  Timer.Enabled := TRUE;
end;



{-------------------------------------------------------------------------------------------------------------
   TWebPageReader
-------------------------------------------------------------------------------------------------------------}

constructor TWebPageReader.Create(ABrowser: TCustomEdgeBrowser);
begin
  inherited Create;
  Assert(ABrowser <> NIL, 'TWebPageReader needs a browser!');
  FBrowser:= ABrowser;

  SettleDelay:= DefaultSettleDelay;
  TimeOut    := DefaultTimeout;

  { We own these events for as long as we live. OnNavigationCompleted is deliberately not used - see trap 4 in the header. }
  FBrowser.OnCreateWebViewCompleted:= BrowserCreated;
  FBrowser.OnNavigationStarting    := NavigationStarting;
  FBrowser.OnExecuteScript         := ScriptCompleted;

  FSettleTimer:= TTimer.Create(NIL);
  FSettleTimer.Enabled := FALSE;
  FSettleTimer.OnTimer := SettleFire;

  FGuardTimer:= TTimer.Create(NIL);
  FGuardTimer.Enabled := FALSE;
  FGuardTimer.OnTimer := GuardFire;

  FGraceTimer:= TTimer.Create(NIL);
  FGraceTimer.Enabled := FALSE;
  FGraceTimer.OnTimer := GraceFire;
end;


destructor TWebPageReader.Destroy;
begin
  StopTimers;
  FreeAndNil(FSettleTimer);
  FreeAndNil(FGuardTimer);
  FreeAndNil(FGraceTimer);

  if FBrowser <> NIL then
   begin
     FBrowser.OnCreateWebViewCompleted:= NIL;
     FBrowser.OnNavigationStarting    := NIL;
     FBrowser.OnExecuteScript         := NIL;

     { WebView2 calls its handlers on this (the UI) thread, so once the handler is removed it can never call this object again. }
     if FNavHooked AND (FBrowser.DefaultInterface <> NIL)
     then FBrowser.DefaultInterface.remove_NavigationCompleted(FNavToken);
   end;

  inherited;
end;


{ Registers TNavCompletedHandler (see trap 4 in the header). Only possible once the WebView exists: DefaultInterface is NIL before that. }
function TWebPageReader.HookNavigation: Boolean;
VAR
  Handler: ICoreWebView2NavigationCompletedEventHandler;
  HR: HResult;
begin
  if FNavHooked then EXIT(TRUE);
  Assert(FBrowser.DefaultInterface <> NIL, 'HookNavigation: the WebView does not exist yet!');

  Handler:= TNavCompletedHandler.Create(Self);   { held in a variable, never passed as a temporary, so its reference count is 1 during the call }
  HR:= FBrowser.DefaultInterface.add_NavigationCompleted(Handler, FNavToken);
  if Winapi.Windows.Failed(HR) then
    begin
      Fail(poBrowserFailed, 'Cannot register the navigation handler. HRESULT $'+ IntToHex(HR, 8));
      EXIT(FALSE);
    end;

  FNavHooked:= TRUE;
  Result:= TRUE;
end;


{-------------------------------------------------------------------------------------------------------------
   THE JOB
-------------------------------------------------------------------------------------------------------------}

procedure TWebPageReader.ReadPage(CONST Url, JavaScript: string);
begin
  Assert(NOT FBusy, 'TWebPageReader is already reading a page!');
  Assert(Url <> '', 'Empty URL!');

  FUrl         := Url;
  FScript      := JavaScript;
  FBusy        := TRUE;
  FNavigated   := FALSE;
  FLastNavError:= '';
  FLastHttpStatus:= 0;

  { The guard timer is armed FIRST, so even a browser that never initialises ends the job. }
  RestartTimer(FGuardTimer, TimeOut);

  { The very first navigation only kicks off the (asynchronous) creation of the WebView.
    We do not rely on Vcl.Edge replaying the URL afterwards - BrowserCreated navigates explicitly instead. }
  if FBrowser.BrowserControlState = TCustomEdgeBrowser.TBrowserControlState.Created
  then
    begin
      if NOT HookNavigation then EXIT;
      FNavigated:= TRUE;
      FBrowser.Navigate(FUrl);
    end
  else
    FBrowser.CreateWebView;
end;


procedure TWebPageReader.BrowserCreated(Sender: TCustomEdgeBrowser; AResult: HResult);
begin
  if NOT FBusy then EXIT;

  if Winapi.Windows.Failed(AResult) then
    begin
      Fail(poBrowserFailed, 'WebView2 could not be created. HRESULT $'+ IntToHex(AResult, 8)
                          + '. Is the WebView2 runtime installed, and is this exe allowed through the firewall?');
      EXIT;
    end;

  if NOT HookNavigation then EXIT;

  if NOT FNavigated then
    begin
      FNavigated:= TRUE;
      FBrowser.Navigate(FUrl);
    end;
end;


{ WebView2 fires this several times for ONE logical page: a redirect replaces the first navigation, and the replaced one completes as FAILED with status UNKNOWN (Chromium's ERR_ABORTED).
  Measured 2026-07-24 on old.reddit.com, which reports exactly that and then loads fine.
  So a failure is normally NOT final here - it is only remembered.
  The job ends when a navigation SUCCEEDS and the page settles, or when the guard timer runs out.
  The guard then reports the last remembered failure, which is a far more useful message than "timed out".

  The one exception is HTTP 404 and 410: the server itself says the URL is wrong. That failure becomes final after SettleDelay, unless a new navigation starts first (a 404 page may redirect by script - see NavigationStarting).
  Measured 2026-09-15, before this rule: a 404 cost the whole 30 s timeout and the message did not say 404.
  Every other error status keeps the full wait. A page that clears itself - an anti-bot challenge that reloads the real page once its script has run - may be served with one of them.
  [UNVERIFIED: which status codes the anti-bot services use. Cloudflare's own page on detecting a challenge response names only the cf-mitigated header.] }
procedure TWebPageReader.NavigationCompleted(IsSuccess: Boolean; WebErrorStatus: COREWEBVIEW2_WEB_ERROR_STATUS; HttpStatus: Integer);
begin
  if NOT FBusy then EXIT;

  if NOT IsSuccess then
    begin
      FLastHttpStatus:= HttpStatus;
      FLastNavError  := 'Navigation failed: ' + HttpStatusText(HttpStatus) + '. WebErrorStatus= ' + IntToStr(Ord(WebErrorStatus));

      { The error page has replaced any earlier page, so a settle still pending from that page would read the error page as content. }
      if (HttpStatus = 404) OR (HttpStatus = 410) then
        begin
          FSettleTimer.Enabled:= FALSE;
          RestartTimer(FGraceTimer, SettleDelay);
        end;
      EXIT;
    end;

  { This page replaced any earlier failure, so a later timeout must not report that failure. }
  FGraceTimer.Enabled:= FALSE;
  FLastNavError      := '';
  FLastHttpStatus    := 0;

  { Let the page finish its deferred scripts before reading it. See the header note about window visibility.
    A later navigation (a redirect that really landed somewhere else) simply restarts the settle, so the page we read is always the LAST one that loaded. }
  RestartTimer(FSettleTimer, SettleDelay);
end;


{ A new navigation may still replace a 404 page, so the 404 is not final yet. If the new one fails too, NavigationCompleted judges it afresh. }
procedure TWebPageReader.NavigationStarting(Sender: TCustomEdgeBrowser; Args: TNavigationStartingEventArgs);
begin
  FGraceTimer.Enabled:= FALSE;
end;


procedure TWebPageReader.SettleFire(Sender: TObject);
begin
  FSettleTimer.Enabled:= FALSE;
  if NOT FBusy then EXIT;

  FBrowser.ExecuteScript(FScript);   { asynchronous overload - the result arrives in ScriptCompleted }
end;


procedure TWebPageReader.ScriptCompleted(Sender: TCustomEdgeBrowser; AResult: HResult; CONST AResultObjectAsJson: string);
VAR Text: string;
begin
  if NOT FBusy then EXIT;

  if Winapi.Windows.Failed(AResult) then
    begin
      Fail(poNavFailed, 'The extractor script failed. HRESULT $'+ IntToHex(AResult, 8));
      EXIT;
    end;

  Text:= UnwrapEnvelope(AResultObjectAsJson);
  if Trim(Text) = ''
  then Fail(poEmpty, 'The page returned no text.')
  else Succeed(Text);
end;


procedure TWebPageReader.GuardFire(Sender: TObject);
VAR Info: string;
begin
  if FLastNavError = '' then
    begin
      Fail(poTimeout, 'Timed out after ' + IntToStr(TimeOut) + ' ms.');
      EXIT;
    end;

  Info:= FLastNavError + ' (and no later navigation succeeded within ' + IntToStr(TimeOut) + ' ms)';
  if FLastHttpStatus >= 400
  then Fail(poHttpError, Info)
  else Fail(poNavFailed, Info);
end;


{ A 404 or 410 that nothing replaced within SettleDelay. See NavigationCompleted. }
procedure TWebPageReader.GraceFire(Sender: TObject);
begin
  FGraceTimer.Enabled:= FALSE;
  if NOT FBusy then EXIT;

  Fail(poHttpError, FLastNavError);
end;


{-------------------------------------------------------------------------------------------------------------
   OUTCOME
-------------------------------------------------------------------------------------------------------------}

procedure TWebPageReader.StopTimers;
begin
  if FSettleTimer <> NIL then FSettleTimer.Enabled:= FALSE;
  if FGuardTimer  <> NIL then FGuardTimer .Enabled:= FALSE;
  if FGraceTimer  <> NIL then FGraceTimer .Enabled:= FALSE;
end;


procedure TWebPageReader.Succeed(CONST Text: string);
begin
  StopTimers;
  FBusy:= FALSE;
  if Assigned(OnPageText) then OnPageText(Self, Text);
end;


procedure TWebPageReader.Fail(Outcome: TPageOutcome; CONST Info: string);
begin
  StopTimers;
  FBusy:= FALSE;
  if Assigned(OnFailed) then OnFailed(Self, Outcome, Info);
end;


{-------------------------------------------------------------------------------------------------------------
   JAVASCRIPT
-------------------------------------------------------------------------------------------------------------}

class function TWebPageReader.BuildExtractScript(CONST ContentSelector, ExtraStrip: string): string;
VAR
  Strip: string;
  Root : string;
begin
  Strip:= DefaultStripSelectors;
  if ExtraStrip <> '' then Strip:= Strip + ',' + ExtraStrip;

  if ContentSelector = ''
  then Root:= 'document.body'
  else Root:= '(document.querySelector("' + ContentSelector + '") || document.body)';

  { We delete the chrome from the LIVE document instead of from a clone. Two reasons:
      - innerText on a DETACHED node gets no layout, so Chromium degrades it to textContent - the line breaks and the hidden-element filtering that make innerText worth using are both lost.
      - This reader opens one page and then the process exits, so mutating the page costs nothing. }
  Result:=
    '(function(){'+
    '  try{'+
    '    var junk = document.querySelectorAll("' + Strip + '");'+
    '    for (var i = junk.length-1; i >= 0; i--){ if (junk[i].parentNode) junk[i].parentNode.removeChild(junk[i]); }'+
    '    var root = ' + Root + ';'+
    '    if (!root) return "";'+
    '    return root.innerText || root.textContent || "";'+
    '  } catch(e) { return "[extractor error] " + e.message; }'+
    '})();';
end;


class function TWebPageReader.UnwrapEnvelope(CONST Envelope: string): string;
VAR Value: TJSONValue;
begin
  Result:= '';
  if Trim(Envelope) = '' then EXIT;

  Value:= TJSONObject.ParseJSONValue(Envelope);
  TRY
    if Value is TJSONString
    then Result:= TJSONString(Value).Value
    else
      if Value <> NIL
      then Result:= Value.ToString    { already an object/array - hand it back as it came }
      else Result:= Envelope;         { not valid JSON at all }
  FINALLY
    FreeAndNil(Value);
  END;
end;


end.
