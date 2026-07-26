UNIT LightVcl.Internet.Browser;

{=============================================================================================================
   2026.07.24
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Reads ONE web page with a REAL browser (WebView2) and hands back whatever a JavaScript snippet extracts
   from the live DOM.

   Why a real browser instead of an HTTP download:
     - It runs the site's JavaScript, so client-rendered pages (SPAs) actually have text to read.
     - It is a normal signed-in browser session, so it clears Cloudflare where an anonymous GET gets 403.
   For a plain server-rendered page a normal download is cheaper - see LightVcl.Internet.Download.WinInet.

   HOW TO USE
     1. Drop a TEdgeBrowser on your form (do NOT create it in code - it needs a parent window).
     2. Set Browser.UserDataFolder BEFORE the first navigation. See the warning below.
     3. Reader:= TWebPageReader.Create(Browser);  Reader.OnPageText:= ...;  Reader.OnFailed:= ...;
     4. Reader.ReadPage(Url, TWebPageReader.BuildExtractScript('', ''));

   The reader is EVENT-DRIVEN. It never blocks and never pumps the message loop itself.

   ------------------------------------------------------------------------------------------------------
   THREE TRAPS, all verified in C:\Delphi\Delphi 13\source\internet\Vcl.Edge.pas (Delphi 13)
   ------------------------------------------------------------------------------------------------------
   1. ExecuteScript has three overloads and TWO of them busy-spin the UI thread at 100% CPU:
        ExecuteScript(JS, AFinishedProc)      -> repeat..until around PeekMessage   (line 1667-1674)
        ExecuteScript(JS, AJsonPath): string  -> calls the one above                (line 1677)
        ExecuteScript(JS)                     -> asynchronous, raises OnExecuteScript (line ~1630)
      This unit uses ONLY the last one.

   2. UserDataFolder defaults to a folder named after the EXE:
        TPath.Combine(CLocalAppData, TPath.GetFileName(ParamStr(0) + '.WebView2'))   (line 967)
      So renaming the exe silently points it at a NEW, EMPTY profile and the logged-in session is gone.
      Always set UserDataFolder explicitly.

   3. Never hide or minimize the window that hosts the browser. Vcl.Edge maps the VCL Visible onto
      WebView2 IsVisible (line 2215) and SC_MINIMIZE forces IsVisible False (line 2223). Chromium
      throttles a non-visible page (reported ~1 s task intervals, requestAnimationFrame stopped), so
      the settle delay below can end up reading an unfinished page. Park the window OFF-SCREEN instead.
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
  TPageOutcome = (poOK, poNavFailed, poEmpty, poTimeout, poBrowserFailed);

  TPageTextEvent = procedure (Sender: TObject; CONST Text: string) of object;
  TPageFailEvent = procedure (Sender: TObject; Outcome: TPageOutcome; CONST Info: string) of object;

  TWebPageReader = class(TObject)
   private
     FBrowser     : TCustomEdgeBrowser;
     FSettleTimer : TTimer;
     FGuardTimer  : TTimer;
     FUrl         : string;
     FScript      : string;
     FBusy        : Boolean;
     FNavigated   : Boolean;   { the navigation has been issued - do not issue it twice }
     FLastNavError: string;    { remembered, not acted on. See NavigationCompleted. }
     procedure BrowserCreated     (Sender: TCustomEdgeBrowser; AResult: HResult);
     procedure NavigationCompleted(Sender: TCustomEdgeBrowser; IsSuccess: Boolean; WebErrorStatus: COREWEBVIEW2_WEB_ERROR_STATUS);
     procedure ScriptCompleted    (Sender: TCustomEdgeBrowser; AResult: HResult; CONST AResultObjectAsJson: string);
     procedure SettleFire(Sender: TObject);
     procedure GuardFire (Sender: TObject);
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


constructor TWebPageReader.Create(ABrowser: TCustomEdgeBrowser);
begin
  inherited Create;
  Assert(ABrowser <> NIL, 'TWebPageReader needs a browser!');
  FBrowser:= ABrowser;

  SettleDelay:= DefaultSettleDelay;
  TimeOut    := DefaultTimeout;

  { We own these events for as long as we live. }
  FBrowser.OnCreateWebViewCompleted:= BrowserCreated;
  FBrowser.OnNavigationCompleted   := NavigationCompleted;
  FBrowser.OnExecuteScript         := ScriptCompleted;

  FSettleTimer:= TTimer.Create(NIL);
  FSettleTimer.Enabled := FALSE;
  FSettleTimer.OnTimer := SettleFire;

  FGuardTimer:= TTimer.Create(NIL);
  FGuardTimer.Enabled := FALSE;
  FGuardTimer.OnTimer := GuardFire;
end;


destructor TWebPageReader.Destroy;
begin
  StopTimers;
  FreeAndNil(FSettleTimer);
  FreeAndNil(FGuardTimer);

  if FBrowser <> NIL then
   begin
     FBrowser.OnCreateWebViewCompleted:= NIL;
     FBrowser.OnNavigationCompleted   := NIL;
     FBrowser.OnExecuteScript         := NIL;
   end;

  inherited;
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

  { The guard timer is armed FIRST, so even a browser that never initialises ends the job. }
  FGuardTimer.Interval:= TimeOut;
  FGuardTimer.Enabled := TRUE;

  { The very first navigation only kicks off the (asynchronous) creation of the WebView. We do not rely on
    Vcl.Edge replaying the URL afterwards - BrowserCreated navigates explicitly instead. }
  if FBrowser.BrowserControlState = TCustomEdgeBrowser.TBrowserControlState.Created
  then
    begin
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

  if NOT FNavigated then
    begin
      FNavigated:= TRUE;
      FBrowser.Navigate(FUrl);
    end;
end;


{ WebView2 fires this several times for ONE logical page: a redirect replaces the first navigation, and the
  replaced one completes as FAILED with status UNKNOWN (Chromium's ERR_ABORTED). Measured 2026-07-24 on
  old.reddit.com, which reports exactly that and then loads fine.
  So a failure is NEVER final here - it is only remembered. The job ends when a navigation SUCCEEDS and the
  page settles, or when the guard timer runs out. The guard then reports the last remembered failure, which
  is a far more useful message than "timed out". }
procedure TWebPageReader.NavigationCompleted(Sender: TCustomEdgeBrowser; IsSuccess: Boolean; WebErrorStatus: COREWEBVIEW2_WEB_ERROR_STATUS);
begin
  if NOT FBusy then EXIT;

  if NOT IsSuccess then
    begin
      FLastNavError:= 'Navigation failed. WebErrorStatus= ' + IntToStr(Ord(WebErrorStatus));
      EXIT;
    end;

  { Let the page finish its deferred scripts before reading it. See the header note about window visibility.
    A later navigation (a redirect that really landed somewhere else) simply restarts the settle, so the page
    we read is always the LAST one that loaded. }
  FSettleTimer.Enabled := FALSE;
  FSettleTimer.Interval:= SettleDelay;
  FSettleTimer.Enabled := TRUE;
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
begin
  if FLastNavError <> ''
  then Fail(poNavFailed, FLastNavError + ' (and no later navigation succeeded within ' + IntToStr(TimeOut) + ' ms)')
  else Fail(poTimeout, 'Timed out after ' + IntToStr(TimeOut) + ' ms.');
end;


{-------------------------------------------------------------------------------------------------------------
   OUTCOME
-------------------------------------------------------------------------------------------------------------}

procedure TWebPageReader.StopTimers;
begin
  if FSettleTimer <> NIL then FSettleTimer.Enabled:= FALSE;
  if FGuardTimer  <> NIL then FGuardTimer .Enabled:= FALSE;
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
      - innerText on a DETACHED node gets no layout, so Chromium degrades it to textContent - the line
        breaks and the hidden-element filtering that make innerText worth using are both lost.
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
