UNIT LightVcl.Internet.PageDriver;

{=============================================================================================================
   2026.11.17
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Drives ONE web page in a TEdgeBrowser (WebView2) through a queue of steps:
   open an address, wait for an element, type text into it, click it, read text, run a script, give a file to a file input.
   It knows no site. The caller (a "recipe") holds the addresses and the CSS selectors of its own site.
   First user: c:\Projects\Projects AI\Social Robot\ (posts to X.com).
   Design: c:\Projects\Projects AI\Social Robot\Issues\Social Robot - design.md, section 5.

   HOW TO USE
     1. Drop a TEdgeBrowser on a form (do NOT create it in code - it needs a parent window).
        Set Browser.UserDataFolder before the first Run (trap 2 in the header of LightVcl.Internet.Browser).
     2. Driver:= TPageDriver.Create(Browser);  Driver.OnDone:= ...;  Driver.OnFailed:= ...;  Driver.OnLog:= ...;
     3. Add the steps, then start them:
          Driver.Navigate('https://example.com/');
          Driver.WaitFor('#editor');
          Driver.InsertText('#editor', 'Hello');
          Driver.Click('#send');
          Driver.Run;
     4. Exactly one OnDone or one OnFailed follows.
        OnFailed names the step that failed and why. A browser that does not start fails with the step name StepBrowserStart.
        A user-agent override that the browser refuses fails with the step name StepUserAgent (see USER AGENT OVERRIDE).
        ResultOf(Name) returns what a RunScript or ReadText step stored - also after a failure, until Clear.
     5. Free the driver BEFORE the browser (FormPreRelease is the place). The destructor unhooks the driver from the browser.

   The driver is EVENT-DRIVEN. It never blocks and never pumps the message loop.
   Timers and WebView2 completion handlers move it from step to step, so the caller's message loop must run.
   One driver per browser: the driver takes the browser's OnCreateWebViewCompleted, and so does TWebPageReader in LightVcl.Internet.Browser.

   USER AGENT OVERRIDE (opt-in; off by default)
     UserAgentOverride holds the JSON parameters of the DevTools call Emulation.setUserAgentOverride: userAgent, and optionally acceptLanguage, platform, userAgentMetadata.
     Definition: https://raw.githubusercontent.com/ChromeDevTools/devtools-protocol/master/pdl/domains/Emulation.pdl (saved in c:\Projects\Projects AI\Social Robot\Sources\cdp-Emulation.pdl\).
     "userAgentMetadata must be set for Client Hint headers to be sent" (same file), so a caller that changes the brand list sets it.
     Run sends the call before the first step whenever UserAgentOverride differs from the parameters sent last, then runs the steps.
     A queue with no steps only sends the call, so a caller can apply the override before it adds its first Navigate.
     Measured in WebView2 157 (c:\Projects\Projects AI\Social Robot\Temp\fix note - L2 bot detection.md): the override holds for later navigations, also to another site, and for dedicated workers.
     It does NOT reach a service worker or a frame of another site (an iframe from another host): both keep the browser's own values, in script and in the headers of their own requests.
     [UNVERIFIED: that attaching to those targets (Target.setAutoAttach) and sending the override in their sessions would cover them.]
     An empty UserAgentOverride sends nothing; it does not undo an override sent before.

   ATTACH FILE
     AttachFile gives one file to a file input (input type="file"), as if a user had picked it in the file dialog. No dialog opens.
     A page script cannot fill a file input; the DevTools call DOM.setFileInputFiles can. The step sends two DevTools calls:
       1. Runtime.evaluate with returnByValue false. Its expression returns the one element that matches the selector, so the answer carries result.objectId.
          The expression throws when no element or more than one matches, or when the element is not a file input. The answer then carries exceptionDetails, and the step fails with its reason.
       2. DOM.setFileInputFiles with files [the full path] and that objectId.
     Definitions: c:\Projects\Projects AI\Social Robot\Sources\cdp-js_protocol.pdl (Runtime.evaluate line 1540, returnByValue line 1558, RemoteObject.objectId line 1182, ExceptionDetails line 1377)
     and c:\Projects\Projects AI\Social Robot\Sources\cdp-DOM.pdl (setFileInputFiles lines 680-690), from https://github.com/ChromeDevTools/devtools-protocol, folder pdl.
     A missing file fails the step before any call. A relative path is expanded against the current folder when the step is added.
     The size of the input does not matter: a site often hides its file input (0x0). WaitFor demands a non-zero size, so it can never wait for such an input; use WaitUntil.
     The step does not check what the page did with the file: the caller reads it back, as after InsertText.
     Measured with SocialRobot.exe --selftest on WebView2 157 (c:\Projects\Projects AI\Social Robot\Test page\selftest.html, an input whose CSS sets width 0, height 0, opacity 0):
     DOM.setFileInputFiles needs no DOM.enable when it gets an objectId, and the page receives an input and a change event, both with isTrusted true.
     A site may make its file input only after a click on its own media button, and that click may open the Windows file dialog:
     measured on LinkedIn (c:\Projects\Projects AI\Social Robot\Probes\Out - linkedin attach - windows dialogs.txt): a script click (element.click()) on its "Media" button
     opened a visible #32770 window "Open" of msedgewebview2.exe. So such a caller sends InterceptFileChooser(TRUE) before that click and InterceptFileChooser(FALSE) when it is done.
     Page.setInterceptFileChooserDialog: "When file chooser interception is enabled, native file chooser dialog is not shown. Instead, a protocol event Page.fileChooserOpened is emitted."
     (https://chromedevtools.github.io/devtools-protocol/tot/Page/, saved in c:\Projects\Projects AI\Social Robot\Sources\cdp pages\02_chromedevtools.github.io_devtools-protocol_tot_Page.txt line 372).
     The driver receives no DevTools event; the event is not needed, because AttachFile sets the files itself.

   ------------------------------------------------------------------------------------------------------
   TRAPS
   ------------------------------------------------------------------------------------------------------
   1. Two of the three TCustomEdgeBrowser.ExecuteScript overloads busy-spin the UI thread: the repeat..until around PeekMessage (c:\Delphi\Delphi 13\source\internet\Vcl.Edge.pas lines 1667-1674), and the overload at line 1677 calls that one.
      The third one is asynchronous, but every call reports through the ONE OnExecuteScript event, so two answers cannot be told apart.
      This unit calls ICoreWebView2.ExecuteScript on Browser.DefaultInterface, with a handler of its own for each call.

   2. A navigation that fails is not final.
      A redirect makes the replaced navigation complete as FAILED (measured on old.reddit.com, TWebPageReader.NavigationCompleted in LightVcl.Internet.Browser).
      A Navigate step ends at the first SUCCESSFUL completion. A failure is only remembered, and named when the step times out.
      TEdgeBrowser.OnNavigationCompleted passes no HTTP status, so TNavHandler registers on the WebView2 interface itself.

   3. WebView2 calls a completion handler later, on the UI thread - maybe after the driver was freed.
      Each handler holds a plain reference to its driver. The destructor sets that reference to NIL in every handler still alive, and a handler with NIL does nothing.
      Every request also carries a ticket number. An answer that arrives after its step ended (a time-out, Cancel) carries an old ticket and is ignored.

   4. A Delphi exception must never travel into WebView2's native code.
      Every handler catches it and hands it to ApplicationHandleException (System.Classes), which a VCL program sets to TApplication.HandleException (c:\Delphi\Delphi 13\source\vcl\Vcl.Forms.pas line 12276).

   5. imPaste sends a synthetic paste event with the text in its clipboardData. The system clipboard is never touched.
      Such an event inserts text only when the page's own paste handler reads clipboardData and inserts it, as rich editors do.
      [UNVERIFIED: that Chromium itself never inserts the text of a synthetic paste event.]
      cmMouse and imKeyboard go through the DevTools protocol (domain Input) instead, so the page receives them as real input.

   6. Every selector and text goes into JavaScript through JsString (a JSON string literal), never glued in between quotes.
      A post text holds quotes, apostrophes and line breaks.

   7. A TTimer with Interval 0 never fires: TTimer.UpdateTimer calls SetTimer only when FInterval <> 0 (c:\Delphi\Delphi 13\source\vcl\Vcl.ExtCtrls.pas line 3233).
      restartTimer uses at least 1 ms, so Pause(0) still ends.

   The DevTools commands and their parameters: https://raw.githubusercontent.com/ChromeDevTools/devtools-protocol/master/pdl/domains/Input.pdl
   (saved as c:\Projects\Projects AI\Social Robot\Sources\cdp-Input.pdl).
   The values sent for Enter, Escape and Tab follow Puppeteer [UNVERIFIED: from memory of Puppeteer's keyboard layout, not read from its source].
=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, System.SysUtils, System.Classes, System.Contnrs, System.Generics.Collections,
  Vcl.ExtCtrls, Vcl.Edge, Winapi.WebView2;

CONST
  DefaultPollInterval = 250;               { ms between two checks of a wait step }
  BrowserStartTimeOut = 60000;             { ms for the WebView2 browser to start }
  StepBrowserStart    = 'browser start';   { the StepName that OnFailed passes when the WebView2 browser does not start }
  StepUserAgent       = 'user agent override';   { the StepName that OnFailed passes when Emulation.setUserAgentOverride fails }
  UserAgentTimeOut    = 10000;             { ms for the answer to Emulation.setUserAgentOverride }

TYPE
  TClickMode  = (cmMouse,        { DevTools mouse events at the centre of the element: a real, trusted click }
                 cmScript);      { element.click() in JavaScript }
  TInsertMode = (imKeyboard,     { focus the element, then Input.insertText line by line and an Enter key press between lines }
                 imExecCommand,  { document.execCommand('insertText') line by line, insertParagraph between lines }
                 imPaste);       { a synthetic paste event that carries the text in a DataTransfer object. The system clipboard is never touched. }

CONST
  ClickModeNames : array[TClickMode]  of string = ('cmMouse', 'cmScript');
  InsertModeNames: array[TInsertMode] of string = ('imKeyboard', 'imExecCommand', 'imPaste');

TYPE
  TPageDriver = class;
  TDriverDoneEvent = procedure(Sender: TPageDriver) of object;
  TDriverFailEvent = procedure(Sender: TPageDriver; CONST StepName, Info: string) of object;
  TDriverLogEvent  = procedure(Sender: TPageDriver; CONST Line: string) of object;

  TPageDriver = class(TObject)
  private type
    TStepKind    = (skNavigate, skWaitFor, skWaitGone, skWaitUntil, skRunScript, skReadText, skInsertText, skClick, skPressKey, skPause, skFocus, skAttachFile, skDevTools);
    TDelayAction = (daNone, daNextStep, daPoll, daPauseEnd);

    TDriverStep = class
      Kind      : TStepKind;
      Name      : string;        { used in the log, in OnFailed and as the key of a stored result }
      Target    : string;        { the address, the selector, the JavaScript or the key name }
      Text      : string;        { InsertText: the text. AttachFile: the full path of the file. }
      InsertMode: TInsertMode;
      ClickMode : TClickMode;
      TimeOut   : Integer;       { ms. For a Pause: its length }
    end;
  private
    FBrowser    : TCustomEdgeBrowser;
    FSteps      : TObjectList;   { of TDriverStep. Owns them. }
    FResults    : TDictionary<string, string>;
    FHandlers   : TObjectList;   { the WebView2 handlers still alive. Does NOT own them: COM reference counting frees them. See trap 3. }
    FStepTimer  : TTimer;        { the time-out of the current step, or of the browser start }
    FDelayTimer : TTimer;        { the pause between two polls, a Pause step, and the hop to the next step }
    FDelayAction: TDelayAction;
    FBusy       : Boolean;
    FStarting   : Boolean;       { Run waits for the WebView to be created }
    FWaitingNav : Boolean;       { a Navigate step waits for its successful completion }
    FIndex      : Integer;       { the current step in FSteps }
    FTicket     : Integer;       { the ticket of the answer the driver waits for. See trap 3. }
    FPhase      : Integer;       { 0 = the first request of a step. 1 = its DevTools calls or its second script. AttachFile: 0 = Runtime.evaluate, 1 = DOM.setFileInputFiles }
    FStepStart  : UInt64;        { GetTickCount64 at the start of the current step }
    FLastSeen   : string;        { the last poll answer or navigation failure. Named when the step times out. }
    FCallMethods: TStringList;   { the DevTools calls still to send in this step... }
    FCallParams : TStringList;   { ...and their parameters, as JSON }
    FLastCall   : string;        { the DevTools call in flight, for the error message }
    FNavHooked  : Boolean;
    FNavToken   : EventRegistrationToken;
    FUaSending  : Boolean;       { Emulation.setUserAgentOverride is in flight; no step runs yet }
    FUaSent     : string;        { the parameters of the last override the browser accepted }

    function  addStep(Kind: TStepKind; CONST Name, Target: string; TimeOut: Integer): TDriverStep;
    function  current: TDriverStep;
    function  stepLabel: string;
    function  elapsedMs: string;
    procedure doLog(CONST Line: string);

    procedure browserCreated(Sender: TCustomEdgeBrowser; AResult: HResult);
    function  hookNavigation: Boolean;
    procedure navigationCompleted(IsSuccess: Boolean; WebErrorStatus: COREWEBVIEW2_WEB_ERROR_STATUS; HttpStatus: Integer);
    procedure answerArrived(Ticket: Integer; ErrorCode: HResult; CONST Json: string);

    procedure startQueue;
    procedure sendUserAgent;
    procedure userAgentAnswered(ErrorCode: HResult; CONST Json: string);
    procedure failUserAgent(CONST Info: string);

    procedure delay(Ms: Integer; Action: TDelayAction);
    procedure delayFire(Sender: TObject);
    procedure stepTimeOut(Sender: TObject);

    procedure nextStep;
    procedure startStep;
    procedure startNavigate;
    procedure sendPoll;
    procedure sendScript(CONST JavaScript: string);
    procedure addCall(CONST Method, Params: string);
    procedure queueKey(CONST Key: string);
    procedure sendNextCall;
    procedure insertFocused(CONST Json: string);
    procedure insertAnswered(CONST Json: string);
    procedure clickLocated(CONST Json: string);
    procedure startAttach;
    procedure attachLocated(CONST Json: string);

    procedure finishStep;
    procedure failStep(CONST Info: string);
    procedure failRun(CONST StepName, Info: string);
    procedure succeedRun;
    procedure stopAll;
  public
    PollInterval: Integer;       { ms between two checks of a wait step. Default 250. }
    OnDone      : TDriverDoneEvent;
    OnFailed    : TDriverFailEvent;
    OnLog       : TDriverLogEvent;
    UserAgentOverride: string;   { the JSON parameters of Emulation.setUserAgentOverride. '' (the default) = the browser's own user agent. See USER AGENT OVERRIDE in the header. }

    constructor Create(ABrowser: TCustomEdgeBrowser);
    destructor Destroy; override;

    { Building the queue. Each call adds one step. Name is used in the log, in OnFailed, and as the key of a stored result. }
    procedure Clear;   { removes the steps and the stored results }
    procedure Navigate  (CONST Url: string; TimeOut: Integer= 30000);
    procedure WaitFor   (CONST Selector: string; TimeOut: Integer= 15000);                { an element exists AND has a non-zero size }
    procedure WaitGone  (CONST Selector: string; TimeOut: Integer= 15000);                { no visible element matches any more }
    procedure WaitUntil (CONST Name, JsCondition: string; TimeOut: Integer= 15000);       { a JavaScript expression that becomes true }
    procedure RunScript (CONST Name, JavaScript: string; TimeOut: Integer= 10000);        { stores the unwrapped result under Name }
    procedure ReadText  (CONST Name, Selector: string; TimeOut: Integer= 10000);          { innerText of the first match, stored under Name; fails if none }
    procedure InsertText(CONST Selector, Text: string; Mode: TInsertMode= imKeyboard; TimeOut: Integer= 15000);
    procedure Click     (CONST Selector: string; Mode: TClickMode= cmMouse; TimeOut: Integer= 10000);
    procedure Focus     (CONST Selector: string; TimeOut: Integer= 10000);                { focuses the first match and puts the caret at its end; fails if none }
    procedure PressKey  (CONST Key: string);    { 'Enter', 'Escape', 'Tab', 'Backspace', 'SelectAll' (Ctrl+A) - sent to the focused element through Input.dispatchKeyEvent. Another key raises EArgumentException. }
    procedure AttachFile(CONST Selector, FilePath: string; TimeOut: Integer= 10000);  { gives the file to the ONE file input that matches Selector, hidden or not; fails if none or more than one match, or if the file does not exist. See ATTACH FILE in the header. }
    procedure InterceptFileChooser(Enabled: Boolean);   { Page.setInterceptFileChooserDialog. While on, a page that opens its file chooser gets no Windows file dialog. See ATTACH FILE in the header. }
    procedure Pause     (Ms: Integer);

    procedure Run;      { starts the queue. Ends in exactly one OnDone or one OnFailed. }
    procedure Cancel;   { stops the queue; no event fires after it }

    function  ResultOf(CONST Name: string): string;   { '' when no step stored that name }
    function  Busy: Boolean;
    class function JsString(CONST S: string): string;  { S as a JSON string literal, safe to put inside JavaScript }
  end;


IMPLEMENTATION

USES
  System.StrUtils, System.Types, System.JSON,
  LightCore, LightVcl.Internet.Browser;

CONST
  BrowserHint = 'Is the WebView2 runtime installed, and is WebView2Loader.dll x64 beside the exe?';
  PressKeyTimeOut = 5000;


{-------------------------------------------------------------------------------------------------------------
   WEBVIEW2 HANDLERS - see traps 3 and 4 in the header
-------------------------------------------------------------------------------------------------------------}
TYPE
  TDriverHandler = class(TInterfacedObject)
  private
    FDriver: TPageDriver;
    FTicket: Integer;
  public
    constructor Create(ADriver: TPageDriver; ATicket: Integer);
    destructor Destroy; override;
  end;

  { One class answers both a script and a DevTools call: the two Invoke methods have the same signature (c:\Delphi\Delphi 13\source\rtl\win\Winapi.WebView2.pas lines 1859 and 1902). }
  TAnswerHandler = class(TDriverHandler, ICoreWebView2ExecuteScriptCompletedHandler, ICoreWebView2CallDevToolsProtocolMethodCompletedHandler)
  public
    function Invoke(ErrorCode: HResult; ResultJson: PWideChar): HResult; stdcall;
  end;

  TNavHandler = class(TDriverHandler, ICoreWebView2NavigationCompletedEventHandler)
  public
    function Invoke(CONST Sender: ICoreWebView2; CONST Args: ICoreWebView2NavigationCompletedEventArgs): HResult; stdcall;
  end;


constructor TDriverHandler.Create(ADriver: TPageDriver; ATicket: Integer);
begin
  inherited Create;
  FDriver:= ADriver;
  FTicket:= ATicket;
  FDriver.FHandlers.Add(Self);
end;


destructor TDriverHandler.Destroy;
begin
  if FDriver <> NIL
  then FDriver.FHandlers.Remove(Self);
  inherited;
end;


function TAnswerHandler.Invoke(ErrorCode: HResult; ResultJson: PWideChar): HResult;
begin
  Result:= S_OK;
  if FDriver = NIL then EXIT;   { the driver was freed }

  TRY
    FDriver.answerArrived(FTicket, ErrorCode, string(ResultJson));
  EXCEPT
    if Assigned(ApplicationHandleException)
    then ApplicationHandleException(Self)
    else RAISE;
  END;
end;


function TNavHandler.Invoke(CONST Sender: ICoreWebView2; CONST Args: ICoreWebView2NavigationCompletedEventArgs): HResult;
VAR
  IsSuccess     : Integer;
  WebErrorStatus: COREWEBVIEW2_WEB_ERROR_STATUS;
  HttpStatus    : Integer;
  Args2         : ICoreWebView2NavigationCompletedEventArgs2;
begin
  Result:= S_OK;
  if FDriver = NIL then EXIT;

  TRY
    IsSuccess     := 0;
    WebErrorStatus:= COREWEBVIEW2_WEB_ERROR_STATUS_UNKNOWN;
    HttpStatus    := 0;

    Args.Get_IsSuccess(IsSuccess);
    Args.Get_WebErrorStatus(WebErrorStatus);
    if Supports(Args, ICoreWebView2NavigationCompletedEventArgs2, Args2)
    then Args2.Get_HttpStatusCode(HttpStatus);

    FDriver.navigationCompleted(IsSuccess <> 0, WebErrorStatus, HttpStatus);
  EXCEPT
    if Assigned(ApplicationHandleException)
    then ApplicationHandleException(Self)
    else RAISE;
  END;
end;



{-------------------------------------------------------------------------------------------------------------
   HELPERS
-------------------------------------------------------------------------------------------------------------}

{ See trap 7 in the header. }
procedure restartTimer(Timer: TTimer; Interval: Integer);
begin
  Timer.Enabled:= FALSE;
  if Interval < 1
  then Timer.Interval:= 1
  else Timer.Interval:= Interval;
  Timer.Enabled:= TRUE;
end;


{ Every internal script returns an object: ok, fatal (waiting longer cannot help), info, and for a mouse click x and y.
  An answer that is no object - the page navigated away while the script ran, or the script did not parse - gives Ok FALSE and names the raw answer. }
procedure readAnswer(CONST Json: string; OUT Ok, Fatal: Boolean; OUT Info: string; OUT X, Y: Integer);
VAR
  Value, Field: TJSONValue;
begin
  Ok   := FALSE;
  Fatal:= FALSE;
  Info := '';
  X    := 0;
  Y    := 0;

  Value:= TJSONObject.ParseJSONValue(Json);
  TRY
    if NOT (Value is TJSONObject) then
      begin
        Info:= 'the script returned ' + Copy(Json, 1, 200);
        EXIT;
      end;

    Field:= TJSONObject(Value).GetValue('ok');
    Ok:= (Field is TJSONBool) AND TJSONBool(Field).AsBoolean;

    Field:= TJSONObject(Value).GetValue('fatal');
    Fatal:= (Field is TJSONBool) AND TJSONBool(Field).AsBoolean;

    Field:= TJSONObject(Value).GetValue('info');
    if Field <> NIL then Info:= Field.Value;

    Field:= TJSONObject(Value).GetValue('x');
    if Field is TJSONNumber then X:= TJSONNumber(Field).AsInt;

    Field:= TJSONObject(Value).GetValue('y');
    if Field is TJSONNumber then Y:= TJSONNumber(Field).AsInt;
  FINALLY
    FreeAndNil(Value);
  END;
end;


{ Wraps the body of an internal script in a function that never throws: an exception comes back as a fatal answer. }
function wrapScript(CONST Body: string): string;
begin
  Result:= '(function(){try{' + Body + '}catch(e){return {ok:false,fatal:true,info:"script error: "+e.message};}})()';
end;


{ The first line of every script that works on one element. }
function findElement(CONST Selector: string): string;
VAR Sel: string;
begin
  Sel:= TPageDriver.JsString(Selector);
  Result:= 'var e=document.querySelector(' + Sel + ');'+
           'if(!e) return {ok:false,info:"not found: "+' + Sel + '};';
end;


CONST
  { The element is disabled when its disabled property or its aria-disabled attribute says so. }
  DisabledCheck = 'if(e.disabled===true||e.getAttribute("aria-disabled")==="true") return {ok:false,info:"disabled"};';

  { "instant" because a page with CSS scroll-behavior:smooth would otherwise still be scrolling when the script reads the position. }
  ScrollToCentre = 'e.scrollIntoView({block:"center",inline:"center",behavior:"instant"});';


function waitForScript(CONST Selector: string): string;
begin
  Result:= wrapScript(
    'var e=document.querySelector(' + TPageDriver.JsString(Selector) + ');'+
    'if(!e) return {ok:false,info:"not found"};'+
    'var r=e.getBoundingClientRect();'+
    'if(r.width>0&&r.height>0) return {ok:true,info:"found"};'+
    'return {ok:false,info:"found, but its size is "+Math.round(r.width)+"x"+Math.round(r.height)};');
end;


function waitGoneScript(CONST Selector: string): string;
begin
  Result:= wrapScript(
    'var l=document.querySelectorAll(' + TPageDriver.JsString(Selector) + '),n=0;'+
    'for(var i=0;i<l.length;i++){var r=l[i].getBoundingClientRect(); if(r.width>0&&r.height>0) n++;}'+
    'return {ok:n===0,info:n+" visible"};');
end;


{ An exception in the condition is not fatal: the element it reads may simply not exist yet.
  The condition is put on lines of its own, so a // comment at its end cannot swallow the closing bracket. }
function waitUntilScript(CONST JsCondition: string): string;
begin
  Result:= '(function(){try{var v=(' + #10 + JsCondition + #10 + ');'+
           'return {ok:!!v,info:"value: "+String(v).substring(0,100)};'+
           '}catch(e){return {ok:false,info:"exception: "+e.message};}})()';
end;


function readTextScript(CONST Selector: string): string;
begin
  Result:= wrapScript(findElement(Selector) +
    'return {ok:true,info:e.innerText};');
end;


{ Focuses the element and puts the caret at its end. The answer says whether the focus landed, for the log. }
function focusScript(CONST Selector: string): string;
begin
  Result:= wrapScript(findElement(Selector) + ScrollToCentre +
    'e.focus();'+
    'if(e.tagName==="INPUT"||e.tagName==="TEXTAREA"){var n=e.value.length;try{e.setSelectionRange(n,n);}catch(x){}}'+
    'else{var s=window.getSelection(),r=document.createRange();r.selectNodeContents(e);r.collapse(false);s.removeAllRanges();s.addRange(r);}'+
    'var a=document.activeElement;'+
    'return {ok:true,info:"focused: "+(a===e||e.contains(a))+", document.hasFocus: "+document.hasFocus()};');
end;


function execCommandScript(CONST Lines: TStringDynArray): string;
VAR
  i: Integer;
  JsLines: string;
begin
  JsLines:= '[';
  for i:= 0 to High(Lines) do
    begin
      if i > 0 then JsLines:= JsLines + ',';
      JsLines:= JsLines + TPageDriver.JsString(Lines[i]);
    end;
  JsLines:= JsLines + ']';

  Result:= wrapScript(
    'var l=' + JsLines + ',ok=true;'+
    'for(var i=0;i<l.length;i++){'+
    '  if(i>0&&!document.execCommand("insertParagraph",false,null)) ok=false;'+
    '  if(l[i]!==""&&!document.execCommand("insertText",false,l[i])) ok=false;'+
    '}'+
    'return ok?{ok:true,info:""}:{ok:false,info:"document.execCommand returned false"};');
end;


function pasteScript(CONST Text: string): string;
begin
  Result:= wrapScript(
    'var t=document.activeElement;'+
    'if(!t||t===document.body) return {ok:false,info:"no element has the focus"};'+
    'var d=new DataTransfer();'+
    'd.setData("text/plain",' + TPageDriver.JsString(Text) + ');'+
    'var v=new ClipboardEvent("paste",{clipboardData:d,bubbles:true,cancelable:true});'+
    't.dispatchEvent(v);'+
    'return {ok:true,info:"paste event sent to "+t.tagName.toLowerCase()+", defaultPrevented: "+v.defaultPrevented};');
end;


{ cmMouse: returns the centre of the element in CSS pixels, rounded, so Delphi never formats a float (a decimal comma would break the JSON).
  A covered element fails and names what covers it: that is how a popup in front of a button shows up in the log. }
function locateScript(CONST Selector: string): string;
begin
  Result:= wrapScript(findElement(Selector) + ScrollToCentre + DisabledCheck +
    'var r=e.getBoundingClientRect(),x=Math.round(r.left+r.width/2),y=Math.round(r.top+r.height/2);'+
    'var h=document.elementFromPoint(x,y);'+
    'if(!h||!(h===e||e.contains(h))){'+
    '  var d=h?h.tagName.toLowerCase()+" data-testid="+(h.getAttribute("data-testid")||"")+" text="+(h.innerText||"").replace(/\s+/g," ").substring(0,60):"nothing";'+
    '  return {ok:false,info:"covered by "+d};'+
    '}'+
    'return {ok:true,x:x,y:y,info:"mouse click at "+x+","+y};');
end;


function scriptClickScript(CONST Selector: string): string;
begin
  Result:= wrapScript(findElement(Selector) + ScrollToCentre + DisabledCheck +
    'e.click();'+
    'return {ok:true,info:"clicked"};');
end;


{ The two DevTools key events of one key press. FALSE for a key this unit does not know.
  A key that types text is sent as keyDown with that text; a key that types nothing as rawKeyDown: the definition of "text" says it is "Not needed for keyUp and rawKeyDown events" (Input.pdl lines 110-111). }
function keyEvents(CONST Key: string; OUT Down, Up: string): Boolean;
begin
  Result:= TRUE;
  if SameText(Key, 'Enter') then
    begin
      Down:= '{"type":"keyDown","key":"Enter","code":"Enter","windowsVirtualKeyCode":13,"text":"\r","unmodifiedText":"\r"}';
      Up  := '{"type":"keyUp","key":"Enter","code":"Enter","windowsVirtualKeyCode":13}';
    end
  else
    if SameText(Key, 'Escape') then
      begin
        Down:= '{"type":"rawKeyDown","key":"Escape","code":"Escape","windowsVirtualKeyCode":27}';
        Up  := '{"type":"keyUp","key":"Escape","code":"Escape","windowsVirtualKeyCode":27}';
      end
    else
      if SameText(Key, 'Tab') then
        begin
          Down:= '{"type":"rawKeyDown","key":"Tab","code":"Tab","windowsVirtualKeyCode":9}';
          Up  := '{"type":"keyUp","key":"Tab","code":"Tab","windowsVirtualKeyCode":9}';
        end
      else
        if SameText(Key, 'Backspace') then
          begin
            { VK_BACK = 8 (c:\Delphi\Delphi 13\source\rtl\win\Winapi.Windows.pas) }
            Down:= '{"type":"rawKeyDown","key":"Backspace","code":"Backspace","windowsVirtualKeyCode":8}';
            Up  := '{"type":"keyUp","key":"Backspace","code":"Backspace","windowsVirtualKeyCode":8}';
          end
        else
          if SameText(Key, 'SelectAll') then
            begin
              { Ctrl+A. "modifiers" 2 is Ctrl, and "commands" carries the editing command (Input.pdl, parameters of dispatchKeyEvent). VK_A = $41 = 65 (Winapi.Windows.pas). }
              Down:= '{"type":"rawKeyDown","key":"a","code":"KeyA","windowsVirtualKeyCode":65,"modifiers":2,"commands":["selectAll"]}';
              Up  := '{"type":"keyUp","key":"a","code":"KeyA","windowsVirtualKeyCode":65,"modifiers":2}';
            end
          else
            begin
              Down:= '';
              Up  := '';
              Result:= FALSE;
            end;
end;


{ AttachFile, call 1: the expression of Runtime.evaluate. It returns the element itself, not an answer object, so the DevTools answer carries its objectId.
  Every refusal is a thrown Error, which comes back as exceptionDetails. No wrapScript: the exception must reach the answer. }
function attachLocateScript(CONST Selector: string): string;
VAR Sel: string;
begin
  Sel:= TPageDriver.JsString(Selector);
  Result:= '(function(){'+
           'var l=document.querySelectorAll(' + Sel + ');'+
           'if(l.length!==1) throw new Error(l.length+" elements match "+' + Sel + '+", not 1");'+
           'var e=l[0];'+
           'if(e.tagName!=="INPUT"||e.type!=="file") throw new Error("not a file input: "+e.tagName.toLowerCase()+(e.type?" type="+e.type:""));'+
           'return e;})()';
end;


{ Reads the answer of Runtime.evaluate: TRUE with the objectId of its result, or FALSE with the reason it has none.
  A thrown exception comes back as exceptionDetails (cdp-js_protocol.pdl line 1596). The reason is its exception.description, else its text,
  and only the first line: in the measured answers the lines after it are the JavaScript stack. }
function readEvaluateAnswer(CONST Json: string; OUT ObjectId, Reason: string): Boolean;
VAR
  Value, Details, Exc, Res, Field: TJSONValue;
  Cut: Integer;
begin
  ObjectId:= '';
  Reason  := '';

  Value:= TJSONObject.ParseJSONValue(Json);
  TRY
    if NOT (Value is TJSONObject) then
      begin
        Reason:= 'Runtime.evaluate returned ' + Copy(Json, 1, 200);
        EXIT(FALSE);
      end;

    Details:= TJSONObject(Value).GetValue('exceptionDetails');
    if Details is TJSONObject then
      begin
        Field:= NIL;
        Exc:= TJSONObject(Details).GetValue('exception');
        if Exc is TJSONObject
        then Field:= TJSONObject(Exc).GetValue('description');
        if Field = NIL
        then Field:= TJSONObject(Details).GetValue('text');

        if Field <> NIL
        then Reason:= Field.Value
        else Reason:= Copy(Json, 1, 200);

        Cut:= Pos(#10, Reason);
        if Cut > 0
        then Reason:= TrimRight(Copy(Reason, 1, Cut-1));
        EXIT(FALSE);
      end;

    Res:= TJSONObject(Value).GetValue('result');
    if Res is TJSONObject then
      begin
        Field:= TJSONObject(Res).GetValue('objectId');
        if Field is TJSONString
        then ObjectId:= Field.Value;
      end;

    if ObjectId = '' then
      begin
        Reason:= 'Runtime.evaluate returned no objectId: ' + Copy(Json, 1, 200);
        EXIT(FALSE);
      end;

    Result:= TRUE;
  FINALLY
    FreeAndNil(Value);
  END;
end;



{-------------------------------------------------------------------------------------------------------------
   CREATE / DESTROY
-------------------------------------------------------------------------------------------------------------}

constructor TPageDriver.Create(ABrowser: TCustomEdgeBrowser);
begin
  inherited Create;
  Assert(ABrowser <> NIL, 'TPageDriver needs a browser!');
  FBrowser:= ABrowser;
  PollInterval:= DefaultPollInterval;

  FSteps      := TObjectList.Create(TRUE);
  FHandlers   := TObjectList.Create(FALSE);
  FResults    := TDictionary<string, string>.Create;
  FCallMethods:= TStringList.Create;
  FCallParams := TStringList.Create;

  FStepTimer:= TTimer.Create(NIL);
  FStepTimer.Enabled:= FALSE;
  FStepTimer.OnTimer:= stepTimeOut;

  FDelayTimer:= TTimer.Create(NIL);
  FDelayTimer.Enabled:= FALSE;
  FDelayTimer.OnTimer:= delayFire;

  { Ours for as long as we live. See "One driver per browser" in the header. }
  FBrowser.OnCreateWebViewCompleted:= browserCreated;
end;


destructor TPageDriver.Destroy;
VAR i: Integer;
begin
  stopAll;

  { See trap 3. The handlers first, so the navigation handler that remove_NavigationCompleted releases below finds a NIL driver. }
  for i:= 0 to FHandlers.Count-1 do
    TDriverHandler(FHandlers[i]).FDriver:= NIL;

  if FBrowser <> NIL then
    begin
      FBrowser.OnCreateWebViewCompleted:= NIL;
      if FNavHooked AND (FBrowser.DefaultInterface <> NIL)
      then FBrowser.DefaultInterface.remove_NavigationCompleted(FNavToken);
    end;

  FreeAndNil(FStepTimer);
  FreeAndNil(FDelayTimer);
  FreeAndNil(FCallParams);
  FreeAndNil(FCallMethods);
  FreeAndNil(FResults);
  FreeAndNil(FHandlers);
  FreeAndNil(FSteps);
  inherited;
end;



{-------------------------------------------------------------------------------------------------------------
   BUILDING THE QUEUE
-------------------------------------------------------------------------------------------------------------}

function TPageDriver.addStep(Kind: TStepKind; CONST Name, Target: string; TimeOut: Integer): TDriverStep;
begin
  Assert(NOT FBusy, 'TPageDriver: a step cannot be added while the queue runs!');
  Result:= TDriverStep.Create;
  Result.Kind   := Kind;
  Result.Name   := Name;
  Result.Target := Target;
  Result.TimeOut:= TimeOut;
  FSteps.Add(Result);
end;


procedure TPageDriver.Clear;
begin
  Assert(NOT FBusy, 'TPageDriver.Clear: the queue still runs. Call Cancel first.');
  FSteps.Clear;
  FResults.Clear;
end;


procedure TPageDriver.Navigate(CONST Url: string; TimeOut: Integer);
begin
  addStep(skNavigate, 'Navigate ' + Url, Url, TimeOut);
end;


procedure TPageDriver.WaitFor(CONST Selector: string; TimeOut: Integer);
begin
  addStep(skWaitFor, 'WaitFor ' + Selector, Selector, TimeOut);
end;


procedure TPageDriver.WaitGone(CONST Selector: string; TimeOut: Integer);
begin
  addStep(skWaitGone, 'WaitGone ' + Selector, Selector, TimeOut);
end;


procedure TPageDriver.WaitUntil(CONST Name, JsCondition: string; TimeOut: Integer);
begin
  addStep(skWaitUntil, Name, JsCondition, TimeOut);
end;


procedure TPageDriver.RunScript(CONST Name, JavaScript: string; TimeOut: Integer);
begin
  addStep(skRunScript, Name, JavaScript, TimeOut);
end;


procedure TPageDriver.ReadText(CONST Name, Selector: string; TimeOut: Integer);
begin
  addStep(skReadText, Name, Selector, TimeOut);
end;


procedure TPageDriver.InsertText(CONST Selector, Text: string; Mode: TInsertMode; TimeOut: Integer);
VAR Step: TDriverStep;
begin
  Step:= addStep(skInsertText, 'InsertText ' + Selector + ' ' + InsertModeNames[Mode], Selector, TimeOut);
  Step.Text      := Text;
  Step.InsertMode:= Mode;
end;


procedure TPageDriver.Click(CONST Selector: string; Mode: TClickMode; TimeOut: Integer);
VAR Step: TDriverStep;
begin
  Step:= addStep(skClick, 'Click ' + Selector + ' ' + ClickModeNames[Mode], Selector, TimeOut);
  Step.ClickMode:= Mode;
end;


procedure TPageDriver.PressKey(CONST Key: string);
VAR Down, Up: string;
begin
  if NOT keyEvents(Key, Down, Up)
  then raise EArgumentException.Create('TPageDriver.PressKey: unknown key "' + Key + '". Known keys: Enter, Escape, Tab, Backspace, SelectAll.');

  addStep(skPressKey, 'PressKey ' + Key, Key, PressKeyTimeOut);
end;


procedure TPageDriver.Focus(CONST Selector: string; TimeOut: Integer);
begin
  addStep(skFocus, 'Focus ' + Selector, Selector, TimeOut);
end;


procedure TPageDriver.AttachFile(CONST Selector, FilePath: string; TimeOut: Integer);
VAR Step: TDriverStep;
begin
  Step:= addStep(skAttachFile, 'AttachFile ' + Selector, Selector, TimeOut);
  Step.Text:= ExpandFileName(FilePath);   { DOM.setFileInputFiles always gets a full path, never one that depends on a current folder }
end;


procedure TPageDriver.InterceptFileChooser(Enabled: Boolean);
VAR Step: TDriverStep;
begin
  if Enabled then
    begin
      Step:= addStep(skDevTools, 'Page.enable', 'Page.enable', PressKeyTimeOut);
      Step.Text:= '{}';
    end;
  Step:= addStep(skDevTools, 'InterceptFileChooser ' + BoolToStr(Enabled, TRUE), 'Page.setInterceptFileChooserDialog', PressKeyTimeOut);
  if Enabled
  then Step.Text:= '{"enabled":true}'
  else Step.Text:= '{"enabled":false}';
end;


procedure TPageDriver.Pause(Ms: Integer);
begin
  addStep(skPause, 'Pause ' + IntToStr(Ms) + ' ms', '', Ms);
end;



{-------------------------------------------------------------------------------------------------------------
   RUN / CANCEL / RESULTS
-------------------------------------------------------------------------------------------------------------}

procedure TPageDriver.Run;
begin
  Assert(NOT FBusy, 'TPageDriver.Run: the queue already runs!');
  FBusy    := TRUE;
  FStarting:= FALSE;
  FIndex   := -1;
  Inc(FTicket);

  if FBrowser.BrowserControlState = TCustomEdgeBrowser.TBrowserControlState.Created then
    begin
      startQueue;
      EXIT;
    end;

  { # Browser start }
  { The first Run creates the WebView. browserCreated follows - synchronously when CreateWebView fails at once (Vcl.Edge.pas line 1965). }
  FStarting := TRUE;
  FStepStart:= GetTickCount64;
  doLog('Starting the WebView2 browser');
  restartTimer(FStepTimer, BrowserStartTimeOut);
  if FBrowser.BrowserControlState <> TCustomEdgeBrowser.TBrowserControlState.Creating
  then FBrowser.CreateWebView;
end;


procedure TPageDriver.Cancel;
begin
  if FBusy then doLog('Cancelled');
  stopAll;
end;


function TPageDriver.ResultOf(CONST Name: string): string;
begin
  if NOT FResults.TryGetValue(Name, Result)
  then Result:= '';
end;


function TPageDriver.Busy: Boolean;
begin
  Result:= FBusy;
end;


class function TPageDriver.JsString(CONST S: string): string;
VAR Value: TJSONString;
begin
  Value:= TJSONString.Create(S);
  TRY
    Result:= Value.ToJSON;   { escapes quotes, backslashes, control characters and every character above 127 }
  FINALLY
    FreeAndNil(Value);
  END;
end;



{-------------------------------------------------------------------------------------------------------------
   BROWSER
-------------------------------------------------------------------------------------------------------------}

{ Vcl.Edge calls this from inside a WebView2 completion handler, so an exception must not leave it (trap 4). }
procedure TPageDriver.browserCreated(Sender: TCustomEdgeBrowser; AResult: HResult);
VAR Info: string;
begin
  if NOT FStarting then EXIT;   { not started by Run, or after a Cancel or a time-out }

  TRY
    FStarting:= FALSE;
    FStepTimer.Enabled:= FALSE;

    if Winapi.Windows.Failed(AResult) then
      begin
        Info:= 'WebView2 could not be created. HRESULT $' + IntToHex(AResult, 8) + '. ' + BrowserHint;
        doLog('FAILED: ' + StepBrowserStart + ': ' + Info);
        failRun(StepBrowserStart, Info);
        EXIT;
      end;

    doLog('The WebView2 browser started in ' + elapsedMs + ' ms. Runtime version ' + FBrowser.BrowserVersionInfo);
    startQueue;
  EXCEPT
    if Assigned(ApplicationHandleException)
    then ApplicationHandleException(Self)
    else RAISE;
  END;
end;


{ The browser exists. The user-agent override goes first when it changed (USER AGENT OVERRIDE in the header), then the first step.
  The first step always starts through the timer, so it never runs inside the WebView2 handler that called browserCreated.
  The override call itself is sent at once; its answer arrives later, in its own handler. }
procedure TPageDriver.startQueue;
begin
  if (UserAgentOverride <> '') AND (UserAgentOverride <> FUaSent)
  then sendUserAgent
  else delay(0, daNextStep);
end;


{ Has its own state (FUaSending), because no step is current yet: FIndex is -1, and current would assert. }
procedure TPageDriver.sendUserAgent;
VAR
  Handler: ICoreWebView2CallDevToolsProtocolMethodCompletedHandler;
  HR: HResult;
begin
  if FBrowser.DefaultInterface = NIL then
    begin
      failUserAgent('the browser has no WebView');
      EXIT;
    end;

  FUaSending:= TRUE;
  FStepStart:= GetTickCount64;
  Inc(FTicket);
  doLog('Sending Emulation.setUserAgentOverride ' + UserAgentOverride);
  restartTimer(FStepTimer, UserAgentTimeOut);

  Handler:= TAnswerHandler.Create(Self, FTicket);   { held in a variable, never passed as a temporary }
  HR:= FBrowser.DefaultInterface.CallDevToolsProtocolMethod('Emulation.setUserAgentOverride', PWideChar(UserAgentOverride), Handler);
  if Winapi.Windows.Failed(HR)
  then failUserAgent('CallDevToolsProtocolMethod refused Emulation.setUserAgentOverride. HRESULT $' + IntToHex(HR, 8));
end;


{ A failed call is final: the run stops, so no page ever sees the browser's own identity when the caller asked for another one.
  On a failure WebView2 puts its information about the error into the answer JSON (https://learn.microsoft.com/en-us/microsoft-edge/webview2/reference/win32/icorewebview2, CallDevToolsProtocolMethod). }
procedure TPageDriver.userAgentAnswered(ErrorCode: HResult; CONST Json: string);
begin
  FUaSending:= FALSE;
  FStepTimer.Enabled:= FALSE;

  if Winapi.Windows.Failed(ErrorCode) then
    begin
      failUserAgent('WebView2 answered with HRESULT $' + IntToHex(ErrorCode, 8) + ': ' + Json);
      EXIT;
    end;

  FUaSent:= UserAgentOverride;
  Inc(FTicket);
  doLog('The user-agent override is in place after ' + elapsedMs + ' ms. Answer: ' + Json);
  delay(0, daNextStep);
end;


procedure TPageDriver.failUserAgent(CONST Info: string);
begin
  FUaSending:= FALSE;
  doLog('FAILED: ' + StepUserAgent + ': ' + Info);
  failRun(StepUserAgent, Info);
end;


{ Registers TNavHandler (trap 2). Only possible once the WebView exists: DefaultInterface is NIL before that. }
function TPageDriver.hookNavigation: Boolean;
VAR
  Handler: ICoreWebView2NavigationCompletedEventHandler;
  HR: HResult;
begin
  if FNavHooked then EXIT(TRUE);

  if FBrowser.DefaultInterface = NIL then
    begin
      failStep('the browser has no WebView');
      EXIT(FALSE);
    end;

  Handler:= TNavHandler.Create(Self, 0);   { held in a variable, never passed as a temporary, so its reference count is 1 during the call }
  HR:= FBrowser.DefaultInterface.add_NavigationCompleted(Handler, FNavToken);
  if Winapi.Windows.Failed(HR) then
    begin
      failStep('Cannot register the navigation handler. HRESULT $' + IntToHex(HR, 8));
      EXIT(FALSE);
    end;

  FNavHooked:= TRUE;
  Result:= TRUE;
end;


{ See trap 2 in the header. }
procedure TPageDriver.navigationCompleted(IsSuccess: Boolean; WebErrorStatus: COREWEBVIEW2_WEB_ERROR_STATUS; HttpStatus: Integer);
begin
  if NOT FBusy OR NOT FWaitingNav then EXIT;

  if IsSuccess then
    begin
      FWaitingNav:= FALSE;
      doLog(stepLabel + ': navigation completed, HTTP status ' + IntToStr(HttpStatus));
      finishStep;
    end
  else
    begin
      FLastSeen:= 'navigation failed, HTTP status ' + IntToStr(HttpStatus) + ', WebErrorStatus ' + IntToStr(Ord(WebErrorStatus));
      doLog(stepLabel + ': ' + FLastSeen + ' (not final: a later navigation may still succeed)');
    end;
end;



{-------------------------------------------------------------------------------------------------------------
   THE STEPS
-------------------------------------------------------------------------------------------------------------}

procedure TPageDriver.delay(Ms: Integer; Action: TDelayAction);
begin
  FDelayAction:= Action;
  restartTimer(FDelayTimer, Ms);
end;


procedure TPageDriver.delayFire(Sender: TObject);
VAR Action: TDelayAction;
begin
  FDelayTimer.Enabled:= FALSE;
  Action:= FDelayAction;
  FDelayAction:= daNone;
  if NOT FBusy OR FStarting then EXIT;

  case Action of
    daNextStep: nextStep;
    daPoll    : sendPoll;
    daPauseEnd: finishStep;
    daNone    : ;
  end;
end;


procedure TPageDriver.stepTimeOut(Sender: TObject);
VAR Info: string;
begin
  FStepTimer.Enabled:= FALSE;
  if NOT FBusy then EXIT;

  if FStarting then
    begin
      Info:= 'The WebView2 browser did not start within ' + IntToStr(BrowserStartTimeOut) + ' ms. ' + BrowserHint;
      doLog('FAILED: ' + StepBrowserStart + ': ' + Info);
      failRun(StepBrowserStart, Info);
      EXIT;
    end;

  if FUaSending then
    begin
      failUserAgent('Emulation.setUserAgentOverride got no answer within ' + IntToStr(UserAgentTimeOut) + ' ms');
      EXIT;
    end;

  Info:= 'timed out after ' + IntToStr(current.TimeOut) + ' ms';
  if FLastSeen <> ''
  then Info:= Info + '. Last seen: ' + FLastSeen;
  failStep(Info);
end;


procedure TPageDriver.nextStep;
begin
  Inc(FIndex);
  if FIndex >= FSteps.Count
  then succeedRun
  else startStep;
end;


procedure TPageDriver.startStep;
VAR Step: TDriverStep;
begin
  Step:= current;
  Inc(FTicket);
  FPhase     := 0;
  FWaitingNav:= FALSE;
  FLastSeen  := '';
  FLastCall  := '';
  FCallMethods.Clear;
  FCallParams.Clear;
  FStepStart := GetTickCount64;
  doLog(stepLabel + ': start');

  if Step.Kind = skPause then
    begin
      delay(Step.TimeOut, daPauseEnd);
      EXIT;
    end;

  restartTimer(FStepTimer, Step.TimeOut);
  case Step.Kind of
    skNavigate  : startNavigate;
    skWaitFor,
    skWaitGone,
    skWaitUntil : sendPoll;
    skRunScript : sendScript(Step.Target);
    skReadText  : sendScript(readTextScript(Step.Target));
    skInsertText,
    skFocus     : sendScript(focusScript(Step.Target));
    skClick     : if Step.ClickMode = cmMouse
                  then sendScript(locateScript(Step.Target))
                  else sendScript(scriptClickScript(Step.Target));
    skPressKey  : begin
                    FPhase:= 1;
                    queueKey(Step.Target);
                    sendNextCall;
                  end;
    skAttachFile: startAttach;
    skDevTools  : begin
                    FPhase:= 1;
                    addCall(Step.Target, Step.Text);
                    sendNextCall;
                  end;
  end;
end;


procedure TPageDriver.startNavigate;
VAR HR: HResult;
begin
  if NOT hookNavigation then EXIT;

  FWaitingNav:= TRUE;
  HR:= FBrowser.DefaultInterface.Navigate(PWideChar(current.Target));
  if Winapi.Windows.Failed(HR)
  then failStep('Navigate refused the address. HRESULT $' + IntToHex(HR, 8));
end;


{ One check of a wait step. The next one is sent only after this one answered (answerArrived), so two checks are never in flight at once. }
procedure TPageDriver.sendPoll;
VAR Step: TDriverStep;
begin
  Step:= current;
  case Step.Kind of
    skWaitFor  : sendScript(waitForScript (Step.Target));
    skWaitGone : sendScript(waitGoneScript(Step.Target));
    skWaitUntil: sendScript(waitUntilScript(Step.Target));
  else
    Assert(FALSE, 'sendPoll: not a wait step');
  end;
end;


{ Trap 1: our own handler for each call. The answer arrives in answerArrived. }
procedure TPageDriver.sendScript(CONST JavaScript: string);
VAR
  Handler: ICoreWebView2ExecuteScriptCompletedHandler;
  HR: HResult;
begin
  if FBrowser.DefaultInterface = NIL then
    begin
      failStep('the browser has no WebView');
      EXIT;
    end;

  Handler:= TAnswerHandler.Create(Self, FTicket);   { held in a variable, never passed as a temporary }
  HR:= FBrowser.DefaultInterface.ExecuteScript(PWideChar(JavaScript), Handler);
  if Winapi.Windows.Failed(HR)
  then failStep('ExecuteScript refused the script. HRESULT $' + IntToHex(HR, 8));
end;


procedure TPageDriver.addCall(CONST Method, Params: string);
begin
  FCallMethods.Add(Method);
  FCallParams.Add(Params);
end;


procedure TPageDriver.queueKey(CONST Key: string);
VAR Down, Up: string;
begin
  if NOT keyEvents(Key, Down, Up)
  then raise EArgumentException.Create('TPageDriver: unknown key "' + Key + '"');

  addCall('Input.dispatchKeyEvent', Down);
  addCall('Input.dispatchKeyEvent', Up);
end;


{ Sends the next queued DevTools call. With none left, the step is done. }
procedure TPageDriver.sendNextCall;
VAR
  Handler: ICoreWebView2CallDevToolsProtocolMethodCompletedHandler;
  Method, Params: string;
  HR: HResult;
begin
  if FCallMethods.Count = 0 then
    begin
      finishStep;
      EXIT;
    end;

  Method:= FCallMethods[0];
  Params:= FCallParams[0];
  FCallMethods.Delete(0);
  FCallParams.Delete(0);
  FLastCall:= Method + ' ' + Params;

  if FBrowser.DefaultInterface = NIL then
    begin
      failStep('the browser has no WebView');
      EXIT;
    end;

  Handler:= TAnswerHandler.Create(Self, FTicket);   { held in a variable, never passed as a temporary }
  HR:= FBrowser.DefaultInterface.CallDevToolsProtocolMethod(PWideChar(Method), PWideChar(Params), Handler);
  if Winapi.Windows.Failed(HR)
  then failStep('CallDevToolsProtocolMethod refused ' + FLastCall + '. HRESULT $' + IntToHex(HR, 8));
end;


{ Every script and DevTools answer of the current step lands here. }
procedure TPageDriver.answerArrived(Ticket: Integer; ErrorCode: HResult; CONST Json: string);
VAR
  Step: TDriverStep;
  Ok, Fatal: Boolean;
  Info: string;
  X, Y: Integer;
begin
  if NOT FBusy OR FStarting OR (Ticket <> FTicket) then EXIT;   { a stale answer: trap 3 }

  if FUaSending then
    begin
      userAgentAnswered(ErrorCode, Json);
      EXIT;
    end;

  Step:= current;

  { A failed script or DevTools call. On a failed DevTools call WebView2 puts the error into Json (the comment above userAgentAnswered). }
  if Winapi.Windows.Failed(ErrorCode) then
    begin
      Info:= 'WebView2 answered with HRESULT $' + IntToHex(ErrorCode, 8);
      if FLastCall <> ''
      then Info:= Info + ' to ' + FLastCall;
      if Json <> ''
      then Info:= Info + '. Answer: ' + Copy(Json, 1, 300);
      failStep(Info);
      EXIT;
    end;

  case Step.Kind of
    skRunScript:
      begin
        FResults.AddOrSetValue(Step.Name, TWebPageReader.UnwrapEnvelope(Json));
        finishStep;
      end;

    skWaitFor,
    skWaitGone,
    skWaitUntil:
      begin
        readAnswer(Json, Ok, Fatal, Info, X, Y);
        if Ok
        then finishStep
        else
          if Fatal
          then failStep(Info)
          else
            begin
              FLastSeen:= Info;
              delay(PollInterval, daPoll);
            end;
      end;

    skReadText:
      begin
        readAnswer(Json, Ok, Fatal, Info, X, Y);
        if Ok then
          begin
            FResults.AddOrSetValue(Step.Name, Info);
            finishStep;
          end
        else
          failStep(Info);
      end;

    skInsertText:
      if FPhase = 0
      then insertFocused(Json)
      else insertAnswered(Json);

    skClick:
      if FPhase = 0
      then clickLocated(Json)
      else sendNextCall;

    skPressKey,
    skDevTools:
      sendNextCall;

    skFocus:
      begin
        readAnswer(Json, Ok, Fatal, Info, X, Y);
        if Ok then
          begin
            doLog(stepLabel + ': ' + Info);
            finishStep;
          end
        else
          failStep(Info);
      end;

    skAttachFile:
      begin
        doLog(stepLabel + ': answer ' + Copy(Json, 1, 300));
        if FPhase = 0
        then attachLocated(Json)
        else sendNextCall;   { DOM.setFileInputFiles answered; no call is left, so the step is done }
      end;
  else
    Assert(FALSE, 'answerArrived: this step sends no request');
  end;
end;


{ The element has the focus and the caret is at its end. Now the text goes in, the way InsertMode says. }
procedure TPageDriver.insertFocused(CONST Json: string);
VAR
  Step: TDriverStep;
  Ok, Fatal: Boolean;
  Info: string;
  X, Y, i: Integer;
  Lines: TStringDynArray;
begin
  readAnswer(Json, Ok, Fatal, Info, X, Y);
  if NOT Ok then
    begin
      failStep(Info);
      EXIT;
    end;

  Step:= current;
  doLog(stepLabel + ': ' + Info);
  FPhase:= 1;
  Lines:= SplitString(ReplaceEnters(Step.Text, #10), #10);   { CR LF, LF and CR all end a line }

  case Step.InsertMode of
    imKeyboard:
      begin
        for i:= 0 to High(Lines) do
          begin
            if i > 0
            then queueKey('Enter');
            if Lines[i] <> ''
            then addCall('Input.insertText', '{"text":' + JsString(Lines[i]) + '}');
          end;
        sendNextCall;
      end;
    imExecCommand: sendScript(execCommandScript(Lines));
    imPaste      : sendScript(pasteScript(ReplaceEnters(Step.Text, #10)));
  end;
end;


{ imKeyboard: one DevTools call answered. The other two modes: their one script answered.
  The step does not check the text that arrived: the caller reads it back itself (ReadText). }
procedure TPageDriver.insertAnswered(CONST Json: string);
VAR
  Ok, Fatal: Boolean;
  Info: string;
  X, Y: Integer;
begin
  if current.InsertMode = imKeyboard then
    begin
      sendNextCall;
      EXIT;
    end;

  readAnswer(Json, Ok, Fatal, Info, X, Y);
  if NOT Ok then
    begin
      failStep(Info);
      EXIT;
    end;

  if Info <> ''
  then doLog(stepLabel + ': ' + Info);
  finishStep;
end;


procedure TPageDriver.clickLocated(CONST Json: string);
VAR
  Ok, Fatal: Boolean;
  Info, Point: string;
  X, Y: Integer;
begin
  readAnswer(Json, Ok, Fatal, Info, X, Y);
  if NOT Ok then
    begin
      failStep(Info);
      EXIT;
    end;

  if current.ClickMode = cmScript then
    begin
      finishStep;   { scriptClickScript has clicked already }
      EXIT;
    end;

  doLog(stepLabel + ': ' + Info);
  FPhase:= 1;
  Point:= '"x":' + IntToStr(X) + ',"y":' + IntToStr(Y);
  addCall('Input.dispatchMouseEvent', '{"type":"mouseMoved",'   + Point + '}');
  addCall('Input.dispatchMouseEvent', '{"type":"mousePressed",'  + Point + ',"button":"left","buttons":1,"clickCount":1}');
  addCall('Input.dispatchMouseEvent', '{"type":"mouseReleased",' + Point + ',"button":"left","buttons":0,"clickCount":1}');
  sendNextCall;
end;


{ AttachFile, call 1 of 2 (ATTACH FILE in the header). A missing file fails the step before any call. }
procedure TPageDriver.startAttach;
VAR Step: TDriverStep;
begin
  Step:= current;
  if NOT FileExists(Step.Text) then
    begin
      failStep('the file does not exist: ' + Step.Text);
      EXIT;
    end;

  addCall('Runtime.evaluate', '{"expression":' + JsString(attachLocateScript(Step.Target)) + ',"returnByValue":false}');
  doLog(stepLabel + ': sending Runtime.evaluate ' + FCallParams[0]);
  sendNextCall;
end;


{ AttachFile, call 2 of 2: the one file input is found; its objectId goes to DOM.setFileInputFiles. }
procedure TPageDriver.attachLocated(CONST Json: string);
VAR ObjectId, Reason: string;
begin
  if NOT readEvaluateAnswer(Json, ObjectId, Reason) then
    begin
      failStep(Reason);
      EXIT;
    end;

  FPhase:= 1;
  addCall('DOM.setFileInputFiles', '{"files":[' + JsString(current.Text) + '],"objectId":' + JsString(ObjectId) + '}');
  doLog(stepLabel + ': sending DOM.setFileInputFiles ' + FCallParams[0]);
  sendNextCall;
end;



{-------------------------------------------------------------------------------------------------------------
   OUTCOME
   failRun and succeedRun fire their event as the very last statement, with Busy already FALSE,
   so the handler may Clear, add steps and Run again.
-------------------------------------------------------------------------------------------------------------}

procedure TPageDriver.finishStep;
begin
  FStepTimer.Enabled:= FALSE;
  Inc(FTicket);
  doLog(stepLabel + ': ok, ' + elapsedMs + ' ms');
  delay(0, daNextStep);   { through the timer, so the next step never starts inside a WebView2 handler }
end;


procedure TPageDriver.failStep(CONST Info: string);
VAR StepName: string;
begin
  StepName:= current.Name;
  doLog(stepLabel + ': FAILED after ' + elapsedMs + ' ms: ' + Info);
  failRun(StepName, Info);
end;


procedure TPageDriver.failRun(CONST StepName, Info: string);
begin
  stopAll;
  if Assigned(OnFailed)
  then OnFailed(Self, StepName, Info);
end;


procedure TPageDriver.succeedRun;
begin
  stopAll;
  doLog('All ' + IntToStr(FSteps.Count) + ' steps done');
  if Assigned(OnDone)
  then OnDone(Self);
end;


procedure TPageDriver.stopAll;
begin
  if FStepTimer  <> NIL then FStepTimer .Enabled:= FALSE;
  if FDelayTimer <> NIL then FDelayTimer.Enabled:= FALSE;
  FDelayAction:= daNone;
  FBusy       := FALSE;
  FStarting   := FALSE;
  FWaitingNav := FALSE;
  FUaSending  := FALSE;
  Inc(FTicket);
end;



{-------------------------------------------------------------------------------------------------------------
   SMALL THINGS
-------------------------------------------------------------------------------------------------------------}

function TPageDriver.current: TDriverStep;
begin
  Assert((FIndex >= 0) AND (FIndex < FSteps.Count), 'TPageDriver: no current step!');
  Result:= TDriverStep(FSteps[FIndex]);
end;


function TPageDriver.stepLabel: string;
begin
  Result:= 'step ' + IntToStr(FIndex + 1) + '/' + IntToStr(FSteps.Count) + ' ' + current.Name;
end;


function TPageDriver.elapsedMs: string;
begin
  Result:= IntToStr(GetTickCount64 - FStepStart);
end;


procedure TPageDriver.doLog(CONST Line: string);
begin
  if Assigned(OnLog)
  then OnLog(Self, Line);
end;


end.
