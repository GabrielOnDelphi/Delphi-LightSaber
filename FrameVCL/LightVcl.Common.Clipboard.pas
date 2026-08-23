UNIT LightVcl.Common.Clipboard;

{=============================================================================================================
   2026.01.29
   www.GabrielMoraru.com

   Robust clipboard string operations with retry logic.

   Windows clipboard can fail with "Cannot open clipboard" when another application has it locked.
   These functions implement retry loops with exponential backoff to handle transient lock failures.

   See: https://stackoverflow.com/questions/1859102/how-can-i-fix-cannot-open-clipboard-access-denied-errors
=============================================================================================================}

INTERFACE
USES
   Winapi.Windows, Winapi.Messages,
   System.Classes, System.SysUtils,
   Vcl.ClipBrd;

CONST
  ClipMonTimerID = 1;                        { SetTimer id. Unique within OUR OWN window, so it cannot collide with a caller timer }

TYPE
  TClipMonErrorEvent = procedure(Sender: TObject; CONST Msg: string) of object;

  { Watches the Windows clipboard and raises OnChange whenever its content changes.

    Owns a private window (AllocateHWnd) and never touches the caller window handle. That is
    deliberate: AddClipboardFormatListener binds to an HWND, and the VCL destroys and recreates a
    form handle on its own (a Vcl.Styles switch, a BorderStyle change - both call RecreateWnd),
    which silently kills a form-bound listener. An owned handle is immune, and it is the only
    option for a caller that has no form at all, such as a design-time IDE expert.
    AllocateHWnd creates a hidden top-level WS_POPUP + WS_EX_TOOLWINDOW window, NOT a message-only
    window (see AllocateHWnd in System.Classes.pas). TTimer and TClipboard both do the same thing.

    Debounce: one copy can put several formats on the clipboard (Thunderbird writes plain text AND
    HTML), each firing its own WM_CLIPBOARDUPDATE. DebounceMs collapses that burst into one event.
    Set it to 0 when latency matters more than tidiness - a security guard that wipes leaked data
    off the clipboard must act immediately, not 100 ms later.

    Lifetime: free this object from FormPreRelease, or from the destructor of whatever owns it.
    NEVER from a finalization section and never late in VCL teardown: DeallocateHWnd calls
    DestroyWindow, which dispatches messages, and that crashes once the VCL is half gone.

    This unit must stay free of AppData: it is linked into a design-time package that runs inside
    the IDE, where the AppData globals do not exist. Errors go out through OnError instead. }
  TClipboardMonitor = class(TObject)
  private
    FHandle     : HWND;                      { our own hidden window. Never a caller form handle }
    FRegistered : Boolean;                   { AddClipboardFormatListener succeeded - gates the matching Remove }
    FInNotify   : Boolean;                   { re-entrancy guard. Only covers the SYNCHRONOUS case - see DoChange }
    FTimerActive: Boolean;
    FDebounceMs : Cardinal;
    FActive     : Boolean;
    FOnChange   : TNotifyEvent;
    FOnError    : TClipMonErrorEvent;
    procedure WndProc(VAR Msg: TMessage);
    procedure SetActive(CONST Value: Boolean);
    procedure DoError (CONST Msg: string);
    procedure DoChange;
  public
    constructor Create(aDebounceMs: Cardinal= 100);    { 100 ms is the value proven in ClipboardHelper. 0 = raise every notification }
    destructor  Destroy; override;
    property Active    : Boolean  read FActive     write SetActive;      { Registers / unregisters the listener. TRUE after Create }
    property DebounceMs: Cardinal read FDebounceMs write FDebounceMs;
    property Registered: Boolean  read FRegistered;                      { FALSE means the OS refused us - OnChange will never fire }
    property OnChange  : TNotifyEvent       read FOnChange write FOnChange;   { Fires on the main thread. Read the clipboard yourself, with a SHORT timeout }
    property OnError   : TClipMonErrorEvent read FOnError  write FOnError;
  end;


 function  StringToClipboard    (CONST Str: string; CONST MaxRetries: Integer= 20): Boolean;       { Returns True if it succeeded in writing to the clipboard }
 function  StringFromClipboard  (CONST MaxWaitTime: Cardinal= 5000): string;                       { Returns clipboard text or empty string if unavailable/timeout }
 function  StringFromClipboardTSL(CONST MaxWaitTime: Cardinal= 5000): TStringList;                 { Returns NIL if clipboard has no text; caller must free result }



IMPLEMENTATION

CONST
  INITIAL_RETRY_DELAY_MS = 30;  { Initial delay between retries in milliseconds }


{-------------------------------------------------------------------------------------------------------------
   StringFromClipboard

   Reads text from the Windows clipboard with retry logic.
   Uses exponential backoff: waits 30ms, then 60ms, 120ms, etc. between retries.

   Parameters:
     MaxWaitTime - Maximum total time to wait for clipboard access (milliseconds).
                   Default is 5000ms (5 seconds).

   Returns:
     The clipboard text, or empty string if:
       - Clipboard contains no text (CF_TEXT/CF_UNICODETEXT not available)
       - Timeout exceeded while waiting for clipboard access

   Note: GetTickCount arithmetic handles 49.7-day wraparound correctly due to unsigned subtraction.
-------------------------------------------------------------------------------------------------------------}
function StringFromClipboard(CONST MaxWaitTime: Cardinal= 5000): string;
VAR
    WaitTime: Integer;
    Success: Boolean;
    StartTime, TimeSpent: Cardinal;
begin
 { Check if clipboard contains text before attempting to read.
   Check both ANSI (CF_TEXT) and Unicode (CF_UNICODETEXT) formats. }
 if NOT (Clipboard.HasFormat(CF_TEXT) OR Clipboard.HasFormat(CF_UNICODETEXT))
 then EXIT('');

 Success:= FALSE;
 WaitTime:= INITIAL_RETRY_DELAY_MS;
 StartTime:= GetTickCount;

 REPEAT
   TRY
     Result:= Clipboard.AsText;
     Success:= TRUE;
   except
     on E: EClipboardException do
       begin
         Sleep(WaitTime);
         WaitTime:= WaitTime * 2;  { Exponential backoff }
       end;
   END;
   TimeSpent:= GetTickCount - StartTime;
 UNTIL Success OR (TimeSpent > MaxWaitTime);

 { If we timed out without success, return empty string }
 if NOT Success
 then Result:= '';
end;


{-------------------------------------------------------------------------------------------------------------
   StringFromClipboardTSL

   Reads text from clipboard and returns it as a TStringList (one line per item).
   Includes retry logic for clipboard access failures.

   Parameters:
     MaxWaitTime - Maximum total time to wait for clipboard access (milliseconds).

   Returns:
     TStringList containing clipboard text split by lines, or NIL if clipboard has no text.
     CALLER IS RESPONSIBLE FOR FREEING THE RETURNED OBJECT.
-------------------------------------------------------------------------------------------------------------}
function StringFromClipboardTSL(CONST MaxWaitTime: Cardinal= 5000): TStringList;
VAR ClipText: string;
begin
 ClipText:= StringFromClipboard(MaxWaitTime);

 if ClipText = ''
 then EXIT(NIL);

 Result:= TStringList.Create;
 Result.Text:= ClipText;
end;


{-------------------------------------------------------------------------------------------------------------
   StringToClipboard

   Writes a string to the Windows clipboard with retry logic.
   Retries with fixed delay (30ms) between attempts.

   Parameters:
     Str        - The string to copy to clipboard
     MaxRetries - Maximum number of attempts before giving up. Default is 20.

   Returns:
     True if clipboard write succeeded, False if all retries failed.
-------------------------------------------------------------------------------------------------------------}
function StringToClipboard(CONST Str: string; CONST MaxRetries: Integer= 20): Boolean;
VAR RetryCount: Integer;
begin
 Result:= FALSE;
 RetryCount:= 0;

 REPEAT
   Inc(RetryCount);
   TRY
     Clipboard.AsText:= Str;
     Result:= TRUE;
   except
     on E: EClipboardException do
       Sleep(INITIAL_RETRY_DELAY_MS);
   END;
 UNTIL Result OR (RetryCount >= MaxRetries);
end;


{-------------------------------------------------------------------------------------------------------------
   TClipboardMonitor
-------------------------------------------------------------------------------------------------------------}

constructor TClipboardMonitor.Create(aDebounceMs: Cardinal= 100);
begin
 inherited Create;
 FDebounceMs:= aDebounceMs;

 { AllocateHWnd must run on the thread that will pump the messages - the main thread }
 FHandle:= AllocateHWnd(WndProc);
 Assert(FHandle <> 0, 'TClipboardMonitor.Create: AllocateHWnd failed');

 SetActive(TRUE);
end;


{ DeallocateHWnd calls DestroyWindow, which dispatches messages. Late in VCL teardown that crashes,
  so the owner must free this object while the VCL is still alive (FormPreRelease), never from a
  finalization section. }
destructor TClipboardMonitor.Destroy;
begin
 SetActive(FALSE);                                       { unregisters the listener and kills the timer }

 if FHandle <> 0 then
  begin
   DeallocateHWnd(FHandle);
   FHandle:= 0;
  end;

 inherited Destroy;
end;


{ FRegistered gates the matching Remove. Microsoft documents neither what AddClipboardFormatListener
  returns for a window that is already registered, nor what RemoveClipboardFormatListener returns for
  one that never was - so we simply never get into either situation. }
procedure TClipboardMonitor.SetActive(CONST Value: Boolean);
begin
 if Value = FActive then EXIT;
 FActive:= Value;

 if FActive
 then
   begin
    FRegistered:= AddClipboardFormatListener(FHandle);
    if NOT FRegistered
    then DoError('AddClipboardFormatListener failed. GetLastError= '+ IntToStr(GetLastError));
   end
 else
   begin
    if FTimerActive then
     begin
      KillTimer(FHandle, ClipMonTimerID);
      FTimerActive:= FALSE;
     end;

    if FRegistered then
     begin
      RemoveClipboardFormatListener(FHandle);
      FRegistered:= FALSE;
     end;
   end;
end;


procedure TClipboardMonitor.DoError(CONST Msg: string);
begin
 if Assigned(FOnError)
 then FOnError(Self, Msg);
end;


{ FInNotify blocks only a SYNCHRONOUS re-entry - a handler that writes to or clears the clipboard
  while DebounceMs is 0. With a debounce the re-fired notification arrives later, after this flag is
  already back to FALSE, so such a handler gets one extra OnChange per write. That is harmless (the
  second pass finds nothing to do) but a handler that writes to the clipboard must be idempotent.

  The blanket except is deliberate and is the one place this unit does not re-raise: an exception
  escaping a window procedure surfaces as a dialog on EVERY clipboard change - and inside the Delphi
  IDE, on every clipboard change in the IDE. It is not swallowed silently; it goes out via OnError. }
procedure TClipboardMonitor.DoChange;
begin
 if FInNotify then EXIT;
 if NOT Assigned(FOnChange) then EXIT;

 FInNotify:= TRUE;
 TRY
   TRY
     FOnChange(Self);
   EXCEPT
     ON E: Exception DO
       DoError(E.ClassName+ ': '+ E.Message);
   END;
 FINALLY
   FInNotify:= FALSE;
 END;
end;


procedure TClipboardMonitor.WndProc(VAR Msg: TMessage);
begin
 CASE Msg.Msg OF

   WM_CLIPBOARDUPDATE:
     begin
      if FDebounceMs = 0
      then DoChange
      else
        begin
         { Restart the timer on every notification. One copy can put several formats on the
           clipboard - Thunderbird writes plain text AND HTML - and each fires its own message. }
         if FTimerActive then KillTimer(FHandle, ClipMonTimerID);
         FTimerActive:= SetTimer(FHandle, ClipMonTimerID, FDebounceMs, NIL) <> 0;
         if NOT FTimerActive then DoChange;              { no timer available: fire now rather than lose the event }
        end;
      Msg.Result:= 0;
     end;

   WM_TIMER:
     if Msg.WParam = ClipMonTimerID
     then
       begin
        KillTimer(FHandle, ClipMonTimerID);
        FTimerActive:= FALSE;
        DoChange;
        Msg.Result:= 0;
       end
     else
       Msg.Result:= DefWindowProc(FHandle, Msg.Msg, Msg.WParam, Msg.LParam);

 ELSE
   { Our window is a normal hidden top-level window, so Windows also sends it broadcasts
     (WM_QUERYENDSESSION, WM_SETTINGCHANGE, WM_DEVICECHANGE...). Anything we do not handle MUST go to
     DefWindowProc or the process looks unresponsive to the system. }
   Msg.Result:= DefWindowProc(FHandle, Msg.Msg, Msg.WParam, Msg.LParam);
 END;
end;


end.