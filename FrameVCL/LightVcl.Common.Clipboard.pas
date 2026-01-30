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
   Winapi.Windows,
   System.Classes, System.SysUtils,
   Vcl.ClipBrd;


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


end.
