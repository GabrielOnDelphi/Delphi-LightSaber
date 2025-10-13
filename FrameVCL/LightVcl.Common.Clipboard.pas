UNIT LightVcl.Common.Clipboard;

{=============================================================================================================
   2025.05
   www.GabrielMoraru.com
=============================================================================================================}

INTERFACE
USES
   Winapi.Windows,
   System.Classes, System.SysUtils,
   Vcl.ClipBrd;


 function  StringToClipboard  (CONST Str: string; CONST MaxRetries: Integer= 20): Boolean;         { Returns true if it succeded in writing the clipboard }
 function  StringFromClipboard(CONST MaxWaitTime: Cardinal= 5000): string;
 function  StringFromClipboardTSL: TStringList;



IMPLEMENTATION



{--------------------------------------------------------------------------------------------------
                             STRING  -  CLIPBOARD

  Documentation for Clipboard.Opne -> Opens the clipboard, preventing other applications from changing its contents until the clipboard is Closed. Call Open before adding a series of items to the clipboard. This prevents other applications from overwriting the clipboard until it is closed. (When adding a single item to the clipboard, there is no need to call Open.)
  https://stackoverflow.com/questions/1859102/how-can-i-fix-cannot-open-clipboard-access-denied-errors
--------------------------------------------------------------------------------------------------}
function StringFromClipboard_old: string;
begin
 if Clipboard.HasFormat(CF_TEXT)
 then Result := Clipboard.AsText
 else Result := '';
end;



function StringFromClipboard(CONST MaxWaitTime: Cardinal= 5000): string;  // in ms
VAR WaitTime: Integer;
    Success: boolean;
    StartTime, TimeSpent: Cardinal;
begin
 if Clipboard.HasFormat(CF_TEXT)
 then Result:= ''
 else EXIT('');         { The clipboard contains no text }

 Success:= FALSE;
 WaitTime:= 30;
 StartTime:= GetTickCount;
 REPEAT
   TRY
     Result:= Clipboard.AsText;
     Success:= TRUE;      { This never get's a chance to be set to TRUE as long as it fails }
   except
     //todo 1: trap only specific exceptions
     Sleep(WaitTime);    { Wait a bit before trying again }
     Inc(WaitTime, WaitTime);
   END;
   TimeSpent:= GetTickCount - StartTime;
 UNTIL Success OR (TimeSpent > MaxWaitTime);
end;



function StringFromClipboardTSL: TStringList;
begin
 if Clipboard.HasFormat(CF_TEXT)
 then
  begin
   Result := TStringList.Create;
   Result.Text:= Clipboard.AsText;
  end
 else Result:= NIL;
end;



{ Returns true if it succeded in reading the clipboard }
function StringToClipboard(CONST Str: string; CONST MaxRetries: Integer= 20): Boolean;
VAR RetryCount: Integer;
begin
 Result:= FALSE;
 RetryCount:= 0;
 REPEAT
   Inc(RetryCount);
   TRY
     Clipboard.AsText:= Str;
     Result:= TRUE;                                                                                { This never get's a chance to be set to TRUE as long as it fails }
   except
     //todo 1: trap only specific exceptions
     Sleep(30);    { Wait a bit before trying again }
   END;
 UNTIL Result OR (RetryCount= MaxRetries);
end;









end.
