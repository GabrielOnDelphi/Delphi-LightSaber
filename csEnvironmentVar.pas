UNIT csEnvironmentVar;

{=============================================================================================================
   SYSTEM - Environment Variables
   2023.01
   www.GabrielMoraru.com
   See Copyright file
==============================================================================================================

  System function to access environment variables

   In this group:
     * csShell.pas
     * csSystem.pas
     * csWindow.pas
     * csWindowMetrics.pas
     * csExecuteProc.pas
     * csExecuteShell.pas

=============================================================================================================}

INTERFACE
USES
   Winapi.Windows, Winapi.Messages, System.Classes,
   System.SysUtils, System.Win.Registry, System.UITypes;


 function ExpandEnvironmentStrings(CONST Vars: string): string;

 function SetEnvironmentVars (CONST Name, Value: string; User: Boolean = True): Boolean;
 function GetEnvironmentVars (CONST Name: string; User: Boolean = True): string;  overload;
 function GetEnvironmentVars (TSL: TStrings): Boolean;                            overload;



IMPLEMENTATION

USES
   ccCore;


function GetEnvironmentVars(TSL: TStrings): Boolean;
var
  Vars: PChar;
  pS: PChar;
begin
  TSL.BeginUpdate;
  TRY
    TSL.Clear;
    Vars := WinApi.Windows.GetEnvironmentStrings;
    Result := Vars <> NIL;
    if Result then
      TRY
        pS := Vars;
        while pS^ <> #0 do
          begin
            TSL.Add(pS);
            pS := StrEnd(pS);
            Inc(pS);
          end;
      FINALLY
        WinApi.Windows.FreeEnvironmentStrings(Vars);
      END;
  FINALLY
    TSL.EndUpdate;
  END;
end;



CONST
   REG_MACHINE_LOCATION = 'System\CurrentControlSet\Control\Session Manager\Environment';
   REG_USER_LOCATION    = 'Environment';

{ WARNING! For Win32 apps on Win64 systems you need to use KEY_WOW64_64KEY:
     RegistryEntry.Access := KEY_READ or KEY_WOW64_64KEY
  There is the GetEnvironmentVariable function in System.SysUtils. However, it only works with the Global env variables, not also with the User env variables
  When User= False, the program requires admin rights

  Tested on Win7 }
function GetEnvironmentVars(CONST Name: string; User: Boolean = True): string;
VAR
   Str: array[0..255] of char;
begin
  with TRegistry.Create do
  TRY
    if User then
     begin
      RootKey := HKEY_CURRENT_USER;
      OpenKeyReadOnly(REG_USER_LOCATION);
     end
    else
     begin
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKeyReadOnly(REG_MACHINE_LOCATION);
     end;

    Result := ReadString(Name);
    WinApi.Windows.ExpandEnvironmentStrings(PChar(Result), Str, 255);
    Result := Str;
  FINALLY
    Free;
  END;
end;


{ Works fine on Win7.
  When User= False, the program requires admin rights }
function SetEnvironmentVars(CONST Name, Value: string; User: Boolean = TRUE): Boolean;
begin
  with TRegistry.Create do
  TRY
    if User then
     begin
      RootKey := HKEY_CURRENT_USER;
      Result  := OpenKey(REG_USER_LOCATION, False);
      WriteString(Name, Value);
     end
    else
     begin
      RootKey := HKEY_LOCAL_MACHINE;
      Result  := OpenKey(REG_MACHINE_LOCATION, False);
     end;

   if Result then
    begin
     WriteString(Name, Value);
     SetEnvironmentVariable(PChar(Name), PChar(Value));                                     { Update Current Process Environment Variable }
     SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, LPARAM(PChar(REG_USER_LOCATION)));    { Send Message To All Top Window for Refresh }
    end;
  FINALLY
    Free;
  END;
end;



function ExpandEnvironmentStrings(CONST Vars: string): string;
var
  Count: Integer;
  MaxLen: Integer;
  LastError: Cardinal;
begin
  MaxLen := Length(Vars) + 32*KB;
  SetLength(Result, MaxLen);

  Count := WinApi.Windows.ExpandEnvironmentStrings(PChar(Vars), PChar(Result), MaxLen);
  if Count > MaxLen then
    begin
      MaxLen := Count + 1;
      SetLength(Result, MaxLen);
      Count := WinApi.Windows.ExpandEnvironmentStrings(PChar(Vars), PChar(Result), MaxLen);
    end;

  if Count = 0 then
    begin
     LastError := GetLastError;
     RaiseLastOSError(LastError, 'Error in ExpandEnvironmentStrings WinAPI - %1:s (%0:d)');
    end;
  SetLength(Result, Count - 1);
end;


end.
