UNIT LightVcl.Common.EnvironmentVar;

{=============================================================================================================
   SYSTEM - Environment Variables

   2026.06.10

   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
==============================================================================================================

  Provides functions to read and write Windows environment variables, both for the current
  user (HKEY_CURRENT_USER) and machine-level (HKEY_LOCAL_MACHINE).

  Key features:
    - GetEnvironmentVars: Read environment variables from registry or process environment block
    - SetEnvironmentVars: Write environment variables to registry and notify system of changes
    - ExpandEnvironmentStrings: Expand variables like %PATH% to their actual values

  Notes:
    - Machine-level operations (User=False) require administrator privileges
    - Changes are broadcast to all top-level windows via WM_SETTINGCHANGE

   In this group:
     * LightVcl.Common.Shell.pas
     * LightVcl.Common.System.pas
     * LightVcl.Common.Window.pas
     * LightVcl.Common.WindowMetrics.pas
     * LightVcl.Common.ExecuteProc.pas
     * LightVcl.Common.ExecuteShell.pas

=============================================================================================================}

INTERFACE
USES
   Winapi.Windows, Winapi.Messages, System.Classes,
   System.SysUtils, System.Win.Registry;


{ Expands environment variable references (e.g., %PATH%) in the input string.
  Raises EOSError if expansion fails. }
function ExpandEnvironmentStrings(CONST Vars: string): string;

{ Writes an environment variable to the registry and updates the current process.
  User=True writes to HKEY_CURRENT_USER (no admin required).
  User=False writes to HKEY_LOCAL_MACHINE (requires admin rights).
  Returns True if the operation succeeded. }
function SetEnvironmentVars(CONST Name, Value: string; User: Boolean = True): Boolean;

{ Reads an environment variable from the registry and expands any embedded variables.
  User=True reads from HKEY_CURRENT_USER.
  User=False reads from HKEY_LOCAL_MACHINE.
  Returns empty string if variable not found or registry key cannot be opened. }
function GetEnvironmentVars(CONST Name: string; User: Boolean = True): string;  overload;

{ Retrieves all environment variables from the current process environment block.
  Each entry is in NAME=VALUE format.
  Returns True if successful, False if GetEnvironmentStrings fails.
  TSL must not be nil. }
function GetEnvironmentVars(TSL: TStrings): Boolean;  overload;



IMPLEMENTATION

USES
   LightCore.Types;


CONST
   REG_MACHINE_LOCATION = 'System\CurrentControlSet\Control\Session Manager\Environment';
   REG_USER_LOCATION    = 'Environment';


{---------------------------------------------------------------------------------------------------------------
   GetEnvironmentVars (TStrings overload)
---------------------------------------------------------------------------------------------------------------}
function GetEnvironmentVars(TSL: TStrings): Boolean;
VAR
  Vars: PChar;
  pS: PChar;
begin
  Assert(TSL <> NIL, 'GetEnvironmentVars: TSL parameter cannot be nil');

  TSL.BeginUpdate;
  TRY
    TSL.Clear;
    Vars:= WinApi.Windows.GetEnvironmentStrings;
    Result:= Vars <> NIL;
    if Result then
      TRY
        pS:= Vars;
        { Environment block: a sequence of null-terminated strings, ending with a double null. Format: VAR1=Value1#0VAR2=Value2#0#0 }
        while pS^ <> #0 do
          begin
            TSL.Add(pS);
            pS:= StrEnd(pS);  { Find end of current string }
            Inc(pS);          { Move past the null terminator to next string }
          end;
      FINALLY
        WinApi.Windows.FreeEnvironmentStrings(Vars);
      END;
  FINALLY
    TSL.EndUpdate;
  END;
end;


{---------------------------------------------------------------------------------------------------------------
   GetEnvironmentVars (single variable)

   Unlike System.SysUtils.GetEnvironmentVariable, which only reads the process environment, this function reads directly from the registry, where persistent environment variables are stored.
   For Win32 apps on Win64 systems, KEY_WOW64_64KEY may be needed for registry access.
   Tested on Windows 7 and later.
---------------------------------------------------------------------------------------------------------------}
function GetEnvironmentVars(CONST Name: string; User: Boolean = True): string;
VAR
  Reg: TRegistry;
  RawValue: string;
  ExpandedLen: Integer;
begin
  Result:= '';
  if Name = '' then EXIT;

  Reg:= TRegistry.Create(KEY_READ);
  TRY
    if User then
      begin
        Reg.RootKey:= HKEY_CURRENT_USER;
        if NOT Reg.OpenKeyReadOnly(REG_USER_LOCATION)
        then EXIT;
      end
    else
      begin
        Reg.RootKey:= HKEY_LOCAL_MACHINE;
        if NOT Reg.OpenKeyReadOnly(REG_MACHINE_LOCATION)
        then EXIT;
      end;

    RawValue:= Reg.ReadString(Name);
    if RawValue = '' then EXIT;

    { Expand environment variable references in the value.
      First call with nil buffer to get required size. }
    ExpandedLen:= WinApi.Windows.ExpandEnvironmentStrings(PChar(RawValue), NIL, 0);
    if ExpandedLen > 0 then
      begin
        SetLength(Result, ExpandedLen - 1);  { -1 because ExpandedLen includes null terminator }
        WinApi.Windows.ExpandEnvironmentStrings(PChar(RawValue), PChar(Result), ExpandedLen);
      end
    else
      Result:= RawValue;  { Fallback to raw value if expansion fails }
  FINALLY
    FreeAndNil(Reg);
  END;
end;


{---------------------------------------------------------------------------------------------------------------
   SetEnvironmentVars

   Writing to the registry makes the change persistent across reboots and available to newly started processes.
   The broadcast message may take some time to be processed by all applications.
   Tested on Windows 7.
---------------------------------------------------------------------------------------------------------------}
function SetEnvironmentVars(CONST Name, Value: string; User: Boolean = TRUE): Boolean;
VAR
  Reg: TRegistry;
  RegLocation: string;
  MsgResult: DWORD_PTR;
begin
  Result:= FALSE;
  if Name = '' then EXIT;

  if User
  then RegLocation:= REG_USER_LOCATION
  else RegLocation:= REG_MACHINE_LOCATION;

  Reg:= TRegistry.Create(KEY_WRITE);
  TRY
    if User
    then Reg.RootKey:= HKEY_CURRENT_USER
    else Reg.RootKey:= HKEY_LOCAL_MACHINE;

    { OpenKey with CanCreate=False - we don't want to create the Environment key }
    if NOT Reg.OpenKey(RegLocation, False)
    then EXIT;

    TRY
      Reg.WriteString(Name, Value);

      { Update current process environment so changes are immediately visible }
      SetEnvironmentVariable(PChar(Name), PChar(Value));

      { Broadcast to all top-level windows to refresh their environment.
        MSDN (Environment Variables): broadcast WM_SETTINGCHANGE with lParam set to the string
        "Environment" - for BOTH user and machine scope. Receivers (Explorer etc.) compare lParam
        against "Environment", so passing the machine registry path here would be ignored.
        SendMessageTimeout + SMTO_ABORTIFHUNG instead of SendMessage: a single hung top-level window
        must not block us forever (MSDN, WM_SETTINGCHANGE). }
      SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, 0, LPARAM(PChar('Environment')), SMTO_ABORTIFHUNG, 5000, @MsgResult);

      Result:= TRUE;
    EXCEPT
      { WriteString raises ERegistryException on access denied or other registry errors.
        Converted to FALSE because the documented contract of this function is a Boolean result. }
      on E: ERegistryException do
        Result:= FALSE;
    END;
  FINALLY
    FreeAndNil(Reg);
  END;
end;


{---------------------------------------------------------------------------------------------------------------
   ExpandEnvironmentStrings

   A wrapper around the Windows API ExpandEnvironmentStrings that handles buffer sizing automatically, for strings of any length.
   Example: ExpandEnvironmentStrings('%TEMP%\myfile.txt') returns 'C:\Users\...\AppData\Local\Temp\myfile.txt'
---------------------------------------------------------------------------------------------------------------}
function ExpandEnvironmentStrings(CONST Vars: string): string;
VAR
  Count: Integer;
  MaxLen: Integer;
begin
  if Vars = '' then EXIT('');

  MaxLen:= Length(Vars) + 32*KB;
  SetLength(Result, MaxLen);

  Count:= WinApi.Windows.ExpandEnvironmentStrings(PChar(Vars), PChar(Result), MaxLen);

  { If buffer was too small, reallocate with exact size needed }
  if Count > MaxLen then
    begin
      MaxLen:= Count + 1;
      SetLength(Result, MaxLen);
      Count:= WinApi.Windows.ExpandEnvironmentStrings(PChar(Vars), PChar(Result), MaxLen);
    end;

  { Count = 0 indicates an error }
  if Count = 0 then
    RaiseLastOSError(GetLastError, 'Error in ExpandEnvironmentStrings WinAPI - %1:s (%0:d)');

  { Count includes null terminator, so subtract 1 for final string length }
  SetLength(Result, Count - 1);
end;


end.
