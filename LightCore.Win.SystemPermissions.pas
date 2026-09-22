UNIT LightCore.Win.SystemPermissions;

{$IFNDEF MSWINDOWS}
  {$MESSAGE FATAL 'LightCore.Win.SystemPermissions is Windows-only. Its package LightCore.Win builds for Win32 and Win64 only.'}
{$ENDIF}

{=============================================================================================================
   2026.09.15
   www.GabrielMoraru.com

==============================================================================================================

   Functions to set/test user permissions and privileges.
   Includes: elevation type detection, privilege elevation, token management.

   Windows-only unit, package LightCore.Win. SetPrivilege takes a Windows privilege name, such as SeShutdownPrivilege, and AppElevationLevel returns a Windows token elevation type as a number. Neither exists off Windows, so a build for Android, macOS or iOS stops at the top of this unit with a fatal compiler message. The admin-rights routines - AppHasAdminRights, and IsUserAdmin and CurrentUserHasAdminRights, which give the same answer - are in LightCore.SystemPermissions.

=============================================================================================================}

INTERFACE

USES
   Winapi.Windows, System.SysUtils;


 function  AppElevationLevel: Integer;

 function  SetPrivilege(CONST PrivilegeName: string; bEnabled : Boolean): Boolean;


IMPLEMENTATION




{-----------------------------------------------------------------------------------------------------------------------
   APP ADMIN RIGHTS
-----------------------------------------------------------------------------------------------------------------------}

{ Returns the elevation type of the current process token.
  Returns:
    1 = TokenElevationTypeDefault (standard user, UAC disabled, or built-in admin)
    2 = TokenElevationTypeFull (elevated admin)
    3 = TokenElevationTypeLimited (non-elevated, UAC enabled)
   -1 = Error (could not query token)
  Source: experts-exchange.com }
function AppElevationLevel: Integer;
{$IF CompilerVersion >= 28}           { http://delphi.wikia.com/wiki/CompilerVersion_Constant }
CONST
    TokenElevationType = 18;
VAR
    Token: NativeUInt;
    ElevationType: Integer;
    dwSize: Cardinal;
{$ELSE}
CONST
    TokenElevationType = 18;
VAR
    Token: Cardinal;
    ElevationType: Integer;
    dwSize: Cardinal;
{$ENDIF}
begin
  Result:= -1;

  if NOT OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, Token)
  then EXIT;

  TRY
    if GetTokenInformation(Token, TTokenInformationClass(TokenElevationType), @ElevationType, SizeOf(ElevationType), dwSize)
    then Result:= ElevationType;
    // On failure, Result remains -1 (no UI shown - caller should handle errors)
  FINALLY
    CloseHandle(Token);
  END;
end;



{-------------------------------------------------------------------------------------------------------------
   Enable/Disables a specific privilege in Windows.
-------------------------------------------------------------------------------------------------------------}
function SetPrivilege(CONST PrivilegeName: string; bEnabled: Boolean): Boolean;
VAR
   TPPrev, TP: TTokenPrivileges;
   Token: THandle;
   dwRetLen: DWord;
begin
  Result := False;

  if PrivilegeName = ''
  then raise Exception.Create('SetPrivilege: PrivilegeName parameter cannot be empty');

  if NOT OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, Token)
  then EXIT;

  TRY
    TP.PrivilegeCount := 1;
    if LookupPrivilegeValue(NIL, PWideChar(PrivilegeName), TP.Privileges[0].LUID) then
    begin
      if bEnabled
      then TP.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED
      else TP.Privileges[0].Attributes := 0;

      dwRetLen := 0;
      Result := AdjustTokenPrivileges(Token, False, TP, SizeOf(TPPrev), TPPrev, dwRetLen);
    end;
  FINALLY
    CloseHandle(Token);
  END;
end;


end.

