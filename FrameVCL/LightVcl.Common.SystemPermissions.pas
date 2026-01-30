UNIT LightVcl.Common.SystemPermissions;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com

==============================================================================================================

   Functions to set/test user permissions and privileges.
   Includes: Admin rights detection, privilege elevation, token management.

   Note: Some legacy functions are preserved for backward compatibility with older Windows versions.

=============================================================================================================}

INTERFACE

USES
   Winapi.Windows, System.SysUtils;


 function  AppElevationLevel: Integer;
 function  AppHasAdminRights: Boolean;

 function  SetPrivilege(CONST PrivilegeName: string; bEnabled : Boolean): Boolean;

 function  IsUserAdmin: Boolean;
 function  CurrentUserHasAdminRights: Boolean;

 function  OsHasNTSecurity: Boolean;


IMPLEMENTATION

USES
   System.Win.Registry, LightVcl.Common.WinVersion;




{-----------------------------------------------------------------------------------------------------------------------
   APP ADMIN RIGHTS
-----------------------------------------------------------------------------------------------------------------------}

function appHasAdminRightsXP: Boolean;  { Source: http://www.delphi3000.com/articles/article_4062.asp?SK= }
CONST
    SECURITY_BUILTIN_DOMAIN_RID   : CARDINAL = $00000020;
    DOMAIN_ALIAS_RID_ADMINS       : CARDINAL = $00000220;
    SECURITY_NT_AUTHORITY         : _SID_IDENTIFIER_AUTHORITY = (Value: (0, 0, 0, 0, 0, 5));
VAR
    ntAuth: SID_IDENTIFIER_AUTHORITY;
    psidAdmin: Pointer;
    IsAdmin: Boolean;
    TokenHandle: THandle;
    cb: DWORD;
    ptg: ^TOKEN_GROUPS;
    i: Integer;
    grp: PSIDAndAttributes;
begin
 if NOT IsNTKernel
 then Result:= TRUE
 else
   begin
    IsAdmin := FALSE;
    ntauth := SECURITY_NT_AUTHORITY;
    psidAdmin := nil;
    ptg := nil;
    TokenHandle := 0;

    if NOT AllocateAndInitializeSid(ntauth, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, psidAdmin)
    then EXIT(FALSE);

    TRY
      if NOT OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, TokenHandle)
      then EXIT(FALSE);

      TRY
        GetTokenInformation(TokenHandle, TokenGroups, nil, 0, cb);
        if cb = 0
        then EXIT(FALSE);

        GetMem(ptg, cb);
        if NOT GetTokenInformation(TokenHandle, TokenGroups, ptg, cb, cb)
        then EXIT(FALSE);

        grp := @(ptg.Groups[0]);
        for i := 0 to ptg.GroupCount - 1 do
         begin
          if EqualSid(psidAdmin, grp.Sid) then
           begin
            IsAdmin := TRUE;
            Break;
           end;
          Inc(grp);
         end;
      FINALLY
        if ptg <> nil then FreeMem(ptg);
        CloseHandle(TokenHandle);
      END;
    FINALLY
      FreeSid(psidAdmin);
    END;

    Result := IsAdmin;
   end;
end;



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


{ Works fine on Win7 }                                 { Also see: www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_23083726.html+delphi+vista+test+if+the+application+has+admin+rights&cd=6&hl=de&ct=clnk&gl=de&client=firefox-a }
function appHasAdminRightsVista: Boolean;
CONST
   TokenElevationTypeDefault = 1;                      { 'elevation type default') }
   TokenElevationTypeFull    = 2;                      { 'elevation type full');   }
   TokenElevationTypeLimited = 3;                      { 'elevation type limited'); }
begin
 Result:= AppElevationLevel = TokenElevationTypeFull;
end;


function AppHasAdminRights_OLD: Boolean;               { The test for this function is here: \MyProjects\Projects SYSTEM\VISTA-Win7\Vista test\ }
begin
 Result:= FALSE;
 case TOSVersion.Major of
  4: Result:= TRUE;
  5: Result:= AppHasAdminRightsXP;                     { Win 2000, 2003, XP }
  6, 7, 8: Result:= AppHasAdminRightsVista;            { Win Vista, 7, 8, Serv2008, Serv2012 }
 end;
end;


{ Returns TRUE if the current application is running with administrator privileges.
  Uses the modern IsUserAdmin approach which checks token membership in the Administrators group.
  This is the recommended method for Vista and later (all modern Windows versions). }
function AppHasAdminRights: Boolean;
begin
  // Use CurrentUserHasAdminRights which properly handles all Windows versions
  Result:= CurrentUserHasAdminRights;
end;






{ Returns TRUE if the OS has NT-based security (Windows NT, 2000, XP, Vista, 7, 8, 10, 11).
  All modern Windows versions (2000+) are NT-based, so this returns TRUE for any supported OS.
  Kept for backward compatibility with legacy code.
  Source: dummzeuch }
function OsHasNTSecurity: Boolean;
begin
  // All modern Windows versions (Windows 2000 and later) are NT-based.
  // TOSVersion.Platform returns pfWindows for all NT-based systems.
  // Windows 9x/ME (non-NT) are no longer supported by Delphi.
  Result:= (TOSVersion.Platform = pfWindows);
end;


TYPE
  TCheckTokenMembership = function(TokenHandle: THandle; SidToCheck: PSID; var IsMember: BOOL): BOOL; stdcall;
VAR
  CheckTokenMembership: TCheckTokenMembership = NIL;
CONST
  SECURITY_NT_AUTHORITY      : SID_IDENTIFIER_AUTHORITY = (Value: (0, 0, 0, 0, 0, 5)); // ntifs
  SECURITY_BUILTIN_DOMAIN_RID: DWORD = $00000020;
  DOMAIN_ALIAS_RID_ADMINS    : DWORD = $00000220;


{ Returns TRUE if the current process is running with administrator privileges.
  In Vista and later with UAC, returns FALSE if not elevated, TRUE if elevated.
  Uses CheckTokenMembership API (recommended over deprecated Shell32.IsUserAnAdmin).
  Source: dummzeuch, based on MSDN CheckTokenMembership documentation }
function IsUserAdmin: Boolean;
VAR
  b: BOOL;
  AdministratorsGroup: PSID;
  Hdl: HMODULE;
begin
  Result:= FALSE;

  if NOT AllocateAndInitializeSid(
    SECURITY_NT_AUTHORITY, 2,         // 2 sub-authorities
    SECURITY_BUILTIN_DOMAIN_RID,      // sub-authority 0
    DOMAIN_ALIAS_RID_ADMINS,          // sub-authority 1
    0, 0, 0, 0, 0, 0,                 // sub-authorities 2-7 not used
    AdministratorsGroup)
  then EXIT;

  TRY
    // Lazy-load CheckTokenMembership from advapi32.dll
    if @CheckTokenMembership = NIL then
    begin
      Hdl:= LoadLibrary(advapi32);
      if Hdl = 0
      then EXIT;

      @CheckTokenMembership:= GetProcAddress(Hdl, 'CheckTokenMembership');
      if @CheckTokenMembership = NIL then
      begin
        FreeLibrary(Hdl);
        EXIT;
      end;
      // Note: Library handle intentionally not freed - function pointer stored in global var for reuse
    end;

    if CheckTokenMembership(0, AdministratorsGroup, b)
    then Result:= b;
  FINALLY
    FreeSid(AdministratorsGroup);
  END;
end;


{ Returns TRUE if the current user has administrator rights.
  For NT-based systems (all modern Windows), checks token membership in Administrators group.
  For legacy non-NT systems (Windows 9x/ME - no longer supported), always returns TRUE. }
function CurrentUserHasAdminRights: Boolean;
begin
  if OsHasNTSecurity
  then Result:= IsUserAdmin
  else Result:= TRUE;  // Non-NT systems have no security model
end;

end.

