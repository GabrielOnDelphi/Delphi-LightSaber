UNIT LightCore.SystemPermissions;

{=============================================================================================================
   2026.09.15
   www.GabrielMoraru.com

==============================================================================================================

   Functions to set/test user permissions and privileges.
   Includes: Admin rights detection.

   AppHasAdminRights, IsUserAdmin and CurrentUserHasAdminRights return a value and have a body only for Windows, so each is declared inside a MSWINDOWS conditional - interface and implementation both. Off Windows a FALSE would read as "this process is not elevated", which is a silent wrong answer. A call written in an Android, macOS or iOS build therefore fails to compile, and this unit compiles empty there.

   SetPrivilege, which takes a Windows privilege name, and AppElevationLevel, which returns a Windows token elevation type, are in LightCore.Win.SystemPermissions, a Windows-only unit of the package LightCore.Win.

=============================================================================================================}

INTERFACE

{$IFDEF MSWINDOWS}
USES
   Winapi.Windows;


 function  AppHasAdminRights: Boolean;

 function  IsUserAdmin: Boolean;                  { UNUSED - TO BE DELETED. Same answer as AppHasAdminRights. }
 function  CurrentUserHasAdminRights: Boolean;    { UNUSED - TO BE DELETED. Same answer as AppHasAdminRights. }
{$ENDIF}


IMPLEMENTATION
{$IFDEF MSWINDOWS}




{-----------------------------------------------------------------------------------------------------------------------
   APP ADMIN RIGHTS
-----------------------------------------------------------------------------------------------------------------------}

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
function AppHasAdminRights: Boolean;
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


{=============================================================================================================
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!   UNUSED - TO BE DELETED
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   IsUserAdmin gives the same answer as AppHasAdminRights, because its body only calls AppHasAdminRights.
   Nothing calls it except its own tests in Test.LightCore.SystemPermissions.pas.
   Call AppHasAdminRights instead.
=============================================================================================================}
function IsUserAdmin: Boolean;
begin
  Result:= AppHasAdminRights;
end;


{=============================================================================================================
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!   UNUSED - TO BE DELETED
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   CurrentUserHasAdminRights gives the same answer as AppHasAdminRights, because its body only calls AppHasAdminRights.
   Nothing calls it except its own tests in Test.LightCore.SystemPermissions.pas.
   Call AppHasAdminRights instead.
=============================================================================================================}
function CurrentUserHasAdminRights: Boolean;
begin
  Result:= AppHasAdminRights;
end;
{$ENDIF}

end.

