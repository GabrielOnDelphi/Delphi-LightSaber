UNIT cmPermissions;

{=============================================================================================================
   2023.01
   See Copyright.txt
==============================================================================================================

   Functions to set/test user permissions and priviledges

=============================================================================================================}

INTERFACE

USES
   Winapi.Windows, System.UITypes, System.SysUtils, Vcl.Dialogs;


 function  AppElevationLevel: Integer;
 function  AppHasAdminRights: Boolean;

 function  SetPrivilege(CONST PrivilegeName: string; bEnabled : Boolean): Boolean;

 function  IsUserAdmin: Boolean;
 function  CurrentUserHasAdminRights: Boolean;

 function  OsHasNTSecurity: Boolean;


IMPLEMENTATION

USES
   System.Win.Registry, cbWinVersion;




{-----------------------------------------------------------------------------------------------------------------------
   APP ADMIN RIGHTS
-----------------------------------------------------------------------------------------------------------------------}

function appHasAdminRightsXP: Boolean;  { Source: http://www.delphi3000.com/articles/article_4062.asp?SK= }
CONST
    SECURITY_BUILTIN_DOMAIN_RID   : CARDINAL = $00000020;
    DOMAIN_ALIAS_RID_ADMINS       : CARDINAL = $00000220;
    SECURITY_NT_AUTHORITY         : _SID_IDENTIFIER_AUTHORITY = (Value: (0, 0, 0, 0, 0, 5));
VAR ntauth: SID_IDENTIFIER_AUTHORITY;
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
    AllocateAndInitializeSid(ntauth, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, psidAdmin);

    TokenHandle := 0;
    OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, TokenHandle);
    GetTokenInformation(TokenHandle, TokenGroups, nil, 0, cb);
    GetMem(ptg, cb);
    GetTokenInformation(TokenHandle, TokenGroups, ptg, cb, cb);

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
    FreeMem(ptg);
    CloseHandle(TokenHandle);
    FreeSid(psidAdmin);
    Result := IsAdmin;
   end;
end;



function AppElevationLevel: Integer;  { From: www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_23083726.html+delphi+vista+test+if+the+application+has+admin+rights&cd=6&hl=de&ct=clnk&gl=de&client=firefox-a }
{$IF CompilerVersion >= 28}           { http://delphi.wikia.com/wiki/CompilerVersion_Constant }
CONST
    TokenElevationType = 18;
VAR Token: nativeuint;
    ElevationType: Integer;
    dwSize: Cardinal;
{$ELSE}
CONST
    TokenElevationType = 18;
VAR token: Cardinal;
    ElevationType: Integer;
    dwSize: Cardinal;
{$ENDIF}
begin
 if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, token)
 then
   TRY
     if GetTokenInformation(token, TTokenInformationClass(TokenElevationType), @ElevationType, SizeOf(ElevationType), dwSize)
     then Result:= ElevationType
     else
      begin
       Result:= -1;
       MessageDlg(SysErrorMessage(GetLastError), mtInformation, [mbOk], 0);
      end;
   FINALLY
     CloseHandle(token);
   END
 else Result:= -1;  { Mesaj(SysErrorMessage(GetLastError)) }
end;



{-------------------------------------------------------------------------------------------------------------
   Enable/Disables a specific privilage in Widnows.
-------------------------------------------------------------------------------------------------------------}
function SetPrivilege(CONST PrivilegeName: string; bEnabled : Boolean): Boolean;
VAR
   TPPrev, TP : TTokenPrivileges;
   Token      : THandle;
   dwRetLen   : DWord;
begin
  Result := False;

  OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, Token);
  TP.PrivilegeCount := 1;
  if( LookupPrivilegeValue(NIL, PwideChar(PrivilegeName), TP.Privileges[0].LUID)) then
  begin
    if (bEnabled)
    then TP.Privileges[0].Attributes  := SE_PRIVILEGE_ENABLED
    else TP.Privileges[0].Attributes  := 0;

    dwRetLen := 0;
    Result := AdjustTokenPrivileges(Token,False, TP, SizeOf(TPPrev), TPPrev, dwRetLen);
  end;
  CloseHandle(Token);
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


{ Source https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Customizing_the_Windows_Application_Manifest_File
  NOT TESTED
  Unable to write to registry -> The application does NOT have Administrator level privileges.
  Write to registry permitted -> The application has Administrator level privileges.
 }
function AppHasAdminRights: boolean;
var reg: TRegistry;
begin
  reg := TRegistry.Create(KEY_READ);
  reg.RootKey := HKEY_LOCAL_MACHINE;
  reg.Access  := KEY_WRITE;
  result:= reg.OpenKey('Software\MyCompanyName\MyApplication\',True);
  reg.Free;  { Free will call automatically CloseKey }
end;






//----------------------------------
function OsHasNTSecurity: Boolean;  //source: dummzeuch
VAR
  vi: TOSVersionInfo;
begin
  FillChar(vi, SizeOf(vi), 0);
  vi.dwOSVersionInfoSize := SizeOf(vi);
  GetVersionEx(vi);
  Result := (vi.dwPlatformId = VER_PLATFORM_WIN32_NT);
end;


TYPE
  TCheckTokenMembership = function(TokenHandle: THandle; SidToCheck: PSID; var IsMember: BOOL): BOOL; stdcall;
VAR
  CheckTokenMembership: TCheckTokenMembership = NIL;
CONST
  SECURITY_NT_AUTHORITY      : SID_IDENTIFIER_AUTHORITY = (Value: (0, 0, 0, 0, 0, 5)); // ntifs
  SECURITY_BUILTIN_DOMAIN_RID: DWORD = $00000020;
  DOMAIN_ALIAS_RID_ADMINS    : DWORD = $00000220;


function IsUserAdmin: Boolean;  //source: dummzeuch
var
  b: BOOL;
  AdministratorsGroup: PSID;
  Hdl: HMODULE;
begin
  {
    This function returns true if you are currently running with admin privileges.
    In Vista and later, if you are non-elevated, this function will return false (you are not running with administrative privileges).
    If you *are* running elevated, then IsUserAdmin will return true, as you are running with admin privileges.

    Windows provides this similar function in Shell32.IsUserAnAdmin.
    But the function is deprecated, and this code is lifted from the docs for CheckTokenMembership:
      http://msdn.microsoft.com/en-us/library/aa376389.aspx

    Routine Description: This routine returns TRUE if the callers process is a member of the Administrators local group. Caller is NOT expected to be impersonating anyone and is expected to be able to open its own process and process token.
      Arguments: None.
      Return Value:
        TRUE - Caller has Administrators local group.
        FALSE - Caller does not have Administrators local group.
  }
  { idea from:
    http://stackoverflow.com/a/8290384/49925
    but heavily modified }
  Result := False;
  if not AllocateAndInitializeSid(
    SECURITY_NT_AUTHORITY, 2, //2 sub-authorities
    SECURITY_BUILTIN_DOMAIN_RID, //sub-authority 0
    DOMAIN_ALIAS_RID_ADMINS, //sub-authority 1
    0, 0, 0, 0, 0, 0, //sub-authorities 2-7 not passed
    AdministratorsGroup)
  then EXIT;
  try
    if @CheckTokenMembership = nil then
    begin
      Hdl := LoadLibrary(advapi32);
      if Hdl = 0
      then EXIT;
      @CheckTokenMembership := GetProcAddress(Hdl, 'CheckTokenMembership');
      if @CheckTokenMembership = nil then
      begin
        FreeLibrary(Hdl);
        EXIT;
      end;
    end;
    if CheckTokenMembership(0, AdministratorsGroup, b)
    then  Result := b;
  finally
    FreeSid(AdministratorsGroup);
  end;
end;


function CurrentUserHasAdminRights: Boolean;
begin
  if OsHasNTSecurity
  then Result := IsUserAdmin // CurrentUserIsInAdminGroup
  else Result := True;
end;

end.

