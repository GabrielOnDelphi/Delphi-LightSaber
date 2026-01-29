UNIT LightVcl.Common.SystemTime;

{=============================================================================================================
   SYSTEM TIME
   2025.01
   www.GabrielMoraru.com
=============================================================================================================}

INTERFACE
USES
   Winapi.Windows,
   System.SysUtils, System.UITypes,
   Vcl.Forms, Vcl.Dialogs;


 procedure DelayEx(CONST ms : cardinal);

 function  GetSysFileTime: TDateTime;                                                              { gets current date from system file - prevents cracking}
 function  SystemTimeIsInvalid: Boolean;                                                           { returns true if the system time is bigger than current clock time }

 procedure CurrentSysTimeStore(CONST SecretKey: string);                                           { Stores current system clock time to a hidden registry key }
 function  CurrentSysTimeValid(CONST SecretKey: string): Boolean;                                  { Read the last saved system time and compares it with current clock. If current value is smaller than the stored value it means that the clock time was set back }

 function  WindowsUpTime: TDateTime;
 function  UserIdleTime: Cardinal;
 { Also see Delphi function: FileAge }


IMPLEMENTATION

USES
   LightVcl.Common.IO, LightVcl.Common.WinVersion, LightVcl.Common.Registry;



{--------------------------------------------------------------------------------------------------
   SYSTEM  TIME
--------------------------------------------------------------------------------------------------}
{ Also see this: http://docwiki.embarcadero.com/Libraries/XE4/en/System.Diagnostics.TStopwatch
  Time since boot
  Use it as: ShowTimeNice(WindowsUpTime)  }
function WindowsUpTime: TDateTime;
begin
 Result:= GetTickCount / SecsPerDay / MSecsPerSec;   // GetTickCount accuracy is 15ms+  https://blogs.msdn.microsoft.com/oldnewthing/20050902-00/?p=34333
end;


{ Available only on Win2000 (and up) machines.
  Details: To track a user's idle time you could hook keyboard and mouse activity.
  Note, however, that installing a system-wide message hook is a very invasive thing
  to do and should be avoided if possible, since it will require your hook DLL to be
  loaded into all processes. }
function UserIdleTime: Cardinal;
VAR
   liInfo: TLastInputInfo;
begin
   liInfo.cbSize := SizeOf(TLastInputInfo);
   if GetLastInputInfo(liInfo)
   then Result := (GetTickCount - liInfo.dwTime) DIV 1000
   else Result := 0;
end;



{ Gets current date from system file - prevents user from reversing time }
function GetSysFileTime: TDateTime;
VAR
  strWinDir, strF: string;
begin
 Result:= 0;
 strWinDir:= GetWinDir;
 Assert(DirectoryExistMsg(strWinDir));

 if LightVcl.Common.WinVersion.IsNTKernel
 then
    begin
     strF:= strWinDir+ 'system32\config\software';                                                 { fisierul asta exista in Windowsul meu (XP) si arata data can a fost oprit Windows-ul ultima data. NU EXISTA IN WIN7!!! }
     if NOT FileExists(strF) then strF:= strWinDir+ 'config\software';
     if NOT FileExists(strF) then strF:= 'c:\pagefile.sys';
     if NOT FileExists(strF) then strF:= 'd:\pagefile.sys';
     if NOT FileExists(strF) then strF:= 'e:\pagefile.sys';
     if NOT FileExists(strF) then strF:= 'f:\pagefile.sys';
    end
 else
    begin
     strF:= strWinDir+ 'system.dat';                                                               { Platform is 'Windows 95' }
     if NOT FileExists(strF)
     then strF:= 'c:\win386.swp';
     if NOT FileExists(strF) then
       begin
        strF:= strWinDir+ 'SYSTEM.DA0';
        if NOT FileExists(strF)
        then strF:= strWinDir+ 'BOOTLOG.TXT';
       end;
    end;

 if FileExists(strF)
 then Result:= FileAge(strF);
end;



function SystemTimeIsInvalid: Boolean;                                                              { returns true if the SysFile time is bigger than current clock time }
VAR SystemTime: TDateTime;
begin
 SystemTime:= GetSysFileTime;                                                                      { exista si functia Delphi:  FileAge }
 if SystemTime= 0 then
  begin
   MessageDlg('Can''t get system time!', mtInformation, [mbOk], 0);
   SystemTime:= Now- 0.1;
  end;
 Result:= (Now < SystemTime);
end;



{ Non-blocking sleep/delay.
  WARNING: This function uses Application.ProcessMessages which is generally discouraged!
  It can cause reentrancy issues if event handlers trigger during the delay.
  Consider using TThread or TTask for better alternatives.
  See: https://blog.dummzeuch.de/2018/09/29/calling-application-processmessages-in-a-delphi-program/ }
procedure DelayEx(CONST ms: Cardinal);
VAR
  Count: Cardinal;
begin
 Count:= GetTickCount;                    { Note: GetTickCount accuracy is > 15ms. See https://blogs.msdn.microsoft.com/oldnewthing/20050902-00/?p=34333 }
 REPEAT
  Sleep(1);                               { Without this we get 100% CPU utilization because the loop is too tight }
  Application.ProcessMessages;            { DISCOURAGED - see function comment }
 UNTIL (GetTickCount - Count) >= ms;
end;






{--------------------------------------------------------------------------------------------------
   TIME-PROTECTION
--------------------------------------------------------------------------------------------------}
procedure CurrentSysTimeStore(CONST SecretKey: string);
begin
 if SecretKey = ''
 then raise Exception.Create('CurrentSysTimeStore: SecretKey parameter cannot be empty');

 RegWriteDate(HKEY_CURRENT_USER, SecretKey, 'System', Now, TRUE);
end;


function CurrentSysTimeValid(CONST SecretKey: string): Boolean;
VAR
  LastTime: TDateTime;
begin
 if SecretKey = ''
 then raise Exception.Create('CurrentSysTimeValid: SecretKey parameter cannot be empty');

 LastTime:= RegReadDate(HKEY_CURRENT_USER, SecretKey, 'System');
 Result:= (LastTime <= Now) AND NOT SystemTimeIsInvalid;
end;







end.


