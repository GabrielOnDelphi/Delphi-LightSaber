UNIT csSystem;

{=============================================================================================================
   SYSTEM
   2025.01
   www.GabrielMoraru.com
   See Copyright file
==============================================================================================================

  System function to access:
     BIOS
     Services
     Clipboard
     Font
     System time
     Computer name
     Screen
     Running processes

  See:
     cmEnvironmentVar.pas for environment variables
     chHardID.pas

   In this group:
     * csShell.pas
     * csSystem.pas
     * csWindow.pas
     * csWindowMetrics.pas
     * csExecuteProc.pas
     * csExecuteShell.pas
     * csProcess.pas

=============================================================================================================}

INTERFACE
USES
   Winapi.Windows, Winapi.Messages, Winapi.WinSock, Winapi.WinSvc,
   System.Classes, System.SysUtils, System.Win.Registry, System.UITypes, IOUtils,
   Vcl.Controls, Vcl.Forms, cbAppDataForm,Vcl.ClipBrd, Vcl.Dialogs;


{==================================================================================================
   BIOS
==================================================================================================}
 function BiosDate : string;
 function BiosID : string;


{==================================================================================================
   SERVICES
==================================================================================================}
 function ServiceStart        (aMachine, aServiceName: string): Boolean;
 function ServiceStop         (aMachine, aServiceName: string): Boolean;
 function ServiceGetStatus    (sMachine, sService    : string): DWord;
 function ServiceGetStatusName(sMachine, sService    : string): string;


{==================================================================================================
   WND HACKS
==================================================================================================}
 procedure Hack_DisableSystemKeys(Disable: Boolean);                                               { Disable task switching - Using the SystemParametersInfo API function you can trick Windows into thinking that the screen saver is running. Doing so disables the Ctrl-Alt-Delete key sequence from displaying the "Close Program" dialog and rebooting the computer and Alt-Tab from switching to another application. It also stops Ctrl-Esc from opening the Start Menu.  If you wish to disable those keys while your application is running call the following SystemKeys function (place it in the Implementation section of your unit's code - and call from any procedure in your application - where needed). When you call SystemKeys, if Disable is True, the keys will be disabled, False otherwise.  After a call to SystemKeys(True) the program runs, but you are unable to Alt-Tab to it nor switch to it in the task list. You can't invoke the Start button, either. }
 function  GetTextFromHandle(hWND: THandle): string;

 function CalculatePasswordStrength(const Password: string): Integer;



{==================================================================================================
   CLIPBOARD
==================================================================================================}
 function  StringToClipboard  (CONST Str: string; CONST MaxRetries: Integer= 20): Boolean;         { Returns true if it succeded in writing the clipboard }
 function  StringFromClipboard(CONST MaxWaitTime: Cardinal= 5000): string;
 function  StringFromClipboardTSL: TStringList;


{==================================================================================================
   FONTS
==================================================================================================}
 function  InstallFont(FontFileName: string): Boolean;
 procedure UseUninstalledFont(FontFile: string);                                                   { Use a font without installing it. DON'T FORGET TO RELEASE IT WHEN YOU FINISH WITH IT or when you close the program }
 procedure FreeUninstalledFont(FontFile: string);


{==================================================================================================
   SYS TIME
==================================================================================================}
 procedure DelayEx(CONST ms : cardinal);

 function  GetSysFileTime: TDateTime;                                                              { gets current date from system file - prevents cracking}
 function  SystemTimeIsInvalid: Boolean;                                                           { returns true if the system time is bigger than current clock time }

 procedure CurrentSysTimeStore(SecretKey: string);                                                 { Stores current system clock time to a hidden registry key }
 function  CurrentSysTimeValid(SecretKey: string): Boolean;                                        { Read the last saved system time and compares it with current clock. If current value is smaller than the stored value it means that the clock time was set back }

 function  WindowsUpTime : TDateTime;
 function  UserIdleTime: Cardinal;
 { Also see Delphi function: FileAge }


{==================================================================================================
   COMPUTER INFO
==================================================================================================}
 function  GetComputerName: string;
 function  GetHostName    : string;

 function  GetLogonName   : string;
 function  GetDomainName  : String;                                                                { for home users it shows the computer name (Qosmio) }
 function  GetUserName (AllowExceptions: Boolean = False): string; // NOT TESTED
 function  GetUserNameEx (ANameFormat: Cardinal): string;                                    { source http://stackoverflow.com/questions/8446940/how-to-get-fully-qualified-domain-name-on-windows-in-delphi }


{==================================================================================================
   Monitor / Screen

   Also see cmPowerUtils.pas:
      Monitor Off,
      ScreenSaver On,
      IsScreenSaverOn
==================================================================================================}
 function  GetDisplayModes: string;                                                               { Returns the resolutions supported }

 procedure PrintScreenActiveWnd;
 procedure PrintScreenFull;


{==================================================================================================
   MOUSE
==================================================================================================}
 procedure JiggleMouse;
 procedure CursorBusy;
 procedure CursorNotBusy;



IMPLEMENTATION

USES
   csKeyboard, ccIO, cmIO, cmIO.Win, cbVersion, cbRegistry;




function GetDisplayModes: string;                                                                  { returns the resolutions supported }
VAR
  cnt : Integer;
  DevMode : TDevMode;
begin
 cnt := 0;
 Result:= '';
 WHILE EnumDisplaySettings(NIL, cnt, DevMode) DO                                                   {TODO: instead of NIL I have to provide the name of the monitor }
  begin
   with Devmode DO
     Result:= Result+ (Format('%dx%d %d Colors', [dmPelsWidth,dmPelsHeight,Int64(1) shl dmBitsperPel]))+ #13#10;
   Inc(cnt) ;
  end;
end;



procedure PrintScreenActiveWnd;
begin
  SimulateKeystroke(VK_SNAPSHOT, 1);    {  1= ActiveWin  |  0= Whole screen }
end;


procedure PrintScreenFull;
begin
  SimulateKeystroke(VK_SNAPSHOT, 0);
end;




























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











{--------------------------------------------------------------------------------------------------
                            GET COMPUTER INFO
--------------------------------------------------------------------------------------------------}

{  Does not work in win95/98
   Also see GetComputerNameEx:
      http://stackoverflow.com/questions/30778736/how-to-get-the-full-computer-name-in-inno-setup/30779280#30779280 }
function GetComputerName: string;
VAR
  buffer: array[0..MAX_COMPUTERNAME_LENGTH + 1] of Char; //ok
  Size: Cardinal;
begin
  Size := MAX_COMPUTERNAME_LENGTH + 1;
  WinApi.Windows.GetComputerName(@buffer, Size);
  Result := StrPas(buffer);
end;



{ works on Win 7 }
function GetLogonName: string;
CONST cnMaxNameLen = 254;
var
  sName: string;
  dwNameLen: DWORD;
begin
  dwNameLen:= cnMaxNameLen - 1;
  SetLength  (sName, cnMaxNameLen);
  WinApi.Windows.GetUserName(PChar(sName), dwNameLen);
  SetLength  (sName, dwNameLen);
  Result:= UpperCase(Trim(sName));
end;


{ For home users it shows the computer name (Qosmio).
  There is another function with the same name in WinSock }
function GetHostName: string;    { It returns the name of my laptop: 'Qosmio' }
var
  HName: array[0..100] of AnsiChar;     //HEnt: pHostEnt;
  WSAData: TWSAData;
begin
 if WSAStartup($0101, WSAData) <> 0 then EXIT('');

 if WinApi.Winsock.gethostname(HName, SizeOf(hName)) = 0
 then Result:= string(HName)   // HEnt := gethostbyname(HName);
 else Result:= '';
end;


{ For home users it shows the computer name (Qosmio) }
function GetDomainName: String;
VAR
  vlDomainName: array[0..30] of WideChar;
  vlSize: ^DWORD;
begin
 New(vlSize);
 vlSize^ := 30;
 WinApi.Windows.ExpandEnvironmentStrings(PChar('%USERDOMAIN%'), vlDomainName, vlSize^);
 system.Dispose(vlSize);
 Result:= vlDomainName;
end;


//See https://msdn.microsoft.com/en-us/library/cc761107.aspx
function GetUserName(AllowExceptions: Boolean = FALSE): string;
CONST
  UNLEN = 256;
  MAX_BUFFER_SIZE = MAX_COMPUTERNAME_LENGTH + UNLEN + 1 + 1;
VAR
  BufSize: DWORD;
begin
  BufSize := MAX_BUFFER_SIZE;
  SetLength(Result, BufSize + 1);
  if WinApi.Windows.GetUserName(PChar(Result), BufSize)
  then SetLength(Result, BufSize - 1)
  else
    begin
      if AllowExceptions
      then RaiseLastOSError;
      Result := '';
    end;
end;


{ For 2, returns computer name + user name.
  Ex: GODZILLA\John Lennon }
function GetUserNameEx(ANameFormat: Cardinal): string;
{See the constants defined in WinApi.Windows.pas EXTENDED_NAME_FORMAT enum.
  NameUnknown            = 0;
  NameFullyQualifiedDN   = 1;
  NameSamCompatible      = 2;
  NameDisplay            = 3;
  NameUniqueId           = 6;
  NameCanonical          = 7;
  NameUserPrincipal      = 8;
  NameCanonicalEx        = 9;
  NameServicePrincipal   = 10;
  NameDnsDomain          = 12;}
var
  Buf: array[0..511] of WideChar; // Use WideChar for Unicode support.
  BufSize: ULONG;                 // ULONG matches the parameter type.
  Secur32: HMODULE;
  GetUserNameEx: function(NameFormat: Cardinal; lpNameBuffer: LPWSTR; var nSize: ULONG): BOOL; stdcall;
begin
  Result := '';
  Secur32 := LoadLibrary('secur32.dll'); // Explicitly load the library.
  if Secur32 = 0
  then RAISE Exception.Create('Unable to load secur32.dll.');
  try
    @GetUserNameEx := GetProcAddress(Secur32, 'GetUserNameExW'); // Use Unicode version.
    if not Assigned(GetUserNameEx) then
      raise Exception.Create('GetUserNameExW function not found in secur32.dll.');
    BufSize := Length(Buf);
    if GetUserNameEx(ANameFormat, Buf, BufSize)
    then Result := WideCharToString(Buf)
    else RaiseLastOSError; // Raise an error if the function call fails.
  finally
    FreeLibrary(Secur32); // Ensure the library is freed.
  end;
end;












{--------------------------------------------------------------------------------------------------
                                   FONTS
--------------------------------------------------------------------------------------------------}
{Resurse despre font-uri:

  How to install fonts                                     http://www.chami.com/tips/delphi/010297D.html
  Convert font attributes to a string and vise versa       http://www.chami.com/tips/delphi/112596D.html
  (Un)installing a font                                    http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_21581694.html?qid=21581694
  install font                                             http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_20906162.html?qid=20906162 }


function InstallFont(FontFileName: string): Boolean;
const
  Win9x= 'Software\Microsoft\Windows\CurrentVersion\Fonts';
  WinXP= 'SOFTWARE\Microsoft\Windows NT\CurrentVersion\Fonts';
var
  CopyToWin: string;
  WindowsPath: array[0..MAX_PATH] of char;  //ok
  RegData: TRegistry;
begin
 Result:= FALSE;
 GetWindowsDirectory(WindowsPath, MAX_PATH);
 CopyToWin:= WindowsPath + '\Fonts\' + ExtractFileName(FontFileName);

 if NOT FileExists(CopyToWin) then
  begin

   { COPY FONT TO WINDOWS }
   TFile.Copy(FontFileName, CopyToWin, FALSE);

   { WRITE TO REGISTRY }
   RegData := TRegistry.Create;
   TRY
     RegData.RootKey  := HKEY_LOCAL_MACHINE;
     RegData.LazyWrite:= FALSE;

     Result:= RegData.KeyExists(WinXP) AND RegData.OpenKey(WinXP, FALSE);                          { poate e Windows XP }
     if NOT Result
     then Result:= RegData.KeyExists(Win9x) AND RegData.OpenKey(Win9x, FALSE);                     { poate e Windows 9x }
     if Result then
      TRY
        RegData.WriteString(ExtractOnlyName(FontFileName), ExtractFileName(FontFileName));
      except
        //todo 1: trap only specific exceptions
        Result:= FALSE;
      END;

    FINALLY
      RegData.CloseKey;
      FreeAndNil(RegData);
    END;

   { NOTIFY THE SYSTEM }
   if Result then
    begin
     AddFontResource(PChar(FontFileName));
     SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);                              //cmDebugger.OutputDebugStr('Preparing to signal new font to the system. Sending message now...');
    end;
  end;
end;


procedure UseUninstalledFont(FontFile: string);                                     { Use a font without installing it. DON'T FORGET TO RELEASE IT WHEN YOU FINISH WITH IT or when you close the program }
begin                                                                               { working only with true type files }
  if FileExists(FontFile) then
   begin
    AddFontResource (PChar(FontFile));
    (* SAU ASA:  Result:= AddFontResourceEx(PChar(FontFile), FR_PRIVATE, nil);      { Font installing (just for the current process, and only for the duration of the process). Cu asta, nu trebuie sa mai apelez SendMessage *)
    SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
   end;
  { DONT FORGET TO RELESE THE RESOUCE - OnFormClose }
end;


procedure FreeUninstalledFont(FontFile: string);                                    { RELESE THE RESOUCE after you uses a font without installing it }
begin
  if FileExists(FontFile) then
   begin
    RemoveFontResource(PChar(FontFile));
    SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
   end;
end;








{--------------------------------------------------------------------------------------------------
                                    SYSTEM HACK
--------------------------------------------------------------------------------------------------}
{ Disable task switching
  Using the SystemParametersInfo API function you can trick Windows into thinking that the screen saver is running. Doing so disables the Ctrl-Alt-Delete key sequence from displaying the "Close Program" dialog and rebooting the computer and Alt-Tab from switching to another application. It also stops Ctrl-Esc from opening the Start Menu.
  If you wish to disable those keys while your application is running call the following SystemKeys function (place it in the Implementation section of your unit's code - and call from any procedure in your application - where needed). When you call SystemKeys, if Disable is True, the keys will be disabled, False otherwise.
  After a call to SystemKeys(True) the program runs, but you are unable to Alt-Tab to it nor switch to it in the task list. You can't invoke the Start button, either. }
procedure Hack_DisableSystemKeys(Disable: Boolean);
VAR OldVal : LongInt;
begin
 SystemParametersInfo (SPI_SCREENSAVERRUNNING, Word(Disable), @OldVal, 0);
end;



function GetTextFromHandle(hWND: THandle): string;
VAR pText : PChar;
    TextLen : integer;
begin
 TextLen:=GetWindowTextLength(hWND);         { get the length of the text}
 GetMem(pText,TextLen);                      { alocate memory}   // takes a pointer
 TRY
   GetWindowText(hWND, pText, TextLen + 1);  { get the control's text}
   Result:= String(pText);                   { display the text}
 FINALLY
   FreeMem(pText);                           { free the memory}
 END;
end;


function CalculatePasswordStrength(const Password: string): Integer;
begin
  Result := 0;

  // Length criteria
  if Length(Password) >= 8 then Inc(Result);

  // Uppercase letter criteria
  if Password.ToUpper <> Password then Inc(Result);

  // Lowercase letter criteria
  if Password.ToLower <> Password then Inc(Result);

  // Digit criteria
  if Password.IndexOfAny(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) >= 0 then Inc(Result);

  // Special character criteria
  if Password.IndexOfAny(['!', '@', '#', '$', '%', '^', '&', '*', '(', ')']) >= 0 then Inc(Result);
end;










{--------------------------------------------------------------------------------------------------
   SYSTEM  TIME
--------------------------------------------------------------------------------------------------}
{ Also see this: http://docwiki.embarcadero.com/Libraries/XE4/en/System.Diagnostics.TStopwatch
  Time since boot
  Use it as: Date2FormatAuto(WindowsUpTime)  }
function WindowsUpTime: TDateTime;
begin
 Result:= GetTickCount / SecsPerDay / MSecsPerSec;   // GetTickCount accuracy is 15ms+  https://blogs.msdn.microsoft.com/oldnewthing/20050902-00/?p=34333
end;


{ Available only on Win2000 (and up) machines.  Details: To track a user's idle time you could hook keyboard and mouse activity. Note, however, that installing a system-wide message hook is a very invasive thing to do and should be avoided if possible, since it will require your hook DLL to be loaded into all processes. }
function UserIdleTime: Cardinal;
VAR
   liInfo: TLastInputInfo;
begin
   liInfo.cbSize := SizeOf(TLastInputInfo) ;
   GetLastInputInfo(liInfo) ;
   Result := (GetTickCount - liInfo.dwTime) DIV 1000;
end;



{ gets current date from system file - prevents user from reverse time }
function GetSysFileTime: TDateTime;
VAR strWinDir, strF: string;
begin
 Result:= 0;
 strWinDir:= '';
 strWinDir:= GetWinDir;
 Assert(DirectoryExistMsg(strWinDir));

 if cbVersion.IsNTKernel
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
  Warning:
    Use it with care: Application.ProcessMessages will create a lot of problems!
    https://blog.dummzeuch.de/2018/09/29/calling-application-processmessages-in-a-delphi-program/ }
procedure DelayEx(CONST ms : Cardinal);
VAR Count: Cardinal;
begin
 Count:= GetTickCount;                    //ToDo: GetTickCount accuracy is > 15ms. Use cDebug TimerStart instead.     https://blogs.msdn.microsoft.com/oldnewthing/20050902-00/?p=34333
 REPEAT
  Sleep(1);                               // without this we get 100% CPU utilization because the loop is too tight
  Application.ProcessMessages;
 UNTIL (GetTickCount-Count)>= ms;
end;






{--------------------------------------------------------------------------------------------------
   TIME-PROTECTION
--------------------------------------------------------------------------------------------------}
procedure CurrentSysTimeStore(SecretKey: string);
begin
 RegWriteDate(HKEY_CURRENT_USER, SecretKey, 'System', Now, TRUE);
end;



function CurrentSysTimeValid(SecretKey: string): Boolean;
VAR LastTime: TDateTime;
begin
 LastTime:= RegReadDate(HKEY_CURRENT_USER, SecretKey, 'System');
 Result:= (LastTime<= Now) AND NOT SystemTimeIsInvalid;
end;





{--------------------------------------------------------------------------------------------------
   SERVICES
   aMachine is UNC path or local machine if left empty
--------------------------------------------------------------------------------------------------}
function ServiceStart(aMachine,aServiceName : string) : boolean;  { From BlackBox.pas }
var
   h_manager,h_svc: SC_Handle;
   svc_status: TServiceStatus;
   Temp: PChar;
   dwCheckPoint: DWord;
begin
  svc_status.dwCurrentState := 1;
  h_manager := OpenSCManager(PChar(aMachine), nil,SC_MANAGER_CONNECT);

  if h_manager > 0 then begin
    h_svc := OpenService(h_manager, PChar(aServiceName),
                         SERVICE_START or SERVICE_QUERY_STATUS);
    if h_svc > 0 then begin
      temp := nil;
      if (StartService(h_svc,0,temp)) then
        if (QueryServiceStatus(h_svc,svc_status)) then begin
          while (SERVICE_RUNNING <> svc_status.dwCurrentState) do begin
            dwCheckPoint := svc_status.dwCheckPoint;
            Sleep(svc_status.dwWaitHint);
            if (not QueryServiceStatus(h_svc,svc_status)) then break;
            if (svc_status.dwCheckPoint < dwCheckPoint) then begin
              // QueryServiceStatus didn't increment dwCheckPoint
              break;
            end;
          end;
        end;
      CloseServiceHandle(h_svc);
    end;
    CloseServiceHandle(h_manager);
  end;

  Result := (SERVICE_RUNNING = svc_status.dwCurrentState);
end;


function ServiceStop(aMachine,aServiceName : string) : boolean;    { From BlackBox.pas }
var h_manager,h_svc   : SC_Handle;
    svc_status     : TServiceStatus;
    dwCheckPoint : DWord;
begin
  h_manager:=OpenSCManager(PChar(aMachine),nil,SC_MANAGER_CONNECT);

  if h_manager > 0 then begin
    h_svc := OpenService(h_manager,PChar(aServiceName), SERVICE_STOP or SERVICE_QUERY_STATUS);

    if h_svc > 0 then
     begin
       if(ControlService(h_svc,SERVICE_CONTROL_STOP,svc_status)) then
         if(QueryServiceStatus(h_svc,svc_status))then
           while(SERVICE_STOPPED <> svc_status.dwCurrentState) do
           begin
             dwCheckPoint := svc_status.dwCheckPoint;
             Sleep(svc_status.dwWaitHint);

             if NOT QueryServiceStatus(h_svc,svc_status) then break;    // couldn't check status
             if (svc_status.dwCheckPoint < dwCheckPoint) then break;
           end;
       CloseServiceHandle(h_svc);
     end;
    CloseServiceHandle(h_manager);
  end;

  Result := (SERVICE_STOPPED = svc_status.dwCurrentState);
end;


// ================================
// Status Constants
// SERVICE_STOPPED
// SERVICE_RUNNING
// SERVICE_PAUSED
// SERVICE_START_PENDING
// SERVICE_STOP_PENDING
// SERVICE_CONTINUE_PENDING
// SERVICE_PAUSE_PENDING
// =================================
function ServiceGetStatus(sMachine, sService: string ): DWord;   { From BlackBox.pas }
var h_manager,h_svc : SC_Handle;
    service_status  : TServiceStatus;
    hStat           : DWord;
begin
  hStat := 0;
  h_manager := OpenSCManager(PChar(sMachine) ,nil,SC_MANAGER_CONNECT);

  if h_manager > 0 then begin
    h_svc := OpenService(h_manager,PChar(sService),SERVICE_QUERY_STATUS);

    if h_svc > 0 then begin
      if(QueryServiceStatus(h_svc, service_status)) then
        hStat := service_status.dwCurrentState;

      CloseServiceHandle(h_svc);
    end;
    CloseServiceHandle(h_manager);
  end;

  Result := hStat;
end;


function ServiceGetStatusName(sMachine,sService: string ): string;   { From BlackBox.pas }
var Cmd : string;
    Status : DWord;
begin
  Status := ServiceGetStatus(sMachine,sService);
  case Status of
    SERVICE_STOPPED         : Cmd := 'STOPPED';
    SERVICE_RUNNING         : Cmd := 'RUNNING';
    SERVICE_PAUSED          : Cmd := 'PAUSED';
    SERVICE_START_PENDING   : Cmd := 'STARTING';
    SERVICE_STOP_PENDING    : Cmd := 'STOPPING';
    SERVICE_CONTINUE_PENDING: Cmd := 'RESUMING';
    SERVICE_PAUSE_PENDING   : Cmd := 'PAUSING';
  else
    Cmd := 'UNKNOWN STATE';
  end;
  Result := Cmd;
end;




// ================================================
// Bios Information: Win2000/NT compatible
// ================================================
function BiosDate : string;   { From BlackBox.pas }
var Cmd : string;
    WinReg : TRegistry;
begin
  WinReg := nil;
  Cmd := '????????';

  // Win 2000/NT
  WinReg := TRegistry.Create;
  TRY
    WinReg.RootKey := HKEY_LOCAL_MACHINE;
    if WinReg.OpenKeyReadOnly('\HARDWARE\DESCRIPTION\System') then
       Cmd := WinReg.ReadString('SystemBiosDate');
  FINALLY
     FreeAndNil(WinReg);
  END;

  Result := Cmd;
end;


function BiosID : string;  { From BlackBox.pas }
var Cmd : string;
    Buffer : PChar;
    WinReg : TRegistry;
begin
  WinReg := nil;
  Cmd := '????????';

  // Win 2000/NT
  TRY
    WinReg := TRegistry.Create;
    WinReg.RootKey := HKEY_LOCAL_MACHINE;
    if WinReg.OpenKeyReadOnly('\HARDWARE\DESCRIPTION\System') then
    begin
       GetMem(Buffer,$2000);
       WinReg.ReadBinaryData('SystemBiosVersion',Buffer^,$2000);
       Cmd := WinReg.ReadString('Identifier') + ' ' + Buffer;
       FreeMem(Buffer);
    end;
  FINALLY
     FreeAndNil(WinReg);
  END;

  Result := Cmd;
end;




{-----------------------------------------------------------------------------
   MONITOR
-----------------------------------------------------------------------------}
{ Simulates mouse movement, so that the screen saver does not start.
  This is the only way to prevent the screen saver to start from Vista onwards if password protection is enabled.
  According to http://stackoverflow.com/a/1675793/49925 }
procedure JiggleMouse;
var
  Inpt: TInput;
begin
  Inpt.Itype := INPUT_MOUSE;
  Inpt.mi.dx := 0;
  Inpt.mi.dy := 0;
  Inpt.mi.mouseData := 0;
  Inpt.mi.dwFlags := MOUSEEVENTF_MOVE;
  Inpt.mi.Time := 0;
  Inpt.mi.dwExtraInfo := 0;
  SendInput(1, Inpt, SizeOf(Inpt));
end;


procedure CursorBusy;
begin
 Screen.Cursor:= crHourGlass;
end;


procedure CursorNotBusy;
begin
 Screen.Cursor:= crDefault;
end;


end.


