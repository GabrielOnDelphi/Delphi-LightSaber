UNIT LightVcl.Common.System;

{=============================================================================================================
   SYSTEM
   2025.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
==============================================================================================================

  System function to access:
     BIOS
     Services
     Clipboard
     Font
     Computer name
     Screen
     Running processes

  See:
     cmEnvironmentVar.pas for environment variables
     chHardID.pas

   In this group:
     * LightVcl.Common.Shell.pas
     * csSystem.pas
     * csWindow.pas
     * LightVcl.Common.WindowMetrics.pas
     * LightVcl.Common.ExecuteProc.pas
     * LightVcl.Common.ExecuteShell.pas
     * csProcess.pas
     * LightVcl.Common.SystemTime

=============================================================================================================}

INTERFACE
USES
   Winapi.Windows, Winapi.Messages, Winapi.WinSock, Winapi.WinSvc,
   System.SysUtils, System.Win.Registry, System.IOUtils,
   Vcl.Controls, Vcl.Forms;


{==================================================================================================
   SYSTEM SERVICES
==================================================================================================}
 function ServiceStart        (CONST aMachine, aServiceName: string): Boolean;
 function ServiceStop         (CONST aMachine, aServiceName: string): Boolean;
 function ServiceGetStatus    (CONST sMachine, sService: string): DWord;
 function ServiceGetStatusName(CONST sMachine, sService: string): string;


{==================================================================================================
   SYSTEM FONTS
==================================================================================================}
 function  InstallFont(CONST FontFileName: string): Boolean;
 procedure UseUninstalledFont(CONST FontFile: string);                                             { Use a font without installing it. DON'T FORGET TO RELEASE IT when you close the program }
 procedure FreeUninstalledFont(CONST FontFile: string);


{==================================================================================================
   SYSTEM COMPUTER INFO
==================================================================================================}
 function  GetComputerName: string;
 function  GetHostName    : string;

 function  GetLogonName   : string;
 function  GetDomainName  : String;                                                                { for home users it shows the computer name (Qosmio) }
 function  GetUserName (AllowExceptions: Boolean = False): string; // NOT TESTED
 function  GetUserNameEx (ANameFormat: Cardinal): string;                                    { source http://stackoverflow.com/questions/8446940/how-to-get-fully-qualified-domain-name-on-windows-in-delphi }


{==================================================================================================
   SYSTEM Screen
===================================================================================================
   Also see LightVcl.Common.PowerUtils.pas: Monitor Off, ScreenSaver On, IsScreenSaverOn
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


{==================================================================================================
   SYSTEM API
==================================================================================================}
 function GetWin32ErrorString(ErrorCode: DWORD): string;


{==================================================================================================
   HARDWARE BIOS
==================================================================================================}
 function BiosDate: string;
 function BiosID: string;



IMPLEMENTATION

USES
   LightVcl.Common.Keyboard, LightCore.IO;




function GetWin32ErrorString(ErrorCode: DWORD): string;
var
  Buffer: array[0..1023] of Char;
  LangID: Word;
begin
  if ErrorCode = ERROR_SUCCESS // ERROR_SUCCESS is 0
  then Result := 'Operation completed successfully.'
  else
    begin
      LangID := MakeLangID(LANG_NEUTRAL, SUBLANG_DEFAULT); // Default system language
      if FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS, nil,
                       ErrorCode,
                       LangID,
                       Buffer,
                       SizeOf(Buffer) - 1, // -1 for null terminator space
                       nil) = 0
      then
        Result := 'Windows Error Code ' + IntToStr(ErrorCode) + ' (No system description available).'  // FormatMessage failed
      else
        begin
          Result := Buffer;
          Result := TrimRight(Result); // Remove trailing CRLF if present
        end;
    end;
end;




function GetDisplayModes: string;                                                                  { returns the resolutions supported }
VAR
  cnt: Integer;
  DevMode: TDevMode;
begin
 cnt:= 0;
 Result:= '';
 WHILE EnumDisplaySettings(NIL, cnt, DevMode) DO                                                   {TODO: instead of NIL I have to provide the name of the monitor }
  begin
   Result:= Result+ Format('%dx%d %d Colors', [DevMode.dmPelsWidth, DevMode.dmPelsHeight, Int64(1) shl DevMode.dmBitsperPel])+ #13#10;
   Inc(cnt);
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
                            GET COMPUTER INFO
--------------------------------------------------------------------------------------------------}

{  Does not work in win95/98
   Also see GetComputerNameEx:
      http://stackoverflow.com/questions/30778736/how-to-get-the-full-computer-name-in-inno-setup/30779280#30779280 }
function GetComputerName: string;
VAR
  buffer: array[0..MAX_COMPUTERNAME_LENGTH + 1] of Char;
  Size: Cardinal;
begin
  Size := MAX_COMPUTERNAME_LENGTH + 1;
  if WinApi.Windows.GetComputerName(@buffer, Size)
  then Result := StrPas(buffer)
  else Result := '';
end;



{ Works on Win 7 }
function GetLogonName: string;
CONST cnMaxNameLen = 254;
var
  sName: string;
  dwNameLen: DWORD;
begin
  dwNameLen:= cnMaxNameLen - 1;
  SetLength(sName, cnMaxNameLen);
  if WinApi.Windows.GetUserName(PChar(sName), dwNameLen)
  then
    begin
      SetLength(sName, dwNameLen - 1);  { -1 because dwNameLen includes null terminator }
      Result:= UpperCase(Trim(sName));
    end
  else
    Result:= '';
end;


{ For home users it shows the computer name (Qosmio).
  There is another function with the same name in WinSock }
function GetHostName: string;    { It returns the name of my laptop: 'Qosmio' }
var
  HName: array[0..100] of AnsiChar;
  WSAData: TWSAData;
begin
 if WSAStartup($0101, WSAData) <> 0
 then EXIT('');
 TRY
   if WinApi.Winsock.gethostname(HName, SizeOf(hName)) = 0
   then Result:= string(HName)
   else Result:= '';
 FINALLY
   WSACleanup;
 END;
end;


{ For home users it shows the computer name (Qosmio) }
function GetDomainName: String;
VAR
  vlDomainName: array[0..30] of WideChar;
  vlSize: DWORD;
begin
 vlSize := Length(vlDomainName);
 WinApi.Windows.ExpandEnvironmentStrings(PChar('%USERDOMAIN%'), vlDomainName, vlSize);
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


function InstallFont(CONST FontFileName: string): Boolean;
const
  Win9x= 'Software\Microsoft\Windows\CurrentVersion\Fonts';
  WinXP= 'SOFTWARE\Microsoft\Windows NT\CurrentVersion\Fonts';
var
  CopyToWin: string;
  WindowsPath: array[0..MAX_PATH] of char;
  RegData: TRegistry;
begin
 if FontFileName = ''
 then raise Exception.Create('InstallFont: FontFileName parameter cannot be empty');

 if NOT FileExists(FontFileName)
 then raise Exception.Create('InstallFont: Font file not found: ' + FontFileName);

 Result:= FALSE;
 GetWindowsDirectory(WindowsPath, MAX_PATH);
 CopyToWin:= WindowsPath + '\Fonts\' + ExtractFileName(FontFileName);

 if NOT FileExists(CopyToWin) then
  begin

   { COPY FONT TO WINDOWS }
   TFile.Copy(FontFileName, CopyToWin, FALSE);  { Note: TFile is deprecated but kept for compatibility }

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
     SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);                              //LightCom.Debugger.OutputDebugStr('Preparing to signal new font to the system. Sending message now...');
    end;
  end;
end;


{ Use a font without installing it.
  DON'T FORGET TO RELEASE IT WHEN YOU FINISH WITH IT or when you close the program.
  Works only with TrueType files. }
procedure UseUninstalledFont(CONST FontFile: string);
begin
  if FontFile = ''
  then raise Exception.Create('UseUninstalledFont: FontFile parameter cannot be empty');

  if NOT FileExists(FontFile)
  then raise Exception.Create('UseUninstalledFont: Font file not found: ' + FontFile);

  AddFontResource(PChar(FontFile));
  { Alternative: AddFontResourceEx(PChar(FontFile), FR_PRIVATE, nil) - installs font just for the current process }
  SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
  { DON'T FORGET TO RELEASE THE RESOURCE - call FreeUninstalledFont OnFormClose }
end;


{ Release the resource after you used a font without installing it }
procedure FreeUninstalledFont(CONST FontFile: string);
begin
  if FontFile = ''
  then raise Exception.Create('FreeUninstalledFont: FontFile parameter cannot be empty');

  if NOT FileExists(FontFile)
  then raise Exception.Create('FreeUninstalledFont: Font file not found: ' + FontFile);

  RemoveFontResource(PChar(FontFile));
  SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
end;







{--------------------------------------------------------------------------------------------------
   SERVICES
   aMachine is UNC path or local machine if left empty
--------------------------------------------------------------------------------------------------}
function ServiceStart(CONST aMachine, aServiceName: string): boolean;  { From BlackBox.pas }
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


function ServiceStop(CONST aMachine, aServiceName: string): boolean;    { From BlackBox.pas }
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
function ServiceGetStatus(CONST sMachine, sService: string): DWord;   { From BlackBox.pas }
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


function ServiceGetStatusName(CONST sMachine, sService: string): string;   { From BlackBox.pas }
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
function BiosDate: string;   { From BlackBox.pas }
var
  Cmd: string;
  WinReg: TRegistry;
begin
  Cmd := '????????';

  WinReg := TRegistry.Create;
  TRY
    WinReg.RootKey := HKEY_LOCAL_MACHINE;
    if WinReg.OpenKeyReadOnly('\HARDWARE\DESCRIPTION\System')
    then Cmd := WinReg.ReadString('SystemBiosDate');
  FINALLY
     FreeAndNil(WinReg);
  END;

  Result := Cmd;
end;


function BiosID: string;  { From BlackBox.pas }
var
  Cmd: string;
  Buffer: PChar;
  WinReg: TRegistry;
begin
  Cmd := '????????';

  WinReg := TRegistry.Create;
  TRY
    WinReg.RootKey := HKEY_LOCAL_MACHINE;
    if WinReg.OpenKeyReadOnly('\HARDWARE\DESCRIPTION\System') then
    begin
       GetMem(Buffer, $2000);
       TRY
         WinReg.ReadBinaryData('SystemBiosVersion', Buffer^, $2000);
         Cmd := WinReg.ReadString('Identifier') + ' ' + Buffer;
       FINALLY
         FreeMem(Buffer);
       END;
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


