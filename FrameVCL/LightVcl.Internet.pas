UNIT LightVcl.Internet;

{-------------------------------------------------------------------------------------------------------------
   Gabriel Moraru
   2025
   www.GabrielMoraru.com
   See Copyright file

   URL utils / URL parsing and validation

   This unit adds 87 kbytes to EXE size

   Related:
      Internet Status Detector: c:\Projects-3rd_Packages\Third party packages\InternetStatusDetector.pas

   Tester:
      LightSaber\Demo\Tester Internet\
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
   Winapi.Windows,
   Winapi.UrlMon,
   Winapi.WinSock,  { Required by GetLocalIP }
   Winapi.WinInet,  { Required by IE_ApplySettings }
   System.SysUtils, System.StrUtils, System.Classes, System.IniFiles, System.Win.Registry,
   LightCore, LightCore.Time, LightCore.Types,
   {LightCore.Internet,} LightVcl.Common.Dialogs;




{--------------------------------------------------------------------------------------------------
   URL PROCESSING
--------------------------------------------------------------------------------------------------}
 // BETTER PARSER HERE: c:\MyProjects\Packages\BSalsa EmbeddedWB\EwbUrl.pas

 function  ParseURL                 (CONST lpszUrl: string): TStringArray;                        { Breaks an URL in all its subcomponents. Example: ParseURL('http://login:password@somehost.somedomain.com/some_path/something_else.html?param1=val&param2=val')   }


{--------------------------------------------------------------------------------------------------
   URL VALIDATION
--------------------------------------------------------------------------------------------------}

 {$EXTERNALSYM PathIsURLA}
 function  PathIsURLA(pszPath: PAnsiChar): BOOL; stdcall;  {$EXTERNALSYM PathIsURLW}              { $HPPEMIT '#include <shlwapi.h>'}
 function  PathIsURLW(pszPath: PWideChar): BOOL; stdcall;                                         { from here: https://msdn.microsoft.com/en-us/library/windows/desktop/bb773724(v=vs.85).aspx. But it is not good at all because it only checks if path starts with http and if it contains space. Otherwise it accepts all other characters. So I use it in conjunction with my own function }

 function  CheckURLStartMsg         (CONST URL: string): Boolean;                                 { Check if the URL starts with HTTP or with www }


{--------------------------------------------------------------------------------------------------
   IP ADDRESS
--------------------------------------------------------------------------------------------------}
 function  GetLocalIP: string;                                              overload;
 function  GetLocalIP(OUT HostName, IpAddress, ErrorMsg: string): Boolean;  overload;

 function  ResolveAddress (CONST HostName: String; out Address: DWORD): Boolean;

 function GenerateInternetRep: string;  // Generate report

 function CoCreateGuid(var guid: TGUID): HResult; stdcall; far external 'ole32.dll';


{--------------------------------------------------------------------------------------------------
   Internet EXPLORER
--------------------------------------------------------------------------------------------------}
 function  IE_EnableProxy(const Server: String): Boolean;
 function  IE_DisableProxy: Boolean;
 function  IE_GetProxySettings(OUT ProxyAdr, ProxyPort: string; OUT IsEnabled: boolean): Boolean;
 procedure IE_DeleteCache;
 procedure IE_EndSession;
 procedure IE_SetProxy(CONST Proxy: string);   { Change IE proxy settings globally }
 //How to abort TWeBrowser navigation progress?    http://stackoverflow.com/questions/8976933/how-to-abort-twebrowser-navigation-progress


 {--------------------------------------------------------------------------------------------------
   IS CONNECTED
--------------------------------------------------------------------------------------------------}
 function  PCConnected2Internet: Boolean;                                       { From here: http://www.delphipages.com/forum/showthread.php?t=198159 }

 function  ProgramConnect2Internet: Integer;                                   { Function returns: -1 if computer is not connected to internet, 0 if local system is connected to internet but application is blocked by firewall, 1 if application can connect to internet}
 function  ProgramConnect2InternetS: string;
 function  TestProgramConnection(ShowMsgOnSuccess: Boolean= FALSE): Integer;
 function  IsPortOpened(const Host: string; Port: Integer): Boolean;            { Here's something very simple with which you can check a port status(opened/closed) on remote host. Add WinSock to uses clause}
 //see: c:\MyProjects\Projects INTERNET\Test Internet is connected\LightVcl.Internet, LightCore.Internet-is_connected.dpr


{--------------------------------------------------------------------------------------------------
   CREATE .URL FILES
--------------------------------------------------------------------------------------------------}
 Procedure CreateUrlOnDesktop (CONST ShortFileName, sFullURL: string);




IMPLEMENTATION

USES
   LightCore.AppData,
   LightCore.IO,
   LightCore.Internet,
   LightCore.Download;


 function  PathIsUrlA; external 'shlwapi' name 'PathIsURLA';
 function  PathIsUrlW; external 'shlwapi' name 'PathIsURLW';




function CheckURLStartMsg(CONST URL: string): Boolean;             { Check if the URL starts with HTTP or with www }
begin
 Result:= CheckURLStart(URL);

 if NOT Result
 then MessageWarning('Invalid URL:'+ CRLFw+ URL);
end;




{ Breaks an URL in all its subcomponents. Example: ParseURL('http://login:password@somehost.somedomain.com/some_path/something_else.html?param1=val&param2=val')   }
function ParseURL(const lpszUrl: string): TStringArray;    { Source: http://stackoverflow.com/questions/16703063/how-do-i-parse-a-web-url }
VAR
  lpszScheme      : array[0..INTERNET_MAX_SCHEME_LENGTH - 1]    of Char;
  lpszHostName    : array[0..INTERNET_MAX_HOST_NAME_LENGTH - 1] of Char;
  lpszUserName    : array[0..INTERNET_MAX_USER_NAME_LENGTH - 1] of Char;
  lpszPassword    : array[0..INTERNET_MAX_PASSWORD_LENGTH - 1]  of Char;
  lpszUrlPath     : array[0..INTERNET_MAX_PATH_LENGTH - 1]      of Char;
  lpszExtraInfo   : array[0..1024 - 1]                          of Char;
  lpUrlComponents : TURLComponents;
begin
  ZeroMemory(@lpszScheme      , SizeOf(lpszScheme));
  ZeroMemory(@lpszHostName    , SizeOf(lpszHostName));
  ZeroMemory(@lpszUserName    , SizeOf(lpszUserName));
  ZeroMemory(@lpszPassword    , SizeOf(lpszPassword));
  ZeroMemory(@lpszUrlPath     , SizeOf(lpszUrlPath));
  ZeroMemory(@lpszExtraInfo   , SizeOf(lpszExtraInfo));
  ZeroMemory(@lpUrlComponents , SizeOf(TURLComponents));

  lpUrlComponents.dwStructSize      := SizeOf(TURLComponents);
  lpUrlComponents.lpszScheme        := lpszScheme;
  lpUrlComponents.dwSchemeLength    := SizeOf(lpszScheme);
  lpUrlComponents.lpszHostName      := lpszHostName;
  lpUrlComponents.dwHostNameLength  := SizeOf(lpszHostName);
  lpUrlComponents.lpszUserName      := lpszUserName;
  lpUrlComponents.dwUserNameLength  := SizeOf(lpszUserName);
  lpUrlComponents.lpszPassword      := lpszPassword;
  lpUrlComponents.dwPasswordLength  := SizeOf(lpszPassword);
  lpUrlComponents.lpszUrlPath       := lpszUrlPath;
  lpUrlComponents.dwUrlPathLength   := SizeOf(lpszUrlPath);
  lpUrlComponents.lpszExtraInfo     := lpszExtraInfo;
  lpUrlComponents.dwExtraInfoLength := SizeOf(lpszExtraInfo);

  InternetCrackUrl(PChar(lpszUrl), Length(lpszUrl), ICU_DECODE or ICU_ESCAPE, lpUrlComponents);

  SetLength(Result, 6);
  Result[0]:= lpszScheme;                  { Protocol        (http)              }
  Result[1]:= lpszHostName;                { Host            (www.domain.com)    }
  Result[2]:= lpszUserName;                { User            ('')                }
  Result[3]:= lpszPassword;                { Password        ('')                }
  Result[4]:= lpszUrlPath;                 { Path            ('/download.html')  }
  Result[5]:= lpszExtraInfo;               { ExtraInfo       ('')                }
end;












function PCConnected2Internet: Boolean;
VAR dwConnectionTypes: DWORD;
begin
 dwConnectionTypes := INTERNET_CONNECTION_MODEM + INTERNET_CONNECTION_LAN + INTERNET_CONNECTION_PROXY;
 Result := InternetGetConnectedState(@dwConnectionTypes, 0);         { Function summary from MS: Retrieves the connected state of the local system. Minimum supported client: Windows 2000 Professional [desktop apps only] }   { API Function documentation: http://msdn.microsoft.com/en-us/library/windows/desktop/aa384702%28v=vs.85%29.aspx }
end;



function ProgramConnect2InternetS: string;
begin
 if PCConnected2Internet
 then
  begin
    Result:= LightCore.Download.DownloadAsString('http://www.google.com/');
    if Result= ''
    then Result:= CheckYourFirewallMsg
    else Result:= ConnectedToInternet
  end
 else
   Result:= ComputerCannotAccessInet;
end;



{ Returns:
           -1 if computer is not connected to internet,
            0 if local system is connected to internet but application is blocked by firewall,
            1 if application can connect to internet}
function ProgramConnect2Internet: Integer;
begin
 if PCConnected2Internet
 then
   if LightCore.Download.DownloadAsString('http://www.google.com/') > ''                                   { 1 = Can connect to internet, 0 = blocked by firewall, -1 = PC not connected to Internet }
   then Result := 1
   else Result := 0
 else
   Result := -1;
end;


 {
function ProgramConnectMsg(CONST Connection: Integer): string;                                     { same as ProgramConnect2Internet but returns a string
begin
 case Connection of
  -1: Result:= ComputerCannotAccessInet;
   0: Result:= CheckYourFirewallMsg;
  +1: Result:= 'Successfully connected to Internet';
 end;
end; }



{ if ShowMsgOnSuccess = fasle then show message ONLY if it cannot connect to Internet }
function TestProgramConnection(ShowMsgOnSuccess: Boolean= FALSE): Integer;                                        { Old name: ProgramConnectMsg }
begin
 Result:= ProgramConnect2Internet;
 case Result of
  -1: MessageWarning(ComputerCannotAccessInet);
   0: MessageError(CheckYourFirewallMsg);
  +1: if ShowMsgOnSuccess
      then MessageInfo('Successfully connected to Internet');
 end;
end;







{==================================================================================================
   GET IP ADDRESS
==================================================================================================}

{
IsConnectedToInternet Example 2

USES WinInet   <-   This will generate error if WinInet library is not installed in the computer. Dont added to the uses clauses if not needed
function IsConnectedToInternet2: Boolean;
CONST
  INTERNET_CONNECTION_MODEM      = 1; // local system uses a modem to connect to the Internet.
  INTERNET_CONNECTION_LAN        = 2; // local system uses a local area network to connect to the Internet.
  INTERNET_CONNECTION_PROXY      = 4; // local system uses a proxy server to connect to the Internet.
  INTERNET_CONNECTION_MODEM_BUSY = 8; // local system's modem is busy with a non-Internet connection.
VAR
  dwConnectionTypes : DWORD;
BEGIN
  dwConnectionTypes :=
   INTERNET_CONNECTION_MODEM +
   INTERNET_CONNECTION_LAN +
   INTERNET_CONNECTION_PROXY;
  Result := InternetGetConnectedState(@dwConnectionTypes,0);
END;
Note: this solution only works if IE is installed, so it would fail on 'older' machines, like most Windows NT 4 computers. You app would then display an error during program startup if you referred to Wininet. Since today, there are many ways to connect to the Internet (via LAN, Dialup/RAS, ADSL, ..) propably the best way would be to test for certain IPs. Here is a link to more information on the topic, including a list of ways to find out whether an Internet connection seems to be active or not. }


{
  Get ALL local IPs?
  http://stackoverflow.com/questions/576538/delphi-how-to-get-all-local-ips - see the last answer (Remko)
}

Function GetLocalIP: string;
VAR
  HostName, IpAddress, Error: string;
begin
  if GetLocalIP(HostName, IpAddress, Error)
  then Result:= IpAddress
  else Result:= Error;
end;


function GetLocalIP(OUT HostName, IpAddress, ErrorMsg: string): Boolean;
VAR
  Addr: PAnsiChar;
  WSAData: TWSAData;
  RemoteHost: pHostEnt;
  HostNameArr: array[0..255] of AnsiChar;
begin
  Result   := False;
  HostName := '';
  IpAddress:= '';
  ErrorMsg := '';

  // Initialize WinSock
  if WSAStartup($0202, WSAData) <> 0 then
  begin
    ErrorMsg := 'WinSock initialization failed!';
    Exit;
  end;

  try
    // Retrieve the local host name
    if gethostname(HostNameArr, SizeOf(HostNameArr)) = SOCKET_ERROR then
    begin
      case WSAGetLastError of
        WSANOTINITIALISED: ErrorMsg := 'WSA Not Initialized';
        WSAENETDOWN      : ErrorMsg := 'Network subsystem is down';
        WSAEINPROGRESS   : ErrorMsg := 'A blocking operation is in progress';
      else
        ErrorMsg:= 'Unknown error retrieving host name';
      end;

      Exit;
    end;

    HostName := string(HostNameArr);

    // Get host details by name
    RemoteHost := gethostbyname(HostNameArr);
    if RemoteHost = nil then
    begin
      ErrorMsg := 'Unable to resolve host details.';
      Exit;
    end;

    // Extract the IP address
    Addr := RemoteHost^.h_addr_list^;
    while Addr <> nil do
    begin
      for VAR I := 0 to RemoteHost^.h_length - 1 do
        IpAddress := IpAddress + IntToStr(Byte(Addr[I])) + '.';

      SetLength(IpAddress, Length(IpAddress) - 1); // Remove trailing dot
      Break; // Only take the first IP address
    end;

    Result:= True;
  finally
    WSACleanup; // Ensure cleanup happens
  end;
end;





function GenerateInternetRep: string;
var HostName, IPaddr, Error: string;
begin
 Result:= ' [INTERNET]'+ CRLF;

 Result:= Result+'  GetExternalIp: '  + Tab + GetExternalIp+ CRLF;
 Result:= Result+'  GetLocalIP: '+ CRLF;
 if GetLocalIP(HostName, IPaddr, Error)
 then
   begin
     Result:= Result+'     Host: '+ Tab + HostName + CRLF;
     Result:= Result+'     IP'    + Tab + IPaddr   + CRLF;
   end
 else
   Result:= Result+ '     FAIL! '+ Error + CRLF;
end;





{==================================================================================================
   .URL
==================================================================================================}


Procedure CreateUrlOnDesktop(CONST ShortFileName, sFullURL: string);                               { create a URL file - The filename should end in .URL }
VAR sDesktop: string;
    MyReg   : TRegIniFile;
begin
 { Get desktop folder }
 MyReg:= TRegIniFile.Create('Software\MicroSoft\Windows\CurrentVersion\Explorer');
 sDesktop:= MyReg.ReadString('Shell Folders','Desktop','');
 FreeAndNil(MyReg);

 { Write URL file }
 CreateUrl(Trail(sDesktop)+ ShortFileName, sFullURL);
end;





















{--------------------------------------------------------------------------------------------------
                                  Internet EXPLORER
---------------------------------------------------------------------------------------------------

Question:
        How do I set the proxy settings in Internet Explorer without having to restart IE to make it load the settings
Answer:
        It's not the best solution, this I know, but it worked for me, and I couldn't get the BEST solution to work.

How to use it:
   You can simply execute EnableProxy('proxyserver:8080') to set a global proxy
   or you can execute EnableProxy('ftp=ftpproxyserver:2121;gopher=goproxyserver:3333;http=httpproxyserver:8080;https=httpsproxyserver:8080');
   you can of course only fill in one of the options like this EnableProxy('http=httpproxyserver:8080');

Required units:
               WinInet, Registry

Source:
       http://www.delphi3000.com/articles/article_3138.asp

Better way:
       O metoda mult mai buna e aici:   http://www.naddalim.com/forum/showthread.php?t=1454        }


procedure IE_ApplySettings;
VAR HInet: HINTERNET;
begin
  hInet:= InternetOpen(PChar(AppDataCore.AppName), INTERNET_OPEN_TYPE_DIRECT, nil, nil, INTERNET_FLAG_OFFLINE);
  TRY
    if hInet <> NIL
    then InternetSetOption(hInet, INTERNET_OPTION_SETTINGS_CHANGED, nil, 0);
  FINALLY
    InternetCloseHandle(hInet);
  END;
end;


function IE_EnableProxy(const Server: String): Boolean;
VAR Reg : TRegistry;
begin
  Reg:= TRegistry.Create;
  TRY
   TRY
    Reg.RootKey:= HKEY_CURRENT_USER;
    Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Internet Settings', FALSE);
    Reg.WriteString('ProxyServer', Server);
    Reg.WriteBool('ProxyEnable', True);
    Reg.CloseKey;
    Result:= TRUE;
  FINALLY
   FreeAndNil(Reg);
  END;
 except                                                                                            { pe unele sisteme nu pot sa deschid cheia asta si imi ridica o eroare, asa ca folosesc un except }
  //todo 1: trap only specific exceptions
  Result:= FALSE;
 END;

 { InternetSetOption(NIL, INTERNET_OPTION_SETTINGS_CHANGED, NIL, 0);           <--  asa era original insa se pare... }
 if Result then IE_ApplySettings;                                                                  { ...ca metoda asta jos e mai buna }
end;


function IE_DisableProxy: Boolean;
VAR Reg : TRegistry;
begin
  Reg:= TRegistry.Create;
  TRY
   TRY
    Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Internet Settings', False);
    Reg.WriteBool('ProxyEnable', False);
    Reg.CloseKey;
    Result:= TRUE;
  FINALLY
   FreeAndNil(Reg);
  END;
 except                                                                                            { pe unele sisteme nu pot sa deschid cheia asta si imi ridica o eroare, asa ca folosesc un except_ }
  //todo 1: trap only specific exceptions
  Result:= FALSE;
 END;

 { InternetSetOption(NIL, INTERNET_OPTION_SETTINGS_CHANGED, NIL, 0);            <-- asa era original insa se pare... }
 if Result then IE_ApplySettings;                                                                  { ...ca metoda asta jos e mai buna }
end;





{
 READ IE PROXY SETTINGS

 DOCS:
      This is easy and fast way but note that if in future the Microsoft will
      change a key where proxy stored, you must change your code too. For a more
      complicated way, see the link below (solution based on WinInet library).

 Sursa:
      http://www.scalabium.com/faq/dct0161.htm                                  }

function IE_GetProxySettings(out ProxyAdr, ProxyPort: string; out IsEnabled: boolean): Boolean;
VAR Reg : TRegistry;
    ProxyServer: string;

{sub}procedure ParseAndBreak();
     VAR i, j: Integer;
     begin
      if (ProxyServer <> '') then
       begin
         { Extract the HTTP part }
         i:= PosInsensitive('http=', ProxyServer);
         if (i > 0) then
          begin
           Delete(ProxyServer, 1, i+5);
           j:= Pos(';', ProxyServer);
           if (j > 0)
           then ProxyServer:= system.COPY(ProxyServer, 1, j-1);
          end;

         { Break into address and port }
         i:= Pos(':', ProxyServer);
         if (i > 0) then
          begin
           ProxyPort := system.COPY(ProxyServer, i+1, Length(ProxyServer)-i);
           ProxyAdr  := system.COPY(ProxyServer,   1, i-1)
          end
       end;
     end;

begin
 Reg:= TRegistry.Create;
 TRY
  TRY
   Reg.RootKey:= HKEY_CURRENT_USER;
   Result:= Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Internet Settings', FALSE)
        AND Reg.ValueExists('ProxyEnable');

   if Result then
    begin
     ProxyServer:= Reg.ReadString ('ProxyServer');
     IsEnabled  := Reg.ReadBool   ('ProxyEnable');
     ParseAndBreak;                                                                                { PARSE }
    end;
   Reg.CloseKey;
  FINALLY
   FreeAndNil(Reg);
  END;
 except                                                                                            { pe unele sisteme nu pot sa deschid cheia asta si imi ridica o eroare, asa ca folosesc un except }
  //todo 1: trap only specific exceptions
  ProxyAdr:= 'Cannot auto-detect proxy settings.';
  Result:= FALSE;
 END;
end;




procedure setProxy(CONST ProxyIP: string);
VAR
   IP: AnsiString;
   PIInfo: PInternetProxyInfo;
begin
 IP:= AnsiString(Trim(ProxyIP));
 New(PIInfo);
 PIInfo^.dwAccessType := INTERNET_OPEN_TYPE_PROXY;
 PIInfo^.lpszProxy:= PAnsiChar(IP);
 PIInfo^.lpszProxyBypass := PAnsiChar('');
 Winapi.UrlMon.UrlMkSetSessionOption(INTERNET_OPTION_PROXY, piinfo, SizeOf(Internet_Proxy_Info), 0);
 Dispose(PIInfo);
end;


procedure IE_DeleteCache;
var
  lpEntryInfo: PInternetCacheEntryInfo;
  hCacheDir: LongWord;
  dwEntrySize: LongWord;
begin
  dwEntrySize := 0;
  FindFirstUrlCacheEntry(nil, TInternetCacheEntryInfo(nil^), dwEntrySize);
  GetMem(lpEntryInfo, dwEntrySize);
  if dwEntrySize > 0
  then lpEntryInfo^.dwStructSize := dwEntrySize;
  hCacheDir := FindFirstUrlCacheEntry(nil, lpEntryInfo^, dwEntrySize);
  if hCacheDir <> 0 then
    REPEAT
      DeleteUrlCacheEntry(lpEntryInfo^.lpszSourceUrlName);
      FreeMem(lpEntryInfo, dwEntrySize);
      dwEntrySize := 0;
      FindNextUrlCacheEntry(hCacheDir, TInternetCacheEntryInfo(nil^), dwEntrySize);
      GetMem(lpEntryInfo, dwEntrySize);
      if dwEntrySize > 0 then lpEntryInfo^.dwStructSize := dwEntrySize;
    UNTIL NOT FindNextUrlCacheEntry(hCacheDir, lpEntryInfo^, dwEntrySize);
  FreeMem(lpEntryInfo, dwEntrySize);
  FindCloseUrlCache(hCacheDir);
end;


procedure IE_EndSession;
begin
 InternetSetOption(NIL, INTERNET_OPTION_END_BROWSER_SESSION, NIL, 0);
end;

// Change IE proxy settings globally:          http://stackoverflow.com/questions/12732843/authentification-on-http-proxy-in-delphi-xe/21445091#21445091
procedure IE_SetProxy(CONST Proxy: string);
begin
 IE_DeleteCache;
 IE_EndSession;
 SetProxy(Proxy);
end;














{
 Check a port status(opened/closed) on remote host. Uses WinSock.
 http://www.delphigeist.com/search?updated-min=2010-01-01T00%3A00%3A00%2B02%3A00&updated-max=2011-01-01T00%3A00%3A00%2B02%3A00&max-results=37
}
function ResolveAddress(CONST HostName: String; out Address: DWORD): Boolean;
VAR
   lpHost: PHostEnt;
   AnsiHostName: AnsiString;
begin
  AnsiHostName:= AnsiString(HostName);
  Address:= DWORD(INADDR_NONE);                                     // Set default address
  TRY
    if Length(AnsiHostName) > 0 then                                // Check host name length
     begin
      Address:= inet_addr(PAnsiChar(AnsiHostName));                  // Try converting the hostname  // In D7 aici a fost PChar
      if (DWORD(Address) = DWORD(INADDR_NONE)) then                  // Check address
       begin
        lpHost := gethostbyname(PAnsiChar(AnsiHostName));            // Attempt to get host by name

        // Check host ent structure for valid ip address
        if Assigned(lpHost) and Assigned(lpHost^.h_addr_list^)
        then Address := u_long(PLongInt(lpHost^.h_addr_list^)^);     // Get the address from the list
      end;
    end;
  FINALLY
    // Check result address
    if (DWORD(Address) = DWORD(INADDR_NONE))
    then Result:= False    // Invalid host specified
    else Result:= True;   // Converted correctly
  END;
end;


function IsPortOpened(const Host: string; Port: Integer): Boolean;
const
  szSockAddr = SizeOf(TSockAddr);
var
  WinSocketData: TWSAData;
  Socket: TSocket;
  Address: TSockAddr;
  dwAddress: DWORD;
label
  lClean;
begin
  // initialize result
  Result := False;
  // create WinSocketData
  if Winapi.WinSock.WSAStartup(MakeWord(1, 1), WinSocketData) = 0 then
  begin
    // set address family
    Address.sin_family := AF_INET;
    // try to translate Host to IP address
    if NOT ResolveAddress(Host, dwAddress) then
      // faild! go to lClean label
      goto lClean;
    // set the address
    Address.sin_addr.S_addr := dwAddress;
    // create a socket
    Socket := Winapi.WinSock.Socket(AF_INET, SOCK_STREAM, IPPROTO_IP);
    // if faild to create socket
    if Socket = INVALID_SOCKET then
      // go to lClean label
      goto lClean;
    // set the port
    Address.sin_port := Winapi.WinSock.htons(Port);
    // attempt remote connection to Host on Port
    if Winapi.WinSock.Connect(Socket, Address, szSockAddr) = 0 then
     begin
      Result := True;
      // close the socket
      Winapi.WinSock.closesocket(Socket);
     end;// if WinSock.Connect(Socket, Address, szSockAddr) = 0 then begin
  end;// if WinSock.WSAStartup(MakeWord(1, 1), WinSocketData) = 0 then begin
  // label to which we jump to clean up
  lClean:
    Winapi.WinSock.WSACleanup;
end;


{HOW TO USE IT:

if IsPortOpened('google.com', 80) then
  ShowMessage('google has port 80 opened')
else
  ShowMessage('google has port 80 closed???');
}




{
 Similar WinInet resources:
   http://www.delphipages.com/threads/thread.cfm?ID=100717&G=100706
   http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_23867246.html#a22860727
   http://www.naddalim.com/forum/showthread.php?t=1454

 IdHTTP proxy:
   http://delphi.newswhat.com/geoxml/forumhistorythread?groupname=borland.public.delphi.non-technical&messageid=3f43efbc$1@newsgroups.borland.com
}





{--------------------------------------------------------------------------------------------------
                                  GET MAC
--------------------------------------------------------------------------------------------------}
function GetMacAddress: string;
var
  g: TGUID;
  i: Byte;
begin
  Result := '';
  CoCreateGUID(g);
  for i := 2 to 7 do
    Result := Result + IntToHex(g.D4[i], 2);
end;





end.
