UNIT LightVcl.Internet.Common;

{-------------------------------------------------------------------------------------------------------------
   Gabriel Moraru
   2026.10.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Internet and URL utilities for VCL applications.

   Features:
     * URL validation with a message box (CheckURLStartMsg)
     * Internet connectivity test with a message box (TestProgramConnectionMsg)
     * Internet Explorer proxy configuration (IE_EnableProxy, IE_DisableProxy, etc.)
     * URL shortcut file creation (CreateUrlOnDesktop)

   ParseURL, GetLocalIP, ResolveAddress, GenerateInternetRep, PCConnected2Internet, ProgramConnect2Internet(S) and IsPortOpened are in LightCore.Internet.pas.

   Related:
      Internet Status Detector: c:\Projects-3rd_Packages\Third party packages\_Out\_TEMPORARY EXCLUDED (Don't delete them yet!)\InternetStatusDetector.pas

   Tester:
      c:\Projects\LightSaber\Demo\Core\Demo Internet\
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
   Winapi.Windows,
   Winapi.UrlMon,
   Winapi.WinInet,  { Required by IE_ApplySettings }
   System.SysUtils, System.Win.Registry,
   LightCore,
   {LightCore.Internet,} LightVcl.Common.Dialogs;




{--------------------------------------------------------------------------------------------------
   URL VALIDATION
--------------------------------------------------------------------------------------------------}

 {$EXTERNALSYM PathIsURLA}
 function  PathIsURLA(pszPath: PAnsiChar): BOOL; stdcall;  {$EXTERNALSYM PathIsURLW}              { $HPPEMIT '#include <shlwapi.h>'}
 function  PathIsURLW(pszPath: PWideChar): BOOL; stdcall;                                         { from here: https://msdn.microsoft.com/en-us/library/windows/desktop/bb773724(v=vs.85).aspx. But it is not good at all because it only checks if path starts with http and if it contains space. Otherwise it accepts all other characters. So I use it in conjunction with my own function }

 function  CheckURLStartMsg         (CONST URL: string): Boolean;                                 { Check if the URL starts with HTTP or with www }


{--------------------------------------------------------------------------------------------------
   GUID
--------------------------------------------------------------------------------------------------}
 function  CoCreateGuid(var guid: TGUID): HResult; stdcall; far external 'ole32.dll';


{--------------------------------------------------------------------------------------------------
   Internet EXPLORER
--------------------------------------------------------------------------------------------------}
 function  IE_EnableProxy(const Server: String): Boolean;
 function  IE_DisableProxy: Boolean;
 function  IE_GetProxySettings(OUT ProxyAdr, ProxyPort: string; OUT IsEnabled: boolean): Boolean;
 procedure IE_DeleteCache;
 procedure IE_EndSession;
 procedure IE_SetProxy(CONST Proxy: string);   { Change IE proxy settings globally }
 //How to abort TWebBrowser navigation progress?    http://stackoverflow.com/questions/8976933/how-to-abort-twebrowser-navigation-progress


 {--------------------------------------------------------------------------------------------------
   IS CONNECTED
--------------------------------------------------------------------------------------------------}
 function  TestProgramConnectionMsg(ShowMsgOnSuccess: Boolean= FALSE): Integer;   { The Msg suffix means: this one puts a modal box on screen. For a silent verdict call LightCore.Internet.ProgramConnect2Internet }


{--------------------------------------------------------------------------------------------------
   CREATE .URL FILES
--------------------------------------------------------------------------------------------------}
 Procedure CreateUrlOnDesktop (CONST ShortFileName, sFullURL: string);



IMPLEMENTATION

USES
   LightCore.AppData,
   LightCore.IO,
   LightCore.Internet;


 function  PathIsUrlA; external 'shlwapi' name 'PathIsURLA';
 function  PathIsUrlW; external 'shlwapi' name 'PathIsURLW';




function CheckURLStartMsg(CONST URL: string): Boolean;
begin
 Result:= CheckURLStart(URL);

 if NOT Result
 then MessageWarning('Invalid URL:'+ CRLFw+ URL);
end;






{ Shows a message based on the connection test result.
  The Msg suffix is the library convention for "this routine puts a modal box on screen", so it must never be called from a thread, a service or a batch.
  ProgramConnect2Internet returns the same verdict silently. }
function TestProgramConnectionMsg(ShowMsgOnSuccess: Boolean= FALSE): Integer;
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
   .URL
==================================================================================================}


Procedure CreateUrlOnDesktop(CONST ShortFileName, sFullURL: string);                               { create a URL file - The filename should end in .URL }
VAR sDesktop: string;
    MyReg   : TRegIniFile;
begin
 { Get desktop folder }
 MyReg:= TRegIniFile.Create('Software\MicroSoft\Windows\CurrentVersion\Explorer');
 TRY
   sDesktop:= MyReg.ReadString('Shell Folders','Desktop','');
 FINALLY
   FreeAndNil(MyReg);
 END;

 { Write URL file }
 CreateUrl(Trail(sDesktop)+ ShortFileName, sFullURL);
end;





















{--------------------------------------------------------------------------------------------------
                                  Internet EXPLORER
---------------------------------------------------------------------------------------------------

Sets the proxy settings in Internet Explorer without having to restart IE to load them.
This is not the best solution, but it works.

How to use it:
   IE_EnableProxy('proxyserver:8080') sets one global proxy.
   IE_EnableProxy('ftp=ftpproxyserver:2121;gopher=goproxyserver:3333;http=httpproxyserver:8080;https=httpsproxyserver:8080') sets one proxy per protocol.
   Only one protocol is allowed too: IE_EnableProxy('http=httpproxyserver:8080').

Source:
       http://www.delphi3000.com/articles/article_3138.asp

Better way:
       http://www.naddalim.com/forum/showthread.php?t=1454        }


{ Notifies the system that Internet settings have changed.
  Call this after modifying proxy settings in the registry. }
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
 except                                                                                            { On some systems this key cannot be opened and an exception is raised, hence the try..except }
  //todo 1: trap only specific exceptions
  Result:= FALSE;
 END;

 { InternetSetOption(NIL, INTERNET_OPTION_SETTINGS_CHANGED, NIL, 0);           <--  this is how it was originally, but it seems... }
 if Result then IE_ApplySettings;                                                                  { ...that IE_ApplySettings is better }
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
 except                                                                                            { On some systems this key cannot be opened and an exception is raised, hence the try..except }
  //todo 1: trap only specific exceptions
  Result:= FALSE;
 END;

 { InternetSetOption(NIL, INTERNET_OPTION_SETTINGS_CHANGED, NIL, 0);            <-- this is how it was originally, but it seems... }
 if Result then IE_ApplySettings;                                                                  { ...that IE_ApplySettings is better }
end;





{ READ IE PROXY SETTINGS
  Easy and fast, but if Microsoft ever moves the registry key where the proxy is stored, this code must change too.
  A more complicated way is a solution based on the WinInet library.
  Source: http://www.scalabium.com/faq/dct0161.htm }

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
 except                                                                                            { On some systems this key cannot be opened and an exception is raised, hence the try..except }
  //todo 1: trap only specific exceptions
  ProxyAdr:= 'Cannot auto-detect proxy settings.';
  Result:= FALSE;
 END;
end;




{ Sets the proxy for the current session using UrlMon API.
  This only affects the current process, not system-wide IE settings. }
procedure SetProxy(CONST ProxyIP: string);
VAR
   IP: AnsiString;
   PIInfo: PInternetProxyInfo;
begin
 IP:= AnsiString(Trim(ProxyIP));
 New(PIInfo);
 TRY
   PIInfo^.dwAccessType:= INTERNET_OPEN_TYPE_PROXY;
   PIInfo^.lpszProxy:= PAnsiChar(IP);
   PIInfo^.lpszProxyBypass:= PAnsiChar('');
   Winapi.UrlMon.UrlMkSetSessionOption(INTERNET_OPTION_PROXY, piinfo, SizeOf(Internet_Proxy_Info), 0);
 FINALLY
   Dispose(PIInfo);
 END;
end;


{ Deletes all entries from Internet Explorer's URL cache.
  This affects the shared WinInet cache used by IE and other applications. }
procedure IE_DeleteCache;
var
  lpEntryInfo: PInternetCacheEntryInfo;
  hCacheDir: THandle;   { FindFirstUrlCacheEntry returns THandle - 64-bit on Win64. A LongWord truncated the handle that is passed back to FindNext/FindCloseUrlCache. }
  dwEntrySize: LongWord;
begin
  dwEntrySize := 0;
  FindFirstUrlCacheEntry(nil, TInternetCacheEntryInfo(nil^), dwEntrySize);
  GetMem(lpEntryInfo, dwEntrySize);
  TRY
    if dwEntrySize > 0
    then lpEntryInfo^.dwStructSize := dwEntrySize;
    hCacheDir := FindFirstUrlCacheEntry(nil, lpEntryInfo^, dwEntrySize);
    if hCacheDir <> 0 then
      TRY
        REPEAT
          DeleteUrlCacheEntry(lpEntryInfo^.lpszSourceUrlName);
          FreeMem(lpEntryInfo, dwEntrySize);
          lpEntryInfo:= NIL;
          dwEntrySize := 0;
          FindNextUrlCacheEntry(hCacheDir, TInternetCacheEntryInfo(nil^), dwEntrySize);
          GetMem(lpEntryInfo, dwEntrySize);
          if dwEntrySize > 0 then lpEntryInfo^.dwStructSize := dwEntrySize;
        UNTIL NOT FindNextUrlCacheEntry(hCacheDir, lpEntryInfo^, dwEntrySize);
      FINALLY
        FindCloseUrlCache(hCacheDir);
      END;
  FINALLY
    if lpEntryInfo <> NIL
    then FreeMem(lpEntryInfo, dwEntrySize);
  END;
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
 Similar WinInet resources:
   http://www.delphipages.com/threads/thread.cfm?ID=100717&G=100706
   http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_23867246.html#a22860727
   http://www.naddalim.com/forum/showthread.php?t=1454

 IdHTTP proxy:
   http://delphi.newswhat.com/geoxml/forumhistorythread?groupname=borland.public.delphi.non-technical&messageid=3f43efbc$1@newsgroups.borland.com
}





{--------------------------------------------------------------------------------------------------
   GetMacAddress
   WARNING: This function does NOT reliably return the actual MAC address!
   It uses CoCreateGuid which includes partial MAC address data only on older systems.
   Modern Windows versions randomize GUID generation for privacy.
   For reliable MAC address retrieval, use GetAdaptersInfo from Iphlpapi.dll instead.
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
