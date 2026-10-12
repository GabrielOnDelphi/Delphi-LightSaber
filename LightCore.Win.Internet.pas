UNIT LightCore.Win.Internet;

{$IFNDEF MSWINDOWS}
  {$MESSAGE FATAL 'LightCore.Win.Internet is Windows-only. Its package LightCore.Win builds for Win32 and Win64 only.'}
{$ENDIF}

{=============================================================================================================
   2026.10.11
   www.GabrielMoraru.com

--------------------------------------------------------------------------------------------------------------

   Internet Explorer (WinInet) settings:
     * Enable, disable and read the IE proxy (IE_EnableProxy, IE_DisableProxy, IE_GetProxySettings)
     * Delete the WinInet URL cache (IE_DeleteCache)
     * End the browser session (IE_EndSession)
     * Change the proxy of the current session (IE_SetProxy)

   See also:
     LightCore.Internet.pas          - URL tools, ParseURL, GetLocalIP, ProgramConnect2Internet, IsPortOpened
     LightVcl.Internet.Common.pas    - CheckURLStartMsg, TestProgramConnectionMsg, CreateUrlOnDesktop

   Windows-only unit, package LightCore.Win. Every routine works on the Internet Explorer proxy key of the Windows registry (HKCU\Software\Microsoft\Windows\CurrentVersion\Internet Settings), on the WinInet URL cache or on the WinInet browser session. None of these exists off Windows, so a build for Android, macOS or iOS stops at the top of this unit with a fatal compiler message.
=============================================================================================================}

INTERFACE


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



IMPLEMENTATION

USES
   Winapi.Windows,
   Winapi.UrlMon,
   Winapi.WinInet,  { Required by IE_ApplySettings }
   System.SysUtils, System.Win.Registry,
   LightCore,
   LightCore.AppData;



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


end.
