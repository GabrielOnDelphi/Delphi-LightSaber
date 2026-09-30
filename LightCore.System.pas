UNIT LightCore.System;

{=============================================================================================================
   2026.09.30
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Programmer's helper and System tools

   Also: computer and user names, fonts, BIOS date and identifier, display modes, print screen, mouse jiggle.
   These have a Windows body only, so far. A function among them is declared only on Windows, so a call written in an Android, macOS or iOS build fails to compile instead of getting a silent wrong answer. A procedure among them is declared on every platform and does nothing off Windows. InstallFont, BiosDate and BiosID use the Windows registry through System.Win.Registry, which this unit names only on Windows.

   See also:
     LightCore.Win.System.pas     - Windows services, the text of a Win32 error code, GetUserNameEx
     LightVcl.Common.System.pas   - the busy mouse cursor of a VCL application
=============================================================================================================}

INTERFACE

USES
   System.AnsiStrings, System.SysUtils,
   System.Classes, System.Types;


{=============================================================================================================
   DEVELOP UTILS
=============================================================================================================}
 procedure NotImplemented;
 procedure EmptyDummy;

 procedure DisposeAndNil(VAR P: Pointer);
 procedure FillZeros(VAR IntArray: TIntegerDynArray);


{=============================================================================================================
   SYSTEM
=============================================================================================================}
 function GetResourceAsString(CONST ResName: string): AnsiString;    { Extract a resource from self (exe) }
 function GetSystemLanguageName: string;
 function GetSystemLanguageNameShort: string;


{==================================================================================================
   SYSTEM FONTS
==================================================================================================}
{$IFDEF MSWINDOWS}
 function  InstallFont(CONST FontFileName: string): Boolean;
{$ENDIF}
 procedure UseUninstalledFont(CONST FontFile: string);                                             { Use a font without installing it. DON'T FORGET TO RELEASE IT when you close the program }
 procedure FreeUninstalledFont(CONST FontFile: string);


{==================================================================================================
   SYSTEM COMPUTER INFO
==================================================================================================}
{$IFDEF MSWINDOWS}
 function  GetComputerName: string;
 function  GetHostName    : string;

 function  GetLogonName   : string;
 function  GetDomainName  : String;                                                                { for home users it shows the computer name (Qosmio) }
 function  GetUserName (AllowExceptions: Boolean = False): string; // NOT TESTED
{$ENDIF}


{==================================================================================================
   SYSTEM Screen
===================================================================================================
   Also see LightVcl.Common.PowerUtils.pas: Monitor Off, ScreenSaver On, IsScreenSaverOn
==================================================================================================}
{$IFDEF MSWINDOWS}
 function  GetDisplayModes: string;                                                               { Returns the resolutions supported }
{$ENDIF}

 procedure PrintScreenActiveWnd;
 procedure PrintScreenFull;


{==================================================================================================
   MOUSE
==================================================================================================}
 procedure JiggleMouse;


{==================================================================================================
   HARDWARE BIOS
==================================================================================================}
CONST
   BiosUnknown = '????????';    { What BiosDate and BiosID return when the machine publishes no BIOS information. Exported so a caller can tell the sentinel from a real value without hard-coding the string }

{$IFDEF MSWINDOWS}
 function BiosDate: string;                                                                                                              { Never returns an empty string. Returns BiosUnknown when the BIOS date is not published }
 function BiosID  : string;                                                                                                              { Never returns an empty string. Returns BiosUnknown when the BIOS identifier is not published }
{$ENDIF}





IMPLEMENTATION

USES
   {$IFDEF MSWINDOWS}
   Winapi.Windows,                                 // GetLocaleInfo, LOCALE_USER_DEFAULT, LOCALE_SLANGUAGE, and the Windows-only routines at the end of this unit
   Winapi.Messages,                                // WM_FONTCHANGE
   Winapi.WinSock,                                 // WSAStartup, gethostname, WSACleanup
   System.Win.Registry,                            // TRegistry
   System.IOUtils,                                 // TFile
   LightCore.Keyboard,                             // SimulateKeystroke
   LightCore.IO,                                   // ExtractOnlyName
   {$ENDIF}
   System.SysConst;                                // SUnknown (used in the non-Windows GetSystemLanguageName branch)




{============================================================================================================
   UTILS
============================================================================================================}

{ Similar to FreeAndNil but it works on pointers.
  Dispose releases the memory allocated for a pointer variable allocated using System.New. }
procedure DisposeAndNil(VAR P: Pointer);
begin
 System.Dispose(p);
 p:= NIL;
end;


{ Fills all elements of a dynamic integer array with zeros.
  Note: Uses FillChar on the array data, not the array reference. }
procedure FillZeros(VAR IntArray: TIntegerDynArray);
begin
 if Length(IntArray) > 0
 then FillChar(IntArray[0], Length(IntArray) * SizeOf(Integer), 0);
end;


procedure EmptyDummy;
begin
 //Does nothing
end;


procedure NotImplemented;
begin
 RAISE Exception.Create('Not implemented yet.');
end;


{ Extract a resource from self (the executable).
  Returns the resource content as AnsiString.
  Raises exception if resource is empty.
  Raises EResNotFound if resource doesn't exist. }
function GetResourceAsString(CONST ResName: string): AnsiString;
VAR
   ResStream: TResourceStream;
begin
  ResStream:= TResourceStream.Create(HInstance, ResName, RT_RCDATA);
  TRY
    ResStream.Position:= 0;
    if ResStream.Size = 0
    then raise Exception.Create('GetResourceAsString');
    SetLength(Result, ResStream.Size);
    ResStream.ReadBuffer(Result[1], ResStream.Size);
  FINALLY
    FreeAndNil(ResStream);
  END;
end;


// Returns the language of the operating system in this format: "English (United States)".
// Uses Windows API directly for better Win11 compatibility (TLanguages can return <unknown>).
function GetSystemLanguageName: string;
{$IFDEF MSWINDOWS}
VAR
  Buffer: array[0..255] of Char;
begin
  if GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_SLANGUAGE, Buffer, Length(Buffer)) > 0
  then Result:= Buffer
  else Result:= 'English';  // Fallback if API fails
end;
{$ELSE}
var
  Languages: TLanguages;
  Locale: TLocaleID;
begin
  Locale:= TLanguages.UserDefaultLocale;
  Languages:= TLanguages.Create;
  try
    Result:= Languages.NameFromLocaleID[Locale];
    if (Result = '') OR (Result = SUnknown)
    then Result:= 'English';  // Fallback
  finally
    FreeAndNil(Languages);
  end;
end;
{$ENDIF}


// Returns the language of the operating system in this format: "English"
function GetSystemLanguageNameShort: string;
var
  FullName: string;
  P: Integer;
begin
  FullName := GetSystemLanguageName;
  P := Pos('(', FullName);
  if P > 0
  then Result := Trim(Copy(FullName, 1, P - 1))
  else Result := FullName;
end;




{$IFDEF MSWINDOWS}

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



{ Returns the current Windows logon username in UPPERCASE.
  Example: 'JOHN'
  Returns empty string on failure.
  Note: Similar to GetUserName but returns uppercase and is simpler. }
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


{ Returns the current Windows username as-is (preserving case).
  Example: 'John'
  AllowExceptions: If TRUE, raises EOS error on failure; if FALSE, returns empty string.
  Note: Similar to GetLogonName but preserves case and supports exceptions.
  See https://msdn.microsoft.com/en-us/library/cc761107.aspx }
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




{--------------------------------------------------------------------------------------------------
                                   FONTS
--------------------------------------------------------------------------------------------------}
{Resurse despre font-uri:

  How to install fonts                                     http://www.chami.com/tips/delphi/010297D.html
  Convert font attributes to a string and vise versa       http://www.chami.com/tips/delphi/112596D.html
  (Un)installing a font                                    http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_21581694.html?qid=21581694
  install font                                             http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_20906162.html?qid=20906162 }


{ Installs a font permanently into Windows.
  Copies the font file to Windows\Fonts folder and registers it in the registry.
  Requires administrator privileges on modern Windows versions.
  Returns TRUE if installation was successful. }
function InstallFont(CONST FontFileName: string): Boolean;
const
  Win9x= 'Software\Microsoft\Windows\CurrentVersion\Fonts';
  WinNT= 'SOFTWARE\Microsoft\Windows NT\CurrentVersion\Fonts';
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
   TFile.Copy(FontFileName, CopyToWin, FALSE);  { Note: Using TFile for compatibility with older code }

   { WRITE TO REGISTRY }
   RegData := TRegistry.Create;
   TRY
     RegData.RootKey  := HKEY_LOCAL_MACHINE;
     RegData.LazyWrite:= FALSE;

     Result:= RegData.KeyExists(WinNT) AND RegData.OpenKey(WinNT, FALSE);    { Try Windows NT/2000/XP and later first }
     if NOT Result
     then Result:= RegData.KeyExists(Win9x) AND RegData.OpenKey(Win9x, FALSE);    { Fallback for Windows 9x }
     if Result then
      TRY
        RegData.WriteString(ExtractOnlyName(FontFileName), ExtractFileName(FontFileName));
      except
        on E: ERegistryException do
          Result:= FALSE;
      END;

    FINALLY
      RegData.CloseKey;
      FreeAndNil(RegData);
    END;

   { NOTIFY THE SYSTEM }
   if Result then
    begin
     AddFontResource(PChar(CopyToWin));   { Register the copy in Windows\Fonts (the registry entry points there) - registering the original path would break the font for this session once the source file (USB stick, temp folder) disappears }
     SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
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




// ================================================
// Bios Information: Win2000/NT compatible
// ================================================
{ Returns BiosUnknown when the machine does not publish a BIOS date.
  ValueExists is not optional: on this UEFI machine (measured 2026-08-22) the System key exists and holds
  SystemBiosVersion, but no SystemBiosDate at all. ReadString then returns '', which used to overwrite the
  '????????' sentinel and make the function return an empty string - the one thing it was written not to do. }
function BiosDate: string;
var
  Reg: TRegistry;
begin
  Result:= BiosUnknown;

  Reg := TRegistry.Create;
  TRY
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('\HARDWARE\DESCRIPTION\System')
    AND Reg.ValueExists('SystemBiosDate')
    then Result := Reg.ReadString('SystemBiosDate');
  FINALLY
     FreeAndNil(Reg);
  END;

  if Result = ''
  then Result:= BiosUnknown;
end; //todo 5: isn't better if we return an empty string?


{ Returns BiosUnknown when the machine publishes no BIOS identifier.

  The OUTPUT FORMAT is deliberately unchanged: the Identifier value, one space, then the FIRST string of the
  REG_MULTI_SZ SystemBiosVersion. Three defects were fixed underneath it (2026-08-22):

  1. The '????????' sentinel was overwritten as soon as the key opened, whether or not anything was read.

  2. The buffer was never zeroed, and ReadBinaryData does NOT raise when the value is missing - it returns 0 and
     leaves the buffer untouched (System.Win.Registry.pas:ReadBinaryData -> GetDataInfo fails -> Result := 0).
     The old code ignored that result and concatenated the buffer as a PChar regardless, so a machine without
     SystemBiosVersion produced whatever uninitialised heap bytes happened to sit there, up to the first #0.

  3. ReadBinaryData calls ReadError (which raises) when the value is bigger than the buffer or has an unexpected
     type. That exception escaped instead of yielding the sentinel. It is now impossible rather than caught:
     the size and type are checked first, mirroring ReadBinaryData's own guard, so nothing has to be swallowed. }
function BiosID: string;  { From BlackBox.pas }
CONST
   BufferBytes = $2000;
var
  WinReg: TRegistry;
  Buffer: TBytes;
  DataSize: Integer;
  Identifier, Version: string;
begin
  Result:= BiosUnknown;
  Version:= '';

  WinReg := TRegistry.Create;
  TRY
    WinReg.RootKey := HKEY_LOCAL_MACHINE;
    if NOT WinReg.OpenKeyReadOnly('\HARDWARE\DESCRIPTION\System') then EXIT;

    Identifier:= WinReg.ReadString('Identifier');                { Returns '' when the value does not exist }

    DataSize:= WinReg.GetDataSize('SystemBiosVersion');          { -1 when the value does not exist }
    if  (DataSize > 0)
    AND (DataSize <= BufferBytes - SizeOf(Char))
    AND (WinReg.GetDataType('SystemBiosVersion') in [rdBinary, rdUnknown, rdMultiString]) then
      begin
        SetLength(Buffer, BufferBytes);
        FillChar(Buffer[0], BufferBytes, 0);                     { The zeroed tail is what guarantees a terminator }
        if WinReg.ReadBinaryData('SystemBiosVersion', Buffer[0], BufferBytes - SizeOf(Char)) > 0
        then Version:= PChar(@Buffer[0]);                        { REG_MULTI_SZ: this takes the first string, as it always did }
      end;
  FINALLY
     FreeAndNil(WinReg);
  END;

  Identifier:= Trim(Identifier + ' ' + Version);
  if Identifier <> ''
  then Result:= Identifier;
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


{$ELSE}

{ Stubs for the platforms that are not Windows. Each of the 5 procedures is declared on every platform, so the call compiles everywhere; here it does nothing. }
procedure PrintScreenActiveWnd;                          begin end;
procedure PrintScreenFull;                               begin end;
procedure UseUninstalledFont (CONST FontFile: string);   begin end;
procedure FreeUninstalledFont(CONST FontFile: string);   begin end;
procedure JiggleMouse;                                   begin end;
{$ENDIF}


end.

