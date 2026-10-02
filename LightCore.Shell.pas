UNIT LightCore.Shell;

{=============================================================================================================
   2026.09.30
   www.GabrielMoraru.com

--------------------------------------------------------------------------------------------------------------

   Shell utilities:
     * Taskbar tools
     * The program associated with a file type
     * Tell whether a DLL exports a function

   GetAssociatedApp, ShowTaskBar, IsTaskbarAutoHideOn and AddFile2TaskbarMRU have a Windows body only, so far. A function among them is declared only on Windows, so a call written in an Android, macOS or iOS build fails to compile instead of getting a silent wrong answer. A procedure among them is declared on every platform and does nothing off Windows.
   IsApiFunctionAvailable is declared on every platform: off Windows, the LoadLibrary and GetProcAddress it calls come from System.SysUtils, which implements them with dlopen and dlsym.

   See also:
     LightCore.Win.Shell.pas      - file associations, shortcuts (.lnk), the context menu, the 'Show Desktop' file, the uninstaller entry
     LightVcl.Common.Shell.pas    - the file properties dialog, the Start menu, INF files, ExtractIconFromFile
=============================================================================================================}

INTERFACE


{==================================================================================================
   FILE ASSOCIATION
==================================================================================================}
{$IFDEF MSWINDOWS}
 function  GetAssociatedApp    (const FileExtension: string): string;                               // Old name: AplicatieAsociata
{$ENDIF}


{--------------------------------------------------------------------------------------------------
   TASKBAR
--------------------------------------------------------------------------------------------------}
 procedure ShowTaskBar(ShowIt: Boolean);
 procedure AddFile2TaskbarMRU(FileName: string);                                                    { Add the file to 'recent open files' menu that appears when right clicking on program's button in TaskBar }
{$IFDEF MSWINDOWS}
 function  IsTaskbarAutoHideOn : Boolean;
{$ENDIF}


{--------------------------------------------------------------------------------------------------
   SHELL API
--------------------------------------------------------------------------------------------------}
 function  IsApiFunctionAvailable(const DLLname, FuncName: string; VAR p: pointer): Boolean;        { Returns True if FuncName exists in DLLname }



IMPLEMENTATION

USES
   {$IFDEF MSWINDOWS}
   Winapi.Windows, Winapi.ShlObj, Winapi.ShellAPI,
   System.Win.Registry,
   {$ENDIF}
   System.SysUtils;



{$IFDEF MSWINDOWS}

{ Returns the application (exe) associated with the specified file extension.
  Also see: 
     http://delphi.about.com/od/adptips2006/qt/appextension.htm 
     https://stackoverflow.com/questions/3048188/shellexecute-not-working-from-ide-but-works-otherwise  }
function GetAssociatedApp(const FileExtension: string) : string;
VAR
    s: string;
    Reg: TRegistry;
begin
 Result:= '';
 Reg:= TRegistry.Create(KEY_READ);
 TRY
   Reg.RootKey := HKEY_CLASSES_ROOT;
   if Reg.OpenKey('.' + FileExtension + '\shell\open\command',  False)
   then
     begin                                                                                           {The open command has been found}
       s:= Reg.ReadString('');
       Reg.CloseKey;
     end
   else
     if Reg.OpenKey('.' + FileExtension, False) then
      begin                                                                                          { Perhaps there is a system file pointer }
       s:= Reg.ReadString('');
       Reg.CloseKey;
       if s <> '' then
        begin                                                                                        {A system file pointer was found}
          if Reg.OpenKey(s + '\shell\open\command', False)
          then s := Reg.ReadString('');                                                               {The open command has been found}
          Reg.CloseKey;
        end;
      end;
 FINALLY
   FreeAndNil(Reg);
 END;

 {Delete any command line, quotes and spaces}
 if Pos('%', s) > 0                            then Delete(s, Pos('%', s), length(s));
 if ((length(s) > 0) AND (s[1] = '"'))         then Delete(s, 1, 1);
 if ((length(s) > 0) AND (s[length(s)] = '"')) then Delete(s, Length(s), 1);

 WHILE ((length(s) > 0)
   AND ((s[length(s)] = #32) OR (s[length(s)] = '"')))
   DO Delete(s, Length(s), 1);
 Result := s;
end;



{--------------------------------------------------------------------------------------------------
   SHELL TASKBAR
--------------------------------------------------------------------------------------------------}
{ Invoke TaskBar from code }
procedure ShowTaskBar(ShowIt: Boolean);
begin
 if ShowIt
 then ShowWindow(FindWindow('Shell_TrayWnd',nil), SW_SHOW)                                          { Instead of SW_SHOW could try also SW_SHOWNA }
 else ShowWindow(FindWindow('Shell_TrayWnd',nil), SW_HIDE)                                          { Hide the TaskBar}
end;


{ Is Windows Taskbar's auto hide feature is enabled? } { uses Winapi.ShellAPI }
function IsTaskbarAutoHideOn : Boolean;
VAR ABData : TAppBarData;
begin
   ABData.cbSize := sizeof(ABData);
   Result := (SHAppBarMessage(ABM_GETSTATE, ABData) AND ABS_AUTOHIDE) > 0;
end;




{--------------------------------------------------------------------------------------------------
   SHELL
--------------------------------------------------------------------------------------------------}

{ Adds a file to the 'Recent' jump list that appears when right-clicking the program's taskbar button.
  See: https://stackoverflow.com/questions/5806731 }
procedure AddFile2TaskbarMRU(FileName: string);
begin
 Assert(FileName <> '', 'FileName cannot be empty!');
 SHAddToRecentDocs(SHARD_PATH, PChar(FileName));
end;


{$ELSE}

{ Stubs for the platforms that are not Windows. Each of the 2 procedures is declared on every platform, so the call compiles everywhere; here it does nothing. }
procedure ShowTaskBar(ShowIt: Boolean);                  begin end;
procedure AddFile2TaskbarMRU(FileName: string);          begin end;
{$ENDIF}



{--------------------------------------------------------------------------------------------------
   API
--------------------------------------------------------------------------------------------------}
{ Returns True if FuncName exists in DLLname.
  Note: The DLL remains loaded - use FreeLibrary if you need to unload it. }
function IsApiFunctionAvailable(const DLLname, FuncName: string; VAR p: pointer): Boolean;
VAR
  Lib: THandle;
begin
  Result:= FALSE;
  p:= NIL;

  Lib:= LoadLibrary(PChar(DLLname));
  if Lib = 0 then EXIT;

  p:= GetProcAddress(Lib, PChar(FuncName));
  Result:= (p <> NIL);

  { Note: We don't call FreeLibrary here because the caller may want to use
    the function pointer. The DLL will be unloaded when the process ends. }
end;



end.
