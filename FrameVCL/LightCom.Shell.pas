UNIT LightCom.Shell;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------

   Shell utilities:
     * Associate a program with a file type (for example when the user double click a jpg file, your program (image viewer) will start and show that image.
     * Add commands to the context menu (right click menu).
     * Taskbar tools
     * Start menu tools
     * Create desktop shortcuts (when your app strats for the first time it can create a shortcut on the desktop so use can easily find it)

   In this group:
     * LightCom.Shell.pas
     * csSystem.pas
     * csWindow.pas
     * LightCom.WindowMetrics.pas
     * LightCom.ExecuteProc.pas
     * LightCom.ExecuteShell.pas
     * csProcess.pas

=============================================================================================================}

INTERFACE  
USES
   Winapi.Windows, Winapi.ShlObj, Winapi.ShellAPI, Winapi.Messages, Winapi.ActiveX,
   System.Win.Registry, System.Win.ComObj, System.Classes, System.SysUtils,
   Vcl.Forms;
 

{==================================================================================================
   FILE ASSOCIATION
==================================================================================================}
 function  GetAssociatedApp    (const FileExtension: string): string;                               // Old name: AplicatieAsociata
 function  AssociateWith       (CONST FileExtension, AsociationName: string; CONST ForAllUsers: Boolean= FALSE; ShowError: Boolean= FALSE; Notify: Boolean= TRUE): Boolean; { Associate a application with an extension. EXAMPLE: FileExtension:= '.txt' /  AsociationName:= 'Metapad' }  { EXAMPLE: FileExtension:= '.txt' /  AsociationName:= 'Metapad' / ShowError show an error messajge is the program cannot write to registry }
 function  AssociationReset    (CONST FileExtension: string; CONST ForAllUsers: Boolean): Boolean;
 procedure AssociateSelf_ShellMenu  (CONST ShowError: Boolean);                                     //Old name: AssociateWithShell


{--------------------------------------------------------------------------------------------------
   CONTEXT MENU
-------------------------------------------------------------------------------------------------}
 function  AddContextMenu   (CONST CommandName, Extensions: string): Boolean;                       overload;
 procedure AddContextMenu   (CONST GUID: TGUID; CONST ShellExtDll, FileExt, UtilityName: string);   overload;
 procedure RemoveContextMenu(CONST GUID: TGUID; CONST FileExt, UtilityName: string);
 procedure InvokePropertiesDialog(CONST FileName: string);                                          { Shows the standard file properties dialog like in Windows Explorer }


{--------------------------------------------------------------------------------------------------
   START MENU
--------------------------------------------------------------------------------------------------}
 procedure InvokeStartMenu(Form: TForm);                                                            { Activate Windows Start button from code  }


{--------------------------------------------------------------------------------------------------
   TASKBAR
--------------------------------------------------------------------------------------------------}
 procedure ShowTaskBar(ShowIt: Boolean);
 procedure AddFile2TaskbarMRU(FileName: string);                                                    { Add the file to 'recent open files' menu that appears when right clicking on program's button in TaskBar }
 function  IsTaskbarAutoHideOn : Boolean;

{--------------------------------------------------------------------------------------------------
   DESKTOP: SHORTCUTS
--------------------------------------------------------------------------------------------------}
 procedure CreateShortcut          (CONST ShortCutName: string; OnDesktop: Boolean);
 procedure CreateShortcutEx        (CONST ShortCutName, ShortcutTo: string; OnDesktop: Boolean);    { Full parameters }
 procedure CreateShortcut_SendTo   (CONST ShortcutName : string);                                   { Add your application in the "Send To" menu and processing the file      http://delphi.about.com/od/adptips2006/qt/app2sendtomenu.htm }
 function  DeleteDesktopShortcut   (CONST ShortcutName: string): Boolean;
 function  DeleteStartMenuShortcut (CONST ShortcutName: string): Boolean;
 function  ExtractPathFromLnkFile  (CONST LnkFile: WideString): string;


{--------------------------------------------------------------------------------------------------
   DESKTOP: SCF
--------------------------------------------------------------------------------------------------}
 function  RestoreOriginalSCF_Association: Boolean;                                                 { Make 'Show Desktop' icon in Quick Launch to work again (to show desktop) }
 function  RemoveShowDesktopFile: Boolean;
 procedure RestoreShowDesktopFile;

 function  InstallINF    (CONST PathName: string; hParent: HWND): Boolean;                          { Example: InstallINF('C:\driver.inf', 0) }
 procedure AddUninstaller(CONST UninstallerExePath, ProductName: string);                           { Uninstaller= Full path to the EXE file that represents the uninstaller; ProductName= the uninstaller will be listed with this name. Keep it simple without special chars like '\'. Example: 'BioniX Wallpaper' }


{--------------------------------------------------------------------------------------------------
   SHELL API
--------------------------------------------------------------------------------------------------}
 function  IsApiFunctionAvailable(const DLLname, FuncName: string; VAR p: pointer): Boolean;        { Returns True if FuncName exists in DLLname }

 function  ExtractIconFromFile(IcoFileName: String): THandle;                                       { Extract icon from file  }


 
IMPLEMENTATION
USES
   LightCom.IO, ccIO, ccTextFile, ccAppData, LightCom.AppData, ccRegistry, ccCore, LightCom.Dialogs;

 



{--------------------------------------------------------------------------------------------------
                              SHORTCUTS (LNK)
--------------------------------------------------------------------------------------------------}
procedure CreateShortcutEx(CONST ShortcutName, ShortcutTo: string; OnDesktop: Boolean);            { OnDesktop:   1=Desktop  0=StartMenu }
var
  MyObject  : IUnknown;
  MySLink   : IShellLink;
  MyPFile   : IPersistFile;
  Directory : String;
  WFileName : WideString;
begin
 { if not (CoInitialize (nil) = S_OK) then Mesaj('Failed to initialize the COM layer!')
 IT IS NECESSARY ONLY WHEN I PRESS THIS FUNCTION BEFORE Application.Initialize
 Because Application.Initialize automatically calls CoInitialize.   http://coding.derkeiler.com/Archive/Delphi/borland.public.delphi.database.ado/2005-03/0064.html
 Also see: http://www.google.ro/search?num=100&q=delphi+%22CoInitialize+has+not+been+called }
 MyObject := CreateComObject(CLSID_ShellLink);
 MySLink  := MyObject as IShellLink;
 MyPFile  := MyObject as IPersistFile;

 WITH MySLink DO
  begin
   SetArguments('');                                                                               {exemplu de argument 'C:\AUTOEXEC.BAT'}
   SetPath(PChar(ShortcutTo));
   SetWorkingDirectory(PChar(AppData.ExeFolder));                                                          { SetWorkingDirectory(PChar(AppData.ExeFolder)) facea probleme foarte urata cand aplicatia ruleaza de pe un mediu ReadOnly. in 2013 am probat pe Win7 si nu face probleme!    Error message: Write Protect Error - The disk cannot be written to because it is write protected. Please remove the write protection from the volume USB_BOOT in drive D:. Cancel TryAgain Continue }
  end;

 if OnDesktop
 then Directory:= GetDesktopFolder
 else
  begin
   Directory:= GetStartMenuFolder;
   CreateDir(Directory);                                                                           {shortcut on start menu}
  end;

 { CREATE FILE }
 WFileName := Directory+ ShortCutName+ '.lnk';                                                     {numele shortcut-ului}
 MyPFile.Save(PWChar(WFileName), False);
end;


procedure CreateShortcut(CONST ShortCutName: string; OnDesktop: Boolean);                          { OnDesktop:   1=Desktop  0=StartMenu }
begin
 CreateShortcutEx(ShortCutName, Application.ExeName, OnDesktop);
end;


function DeleteDesktopShortcut(CONST ShortcutName: string): Boolean;                               { Delete a desktop icon. The user has to provide only the Name. The extension (lnk) is automatically added. }
begin
 Result:= DeleteFile(GetDesktopFolder+ ShortCutName+ '.lnk');
 {TODO: send a refresh signal to the desktop otherwise the icon is still drawn there }
end;


function DeleteStartMenuShortcut(CONST ShortcutName: string): Boolean;
var
 Directory : String;
 MyReg     : TRegIniFile;
begin
 MyReg:= TRegIniFile.Create('Software\MicroSoft\Windows\CurrentVersion\Explorer');
 Directory:= MyReg.ReadString('Shell Folders','Start Menu','');
 Result:= DeleteFile(Directory+ '\'+ ShortCutName+ '.lnk');                                        { Delete Start menu icon }
 FreeAndNil(MyReg);
end;



{--------------------------------------------------------------------------------------------------
  Add your application in the "Send To" menu of a file and load that file.

  Usage:
    1. On the FirstRun, call CreateShortcutIn(CSIDL_SENDTO, App_Name2) to add self to the 'SendTo' menu
    2. In TForm1.OnCreate do this:  if ParamCount > 0 then Mesaj(ParamStr(1))

  From:
    http://delphi.about.com/od/adptips2006/qt/app2sendtomenu.htm
--------------------------------------------------------------------------------------------------}
procedure CreateShortcut_SendTo(CONST ShortcutName : string);
CONST
   SentToIDL= CSIDL_SENDTO;
VAR
   IObject : IUnknown;
   ISLink  : IShellLink;
   IPFile  : IPersistFile;
   PIDL    : PItemIDList;
   InFolder  : array[0..MAX_PATH] of Char;
   TargetName: String;
   LinkName  : WideString;
begin
 TargetName := ParamStr(0) ;

 IObject := CreateComObject(CLSID_ShellLink) ;
 ISLink  := IObject as IShellLink;
 IPFile  := IObject as IPersistFile;

 with ISLink DO
  begin
   SetPath(pChar(TargetName)) ;
   SetWorkingDirectory(pChar(ExtractFilePath(TargetName))) ;
  end;

 { Get the location of the "special folder" }
 SHGetSpecialFolderLocation(0, SentToIDL, PIDL) ;
 SHGetPathFromIDList(PIDL, InFolder) ;

 LinkName := Format('%s\%s.lnk',[InFolder, shortcutName]) ;

 IPFile.Save(PWChar(LinkName), False) ;
end;



function ExtractPathFromLnkFile(CONST LnkFile: WideString): String;
VAR ShellLink: IShellLink;
    Path: array[0..MAX_PATH] of Char;  //ok
begin
 Result := '';
 ShellLink := CreateComObject(CLSID_ShellLink) as IShellLink;
 if (ShellLink as IPersistFile).Load(PWideChar(LnkFile), STGM_READ) = 0
 then
   if ShellLink.GetPath(Path, MAX_PATH, TWin32FindData(nil^), SLGP_SHORTPATH) = 0
   then Result := Path;
end;



 

{-------------------------------------------------------------------------------------------------------------
   ASSOCIATE APP WITH A FILE TYPE

   ALSO SEE THIS:
       ms-help://embarcadero.rs_xe7/codeexamples/JumpListTest_(Delphi).html
-------------------------------------------------------------------------------------------------------------}

{ Let Windows Explorer know we added our file type }
procedure AssociationChanged;
begin
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;


 
{ If Notify= true then notify Windows
  Use dot in file extension. Ex: '.txt' }
function AssociateWith;
VAR
   FName: string;
   RootKey: HKEY;
CONST
   Path= '\Software\Classes\';
begin
 Result:= FALSE;

 Assert(FileExtension.Length > 0, 'FileExtension is empty!');
 Assert(FileExtension[1] = '.', 'FileExtension should start with dot!');
 Assert(Pos(FileExtension, '*')<= 0, 'Invalid file extension in AssociateWith!');

 if ForAllUsers                      { On Windows 7 and up, you need admin rights to register a file type for all the machine users }
 then RootKey:= HKEY_CLASSES_ROOT
 else RootKey:= HKEY_CURRENT_USER;

 FName:= System.Copy(FileExtension+ '_file', 2, MaxInt);

 TRY
   Result:=
     RegWriteString(RootKey, Path+ FileExtension, '', FName, TRUE) AND
     RegWriteString(RootKey, Path+ FName, '', AsociationName,  TRUE) AND
     RegWriteString(RootKey, Path+ FName+'\shell', '', 'open', TRUE) AND
     RegWriteString(RootKey, Path+ FName+'\shell\open\command', '', Application.ExeName+ ' "%L"', TRUE) AND  { daca pun %L in loc de %1 imi ia calea in format lung in loc de calea in format 8.3/DOS }
     RegWriteString(RootKey, Path+ FName+'\DefaultIcon'       , '', Application.ExeName+ ',0',    TRUE);

   if NOT Result AND ShowError
   then MessageWarning('Cannot associate application with '+FileExtension);
 EXCEPT
   on E: ERegistryException DO
     begin
       if ShowError    //todo: trap only specific exceptions
       then MessageWarning('Cannot associate application with '+FileExtension);
     end;
   else RAISE;
 END;

 if Result AND Notify
 then AssociationChanged;
end;


{ Note: use dot in the file extension: '.txt'  }
function AssociationReset(CONST FileExtension: string; CONST ForAllUsers: Boolean): Boolean;
begin
 Assert(FileExtension.Length > 0, 'FileExtension is empty!');
 Assert(FileExtension[1] = '.', 'FileExtension should start with dot!');

 if ForAllUsers
 then Result:= RegDeleteKey(HKEY_CLASSES_ROOT, FileExtension)
 else Result:= RegDeleteKey(HKEY_CURRENT_USER, '\Software\Classes\'+ FileExtension);

 if Result
 then AssociationChanged
end;


{ Add current application in the 'Open with' section of the 'File Properties' popup menu that appears when we right click a file in Explorer }
procedure AssociateSelf_ShellMenu(CONST ShowError: Boolean);
begin
 TRY
  VAR Reg:= TRegistry.Create;
  TRY
    Reg.RootKey:= HKEY_CLASSES_ROOT;
    Reg.OpenKey('*\Shell\Open with '+AppData.AppName +'\Command', TRUE);
    Reg.WriteString('', Lowercase(ParamStr(0)) + ' %1');
    Reg.CloseKey;
  FINALLY
    FreeAndNil(Reg);
  end;
 EXCEPT
   on E: ERegistryException DO
     begin
       if ShowError    //todo: trap only specific exceptions
       then MessageError('Cannot associate application!');
     end;
   else RAISE;
 END;
end;


{ Add current application in the 'properties' menu of this filetype.
  Example: AddContextMenu2('Open with '+ AppName, '.nfo') }
function AddContextMenu(CONST CommandName, Extensions: string): Boolean;
VAR
  extns: TStringList;
  Reg: TRegistry;
  name: string;
  command: string;
begin
  Result:= FALSE;
  Reg   := TRegistry.Create;
  extns := TStringList.Create;
  TRY
    Reg.RootKey := HKEY_CLASSES_ROOT;
    extns.CommaText := Extensions;
    command := '"' + Application.ExeName + '" "%1"';                                   { Build the command string we want to store }

    TRY
      // Loop over extensions we can handle
      for var Extension in extns do
        // See if this extension is already known in HKCR
        if Reg.OpenKeyReadOnly ('\' + Extension) then
          begin
            name := Reg.ReadString ('');   // Get the name of this type
            if name <> '' then
              begin
                // If not blank, open this type's shell key, but don't create it
                if Reg.OpenKey ('\' + name + '\shell', False)
                then Reg.Access := KEY_READ or KEY_WRITE;                              { Try to create a new key called command_name }

                if Reg.OpenKey (CommandName, TRUE) then
                 begin
                   Reg.WriteString ('', '&' + CommandName);                            { The default value will be displayed in the context menu }
                   Reg.Access := KEY_READ or KEY_WRITE;
                   if Reg.OpenKey ('command', TRUE) then
                    begin
                      Reg.WriteString ('', command);                                   { Write the command string as the default value }
                      Result:= TRUE;
                    end;
                 end;
              end;
          end;

      EXCEPT
        on E: ERegistryException DO Result:= FALSE;
        else RAISE;
      END;
  FINALLY
    FreeAndNil(extns);
    FreeAndNil(Reg);
  END;
end;


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
      begin                                                                                          {perhaps thier is a system file pointer}
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
   SHELL EXTENSION
--------------------------------------------------------------------------------------------------}
{ Add shell extension to registry }
procedure AddContextMenu(CONST GUID: TGUID; CONST ShellExtDll, FileExt, UtilityName: string);
VAR
    Key: string;
    Rg: TRegistry;
    BufGUID: array [0..255] of WideChar;
    FileType: string;
begin
 Rg:= TRegistry.Create;
 with Rg DO
 TRY
   RootKey:= HKEY_CLASSES_ROOT;
   StringFromGUID2(GUID, BufGUID, SizeOf(BufGUID));

   if NOT OpenKey(FileExt, False) then EXIT;

   if FileExt <> '*' then
   begin
     FileType := ReadString('');
     CloseKey;
     OpenKey(FileType, False);
   end;

   Key := Format('shellex\ContextMenuHandlers\%s', [UtilityName]);
   OpenKey(Key, TRUE);
   WriteString('', BufGUID);
   CloseKey;

   Key := Format('CLSID\%s', [BufGUID]);
   OpenKey(Key, True);
   WriteString('', UtilityName);
   OpenKey('InprocServer32', True);
   WriteString('', ShellExtDll);
   WriteString('ThreadingModel', 'Apartment');
   CloseKey;
 FINALLY
   FreeAndNil(Rg);
 END;
end;


{ Remove shell extension from the context menu }
procedure RemoveContextMenu(CONST GUID: TGUID; CONST FileExt, UtilityName: string);
VAR Key: string;
    BufGUID: array [0..255] of WideChar;
    FileType: string;
begin
  with TRegistry.Create do
  TRY
    RootKey := HKEY_CLASSES_ROOT;
    StringFromGUID2(GUID, BufGUID, SizeOf(BufGUID));
    FileType := FileExt;
    if FileType <> '*' then
    begin
      if not OpenKey(FileExt, False) then Exit;
      FileType := ReadString('');
      CloseKey;
    end;

    Key := Format('%s\shellex\ContextMenuHandlers\%s', [FileType,UtilityName]);
    DeleteKey(Key);
    Key := Format('CLSID\%s', [BufGUID]);
    DeleteKey(Key);
  FINALLY
    Free;
  end;
end;


(*
procedure DoContextMenuVerb(AFolder: TShellFolder; Verb: PChar);       { executes a command }
var
  ICI: TCMInvokeCommandInfo;
  CM: IContextMenu;
  PIDL: PItemIDList;
begin
  if AFolder = nil then Exit;
  FillChar(ICI, SizeOf(ICI), #0);
  with ICI do begin
    cbSize := SizeOf(ICI);
    fMask := CMIC_MASK_ASYNCOK;
    hWND := 0;
    lpVerb := Verb;
    nShow := SW_SHOWNORMAL;
    end; {with ICI..}
  PIDL := AFolder.RelativeID;
  AFolder.ParentShellFolder.GetUIObjectOf(0, 1, PIDL, IID_IContextMenu, nil, CM);
  CM.InvokeCommand(ICI);
end; *)





{--------------------------------------------------------------------------------------------------
   SHELL INVOKE
--------------------------------------------------------------------------------------------------}

{ Invoke the standard file properties dialog like in Windows Explorer }
procedure InvokePropertiesDialog(CONST FileName: string);
VAR sei: TShellExecuteInfo;
begin
  FillChar(sei, SizeOf(sei), 0);
  sei.cbSize := SizeOf(sei);
  sei.lpFile := PChar(FileName);
  sei.lpVerb := 'properties';
  sei.fMask  := SEE_MASK_INVOKEIDLIST;
  ShellExecuteEx(@sei);
end;


{ Invoke Windows Start button from code }
procedure InvokeStartMenu(Form: TForm);
begin
 SendMessage(Form.Handle, WM_SYSCOMMAND, SC_TASKLIST, 0) ;
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

{ Add the file to 'recent open files' menu taht appears when right clicking on program's button in TaskBar }       { From here : http://stackoverflow.com/questions/5806731/how-do-i-add-recent-items-to-my-programs-jump-list-on-the-windows-7-taskbar }
procedure AddFile2TaskbarMRU(FileName: string);
begin
 SHAddToRecentDocs(SHARD_PATH, PChar(FileName));
end;



function GetDesktopListView: HWnd;
begin
  Result := GetWindow(GetWindow(FindWindow('Progman', nil), GW_CHILD), GW_CHILD);
end;







{--------------------------------------------------------------------------------------------------
   SCF 'SHOW DESKTOP' icon
--------------------------------------------------------------------------------------------------}
function RestoreOriginalSCF_Association: Boolean;                                                  { Make 'Show Desktop' icon in Quick Launch to work again (to show desktop) }
begin
 Result:= RegWriteString(HKEY_CLASSES_ROOT, '.scf', '', 'SHCmdFile', TRUE);
 AssociationChanged;
 { Registry path:
    [HKEY_CLASSES_ROOT\.scf]
    @="SHCmdFile"                                                              }
end;


function RemoveShowDesktopFile: Boolean;
VAR ShowDesktopFilePath: string;
begin
 ShowDesktopFilePath:=
      GetSpecialFolder(CSIDL_APPDATA)
      +'Microsoft\Internet Explorer\Quick Launch\Show Desktop.scf';
 Result:= FALSE;
 if FileExists(ShowDesktopFilePath)
 then Result:= DeleteFile(ShowDesktopFilePath);
end;


procedure RestoreShowDesktopFile;
VAR
   ShowDesktopFilePath: string;
CONST
   ShowDesktopContent=
       '[Shell]'                + CRLFw+
       'Command=2'              + CRLFw+
       'IconFile=explorer.exe,3'+ CRLFw+
       '[Taskbar]'              + CRLFw+
       'Command=ToggleDesktop';
begin
 ShowDesktopFilePath:= GetSpecialFolder(CSIDL_APPDATA)+'Microsoft\Internet Explorer\Quick Launch\Show Desktop.scf';
 StringToFileA(ShowDesktopFilePath, ShowDesktopContent, woOverwrite);
end;




{ MINIMIZE ALL
  Does not work:

1
procedure ShowDesktop(CONST YesNo : Boolean);
  setwindowlong ptr(getwindow(self.Handle,GW_OWNER),GWL_STYLE,0);
  setwindowlong ptr(getwindow(self.Handle,GW_OWNER),GWL_EXSTYLE,0);

2
procedure ShowDesktop(CONST YesNo : Boolean);
VAR h : THandle;
  h := FindWindow('ProgMan', nil) ;
  h := GetWindow(h, GW_CHILD) ;
  ShowWindow(h, SW_SHOW);
  ShowWindow(h, SW_HIDE);                                                      }


function InstallINF(CONST PathName: string; hParent: HWND): Boolean;                                { Example: InstallINF('C:\driver.inf', 0); }   { uses Winapi.ShellAPI }
VAR instance: HINST;
begin
 instance := ShellExecute(hParent, PChar('open'), PChar('rundll32.exe'), PChar('setupapi,InstallHinfSection DefaultInstall 132 ' + PathName), NIL, SW_HIDE);
 Result := instance > 32;
end;


{ UninstallerExePath= Full path to the uninstaller EXE file.
  ProductName= the uninstaller will be listed with this name. Keep it simple without special chars like '\'. }
procedure AddUninstaller(CONST UninstallerExePath, ProductName: string);
begin
 RegWriteString(HKEY_CURRENT_USER, 'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\'+ ProductName, 'DisplayName', ProductName);
 RegWriteString(HKEY_CURRENT_USER, 'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\'+ ProductName, 'UninstallString', UninstallerExePath);
end;






{--------------------------------------------------------------------------------------------------
   API / ICON
--------------------------------------------------------------------------------------------------}
function IsApiFunctionAvailable(const DLLname, funcname: string; VAR p: pointer): Boolean;               { return True if _funcname exists in _dllname }
var Lib: tHandle;
begin
  Result:= False;
  if LoadLibrary(PChar(DLLname)) = 0
  then exit;
  Lib:= GetModuleHandle(PChar(DLLname)) ;
  if Lib <> 0 then
   begin
      p:= GetProcAddress(Lib, PChar(funcname));
      if p <> NIL
      then Result:= true;
   end;
end;


{ Extract icon from file
  HOW TO USE IT:
     VAR MyIcon: TIcon; IF IcoHandle > 0 then begin MyIcon:= TIcon.Create;
         MyIcon.Handle:= ExtractIconFromFile; BitBtn1.Glyph.Height:= MyIcon.Height; BitBtn1.Glyph.Width := MyIcon.Width; BitBtn1.Glyph.Canvas.Draw(2, 2, MyIcon); Image1.Picture.Icon := MyIcon; Application.Icon := MyIcon;  FreeAndNil(MyIcon) );  end;  }
function ExtractIconFromFile(IcoFileName: String): THandle;
BEGIN
 Result:= ExtractIcon(Application.Handle, PChar(IcoFilename), Word(0));         // the last parameter is the index of the icon
END;



end.
