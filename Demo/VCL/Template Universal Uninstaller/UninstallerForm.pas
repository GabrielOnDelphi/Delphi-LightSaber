UNIT UninstallerForm;

{--------------------------------------------------------------------------------------------------
  GabrielMoraru.com
  2026.09.25
  Universal Uninstaller

  How to use it:
     Copy this folder for your product and set the four constants at the top of the INTERFACE section.
     Ship the uninstaller as <install folder>\System\Uninstall.exe.
     The application needs no code: TAppData.CreateMainForm calls TAppData.RegisterUninstaller at every start (LightVcl.Visual.AppData.pas).
     The DNA Baser v6 uninstaller (c:\Projects\Biology\Baser\Uninstaller v6\) is the copy this design was proven on, 2026.09.22-23.

---------------------------------------------------------------------------------------------------
  How the uninstaller works:
   1. The exe the user starts (StartCopy) copies itself into its own subfolder of the Temp folder, starts that copy with the install folder as parameter, and ends. It never creates AppData.
   2. The copy does the work. PrepareOwnFolder puts 'portable.marker' and an empty INI next to it, so:
        - AppData keeps the settings of the uninstaller in that folder, never in the settings folder of the product that it removes;
        - RunningFirstTime is FALSE.
      TAppData.CreateMainForm calls RegisterUninstaller at every start, but it writes nothing here: the Temp copy has no System\Uninstall.exe beside it.
      At the end the copy removes that Temp subfolder with everything in it: AppData.SelfDelete(TRUE).

  Why the copy:
    The uninstaller lives in the install folder, and that folder cannot be deleted while an exe inside it runs.

  Why its own subfolder:
    SelfDelete(TRUE) removes the folder of the exe with 'rd /s /q', which bypasses the Recycle Bin. Until 2026.09.25 this template copied the exe straight into the Temp folder, so that call would have erased the user's whole Temp folder.

  Where the install folder comes from:
    The folder above the System folder of the exe the user started. The registry value HKCU\Software\CubicDesign\<UninstalledAppName>, 'Install path', is only shown as a hint: the product writes it at every start, so on a development PC it names the SOURCE folder.

  What it removes:
    - the install folder (to the Recycle Bin)
    - the settings folder %AppData%\<UninstalledAppName>\ (to the Recycle Bin)
    - every file association whose 'open' command runs an exe inside the install folder
    - the Windows uninstall entry and HKCU\Software\CubicDesign\<UninstalledAppName>, unless they belong to another installation
    - the desktop and Start menu shortcuts named <UninstalledAppName>, only when they start a program inside the install folder (to the Recycle Bin)
  It never touches the licence, in HKCU\SOFTWARE\Microsoft\Windows\CurrentVersion\Defrag64
--------------------------------------------------------------------------------------------------}

{ http://stackoverflow.com/questions/4014268/how-do-i-delete-a-folder-that-another-process-has-open }

INTERFACE

USES
  Winapi.Windows, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Controls,
  Vcl.ExtCtrls, System.Classes, System.SysUtils, Vcl.Forms, System.Win.Registry,
  LightVcl.Visual.PathEdit, LightVcl.Visual.CountDown, LightVcl.Visual.RichLog,
  InternetLabel, Vcl.Imaging.pngimage, LightVcl.Visual.AppDataForm;

CONST
  { Set these four when you copy this template for a product }
  UninstalledAppName     = 'Light Template Full';          { AppData.AppName of the product this program removes. It names the product's settings folder, its Windows uninstall entry, HKCU\Software\CubicDesign\<AppName> and its desktop and Start menu shortcuts }
  UninstalledDisplayName = 'Light Template Full';          { The name of the product as the user reads it }
  UninstalledClassName   = '';                             { AppData.SingleInstClassName of the product, to see whether it still runs. Empty = the product is not a single-instance program, so this program cannot see it running and does not wait for it }
  UninstallerAppName     = 'Light Template Uninstaller';   { The AppName of this uninstaller. Its own, so its settings never go into the settings folder of the product. It also names the Temp subfolder of the copy, so it must not be empty }

TYPE
  TfrmMain = class(TLightForm)
    imgLogo      : TImage;
    lblVersion   : TLabel;
    Panel1       : TPanel;
    mmoLog       : TRichLog;
    edtPath      : TlightPathEdit;
    btnUninstall : TButton;
    btnResetIni  : TButton;
    btnFeedback  : TButton;
    btn1         : TButton;
    lblDiscount  : TLabel;
    inetDiscount : TInternetLabel;
    CountDown    : TLightCountDown;
    procedure btnUninstallClick (Sender: TObject);
    procedure btnFeedbackClick  (Sender: TObject);
    procedure FormCreate        (Sender: TObject);
    procedure imgLogoClick      (Sender: TObject);
    procedure btn1Click         (Sender: TObject);
    procedure edtPathPathChanged(Sender: TObject);
    procedure btnResetIniClick  (Sender: TObject);
    procedure CountDownTimesUp  (Sender: TObject);
  private
    FeedbackSent: Boolean;
    function  RecycleFolder(CONST Folder: string): Boolean;
    function  RecycleShortcut(CONST Folder, ShortcutName, InstallFolder: string): Boolean;
    function  CheckInstallFolder(CONST Folder: string): string;
    function  WaitUntilProductCloses: Boolean;
    procedure RemoveAssociations  (CONST InstallFolder: string);
    procedure RemoveAssociationsIn(Root: HKEY; View: LongWord; CONST InstallFolder: string);
    procedure RemoveAssociation   (Root: HKEY; View: LongWord; CONST Ext, ProgId: string);
    procedure RemoveRegistryKeys  (CONST InstallFolder: string);
    procedure DeleteKeyLogged     (Reg: TRegistry; CONST Key: string; Announce: Boolean = FALSE);
  public
    procedure FormPostInitialize; override;
 end;


VAR
  frmMain: TfrmMain;

function  StartCopy: Boolean;
procedure PrepareOwnFolder;


IMPLEMENTATION {$R *.dfm}

USES
   Winapi.ShlObj, Winapi.ActiveX, System.IOUtils,
   LightCore, LightCore.IO, LightCore.TextFile, LightCore.AppData,
   LightVcl.Common.IO, LightVcl.Common.Dialogs, LightVcl.Common.Window, LightVcl.Common.ExecuteShell, LightVcl.Common.Shell, LightVcl.Visual.AppData;

CONST
  UninstallerExe    = 'Uninstall.exe';                  { In <install folder>\System\. TAppData.RegisterUninstaller expects this name and this folder (LightVcl.Visual.AppData.pas) }
  ParamUninstall    = 'uninstall';                      { First parameter of the copy. ParamStr(2) is the install folder }
  PortableMarker    = 'portable.marker';                { Read by TAppDataCore.Create: settings next to the exe }
  ClassesKey        = 'Software\Classes\';
  UninstallKey      = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\';
  CubicDesignKey    = 'Software\CubicDesign\';          { Written by TAppData.RegisterUninstaller (LightVcl.Visual.AppData.pas) }
  ShowDesktopProgId = 'SHCmdFile';                      { The Windows file type of .scf ('Show Desktop'). A product can take .scf over: see RestoreOriginalSCF_Association in LightVcl.Common.Shell.pas }




{--------------------------------------------------------------------------------------------------
   START
--------------------------------------------------------------------------------------------------}

{ The Temp subfolder the copy runs from. StartCopy creates it; SelfDelete(TRUE) removes it at the end.
  Raises on an empty UninstallerAppName: the result would then be the Temp folder itself, and SelfDelete(TRUE) would erase it }
function CopyFolder: string;
begin
 if Trim(UninstallerAppName) = ''
 then RAISE Exception.Create('UninstallerForm: UninstallerAppName is empty. Set it at the top of UninstallerForm.pas.');
 Result:= Trail(TPath.GetTempPath)+ Trail(UninstallerAppName);
end;


{ Runs in the exe the user started, before AppData exists.
  Returns TRUE when it started the copy: the DPR must then end.
  Returns FALSE when this process must do the work itself: it is the copy, or a build run from the IDE. }
function StartCopy: Boolean;
VAR Folder, SelfCopy, InstallFolder: string;
begin
 if TAppData.RunningHome OR (ParamCount > 0)
 then EXIT(FALSE);

 Folder       := CopyFolder;
 SelfCopy     := Folder+ ExtractFileName(ParamStr(0));
 InstallFolder:= ExtractFilePath(ExcludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))));        { The exe lives in <install folder>\System\ }

 if NOT ForceDirectoriesB(Folder)
 OR NOT LightCore.IO.CopyFile(ParamStr(0), SelfCopy)
 then MessageError('Cannot create a temporary copy of the uninstaller in '+ Folder+ CRLF+ 'Most probable cause: your antivirus is blocking it.')
 else
   if NOT ExecuteFileEx(SelfCopy, ParamUninstall+ ' "'+ ExcludeTrailingPathDelimiter(InstallFolder)+ '"')
   then MessageError('Cannot start the temporary copy of the uninstaller: '+ SelfCopy);

 Result:= TRUE;
end;


{ Runs in the copy, before AppData exists. See the header: PortableMarker keeps the settings of the uninstaller next to its exe, and the INI makes RunningFirstTime FALSE, so no first-run code runs in a throwaway copy }
procedure PrepareOwnFolder;
VAR Folder: string;
begin
 Folder:= TAppData.AppFolder;
 if NOT FileExists(Folder+ PortableMarker)
 then StringToFile(Folder+ PortableMarker, '');
 if NOT FileExists(Folder+ UninstallerAppName+ '.ini')
 then StringToFile(Folder+ UninstallerAppName+ '.ini', '');
end;




{--------------------------------------------------------------------------------------------------
   HELPERS
--------------------------------------------------------------------------------------------------}

{ Returns '' when the value does not exist or is not a string }
function ReadStringValue(Reg: TRegistry; CONST Name: string): string;
begin
 if Reg.GetDataType(Name) in [rdString, rdExpandString]
 then Result:= Reg.ReadString(Name)
 else Result:= '';
end;


{ TRUE when the command line (or the path) starts inside Folder. AssociateWith writes the exe path without quotes; other programs quote it }
function CommandRunsFrom(Command: string; CONST Folder: string): Boolean;
begin
 Command:= Trim(Command);
 if (Command <> '') AND (Command[1] = '"')
 then Delete(Command, 1, 1);
 Result:= (Folder <> '') AND SameText(Copy(Command, 1, Length(Trail(Folder))), Trail(Folder));
end;


{ The path of the exe at the start of a command line, without quotes and arguments }
function ExeOfCommand(Command: string): string;
VAR i: Integer;
begin
 Command:= Trim(Command);
 if (Command <> '') AND (Command[1] = '"')
 then
  begin
   Delete(Command, 1, 1);
   i:= Pos('"', Command);
   if i > 0
   then Command:= Copy(Command, 1, i- 1);
  end;
 Result:= Command;
end;


{ The program a .lnk starts, or '' when it cannot be read.
  SLGP_RAWPATH returns the path as it is stored in the shortcut: the plain call lets Windows "repair" a broken shortcut and answer with a different program. }
function ShortcutTarget(CONST LnkPath: string): string;
VAR
   Link       : IShellLink;
   PersistFile: IPersistFile;
   Buffer     : array[0..MAX_PATH] of Char;
   FindData   : TWin32FindData;
   ComResult  : HResult;
begin
 Result:= '';

 ComResult:= CoInitialize(NIL);
 if (ComResult <> S_OK) AND (ComResult <> S_FALSE)
 then EXIT;                                                                                        { RPC_E_CHANGED_MODE: COM is already up in another apartment. Leave it alone }
 TRY
   if FAILED(CoCreateInstance(CLSID_ShellLink, NIL, CLSCTX_INPROC_SERVER, IShellLink, Link))
   OR NOT Supports(Link, IPersistFile, PersistFile)
   OR FAILED(PersistFile.Load(PChar(LnkPath), STGM_READ))
   then EXIT;

   if SUCCEEDED(Link.GetPath(Buffer, Length(Buffer), FindData, SLGP_RAWPATH))
   then Result:= Buffer;
 FINALLY
   { Both interfaces must be released BEFORE CoUninitialize closes the apartment. Without these two lines Delphi releases them when the routine ends, which is after CoUninitialize, and _Release then calls into a COM apartment that no longer exists }
   PersistFile:= NIL;
   Link:= NIL;
   CoUninitialize;
 END;
end;


function RootName(Root: HKEY): string;
begin
 if Root = HKEY_LOCAL_MACHINE
 then Result:= 'HKLM\'
 else Result:= 'HKCU\';
end;




{--------------------------------------------------------------------------------------------------
   FORM
--------------------------------------------------------------------------------------------------}

procedure TfrmMain.FormCreate(Sender: TObject);
VAR InstallFolder, Registered: string;
begin
 { A product sets AppData.ProductUninstal (the "why did you uninstall" page) and AppData.ProductSupport (the support e-mail) here. The DNA Baser v6 uninstaller shows how }
 lblVersion.Caption:= 'Uninstaller version: '+ TAppData.GetVersionInfo;
 /// inetDiscount.Link:= BxConstants.wwwUninstallDiscount; { Give user a 50% discount so he won't uninstall. }
 mmoLog.AddEmptyRow;

 { The folder to delete comes ONLY from the exe the user started: StartCopy passes the folder above its own System folder.
   The registry value 'Install path' is never used as the target. On a development PC it holds the SOURCE folder, and that folder may hold System\Uninstall.exe too, so it would pass every other check and the source code would go to the Recycle Bin }
 if SameText(ParamStr(1), ParamUninstall)
 then InstallFolder:= Trail(ParamStr(2))
 else InstallFolder:= '';

 Registered:= AppData.ReadInstallationFolder(UninstalledAppName);

 if InstallFolder = '' then
  begin
   edtPath.Path:= '';
   btnUninstall.Enabled:= FALSE;
   mmoLog.AddWarn('Started without an installation folder, so this program will delete nothing.');
   mmoLog.AddInfo('Start it where '+ UninstalledDisplayName+ ' installs it: <installation folder>\System\'+ UninstallerExe);
   if Registered <> ''
   then mmoLog.AddHint('The registry says '+ UninstalledDisplayName+ ' was started from: '+ Registered+ ' - this program will not delete a folder it was not started from.');
   EXIT;
  end;

 if (Registered <> '') AND NOT SameFolder(Registered, InstallFolder)
 then mmoLog.AddHint(UninstalledDisplayName+ ' was first started from another folder: '+ Registered);

 edtPath.Path:= InstallFolder;
 btnUninstall.Enabled:= FALSE;   { Enabled by CountDown }

 if NOT DirectoryExists(InstallFolder)
 then mmoLog.AddWarn('The installation folder does not exist. Probably moved or deleted manually. Folder: '+ InstallFolder);
 CountDown.Start;
end;


{ The caption belongs here and not in FormCreate: TAppData.CreateMainForm overwrites the caption of the main form with the application name and the version right AFTER FormCreate has run (the MainFormCaption('') call in LightVcl.Visual.AppData.pas). FormPostInitialize runs later, so what it writes is what the user sees. Moved here 2026.09.25, as in the DNA Baser v6 uninstaller }
procedure TfrmMain.FormPostInitialize;
begin
 inherited FormPostInitialize;
 Caption:= UninstalledDisplayName+ ' uninstaller - This program is provided "as is"';
end;




{--------------------------------------------------------------------------------------------------
   UNINSTALL
--------------------------------------------------------------------------------------------------}

procedure TfrmMain.btnUninstallClick(Sender: TObject);
VAR
   InstallFolder, SettingsFolder, Problem, LogFile: string;
begin
 InstallFolder := Trail(edtPath.Path);
 SettingsFolder:= Trail(TPath.Combine(TPath.GetHomePath, UninstalledAppName));    { The same formula as TAppDataCore.AppDataFolder }

 mmoLog.Clear;
 mmoLog.AddInfo('Uninstall process started...');

 { # Check the folder }
 Problem:= CheckInstallFolder(InstallFolder);
 if Problem <> '' then
  begin
   mmoLog.AddError('Cannot uninstall from '+ InstallFolder);
   mmoLog.AddError(Problem);
   mmoLog.AddInfo('Uninstall process aborted.');
   MessageError('Cannot uninstall from '+ InstallFolder+ CRLF+ Problem);
   EXIT;
  end;

 { # Give user a 50% discount so he won't uninstall }
 lblDiscount.Visible:= TRUE;
 inetDiscount.Visible:= TRUE;
 Refresh;
 Sleep(3000);

 if NOT WaitUntilProductCloses then
  begin
   mmoLog.AddInfo('Uninstall process aborted by user.');
   EXIT;
  end;

 { # Confirmation }
 if MesajYesNo('Do you approve the deletion of the following items?'
           +CRLF+ '  '+ InstallFolder
           +CRLF+ '  '+ SettingsFolder
           +CRLF+ '  The file associations of '+ UninstalledDisplayName
           +CRLF+ '  Desktop icon'
           +CRLF+ '  Start menu icon')                                     { Always test for mrYes, never for mrNo: http://capnbry.net/blog/?p=46 }
 then mmoLog.AddInfo('The user approved the deletion.')
 else
  begin
   mmoLog.AddInfo('Uninstall process aborted by user.');
   EXIT;
  end;

 { # Folders }
 if NOT RecycleFolder(InstallFolder) then
  begin
   MessageInfo(InstallFolder+ CRLF+ 'This folder is locked by a 3rd party application. Please try to delete it manually.');
   ExecuteExplorer(InstallFolder);
  end;

 if DirectoryExists(SettingsFolder)
 then RecycleFolder(SettingsFolder);

 { # Registry }
 RemoveAssociations(InstallFolder);
 RemoveRegistryKeys(InstallFolder);

 { # Shortcuts }
 { Not DeleteDesktopShortcut/DeleteStartMenuShortcut (LightVcl.Common.Shell): both call DeleteFile, which deletes for good, and they do not look at which program the shortcut starts. Everything this program removes must be recoverable from the Recycle Bin }
 if RecycleShortcut(GetDesktopFolder, UninstalledAppName, InstallFolder)                  then mmoLog.AddInfo('Desktop icon deleted to the Recycle Bin.');
 if RecycleShortcut(GetSpecialFolder(CSIDL_STARTMENU), UninstalledAppName, InstallFolder) then mmoLog.AddInfo('Start menu icon deleted to the Recycle Bin.');

 mmoLog.AddEmptyRow;
 mmoLog.AddInfo('Uninstall done.');
 mmoLog.AddEmptyRow;

 { # Log on the desktop }
 LogFile:= GetDesktopFolder+ UninstalledDisplayName+ ' - uninstall log '+ TimeToStr_IO(Now)+ '.RTF';
 TRY
  mmoLog.SaveAsRtf(LogFile);
 EXCEPT
   on EFCreateError do MesajGeneric('The uninstaller was blocked from creating the uninstall log on your desktop! Some overzealous antivirus programs misbehave by blocking legitimate applications.')
   { This is here because I got bug reports from users saying "Cannot create file".
     However, there is ultimate evidence that some antivirus programs will stop BioniX from creating a file on desktop! }
 END;

 if NOT FeedbackSent then
  begin
   MessageInfo('Please tell us why you uninstalled the program so we can make it better. CRLF CRLF CONSTRUCTIVE feedback is always appreciated. CRLF CRLF Thank you.');
   ExecuteURL(AppData.ProductUninstal);
  end;

 edtPath.Path:= '';

 { TRUE = remove the folder of this exe too, but only when that folder is the Temp subfolder of the copy (see StartCopy). A build started from the IDE runs from its source folder, and SelfDelete(TRUE) there would delete the source code, because 'rd /s /q' bypasses the Recycle Bin. Added 2026.09.25 }
 AppData.SelfDelete(SameFolder(TAppData.AppFolder, CopyFolder));
end;


{ Returns '' when Folder can be deleted as an installation of the product, else the reason why not }
function TfrmMain.CheckInstallFolder(CONST Folder: string): string;
CONST
   SourceMasks: array[0..4] of string = ('*.dpr', '*.dproj', '*.dpk', '*.pas', '*.dfm');
   DevFolders : array[0..2] of string = ('.git', '__history', '__recovery');    { __history and __recovery are written by the Delphi IDE, so they mark a source folder even when its root holds no source file }
VAR
   SourceFiles: TStringList;
   Mask, Sub: string;
begin
 if (Folder = '') OR NOT DirectoryExists(Folder)
 then EXIT('The folder does not exist.');

 if SameFolder(Folder, ExtractFileDrive(Folder))
 then EXIT('This is the root of a drive.');

 if FolderIsSpecial(Folder)
 then EXIT('This is a Windows special folder.');

 if NOT FileExists(Trail(Folder)+ 'System\'+ UninstallerExe)
 then EXIT('This folder does not hold an installation of '+ UninstalledDisplayName+ ': System\'+ UninstallerExe+ ' is missing.');

 { A development folder is never an installation. The source folder of a product can hold System\Uninstall.exe as well, so these two checks are what keep the source code safe when a user types or browses to it }
 for Sub in DevFolders do
  if DirectoryExists(Trail(Folder)+ Sub)
  then EXIT('This folder holds a '+ Sub+ ' folder. It is a development folder, not an installation.');

 for Mask in SourceMasks do
  begin
   SourceFiles:= ListFilesOf(Folder, Mask, FALSE, FALSE);
   TRY
     if SourceFiles.Count > 0
     then EXIT('This folder holds source code ('+ SourceFiles[0]+ '). It is a development folder, not an installation.');
   FINALLY
     FreeAndNil(SourceFiles);
   END;
  end;

 Result:= '';
end;


{ Returns FALSE when the user gives up. Returns TRUE at once when UninstalledClassName is empty: then there is no window class to look for.
  Result is set first on purpose: with an empty constant the compiler drops the loop, and an 'EXIT(TRUE)' guard at the top then gives hint H2077 on a 'Result:= TRUE' at the end }
function TfrmMain.WaitUntilProductCloses: Boolean;
begin
 Result:= TRUE;
 if UninstalledClassName <> '' then
   WHILE RestoreWindowByName(UninstalledClassName) DO
     begin
      mmoLog.AddWarn(UninstalledDisplayName+ ' is running. Please shut it down.');
      if NOT MesajYesNo(UninstalledDisplayName+ ' is still running. Please shut it down NOW in order to uninstall it properly!'
                   +CRLF+ 'Note: If the application is not responding you can use the TaskManager to kill it.'
                   +CRLF
                   +CRLF+ 'Press Yes when ready. Press No to stop the uninstall.')
      then EXIT(FALSE);
     end;
end;


{ TRUE when the shortcut was there and went to the Recycle Bin.
  The name alone is not proof that the shortcut belongs to this installation, so the program it starts must live in InstallFolder. A shortcut that points at a program which no longer exists is removed too: that is the shortcut of this installation after the folder was recycled a moment ago. }
function TfrmMain.RecycleShortcut(CONST Folder, ShortcutName, InstallFolder: string): Boolean;
VAR FullPath, Target: string;
begin
 Result:= FALSE;
 if Folder = '' then EXIT;

 FullPath:= Trail(Folder)+ ShortcutName+ '.lnk';
 if NOT FileExists(FullPath) then EXIT;

 Target:= ShortcutTarget(FullPath);
 if Target = '' then
  begin
   mmoLog.AddWarn('Shortcut kept, because this program cannot read which program it starts: '+ FullPath);
   EXIT;
  end;

 if NOT CommandRunsFrom(Target, InstallFolder) AND FileExists(Target) then
  begin
   mmoLog.AddWarn('Shortcut kept, because it starts another program ('+ Target+ '): '+ FullPath);
   EXIT;
  end;

 Result:= RecycleItem(FullPath, TRUE, FALSE);
 if NOT Result
 then mmoLog.AddWarn('Cannot delete the shortcut '+ FullPath);
end;


function TfrmMain.RecycleFolder(CONST Folder: string): Boolean;
begin
 { This creates problems with RamDisk. The folder gets locked after BioniX deletes it }
 Result:= RecycleItem(ExcludeTrailingPathDelimiter(Folder), TRUE, FALSE);
 if Result
 then mmoLog.AddInfo('Folder '+ Folder+ ' deleted to the Recycle Bin.')
 else mmoLog.AddWarn('Folder '+ Folder+ ' not deleted. Probably another application is locking the folder.');
end;




{--------------------------------------------------------------------------------------------------
   REGISTRY
--------------------------------------------------------------------------------------------------}

{ LightVcl.Common.Shell.AssociateWith writes an association as '.ext' -> '<ext>_file' -> shell\open\command = '<exe> "%L"', in HKCU, or in HKLM when the product ran as administrator.
  Both registry views are searched, so the result does not depend on whether Windows redirects these keys for a 32-bit program. }
procedure TfrmMain.RemoveAssociations(CONST InstallFolder: string);
begin
 RemoveAssociationsIn(HKEY_CURRENT_USER , KEY_WOW64_64KEY, InstallFolder);
 RemoveAssociationsIn(HKEY_CURRENT_USER , KEY_WOW64_32KEY, InstallFolder);
 RemoveAssociationsIn(HKEY_LOCAL_MACHINE, KEY_WOW64_64KEY, InstallFolder);
 RemoveAssociationsIn(HKEY_LOCAL_MACHINE, KEY_WOW64_32KEY, InstallFolder);
 SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, NIL, NIL);
end;


{ Every '.ext' key under Root\Software\Classes whose file type opens with an exe inside InstallFolder }
procedure TfrmMain.RemoveAssociationsIn(Root: HKEY; View: LongWord; CONST InstallFolder: string);
VAR
   Reg: TRegistry;
   Keys: TStringList;
   Ext, ProgId, Command: string;
begin
 Keys:= TStringList.Create;
 Reg:= TRegistry.Create(KEY_READ OR View);
 TRY
   Reg.RootKey:= Root;
   if NOT Reg.OpenKeyReadOnly(ClassesKey)
   then EXIT;
   Reg.GetKeyNames(Keys);
   Reg.CloseKey;

   for Ext in Keys do
    if (Ext <> '') AND (Ext[1] = '.')
    AND Reg.OpenKeyReadOnly(ClassesKey+ Ext) then
     begin
      ProgId:= ReadStringValue(Reg, '');
      Reg.CloseKey;

      if (ProgId <> '')
      AND Reg.OpenKeyReadOnly(ClassesKey+ ProgId+ '\shell\open\command') then
       begin
        Command:= ReadStringValue(Reg, '');
        Reg.CloseKey;
        if CommandRunsFrom(Command, InstallFolder)
        then RemoveAssociation(Root, View, Ext, ProgId);
       end;
     end;
 FINALLY
   FreeAndNil(Reg);
   FreeAndNil(Keys);
 END;
end;


{ The default value of '.ext' goes, and the key too when nothing else is left in it. The '<ext>_file' key goes whole }
procedure TfrmMain.RemoveAssociation(Root: HKEY; View: LongWord; CONST Ext, ProgId: string);
VAR
   Reg: TRegistry;
   Info: TRegKeyInfo;
   ExtKeyEmpty: Boolean;
begin
 Reg:= TRegistry.Create(KEY_READ OR KEY_WRITE OR View);
 TRY
   Reg.RootKey:= Root;
   if NOT Reg.OpenKey(ClassesKey+ Ext, FALSE) then
    begin
     mmoLog.AddWarn('Cannot change '+ RootName(Root)+ ClassesKey+ Ext+ ': '+ Reg.LastErrorMsg+ '. Run the uninstaller as administrator to remove this file association.');
     EXIT;
    end;

   if SameText(Ext, '.scf') AND (Root = HKEY_LOCAL_MACHINE)
   then Reg.WriteString('', ShowDesktopProgId)      { HKLM holds the Windows default of .scf. Without it .scf files would have no file type at all }
   else Reg.DeleteValue('');
   ExtKeyEmpty:= Reg.GetKeyInfo(Info) AND (Info.NumSubKeys = 0) AND (Info.NumValues = 0);
   Reg.CloseKey;

   if ExtKeyEmpty
   then DeleteKeyLogged(Reg, ClassesKey+ Ext);
   DeleteKeyLogged(Reg, ClassesKey+ ProgId);
   mmoLog.AddInfo('File association removed: '+ Ext+ ' ('+ RootName(Root)+ ClassesKey+ ProgId+ ')');
 FINALLY
   FreeAndNil(Reg);
 END;
end;


{ The Windows uninstall entry and the CubicDesign key go when they belong to this installation, or to one that no longer exists. Another installation of the product still needs them }
procedure TfrmMain.RemoveRegistryKeys(CONST InstallFolder: string);
VAR
   Reg: TRegistry;
   Value: string;
begin
 Reg:= TRegistry.Create(KEY_READ OR KEY_WRITE);
 TRY
   Reg.RootKey:= HKEY_CURRENT_USER;

   { Written by TAppData.RegisterUninstaller (LightVcl.Visual.AppData.pas) }
   if Reg.OpenKey(UninstallKey+ UninstalledAppName, FALSE) then
    begin
     Value:= ExeOfCommand(ReadStringValue(Reg, 'UninstallString'));
     Reg.CloseKey;
     if CommandRunsFrom(Value, InstallFolder) OR NOT FileExists(Value)
     then DeleteKeyLogged(Reg, UninstallKey+ UninstalledAppName, TRUE)
     else mmoLog.AddWarn('Windows uninstall entry kept: it belongs to another installation ('+ Value+ ')');
    end;

   if Reg.OpenKey(CubicDesignKey+ UninstalledAppName, FALSE) then
    begin
     Value:= ReadStringValue(Reg, 'Install path');
     Reg.CloseKey;
     if SameFolder(Value, InstallFolder) OR NOT DirectoryExists(Value)
     then DeleteKeyLogged(Reg, CubicDesignKey+ UninstalledAppName, TRUE)
     else mmoLog.AddWarn('Registry key HKCU\'+ CubicDesignKey+ UninstalledAppName+ ' kept: it belongs to another installation ('+ Value+ ')');
    end;
 FINALLY
   FreeAndNil(Reg);
 END;
end;


{ Announce=TRUE writes the success as an Info line, so the user reads in the log which key went. The file associations pass FALSE: RemoveAssociation already writes one Info line for each of them }
procedure TfrmMain.DeleteKeyLogged(Reg: TRegistry; CONST Key: string; Announce: Boolean = FALSE);
begin
 if NOT Reg.DeleteKey(Key)
 then mmoLog.AddWarn('Cannot remove the registry key '+ RootName(Reg.RootKey)+ Key+ ': '+ Reg.LastErrorMsg)
 else
   if Announce
   then mmoLog.AddInfo('Registry key removed: '+ RootName(Reg.RootKey)+ Key)
   else mmoLog.AddVerb('Registry key removed: '+ RootName(Reg.RootKey)+ Key);
end;




{--------------------------------------------------------------------------------------------------
   GUI
--------------------------------------------------------------------------------------------------}

procedure TfrmMain.CountDownTimesUp(Sender: TObject);
begin
 btnUninstall.Enabled:= TRUE;
end;


procedure TfrmMain.btn1Click(Sender: TObject);
begin
 //DeleteItem('c:\test1');
 edtPath.Path:= 'c:\test1';
end;


procedure TfrmMain.btnFeedbackClick(Sender: TObject);
begin
 ExecuteSendEmail(AppData.ProductSupport);
 ExecuteURL(AppData.ProductUninstal);
 FeedbackSent:= TRUE;
end;


procedure TfrmMain.btnResetIniClick(Sender: TObject);
begin
 ///ExecuteURL(BxConstants.wwwResetToFactory);  put it back
end;


procedure TfrmMain.edtPathPathChanged(Sender: TObject);
begin
 btnUninstall.Enabled:= edtPath.PathIsValid= '';
end;


procedure TfrmMain.imgLogoClick(Sender: TObject);
begin
 ExecuteURL(AppData.ProductUninstal);
end;



end.
