UNIT LightFMX.AppData;

{=============================================================================================================
   2025.03
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   FEATURES

     TAppDataCore:
       - Get application's %appdata% folder (the folder where you save temporary, app-related and ini files)
       - Get application's command line parameters
       - Force single instance (allow only one instance of your program to run). Second inst sends its command line to the first inst then shuts down
       - Detect if the application is running for the first time on this computer
       - Application self-restart
       - Application self-delete
	   	   
     TAppDataEx	   
       - Get application's version
       - Log error messages to a special window that pops up when you send warnings and errors to it. 
       - Easily create new forms and set its font to be the same as main forms' font.
       - Change the font for all running forms
       - Basic support for Uninstaller (The Uninstaller can find out where the app was installed)
       - Basic support for licensing (trial period) system. See Proteus for details.
       - Translation
         (Multi-language GUI) 

       - etc, etc
 ____________________________________________________________________________________________________________

   HOW TO USE IT

     In the DPR file replace the code with:
       uses
         FastMM4,
         Forms,
         FormRamLog,
         cbINIFile,
         ccAppData,
         cbAppDataVCL,
         MainForm in 'MainForm.pas' {frmMain);
       begin
         AppData:= TAppData.Create('MyCollApp');
         AppData.CreateMainForm(TMainForm, MainForm, False, True);   // Main form
         AppData.CreateForm(TSecondFrom, frmSecond);                 // Secondary form (optional)
         TfrmRamLog.CreateGlobalLog;                                 // Log (optional)
         AppData.Run;
       end.

 ____________________________________________________________________________________________________________

   DOCUMENTATION

     APPDATA global var

        It is important to create the AppData object as soon as the application starts.
        Not necessary to free AppData. It frees itself.


     OnFormCreate

        See explanation in cbAppDataForm.pas


     DPR FILE
        In the DPR file these lines of code are not necessary anymore and you MUST remove them.
           Application.Title := 'x';
           Application.ShowMainForm:= True;
           MainForm.Show;


     AppData.Initializing

        When the application starts, this flag is set to True.
        Then it is automatically set to False once all the forms are is fully loaded.
        If you don't want this to happen, set the AutoSignalInitializationEnd variable to False.
        In this case, you will have to set the Initializing manually, once the program is fully initialized (usually in the LateInitialize of the main form, or in the LateInitialize of the last created form).
        If you forget it, the AppData will not save the forms to the INI file and you will have a warning on shutdown.
        Usage:
          Used by SaveForm in cbINIFile.pas/cvINIFile.pas (and a few other places) to signal not to save the form if the application has crashed while still in the initialization phase.
          You can use it also personally, to avoid executing some of your code during the initialization stages.


     MainFormOnTaskbar

        [TASKBAR]
           if TRUE = A taskbar button represents the application's main form & displays its caption.
           if FALSE= A taskbar button represents the application's (hidden) main window & displays the application's Title.

        [Modality]
            if TRUE = All child forms will stay on top of the MainForm.
                      Bad since we don't really want "modal" forms all over in our app.
                      When we do want a child form to stay on top, then we make it modal or use fsStayOnTop.

        [AERO]
            Windows Aero effects (Live taskbar thumbnails, Dynamic Windows, Windows Flip, Windows Flip 3D)
            if False = Aero effects work
            if False = Aero effects do not work

        Details
            https://stackoverflow.com/questions/66720721/
            https://stackoverflow.com/questions/14587456/how-can-i-stop-my-application-showing-on-the-taskbar

        BX: This must be FALSE in order to make 'Start minimized' option work



     Known issues
         If you are creating copies of the same form, the second, third, etc will get a dynamic name. This means that they will not be stored/loaded properly from the INI file (because of the dynamic name).

=============================================================================================================}

INTERFACE
{$I Frameworks.inc}

USES

  {$IFDEF msWindows}
    Winapi.Windows,


    System.Win.Registry,
  {$ENDIF}

  System.SysUtils, System.Classes, System.UITypes, System.Types,
  FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Types, FMX.Controls, FMX.Controls.Presentation, FMX.Platform,

  {$IFDEF ANDROID}
   Androidapi.Helpers, Androidapi.JNI.App, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText,
  {$ENDIF}

  {$IFDEF MacOS}
   Posix.Stdlib, Posix.Unistd,
  {$ENDIF}


  ccCore, ccINIFile, ccAppData;

TYPE
  TAppData= class(TAppDataCore)
  private
    FFont: TFont;

    procedure setFont(aFont: TFont);
  protected
    CopyDataID: DWORD;          // For SingleInstance. This is a unique message ID for our applications. Used when we send the command line to the first running instance via WM_COPYDATA
  public
   {--------------------------------------------------------------------------------------------------
      INIT
   --------------------------------------------------------------------------------------------------}
    constructor Create(CONST aAppName: string; CONST WindowClassName: string= ''; MultiThreaded: Boolean= FALSE); override;
    destructor Destroy; override;     // This is called automatically by "Finalization" in order to call it as late as possible }
    procedure Run;

    {$IFDEF FullAppData}
   {--------------------------------------------------------------------------------------------------
      Dialog boxes
   --------------------------------------------------------------------------------------------------}
   function PromptToSaveFile (VAR FileName: string; CONST Filter: string = ''; CONST DefaultExt: string= ''; CONST Title: string= ''): Boolean;
   function PromptToLoadFile (VAR FileName: string; CONST Filter: string = '';                               CONST Title: string= ''): Boolean;
   function PromptForFileName(VAR FileName: string; SaveDialog: Boolean; CONST Filter: string = ''; CONST DefaultExt: string= ''; CONST Title: string= ''; CONST InitialDir: string = ''): Boolean;

   {--------------------------------------------------------------------------------------------------
      App Control
   --------------------------------------------------------------------------------------------------}
    procedure Restore;
    procedure Restart;
    procedure SelfDelete;
    {$ENDIF}
    procedure Minimize; override;

    {$IFDEF MSWINDOWS}
    function  RunFileAtStartUp(CONST FilePath: string; Active: Boolean): Boolean; {$ENDIF}
    {$IFDEF MacOS}
    function RunFileAtStartup(const FilePath: string; Active: Boolean): Boolean; {$ENDIF}
    {$IFDEF ANDROID}
    function RunFileAtStartup(const FilePath: string; Active: Boolean): Boolean; {$ENDIF}
    {$IFDEF LINUX}
    function RunFileAtStartup(const FilePath: string; Active: Boolean): Boolean; {$ENDIF}
    function  RunSelfAtStartUp(Active: Boolean): Boolean;



   {--------------------------------------------------------------------------------------------------
      FORM
   --------------------------------------------------------------------------------------------------}
    procedure CreateMainForm  (aClass: TComponentClass; OUT aReference; MainFormOnTaskbar: Boolean= FALSE;                         AutoState: TAutoState= asPosOnly); overload;
    procedure CreateMainForm  (aClass: TComponentClass;                 MainFormOnTaskbar: Boolean= FALSE; Visible: Boolean= TRUE; AutoState: TAutoState= asPosOnly); overload;

    procedure CreateForm      (aClass: TComponentClass; OUT Reference;                                    Visible: Boolean= TRUE; Owner: TFmxObject = NIL; Parented: Boolean= FALSE; CreateBeforeMainForm: Boolean= FALSE);
    procedure CreateFormHidden(aClass: TComponentClass; OUT Reference;                                                            ParentWnd: TFmxObject = NIL);

    procedure CreateFormModal (aClass: TComponentClass; OUT Reference;                                                            ParentWnd: TFmxObject= NIL); overload;   // Do I need this?
    procedure CreateFormModal (aClass: TComponentClass;                                                                           ParentWnd: TFmxObject= NIL); overload;

    procedure SetMaxPriority;

   {--------------------------------------------------------------------------------------------------
      User settings
   --------------------------------------------------------------------------------------------------}
    property Font: TFont read FFont write setFont;

    procedure MainFormCaption(const Caption: string); override;
  end;

VAR                      //ToDo: make sure AppData is unique (make it Singleton)
   AppData: TAppData;    // This obj is automatically freed on app shutdown (via FINALIZATION)



IMPLEMENTATION

USES
  {$IFDEF MsWindows}
    //cbVersion,
    //cbRegistry,
    //cbDialogs,
    //cbCenterControl,
    //FormRamLog,
  {$ENDIF}
  ccIO;


{ Warning: We cannot use Application.CreateForm here because this will make the Log the main form! }
constructor TAppData.Create(CONST aAppName: string; CONST WindowClassName: string= ''; MultiThreaded: Boolean= FALSE);
begin
  inherited Create(aAppName, WindowClassName, MultiThreaded);
  Application.Initialize;                         // Note: Emba: Although Initialize is the first method called in the main project source code, it is not the first code that is executed in a GUI application. For example, in Delphi, the application first executes the initialization section of all the units used by the Application. in modern Delphi (non-.NET), you can remove Application.Initialize without breaking your program. The method is almost empty and no longer plays a critical role in setting up the VCL or application environment. Its historical purpose was to initialize COM and CORBA, but since those are no longer used, the method is effectively redundant.
  Application.Title         := aAppName;

  { All done }
  LogVerb(AppName+ ' started.');
end;


destructor TAppData.Destroy;
begin
  inherited;
end;


procedure TAppData.Run;
begin
  Initializing:= FALSE;

  // Ignore the "Show" parameter if "StartMinimized" is active
  // Note: FMX: CreateForm does not create the given form immediately. It just adds a request to the pending list. RealCreateForms creates the real forms.
  if StartMinim
  then Minimize;

  Application.Run;
end;



{-------------------------------------------------------------------------------------------------------------
   CREATE FORMS

   Note:
     On FMX, CreateForm does not create the given form immediately.
     It just adds a request to the pending list. RealCreateForms creates the real forms.
-------------------------------------------------------------------------------------------------------------}
procedure TAppData.CreateMainForm(aClass: TComponentClass; OUT aReference; MainFormOnTaskbar: Boolean= FALSE; AutoState: TAutoState= asPosOnly);
begin
  Assert(Application.MainForm = NIL, 'MainForm already exists!');  //ToDo: test if this works under FMX because of RealCreateForms
  Application.CreateForm(aClass, aReference);                       // Reference is NIL here because of RealCreateForms

  //Assert(TForm(Reference) <> NIL, ' Reference is NIL here because of RealCreateForms!');
  //TLightForm(Reference).AutoState:= AutoState;
end;


procedure TAppData.CreateMainForm(aClass: TComponentClass; MainFormOnTaskbar: Boolean= FALSE; Visible: Boolean= TRUE; AutoState: TAutoState= asPosOnly);
begin
  VAR aReference: TForm;
  CreateMainForm(aClass, aReference, MainFormOnTaskbar, AutoState);
end;


{ Create secondary form
  "Loading" indicates if the GUI settings are remembered or not }
procedure TAppData.CreateForm(aClass: TComponentClass; OUT Reference; Visible: Boolean= TRUE; Owner: TFmxObject= NIL; Parented: Boolean= FALSE; CreateBeforeMainForm: Boolean= FALSE);
begin
  if CreateBeforeMainForm
  then
    begin
      // We allow the Log form to be created before the main form.
      if Owner = NIL
      then TComponent(Reference):= aClass.Create(Application)  // Owned by Application. But we cannot use Application.CreateForm here because then, this form will be the main form!
      else TComponent(Reference):= aClass.Create(Owner);       // Owned by Owner

      //ToDo 4: For the case where the frmLog is created before the MainForm: copy the frmLog.Font from the MainForm AFTER the main MainForm created.
    end
  else
    begin
      if Application.MainForm = NIL
      then RAISE Exception.Create('Probably you forgot to create the main form with AppData.CreateMainForm!');

      if Owner = NIL
      then Application.CreateForm(aClass, Reference)        // Owned by Application
      else TComponent(Reference):= aClass.Create(Owner);    // Owned by Owner
    end;

  // Center form in the Owner
  if (Owner <> NIL)
  AND Parented then
    begin
      Assert(TForm(Reference) <> NIL, ' Reference is NIL here because of RealCreateForms!');
      TForm(Reference).Parent:= Owner;
      {$IFDEF FullAppData}
      CenterChild(TForm(Reference), Owner);
      {$ENDIF}
    end;

  if Show
  then TForm(Reference).Show;
end;



{ Create secondary form }
procedure TAppData.CreateFormHidden(aClass: TComponentClass; OUT Reference; ParentWnd: TFmxObject= NIL);
begin
  CreateForm(aClass, Reference, FALSE, ParentWnd);
end;


{ Create secondary form }
procedure TAppData.CreateFormModal(aClass: TComponentClass; ParentWnd: TFmxObject= NIL);
VAR Reference: TForm;
begin
  CreateForm(aClass, Reference, FALSE, ParentWnd);
  Reference.ShowModal;
end;


{ Create secondary form }
//ToDo: Do I need this? Since the form is modal, I should never need the Reference? To be deleted
procedure TAppData.CreateFormModal(aClass: TComponentClass; OUT Reference; ParentWnd: TFmxObject= NIL);
begin
  CreateFormModal(aClass, ParentWnd);
end;








{--------------------------------------------------------------------------------------------------
   APPLICATION / WIN START UP
--------------------------------------------------------------------------------------------------}
{
Summary

    macOS: Uses launchctl to manage startup items.
    Android: Uses broadcast receivers to trigger actions at boot.
    Linux: Uses .desktop files in the ~/.config/autostart directory to manage startup items.
}

//ToDo: Ensure that you handle permissions and platform-specific requirements properly. For example, on Android, you need to declare the appropriate permissions in the manifest file, and on Linux, ensure the .desktop file has the correct permissions.

{ Run the specified application at Windows startup }
{$IFDEF MSWINDOWS}
function TAppData.RunFileAtStartUp(CONST FilePath: string; Active: Boolean): Boolean;
VAR Reg: TRegistry;
begin
 Result:= FALSE;
 TRY
  Reg:= TRegistry.Create;
  TRY
   Reg.LazyWrite:= TRUE;
   Reg.RootKey:= HKEY_CURRENT_USER;                                                   { This is set by default by the constructor }
   if Reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Run', TRUE) then
     begin
      if Active
      then Reg.WriteString(ExtractOnlyName(FilePath),'"'+ FilePath + '"')             { I got once an error here: ERegistryexception-Failed to set data for 'App-Name2'. Probably cause by an anti-virus }
      else Reg.DeleteValue(ExtractOnlyName(FilePath));
      Reg.CloseKey;
      Result:= TRUE;
     end;
  FINALLY
    FreeAndNil(Reg);
  END;
 except
   Result:= FALSE;                                                                    { To catch possible issues caused by antivirus programs that won't let the program write to 'autorun' section }
 END;
end;
{$ENDIF}


{ Run THIS application at Windows startup }
function TAppData.RunSelfAtStartUp(Active: Boolean): Boolean;
begin
  Result:= RunFileAtStartUp(ParamStr(0), Active);
end;


{$IFDEF MACOS}
function TAppData.RunFileAtStartup(const FilePath: string; Active: Boolean): Boolean;
var
  LaunchAgentPath: string;
  LaunchAgentContent: TStringList;
begin
  Result := False;
  LaunchAgentPath := TPath.Combine(TPath.GetHomePath, 'Library/LaunchAgents/com.myapp.startup.plist');

  if Active then
  begin
    LaunchAgentContent := TStringList.Create;
    try
      LaunchAgentContent.Add('<?xml version="1.0" encoding="UTF-8"?>');
      LaunchAgentContent.Add('<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">');
      LaunchAgentContent.Add('<plist version="1.0">');
      LaunchAgentContent.Add('<dict>');
      LaunchAgentContent.Add('  <key>Label</key>');
      LaunchAgentContent.Add('  <string>com.myapp.startup</string>');
      LaunchAgentContent.Add('  <key>ProgramArguments</key>');
      LaunchAgentContent.Add('  <array>');
      LaunchAgentContent.Add('    <string>' + FilePath + '</string>');
      LaunchAgentContent.Add('  </array>');
      LaunchAgentContent.Add('  <key>RunAtLoad</key>');
      LaunchAgentContent.Add('  <true/>');
      LaunchAgentContent.Add('</dict>');
      LaunchAgentContent.Add('</plist>');
      LaunchAgentContent.SaveToFile(LaunchAgentPath);
      _system(PAnsiChar('launchctl load ' + LaunchAgentPath));
      Result := True;
    finally
      LaunchAgentContent.Free;
    end;
  end
  else
  begin
    if TFile.Exists(LaunchAgentPath) then
    begin
      _system(PAnsiChar('launchctl unload ' + LaunchAgentPath));
      TFile.Delete(LaunchAgentPath);
      Result := True;
    end;
  end;
end;
{$ENDIF}


{$IFDEF ANDROID}
function TAppData.RunFileAtStartup(const FilePath: string; Active: Boolean): Boolean;
var
  Intent: JIntent;
  Receiver: JComponentName;
begin
  Intent := TJIntent.Create;
  Receiver := TJComponentName.JavaClass.init(TAndroidHelper.Context.getPackageName, StringToJString('com.myapp.BootReceiver'));

  if Active then
  begin
    Intent.setComponent(Receiver);
    Intent.setAction(TJIntent.JavaClass.ACTION_BOOT_COMPLETED);
    TAndroidHelper.Context.sendBroadcast(Intent);
    Result := True;
  end
  else
  begin
    // Unregister the receiver (implementation depends on how you registered it)
    // This is just a placeholder for the actual implementation
    Result := True;
  end;
end;
{$ENDIF}


{$IFDEF LINUX}
uses
  System.IOUtils;

function TAppData.RunFileAtStartup(const FilePath: string; Active: Boolean): Boolean;
var
  AutostartPath: string;
  DesktopEntry: TStringList;
begin
  Result := False;
  AutostartPath := TPath.Combine(TPath.GetHomePath, '.config/autostart/com.myapp.desktop');

  if Active then
  begin
    DesktopEntry := TStringList.Create;
    try
      DesktopEntry.Add('[Desktop Entry]');
      DesktopEntry.Add('Type=Application');
      DesktopEntry.Add('Exec=' + FilePath);
      DesktopEntry.Add('Hidden=false');
      DesktopEntry.Add('NoDisplay=false');
      DesktopEntry.Add('X-GNOME-Autostart-enabled=true');
      DesktopEntry.Add('Name=MyApp');
      DesktopEntry.Add('Comment=Start MyApp at login');
      DesktopEntry.SaveToFile(AutostartPath);
      Result := True;
    finally
      DesktopEntry.Free;
    end;
  end
  else
  begin
    if TFile.Exists(AutostartPath) then
    begin
      TFile.Delete(AutostartPath);
      Result := True;
    end;
  end;
end;
{$ENDIF}










{--------------------------------------------------------------------------------------------------
   APPLICATION Control
--------------------------------------------------------------------------------------------------}
procedure TAppData.Minimize;
begin
  var WindowService: IFMXWindowService;
  if TPlatformServices.Current.SupportsPlatformService(IFMXWindowService, IInterface(WindowService)) then
    WindowService.SetWindowState(Application.MainForm, TWindowState.wsMinimized);
end;





{--------------------------------------------------------------------------------------------------
   VERSION INFO
--------------------------------------------------------------------------------------------------}
(*
{ $IFDEF MsWindows}
function TAppData.GetVersionInfoMajor: Word;
VAR FixedInfo: TVSFixedFileInfo;
begin
  if GetVersionInfoFile(ExeName, FixedInfo)
  then Result:= HiWord(FixedInfo.dwFileVersionMS)
  else Result:= 0;
end;


function TAppData.GetVersionInfoMinor: Word;
VAR FixedInfo: TVSFixedFileInfo;
begin
  if GetVersionInfoFile(ExeName, FixedInfo)
  then Result:= LoWord(FixedInfo.dwFileVersionMS)
  else Result:= 0;
end;

{ Returns version with/without build number.
  Example:
     1.0.0.999
     1.0.0
  See also: CheckWin32Version }
class function TAppData.GetVersionInfo(ShowBuildNo: Boolean= False): string;
VAR FixedInfo: TVSFixedFileInfo;
begin
  FixedInfo.dwSignature:= 0;
  if cbVersion.GetVersionInfoFile(ExeName, FixedInfo)
  then
     begin
      Result:= IntToStr(HiWord(FixedInfo.dwFileVersionMS))+'.'+ IntToStr(LoWord(FixedInfo.dwFileVersionMS))+'.'+ IntToStr(HiWord(FixedInfo.dwFileVersionLS));
      if ShowBuildNo
      then Result:= Result+ '.'+ IntToStr(LoWord(FixedInfo.dwFileVersionLS));
     end
  else Result:= 'N/A';
end;


{ Returns version without build number. Example: v1.0.0.
  Automatically adds the v in front of the number. }
class function TAppData.GetVersionInfoV: string;
begin
  Result:= ' v'+ GetVersionInfo(False);
end;

*)





{-------------------------------------------------------------------------------------------------------------
   OTHERS
-------------------------------------------------------------------------------------------------------------}
{ $IFDEF Framework_VCL}
{ Apply this font to all existing forms. }
procedure TAppData.setFont(aFont: TFont);
begin
  if FFont = NIL
  then FFont:= aFont   // We set the font for the first time.
  else
    begin
      // Note: FormCount also counts invisible forms and forms created TFrom.Create(Nil).
      FFont:= aFont;
      for VAR i:= 0 to Screen.FormCount - 1 DO    // FormCount => forms currently displayed on the screen. CustomFormCount = as FormCount but also includes the property pages
        //Screen.Forms[i].Font:= aFont;
    end;
end;


{ Set this process to maximum priority. Usefull when measuring time }
procedure TAppData.SetMaxPriority;
begin
  {$IFDEF MSWINDOWS}
  SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS); //  https://stackoverflow.com/questions/13631644/setthreadpriority-and-setpriorityclass
  {$ENDIF}
end;




{$IFDEF FullAppData}
{--------------------------------------------------------------------------------------------------
   UNINSTALLER
---------------------------------------------------------------------------------------------------
   READ/WRITE folders to registry
   This is used by the Uninstaller.
   See c:\MyProjects\Project support\Cubic Universal Uninstaller\Uninstaller.dpr
--------------------------------------------------------------------------------------------------}
CONST
   RegKey: string= 'Software\CubicDesign\';

procedure TAppData.WriteAppDataFolder;                                           { Called by the original app }
begin
 RegWriteString(HKEY_CURRENT_USER, RegKey+ AppName, 'App data path', AppDataFolder);                                                                                                                              {Old name: WriteAppGlobalData }
end;


function TAppData.ReadAppDataFolder(CONST UninstalledApp: string): string;       { Called by the uninstaller }
begin
 Result:= RegReadString(HKEY_CURRENT_USER, RegKey+ UninstalledApp, 'App data path');
end;


{------------------------
   Instalation Folder
------------------------}
procedure TAppData.WriteInstallationFolder;                                     { Called by the original app }                                                                                                                                       {Old name: WriteAppGlobalData }
begin
 RegWriteString(HKEY_CURRENT_USER, RegKey+ AppName, 'Install path', CurFolder);
end;


function TAppData.ReadInstallationFolder(CONST UninstalledApp: string): string;  { Called by the uninstaller }
begin
 Result:= RegReadString(HKEY_CURRENT_USER, RegKey+ UninstalledApp, 'Install path');
end;




{-------------------------------------------------------------------------------------------------------------
   Prompt To Save/Load File

   These functions are also duplicated in cmIO.Win.
   The difference is that there, those functions cannot read/write the LastUsedFolder var so the app cannot remmeber last use folder.

   Example: PromptToSaveFile(s, cGraphUtil.JPGFtl, 'txt');

   DefaultExt:
     Extensions longer than three characters are not supported!
     Do not include the dot (.)
-------------------------------------------------------------------------------------------------------------}
function TAppData.PromptToSaveFile(VAR FileName: string; CONST Filter: string = ''; CONST DefaultExt: string= ''; CONST Title: string= ''): Boolean;
VAR InitialDir: string;
begin
 if FileName > '' then
   if IsFolder(FileName)
   then InitialDir:= FileName
   else InitialDir:= ExtractFilePath(FileName);

 Result:= PromptForFileName(FileName, TRUE, Filter, DefaultExt, Title, InitialDir);
end;


Function TAppData.PromptToLoadFile(VAR FileName: string; CONST Filter: string = ''; CONST Title: string= ''): Boolean;
VAR InitialDir: string;
begin
 if FileName > '' then
   if IsFolder(FileName)
   then InitialDir:= FileName
   else InitialDir:= ExtractFilePath(FileName);

 Result:= PromptForFileName(FileName, FALSE, Filter, '', Title, InitialDir);
end;


{ Based on Vcl.Dialogs.PromptForFileName.
  AllowMultiSelect cannot be true, because I return a single file name (cannot return a Tstringlist)  }
Function TAppData.PromptForFileName(VAR FileName: string; SaveDialog: Boolean; CONST Filter: string = ''; CONST DefaultExt: string= ''; CONST Title: string= ''; CONST InitialDir: string = ''): Boolean;
VAR
  Dialog: TOpenDialog;
begin
  if SaveDialog
  then Dialog := TSaveDialog.Create(NIL)
  else Dialog := TOpenDialog.Create(NIL);
  TRY
    { Options }
    Dialog.Options := Dialog.Options + [ofEnableSizing, ofForceShowHidden];
    if SaveDialog
    then Dialog.Options := Dialog.Options + [ofOverwritePrompt]
    else Dialog.Options := Dialog.Options + [ofFileMustExist];

    Dialog.Title := Title;
    Dialog.DefaultExt := DefaultExt;

    if Filter = ''
    then Dialog.Filter := Vcl.Consts.sDefaultFilter
    else Dialog.Filter := Filter;

    if InitialDir= ''
    then Dialog.InitialDir:= Self.AppDataFolder  // Customization
    else Dialog.InitialDir:= InitialDir;

    Dialog.FileName := FileName;

    Result := Dialog.Execute;

    if Result then
      begin
        FileName:= Dialog.FileName;
        Self.LastUsedFolder:= ExtractFilePath(FileName);  // Customization
      end;
  FINALLY
    FreeAndNil(Dialog);
  END;
end;
{$ENDIF}






{-------------------------------------------------------------------------------------------------------------
   OTHERS
-------------------------------------------------------------------------------------------------------------}
procedure TAppData.MainFormCaption(CONST Caption: string);
begin
  inherited;
  
  {$IFDEF Framework_FMX}
   // Note: FMX: CreateForm does not create the given form immediately. It just adds a request to the pending list. RealCreateForms creates the real forms.
   if Application.MainForm = NIL then EXIT;
  {$ENDIF}
  if Caption= ''
  then Application.MainForm.Caption:= AppName+ ' '
  else Application.MainForm.Caption:= AppName+ ' '+ ' - ' + Caption;
end;



INITIALIZATION
// Hint We could create AppData here

FINALIZATION
begin
  FreeAndNil(AppData);
end;



end.
