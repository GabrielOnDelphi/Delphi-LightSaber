UNIT cbAppDataFMX;




{ WARNING! While it works for me, the FMX part of LightSaber is still in Beta stage! }








{=============================================================================================================
   Gabriel Moraru
   2025.01.19
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

   FEATURES

    Via class you can:
       - Get application's appdata folder (the folder where you save temporary, app-related and ini files)
       - Get application's command line parameters
       - Get application's version

       - Force single instance (allow only one instance of your program to run). Second inst sends its command line to the first inst then shuts down
       - Detect if the application is running for the first time on this computer

       - Application self-restart
       - Application self-delete

       - Easily create new forms and set its font to be the same as main forms' font.
       - Change the font for all running forms

       - Log error messages to a special window that is automatically created
       - Basic support for Uninstaller (The Uninstaller can find out where the app was installed)
       - Basic support for licensing (trial period) system. See Proteus for details.

       - LOG
         TAppData class also creates a global Log form.
         This form automatically pops up when you send warnings and errors to it.
         For details see: FormLog.pas

       - etc

 ____________________________________________________________________________________________________________

   HOW TO USE IT

     In the DPR file replace the code with:
       uses
         FastMM4,
         FMX.Forms,
         FormRamLog,
         //cbINIFile,
         cbAppData,
         MainForm in 'MainForm.pas' {frmMain);
       begin
         AppData:= TAppData.Create('MyCollApp');
         AppData.CreateMainForm(TMainForm, MainForm, False, True);   // Main form
         AppData.CreateForm(TSecondFrom, frmSecond);                 // Secondary form (optional)
         TfrmRamLog.CreateGlobalLog;                                 // Log (optional)
         Application.Run;
       end.

     Full demo code in LightSaber\Demo\Template App\TemplateApp.dpr
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
        Then it is automatically set to False once the main form is fully loaded.
        If you don't want this to happen, set the AutoSignalInitializationEnd variable to False.
        In this case, you will have to set the Initializing manually, once the program is fully initialized (usually in the LateInitialize of the main form, or in the LateInitialize of the last created form).
        If you forget, the AppData will not save the forms to the INI file and you will have a warning on shutdown.
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
 ____________________________________________________________________________________________________________

   Demo app:
      c:\MyProjects\Project support\Template - Empty app\StarterProject.dpr
=============================================================================================================}

INTERFACE

{$I Frameworks.inc}

USES
  System.SysUtils, System.Classes, System.IOUtils, System.UITypes, System.Types, System.Messaging,

   {$IFDEF mswindows}
    Winapi.Windows,
    Winapi.Messages,
    Winapi.ShellAPI,
    System.Win.Registry,
  {$ENDIF}
    FMX.Forms,
    FMX.Graphics,
    FMX.Dialogs, FMX.Types, FMX.Controls, FMX.Controls.Presentation,
    FMX.Platform,

  {$IFDEF ANDROID}
   Androidapi.Helpers, Androidapi.JNI.App, Androidapi.JNI.JavaTypes,
   Androidapi.JNI.GraphicsContentViewText,
  {$ENDIF}
  ccCore,
  ccINIFile,
  cbLogRam,
  cbAppDataFmxForm;

TYPE
  THintType = (htOff,                      // Turn off the embedded help system
               htTooltips,                 // Show help as tool-tips
               htStatBar);                 // Show help in status bar

  TAppData= class(TObject)
  private
    FShowOnError: Boolean;                 // Automatically show the visual log form when warnings/errors are added to the log. This way the user is informed about the problems.
    FLastFolder: string;
    FSingleInstClassName: string;          { Used by the Single Instance mechanism. } {Old name: AppWinClassName }
    FRunningFirstTime: Boolean;
    procedure SetGuiProperties(Form: TForm);
    procedure setShowOnError(const Value: Boolean);

    {$IFDEF MSWINDOWS}
    function  RunFileAtStartUp(CONST FilePath: string; Active: Boolean): Boolean; {$ENDIF}
    {$IFDEF ANDROID}
    function RunFileAtStartup(const FilePath: string; Active: Boolean): Boolean; {$ENDIF}
    {$IFDEF LINUX}
    function RunFileAtStartup(const FilePath: string; Active: Boolean): Boolean; {$ENDIF}
    CONST
      Signature: AnsiString= 'AppDataSettings';{ Do not change it! }
      DefaultHomePage= 'https://www.GabrielMoraru.com';
    class VAR FCreated: Boolean;     { Sanity check }
    class VAR FAppName: string;
    function  getLastUsedFolder: string;

    function  RunSelfAtStartUp(Active: Boolean): Boolean;
	
    procedure LoadSettings;
    procedure SaveSettings;
  protected
    CopyDataID: DWORD;          // For SingleInstance. This is a unique message ID for our applications. Used when we send the command line to the first running instance via WM_COPYDATA
  public
    RamLog: TRamLog;

    { Product details }
    CompanyName    : string;    // Optional. Used by the 'About' form
    CompanyHome    : WebURL;    // Company's home page
    ProductHome    : WebURL;    // Product's home page
    ProductOrder   : WebURL;    // Product's order page
    ProductSupport : WebURL;    // Product's user manual
    ProductWelcome : WebURL;    // Product's welcome page (gives a short introduction on how the product works).
    ProductUninstal: WebURL;    // Ask user why they uninstalled the product (feedback)

   {--------------------------------------------------------------------------------------------------
      App Single Instance
   --------------------------------------------------------------------------------------------------}
    class VAR Initializing: Boolean;                 // See documentation at the top of the file
    class VAR AutoSignalInitializationEnd: Boolean;  // See documentation at the top of the file
    procedure Minimize;

    constructor Create(CONST aAppName: string; CONST WindowClassName: string= ''; SignalInitEnd: Boolean= TRUE; MultiThreaded: Boolean= FALSE); virtual;
    destructor Destroy; override;                    // This is called automatically by "Finalization" in order to call it as late as possible }

   {--------------------------------------------------------------------------------------------------
      User settings
   --------------------------------------------------------------------------------------------------}
    var
      UserPath     : string;           // User defined path where to save (large) files. Useful when the program needs to save large amounts of data that we don't want to put on a SSD drive.
      AutoStartUp  : Boolean;          // Start app at Windows startup
      StartMinim   : Boolean;          // Start minimized
      Minimize2Tray: Boolean;          // Minimize to tray
      HintType     : THintType;        // Turn off the embedded help system
      Opacity      : Integer;          // Form opacity

   {--------------------------------------------------------------------------------------------------
      App path/name
   --------------------------------------------------------------------------------------------------}
    function CurFolder: string;        // The folder where the exe file is located. Old name: AppDir
    function SysDir: string;
    function CheckSysDir: Boolean;
    class function IniFile: string;
    class function AppDataFolder(ForceDir: Boolean= FALSE): string;
    class function AppDataFolderAllUsers(ForceDir: Boolean = FALSE): string;

    function AppShortName:  string;
    property LastUsedFolder: string read getLastUsedFolder write FLastFolder;

    class property AppName:  string read FAppName;

   {--------------------------------------------------------------------------------------------------
      Installer
   --------------------------------------------------------------------------------------------------}
    {$IFDEF MsWindows}
    procedure RegisterUninstaller;
    procedure WriteAppDataFolder;
    function  ReadAppDataFolder(CONST UninstalledApp: string): string;

    procedure WriteInstallationFolder;
    function  ReadInstallationFolder(CONST UninstalledApp: string): string;
    {$ENDIF}

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


    property  RunningFirstTime: Boolean read FRunningFirstTime;    // Returns true if the application is running for the first time on this computer.


   {--------------------------------------------------------------------------------------------------
      FORM
   --------------------------------------------------------------------------------------------------}
    procedure CreateMainForm  (aClass: TComponentClass;                MainFormOnTaskbar: Boolean= FALSE; Show: Boolean= TRUE; Loading: TAutoState= asPosOnly); overload;
    procedure CreateMainForm  (aClass: TComponentClass; OUT Reference; MainFormOnTaskbar: Boolean= FALSE; Show: Boolean= TRUE; Loading: TAutoState= asPosOnly); overload;

    procedure CreateForm      (aClass: TComponentClass; OUT Reference; Show: Boolean= TRUE; Loading: TAutoState= asPosOnly; Owner: TFmxObject = NIL; Parented: Boolean= FALSE; CreateBeforeMainForm: Boolean= FALSE);
    procedure CreateFormHidden(aClass: TComponentClass; OUT Reference; Loading: TAutoState= asPosOnly; ParentWnd: TFmxObject = NIL);

    procedure CreateFormModal (aClass: TComponentClass; OUT Reference; Loading: TAutoState= asPosOnly; ParentWnd: TFmxObject= NIL); overload;   // Do I need this?
    procedure CreateFormModal (aClass: TComponentClass;                Loading: TAutoState= asPosOnly; ParentWnd: TFmxObject= NIL); overload;

    {-------------------------------------------------------------------------------------------------
      App Version
   --------------------------------------------------------------------------------------------------}
    {$IFDEF FullAppData}
    class function GetVersionInfo(ShowBuildNo: Boolean= False): string;
    function  GetVersionInfoMajor: Word;
    function  GetVersionInfoMinor: Word;
    {$ENDIF}

    function  GetVersionInfoV      : string;               // Returns version without Build number. Example: v1.0.0

    procedure MainFormCaption(const Caption: string);


   {--------------------------------------------------------------------------------------------------
      BetaTester tools
   --------------------------------------------------------------------------------------------------}
    class function RunningHome: Boolean;
    function  BetaTesterMode: Boolean;
    function  IsHardCodedExp(Year, Month, Day: word): Boolean;

    class procedure RaiseIfStillInitializing;

   {--------------------------------------------------------------------------------------------------
      App Log
   --------------------------------------------------------------------------------------------------}
    procedure LogEmptyRow;
    procedure LogBold  (CONST Msg: string);
    procedure LogError (CONST Msg: string);
    procedure LogHint  (CONST Msg: string);
    procedure LogImpo  (CONST Msg: string);
    procedure LogInfo  (CONST Msg: string);
    procedure LogMsg   (CONST Msg: string);
    procedure LogVerb  (CONST Msg: string);
    procedure LogWarn  (CONST Msg: string);
    procedure LogClear;

    procedure PopUpLogWindow;

    property ShowLogOnError: Boolean read FShowOnError write setShowOnError;       // Automatically show the visual log form when warnings/errors are added to the log. This way the user is informed about the problems.
  end;



{-------------------------------------------------------------------------------------------------
   Command line
--------------------------------------------------------------------------------------------------}
function  CommandLinePath: string;
procedure ExtractPathFromCmdLine(MixedInput: string; OUT Path, Parameters: string);
function  FindCmdLineSwitch(const Switch: string; IgnoreCase: Boolean): Boolean; deprecated 'Use System.SysUtils.FindCmdLineSwitch';


VAR                      //ToDo: make sure AppData is unique (make it Singleton)
   AppData: TAppData;    // This obj is automatically freed on app shutdown (via FINALIZATION)

IMPLEMENTATION

USES
  {$IFDEF MsWindows}
    cbVersion,
    cbRegistry,
    //cbDialogs,
    //cbCenterControl,
    //cbINIFile,
    //FormRamLog,
  {$ENDIF}
  ccIO, ccTextFile;



function ExeName: string;
begin
  Result:= ParamStr(0);   //  Application.ExeName is available only on VCL
end;


{-------------------------------------------------------------------------------------------------------------
 Parameters

    AppName
       The AppName is the central part of this class. It is used by IniFile/MessageBox/etc.
       Should contain only I/O-safe characters (so no ? <> * % /).
       It is used to generate the INI file that will store the form position/size.

    WindowClassName - [Optional]
       Used in InstanceRunning to detect if the application is already running, by looking to a form with this Class Name.
       Needed ONLY if you use the Single Instance functionality.
       This string must be unique in the whole computer! No other app is allowed to have this ID.
       If you leave it empty, the aAppName is used. But AppName might not be that unique, or you might want to change it over time.
-------------------------------------------------------------------------------------------------------------}
constructor TAppData.Create(CONST aAppName: string; CONST WindowClassName: string= ''; SignalInitEnd: Boolean= TRUE; MultiThreaded: Boolean= FALSE);
begin
  Application.Initialize;                         // Note: Emba: Although Initialize is the first method called in the main project source code, it is not the first code that is executed in a GUI application. For example, in Delphi, the application first executes the initialization section of all the units used by the Application.

  inherited Create;
  AutoSignalInitializationEnd:= SignalInitEnd;    // See documentation at the top of the file
  Initializing:= True;                            // Used in cv_IniFile.pas. Set it to false once your app finished initializing.

  { Sanity check }
  if FCreated
  then RAISE Exception.Create('Error! AppData already constructed!')
  else FCreated:= TRUE;

  { App single instance }
  if WindowClassName = ''
  then FSingleInstClassName:= aAppName
  else FSingleInstClassName:= WindowClassName;    // We use FSingleInstClassName to identify the window/instance (when we check for an existing instance)

  { App name }
  Assert(System.IOUtils.TPath.HasValidPathChars(aAppName, FALSE), 'Invalid characters in AppName'+ aAppName);
  Assert(aAppName > '', 'AppName is empty!');
  FAppName:= aAppName;
  Application.Title:= aAppName;
  AppDataFolder(TRUE);               // Force the creation of AppData folder.

  { App first run }
  FRunningFirstTime:= NOT FileExists(IniFile);

  { Log }
  { Warning: We cannot use Application.CreateForm here because this will make the Log the main form! }
  Assert(RamLog = NIL, 'Log already created!');  // Call this as soon as possible so it can catch all Log messages generated during app start up. A good place might be in your DPR file before Application.CreateForm(TMainForm, frmMain)
  RamLog:= TRamLog.Create(ShowLogOnError, NIL, MultiThreaded);

  LogVerb(AppName+ GetVersionInfoV+ ' started.');

  { App settings }
  if FileExists(IniFile)
  then LoadSettings
  else
    begin
      { Default settings }
      AutoStartUp  := TRUE;
      StartMinim   := FALSE;
      Minimize2Tray:= TRUE;                      // Minimize to tray
      HintType     := htTooltips;                // Turn off the embeded help system

      Opacity      := 250;
      UserPath     := AppDataFolder;
    end;

  { Product details }
  CompanyName    := 'SciVance';
  CompanyHome    := DefaultHomePage;
  ProductHome    := DefaultHomePage;
  ProductOrder   := DefaultHomePage;
  ProductSupport := DefaultHomePage;
  ProductWelcome := DefaultHomePage;
  ProductUninstal:= DefaultHomePage;
end;


{ This destructor is called automatically from the "Finalization" section.

  Warning:
    At this point the AppData var is already nil because FreeAndNil (called in the initialization section), first puts the var to Nil, then calls the .Free.
    So we need to destroy the Log form here, and not in the Finalization section! }
destructor TAppData.Destroy;
begin
  SaveSettings;
  FreeAndNil(RamLog); // Call this as late as possible
  inherited;
end;


{-------------------------------------------------------------------------------------------------------------
   FORMS
-------------------------------------------------------------------------------------------------------------}
{ 1. Create the form
  2. Set the font of the new form to be the same as the font of the MainForm
  3. Show it }
procedure TAppData.CreateMainForm(aClass: TComponentClass; MainFormOnTaskbar: Boolean= FALSE; Show: Boolean= TRUE; Loading: TAutoState= asPosOnly);
begin
  VAR Reference: TForm;
  CreateMainForm(aClass, Reference, MainFormOnTaskbar, Show, Loading);
end;


procedure TAppData.CreateMainForm(aClass: TComponentClass; OUT Reference; MainFormOnTaskbar: Boolean= FALSE; Show: Boolean= TRUE; Loading: TAutoState= asPosOnly);
begin
  Assert(Application.MainForm = NIL, 'MainForm already exists!');

  // Note: FMX: CreateForm does not create the given form immediately. It just adds a request to the pending list. RealCreateForms creates the real forms.
  Application.CreateForm(aClass, Reference);

  MainFormCaption('Initializing...');

  SetGuiProperties(TForm(Reference));

  {$IFDEF FullAppData}
   if (Loading = flFull) OR (Loading = asPosOnly) then
    begin
      // Load form

      // ISSUE HERE!
      // At this point we can only load "standard" Delphi components.
      // Loading of our Light components can only be done in cv_IniFile.pas -> TIniFileVCL
      // For the moment the work around is load only the stadard components.
      cbINIFile.LoadFormBase(TForm(Reference), Loading);

      { Write path to app in registry }
      if RunningFirstTime then RegisterUninstaller;
    end;

   // if the program is off-screen, bring it on-screen
   CorrectFormPositionScreen(TForm(Reference));

  // Ignore the "Show" parameter if "StartMinimized" is active
  // Note: FMX: CreateForm does not create the given form immediately. It just adds a request to the pending list. RealCreateForms creates the real forms.
  if StartMinim
  then Minimize
  else
     if Show
     then TForm(Reference).Show;
  {$ENDIF}

  // Window fully constructed.
  // Now we can let user run its own initialization process.
  // This is the ONLY correct place where we can properly initialize the application (see "Delphi in all its glory [Part 2]" book) for details.
  if TObject(Reference) is TLightForm
  then TLightForm(Reference).FormInitialize;

  if AutoSignalInitializationEnd
  then Initializing:= FALSE;

  MainFormCaption('');
end;


{ Create secondary form }
{ Load indicates if the GUI settings are remembered or not }
procedure TAppData.CreateForm(aClass: TComponentClass; OUT Reference; Show: Boolean= TRUE; Loading: TAutoState= asPosOnly; Owner: TFmxObject= NIL; Parented: Boolean= FALSE; CreateBeforeMainForm: Boolean= FALSE);
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
      TForm(Reference).Parent:= Owner;
      {$IFDEF FullAppData}
       CenterChild(TForm(Reference), Owner);
      {$ENDIF}
    end;

  // Font, snap, alpha
  SetGuiProperties(TForm(Reference));

  // Load previous form settings/position
  {$IFDEF FullAppData}
   cbINIFile.LoadFormBase(TForm(Reference), Loading);
  {$ENDIF}
  if TForm(Reference) is TLightForm
  then TLightForm(Reference).Loading:= Loading;

  if Show
  then TForm(Reference).Show;

  // Window fully constructed.
  // Now we can let user run its own initialization process.
  if TObject(Reference) is TLightForm
  then TLightForm(Reference).FormInitialize;

  if AutoSignalInitializationEnd
  then Initializing:= FALSE;
end;



{ Create secondary form }
procedure TAppData.CreateFormHidden(aClass: TComponentClass; OUT Reference; Loading: TAutoState= asPosOnly; ParentWnd: TFmxObject= NIL);
begin
  CreateForm(aClass, Reference, FALSE, Loading, ParentWnd);
end;


{ Create secondary form }
procedure TAppData.CreateFormModal(aClass: TComponentClass; Loading: TAutoState= asPosOnly; ParentWnd: TFmxObject= NIL);
VAR Reference: TForm;
begin
  CreateForm(aClass, Reference, FALSE, Loading, ParentWnd);
  Reference.ShowModal;
end;


{ Create secondary form }
//ToDo: Do I need this? Since the form is modal, I should never need the Reference? To be deleted
procedure TAppData.CreateFormModal(aClass: TComponentClass; OUT Reference; Loading: TAutoState= asPosOnly; ParentWnd: TFmxObject= NIL);
begin
  CreateFormModal(aClass, Loading, ParentWnd);
end;


procedure TAppData.SetGuiProperties(Form: TForm);
begin
  {$IFDEF FullAppData}
   // Font
   if Form = Application.MainForm
   then Self.Font:= Form.Font   // We TAKE the font from the main form. Then we apply it to all existing and all future windows.
   else
     if Self.Font <> nil
     then Form.Font:= Self.Font;  // We set the same font for secondary forms

   // Fix issues with snap to edge of the screen
   if cbVersion.IsWindows8Up
   then Form.SnapBuffer:= 4
   else Form.SnapBuffer:= 10;

   // Form transparency
   Form.AlphaBlendValue := Opacity;
   Form.AlphaBlend:= Opacity< 255;
  {$ENDIF}
end;




{-------------------------------------------------------------------------------------------------------------
   APP PATH
-------------------------------------------------------------------------------------------------------------}

{ Returns the folder where the EXE file resides.
  The path ended with backslash. Works with UNC paths. Example: c:\Program Files\MyCoolApp\ }
function TAppData.CurFolder: string;
begin
  Result:= ExtractFilePath(ExeName);
end;


{ Returns the folder where the EXE file resides plus one extra folder called 'System'
  The path ended with backslash. Works with UNC paths. Example: c:\Program Files\MyCoolApp\System\ }
function TAppData.SysDir: string;
begin
  Result:= CurFolder+ Trail('System');
end;


{ Check if the System folder exists. If not, we should kill the program! }
function TAppData.CheckSysDir: Boolean;
begin
  Assert(AppData <> NIL, 'AppData var might not be available at this point!');
  Result:= DirectoryExists(AppData.SysDir);

  {$IFDEF FullAppData}
   if NOT Result
   then MesajError('The program was not properly installed! The "System" folder is missing.');
  {$ENDIF}
end;


{ Returns ONLY the name of the app (exe name without extension) }
function TAppData.AppShortName: string;
begin
  Result:= ExtractOnlyName(ExeName);
end;


{ Returns the last folder used when the user opened a LoadFile/SaveFile dialog box }
//ToDo: save this to a INI file
function TAppData.getLastUsedFolder: string;
begin
  if FLastFolder = ''
  then Result:= GetMyDocuments
  else Result:= FLastFolder;
end;


{ Returns the path to current user's AppData folder on Windows, and to the current user's home directory on Mac OS X.
  Cannot be used at design-time because AppName is not set!

  Windows XP       C:\Documents and Settings\UserName\Application Data
  Windows Vista+   C:\Users\UserName\AppData\Roaming
  OS X             /Users/UserName
  iOS Device       /private/var/mobile/Containers/Data/Application/Application_ID
  iOS Simulator    /Users/UserName/Library/Developer/CoreSimulator/Devices/Device_ID/data/Containers/Data/Application/Application_ID
  Android          /data/data/Application_ID/files
  Linux            /home/UserName

  If ForceDir, then it creates the folder (full path) where the INI file will be written. }
class function TAppData.AppDataFolder(ForceDir: Boolean = FALSE): string;
begin
  Assert(AppName > '', 'AppName is empty!');
  Assert(System.IOUtils.TPath.HasValidFileNameChars(AppName, FALSE), 'Invalid chars in AppName: '+ AppName);

  Result := Trail(TPath.Combine(TPath.GetHomePath, AppName));

  if ForceDir
  then ForceDirectories(Result);
end;



{ Example:
   Windows XP      C:\Documents and Settings\All Users\Application Data
   Windows Vista+  C:\ProgramData
   OS X            /Users/<username>/Public
   iOS Device      N/A
   iOS Simulator   /Users/<username>/Library/Developer/CoreSimulator/Devices/<Device ID>/data/Containers/Data/Application/<application ID>/Public
   Android         /storage/emulated/0/Android/data/<application ID>/files
}
class function TAppData.AppDataFolderAllUsers(ForceDir: Boolean = FALSE): string;
begin
  Assert(AppName > '', 'AppName is empty!');
  Assert(TPath.HasValidFileNameChars(AppName, FALSE), 'Invalid chars in AppName: '+ AppName);

  Result := Trail(TPath.Combine(TPath.GetPublicPath, AppName));

  if ForceDir
  then ForceDirectories(Result);
end;


{ Returns the name of the INI file (where we will write application's settings).
  It is based on the name of the application. Example: c:\Documents and Settings\MyName\Application Data\MyApp\MyApp.ini }
class function TAppData.IniFile: string;
begin
  Assert(AppName > '', 'AppName is empty!');
  Assert(TPath.HasValidFileNameChars(AppName, FALSE), 'Invalid chars in AppName: '+ AppName);

  Result := TPath.Combine(AppDataFolder, AppName + '.ini');
end;






{-----------------------------------------------------------------------------------------------------------------------
   APP UTILS
-----------------------------------------------------------------------------------------------------------------------}

{ Returns true if the application is "home" (in the computer where it was created). This is based on the presence of a DPR file that has the same name as the exe file. }
class function TAppData.RunningHome: Boolean;
begin
  Result:= FileExists(ChangeFileExt(ExeName, '.dpr'));
end;


{ Returns true if a file called 'betatester' exists in application's folder or in application's system folder. }
function TAppData.BetaTesterMode: Boolean;
begin
  Result:= FileExists(SysDir+ 'betatester') OR FileExists(CurFolder+ 'betatester');
end;


{ Check if today is past the specified (expiration) date.
  If a file called 'dvolume.bin' exists, then the check is overridden.
  Good for checking exiration dates. }
function TAppData.IsHardCodedExp(Year, Month, Day: word): Boolean;
VAR
   s: string;
   HardCodedDate: TDateTime;
begin
 if FileExists(CurFolder+ 'dvolume.bin')         { If file exists, ignore the date passed as parameter and use the date written in file }
 then
   begin
     s:= StringFromFile(CurFolder+ 'dvolume.bin');
     HardCodedDate:= StrToInt64Def(s, 0);
     Result:= round(HardCodedDate- Date) <= 0;     { For example: 2016.07.18 is 3678001053146ms. One day more is: 3678087627949 }
   end
 else
   begin
     HardCodedDate:= EncodeDate(Year, Month, Day);
     Result:= round(HardCodedDate- Date) <= 0;
   end;
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
{ Run THIS application at Windows startup }
function TAppData.RunSelfAtStartUp(Active: Boolean): Boolean;
begin
  Result:= RunFileAtStartUp(ParamStr(0), Active);
end;

{$IFDEF MSWINDOWS}
function TAppData.RunFileAtStartUp(CONST FilePath: string; Active: Boolean): Boolean;
VAR Reg: TRegistry;
begin
 Result:= FALSE;
 TRY
  Reg:= TRegistry.Create;
  TRY
   Reg.LazyWrite:= TRUE;
   Reg.RootKey:= HKEY_CURRENT_USER;                                                                { This is set by default by the constructor }
   if Reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Run', TRUE) then
     begin
      if Active
      then Reg.WriteString(ExtractOnlyName(FilePath),'"'+ FilePath + '"')                                { I got once an error here: ERegistryexception-Failed to set data for 'App-Name2'. Probably cause by an anti-virus }
      else Reg.DeleteValue(ExtractOnlyName(FilePath));
      Reg.CloseKey;
      Result:= TRUE;
     end;
  FINALLY
    FreeAndNil(Reg);
  END;
 except
   Result:= FALSE;                                                                                 { To catch possible issues caused by antivirus programs that won't let the program write to 'autorun' section }
 END;
end;
{$ENDIF}


{$IFDEF MACOS}
uses
  Posix.Stdlib, Posix.Unistd;

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
{$IFDEF Framework_VCL}
procedure TAppData.Restart;
begin
  VAR PAppName:= PChar(ExeName);
  Winapi.ShellAPI.ShellExecute({Handle} 0, 'open', PAppName, nil, nil, SW_SHOWNORMAL);   { Handle does not work. Replaced with 0. }
  Application.Terminate;
end;


{--------------------------------------------------------------------------------------------------
   SCREEN
--------------------------------------------------------------------------------------------------}
{ Bring the application back to screen (if minimized, in background, hidden) }
procedure TAppData.Restore;
begin
  Application.MainForm.Visible:= TRUE;
  if Application.MainForm.WindowState = wsMinimized
  then Application.MainForm.WindowState:= TWindowState.wsNormal;

  //Use Restore to restore the application to its previous size before it was minimized. When the user restores the application to normal size, Restore is automatically called.
  //Note: Don't confuse the Restore method, which restores the entire application, with restoring a form or window to its original size. To minimize, maximize, and restore a window or form, change the value of its WindowState property.
  Application.Restore;
  SetForegroundWindow(Application.MainForm.Handle);
  Application.BringToFront;
end;
{$ENDIF}




procedure TAppData.Minimize;
begin
  var WindowService: IFMXWindowService;
  if TPlatformServices.Current.SupportsPlatformService(IFMXWindowService, IInterface(WindowService)) then
    WindowService.SetWindowState(Application.MainForm, TWindowState.wsMinimized);
end;





{--------------------------------------------------------------------------------------------------
   VERSION INFO
--------------------------------------------------------------------------------------------------}
{$IFDEF FullAppData}
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
function TAppData.GetVersionInfoV: string;
begin
  Result:= ' v'+ GetVersionInfo(False);
end;
{$ELSE}
function TAppData.GetVersionInfoV: string;
begin
  Result:= '';
end;
{$ENDIF}







{-------------------------------------------------------------------------------------------------------------
   OTHERS
-------------------------------------------------------------------------------------------------------------}
{$IFDEF Framework_VCL}
{ Apply this font to all existing forms. }
procedure TAppData.setFont(aFont: TFont);
begin
  if FFont = NIL
  then FFont:= aFont   // We set the font for the first time.
  else
    begin
      // Note: FormCount also counts invisible forms and forms created TFrom.Create(Nil).
      FFont:= aFont;
      for VAR i:= 0 to Screen.CustomFormCount - 1 DO    // FormCount => forms currently displayed on the screen. CustomFormCount = as FormCount but also includes the property pages
        Screen.Forms[i].Font:= aFont;
    end;
end;


{ Set this process to maximum priority. Usefull when measuring time }
procedure TAppData.SetMaxPriority;
begin
  SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS); //  https://stackoverflow.com/questions/13631644/setthreadpriority-and-setpriorityclass
end;
{$ENDIF}


class procedure TAppData.RaiseIfStillInitializing;
CONST
   AppStillInitializingMsg = 'Application not properly initialized.'+sLineBreak + sLineBreak+ 'PLEASE REPORT the steps necessary to reproduce this bug and restart the application.';
begin
 if AppData.Initializing
 then RAISE Exception.Create(AppStillInitializingMsg);
end;




{--------------------------------------------------------------------------------------------------
   UNINSTALLER
---------------------------------------------------------------------------------------------------
   READ/WRITE folders to registry
   This is used by the Uninstaller.
   See c:\MyProjects\Project support\Cubic Universal Uninstaller\Uninstaller.dpr
--------------------------------------------------------------------------------------------------}
{$IFDEF MsWindows}
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
{$ENDIF}




{-------------------------------------------------------------------------------------------------------------
   Prompt To Save/Load File

   These functions are also duplicated in cmIO, cmIO.Win.
   The difference is that there, those functions cannot read/write the LastUsedFolder var so the app cannot remmeber last use folder.

   Example: PromptToSaveFile(s, cGraphUtil.JPGFtl, 'txt');

   DefaultExt:
     Extensions longer than three characters are not supported!
     Do not include the dot (.)
-------------------------------------------------------------------------------------------------------------}
{$IFDEF FullAppData}
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
   TOOLS
   APP COMMAND LINE
-------------------------------------------------------------------------------------------------------------}

{ Returns the path sent as command line param. }
function CommandLinePath: string;
begin
 if ParamCount > 0
 then Result:= Trim(ParamStr(1))     { Do we have parameter into the command line? }
 else Result := '';
end;


{ Recieves a full path and returns the path and the parameters separately }
procedure ExtractPathFromCmdLine(MixedInput: string; OUT Path, Parameters: string);
VAR i: Integer;
begin
 Assert(Length(MixedInput) > 0, 'Command line length is 0!');

 Path:= '';
 Parameters:= '';
 MixedInput:= Trim(MixedInput);

  // Check if the first character is a double quote
 if MixedInput[1]<> '"'
 then Path:= MixedInput
 else
   { Copy all between ""}
   for i:= 2 to Length(MixedInput) DO                                                   { This supposes that " is on the first position }
    if MixedInput[i]= '"' then                                                          { Find next " character }
     begin
      // ToDo: use ccCore.ExtractTextBetween
      Path:= CopyTo(MixedInput, 1+1, i-1);   // Exclude the double quotes               { +1 si -1 because we want to exclude "" }
      Parameters:= Trim(System.COPY(MixedInput, i+1, MaxInt));
      Break;
     end;
end;   { See also: http://delphi.about.com/od/delphitips2007/qt/parse_cmd_line.htm }


function FindCmdLineSwitch(const Switch: string; IgnoreCase: Boolean): Boolean;
begin
  Result:= System.SysUtils.FindCmdLineSwitch(Switch, IgnoreCase);
end;




{-------------------------------------------------------------------------------------------------------------
   LOG
   ---
   SEND MESSAGES DIRECTLY TO LOG WND
-------------------------------------------------------------------------------------------------------------}
procedure TAppData.LogVerb(CONST Msg: string);
begin
 RamLog.AddVerb(Msg);   // This will call NotifyLogObserver
end;


procedure TAppData.LogHint(CONST Msg: string);
begin
 RamLog.AddHint(Msg);    // This will call NotifyLogObserver
end;


procedure TAppData.LogInfo(CONST Msg: string);
begin
 RamLog.AddInfo(Msg);    // This will call NotifyLogObserver
end;


procedure TAppData.LogImpo(CONST Msg: string);
begin
 RamLog.AddImpo(Msg);    // This will call NotifyLogObserver
end;


procedure TAppData.LogWarn(CONST Msg: string);
begin
 RamLog.AddWarn(Msg);    // This will call NotifyLogObserver
end;


procedure TAppData.LogError(CONST Msg: string);
begin
 RamLog.AddError(Msg);   // This will call NotifyLogObserver
end;


procedure TAppData.LogMsg(CONST Msg: string);  { Always show this message, no matter the verbosity of the log. Equivalent to Log.AddError but the msg won't be shown in red. }
begin
 RamLog.AddMsg(Msg);     // This will call NotifyLogObserver
end;


procedure TAppData.LogBold(CONST Msg: string);
begin
 RamLog.AddBold(Msg);    // This will call NotifyLogObserver
end;


procedure TAppData.LogClear;
begin
 RamLog.clear;
end;


procedure TAppData.LogEmptyRow;
begin
 RamLog.AddEmptyRow;
end;


procedure TAppData.setShowOnError(const Value: Boolean);
begin
  FShowOnError:= Value;
  RamLog.ShowonError:= Value;
end;


procedure TAppData.PopUpLogWindow;
begin
  RamLog.PopUpWindow;
end;










procedure TAppData.MainFormCaption(CONST Caption: string);
begin
 {$IFDEF Framework_FMX}
  // Note: FMX: CreateForm does not create the given form immediately. It just adds a request to the pending list. RealCreateForms creates the real forms.
  if Application.MainForm = NIL then EXIT;
 {$ENDIF}
 if Caption= ''
 then Application.MainForm.Caption:= AppName+ ' '+ GetVersionInfoV
 else Application.MainForm.Caption:= AppName+ ' '+ GetVersionInfoV+ ' - ' + Caption;
end;


{$IFDEF MsWindows}
{--------------------------------------------------------------------------------------------------
   UNINSTALLER

   Utility functions for c:\MyProjects\Project support\Universal Uninstaller\Uninstaller.dpr
--------------------------------------------------------------------------------------------------}
(*del
procedure TAppData.AddUninstallerToCtrlPanel(CONST UninstallerExePath: string);          { UninstallerExePath= Full path to the EXE file that represents the uninstaller; ProductName= the uninstaller will be listed with this name. Keep it simple without special chars like '\'. Example: 'BioniX Wallpaper' }
begin
 RegWriteString(HKEY_CURRENT_USER, 'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\'+ AppName, 'DisplayName', AppName);
 RegWriteString(HKEY_CURRENT_USER, 'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\'+ AppName, 'UninstallString', UninstallerExePath);
end; *)


procedure TAppData.RegisterUninstaller;
begin
 RegWriteString(HKEY_CURRENT_USER, 'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\'+ AppName, 'DisplayName', AppName);
 RegWriteString(HKEY_CURRENT_USER, 'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\'+ AppName, 'UninstallString', AppData.SysDir+ 'Uninstall.exe');

 AppData.WriteAppDataFolder;
 AppData.WriteInstallationFolder;
end;
{$ENDIF}





{--------------------------------------------------------------------------------------------------
   LOAD/SAVE Settings
--------------------------------------------------------------------------------------------------}

procedure TAppData.SaveSettings;
begin
  VAR IniFile := TIniFileEx.Create('AppData Settings', IniFile);
  try
    IniFile.Write('AutoStartUp'   , AutoStartUp);
    IniFile.Write('StartMinim'    , StartMinim);
    IniFile.Write('Minimize2Tray' , Minimize2Tray);
    IniFile.Write('Opacity'       , Opacity);
    IniFile.Write('ShowOnError'   , ShowLogOnError);
    IniFile.Write('HintType'      , Ord(HintType));
  finally
    FreeAndNil(IniFile);
  end;
end;


procedure TAppData.LoadSettings;
begin
  VAR IniFile := TIniFileEx.Create('AppData Settings', IniFile);
  try
    AutoStartUp   := IniFile.Read('AutoStartUp'        , False);
    StartMinim    := IniFile.Read('StartMinim'         , False);
    Minimize2Tray := IniFile.Read('Minimize2Tray'      , False);
    Opacity       := IniFile.Read('Opacity'            , 255);
    ShowLogOnError:= IniFile.Read('ShowOnError'        , True);
    HintType      := THintType(IniFile.Read('HintType' , 0));
  finally
    FreeAndNil(IniFile);
  end;

  RunSelfAtStartUp(AutoStartUp);
end;



initialization

FINALIZATION
begin
  FreeAndNil(AppData);
end;


end.
