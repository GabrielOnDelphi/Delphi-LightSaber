UNIT LightFmx.Common.AppData;

{=============================================================================================================
   2025.05.27
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   FEATURES

     TAppData
       - Get application's version.
       - Log error messages to a special window that pops up when you send warnings and errors to it.
         The Log window (TfrmRamLog) is automatically created (hidden) and destroyed.
       - Easily create new forms and set its font to be the same as main forms' font.
       - Change the font for all running forms.
       - Basic support for Uninstaller (The Uninstaller can find out where the app was installed).
       - Basic support for licensing (trial period) system. See Proteus for details.
       - Translation engine (multi-language GUI) (Not ready yet, under FMX)
       - etc, etc
 ____________________________________________________________________________________________________________

   HOW TO USE IT

     Use the template:
         c:\Projects\LightSaber\Demo\Template App\VCL Simple\TemplateSimple.dpr

     Or use something like this:
       uses
         FastMM4,
         LightCore.INIFile,
         LightFmx.Common.AppData,
         LightFmx.Common.AppData.Form in 'LightFmx.Common.AppData.Form.pas';
         MainForm in 'MainForm.pas' {frmMain);
       begin
         AppData:= TAppData.Create('MyAppName', '', MultiThreaded);
         AppData.CreateMainForm(TMainForm, MainForm, TRUE, TRUE);
         AppData.CreateForm(TSecondFrom, frmSecond);                 // Secondary form (optional)
         AppData.Run;
       end.

     DPR file
        In the DPR file these lines of code are not necessary anymore and you MUST remove them.
           Application.Title := 'x';
           Application.ShowMainForm:= True;
           MainForm.Show;

 ____________________________________________________________________________________________________________

   DOCUMENTATION

     The APPDATA global var
        It is important to create the AppData object as soon as the application starts.
        Not necessary to free AppData. It frees itself.


     OnFormCreate
        See LightFmx.Common.AppDataForm.FormPostInitialize()


     AppData.Initializing
        When the application starts, this flag is set to True.
        Then it is automatically set to False once all the forms are is fully loaded.
        If you don't want this to happen, set the AutoSignalInitializationEnd variable to False.
        In this case, you will have to set the Initializing manually, once the program is fully initialized (usually in the LateInitialize of the main form, or in the LateInitialize of the last created form).
        If you forget it, the AppData will not save the forms to the INI file and you will have a warning on shutdown.
        Usage:
          Used by SaveForm in Light_FMX.Common.IniFile.pas/Light_FMX.Visual.INIFile.pas (and a few other places) to signal not to save the form if the application has crashed while still in the initialization phase.
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


     Known issues
           If you are creating copies of the same form, the second, third, etc will get a dynamic name.
           This means that they will not be stored/loaded properly from the INI file (because of the dynamic name).

=============================================================================================================}

INTERFACE
{$I Frameworks.inc}

USES

  {$IFDEF MsWindows}
    Winapi.Windows, System.Win.Registry, 
  {$ENDIF}

  {$IFDEF ANDROID}
   Androidapi.Helpers, Androidapi.JNI.App, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText,
  {$ENDIF}
  {$IFDEF MacOS}
   Posix.Stdlib, Posix.Unistd,
  {$ENDIF}  
  System.SysUtils, System.Classes, System.UITypes, System.Types, System.Generics.Collections,
  FMX.Forms, FMX.Graphics, FMX.Types, FMX.Platform,
  LightFmx.Common.LogViewer, LightFMX.LogForm,


  LightCore, LightCore.INIFile, LightCore.AppData; //Light_FMX.LogForm

TYPE
  TAppData= class(TAppDataCore)
  private
    FAutoStateQueue: TList<TAutoState>;
    FFont: TFont;
    FFormLog: TfrmRamLog;       // Create the Log form (to be used by the entire program). It is released by TApplication
    procedure setFont(aFont: TFont);
    procedure createGlobalLog;
  protected
    procedure setHintType(const aHintType: THintType); override;
    procedure setHideHint(const Value: Integer); override;
  public
   {--------------------------------------------------------------------------------------------------
      INIT
   --------------------------------------------------------------------------------------------------}
    constructor Create(CONST aAppName: string; CONST WindowClassName: string= ''; MultiThreaded: Boolean= FALSE); override;
    destructor Destroy; override;     // This is called automatically by the "Finalization" section. We need to call it as late as possible.
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
      FORMS
   --------------------------------------------------------------------------------------------------}
    procedure CreateMainForm  (aClass: TComponentClass; OUT aReference; Show: Boolean = TRUE; AutoState: TAutoState = asPosOnly); overload;
    procedure CreateMainForm  (aClass: TComponentClass;                 Show: Boolean = TRUE); overload;

    procedure CreateForm      (aClass: TComponentClass; OUT aReference; Show: Boolean = TRUE; AutoState: TAutoState = asPosOnly; Parented: Boolean = FALSE; CreateBeforeMainForm: Boolean = FALSE); overload;
    procedure CreateFormHidden(aClass: TComponentClass; OUT aReference);
    procedure CreateFormModal (aClass: TComponentClass);  // Problem in Android with modal forms!

    function  GetAutoState: TAutoState;

   {--------------------------------------------------------------------------------------------------
      Others
   --------------------------------------------------------------------------------------------------}
    procedure SetMaxPriority;
    procedure MainFormCaption(const Caption: string); override;
    property FormLog: TfrmRamLog read FFormLog;   //I could create this form, at runtime, as necessary
    property Font: TFont read FFont write setFont;
  end;


VAR                      // ToDo 5: make sure AppData is unique (make it Singleton)
   AppData: TAppData;    // This obj is automatically freed on app shutdown (via FINALIZATION)


IMPLEMENTATION

USES
  LightCore.IO, LightFmx.Common.AppData.Form;



{ Warning: We cannot use Application.CreateForm here because this will make the Log the main form! }
constructor TAppData.Create(CONST aAppName: string; CONST WindowClassName: string= ''; MultiThreaded: Boolean= FALSE);
begin
  inherited Create(aAppName, WindowClassName, MultiThreaded);
  FFormLog := nil;  
  AppDataCore:= Self;                             // This sets the other variable to self. The non-visual code uses it.
  Application.Initialize;                         // Note: Emba: Although Initialize is the first method called in the main project source code, it is not the first code that is executed in a GUI application. For example, in Delphi, the application first executes the initialization section of all the units used by the Application. in modern Delphi (non-.NET), you can remove Application.Initialize without breaking your program. The method is almost empty and no longer plays a critical role in setting up the VCL or application environment. Its historical purpose was to initialize COM and CORBA, but since those are no longer used, the method is effectively redundant.
  { App stuff }
  Application.Title               := aAppName;
  Application.ShowHint := TRUE;                   // Set later via the HintType property. It is true by default anyway

  FAutoStateQueue := TList<TAutoState>.Create;
  
  { All done }
  LogVerb(AppName+ ' started.');
end;


destructor TAppData.Destroy;
begin
  FreeAndNil(FAutoStateQueue);
  //FreeAndNil(FormLog);  Freed by TApplication
  inherited Destroy;
end;


procedure TAppData.Run;
begin
  Initializing:= FALSE;

  // Later, we ignore the "Show" parameter in CreateMainForm() if "StartMinim" is true. StartMinim remmbers application's last state (it was minimized or not)
  // Note: FMX: CreateForm does not create the given form immediately. It just adds a request to the pending list. RealCreateForms creates the real forms.
  if StartMinim
  then Minimize;

  Application.Run;
end;




{-------------------------------------------------------------------------------------------------------------
   CREATE FORMS
--------------------------------------------------------------------------------------------------------------
   W A R N I N G
     On FMX, CreateForm does not create the given form immediately.
     It just adds a request to the pending list. RealCreateForms creates the real forms.
     So, we cannot access the form here because it was not yet created!
-------------------------------------------------------------------------------------------------------------}
procedure TAppData.CreateMainForm(aClass: TComponentClass; OUT aReference; Show: Boolean = TRUE; AutoState: TAutoState = asPosOnly);
begin
  Assert(Application.MainForm = NIL, 'MainForm already exists!');   //ToDo: test if this works under FMX because of RealCreateForms
  CreateGlobalLog;
  
  FAutoStateQueue.Add(AutoState);
  Application.CreateForm(aClass, aReference);                       // Reference is NIL here because of RealCreateForms
end;


procedure TAppData.CreateMainForm(aClass: TComponentClass; Show: Boolean= TRUE);
var
  aReference: TForm;
begin
  CreateMainForm(aClass, aReference, Show);
end; 



{ Create secondary form
  "Loading" indicates if the GUI settings are remembered or not }
procedure TAppData.CreateForm(aClass: TComponentClass; OUT aReference; Show: Boolean = TRUE; AutoState: TAutoState = asPosOnly; Parented: Boolean = FALSE; CreateBeforeMainForm: Boolean = FALSE);
begin
  if CreateBeforeMainForm
  then
    begin
      FAutoStateQueue.Add(AutoState);
      Application.CreateForm(aClass, aReference);
      ///TForm(aReference).SetOwner(nil);  // Optional: manage lifetime manually
    end
  else
    begin
      if Application.MainForm = nil
      then RAISE Exception.Create('Probably you forgot to create the main form with AppData.CreateMainForm!');

      // Deferred creation (initial or queued dynamic form)
      FAutoStateQueue.Add(AutoState);
      Application.CreateForm(aClass, aReference);
    end;
end;


{ Returns the next AutoState from the list. }
function TAppData.GetAutoState: TAutoState;
begin
  if (FAutoStateQueue <> nil)  // Check if the queue exists and has items
  AND (FAutoStateQueue.Count > 0) then
    begin
      // Retrieve and remove this form's AutoState
      Result := FAutoStateQueue.First;
      FAutoStateQueue.Delete(0);

      // If the queue is now empty, free it
      if FAutoStateQueue.Count = 0
      then FreeAndNil(FAutoStateQueue);
    end
  else
    Result := asNone; // Default value if queue is empty or nil
end;


{ Create secondary form }
procedure TAppData.CreateFormHidden(aClass: TComponentClass; OUT aReference);
begin
  CreateForm(aClass, aReference, FALSE);
end;


{ Create secondary form }
procedure TAppData.CreateFormModal(aClass: TComponentClass);
VAR aReference: TForm;
begin
  // Problem in Android with modal forms!
  CreateForm(aClass, aReference, FALSE);
  aReference.ShowModal;
end;


// The TfrmRamLog is destroyed by AppData in Finalize
procedure TAppData.createGlobalLog;
begin
  Assert(RamLog  <> NIL, 'RamLog not created!');
  Assert(FFormLog = NIL, 'Form log already created!');
  
  Application.CreateForm(TfrmRamLog, FFormLog);
  //FFormLog.Owner:= NIL;  // Remove from TApplication ownership
  FAutoStateQueue.Add(asPosOnly);  // Queue its AutoState
end;










procedure TAppData.setHideHint(const Value: Integer);
begin
  inherited;
end;


// Note:
// In FMX the forms don't have a hint.
// Hints are supported on Windows and macOS only.
// Application.ShowHint = Global setting. Applies also to actions.
// https://docwiki.embarcadero.com/RADStudio/Athens/en/Using_Hints_to_Show_Contextual_Help_in_a_FireMonkey_Application
procedure TAppData.setHintType(const aHintType: THintType);
begin
  FHintType:= aHintType;
  Application.ShowHint:= aHintType > htOff;
end;










{--------------------------------------------------------------------------------------------------
   APPLICATION CONTROL
      WIN START UP
--------------------------------------------------------------------------------------------------}
{Summary

    macOS: Uses launchctl to manage startup items.
    Android: Uses broadcast receivers to trigger actions at boot.
    Linux: Uses .desktop files in the ~/.config/autostart directory to manage startup items.}

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


{ Run THIS application at Windows startup }
function TAppData.runSelfAtStartUp(Active: Boolean): Boolean;
begin
  Result:= RunFileAtStartUp(ParamStr(0), Active);
end;

{ Set this process to maximum priority. Usefull when measuring time }
procedure TAppData.SetMaxPriority;
begin
  {$IFDEF MSWINDOWS}
  SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS); //  https://stackoverflow.com/questions/13631644/setthreadpriority-and-setpriorityclass
  {$ENDIF}
end;


{--------------------------------------------------------------------------------------------------
   APPLICATION CONTROL
        WND STATE
--------------------------------------------------------------------------------------------------}
procedure TAppData.Minimize;
begin
  var WindowService: IFMXWindowService;
  if TPlatformServices.Current.SupportsPlatformService(IFMXWindowService, IInterface(WindowService)) then
    WindowService.SetWindowState(Application.MainForm, TWindowState.wsMinimized);
  StartMinim:= TRUE;
end;






{-------------------------------------------------------------------------------------------------------------
   OTHERS
-------------------------------------------------------------------------------------------------------------}
{ Apply this font to all existing forms. }
procedure TAppData.setFont(aFont: TFont);
begin
  {if FFont = NIL
  then FFont:= aFont   // We set the font for the first time.
  else
    begin
      // Note: FormCount also counts invisible forms and forms created TFrom.Create(Nil).
      FFont:= aFont;
      for VAR i:= 0 to Screen.FormCount - 1 DO    // FormCount => forms currently displayed on the screen. CustomFormCount = as FormCount but also includes the property pages
        Screen.Forms[i].Font:= aFont;
    end;  }
end;





{$IFDEF FullAppData}





{-------------------------------------------------------------------------------------------------------------
   Prompt To Save/Load File
   Remembers the last used folder.
   Example: PromptToSaveFile(s, Light_FMX.Graph.Util.JPGFtl, 'txt');

   Note:
      These functions are also duplicated in Light_FMX.Common.IO.Win.
      The difference is that there, those functions cannot read/write the LastUsedFolder var so the app cannot remmeber last use folder.

   Note:
      Extensions longer than three characters are not supported! Do not include the dot (.)
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
  AllowMultiSelect cannot be true, because I return a single file name (cannot return a Tstringlist).
  Once the user selected a folder it is remembered in "LastUsedFolder" var  }
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
        // Remember last used folder
        Self.LastUsedFolder:= ExtractFilePath(FileName);
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
  
  // Note: FMX: CreateForm does not create the given form immediately. It just adds a request to the pending list. RealCreateForms creates the real forms.
  if Application.MainForm = NIL then EXIT;

  if Caption= ''
  then Application.MainForm.Caption:= AppName+ ' '
  else Application.MainForm.Caption:= AppName+ ' '+ ' - ' + Caption;
end;








INITIALIZATION
// Hint: We could create AppData here but the Initialization sections are executed in random order.

FINALIZATION
begin
  AppData.Free;   // DON'T use FreeAndNil here because it will set the AppData variable to Nil too early. We still need the variable in the destructors of the forms.
  AppData:= NIL;
end;


end.