UNIT LightVcl.Common.AppData;

{=============================================================================================================
   2025.08.01
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
       - Translation engine (multi-language GUI).
       - etc, etc
 ____________________________________________________________________________________________________________

   HOW TO USE IT

     Use the template:
         c:\Projects\LightSaber\Demo\Template App\VCL Simple\TemplateSimple.dpr

     Or use something like this:
       uses
         FastMM4,
         LightCore.INIFile,
         LightVcl.Common.AppData,
         LightVcl.Common.AppDataForm in 'LightCom.AppDataForm.pas';
         MainForm in 'MainForm.pas' {frmMain);
       begin
         AppData:= TAppData.Create('MyAppName', '', MultiThreaded);
         AppData.CreateMainForm(TMainForm, MainForm, TRUE, TRUE, asFull);
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
        See LightVcl.Common.AppDataForm.FormPostInitialize()


     AppData.Initializing
        When the application starts, this flag is set to True.
        Then it is automatically set to False once all the forms are is fully loaded.
        If you don't want this to happen, set the AutoSignalInitializationEnd variable to False.
        In this case, you will have to set the Initializing manually, once the program is fully initialized (usually in the LateInitialize of the main form, or in the LateInitialize of the last created form).
        If you forget it, the AppData will not save the forms to the INI file and you will have a warning on shutdown.
        Usage:
          Used by SaveForm in LightVcl.Common.IniFile.pas/LightVcl.Visual.INIFile.pas (and a few other places) to signal not to save the form if the application has crashed while still in the initialization phase.
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

        [TCoolTrayIcon]
           This must be FALSE in order to make 'Start minimized' option work

        Details
           https://stackoverflow.com/questions/66720721/
           https://stackoverflow.com/questions/14587456/how-can-i-stop-my-application-showing-on-the-taskbar


     Known issues
           If you are creating copies of the same form, the second, third, etc will get a dynamic name.
           This means that they will not be stored/loaded properly from the INI file (because of the dynamic name).

=============================================================================================================}

//ToDo: don't create the LogForm when application starts.Created when needed

INTERFACE

USES
  Winapi.Windows, Winapi.Messages, Winapi.ShellAPI,
  System.Win.Registry, System.SysUtils, System.Classes, System.IOUtils, System.UITypes, System.Types,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Consts,
  LightCore.AppData, LightVcl.Common.LogForm;


TYPE
  TAppData= class(TAppDataCore)
  private
    FFont: TFont;
    FFormLog: TfrmRamLog;       // Create the Log form globally (to be used by the entire Application).
    CopyDataID: DWORD;          // For SingleInstance. This is a unique message ID for our applications. Used when we send the command line to the first running instance via WM_COPYDATA
    procedure setGuiProperties(Form: TForm);
    procedure setFont(aFont: TFont);
    function getGlobalLog: TfrmRamLog;

    // Installer
    procedure writeAppDataFolder;
    procedure writeInstallationFolder;
  protected
    procedure setHintType(const aHintType: THintType); override;
    procedure setHideHint(const Value: Integer); override;

    // Installer
    procedure RegisterUninstaller;
  public
   {--------------------------------------------------------------------------------------------------
      INIT
   --------------------------------------------------------------------------------------------------}
    constructor Create(CONST aAppName: string; CONST WindowClassName: string= ''; MultiThreaded: Boolean= FALSE); override;
    destructor Destroy; override;     // This is called automatically by the "Finalization" section. We need to call it as late as possible.
    procedure Run;

   {--------------------------------------------------------------------------------------------------
      App Single Instance
   --------------------------------------------------------------------------------------------------}
    procedure ResurrectInstance(CONST CommandLine: string);
    function  InstanceRunning: Boolean;
    procedure SetSingleInstanceName(var Params: TCreateParams);
    function  ExtractData(VAR Msg: TWMCopyData; OUT FirstInstance: string): Boolean;

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
    procedure Minimize; override;

    function  RunFileAtStartUp(CONST FilePath: string; Active: Boolean): Boolean;
    function  RunSelfAtStartUp(Active: Boolean): Boolean;

    class procedure RaiseIfStillInitializing;

   {--------------------------------------------------------------------------------------------------
      Installer
   --------------------------------------------------------------------------------------------------}
    function  ReadAppDataFolder(CONST UninstalledApp: string): string;  //used by Uninstaller App
    function  ReadInstallationFolder(CONST UninstalledApp: string): string;

   {--------------------------------------------------------------------------------------------------
      FORMS
   --------------------------------------------------------------------------------------------------}
    procedure CreateMainForm  (aClass: TFormClass;                MainFormOnTaskbar: Boolean= FALSE; Show: Boolean= TRUE; AutoState: TAutoState= asPosOnly); overload;
    procedure CreateMainForm  (aClass: TFormClass; OUT Reference; MainFormOnTaskbar: Boolean= FALSE; Show: Boolean= TRUE; AutoState: TAutoState= asPosOnly); overload;

    procedure CreateForm      (aClass: TFormClass; OUT Reference; Show: Boolean= TRUE; AutoState: TAutoState= asPosOnly; Owner: TWinControl = NIL; Parented: Boolean= FALSE; CreateBeforeMainForm: Boolean= FALSE);
    procedure CreateFormHidden(aClass: TFormClass; OUT Reference;                      AutoState: TAutoState= asPosOnly; ParentWnd: TWinControl = NIL);

    procedure CreateFormModal (aClass: TFormClass; OUT Reference;                      AutoState: TAutoState= asPosOnly; ParentWnd: TWinControl= NIL); overload;   // Do I need this?
    procedure CreateFormModal (aClass: TFormClass;                                     AutoState: TAutoState= asPosOnly; ParentWnd: TWinControl= NIL); overload;

    procedure SetMaxPriority;
    procedure HideFromTaskbar;

    {-------------------------------------------------------------------------------------------------
      App Version
   --------------------------------------------------------------------------------------------------}
    class function GetVersionInfo(ShowBuildNo: Boolean= False): string;
    class function GetVersionInfoV: string;               // Returns version without Build number. Example: v1.0.0
    function  GetVersionInfoMajor: Word;
    function  GetVersionInfoMinor: Word;

   {--------------------------------------------------------------------------------------------------
      Others
   --------------------------------------------------------------------------------------------------}
    procedure MainFormCaption(const Caption: string); override;
    property FormLog: TfrmRamLog read getGlobalLog;
    property Font: TFont read FFont write setFont;
  end;


VAR                      // ToDo 5: make sure AppData is unique (make it Singleton)
   AppData: TAppData;    // This obj is automatically freed on app shutdown (via FINALIZATION)


IMPLEMENTATION

USES
  LightVcl.Common.WinVersion, LightVcl.Common.ExeVersion, LightVcl.Common.Translate,
  LightCore.IO, LightVcl.Common.CenterControl, LightVcl.Common.AppDataForm, LightVcl.Common.Registry;


{ Warning: We cannot use Application.CreateForm here because this will make the Log the main form! }
constructor TAppData.Create(CONST aAppName: string; CONST WindowClassName: string= ''; MultiThreaded: Boolean= FALSE);
begin
  inherited Create(aAppName, WindowClassName, MultiThreaded);
  AppDataCore:= Self;                             // This sets the other variable to self. The non-visual code uses it.
  Application.Initialize;                         // Note: Emba: Although Initialize is the first method called in the main project source code, it is not the first code that is executed in a GUI application. For example, in Delphi, the application first executes the initialization section of all the units used by the Application. in modern Delphi (non-.NET), you can remove Application.Initialize without breaking your program. The method is almost empty and no longer plays a critical role in setting up the VCL or application environment. Its historical purpose was to initialize COM and CORBA, but since those are no longer used, the method is effectively redundant.

  { Single instance } { Register a unique message ID for this applications. Used when we send the command line to the first running instance via WM_COPYDATA. We can do this only once per app so the best place is the Initialization section. However, since AppData is created only once, we can also do it here, in the constructor. https://stackoverflow.com/questions/35516347/sendmessagewm-copydata-record-string }
  CopyDataID:= RegisterWindowMessage('LightSaberCopyDataID');

  { App stuff }
  Application.Title               := aAppName;
  Application.HintColor           := $c0c090;
  Application.HintShortPause      := 40;         // Specifies the time period to wait before bringing up a hint if another hint has already been shown. Windows' default is 50 ms
  Application.UpdateFormatSettings:= FALSE;      // more http://www.delphi3000.com/articles/article_4462.asp?SK=

  // VCL: DefaultFont specifies the default font used by the application. All newly created forms use DefaultFont if the ParentFont property is set to True.
  // Changing DefaultFont later at run-time will trigger an update for all forms that use the parent font.
  // However, we cannot use it becuase it applies only to forms that are controlled by TApplication. But TFormLoglog is not controlled by TApplication (we want to manually release it as late as possible).
  // Now we take the font from the main form (whatwever the user sets there). So Application.DefaultFont isn't doing anything here!
  // The font of the MainForm is saved in TIniFileVCL.WriteComp
  Application.DefaultFont.Name    := 'Segoe UI';
  Application.DefaultFont.Size    := 10;
  Application.ShowHint := TRUE;                   // Set later via the HintType property. It is true by default anyway

  { Translator }
  Translator:= TTranslator.Create(Self);         //ToDo: create it only if necessary - If this app uses translations; we could look for the 'Lang' folder. If it exists and it is not empty then we create the object.
  if NOT RunningHome
  then Translator.LoadLastTranslation;           // Load last language and apply it to all existing forms

  { All done }
  LogVerb(AppName+ GetVersionInfoV+ ' started.');
end;


destructor TAppData.Destroy;
begin
  FreeAndNil(FFormLog);
  FreeAndNil(Translator);
  inherited Destroy;
end;


procedure TAppData.Run;
begin
  Initializing:= FALSE;

  // Later, we ignore the "Show" parameter in CreateMainForm() if "StartMinim" is true. StartMinim remmbers application's last state (it was minimized or not)
  if StartMinim
  then Minimize;

  Application.Run;
end;



{-------------------------------------------------------------------------------------------------------------
   CREATE FORMS
-------------------------------------------------------------------------------------------------------------}
procedure TAppData.CreateMainForm(aClass: TFormClass; MainFormOnTaskbar: Boolean= FALSE; Show: Boolean= TRUE; AutoState: TAutoState= asPosOnly);
begin
  VAR Reference: TForm;
  CreateMainForm(aClass, Reference, MainFormOnTaskbar, Show, AutoState);
end;


procedure TAppData.CreateMainForm(aClass: TFormClass; OUT Reference; MainFormOnTaskbar: Boolean= FALSE; Show: Boolean= TRUE; AutoState: TAutoState= asPosOnly);
begin
  Assert(Vcl.Dialogs.UseLatestCommonDialogs= TRUE);      { This is true anyway by default, but I check it to remember myself about it. Details: http://stackoverflow.com/questions/7944416/tfileopendialog-requires-windows-vista-or-later }
  Assert(Application.MainForm = NIL, 'MainForm already exists!');
  Assert(Font = NIL,                 'AppData.Font already assigned!');

  // Create the log BEFORE we create the main form so we can log possible problems.
  getGlobalLog;

  // Create form
  Application.MainFormOnTaskbar := MainFormOnTaskbar;
  Application.ShowMainForm      := Show;      // Must be false if we want to prevent form flicker during skin loading at startup
  Application.CreateForm(aClass, Reference);

  // Load form size and position
  // Load form font
  // Repair position on desktop
  //
  // Limitation: At this point we can only load "standard" Delphi components. Loading of Light components can only be done in LightVcl.Visual.INIFile.pas -> TIniFileVCL. Work around: The user can override the TLightForm.FormPostInitialize and use the OnBeforeRelease event to insert his own code.
  if TForm(Reference) is TLightForm then
    begin
      TLightForm(Reference).AutoState:= AutoState;
      if AutoState <> asNone
      then TLightForm(Reference).LoadForm; // Load the font
    end;

  // Font
  SetGuiProperties(TForm(Reference));

  // Ignore the "Show" parameter if "StartMinim" is active
  if StartMinim
  then Application.Minimize
  else
    if Show
    then TForm(Reference).Show;

  // Show app name
  MainFormCaption('');      // Must be before FormPostInitialize because the user could put his own caption there.

  // Window fully constructed. Now we can let user run its own initialization process.
  // This is the ONLY correct place where we can properly initialize the application (see "Delphi in all its glory [Part 2]" book) for details.
  if TObject(Reference) is TLightForm
  then TLightForm(Reference).FormPostInitialize;

  // Uninstaller
  if RunningFirstTime
  AND NOT RunningHome
  then RegisterUninstaller;

  // Translate form
  if Translator <> NIL
  then Translator.LoadTranslation(TForm(Reference));

  StartMinim:= FALSE;      // We started minimized ONCE. Now (once started) we need to forget this.
end;


{ Create secondary form
  "Loading" indicates if the GUI settings are remembered or not }
procedure TAppData.CreateForm(aClass: TFormClass; OUT Reference; Show: Boolean= TRUE; AutoState: TAutoState= asPosOnly; Owner: TWinControl= NIL; Parented: Boolean= FALSE; CreateBeforeMainForm: Boolean= FALSE);
begin
  if CreateBeforeMainForm
  then
    begin
      // We allow the Log form to be created before the main form.
      if (aClass = TfrmRamLog)
      then TForm(Reference):= aClass.Create(NIL) // AppData will release the logform on Finalize
      else
        if (Owner = NIL)
        then TForm(Reference):= TFormClass.Create(Application)  // Owned by Application. But we cannot use Application.CreateForm here because then, this form will be the main form!
        else TForm(Reference):= TFormClass.Create(Owner);       // Owned by Owner
      //ToDo 4: For the case where the frmLog is created before the MainForm: copy the frmLog.Font from the MainForm AFTER the main MainForm created.
    end
  else
    begin
      if Application.MainForm = NIL
      then RAISE Exception.Create('Probably you forgot to create the main form with AppData.CreateMainForm!');

      if Owner = NIL
      then Application.CreateForm(aClass, Reference)        // Owned by Application
      else TForm(Reference):= TFormClass.Create(Owner);    // Owned by Owner
    end;

  // Center form in the Owner
  if (Owner <> NIL)
  AND Parented then
    begin
      TForm(Reference).Parent:= Owner;
      CenterChild(TForm(Reference), Owner);
    end;

  // Font, snap, alpha
  SetGuiProperties(TForm(Reference));

  // Load form settings/position
  if TForm(Reference) is TLightForm then
    begin

     // Load form size and position
     // Load form font
     // Repair position on desktop

     TLightForm(Reference).AutoState:= AutoState;
     TLightForm(Reference).LoadForm;
    end;

  if Show
  then TForm(Reference).Show;

  // Window fully constructed.
  // Now we can let user run its own initialization process.
  if TComponent(Reference).InheritsFrom(TLightForm)
  then TLightForm(Reference).FormPostInitialize;

  // Translator
  if Translator <> NIL
  then Translator.LoadTranslation(TForm(Reference));
end;



{ Create secondary form }
procedure TAppData.CreateFormHidden(aClass: TFormClass; OUT Reference; AutoState: TAutoState= asPosOnly; ParentWnd: TWinControl= NIL);
begin
  CreateForm(aClass, Reference, FALSE, AutoState, ParentWnd);
end;


{ Create secondary form }
procedure TAppData.CreateFormModal(aClass: TFormClass; AutoState: TAutoState= asPosOnly; ParentWnd: TWinControl= NIL);
VAR Reference: TForm;
begin
  CreateForm(aClass, Reference, FALSE, AutoState, ParentWnd);
  Reference.ShowModal;
end;


{ Create secondary form }
//ToDo: Do I need this? Since the form is modal, I should never need the Reference? To be deleted
procedure TAppData.CreateFormModal(aClass: TFormClass; OUT Reference; AutoState: TAutoState= asPosOnly; ParentWnd: TWinControl= NIL);
begin
  CreateFormModal(aClass, AutoState, ParentWnd);
end;


// The TfrmRamLog is destroyed by AppData in Finalize
function TAppData.getGlobalLog: TfrmRamLog;
begin
  Assert(RamLog <> NIL, 'RamLog not created!');

  if FFormLog = NIL
  then
    begin
      VAR CreateBeforeMainForm:= Application.MainForm = NIL;
      CreateForm(TfrmRamLog, FFormLog, FALSE, asPosOnly, NIL, FALSE, CreateBeforeMainForm);
      Assert(FFormLog.Log <> NIL);
      FFormLog.Log.AssignExternalRamLog(RamLog);   // Connect the RamLog with the visual log. FormLog will display data from AppData's RAM log

      // Set font
      if Application.MainForm <> NIL
      then FFormLog.Font:= Application.MainForm.Font;   // If the frmLog is created before the MainForm, copy the frmLog.Font from the MainForm AFTER the main MainForm created.
    end
  else
    FFormLog.Show;

  Result:= FFormLog;
  Assert(Application.MainForm <> FFormLog, 'Sanity check! The Log should not be the MainForm!'); { Make sure this is not the first form created }
end;


// Note:
// In FMX the forms don't have a hint.
// Hints are supported on Windows and macOS only.
// Info: Setting ShowHint for the application to False disables all Help Hints, regardless of the value of the ShowHint properties for individual controls.
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
{ Run the specified application at Windows startup }
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


{ Run THIS application at Windows startup }
function TAppData.RunSelfAtStartUp(Active: Boolean): Boolean;
begin
  Result:= RunFileAtStartUp(ParamStr(0), Active);
end;



{--------------------------------------------------------------------------------------------------
   APPLICATION CONTROL
        LOW LEVEL
--------------------------------------------------------------------------------------------------}
procedure TAppData.Restart;
begin
  VAR PAppName:= PChar(Application.ExeName);
  Winapi.ShellAPI.ShellExecute({Handle} 0, 'open', PAppName, nil, nil, SW_SHOWNORMAL);   { Handle does not work. Replaced with 0. }
  Application.Terminate;
end;


{ This is a bit dirty! It creates a BAT that deletes the EXE. Some antiviruses might block this behavior? }
procedure TAppData.SelfDelete;
CONST
  BatCode = ':delete_exe' + sLineBreak + 'del "%s"' + sLineBreak + 'if exist "%s" goto delete_exe' + sLineBreak + 'del "%s"';
VAR
 List    : TStringList;
 PI      : TProcessInformation;
 SI      : TStartupInfo;
 BatPath : string;
begin
  BatPath := TPath.Combine(TPath.GetTempPath, ChangeFileExt(AppShortName, '.BAT'));
  List := TStringList.Create;
  TRY
    List.Text := Format(BatCode, [Application.ExeName, Application.ExeName, BatPath]);
    List.SaveToFile(BatPath);
  FINALLY
    FreeAndNil(List);
  END;

  FillChar(SI, SizeOf(SI), 0);
  SI.dwFlags := STARTF_USESHOWWINDOW;
  SI.wShowWindow := SW_HIDE;

  if CreateProcess( NIL, PChar(BatPath), NIL, NIL, False, IDLE_PRIORITY_CLASS, NIL, NIL, SI, PI) then
   begin
     CloseHandle(PI.hThread);
     CloseHandle(PI.hProcess);
   end;

  Application.Terminate;                                                                            { This is mandatory }
end;


{ Set this process to maximum priority. Usefull when measuring time }
procedure TAppData.SetMaxPriority;
begin
  SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS); //  https://stackoverflow.com/questions/13631644/setthreadpriority-and-setpriorityclass
end;


{--------------------------------------------------------------------------------------------------
   APPLICATION CONTROL
        WND STATE
--------------------------------------------------------------------------------------------------}
procedure TAppData.Minimize;
begin
  Application.Minimize;
  StartMinim:= TRUE;
end;


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
  StartMinim:= FALSE;
end;


{ Hides Application's TaskBar Button
  Source: http://stackoverflow.com/questions/14811935/how-to-hide-an-application-from-taskbar-in-windows-7
  If Application.MainFormOnTaskbar is true, a taskbar button represents the application's main form and displays its caption.
  All child forms will stay on top of the MainForm (bad)! If False, a taskbar button represents the application's (hidden) main window and bears the application's Title. Must be True to use Windows (Vista) Aero effects (ive taskbar thumbnails, Dynamic Windows, Windows Flip, Windows Flip 3D). https://stackoverflow.com/questions/66720721/ }     //Do this in DPR
procedure TAppData.HideFromTaskbar;
begin
 ShowWindow(Application.Handle, SW_HIDE);
 //ShowWindow(MainForm.Handle, SW_HIDE);     //this also hides the form from the screen
{ Code below not working in Win7 !!!
  ShowWindow(Application.Handle, SW_HIDE);
  SetWindowLongPtr(Application.Handle, GWL_EXSTYLE, GetWindowLongPtr(Application.Handle, GWL_EXSTYLE) OR WS_EX_TOOLWINDOW);}      { getWindowLong_ was replaced with getWindowLongPtr for 64 bit compatibility. Details: http://docwiki.embarcadero.com/RADStudio/Seattle/en/Converting_32-bit_Delphi_Applications_to_64-bit_Windows }
end;



{-------------------------------------------------------------------------------------------------------------
   OTHERS
-------------------------------------------------------------------------------------------------------------}
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


procedure TAppData.setGuiProperties(Form: TForm);
begin
  MainFormCaption('Initializing form '+ Form.Name);

  Form.ShowHint:= HintType > htOff;
  Form.ParentFont:= TRUE;

  if Form = Application.MainForm
  then
    // TAppData (self) takes the font of the main form and to apply it later to all existing and all future windows.
    Self.Font:= Form.Font
  else
    // We set the same font for secondary forms
    if Self.Font <> nil
    then Form.Font:= Self.Font;

  // Fix issues with snap to edge of the screen
  if LightVcl.Common.WinVersion.IsWindows8Up
  then Form.SnapBuffer:= 4;

  // Form transparency
  Form.AlphaBlendValue := Opacity;
  Form.AlphaBlend:= Opacity< 255;
end;





{--------------------------------------------------------------------------------------------------
   VERSION INFO
--------------------------------------------------------------------------------------------------}

function TAppData.GetVersionInfoMajor: Word;
VAR FixedInfo: TVSFixedFileInfo;
begin
  if GetVersionInfoFile(Application.ExeName, FixedInfo)
  then Result:= HiWord(FixedInfo.dwFileVersionMS)
  else Result:= 0;
end;


function TAppData.GetVersionInfoMinor: Word;
VAR FixedInfo: TVSFixedFileInfo;
begin
  if GetVersionInfoFile(Application.ExeName, FixedInfo)
  then Result:= LoWord(FixedInfo.dwFileVersionMS)
  else Result:= 0;
end;


{ Returns version with/without build number.
  Example: 1.0.0 for 1.0.0.999
  See also: CheckWin32Version }
class function TAppData.GetVersionInfo(ShowBuildNo: Boolean= False): string;
VAR FixedInfo: TVSFixedFileInfo;
begin
  FixedInfo.dwSignature:= 0;
  if LightVcl.Common.ExeVersion.GetVersionInfoFile(Application.ExeName, FixedInfo)
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





{-------------------------------------------------------------------------------------------------------------
   SINGLE INSTANCE
--------------------------------------------------------------------------------------------------------------
   This allows us to detect if an instance of this program is already running.
   If so, we can send the command line of the second instance to the first instance, and shutdown the second instance.

   Scenario:
     We have a music player playing and MP3 file. We want of course only one player playing music. Who wants otherwise (unless you like to listen to Yes).
     The user double clicks a new MP3 file to listen it. A new instance will start.
     This new instance will detect that an instance is already running and pass the MP3 file name to the already existing instance (via a CopyData message) and shutdown itself.
     The existing instance would have to process the received message accordingly (stop current MP3 file and load the new one).

   USAGE:
     In DPR write:
      AppData:= TAppData.Create('MyCoolAppName', SingleInstClassName);

      if AppData.InstanceRunning
      then TAppData.ResurectInstance(ParamStr(1))
      else
       begin
        Application.Initialize;
        Application.CreateForm(TMainForm, MainForm);
        Application.Run;
       end;

     In MainForm:
       procedure TMainFrom.CreateParams(var Params: TCreateParams);
       begin
         inherited;
         AppData.SetSingleInstanceName(Params);
       end;

     Check BioniX/Baser for a full example.

  UNIQUE ID:
     SingleInstClassName is a constant representing your application name/ID. This string must be unique.
     We use it in InstanceRunning to detect if the the application is already running, by looking to a form with this Class Name.

  Receiver:
     Application's main form needs to implement WMCopyData.
     Warning: We should never use PostMessage() with the WM_COPYDATA message because the data that is passed to the receiving application is only valid during the call. Finally, be aware that the call to SendMessage will not return untill the message is processed.

     Recommendation:
       1. The receiver procedure should also restore (bring to front) that instance.
       2. We should close this second instance after we sent the message to the first instance.

  Links:
     https://stackoverflow.com/questions/35516347/sendmessagewm-copydata-record-string
     https://stackoverflow.com/questions/8688078/preventing-multiple-instances-but-also-handle-the-command-line-parameters
     https://stackoverflow.com/questions/23031208/delphi-passing-running-parameters-to-other-instance-via-wm-copydata-gives-wrong
     https://github.com/delphidabbler/articles/blob/master/article-13-pt2.pdf

  Alternative implementation:
     LightVcl.Common.MutexSingleInstance.pas

  I would have loved to have all the code in a single procedure but unfortunately this is not possible. Three procedures are required.
-------------------------------------------------------------------------------------------------------------}

{ Returns True if an instance of this application was found already running }
function TAppData.InstanceRunning: Boolean;
begin
  Result:= WinApi.Windows.FindWindow(PWideChar(SingleInstClassName), NIL) > 0;
end;


procedure TAppData.ResurrectInstance(CONST CommandLine: string);
VAR
   Window: HWND;
   DataToSend: TCopyDataStruct;
begin
  { Prepare the data you want to send }
  DataToSend.dwData := CopyDataID;  // Registered unique ID for LighSaber apps
  DataToSend.cbData := Length(CommandLine) * SizeOf(Char);
  DataToSend.lpData := PChar(CommandLine);

  Window:= WinApi.Windows.FindWindow(PWideChar(SingleInstClassName), NIL);    // This is a copy of csWindow.FindTopWindowByClass
  SendMessage(Window, WM_COPYDATA, 0, LPARAM(@DataToSend));
  { Winapi.Windows.ShowWindow(Window, SW_SHOWNORMAL); Winapi.Windows.SetForegroundWindow(Window);   }
end;


{ Give a unique name to our form.
  We need to call this in TMainForm.CreateParams(var Params: TCreateParams)  }
procedure TAppData.SetSingleInstanceName(VAR Params: TCreateParams);
begin
  // Copies a null-terminated string. StrCopy is designed to copy up to 255 characters from the source buffer into the destination buffer. If the source buffer contains more than 255 characters, the procedure will copy only the first 255 characters.
  System.SysUtils.StrCopy(Params.WinClassName, PChar(SingleInstClassName));
  //Hint: This would work if WindowClassName would be a constant: Params.WinClassName:= WindowClassName
end;


{ Extract the data (from them Msg) that was sent to us by the second instance.
  Returns True if the message was indeed for us AND it is not empty.
  Returns the extracted data in 'FirstInstance'.
  It will also bring the first instance to foreground. }
function TAppData.ExtractData(VAR Msg: TWMCopyData; OUT FirstInstance: string): Boolean;
begin
 FirstInstance:= '';
 Result:= FALSE;
 if Msg.CopyDataStruct.dwData = CopyDataID then { Only react on this specific message }
   begin
    if Msg.CopyDataStruct.cbData > 0 then       { Do we receive an empty string? }
      begin
        SetString(FirstInstance, PChar(Msg.CopyDataStruct.lpData), Msg.CopyDataStruct.cbData div SizeOf(Char)); { We need a true copy of the data before it disappear }
        Msg.Result:= 2006;                      { Send something back as positive answer }
        Result:= TRUE;
      end;
    Restore;
   end;
end;



{-------------------------------------------------------------------------------------------------------------
   Prompt To Save/Load File
   Remembers the last used folder.
   Example: PromptToSaveFile(s, LightVcl.Graph.Util.JPGFtl, 'txt');

   Note:
      These functions are also duplicated in LightVcl.Common.IO.Win.
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






{-------------------------------------------------------------------------------------------------------------
   OTHERS
-------------------------------------------------------------------------------------------------------------}
procedure TAppData.MainFormCaption(CONST Caption: string);
begin
  inherited;
  
  if Caption= ''
  then Application.MainForm.Caption:= AppName+ ' '+ GetVersionInfoV
  else Application.MainForm.Caption:= AppName+ ' '+ GetVersionInfoV+ ' - ' + Caption;
end;


procedure TAppData.setHideHint(const Value: Integer);
begin
  inherited;
  Application.HintHidePause:= Value;       // Specifies the time interval to wait before hiding the Help Hint if the mouse has not moved from the control or menu item. Windows' default is 2500 ms
end;


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
CONST
   UninstallerRegKey: string= 'Software\CubicDesign\';

procedure TAppData.writeAppDataFolder;                                           { Called by the original app }
begin
  RegWriteString(HKEY_CURRENT_USER, UninstallerRegKey+ AppName, 'App data path', AppDataFolder);                                                                                                                              {Old name: WriteAppGlobalData }
end;


function TAppData.ReadAppDataFolder(CONST UninstalledApp: string): string;       { Called by the uninstaller }
begin
  Result:= RegReadString(HKEY_CURRENT_USER, UninstallerRegKey+ UninstalledApp, 'App data path');
end;


{------------------------
   Instalation Folder
------------------------}
procedure TAppData.writeInstallationFolder;                                      { Called by the original app }                                                                                                                                       {Old name: WriteAppGlobalData }
begin
  RegWriteString(HKEY_CURRENT_USER, UninstallerRegKey+ AppName, 'Install path', ExeFolder);
end;


function TAppData.ReadInstallationFolder(CONST UninstalledApp: string): string;  { Called by the uninstaller }
begin
  Result:= RegReadString(HKEY_CURRENT_USER, UninstallerRegKey+ UninstalledApp, 'Install path');
end;


// This will be called automatically by CreateMainForm
procedure TAppData.RegisterUninstaller;
begin
  // Write to Control Panel
  RegWriteString(HKEY_CURRENT_USER, 'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\'+ AppName, 'DisplayName', AppName);
  RegWriteString(HKEY_CURRENT_USER, 'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\'+ AppName, 'UninstallString', SysDir+ 'Uninstall.exe');

  writeAppDataFolder;
  writeInstallationFolder;
end;







INITIALIZATION
// Hint: We could create AppData here but the Initialization sections are executed in random order.

FINALIZATION
begin
  AppData.Free;   // DON'T use FreeAndNil here because it will set the AppData variable to Nil too early. We still need the variable in the destructors of the forms.
  AppData:= NIL;
end;


end.
