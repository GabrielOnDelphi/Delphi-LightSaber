UNIT LightFmx.Common.AppData;

{=============================================================================================================
   2026.07.06
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
         LightFmx.Common.AppData.Form in 'LightFmx.Common.AppData.Form.pas',
         MainForm in 'MainForm.pas' {frmMain);   // Deliberately closed with a round paren, not a curly brace - see the warning further down, right before this comment block's real closing line.
       begin
         AppData:= TAppData.Create('MyAppName', '', MultiThreaded);
         AppData.CreateMainForm(TMainForm, frmMain, asPosOnly);
         AppData.CreateForm(TSecondForm, frmSecond);                 // Secondary form (optional)
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


     AppData.Initializing
        When the application starts, this flag is set to True.
        Then it is automatically set to False once all the forms are fully loaded (TLightForm.Loaded defers TAppDataCore.EndInitialization to the message queue).
        Note: this happens only for TLightForm descendants. If your app has no TLightForm at all, call TAppDataCore.EndInitialization yourself once the program is fully initialized, otherwise the forms will not be saved to the INI file.
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

   WARNING ABOUT THIS COMMENT BLOCK
      Curly-brace comments do not nest in Delphi. This whole block is one comment, opened at the
      top of the file and closed by the line right below this paragraph. The sample 'uses' clause
      above intentionally writes MainForm's IDE hint as an unmatched open-paren pair, not a matched
      curly-brace pair - a real closing brace there would end this comment early, and everything
      from that point down to the real closing line below would be handed to the compiler as code.
      Do not "correct" that pair to use a curly brace on both sides.

=============================================================================================================}

INTERFACE
{$I Frameworks.inc}
{$IMPORTEDDATA ON}   // E2201: TPendingAutoState (record with managed string field) lives in this
                     // unit's interface, so other units in the same package emit cross-unit
                     // record-RTTI references that require imported-data ($G+) to resolve.

USES
  {$IFDEF MsWindows}
    Winapi.Windows, System.Win.Registry,
  {$ENDIF}

  {$IFDEF ANDROID}
   Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText,
  {$ENDIF}
  {$IFDEF MacOS}
   Posix.Stdlib, Posix.Unistd,
  {$ENDIF}
  System.SysUtils, System.Classes, System.UITypes, System.Types, System.IOUtils, System.Generics.Collections,
  FMX.Forms, FMX.Platform,
  LightFmx.Common.LogForm,
  LightCore.AppData, LightCore.IO;

TYPE
  TPendingAutoState = record
    ClassName: string;
    AutoState: TAutoState;
    QueuedBeforeRun: Boolean;  // TRUE = queued in DPR before Run; FALSE = created dynamically during/after Run
  end;

TYPE
  TAppData= class(TAppDataCore)
  private
    FPendingAutoStates: TList<TPendingAutoState>;  // Stores AutoState by class name for queued forms (aReference = NIL)
    FFormLog: TfrmRamLog;       // The Log form (to be used by the entire program). Owned by TApplication, but explicitly freed in TAppData.Destroy — see the comment there (Application itself dies only AFTER this unit's finalization).
    function getLogForm: TfrmRamLog;
  protected
    procedure setHintType(const aHintType: THintType); override;
  public
   {--------------------------------------------------------------------------------------------------
      INIT
   --------------------------------------------------------------------------------------------------}
    constructor Create(CONST aAppName: string; CONST WindowClassName: string= ''; MultiThreaded: Boolean= FALSE); override;
    destructor Destroy; override;     // This is called automatically by the "Finalization" section. We need to call it as late as possible.
    procedure Run;

    procedure Minimize; override;
    procedure Restore;   // Counterpart of Minimize (mirrors the VCL TAppData.Restore). Also clears StartMinim — without it, one Minimize would latch StartMinim=TRUE in the INI forever and the app would start minimized on every subsequent launch.
    function  RunFileAtStartup(const FilePath: string; Active: Boolean): Boolean;
    function  RunSelfAtStartUp(Active: Boolean): Boolean;
    function  GetAppVersion: string;


   {--------------------------------------------------------------------------------------------------
      FORMS
   --------------------------------------------------------------------------------------------------}
    procedure CreateMainForm  (aClass: TComponentClass; OUT aReference; aAutoState: TAutoState = asPosOnly);
    procedure CreateForm      (aClass: TComponentClass; OUT aReference; aAutoState: TAutoState = asPosOnly; AOwner: TComponent = NIL); overload;
    procedure CreateForm      (aClass: TComponentClass;                 aAutoState: TAutoState = asPosOnly; AOwner: TComponent = NIL); overload;
    procedure CreateEmbedded  (aClass: TComponentClass; OUT aReference; AOwner: TComponent = NIL);   // For forms whose Container is reparented into a host. See TLightForm.CreateEmbedded.
    procedure CreateFormHidden(aClass: TComponentClass; OUT aReference);
    procedure CreateFormModal (aClass: TComponentClass);  // Problem in Android with modal forms!

    function  GetAutoState(Form: TForm): TAutoState;  // Called from TLightForm.Loaded
    procedure ShowModal(aForm: TForm);    // Available everywhere except Android

   {--------------------------------------------------------------------------------------------------
      Others
   --------------------------------------------------------------------------------------------------}
    procedure SetMaxPriority;
    property FormLog: TfrmRamLog read getLogForm;   //Created at runtime, as/if necessary
  end;


VAR                      // To-Do: make sure AppData is unique (make it Singleton)
   AppData: TAppData;    // This obj is automatically freed on app shutdown (via FINALIZATION)


IMPLEMENTATION
USES LightCore, LightFmx.Common.AppData.Form;





{ Warning: We cannot use Application.CreateForm here because this will make the Log the main form! }
constructor TAppData.Create(CONST aAppName: string; CONST WindowClassName: string= ''; MultiThreaded: Boolean= FALSE);
begin
  { App stuff }
  Application.Initialize;                         // Note: Emba: Although Initialize is the first method called in the main project source code, it is not the first code that is executed in a GUI application. For example, in Delphi, the application first executes the initialization section of all the units used by the Application. in modern Delphi (non-.NET), you can remove Application.Initialize without breaking your program. The method is almost empty and no longer plays a critical role in setting up the VCL or application environment. Its historical purpose was to initialize COM and CORBA, but since those are no longer used, the method is effectively redundant.
  Application.Title    := aAppName;
  Application.ShowHint := TRUE;                   // Set later via the HintType property. It is true by default anyway
  FPendingAutoStates   := TList<TPendingAutoState>.Create;

  inherited Create(aAppName, WindowClassName, MultiThreaded);
  FFormLog:= NIL;
  AppDataCore:= Self;                             // This sets the other global variable to self. So we can use both variables. The first one is for non-visual code. The second one (this one) is for visual (GUI) code.

  { All done }
  LogVerb(AppName+ ' started.');
end;


destructor TAppData.Destroy;
VAR i: Integer;
begin
  // Save all still-alive TLightForms NOW, while AppData (and its RamLog) are fully functional.
  // Reason: this destructor runs from this unit's FINALIZATION, but Application-owned forms are
  // destroyed LATER (TPlatformWin.Destroy -> FreeAndNil(Application), in the platform unit's
  // finalization). A never-closed form saved at that point runs FormPreRelease/SaveForm in a dead
  // context: AppData var is already NIL (TIniFileApp.WriteComp asserts on it) and RamLog is freed.
  // Saving here makes the late destruction a no-op (FFormSaved=TRUE guard).
  // 'AppData = Self' guard: Destroy also runs when a constructor RAISES (e.g. erroneous second
  // TAppData creation). The global var does not point to that half-built instance, so we must not
  // let it prematurely save/pre-release the forms of the healthy running instance.
  if (AppData = Self) AND (Screen <> NIL) then
    for i:= Screen.FormCount - 1 downto 0 do
      if Screen.Forms[i] is TLightForm then
        try
          TLightForm(Screen.Forms[i]).saveBeforeExit;
        except
          // Isolate one bad form's FormPreRelease/SaveForm from the REST of this loop and from the
          // code below (FreeAndNil(FFormLog), inherited Destroy -> SaveSettings/FreeAndNil(RamLog)) -
          // an unguarded raise here would skip all of that for a reason unrelated to this one form.
          // RamLog is still alive at this point, so logging (not reraising) is the correct boundary.
          on E: Exception
          do LogError('TAppData.Destroy: saveBeforeExit failed for '+ Screen.Forms[i].Name+ ' ('+ Screen.Forms[i].ClassName+ '): '+ E.ClassName+ ' - '+ E.Message);
        end;

  // Destroy the log form NOW, while RamLog is still alive. Although TApplication owns the form,
  // Application itself is destroyed only in the platform unit's finalization — AFTER this one.
  // If we left the log form to die there, its FormDestroy/TLogViewer.Destroy would call
  // UnregisterLogObserver on the ALREADY FREED RamLog (freed below, in TAppDataCore.Destroy)
  // and TfrmRamLog.SaveSettings would hit a NIL AppData. Freeing it here keeps the whole
  // teardown inside a live-AppData context. TComponent.Destroy unhooks it from Application.
  FreeAndNil(FFormLog);

  FreeAndNil(FPendingAutoStates);
  inherited Destroy;
end;


procedure TAppData.Run;
begin
  // StartMinim remembers application's last state (minimized or not) and minimizes on startup if it was minimized before.
  // Note: FMX defers form creation — Application.MainForm is NIL until RealCreateForms runs INSIDE Application.Run.
  // Calling Minimize here would hit its 'MainForm = NIL' early-exit and silently do nothing.
  // So we defer the Minimize to the message queue: by the time the queued block runs, RealCreateForms has created the main form and Application.MainForm is assigned.
  if StartMinim
  then TThread.ForceQueue(NIL, procedure
       begin
         Minimize;
       end);

  Application.Run;
end;




{-------------------------------------------------------------------------------------------------------------
   CREATE FORMS
--------------------------------------------------------------------------------------------------------------
   W A R N I N G
     On FMX, CreateForm behavior depends on timing:
       - During startup (before Run): Forms are queued and created later by RealCreateForms
         → aReference = NIL (form not created yet)
       - After Run (dynamic): Forms are created immediately
         → aReference <> NIL (form exists)

     IMPORTANT: Application.CreateForm may trigger Loaded SYNCHRONOUSLY (before returning),
     especially when called during another form's FormCreate. This means we must add to the
     pending list BEFORE calling Application.CreateForm, so GetAutoState can find the entry
     when Loaded is called.

     This implementation:
       1. Always adds AutoState to pending list BEFORE calling Application.CreateForm
       2. GetAutoState (called from TLightForm.Loaded) retrieves and removes the entry
       3. If form was created immediately (aReference <> NIL), cleanup any leftover entry
-------------------------------------------------------------------------------------------------------------}

{ Caller MUST pass a variable whose storage outlives Application.Run — typically the
  global FormMain/frmMain declared in the main form's unit.

  Why NOT a local Dummy variable:
    Untyped OUT/VAR params pass the *address* of the caller's storage slot.
    On FMX, Application.CreateForm defers real form construction until RealCreateForms
    runs inside Application.Run. It stores the pointer we hand it and writes the new
    instance back into that address later. If the slot was a local stack var, the
    stack frame is already gone by then — the write lands on garbage memory, the
    global form variable stays NIL, and downstream code (incl. pending-autostate
    bookkeeping that relies on the eventual write) breaks with a "Form was not
    created via AppData.CreateForm" exception.
    Nothing to do with interfaces/refcounting — pure pointer-lifetime issue.

  Therefore: no Dummy-based overload. Always pass the real global var. }
procedure TAppData.CreateMainForm(aClass: TComponentClass; OUT aReference; aAutoState: TAutoState = asPosOnly);
begin
  Assert(Application.MainForm = NIL, 'MainForm already exists!');
  CreateForm(aClass, aReference, aAutoState);
end;


{ Create secondary forms.
  AOwner: when NIL (default), Application owns the form (standard FMX lifetime).
  Pass a specific owner to control destruction order — e.g. embedded forms owned by their host. }
procedure TAppData.CreateForm(aClass: TComponentClass; OUT aReference; aAutoState: TAutoState = asPosOnly; AOwner: TComponent = NIL);
VAR
  Pending: TPendingAutoState;
  i: Integer;
begin
  // Add to pending list BEFORE creating form.
  // This ensures GetAutoState can find it when Loaded is called during form creation.
  // (Application.CreateForm may trigger Loaded synchronously before returning)
  Pending.ClassName:= aClass.ClassName;
  Pending.AutoState:= aAutoState;
  Pending.QueuedBeforeRun:= Initializing;  // Track if queued before Run (DPR) or created dynamically
  FPendingAutoStates.Add(Pending);

  if AOwner <> NIL
  then TComponent(aReference):= aClass.Create(AOwner)         // Explicit owner — destruction order guaranteed
  else Application.CreateForm(aClass, aReference);            // Reference may be NIL if form creation is deferred

  // If form was created immediately (reference not nil), AutoState was already
  // set via GetAutoState in Loaded. Clean up any remaining pending entry.
  // Delete the OLDEST matching entry (lowest index) — GetAutoState also consumes the oldest entry first. 
  // Both loops MUST agree on FIFO order, otherwise with two queued forms of the same class GetAutoState consumes one entry and this
  // cleanup deletes the other, leaving a desynced list (one form gets the wrong AutoState, the other raises 'Form was not created via AppData.CreateForm').
  if TObject(aReference) <> NIL then
    for i := 0 to FPendingAutoStates.Count - 1 do
      if FPendingAutoStates[i].ClassName = aClass.ClassName then
      begin
        FPendingAutoStates.Delete(i);
        Break;
      end;
  // If reference is NIL, form is queued for later - pending entry remains for GetAutoState
end;


procedure TAppData.CreateForm(aClass: TComponentClass; aAutoState: TAutoState = asPosOnly; AOwner: TComponent = NIL);
VAR Dummy: TForm;
begin
  // SAFETY: during initialization FMX defers form creation — Application.CreateForm stores the ADDRESS
  // of the local Dummy and RealCreateForms writes the new instance into that dead stack slot later,
  // corrupting whatever occupies the stack by then. A debug-only Assert is not enough for silent
  // memory corruption, so this must be a hard raise that also survives release builds.
  if Initializing
  then RAISE Exception.Create('CreateForm (2-param) called during initialization — use the 4-param overload with a GLOBAL form variable and check for NIL before Show');
  Dummy:= NIL;   // Must init: the 'if TObject(aReference)<>NIL' test in the 4-param overload must not read an uninitialized stack slot.
  CreateForm(aClass, Dummy, aAutoState, AOwner);
  // After Run, FMX creates forms synchronously — Dummy is always assigned here. The raise above guarantees we never reach this in deferred-creation mode.
  if Assigned(Dummy)
  then Dummy.Show;
end;


{ Companion to TLightForm.CreateEmbedded. Use when the caller will follow up with form.EmbedIn(Container, host). 
  aClass MUST descend from TLightForm.
  AOwner: 
   pass the host form when destruction order matters (e.g. tab-child of another form); 
   pass NIL for top-level singletons — the form is then owned by Application and freed at app shutdown if CloseEmbedded didn't already free it. }
procedure TAppData.CreateEmbedded(aClass: TComponentClass; OUT aReference; AOwner: TComponent = NIL);
VAR EffectiveOwner: TComponent;
begin
  Assert(aClass.InheritsFrom(TLightForm), 'CreateEmbedded: aClass must descend from TLightForm. Got: ' + aClass.ClassName);

  // Default to Application as owner — matches the prior `AppData.CreateForm(..., asNone)` (no AOwner) behavior
  // where the form ended up in Application.Components and was freed at shutdown.
  if AOwner <> NIL
  then EffectiveOwner:= AOwner
  else EffectiveOwner:= Application;

  // Direct construction (synchronous) via TLightForm.CreateEmbedded — bypasses Application.CreateForm's
  // pending queue. AfterConstruction fires AFTER the constructor chain returns and reads FEmbedded.
  TLightForm(aReference):= TLightFormClass(aClass).CreateEmbedded(EffectiveOwner);
end;


// Show this form modal. On Android, we fall back to non-modal because Android is crappy.
// Forms with AutoState=asNone have no saved position — center them on screen.
procedure TAppData.ShowModal(aForm: TForm);
begin
  if (aForm is TLightForm) and (TLightForm(aForm).AutoState = asNone)
  then aForm.Position:= TFormPosition.MainFormCenter;

  if TEST_MODE then EXIT;  // Unit tests: bypass the (blocking) modal display — same contract as the VCL CreateFormModal overloads. See TAppDataCore.TEST_MODE.

  {$IFDEF ANDROID}
    aForm.Show; // Modal forms not supported on Android!
  {$ELSE}
    aForm.ShowModal;
  {$ENDIF}
end;


{ Retrieves AutoState for queued forms (from pending list by class name).
  For immediate forms, AutoState is already set via property in CreateForm/CreateMainForm.
  This method is called from TLightForm.Loaded when FAutoState = asUndefined }
function TAppData.GetAutoState(Form: TForm): TAutoState;
VAR
  i: Integer;
  {$IFDEF DEBUG}
  WasQueuedBeforeRun: Boolean;
  {$ENDIF}
begin
  // Search pending list by class name (for queued forms only)
  for i:= 0 to FPendingAutoStates.Count - 1 do
    if FPendingAutoStates[i].ClassName = Form.ClassName then
    begin
      Result:= FPendingAutoStates[i].AutoState;
      {$IFDEF DEBUG}
      WasQueuedBeforeRun:= FPendingAutoStates[i].QueuedBeforeRun;
      {$ENDIF}
      FPendingAutoStates.Delete(i);  // Remove this entry

      {$IFDEF DEBUG}
      // Log if form was created from pending queue (queued in DPR before Run) or created instantly (dynamically during/after Run)
      if WasQueuedBeforeRun
      then LogVerb('Form created from pending queue: ' + Form.ClassName)
      else LogVerb('Form created instantly: ' + Form.ClassName);
      {$ENDIF}

      EXIT;
    end;

  RAISE Exception.Create('Form was not created via AppData.CreateForm()!' + CRLF +
                         'Form: ' + Form.Name + ' (' + Form.ClassName + ')' + CRLF + CRLF +
                         'Use: AppData.CreateForm(' + Form.ClassName + ', MyForm, asPosOnly);' + CRLF +
                         'Or: MyForm := ' + Form.ClassName + '.Create(nil, asPosOnly);');
end;


{ Create secondary form. The form's visibility depends on its Visible property at design-time. }
procedure TAppData.CreateFormHidden(aClass: TComponentClass; OUT aReference);
begin
  CreateForm(aClass, aReference);
end;


{ Create secondary form and show it modal.
  Warning: On Android, modal forms are not supported - falls back to non-modal Show. }
procedure TAppData.CreateFormModal(aClass: TComponentClass);
VAR aReference: TForm;
begin
  // Same deferred-creation hazard as the 2-param CreateForm: during initialization the form is only
  // queued (aReference stays NIL, and RealCreateForms would later write into this dead stack slot).
  if Initializing
  then RAISE Exception.Create('CreateFormModal called during initialization — create the form after AppData.Run started the message loop');
  aReference:= NIL;
  CreateForm(aClass, aReference);
  Assert(aReference <> NIL, 'CreateFormModal: Form was not created!');
  ShowModal(aReference);
end;


// TfrmRamLog is owned by TApplication but freed early, in TAppData.Destroy (see comment there).
// Warning: during Initializing (before Run), FMX defers form creation — FFormLog stays NIL until
// RealCreateForms runs, so this returns NIL if accessed that early.
function TAppData.getLogForm: TfrmRamLog;
begin
  Assert(RamLog <> NIL, 'RamLog not created!');

  if FFormLog = NIL
  then CreateForm(TfrmRamLog, FFormLog, asPosOnly);  // Use CreateForm instead of direct queue access
  Result:= FFormLog;
end;



{--------------------------------------------------------------------------------------------------
   SELF START UP
--------------------------------------------------------------------------------------------------}
{Summary

    macOS  : Uses launchctl to manage startup items.
    Android: Uses broadcast receivers to trigger actions at boot.
    Linux  : Uses .desktop files in the ~/.config/autostart directory to manage startup items.

    To-Do: Ensure that we handle permissions and platform-specific requirements properly. For example, on Android, we need to declare the appropriate permissions in the manifest file, and on Linux, ensure the .desktop file has the correct permissions. }

{ Run THIS application at Windows startup }
function TAppData.runSelfAtStartUp(Active: Boolean): Boolean;
begin
  Result:= RunFileAtStartUp(ParamStr(0), Active);
end;


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
      Reg.RootKey:= HKEY_CURRENT_USER;                                                { This is set by default by the constructor }
      if Reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Run', TRUE) then
        begin
          if Active
          then Reg.WriteString(ExtractOnlyName(FilePath),'"'+ FilePath + '"')         { I got once an error here: ERegistryexception-Failed to set data for 'App-Name2'. Probably cause by an anti-virus }
          else Reg.DeleteValue(ExtractOnlyName(FilePath));
          Reg.CloseKey;
          Result:= TRUE;
        end;
    FINALLY
      FreeAndNil(Reg);
    END;
  except
    on ERegistryException do
      Result:= FALSE;  { Antivirus or policy blocked the registry write — not fatal }
    on EOSError do
      Result:= FALSE;  { OS-level access denied — not fatal }
  END;
end;
{$ENDIF}


{$IFDEF MACOS}
function TAppData.RunFileAtStartup(const FilePath: string; Active: Boolean): Boolean;
var
  LaunchAgentPath: string;
  LaunchAgentContent: TStringList;
begin
  Result:= False;
  LaunchAgentPath:= TPath.Combine(TPath.GetHomePath, 'Library/LaunchAgents/com.' + AppName + '.startup.plist');

  if Active then
  begin
    LaunchAgentContent:= TStringList.Create;
    try
      LaunchAgentContent.Add('<?xml version="1.0" encoding="UTF-8"?>');
      LaunchAgentContent.Add('<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">');
      LaunchAgentContent.Add('<plist version="1.0">');
      LaunchAgentContent.Add('<dict>');
      LaunchAgentContent.Add('  <key>Label</key>');
      LaunchAgentContent.Add('  <string>com.' + AppName + '.startup</string>');
      LaunchAgentContent.Add('  <key>ProgramArguments</key>');
      LaunchAgentContent.Add('  <array>');
      LaunchAgentContent.Add('    <string>' + FilePath + '</string>');
      LaunchAgentContent.Add('  </array>');
      LaunchAgentContent.Add('  <key>RunAtLoad</key>');
      LaunchAgentContent.Add('  <true/>');
      LaunchAgentContent.Add('</dict>');
      LaunchAgentContent.Add('</plist>');
      LaunchAgentContent.SaveToFile(LaunchAgentPath);
      // Path may contain non-ASCII chars (accented username); UnicodeString → PAnsiChar
      // cast just reinterprets bytes — round-trip via UTF8String for proper encoding.
      var Cmd: UTF8String;
      Cmd:= UTF8String('launchctl load "' + LaunchAgentPath + '"');
      _system(MarshaledAString(Cmd));
      Result:= True;
    finally
      FreeAndNil(LaunchAgentContent);
    end;
  end
  else
  begin
    if FileExists(LaunchAgentPath) then
    begin
      var Cmd: UTF8String;
      Cmd:= UTF8String('launchctl unload "' + LaunchAgentPath + '"');
      _system(MarshaledAString(Cmd));
      DeleteFile(LaunchAgentPath);
      Result:= True;
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
function TAppData.RunFileAtStartup(const FilePath: string; Active: Boolean): Boolean;
var
  AutostartPath: string;
  DesktopEntry: TStringList;
begin
  Result:= False;
  // Per-app autostart filename — previously hardcoded 'com.myapp.desktop' so every
  // LightSaber app overwrote the same file. AppName makes each app unique.
  AutostartPath:= TPath.Combine(TPath.GetHomePath, '.config/autostart/' + AppName + '.desktop');

  if Active then
  begin
    DesktopEntry:= TStringList.Create;
    try
      DesktopEntry.Add('[Desktop Entry]');
      DesktopEntry.Add('Type=Application');
      DesktopEntry.Add('Exec=' + FilePath);
      DesktopEntry.Add('Hidden=false');
      DesktopEntry.Add('NoDisplay=false');
      DesktopEntry.Add('X-GNOME-Autostart-enabled=true');
      DesktopEntry.Add('Name=' + AppName);
      DesktopEntry.Add('Comment=Start ' + AppName + ' at login');
      DesktopEntry.SaveToFile(AutostartPath);
      Result:= True;
    finally
      FreeAndNil(DesktopEntry);
    end;
  end
  else
  begin
    if FileExists(AutostartPath) then
    begin
      DeleteFile(AutostartPath);
      Result:= True;
    end;
  end;
end;
{$ENDIF}


{--------------------------------------------------------------------------------------------------
   APPLICATION CONTROL
--------------------------------------------------------------------------------------------------}
procedure TAppData.Minimize;
begin
  if Application.MainForm = NIL then EXIT;

  var WindowService: IFMXWindowService;
  if TPlatformServices.Current.SupportsPlatformService(IFMXWindowService, IInterface(WindowService))
  then WindowService.SetWindowState(Application.MainForm, TWindowState.wsMinimized);
  StartMinim:= TRUE;
end;


{ Bring the application back to screen (counterpart of Minimize — call it from tray/restore handlers).
  Clears StartMinim so the app does not auto-minimize at the next startup (same contract as the VCL TAppData.Restore). }
procedure TAppData.Restore;
begin
  if Application.MainForm = NIL then EXIT;

  var WindowService: IFMXWindowService;
  if TPlatformServices.Current.SupportsPlatformService(IFMXWindowService, IInterface(WindowService))
  then WindowService.SetWindowState(Application.MainForm, TWindowState.wsNormal);
  Application.MainForm.Show;
  Application.MainForm.BringToFront;
  StartMinim:= FALSE;
end;


{ Set this process to maximum priority. Usefull when measuring time }
procedure TAppData.SetMaxPriority;
begin
  {$IFDEF MSWINDOWS}
  SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS); //  https://stackoverflow.com/questions/13631644/setthreadpriority-and-setpriorityclass
  {$ENDIF}
end;


{ Note: In FMX the forms don't have a hint.
 Hints are supported on Windows and macOS only.
 Application.ShowHint = Global setting. Applies also to actions.
 https://docwiki.embarcadero.com/RADStudio/Athens/en/Using_Hints_to_Show_Contextual_Help_in_a_FireMonkey_Application }
procedure TAppData.setHintType(const aHintType: THintType);
begin
  FHintType:= aHintType;
  Application.ShowHint:= aHintType > htOff;
end;


function TAppData.GetAppVersion: string;
var AppService: IFMXApplicationService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationService, AppService)
  then Result := AppService.AppVersion
  else Result := '';
end;


INITIALIZATION
// Hint: We could create AppData here but the Initialization sections are executed in random order.

FINALIZATION
begin
  AppData.Free;   // DON'T use FreeAndNil here because it will set the AppData variable to Nil too early. We still need the variable in the destructors of the forms.
  AppData:= NIL;
  AppDataCore:= NIL;  // SAME object as AppData (assigned in TAppData.Create). Without this, every 'if AppDataCore <> NIL' guard in LightCore (StreamBuff, Download, Graph, ...) passes on a DANGLING pointer during late shutdown (forms owned by Application are destroyed after this finalization).
end;


end.
