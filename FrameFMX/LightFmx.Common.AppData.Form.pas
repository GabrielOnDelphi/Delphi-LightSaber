UNIT LightFmx.Common.AppData.Form;

{=============================================================================================================
   2026.04.30
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   DESCRIPTION

     By deriving your forms from TLightForm they gain the ability to save to disk their:
       * size
       * position
       * controls (checkboxes, radiobuttons, etc)
     When the application starts again, all the above properties are restored automagically.

--------------------------------------------------------------------------------------------------------------
   HOW TO USE IT?

      1. Change the .DPR file as shown in the demo below (use TAppData.CreateForm to create your forms).
      2. When you create a new form in Delphi, use TLightForm as ancestor:
        2a. In the .PAS file, change the declaration of your form from "TForm" to "TLightForm".
        2b. In the .FMX file, change the declaration of your form from "object Form1: TForm" to "inherited Form1: TForm". Only this way, all the visual "goodies" in TLightForm will appear in your form editor.

      Full demos here: LightSaber\Demo\FMX\Template - Minimal app\FMX_MinimalApp.dpr

--------------------------------------------------------------------------------------------------------------

   DETAILS

     Events execution order:

        TAppData.CreateMainForm
          TLightForm.Loaded
            TLightForm.LoadForm
              TForm1.FormCreate

        TForm1.FormPreRelease
           TLightForm.FormPreRelease
             TLightForm.SaveForm

     More about this here:
        LightSaber\FrameFMX\FMX form event order.txt


     FormPreRelease

        The TLightForm.FormPreRelease method, unlike other events, is guaranteed to be executed once and only once when the form is closed.
        This is your nice chance to perform clean up code.


     Self saving forms

         Using SaveForm/LoadForm, a form can save its size and position to disk.


     Embedded forms

         Forms can be embedded into a host form by calling EmbedIn(Container, ParentControl).
         This reparents the form's Container layout into the host and sets FEmbedded:= TRUE.

         AppData.CreateEmbedded(TFooForm, FInstance, AOwner)
         This sets FEmbedded and AutoState=asNone BEFORE the inherited constructor runs, so:
           - AfterConstruction skips its auto-Show (prevents flash of a top-level window before EmbedIn reparents the Container into the host).
           - Loaded sees AutoState <> asUndefined and skips AppData.GetAutoState (which would raise because CreateEmbedded bypasses CreateForm's pending queue).
           - The VK subscription still registers.

         Key rules:
         - Embedded forms must use AutoState = asNone (enforced by assertion in EmbedIn). CreateEmbedded sets it for you.
         - FormCloseQuery does NOT fire for embedded forms (FMX limitation).
           Override FormCloseQueryEmbedded to block closing (e.g. during AI tasks or validation).
         - Virtual keyboard padding is handled by the host form; embedded forms skip it (see HandleVKStateChange).
         - Use TThread.ForceQueue(nil, ...) for deferred destruction after button clicks.

         TThread.ForceQueue(NIL, ...) on Android:
           ForceQueue(NIL, ...) is reliable as long as the queuing code does not block the main thread before the message loop resumes. The Android FMX loop drains the sync queue (CheckSynchronize) on every pump, and TThread.ForceQueue has no platform-specific branch — verified in System.Classes.pas / FMX.Platform.Android.pas.
           One unverified report (delphipraxis.net topic 14705, Delphi 12.3) describes
           ForceQueue callbacks not firing on Android; it was never reproduced and never root-caused — an Embarcadero MVP on that thread attributed it to the main thread being overloaded by a long synchronous run ("Skipped frames" in logcat), not to a ForceQueue defect. 
           So: don't do heavy synchronous work between the ForceQueue call and the loop resuming. Passing NIL (not a TThread instance) as the first parameter is required — a TThread arg makes the entry get dropped when that thread is freed.

=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Classes, System.UITypes, System.IniFiles, System.Math, System.Messaging, System.Types,
  FMX.Forms, FMX.Types, FMX.StdCtrls, FMX.Controls, FMX.Controls.Presentation,
  LightCore.AppData, LightCore.Platform, LightFmx.Common.IniFile;

TYPE
  TLightForm = class;
  TLightFormClass = class of TLightForm; 

  TLightForm = class(TForm)
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure btnOsBackClick(Sender: TObject);
  private
    FOnAfterCtur: TNotifyEvent;
    FCloseOnEscape: Boolean;
    FVKSubscriptionId: TMessageSubscriptionId;
    procedure SetGuiProperties(Form: TForm);
    procedure HandleVKStateChange(const Sender: TObject; const M: TMessage);
  protected
    FEmbedded: Boolean;    { TRUE when form's layout is reparented into another form (embedded mode) }
    FFormSaved: Boolean;   { TRUE once saveBeforeExit has run — prevents double-save on shutdown }
    procedure saveBeforeExit;
    procedure CreateToolbar;
    { Called by FormKeyUp when the user presses Back (Android) or Escape (desktop).
      Override in the main form to close embedded forms before the default behavior.
      Return TRUE if handled; FALSE to let the default proceed (moveTaskToBack / Close). }
    function HandleBackButton: Boolean; virtual;

    procedure FormKeyPress(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState); // We can use this later in the destructor to know how to save the form: asPosOnly/asFull
    procedure Loaded; override;
    procedure DoClose(var Action: TCloseAction); override;
  public
    AutoState      : TAutoState;    // Controls form save/restore behavior (asNone, asPosOnly, asFull)
    TopBar         : TToolBar;
    lblToolBarCapt : TLabel;
    btnOsBack      : TSpeedButton;
    btnOsNext      : TSpeedButton;
    AfterClose     : TProc;         { Optional callback fired once in FormPreRelease. Needed on Android where ShowModal is non-blocking, so callers that care about side effects get notified. Cleared after firing. }

    constructor Create(AOwner: TComponent; aAutoState: TAutoState); reintroduce; overload; virtual;
    constructor CreateEmbedded(AOwner: TComponent); reintroduce; overload; virtual;   { Use this when the caller will reparent Container into a host via EmbedIn. Suppresses the auto-Show in AfterConstruction (which would briefly flash a separate window before the host pulls out the inner layout) and still registers the VK subscription. Equivalent to Create(AOwner, asNone) but with FEmbedded pre-set so AfterConstruction can see it. }
    procedure AfterConstruction; override;
    function CloseQuery: Boolean; override;

    procedure FormPreRelease; virtual;
    procedure FormPostInitialize; virtual;   { Runs once after the form is fully loaded AND the message loop is pumping (first paint done). Override for startup work that should not block the window from appearing — e.g. loading a large file. FMX counterpart of the VCL WM_POSTINIT hook. }
    procedure ShowModal; reintroduce;

    procedure LoadForm; virtual;
    procedure SaveForm; virtual;
    procedure EmbedIn(ALayout: TControl; AParent: TFmxObject);    { Marks form as embedded and reparents ALayout into AParent (Client-aligned). }

    { Embedded equivalent of FormCloseQuery. Returns TRUE if this form can be closed/navigated away from.
      Override to block closing (e.g. while an AI task is running or mandatory fields are empty).
      Default: asserts the form is embedded, then returns TRUE. }
    function FormCloseQueryEmbedded: Boolean; virtual;

    destructor Destroy; override;

    procedure MainFormCaption(aCaption: string);
    procedure MaximizeVertically;
  published
    property CloseOnEscape: Boolean  read FCloseOnEscape  write FCloseOnEscape;        // Close this form when the Esc key is pressed
    property OnAfterConstruction: TNotifyEvent read FOnAfterCtur write FOnAfterCtur;   // Unfortunatelly this won't appears in the object inspector
  end;


IMPLEMENTATION   {$R *.fmx}
USES
  FMX.Platform, FMX.VirtualKeyboard,
  {$IFDEF ANDROID}
  Androidapi.Helpers, Androidapi.JNI.App,
  {$ENDIF}
  LightFmx.Common.AppData, LightFmx.Common.CenterControl, LightFmx.Common.Dialogs;


{-------------------------------------------------------------------------------------------------------------
   CREATE
-------------------------------------------------------------------------------------------------------------}
constructor TLightForm.Create(AOwner: TComponent; aAutoState: TAutoState);
begin
  inherited Create(AOwner);

  Showhint   := TRUE;
  FFormSaved := FALSE;
  AutoState  := aAutoState;
end;


{ FEmbedded and AutoState MUST be set BEFORE inherited Create.
  Reason: FMX's TCommonCustomForm.Create reads the .fmx resource inside the inherited chain and fires Loaded before returning. Loaded checks `if AutoState = asUndefined then AutoState:= AppData.GetAutoState(Self)`,
  and GetAutoState raises if the class is not in the pending queue (which it isn't for CreateEmbedded — we bypass AppData.CreateForm's queueing).
  Pre-setting AutoState=asNone makes Loaded skip the lookup.

  FEmbedded is set early so AfterConstruction (fired by the runtime AFTER the
  entire constructor chain returns) sees TRUE and skips its auto-Show. }
constructor TLightForm.CreateEmbedded(AOwner: TComponent);
begin
  FEmbedded:= TRUE;
  AutoState:= asNone;          // must be set before inherited — see Loaded (the asUndefined branch)
  inherited Create(AOwner);
  Showhint   := TRUE;
  FFormSaved := FALSE;
end;


{ Marks this form as embedded and reparents ALayout (e.g. layRoot) into AParent (e.g. a TTabItem).
  Centralizes the 3 steps that every embedded form needs: FEmbedded flag + reparent + align.
  Embedded forms must use asNone — SaveForm writes position/size which is meaningless for embedded layouts. }
procedure TLightForm.EmbedIn(ALayout: TControl; AParent: TFmxObject);
begin
  Assert(AutoState = asNone, ClassName + '.EmbedIn: embedded forms must use AutoState = asNone (passed to CreateForm)');
  FEmbedded:= TRUE;
  ALayout.Parent:= AParent;
  ALayout.Align:= TAlignLayout.Client;
end;


procedure TLightForm.AfterConstruction;
begin
  inherited;

  // Ensure form is visible on first run (not off-screen from previous session with different monitor setup)
  if AppData.RunningFirstTime
  then EnsureFormVisibleOnScreen(Self);

  // Don't show the form if StartMinim is active (app starts minimized)
  // Note: In FMX, CreateForm does not create forms immediately - it queues them for RealCreateForms
  // Skip Show for forms created via CreateEmbedded — the host pulls out Container via EmbedIn, and showing the bare form first would flash an empty window between construction and reparent.
  if NOT AppData.StartMinim
  AND Visible
  AND NOT FEmbedded
  then Show;

  // For virtual keyboard
  FVKSubscriptionId:= TMessageManager.DefaultManager.SubscribeToMessage(TVKStateChangeMessage, HandleVKStateChange);

  if Assigned(FOnAfterCtur) then FOnAfterCtur(Self);
end;


// WARNING: In FMX, Loaded is called before FormCreate!
procedure TLightForm.Loaded;
begin
  Position:= TFormPosition.Designed;

  inherited Loaded;

  // Font, snap, alpha
  SetGuiProperties(Self);

  // Note: we CANNOT set the MainForm caption here. FMX assigns Application.MainForm only AFTER the constructor (and thus Loaded) returns — see RealCreateForms in FMX.Forms.pas. During Loaded, Application.MainForm is still NIL, so any `Self = Application.MainForm` test here is always FALSE. The caption is set in the deferred ForceQueue block at the end of this method, where MainForm is valid.

  // Load form
  // Limitation: At this point we can only load "standard" Delphi components. Loading of our Light components can only be done in Light_FMX.Visual.INIFile.pas -> TIniFileVCL

  if AutoState = asUndefined  // Only retrieve if AutoState wasn't set in Create
  then AutoState:= AppData.GetAutoState(Self);  // Raises exception if not found

  // Load form if AutoState requests it
  if AutoState <> asNone
  then LoadForm;

  // Mark initialization complete so saveBeforeExit can save form state.
  // The VCL version calls EndInitialization in LightVcl.Visual.AppDataForm.pas.

  // WARNING: Can't check Self=Application.MainForm here because FMX assigns MainForm AFTER the constructor returns, so it's still nil during Loaded.
  // Defer via ForceQueue: when several forms are queued (CreateMainForm + CreateForm),
  // FMX's RealCreateForms constructs them sequentially and each fires Loaded. Calling
  // EndInitialization synchronously here flips Initializing=FALSE on the FIRST form,
  // so any LoadForm side-effect raise in a LATER form would let saveBeforeExit persist
  // half-constructed state. Posting to the message queue defers the flag-flip until
  // RealCreateForms has returned and all queued forms are fully loaded.
  // EndInitialization is idempotent (just sets a Boolean), so multiple posts are harmless.
  //
  // FormPostInitialize runs in the SAME queued callback, AFTER EndInitialization, so
  // user startup code sees Initializing=FALSE and runs only once the message loop is
  // pumping (window already painted). One queued block per TLightForm — FIFO ordering
  // keeps EndInitialization before FormPostInitialize. This is the deferral the VCL
  // frame gets from PostMessage(WM_POSTINIT); FMX forms have no HWND, so ForceQueue is
  // the cross-platform equivalent.
  TThread.ForceQueue(NIL, procedure
    begin
      TAppDataCore.EndInitialization;
      // Set the main-form caption here (not in the synchronous Loaded body): by the
      // time this queued block runs, RealCreateForms has returned and Application.MainForm
      // is assigned, so the Self=Application.MainForm test is meaningful.
      if Self = Application.MainForm
      then MainFormCaption('');
      FormPostInitialize;
    end);
end;


{ Empty by default. Override in a descendant form to do startup work that should run
  after the window is on-screen (see the declaration comment + Loaded). }
procedure TLightForm.FormPostInitialize;
begin
  // Show app name
  if Self=Application.MainForm then MainFormCaption('');
  
  // Overriding point
end;


// Apply font from MainForm and transparency settings
procedure TLightForm.SetGuiProperties(Form: TForm);
begin
  {$IFDEF FullAppData}
   // Font - copy from MainForm to secondary forms
   if (Form <> Application.MainForm)
   AND (Application.MainForm <> NIL)
   then Form.Font:= Application.MainForm.Font;

   // Form transparency
   Form.AlphaBlendValue:= AppData.Opacity;
   Form.AlphaBlend:= AppData.Opacity < 255;
  {$ENDIF}
end;



// Show this form modal if not running on Android. On Android, we fall back to non-modal
procedure TLightForm.ShowModal;
begin
  AppData.ShowModal(Self);
end;




{-------------------------------------------------------------------------------------------------------------
   CLOSE
-------------------------------------------------------------------------------------------------------------}
procedure TLightForm.DoClose(var Action: TCloseAction);
begin
  inherited DoClose(Action);
  if Action <> TCloseAction.caNone
  then saveBeforeExit;
end;


function TLightForm.CloseQuery: Boolean;
begin
  Result:= inherited CloseQuery;
end;


{ This code is guaranteed to be called ONLY once.
  Tech details: It is enough to put SaveBeforeExit in these two places only: OnCloseQueryand & OnDestroy.
  Details: https://groups.google.com/forum/#!msg/borland.public.delphi.objectpascal/82AG0_kHonU/ft53lAjxWRMJ }
procedure TLightForm.saveBeforeExit;
begin
  if NOT FFormSaved
  AND NOT AppData.Initializing then
  begin
    try
      FormPreRelease;

      if AutoState > asNone     // Give the user the option not to save the form
      then SaveForm;
    finally
      FFormSaved:= TRUE;        // Make sure it is put to true even on accidents, otherwise we might call it multiple times.
    end;
  end;
end;


{ If we close the main application window, secondary forms are often destroyed directly by Application without DoClose being called. }
destructor TLightForm.Destroy;
begin
  // Catches the Android non-blocking-Show antipattern: caller did `AppData.ShowModal(F); FreeAndNil(F)`
  // — but on Android ShowModal falls back to non-blocking Show, so FreeAndNil runs before the user
  // sees the form. Use `Action:= caFree` in OnClose, or wire an AfterClose handler instead.
  //
  // Exempted cases (not the antipattern):
  //   - FEmbedded — embedded forms (Container reparented into a host); host manages lifetime,
  //     these are Free'd without Close by design. Most LearnAssist forms default to Visible=TRUE
  //     in their .fmx, so AfterConstruction Shows them before EmbedIn — Visible stays TRUE.
  //   - Released in FormState — legitimate caFree close: FMX's ReleaseForm sets this flag and
  //     queues Free, but DOES NOT set FVisible:=False (only caHide does). So Visible stays TRUE
  //     across the caFree path. Verified in FMX.Forms.pas:3213-3229 (ReleaseForm) vs :3419-3428 (Hide).
  //   - Application = NIL — very late shutdown, FMX globals being freed.
  //   - Application destroying — FMX shutdown destroys secondary forms that weren't closed first.
  //   - MainForm — always Visible at app shutdown.
  // Clause order: cheapest checks first (short-circuit). NIL/destroying guards MUST come before
  // any Application.* deref (Pascal short-circuits left-to-right).
  Assert(NOT Visible
      OR FEmbedded
      OR (TFmxFormState.Released in FormState)
      OR NOT Assigned(Application)
      OR (csDestroying in Application.ComponentState)
      OR (Self = Application.MainForm),
    ClassName + ' destroyed while still visible. ' +
    'Likely cause: caller used FreeAndNil after AppData.ShowModal, which is non-blocking on Android. ' +
    'Use "Action:= caFree" in OnClose, or wire an AfterClose callback.');

  TMessageManager.DefaultManager.Unsubscribe(TVKStateChangeMessage, FVKSubscriptionId);
  saveBeforeExit; // Try to save if we haven't already
  inherited;
end;


{ Called ONLY once, when FFormSaved = False }
procedure TLightForm.FormPreRelease;
VAR
   Cb: TProc;
begin
  // Give user a chance to call its own finalization code (guaranteed once)
  if Assigned(AfterClose) then
    begin
      Cb:= AfterClose;
      AfterClose:= NIL;  // clear before firing to avoid dangling refs across lifetimes
      Cb();
    end;
end;


function TLightForm.HandleBackButton: Boolean;
begin
  Result:= FALSE;
end;


function TLightForm.FormCloseQueryEmbedded: Boolean;
begin
  Assert(FEmbedded, ClassName + '.FormCloseQueryEmbedded called on a non-embedded form. Use FormCloseQuery for normal forms.');
  Result:= TRUE;
end;


{ Handles Escape key to close form when CloseOnEscape is True.
  Note: To use this, connect it to the form's OnKeyDown event in the FMX designer
  or in code: Self.OnKeyDown := FormKeyPress; }
procedure TLightForm.FormKeyPress(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if CloseOnEscape
  AND (Key = vkEscape)
  then Close;
end;


{ Mobile toolbar back button click handler }
procedure TLightForm.btnOsBackClick(Sender: TObject);
begin
  Close;  // This will trigger saveBeforeExit
end;


{ Handles hardware back button on Android and Escape on desktop. Connected via FMX designer.
  Priority chain:
    1. Dismiss virtual keyboard if visible (hardware back only)
    2. Let HandleBackButton handle it (override point for embedded form navigation)
    3. Main form: move app to background (hardware back only, Android)
    4. Secondary forms: close form normally (hardware back only) }
procedure TLightForm.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
var
  VKService: IFMXVirtualKeyboardService;
begin
  if Key = vkHardwareBack then
    begin
      // Step 1: If virtual keyboard is visible, dismiss it first
      if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, VKService)
      AND (TVirtualKeyboardState.Visible in VKService.VirtualKeyBoardState) then
        begin
          VKService.HideVirtualKeyboard;
          Key:= 0;
          EXIT;
        end;

      // Step 2: Let the form handle it (e.g., close embedded forms)
      if HandleBackButton then
        begin
          Key:= 0;
          EXIT;
        end;

      // Step 3: Main form - move to background instead of closing (prevents activity destruction/restart)
      {$IFDEF ANDROID}
      if Self = Application.MainForm then
        begin
          TAndroidHelper.Activity.moveTaskToBack(True);
          Key:= 0;
          EXIT;
        end;
      {$ENDIF}

      // Step 4: Secondary forms - close normally
      Close;
      Key:= 0;
    end
  else
    // Escape: desktop equivalent of back navigation for embedded forms only.
    // CloseOnEscape (when no embedded form is active) is handled separately in FormKeyPress.
    if Key = vkEscape then
      begin
        if HandleBackButton then begin Key:= 0; EXIT; end;
        if FCloseOnEscape  then begin Close; Key:= 0; end;
      end;
end;


{ Adjusts form padding when the virtual keyboard appears/disappears.
  Uses the Bounds from TVKStateChangeMessage (screen coords) converted to form-local coords.
  Only standalone (non-embedded) forms adjust — embedded forms get the adjustment for free
  through the layout cascade (parent form's padding shrinks all Client-aligned children). }
procedure TLightForm.HandleVKStateChange(const Sender: TObject; const M: TMessage);
var
  VKMsg: TVKStateChangeMessage;
  KBTopLeft: TPointF;
begin
  if FEmbedded then EXIT;

  VKMsg:= TVKStateChangeMessage(M);
  if VKMsg.KeyboardVisible AND (VKMsg.KeyboardBounds.Height > 0) then
    begin
      KBTopLeft:= ScreenToClient(VKMsg.KeyboardBounds.TopLeft);
      Self.Padding.Bottom:= Max(0, ClientHeight - KBTopLeft.Y);
    end
  else
    Self.Padding.Bottom:= 0;
end;



{-------------------------------------------------------------------------------------------------------------
   Load/Save form state to INI file.
   Behavior is controlled by the AutoState property:
     asNone    -> Don't save the form
     asPosOnly -> Save only position (Left/Top, no width/height/WindowState)
     asFull    -> Save position and all supported controls (checkboxes, radiobuttons, etc.)

   Also see LoadForm/SaveForm in LightFmx.Common.IniFile.pas
-------------------------------------------------------------------------------------------------------------}

procedure TLightForm.SaveForm;
VAR IniFile: TIniFileApp;
begin
  if TAppData.Initializing
  AND (Self= Application.MainForm) then
   begin
     if TAppDataCore.RunningHome
     then MessageError('Closing application while still initializing!');
     Exit; // We don't save anything if the start up was improper!
   end;

  Assert(AppData <> NIL, 'AppData is NIL at app shutdown!');
  IniFile:= TIniFileApp.Create(Self.Name);
  TRY
   TRY
     IniFile.SaveForm(Self, AutoState);
   EXCEPT
     ON EIniFileException DO
       begin
         // Log as well as show: SaveForm runs on the shutdown path; the log gives a post-mortem trail.
         // We do NOT re-raise — re-raising here would abort form destruction (DoClose/Destroy path).
         if AppData <> NIL
         then AppData.LogError('Cannot save INI file: '+ IniFile.FileName);
       end;
   END;
  FINALLY
    FreeAndNil(IniFile);
  END;
end;



{ Override this method if you want to do your own loading from INI file. In this case, don't call inherited!

  LoadForm also does:
    * Set the font for all forms to be the same as the font of the MainForm.
    * If the form is out of screen, LoadForm will also bring the form back to screen. }
procedure TLightForm.LoadForm;
VAR
   IniFile: TIniFileApp;
begin
  (*if AppData = NIL then                { If AppData exists, let it deal with the font }
    if (Application.MainForm <> NIL)     { Set font only for secondary forms }
    AND (Self <> Application.MainForm)
    then Self.Font:= Application.MainForm.Font; *)

  IniFile:= TIniFileApp.Create(Self.Name);
  TRY
   TRY
     IniFile.LoadForm(Self, AutoState);
     //CorrectFormPositionScreen [or] CorrectFormPositionDesktop(Self);
   EXCEPT
     ON EIniFileException DO
       if AppData <> NIL
       then AppData.LogWarn('Cannot load INI file: '+ IniFile.FileName);
   END;
  FINALLY
    FreeAndNil(IniFile);
  END;
end;


{ Creates a mobile toolbar with back/next buttons. Only called on mobile platforms. }
procedure TLightForm.CreateToolbar;
begin
  if NOT OsIsMobile then EXIT;

  // Create toolbar
  TopBar:= TToolBar.Create(Self);
  TopBar.Parent:= Self;
  TopBar.Name:= 'TopBar';
  TopBar.Anchors:= [];
  TopBar.Height:= 56;  // Standard Material Design header height
  TopBar.TabOrder:= 0;
  {$IFDEF ANDROID}
  TopBar.Align := TAlignLayout.Top; {$ENDIF}
  {$IFDEF IOS}
  TopBar.Align := TAlignLayout.Bottom; {$ENDIF}

  // Create caption label
  lblToolBarCapt:= TLabel.Create(Self);
  lblToolBarCapt.Parent:= TopBar;
  lblToolBarCapt.Name:= 'lblToolBarCapt';
  lblToolBarCapt.Align:= TAlignLayout.Contents;
  lblToolBarCapt.StyleLookup:= 'toollabel';
  lblToolBarCapt.TextSettings.HorzAlign:= TTextAlign.Center;
  lblToolBarCapt.Text:= Self.Caption; // Automatically grab the Form's caption

  // Create back button
  btnOsBack:= TSpeedButton.Create(Self);
  btnOsBack.Parent:= TopBar;
  btnOsBack.Name:= 'btnOsBack';
  btnOsBack.Align:= TAlignLayout.MostLeft;
  btnOsBack.Width:= 48;
  btnOsBack.StyleLookup:= 'backtoolbutton';
  btnOsBack.OnClick:= btnOsBackClick;

  // Create next button (hidden by default)
  btnOsNext:= TSpeedButton.Create(Self);
  btnOsNext.Parent:= TopBar;
  btnOsNext.Name:= 'btnOsNext';
  btnOsNext.Align:= TAlignLayout.MostRight;
  btnOsNext.Width:= 44;
  btnOsNext.StyleLookup:= 'nexttoolbutton';
  btnOsNext.Visible:= False;
end;




{-------------------------------------------------------------------------------------------------------------
   OTHERS
-------------------------------------------------------------------------------------------------------------}
{ Stretches the form vertically across the current monitor's work area (excludes taskbar).
  On mobile the form is fullscreen anyway, so the call is a no-op. }
procedure TLightForm.MaximizeVertically;
VAR WorkArea: TRectF;
begin
  if OsIsMobile then EXIT;
  WorkArea:= Screen.DisplayFromForm(Self).WorkArea;
  Top    := Trunc(WorkArea.Top);
  Height := Trunc(WorkArea.Height);
end;


// WARNING: FMX: CreateForm does not create the given form immediately. RealCreateForms creates the real forms. So, we cannot access Application.MainForm here.
procedure TLightForm.MainFormCaption(aCaption: string);
begin
  if aCaption = ''
  then aCaption:= AppData.AppName
  else aCaption:= AppData.AppName + ' - ' + aCaption;

  if AppData.RunningHome
  then aCaption:= aCaption+ ' [Running home]';

  if AppData.BetaTesterMode
  then aCaption:= aCaption+ ' [BetaTesterMode]';

  {$IFDEF DEBUG}
    aCaption:= aCaption+ ' [Debug]';
  {$ENDIF}

  Caption:= aCaption;
end;







{

Designtime error:
  Error creating form in FormWhatsApp.fmx: Ancestor for 'TLightForm' not found.

procedure Register;
begin
  RegisterNoIcon([TLightForm]);
end;


initialization
  RegisterNoIcon([TLightForm]);  //todo: do I need this? RegisterComponents('LightSaber', [TLightForm]);

finalization
  UnregisterClass(TLightForm);}

end.
