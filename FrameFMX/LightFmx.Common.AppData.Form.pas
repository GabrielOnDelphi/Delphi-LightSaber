UNIT LightFmx.Common.AppData.Form;

{=============================================================================================================
   2026.01.31
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

         Key rules:
         - Embedded forms must use AutoState = asNone (enforced by assertion in EmbedIn).
         - FormCloseQuery does NOT fire for embedded forms (FMX limitation).
           Override FormCloseQueryEmbedded to block closing (e.g. during AI tasks or validation).
         - Virtual keyboard padding is handled by the host form; embedded forms skip it (see HandleVKStateChange).
         - Use TThread.ForceQueue(nil, ...) for deferred destruction after button clicks.

         Known issue (Android):
           TThread.ForceQueue may silently fail to execute on Android in some call depths.
           If this occurs, use a TTimer with Interval=0 as a fallback.
           See: https://en.delphipraxis.net/topic/14705-tthreadforcequeue-inconsistency-on-android/

=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Classes, System.UITypes, System.IniFiles, System.Math, System.Messaging, System.Types,
  FMX.Forms, FMX.Types, FMX.StdCtrls, FMX.Controls, FMX.Controls.Presentation,
  LightCore.AppData, LightCore.Platform, LightFmx.Common.IniFile;

TYPE
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
    Saved: Boolean;
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

    constructor Create(AOwner: TComponent; aAutoState: TAutoState); reintroduce; overload; virtual;
    procedure AfterConstruction; override;
    function CloseQuery: Boolean; override;

    procedure FormPreRelease; virtual;
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

  Showhint := TRUE;
  Saved    := FALSE;
  AutoState:= aAutoState;
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
  if NOT AppData.StartMinim
  AND Visible
  then Show;

  // For virtual keyboard
  FVKSubscriptionId:= TMessageManager.DefaultManager.SubscribeToMessage(TVKStateChangeMessage, HandleVKStateChange);

  if Assigned(FOnAfterCtur) then FOnAfterCtur(Self);
end;


// WARNING: In FMX, Loaded is called before FormCreate!
procedure TLightForm.Loaded;
begin
  Position:= TFormPosition.Designed;

  if Self=Application.MainForm
  then MainFormCaption('Initializing...');

  inherited Loaded;

  // Font, snap, alpha
  SetGuiProperties(Self);

  // Show app name
  if Self=Application.MainForm
  then MainFormCaption('');

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
  TAppDataCore.EndInitialization;
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
  if NOT Saved
  AND NOT AppData.Initializing then
  begin
    try
      FormPreRelease;

      if AutoState > asNone     // Give the user the option not to save the form
      then SaveForm;
    finally
      Saved:= TRUE;             // Make sure it is put to true even on accidents, otherwise we might call it multiple times.
    end;
  end;
end;


{ If we close the main application window, secondary forms are often destroyed directly by Application without DoClose being called. }
destructor TLightForm.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TVKStateChangeMessage, FVKSubscriptionId);
  saveBeforeExit; // Try to save if we haven't already
  inherited;
end;


{ Called ONLY once, when Saved = False }
procedure TLightForm.FormPreRelease;
begin
  // Give user a chance to call its own finalization code (guaranteed once)
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
       MessageError('Cannot save INI file: '+ IniFile.FileName);
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
