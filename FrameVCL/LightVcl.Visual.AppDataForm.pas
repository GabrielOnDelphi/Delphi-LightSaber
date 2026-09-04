UNIT LightVcl.Visual.AppDataForm;

{=============================================================================================================
   2026.08.20
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Motivation - Where to initialize own code?

      VCL forms offer no good place where to execute your initialization/finalization code.
      OnCreate is too early, and OnShow may never be called (or called too late) or called multiple times.

      The TLightForm provides two places that (in conjunction with TAppData) offer you two methods that are guaranteed to be executed:
      TMyForm = class(TLightForm)
          protected
            procedure FormPostInitialize; override; // Called after the main form was fully initialized.
            procedure FormPreRelease;
         end;



   How to use it

      Change the declaration of your form from TForm to TLightForm.
      Optionally, if you want to execute your own initialization code, override the LateInitialize (don't forget to call inherited).

      uses LightVcl.Visual.AppDataForm;
      Type
        TYourForm = class(TLightForm)
        protected
        public
          procedure FormPostInitialize;  override;    // Optional
        end;

       procedure TYourForm.FormPostInitialize;
       begin
         inherited FormPostInitialize;

         // Initialize your own code here
       end;

--------------------------------------------------------------------------------------------------------------

  Extra features

      FormPreRelease

         Optionally you can use the FormPreRelease event to execute your code on application shutdown.
         Unlike other events, FormPreRelease is always called, and it is guaranteed to be called once and only once!

         Execution order: FormPreRelease -> FormClose -> FormDestroy

      Self saving forms

         Using SaveForm/LoadForm, a form can save its status (including checkboxes/radio buttons/etc on it))
         to disk on shutdown and resume exaclty from where it left on application startup.

         LoadForm is automatically called by TAppData.CreateForm(). Therefore, you must create all your forms with this method.
         The TLightForm.SaveForm is called automatically when the form closes.

         Override SaveForm/LoadForm if you want to do your own loading/saving (in this case, don't call inherited)!

=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, System.IniFiles, Vcl.Controls, Vcl.Forms,
  LightCore.AppData, LightVcl.Common.Dialogs, LightVcl.Common.CenterControl, LightVcl.Common.IniFile;

CONST
  WM_POSTINIT = WM_APP + 745;   // LEGACY trigger for the post-init step. Nothing in LightSaber posts it any more (see SchedulePostInitialize); kept public because an application might. Both routes run RunPostInitialize, and it executes only once.
  {$IFDEF AUTOPILOT}
  WM_AUTOPILOT_UNGATE = WM_APP + 746;   // Posted by TLightForm.DoShow to drop WS_EX_NOACTIVATE once the window is on screen. See TLightForm.CreateParams.
  {$ENDIF}

TYPE
  TLightForm = class(TForm)
  private
    FCloseOnEscape: Boolean;
    FAutoSaveForm: TAutoState;
    FPostInitDone: Boolean;      { RunPostInitialize must execute exactly once, no matter which of the two triggers arrives (the queued entry, or a legacy WM_POSTINIT posted by an application). }
    procedure WMPostInit(var Msg: TMessage); message WM_POSTINIT;
    procedure QueuedPostInitialize;   { Deferred from SchedulePostInitialize via ForceQueue. A NAMED method (not an anonymous block) so DoDestroy can cancel the pending entry with TThread.RemoveQueuedEvents - which matches on the method. An anonymous TThreadProcedure cannot be removed, and the entry holds Self, so a form freed before the queue drains would be dereferenced (use-after-free). }
    procedure RunPostInitialize;
  protected
    FFormSaved: Boolean;         { TRUE once saveBeforeExit has FULLY run (set in the finally, AFTER FormPreRelease/SaveForm) — prevents double-save on shutdown. FormPreRelease's contract (see its declaration comment) is to observe this as FALSE during its own call, so descendants can gate one-time cleanup on "if NOT FFormSaved then ..." (see Demo\VCL\Template App Full\FormMain.FormPreRelease). }
    FSavingInProgress: Boolean; { TRUE from the moment saveBeforeExit starts working — separate reentrancy guard so a second Close arriving while FormPreRelease/SaveForm pump messages (e.g. a confirmation dialog) cannot re-enter and run FormPreRelease twice. Cannot reuse FFormSaved for this: it must stay FALSE until FormPreRelease returns (see above). }
    procedure Loaded; override;

    {$IFDEF AUTOPILOT}
    procedure CreateParams(VAR Params: TCreateParams); override;   // Autopilot builds only: bring the startup window up WITHOUT taking the keyboard focus. See the implementation.
    procedure DoShow; override;
    procedure SetZOrder(TopMost: Boolean); override;   // Autopilot builds only: BringToFront must not activate while the startup gate is up
    procedure WMAutopilotUnGate(VAR Msg: TMessage); message WM_AUTOPILOT_UNGATE;
    {$ENDIF}

    procedure DoDestroy; override;
    procedure DoClose(VAR Action: TCloseAction); override;
    procedure FormKeyPress(Sender: TObject; var Key: Char);  // We can use this later in the destructor to know how to save the form: asPosOnly/asFull
    procedure WMEndSession(VAR Msg: TWMEndSession); message WM_ENDSESSION;  { Save form state on Windows logoff/shutdown, but only when Msg.EndSession is TRUE (a canceled logoff must not save). The FFormSaved guard in saveBeforeExit prevents a double-save when CloseQuery already ran via WM_QUERYENDSESSION. }
  public
    constructor Create(AOwner: TComponent; AutoSaveForm: TAutoState); reintroduce; overload; virtual;
    function  CloseQuery: boolean; override;

    procedure SchedulePostInitialize;        // Queue FormPostInitialize to run once the message loop is pumping. Called by TAppData.CreateMainForm. See the implementation for why this is NOT a posted window message.
    procedure FormPostInitialize; virtual;   // Takes place after the form was fully created
    procedure FormPreRelease; virtual;       // Takes place before the form is destroyed. It is guaranteed to be called excetly once.
    procedure saveBeforeExit;                // Idempotent (FFormSaved guard). Public so TAppData.Destroy can save all still-open forms while AppData is alive — Application-owned forms are otherwise destroyed AFTER AppData's finalization (in Vcl.Forms' finalization).

    procedure LoadForm; virtual;
    procedure SaveForm; virtual;

    procedure MainFormCaption(const Caption: string);
  published
    property AutoState: TAutoState   read FAutoSaveForm   write FAutoSaveForm;
    property CloseOnEscape: Boolean  read FCloseOnEscape  write FCloseOnEscape;    // Close this form when the Esc key is pressed
  end;


IMPLEMENTATION
USES
  Vcl.StdCtrls,
  LightVcl.Visual.AppData, LightVcl.Visual.INIFile;


constructor TLightForm.Create(AOwner: TComponent; AutoSaveForm: TAutoState);
begin
  inherited Create(AOwner);

  ScreenSnap:= TRUE;
  Position  := poDesigned;  // Without this we cannot restore form's position on screen!
  Showhint  := TRUE;
  FFormSaved:= FALSE;
  FSavingInProgress:= FALSE;

  FAutoSaveForm := AutoSaveForm; // Default value. Can be overriden by AppData.CreateForm
end;


{ Centers the form on the main form's monitor after DFM streaming completes.
  This provides a sensible default position for all TLightForm descendants.
  For forms created via AppData.CreateForm, LoadForm will override this with the saved position. }
procedure TLightForm.Loaded;
begin
  inherited;
  if (Application.MainForm <> NIL) 
  AND (Self <> Application.MainForm)
  then CenterFormOnMainFormMonitor(Self);
end;


{$IFDEF AUTOPILOT}
{ AUTOPILOT builds only.
  Not reachable in Release - the symbol is set in the Debug configuration only.

  Problem: Claude launches the app and drives it over the Autopilot bridge while the user keeps typing somewhere else.
  A normal startup ACTIVATES the new window, so his next keystrokes land in our app.

  WS_EX_NOACTIVATE is the only thing that stops it, and it must be present on the handle BEFORE the window is first shown - which is why this sits in CreateParams and not in the bridge: TAppData.CreateMainForm shows the form itself, and StartBridge only runs after CreateMainForm returns, when the focus is already gone.

  The flag makes the window unusable BY HAND (Microsoft: "does not become the foreground window when the user clicks it"), so UnGateStartupWindows takes it back off at the very end of the post-init step.
  That is the whole add/remove dance: set at handle creation, cleared when startup is over.

  Deliberately NOT adding WS_EX_APPWINDOW next to it.
  Microsoft says a WS_EX_NOACTIVATE window stays off the taskbar "by default", but TCustomForm.CreateParams already sets WS_EX_APPWINDOW when the form owns the taskbar button; forcing it here would give a MainFormOnTaskbar=FALSE app a SECOND taskbar button beside the TApplication one.
  https://learn.microsoft.com/en-us/windows/win32/winmsg/extended-window-styles }


{ Is the startup gate still up?

  NOT simply AppData.Initializing.
  RunPostInitialize calls EndInitialization BEFORE FormPostInitialize, so any window a form opens from its own startup code is created with Initializing already FALSE and would be left ungated - a splash screen, a first-run wizard, an EULA box.
  (Not measured on a real splash: the template's never showed during these runs. The reasoning is from the call order, which IS verified - see RunPostInitialize below.)

  So the gate's lifetime is read off the main form's own window instead: it carries WS_EX_NOACTIVATE from its CreateParams until UnGateStartupWindows clears it, which is the exact span we want.
  No flag variable to keep in sync, and nothing global. }
function StartupGateActive: Boolean;
begin
  if (AppData <> NIL) AND AppData.Initializing
  then EXIT(TRUE);                        // the main form's own handle is being created; MainForm is still NIL

  Result:= (Application.MainForm <> NIL)
       AND Application.MainForm.HandleAllocated
       AND ((GetWindowLong(Application.MainForm.Handle, GWL_EXSTYLE) AND WS_EX_NOACTIVATE) <> 0);
end;


procedure TLightForm.CreateParams(VAR Params: TCreateParams);
begin
  inherited CreateParams(Params);

  if NOT StartupGateActive then EXIT;

  Params.ExStyle:= Params.ExStyle OR WS_EX_NOACTIVATE;

  { The forms are not the whole story.
    With MainFormOnTaskbar=FALSE the TApplication proxy window owns the taskbar button, and TCustomForm.CreateParams has just stripped WS_EX_TOOLWINDOW off it (Vcl.Forms.pas) - so it is a plain, activatable window, and THAT is what takes the foreground.
    Measured on Demo\VCL\Template App Full: both forms came up with WS_EX_NOACTIVATE and the app still stole the focus, through TApplication (exstyle 0x00040100).

    The RTL only protects that window in the OTHER configuration: TApplication.CreateForm and TApplication.UpdateVisible both add WS_EX_NOACTIVATE to it, but each guards on MainFormOnTaskBar (both in Vcl.Forms.pas).
    So we add it exactly where the RTL does not, and UnGateStartupWindows clears it again under the same condition - never touching the flag the RTL owns. }
  if NOT Application.MainFormOnTaskbar
  AND (Application.Handle <> 0)
  then SetWindowLong(Application.Handle, GWL_EXSTYLE, GetWindowLong(Application.Handle, GWL_EXSTYLE) OR WS_EX_NOACTIVATE);
end;


{ The gate has a hole that WS_EX_NOACTIVATE cannot plug on its own, so this closes it.

  TCustomForm.Show ends in BringToFront, and TWinControl.SetZOrder raises the window with
  SetWindowPos(WindowHandle, HWND_TOP, 0,0,0,0, SWP_NOMOVE + SWP_NOSIZE) - note the MISSING
  SWP_NOACTIVATE (Vcl.Controls.pas:13355). That is a request to ACTIVATE, and measurement says it goes
  straight through WS_EX_NOACTIVATE: startup code that calls MainForm.Show a SECOND time - the LightSaber
  template does, from LateInitialization - took the foreground with every window correctly gated.

  While the gate is up we simply do not raise. Nothing is lost: the window is already on top from its own
  show, and the raise is restored the moment startup ends. Only the TopMost direction is skipped;
  SendToBack still works. }
procedure TLightForm.SetZOrder(TopMost: Boolean);
begin
  if TopMost
  AND StartupGateActive then EXIT;

  inherited SetZOrder(TopMost);
end;


{ Lifts the gate off every window that could be carrying it, and off the TApplication proxy.

  The gate lasts for the WHOLE initialization, not just until the first show, and that is a measured
  requirement rather than caution. Demo\VCL\Template App Full calls MainForm.Show a second time from
  LateInitialization (uInitialization.pas), and TCustomForm.Show ends in BringToFront -> SetZOrder ->
  SetWindowPos WITHOUT SWP_NOACTIVATE (Vcl.Controls.pas:13355), which activates. Ungating at the first
  show let exactly that call steal the focus back - measured, FOCUS: STOLEN, with the harness naming
  TMainForm as the thief. The SetZOrder override above is the other half of that answer.

  Only TLightForm instances are touched - they are the only ones CreateParams gates. A form that
  deliberately wants WS_EX_NOACTIVATE for its own reasons is left alone.

  Nothing here can rescue an app that calls SetForegroundWindow (TApplication.BringToFront does,
  Vcl.Forms.pas:13208). Microsoft documents that as the sanctioned way to activate a WS_EX_NOACTIVATE
  window, so it wins over any ex-style. That is an app-level decision, not something the gate can undo. }
procedure UnGateStartupWindows;
VAR i: Integer;
begin
  for i:= 0 to Screen.FormCount-1 do
    if (Screen.Forms[i] is TLightForm)
    AND Screen.Forms[i].HandleAllocated
    then SetWindowLong(Screen.Forms[i].Handle, GWL_EXSTYLE, GetWindowLong(Screen.Forms[i].Handle, GWL_EXSTYLE) AND NOT WS_EX_NOACTIVATE);

  { Undo the TApplication half under exactly the condition that added it, and never otherwise: with
    MainFormOnTaskbar=TRUE the flag on that window belongs to the RTL, which puts it there on purpose
    and expects it to stay. With FALSE the taskbar button IS that window, and a WS_EX_NOACTIVATE window
    does not come to the front when clicked - leaving it on would kill the taskbar button. }
  if NOT Application.MainFormOnTaskbar
  AND (Application.Handle <> 0)
  then SetWindowLong(Application.Handle, GWL_EXSTYLE, GetWindowLong(Application.Handle, GWL_EXSTYLE) AND NOT WS_EX_NOACTIVATE);
end;


{ Put the startup window directly BELOW the window the user is working in, without activating it.

  WHY THIS EXISTS. Nothing else decides where the window goes. TWinControl.CMShowingChanged shows it
  with SWP_NOZORDER (Vcl.Controls.pas:12809-12812), so the show does not touch the z-order at all, and
  the SetZOrder override above deliberately does not raise while the gate is up. What is left is the
  position CreateWindowEx handed out, which is the top of the non-topmost stack. So the window comes up
  OVER whatever the user was working in - it just does not take the keyboard.

  Measured 2026-09-01 on Demo\VCL\Template App Full, with charmap.exe freshly launched and holding the
  foreground: the window landed TWO positions ABOVE the active window on 5 of 5 launches, with the
  foreground correctly left alone every time. Harness: Autopilot for Delphi\_Local info\Issues\
  No focus steal\Measure-LaunchPlacement.ps1.

  That is the whole reported complaint, and it also explains the "and it STAYS on top" half: clicking a
  window that is ALREADY active moves nothing in the z-order, so the user cannot get their own window
  back above ours without first clicking ours and then clicking back.

  SetWindowPos's hWndInsertAfter is documented as "a handle to the window to PRECEDE the positioned
  window in the Z order", so passing the foreground window puts us directly beneath it. SWP_NOACTIVATE
  leaves the keyboard where it is, which is the entire point of the gate.
  https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-setwindowpos }
{ Is this window the desktop or the taskbar? Those are the two anchors that must never be used - see the
  comment at the call site in PlaceBeneathForegroundWindow. }
function IsDesktopOrTaskbar(AWnd: HWND): Boolean;
VAR
  Buf: array[0..63] of Char;   // not named ClassName: that would shadow TObject.ClassName
  Cls: String;
begin
  if GetClassName(AWnd, Buf, Length(Buf)) <= 0 then EXIT(FALSE);

  Cls:= PChar(@Buf[0]);   // GetClassName null-terminates, so the PChar assignment stops at the null

  Result:= SameText(Cls, 'Progman')          // the desktop
        OR SameText(Cls, 'WorkerW')          // the desktop again, when a wallpaper host is present
        OR SameText(Cls, 'Shell_TrayWnd');   // the taskbar
end;


procedure PlaceBeneathForegroundWindow(AForm: TCustomForm);
VAR
  Fg: HWND;
  FgProcId: DWORD;
begin
  if (AForm = NIL) OR NOT AForm.HandleAllocated then EXIT;

  Fg:= GetForegroundWindow;
  if (Fg = 0) OR (Fg = AForm.Handle) then EXIT;      // nothing to anchor to, or it is already us

  { Never anchor to one of our OWN windows. A splash screen, a first-run wizard or a second form of this
    same application is not "the window the user was working in"; inserting beneath one would push the
    main form under our own startup furniture. }
  FgProcId:= 0;
  GetWindowThreadProcessId(Fg, FgProcId);
  if FgProcId = GetCurrentProcessId then EXIT;

  { Never anchor to a TOPMOST window. Microsoft says a window becomes topmost either through
    HWND_TOPMOST or "by setting a window's position in the Z order so that it is above any existing
    topmost windows" - and slotting in directly beneath ONE topmost window does place us above every
    other topmost window. That would pin the application over everything, the exact opposite of what
    this procedure is for. }
  if (GetWindowLong(Fg, GWL_EXSTYLE) AND WS_EX_TOPMOST) <> 0 then EXIT;

  { Never anchor to the DESKTOP or the TASKBAR. This is the guard that keeps the cure from becoming the
    other half of the disease. The desktop sits at the very bottom of the z-order, so inserting beneath
    it would bury the application behind every open window - which is the SECOND defect recorded in
    "No focus steal.md": the same unchanged exe once came up at z=22 of 25, hidden behind everything.
    Trading "on top of the user's window" for "invisible" is not a fix.

    It is not a hypothetical case either: with no other program in front - the app started from a bare
    desktop, or from a shortcut after the user clicked the wallpaper - GetForegroundWindow returns the
    desktop, and that was observed during this investigation. 'Progman' and 'WorkerW' are the two
    desktop window classes; 'Shell_TrayWnd' is the taskbar. When the anchor is one of these there is no
    "user window" to sit under, so leaving the placement alone is the right answer. }
  if IsDesktopOrTaskbar(Fg) then EXIT;

  SetWindowPos(AForm.Handle, Fg, 0, 0, 0, 0, SWP_NOMOVE OR SWP_NOSIZE OR SWP_NOACTIVATE);
end;


{ Safety net for a window still carrying the gate after startup is over - a form whose handle was made
  during startup but which is first shown much later. TfrmRamLog is exactly that: created by
  getGlobalLog, shown only when someone asks for the log.

  Silent while the gate is up: during startup the flag must stay, and UnGateStartupWindows is what
  lifts it. The posted message lands one pass of the message loop later, when the window is already up. }
procedure TLightForm.DoShow;
begin
  inherited DoShow;

  if HandleAllocated
  AND ((GetWindowLong(Handle, GWL_EXSTYLE) AND WS_EX_NOACTIVATE) <> 0)
  AND NOT StartupGateActive
  then PostMessage(Handle, WM_AUTOPILOT_UNGATE, 0, 0);
end;


procedure TLightForm.WMAutopilotUnGate(var Msg: TMessage);
begin
  { Clears the WHOLE gate, not just this window's own flag. DoShow posts this message only once startup
    is over, so anything still carrying the flag at this point is a leftover - the TApplication proxy
    included. CreateParams gates that proxy, but the normal path that clears it (RunPostInitialize)
    requires the MAIN form to be a TLightForm. An application whose main form is a plain TForm would
    never reach that path, and a proxy left with WS_EX_NOACTIVATE has a dead taskbar button. }
  UnGateStartupWindows;
end;
{$ENDIF}


{ Queue the post-init step so it runs once the message loop is pumping. Called by TAppData.CreateMainForm.

  TThread.ForceQueue - NOT PostMessage(WM_POSTINIT), which is what this did until 2026.08.20. A posted
  message belongs to a window HANDLE, and the VCL recreates the main form's handle for several ordinary
  reasons: applying a VCL style, Application.MainFormOnTaskbar changing (TApplication.SetMainFormOnTaskBar
  does FMainForm.Perform(CM_RECREATEWND) - Vcl.Forms.pas:14758), or any other RecreateWnd. The recreation
  DISCARDS the queued message, so FormPostInitialize never fired and the app came up half-initialized with
  nothing raised. Four separate triggers of that one root cause were documented in this repo, each patched
  with its own comment or guard; this removes the root cause instead.

  A ForceQueue entry lives in the RTL queue, which no window owns, so no handle change can lose it. It is
  drained by CheckSynchronize, which TApplication pumps two independent ways: WM_NULL in
  TApplication.WndProc (Vcl.Forms.pas:13086, woken by TApplication.WakeMainThread :14669) and again in
  TApplication.Idle (:14067). This is also what the FMX twin already does - see
  LightFmx.Common.AppData.Form.pas, TLightForm.Loaded. }
procedure TLightForm.SchedulePostInitialize;
begin
  TThread.ForceQueue(NIL, QueuedPostInitialize);
end;


procedure TLightForm.QueuedPostInitialize;
begin
  RunPostInitialize;
end;


{ Legacy entry point - see the WM_POSTINIT declaration. }
procedure TLightForm.WMPostInit(var Msg: TMessage);
begin
  RunPostInitialize;
end;


{ Runs the user initialization code, deferred until the message loop pumps: by now the form has fully
  settled (all pending WM_SIZE/layout messages processed) before user code that might itself pump
  messages (e.g. a modal dialog). }
procedure TLightForm.RunPostInitialize;
begin
  if FPostInitDone then EXIT;   // Two triggers exist (queued entry + legacy WM_POSTINIT). EndInitialization and the user's code must run once.
  FPostInitDone:= TRUE;

  {$IFDEF AUTOPILOT}
  // try..finally, not a posted message: FormPostInitialize is allowed to pump (a modal dialog), and a
  // posted ungate would then be handled in the MIDDLE of it - lifting the gate while startup is still
  // running, which is the bug this whole block exists to avoid. The finally also guarantees that a
  // raise inside FormPostInitialize cannot leave the windows unclickable.
  try
  {$ENDIF}

  if Self = Application.MainForm
  then AppData.EndInitialization;

  // Run user initialization code
  FormPostInitialize;

  { Note: Do NOT reset StartMinim here. The Minimize/Restore methods manage this flag
    at runtime, and FormPreRelease re-syncs it before SaveSettings. Resetting it here
    caused a bug: SaveSettings would persist FALSE even when the user's "Start minimized"
    checkbox was checked, so the early minimize in CreateMainForm would not fire on next startup. }

  if AppData.Translator <> NIL
  then AppData.Translator.LoadTranslation(Self);

  {$IFDEF AUTOPILOT}
  finally
    if Self = Application.MainForm then
      begin
        UnGateStartupWindows;                  // startup is over - hand the windows back to the mouse
        PlaceBeneathForegroundWindow(Self);    // ...and put it where it belongs: under the user's window
      end;
  end;
  {$ENDIF}
end;


procedure TLightForm.FormPostInitialize;
begin
  // This can be overridden by the user to implement initialization after the form is ready
end;


procedure TLightForm.WMEndSession(var Msg: TWMEndSession);
begin
  if Msg.EndSession                { Only save when the session is really ending. On a canceled logoff (EndSession=FALSE) saveBeforeExit would set FFormSaved=TRUE and suppress the real save on the later close. }
  then saveBeforeExit;
  inherited;
end;

procedure TLightForm.DoDestroy;
begin
  TThread.RemoveQueuedEvents(QueuedPostInitialize);   { Cancel the entry queued by SchedulePostInitialize, if it did not run yet (form freed before the message queue drained) - it holds Self. Same guard as the FMX twin. }
  saveBeforeExit;
  inherited;
end;

procedure TLightForm.DoClose(var Action: TCloseAction);
begin
  inherited DoClose(Action);
  if Action = TCloseAction.caFree then saveBeforeExit;
end;


function TLightForm.CloseQuery: Boolean;
begin
  // Ask FIRST (inherited fires OnCloseQuery), save only if the close is allowed.
  // The old order (save first) broke vetoed closes: OnCloseQuery returning CanClose=FALSE left
  // FFormSaved=TRUE and FormPreRelease already executed on a still-live form — so the user kept
  // working, but the LATER real close saved nothing (stale INI) and the "guaranteed once,
  // before release" contract of FormPreRelease was violated.
  Result:= inherited CloseQuery;
  if Result
  then saveBeforeExit;
end;


{ This code is guaranteed to be called ONLY once.

  Tech details: It is enough to put SaveBeforeExit in these two places only: OnCloseQueryand & OnDestroy.
  Details: https://groups.google.com/forum/#!msg/borland.public.delphi.objectpascal/82AG0_kHonU/ft53lAjxWRMJ }
procedure TLightForm.saveBeforeExit;
begin
  if NOT FFormSaved
  AND NOT FSavingInProgress   // Reentrancy guard - see the field's declaration comment.
  AND NOT AppData.Initializing then
  begin
    FSavingInProgress:= TRUE;
    try
      FormPreRelease;

      if AutoState > asNone  // Give the user the option not to save the form
      then SaveForm;
    finally
      FFormSaved:= TRUE;        // Make sure it is put to true even on accidents, otherwise we might call it multiple times.
    end;
  end;
end;


{ Called ONLY once, when FFormSaved = False }
procedure TLightForm.FormPreRelease;
begin
  // Give user a chance to call its own finalization code (guaranteed once)
end;


procedure TLightForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if CloseOnEscape
  AND (Ord(Key) = VK_ESCAPE)
  then Close;
end;








{$IFDEF DEBUG}
{ Scan for TRadioButtons with TabStop=True that are not Checked.
  This is a common source of bugs: when the form is shown, Windows gives focus to the first
  TabStop control. If that's a radio button, it gets automatically checked, overriding
  whatever was set in code. See: "The Hidden Dangers of TRadioButton.TabStop" }
procedure ScanForDangerousTabStops(aParent: TWinControl);
begin
  for var i:= 0 to aParent.ControlCount-1 do
   begin
     if (aParent.Controls[i] is TRadioButton)
     AND TRadioButton(aParent.Controls[i]).TabStop
     AND NOT TRadioButton(aParent.Controls[i]).Checked
     then AppData.LogWarn(aParent.Controls[i].Name+ ' on '+ aParent.Controls[i].GetParentComponent.Name+ ' has TabStop=True but is not Checked!');

     if aParent.Controls[i] is TWinControl
     then ScanForDangerousTabStops(TWinControl(aParent.Controls[i]));
   end;
end;
{$ENDIF}


{-----------------------------------------------------------------------------------------------------------------------
   MAIN

   Load/Save all controls on this form to their initial state.

   Parameters:
         OnlyFormPos=False  ->  Save all supported controls on this form
         OnlyFormPos=True   ->  It will only save the position of the form (only Left/Top, no width/height/WndState)


   Also see LoadForm/SaveForm in LightVcl.Visual.INIFile.pas
-----------------------------------------------------------------------------------------------------------------------}

procedure TLightForm.SaveForm;
VAR
   IniFile: TIniFileVCL;
begin
  if TAppDataCore.Initializing
  AND (Self= Application.MainForm) then
   begin
     if TAppDataCore.RunningHome
     then MessageError('Closing application while still initializing!');
     Exit; // We don't save anything if the start up was improper!
   end;

  {$IFDEF DEBUG}
  ScanForDangerousTabStops(Self);
  {$ENDIF}

  IniFile:= TIniFileVCL.Create(Self.Name);
  TRY
   TRY
     IniFile.SaveForm(Self, AutoState);
   EXCEPT
     ON EIniFileException DO
       if AppData <> NIL
       then AppData.LogWarn('Cannot save INI file: '+ IniFile.FileName);
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
   IniFile: TIniFileVCL;
begin
  IniFile:= TIniFileVCL.Create(Self.Name);
  TRY
   TRY
     IniFile.LoadForm(Self, AutoState);
   EXCEPT
     ON EIniFileException DO
       if AppData <> NIL
       then AppData.LogWarn('Cannot load INI file: '+ IniFile.FileName);
   END;
  FINALLY
    FreeAndNil(IniFile);
  END;

  CorrectFormPositionDesktop(Self); // Form is off screen?

  { Set font for secondary forms }
  {moved to TAppData.setGuiProperties
  if Self <> Application.MainForm
  then Self.Font:= AppData.Font;  }
end;


procedure TLightForm.MainFormCaption(CONST Caption: string);
begin
  Assert(Application.MainForm <> NIL, 'MainFormCaption called before the main form exists!');

  if Caption= ''
  then Application.MainForm.Caption:= appData.AppName+ ' '+ appData.GetVersionInfoV
  else Application.MainForm.Caption:= appData.AppName+ ' '+ appData.GetVersionInfoV+ ' - ' + Caption;
  //todo: show debug release modes as in fmx
end;


end.
