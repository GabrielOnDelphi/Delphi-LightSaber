UNIT CoolTrayIcon;

{-------------------------------------------------------------------------------------------------------------
    A component for placing icons in the notification area of the Windows systray/taskbar
--------------------------------------------------------------------------------------------------------------
    Code updated 09.2026
    GabrielMoraru.com
       BREAKING: ShowBalloonHint and ShowBalloonHintUnicode lost their last parameter, TimeoutSecs.
       Windows has ignored the balloon timeout since Vista - it now comes from the system accessibility settings - so the parameter did nothing at all.
       The break is on purpose: the compiler now stops at every call site, instead of letting the caller believe a timeout is applied. Fix a call by deleting the last argument.
       Full explanation and the Microsoft source: the comment on TBalloonHintTimeOut in the interface section.

    Code updated 08.2026
    GabrielMoraru.com
       New FocusFormOnShow field. TRUE (the default) is exactly the old behavior, so nothing changes for
       existing applications. Set it to FALSE and the component stops calling SetForegroundWindow when the
       owner form is shown - needed by an app that must appear on screen WITHOUT taking the keyboard focus
       away from the user. See the field's own comment for why it is public and not published.

    Code updated 07.2026
    GabrielMoraru.com
       MinimizeToTray no longer needs an OnMinimizeToTray handler. Only the taskbar route used to work
       without one; the form's own minimize button now goes to the tray too.
       Note for existing users: with MinimizeToTray=TRUE and no handler, the form's minimize button is
       now SWALLOWED, so Application.OnMinimize no longer fires on that route.

    Code updated 01.2023
    GabrielMoraru.com
       Brought up to date for Delphi 13
       Code cleanup
       Reformatting
       Safer code
       Code works on 64 bit now
       Fixed some bugs
       Added two extra functions
       Removed code for WinNT
       Better documentation about how to use the components
       Let user know if MainFormOnTaskbar is True.

    Known issues
       1. It doesn work if Application.MainFormOnTaskbar= true. The requirement is to set MainFormOnTaskbar = false before Application.Run() is called.
       2. The TTrayIcon doesn't work properly if we call it during app start up. We can use a timer to check a bit later if the application needs to start minimized.

   Original code
       This is a major rewrite of the original TCoolTrayIcon:
         Troels Jakobsen - troels.jakobsen@gmail.com Copyright (c) 2006
         Portions by Jouni Airaksinen - mintus@codefield.com
         https://github.com/coolshou/CoolTrayIcon

--------------------------------------------------------------------------------------------------------------

  How to use it:

    procedure TForm1.TrayIconClick(Sender: TObject);
    begin
      TrayIcon.PutIconOnlyInTask;
    end;

    procedure TForm1.appEventsMinimize(Sender: TObject);
    begin
      TrayIcon.PutIconOnlyInTray;
    end;

-------------------------------------------------------------------------------------------------------------}

{$DebugInfo OFF}

INTERFACE

USES
  Winapi.Windows, Winapi.Messages, Winapi.ShellApi, System.SysUtils, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Menus, Vcl.Dialogs, Vcl.ImgList, SimpleTimer;

CONST
  WM_TRAYNOTIFY = WM_USER + 1024;    // User-defined message sent by the trayicon

TYPE
 TBalloonHintIcon    = (bitNone, bitInfo, bitWarning, bitError, bitCustom);

 { DEAD SINCE WINDOWS VISTA - the balloon timeout cannot be set by a program any more.
   The value used to go into NOTIFYICONDATA.uTimeout. Microsoft deprecated that field in Vista: the balloon now stays on screen for as long as the system accessibility settings say, and whatever the program writes into uTimeout is ignored.
   Source: https://learn.microsoft.com/en-us/windows/win32/api/shellapi/ns-shellapi-notifyicondataw - "This member is deprecated as of Windows Vista. Notification display times are now based on system accessibility settings."
   So on 09.2026 the TimeoutSecs parameter was removed from ShowBalloonHint and ShowBalloonHintUnicode. That is deliberate: passing a timeout did nothing, and a parameter that does nothing is worse than no parameter, because the caller believes it works. Removing it stops the compiler at every call site instead. There is no replacement - delete the argument from the call.
   The type itself is kept only so that old code which declares a variable of it still compiles, with a warning. }
 TBalloonHintTimeOut = 10 .. 60 deprecated 'Windows ignores the balloon timeout since Vista. Remove it.';
 TBehavior           = (bhWin95, bhWin2000);
 THintString = UnicodeString;       // 128 bytes, last char should be #0

 TCycleEvent   = procedure(Sender: TObject; NextIndex: Integer) of object;
 TStartupEvent = procedure(Sender: TObject; var ShowMainForm: Boolean) of object;

 TCoolTrayIcon = class(TComponent)
  private
   FEnabled                 : Boolean;
   FIcon                    : TIcon;
   FIconID                  : Cardinal;
   FIconVisible             : Boolean;
   FHint                    : THintString;
   FShowHint                : Boolean;
   FPopupMenu               : TPopupMenu;
   FLeftPopup               : Boolean;
   FOnClick,
   FOnDblClick              : TNotifyEvent;
   FOnCycle                 : TCycleEvent;
   FOnStartup               : TStartupEvent;
   FOnMouseDown,
   FOnMouseUp               : TMouseEvent;
   FOnMouseMove             : TMouseMoveEvent;
   FOnMouseEnter            : TNotifyEvent;
   FOnMouseExit             : TNotifyEvent;
   FOnMinimizeToTray        : TNotifyEvent;
   FOnBalloonHintShow,
   FOnBalloonHintHide,
   FOnBalloonHintTimeout,
   FOnBalloonHintClick      : TNotifyEvent;
   FMinimizeToTray          : Boolean;
   FClickStart              : Boolean;
   FClickReady              : Boolean;
   CycleTimer               : TSimpleTimer; // For icon cycling
   ClickTimer               : TSimpleTimer; // For distinguishing click and dbl.click
   ExitTimer                : TSimpleTimer; // For OnMouseExit event
   LastMoveX, LastMoveY     : Integer;
   FDidExit                 : Boolean;
   FWantEnterExitEvents     : Boolean;
   FBehavior                : TBehavior;
   IsDblClick               : Boolean;
   FIconIndex               : Integer; // Current index in imagelist
   FDesignPreview           : Boolean;
   SettingPreview           : Boolean; // Internal status flag
   SettingMDIForm           : Boolean; // Internal status flag
   FIconList                : TCustomImageList;
   FCycleIcons              : Boolean;
   FCycleInterval           : Cardinal;
   OldWndProc,
   NewWndProc               : Pointer; // Procedure variables
   procedure SetDesignPreview       (Value: Boolean);
   procedure SetCycleIcons          (Value: Boolean);
   procedure SetCycleInterval       (Value: Cardinal);
   function InitIcon: Boolean;
   procedure SetIcon                (Value: TIcon);
   procedure SetIconVisible         (Value: Boolean);
   procedure SetIconList            (Value: TCustomImageList);
   procedure SetIconIndex           (Value: Integer);
   procedure SetHint                (Value: THintString);
   procedure SetShowHint            (Value: Boolean);
   procedure SetWantEnterExitEvents (Value: Boolean);
   procedure SetBehavior            (Value: TBehavior);
   procedure IconChanged            (Sender: TObject);
   // Hook methods
   function HookAppProc(var Msg: TMessage): Boolean;
   procedure HookForm;
   procedure UnhookForm;
   procedure HookFormProc(var Msg: TMessage);
   // SimpleTimer event methods
   procedure ClickTimerProc(Sender: TObject);
   procedure CycleTimerProc(Sender: TObject);
   procedure MouseExitTimerProc(Sender: TObject);

  protected
   IconData: NOTIFYICONDATA; // Data of the tray icon wnd.
   procedure Loaded; override;
   function LoadDefaultIcon: Boolean; virtual;
   function ShowIcon: Boolean; virtual;
   function HideIcon: Boolean; virtual;
   function ModifyIcon: Boolean; virtual;
   procedure Click; dynamic;
   procedure DblClick; dynamic;
   procedure CycleIcon; dynamic;
   procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); dynamic;
   procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); dynamic;
   procedure MouseMove(Shift: TShiftState; X, Y: Integer); dynamic;
   procedure MouseEnter; dynamic;
   procedure MouseExit; dynamic;
   procedure DoMinimizeToTray; dynamic;
   procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
   { Does this component pull the owner form to the foreground when that form is shown?
   TRUE (set by the constructor) is the behavior every existing application already has.

   FALSE stops the two SetForegroundWindow calls that run on show - the one in HookFormProc and the one at the end of ShowMainForm. Windows documents SetForegroundWindow as the way to activate even a
   WS_EX_NOACTIVATE window, so those two calls walk through any "do not focus me" window style the application sets up, and take the keyboard away from whatever the user is typing in.

   Deliberately PUBLIC and not published: a published property can be stored as FALSE in a DFM and ship that way by accident. Public means the only way to switch it off is code, which the caller can wrap in a conditional define, so a Release build cannot reach it. }
   FocusFormOnShow: Boolean;

   property Handle: HWND read IconData.Wnd;
   property Behavior: TBehavior read FBehavior write SetBehavior default bhWin95;
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   function Refresh: Boolean;
   function ShowBalloonHint(Title, Text: String; IconType: TBalloonHintIcon): Boolean;                 // The TimeoutSecs parameter was removed on 09.2026 - see the comment on TBalloonHintTimeOut above.
   function ShowBalloonHintUnicode(Title, Text: WideString; IconType: TBalloonHintIcon): Boolean;      // The TimeoutSecs parameter was removed on 09.2026 - see the comment on TBalloonHintTimeOut above.
   function HideBalloonHint: Boolean;
   procedure Popup(X, Y: Integer);
   procedure PopupAtCursor;
   procedure BitmapToIcon(CONST Bitmap: TBitmap; const Icon: TIcon; MaskColor: TColor);
   function GetClientIconPos(X, Y: Integer): TPoint;
   function GetTooltipHandle: hWnd;
   function GetBalloonHintHandle: hWnd;
   function SetFocus: Boolean;

   // ----- SPECIAL: methods that only apply when owner is a form -----
   procedure HideTaskbarIcon;
   procedure ShowTaskbarIcon;
   procedure ShowMainForm;
   procedure HideMainForm;
   procedure PutIconOnlyInTask;     // NEW
   procedure PutIconOnlyInTray;     // NEW
  published
   // Properties:
   property DesignPreview: Boolean read FDesignPreview write SetDesignPreview default False;
   property IconList: TCustomImageList read FIconList write SetIconList;
   property CycleIcons: Boolean read FCycleIcons write SetCycleIcons default False;
   property CycleInterval: Cardinal read FCycleInterval write SetCycleInterval;
   property Enabled: Boolean read FEnabled write FEnabled default True;
   property Hint: THintString read FHint write SetHint;
   property ShowHint: Boolean read FShowHint write SetShowHint default True;
   property Icon: TIcon read FIcon write SetIcon;
   property IconVisible: Boolean read FIconVisible write SetIconVisible default False;
   property IconIndex: Integer read FIconIndex write SetIconIndex;
   property PopupMenu: TPopupMenu read FPopupMenu write FPopupMenu;
   property LeftPopup: Boolean read FLeftPopup write FLeftPopup default False;
   property WantEnterExitEvents: Boolean read FWantEnterExitEvents write SetWantEnterExitEvents default False;
   property MinimizeToTray: Boolean read FMinimizeToTray write FMinimizeToTray default False; // SPECIAL: properties that only apply when owner is a form // Minimize main form to tray when minimizing?
   // Events:
   property OnClick: TNotifyEvent read FOnClick write FOnClick;
   property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
   property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
   property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
   property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
   property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
   property OnMouseExit: TNotifyEvent read FOnMouseExit write FOnMouseExit;
   property OnCycle: TCycleEvent read FOnCycle write FOnCycle;
   property OnBalloonHintShow: TNotifyEvent read FOnBalloonHintShow write FOnBalloonHintShow;
   property OnBalloonHintHide: TNotifyEvent read FOnBalloonHintHide write FOnBalloonHintHide;
   property OnBalloonHintTimeout: TNotifyEvent read FOnBalloonHintTimeout write FOnBalloonHintTimeout;
   property OnBalloonHintClick: TNotifyEvent read FOnBalloonHintClick write FOnBalloonHintClick;
   // ----- SPECIAL: events that only apply when owner is a form -----
   property OnMinimizeToTray: TNotifyEvent read FOnMinimizeToTray write FOnMinimizeToTray;
   property OnStartup: TStartupEvent read FOnStartup write FOnStartup;
   // ----- END SPECIAL -----
 end;

procedure Register;

IMPLEMENTATION

USES
  System.Types, Vcl.ComCtrls, Winapi.CommCtrl;


TYPE
 TTrayIconHandler = class(TObject)
  private
   RefCount: Cardinal;
   FHandle: hWnd;
  public
   constructor Create;
   destructor Destroy; override;
   procedure Add;
   procedure Remove;
   procedure HandleIconMessage(var Msg: TMessage);
 end;


VAR
 TrayIconHandler: TTrayIconHandler = nil;
 IconRegistry: TList = nil;      // Maps sequential IDs to TCoolTrayIcon instances
 NextIconID: Cardinal = 0;       // Sequential counter for 64-bit safe icon IDs
 WM_TASKBARCREATED: Cardinal;
{ Forward declarations }
function FindCoolTrayIcon(ID: WPARAM): TCoolTrayIcon; forward;


{ ------------------ TTrayIconHandler ------------------ }

constructor TTrayIconHandler.Create;
begin
 inherited Create;
 RefCount := 0;
 FHandle := Classes.AllocateHWnd(HandleIconMessage);
end;


destructor TTrayIconHandler.Destroy;
begin
 Classes.DeallocateHWnd(FHandle); // Free the tray window
 inherited Destroy;
end;


procedure TTrayIconHandler.Add;
begin
 Inc(RefCount);
end;


procedure TTrayIconHandler.Remove;
begin
 if RefCount > 0 then
  Dec(RefCount);
end;


{ HandleIconMessage handles messages that go to the shell notification
  window (tray icon) itself. Most messages are passed through WM_TRAYNOTIFY.
  In these cases we use lParam to get the actual message, eg. WM_MOUSEMOVE.
  The method fires the appropriate event methods like OnClick and OnMouseMove. }

{ The message always goes through the container, TrayIconHandler.
  Msg.wParam contains the sequential ID of the TCoolTrayIcon instance.
  We look it up via FindCoolTrayIcon.

  Note: The tray icon never receives WM_MOUSEWHEEL messages.   }

procedure TTrayIconHandler.HandleIconMessage(var Msg: TMessage);

 function ShiftState: TShiftState; // Return the state of the shift, ctrl, and alt keys
 begin
  Result := [];
  if GetAsyncKeyState(VK_SHIFT)   < 0 then Include(Result, ssShift);
  if GetAsyncKeyState(VK_CONTROL) < 0 then Include(Result, ssCtrl);
  if GetAsyncKeyState(VK_MENU)    < 0 then Include(Result, ssAlt);
 end;

var
 Icon: TCoolTrayIcon;
 Pt: TPoint;
 Shift: TShiftState;
 I: Integer;
 M: TMenuItem;
begin
 if Msg.Msg = WM_TRAYNOTIFY then
  // Take action if a message from the tray icon comes through
  begin
   Icon := FindCoolTrayIcon(Msg.wParam);
   if Icon = nil then Exit;

   case Msg.lParam of

      WM_MOUSEMOVE:
       if Icon.FEnabled then
        begin
         // MouseEnter event
         if Icon.FWantEnterExitEvents then
          if Icon.FDidExit then
           begin
            Icon.MouseEnter;
            Icon.FDidExit := False;
           end;
         // MouseMove event
         Shift := ShiftState;
         GetCursorPos(Pt);
         Icon.MouseMove(Shift, Pt.X, Pt.Y);
         Icon.LastMoveX := Pt.X;
         Icon.LastMoveY := Pt.Y;
        end;

      WM_LBUTTONDOWN:
       if Icon.FEnabled then
        begin
         { If we have no OnDblClick event, fire the Click event immediately.
           Otherwise start a timer and wait for a short while to see if user
           clicks again. If he does click again inside this period we have
           a double click in stead of a click. }
         if Assigned(Icon.FOnDblClick) then
          begin
           Icon.ClickTimer.Interval := GetDoubleClickTime;
           Icon.ClickTimer.Enabled := True;
          end;
         Shift := ShiftState + [ssLeft];
         GetCursorPos(Pt);
         Icon.MouseDown(mbLeft, Shift, Pt.X, Pt.Y);
         Icon.FClickStart := True;
         if Icon.FLeftPopup then
          if (Assigned(Icon.FPopupMenu)) and (Icon.FPopupMenu.AutoPopup) then
           begin
            SetForegroundWindow(TrayIconHandler.FHandle);
            Icon.PopupAtCursor;
           end;
        end;

      WM_RBUTTONDOWN:
       if Icon.FEnabled then
        begin
         Shift := ShiftState + [ssRight];
         GetCursorPos(Pt);
         Icon.MouseDown(mbRight, Shift, Pt.X, Pt.Y);
         if (Assigned(Icon.FPopupMenu)) and (Icon.FPopupMenu.AutoPopup) then
          begin
           SetForegroundWindow(TrayIconHandler.FHandle);
           Icon.PopupAtCursor;
          end;
        end;

      WM_MBUTTONDOWN:
       if Icon.FEnabled then
        begin
         Shift := ShiftState + [ssMiddle];
         GetCursorPos(Pt);
         Icon.MouseDown(mbMiddle, Shift, Pt.X, Pt.Y);
        end;

      WM_LBUTTONUP:
       if Icon.FEnabled then
        begin
         Shift := ShiftState + [ssLeft];
         GetCursorPos(Pt);

         if Icon.FClickStart then // Then WM_LBUTTONDOWN was called before
          Icon.FClickReady := True;

         if Icon.FClickStart and (not Icon.ClickTimer.Enabled) then
          begin
           { At this point we know a mousedown occured, and the dblclick timer
             timed out. We have a delayed click. }
           Icon.FClickStart := False;
           Icon.FClickReady := False;
           Icon.Click; // We have a click
          end;

         Icon.FClickStart := False;

         Icon.MouseUp(mbLeft, Shift, Pt.X, Pt.Y);
        end;

      WM_RBUTTONUP:
       if Icon.FBehavior = bhWin95 then
        if Icon.FEnabled then
         begin
          Shift := ShiftState + [ssRight];
          GetCursorPos(Pt);
          Icon.MouseUp(mbRight, Shift, Pt.X, Pt.Y);
         end;

      WM_CONTEXTMENU, NIN_SELECT, NIN_KEYSELECT:
       if Icon.FBehavior = bhWin2000 then
        if Icon.FEnabled then
         begin
          Shift := ShiftState + [ssRight];
          GetCursorPos(Pt);
          Icon.MouseUp(mbRight, Shift, Pt.X, Pt.Y);
         end;

      WM_MBUTTONUP:
       if Icon.FEnabled then
        begin
         Shift := ShiftState + [ssMiddle];
         GetCursorPos(Pt);
         Icon.MouseUp(mbMiddle, Shift, Pt.X, Pt.Y);
        end;

      WM_LBUTTONDBLCLK:
       if Icon.FEnabled then
        begin
         Icon.FClickReady := False;
         Icon.IsDblClick := True;
         Icon.DblClick;
         { Handle default menu items. But only if LeftPopup is false, or it
           will conflict with the popupmenu when it is called by a click event. }
         M := nil;
         if Assigned(Icon.FPopupMenu) then
          if (Icon.FPopupMenu.AutoPopup) and (not Icon.FLeftPopup) then
           for I := Icon.FPopupMenu.Items.Count - 1 downto 0 do
            begin
             if Icon.FPopupMenu.Items[I].Default then
              M := Icon.FPopupMenu.Items[I];
            end;
         if M <> nil then
          M.Click;
        end;

      NIN_BALLOONSHOW:
       if Assigned(Icon.FOnBalloonHintShow) then Icon.FOnBalloonHintShow(Icon);

      NIN_BALLOONHIDE:
       if Assigned(Icon.FOnBalloonHintHide) then Icon.FOnBalloonHintHide(Icon);

      NIN_BALLOONTIMEOUT:
       if Assigned(Icon.FOnBalloonHintTimeout) then Icon.FOnBalloonHintTimeout(Icon);

      NIN_BALLOONUSERCLICK:
       if Assigned(Icon.FOnBalloonHintClick) then Icon.FOnBalloonHintClick(Icon);

   end;
  end

 else // Messages that didn't go through the tray icon
   case Msg.Msg of
    { Windows sends us a WM_QUERYENDSESSION message when it prepares for shutdown. Msg.Result must not return 0, or the system will be unable to shut down. The same goes for other specific system messages. }
    WM_CLOSE, WM_QUIT,
    WM_DESTROY, WM_NCDESTROY: Msg.Result := 1;
    WM_QUERYENDSESSION,
    WM_ENDSESSION: Msg.Result := 1;

   else // Handle all other messages with the default handler
     Msg.Result := DefWindowProc(FHandle, Msg.Msg, Msg.wParam, Msg.lParam);
   end;
end;

{ ---------------- Container management ---------------- }

procedure AddTrayIcon;
begin
 if not Assigned(TrayIconHandler)
 then TrayIconHandler := TTrayIconHandler.Create;    // Create new handler
 TrayIconHandler.Add;
end;


procedure RemoveTrayIcon;
begin
 if Assigned(TrayIconHandler) then
  begin
    TrayIconHandler.Remove;
    if TrayIconHandler.RefCount = 0
    then FreeAndNil(TrayIconHandler);
  end;
end;

{ --------------- Icon registry (64-bit safe) --------------- }
{ Shell_NotifyIcon.uID is always 32-bit UINT, so we cannot store a 64-bit
  pointer there. Instead we assign sequential Cardinal IDs and keep a
  registry list for reverse lookup. With 1-3 tray icons per app, the
  linear scan is effectively O(1). }

function RegisterCoolTrayIcon(Icon: TCoolTrayIcon): Cardinal;
begin
 Inc(NextIconID);
 if IconRegistry = nil
 then IconRegistry := TList.Create;
 IconRegistry.Add(Icon);
 EXIT(NextIconID);
end;


procedure UnregisterCoolTrayIcon(Icon: TCoolTrayIcon);
begin
 if IconRegistry <> nil then
  begin
    IconRegistry.Remove(Icon);
    if IconRegistry.Count = 0
    then FreeAndNil(IconRegistry);
  end;
end;


function FindCoolTrayIcon(ID: WPARAM): TCoolTrayIcon;
var
 I: Integer;
 IDCardinal: Cardinal;
begin
 IDCardinal := Cardinal(ID);
 if IconRegistry <> nil then
  for I := 0 to IconRegistry.Count - 1 do
   if TCoolTrayIcon(IconRegistry[I]).FIconID = IDCardinal
   then EXIT(TCoolTrayIcon(IconRegistry[I]));
 EXIT(nil);
end;

{ ------------- SimpleTimer event methods -------------- }

procedure TCoolTrayIcon.ClickTimerProc(Sender: TObject);
begin
 ClickTimer.Enabled := False;
 if NOT IsDblClick then
  if FClickReady then
   begin
     FClickReady := False;
     Click;
   end;
 IsDblClick := False;
end;


procedure TCoolTrayIcon.CycleTimerProc(Sender: TObject);
begin
 if Assigned(FIconList) then
  begin
    FIconList.GetIcon(FIconIndex, FIcon);
    // IconChanged(AOwner);
    CycleIcon; // Call event method

    if FIconIndex < FIconList.Count - 1
    then SetIconIndex(FIconIndex + 1)
    else SetIconIndex(0);
  end;
end;


procedure TCoolTrayIcon.MouseExitTimerProc(Sender: TObject);
var
 Pt: TPoint;
begin
 if FDidExit then Exit;
 GetCursorPos(Pt);
 if (Pt.X <> LastMoveX) or (Pt.Y <> LastMoveY) then
  begin
    FDidExit := True;
    MouseExit;
  end;
end;

{ ------------------- TCoolTrayIcon -------------------- }

constructor TCoolTrayIcon.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);

 AddTrayIcon; // Container management
 FIconID := RegisterCoolTrayIcon(Self); // 64-bit safe sequential ID
 SettingMDIForm := True;
 FEnabled  := True; // Enabled by default
 FShowHint := True; // Show hint by default
 FocusFormOnShow := True; // Default = the behavior every existing application already has
 SettingPreview := False;

 FIcon := TIcon.Create;
 FIcon.OnChange := IconChanged;
 FillChar(IconData, SizeOf(IconData), 0);
 IconData.cbSize := SizeOf(NOTIFYICONDATA);
 { IconData.Wnd points to procedure to receive callback messages from the icon. We set it to our TrayIconHandler instance. }
 IconData.Wnd := TrayIconHandler.FHandle;
 // Add an id for the tray icon
 IconData.uID := FIconID;
 // We want icon, message handling, and tooltips by default
 IconData.uFlags := NIF_ICON or NIF_MESSAGE or NIF_TIP;
 // Message to send to IconData.Wnd when event occurs
 IconData.uCallbackMessage := WM_TRAYNOTIFY;

 // Create SimpleTimers for later use
 CycleTimer := TSimpleTimer.Create;
 CycleTimer.OnTimer := CycleTimerProc;
 ClickTimer := TSimpleTimer.Create;
 ClickTimer.OnTimer := ClickTimerProc;
 ExitTimer := TSimpleTimer.CreateEx(20, MouseExitTimerProc);

 FDidExit := True; // Prevents MouseExit from firing at startup

 SetDesignPreview(FDesignPreview);

 // Set hook(s)
 if not(csDesigning in ComponentState) then
  begin
    { For MinimizeToTray to work, we need to know when the form is minimized
      (happens when either the application or the main form minimizes).
      The straight-forward way is to make TCoolTrayIcon trap the Application.OnMinimize event. However, if you also make use of this event in the application, the OnMinimize code used by TCoolTrayIcon is discarded.
      The solution is to hook into the app.'s message handling (via HookAppProc).
      You can then catch any message that goes through the app. and still use the OnMinimize event. }
    Application.HookMainWindow(HookAppProc);
    { You can hook into the main form (or any other window), allowing you to handle any message that window processes. This is necessary in order to properly handle when the user minimizes the form using the TASKBAR icon. }
    if Owner is TWinControl
    then HookForm;
  end;
end;


destructor TCoolTrayIcon.Destroy;
begin
 try
    SetIconVisible(False);   // Remove the icon from the tray
    SetDesignPreview(False); // Remove any DesignPreview icon
    FreeAndNil(CycleTimer);
    FreeAndNil(ClickTimer);
    FreeAndNil(ExitTimer);
    FreeAndNil(FIcon);
 finally
  // It is important to unhook any hooked processes
  if not(csDesigning in ComponentState) then
   begin
     Application.UnhookMainWindow(HookAppProc);
     if Owner is TWinControl
     then UnhookForm;
   end;
  UnregisterCoolTrayIcon(Self);
  RemoveTrayIcon; // Container management
  inherited Destroy;
 end
end;


procedure TCoolTrayIcon.Loaded;
{ This method is called when all properties of the component have been initialized.
  The method SetIconVisible must be called here, after the tray icon (FIcon) has loaded itself.
  Otherwise, the tray icon will be blank (no icon image).
  Other boolean values must also be set here. }
var
 Show: Boolean;
begin
 inherited Loaded; // Always call inherited Loaded first

 if Owner is TWinControl then
  if not(csDesigning in ComponentState) then
   begin
    Show := True;
    if Assigned(FOnStartup)
    then FOnStartup(Self, Show);

    if not Show then
     begin
      Application.ShowMainForm := False;
      HideMainForm;
     end;

    // ShowMainFormOnStartup := Show;
   end;

 ModifyIcon;
 SetIconVisible(FIconVisible);
 SetCycleIcons(FCycleIcons);
 SetWantEnterExitEvents(FWantEnterExitEvents);
 SetBehavior(FBehavior);
end;


function TCoolTrayIcon.LoadDefaultIcon: Boolean;
{ This method is called to determine whether to assign a default icon to the component. Descendant classes (like TextTrayIcon) can override the method to change this behavior. }
begin
 Result := True;
end;


procedure TCoolTrayIcon.Notification(AComponent: TComponent; Operation: TOperation);
begin
 inherited Notification(AComponent, Operation);
 // Check if either the imagelist or the popup menu is about to be deleted
 if (AComponent = FIconList) and (Operation = opRemove) then
    FIconList := nil;

 if (AComponent = FPopupMenu) and (Operation = opRemove) then
    FPopupMenu := nil;
end;


procedure TCoolTrayIcon.IconChanged(Sender: TObject);
begin
  ModifyIcon;
end;


{ All app. messages pass through HookAppProc. You can override the messages
  by not passing them along to Windows (set Result=True). }

function TCoolTrayIcon.HookAppProc(var Msg: TMessage): Boolean;
var
  Show: Boolean;
begin
 Result := False; // Should always be False unless we don't want the default message handling

 case Msg.Msg of

  WM_SIZE:
   // Handle MinimizeToTray by capturing minimize event of application
   if Msg.wParam = SIZE_MINIMIZED then
    begin
     if FMinimizeToTray then
      DoMinimizeToTray;
     { You could insert a call to a custom minimize event here, but it would
       behave exactly like Application.OnMinimize, so I see no need for it. }
    end;

  WM_WINDOWPOSCHANGED:
   begin
    { Handle MDI forms: MDI children cause the app. to be redisplayed on the
      taskbar. We hide it again. This may cause a quick flicker. }
    if SettingMDIForm then
     if Application.MainForm <> nil then
      begin

       if Application.MainForm.FormStyle = fsMDIForm then
        begin
         Show := True;
         if Assigned(FOnStartup) then
          FOnStartup(Self, Show);
         if not Show
         then HideTaskbarIcon;
        end;

       SettingMDIForm := False; // So we only do this once
      end;
   end;

  WM_SYSCOMMAND:
   // Handle MinimizeToTray by capturing minimize event of application
   if Msg.wParam = SC_RESTORE then
    if Application.MainForm <> nil then
    begin
     if Application.MainForm.WindowState = wsMinimized
     then Application.MainForm.WindowState := wsNormal;
     Application.MainForm.Visible := True;
    end;
 end;

 // Show the tray icon if the taskbar has been re-created after an Explorer crash
 if Msg.Msg = WM_TASKBARCREATED then
  if FIconVisible
  then ShowIcon;
end;


procedure TCoolTrayIcon.HookForm;
begin
 if (Owner as TWinControl) <> nil then
  begin
   // Hook the parent window
   OldWndProc := Pointer(GetWindowLongPtr((Owner as TWinControl).Handle, GWL_WNDPROC));      { SetWindowLong was replaced with SetWindowLongPtr for 64 bit compatibility.  Details: http://docwiki.embarcadero.com/RADStudio/Seattle/en/Converting_32-bit_Delphi_Applications_to_64-bit_Windows }
   NewWndProc := Classes.MakeObjectInstance(HookFormProc);
   SetWindowLongPtr((Owner as TWinControl).Handle, GWL_WNDPROC, LONG_PTR(NewWndProc));     { SetWindowLong was replaced with SetWindowLongPtr for 64 bit compatibility.  Details: http://docwiki.embarcadero.com/RADStudio/Seattle/en/Converting_32-bit_Delphi_Applications_to_64-bit_Windows }
  end;
end;


procedure TCoolTrayIcon.UnhookForm;
begin
 if ((Owner as TWinControl) <> nil)
 and (Assigned(OldWndProc))
 then SetWindowLongPtr((Owner as TWinControl).Handle, GWL_WNDPROC, LONG_PTR(OldWndProc));     { SetWindowLong was replaced with SetWindowLongPtr for 64 bit compatibility.  Details: http://docwiki.embarcadero.com/RADStudio/Seattle/en/Converting_32-bit_Delphi_Applications_to_64-bit_Windows }
 if Assigned(NewWndProc)
 then Classes.FreeObjectInstance(NewWndProc);
 NewWndProc := nil;
 OldWndProc := nil;
end;


{ All main form messages pass through HookFormProc. You can override the
  messages by not passing them along to Windows (via CallWindowProc).
  You should be careful with the graphical messages, though. }

procedure TCoolTrayIcon.HookFormProc(var Msg: TMessage);

 { Returns TRUE if we swallowed the message (the form went to the tray, so Windows must not minimize it).
   OnMinimizeToTray is OPTIONAL. Until 07.2026 this whole block was skipped unless a handler was assigned,
   so the form's OWN minimize button fell through to a plain Windows minimize.
   The app-window route was never affected: HookAppProc (WM_SIZE/SIZE_MINIMIZED) already called
   DoMinimizeToTray with no handler check - which is why the two hooks used to disagree. }
 function DoMinimizeEvents: Boolean;
 begin
  if NOT FMinimizeToTray then EXIT(FALSE);

  if Assigned(FOnMinimizeToTray)
  then FOnMinimizeToTray(Self);

  DoMinimizeToTray;
  Msg.Result := 1;
  Result     := TRUE;
 end;

begin
 case Msg.Msg of

  WM_SHOWWINDOW:
   begin
    if (Msg.wParam = 1) and (Msg.lParam = 0) then
     begin
       // Show the taskbar icon (Windows may have shown it already)
       // ShowWindow(Application.Handle, SW_RESTORE);
       { Bring the taskbar icon and the main form to the foreground.
         This fires on EVERY show of the owner form, and the handler cannot tell "the user restored me from
         the tray" - where the grab is what the user asked for - from "the application is starting up",
         where it is not. FocusFormOnShow is how a caller opts out of both cases. }
       if FocusFormOnShow then
        begin
          SetForegroundWindow(Application.Handle);
          SetForegroundWindow((Owner as TWinControl).Handle);
        end;
     end
    else
     if (Msg.wParam = 0) and (Msg.lParam = SW_PARENTCLOSING) then
      begin
        // Application is minimizing (or closing), handle MinimizeToTray
        if not Application.Terminated
        AND DoMinimizeEvents
        then Exit; // Don't pass the message on
      end;

   end;

  WM_SYSCOMMAND:
   // Handle MinimizeToTray by capturing minimize event of form
   if Msg.wParam = SC_MINIMIZE then
    if DoMinimizeEvents then
     Exit; // Don't pass the message on
 end;

 // Pass the message on
 Msg.Result := CallWindowProc(OldWndProc, (Owner as TWinControl).Handle, Msg.Msg, Msg.wParam, Msg.lParam);
end;


procedure TCoolTrayIcon.SetIcon(Value: TIcon);
begin
  FIcon.OnChange := nil;
  FIcon.Assign(Value);
  FIcon.OnChange := IconChanged;
  ModifyIcon;
end;


procedure TCoolTrayIcon.SetIconVisible(Value: Boolean);
begin
  if Value
  then ShowIcon
  else HideIcon;
end;


procedure TCoolTrayIcon.SetDesignPreview(Value: Boolean);
begin
  FDesignPreview := Value;
  SettingPreview := True; // Raise flag
  { Assign a default icon if Icon property is empty. This will assign an icon
    to the component when it is created for the very first time. When the user
    assigns another icon it will not be overwritten next time the project loads.
    HOWEVER, if the user has decided explicitly to have no icon a default icon
    will be inserted regardless. I figured this was a tolerable price to pay. }
  if (csDesigning in ComponentState) then
   begin
    if FIcon.Handle = 0 then
     if LoadDefaultIcon then
      FIcon.Handle := LoadIcon(0, IDI_WINLOGO);
    { It is tempting to assign the application's icon (Application.Icon) as a
      default icon. The problem is there's no Application instance at design time.
      Or is there? Yes there is: the Delphi editor! Application.Icon is the icon found in delphi32.exe. How to use:
      FIcon.Assign(Application.Icon);
      Seems to work, but I don't recommend it. Why would you want to, anyway? }
    SetIconVisible(Value);
   end;
  SettingPreview := False; // Clear flag
end;


procedure TCoolTrayIcon.SetCycleIcons(Value: Boolean);
begin
  FCycleIcons := Value;
  if Value
  then
    begin
     SetIconIndex(0);
     CycleTimer.Interval := FCycleInterval;
     CycleTimer.Enabled := True;
    end
  else
    CycleTimer.Enabled := False;
end;


procedure TCoolTrayIcon.SetCycleInterval(Value: Cardinal);
begin
  if Value <> FCycleInterval then
   begin
     FCycleInterval := Value;
     SetCycleIcons(FCycleIcons);
   end;
end;


procedure TCoolTrayIcon.SetIconList(Value: TCustomImageList);
begin
  FIconList := Value;
  SetIconIndex(0);
end;


procedure TCoolTrayIcon.SetIconIndex(Value: Integer);
begin
 if FIconList <> nil
 then
   begin
    FIconIndex := Value;
    if Value >= FIconList.Count
    then FIconIndex := FIconList.Count - 1;
    FIconList.GetIcon(FIconIndex, FIcon);
   end
 else
   FIconIndex := 0;

 ModifyIcon;
end;


procedure TCoolTrayIcon.SetHint(Value: THintString);
begin
  FHint := Value;
  ModifyIcon;
end;


procedure TCoolTrayIcon.SetShowHint(Value: Boolean);
begin
  FShowHint := Value;
  ModifyIcon;
end;


procedure TCoolTrayIcon.SetWantEnterExitEvents(Value: Boolean);
begin
  FWantEnterExitEvents := Value;
  ExitTimer.Enabled := Value;
end;


procedure TCoolTrayIcon.SetBehavior(Value: TBehavior);
begin
  FBehavior := Value;
  case FBehavior of
    bhWin95: IconData.uVersion := 0;
    bhWin2000: IconData.uVersion := NOTIFYICON_VERSION;
  end;
  Shell_NotifyIcon(NIM_SETVERSION, @IconData);
end;


function TCoolTrayIcon.InitIcon: Boolean;
// Set icon and tooltip
VAR  ok: Boolean;
begin
  Result := False;
  ok := True;
  if (csDesigning in ComponentState)
  then ok := (SettingPreview or FDesignPreview);

  if ok then
   begin
    if FIcon.Empty
    then IconData.hIcon := 0
    else IconData.hIcon := FIcon.Handle;

    if (FHint <> '') and (FShowHint) then
     begin
       StrPLCopy(IconData.szTip, FHint, Length(IconData.szTip) - 1);
       if Length(FHint) > 0
       then IconData.uFlags := IconData.uFlags or NIF_TIP
       else IconData.uFlags := IconData.uFlags and not NIF_TIP;
     end
    else
     IconData.szTip := '';

    Result := True;
   end;
end;


function TCoolTrayIcon.ShowIcon: Boolean;
// Add/show the icon on the tray
begin
  Result := False;
  if not SettingPreview then FIconVisible := True;

  if (csDesigning in ComponentState)
  then
   begin
    if SettingPreview AND InitIcon
    then Result := Shell_NotifyIcon(NIM_ADD, @IconData);
   end
  else
    if InitIcon
    then Result := Shell_NotifyIcon(NIM_ADD, @IconData);
end;


// Remove/hide the icon from the tray
function TCoolTrayIcon.HideIcon: Boolean;
begin
  Result := False;
  if not SettingPreview
  then FIconVisible := False;

  if (csDesigning in ComponentState)
  then
   begin
     if SettingPreview AND InitIcon
     then Result := Shell_NotifyIcon(NIM_DELETE, @IconData);
   end
  else
    if InitIcon
    then Result := Shell_NotifyIcon(NIM_DELETE, @IconData);
end;


// Change icon or tooltip if icon already placed
function TCoolTrayIcon.ModifyIcon: Boolean;
begin
  if InitIcon then
    Result := Shell_NotifyIcon(NIM_MODIFY, @IconData)
  else
 Result := False;
end;


function TCoolTrayIcon.ShowBalloonHint(Title, Text: String; IconType: TBalloonHintIcon): Boolean; // Show balloon hint. Return false if error.
{ No timeout parameter: Windows has ignored NOTIFYICONDATA.uTimeout since Vista. See the comment on TBalloonHintTimeOut in the interface section.
  Not writing uTimeout also removes a second, quieter problem: in NOTIFYICONDATA the fields uTimeout and uVersion are the SAME memory (a variant part in Delphi, a union in C - see Winapi.ShellAPI.pas, record _NOTIFYICONDATAW). Every balloon used to overwrite the version number that SetBehavior had put there. }
const
 aBalloonIconTypes: array [TBalloonHintIcon] of Byte = (NIIF_NONE, NIIF_INFO, NIIF_WARNING, NIIF_ERROR, NIIF_USER);
begin
 // Remove old balloon hint
 HideBalloonHint;

 // Display new balloon hint
 IconData.uFlags := IconData.uFlags or NIF_INFO;
 StrLCopy(IconData.szInfo, PChar(Text), High(IconData.szInfo));
 StrLCopy(IconData.szInfoTitle, PChar(Title), High(IconData.szInfoTitle));
 IconData.dwInfoFlags := aBalloonIconTypes[IconType];

 Result := ModifyIcon;
 // Remove NIF_INFO before next call to ModifyIcon (or the balloon hint will redisplay itself)
 IconData.uFlags := NIF_ICON or NIF_MESSAGE or NIF_TIP;
end;


function TCoolTrayIcon.ShowBalloonHintUnicode(Title, Text: WideString; IconType: TBalloonHintIcon): Boolean;
// In Unicode Delphi, String is already UnicodeString. Delegate to ShowBalloonHint.
begin
 Result := ShowBalloonHint(String(Title), String(Text), IconType);
end;


function TCoolTrayIcon.HideBalloonHint: Boolean;
// Hide balloon hint. Return false if error.
begin
 IconData.uFlags := IconData.uFlags or NIF_INFO;
 StrPCopy(IconData.szInfo, '');
 Result := ModifyIcon;
end;


{ Render an icon from a 16x16 bitmap. Return false if error.
  MaskColor is a color that will be rendered transparently. Use clNone for
  no transparency. }
procedure TCoolTrayIcon.BitmapToIcon(const Bitmap: TBitmap; const Icon: TIcon; MaskColor: TColor);
var
 BitmapImageList: TImageList;
begin
 BitmapImageList := TImageList.CreateSize(16, 16);
 try
   BitmapImageList.AddMasked(Bitmap, MaskColor);
   BitmapImageList.GetIcon(0, Icon);
 FINALLY
   FreeAndNil(BitmapImageList);
 end;
end;


// Return the cursor position inside the tray icon
function TCoolTrayIcon.GetClientIconPos(X, Y: Integer): TPoint;
const
 IconBorder = 1;
var
  H: hWnd;
  P: TPoint;
  IconSize: Integer;
begin
 { The CoolTrayIcon.Handle property is not the window handle of the tray icon.
   We can find the window handle via WindowFromPoint when the mouse is over the tray icon. (It can probably be found via GetWindowLong_ptr as well).

   BTW: The parent of the tray icon is the TASKBAR - not the traybar, which
   contains the tray icons and the clock. The traybar seems to be a canvas, not a real window (?). }

 // Get the icon size
 IconSize := GetSystemMetrics(SM_CYCAPTION) - 3;

 P.X := X;
 P.Y := Y;
 H := WindowFromPoint(P);
 { Convert current cursor X,Y coordinates to tray client coordinates. Add borders to tray icon size in the calculations. }
 WinApi.Windows.ScreenToClient(H, P);
 P.X := (P.X mod ((IconBorder * 2) + IconSize)) - 1;
 P.Y := (P.Y mod ((IconBorder * 2) + IconSize)) - 1;
 Result := P;
end;


function TCoolTrayIcon.GetTooltipHandle: hWnd;
{ All tray icons (but not the clock) share the same tooltip.
  Return the tooltip handle or 0 if error. }
var
  wnd, lTaskBar: hWnd;
  pidTaskBar, pidWnd: DWORD;
begin
 // Get the TaskBar handle
 lTaskBar := FindWindowEx(0, 0, 'Shell_TrayWnd', nil);

 // Get the TaskBar Process ID
 GetWindowThreadProcessId(lTaskBar, @pidTaskBar);

 // Enumerate all tooltip windows
 wnd := FindWindowEx(0, 0, TOOLTIPS_CLASS, nil);
 while wnd <> 0 do
  begin
   // Get the tooltip process ID
   GetWindowThreadProcessId(wnd, @pidWnd);

   { Compare the process ID of the taskbar and the tooltip. If they are the same we have one of the taskbar tooltips. }
   if pidTaskBar = pidWnd
   then    { Get the tooltip style. The tooltip for tray icons does not have the TTS_NOPREFIX style. }
     if GetWindowLongptr(wnd, GWL_STYLE) and TTS_NOPREFIX = 0 then Break;     { getWindowLong was replaced with getWindowLongPtr for 64 bit compatibility.  Details: http://docwiki.embarcadero.com/RADStudio/Seattle/en/Converting_32-bit_Delphi_Applications_to_64-bit_Windows }

   { Get the tooltip style. The tooltip for tray icons does not have the TTS_NOPREFIX style. }

   wnd := FindWindowEx(0, wnd, TOOLTIPS_CLASS, nil);
  end;
 Result := wnd;
end;


function TCoolTrayIcon.GetBalloonHintHandle: hWnd;
{ All applications share the same balloon hint.
  Return the balloon hint handle or 0 if error. }
var
 wnd, lTaskBar: hWnd;
 pidTaskBar, pidWnd: DWORD;
begin
 // Get the TaskBar handle
 lTaskBar := FindWindowEx(0, 0, 'Shell_TrayWnd', nil);
 // Get the TaskBar Process ID
 GetWindowThreadProcessId(lTaskBar, @pidTaskBar);

 // Enumerate all tooltip windows
 wnd := FindWindowEx(0, 0, TOOLTIPS_CLASS, nil);
 while wnd <> 0 do
  begin
   // Get the tooltip process ID
   GetWindowThreadProcessId(wnd, @pidWnd);

   { Compare the process ID of the taskbar and the tooltip.  If they are the same we have one of the taskbar tooltips. }
   if pidTaskBar = pidWnd then
    // We don't want windows with the TTS_NOPREFIX style. That's the simple tooltip.
     if (GetWindowLongptr(wnd, GWL_STYLE) and TTS_NOPREFIX) <> 0 then Break;   { getWindowLong was replaced with getWindowLongPtr for 64 bit compatibility.  Details: http://docwiki.embarcadero.com/RADStudio/Seattle/en/Converting_32-bit_Delphi_Applications_to_64-bit_Windows }

   wnd := FindWindowEx(0, wnd, TOOLTIPS_CLASS, nil);
  end;
 Result := wnd;
end;


function TCoolTrayIcon.SetFocus: Boolean;
begin
 Result := Shell_NotifyIcon(NIM_SETFOCUS, @IconData);
end;


function TCoolTrayIcon.Refresh: Boolean;
// Refresh the icon
begin
 Result := ModifyIcon;
end;


procedure TCoolTrayIcon.Popup(X, Y: Integer);
begin
 if Assigned(FPopupMenu) then
  begin
   { Bring the main form (or its modal dialog) to the foreground.
     Do this by calling SetForegroundWindow(Handle);
     We don't use Application.Handle as it will make the taskbar button visible in case the form/application is hidden. }
   SetForegroundWindow(Handle);
   // Now make the menu pop up
   FPopupMenu.PopupComponent := Self;
   FPopupMenu.Popup(X, Y);
   // Remove the popup again in case user deselects it
   if Owner is TWinControl then // Owner might be of type TService
    // Post an empty message to the owner form so popup menu disappears
    PostMessage((Owner as TWinControl).Handle, WM_NULL, 0, 0)
    {
      else
      // Owner is not a form; send the empty message to the app.
      PostMessage(Application.Handle, WM_NULL, 0, 0);
    }
  end;
end;


procedure TCoolTrayIcon.PopupAtCursor;
var CursorPos: TPoint;
begin
 if GetCursorPos(CursorPos)
 then Popup(CursorPos.X, CursorPos.Y);
end;


procedure TCoolTrayIcon.Click;
begin
 if Assigned(FOnClick) then
  FOnClick(Self);
end;


procedure TCoolTrayIcon.DblClick;
begin
 if Assigned(FOnDblClick) then
  FOnDblClick(Self);
end;


procedure TCoolTrayIcon.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Assigned(FOnMouseDown) then
  FOnMouseDown(Self, Button, Shift, X, Y);
end;


procedure TCoolTrayIcon.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Assigned(FOnMouseUp) then
  FOnMouseUp(Self, Button, Shift, X, Y);
end;


procedure TCoolTrayIcon.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
 if Assigned(FOnMouseMove) then
  FOnMouseMove(Self, Shift, X, Y);
end;


procedure TCoolTrayIcon.MouseEnter;
begin
 if Assigned(FOnMouseEnter) then
  FOnMouseEnter(Self);
end;


procedure TCoolTrayIcon.MouseExit;
begin
 if Assigned(FOnMouseExit) then
  FOnMouseExit(Self);
end;


procedure TCoolTrayIcon.CycleIcon;
var
 NextIconIndex: Integer;
begin
 if (FIconList <> nil) and (FIconIndex < FIconList.Count - 1)
 then NextIconIndex := FIconIndex + 1
 else NextIconIndex := 0;

 if Assigned(FOnCycle) then
  FOnCycle(Self, NextIconIndex);
end;


procedure TCoolTrayIcon.DoMinimizeToTray;
begin
 // Override this method to change automatic tray minimizing behavior
 HideMainForm;
 IconVisible := True;
end;



procedure TCoolTrayIcon.HideTaskbarIcon;
begin
 if IsWindowVisible(Application.Handle) then
  ShowWindow(Application.Handle, SW_HIDE);
end;


procedure TCoolTrayIcon.ShowTaskbarIcon;
begin
 if NOT IsWindowVisible(Application.Handle) then
  ShowWindow(Application.Handle, SW_SHOW);
end;


procedure TCoolTrayIcon.ShowMainForm;
begin
 if Owner is TWinControl then // Owner might be of type TService
  if Application.MainForm <> nil then
   begin
    // Restore the app, but don't automatically show its taskbar icon
    // Show application's TASKBAR icon (not the tray icon)
    // ShowWindow(Application.Handle, SW_RESTORE);
    Application.Restore;
    // Show the form itself
    if Application.MainForm.WindowState = wsMinimized
    then Application.MainForm.WindowState := wsNormal; // Override minimized state
    Application.MainForm.Visible := True;
    // Bring the main form (or its modal dialog) to the foreground
    if FocusFormOnShow
    then SetForegroundWindow(Application.Handle);
   end;
end;


procedure TCoolTrayIcon.HideMainForm;
begin
 if Owner is TWinControl then // Owner might be of type TService
  if Application.MainForm <> NIL then
   begin
    // Hide the form itself (and thus any child windows)
    Application.MainForm.Visible := False;
    { Hide application's TASKBAR icon (not the tray icon). Do this AFTER the main form is hidden, or any child windows will redisplay the taskbar icon if they are visible. }
    HideTaskbarIcon;
   end;
end;








// MINE !!!
procedure TCoolTrayIcon.PutIconOnlyInTask;
begin
 ShowTaskbarIcon; { icon in TaskBar= True }
 IconVisible := False; { icon in Tray= False }
 ShowMainForm;
end;


// MINE !!!
procedure TCoolTrayIcon.PutIconOnlyInTray;
begin
 Assert(NOT Application.MainFormOnTaskbar, 'MainFormOnTaskbar must be false otherwise PutIconOnlyInTray won''t work!');

 //HideTaskbarIcon;
 ShowWindow(Application.Handle, SW_HIDE);
 ShowWindow(Application.MainForm.Handle, SW_HIDE);
 IconVisible := True;
 Application.ShowMainForm := False;
end;



procedure Register;
begin
 RegisterComponents('3rd_party', [TCoolTrayIcon]);
end;




INITIALIZATION
 if GetComCtlVersion >= ComCtlVersionIE4 then
    WM_TASKBARCREATED := RegisterWindowMessage('TaskbarCreated');

finalization
 FreeAndNil(IconRegistry);
 FreeAndNil(TrayIconHandler);

end.
