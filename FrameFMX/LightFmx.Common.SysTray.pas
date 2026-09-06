unit LightFmx.Common.SysTray;

{======================================================================================================
   2026.07
   www.GabrielMoraru.com

   System tray icon via Win32 Shell_NotifyIcon + AllocateHWnd message window.

   FMX does NOT expose the main form's HWND message loop, so we create a dedicated hidden helper window with AllocateHWnd to receive WM_TRAYICON notifications.

   Icon:         16×16 GDI bitmap (solid color or sparkline).
   Context menu: Win32 TrackPopupMenu (avoids FMX thread/modal issues).

   Generic base class — no app-specific types.
   Derive for app-specific UpdateXxx methods that call SetTooltip / SetColorIcon / SetSparklineIcon.


======================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Classes,
  {$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  FMX.Forms;

{$IFDEF MSWINDOWS}
const
  WM_TRAYICON = WM_USER + 12;
{$ENDIF}

type
  TTrayColor       = (tcGreen, tcYellow, tcOrange, tcRed, tcGray);   // tcGray = neutral/idle (no data to color-code)
  TBalloonIconType = (biNone, biInfo, biWarning, biError);   // maps to NIIF_* constants

  TTrayIcon = class
  strict private
    { Platform-independent: the public AppName property reads FAppName, so these two
      must exist on non-Windows too (the unit compiles there as a no-op stub). }
    FAppName:     string;     // used for default tooltip
    FTooltip:     string;     // current tip
  {$IFDEF MSWINDOWS}
  strict private class var
    FWM_TASKBARCREATED: UINT;   // cached from RegisterWindowMessage('TaskbarCreated'); zero = not yet registered
  strict private
    FHelperHWnd:  THandle;
    FIconGreen:   THandle;
    FIconYellow:  THandle;
    FIconOrange:  THandle;
    FIconRed:     THandle;
    FIconGray:    THandle;
    FSparkIcon:   THandle;   // dynamic 16x16 sparkline icon (regenerated on SetSparklineIcon)
    FCustomIcon:  THandle;   // user-supplied via SetIcon/LoadIconFromResource (owned, destroyed in Destroy)
    FCurrentIcon: THandle;
    FOnLeftClick: TProc;
    FOnSettings:  TProc;
    FOnExit:      TProc;
    FLastLeftClickTick: UInt64;   // GetTickCount64 of last actioned left-click; debounces a double-click's 2nd WM_LBUTTONUP
    FInstalled:   Boolean;
    FHookedHWnd:  HWND;
    FOldFormProc: Pointer;
    FNewFormProc: Pointer;
    FUnhooked:    Boolean;    // set TRUE inside FormWndProc on WM_NCDESTROY
    FAppHWnd:     HWND;
    FOldAppProc:  Pointer;
    FNewAppProc:  Pointer;
    FHidingAppWnd: Boolean;   // reentrancy guard for HideAppWindow — SW_HIDE re-enters AppWndProc (WM_SHOWWINDOW arrives while the window still reads as visible)
    procedure HideAppWindow;  // re-assert "ApplicationHWND is never on screen"
    procedure TrayWndProc(var Msg: TMessage);
    procedure FormWndProc(var Msg: TMessage);
    procedure AppWndProc (var Msg: TMessage);
    procedure HandleLeftClick;   // debounced FOnLeftClick (swallows a double-click's 2nd up)
    procedure ShowContextMenu;
    procedure ApplyToShell;   // NIM_MODIFY with current icon + tip
  {$ENDIF}
  public
    constructor Create(const AAppName: string; const AOnLeftClick, AOnSettings, AOnExit: TProc);
    destructor  Destroy; override;
    procedure   Install;
    procedure   Uninstall;
    procedure   HookForm(const AForm: TCommonCustomForm);
    procedure   UnhookForm;

    { Setters for descendants — never virtual. App-specific classes derive and add their own UpdateXxx(...) methods that call these primitives. }
    procedure   SetTooltip(const S: string);
    procedure   SetColorIcon(Preset: TTrayColor);
    procedure   SetSparklineIcon(const Bars: array of Int64; StartIdx, ActiveCnt: Integer; MaxY: Int64; R, G, B: Byte);

    { Raw HICON — caller transfers ownership. Prior custom icon is destroyed.
      Pass 0 to revert to default (green preset). }
    procedure   SetIcon(AIcon: THandle);
    procedure   LoadIconFromResource(const ResName: string);    // LoadIcon from app .res; 0 = default icon
    procedure   UseFormIcon(const AForm: TCommonCustomForm);    // grab form's GCL_HICONSM as tray icon

    { Windows balloon notification — tray must be Installed first. }
    procedure   ShowBalloonHint(const Title, Text: string; Icon: TBalloonIconType = biInfo);

    property    AppName: string read FAppName;

    { Low-level icon builders — exposed for descendants that want custom icons }
    {$IFDEF MSWINDOWS}
    class function MakeColorIcon16(R, G, B: Byte): HICON; static;
    class function MakeSparklineIcon16(const Bars: array of Int64; StartIdx, ActiveCnt: Integer; MaxY: Int64; BarR, BarG, BarB: Byte): HICON; static;
    {$ENDIF}
  end;

implementation

{$IFDEF MSWINDOWS}
USES
  Winapi.ShellAPI,
  FMX.Platform.Win;

{ TrackPopupMenu with TPM_RETURNCMD returns the command ID as an integer, not a boolean.
  Delphi declares the return as BOOL (LongBool), causing the compiler to normalize any nonzero result to 1 — losing the actual command ID.
  Re-declare with UINT return to get the raw value. }
function TrackPopupMenuCmd(hMenu: HMENU; uFlags: UINT; x, y, nReserved: Integer; hWnd: HWND; prcRect: PRect): UINT; stdcall; external user32 name 'TrackPopupMenu';


{-- Icon helpers -------------------------------------------------------------------}

class function TTrayIcon.MakeColorIcon16(R, G, B: Byte): HICON;
var
  BmpColor, BmpMask: HBITMAP;
  DC, Mem:   HDC;
  Prev:      HGDIOBJ;
  Brush:     HBRUSH;
  Rect:      TRect;
  Info:      TIconInfo;
  MaskData:  array[0..63] of Byte;  // 16×16 @ 1bpp; DWORD-aligned rows = 4 bytes × 16 = 64
begin
  DC       := GetDC(0);
  BmpColor := CreateCompatibleBitmap(DC, 16, 16);
  Mem      := CreateCompatibleDC(DC);
  Prev     := SelectObject(Mem, BmpColor);

  Brush := CreateSolidBrush(RGB(R, G, B));
  Rect  := TRect.Create(0, 0, 16, 16);
  FillRect(Mem, Rect, Brush);
  DeleteObject(Brush);

  SelectObject(Mem, Prev);
  DeleteDC(Mem);
  ReleaseDC(0, DC);

  // Mask: all zeros = all pixels opaque (AND mask with all 0s → use color bitmap for all pixels)
  FillChar(MaskData, SizeOf(MaskData), 0);
  BmpMask := CreateBitmap(16, 16, 1, 1, @MaskData[0]);

  Info.fIcon    := True;
  Info.xHotspot := 0;
  Info.yHotspot := 0;
  Info.hbmMask  := BmpMask;
  Info.hbmColor := BmpColor;
  Result := CreateIconIndirect(Info);

  DeleteObject(BmpColor);
  DeleteObject(BmpMask);
end;


{ 16x16 icon showing one vertical bar per x-column; bars sampled from
  Bars[StartIdx..StartIdx+ActiveCnt-1]. Height scaled against MaxY.
  Background: dark gray. Bar color: RGB(BarR, BarG, BarB). }
class function TTrayIcon.MakeSparklineIcon16(const Bars: array of Int64; StartIdx, ActiveCnt: Integer; MaxY: Int64; BarR, BarG, BarB: Byte): HICON;
var
  BmpColor, BmpMask: HBITMAP;
  DC, Mem:           HDC;
  Prev:              HGDIOBJ;
  Brush, BgBrush:    HBRUSH;
  R:                 TRect;
  Info:              TIconInfo;
  MaskData:          array[0..63] of Byte;
  x, H, SrcIdx:      Integer;
  Val:               Int64;
begin
  DC       := GetDC(0);
  BmpColor := CreateCompatibleBitmap(DC, 16, 16);
  Mem      := CreateCompatibleDC(DC);
  Prev     := SelectObject(Mem, BmpColor);

  // Dark background (so colored bars stand out)
  BgBrush := CreateSolidBrush(RGB(32, 32, 32));
  R       := TRect.Create(0, 0, 16, 16);
  FillRect(Mem, R, BgBrush);
  DeleteObject(BgBrush);

  if (ActiveCnt > 0) and (MaxY > 0) then
    begin
    Brush := CreateSolidBrush(RGB(BarR, BarG, BarB));
    for x := 0 to 15 do
      begin
      // Nearest-neighbor sample: map x∈[0..15] onto ActiveCnt source buckets
      SrcIdx := StartIdx + (x * ActiveCnt) div 16;
      if (SrcIdx < 0) or (SrcIdx > High(Bars)) then Continue;
      Val := Bars[SrcIdx];
      if Val <= 0 then Continue;
      H := Val * 16 div MaxY;
      if H < 1  then H := 1;
      if H > 16 then H := 16;
      R := TRect.Create(x, 16 - H, x + 1, 16);
      FillRect(Mem, R, Brush);
      end;
    DeleteObject(Brush);
    end;

  SelectObject(Mem, Prev);
  DeleteDC(Mem);
  ReleaseDC(0, DC);

  FillChar(MaskData, SizeOf(MaskData), 0);
  BmpMask := CreateBitmap(16, 16, 1, 1, @MaskData[0]);

  Info.fIcon    := TRUE;
  Info.xHotspot := 0;
  Info.yHotspot := 0;
  Info.hbmMask  := BmpMask;
  Info.hbmColor := BmpColor;
  Result := CreateIconIndirect(Info);

  DeleteObject(BmpColor);
  DeleteObject(BmpMask);
end;


{-- TTrayIcon ---------------------------------------------------------------------}

constructor TTrayIcon.Create(const AAppName: string; const AOnLeftClick, AOnSettings, AOnExit: TProc);
begin
  inherited Create;
  FAppName     := AAppName;
  FTooltip     := AAppName;
  FOnLeftClick := AOnLeftClick;
  FOnSettings  := AOnSettings;
  FOnExit      := AOnExit;

  FIconGreen  := MakeColorIcon16(30,  180, 60);
  FIconYellow := MakeColorIcon16(230, 200, 20);
  FIconOrange := MakeColorIcon16(230, 120, 20);
  FIconRed    := MakeColorIcon16(210, 40,  40);
  FIconGray   := MakeColorIcon16(128, 128, 128);
  FCurrentIcon := FIconGreen;

  FHelperHWnd := AllocateHWnd(TrayWndProc);

  { Explorer broadcasts this message when taskbar is re-created (crash,
    DPI change, explorer.exe restart). Cache ID for comparison in TrayWndProc. }
  if FWM_TASKBARCREATED = 0
  then FWM_TASKBARCREATED := RegisterWindowMessage('TaskbarCreated');
end;


destructor TTrayIcon.Destroy;
begin
  try
    Uninstall;
    if FIconGreen  <> 0 then DestroyIcon(FIconGreen);
    if FIconYellow <> 0 then DestroyIcon(FIconYellow);
    if FIconOrange <> 0 then DestroyIcon(FIconOrange);
    if FIconRed    <> 0 then DestroyIcon(FIconRed);
    if FIconGray   <> 0 then DestroyIcon(FIconGray);
    if FSparkIcon  <> 0 then DestroyIcon(FSparkIcon);
    if FCustomIcon <> 0 then DestroyIcon(FCustomIcon);
  finally
    { Unhook form subclass BEFORE destroying helper HWND — while form HWND still alive. DeallocateHWnd must run even if above raised. }
    UnhookForm;
    if FHelperHWnd <> 0 then
      begin
        DeallocateHWnd(FHelperHWnd);
        FHelperHWnd := 0;
      end;
    inherited Destroy;
  end;
end;


{-- Form hook: hide taskbar entry + intercept minimize --------------------------}

procedure TTrayIcon.HookForm(const AForm: TCommonCustomForm);
{ Idempotent. Safe to call on every FormShow.
  FMX creates HWND lazily on first show — caller MUST invoke after HWND exists
  (from OnShow, NOT from AfterConstruction). Silently bails if HWND=0.
  Also handles HWND recreation (DPI/border/fullscreen change): if HWND differs
  from previously hooked, unhooks stale then re-hooks fresh. }
var
  ExStyle: NativeInt;
  Wnd:    THandle;
  AppHWnd: HWnd;
begin
  if AForm = nil then Exit;
  Wnd := FormToHWND(AForm);
  if Wnd = 0 then Exit;                             // HWND not realized yet
  if (Wnd = FHookedHWnd) and not FUnhooked then Exit;   // already hooked to same live HWND. FUnhooked=TRUE means that HWND died (WM_NCDESTROY) — an equal value now is a RECYCLED handle belonging to a new window, so fall through and re-hook.

  if FHookedHWnd <> 0 then UnhookForm;

  FHookedHWnd := Wnd;
  FUnhooked   := False;

  { Taskbar button belongs to FMX's hidden owner window (ApplicationHWND),
    NOT the form HWND. Applying WS_EX_TOOLWINDOW only to the form leaves
    Explorer showing the parent's button. Must target ApplicationHWND.
    Source: en.delphipraxis.net/topic/4308-how-to-completely-hide-application... }
  AppHWnd := ApplicationHWND;
  if AppHWnd <> 0 then
    begin
    ShowWindow(AppHWnd, SW_HIDE);
    ExStyle := GetWindowLongPtr(AppHWnd, GWL_EXSTYLE);
    ExStyle := (ExStyle or WS_EX_TOOLWINDOW) and not WS_EX_APPWINDOW;
    SetWindowLongPtr(AppHWnd, GWL_EXSTYLE, ExStyle);
    // Do NOT SW_SHOW — ApplicationHWND stays hidden as an owner-only window.

    { Subclass ApplicationHWND too: FMX's minimize-button click can route
      through ApplicationHWND (owner), bypassing our form subclass and
      producing the Windows 3.0 legacy stub at screen bottom-left. Catch
      SC_MINIMIZE / SIZE_MINIMIZED here too and hide the form HWND. }
    FAppHWnd    := AppHWnd;
    FOldAppProc := Pointer(GetWindowLongPtr(AppHWnd, GWL_WNDPROC));
    FNewAppProc := system.Classes.MakeObjectInstance(AppWndProc);
    SetWindowLongPtr(AppHWnd, GWL_WNDPROC, LONG_PTR(FNewAppProc));
    end;

  { NOTE: do NOT also set WS_EX_TOOLWINDOW on the form HWND.
    Tool-window ex-style changes minimize semantics — Windows dispatches
    minimize via a different path and our SC_MINIMIZE intercept is bypassed,
    producing the old "tiny rectangle" stub. ApplicationHWND alone is enough
    to suppress the taskbar button. }

  // Subclass to intercept minimize (FMX default = tiny-rect stub) + auto-unhook at NCDESTROY
  FOldFormProc := Pointer(GetWindowLongPtr(Wnd, GWL_WNDPROC));
  FNewFormProc := system.Classes.MakeObjectInstance(FormWndProc);
  SetWindowLongPtr(Wnd, GWL_WNDPROC, LONG_PTR(FNewFormProc));
end;


procedure TTrayIcon.HideAppWindow;
{ Re-assert the invariant "ApplicationHWND is never on screen".

  In a tray app the FMX application window is an owner-only window: HookForm hides it and marks
  it WS_EX_TOOLWINDOW so Explorer shows no taskbar button. A tool window that is visible AND
  minimized has nowhere to go, so Windows draws it as the legacy caption-bar stub in the
  bottom-left corner of the desktop — the "Windows 3.1 window" the user sees.

  Hiding it ONCE in HookForm is not enough: FMX un-hides it behind our back whenever the MAIN
  form is minimized, through calls that never pass through this window's procedure, so there is
  no single message to intercept —
    TPlatformWin.SetWindowState -> ShowWindow(ApplicationHWND, SW_MINIMIZE)              FMX.Platform.Win.pas
    TPlatformWin.MinimizeApp    -> SetWindowPos(ApplicationHWND, ..., SWP_SHOWWINDOW)    FMX.Platform.Win.pas
                                -> DefWindowProc(ApplicationHWND, WM_SYSCOMMAND, SC_MINIMIZE, 0)
  (that last one is called DIRECTLY on the handle, which is why a subclass cannot see it).
  Hence AppWndProc calls this on every message rather than enumerating the paths.

  VISIBLE **AND MINIMIZED** is the minimize signal, and the window must be left hidden AND
  NON-minimized. All three points below were measured on the running app (2026-08-11); each one
  was a separate failed attempt, so do not "simplify" them away:

    * Why not the WM_SIZE branch alone: if this window is left minimized, the SECOND minimize
      finds it already WS_MINIMIZED and changes only visibility. Windows then sends NO WM_SIZE
      (the size did not change), so a WM_SIZE-driven intercept silently stops working from the
      2nd minimize onwards — the form simply stayed on screen.

    * Why the minimized state MUST be cleared: Windows refuses to display an owned window while
      its owner is minimized. Leaving this window iconic left the tray icon unable to bring the
      form back at all — Show ran, and nothing appeared. Proven by clearing it from outside the
      process: the very next tray click restored the form.

    * Why SW_SHOWNOACTIVATE and why the form is hidden LAST: un-minimizing makes Windows re-show
      the owned windows it hid, so the form pops back up unless it is hidden after that step.
      SW_SHOWNOACTIVATE un-minimizes without taking focus, and this window is 0x0
      (CreateAppHandle, FMX.Platform.Win.pas:2982), so nothing is ever drawn. }
VAR Minimizing: Boolean;
begin
  if FHidingAppWnd then Exit;                      // already inside the ShowWindow calls below
  if (FAppHWnd = 0) or not IsWindow(FAppHWnd) then Exit;
  if not IsWindowVisible(FAppHWnd) then Exit;      // invariant already holds — the common case

  Minimizing := IsIconic(FAppHWnd);

  FHidingAppWnd := True;
  try
    if Minimizing
    then ShowWindow(FAppHWnd, SW_SHOWNOACTIVATE);  // leave it restorable for the next tray click

    ShowWindow(FAppHWnd, SW_HIDE);                 // kill the stub

    if Minimizing
    and (FHookedHWnd <> 0)
    and not FUnhooked                              // FUnhooked=TRUE: that HWND died (WM_NCDESTROY); an IsWindow hit now is a RECYCLED handle owned by another window — do not hide it
    and IsWindow(FHookedHWnd)
    then ShowWindow(FHookedHWnd, SW_HIDE);         // complete the minimize-to-tray — must be last
  finally
    FHidingAppWnd := False;
  end;
end;


procedure TTrayIcon.AppWndProc(var Msg: TMessage);
{ ApplicationHWND subclass — catches minimize that FMX routes through the
  owner window. Hide the form HWND instead of allowing default minimize
  (which would produce the Windows 3.0 desktop-icon stub). }
var
  IsMinimize: Boolean;
  LocalHWnd:  HWND;
begin
  LocalHWnd := FAppHWnd;   // capture before potential WM_NCDESTROY zero-out

  HideAppWindow;   // see there: FMX re-shows this window behind our back on every minimize

  IsMinimize :=
    ((Msg.Msg = WM_SYSCOMMAND) and ((Msg.WParam and $FFF0) = SC_MINIMIZE)) or
    ((Msg.Msg = WM_SIZE)       and  (Msg.WParam = SIZE_MINIMIZED));

  { Swallow the minimize whether or not the form is still around: letting it reach FMX would run
    TPlatformWin.MinimizeApp, which re-shows this window (see HideAppWindow) and re-creates the stub. }
  if IsMinimize then
    begin
      if (FHookedHWnd <> 0) and not FUnhooked and IsWindow(FHookedHWnd)   // not FUnhooked: dead form HWND may be recycled for a foreign window — never hide that
      then ShowWindow(FHookedHWnd, SW_HIDE);
      Msg.Result := 0;
      Exit;
    end;

  { Guard against FMX un-hiding the form behind our back.

    WHAT IT DOES: while the hooked form window is hidden, answer WM_ACTIVATEAPP(TRUE) with plain
    DefWindowProc instead of forwarding to FMX. FMX's handler for this message is DefWindowProc +
    TPlatformWin.RestoreApp and nothing else (FMX.Platform.Win.pas), so this skips exactly
    the RestoreApp and changes nothing else.

    WHY RestoreApp is dangerous here: it ends in TCommonCustomForm.Activate, whose guard
    (FMX.Forms.pas) tests the FMX Visible FLAG — still TRUE after our raw-ShowWindow hide — and
    TPlatformWin.Activate then finds the window invisible and calls ShowWindow(SW_RESTORE).
    That chain puts a tray-hidden form back on screen. It was seen live on 2026-08-10, while the
    owner window's iconic state was being cleared from outside the process.

    WHICH branch of RestoreApp runs is decided at the moment the message arrives:
      - Screen.ActiveForm is COMPUTED, not stored — first form with Visible AND Active
        (FMX.Forms.pas). A tray-hidden form is Visible-flag TRUE but Active FALSE (the raw
        hide made Windows send WM_ACTIVATE(WA_INACTIVE) -> Deactivate -> FActive:=False), so
        it reads NIL and RestoreApp falls to the GetActiveWindow branch.
      - GetActiveWindow <> 0 and <> ApplicationHWND -> the ACTIVE window is activated,
        not the main form.  GetActiveWindow = 0 or ApplicationHWND -> Application.MainForm.Activate
        — the resurrecting branch.
    Measured 2026-08-11 with a Win32 probe reproducing this window topology: on a tray RIGHT-click
    (ShowContextMenu's SetForegroundWindow(FHelperHWnd)) and when a secondary form is shown while
    the main form is hidden, GetActiveWindow already returns the helper / that form by the time
    WM_ACTIVATEAPP is dispatched — so those two routes take the SAFE branch. The resurrecting branch
    was NOT reproduced from inside the process. Same probe also confirmed the two preconditions this
    block assumes: a hidden WS_EX_TOOLWINDOW top-level window DOES receive WM_ACTIVATEAPP, and
    SetForegroundWindow on a hidden window succeeds.
    So this is a cheap guard on the residual branch, not a fix for a reproduced in-app path. Keep it
    unless you can show the resurrecting branch is unreachable for every caller of this base class.

    It cannot fire during a restore: ShowWindow sets WS_VISIBLE BEFORE the activation that dispatches
    WM_ACTIVATEAPP (same probe — the owner saw wParam=1 with IsWindowVisible(form) already TRUE), so
    the test below is FALSE by then. And even if it did fire, the restore does not depend on
    RestoreApp: TCommonCustomForm.Show calls Activate itself (FMX.Forms.pas) and OnTrayLeftClick
    then calls SetForegroundWindow. With the form hidden FMX-side too (Visible flag FALSE, e.g. after
    the tray-click Hide) RestoreApp is a no-op anyway — Activate's guard fails. }
  if (Msg.Msg = WM_ACTIVATEAPP) and (Msg.WParam <> 0)
  and (FHookedHWnd <> 0) and not FUnhooked and IsWindow(FHookedHWnd)
  and not IsWindowVisible(FHookedHWnd) then
    begin
      Msg.Result := DefWindowProc(LocalHWnd, Msg.Msg, Msg.WParam, Msg.LParam);
      Exit;
    end;

  { Auto-unhook on ApplicationHWND death (mirrors FormWndProc pattern) }
  if Msg.Msg = WM_NCDESTROY then
    begin
    if Assigned(FOldAppProc)
    then SetWindowLongPtr(LocalHWnd, GWL_WNDPROC, LONG_PTR(FOldAppProc));
    FAppHWnd := 0;   // mark dead so UnhookForm won't touch it
    end;

  if Assigned(FOldAppProc)
  then Msg.Result := CallWindowProc(FOldAppProc, LocalHWnd, Msg.Msg, Msg.WParam, Msg.LParam)
  else Msg.Result := DefWindowProc(LocalHWnd, Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TTrayIcon.UnhookForm;
var
  Current:     Pointer;
  CanFreeStub: Boolean;
begin
  { Unhook ApplicationHWND subclass first (independent of form HWND lifetime) }
  if Assigned(FNewAppProc) then
    begin
    CanFreeStub := True;
    if (FAppHWnd <> 0) and IsWindow(FAppHWnd) then
      begin
        Current := Pointer(GetWindowLongPtr(FAppHWnd, GWL_WNDPROC));
        if (Current = FNewAppProc) and Assigned(FOldAppProc)
        then SetWindowLongPtr(FAppHWnd, GWL_WNDPROC, LONG_PTR(FOldAppProc))
        else CanFreeStub := False;
      end;
    if CanFreeStub
    then system.Classes.FreeObjectInstance(FNewAppProc);
    FNewAppProc := nil;
    end;
  FOldAppProc := nil;
  FAppHWnd    := 0;

  if FHookedHWnd = 0 then Exit;

  CanFreeStub := True;   // default: safe to free (window dead / we're top)

  if Assigned(FNewFormProc) then
    begin
    { If WM_NCDESTROY already auto-unhooked us (FUnhooked=TRUE), HWND is
      dying/dead — safe to free stub, no more dispatches possible.
      Otherwise if HWND alive: only free stub if we're still top of chain
      (SetWindowLongPtr successfully restores). If another subclass sits
      on top, we CANNOT free — Windows would call freed trampoline. Leak
      it instead (one-shot at shutdown, better than AV).

      DEFERRED (2026-07-07): migrating this manual GWL_WNDPROC subclass to
      comctl32 SetWindowSubclass/RemoveWindowSubclass would remove the
      dangling-trampoline hazard entirely (the OS owns the chain). Not done:
      in this app only ONE subclass is installed and no foreign code layers
      on top, so the leak branch is unreachable. Revisit if this base is
      reused where third-party subclassing is possible. }
    if (not FUnhooked) and IsWindow(FHookedHWnd) then
      begin
        Current := Pointer(GetWindowLongPtr(FHookedHWnd, GWL_WNDPROC));
        if (Current = FNewFormProc) and Assigned(FOldFormProc)
        then SetWindowLongPtr(FHookedHWnd, GWL_WNDPROC, LONG_PTR(FOldFormProc))
        else CanFreeStub := False;    // layer above us — leak stub, can't safely free
      end;

    if CanFreeStub
    then system.Classes.FreeObjectInstance(FNewFormProc);
    FNewFormProc := nil;
    end;
  FOldFormProc := nil;
  FHookedHWnd  := 0;
  FUnhooked    := False;
end;


procedure TTrayIcon.FormWndProc(var Msg: TMessage);
{ Canonical MSDN subclass pattern: auto-unhook at WM_NCDESTROY, the last
  message the HWND ever receives. Restore original proc BEFORE dispatching
  NCDESTROY so further routing (unlikely but possible) bypasses our stub.
  Stub memory freed later in destructor, by which time no callbacks possible. }
var
  IsMinimize: Boolean;
begin
  { Cover all minimize paths:
    - WM_SYSCOMMAND / SC_MINIMIZE:  system menu, minimize button, hotkey
    - WM_SIZE / SIZE_MINIMIZED:     programmatic ShowWindow(SW_MINIMIZE),
                                    Application.Minimize, any other path }
  IsMinimize :=
    ((Msg.Msg = WM_SYSCOMMAND) and ((Msg.WParam and $FFF0) = SC_MINIMIZE)) or
    ((Msg.Msg = WM_SIZE)       and  (Msg.WParam = SIZE_MINIMIZED));

  if IsMinimize then
    begin
    ShowWindow(FHookedHWnd, SW_HIDE);
    Msg.Result := 0;
    Exit;
    end;

  if Msg.Msg = WM_NCDESTROY then
    begin
    // Restore original proc — guarantees no further dispatch into our stub.
    if Assigned(FOldFormProc)
    then SetWindowLongPtr(FHookedHWnd, GWL_WNDPROC, LONG_PTR(FOldFormProc));
    FUnhooked := True;
    end;

  if Assigned(FOldFormProc)
  then Msg.Result := CallWindowProc(FOldFormProc, FHookedHWnd, Msg.Msg, Msg.WParam, Msg.LParam)
  else Msg.Result := DefWindowProc(FHookedHWnd, Msg.Msg, Msg.WParam, Msg.LParam);
end;


procedure TTrayIcon.Install;
var
  Nid: TNotifyIconData;
begin
  if FInstalled then Exit;
  if FHelperHWnd = 0 then Exit;          // AllocateHWnd failed — nothing to route to
  FillChar(Nid, SizeOf(Nid), 0);
  Nid.cbSize           := SizeOf(Nid);
  Nid.Wnd              := FHelperHWnd;
  Nid.uID              := 1;
  Nid.uFlags           := NIF_ICON or NIF_TIP or NIF_MESSAGE;
  Nid.uCallbackMessage := WM_TRAYICON;
  Nid.hIcon            := FCurrentIcon;
  StrPLCopy(Nid.szTip, FTooltip, SizeOf(Nid.szTip) div SizeOf(Char) - 1);
  // Track actual result — silent failure leaves us in inconsistent state otherwise
  FInstalled := Shell_NotifyIcon(NIM_ADD, @Nid);

  { NIM_ADD talks to Explorer via SendMessageTimeout (4-7s). During a logon storm it can
    return FALSE (ERROR_TIMEOUT) even though the shell DID add the icon. A successful
    NIM_MODIFY proves the icon exists; if the icon is genuinely absent, MODIFY fails too
    and FInstalled stays FALSE so the caller may retry Install later (it is idempotent).
    https://learn.microsoft.com/en-us/windows/win32/api/shellapi/nf-shellapi-shell_notifyiconw }
  if NOT FInstalled
  then FInstalled := Shell_NotifyIcon(NIM_MODIFY, @Nid);
end;


procedure TTrayIcon.Uninstall;
var
  Nid: TNotifyIconData;
begin
  if not FInstalled then Exit;
  FillChar(Nid, SizeOf(Nid), 0);
  Nid.cbSize := SizeOf(Nid);
  Nid.Wnd    := FHelperHWnd;
  Nid.uID    := 1;
  Shell_NotifyIcon(NIM_DELETE, @Nid);
  FInstalled := False;
end;


procedure TTrayIcon.ApplyToShell;
var
  Nid: TNotifyIconData;
begin
  if not FInstalled then Exit;
  FillChar(Nid, SizeOf(Nid), 0);
  Nid.cbSize := SizeOf(Nid);
  Nid.Wnd    := FHelperHWnd;
  Nid.uID    := 1;
  Nid.uFlags := NIF_ICON or NIF_TIP;
  Nid.hIcon  := FCurrentIcon;
  StrPLCopy(Nid.szTip, FTooltip, SizeOf(Nid.szTip) div SizeOf(Char) - 1);
  Shell_NotifyIcon(NIM_MODIFY, @Nid);
end;


procedure TTrayIcon.SetTooltip(const S: string);
begin
  if S = FTooltip then Exit;
  FTooltip := S;
  ApplyToShell;
end;


procedure TTrayIcon.SetColorIcon(Preset: TTrayColor);
var
  NewIcon: THandle;
begin
  case Preset of
    tcGreen:  NewIcon := FIconGreen;
    tcYellow: NewIcon := FIconYellow;
    tcOrange: NewIcon := FIconOrange;
    tcGray:   NewIcon := FIconGray;
  else        NewIcon := FIconRed;
  end;
  if NewIcon = FCurrentIcon then Exit;
  FCurrentIcon := NewIcon;
  ApplyToShell;
end;


procedure TTrayIcon.SetSparklineIcon(const Bars: array of Int64; StartIdx, ActiveCnt: Integer; MaxY: Int64; R, G, B: Byte);
begin
  // Rebuild each call — input data varies
  if FSparkIcon <> 0 then
    begin
    DestroyIcon(FSparkIcon);
    FSparkIcon := 0;
    end;
  FSparkIcon   := MakeSparklineIcon16(Bars, StartIdx, ActiveCnt, MaxY, R, G, B);
  FCurrentIcon := FSparkIcon;
  ApplyToShell;
end;


procedure TTrayIcon.SetIcon(AIcon: THandle);
{ Takes ownership of AIcon — prior custom icon destroyed.
  AIcon=0 → revert to green preset. Caller must not DestroyIcon afterwards. }
begin
  if FCustomIcon <> 0 then
    begin
    DestroyIcon(FCustomIcon);
    FCustomIcon := 0;
    end;
  if AIcon = 0
  then FCurrentIcon := FIconGreen
  else
    begin
    FCustomIcon  := AIcon;
    FCurrentIcon := FCustomIcon;
    end;
  ApplyToShell;
end;


procedure TTrayIcon.LoadIconFromResource(const ResName: string);
{ Load HICON from app's .res via LoadIcon(hInstance, Name). Ownership via SetIcon. }
var
  H: HICON;
begin
  H := LoadIcon(hInstance, PChar(ResName));
  { LoadIcon returns a SHARED icon — DestroyIcon must never be called on those, but
    SetIcon takes ownership and destroys. Hand SetIcon its own private copy instead.
    https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-destroyicon }
  if H <> 0
  then H := CopyIcon(H);
  SetIcon(H);   // SetIcon handles H=0 (revert to default)
end;


procedure TTrayIcon.UseFormIcon(const AForm: TCommonCustomForm);
{ Pull form's small class icon (GCL_HICONSM) and use it. Does NOT take ownership —
  class icon belongs to the window class. Copy it so SetIcon can manage lifetime. }
var
  Wnd:  HWND;
  Orig: HICON;
  Copy: HICON;
begin
  if AForm = nil then Exit;
  Wnd := FormToHWND(AForm);
  if Wnd = 0 then Exit;
  Orig := GetClassLongPtr(Wnd, GCL_HICONSM);
  if Orig = 0
  then Orig := GetClassLongPtr(Wnd, GCL_HICON);
  if Orig = 0 then Exit;
  Copy := CopyIcon(Orig);   // SetIcon will destroy — must be our own handle
  if Copy <> 0
  then SetIcon(Copy);
end;


procedure TTrayIcon.ShowBalloonHint(const Title, Text: string; Icon: TBalloonIconType);
const
  FlagMap: array[TBalloonIconType] of UINT = (NIIF_NONE, NIIF_INFO, NIIF_WARNING, NIIF_ERROR);
var
  Nid: TNotifyIconData;
begin
  if not FInstalled then Exit;
  FillChar(Nid, SizeOf(Nid), 0);
  Nid.cbSize      := SizeOf(Nid);
  Nid.Wnd         := FHelperHWnd;
  Nid.uID         := 1;
  Nid.uFlags      := NIF_INFO;
  Nid.dwInfoFlags := FlagMap[Icon];
  StrPLCopy(Nid.szInfo,      Text,  SizeOf(Nid.szInfo)      div SizeOf(Char) - 1);
  StrPLCopy(Nid.szInfoTitle, Title, SizeOf(Nid.szInfoTitle) div SizeOf(Char) - 1);
  Shell_NotifyIcon(NIM_MODIFY, @Nid);
end;


procedure TTrayIcon.TrayWndProc(var Msg: TMessage);
begin
  { Defensive: Windows shutdown messages dispatched to helper HWND must return nonzero to avoid halting shutdown (CoolTrayIcon pattern). }
  case Msg.Msg of
    WM_CLOSE, WM_QUIT, WM_DESTROY, WM_NCDESTROY,
    WM_QUERYENDSESSION, WM_ENDSESSION:
      begin
      Msg.Result := 1;
      Exit;
      end;
  end;

  { Explorer.exe restarted — re-register tray icon (else icon vanishes forever).
    Broadcast message, so compare against cached ID from RegisterWindowMessage. }
  if (FWM_TASKBARCREATED <> 0) and (Msg.Msg = FWM_TASKBARCREATED) then
    begin
    FInstalled := False;      // force Install to re-add
    Install;
    Msg.Result := 0;
    Exit;
    end;

  if Msg.Msg = WM_TRAYICON then
    begin
      case Msg.LParam of
        WM_LBUTTONUP: HandleLeftClick;
        WM_RBUTTONUP: ShowContextMenu;
      end;
      Msg.Result := 0;
    end
  else
    Msg.Result := DefWindowProc(FHelperHWnd, Msg.Msg, Msg.WParam, Msg.LParam);
end;


procedure TTrayIcon.HandleLeftClick;
{ The classic tray callback sends WM_LBUTTONUP once per physical click, so a
  double-click arrives as TWO ups. FOnLeftClick toggles the form show/hide, so a
  double-click would toggle twice (show+hide) = a visible flash. Swallow a 2nd
  click that lands within the system double-click time; the first click still
  fires instantly, so single-click feel is unchanged. GetDoubleClickTime is the
  user's own single-vs-double boundary, so this matches OS-wide semantics. }
var
  Now64: UInt64;
begin
  Now64 := GetTickCount64;
  if (Now64 - FLastLeftClickTick) < GetDoubleClickTime then Exit;   // 2nd up of a double-click — ignore
  FLastLeftClickTick := Now64;
  if Assigned(FOnLeftClick) then FOnLeftClick();
end;


procedure TTrayIcon.ShowContextMenu;
var
  Menu:    HMENU;
  CurPos:  TPoint;
  Cmd:     UINT;
begin
  Menu := CreatePopupMenu;
  if Menu = 0 then Exit;
  try
    AppendMenu(Menu, MF_STRING, 1, 'Show / Hide');
    AppendMenu(Menu, MF_SEPARATOR, 0, nil);
    AppendMenu(Menu, MF_STRING, 2, 'Settings...');
    AppendMenu(Menu, MF_SEPARATOR, 0, nil);
    AppendMenu(Menu, MF_STRING, 3, 'Exit');

    GetCursorPos(CurPos);
    SetForegroundWindow(FHelperHWnd);  // Required for proper menu dismiss
    Cmd := TrackPopupMenuCmd(Menu, TPM_RETURNCMD or TPM_RIGHTBUTTON or TPM_BOTTOMALIGN,
                            CurPos.X, CurPos.Y, 0, FHelperHWnd, nil);
    PostMessage(FHelperHWnd, WM_NULL, 0, 0);  // Fix Windows popup-menu dismiss bug

    case Cmd of
      1: if Assigned(FOnLeftClick) then FOnLeftClick();
      2: if Assigned(FOnSettings)  then FOnSettings();
      3: if Assigned(FOnExit)      then FOnExit();
    end;
  finally
    DestroyMenu(Menu);
  end;
end;

{$ELSE}

{ Stub: no-op tray for non-Windows platforms }
constructor TTrayIcon.Create(const AAppName: string; const AOnLeftClick, AOnSettings, AOnExit: TProc); begin inherited Create; FAppName:= AAppName; FTooltip:= AAppName; end;
destructor  TTrayIcon.Destroy;                                                                         begin inherited Destroy; end;
procedure   TTrayIcon.Install;                                                                         begin end;
procedure   TTrayIcon.Uninstall;                                                                       begin end;
procedure   TTrayIcon.HookForm(const AForm: TCommonCustomForm);                                        begin end;
procedure   TTrayIcon.UnhookForm;                                                                      begin end;
procedure   TTrayIcon.SetTooltip(const S: string);                                                     begin end;
procedure   TTrayIcon.SetColorIcon(Preset: TTrayColor);                                                begin end;
procedure   TTrayIcon.SetSparklineIcon(const Bars: array of Int64; StartIdx, ActiveCnt: Integer; MaxY: Int64; R, G, B: Byte); begin end;
procedure   TTrayIcon.SetIcon(AIcon: THandle);                                                         begin end;
procedure   TTrayIcon.LoadIconFromResource(const ResName: string);                                     begin end;
procedure   TTrayIcon.UseFormIcon(const AForm: TCommonCustomForm);                                     begin end;
procedure   TTrayIcon.ShowBalloonHint(const Title, Text: string; Icon: TBalloonIconType);              begin end;

{$ENDIF}

end.
