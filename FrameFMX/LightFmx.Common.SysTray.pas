unit LightFmx.Common.SysTray;

{======================================================================================================
   2026.04
   www.GabrielMoraru.com

   System tray icon via Win32 Shell_NotifyIcon + AllocateHWnd message window.

   FMX does NOT expose the main form's HWND message loop, so we create a dedicated hidden helper window with AllocateHWnd to receive WM_TRAYICON notifications.

   Icon:         16×16 GDI bitmap (solid color or sparkline).
   Context menu: Win32 TrackPopupMenu (avoids FMX thread/modal issues).

   Generic base class — no app-specific types. Derive for app-specific
   UpdateXxx methods that call SetTooltip / SetColorIcon / SetSparklineIcon.


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
  TTrayColor       = (tcGreen, tcYellow, tcOrange, tcRed);
  TBalloonIconType = (biNone, biInfo, biWarning, biError);   // maps to NIIF_* constants

  TTrayIcon = class
  {$IFDEF MSWINDOWS}
  strict private class var
    FWM_TASKBARCREATED: UINT;   // cached from RegisterWindowMessage('TaskbarCreated'); zero = not yet registered
  strict private
    FHelperHWnd:  THandle;
    FIconGreen:   THandle;
    FIconYellow:  THandle;
    FIconOrange:  THandle;
    FIconRed:     THandle;
    FSparkIcon:   THandle;   // dynamic 16x16 sparkline icon (regenerated on SetSparklineIcon)
    FCustomIcon:  THandle;   // user-supplied via SetIcon/LoadIconFromResource (owned, destroyed in Destroy)
    FCurrentIcon: THandle;
    FOnLeftClick: TProc;
    FOnSettings:  TProc;
    FOnExit:      TProc;
    FInstalled:   Boolean;
    FHookedHWnd:  HWND;
    FOldFormProc: Pointer;
    FNewFormProc: Pointer;
    FUnhooked:    Boolean;    // set TRUE inside FormWndProc on WM_NCDESTROY
    FAppHWnd:     HWND;
    FOldAppProc:  Pointer;
    FNewAppProc:  Pointer;
    FAppName:     string;     // used for default tooltip
    FTooltip:     string;     // current tip
    procedure TrayWndProc(var Msg: TMessage);
    procedure FormWndProc(var Msg: TMessage);
    procedure AppWndProc (var Msg: TMessage);
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

// TrackPopupMenu with TPM_RETURNCMD returns the command ID as an integer,
// not a boolean. Delphi declares the return as BOOL (LongBool), causing
// the compiler to normalize any nonzero result to 1 — losing the actual
// command ID. Re-declare with UINT return to get the raw value.
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
  if Wnd = FHookedHWnd then Exit;                   // already hooked to same HWND

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


procedure TTrayIcon.AppWndProc(var Msg: TMessage);
{ ApplicationHWND subclass — catches minimize that FMX routes through the
  owner window. Hide the form HWND instead of allowing default minimize
  (which would produce the Windows 3.0 desktop-icon stub). }
var
  IsMinimize: Boolean;
  LocalHWnd:  HWND;
begin
  LocalHWnd := FAppHWnd;   // capture before potential WM_NCDESTROY zero-out

  IsMinimize :=
    ((Msg.Msg = WM_SYSCOMMAND) and ((Msg.WParam and $FFF0) = SC_MINIMIZE)) or
    ((Msg.Msg = WM_SIZE)       and  (Msg.WParam = SIZE_MINIMIZED));

  if IsMinimize and (FHookedHWnd <> 0) and IsWindow(FHookedHWnd) then
    begin
      ShowWindow(FHookedHWnd, SW_HIDE);
      Msg.Result := 0;
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
      it instead (one-shot at shutdown, better than AV). }
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
        WM_LBUTTONUP: if Assigned(FOnLeftClick) then FOnLeftClick();
        WM_RBUTTONUP: ShowContextMenu;
      end;
      Msg.Result := 0;
    end
  else
    Msg.Result := DefWindowProc(FHelperHWnd, Msg.Msg, Msg.WParam, Msg.LParam);
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
constructor TTrayIcon.Create(const AAppName: string; const AOnLeftClick, AOnSettings, AOnExit: TProc); begin inherited Create; end;
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
