UNIT FormDrawing;

{=============================================================================================================
   2026.01.29
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Creates a special hidden window that sits behind desktop icons, allowing dynamic wallpaper painting (animated wallpapers, video backgrounds, etc).

   COMPATIBILITY:
     Platform      | Behavior
     --------------|----------
     Windows XP    | Unknown
     Windows Vista | Unknown
     Windows 7     | Aero=over icons, No Aero=under icons
     Windows 8+    | Under icons (permanent until shutdown)
     Windows 10/11 | Under icons (permanent until shutdown)

  Persistence:
     On Win7 the drawing is temporary (gets deleted as soon as the desktop is refreshed)
     On Win10 it is permanent (then drawing stays there even if I move then icons or refresh the desktop)
     However, there are some persistence issues on Win10:
               1. The wallpaper is lost at Windows shutdown
               2. The accent color (color of all windows) is not properly set because Windows does not see this wallpaper.

  Speed:
     It can draw about 150 frames per second, when bitmap is a 608x1200 pixels

  Note:
     The window is lost if the user activates the "Personalize" menu (right click on desktop) and changes the current window theme.
     Fortunately, the window is restored when the next animated wallpaper is applied.
________________________________________________________________________________

   HOW IT WORKS:
     1. Creates a special WorkerW using undocumented Windows message $052C
     2. Hides that WorkerW window
     3. Inserts our form as a child of Progman (behind desktop icons)
     4. Draws on our form's canvas

  WINDOW HIERARCHY (Win7 Aero enabled):
     Desktop
       -> Progman
         -> SHELLDLL_DefView
           -> SysListView32 (List view)   <-- this wnd contains the icons
             ->  SysHeader32 (Header).

  WINDOW HIERARCHY (Win7-11 x64):
    '', WorkerW
       '', SHELLDLL_DefView
           'FolderView' SysListView32  <-- this wnd contains the icons
              '', SysHeader32

  SOURCES:
      stackoverflow.com/questions/34952967/drawing-to-the-desktop-via-injection
      codeproject.com/Articles/856020/Draw-Behind-Desktop-Icons-in-Windows
      github.com/payalord/ZMatrix/issues/1     (open source)
--------------------------------------------------------------------------------------------------------------
  TESTER:
     c:\Projects\Project Testers\gr cmDesktop.pas Tester\
=============================================================================================================}

INTERFACE

USES
   Winapi.Windows, System.Classes, System.SysUtils, System.Types,
   Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls,
   LightVcl.Visual.AppDataForm;

CONST
  {TODO 1: Move TAG_DONT_TRANSLATE to translator lib }
  TAG_DONT_TRANSLATE = 128;  // Used by LightVcl.Translate.pas to skip translation
  MAX_WORKERW_SEARCH = 100;  // Maximum iterations when searching for WorkerW windows

TYPE
  TDrawingForm = class(TLightForm)      { This is the 'secret' form ($052C) on which we paint the animation }
    pnlVideoDisplay: TPanel;
  private
    function initPaintingBkgWnd: Boolean;
  public
    ExpandOnAllMon: Boolean;
    procedure HidePlayer;
    procedure ShowPlayer(ViewPortRect: TRect);
    procedure ClearBkg;
    procedure Repair;
    constructor Create(aOwner: TComponent); override;
    class function GetHiddenWindow(BkgColor: TColor; ExpandOnAllMon: Boolean): TDrawingForm;
  end;


IMPLEMENTATION {$R *.dfm}

USES
   LightVcl.Common.WinVersion, LightVcl.Common.WinVersionAPI, LightVcl.Common.ExeVersion, LightVcl.Common.Dialogs,
   LightCore.AppData, LightVcl.Visual.AppData, LightVcl.Visual.INIFile, LightVcl.Graph.Desktop;

VAR
  WorkerW: HWND;  // Set by EnumFindDefView callback

{ EnumWindows callback: searches ALL top-level windows for SHELLDLL_DefView.
  This works regardless of whether DefView is under WorkerW or Progman.
  The old FindWindowEx-only loop only searched WorkerW children and failed
  when DefView was directly under Progman (fresh boot, $052C message failure). }
function EnumFindDefView(Handle: HWND; Param: LPARAM): BOOL; stdcall;
var hDefView: HWND;
begin
  Result:= TRUE; // continue enumeration
  hDefView:= FindWindowEx(Handle, 0, 'SHELLDLL_DefView', NIL);
  if hDefView <> 0 then
    begin
      // Found DefView - now get the WorkerW that comes right after this window in Z-order
      WorkerW:= FindWindowEx(0, Handle, 'WorkerW', NIL);
      Result:= FALSE; // stop enumeration
    end;
end;


{ Creates and returns a hidden window for painting behind desktop icons.
  Parameters:
    ExpandOnAllMon - True to span all monitors, False for primary only

  Note: Close it with FreeAndNil(DrawingForm), not with DrawingForm.Close. }
class function TDrawingForm.GetHiddenWindow(BkgColor: TColor; ExpandOnAllMon: Boolean): TDrawingForm;
begin
  Result:= TDrawingForm.Create(NIL);     { Create a new window, that we put behind the desktop icons, as a child of Progman. We draw the animation on this window }
  Result.Color:= BkgColor;
  Result.ExpandOnAllMon:= ExpandOnAllMon;
  if NOT Result.initPaintingBkgWnd
  then MessageError('The program was not allowed to created a special window for animations. Please make sure you don''t use other programs that interfere with your desktop!');
end;


{ Creates the drawing form with settings optimized for desktop painting.
  Note: The form is created visible by default but will be positioned behind icons. }
constructor TDrawingForm.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Visible       := TRUE;
  ClearBkg;
  BorderIcons   := [];
  BorderStyle   := bsNone;
  DoubleBuffered:= TRUE;
  Caption       := 'frmBxDraw';
  Name          := 'frmBxDraw';
  Tag           := TAG_DONT_TRANSLATE;  // Skip translation in LightVcl.Translate.pas
  StyleElements := [];                  // The form will be painted to the skin color if skins are enabled. My color will be ignored. Therefore we need to disable skins for this form!
  Assert(Align <> alClient, 'Align=alClient causes issues with multi-monitor setups where monitors have different resolutions');
end;


{ Clears the entire drawing surface with the form's background color.
  Affects all screens if ExpandOnAllMon is True. }
procedure TDrawingForm.ClearBkg;
begin
  Canvas.Brush.Color:= Color;
  Canvas.FillRect(Rect(0, 0, ClientWidth, ClientHeight));
end;


{ Hides the video display panel by moving it off-screen.
  This is a workaround for video player visibility issues. }
procedure TDrawingForm.HidePlayer;
begin
  pnlVideoDisplay.Left:= 20000;  // Move far off-screen
end;


{ Shows the video display panel at the specified viewport position.
  Parameters: ViewPortRect - Rectangle defining the display area (uses Left position) }
procedure TDrawingForm.ShowPlayer(ViewPortRect: TRect);
begin
  pnlVideoDisplay.Visible:= TRUE;
  pnlVideoDisplay.Left:= ViewPortRect.Left;
end;


{ Initializes the background painting window by finding/creating the special WorkerW window and positioning our form behind the desktop icons.
  Uses EnumWindows to search ALL top-level windows for SHELLDLL_DefView, which works regardless of whether DefView is under WorkerW or Progman. }
function TDrawingForm.initPaintingBkgWnd: Boolean;
var
   Progman  : HWND;
   MsgResult: DWORD_PTR;
   Attempt  : Integer;
CONST
  MaxRetries   = 5;
  RetryDelayMs = 500;
begin
  Progman:= ProgManHandle;
  AppData.LogVerb('Progman hwnd: ' + IntToStr(Progman));

  if NOT IsWindows7Up then
    begin
      AppData.LogVerb('Windows version too old - requires Windows 7+');
      EXIT(FALSE);
    end;

  { Validate Progman handle before sending message }
  if Progman = 0 then
    begin
      AppData.LogError('Progman window not found! Explorer may not be running.');
      EXIT(FALSE);
    end;

  { Create special WorkerW using the undocumented parameter $052C.
    Send twice (LPARAM 0 and 1) for robustness - multiple wallpaper engines
    (Lively, SpoutWallpaper) found this more reliable. }
  AppData.LogVerb('Sending $052C to ProgMan (LPARAM=0)');
  SendMessageTimeout(Progman, $052C, 0, 0, SMTO_NORMAL, 1000, @MsgResult);

  AppData.LogVerb('Sending $052C to ProgMan (LPARAM=1)');
  SendMessageTimeout(Progman, $052C, 0, 1, SMTO_NORMAL, 1000, @MsgResult);

  { Use EnumWindows to search ALL top-level windows for SHELLDLL_DefView.
    This works regardless of whether DefView is under WorkerW or Progman
    (on fresh boot, DefView is typically under Progman, not WorkerW).
    Retry with delay because Windows 11 24H2 can be slow to create WorkerW. }
  WorkerW:= 0;
  for Attempt:= 1 to MaxRetries do
    begin
      EnumWindows(@EnumFindDefView, 0);
      AppData.LogVerb('EnumWindows attempt '+ IntToStr(Attempt) +': WorkerW='+ IntToStr(WorkerW));
      if WorkerW <> 0 then Break;
      Sleep(RetryDelayMs);
    end;

  if WorkerW = 0 then
    begin
      AppData.LogError('Cannot find WorkerW via EnumWindows after ' + IntToStr(MaxRetries) + ' retries. This may happen under RDP, in VMs, or with custom shells.');
      EXIT(FALSE);
    end;

  { Re-fetch Progman to be safe }
  Progman:= FindWindowEx(GetDesktopWindow, 0, 'Progman', 'Program Manager');
  AppData.LogVerb('Final Progman hwnd: '+ IntToStr(Progman));

  { Hide the WorkerW we found }
  ShowWindow(WorkerW, SW_HIDE);

  { Put our window behind the desktop icons, as a child of Progman. }
  /// https://stackoverflow.com/questions/14440717/delphi-how-to-use-showwindow-properly-on-external-application
  ParentWindow:= Progman;
  AppData.LogVerb('ParentWindow set to: ' + IntToStr(ParentWindow));

  Top   := 0;
  Left  := 0;
  Result:= TRUE;
end;


procedure TDrawingForm.Repair;
begin
{ Attempts to restore the drawing window if it becomes invisible.
  This can happen after theme changes via the Personalize menu.
  Note: Currently not implemented
  Current workaround: Destroy this form and call GetHiddenWindow again. }
  //TODO 6: Investigate if re-parenting to Progman can restore visibility.
end;

 
end.
