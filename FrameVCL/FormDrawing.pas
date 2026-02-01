UNIT FormDrawing;

{=============================================================================================================
   PAINT wallpaper dynamically UNDER DESKTOP ICONS
   
   2026.01.29
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   PAINT WALLPAPER DYNAMICALLY UNDER DESKTOP ICONS

   Creates a special hidden window that sits behind desktop icons, allowing
   dynamic wallpaper painting (animated wallpapers, video backgrounds, etc).

   USAGE:
     var DrawWnd: TDrawingForm;
     DrawWnd:= GetHiddenWindow(clBlack, True, False);
     // Draw on DrawWnd.Canvas or use DrawWnd.pnlVideoDisplay for video
     FreeAndNil(DrawWnd);  // Always use FreeAndNil, not Close

   HOW IT WORKS:
     1. Creates a special WorkerW using undocumented Windows message $052C
     2. Hides that WorkerW window
     3. Inserts our form as a child of Progman (behind desktop icons)
     4. Draws on our form's canvas

   COMPATIBILITY:
     Platform      | Behavior
     --------------|----------
     Windows XP    | Unknown
     Windows Vista | Unknown
     Windows 7     | Aero=over icons, No Aero=under icons
     Windows 8+    | Under icons (permanent until shutdown)
     Windows 10/11 | Under icons (permanent until shutdown)

   PERSISTENCE:
     - Win7: Drawing is temporary (lost on desktop refresh)
     - Win10+: Drawing persists (survives icon moves, desktop refresh)
     - All: Lost on Windows shutdown or theme change via Personalize menu

   PERFORMANCE:
     ~150 FPS with 608x1200 pixel bitmaps

   WINDOW HIERARCHY (Win7 SP1 x64):
     WorkerW
       SHELLDLL_DefView
         SysListView32 (FolderView) <- contains icons
           SysHeader32

   SOURCES:
     - https://www.codeproject.com/Articles/856020/Draw-Behind-Desktop-Icons-in-Windows
     - https://stackoverflow.com/questions/34952967/drawing-to-the-desktop-via-injection

   TESTER:
     c:\Projects\Project Testers\gr cmDesktop.pas Tester\

   RELATED:
     https://github.com/payalord/ZMatrix/issues/1
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
  TDrawingForm = class(TLightForm)
    pnlVideoDisplay: TPanel;
  private
    function initPaintingBkgWnd(Hidden: Boolean): Boolean;
  public
    ExpandOnAllMon: Boolean;
    procedure HidePlayer;
    procedure ShowPlayer(ViewPortRect: TRect);
    procedure ClearBkg;
    procedure Repair;
    constructor Create(aOwner: TComponent); override;
  end;


{ Creates and returns a hidden window for painting behind desktop icons.
  Parameters:
    Color          - Background color of the drawing surface
    ExpandOnAllMon - True to span all monitors, False for primary only
    Hidden         - Reserved for future use
  Returns:
    The created TDrawingForm instance (caller must free with FreeAndNil)
  Note: Use FreeAndNil to close, not Form.Close }
function GetHiddenWindow(Color: TColor; ExpandOnAllMon, Hidden: Boolean): TDrawingForm;


IMPLEMENTATION {$R *.dfm}

USES
   LightVcl.Common.WinVersion, LightVcl.Common.WinVersionAPI, LightVcl.Common.ExeVersion,
   LightCore.AppData, LightVcl.Visual.AppData, LightVcl.Visual.INIFile, LightVcl.Graph.Desktop;


function GetHiddenWindow(Color: TColor; ExpandOnAllMon, Hidden: Boolean): TDrawingForm;
begin
  Result:= TDrawingForm.Create(NIL);
  Result.Color:= Color;
  Result.ExpandOnAllMon:= ExpandOnAllMon;
  Result.initPaintingBkgWnd(Hidden);
end;





{ Creates the drawing form with settings optimized for desktop painting.
  Note: The form is created visible by default but will be positioned behind icons. }
constructor TDrawingForm.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Visible:= TRUE;
  ClearBkg;
  BorderIcons:= [];
  BorderStyle:= bsNone;
  DoubleBuffered:= TRUE;
  Caption:= 'frmBxDraw';
  Name:= 'frmBxDraw';
  Tag:= TAG_DONT_TRANSLATE;  // Skip translation in LightVcl.Translate.pas
  StyleElements:= [];        // Disable VCL skins - we need exact color control

  // Note: Don't use Align:=alClient - causes issues with multi-monitor setups
  // where monitors have different resolutions
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
  Parameters:
    ViewPortRect - Rectangle defining the display area (uses Left position) }
procedure TDrawingForm.ShowPlayer(ViewPortRect: TRect);
begin
  pnlVideoDisplay.Visible:= TRUE;
  pnlVideoDisplay.Left:= ViewPortRect.Left;
end;






{ Initializes the background painting window by finding/creating the special
  WorkerW window and positioning our form behind the desktop icons.

  Uses undocumented Windows message $052C to create the WorkerW window
  (same technique used by Windows "Personalize" feature).

  Parameters:
    Hidden - Reserved for future use
  Returns:
    True if initialization succeeded, False otherwise }
function TDrawingForm.initPaintingBkgWnd(Hidden: Boolean): Boolean;
const
  WM_SPAWN_WORKER = $052C;  // Undocumented message to create WorkerW
var
  Desktop, Progman: HWND;
  DefView, Worker: HWND;
  Iterations: Integer;
begin
  DefView:= 0;
  Worker:= 0;
  Iterations:= 0;

  Desktop:= GetDesktopWindow;
  Progman:= ProgManHandle;
  AppData.LogVerb('Desktop hwnd: ' + IntToStr(Desktop));
  AppData.LogVerb('Progman hwnd: ' + IntToStr(Progman));

  // This technique only works on Windows 7 and later
  if NOT IsWindows7Up then
    begin
      AppData.LogVerb('Windows version too old - requires Windows 7+');
      EXIT(FALSE);
    end;

  AppData.LogVerb('Windows 7+ detected');

  // Create special WorkerW using undocumented Windows message
  AppData.LogVerb('Sending WM_SPAWN_WORKER to ProgMan');
  SendMessageTimeout(Progman, WM_SPAWN_WORKER, 0, 0, SMTO_NORMAL, 1000, NIL);

  // Keep enumerating until we find the SHELLDLL_DefView window
  while (DefView = 0) and (Iterations < MAX_WORKERW_SEARCH) do
    begin
      Inc(Iterations);
      Worker:= FindWindowEx(Desktop, Worker, 'WorkerW', NIL);
      AppData.LogVerb('Worker iteration ' + IntToStr(Iterations) + ': ' + IntToStr(Worker));

      if Worker = 0 then
        begin
          AppData.LogVerb('Cannot find WorkerW window');
          EXIT(FALSE);
        end;

      DefView:= FindWindowEx(Worker, 0, 'SHELLDLL_DefView', NIL);
      AppData.LogVerb('DefView: ' + IntToStr(DefView));
    end;

  if Iterations >= MAX_WORKERW_SEARCH then
    begin
      AppData.LogWarn('WorkerW search exceeded maximum iterations');
      EXIT(FALSE);
    end;

  // Find the NEXT WorkerW (the one we'll use for painting)
  Worker:= FindWindowEx(Desktop, Worker, 'WorkerW', NIL);
  AppData.LogVerb('Target Worker: ' + IntToStr(Worker));

  if Worker = 0 then
    begin
      AppData.LogVerb('Cannot find target WorkerW');
      EXIT(FALSE);
    end;

  // Re-find Progman to ensure we have the correct handle
  Progman:= FindWindowEx(GetDesktopWindow, 0, 'Progman', 'Program Manager');
  AppData.LogVerb('Final Progman hwnd: ' + IntToStr(Progman));

  // Hide the WorkerW we found
  ShowWindow(Worker, SW_HIDE);

  // Put our form behind the desktop icons as a child of Progman
  // See: https://stackoverflow.com/questions/14440717/delphi-how-to-use-showwindow-properly-on-external-application
  ParentWindow:= Progman;
  AppData.LogVerb('ParentWindow set to: ' + IntToStr(ParentWindow));

  // Position and size the form
  Top:= 0;
  Left:= 0;

  if ExpandOnAllMon 
  then
    begin
      ClientWidth:= Screen.DesktopWidth;
      ClientHeight:= Screen.DesktopHeight;
    end
  else
    begin
      ClientWidth:= Screen.Width;
      ClientHeight:= Screen.Height;
    end;

  AppData.LogVerb('Initialization complete. Size: ' + IntToStr(ClientWidth) + 'x' + IntToStr(ClientHeight));
  Result:= TRUE;
end;


{ Attempts to restore the drawing window if it becomes invisible.
  This can happen after theme changes via the Personalize menu.
  Note: Currently not implemented - call GetHiddenWindow again instead. }
procedure TDrawingForm.Repair;
begin
  // The drawing window is lost when user changes themes via Personalize menu.
  // Current workaround: Destroy this form and call GetHiddenWindow again.
  // TODO: Investigate if re-parenting to Progman can restore visibility.
end;


end.




