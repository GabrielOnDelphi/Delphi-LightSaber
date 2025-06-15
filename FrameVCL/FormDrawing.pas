UNIT FormDrawing;

{=============================================================================================================
  PAINT wallpaper dynamically UNDER DESKTOP ICONS
  2021-05-05


  GetHiddenWindow()
  Returns: A handle to that special WorkerW window

  Note about true dynamic wallpaper painting (painting under icons):
    It works on XP only and Win7 with Aero disabled. Win 7 creates a lot of problems when Aero is enabled.
    For Win 8 and 10 see here: http://www.codeproject.com/Articles/856020/Draw-Behind-Desktop-Icons-in-Windows

  Window order:
    On Win 7 with Aero enabled, the order is: Desktop -> Progman -> SHELLDLL_DefView -> SysListView32 (List view) ->  SysHeader32 (Header).
    Icons are in SysListView32;


  Compatibility:
            WinXP   WinVista   Win7Aero   Win7NoAero   Windows8    Windows10
             ?          ?        over       under?      under?       under

  Original author reported to work also on Win8 (and up).

  Persistence:
     On Win7 the drawing is temporary (gets deleted as soon as the desktop is refreshed)
     On Win10 it is permanent (then drawing stays there even if I move then icons or refresh the desktop)
     However, there are some persistence issues on Win10:
               1. The wallpaper is lost at Windows shutdown
               2. The accent color (color color of all window) is not properly set because Windows does not see this wallpaper.

  Speed:
     It can draw about 150 frames per second, when bitmap is a 608x1200 pixels

  Note:
    The window is lost if the user activates the "Personalize" menu (right click on d4esktop) and changes the current window theme.
    However, fortunately, the window is restored when the next animated wallpaper is applied.
________________________________________________________________________________

  How it works:
    We create a special WorkerW using the undocumented parameter $052C. This is how MS does it in 'Personalize'
    We hide that window
    We insert our own window on top of it
    We draw on our own window.

  Structure of my desktop on Win7 SP1 64 bits:
   '', WorkerW
      '', SHELLDLL_DefView
          'FolderView' SysListView32  <-- this wnd contains the icons
             '', SysHeader32

  Source (combined):
     1 https://www.codeproject.com/Articles/856020/Draw-Behind-Desktop-Icons-in-Windows-plus?msg=5164145#xx5164145xx
     2 https://stackoverflow.com/questions/34952967/drawing-to-the-desktop-via-injection
--------------------------------------------------------------------------------------------------------------

  Tester:
     c:\MyProjects\Project Testers\gr cmDesktop.pas Tester\

  Related:
    https://github.com/payalord/ZMatrix/issues/1     (open source)
=======================================================================================================================}

INTERFACE

USES
   Winapi.Windows, System.Classes, System.SysUtils, System.Types, Vcl.Graphics, Vcl.Controls, Vcl.Forms, LightVcl.Common.AppDataForm,Vcl.ExtCtrls;

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

{VAR DrawingForm: TDrawingForm= NIL;  { The 'secret' form ($052C) on which we paint the animation }


function GetHiddenWindow(Color: TColor; ExpandOnAllMon, Hidden: Boolean): TDrawingForm;


IMPLEMENTATION {$R *.dfm}
USES
    LightVcl.Common.WinVersion, LightVcl.Common.WinVersionAPI, LightVcl.Common.ExeVersion, LightCore.AppData, LightVcl.Common.AppData
, LightVcl.Visual.INIFile, LightVcl.Graph.Desktop;



{ Note: Close it with FreeAndNil(DrawingForm), not with DrawingForm.Close }
function GetHiddenWindow(Color: TColor; ExpandOnAllMon, Hidden: Boolean): TDrawingForm;
begin
 Result:= TDrawingForm.Create(NIL);     { Create a new window, that we put behind the desktop icons, as a child of Progman. We draw the animation on this window }
 Result.Color:= Color;
 Result.ExpandOnAllMon:= ExpandOnAllMon;
 { Initialize our wnd }
 Result.initPaintingBkgWnd(Hidden);
end;





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
 Tag           := 128; {128 = DontTranslate in LightVcl.Common.Translate.pas }
 StyleElements := [];    { The form will be painted to the skin color if skins are enabled. My color will be ignored. Therefore we need to disable skins for this form! }

 {del
  This fixes bug:
   Start gif animation on small monitor. Put static wallpaper.
   Move Bionix on the big monitor. Start animation again.
   The DrawingWindow will not cover the entire monitor now. I think I need to use Align = alClient
   However, now, the DrawWindow will not be tied to the monitor on which BioniX is running.
   It will be always tied on the main monitor. Sad, because that was cool. }
 //Align:= alClient; if I set it to alClient, some of the screen portions will remain uncovered, when the two monitors have different res/size
end;



{ Clear THE ENTIRE desktop (all screens) }
procedure TDrawingForm.ClearBkg;
begin
 Canvas.Brush.Color:= Color;
 Canvas.FillRect(rect(0, 0, ClientWidth, ClientHeight));
end;


{ Tricks to make the video player work }
procedure TDrawingForm.HidePlayer;
begin
 //pnlVideoDisplay.Parent:= NIL;
 //pnlVideoDisplay.Visible:= FALSE;
 //pnlVideoDisplay.Refresh;
 //DrawingForm.pnlVideoDisplay.SetBounds(0, 0, 300, 300);
 pnlVideoDisplay.Left:= 20000;
end;


procedure TDrawingForm.ShowPlayer(ViewPortRect: TRect);
begin
 //pnlVideoDisplay.Parent:= Self;
 pnlVideoDisplay.Visible:= TRUE;
 //pnlVideoDisplay.Refresh;
 //Refresh;
 //DrawingForm.pnlVideoDisplay.SetBounds(0, 0, 300, 300);
 pnlVideoDisplay.Left:= ViewPortRect.Left;
end;






function TDrawingForm.initPaintingBkgWnd(Hidden: Boolean): Boolean;
var
   Desktop, Progman: HWND;
   DefView, Worker: HWND;
begin
 Result  := TRUE;
 DefView := 0;
 Worker  := 0;
 Desktop := GetDesktopWindow;
 Progman := ProgManHandle;
 AppData.LogVerb('Desktop hwnd: '+ IntToStr(Desktop));
 AppData.LogVerb('Progman hwnd: '+ IntToStr(Progman));

 if IsWindows7Up then  { We only need to do this on Win7 + }
  begin
    AppData.LogVerb('IsWindows7Up detected');       ///////////// LogVerb('sssssss: '+ IntToStr(sssssss));

    { Create special WorkerW using the undocumented parameter $052C }
    AppData.LogVerb('Sending msg to ProgMan');
    SendMessageTimeout(Progman, $052C, 0, 0, SMTO_NORMAL, 1000, NIL);

    { Kee anumerating until we find the SHELLDLL_DefView window }
    WHILE DefView= 0 DO
     begin
        Worker := FindWindowEx(Desktop, Worker, 'WorkerW', NIL);
        AppData.LogVerb('Worker: '+ IntToStr(Worker));
        if Worker = 0 then
         begin
          AppData.LogVerb('Loop failed. Cannot find WorkerW!');
          EXIT(FALSE);
         end;
        DefView := FindWindowEx(Worker, 0, 'SHELLDLL_DefView', NIL);
        AppData.LogVerb('DefView: '+ IntToStr(DefView));
     end;

    if Worker= 0 then  // First WorkerW failed
     begin
      AppData.LogVerb('Worker failed!');
      EXIT(FALSE);
     end;

    { Find the next WorkerW }
    Worker := FindWindowEx(Desktop, Worker, 'WorkerW', NIL);
    AppData.LogVerb('Worker2: '+ IntToStr(Worker));

    Result:= Worker > 0;
    if NOT Result then
     begin
      AppData.LogVerb('Worker2 failed!');
      EXIT(FALSE);
     end;

    AppData.LogVerb('GetDesktopWindow: '+ IntToStr(GetDesktopWindow));
    Progman:= FindWindowEx(GetDesktopWindow, 0, 'Progman', 'Program Manager');
    AppData.LogVerb('Progman hwnd: '+ IntToStr(Progman));

    // Hide this new that you just created
    ShowWindow(Worker, SW_HIDE);


    //Also see: csWindow.RestoreWindow
    //Also try to set Application.ShowMainForm to true.

    //////////// SEE THIS!
    /// https://stackoverflow.com/questions/14440717/delphi-how-to-use-showwindow-properly-on-external-application

    { Put our window behind the desktop icons, as a child of Progman. }
    ParentWindow:= Progman;
    AppData.LogVerb('ParentWindow: '+ IntToStr(ParentWindow));

    Top := 0;
    Left:= 0;
    if ExpandOnAllMon
    then
     begin
      ClientWidth  := Screen.DesktopWidth;  //4480
      ClientHeight := Screen.DesktopHeight;  //1440
     end
    else
     begin
      ClientWidth  := Screen.Width;
      ClientHeight := Screen.Height;
     end;

    { Done. Now the caller can make the form visible. DrawingForm.Visible:= TRUE; }
  end;
  AppData.LogVerb('Done!');
end;


{ Try to make the form again visible }
procedure TDrawingForm.Repair;
begin
 //ToDo: Try to make the form again visible
end;


(*
function DrawOnDesktop_orig(CONST X, Y: Integer; const BMP : TBitmap): Boolean;      // function returns true if succeseful
VAR
   Desktop : HWND;
   Canvas  : TCanvas;
   DC : HDC;
begin
 Result  := FALSE;                                                              // If the function fails the value is false
 Desktop := FindWindow('ProgMan', nil);
 Desktop := FindWindowEx(Desktop, 0, 'SHELLDLL_DefView', nil);
 Desktop := FindWindowEx(Desktop, 0, 'SysListView32', nil);                     // We found the listview that contains the desktop icons
 if (Desktop > 0) then                                                          // if change is possible then
 begin
   TRY
    DC := GetDC(Desktop);                                                       // the dc is freed after repainting
    Canvas := TCanvas.Create;                                                   // After the drawing the object is freed
    Canvas.Handle := DC;
    Canvas.Draw(X, Y, bmp);                                                     // Draw the bitmap on desktop
    Canvas.Free;
    Result := True;
    {
     Repaint desktop icons
     If I activate it it will cause A LOT of refresh
     RedrawWindow(Desktop, nil, 0, RDW_NOERASE or RDW_INVALIDATE or RDW_UPDATENOW);
     }
    //InvalidateRect( Desktop, NIL, TRUE ); UpdateWindow( Desktop );
    ReleaseDC(Desktop,DC);
   except_
    Result:= FALSE;
   END;
 end;
end; *)


end.




