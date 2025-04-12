UNIT cGraphDesktop;

{=============================================================================================================
   2023.01
   www.GabrielMoraru.com
   See Copyright file
==============================================================================================================

  Tester:
     c:\MyProjects\Project Testers\gr cDesktop.pas Tester\cDesktop_Tester.dpr

=============================================================================================================}

INTERFACE

USES
   Winapi.Windows, Vcl.Graphics, system.win.Registry, System.SysUtils, System.Types, Vcl.Forms;

TYPE
  MSWallStyle = (dsStretch, dsCenter, dsTile);

 procedure SysForceTileWallpaper;                                      { Force Windows wallpaper style to Tile if the user has more than one monitor. If the style is not 'Tile' then the wallpaper will not be centered properly on dual monitor systems }
 procedure SetDesktopWallpaperStyle(Style: MSWallStyle; Lazy: Boolean= true);
 procedure ShowDesktopIcons  (CONST Show: Boolean);

 procedure BlankDesktop(Logo: string= ''; bkgColor: TColor= clBlack);  { It will blank all monitors. The blank is permanent (cur wallpaper is 'lost') }

 procedure RefreshDesktopQuick;
 procedure RefreshDesktop;                                             { Forces Windows to fully redrawn the wallpaper. Useful to call after the user changes the wallpaper style. }

 function  GetShellWindow: HWND;
 function  ProgManHandle : HWND;
 function  GetDesktopHandle: HWND;

 { Paint over wallpaper }
 function  PaintOverIcons(X,Y : Integer; Bmp : TBitmap): Boolean;      { Paints over icons on Win7 and under icons in Win 8+ } // old name: DrawOnDesktop
 procedure WriteTextOnDesktopOver (x, y: integer; Text: string; FontName: string; Size: integer; Color: tcolor);   { Write on desktop over icons and windows }
 procedure WriteTextOnDesktopUnder(x, y: integer; Text: string);       { Write on wallpaper below icons and windows }
 procedure WriteTextOverAllDesktop(x, y: integer; Text: string; FontName: string; Size: integer; Color: TColor);

 { Set wallpaper IO }
 function  SetWallpaperBrodcast (FileName: string): Boolean;           { IO }
 function  SetWallpaper         (FileName: string): Boolean;           { IO }
 function  SetWallpaper0        (FileName: string): Boolean;           { IO }

 procedure SetSystemColor (PropertyToChange: Integer; Color: TColor);  { Sets a color (system wide), for example the color of the window Caption. }
 function  GetDesktopResolutionAPI: TRect;                             { CEL MAI SIMPLU E ASA: Screen.DesktopWidth, Screen.DesktopHeight}
 function  GetDPI: Integer;

 { }
 function  DrawOnWindow        (Handle: HWND; X, Y: Integer; BMP : TBitmap): Boolean;
 function  DrawOnWindowBitBlt  (Handle: HWND; X, Y: Integer; BMP : TBitmap): Boolean;             { It is not faster than the one above, without bitblt }



IMPLEMENTATION

USES
   cGraphBitmap, ccIO, ccCore, cbDialogs, cbVersion;




function GetDesktopResolutionAPI: TRect;                                                           {DOES NOT WORK ON DUAL MONITOR! IT WILL ONLY RETURN THE RES OF MAIN MONITOR }
VAR r: TRect;
begin
  SystemParametersInfo(SPI_GETWORKAREA, 0, @r, 0);
  Result:= r;                                                                                      {CEL MAI SIMPLU E ASA: Screen.DesktopWidth, Screen.DesktopHeight}
end;


function GetDPI: Integer;
begin
 Result:= Screen.PixelsPerInch;                                                                    { returneaza dpi-ul userului standard este 96 }
end;


{ Temporary alters Windows color scheme (caption ,desktop, etc)

  API details
    The SetSysColors function sends a WM_SYSCOLORCHANGE message to all windows to inform them of the change in color.
    It also directs Windows to repaint. It hanges the current Windows session only!!! The new colors are not saved when Windows terminates!!!

  PropertyToChange
    COLOR_3DDKSHADOW             Dark shadow for three-dimensional display elements.
    COLOR_3DFACE,
    COLOR_BTNFACE                Face color for three-dimensional display elements.
    COLOR_3DHILIGHT,
    COLOR_3DHIGHLIGHT,
    COLOR_BTNHILIGHT,
    COLOR_BTNHIGHLIGHT           Highlight color for three-dimensional display elements (for edges facing the light source.)
    COLOR_3DLIGHT                Light color for three-dimensional display elements (for edges facing the light source.)
    COLOR_3DSHADOW,              Shadow color
    COLOR_BTNSHADOW              Shadow color for three-dimensional display elements (for edges facing away from the light source).
    COLOR_ACTIVEBORDER           Active window border.
    COLOR_ACTIVECAPTION          Active window caption.
    COLOR_APPWORKSPACE           Background color of multiple document interface (MDI) applications.
    COLOR_BACKGROUND,
    COLOR_DESKTOP
    COLOR_BTNTEXT                Text on push buttons.
    COLOR_CAPTIONTEXT            Text in caption, size box, and scroll bar arrow box.
    COLOR_GRAYTEXT               Grayed (disabled) text. This color is set to 0 if the current display driver does not support a solid gray color.
    COLOR_HIGHLIGHT              Item(s) selected in a control.
    COLOR_HIGHLIGHTTEXT          Text of item(s) selected in a control.
    COLOR_INACTIVEBORDER         Inactive window border.
    COLOR_INACTIVECAPTION        Inactive window caption.
    COLOR_INACTIVECAPTIONTEXT    Color of text in an inactive caption.
    COLOR_INFOBK                 Background color for tooltip controls.
    COLOR_INFOTEXT               Text color for tooltip controls.
    COLOR_MENU                   Menu background.
    COLOR_MENUTEXT               Text in Vcl.Menus.
    COLOR_SCROLLBAR              Scroll bar gray area.
    COLOR_WINDOW                 Window background.
    COLOR_WINDOWFRAME            Window frame.
    COLOR_WINDOWTEXT             Text in windows.  }

{ FUCKING IMPORTANT! If I call this, I loose the DrawingForm in BioniX }
procedure SetSystemColor(PropertyToChange: Integer; Color: TColor);
begin
  Winapi.Windows.SetSysColors(1, PropertyToChange, Color);   { 1 arata cati parametri are de schimbat. in cazul nostru, doar unu: PropertyToChange }
end;





{ Microsoft documentation:
  http://technet.microsoft.com/en-us/library/cc978626.aspx

   TileWallpaper
    0 = Wallpaper is centered on the screen.
    1 = Wallpaper is tiled across the screen.
   ----------
   WallpaperStyle
    0 = Center the bitmap on the desktop.
    2 = Stretch the bitmap vertically and horizontally to fit the desktop.

 ---------------------------------------------------------------------------
   Style              WallpaperStyle    TileWallpaper        Win 7 only
 ---------------------------------------------------------------------------
   fill                   10                 0                  True
   fit                     6                 0                  True
   stretch                 2                 0                   -
   tile                    0                 1                   -
   center                  0                 0                   -
 ----------------------------------------------------------------------------

 Related: http://delphi-kb.blogspot.de/2011/01/change-desktop-wallpaper-through-code.html
 -------------------------------------------------------------------------------------------------- }
procedure SetDesktopWallpaperStyle(Style: MSWallStyle; Lazy: Boolean= true);      { Set how the image will be displayed on the desktop }
VAR Rg: TRegistry;
begin
 Rg:= TRegistry.Create;
 TRY
  Rg.RootKey := HKEY_CURRENT_USER;
  Rg.LazyWrite:= Lazy;
  if Rg.OpenKey('Control Panel\Desktop', TRUE) then
    case Style of

     dSTile:       { Tile }
      begin
       Rg.WriteString('TileWallpaper' , '1');
       Rg.WriteString('WallpaperStyle', '0');
      end;

     dsStretch:                                                                                    { Stretch }
      begin
       Rg.WriteString('TileWallpaper' , '0');
       Rg.WriteString('WallpaperStyle', '2');
      end ;

     dsCenter:     { Center }
      begin
       Rg.WriteString('TileWallpaper' , '0');
       Rg.WriteString('WallpaperStyle', '0');
      end;
    end;
 FINALLY
  Rg.CloseKey;
  FreeAndNil(Rg);
 end;
end;


procedure SysForceTileWallpaper;     { Force Windows wallpaper style to Tile if the user has more than one monitor. If the style is not 'Tile' then the wallpaper will not be centered properly on dual monitor systems }
begin
 if Screen.MonitorCount > 1
 then SetDesktopWallpaperStyle(dSTile, TRUE);
end;





{ Write on desktop over icons and all open windows.
  However, on Win7 the text is "corrupted" }
procedure WriteTextOverAllDesktop(x, y: integer; Text: string; FontName: string; Size: integer; Color: TColor);
VAR
  MyHand: HWND;
  MyDc  : HDC;
  MyCanvas: TCanvas;
begin
  MyHand  := GetDesktopWindow;
  MyDc    := GetWindowDC(MyHand);
  MyCanvas:= TCanvas.Create;
  TRY
   MyCanvas.Handle := MyDC;
   BeginPath(MyCanvas.Handle);
   MyCanvas.Font.Color := Color;
   MyCanvas.Font.Name  := FontName;
   MyCanvas.Font.Size  := Size;
   MyCanvas.Brush.Color:= clBlack; // <--------------- DOES NOT WORK! (Ignored)
   MyCanvas.Brush.Style:= bsSolid;
   SetBkMode(MyCanvas.Handle, TRANSPARENT);
   EndPath(MyCanvas.Handle);
   MyCanvas.TextOut(x, y, Text);
  FINALLY
   FreeAndNil(MyCanvas);
  END;
end;


{ Win  7: Writes text over desktop icons
  Win 10: Writes text over desktop icons

  2021.03 - Does not work anymore! Maybe because BX changed the desktop Wnd order. YES. Confirmed! }
procedure WriteTextOnDesktopOver(x, y: integer; Text: string; FontName: string; Size: integer; Color: TColor);
VAR
  MyCanvas: TCanvas;
begin
  MyCanvas:= TCanvas.Create;
  TRY
   MyCanvas.Handle := GetWindowDC(GetDesktopHandle);
   BeginPath(MyCanvas.Handle);
   MyCanvas.Font.Color := Color;
   MyCanvas.Font.Name  := FontName;
   MyCanvas.Font.Size  := Size;
   SetBkMode(MyCanvas.Handle, TRANSPARENT);
   EndPath(MyCanvas.Handle);
   MyCanvas.TextOut(x, y, Text);
  FINALLY
   FreeAndNil(MyCanvas);
  END;
end;


{ Win  7: Writes text under desktop icons
  Win 10: Writes text under desktop icons.
  This will paint on the actual desktop window, and not over open windows, so the text could be hidden by any possible open window.
  The text is delete if I move an icon over it.

  This works even if BX changed the desktop window order! }
procedure WriteTextOnDesktopUnder(x, y: integer; Text: string);    //old name: WriteTextOnWallpaper
VAR Handle: HWND;
    DC: HDC;
    OutString: PChar;
begin
   Handle:= GetDesktopHandle;
   DC := GetWindowDC(Handle);
   OutString := PChar(Text);
   TextOut(DC, x, y, OutString, Length(OutString));
end;









{--------------------------------------------------------------------------------------------------
   HANDLES
---------------------------------------------------------------------------------------------------}
TYPE
  TMyData = record
    Handle: HWND;
    Pid: DWORD;
    Caption: String;
    ClassName: String;
  end;
  PMyData = ^TMyData;


{ Returns a handle to the SysListView32 that contains the icons
  Works with:
    Win 7 (Aero enabled/disabled)
    Reported to work also on Win XP, Win 8, 10
  Source: http://stackoverflow.com/questions/3482227/old-delphi-hide-show-desktop-icons-method-not-working-under-windows-7-64-bit }
function getDesktopHandleWin7 (Handle: HWND; MyData: PMyData): bool; stdcall;    { Callback function for GetDesktopHandle. It cannot be a local function. Details: http://stackoverflow.com/questions/36717308/how-to-use-the-enumwindows-call-back-function }
VAR hChild : HWND;
begin
 Result:= Handle <> 0;
 if NOT Result then Exit;

 hChild := FindWindowEx(Handle, 0, 'SHELLDLL_DefView', nil);
 if hChild <> 0 then
  begin
   hChild := FindWindowEx(hChild, 0, 'SysListView32', nil);    { There is a suggestion to use FolderView instead of NIL: https://stackoverflow.com/questions/8364758/get-handle-to-desktop-shell-window }
   if hChild <> 0
   then MyData.Handle := hChild;   {TODO 4: _getDesktopWin7. Return the DeskHandle as specified here: http://stackoverflow.com/questions/36717308/how-to-use-the-enumwindows-call-back-function }
  end;
end;


function GetDesktopHandleWin7_: THandle;  { Alternative }
VAR S: String;
begin
  Result := FindWindow('ProgMan', 'Program Manager' {or NIL});
  Result := GetWindow(Result, GW_CHILD);
  Result := GetWindow(Result, GW_CHILD);
  SetLength(S, 40);
  GetClassName(Result, PChar(S), 39);
  if PChar(S) <> 'SysListView32'
  then Result := 0;
end;


{ Source
  https://stackoverflow.com/questions/34952967/drawing-to-the-desktop-via-injection }
function GetDesktopHandleWin7__: HWND;
var
   worker, defView, desktop: HWND;
begin
 Result   := 0;
 defView  := 0;
 worker   := 0;
 desktop  := GetDesktopWindow();

 while defView= 0 do
  begin
     worker := FindWindowEx(desktop, worker, 'WorkerW', nil);
     if worker = 0 then exit(0);

     defView := FindWindowEx(worker, 0, 'SHELLDLL_DefView', nil);

     if defView > 0
     then Result := FindWindowEx(defView, 0, 'SysListView32', nil);   { There is a suggestion to use FolderView instead of NIL: https://stackoverflow.com/questions/8364758/get-handle-to-desktop-shell-window }
  end;
end;



{ Since there is more than one window with title "" and class 'WorkerW', we have
     to go through the window tree sequentially. This can be done using the EnumWindows function.
     EnumWindows used the _getDesktopWinX callback function for every top level window.
     From there, we can check if the current window contains a child named 'SHELLDLL_DefView',
     which indicates that the current window represents the desktop icons.
     We then take the next sibling of that window.
 Callback function for GetDesktopHandle. It cannot be a local function.
 Details: http://stackoverflow.com/questions/36717308/how-to-use-the-enumwindows-call-back-function

 Another source (REALLY NICE):
     https://www.codeproject.com/Articles/856020/Draw-behind-Desktop-Icons-in-Windows
     https://stackoverflow.com/questions/8364758/get-handle-to-desktop-shell-window
}

function getDesktopHandleWin8 (Handle: HWND; MyData: PMyData): bool; stdcall;
VAR hChild : HWND;
begin
 Result:= Handle <> 0;
 if NOT Result then Exit;

 hChild := FindWindowEx(Handle, 0, 'SHELLDLL_DefView', nil);
 if hChild <> 0 then
  begin
   hChild := FindWindowEx(0, Handle, 'WorkerW', nil);
   if hChild <> 0 then
    begin
     MyData.Handle := hChild;      { http://stackoverflow.com/questions/36717308/how-to-use-the-enumwindows-call-back-function/36722139#36722139 }
     //todo 4: _getDesktopWin8 MyData.Caption  := Caption;
     //todo 4: _getDesktopWin8 MyData.ClassName:= ClassName;
    end  { return the DeskHandle as specified here: http://stackoverflow.com/questions/36717308/how-to-use-the-enumwindows-call-back-function }
  end;
end;


{ Detects if it is Win7 or Win8+ }
function GetDesktopHandle: HWND;
VAR
   MyData: TMyData;
begin
 if IsWindowsXP
 then Result:= FindWindowEx(FindWindow('Progman', NIL), 0,'ShellDll_DefView', NIL)   { Works up to Win XP }
 else
  begin
   ZeroMemory(@MyData, SizeOf(MyData));
   if IsWindows8Up
   then EnumWindows(@GetDesktopHandleWin8, NativeInt(@MyData))         { Win 8 }
   else EnumWindows(@GetDesktopHandleWin7, NativeInt(@MyData));        { Win 7 }
   Result:= MyData.Handle;
  end;
end;









{--------------------------------------------------------------------------------------------------
   PROGMAN
---------------------------------------------------------------------------------------------------}

{ Returns a handle to the Windows Shell window (Progman) }                                                    { Old name: GetProgManHandle }
function GetShellWindow: HWND;
TYPE
  TGetShellWindow = function(): HWND; stdcall;
VAR
  hUser32: THandle;
  GetShellWnd: TGetShellWindow;
begin
  Result := 0;
  hUser32 := GetModuleHandle('user32.dll');
  if hUser32 > 0 then
   begin
    @GetShellWnd := GetProcAddress(hUser32, 'GetShellWindow');
    if Assigned(GetShellWnd)
    then Result := GetShellWnd;
   end;
end;


{ Returns a handle to the Windows Shell window (Progman) }                                                    { Old name: GetProgManHandle_ }
function ProgManHandle: HWND;
begin
  Result:= FindWindowEx(GetDesktopWindow, 0, 'Progman', 'Program Manager');
end;















{--------------------------------------------------------------------------------------------------
  DrawOnDesktop IO

  + Paints under icons even on Win7 (Aero enabled)
  - It causes intensive IO activity (each time the function is called, the file is written to disk

---------------------------------------------------------------------------------------------------
 API SystemParametersInfo
                     0      = Can be zero if you do not want to update the user profile or broadcast the WM_SETTINGCHANGE message, or it can be one or more of the following values.
    SPIF_UPDATEINIFILE      = Writes the new system-wide parameter setting to the user profile.
    SPIF_SENDCHANGE         = (same as SPIF_SENDWININICHANGE) Broadcasts the WM_SETTINGCHANGE message after updating the user profile.

  My tests show this:
    0                  = Image set without refresh msg sent to all windows. Other applications won't see the wallpaper set by BioniX.
    SPIF_UPDATEINIFILE = Image set without refresh msg sent to all windows. Other applications will  see the wallpaper set by BioniX.
    SPIF_SENDCHANGE    = Image set without refresh msg sent to all windows. Other applications will  see the wallpaper set by BioniX.
---------------------------------------------------------------------------------------------------}

{ The effect is permanent. }
function SetWallpaper(FileName: string): Boolean;    //SetWallpaper
begin
 Result:= SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, PChar(FileName), SPIF_UPDATEINIFILE {1});   {if not result then RaiseLastOSError }
end;


{ The effect lasts until Win restart }
function SetWallpaperBrodcast(FileName: string): Boolean;
begin
 Result:= SystemParametersInfo (SPI_SETDESKWALLPAPER, 0, PChar(FileName), SPIF_SENDCHANGE {2});
end;     {I got this error here during GIF: Debugger Fault Notification. Project raised too many consecutive exceptions: 'system exception (code 0xc0000002) at 0x76dbc54f'. Process Stopped. Use Step or Run to continue. }


{ The effect lasts until Win restart. This MIGHT be the fastest way to dynamically paint animation on desktop since it doesn't send any messages }
function SetWallpaper0(FileName: string): Boolean;
begin
 Result:= SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, PChar(FileName), 0);   { Don't use this! Other applications won't see the wallpaper set by BioniX }
end;




{ Old drawing method.
  Win  7: Paints over icons
  Win  8: Paints under icons
  Win 10: Does nothing

  Looks nasty when the user move cursor over icons as only that area of the desktop will get refresh.
  This is a temporary painting. Lasts until user (or other apps) presses F5 to refresh desktop

  It is odd that when DreamScene was installed, this function blended the specified color (clBlack in
  this case) with the original JPG wallpaper set by BioniX instead of blanking the desktop.
  The image 'normally' visible before calling this function was the last frame from DreamScene WMV movie. Where I paused.
  So, it looks like it is dependent on the existence of the special hidden window ($052C) }
function PaintOverIcons(X, Y: Integer; BMP : TBitmap): Boolean;
VAR
   Progman, Handle : HWND;
begin
 Result:= FALSE;

 if IsWindows8Up then
  begin
   Progman:= FindWindow('Progman', NIL);     // Obtain Program Manager Handle
   if Progman <> 0
   then SendMessageTimeout(Progman, $052C, 0, 0, SMTO_NORMAL, 1000, {Res}NIL)      //Send  0x052C Message to Program Manager (Progman). This message directs Progman to spawn a WorkerW behind the desktop icons. If it is already there, nothing happens.
   else MesajError('Cannot initiate Progman window!');
  end;

 Handle:= GetDesktopHandle;
 if Handle > 0
 then Result:= DrawOnWindow(Handle, x, y, BMP);  // <--------------------- I could draw directly on Canvas!
end;









function DrawOnWindow(Handle: HWND; X, Y: Integer; BMP : TBitmap): Boolean;
VAR
   DC: HDC;
   Canvas: TCanvas;
begin
 Result:= TRUE;
 Assert(Handle > 0);
 TRY
  DC := GetDC(Handle);
  Canvas:= TCanvas.Create;                                                  { This is probably slow }
  Canvas.Handle := DC;
  Canvas.Draw(x, y, bmp);                                                   { Draw the bitmap }
  Canvas.Free;
  ReleaseDC(Handle, DC);
 EXCEPT
  //todo 1: trap only specific exceptions
  Result:= FALSE;
 END;
end;



{ It is not faster than the one above, without bitblt }
function DrawOnWindowBitBlt(Handle: HWND; X, Y: Integer; BMP : TBitmap): Boolean;  { It is not faster than the one above, without bitblt }  {    BitBlt documentation: https://msdn.microsoft.com/en-us/library/windows/desktop/dd183370(v=vs.85).aspx  }
VAR
   DC: HDC;
begin
 Result:= TRUE;
 Assert(Handle > 0);
 TRY
   DC := GetDC(Handle);                                                      { the dc is freed after repainting }
   BitBlt(DC, x, y, BMP.Width, BMP.Height, BMP.Canvas.Handle, 0, 0, SRCCOPY);
   ReleaseDC(Handle, DC);
 except
  //todo 1: trap only specific exceptions
  Result:= FALSE;
 END;
end;


















{-------------------------------------------------------------------------------
   REFRESH
-------------------------------------------------------------------------------}

{ Forces Windows to fully redrawn the wallpaper. Useful to call after the user changes the wallpaper style. }
procedure RefreshDesktop;
begin
 SystemParametersInfo (SPI_SETDESKWALLPAPER, 0, NIL, SPIF_SENDCHANGE);           { SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, PChar(sFileName), SPIF_SENDWININICHANGE);}
end;


{ It will only refresh the window. It will not restore the wallpaper. }
procedure RefreshDesktopQuick;    //old name:  RefreshDesktopWnd
VAR Desktop : HWND;
begin
 Desktop:= GetDesktopHandle;
 if Desktop > 0
 then RedrawWindow(Desktop, NIL, 0, RDW_NOERASE or RDW_INVALIDATE or RDW_UPDATENOW);
end;






{-------------------------------------------------------------------------------
   BLANK
-------------------------------------------------------------------------------}



{ This function also exists in cGraphBitmap.pas but we don't want to depend on that file }
function CreateBlankBitmap(CONST Width, Height: Integer; BkgClr: TColor= clBlack): TBitmap;
begin
 Result:= TBitmap.Create;
 TRY
  Result.PixelFormat:= pf24bit;
  Result.SetSize(Width, Height);
  Result.Canvas.Brush.Color:= BkgClr;
  Result.Canvas.Brush.Style:= bsSolid;
  Result.Canvas.FillRect(Result.Canvas.ClipRect);
  {if Text > '' then
   begin
    Result.Canvas.Brush.Color:= BkgClr;
    Result.Canvas.Font.Name  := 'Verdana';
    Result.Canvas.Font.Size  := 8;
    Result.Canvas.Font.Color := clLime;
    Result.Canvas.TextOut((Result.Width- Result.Canvas.TextWidth(Text)) DIV 2, (Result.Height- Result.Canvas.TextHeight(Text)) DIV 2, Text);
   end; }
 EXCEPT
  FreeAndNil(Result);
  RAISE;
 end;
end;



{ It will blank all monitors.
  The blank is permanent (cur wallpaper is 'lost') }
procedure BlankDesktop(Logo: string= ''; bkgColor: TColor= clBlack);        //old name: BlankMonitors
VAR
   SaveAs: string;
   BMP: TBitmap;
CONST
  PleaseReportIssue= 'Please report the steps to reproduce this error.';
begin
 SaveAs:= GetTempFolder+ 'BlankDesktop.BMP';
 BMP:= CreateBlankBitmap(Screen.DesktopWidth, Screen.DesktopHeight, bkgColor);
 TRY
    cGraphBitmap.CenterText(BMP, Logo {bkgColor});
    TRY
     BMP.SaveToFile(SaveAs);                { LOTS of errors in v9.113 on this line.          Cannot create file "C:\Users\UserName\AppData\Local\Temp\BlankDesktop.BMP". The process cannot access the file because it is being used by another process.        Possible causes:       * File locked by antivirus       * File locked by SystemParametersInfo API (called in DrawOnDesktop) }
    EXCEPT
      //todo 1: trap only specific exceptions
      on E: Exception DO MesajError(E.Message+ ' (in BlankDesktop)'+ CRLFw+ PleaseReportIssue);
    END;
    SetWallpaper(SaveAs);
 FINALLY
   FreeAndNil(BMP);
 END;
end;




{--------------------------------------------------------------------------------------------------
   SHOW/HIDE DESKTOP ICONS
   Win7: works ok
   Win10: does nothing
--------------------------------------------------------------------------------------------------}
procedure ShowDesktopIcons(CONST Show: Boolean);
begin
{
 if IsWindowsXP
 then Handle:= FindWindowEx(FindWindow('Progman', NIL), 0,'ShellDll_DefView', NIL)   { Works up to Win XP
 else Handle:= GetDesktopHandle;  }
 if Show
 then ShowWindow(GetDesktopHandle, SW_SHOW)
 else ShowWindow(GetDesktopHandle, SW_HIDE);
end;



end.
