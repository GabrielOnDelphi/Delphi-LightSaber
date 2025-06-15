UNIT LightVcl.Common.WindowMetrics;

{=============================================================================================================
   SYSTEM - Window Metrics
   2023.01
   www.GabrielMoraru.com
   See Copyright file
==============================================================================================================

   Get Window metrics
   Set Scrollbar metrics

   In this group:
     * LightVcl.Common.Shell.pas
     * csSystem.pas
     * csWindow.pas
     * LightVcl.Common.WindowMetrics.pas
     * LightVcl.Common.ExecuteProc.pas
     * LightVcl.Common.ExecuteShell.pas

=============================================================================================================}

INTERFACE

USES
   Winapi.Windows, Vcl.Controls, Vcl.StdCtrls;

{--------------------------------------------------------------------------------------------------
   WINDOW METRICS
--------------------------------------------------------------------------------------------------}
 function  GetCaptionHeight     (Handle: HWnd): Integer;
 function  GetMainMenuHeight    (Handle: HWnd): Integer; { Height, in pixels, of a single-line menu bar }
 function  GetFrameSize         (Handle: HWnd): Integer;

 function  GetWinBorderWidth    (Handle: HWnd): Integer; { Width  (pixels) of a window border }
 function  GetWinBorderHeight   (Handle: HWnd): Integer; { Height (pixels) of a window border }
 function  GetWin3DBorderWidth  (Handle: HWnd): Integer; { Width  (pixels) of a window (with 3D look) border }
 function  GetWin3DBorderHeight (Handle: HWnd): Integer; { Height (pixels) of a window (with 3D look) border }


 {--------------------------------------------------------------------------------------------------
   SCROLL BAR METRICS
--------------------------------------------------------------------------------------------------}
 function  HorizScrollBarVisible (WindowHandle: THandle): Boolean;
 function  VertScrollBarVisible  (WindowHandle: THandle): Boolean;

{--------------------------------------------------------------------------------------------------
   SCROLL BAR
--------------------------------------------------------------------------------------------------}
 function  GetScrollbarSize: Integer; deprecated 'Use GetScrollBarWidth instead.';        // GetWinScrollBarWidth
 procedure SetScrollbarWidth(Width: Integer);
 function  GetScrollBarWidth (Handle: HWnd): Integer;
 function  GetNumScrollLines: Integer;

 { DOESN'T WORK }
 procedure SetProportionalThumbV (ScrollBar: TScrollBar; OwnerClientHeight: Integer);
 procedure SetProportionalThumbH (ScrollBar: TScrollBar; OwnerClientWidth : Integer);

 

IMPLEMENTATION




function GetCaptionHeight(Handle: HWnd): Integer;
begin
 Result:= Vcl.Controls.GetSystemMetricsForWindow(SM_CYCAPTION, Handle);
end;


function GetFrameSize(Handle: HWnd): Integer;
begin
 Result:= GetSystemMetricsForWindow(SM_CYSIZEFRAME, Handle);
end;


{ Height, in pixels, of a single-line menu bar }
function GetMainMenuHeight(Handle: HWnd): Integer;
begin
 Result:= GetSystemMetricsForWindow(SM_CYMENU, Handle);
end;


{ Width (pixels) of a window border }
function GetWinBorderWidth(Handle: HWnd): Integer;
begin
 Result:= GetSystemMetricsForWindow(SM_CXBORDER, Handle);
end;


{ Height (pixels) of a window border }
function GetWinBorderHeight(Handle: HWnd): Integer;
begin
 Result:= GetSystemMetricsForWindow(SM_CYBORDER, Handle);
end;


{ Width (pixels) of a window (with 3D look) border }
function GetWin3DBorderWidth(Handle: HWnd): Integer;
begin
 Result:= GetSystemMetricsForWindow(SM_CXEDGE, Handle);
end;


{ Height (pixels) of a window (with 3D look) border }
function GetWin3DBorderHeight(Handle: HWnd): Integer;
begin
 Result:= GetSystemMetricsForWindow(SM_CYEDGE, Handle);
end;






{--------------------------------------------------------------------------------------------------
   VCL SCROLLBAR
--------------------------------------------------------------------------------------------------}
function GetScrollbarSize: integer;     { DEPRECATED }
VAR NCMet: TNonClientMetrics;
begin
 FillChar(NCMet, SizeOf(NCMet), 0);
 NCMet.cbSize:= SizeOf(NCMet);
 SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NCMet), @NCMet, 0);
 Result:= NCMet.iScrollWidth;
end;


{ Apply to the whole system }
procedure SetScrollbarWidth(Width: Integer);
VAR NCMet: TNonClientMetrics;
begin
 FillChar(NCMet, SizeOf(NCMet), 0);
 NCMet.cbSize:= SizeOf(NCMet);
 NCMet.iScrollWidth:= Width;
 SystemParametersInfo(SPI_SETNONCLIENTMETRICS, SizeOf(NCMet), @NCMet, SPIF_SENDCHANGE);   // set the new metrics
end;


function GetScrollBarWidth(Handle: HWnd): Integer;
begin
 Result:= GetSystemMetricsForWindow(SM_CXVSCROLL, Handle);
end;



{ Show how many lines a page scrolls when I hit WHEEL SCROLL }
function GetNumScrollLines: Integer;
begin
 SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @Result, 0);
end;


{ Detect if the scrollbars of a windowed control are visible. It can be used on anything that is a windows (TPanel, TGrid, TForm) }
function HorizScrollBarVisible (WindowHandle: THandle): Boolean;
begin
 Result:= (GetWindowLongPtr(WindowHandle, GWL_STYLE) AND WS_HSCROLL) <> 0;                         { getWindowLong_ was replaced with getWindowLongPtr for 64 bit compatibility. Details: http://docwiki.embarcadero.com/RADStudio/Seattle/en/Converting_32-bit_Delphi_Applications_to_64-bit_Windows }
end;


{ If I use it with a form that is MDI parent I have to use it like this:  "VertScrollBarVisible(MainForm.ClientHandle)"  }
function VertScrollBarVisible(WindowHandle: THandle): Boolean;
begin
 Result:= (GetWindowlongPtr(WindowHandle, GWL_STYLE) AND WS_VSCROLL) <> 0;
end;


{ DOESN'T WORK }
{ Sets the thumb tab of a vertical scroll bar so that it represents the proportion of the scrolling range that is visible. }
procedure SetProportionalThumbV (ScrollBar: TScrollBar; OwnerClientHeight: Integer);
VAR
  TrackHeight: Integer;                                                                            { The size of the scroll bar track }
  MinHeight: Integer;                                                                              { The default size of the thumb tab }
begin
  MinHeight:= GetSystemMetrics(SM_CYVTHUMB);                                                       { Save the default size. }
  TrackHeight := OwnerClientHeight - 2 * GetSystemMetrics(SM_CYVSCROLL);
  ScrollBar.PageSize := TrackHeight DIV (ScrollBar.Max - ScrollBar.Min + 1);
  if ScrollBar.PageSize < MinHeight
  then ScrollBar.PageSize := MinHeight;
end;


 { DOESN'T WORK } 
procedure SetProportionalThumbH (ScrollBar: TScrollBar; OwnerClientWidth: Integer);
VAR
  MinWidth: Integer;                                                                               { The default size of the thumb tab }
  TotalData: Integer;                                                                              { The size of the scroll bar track }
begin
  MinWidth:= GetSystemMetrics(SM_CXHTHUMB);                                                        { Save the default size. }
  OwnerClientWidth:= OwnerClientWidth -  2*GetSystemMetrics(SM_CXVSCROLL);
  TotalData:= (ScrollBar.Max - ScrollBar.Min + 1);

  ScrollBar.PageSize:= TotalData DIV OwnerClientWidth;
  if ScrollBar.PageSize < MinWidth                                                                 { PageSize is the size of the thumb tab, measured in the same units as Position, Min, and Max (not pixels). }
  then ScrollBar.PageSize := MinWidth;
end;


end.
