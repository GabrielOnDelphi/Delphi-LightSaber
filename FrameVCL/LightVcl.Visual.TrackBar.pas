UNIT LightVcl.Visual.TrackBar;

{=============================================================================================================
   Gabriel Moraru
   2026.11
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
  A TTrackBar with one more event: OnChangeDone.

  OnChange fires at every step of a drag, so it is the wrong place for work that costs much (a restart, a reload).
  OnChangeDone fires once per user gesture, when the gesture ends at a value other than the one it started from:
    - the mouse button is released, after a drag or after a click in the channel
    - a direction key (arrows, PageUp, PageDown, Home, End) is released
    - one notch of the mouse wheel moved the thumb
  A Position set in code never fires it.

  How: the control itself sends TB_ENDTRACK when a gesture ends, so no timer is needed.
  Microsoft, "Default Trackbar Message Processing": WM_LBUTTONUP and WM_CAPTURECHANGED "always" send TB_ENDTRACK,
  and WM_KEYUP sends it "if the key was one of the direction keys".
  Source: https://learn.microsoft.com/en-us/windows/win32/controls/trackbar-controls
  The parent gets these as WM_HSCROLL / WM_VSCROLL; the VCL hands them back to the trackbar as CN_HSCROLL / CN_VSCROLL.
  That page says nothing about WM_MOUSEWHEEL.
  Measured (TestProbe_WheelScrollCodes, comctl32 6.0.26100.9818, Windows 11): one notch sends TB_THUMBPOSITION only, no TB_ENDTRACK.
  So WMMouseWheel ends the gesture itself.
  A comctl32 that also sends TB_ENDTRACK for a notch ends the gesture there first, and nothing fires twice.

  Tests: c:\Projects\LightSaber\UnitTesting\Test.LightVcl.Visual.TrackBar.pas
=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, Winapi.Messages, Winapi.CommCtrl,
  System.Classes,
  Vcl.Controls, Vcl.ComCtrls;

TYPE
  TLightTrackBar = class(TTrackBar)
  private
    FOnChangeDone: TNotifyEvent;
    FInGesture   : Boolean;   { TRUE from the first scroll notification of a gesture until the gesture ends }
    FGestureStart: Integer;   { Position when the gesture started }
    procedure beforeScroll(ScrollCode: Integer);
    procedure afterScroll(ScrollCode: Integer);
    procedure endGesture;
    procedure CNHScroll(var Message: TWMHScroll); message CN_HSCROLL;
    procedure CNVScroll(var Message: TWMVScroll); message CN_VSCROLL;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
  protected
    procedure ChangeDone; dynamic;
  published
    { Fires once when the user ends a drag, a click, a key press or a wheel notch with a new value. Never for a Position set in code }
    property OnChangeDone: TNotifyEvent read FOnChangeDone write FOnChangeDone;
  end;


procedure Register;


IMPLEMENTATION


{ Called before inherited: Position still holds the value from before this notification,
  because TTrackBar.CNHScroll reads the new one (TBM_GETPOS) only in inherited }
procedure TLightTrackBar.beforeScroll(ScrollCode: Integer);
begin
  if (ScrollCode <> TB_ENDTRACK) AND NOT FInGesture then
   begin
    FInGesture   := TRUE;
    FGestureStart:= Position;
   end;
end;


procedure TLightTrackBar.afterScroll(ScrollCode: Integer);
begin
  if ScrollCode = TB_ENDTRACK
  then endGesture;
end;


{ A gesture that ends where it started (a drag back to the start, a key press at the end of the range) fires nothing }
procedure TLightTrackBar.endGesture;
VAR Moved: Boolean;
begin
  Moved:= FInGesture AND (Position <> FGestureStart);
  FInGesture:= FALSE;
  if Moved
  then ChangeDone;
end;


procedure TLightTrackBar.CNHScroll(var Message: TWMHScroll);
VAR ScrollCode: Integer;
begin
  ScrollCode:= Message.ScrollCode;
  beforeScroll(ScrollCode);
  inherited;
  afterScroll(ScrollCode);
end;


procedure TLightTrackBar.CNVScroll(var Message: TWMVScroll);
VAR ScrollCode: Integer;
begin
  ScrollCode:= Message.ScrollCode;
  beforeScroll(ScrollCode);
  inherited;
  afterScroll(ScrollCode);
end;


{ The thumb moves while inherited runs: the control sends its scroll notification (TB_THUMBPOSITION) from inside its own wheel handler.
  Not while a drag holds the mouse capture (WM_LBUTTONDOWN sets it, per the page in the header): the release of the button ends that gesture }
procedure TLightTrackBar.WMMouseWheel(var Message: TWMMouseWheel);
begin
  inherited;
  if GetCapture <> Handle
  then endGesture;
end;


procedure TLightTrackBar.ChangeDone;
begin
  if Assigned(FOnChangeDone)
  then FOnChangeDone(Self);
end;



procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TLightTrackBar]);
end;


end.
