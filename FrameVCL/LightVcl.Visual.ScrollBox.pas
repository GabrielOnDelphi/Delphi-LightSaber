UNIT LightVcl.Visual.ScrollBox;

{=============================================================================================================
   Gabriel Moraru
   2026.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------

  Features:
      + OnVerticalScroll event      (when the user clicks the scrollbar and starts scrolling )
      + OnHorizontalScroll event
=============================================================================================================}
{See: What's the difference between CreateWnd and CreateWindowHandle? https://stackoverflow.com/questions/582903/whats-the-difference-between-createwnd-and-createwindowhandle .   //CreateWnd can be called more than once:  http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Controls_TWinControl_CreateWnd.html }

INTERFACE

USES
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms;

TYPE
  TVScrollEventType = (vsLineUp, vsLineDown, vsPageUp, vsPageDown, vsThumbPos, vsThumbTrack, vsTop, vsBottom, vsEndScroll);
  THScrollEventType = (hsLineLeft, hsLineRight, hsPageLeft, hsPageRight, hsThumbPos, hsThumbTrack, hsLeft, hsRight, hsEndScroll);
  TVScrollEvent     = Procedure(Sender: TObject; Pos: SmallInt; EventType: TVScrollEventType) of Object;
  THScrollEvent     = Procedure(Sender: TObject; Pos: SmallInt; EventType: THScrollEventType) of Object;

  TCubicScrollBox = class(TScrollBox)
    private
      FOnVScroll : TVScrollEvent;
      FOnHScroll : THScrollEvent;
      FPrevCtrl  : TControl;                                                                           { Used by NextControl enumeration }
      Procedure WMVScroll(Var Message : TWMScroll); message WM_VScroll;
      Procedure WMHScroll(Var Message : TWMScroll); message WM_HScroll;
    protected
      Procedure VScroll(Pos: integer; EventType : TVScrollEventType); virtual;
      Procedure HScroll(Pos: integer; EventType : THScrollEventType); virtual;
      function  DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;  {DOESN'T WORK!}
    public
      constructor Create(AOwner : TComponent); override;
      {}
      procedure ResetToFirstCtrl;
      function  FirstControl: TControl;
      function  NextControl: TControl;   { Returns the controls in physical order (sorted by .Top) }
      function  LastControl: TControl;
    published
      Property OnVerticalScroll  : TVScrollEvent read  FOnVScroll Write FOnVScroll;
      Property OnHorizontalScroll: THScrollEvent read  FOnHScroll Write FOnHScroll;
    end;

procedure Register;

IMPLEMENTATION



Constructor TCubicScrollBox.Create(AOwner : TComponent);
begin
 inherited Create(AOwner);
 // Note: Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create
 FOnVScroll:= NIL;
 FOnHScroll:= NIL;
 FPrevCtrl := NIL;
end;





Procedure TCubicScrollBox.VScroll(Pos: integer; EventType: TVScrollEventType);
begin
  if Assigned(FOnVScroll)
  then FOnVScroll(Self, Pos, EventType);
end;

Procedure TCubicScrollBox.HScroll(Pos: Integer; EventType: THScrollEventType);
begin
  if assigned(FOnHScroll)
  then FOnHScroll(Self, Pos, EventType);
end;





Procedure TCubicScrollBox.WMVScroll(Var Message: TWMScroll);
var EventType : TVScrollEventType;
begin
  inherited;
  EventType := TVScrollEventType(Message.ScrollCode);
  if   EventType in [vsThumbPos, vsThumbTrack]
  then VScroll(Message.Pos,            EventType)
  else VScroll(VertScrollBar.Position, EventType)
end;


Procedure TCubicScrollBox.WMHScroll(Var Message: TWMScroll);
var EventType : THScrollEventType;
begin
  inherited;
  EventType := THScrollEventType(Message.ScrollCode);
  if EventType in [hsThumbPos, hsThumbTrack]
  then HScroll(Message.Pos,            EventType)
  else HScroll(HorzScrollBar.Position, EventType)
end;












function TCubicScrollBox.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
{from here: http://stackoverflow.com/questions/12955008/how-can-i-fix-the-tscrollbar-mousewheel-malfunction
 DOESN'T WORK!}
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then
   begin
    if Shift*[ssShift..ssCtrl]=[] then
     begin
      VertScrollBar.Position := VertScrollBar.Position - WheelDelta;
      Result := True;
     end;
   end;
end;













{-------------------------------------------------------------------------------------------------------------
  Returns the controls sorted in physical order (sorted by .Top)
  Returns NIL where there are no more controls.
-------------------------------------------------------------------------------------------------------------}
procedure TCubicScrollBox.ResetToFirstCtrl;  { Call this after using NextControl to enumerate ALL controls if you want to reset the loop and start over }
begin
  FPrevCtrl:= NIL;
end;


function TCubicScrollBox.NextControl: TControl;
VAR
   CurCtrl: TControl;
   i, BtmDist: Integer;
   LastBtmDist: Integer;
begin
 if ControlCount= 0
 then raise exception.Create('No controls!');

 Result:= NIL;     { Return NIL when there are no more controls }
 LastBtmDist:= MaxInt;

 if FPrevCtrl= NIL
 then
  begin
   Result:= FirstControl;
   FPrevCtrl:= Result;
  end
 else
   for i:= 0 to ControlCount-1 DO
    begin
     CurCtrl:= Controls[i];

     { Don't compare to itself }
     if FPrevCtrl = CurCtrl
     then Continue;

     { CurCtrl is above FPrevCtrl }
     if CurCtrl.Top <= FPrevCtrl.Top
     then Continue;

     { Distance between previous returned control's bottom and current ctrl's top }
     BtmDist:= (FPrevCtrl.Top+ FPrevCtrl.Height) - CurCtrl.Top;

     if BtmDist < 0        { Negative means CurCtrl starts below FPrevCtrl's bottom }
     then Continue;        { Skip - looking for overlapping or adjacent controls }

     if BtmDist < LastBtmDist then
      begin
       LastBtmDist:= BtmDist;
       Result:= CurCtrl;
       FPrevCtrl:= CurCtrl;
      end;
    end;
end;


function TCubicScrollBox.FirstControl: TControl;   { Returns the control with the smallest Top (0 in most cases) }
VAR
   LastTop, i: Integer;
begin
 Assert(ControlCount > 0, 'TCubicScrollBox.FirstControl: No controls!');
 Result:= Controls[0];
 LastTop:= MaxInt;

 for i:= 0 to ControlCount-1 DO
    if Controls[i].Top < LastTop then
      begin
       LastTop:= Controls[i].Top;
       Result:= Controls[i];
      end;
end;


function TCubicScrollBox.LastControl: TControl;  { Returns the control with the biggest Top }
VAR
   LastTop, i: Integer;
begin
 Assert(ControlCount > 0, 'TCubicScrollBox.LastControl: No controls!');
 Result:= Controls[0];
 LastTop:= 0;

 for i:= 0 to ControlCount-1 DO
    if Controls[i].Top > LastTop then
      begin
       LastTop:= Controls[i].Top;
       Result:= Controls[i];
      end;
end;






procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TCubicScrollBox]);
end;


end.




