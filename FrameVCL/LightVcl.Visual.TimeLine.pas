UNIT LightVcl.Visual.TimeLine;

{=============================================================================================================
   Gabriel Moraru
   2026.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------

 A control that simulates the timeline bar in VLC or BSPlayer.
 The user can click on the progress bar to change position.
 Additionally, a "marker" (^) can be placed at a specific position via MarkerPos.

 Duration/MarkerPos units are generic (can represent seconds, frames, etc. - depends on usage).

 Similar controls:
   C:\Projects\Packages\Third party packages]RangeSelector.pas

 Tester:
   c:\Projects\Project Testers\TimeLine tester\
}

INTERFACE

USES
   System.SysUtils, System.Classes,
   Vcl.Controls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Graphics, Vcl.StdCtrls;


TYPE
 TTimeLine = class(TPanel)
  private
    FProgress: TProgressBar;
    FMarker: TLabel;
    FMarkerPos: Integer;
    FPositionChanged: TNotifyEvent;
    function GetDuration: Integer;
    procedure SetDuration(Value: Integer);
    procedure SetMarkerPos(Value: Integer);
    procedure ProgressMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function PixelsPerUnit: Double;
    procedure UpdateMarkerPosition;
  protected
    procedure Loaded; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Duration: Integer read GetDuration write SetDuration default 120;
    property MarkerPos: Integer read FMarkerPos write SetMarkerPos default 0;
    property Progress: TProgressBar read FProgress;
    property PositionChanged: TNotifyEvent read FPositionChanged write FPositionChanged;   { Triggered when the user clicks the TimeLine to change its position }
 end;

 procedure Register;


IMPLEMENTATION


constructor TTimeLine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner); // Note: Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create
  Width  := 400;
  Height := 35;
  BevelOuter:= bvNone;
  DoubleBuffered:= TRUE;

  FProgress:= TProgressBar.Create(Self);
  FProgress.SetSubComponent(TRUE);
  FProgress.Name        := 'Progress';
  FProgress.Parent      := Self;
  FProgress.Visible     := TRUE;
  FProgress.Align       := alTop;
  FProgress.Max         := 120;
  FProgress.Smooth      := TRUE;
  FProgress.MarqueeInterval := 100;
  FProgress.OnMouseDown := ProgressMouseDown;

  FMarker:= TLabel.Create(Self);
  FMarker.SetSubComponent(TRUE);
  FMarker.Name        := 'Marker';
  FMarker.Parent      := Self;
  FMarker.Left        := 0;
  FMarker.Top         := FProgress.Height;
  FMarker.Caption     := '^';
  FMarker.Font.Height := -12;
  FMarker.Font.Name   := 'Tahoma';
  FMarker.Font.Style  := [fsBold];
  FMarker.AutoSize    := TRUE;
end;


procedure TTimeLine.Loaded;
begin
  inherited;
  UpdateMarkerPosition;
end;


procedure TTimeLine.Resize;
begin
  inherited;
  UpdateMarkerPosition;
end;


{ Handles click on the progress bar: converts pixel X coordinate to timeline position }
procedure TTimeLine.ProgressMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FProgress.ClientWidth > 0
  then FProgress.Position:= Round(X * FProgress.Max / FProgress.ClientWidth);

  if Assigned(FPositionChanged)
  then FPositionChanged(Self);
end;


{ Returns how many pixels correspond to one unit of duration.
  Returns 0 if Duration is 0 (to prevent division by zero). }
function TTimeLine.PixelsPerUnit: Double;
begin
  if FProgress.Max > 0
  then Result:= FProgress.ClientWidth / FProgress.Max
  else Result:= 0;
end;


{ Positions the marker (^) centered on the MarkerPos.
  Note: When MarkerPos is 0, Left may become negative to center the marker. }
procedure TTimeLine.UpdateMarkerPosition;
VAR
   PixPerUnit: Double;
begin
  PixPerUnit:= PixelsPerUnit;
  if PixPerUnit > 0
  then FMarker.Left:= Round((FMarkerPos * PixPerUnit) - (FMarker.Width / 2))
  else FMarker.Left:= 0;
end;


procedure TTimeLine.SetMarkerPos(Value: Integer);
begin
  if Value < 0
  then raise EArgumentOutOfRangeException.CreateFmt('TTimeLine.MarkerPos: Value (%d) cannot be negative', [Value]);

  if Value > FProgress.Max
  then raise EArgumentOutOfRangeException.CreateFmt('TTimeLine.MarkerPos: Value (%d) exceeds Duration (%d)', [Value, FProgress.Max]);

  FMarkerPos:= Value;
  UpdateMarkerPosition;
end;


function TTimeLine.GetDuration: Integer;
begin
  Result:= FProgress.Max;
end;


procedure TTimeLine.SetDuration(Value: Integer);
begin
  if Value < 0
  then raise EArgumentOutOfRangeException.CreateFmt('TTimeLine.Duration: Value (%d) cannot be negative', [Value]);

  FProgress.Max:= Value;

  { Adjust marker if it's now out of bounds }
  if FMarkerPos > Value
  then FMarkerPos:= Value;

  UpdateMarkerPosition;
end;


procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TTimeLine]);
end;


end.
