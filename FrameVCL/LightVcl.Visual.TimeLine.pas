UNIT LightVcl.Visual.TimeLine;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------

 A control that tries to simulate the timeline bar in VLC or BSPlayer.
 Additionally the user can place a "maker" at a certain timestamp, with MarkerPos.

 Similar controls:
   C:\MyProjects\Packages\Third party packages]RangeSelector.pas

 Tester:
   c:\Myprojects\Project Testers\TimeLine tester\
}

INTERFACE

USES
   System.SysUtils, System.Classes, Vcl.StdCtrls, Vcl.Dialogs,
  Vcl.Controls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Graphics;


TYPE
 TTimeLine = class(TPanel)
  protected
    FProgress: TProgressBar;
    FPositionChanged: TNotifyEvent;
    Marker: TLabel;
    FMarkerPos: Integer;
    procedure setMarkerPos(Value: Integer);
    procedure progressMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    function PixelsPerSec: Real;
  public
    constructor Create(aOwner: TComponent); override;
  published
    property MarkerPos: integer read FMarkerPos write setMarkerPos;
    property Progress: TProgressBar read FProgress write FProgress;
    property PositionChanged: TNotifyEvent read FPositionChanged write FPositionChanged;   // Triggered when the user click the TimeLine to change its position
 end;

 procedure Register;


implementation


constructor TTimeLine.Create(aOwner: TComponent);
begin
 inherited Create(aOwner); // Note: Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create
 Width  := 400;
 Height := 35;
 BevelOuter:= bvNone;
 DoubleBuffered:= True;

 fProgress:= TProgressBar.Create(Self);
 fProgress.Parent := Self;    //todo 1: make this a subcomponent
 fProgress.SetSubComponent(True);
 fProgress.Name := 'Progress';
 FProgress.Visible:= TRUE;
 FProgress.Align  := alTop;
 FProgress.Max    := 120;
 FProgress.Smooth := TRUE;
 FProgress.MarqueeInterval := 100;
 FProgress.OnMouseDown := ProgressMouseDown;

 Marker:= TLabel.Create(Self);
 Marker.Parent      := Self;
 Marker.Left        := 20;
 Marker.Top         := Progress.Height;
 Marker.Caption     := '^';
 Marker.Font.Height := -12;
 Marker.Font.Name   := 'Tahoma';
 Marker.Font.Style  := [fsBold];
 Marker.AutoSize    := True;
end;


procedure TTimeLine.ProgressMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 Progress.Position:= Round(x * Progress.Max / Progress.ClientWidth);
 if Assigned(FPositionChanged)
 then FPositionChanged(Sender);    // Triggered when the user click the TimeLine to change its position
end;



function TTimeLine.PixelsPerSec: Real;  { Returns the number of pixels per timeline second. }
begin
 Result:= Progress.ClientWidth / Progress.Max;
end;


procedure TTimeLine.setMarkerPos(Value: Integer);
begin
 if (Value < 0) OR (Value > Progress.Max)
 then ShowMessage('TTimeLine.setMarkerPos - Invalid input: '+ IntToStr(Value))
 else
  begin
   FMarkerPos:= Value;
   Marker.Left:= Round((Value * PixelsPerSec) - (Marker.Width / 2));
  end;
end;

{
function TTimeLine.getMarkerPos: integer;
begin
 Result:= Marker.Left + round(Marker.Width / 2);
end;  }






procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TTimeLine]);
end;


end.
