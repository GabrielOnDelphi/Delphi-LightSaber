UNIT cvTimer;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

  Better Timer
-----------------------------------------------------------------------------------------------------------------------}

INTERFACE  {.$WARN UNIT_PLATFORM OFF}

USES
   System.Classes, Vcl.ExtCtrls;

TYPE
  TCubicTimer = class(TTimer)
  private
  public
    procedure Restart;   { Sets the countdown back to zero and starts the timer }
    procedure Reset;     { Sets the countdown back to zero and starts the timer ONLY if was enabled before }
    procedure Stop;
  end;

procedure ResetTimer(Timer: TTimer);
procedure Register;



IMPLEMENTATION



procedure ResetTimer(Timer: TTimer);   { Utility for those timers that are TTimer and not TCubicTimer }
begin
 if NOT Timer.Enabled then EXIT;

 Timer.Enabled:= FALSE;      { Stop timer }
 Timer.Enabled:= TRUE;       { Start the timer ONLY if was enabled before }
end;







procedure TCubicTimer.Reset;   { Start the timer ONLY if was enabled before }
begin
 if Enabled then Restart;
end;


procedure TCubicTimer.Restart; { Sets the countdown back to zero and starts the timer }
begin
 Enabled:= FALSE;
 Enabled:= TRUE;
end;


procedure TCubicTimer.Stop;   { Does nothing more special than Enabled=false, but has a better name... }
begin
 Enabled:= FALSE;
end;




procedure Register;
begin
 RegisterComponents('LightSaber', [TCubicTimer]);
end;


end.
