UNIT LightVcl.Visual.Timer;

{=============================================================================================================
   Gabriel Moraru
   2026.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------

  Enhanced timer component with convenient restart/reset methods.
  Restart: Resets the countdown to zero and starts the timer.
  Reset:   Resets the countdown to zero but only starts if timer was already enabled.
  Stop:    Convenient alias for Enabled:= FALSE.
=============================================================================================================}

INTERFACE

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



{---------------------------------------------------------------------------------------------------------------
  Utility function for resetting standard TTimer objects (not TCubicTimer).
  Resets the timer interval countdown by disabling and re-enabling.
  Only acts if the timer is currently enabled.
---------------------------------------------------------------------------------------------------------------}
procedure ResetTimer(Timer: TTimer);
begin
 Assert(Timer <> NIL, 'ResetTimer: Timer parameter cannot be nil');
 if NOT Timer.Enabled then EXIT;

 Timer.Enabled:= FALSE;
 Timer.Enabled:= TRUE;
end;





{---------------------------------------------------------------------------------------------------------------
  Resets the timer interval countdown but only if the timer was already enabled.
  Use this when you want to restart the countdown without changing the enabled state.
---------------------------------------------------------------------------------------------------------------}
procedure TCubicTimer.Reset;
begin
 if Enabled then Restart;
end;


{---------------------------------------------------------------------------------------------------------------
  Resets the timer interval countdown and starts the timer (sets Enabled:= TRUE).
  Use this when you want to ensure the timer is running from a fresh countdown.
---------------------------------------------------------------------------------------------------------------}
procedure TCubicTimer.Restart;
begin
 Enabled:= FALSE;
 Enabled:= TRUE;
end;


{---------------------------------------------------------------------------------------------------------------
  Stops the timer. Semantically clearer alias for Enabled:= FALSE.
---------------------------------------------------------------------------------------------------------------}
procedure TCubicTimer.Stop;
begin
 Enabled:= FALSE;
end;




procedure Register;
begin
 RegisterComponents('LightSaber VCL', [TCubicTimer]);
end;


end.
