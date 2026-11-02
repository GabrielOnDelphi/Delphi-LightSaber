UNIT LightCore.FramePacer;

{=============================================================================================================
   2026.11.02
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   TFramePacer - the frame schedule of a frame timer, with no timer inside.

   The caller passes the clock in (milliseconds, any origin), so the schedule can be tested without a real timer.
   LightCore.Win.FrameTimer.pas uses it on its worker thread.

   Rules:
     The next frame is due one period after this one was DUE, not after it was drawn, so a late frame adds no drift.
     After a stall longer than one period the schedule restarts at now + period instead of delivering a burst.
     SetIntervalMs on a running pacer counts from the due time of the last frame: next due = last due + new interval.

   Thread safety: none. Only one thread may use one TFramePacer.
=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Math;

TYPE
  TFramePacer = record
  private
    FNextDueMs : Double;
    FLastDueMs : Double;
    FIntervalMs: Double;
  public
    procedure Start(NowMs: Double);                 { First frame due at NowMs + IntervalMs, as TTimer }
    function  IsDue(NowMs: Double): Boolean;        { TRUE once per period; advances the schedule }
    function  MsUntilDue(NowMs: Double): Double;    { How long the caller should wait; 0 when due }
    procedure SetIntervalMs(Value: Double);         { Raises on <= 0, NaN or infinite }
    property  IntervalMs: Double read FIntervalMs;
  end;


IMPLEMENTATION


procedure TFramePacer.Start(NowMs: Double);
begin
  if NOT (FIntervalMs > 0)
  then raise EInvalidOpException.Create('TFramePacer.Start: call SetIntervalMs first.');

  FLastDueMs:= NowMs;
  FNextDueMs:= NowMs + FIntervalMs;
end;


function TFramePacer.IsDue(NowMs: Double): Boolean;
begin
  Result:= NowMs >= FNextDueMs;
  if NOT Result then EXIT;

  FLastDueMs:= FNextDueMs;
  FNextDueMs:= FNextDueMs + FIntervalMs;

  { Stall: restart the schedule from now, so the frames missed are dropped, not delivered back to back }
  if FNextDueMs < NowMs then
  begin
    FLastDueMs:= NowMs;
    FNextDueMs:= NowMs + FIntervalMs;
  end;
end;


function TFramePacer.MsUntilDue(NowMs: Double): Double;
begin
  Result:= Max(0, FNextDueMs - NowMs);
end;


procedure TFramePacer.SetIntervalMs(Value: Double);
begin
  if IsNan(Value) OR IsInfinite(Value) OR (Value <= 0)
  then raise EArgumentOutOfRangeException.Create('TFramePacer.SetIntervalMs: the interval must be a positive number of milliseconds.');

  FIntervalMs:= Value;
  FNextDueMs := FLastDueMs + Value;   { Before Start this value is overwritten by Start }
end;


end.
