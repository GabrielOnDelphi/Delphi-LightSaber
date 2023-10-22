UNIT cvCountDown;

{=============================================================================================================
 2020.10.30
 A control that counts down from 'StartValue' to 0ms.

 Every 'Resolution' ms a Tick event is generated.

 TimeLeft shows how many seconds are left until zero.
 When the time is up, an event is triggered.
 The timer can also be manually decremented from outside, by calling Tick.
=============================================================================================================}


INTERFACE

USES
   System.SysUtils, System.Classes, Vcl.Taskbar, cvTimer, cvActivityIndicator;

TYPE
 TIntervalChg = procedure(Sender: TObject; NewTime: Cardinal) of object;

 TCountDown = class(TComponent)
 private
   wasEnabled     : Boolean;       { Used to resume the countdown }
   Timer          : TCubicTimer;
   FTimeLeft      : Cardinal;
   FStartValue    : Cardinal;
   FOnTimeUpEvent : TNotifyEvent;
   FOnTick        : TNotifyEvent;
   FIntervalChg   : TIntervalChg;  { ms }
   procedure setStartValue(Value: Cardinal);
   procedure TimerTimer(Sender: TObject);
   procedure setResolution(Value: Cardinal);
   function  getResolution: Cardinal;
 protected
   procedure Tick;                     { Call this to advance the countdown }
 public
   Taskbar      : TTaskbar;            { Associated indicators. Show time progress }
   ActivityIndic: TActivityIndicatorC; { Associated indicators. Show time progress }

   constructor Create(AOwner: TComponent); override;
   function  TimeLeftNice: Integer;    { If interval under 15 minutes, show time in seconds (max 3 digits: up to 999 seconds) else show time in minutes }
   procedure Reset;                    { Resets the TimeLeft. Does not start the timer! }
   procedure Restart;                  { Resets the TimeLeft and start the timer }
   procedure Start;
   procedure Stop;
   function  Resume: Boolean;          { Resume the countdown from where it was left BUT ONLY if when we called Stop, it was enabled/running }
   function  Enabled: Boolean;
   function  TimeLeftS: string;
 published
   property TimeLeft  : Cardinal     read FTimeLeft;
   property StartValue: Cardinal     read FStartValue    write setStartValue default 10000; { The value from which the count down starts. Cannot be zero! }
   property Resolution: Cardinal     read getResolution  write setResolution default  1000; { How much it should substract from 'TimeLeft' every time 'Tick' is called. Basically it should be equal with the ExternalTimer.Interval }

   property OnTimesUp : TNotifyEvent read FOnTimeUpEvent write FOnTimeUpEvent;
   property OnTick    : TNotifyEvent read FOnTick        write FOnTick;
   property OnIntervalChg: TIntervalChg read FIntervalChg  write FIntervalChg; { Triggered when we change the StartValue }
 end;


procedure Register;

IMPLEMENTATION

USES cmMath, ccCore;




constructor TCountDown.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FStartValue  := 10000;
 FTimeLeft    := FStartValue;
 { Timer }
 Timer        := TCubicTimer.Create(Self);  { Freed by: Self }
 Timer.Enabled:= FALSE;
 Timer.OnTimer:= TimerTimer;
 Timer.Interval:= 1000;                     { 1sec }
end;


procedure TCountDown.Tick;
begin
 if Enabled then
  begin
   FTimeLeft:= FTimeLeft- Resolution;

   if Assigned(OnTick)
   then OnTick(Self);

   if FTimeLeft <= 0 then
    begin
     Reset;              { This must be ABOVE the OnTimesUp event! because if OnTimesUp calls a procedure that contains Application.ProcessMessages and the timer will advance without getting the chance to reset TimeLeft, so timeleft may become negative! }
     if Assigned(OnTimesUp)
     then OnTimesUp(Self);
    end;
  end;
end;


procedure TCountDown.Start;
begin
 FTimeLeft:= StartValue;     { Reset the time left to its original value }
 Timer.Restart;              { Sets the countdown back to zero and starts the timer }
 if ActivityIndic <> NIL then ActivityIndic.Animate:= TRUE;
 if Taskbar <> NIL then Taskbar.ProgressValue:= 0;
end;


procedure TCountDown.Stop;
begin
 wasEnabled:= Timer.Enabled; { Used by ResumeCountdown }
 Timer.Enabled:= FALSE;      { Stop timer }
 if ActivityIndic <> NIL then ActivityIndic.Animate:= FALSE;
 if Taskbar       <> NIL then Taskbar.ProgressValue:= 0;
end;


procedure TCountDown.Reset;
begin
 FTimeLeft:= StartValue;     { Reset the TimeLeft to its original value and starts timer ONLY if was enabled before }
 Timer.Reset;
end;


procedure TCountDown.Restart;
begin
 Reset;                      { Reset the time left to its original value }
 Timer.Enabled:= TRUE;
end;


function TCountDown.Resume: Boolean; { Resume the countdown from where it was left BUT ONLY if when we called Stop, it was enabled/running }
begin
 Result:= wasEnabled;
 if Result
 then Start;
 if ActivityIndic <> NIL then ActivityIndic.Animate:= Result;
 if Taskbar       <> NIL then Taskbar.ProgressValue:= 0;
end;








{ If interval under 15 minutes, show time in seconds (max 3 digits: up to 999 seconds) else show time in minutes }
function TCountDown.TimeLeftNice: Integer;        { Used by TrayText.Text }
begin
 if StartValue <= 15 * SecsPerMin * MSecsPerSec
 then Result:= RoundEx(TimeLeft/1000)
 else Result:= RoundEx(TimeLeft/60000)
end;


function TCountDown.TimeLeftS: string;
begin
 Result:= MiliSecToTimeAuto(TimeLeft);
end;


procedure TCountDown.TimerTimer(Sender: TObject);
begin
 Tick;
end;






{-------------------------------------------------------------------------------------------------------------
   GETTERS/SETTERS
-------------------------------------------------------------------------------------------------------------}
function TCountDown.Enabled: Boolean;
begin
 Result:= Timer.Enabled;
end;



procedure TCountDown.setStartValue(Value: Cardinal);
VAR iTime: Integer;
begin
 if FStartValue <> Value then
  begin
   FStartValue:= Value;
   FTimeLeft:= Value;
   Reset;

   { Update associated indicators }
   iTime:= Round(Value / 2);            { Compute animation speed for aciCountDown }
   cmMath.EnsureRange(iTime, 50, 1000);  { 50 is hit at 1min 38sec. 100 is hit at 33 minutes }
   if ActivityIndic <> NIL then ActivityIndic.FrameDelay:= iTime;
   if Taskbar <> NIL then
    begin
     Taskbar.ProgressMaxValue:= Value DIV MSecsPerSec;  { In seconds }
     Taskbar.ProgressValue:= 0;
    end;

   if Assigned(FIntervalChg)
   then FIntervalChg(Self, Value);   { We let the user know that the StartValue was changed }
  end;
end;


procedure TCountDown.setResolution(Value: Cardinal);
begin
 Timer.Interval:= Value;
end;


function TCountDown.getResolution: Cardinal;
begin
 Result:= Timer.Interval;
end;







procedure Register;
begin
  RegisterComponents('LightSaber', [TCountDown]);
end;

end.
