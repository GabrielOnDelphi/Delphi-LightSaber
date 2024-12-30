UNIT cvSpinEditDelayed;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------
  TCubicSpinEditD - Fixes the MinValue/MaxValue issue.

  Problem:
      If we set the TSpinEdit.MaxValue to 100, when the program runs, it still lets the user enter values
      over 100 by manually entering the number (instead of using the spins).
      When the user enters a wrong value, this can result in a RageCheckError or something similar.
      So, the programmer needs to write code to check the value in OnChange event.
  Fix:
      This control offers an extra event called OnChanged which is called 1200ms after the user entered the value.

  How to use it:
      Set 'Delay' property.
      Put your code in OnChanged event instead of OnChange.

  Warning!
      OnChanged event fires after a delay.
      If it fires while the program is stuck during startup in a messagebox, OnChanged might access an object
      that was not yet created (because the initialization was halted by that message box). So, use it with care.

  Tester
     c:\Myprojects\Project Testers\Cubic VCL SpinEdits\Tester.dpr
-------------------------------------------------------------------------------------------------------------}

INTERFACE

{$D+}
{$WARN GARBAGE OFF}   {Silent the: 'W1011 Text after final END' warning }

USES
  System.SysUtils, System.Classes, Vcl.Samples.Spin, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls;


TYPE
 TCubicSpinEditD = class(TSpinEdit)
  private
   Timer: TTimer; 
   FDelay: integer;
   FOnChanged: TNotifyEvent;
   procedure TimerTimer(Sender: TObject);
   procedure SetDelay(CONST aValue: Integer);
  public
   constructor Create (AOwner: TComponent); override;
   procedure Change;  override;
   procedure StopTimer;    { Used by LoadForm to stop the even from triggering some seconds later after the value of this spinbox was loaded from the INI file }
  published
   property Delay    : integer      read FDelay     write SetDelay default 1200;                                { The OnChanged event will be called after this delay (ms). Set it to zero to disable it. }
   property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
 end;


procedure Register;


IMPLEMENTATION


procedure Register;
begin

 RegisterComponents('LightSaber', [TCubicSpinEditD]);
end;














{--------------------------------------------------------------------------------------------------
   TCubicSpinEditD
--------------------------------------------------------------------------------------------------}
constructor TCubicSpinEditD.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);  // Note: Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create
 Timer:= TTimer.Create(Self);
 Timer.OnTimer:= TimerTimer;
 FDelay:= 1200;                                                                                    { ms }
 Timer.Interval:= Delay;                                                                           { allow user few seconds to enter a new correct value }
end;



procedure TCubicSpinEditD.Change;
begin
 if (Timer<> NIL)  { Timer is NIL at start up }
 AND (Delay> 0) then
  begin
   Timer.Enabled:= FALSE;
   Timer.Enabled:= TRUE;
  end;
 inherited;
end;


procedure TCubicSpinEditD.TimerTimer(Sender: TObject);
begin
 Timer.Enabled:= FALSE;

 if (MaxValue<> 0) AND (Value> MaxValue)
 then Value:= MaxValue;

 if (MinValue<> 0) AND (Value< MinValue)
 then Value:= MinValue;

 if Assigned(FOnChanged)
 then FOnChanged(Self);
end;


procedure TCubicSpinEditD.SetDelay(CONST aValue: Integer);
begin
 FDelay:= aValue;
 Timer.Interval:= FDelay;
end;


procedure TCubicSpinEditD.StopTimer;
begin
 Timer.Enabled:= FALSE;
end;






end.

