UNIT cvSpinEditDelayed;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------
  TCubicSpinEditD Features:
      Fixes the OnChange MinValue/MaxValue issue.
      Details here: http://stackoverflow.com/questions/17655854/how-do-i-prevent-users-from-entering-values-that-exceed-tspinedit-maxvalue

  How to use it:
      Set 'Delay' property
      Put your code in OnChanged event instead of OnChange

  Warning!
      If the OnChanged event fires while the program is stuck during startup in a messagebox (like Proteus showing "trial expired"), that event might access an object that was not yet created (because the initialization was stuck in that message box).

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

