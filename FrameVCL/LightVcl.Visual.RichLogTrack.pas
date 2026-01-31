UNIT LightVcl.Visual.RichLogTrack;

// OLD LOG based on RichEdit

{=============================================================================================================
   Gabriel Moraru
   2026.01.31
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Verbosity controller for TRichLog.
   A panel with a trackbar and label that controls the verbosity level of an associated TRichLog.
     Min= 0 (lvrVerbose)
     Max= 5 (lvrErrors)

   Tester:
     c:\Projects\LightSaber\Demo\LightLog\
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Classes, Vcl.Controls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls,
   LightVcl.Visual.RichLog, LightVcl.Visual.RichLogUtils;

TYPE
  TRichLogTrckbr = class(TPanel)
   private
     FLog    : TRichLog;
     VerboLabel: TLabel;
     FTrackBar: TTrackBar;
     Initialized : Boolean;
     FVerbChanged: TNotifyEvent;
     function  getVerbosity: TLogVerb;
     procedure setVerbosity(Value: TLogVerb);
     procedure setRichLog (const Value: TRichLog);
   protected
     procedure CreateWnd; override;
     procedure TrackBarChange(Sender: TObject);
   public
     constructor Create(AOwner: TComponent); override;
  published
     property TrackBar      : TTrackBar     read FTrackBar     write FTrackBar;
     property OnVerbChanged : TNotifyEvent  read FVerbChanged  write FVerbChanged;   { Triggered when the user changes the verbosity level }
     property Verbosity     : TLogVerb      read getVerbosity  write setVerbosity;
     property Log           : TRichLog      read FLog          write setRichLog;
  end;

procedure Register;


IMPLEMENTATION
USES LightCore, LightCore.Time;



constructor TRichLogTrckbr.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);             { Note: Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create }
 ControlState:= ControlState+ [csCreating];

 VerboLabel:= TLabel.Create(Self);     { Freed by: Owner }
 VerboLabel.Parent:= Self;             { Here we can set the parent }

 TrackBar:= TTrackBar.Create(Self);
 TrackBar.SetSubComponent(True);
 TrackBar.Parent:= Self;
end;


//CreateWnd can be called more than once: http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Controls_TWinControl_CreateWnd.html
procedure TRichLogTrckbr.CreateWnd;
begin
 inherited CreateWnd;
 ShowCaption := FALSE;                 { https://stackoverflow.com/questions/56859524/how-to-initialize-a-custom-control/64974040?noredirect=1#comment114931111_64974040 }

 if NOT Initialized then               { Make sure we don't call this code twice }
  begin
   Initialized:= TRUE;

   Width := 260;
   Height:= 27;
   BevelOuter:= bvNone;
   AlignWithMargins:= True;

   VerboLabel.Layout   := tlCenter;
   VerboLabel.Align    := alClient;
   VerboLabel.Alignment:= taCenter;
   VerboLabel.Hint     := 'Log verbosity' +#13#10+ 'Hide all messages below this level.';
   VerboLabel.Caption  := 'Log verbosity: '+ Verbosity2String(DefaultVerbosity);
  end;


 TrackBar.Min        := 0;
 TrackBar.Max        := Ord(lvrErrors);                        { About enumerations: http://www.delphipages.com/forum/showthread.php?t=58129 }
 TrackBar.Hint       := 'Hide all messages below this level';
 TrackBar.Align      := alRight;
 TrackBar.Name       := 'VerbosityTrackbar';                  { This control MUST have a name so I can save it to INI file }
 TrackBar.OnChange   := TrackBarChange;
 { Finally set the verbosity }
 TrackBar.Position   := Ord(DefaultVerbosity);                { I need this to synchroniz Log's and track's verbosity. Cannot be moved to CreateWnd if I use LoadForm(). Must be after TrackBar.OnChange:= TrackBarChange }
 
 ControlState:= ControlState- [csCreating];
end;



procedure TRichLogTrckbr.TrackBarChange(Sender: TObject);
begin
 if NOT (csLoading in ComponentState)
 AND NOT (csCreating in ControlState) then { This is MANDATORY because when the project loads, the value of the trackbar may change BEFORE the DFM loader assigns the Log to this trackbar. In other words, crash when I load a DFM file that contains this control }
  begin
   Assert(Log <> NIL, 'No log assigned!');
   Log.Verbosity:= Verbosity;
   VerboLabel.Caption:= 'Log verbosity: '+ Verbosity2String(Verbosity);

   if Assigned(FVerbChanged)
   then FVerbChanged(Self);                                                                  { Let GUI know that the user changed the verbosity }
  end;
end;





{ Returns the position of the trackbar as TLogVerb (instead of integer) }
function TRichLogTrckbr.getVerbosity: TLogVerb;
begin
 Result:= TLogVerb(TrackBar.Position);
end;


procedure TRichLogTrckbr.setVerbosity(Value: TLogVerb);
begin
 TrackBar.Position:= Ord(Value);
end;



procedure TRichLogTrckbr.setRichLog(CONST Value: TRichLog);
begin
 FLog:= Value;
 if FLog <> NIL
 then FLog.Verbosity:= Verbosity;
end;







procedure Register;
begin
 RegisterComponents('LightSaber VCL', [TRichLogTrckbr]);
end;


end.
