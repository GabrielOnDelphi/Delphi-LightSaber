UNIT LightVcl.Visual.RichLogTrack;

{=============================================================================================================
   Gabriel Moraru
   2026.11.06
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Verbosity controller for TRichLog.
   A panel with a trackbar and label that controls the verbosity level of an associated TRichLog.
     Min= 0 (lvrVerbose)
     Max= 5 (lvrErrors)

   Tester:
     c:\Projects\LightSaber\Demo\VCL\Demo LightLog\VCL_Demo_Log.dpr
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
     procedure Notification(AComponent: TComponent; Operation: TOperation); override;
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
VAR FirstWnd: Boolean;
begin
 inherited CreateWnd;
 ShowCaption := FALSE;                 { https://stackoverflow.com/questions/56859524/how-to-initialize-a-custom-control/64974040?noredirect=1#comment114931111_64974040 }

 FirstWnd:= NOT Initialized;
 if FirstWnd then                      { Make sure we don't call this code twice }
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
 { Finally set the verbosity. Only on the first window: a window created again (RecreateWnd) keeps the level the user chose (test: TestRecreateWnd_KeepsLevel) }
 if FirstWnd
 then TrackBar.Position:= Ord(DefaultVerbosity);              { I need this to synchroniz Log's and track's verbosity. Cannot be moved to CreateWnd if I use LoadForm(). Must be after TrackBar.OnChange:= TrackBarChange }

 ControlState:= ControlState- [csCreating];

 { TrackBarChange skips the log while csCreating is set, and a log attached before the window got the position of the slider without a window (0 = lvrVerbose) }
 if FLog <> NIL
 then FLog.Verbosity:= Verbosity;
end;


{ A log freed while attached must not stay in FLog: the next move of the slider would write to freed memory }
procedure TRichLogTrckbr.Notification(AComponent: TComponent; Operation: TOperation);
begin
 inherited Notification(AComponent, Operation);
 if (Operation = opRemove) AND (AComponent = FLog)
 then FLog:= NIL;
end;



procedure TRichLogTrckbr.TrackBarChange(Sender: TObject);
begin
 if NOT (csLoading in ComponentState)
 AND NOT (csCreating in ControlState) then { This is MANDATORY because when the project loads, the value of the trackbar may change BEFORE the DFM loader assigns the Log to this trackbar. In other words, crash when I load a DFM file that contains this control }
  begin
   { Log is allowed to be NIL here. setRichLog copies the current Verbosity into the log at the
     moment the log is finally attached, so the order "set Verbosity first, attach Log after" is
     one this component supports on purpose - the old Assert forbade exactly that order. And with
     assertions switched off, which is what the Release build does, the next line was an access
     violation on a NIL log rather than a message. }
   if Log <> NIL
   then Log.Verbosity:= Verbosity;
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



{ FreeNotification: a log with another owner, or with none, tells the slider when it is freed (Notification) }
procedure TRichLogTrckbr.setRichLog(CONST Value: TRichLog);
begin
 if (FLog <> NIL) AND (FLog <> Value)
 then FLog.RemoveFreeNotification(Self);

 FLog:= Value;
 if FLog <> NIL then
  begin
   FLog.FreeNotification(Self);
   FLog.Verbosity:= Verbosity;
  end;
end;







procedure Register;
begin
 RegisterComponents('LightSaber VCL', [TRichLogTrckbr]);
end;


end.
