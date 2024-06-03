UNIT clVisLogTrack;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

   WARNING!
   THIS UNIT IS STILL UNDER CONSTRUCTION.
   For the moment the library uses the old ccRichLog unit.


==============================================================================================================
   For the new log (the one based on TStringGrid)
     Min= ?,      lvVerbose
     Max= ?       lvErrors

   Tester:
     c:\Myprojects\Packages\LightSaber\Demo\LightLog\
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Classes, Vcl.Controls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls,
   clVisLog, clVisLogUtils;

TYPE
  TLogVisTrckbr = class(TPanel)
   private
     FLog: TVisLog;
     VerboLabel: TLabel;
     FTrackBar: TTrackBar;
     Initialized : Boolean;
     FShowDebugMsg: Boolean;
     FVerbChanged: TNotifyEvent;
     function  getVerbosity: TLogVerbLvl;
     procedure setVerbosity(Value: TLogVerbLvl);
     procedure setLog(const Value: TVisLog);
     procedure setShowDebugMsg(const Value: Boolean);
   protected
     procedure CreateWnd; override;
     procedure TrackBarChange(Sender: TObject);
   public
     constructor Create(AOwner: TComponent); override;
  published
     property ShowDebugMsg  : Boolean       read FShowDebugMsg  write setShowDebugMsg default FALSE; { Allow the user to access the lowest (Debug) level }
     property TrackBar      : TTrackBar     read FTrackBar      write FTrackBar;
     property OnVerbChanged : TNotifyEvent  read FVerbChanged   write FVerbChanged;   { Triggered before deleting the content of a cell }
     property Verbosity     : TLogVerbLvl      read getVerbosity   write setVerbosity;
     property Log           : TVisLog       read FLog           write setLog;
  end;

procedure Register;

IMPLEMENTATION
Uses ccCore;




constructor TLogVisTrckbr.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);             { Note: Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create }
 FShowDebugMsg:= False;

 VerboLabel:= TLabel.Create(Self);     { Freed by: Owner }
 VerboLabel.Parent:= Self;             { Here we can set the parent }

 TrackBar:= TTrackBar.Create(Self);
 TrackBar.SetSubComponent(True);
 TrackBar.Parent:= Self;
end;


//CreateWnd can be called more than once: http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Controls_TWinControl_CreateWnd.html
procedure TLogVisTrckbr.CreateWnd;
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

   TrackBar.Min        := 1;
   TrackBar.Max        := Integer(High(TLogVerbLvl));                  { About enumerations: http://www.delphipages.com/forum/showthread.php?t=58129 }
   TrackBar.Position   := Ord(DefaultVerbosity);                { I need this to synchroniz Log's and track's verbosity. Cannot be moved to CreateWnd if I use LoadForm() }
   TrackBar.Hint       := 'Hide all messages below this level';
   TrackBar.Align      := alRight;
   TrackBar.Name       := 'VerbosityTrackbar';                  { This control MUST have a name so I can save it to INI file }
   TrackBar.OnChange   := TrackBarChange;
  end;
end;




procedure TLogVisTrckbr.TrackBarChange(Sender: TObject);
begin
 if NOT (csLoading in ComponentState) then { This is MANDATORY because when the project loads, the value of the trackbar may change BEFORE the DFM loader assigns the Log to this trackbar. In other words, crash when I load a DFM file that contains this control }
  begin
   if Log = NIL
   then MesajError('No log assigned!')
   else Log.Verbosity:= Verbosity;

   VerboLabel.Caption:= 'Log verbosity: '+ Verbosity2String(Verbosity);

   if Assigned(FVerbChanged)
   then FVerbChanged(Self);                                                                  { Let GUI know that the user changed the verbosity }
  end;

 //Log.InvalidateGrid;  put it back
end;





{ Returns the position of the trackbar as TLogVerbLvl (instead of integer) }
function TLogVisTrckbr.getVerbosity: TLogVerbLvl;
begin
 Result:= TLogVerbLvl(TrackBar.Position);
end;


procedure TLogVisTrckbr.setVerbosity(Value: TLogVerbLvl);
begin
 TrackBar.Position:= Ord(Value);
end;



procedure TLogVisTrckbr.setLog(CONST Value: TVisLog);
begin
 FLog:= Value;
 FLog.Verbosity:= Verbosity;
end;


procedure TLogVisTrckbr.setShowDebugMsg(const Value: Boolean);
begin
 FShowDebugMsg := Value;

 if FShowDebugMsg
 then TrackBar.Min:= 0
 else TrackBar.Min:= 1;
end;









procedure Register;
begin
  RegisterComponents('LightSaber', [TLogVisTrckbr]);
end;


end.
