UNIT LightFmx.lbLogFilter;

// NEW LOG based on TStringGrid

{=============================================================================================================
   2024.05
   www.GabrielMoraru.com

==============================================================================================================
   For the new log (the one based on TStringGrid)
     Min= ?,      lvVerbose
     Max= ?       lvErrors

   Tester:
     c:\Projects\LightSaber\Demo\Demo LightLog\Demo_Log.dpr
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Classes,
   FMX.Controls, FMX.StdCtrls, FMX.Types,
   LightFmx.lbLogViewer, ccLogTypes;

TYPE
  TLogVerbFilter = class(FMX.StdCtrls.TPanel)//could be TLayout
   private
     FLog          : TLogViewer;
     VerboLabel    : TLabel;
     FTrackBar     : TTrackBar;
     Initialized   : Boolean;
     FShowDebugMsg : Boolean;

     function  getVerbosity: TLogVerbLvl;            //Note: The "master" of the verbosity events is the Grid not the trackbar
     procedure setVerbosity   (Value: TLogVerbLvl);
     procedure setLog         (Value: TLogViewer);
     procedure setShowDebugMsg(Value: Boolean);
   public
     constructor Create(AOwner: TComponent); override;
     procedure TrackBarChange(Sender: TObject);
  published
     property ShowDebugMsg  : Boolean       read FShowDebugMsg  write setShowDebugMsg default FALSE; { Allow the user to access the lowest (Debug) level }
     property TrackBar      : TTrackBar     read FTrackBar      write FTrackBar;
     property Verbosity     : TLogVerbLvl   read getVerbosity   write setVerbosity;      //Note: The "master" of the verbosity events is the Grid not the trackbar
     property Log           : TLogViewer    read FLog           write setLog;
  end;

procedure Register;

IMPLEMENTATION

Uses LightFMX.lbDialogs;

const
  DefaultVerbosity = lvInfos;

constructor TLogVerbFilter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);                 { Note: Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create }
  FShowDebugMsg:= False;
  Initialized := False;

  VerboLabel       := TLabel.Create(Self);  { Freed by: Owner }
  VerboLabel.Parent:= Self;                 { Here we can set the parent }
  VerboLabel.Align := TAlignLayout.Left;
  VerboLabel.TextSettings.HorzAlign := TTextAlign.Center;
  VerboLabel.TextSettings.VertAlign := TTextAlign.Center;
  VerboLabel.Hint  := 'Log verbosity' + sLineBreak + '(Hide all messages below this level.)';
  VerboLabel.Text  := 'Verbosity: ' + Verbosity2String(DefaultVerbosity);

  TrackBar := TTrackBar.Create(Self);
  TrackBar.Parent := Self;

   //ShowText := FALSE;                     { https://stackoverflow.com/questions/56859524/how-to-initialize-a-custom-control/64974040?noredirect=1#comment114931111_64974040 }

  TrackBar.Align      := TAlignLayout.Right;
  TrackBar.Min        := 1;
  TrackBar.Max        := Ord(High(TLogVerbLvl));               { About enumerations: http://www.delphipages.com/forum/showthread.php?t=58129 }
  TrackBar.Value      := Ord(DefaultVerbosity);                { I need this to synchroniz Log's and track's verbosity. Cannot be moved to CreateWnd if I use LoadForm() }
  TrackBar.Frequency  := 1;                                    { Ensure discrete steps for verbosity levels }
  TrackBar.Hint       := 'Hide all messages below this level';
  TrackBar.Name       := 'VerbosityTrackbar';                  { This control MUST have a name so I can save it to INI file }
  TrackBar.OnChange   := TrackBarChange;
  TrackBar.Width      := 120;

  // Set panel properties
  Width := 260;
  Height := 27;
  Initialized := True;
end;




procedure TLogVerbFilter.TrackBarChange(Sender: TObject);
begin
 if NOT (csLoading in ComponentState) then { This is MANDATORY because when the project loads, the value of the trackbar may change BEFORE the DFM loader assigns the Log to this trackbar. In other words, crash when I load a DFM file that contains this control }
   begin
     if Log = NIL then
       begin
         MessageError('No log assigned!');
         EXIT;
       end;

     Log.Verbosity:= Verbosity;
     VerboLabel.Text := 'Verbosity: ' + Verbosity2String(Verbosity);
   end;

 Log.Populate;
end;





{ Returns the position of the trackbar as TLogVerbLvl (instead of integer) }
function TLogVerbFilter.getVerbosity: TLogVerbLvl;
begin
  Result := TLogVerbLvl(Round(TrackBar.Value));
end;


procedure TLogVerbFilter.setVerbosity(Value: TLogVerbLvl);
begin
  TrackBar.Value := Ord(Value);
end;


procedure TLogVerbFilter.setLog(Value: TLogViewer);
begin
  FLog := Value;
  if FLog <> nil then
  begin
    Verbosity := FLog.Verbosity;
    FLog.RegisterVerbFilter(Self);      // Let the Log know that its verbosity is controlled by this TrackBar
  end;
end;


procedure TLogVerbFilter.setShowDebugMsg(Value: Boolean);
begin
  FShowDebugMsg := Value;

  if FShowDebugMsg
  then TrackBar.Min:= 0
  else TrackBar.Min:= 1;
end;


function Verbosity2String(Verbosity: TLogVerbLvl): string;
begin
  case Verbosity of
    lvDebug     : Result := 'Debug';
    lvVerbose   : Result := 'Verbose';
    lvHints     : Result := 'Hints';
    lvInfos     : Result := 'Info';      { This is the default level of verbosity }
    lvImportant : Result := 'Important';
    lvWarnings  : Result := 'Warnings';
    lvErrors    : Result := 'Errors';
  else
    raise Exception.Create('Invalid verbosity');
  end;
end;


procedure Register;
begin
  RegisterComponents('LightSaber', [TLogVerbFilter]);
end;


END.
