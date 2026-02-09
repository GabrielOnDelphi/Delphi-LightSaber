UNIT LightFmx.Common.LogFilter;

{=============================================================================================================
   2026.01.31
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Verbosity filter control for TLogViewer.
   Allows users to adjust the log verbosity level via a trackbar.

   Usage:
     1. Drop TLogVerbFilter on your form
     2. Assign the Log property to connect it to a TLogViewer
     3. The trackbar will control which verbosity levels are displayed

   Tester:
     c:\Projects\LightSaber\Demo\Demo LightLog\Demo_Log.dpr
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Classes,
   FMX.Controls, FMX.StdCtrls, FMX.Types, FMX.Layouts,
   LightCore.LogTypes,
   LightFmx.Common.LogViewer;

TYPE
  TLogVerbFilter = class(FMX.Layouts.TLayout)
   private
     FLog          : TLogViewer;
     VerboLabel    : TLabel;                      // Here we display the verbosity level
     FTrackBar     : TTrackBar;
     FShowDebugMsg : Boolean;

     function  getVerbosity: TLogVerbLvl;
     procedure setVerbosity   (Value: TLogVerbLvl);
     procedure setLog         (Value: TLogViewer);
     procedure setShowDebugMsg(Value: Boolean);
   public
     constructor Create(AOwner: TComponent); override;
     procedure TrackBarChange(Sender: TObject);
  published
     property ShowDebugMsg  : Boolean       read FShowDebugMsg  write setShowDebugMsg default FALSE; { Allow the user to access the lowest (Debug) level }
     property TrackBar      : TTrackBar     read FTrackBar      write FTrackBar;
     property Verbosity     : TLogVerbLvl   read getVerbosity   write setVerbosity;      { Mirrors TrackBar.Value as TLogVerbLvl }
     property Log           : TLogViewer    read FLog           write setLog;            { The TLogViewer this filter controls }
  end;

procedure Register;

IMPLEMENTATION

USES
  LightFmx.Common.Dialogs;

{ Note: DefaultVerbosity constant is defined in LightCore.LogTypes }

constructor TLogVerbFilter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);                 { Note: Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create }
  FShowDebugMsg:= False;
  //StyleLookup := 'layoutstyle';  // FMX's built-in layout style

  VerboLabel          := TLabel.Create(Self);  { Freed by: Owner }
  VerboLabel.Parent   := Self;                 { Here we can set the parent }
  VerboLabel.Align    := TAlignLayout.Left;
  VerboLabel.Hint     := 'Log verbosity' + sLineBreak + '(Hide all messages below this level.)';
  VerboLabel.Text     := 'Verbosity: ' + Verbosity2String(DefaultVerbosity);
  VerboLabel.Locked   := TRUE;
  VerboLabel.Stored   := FALSE;
  VerboLabel.TextSettings.HorzAlign := TTextAlign.Center;
  VerboLabel.TextSettings.VertAlign := TTextAlign.Center;

  TrackBar            := TTrackBar.Create(Self);
  TrackBar.Parent     := Self;
  TrackBar.Align      := TAlignLayout.Client;
  TrackBar.Min        := 1;
  TrackBar.Max        := Ord(High(TLogVerbLvl));               { About enumerations: http://www.delphipages.com/forum/showthread.php?t=58129 }
  TrackBar.Value      := Ord(DefaultVerbosity);                { I need this to synchroniz Log's and track's verbosity. Cannot be moved to CreateWnd if I use LoadForm() }
  TrackBar.Frequency  := 1;                                    { Ensure discrete steps for verbosity levels }
  TrackBar.Hint       := 'Hide all messages below this level';
  TrackBar.Name       := 'VerbosityTrackbar';                  { This control MUST have a name so I can save it to INI file }
  TrackBar.OnChange   := TrackBarChange;
  //TrackBar.constraints.MinWidth      := 30;
  TrackBar.Locked     := TRUE;
  TrackBar.Stored     := FALSE;
  //ShowText := FALSE;                     { https://stackoverflow.com/questions/56859524/how-to-initialize-a-custom-control/64974040?noredirect=1#comment114931111_64974040 }

  // Set panel properties
  Width := 200;
  Height:= 27;
  //Initialized:= True;
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
     Log.Populate;
     VerboLabel.Text := 'Verbosity: ' + Verbosity2String(Verbosity);
   end;
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


{ Assigns the TLogViewer that this filter will control.
  Establishes a bidirectional link: this filter controls the Log's verbosity,
  and the Log knows about this filter for synchronization. }
procedure TLogVerbFilter.setLog(Value: TLogViewer);
begin
  FLog:= Value;
  if FLog <> NIL then
  begin
    Verbosity:= FLog.Verbosity;         { Sync trackbar to current Log verbosity }
    FLog.RegisterVerbFilter(Self);      { Register for bidirectional updates }
  end;
end;


{ Controls whether the Debug verbosity level (lvDebug=0) is accessible via the trackbar.
  When False, the trackbar minimum is 1 (lvVerbose), hiding the Debug level from users. }
procedure TLogVerbFilter.setShowDebugMsg(Value: Boolean);
begin
  FShowDebugMsg:= Value;

  if FShowDebugMsg
  then TrackBar.Min:= 0    { Allow access to lvDebug (ordinal 0) }
  else TrackBar.Min:= 1;   { Start at lvVerbose (ordinal 1) }
end;

{ Note: Verbosity2String is defined in LightCore.LogTypes }


procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TLogVerbFilter]);
end;


END.
