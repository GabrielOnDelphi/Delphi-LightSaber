UNIT FormReminder;
{=============================================================================================================
   2020-02-12
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   REMINDER FORM

   Shows a reminder after a specified number of minutes.
   Can optionally execute a file, put the system to sleep, or shut down the computer.

   DON'T ADD IT TO ANY DPK!

   USAGE:
     frmReminder:= TfrmReminder.Create(Self);
     frmReminder.Initialize(OnAdvanceHandler, OnTimeUpHandler);
     frmReminder.Show;

   FEATURES:
     - Countdown timer with visual feedback
     - Optional sound notification when time is up
     - Execute file/program when timer expires
     - Put system to sleep or shutdown
     - Run once or repeat mode
     - OnTimeUp event for custom actions
     - OnAdvance event called every second

   TODO:
     - Fix Start/Stop button caption sync issue
     - Disable all controls when "Show a reminder" is unchecked
     - Add option: shutdown computer with delay
     - Show reminder in a non-modal window instead of just painting on desktop
=============================================================================================================}

INTERFACE
{$DENYPACKAGEUNIT ON}  // Prevents unit from being placed in a package

USES
  System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Samples.Spin,
  LightVcl.Visual.AppDataForm, LightVcl.Visual.PathEdit, LightVcl.Visual.Timer,
  LightVcl.Visual.RadioButton, LightVcl.Visual.CheckBox, LightVcl.Visual.GroupBox,
  LightVcl.Visual.SpinEdit;

CONST
  { Color for active timer state (light green) }
  TIMER_ACTIVE_COLOR = $00C6E7C6;

  { Desktop reminder text settings }
  REMINDER_TEXT_X = 10;
  REMINDER_TEXT_Y = 10;
  REMINDER_FONT_NAME = 'Arial';
  REMINDER_FONT_SIZE = 30;

TYPE
  TfrmReminder = class(TLightForm)
    btnReset     : TButton;            // Reset timer to initial value
    btnRun       : TButton;            // Manually run the configured file
    btnStart     : TButton;            // Start the timer
    btnStop      : TButton;            // Stop/pause the timer
    chkMakeNoise : TCubicCheckBox;     // Play sound when time is up
    chkRunOnce   : TCubicCheckBox;     // Run action only once (don't repeat)
    radRunFile   : TCubicRadioButton;  // Execute file when time is up
    radShutDown  : TCubicRadioButton;  // Shutdown computer when time is up
    radSleep     : TCubicRadioButton;  // Put system to sleep when time is up
    edtPath      : TCubicPathEdit;     // Path to file to execute
    grpExecute   : TCubicGroupBox;     // Group for file execution settings
    grpTimer     : TCubicGroupBox;     // Group for timer settings
    pnlReminder  : TCubicGroupBox;     // Main reminder panel
    lblInterval  : TLabel;             // Label for interval setting
    Label1       : TLabel;             // Additional label
    Timer        : TTimer;             // Main countdown timer (1 second interval)
    spnTime      : TCubicSpinEdit;     // Timer interval in minutes
    procedure btnResetClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure radRunFileClick(Sender: TObject);
    procedure radSleepClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure spnTimeChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FTimeUp: TNotifyEvent;
    FAdvance: TNotifyEvent;
    FTimeLeft: Integer;               // Number of seconds remaining
    procedure TimesUp;
    procedure AdvanceTimer;
    procedure UpdateDisplay;
    procedure ExecuteTimerAction;
  public
    procedure Initialize(AOnAdvance, AOnTimeUp: TNotifyEvent);

    { Number of seconds left until reminder fires }
    property TimeLeft: Integer read FTimeLeft write FTimeLeft;

    { Fired when the timer reaches zero }
    property OnTimeUp: TNotifyEvent read FTimeUp write FTimeUp;

    { Fired every second while timer is running }
    property OnAdvance: TNotifyEvent read FAdvance write FAdvance;
  end;

VAR
  { Global form reference - used for singleton access pattern.
    Consider using AppData.GetForm<TfrmReminder> instead for better encapsulation. }
  frmReminder: TfrmReminder = NIL;


IMPLEMENTATION {$R *.dfm}

USES
  LightVcl.Common.VclUtils,
  LightVcl.Common.Colors,
  LightVcl.Common.Sound,
  LightVcl.Common.PowerUtils,
  LightVcl.Common.ExecuteShell,
  LightVcl.Common.Dialogs,
  LightVcl.Graph.Desktop,
  LightVcl.Visual.INIFile,
  LightCore.Time;



{--------------------------------------------------------------------------------------------------
   FORM LIFECYCLE
--------------------------------------------------------------------------------------------------}

{ Initializes the form with default values }
procedure TfrmReminder.FormCreate(Sender: TObject);
begin
  FTimeLeft:= MaxInt;  // Will be set properly when timer starts
end;


{ Initializes the form with event handlers.

  Parameters:
    AOnAdvance - Called every second while timer is running (can be nil)
    AOnTimeUp  - Called when timer reaches zero (can be nil) }
procedure TfrmReminder.Initialize(AOnAdvance, AOnTimeUp: TNotifyEvent);
begin
  FTimeUp:= AOnTimeUp;
  FAdvance:= AOnAdvance;
end;


{ Cleanup when form is destroyed }
procedure TfrmReminder.FormDestroy(Sender: TObject);
begin
  Timer.Enabled:= FALSE;
  // Note: SaveForm is called automatically by AppData
end;



{--------------------------------------------------------------------------------------------------
   TIMER LOGIC
--------------------------------------------------------------------------------------------------}

{ Timer event - called every second }
procedure TfrmReminder.TimerTimer(Sender: TObject);
begin
  AdvanceTimer;
end;


{ Decrements the timer and checks if time is up }
procedure TfrmReminder.AdvanceTimer;
begin
  Dec(FTimeLeft);

  if FTimeLeft <= 0 then
    begin
      FTimeLeft:= spnTime.Value * 60;  // Reset for next cycle
      TimesUp;
    end;

  // Update visual feedback
  if Timer.Enabled
  then spnTime.Color:= TIMER_ACTIVE_COLOR
  else spnTime.Color:= clDkGray;

  UpdateDisplay;

  // Fire advance event
  if Assigned(FAdvance)
  then FAdvance(Self);
end;


{ Called when the timer reaches zero - executes the configured action }
procedure TfrmReminder.TimesUp;
begin
  // Stop timer if configured to run only once or if sleep/shutdown selected
  if chkRunOnce.Checked OR radSleep.Checked
  then Timer.Enabled:= FALSE;

  // Play notification sound if enabled
  if chkMakeNoise.Checked
  then BipCoconuts;

  // Fire the OnTimeUp event
  if Assigned(FTimeUp)
  then FTimeUp(Self);

  // Reset time for next cycle
  FTimeLeft:= spnTime.Value * 60;

  // Show reminder on desktop
  LightVcl.Graph.Desktop.WriteTextOnDesktopOver(
    REMINDER_TEXT_X,
    REMINDER_TEXT_Y,
    'Reminder!',
    REMINDER_FONT_NAME,
    REMINDER_FONT_SIZE,
    clRedBrick);

  // Execute the configured action
  ExecuteTimerAction;
end;


{ Executes the action selected by the user (run file, sleep, or shutdown) }
procedure TfrmReminder.ExecuteTimerAction;
begin
  if radRunFile.Checked then
    begin
      if edtPath.Path <> '' then
        begin
          Timer.Enabled:= FALSE;
          if NOT ExecuteShell(edtPath.Path)
          then BipError;
        end
      else
        MessageError('Reminder time is up!' + sLineBreak + 'No file to execute!');
    end
  else 
  if radSleep.Checked then LightVcl.Common.PowerUtils.SystemSleep
  else 
  if radShutDown.Checked then LightVcl.Common.PowerUtils.WinShutDown(TRUE, FALSE);
end;


{ Updates the form caption to show remaining time }
procedure TfrmReminder.UpdateDisplay;
begin
  if Timer.Enabled
  then Caption:= 'Reminder in ' + ShowTimeNice(FTimeLeft)
  else Caption:= 'Timer disabled!';
end;



{--------------------------------------------------------------------------------------------------
   BUTTON HANDLERS
--------------------------------------------------------------------------------------------------}

{ Starts the countdown timer }
procedure TfrmReminder.btnStartClick(Sender: TObject);
begin
  Timer.Enabled:= TRUE;
  spnTimeChange(Sender);
end;


{ Stops/pauses the countdown timer }
procedure TfrmReminder.btnStopClick(Sender: TObject);
begin
  Timer.Enabled:= FALSE;
  UpdateDisplay;
end;


{ Resets the timer to the configured interval }
procedure TfrmReminder.btnResetClick(Sender: TObject);
begin
  spnTimeChange(Sender);
end;


{ Manually runs the configured file without waiting for timer }
procedure TfrmReminder.btnRunClick(Sender: TObject);
begin
  btnResetClick(Sender);  // Reset timer first

  if edtPath.Path <> ''
  then ExecuteShell(edtPath.Path)
  else MessageWarning('No file path specified.');
end;



{--------------------------------------------------------------------------------------------------
   SETTINGS HANDLERS
--------------------------------------------------------------------------------------------------}

{ Called when the timer interval is changed }
procedure TfrmReminder.spnTimeChange(Sender: TObject);
begin
  FTimeLeft:= spnTime.Value * 60;  // Convert minutes to seconds
  LightVcl.Visual.Timer.ResetTimer(Timer);
  UpdateDisplay;
end;


{ Updates UI when "Run File" option is selected }
procedure TfrmReminder.radRunFileClick(Sender: TObject);
begin
  EnableDisable(grpExecute, radRunFile.Checked);
end;


{ Updates UI when "Sleep" option is selected }
procedure TfrmReminder.radSleepClick(Sender: TObject);
begin
  EnableDisable(grpExecute, radRunFile.Checked);
end;


end.
