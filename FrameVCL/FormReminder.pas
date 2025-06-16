UNIT FormReminder;
{--------------------------------------------------------------------------------------------------
   REMINDER FORM
   2020-02-12
   Shows a riminder after x seconds.

   DON'T ADD IT TO ANY DPK!

   How to use it:
     frmReminder:= TfrmReminder(Self);
     frmReminder.LoadForm(Self);
     Use the OnTimeUp event if you want to execute your own stuff.

    TODO:
      There is a problem when I press Start/Stop - See the TitleBar (says stop when I press start). When I press stop is should say: Timer disabled.
      Disable all controls when "Show a reminder" is unckeched.
      Later: Add option: shutdown computer.
      Copy the tool tip from the "execute a file" radio btn also to the "file to execute groupbox".
      Bug: if I press the Apply btn the "show a reminder" timer will be reset. Get rid of that button. The effects are applied as soon as the user clicks the radiobox.
      Issue: when it shows the reminder, it only paints it on desktop.
                Maybe I should also show a non-modal window, that I bring to top. Show in it the reminder and the time when it was shown.
                Also, paint BIG, in center of the screen!
--------------------------------------------------------------------------------------------------}
INTERFACE
{$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, LightVcl.Common.AppDataForm,Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Samples.Spin, LightVcl.Visual.PathEdit, LightVcl.Visual.Timer, LightVcl.Visual.RadioButton, LightVcl.Visual.CheckBox, LightVcl.Visual.GroupBox, LightVcl.Visual.SpinEdit;

TYPE
  TfrmReminder = class(TLightForm)
    btnReset     : TButton;
    btnRun       : TButton;
    btnStart     : TButton;
    btnStop      : TButton;
    chkMakeNoise : TCubicCheckBox;
    radRunFile: TCubicRadioButton;
    edtPath      : TCubicPathEdit;
    grpExecute   : TCubicGroupBox;
    grpTimer     : TCubicGroupBox;
    Label1       : TLabel;
    lblInterval  : TLabel;
    pnlReminder  : TCubicGroupBox;
    radShutDown  : TCubicRadioButton;
    radSleep     : TCubicRadioButton;
    Timer        : TTimer;
    chkRunOnce: TCubicCheckBox;
    spnTime: TCubicSpinEdit;                                             { replace this with TCubicTimer}
    procedure btnResetClick  (Sender: TObject);
    procedure btnRunClick    (Sender: TObject);
    procedure btnStartClick  (Sender: TObject);
    procedure btnStopClick   (Sender: TObject);
    procedure radRunFileClick(Sender: TObject);
    procedure FormCreate     (Sender: TObject);
    procedure FormDestroy    (Sender: TObject);
    procedure spnTimeChange  (Sender: TObject);
    procedure TimerTimer     (Sender: TObject);
    procedure radSleepClick  (Sender: TObject);
  private
    FTimeUp: TNotifyEvent;
    FAdvance: TNotifyEvent;
    procedure TimesUp;
    procedure AdvanceReminderWith1Sec;
    procedure ShowRemainingTime;
  public
    TimeLeft: Integer;         { Number of seconds left }
    procedure Initialize(aOnAdvance, aOnTimeUp: TNotifyEvent);
  published
    property OnTimeUp : TNotifyEvent read FTimeUp  write FTimeUp;
    property OnAdvance: TNotifyEvent read FAdvance write FAdvance;
  end;

                                                  { Don't let the user choose an interval under 2s }
VAR
   frmReminder: TfrmReminder= NIL;


IMPLEMENTATION {$R *.dfm}

USES
   LightVcl.Common.VclUtils, LightVcl.Common.Colors, LightVcl.Graph.Util, LightVcl.Common.Sound, LightVcl.Graph.Desktop, LightVcl.Visual.INIFile, LightCore, LightVcl.Common.SystemTime, LightVcl.Common.Clipboard, LightVcl.Common.Dialogs, LightCore.INIFile, LightVcl.Common.AppDataForm, LightVcl.Common.PowerUtils, LightVcl.Common.ExecuteShell;






{--------------------------------------------------------------------------------------------------
   CREATE/DESTROY
--------------------------------------------------------------------------------------------------}
procedure TfrmReminder.FormCreate(Sender: TObject);
begin
 TimeLeft:= MaxInt;
end;


procedure TfrmReminder.Initialize(aOnAdvance, aOnTimeUp: TNotifyEvent);
begin
 LoadForm(Self);                               { Reminder editor }
 OnTimeUp := aOnTimeUp;
 OnAdvance:= aOnAdvance;
end;


procedure TfrmReminder.FormDestroy(Sender: TObject);
begin
 Timer.Enabled:= FALSE;
 //SaveForm(Self); called by AppData
end;











{-------------------------------------------------------------------------------
   TIMER
--------------------------------------------------------------------------------}
procedure TfrmReminder.btnRunClick(Sender: TObject);
begin
 btnResetClick(Sender);                { RESET TIMER }
 ExecuteShell(edtPath.Path);           { MUST BE shellexec so I can load mp3, txt, doc, exe, etc }
end;


procedure TfrmReminder.TimerTimer(Sender: TObject);
begin
 AdvanceReminderWith1Sec;
end;


procedure TfrmReminder.TimesUp;
begin
 if chkRunOnce.Checked                  { Don't run the file multiple times }
 or RadSleep.Checked                    { Don't enter sleep twice! }
 then Timer.Enabled:= FALSE;

 if chkMakeNoise.Checked
 then BipCoconuts;

 if Assigned(FTimeUp)
 then FTimeUp(Self);

 TimeLeft:= spnTime.Value * 60;
 LightVcl.Graph.Desktop.WriteTextOnDesktopOver(10, 10, 'Reminder!', 'Arial', 30, clRedBrick);  { Paint text over all windows }

 if radRunFile.Checked then
   if (edtPath.Path > '')
   then
    begin
     Timer.Enabled:= FALSE;
     if NOT ExecuteShell(edtPath.Path)              { MUST BE shellexec so I can load mp3, txt, doc, exe, etc }
     then BipError;
    end
   else MessageError('[Reminder times up] CRLF No file to execute!');

 if radSleep.Checked    then LightVcl.Common.PowerUtils.SystemSleep;
 if radShutDown.Checked then LightVcl.Common.PowerUtils.WinShutDown(TRUE, FALSE);
end;







{--------------------------------------------------------------------------------------------------
   UTILS
--------------------------------------------------------------------------------------------------}
procedure TfrmReminder.AdvanceReminderWith1Sec;
begin
 Dec(TimeLeft);

 if TimeLeft <= 0 then
  begin
   TimeLeft:= spnTime.Value * 60;
   TimesUp;
  end;

 if Timer.Enabled
 then spnTime.Color:= $00C6E7C6                                                 { Verde }
 else spnTime.Color:= clDkGray;

 ShowRemainingTime;

 if Assigned(FAdvance)
 then FAdvance(Self);
end;



procedure TfrmReminder.ShowRemainingTime;
begin
 if Timer.Enabled
 then Caption:= 'Reminder in '+ SecondsToTimeAuto(TimeLeft)
 else Caption:= 'Timer disabled!';
end;





{--------------------------------------------------------------------------------------------------
   SETTINGS
--------------------------------------------------------------------------------------------------}
procedure TfrmReminder.spnTimeChange(Sender: TObject);
begin
 TimeLeft:= spnTime.Value* 60;
 LightVcl.Visual.Timer.ResetTimer(Timer);
 ShowRemainingTime;
end;


procedure TfrmReminder.radRunFileClick(Sender: TObject);
begin
 EnableDisable(grpExecute, radRunFile.Checked);
end;


procedure TfrmReminder.radSleepClick(Sender: TObject);
begin
 EnableDisable(grpExecute, radRunFile.Checked);
end;


{ START/STOP TIMER}
procedure TfrmReminder.btnStartClick(Sender: TObject);
begin
 Timer.Enabled:= TRUE;
 spnTimeChange(Sender);
end;


procedure TfrmReminder.btnStopClick(Sender: TObject);
begin
 Timer.Enabled:= FALSE;
 ShowRemainingTime;
end;


procedure TfrmReminder.btnResetClick(Sender: TObject);
begin
 spnTimeChange(Sender);
end;






end.
