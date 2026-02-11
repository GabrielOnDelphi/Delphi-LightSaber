UNIT FormPower;

{=============================================================================================================
   2026.01.29
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   POWER MANAGEMENT FORM

   Collects and displays:
     - CPU utilization data (average usage percentage)
     - Battery status (AC/battery power, percentage remaining)

   The check is performed automatically every 1 second via TimerPwr.

   OkToChangeWallpaper:
     Utility function that returns True if:
       - CPU usage is below the configured threshold
       - Power type is not battery (when battery check is enabled)

   USAGE:
     var PowerSettings: RPowerSettings;
     PowerSettings.Reset;
     frmPower:= TfrmPower.Create(Application);
     frmPower.Initialize(SomeParentPanel, @PowerSettings);  // REQUIRED - reparents Container and starts timer

   DEPENDENCIES:
     - CpuUsageTotal unit for AverageCpuUsage function
     - LightVcl.Common.PowerUtils for battery/power status

   NOTE:
     The Container panel is reparented to the specified parent control during Initialize,
     and moved back to Self during FormDestroy to ensure proper INI file saving.
=============================================================================================================}

INTERFACE
{$DENYPACKAGEUNIT ON}  // Prevents unit from being placed in a package

USES
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.StdCtrls, LightVcl.Visual.SpinEdit, Vcl.ExtCtrls,
  LightVcl.Visual.CheckBox, LightVcl.Visual.GroupBox,
  LightVcl.Visual.AppDataForm, LightVcl.Common.PowerUtils,
  Vcl.ComCtrls;

TYPE
  TfrmPower = class(TLightForm)
    chkBatteries  : TCubicCheckBox;   // Check to prevent wallpaper changes on battery
    chkOutOfJuice : TCubicCheckBox;   // Check to show power change notifications
    grpPowerInfo  : TCubicGroupBox;   // Group box for power information display
    grpPowerOpt   : TCubicGroupBox;   // Group box for power options
    lblBatProc    : TLabel;           // Shows battery percentage remaining
    lblPwrType    : TLabel;           // Shows current power type (AC/Battery)
    Container     : TPanel;           // Main container - reparented during Initialize
    TimerPwr      : TTimer;           // Timer for periodic CPU/power checks (1 second)
    pnlCPU        : TPanel;           // Panel containing CPU usage controls
    lblCPU        : TLabel;           // Label for CPU usage display
    spnMaxCPU     : TCubicSpinEditSplit;  // Maximum CPU threshold for wallpaper changes
    proCpu        : TProgressBar;     // Visual display of CPU usage
    procedure FormDestroy(Sender: TObject);
    procedure TimerPwrTimer(Sender: TObject);
    procedure spnMaxCPUChange(Sender: TObject);
  private
    Settings: PPowerSettings;
    Initializing: Boolean;            // Prevent OnChange events during initialization
    FLastPowerType: TPowerType;       // Tracks power type for change detection
    procedure CheckSupplyTypeChanged;
    procedure ShowCpuUtilization;
    procedure PaintPowerTypeOnDesktop(const Msg: string);
    procedure SettingChanged(Sender: TObject);
    procedure ObjectFromGUI;
  public
    procedure GuiFromObject;
    function  OkToChangeWallpaper: Boolean;
    procedure Initialize(Parent: TWinControl; aSettings: PPowerSettings);
  end;

x
VAR
  { Global form reference - used for singleton access pattern.
    Consider using AppData.GetForm<TfrmPower> instead for better encapsulation. }
  frmPower: TfrmPower = NIL;


IMPLEMENTATION {$R *.dfm}

USES
  CpuUsageTotal,
  LightCore, LightCore.Time, LightCore.Types, LightCore.INIFile, LightCore.AppData,
  LightVcl.Visual.AppData, LightVcl.Visual.INIFile,
  LightVcl.Common.SystemTime, LightVcl.Common.Clipboard, LightVcl.Common.Dialogs,
  LightVcl.Common.Sound;



{--------------------------------------------------------------------------------------------------
   INITIALIZATION
--------------------------------------------------------------------------------------------------}

{ Initializes the form by reparenting the Container to the specified parent control.
  IMPORTANT: Must be called after form creation before the form is used.

  Parameters:
    Parent - The control that will become the new parent of Container.
             Must not be nil.

  The Container is reparented to allow embedding this form's controls
  into another form or panel. During FormDestroy, Container is moved back
  to Self to ensure proper INI file saving of child controls. }
procedure TfrmPower.Initialize(Parent: TWinControl; aSettings: PPowerSettings);
begin
  Assert(Parent <> NIL, 'TfrmPower.Initialize: Parent cannot be nil');
  Assert(aSettings <> NIL, 'TfrmPower.Initialize: aSettings cannot be nil');

  Settings:= aSettings;
  FLastPowerType:= pwUnknown;  // Initialize power type tracking

  Initializing:= TRUE;
  try
    LightVcl.Visual.INIFile.LoadForm(Self);  // Load INI settings before reparenting
    GuiFromObject;
  finally
    Initializing:= FALSE;
  end;

  { Wire settings controls to sync record on change }
  chkBatteries.OnClick:= SettingChanged;
  chkOutOfJuice.OnClick:= SettingChanged;

  Container.Parent:= Parent;
  TimerPwr.Enabled:= TRUE;  // Start collecting CPU usage data

  Assert(WindowState = wsNormal, 'TfrmPower.WindowState <> wsNormal');
end;


{ Cleans up the form and ensures Container is properly parented for INI saving.
  The Container is moved back to Self so that SaveForm (called by AppData)
  can correctly enumerate and save all child control settings. }
procedure TfrmPower.FormDestroy(Sender: TObject);
begin
  TimerPwr.Enabled:= FALSE;

  // Don't save if application startup was improper
 if AppData.Initializing then EXIT;  { We don't save anything if the start up was improper! }

  Assert(ComponentCount > 5, 'TfrmPower.Container already freed?');

  // Final sync from GUI to the settings object
  if Settings <> NIL
  then ObjectFromGUI;

  // Move Container back for proper INI saving
  Container.Parent:= Self;
end;



{--------------------------------------------------------------------------------------------------
   GUI <-> OBJECT TRANSFER
--------------------------------------------------------------------------------------------------}

procedure TfrmPower.GuiFromObject;
begin
  Assert(Settings <> NIL);
  chkBatteries.Checked := Settings.CheckBatteries;
  chkOutOfJuice.Checked:= Settings.NotifyPowerChange;
  spnMaxCPU.Value      := Settings.MaxCPU;
end;


procedure TfrmPower.ObjectFromGUI;
begin
  Assert(Settings <> NIL);
  Settings.CheckBatteries   := chkBatteries.Checked;
  Settings.NotifyPowerChange:= chkOutOfJuice.Checked;
  Settings.MaxCPU           := spnMaxCPU.Value;
end;


procedure TfrmPower.SettingChanged(Sender: TObject);
begin
  if NOT Initializing then ObjectFromGUI;
end;



{--------------------------------------------------------------------------------------------------
   TIMER - CPU AND BATTERY MONITORING
--------------------------------------------------------------------------------------------------}

{ Timer event handler - called every second to update power and CPU information.
  Only updates the display if the Container's parent is visible. }
procedure TfrmPower.TimerPwrTimer(Sender: TObject);
begin
  // Check for power type changes (AC <-> Battery)
  CheckSupplyTypeChanged;

 { Only update GUI if the parent panel is visible }
  if (Container.Parent <> NIL) 
  AND Container.Parent.Visible then
    begin
      ShowCpuUtilization;
      lblPwrType.Caption:= LightVcl.Common.PowerUtils.PowerStatusString;

      if LightVcl.Common.PowerUtils.BatteryLeft <= 100
      then lblBatProc.Caption:= ' Battery left: ' + IntToStr(BatteryLeft) + '%'
      else lblBatProc.Caption:= 'No battery detected.';
    end;
end;


{ Updates the CPU usage progress bar with the current average CPU usage.
  Handles the case where AverageCpuUsage returns -1 (error obtaining data). }
procedure TfrmPower.ShowCpuUtilization;
var TotalUsage: Integer;
begin
  TotalUsage:= AverageCpuUsage;  // Get the average CPU usage

  // -1 indicates an error obtaining CPU data
  if TotalUsage > -1
  then proCpu.Position:= TotalUsage;
end;


{ Event handler for spin edit value changes. Syncs to Settings object. }
procedure TfrmPower.spnMaxCPUChange(Sender: TObject);
begin
  SettingChanged(Sender);
end;



{--------------------------------------------------------------------------------------------------
   POWER TYPE CHANGE DETECTION
--------------------------------------------------------------------------------------------------}

{ Checks if the power supply type has changed (AC <-> Battery) and notifies the user.
  Paints a message on the desktop and plays alert sounds when switching to battery. }
procedure TfrmPower.CheckSupplyTypeChanged;
var CurrentPower: TPowerType;
begin
  Assert(Settings <> NIL);
  if NOT Settings.NotifyPowerChange then EXIT;

  CurrentPower:= LightVcl.Common.PowerUtils.PowerStatus;

  if CurrentPower <> FLastPowerType then
    case CurrentPower of
      pwTypeBat:
        begin
          PaintPowerTypeOnDesktop('Powered from batteries.');
          BipError;
          Bip300;
          BipError;
        end;
      pwTypeAC:
        PaintPowerTypeOnDesktop('Powered from grid.');
      pwUnknown:
        PaintPowerTypeOnDesktop('Power supply status unavailable!');
    end;

  FLastPowerType:= CurrentPower;
end;


{ Paints a power status message directly on the desktop.
  Uses the desktop device context to draw text at the top-left corner.

  Parameters:
    Msg - The message to display on the desktop.

  Note: The message will be overwritten by Windows when the desktop refreshes. }
procedure TfrmPower.PaintPowerTypeOnDesktop(const Msg: string);
var
  Canv: TCanvas;
  DC: HDC;
begin
  DC:= GetWindowDC(0);  // Get desktop DC
  if DC = 0
  then EXIT;

  Canv:= TCanvas.Create;
  try
    Canv.Handle:= DC;
    Canv.Font.Size:= 32;
    Canv.TextOut(3, 3, Msg);
  finally
    Canv.Handle:= 0;  // Detach before freeing
    FreeAndNil(Canv);
    ReleaseDC(0, DC);  // Release the desktop DC
  end;
end;



{--------------------------------------------------------------------------------------------------
   WALLPAPER CHANGE DECISION
--------------------------------------------------------------------------------------------------}

{ Determines if it's appropriate to change the wallpaper based on power and CPU status.

  Returns True if:
    - Computer is not running on batteries (or battery check is disabled)
    - CPU usage is below the configured maximum threshold

  Logs warnings when wallpaper change is blocked due to battery or CPU conditions. }
function TfrmPower.OkToChangeWallpaper: Boolean;
var
  CpuUsage: Integer;
begin
  Assert(Settings <> NIL);
  CpuUsage:= AverageCpuUsage;
  Result:= Settings.OkToChangeWallpaper(CpuUsage);

  if NOT Result then
    begin
      if Settings.CheckBatteries AND (PowerStatus <= pwTypeBat)
      then AppData.LogWarn('Wallpaper not changed because the computer is running on batteries.')
      else AppData.LogWarn('Wallpaper not changed because the computer is too busy (CPU: ' +
        IntToStr(CpuUsage) + '%). You can change this behaviour in ''Settings''');
    end;
end;


end.
