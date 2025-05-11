UNIT FormPower;

{-------------------------------------------------------------------------------------------------------------
 2020-01-18
 COLLECT AND SHOW:
     CPU UTILIZATION DATA
     BATTERY STATUS

 The check is done automatically every 1 second in the Timer

 PowerOkToChangeWallpaper:
     Utility function that returns True if the CPU is noy bussy AND the power type is not battery
-------------------------------------------------------------------------------------------------------------}

INTERFACE
{$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.StdCtrls, cvSpinEdit, {ProgressBarAL, }Vcl.ExtCtrls, cvCheckBox, cvGroupBox,
  Vcl.ComCtrls;

TYPE
  TfrmPower = class(TLightForm)
    chkBatteries  : TCubicCheckBox;
    chkOutOfJuice : TCubicCheckBox;
    grpPowerInfo  : TCubicGroupBox;
    grpPowerOpt   : TCubicGroupBox;
    lblBatProc    : TLabel;
    lblPwrType    : TLabel;
    Container     : TPanel;
    TimerPwr      : TTimer;
    pnlCPU        : TPanel;
    lblCPU        : TLabel;
    spnMaxCPU: TCubicSpinEditSplit;
    proCpu: TProgressBar;
    procedure FormDestroy(Sender: TObject);
    procedure TimerPwrTimer (Sender: TObject);
    procedure spnMaxCPUChange(Sender: TObject);
  private
    procedure CheckSupplyTypeChanged;
    procedure ShowCpuUtilization;
  public
    function  OkToChangeWallpaper: Boolean;
    procedure Initialize(Parent: TWinControl);
  end;

VAR
  frmPower: TfrmPower= NIL;


IMPLEMENTATION {$R *.dfm}
USES
   CpuUsageTotal, ccAppData, LightCom.AppData
, cvINIFile, LightCom.PowerUtils, ccCore, LightCom.SystemTime, LightCom.Clipboard, LightCom.Dialogs, ccINIFile, LightCom.AppDataForm, FormLog;







procedure TfrmPower.Initialize(Parent: TWinControl);
begin
 LoadForm(Self);   { We need to have the container parented in its original form, in order to let that form to correctly load its children }
 Container.Parent:= Parent;
 TimerPwr.Enabled:= TRUE;            { Start collecting CPU usage data }
 Assert(WindowState = wsNormal, 'TfrmPower.WindowState <> wsNormal');
end;


procedure TfrmPower.FormDestroy(Sender: TObject);
begin
 TimerPwr.Enabled:= FALSE;
 if AppData.Initializing then EXIT;  { We don't save anything if the start up was improper! }

 Assert(ComponentCount > 5, 'TfrmTimer.Container already nilled?');
 Container.Parent:= Self;    { We need to move the container back on its original form, in order to let that form to correctly save its children }
 //SaveForm(Self); called by AppData
end;








{--------------------------------------------------------------------------------------------------
   SHOW BATTERY/CPU
--------------------------------------------------------------------------------------------------}
procedure TfrmPower.TimerPwrTimer(Sender: TObject);
begin
 CheckSupplyTypeChanged;                                   { Paint on desktop announce that power type has changed (the laptop switched from AC to battery, for example) }

 if (Container.Parent <> NIL)
 AND Container.Parent.Visible then

  begin
   ShowCpuUtilization;                                     { Show the average CPU usage }
   lblPwrType.Caption:= LightCom.PowerUtils.PowerStatusString;

   if LightCom.PowerUtils.BatteryLeft<= 100
   then lblBatProc.Caption:= ' Battery left: '+ IntToStr(BatteryLeft)+ '%'
   else lblBatProc.Caption:= 'No battery detected.';
  end;
end;


procedure TfrmPower.ShowCpuUtilization;
VAR TotalUsage: Integer;  { Sometimes I get -1 values }
begin
 TotalUsage:= AverageCpuUsage;            { Get the average CPU usage }
 if TotalUsage > -1                       { if -1 then means I had problems obtaining the CPU data }
 then proCpu.Position:= TotalUsage;
end;












procedure TfrmPower.spnMaxCPUChange(Sender: TObject);
begin

end;

{--------------------------------------------------------------------------------------------------
   UTILS
--------------------------------------------------------------------------------------------------}

function TfrmPower.OkToChangeWallpaper: Boolean;
VAR
   iCpuUsage: Integer;
   OnBattery, CpuUBussy: Boolean;
begin
 { Running on batteries? }
 OnBattery:= chkBatteries.Checked AND (PowerStatus<= pwTypeBat);
 if OnBattery
 then AppData.LogWarn('Wallpaper not changed because the computer is running on batteries.');
 Result:= NOT OnBattery;

 if Result then
  begin
   { CPU too bussy? }
   iCpuUsage:= AverageCpuUsage;
   CpuUBussy:= iCpuUsage>= spnMaxCPU.Value;
   if CpuUBussy
   then AppData.LogWarn('Wallpaper not changed because the computer is too busy (CPU: '+ IntToStr(iCpuUsage)+'%). You can change this behaviour in ''Settings''');
   Result:= NOT CpuUBussy;
  end;
end;








VAR
   LastPowerType: TPowerType= pwUnknown;        { Status of the power (AC or battery). Updated ever second }

procedure PaintPowerType(CONST Msg: string);
VAR Canv: TCanvas;
begin
 Canv:= TCanvas.Create;
 TRY
   Canv.Handle:= GetWindowDC(0);
   Canv.Font.Size:= 32;
   Canv.TextOut(3, 3, Msg);
 FINALLY
   FreeAndNil(Canv);
 END;
end;



procedure TfrmPower.CheckSupplyTypeChanged;
VAR Now: TPowerType;
begin
 if NOT chkOutOfJuice.Checked then EXIT;

 Now:= LightCom.PowerUtils.PowerStatus;
 if (Now<> LastPowerType) then
  begin
    case Now of
     pwTypeBat:
       begin
         PaintPowerType('Powered from batteries.');
         BipError; Bip300; BipError;
       end;
     pwTypeAC:
         PaintPowerType('Powered from grid.');
     pwUnknown:
         PaintPowerType('Power supply status unavailable!');
    end;
  end;

 LastPowerType:= PowerStatus;                                                                      {  0= Is on batteries /  1= Is on AC /  >1= Unknown }
end;





end.
