program Demo_SaveGUI;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  System.SysUtils,
  MainForm in 'MainForm.pas' {frmTester},
  SecondForm in 'SecondForm.pas' {frmContainer},
  FormRamLog in '..\..\FrameVCL\FrameVCL\FormRamLog.pas',
  cbAppDataVCL in '..\..\FrameVCL\FrameVCL\FrameVCL\cbAppDataVCL.pas';

{$R *.res}

procedure Main;
 begin
  AppData:= TAppData.Create('Light IniFileEx Tester');

  { Properly installed? }
  //if NOT AppData.CheckSysDir then EXIT;

  if AppData.InstanceRunning
  then
    { Send command line to the already running instance and restore (bring to front) that instance }
    AppData.ResurrectInstance(Trim(ParamStr(1))) //ToDo: I need to send the URestore message because I need to call RestoreBioniX (to remove icon fromsystray) on that side (BX first instance)
  else
   begin
     AppData.CreateMainForm(TfrmTester, frmTester, TRUE, TRUE);
     TfrmRamLog.CreateGlobalLog;
     AppData.Run;
   end;
 end;

begin
  Main;
end.
