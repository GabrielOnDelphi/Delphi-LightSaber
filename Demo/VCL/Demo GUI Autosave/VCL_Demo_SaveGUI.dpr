program VCL_Demo_SaveGUI;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  System.SysUtils,
  MainForm in 'MainForm.pas' {frmTester},
  SecondForm in 'SecondForm.pas' {frmContainer},
  LightVcl.Visual.AppData in '..\..\..\FrameVCL\LightVcl.Visual.AppData.pas',
  LightVcl.Visual.AppDataForm in '..\..\..\FrameVCL\LightVcl.Visual.AppDataForm.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas';

{$R *.res}

procedure Main;
 begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo Save GUI', 'Light_Unique_ID');
  if AppData.InstanceRunning
  then
    { Send command line to the already running instance and restore (bring to front) that instance }
    AppData.ResurrectInstance(Trim(ParamStr(1))) //ToDo: I need to send the URestore message because I need to call RestoreBioniX (to remove icon fromsystray) on that side (BX first instance)
  else
   begin
     AppData.CreateMainForm(TfrmTester, frmTester, TRUE, TRUE, asFull);
     AppData.Run;
   end;
 end;

begin
  Main;
end.
