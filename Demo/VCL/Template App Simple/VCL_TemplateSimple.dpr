program VCL_TemplateSimple;

uses
  {$IFDEF DEBUG}FastMM4,{$ENDIF}
  FormMain in 'FormMain.pas' {MainForm},
  LightVcl.Visual.AppData in '..\..\..\FrameVCL\LightVcl.Visual.AppData.pas',
  LightVcl.Visual.AppDataForm in '..\..\..\FrameVCL\LightVcl.Visual.AppDataForm.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;
  
  CONST
     MultiThreaded= FALSE;         // True => Only if we need to use multithreading in the Log.
  CONST
     AppName= 'Light Template Simple';  // Absolutelly critical if you use the SaveForm/LoadForm functionality. This string will be used as the name of the INI file.

  AppData:= TAppData.Create(AppName, '', MultiThreaded);
  AppData.CreateMainForm(TMainForm, MainForm, TRUE, TRUE, asFull);
  AppData.Run;
end.
