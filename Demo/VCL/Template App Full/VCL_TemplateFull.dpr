program VCL_TemplateFull;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  FormMain in 'FormMain.pas' {MainForm},
  FormSettings in 'FormSettings.pas',
  uInitialization in 'uInitialization.pas',
  LighCore.AppData in '..\..\..\LighCore.AppData.pas',
  LightVcl.Common.AppDataForm in '..\..\..\FrameVCL\LightCom.AppDataForm.pas',
  LightVcl.Common.IniFile in '..\..\..\FrameVCL\LightCom.IniFile.pas',
  LightVcl.Common.AppData in '..\..\..\FrameVCL\LightCom.AppData.pas',
  LightVcl.LogForm in '..\..\..\FrameVCL\LightVcl.LogForm.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;
  
  CONST
     MultiThreaded= FALSE;                 // True => Only if we need to use multithreading in the Log.
  CONST
     AppName= 'Light Template Full';       // Absolutelly critical if you use the SaveForm/LoadForm functionality. This string will be used as the name of the INI file.

  AppData:= TAppData.Create(AppName, '', MultiThreaded);
  AppData.CreateMainForm(TMainForm, MainForm, FALSE, TRUE, asFull);
  AppData.Run;
end.
