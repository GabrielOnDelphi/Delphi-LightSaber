program VCL_TemplateSimple;

uses
  {$IFDEF DEBUG}FastMM4,{$ENDIF }
  FormMain in 'FormMain.pas' {MainForm},
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  LightVcl.Common.AppDataForm in '..\..\..\FrameVCL\LightCom.AppDataForm.pas',
  LightVcl.Common.IniFile in '..\..\..\FrameVCL\LightCom.IniFile.pas',
  LightVcl.Common.AppData in '..\..\..\FrameVCL\LightCom.AppData.pas',
  LightVcl.LogForm in '..\..\..\FrameVCL\LightVcl.LogForm.pas';

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
