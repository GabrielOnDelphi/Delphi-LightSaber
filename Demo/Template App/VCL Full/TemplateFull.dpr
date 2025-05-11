program TemplateFull;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  ccINIFile,
  LightCom.AppData,
  FormMain in 'FormMain.pas' {MainForm},
  FormSettings in 'FormSettings.pas',
  uInitialization in 'uInitialization.pas',
  LightCom.AppDataForm in '..\..\..\FrameVCL\LightCom.AppDataForm.pas',
  ccAppData in '..\..\..\ccAppData.pas',
  LightCom.IniFile in '..\..\..\FrameVCL\LightCom.IniFile.pas';

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
