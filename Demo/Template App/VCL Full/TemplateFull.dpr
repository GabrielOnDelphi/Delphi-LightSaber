program TemplateFull;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  ccINIFile,
  cbAppDataVCL,
  FormMain in 'FormMain.pas' {MainForm},
  FormSettings in 'FormSettings.pas',
  uInitialization in 'uInitialization.pas',
  cbAppDataForm in '..\..\..\FrameVCL\cbAppDataForm.pas',
  ccAppData in '..\..\..\ccAppData.pas',
  cbINIFile in '..\..\..\FrameVCL\cbINIFile.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;
  
  CONST MultiThreaded= FALSE;         // True => Only if we need to use multithreading in the Log.
  CONST AppName= 'Light Template Full';       // Absolutelly critical if you use the SaveForm/LoadForm functionality. This string will be used as the name of the INI file.

  AppData:= TAppData.Create(AppName, '', MultiThreaded);
  AppData.CreateMainForm(TMainForm, MainForm, FALSE, TRUE, asFull);
  
  AppData.Run;
end.
