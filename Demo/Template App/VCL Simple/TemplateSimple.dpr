program TemplateSimple;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  ccINIFile,
  cbAppDataVCL,
  FormMain in 'FormMain.pas' {MainForm},
  FormRamLog in '..\..\FrameVCL\FormRamLog.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;
  
  CONST
     MultiThreaded= FALSE;         // True => Only if we need to use multithreading in the Log.
  CONST
     AppName= 'Light Template Simple';  // Absolutelly critical if you use the SaveForm/LoadForm functionality. This string will be used as the name of the INI file.

  AppData:= TAppData.Create(AppName, '', MultiThreaded);
  AppData.CreateMainForm(TMainForm, MainForm, TRUE, TRUE, asFull);
  TfrmRamLog.CreateGlobalLog !Remove this!;
  AppData.Run;
end.
