program TemplateFull;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  ccINIFile,
  cbAppDataVCL,
  FormMain in 'FormMain.pas' {MainForm},
  FormRamLog in '..\..\FrameVCL\FormRamLog.pas',
  FormSettings in 'FormSettings.pas',
  FormAbout in '..\..\FrameVCL\FormAbout.pas',
  FormSkinsDisk in '..\..\FrameVCL\FormSkinsDisk.pas',
  FormSkinsRes in '..\..\FrameVCL\FormSkinsRes.pas',
  FormSplashScreen in '..\..\FrameVCL\FormSplashScreen.pas',
  FormUniversalEula in '..\..\FrameVCL\FormUniversalEula.pas',
  FormUpdaterNotifier in '..\..\FrameVCL\Updater\FormUpdaterNotifier.pas',
  FormUpdaterRecEditor in '..\..\FrameVCL\Updater\FormUpdaterRecEditor.pas',
  FormUpdaterSettings in '..\..\FrameVCL\Updater\FormUpdaterSettings.pas',
  uInitialization in 'uInitialization.pas',
  ciUpdater in '..\..\FrameVCL\Updater\ciUpdater.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;
  
  CONST MultiThreaded= FALSE;         // True => Only if we need to use multithreading in the Log.
  CONST AppName= 'Light Template Full';       // Absolutelly critical if you use the SaveForm/LoadForm functionality. This string will be used as the name of the INI file.

  AppData:= TAppData.Create(AppName, '', MultiThreaded);
  AppData.CreateMainForm(TMainForm, MainForm, FALSE, TRUE, asFull);
  TfrmRamLog.CreateGlobalLog;
  
  AppData.Run;
end.
